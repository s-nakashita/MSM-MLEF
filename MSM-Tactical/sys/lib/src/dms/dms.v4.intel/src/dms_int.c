/*---------------------------------------------------------------------*\
 *                                                                     *
 *  Program file: dms_int.c                                            *
 *  Purpose: The API Modules used to support C and Fortran language .  *
 *                                                                     *
\*---------------------------------------------------------------------*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "dms.h"
#include "dmserrno.h"
#include "dms_log.h"
#include "dms_api.h"
#include "dms_lib.h"
#include "dms_int.h"
#include "key_lib.h"

extern DMSCONF dmsconfig;

int opncnt = 0;    	   /* open file count  */
OPNFILE opnfile[MAXFILE];  /* open file struct */

/*---------------------------------------------------------------------------*/
/*++++++++++++++++++< Routines for internal reference >++++++++++++++++++++++*/
/*---------------------------------------------------------------------------*/
int search_file(char *dmsfile,DMS **dmsp,int *fileno)
/*---------------------------------------------------------------------------*/
{ int i,stat=EDMS_NOTOPEN;

  if(opncnt == 0) return(stat);
  for(i=0; i<opncnt; i++)
  { if( strcmp(opnfile[i].dmsfile,dmsfile) == 0)
    { *fileno=i; *dmsp=opnfile[i].dmsp; stat=EDMS_OK; break; }
  }
  return(stat);
}

/*---------------------------------------------------------------------------*/
/*+++++++++++++++++< Routines for C language interface >+++++++++++++++++++++*/
/*---------------------------------------------------------------------------*/
int cdmscfg(char *type,char *argument,int *status)
/*---------------------------------------------------------------------------*/
{ CONF_ENUM conftype=S_UNKNOWN;                  

  if( strncasecmp(type,"KEYLEN",strlen(type)) == 0 ) conftype = S_KEYLEN;
  if( strncasecmp(type,"RORDER",strlen(type)) == 0 ) conftype = S_RORDER;
  if( strncasecmp(type,"WORDER",strlen(type)) == 0 ) conftype = S_WORDER;
  if( strncasecmp(type,"MAPFILE",strlen(type)) == 0 ) conftype = S_MAPFILE;
  if( conftype == S_UNKNOWN ) return( *status = EDMS_CONFIG );
  return( *status = dms_conf(conftype,argument) );
}

/*---------------------------------------------------------------------------*/
int cdmsinit(void (*sig_handler)(),int *status)
/*---------------------------------------------------------------------------*/
{ return( *status = dms_init(sig_handler) ); }

/*---------------------------------------------------------------------------*/
int cdmsmsg(char *type,int *status)
/*---------------------------------------------------------------------------*/
{ char argument[8];
  int  msg=LOG_MIN;

  if( strncasecmp(type,"OFF",3) == 0 ) msg = LOG_MIN;
  if( strncasecmp(type,"ERR",3) == 0 ) msg = LOG_ERR;
  if( strncasecmp(type,"MSG",3) == 0 ) msg = LOG_INF;
  if( strncasecmp(type,"ALL",3) == 0 ) msg = LOG_MAX-1;
  sprintf( argument, "%d", msg );
  return ( *status = dms_conf(S_LOG,argument) );
}

/*---------------------------------------------------------------------------*/
int cdmsopn(char *dmsfile,char *mode,int *status)
/*---------------------------------------------------------------------------*/
{ int   i=0,stat,fileno,hasopn=0;
  u_int filemode;
  DMS   *dmsp;

  switch (mode[0])
  { case 'R':
    case 'r':
      filemode = ODMS_RDONLY;
      break;
    case 'W':
    case 'w':
      filemode = ODMS_RDWR;
      break;
    default:
      return(EDMS_BADARG);
  }
  stat = search_file(dmsfile,&dmsp,&fileno);
  if (stat == EDMS_OK)
  {
    if(dmsp->access == filemode || dmsp->access == ODMS_RDWR) 
      return(*status=EDMS_OK); 
    hasopn = 1;
  }
  if(hasopn == 0)
  { 
    if (opncnt+1 > MAXFILE) return(EDMS_TMFILE);
    if ( (dmsp=(DMS *)malloc(sizeof(DMS))) == NULL ) return(EDMS_NOMEM); 
  }
  stat = dms_fopn(dmsfile,filemode,dmsp);
  if(stat == EDMS_OK && hasopn == 0)
  { 
    strcpy(opnfile[opncnt].dmsfile,dmsfile);
    opnfile[opncnt].dmsp = dmsp;
    opncnt++;
  }
  return( *status = stat );
}

/*---------------------------------------------------------------------------*/
int cdmschkf(char *dmsfile,int *status)
/*---------------------------------------------------------------------------*/
{ int stat,fileno;
  DMS *dmsp;

  stat = search_file(dmsfile,&dmsp,&fileno);
  if (stat == EDMS_OK) return(*status=0);
  if ( (dmsp=(DMS *)malloc(sizeof(DMS))) == NULL ) return(EDMS_NOMEM);
  stat = dms_fopn(dmsfile,ODMS_RDONLY,dmsp);
  if ( stat != EDMS_OK )
  { free(dmsp); }
  else
  { dms_fcls(dmsp); }
  return( *status = stat );
}

/*---------------------------------------------------------------------------*/
int cdmsinv(char *dmsfile,char *key,char *buf,int *num,int *status)
/*---------------------------------------------------------------------------*/
{ DMS *dmsp;
  DMST tmpkey;
  int stat,fileno,keynum,totnum=0,i,keylen,tmp;
  char msgstr[128],keyset[MAXKEYL],*keyin,*kptr,*bptr;

  stat = search_file(dmsfile,&dmsp,&fileno);
  if (stat != EDMS_OK) return( *status = stat );
  /* tmpkey.size = keylen = dmsp->keylen; */
  bptr = buf;

  i = max(MAX_DMS_STRL,strlen(key)+1);
  keyin = (char *)malloc(i);
  if( keyin == NULL ) return(*status=EDMS_NOMEM);
  strcpy(keyin,key);

  if( (kptr=strtok(keyin,"\n")) != NULL )
  {
    do
    {
      if ( strchr(kptr,'*') == NULL )
      { tmpkey.data = kptr; }
      else
      {
        keylen = dmsconfig.rorder[0];
        tmpkey.data = key_expend(kptr,keylen);
      }
      tmpkey.size = strlen(tmpkey.data); 
      keynum = *num - totnum;
      stat = dms_rlst(dmsp,&tmpkey,bptr,&keynum);
      /** if(stat != EDMS_OK) return( *status = stat ); **/
      totnum += keynum;
      if(stat == EDMS_OK)
      {
        for(i=1;i<=keynum;i++)
        { 
          tmp=strlen(bptr); bptr[tmp]='\n'; 
          if(i == keynum) bptr[tmp+1]='\0';
        }
        bptr += strlen(bptr);
      }
    } while( (kptr=strtok(NULL,"\n")) != NULL );
  }
  *num = totnum;
  free(keyin);
  return( *status = stat );
}

/*---------------------------------------------------------------------------*/
int cdmschkr(char *dmsfile,char *keyin,int *status)
/*---------------------------------------------------------------------------*/
{
  int  stat,flag=0,i=0,fileno,curlen;
  DMS  *dmsp;
  DMST tmpkey,tmpdata;
  char dmskey[MAXKEYL],key[MAXKEYL];

  stat = search_file(dmsfile,&dmsp,&fileno);
  if (stat != EDMS_OK)
  { if ( (dmsp=(DMS *)malloc(sizeof(DMS))) == NULL ) return(EDMS_NOMEM);
    stat = dms_fopn(dmsfile,ODMS_RDONLY,dmsp);
    if (stat != EDMS_OK) return(*status=stat);
    flag=1;
  }

  strcpy(key, keyin);
  uppercase(key);

  *status = EDMS_RMAPERR;
  while( (dmsconfig.rorder[i] != 0 && i < KEYCNT) && *status != EDMS_OK )
  {
    curlen = dmsconfig.rorder[i]; i++;
    if( curlen != strlen(key) )
    {
      stat = key_translate(key,curlen,dmskey);
      if( stat != EDMS_OK ) continue;
    }
    else
    { strcpy(dmskey, key); }
    tmpkey.size = curlen;
    tmpkey.data = dmskey;
    key_use(curlen);
    tmpdata.size = key_size(dmskey) * key_nitm(dmskey);
    if( (tmpdata.data=(char *)malloc(tmpdata.size)) == NULL )
    { dms_fcls(dmsp); return(*status=EDMS_NOMEM); }
    *status = dms_rget(dmsp, &tmpkey, &tmpdata);
  }

  if ( flag ) dms_fcls(dmsp);
  free(tmpdata.data);
  return( *status );
}

/*---------------------------------------------------------------------------*/
int cdmsdlt(char *dmsfile,char *keyin,int *status)
/*---------------------------------------------------------------------------*/
{ int stat,fileno,curlen,i=0;
  DMS *dmsp;
  DMST tmpkey, tmpdata;
  char dmskey[MAXKEYL],key[MAXKEYL];

  stat = search_file(dmsfile,&dmsp,&fileno);
  if (stat != EDMS_OK) return(*status=stat);

  strcpy(key, keyin);
  uppercase(key);

  *status = EDMS_RMAPERR;
  while( (dmsconfig.rorder[i] != 0 && i < KEYCNT) && *status != EDMS_OK )
  {
    curlen = dmsconfig.rorder[i]; i++;
    if( curlen != strlen(key) )
    {
      stat = key_translate(key,curlen,dmskey);
      if( stat != EDMS_OK ) continue;
    }
    else
    { strcpy(dmskey, key); }
    tmpkey.size = curlen;
    tmpkey.data = dmskey;
    key_use(curlen);
    *status = dms_rdlt(dmsp, &tmpkey);
  }
  return ( *status );
}

/*---------------------------------------------------------------------------*/
int cdmsget(char *dmsfile,char *keyin,char *buf,int *status)
/*---------------------------------------------------------------------------*/
{ DMS *dmsp;
  DMST tmpkey,tmpdata;
  char dmskey[MAXKEYL],key[MAXKEYL];
  int stat,fileno,curlen,i=0;

  stat = search_file(dmsfile, &dmsp, &fileno);
  if (stat != EDMS_OK) return( *status = stat );

  strcpy(key, keyin);
  uppercase(key);
 
  *status = EDMS_RMAPERR;
  while( (dmsconfig.rorder[i] != 0 && i < KEYCNT) && *status != EDMS_OK )
  {
    curlen = dmsconfig.rorder[i]; i++;
    if( curlen != strlen(key) )
    {
      stat = key_translate(key,curlen,dmskey);
      if( stat != EDMS_OK ) continue;
    }
    else
    { strcpy(dmskey, key); }
    tmpkey.size = curlen;
    tmpkey.data = dmskey;
    key_use(curlen);
    tmpdata.size = key_size(dmskey) * key_nitm(dmskey);
    tmpdata.data = buf;
    *status = dms_rget(dmsp, &tmpkey, &tmpdata);
  }
  return( *status );
}

/*---------------------------------------------------------------------------*/
int cdmsput(char *dmsfile,char *keyin,char *buf,int *status)
/*---------------------------------------------------------------------------*/
{ DMS *dmsp;
  DMST tmpkey,tmpdata;
  char dmskey[MAXKEYL],key[MAXKEYL];
  int stat,fileno,curlen,i=0;

  stat = search_file(dmsfile,&dmsp,&fileno);
  if (stat != EDMS_OK) return( *status = stat );

  strcpy(key, keyin);
  uppercase(key);

  *status = EDMS_RMAPERR;
  while( (dmsconfig.worder[i] != 0 && i < KEYCNT) && *status != EDMS_OK )
  {
    curlen = dmsconfig.worder[i]; i++;
    if( curlen != strlen(key) ) 
    {
      stat = key_translate(key,curlen,dmskey);
      if( stat != EDMS_OK ) continue;
    } 
    else
    { strcpy(dmskey, key); }
    tmpkey.size = curlen;
    tmpkey.data = dmskey;
    key_use(curlen);
    tmpdata.size = key_size(dmskey) * key_nitm(dmskey);
    tmpdata.data = buf;
    *status = dms_rput(dmsp,&tmpkey,&tmpdata);
  }
  return( *status );
}

/*---------------------------------------------------------------------------*/
int cdmscls(char *dmsfile,int *status)
/*---------------------------------------------------------------------------*/
{ int i,stat,fileno;
  DMS *dmsp;

  stat = search_file(dmsfile,&dmsp,&fileno);
  if (stat != EDMS_OK) return(*status=stat);
  stat = dms_fcls(dmsp);
  free(dmsp);
  if (stat != EDMS_OK) return(*status=stat);
  for( i=fileno+1; i<opncnt; i++)
  { opnfile[i-1] = opnfile[i]; }
  opncnt--;
  return( *status = EDMS_OK );
}

/*---------------------------------------------------------------------------*/
int cdmsexit(int *status)
/*---------------------------------------------------------------------------*/
{ int i,stat;
  DMS *dmsp;

  for( i=0; i<opncnt; i++)
  { dmsp = opnfile[i].dmsp;
    stat = dms_fcls(dmsp);
    if (stat != EDMS_OK) *status=stat;
  }
  opncnt = 0;
  return( *status = dms_exit(*status) );
}

/*---------------------------------------------------------------------------*/
/*++++++++++++++< Routines for FORTRAN language interface >++++++++++++++++++*/
/*---------------------------------------------------------------------------
#ifdef cray
#define fdmscfg    DMSCFG
#define fdmsmsg    DMSMSG
#define fdmsopn    DMSOPN
#define fdmschkf   DMSCHKF
#define fdmschkr   DMSCHKR
#define fdmsinv    DMSINV
#define fdmscls    DMSCLS
#define fdmsdlt    DMSDLT
#define fdmsget    DMSGET
#define fdmsput    DMSPUT
#define fdmsexit   DMSEXIT
#define fdmsinit   DMSINIT
#elif hpux
#define fdmscfg    dmscfg 
#define fdmsmsg    dmsmsg
#define fdmsopn    dmsopn
#define fdmschkf   dmschkf
#define fdmschkr   dmschkr
#define fdmsinv    dmsinv
#define fdmscls    dmscls
#define fdmsdlt    dmsdlt
#define fdmsget    dmsget
#define fdmsput    dmsput
#define fdmsexit   dmsexit
#define fdmsinit   dmsinit
#elif AIX
#define fdmscfg    dmscfg
#define fdmsmsg    dmsmsg
#define fdmsopn    dmsopn
#define fdmschkf   dmschkf
#define fdmschkr   dmschkr
#define fdmsinv    dmsinv
#define fdmscls    dmscls
#define fdmsdlt    dmsdlt
#define fdmsget    dmsget
#define fdmsput    dmsput
#define fdmsexit   dmsexit
#define fdmsinit   dmsinit
#else
#define fdmscfg    dmscfg_
#define fdmsmsg    dmsmsg_
#define fdmsopn    dmsopn_
#define fdmschkf   dmschkf_
#define fdmschkr   dmschkr_
#define fdmsinv    dmsinv_
#define fdmscls    dmscls_
#define fdmsdlt    dmsdlt_
#define fdmsget    dmsget_
#define fdmsput    dmsput_
#define fdmsexit   dmsexit_
#define fdmsinit   dmsinit_
#endif
---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
int fdmscfg(char *type,char *argument,int *status)
/*---------------------------------------------------------------------------*/
{ return ( cdmscfg(type,argument,status) ); }

/*---------------------------------------------------------------------------*/
int fdmsinit(void (*sig_handler)(),int *status)
/*---------------------------------------------------------------------------*/
{ return( cdmsinit(sig_handler,status) ); }

/*---------------------------------------------------------------------------*/
int fdmsmsg(char *type,int *status)
/*---------------------------------------------------------------------------*/
{ char ctype[4];

  strncpy(ctype,type,3);
  ctype[3]='\0';
  return( cdmsmsg(ctype,status) );
}

/*---------------------------------------------------------------------------*/
int fdmsopn(char *dmsfile,char *mode,int *status)
/*---------------------------------------------------------------------------*/
{ char mode1[2];

  checkname(dmsfile);
  strncpy(mode1,mode,1);
  mode1[1]='\0';
  return( cdmsopn(dmsfile,mode1,status) );
}

/*---------------------------------------------------------------------------*/
int fdmschkf(char *dmsfile,int *status)
/*---------------------------------------------------------------------------*/
{
  checkname(dmsfile);
  return( cdmschkf(dmsfile,status) );
}

/*---------------------------------------------------------------------------*/
int fdmsinv(char *dmsfile,char *key,char *buf,int *len,int *status)
/*---------------------------------------------------------------------------*/
{ char tmpkey[4096],tmpdata[MAXBUFFER];
  int  tmpsize, fileno, stat, keylen;
  DMS  *dmsp;

  checkname(dmsfile);
  stat = search_file(dmsfile,&dmsp,&fileno);
  if (stat != EDMS_OK) return( *status = stat );

  strcpy(tmpkey,key);
  tmpkey[4095]='\0';
  checkkey(tmpkey);
  keylen = strlen(tmpkey);
  *len = MAXBUFFER / (keylen+1); 
  cdmsinv(dmsfile,tmpkey,tmpdata,len,status);
  tmpsize = (*len) * (keylen+1);
  strncpy(buf,tmpdata,tmpsize);
  return( *status );
}

/*---------------------------------------------------------------------------*/
int fdmschkr(char *dmsfile,char *key,int *status)
/*---------------------------------------------------------------------------*/
{ char tmpkey[MAXKEYL];
  int  keylen, stat, fileno;
  DMS  *dmsp;

  checkname(dmsfile);
  stat = search_file(dmsfile,&dmsp,&fileno);
  if (stat != EDMS_OK) return( *status = stat );

  memset(tmpkey, 0x00, MAXKEYL);
  strcpy(tmpkey,key);
  checkkey(tmpkey);
  keylen = strlen(tmpkey);
  return( cdmschkr(dmsfile,tmpkey,status) );
}

/*---------------------------------------------------------------------------*/
int fdmsdlt(char *dmsfile,char *key,int *status)
/*---------------------------------------------------------------------------*/
{ char tmpkey[MAXKEYL];
  int  keylen, stat, fileno;
  DMS  *dmsp;

  checkname(dmsfile);
  stat = search_file(dmsfile,&dmsp,&fileno);
  if (stat != EDMS_OK) return( *status = stat );

  memset(tmpkey, 0x00, MAXKEYL);
  strcpy(tmpkey, key);
  checkkey(tmpkey);
  keylen = strlen(tmpkey);
  return( cdmsdlt(dmsfile,tmpkey,status) );
}

/*---------------------------------------------------------------------------*/
int fdmsget(char *dmsfile,char *key,char *buff,int *status)
/*---------------------------------------------------------------------------*/
{ char tmpkey[MAXKEYL];
  int  keylen, stat, fileno;
  DMS  *dmsp;

  checkname(dmsfile);
  stat = search_file(dmsfile,&dmsp,&fileno);
  if (stat != EDMS_OK) return( *status = stat );

  memset(tmpkey, 0x00, MAXKEYL);
  strcpy(tmpkey, key);
  checkkey(tmpkey);
  keylen = strlen(tmpkey);
  return( cdmsget(dmsfile,tmpkey,buff,status) );
}

/*---------------------------------------------------------------------------*/
int fdmsput(char *dmsfile,char *key,char *buff,int *status)
/*---------------------------------------------------------------------------*/
{ char tmpkey[MAXKEYL];
  int  keylen, stat, fileno;
  DMS  *dmsp;

  checkname(dmsfile);
  stat = search_file(dmsfile,&dmsp,&fileno);
  if (stat != EDMS_OK) return( *status = stat );

  memset(tmpkey, 0x00, MAXKEYL);
  strcpy(tmpkey,key);
  checkkey(tmpkey);
  keylen = strlen(tmpkey);
  return( cdmsput(dmsfile,tmpkey,buff,status) );
}

/*---------------------------------------------------------------------------*/
int fdmscls(char *dmsfile,int *status)
/*---------------------------------------------------------------------------*/
{
  checkname(dmsfile);
  return( cdmscls(dmsfile,status) );
}

/*---------------------------------------------------------------------------*/
int fdmsexit(int *status)
/*---------------------------------------------------------------------------*/
{ return( cdmsexit(status) ); }


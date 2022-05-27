/*---------------------------------------------------------------------------*\
 *                                                                           *
 *  Program file: dms_lib.c                                                  *
 *  Purpose: This program is a library including some common routines used   *
 *           by most of the modules.                                         *
 *                                                                           *
\*---------------------------------------------------------------------------*/

#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <pwd.h>
#include <netdb.h>
#include <ctype.h>
#include <sys/types.h>
#include <sys/socket.h>

#include "dms.h"
#include "dmserrno.h"
#include "dms_lib.h"
#include "dms_env.h"
#include "dms_log.h"
#include "key_lib.h" 

#define BUFFSIZE    1024
#define MAXSTRING   25000
#define MAXSOCKETSZ 30720

extern int pltcnt;
extern DMS_PLT pltapi[];
extern DMSMAP dmsmap[];

/*---------------------------------------------------------------------------*/
/*++++++++++++++++++< Routines for external reference >++++++++++++++++++++++*/
/*---------------------------------------------------------------------------*/
void uppercase(char *s)
/*---------------------------------------------------------------------------*/
{
  while(*s)
  { *s=toupper(*s); s++; }
}

/* -------------------------------------------------------------------- */
void checkname(char *s)
/* -------------------------------------------------------------------- */
{
  while( *s )
  { if( isspace(*s) )
    { *s='\0'; break; }
    s++;
  }
}

/* -------------------------------------------------------------------- */
void checkkey(char *s)
/* -------------------------------------------------------------------- */
{
  int  i;

  if ( strlen(s) != 0 )
  {
    for( i=min(MAXKEYL,strlen(s)-1); i>=0; i--)
    { if( !isspace(s[i]) ) break; }
    s[i+1] = '\0';
  }
}

/* -------------------------------------------------------------------- */
char *cutspace(char *s)
/* -------------------------------------------------------------------- */
{
  static char rtnstr[MAX_DMS_STRL];
  int i=0;

  while( *s )
  { if( ! isspace(*s) ) break;
    s++;
  }

  if ( strlen(s) != 0 )
  {
    for( i=strlen(s)-1; i>=0; i--)
    { if( !isspace(s[i]) ) break; }
    s[i+1] = '\0';
    strcpy( rtnstr, s);
  }
  else
  {
    rtnstr[0] = 0x00;
  }
  return( rtnstr );
}

/*---------------------------------------------------------------------------*/
int getdmspath(char dmspath[],char *line)
/* line : 'F' - First Line                                                   */ 
/*        Other - All Line                                                   */ 
/*---------------------------------------------------------------------------*/
{ FILE *fp;
  char s[MAX_DMS_STRL],*pathset;
  int find=0;

  pathset = dmspath;
  *pathset = '\0';
  sprintf(s,"%s/%s",getenv("HOME"),DMSRC);
  if(access(s,F_OK) == 0)
  { 
    if((fp=fopen(s,"r")) == NULL) return(EDMS_PATHSET);
    while ( fgets(s,MAX_DMS_STRL,fp) )
    { checkname(s);
      if( strncmp(s,PATH_SET,strlen(PATH_SET))==0 ) 
      { find = 1; break; }  
    }
  }
  else
  {
    sprintf(s,"%s/%s",getenv("HOME"),DMSPATH);
    if(access(s,F_OK) == 0)
    { if((fp=fopen(s,"r")) == NULL) return(EDMS_PATHSET); }
    else return(EDMS_PATHSET);
    find = 1;
  }

  while ( find && fgets(s,MAX_DMS_STRL,fp) )
  { checkname(s);
    if(s[0] == '[') break; /* next section */
    if(s[0]!='\0' && s[0]!='#') 
    { if( access(s,F_OK) != 0 ) { fclose(fp); return(EDMS_PATHSET); }
      strcat(pathset,s); 
      if( *line == 'F' ) break; 
      strcat(pathset,"#"); 
    }
  }
  fclose(fp);
  return(find == 0 ? EDMS_PATHSET : EDMS_OK);
}

/* Send a response buffer to the client.     */
/*---------------------------------------------------------------------------*/
int resp_buff(int x, char *buff,int nbytes)
/*---------------------------------------------------------------------------*/
{ int i, cc;

  for(i=0; i< nbytes; i+=cc)
  { if((nbytes-i) < MAXSOCKETSZ)
    { if((cc=write(x, &buff[i], nbytes-i)) <= 0) return(EDMS_IO); }
    else
    { if((cc=write(x, &buff[i], MAXSOCKETSZ)) <= 0) return(EDMS_IO); }
  }
  return(EDMS_OK);
}

static int maxsocketsize = -1;
/*---------------------------------------------------------------------------*/
void checksocket(int size,int option)
/*---------------------------------------------------------------------------*/
{
  if(size > MAXSOCKETSZ) size=MAXSOCKETSZ;
  if(size <= maxsocketsize) return;
  maxsocketsize=size;
  while( (size > 1024) &&
         (setsockopt(0,SOL_SOCKET,option,(char *)&size,sizeof(size)) < 0) )
  { size -= 1024; }
}

/*---------------------------------------------------------------------------*/
int getbuf(DMST *ptr,int memsz)
/*---------------------------------------------------------------------------*/
{
  if (memsz == ptr->size) return(EDMS_OK);
  if (ptr->data != NULL) free((void *)ptr->data);
  if((ptr->data =(char *)malloc(memsz))==NULL) return(EDMS_NOMEM);
  ptr->size = memsz;
  return(EDMS_OK);
}

/*---------------------------------------------------------------------------*/
int checkformat(char *s)
/*---------------------------------------------------------------------------*/
{
  if(strcmp(s,FORMAT)==0) return(0);
  else if(strcmp(s,"XDR")==0) return(2);
  else return(-2);
}

/*---------------------------------------------------------------------------*/
int validuser(char *cfgdir,char *rhost,char *ruser,char *dmsfile)
/*---------------------------------------------------------------------------*/
{ register char *ptr;
  char tmpstr[MAX_DMS_STRL],*h,*u,*df,*m;
  FILE *fp;
  int mode,plus,pmode[3],rtn;

  sprintf(tmpstr,"%s/%s",cfgdir,DMSRC);
  if((fp=fopen(tmpstr,"r"))!=NULL) 
  {
    while ( fgets(tmpstr,MAX_DMS_STRL,fp) )
    { checkname(tmpstr);
      if( strncmp(tmpstr,HOST_SET,strlen(HOST_SET))==0 ) break;  
    }
  }
  else
  {
    sprintf(tmpstr,"%s/%s",cfgdir,SECURITYFILE);
    if((fp=fopen(tmpstr,"r"))==NULL) return(0);
  }
  pmode[0] = pmode[1] = pmode[2] = 0;
  while ( fgets(tmpstr,MAX_DMS_STRL,fp) )
  { plus = 0;
    ptr=tmpstr;
    while( isspace(*ptr) ) ptr++;
    h=ptr;
    if(h[0]=='\0' || h[0]=='#') continue; /* space line or remark line */
    if(h[0]=='[') break; /* next item header */
    while( !isspace(*ptr) ) 
    { *ptr=isupper(*ptr) ? tolower(*ptr) : *ptr;
      ptr++;
    }
    if(*ptr == ' ' || *ptr=='\t') /* host field */
    { *ptr++='\0';
      if( (strcmp(h,rhost)!=0) && (h[0]!='+') ) continue;
      if( h[0]=='+' ) plus++;
    }
    while(*ptr==' ' || *ptr=='\t') ptr++;
    u=ptr;
    while( !isspace(*ptr) ) ptr++;
    if(*ptr == ' ' || *ptr=='\t') /* user field */
    { *ptr++='\0';
      if( (strcmp(u,ruser)!=0) && (u[0]!='+') ) continue;
      if(u[0]=='+') plus++;
    }
    while(*ptr==' ' || *ptr=='\t') ptr++;
    df=ptr;
    while( !isspace(*ptr) ) ptr++;
    if(*ptr==' ' || *ptr=='\t') /* dmsfile field */
    { *ptr++='\0';
      if((strcmp(df,dmsfile)!=0) && (df[0]!='+')) continue;
      if(df[0]=='+') plus++;
    }
    while(*ptr==' ' || *ptr=='\t') ptr++;
    m=ptr;
    while( !isspace(*ptr) ) ptr++;
    if(*m=='R' || *m=='r') mode=ODMS_RDONLY;
    else if(*m=='W' || *m=='w') mode=(ODMS_RDWR|ODMS_RDONLY);
    else if(*m=='C' || *m=='c') mode=(ODMS_RDWR|ODMS_RDONLY|ODMS_CREATE);
    else mode=0;
    if( plus == 0 ) 
    { return(mode); }
    else 
    { pmode[plus-1] = (mode == 0 ? 0 : pmode[plus-1] | mode); }
  }
  fclose(fp);
  if(pmode[0] != 0) return(pmode[0]);
  if(pmode[1] != 0) return(pmode[1]);
  if(pmode[2] != 0) return(pmode[2]);
  return(0);
}

/*---------------------------------------------------------------------------*/
char *get_dmsname(char *logical_name)
/*---------------------------------------------------------------------------*/
{ static char dmsname[MAX_DMS_FNAME];
  char s[MAX_DMS_FNAME],tmpstr[MAX_DMS_FNAME],*ptr;
  int  i,flag=0;
  FILE *fp;

  sprintf(s,"%s/%s",getenv("HOME"),DMSRC);
  if( access(s,F_OK) == 0 )
  { if( (fp=fopen(s,"r")) == NULL ) return(NULL);
    while ( fgets(s,MAX_DMS_STRL,fp) )
    { checkname(s);
      if( strncmp(s,NAME_SET,strlen(NAME_SET))==0 ) break;
    }
    while ( fgets(s,MAX_DMS_STRL,fp) )
    { checkname(s);
      if( s[0]=='\0' || s[0]=='#' ) continue; /* space line or remark line */
      if( s[0]=='[' ) break; /* next item header */
/****
      if( strncmp(s,logical_name,strlen(logical_name))==0 )
      {
        if( (ptr=strchr(s,'=')) == NULL ) continue;
        strcpy( dmsname, ++ptr );
        checkname( dmsname );
        flag = 1;
        break;
      }
****/
      if( (ptr=strchr(s,'=')) != NULL )
      {
        *ptr = 0x20;
        sscanf(s,"%s %s",tmpstr,dmsname);
        if( strcmp(logical_name,tmpstr) == 0 )
        { checkname( dmsname );
          flag = 1;
          break;
        }
      }
    }
    fclose(fp);
    return( flag == 1 ? dmsname : NULL );
  }
  else
  { return( NULL ); }
}

/*---------------------------------------------------------------------------*/
int chk_filename(char *dmsfile,char *dmsdb,char *logical_name)
/*---------------------------------------------------------------------------*/
{ char *eptr;

  if( strchr(dmsfile,'@') != NULL && dmsdb != NULL ) return( EDMS_BADFMT );
  if( dmsdb == NULL || dmsdb[0] == 0x00 )
    sprintf(logical_name,"%s",dmsfile);
  else
    sprintf(logical_name,"%s@%s",dmsfile,dmsdb);
  return( EDMS_OK );
}

/*---------------------------------------------------------------------------*/
void xinvsort(char *pout,int number,int keylen)
/*---------------------------------------------------------------------------*/
{ char string[BUFFSIZE][MAXKEYL];
  char *line[BUFFSIZE], *p;
  int i,j,k=0,ts,tl,fs,fl;

  if(number > 0)
  { /* Modified for sorting */
    for(i=0;i<number;i++)
    { memcpy(string[i],&pout[k],keylen+1);
      line[i]=(char *)string[i];
      k+=(keylen+1);
    }
    key_time_def(&ts,&tl);
    key_field_def(&fs,&fl);
    for(j=0;j<number;j++)
    { for(k=j+1;k<number;k++)
      { if(memcmp(&line[j][ts],&line[k][ts],tl) >0)
        { p=line[k];
          line[k]=line[j];
          line[j]=p;
        }
        if( (memcmp(&line[j][ts],&line[k][ts],tl) == 0) &&
            (memcmp(&line[j][fs],&line[k][fs],fl) > 0) )
        { p=line[k];
          line[k]=line[j];
          line[j]=p;
        }
      }
    }
    for(i=0;i<number;i++)
      strncpy(&pout[i*(keylen+1)],line[i],keylen+1);
    pout[i*(keylen+1)]='\0';
  }
}

/*---------------------------------------------------------------------------*/
void adtfflush(FILE *out,char *pout,int number,int keylen)
/*---------------------------------------------------------------------------*/
{ int  j;
  char *ptr;

  if(number > 0)
  { invsort(pout,number,keylen);
    ptr = pout;
    for( j=0; j<number; j++,ptr+=(keylen+1) ) fprintf(out,"%s\n",ptr);
  }
}


/*---------------------------------------------------------------------------*/
int local_info(char local_host[],char local_user[])
/*---------------------------------------------------------------------------*/
{ char temphost[64];
  struct passwd *pwd;

  gethostname(temphost,64);
  pwd=getpwuid(geteuid());

  strcpy( local_host, temphost );
  strcpy( local_user, pwd->pw_name );
  return( EDMS_OK );
}

/*---------------------------------------------------------------------------*/
/*++++++++++++++++++< Inventory sorting   routine set >++++++++++++++++++++++*/
/*---------------------------------------------------------------------------*/
static int cmp_st,cmp_ln;
/*---------------------------------------------------------------------------*/
static int inv_cmp(const void *ptr1,const void *ptr2)
/*---------------------------------------------------------------------------*/
{ int  i;
  char *p1,*p2;

  p1 = (char *)ptr1 + cmp_st ;
  p2 = (char *)ptr2 + cmp_st ;
  for( i=0 ; i < cmp_ln ; i++,p1++,p2++ )
  { if( *p1 > *p2 ) return( 1 ) ;
    if( *p1 < *p2 ) return(-1 ) ;
  }
  return( 0 ) ;
}

/*---------------------------------------------------------------------------*/
void invsort(char *pout,int number,int keylen)
/*---------------------------------------------------------------------------*/
{ int width=keylen+1;

  key_field_def(&cmp_st,&cmp_ln);
  qsort(pout,number,width,inv_cmp);

  key_tau_def(&cmp_st,&cmp_ln);
  qsort(pout,number,width,inv_cmp);

  key_time_def(&cmp_st,&cmp_ln);
  qsort(pout,number,width,inv_cmp);
}

/*---------------------------------------------------------------------------*/
/*++++++++++++++++++< Object manipulation routine set >++++++++++++++++++++++*/
/*---------------------------------------------------------------------------*/
/*---------------------------------------------------------------------------*/
void *obj_dupx(void *obj_ptr,int obj_siz)
/*---------------------------------------------------------------------------*/
{ void *tmp_ptr;

  if( (tmp_ptr = malloc(obj_siz)) == NULL ) return( NULL );
  memcpy(tmp_ptr,obj_ptr,obj_siz);
  return( tmp_ptr );
}
/*---------------------------------------------------------------------------*/
int  obj_init(OBJLIST *listp)
/*---------------------------------------------------------------------------*/
{ int i;

  listp->nodes = calloc( listp->maxsz , sizeof(OBJNODE) );
  if( listp->nodes == NULL ) return(-1);
  return(0);
}

/*---------------------------------------------------------------------------*/
int  obj_walk(OBJLIST *listp,int (*func)(OBJNODE *) )
/*---------------------------------------------------------------------------*/
{ int     i,stat;
  OBJNODE *nodep;

  if( (nodep = listp->nodes) == NULL ) obj_init(listp) ;
  for( i=0 ; i < listp->cursz ; i++ )
  { if( (stat=func(&nodep[i])) != 0 ) return(stat); }
  return( 0 ) ;
}

/*---------------------------------------------------------------------------*/
int  obj_indx(OBJLIST *listp,char *obj_key)
/*---------------------------------------------------------------------------*/
{ int     i;
  OBJNODE *nodep;

  if( (nodep = listp->nodes) == NULL ) obj_init(listp) ;
  for( i=0 ; i < listp->cursz ; i++ )
  { if( strcmp( nodep[i].key , obj_key ) == 0 ) return( i ) ; }
  return( -1 ) ;
}

/*---------------------------------------------------------------------------*/
void *obj_ptrx(OBJLIST *listp,char *obj_key)
/*---------------------------------------------------------------------------*/
{ int     i;

  if( (i = obj_indx(listp,obj_key)) < 0 ) return( NULL );
  return( (listp->nodes)[i].ptr );
}

/*---------------------------------------------------------------------------*/
int  obj_addx(OBJLIST *listp,char *obj_key,void *obj_ptr, int obj_siz)
/*---------------------------------------------------------------------------*/
{ int     i;

  if( listp->nodes == NULL ) obj_init(listp) ;
  /* --- if( (i = obj_indx(listp,obj_key)) >= 0 ) return( i ); --- */
  if( listp->cursz >= listp->maxsz ) return( -1 );
  i = listp->cursz++ ;
  (listp->nodes)[i].use = 1;
  (listp->nodes)[i].key = obj_dupx(obj_key,strlen(obj_key)+1);
  (listp->nodes)[i].ptr = obj_dupx(obj_ptr,obj_siz);
  return( i ) ;
}

/*---------------------------------------------------------------------------*/
int  obj_delt(OBJLIST *listp,char *obj_key)
/*---------------------------------------------------------------------------*/
{ int  i,j;
  OBJNODE *nodep;

  if( (i = obj_indx(listp,obj_key)) < 0 ) return( i );
  nodep = listp->nodes ;
  free( nodep[i].key ) ; free( nodep[i].ptr );
  j = --listp->cursz - i ;
  memmove(&nodep[i],&nodep[i+1],j*sizeof(OBJNODE));
  return(0);
}

/*---------------------------------------------------------------------------*/
int  obj_free(OBJLIST *listp)
/*---------------------------------------------------------------------------*/
{ int i;
  OBJNODE *nodep;

  nodep = listp->nodes ;
  for( i=0 ; i < listp->cursz ; i++ )
  { free( nodep[i].key ); free( nodep[i].ptr ); }
  free( listp->nodes );
  listp->nodes = NULL ;
  listp->cursz = 0 ;
  return(0);
}

/*---------------------------------------------------------------------------*/
/*+++++++++++++++++< Get dms server connection routine set >+++++++++++++++++*/
/*---------------------------------------------------------------------------*/

/*----------------------------------------------------------------------------+
 | Name : get_connect(char *obj,char *host,char *user,char *psw,int *pid)     |
 | Purpose : Using net_connect_obj or rexec to connect to an exist network    |
 |           object and get child pid                                         |
 | Return  : > 0 : connected fd                                               |
 |           -1  : connect error                                              |
 +----------------------------------------------------------------------------*/
int get_connect(char *obj,char *host,char *user,char *psw,int *pid)
{ struct servent *sp;
  int  netfd;
  char *passwd, *rhost;

  *pid = 0 ;
  if((sp=getservbyname("exec","tcp"))==NULL)  return(-1);
  rhost=(char *)malloc(64);
  memcpy(rhost, &host[0],strlen(host)+1);

  if( *psw == '\0' )
  { if((passwd=ngetdmspass(host,user))==NULL)
    { netfd = net_connect_obj(obj,host,user,psw,pid); }
    else
    { netfd = rexec(&rhost,sp->s_port,user,passwd,obj,(int *)0); }
  }
  else
  { netfd = rexec(&rhost,sp->s_port,user,psw,obj,(int *)0); }

  if( netfd < 0 ) return(-1);
  return(netfd);
}

/*---------------------------------------------------------------------------*/
char *getdmspass(char *host,char *user)
/*---------------------------------------------------------------------------*/
{ FILE *fp;
  int i;
  char buf[90],*ptr1,*ptr2,*ptr3;
  static char pass[20];

  /* Modified for checking .dmshost file */
  if((fp=fopen(FPATH,"r"))==NULL) return(NULL);

  while(fgets(buf,90,fp)!=NULL)
  { if((ptr1=strchr(buf,':'))==NULL) break;
    *ptr1++ =0;
    if((ptr2=strchr(ptr1,':'))==NULL) break;
    *ptr2++ =0;
    if((strcmp(buf,host)==0) && (strcmp(ptr1,user)==0))
    { if((ptr3=strchr(ptr2,':'))==NULL) break;
      *ptr3++ =0;
      for(i=0;i<strlen(ptr2);i++) ptr2[i] ^= 0x88;
      fclose(fp);
      memcpy(pass,ptr2,strlen(ptr2)+1);
      return(pass);
    }
  }

  fclose(fp);
  return(NULL);
}

/*---------------------------------------------------------------------------*/
/* ngetdmspass function can obtain the password throught the environment
   variable 'DMSHOST'. To set up the DMSHOST, users have to export           */
/* DMSHOST=user1*pass1@node1#user2*pass2@node2#....... (dms.v2)              */
/* DMSHOST=user1:pass1@node1#user2:pass2@node2#....... (dms.v3)              */
/*---------------------------------------------------------------------------*/
char *ngetdmspass(char *host,char *user)
/*---------------------------------------------------------------------------*/
{ static char pass[33];
  char *ptr,*envptr,*userptr,*passptr,*hostptr,dmshost[128];

  if( (envptr=getenv("DMSHOST")) == NULL )
    return( getdmspass(host,user) );
  else strcpy( dmshost , envptr );
  if( (userptr=strtok(dmshost,"#")) == NULL ) return( NULL );
  do
/*********************** Modify by Shelly Hsu -- 2000-07-17 ******************
  { if( (ptr=strchr(userptr,(int)'*')) == NULL ) continue ;
******************************************************************************/
  { if( (ptr=strchr(userptr,(int)':')) == NULL ) continue ;
/*****************************************************************************/
    *ptr++ = '\0' ;
    if( strcmp(userptr,user) != 0 ) continue;
    passptr = ptr ;
    if( (ptr=strchr(passptr,(int)'@')) == NULL ) continue ;
    *ptr++ = '\0' ;
    hostptr = ptr ;
    if( strcmp(hostptr,host) != 0 ) continue;
    return( strcpy(pass,passptr) );
  } while( (userptr=strtok(NULL,"#")) != NULL );
  return( NULL );
}

/*---------------------------------------------------------------------------*/
/* ngetsvrpath function can obtain the Server throught the environment
   variable 'SVRPATH'. To set up the SVRPATH, users have to export
   SVRPATH=/path1/server1@node1#/path2/server2@node2#.......                 */
/*---------------------------------------------------------------------------*/
char *ngetsvrpath(char *host)
/*---------------------------------------------------------------------------*/
{ static char dms_svr[MAX_DMS_STRL];
  char *ptr,*envptr,*svrptr,*hostptr,svrpath[MAX_DMS_STRL];

  strcpy( dms_svr, SERVER );
  if( (envptr=getenv("SVRPATH")) == NULL )
    return( dms_svr );
  else strcpy( svrpath , envptr );
  if( (svrptr=strtok(svrpath,"#")) == NULL ) return( dms_svr );
  do
  { if( (ptr=strchr(svrptr,(int)'@')) == NULL ) continue ;
    *ptr++ = '\0' ;
    hostptr = ptr ;
    if( strcmp(hostptr,host) != 0 ) continue;
    return( strcpy(dms_svr,svrptr) );
  } while( (svrptr=strtok(NULL,"#")) != NULL );
  return( dms_svr );
}

/*---------------------------------------------------------------------------*/
char *getcfgname(char *dmsname)
/*---------------------------------------------------------------------------*/
{ static char cfgname[MAX_DMS_FNAME];
  char *pathptr,*nameptr;

  pathptr = dmsname;
  nameptr = strrchr(pathptr,'/'); 
  if( nameptr == NULL ) return(dmsname); 
  *nameptr = '\0';
  if( *(++nameptr) != '.' )
    sprintf(cfgname,"%s/.%s_cfg",pathptr,nameptr);
  else
    sprintf(cfgname,"%s_cfg",dmsname);
  *(--nameptr) = '/';
  return(cfgname);
}

/*---------------------------------------------------------------------------*/
int read_maplist(char *mapfile,int mapuse,int mapklen1,int mapklen2)
/*---------------------------------------------------------------------------*/
{ FILE *fmap,*pfile;
  char tmpbuf[MAX_DMS_STRL],tmpkey1[MAXKEYL],tmpkey2[MAXKEYL];
  char *cptr,*ptr,*keymap1,*kp1,*keymap2,*kp2;
  int  i,fn,tn,errflg,status=EDMS_OK,cnt,fkeylen,tkeylen;

  i = cnt = errflg = 0;
  if( access(mapfile,F_OK) != 0 ) return(EDMS_CONFIG);

  sprintf(tmpbuf,"cat %s | wc -l",mapfile);
  if( (pfile=popen(tmpbuf,"r")) == NULL) return( EDMS_FAIL );
  fscanf(pfile,"%d",&i);
  pclose( pfile );
  if( i == 0 ) return(EDMS_CONFIG);
  if( (dmsmap[mapuse].maplist=(MAPLIST *)malloc((i+1)*sizeof(MAPLIST)))==NULL )
    return( EDMS_NOMEM );

  if( ( fmap = fopen(mapfile,"r") ) == NULL )
  {
    if( dmsmap[mapuse].maplist != NULL ) free( dmsmap[mapuse].maplist );
    dmsmap[mapuse].maplist = NULL;
    return( EDMS_CONFIG );
  }

  while( fgets(tmpbuf,90,fmap) != NULL && status == EDMS_OK )
  {
    cptr = cutspace(tmpbuf);
    if( *cptr == '#' || *cptr == 0x00 ) continue;
    sscanf(cptr,"%s %s",tmpkey1,tmpkey2);

    kp1 = keymap1 = dmsmap[mapuse].maplist[cnt].keymap1;
    kp2 = keymap2 = dmsmap[mapuse].maplist[cnt].keymap2;

    strcpy(keymap1,key_expend(tmpkey1,mapklen1));
    strcpy(keymap2,key_expend(tmpkey2,mapklen2));
    
    for( fn = 0 ; *kp1 ; kp1++ ) 
    { if ( *kp1 == '_' ) *kp1 = 0x20; if ( *kp1 == '?' ) fn++; }
    for( tn = 0 ; *kp2 ; kp2++ ) 
    { if ( *kp2 == '_' ) *kp2 = 0x20; if ( *kp2 == '?' ) tn++; }

    if( fn == tn )
      cnt++;
    else
    {
      status = EDMS_CONFIG;
      log_write(LOG_ERR,"MAP Error keymap1=%s,length=%d\n",keymap1,fn);
      log_write(LOG_ERR,"MAP Error keymap2=%s,length=%d\n",keymap2,tn);
    }
  }

/*** for debug ****************************************************
  for(cnt=0; cnt < dmsmap[mapuse].mapline ; cnt++)
  {
     printf("Source Key=%s\n",&maplist[cnt].keymap1);
     printf("Target Key=%s\n",&maplist[cnt].keymap2);
  }
*******************************************************************/

  dmsmap[mapuse].mapline = cnt;

  fclose( fmap );
  return( status );
}


/*---------------------------------------------------------------------------*/
/*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/
/*---------------------------------------------------------------------------*/

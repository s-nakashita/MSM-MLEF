/*--------------------------------------------------------------------------*\
 *                                                                          *
 *  Program file : dms_api.c                                                *
 *  Purpose : 1.To judge which platform, ufs, gdb or others shall be used.  *
 *            2.To write logging when user used API.                        *
 *            3.Change the logical name to physical name before call        *
 *              platform dependent API.                                     *
 *                                                                          *
\*--------------------------------------------------------------------------*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <dirent.h>
#include <pwd.h>
#include "dms.h"
#include "dmserrno.h"
#include "dms_log.h"
#include "dms_api.h"
#include "platform.h"
#include "dms_lib.h"
#include "dms_env.h"
#define DMSLOG  "dms.log"
#define DMSROOT 0

DMSCONF dmsconfig = 
{ LOG_MIN, 
  24, 
  {34,24,32}, {34,24,32},
  3,
  { 
    "/package/dms/dms.v4/dms_24to34.cfg",
    "/package/dms/dms.v4/dms_32to34.cfg",
    "/package/dms/dms.v4/dms_24to32.cfg" 
  } 
};

DMSMAP dmsmap[KEYCNT] =
{
  {24,34,-1,NULL},
  {32,34,-1,NULL},
  {24,32,-1,NULL}
};

/*---------------------------------------------------------------------------*/
/*++++++++++++++++++++< Routines for macro function >++++++++++++++++++++++++*/
/*---------------------------------------------------------------------------*/
#define GET_REALNAME(name,caller,level) \
{ \
  stat = get_phyname(name,phyname,level); \
  if (stat != EDMS_OK) \
  { log_write(LOG_ERR,"%s : E%04x (%s) get_phyname error!\n",caller,stat,name); \
    return(stat); \
  } \
  stat = get_pltype(phyname,&pltype,level); \
  if (stat != EDMS_OK) \
  { log_write(LOG_ERR,"%s : E%04x (%s) get_pltype error!\n",caller,stat,name); \
    return(stat); \
  } \
}

/*---------------------------------------------------------------------------*/
/*++++++++++++++++++< Routines for internal reference >++++++++++++++++++++++*/

/******
#define D_LEVEL 1
#define F_LEVEL 2
******/
/*---------------------------------------------------------------------------*/
static int get_phyname(char *logical_name,char phyname[],u_int level)
/*---------------------------------------------------------------------------*/
{ DIR *dirdb,*dirfil;
  struct dirent *entdb,*entfil;
  int stat,status=EDMS_BADSET,i,at=0,db_flag=0,epath=0;
  char *namedb,*namefil,*pptr,*ptr;
  char dbname[MAX_DMS_FNAME],dmspath[MAX_DMS_STRL*10];
  char tmp[MAX_DMS_FNAME],namedms[MAX_DMS_FNAME];

  sprintf(namedms,"%s",logical_name);  
  if( (ptr=get_dmsname(logical_name)) != NULL ) sprintf(namedms,"%s",ptr);
  if( (ptr=getenv(logical_name)) != NULL ) sprintf(namedms,"%s",ptr);
  if( (ptr=strchr(namedms,'*')) != NULL )
  { *ptr = '\0'; 
    sprintf(tmp,"%s@dummy@%s",namedms,++ptr);
    strcpy(namedms,tmp);
  } 
  ptr = namedms;
  while( *ptr++ ) if (*ptr == '@') at++;

  switch( at )
  {
    case 0 : /* format : dmsfile */
      if( getdmspath(dmspath,"A") != EDMS_OK ) return( EDMS_PATHSET );
      if( (pptr=strtok(dmspath,"#")) != NULL )
      do
      {
        if( (dirdb=opendir(pptr)) == NULL ) continue;
        while( (entdb=readdir(dirdb)) != NULL && db_flag == 0 )
        { sprintf(tmp,"%s/%s",pptr,entdb->d_name);
          if(*(namedb=entdb->d_name)=='.'||(dirfil=opendir(tmp))==NULL)
             continue;
          while( (entfil=readdir(dirfil)) != NULL )
          { if( level == D_LEVEL )
            { sprintf(dbname,"%s/%s",pptr,namedb); db_flag=1; break; }
            if( *(namefil=entfil->d_name) == '.' ) continue ;
            if( strcmp(namefil,namedms) == 0)
            { sprintf(dbname,"%s/%s",pptr,namedb); db_flag=1; break; }
          }
        }
      } while( (pptr=strtok(NULL,"#")) != NULL && db_flag == 0 );

      if ( db_flag )
      { sprintf(phyname,"%s/%s",dbname,namedms); status = EDMS_OK; }
       break;

    case 1 : /* format : dmsfile@db or dmsfile@/path/db */
      ptr = strchr(namedms,'@');
      *ptr++ = '\0';
      if ( *ptr != '/' )
      { if( getdmspath(dmspath,"A") != EDMS_OK ) return( EDMS_PATHSET );
        if( (pptr=strtok(dmspath,"#")) == NULL ) return(EDMS_PATHSET);
      }
      else
      { sprintf(tmp,"%s",ptr); epath=1; }
      do
      {
        if( epath == 0 ) sprintf(tmp,"%s/%s",pptr,ptr);
        for( i=0; i<pltcnt; i++ )
        { sprintf(dbname,"%s.%s",tmp,pltapi[i].pltname);
          if(level == D_LEVEL)
          { if( (stat=access(dbname,F_OK)) == EDMS_OK ) 
            { strcat(dbname,"/"); strcat(dbname,namedms);
              sprintf(phyname,"%s",dbname);
              return(EDMS_OK);
            }
          }
          else 
          { strcat(dbname,"/"); strcat(dbname,namedms);
            if( (stat=access(dbname,F_OK)) == EDMS_OK )
            { sprintf(phyname,"%s",dbname); return( EDMS_OK ); }
          }
        }
      } while( (pptr=strtok(NULL,"#")) != NULL && epath == 0 );
      break;

  default : /* format : dmsfile@db@user@host, dmsfile@db@user */
    sprintf(phyname,"%s",namedms);
    return( EDMS_OK );
  }

  return(status);
}

/*---------------------------------------------------------------------------*/
int get_pltype(char *phyname,int *pltype,u_int level)
/*---------------------------------------------------------------------------*/
{ int i,stat,status,at=0,len;
  char *tmpname,*ptr,tmpstr[MAX_DMS_FNAME];
  struct passwd pwbuf;
  uid_t euid;
  long pid;
    
  *pltype = 0;
  status = EDMS_BADPLT;
  strcpy(tmpstr, phyname);
  ptr = tmpstr;
  while (*ptr++) if (*ptr == '@') at++;
  if( (tmpname=strrchr(tmpstr,'/')) != NULL ) *tmpname='\0';
  if( (tmpname=strrchr(phyname,'.')) == NULL && at <= 1 ) return(status);
  if( at == 1 ) return(status);
  if(tmpname != NULL) tmpname++;
  for(i=0;i<pltcnt;i++)
  { len=strlen(pltapi[i].pltname);
    if(at >= 2 && strncmp(pltapi[i].pltname,"net",len) == 0) 
    { *pltype = i; status = 0; break; }
    if(at == 0)
    { 
      if(level == D_LEVEL)
        stat = ipc_getpw(tmpstr,&pwbuf);
      else
        stat = ipc_getpw(phyname,&pwbuf);
      if(stat != EDMS_OK) return(stat);
      euid = geteuid();
      if( euid != DMSROOT && euid != pwbuf.pw_uid && 
          strncmp(pltapi[i].pltname,"ipc",len) == 0 ) 
      { *pltype = i; status = 0; break; }
      if( (euid == pwbuf.pw_uid || euid == DMSROOT) &&
          strncmp(pltapi[i].pltname,tmpname,len) == 0) 
      { *pltype = i; status = 0; break; }
    }
  }
  return(status);
}

/*---------------------------------------------------------------------------*/
void dms_clear(DMS *dmsp)
/*---------------------------------------------------------------------------*/
{
  dmsp->pltype     = (PLT_ENUM) 0 ;
  *(dmsp->phyname) = 0 ;
  *(dmsp->lgcname) = 0 ;
  if( dmsp->lrucache != NULL ) { free(dmsp->lrucache); dmsp->lrucache=NULL; }
  if( dmsp->internal != NULL ) { free(dmsp->internal); dmsp->internal=NULL; }
  dmsp->access  = 0 ;
  dmsp->keylen  = 0 ;
  dmsp->fcls    = NULL ;
  dmsp->rget    = NULL ;
  dmsp->rput    = NULL ;
  dmsp->rdlt    = NULL ;
  dmsp->rlst    = NULL ;
  dmsp->conf    = NULL ;
  dmsp->init    = NULL ;
  dmsp->exit    = NULL ;
}

/*---------------------------------------------------------------------------*/
/*++++++++++++++++++< Routines for external reference >++++++++++++++++++++++*/
/*---------------------------------------------------------------------------*/
int dms_init(void (*sig_handler)())
/*---------------------------------------------------------------------------*/
{ int i,stat;

  log_write(LOG_INF,"DMSINIT\n");
  dmsconfig.keylen=24;
  key_use( dmsconfig.keylen );
  dmsconfig.log=LOG_MIN;
  for(i=0;i<pltcnt;i++)
  { stat = pltapi[i].init(sig_handler); 
    if( stat != EDMS_OK )
      log_write(LOG_ERR,"Error: E%04x initialize error.\n",stat);  
  }
  return( stat );
}

/*---------------------------------------------------------------------------*/
int dms_exit(int exitno)
/*---------------------------------------------------------------------------*/
{ char argument[8];
  int i,stat;

  log_write(LOG_INF,"DMSEXIT\n");
  for(i=0;i<pltcnt;i++)
  { stat = pltapi[i].exit(exitno);
    if( stat != EDMS_OK )
      log_write(LOG_ERR,"Error: E%04x initialize error.\n",stat);
  }
  sprintf(argument,"%d",LOG_MIN);
  dms_conf(S_LOG,argument);
  for( i = 0 ; i < dmsconfig.mapcnt ; i++ )
  { if( dmsmap[i].maplist != NULL ) free( dmsmap[i].maplist ); }
  exit(exitno);
  return( exitno ); 
}

/*---------------------------------------------------------------------------*/
int dms_dbcrt(char *dbname,char *plt,char physical_name[],char *cfgstr)
/*---------------------------------------------------------------------------*/
{ int i,stat,pltype=-1;
  char phyname[MAX_DMS_FNAME],tmpname[MAX_DMS_FNAME],dmspath[MAX_DMS_STRL*10];
  char *eptr;

  log_write(LOG_INF,"DMSDBCRT: DB(%s) Plt(%s)\n",dbname,plt);
  if( (eptr=getenv(dbname)) == NULL ) eptr = dbname;
  strcpy(physical_name,eptr);
  if(eptr[0] != '/')
  { 
    if( getdmspath(dmspath,"F") != EDMS_OK ) return( EDMS_PATHSET );
    sprintf(phyname,"%s/%s.",dmspath,eptr);
  }
  else
  { sprintf(phyname,"%s.",eptr); }
  for(i=0;i<pltcnt;i++)
  { if( pltapi[i].plttype != 1 ) continue;
    sprintf(tmpname,"%s%s",phyname,pltapi[i].pltname);
    if( access(tmpname,F_OK) == 0 ) return( EDMS_DBEXIST );
    if( strcmp(plt,pltapi[i].pltname)==0 ) pltype=i;
  }
  if( pltype == -1 ) return(EDMS_FAIL);
  strcat(phyname,plt);
  stat = pltapi[pltype].dbcrt(phyname,cfgstr);
  if (stat != EDMS_OK)
    log_write(LOG_ERR,"DMSDBCRT: E%04x DB(%s) create error!\n",stat,phyname);
  strcpy(physical_name,phyname);
  return(stat);
}

/*---------------------------------------------------------------------------*/
int dms_dbdlt(char *dbname,char physical_name[])
/*---------------------------------------------------------------------------*/
{ int i,stat,pltype=-1;
  char tmpname[MAX_DMS_FNAME],cmd[MAX_DMS_FNAME],*eptr,*ptr;
  char phyname[MAX_DMS_FNAME],dmspath[MAX_DMS_STRL*10];
  struct dirent  *dp;
  DIR    *dirp;

  log_write(LOG_INF,"DMSDBDLT: DB(%s)\n",dbname);
  if( (eptr=getenv(dbname)) == NULL ) eptr = dbname;
  strcpy(physical_name,eptr);
  if(eptr[0] != '/')
  { 
    if( getdmspath(dmspath,"A") != EDMS_OK ) return(EDMS_PATHSET);
    if( (ptr=strtok(dmspath,"#")) != NULL )
    do
    {
      sprintf(tmpname,"%s/%s",ptr,eptr);
      for(i=0; i<pltcnt; i++)
      { sprintf(phyname,"%s.%s",tmpname,pltapi[i].pltname);
        if( access(phyname,F_OK) == 0 ) { pltype=i; break; }
      }
    } while( (ptr=strtok(NULL,"#")) != NULL && pltype == -1 );
  }
  else
  {
    sprintf(tmpname,"%s",eptr);
    for(i=0; i<pltcnt; i++)
    { sprintf(phyname,"%s.%s",tmpname,pltapi[i].pltname);
      if( access(phyname,F_OK) == 0 ) { pltype=i; break; }
    }
  }
  if(pltype == -1)
  { log_write(LOG_ERR,"DMSDBDLT: E%04x DB(%s) not exist.\n",EDMS_NODB,tmpname);
    return(EDMS_NODB);
  }

  dirp=opendir(phyname);
  while( (dp=readdir(dirp)) != NULL )
  { if(dp->d_name[0] != '.')
    { log_write(LOG_ERR,"DMSDBDLT: E%04x DB(%s) not empty!\n",EDMS_NOTEMPTY,phyname);  
      closedir(dirp);
      return(EDMS_NOTEMPTY);
    }
  }
  closedir(dirp);

  stat = pltapi[pltype].dbdlt(phyname);
  if (stat != EDMS_OK)
  { log_write(LOG_ERR,"DMSDBDLT: E%04x DB(%s) delete err !\n",stat,phyname); }
  else
  {
    strcpy(cmd,getcfgname(phyname));
    if( access(cmd,F_OK) == 0 ) unlink(cmd);
  }
  strcpy(physical_name,phyname);
  return(stat);
}

/*---------------------------------------------------------------------------*/
int dms_flst(char *dbname,DMST *flstbuf,int *flstnum,char physical_name[])
/*---------------------------------------------------------------------------*/
{ int i,stat,pltype=-1;
  char tmpname[MAX_DMS_FNAME],*eptr,*ptr;
  char phyname[MAX_DMS_FNAME],dmspath[MAX_DMS_STRL*10];

  log_write(LOG_INF,"DMSFLST: File(%s)\n",dbname);
  if( (eptr=getenv(dbname)) == NULL ) eptr = dbname;
  strcpy(physical_name,eptr);
  if(eptr[0] != '/')
  {
    if( getdmspath(dmspath,"A") != EDMS_OK ) return(EDMS_PATHSET);
    if( (ptr=strtok(dmspath,"#")) != NULL )
    do
    {
      sprintf(tmpname,"%s/%s",ptr,eptr);
      for(i=0; i<pltcnt; i++)
      { sprintf(phyname,"%s.%s",tmpname,pltapi[i].pltname);
        if( access(phyname,F_OK) == 0 ) { pltype=i; break; }
      }
    } while( (ptr=strtok(NULL,"#")) != NULL && pltype == -1 );
  }
  else
  {
    sprintf(tmpname,"%s",eptr);
    for(i=0; i<pltcnt; i++)
    { sprintf(phyname,"%s.%s",tmpname,pltapi[i].pltname);
      if( access(phyname,F_OK) == 0 ) { pltype=i; break; }
    }
  }
/******
  if(eptr[0] != '/')
  { if( getdmspath(dmspath,"F") != EDMS_OK ) return(EDMS_PATHSET);
    sprintf(tmpname,"%s/%s",dmspath,eptr);
  }
  else
    sprintf(tmpname,"%s",eptr);
  for(i=0; i<pltcnt; i++)
  { sprintf(phyname,"%s.%s",tmpname,pltapi[i].pltname);
    if( access(phyname,F_OK) == 0 ) { pltype=i; break; }
  }
******/
  if(pltype == -1)
  { log_write(LOG_ERR,"DMSFLST: E%04x DB(%s) not exist.\n",EDMS_NODB,tmpname);
    return(EDMS_NODB);
  }
  stat = pltapi[pltype].flst(phyname,flstbuf,flstnum);
  if (stat != EDMS_OK)
    log_write(LOG_ERR,"DMSFLST: E%04x DB(%s) list err !\n",stat,dbname);
  strcpy(physical_name,phyname);
  return(stat);
}

/*---------------------------------------------------------------------------*/
int dms_fchk(char *dmsfile,char physical_name[])
/*---------------------------------------------------------------------------*/
{ int stat,pltype;
  char phyname[MAX_DMS_FNAME];

  log_write(LOG_INF,"DMSFCHK: File(%s)\n",dmsfile);
  strcpy(physical_name,dmsfile);
  GET_REALNAME(dmsfile,"DMSFCHK",F_LEVEL);
  stat = pltapi[pltype].fchk(phyname);
  if (stat != EDMS_OK)
  { log_write(LOG_ERR,"DMSFCHK: E%04x File(%s) check err !\n",stat,dmsfile); }
  strcpy(physical_name,phyname);
  return(stat);
}

/*---------------------------------------------------------------------------*/
int dms_frog(char *dmsfile,char physical_name[])
/*---------------------------------------------------------------------------*/
{ int stat,pltype;
  char phyname[MAX_DMS_FNAME];

  log_write(LOG_INF,"DMSFROG: File(%s)\n",dmsfile);
  strcpy(physical_name,dmsfile);
  GET_REALNAME(dmsfile,"DMSFROG",F_LEVEL);
  stat = pltapi[pltype].frog(phyname);
  if (stat != EDMS_OK)
  { log_write(LOG_ERR,"DMSFROG: E%04x File(%s) reorgnize !\n",stat,dmsfile); }
  strcpy(physical_name,phyname);
  return(stat);
}

/*---------------------------------------------------------------------------*/
int dms_fcrt(char *dmsfile,char physical_name[],char *cfgstr)
/*---------------------------------------------------------------------------*/
{ int stat,pltype;
  char phyname[MAX_DMS_FNAME];

  log_write(LOG_INF,"DMSFCRT: File(%s)\n",dmsfile);
  strcpy(physical_name,dmsfile);
  GET_REALNAME(dmsfile,"DMSFCRT",D_LEVEL);
  stat = pltapi[pltype].fcrt(phyname,cfgstr);
  if (stat != EDMS_OK)
  { log_write(LOG_ERR,"DMSFCRT: E%04x File(%s) create err!\n",stat,dmsfile); }
  strcpy(physical_name,phyname);
  return(stat);
}

/*---------------------------------------------------------------------------*/
int dms_fdlt(char *dmsfile,char physical_name[])
/*---------------------------------------------------------------------------*/
{ int stat,pltype;
  char phyname[MAX_DMS_FNAME],cmd[MAX_DMS_FNAME];

  log_write(LOG_INF,"DMSFDLT: File(%s)\n",dmsfile);
  strcpy(physical_name,dmsfile);
  GET_REALNAME(dmsfile,"DMSFDLT",F_LEVEL);
  stat = pltapi[pltype].fdlt(phyname);
  if (stat != EDMS_OK)
  { log_write(LOG_ERR,"DMSFDLT: E%04x file(%s) delete err!\n",stat,dmsfile); }
  strcpy(physical_name,phyname);
  return(stat);
}

/*---------------------------------------------------------------------------*/
int dms_fopn(char *dmsfile,u_int access_mode,DMS *dmsp)
/*---------------------------------------------------------------------------*/
{ int stat,pltype;
  char phyname[MAX_DMS_FNAME];

  log_write(LOG_INF,"DMSFOPN: File(%s)\n",dmsfile);
  GET_REALNAME(dmsfile,"DMSFOPN",F_LEVEL);
  stat = pltapi[pltype].fopn(phyname,access_mode,dmsp);
  if (stat != EDMS_OK)
  { log_write(LOG_ERR,"DMSFOPN: E%04x file(%s) open err!\n",stat,dmsfile);
    return(stat);
  }

  dmsp->pltype  = (PLT_ENUM) pltype;
  strcpy(dmsp->lgcname,dmsfile);
  dmsp->access  = access_mode ;
  
  dmsp->keylen  = dmsconfig.keylen;
/*
  strcpy(dmsp->phyname,phyname);
  dmsp->fcls    = pltapi[pltype].fcls ;
  dmsp->rget    = pltapi[pltype].rget ;
  dmsp->rput    = pltapi[pltype].rput ;
  dmsp->rdlt    = pltapi[pltype].rdlt ;
  dmsp->rlst    = pltapi[pltype].rlst ;
  dmsp->conf    = pltapi[pltype].conf ;
  dmsp->init    = pltapi[pltype].init ;
  dmsp->exit    = pltapi[pltype].exit ;
*/  
  return(EDMS_OK);
}

/*---------------------------------------------------------------------------*/
int dms_rget(DMS *dmsp,DMST *keyp,DMST *data)
/*---------------------------------------------------------------------------*/
{ int stat;

  log_write(LOG_INF,"DMSRGET: File(%s) Key(%s)\n",dmsp->lgcname,keyp->data);
  stat = dmsp->rget(dmsp,keyp,data);
  if (stat != EDMS_OK)
  { log_write(LOG_ERR,"DMSRGET: E%04x Key(%s) get err!\n",stat,keyp->data); }
  else
  { if (stat > EDMS_OLVL_MAX) stat = EDMS_OLVL_MAX; }
  return(stat);
}

/*---------------------------------------------------------------------------*/
int dms_rput(DMS *dmsp,DMST *keyp,DMST *datp)
/*---------------------------------------------------------------------------*/
{ int stat;

  log_write(LOG_INF,"DMSRPUT: File(%s) Key(%s)\n",dmsp->lgcname,keyp->data);
  stat = dmsp->rput(dmsp,keyp,datp);
  if (stat != EDMS_OK)
  { log_write(LOG_ERR,"DMSRPUT: E%04x Key(%s) put err!\n",stat,keyp->data); }
  else
  { if (stat > EDMS_OLVL_MAX) stat = EDMS_OLVL_MAX; }
  return(stat);
}

/*---------------------------------------------------------------------------*/
int dms_rlst(DMS *dmsp,DMST *keyset,char *keybuf,int *keynum)
/*---------------------------------------------------------------------------*/
{ int stat;

  log_write(LOG_INF,"DMSRLST: File(%s) Key(%s)\n",dmsp->lgcname,keyset->data);
  stat = dmsp->rlst(dmsp,keyset,keybuf,keynum);
  if (stat != EDMS_OK)
  { log_write(LOG_ERR,"DMSRLST: E%04x Key(%s) list err!\n",stat,keyset->data); }
  else
  { if (stat > EDMS_OLVL_MAX) stat = EDMS_OLVL_MAX; }
  return(stat);
}

/*---------------------------------------------------------------------------*/
int dms_rdlt(DMS *dmsp,DMST *keyp)
/*---------------------------------------------------------------------------*/
{ int stat;

  log_write(LOG_INF,"DMSRDLT: File(%s) Key(%s)\n",dmsp->lgcname,keyp->data);
  stat = dmsp->rdlt(dmsp,keyp);
  if (stat != EDMS_OK)
  { log_write(LOG_ERR,"DMSRDLT: E%04x Key(%s) delete err!\n",stat,keyp->data); }
  else
  { if (stat > EDMS_OLVL_MAX) stat = EDMS_OLVL_MAX; }
  return(stat);
}

/*---------------------------------------------------------------------------*/
int dms_fcls(DMS *dmsp)
/*---------------------------------------------------------------------------*/
{ int stat;

  log_write(LOG_INF,"DMSFCLS: File(%s)\n",dmsp->lgcname);
  stat = dmsp->fcls(dmsp);
  if (stat != EDMS_OK)
  {
   log_write(LOG_ERR,"DMSFCLS: E%04x File(%s) close err!\n",stat,dmsp->lgcname);
  }
  dms_clear(dmsp);
  return(stat);
}

/*---------------------------------------------------------------------------*/
int dms_conf(CONF_ENUM conftype,char *argument)
/*---------------------------------------------------------------------------*/
{ char tempstr[MAX_DMS_STRL],*flog,*logfile;
  int stat=EDMS_OK,i,j,order[KEYCNT]={0,0,0};
  static FILE *logptr=NULL;
  static int smapcnt=0;

  log_write(LOG_INF,"DMSCFG: type(%04x) argument(%s)\n",conftype,argument);

  switch(conftype)
  { 
    case S_LOG:     stat = log_level( i = atoi(argument) );
                    if( stat == EDMS_OK ) dmsconfig.log = i;
                    break;
    case G_LOG:     sprintf( argument, "%d", dmsconfig.log );
                    break;
    case S_KEYLEN:  stat = key_use( i = atoi(argument) );
                    if( stat != EDMS_FAIL ) dmsconfig.keylen = i;
                    stat = EDMS_OK;
                    break;
    case G_KEYLEN:  sprintf( argument, "%d", dmsconfig.keylen );
                    break;
    case S_RORDER:  sscanf( argument,"%d:%d:%d",&order[0],&order[1],&order[2] );
                    for( i = 0 ; i < KEYCNT ; i++ )
                    { 
                      stat = key_use(order[i]);
                      if( stat==EDMS_FAIL && order[i]!=0 ) return(EDMS_KEYLEN);
                      dmsconfig.rorder[i] = order[i];
                    }
                    stat = EDMS_OK;
                    break;
    case G_RORDER:
                    break;
    case S_WORDER:  sscanf( argument,"%d:%d:%d",&order[0],&order[1],&order[2] );
                    for( i = 0 ; i < KEYCNT ; i++ )
                    {  
                      stat = key_use(order[i]);
                      if( stat==EDMS_FAIL && order[i]!=0 ) return(EDMS_KEYLEN);
                      dmsconfig.worder[i] = order[i];
                    }
                    stat = EDMS_OK;
                    break;
    case G_WORDER:
                    break;
    case S_MAPFILE: sscanf( argument,"%d:%d,%s",&i,&j,tempstr);
                    if( smapcnt == KEYCNT )
                    { stat = EDMS_CONFIG; break; }
                    dmsmap[smapcnt].keymaplen1 = i;
                    dmsmap[smapcnt].keymaplen2 = j;
                    dmsmap[smapcnt].mapline = -1;
                    strcpy( dmsconfig.mapfile[smapcnt], tempstr );
                    smapcnt++;
                    dmsconfig.mapcnt = smapcnt;
                    break;
    case G_MAPFILE:
                    break;
    default:        stat = EDMS_BADARG;
  }

  if(stat != EDMS_OK) return(stat);

  if( conftype==S_LOG && atoi(argument)!=LOG_MIN && logptr==NULL )
  { if((flog=getenv("dmslog"))==NULL)
    { sprintf(tempstr,"%s",DMSLOG); }
    else
    { strcpy(tempstr,flog); }
    logfile = tempstr;
    if ( (logptr=log_open(logfile)) == NULL )
    { return(EDMS_LOGFILE); }
    else
     log_write(LOG_INF,"DMSCFG: type(%04x) value(%s)\n",conftype,argument);
  }

  if ( conftype==S_LOG && atoi(argument)==LOG_MIN)
  { if ( logptr != NULL ) log_close(logptr); 
    log_fptr(stderr);
  }

  return(stat);
}

/*---------------------------------------------------------------------------*/
/*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/
/*---------------------------------------------------------------------------*/

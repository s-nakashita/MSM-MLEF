/*---------------------------------------------------------------------------*\
 *                                                                           *
 *  Program File : ufs_dms.c                                                 *
 *  Purpose      : To support UFS platform for DMS                           *
 *  Remark       : The dmsfile name is physical name for this module.        *
 *                                                                           *
\*---------------------------------------------------------------------------*/

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>
#include <dirent.h>
#include <errno.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <fcntl.h>
/*---------------------------------------------------------------------------*/
#include "dms.h"
#include "dmserrno.h"
#include "dms_lib.h"
#include "key_lib.h"
#include "ufs_dms.h"
#include "dms_log.h"

extern DMSCONF dmsconfig;
extern int keyuse;
extern DMS_KEY keydef[];

#define KEYREORDER 1
/*---------------------------------------------------------------------------*/
/*++++++++++++++++++< Routines for internal reference >++++++++++++++++++++++*/
/*---------------------------------------------------------------------------*/
static int ufs_key_level(char *keymask)
/*---------------------------------------------------------------------------*/
{ int  i,keylevel,keyvalue,keylen;

  keylen = strlen(keymask);
  for( keylevel=i=0 ; i < keylen ; i++ )
  { if( (keyvalue=keymask[i]-0x30) > keylevel ) keylevel = keyvalue ; }
  return( keylevel );
}

/*---------------------------------------------------------------------------*/
static int ufs_key_levelen(char *keymask,int masklevel)
/*---------------------------------------------------------------------------*/
{ int i,j,len;
  char maskchar=0x30|masklevel;
 
  len = strlen(keymask); 
  for( i=j=0 ; i < len && keymask[i]!=0 ; i++ )
  { if( keymask[i] == maskchar ) j++; }
  return( j ); 
}

/*---------------------------------------------------------------------------*/
static char *ufs_key_reorder(char *keypart,char *keymask,char *ordermask,int masklevel,char flag)
/*---------------------------------------------------------------------------*/
{ static char orderkey[MAXKEYL],maskord[MAXKEYL];
  char maskchar=0x30|masklevel;
  int keylevel,len,i,j,k;

  strcpy(orderkey,keypart);
  len = strlen(keymask);
#ifdef KEYREORDER
  for( i=j=0 ; i<len && keymask[i]!=0 ; i++ )
  { if( keymask[i] == maskchar ) maskord[j++] = ordermask[i]; }
  maskord[j] = '\0';
  keylevel = ufs_key_level(maskord);
  if( flag == 'e' )
  { for( i=1,j=0 ; i<=keylevel ; i++ )
    { for( k=0,maskchar=0x30|i ; k<strlen(maskord) ; k++ )
      { if( maskord[k] == maskchar ) orderkey[j++] = keypart[k]; }
    }
    orderkey[j]='\0';
  }
  else
  { for( i=1,j=0 ; i<=keylevel ; i++ )
    { for( k=0,maskchar=0x30|i ; k<strlen(maskord) ; k++ )
      { if( maskord[k] == maskchar ) orderkey[k] = keypart[j++]; } 
    }
    orderkey[k]='\0';
  }
#endif
  return( orderkey );
}

/*---------------------------------------------------------------------------*/
static char *ufs_key_keymask(int keylen,int *keylevel)
/*---------------------------------------------------------------------------*/
{ 
  static char *keymask;

  key_use(keylen);
  keymask = keydef[keyuse].keymask;
  *keylevel = keydef[keyuse].keylevel;
  return(keymask);
}

/*---------------------------------------------------------------------------*/
static char *ufs_key_ordermask(int keylen,int *orderlevel)
/*---------------------------------------------------------------------------*/
{ 
  static char *ordermask;

  key_use(keylen);
  ordermask  = keydef[keyuse].ordermask;
  *orderlevel = ufs_key_level(ordermask);
  return(ordermask);
}

/*---------------------------------------------------------------------------*/
static char *ufs_key_extract(char *key,char *keymask,char *ordermask,int masklevel)
/*---------------------------------------------------------------------------*/
{ static char keypart[MAXKEYL];
  char   maskchar=0x30|masklevel;
  int    len,i,j;

  len = strlen(keymask) ;
  for( i=j=0 ; i<len && key[i]!=0 ; i++)
  { if( keymask[i] == maskchar ) keypart[j++] = key[i] ; }
  keypart[j] = 0 ;
  return( ufs_key_reorder(keypart,keymask,ordermask,masklevel,'e') );
}

/*---------------------------------------------------------------------------*/
static char *ufs_key_insert(char *keypart,char *keymask,char *ordermask,int masklevel)
/*---------------------------------------------------------------------------*/
{ static char key[MAXKEYL];
  char   *ordpart,maskchar=0x30|masklevel;
  int    len,i,j;

  ordpart = keypart ;
  ordpart = ufs_key_reorder(keypart,keymask,ordermask,masklevel,'i') ;
  len = strlen(keymask) ;
  for( i=j=0 ; i<len && ordpart[j]!=0 ; i++)
  { if( keymask[i] == maskchar ) key[i] = ordpart[j++] ; }
  key[len] = 0 ;
  return(key);
}

/*---------------------------------------------------------------------------*/
static int ufs_key_maskcmp(char *k1,char *k2,char *keymask,char *ordermask,int masklevel)
/*---------------------------------------------------------------------------*/
{ char *kr,maskchar=0x30|masklevel;
  int  len,i,j;

  len = strlen(keymask) ;
  kr = ufs_key_reorder(k1,keymask,ordermask,masklevel,'i');
  for( i=j=0 ; i < len ; i++ )
  { if( keymask[i] == maskchar )
    { if( (kr[j++] != k2[i]) && (k2[i] != '?') ) return(1); }
  }
  return(0);
}

/*---------------------------------------------------------------------------*/
static char *ufs_path_composer(char *dmsfile,char *key)
/*---------------------------------------------------------------------------*/
{ static char odms[MAX_DMS_FNAME];
  static char keymask[MAXKEYL],ordermask[MAXKEYL];
  static char path[MAX_DMS_FNAME];
  static int  skeylen,keylevel,orderlevel;
  char   *kpartptr;
  int    i,keylen;

  keylen = strlen(key);
  if( strcmp(odms,dmsfile) != 0 || keylen != skeylen )
  { strcpy(odms,dmsfile) ; 
    skeylen = keylen ;
    strcpy(keymask,ufs_key_keymask(keylen,&keylevel)); 
    strcpy(ordermask,ufs_key_ordermask(keylen,&orderlevel)); 
  }

  strcpy(path,dmsfile);
  for( i=1 ; i <= keylevel ; i++ )
  { kpartptr = ufs_key_extract(key,keymask,ordermask,i);
    strcat(path,"/"); strcat(path,kpartptr);
  }
  return(path);
}

/*-----------------------------------------------------------------------------
static char *ufs_path_decomposer(UFS *ufs,char *dmsfile,char *path)
-------------------------------------------------------------------------------
{ static char odms[MAX_DMS_FNAME];
  static char keymask[MAXKEYL],ordermask[MAXKEYL],key[MAXKEYL];
  static int  keylevel,orderlevel;
  char   *keypart,*keyptr,pathbuf[MAX_DMS_FNAME];
  int    i,keylen;

  if( strcmp(odms,dmsfile) != 0 )
  { strcpy(odms,dmsfile) ; 
    strcpy(keymask,ufs_key_keymask(keylen,&keylevel)); 
    strcpy(ordermask,ufs_key_ordermask(keylen,&orderlevel)); 
  }

  strcpy(pathbuf,path);
  for( i=keylevel ; i > 0 ; i-- )
  { keypart = strrchr(pathbuf,'/') ;
    keyptr  = ufs_key_insert(++keypart,keymask,ordermask,i);
    *(--keypart) = '\0' ;
  }
  strcpy(key,keyptr);
  return(key);
}
-----------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
static int ufs_path_creator(char *path)
/*---------------------------------------------------------------------------*/
{ char *pathptr,pathbuf[MAX_DMS_FNAME];
  int  i,nlvl;

  strcpy(pathbuf,path);

  nlvl = 0 ;
  pathptr = strrchr(pathbuf,'/'); *pathptr = '\0' ;
  while( access(pathbuf,F_OK) != 0 )
  { pathptr = strrchr(pathbuf,'/'); *pathptr = '\0' ; nlvl++ ; }

  for( i=0 ; i < nlvl ; i++)
  { pathbuf[strlen(pathbuf)] = '/' ;
    if( mkdir(pathbuf,0755) != 0 ) return( EDMS_RCRTERR );
  }
  return( EDMS_OK );
}

/*---------------------------------------------------------------------------*/
static int ufs_path_remover(int keylen,char *dmsfile,char *path)
/*---------------------------------------------------------------------------*/
{ static char odms[MAX_DMS_FNAME];
  static int  keylevel;
  char   *pathptr,pathbuf[MAX_DMS_FNAME];
  struct dirent  *dp;
  DIR    *dirp;
  int    n,fn;

  if( strcmp(odms,dmsfile) != 0 )
  { strcpy(odms,dmsfile) ; 
    key_use(keylen);
    ufs_key_keymask(keylen,&keylevel);
  }
  strcpy(pathbuf,path);
  for( n=1 ; n <= keylevel ; n++ )
  { pathptr = strrchr(pathbuf,'/') ; *pathptr = '\0' ;
    dirp = opendir(pathbuf);
    for( fn=0 ; (dp=readdir(dirp)) != NULL ; fn++ );
    closedir(dirp);
    if( fn <= 2 ) rmdir(pathbuf);
  }
  return(EDMS_OK);
}

#define MAX_LOCK_RETRY 5
/*---------------------------------------------------------------------------*/
static int ufs_lock(int fd,int mode,int *locktype) /* F_RDLCK,F_WRLCK,F_UNLCK */
/*---------------------------------------------------------------------------*/
{ int    stat,retry;
  struct flock lck;

  lck.l_type   = *locktype;
  lck.l_whence = 0;
  lck.l_start  = 0L;
  lck.l_len    = 0L;

  stat = retry = 0 ;
  if( mode == F_SETLK )
  {
/*
#ifdef __alpha
    for( ; fcntl(fd,F_SETLK,&lck) != -1 ; stat=0,retry++ )
#else
*/
    for( ; fcntl(fd,F_SETLK,&lck) < 0 ; stat=0,retry++ ) 
/*
#endif
*/
    { stat = -1 ;
      if( errno!=EAGAIN  &&  errno!=EACCES ) break ;
      if( retry < MAX_LOCK_RETRY ) sleep(1);
    }
  }
  else
  {
    if( fcntl(fd,F_GETLK,&lck) < 0 ) stat = -1 ;
  }
  *locktype = lck.l_type ;
  return( stat );
}

/*---------------------------------------------------------------------------*/
/*++++++++++++++++++< Routines for external reference >++++++++++++++++++++++*/
/*---------------------------------------------------------------------------*/
int ufs_get_configure(char *dmsfile,UFS *ufs)
/*---------------------------------------------------------------------------*/
{ char *ptr,*path,*name;
  char cfgfile[MAX_DMS_FNAME],tmpstr[MAX_DMS_FNAME]; 
  char cfgstr[MAX_DMS_STRL],cfgval[MAX_DMS_STRL],buf[MAX_DMS_STRL]; 
  int  i,j;
  FILE *fp;

/***
  strcpy(ufs->keymask,"222111221111111122222222");
  strcpy(ufs->ordermask,"333222331111111133333333");
***/

  ufs->keylen = keydef[keyuse].keylen;
  strcpy(ufs->keymask,keydef[keyuse].keymask);
  strcpy(ufs->ordermask,keydef[keyuse].ordermask);

  strcpy(tmpstr,dmsfile);
  for( i=0 ; i<2 ; i++ )
  {
    if( (ptr=strrchr(tmpstr,'/')) == NULL ) break;
    *ptr='\0'; ptr++;
    sprintf(cfgfile,"%s/.%s_cfg",tmpstr,ptr); 
    if( (fp=fopen(cfgfile,"r")) != NULL )
    { 
      while( fgets(cfgstr,90,fp) != NULL )
      {
        if( !strncmp(cfgstr,KEYLEN,j=strlen(KEYLEN)) ) 
        { ufs->keylen = atoi(&cfgstr[j+1]); break; }
      }
      fseek(fp,0,SEEK_SET);
      key_use(ufs->keylen);
      strcpy(ufs->keymask,keydef[keyuse].keymask);
      strcpy(ufs->ordermask,keydef[keyuse].ordermask);
      while( fgets(cfgstr,90,fp) != NULL )
      {
        if( !strncmp(cfgstr,KEYMASK,strlen(KEYMASK)) ) 
        { strcpy(ufs->keymask,&cfgstr[j+1]); }
        if( !strncmp(cfgstr,ORDERMASK,strlen(ORDERMASK)) ) 
        { strcpy(ufs->ordermask,&cfgstr[j+1]); }
      }
      fclose(fp);
    }
  }
  return( EDMS_OK );
}

/*---------------------------------------------------------------------------*/
int ufs_set_configure(char *cfgfile,UFS *ufs)
/*---------------------------------------------------------------------------*/
{
  FILE *fp;

  if( (fp=fopen(cfgfile,"w")) != NULL ) 
  { fprintf(fp,"KEYMASK=%s\n",ufs->keymask);
    fprintf(fp,"ORDERMASK=%s\n",ufs->ordermask);
    fclose(fp);
    return( EDMS_OK );
  }
  else
  { return( EDMS_FAIL ); }
}

/*---------------------------------------------------------------------------*/
int ufs_init(void (*sig_handler)())
/*---------------------------------------------------------------------------*/
{
  return(EDMS_OK);
}

/*---------------------------------------------------------------------------*/
int ufs_exit(int exitno)
/*---------------------------------------------------------------------------*/
{
  return(EDMS_OK);
}

/*---------------------------------------------------------------------------*/
int ufs_dbcrt(char *db_name,char *cfgstr)
/*---------------------------------------------------------------------------*/
{ char *tptr,cfgname[MAX_DMS_FNAME];
  int stat;

  if( (tptr=strrchr(db_name,'.')) == NULL ) return( EDMS_NAME );
  if( access(db_name,F_OK) == 0) return( EDMS_DBEXIST );
  if( mkdir(db_name,0775)  != 0) return( EDMS_CRTDB   );
  strcpy(cfgname,getcfgname(db_name));
  stat = ufs_ckcfg(cfgname,"W",cfgstr);
  return(stat);
}

/*---------------------------------------------------------------------------*/
int ufs_dbdlt(char *db_name)
/*---------------------------------------------------------------------------*/
{ char tmp[MAX_DMS_STRL],cmd[MAX_DMS_FNAME],*tptr,*dptr;

  strcpy(tmp,db_name);
  if( (tptr=strrchr(tmp,'.')) == NULL ) return( EDMS_NAME );
  if( access(db_name,F_OK) != 0 ) return(EDMS_NODB);
  
  if( (dptr=strrchr(tmp,'/')) != NULL )
  { *tptr=*dptr='\0'; dptr++;
    sprintf(cmd,"%s/.%s_cfg",tmp,dptr);
    unlink(cmd);
  }
  sprintf(cmd,"rm -r -f '%s'",db_name);
  system(cmd);
  return(EDMS_OK);
}

/*---------------------------------------------------------------------------*/
int ufs_flst(char *db_name,DMST *flstbuf,int *flstnum)
/*---------------------------------------------------------------------------*/
{ char tempbuf[MAX_DMS_STRL],cmd[128],fname[64],*ptr,*fptr;
  FILE *pfile;
  int  fsize,len,sizecnt=0;

  if( access(db_name,F_OK) != 0) return( EDMS_NODB );
  *flstnum = 0 ;
#ifdef SUN
  sprintf(cmd,"sdu -k -s %s/* 2> /dev/null",db_name);
#else
  sprintf(cmd,"du -s %s/* 2> /dev/null",db_name);
#endif
  if( (pfile=popen(cmd,"r")) == NULL) return( EDMS_FAIL );
  fgets(tempbuf,MAX_DMS_STRL,pfile);
  ptr = flstbuf->data;
  while( !feof(pfile) )
  { 
    if( isdigit( tempbuf[0] ) )
    {
      sscanf(tempbuf,"%d %s",&fsize,fname);
      if((fptr=strrchr(fname,'/')) != NULL)
        fptr++;
      else
        fptr=fname;
      if( *fptr != '*' )
      { sprintf(tempbuf,"%-15s\t%10d",fptr,fsize);
        len = strlen(tempbuf)+1;
        if(sizecnt + len <= flstbuf->size)
        { memcpy(ptr,tempbuf,len);
          (*flstnum)++ ;
          ptr+=len;
          sizecnt += len;
        }
        else
        { pclose(pfile); return(EDMS_FEWSIZE); }
      }
    }
    fgets(tempbuf,MAX_DMS_STRL,pfile);
  }
  pclose(pfile);
  return(EDMS_OK);
}

/*---------------------------------------------------------------------------*/
int ufs_fchk(char *physical_name)
/*---------------------------------------------------------------------------*/
{ return(EDMS_OK); }

/*---------------------------------------------------------------------------*/
int ufs_frog(char *physical_name)
/*---------------------------------------------------------------------------*/
{ return(EDMS_OK); }

/*---------------------------------------------------------------------------*/
int ufs_fcrt(char *physical_name,char *cfgstr)
/*---------------------------------------------------------------------------*/
{ char cfgname[MAX_DMS_FNAME];
  int stat;

  if( access(physical_name,F_OK) == 0) return( EDMS_FILEXIST );
  if( mkdir(physical_name,0775)  != 0) return( EDMS_CRTFILE  );
  strcpy(cfgname,getcfgname(physical_name));
  stat = ufs_ckcfg(cfgname,"W",cfgstr);
  return( stat );
}

/*---------------------------------------------------------------------------*/
int ufs_fdlt(char *physical_name)
/*---------------------------------------------------------------------------*/
{ char *ptr,tmp[MAX_DMS_FNAME],cmd[MAX_DMS_FNAME];

  if(access(physical_name,F_OK) != 0) return(EDMS_NOFILE);
  strcpy(tmp,physical_name);
  if( (ptr=strrchr(tmp,'/')) != NULL ) 
  { *ptr='\0'; ptr++;
    sprintf(cmd,"%s/.%s_cfg",tmp,ptr);
    unlink(cmd);
  }
  sprintf(cmd,"rm -r -f '%s'",physical_name);
  system(cmd);
  return(EDMS_OK);
}

/*---------------------------------------------------------------------------*/
int ufs_fopn(char *physical_name,u_int access_mode,DMS *dmsp)
/*---------------------------------------------------------------------------*/
{ int stat;
  UFS *ufs;
  char cfgstr[MAX_DMS_STRL];

  cfgstr[0]='\0';
  if( (ufs=(UFS *)malloc(sizeof(UFS))) == NULL ) return( EDMS_NOMEM );
  ufs_get_configure(physical_name,ufs);
  if( access(physical_name,F_OK) != 0 )
  { if( access_mode & ODMS_CREATE )
    { 
      if( (stat=ufs_fcrt(physical_name,cfgstr)) != EDMS_OK )  
      { free(ufs); return(stat); }
    }
    else
    { free(ufs); return( EDMS_NOFILE ) ; }
  }

  if( access_mode & ODMS_RDWR )
  { 
    if( access(physical_name,W_OK) != 0 ) 
    { free(ufs); return( EDMS_NRWPERMIT ) ; } 
  }

  dmsp->pltype  = DMS_UFS ;
  strcpy( dmsp->phyname , physical_name ) ;
  dmsp->lrucache= NULL ;
  dmsp->internal= (void *) ufs ;
  dmsp->access  = access_mode ;
  dmsp->keylen  = ufs->keylen ; 
  dmsp->fcls    = ufs_fcls ;
  dmsp->rget    = ufs_rget ;
  dmsp->rput    = ufs_rput ;
  dmsp->rdlt    = ufs_rdlt ;
  dmsp->rlst    = ufs_rlst ;
  dmsp->conf    = ufs_conf ;
  dmsp->init    = ufs_init ;
  dmsp->exit    = ufs_exit ;

  return(EDMS_OK);
}

/*---------------------------------------------------------------------------*/
int ufs_rget_orig(DMS *dmsp,DMST *keyp,DMST *datp)
/*---------------------------------------------------------------------------*/
{ char *path;
  int  fd,tmpsize,lockflag;
  UFS  *ufs;

  uppercase(keyp->data);
  /* if( dmsp->keylen != keyp->size ) return( EDMS_KEYLEN ); */
  key_use( strlen(keyp->data) );
  tmpsize = key_size(keyp->data) * key_nitm(keyp->data);
  if( datp->size < tmpsize ) return( EDMS_FEWSIZE );

  ufs = (UFS *)dmsp->internal;
  path = ufs_path_composer(dmsp->phyname,keyp->data);
  if( access(path,F_OK) != 0 ) return( EDMS_NOREC );

  if( (fd=open(path,O_RDONLY)) < 0 ) return( EDMS_ROPERR );

  lockflag = F_RDLCK ;
  if( ufs_lock(fd,F_SETLK,&lockflag) != 0 ) 
  { close(fd); return( EDMS_RECLOCK ); }

  if( read(fd,datp->data,datp->size) != datp->size ) 
  {
    lockflag = F_UNLCK ;
    if( ufs_lock(fd,F_SETLK,&lockflag) != 0 ) return( EDMS_RECLOCK ); 
    close(fd);
    return( EDMS_RRDERR ); 
  }

  lockflag = F_UNLCK ;
  if( ufs_lock(fd,F_SETLK,&lockflag) != 0 ) 
  { close(fd); return( EDMS_RECLOCK ); }

  close(fd);
  return( EDMS_OK );
}

/*---------------------------------------------------------------------------*/
int ufs_rget(DMS *dmsp,DMST *keyp,DMST *datp)
/*---------------------------------------------------------------------------*/
{ char *path;
char *beg, temp;
  int  fd,tmpsize,lockflag;
int  i, dsize;
  UFS  *ufs;

  uppercase(keyp->data);
  /* if( dmsp->keylen != keyp->size ) return( EDMS_KEYLEN ); */
  key_use( strlen(keyp->data) );
  tmpsize = key_size(keyp->data) * key_nitm(keyp->data);
  if( datp->size < tmpsize ) return( EDMS_FEWSIZE );

  ufs = (UFS *)dmsp->internal;
  path = ufs_path_composer(dmsp->phyname,keyp->data);
  if( access(path,F_OK) != 0 ) return( EDMS_NOREC );

  if( (fd=open(path,O_RDONLY)) < 0 ) return( EDMS_ROPERR );

  lockflag = F_RDLCK ;
  if( ufs_lock(fd,F_SETLK,&lockflag) != 0 ) 
  { close(fd); return( EDMS_RECLOCK ); }

  if( read(fd,datp->data,datp->size) != datp->size ) 
  {
    lockflag = F_UNLCK ;
    if( ufs_lock(fd,F_SETLK,&lockflag) != 0 ) return( EDMS_RECLOCK ); 
    close(fd);
    return( EDMS_RRDERR ); 
  }

  lockflag = F_UNLCK ;
  if( ufs_lock(fd,F_SETLK,&lockflag) != 0 ) 
  { close(fd); return( EDMS_RECLOCK ); }

  close(fd);

  dsize = key_size(keyp->data);
  tmpsize = datp->size/dsize;
  switch( dsize )
  {
   case 16:
     for ( i = 0, beg = datp->data; i < tmpsize ; i++, beg += 16) {
         temp = beg[0];
         beg[0] = beg[15];
         beg[15] = temp;
         temp = beg[1];
         beg[1] = beg[14];
         beg[14] = temp;
         temp = beg[2];
         beg[2] = beg[13];
         beg[13] = temp;
         temp = beg[3];
         beg[3] = beg[12];
         beg[15] = temp;
         temp = beg[4];
         beg[4] = beg[11];
         beg[11] = temp;
         temp = beg[5];
         beg[5] = beg[10];
         beg[10] = temp;
         temp = beg[6];
         beg[6] = beg[9];
         beg[9] = temp;
         temp = beg[7];
         beg[7] = beg[8];
         beg[8] = temp;
     }
     break;
   case 8:
     for ( i = 0, beg = datp->data; i < tmpsize ; i++, beg += 8) {
         temp = beg[0];
         beg[0] = beg[7];
         beg[7] = temp;
         temp = beg[1];
         beg[1] = beg[6];
         beg[6] = temp;
         temp = beg[2];
         beg[2] = beg[5];
         beg[5] = temp;
         temp = beg[3];
         beg[3] = beg[4];
         beg[4] = temp;
     }
     break;
   case 4:
     for ( i = 0, beg = datp->data; i < tmpsize ; i++, beg += 4) {
         temp = beg[0];
         beg[0] = beg[3];
         beg[3] = temp;
         temp = beg[1];
         beg[1] = beg[2];
         beg[2] = temp;
     }
     break;
   case 2:
     for ( i = 0, beg = datp->data; i < tmpsize ; i++, beg += 2) {
         temp = beg[0];
         beg[0] = beg[1];
         beg[1] = temp;
     }
     break;
   default: break;
  }
  return( EDMS_OK );
}

/*---------------------------------------------------------------------------*/
int ufs_rput(DMS *dmsp,DMST *keyp,DMST *datp)
/*---------------------------------------------------------------------------*/
{ char *path;
  int  fd,tmpsize,lockflag;
  int  mode=O_CREAT|O_RDWR,permiss=S_IRUSR|S_IWUSR|S_IRGRP|S_IROTH;
  UFS  *ufs;

char *beg, temp;
int  i, dsize;

  uppercase(keyp->data);
  if( (dmsp->access & ODMS_RDWR) == 0 ) return( EDMS_NRWPERMIT );
  /* if( dmsp->keylen != keyp->size ) return( EDMS_KEYLEN ); */
  key_use( strlen(keyp->data) );
  tmpsize = key_size(keyp->data) * key_nitm(keyp->data);
  if( datp->size < tmpsize ) return( EDMS_FEWSIZE );

  ufs = (UFS *)dmsp->internal;
  path = ufs_path_composer(dmsp->phyname,keyp->data);
  ufs_path_creator(path);

  if( (fd=open(path,mode,permiss)) < 0 ) return( EDMS_ROPERR );

/* >>> */
  dsize = key_size(keyp->data);
  tmpsize = datp->size/dsize;
  switch( dsize )
  {
   case 16:
     for ( i = 0, beg = datp->data; i < tmpsize ; i++, beg += 16) {
         temp = beg[0];
         beg[0] = beg[15];
         beg[15] = temp;
         temp = beg[1];
         beg[1] = beg[14];
         beg[14] = temp;
         temp = beg[2];
         beg[2] = beg[13];
         beg[13] = temp;
         temp = beg[3];
         beg[3] = beg[12];
         beg[15] = temp;
         temp = beg[4];
         beg[4] = beg[11];
         beg[11] = temp;
         temp = beg[5];
         beg[5] = beg[10];
         beg[10] = temp;
         temp = beg[6];
         beg[6] = beg[9];
         beg[9] = temp;
         temp = beg[7];
         beg[7] = beg[8];
         beg[8] = temp;
     }
     break;
   case 8:
     for ( i = 0, beg = datp->data; i < tmpsize ; i++, beg += 8) {
         temp = beg[0];
         beg[0] = beg[7];
         beg[7] = temp;
         temp = beg[1];
         beg[1] = beg[6];
         beg[6] = temp;
         temp = beg[2];
         beg[2] = beg[5];
         beg[5] = temp;
         temp = beg[3];
         beg[3] = beg[4];
         beg[4] = temp;
     }
     break;
   case 4:
     for ( i = 0, beg = datp->data; i < tmpsize ; i++, beg += 4) {
         temp = beg[0];
         beg[0] = beg[3];
         beg[3] = temp;
         temp = beg[1];
         beg[1] = beg[2];
         beg[2] = temp;
     }
     break;
   case 2:
     for ( i = 0, beg = datp->data; i < tmpsize ; i++, beg += 2) {
         temp = beg[0];
         beg[0] = beg[1];
         beg[1] = temp;
     }
     break;
   default: break;
  }

/* <<< */

  lockflag = F_WRLCK ;
  if( ufs_lock(fd,F_SETLK,&lockflag) != 0 ) 
  { close(fd); return( EDMS_RECLOCK ); }

  if( write(fd,datp->data,datp->size) != datp->size ) 
  {  
    lockflag = F_UNLCK ;
    if( ufs_lock(fd,F_SETLK,&lockflag) != 0 ) return( EDMS_RECLOCK ); 
    close(fd);
    return( EDMS_RWRERR ); 
  }

  lockflag = F_UNLCK ;
  if( ufs_lock(fd,F_SETLK,&lockflag) != 0 ) 
  { close(fd); return( EDMS_RECLOCK ); }

  close(fd);
  return( EDMS_OK );
}

/*---------------------------------------------------------------------------*/
int ufs_rlst(DMS *dmsp,DMST *keyp,char *keybuf,int *keynum)
/*---------------------------------------------------------------------------*/
{ char   keymask[MAXKEYL],ordermask[MAXKEYL],keymatch[MAXKEYL];
  char   *phyname,*keyptr;
  char   *name,pathx[MAX_DMS_FNAME],*pathptr;
  struct dirent *entp;
  DIR    *dirp,*stkdirp[10];
  int    dirl,stkdirl[10];
  int    keylen,keyuse,keylevel,orderlevel,maxnum,outnum,dirlvl,entlvl;
  int    stat=EDMS_OK, question=0;
  UFS    *ufs;

  uppercase(keyp->data);
  keylen = strlen(keyp->data);
  maxnum  = *keynum ;
  *keynum = outnum = 0 ;
 
  ufs = (UFS *)dmsp->internal;
  key_use( strlen(keyp->data) );
  strcpy(keymatch,keyp->data);
  strcpy(keymask,ufs_key_keymask(keylen,&keylevel));
  strcpy(ordermask,ufs_key_ordermask(keylen,&orderlevel));
  if( strchr(keymatch,'?') != NULL ) question=1;

  if( question == 0 )
  {
    pathptr = ufs_path_composer(dmsp->phyname,keymatch);
    if( access(pathptr,F_OK) == 0 ) 
    {
      outnum++; 
      if( outnum > maxnum ) 
        stat = EDMS_FEWSIZE; 
      else
        strcpy(keybuf,keymatch);
    }
  }
  else
  {
    strcpy(pathx,phyname=dmsp->phyname);
    dirlvl = 2 ;
    if( (stkdirp[1]=opendir(pathx)) == NULL ) return( EDMS_NOFILE );
    stkdirl[1] = ufs_key_levelen(keymask,1);
    strcat(pathx,"/");
    do
    { dirp = stkdirp[--dirlvl] ;
      dirl = stkdirl[dirlvl] ;
      pathptr = strrchr(pathx,'/'); *pathptr = 0 ;
      while( (entp=readdir(dirp)) != NULL )
      { if( *(name=entp->d_name) == '.' ) continue ;
        if( strlen(name) != dirl ) continue;
        if(ufs_key_maskcmp(name,keymatch,keymask,ordermask,dirlvl)!=0) continue;
        keyptr = ufs_key_insert(name,keymask,ordermask,dirlvl);
        if( dirlvl < keylevel )
        { stkdirp[dirlvl++] = dirp ;
          stkdirl[dirlvl] = dirl ;
          strcat(pathx,"/"); strcat(pathx,name);
          dirp = opendir(pathx);
          dirl = ufs_key_levelen(keymask,dirlvl);
        }
        else
        { outnum++;
          if( outnum > maxnum && stat == EDMS_OK ) stat = EDMS_FEWSIZE; 
          if( stat == EDMS_OK )
          { strcpy(keybuf,keyptr); keybuf += keylen + 1 ; }
        }
      }
      closedir(dirp);
    } while( dirlvl > 1 );
  }
  *keynum = outnum ;
  return( stat );
}

/*---------------------------------------------------------------------------*/
int ufs_rdlt(DMS *dmsp,DMST *keyp)
/*---------------------------------------------------------------------------*/
{ char *path;
  UFS  *ufs;
  int  keylen;

  uppercase(keyp->data);
  keylen = strlen(keyp->data);
  if( (dmsp->access & ODMS_RDWR) == 0 ) return( EDMS_NRWPERMIT );
  /* if( dmsp->keylen != keyp->size ) return( EDMS_KEYLEN ); 
  ufs = (UFS *)dmsp->internal; */
  path = ufs_path_composer(dmsp->phyname,keyp->data);
  if( unlink(path) == -1 ) return( EDMS_RDLTERR );
  ufs_path_remover(keylen,dmsp->phyname,path);
  return(EDMS_OK);
}

/*---------------------------------------------------------------------------*/
int ufs_fcls(DMS *dmsp)
/*---------------------------------------------------------------------------*/
{
  if (dmsp == NULL) return(EDMS_OK);
  if (dmsp->lrucache != NULL) { free(dmsp->lrucache); dmsp->lrucache=NULL; } 
  if (dmsp->internal != NULL) { free(dmsp->internal); dmsp->internal=NULL; }
  return(EDMS_OK);
}

/*---------------------------------------------------------------------------*/
int ufs_conf(DMS *dmsp)
/*---------------------------------------------------------------------------*/
{
  return(EDMS_OK);
}

/*---------------------------------------------------------------------------*/
int ufs_ckcfg(char *cfgname,char *mode,char *cfgstr)
/*---------------------------------------------------------------------------*/
{ int stat=EDMS_OK,keylen=24;
  char *cfgptr,buf[128],cfgarg[64],cfgval[64];
  FILE *fp;

  if( mode[0]=='R' || mode[0]=='r' )
  { if( (fp=fopen(cfgname,"r")) == NULL ) return(EDMS_CONFIG);
    cfgstr[0]='\0';
    while( fgets(buf,90,fp) != NULL ) strcat(cfgstr,buf);
  }

  if( mode[0]=='W' || mode[0]=='w' )
  { if(cfgstr == NULL || cfgstr[0] == '\0' || cfgstr[0] == ' ') return(EDMS_OK);
    if( (fp=fopen(cfgname,"w")) == NULL ) return(EDMS_CONFIG);
    if( (cfgptr=strtok(cfgstr,"#")) != NULL )
    do
    {
      if( !strncmp(cfgptr,KEYLEN,strlen(KEYLEN)) )
      { sscanf(cfgptr,"KEYLEN=%d",&keylen);
        fprintf(fp,"%s\n",cfgptr);
      }
      else if( !strncmp(cfgptr,KEYMASK,strlen(KEYMASK)) )
      { sscanf(cfgptr,"KEYMASK=%s",cfgval);
        if( strlen(cfgval) == keylen ) fprintf(fp,"%s\n",cfgptr);
        else stat = EDMS_CONFIG;
      }
      else if( !strncmp(cfgptr,ORDERMASK,strlen(ORDERMASK)) )
      { sscanf(cfgptr,"ORDERMASK=%s",cfgval);
        if( strlen(cfgval) == keylen ) fprintf(fp,"%s\n",cfgptr);
        else stat = EDMS_CONFIG;
      }
      else stat = EDMS_CONFIG;
    } while( (cfgptr=strtok(NULL,"#")) != NULL && stat == EDMS_OK );
  }
  fclose(fp);
  return(stat);
}

/*---------------------------------------------------------------------------*/
/*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/
/*---------------------------------------------------------------------------*/


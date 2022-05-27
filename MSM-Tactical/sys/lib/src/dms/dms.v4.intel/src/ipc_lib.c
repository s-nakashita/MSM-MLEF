/*---------------------------------------------------------------------------*\
 *                                                                           *
 * FILE    : ipc_lib.c                                                       *
 * PURPOSE : To support IPC I/O through IPC links.                           *
 *                                                                           *
 * History :                                                                 *
 *  Oct-01-2000   Shelly Hsu   Original.                                     *
 *                                                                           *
\*---------------------------------------------------------------------------*/

#include <string.h>
/* #include <netdb.h> */
#include <unistd.h>
#include <pwd.h>
#include <fcntl.h>
#include <errno.h>
#include <sys/stat.h>
#include <sys/types.h>

#include "dms.h"
#include "dmserrno.h"
#include "dms_log.h"
#include "mesg.h"
#include "ipc_lib.h"

extern int pltcnt;
extern DMS_PLT pltapi[];
/*---------------------------------------------------------------------------*/
int ipc_putstr(int msgid, long msgtype, char *strbuf)
/*---------------------------------------------------------------------------*/
{
  MESG msgptr;
  int n;

  msgptr.mesg_type = msgtype;
  strcpy(msgptr.mesg_data, strbuf);
  msgptr.mesg_len = strlen(msgptr.mesg_data);
  log_write(LOG_DBG,"sndmsg %s\n",msgptr.mesg_data);
  n = msgsnd(msgid,(char *)&(msgptr.mesg_type),msgptr.mesg_len,0);
  return( n != 0 ? EDMS_FAIL : EDMS_OK );
}

/*---------------------------------------------------------------------------*/
int ipc_getstr(int msgid, long msgtype, char *strbuf)
/*---------------------------------------------------------------------------*/
{
  MESG msgptr;
  int n,stat;

  msgptr.mesg_type = msgtype;
  n=msgrcv(msgid,(char *)&(msgptr.mesg_type),MAXMESGDATA,msgptr.mesg_type,0);
  if ( (msgptr.mesg_len = n) < 0 ) return( EDMS_FAIL );

  msgptr.mesg_data[n] = '\0';
  strcpy(strbuf, msgptr.mesg_data);
  log_write(LOG_DBG,"rcvmsg %s\n",msgptr.mesg_data);
  return( EDMS_OK );
}

/*---------------------------------------------------------------------------*/
int ipc_sndmsg(int msgid, long msgtype, char *fmt, ...)
/*---------------------------------------------------------------------------*/
{
  static char strbuf[MAX_DMS_STRL];
  va_list args;
  int status;

  va_start(args, fmt);
  vsprintf(strbuf, fmt, args);
  va_end(args);

  status = ipc_putstr(msgid, msgtype, strbuf);
  return( status );
}

/*---------------------------------------------------------------------------*/
int ipc_rcvmsg(int msgid, long msgtype, char *fmt, ...)
/*---------------------------------------------------------------------------*/
{
  static char strbuf[MAX_DMS_STRL];
  va_list args;
  int status;

  status = ipc_getstr(msgid, msgtype, strbuf);
  if( status != EDMS_OK ) return( status );

  va_start(args, fmt);
  status = net_vsscanf(strbuf, fmt, args);
  va_end(args);

  return( EDMS_OK );
}

/*---------------------------------------------------------------------------*/
int ipc_sndbuf(int msgid, long msgtype, char *buf, int *nbytes)
/*---------------------------------------------------------------------------*/
{ static char strbuf[MAX_DMS_STRL];
  int fd,cc,n,size=*nbytes;
  MESG msgptr;

  sprintf(strbuf,"/tmp/%ld.%s",msgtype,log_time(2));
  if ( (fd=open(strbuf,O_WRONLY | O_CREAT | O_TRUNC,0644)) == -1)
  { log_write(LOG_ERR,"Error: E%04x Open file(%s) error.\n",errno,strbuf);
    return(EDMS_FAIL);
  }
  if( (cc=write(fd,buf,size) != size) )
  { log_write(LOG_ERR,"Error: E%04x Write file(%s) error.\n",errno,strbuf);
    close(fd);
    return(EDMS_FAIL);
  }
  close(fd);

  msgptr.mesg_type = msgtype;
  sprintf(msgptr.mesg_data, "P%s\n", strbuf);
  msgptr.mesg_len = strlen(msgptr.mesg_data);
  n = msgsnd(msgid,(char *)&(msgptr.mesg_type),msgptr.mesg_len,0);
  log_write(LOG_DBG,"sndbuf %s\n",msgptr.mesg_data);
  return( n != 0 ? EDMS_FAIL : EDMS_OK );
}

/*---------------------------------------------------------------------------*/
int ipc_rcvbuf(int msgid, long msgtype, char *buf, int *nbytes)
/*---------------------------------------------------------------------------*/
{ static char strbuf[MAX_DMS_STRL],msgname[MAX_DMS_STRL];
  int fd,cc,n,size=*nbytes;
  MESG msgptr;

  msgptr.mesg_type = msgtype;
  n=msgrcv(msgid,(char *)&(msgptr.mesg_type),MAXMESGDATA,msgptr.mesg_type,0);
  log_write(LOG_DBG,"rcvbuf %s\n",msgptr.mesg_data);
  if ( (msgptr.mesg_len = n) < 0 ) return( EDMS_FAIL );

  msgptr.mesg_data[n] = '\0';
  strcpy(strbuf, msgptr.mesg_data);
  sscanf(strbuf, "P%s\n", msgname);
  if ( (fd=open(msgname,O_RDONLY)) < 0)
  { log_write(LOG_ERR,"Error: E%04x Open file(%s) error.\n",errno,msgname);
    return(EDMS_FAIL);
  }
  if( (cc=read(fd,buf,size) != size) )
  { log_write(LOG_ERR,"Error: E%04x Read file(%s) error.\n",errno,msgname);
    close(fd);
    return(EDMS_FAIL);
  }
  unlink(msgname);
  close(fd);
  return(EDMS_OK);
}

/*---------------------------------------------------------------------------*/
int ipc_getpw(char *logdmsf,struct passwd *pwbuf) 
/*---------------------------------------------------------------------------*/
{ struct stat   statbuf;
/*  struct passwd *pw, *getpwuid(); */
  struct passwd *pw;
  uid_t uid;

  if( stat(logdmsf,&statbuf) != 0 )  
  { log_write(LOG_ERR,"Error: E%04x Get file(%s) stat.\n",errno,logdmsf);
    return(EDMS_FAIL);
  }
  if( (pw = getpwuid(statbuf.st_uid)) == NULL ) 
  { log_write(LOG_ERR,"Error: E%04x Get uid(%d) stat.\n",errno,statbuf.st_uid);
    return(EDMS_FAIL);
  }
  memcpy(pwbuf,pw,sizeof(struct passwd)); 
  return(EDMS_OK);
}

/*---------------------------------------------------------------------------*/
/*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/
/*---------------------------------------------------------------------------*/

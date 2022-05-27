/*---------------------------------------------------------------------------*\
 *                                                                           *
 * FILE    : net_lib.c                                                       *
 * PURPOSE : To support network I/O through network links.                   *
 * COMPILE : ULTRIX -> cc -DMTIO -c rfilelib.c                               *
 *           HP/UX  -> cc -DMTIO -Aa -Ae -c rfilelib.c                       *
 *           CRAY   -> cc -DMTIO -c rfilelib.c                               *
 *                                                                           *
 * History :                                                                 *
 *  May-11-1994   C.P.DZEN   Original.                                       *
 *  May-16-1994   C.P.DZEN   Seperate child handling from connection routine *
 *  Aug-23-1999   C.P.DZEN   use dms logging facility                        *
 *                                                                           *
\*---------------------------------------------------------------------------*/

#include <string.h>
#include <stdlib.h>
#include <netdb.h>
#include <pwd.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/wait.h>

#include "dms.h"
#include "dmserrno.h"
#include "dms_log.h"
#include "net_lib.h"
#include "retry_rexec.c"

#define CHN_READ(id,ptr,len)    read(id,ptr,len)
#define CHN_WRITE(id,ptr,len)   write(id,ptr,len)

static int child_cnt=0;
static int child_pid[MAX_NET_CHILD]={0};
/*----------------------------------------------------------------------------+
 | Name    : int net_query_child(int cid)                                     |
 | Purpose : Give child id return the chile process id.                       |
 | Return  : > 0 : child process id                                           |
 |           -1  : chile id error                                             |
 +----------------------------------------------------------------------------*/
int net_query_child(int cid)
/*---------------------------------------------------------------------------*/
{
  if( cid < MAX_NET_CHILD ) return( child_pid[cid] );
  return( -1 );
}

/*----------------------------------------------------------------------------+
 | Name    : int net_regist_child(int cpid)                                   |
 | Purpose : register a child process id to be managet by parent.             |
 | Return  : >=0 : child id                                                   |
 |           -1  : regist error                                               |
 +----------------------------------------------------------------------------*/
int net_regist_child(int cpid)
/*---------------------------------------------------------------------------*/
{ int i;
  if( child_cnt > MAX_NET_CHILD ) return( -1 );
  for( i=0 ; i < MAX_NET_CHILD ; i++ )
  { if( child_pid[i] != 0 ) continue;
    child_cnt++ ;
    child_pid[i] = cpid ;
    return( i );
  }
  return( -1 );
}

/*----------------------------------------------------------------------------+
 | Name    : int net_remove_child(int cid)                                    |
 | Purpose : Remove a child from managemented list.                           |
 | Return  : > 0 : child process id removed                                   |
 |           -1  : remove error                                               |
 |           -2  : child is still alive                                       |
 +----------------------------------------------------------------------------*/
int net_remove_child(int cid)
/*---------------------------------------------------------------------------*/
{ int  status,wait_pid,nid;

  if( cid >= MAX_NET_CHILD ) return( -1 );
  if( (nid=child_pid[cid]) == 0 ) return( -1 );
  wait_pid = waitpid( nid, &status, WNOHANG );
  if( wait_pid != nid )
  { log_write(LOG_WRN,"waitpid:child %d still alive",nid); return( -2 ); }
  child_pid[cid] = 0 ;
  child_cnt-- ;
  return( nid );
}

/*----------------------------------------------------------------------------+
 | Name : net_connect_obj(char *obj,char *host,char *user,char *pass,int *pid)|
 | Purpose : Connect to an exist network object and optionaly return child pid|
 | Return  : > 0 : Network channel                                            |
 |           < 0 : connection error                                           |
 +----------------------------------------------------------------------------*/
int net_connect_obj(char *obj, char *host, char *user, char *pass, int *pid)
/*---------------------------------------------------------------------------*/
{ char *net_object=obj;
  struct passwd   *pwd;
  struct servent  *sp;
  int    fd[2],sfd;

  if( host == NULL || *host == '\0' ) return(-1);
  if( user == NULL || *user == '\0' ) return(-2);
  *pid = 0 ;

  if( geteuid() == 0 )
  {
    if( (pwd=getpwuid(getuid())) == NULL )
    { log_write(LOG_ERR,"%s","Who are you ?"); return(-3); }
    if( (sp = getservbyname("shell", "tcp")) == NULL )
    { log_write(LOG_ERR,"%s","Shell/tcp unknown service."); return(-4); }
    if( (sfd=rcmd(&host,sp->s_port,pwd->pw_name,user,net_object,NULL)) < 0 )
    { log_write(LOG_ERR,"%s","Rcmd connection fail."); return(-5); }
    log_write(LOG_INF,"%s","Rcmd connection.");
  }
  else if( pass != NULL && *pass != '\0' )
  {
    if( (sp=getservbyname("exec", "tcp")) == NULL )
    { log_write(LOG_ERR,"%s","Exec/tcp unknown service"); return(-7); }
    if( (sfd=rexec(&host,sp->s_port,user,pass,net_object,NULL)) < 0 )
    { log_write(LOG_ERR,"%s","Rexec connection fail."); return(-8); }
    log_write(LOG_INF,"%s","Rexec connection.");
  }
  else
  {
    if( socketpair(AF_UNIX, SOCK_STREAM, 0, fd) < 0 )
    { log_write(LOG_ERR,"%s","Create socketpair fail."); return(-10); }
    if( (*pid=fork()) < 0 )
    { log_write(LOG_ERR,"%s","Fork child fail."); return(-12); }
    else if( *pid > 0 ) /* parent process */
    { close(fd[1]); sfd = fd[0]; }
    else /* child process */
    { close(fd[0]);
      if( dup2(fd[1],0) != 0 )
      { log_write(LOG_ERR,"%s","dup stdin error.");  _exit(1); }
      if( dup2(fd[1],1) != 1 )
      { log_write(LOG_ERR,"%s","dup stdout error."); _exit(1); }
      execl("/usr/ucb/rsh"  ,"rsh"  ,host,"-l",user,net_object,NULL);
      execl("/usr/bin/remsh","remsh",host,"-l",user,net_object,NULL);
      execl("/usr/bin/rsh"  ,"rsh"  ,host,"-l",user,net_object,NULL);
      log_write(LOG_ERR,"%s","Rsh fail.");
      _exit(1);
    }
    log_write(LOG_INF,"%s","Rsh connection.");
  }
  return(sfd);
}

/*----------------------------------------------------------------------------+
 | Name    : int net_close_obj(int fd)                                        |
 | Purpose : Close a network channel                                          |
 | Return  : = 0 : close success                                              |
 |           -1  : close error                                                |
 +----------------------------------------------------------------------------*/
int net_close_obj(int fd)
/*---------------------------------------------------------------------------*/
{
  return( close(fd) );
}

/*----------------------------------------------------------------------------+
 | Name    : int net_read(int fd,char *ptr,int nbytes)                        |
 | Purpose : Read n bytes from network channel into buffer.                   |
 | Return  : > 0 : bytes readed                                               |
 |           -1  : read error                                                 |
 +----------------------------------------------------------------------------*/
int net_read(register int fd, register char *ptr, register int nbytes)
/*---------------------------------------------------------------------------*/
{ int   nleft, nread;

  nleft = nbytes;
  while(nleft > 0)
  { nread = CHN_READ(fd, ptr, nleft);
    if(nread < 0) return(-1);
    else if(nread == 0) break;
    nleft -= nread;
    ptr += nread;
  }
  return(nbytes - nleft);
}

/*----------------------------------------------------------------------------+
 | Name    : int net_write(int fd,char *ptr,int nbytes)                       |
 | Purpose : write n bytes from buffer to network channel.                    |
 | Return  : > 0 : bytes writed                                               |
 |           -1  : write error                                                |
 +----------------------------------------------------------------------------*/
int net_write(register int fd, register char *ptr, register int nbytes)
/*---------------------------------------------------------------------------*/
{ int   nleft, nwritten;

  nleft = nbytes;
  while(nleft > 0)
  { nwritten = CHN_WRITE(fd, ptr, nleft);
    if(nwritten <= 0) return(-1);
    nleft -= nwritten;
    ptr += nwritten;
  }
  return(nbytes - nleft);
}

/*----------------------------------------------------------------------------+
 | Name    : int net_ngets(int fd,char *ptr,int maxlen)                       |
 | Purpose : Read a LF terminated string from network channel.                |
 | Return  : > 0 : bytes readed                                               |
 |           = 0 : read EOF                                                   |
 |           -1  : read error                                                 |
 +----------------------------------------------------------------------------*/
int net_ngets(register int fd, register char *ptr, register int maxlen)
/*---------------------------------------------------------------------------*/
{ register int  n,rc;
  char c;

  for( n=1 ; n < maxlen ; n++)
  { if( ( rc = CHN_READ(fd, &c, 1) ) == 1)
    { if( (*ptr++ = c) == '\n' ) break ; }
    else if ( rc == 0 )
    { if( n == 1 ) return(0); else break; }
    else
    { return( -1 ) ; }
  }
  *ptr = 0 ;
  return( n );
}

/*----------------------------------------------------------------------------+
 | Name    : int net_gets(int fd,char *ptr)                                   |
 | Purpose : Read a LF terminated string from network channel.                |
 | Return  : > 0 : bytes readed                                               |
 |           = 0 : read EOF                                                   |
 |           -1  : read error                                                 |
 +----------------------------------------------------------------------------*/
int net_gets(register int fd, register char *ptr)
/*---------------------------------------------------------------------------*/
{
  return( net_ngets(fd,ptr,256) );
}

/*----------------------------------------------------------------------------+
 | Name    : int net_puts(register int fd, register char *ptr)                |
 | Purpose : Write a LF terminated string to network channel.                 |
 | Return  : > 0 : bytes writed                                               |
 |           = 0 : write error                                                |
 +----------------------------------------------------------------------------*/
int net_puts(register int fd, register char *ptr)
/*---------------------------------------------------------------------------*/
{ int nw,nc=strlen(ptr);

  if( ptr[nc-1] != '\n' ) ptr[nc++] = '\n' ;
  if( (nw = CHN_WRITE(fd, ptr, nc) ) != nc ) return( 0 );
  return( nc );
}

static long  net_max_buf_size = -1;
/*----------------------------------------------------------------------------+
 | Name    : char *net_set_buf(int fd, char *ptr, long size, int option)      |
 | Purpose : Set socket buffer size of a network channel.                     |
 |           Reallocate the buffer with suitable size if needed.              |
 | Return  : pointer of buffer                                                |
 |           NULL : mamory allocate error                                     |
 +----------------------------------------------------------------------------*/
char *net_set_buf(int fd, char *ptr, long size, int option)
/*---------------------------------------------------------------------------*/
{
  if( option!=SO_SNDBUF || option!=SO_RCVBUF ) return( ptr );
  if( size <= net_max_buf_size ) return( ptr );
  if( ptr != NULL ) free( ptr );
  ptr = (char *)malloc( size );
  if( ptr == NULL ) return( ptr );
  net_max_buf_size = size;
  while( (size > 1024) &&
         (setsockopt(fd,SOL_SOCKET,option,(char *)&size,sizeof(size)) < 0)
  ) size -= 1024 ;
  if( size != net_max_buf_size )
  { char asclen[10];
    sprintf(asclen,"%d",size);
    log_write(LOG_WRN,"Max socket buffer size : %d",asclen);
  }
  return( ptr );
}

/*---------------------------------------------------------------------------*/
int  parse_nfname(char *nfname,
                  char *rname,char *rpath,char *ruser,char *rpswd,char *rhost)
/*---------------------------------------------------------------------------*/
{ char *ptr1, *ptr2, *ptr3, *pwd, ldms[128], localhost[64];

  strcpy(ldms,nfname);
  if((ptr1=strchr(ldms,'@'))==NULL)
  { log_write(LOG_ERR,"parse_nfname: %s - name error.\n",ldms); return(-1); }

  *ptr1++ = 0;
  if((ptr2=strchr(ptr1,'@'))==NULL)
  { log_write(LOG_ERR,"parse_nfname: %s - path error.\n",ldms); return(-2); }

  *ptr2++ = 0;
  if((pwd=strchr(ptr2,':'))!=NULL)
  { *pwd++ = 0;
    if((ptr3=strchr(pwd,'@'))!=NULL)
    { *ptr3++ = 0; strcpy(rhost,ptr3); }
    else
    { gethostname(localhost,64); strcpy(rhost,localhost); }
    strcpy(rpswd,pwd);
  }
  else
  {
    if((ptr3=strchr(ptr2,'@'))!=NULL)
    { *ptr3++ = 0; strcpy(rhost,ptr3); }
    else
    { gethostname(localhost,20); strcpy(rhost,localhost); }
    *rpswd='\0';
  }
  strcpy(rname,ldms);
  strcpy(rpath,ptr1);
  strcpy(ruser,ptr2);
  return(0);
}

/*---------------------------------------------------------------------------*/
int net_vsscanf(char *strbuf,char *fmtbuf,va_list args)
/*---------------------------------------------------------------------------*/
{ static  char tmpstr[MAX_NET_STRL],tmpfmt[MAX_NET_STRL];
  char    *strptr,*fmtptr,*bufptr;
  int     i,stat,knt;

  knt  = 0 ;
  stat = 1 ;
  strptr = strbuf ;
  fmtptr = fmtbuf ;

  while( *fmtptr != '\0' && stat == 1 )
  {
    bufptr = tmpstr ;
    for( ; *strptr != '\n' ; ) *bufptr++ = *strptr++ ;
    *bufptr++ = *strptr++ ;    *bufptr   = 0 ;

    bufptr = tmpfmt ;
    for( ; *fmtptr != '\n' ; ) *bufptr++ = *fmtptr++ ;
    *bufptr++ = *fmtptr++ ;    *bufptr   = 0 ;

    stat = sscanf(tmpstr,tmpfmt,va_arg(args,char *));
    if( stat == 1 ) knt++ ;
  }
  return( (stat <= 0 ) ? stat : knt );
}

/*---------------------------------------------------------------------------*/
int net_sndmsg(int fd,char *fmt, ...)
/*---------------------------------------------------------------------------*/
{ static  char lenbuf[8],strbuf[MAX_NET_STRL];
  va_list args;
  int     nb,sl,stat;

  va_start(args, fmt);
  vsprintf(strbuf, fmt, args);
  va_end(args);
#ifdef DMS_VLEN
  sprintf(lenbuf,"L%04x\n",sl=strlen(strbuf));
  if( stat=net_puts(fd,lenbuf) <= 0 ) return(SND_ERR);
#else
  sl = strlen(strbuf);
  memset(&strbuf[sl],0x20,MAX_NET_STRL-sl);
  strbuf[MAX_NET_STRL-1]='\0';
  sl = MAX_NET_STRL;
#endif
  if( stat=net_write(fd,strbuf,sl) <= 0 ) return(SND_ERR);
#ifdef DMS_ACK
  if( stat=net_read(fd,lenbuf,6) <= 0 ) return(SND_ERR);
  lenbuf[6]='\0';
  sscanf(lenbuf,"L%04x\n",&nb);
  if( nb != sl ) return(SND_ERR);
#endif

  return(SND_OK);
}
/*---------------------------------------------------------------------------*/
int net_rcvmsg(int fd,char *fmt, ...)
/*---------------------------------------------------------------------------*/
{ static  char lenbuf[8],strbuf[MAX_NET_STRL];
  char    *cptr;
  va_list args;
  int     nb,sl,stat;

#ifdef DMS_VLEN 
  if( stat=net_read(fd,lenbuf,6) <= 0 ) return(RCV_ERR);
  lenbuf[6]='\0';
  sscanf(lenbuf,"L%04x\n",&nb);
#else
  nb = MAX_NET_STRL;
#endif
  if( stat=net_read(fd,strbuf,nb) <= 0 ) return(RCV_ERR);
  strbuf[nb]='\0';
#ifndef DMS_VLEN
  if ( (cptr=strrchr(strbuf,'\n')) != NULL ) *(cptr+1)='\0';
#endif

  va_start(args, fmt);
  stat = net_vsscanf(strbuf, fmt, args);  
  va_end(args);
#ifdef DMS_ACK
  sprintf(lenbuf,"L%04x\n",nb);
  if( stat=net_puts(fd,lenbuf) <= 0 ) return(RCV_ERR);
#endif 

  return(RCV_OK);
}

/*---------------------------------------------------------------------------*/
int net_sndbuf(int fd, char *buf,int *nbytes)
/*---------------------------------------------------------------------------*/
{ int i, cc, size=*nbytes;

  for( i=0 ; i < size ; i+=cc )
  { if( (size-i) < MAX_SOCKET_SIZE )
    { if( (cc=write(fd, &buf[i], size-i)) <= 0) return(SND_ERR); }
    else
    { if( (cc=write(fd, &buf[i], MAX_SOCKET_SIZE)) <= 0) return(SND_ERR); }
  }
  return(SND_OK);
}

/*---------------------------------------------------------------------------*/
int net_rcvbuf(int fd, char *buf,int *nbytes)
/*---------------------------------------------------------------------------*/
{ int i, cc, size=*nbytes;

  for( i=0 ; i < size ; i+=cc )
  { if( (size-i) < MAX_SOCKET_SIZE )
    { if( (cc=read(fd, &buf[i], size-i)) <= 0 ) return(RCV_ERR); }
    else
    { if( (cc=read(fd, &buf[i], MAX_SOCKET_SIZE)) <= 0 ) return(RCV_ERR); } 
  }
  return(RCV_OK);
}

/*---------------------------------------------------------------------------*/
/* Get a string from the command line (the socket). */
int net_rcvstr(int fd,char *strbuf)
/*---------------------------------------------------------------------------*/
{
  register int i;
  register char *cp;

  cp = strbuf;
  for( i=0; i<(MAX_DMS_STRL-1); i++ )
  { 
    if( net_read(fd,cp+i,1) != 1)
    { log_write(LOG_ERR,"Read error %04x !\n",errno); cp[0] = '\0'; break; }
    if( cp[i] == '\n' ) break; 
  }
  cp[i] = '\0';
  return( cp[0] == '\0' ? RCV_ERR : RCV_OK );
}

/* -------------------------------------------------------------------- */
/* Send a normal response to the client */
int net_sndstr(int fd,char *strbuf)
/* -------------------------------------------------------------------- */
{
     register int n;

     n=strlen(strbuf);
     return( net_write(fd,strbuf,n) != n ? SND_ERR : SND_OK );
}

/*---------------------------------------------------------------------------*/
/*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/
/*---------------------------------------------------------------------------*/

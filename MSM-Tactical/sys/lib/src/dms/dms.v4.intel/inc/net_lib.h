#ifndef _NET_LIB_H_
#define _NET_LIB_H_
/*---------------------------------------------------------------------------*/
/*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/
/*---------------------------------------------------------------------------*/

#include <stdarg.h>

#define  MAX_NET_STRL       256
#define  MAX_SOCKET_SIZE    16384
#define  MAX_NET_CHILD      32
/*---------------------------------------------------------------------------*/
enum NET_STATUS
{ NET_STATUS_MIN=0x0,
  SND_OK,  SND_ERR,
  RCV_OK,  RCV_ERR,
  ACK_OK,  ACK_ERR,
  NET_STATUS_MAX
};
/* ------------------------------------------------------------------------- */
/*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/
/* ------------------------------------------------------------------------- */
int net_query_child(int cid);
int net_regist_child(int cpid);
int net_remove_child(int cid);
/* ------------------------------------------------------------------------- */
int net_connect_obj(char *obj,char *host,char *user,char *pass,int *pid);
int net_close_obj(int fd);
/* ------------------------------------------------------------------------- */
int net_read(int fd,char *ptr,int nbytes);
int net_write(int fd,char *ptr,int nbytes);
int net_ngets(int fd,char *ptr,int maxlen);
int net_gets(int fd,char *ptr);
int net_puts(int fd,char *ptr);
/* ------------------------------------------------------------------------- */
int net_vsscanf(char *strbuf,char *fmtbuf,va_list args);
int net_sndmsg(int fd,char *fmt, ...);
int net_rcvmsg(int fd,char *fmt, ...);
int net_sndbuf(int fd,char *buf,int *size);
int net_rcvbuf(int fd,char *buf,int *size);
int net_getstr(int fd,char *strbuf);
int net_putstr(int fd,char *strbuf);
/* ------------------------------------------------------------------------- */
char *net_set_buf(int fd,char *ptr,long size,int option);
/* ------------------------------------------------------------------------- */
int  parse_nfname(char *nfname,char *rn,char *rp,char *ru,char *rw,char *rh);
/* ------------------------------------------------------------------------- */
/*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/
/*---------------------------------------------------------------------------*/
#endif

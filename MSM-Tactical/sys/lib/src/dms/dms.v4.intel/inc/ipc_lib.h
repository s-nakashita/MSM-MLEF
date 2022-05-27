#ifndef _IPC_LIB_H_

#define _IPC_LIB_H_
/*---------------------------------------------------------------------------*/
/*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/
/*---------------------------------------------------------------------------*/

#include <stdarg.h>

/* ------------------------------------------------------------------------- */
int ipc_putstr(int msgid, long msgtype, char *strbuf);
int ipc_getstr(int msgid, long msgtype, char *strbuf);
int ipc_sndmsg(int msgid, long msgtype, char *fmt, ...);
int ipc_rcvmsg(int msgid, long msgtype, char *fmt, ...);
int ipc_sndbuf(int msgid, long msgtype, char *buf, int *nbytes);
int ipc_rcvbuf(int msgid, long msgtype, char *buf, int *nbytes);
int ipc_getpw(char *logdmsf,struct passwd *pwbuf);
/* ------------------------------------------------------------------------- */
/*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/
/*---------------------------------------------------------------------------*/
#endif

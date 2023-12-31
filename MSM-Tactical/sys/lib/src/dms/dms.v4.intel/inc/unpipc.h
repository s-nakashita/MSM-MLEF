/* Our own header. Tabs are set for 4 spaces, not 8 */

#ifndef __unpipc_h
#define __unpipc_h

#include "config.h" 	/* configuration options for current OS */
			/* "config.h" is generated by configure */

/* If anything changes in the following list of #include, must change
   ../aclocal.m4 and ../confige.in also, for configure's tests. */

#include <sys/types.h>	/* basic system data types */
#include <sys/time.h>	/* timeval{} for select() */
#include <time.h>	/* timespec{} for pselect() */
#include <errno.h>
#include <fcntl.h>	/* for nonblocking */
#include <limits.h>	/* PIPE_BUF */
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>	/* for S_xxx file mode constants */
#include <unistd.h>
#include <sys/wait.h>	

#ifdef HAVE_MQUEUE_H
#include <mqueue.h>	/* Posix message queues */
#endif

#ifdef SEM_FAILED
#define SEM_FAILED ((sem_t *)(-1))
#endif

#endif

#ifdef HAVE_SYS_MMAN_H
#include <sys/mman.h>	/* Posix shared memory */
#endif

#ifndef MAP_FAILED
#define MAP_FAILED ((void *)(-1))
#endif

#ifdef HAVE_SYS_MSG_H
#include <sys/msg.h>	/* System V IPC */
#endif

#ifdef HAVE_SYS_MSG_H
#ifdef __bsdi__
#undef HAVE_SYS_SEM_H	/* hack: BSDI's semctl() prototype is wrong */
#else
#include <sys/sem.h>	/* System V semaphores */
#endif

#ifndef HAVE_SEMUN_UNION
union semun		/* define union for semctl() */
{
  int val;
  struct semid_ds *buf;
  unsigned short *array;
};
#endif
#endif /* HAVE_SYS_SEM_H */

#ifdef HAVE_SYS_SHM_H
#include <sys/shm.h>	/* System V shared memory */
#endif

#ifdef HAVE_SYS_SELECT_H
#include <sys/select.h>	/* for convenience */
#endif

#ifdef HAVE_POLL_H
#include <poll.h>	/* for convenience */
#endif

#ifdef HAVE_STROPTS_H
#include <stropts.h>	/* for convenience */
#endif

#ifdef HAVE_STRINGS_H
#include <strings.h>	/* for convenience */
#endif

/* Next three headers are normally needed for socket/file ioctl's:
 * <sys/ioctl.h>, <sys/filio.h>, and <sys/sockio.h>.
 */
#ifdef HAVE_SYS_IOCTL_H
#include <sys/ioctl.h>
#endif
#ifdef HAVE_SYS_FILIO_H
#include <sys/filio.h>
#endif

#ifdef HAVE_PTHREAD_H
#include <pthread.h>
#endif

#ifdef HAVE_DOOR_H
#include <door.h>	/* Solaris doors API */
#endif

#ifdef HAVE_RPC_RPC_H
#ifdef _PSX4_NSPACE_H_TS	/* Digital Unix 4.0b hack, hack, hack */
#undef SUCCESS
#endif
#include <rpc/rpc.h>	/* Sun RPC */
#endif

/* Define bzero() as macro if it's not in standard C library. */
#ifndef HAVE_BZERO
#define bzero(ptr,n) memset(ptr,0,n)
#endif

/* Posix.1g requires that an #include of <poll.h> defome INFTIM, but many
   system still define it in <sys/stropts.h>. We don't want to include 
   all the streams stuff if it's not needed, so we just define INFTIM here.
   This is the standard value, but there's no guarantee it is -1. */
#ifndef INFTIM
#define INFTIM (-1)	/* infinite poll timeout */
#ifdef HAVE_POLL_H
#define INFTIM_UNPH	/* tell unpxti.h we defined it */
#endif
#endif

/* Miscellaneous constants */
#ifndef PATH_MAX	/* should be in <limits.h> */
#define PATH_MAX 1024	/* max # of characters in a pathname */
#endif

#define MAX_PATH 1024
#define MAXLINE 4096	/* max text line length */
#define BUFFSIZE 8192	/* buffer size for reads and writes */

#define FILE_MODE  (S_IRUSE | S_IWUSR | S_IRGRP | S_IROTH)
		   /* default permission for new files */
#define DIR_MODE   (FILE_MODE | S_IXUSR | S_IXGRP | S_IXOTH)
		   /* default permission for new directories */

#define SVMSG_MODE (MSG_R | MSG_W | MSG_R>>3 | MSG_R>>6)
		   /* default permissions for new SV message queues */
#define SVSEM_MODE (SEM_R | SEM_A | SEM_R>>3 | SEM_R>>6)
		   /* default permissions for new SV semaphores */
#define SVSHM_MODE (SHM_R | SHM_W | SHM_R>>3 | SHM_R>>6)
		   /* default permissions for new SV shared memory */

typedef void Sigfunc(int);	/* for signal handlers */

#ifdef HAVE_SIGINFO_T_STRUCT
typedef void Sigfunc_rt(int,siginfo_t *,void *);
#endif

#define min(a,b) ((a) < (b) ? (a) : (b))
#define max(a,b) ((a) > (b) ? (a) : (b))

#ifndef HAVE_TIMESPEC_STRUCT
struct timespec 
{
  time_t tv_sec;	/* seconds */
  long   tv_nsec;	/* and nanoseconds */
};
#endif

/*
 * In our wrappers for open(), mq_open(), and sem_open() we handle the
 * optional arguments using the va_XXX() macros, But one of the optional
 * arguments is of type "mode_t" and this breaks under BSD/OS because it
 * uses a 16-bit integer for this datatype. But when our wrapper function
 * is called, the compiler expands the 16-bit short integer to a 32-bit
 * integer. This breaks our call to va_arg(). All we can do is the
 * following hack. Other systems in addition to BSD/OS might have this
 * problem too ...
 */

#ifdef __bsdi__
#define va_mode_t int
#else
#define va_mode_t mode_t
#endif

	/* our record locking macros */
#define read_lock(fd, offset, whence, len) \
		lock_reg(fd, F_SETLK, F_RDLCK, offset, whence, len)
#define readw_lock(fd, offset, whence, len) \
		lock_reg(fd, F_SETLKW, F_RDLCK, offset, whence, len)
#define write_lock(fd, offset, whence, len) \
		lock_reg(fd, F_SETLK, F_WRLCK, offset, whence, len)
#define writew_lock(fd, offset, whence, len) \
		lock_reg(fd, F_SETLKW, F_WRLCK, offset, whence, len)
#define un_lock(fd, offset, whence, len) \
		lock_reg(fd, F_SETLK, F_UNLCK, offset, whence, len)
#define is_read_lockable(fd, offset, whence, len) \
		lock_test(fd, F_RDLCK, offset, whence, len)
#define is_write_lockable(fd, offset, whence, len) \
		lock_test(fd, F_WRLCK, offset, whence, len)


/*---------------------------------------------------------------------------*\
 *                                                                           *
 * mesg.h -  The include file for the message queue of DMS system.           *
 *                                                                           *
\*---------------------------------------------------------------------------*/

/* Protection for multiple includes. */
#ifndef _MESG_H_

#define _MESG_H_

#include <limits.h>
#include <sys/types.h>
#include <sys/ipc.h>
#include <sys/msg.h>

#define SVRTYPE 1L
#define MESG_KEY 0x4c6c0405
#define MESG_LIMITS 8192

#ifndef MSG_R
#define       MSG_R   0400    /* read permission */
#endif

#ifndef MSG_W
#define       MSG_W   0200    /* write permission */
#endif

#define MESG_MODE (MSG_R | MSG_W | MSG_R>>3 | MSG_W>>3 | MSG_R>>6 | MSG_W>>6)

#ifndef PIPE_BUF
#define PIPE_BUF MESG_LIMITS
#endif

	/* want sizeof(MESG) <= PIPE_BUF or MESG_LIMITS */
#if PIPE_BUF > MESG_LIMITS
#define	MAXMESGDATA	(MESG_LIMITS - 2*sizeof(long))
#else
#define	MAXMESGDATA	(PIPE_BUF - 2*sizeof(long))
#endif

	/* length of mesg_len and mesg_type */
#define	MESGHDRSIZE	(sizeof(MESG) - MAXMESGDATA)

typedef struct {
  long mesg_len;  /* #bytes in mesg_data, can be 0 or > 0 */
  long mesg_type; /* message type, must be > 0 */
  char mesg_data[MAXMESGDATA];
} MESG;

#endif /* _MESG_H_ */


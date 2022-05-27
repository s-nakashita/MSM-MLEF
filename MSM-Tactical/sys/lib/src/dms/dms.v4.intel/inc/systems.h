/* systems.h - Most of the system dependant code and defines are here. */
/* $Id: systems.h,v 1.8 1995/01/25 19:43:18 steve Exp $ */

/*  This file is part of GDBM, the GNU data base manager, by Philip A. Nelson.
    Copyright (C) 1990, 1991, 1993  Free Software Foundation, Inc.

    GDBM is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2, or (at your option)
    any later version.

    GDBM is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with GDBM; see the file COPYING.  If not, write to
    the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

    You may contact the author by:
       e-mail:  phil@wwu.edu
      us-mail:  Philip A. Nelson
                Computer Science Department
                Western Washington University
                Bellingham, WA 98226
       
*************************************************************************/


/* Include all system headers first. */
#include <stdio.h>	
#include <string.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/types.h>
#include <fcntl.h>
#include <sys/stat.h>


#ifndef USE_FLOCK

/* default is to use POSIX fcntl file locking */

#define UNLOCK_FILE(dbf) \
	{					\
	  struct flock flock;			\
	  flock.l_type = F_UNLCK;		\
	  flock.l_whence = 0;			\
	  flock.l_start = flock.l_len = 0L;	\
	  fcntl (dbf->desc, F_SETLK, &flock);	\
	}
#define READLOCK_FILE(dbf) \
	{					\
	  struct flock flock;			\
	  flock.l_type = F_RDLCK;		\
	  flock.l_whence = 0;			\
	  flock.l_start = flock.l_len = 0L;	\
	  lock_val = fcntl (dbf->desc, F_SETLK, &flock);	\
	}
#define WRITELOCK_FILE(dbf) \
	{					\
	  struct flock flock;			\
	  flock.l_type = F_WRLCK;		\
	  flock.l_whence = 0;			\
	  flock.l_start = flock.l_len = 0L;	\
	  lock_val = fcntl (dbf->desc, F_SETLK, &flock);	\
	}

#else

/*  Use BSD flock() advisory locking */

#include <sys/file.h>

#ifndef LOCK_SH
#define LOCK_SH	1
#endif

#ifndef LOCK_EX
#define LOCK_EX	2
#endif

#ifndef LOCK_NB
#define LOCK_NB 4
#endif

#ifndef LOCK_UN
#define LOCK_UN 8
#endif

#define UNLOCK_FILE(dbf) flock (dbf->desc, LOCK_UN)
#define READLOCK_FILE(dbf) lock_val = flock (dbf->desc, LOCK_SH + LOCK_NB)
#define WRITELOCK_FILE(dbf) lock_val = flock (dbf->desc, LOCK_EX + LOCK_NB)

#endif

/* Do we have ansi memxxx() functions? */
#if NO_MEMMOVE || NO_MEMCPY || NO_MEMCMP
/* define memmove in terms of bcopy */
#define memmove(d1, d2, n) bcopy((d2), (d1), (n))
/* define memcpy in terms of bcopy */
#define memcpy(d1, d2, n) bcopy((d2), (d1), (n))
/* define memcpy in terms of bcmp */
#define memcmp(d1, d2, n)	bcmp((d1), (d2), (n))
#endif /* NO_MEMMOVE */

/* Do we have fsync? */
#ifdef NO_FSYNC
/* why are we concerned about synchro at this level? Non Unix systems? -gd */
#define fsync(f) {sync(); sync();}
#endif

/* Default block size.  Some systems do not have blocksize in their
   stat record. This code uses the BSD blocksize from stat. */

#if HAVE_ST_BLKSIZE
#define STATBLKSIZE file_stat.st_blksize
#else
#ifndef STATBLKSIZE
#define STATBLKSIZE 8192
#endif
#endif

/* Do we have ftruncate? */
#ifndef NO_FTRUNCATE
#define TRUNCATE(dbf) ftruncate (dbf->desc, 0)
#else
/* I'm not convinced that this will do what is desired -gd */
#define TRUNCATE(dbf) close( open (dbf->name, O_RDWR|O_TRUNC, mode));
#endif

/* Do we have 32bit or 64bit longs? */
#if SIZEOF_LONG == 8 || SIZEOF_INT != 2
typedef int word_t;
#else
typedef long word_t;
#endif

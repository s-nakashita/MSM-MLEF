/*----------------------------------------------------------------------*/
/*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/
/*----------------------------------------------------------------------*/
/*
   FILE    : dms_log.c
   PURPOSE : Package for parallel file system logging.
   MACHINE : Fujitsu/Vpp300E
   SYSTEM  : PIOFS/LOG
   HISTORY :
     Jun-20-1999   ftl   Origional
*/
/*----------------------------------------------------------------------*/
/*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/
/*----------------------------------------------------------------------*/
#include <stdio.h>
#include <stdarg.h>
#include <time.h>
#ifdef MPI
#include "mpi.h"
#endif
#include "dms_log.h"
#include "dmserrno.h"

#define MAX_DMS_STRL 256
/*---------------------------------------------------------------------------*/
/*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/
/*---------------------------------------------------------------------------*/
static int  log_lvl     = LOG_MAX - 1 ;
static FILE *log_filptr = NULL ;

/*---------------------------------------------------------------------------*/
/*++++++++++++++++++< Routines for internal reference >++++++++++++++++++++++*/
/*---------------------------------------------------------------------------*/
char *log_time(int fmt_flag)
/*---------------------------------------------------------------------------*/
{ struct tm *newtime;
  time_t long_time;
  static char temp[32];

  time(&long_time);
  newtime=localtime(&long_time);
  switch (fmt_flag)
  { case 1 :
      sprintf(temp,"%02d/%02d-%02d:%02d:%02d", newtime->tm_mon+1,
          newtime->tm_mday,newtime->tm_hour, newtime->tm_min, newtime->tm_sec);
      break ;
    case 2 :
      sprintf(temp,"%02d%02d%02d%02d%02d", newtime->tm_mon+1,
          newtime->tm_mday,newtime->tm_hour, newtime->tm_min, newtime->tm_sec);
      break ;
    default :
      sprintf(temp,"%02d%02d%02d",
          newtime->tm_hour, newtime->tm_min, newtime->tm_sec);
  }
  return(temp);
}

/*----------------------------------------------------------------------*/
int log_level(int level)
/*----------------------------------------------------------------------*/
{ if( level < LOG_MIN  ||  level > LOG_MAX ) return( EDMS_FAIL );
  log_lvl = level;
  return( EDMS_OK );
}

/*----------------------------------------------------------------------*/
FILE *log_fptr(FILE *filptr)
/*----------------------------------------------------------------------*/
{ return( log_filptr=filptr ); }

/*----------------------------------------------------------------------*/
FILE *log_open(char *log_name)
/*----------------------------------------------------------------------*/
{ FILE *filptr;
  if( log_name == NULL ) return( log_filptr=NULL );
  if( (filptr=fopen(log_name,"a")) != NULL ) return( log_filptr=filptr );
  return( NULL );
}

/*----------------------------------------------------------------------*/
FILE *log_close(FILE *filptr)
{
  if( filptr != NULL ) fclose(filptr);
  return( log_filptr = NULL );
}

/*----------------------------------------------------------------------*/
char *log_str(int level, char *fmt, ...)
/*----------------------------------------------------------------------*/
{ static  char strbuf[MAX_DMS_STRL];
  va_list args;

  if( level > log_lvl ) { strbuf[0] = 0 ; return(strbuf); }
  va_start(args, fmt);
  vsprintf(strbuf, fmt, args);
  va_end(args);
  return(strbuf);
}

/*----------------------------------------------------------------------*/
char *log_write(int level, char *fmt, ...)
/*----------------------------------------------------------------------*/
{ static  char strbuf[MAX_DMS_STRL];
  va_list args;
  int rank ;

  if( level > log_lvl ) { strbuf[0] = 0 ; return(strbuf); }
  va_start(args, fmt);
  vsprintf(strbuf, fmt, args);
  va_end(args);

  if( *strbuf != 0  &&  log_filptr != NULL )
  {
#ifdef MPI
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    fprintf(log_filptr,"P%0.2d>%s %s",rank,log_time(2),strbuf);
#else
    fprintf(log_filptr,"%s %s",log_time(2),strbuf);
#endif
    fflush(log_filptr);
  }
  return(strbuf);
}

/*----------------------------------------------------------------------*/
/*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/
/*----------------------------------------------------------------------*/
#ifdef  TEST_MAIN

main(int argc,char *argv[])
{ int  level;

#ifdef MPI
  MPI_Init(&argc,&argv);
#endif

  level = atoi(argv[1]);

  log_fptr(stderr);
  printf("\n\nT1 : Error level is %d\n",log_level(level));
  log_write(LOG_DBG,"This %s level %d %s\n","is",LOG_DBG,"=");
  log_write(LOG_INF,"This %s level %d %s\n","is",LOG_INF,"=");
  log_write(LOG_WRN,"This %s level %d %s\n","is",LOG_WRN,"=");
  log_write(LOG_ERR,"This %s level %d %s\n","is",LOG_ERR,"=");
/*
  log_fptr(NULL);
  printf("\n\nT2 : Error level is %d\n",log_level(level));
  printf("= %s\n",log_write(LOG_DBG,"This %s level %d %s\n","is",LOG_DBG,"="));
  printf("= %s\n",log_write(LOG_INF,"This %s level %d %s\n","is",LOG_INF,"="));
  printf("= %s\n",log_write(LOG_WRN,"This %s level %d %s\n","is",LOG_WRN,"="));
  printf("= %s\n",log_write(LOG_ERR,"This %s level %d %s\n","is",LOG_ERR,"="));
*/
  log_fptr(stdout);
  printf("\n\nT3 : Error level is %d\n",log_level(level));
  log_write(LOG_ERR,"Test %s SHOW_LOG %d %s\n","for",LOG_ERR,"...");

#ifdef MPI
  MPI_Finalize();
#endif
}

#endif
/*----------------------------------------------------------------------*/
/*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/
/*----------------------------------------------------------------------*/

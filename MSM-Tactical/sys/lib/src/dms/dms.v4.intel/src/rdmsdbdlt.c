/*---------------------------------------------------------------------------*\
 *                                                                           *
 *  Program file: rdmsdbdlt.c                                                *
 *  Purpose: This program is a main program for rdmsdbdlt utility.           *
 *                                                                           *
\*---------------------------------------------------------------------------*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <dirent.h>
#include <unistd.h>
#include "dms.h"
#include "dmserrno.h"
#include "dms_log.h"
#include "dms_lib.h"

extern int pltcnt;
extern DMS_PLT pltapi[];
/*
****************************************************************************
*
* Function Name : main()
*
* Man page : Yes
*
* Usage :
*   rdmsdbdlt -f [-v] dmsdb
*
* Description :
*   To delete the appointed database. If exist any dms file in that database,
*   it can not be deleted.
*
* Arguments :
*   -f
*      Force the prompt off for the confirm to delete database in process.
*   -v
*      Logging when call the DMS API.
*
* Data Files : None
*
* Sub./Fun. Called :
*      dms_dbdlt()
*
* Return Value :
*   0  : execution successfully.
*   -1 : Process was aborted because errors occurred.
*        Please check standard error or log file for reasons cause the error.
*
* Example :
*
*   rdmsdbdlt UFSDB
*
*   rdmsdbcrt -f /path/GDBMDB
*
* Restriction :
*   Only the local database can be deleted.
*
* See Also :
*   rdmsdbcrt(1)
*
* Date :
*    Jun. 11, 2004
*
* Side Effect : None
****************************************************************************
*/
/*---------------------------------------------------------------------------*/
main(int argc,char *argv[])
/*---------------------------------------------------------------------------*/
{ char *dbname,*eptr,ans,argument[8];
  char phyname[MAX_DMS_FNAME],tmpname[MAX_DMS_FNAME],dmspath[MAX_DMS_STRL*10];
  int  i,c,stat=0,errflg=0,j=0,rm_f=1,pltype;
  struct dirent  *dp;
  DIR    *dirp;

/**  log_fptr(stderr);  **/
  stat = dms_init(NULL);

  if(argc<2)
  {  printf("Usage: %s [-f][-v] DataBase\n",argv[0]);
     exit(1);
  }
  while ((c=getopt(argc, argv,"fv")) != EOF)
  { switch(c)
    { case 'f' : rm_f=0;
                 break;
      case 'v' : sprintf( argument, "%d", LOG_DBG );
                 dms_conf(S_LOG,argument);
                 break;
      case '?' : errflg++;
                 break;
     }
  }
  if(errflg)
  { printf("Usage: %s -pPlatform [-v] DataBase\n",argv[0]);
    dms_exit(EDMS_FAIL);
  }
  dbname=argv[argc-1];
  if( (eptr=getenv(dbname)) == NULL ) eptr = dbname;
  if( strchr(eptr,'@') != NULL )
  { printf("Error: Cannot delete remote DB(%s).\n",eptr);
    dms_exit(EDMS_FAIL);
  }

  if( rm_f )
  { printf("Delete Database %s? (y/n)",dbname);
    ans = getchar();
    if(tolower(ans) != 'y')
    { printf("Warn: DB(%s) not delete.\n",dbname);
      dms_exit(EDMS_OK);
    }
  }
  stat = dms_dbdlt(dbname,phyname);
  if(stat != EDMS_OK)
    printf("Error: E%04x delete DB(%s) error.\n",stat,dbname);
  else
    printf("DB(%s) has been deleted.\n",phyname);
  dms_exit(stat);
}


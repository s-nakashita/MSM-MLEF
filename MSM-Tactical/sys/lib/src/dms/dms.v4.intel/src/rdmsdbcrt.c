/*---------------------------------------------------------------------------*\
 *                                                                           *
 *  Program file: rdmsdbcrt.c                                                *
 *  Purpose: This program is a main program for rdmsdbcrt utility.           *
 *                                                                           *
\*---------------------------------------------------------------------------*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "dms.h"
#include "dmserrno.h"
#include "dms_log.h"
#include "dms_api.h"

/*---------------------------------------------------------------------------*/
/*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/
/*
****************************************************************************
*
* Function Name : main()
*
* Man page : Yes
*
* Usage :
*   rdmsdbcrt -p {ufs|gdb} [-c CFGSTR=CFGVAL] [-v] dmsdb
*
* Description :
*   According to the appointed type to create database.
*
* Arguments :
*   -p {ufs|gdb} 
*      Appoint the type of database.  The ufs means the unix file system 
*      database, it saves one key in a file.  The gdb means the GNU dbm 
*      database, it saves the all key in the same file. 
*   -c CFGSTR=CFGVAL
*      Set the configuration for the DMS system.  The CFGSTR may be KEYLEN,
*      KEYMASK, ORDERMASK.  The KEYLEN set the key length, it must be 24,
*      32 or 34.  The KEYMASK set the save class.  The ORDERMASK set the save
*      order. By default, KEYLEN=34, KEYMASK=2222221111222211111111111122222222
*      and ORDERMASK=3333332222333311111111111133333333.
*   -v 
*      Logging when call the DMS API.
*
* Data Files : None
*
* Sub./Fun. Called :
*      dms_dbcrt()
*
* Return Value :
*   0  : execution successfully.
*   -1 : Process was aborted because errors occurred.
*        Please check standard error or log file for reasons cause the error.
*
* Example :
*
*   rdmsdbcrt -p ufs UFSDB
*
*   rdmsdbcrt -p gdb /path/GDBMDB
*
* Restriction : 
*   Only the local database can be created.
*
* See Also :
*   rdmsdbdlt(1)
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
{ extern char *optarg;
  char *dbname,*platform,*eptr,argument[8];
  char phyname[MAX_DMS_FNAME],cfgname[MAX_DMS_FNAME],cfgstr[MAX_DMS_STRL];
  int c,errflg=0,stat=0;

  dms_init(NULL);
  if(argc < 3)
  { printf("Usage: %s -p {ufs|gdb} [-c CFGSTR=CFGVAL] [-v] db_name\n",argv[0]);
    dms_exit(EDMS_FAIL);
  }
  cfgstr[0] = '\0';
  while ((c=getopt(argc, argv,"p:c:v")) != EOF)
  { switch(c)
    { case 'p' : platform=optarg;
                 break;
      case 'c' : strcat(cfgstr, optarg);
                 strcat(cfgstr,"#");
                 break;
      case 'v' : sprintf( argument, "%d", LOG_DBG );
                 dms_conf(S_LOG,argument);
                 break;
      case '?' : errflg++;
                 break;
     }
  }
  if(errflg)
  { printf("Usage: %s -p {ufs|gdb} [-c CFGSTR=CFGVAL] [-v] db_name\n",argv[0]);
    dms_exit(EDMS_FAIL);
  }
  dbname=argv[argc-1];
  if( (eptr=getenv(dbname)) == NULL ) eptr = dbname;
  if( strchr(eptr,'@') != NULL )
  { printf("Error: Cannot create remote DB(%s).\n",eptr);
    dms_exit(EDMS_FAIL);
  }
  stat = dms_dbcrt(dbname,platform,phyname,cfgstr);
  if(stat != EDMS_OK)
    printf("Error: E%04x create DB(%s) error.\n",stat,phyname);
  else
    printf("DB(%s) is created.\n",phyname);
  dms_exit(stat);
}


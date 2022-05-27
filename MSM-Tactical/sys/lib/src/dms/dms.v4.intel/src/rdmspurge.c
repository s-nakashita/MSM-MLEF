/*---------------------------------------------------------------------------*\
 *                                                                           *
 *  Program file: rdmspurge.c                                                *
 *  Purpose: This program is the main program of rdmspurge utility.          *
 *                                                                           *
\*---------------------------------------------------------------------------*/

#include <stdio.h>
#include "dms.h"
#include "dmserrno.h"
#include "dms_log.h"
#include "dms_api.h"

/*
****************************************************************************
*
* Function Name : main()
*
* Man page : Yes
*
* Usage :
*   rdmspurge [-a dmsdb] [-f] [-v] dmsfile
*
* Description :
*   To delete the appointed dmsfile. When the dmsdb doesn't appoint, the
*   first dmsfile that match specific name will be purged. 
*   To use dmsfile@dmsdb@use@host can deleted remote dmsfile, but must
*   have W(Write) or C(Create) privilege.
*
* Arguments :
*   -a dmsdb
*      Specific database include that appointed dmsfile. 
*   -f
*      Force the prompt off for the confirm to delete database in process.
*   -v
*      Logging when call the DMS API.
*
* Data Files : None
*
* Sub./Fun. Called :
*      dms_fdlt()
*
* Return Value :
*   0  : execution successfully.
*   -1 : Process was aborted because errors occurred.
*        Please check standard error or log file for reasons cause the error.
*
* Example :
*
*   rdmspurge -a UFSDB ufsfile
*
*   rdmspurge -a GDBMDB -f gdbfile
*
*   rdmspurge ufsfile@UFSDB@dbadm@dcs42.mic.cwb
*
* Restriction :
*   Only the C(Create) or W(Write) privilege can purge remote dmsfile.
*
* See Also :
*   rdmscrt(1)
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
  char dmsfile[MAX_DMS_FNAME],phyname[MAX_DMS_FNAME],argument[8];
  char *dmsdb=NULL,ans;
  int stat,errflg=0,c,rm_f=1;

  stat = dms_init(NULL);
  if(argc < 2)
  {  printf("Usage: %s [-aDataBase][-f][-v] dmsfile\n",argv[0]);
     dms_exit(EDMS_FAIL);
  }
  while ((c=getopt(argc, argv,"a:fv")) != EOF)
  { switch(c)
    { case 'a' : dmsdb=optarg;
                 break;
      case 'f' : rm_f=0;
                 break;
      case 'v' : sprintf( argument, "%d", LOG_DBG );
                 dms_conf(S_LOG,argument);
                 break;
      case '?' : errflg++;
                 break;
    }
  }
  if(errflg)
  { printf("Usage: %s [-aDataBase][-f][-v] dmsfile\n",argv[0]);
    dms_exit(EDMS_FAIL);
  }
  stat = chk_filename(argv[argc-1], dmsdb, dmsfile);
  if(stat != 0)
  { printf("Error: DataBase redundance.\n");
    printf("DB:%s\tDMSFile:%s\n",dmsdb,argv[argc-1]);
    dms_exit( stat );
  }
  if( rm_f )
  { printf("Certainly delete File %s? (y/n)",dmsfile);
    ans = getchar();
    if(tolower(ans) != 'y')
    { printf("Warn: File(%s) not delete.\n",dmsfile);
      dms_exit(EDMS_OK);
    }
  }
  stat = dms_fdlt(dmsfile,phyname);
  if(stat != 0)
    printf("Error: E%04x DMSFile(%s) delete error.\n",stat,phyname);
  else
    printf("DMSFile(%s) has been delete.\n",phyname);
  dms_exit(stat);
}

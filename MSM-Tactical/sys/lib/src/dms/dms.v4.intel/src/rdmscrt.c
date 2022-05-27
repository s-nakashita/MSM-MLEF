/*---------------------------------------------------------------------------*\
 *                                                                           *
 *  Program file: rdmscrt.c                                                  *
 *  Purpose: This program is a main program for rdmscrt utility.             *
 *                                                                           *
\*---------------------------------------------------------------------------*/
                                                                       
#include <stdio.h>
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
*   rdmscrt [-a dmsdb] [-v] dmsfile
*
* Description :
*   Create dmsfile.
*
* Arguments :
*   -a dmsdb
*      Specific database that dmsfile will be created.
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
*      dms_fcrt()
*
* Return Value :
*   0  : execution successfully.
*   -1 : Process was aborted because errors occurred.
*        Please check standard error or log file for reasons cause the error.
*
* Example :
*
*   rdmscrt -a UFSDB ufsfile
*
*   rdmscrt ufsfile@UFSDB@dbadm@dcs42.mic.cwb
*
* Restriction :
*   Only the C(Create) privilege can creat remote dmsfile.
*
* See Also :
*   rdmspurge(1)
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
  char dmsfile[MAX_DMS_FNAME],*dmsdb=NULL,phyname[MAX_DMS_FNAME];
  char cfgname[MAX_DMS_FNAME],cfgstr[MAX_DMS_STRL],argument[8];
  int c,errflg=0,stat;

/**  log_fptr(stderr);  **/
  stat = dms_init(NULL);
  if(argc < 2)
  { printf("Usage: %s [-lKeylen][-aDataBase][-c CFGSTR=CFGVAL][-v] dmsfile\n",argv[0]);
    dms_exit(EDMS_FAIL);
  }
  cfgstr[0] = '\0';
  while((c=getopt(argc, argv,"l:a:c:v")) != EOF)
  { switch(c)
    { case 'l' : strcpy( argument, optarg );
                 stat = dms_conf(S_KEYLEN, argument);
                 if(stat != EDMS_OK)
                 { printf("Error: E%04x Keylen(%d) set error.\n",stat,argument);
                   dms_exit(stat);
                 }
                 break;
      case 'a' : dmsdb=optarg;
                 break;
      case 'c' : strcat(cfgstr,optarg);
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
  { printf("Usage: %s [-lKeylen][-aDataBase][-v] dmsfile\n",argv[0]);
    dms_exit(EDMS_FAIL);
  }
  stat = chk_filename(argv[argc-1], dmsdb, dmsfile);
  if( stat != 0 )
  { printf("Error: DataBase redundance.\n");
    printf("DB:%s\tDMSFile:%s\n",dmsdb,argv[argc-1]);
    dms_exit( stat );
  }
  stat = dms_fcrt(dmsfile,phyname,cfgstr);
  if(stat != EDMS_OK)
    printf("Error: E%04x create DMSFile(%s) error.\n",stat,phyname);
  else
    printf("DMSFile(%s) is created.\n",phyname);
  dms_exit(stat);
}


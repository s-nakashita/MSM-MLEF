/*---------------------------------------------------------------------------*\
 *                                                                           *
 *  Program file: rdmsreorg.c                                                *
 *  Purpose: This program is the main program of rdmspurge utility.          *
 *                                                                           *
\*---------------------------------------------------------------------------*/

#include <stdio.h>
#include "dms.h"
#include "dmserrno.h"
#include "dms_log.h"
#include "dms_api.h"

/*---------------------------------------------------------------------------*/
/*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/
/*---------------------------------------------------------------------------*/
main(int argc,char *argv[])
/*---------------------------------------------------------------------------*/
{ extern char *optarg;
  char dmsfile[MAX_DMS_FNAME],phyname[MAX_DMS_FNAME],argument[8],*dmsdb=NULL;
  int stat,errflg=0,c;

  stat = dms_init(NULL);
  if(argc < 2)
  {  printf("Usage: %s [-aDataBase][-v] dmsfile\n",argv[0]);
     dms_exit(EDMS_FAIL);
  }
  while ((c=getopt(argc, argv,"a:v")) != EOF)
  { switch(c)
    { case 'a' : dmsdb=optarg;
                 break;
      case 'v' : sprintf( argument, "%d", LOG_DBG );
                 dms_conf(S_LOG,argument);
                 break;
      case '?' : errflg++;
                 break;
    }
  }
  if(errflg)
  { printf("Usage: %s [-aDataBase][-v] dmsfile\n",argv[0]);
    dms_exit(EDMS_FAIL);
  }
  stat = chk_filename(argv[argc-1], dmsdb, dmsfile);
  if(stat != 0)
  { printf("Error: DataBase redundance.\n"); 
    printf("DB:%s\tDMSFile:%s\n",dmsdb,argv[argc-1]);
    dms_exit( stat );
  }
  stat = dms_frog(dmsfile,phyname);
  if(stat != 0)
    printf("Error: E%04x DMSFile(%s) reorganize error.\n",stat,phyname);
  else
  { printf("DMSFile (%s)\n",phyname);
    printf("%s reorganization succeeded.\n",dmsfile);
  }
  dms_exit(stat);
}

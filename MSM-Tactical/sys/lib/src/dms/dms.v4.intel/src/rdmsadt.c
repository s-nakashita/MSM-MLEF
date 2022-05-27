/*---------------------------------------------------------------------------*\
 *                                                                           *
 *  Program file: rdmsadt.c                                                  *
 *  Purpose: This program is the main program of rdmsadt utility.            *
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
/*---------------------------------------------------------------------------*/
main(int argc,char *argv[])
/*---------------------------------------------------------------------------*/
{ extern char *optarg;
  FILE *fout;
  DMS   dms;
  DMST  keyset;
  char  *outfile=NULL,*dmsdb=NULL,*pout,key[MAXKEYL];
  char  dmsfile[MAX_DMS_FNAME],argument[8];
  int   stat=0,number=0,c,errflg=0,buffsize=MAXBUFFER,keylen=0;

/**  log_fptr(stderr);  **/ 
  stat = dms_init(NULL);
  if( stat != EDMS_OK )
  { printf("Error: E%04x DMS Initiation error.\n",stat);
    dms_exit( stat );
  }
  if( argc < 2 )
  { printf("Usage: %s [-lKeyLen][-mMemSize][-oOutFile][-aDataBase][-v] dmsfile\n",argv[0]);
    dms_exit( EDMS_FAIL );
  }
  while( (c=getopt(argc, argv,"m:o:a:l:v")) != EOF )
  { switch(c)
    {
      case 'm' : buffsize=max(atof(optarg)*1024,buffsize);
                 break;
      case 'o' : outfile=optarg;
                 break;
      case 'a' : dmsdb = optarg;
                 break;
      case 'l' : keylen = atoi(optarg);
                 stat = key_use(keylen);
                 if( stat == EDMS_FAIL )
                 { printf("Error: E%04x Key Length error.\n",EDMS_KEYLEN);
                   dms_exit(EDMS_KEYLEN);
                 }
                 break;
      case 'v' : sprintf( argument, "%d", LOG_DBG );
                 dms_conf(S_LOG,argument);
                 break;
      case '?' : errflg++;
                 break;
     }
  }
  if( errflg )
  { printf("Usage: %s [-lKeyLen][-mMemSize][-oOutFile][-aDataBase][-v] dmsfile\n",argv[0]);
    dms_exit( EDMS_FAIL );
  }

  if( keylen == 0 )
  { printf("Error: Please choose (-l KeyLen).\n");
    dms_exit(EDMS_FAIL);
  }

  stat = chk_filename(argv[argc-1],dmsdb,dmsfile);
  if( stat != 0 )
  { printf("Error: DataBase redundance.\n");
    printf("DB:%s\tDMSFile:%s\n",dmsdb,argv[argc-1]);
    dms_exit( stat );
  }

  if( (pout=(char *)malloc(buffsize)) == NULL )
  { printf("Error: Can't allocate memory, exit!\n");
    dms_exit( EDMS_FAIL );
  }
  else
    memset(pout,0x00,buffsize);
  stat = dms_fopn(dmsfile,ODMS_RDONLY,&dms);
  if( stat != EDMS_OK )
  { printf("Error: E%04x Dmsfile(%s) open error!\n",stat,dmsfile);
    free(pout);
    dms_exit(stat);
  }

  number = buffsize / (keylen+1);
  memset(key,'?',keylen); key[keylen] = '\0';
  keyset.data=key;
  keyset.size=strlen(keyset.data);
  stat = dms_rlst(&dms,&keyset,pout,&number);
  if( stat != EDMS_OK )
  { printf("Error: E%04x Dmsfile(%s) audit error!\n",stat,dmsfile);
    free(pout);
    dms_exit(stat);
  }
  if( outfile == NULL )
    fout=stdout;
  else
  { if( (fout=fopen(outfile,"a")) == NULL )
    { printf("Error: %s open error.\n",outfile);
      free(pout);
      dms_exit( EDMS_FAIL );
    }
  }
  fprintf(fout,"dmsfile (%s)\n",dms.phyname);
  adtfflush(fout,pout,number,keylen);
  fprintf(fout,"Total of %s = %d\n",dmsfile,number);
  if (fout != stdout) fclose(fout);
  free(pout);
  dms_fcls(&dms);
  dms_exit(stat);
}


/*---------------------------------------------------------------------------*\
 *                                                                           *
 *  Program file: rdmsdlt.c                                                  *
 *  Purpose: This program is a main program for rdmsdlt utility.             *
 *                                                                           *
\*---------------------------------------------------------------------------*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <fcntl.h>
#include <errno.h>
#include "dms.h"
#include "dmserrno.h"
#include "dms_log.h"
#include "dms_api.h"
#include "key_lib.h"

/*---------------------------------------------------------------------------*/
/*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/
/*---------------------------------------------------------------------------*/
main(int argc,char *argv[])
/*---------------------------------------------------------------------------*/
{ extern char *optarg;
  DMS dms;
  DMST keyset,dltkey;
  char *outfile=NULL,*kptr=NULL,*dmsdb=NULL,*buffer,*ptr,argument[8];
  char key[MAXBUFFER],temp[MAXKEYL],dmsfile[MAX_DMS_FNAME],msgstr[MAX_DMS_STRL];
  FILE *dirf=NULL;
  int  i,j,c,errflg=0,out,no=0,stat=0,keylen=0,keynum,buffsize=MAXBUFFER;

/**  log_fptr(stderr);  **/
  stat = dms_init(NULL);
  if(argc < 2)
  { printf("Usage: %s [(-kKey)(-iKeyList)][-mMemSize][-oOutFile][-aDataBase][-v] dmsfile\n",argv[0]);
    dms_exit(EDMS_FAIL);
  }
  while((c=getopt(argc, argv,"k:i:m:o:a:l:v")) != EOF)
  { switch(c)
    { 
      case 'k' : kptr=optarg;
                 break;
      case 'i' : dirf=fopen(optarg,"r");
                 break;
      case 'm' : buffsize=max(atof(optarg)*1024,buffsize);
		 break;
      case 'o' : outfile=optarg;
                 break;
      case 'a' : dmsdb=optarg;
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

  if(errflg)
  { printf("Usage: %s [(-kKey)(-iKeyList)][-mMemSize][-oOutFile][-aDataBase][-v] dmsfile\n",argv[0]);
    dms_exit(EDMS_FAIL);
  }

  if( (buffer=(char *)malloc(buffsize)) == NULL )
  { printf("Error: Can't allocate memory, exit!\n");
    dms_exit( EDMS_FAIL );
  }

  stat = chk_filename(argv[argc-1],dmsdb,dmsfile);
  if( stat != 0 )
  { printf("Error: DataBase redundance.\n");
    printf("DB:%s\tDMSFile:%s\n",dmsdb,argv[argc-1]);
    dms_exit( stat );
  }

  stat = dms_fopn(dmsfile,ODMS_RDWR,&dms);
  if(stat != EDMS_OK)
  { printf("Error: E%04x DMSFile(%s) open error.\n",stat,dmsfile);
    dms_exit(EDMS_FAIL);
  }

  if(kptr != NULL)
  {
    ptr = strchr(kptr,'*');
    if( ptr != NULL && keylen == 0 )
    { printf("Error: Please choose (-l KeyLen).\n");
      dms_exit(EDMS_FAIL);
    }
    else if ( ptr == NULL )
    { keylen = strlen(kptr); }
    memcpy(&key[no],key_expend(kptr,keylen),keylen+1);
    no += (keylen+1);
  }

  if(dirf != NULL)
  { while( fgets(temp,MAXKEYL,dirf) != NULL )
    { temp[strlen(temp)-1]='\0';
      if( keylen == 0 ) keylen = strlen(temp);
      memcpy(&key[no],key_expend(temp,keylen),keylen+1);
      no += (keylen+1);
      if ( (no+(keylen+1)) > MAXBUFFER )
      { printf("Error: Too many key in dirf(%s)\n",dirf);
        fclose(dirf);
        dms_exit(EDMS_FAIL);
      }
    }
    fclose(dirf);
  }

  if(no == 0)
  { printf("Error: Please choose (-k Key) or (-i KeyList).\n");
    dms_exit(EDMS_FAIL);
  }

  if (outfile == NULL) out=1;
  else
  { if ( (out=open(outfile,O_WRONLY | O_CREAT | O_TRUNC,0644)) == -1)
    { printf("Error: E%04x Open file(%s) error.\n",errno,outfile);
      dms_exit(EDMS_FAIL);
    }
  }
  if( no == (keylen+1) )
  { keyset.data = key;
    keyset.size = strlen(key);
  }
  else
  { memset(temp,'?',keylen); temp[keylen] = '\0'; 
    keyset.data = temp;
    keyset.size = strlen(keyset.data);
  }
  keynum = buffsize / (keylen+1);
  stat = dms_rlst(&dms,&keyset,buffer,&keynum);
  if(stat == EDMS_OK)
  { if(keynum == 0)
    { printf("Warm: DMSFile(%s) has no match record.\n",dms.phyname); }
    else
    { printf("DMSFile (%s)\n",dms.phyname);
      for( i=0,ptr=buffer; i<keynum; i++,ptr+=(keylen+1) )
      { for( j=0; j<no; j+=(keylen+1) )
        { if( key_compare(ptr,&key[j],keylen) == 0 )
          { dltkey.data = ptr;
            dltkey.size = strlen(ptr);
            stat = dms_rdlt(&dms,&dltkey);
            if( stat == 0 )
            { sprintf(msgstr,"DMSDLT: %s\n",dltkey.data);
              resp_buff(out,msgstr,strlen(msgstr));
            }
            else
            { sprintf(msgstr,"Error: E%04x Key(%s) delete error.\n",stat,dltkey.data);
              resp_buff(out,msgstr,strlen(msgstr));
            }
          } /* compare */
        } /* for j */
      } /* for i */
    } /* else */
  }
  else
  { printf("Error: E%04x (%s) dms_rlst error.\n",stat,dms.phyname); }
  dms_fcls(&dms);
  if (out != 1) close(out);
  dms_exit( stat );
}


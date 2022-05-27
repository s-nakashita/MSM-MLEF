/*---------------------------------------------------------------------------*\
 *                                                                           *
 *  Program file: rdmscpy.c                                                  *
 *  Purpose: This program is a main program for rdmscpy utility.             *
 *                                                                           *
\*---------------------------------------------------------------------------*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <fcntl.h>
#include "dms.h"
#include "dmserrno.h"
#include "dms_log.h"
#include "dms_api.h"
#include "key_lib.h"

int buffsize=MAXBUFFER;
/*---------------------------------------------------------------------------*/
/*++++++++++++++++++< Routines for internal reference >++++++++++++++++++++++*/
/*---------------------------------------------------------------------------*/
int dms_copy(int out,DMS *dmsff,DMS *dmstt,DMST *keyset)
/*---------------------------------------------------------------------------*/
{ int  n,stat,status,i,j,keynum,keylen;
  char msgstr[MAX_DMS_STRL],*keyptr;
  DMST tmpkey,tmpdata,rlstkey,buffer;

  INITDMST(tmpkey);
  INITDMST(tmpdata);
  INITDMST(buffer);

  keylen = dmsff->keylen;
  stat = getbuf(&tmpkey,buffsize);
  if(stat != EDMS_OK)
  { printf("DMSCPY: E%04x get buffer error.\n",stat);
    return(stat);
  }
  stat = getbuf(&buffer,buffsize);
  if(stat != EDMS_OK)
  { printf("DMSCPY: E%04x get buffer error.\n",stat);
    return(stat);
  }
  printf("From_DMSFile (%s)\n",dmsff->phyname);
  printf("To_DMSFile (%s)\n",dmstt->phyname);
  for(i=0; i<keyset->size; i+=(keylen+1))
  { rlstkey.data = &keyset->data[i];
    rlstkey.size = keylen+1;
    keynum = buffer.size / (dmsff->keylen+1);
    stat = dmsff->rlst(dmsff,&rlstkey,buffer.data,&keynum);
    if(stat != EDMS_OK)
    { printf("Error: E%04x (%s) record list error.\n",stat,rlstkey.data);
      continue;
    }
    for(j=0,keyptr=buffer.data; j<keynum; j++,keyptr+=(keylen+1))
    { 
      tmpkey.size = dmsff->keylen;
      strncpy(tmpkey.data,keyptr,dmsff->keylen);
      tmpkey.data[dmsff->keylen] = '\0';
      n = key_nitm(tmpkey.data)*key_size(tmpkey.data);
      stat = getbuf(&tmpdata,n);
      if(stat != EDMS_OK)
      { status = stat;
        printf("DMSCPY: E%04x get buffer error.\n",stat);
        continue;
      }
      stat = dmsff->rget(dmsff,&tmpkey,&tmpdata);
      if(stat != EDMS_OK)
      { status = stat;
        printf("DMSCPY: E%04x (%s) record get error.\n",stat,tmpkey.data);
        continue;
      }
      stat = dmstt->rput(dmstt,&tmpkey,&tmpdata);
      if (stat != EDMS_OK)
      { status = stat;
        printf("DMSCPY: E%04x (%s) record put error.\n",stat,tmpkey.data);
        continue;
      }
      sprintf(msgstr,"DMSCPY: %s\n",tmpkey.data);
      resp_buff(out,msgstr,strlen(msgstr));
    } /* for j */
  } /* for i */
  FREEDMST(tmpkey);
  FREEDMST(tmpdata);
  FREEDMST(buffer);
  return(stat);
}

/*---------------------------------------------------------------------------*/
/*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/
/*---------------------------------------------------------------------------*/
main(int argc,char *argv[])
/*---------------------------------------------------------------------------*/
{ extern char *optarg;
  FILE *dirf=NULL;
  DMS fdms,tdms;
  DMST keyset;
  char fdmsfile[MAX_DMS_FNAME],tdmsfile[MAX_DMS_FNAME],temp[MAXKEYL];
  char *fdmsdb=NULL,*tdmsdb=NULL,*kptr=NULL,*outfile=NULL;
  char argument[8],*key,*ptr;
  int  errflg=0,no=0,keylen=0,i,out,c,stat;

  dms_init(NULL);
  if(argc < 2)
  { printf("Usage: %s [(-kKey)(-iKeyList)][-lKeyLen][-mMemSize][-oOutfile][-aF_DB][-bT_DB][-v] F_dmsfile T_dmsfile\n",argv[0]);
    dms_exit(EDMS_FAIL);
  }
  while((c=getopt(argc, argv, "k:i:o:a:b:m:l:v")) != EOF)
  { switch(c)
    { 
      case 'k' : kptr = optarg;
                 break;
      case 'i' : dirf = fopen(optarg,"r");
                 break;
      case 'o' : outfile = optarg;
                 break;
      case 'a' : fdmsdb = optarg;
                 break;
      case 'b' : tdmsdb = optarg;
                 break;
      case 'm' : buffsize = max(atof(optarg)*1024,buffsize);
                 break;
      case 'l' : keylen = atoi(optarg);
                 stat = key_use(keylen);
                 if( stat == EDMS_FAIL ) 
                 { printf("Error: E%04x Key Length error.\n",EDMS_KEYLEN);
                   dms_exit(EDMS_KEYLEN);         
                 }
                 break;
      case 'v' : sprintf( argument, "%d", LOG_DBG );
                 dms_conf(S_LOG, argument);
                 break;
      case '?' : errflg++;
                 break;
    }
  }

  if( errflg )
  { printf("Usage: %s {(-kKey)|(-iKeyList)}[-lKeyLen][-mMemSize][-oOutfile][-aF_DB][-bT_DB][-kKeyLen][-v] F_dmsfile T_dmsfile\n",argv[0]);
    dms_exit(EDMS_FAIL);
  }

  stat = chk_filename(argv[argc-2],fdmsdb,fdmsfile);
  if( stat != 0 )
  { printf("Error: E%04x DataBase redundance.\n",stat);
    printf("DB:%s\tDMSFile:%s\n",fdmsdb,argv[argc-2]);
    dms_exit( stat );
  }
  stat = chk_filename(argv[argc-1],tdmsdb,tdmsfile);
  if( stat != 0 )
  { printf("Error: E%04x DataBase redundance.\n",stat); 
    printf("DB:%s\tDMSFile:%s\n",tdmsdb,argv[argc-1]);
    dms_exit( stat );
  }

  if( (key=(char *)malloc(buffsize)) == NULL )
  { printf("Error: E%04x Can't allocate memory for key, exit!\n",EDMS_NOMEM);
    dms_exit(EDMS_FAIL);
  } else
    memset(key,0x00,buffsize);

  stat = dms_fopn(fdmsfile,ODMS_RDONLY,&fdms);
  if(stat != EDMS_OK)
  { printf("Error: E%04x Open source dmsfile error.\n",stat);
    printf("Dmsfile: %s\n",fdmsfile);
    dms_exit(stat);
  }

  stat = dms_fopn(tdmsfile,ODMS_RDWR,&tdms);
  if(stat != EDMS_OK)
  { printf("Error: E%04x Open target dmsfile error.\n",stat);
    printf("Dmsfile: %s\n",tdmsfile);
    dms_exit(stat);
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
      if ( (no+(keylen+1)) > buffsize )
      { printf("Too many key in dirf(%s)\n",dirf);
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

  fdms.keylen = keylen;
  tdms.keylen = keylen;
  key_use(keylen);

  if (outfile == NULL) out=1;
  else
  { if((out=open(outfile,O_WRONLY | O_CREAT | O_TRUNC,0644)) == -1)
    { printf("Error: %s open failed.\n",outfile);
      dms_exit(EDMS_FAIL);
    }
  }

  keyset.size = no;
  keyset.data = key;
  stat = dms_copy(out,&fdms,&tdms,&keyset);
  dms_fcls(&fdms);
  dms_fcls(&tdms);
  if(out != 1) close(out);
  free(key);
  dms_exit(stat);
}

/*---------------------------------------------------------------------------*/
/*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/
/*---------------------------------------------------------------------------*/

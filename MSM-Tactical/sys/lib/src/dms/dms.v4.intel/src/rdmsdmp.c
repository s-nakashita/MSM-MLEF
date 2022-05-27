/*---------------------------------------------------------------------------*\
 *                                                                           *
 *  Program file: rdmsdmp.c                                                  *
 *  Purpose: This program is a main program for rdmsdmp utility.             *
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

static long buffsize=MAXBUFFER;

/*---------------------------------------------------------------------------*/
/*++++++++++++++++++< Routines for internal reference >++++++++++++++++++++++*/
/*---------------------------------------------------------------------------*/
int dms_dump(int out,DMS *dmsp,char *MACODE,DMST *keyset)
/*---------------------------------------------------------------------------*/
{ char msgstr[128],tmpbuff[MAXKEYL],*kptr,*dptr,key[MAXKEYL];
  int i,j,stat,status=0,xdr_n,get_n,keynum,keylen;
  DMST tmpkey,tmprlst,bbf_xdr,bbf_get,bbf_key;

  /* Write Dump File Header */
  sprintf(msgstr,"KEYLEN=%d\n",dmsp->keylen);
  resp_buff(out,msgstr,strlen(msgstr));
  sprintf(msgstr,"%s\n",MACODE);
  resp_buff(out,msgstr,strlen(msgstr));

  sprintf(msgstr,"DMSFile (%s)\n",dmsp->phyname);
  resp_buff(1,msgstr,strlen(msgstr));

  INITDMST(tmprlst);
  INITDMST(bbf_xdr);
  INITDMST(bbf_get);
  stat = getbuf(&tmprlst,buffsize);
  if (stat != 0) 
  { printf("Error: E%04x no memory %d\n",stat,buffsize); 
    return(stat);
  }

  keylen = dmsp->keylen;
  if( keyset->size <= (keylen+1) )
  { tmpkey.data = keyset->data;
    tmpkey.size = keyset->size;
  }
  else
  { memset(key,'?',keylen); key[keylen] = '\0';
    tmpkey.data = key;
    tmpkey.size = strlen(tmpkey.data);
  }
  keynum = buffsize / (keylen+1);
  stat = dms_rlst(dmsp,&tmpkey,tmprlst.data,&keynum);
  if (stat == EDMS_OK)
  { if( keynum == 0)
    { printf("Warm: DMSFile(%s) has no match record.\n",dmsp->phyname);
      FREEDMST(tmprlst);
      return(EDMS_OK);
    }
    for(i=0,kptr=tmprlst.data;i<keynum;i++,kptr+=(keylen+1))
    { for(j=0,dptr=keyset->data; j<keyset->size; j+=(keylen+1),dptr+=(keylen+1))
      { kptr[keylen]='\0';
        if(key_compare(kptr,dptr,keylen) == 0)
        { strcpy(tmpbuff,kptr);
          if(strcmp(MACODE,"XDR") == 0 && xdr_size(tmpbuff) > 0)
          { xdr_n = xdr_size(tmpbuff)*key_nitm(tmpbuff);
            stat = getbuf(&bbf_xdr,xdr_n);
            if (stat != EDMS_OK)
            { printf("Error: E%04x no memory %d\n",stat,xdr_n); 
              FREEDMST(tmprlst); return(stat); 
            }
          }
          get_n = key_size(tmpbuff)*key_nitm(tmpbuff);
          stat = getbuf(&bbf_get,get_n);
          if (stat != EDMS_OK)
          { printf("Error: E%04x no memory %d\n",stat,xdr_n); 
            FREEDMST(tmprlst); FREEDMST(bbf_xdr); return(stat); 
          }
          bbf_key.size=strlen(tmpbuff);
          bbf_key.data=tmpbuff;
          stat = dmsp->rget(dmsp,&bbf_key,&bbf_get);
          if (stat == EDMS_OK)
          { sprintf(msgstr,"\n#%s",tmpbuff);
            resp_buff(out,msgstr,strlen(msgstr));
            if(strcmp(MACODE,"XDR") == 0 && xdr_size(tmpbuff) > 0)
            { xdr_fun("E",tmpbuff,bbf_get.data,bbf_xdr.data,key_nitm(tmpbuff));
              resp_buff(out,(void *)bbf_xdr.data,xdr_n);
              sprintf(msgstr,"DMSDMP : %s\n",tmpbuff);
              resp_buff(1,msgstr,strlen(msgstr));
            }
            else
            { resp_buff(out,(void *)bbf_get.data,get_n);
              sprintf(msgstr,"DMSDMP : %s\n",tmpbuff);
              resp_buff(1,msgstr,strlen(msgstr));
            }
          }
          else
          { printf("Error: E%04x record get error.\n",stat); } 
        } /* compare */
      } /* for j */
      if (stat != EDMS_OK) status = stat;
    } /* for i */
  } /* if */
  else
  { printf("Error: E%04x (%s) dms_rlst error.\n",stat,dmsp->phyname); }
  FREEDMST(tmprlst);
  FREEDMST(bbf_xdr);
  FREEDMST(bbf_get);
  return(status);
}

/*---------------------------------------------------------------------------*/
/*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/
/*---------------------------------------------------------------------------*/
main(int argc,char *argv[])
/*---------------------------------------------------------------------------*/
{ extern char *optarg;
  FILE *dirf=NULL,*outfp;
  DMS dms;
  DMST keyset;
  char dmsfile[MAX_DMS_FNAME],temp[MAXKEYL],argument[8],mopt[10];
  char *key,*dmsdb=NULL,*kptr=NULL,*ptr;
  int  stat=0,xflag=0,errflg=0,no=0,keylen=0,out,c;

  stat = dms_init(NULL);
  if(argc < 2)
  { printf("Usage: %s [-lKeylen][(-kKey)(-iKeyList)][-mMemSize][-X][-aDataBase][-v] dmsfile dumpfile\n",argv[0]);
    dms_exit(EDMS_FAIL);
  }
  while ((c=getopt(argc, argv,"k:i:m:a:l:Xv")) != EOF)
  { switch(c)
    { 
      case 'm' : buffsize = max(atof(optarg)*1024,MAXBUFFER);
                 break;
      case 'k' : kptr = optarg;
                 break;
      case 'i' : dirf = fopen(optarg,"r");
                 break;
      case 'X' : xflag = 1;
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
      case 'v' : sprintf(argument, "%d", LOG_DBG);
	  	 dms_conf(S_LOG,argument);
                 break;
      case '?' : errflg++;
                 break;
    }
  }

  if(errflg)
  { printf("Usage: %s [-lKeylen][(-kKey)(-iKeyList)][-mMemSize][-X][-aDataBase][-v] dmsfile dumpfile\n",argv[0]);
    dms_exit(EDMS_FAIL);
  }

  stat = chk_filename(argv[argc-2], dmsdb, dmsfile);
  if( stat != 0 )
  { printf("Error: DataBase redundance.\n");
    printf("DB:%s\tDMSFile:%s\n",dmsdb,argv[argc-2]);
    dms_exit( stat );
  }

  stat = dms_fopn(dmsfile,ODMS_RDONLY,&dms);
  if(stat != EDMS_OK)
  { printf("Error: E%04x DMSFile(%s) open error.\n",stat,dmsfile);
    dms_exit(EDMS_FAIL);
  }


  if ((key=(char *)malloc(buffsize)) == NULL)
  { printf("Usage: Can't allocate memory, exit!\n");
    dms_fcls(&dms);
    dms_exit(EDMS_FAIL);
  } else memset(key,0x00,buffsize);

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
      { printf("Error: Too many key in dirf(%s)\n",dirf);
        fclose(dirf);
        free(key);
        dms_fcls(&dms);
        dms_exit(EDMS_FAIL);
      }
    }
    fclose(dirf);
  }

  dms.keylen = keylen;

  if(no == 0)
  { printf("Error: Please choose (-k Key) or (-i KeyList).\n");
    free(key);
    dms_fcls(&dms);
    dms_exit(EDMS_FAIL);
  }
  if(xflag == 1)
    strcpy(mopt,"XDR");
  else
    strcpy(mopt,FORMAT);
  if((out=open(argv[argc-1],O_WRONLY|O_CREAT|O_TRUNC,0644)) == -1)
  { printf("Error: Can't open Dump file (%s)\n",argv[argc-1]);
    free(key);
    dms_exit(EDMS_FAIL);
  }
  keyset.size = no;
  keyset.data = key;
  stat = dms_dump(out,&dms,mopt,&keyset);
  close(out);
  free(key);
  dms_fcls(&dms);
  dms_exit(stat);
}


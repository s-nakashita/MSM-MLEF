/*---------------------------------------------------------------------------*\
 *                                                                           *
 *  Program file: rdmsdmp.c                                                  *
 *  Purpose: This program is a main program for rdmsdmp utility.             *
 *                                                                           *
\*---------------------------------------------------------------------------*/
                                                                  
#include <stdio.h>
#include <stdlib.h>
#include <fcntl.h>
#include "dms.h"
#include "dmserrno.h"
#include "dms_log.h"
#include "dms_api.h"
#include "key_lib.h"

static long buffsize=MAXBUFFER;
char LDFMT[16];

/*---------------------------------------------------------------------------*/
/*++++++++++++++++++< Routines for internal reference >++++++++++++++++++++++*/
/*---------------------------------------------------------------------------*/
int dmp_head(FILE *ldfp,char *MACODE,int *keylen)
/*---------------------------------------------------------------------------*/
{
  int  dkeylen=24,stat;

  /* Check Dump file header */
  fgets(LDFMT,16,ldfp);
  LDFMT[strlen(LDFMT)-1]='\0';
  if ( strncmp(LDFMT,KEYLEN,strlen(KEYLEN)) == 0 )
  { sscanf(LDFMT,"KEYLEN=%d",&dkeylen);
    stat = key_use(dkeylen);
    if( stat == EDMS_FAIL ) return(EDMS_KEYLEN);
    fgets(LDFMT,8,ldfp);
    LDFMT[strlen(LDFMT)-1]='\0';
  }
  *keylen = dkeylen;
  if (strcmp(LDFMT,MACODE) != 0 && strcmp(LDFMT,"XDR") != 0) 
  { printf("DMSLD : Dump file type mismatch (%s,%s)!\n",LDFMT,MACODE);
    return(EDMS_DMPTYPE);
  }
  else
  { return(EDMS_OK); }
}

/*---------------------------------------------------------------------------*/
int dms_load(FILE *ldfp,DMS *dmsp,DMST *keyset)
/*---------------------------------------------------------------------------*/
{ char msgstr[128],tmpbuff[MAXKEYL],*ptr;
  int  i,c,stat,status=0,keylen,xdr_n,get_n;
  DMST tmpkey,tmpdata,bbf_xdr,bbf_get;

  uppercase(keyset->data);
  INITDMST(bbf_xdr);
  INITDMST(bbf_get);

  keylen = dmsp->keylen;
  printf("DMSFile (%s)\n",dmsp->phyname);

  for(;((c=getc(ldfp))!=35)&&(c!=EOF);) ;
  while (fread(tmpbuff,sizeof(char),keylen,ldfp) == keylen )
  { 
    tmpbuff[keylen]='\0';
    uppercase(tmpbuff);
    xdr_n = get_n = key_nitm(tmpbuff) * key_size(tmpbuff);
    if(strcmp(LDFMT,"XDR")==0 && xdr_size(tmpbuff) > 0)
      xdr_n = key_nitm(tmpbuff) * xdr_size(tmpbuff);
    if(xdr_n == 0) break;
    stat = getbuf(&bbf_get,get_n);
    stat = getbuf(&bbf_xdr,xdr_n);
    if( (stat=fread(bbf_xdr.data,sizeof(char),xdr_n,ldfp)) != xdr_n )
    { printf("DMSLD : (%s) read dumpfile error\n",tmpbuff);
      fclose(ldfp); FREEDMST(bbf_xdr); FREEDMST(bbf_get); 
      return(EDMS_DMPFILE);
    }
    if(strcmp(LDFMT,"XDR") == 0 && xdr_size(tmpbuff) > 0)
    { 
      xdr_fun("D",tmpbuff,bbf_get.data,bbf_xdr.data,key_nitm(tmpbuff));
      tmpdata.size = get_n;
      tmpdata.data = bbf_get.data;
    }
    else
    { 
      tmpdata.size = xdr_n;
      tmpdata.data = bbf_xdr.data; 
    }
    tmpkey.size = strlen(tmpbuff);
    tmpkey.data = tmpbuff;
    for(i=0,ptr=keyset->data; i<keyset->size; i+=(keylen+1),ptr+=(keylen+1))
    { 
      if(key_compare(tmpbuff,ptr,keylen) == 0)
      { 
        stat = dmsp->rput(dmsp,&tmpkey,&tmpdata);
        if(stat == 0)
        { printf("DMSLD : %s\n",tmpbuff); }
        else
        { printf("DMSLD : E%04x Key(%s) load data error\n",stat,tmpbuff);
          status = stat;
        }
      } /* if */
    } /* for */
    for(;((c=getc(ldfp))!=35)&&(c!=EOF);) ;
  }
  FREEDMST(bbf_xdr);
  FREEDMST(bbf_get);
  return(status);
}

/*
***************************************************************************
*
* Function Name :
*    main(int argc, char **argv)
*
* Man Page : yes
*
* Usage :
*    rdmsld {-kKey|-iKeyList} [-a Database] [-v] DMSFile Dumpfile
*
* Description :
*    To load the dump file to DMS file.
*
* Arguments :
*    -k Key 
*       To appoint the key you want to load to the dmsfile. You can use
*       the '?' or '*' to replace one or more characters.
*    -i KeyList
*    -a Database
*       Set database information
*    -v 
*
* Data Files : none
*
* Sub./Fun. Called :
*    dms_fopn(), dms_fcls(), dms_exit()
*
* Return Value :
*    0      : execution successfully.
*    Others : Process was aborted because errors occurred.
*             Please check log file for reasons cause the error.
*
* Example :
*    rdmsld -k"*" -a UFSDB ufsfile34 dms_key34.dmp
*
* Restrictions :
*    It can be loaded both the same OS type and XDR type.
*
* See Also :
*    rdmsdmp(1)
*
* Date :
*    May. 24, 2005
*
* Side Effect : none
*
**************************************************************************
*/
main(int argc,char *argv[])
{ 
  extern char *optarg;
  FILE *dirf=NULL,*ldfp;
  DMS dms;
  DMST keyset;
  char dmsfile[MAX_DMS_FNAME],temp[MAXKEYL],*dmsdb=NULL;
  char buff[MAX_DMS_STRL],argument[8];
  char *pdms=NULL,*dot_db=NULL,*kptr=NULL,*dmspath,*key;
  int  len,j=0,keylen,c,stat=0,xflag=0,errflg=0,no=0,i,n;

  stat = dms_init(NULL);
  if(argc < 2)
  { printf("Usage: %s [(-kKey)(-iKeyList)][-aDataBase][-v] dmsfile loadfile\n",argv[0]);
    dms_exit(EDMS_FAIL);
  }
  while ((c=getopt(argc, argv,"k:i:m:a:v")) != EOF)
  { switch(c)
    { 
      case 'm' : buffsize=max(atof(optarg)*1024,MAXBUFFER);
                 break;
      case 'k' : kptr = optarg;
                 break;
      case 'i' : dirf=fopen(optarg,"r");
                 break;
      case 'a' : dmsdb=optarg;
                 break;
      case 'v' : sprintf( argument, "%d", LOG_DBG );
                 dms_conf(S_LOG, argument);
                 break;
      case '?' : errflg++;
                 break;
    }
  }
  if(errflg)
  { printf("Usage: %s [(-kKey)(-iKeyList)][-aDataBase][-v] dmsfile dumpfile\n",argv[0]);
    dms_exit(EDMS_FAIL);
  }

  stat = chk_filename(argv[argc-2], dmsdb, dmsfile);
  if( stat != 0 )
  { printf("Error: DataBase redundance.\n"); 
    printf("DB:%s\tDMSFile:%s\n",dmsdb,argv[argc-2]);
    dms_exit( stat );
  }

  stat = dms_fopn(dmsfile,ODMS_RDWR,&dms);
  if(stat != EDMS_OK)
  { printf("Error: E%04x DMSFile(%s) open error.\n",stat,dmsfile);
    dms_exit(EDMS_FAIL);
  }

  if ((key=(char *)malloc(buffsize)) == NULL)
  { printf("Usage: Can't allocate memory, exit!\n");
    dms_fcls(&dms);
    dms_exit(EDMS_FAIL);
  } else memset(key,0x00,buffsize);

  if((ldfp=fopen(argv[argc-1],"r")) == NULL)
  { printf("Error: Can't open Dump file (%s)\n",argv[argc-1]);
    free(key);
    dms_exit(EDMS_FAIL);
  }
  else
  {
    stat = dmp_head(ldfp,FORMAT,&keylen);
    if(stat != EDMS_OK)
    { printf("Error: E%04x Dumpfile(%s) read error.\n",stat,argv[argc-1]);
      dms_exit(stat);
    }
  }

  if(kptr != NULL)
  { memcpy(&key[no],key_expend(kptr,keylen),keylen+1);
    no += (keylen+1);
  }

  if(dirf != NULL)
  { while( fgets(temp,MAXKEYL,dirf) != NULL )
    { temp[strlen(temp)-1]='\0';
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

  if(no == 0)
  { printf("Error: Please choose (-k Key) or (-i KeyList).\n");
    free(key);
    dms_fcls(&dms);
    dms_exit(EDMS_FAIL);
  }

  keyset.size = no;
  keyset.data = key;
  dms.keylen = keylen;
  stat = dms_load(ldfp,&dms,&keyset);
  fclose(ldfp);
  free(key);
  dms_fcls(&dms);
  dms_exit(stat);
}


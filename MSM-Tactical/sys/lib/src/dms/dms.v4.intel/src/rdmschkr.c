/*---------------------------------------------------------------------------*\
 *                                                                           *
 *  Program file: rdmschkr.c                                                 *
 *  Purpose: This program is the main program for rdmschkr utility.          *
 *                                                                           *
\*---------------------------------------------------------------------------*/
                                                                       
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <fcntl.h>
#include <errno.h>
#include <sys/socket.h>
#include "dms.h"
#include "dmserrno.h"
#include "dms_log.h"
#include "dms_api.h"
#include "key_lib.h"

#define percent "%"
/*- 6/17,2003 CPD change default float format to "e" and integer format -*/
#define f_format "%14.7e  "
#define l_format "%16ld  "
#define i_format "%12d  "
#define c_format "%c  "
#define b_format "%x  "

/*extern int errno;*/
int buffsize=MAXBUFFER;

/*---------------------------------------------------------------------------*/
/*++++++++++++++++++< Routines for internal reference >++++++++++++++++++++++*/
/* -------------------------------------------------------------------- */
static void rec_show(int out,char *keyp,char *datp,int from,int num,int col,char *format)
/* -------------------------------------------------------------------- */
{ char *buff,cc,temp[128];
  FILE *fp;
  long nwptr;
  union 
  {
     double *d;
#ifdef Full_Precision
     double *f;
#else
     float *f;
#endif
     float *r;
     long *l;
     short *s;
     int *i;
     char *c; 
  } data;
  int  sz,i,j,total,start,length;
  char fc;

  sz=key_size(keyp);
  total=key_nitm(keyp);
  /*- if( num == 0 ) num = total ;   6/16,2003 CPD : fix num=0 problem -*/
  data.c=&datp[sz*from];
  if( (num+from) > total ) num=total-from;
  if(num > 0)
  { key_type_def(&start,&length);
    /*- 6/17,2003 CPD add "fc" and alow "e" format for float -*/
    fc = tolower(format[strlen(format)-3]) ;
    switch(keyp[start])
    { case 'F' :
        if((format[0]!='*') && (fc!='f' && fc!='e'))
        { sprintf(temp,"** -f format <%c> error\n",fc);
          resp_buff(out,temp,strlen(temp));
          break;
        }
        for(i=0;i<num;i++)
        { if(format[0]=='*')
            sprintf(temp,f_format,data.d[i]);
          else
            sprintf(temp,format,data.d[i]);
          resp_buff(out,temp,strlen(temp));
          if(((i+1)%col)==0) resp_buff(out,"\n",1);
        }
        break;
      case 'H' :
        if((format[0]!='*') && (fc!='f' && fc!='e'))
        { sprintf(temp,"** -f format <%c> error\n",fc);
          resp_buff(out,temp,strlen(temp));
          break;
        }
        for(i=0;i<num;i++)
        { if(format[0]=='*')
            sprintf(temp,f_format,data.f[i]);
          else
            sprintf(temp,format,data.f[i]);
          resp_buff(out,temp,strlen(temp));
          if(((i+1)%col)==0) resp_buff(out,"\n",1);
        }
        break;
      case 'R' :
        if((format[0]!='*') && (fc!='f' && fc!='e'))
        { sprintf(temp,"** -f format <%c> error\n",fc);
          resp_buff(out,temp,strlen(temp));
          break;
        }
        for(i=0;i<num;i++)
        { if(format[0]=='*')
            sprintf(temp,f_format,data.r[i]);
          else
            sprintf(temp,format,data.r[i]);
          resp_buff(out,temp,strlen(temp));
          if(((i+1)%col)==0) resp_buff(out,"\n",1);
        }
        break;
      case 'L' :
        if(format[0]!='*')
        { if((fc!='i') && (fc!='d'))
          { sprintf(temp,"** -f format <%c> error\n",fc);
            resp_buff(out,temp,strlen(temp));
            break;
          }
        }
        for(i=0;i<num;i++)
        { if(format[0]=='*')
            sprintf(temp,l_format,data.l[i]);
          else
            sprintf(temp,format,data.l[i]);
          resp_buff(out,temp,strlen(temp));
          if(((i+1)%col)==0) resp_buff(out,"\n",1);
        }
        break;
      case 'I' :
        if(format[0]!='*')
        { if((fc!='i') && (fc!='d'))
          { sprintf(temp,"** -f format <%c> error\n",fc);
            resp_buff(out,temp,strlen(temp));
            break;
          }
        }
        for(i=0;i<num;i++)
        { if(format[0]=='*')
            sprintf(temp,i_format,data.i[i]);
          else
            sprintf(temp,format,data.i[i]);
          resp_buff(out,temp,strlen(temp));
          if(((i+1)%col)==0) resp_buff(out,"\n",1);
        }
        break;
      case 'S' :
        if(format[0]!='*')
        { if((fc!='i') && (fc!='d'))
          { sprintf(temp,"** -f format <%c> error\n",fc);
            resp_buff(out,temp,strlen(temp));
            break;
          }
        }
        for(i=0;i<num;i++)
        { if(format[0]=='*')
            sprintf(temp,i_format,data.s[i]);
          else
            sprintf(temp,format,data.s[i]);
          resp_buff(out,temp,strlen(temp));
          if(((i+1)%col)==0) resp_buff(out,"\n",1);
        }
        break;
      case 'C' :
        if((format[0]!='*') && (fc!='c'))
        { sprintf(temp,"** -f format <%c> error\n",fc);
          resp_buff(out,temp,strlen(temp));
          break;
        }
        for(i=0;i<num;i++)
        { if(format[0]=='*')
            sprintf(temp,c_format,data.c[i]);
          else 
            sprintf(temp,format,data.c[i]);
          resp_buff(out,temp,strlen(temp));
          if(((i+1)%col)==0) resp_buff(out,"\n",1);
        }
        break;
      case 'B' :
      case 'D' :
      case 'Q' :
      case 'O' :
      case 'X' :
        for(i=0;i<num;i++)
        { for(j=0;j<sz;j++) 
          { sprintf(temp,b_format,data.c[i*sz+j]);
            resp_buff(out,temp,strlen(temp));
          }
          resp_buff(out,"  ",2);
          if(((i+1)%col)==0) resp_buff(out,"\n",1);
        }
        break;
/******
        sprintf(temp,"\n **** Byte format can't show ***\n");
        resp_buff(out,temp,strlen(temp));
        break;
******/
    }
    resp_buff(out,"\n",1);
  }
}

/*---------------------------------------------------------------------------*/
static int dms_chkr(int out,DMS *dms,char *keyset,int keyno,int from,int num,int col,char *format)
/*---------------------------------------------------------------------------*/
{ int i,j,keylen,tmp,keynum,stat,status=0,find=0;
  char key[MAXKEYL],msgstr[MAX_DMS_STRL],*keyptr,*lstptr;
  DMST keyp,keybuf,tmpkey,tmpdata;

  INITDMST(tmpkey); INITDMST(tmpdata);

  keylen = dms->keylen;

  INITDMST(keybuf);
  stat = getbuf(&keybuf,buffsize);
  if (stat != EDMS_OK ) return(EDMS_NOMEM); 

  if( keyno == (keylen+1) )
  { keyp.data = keyset;
    keyp.size = strlen(keyset);
  }
  else
  { memset(key, '?', keylen);
    key[keylen] = '\0';
    keyp.data = key;
    keyp.size = strlen(keyp.data);
  }
  keynum = buffsize / (keylen+1);
  stat = dms->rlst(dms,&keyp,keybuf.data,&keynum);
  if(stat == EDMS_OK)
  { if(keynum == 0)
    { printf("Warm: DMSFile(%s) has no match record.\n",dms->phyname); }
    else
    { for( i=0,keyptr=keybuf.data; i<keynum; i++,keyptr+=(keylen+1) )
      { for( j=0; j<keyno; j+=(keylen+1) )
        { if( key_compare(keyptr,&keyset[j],keylen) == 0)
          { find=1;
            resp_buff(out,keyptr,strlen(keyptr));
            resp_buff(out,"\n",1);
            if( num > 0 ) 
            /*- if( num >= 0 )     6/16,2003 CPD : fix num=0 problem -*/
            {              
              tmpkey.data = keyptr;
              tmpkey.size = strlen(keyptr);
              tmp=key_size(tmpkey.data)*key_nitm(tmpkey.data);
              stat=getbuf(&tmpdata,tmp);
              if(stat != EDMS_OK)
              { printf("Error: E%04x getbuf error.\n",stat);
                status = stat;
                FREEDMST(keyp); FREEDMST(keybuf);
                return(stat);
              }
              stat = dms->rget(dms,&tmpkey,&tmpdata);
              if(stat != EDMS_OK)
              { printf("Error: E%04x Key(%s) getdata err.\n",stat,tmpkey.data);
                status = stat; break;
              }
              rec_show(out,tmpkey.data,tmpdata.data,from,num,col,format);
            } /* num > 0 */
          } 
        } /* for j */ 
      } /* for i */ 
      if(find == 0)
      { printf("Warm: DMSFile(%s) has no match record.\n",dms->phyname); }
    } 
  }
  else
  { printf("Error: E%04x (%s) dms_rlst error.\n",stat,dms->phyname); 
    status = stat;
  }
  FREEDMST(keybuf); FREEDMST(tmpdata);
  return(status);
}

/*---------------------------------------------------------------------------*/
/*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/
/*---------------------------------------------------------------------------*/
main(int argc,char *argv[])
/*---------------------------------------------------------------------------*/
{ extern char *optarg;
  char key[MAXBUFFER],dmsfile[MAX_DMS_STRL],temp[MAXKEYL],format[10],str[10];
  char *outfile=NULL,*dopt=NULL,*sopt=NULL,*kptr=NULL,*dmsdb=NULL,*dot_db=NULL;
  char argument[8],*dmspath,*ptr;
  FILE *dirf=NULL;
  int c,stat=0,errflg=0,fd,out,col=5,no=0,i,num=0,from=0,j=0,keylen=0;
  DMS dms;

  stat = dms_init(NULL);
  if( stat != EDMS_OK )
  { printf("Error: E%04x DMS Initiation error.\n",stat);
    dms_exit( stat );
  }
  if(argc < 2)
  { printf("Usage: %s [(-kKey)(-iDirf)][-lKeyLen][-mMemSize][-fFormat][-oOutFile][-dNo][-sNo:No][-aDataBase][-v] dmsfile\n",argv[0]);
    dms_exit(EDMS_FAIL);
  }
  format[0]='*';
  while( (c=getopt(argc, argv,"k:i:m:f:d:s:o:a:l:v")) != EOF )
  { switch(c)
    { 
      case 'k' : kptr = optarg;
                 break;
      case 'i' : dirf=fopen(optarg,"r");
                 break;
      case 'm' : buffsize=max(atof(optarg)*1024,MAXBUFFER);
		 break;
      /* Modified for the format of layout */
      case 'f' : if( isalpha(optarg[0]) )
                   strcpy(str,optarg);
                 else
                   sscanf(optarg,"%d%s",&col,str);
                 sprintf(format,"%s%s%c%s",percent,&str[1],str[0],"  ");
                 break;
      case 'd' : dopt=optarg;
                 break;
      case 's' : sopt=optarg;
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

  if( errflg )
  { printf("Usage: %s [(-kKey)(-iDirf)][-lKeyLen][-mMemSize][-fFormat][-oOutFile][-dNo][-sNo:No][-aDataBase][-v] dmsfile\n",argv[0]);
    dms_exit(EDMS_FAIL);
  }

  stat = chk_filename(argv[argc-1],dmsdb,dmsfile);
  if( stat != 0 )
  { printf("Error: E%04x DataBase redundance.\n",stat);
    printf("DB:%s\tDMSFile:%s\n",dmsdb,argv[argc-1]);
    dms_exit( stat );
  }

  stat = dms_fopn(dmsfile,ODMS_RDONLY,&dms);
  if( stat != EDMS_OK ) 
  { printf("Error: E%04x Open file(%s) error.\n",stat,dmsfile); 
    dms_exit( stat ); 
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

  dms.keylen = keylen;
  key_use(keylen);

  if (outfile == NULL) out=1;
  else
  { if ( (out=open(outfile,O_WRONLY | O_CREAT | O_TRUNC,0644)) == -1)
    { printf("Error: E%04x Open file(%s) error.\n",errno,outfile); 
      dms_exit(EDMS_FAIL); 
    }
  }

  if(dopt != NULL) num=atoi(dopt);
  if(sopt != NULL)
  { if( (ptr=strchr(sopt,':')) != NULL )
    { *ptr++ = 0;
      from=atoi(sopt)-1;
      if(from < 0)
      { printf("Error: Range error for check record.\n");
        dms_exit(EDMS_FAIL); 
      }
      num=atoi(ptr)-from;
    }
    else
    { printf("Error: Range error for check record.\n");
      dms_exit(EDMS_FAIL); 
    }
  }

  stat = dms_chkr(out,&dms,key,no,from,num,col,format);
  if(stat != EDMS_OK)
    printf("Error: E%04x DMS check record error.\n",stat);
  if (out!=1) close(out);
  dms_fcls(&dms);
  dms_exit(stat);
}


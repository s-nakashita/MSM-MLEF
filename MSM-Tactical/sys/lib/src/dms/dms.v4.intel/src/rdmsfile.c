/*---------------------------------------------------------------------------*\
 *                                                                           *
 *  Program file: rdmsfile.c                                                 *
 *  Purpose: This program is a main program for rdmsfile utility.            *
 *                                                                           *
\*---------------------------------------------------------------------------*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <dirent.h>
#include <unistd.h>
#include "dms.h"
#include "dmserrno.h"
#include "dms_log.h"
#include "dms_api.h"
#include "dms_env.h"

/*---------------------------------------------------------------------------*/
/*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/
/*---------------------------------------------------------------------------*/
main(int argc,char *argv[],char *env[])
/*---------------------------------------------------------------------------*/
{ extern char *optarg;
  struct dirent *entry;
  DIR *dir;
  FILE *fp;
  DMST flstbuf;
  int total=0,env_no=0,lgc_no=0,status=0,errflg=0;
  int stat,fileno,i,c,buffsize=MAXBUFFER,len;
  char *cc,*dmsdb=NULL,*ptr,argument[8];
  char dmspath[MAX_DMS_STRL*10],phyname[MAX_DMS_FNAME],s[MAX_DMS_FNAME];

/**  log_fptr(stderr);  **/
  INITDMST(flstbuf);
  stat = dms_init(NULL);
  while((c=getopt(argc, argv,"a:v")) != EOF)
  { switch(c)
    { case 'a' : dmsdb=optarg;
                 break;
      case 'v' : sprintf( argument, "%d", LOG_DBG );
                 dms_conf(S_LOG, argument);
                 break;
      case '?' : errflg++;
                 break;
    }
  }
  if(errflg)
  { printf("Usage: %s [-aDataBase][-v]\n",argv[0]);
    dms_exit(EDMS_FAIL);
  }
  stat = getbuf(&flstbuf,buffsize);
  if( stat != EDMS_OK )
  { printf("Error: E%04x getbuf error.\n",stat);
    dms_exit(EDMS_FAIL);
  }
  if(dmsdb != NULL)
  { if( (ptr=strrchr(dmsdb,'.')) != NULL ) *ptr='\0';
    stat = dms_flst(dmsdb,&flstbuf,&fileno,phyname);
    if(stat != EDMS_OK)
    { printf("Error: E%04x DB(%s) list file error.\n",stat,phyname);
      return(stat);
    }
    printf("\nDatabase: %s\n",phyname);
    printf("-----------------------------------------------------------\n");
    if(fileno != 0)
    { ptr = flstbuf.data;
      for(i=0; i<fileno; i++)
      { printf("%s\n",ptr); ptr+=strlen(ptr)+1; }
      total += fileno;
    }
    printf("LOCAL DATABASE has %d %s\n",fileno,(fileno>1?"dmsfiles":"dmsfile"));
    printf("-----------------------------------------------------------\n");
  }
  status=getdmspath(dmspath,"A");
  if(dmsdb == NULL && status == EDMS_OK)
  { if( (cc=strtok(dmspath,"#")) != NULL ) 
    do
    {
      dir=opendir(cc);
      while( (entry=readdir(dir)) != NULL )
      { if (entry->d_name[0] != '.')
        { sprintf(s,"%s/%s",cc,entry->d_name);
          if( (ptr=strrchr(s,'.')) == NULL ) continue; else *ptr='\0';
          stat = dms_flst(s,&flstbuf,&fileno,phyname);
          if(stat != EDMS_OK)
          { printf("Error: E%04x DB(%s) list file error.\n",stat,phyname);
            status = stat;
            continue;
          }
          printf("\nDatabase: %s\n",phyname);
          printf("---------------------------------------------------------\n");
          if(fileno != 0)
          { ptr = flstbuf.data;
            for(i=0; i<fileno; i++)
            { printf("%s\n",ptr); ptr+=strlen(ptr)+1; }
            total += fileno;
          }
          printf("LOCAL DataBase has %d %s\n",
                  fileno,(fileno>1?"dmsfiles":"dmsfile"));
          printf("---------------------------------------------------------\n");
        }
      }
      closedir(dir);
    } while( (cc=strtok(NULL,"#")) != NULL );
  }
 
  sprintf(s,"%s/%s",getenv("HOME"),DMSRC);
  if( access(s,F_OK) == 0 )
  { if( (fp=fopen(s,"r")) != NULL ) 
    { while ( fgets(s,MAX_DMS_STRL,fp) )
      { checkname(s);
        if( strncmp(s,NAME_SET,strlen(NAME_SET))==0 ) break;
      }
      while ( fgets(s,MAX_DMS_STRL,fp) )
      { checkname(s);
        if( s[0]=='\0' || s[0]=='#' ) continue; /* space line or remark line */
        if( s[0]=='[' ) break; /* next item header */
        if( lgc_no == 0 ) printf("\n"); 
        printf("%s\n",s);
        lgc_no++;
      }
      fclose(fp);
    }
  }
  if(lgc_no > 0)
  { printf("LOGICAL DataBase has %d %s\n",
            lgc_no,(lgc_no>1?"dmsfiles":"dmsfile"));
    printf("-----------------------------------------------------------\n");
  }
  total += lgc_no;

  for( i=0; env[i] != NULL; ++i )
  { if( (cc=strchr(env[i],'@')) != NULL )
    { if( strncmp(env[i],"DMSHOST",7) != 0 )
      { if( env_no == 0 ) printf("\n"); 
        printf("%s\n",env[i]);
      }
      env_no++;
    }
  }
  if(env_no > 0)
  { printf("ENVIRONMENT DataBase has %d %s\n",
            env_no,(env_no>1?"dmsfiles":"dmsfile"));
    printf("-----------------------------------------------------------\n");
  }
  total += env_no;

  printf("\n");
  printf("DataBase total have %d %s\n\n",total,(total>1?"dmsfiles":"dmsfile"));
  FREEDMST(flstbuf);
  dms_exit(EDMS_OK);
}

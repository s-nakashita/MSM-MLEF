/*---------------------------------------------------------------------------*\
 *                                                                           *
 *  Program file: dms_int_att.h                                              *
 *  Purpose: Header modules used to support C and Fortran language.          *
 *                                                                           *
\*---------------------------------------------------------------------------*/

#define MAXGLBATTBUF  128
#define ATTLEN         26
#define MAXATTLEN      34
/*---------------------------------------------------------------------------*/
/*+++++++++++++++++< Routines for C language interface >+++++++++++++++++++++*/
/*---------------------------------------------------------------------------*/
int cdmsinqatt(char *key,char *att,char *att_val,int *ratt_val_len,int *status);
int cdmsgetatt(char *dmsfile,char *key,int *status);
int cdmssetatt(char *key,char *att,char *att_val,int *status);
int cdmsputatt(char *dmsfile,char *key,int *status);
int cdmsclsatt(char *key,int *status);

#ifdef cray
#define fdmsgetatt    DMSGETATT
#define fdmsinqatt    DMSINQATT
#define fdmssetatt    DMSSETATT
#define fdmsputatt    DMSPUTATT
#define fdmsclsatt    DMSCLSATT
#elif hpux
#define fdmsgetatt    dmsgetatt
#define fdmsinqatt    dmsinqatt
#define fdmssetatt    dmssetatt
#define fdmsputatt    dmsputatt
#define fdmsclsatt    dmsclsatt
#elif AIX
#define fdmsgetatt    dmsgetatt
#define fdmsinqatt    dmsinqatt
#define fdmssetatt    dmssetatt
#define fdmsputatt    dmsputatt
#define fdmsclsatt    dmsclsatt
#else
#define fdmsgetatt    dmsgetatt_
#define fdmsinqatt    dmsinqatt_
#define fdmssetatt    dmssetatt_
#define fdmsputatt    dmsputatt_
#define fdmsclsatt    dmsclsatt_
#endif


/*---------------------------------------------------------------------------*/
int fdmsinqatt(char *key,char *att,char *att_val,int *ratt_val_len,int *status);
int fdmsgetatt(char *dmsfile,char *key,int *status);
int fdmssetatt(char *key,char *att,char *att_val,int *status);
int fdmsputatt(char *dmsfile,char *key,int *status);
int fdmsclsatt(char *key,int *status);
/*---------------------------------------------------------------------------*/


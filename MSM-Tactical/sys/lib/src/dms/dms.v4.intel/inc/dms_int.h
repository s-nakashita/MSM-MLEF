/*---------------------------------------------------------------------------*\
 *                                                                           *
 *  Program file: dms_int.h                                                  *
 *  Purpose: Header modules used to support C and Fortran language.          *
 *                                                                           *
\*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/*+++++++++++++++++< Routines for C language interface >+++++++++++++++++++++*/
/*---------------------------------------------------------------------------*/
int cdmscfg(char *type,char *argument,int *status);
int cdmsinit(void (*sig_handler)(),int *status);
int cdmsmsg(char *type,int *status);
int cdmsopn(char *dmsfile,char *mode,int *status);
int cdmschkf(char *dmsfile,int *status);
int cdmsinv(char *dmsfile,char *key,char *buf,int *num,int *status);
int cdmschkr(char *dmsfile,char *key,int *status);
int cdmsdlt(char *dmsfile,char *key,int *status);
int cdmsget(char *dmsfile,char *key,char *buf,int *status);
int cdmsput(char *dmsfile,char *key,char *buf,int *status);
int cdmscls(char *dmsfile,int *status);
int cdmsexit(int *status);

/*---------------------------------------------------------------------------*/
/*++++++++++++++< Routines for FORTRAN language interface >++++++++++++++++++*/
/*---------------------------------------------------------------------------*/

#ifdef cray
#define fdmscfg   DMSCFG
#define fdmsmsg    DMSMSG
#define fdmsopn    DMSOPN
#define fdmschkf   DMSCHKF
#define fdmschkr   DMSCHKR
#define fdmsinv    DMSINV
#define fdmscls    DMSCLS
#define fdmsdlt    DMSDLT
#define fdmsget    DMSGET
#define fdmsput    DMSPUT
#define fdmsexit   DMSEXIT
#define fdmsinit   DMSINIT
#elif hpux
#define fdmscfg    dmscfg
#define fdmsmsg    dmsmsg
#define fdmsopn    dmsopn
#define fdmschkf   dmschkf
#define fdmschkr   dmschkr
#define fdmsinv    dmsinv
#define fdmscls    dmscls
#define fdmsdlt    dmsdlt
#define fdmsget    dmsget
#define fdmsput    dmsput
#define fdmsexit   dmsexit
#define fdmsinit   dmsinit
#elif AIX
#define fdmscfg    dmscfg
#define fdmsmsg    dmsmsg
#define fdmsopn    dmsopn
#define fdmschkf   dmschkf
#define fdmschkr   dmschkr
#define fdmsinv    dmsinv
#define fdmscls    dmscls
#define fdmsdlt    dmsdlt
#define fdmsget    dmsget
#define fdmsput    dmsput
#define fdmsexit   dmsexit
#define fdmsinit   dmsinit
#else
#define fdmscfg   dmscfg_
#define fdmsmsg    dmsmsg_
#define fdmsopn    dmsopn_
#define fdmschkf   dmschkf_
#define fdmschkr   dmschkr_
#define fdmsinv    dmsinv_
#define fdmscls    dmscls_
#define fdmsdlt    dmsdlt_
#define fdmsget    dmsget_
#define fdmsput    dmsput_
#define fdmsexit   dmsexit_
#define fdmsinit   dmsinit_
#endif

/*---------------------------------------------------------------------------*/
int fdmscfg(char *type,char *argument,int *status);
int fdmsinit(void (*sig_handler)(),int *status);
int fdmsmsg(char *type,int *status);
int fdmsopn(char *dmsfile,char *mode,int *status);
int fdmschkf(char *dmsfile,int *status);
int fdmsinv(char *dmsfile,char *key,char *buf,int *len,int *status);
int fdmschkr(char *dmsfile,char *key,int *status);
int fdmsdlt(char *dmsfile,char *key,int *status);
int fdmsget(char *dmsfile,char *key,char *buff,int *status);
int fdmsput(char *dmsfile,char *key,char *buff,int *status);
int fdmscls(char *dmsfile,int *status);
int fdmsexit(int *status);
/*---------------------------------------------------------------------------*/


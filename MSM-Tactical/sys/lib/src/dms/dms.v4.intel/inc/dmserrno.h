/* $Header: dmserrno.h,v 3.00.00.0 99/05/25 14:25:55 smp Exp $ */

#ifndef _DMS_ERRNO_INCLUDED
#define _DMS_ERRNO_INCLUDED

#define EDMS_OK         0
#define EDMS_FAIL       -1

enum RDMS_RLVL_ERROR
{ EDMS_RLVL_MIN=0x1100,
  EDMS_KEYLEN,          /* 0x1101 Read/Write Key Length error        */
  EDMS_RECEXIST,        /* 0x1102 Record has existence               */
  EDMS_NOREC,           /* 0x1103 Record not exist                   */
  EDMS_RECLOCK,         /* 0x1104 Record is locked                   */
  EDMS_ROPERR,          /* 0x1105 Record open error                  */
  EDMS_RRDERR,          /* 0x1106 Record Read error                  */
  EDMS_RWRERR,          /* 0x1107 Record Write error                 */
  EDMS_RDLTERR,         /* 0x1108 Record delete error                */
  EDMS_RCRTERR,         /* 0x1109 Record (path/file) create error    */
  EDMS_RMAPERR,         /* 0x110a Record (path/file) translate error */
  EDMS_RLVL_MAX=0x1999
};

enum RDMS_FLVL_ERROR
{ EDMS_FLVL_MIN=0x2100,
  EDMS_FILEXIST,        /* 0x2101 File has existence         */
  EDMS_NOFILE,          /* 0x2102 File not exist             */
  EDMS_NOTOPEN,         /* 0x2103 File not open              */
  EDMS_FOPERR,          /* 0x2104 File open error            */
  EDMS_TMFILE,          /* 0x2105 Too many files open        */
  EDMS_CRTFILE,         /* 0x2106 Create DMS file error      */
  EDMS_NRWPERMIT,       /* 0x2107 No Read/Write Permission   */
  EDMS_FROGERR,         /* 0x2108 File reorganize error      */
  EDMS_OPEN,            /* 0x2109 File is open               */
  EDMS_FLVL_MAX
};

enum RDMS_DLVL_ERROR
{ EDMS_DLVL_MIN=0x3100,
  EDMS_DBEXIST,         /* 0x3101 DataBase has existence */
  EDMS_NODB,            /* 0x3102 DataBase not exist     */
  EDMS_CRTDB,           /* 0x3103 DataBase create error  */
  EDMS_NOTEMPTY,	/* 0x3104 DataBase not empty     */
  EDMS_DLVL_MAX
};

enum RDMS_OLVL_ERROR
{ EDMS_OLVL_MIN=0x4100,
  EDMS_BADARG,          /* 0x4101 Bad argument value            */
  EDMS_BADPLT,          /* 0x4102 Bad platform type             */
  EDMS_LOGFILE,         /* 0x4103 Can not access dms log file   */
  EDMS_OPENLOG,         /* 0x4104 Log file not open             */
  EDMS_CANCEL,          /* 0x4105 User Cancel for delete        */
  EDMS_NOMEM,           /* 0x4106 Can not get memory            */
  EDMS_DMPFILE,         /* 0x4107 Access Dump file error        */
  EDMS_DMPTYPE,         /* 0x4108 Dump file type error          */
  EDMS_BADFMT,          /* 0x4109 Dmsfile format error          */
  EDMS_FEWSIZE,         /* 0x410a Buffer size isn't enough      */
  EDMS_PATHSET,         /* 0x410b .dmspath access error         */
  EDMS_IO,              /* 0x410c Read/Write error              */
  EDMS_BADSET,          /* 0x410d Set DB or Dmsfile incorrect   */
  EDMS_CONNECT,         /* 0x410e network connection error      */
  EDMS_PROTOCOL,        /* 0x410f network protocol error        */
  EDMS_NAME,            /* 0x4110 db/file name syntex error     */
  EDMS_LIMIT,           /* 0x4111 resource limitation error     */
  EDMS_CONFIG,          /* 0x4112 Configuration error           */
  EDMS_OLVL_MAX
};

#endif /* _DMS_ERRNO_INCLUDED */


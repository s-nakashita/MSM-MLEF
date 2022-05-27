#ifndef _XDR_LIB_H_
#define _XDR_LIB_H_
/*---------------------------------------------------------------------*\
 *                                                                     *
 *  Program file: xdr_lib.h                                            *
 *  Purpose: Header file for xdr_lib.c                                 *
 *                                                                     *
\*---------------------------------------------------------------------*/

#include <rpc/types.h>
#include <rpc/xdr.h>

/*---------------------------------------------------------------------*/
#ifndef VPP5000

void
#ifdef __sgi
xdrmem_create(register XDR *xdrs,void * addr,u_int size,enum xdr_op op);
#else
xdrmem_create(register XDR *xdrs,caddr_t addr,u_int size,enum xdr_op op);
#endif

#endif
/*---------------------------------------------------------------------*/
int xdr_init(void);
int xdr_exit(void);
int xdr_fun(char *fun,char *key,char *buff,char *bbf_xdr,int nelem);
int xdr_trans(register XDR *io_xdrs,register char *key,register char *io_buf,
              register u_int nelem,register u_int elemsize);
int xdr_size(char *key);

#ifdef UNIT_TEST
int ieeesiz(char *tysize);
#endif
/*---------------------------------------------------------------------*/
/*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/
/*---------------------------------------------------------------------*/
#endif

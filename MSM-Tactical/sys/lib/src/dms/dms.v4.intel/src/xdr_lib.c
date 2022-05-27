/*---------------------------------------------------------------------------*\
 *                                                                           *
 * Program File: xdrlib.c                                                    *
 * Purpose     : This routine is used to transform different types under     *
 *               different platforms by using xdr.                           *
 *                                                                           *
 * If you have some data to be interpreted as external data representation   *
 * or to be converted to external data representation in a memory buffer,    *
 * then this is the package for you.                                         *
\*---------------------------------------------------------------------------*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
/**
#include <rpc/xdr.h>
#include <rpc/types.h>
**/
/*---------------------------------------------------------------------------*/
#include "dms.h"
#include "xdr_lib.h"
#include "key_lib.h"
/*---------------------------------------------------------------------------*/
#ifndef VPP5000

#ifdef hpux
#include <netinet/in.h>
#endif

#ifdef  __alpha

#define netlong int

#ifdef KERNEL
#include <sys/param.h>
#endif

#include <netinet/in.h>

#else

#define netlong long

#endif

static struct xdr_ops *xdrmem_ops();

/*---------------------------------------------------------------------------*/
/*+++++++++++++++++< XDR memory access routine set >+++++++++++++++++++++++++*/
/*---------------------------------------------------------------------------*/
/*
 * The procedure xdrmem_create initializes a stream descriptor for a
 * memory buffer.
 */
/*---------------------------------------------------------------------------*/
void
#ifdef __sgi
xdrmem_create(register XDR *xdrs,void * addr,u_int size,enum xdr_op op)
#else
xdrmem_create(register XDR *xdrs,caddr_t addr,u_int size,enum xdr_op op)
#endif
/*---------------------------------------------------------------------------*/
{
  xdrs->x_op = op;
  xdrs->x_ops = xdrmem_ops();
  xdrs->x_private = xdrs->x_base = addr;
#ifdef  CRAY
  /* Cray -- ensure alignment to half-word boundary */
  if(((addr - (caddr_t)(word64 *)addr) & (BYTES_PER_XDR_UNIT - 1)) != 0) {
#else
  /* Byte-addressible machine -- ensure alignment to int size */
  if(((long)addr & (sizeof(int) - 1)) != 0) {
#endif
    (void) fprintf(stderr, "RPC error: xdrmem_create with non-aligned buffer (%lx)\n", addr);
    abort();
  }
  xdrs->x_handy = size;
}

/*---------------------------------------------------------------------------*/
static void xdrmem_destroy(register XDR *xdrs) { }
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
static bool_t xdrmem_getlong(register XDR *xdrs, netlong *lp)
/*---------------------------------------------------------------------------*/
{
#ifdef  CRAY
  register u_char *bp;

  if ((xdrs->x_handy -= BYTES_PER_XDR_UNIT) < 0) return (FALSE);
  bp = xdrs->x_private;
  xdrs->x_private = bp + BYTES_PER_XDR_UNIT;
  if(bp == (u_char *)(word64 *)bp)
  { *lp = ((word64 *)bp)->upper; }
  else
  { *lp = ((word64 *)bp)->lower; }
#else
  if ((xdrs->x_handy -= BYTES_PER_XDR_UNIT) < 0) return (FALSE);
#ifdef HPIA64
  *lp = ntohl((netlong)(*((int32_t *)(xdrs->x_private))));
#elif AIX5
  *lp = ntohl((netlong)(*((int32_t *)(xdrs->x_private))));
#else
  *lp = ntohl(*((netlong *)(xdrs->x_private)));
#endif
  xdrs->x_private += BYTES_PER_XDR_UNIT;
#endif
  return (TRUE);
}

/*---------------------------------------------------------------------------*/
#if defined(LINUX) || defined(LXSGI)
static bool_t xdrmem_putlong(register XDR *xdrs, __const netlong *lp)
#else
static bool_t xdrmem_putlong(register XDR *xdrs, netlong *lp)
#endif
/*---------------------------------------------------------------------------*/
{
#ifdef  CRAY
  register caddr_t bp;

  if((xdrs->x_handy -= BYTES_PER_XDR_UNIT) < 0) return(FALSE);
  bp = (u_char *) xdrs->x_private;
  xdrs->x_private = bp + BYTES_PER_XDR_UNIT;
  if(bp == (caddr_t)(word64 *)bp)
  { ((word64 *)bp)->upper = *lp; }
  else
  { ((word64 *)bp)->lower = *lp; }
#else
  if ((xdrs->x_handy -= BYTES_PER_XDR_UNIT) < 0) return (FALSE);
#ifdef HPIA64
  *(int32_t *)xdrs->x_private = (int32_t) htonl((int32_t)(*lp));
#elif AIX5
  *(int32_t *)xdrs->x_private = (int32_t) htonl((int32_t)(*lp));
#else
  *(netlong *)xdrs->x_private = htonl(*lp);
#endif
  xdrs->x_private += BYTES_PER_XDR_UNIT;
#endif
  return (TRUE);
}

/*---------------------------------------------------------------------------*/
#ifdef __sgi
static bool_t xdrmem_getbytes(struct __xdr_s *xdrs,void *addr,u_int len)
#elif defined(LINUX) || defined(LXSGI)
static bool_t xdrmem_getbytes(register XDR *xdrs,caddr_t addr,register u_int len)
#else
static bool_t xdrmem_getbytes(register XDR *xdrs,caddr_t addr,register int len)
#endif
/*---------------------------------------------------------------------------*/
{
  if ((xdrs->x_handy -= len) < 0) return (FALSE);
  bcopy(xdrs->x_private, addr, len);
  xdrs->x_private += len;
  return (TRUE);
}

/*---------------------------------------------------------------------------*/
#ifdef __sgi
static bool_t xdrmem_putbytes(struct __xdr_s *xdrs,void *addr,u_int len)
#elif defined(LINUX) || defined(LXSGI)
static bool_t xdrmem_putbytes(register XDR *xdrs,__const char *addr,register u_int len)
#else
static bool_t xdrmem_putbytes(register XDR *xdrs,caddr_t addr,register int len)
#endif
/*---------------------------------------------------------------------------*/
{
  if ((xdrs->x_handy -= len) < 0) return (FALSE);
  bcopy(addr, xdrs->x_private, len);
  xdrs->x_private += len;
  return (TRUE);
}

/*---------------------------------------------------------------------------*/
#if defined(LINUX) || defined(LXSGI)
static u_int xdrmem_getpos(register __const XDR *xdrs)
#else
static u_int xdrmem_getpos(register XDR *xdrs)
#endif
/*---------------------------------------------------------------------------*/
{
#ifdef CRAY
  return((u_int)(xdrs->x_private - xdrs->x_base));
#else
  return((u_int)xdrs->x_private - (u_int)xdrs->x_base);
#endif
}

/*---------------------------------------------------------------------------*/
static bool_t xdrmem_setpos(register XDR *xdrs, u_int pos)
/*---------------------------------------------------------------------------*/
{
  register caddr_t newaddr = xdrs->x_base + pos;
  register caddr_t lastaddr = xdrs->x_private + xdrs->x_handy;
#ifdef CRAY
  if (newaddr > lastaddr)
#else
  if ((long)newaddr > (long)lastaddr)
#endif
    return (FALSE);
  xdrs->x_private = newaddr;
#ifdef CRAY
  xdrs->x_handy = (int)(lastaddr - newaddr);
#else
  xdrs->x_handy = (int)lastaddr - (int)newaddr;
#endif
  return (TRUE);
}

/*---------------------------------------------------------------------------*/
#ifdef CRAY
static inline_t *
#elif defined(LINUX) || defined(LXSGI)
static int32_t *
#else
static netlong *
#endif
xdrmem_inline(register XDR *xdrs, int len)
/*---------------------------------------------------------------------------*/
{
#ifdef CRAY
  inline_t *buf = 0;
#elif defined(LINUX) || defined(LXSGI)
  int32_t *buf=0;
#else
  netlong *buf = 0;
#endif
  if (xdrs->x_handy >= len)
  { xdrs->x_handy -= len;
#ifdef CRAY
    buf = (inline_t *) xdrs->x_private;
#elif defined(LINUX) || defined(LXSGI)
    buf = (int32_t *) xdrs->x_private;
#else
    buf = (netlong *) xdrs->x_private;
#endif
    xdrs->x_private += len;
  }
  return (buf);
}

/*---------------------------------------------------------------------------*/
static struct xdr_ops *xdrmem_ops()
/*---------------------------------------------------------------------------*/
{ static struct xdr_ops ops;

  if (ops.x_getlong == NULL)
  { ops.x_getlong = xdrmem_getlong;
    ops.x_putlong = xdrmem_putlong;
    ops.x_getbytes = xdrmem_getbytes;
    ops.x_putbytes = xdrmem_putbytes;
    ops.x_getpostn = xdrmem_getpos;
    ops.x_setpostn = xdrmem_setpos;
    ops.x_inline = xdrmem_inline;
    ops.x_destroy = xdrmem_destroy;
  }
  return (&ops);
}
#endif

/*---------------------------------------------------------------------------*/
/*+++++++++++++++++< XDR type conversion routine set >+++++++++++++++++++++++*/
/*---------------------------------------------------------------------------*/
#define  SIZEOF_XDR_DATA  8
#define  CNMAX  nelem*SIZEOF_XDR_DATA
static   XDR  *encode_xdrs,*decode_xdrs;
static   int  xdr_inited=0;
char     xdr_operation;

/*---------------------------------------------------------------------------*/
int xdr_init(void)
/*---------------------------------------------------------------------------*/
{
  if( xdr_inited != 0 ) return(0);

  if( (encode_xdrs=(XDR *)malloc(sizeof(XDR))) == (XDR *)NULL )
  { printf("Encode XDR memory allocation error ...\n"); return(-1); }
  /* memset( (void *)encode_xdrs, 0, sizeof(XDR) ); */

  if( (decode_xdrs=(XDR *)malloc(sizeof(XDR))) == (XDR *)NULL ) 
  { printf("Decode XDR memory allocation error ...\n"); return(-1); }
  /* memset( (void *)decode_xdrs, 0, sizeof(XDR) ); */

  xdr_inited = 1 ;
  return(0);
}

/*---------------------------------------------------------------------------*/
int xdr_exit(void)
/*---------------------------------------------------------------------------*/
{
  if( encode_xdrs != NULL ) { free(encode_xdrs); encode_xdrs = NULL ; }
  if( decode_xdrs != NULL ) { free(decode_xdrs); decode_xdrs = NULL ; }
  xdr_inited = 0 ;
  return(0);
}

/*---------------------------------------------------------------------------*/
int  xdr_fun(char *fun,char *key,char *iobuf,char *xdrbuf,int nelem)
/*---------------------------------------------------------------------------*/
{ XDR *xdrs;
  int status=0;

  if( xdr_inited == 0 ) { if( xdr_init() < 0 ) return(-1); }

  key_use( strlen(key) );

  xdr_operation = toupper(*fun);
  if( xdr_operation == 'E' )
  {
    memset( (void *)encode_xdrs, 0, sizeof(XDR) );
    xdrmem_create(encode_xdrs, xdrbuf, CNMAX, XDR_ENCODE);
    status = xdr_trans(encode_xdrs, key, iobuf, nelem, key_size(key));
  }
  if( xdr_operation == 'D' )
  {
    memset( (void *)decode_xdrs, 0, sizeof(XDR) );
    xdrmem_create(decode_xdrs, xdrbuf, CNMAX, XDR_DECODE);
    status = xdr_trans(decode_xdrs, key, iobuf, nelem, key_size(key));
  }

  if( xdr_inited == 1 ) xdr_exit();

  return(status);
}

#ifdef VPP5000
/*---------------------------------------------------------------------*/
int xdr_trans(XDR *io_xdrs,char *dmskey,char *io_buf,u_int nelem,u_int selem)
/*---------------------------------------------------------------------*/
{ u_int status,i,j;
  char   *elptr,*bufptr,type;
  int   start,length;

  elptr = io_buf ;
  key_use( strlen(dmskey) );
  key_type_def(&start,&length);
  type = toupper( dmskey[start] );

  switch( type )
  {
#ifdef Full_Precision
    case 'H' :
      { int    fltsize;
        float  *fltbuf;
        double *dblbuf;

        fltsize = sizeof(float);
        dblbuf = (double *)io_buf ;
        fltbuf = (float *)malloc( fltsize*nelem ) ;
        if( xdr_operation == 'E' )
        {
          for (i = 0; i < nelem; i++) fltbuf[i] = dblbuf[i];
          status = xdr_vector(io_xdrs,(char *)fltbuf,nelem,fltsize,xdr_float);
        }
        else
        {
          status = xdr_vector(io_xdrs,(char *)fltbuf,nelem,fltsize,xdr_float);
          for (i = 0; i < nelem; i++) dblbuf[i] = fltbuf[i];
        }
        free(fltbuf);
      }
      break;
#else
    case 'H' : status = xdr_vector(io_xdrs,io_buf,nelem,selem,xdr_float );
               /* Can't do patch for DEC/alpha floating normalization problem */               break;
#endif
    case 'R' : status = xdr_vector(io_xdrs,io_buf,nelem,selem,xdr_float );
               /* Can't do patch for DEC/alpha floating normalization problem */               break;
    case 'F' : status = xdr_vector(io_xdrs,io_buf,nelem,selem,xdr_double);
               break;
    case 'L' : status = xdr_vector(io_xdrs,io_buf,nelem,selem,xdr_hyper );
               break;
    case 'I' : status = xdr_vector(io_xdrs,io_buf,nelem,selem,xdr_int   );
               break;
    case 'S' : status = xdr_vector(io_xdrs,io_buf,nelem,selem,xdr_short );
               break;
    case 'C' :
    case 'B' : case 'D' : case 'Q' : case 'O' :
    case 'X' :
               bufptr = io_xdrs->x_base ;
               if( xdr_operation == 'E' )
                 for (i = 0; i < nelem; i++)
                 { for(j=0; j < selem ; j++ ) *bufptr++ = *elptr++ ; }
               else
                 for (i = 0; i < nelem; i++)
                 { for(j=0; j < selem ; j++ ) *elptr++ = *bufptr++ ; }
               break;
  }
  return(0);
}
#else
/*---------------------------------------------------------------------------*/
int xdr_trans(XDR  *io_xdrs,char *key,char *io_buf,u_int nelem,u_int selem)
/*---------------------------------------------------------------------------*/
{ register u_int i,j;
  register char *elptr,*bufptr,type;
  int      start,length,status;

  elptr = io_buf ;
  key_use( strlen(key) );
  key_type_def(&start,&length);
  type = toupper( key[start] );

  switch( type )
  {
    case 'H' :
#ifdef Full_Precision
               if( xdr_operation == 'E' )
                 for (i = 0; i < nelem; i++)
                 { float eltmp=*((double *)elptr);
                   xdr_float(io_xdrs, &eltmp); elptr += selem;
                 }
               else
                 for (i = 0; i < nelem; i++)
                 { float eltmp;
                   status = xdr_float(io_xdrs, &eltmp) ; 
                   *((double *)elptr) = eltmp ; elptr += selem;
                 }
#else
               for (i = 0; i < nelem; i++)
               {
#ifdef __alpha
                 if ( xdr_operation=='D' && (*(io_xdrs->x_private)&0x7F)==0 )
                   memset( (io_xdrs->x_private)+1, 0x00, 3);
#endif
                 xdr_float(io_xdrs, (float *)elptr);
                 elptr += selem;
               }
#endif
               break;
    case 'R' :
               for (i = 0; i < nelem; i++)
               {
#ifdef __alpha
                 if ( xdr_operation=='D' && (*(io_xdrs->x_private)&0x7F)==0 )
                   memset( (io_xdrs->x_private)+1, 0x00, 3);
#endif
                 xdr_float(io_xdrs, (float *)elptr);
                 elptr += selem;
               }
               break;
    case 'F' : for (i = 0; i < nelem; i++)
               { xdr_double(io_xdrs,(double *)elptr); elptr += selem; }
               break;
    case 'L' : for (i = 0; i < nelem; i++)
               { xdr_long(io_xdrs,(long *)elptr); elptr += selem; }
               break;
    case 'I' : for (i = 0; i < nelem; i++)
               { xdr_int(io_xdrs,(int *)elptr); elptr += selem; }
               break;
    case 'S' : for (i = 0; i < nelem; i++)
               { xdr_short(io_xdrs,(short *)elptr); elptr += selem; }
               break;
    case 'C' : for (i = 0; i < nelem; i++)
               { xdr_char(io_xdrs,(char *)elptr); elptr += selem; }
               break;
    case 'B' : case 'D' : case 'Q' : case 'O' :
    case 'X' :
               bufptr = io_xdrs->x_base ;
               if( xdr_operation == 'E' )
                 for (i = 0; i < nelem; i++)
                 { for(j=0; j < selem ; j++ ) *bufptr++ = *elptr++ ; }
               else
                 for (i = 0; i < nelem; i++)
                 { for(j=0; j < selem ; j++ ) *elptr++ = *bufptr++ ; }
               break;

  }
  return(0);
}
#endif

/*---------------------------------------------------------------------------*/
int xdr_size(char *key)
/*---------------------------------------------------------------------------*/
{ int  start,length;
  char type;

  key_use( strlen(key) );
  key_type_def(&start,&length);
  type = toupper( key[start] );
  switch( type )
  { /* --- floating number */
    case 'F' : return(  8 );
    case 'H' : return(  4 );
    case 'R' : return(  4 );
    /* --- integer number --- */
    case 'L' : return(  8 );
    case 'I' : return(  4 );
    case 'S' : return(  2 );
    /* --- character stream --- */
/*  case 'C' : return(  1 ); */
    /* --- binary stream(BLOB) --- no convertion performed --- */
    case 'C' : return(  0 );
    case 'B' : return(  0 );
    case 'D' : return(  0 );
    case 'Q' : return(  0 );
    case 'O' : return(  0 );
    case 'X' : return(  0 );
    /* --- wrong type --- */
    default  : return( -1 );
  }
}

/* -------------------------------------------------------------------- */
/* ****************< Unit test suit goes below >*********************** */
/* -------------------------------------------------------------------- */
#ifdef  UNIT_TEST
/* -------------------------------------------------------------------- */
int ieeesiz(char *dmskey)
/* -------------------------------------------------------------------- */
{ char type;
  int  start,length;

  key_use( strlen(dmskey) );
  key_type_def(&start,&length);
  type = toupper( dmskey[start] );

  switch( type )
  {
      case 'F' : return(  8 );
      case 'H' : return(  4 );
      case 'R' : return(  4 );
      case 'L' : return(  8 );
      case 'I' : return(  4 );
      case 'S' : return(  2 );
      case 'C' : return(  1 );
      case 'B' : return(  1 );
      case 'D' : return(  2 );
      case 'Q' : return(  4 );
      case 'O' : return(  8 );
      case 'X' : return( 16 );
      default  : return(  0 );
  }
}
#endif
/* -------------------------------------------------------------------- */
/* ****************< Unit test suit goes above >*********************** */
/* -------------------------------------------------------------------- */

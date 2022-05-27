/*---------------------------------------------------------------------------*\
 *                                                                           *
 *  Program file: key_lib.c                                                  *
 *  Purpose: This program is a library including some common routines used   *
 *           by DMS KEY process.                                             *
 *                                                                           *
\*---------------------------------------------------------------------------*/
#include <ctype.h>
#include <stdio.h>
#include <string.h>
/*---------------------------------------------------------------------------*/
#include "dms.h"
#include "dmserrno.h"
#include "key_lib.h"
/*---------------------------------------------------------------------------*/

int keycnt=KEYCNT;
int keyuse=0;

MAPLIST *maplist;
extern DMSMAP dmsmap[KEYCNT];
extern DMSCONF dmsconfig;

DMS_KEY keydef[] =
{
  { /* 34 key */
    34,
    0,3,6,10,14,26,27,
    3,3,4,4,12,1,7,
    2,
    "2222221111222211111111111122222222",
    "3333332222333311111111111133333333"
  },
  { /* 24 key */
    24,
    0,1,3,6,8,16,17,
    1,2,3,2,8,1,7,
    2,
    "222111221111111122222222",
    "333222331111111133333333"
  },
  { /* 32 key */ 
    32,
    0,2,4,8,12,24,25,
    2,2,4,4,12,1,7,
    2,
    "22221111222211111111111122222222",
    "33332222333311111111111133333333"
  },
};

/*---------------------------------------------------------------------------*/
/*++++++++++++++++++< Routines for external reference >++++++++++++++++++++++*/
/*---------------------------------------------------------------------------*/
int key_use(int keylen)
/*---------------------------------------------------------------------------*/
{ int i;

  for( i=0 ; i < KEYCNT ; i++ )
  { if ( keylen == keydef[i].keylen ) { keyuse = i; return( i ); }  }
  return( EDMS_FAIL );
}

/*---------------------------------------------------------------------------*/
void key_height(char *key,char *height)
/*---------------------------------------------------------------------------*/
{ int itmp,ntmp;
  
  memset(height, 0x20, keydef[keyuse].keylen);
  itmp = keydef[keyuse].height;
  ntmp = keydef[keyuse].height_len;
  memcpy(height, &key[itmp], ntmp);
  height[ntmp] = '\0';
}

/*---------------------------------------------------------------------------*/
void key_height_def(int *start,int *length)
/*---------------------------------------------------------------------------*/
{
  *start  = keydef[keyuse].height;
  *length = keydef[keyuse].height_len;
}

/*---------------------------------------------------------------------------*/
void key_field(char *key,char *field)
/*---------------------------------------------------------------------------*/
{ int itmp,ntmp;

  memset(field, 0x20, keydef[keyuse].keylen);
  itmp = keydef[keyuse].field;
  ntmp = keydef[keyuse].field_len;
  memcpy(field, &key[itmp], ntmp);
  field[ntmp] = '\0';
}

/*---------------------------------------------------------------------------*/
void key_field_def(int *start,int *length)
/*---------------------------------------------------------------------------*/
{
  *start  = keydef[keyuse].field;
  *length = keydef[keyuse].field_len;
}

/*---------------------------------------------------------------------------*/
void key_tau(char *key,char *tau)
/*---------------------------------------------------------------------------*/
{ int itmp,ntmp;

  memset(tau, 0x20, keydef[keyuse].keylen);
  itmp = keydef[keyuse].tau;
  ntmp = keydef[keyuse].tau_len;
  memcpy(tau, &key[itmp], ntmp);
  tau[ntmp] = '\0';
}

/*---------------------------------------------------------------------------*/
void key_tau_def(int *start,int *length)
/*---------------------------------------------------------------------------*/
{
  *start  = keydef[keyuse].tau;
  *length = keydef[keyuse].tau_len;
}

/*---------------------------------------------------------------------------*/
void key_flap(char *key,char *flap)
/*---------------------------------------------------------------------------*/
{ int itmp,ntmp;

  memset(flap, 0x20, keydef[keyuse].keylen);
  itmp = keydef[keyuse].flap;
  ntmp = keydef[keyuse].flap_len;
  memcpy(flap, &key[itmp], ntmp);
  flap[ntmp] = '\0';
}

/*---------------------------------------------------------------------------*/
void key_flap_def(int *start,int *length)
/*---------------------------------------------------------------------------*/
{
  *start  = keydef[keyuse].flap;
  *length = keydef[keyuse].flap_len;
}

/*---------------------------------------------------------------------------*/
void key_time(char *key,char *time)
/*---------------------------------------------------------------------------*/
{ int itmp,ntmp;

  memset(time, 0x20, keydef[keyuse].keylen);
  itmp = keydef[keyuse].time;
  ntmp = keydef[keyuse].time_len;
  memcpy(time, &key[itmp], ntmp);
  time[ntmp] = '\0';
}

/*---------------------------------------------------------------------------*/
void key_time_def(int *start,int *length)
/*---------------------------------------------------------------------------*/
{
  *start  = keydef[keyuse].time;
  *length = keydef[keyuse].time_len;
}

/*---------------------------------------------------------------------------*/
void key_utc(char *key,char *utc)
/*---------------------------------------------------------------------------*/
{ int itmp,ntmp,offset;

  memset(utc, 0x20, keydef[keyuse].keylen);
  offset = (keydef[keyuse].keylen == 24 ? 6 : 8 );
  itmp = keydef[keyuse].time + offset;
  ntmp = (keydef[keyuse].keylen == 24 ? 2 : 4 );
  memcpy(utc, &key[itmp], ntmp);
  utc[ntmp] = '\0';
}

/*---------------------------------------------------------------------------*/
void key_utc_def(int *start,int *length)
/*---------------------------------------------------------------------------*/
{ int offset;

  offset = (keydef[keyuse].keylen == 24 ? 6 : 8 );
  *start  = keydef[keyuse].time + offset;
  *length = (keydef[keyuse].keylen == 24 ? 2 : 4 );
}

/*---------------------------------------------------------------------------*/
void key_type(char *key,char *type)
/*---------------------------------------------------------------------------*/
{ int itmp,ntmp;

  memset(type, 0x20, keydef[keyuse].keylen);
  itmp = keydef[keyuse].type;
  ntmp = keydef[keyuse].type_len;
  memcpy(type, &key[itmp], ntmp);
  type[ntmp] = '\0';
}

/*---------------------------------------------------------------------------*/
void key_type_def(int *start,int *length)
/*---------------------------------------------------------------------------*/
{
  *start  = keydef[keyuse].type;
  *length = keydef[keyuse].type_len;
}

/*---------------------------------------------------------------------------*/
void key_item(char *key,char *item)
/*---------------------------------------------------------------------------*/
{ int itmp,ntmp;

  memset(item, 0x20, keydef[keyuse].keylen);
  itmp = keydef[keyuse].size;
  ntmp = keydef[keyuse].size_len;
  memcpy(item, &key[itmp], ntmp);
  item[ntmp] = '\0';
}

/*---------------------------------------------------------------------------*/
void key_item_def(int *start,int *length)
/*---------------------------------------------------------------------------*/
{
  *start  = keydef[keyuse].size;
  *length = keydef[keyuse].size_len;
}

/*---------------------------------------------------------------------------*/
int key_nitm(char *key)
/*---------------------------------------------------------------------------*/
{ char temp[MAXKEYL];

  key_item(key,temp);
  return(atoi(temp));
}

/*---------------------------------------------------------------------------*/
/*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/
/*---------------------------------------------------------------------------*/
int key_size(char *key)
/*---------------------------------------------------------------------------*/
{ int start,length;

  key_type_def(&start,&length);
  switch( toupper(key[start]) )
  { /* --- floating number */
    case 'F' : return( sizeof(double) );
#ifdef Full_Precision
    case 'H' : return( sizeof(double) );
#else
    case 'H' : return( sizeof(float) );
#endif
    case 'R' : return( sizeof(float) );
    /* --- integer number --- */
    case 'L' : return( sizeof(long)  );
    case 'I' : return( sizeof(int)   );
    case 'S' : return( sizeof(short) );
    /* --- character stream --- */
/* case 'C' : return( sizeof(char) ); */
    /* --- binary stream(BLOB) --- */
    case 'C' : return(  1 );
    case 'B' : return(  1 );
    case 'D' : return(  2 );
    case 'Q' : return(  4 );
    case 'O' : return(  8 );
    case 'X' : return( 16 );
    /* --- wrong type --- */
    default  : return( -1 );
  }
}

/*---------------------------------------------------------------------------*/
/*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/
/*---------------------------------------------------------------------------*/
int key_compare(char *k1,char *k2,int len)
/*---------------------------------------------------------------------------*/
{ int i;

  if( strlen(k1) != strlen(k2) ) return(1);
  for( i=0 ; i < len ; i++ )
  { if( (k2[i] != '?') && (k1[i] != k2[i]) ) return(1); }
  return(0);
}

/*---------------------------------------------------------------------------*/
int key_wildcard(char *keyset)
/*---------------------------------------------------------------------------*/
{ int i;

  for( i=0 ; i < strlen(keyset) ; i++ )
  { if( (keyset[i] != '?') || (keyset[i] == '*') ) return(1); }
  return(0);
}

/*---------------------------------------------------------------------------*/
char *key_expend(char *keyin,int klen)
/*---------------------------------------------------------------------------*/
{ static char keyout[MAXKEYL];
  int    inklen,i,j,s,sidx,slen;

  inklen = strlen(keyin) ;
  for( sidx=0 ; sidx<inklen  &&  keyin[sidx]!='*' ; sidx++ );

  slen = 0 ;
  if( (s=sidx) < inklen ) slen = klen - ( inklen - 1 ) ;
  for( i=0 ; i < sidx ; i++ ) keyout[i] = keyin[i] ;
  for( j=0 ; j < slen ; j++ ) keyout[i++] = '?' ;
  for( j=s ; i < klen ; i++ ) keyout[i] = keyin[++j] ;
  keyout[klen] = '\0' ;
  uppercase(keyout);
  return(keyout);
}

/*---------------------------------------------------------------------------*/
char *key_trans(char *keyin,char *keysrc,char *keytns)
/*---------------------------------------------------------------------------*/
{ static char keyout[MAXKEYL];
  int    inklen,outklen,i,j;

  i = j = 0;
  inklen = strlen(keyin);
  outklen = strlen(keytns);
  strcpy(keyout, keytns);

  do
  {
    while( keysrc[i] != '?' && i < inklen ) i++;
    while( keytns[j] != '?' && j < outklen ) j++;
    keyout[j] = keyin[i];
    i++;
    j++;
  } while( i < inklen );

  keyout[outklen] = '\0';
  uppercase(keyout);
  return(keyout);
}

/*---------------------------------------------------------------------------*/
int key_translate(char *keyin,int outklen, char *keyout)
/*---------------------------------------------------------------------------*/
{ static  int  inklen_s=0, outklen_s=0,order,mapuse;
  char    *keymap1, *keymap2;
  int     status,inklen,sflag,i,j,k;

  inklen = strlen(keyin);

  if( inklen != inklen_s || outklen != outklen_s )
  {
    for( i = 0 ; i < dmsconfig.mapcnt ; i++ )
    { 
      if( inklen == dmsmap[i].keymaplen1 && outklen == dmsmap[i].keymaplen2 ) 
      { mapuse = i; order = 1; break; }
      if( inklen == dmsmap[i].keymaplen2 && outklen == dmsmap[i].keymaplen1 ) 
      { mapuse = i; order = 2; break; }
    }
    inklen_s = inklen;
    outklen_s = outklen;
    /* if( i == KEYCNT ) return( EDMS_FAIL ); */
    if( i == dmsconfig.mapcnt ) return( EDMS_FAIL );
  }

  if( dmsmap[mapuse].mapline == -1 )
  {
    /*status = read_maplist(dmsconfig.mapfile[mapuse],mapuse,inklen,outklen);*/
    status = read_maplist(dmsconfig.mapfile[mapuse],mapuse,
                          dmsmap[i].keymaplen1,dmsmap[i].keymaplen2);
    if( status != EDMS_OK ) return( status );
  }

  for( k = 0 ; k < dmsmap[mapuse].mapline ; k++ )
  {
    maplist = dmsmap[mapuse].maplist;
    if( order == 1 )
    {
      keymap1 = dmsmap[mapuse].maplist[k].keymap1;
      keymap2 = dmsmap[mapuse].maplist[k].keymap2;
    }
    else
    {
      keymap1 = dmsmap[mapuse].maplist[k].keymap2;
      keymap2 = dmsmap[mapuse].maplist[k].keymap1;
    }
    sflag = key_compare(keyin,keymap1,inklen);
    if( sflag == 0 )
    { 
      strcpy(keyout, keymap2);
      i = j = 0;
      do
      {
        while( keymap1[i] != '?' && i < inklen ) i++;
        while( keymap2[j] != '?' && j < outklen ) j++;
        keyout[j++] = keyin[i++];
      } while( i < inklen );
      keyout[outklen] = '\0';
      uppercase(keyout);
      return( EDMS_OK );
    }
  }

  return( EDMS_RMAPERR );
}

/*---------------------------------------------------------------------------*/
/*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/
/*---------------------------------------------------------------------------*/

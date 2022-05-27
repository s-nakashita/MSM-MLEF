#ifndef _KEY_LIB_H_
#define _KEY_LIB_H_
/*---------------------------------------------------------------------------*\
 *                                                                           *
 *  Program file: key_lib.h                                                  *
 *  Purpose: This program is a library including some common                 *
 *           routines used by DMS KEY process.                               *
 *                                                                           *
\*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/*++++++++++++++++++< Routines for external reference >++++++++++++++++++++++*/
/*---------------------------------------------------------------------------*/
int  key_use(int keylen);
void key_height(char *key,char *height);
void key_height_def(int *start,int *length);
void key_field(char *key,char *field);
void key_field_def(int *start,int *length);
void key_tau(char *key,char *tau);
void key_tau_def(int *start,int *length);
void key_flap(char *key,char *flap);
void key_flap_def(int *start,int *length);
void key_time(char *key,char *time);
void key_time_def(int *start,int *length);
void key_utc(char *key,char *utc);
void key_utc_def(int *start,int *length);
void key_type(char *key,char *type);
void key_type_def(int *start,int *length);
void key_item(char *key,char *item);
void key_item_def(int *start,int *length);
int  key_nitm(char *key);
int  key_size(char *key);
/*---------------------------------------------------------------------------*/
int  key_compare(char *k1,char *k2,int len);
int  key_wildcard(char *keyset);
char *key_expend(char *keyin,int klen);
char *key_trans(char *keyin,char *keysrc,char *keytns);
int  key_translate(char *keyin,int keylen,char *keyout);
/*---------------------------------------------------------------------------*/
/*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/
/*---------------------------------------------------------------------------*/
#endif

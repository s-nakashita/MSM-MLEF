/* @(#)$Header$ */
/*
***************************************************************************
*   System/Sub-system: NICE/DM/GRID
*
*   File Name : NdmGetDmsKey.h
*
*   Descriptions : define error number's values, program prototype.
*
*   History Section :
*      09/13/1996  YV.Lee  Original 
*
*********************************************************************
*/
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <varargs.h>

/* extern int NdmGetDmsKey( long va_alist ); */
/* extern int NdmGetDmsKey( va_dcl va_alist ); */

/*
*  OK              : ok
*  INVALID_ARG     : invalid argument
*  INVALID_VALUE   : invalid value
*  NICE_NOT_DECLARD: NICE environment is not declared.
*  ERROR_OPEN_FILE : file can't open
*  ERROR_CHDIR     : can't change directory
*  ERROR_PARSE     : error in parse file spec
*  ERROR_WRITE_ARC : error in writing archive
*  ERROR_RCP       : error in remote copying
*/
#define OK			0
#define INVALID_ARG		-1
#define INVALID_VALUE		-2
#define NICE_NOT_DECLARD 	-3
#define ERROR_OPEN_FILE 	-4
#define ERROR_CHDIR		-5
#define ERROR_PARSE     	-6
#define ERROR_WRITE_ARC 	-7
#define ERROR_RCP		-8
#define ERROR_REMOVE		-9
#define ERROR_DOWNLOAD	       -10
#define LABEL_NOT_FOUND	       -11

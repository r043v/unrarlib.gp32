/* ***************************************************************************
 **
 **  This file is part of the UniquE RAR File Library.
 **
 **  Copyright (C) 2000-2002 by Christian Scheurer (www.ChristianScheurer.ch)
 **  UNIX port copyright (c) 2000-2002 by Johannes Winkelmann (jw@tks6.net)
 **  GP32 port copyright (c) 2004 by noferi Mickaël aka -rov (http://procvor.free.fr)
 **
 **  The contents of this file are subject to the UniquE RAR File Library
 **  License (the "unrarlib-license.txt"). You may not use this file except
 **  in compliance with the License. You may obtain a copy of the License
 **  at http://www.unrarlib.org/license.html.
 **  Software distributed under the License is distributed on an "AS IS"
 **  basis, WITHOUT WARRANTY OF ANY KIND, either express or implied warranty.
 **
 **  Alternatively, the contents of this file may be used under the terms
 **  of the GNU General Public License Version 2 or later (the "GPL"), in
 **  which case the provisions of the GPL are applicable instead of those
 **  above. If you wish to allow use of your version of this file only
 **  under the terms of the GPL and not to allow others to use your version
 **  of this file under the terms of the UniquE RAR File Library License,
 **  indicate your decision by deleting the provisions above and replace
 **  them with the notice and other provisions required by the GPL. If you
 **  do not delete the provisions above, a recipient may use your version
 **  of this file under the terms of the GPL or the UniquE RAR File Library
 **  License.
 **
 ************************************************************************** */

/* include file for the "UniquE RAR File Library"  */
/* (C) 2000-2002 by Christian Scheurer aka. UniquE */
/* multi-OS version (Win32, Linux and SUN)         */

#ifndef __URARLIB_H
#define __URARLIB_H

#ifdef __cplusplus
extern "C"
{
#endif

/* ************************************************************************ */
/* ************************************************************************ */
/* **                                                                    ** */
/* **   CONFIGURATION of the UniquE RAR FileLib                          ** */
/* **   ==> you may change the setting for the lib HERE!                 ** */
/* **                                                                    ** */
/* ************************************************************************ */
/* ************************************************************************ */

//         not let many thing to configure in the gp port :p

#define _DO_CRC32_CHECK                     /* perform cyclical redundancy  */
                                            /* check (CRC32) - disable this */
                                            /* for a little speed-up        */

/* ------------------------------------------------------------------------ */



/* -- global type definitions --------------------------------------------- */

#define	uchar	unsigned char
#define	ushort	unsigned short
#define	uint	unsigned int
#define	ulong	unsigned long

typedef int	bool;
typedef char	BYTE;
typedef long	DWORD;
typedef uchar	UBYTE;
typedef ushort	UWORD;
typedef ulong	UDWORD;

/* This structure is used for listing archive content                       */
struct RAR20_archive_entry                  /* These infos about files are  */
{                                           	/* stored in RAR v2.0 archives  */
  char   *Name;
  UWORD  NameSize;
  UDWORD PackSize;
  UDWORD UnpSize;
  UBYTE  HostOS;                            /* MSDOS=0,OS2=1,WIN32=2,UNIX=3 */
  UDWORD FileCRC;
  UDWORD FileTime;
  UBYTE  UnpVer;
  UBYTE  Method;
  UDWORD FileAttr;
};

typedef struct  archivelist                 /* used to list archives        */
{
  struct RAR20_archive_entry item;
  struct archivelist         *next;
} ArchiveList_struct;

typedef struct  memory_file                 /* used to decompress files in  */
{                                           /* memory                       */
  void                       *data;         /* pointer to the file data     */
  unsigned long              size;          /* total size of the file data  */
  unsigned long              offset;        /* offset within "memory-file"  */
} MemoryFile;

/* -- global functions ---------------------------------------------------- */

/* urarlib_get:
 * decompresses and decrypt data from a RAR file to a buffer in system memory.
 *
 *   input: *output         pointer to an empty char*. This pointer will show
 *                          to the extracted data
 *          *size           shows where to write the size of the decompressed
 *                          file
 *                          (**NOTE: URARLib _does_ memory allocation etc.!**)
 *          *filename       pointer to string containing the file to decompress
 *          *rarfile        pointer to a string with the full name and path of
 *                          the RAR file or pointer to a RAR file in memory if
 *                          memory-to-memory decompression is active.
 *          *libpassword    pointer to a string with the password used to
 *                          en-/decrypt the RAR
 *   output: int            returns TRUE on success or FALSE on error
 *                          (FALSE=0, TRUE=1)
 */

extern int urarlib_get(void  *output,
                       unsigned long *size,
                       char *filename,
                       void *rarfile,
                       char *libpassword);



/* urarlib_list:
 * list the content of a RAR archive.
 *
 *   input: *rarfile        pointer to a string with the full name and path of
 *                          the RAR file or pointer to a RAR file in memory if
 *                          memory-to-memory decompression is active.
 *          *list           pointer to an ArchiveList_struct that can be
 *                          filled with details about the archive
 *                          to the extracted data
 *   output: int            number of files/directories within archive
 */

extern int urarlib_list(void *rarfile, ArchiveList_struct *list);


/* urarlib_freelist:
 * (after the suggestion and code of Duy Nguyen, Sean O'Blarney
 * and Johannes Winkelmann who independently wrote a patch)
 * free the memory of a ArchiveList_struct created by urarlib_list.
 *
 *    input: *list          pointer to an ArchiveList_struct
 *    output: -
 */

extern void urarlib_freelist(ArchiveList_struct *list);

/* ------------------------------------------------------------------------ */

/* <-- add by -rov ------------------------------------------------> */

// redirect classic function names to gp sdk names
#define	sprintf		(gp_str_func.sprintf)
#define	free		(gp_mem_func.free)
#define	strlen		(gp_str_func.gpstrlen)
#define	strcmp		(gp_str_func.compare)
#define	strcpy		(gp_str_func.strcpy)
#define	memcpy		(gp_str_func.memcpy)
#define	memset		(gp_str_func.memset)
#define	getFreeRam	(gp_mem_func.availablemem)

// function who missing in the gp sdk
long	ftell(F_HANDLE * file)						;
char	*strchr(char * string, char value)			;
void	strncpy(void * dst, void * src, int size)	;

#ifndef	_GDL_
	// not redirect malloc to gp sdk malloc, create a function who show error on aloc error.
	void	*malloc(unsigned size)	;
	// pointer to the function who'll show errors, at you to afect it.
	void	(*error)(const char * txt)	;
	char	err[1024] ;		// a string to store error msg
#endif

// the gp strupper not work same as the classic strupper.
char	*strupper(char *Str)	;

// the normal unrarlib work from memory OR from disk, can specifie that with a define
// the gp unrarlib, see what contain the pointer, a path or the file data,
// so add a var who said at unrarlib how read, and functions to check pointer and affect the var.
int		useMemoryToMemoryDecomp		;
int		checkIfPath(char * string)	;
void		setPathFlag(char * string)	;

// declare fseek define ... as GpFileSeek define
#define SEEK_CUR FROM_CURRENT
#define SEEK_SET FROM_BEGIN

#define debug_log error	// redirect unrarlib error to *error(..)

memory_file * loadMemoryFile(const char * path, ulong fsize=0) ;	// function to load a file (from disk or memory) and it's info into a memory file structure.

/* <-----------------------------------------------------> */

#ifdef __cplusplus
}
#endif

#endif


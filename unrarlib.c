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

/* ***************************************************************************
 **
 **                           UniquE RAR File Library
 **                     The free file lib for the demoscene
 **                   multi-OS version (Win32, Linux and SunOS)
 **
 *****************************************************************************
 **
 **   ==> Please configure the program in "unrarlib.h". <==
 **
 **   RAR decompression code:
 **    (C) Eugene Roshal
 **   Modifications to a FileLib:
 **    (C) 2000-2002 Christian Scheurer aka. UniquE/Vantage (cs@unrarlib.org)
 **   Linux port:
 **    (C) 2000-2002 Johannes Winkelmann (jw@tks6.net)
 **   Gp32 port :
 **    (C) 2004	noferi Mickaël aka -rov (rov@gp32news.com)
 **
 **  The UniquE RAR File Library gives you the ability to access RAR archives
 **  (any compression method supported in RAR v2.0 including Multimedia
 **  Compression and encryption) directly from your program with ease an by
 **  adding only 12kB (6kB UPX-compressed) additional code to your program.
 **  Both solid and normal (recommended for fast random access to the files!)
 **  archives are supported. This FileLib is made for the Demo scene, so it's
 **  designed for easy use within your demos and intros.
 **  Please read "licence.txt" to learn more about how you may use URARFileLib
 **  in your productions.
 **
 *****************************************************************************/
 
/* -- include files ------------------------------------------------------- */
#include "unrarlib.h"                       /* include global configuration */
/* ------------------------------------------------------------------------ */

/* -- global stuff -------------------------------------------------------- */

#define MAXWINSIZE      0x100000	// 1mo
#define MAXWINMASK      (MAXWINSIZE-1)
#define UNP_MEMORY      MAXWINSIZE
#define Min(x,y) (((x)<(y)) ? (x):(y))
#define Max(x,y) (((x)>(y)) ? (x):(y))
#define NM  260

#define SIZEOF_MARKHEAD         7
#define SIZEOF_OLDMHD           7
#define SIZEOF_NEWMHD          13
#define SIZEOF_OLDLHD          21
#define SIZEOF_NEWLHD          32
#define SIZEOF_SHORTBLOCKHEAD   7
#define SIZEOF_LONGBLOCKHEAD   11
#define SIZEOF_COMMHEAD        13
#define SIZEOF_PROTECTHEAD     26

#define PACK_VER       20	/* version of decompression code*/
#define UNP_VER        20
#define PROTECT_VER    20

#define LHD_PASSWORD       4
#define LHD_SOLID          16

#define LONG_BLOCK         0x8000
#define READSUBBLOCK       0x8000

enum { ALL_HEAD=0,MARK_HEAD=0x72,MAIN_HEAD=0x73,FILE_HEAD=0x74,
       COMM_HEAD=0x75,AV_HEAD=0x76,SUB_HEAD=0x77,PROTECT_HEAD=0x78};

struct MarkHeader
{
  UBYTE Mark[7];
};

struct NewMainArchiveHeader
{
  UWORD HeadCRC;
  UBYTE HeadType;
  UWORD Flags;
  UWORD HeadSize;
  UWORD Reserved;
  UDWORD Reserved1;
};

struct NewFileHeader
{
  UWORD HeadCRC;
  UBYTE HeadType;
  UWORD Flags;
  UWORD HeadSize;
  UDWORD PackSize;
  UDWORD UnpSize;
  UBYTE HostOS;
  UDWORD FileCRC;
  UDWORD FileTime;
  UBYTE UnpVer;
  UBYTE Method;
  UWORD NameSize;
  UDWORD FileAttr;
};

struct BlockHeader
{
  UWORD HeadCRC;
  UBYTE HeadType;
  UWORD Flags;
  UWORD HeadSize;
  UDWORD DataSize;
};

struct Decode
{
  unsigned int MaxNum;
  unsigned int DecodeLen[16];
  unsigned int DecodePos[16];
  unsigned int DecodeNum[2];
};

struct MarkHeader MarkHead;
struct NewMainArchiveHeader NewMhd;
struct NewFileHeader NewLhd;
struct BlockHeader BlockHead;

UBYTE *TempMemory;                          /* temporary unpack-buffer      */
char *CommMemory;

UBYTE *UnpMemory;
char ArgName[NM];                           /* current file in rar archive  */
char ArcFileName[NM];                       /* file to decompress           */

MemoryFile *MemRARFile;                   /* pointer to RAR file in memory*/
char ArcName[255];                        /* RAR archive name             */
F_HANDLE ArcPtr;                             /* input RAR file handler       */

char Password[255];                         /* password to decrypt files    */

unsigned char *temp_output_buffer;          /* extract files to this pointer*/
unsigned long *temp_output_buffer_offset;   /* size of temp. extract buffer */

BOOL FileFound;                             /* TRUE=use current extracted   */
                                            /* data FALSE=throw data away,  */
                                            /* wrong file                   */
int MainHeadSize;
long CurBlockPos,NextBlockPos;

unsigned long CurUnpRead, CurUnpWrite;
long UnpPackedSize;
long DestUnpSize;

UDWORD HeaderCRC;
int Encryption;

unsigned int UnpWrSize;
unsigned char *UnpWrAddr;
unsigned int UnpPtr,WrPtr;

unsigned char PN1,PN2,PN3;
unsigned short OldKey[4];


/* function header definitions */

int ReadHeader(int BlockType);
BOOL ExtrFile(void);
BOOL ListFile(void);
int tread(void *stream,void *buf,unsigned len);
int tseek(F_HANDLE *stream,long offset,int fromwhere);
BOOL UnstoreFile(void);
int IsArchive(void);
int ReadBlock(int BlockType);
unsigned int UnpRead(unsigned char *Addr,unsigned int Count);
void UnpInitData(void);
void Unpack(unsigned char *UnpAddr);
UBYTE DecodeAudio(int Delta);
static void DecodeNumber(struct Decode *Dec);
void UpdKeys(UBYTE *Buf);
void SetCryptKeys(char *Password);
void SetOldKeys(char *Password);
void DecryptBlock(unsigned char *Buf);
void InitCRC(void);
UDWORD CalcCRC32(UDWORD StartCRC,UBYTE *Addr,UDWORD Size);
void UnpReadBuf(int FirstBuf);
void ReadTables(void);
static void ReadLastTables(void);
static void MakeDecodeTables(unsigned char *LenTab,
                             struct Decode *Dec,
                             int Size);
int stricomp(char *Str1,char *Str2);


/* <------ add by -rov -------------------------------------------------> */

#ifndef	_GDL_
	void * malloc(unsigned size)
	{
		void * mem = gm_malloc(size) ;
		if(!mem)
		{
			sprintf(err,"error on alocating %i bytes",size) ;
			error(err) ;
		}
		return mem ;
	}
#endif

long ftell(F_HANDLE * file)
{	long offset ;
	GpFileSeek(*file,FROM_CURRENT,0,&offset);
	return offset ;
}

int checkIfPath(char * string)
{
	if((*string=='g')&&(*(string+1)=='p')) return 1 ;
	return 0 ;
}

void setPathFlag(char * string)
{
	useMemoryToMemoryDecomp = !checkIfPath(string) ;
}

char* strupper(char *Str)
{
	gp_str_func.uppercase(Str,strlen(Str)) ;
	return(Str);
}

char * strchr(char * string, char value)
{
	char * yo = string ;
	int maxSize = strlen(string) ;
	
	while(((yo-string) < maxSize)&&((*yo) != value)) yo++ ;

	if((!(yo-string))||((yo-string)>=maxSize)) return 0 ;
	return yo ;
}

void   strncpy(void * dst, void * src, int size)
{
	memcpy(dst,src,size) ;
}

memory_file * loadMemoryFile(const char * path, ulong fsize)
{	memory_file * mfile = (memory_file *)malloc(sizeof(struct memory_file)) ;

	if(checkIfPath(path)) // load from disk
	{
		F_HANDLE file ;
		if(SM_OK != GpFileGetSize(path,&fsize)) { free(mfile) ; error("unable to get file size, not found ?") ; return 0 ; }
		if(fsize > getFreeRam() + 0x1000) { free(mfile) ; error("not enought ram to load file.") ; return 0 ; }
		if(SM_OK != GpFileOpen(path, OPEN_R, &file)) { free(mfile) ; error("unable to open file.") ; return 0 ; }
		mfile->data = (char*)malloc(fsize) ;
		GpFileRead(file,mfile->data,fsize,0);
		GpFileClose(file) ;
	} else mfile->data = path ;

	mfile->offset = 0 ;
	mfile->size = fsize ;
	return mfile ;
}

/* <-----------------------------------------------------> */

/* -- global functions ---------------------------------------------------- */

int urarlib_get(void *output,
                unsigned long *size,
                char *filename,
                void *rarfile,
                char *libpassword)
/* Get a file from a RAR file to the "output" buffer. The UniquE RAR FileLib
 * does everything from allocating memory, decrypting and unpacking the file
 * from the archive. TRUE is returned if the file could be successfully
 * extracted, else a FALSE indicates a failure.
 */
{
  BOOL  retcode = FALSE;
  InitCRC();                                /* init some vars               */

	setPathFlag((char *)rarfile);				// check if pointer design data or a path

  strcpy(ArgName, filename);                /* set file(s) to extract       */
  
	if(useMemoryToMemoryDecomp)	MemRARFile = (MemoryFile*)rarfile	;
	else				strcpy(ArcName, (char *)rarfile)	;

  if(libpassword != NULL)
    strcpy(Password, libpassword);          /* init password                */

  temp_output_buffer = NULL;
  temp_output_buffer_offset=size;           /* set size of the temp buffer  */

  retcode = ExtrFile();                     /* unpack file now!             */
  
  memset(Password,0,sizeof(Password));      /* clear password               */

	if(!useMemoryToMemoryDecomp){
	  if (ArcPtr!=NULL){
	      GpFileClose(ArcPtr);
	      ArcPtr = NULL;
	  }
	}

  free(UnpMemory);                          /* free memory                  */
  free(TempMemory);
  free(CommMemory);
  UnpMemory=NULL;
  TempMemory=NULL;
  CommMemory=NULL;

  if(retcode == FALSE)
  {
    free(temp_output_buffer);               /* free memory and return NULL  */
    temp_output_buffer=NULL;
    *(DWORD*)output=0;                      /* pointer on errors            */
    *size=0;
   	sprintf(err, "Error - couldn't extract %s\nand alloc %i bytes", filename,(uint)*size);	error(err) ;
  }

  *(DWORD*)output=(DWORD)temp_output_buffer;/* return pointer for unpacked*/
                                            /* data                       */
  return retcode;
}


int urarlib_list(void *rarfile, ArchiveList_struct *list)
{
  ArchiveList_struct *tmp_List = NULL;
  int NoOfFilesInArchive       = 0;         /* number of files in archive   */
	setPathFlag((char*)rarfile);
  InitCRC();                                /* init some vars               */
	if(useMemoryToMemoryDecomp){
	  MemRARFile         = (MemoryFile*)rarfile;             /* assign pointer to RAR file   */
	  MemRARFile->offset = 0;
	  if (!IsArchive())
	    return NoOfFilesInArchive;              /* error => exit!               */
	}else {
	  /* open and identify archive                                              */
	  if(SM_OK == GpFileOpen((char*)rarfile,OPEN_R,&ArcPtr))
	  {
	    if (!IsArchive())
	    {
	      GpFileClose(ArcPtr);
	      ArcPtr = NULL;
	      return NoOfFilesInArchive;            /* error => exit!               */
	    }
	  } else {
	    debug_log("Error opening file.");
	    return NoOfFilesInArchive;
	  }
}

  if ((UnpMemory=(uchar *)malloc(UNP_MEMORY))==NULL)
  {
    debug_log("Can't allocate memory for decompression!");
    return NoOfFilesInArchive;
  }

	if(useMemoryToMemoryDecomp){
	  MemRARFile->offset+=NewMhd.HeadSize-MainHeadSize;
	}else{
	  tseek(&ArcPtr,NewMhd.HeadSize-MainHeadSize,SEEK_CUR);
	}
  (*(DWORD*)list) = (DWORD)NULL;            /* init file list               */
  /* do while file is not extracted and there's no error                    */

  while (TRUE)
  {
    if (ReadBlock(FILE_HEAD | READSUBBLOCK) <= 0) /* read name of the next  */
		break;                              /* file within the RAR archive  */
     		                                /* error, file not found in     */
	                                        /* archive or I/O error         */
    if (BlockHead.HeadType==SUB_HEAD)
    {
      debug_log("Sorry, sub-headers not supported.");
      break;                                /* error => exit                */
    }

    if((void*)(*(DWORD*)list) == NULL)      /* first entry                  */
    {
      tmp_List = (ArchiveList_struct*)malloc(sizeof(ArchiveList_struct));
      tmp_List->next = NULL;

      (*(DWORD*)list) = (DWORD)tmp_List;

    } else                                  /* add entry                    */
    {
      tmp_List->next = (ArchiveList_struct *)malloc(sizeof(ArchiveList_struct));
      tmp_List = (ArchiveList_struct*) tmp_List->next;
      tmp_List->next = NULL;
    }

    tmp_List->item.Name = (char *)malloc(NewLhd.NameSize + 1);
    strcpy(tmp_List->item.Name, ArcFileName);
    tmp_List->item.NameSize = NewLhd.NameSize;
    tmp_List->item.PackSize = NewLhd.PackSize;
    tmp_List->item.UnpSize = NewLhd.UnpSize;
    tmp_List->item.HostOS = NewLhd.HostOS;
    tmp_List->item.FileCRC = NewLhd.FileCRC;
    tmp_List->item.FileTime = NewLhd.FileTime;
    tmp_List->item.UnpVer = NewLhd.UnpVer;
    tmp_List->item.Method = NewLhd.Method;
    tmp_List->item.FileAttr = NewLhd.FileAttr;

    NoOfFilesInArchive++;                   /* count files                  */

	if(useMemoryToMemoryDecomp)	    MemRARFile->offset = NextBlockPos;
	else if (ArcPtr!=NULL) tseek(&ArcPtr,NextBlockPos,SEEK_SET);
  };

  /* free memory, clear password and close archive                          */
  memset(Password,0,sizeof(Password));      /* clear password               */
	if(!useMemoryToMemoryDecomp){
	  if (ArcPtr!=NULL){
	      GpFileClose(ArcPtr);
	      ArcPtr = NULL;
	  }
	}

  free(UnpMemory);                          /* free memory                  */
  free(TempMemory);
  free(CommMemory);
  UnpMemory=NULL;
  TempMemory=NULL;
  CommMemory=NULL;

  return NoOfFilesInArchive;
}

/* urarlib_freelist:
 * (after the suggestion and code of Duy Nguyen, Sean O'Blarney
 * and Johannes Winkelmann who independently wrote a patch)
 * free the memory of a ArchiveList_struct created by urarlib_list.
 *
 *    input: *list          pointer to an ArchiveList_struct
 *    output: -
 */

void urarlib_freelist(ArchiveList_struct *list)
{
    ArchiveList_struct* tmp = list;

    while ( list ) {
        tmp = list->next;
        free( list->item.Name );
        free( list );
        list = tmp;
    }
}

/****************************************************************************
 ****************************************************************************
 ****************************************************************************
 ****************************************************************************
 *******                                                              *******
 *******                                                              *******
 *******                                                              *******
 *******                    B L O C K   I / O                         *******
 *******                                                              *******
 *******                                                              *******
 *******                                                              *******
 ****************************************************************************
 ****************************************************************************
 ****************************************************************************
 ****************************************************************************/

#define GetHeaderByte(N) Header[N]

#define GetHeaderWord(N) (Header[N]+((UWORD)Header[N+1]<<8))

#define GetHeaderDword(N) (Header[N]+((UWORD)Header[N+1]<<8)+\
                          ((UDWORD)Header[N+2]<<16)+\
                          ((UDWORD)Header[N+3]<<24))

int ReadBlock(int BlockType)
{
  struct NewFileHeader SaveFileHead;
  int Size=0,ReadSubBlock=0;
  static int LastBlock;
  memcpy(&SaveFileHead,&NewLhd,sizeof(SaveFileHead));
  if (BlockType & READSUBBLOCK)
    ReadSubBlock=1;
  BlockType &= 0xff;
  {
    while (1)
    {
		if(useMemoryToMemoryDecomp)	CurBlockPos=MemRARFile->offset;       /* get offset of mem-file       */
		else						CurBlockPos=ftell(&ArcPtr);

		Size=ReadHeader(FILE_HEAD);
		if (Size!=0)
		{
		if (NewLhd.HeadSize<SIZEOF_SHORTBLOCKHEAD)
		  return(0);
		NextBlockPos=CurBlockPos+NewLhd.HeadSize;
		if (NewLhd.Flags & LONG_BLOCK)
		  NextBlockPos+=NewLhd.PackSize;
		if (NextBlockPos<=CurBlockPos)
		  return(0);
		}

		if (Size > 0 && BlockType!=SUB_HEAD)
		LastBlock=BlockType;
		if	(Size==0 || BlockType==ALL_HEAD || NewLhd.HeadType==BlockType || 
			(NewLhd.HeadType==SUB_HEAD && ReadSubBlock && LastBlock==BlockType))	break;

		if(useMemoryToMemoryDecomp)	MemRARFile->offset = NextBlockPos;
		else						tseek(&ArcPtr, NextBlockPos, SEEK_SET);
    } ;
  }

  BlockHead.HeadCRC=NewLhd.HeadCRC;
  BlockHead.HeadType=NewLhd.HeadType;
  BlockHead.Flags=NewLhd.Flags;
  BlockHead.HeadSize=NewLhd.HeadSize;
  BlockHead.DataSize=NewLhd.PackSize;

  if (BlockType!=NewLhd.HeadType) BlockType=ALL_HEAD;

  if((FILE_HEAD == BlockType) && (Size>0))
  {
    NewLhd.NameSize=Min(NewLhd.NameSize,sizeof(ArcFileName)-1);
	if(useMemoryToMemoryDecomp){
	    tread(MemRARFile, ArcFileName, NewLhd.NameSize);
	}else{
	    tread(&ArcPtr,ArcFileName,NewLhd.NameSize);
	}
    ArcFileName[NewLhd.NameSize]=0;

    if(NewLhd.HeadCRC!=(UWORD)~CalcCRC32(HeaderCRC,(UBYTE*)&ArcFileName[0],NewLhd.NameSize))
      debug_log("file header broken");

    Size+=NewLhd.NameSize;
  } else
  {
    memcpy(&NewLhd,&SaveFileHead,sizeof(NewLhd));
	if(useMemoryToMemoryDecomp)	MemRARFile->offset = CurBlockPos	;
	else						tseek(&ArcPtr,CurBlockPos,SEEK_SET)	;
  }
  return(Size);
}

int ReadHeader(int BlockType)
{
  int Size = 0;
  unsigned char Header[64];
  switch(BlockType)
  {
    case MAIN_HEAD:
		if(useMemoryToMemoryDecomp)	Size=tread(MemRARFile, Header, SIZEOF_NEWMHD);
		else						Size=tread(&ArcPtr,Header,SIZEOF_NEWMHD);
        NewMhd.HeadCRC=(unsigned short)GetHeaderWord(0);
        NewMhd.HeadType=GetHeaderByte(2);
        NewMhd.Flags=(unsigned short)GetHeaderWord(3);
        NewMhd.HeadSize=(unsigned short)GetHeaderWord(5);
        NewMhd.Reserved=(unsigned short)GetHeaderWord(7);
        NewMhd.Reserved1=GetHeaderDword(9);
        HeaderCRC=CalcCRC32(0xFFFFFFFFL,&Header[2],SIZEOF_NEWMHD-2);
      break;
    case FILE_HEAD:
		if(useMemoryToMemoryDecomp)	Size=tread(MemRARFile, Header, SIZEOF_NEWLHD);
		else				        Size=tread(&ArcPtr,Header,SIZEOF_NEWLHD);
        NewLhd.HeadCRC=(unsigned short)GetHeaderWord(0);
        NewLhd.HeadType=GetHeaderByte(2);
        NewLhd.Flags=(unsigned short)GetHeaderWord(3);
        NewLhd.HeadSize=(unsigned short)GetHeaderWord(5);
        NewLhd.PackSize=GetHeaderDword(7);
        NewLhd.UnpSize=GetHeaderDword(11);
        NewLhd.HostOS=GetHeaderByte(15);
        NewLhd.FileCRC=GetHeaderDword(16);
        NewLhd.FileTime=GetHeaderDword(20);
        NewLhd.UnpVer=GetHeaderByte(24);
        NewLhd.Method=GetHeaderByte(25);
        NewLhd.NameSize=(unsigned short)GetHeaderWord(26);
        NewLhd.FileAttr=GetHeaderDword(28);
        HeaderCRC=CalcCRC32(0xFFFFFFFFL,&Header[2],SIZEOF_NEWLHD-2);
      break;

	  case COMM_HEAD:		error("Commented archive not supported!");	break;
	  case PROTECT_HEAD:	error("Protected headers not supported!");	break;
	  case ALL_HEAD:		error("ShortBlockHeader not supported!");	break;
	  default:				error("Unknown//unsupported !");			break;
  };

  return(Size);
}

/* **************************************************************************
 ****************************************************************************
 ****************************************************************************
 ****************************************************************************
 *******                                                              *******
 *******                                                              *******
 *******                                                              *******
 *******                  E X T R A C T   L O O P                     *******
 *******                                                              *******
 *******                                                              *******
 *******                                                              *******
 ****************************************************************************
 ****************************************************************************
 ****************************************************************************
 ************************************************************************** */


int IsArchive(void)
{
	void * readFromWhat = MemRARFile ;
	if(!useMemoryToMemoryDecomp) readFromWhat = &ArcPtr ;
	if(tread(readFromWhat, MarkHead.Mark, SIZEOF_MARKHEAD) != SIZEOF_MARKHEAD) return 0 ;

	char rarHeader[7] = { 0x52,0x61,0x72,0x21,0x1a,0x07,0 } ; // rar v2 header

	if(strcmp((char*)MarkHead.Mark,rarHeader)) /* not a rar ! */
	{
		sprintf(err,"unknown archive type\n\nonly plain rar v2 supported (normal and solid)\nSFX and Volumes are NOT supported!") ;
		error(err) ;	return 0 ;
    } else if(ReadHeader(MAIN_HEAD) != SIZEOF_NEWMHD)	{ error("rar start ok but wrong header") ; return 0 ; }

	MainHeadSize=SIZEOF_NEWMHD;
	return 1 ;
}

BOOL ExtrFile(void)
{
  BOOL ReturnCode=TRUE;
  FileFound=FALSE;                          /* no file found by default     */

	if(useMemoryToMemoryDecomp){
	  MemRARFile->offset = 0;                   /* start reading from offset 0  */
	  if (!IsArchive())
	  {
		debug_log("Not a RAR file");
	    return FALSE;                           /* error => exit!               */
	  }
	} else {
	  /* open and identify archive                                              */
	  if(SM_OK == GpFileOpen(ArcName,OPEN_R,&ArcPtr))
	  {
	    if (!IsArchive())
	    {
	      GpFileClose(ArcPtr);
	      ArcPtr = NULL;
	      return FALSE;                         /* error => exit!               */
	    }
	  } else {
	    debug_log("Error opening file.");
	    return FALSE;
	  }
	}

  if ((UnpMemory=(uchar*)malloc(UNP_MEMORY))==NULL)
  {
    debug_log("Can't allocate memory for decompression!");
    return FALSE;
  }

	if(useMemoryToMemoryDecomp){
	  MemRARFile->offset+=NewMhd.HeadSize-MainHeadSize;
	}else{
	  tseek(&ArcPtr,NewMhd.HeadSize-MainHeadSize,SEEK_CUR);
	}

  /* do while file is not extracted and there's no error                    */
  do
  {	
    if (ReadBlock(FILE_HEAD | READSUBBLOCK) <= 0) /* read name of the next  */
    {                                       /* file within the RAR archive  */
	  error("file not found or corupt rar.");

      ReturnCode=FALSE;
      break;                                /* error, file not found in     */
    }                                       /* archive or I/O error         */
    if (BlockHead.HeadType==SUB_HEAD)
    {
      debug_log("Sorry, sub-headers not supported.");
      ReturnCode=FALSE;
      break;                                /* error => exit                */
    }

    if(TRUE == (FileFound=(stricomp(ArgName, ArcFileName) == 0)))	/* *** file found! *** */
    {
    	temp_output_buffer=(uchar*)malloc(NewLhd.UnpSize);	/* allocate memory for the	*/
	    *temp_output_buffer_offset=0;         				/* file. The default offset */
	                                          				/* within the buffer is 0   */
      if(temp_output_buffer == NULL)
      {
        debug_log("can't allocate memory for the file decompression");
        ReturnCode=FALSE;
        break;                              /* error, can't extract file!   */
      }
    }

    /* in case of a solid archive, we need to decompress any single file till
     * we have found the one we are looking for. In case of normal archives
     * (recommended!!), we skip the files until we are sure that it is the
     * one we want.
     */
    if((NewMhd.Flags & 0x08) || FileFound)
    {
      if (NewLhd.UnpVer<13 || NewLhd.UnpVer>UNP_VER)
      {
        debug_log("unknown compression method");
        ReturnCode=FALSE;
        break;          /* error, can't extract file!   */
      }

      CurUnpRead=CurUnpWrite=0;
      if ((*Password!=0) && (NewLhd.Flags & LHD_PASSWORD))
        Encryption=NewLhd.UnpVer;
      else
        Encryption=0;
      if (Encryption) SetCryptKeys(Password);

      UnpPackedSize=NewLhd.PackSize;
      DestUnpSize=NewLhd.UnpSize;

      if (NewLhd.Method==0x30)
      {
        UnstoreFile();
      } else
      {
        Unpack(UnpMemory);
      }
		#ifdef _DO_CRC32_CHECK  /* calculate CRC32 */
		      if((UBYTE*)temp_output_buffer != NULL)
		      {
		        if(NewLhd.FileCRC!=~CalcCRC32(0xFFFFFFFFL,
		                                      (UBYTE*)temp_output_buffer,
		                                      NewLhd.UnpSize))
		        {
		          debug_log("CRC32 error, can't extract file!");
		          ReturnCode=FALSE;	break;
		        }
		      }
		#endif
    }

	if(useMemoryToMemoryDecomp)	MemRARFile->offset = NextBlockPos;
	else	if(ArcPtr!=NULL) tseek(&ArcPtr,NextBlockPos,SEEK_SET);

  } while(stricomp(ArgName, ArcFileName) != 0);/* exit if file is extracted */

  /* free memory, clear password and close archive                          */
  free(UnpMemory);
  UnpMemory=NULL;
	if(!useMemoryToMemoryDecomp){
	  if (ArcPtr!=NULL){
	      GpFileClose(ArcPtr);
	      ArcPtr = NULL;
	  }
	}

  return ReturnCode;   /* file extracted successful!   */
}

/* **************************************************************************
 ****************************************************************************
 ****************************************************************************
 ****************************************************************************
 *******                                                              *******
 *******                                                              *******
 *******                                                              *******
 *******             G L O B A L   F U N C T I O N S                  *******
 *******                                                              *******
 *******                                                              *******
 *******                                                              *******
 ****************************************************************************
 ****************************************************************************
 ****************************************************************************
 ************************************************************************** */


int tread(void *stream,void *buf,unsigned len)
{
	if(useMemoryToMemoryDecomp)
	{
		ulong size = ((MemoryFile*)stream)->size ;
		ulong offset = ((MemoryFile*)stream)->offset ;
		char * yop ;
		
		if((offset + len > size) || (!len))
		 return 0;

		yop =  (char *)(((MemoryFile*)stream)->data) ;
		yop += (((MemoryFile*)stream)->offset) ;
		memcpy(buf,yop,len) ;
		((MemoryFile*)stream)->offset+=len ;
		return len ;
	}else{
		ulong count ;
		GpFileRead(*(F_HANDLE*)stream,buf,(uint)len,&count);
		return (int)count ;
	}
}

int tseek(F_HANDLE *stream,long offset,int fromwhere)
{
  return (SM_OK != GpFileSeek(*stream,fromwhere,offset,NULL)) ;
}

int stricomp(char *Str1,char *Str2)
/* compare strings without regard of '\' and '/'                            */
{
  char S1[512],S2[512];
  char *chptr;

	//sprintf(err,"str comp.\n%20s\n%20s",Str1,Str2) ; error(err) ;
	
  strncpy(S1,Str1,sizeof(S1));
  strncpy(S2,Str2,sizeof(S2));

  while((chptr = strchr(S1, '\\')) != NULL) /* ignore backslash             */
  {
    *chptr = '_';
  }

  while((chptr = strchr(S2, '\\')) != NULL) /* ignore backslash             */
  {
    *chptr = '_';
  }

  while((chptr = strchr(S1, '/')) != NULL)  /* ignore slash                 */
  {
    *chptr = '_';
  }

  while((chptr = strchr(S2, '/')) != NULL)  /* ignore slash                 */
  {
    *chptr = '_';
  }

  return(strcmp(strupper(S1),strupper(S2)));
}

/* **************************************************************************
 ****************************************************************************
 ****************************************************************************
 ****************************************************************************
 *******                                                              *******
 *******                                                              *******
 *******                                                              *******
 *******                   U N P A C K   C O D E                      *******
 *******                                                              *******
 *******                                                              *******
 *******                                                              *******
 ****************************************************************************
 ****************************************************************************
 ****************************************************************************
 ************************************************************************** */


/* *****************************
 * ** unpack stored RAR files **
 * *****************************/

BOOL UnstoreFile(void)
{
  if ((long)(*temp_output_buffer_offset=UnpRead(temp_output_buffer, NewLhd.UnpSize))==-1)
  {
    debug_log("Read error of stored file!");
    return FALSE;
  }
  return TRUE;
}


/* ****************************************
 * ** RAR decompression code starts here **
 * ****************************************/

#define NC 298                              /* alphabet = {0,1,2, .,NC - 1} */
#define DC 48
#define RC 28
#define BC 19
#define MC 257

enum {CODE_HUFFMAN=0,CODE_LZ=1,CODE_LZ2=2,CODE_REPEATLZ=3,CODE_CACHELZ=4,
      CODE_STARTFILE=5,CODE_ENDFILE=6,CODE_STARTMM=8,CODE_ENDMM=7,
      CODE_MMDELTA=9};

struct AudioVariables
{
  int K1,K2,K3,K4,K5;
  int D1,D2,D3,D4;
  int LastDelta;
  unsigned int Dif[11];
  unsigned int ByteCount;
  int LastChar;
};


#define NC 298  /* alphabet = {0, 1, 2, ..., NC - 1} */
#define DC 48
#define RC 28
#define BC 19
#define MC 257


struct AudioVariables AudV[4];

#define GetBits()                                                 \
        BitField = ( ( ( (UDWORD)InBuf[InAddr]   << 16 ) |        \
                       ( (UWORD) InBuf[InAddr+1] <<  8 ) |        \
                       (         InBuf[InAddr+2]       ) )        \
                       >> (8-InBit) ) & 0xffff;


#define AddBits(Bits)                          \
        InAddr += ( InBit + (Bits) ) >> 3;     \
        InBit  =  ( InBit + (Bits) ) &  7;

static unsigned char *UnpBuf;
static unsigned int BitField;
static unsigned int Number;

unsigned char InBuf[8192];                  /* input read buffer            */

unsigned char UnpOldTable[MC*4];

unsigned int InAddr,InBit,ReadTop;

unsigned int LastDist,LastLength;
static unsigned int Length,Distance;

unsigned int OldDist[4],OldDistPtr;


struct LitDecode
{
  unsigned int MaxNum;
  unsigned int DecodeLen[16];
  unsigned int DecodePos[16];
  unsigned int DecodeNum[NC];
} LD;

struct DistDecode
{
  unsigned int MaxNum;
  unsigned int DecodeLen[16];
  unsigned int DecodePos[16];
  unsigned int DecodeNum[DC];
} DD;

struct RepDecode
{
  unsigned int MaxNum;
  unsigned int DecodeLen[16];
  unsigned int DecodePos[16];
  unsigned int DecodeNum[RC];
} RD;

struct MultDecode
{
  unsigned int MaxNum;
  unsigned int DecodeLen[16];
  unsigned int DecodePos[16];
  unsigned int DecodeNum[MC];
} MD[4];

struct BitDecode
{
  unsigned int MaxNum;
  unsigned int DecodeLen[16];
  unsigned int DecodePos[16];
  unsigned int DecodeNum[BC];
} BD;

static struct MultDecode *MDPtr[4]={&MD[0],&MD[1],&MD[2],&MD[3]};

int UnpAudioBlock,UnpChannels,CurChannel,ChannelDelta;


void Unpack(unsigned char *UnpAddr)
/* *** 38.3% of all CPU time is spent within this function!!!               */
{
  static unsigned char LDecode[]={0,1,2,3,4,5,6,7,8,10,12,14,16,20,24,28,32,
                                  40,48,56,64,80,96,112,128,160,192,224};
  static unsigned char LBits[]=  {0,0,0,0,0,0,0,0,1,1,1,1,2,2,2,2,3,
                                  3,3,3,4,4,4,4,5,5,5,5};
  static int DDecode[]={0,1,2,3,4,6,8,12,16,24,32,48,64,96,128,192,256,384,
                        512,768,1024,1536,2048,3072,4096,6144,8192,12288,
                        16384,24576,32768U,49152U,65536,98304,131072,196608,
                        262144,327680,393216,458752,524288,589824,655360,
                        720896,786432,851968,917504,983040};
  static unsigned char DBits[]=  {0,0,0,0,1,1,2,2,3,3,4,4,5,5,6,6,7,7,8,8,9,
                                  9,10,10,11,11,12,12,13,13,14,14,15,15,16,
                                  16,16,16,16,16,16,16,16,16,16,16,16,16};
  static unsigned char SDDecode[]={0,4,8,16,32,64,128,192};
  static unsigned char SDBits[]={2,2,3, 4, 5, 6,  6,  6};
  unsigned int Bits;


  UnpBuf=UnpAddr;                           /* UnpAddr is a pointer to the  */
  UnpInitData();                            /* unpack buffer                */
  UnpReadBuf(1);
  if (!(NewLhd.Flags & LHD_SOLID))
     ReadTables();
   DestUnpSize--;

  while (DestUnpSize>=0)
  {
    UnpPtr&=MAXWINMASK;

    if (InAddr>sizeof(InBuf)-30)
      UnpReadBuf(0);
    if (((WrPtr-UnpPtr) & MAXWINMASK)<270 && WrPtr!=UnpPtr)
    {


      if (FileFound)
      {

        if (UnpPtr<WrPtr)
        {
                        if((*temp_output_buffer_offset + UnpPtr) > NewLhd.UnpSize)
                        {
                           debug_log("Fatal! Buffer overrun during decompression!");
                          DestUnpSize=-1;

                    } else
                        {
              /* copy extracted data to output buffer                         */
              memcpy(temp_output_buffer + *temp_output_buffer_offset,
                     &UnpBuf[WrPtr], (0-WrPtr) & MAXWINMASK);
              /* update offset within buffer                                  */
              *temp_output_buffer_offset+= (0-WrPtr) & MAXWINMASK;
              /* copy extracted data to output buffer                         */
              memcpy(temp_output_buffer + *temp_output_buffer_offset, UnpBuf,
                     UnpPtr);
              /* update offset within buffer                                  */
              *temp_output_buffer_offset+=UnpPtr;
                        }
        } else
        {
                        if((*temp_output_buffer_offset + (UnpPtr-WrPtr)) > NewLhd.UnpSize)
                        {
                           debug_log("Fatal! Buffer overrun during decompression!");
                          DestUnpSize=-1;
                    } else
                        {
                  /* copy extracted data to output buffer                       */
              memcpy(temp_output_buffer + *temp_output_buffer_offset,
                     &UnpBuf[WrPtr], UnpPtr-WrPtr);
              *temp_output_buffer_offset+=UnpPtr-WrPtr;                                                /* update offset within buffer */
                    }

            }
      }

      WrPtr=UnpPtr;
    }

    if (UnpAudioBlock)
    {
      DecodeNumber((struct Decode *)MDPtr[CurChannel]);
      if (Number==256)
      {
        ReadTables();
        continue;
      }
      UnpBuf[UnpPtr++]=DecodeAudio(Number);
      if (++CurChannel==UnpChannels)
        CurChannel=0;
      DestUnpSize--;
      continue;
    }

    DecodeNumber((struct Decode *)&LD);
    if (Number<256)
    {
      UnpBuf[UnpPtr++]=(UBYTE)Number;
      DestUnpSize--;
      continue;
    }
    if (Number>269)
    {
      Length=LDecode[Number-=270]+3;
      if ((Bits=LBits[Number])>0)
      {
        GetBits();
        Length+=BitField>>(16-Bits);
        AddBits(Bits);
      }

      DecodeNumber((struct Decode *)&DD);
      Distance=DDecode[Number]+1;
      if ((Bits=DBits[Number])>0)
      {
        GetBits();
        Distance+=BitField>>(16-Bits);
        AddBits(Bits);
      }

      if (Distance>=0x40000L)
        Length++;

      if (Distance>=0x2000)
        Length++;

       LastDist=OldDist[OldDistPtr++ & 3]=Distance;
       DestUnpSize-=(LastLength=Length);
       while (Length--)
       {
         UnpBuf[UnpPtr]=UnpBuf[(UnpPtr-Distance) & MAXWINMASK];
         UnpPtr=(UnpPtr+1) & MAXWINMASK;
       }

      continue;
    }
    if (Number==269)
    {
      ReadTables();
      continue;
    }
    if (Number==256)
    {
      Length=LastLength;
      Distance=LastDist;
       LastDist=OldDist[OldDistPtr++ & 3]=Distance;
       DestUnpSize-=(LastLength=Length);
       while (Length--)
       {
         UnpBuf[UnpPtr]=UnpBuf[(UnpPtr-Distance) & MAXWINMASK];
         UnpPtr=(UnpPtr+1) & MAXWINMASK;
       }
      continue;
    }
    if (Number<261)
    {
      Distance=OldDist[(OldDistPtr-(Number-256)) & 3];
      DecodeNumber((struct Decode *)&RD);
      Length=LDecode[Number]+2;
      if ((Bits=LBits[Number])>0)
      {
        GetBits();
        Length+=BitField>>(16-Bits);
        AddBits(Bits);
      }
      if (Distance>=0x40000)
        Length++;
      if (Distance>=0x2000)
        Length++;
      if (Distance>=0x101)
        Length++;
       LastDist=OldDist[OldDistPtr++ & 3]=Distance;
       DestUnpSize-=(LastLength=Length);
       while (Length--)
       {
         UnpBuf[UnpPtr]=UnpBuf[(UnpPtr-Distance) & MAXWINMASK];
         UnpPtr=(UnpPtr+1) & MAXWINMASK;
       }
      continue;
    }
    if (Number<270)
    {
      Distance=SDDecode[Number-=261]+1;
      if ((Bits=SDBits[Number])>0)
      {
        GetBits();
        Distance+=BitField>>(16-Bits);
        AddBits(Bits);
      }
      Length=2;
       LastDist=OldDist[OldDistPtr++ & 3]=Distance;
       DestUnpSize-=(LastLength=Length);
       while (Length--)
       {
         UnpBuf[UnpPtr]=UnpBuf[(UnpPtr-Distance) & MAXWINMASK];
         UnpPtr=(UnpPtr+1) & MAXWINMASK;
       }
      continue;
   }
  }
  ReadLastTables();

  if (FileFound)                            /* flush buffer                 */
  {

    if (UnpPtr<WrPtr)
    {
          if((*temp_output_buffer_offset + UnpPtr) > NewLhd.UnpSize)
          {
            debug_log("Fatal! Buffer overrun during decompression!");
                DestUnpSize=-1;
          } else
          {
        /* copy extracted data to output buffer                             */
        memcpy(temp_output_buffer + *temp_output_buffer_offset, &UnpBuf[WrPtr],
               (0-WrPtr) & MAXWINMASK);
        /* update offset within buffer                                      */
        *temp_output_buffer_offset+= (0-WrPtr) & MAXWINMASK;
        /* copy extracted data to output buffer                             */
        memcpy(temp_output_buffer + *temp_output_buffer_offset, UnpBuf, UnpPtr);
        /* update offset within buffer                                      */
        *temp_output_buffer_offset+=UnpPtr;
          }
    } else
    {
          if((*temp_output_buffer_offset + (UnpPtr-WrPtr)) > NewLhd.UnpSize)
          {
                 debug_log("Fatal! Buffer overrun during decompression!");
                DestUnpSize=-1;
          } else
          {
        /* copy extracted data to output buffer                             */
        memcpy(temp_output_buffer + *temp_output_buffer_offset, &UnpBuf[WrPtr],
               UnpPtr-WrPtr);
        /* update offset within buffer                                      */
        *temp_output_buffer_offset+=UnpPtr-WrPtr;
          }
    }
  }

  WrPtr=UnpPtr;
}


unsigned int UnpRead(unsigned char *Addr,unsigned int Count)
{
  int RetCode=0;
  unsigned int I,ReadSize,TotalRead=0;
  unsigned char *ReadAddr;
  ReadAddr=Addr;
  while (Count > 0)
  {
    ReadSize=(unsigned int)((Count>(unsigned long)UnpPackedSize) ?
                                                  UnpPackedSize : Count);
	if(useMemoryToMemoryDecomp){
	    if(MemRARFile->data == NULL)
	      return(0);
	    RetCode=tread(MemRARFile, ReadAddr, ReadSize);
	}else{
	    if (ArcPtr==NULL) return 0 ;
	    RetCode=tread(&ArcPtr,ReadAddr,ReadSize);
	}
    CurUnpRead+=RetCode;
    ReadAddr+=RetCode;
    TotalRead+=RetCode;
    Count-=RetCode;
    UnpPackedSize-=RetCode;
      break;
  }
  if (RetCode!= -1)
  {
    RetCode=TotalRead;
    if (Encryption)
    {
      if (Encryption<20)  debug_log("Old Crypt() not supported!");
      else
      {
        for (I=0;I<(unsigned int)RetCode;I+=16)
          DecryptBlock(&Addr[I]);
      }
    }
  }
  return(RetCode);
}


void UnpReadBuf(int FirstBuf)
{
  int RetCode;
  if (FirstBuf)
  {
    ReadTop=UnpRead(InBuf,sizeof(InBuf));
    InAddr=0;
  }
  else
  {
    memcpy(InBuf,&InBuf[sizeof(InBuf)-32],32);
    InAddr&=0x1f;
    RetCode=UnpRead(&InBuf[32],sizeof(InBuf)-32);
    if (RetCode>0)
      ReadTop=RetCode+32;
    else
      ReadTop=InAddr;
  }
}


void ReadTables(void)
{
  UBYTE BitLength[BC];
  unsigned char Table[MC*4];
  int TableSize,N,I;
  if (InAddr>sizeof(InBuf)-25)
    UnpReadBuf(0);
  GetBits();
  UnpAudioBlock=(BitField & 0x8000);

  if (!(BitField & 0x4000))
    memset(UnpOldTable,0,sizeof(UnpOldTable));
  AddBits(2);


  if (UnpAudioBlock)
  {
    UnpChannels=((BitField>>12) & 3)+1;
    if (CurChannel>=UnpChannels)
      CurChannel=0;
    AddBits(2);
    TableSize=MC*UnpChannels;
  }
  else
    TableSize=NC+DC+RC;


  for (I=0;I<BC;I++)
  {
    GetBits();
    BitLength[I]=(UBYTE)(BitField >> 12);
    AddBits(4);
  }
  MakeDecodeTables(BitLength,(struct Decode *)&BD,BC);
  I=0;
  while (I<TableSize)
  {
    if (InAddr>sizeof(InBuf)-5)
      UnpReadBuf(0);
    DecodeNumber((struct Decode *)&BD);
    if (Number<16)
      Table[I++]=(Number+UnpOldTable[I]) & 0xf;
    else
      if (Number==16)
      {
        GetBits();
        N=(BitField >> 14)+3;
        AddBits(2);
        while (N-- > 0 && I<TableSize)
        {
          Table[I]=Table[I-1];
          I++;
        }
      }
      else
      {
        if (Number==17)
        {
          GetBits();
          N=(BitField >> 13)+3;
          AddBits(3);
        }
        else
        {
          GetBits();
          N=(BitField >> 9)+11;
          AddBits(7);
        }
        while (N-- > 0 && I<TableSize)
          Table[I++]=0;
      }
  }
  if (UnpAudioBlock)
    for (I=0;I<UnpChannels;I++)
      MakeDecodeTables(&Table[I*MC],(struct Decode *)MDPtr[I],MC);
  else
  {
    MakeDecodeTables(&Table[0],(struct Decode *)&LD,NC);
    MakeDecodeTables(&Table[NC],(struct Decode *)&DD,DC);
    MakeDecodeTables(&Table[NC+DC],(struct Decode *)&RD,RC);
  }
  memcpy(UnpOldTable,Table,sizeof(UnpOldTable));
}


static void ReadLastTables(void)
{
  if (ReadTop>=InAddr+5)
  {
    if (UnpAudioBlock)
    {
      DecodeNumber((struct Decode *)MDPtr[CurChannel]);
      if (Number==256)
        ReadTables();
    }
    else
    {
      DecodeNumber((struct Decode *)&LD);
      if (Number==269)
        ReadTables();
    }
  }
}


static void MakeDecodeTables(unsigned char *LenTab,
                             struct Decode *Dec,
                             int Size)
{
  int LenCount[16],TmpPos[16],I;
  long M,N;
  memset(LenCount,0,sizeof(LenCount));
  for (I=0;I<Size;I++)
    LenCount[LenTab[I] & 0xF]++;

  LenCount[0]=0;
  for (TmpPos[0]=Dec->DecodePos[0]=Dec->DecodeLen[0]=0,N=0,I=1;I<16;I++)
  {
    N=2*(N+LenCount[I]);
    M=N<<(15-I);
    if (M>0xFFFF)
      M=0xFFFF;
    Dec->DecodeLen[I]=(unsigned int)M;
    TmpPos[I]=Dec->DecodePos[I]=Dec->DecodePos[I-1]+LenCount[I-1];
  }

  for (I=0;I<Size;I++)
    if (LenTab[I]!=0)
      Dec->DecodeNum[TmpPos[LenTab[I] & 0xF]++]=I;
  Dec->MaxNum=Size;
}


static void DecodeNumber(struct Decode *Deco)
/* *** 52.6% of all CPU time is spent within this function!!!               */
{
  unsigned int I;
  register unsigned int N;
  GetBits();

  N=BitField & 0xFFFE;
  if (N<Deco->DecodeLen[8])  {
    if (N<Deco->DecodeLen[4]) {
      if (N<Deco->DecodeLen[2]) {
        if (N<Deco->DecodeLen[1])
          I=1;
        else
          I=2;
      } else {
        if (N<Deco->DecodeLen[3])
          I=3;
        else
          I=4;
      }
    } else {
      if (N<Deco->DecodeLen[6])  {
        if (N<Deco->DecodeLen[5])
          I=5;
        else
          I=6;
      } else {
        if (N<Deco->DecodeLen[7])
          I=7;
        else
          I=8;
      }
   }
  } else {
    if (N<Deco->DecodeLen[12]) {
      if (N<Deco->DecodeLen[10]) {
        if (N<Deco->DecodeLen[9])
          I=9;
        else
          I=10;
      } else {
        if (N<Deco->DecodeLen[11])
          I=11;
        else
          I=12;
      }
    } else {
      if (N<Deco->DecodeLen[14]) {
        if (N<Deco->DecodeLen[13])
          I=13;
        else
          I=14;

      } else {
          I=15;
      }
    }

  }

  AddBits(I);
  if ((N=Deco->DecodePos[I]+((N-Deco->DecodeLen[I-1])>>(16-I)))>=Deco->MaxNum)
      N=0;
  Number=Deco->DecodeNum[N];
}


void UnpInitData()
{
  InAddr=InBit=0;
  if (!(NewLhd.Flags & LHD_SOLID))
  {
    ChannelDelta=CurChannel=0;

    memset(AudV,0,sizeof(AudV));
    memset(OldDist,0,sizeof(OldDist));
    OldDistPtr=0;
    LastDist=LastLength=0;
    memset(UnpBuf,0,MAXWINSIZE);
    memset(UnpOldTable,0,sizeof(UnpOldTable));
    UnpPtr=WrPtr=0;
  }
}


UBYTE DecodeAudio(int Delta)
{
  struct AudioVariables *V;
  unsigned int Ch;
  unsigned int NumMinDif,MinDif;
  int PCh,I;

  V=&AudV[CurChannel];
  V->ByteCount++;
  V->D4=V->D3;
  V->D3=V->D2;
  V->D2=V->LastDelta-V->D1;
  V->D1=V->LastDelta;
  PCh=8*V->LastChar+V->K1*V->D1+V->K2*V->D2+
           V->K3*V->D3+V->K4*V->D4+V->K5*ChannelDelta;
  PCh=(PCh>>3) & 0xFF;

  Ch=PCh-Delta;

  I=((signed char)Delta)<<3;

  V->Dif[0]+=abs(I);
  V->Dif[1]+=abs(I-V->D1);
  V->Dif[2]+=abs(I+V->D1);
  V->Dif[3]+=abs(I-V->D2);
  V->Dif[4]+=abs(I+V->D2);
  V->Dif[5]+=abs(I-V->D3);
  V->Dif[6]+=abs(I+V->D3);
  V->Dif[7]+=abs(I-V->D4);
  V->Dif[8]+=abs(I+V->D4);
  V->Dif[9]+=abs(I-ChannelDelta);
  V->Dif[10]+=abs(I+ChannelDelta);

  ChannelDelta=V->LastDelta=(signed char)(Ch-V->LastChar);
  V->LastChar=Ch;

  if ((V->ByteCount & 0x1F)==0)
  {
    MinDif=V->Dif[0];
    NumMinDif=0;
    V->Dif[0]=0;
    for (I=1;(unsigned int)I<sizeof(V->Dif)/sizeof(V->Dif[0]);I++)
    {
      if (V->Dif[I]<MinDif)
      {
        MinDif=V->Dif[I];
        NumMinDif=I;
      }
      V->Dif[I]=0;
    }
    switch(NumMinDif)
    {
      case 1:
        if (V->K1>=-16)
          V->K1--;
        break;
      case 2:
        if (V->K1<16)
          V->K1++;
        break;
      case 3:
        if (V->K2>=-16)
          V->K2--;
        break;
      case 4:
        if (V->K2<16)
          V->K2++;
        break;
      case 5:
        if (V->K3>=-16)
          V->K3--;
        break;
      case 6:
        if (V->K3<16)
          V->K3++;
        break;
      case 7:
        if (V->K4>=-16)
          V->K4--;
        break;
      case 8:
        if (V->K4<16)
          V->K4++;
        break;
      case 9:
        if (V->K5>=-16)
          V->K5--;
        break;
      case 10:
        if (V->K5<16)
          V->K5++;
        break;
    }
  }
  return((UBYTE)Ch);
}


/* ***************************************************
 * ** CRCCrypt Code - decryption engine starts here **
 * ***************************************************/


#define NROUNDS 32

#define rol(x,n)  (((x)<<(n)) | ((x)>>(8*sizeof(x)-(n))))
#define ror(x,n)  (((x)>>(n)) | ((x)<<(8*sizeof(x)-(n))))

#define substLong(t) ( (UDWORD)SubstTable[(int)t&255] | \
           ((UDWORD)SubstTable[(int)(t>> 8)&255]<< 8) | \
           ((UDWORD)SubstTable[(int)(t>>16)&255]<<16) | \
           ((UDWORD)SubstTable[(int)(t>>24)&255]<<24) )


UDWORD CRCTab[256];

UBYTE SubstTable[256];
UBYTE InitSubstTable[256]={
  215, 19,149, 35, 73,197,192,205,249, 28, 16,119, 48,221,  2, 42,
  232,  1,177,233, 14, 88,219, 25,223,195,244, 90, 87,239,153,137,
  255,199,147, 70, 92, 66,246, 13,216, 40, 62, 29,217,230, 86,  6,
   71, 24,171,196,101,113,218,123, 93, 91,163,178,202, 67, 44,235,
  107,250, 75,234, 49,167,125,211, 83,114,157,144, 32,193,143, 36,
  158,124,247,187, 89,214,141, 47,121,228, 61,130,213,194,174,251,
   97,110, 54,229,115, 57,152, 94,105,243,212, 55,209,245, 63, 11,
  164,200, 31,156, 81,176,227, 21, 76, 99,139,188,127, 17,248, 51,
  207,120,189,210,  8,226, 41, 72,183,203,135,165,166, 60, 98,  7,
  122, 38,155,170, 69,172,252,238, 39,134, 59,128,236, 27,240, 80,
  131,  3, 85,206,145, 79,154,142,159,220,201,133, 74, 64, 20,129,
  224,185,138,103,173,182, 43, 34,254, 82,198,151,231,180, 58, 10,
  118, 26,102, 12, 50,132, 22,191,136,111,162,179, 45,  4,148,108,
  161, 56, 78,126,242,222, 15,175,146, 23, 33,241,181,190, 77,225,
    0, 46,169,186, 68, 95,237, 65, 53,208,253,168,  9, 18,100, 52,
  116,184,160, 96,109, 37, 30,106,140,104,150,  5,204,117,112, 84
};

UDWORD Key[4];


void EncryptBlock(UBYTE *Buf)
{
  int I;

  UDWORD A,B,C,D,T,TA,TB;
  UDWORD *BufPtr;
  BufPtr=(UDWORD *)Buf;
  A=BufPtr[0]^Key[0];
  B=BufPtr[1]^Key[1];
  C=BufPtr[2]^Key[2];
  D=BufPtr[3]^Key[3];

  for(I=0;I<NROUNDS;I++)
  {
    T=((C+rol(D,11))^Key[I&3]);
    TA=A^substLong(T);
    T=((D^rol(C,17))+Key[I&3]);
    TB=B^substLong(T);
    A=C;
    B=D;
    C=TA;
    D=TB;
  }

  BufPtr[0]=C^Key[0];
  BufPtr[1]=D^Key[1];
  BufPtr[2]=A^Key[2];
  BufPtr[3]=B^Key[3];

  UpdKeys(Buf);
}


void DecryptBlock(UBYTE *Buf)
{
  int I;
  UBYTE InBuf[16];
  UDWORD A,B,C,D,T,TA,TB;

  UDWORD *BufPtr;
  BufPtr=(UDWORD *)Buf;
  A=BufPtr[0]^Key[0];                       /* xxx may be this can be       */
  B=BufPtr[1]^Key[1];                       /* optimized in assembler       */
  C=BufPtr[2]^Key[2];
  D=BufPtr[3]^Key[3];

  memcpy(InBuf,Buf,sizeof(InBuf));
  for(I=NROUNDS-1;I>=0;I--)
  {
    T=((C+rol(D,11))^Key[I&3]);
    TA=A^substLong(T);
    T=((D^rol(C,17))+Key[I&3]);
    TB=B^substLong(T);
    A=C;
    B=D;
    C=TA;
    D=TB;
  }

  BufPtr[0]=C^Key[0];
  BufPtr[1]=D^Key[1];
  BufPtr[2]=A^Key[2];
  BufPtr[3]=B^Key[3];

  UpdKeys(InBuf);
}


void UpdKeys(UBYTE *Buf)
{
  int I;
  for (I=0;I<16;I+=4)
  {
    Key[0]^=CRCTab[Buf[I]];                 /* xxx may be I'll rewrite this */
    Key[1]^=CRCTab[Buf[I+1]];               /* in asm for speedup           */
    Key[2]^=CRCTab[Buf[I+2]];
    Key[3]^=CRCTab[Buf[I+3]];
  }
}

void SetCryptKeys(char *Password)
{
  unsigned int I,J,K,PswLength;
  unsigned char N1,N2;
  unsigned char Psw[256];

  UBYTE Ch;

  SetOldKeys(Password);

  Key[0]=0xD3A3B879L;
  Key[1]=0x3F6D12F7L;
  Key[2]=0x7515A235L;
  Key[3]=0xA4E7F123L;
  memset(Psw,0,sizeof(Psw));
  strcpy((char *)Psw,Password);
  PswLength=strlen(Password);
  memcpy(SubstTable,InitSubstTable,sizeof(SubstTable));

  for (J=0;J<256;J++)
    for (I=0;I<PswLength;I+=2)
    {
      N2=(unsigned char)CRCTab[(Psw[I+1]+J)&0xFF];
      for (K=1, N1=(unsigned char)CRCTab[(Psw[I]-J)&0xFF];
           (N1!=N2) && (N1 < 256);      /* I had to add "&& (N1 < 256)",    */
           N1++, K++)                   /* because the system crashed with  */
          {                             /* encrypted RARs                   */
             Ch=SubstTable[N1];
             SubstTable[N1]=SubstTable[(N1+I+K)&0xFF];
             SubstTable[(N1+I+K)&0xFF]=Ch;
          }
    }
  for (I=0;I<PswLength;I+=16)
    EncryptBlock(&Psw[I]);
}


void SetOldKeys(char *Password)
{
  UDWORD PswCRC;
  UBYTE Ch;
  PswCRC=CalcCRC32(0xFFFFFFFFL,(UBYTE*)Password,strlen(Password));
  OldKey[0]=(UWORD)PswCRC;
  OldKey[1]=(UWORD)(PswCRC>>16);
  OldKey[2]=OldKey[3]=0;
  PN1=PN2=PN3=0;
  while ((Ch=*Password)!=0)
  {
    PN1+=Ch;
    PN2^=Ch;
    PN3+=Ch;
    PN3=(UBYTE)rol(PN3,1);
    OldKey[2]^=((UWORD)(Ch^CRCTab[Ch]));
    OldKey[3]+=((UWORD)(Ch+(CRCTab[Ch]>>16)));
    Password++;
  }
}

void InitCRC(void)
{
  int I, J;
  UDWORD C;
  for (I=0;I<256;I++)
  {
    for (C=I,J=0;J<8;J++)
      C=(C & 1) ? (C>>1)^0xEDB88320L : (C>>1);
    CRCTab[I]=C;
  }
}


UDWORD CalcCRC32(UDWORD StartCRC,UBYTE *Addr,UDWORD Size)
{
  unsigned int I;
  for (I=0; I<Size; I++)
    StartCRC = CRCTab[(UBYTE)StartCRC ^ Addr[I]] ^ (StartCRC >> 8);
  return(StartCRC);
}


/* **************************************************************************
 ****************************************************************************
 ****************************************************************************
 ************************************************************************** */


/* end of file urarlib.c */

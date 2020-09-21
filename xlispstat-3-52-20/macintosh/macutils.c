#include "macutils.h"

static void CopyNamePart(StringPtr Name, ConstStr255Param fileName, int start)
{
  int end = fileName[0] + 1, nlen, pos;
  for (nlen = 0, pos = start; pos < end && fileName[pos] == ':'; nlen++, pos++)
    Name[nlen + 1] = ':';
  for (; pos < end && fileName[pos] != ':'; nlen++, pos++)
    Name[nlen + 1] = fileName[pos];
  Name[0] = nlen;
}

/* This function is an adaptation of the function FSpLocationFromPath in
   tclMacUtils.c in the Tcl 8.0 distribution */
OSErr FSMakeFSSpecFromPath(ConstStr255Param fileName, FSSpecPtr spec)
{
  Boolean isDir, wasAlias;
  int pos, end;
  OSErr err;
  Str255 Name;
  short vRefNum;
  long dirID;
  
  /* get the initial directory information and set up first path component */
  CopyNamePart(Name, fileName, 1);
  if (Name[0] < fileName[0] && Name[1] != ':') { /* absolute path */
    Name[0]++;
    Name[Name[0]] = ':';
    if ((err = FSMakeFSSpec(0, 0, Name, spec)) != noErr)
      return err;
    if ((err = FSpGetDirectoryID(spec, &dirID, &isDir)) != noErr)
      return err;
    if (! isDir)
      return dirNFErr;
    vRefNum = spec->vRefNum;
    pos = Name[0] + 1;
    CopyNamePart(Name, fileName, pos);
  }
  else {
    dirID = 0;
    vRefNum = 0;
    pos = 1;
    isDir = true;
  }
  
  /* process remaining path parts */
  end = fileName[0] + 1;
  while (true) {
    if ((err = FSMakeFSSpec(vRefNum, dirID, Name[0] == 0 ? NULL : Name,
                            spec)) != noErr ||
        (err = ResolveAliasFile(spec, true, &isDir, &wasAlias)) != noErr)
      return err;
    pos += Name[0];
    if (pos < end) {
      if ((err = FSpGetDirectoryID(spec, &dirID, &isDir)) != noErr)
        return err;
      if (! isDir)
        return dirNFErr;
      vRefNum = spec->vRefNum;
      CopyNamePart(Name, fileName, pos);
    }
    else
      return noErr;
  }
}


/*
 * The following functions are taken from MoreFiles. For now these are all I
 * use; if I end up using lots more I'll include the whole MoreFiles library.
 */

OSErr GetDirectoryID(short vRefNum, long dirID,
                     StringPtr name, long *theDirID, Boolean *isDirectory)
{
  CInfoPBRec pb;
  Str31 tempName;
  OSErr error;

  /* Protection against File Sharing problem */
  if ( (name == NULL) || (name[0] == 0) ) {
    tempName[0] = 0;
    pb.hFileInfo.ioNamePtr = tempName;
    pb.hFileInfo.ioFDirIndex = -1;      /* use ioDirID */
  }
  else {
    pb.hFileInfo.ioNamePtr = name;
    pb.hFileInfo.ioFDirIndex = 0;       /* use ioNamePtr and ioDirID */
  }
  pb.hFileInfo.ioVRefNum = vRefNum;
  pb.hFileInfo.ioDirID = dirID;
  error = PBGetCatInfoSync(&pb);
  *isDirectory = (pb.hFileInfo.ioFlAttrib & ioDirMask) != 0;
  *theDirID = (*isDirectory) ? pb.dirInfo.ioDrDirID : pb.hFileInfo.ioFlParID;
  return error;
}

OSErr FSpGetDirectoryID(const FSSpec *spec, long *theDirID,
                        Boolean *isDirectory)
{
  return GetDirectoryID(spec->vRefNum, spec->parID,
                       (StringPtr)spec->name, theDirID, isDirectory);
}

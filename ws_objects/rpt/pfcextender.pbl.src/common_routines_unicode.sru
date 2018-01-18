$PBExportHeader$common_routines_unicode.sru
$PBExportComments$(PB70Base) -  NVO holds common routines for Unicode
forward
global type common_routines_unicode from common_routines
end type
type os_securityattributes from structure within common_routines_unicode
end type
end forward

type os_securityattributes from structure
	unsignedlong		ul_length
	string				ch_description
	boolean			b_inherit
end type

global type common_routines_unicode from common_routines
end type
global common_routines_unicode common_routines_unicode

type prototypes
/*  OLD PowerSite System Routines for Dynamo,
	Used only for the Dynamo Wizard running in Powersite.
	Not used for Dynamo Wizard running in PB  */


FUNCTION integer 		pbeas_web_get_settings(int actions[], readonly string aKeyNames[], readonly string aKeyValues[], REF string aNames[], REF string aValues[], REF string as_error) system library "pbeas105.dll"
FUNCTION string 		pbeas_get_drive_strings() system library "pbeas105.dll"

/*  PB System Routines  - Only called from PowerBuilder  */

/* Kernel exported Routines to handle Ansi Library */
FUNCTION int			GetLibraryType (string libname) system library "pbvm105.dll" alias for "fnGetPBLBinaryType"
FUNCTION string		        ListEntries (string libname, int dirtype) system library "pbvm105.dll" alias for "fnListAnsiLibraryEntries"


/*  Miscellaneous Windows routines  */
FUNCTION boolean		CopyFileW (string lpExistingFileName, string lpNewFileName, boolean bFailIfExists) LIBRARY "kernel32.dll" alias for "CopyFileW;UNICODE"
//FUNCTION boolean		CreateDirectoryW (ref string directoryname, ref os_securityattributes secattr) library "kernel32.dll" alias for "CreateDirectoryW;UNICODE"
FUNCTION ulong			GetCurrentDirectoryW (ulong textlen, ref string dirtext) library "kernel32.dll" alias for "GetCurrentDirectoryW;UNICODE"
//FUNCTION ulong			GetDriveTypeW (string drive) library "kernel32.dll" alias for "GetDriveTypeW;UNICODE"
FUNCTION ulong			GetFileAttributesW (ref string filename) library "kernel32.dll" alias for "GetFileAttributesW;UNICODE"
end prototypes

type variables
end variables

forward prototypes
public function boolean f_DirectoryExists (string as_DirectoryName)
public function string f_getdrives ()
public function integer f_getlibrarytype (string as_libname)
public function integer f_createdirectory (string as_directoryname)
public function integer f_FileCopy (string as_SourceFile, string as_TargetFile)
public function string f_GetCurrentDirectory ()
public function string f_getwebsettingsbyname (integer ai_actions[], readonly string as_contextnames[], readonly string as_contextvalues[], ref string as_names[], ref string as_values[], ref string as_error, readonly string as_settingname)
public function string f_listentries (string as_libname, unsignedinteger ai_type)
end prototypes

public function boolean f_DirectoryExists (string as_DirectoryName);//*-----------------------------------------------------------------------------------------------------------------------*/
//*    f_DirectoryExists:  Does directory exist?
//*-----------------------------------------------------------------------------------------------------------------------*/
ULong	lul_RC

lul_RC = GetFileAttributesW ( as_DirectoryName )

// Check if 5th bit is set, if so this is a directory
If Mod ( Integer ( lul_RC / 16 ), 2 ) > 0 Then 
	Return True
Else
	Return False
End If
end function

public function string f_getdrives ();//*-----------------------------------------------------------------------------------------------------------------------*/
//*    f_GetDrives: Get Mapped Drive Information 
//*-----------------------------------------------------------------------------------------------------------------------*/
Return pbeas_get_drive_strings ( )
end function

public function integer f_getlibrarytype (string as_libname);// -----------------------------------------------------------------------------
// Get the type of entries in library (PBL)  0 - Unicode, 1 - Ansi
// -----------------------------------------------------------------------------
return GetLibraryType  ( as_libname )
end function

public function integer f_createdirectory (string as_directoryname);//*-----------------------------------------------------------------------------------------------------------------------*/
//*    f_CreateDirectory:  Create a directory
//*-----------------------------------------------------------------------------------------------------------------------*/
os_securityattributes	lstr_Security
string ls_drive, ls_dir, ls_file, ls_dirarray[], ls_builddir
int li_i, li_max

f_ParsePath ( as_DirectoryName, ls_drive, ls_dir )
f_ParseToArray ( ls_dir, is_separator, ls_dirarray )
li_max = UpperBound ( ls_dirarray )

lstr_Security.ul_Length = 7
SetNull ( lstr_Security.ch_description )	//use default security
lstr_Security.b_Inherit = False

If Right ( ls_drive, 1 ) = is_separator Then ls_drive = Left ( ls_drive, Len ( ls_drive ) - 1 )
For li_i = 1 to li_max
	ls_dir = f_RemoveSeparator ( f_AssemblePath ( ls_drive, ls_builddir + is_separator + ls_dirarray[li_i], "" ) )
	ls_builddir = ls_builddir + is_separator + ls_dirarray[li_i]
	
	If f_DirectoryExists (  ls_dir ) Then Continue
	
	If CreateDirectory ( ls_dir ) <> 1 Then
		Return -1
//	Else
//		If IsValid ( iw_Requestor ) Then iw_Requestor.f_AddNewDir ( ls_dir )
	End If
Next

Return 1
end function

public function integer f_FileCopy (string as_SourceFile, string as_TargetFile);//*-----------------------------------------------------------------------------------------------------------------------*/
//*    f_FileCopy:  Copy a File
//*-----------------------------------------------------------------------------------------------------------------------*/
FileDelete ( as_targetfile )

If CopyFileW ( as_sourcefile, as_targetfile, False ) Then
	Return 1
Else
	Return -1
End If
end function

public function string f_GetCurrentDirectory ();//*-----------------------------------------------------------------------------------------------------------------------*/
//*    f_GetCurrentDirectory:  Returns the current path
//*-----------------------------------------------------------------------------------------------------------------------*/
ulong		lul_size = 260 	/* MAX_PATH */
ulong		lul_Rc
string	ls_CurrentDir

ls_CurrentDir = Space ( lul_size )

lul_rc = GetCurrentDirectoryW ( lul_size, ls_CurrentDir )

If Right ( ls_CurrentDir, 1 ) = is_separator Then ls_CurrentDir = Left ( ls_CurrentDir, Len ( ls_CurrentDir ) - 1 )
If lul_rc > 0 Then
	Return ls_CurrentDir
Else
	Return ""
End If
end function

public function string f_getwebsettingsbyname (integer ai_actions[], readonly string as_contextnames[], readonly string as_contextvalues[], ref string as_names[], ref string as_values[], ref string as_error, readonly string as_settingname);//*-----------------------------------------------------------------------------------------------------------------------*/
//*    f_GetWebSettingsByName:  Generic Get Routines for Web Stuff
//*			Allows you to get 1 setting only
//*-----------------------------------------------------------------------------------------------------------------------*/
int li_rc, li_n, li_names

as_error = Space ( 2048 ) 
li_rc = pbeas_web_get_settings ( ai_actions, as_contextnames, as_contextvalues, as_names, as_values, as_error )
If li_rc <> 0 Then Return ""

li_names = UpperBound ( as_names )
For li_n = 1 to li_names
	If Lower ( as_names[li_n] ) = Lower ( as_settingname ) Then
		Return as_values[li_n]
	End If
Next

Return ""
end function

public function string f_listentries (string as_libname, unsignedinteger ai_type);// ----------------------------------------------------------------
// List the entry with required type (e.g. DirApplication!)
// ----------------------------------------------------------------
return ListEntries ( as_libname, ai_type )
end function

on common_routines_unicode.create
call super::create
end on

on common_routines_unicode.destroy
call super::destroy
end on

event constructor;//*-----------------------------------------------------------------*/
//*    constructor:  Setup Platform-Specific data
//*-----------------------------------------------------------------*/
/*  Get the name of the Template DLL  */
is_template = "pbgen105.dll"

is_FileType = "pbl"
is_separator = "\"
end event


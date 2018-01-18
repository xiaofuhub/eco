$PBExportHeader$common_routines_win32.sru
$PBExportComments$(PB70Base) -  NVO holds common routines for WIN32
forward
global type common_routines_win32 from common_routines
end type
type os_securityattributes from structure within common_routines_win32
end type
end forward

type os_securityattributes from structure
	unsignedlong		ul_length
	string				ch_description
	boolean			b_inherit
end type

global type common_routines_win32 from common_routines
end type
global common_routines_win32 common_routines_win32

type prototypes
FUNCTION integer		pbeas_web_get_settings(int actions[], readonly string aKeyNames[], readonly string aKeyValues[], REF string aNames[], REF string aValues[], REF string as_error) system library "pbeas105.dll"
FUNCTION string			pbeas_get_drive_strings() system library "pbeas105.dll"
/*  Miscellaneous Windows routines  */
FUNCTION boolean		CopyFileA (string lpExistingFileName, string lpNewFileName, boolean bFailIfExists) LIBRARY "kernel32.dll" Alias for "CopyFileA;ansi"
FUNCTION ulong			GetCurrentDirectoryA (ulong textlen, ref string dirtext) library "kernel32.dll" Alias for "GetCurrentDirectoryA;ansi"
FUNCTION ulong			GetFileAttributesA (ref string filename) library "kernel32.dll" Alias for "GetFileAttributesA;ansi"
/*  WEb Service routines */
end prototypes

type variables
end variables

forward prototypes
end prototypes

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

public function boolean f_DirectoryExists (string as_DirectoryName);//*-----------------------------------------------------------------------------------------------------------------------*/
//*    f_DirectoryExists:  Does directory exist?
//*-----------------------------------------------------------------------------------------------------------------------*/
ULong	lul_RC

lul_RC = GetFileAttributesA ( as_DirectoryName )

// Check if 5th bit is set, if so this is a directory
If Mod ( Integer ( lul_RC / 16 ), 2 ) > 0 Then 
	Return True
Else
	Return False
End If
end function

public function integer f_FileCopy (string as_SourceFile, string as_TargetFile);//*-----------------------------------------------------------------------------------------------------------------------*/
//*    f_FileCopy:  Copy a File
//*-----------------------------------------------------------------------------------------------------------------------*/
FileDelete ( as_targetfile )

If CopyFileA ( as_sourcefile, as_targetfile, False ) Then
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

lul_rc = GetCurrentDirectoryA ( lul_size, ls_CurrentDir )

If Right ( ls_CurrentDir, 1 ) = is_separator Then ls_CurrentDir = Left ( ls_CurrentDir, Len ( ls_CurrentDir ) - 1 )
If lul_rc > 0 Then
	Return ls_CurrentDir
Else
	Return ""
End If
end function

public function string f_GetDrives ();//*-----------------------------------------------------------------------------------------------------------------------*/
//*    f_GetDrives: Get Mapped Drive Information 
//*-----------------------------------------------------------------------------------------------------------------------*/
Return pbeas_get_drive_strings ( )
end function

public function string f_GetWebSettingsByName (int ai_actions[], readonly string as_contextnames[], readonly string as_contextvalues[], REF string as_names[], REF string as_values[],REF string as_error, readonly string as_settingname);//*-----------------------------------------------------------------------------------------------------------------------*/
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

on common_routines_win32.create
call super::create
end on

on common_routines_win32.destroy
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


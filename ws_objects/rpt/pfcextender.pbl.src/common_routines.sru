$PBExportHeader$common_routines.sru
$PBExportComments$(PB70Base) -  NVO holds common routines
forward
global type common_routines from nonvisualobject
end type
end forward

global type common_routines from nonvisualobject
end type
global common_routines common_routines

type variables
base_constants inv_const
/*  Requesting Object */
//base_wizard iw_Requestor
/*  PB Library Extension  */
string 	is_FileType
/*  File Separator  */
string 	is_separator
/*  The name of the template dll  */
string 	is_template

/*  PB Constants  */
string 	is_PBEXE
string 	is_PBUserRegKey, is_PBMachineRegKey
int 	 	ii_PBVers, ii_PBVersMinor

/*  Save Information on Target File  */
string is_TargetFile
string is_Application, is_AppLibrary, is_LibraryList[]


end variables

forward prototypes
public function integer f_FileCopy (string as_SourceFile, string as_TargetFile)
public function integer f_RemoveDirectory (string as_directoryname)
public function integer f_createdirectory (string as_directoryname)
public function string f_GetDrives ()
public function integer f_getlibrarytype (string as_libname)
public function boolean f_DirectoryExists (string as_DirectoryName)
public function string f_AssemblePath (readonly string as_dirpath, readonly string as_filename)
public function string f_AssemblePath (readonly string as_drive, readonly string as_dirpath, readonly string as_filename)
public function string f_AssemblePath (readonly string as_drive, readonly string as_dirpath, readonly string as_filename, readonly string as_ext)
protected function string f_BuildPath (string as_drive, string as_dirarray[], string as_libin)
public function string f_GetCurrentDirectory ()
protected function integer f_GetLibsFromTarget (ref string as_appname, ref string as_applib, ref string as_liblist[])
public function string f_GetTargetAppLib ()
public function string f_GetTargetApplication ()
public function integer f_GetTargetLibList (ref string as_liblist[])
public function string f_GetToken (ref string as_source, readonly string as_separator)
public function string f_GetUserRegKey ()
public function string f_GetWorkspaceDirectory ()
public function string  f_listentries (string as_libname, unsignedinteger ai_type)
public function integer f_SetTargetFile (string as_targetfile)
public function integer f_SetPBConstants (readonly string as_EXEName, readonly integer ai_version, readonly integer ai_minorvers, readonly string as_UserKey, readonly string as_MachineKey)
public function string f_RemoveSeparator (string as_name)
public function long f_ParseToArray (string as_source, string as_delimiter, ref string as_rtnarray[])
public function integer f_ParsePath (string as_path, ref string as_drive, ref string as_dirpath, ref string as_filename, ref string as_ext)
public function integer f_ParsePath (string as_path, ref string as_drive, ref string as_dirpath, ref string as_filename)
public function integer f_ParsePath (string as_path, ref string as_drive, ref string as_dirpath)
public function integer f_LastPos (string as_source, string as_target, long al_start)
public function string f_GetWebSettingsByName (int ai_actions[], readonly string as_contextnames[], readonly string as_contextvalues[], REF string as_names[], REF string as_values[],REF string as_error, readonly string as_settingname)
public function string f_GlobalReplace (string as_source, string as_old, readonly string as_new, readonly boolean ab_ignorecase)
end prototypes

public function string f_AssemblePath (readonly string as_dirpath, readonly string as_filename);//*-----------------------------------------------------------------------------------------------------------------------*/
//*    f_AssemblePath:  Assemble a path and filename from its parts
//*-----------------------------------------------------------------------------------------------------------------------*/
string ls_Path

/*  Set the Drive and Path  */
ls_Path = f_RemoveSeparator ( Trim ( as_DirPath ) )

/*  Add the filename  */
ls_Path = ls_Path + is_Separator + f_RemoveSeparator ( Trim ( as_FileName ) )

/*  Return the assembled path  */
Return f_RemoveSeparator ( ls_Path )
end function

public function string f_AssemblePath (readonly string as_drive, readonly string as_dirpath, readonly string as_filename);//*-----------------------------------------------------------------------------------------------------------------------*/
//*    f_AssemblePath:  Assemble a path and filename from its parts
//*-----------------------------------------------------------------------------------------------------------------------*/
string ls_Drive, ls_DirPath, ls_Path

ls_Drive = f_RemoveSeparator ( Trim ( as_Drive ) )

ls_DirPath = f_RemoveSeparator ( Trim ( as_DirPath ) )

/*  Set the Drive and Path  */
ls_Path = Trim ( ls_Drive ) + is_Separator + Trim ( ls_DirPath )

/*  Add the filename  */
ls_Path = f_RemoveSeparator ( ls_Path ) + is_Separator + f_RemoveSeparator ( Trim ( as_FileName ) )

/*  Return the assembled path  */
Return ls_Path
end function

public function string f_AssemblePath (readonly string as_drive, readonly string as_dirpath, readonly string as_filename, readonly string as_ext);//*-----------------------------------------------------------------------------------------------------------------------*/
//*    f_AssemblePath:  Assemble a path and filename from its parts
//*						 (with the file extension separated)
//*-----------------------------------------------------------------------------------------------------------------------*/
string ls_Drive, ls_DirPath, ls_Path

ls_Drive = f_RemoveSeparator ( Trim ( as_Drive ) )

ls_DirPath = f_RemoveSeparator ( Trim ( as_DirPath ) )

/*  Set the Drive and Path  */
ls_Path = Trim (ls_Drive ) + is_Separator + Trim ( ls_DirPath )

/*  Add the filename  */
ls_Path = f_RemoveSeparator ( ls_Path ) + is_Separator + f_RemoveSeparator ( Trim ( as_FileName ) )

/*  Add the Extension  */
If Trim ( as_Ext ) <> "" Then
	ls_Path = ls_Path + "." + f_RemoveSeparator ( Trim ( as_Ext ) )
End if

/*  Return the assembled path  */
Return ls_Path
end function

protected function string f_BuildPath (string as_drive, string as_dirarray[], string as_libin);//*-----------------------------------------------------------------------------------------------------------------------*/
//*    f_BuildPath:  Convert the relative path ( ie: ..\\..\\temp.my.pbl )
//*					to a absolute path based on the target folders 
//*-----------------------------------------------------------------------------------------------------------------------*/
string ls_drive, ls_tempdir, ls_file, ls_sep, ls_library, ls_dir
long ll_pos, ll_cnt, ll_d, ll_dirs

ls_sep = "\\"

/*  Parse the relative path  */
f_ParsePath ( as_libin, ls_drive, ls_tempdir, ls_file ) 
If ls_drive > "" Then 
	/*  If there is a drive, then it is not a relative path  */
	ls_library = f_GlobalReplace ( as_libin, ls_sep, "\", True )
	Return ls_library
End If

/*  Find the ".." to determine the level of folders  */
ll_pos = Pos ( as_libin, ".." + ls_sep ) 
Do While ll_pos = 1
	ll_cnt++
	as_libin = Mid ( as_libin, ll_pos + 4 ) 
	ll_pos = Pos ( as_libin, ".." + ls_sep ) 
Loop

/*  Now adjust the folder count by the number of ".." found  */
ll_dirs = UpperBound ( as_dirarray )
ll_dirs = ll_dirs - ll_cnt

/*  Build the folder path */
For ll_d =1 to ll_dirs
	ls_dir = ls_dir + as_dirarray[ll_d] + "\"
Next

/*  Reconstruct the library path  */
ls_library = f_AssemblePath ( as_drive, ls_dir, as_libin ) 
ls_library = f_GlobalReplace ( ls_library, ls_sep, "\", True )

Return ls_library
end function

public function string f_GetCurrentDirectory ();//*-----------------------------------------------------------------------------------------------------------------------*/
//*    f_GetCurrentDirectory:  Returns the current path
//*-----------------------------------------------------------------------------------------------------------------------*/
Return ""
end function

protected function integer f_GetLibsFromTarget (ref string as_appname, ref string as_applib, ref string as_liblist[]);//*-----------------------------------------------------------------------------------------------------------------------*/
//*    f_GetLibsFromTarget:  Get the library information from the target file
//*-----------------------------------------------------------------------------------------------------------------------*/
string ls_drive, ls_dir, ls_file, ls_dirs[]
string ls_line, ls_liblist
long ll_FileNo, ll_rc, ll_l, ll_libs
long ll_pos, ll_pos2, ll_pos3

/* Open the target for read  */
ll_FileNo = FileOpen ( is_targetfile, LineMode!, Read! )
If ll_FileNo < 0 Then Return -1

/*  Read the target file extracting information  */
ll_rc = FileRead ( ll_FileNo, ls_line ) 
Do While ll_rc > 0 
	/*  Look for appname  */
	ll_pos = Pos ( Lower ( ls_line ), "appname" )
	If ll_pos = 1 Then
		ll_pos2 = Pos ( ls_line, "~"" ) 
		If ll_pos2 > 0 Then
			ls_line = Mid ( ls_line, ll_pos2 + 1 )
			ll_pos3 = Pos ( ls_line, "~"" ) 
			If ll_pos3 > 0 Then 
				as_appname = Left ( ls_line, ll_pos3 - 1 )
			End If
		End If
		ll_rc = FileRead (ll_FileNo, ls_line ) 
		Continue
	End If
	/*  Look for applib  */
	ll_pos = Pos ( Lower ( ls_line ), "applib" )
	If ll_pos = 1 Then
		ll_pos2 = Pos ( ls_line, "~"" ) 
		If ll_pos2 > 0 Then
			ls_line = Mid ( ls_line, ll_pos2 + 1 )
			ll_pos3 = Pos ( ls_line, "~"" ) 
			If ll_pos3 > 0 Then 
				as_applib = Left ( ls_line, ll_pos3 - 1 )
			End If
		End If
		ll_rc = FileRead ( ll_FileNo, ls_line ) 
		Continue
	End If
	/*  Look for liblist */
	ll_pos = Pos ( Lower ( ls_line ), "liblist" )
	If ll_pos = 1 Then
		ll_pos2 = Pos ( ls_line, "~"" ) 
		If ll_pos2 > 0 Then
			ls_line = Mid ( ls_line, ll_pos2 + 1 )
			ll_pos3 = Pos ( ls_line, "~"" ) 
			If ll_pos3 > 0 Then 
				ls_liblist = Left ( ls_line, ll_pos3 - 1 )
			End If
		End If
		ll_rc = FileRead ( ll_FileNo, ls_line ) 
		Continue
	End If
	ll_rc = FileRead (ll_FileNo, ls_line ) 
Loop

FileClose ( ll_FileNo)

/*  Parse the library list to an array */
ll_libs = f_ParseToArray ( ls_liblist, ";", as_liblist )

/*  Get the path of the target file and create an array of its folders  */
f_ParsePath ( is_targetfile, ls_drive, ls_dir, ls_file )
f_ParseToArray ( ls_dir, "\", ls_dirs )

/*  Parse the relative path to a full path  */
as_applib = f_BuildPath ( ls_drive, ls_dirs, as_applib ) 

/*  For each of the libraries, Parse the relative path to a full path  */
For ll_l = 1 to ll_libs
	as_liblist[ll_l] = f_BuildPath ( ls_drive, ls_dirs, as_liblist[ll_l] ) 
Next

Return 1
end function

public function string f_GetTargetAppLib ();//*-------------------------------------------------------------------
//*  	f_GetTargetAppLib: Return the application library for target
//*-------------------------------------------------------------------
Return is_AppLibrary
end function

public function string f_GetTargetApplication ();//*-------------------------------------------------------------------
//*  	f_GetTargetApplication:	Return the application for target
//*-------------------------------------------------------------------
Return is_Application
end function

public function integer f_GetTargetLibList (ref string as_liblist[]);//*-------------------------------------------------------------------
//*   f_GetTargetLibList: Get the Library List for target
//*-------------------------------------------------------------------
as_liblist = is_librarylist

Return UpperBound ( as_liblist ) 
end function

public function string f_GetToken (ref string as_source, readonly string as_separator);//*-----------------------------------------------------------------------------------------------------------------------*/
//*    f_GetToken:  Get token of left of string.
//*					  Return string minus token	
//*-----------------------------------------------------------------------------------------------------------------------*/
int li_pos
string 	ls_ret

/*  Check parameters  */
If IsNull ( as_source ) or IsNull ( as_separator ) Then
	string ls_null
	SetNull ( ls_null )
	Return ls_null
End If

/*  Get the position of the separator  */
li_pos = Pos ( as_source, as_separator )	

/*  If no separator, the token to be stripped is the entire source string  */
If li_pos = 0 Then
	ls_ret = as_source
	as_source = ""	
Else
	/*  Otherwise, return just the token and strip it & 
		 the separator from the source string  */
	ls_ret = Mid ( as_source, 1, li_pos - 1 )
	as_source = Right ( as_source, Len ( as_source ) - ( li_pos + Len ( as_separator ) -1 ) )
End If

Return ls_ret
end function

public function string f_GetUserRegKey ();//*-----------------------------------------------------------------------------------------------------------------------*/
//*    f_GetUserRegKey:  Get the HKEY_CURRENT_USER Registry Key
//*-----------------------------------------------------------------------------------------------------------------------*/
Return is_PBUserRegKey
end function

public function string f_GetWorkspaceDirectory ();//*-----------------------------------------------------------------------------------------------------------------------*/
//*    f_GetWorkspaceDirectory:  Get the workspace directory
//*-----------------------------------------------------------------------------------------------------------------------*/
string ls_names[], ls_values[], ls_contextnames[], ls_contextvalues[], ls_init[]
string ls_workspace, ls_error
int li_work[], li_rc

li_work[1] = inv_const.II_CURRENTWORKSPACE    
ls_workspace = f_GetWebSettingsByName ( li_work, ls_contextnames, ls_contextvalues, ls_names, ls_values, ls_error, "Directory" ) 
If Trim ( ls_workspace ) = "" Then ls_error = "Unknown Error getting Workspace Directory."
ls_error = "Get Current Workspace failed.  Cause is " + ls_error
If ls_workspace = "" Then
	MessageBox ( "Dev. Time Coding Error", ls_error ) 
	Return ""
ElseIf li_rc < 0 Then
	MessageBox ( "Get Current Workspace Error", ls_error ) 
	Return ""
End If

If Right ( ls_workspace, 1 ) = is_separator Then ls_workspace = Left ( ls_workspace, Len ( ls_workspace ) - 1 ) 

Return ls_workspace
end function

public function string  f_listentries (string as_libname, unsignedinteger ai_type);// ----------------------------------------------------------------
// List the entry with required type (e.g. DirApplication!)
// ----------------------------------------------------------------
return ""
end function

public function integer f_SetTargetFile (string as_targetfile);//*-------------------------------------------------------------------
//*  	f_SetTargetFile:  Set the Target File 
//*-------------------------------------------------------------------
is_targetfile = as_targetfile

Return f_GetLibsFromTarget ( is_application, is_applibrary, is_librarylist )
end function

public function integer f_SetPBConstants (readonly string as_EXEName, readonly integer ai_version, readonly integer ai_minorvers, readonly string as_UserKey, readonly string as_MachineKey);//*-----------------------------------------------------------------------------------------------------------------------*/
//*    f_SetPBConstants:  Set the PB Values that are constant in this application
//*-----------------------------------------------------------------------------------------------------------------------*/
is_PBEXE						= as_EXEName
is_PBUserRegKey				= as_UserKey
is_PBMachineRegKey			= as_MachineKey
ii_PBVers						= ai_version
ii_PBVersMinor					= ai_minorvers
Return 1
end function

public function string f_RemoveSeparator (string as_name);//*-----------------------------------------------------------------------------------------------------------------------*/
//*    f_RemoveSeparator:  Return name without Separator at end
//*-----------------------------------------------------------------------------------------------------------------------*/
If Right ( as_name, 1 ) = is_Separator Then as_name = Left ( as_name, Len ( as_name ) - 1 )
If Left ( as_name, 1 ) = is_Separator Then as_name = Right ( as_name, Len ( as_name ) - 1 )

Return as_name
end function

public function long f_ParseToArray (string as_source, string as_delimiter, ref string as_rtnarray[]);//*-----------------------------------------------------------------------------------------------------------------------*/
//*    f_ParseToArray:  Parse a delimited string into an array of strings
//*-----------------------------------------------------------------------------------------------------------------------*/
long ll_DelLen, ll_pos, ll_Count, ll_Start, ll_Length
string ls_holder

If IsNull ( as_source ) Or IsNull ( as_delimiter ) Then
	long ll_null
	SetNull ( ll_null )
	Return ll_null
End If

/*  Check for at least one entry  */
If Trim ( as_source ) = "" Then Return 0

/*  Get the length of the delimiter  */
ll_DelLen = Len ( as_Delimiter )

ll_pos =  Pos ( Upper ( as_source ), Upper ( as_Delimiter ) )
/*  Only one entry was found  */
If ll_pos = 0 Then
	as_RtnArray[1] = as_source
	Return 1
End If

/*  More than one entry was found - loop to get all of them  */
ll_Count = 0
ll_Start = 1

Do While ll_pos > 0
	/*  Set current entry  */
	ll_Length = ll_pos - ll_Start
	ls_holder = Mid ( as_source, ll_start, ll_length )

	/*  Update array and counter  */
	ll_Count ++
	as_RtnArray[ll_Count] = ls_holder
	
	/*  Set the new starting position  */
	ll_Start = ll_pos + ll_DelLen

	ll_pos =  Pos ( Upper ( as_source ), Upper ( as_Delimiter ), ll_Start )
Loop

/*  Set last entry  */
ls_holder = Mid ( as_source, ll_start, Len ( as_source ) )

/*  Update array and counter if necessary  */
If Trim ( ls_holder ) > "" Then
	ll_Count ++
	as_RtnArray[ll_Count] = ls_holder
End If

/*  Return the number of entries found  */
Return ll_Count
end function

public function integer f_ParsePath (string as_path, ref string as_drive, ref string as_dirpath, ref string as_filename, ref string as_ext);//*-----------------------------------------------------------------------------------------------------------------------*/
//*    f_ParsePath:  Parse a path and filename into its components
//*-----------------------------------------------------------------------------------------------------------------------*/
int	li_pos
string ls_File

If IsNull ( as_path ) or Len ( Trim ( as_path ) ) = 0 Then Return -1

/*  Get the Drive  */
If Left ( as_path, 2 ) = "\\" Then
	/*  Look for UNC Coding Convention  */
	li_pos = Pos ( as_path, is_separator, 3 ) 
	If li_pos > 0 Then
		/*  Found the first one  */
		li_pos = Pos ( as_path, is_separator, li_pos + 1 ) 
		If li_pos > 0 Then
			as_drive = Left ( as_path, li_pos - 1 )
			as_path = Mid ( as_path, li_pos + 1 ) 
		End If
	Else
		Return -1
	End If
Else
	li_pos = Pos ( as_Path, ":" )
	If li_pos = 0 Then
		as_Drive = ""
	Else
		If Mid ( as_Path, ( li_pos + 1 ), 1 ) = is_Separator Then
			li_pos ++
		End if
	as_Drive = Left ( as_Path, li_pos )
	as_Path = Right ( as_Path, ( Len ( as_Path ) - li_pos ) )
	End if
End If

/*  Get the file name and extension  */
li_pos = f_LastPos ( as_Path, is_Separator, 0 )
ls_File = Right ( as_Path, ( Len ( as_Path ) - li_pos ) )
as_Path = Left ( as_Path, li_pos )

If IsNull ( as_Ext ) Then
	as_FileName = ls_File
	as_Ext = ""
Else
	/*  Get the extension  */
	li_pos = f_LastPos ( ls_File, ".", 0 )
	If li_pos > 0 Then
		as_FileName = Left ( ls_File, ( li_pos - 1 ) )
		as_Ext = Right ( ls_File, ( Len ( ls_File ) - li_pos ) )
	Else
		as_FileName = ls_File
		as_Ext = ""
	End if
End If

/*  Everything left is the directory path  */
as_DirPath = as_Path

/*  Make sure there is no separator at end  */
If Right ( as_DirPath, 1 ) = is_Separator Then as_DirPath = Left ( as_DirPath, Len ( as_DirPath ) - 1 )

Return 1
end function

public function integer f_ParsePath (string as_path, ref string as_drive, ref string as_dirpath, ref string as_filename);//*-----------------------------------------------------------------------------------------------------------------------*/
//*    f_ParsePath:  Parse a path and filename into its components
//*-----------------------------------------------------------------------------------------------------------------------*/
int	li_pos
string ls_File

If IsNull ( as_path ) or Len ( Trim ( as_path ) ) = 0 Then Return -1

/*  Get the Drive  */
If Left ( as_path, 2 ) = "\\" Then
	/*  Look for UNC Coding Convention  */
	li_pos = Pos ( as_path, is_separator, 3 ) 
	If li_pos > 0 Then
		/*  Found the first one  */
		li_pos = Pos ( as_path, is_separator, li_pos + 1 ) 
		If li_pos > 0 Then
			as_drive = Left ( as_path, li_pos - 1 )
			as_path = Mid ( as_path, li_pos + 1 ) 
		End If
	Else
		Return -1
	End If
Else
	/*  Get the Drive  */

	li_pos = Pos ( as_Path, ":" )
	If li_pos = 0 Then
		as_Drive = ""
	Else
		If Mid ( as_Path, ( li_pos + 1 ), 1 ) = is_Separator Then
			li_pos ++
		End if
	
		as_Drive = Left ( as_Path, li_pos )
		as_Path = Right ( as_Path, ( Len ( as_Path ) - li_pos ) )
	End if
End If

/*  Get the file name and extension  */
li_pos = f_LastPos ( as_Path, is_Separator, 0 )
ls_File = Right ( as_Path, ( Len ( as_Path ) - li_pos ) )
as_Path = Left ( as_Path, li_pos )

as_FileName = ls_File

/*  Everything left is the directory path  */
as_DirPath = as_Path

/*  Make sure there is no separator at end  */
If Right ( as_DirPath, 1 ) = is_Separator Then as_DirPath = Left ( as_DirPath, Len ( as_DirPath ) - 1 )

Return 1
end function

public function integer f_ParsePath (string as_path, ref string as_drive, ref string as_dirpath);//*-----------------------------------------------------------------------------------------------------------------------*/
//*    f_ParsePath:  Parse a path into its components
//*-----------------------------------------------------------------------------------------------------------------------*/
int	li_pos
string ls_File

If IsNull ( as_path ) or Len ( Trim ( as_path ) ) = 0 Then Return -1

/*  Get the Drive  */
If Left ( as_path, 2 ) = "\\" Then
	/*  Look for UNC Coding Convention  */
	li_pos = Pos ( as_path, is_separator, 3 ) 
	If li_pos > 0 Then
		/*  Found the first one  */
		li_pos = Pos ( as_path, is_separator, li_pos + 1 ) 
		If li_pos > 0 Then
			as_drive = Left ( as_path, li_pos - 1 )
			as_path = Mid ( as_path, li_pos + 1 ) 
		End If
	Else
		Return -1
	End If
Else
	/*  Get the Drive  */
	li_pos = Pos ( as_Path, ":" )
	If li_pos = 0 Then
		as_Drive = ""
	Else
		If Mid ( as_Path, ( li_pos + 1 ), 1 ) = is_Separator Then
			li_pos ++
		End if
	
		as_Drive = Left ( as_Path, li_pos )
		as_Path = Right ( as_Path, ( Len ( as_Path ) - li_pos ) )
	End if
End If

/*  Get the file name and extension  */
li_pos = f_LastPos ( as_Path, is_Separator, 0 )
ls_File = Right ( as_Path, ( Len ( as_Path ) - li_pos ) )
If Pos ( ls_File, "." ) > 0 Then as_Path = Left ( as_Path, li_pos )

/*  Everything left is the directory path  */
as_DirPath = as_Path

Return 1
end function

public function integer f_LastPos (string as_source, string as_target, long al_start);//*-----------------------------------------------------------------------------------------------------------------------*/
//*    f_LastPos:  Pos function in reverse
//*-----------------------------------------------------------------------------------------------------------------------*/
long ll_cnt, ll_pos

/*  Check for Null Parameters  */
If IsNull ( as_source ) Or IsNull ( as_target ) Or IsNull ( al_start ) Then
	SetNull ( ll_cnt )
	Return ll_cnt
End If

/*  Check for an empty string  */
If Len ( as_Source ) = 0 Then Return 0

/*  Check for the starting position, 0 means start at the end  */
If al_start=0 Then al_start = Len ( as_Source )

/*  Perform find  */
For ll_cnt = al_start to 1 Step -1
	ll_pos = Pos ( as_Source, as_Target, ll_cnt )
	If ll_pos = ll_cnt Then 
		/*  String was found  */
		Return ll_cnt
	End If
Next

/*  String was not found  */
Return 0
end function

public function string f_GlobalReplace (string as_source, string as_old, readonly string as_new, readonly boolean ab_ignorecase);//*-----------------------------------------------------------------------------------------------------------------------*/
//*    f_GlobalReplace:  Replace all occurences of string in string
//*-----------------------------------------------------------------------------------------------------------------------*/
long	ll_Start, ll_OldLen, ll_NewLen
string ls_Source

/*  Get the string lengths  */
ll_OldLen = Len ( as_Old )
ll_NewLen = Len ( as_New )

/*  Should function respect case  */
If ab_ignorecase Then
	as_old = Lower ( as_old )
	ls_source = Lower ( as_source )
Else
	ls_source = as_source
End If

/*  Search for the first occurrence of as_Old  */
ll_Start = Pos ( ls_Source, as_Old )

Do While ll_Start > 0
	/*  Replace as_Old with as_New  */
	as_Source = Replace ( as_Source, ll_Start, ll_OldLen, as_New )
	
	/*  Should function respect case  */
	If ab_ignorecase Then 
		ls_source = Lower ( as_source )
	Else
		ls_source = as_source
	End If
	
	/*  Find the next occurrence of as_Old  */
	ll_Start = Pos ( ls_Source, as_Old, ( ll_Start + ll_NewLen ) )
Loop

Return as_Source
end function

on common_routines.create
call super::create
TriggerEvent( this, "constructor" )
end on

on common_routines.destroy
TriggerEvent( this, "destructor" )
call super::destroy
end on

event constructor;//*-----------------------------------------------------------------*/
//*    constructor:  Setup Platform-Specific data
//*-----------------------------------------------------------------*/
end event


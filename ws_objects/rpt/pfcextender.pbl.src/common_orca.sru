$PBExportHeader$common_orca.sru
$PBExportComments$(PB70Base) - Common ORCA Routines
forward
global type common_orca from nonvisualobject
end type
end forward

global type common_orca from nonvisualobject
end type
global common_orca common_orca

type prototypes

end prototypes

type variables
Protected :
/*  Current Application Name  */
string is_ApplicationName
/*  Current Application Library Name  */
string is_ApplicationLibrary
/*  Current Application Library Search Path  */
string is_LibraryList[]
/*  Has Object Info been loaded?  */
boolean ib_CompleteObjInfoLoaded

Public:
/*  The current index to the loaded objects  */
int ii_CurrentObject
/*  The current library being processed  */
string is_CurrentLibrary
/*  Handle to current object  */
unsignedlong iul_CurObj

/*  CONSTANTS  */
/*  PowerBuilder Object Types. In the PBORCA.H file,  */ 
/*  it is defined as an enum. So, the index starts from 0.  */
CONSTANT int II_APPLICATION = 0 
CONSTANT int II_DATAWINDOW = 1
CONSTANT int II_FUNCTION = 2 
CONSTANT int II_MENU = 3 
CONSTANT int II_QUERY = 4 
CONSTANT int II_STRUCTURE = 5 
CONSTANT int II_USEROBJECT = 6 
CONSTANT int II_WINDOW = 7 
CONSTANT int II_PIPELINE = 8 
CONSTANT int II_PROJECT = 9 
CONSTANT int II_PROXYOBJECT = 10 
end variables

forward prototypes
public function string of_GetAppLibName ()
public function string of_GetApplName ()
public function integer of_getlibrarycomments (readonly string as_library, ref string as_libcomments)
public subroutine of_pbunloadobject (unsignedlong aul_hobjbuf)
public function integer of_enumlibraryobjects (string as_library)
public function unsignedlong of_pbloadobject (string as_library, string as_object, integer ai_objecttype)
public function integer of_reset (readonly boolean ab_state)
public function long of_getsourcesize (readonly unsignedlong aul_hobjbuf)
public function integer of_setapplication (readonly string as_appname, readonly string as_applibrary, readonly string as_liblist[])
public function integer of_pbimportobject (unsignedlong aul_hobjbuf, string as_library, string as_object, integer ai_objecttype, readonly string as_comment, ref string as_errormsg)
public function integer of_gethierarchy (readonly string as_library, readonly string as_object, readonly integer ai_objtype, ref string as_userdata, ref string as_ancestors[])
public function unsignedlong of_replacename (readonly unsignedlong aul_objbuf, readonly long al_start, readonly string as_oldname, readonly string as_newname)
public subroutine of_getsource (readonly unsignedlong aul_objbuf, ref string as_source)
public function long of_findgeneric (readonly unsignedlong aul_objbuf, readonly long al_start, readonly string as_search, readonly long al_size, ref string as_line, readonly integer ai_case)
public function integer of_setlibrarylist (readonly string as_libraries[])
public function integer of_createlibrary (string as_library, string as_libcomments)
public function integer of_startorca ()
public function integer of_stoporca ()
public function integer of_listinheritables (ref s_objectinfo astr_objects[], readonly string as_liblist[])
public function integer of_listinheritables (ref s_objectinfo astr_objects[])
end prototypes

public function string of_GetAppLibName ();//*-------------------------------------------------------------------
//*  Function:			of_GetAppLibName 
//*  Purpose:			Returns the name of the application library
//*-------------------------------------------------------------------
Return is_ApplicationLibrary
end function

public function string of_GetApplName ();//*-------------------------------------------------------------------
//*  Function:			of_GetApplName 
//*  Purpose:			Returns the name of the application object
//*-------------------------------------------------------------------
Return is_ApplicationName
end function

public function integer of_getlibrarycomments (readonly string as_library, ref string as_libcomments);//*-------------------------------------------------------------------
//*  Function:			of_GetLibraryComments 
//*  Purpose:			Returns comments associated with the library
//*-------------------------------------------------------------------
as_libcomments = ""
Return -1
end function

public subroutine of_pbunloadobject (unsignedlong aul_hobjbuf);
end subroutine

public function integer of_enumlibraryobjects (string as_library);//*-------------------------------------------------------------------
//*  Function:			of_EnumLibraryObjects 
//*  Purpose:			Enumerate the library objects,
//*						stored in the DLL memory
//*-------------------------------------------------------------------
Return -1
end function

public function unsignedlong of_pbloadobject (string as_library, string as_object, integer ai_objecttype);//*-------------------------------------------------------------------
//*  Function:			of_PBLoadObject 
//*  Purpose:			Loads object into memory for ORCA,
//*						returns handle to object
//*-------------------------------------------------------------------
Return -1
end function

public function integer of_reset (readonly boolean ab_state);//*-------------------------------------------------------------------
//*  Function:			of_Reset 
//*  Purpose:			Sets indicator that application has been set
//*-------------------------------------------------------------------
Return -1
end function

public function long of_getsourcesize (readonly unsignedlong aul_hobjbuf);//*-------------------------------------------------------------------
//*  Function:			of_GetSourceSize 
//*  Purpose:			Returns the size of the object source 
//*-------------------------------------------------------------------
Return -1
end function

public function integer of_setapplication (readonly string as_appname, readonly string as_applibrary, readonly string as_liblist[]);//*-------------------------------------------------------------------
//*  Function:			of_SetApplication 
//*  Purpose:			Initialize the application object & path
//*-------------------------------------------------------------------
/*  Initialize the application in ORCA  */
Return -1
end function

public function integer of_pbimportobject (unsignedlong aul_hobjbuf, string as_library, string as_object, integer ai_objecttype, readonly string as_comment, ref string as_errormsg);Return -1
end function

public function integer of_gethierarchy (readonly string as_library, readonly string as_object, readonly integer ai_objtype, ref string as_userdata, ref string as_ancestors[]);//*-------------------------------------------------------------------
//*  Function:			of_GetHierarchy 
//*  Purpose:			Obtains an array reference to an object's
//*						ancestors (Holds up to 10 ancestors)
//*-------------------------------------------------------------------
Return 0
end function

public function unsignedlong of_replacename (readonly unsignedlong aul_objbuf, readonly long al_start, readonly string as_oldname, readonly string as_newname);Return -1
end function

public subroutine of_getsource (readonly unsignedlong aul_objbuf, ref string as_source);/*  For DEBUGGING Purposes  */

end subroutine

public function long of_findgeneric (readonly unsignedlong aul_objbuf, readonly long al_start, readonly string as_search, readonly long al_size, ref string as_line, readonly integer ai_case);Return -1
end function

public function integer of_setlibrarylist (readonly string as_libraries[]);//*-------------------------------------------------------------------
//*  Function:			of_SetLibraryList
//*  Purpose:			Set the list of libraries.  To be used
//* 						when not setting application
//*-------------------------------------------------------------------
is_LibraryList = as_libraries

Return UpperBound ( is_LibraryList ) 
end function

public function integer of_createlibrary (string as_library, string as_libcomments);//*-------------------------------------------------------------------
//*  Function:			of_CreateLibrary 
//*  Purpose:			Creates a new PowerBuilder Library
//*-------------------------------------------------------------------
Return -1
end function

public function integer of_startorca ();Return -1
end function

public function integer of_stoporca ();Return -1
end function

public function integer of_listinheritables (ref s_objectinfo astr_objects[], readonly string as_liblist[]);Return -1
end function

public function integer of_listinheritables (ref s_objectinfo astr_objects[]);Return -1
end function

on common_orca.create
call super::create
TriggerEvent( this, "constructor" )
end on

on common_orca.destroy
TriggerEvent( this, "destructor" )
call super::destroy
end on


$PBExportHeader$common_orca_unicode.sru
$PBExportComments$(PB70Base) - Common ORCA Routines for Unicode
forward
global type common_orca_unicode from common_orca
end type
end forward

global type common_orca_unicode from common_orca
end type
global common_orca_unicode common_orca_unicode

type prototypes
/*  ORCA Functions in PBDEV */
FUNCTION long	CP_CreateLibrary (string sLibrary, string sComments)  LIBRARY "pbdev105.dll" alias for "CP_CreateLibrary;Unicode"
FUNCTION int	CP_ImportObject (ulong hObjBuf, string sLibrary, string sObject, int iObjectType, string sLangComment,  REF string sErr[10])  LIBRARY "pbdev105.dll" alias for "CP_ImportObject;Unicode"
FUNCTION int	CP_InitAppl (string sLibraryArr[], int iNoLibs, string sApplName, string sApplLibName)  LIBRARY "pbdev105.dll" alias for "CP_InitAppl;Unicode"
FUNCTION ulong	CP_LoadObject (string sLibrary, string sObject, int iObjectType)  LIBRARY "pbdev105.dll" alias for "CP_LoadObject;Unicode"
SUBROUTINE	CP_UnloadObject (ulong hObjBuf)  LIBRARY "pbdev105.dll"
FUNCTION int	CP_GetObjectHierarchy( string sLibrary, string sObject, int iEntryType, REF string sUserData, REF string sAnc[])  LIBRARY "pbdev105.dll" alias for "CP_GetObjectHierarchy;Unicode"
FUNCTION long	CP_GetSourceSize(ulong hObjBuf)  LIBRARY "pbdev105.dll"
FUNCTION ulong	CP_ReplaceName ( ulong hObjBuf, long lstart, int inoChar, string sReplace )  LIBRARY "pbdev105.dll" Alias for "CP_ReplaceName;Unicode"
FUNCTION long	CP_FindGeneric ( ulong hObjBuf, long lstart, string sName, long lsize, int iCase )   LIBRARY "pbdev105.dll" Alias for "CP_FindGeneric;Unicode"
SUBROUTINE	CP_GetSource ( ulong hObjBuf, REF string sSource)   LIBRARY "pbdev105.dll" Alias for "CP_GetSource;Unicode"
FUNCTION int	CP_StartOrca ()  LIBRARY "pbdev105.dll"
FUNCTION int	CP_StopOrca ()  LIBRARY "pbdev105.dll"
// new external function for CR337000
FUNCTION int	CP_SetLibraryList (string sLibraryArr[], int iNoLibs) LIBRARY "pbdev105.dll" Alias for "CP_SetLibraryList;Unicode"
end prototypes

type variables
datastore ids_objects
end variables

forward prototypes
public function integer of_CreateLibrary (string as_Library, string as_LibComments)
public function integer of_getlibrarycomments (readonly string as_library, ref string as_libcomments)
public subroutine of_PBUnloadObject (unsignedlong aul_hObjBuf)
public function integer of_EnumLibraryObjects (string as_Library)
public function unsignedlong of_PBLoadObject (string as_Library, string as_Object, integer ai_ObjectType)
public function integer of_reset (readonly boolean ab_state)
public function long of_GetSourceSize (readonly unsignedlong aul_hObjBuf)
public function integer of_pbimportobject (unsignedlong aul_hobjbuf, string as_library, string as_object, integer ai_objecttype, readonly string as_comment, ref string as_errormsg)
public function integer of_gethierarchy (readonly string as_library, readonly string as_object, readonly integer ai_objtype, ref string as_userdata, ref string as_ancestors[])
public function unsignedlong of_replacename (readonly unsignedlong aul_objbuf, readonly long al_start, readonly string as_oldname, readonly string as_newname)
public subroutine of_getsource (readonly unsignedlong aul_objbuf, ref string as_source)
public function integer of_getline (long al_offset, ref string as_line, long al_size)
public function long of_findgeneric (readonly unsignedlong aul_objbuf, readonly long al_start, readonly string as_search, readonly long al_size, ref string as_line, readonly integer ai_case)
public function integer of_startorca ()
public function integer of_stoporca ()
public function integer of_listinheritables (ref s_objectinfo astr_objects[])
public function integer of_listinheritables (ref s_objectinfo astr_objects[], readonly string as_liblist[])
public function integer of_setapplication (readonly string as_appname, readonly string as_applibrary, readonly string as_liblist[])
public function integer of_setlibrarylist (readonly string as_libraries[])
end prototypes

public function integer of_CreateLibrary (string as_Library, string as_LibComments);//*-------------------------------------------------------------------
//*  Function:			of_CreateLibrary 
//*  Purpose:			Creates a new PowerBuilder Library
//*-------------------------------------------------------------------
Return CP_CreateLibrary ( as_Library, as_LibComments ) 
end function

public function integer of_getlibrarycomments (readonly string as_library, ref string as_libcomments);//*-------------------------------------------------------------------
//*  Function:			of_GetLibraryComments 
//*  Purpose:			Returns comments associated with the library
//*-------------------------------------------------------------------
as_LibComments = Space ( 300 ) 

// Return CP_GetLibraryComments ( as_Library,  as_LibComments )

return -1
end function

public subroutine of_PBUnloadObject (unsignedlong aul_hObjBuf);//*-------------------------------------------------------------------
//*  Function:			of_PBUnloadObject 
//*  Purpose:			Unloads the object thru ORCA,
//*						object handle was returned from of_PBLoadObject.
//*-------------------------------------------------------------------
CP_UnloadObject ( aul_hObjBuf ) 
end subroutine

public function integer of_EnumLibraryObjects (string as_Library);//*-------------------------------------------------------------------
//*  Function:			of_EnumLibraryObjects 
//*  Purpose:			Enumerate the library objects,
//*						stored in the DLL memory
//*-------------------------------------------------------------------
// Return CP_EnumLibraryObjects ( as_Library )

Return -1
end function

public function unsignedlong of_PBLoadObject (string as_Library, string as_Object, integer ai_ObjectType);//*-------------------------------------------------------------------
//*  Function:			of_PBLoadObject 
//*  Purpose:			Loads object into memory for ORCA,
//*						returns handle to object
//*-------------------------------------------------------------------
Return CP_LoadObject ( as_Library, as_Object, ai_ObjectType )
end function

public function integer of_reset (readonly boolean ab_state);//*-------------------------------------------------------------------
//*  Function:			of_Reset 
//*  Purpose:			Sets indicator that application has been set
//*-------------------------------------------------------------------
int li_i, li_libs, li_last=0

/*  Delete the library classes in the DLL  */
IF Not ab_state THEN
	li_libs = UpperBound ( is_LibraryList )
	
	FOR li_i = 1 to li_libs
	
//		IF li_i = li_libs THEN li_last = 1
//		CP_ClearLibrary ( is_LibraryList[li_i], li_last )
		
	NEXT
	
	li_last = 0

END IF

ib_CompleteObjInfoLoaded = ab_state

Return 1
end function

public function long of_GetSourceSize (readonly unsignedlong aul_hObjBuf);//*-------------------------------------------------------------------
//*  Function:			of_GetSourceSize 
//*  Purpose:			Returns the size of the object source 
//*-------------------------------------------------------------------
Return CP_GetSourceSize ( aul_hObjBuf )
end function

public function integer of_pbimportobject (unsignedlong aul_hobjbuf, string as_library, string as_object, integer ai_objecttype, readonly string as_comment, ref string as_errormsg);//*-------------------------------------------------------------------
//*  Function:			of_PBImportObject 
//*  Purpose:			Imports & Compiles an object.  Source is
//*						stored in DLL memory.
//*-------------------------------------------------------------------
int li_i, li_rc
string ls_Error[10]

as_ErrorMsg = ""

FOR li_i = 1 TO 10
	ls_Error[li_i] = Space (256)
NEXT 

li_rc = CP_ImportObject ( aul_hObjBuf, as_Library, as_Object, ai_ObjectType, &
		as_comment, ls_Error ) 

IF li_rc <> 0 THEN
	FOR li_i = 1 to 10
		IF Trim ( ls_Error[li_i] ) <> "" THEN
			as_ErrorMsg = as_ErrorMsg + "~n" + ls_Error[li_i]
		END IF
	NEXT
END IF

Return li_rc
end function

public function integer of_gethierarchy (readonly string as_library, readonly string as_object, readonly integer ai_objtype, ref string as_userdata, ref string as_ancestors[]);//*-------------------------------------------------------------------
//*  Function:			of_GetHierarchy 
//*  Purpose:			Obtains an array reference to an object's
//*						ancestors (Holds up to 10 ancestors)
//*-------------------------------------------------------------------
int i, j
string ls_Ancestors[], ls_emptylist[]

/*  Initialize passed reference arguments  */
as_userdata = space (256)
FOR i = 1 TO 10
	as_Ancestors[i] = Space (256)
NEXT 

CP_GetObjectHierarchy ( as_Library, as_Object, ai_ObjType, as_UserData, as_Ancestors )

FOR i = 1 to 10
	IF Trim ( as_Ancestors[i] ) = "" &
	OR IsNull ( as_Ancestors[i] )  &
	OR Lower ( Trim ( as_Ancestors[i] ) ) = Lower ( as_Object ) THEN
		Continue
	ELSE
		j++
		ls_Ancestors[j] = as_Ancestors[i]
	END IF
NEXT

as_Ancestors = ls_emptylist
as_Ancestors = ls_Ancestors 

Return UpperBound ( as_Ancestors )
end function

public function unsignedlong of_replacename (readonly unsignedlong aul_objbuf, readonly long al_start, readonly string as_oldname, readonly string as_newname);int li_replace

li_replace = Len ( as_oldname )

Return CP_ReplaceName ( aul_ObjBuf, al_start, li_replace, as_newname )
end function

public subroutine of_getsource (readonly unsignedlong aul_objbuf, ref string as_source);/*  For DEBUGGING Purposes  */
as_source = Space ( 64000 )
CP_GetSource ( aul_ObjBuf, as_Source )
end subroutine

public function integer of_getline (long al_offset, ref string as_line, long al_size);return 1
end function

public function long of_findgeneric (readonly unsignedlong aul_objbuf, readonly long al_start, readonly string as_search, readonly long al_size, ref string as_line, readonly integer ai_case);long ll_OffSet

IF ai_case = 0 THEN
	/*  Find the offset of the search string in the source  */
	ll_OffSet = CP_FindGeneric ( aul_ObjBuf, al_start, Lower ( as_search ), al_size, ai_case )
ELSE
	/*  Find the offset of the search string in the source  */
	ll_OffSet = CP_FindGeneric ( aul_ObjBuf, al_start, as_search, al_size, ai_case )
END IF

Return ll_OffSet
end function

public function integer of_startorca ();Return CP_StartOrca ( )
end function

public function integer of_stoporca ();Return CP_StopOrca ( )
end function

public function integer of_listinheritables (ref s_objectinfo astr_objects[]);//*-------------------------------------------------------------------
//*  Function:			of_ListInheritables
//*  Purpose:			Loads the inheritable object list for all 
//*						libraries in the search path
//*-------------------------------------------------------------------
int li_curr, li_totlibs, li_typearr[], li_empty[], li_i
int li_totobjs, li_curobj, li_rc, li_objtype, li_ctr
long ll_start, ll_start2, ll_end, ll_end2
string ls_Object, ls_comments, ls_libarray[]
long ll_size
s_objectinfo lstr_init[]

IF ib_CompleteObjInfoLoaded THEN Return 1

li_totlibs = UpperBound ( is_LibraryList )

ids_objects.Reset ( )

FOR li_curr = 1 TO li_totlibs
	ll_start2 = ids_objects.RowCount ( ) + 1 
	
	ll_start = ll_start2
	ids_objects.ImportString ( LibraryDirectory ( is_LibraryList[li_curr], DirMenu! ) )
	ll_end = ids_objects.RowCount ( ) 
	li_typearr = li_empty
	FOR li_i = 1 to ( ( ll_end - ll_start ) + 1 ) 
		li_typearr[li_i] = 3
	NEXT	
	IF ll_end >= ll_start THEN 
		ids_objects.Object.ObjectType.Current[ll_start, ll_end] = li_typearr
	END IF
		
	ll_start = ll_end + 1 
	ids_objects.ImportString ( LibraryDirectory ( is_LibraryList[li_curr], DirUserObject! ) )
	ll_end = ids_objects.RowCount ( ) 
	li_typearr = li_empty
	FOR li_i = 1 to ( ( ll_end - ll_start ) + 1 ) 
		li_typearr[li_i] = 6
	NEXT
	IF ll_end >= ll_start THEN 
		ids_objects.Object.ObjectType.Current[ll_start, ll_end] = li_typearr
	END IF
		
	ll_start = ll_end + 1 
	ids_objects.ImportString ( LibraryDirectory ( is_LibraryList[li_curr], DirWindow! ) )
	ll_end = ids_objects.RowCount ( ) 
	li_typearr = li_empty
	FOR li_i = 1 to ( ( ll_end - ll_start ) + 1 ) 
		li_typearr[li_i] = 7
	NEXT
	IF ll_end >= ll_start THEN 
		ids_objects.Object.ObjectType.Current[ll_start, ll_end] = li_typearr
	END IF
	
	ll_end2 = ll_end
	FOR li_i = 1 to ( ( ll_end2 - ll_start2 ) + 1 ) 
		ls_Libarray[li_i] = is_LibraryList[li_curr]
	NEXT
	IF ll_end2 >= ll_start2 THEN ids_objects.Object.Library.Current[ll_start2, ll_end2] = ls_Libarray
NEXT 

astr_objects = lstr_init
If ids_objects.RowCount ( ) > 0 Then astr_objects = ids_objects.Object.Data

ib_CompleteObjInfoLoaded = TRUE

Return 1
end function

public function integer of_listinheritables (ref s_objectinfo astr_objects[], readonly string as_liblist[]);//*-------------------------------------------------------------------
//*  Function:			of_ListInheritables
//*  Purpose:			Loads the object list for all 
//*						libraries in the search path
//*-------------------------------------------------------------------
int li_curr, li_totlibs, li_typearr[], li_empty[], li_i
int li_totobjs, li_curobj, li_rc, li_objtype, li_ctr
long ll_start, ll_start2, ll_end, ll_end2
string ls_Object, ls_comments, ls_libarray[]
long ll_size
s_objectinfo lstr_init[]

IF ib_CompleteObjInfoLoaded THEN Return 1

li_totlibs = UpperBound ( as_liblist )

ids_objects.Reset ( )

FOR li_curr = 1 TO li_totlibs
	ll_start2 = ids_objects.RowCount ( ) + 1 
	
	ll_start = ll_start2
	ids_objects.ImportString ( LibraryDirectory ( as_liblist[li_curr], DirMenu! ) )
	ll_end = ids_objects.RowCount ( ) 
	li_typearr = li_empty
	FOR li_i = 1 to ( ( ll_end - ll_start ) + 1 ) 
		li_typearr[li_i] = 3
	NEXT	
	IF ll_end >= ll_start THEN 
		ids_objects.Object.ObjectType.Current[ll_start, ll_end] = li_typearr
	END IF
		
	ll_start = ll_end + 1 
	ids_objects.ImportString ( LibraryDirectory ( as_liblist[li_curr], DirUserObject! ) )
	ll_end = ids_objects.RowCount ( ) 
	li_typearr = li_empty
	FOR li_i = 1 to ( ( ll_end - ll_start ) + 1 ) 
		li_typearr[li_i] = 6
	NEXT
	IF ll_end >= ll_start THEN 
		ids_objects.Object.ObjectType.Current[ll_start, ll_end] = li_typearr
	END IF
		
	ll_start = ll_end + 1 
	ids_objects.ImportString ( LibraryDirectory ( as_liblist[li_curr], DirWindow! ) )
	ll_end = ids_objects.RowCount ( ) 
	li_typearr = li_empty
	FOR li_i = 1 to ( ( ll_end - ll_start ) + 1 ) 
		li_typearr[li_i] = 7
	NEXT
	IF ll_end >= ll_start THEN 
		ids_objects.Object.ObjectType.Current[ll_start, ll_end] = li_typearr
	END IF
	
	ll_end2 = ll_end
	FOR li_i = 1 to ( ( ll_end2 - ll_start2 ) + 1 ) 
		ls_Libarray[li_i] = as_liblist[li_curr]
	NEXT
	IF ll_end2 >= ll_start2 THEN ids_objects.Object.Library.Current[ll_start2, ll_end2] = ls_Libarray
NEXT 

astr_objects = lstr_init
If ids_objects.RowCount ( ) > 0 Then astr_objects = ids_objects.Object.Data
ib_CompleteObjInfoLoaded = TRUE
Return 1
end function

public function integer of_setapplication (readonly string as_appname, readonly string as_applibrary, readonly string as_liblist[]);//*-------------------------------------------------------------------
//*  Function:			of_SetApplication 
//*  Purpose:			Initialize the application object & path
//*-------------------------------------------------------------------
string ls_empty[]

/*  Save Application Name  */
is_ApplicationName = as_appname

/*  Save Application Library  */
is_ApplicationLibrary = as_applibrary

/*  Save Library List  */
is_LibraryList = ls_empty
is_LibraryList = as_liblist

/*  Initialize the application in ORCA  */
Return CP_InitAppl ( is_LibraryList, UpperBound (is_LibraryList), &
	 is_ApplicationName, is_ApplicationLibrary )
end function

public function integer of_setlibrarylist (readonly string as_libraries[]);// for CR337000, call the following external function for storing a map containing
// the relationship between library and it's binary type.
Return CP_SetLibraryList( as_Libraries, UpperBound (  as_Libraries ) )
end function

on common_orca_unicode.create
call super::create
end on

on common_orca_unicode.destroy
call super::destroy
end on

event constructor;call super::constructor;ids_objects = Create datastore
ids_objects.DataObject = "d_objectlist"
end event

event destructor;call super::destructor;Destroy ids_objects
end event


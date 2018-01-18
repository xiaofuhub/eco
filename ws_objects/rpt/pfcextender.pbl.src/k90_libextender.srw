$PBExportHeader$k90_libextender.srw
$PBExportComments$(K90) - PFC Extender Standard Wizard Window
forward
global type k90_libextender from base_center
end type
type sizegrip from hscrollbar within k90_libextender
end type
type cb_print from commandbutton within k90_libextender
end type
type st_info from statictext within k90_libextender
end type
type cb_help from commandbutton within k90_libextender
end type
type cb_cancel from commandbutton within k90_libextender
end type
type cb_next from commandbutton within k90_libextender
end type
type cb_back from commandbutton within k90_libextender
end type
type gb_line from groupbox within k90_libextender
end type
type p_1 from picture within k90_libextender
end type
end forward

global type k90_libextender from base_center
string tag = "2395 x 1460"
integer x = 1074
integer y = 484
integer width = 2395
integer height = 1460
string title = "untitled"
boolean minbox = false
boolean maxbox = false
boolean resizable = false
windowtype windowtype = response!
long backcolor = 79416533
event type integer ue_next ( readonly boolean ab_state )
event type integer ue_back ( readonly boolean ab_state )
event type integer ue_step ( readonly integer ai_step )
event ue_setinfo ( readonly string as_text )
event ue_postopen ( )
event wm_getminmaxinfo pbm_getminmaxinfo
sizegrip sizegrip
cb_print cb_print
st_info st_info
cb_help cb_help
cb_cancel cb_cancel
cb_next cb_next
cb_back cb_back
gb_line gb_line
p_1 p_1
end type
global k90_libextender k90_libextender

type prototypes
protected:

Function long GetWindowLong (long hWindow, integer nIndex) Library "user32.dll" ALIAS FOR GetWindowLongW
Function long SetWindowLong (long hWindow, integer nIndex, long dwNewLong) Library "user32.dll" ALIAS FOR SetWindowLongW
subroutine GetMinMaxInfo ( ref s_minmaxinfo d, long s, long l ) library 'kernel32.dll' alias for RtlMoveMemory 
subroutine SetMinMaxInfo ( long d, s_minmaxinfo s, long l ) library 'kernel32.dll' alias for RtlMoveMemory
FUNCTION ulong GetSystemMenu(ulong hWnd, BOOLEAN bRevert)  Library "USER32"
FUNCTION boolean DeleteMenu( ulong hMenu, uint uPosition, uint uFlags ) LIBRARY "user32.dll"
FUNCTION boolean InsertMenu( ulong HMenu, uint uPosition, uint uFlags, ulong uIDNewItem, string lpNewItem ) LIBRARY "user32.dll" ALIAS FOR InsertMenuW
FUNCTION boolean DrawMenuBar( ulong hWnd ) LIBRARY "user32.dll"
FUNCTION int GetSystemMetrics(integer nIndex) LIBRARY "user32.dll"
FUNCTION boolean SetWindowPos ( ulong hWnd, ulong hWinInsertAfter, int newx, int newy, int newwidth, int newheight, long flags ) LIBRARY "user32.dll"
end prototypes

type variables
/*  Make instance since of_CreateNewObject  
    can't handle # of arguments  */
string is_importlibrary, is_Loadlibrary
string is_oldanc, is_newanc
string is_oldobj, is_newobj
/*  The wizard userobject instances */
k90_page iuo_wiz []
/*  The names of the wizard objects  */
string is_wizuos []
/*  The current step  */
int ii_step=0
/*  File Services  */
common_routines inv_common
/*  ORCA Services  */
common_orca inv_orca
/*  Response to message  */
int ii_msg
/*  Contains the prefixes chosen  */
string is_hierarchy[], is_newprefix, is_dropprefix
/*  Template application library  */
string is_template
/*  Indicators to rebuild page data  */
boolean ib_build2, ib_build3, ib_build5, ib_build6
/*  When TRUE, User cancelled the process  */
boolean ib_canceled=FALSE
/*  When FALSE, Cancel button won't close the window  */
boolean ib_canclose = TRUE
/*  The number of errors that occurred  */
int ii_errorcount=0
/*  User chooses to move customizations  */
boolean ib_PFEIsEmpty=FALSE
/*  Object Datastores  */
datastore ids_objects, ids_allobjects, ids_hierarchy
datastore ids_libnames
/*  App Name and Library */
string is_AppName
string is_AppLibrary
/*  Extension Option  */
boolean ib_noapp
/*  Paste Lib Path Options  */
boolean ib_pastefile=TRUE
boolean ib_pasteclip=TRUE
/*  Application Path */
string is_AppPath[], is_OrigPath[]
/*  EXE Directory  */
string is_CurrentDirectory
/*  Constants  */
CONSTANT int II_MAXSTEPS = 7
CONSTANT int II_OPENX = 613
CONSTANT int II_OPENY = 36
/*  Names of Template Objects  */
CONSTANT string IS_MENU = "menu_template"
CONSTANT string IS_WINDOW = "window_template"
CONSTANT string IS_NVO = "nonvisualobject_template"
CONSTANT string IS_NVOAUTO = "nonvisualobject_auto_template"
CONSTANT string IS_CUSTOMUO = "customvisualobject_template"
CONSTANT string IS_STDUO = "standardobject_template"

/*  Object is Auto-Instantiate  */
boolean ib_IsAuto=FALSE

protected:

long il_orig_width, il_orig_height

private:

boolean	ib_resizeable = FALSE
boolean	ib_show_szgrip = FALSE
end variables

forward prototypes
private function integer of_SetTitle (readonly string as_info)
private function integer of_makelibrary (readonly string as_library, readonly string as_comments)
private function string of_getprefixname (string as_objectname, string as_pfename)
public function integer of_setapplication (readonly string as_appname, readonly string as_applibrary, readonly boolean ab_skip)
public function boolean of_getpage2status (readonly boolean ab_status)
public function boolean of_getpage3status (readonly boolean ab_status)
public function boolean of_getpage5status (readonly boolean ab_status)
public function boolean of_getpage6status (readonly boolean ab_status)
public function string of_getapplibrary ()
public function string of_getappname ()
public function integer of_setcustomoptions (readonly boolean ab_custom)
private function string of_getloadlibrary (readonly string as_object)
public subroutine of_setpastefile (readonly boolean ab_paste)
public subroutine of_setpasteclip (readonly boolean ab_paste)
public function integer of_setobjects (s_objectinfo astr_objects[])
public function integer of_gethierarchy (ref string as_prefixes[], ref string as_newprefix)
public function integer of_sethierarchy (readonly long al_newitem)
public function integer of_getlevelobjects ()
public function integer of_getobjectselection ()
private function integer of_changeancestry (readonly long al_objsize, readonly string as_oldancestry, readonly string as_newancestry, ref long al_newsize)
private function integer of_createnewobject (readonly integer ai_objtype, string as_comments, readonly boolean ab_ispfe)
private function integer of_finish ()
public function integer of_getancestors (string as_object, ref string as_ancestors[], string as_path[])
private function integer of_buildobjects ()
private function string of_picktemplate (readonly string as_ancestors[], readonly integer ai_objtype)
private function integer of_createlibrarypath (ref string as_libpath[], readonly boolean ab_pfeonly, readonly boolean ab_noapp)
public function string of_getimportlibrary (readonly string as_filename)
public function integer of_getliblist ()
private function integer of_pasteliblist ()
private function integer of_copypfe (readonly string as_oldpfename, readonly string as_newpfename)
private function string of_createlibraryname (string as_oldpfename)
private function string of_createlibraryname (string as_oldpfename, string as_prefix, string as_genericname)
public function integer of_getprefixes (ref datastore adw)
public function integer of_settarget (readonly string as_target, readonly boolean ab_skip)
private function integer of_createorigpath (ref string as_libpath[])
private function string of_getprefixnameforobject (string as_objectname, string as_pfename)
public function integer of_setresizeable (boolean ab_showgrip)
protected subroutine of_resizetab (integer ai_step)
end prototypes

event ue_next;//*-------------------------------------------------------------------
//*  Event:				ue_next 
//*  Purpose:			Sets the state of the Next button
//*-------------------------------------------------------------------
cb_next.Enabled = ab_state

Return 1
end event

event ue_back;//*-------------------------------------------------------------------
//*  Event:				ue_back
//*  Purpose:			Sets the state of the Back button
//*-------------------------------------------------------------------
cb_back.Enabled = ab_state

Return 1
end event

event type integer ue_step(readonly integer ai_step);//*-------------------------------------------------------------------
//*  Event:				ue_step
//*  Purpose:			Go to specified step
//*-------------------------------------------------------------------
string ls_direction = "next"

SetPointer ( HourGlass! )

//this.SetReDraw ( FALSE ) 

IF ii_step > ai_step THEN ls_direction = "back" 

/*  Clean up/Validation on current step  */
IF ii_step > 0 THEN
	IF IsValid ( iuo_wiz[ii_step] ) THEN
		IF iuo_wiz[ii_step].Event ue_losefocus ( ls_direction ) = -1 THEN Return -1
	END IF  
END IF

/*  Don't exceed the maximum number of steps  */
IF ai_step > ii_maxsteps THEN Return -1

/*  Change the button text on the last step  */
IF ai_step = ii_maxsteps THEN
	cb_next.Text = "&Finish"
ELSE
	cb_next.Text = "&Next >" 
END IF

cb_back.Enabled = ( ai_step > 1 ) 

//hide the old tab
IF ii_step > 0 THEN iuo_wiz[ii_step].Visible = FALSE

/*  Change to new step  */
ii_step = ai_step

/*  If you've already been to step, then just show the page  */
IF ii_step <= UpperBound ( iuo_wiz ) THEN
	IF IsValid ( iuo_wiz [ii_step] ) THEN
		iuo_wiz[ii_step].Visible = TRUE
		iuo_wiz[ii_step].BringToTop = TRUE
		/*  Force a focus event  */
		of_resizetab ( ii_step )
		iuo_wiz[ii_step].Event ue_getfocus ( ls_direction ) 
		of_SetTitle ( iuo_wiz[ii_step].Tag )
		this.SetReDraw ( TRUE ) 
		Return 1
	END IF
END IF 

/*  First time for this page, open the Page UserObject  */
OpenUserObject ( iuo_wiz[ii_step], is_wizuos[ii_step], II_OPENX, II_OPENY )

iuo_wiz[ii_step].TabOrder = ii_step
iuo_wiz[ii_step].Visible = TRUE
iuo_wiz[ii_step].BringToTop = TRUE
iuo_wiz[ii_step].Show ( ) 
IF IsValid ( iuo_wiz[ii_step].idrg_focus ) THEN 
	iuo_wiz[ii_step].idrg_focus.SetFocus ( )
ELSE
	iuo_wiz[ii_step].SetFocus ( ) 
END IF

of_resizetab ( ii_step )

/*  Force a focus event  */
iuo_wiz[ii_step].Event ue_getfocus ( ls_direction ) 

of_SetTitle ( iuo_wiz[ii_step].Tag )

IF ii_step > 1 THEN this.SetReDraw ( TRUE ) 

Return 1
end event

event ue_setinfo;//*-------------------------------------------------------------------
//*  Event:				ue_setinfo
//*  Purpose:			Set Text Message
//*-------------------------------------------------------------------
st_info.Text = as_text
end event

event ue_postopen();//*-------------------------------------------------------------------
//*  Event:				ue_postopen
//*  Purpose:			Obtain the name of the template PBL
//*-------------------------------------------------------------------
is_template = inv_common.is_template

If IsValid(sizegrip) Then
	// move sizegrip control to back of Z order
	sizegrip.SetPosition(ToBottom!)
End If

end event

event wm_getminmaxinfo;//If this isn't a resizeable response window, don't bother with this
IF NOT ib_resizeable THEN RETURN

//Since we're resizable, we want to make sure that the user doesn't try to resize the
//window smaller than it's original size
s_MinMaxInfo lstr_MinMaxInfo

//Populate the structure
GetMinMaxInfo(lstr_MinMaxInfo, MinMaxInfo, 40)

//Determine the minimum size of the window (the original size)
lstr_MinMaxInfo.ptMinTrackSize.x = UnitsToPixels(il_orig_width, XUnitsToPixels!)
lstr_MinMaxInfo.ptMinTrackSize.y = UnitsToPixels(il_orig_height, YUnitsToPixels!)

//Now set the Min/Max info
SetMinMaxInfo(MinMaxInfo,lstr_MinMaxInfo,40)

return 0
end event

private function integer of_SetTitle (readonly string as_info);//*-------------------------------------------------------------------
//*  Function:			of_SetTitle 
//*  Purpose:			Modify the Wizard's title
//*-------------------------------------------------------------------
this.Title = "PFC Library Extender - " + as_info

Return 1
end function

private function integer of_makelibrary (readonly string as_library, readonly string as_comments);//*-------------------------------------------------------------------
//*  Function:			of_MakeLibrary
//*  Purpose:			Make a new Extension Library
//*-------------------------------------------------------------------
string ls_drive, ls_dir, ls_filename, ls_pathonly

SetPointer ( HourGlass! )

inv_common.f_ParsePath ( as_library, ls_drive, ls_dir, ls_filename )
ls_pathonly = ls_drive + ls_dir 

/*  Determine if the path exists, otherwise create  */
IF Not inv_common.f_DirectoryExists ( ls_pathonly ) THEN 
	IF inv_common.f_CreateDirectory ( ls_pathonly ) <> 1 THEN 
		f_error ( "Unable to create directory '" + ls_pathonly + "'." + &
			" Cannot continue process.", "k90_libextender", "of_MakeLibrary", StopSign!, OK!, 1 )
		Return -1
	END IF
END IF

///*  See if Library Already Exists  */
//IF FileExists ( as_library ) THEN
//	IF ii_msg = 0 THEN
//		ii_msg = f_error ( 2, "At least some of the libraries already exist in " + ls_pathonly + ".  Is it OK to" + &
//			" overwrite them all?", &
//			"of_MakeLibrary", "k90_libextender", YesNo!, 1, "" )
//	END IF
//	IF ii_msg = 2 THEN Return -1
//END IF

FileDelete ( as_library )

/*  Create the library */
Return inv_orca.of_CreateLibrary ( as_library, as_comments )
end function

private function string of_getprefixname (string as_objectname, string as_pfename);//*-------------------------------------------------------------------
//*  Function:			of_GetPrefixName
//*  Purpose:			Obtain a prefix from object name 
//*-------------------------------------------------------------------
string ls_prefix 

/*  Extract the prefix from the objectname  */
ls_prefix = inv_common.f_GetToken ( as_objectname, "_" )

IF ls_prefix = "w" &
OR ls_prefix = "u" &
OR ls_prefix = "n" &
OR ls_prefix = "m" THEN
	/*  Extract the prefix from the objectname  */
	ls_prefix = inv_common.f_GetToken ( as_objectname, "_" )
END IF

IF ls_prefix = "" THEN
	/*  Unable to extract prefix, make-up a name  */
	ls_prefix = "<level @>"
END IF

Return ls_prefix
end function

public function integer of_setapplication (readonly string as_appname, readonly string as_applibrary, readonly boolean ab_skip);//*-------------------------------------------------------------------
//*  Function:			of_SetApplication
//*  Purpose:			Save the application name and library
//*-------------------------------------------------------------------
/*  Indicator that Page 2 needs to be rebuilt  */
ib_build2 = TRUE

IF ab_skip <> ib_noapp THEN
	IF UpperBound ( iuo_wiz ) >= 2 THEN
		IF IsValid ( iuo_wiz[2] ) THEN
			CloseUserObject ( iuo_wiz[2] )
		END IF
	END IF
END IF

IF ab_skip THEN
	is_AppName = "pfcext_application_template"
	is_AppLibrary = is_Template

ELSE

	is_AppName = as_appname
	is_AppLibrary = as_applibrary

END IF

ib_noapp = ab_skip

IF ab_skip THEN 
	is_wizuos [2] = "k90_page2a"
ELSE
	is_wizuos [2] = "k90_page2b"
END IF

Return 1
end function

public function boolean of_getpage2status (readonly boolean ab_status);//*-------------------------------------------------------------------
//*  Function:			of_GetPage2Status
//*  Purpose:			Does Page 2 need to be re-initialized? 
//*-------------------------------------------------------------------
boolean lb_state

lb_state = ib_build2

ib_build2 = ab_status

Return lb_state
end function

public function boolean of_getpage3status (readonly boolean ab_status);//*-------------------------------------------------------------------
//*  Function:			of_GetPage3Status
//*  Purpose:			Does Page 3 need to be re-initialized? 
//*-------------------------------------------------------------------
boolean lb_state

lb_state = ib_build3

ib_build3 = ab_status

Return lb_state
end function

public function boolean of_getpage5status (readonly boolean ab_status);//*-------------------------------------------------------------------
//*  Function:			of_GetPage5Status
//*  Purpose:			Does Page 5 need to be re-initialized? 
//*-------------------------------------------------------------------
boolean lb_state

lb_state = ib_build5

ib_build5 = ab_status

Return lb_state
end function

public function boolean of_getpage6status (readonly boolean ab_status);//*-------------------------------------------------------------------
//*  Function:			of_GetPage6Status
//*  Purpose:			Does Page 6 need to be re-initialized? 
//*-------------------------------------------------------------------
boolean lb_state

lb_state = ib_build6

ib_build6 = ab_status

Return lb_state
end function

public function string of_getapplibrary ();//*-------------------------------------------------------------------
//*  Function:			of_GetAppLibrary
//*  Purpose:			Return the name of the application library
//*-------------------------------------------------------------------
Return is_AppLibrary
end function

public function string of_getappname ();//*-------------------------------------------------------------------
//*  Function:			of_GetAppName
//*  Purpose:			Return the name of the application object
//*-------------------------------------------------------------------
Return is_AppName
end function

public function integer of_setcustomoptions (readonly boolean ab_custom);//*-------------------------------------------------------------------
//*  Function:			of_SetCustomOptions
//*  Purpose:			Set the options from the Customize step,
//*						called from Step 3 Userobject
//*-------------------------------------------------------------------
ib_PFEIsEmpty = ab_custom

Return 1
end function

private function string of_getloadlibrary (readonly string as_object);//*-------------------------------------------------------------------
//*  Function:			of_GetLoadLibraryName
//*  Purpose:			Get the library name
//*-------------------------------------------------------------------
long ll_found
string ls_Library

SetPointer ( HourGlass! )

ll_found = ids_allobjects.Find ( "ObjectName='" + as_object + "'", 1, &
		ids_allobjects.RowCount ( ) )

IF ll_found > 0 THEN
	ls_Library = ids_allobjects.GetItemString ( ll_found, "Library" ) 
ELSE
	ls_Library = ""
END IF

Return ls_Library
end function

public subroutine of_setpastefile (readonly boolean ab_paste);ib_pastefile = ab_paste
end subroutine

public subroutine of_setpasteclip (readonly boolean ab_paste);ib_pasteclip = ab_paste
end subroutine

public function integer of_setobjects (s_objectinfo astr_objects[]);//*-------------------------------------------------------------------
//*  Function:			of_SetObjects
//*  Purpose:			Called from Object Select Page, 
//*						to establish the selected objects
//*-------------------------------------------------------------------
IF UpperBound ( astr_objects ) > 0 THEN
	ids_objects.DataObject = "d_objectlist"
	ids_objects.Object.Data = astr_objects
END IF

Return 1
end function

public function integer of_gethierarchy (ref string as_prefixes[], ref string as_newprefix);//*-------------------------------------------------------------------
//*  Function:			of_GetHierarchy
//*  Purpose:			Obtain the extension prefixes,
//*						called from Step 2 Userobject
//*-------------------------------------------------------------------
as_prefixes = is_hierarchy

as_newprefix = is_newprefix

Return UpperBound ( is_hierarchy ) 
end function

public function integer of_sethierarchy (readonly long al_newitem);//*-------------------------------------------------------------------
//*  Function:			of_SetHierarchy
//*  Purpose:			Establish the new extension prefixes,
//*						called from Step 2 Userobject
//*-------------------------------------------------------------------
IF al_newitem > 0 THEN
	is_newprefix = is_hierarchy[al_newitem]
	is_dropprefix = is_hierarchy[al_newitem + 1]
END IF

/*  Page 5 needs to be rebuilt  */
ib_build5 = TRUE
	
/*  Page 6 needs to be rebuilt  */
ib_build6 = TRUE
	
Return 1
end function

public function integer of_getlevelobjects ();//*-------------------------------------------------------------------
//*  Function:			of_GetLevelObjects
//*  Purpose:			Get Objects which have the "dropped-on" 
//*						level in their hierarchy
//*-------------------------------------------------------------------
int li_obj, li_maxobj
long ll_i, ll_rows, ll_hrows
string ls_object, ls_library
s_objectinfo lstr_objects[], lstr_empty[]

SetPointer ( HourGlass! )

ids_objects.SetFilter ( "" ) 
ids_objects.Filter ( )
lstr_objects = ids_objects.Object.Data

/*  Turn off selected indictor on all objects  */
//  was commented
li_maxobj = UpperBound ( lstr_objects ) 
FOR li_obj = 1 to li_maxobj
	lstr_objects[li_obj].ISelected = 0
NEXT

IF is_dropprefix = "pfe" THEN 
	ids_hierarchy.SetFilter ( "IsNull ( sObjPrefix )" )
ELSE
	ids_hierarchy.SetFilter ( "sObjPrefix = '" + is_dropprefix + "'" )
END IF 

ids_hierarchy.Filter ( )
ll_hrows = ids_hierarchy.RowCount ( ) 

/*  Now match those objects with the main object list.
	 This is what the user can select from  */
//  was commented
ll_rows = ids_hierarchy.RowCount ( ) 
FOR ll_i = 1 to ll_rows
	ls_object = ids_hierarchy.GetItemString ( ll_i, "sObject" ) 
	ls_library = ids_hierarchy.GetItemString ( ll_i, "sLibrary" ) 
	FOR li_obj = 1 to li_maxobj
		IF ( ls_object = lstr_objects[li_obj].sObject ) &
		AND ( ls_library = lstr_objects[li_obj].sLibrary ) THEN
			lstr_objects[li_obj].ISelected = 1
		END IF
	NEXT
NEXT

ll_rows = UpperBound ( lstr_objects ) 
FOR ll_i = 1 to ll_rows 
	IF ids_hierarchy.Find ( "sObject='" + lstr_objects[ll_i].sObject + "' And sLibrary='" + &
			lstr_objects[ll_i].sLibrary + "'", 1, ll_hrows ) > 0 THEN 
		lstr_objects[ll_i].ISelected = 1
	END IF
NEXT

IF UpperBound ( lstr_objects ) = 0 THEN
	ids_objects.DataObject = "d_objectlist"
 	Return -1
END IF

/*  Now filter the selected objects to the data window  */
ids_objects.DataObject = "d_objectlist"
ids_objects.Object.Data = lstr_objects
ids_objects.SetFilter ( "selected = 1" ) 
ids_objects.Filter ( ) 

IF ids_objects.RowCount ( ) = 0 THEN Return -1

/*  Now reset the dw so that only the selected are available  */
lstr_objects = lstr_empty
lstr_objects = ids_objects.Object.Data
ids_objects.DataObject = "d_objectlist"
ids_objects.Object.Data = lstr_objects
ids_objects.SetFilter ( "" ) 

Return 1
end function

public function integer of_getobjectselection ();//*-------------------------------------------------------------------
//*  Function:			of_GetObjectSelection
//*  Purpose:			Get a list of the objects 
//*						and make available for selection,
//*						called from Step 4 Userobject
//*-------------------------------------------------------------------
string ls_LibraryPath[], ls_TempPath[]
s_objectinfo lstr_objects[]

SetPointer ( HourGlass! )

/*  Reset the Library List in ORCA  */
//inv_orca.of_Reset ( FALSE ) 

/*  Create the library path for ORCA  */
//of_CreateLibraryPath ( ls_LibraryPath, FALSE, FALSE ) 
//	
///*  Set the application list in ORCA  */
//IF inv_orca.of_SetApplication ( is_AppName, is_AppLibrary, ls_LibraryPath ) <> 0 THEN
//	f_error ( "Error setting library list.~n" + &
//			"Possible cause: Application has not been migrated to Current Version", &
//			"k90_libextender", "of_GetObjectSelection", StopSign!, OK!, 1 )
//	Return -1
//END IF

/*  Now create a list of the libraries without the application library.
	 Don't want objects from the application library in the selection list.  */
of_CreateLibraryPath ( ls_TempPath, FALSE, TRUE )
inv_orca.of_ListInheritables ( lstr_objects, ls_TempPath )
ids_objects.DataObject = "d_objectlist" 
IF UpperBound ( lstr_objects ) > 0 THEN
	/*  Populate the list of objects  */
	ids_objects.Object.Data = lstr_objects
END IF

/*  Set filter so template objects don't show in the list  */
ids_objects.SetFilter ( "(Left(comments,23) <> 'DO NOT DELETE OR MODIFY') AND (objecttype = 3 or objecttype = 6 or objecttype = 7) AND ( Left(objectname,4) <> 'pfc_')" )
ids_objects.Filter ( )
ids_objects.SetSort ( "Library A, ObjectType A, ObjectName A" ) 
ids_objects.Sort ( ) 

Return 1
end function

private function integer of_changeancestry (readonly long al_objsize, readonly string as_oldancestry, readonly string as_newancestry, ref long al_newsize);//*-------------------------------------------------------------------
//*  Function:			of_ChangeAncestry
//*  Purpose:			Modify the object source for new ancestry
//*-------------------------------------------------------------------
long ll_offset, ll_size
string ls_temp

/*  No change, return  */
IF as_oldancestry = as_newancestry THEN
	al_newsize = al_objsize
	Return -2
END IF

/*  Find the offset into source of the old values  */
ll_offset = inv_orca.of_FindGeneric ( inv_orca.iul_CurObj, 1, &
		as_oldancestry, al_objsize, ls_temp, 1 ) 

IF ll_offset > 0 THEN 

	/*  Replace the ancestry with the new name  */
	inv_orca.iul_CurObj = inv_orca.of_ReplaceName ( inv_orca.iul_CurObj, &
		ll_offset, as_oldancestry, as_newancestry )

	IF inv_orca.iul_CurObj <= 0 THEN
		al_newsize = 0
		Return -1
	END IF

	ll_size = al_objsize + Len ( as_newancestry ) - Len ( as_oldancestry ) 

	al_newsize = ll_size
	Return 1

ELSE
	
	al_newsize = al_objsize
	Return -2

END IF
end function

private function integer of_createnewobject (readonly integer ai_objtype, string as_comments, readonly boolean ab_ispfe);//*-------------------------------------------------------------------
//*  Function:			of_CreateNewObject
//*  Purpose:			Build the new object by adjusting the ancestry
//*-------------------------------------------------------------------
long ll_msgrow, ll_size
string ls_error, ls_comments

/*  Load the object from the Original Library  */
inv_orca.iul_CurObj = inv_orca.of_PBLoadObject ( is_LoadLibrary, is_oldobj, ai_ObjType ) 

/*  Show message that processing is occurring  */
IF ab_IsPFE THEN
	ll_msgrow = iuo_wiz[ii_step].of_AddMessage ( "Rebuilding " + is_newobj + "...", -2 )
	ls_comments = as_Comments
ELSE
	ll_msgrow = iuo_wiz[ii_step].of_AddMessage ( "Building " + is_newobj + "...", -2 )
	ls_comments = "(" + Upper(is_newprefix) + ") " + as_Comments
END IF

/*  Get the size of the object  */
ll_size = inv_orca.of_GetSourceSize ( inv_orca.iul_CurObj ) 

/*  Now start changing the ancestry  */
IF of_ChangeAncestry ( ll_size, &
	"global type " + is_oldobj + " from " + is_oldanc, &
	"global type " + is_newobj + " from " + is_newanc, ll_size ) = -1 THEN Return -1

IF of_ChangeAncestry ( ll_size, &
	"global type " + is_oldobj + " from " + is_oldanc, &
	"global type " + is_newobj + " from " + is_newanc, ll_size ) = -1 THEN Return -1

IF of_ChangeAncestry ( ll_size, &
	"global " + is_oldobj + " " + is_oldobj, &
	"global " + is_newobj + " " + is_newobj, ll_size ) = -1 THEN Return -1

IF of_ChangeAncestry ( ll_size, &
	"on " + is_oldobj + ".create", &
	"on " + is_newobj + ".create", ll_size ) = -1 THEN Return -1

IF of_ChangeAncestry ( ll_size, &
	"on " + is_oldobj + ".destroy", &
	"on " + is_newobj + ".destroy", ll_size ) = -1 THEN Return -1

IF of_ChangeAncestry ( ll_size, &
	is_oldobj + "=this", &
	is_newobj + "=this", ll_size ) = -1 THEN Return -1

IF of_ChangeAncestry ( ll_size, &
	"call " + is_oldanc + "::destroy", &
	"call " + is_newanc + "::destroy", ll_size ) = -1 THEN Return -1

IF of_ChangeAncestry ( ll_size, &
	"call " + is_oldanc + "::create", &
	"call " + is_newanc + "::create", ll_size ) = -1 THEN Return -1

DO UNTIL of_ChangeAncestry ( ll_size, &
	" from " + is_oldanc + "`", &
	" from " + is_newanc + "`", ll_size )  = -2
LOOP

DO UNTIL of_ChangeAncestry ( ll_size, &
	" within " + is_oldobj, &
	" within " + is_newobj, ll_size ) = -2
LOOP

IF IsNull ( as_Comments ) THEN as_Comments = ""

/*  Build the PFE object as the new name  */
inv_orca.of_PBImportObject ( inv_orca.iul_CurObj, &
			is_ImportLibrary, is_newobj, ai_ObjType, ls_comments, ls_error )

/*  Report the success or failure  */
IF ls_error <> "" THEN
	iuo_wiz[ii_step].of_AddMessage ( ll_msgrow, "", 1 ) 
	iuo_wiz[ii_step].of_AddMessage ( ls_error, -1 )
	iuo_wiz[ii_step].of_AddMessage ( "", -2 ) 
	ii_errorcount++
ELSE
	iuo_wiz[ii_step].of_AddMessage ( ll_msgrow, "", 0 )
	iuo_wiz[ii_step].SetReDraw ( TRUE ) 
END IF

/*  Unload the object  */
inv_orca.of_PBUnLoadObject ( inv_orca.iul_CurObj ) 

Return 1
end function

private function integer of_finish ();//*-------------------------------------------------------------------
//*  Function:			of_Finish
//*  Purpose:			Create the PFC Extensions 
//*-------------------------------------------------------------------
int li_libs, li_totlibs
string ls_prefix, ls_OrigLibraryName, ls_NewLibraryName
string ls_LibComments, ls_LibraryPath[]
s_objectinfo lstr_AllObjects[], lstr_emptylist[]
k90_page7 luo_page

SetPointer ( HourGlass! )

ib_canceled = FALSE

//*------------------------------------------------------------------------
//*   Initialization
//*------------------------------------------------------------------------
ii_msg = 0
ii_errorcount = 0

/*  Can't close the window until this is done  */
ib_canclose = FALSE

cb_print.Visible = TRUE
cb_print.Enabled = FALSE
cb_back.Enabled = FALSE
cb_next.Enabled = FALSE

/*  Construct the original path for the ClassDefinition calls  */
of_CreateOrigPath ( is_OrigPath )

//*------------------------------------------------------------------------
//*   Make copies of the Libraries
//*------------------------------------------------------------------------
li_totlibs = ids_LibNames.RowCount ( ) 

FOR li_libs = 1 to li_totlibs

	ls_prefix = Lower ( ids_LibNames.GetItemString ( li_libs, "CommonName" ) )
	IF ls_prefix = "" Or IsNull ( ls_prefix ) THEN Continue

	/*  Get the library names  */
	ls_OrigLibraryName = ids_LibNames.GetItemString ( li_libs, "OrigLibName" )
	ls_NewLibraryName = ids_LibNames.GetItemString ( li_libs, "LibraryName" )

	IF Left ( ls_prefix, 3 ) = "pfe" THEN
		/*  OK, if its the lower level prefix, then you want
			 to make a filecopy.  If it's the new level, you want to 
			 do a create library  */
		IF of_CopyPFE ( ls_OrigLibraryName, ls_NewLibraryName ) <> 1 THEN
			Return -1
		END IF
	
	ELSEIF Left ( ls_prefix, 3 ) = "new" THEN
		/*  Use the same comments as the PFE library  */
		inv_orca.of_GetLibraryComments ( ls_OrigLibraryName, ls_LibComments )
	
		IF IsNull ( ls_LibComments ) THEN ls_LibComments = ""
		ls_LibComments = "(" + Upper ( is_newprefix ) + ") " + ls_LibComments
			
		/*  Create a new library  */
		of_MakeLibrary ( ls_NewLibraryName, ls_LibComments )

	END IF

NEXT

//*------------------------------------------------------------------------
//*   Set the application list in ORCA
//*------------------------------------------------------------------------
inv_orca.of_Reset ( FALSE ) 
//
inv_orca.of_StopOrca ( ) 
inv_orca.of_StartOrca ( )

/*  Create the library application path in string array  */
of_CreateLibraryPath ( ls_LibraryPath, FALSE, FALSE ) 
IF Not ib_noapp THEN
	/*  Add template library as well  */
	ls_LibraryPath[UpperBound (ls_LibraryPath) + 1] = is_Template
END IF

IF inv_orca.of_SetApplication ( is_AppName, is_AppLibrary, ls_LibraryPath ) <> 0 THEN
	f_error ( "Error setting library list.~n" + &
			"Possible cause: Application has not been migrated to Current Version", &
			 "k90_libextender", "of_Finish", StopSign!, OK!, 1 )
	Return -1
END IF

is_AppPath = ls_LibraryPath
inv_orca.of_SetLibraryList(is_AppPath)
//*------------------------------------------------------------------------
//*   Create a list of all objects
//*  (Needed to determine object template)
//*------------------------------------------------------------------------
lstr_AllObjects = lstr_emptylist
inv_orca.of_Reset ( FALSE ) 
inv_orca.of_ListInheritables ( lstr_AllObjects ) 
IF UpperBound ( lstr_AllObjects ) > 0 THEN
	ids_allobjects.Object.Data = lstr_AllObjects
END IF

luo_page = iuo_wiz[ii_step]
luo_page.X = 37
luo_page.Width = 2341
luo_page.dw_messages.Visible = TRUE
luo_page.cbx_clipboard.Visible = FALSE
luo_page.cbx_file.Visible = FALSE
luo_page.gb_pasteoptions.Visible = FALSE
luo_page.st_congrats.Visible = FALSE

of_resizetab( ii_step )

//*------------------------------------------------------------------------
//*   Build the new objects
//*------------------------------------------------------------------------
of_BuildObjects ( )

iuo_wiz[ii_step].of_AddMessage ( "", -2  )
iuo_wiz[ii_step].of_AddMessage ( "", -2 )

/*  Paste the library path to the clipboard  */
of_PasteLibList ( )

//*------------------------------------------------------------------------
//*   Now that there are new objects, rebuild the object list
//*------------------------------------------------------------------------
lstr_AllObjects = lstr_emptylist
ids_objects.DataObject = "d_objectlist"
inv_orca.of_Reset ( FALSE ) 
inv_orca.of_ListInheritables ( lstr_AllObjects ) 
IF UpperBound ( lstr_AllObjects ) > 0 THEN
	ids_objects.Object.Data = lstr_AllObjects
END IF
ids_objects.SetSort ( "Library A, ObjectType A, ObjectName A" ) 
ids_objects.Sort ( ) 

/*  Tell the user object that you're done. This will create the tree   */
iuo_wiz[ii_step].Event ue_finished  ( )

IF ib_canceled THEN
	iuo_wiz[ii_step].of_AddMessage ( "Operation canceled!!!", -1 )
	f_error ( "The extension operation was canceled.  The resulting" + &
			" libraries may be unusable.", &
			"k90_libextender", "of_Finish", Information!, OK!, 1 )
ELSE
	/*  Report the results  */
	IF ii_errorcount = 0 THEN
		iuo_wiz[ii_step].of_AddMessage ( "Extensions are complete!", -1 )
		f_message ( "Complete", "Extensions have been successfully built and the new " + &
				"library list has been pasted to the clipboard.  You should" + &
				" perform a Full ReBuild of the new libraries.", &
				"k90_libextender", "of_Finish", Information!, OK!, 1 )
	ELSE
		iuo_wiz[ii_step].of_AddMessage ( String ( ii_errorcount ) + " errors occurred!!!", -1 )
		f_error ( "Errors occurred creating extensions.  The resulting" + &
				" libraries may be unusable.", &
				"k90_libextender", "of_Finish", Information!, OK!, 1 )
	END IF
END IF

SetPointer ( HourGlass! )

/*  Reset controls  */
ib_canclose = TRUE
cb_next.Enabled = TRUE
cb_print.Enabled = TRUE

Return 1
end function

public function integer of_getancestors (string as_object, ref string as_ancestors[], string as_path[]);classdefinition lcd_class, lcd_parent
int li_a

lcd_class = FindClassDefinition ( as_object, as_Path ) 
If IsValid ( lcd_class ) Then
	
	If lcd_class.IsAutoInstantiate Then
		ib_IsAuto = True
	Else
		ib_IsAuto = False
	End If
	
	lcd_parent = lcd_class.Ancestor
	If IsValid ( lcd_parent ) Then 
		
		If lcd_parent.Name = lcd_parent.DataTypeOf Then Return UpperBound ( as_ancestors ) 
		
		li_a = UpperBound(as_ancestors)+1
		as_ancestors[li_a] = lcd_parent.Name
		of_GetAncestors ( as_ancestors[li_a], as_ancestors, as_Path ) 
	Else
		Return UpperBound ( as_ancestors ) 
	End if
End If 

Destroy lcd_class
Destroy lcd_parent

Return UpperBound ( as_ancestors ) 
end function

private function integer of_buildobjects ();//*-------------------------------------------------------------------
//*  Function:			of_BuildObject
//*  Purpose:			Build the objects for the new extension level
//*-------------------------------------------------------------------
int li_ObjTypes[], li_p, li_parts, li_p2, li_AncCount[], li_Prior
long ll_obj, ll_maxobjs
long ll_end, ll_end2, ll_start, ll_findrow
string ls_Objects[], ls_Comments[], ls_Libraries[], ls_NewObject
string ls_LoadLibrary, ls_ImportLibrary, ls_userdata, ls_Ancestors[]
string ls_Super, ls_Template, ls_parts[], ls_empty[]
string ls_drive, ls_dir, ls_filename, ls_ext, ls_error, ls_Object
boolean lb_found

SetPointer ( HourGlass! )

//*------------------------------------------------------------------------
//*   Establish the list of objects to be built
//*------------------------------------------------------------------------
ids_objects.SetFilter ( "selected = 1 AND ( objecttype = 3 or objecttype = 6 or objecttype = 7)" )
ids_objects.Filter ( )

/*  Now get the sort order by establishing how many PFC
	 ancestors an object has  */
ll_end = ids_objects.RowCount ( )
ll_end2 = ids_hierarchy.RowCount ( ) 
For ll_start = 1 to ll_end
	ls_object = ids_objects.Object.ObjectName.Current[ll_start]
	ll_findrow = ids_hierarchy.Find ( "sobject='" + &
			ids_objects.Object.ObjectName.Current[ll_start] + "'", 1, ll_end2 )
	If ll_findrow > 0 Then
		ids_objects.Object.sortanc.Current[ll_start] = &
					ids_hierarchy.Object.ianccount.Current[ll_findrow]
	Else
		ids_objects.Object.sortanc.Current[ll_start] = 9
	End If
Next	 
	 
ids_objects.SetSort ( "SortAnc D, ObjectName A" )
ids_objects.Sort ( ) 

ll_maxobjs = ids_objects.RowCount ( ) 
IF ll_maxobjs <= 0 THEN
	f_error ( "No objects selected!", "k90_libextender", "of_BuildObjects", Information!, OK!, 1 )
 	Return -1
END IF

//*------------------------------------------------------------------------
//*   Get object information for selected objects
//*------------------------------------------------------------------------
li_ObjTypes = ids_objects.Object.ObjectType.Current[1, ll_maxobjs]
ls_Objects = ids_objects.Object.ObjectName.Current[1, ll_maxobjs]
ls_Comments = ids_objects.Object.Comments.Current[1, ll_maxobjs]
ls_Libraries = ids_objects.Object.Library.Current[1, ll_maxobjs]
li_AncCount = ids_objects.Object.SortAnc.Current[1, ll_maxobjs]

//*------------------------------------------------------------------------
//*   Loop thru each object and build the new hierarchy
//*------------------------------------------------------------------------
FOR ll_obj = 1 to ll_maxobjs

	/*  See if user cancelled  */
	Yield ( )
	Yield ( )
	IF this.ib_canceled THEN Return -1

	/*  Get name of library from which to load the object in ORCA  */
	ls_LoadLibrary = of_GetLoadLibrary ( ls_Objects[ll_obj] )
	
	/*  Find the object's ancestor.  If there is no PFC ancestor, then skip it. */
	ls_Ancestors = ls_empty
	If of_GetAncestors ( ls_Objects[ll_obj], ls_Ancestors, is_OrigPath ) <= 0 THEN
		/*  No ancestors  */
		Continue
	END IF

	/*  Save the name of the object's direct ancestor  */
	ls_Super = ls_Ancestors[1] 

	lb_found = FALSE
	li_parts = inv_common.f_ParseToArray ( ls_Objects[ll_obj], "_", ls_Parts ) 
	FOR li_p = 1 to li_parts
		IF ls_Parts[li_p] = is_dropprefix THEN
			ls_Parts[li_p] = is_newprefix
			lb_found = TRUE
		END IF

	NEXT 

	ls_NewObject = ""
	IF lb_found THEN
		FOR li_p = 1 to li_parts
			ls_NewObject = ls_NewObject + ls_Parts[li_p] + "_" 
		NEXT 
		IF Right ( ls_NewObject, 1 ) = "_" THEN ls_NewObject = Left ( ls_NewObject, Len ( ls_NewObject ) - 1 ) 
	ELSE
		ls_NewObject = is_newprefix + "_" + ls_Objects[ll_obj]
	END IF

	/*------------------------------------------------------------------------
		 Loop thru the selected objects and insert a new ancestor,
		 based on the selected prefix
	 -----------------------------------------------------------------------*/
	/*  Pick a template object for empty layers  */
	ls_Template = of_PickTemplate ( ls_Ancestors[], li_ObjTypes[ll_obj] ) 	

	/*  Get name of library where the object will be created  */
	inv_common.f_ParsePath ( ls_LoadLibrary, ls_drive, ls_dir, ls_filename, ls_ext )
	ls_ImportLibrary = of_GetImportLibrary ( ls_filename ) 

	IF IsNull ( ls_Comments[ll_obj] ) THEN ls_Comments[ll_obj] = ""

	IF ib_PFEIsEmpty THEN
				
		/*  Create new prefix object as a copy of the PFE object  */
		is_LoadLibrary = ls_LoadLibrary
		is_ImportLibrary = ls_ImportLibrary
		is_oldanc = ls_Super
		is_newanc = ls_Super
		is_oldobj = ls_Objects[ll_obj]
		is_newobj = ls_NewObject
		of_CreateNewObject ( li_ObjTypes[ll_obj],	ls_Comments[ll_obj], FALSE ) 
	
		/*  Recreate PFE object from template object */
		is_LoadLibrary = is_template
		is_ImportLibrary = ls_LoadLibrary
		is_oldanc = "pfcext_" + ls_Template
		is_newanc = ls_NewObject
		is_oldobj = ls_Template
		is_newobj = ls_Objects[ll_obj]
		of_CreateNewObject ( li_ObjTypes[ll_obj], ls_Comments[ll_obj], TRUE ) 

	ELSE

		/*  Create new prefix object from template object  */
		is_LoadLibrary = is_template
		is_ImportLibrary = ls_ImportLibrary
		is_oldanc = "pfcext_" + ls_Template
		is_newanc = ls_Super
		is_oldobj = ls_Template
		is_newobj = ls_NewObject
		of_CreateNewObject ( li_ObjTypes[ll_obj], ls_Comments[ll_obj], FALSE )  

		/*  Recreate PFE object be changing its ancestry */
		is_LoadLibrary = ls_LoadLibrary
		is_ImportLibrary = ls_LoadLibrary
		is_oldanc = ls_Super
		is_newanc = ls_NewObject
		is_oldobj = ls_Objects[ll_obj]
		is_newobj = ls_Objects[ll_obj]
		of_CreateNewObject ( li_ObjTypes[ll_obj], ls_Comments[ll_obj], TRUE ) 

	END IF

NEXT		/*  End of Object Loop  */

IF ib_canceled THEN Return -1

Return 1
end function

private function string of_picktemplate (readonly string as_ancestors[], readonly integer ai_objtype);//*-------------------------------------------------------------------
//*  Function:			of_PickTemplate
//*  Purpose:			Choose a standard template object
//*-------------------------------------------------------------------
int li_i, li_max
long ll_found, ll_rows
string ls_Library, ls_Source

CHOOSE CASE ai_objtype

	CASE inv_orca.II_MENU
		Return IS_MENU

	CASE inv_orca.II_WINDOW
		Return IS_WINDOW

	CASE inv_orca.II_USEROBJECT

		/*  Determine if Custom Non-Visual, Custom Visual or Standard UO  */
		li_max = UpperBound ( as_ancestors ) 
		ll_rows = ids_allobjects.RowCount ( )

		FOR li_i = 1 to li_max
			ll_found = ids_allobjects.Find ( "objectname='" + as_ancestors[li_i] + "'", 1, ll_rows )  
			IF ll_found <= 0 THEN Continue
			ls_Library = ids_allobjects.Object.Library.Current[ll_found]
			ls_Source = LibraryExport ( ls_Library, as_ancestors[li_i], ExportUserObject! )

			IF Pos ( ls_Source, "from nonvisualobject", 1 ) > 0 THEN
			
				If ib_IsAuto THEN
					Return IS_NVOAUTO
				Else
					Return IS_NVO
				End If
	
			ELSEIF Pos ( ls_Source, "from userobject", 1 ) > 0 THEN
				Return IS_CUSTOMUO
			END IF
		NEXT
					
END CHOOSE

Return IS_STDUO
end function

private function integer of_createlibrarypath (ref string as_libpath[], readonly boolean ab_pfeonly, readonly boolean ab_noapp);//*-------------------------------------------------------------------
//*  Function:			of_CreateLibraryPath
//*  Purpose:			Make a string array containing the library list
//*-------------------------------------------------------------------
int li_i, li_totlibs, li_pathlibs, li_j
string ls_LibraryName, ls_prefix
string ls_drive, ls_dir, ls_filename, ls_testfile
boolean lb_libexists, lb_applibexists

SetPointer ( HourGlass! )

li_totlibs = ids_LibNames.RowCount ( ) 

FOR li_i = 1 to li_totlibs

	ls_LibraryName = ids_LibNames.GetItemString ( li_i, "LibraryName" ) 
	IF ls_LibraryName = "" THEN Continue

	IF ab_pfeonly THEN
		/*  Only want PFE libraries this time  */
		ls_prefix = ids_LibNames.GetItemString ( li_i, "CommonName" ) 
		IF Left ( ls_prefix, 3 ) <> "pfe" THEN Continue
	END IF

	/*  Get filename only for comparison  */
	inv_common.f_ParsePath ( ls_LibraryName, ls_drive, ls_dir, ls_filename )

	/*  Make sure the library names aren't duplicated in the list  */
	li_pathlibs = UpperBound ( as_libpath )
	lb_libexists = FALSE
	FOR li_j = 1 to li_pathlibs
		inv_common.f_ParsePath ( as_libpath[li_j], ls_drive, ls_dir, ls_testfile )
		IF Lower ( ls_testfile ) = Lower ( ls_filename ) THEN
			lb_libexists = TRUE
			EXIT
		END IF
	NEXT

	IF Not lb_libexists THEN 
		li_pathlibs++
		as_libpath[li_pathlibs] = ls_LibraryName
	END IF

NEXT 

IF Not ab_noapp THEN
	/*  Make sure app library is in the list  */
	li_pathlibs = UpperBound ( as_libpath )
	FOR li_j = 1 to li_pathlibs
		IF Lower ( as_libpath[li_j] ) = Lower ( is_AppLibrary ) THEN
			lb_applibexists = TRUE
			EXIT
		END IF
	NEXT
	
	IF Not lb_applibexists THEN 
		li_pathlibs++
		as_libpath[li_j] = is_AppLibrary
	END IF
END IF
	
Return UpperBound ( as_libpath )
end function

public function string of_getimportlibrary (readonly string as_filename);//*-------------------------------------------------------------------
//*  Function:			of_GetImportLibrary
//*  Purpose:			Get the name of the library where
//*						the object will be built
//*-------------------------------------------------------------------
int li_i, li_max

li_max = ids_LibNames.RowCount ( )

FOR li_i = 1 to li_max
	IF ids_LibNames.GetItemString ( li_i, "CommonName" )  = "new" + as_filename THEN
		Return ids_LibNames.GetItemString ( li_i, "LibraryName" )
	END IF
NEXT 

Return ""
end function

public function integer of_getliblist ();//*-------------------------------------------------------------------
//*  Function:			of_GetLibList
//*  Purpose:			Estblish the names of the new libraries
//*  						and return the list to the userobject.			
//*-------------------------------------------------------------------
int li_libs, li_totlibs
long ll_i, ll_rows, ll_new
string ls_savefilter, ls_Library, ls_PrevLibrary, ls_LibraryName
string ls_NewLibraryName
string ls_drive, ls_dir, ls_filename, ls_ext

SetPointer ( HourGlass! )

ls_savefilter = ids_objects.Object.DataWindow.Table.Filter
li_totlibs = ids_LibNames.RowCount ( )

/*  Filter for selected objects only  */
ids_objects.SetFilter ( "selected = 1" )
ids_objects.Filter ( ) 
ids_objects.SetSort ( "library a" )
ids_objects.Sort ( ) 

/*  Get the names of the libraries for the selected
	 objects only and create the new library names  */
ll_rows = ids_objects.RowCount ( ) 
FOR ll_i = 1 to ll_rows
	ls_Library = ids_objects.GetItemString ( ll_i, "library" ) 
	IF ls_Library <> ls_PrevLibrary THEN
		ls_PrevLibrary = ls_Library
		FOR li_libs = 1 to li_totlibs
			ls_LibraryName = ids_LibNames.GetItemString ( li_libs, "LibraryName" )
			IF ls_LibraryName = ls_Library THEN
				/*  Create the name of the library in a new directory  */
				ls_NewLibraryName = of_CreateLibraryName ( ls_LibraryName )
				ids_LibNames.SetItem ( li_libs, "LibraryName", ls_NewLibraryName )
				inv_common.f_ParsePath ( ls_NewLibraryName, ls_drive, ls_dir, ls_filename, ls_ext ) 
				ids_LibNames.SetItem ( li_Libs, "CommonName", "pfe" + ls_filename )
				
				/*  Now create a new PBL name for the inserted layer  */
				ll_new = ids_LibNames.InsertRow ( 0 ) 
				ls_NewLibraryName = of_CreateLibraryName ( ls_LibraryName, is_newprefix, "" )
				ids_LibNames.SetItem ( ll_new, "LibraryName", ls_NewLibraryName )
				ids_LibNames.SetItem ( ll_new, "OrigLibName", ls_LibraryName )
				ids_LibNames.SetItem ( ll_new, "CommonName", "new" + ls_filename )
			END IF
		NEXT
	END IF
NEXT

/*  Restore the filter  */
IF ls_savefilter = "?" THEN
	ids_objects.SetFilter ( "" )
ELSE
	ids_objects.SetFilter ( ls_savefilter )
END IF
ids_objects.Filter ( )

Return ids_LibNames.RowCount ( ) 
end function

private function integer of_pasteliblist ();//*-------------------------------------------------------------------
//*  Function:			of_PasteLibList
//*  Purpose:			Paste the library list to the clipboard
//*-------------------------------------------------------------------
int li_i, li_totlibs, li_file
string ls_LibList, ls_Library
string ls_drive, ls_dir, ls_file

SetPointer ( HourGlass! )

IF ( Not ib_pastefile ) &
AND ( Not ib_pasteclip ) THEN Return -1

li_totlibs = ids_LibNames.RowCount ( ) 
FOR li_i = 1 to li_totlibs

	IF ids_LibNames.GetItemString ( li_i, "CommonName" ) = "sample" THEN Continue

	ls_Library = ids_LibNames.GetItemString ( li_i, "LibraryName" )

	/*  Don't add the same library twice  */
	IF Pos ( Lower ( ls_LibList ), Lower ( ls_Library ) ) > 0 THEN
		Continue
	END IF

	ls_LibList = ls_LibList + ls_Library + ";~r~n"
NEXT 

IF ib_pasteclip THEN 
	ClipBoard ( ls_LibList ) 
END IF

IF ib_pastefile THEN 
	inv_common.f_ParsePath ( ls_Library, ls_drive, ls_dir, ls_file ) 
	ls_file = inv_common.f_AssemblePath ( ls_drive, ls_dir + inv_common.is_Separator, "libpath.txt" )
	li_file = FileOpen ( ls_file, StreamMode!, Write!, LockReadWrite!, Replace! )
	IF li_file > 0 THEN 
		FileWrite ( li_file, ls_LibList )
	END IF 
	FileClose ( li_file )
END IF

Return 1
end function

private function integer of_copypfe (readonly string as_oldpfename, readonly string as_newpfename);//*-------------------------------------------------------------------
//*  Function:			of_CopyPFE
//*  Purpose:			Makes a copy of a PFE Library to	a new location
//*-------------------------------------------------------------------
string ls_drive, ls_dir, ls_filename, ls_pathonly

SetPointer ( HourGlass! )

/*  Parse the filename into its components  */
inv_common.f_ParsePath ( as_newpfename, ls_drive, ls_dir, ls_filename ) 
ls_pathonly = inv_common.f_AssemblePath ( ls_drive, ls_dir + inv_common.is_Separator, "" ) 

/*  Determine if the path exists, otherwise create  */
IF Not inv_common.f_DirectoryExists ( ls_pathonly ) THEN 
	IF inv_common.f_CreateDirectory ( ls_pathonly ) <> 1 THEN 
		f_error ( "Unable to create directory '" + ls_pathonly + "'." + &
			" Cannot continue process.", "k90_libextender", "of_copypfe", StopSign!, OK!, 1 )
		Return -1
	END IF
END IF

///*  See if Library Already Exists  */
//IF FileExists ( as_newpfename  ) THEN
//	IF ii_msg = 0 THEN
//		ii_msg = f_error ( 2, "At least some of the libraries already exist in " + ls_pathonly + ".  Is it OK to" + &
//			" overwrite them all?", &
//			"of_CopyPFE", "k90_libextender", YesNo!, 1, "" )
//	END IF
//	IF ii_msg = 2 THEN Return -1
//END IF

SetPointer ( HourGlass! )

inv_common.f_FileCopy ( as_oldpfename, as_newpfename )

Return 1
end function

private function string of_createlibraryname (string as_oldpfename);//*-------------------------------------------------------------------
//*  Function:			of_CreateLibraryName
//*  Purpose:			Create the new PFE library name.
//*						( Dir: NewExt + <old lib name> )
//*-------------------------------------------------------------------
string ls_dir, ls_drive, ls_filename

SetPointer ( HourGlass! )

inv_common.f_ParsePath ( as_oldpfename, ls_drive, ls_dir, ls_filename ) 

Return inv_common.f_AssemblePath ( ls_drive, ls_dir + inv_common.is_Separator + "NewExt", ls_filename )
end function

private function string of_createlibraryname (string as_oldpfename, string as_prefix, string as_genericname);//*-------------------------------------------------------------------
//*  Function:			of_CreateLibraryName
//*  Purpose:			Given the prefix and the PFE name, make the 
//*						name of the new level PBL name.
//*-------------------------------------------------------------------
string ls_dir, ls_drive, ls_filename

SetPointer ( HourGlass! )

inv_common.f_ParsePath ( as_oldpfename, ls_drive, ls_dir, ls_filename ) 

/*  Make sure you didn't get the name of the new pfe library  */
ls_dir = inv_common.f_GlobalReplace ( ls_dir, "\NewExt", "", True )

/*  Parse the prefix name out of the filename,
	 For example, 'pfeapsrv' yielding 'apsrv'  */
ls_filename = inv_common.f_GlobalReplace ( ls_filename, is_dropprefix, "", True )

ls_filename = as_prefix + ls_filename

Return inv_common.f_AssemblePath ( ls_drive, ls_dir + inv_common.is_Separator + "NewExt", ls_filename )
end function

public function integer of_getprefixes (ref datastore adw);//*-------------------------------------------------------------------
//*  Function:			of_GetPrefixes
//*  Purpose:			For all objects in the library path,
//*						Load the hierarchical Prefixes
//*-------------------------------------------------------------------
int li_obj, li_maxobj, li_prefix, li_anc, li_maxanc, li_pfc, li_anccount
string ls_AppPath[], ls_userdata, ls_Ancestors[], ls_AncestorName, ls_ObjPrefix, ls_empty[]
s_objectinfo lstr_objects[], lstr_empty[]

SetPointer ( HourGlass! )

adw = ids_hierarchy

IF ids_objects.RowCount ( ) > 0 THEN 
	lstr_objects = ids_objects.Object.Data 
ELSE
	f_error ( "No qualifying objects found in the selected libraries.  Please step back " + &
		"and review your library list", "k90_libextender", &
		"of_GetPrefixes", Information!, OK!, 1 )
	Return -1
END IF

of_CreateLibraryPath ( ls_AppPath, FALSE, FALSE )

/*  Clear the existing hierarchy  */
ids_hierarchy.Reset ( ) 

OpenWithParm ( base_progress, "" ) 

li_maxobj = UpperBound ( lstr_objects ) 
base_progress.f_SetRange ( 1, li_maxobj )

FOR li_obj = 1 to li_maxobj

	If base_progress.f_Progress ( lstr_objects[li_obj].sObject ) Then
		Close ( base_progress )
		Return -1
	End If
	
	IF  (lstr_objects[li_obj].iObjType <> inv_orca.II_WINDOW ) &
	AND (lstr_objects[li_obj].iObjType <> inv_orca.II_USEROBJECT ) &
	AND (lstr_objects[li_obj].iObjType <> inv_orca.II_MENU ) THEN
		Continue
	END IF

	li_prefix = 0
	li_pfc = 0 

	li_anccount = 0
	ls_ancestors = ls_empty
	li_maxanc = of_GetAncestors ( lstr_objects[li_obj].sObject, ls_Ancestors, ls_AppPath ) 
	
	IF li_maxanc > 0 THEN
		/*  Get the prefix for the object itself  */
		ls_ObjPrefix = of_GetPrefixNameForObject ( lstr_objects[li_obj].sObject, lstr_objects[li_obj].sObject )
		IF ls_ObjPrefix = "<level @>" THEN
				ls_ObjPrefix = inv_common.f_GlobalReplace ( ls_ObjPrefix, "@", "0", True ) 
		END IF
		/*  Determine how many PFC ancestors this object has  */
		FOR li_anc = 1 to li_maxanc
			ls_AncestorName = of_GetPrefixName ( ls_Ancestors[li_anc], lstr_objects[li_obj].sObject )
			IF ls_AncestorName = "pfc" THEN
				li_anccount++
			END IF
		NEXT
		/*  First find the pfc ancestor index.  This is where to start from.
		 This prevents strange hierarchies when picking an object like w_print  */
		FOR li_anc = 1 to li_maxanc
			ls_AncestorName = of_GetPrefixName ( ls_Ancestors[li_anc], lstr_objects[li_obj].sObject )
			IF ls_AncestorName = "pfc" THEN
				li_pfc = li_anc
				EXIT
			END IF
		NEXT
		IF li_pfc = 0 THEN li_pfc = li_maxanc

		FOR li_anc = li_pfc to 1 STEP -1
			li_prefix++ 
			ls_AncestorName = of_GetPrefixName ( ls_Ancestors[li_anc], lstr_objects[li_obj].sObject )
			IF ls_AncestorName = "<level @>" THEN
				ls_AncestorName = inv_common.f_GlobalReplace ( ls_AncestorName, "@", String ( li_prefix ), True ) 
			END IF
			ids_hierarchy.ImportString ( lstr_objects[li_obj].sObject + "~t" + &
				lstr_objects[li_obj].sLibrary + "~t" + ls_AncestorName + "~t" + &
				String ( li_prefix ) + "~t" + String ( lstr_objects[li_obj].iObjType ) + "~t~t" + &
					ls_ObjPrefix + "~t" + String ( li_anccount )  )
		NEXT

		ids_hierarchy.ImportString ( lstr_objects[li_obj].sObject + "~t" + &
				lstr_objects[li_obj].sLibrary + "~tpfe~t" + &
				String ( li_prefix + 1) + "~t" + String ( lstr_objects[li_obj].iObjType ) + "~t" + &
				ls_AncestorName + "~t" + ls_ObjPrefix + "~t" + String ( li_anccount )  )
	END IF

NEXT

Close ( base_progress )

Return 1
end function

public function integer of_settarget (readonly string as_target, readonly boolean ab_skip);//*-------------------------------------------------------------------
//*  Function:			of_SetTarget
//*  Purpose:			Save the target file name
//*-------------------------------------------------------------------
/*  Indicator that Page 2 needs to be rebuilt  */
ib_build2 = TRUE

IF ab_skip <> ib_noapp THEN
	IF UpperBound ( iuo_wiz ) >= 2 THEN
		IF IsValid ( iuo_wiz[2] ) THEN
			CloseUserObject ( iuo_wiz[2] )
		END IF
	END IF
END IF

IF ab_skip THEN
	is_AppName = "pfcext_application_template"
	is_AppLibrary = is_Template

ELSE
	
	inv_common.f_SetTargetFile ( as_target )
	is_AppName = inv_common.f_GetTargetApplication ( )
	is_AppLibrary = inv_common.f_GetTargetAppLib ( )

END IF

ib_noapp = ab_skip

IF ab_skip THEN 
	is_wizuos [2] = "k90_page2a"
ELSE
	is_wizuos [2] = "k90_page2b"
END IF

Return 1
end function

private function integer of_createorigpath (ref string as_libpath[]);//*-------------------------------------------------------------------
//*  Function:			of_CreateOrigPath
//*  Purpose:			Make a string array containing the original library list
//*-------------------------------------------------------------------
int li_i, li_totlibs, li_pathlibs
string ls_LibraryName

SetPointer ( HourGlass! )

li_totlibs = ids_LibNames.RowCount ( ) 

FOR li_i = 1 to li_totlibs

	ls_LibraryName = ids_LibNames.GetItemString ( li_i, "LibraryName" ) 
	IF ls_LibraryName = "" THEN Continue

	li_pathlibs++
	as_libpath[li_pathlibs] = ls_LibraryName

NEXT 

Return UpperBound ( as_libpath )
end function

private function string of_getprefixnameforobject (string as_objectname, string as_pfename);//*-------------------------------------------------------------------
//*  Function:			of_GetPrefixNameforObject
//*  Purpose:			Obtain a prefix from object name.
//*						Call this for the object itself.
//*					   This identifies the object as being part of 
//*						a "pfe" layer which you can't determine
//*						based on a named prefix.
//*-------------------------------------------------------------------
string ls_prefix, ls_parts[]
int li_p

li_p = inv_common.f_ParseToArray ( as_objectname, "_", ls_parts ) 

ls_prefix = ls_parts[1]

IF ls_prefix = "w" &
OR ls_prefix = "u" &
OR ls_prefix = "n" &
OR ls_prefix = "m" THEN
	ls_prefix = ""
END IF

IF ls_prefix = "" &
AND li_p > 2 THEN
	IF  ls_parts[2] <> "cst" &
	AND ls_parts[2] <> "dwproperty" &
	AND ls_parts[2] <> "st" THEN
		ls_prefix = ls_parts[2]
	END IF
END IF

Return ls_prefix
end function

public function integer of_setresizeable (boolean ab_showgrip);//A few constants we'll need for the Windows API calls
CONSTANT int 	GWL_STYLE  			= -16

CONSTANT ulong	WS_SYSMENU 			= 524288
CONSTANT ulong WS_THICKFRAME 		= 262144
CONSTANT ulong WS_MINIMIZEBOX		= 65536
CONSTANT ulong WS_MAXIMIZEBOX		= 131072

CONSTANT uint 	SC_RESTORE      	= 61728
CONSTANT uint 	SC_MAXIMIZE     	= 61488
CONSTANT uint 	SC_MINIMIZE     	= 61472
CONSTANT uint	SC_SIZE				= 61440

CONSTANT uint 	MF_BYCOMMAND 		= 0
CONSTANT uint	MF_STRING			= 0
CONSTANT uint	MF_BYPOSITION		= 1024

CONSTANT uint	SWP_NOSIZE			= 1
CONSTANT uint	SWP_NOMOVE			= 2
CONSTANT uint	SWP_NOZORDER		= 4
CONSTANT uint	SWP_FRAMECHANGED	= 32

ulong 	ll_style, hMenu, hWnd, ll_newstyle

hWnd = Handle( this )

//Turn on the instance variable that is used elsewhere to see if the window is resizeable
ib_resizeable = TRUE

//Get the original size of the window, which we will prevent the user from sizing the window
//less than
il_orig_width = this.width
il_orig_height = this.height

//Get the current window style
ll_style = GetWindowLong(handle(this), gwl_style)

IF this.ControlMenu THEN
	//You have to include the MINIMIZEBOX attribute to get the controls to show
	ll_newstyle = ll_style + WS_THICKFRAME + WS_MINIMIZEBOX
ELSE
	ll_newstyle = ll_style + WS_THICKFRAME
END IF

IF ll_style <> 0 THEN
	IF SetWindowLong ( hWnd, gwl_style, ll_newstyle ) <> 0 THEN
		IF this.ControlMenu THEN
			//Get a handle to the system menu
			hMenu = GetSystemMenu( hWnd, FALSE )
			IF hMenu > 0 THEN
				InsertMenu( hMenu, 1, MF_BYPOSITION + MF_STRING, SC_MAXIMIZE, "Maximize" ) ;
				InsertMenu( hMenu, 1, MF_BYPOSITION + MF_STRING, SC_RESTORE, "Restore" ) ;
				//The Size menu option has to be added to allow the resize gripper to work
				//if there is a control menu
				InsertMenu( hMenu, 1, MF_BYPOSITION + MF_STRING, SC_SIZE, "Size" ) ;
				DrawMenuBar( hWnd )
			END IF
		END IF
		//Force a repaint
		SetWindowPos ( hWnd, 0, 0, 0, 0, 0, SWP_NOSIZE + SWP_NOMOVE + SWP_NOZORDER + SWP_FRAMECHANGED )
		this.SetRedraw( TRUE )
		//Track whether or not the size grip should show
		ib_show_szgrip = ab_showgrip
	END IF
END IF

Return 1
end function

protected subroutine of_resizetab (integer ai_step);IF ai_step = ii_maxsteps THEN
	iuo_wiz [ai_step].Resize ( this.width, this.height - 400 )	
ELSE
	iuo_wiz [ai_step].Resize ( this.width - 600 , this.height - 400 )
END IF
end subroutine

on k90_libextender.create
int iCurrent
call super::create
this.sizegrip=create sizegrip
this.cb_print=create cb_print
this.st_info=create st_info
this.cb_help=create cb_help
this.cb_cancel=create cb_cancel
this.cb_next=create cb_next
this.cb_back=create cb_back
this.gb_line=create gb_line
this.p_1=create p_1
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.sizegrip
this.Control[iCurrent+2]=this.cb_print
this.Control[iCurrent+3]=this.st_info
this.Control[iCurrent+4]=this.cb_help
this.Control[iCurrent+5]=this.cb_cancel
this.Control[iCurrent+6]=this.cb_next
this.Control[iCurrent+7]=this.cb_back
this.Control[iCurrent+8]=this.gb_line
this.Control[iCurrent+9]=this.p_1
end on

on k90_libextender.destroy
call super::destroy
destroy(this.sizegrip)
destroy(this.cb_print)
destroy(this.st_info)
destroy(this.cb_help)
destroy(this.cb_cancel)
destroy(this.cb_next)
destroy(this.cb_back)
destroy(this.gb_line)
destroy(this.p_1)
end on

event open;call super::open;//*-------------------------------------------------------------------
//*  Event:				open
//*  Purpose:			Initialization and open first wizard step
//*-------------------------------------------------------------------
/*  Dummy references so these will go into the EXE  */
k90_page1 luo1
k90_page2a luo2a
k90_page2b luo2b
k90_page3 luo3
k90_page4 luo4
k90_page5 luo5
k90_page6 luo6
k90_page7 luo7

SetPointer ( HourGlass! )

f_SetCommon ( inv_common, TRUE )
f_SetOrca ( inv_orca, TRUE )

inv_orca.of_startorca ( )

is_CurrentDirectory = inv_common.f_GetCurrentDirectory (  )

ids_objects = CREATE datastore
ids_objects.DataObject = "d_objectlist"

ids_allobjects = CREATE datastore
ids_allobjects.DataObject = "d_objectlist"

ids_hierarchy = CREATE datastore
ids_hierarchy.DataObject = "k90_hierarchy"

ids_libnames = CREATE datastore
ids_libnames.DataObject = "k90_librarynames"

/*  These are the wizard steps userobjects  */
is_wizuos [1] = "k90_page1" 
is_wizuos [2] = "k90_page2a"
is_wizuos [3] = "k90_page3" 
is_wizuos [4] = "k90_page4" 
is_wizuos [5] = "k90_page5" 
is_wizuos [6] = "k90_page6" 
is_wizuos [7] = "k90_page7" 

//return 1

of_setresizeable( TRUE )

this.Post Event ue_postopen ( )

/*  Start at the first step  */
this.Event ue_step ( 1 )
end event

event close;//*-------------------------------------------------------------------
//*  Event:				close
//*  Purpose:			Clean-up
//*-------------------------------------------------------------------
int li_i, li_totsteps

li_totsteps = UpperBound ( iuo_wiz )

FOR li_i = 1 to li_totsteps
	IF IsValid ( iuo_wiz[li_i] ) THEN
		CloseUserObject ( iuo_wiz[li_i] )
	END IF
NEXT

DESTROY ids_objects
DESTROY ids_allobjects
DESTROY ids_libnames
DESTROY ids_hierarchy

inv_orca.of_stoporca ( )
f_SetORCA ( inv_orca, FALSE )
f_SetCommon ( inv_common, FALSE )
end event

event resize;call super::resize;integer li_height, li_width, li_index, li_count

//If we're using a resizeable response window
IF ib_resizeable THEN
	//Move the resizing grip
	sizegrip.Move(newwidth - sizegrip.width, newheight - sizegrip.height)
	IF NOT sizegrip.visible AND ib_show_szgrip THEN 
		sizegrip.Show()
	END IF
END IF

cb_back.Move ( cb_back.x, newheight - 150 )
cb_cancel.Move ( cb_cancel.x, newheight - 150 )
cb_help.Move ( cb_help.x, newheight - 150 )
cb_next.Move ( cb_next.x, newheight - 150 )
cb_print.Move ( cb_print.x, newheight - 150 )

li_count = UpperBound ( iuo_wiz )
FOR li_index = 1 TO li_count
	of_resizetab ( li_index )
NEXT
end event

type sizegrip from hscrollbar within k90_libextender
boolean visible = false
integer x = 2341
integer y = 1344
integer width = 73
integer height = 48
end type

event constructor;CONSTANT int 	GWL_STYLE		= -16
CONSTANT int 	SBS_SIZEGRIP	= 16
CONSTANT int	SM_CYHSCROLL	= 3
CONSTANT int 	SM_CXHSCROLL	= 21

CONSTANT uint	SWP_NOSIZE			= 1
CONSTANT uint	SWP_NOMOVE			= 2
CONSTANT uint	SWP_NOZORDER		= 4
CONSTANT uint	SWP_FRAMECHANGED	= 32

ulong ll_style

ll_style = GetWindowLong(handle(this), gwl_style)

IF ll_style <> 0 THEN
	IF SetWindowLong(handle(this), gwl_style, ll_style +  SBS_SIZEGRIP) <> 0 THEN
		width = (PixelsToUnits( GetSystemMetrics( SM_CXHSCROLL ) , XPixelsToUnits! ) )
		height = (PixelsToUnits( GetSystemMetrics( SM_CYHSCROLL ), YPixelsToUnits! ) )
		SetWindowPos ( handle(this), 0, 0, 0, 0, 0, SWP_NOSIZE + SWP_NOMOVE + SWP_NOZORDER + SWP_FRAMECHANGED )
	END IF
END IF

end event

type cb_print from commandbutton within k90_libextender
boolean visible = false
integer x = 864
integer y = 1248
integer width = 334
integer height = 92
integer taborder = 70
integer textsize = -8
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
boolean enabled = false
string text = "&Print"
end type

event clicked;//*-------------------------------------------------------------------
//*  Event:				clicked
//*  Purpose:			Print the error messages
//*-------------------------------------------------------------------
iuo_wiz[ii_step].Event ue_print ( )
end event

type st_info from statictext within k90_libextender
boolean visible = false
integer x = 1714
integer y = 464
integer width = 1591
integer height = 60
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 33554432
long backcolor = 79416533
boolean enabled = false
boolean focusrectangle = false
end type

type cb_help from commandbutton within k90_libextender
integer x = 91
integer y = 1248
integer width = 334
integer height = 92
integer taborder = 50
integer textsize = -8
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "&Help"
end type

event clicked;//*-------------------------------------------------------------------
//*  Event:				clicked
//*  Purpose:			Invoke the Help Topic
//*-------------------------------------------------------------------
int li_helptopic

CHOOSE CASE ii_step
		
	CASE 1
		li_helptopic = 360
	CASE 2
		IF ib_noapp THEN
			li_helptopic = 361
		ELSE
			li_helptopic = 362
		END IF 
	CASE 3
		li_helptopic = 363
	CASE 4
		li_helptopic = 364
	CASE 5
		li_helptopic = 365
	CASE 6
		li_helptopic = 366
	CASE 7
		li_helptopic = 367	
		
END CHOOSE

ShowHelp ( "pbhlp105.hlp", Topic!, li_helptopic )  
end event

event losefocus;IF IsValid ( iuo_wiz[ii_step] ) THEN
	iuo_wiz[ii_step].idrg_focus.SetFocus ( )
END IF
end event

type cb_cancel from commandbutton within k90_libextender
integer x = 1943
integer y = 1248
integer width = 334
integer height = 92
integer taborder = 40
integer textsize = -8
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "Cancel"
boolean cancel = true
end type

event clicked;//*-------------------------------------------------------------------
//*  Event:				clicked
//*  Purpose:			First time, cancel the build process
//*-------------------------------------------------------------------
ib_canceled = TRUE

IF ib_CanCLose THEN
	IF MessageBox ( "Exit", "Are you sure you want to exit the PFC Library Extender?", &
			Question!, YesNo!, 1 ) = 1 THEN
		Close ( Parent ) 
 		Return
	END IF
END IF

ib_canclose = TRUE
end event

type cb_next from commandbutton within k90_libextender
integer x = 1559
integer y = 1248
integer width = 334
integer height = 92
integer taborder = 20
integer textsize = -8
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
boolean enabled = false
string text = "Next >"
boolean default = true
end type

event clicked;//*-------------------------------------------------------------------
//*  Event:				clicked 
//*  Purpose:			Go to next step
//*-------------------------------------------------------------------
int li_step

SetPointer ( HourGlass! ) 

/*  Increase step  */
li_step = ii_step + 1

IF this.Text = "&Finish" THEN
	IF of_Finish ( ) = 1 THEN
		this.Text = "E&xit"
	END IF
ELSEIF this.Text = "E&xit" THEN
	Close ( Parent )
ELSE
	Parent.Event ue_step ( li_step )
END IF
end event

type cb_back from commandbutton within k90_libextender
integer x = 1216
integer y = 1248
integer width = 334
integer height = 92
integer taborder = 30
integer textsize = -8
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
boolean enabled = false
string text = "< &Back"
end type

event clicked;//*-------------------------------------------------------------------
//*  Event:				clicked 
//*  Purpose:			Go to previous step
//*-------------------------------------------------------------------
int li_step

/*  Decrease step  */
li_step = ii_step - 1

Parent.Event ue_step ( li_step )
end event

type gb_line from groupbox within k90_libextender
integer x = 50
integer y = 1176
integer width = 2258
integer height = 32
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 255
long backcolor = 75562880
end type

type p_1 from picture within k90_libextender
integer x = 37
integer y = 136
integer width = 549
integer height = 920
string picturename = "pfcext7.bmp"
boolean focusrectangle = false
end type


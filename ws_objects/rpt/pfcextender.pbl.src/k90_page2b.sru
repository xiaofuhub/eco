$PBExportHeader$k90_page2b.sru
$PBExportComments$(K90) - PFC Extender Step 2b:  Specify Application Libraries
forward
global type k90_page2b from k90_page
end type
type st_instruct from statictext within k90_page2b
end type
type st_lblabel from statictext within k90_page2b
end type
type tv_filelist from u_library_painter within k90_page2b
end type
type tv_selectedlibs from treeview within k90_page2b
end type
type cb_reset from commandbutton within k90_page2b
end type
end forward

global type k90_page2b from k90_page
string tag = "Specify Application Library List"
st_instruct st_instruct
st_lblabel st_lblabel
tv_filelist tv_filelist
tv_selectedlibs tv_selectedlibs
cb_reset cb_reset
end type
global k90_page2b k90_page2b

type variables
/*  When true, folder is being dragged  */
boolean ib_folderdrag
/*  The handle of item being dragged  */
long il_dragsource
end variables

forward prototypes
public function integer of_addlibrarytolist (readonly long al_item)
public function boolean of_checkrequired ()
public function integer of_savelibrarylist ()
public function boolean of_checkfordupelibrary (readonly string as_addlibrary)
public function integer of_deletelibraryfromlist (readonly long al_item)
public function integer of_getliblistfrominifile ()
end prototypes

public function integer of_addlibrarytolist (readonly long al_item);//*-------------------------------------------------------------------
//*  Function:			of_AddLibraryToList 
//*  Purpose:			Add libraries to the selected list
//*-------------------------------------------------------------------
long ll_child
string ls_library, ls_drive, ls_dir, ls_file, ls_ext
treeviewitem ltvi_item, ltvi_child

SetPointer ( HourGlass! )

/*  Get the item to be added  */
tv_filelist.GetItem ( al_item, ltvi_item ) 

/*  Prevent errors when dragging a folder with non-PFC Libraries  */
ib_folderdrag = TRUE

/*  If a library is D-Clicked, then add it to the selected path  */
IF al_item > 0 THEN 

	/*  User clicked on a library  */
	IF NOT ltvi_item.Children THEN
		/*  Eliminate duplicate libraries  */
		ls_library = Lower ( String ( ltvi_item.Data ) )
		IF Not of_CheckforDupeLibrary ( ls_library )THEN
			tv_selectedlibs.InsertItemSort ( 0, ls_library, 1 ) 
			iw_parent.inv_common.f_ParsePath ( ls_library, ls_drive, ls_dir, ls_file, ls_ext )
			iw_parent.ids_LibNames.ImportString ( ls_library +	"~t~t" + &
					ls_library + "~t" + ls_file ) 
			ib_changed = TRUE
		END IF
	ELSE
		/*  User clicked on a directory - so add all the libraries
			 within to the selected path  */
		ll_child = tv_filelist.FindItem ( ChildTreeItem!, al_item ) 
		DO UNTIL ll_child <= 0
			tv_filelist.GetItem ( ll_child, ltvi_child ) 
			ls_library = Lower ( String ( ltvi_child.Data ) )
			IF Pos ( ls_library, ".pbl" ) <= 0 THEN
				ll_child = tv_filelist.FindItem ( NextTreeItem!, ll_child ) 
				Continue
			END IF
	
			/*  See if file exist  */
			IF Not FileExists ( ls_library) THEN
				f_error ( "The file " + ls_library + " no longer exists!", &
					"k90_page2b", "of_AddLibraryToList", Information!, OK!, 1 )
			ELSE
				/*  Eliminate duplicate libraries  */
				IF Not of_CheckforDupeLibrary ( ls_library ) THEN
					/*  Add the library to the list  */
					tv_selectedlibs.InsertItemSort ( 0, Lower(ls_library), 1 ) 
					iw_parent.inv_common.f_ParsePath ( ls_library, ls_drive, ls_dir, ls_file, ls_ext )
					iw_parent.ids_LibNames.ImportString ( ls_library +	"~t~t" + &
							ls_library + "~t" + ls_file ) 
					ib_changed = TRUE
				END IF
			END IF
	
			ll_child = tv_filelist.FindItem ( NextTreeItem!, ll_child ) 

		LOOP

	END IF

END IF

ib_folderdrag = FALSE

/*  If list is complete, enable the Next button  */
iw_parent.Event ue_next ( of_CheckRequired ( ) ) 

Return 1
end function

public function boolean of_checkrequired ();//*-------------------------------------------------------------------
//*  Function:			of_CheckRequired
//*  Purpose:			Is the library list complete ?
//*-------------------------------------------------------------------
Return ( iw_parent.ids_LibNames.RowCount ( ) > 0 ) 
end function

public function integer of_savelibrarylist ();//*-------------------------------------------------------------------
//*  Function:			of_SaveLibraryList
//*  Purpose:			Save the Library List in the Registry
//*-------------------------------------------------------------------
SetPointer ( HourGlass! )

/*  Pages 3,5,6 should be rebuilt on focus  */
iw_parent.ib_build3 = TRUE
iw_parent.ib_build5 = TRUE
iw_parent.ib_build6 = TRUE

/*  List the objects  */
Return iw_parent.of_GetObjectSelection ( )
end function

public function boolean of_checkfordupelibrary (readonly string as_addlibrary);//*-------------------------------------------------------------------
//*  Function:			of_CheckforDupeLibrary 
//*  Purpose:			Make sure that the library to be selected is
//*						not already in the list
//*-------------------------------------------------------------------
string ls_drive, ls_dir, ls_library, ls_ext
boolean lb_match, lb_matchpath
long ll_max, ll_found

SetPointer ( HourGlass! )

ll_max = iw_parent.ids_LibNames.RowCount ( ) 

/*  Look for match on PBL name only  */
iw_parent.inv_common.f_ParsePath ( Lower ( as_addlibrary ), ls_drive, ls_dir, ls_library, ls_ext ) 
ll_found = iw_parent.ids_LibNames.Find ( "LibNoPath='" + ls_library + "'", 1, ll_max ) 
IF ll_found > 0 THEN lb_match = TRUE

/*  Look for match on Path and PBL name  */
ll_found = iw_parent.ids_LibNames.Find ( "LibraryName='" + as_addlibrary + "'", 1, ll_max ) 
IF ll_found > 0 THEN lb_matchpath = TRUE

CHOOSE CASE TRUE
	/*  Add Library matches library in the list  */
	CASE lb_matchpath			
		Return TRUE					/*  Ok, simply don't add it  */

	/* Add Library matches on PBL name only */
	CASE lb_match					/*  Display message and allow user to choose  */
		IF MessageBox ( "Duplicate Library", "The library '" + as_addlibrary + &
				"' is already selected in the library list, but from a different" + &
				" path.  Do you wish to add anyway?", Question!, YesNo!, 2 ) = 1 THEN
			Return FALSE			/*  Ok, Add anyway  */
		ELSE
			Return TRUE				/*  Not OK, Don't add  */
		END IF

	/* Add Library doesn't match any condition */
	CASE ELSE
		Return FALSE

END CHOOSE
end function

public function integer of_deletelibraryfromlist (readonly long al_item);//*-------------------------------------------------------------------
//*  Function:			of_DeleteLibraryFromList 
//*  Purpose:			Delete libraries from the selected list
//*-------------------------------------------------------------------
long ll_row
string ls_library
treeviewitem ltvi_item

SetPointer ( HourGlass! )

IF al_item > 0 THEN

	/*  Get the item to be deleted and save the library name  */
	tv_selectedlibs.GetItem ( al_item, ltvi_item ) 
	ls_library = ltvi_item.Label

	tv_selectedlibs.DeleteItem ( al_item )
	ib_changed = TRUE
	
	/*  Find the corresponding row in the library list and delete it  */
	ll_row = iw_parent.ids_LibNames.Find ( "Lower(LibraryName)='" + Lower(ls_library) + "'", &
			1, iw_parent.ids_LibNames.RowCount ( ) ) 
	IF ll_row > 0 THEN iw_parent.ids_LibNames.DeleteRow ( ll_row )

	/*  If the list is complete, enable the Next button  */
	iw_parent.Event ue_next ( of_CheckRequired ( ) ) 

	Return 1

ELSE

	Return -1
END IF
end function

public function integer of_getliblistfrominifile ();////*-------------------------------------------------------------------
////*  Function:			of_GetLibListFromIniFile
////*  Purpose:			Locates pb.ini and optains the application
////*					   search path for this application
////*-------------------------------------------------------------------
//int li_i, li_libs
//string ls_appname, ls_applibrary, ls_appkey, ls_appstring
//string ls_pbini, ls_libs[]
//string ls_drive, ls_dir, ls_file, ls_ext
//
//SetPointer ( HourGlass! )
//
//iw_parent.ids_LibNames.Reset ( ) 
//tv_selectedlibs.DeleteItem ( 0 )
//
///*  Add the application library to the list  */
//ls_applibrary = Lower ( iw_parent.of_GetAppLibrary ( ) )
//tv_selectedlibs.InsertItemSort ( 0, ls_applibrary, 2 )
// 
//iw_parent.inv_common.f_ParsePath ( ls_applibrary, ls_drive, ls_dir, ls_file, ls_ext )
//iw_parent.ids_LibNames.ImportString ( ls_applibrary +	"~t~t" + &
//			ls_applibrary + "~t" + ls_file ) 
//
///*  Get the path to PB.INI  */
//iw_parent.inv_common.f_GetPBIniPath ( ls_pbini )
//IF Not FileExists ( ls_pbini ) THEN	Return -1
//
///*  Get the application path key  */
//ls_appname = iw_parent.of_GetAppName ( )
//ls_appkey = "$" + ls_applibrary + "(" + ls_appname + ")"
//ls_appstring = ProfileString ( ls_pbini, "Application", ls_appkey, "" )
//
//iw_parent.inv_common.f_ParseToArray ( ls_appstring, ";", ls_libs ) 
//
///*  Set the library search path  */
//li_libs = UpperBound ( ls_libs )
//FOR li_i = 1 to li_libs
//	IF Lower ( ls_libs[li_i] ) = Lower ( ls_applibrary ) THEN Continue
//	IF Not of_CheckForDupeLibrary ( ls_libs[li_i] ) THEN
//		tv_selectedlibs.InsertItemSort ( 0, Lower ( ls_libs[li_i] ), 1 ) 
//		iw_parent.inv_common.f_ParsePath ( Lower ( ls_libs[li_i] ), ls_drive, ls_dir, ls_file, ls_ext )
//		iw_parent.ids_LibNames.ImportString ( Lower ( ls_libs[li_i] ) + "~t~t" + &
//				Lower ( ls_libs[li_i] ) + "~t" + ls_file ) 
//	END IF
//NEXT
//
//ib_changed = TRUE
//
//Return 1
//*-------------------------------------------------------------------
//*  Function:			of_GetLibListFromIniFile
//*  Purpose:			Locates pb.ini and optains the application
//*					   search path for this application
//*-------------------------------------------------------------------
int li_i, li_libs
string ls_appname, ls_applibrary, ls_appkey, ls_appstring
string ls_pbini, ls_libs[]
string ls_drive, ls_dir, ls_file, ls_ext

SetPointer ( HourGlass! )

iw_parent.ids_LibNames.Reset ( ) 
tv_selectedlibs.DeleteItem ( 0 )

/*  Add the application library to the list  */
ls_applibrary = Lower ( iw_parent.of_GetAppLibrary ( ) )
tv_selectedlibs.InsertItemSort ( 0, ls_applibrary, 2 )
 
iw_parent.inv_common.f_ParsePath ( ls_applibrary, ls_drive, ls_dir, ls_file, ls_ext )
iw_parent.ids_LibNames.ImportString ( ls_applibrary + "~t~t" + &
			ls_applibrary + "~t" + ls_file ) 

iw_parent.inv_common.f_GetTargetLibList ( ls_libs ) 

/*  Set the library search path  */
li_libs = UpperBound ( ls_libs )
FOR li_i = 1 to li_libs
	IF Lower ( ls_libs[li_i] ) = Lower ( ls_applibrary ) THEN Continue
	IF Not of_CheckForDupeLibrary ( ls_libs[li_i] ) THEN
		tv_selectedlibs.InsertItemSort ( 0, Lower ( ls_libs[li_i] ), 1 ) 
		iw_parent.inv_common.f_ParsePath ( Lower ( ls_libs[li_i] ), ls_drive, ls_dir, ls_file, ls_ext )
		iw_parent.ids_LibNames.ImportString ( Lower ( ls_libs[li_i] ) + "~t~t" + &
				Lower ( ls_libs[li_i] ) + "~t" + ls_file ) 
	END IF
NEXT

ib_changed = TRUE

Return 1
end function

event constructor;call super::constructor;//*-------------------------------------------------------------------
//*  Event:				constructor
//*  Purpose:			Set Focus Control
//*-------------------------------------------------------------------
SetPointer ( HourGlass! )

/*  Set control that should get focus first  */
idrg_focus = tv_filelist
end event

event ue_getfocus;call super::ue_getfocus;//*-------------------------------------------------------------------
//*  Event:				ue_getfocus
//*  Purpose:			Populate saved list from the Registry
//*-------------------------------------------------------------------
SetPointer ( HourGlass! )

IF iw_parent.of_GetPage2Status ( FALSE ) THEN
	/*  Set the Library List if available in PB.INI  */
	of_GetLibListFromIniFile ( )
	iw_parent.of_GetPage3Status ( TRUE ) 
END IF

/*  If the list is complete, then enable Next  */
iw_parent.Event ue_next ( of_CheckRequired ( ) ) 

Return 1
end event

on k90_page2b.create
int iCurrent
call super::create
this.st_instruct=create st_instruct
this.st_lblabel=create st_lblabel
this.tv_filelist=create tv_filelist
this.tv_selectedlibs=create tv_selectedlibs
this.cb_reset=create cb_reset
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_instruct
this.Control[iCurrent+2]=this.st_lblabel
this.Control[iCurrent+3]=this.tv_filelist
this.Control[iCurrent+4]=this.tv_selectedlibs
this.Control[iCurrent+5]=this.cb_reset
end on

on k90_page2b.destroy
call super::destroy
destroy(this.st_instruct)
destroy(this.st_lblabel)
destroy(this.tv_filelist)
destroy(this.tv_selectedlibs)
destroy(this.cb_reset)
end on

event ue_losefocus;call super::ue_losefocus;//*-------------------------------------------------------------------
//*  Event:				ue_losefocus 
//*  Purpose:			Store library list
//*-------------------------------------------------------------------
int li_rc

IF ib_changed THEN
	IF as_direction = "next" THEN
		f_message ( "Information", "You must be able to successfully rebuild these libraries in PowerBuilder " + &
			"in order to proceed!", "k90_page2b", "ue_losefocus", Information!, OK!, 1 ) 
		SetPointer ( HourGlass! )
		ib_changed = FALSE
		li_rc = of_SaveLibraryList ( )
		IF li_rc <> 1 THEN
			ib_changed = TRUE
		END IF
		Return li_rc
	ELSE
		Return 1
	END IF
ELSE
	Return 1
END IF
end event

event destructor;call super::destructor;//*-------------------------------------------------------------------
//*  Event:				destructor
//*  Purpose:			Clean-Up
//*-------------------------------------------------------------------
end event

event resize;call super::resize;tv_filelist.Resize ( newwidth - 150, ( newheight / 2 ) - 140 )

tv_selectedlibs.Resize( newwidth - 150, ( newheight / 2 ) - 140 )
tv_selectedlibs.Move ( tv_selectedlibs.x, ( newheight / 2 ) + 60 )

st_lblabel.Move ( st_lblabel.x, ( newheight / 2 ) - 20 )
cb_reset.Move ( cb_reset.x, ( newheight / 2 ) - 25 )
end event

type st_instruct from statictext within k90_page2b
integer x = 41
integer y = 20
integer width = 1591
integer height = 76
boolean bringtotop = true
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 33554432
long backcolor = 67108864
boolean enabled = false
string text = "Adjust the &library list by dragging to the Application Library Path."
boolean focusrectangle = false
end type

type st_lblabel from statictext within k90_page2b
integer x = 41
integer y = 592
integer width = 544
integer height = 76
boolean bringtotop = true
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 33554432
long backcolor = 67108864
boolean enabled = false
string text = "Application Library &Path"
boolean focusrectangle = false
end type

type tv_filelist from u_library_painter within k90_page2b
integer x = 41
integer y = 96
integer width = 1614
integer height = 460
integer taborder = 10
string dragicon = "None!"
integer accelerator = 108
long picturemaskcolor = 16777215
end type

event doubleclicked;call super::doubleclicked;//*-------------------------------------------------------------------
//*  Event:				doubleclicked 
//*  Purpose:			Add library d-clicked on
//*-------------------------------------------------------------------
SetPointer ( HourGlass! )

of_AddLibraryToList ( handle )
end event

event dragdrop;call super::dragdrop;//*-------------------------------------------------------------------
//*  Event:				dragdrop
//*  Purpose:			Delete dragged libraries from the list
//*-------------------------------------------------------------------
/*  Do nothing if you drop on yourself  */
IF ClassName ( source ) = "tv_filelist" THEN Return

SetPointer ( HourGlass! )
of_DeleteLibraryFromList ( il_dragsource ) 
end event

event constructor;call super::constructor;//*-------------------------------------------------------------------
//*  Event:				constructor
//*  Purpose:			Position the file list 
//*-------------------------------------------------------------------
string ls_appdir, ls_startdir, ls_drive, ls_dir, ls_file
 
ls_appdir = iw_parent.of_GetAppLibrary ( ) 

iw_parent.inv_common.f_ParsePath ( ls_appdir, ls_drive, ls_dir, ls_file ) 
ls_startdir = iw_parent.inv_common.f_AssemblePath ( ls_drive, ls_dir ) 

If Not iw_parent.inv_common.f_DirectoryExists ( ls_startdir ) Then ls_startdir = "c:\"

/*  Set the starting position in the tree  */
this.of_SetPosition ( ls_startdir ) 
end event

event begindrag;call super::begindrag;//*-------------------------------------------------------------------
//*  Event:				begindrag 
//*  Purpose:			Select the item to be dragged
//*-------------------------------------------------------------------
this.SelectItem ( handle )
il_dragsource = handle
end event

type tv_selectedlibs from treeview within k90_page2b
integer x = 41
integer y = 672
integer width = 1614
integer height = 468
integer taborder = 20
boolean dragauto = true
boolean bringtotop = true
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 33554432
integer accelerator = 112
borderstyle borderstyle = stylelowered!
boolean disabledragdrop = false
string picturename[] = {"Library5!","libapp.bmp"}
integer picturewidth = 16
integer pictureheight = 16
long picturemaskcolor = 12632256
long statepicturemaskcolor = 536870912
end type

event dragdrop;//*-------------------------------------------------------------------
//*  Event:				dragdrop
//*  Purpose:			Add dragged libraries to the list
//*-------------------------------------------------------------------
/*  Do nothing if you drop on yourself  */
IF ClassName ( source ) = "tv_selectedlibs" THEN Return

SetPointer ( HourGlass! )

tv_filelist.ExpandItem ( il_dragsource )

of_AddLibraryToList ( il_dragsource )
end event

event doubleclicked;//*-------------------------------------------------------------------
//*  Event:				doubleclicked 
//*  Purpose:			Delete library d-clicked on
//*-------------------------------------------------------------------
SetPointer ( HourGlass! )

of_DeleteLibraryFromList ( handle )
end event

event begindrag;//*-------------------------------------------------------------------
//*  Event:				begindrag
//*  Purpose:			Store the item handle being dragged
//*-------------------------------------------------------------------
il_dragsource = handle
end event

type cb_reset from commandbutton within k90_page2b
integer x = 1394
integer y = 580
integer width = 261
integer height = 84
integer taborder = 2
integer textsize = -8
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "&Reset"
end type

event clicked;call super::clicked;//*-------------------------------------------------------------------
//*  Event:				clicked
//*  Purpose:			Clear the selected library list
//*-------------------------------------------------------------------
SetPointer ( HourGlass! )

tv_selectedlibs.DeleteItem ( 0 ) 

iw_parent.ids_LibNames.Reset ( ) 

ib_changed = TRUE

of_GetLibListFromINIFile ( ) 

/*  Disable the Next button  */
iw_parent.Event ue_next ( of_CheckRequired ( ) ) 
end event


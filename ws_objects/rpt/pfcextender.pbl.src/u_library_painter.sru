$PBExportHeader$u_library_painter.sru
$PBExportComments$(PTT) - Library Painter emulator ( Used in New Project Wizard )
forward
global type u_library_painter from base_treeview
end type
end forward

global type u_library_painter from base_treeview
integer width = 2386
integer height = 960
integer taborder = 1
string dragicon = "Question!"
boolean dragauto = true
boolean disabledragdrop = false
string picturename[] = {"foldclos.bmp","foldopen.bmp","Library5!","Application5!","","cdrive.ico","ddrive.ico","adrive.ico","xdrive.ico"}
integer picturewidth = 16
integer pictureheight = 16
long picturemaskcolor = 553648127
long statepicturemaskcolor = 12632256
end type
global u_library_painter u_library_painter

type variables
w_library_painter_helper iw_win
common_routines inv_common
string is_position
long il_position
datastore ids_objects
boolean ib_found
string is_separator, is_filetype
end variables

forward prototypes
public function long of_GetPosition ()
public function long of_getfiles (string as_path, long al_parent)
protected subroutine of_populatedrives (readonly string as_drive)
protected subroutine of_getsubdirectories (string as_path, long al_parent)
public subroutine of_changedrive (readonly string as_drive)
public function integer of_finditem (string as_dirs[])
public function long of_positiondrive (string as_drive)
public function integer of_positionitem (string as_position)
public function integer of_reset ()
public function string of_unctodrive (string as_position)
public function integer of_setposition (readonly string as_compare)
end prototypes

public function long of_GetPosition ();//*-------------------------------------------------------------------
//*  Function:			of_GetPosition 
//*  Purpose:			Obtain the tree position
//*-------------------------------------------------------------------
Return il_position
end function

public function long of_getfiles (string as_path, long al_parent);//*-------------------------------------------------------------------
//*  Function:			of_GetFiles 
//*  Purpose:			Add files to the tree
//*-------------------------------------------------------------------
string ls_path
long ll_totfile, ll_i, ll_pblhandle, ll_a, ll_apps, ll_item
treeviewitem ltvi_item, ltvi_pbl, ltvi_app
string ls_entries
string ls_libname
int li_type

SetPointer ( HourGlass! ) 

ids_objects = Create datastore
ids_objects.DataObject = "d_objects"

/*  Get the Parent item */
this.GetItem ( al_parent, ltvi_item ) 

//  Get the PBLs in the passed path argument
iw_win.lb_files.DirList ( as_path + is_separator + "*." + is_filetype, 0+1+32, iw_win.st_1 ) 

ll_totfile = iw_win.lb_files.TotalItems ( ) 

/*  Add the PBLs to the Tree  */
For ll_i = 1 to ll_totfile

	If iw_win.lb_files.Text ( ll_i ) = "" Then Continue

	ltvi_pbl.Level = 3
	ltvi_pbl.PictureIndex = 3
	ltvi_pbl.SelectedPictureIndex = 3
	ltvi_pbl.Label = iw_win.lb_files.Text ( ll_i )
	
	ids_objects.Reset ( )
	ls_libname = as_path + is_separator + iw_win.lb_files.Text( ll_i )
	// Use VM exported functions to check library type
	li_type = inv_common.f_getlibrarytype ( ls_libname )
	If li_type = 1 Then
		// Ansi library
		ls_entries = inv_common.f_listentries ( ls_libname, 1 )
	else
		// Unicode library
		ls_entries = LibraryDirectory ( ls_libname, DirApplication! ) 
	End If	
	ids_objects.ImportString ( ls_entries )
	ltvi_pbl.Children = ( ids_objects.RowCount ( ) > 0 ) 
	ltvi_pbl.Data = as_path + is_separator + iw_win.lb_files.Text ( ll_i )
	ll_pblhandle = this.InsertItemLast ( al_parent, ltvi_pbl ) 
	If  ids_objects.RowCount ( ) > 0 Then
		ll_apps = ids_objects.RowCount ( ) 
		For ll_a = 1 to ll_apps
			ltvi_app.Label = ids_objects.Object.ObjectName.Current[ll_a]
			ltvi_app.PictureIndex = 4
			ltvi_app.SelectedPictureIndex = 4
			ltvi_app.Data = as_path + is_separator + iw_win.lb_files.Text ( ll_i )
			ll_item = this.InsertItemLast ( ll_pblhandle, ltvi_app ) 
			/*  If this is the position item, then save its reference  */
			ls_path = Lower ( ltvi_app.Data ) + "(" + Lower ( ltvi_app.Label ) + ")"
			If Lower ( ls_path ) = is_position Then
				il_position = ll_item
			End If
		Next 
	End If
Next

Destroy ids_objects

Return ll_totfile
end function

protected subroutine of_populatedrives (readonly string as_drive);//*-------------------------------------------------------------------
//*  Function:			of_PopulateDrives 
//*  Purpose:			Add drive letters to the tree
//*-------------------------------------------------------------------
int li_drivetype
long ll_total, ll_i, ll_item, ll_r, ll_rows
string ls_drive, ls_driveletter, ls_drivename
datastore lds_drives
treeviewitem ltvi_item

SetPointer ( HourGlass! ) 

/*  Clear the treeview  */
of_Reset ( ) 

/*   Get Mapped Drive Information  */
lds_drives = Create datastore
lds_drives.DataObject = "d_driveinfo"
lds_drives.ImportString(inv_common.f_GetDrives ( ))
ll_rows = lds_drives.RowCount ( ) 

/*  Get the number of drives  */
ll_total = iw_win.lb_drives.TotalItems ( ) 

/*  Add the drives to the treeview  */
For ll_i = 1 to ll_total

	ls_drive = iw_win.lb_drives.Text ( ll_i )  
	ls_drive = Mid ( ls_drive, 3, 1 )
	ll_r = lds_drives.Find ( "Lower ( Left ( driveletter, 1 ) ) = '" + Lower ( ls_drive ) + "'", 1, ll_rows ) 
	If ll_r > 0 Then
		ls_driveletter = lds_drives.GetItemString ( ll_r, "driveletter" ) 
		li_drivetype = lds_drives.GetItemNumber ( ll_r, "drivetype" ) + 6
		ls_drivename = lds_drives.GetItemString ( ll_r, "drivename" ) 
	Else
		ls_driveletter = Upper(ls_Drive) + ":\"
		li_drivetype = 6
		ls_drivename = ls_driveletter
	End If
	ltvi_item.Label = ls_drivename
	ltvi_item.Children = True
	ltvi_item.Data = ls_drive + ":"
	ltvi_item.Level = 1
	ltvi_item.PictureIndex = li_drivetype
	ltvi_item.SelectedPictureIndex = ltvi_item.PictureIndex
	ltvi_item.OverlayPictureIndex = 0
	ll_item = this.InsertItemLast ( 0, ltvi_item ) 
	
	/*  If this is the position item, then save its reference  */
	If Lower ( ls_drive ) + ":" = as_drive Then
		il_position = ll_item
	End If

Next

If ll_total <= 0 Then Return

this.SetFirstVisible ( il_position ) 
this.ExpandItem ( il_position )
end subroutine

protected subroutine of_getsubdirectories (string as_path, long al_parent);//*-------------------------------------------------------------------
//*  Function:			of_GetSubDirectories 
//*  Purpose:			Add directories to the tree
//*-------------------------------------------------------------------
long ll_total, ll_i, ll_item, ll_pos
long ll_expanditem, ll_positionitem
string ls_subdir, ls_path
treeviewitem ltvi_sub

SetPointer ( HourGlass! ) 

If Right ( as_path, 1 ) <> is_separator Then as_path = as_path + is_separator

iw_win.lb_dir.DirList ( as_path, 16+32768, iw_win.st_1 )

ll_total = iw_win.lb_dir.TotalItems ( ) 

/*  Get Sub-Directories & add them to the Tree  */
For ll_i = 1 to ll_total

	ls_subdir = iw_win.lb_dir.Text ( ll_i ) 
	ls_subdir = inv_common.f_GlobalReplace ( ls_subdir, "[.]", "", True )
	ls_subdir = inv_common.f_GlobalReplace ( ls_subdir, "[..]", "", True )
	ls_subdir = inv_common.f_GlobalReplace ( ls_subdir, "[", "", True )
	ls_subdir = inv_common.f_GlobalReplace ( ls_subdir, "]", "", True )

	If ls_subdir = "" Then Continue

	ls_path = as_path + ls_subdir 

	ltvi_sub.Label = ls_subdir

	/*  See if there are any sub-directories */
	iw_win.lb_test.DirList ( ls_path + is_separator + "*." + is_filetype, 16, iw_win.st_1 )
	ltvi_sub.Children = ( iw_win.lb_test.TotalItems ( ) > 1 )

	ltvi_sub.Data = ls_path
	ltvi_sub.Level = 2
	ltvi_sub.PictureIndex = 1
	ltvi_sub.SelectedPictureIndex = 2
	ll_item = this.InsertItemLast ( al_parent, ltvi_sub )

Next
end subroutine

public subroutine of_changedrive (readonly string as_drive);//*-------------------------------------------------------------------
//*  Function:			of_ChangeDrive 
//*  Purpose:			The drive has changed, populate subdirectories
//*-------------------------------------------------------------------
string ls_drive

SetPointer ( HourGlass! )

/*  Replace special characters in the drive list and format to x:  */
If Left ( ls_drive, 2 ) <> "\\" Then
	ls_drive = inv_common.f_GlobalReplace ( as_drive, ":", "", True )
	ls_drive = inv_common.f_GlobalReplace ( ls_drive, is_separator, "", True )
	ls_drive = inv_common.f_GlobalReplace ( ls_drive, "[-", "", True )
	ls_drive = inv_common.f_GlobalReplace ( ls_drive, "-]", "", True )
	ls_drive = ls_drive + ":"
End If

/*  Get a list of directories  */
iw_win.lb_drives.DirList ( ls_drive, 16384+32768, iw_win.st_1 )

/*  Add directories to the tree */
of_PopulateDrives ( ls_drive ) 
end subroutine

public function integer of_finditem (string as_dirs[]);//*-----------------------------------------------------------------*/
//*    of_FindItem:  Find a path in the tree
//*-----------------------------------------------------------------*/
long ll_item, ll_d, ll_dirs
treeviewitem ltvi_item
boolean lb_found

SetPointer ( HourGlass! )

/*  Find the item containing the first dir  */
ll_dirs = UpperBound ( as_dirs ) 

ll_item = this.FindItem ( ChildTreeItem!, il_position )
For ll_d = 1 to ll_dirs 
	Do While ll_item > 0 
		If ll_item > 0 Then
			this.GetItem ( ll_item, ltvi_item )
			If Lower ( ltvi_item.Label ) = Lower ( as_dirs[ll_d] ) Then
				If ltvi_item.Children Then
					this.SelectItem ( ll_item )
					this.ExpandItem ( ll_item )
				Else
					this.SelectItem ( ll_item )
					lb_found = True
					Exit
				End If
				Exit
			Else
				ll_item = this.FindItem ( NextTreeItem!, ll_item )
			End If
		End If
	Loop
	ll_item = this.FindItem ( ChildTreeItem!, ll_item )
Next 

If lb_found Then
	Return 1
Else
	Return -1
End If
end function

public function long of_positiondrive (string as_drive);//*-----------------------------------------------------------------*/
//*    of_PositionDrive:  Set the drive position
//*-----------------------------------------------------------------*/
long ll_item
treeviewitem ltvi_item

SetPointer ( HourGlass! )

ll_item = this.FindItem ( RootTreeItem!, 0 )
Do While ll_item > 0 
	If ll_item > 0 Then
		this.GetItem ( ll_item, ltvi_item )
		If Pos ( Lower ( ltvi_item.Label ), Lower ( as_drive ) ) > 0 Then
			this.ExpandItem ( ll_item )
			Return ll_item
		End If
	End If
	ll_item = this.FindItem ( NextTreeItem!, ll_item )
Loop

Return -1
end function

public function integer of_positionitem (string as_position);//*-----------------------------------------------------------------*/
//*    of_PositionItem:  Position the tree to the passed item
//*-----------------------------------------------------------------*/
string ls_drive, ls_dir, ls_dirs[], ls_file
string ls_position

SetPointer ( HourGlass! )

/*  Convert UNC to Drive Letter  */
ls_position = of_UNCtoDrive ( as_position )

/*  Breakdown the path  */
inv_common.f_ParsePath ( ls_position, ls_drive, ls_dir, ls_file )

/*  First position the drive  */
If Left ( ls_drive, 2 ) = "\\" Then
	il_position = of_PositionDrive ( ls_drive ) 
Else
	il_position = of_PositionDrive ( Left ( ls_drive, 2 ) ) 
End If
	
inv_common.f_ParseToArray ( ls_dir, inv_common.is_separator, ls_dirs )

If ls_file > "" Then ls_dirs[UpperBound(ls_dirs)+1] = ls_file

Return this.of_FindItem ( ls_dirs ) 
end function

public function integer of_reset ();//*-------------------------------------------------------------------
//*  Function:			of_Reset 
//*  Purpose:			Clear the tree
//*-------------------------------------------------------------------
long ll_item

SetPointer ( HourGlass! )

this.SetReDraw ( False )

ll_item = this.FindItem ( RootTreeItem! , 0 )

Do Until ll_item <= 0 
	this.DeleteItem ( ll_item )
	ll_item = this.FindItem ( RootTreeItem! , 0 )
Loop

this.SetReDraw ( True )

Return 1
end function

public function string of_unctodrive (string as_position);//*-----------------------------------------------------------------*/
//*    of_UNCtoDrive:  Convert a UNC to Drive Letter
//*-----------------------------------------------------------------*/
long ll_item, ll_d, ll_dirs, ll_pos, ll_saveitem
string ls_dirs[], ls_searchdrive, ls_drivename, ls_drive, ls_position
treeviewitem ltvi_item
boolean lb_found

SetPointer ( HourGlass! )

If Left ( as_position, 2 ) = "\\" Then
	as_position = Mid ( as_position, 3 ) 
Else
	Return as_position
End If

ll_dirs = inv_common.f_ParseToArray ( as_position, inv_common.is_separator, ls_dirs ) 
If ll_dirs > 1 Then
	ls_searchdrive = ls_dirs[2] + " on '" + ls_dirs[1] + "'"
Else
	Return as_position
End If

ll_item = this.FindItem ( RootTreeItem!, 0 )
Do While ll_item > 0 
	If ll_item > 0 Then
		this.GetItem ( ll_item, ltvi_item )
		If Pos ( Lower ( ltvi_item.Label ), Lower ( ls_searchdrive ) ) = 1 Then
			lb_found = True
			ls_drivename = ltvi_item.Label
			/*  Look for Drive Letter  */
			ll_pos = Pos ( ls_drivename, "(" ) 
			If ll_pos > 0 Then
				ls_drive = Mid ( ls_drivename, ll_pos + 1, 2 ) 
				/*  Create position item  */
				ls_position = ls_drive + inv_common.is_separator
				For ll_d = 3 to ll_dirs
					ls_position = ls_position + ls_dirs[ll_d] + inv_common.is_separator
				Next
				Return ls_position
			Else
				Return as_position
			End If
		End If
	End If
	ll_item = this.FindItem ( NextTreeItem!, ll_item )
Loop

If Not lb_found Then
	/*  UNC Not Found, So add it to the tree  */
	ltvi_item.Label = "\\" + ls_dirs[1] + "\" + ls_dirs[2]
	ltvi_item.Children = True
	ltvi_item.Data = ltvi_item.Label
	ltvi_item.Level = 1
	ltvi_item.PictureIndex = 8
	ltvi_item.SelectedPictureIndex = ltvi_item.PictureIndex
	ltvi_item.OverlayPictureIndex = 0
	ll_saveitem = this.InsertItemLast ( 0, ltvi_item ) 
	this.ExpandItem ( ll_saveitem )
	Return "\\" + as_position
End If

Return as_position
end function

public function integer of_setposition (readonly string as_compare);//*-------------------------------------------------------------------
//*  Function:			of_SetPosition 
//*  Purpose:			Store the position of the tree
//*-------------------------------------------------------------------
string ls_drive, ls_dir

Open ( iw_win ) 

SetPointer ( HourGlass! )

f_SetCommon ( inv_common, True )
is_separator = inv_common.is_separator
is_filetype = inv_common.is_FileType
is_position = Lower ( as_compare )

inv_common.f_ParsePath ( is_position, ls_drive, ls_dir )
this.of_ChangeDrive ( ls_drive )

of_PositionItem ( is_position )

Return 1
end function

event itemexpanding;//*-------------------------------------------------------------------
//*  Event:				itemexpanding
//*  Purpose:			Populate lower levels
//*-------------------------------------------------------------------
treeviewitem ltvi_item

/*  Bypass invalid handles  */
If handle <= 0 Then Return

/*  Get the TreeView item clicked on  */
this.GetItem ( handle, ltvi_item ) 

/*  If item has been expanded, then it is populated already  */
If ltvi_item.ExpandedOnce Then Return

/*  If the item doesn't have children, return  */
If Not ltvi_item.Children Then Return 

this.SetReDraw ( False ) 

If ltvi_item.PictureIndex = 3 Then 
	this.SetReDraw ( True ) 
	return
End If

/*  Add the PBL items to the tree  */
of_GetFiles ( ltvi_item.Data, handle ) 

/*  Add any sub-directories to the tree  */
of_GetSubDirectories ( ltvi_item.Data, handle )

this.SetReDraw ( True ) 
end event

event destructor;//*-------------------------------------------------------------------
//*  Event:				destructor
//*  Purpose:			Clean-up
//*-------------------------------------------------------------------
f_SetCommon ( inv_common, False )

Close ( iw_win )
end event

on u_library_painter.create
call super::create
end on

on u_library_painter.destroy
call super::destroy
end on


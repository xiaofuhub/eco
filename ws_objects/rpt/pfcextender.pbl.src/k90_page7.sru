$PBExportHeader$k90_page7.sru
$PBExportComments$(K90) - PFC Extender Step 7:  Build the extensions
forward
global type k90_page7 from k90_page
end type
type tv_hierarchy from treeview within k90_page7
end type
type dw_messages from datawindow within k90_page7
end type
type st_congrats from statictext within k90_page7
end type
type cbx_clipboard from checkbox within k90_page7
end type
type cbx_file from checkbox within k90_page7
end type
type gb_pasteoptions from groupbox within k90_page7
end type
type st_resizebar from u_splitbar within k90_page7
end type
end forward

global type k90_page7 from k90_page
string tag = "Build Extensions"
integer width = 2331
integer height = 1136
tv_hierarchy tv_hierarchy
dw_messages dw_messages
st_congrats st_congrats
cbx_clipboard cbx_clipboard
cbx_file cbx_file
gb_pasteoptions gb_pasteoptions
st_resizebar st_resizebar
end type
global k90_page7 k90_page7

type variables

end variables

forward prototypes
public function integer of_addmessage (readonly string as_message, readonly integer ai_errorcd)
public function integer of_addmessage (readonly long al_row, readonly string as_error, readonly integer ai_errorcd)
public function integer of_addobjects (readonly string as_library, readonly long al_parent, readonly string as_prefix)
public function integer of_addlibraries (readonly long al_parent)
end prototypes

public function integer of_addmessage (readonly string as_message, readonly integer ai_errorcd);//*-------------------------------------------------------------------
//*  Function:			of_AddMessage
//*  Purpose:			Add processing and/or error message
//*-------------------------------------------------------------------
long ll_row

SetPointer ( HourGlass! )

dw_messages.ImportString ( as_message + "~t" + String ( ai_errorcd ) )

ll_row = dw_messages.RowCount ( ) 

IF ll_row > 0 THEN dw_messages.ScrollToRow ( ll_row )

Return ll_row
end function

public function integer of_addmessage (readonly long al_row, readonly string as_error, readonly integer ai_errorcd);//*-------------------------------------------------------------------
//*  Function:			of_AddMessage
//*  Purpose:			Add results code to the message
//*						This allows the bitmap to display.
//*-------------------------------------------------------------------
SetPointer ( HourGlass! )

dw_messages.Object.ErrorCD.Current[al_row] = ai_errorcd

Return 0
end function

public function integer of_addobjects (readonly string as_library, readonly long al_parent, readonly string as_prefix);//*-------------------------------------------------------------------
//*  Function:			of_AddObjects
//*  Purpose:			Add Object names to the tree,
//*						when the tree item is expanded.
//*-------------------------------------------------------------------
int li_i, li_totobjects, li_objtypes[]
long ll_rows
string ls_objects[]
treeviewitem ltvi_item

SetPointer ( HourGlass! )

/*  Filter the object list for this library only  */
IF as_prefix = "pfc" THEN
	iw_parent.ids_objects.SetFilter ( "Library = '" + as_library + "' AND Left ( objectname, 4 ) = 'pfc_'" )
ELSEIF as_prefix = "pfe" THEN
	iw_parent.ids_objects.SetFilter ( "Library = '" + as_library + "' AND Left ( objectname, 4 ) <> 'pfc_'" )
ELSE
	iw_parent.ids_objects.SetFilter ( "Library = '" + as_library + "'" )
END IF

iw_parent.ids_objects.Filter (  )

ll_rows = iw_parent.ids_objects.RowCount (  )
IF ll_rows > 0 THEN
	li_objtypes = iw_parent.ids_objects.Object.ObjectType.Current[1, ll_rows]
	ls_objects = iw_parent.ids_objects.Object.ObjectName.Current[1, ll_rows]
	
	li_totobjects = UpperBound ( ls_objects )
	
	FOR li_i = 1 to li_totobjects
		ltvi_item.Label = ls_objects[li_i]
		ltvi_item.Data  = "obj"
		ltvi_item.Level = 3
		ltvi_item.PictureIndex = li_objtypes[li_i] + 1
		ltvi_item.SelectedPictureIndex = li_objtypes[li_i] + 1
		ltvi_item.Children = False
		ltvi_item.Bold = False
		/*  Insert item at the bottom of the tree  */


		tv_hierarchy.InsertItemLast ( al_parent, ltvi_item )
	NEXT
	Return 1
ELSE
	Return -1
END IF
end function

public function integer of_addlibraries (readonly long al_parent);//*-------------------------------------------------------------------
//*  Function:			of_AddLibraries
//*  Purpose:			Add Library names to the Tree, 
//*						after extensions are built.
//*-------------------------------------------------------------------
int li_i, li_j, li_totlibraries, li_totlibtypes
string ls_library 
treeviewitem ltvi_item
SetPointer ( HourGlass! )

li_totlibraries = iw_parent.ids_LibNames.RowCount ( ) 

FOR li_i = 1 to li_totlibraries
	ls_library = iw_parent.ids_LibNames.GetItemString ( li_i, "LibraryName" )

	IF ls_Library > "" THEN
		/*  Add the library name to the tree  */
		ltvi_item.Label = ls_Library
		ltvi_item.Data  = "lib"
		ltvi_item.Level = 2
		ltvi_item.PictureIndex = 12
		ltvi_item.SelectedPictureIndex = 12
		ltvi_item.Children = True
		ltvi_item.Bold = False
		/*  Insert item at the bottom of the tree  */
		tv_hierarchy.InsertItemLast ( al_parent, ltvi_item )
	END IF
NEXT

Return 1
end function

on k90_page7.create
int iCurrent
call super::create
this.tv_hierarchy=create tv_hierarchy
this.dw_messages=create dw_messages
this.st_congrats=create st_congrats
this.cbx_clipboard=create cbx_clipboard
this.cbx_file=create cbx_file
this.gb_pasteoptions=create gb_pasteoptions
this.st_resizebar=create st_resizebar
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.tv_hierarchy
this.Control[iCurrent+2]=this.dw_messages
this.Control[iCurrent+3]=this.st_congrats
this.Control[iCurrent+4]=this.cbx_clipboard
this.Control[iCurrent+5]=this.cbx_file
this.Control[iCurrent+6]=this.gb_pasteoptions
this.Control[iCurrent+7]=this.st_resizebar
end on

on k90_page7.destroy
call super::destroy
destroy(this.tv_hierarchy)
destroy(this.dw_messages)
destroy(this.st_congrats)
destroy(this.cbx_clipboard)
destroy(this.cbx_file)
destroy(this.gb_pasteoptions)
destroy(this.st_resizebar)
end on

event ue_finished;call super::ue_finished;//*-------------------------------------------------------------------
//*  Event:				ue_finished
//*  Purpose:			When the processing is done, populate the tree
//*-------------------------------------------------------------------
int li_i, li_totprefixes
long ll_parent=0
string ls_hierarchy[], ls_newprefix
treeviewitem ltvi_item

SetPointer ( HourGlass! )

/*  Clear the Tree  */
tv_hierarchy.DeleteItem ( 0 )

		ltvi_item.Label = "Library Path"
		ltvi_item.Data  = "prefix"
		ltvi_item.Level = 1
		ltvi_item.PictureIndex = 16
		ltvi_item.SelectedPictureIndex = 16
		ltvi_item.Children = True
		ltvi_item.Bold = False
		/*  Insert item at the bottom of the tree  */
		ll_parent = tv_hierarchy.InsertItemLast ( 0, ltvi_item )
		
/*  Add the libraries for this prefix  */
of_AddLibraries ( ll_parent )

tv_hierarchy.ExpandItem ( 1 )

tv_hierarchy.Visible = TRUE
st_resizebar.Visible = TRUE

Return 1
end event

event ue_getfocus;call super::ue_getfocus;//*-------------------------------------------------------------------
//*  Event:				ue_getfocus
//*  Purpose:			Initial processing
//*-------------------------------------------------------------------
SetPointer ( HourGlass! )

dw_messages.Visible = FALSE

iw_parent.cb_print.Visible = FALSE

Return 1
end event

event ue_print;call super::ue_print;//*-------------------------------------------------------------------
//*  Event:				ue_print
//*  Purpose:			Print Error Message report
//*-------------------------------------------------------------------
SetPointer ( HourGlass! )

dw_messages.Print ( ) 

Return 1
end event

event ue_losefocus;call super::ue_losefocus;//*-------------------------------------------------------------------
//*  Event:				ue_losefocus
//*  Purpose:			When the user hits the Back button
//*-------------------------------------------------------------------
Return 1
end event

event constructor;call super::constructor;//*-------------------------------------------------------------------
//*  Event:				constructor
//*  Purpose:			Establish the pane resizing information
//*-------------------------------------------------------------------
idrg_focus = cbx_clipboard

Return 1
end event

event resize;call super::resize;tv_hierarchy.Resize ( ( newwidth / 2 ) - 80, newheight - 140 )

st_resizebar.Resize ( st_resizebar.width , newheight - 140 )
st_resizebar.Move ( tv_hierarchy.x + tv_hierarchy.width + 5, st_resizebar.y )

dw_messages.Resize( ( newwidth / 2 ) - 80, newheight - 140 )
dw_messages.Move ( st_resizebar.x + st_resizebar.width + 5, dw_messages.y )


end event

type tv_hierarchy from treeview within k90_page7
boolean visible = false
integer x = 18
integer y = 16
integer width = 974
integer height = 1092
integer taborder = 20
boolean bringtotop = true
integer textsize = -8
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long backcolor = 1073807359
borderstyle borderstyle = stylelowered!
string picturename[] = {"Application5!","DataWindow5!","Function!","Menu5!","Query5!","Structure5!","UserObject5!","Window!","DataPipeline!","Project!","proxyg.bmp","Library5!","libopen.bmp","CreateLibrary5!","",""}
long picturemaskcolor = 12632256
end type

event itemexpanding;call super::itemexpanding;//*-------------------------------------------------------------------
//*  Event:				itemexpanding
//*  Purpose:			Populate the objects
//*-------------------------------------------------------------------
long ll_parent
string ls_prefix 
treeviewitem ltvi_item, ltvi_parent

IF handle > 0 THEN
	SetPointer ( HourGlass! )
	this.GetItem ( handle, ltvi_item )
	IF Not ltvi_item.ExpandedOnce THEN
		IF String ( ltvi_item.Data ) = "lib" THEN
			ll_parent = this.FindItem ( ParentTreeItem!, handle )
			this.GetItem ( ll_parent, ltvi_parent ) 
			IF ll_parent > 0 THEN ls_prefix = ltvi_parent.Label
			
			of_AddObjects ( ltvi_item.Label, handle, ls_prefix ) 

		END IF
	END IF
END IF
end event

type dw_messages from datawindow within k90_page7
boolean visible = false
integer x = 1019
integer y = 24
integer width = 1271
integer height = 1092
integer taborder = 10
boolean bringtotop = true
string dataobject = "k90_messages"
boolean hscrollbar = true
boolean vscrollbar = true
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type st_congrats from statictext within k90_page7
integer x = 219
integer y = 584
integer width = 1175
integer height = 344
boolean bringtotop = true
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 33554432
long backcolor = 79416533
boolean enabled = false
string text = "Congratulations!   You~'re ready to proceed with building the new libraries.   You may click Back to review the information before final processing, or click Finish to complete the library creation process."
boolean focusrectangle = false
end type

type cbx_clipboard from checkbox within k90_page7
integer x = 398
integer y = 208
integer width = 663
integer height = 76
boolean bringtotop = true
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 33554432
long backcolor = 67108864
string text = "Clipboard"
boolean checked = true
end type

event clicked;iw_parent.of_SetPasteClip ( this.Checked ) 
end event

type cbx_file from checkbox within k90_page7
integer x = 398
integer y = 288
integer width = 663
integer height = 76
boolean bringtotop = true
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 33554432
long backcolor = 67108864
string text = "File ( ~"libpath.txt~" ) "
boolean checked = true
end type

event clicked;iw_parent.of_SetPasteFile ( this.Checked ) 
end event

type gb_pasteoptions from groupbox within k90_page7
integer x = 219
integer y = 116
integer width = 1147
integer height = 320
integer taborder = 11
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 33554432
long backcolor = 67108864
string text = "Paste New Library Search Path to"
end type

type st_resizebar from u_splitbar within k90_page7
boolean visible = false
integer x = 997
integer y = 20
integer width = 27
integer height = 1092
boolean border = true
borderstyle borderstyle = styleraised!
end type

event constructor;call super::constructor;// Register the controls with the Vertical SplitBar
this.of_SetStyle ( this.VERTICAL ) 
this.of_Register ( tv_hierarchy, this.LEFT)
this.of_Register ( dw_messages, this.RIGHT)
end event


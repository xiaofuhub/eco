$PBExportHeader$k90_page5.sru
$PBExportComments$(K90) - PFC Extender Step 5:  Select / Deselect objects
forward
global type k90_page5 from k90_page
end type
type st_instruct from statictext within k90_page5
end type
type dw_objects from u_dw within k90_page5
end type
end forward

global type k90_page5 from k90_page
string tag = "Select Objects to be Extended"
st_instruct st_instruct
dw_objects dw_objects
end type
global k90_page5 k90_page5

on k90_page5.create
int iCurrent
call super::create
this.st_instruct=create st_instruct
this.dw_objects=create dw_objects
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_instruct
this.Control[iCurrent+2]=this.dw_objects
end on

on k90_page5.destroy
call super::destroy
destroy(this.st_instruct)
destroy(this.dw_objects)
end on

event ue_getfocus;call super::ue_getfocus;//*-------------------------------------------------------------------
//*  Event:				ue_getfocus
//*  Purpose:			Populate the object list, if it needs to be set
//*-------------------------------------------------------------------
s_objectinfo lstr_objects[]

SetPointer ( HourGlass! ) 

IF as_direction = "next" THEN
	IF iw_parent.of_GetPage5Status ( FALSE ) THEN
		/*  Re-List the objects  */
		//iw_parent.of_GetObjectSelection ( )
		/*  Get objects for level only  */
		IF iw_parent.of_GetLevelObjects ( ) = 1 THEN
			/*  Can't use ShareData, then row selection doesn't work  */
			lstr_objects = iw_parent.ids_objects.Object.Data
			dw_objects.DataObject = "d_objectlist"
			dw_objects.Object.Data = lstr_objects
		END IF
	END IF
END IF

Return 1
end event

event ue_losefocus;call super::ue_losefocus;//*-------------------------------------------------------------------
//*  Event:				ue_losefocus
//*  Purpose:			Save the object selection
//*-------------------------------------------------------------------
s_objectinfo lstr_objects[]

SetPointer ( HourGlass! ) 

IF as_direction = "next" THEN
	IF ib_changed THEN
		/*  Set the selection in the parent window  */
		lstr_objects = dw_objects.Object.Data

		iw_parent.of_SetObjects ( lstr_objects )
		ib_changed = FALSE
	END IF
END IF

Return 1
end event

event constructor;call super::constructor;//*-------------------------------------------------------------------
//*  Event:				constructor
//*  Purpose:			Initialization
//*-------------------------------------------------------------------
/*  Set the focus object  */
idrg_focus = dw_objects
end event

event resize;call super::resize;dw_objects.Resize ( newwidth - 150, newheight - 140 )

end event

type st_instruct from statictext within k90_page5
integer x = 41
integer y = 28
integer width = 1618
integer height = 120
boolean bringtotop = true
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 33554432
long backcolor = 79416533
boolean enabled = false
string text = "You can deselect any objects you do not want to add to the new extensions.   If you want to include all objects, click Next."
boolean focusrectangle = false
end type

type dw_objects from u_dw within k90_page5
integer x = 41
integer y = 152
integer width = 1637
integer height = 968
integer taborder = 20
string dataobject = "d_objectlist"
boolean hscrollbar = true
end type

event constructor;call super::constructor;//*-------------------------------------------------------------------
//*  Event:				constructor
//*  Purpose:			Set datawindow behavior
//*-------------------------------------------------------------------
of_SetRowSelect ( TRUE )

/*  Extended row selection  */
inv_RowSelect.of_SetStyle ( 2 )

/*  Row selection is indicated by a check mark  */
inv_RowSelect.of_SetNoHighlight ( TRUE )
end event

event clicked;call super::clicked;//*-------------------------------------------------------------------
//*  Event:				clicked
//*  Purpose:			When clicked, row selection changes the list
//*-------------------------------------------------------------------
ib_changed = TRUE
end event


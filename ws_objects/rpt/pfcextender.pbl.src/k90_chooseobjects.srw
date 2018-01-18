$PBExportHeader$k90_chooseobjects.srw
$PBExportComments$(K90) - PFC Extender Choose object dialog, for establishing hierarchy
forward
global type k90_chooseobjects from base_center
end type
type cb_cancel from commandbutton within k90_chooseobjects
end type
type cb_ok from commandbutton within k90_chooseobjects
end type
type st_info from statictext within k90_chooseobjects
end type
type dw_objectlist from u_dw within k90_chooseobjects
end type
end forward

global type k90_chooseobjects from base_center
integer width = 1266
integer height = 1000
string title = "Choose Object"
boolean minbox = false
boolean maxbox = false
boolean resizable = false
windowtype windowtype = response!
long backcolor = 79416533
event type string ue_close ( readonly long al_row )
cb_cancel cb_cancel
cb_ok cb_ok
st_info st_info
dw_objectlist dw_objectlist
end type
global k90_chooseobjects k90_chooseobjects

type variables
/*  Selected row  */
long il_row
end variables

event ue_close;call super::ue_close;//*-------------------------------------------------------------------
//*  Event:				ue_close
//*  Purpose:			Return name of object chosen
//*-------------------------------------------------------------------
string ls_Library, ls_Object, ls_Rtn
int li_Type

IF al_row > 0 THEN
	ls_library = dw_objectlist.Object.sLibrary.Current [al_row]
	ls_Object = dw_objectlist.Object.sObject.Current [al_row]
	li_Type = dw_objectlist.Object.iObjType.Current [al_row]

	IF IsNull ( ls_library ) THEN ls_library = ""
	IF IsNull ( ls_Object ) THEN ls_Object = ""
	IF IsNull ( li_Type ) THEN li_Type = 0

	ls_Rtn = ls_Object + "~n" + ls_Library + "~n" + String ( li_Type ) 

	Return ls_Rtn

ELSE

	Return ""

END IF
end event

event open;call super::open;//*-------------------------------------------------------------------
//*  Event:				open
//*  Purpose:			Using the passed list of libraries, 
//*						Populate a list of objects
//*-------------------------------------------------------------------
datastore lds_prefixes

/*  Get the passed parm  */
lds_prefixes = Message.PowerObjectParm 

IF IsValid ( lds_prefixes ) THEN 
	lds_prefixes.ShareData ( dw_objectlist ) 
END IF

/*  Sort and filter the list  */
dw_objectlist.Sort ( ) 
dw_objectlist.SetFilter ( "ilevel = 1" )
dw_objectlist.Filter ( ) 
dw_objectlist.GroupCalc ( ) 

IF dw_objectlist.RowCount ( ) > 0 THEN
	dw_objectlist.SelectRow ( 1, TRUE )
	il_row = 1
ELSE
	il_row = 0 
END IF
end event

on k90_chooseobjects.create
int iCurrent
call super::create
this.cb_cancel=create cb_cancel
this.cb_ok=create cb_ok
this.st_info=create st_info
this.dw_objectlist=create dw_objectlist
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.cb_cancel
this.Control[iCurrent+2]=this.cb_ok
this.Control[iCurrent+3]=this.st_info
this.Control[iCurrent+4]=this.dw_objectlist
end on

on k90_chooseobjects.destroy
call super::destroy
destroy(this.cb_cancel)
destroy(this.cb_ok)
destroy(this.st_info)
destroy(this.dw_objectlist)
end on

event close;call super::close;//*-------------------------------------------------------------------
//*  Event:				Close
//*  Purpose:			Return name of object chosen
//*-------------------------------------------------------------------
CloseWithReturn ( this, this.Event ue_close ( il_row ) ) 
end event

type cb_cancel from commandbutton within k90_chooseobjects
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "Cancel"
boolean cancel = true
boolean default = true
integer x = 878
integer y = 788
integer width = 311
integer height = 88
integer taborder = 10
end type

event clicked;//*-------------------------------------------------------------------
//*  Event:				clicked
//*  Purpose:			User cancelled.  Return nothing
//*-------------------------------------------------------------------
il_row = 0 
CloseWithReturn ( Parent, Parent.Event ue_close ( il_row ) ) 
end event

type cb_ok from commandbutton within k90_chooseobjects
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "OK"
boolean default = true
integer x = 549
integer y = 788
integer width = 311
integer height = 88
integer taborder = 20
end type

event clicked;//*-------------------------------------------------------------------
//*  Event:				clicked
//*  Purpose:			Return name of object chosen
//*-------------------------------------------------------------------
CloseWithReturn ( Parent, Parent.Event ue_close ( il_row ) ) 
end event

type st_info from statictext within k90_chooseobjects
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 33554432
long backcolor = 67108864
boolean enabled = false
string text = "Select object to be used as the basis for the current hierarchy"
boolean focusrectangle = false
integer x = 59
integer y = 20
integer width = 1070
integer height = 112
end type

type dw_objectlist from u_dw within k90_chooseobjects
event key pbm_dwnkey
string dataobject = "k90_hierarchy"
boolean hscrollbar = true
integer x = 50
integer y = 152
integer width = 1138
integer height = 604
end type

event key;call super::key;//*-------------------------------------------------------------------
//*  Event:				Key
//*  Purpose:			Capture arrow keys
//*-------------------------------------------------------------------
long ll_row

ll_row = this.GetRow ( ) 

IF key = KeyDownArrow! THEN
	ll_row++
ELSEIF key = KeyUpArrow! THEN
	ll_row --
ELSE
	Return 
END IF

IF ( ll_row > 0 ) &
AND ( ll_row <= this.RowCount ( ) ) THEN
	this.SelectRow ( 0, FALSE ) 
	this.SelectRow ( ll_row, Not IsSelected ( ll_row ) ) 
	il_row = ll_row
END IF
end event

event clicked;call super::clicked;//*-------------------------------------------------------------------
//*  Event:				clicked
//*  Purpose:			Select clicked row
//*-------------------------------------------------------------------
IF row > 0 THEN
	this.SelectRow ( 0, FALSE ) 
	this.SelectRow ( row, Not IsSelected ( row ) ) 
END IF

il_row = row
end event


$PBExportHeader$w_eco_search_patron.srw
forward
global type w_eco_search_patron from w_main
end type
type cb_exit from u_cb within w_eco_search_patron
end type
type cb_clear from u_cb within w_eco_search_patron
end type
type st_1 from statictext within w_eco_search_patron
end type
type sle_rows from singlelineedit within w_eco_search_patron
end type
type dw_eco_units_search from u_dw within w_eco_search_patron
end type
type cb_find from commandbutton within w_eco_search_patron
end type
end forward

global type w_eco_search_patron from w_main
integer x = 5
integer y = 4
integer width = 2926
integer height = 1680
string title = "Search Screen"
cb_exit cb_exit
cb_clear cb_clear
st_1 st_1
sle_rows sle_rows
dw_eco_units_search dw_eco_units_search
cb_find cb_find
end type
global w_eco_search_patron w_eco_search_patron

type variables
boolean rows_exist=FALSE
end variables

on w_eco_search_patron.create
int iCurrent
call super::create
this.cb_exit=create cb_exit
this.cb_clear=create cb_clear
this.st_1=create st_1
this.sle_rows=create sle_rows
this.dw_eco_units_search=create dw_eco_units_search
this.cb_find=create cb_find
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.cb_exit
this.Control[iCurrent+2]=this.cb_clear
this.Control[iCurrent+3]=this.st_1
this.Control[iCurrent+4]=this.sle_rows
this.Control[iCurrent+5]=this.dw_eco_units_search
this.Control[iCurrent+6]=this.cb_find
end on

on w_eco_search_patron.destroy
call super::destroy
destroy(this.cb_exit)
destroy(this.cb_clear)
destroy(this.st_1)
destroy(this.sle_rows)
destroy(this.dw_eco_units_search)
destroy(this.cb_find)
end on

event open;call super::open;this.windowstate = maximized!

dw_eco_units_search.SetFocus()
end event

event pfc_preopen;this.of_SetPreference(TRUE)
this.inv_preference.of_SetToolBars(TRUE)
this.inv_preference.of_SetWindow(TRUE)
this.of_SetResize(TRUE)
this.inv_resize.of_SetOrigSize(this.workspacewidth(),this.workspaceheight())
inv_resize.of_register(cb_clear,"Scale")
inv_resize.of_register(cb_find,"Scale")
inv_resize.of_register(cb_exit,"Scale")
inv_resize.of_register(dw_eco_units_search,"Scale")
inv_resize.of_register(sle_rows,"Scale")
inv_resize.of_register(st_1,"Scale")

cb_clear.Enabled = FALSE

// Turn on query mode so user can specify data
dw_eco_units_search.object.DataWindow.QueryMode='Yes'

end event

type cb_exit from u_cb within w_eco_search_patron
integer x = 2597
integer y = 1448
integer width = 265
integer taborder = 0
boolean bringtotop = true
string text = "E&xit"
end type

event clicked;ib_disableclosequery=TRUE
close(parent)

end event

type cb_clear from u_cb within w_eco_search_patron
integer x = 2304
integer y = 1448
integer width = 265
integer taborder = 0
boolean bringtotop = true
string text = "&Clear"
end type

event clicked;long ll_rows

dw_eco_units_search.Reset()

// Clear the querymode of the datawindow.
//dw_eco_patron.Object.DataWindow.QueryClear = 'Yes'

ll_rows = dw_eco_units_search.InsertRow(0)


// Scroll to that row
dw_eco_units_search.ScrolltoRow(ll_rows)


// Reset Update flag in all the datawindow for exit button validation
dw_eco_units_search.ResetUpdate( )

//
dw_eco_units_search.Object.DataWindow.QueryClear = 'Yes'
dw_eco_units_search.Object.DataWindow.QueryMode = 'Yes'

cb_clear.Enabled = FALSE
cb_find.Enabled = TRUE

sle_rows.text=""

// Set the focus on the control number
dw_eco_units_search.setfocus()
end event

type st_1 from statictext within w_eco_search_patron
integer x = 32
integer y = 1452
integer width = 562
integer height = 76
boolean bringtotop = true
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 33554432
long backcolor = 67108864
boolean enabled = false
string text = "Rows Counted:"
boolean focusrectangle = false
end type

type sle_rows from singlelineedit within w_eco_search_patron
integer x = 366
integer y = 1444
integer width = 274
integer height = 92
boolean bringtotop = true
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 33554432
boolean autohscroll = false
textcase textcase = upper!
borderstyle borderstyle = stylelowered!
end type

type dw_eco_units_search from u_dw within w_eco_search_patron
integer x = 32
integer y = 24
integer width = 2848
integer height = 1364
integer taborder = 10
string dataobject = "d_eco_units_search"
end type

event constructor;this.of_SetFilter(TRUE)
this.of_SetFind(TRUE)
this.inv_filter.of_SetStyle(1)
this.inv_filter.of_SetColumnNameSource(2)
this.of_SetSort(TRUE)
this.inv_sort.of_SetStyle(1)
this.inv_sort.of_SetColumnNameSource(2)
this.of_SetTransObject(sqlservertrans)

end event

event retrievestart;call super::retrievestart;OpenWithParm(w_pics_retrieve_msg_box,"Retrieving records, Please wait...")

end event

event retrieveend;call super::retrieveend;Close(w_pics_retrieve_msg_box)

end event

type cb_find from commandbutton within w_eco_search_patron
integer x = 2021
integer y = 1448
integer width = 247
integer height = 92
integer taborder = 20
boolean bringtotop = true
integer textsize = -8
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "F&ind"
end type

event clicked;int ll_rows

dw_eco_units_search.object.DataWindow.QueryMode='No'

dw_eco_units_search.AcceptText()

ll_rows = dw_eco_units_search.Retrieve()
	
IF ll_rows = -1 THEN
	MessageBox("Error","Retrieve error ")
	cb_clear.TriggerEvent(Clicked!)
ELSEIF ll_rows = 0 THEN
	MessageBox("Retrieve","Record does not exist. ",Information!,OK!)
	cb_clear.TriggerEvent(Clicked!)
ELSE
	sle_rows.text = string(ll_rows)
	dw_eco_units_search.SetFocus( )
	cb_clear.Enabled = TRUE
	cb_find.Enabled = FALSE
END IF


end event


$PBExportHeader$w_sheet_cat_initial.srw
forward
global type w_sheet_cat_initial from w_sheet
end type
type st_1 from statictext within w_sheet_cat_initial
end type
type st_displayed from statictext within w_sheet_cat_initial
end type
type cb_find from commandbutton within w_sheet_cat_initial
end type
type cb_export from commandbutton within w_sheet_cat_initial
end type
type cb_exit from commandbutton within w_sheet_cat_initial
end type
type dw_cat_initials from u_pics_dw within w_sheet_cat_initial
end type
end forward

global type w_sheet_cat_initial from w_sheet
integer x = 46
integer y = 20
integer width = 2843
integer height = 1828
string title = "BCS Edit Info Report"
st_1 st_1
st_displayed st_displayed
cb_find cb_find
cb_export cb_export
cb_exit cb_exit
dw_cat_initials dw_cat_initials
end type
global w_sheet_cat_initial w_sheet_cat_initial

event open;call super::open;dw_cat_initials.of_SetTransObject(sqlservertrans)
w_sheet_cat_initial.cb_find.Event clicked()
dw_cat_initials.SetFocus()
end event

event resize;call super::resize;long ll_height

This.X = w_pics_main.X
This.Y = w_pics_main.Y
ll_height = w_pics_main.mdi_1.Height
This.Resize(w_pics_main.WorkSpaceWidth(), ll_height)

end event

event pfc_preopen;call super::pfc_preopen;this.of_SetPreference(TRUE)
this.inv_preference.of_SetToolBars(TRUE)
this.inv_preference.of_SetWindow(TRUE)
this.of_SetResize(TRUE)
this.inv_resize.of_SetOrigSize(this.workspacewidth(),this.workspaceheight())
inv_resize.of_Register(dw_cat_initials,"Scale")
inv_resize.of_Register(cb_exit, "Scale")
inv_resize.of_Register(cb_find, "Scale")
inv_resize.of_Register(cb_export, "Scale")
inv_resize.of_Register(st_1, "Scale")
inv_resize.of_Register(st_displayed, "Scale")

end event

on w_sheet_cat_initial.create
int iCurrent
call super::create
this.st_1=create st_1
this.st_displayed=create st_displayed
this.cb_find=create cb_find
this.cb_export=create cb_export
this.cb_exit=create cb_exit
this.dw_cat_initials=create dw_cat_initials
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_1
this.Control[iCurrent+2]=this.st_displayed
this.Control[iCurrent+3]=this.cb_find
this.Control[iCurrent+4]=this.cb_export
this.Control[iCurrent+5]=this.cb_exit
this.Control[iCurrent+6]=this.dw_cat_initials
end on

on w_sheet_cat_initial.destroy
call super::destroy
destroy(this.st_1)
destroy(this.st_displayed)
destroy(this.cb_find)
destroy(this.cb_export)
destroy(this.cb_exit)
destroy(this.dw_cat_initials)
end on

type st_1 from statictext within w_sheet_cat_initial
integer x = 46
integer y = 1536
integer width = 430
integer height = 72
integer textsize = -10
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 79741120
boolean enabled = false
string text = "Rows Selected:"
boolean focusrectangle = false
end type

type st_displayed from statictext within w_sheet_cat_initial
integer x = 475
integer y = 1520
integer width = 242
integer height = 100
integer textsize = -10
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 16777215
boolean enabled = false
boolean border = true
borderstyle borderstyle = stylelowered!
boolean focusrectangle = false
end type

type cb_find from commandbutton within w_sheet_cat_initial
event clicked pbm_bnclicked
integer x = 1888
integer y = 1540
integer width = 270
integer height = 108
integer taborder = 30
integer textsize = -10
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "F&ind"
end type

event clicked;string rtn,rc,mod_string
long ll_rows
integer res

w_sheet_cat_initial.dw_cat_initials.AcceptText()

// IF find is dispalyed on the push button
IF cb_find.BringToTop = FALSE THEN
	// disable the buttons 
	cb_export.Enabled = FALSE
	// Turn on query mode so user can specify data
	rtn = w_sheet_cat_initial.dw_cat_initials.Modify("DataWindow.QueryMode=YES")
	IF rtn = "" THEN
		// If Modify succeeds, show Execute,
		// Query mode is on and display sort CheckBox
		This.BringToTop = TRUE
		This.Text = "Exec&ute"
	ELSE
		MessageBox("Error", "Can't access query mode to select data.")
	END IF
ELSE
	// Turn off Query mode and retrieve data 
	// based on user's choices
	rtn = w_sheet_cat_initial.dw_cat_initials.Modify("DataWindow.QueryMode=NO")
	IF rtn = "" THEN
		// If Modify succeeds, show Find,
		// Query mode is off, and retrieve data
		This.BringToTop = FALSE
		This.Text = "F&ind"
		ll_rows=w_sheet_cat_initial.dw_cat_initials.Retrieve()
		IF ll_rows > 0 THEN 
			// If any rows were retrieved.
			st_displayed.text=String(dw_cat_initials.RowCount())
			cb_exit.Enabled = TRUE
			cb_export.Enabled = TRUE
		ELSE
			// If no rows were retrieved, ask if they want to continue with retrieval.
			st_displayed.text=String(dw_cat_initials.RowCount())
			res = MessageBox("Retrieve Error","No records were retrieved. Continue with query mode?", Question!, OkCancel!, 2 )
			IF res = 1 THEN
				// If yes continue the reterival process
				cb_find.TriggerEvent(Clicked!)
			ELSE
				// Restore the original select statement and modify the datawindow
				// mod_string is a shared variable, and is set in ue_postconstructor event of dw_cat_initials.
				res = dw_cat_initials.Reset()
				IF res = 1 THEN
				// Restore the original data from dataobject
				   dw_cat_initials.DataObject = dw_cat_initials.DataObject
					dw_cat_initials.SetTransObject( sqlservertrans )
					mod_string = "DataWindow.Table.Select='" + dw_cat_initials.Describe("DataWindow.Table.Select") + "'"
					rc = dw_cat_initials.Modify(mod_string)
				// if the modify select statement fails display error
				// messages, else retrieve the original data.
					IF rc = "" THEN
						ll_rows = dw_cat_initials.Event pfc_retrieve()
						IF ll_rows > 0 THEN
							st_displayed.text=String(dw_cat_initials.RowCount())
							cb_exit.Enabled = TRUE
							cb_export.Enabled = TRUE
						END IF // ll_rows > 0
					ELSE
						MessageBox("Error","Error in restoring the original select statement. ReturnCode=" + rc)
					END IF // rc = ""
				END IF // res = 1
			END IF // res = 1 
		END IF // ll_rows > 0
	ELSE
		MessageBox("Error","Failure exiting query mode.")
		cb_exit.Enabled = TRUE
		cb_export.Enabled = TRUE
	END IF // rtn = ""
END IF // if pb_find...
end event

type cb_export from commandbutton within w_sheet_cat_initial
event clicked pbm_bnclicked
integer x = 2222
integer y = 1540
integer width = 270
integer height = 108
integer taborder = 20
integer textsize = -10
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Exp&ort"
end type

event clicked;dw_cat_initials.SaveAs()
end event

type cb_exit from commandbutton within w_sheet_cat_initial
event clicked pbm_bnclicked
integer x = 2542
integer y = 1540
integer width = 229
integer height = 108
integer taborder = 10
integer textsize = -10
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "E&xit"
end type

event clicked;close(parent)
m_pics_main.m_menu.PopMenu(300, 0)

end event

type dw_cat_initials from u_pics_dw within w_sheet_cat_initial
integer x = 23
integer y = 28
integer width = 2752
integer height = 1456
integer taborder = 0
string dataobject = "d_cat_initials"
boolean hscrollbar = true
end type

event constructor;call super::constructor;this.of_SetFilter(TRUE)
this.of_SetFind(TRUE)
this.inv_filter.of_SetStyle(1)
this.inv_filter.of_SetColumnNameSource(2)
this.of_SetSort(TRUE)
this.inv_sort.of_SetStyle(1)
this.inv_sort.of_SetColumnNameSource(2)

end event

event pfc_deleterow;//
RETURN 1
end event

event pfc_insertrow;//
RETURN 1
end event

event pfc_addrow;//
RETURN 1
end event


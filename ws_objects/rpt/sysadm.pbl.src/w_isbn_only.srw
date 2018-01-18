$PBExportHeader$w_isbn_only.srw
forward
global type w_isbn_only from w_sheet
end type
type cb_deselect from u_cb within w_isbn_only
end type
type cb_selectall from u_cb within w_isbn_only
end type
type dw_isbn_bkno from u_pics_dw within w_isbn_only
end type
type cb_print from commandbutton within w_isbn_only
end type
type cb_exit from u_cb within w_isbn_only
end type
type dw_isbn_conno from u_dw within w_isbn_only
end type
end forward

global type w_isbn_only from w_sheet
integer width = 3360
integer height = 1552
string title = "ISBN Table"
cb_deselect cb_deselect
cb_selectall cb_selectall
dw_isbn_bkno dw_isbn_bkno
cb_print cb_print
cb_exit cb_exit
dw_isbn_conno dw_isbn_conno
end type
global w_isbn_only w_isbn_only

on w_isbn_only.create
int iCurrent
call super::create
this.cb_deselect=create cb_deselect
this.cb_selectall=create cb_selectall
this.dw_isbn_bkno=create dw_isbn_bkno
this.cb_print=create cb_print
this.cb_exit=create cb_exit
this.dw_isbn_conno=create dw_isbn_conno
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.cb_deselect
this.Control[iCurrent+2]=this.cb_selectall
this.Control[iCurrent+3]=this.dw_isbn_bkno
this.Control[iCurrent+4]=this.cb_print
this.Control[iCurrent+5]=this.cb_exit
this.Control[iCurrent+6]=this.dw_isbn_conno
end on

on w_isbn_only.destroy
call super::destroy
destroy(this.cb_deselect)
destroy(this.cb_selectall)
destroy(this.dw_isbn_bkno)
destroy(this.cb_print)
destroy(this.cb_exit)
destroy(this.dw_isbn_conno)
end on

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

inv_resize.of_Register(cb_exit, "scale")
inv_resize.of_Register(cb_print, "scale")
inv_resize.of_Register(cb_deselect, "scale")
inv_resize.of_Register(cb_selectall, "scale")
inv_resize.of_Register(dw_isbn_conno, "scale")
end event

event open;call super::open;//open the sheet in maximized state
this.windowstate = maximized!


end event

event mousemove;call super::mousemove;w_pics_main.Event pfc_microhelp("Ready")
end event

type cb_deselect from u_cb within w_isbn_only
string tag = "Deselects all the menu items"
integer x = 974
integer y = 1292
integer width = 430
integer taborder = 40
integer textsize = -10
string text = "Unselect Al&l"
end type

event clicked;call super::clicked;Integer li_loop, li_maxrows

//This will enable all the items in the datawindow
li_maxrows = dw_isbn_conno.RowCount()
For li_loop = 1 to li_maxrows
	dw_isbn_conno.SetItem(li_loop,"prnt",'N')
NEXT

end event

type cb_selectall from u_cb within w_isbn_only
string tag = "Selects all the menu items"
integer x = 553
integer y = 1292
integer width = 361
integer taborder = 30
integer textsize = -10
string text = "&Select All"
end type

event clicked;call super::clicked;Integer li_loop, li_maxrows

//This will enable all the items in the datawindow
li_maxrows = dw_isbn_conno.RowCount()
For li_loop = 1 to li_maxrows
	dw_isbn_conno.SetItem(li_loop,"prnt",'Y')
NEXT

end event

type dw_isbn_bkno from u_pics_dw within w_isbn_only
boolean visible = false
integer x = 2057
integer y = 1292
integer width = 151
integer height = 96
integer taborder = 20
string dataobject = "d_isbn_bkno"
boolean vscrollbar = false
boolean border = false
boolean livescroll = false
end type

event constructor;call super::constructor;this.SetTransObject(SqlServerTrans)

end event

type cb_print from commandbutton within w_isbn_only
integer x = 37
integer y = 1292
integer width = 457
integer height = 96
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "Print Labels"
end type

event clicked;int i, ll_rows
string DWfilter

dw_isbn_conno.accepttext()

DWfilter = "prnt = 'Y'"
dw_isbn_conno.SetFilter(DWfilter)
dw_isbn_conno.Filter( )

ll_rows = dw_isbn_conno.rowcount()

IF ll_rows >= 1 THEN
	
dw_isbn_bkno.reset()

	FOR i = 1 to ll_rows
		dw_isbn_bkno.InsertRow(0)
		dw_isbn_bkno.object.bkno[i] = dw_isbn_conno.object.bkno[i]
		dw_isbn_bkno.object.conno[i] = dw_isbn_conno.object.conno[i]
		dw_isbn_bkno.object.sttl[i] = dw_isbn_conno.object.sttl[i]
	NEXT
	
	dw_isbn_bkno.accepttext()
		
	dw_isbn_bkno.SaveAs("label.csv",CSV!, TRUE)
	
	MessageBox("Labels","label.csv was created in the default directory with these records. You will need to run the PT-Editor to create the labels.")
	
	Run("Ptedit3.exe")
ELSE
	MessageBox("Labels","There are no books selected.")
	RETURN
END IF
end event

type cb_exit from u_cb within w_isbn_only
event ue_hint_text pbm_mousemove
string tag = "Exits the screen"
integer x = 2976
integer y = 1292
integer width = 297
integer height = 96
integer taborder = 0
integer textsize = -10
string text = "E&xit"
end type

event ue_hint_text;call super::ue_hint_text;w_pics_main.Event pfc_microhelp(this.tag)
end event

event clicked;call super::clicked;ib_disableclosequery = TRUE
Parent.Event pfc_close()

end event

type dw_isbn_conno from u_dw within w_isbn_only
event ue_enter_to_tab pbm_dwnprocessenter
event ue_hint_text pbm_mousemove
integer x = 23
integer y = 48
integer width = 3264
integer height = 1228
integer taborder = 10
string dataobject = "d_isbn_only"
boolean hscrollbar = true
end type

event constructor;this.of_SetFilter(TRUE)
this.of_SetFind(TRUE)
this.of_SetSort(TRUE)
this.inv_filter.of_SetStyle(1)
this.inv_filter.of_SetColumnNameSource(2)
this.inv_sort.of_SetStyle(1)
this.inv_sort.of_SetColumnNameSource(2)

this.SetTransObject(SqlServerTrans)
this.retrieve()

end event

event retrieveend;call super::retrieveend;int i

FOR i = 1 to rowcount
	dw_isbn_conno.object.prnt[i]='N'
NEXT
end event

event pfc_deleterow;call super::pfc_deleterow;//
return -1
end event


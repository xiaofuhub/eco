$PBExportHeader$w_bcs_editinfo_without_stage2_date.srw
forward
global type w_bcs_editinfo_without_stage2_date from w_sheet
end type
type cb_dbcancel from commandbutton within w_bcs_editinfo_without_stage2_date
end type
type dw_bcs_editinfo_with_no_s2out from u_pics_dw within w_bcs_editinfo_without_stage2_date
end type
type cb_export from commandbutton within w_bcs_editinfo_without_stage2_date
end type
type cb_print from commandbutton within w_bcs_editinfo_without_stage2_date
end type
type cb_exit from commandbutton within w_bcs_editinfo_without_stage2_date
end type
type st_1 from statictext within w_bcs_editinfo_without_stage2_date
end type
type sle_selected from singlelineedit within w_bcs_editinfo_without_stage2_date
end type
end forward

global type w_bcs_editinfo_without_stage2_date from w_sheet
integer x = 69
integer y = 100
integer width = 2816
integer height = 1616
string title = "Editinfo records without stage II date in"
cb_dbcancel cb_dbcancel
dw_bcs_editinfo_with_no_s2out dw_bcs_editinfo_with_no_s2out
cb_export cb_export
cb_print cb_print
cb_exit cb_exit
st_1 st_1
sle_selected sle_selected
end type
global w_bcs_editinfo_without_stage2_date w_bcs_editinfo_without_stage2_date

type variables
boolean ib_cancel=FALSE
end variables

on w_bcs_editinfo_without_stage2_date.create
int iCurrent
call super::create
this.cb_dbcancel=create cb_dbcancel
this.dw_bcs_editinfo_with_no_s2out=create dw_bcs_editinfo_with_no_s2out
this.cb_export=create cb_export
this.cb_print=create cb_print
this.cb_exit=create cb_exit
this.st_1=create st_1
this.sle_selected=create sle_selected
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.cb_dbcancel
this.Control[iCurrent+2]=this.dw_bcs_editinfo_with_no_s2out
this.Control[iCurrent+3]=this.cb_export
this.Control[iCurrent+4]=this.cb_print
this.Control[iCurrent+5]=this.cb_exit
this.Control[iCurrent+6]=this.st_1
this.Control[iCurrent+7]=this.sle_selected
end on

on w_bcs_editinfo_without_stage2_date.destroy
call super::destroy
destroy(this.cb_dbcancel)
destroy(this.dw_bcs_editinfo_with_no_s2out)
destroy(this.cb_export)
destroy(this.cb_print)
destroy(this.cb_exit)
destroy(this.st_1)
destroy(this.sle_selected)
end on

event pfc_preopen;call super::pfc_preopen;this.of_SetPreference(TRUE)
this.inv_preference.of_SetToolBars(TRUE)
this.inv_preference.of_SetWindow(TRUE)
this.of_SetResize(TRUE)
this.inv_resize.of_SetOrigSize(this.workspacewidth(),this.workspaceheight())

inv_resize.of_Register(dw_bcs_editinfo_with_no_s2out, "Scale")
inv_resize.of_Register(sle_selected, "Scale")
inv_resize.of_Register(st_1, "Scale")
inv_resize.of_Register(cb_dbcancel, "Scale")
inv_resize.of_Register(cb_exit, "Scale")
inv_resize.of_Register(cb_print, "Scale")
inv_resize.of_Register(cb_export, "Scale")

end event

event resize;call super::resize;long ll_height

This.X = w_pics_main.X
This.Y = w_pics_main.Y
ll_height = w_pics_main.mdi_1.Height
This.Resize(w_pics_main.WorkSpaceWidth(), ll_height)

end event

event pfc_postopen;call super::pfc_postopen;long ll_rows
date sdate

SetMicroHelp(w_pics_main,"Please Wait...")
cb_export.Enabled = FALSE
cb_print.Enabled = FALSE

ll_rows = dw_bcs_editinfo_with_no_s2out.Retrieve()
sle_selected.text = string(ll_rows)
if ll_rows < 1 THEN 
 	MessageBox("ERROR", "No rows retrieved.",Exclamation!)
	SetMicroHelp(w_pics_main,"")
else
	SetMicroHelp(w_pics_main,"")
	cb_export.Enabled = TRUE
	cb_print.Enabled = TRUE
	dw_bcs_editinfo_with_no_s2out.SetFocus()
end if

end event

type cb_dbcancel from commandbutton within w_bcs_editinfo_without_stage2_date
integer x = 1207
integer y = 1364
integer width = 293
integer height = 116
integer taborder = 50
integer textsize = -8
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "&DBCancel"
end type

event clicked;ib_cancel=TRUE
SetMicroHelp(w_pics_main,"")
cb_export.Enabled = TRUE
cb_print.Enabled = TRUE
cb_dbcancel.Enabled = FALSE
close(w_pics_retrieve_msg_box)
end event

type dw_bcs_editinfo_with_no_s2out from u_pics_dw within w_bcs_editinfo_without_stage2_date
event rbuttondown pbm_dwnrbuttondown
event rbuttonup pbm_dwnrbuttonup
event ue_postconstructor ( )
integer x = 41
integer y = 32
integer width = 2683
integer height = 1296
integer taborder = 30
string dataobject = "d_bcs_editinfo_with_no_s2out"
boolean hscrollbar = true
end type

event rbuttondown;//
end event

event rbuttonup;//
end event

event ue_postconstructor;//
end event

event constructor;call super::constructor;this.SetTransObject( SQLServerTrans )
this.of_SetFilter(TRUE)
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

event pfc_addrow;call super::pfc_addrow;//
RETURN 1
end event

event retrievestart;call super::retrievestart;OpenWithParm(w_pics_retrieve_msg_box,"Retrieving data, Please Wait...")
end event

event retrieveend;call super::retrieveend;IF ib_cancel THEN
	return 1
END IF
close(w_pics_retrieve_msg_box)
end event

type cb_export from commandbutton within w_bcs_editinfo_without_stage2_date
event clicked pbm_bnclicked
integer x = 1806
integer y = 1364
integer width = 293
integer height = 116
integer taborder = 40
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "Expor&t"
end type

event clicked;dw_bcs_editinfo_with_no_s2out.SaveAs()

end event

type cb_print from commandbutton within w_bcs_editinfo_without_stage2_date
event clicked pbm_bnclicked
integer x = 2112
integer y = 1364
integer width = 293
integer height = 116
integer taborder = 20
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "&Print"
end type

event clicked;long job
int Ans

Ans = MessageBox("Print", "Print Report information?", Question!, OKCancel!, 2)
if Ans = 1 THEN 
  job = PrintOpen( ) 
  PrintDataWindow(job, dw_bcs_editinfo_with_no_s2out) 
  PrintClose(job)
else
  return
end if

end event

type cb_exit from commandbutton within w_bcs_editinfo_without_stage2_date
event clicked pbm_bnclicked
integer x = 2427
integer y = 1364
integer width = 293
integer height = 116
integer taborder = 10
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "E&xit"
end type

event clicked;close(parent)
end event

type st_1 from statictext within w_bcs_editinfo_without_stage2_date
integer x = 69
integer y = 1376
integer width = 329
integer height = 76
boolean bringtotop = true
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long backcolor = 12632256
boolean enabled = false
string text = "Row Selected:"
boolean focusrectangle = false
end type

type sle_selected from singlelineedit within w_bcs_editinfo_without_stage2_date
integer x = 407
integer y = 1372
integer width = 247
integer height = 64
boolean bringtotop = true
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long backcolor = 16777215
boolean autohscroll = false
borderstyle borderstyle = stylelowered!
end type


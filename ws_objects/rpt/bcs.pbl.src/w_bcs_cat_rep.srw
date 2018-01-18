$PBExportHeader$w_bcs_cat_rep.srw
forward
global type w_bcs_cat_rep from w_sheet
end type
type dw_cat_rep from u_pics_dw within w_bcs_cat_rep
end type
type cb_export from commandbutton within w_bcs_cat_rep
end type
type cb_print from commandbutton within w_bcs_cat_rep
end type
type cb_exit from commandbutton within w_bcs_cat_rep
end type
end forward

global type w_bcs_cat_rep from w_sheet
integer x = 55
integer y = 88
integer width = 2816
integer height = 1616
string title = "BCS Catalog date Report"
dw_cat_rep dw_cat_rep
cb_export cb_export
cb_print cb_print
cb_exit cb_exit
end type
global w_bcs_cat_rep w_bcs_cat_rep

on w_bcs_cat_rep.create
int iCurrent
call super::create
this.dw_cat_rep=create dw_cat_rep
this.cb_export=create cb_export
this.cb_print=create cb_print
this.cb_exit=create cb_exit
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_cat_rep
this.Control[iCurrent+2]=this.cb_export
this.Control[iCurrent+3]=this.cb_print
this.Control[iCurrent+4]=this.cb_exit
end on

on w_bcs_cat_rep.destroy
call super::destroy
destroy(this.dw_cat_rep)
destroy(this.cb_export)
destroy(this.cb_print)
destroy(this.cb_exit)
end on

event pfc_preopen;call super::pfc_preopen;this.of_SetPreference(TRUE)
this.inv_preference.of_SetToolBars(TRUE)
this.inv_preference.of_SetWindow(TRUE)
this.of_SetResize(TRUE)
this.inv_resize.of_SetOrigSize(this.workspacewidth(),this.workspaceheight())
inv_resize.of_Register(dw_cat_rep, "Scale")
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

type dw_cat_rep from u_pics_dw within w_bcs_cat_rep
event rbuttondown pbm_dwnrbuttondown
event rbuttonup pbm_dwnrbuttonup
event ue_postconstructor ( )
integer x = 41
integer y = 32
integer width = 2683
integer height = 1296
integer taborder = 30
string dataobject = "d_cat_rep"
boolean vscrollbar = false
boolean livescroll = false
boolean ib_isupdateable = false
boolean ib_rmbmenu = false
boolean ib_rmbfocuschange = false
string is_updatesallowed = ""
end type

event rbuttondown;//
end event

event rbuttonup;//
end event

event ue_postconstructor;long ll_rows
date cdate
string original_select,where_clause,mod_string, ls_rc

IF IsValid(w_catdate_rep) THEN
	cdate = w_catdate_rep.catdate
END IF

dw_cat_rep.SetTransObject( SQLServerTrans )
SetMicroHelp(w_pics_main,"Please Wait...")
cb_export.Enabled = FALSE
cb_print.Enabled = FALSE

original_select =	dw_cat_rep.Describe("DataWindow.Table.Select")
//" WHERE ~~'"+To_CHAR(catalog.cat, 'mm/dd/yyyy')+"~~' = "+"~~'" +string(cdate,'MM/DD/YYYY') + "~~'"
//where_clause = " WHERE To_CHAR(catalog.cat, "+"~'"+"MM/DD/YYYY"+"~') = "+ "~'" + string(cdate,'MM/DD/YYYY') + "~'"
//where_clause = " WHERE To_CHAR(catalog.cat, "+"~~'"+"MM/DD/YYYY"+"~~') = "+ "~~'" + string(cdate,'MM/DD/YYYY') + "~~'"

mod_string = "DataWindow.Table.Select='" + original_select + where_clause  + "	ORDER BY catalog.conno DESC " + "'"

ls_rc=dw_cat_rep.Modify(mod_string)
ll_rows = dw_cat_rep.Retrieve()
if ll_rows < 1 THEN 
 	MessageBox("ERROR", "No rows retrieved.",Exclamation!)
	SetMicroHelp(w_pics_main,"")
 	close(w_bcs_cat_rep)
else
	SetMicroHelp(w_pics_main,"")
	cb_export.Enabled = TRUE
	cb_print.Enabled = TRUE
	close(w_catdate_rep)
end if

end event

type cb_export from commandbutton within w_bcs_cat_rep
event clicked pbm_bnclicked
integer x = 1851
integer y = 1360
integer width = 247
integer height = 120
integer taborder = 40
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "Expor&t"
end type

event clicked;dw_cat_rep.SaveAs()

end event

type cb_print from commandbutton within w_bcs_cat_rep
event clicked pbm_bnclicked
integer x = 2158
integer y = 1360
integer width = 247
integer height = 120
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
  PrintDataWindow(job, dw_cat_rep) 
  PrintClose(job)
else
  return
end if

end event

type cb_exit from commandbutton within w_bcs_cat_rep
event clicked pbm_bnclicked
integer x = 2473
integer y = 1356
integer width = 247
integer height = 120
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


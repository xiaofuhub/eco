$PBExportHeader$w_sheet_conno_info.srw
forward
global type w_sheet_conno_info from w_sheet
end type
type dw_conno_info from u_pics_dw within w_sheet_conno_info
end type
type cb_exit from commandbutton within w_sheet_conno_info
end type
end forward

global type w_sheet_conno_info from w_sheet
integer x = 46
integer y = 28
integer width = 2839
integer height = 1644
dw_conno_info dw_conno_info
cb_exit cb_exit
end type
global w_sheet_conno_info w_sheet_conno_info

on w_sheet_conno_info.create
int iCurrent
call super::create
this.dw_conno_info=create dw_conno_info
this.cb_exit=create cb_exit
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_conno_info
this.Control[iCurrent+2]=this.cb_exit
end on

on w_sheet_conno_info.destroy
call super::destroy
destroy(this.dw_conno_info)
destroy(this.cb_exit)
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
inv_resize.of_Register(dw_conno_info, "Scale")
inv_resize.of_Register(cb_exit, "Scale")

end event

type dw_conno_info from u_pics_dw within w_sheet_conno_info
event rbuttondown pbm_dwnrbuttondown
event rbuttonup pbm_dwnrbuttonup
event ue_postconstructor ( )
integer x = 18
integer y = 16
integer width = 2757
integer height = 1336
boolean enabled = false
string dataobject = "d_conno_info"
boolean vscrollbar = false
boolean livescroll = false
end type

event rbuttondown;//
end event

event rbuttonup;//
end event

event ue_postconstructor;call super::ue_postconstructor;string original_select,mod_string,where_clause,rc,Lconno
int ll_rows,RowNum

RowNum = w_sheet_catalog.dw_catalog.GetRow()
Lconno = w_sheet_catalog.dw_catalog.Object.cconno[RowNum]

dw_conno_info.SetTransObject( SQLServerTrans )

original_select =	dw_conno_info.Describe("DataWindow.Table.Select")

where_clause = " AND mchar.conno="+ "~'" + string(Lconno) + "~'"

mod_string = "DataWindow.Table.Select=~"" + original_select + where_clause + "~""

rc = dw_conno_info.Modify(mod_string)
IF rc = "" THEN
	ll_rows = dw_conno_info.Retrieve()
   IF ll_rows < 1 THEN 
     MessageBox("Error", "There are no conno number " + string(Lconno),StopSign!, OK!, 2)
     close(w_sheet_conno_info)
     return
   end if
ELSE
	MessageBox("Status", "Modify Failed" + rc)
END if


end event

event retrievestart;call super::retrievestart;OpenWithParm(w_pics_retrieve_msg_box,"Retrieving BCS Information, Please Wait...")
end event

event retrieveend;call super::retrieveend;close(w_pics_retrieve_msg_box)
end event

type cb_exit from commandbutton within w_sheet_conno_info
event clicked pbm_bnclicked
integer x = 2523
integer y = 1384
integer width = 247
integer height = 108
integer taborder = 2
integer textsize = -10
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Exit"
end type

event clicked;close(parent)
end event


$PBExportHeader$w_add_cntr_cmts.srw
forward
global type w_add_cntr_cmts from w_response
end type
type cb_clear from commandbutton within w_add_cntr_cmts
end type
type cb_new from commandbutton within w_add_cntr_cmts
end type
type dw_add_cntr_cmts from u_pics_dw within w_add_cntr_cmts
end type
type cb_exit from commandbutton within w_add_cntr_cmts
end type
type cb_update from commandbutton within w_add_cntr_cmts
end type
end forward

global type w_add_cntr_cmts from w_response
integer x = 302
integer y = 400
integer width = 2469
integer height = 792
string title = "Add Contract Comments"
cb_clear cb_clear
cb_new cb_new
dw_add_cntr_cmts dw_add_cntr_cmts
cb_exit cb_exit
cb_update cb_update
end type
global w_add_cntr_cmts w_add_cntr_cmts

forward prototypes
public function long wf_calc_max_msgid ()
end prototypes

public function long wf_calc_max_msgid ();long msgcnt

select count(*) into :msgcnt
from message
using sqlserveroracletrans;

return msgcnt+1
end function

on w_add_cntr_cmts.create
int iCurrent
call super::create
this.cb_clear=create cb_clear
this.cb_new=create cb_new
this.dw_add_cntr_cmts=create dw_add_cntr_cmts
this.cb_exit=create cb_exit
this.cb_update=create cb_update
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.cb_clear
this.Control[iCurrent+2]=this.cb_new
this.Control[iCurrent+3]=this.dw_add_cntr_cmts
this.Control[iCurrent+4]=this.cb_exit
this.Control[iCurrent+5]=this.cb_update
end on

on w_add_cntr_cmts.destroy
call super::destroy
destroy(this.cb_clear)
destroy(this.cb_new)
destroy(this.dw_add_cntr_cmts)
destroy(this.cb_exit)
destroy(this.cb_update)
end on

type cb_clear from commandbutton within w_add_cntr_cmts
integer x = 1467
integer y = 564
integer width = 251
integer height = 116
integer taborder = 40
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "&Clear"
end type

event clicked;dw_add_cntr_cmts.Reset()
cb_new.TriggerEvent(Clicked!)

end event

type cb_new from commandbutton within w_add_cntr_cmts
boolean visible = false
integer x = 1147
integer y = 564
integer width = 261
integer height = 116
integer taborder = 20
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "&New"
end type

event clicked;int row_num

dw_add_cntr_cmts.event pfc_addrow()
row_num = dw_add_cntr_cmts.RowCount()
dw_add_cntr_cmts.ScrollToRow(row_num)
dw_add_cntr_cmts.object.rpt_month[row_num]=w_pcs_reports.cntr_st_dt

dw_add_cntr_cmts.object.cntr[row_num]=w_pcs_reports.cntr_number


end event

type dw_add_cntr_cmts from u_pics_dw within w_add_cntr_cmts
event ue_enterkey pbm_dwnprocessenter
integer x = 37
integer y = 20
integer width = 2391
integer height = 516
integer taborder = 20
string dataobject = "d_add_cnt_cmts"
boolean vscrollbar = false
boolean livescroll = false
end type

event ue_enterkey;Send(Handle(this),256,9,Long(0,0))
return(1)

end event

event constructor;call super::constructor;// set the transaction object.
This.of_SetTransObject(SQLServerTrans)

end event

event ue_postconstructor;call super::ue_postconstructor;string lcntr
date ld_stdt
int rtn

ld_stdt = w_pcs_reports.cntr_st_dt

lcntr = w_pcs_reports.cntr_number

This.of_SetTransObject(SQLServerTrans)

rtn = this.retrieve(lcntr,ld_stdt)
if rtn = 0 then
	cb_new.TriggerEvent(Clicked!)
end if
end event

type cb_exit from commandbutton within w_add_cntr_cmts
integer x = 2176
integer y = 564
integer width = 247
integer height = 116
boolean bringtotop = true
integer textsize = -10
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "E&xit"
end type

event clicked;dw_add_cntr_cmts.ResetUpdate()
parent.Event pfc_close()

end event

type cb_update from commandbutton within w_add_cntr_cmts
integer x = 1787
integer y = 564
integer width = 311
integer height = 116
boolean bringtotop = true
integer textsize = -10
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "&Update"
end type

event clicked;int rc

// Accept the text that was put on the screen.
dw_add_cntr_cmts.AcceptText()

rc = dw_add_cntr_cmts.Event pfc_update(TRUE, TRUE)
if rc = 1 THEN
	COMMIT USING SQLServerOracleTrans;
	MessageBox("Update","Update Successful.",Information!)
	dw_add_cntr_cmts.Resetupdate()
	return 1
else
	MessageBox("ERROR","Update failed.",StopSign!)
	dw_add_cntr_cmts.Resetupdate()
	RETURN 0
end if

end event


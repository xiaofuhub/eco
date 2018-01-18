$PBExportHeader$w_bcs_reset_stage2_out.srw
forward
global type w_bcs_reset_stage2_out from w_response
end type
type cb_ok from commandbutton within w_bcs_reset_stage2_out
end type
type cb_cancel from commandbutton within w_bcs_reset_stage2_out
end type
type st_reset from statictext within w_bcs_reset_stage2_out
end type
type em_conno from uo_conno within w_bcs_reset_stage2_out
end type
type st_1 from statictext within w_bcs_reset_stage2_out
end type
end forward

global type w_bcs_reset_stage2_out from w_response
integer x = 809
integer y = 412
integer width = 1056
integer height = 788
string title = "Control Number reset"
cb_ok cb_ok
cb_cancel cb_cancel
st_reset st_reset
em_conno em_conno
st_1 st_1
end type
global w_bcs_reset_stage2_out w_bcs_reset_stage2_out

on w_bcs_reset_stage2_out.create
int iCurrent
call super::create
this.cb_ok=create cb_ok
this.cb_cancel=create cb_cancel
this.st_reset=create st_reset
this.em_conno=create em_conno
this.st_1=create st_1
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.cb_ok
this.Control[iCurrent+2]=this.cb_cancel
this.Control[iCurrent+3]=this.st_reset
this.Control[iCurrent+4]=this.em_conno
this.Control[iCurrent+5]=this.st_1
end on

on w_bcs_reset_stage2_out.destroy
call super::destroy
destroy(this.cb_ok)
destroy(this.cb_cancel)
destroy(this.st_reset)
destroy(this.em_conno)
destroy(this.st_1)
end on

type cb_ok from commandbutton within w_bcs_reset_stage2_out
integer x = 201
integer y = 552
integer width = 247
integer height = 108
boolean bringtotop = true
integer textsize = -10
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "&Ok"
boolean default = true
end type

event clicked;string Lconno,Lcon
date Ls2out,Lcat,Ls2in
int rtn

Lconno = em_conno.text

select conno,s2in,s2out,cat into :Lcon,:Ls2in,:Ls2out,:Lcat from catalog where conno=:Lconno
using sqlservertrans;

If Sqlservertrans.sqlcode=100 then
	MessageBox("ERROR","Invalid Control number.")
	em_conno.SetFocus()
elseif Sqlservertrans.sqlcode=0 then
	IF IsNull(Ls2in)=TRUE THEN
		MessageBox("ERROR","Control number: "+Lcon+" has not passed STAGE I.",StopSign!)
	ELSEIF IsNull(Lcat)=FALSE THEN
		MessageBox("ERROR","Control number: "+Lcon+" has already passed Final Approval.",StopSign!)
	ELSE	
		update catalog set s2out=NULL where conno=:Lconno using sqlservertrans;
		if sqlservertrans.sqlcode=0 then
			commit using sqlservertrans;
			close(parent)
		else
			MessageBox("ERROR","Error in updating control number.")
			return -1
		end if
	END IF
else
	MessageBox("ERROR","Error in select statement.")
end if
	
		
end event

type cb_cancel from commandbutton within w_bcs_reset_stage2_out
integer x = 544
integer y = 552
integer width = 297
integer height = 108
boolean bringtotop = true
integer textsize = -10
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "&Cancel"
end type

event clicked;close(parent)
end event

type st_reset from statictext within w_bcs_reset_stage2_out
integer x = 55
integer y = 36
integer width = 928
integer height = 200
boolean bringtotop = true
integer textsize = -10
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long backcolor = 79741120
boolean enabled = false
string text = "Please Enter the control number, that you want to reset it~'s Stage II date out."
alignment alignment = center!
boolean focusrectangle = false
end type

type em_conno from uo_conno within w_bcs_reset_stage2_out
integer x = 507
integer y = 352
integer width = 325
integer height = 92
integer taborder = 10
maskdatatype maskdatatype = stringmask!
string displaydata = "~t/"
end type

type st_1 from statictext within w_bcs_reset_stage2_out
integer x = 206
integer y = 364
integer width = 302
integer height = 64
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long backcolor = 79741120
boolean enabled = false
string text = "Control No:"
boolean focusrectangle = false
end type


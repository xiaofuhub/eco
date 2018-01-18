$PBExportHeader$w_mm_qastg_info.srw
forward
global type w_mm_qastg_info from w_response
end type
type cb_exit from commandbutton within w_mm_qastg_info
end type
type dw_qa_qastg from u_pics_dw within w_mm_qastg_info
end type
end forward

global type w_mm_qastg_info from w_response
integer width = 1682
integer height = 984
string title = "Quality Assurance (View only)"
cb_exit cb_exit
dw_qa_qastg dw_qa_qastg
end type
global w_mm_qastg_info w_mm_qastg_info

on w_mm_qastg_info.create
int iCurrent
call super::create
this.cb_exit=create cb_exit
this.dw_qa_qastg=create dw_qa_qastg
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.cb_exit
this.Control[iCurrent+2]=this.dw_qa_qastg
end on

on w_mm_qastg_info.destroy
call super::destroy
destroy(this.cb_exit)
destroy(this.dw_qa_qastg)
end on

type cb_exit from commandbutton within w_mm_qastg_info
integer x = 1280
integer y = 736
integer width = 329
integer height = 96
integer taborder = 20
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "E&xit"
end type

event clicked;close(parent)
end event

type dw_qa_qastg from u_pics_dw within w_mm_qastg_info
integer x = 41
integer y = 24
integer width = 1568
integer height = 696
integer taborder = 10
string dataobject = "d_mm_view_qastg"
end type

event ue_postconstructor;call super::ue_postconstructor;long Lbkseq
string Lmed,Lcntr
int rtn

dw_qa_qastg.SetTransObject(sqlservertrans) 
	
//Lbkseq	= w_mm_extend_due_date.dw_manu_ext.object.ext_bkseq[1]
Lbkseq	= long(w_mm_extend_due_date.em_bkno.text)
Lcntr		= w_mm_extend_due_date.dw_cntr.object.cntr[1]

Open(w_pics_retrieve_msg_box)
rtn = dw_qa_qastg.Retrieve(Lbkseq,Lcntr)
close(w_pics_retrieve_msg_box)
dw_qa_qastg.enabled = FALSE

if rtn = 0 THEN
	MessageBox("ERROR","QA information has not been entered for Book number: "+string(Lbkseq)+" and contract number: "+Lcntr)
	close(parent)	
end if

end event


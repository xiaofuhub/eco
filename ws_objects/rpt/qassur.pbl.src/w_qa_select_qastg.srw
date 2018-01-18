$PBExportHeader$w_qa_select_qastg.srw
forward
global type w_qa_select_qastg from w_response
end type
type dw_qa_qastg from u_pics_dw within w_qa_select_qastg
end type
type st_1 from statictext within w_qa_select_qastg
end type
type cb_cancel from commandbutton within w_qa_select_qastg
end type
type cb_ok from commandbutton within w_qa_select_qastg
end type
end forward

global type w_qa_select_qastg from w_response
integer x = 905
integer y = 620
integer width = 818
integer height = 824
string title = "Select Qastg"
boolean controlmenu = false
dw_qa_qastg dw_qa_qastg
st_1 st_1
cb_cancel cb_cancel
cb_ok cb_ok
end type
global w_qa_select_qastg w_qa_select_qastg

type variables

end variables

on w_qa_select_qastg.create
int iCurrent
call super::create
this.dw_qa_qastg=create dw_qa_qastg
this.st_1=create st_1
this.cb_cancel=create cb_cancel
this.cb_ok=create cb_ok
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_qa_qastg
this.Control[iCurrent+2]=this.st_1
this.Control[iCurrent+3]=this.cb_cancel
this.Control[iCurrent+4]=this.cb_ok
end on

on w_qa_select_qastg.destroy
call super::destroy
destroy(this.dw_qa_qastg)
destroy(this.st_1)
destroy(this.cb_cancel)
destroy(this.cb_ok)
end on

event open;call super::open;long lbkseq
lbkseq = Message.DoubleParm
dw_qa_qastg.Retrieve(lbkseq)

end event

type dw_qa_qastg from u_pics_dw within w_qa_select_qastg
integer x = 233
integer y = 124
integer width = 219
integer height = 460
integer taborder = 10
string dataobject = "dddw_qa_qastg"
end type

event constructor;call super::constructor;SetTransObject(SqlServerTrans)


end event

event doubleclicked;call super::doubleclicked;str_qa_comments lstr_qa_comments

lstr_qa_comments.qastg = dw_qa_qastg.object.qastg[dw_qa_qastg.Getrow()] 
lstr_qa_comments.qacompdt = dw_qa_qastg.object.qacompdt[dw_qa_qastg.Getrow()] 
lstr_qa_comments.qainit = dw_qa_qastg.object.qainit[dw_qa_qastg.Getrow()] 

CloseWithReturn(Parent,lstr_qa_comments)

end event

type st_1 from statictext within w_qa_select_qastg
integer x = 27
integer y = 24
integer width = 736
integer height = 76
boolean bringtotop = true
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean underline = true
long backcolor = 78164112
boolean enabled = false
string text = "Double Click or Click OK"
boolean focusrectangle = false
end type

type cb_cancel from commandbutton within w_qa_select_qastg
integer x = 411
integer y = 620
integer width = 238
integer height = 80
integer taborder = 20
boolean bringtotop = true
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "&Cancel"
end type

event clicked;str_qa_comments lstr_qa_comments
CloseWithReturn(Parent,lstr_qa_comments)
end event

type cb_ok from commandbutton within w_qa_select_qastg
integer x = 114
integer y = 620
integer width = 210
integer height = 80
integer taborder = 3
boolean bringtotop = true
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "&Ok"
boolean default = true
end type

event clicked;str_qa_comments lstr_qa_comments

lstr_qa_comments.qastg = dw_qa_qastg.object.qastg[dw_qa_qastg.Getrow()] 
lstr_qa_comments.qacompdt = dw_qa_qastg.object.qacompdt[dw_qa_qastg.Getrow()] 
lstr_qa_comments.qainit = dw_qa_qastg.object.qainit[dw_qa_qastg.Getrow()] 

CloseWithReturn(Parent,lstr_qa_comments)

end event


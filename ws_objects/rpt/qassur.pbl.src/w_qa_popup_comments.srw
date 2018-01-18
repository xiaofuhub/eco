$PBExportHeader$w_qa_popup_comments.srw
forward
global type w_qa_popup_comments from w_popup
end type
type mle_comments from multilineedit within w_qa_popup_comments
end type
type cb_ok from commandbutton within w_qa_popup_comments
end type
type cb_cancel from commandbutton within w_qa_popup_comments
end type
end forward

global type w_qa_popup_comments from w_popup
integer x = 214
integer y = 221
integer width = 2725
integer height = 1380
string title = "QA Comments"
boolean controlmenu = false
boolean minbox = false
boolean maxbox = false
boolean resizable = false
windowtype windowtype = response!
mle_comments mle_comments
cb_ok cb_ok
cb_cancel cb_cancel
end type
global w_qa_popup_comments w_qa_popup_comments

type variables
string old_comments
end variables

on w_qa_popup_comments.create
int iCurrent
call super::create
this.mle_comments=create mle_comments
this.cb_ok=create cb_ok
this.cb_cancel=create cb_cancel
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.mle_comments
this.Control[iCurrent+2]=this.cb_ok
this.Control[iCurrent+3]=this.cb_cancel
end on

on w_qa_popup_comments.destroy
call super::destroy
destroy(this.mle_comments)
destroy(this.cb_ok)
destroy(this.cb_cancel)
end on

event open;call super::open;mle_comments.Text = Message.StringParm
old_comments = Message.StringParm
end event

type mle_comments from multilineedit within w_qa_popup_comments
integer x = 37
integer y = 32
integer width = 2633
integer height = 1056
integer taborder = 10
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
boolean autovscroll = true
borderstyle borderstyle = styleraised!
end type

type cb_ok from commandbutton within w_qa_popup_comments
integer x = 1938
integer y = 1152
integer width = 311
integer height = 100
boolean bringtotop = true
integer textsize = -10
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "&OK"
end type

event clicked;string comment_str,Lstr

comment_str = mle_comments.text

CloseWithReturn(w_qa_popup_comments,comment_str)

end event

type cb_cancel from commandbutton within w_qa_popup_comments
integer x = 2341
integer y = 1152
integer width = 306
integer height = 100
boolean bringtotop = true
integer textsize = -10
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "&Cancel"
end type

event clicked;CloseWithReturn(w_qa_popup_comments,old_comments)

end event


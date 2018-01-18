$PBExportHeader$w_pics_upgrade_message.srw
forward
global type w_pics_upgrade_message from w_response
end type
type mle_pics_msg from multilineedit within w_pics_upgrade_message
end type
type cb_cancel from commandbutton within w_pics_upgrade_message
end type
type cb_ok from commandbutton within w_pics_upgrade_message
end type
end forward

global type w_pics_upgrade_message from w_response
integer x = 214
integer y = 221
integer width = 2743
integer height = 1724
string title = "PICS Release message"
mle_pics_msg mle_pics_msg
cb_cancel cb_cancel
cb_ok cb_ok
end type
global w_pics_upgrade_message w_pics_upgrade_message

type variables
string pics_msg
end variables

on w_pics_upgrade_message.create
int iCurrent
call super::create
this.mle_pics_msg=create mle_pics_msg
this.cb_cancel=create cb_cancel
this.cb_ok=create cb_ok
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.mle_pics_msg
this.Control[iCurrent+2]=this.cb_cancel
this.Control[iCurrent+3]=this.cb_ok
end on

on w_pics_upgrade_message.destroy
call super::destroy
destroy(this.mle_pics_msg)
destroy(this.cb_cancel)
destroy(this.cb_ok)
end on

event pfc_postopen;call super::pfc_postopen;mle_pics_msg.text = Message.StringParm
end event

type mle_pics_msg from multilineedit within w_pics_upgrade_message
integer x = 27
integer y = 36
integer width = 2642
integer height = 1392
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
boolean hscrollbar = true
boolean vscrollbar = true
borderstyle borderstyle = stylelowered!
end type

type cb_cancel from commandbutton within w_pics_upgrade_message
integer x = 192
integer y = 1476
integer width = 1006
integer height = 112
integer taborder = 10
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&Cancel and ignore synchronization"
end type

event clicked;CloseWithReturn(Parent, 'No')
end event

type cb_ok from commandbutton within w_pics_upgrade_message
integer x = 1518
integer y = 1476
integer width = 1038
integer height = 112
integer taborder = 20
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&OK and synchronize to new release"
end type

event clicked;CloseWithReturn(Parent, 'Yes')
end event


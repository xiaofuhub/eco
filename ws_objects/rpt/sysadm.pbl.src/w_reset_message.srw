$PBExportHeader$w_reset_message.srw
forward
global type w_reset_message from w_response
end type
type cb_1 from commandbutton within w_reset_message
end type
type st_3 from statictext within w_reset_message
end type
type st_2 from statictext within w_reset_message
end type
type st_1 from statictext within w_reset_message
end type
end forward

global type w_reset_message from w_response
integer x = 1500
integer y = 1500
integer width = 1856
integer height = 696
string title = "Password Reset"
boolean controlmenu = false
boolean center = true
boolean ib_isupdateable = false
cb_1 cb_1
st_3 st_3
st_2 st_2
st_1 st_1
end type
global w_reset_message w_reset_message

on w_reset_message.create
int iCurrent
call super::create
this.cb_1=create cb_1
this.st_3=create st_3
this.st_2=create st_2
this.st_1=create st_1
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.cb_1
this.Control[iCurrent+2]=this.st_3
this.Control[iCurrent+3]=this.st_2
this.Control[iCurrent+4]=this.st_1
end on

on w_reset_message.destroy
call super::destroy
destroy(this.cb_1)
destroy(this.st_3)
destroy(this.st_2)
destroy(this.st_1)
end on

event open;call super::open;st_1.text = Message.stringparm
end event

type cb_1 from commandbutton within w_reset_message
integer x = 754
integer y = 436
integer width = 320
integer height = 100
integer taborder = 10
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "OK"
boolean default = true
end type

event clicked;close(parent)
end event

type st_3 from statictext within w_reset_message
integer x = 238
integer y = 280
integer width = 1202
integer height = 64
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "Click ~'Update~' button to Save and Inform User"
boolean focusrectangle = false
end type

type st_2 from statictext within w_reset_message
integer x = 238
integer y = 96
integer width = 571
integer height = 64
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "Password Reset to : "
boolean focusrectangle = false
end type

type st_1 from statictext within w_reset_message
integer x = 823
integer y = 92
integer width = 722
integer height = 148
integer textsize = -12
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "none"
boolean focusrectangle = false
end type


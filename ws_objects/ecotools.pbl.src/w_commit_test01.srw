$PBExportHeader$w_commit_test01.srw
forward
global type w_commit_test01 from window
end type
type cb_1 from commandbutton within w_commit_test01
end type
end forward

global type w_commit_test01 from window
integer width = 3959
integer height = 1648
boolean titlebar = true
string title = "Untitled"
boolean controlmenu = true
boolean minbox = true
boolean maxbox = true
boolean resizable = true
long backcolor = 67108864
string icon = "AppIcon!"
boolean center = true
cb_1 cb_1
end type
global w_commit_test01 w_commit_test01

on w_commit_test01.create
this.cb_1=create cb_1
this.Control[]={this.cb_1}
end on

on w_commit_test01.destroy
destroy(this.cb_1)
end on

event open;//09909
end event

type cb_1 from commandbutton within w_commit_test01
integer x = 1449
integer y = 536
integer width = 402
integer height = 112
integer taborder = 10
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "none"
end type


$PBExportHeader$w_group_access_hidden.srw
forward
global type w_group_access_hidden from Window
end type
type cb_1 from commandbutton within w_group_access_hidden
end type
type dw_group_access_hidden from datawindow within w_group_access_hidden
end type
end forward

global type w_group_access_hidden from Window
int X=833
int Y=361
int Width=2922
int Height=1209
boolean Visible=false
boolean TitleBar=true
string Title="Group Access Hidden"
boolean ControlMenu=true
boolean MinBox=true
boolean MaxBox=true
boolean Resizable=true
cb_1 cb_1
dw_group_access_hidden dw_group_access_hidden
end type
global w_group_access_hidden w_group_access_hidden

on w_group_access_hidden.create
this.cb_1=create cb_1
this.dw_group_access_hidden=create dw_group_access_hidden
this.Control[]={ this.cb_1,&
this.dw_group_access_hidden}
end on

on w_group_access_hidden.destroy
destroy(this.cb_1)
destroy(this.dw_group_access_hidden)
end on

type cb_1 from commandbutton within w_group_access_hidden
int X=229
int Y=941
int Width=247
int Height=109
int TabOrder=2
string Text="&Close"
int TextSize=-8
int Weight=400
string FaceName="MS Sans Serif"
FontFamily FontFamily=Swiss!
FontPitch FontPitch=Variable!
end type

event clicked;close(parent)
end event

type dw_group_access_hidden from datawindow within w_group_access_hidden
int X=508
int Y=205
int Width=2177
int Height=577
int TabOrder=1
string DataObject="d_group_access_hidden"
boolean HScrollBar=true
boolean VScrollBar=true
boolean LiveScroll=true
end type

event constructor;string ls_group

settransobject(sqlservertrans)

// This is window will retrieve...



ls_group = Message.StringParm

dw_group_access_hidden.retrieve(ls_group)
end event


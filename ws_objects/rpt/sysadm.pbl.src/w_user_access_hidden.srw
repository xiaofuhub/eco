$PBExportHeader$w_user_access_hidden.srw
forward
global type w_user_access_hidden from Window
end type
type dw_user_access_hidden from datawindow within w_user_access_hidden
end type
end forward

global type w_user_access_hidden from Window
int X=307
int Y=489
int Width=3050
int Height=1425
boolean Visible=false
boolean TitleBar=true
string Title="w_user_access_hidden"
long BackColor=79741120
boolean ControlMenu=true
boolean MinBox=true
boolean MaxBox=true
boolean Resizable=true
dw_user_access_hidden dw_user_access_hidden
end type
global w_user_access_hidden w_user_access_hidden

on w_user_access_hidden.create
this.dw_user_access_hidden=create dw_user_access_hidden
this.Control[]={ this.dw_user_access_hidden}
end on

on w_user_access_hidden.destroy
destroy(this.dw_user_access_hidden)
end on

type dw_user_access_hidden from datawindow within w_user_access_hidden
int X=110
int Y=241
int Width=2561
int Height=817
int TabOrder=1
string DataObject="d_user_access_hidden"
boolean VScrollBar=true
boolean LiveScroll=true
end type

event constructor;string ls_userid

settransobject(sqlservertrans)

ls_userid = message.stringparm

dw_user_access_hidden.retrieve(ls_userid)



end event


$PBExportHeader$w_pics_logon.srw
$PBExportComments$Extension Logon window
forward
global type w_pics_logon from pfc_w_logon
end type
type st_pics from statictext within w_pics_logon
end type
type st_version from statictext within w_pics_logon
end type
end forward

global type w_pics_logon from pfc_w_logon
integer x = 402
integer y = 20
integer width = 2057
integer height = 1724
boolean titlebar = false
string title = ""
boolean controlmenu = false
st_pics st_pics
st_version st_version
end type
global w_pics_logon w_pics_logon

on w_pics_logon.create
int iCurrent
call super::create
this.st_pics=create st_pics
this.st_version=create st_version
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_pics
this.Control[iCurrent+2]=this.st_version
end on

on w_pics_logon.destroy
call super::destroy
destroy(this.st_pics)
destroy(this.st_version)
end on

event open;call super::open;sle_userid.SetFocus()
//ddlb_env.text = "RS20 Server"
this.of_SetBase(TRUE)
this.inv_base.of_Center()
st_version.text = gnv_app.of_GetVersion()
end event

type p_logo from pfc_w_logon`p_logo within w_pics_logon
integer y = 344
integer width = 1947
integer height = 480
string picturename = "nlsbphh.bmp"
end type

type st_help from pfc_w_logon`st_help within w_pics_logon
integer x = 27
integer y = 160
integer width = 1989
integer height = 112
integer textsize = -10
string text = "Enter a User ID and Password to log onto "
alignment alignment = center!
end type

type cb_ok from pfc_w_logon`cb_ok within w_pics_logon
integer x = 549
integer y = 1504
integer height = 132
integer taborder = 40
integer textsize = -10
string text = "&OK"
end type

type cb_cancel from pfc_w_logon`cb_cancel within w_pics_logon
integer x = 1061
integer y = 1500
integer height = 132
integer taborder = 60
integer textsize = -10
string text = "&Cancel"
end type

type sle_userid from pfc_w_logon`sle_userid within w_pics_logon
string tag = "user name"
integer x = 905
integer y = 912
integer width = 398
integer height = 92
integer textsize = -10
end type

type sle_password from pfc_w_logon`sle_password within w_pics_logon
string tag = "password"
integer x = 905
integer y = 1036
integer width = 398
integer height = 92
integer textsize = -10
end type

type st_2 from pfc_w_logon`st_2 within w_pics_logon
integer x = 521
integer y = 932
integer width = 334
integer textsize = -10
end type

type st_3 from pfc_w_logon`st_3 within w_pics_logon
integer x = 544
integer y = 1032
integer width = 311
integer textsize = -10
end type

type st_pics from statictext within w_pics_logon
integer x = 32
integer y = 32
integer width = 1984
integer height = 104
integer textsize = -18
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 16711680
long backcolor = 79741120
boolean enabled = false
string text = "Welcome  to NLS"
alignment alignment = center!
boolean focusrectangle = false
end type

type st_version from statictext within w_pics_logon
integer x = 667
integer y = 1236
integer width = 521
integer height = 76
boolean bringtotop = true
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 33554432
long backcolor = 79741120
alignment alignment = center!
boolean focusrectangle = false
end type


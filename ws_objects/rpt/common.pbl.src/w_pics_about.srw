$PBExportHeader$w_pics_about.srw
$PBExportComments$Extension About window
forward
global type w_pics_about from pfc_w_about
end type
type st_active_window from statictext within w_pics_about
end type
end forward

global type w_pics_about from pfc_w_about
integer x = 795
integer width = 2208
integer height = 1232
st_active_window st_active_window
end type
global w_pics_about w_pics_about

on w_pics_about.create
int iCurrent
call super::create
this.st_active_window=create st_active_window
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_active_window
end on

on w_pics_about.destroy
call super::destroy
destroy(this.st_active_window)
end on

event pfc_preopen;call super::pfc_preopen;st_active_window.text = ls_current_active_window
end event

type p_about from pfc_w_about`p_about within w_pics_about
end type

type st_application from pfc_w_about`st_application within w_pics_about
integer x = 41
integer y = 756
end type

type st_version from pfc_w_about`st_version within w_pics_about
integer x = 41
integer y = 840
end type

type cb_ok from pfc_w_about`cb_ok within w_pics_about
integer x = 1815
integer y = 1028
end type

type st_copyright from pfc_w_about`st_copyright within w_pics_about
integer x = 41
integer y = 1048
integer height = 68
end type

type st_active_window from statictext within w_pics_about
integer x = 41
integer y = 948
integer width = 1216
integer height = 56
boolean bringtotop = true
integer textsize = -8
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 33554432
long backcolor = 79741120
boolean enabled = false
boolean focusrectangle = false
end type


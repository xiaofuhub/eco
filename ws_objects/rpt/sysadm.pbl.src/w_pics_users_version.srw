$PBExportHeader$w_pics_users_version.srw
forward
global type w_pics_users_version from w_sheet
end type
type st_1 from statictext within w_pics_users_version
end type
type dw_users_pics_version from u_pics_dw within w_pics_users_version
end type
type cb_exit from u_cb within w_pics_users_version
end type
end forward

global type w_pics_users_version from w_sheet
integer x = 283
integer y = 328
integer width = 1970
integer height = 1840
string title = "PICS Users and their version "
st_1 st_1
dw_users_pics_version dw_users_pics_version
cb_exit cb_exit
end type
global w_pics_users_version w_pics_users_version

on w_pics_users_version.create
int iCurrent
call super::create
this.st_1=create st_1
this.dw_users_pics_version=create dw_users_pics_version
this.cb_exit=create cb_exit
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_1
this.Control[iCurrent+2]=this.dw_users_pics_version
this.Control[iCurrent+3]=this.cb_exit
end on

on w_pics_users_version.destroy
call super::destroy
destroy(this.st_1)
destroy(this.dw_users_pics_version)
destroy(this.cb_exit)
end on

event resize;call super::resize;This.X = 302
This.Y = 105
This.Width = 2355
This.Height = 1249
end event

event pfc_postopen();call super::pfc_postopen;//Disable the addrow and delete row menu items
m_pics_main.m_edit.m_deleterow.Enabled = FALSE
m_pics_main.m_edit.m_addrow.enabled = FALSE

end event

event pfc_preopen();call super::pfc_preopen;this.of_SetPreference(TRUE)
this.inv_preference.of_SetToolBars(TRUE)
this.inv_preference.of_SetWindow(TRUE)
this.of_SetResize(TRUE)
this.inv_resize.of_SetOrigSize(this.workspacewidth(),this.workspaceheight())
inv_resize.of_Register(dw_users_pics_version, "scale")
inv_resize.of_Register(cb_exit, "scale")
inv_resize.of_Register(st_1, "scale")



end event

type st_1 from statictext within w_pics_users_version
integer x = 41
integer y = 1448
integer width = 1728
integer height = 88
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
boolean underline = true
long textcolor = 33554432
long backcolor = 67108864
string text = "Beta Testers in red display. They must be in latest PICS version"
boolean focusrectangle = false
end type

type dw_users_pics_version from u_pics_dw within w_pics_users_version
integer x = 37
integer y = 36
integer width = 1865
integer height = 1388
integer taborder = 10
string dataobject = "d_users_pics_version"
end type

event ue_postconstructor();call super::ue_postconstructor;this.of_settransobject(SqlServerTrans)
this.retrieve()
end event

type cb_exit from u_cb within w_pics_users_version
integer x = 1554
integer y = 1612
integer taborder = 0
integer textsize = -10
string text = "E&xit"
end type

event clicked;call super::clicked;Parent.Event pfc_close()

end event


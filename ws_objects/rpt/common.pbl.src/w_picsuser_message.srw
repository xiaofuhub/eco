$PBExportHeader$w_picsuser_message.srw
forward
global type w_picsuser_message from w_sheet
end type
type dw_picsuser_message from u_pics_dw within w_picsuser_message
end type
type cb_exit from commandbutton within w_picsuser_message
end type
end forward

global type w_picsuser_message from w_sheet
integer width = 2491
integer height = 1508
windowstate windowstate = maximized!
dw_picsuser_message dw_picsuser_message
cb_exit cb_exit
end type
global w_picsuser_message w_picsuser_message

on w_picsuser_message.create
int iCurrent
call super::create
this.dw_picsuser_message=create dw_picsuser_message
this.cb_exit=create cb_exit
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_picsuser_message
this.Control[iCurrent+2]=this.cb_exit
end on

on w_picsuser_message.destroy
call super::destroy
destroy(this.dw_picsuser_message)
destroy(this.cb_exit)
end on

event resize;call super::resize;long ll_height

This.X = w_pics_main.X
This.Y = w_pics_main.Y
ll_height = w_pics_main.mdi_1.Height
This.Resize(w_pics_main.WorkSpaceWidth(), ll_height)


end event

event pfc_preopen;call super::pfc_preopen;this.of_SetPreference(TRUE)
this.inv_preference.of_SetToolBars(TRUE)
this.inv_preference.of_SetWindow(TRUE)
this.of_SetResize(TRUE)
this.inv_resize.of_SetOrigSize(this.workspacewidth(),this.workspaceheight())
inv_resize.of_Register(dw_picsuser_message, "Scale")
inv_resize.of_Register(cb_exit, "Scale")

end event

type dw_picsuser_message from u_pics_dw within w_picsuser_message
integer x = 32
integer y = 28
integer width = 2386
integer height = 1172
integer taborder = 10
string dataobject = "d_picsuser_message"
boolean hscrollbar = true
end type

event constructor;call super::constructor;this.of_SetTransObject(SQLServerTrans)
this.retrieve()

end event

type cb_exit from commandbutton within w_picsuser_message
integer x = 2021
integer y = 1232
integer width = 402
integer height = 112
integer taborder = 10
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "E&xit"
end type

event clicked;close(parent)
end event


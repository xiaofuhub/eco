$PBExportHeader$w_maintain_marcfiles.srw
forward
global type w_maintain_marcfiles from w_sheet
end type
type st_1 from statictext within w_maintain_marcfiles
end type
type cb_clear from u_cb within w_maintain_marcfiles
end type
type cb_exit from u_cb within w_maintain_marcfiles
end type
type cb_update from u_cb within w_maintain_marcfiles
end type
type dw_maintain_marcfiles from u_dw within w_maintain_marcfiles
end type
end forward

global type w_maintain_marcfiles from w_sheet
integer x = 105
integer y = 460
integer width = 3255
integer height = 1644
string title = "Deleting MARC Records"
st_1 st_1
cb_clear cb_clear
cb_exit cb_exit
cb_update cb_update
dw_maintain_marcfiles dw_maintain_marcfiles
end type
global w_maintain_marcfiles w_maintain_marcfiles

type variables
string is_libcd,ls_libcds
datastore ids1,ids2
end variables

on w_maintain_marcfiles.create
int iCurrent
call super::create
this.st_1=create st_1
this.cb_clear=create cb_clear
this.cb_exit=create cb_exit
this.cb_update=create cb_update
this.dw_maintain_marcfiles=create dw_maintain_marcfiles
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_1
this.Control[iCurrent+2]=this.cb_clear
this.Control[iCurrent+3]=this.cb_exit
this.Control[iCurrent+4]=this.cb_update
this.Control[iCurrent+5]=this.dw_maintain_marcfiles
end on

on w_maintain_marcfiles.destroy
call super::destroy
destroy(this.st_1)
destroy(this.cb_clear)
destroy(this.cb_exit)
destroy(this.cb_update)
destroy(this.dw_maintain_marcfiles)
end on

event close;call super::close;//close(parent)
end event

event pfc_preopen;call super::pfc_preopen;this.of_SetPreference(TRUE)
this.inv_preference.of_SetToolBars(TRUE)
this.inv_preference.of_SetWindow(TRUE)

end event

type st_1 from statictext within w_maintain_marcfiles
integer x = 73
integer y = 1248
integer width = 3035
integer height = 64
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 33554432
long backcolor = 67108864
string text = "Right Click on the row and select delete to remove the row from the screen and then press the update button."
boolean focusrectangle = false
end type

type cb_clear from u_cb within w_maintain_marcfiles
event pfc_hinttext pbm_mousemove
string tag = "Clear the records"
integer x = 2290
integer y = 1380
integer width = 393
integer taborder = 0
integer textsize = -10
string text = "&Clear"
end type

event pfc_hinttext;call super::pfc_hinttext;w_pics_main.setmicrohelp(This.tag)
end event

event clicked;call super::clicked;dw_maintain_marcfiles.Reset()
dw_maintain_marcfiles.SetTransObject(sqlserveroracletrans)	
dw_maintain_marcfiles.Retrieve()

end event

type cb_exit from u_cb within w_maintain_marcfiles
event pfc_hinttext pbm_mousemove
string tag = "Exits the current screen"
integer x = 2779
integer y = 1380
integer width = 402
integer taborder = 0
integer textsize = -10
string text = "E&xit"
end type

event clicked;call super::clicked;parent.Event pfc_close()

end event

type cb_update from u_cb within w_maintain_marcfiles
event pfc_hinttext pbm_mousemove
string tag = "Update the MARC Record"
integer x = 1829
integer y = 1376
integer width = 393
integer taborder = 0
integer textsize = -10
string text = "&Update"
end type

event pfc_hinttext;call super::pfc_hinttext;w_pics_main.setmicrohelp(This.tag)
end event

event clicked;call super::clicked;Integer li_save

dw_maintain_marcfiles.AcceptText()

li_save = dw_maintain_marcfiles.Update()
IF li_save = 1 THEN
	COMMIT USING sqlserveroracletrans;
	Messagebox("Delete","MARC records successfully deleted. ~n ***NOTE*** Please make sure the files are delete from /pics/prd/htdocs/download/marcfile.")
	cb_clear.TriggerEvent(clicked!)
	RETURN
ELSE
	ROLLBACK USING sqlserveroracletrans;
	MessageBox("ERROR", "Error in deleting the records")
	RETURN
END IF

end event

type dw_maintain_marcfiles from u_dw within w_maintain_marcfiles
event pfc_hinttext pbm_mousemove
event ue_enterkey pbm_dwnprocessenter
integer x = 5
integer y = 20
integer width = 3182
integer height = 1216
integer taborder = 10
boolean bringtotop = true
string dataobject = "d_maintain_marcfiles"
boolean livescroll = false
end type

event constructor;call super::constructor;this.of_SetTransObject( sqlserveroracletrans )
this.retrieve()
this.Setfocus()






end event

event getfocus;call super::getfocus;m_pics_main.m_edit.m_deleterow.Enabled =	FALSE
m_pics_main.m_edit.m_addrow.Enabled =	FALSE
m_pics_main.m_edit.m_cut.Enabled =	FALSE
end event


$PBExportHeader$w_bkget_template.srw
forward
global type w_bkget_template from w_response
end type
type sle_temp from uo_pics_sle within w_bkget_template
end type
type st_tmptop from statichyperlink within w_bkget_template
end type
type cb_ok from u_cb within w_bkget_template
end type
type cb_cancel from u_cb within w_bkget_template
end type
type st_tmpleft from u_st within w_bkget_template
end type
end forward

global type w_bkget_template from w_response
integer x = 878
integer y = 580
integer width = 1033
integer height = 712
string title = "get book template"
sle_temp sle_temp
st_tmptop st_tmptop
cb_ok cb_ok
cb_cancel cb_cancel
st_tmpleft st_tmpleft
end type
global w_bkget_template w_bkget_template

type variables
boolean ib_element=false
end variables

on w_bkget_template.create
int iCurrent
call super::create
this.sle_temp=create sle_temp
this.st_tmptop=create st_tmptop
this.cb_ok=create cb_ok
this.cb_cancel=create cb_cancel
this.st_tmpleft=create st_tmpleft
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.sle_temp
this.Control[iCurrent+2]=this.st_tmptop
this.Control[iCurrent+3]=this.cb_ok
this.Control[iCurrent+4]=this.cb_cancel
this.Control[iCurrent+5]=this.st_tmpleft
end on

on w_bkget_template.destroy
call super::destroy
destroy(this.sle_temp)
destroy(this.st_tmptop)
destroy(this.cb_ok)
destroy(this.cb_cancel)
destroy(this.st_tmpleft)
end on

event key;call super::key;IF KeyDown(KeyEnter!) THEN 
  cb_ok.TriggerEvent(Clicked!)
ELSEIF KeyDown(KeyEscape!) THEN 
  cb_cancel.TriggerEvent(Clicked!)
END IF


end event

event open;call super::open;string ls_elmt

sle_temp.SetFocus()
ls_elmt=Message.stringParm
if ls_elmt='add' then
	st_tmpleft.text='Element:'
	st_tmptop.text='Enter Element:'
	this.title='Enter Element'
	ib_element=true
end if
end event

type sle_temp from uo_pics_sle within w_bkget_template
integer x = 329
integer y = 236
integer width = 622
integer height = 76
integer taborder = 10
end type

type st_tmptop from statichyperlink within w_bkget_template
integer x = 329
integer y = 68
integer width = 663
integer height = 104
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string pointer = "HyperLink!"
long backcolor = 80263581
string text = "Enter New Template"
boolean focusrectangle = false
end type

type cb_ok from u_cb within w_bkget_template
integer x = 151
integer y = 440
integer width = 288
integer height = 84
integer taborder = 20
integer textsize = -10
string text = "&OK"
end type

event clicked;call super::clicked;string ls_temp
	
IF ISNULL(trim(sle_temp.text)) OR trim(sle_temp.text) = ""  Then
	if ib_element=false then
		MessageBox("Error","Invalid Template.")
	elseif ib_element=true then
		MessageBox("Error","Invalid Element.")
	end if
	sle_temp.SetFocus()
	Return
ELSE
	ls_temp = trim(sle_temp.text)
	CloseWithReturn(w_bkget_template,ls_temp)
END IF



end event

type cb_cancel from u_cb within w_bkget_template
integer x = 645
integer y = 440
integer width = 288
integer height = 84
integer taborder = 30
integer textsize = -10
string text = "&Cancel"
end type

event clicked;call super::clicked;string ls_temp='Cancel'

CloseWithReturn(w_bkget_template,ls_temp)
end event

type st_tmpleft from u_st within w_bkget_template
integer x = 9
integer y = 248
integer width = 338
integer height = 68
integer textsize = -10
string text = " Template:"
end type


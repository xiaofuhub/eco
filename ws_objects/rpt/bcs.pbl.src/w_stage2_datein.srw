$PBExportHeader$w_stage2_datein.srw
forward
global type w_stage2_datein from window
end type
type st_4 from statictext within w_stage2_datein
end type
type st_3 from statictext within w_stage2_datein
end type
type em_s2infrom from uo_date within w_stage2_datein
end type
type em_s2into from uo_date within w_stage2_datein
end type
type cb_cancel from commandbutton within w_stage2_datein
end type
type cb_search from commandbutton within w_stage2_datein
end type
type cb_ok from commandbutton within w_stage2_datein
end type
type st_2 from statictext within w_stage2_datein
end type
type st_1 from statictext within w_stage2_datein
end type
end forward

global type w_stage2_datein from window
integer x = 562
integer y = 360
integer width = 1751
integer height = 832
boolean titlebar = true
string title = "Stage II Date in"
windowtype windowtype = response!
long backcolor = 79741120
st_4 st_4
st_3 st_3
em_s2infrom em_s2infrom
em_s2into em_s2into
cb_cancel cb_cancel
cb_search cb_search
cb_ok cb_ok
st_2 st_2
st_1 st_1
end type
global w_stage2_datein w_stage2_datein

type variables
string S2dateinto,S2dateinfrom
end variables

on w_stage2_datein.create
this.st_4=create st_4
this.st_3=create st_3
this.em_s2infrom=create em_s2infrom
this.em_s2into=create em_s2into
this.cb_cancel=create cb_cancel
this.cb_search=create cb_search
this.cb_ok=create cb_ok
this.st_2=create st_2
this.st_1=create st_1
this.Control[]={this.st_4,&
this.st_3,&
this.em_s2infrom,&
this.em_s2into,&
this.cb_cancel,&
this.cb_search,&
this.cb_ok,&
this.st_2,&
this.st_1}
end on

on w_stage2_datein.destroy
destroy(this.st_4)
destroy(this.st_3)
destroy(this.em_s2infrom)
destroy(this.em_s2into)
destroy(this.cb_cancel)
destroy(this.cb_search)
destroy(this.cb_ok)
destroy(this.st_2)
destroy(this.st_1)
end on

event activate;em_s2infrom.SetFocus()
end event

type st_4 from statictext within w_stage2_datein
integer x = 901
integer y = 360
integer width = 123
integer height = 76
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long backcolor = 67108864
boolean enabled = false
string text = "To:"
boolean focusrectangle = false
end type

type st_3 from statictext within w_stage2_datein
integer x = 233
integer y = 360
integer width = 192
integer height = 76
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long backcolor = 67108864
boolean enabled = false
string text = "From:"
boolean focusrectangle = false
end type

type em_s2infrom from uo_date within w_stage2_datein
integer x = 439
integer y = 344
integer width = 347
integer height = 104
integer taborder = 10
boolean autoskip = true
end type

type em_s2into from uo_date within w_stage2_datein
integer x = 1024
integer y = 344
integer width = 347
integer height = 108
integer taborder = 20
boolean autoskip = true
end type

type cb_cancel from commandbutton within w_stage2_datein
integer x = 1106
integer y = 560
integer width = 357
integer height = 108
integer taborder = 50
integer textsize = -10
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "&Cancel"
end type

event clicked;close(parent)
end event

type cb_search from commandbutton within w_stage2_datein
integer x = 667
integer y = 560
integer width = 357
integer height = 108
integer taborder = 40
integer textsize = -10
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "&Search..."
end type

event clicked;SetMicroHelp(w_pics_main,"Please wait...")
close(parent)
open(w_s2datein)
end event

type cb_ok from commandbutton within w_stage2_datein
integer x = 224
integer y = 560
integer width = 357
integer height = 108
integer taborder = 30
integer textsize = -10
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "&OK"
boolean default = true
end type

event clicked;IF em_s2into.text <> "" AND em_s2infrom.text <> "" THEN
	S2dateinfrom = em_s2infrom.text
	S2dateinto = em_s2into.text
ELSEIF em_s2into.text = "" AND em_s2infrom.text <> "" THEN
	SetNull(S2dateinto)
	S2dateinfrom = em_s2infrom.text
ELSEIF em_s2into.text <> "" AND em_s2infrom.text = "" THEN
	SetNull(S2dateinfrom)
	S2dateinto = em_s2into.text
ELSEIF em_s2into.text = "" AND em_s2infrom.text = "" THEN
	MessageBox("Error", "A date must be entered!")
	RETURN
END IF
IF IsValid(w_sheet_bcs_stage2) = FALSE THEN 
	OpenSheet(w_sheet_bcs_stage2, w_pics_main, 0, Original!)
ELSE
	w_sheet_bcs_stage2.wf_get_new_range()
END IF


end event

type st_2 from statictext within w_stage2_datein
integer x = 233
integer y = 204
integer width = 709
integer height = 72
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long backcolor = 67108864
boolean enabled = false
string text = "or enter specific date."
boolean focusrectangle = false
end type

type st_1 from statictext within w_stage2_datein
integer x = 229
integer y = 128
integer width = 1042
integer height = 80
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long backcolor = 79741120
boolean enabled = false
string text = "Please enter the range of dates, "
boolean focusrectangle = false
end type


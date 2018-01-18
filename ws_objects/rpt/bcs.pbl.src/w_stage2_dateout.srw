$PBExportHeader$w_stage2_dateout.srw
forward
global type w_stage2_dateout from window
end type
type st_5 from statictext within w_stage2_dateout
end type
type st_4 from statictext within w_stage2_dateout
end type
type em_s2outfrom from uo_date within w_stage2_dateout
end type
type em_catdate from uo_date within w_stage2_dateout
end type
type em_s2outto from uo_date within w_stage2_dateout
end type
type st_3 from statictext within w_stage2_dateout
end type
type cb_cancel from commandbutton within w_stage2_dateout
end type
type cb_search from commandbutton within w_stage2_dateout
end type
type cb_ok from commandbutton within w_stage2_dateout
end type
type st_2 from statictext within w_stage2_dateout
end type
type st_1 from statictext within w_stage2_dateout
end type
end forward

global type w_stage2_dateout from window
integer x = 539
integer y = 368
integer width = 1751
integer height = 944
boolean titlebar = true
string title = "Stage II Date out"
windowtype windowtype = response!
long backcolor = 79741120
st_5 st_5
st_4 st_4
em_s2outfrom em_s2outfrom
em_catdate em_catdate
em_s2outto em_s2outto
st_3 st_3
cb_cancel cb_cancel
cb_search cb_search
cb_ok cb_ok
st_2 st_2
st_1 st_1
end type
global w_stage2_dateout w_stage2_dateout

type variables
string S2dateoutfrom,S2dateoutto,Catdate
end variables

on w_stage2_dateout.create
this.st_5=create st_5
this.st_4=create st_4
this.em_s2outfrom=create em_s2outfrom
this.em_catdate=create em_catdate
this.em_s2outto=create em_s2outto
this.st_3=create st_3
this.cb_cancel=create cb_cancel
this.cb_search=create cb_search
this.cb_ok=create cb_ok
this.st_2=create st_2
this.st_1=create st_1
this.Control[]={this.st_5,&
this.st_4,&
this.em_s2outfrom,&
this.em_catdate,&
this.em_s2outto,&
this.st_3,&
this.cb_cancel,&
this.cb_search,&
this.cb_ok,&
this.st_2,&
this.st_1}
end on

on w_stage2_dateout.destroy
destroy(this.st_5)
destroy(this.st_4)
destroy(this.em_s2outfrom)
destroy(this.em_catdate)
destroy(this.em_s2outto)
destroy(this.st_3)
destroy(this.cb_cancel)
destroy(this.cb_search)
destroy(this.cb_ok)
destroy(this.st_2)
destroy(this.st_1)
end on

type st_5 from statictext within w_stage2_dateout
integer x = 887
integer y = 336
integer width = 133
integer height = 76
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long backcolor = 67108864
boolean enabled = false
string text = "TO:"
boolean focusrectangle = false
end type

type st_4 from statictext within w_stage2_dateout
integer x = 165
integer y = 336
integer width = 219
integer height = 76
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long backcolor = 67108864
boolean enabled = false
string text = "FROM:"
boolean focusrectangle = false
end type

type em_s2outfrom from uo_date within w_stage2_dateout
integer x = 421
integer y = 328
integer width = 347
integer height = 88
integer taborder = 10
boolean autoskip = true
string displaydata = "~t/"
end type

type em_catdate from uo_date within w_stage2_dateout
integer x = 695
integer y = 492
integer width = 347
integer height = 104
integer taborder = 30
boolean autoskip = true
string displaydata = "~t/"
end type

type em_s2outto from uo_date within w_stage2_dateout
integer x = 1042
integer y = 324
integer width = 347
integer height = 96
integer taborder = 20
boolean autoskip = true
string displaydata = "~t/"
end type

type st_3 from statictext within w_stage2_dateout
integer x = 229
integer y = 496
integer width = 457
integer height = 76
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long backcolor = 67108864
boolean enabled = false
string text = "Catalog Date:"
boolean focusrectangle = false
end type

type cb_cancel from commandbutton within w_stage2_dateout
integer x = 1134
integer y = 692
integer width = 357
integer height = 108
integer taborder = 60
integer textsize = -10
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "&Cancel"
end type

event clicked;close(parent)
end event

type cb_search from commandbutton within w_stage2_dateout
integer x = 695
integer y = 692
integer width = 357
integer height = 108
integer taborder = 50
integer textsize = -10
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "&Search..."
end type

event clicked;SetMicroHelp(w_pics_main,"Please wait...")
IF em_catdate.text <> "" then
	Catdate = em_catdate.text
ELSE
	Catdate = string(Today(),'MM/DD/YYYY')
end if
open(w_s2dateout)

end event

type cb_ok from commandbutton within w_stage2_dateout
integer x = 251
integer y = 692
integer width = 357
integer height = 108
integer taborder = 40
integer textsize = -10
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "&OK"
boolean default = true
end type

event clicked;IF em_s2outto.text <> "" AND em_s2outfrom.text <> "" THEN
	S2dateoutfrom = em_s2outfrom.text
	S2dateoutto = em_s2outto.text
ELSEIF em_s2outto.text = "" AND em_s2outfrom.text <> "" THEN
	SetNull(S2dateoutto)
	S2dateoutfrom = em_s2outfrom.text
ELSEIF em_s2outto.text <> "" AND em_s2outfrom.text = "" THEN
	SetNull(S2dateoutfrom)
	S2dateoutto = em_s2outto.text
ELSEIF em_s2outto.text = "" AND em_s2outfrom.text = "" THEN
	MessageBox("Error", "A date must be entered!")
	RETURN
END IF	
IF em_catdate.text <> "" then
	Catdate = em_catdate.text
	OpenSheet(w_sheet_catalog, w_pics_main, 0, Original!)
ELSE
	Catdate = string(Today(),'MM/DD/YYYY')
	OpenSheet(w_sheet_catalog, w_pics_main, 0, Original!)
END IF
end event

type st_2 from statictext within w_stage2_dateout
integer x = 155
integer y = 156
integer width = 1321
integer height = 72
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long backcolor = 67108864
boolean enabled = false
string text = "and enter a catalog date and click on OK."
boolean focusrectangle = false
end type

type st_1 from statictext within w_stage2_dateout
integer x = 128
integer y = 76
integer width = 1495
integer height = 80
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long backcolor = 67108864
boolean enabled = false
string text = "Please enter the range of the Stage II Date out,"
boolean focusrectangle = false
end type


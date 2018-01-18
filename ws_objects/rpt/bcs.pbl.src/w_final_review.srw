$PBExportHeader$w_final_review.srw
forward
global type w_final_review from Window
end type
type em_fr from uo_date within w_final_review
end type
type cb_cancel from commandbutton within w_final_review
end type
type cb_search from commandbutton within w_final_review
end type
type cb_ok from commandbutton within w_final_review
end type
type st_2 from statictext within w_final_review
end type
type st_1 from statictext within w_final_review
end type
end forward

global type w_final_review from Window
int X=540
int Y=369
int Width=1751
int Height=853
boolean TitleBar=true
string Title="Final Review Date"
long BackColor=79741120
WindowType WindowType=response!
em_fr em_fr
cb_cancel cb_cancel
cb_search cb_search
cb_ok cb_ok
st_2 st_2
st_1 st_1
end type
global w_final_review w_final_review

type variables
date Frdate
end variables

on w_final_review.create
this.em_fr=create em_fr
this.cb_cancel=create cb_cancel
this.cb_search=create cb_search
this.cb_ok=create cb_ok
this.st_2=create st_2
this.st_1=create st_1
this.Control[]={ this.em_fr,&
this.cb_cancel,&
this.cb_search,&
this.cb_ok,&
this.st_2,&
this.st_1}
end on

on w_final_review.destroy
destroy(this.em_fr)
destroy(this.cb_cancel)
destroy(this.cb_search)
destroy(this.cb_ok)
destroy(this.st_2)
destroy(this.st_1)
end on

event activate;em_fr.SetFocus()
end event

type em_fr from uo_date within w_final_review
event doubleclick pbm_dwnlbuttondblclk
int X=650
int Y=341
int Height=97
int TabOrder=10
string MinMax="01/01/1985~~01/01/2010"
end type

type cb_cancel from commandbutton within w_final_review
int X=1107
int Y=577
int Width=357
int Height=109
int TabOrder=40
string Text="&Cancel"
int TextSize=-10
int Weight=400
string FaceName="MS Sans Serif"
FontFamily FontFamily=Swiss!
FontPitch FontPitch=Variable!
end type

event clicked;close(parent)
end event

type cb_search from commandbutton within w_final_review
int X=668
int Y=577
int Width=357
int Height=109
int TabOrder=30
string Text="&Search..."
int TextSize=-10
int Weight=400
string FaceName="MS Sans Serif"
FontFamily FontFamily=Swiss!
FontPitch FontPitch=Variable!
end type

event clicked;SetMicroHelp(w_pics_main,"Please wait...")
close(parent)
open(w_frdate)
end event

type cb_ok from commandbutton within w_final_review
int X=229
int Y=577
int Width=357
int Height=109
int TabOrder=20
string Text="&OK"
boolean Default=true
int TextSize=-10
int Weight=400
string FaceName="MS Sans Serif"
FontFamily FontFamily=Swiss!
FontPitch FontPitch=Variable!
end type

event clicked;Frdate = date(em_fr.text)
OpenSheet(w_sheet_bcs_stage1, w_pics_main, 0, Original!)


end event

type st_2 from statictext within w_final_review
int X=229
int Y=205
int Width=654
int Height=73
boolean Enabled=false
string Text="and click on OK."
boolean FocusRectangle=false
long BackColor=79741120
int TextSize=-10
int Weight=700
string FaceName="MS Sans Serif"
FontFamily FontFamily=Swiss!
FontPitch FontPitch=Variable!
end type

type st_1 from statictext within w_final_review
int X=229
int Y=141
int Width=1207
int Height=81
boolean Enabled=false
string Text="Please enter the Final Review date"
boolean FocusRectangle=false
long BackColor=79741120
int TextSize=-10
int Weight=700
string FaceName="MS Sans Serif"
FontFamily FontFamily=Swiss!
FontPitch FontPitch=Variable!
end type


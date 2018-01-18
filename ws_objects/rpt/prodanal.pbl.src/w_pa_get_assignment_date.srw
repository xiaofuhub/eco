$PBExportHeader$w_pa_get_assignment_date.srw
forward
global type w_pa_get_assignment_date from w_response
end type
type cb_ok from u_cb within w_pa_get_assignment_date
end type
type em_assigndt from uo_date within w_pa_get_assignment_date
end type
type st_1 from statictext within w_pa_get_assignment_date
end type
type cb_cancel from u_cb within w_pa_get_assignment_date
end type
type cb_1 from commandbutton within w_pa_get_assignment_date
end type
end forward

global type w_pa_get_assignment_date from w_response
int X=613
int Y=349
int Width=1454
int Height=537
boolean TitleBar=true
string Title="Assignment Date"
cb_ok cb_ok
em_assigndt em_assigndt
st_1 st_1
cb_cancel cb_cancel
cb_1 cb_1
end type
global w_pa_get_assignment_date w_pa_get_assignment_date

type variables

end variables

on w_pa_get_assignment_date.create
int iCurrent
call w_response::create
this.cb_ok=create cb_ok
this.em_assigndt=create em_assigndt
this.st_1=create st_1
this.cb_cancel=create cb_cancel
this.cb_1=create cb_1
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=cb_ok
this.Control[iCurrent+2]=em_assigndt
this.Control[iCurrent+3]=st_1
this.Control[iCurrent+4]=cb_cancel
this.Control[iCurrent+5]=cb_1
end on

on w_pa_get_assignment_date.destroy
call w_response::destroy
destroy(this.cb_ok)
destroy(this.em_assigndt)
destroy(this.st_1)
destroy(this.cb_cancel)
destroy(this.cb_1)
end on

event open;call super::open;em_assigndt.SetFocus()
end event

type cb_ok from u_cb within w_pa_get_assignment_date
int X=65
int Y=297
int TabOrder=0
string Text="&OK"
boolean Default=true
end type

event clicked;call super::clicked;//w_pa_assigning_books.Lassigndt = date(em_assigndt.text)
IF IsNull(em_assigndt.text) OR em_assigndt.text="" THEN
	MessageBox("Assignment Date","Please enter a valid date.")
	SetFocus(em_assigndt)
ELSE
	CloseWithReturn(w_pa_get_assignment_date,em_assigndt.text)
END IF
end event

type em_assigndt from uo_date within w_pa_get_assignment_date
int X=513
int Y=149
int Width=362
int Height=101
int TabOrder=10
BorderStyle BorderStyle=StyleLowered!
string DisplayData=""
end type

type st_1 from statictext within w_pa_get_assignment_date
int X=60
int Y=33
int Width=1331
int Height=69
boolean Enabled=false
boolean BringToTop=true
string Text="Please enter the Assignment Date and press OK."
Alignment Alignment=Center!
boolean FocusRectangle=false
long BackColor=12632256
int TextSize=-8
int Weight=700
string FaceName="MS Sans Serif"
FontFamily FontFamily=Swiss!
FontPitch FontPitch=Variable!
end type

type cb_cancel from u_cb within w_pa_get_assignment_date
int X=979
int Y=297
int TabOrder=0
boolean BringToTop=true
string Text="&Cancel"
end type

event clicked;call super::clicked;Close(parent)
end event

type cb_1 from commandbutton within w_pa_get_assignment_date
int X=522
int Y=297
int Width=353
int Height=93
boolean BringToTop=true
string Text="&Search"
int TextSize=-8
int Weight=400
string FaceName="MS Sans Serif"
FontFamily FontFamily=Swiss!
FontPitch FontPitch=Variable!
end type

event clicked;SetMicroHelp(w_pics_main,"Please wait...")
close(parent)
open(w_assigndate)
end event


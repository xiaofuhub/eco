$PBExportHeader$w_pcs_gets_dates_cntr.srw
forward
global type w_pcs_gets_dates_cntr from w_response
end type
type cb_ok from u_cb within w_pcs_gets_dates_cntr
end type
type cb_cancel from u_cb within w_pcs_gets_dates_cntr
end type
type em_stdt from u_em within w_pcs_gets_dates_cntr
end type
type em_enddt from u_em within w_pcs_gets_dates_cntr
end type
type em_cntr from u_em within w_pcs_gets_dates_cntr
end type
type st_2 from statictext within w_pcs_gets_dates_cntr
end type
type st_3 from statictext within w_pcs_gets_dates_cntr
end type
type st_4 from statictext within w_pcs_gets_dates_cntr
end type
end forward

global type w_pcs_gets_dates_cntr from w_response
int X=997
int Y=501
int Width=970
int Height=649
boolean TitleBar=true
string Title="Average Deviation Report"
cb_ok cb_ok
cb_cancel cb_cancel
em_stdt em_stdt
em_enddt em_enddt
em_cntr em_cntr
st_2 st_2
st_3 st_3
st_4 st_4
end type
global w_pcs_gets_dates_cntr w_pcs_gets_dates_cntr

on w_pcs_gets_dates_cntr.create
int iCurrent
call w_response::create
this.cb_ok=create cb_ok
this.cb_cancel=create cb_cancel
this.em_stdt=create em_stdt
this.em_enddt=create em_enddt
this.em_cntr=create em_cntr
this.st_2=create st_2
this.st_3=create st_3
this.st_4=create st_4
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=cb_ok
this.Control[iCurrent+2]=cb_cancel
this.Control[iCurrent+3]=em_stdt
this.Control[iCurrent+4]=em_enddt
this.Control[iCurrent+5]=em_cntr
this.Control[iCurrent+6]=st_2
this.Control[iCurrent+7]=st_3
this.Control[iCurrent+8]=st_4
end on

on w_pcs_gets_dates_cntr.destroy
call w_response::destroy
destroy(this.cb_ok)
destroy(this.cb_cancel)
destroy(this.em_stdt)
destroy(this.em_enddt)
destroy(this.em_cntr)
destroy(this.st_2)
destroy(this.st_3)
destroy(this.st_4)
end on

event key;call super::key;IF KeyDown(KeyEnter!) THEN 
  cb_ok.TriggerEvent(Clicked!)
ELSEIF KeyDown(KeyEscape!) THEN 
  cb_cancel.TriggerEvent(Clicked!)
END IF


end event

event open;call super::open;em_stdt.SetFocus()
end event

event pfc_open;call super::pfc_open;em_stdt.SetFocus()
end event

type cb_ok from u_cb within w_pcs_gets_dates_cntr
int X=42
int Y=417
int Width=316
int Height=85
int TabOrder=40
string Text="&OK"
int TextSize=-10
end type

event clicked;call super::clicked;date ld_stdt, ld_enddt
str_pcs_report lstr_pcs_report
	
IF NOT ISDATE(em_stdt.Text) or NOT ISDATE(em_enddt.text) or & 
		IsNull(em_cntr) or em_cntr.text = "" Then
		MessageBox("Error","Expecting start and end dates and contract number!")
		em_stdt.SetFocus()
ElseIF ISDATE(em_stdt.Text) and NOT ISDATE(em_enddt.text) or &
		IsNull(em_cntr) or em_cntr.text = "" Then
		MessageBox("Error","Expecting start and end dates and contract number!")
		em_enddt.SetFocus()
ElseIF ISDATE(em_stdt.Text) and ISDATE(em_enddt.text) and &
		IsNull(em_cntr) or em_cntr.text = "" Then
		MessageBox("Error","Expecting start and end dates and contract number!")
		em_cntr.SetFocus()
		Return
Else
	lstr_pcs_report.ld_stdt = date(em_stdt.text)
	lstr_pcs_report.ld_enddt = date(em_enddt.text)
	lstr_pcs_report.ls_cntr = string(em_cntr.text)
	OpenWithParm(w_pcs_reports,lstr_pcs_report)
	CloseWithReturn(w_pcs_gets_dates_cntr,lstr_pcs_report)	
End IF




end event

type cb_cancel from u_cb within w_pcs_gets_dates_cntr
int X=545
int Y=417
int Width=316
int Height=85
int TabOrder=50
string Text="&Cancel"
int TextSize=-10
end type

event clicked;call super::clicked;Close(parent)
close(w_pcs_reports)
//IF NOT IsDate(em_stdt.text) or Not IsDate(em_enddt.text) or IsNull(em_cntr.text) or em_cntr.text = "" THEN
//	ib_disableclosequery = TRUE
//	close(w_pcs_gets_dates)
//	close(w_pcs_reports)
//	RETURN
//else
//	Parent.Event pfc_close()
//END IF
	
end event

type em_stdt from u_em within w_pcs_gets_dates_cntr
int X=545
int Y=37
int Width=371
int Height=89
int TabOrder=10
Alignment Alignment=Center!
string Mask="mm/dd/yyyy"
MaskDataType MaskDataType=DateMask!
boolean AutoSkip=true
string DisplayData=""
double Increment=0
string MinMax=""
int TextSize=-10
int Weight=700
end type

type em_enddt from u_em within w_pcs_gets_dates_cntr
int X=545
int Y=149
int Width=371
int Height=89
int TabOrder=20
Alignment Alignment=Center!
string Mask="mm/dd/yyyy"
MaskDataType MaskDataType=DateMask!
boolean AutoSkip=true
string DisplayData=""
double Increment=0
string MinMax=""
int TextSize=-10
int Weight=700
end type

type em_cntr from u_em within w_pcs_gets_dates_cntr
int X=545
int Y=265
int Width=371
int Height=89
int TabOrder=30
Alignment Alignment=Center!
string Mask="!!!!!!!"
MaskDataType MaskDataType=StringMask!
string DisplayData="Ä"
double Increment=0
string MinMax=""
int TextSize=-10
int Weight=700
end type

event modified;call super::modified;cb_ok.Enabled = True
cb_ok.Default = true
end event

type st_2 from statictext within w_pcs_gets_dates_cntr
int X=42
int Y=49
int Width=298
int Height=77
boolean Enabled=false
boolean BringToTop=true
string Text="Start Date:"
Alignment Alignment=Center!
boolean FocusRectangle=false
long BackColor=12632256
int TextSize=-10
int Weight=400
string FaceName="MS Sans Serif"
FontFamily FontFamily=Swiss!
FontPitch FontPitch=Variable!
end type

type st_3 from statictext within w_pcs_gets_dates_cntr
int X=42
int Y=161
int Width=284
int Height=77
boolean Enabled=false
boolean BringToTop=true
string Text="End Date:"
Alignment Alignment=Center!
boolean FocusRectangle=false
long BackColor=12632256
int TextSize=-10
int Weight=400
string FaceName="MS Sans Serif"
FontFamily FontFamily=Swiss!
FontPitch FontPitch=Variable!
end type

type st_4 from statictext within w_pcs_gets_dates_cntr
int X=42
int Y=277
int Width=485
int Height=77
boolean Enabled=false
boolean BringToTop=true
string Text="Contract Number:"
boolean FocusRectangle=false
long BackColor=12632256
int TextSize=-10
int Weight=400
string FaceName="MS Sans Serif"
FontFamily FontFamily=Swiss!
FontPitch FontPitch=Variable!
end type


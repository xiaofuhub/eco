$PBExportHeader$w_pcs_gets_dates.srw
forward
global type w_pcs_gets_dates from w_response
end type
type cb_ok from u_cb within w_pcs_gets_dates
end type
type cb_cancel from u_cb within w_pcs_gets_dates
end type
type em_stdt from u_em within w_pcs_gets_dates
end type
type em_enddt from u_em within w_pcs_gets_dates
end type
type st_1 from u_st within w_pcs_gets_dates
end type
type st_2 from statictext within w_pcs_gets_dates
end type
end forward

global type w_pcs_gets_dates from w_response
int X=801
int Y=621
int Width=983
int Height=609
boolean TitleBar=true
string Title="PCS Report"
cb_ok cb_ok
cb_cancel cb_cancel
em_stdt em_stdt
em_enddt em_enddt
st_1 st_1
st_2 st_2
end type
global w_pcs_gets_dates w_pcs_gets_dates

on w_pcs_gets_dates.create
int iCurrent
call w_response::create
this.cb_ok=create cb_ok
this.cb_cancel=create cb_cancel
this.em_stdt=create em_stdt
this.em_enddt=create em_enddt
this.st_1=create st_1
this.st_2=create st_2
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=cb_ok
this.Control[iCurrent+2]=cb_cancel
this.Control[iCurrent+3]=em_stdt
this.Control[iCurrent+4]=em_enddt
this.Control[iCurrent+5]=st_1
this.Control[iCurrent+6]=st_2
end on

on w_pcs_gets_dates.destroy
call w_response::destroy
destroy(this.cb_ok)
destroy(this.cb_cancel)
destroy(this.em_stdt)
destroy(this.em_enddt)
destroy(this.st_1)
destroy(this.st_2)
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

type cb_ok from u_cb within w_pcs_gets_dates
int X=97
int Y=365
int Width=316
int Height=85
int TabOrder=30
string Text="&OK"
int TextSize=-10
end type

event clicked;call super::clicked;date ld_stdt, ld_enddt
str_cds_report lstr_cds_report
	
IF NOT ISDATE(em_stdt.Text) or NOT ISDATE(em_enddt.text) Then
		MessageBox("Error","Expecting start and end dates!")
		em_stdt.SetFocus()
ElseIF ISDATE(em_stdt.Text) and NOT ISDATE(em_enddt.text) Then
		MessageBox("Error","Expecting start and end dates!")
		em_enddt.SetFocus()
		Return
Else
	lstr_cds_report.ld_stdt = date(em_stdt.text)
	lstr_cds_report.ld_enddt = date(em_enddt.text)
	OpenWithParm(w_pcs_reports,lstr_cds_report)
	CloseWithReturn(w_pcs_gets_dates,lstr_cds_report)	
End IF




end event

type cb_cancel from u_cb within w_pcs_gets_dates
int X=517
int Y=365
int Width=316
int Height=85
int TabOrder=40
string Text="&Cancel"
int TextSize=-10
end type

event clicked;call super::clicked;ib_disableclosequery = TRUE
close(w_pcs_gets_dates)
close(w_pcs_reports)

end event

type em_stdt from u_em within w_pcs_gets_dates
int X=517
int Y=93
int Width=371
int Height=89
int TabOrder=10
Alignment Alignment=Center!
string Mask="mm/dd/yyyy"
MaskDataType MaskDataType=DateMask!
boolean AutoSkip=true
string DisplayData=""
double Increment=0
string MinMax=""
int TextSize=-10
int Weight=700
end type

type em_enddt from u_em within w_pcs_gets_dates
int X=517
int Y=225
int Width=371
int Height=89
int TabOrder=20
Alignment Alignment=Center!
string Mask="mm/dd/yyyy"
MaskDataType MaskDataType=DateMask!
string DisplayData=""
double Increment=0
string MinMax=""
int TextSize=-10
int Weight=700
end type

event modified;call super::modified;cb_ok.Enabled = True
cb_ok.Default = true
end event

type st_1 from u_st within w_pcs_gets_dates
int X=97
int Y=125
int Width=302
int Height=57
string Text="Start Date:"
int TextSize=-10
end type

type st_2 from statictext within w_pcs_gets_dates
int X=97
int Y=257
int Width=298
int Height=57
boolean Enabled=false
boolean BringToTop=true
string Text="End Date:"
boolean FocusRectangle=false
long BackColor=12632256
int TextSize=-10
int Weight=400
string FaceName="MS Sans Serif"
FontFamily FontFamily=Swiss!
FontPitch FontPitch=Variable!
end type


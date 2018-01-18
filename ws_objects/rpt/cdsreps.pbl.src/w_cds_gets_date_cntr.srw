$PBExportHeader$w_cds_gets_date_cntr.srw
forward
global type w_cds_gets_date_cntr from w_response
end type
type cb_ok from u_cb within w_cds_gets_date_cntr
end type
type cb_cancel from u_cb within w_cds_gets_date_cntr
end type
type em_stdt from u_em within w_cds_gets_date_cntr
end type
type st_1 from u_st within w_cds_gets_date_cntr
end type
type em_cntr from u_em within w_cds_gets_date_cntr
end type
type st_2 from statictext within w_cds_gets_date_cntr
end type
end forward

global type w_cds_gets_date_cntr from w_response
int X=997
int Y=501
int Width=1006
int Height=577
boolean TitleBar=true
string Title="Voucher Report"
cb_ok cb_ok
cb_cancel cb_cancel
em_stdt em_stdt
st_1 st_1
em_cntr em_cntr
st_2 st_2
end type
global w_cds_gets_date_cntr w_cds_gets_date_cntr

on w_cds_gets_date_cntr.create
int iCurrent
call w_response::create
this.cb_ok=create cb_ok
this.cb_cancel=create cb_cancel
this.em_stdt=create em_stdt
this.st_1=create st_1
this.em_cntr=create em_cntr
this.st_2=create st_2
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=cb_ok
this.Control[iCurrent+2]=cb_cancel
this.Control[iCurrent+3]=em_stdt
this.Control[iCurrent+4]=st_1
this.Control[iCurrent+5]=em_cntr
this.Control[iCurrent+6]=st_2
end on

on w_cds_gets_date_cntr.destroy
call w_response::destroy
destroy(this.cb_ok)
destroy(this.cb_cancel)
destroy(this.em_stdt)
destroy(this.st_1)
destroy(this.em_cntr)
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

type cb_ok from u_cb within w_cds_gets_date_cntr
int X=55
int Y=325
int Width=316
int Height=85
int TabOrder=30
string Text="&OK"
int TextSize=-10
end type

event clicked;call super::clicked;date ld_stdt
string ls_cntr
str_prod_cntr lstr_prod_cntr
	
IF NOT ISDATE(em_stdt.Text)  or IsNull(em_cntr.text) or em_cntr.text = "" Then
		MessageBox("Error","Expecting start date and contract number!")
		em_stdt.SetFocus()
ElseIF IsDate(em_stdt.text) and IsNull(em_cntr.text) or em_cntr.text = "" Then
		MessageBox("Error","Expecting start date and contract number!")
		em_cntr.SetFocus()
		Return
Else
	lstr_prod_cntr.ld_stdt = date(em_stdt.text)
	lstr_prod_cntr.ls_cntr = string(em_cntr.text)
	OpenWithParm(w_pcs_reports,lstr_prod_cntr)
	CloseWithReturn(w_pcs_gets_date_cntr,lstr_prod_cntr)	
End IF




end event

type cb_cancel from u_cb within w_cds_gets_date_cntr
int X=526
int Y=325
int Width=316
int Height=85
int TabOrder=40
string Text="&Cancel"
int TextSize=-10
end type

event clicked;call super::clicked;ib_disableclosequery = TRUE
close(w_cds_gets_date_cntr)
close(w_cds_reports)
RETURN

end event

type em_stdt from u_em within w_cds_gets_date_cntr
int X=526
int Y=53
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

event modified;call super::modified;cb_ok.Enabled = True
cb_ok.Default = true
end event

type st_1 from u_st within w_cds_gets_date_cntr
int X=60
int Y=57
int Width=307
int Height=85
string Text="Start Date:"
int TextSize=-10
end type

type em_cntr from u_em within w_cds_gets_date_cntr
int X=526
int Y=185
int Width=316
int Height=89
int TabOrder=20
Alignment Alignment=Center!
string Mask=""
MaskDataType MaskDataType=StringMask!
boolean AutoSkip=true
string DisplayData="¨"
double Increment=0
string MinMax=""
int TextSize=-10
int Weight=700
end type

event modified;call super::modified;cb_ok.Enabled = True
cb_ok.Default = true
end event

type st_2 from statictext within w_cds_gets_date_cntr
int X=37
int Y=193
int Width=471
int Height=77
boolean Enabled=false
boolean BringToTop=true
string Text="Contract number:"
boolean FocusRectangle=false
long BackColor=12632256
int TextSize=-10
int Weight=400
string FaceName="MS Sans Serif"
FontFamily FontFamily=Swiss!
FontPitch FontPitch=Variable!
end type


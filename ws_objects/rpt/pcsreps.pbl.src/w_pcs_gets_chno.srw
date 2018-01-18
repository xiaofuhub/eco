$PBExportHeader$w_pcs_gets_chno.srw
forward
global type w_pcs_gets_chno from w_response
end type
type cb_ok from u_cb within w_pcs_gets_chno
end type
type cb_cancel from u_cb within w_pcs_gets_chno
end type
type em_chno from u_em within w_pcs_gets_chno
end type
type st_1 from u_st within w_pcs_gets_chno
end type
end forward

global type w_pcs_gets_chno from w_response
int X=878
int Y=581
int Width=897
int Height=509
boolean TitleBar=true
string Title="PC PAR Report"
cb_ok cb_ok
cb_cancel cb_cancel
em_chno em_chno
st_1 st_1
end type
global w_pcs_gets_chno w_pcs_gets_chno

on w_pcs_gets_chno.create
int iCurrent
call w_response::create
this.cb_ok=create cb_ok
this.cb_cancel=create cb_cancel
this.em_chno=create em_chno
this.st_1=create st_1
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=cb_ok
this.Control[iCurrent+2]=cb_cancel
this.Control[iCurrent+3]=em_chno
this.Control[iCurrent+4]=st_1
end on

on w_pcs_gets_chno.destroy
call w_response::destroy
destroy(this.cb_ok)
destroy(this.cb_cancel)
destroy(this.em_chno)
destroy(this.st_1)
end on

event key;call super::key;IF KeyDown(KeyEnter!) THEN 
  cb_ok.TriggerEvent(Clicked!)
ELSEIF KeyDown(KeyEscape!) THEN 
  cb_cancel.TriggerEvent(Clicked!)
END IF


end event

event open;call super::open;em_chno.SetFocus()
end event

type cb_ok from u_cb within w_pcs_gets_chno
int X=110
int Y=269
int Width=289
int Height=85
int TabOrder=20
string Text="&OK"
int TextSize=-10
end type

event clicked;call super::clicked;string Lchart_no
	
IF ISNULL(em_chno.text) OR em_chno.text = ""  Then
	MessageBox("Error","Invalid Chart Number.")
	em_chno.SetFocus()
	Return
ELSE
	Lchart_no = em_chno.text
	OpenWithParm(w_pcs_reports,Lchart_no)
	CloseWithReturn(w_pcs_gets_chno,Lchart_no)
END IF



end event

type cb_cancel from u_cb within w_pcs_gets_chno
int X=503
int Y=269
int Width=289
int Height=85
int TabOrder=30
string Text="&Cancel"
int TextSize=-10
end type

event clicked;call super::clicked;Close(parent)
close(w_pcs_reports)

end event

type em_chno from u_em within w_pcs_gets_chno
int X=517
int Y=105
int Width=321
int Height=81
int TabOrder=10
Alignment Alignment=Center!
string Mask="######"
MaskDataType MaskDataType=StringMask!
boolean AutoSkip=true
string DisplayData="Ä"
double Increment=0
string MinMax=""
int TextSize=-10
int Weight=700
end type

event modified;call super::modified;cb_ok.Enabled = True
cb_ok.Default = true
end event

type st_1 from u_st within w_pcs_gets_chno
int X=33
int Y=117
int Width=426
int Height=69
string Text=" Chart Number:"
int TextSize=-10
end type


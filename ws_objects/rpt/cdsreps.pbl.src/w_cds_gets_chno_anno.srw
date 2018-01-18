$PBExportHeader$w_cds_gets_chno_anno.srw
forward
global type w_cds_gets_chno_anno from w_response
end type
type cb_ok from u_cb within w_cds_gets_chno_anno
end type
type cb_cancel from u_cb within w_cds_gets_chno_anno
end type
type em_chno from u_em within w_cds_gets_chno_anno
end type
type st_1 from u_st within w_cds_gets_chno_anno
end type
end forward

shared variables

end variables

global type w_cds_gets_chno_anno from w_response
int X=947
int Y=521
int Width=956
int Height=557
boolean TitleBar=true
string Title="Edit Annotation Report"
cb_ok cb_ok
cb_cancel cb_cancel
em_chno em_chno
st_1 st_1
end type
global w_cds_gets_chno_anno w_cds_gets_chno_anno

on w_cds_gets_chno_anno.create
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

on w_cds_gets_chno_anno.destroy
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

type cb_ok from u_cb within w_cds_gets_chno_anno
int X=33
int Y=289
int TabOrder=20
string Text="&OK"
int TextSize=-10
end type

event clicked;call super::clicked;string Lchart_no
	
IF ISNULL(em_chno.text) OR em_chno.text = ""  Then
	MessageBox("Error","You must enter a Chart Number.")
	em_chno.SetFocus()
	Return
ELSE
	Lchart_no = em_chno.text
	OpenWithParm(w_cds_reports,Lchart_no)
	CloseWithReturn(w_cds_gets_chno_anno,Lchart_no)
END IF



end event

type cb_cancel from u_cb within w_cds_gets_chno_anno
int X=535
int Y=293
int TabOrder=30
string Text="&Cancel"
int TextSize=-10
end type

event clicked;call super::clicked;ib_disableclosequery = TRUE
close(w_cds_gets_chno_anno)
close(w_cds_reports)
Return

end event

type em_chno from u_em within w_cds_gets_chno_anno
int X=517
int Y=109
int Width=371
int Height=89
int TabOrder=10
Alignment Alignment=Center!
string Mask="########"
MaskDataType MaskDataType=StringMask!
boolean AutoSkip=true
string DisplayData=""
double Increment=0
string MinMax=""
int TextSize=-10
int Weight=700
end type

event modified;call super::modified;cb_ok.Enabled = True
cb_ok.Default = true
end event

type st_1 from u_st within w_cds_gets_chno_anno
int X=33
int Y=113
int Width=467
int Height=69
string Text=" Control Number:"
int TextSize=-10
end type


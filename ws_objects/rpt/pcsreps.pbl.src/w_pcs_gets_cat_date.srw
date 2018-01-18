$PBExportHeader$w_pcs_gets_cat_date.srw
forward
global type w_pcs_gets_cat_date from w_response
end type
type cb_ok from u_cb within w_pcs_gets_cat_date
end type
type cb_cancel from u_cb within w_pcs_gets_cat_date
end type
type em_date from u_em within w_pcs_gets_cat_date
end type
type st_1 from u_st within w_pcs_gets_cat_date
end type
end forward

global type w_pcs_gets_cat_date from w_response
int X=878
int Y=641
int Width=1134
int Height=597
boolean TitleBar=true
string Title="Catalog Cutoff Date"
cb_ok cb_ok
cb_cancel cb_cancel
em_date em_date
st_1 st_1
end type
global w_pcs_gets_cat_date w_pcs_gets_cat_date

on w_pcs_gets_cat_date.create
int iCurrent
call w_response::create
this.cb_ok=create cb_ok
this.cb_cancel=create cb_cancel
this.em_date=create em_date
this.st_1=create st_1
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=cb_ok
this.Control[iCurrent+2]=cb_cancel
this.Control[iCurrent+3]=em_date
this.Control[iCurrent+4]=st_1
end on

on w_pcs_gets_cat_date.destroy
call w_response::destroy
destroy(this.cb_ok)
destroy(this.cb_cancel)
destroy(this.em_date)
destroy(this.st_1)
end on

event key;call super::key;IF KeyDown(KeyEnter!) THEN 
  cb_ok.TriggerEvent(Clicked!)
ELSEIF KeyDown(KeyEscape!) THEN 
  cb_cancel.TriggerEvent(Clicked!)
END IF


end event

event open;call super::open;em_date.SetFocus()
end event

type cb_ok from u_cb within w_pcs_gets_cat_date
int X=179
int Y=365
int Width=307
int Height=73
int TabOrder=20
string Text="&OK"
int TextSize=-10
end type

event clicked;call super::clicked;date ld_date

IF NOT ISDATE (em_date.Text) Then
	Messagebox("DATE REQUIRED","Please enter valid date.")
	em_date.SetFocus()
	RETURN
ELSE
	ld_date = date(em_date.text)
	OpenWithParm(w_pcs_reports,string(ld_date))
	CloseWithReturn(w_pcs_gets_cat_date,string(ld_date))
End If
end event

type cb_cancel from u_cb within w_pcs_gets_cat_date
int X=618
int Y=369
int Width=307
int Height=73
int TabOrder=30
string Text="&Cancel"
int TextSize=-10
end type

event clicked;call super::clicked;ib_disableclosequery = TRUE
close(w_pcs_gets_cat_date)
close(w_pcs_reports)

end event

type em_date from u_em within w_pcs_gets_cat_date
int X=380
int Y=205
int Width=380
int TabOrder=10
Alignment Alignment=Center!
string Mask="mm/dd/yyyy"
MaskDataType MaskDataType=DateMask!
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

type st_1 from u_st within w_pcs_gets_cat_date
int X=101
int Y=77
int Width=887
int Height=97
string Text="Please enter catalog cutoff date:"
Alignment Alignment=Right!
int TextSize=-10
end type


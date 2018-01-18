$PBExportHeader$w_pcs_gets_date.srw
forward
global type w_pcs_gets_date from w_response
end type
type cb_ok from u_cb within w_pcs_gets_date
end type
type cb_cancel from u_cb within w_pcs_gets_date
end type
type em_date from u_em within w_pcs_gets_date
end type
type st_1 from u_st within w_pcs_gets_date
end type
end forward

global type w_pcs_gets_date from w_response
integer x = 878
integer y = 640
integer width = 873
integer height = 492
string title = "TBT/BBR Report"
cb_ok cb_ok
cb_cancel cb_cancel
em_date em_date
st_1 st_1
end type
global w_pcs_gets_date w_pcs_gets_date

on w_pcs_gets_date.create
int iCurrent
call super::create
this.cb_ok=create cb_ok
this.cb_cancel=create cb_cancel
this.em_date=create em_date
this.st_1=create st_1
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.cb_ok
this.Control[iCurrent+2]=this.cb_cancel
this.Control[iCurrent+3]=this.em_date
this.Control[iCurrent+4]=this.st_1
end on

on w_pcs_gets_date.destroy
call super::destroy
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

type cb_ok from u_cb within w_pcs_gets_date
integer x = 64
integer y = 248
integer width = 306
integer height = 72
integer taborder = 20
integer textsize = -10
string text = "&OK"
end type

event clicked;call super::clicked;date ld_date

IF NOT ISDATE (em_date.Text) Then
	Messagebox("DATE REQUIRED","Please enter valid date.")
	em_date.SetFocus()
	RETURN
ELSE
	ld_date = date(em_date.text)
	OpenWithParm(w_pcs_reports,string(ld_date,'mm/dd/yyyy'))
	CloseWithReturn(w_pcs_gets_date,string(ld_date,'mm/dd/yyyy'))
End If
end event

type cb_cancel from u_cb within w_pcs_gets_date
integer x = 443
integer y = 248
integer width = 306
integer height = 72
integer taborder = 30
integer textsize = -10
string text = "&Cancel"
end type

event clicked;call super::clicked;ib_disableclosequery = TRUE
close(w_pcs_gets_date)
close(w_pcs_reports)

end event

type em_date from u_em within w_pcs_gets_date
integer x = 448
integer y = 64
integer width = 379
integer height = 84
integer taborder = 10
integer textsize = -10
integer weight = 700
alignment alignment = center!
maskdatatype maskdatatype = datemask!
string mask = "mm/dd/yyyy"
boolean autoskip = true
string displaydata = "H"
double increment = 0
string minmax = ""
end type

event modified;call super::modified;cb_ok.Enabled = True
cb_ok.Default = true
end event

type st_1 from u_st within w_pcs_gets_date
integer x = 37
integer y = 72
integer width = 398
integer height = 68
integer textsize = -10
string text = "PMQDT Date:"
end type


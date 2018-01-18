$PBExportHeader$w_gets_date_to_update.srw
forward
global type w_gets_date_to_update from w_response
end type
type cb_ok from u_cb within w_gets_date_to_update
end type
type cb_cancel from u_cb within w_gets_date_to_update
end type
type em_date from u_em within w_gets_date_to_update
end type
type st_1 from u_st within w_gets_date_to_update
end type
end forward

global type w_gets_date_to_update from w_response
integer x = 864
integer y = 576
integer width = 1330
integer height = 708
string title = "Titles Passing Final Review Report"
cb_ok cb_ok
cb_cancel cb_cancel
em_date em_date
st_1 st_1
end type
global w_gets_date_to_update w_gets_date_to_update

on w_gets_date_to_update.create
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

on w_gets_date_to_update.destroy
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

type cb_ok from u_cb within w_gets_date_to_update
integer x = 41
integer y = 448
integer taborder = 0
integer textsize = -10
string text = "&OK"
boolean default = true
end type

event clicked;call super::clicked;date ld_date

IF NOT ISDATE (em_date.Text) or em_date.text = "" Then
	Messagebox("DATE REQUIRED","Please enter valid date.")
	em_date.SetFocus()
	RETURN
ELSE
	ld_date = date(em_date.text)
//	OpenWithParm(w_cds_reports,string(ld_date,'mm/dd/yyyy'))
	CloseWithReturn(w_gets_date_to_update,string(ld_date,'mm/dd/yyyy'))
End If
end event

type cb_cancel from u_cb within w_gets_date_to_update
integer x = 914
integer y = 444
integer taborder = 0
integer textsize = -10
string text = "&Cancel"
end type

event clicked;call super::clicked;ib_disableclosequery = TRUE
ClosewithReturn(w_gets_date_to_update,'cancel')
RETURN

end event

type em_date from u_em within w_gets_date_to_update
integer x = 750
integer y = 216
integer width = 384
integer height = 96
integer taborder = 10
integer textsize = -10
integer weight = 700
alignment alignment = center!
maskdatatype maskdatatype = datemask!
string mask = "mm/dd/yyyy"
boolean autoskip = true
string displaydata = ""
double increment = 0
string minmax = ""
end type

event modified;call super::modified;cb_ok.Enabled = True
cb_ok.Default = true
end event

type st_1 from u_st within w_gets_date_to_update
integer x = 82
integer y = 184
integer width = 562
integer height = 176
integer textsize = -10
string text = "Start Date to Update soundex:"
end type


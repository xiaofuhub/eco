$PBExportHeader$w_cds_gets_conno.srw
forward
global type w_cds_gets_conno from w_response
end type
type cb_ok from u_cb within w_cds_gets_conno
end type
type cb_cancel from u_cb within w_cds_gets_conno
end type
type em_chno from u_em within w_cds_gets_conno
end type
type st_1 from u_st within w_cds_gets_conno
end type
end forward

shared variables

end variables

global type w_cds_gets_conno from w_response
integer x = 946
integer y = 520
integer width = 955
integer height = 556
string title = "Control Number"
cb_ok cb_ok
cb_cancel cb_cancel
em_chno em_chno
st_1 st_1
end type
global w_cds_gets_conno w_cds_gets_conno

on w_cds_gets_conno.create
int iCurrent
call super::create
this.cb_ok=create cb_ok
this.cb_cancel=create cb_cancel
this.em_chno=create em_chno
this.st_1=create st_1
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.cb_ok
this.Control[iCurrent+2]=this.cb_cancel
this.Control[iCurrent+3]=this.em_chno
this.Control[iCurrent+4]=this.st_1
end on

on w_cds_gets_conno.destroy
call super::destroy
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

type cb_ok from u_cb within w_cds_gets_conno
integer x = 32
integer y = 288
integer taborder = 20
integer textsize = -10
string text = "&OK"
end type

event clicked;call super::clicked;string Lchart_no
	
IF ISNULL(em_chno.text) OR em_chno.text = ""  Then
	MessageBox("Error","You must enter a Control Number.")
	em_chno.SetFocus()
	Return
ELSE
	Lchart_no = em_chno.text
	CloseWithReturn(w_cds_gets_conno,Lchart_no)
END IF



end event

type cb_cancel from u_cb within w_cds_gets_conno
integer x = 535
integer y = 292
integer taborder = 30
integer textsize = -10
string text = "&Cancel"
end type

event clicked;call super::clicked;ib_disableclosequery = TRUE
close(w_cds_gets_conno)
Return

end event

type em_chno from u_em within w_cds_gets_conno
integer x = 517
integer y = 108
integer width = 370
integer height = 88
integer taborder = 10
integer textsize = -10
integer weight = 700
alignment alignment = center!
maskdatatype maskdatatype = stringmask!
string mask = "########"
boolean autoskip = true
string displaydata = ""
double increment = 0
string minmax = ""
end type

event modified;call super::modified;cb_ok.Enabled = True
cb_ok.Default = true
end event

type st_1 from u_st within w_cds_gets_conno
integer x = 32
integer y = 112
integer width = 466
integer height = 68
integer textsize = -10
string text = " Control Number:"
end type


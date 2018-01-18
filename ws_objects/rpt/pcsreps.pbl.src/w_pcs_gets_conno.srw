$PBExportHeader$w_pcs_gets_conno.srw
forward
global type w_pcs_gets_conno from w_response
end type
type st_4 from u_st within w_pcs_gets_conno
end type
type st_3 from u_st within w_pcs_gets_conno
end type
type st_2 from u_st within w_pcs_gets_conno
end type
type em_bkseq from u_em within w_pcs_gets_conno
end type
type cb_ok from u_cb within w_pcs_gets_conno
end type
type cb_cancel from u_cb within w_pcs_gets_conno
end type
type em_conno from u_em within w_pcs_gets_conno
end type
type st_1 from u_st within w_pcs_gets_conno
end type
end forward

global type w_pcs_gets_conno from w_response
integer x = 896
integer y = 572
integer width = 1467
integer height = 848
string title = "PAR report criteria"
st_4 st_4
st_3 st_3
st_2 st_2
em_bkseq em_bkseq
cb_ok cb_ok
cb_cancel cb_cancel
em_conno em_conno
st_1 st_1
end type
global w_pcs_gets_conno w_pcs_gets_conno

on w_pcs_gets_conno.create
int iCurrent
call super::create
this.st_4=create st_4
this.st_3=create st_3
this.st_2=create st_2
this.em_bkseq=create em_bkseq
this.cb_ok=create cb_ok
this.cb_cancel=create cb_cancel
this.em_conno=create em_conno
this.st_1=create st_1
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_4
this.Control[iCurrent+2]=this.st_3
this.Control[iCurrent+3]=this.st_2
this.Control[iCurrent+4]=this.em_bkseq
this.Control[iCurrent+5]=this.cb_ok
this.Control[iCurrent+6]=this.cb_cancel
this.Control[iCurrent+7]=this.em_conno
this.Control[iCurrent+8]=this.st_1
end on

on w_pcs_gets_conno.destroy
call super::destroy
destroy(this.st_4)
destroy(this.st_3)
destroy(this.st_2)
destroy(this.em_bkseq)
destroy(this.cb_ok)
destroy(this.cb_cancel)
destroy(this.em_conno)
destroy(this.st_1)
end on

event key;call super::key;IF KeyDown(KeyEnter!) THEN 
  cb_ok.TriggerEvent(Clicked!)
ELSEIF KeyDown(KeyEscape!) THEN 
  cb_cancel.TriggerEvent(Clicked!)
END IF


end event

event open;call super::open;em_bkseq.SetFocus()
end event

type st_4 from u_st within w_pcs_gets_conno
integer x = 311
integer y = 324
integer width = 466
integer textsize = -10
string text = "or"
alignment alignment = center!
end type

type st_3 from u_st within w_pcs_gets_conno
integer x = 201
integer y = 32
integer width = 1029
integer height = 140
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
string text = "Please select book number or control number"
alignment alignment = center!
end type

type st_2 from u_st within w_pcs_gets_conno
integer x = 311
integer y = 260
integer width = 466
integer textsize = -10
string text = "Book Number:"
alignment alignment = right!
end type

type em_bkseq from u_em within w_pcs_gets_conno
integer x = 777
integer y = 256
integer width = 334
integer height = 84
integer taborder = 10
integer textsize = -10
integer weight = 700
alignment alignment = center!
string mask = "########"
boolean autoskip = true
string displaydata = "Ä"
double increment = 0
string minmax = ""
end type

event modified;call super::modified;cb_ok.Enabled = True
cb_ok.Default = true
end event

type cb_ok from u_cb within w_pcs_gets_conno
integer x = 421
integer y = 596
integer width = 293
integer height = 80
integer taborder = 20
integer textsize = -10
string text = "&OK"
end type

event clicked;call super::clicked;string Lcontr_no
long Li_bkseq
	
IF NOT(ISNULL(em_bkseq.text)) AND long(em_bkseq.text) <> 0 THEN
	Li_bkseq = LONG(em_bkseq.text)
	//MessageBox('Data',string(li_bkseq))
   select distinct conno 
	into :lcontr_no
	from mchar
	where bkseq =:Li_bkseq
	using sqlservertrans;
	IF NOT(ISNULL(Lcontr_no)) THEN
		OpenWithParm(w_pcs_reports,Lcontr_no)
		CloseWithReturn(w_pcs_gets_conno,Lcontr_no)
	ELSE
		MessageBox("ERROR","Book number was not found in mchar.")
	END IF
ELSEIF (NOT(ISNULL(em_conno.text)) AND em_conno.text <> "") THEN
	Lcontr_no = em_conno.text
	//MessageBox('Data',Lcontr_no)
	OpenWithParm(w_pcs_reports,Lcontr_no)
	CloseWithReturn(w_pcs_gets_conno,Lcontr_no)
ELSEIF (ISNULL(em_conno.text) OR em_conno.text = "") AND (ISNULL(em_bkseq.text) OR long(em_bkseq.text) = 0) Then
	MessageBox("Error","Invalid Control Number or Book number.")
	em_conno.SetFocus()
	Return
END IF



end event

type cb_cancel from u_cb within w_pcs_gets_conno
integer x = 800
integer y = 596
integer width = 293
integer height = 80
integer taborder = 30
integer textsize = -10
string text = "&Cancel"
end type

event clicked;call super::clicked;ib_disableclosequery = TRUE
close(w_pcs_gets_conno)
close(w_pcs_reports)



end event

type em_conno from u_em within w_pcs_gets_conno
integer x = 777
integer y = 376
integer width = 334
integer height = 84
integer taborder = 10
integer textsize = -10
integer weight = 700
alignment alignment = center!
maskdatatype maskdatatype = stringmask!
string mask = "########"
boolean autoskip = true
string displaydata = "Ä"
double increment = 0
string minmax = ""
end type

event modified;call super::modified;cb_ok.Enabled = True
cb_ok.Default = true
end event

type st_1 from u_st within w_pcs_gets_conno
integer x = 311
integer y = 384
integer width = 466
integer textsize = -10
string text = "Control Number:"
alignment alignment = right!
end type


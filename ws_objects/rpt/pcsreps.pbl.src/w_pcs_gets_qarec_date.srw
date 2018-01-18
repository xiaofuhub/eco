$PBExportHeader$w_pcs_gets_qarec_date.srw
forward
global type w_pcs_gets_qarec_date from w_response
end type
type cb_ok from u_cb within w_pcs_gets_qarec_date
end type
type cb_cancel from u_cb within w_pcs_gets_qarec_date
end type
type em_qarecdt from u_em within w_pcs_gets_qarec_date
end type
type st_1 from u_st within w_pcs_gets_qarec_date
end type
type em_bkmed from u_em within w_pcs_gets_qarec_date
end type
type st_2 from statictext within w_pcs_gets_qarec_date
end type
type st_3 from statictext within w_pcs_gets_qarec_date
end type
type em_bkseq from u_em within w_pcs_gets_qarec_date
end type
end forward

global type w_pcs_gets_qarec_date from w_response
integer x = 878
integer y = 640
integer width = 1097
integer height = 860
string title = "QA Received Date Report"
boolean controlmenu = false
cb_ok cb_ok
cb_cancel cb_cancel
em_qarecdt em_qarecdt
st_1 st_1
em_bkmed em_bkmed
st_2 st_2
st_3 st_3
em_bkseq em_bkseq
end type
global w_pcs_gets_qarec_date w_pcs_gets_qarec_date

on w_pcs_gets_qarec_date.create
int iCurrent
call super::create
this.cb_ok=create cb_ok
this.cb_cancel=create cb_cancel
this.em_qarecdt=create em_qarecdt
this.st_1=create st_1
this.em_bkmed=create em_bkmed
this.st_2=create st_2
this.st_3=create st_3
this.em_bkseq=create em_bkseq
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.cb_ok
this.Control[iCurrent+2]=this.cb_cancel
this.Control[iCurrent+3]=this.em_qarecdt
this.Control[iCurrent+4]=this.st_1
this.Control[iCurrent+5]=this.em_bkmed
this.Control[iCurrent+6]=this.st_2
this.Control[iCurrent+7]=this.st_3
this.Control[iCurrent+8]=this.em_bkseq
end on

on w_pcs_gets_qarec_date.destroy
call super::destroy
destroy(this.cb_ok)
destroy(this.cb_cancel)
destroy(this.em_qarecdt)
destroy(this.st_1)
destroy(this.em_bkmed)
destroy(this.st_2)
destroy(this.st_3)
destroy(this.em_bkseq)
end on

event key;call super::key;IF KeyDown(KeyEnter!) THEN 
  cb_ok.TriggerEvent(Clicked!)
ELSEIF KeyDown(KeyEscape!) THEN 
  cb_cancel.TriggerEvent(Clicked!)
END IF


end event

event open;call super::open;em_qarecdt.SetFocus()
end event

type cb_ok from u_cb within w_pcs_gets_qarec_date
integer x = 169
integer y = 612
integer width = 306
integer height = 88
integer taborder = 40
integer textsize = -10
string text = "&OK"
end type

event clicked;call super::clicked;date ld_qarecdt
//string ls_bkmed
//long ln_bkseq
str_qa_report lstr_qa_report
	
IF NOT ISDATE(em_qarecdt.Text) THEN
	MessageBox("Error","QA Received date must be entered.")
	em_qarecdt.SetFocus()	
	RETURN
END IF

IF (IsNull(em_bkmed.text) OR em_bkmed.text="") AND &
	 (IsNull(em_bkseq.text) OR em_bkseq.text="")	Then
	lstr_qa_report.ls_qamode = 'dateonly'
ELSEIF (IsNull(em_bkmed.text) OR em_bkmed.text="") AND &
	 (IsNull(em_bkseq.text)=FALSE and em_bkseq.text<>"")	THEN
	lstr_qa_report.ls_qamode = 'date_bkseq'
ELSEIF (IsNull(em_bkseq.text) OR em_bkseq.text="") AND &
	(IsNull(em_bkmed.text)=FALSE and em_bkmed.text<>"") THEN
	lstr_qa_report.ls_qamode = 'date_bkmed'
ELSEIF (IsNull(em_bkseq.text)=FALSE and em_bkseq.text<>"") AND &
	(IsNull(em_bkmed.text)=FALSE and em_bkmed.text<>"") THEN
	lstr_qa_report.ls_qamode = 'alldata'
END IF

lstr_qa_report.ld_recdt = date(em_qarecdt.text)
IF em_bkmed.text='P/B' THEN
	em_bkmed.text='BR'
END IF
lstr_qa_report.ls_bkmed = em_bkmed.text
lstr_qa_report.ln_bkseq = long(em_bkseq.text)

OpenWithParm(w_pcs_reports,lstr_qa_report)
CloseWithReturn(w_pcs_gets_qarec_date,lstr_qa_report)	



end event

type cb_cancel from u_cb within w_pcs_gets_qarec_date
integer x = 622
integer y = 608
integer width = 306
integer taborder = 50
integer textsize = -10
string text = "&Cancel"
end type

event clicked;call super::clicked;ib_disableclosequery = TRUE
close(w_pcs_gets_qarec_date)
close(w_pcs_reports)



end event

type em_qarecdt from u_em within w_pcs_gets_qarec_date
integer x = 631
integer y = 76
integer width = 379
integer height = 84
integer taborder = 10
integer textsize = -10
integer weight = 700
alignment alignment = center!
maskdatatype maskdatatype = datemask!
string mask = "mm/dd/yyyy"
boolean autoskip = true
string displaydata = ""
double increment = 0
string minmax = ""
end type

event modified;call super::modified;cb_ok.Enabled = True
cb_ok.Default = true
end event

type st_1 from u_st within w_pcs_gets_qarec_date
integer x = 32
integer y = 76
integer width = 576
integer height = 88
integer textsize = -10
string text = "QA Received Date:"
alignment alignment = right!
end type

type em_bkmed from u_em within w_pcs_gets_qarec_date
integer x = 631
integer y = 244
integer width = 251
integer height = 92
integer taborder = 20
integer textsize = -10
integer weight = 700
textcase textcase = upper!
maskdatatype maskdatatype = stringmask!
string mask = "xxx"
string displaydata = "D"
double increment = 0
string minmax = ""
end type

event modified;call super::modified;IF (em_bkmed.text <> 'RC' OR em_bkmed.text <> 'BR' OR em_bkmed.text <> 'P/B') THEN
	return 2
END IF
end event

type st_2 from statictext within w_pcs_gets_qarec_date
integer x = 261
integer y = 252
integer width = 347
integer height = 76
boolean bringtotop = true
integer textsize = -10
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long backcolor = 80263581
boolean enabled = false
string text = "Book Media"
alignment alignment = right!
boolean focusrectangle = false
end type

type st_3 from statictext within w_pcs_gets_qarec_date
integer x = 215
integer y = 408
integer width = 393
integer height = 76
boolean bringtotop = true
integer textsize = -10
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long backcolor = 80263581
boolean enabled = false
string text = "Book Number"
alignment alignment = right!
boolean focusrectangle = false
end type

type em_bkseq from u_em within w_pcs_gets_qarec_date
integer x = 631
integer y = 412
integer width = 379
integer height = 92
integer taborder = 30
boolean bringtotop = true
integer textsize = -10
integer weight = 700
string mask = "#######"
string displaydata = "h"
double increment = 0
string minmax = ""
end type


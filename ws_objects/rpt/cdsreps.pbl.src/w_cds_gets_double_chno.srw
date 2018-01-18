$PBExportHeader$w_cds_gets_double_chno.srw
forward
global type w_cds_gets_double_chno from w_response
end type
type cb_ok from u_cb within w_cds_gets_double_chno
end type
type cb_cancel from u_cb within w_cds_gets_double_chno
end type
type em_st_chno from u_em within w_cds_gets_double_chno
end type
type em_end_chno from u_em within w_cds_gets_double_chno
end type
type st_1 from u_st within w_cds_gets_double_chno
end type
type st_2 from statictext within w_cds_gets_double_chno
end type
end forward

global type w_cds_gets_double_chno from w_response
integer x = 1161
integer y = 716
integer width = 1111
integer height = 584
string title = "Titles for Consideration Report"
cb_ok cb_ok
cb_cancel cb_cancel
em_st_chno em_st_chno
em_end_chno em_end_chno
st_1 st_1
st_2 st_2
end type
global w_cds_gets_double_chno w_cds_gets_double_chno

on w_cds_gets_double_chno.create
int iCurrent
call super::create
this.cb_ok=create cb_ok
this.cb_cancel=create cb_cancel
this.em_st_chno=create em_st_chno
this.em_end_chno=create em_end_chno
this.st_1=create st_1
this.st_2=create st_2
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.cb_ok
this.Control[iCurrent+2]=this.cb_cancel
this.Control[iCurrent+3]=this.em_st_chno
this.Control[iCurrent+4]=this.em_end_chno
this.Control[iCurrent+5]=this.st_1
this.Control[iCurrent+6]=this.st_2
end on

on w_cds_gets_double_chno.destroy
call super::destroy
destroy(this.cb_ok)
destroy(this.cb_cancel)
destroy(this.em_st_chno)
destroy(this.em_end_chno)
destroy(this.st_1)
destroy(this.st_2)
end on

event key;call super::key;IF KeyDown(KeyEnter!) THEN 
  cb_ok.TriggerEvent(Clicked!)
ELSEIF KeyDown(KeyEscape!) THEN 
  cb_cancel.TriggerEvent(Clicked!)
END IF


end event

event open;call super::open;em_st_chno.SetFocus()
end event

event pfc_postopen;call super::pfc_postopen;string ls_labels
ls_labels =Message.StringParm
IF ls_labels ='Labels' THEN
	This.title ='Labels'
END IF
end event

type cb_ok from u_cb within w_cds_gets_double_chno
integer x = 151
integer y = 352
integer width = 325
integer taborder = 30
integer textsize = -10
string text = "&OK"
end type

event clicked;call super::clicked;string ls_chart_no, ls_chart_no1
str_print_report lstr_print_report

	
IF IsNull(em_st_chno.Text) or em_st_chno.text = "" and &
	IsNull(em_end_chno.text) or em_end_chno.text = "" Then
	MessageBox("Error","Expecting start and end chart numbers!")
	em_st_chno.SetFocus()
ElseIF not isNull(em_st_chno.text) and IsNull(em_end_chno.text) or &
	em_end_chno.text = "" Then
	MessageBox("Error","Expecting start and end chart numbers!")
	em_end_chno.SetFocus()
	Return
Else
	lstr_print_report.lchart_no = em_st_chno.text
	lstr_print_report.lchart_no1 = em_end_chno.text
	OpenWithParm(w_cds_reports,lstr_print_report)
	CloseWithReturn(w_cds_gets_double_chno,lstr_print_report)	
End IF




end event

type cb_cancel from u_cb within w_cds_gets_double_chno
integer x = 571
integer y = 352
integer width = 325
integer taborder = 40
integer textsize = -10
string text = "&Cancel"
end type

event clicked;call super::clicked;ib_disableclosequery = TRUE
close(w_cds_gets_double_chno)
close(w_cds_reports)
Return

end event

type em_st_chno from u_em within w_cds_gets_double_chno
integer x = 613
integer y = 40
integer width = 347
integer height = 96
integer taborder = 10
integer textsize = -10
integer weight = 700
alignment alignment = center!
textcase textcase = upper!
maskdatatype maskdatatype = stringmask!
string mask = "######"
boolean autoskip = true
string displaydata = "Ä"
double increment = 0
string minmax = ""
end type

event modified;call super::modified;cb_ok.Enabled = True
cb_ok.Default = True
end event

type em_end_chno from u_em within w_cds_gets_double_chno
integer x = 613
integer y = 192
integer width = 347
integer height = 96
integer taborder = 20
integer textsize = -10
integer weight = 700
alignment alignment = center!
textcase textcase = upper!
maskdatatype maskdatatype = stringmask!
string mask = "######"
string displaydata = "\Ð"
double increment = 0
string minmax = ""
end type

event modified;call super::modified;cb_ok.Enabled = True
cb_ok.Default = True
end event

type st_1 from u_st within w_cds_gets_double_chno
integer x = 46
integer y = 60
integer width = 535
integer height = 72
integer textsize = -10
string text = "Start Chart Number:"
end type

type st_2 from statictext within w_cds_gets_double_chno
integer x = 46
integer y = 196
integer width = 530
integer height = 76
boolean bringtotop = true
integer textsize = -10
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long backcolor = 12632256
boolean enabled = false
string text = "End Chart Number:"
boolean focusrectangle = false
end type


$PBExportHeader$w_pcpar_report.srw
forward
global type w_pcpar_report from window
end type
type sle_no_copies from singlelineedit within w_pcpar_report
end type
type st_3 from statictext within w_pcpar_report
end type
type dw_cc_par_report from datawindow within w_pcpar_report
end type
type em_conno from uo_conno within w_pcpar_report
end type
type st_2 from statictext within w_pcpar_report
end type
type cb_cancel from commandbutton within w_pcpar_report
end type
type cb_ok from commandbutton within w_pcpar_report
end type
end forward

global type w_pcpar_report from window
integer x = 846
integer y = 460
integer width = 1143
integer height = 776
boolean titlebar = true
string title = "PCPAR Report"
boolean controlmenu = true
windowtype windowtype = response!
long backcolor = 79741120
event ue_postevent ( )
sle_no_copies sle_no_copies
st_3 st_3
dw_cc_par_report dw_cc_par_report
em_conno em_conno
st_2 st_2
cb_cancel cb_cancel
cb_ok cb_ok
end type
global w_pcpar_report w_pcpar_report

type variables

end variables

event key;IF KeyDown(KeyEnter!) THEN 
  cb_ok.TriggerEvent(Clicked!)
ELSEIF KeyDown(KeyEscape!) THEN 
  cb_cancel.TriggerEvent(Clicked!)
END IF


end event

on w_pcpar_report.create
this.sle_no_copies=create sle_no_copies
this.st_3=create st_3
this.dw_cc_par_report=create dw_cc_par_report
this.em_conno=create em_conno
this.st_2=create st_2
this.cb_cancel=create cb_cancel
this.cb_ok=create cb_ok
this.Control[]={this.sle_no_copies,&
this.st_3,&
this.dw_cc_par_report,&
this.em_conno,&
this.st_2,&
this.cb_cancel,&
this.cb_ok}
end on

on w_pcpar_report.destroy
destroy(this.sle_no_copies)
destroy(this.st_3)
destroy(this.dw_cc_par_report)
destroy(this.em_conno)
destroy(this.st_2)
destroy(this.cb_cancel)
destroy(this.cb_ok)
end on

type sle_no_copies from singlelineedit within w_pcpar_report
integer x = 672
integer y = 380
integer width = 210
integer height = 92
integer taborder = 30
integer textsize = -10
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long backcolor = 16777215
string text = "1"
boolean autohscroll = false
borderstyle borderstyle = stylelowered!
end type

type st_3 from statictext within w_pcpar_report
integer x = 41
integer y = 388
integer width = 626
integer height = 76
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long backcolor = 79741120
boolean enabled = false
string text = "Number of copies:"
alignment alignment = center!
boolean focusrectangle = false
end type

type dw_cc_par_report from datawindow within w_pcpar_report
boolean visible = false
integer x = 891
integer y = 248
integer width = 288
integer height = 172
integer taborder = 10
boolean enabled = false
string dataobject = "d_cc_par_report_conno1"
end type

event constructor;dw_cc_par_report.SetTransObject( SQLServerTrans )

end event

type em_conno from uo_conno within w_pcpar_report
integer x = 672
integer y = 184
integer width = 315
integer height = 96
integer taborder = 20
integer textsize = -10
end type

type st_2 from statictext within w_pcpar_report
integer x = 41
integer y = 196
integer width = 544
integer height = 80
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long backcolor = 79741120
boolean enabled = false
string text = "Control Number:"
alignment alignment = center!
long bordercolor = 79741120
boolean focusrectangle = false
end type

type cb_cancel from commandbutton within w_pcpar_report
integer x = 663
integer y = 556
integer width = 297
integer height = 108
integer textsize = -10
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "&Cancel"
end type

event clicked;close(parent)
end event

type cb_ok from commandbutton within w_pcpar_report
integer x = 160
integer y = 560
integer width = 306
integer height = 108
integer textsize = -10
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "&OK"
end type

event clicked;long job
int rc,no_copies,i
string Lconno

Lconno = em_conno.text
no_copies = integer(sle_no_copies.text)

IF Lconno <> "" THEN
	dw_cc_par_report.retrieve(Lconno)
		
	job = PrintOpen( ) 
	FOR i=1 to no_copies 
		PrintDataWindow(job, dw_cc_par_report) 
	NEXT
	PrintClose(job)
ELSE
	MessageBox("ERROR", "Please enter the Control Number.",StopSign!)
	em_conno.SetFocus()
END IF
cb_ok.Enabled = TRUE

end event


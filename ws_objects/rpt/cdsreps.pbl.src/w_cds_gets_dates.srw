$PBExportHeader$w_cds_gets_dates.srw
forward
global type w_cds_gets_dates from w_response
end type
type cb_ok from u_cb within w_cds_gets_dates
end type
type cb_cancel from u_cb within w_cds_gets_dates
end type
type em_stdt from u_em within w_cds_gets_dates
end type
type em_enddt from u_em within w_cds_gets_dates
end type
type st_1 from u_st within w_cds_gets_dates
end type
type st_2 from statictext within w_cds_gets_dates
end type
end forward

global type w_cds_gets_dates from w_response
integer x = 937
integer y = 592
integer width = 960
integer height = 616
string title = "CDS Titles Annotated Report"
cb_ok cb_ok
cb_cancel cb_cancel
em_stdt em_stdt
em_enddt em_enddt
st_1 st_1
st_2 st_2
end type
global w_cds_gets_dates w_cds_gets_dates

on w_cds_gets_dates.create
int iCurrent
call super::create
this.cb_ok=create cb_ok
this.cb_cancel=create cb_cancel
this.em_stdt=create em_stdt
this.em_enddt=create em_enddt
this.st_1=create st_1
this.st_2=create st_2
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.cb_ok
this.Control[iCurrent+2]=this.cb_cancel
this.Control[iCurrent+3]=this.em_stdt
this.Control[iCurrent+4]=this.em_enddt
this.Control[iCurrent+5]=this.st_1
this.Control[iCurrent+6]=this.st_2
end on

on w_cds_gets_dates.destroy
call super::destroy
destroy(this.cb_ok)
destroy(this.cb_cancel)
destroy(this.em_stdt)
destroy(this.em_enddt)
destroy(this.st_1)
destroy(this.st_2)
end on

event key;call super::key;IF KeyDown(KeyEnter!) THEN 
  cb_ok.TriggerEvent(Clicked!)
ELSEIF KeyDown(KeyEscape!) THEN 
  cb_cancel.TriggerEvent(Clicked!)
END IF


end event

event open;call super::open;em_stdt.SetFocus()
end event

event pfc_open;call super::pfc_open;em_stdt.SetFocus()
end event

type cb_ok from u_cb within w_cds_gets_dates
integer x = 59
integer y = 372
integer taborder = 30
integer textsize = -10
string text = "&OK"
end type

event clicked;call super::clicked;date ld_stdt, ld_enddt
str_cds_report lstr_cds_report
	
IF NOT ISDATE(em_stdt.Text) or em_stdt.text = "" and &
		NOT ISDATE(em_enddt.text) or em_enddt.text = "" Then
		MessageBox("Error","Expecting start and end dates!")
		em_stdt.SetFocus()
ElseIF ISDATE(em_stdt.Text)  and &
		NOT ISDATE(em_enddt.text) or em_enddt.text = "" Then
		MessageBox("Error","Expecting start and end dates!")
		em_enddt.SetFocus()
		Return
Else
	lstr_cds_report.ld_stdt = date(em_stdt.text)
	lstr_cds_report.ld_enddt = date(em_enddt.text)
	OpenWithParm(w_cds_reports,lstr_cds_report)
	CloseWithReturn(w_cds_gets_dates,lstr_cds_report)	
End IF




end event

type cb_cancel from u_cb within w_cds_gets_dates
integer x = 480
integer y = 372
integer taborder = 40
integer textsize = -10
string text = "&Cancel"
end type

event clicked;call super::clicked;str_cds_report lstr_cds_report
ib_disableclosequery = TRUE
lstr_cds_report.ls_string='CANCEL'
closewithRETURN(w_cds_gets_dates,lstr_cds_report)
//close(w_cds_reports)
//RETURN

end event

type em_stdt from u_em within w_cds_gets_dates
integer x = 416
integer y = 64
integer width = 402
integer height = 96
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

type em_enddt from u_em within w_cds_gets_dates
integer x = 416
integer y = 208
integer width = 402
integer height = 96
integer taborder = 20
integer textsize = -10
integer weight = 700
alignment alignment = center!
maskdatatype maskdatatype = datemask!
string mask = "mm/dd/yyyy"
string displaydata = "~b"
double increment = 0
string minmax = ""
end type

event modified;call super::modified;cb_ok.Enabled = True
cb_ok.Default = true
end event

type st_1 from u_st within w_cds_gets_dates
integer x = 91
integer y = 80
integer width = 297
integer height = 68
integer textsize = -10
string text = "Start Date:"
end type

type st_2 from statictext within w_cds_gets_dates
integer x = 105
integer y = 220
integer width = 283
integer height = 76
boolean bringtotop = true
integer textsize = -10
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long backcolor = 12632256
boolean enabled = false
string text = "End Date:"
alignment alignment = center!
boolean focusrectangle = false
end type


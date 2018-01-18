$PBExportHeader$w_pcs_gets_dates_cd_blnd.srw
forward
global type w_pcs_gets_dates_cd_blnd from w_response
end type
type cb_ok from u_cb within w_pcs_gets_dates_cd_blnd
end type
type cb_cancel from u_cb within w_pcs_gets_dates_cd_blnd
end type
type em_stdt from u_em within w_pcs_gets_dates_cd_blnd
end type
type em_enddt from u_em within w_pcs_gets_dates_cd_blnd
end type
type st_1 from u_st within w_pcs_gets_dates_cd_blnd
end type
type st_2 from statictext within w_pcs_gets_dates_cd_blnd
end type
end forward

global type w_pcs_gets_dates_cd_blnd from w_response
integer x = 800
integer y = 620
integer width = 1207
integer height = 608
string title = "Shipped but not yet on CD BLND"
boolean controlmenu = false
cb_ok cb_ok
cb_cancel cb_cancel
em_stdt em_stdt
em_enddt em_enddt
st_1 st_1
st_2 st_2
end type
global w_pcs_gets_dates_cd_blnd w_pcs_gets_dates_cd_blnd

on w_pcs_gets_dates_cd_blnd.create
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

on w_pcs_gets_dates_cd_blnd.destroy
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

type cb_ok from u_cb within w_pcs_gets_dates_cd_blnd
integer x = 197
integer y = 376
integer width = 315
integer height = 84
integer taborder = 30
integer textsize = -10
string text = "&OK"
end type

event clicked;call super::clicked;date ld_stdt, ld_enddt
str_cds_report lstr_cds_report
	
IF NOT ISDATE(em_stdt.Text) or NOT ISDATE(em_enddt.text) Then
		MessageBox("Error","Expecting stage II date in and actual end dates!")
		em_stdt.SetFocus()
ElseIF ISDATE(em_stdt.Text) and NOT ISDATE(em_enddt.text) Then
		MessageBox("Error","Expecting stage II date in and actual end dates!")
		em_enddt.SetFocus()
		Return
Else
	lstr_cds_report.ld_stdt = date(em_stdt.text)
	lstr_cds_report.ld_enddt = date(em_enddt.text)
	OpenWithParm(w_pcs_reports,lstr_cds_report)
	CloseWithReturn(w_pcs_gets_dates_cd_blnd,lstr_cds_report)	
End IF




end event

type cb_cancel from u_cb within w_pcs_gets_dates_cd_blnd
integer x = 617
integer y = 376
integer width = 315
integer height = 84
integer taborder = 40
integer textsize = -10
string text = "&Cancel"
end type

event clicked;call super::clicked;ib_disableclosequery = TRUE
close(w_pcs_gets_dates_cd_blnd)
close(w_pcs_reports)

end event

type em_stdt from u_em within w_pcs_gets_dates_cd_blnd
integer x = 635
integer y = 92
integer width = 370
integer height = 88
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

type em_enddt from u_em within w_pcs_gets_dates_cd_blnd
integer x = 635
integer y = 224
integer width = 370
integer height = 88
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

type st_1 from u_st within w_pcs_gets_dates_cd_blnd
integer x = 137
integer y = 104
integer width = 439
integer height = 80
integer textsize = -10
string text = "Stage II Date in:"
end type

type st_2 from statictext within w_pcs_gets_dates_cd_blnd
integer x = 142
integer y = 244
integer width = 466
integer height = 56
boolean bringtotop = true
integer textsize = -10
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long backcolor = 12632256
boolean enabled = false
string text = "Actual End Date:"
boolean focusrectangle = false
end type


$PBExportHeader$w_pcs_gets_dates_cntr_option_act_cntr.srw
forward
global type w_pcs_gets_dates_cntr_option_act_cntr from w_response
end type
type cbx_bks_sorted from checkbox within w_pcs_gets_dates_cntr_option_act_cntr
end type
type cb_ok from u_cb within w_pcs_gets_dates_cntr_option_act_cntr
end type
type cb_cancel from u_cb within w_pcs_gets_dates_cntr_option_act_cntr
end type
type em_stdt from u_em within w_pcs_gets_dates_cntr_option_act_cntr
end type
type em_enddt from u_em within w_pcs_gets_dates_cntr_option_act_cntr
end type
type em_cntr from u_em within w_pcs_gets_dates_cntr_option_act_cntr
end type
type st_2 from statictext within w_pcs_gets_dates_cntr_option_act_cntr
end type
type st_3 from statictext within w_pcs_gets_dates_cntr_option_act_cntr
end type
type st_4 from statictext within w_pcs_gets_dates_cntr_option_act_cntr
end type
type cbx_act_cntr from u_cbx within w_pcs_gets_dates_cntr_option_act_cntr
end type
end forward

global type w_pcs_gets_dates_cntr_option_act_cntr from w_response
integer x = 997
integer y = 500
integer width = 1253
integer height = 944
string title = "Average Deviation Report Criteria"
cbx_bks_sorted cbx_bks_sorted
cb_ok cb_ok
cb_cancel cb_cancel
em_stdt em_stdt
em_enddt em_enddt
em_cntr em_cntr
st_2 st_2
st_3 st_3
st_4 st_4
cbx_act_cntr cbx_act_cntr
end type
global w_pcs_gets_dates_cntr_option_act_cntr w_pcs_gets_dates_cntr_option_act_cntr

type variables

end variables

on w_pcs_gets_dates_cntr_option_act_cntr.create
int iCurrent
call super::create
this.cbx_bks_sorted=create cbx_bks_sorted
this.cb_ok=create cb_ok
this.cb_cancel=create cb_cancel
this.em_stdt=create em_stdt
this.em_enddt=create em_enddt
this.em_cntr=create em_cntr
this.st_2=create st_2
this.st_3=create st_3
this.st_4=create st_4
this.cbx_act_cntr=create cbx_act_cntr
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.cbx_bks_sorted
this.Control[iCurrent+2]=this.cb_ok
this.Control[iCurrent+3]=this.cb_cancel
this.Control[iCurrent+4]=this.em_stdt
this.Control[iCurrent+5]=this.em_enddt
this.Control[iCurrent+6]=this.em_cntr
this.Control[iCurrent+7]=this.st_2
this.Control[iCurrent+8]=this.st_3
this.Control[iCurrent+9]=this.st_4
this.Control[iCurrent+10]=this.cbx_act_cntr
end on

on w_pcs_gets_dates_cntr_option_act_cntr.destroy
call super::destroy
destroy(this.cbx_bks_sorted)
destroy(this.cb_ok)
destroy(this.cb_cancel)
destroy(this.em_stdt)
destroy(this.em_enddt)
destroy(this.em_cntr)
destroy(this.st_2)
destroy(this.st_3)
destroy(this.st_4)
destroy(this.cbx_act_cntr)
end on

event key;call super::key;IF KeyDown(KeyEnter!) THEN 
  cb_ok.TriggerEvent(Clicked!)
ELSEIF KeyDown(KeyEscape!) THEN 
  cb_cancel.TriggerEvent(Clicked!)
END IF


end event

event open;call super::open;cbx_bks_sorted.VISIBLE =false
//em_stdt.text = string(dev_stdt)
//em_enddt.text = string(dev_enddt)
em_stdt.SetFocus()

end event

event pfc_open;call super::pfc_open;em_stdt.SetFocus()

end event

type cbx_bks_sorted from checkbox within w_pcs_gets_dates_cntr_option_act_cntr
integer x = 201
integer y = 536
integer width = 946
integer height = 80
integer textsize = -8
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 33554432
long backcolor = 67108864
string text = "Book numbers sorted within contracts"
end type

event clicked;int rtn

IF this.checked THEN
	rtn = MessageBox("Books Sorted","This option will cause the retrieve to take a longer time to display. If you need to cancel the retrival, push the DBCancel button. Continue?",Question!,YesNo!,1)
	IF rtn = 2 THEN
		this.checked=false
	end if
END IF
end event

type cb_ok from u_cb within w_pcs_gets_dates_cntr_option_act_cntr
integer x = 192
integer y = 708
integer width = 315
integer height = 84
integer taborder = 50
integer textsize = -10
string text = "&Ok"
end type

event clicked;call super::clicked;date ld_stdt, ld_enddt
str_pcs_report lstr_pcs_report
lstr_pcs_report.b_ok =true
//THERE ARE TWO CASE ONE IS CHECKED BOX NOT CHECKED, THE OTHER IS CHECKED
IF NOT (cbx_act_cntr.CHECKED ) THEN	
	IF NOT ISDATE(em_stdt.Text) or NOT ISDATE(em_enddt.text) or & 
			IsNull(em_cntr) or em_cntr.text = "" Then
			MessageBox("Error","Expecting start and end dates and contract number!")
			em_stdt.SetFocus()
	ElseIF ISDATE(em_stdt.Text) and NOT ISDATE(em_enddt.text) or &
			IsNull(em_cntr) or em_cntr.text = "" Then
			MessageBox("Error","Expecting start and end dates and contract number!")
			em_enddt.SetFocus()
	ElseIF ISDATE(em_stdt.Text) and ISDATE(em_enddt.text) and &
			IsNull(em_cntr) or em_cntr.text = "" Then
			MessageBox("Error","Expecting start and end dates and contract number!")
			em_cntr.SetFocus()
			Return
	Else
		lstr_pcs_report.ld_stdt = date(em_stdt.text)
		lstr_pcs_report.ld_enddt = date(em_enddt.text)
//		dev_stdt = date(em_stdt.text)
//		dev_enddt = date(em_enddt.text)
		lstr_pcs_report.ls_cntr = string(em_cntr.text)
		lstr_pcs_report.b_cbx_checked= false
		CloseWithReturn(w_pcs_gets_dates_cntr_option_act_cntr,lstr_pcs_report)
		RETURN
	End IF
END IF
//IF CHECKED BOX IS CHECKED, ACTIVE CONTRACT # WILL BE RETRIEVED FROM DATABASE 
//INSTEAD OF TYPE IN
IF (cbx_act_cntr.CHECKED) THEN
	
	IF  NOT ISDATE(em_enddt.text) THEN
		MessageBox("Error","Expecting  end dates ")
		em_enddt.SetFocus()
		RETURN
	END IF
	IF  NOT ISDATE(em_stdt.text) THEN
		MessageBox("Error","Expecting  start dates ")
		em_stdt.SetFocus()
		RETURN
	END IF
	lstr_pcs_report.ld_stdt = date(em_stdt.text)
//	lstr_pcs_report.ls_cntr = string(em_cntr.text)
	lstr_pcs_report.ld_enddt = date(em_enddt.text)
//	dev_stdt = date(em_stdt.text)
//	dev_enddt = date(em_enddt.text)
	lstr_pcs_report.b_cbx_checked =true
	IF cbx_bks_sorted.Checked THEN
		lstr_pcs_report.b_bks_sorted =true
	ELSE
		lstr_pcs_report.b_bks_sorted =false	
	END IF
	CloseWithReturn(w_pcs_gets_dates_cntr_option_act_cntr,lstr_pcs_report)	
END IF



end event

type cb_cancel from u_cb within w_pcs_gets_dates_cntr_option_act_cntr
integer x = 741
integer y = 708
integer width = 315
integer height = 84
integer taborder = 60
integer textsize = -10
string text = "&Cancel"
end type

event clicked;call super::clicked;str_pcs_report lstr_pcs_report
lstr_pcs_report.b_ok =false
CloseWithReturn(w_pcs_gets_dates_cntr_option_act_cntr, lstr_pcs_report)
RETURN
end event

type em_stdt from u_em within w_pcs_gets_dates_cntr_option_act_cntr
integer x = 704
integer y = 60
integer width = 425
integer height = 88
integer taborder = 20
integer textsize = -10
integer weight = 700
alignment alignment = center!
maskdatatype maskdatatype = datemask!
string mask = "mm/dd/yyyy"
boolean autoskip = true
string displaydata = ""
double increment = 0
string minmax = ""
end type

type em_enddt from u_em within w_pcs_gets_dates_cntr_option_act_cntr
integer x = 704
integer y = 172
integer width = 421
integer height = 88
integer taborder = 30
integer textsize = -10
integer weight = 700
alignment alignment = center!
maskdatatype maskdatatype = datemask!
string mask = "mm/dd/yyyy"
boolean autoskip = true
string displaydata = "~b"
double increment = 0
string minmax = ""
end type

type em_cntr from u_em within w_pcs_gets_dates_cntr_option_act_cntr
integer x = 704
integer y = 288
integer width = 416
integer height = 88
integer taborder = 40
integer textsize = -10
integer weight = 700
alignment alignment = center!
maskdatatype maskdatatype = stringmask!
string mask = "!!!!!!!"
string displaydata = "Ä"
double increment = 0
string minmax = ""
end type

event modified;call super::modified;cb_ok.Enabled = True
cb_ok.Default = true
end event

type st_2 from statictext within w_pcs_gets_dates_cntr_option_act_cntr
integer x = 201
integer y = 72
integer width = 297
integer height = 76
boolean bringtotop = true
integer textsize = -10
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long backcolor = 67108864
boolean enabled = false
string text = "Start Date:"
alignment alignment = center!
boolean focusrectangle = false
end type

type st_3 from statictext within w_pcs_gets_dates_cntr_option_act_cntr
integer x = 201
integer y = 184
integer width = 283
integer height = 76
boolean bringtotop = true
integer textsize = -10
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long backcolor = 67108864
boolean enabled = false
string text = "End Date:"
alignment alignment = center!
boolean focusrectangle = false
end type

type st_4 from statictext within w_pcs_gets_dates_cntr_option_act_cntr
integer x = 201
integer y = 300
integer width = 485
integer height = 76
boolean bringtotop = true
integer textsize = -10
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long backcolor = 67108864
boolean enabled = false
string text = "Contract Number:"
boolean focusrectangle = false
end type

type cbx_act_cntr from u_cbx within w_pcs_gets_dates_cntr_option_act_cntr
integer x = 201
integer y = 436
integer width = 498
integer height = 68
string text = "Active contracts"
end type

event clicked;call super::clicked;IF cbx_act_cntr.CHECKED THEN
	em_cntr.VISIBLE =false
	em_stdt.visible =true
	em_enddt.visible =true
	cbx_bks_sorted.VISIBLE =true
	st_2.visible =true
	st_3.visible =true
	st_4.visible =false
//	dw_act_cntr.SetTransObject(sqlservertrans )
//	dw_act_cntr.Retrieve()
ELSE
	em_cntr.VISIBLE =TRUE
	em_stdt.visible =true
	em_enddt.visible =true
	cbx_bks_sorted.VISIBLE =false
	cbx_bks_sorted.CHECKED =false
	st_2.visible =true
	st_3.visible =true
	st_4.visible =TRUE
END IF
end event


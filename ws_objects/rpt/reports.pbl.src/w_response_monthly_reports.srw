$PBExportHeader$w_response_monthly_reports.srw
forward
global type w_response_monthly_reports from w_response
end type
type cbx_backall from u_cbx within w_response_monthly_reports
end type
type r_all_prdr from rectangle within w_response_monthly_reports
end type
type st_back from statictext within w_response_monthly_reports
end type
type st_all from statictext within w_response_monthly_reports
end type
type cbx_back from u_cbx within w_response_monthly_reports
end type
type cb_save from u_cb within w_response_monthly_reports
end type
type cbx_one_prdr_incld from u_cbx within w_response_monthly_reports
end type
type cbx_all_prdr_incld from u_cbx within w_response_monthly_reports
end type
type dw_rpt_for_cntrmed from u_pics_dw within w_response_monthly_reports
end type
type st_med from statictext within w_response_monthly_reports
end type
type cbx_one_prdr from u_cbx within w_response_monthly_reports
end type
type mle_qa from u_mle within w_response_monthly_reports
end type
type mle_prd from u_mle within w_response_monthly_reports
end type
type st_qa_commts from statictext within w_response_monthly_reports
end type
type st_prd_commts from statictext within w_response_monthly_reports
end type
type r_one_prdr from rectangle within w_response_monthly_reports
end type
type rb_unsatisqa from u_rb within w_response_monthly_reports
end type
type rb_satisqa from u_rb within w_response_monthly_reports
end type
type rb_unsatis_prd from u_rb within w_response_monthly_reports
end type
type rb_satis_prd from u_rb within w_response_monthly_reports
end type
type st_prdr_status from statictext within w_response_monthly_reports
end type
type st_mon_one from statictext within w_response_monthly_reports
end type
type st_yr_one from statictext within w_response_monthly_reports
end type
type cbx_regen from u_cbx within w_response_monthly_reports
end type
type ddlb_month_one from u_ddlb within w_response_monthly_reports
end type
type dw_cntrfy_one from u_pics_dw within w_response_monthly_reports
end type
type cbx_all_prdr from u_cbx within w_response_monthly_reports
end type
type st_one_prdr from statictext within w_response_monthly_reports
end type
type st_mon_all from statictext within w_response_monthly_reports
end type
type dw_cntrfy_all from u_pics_dw within w_response_monthly_reports
end type
type dw_one_prdr from u_pics_dw within w_response_monthly_reports
end type
type ddlb_month_all from u_ddlb within w_response_monthly_reports
end type
type cb_ok from u_cb within w_response_monthly_reports
end type
type cb_cancel from u_cb within w_response_monthly_reports
end type
type st_yr_all from statictext within w_response_monthly_reports
end type
type st_qa_status from statictext within w_response_monthly_reports
end type
type gb_qa from groupbox within w_response_monthly_reports
end type
type gb_pd from groupbox within w_response_monthly_reports
end type
end forward

global type w_response_monthly_reports from w_response
integer x = 878
integer y = 640
integer width = 2126
integer height = 2016
string title = "Monthly Report Option"
boolean controlmenu = false
cbx_backall cbx_backall
r_all_prdr r_all_prdr
st_back st_back
st_all st_all
cbx_back cbx_back
cb_save cb_save
cbx_one_prdr_incld cbx_one_prdr_incld
cbx_all_prdr_incld cbx_all_prdr_incld
dw_rpt_for_cntrmed dw_rpt_for_cntrmed
st_med st_med
cbx_one_prdr cbx_one_prdr
mle_qa mle_qa
mle_prd mle_prd
st_qa_commts st_qa_commts
st_prd_commts st_prd_commts
r_one_prdr r_one_prdr
rb_unsatisqa rb_unsatisqa
rb_satisqa rb_satisqa
rb_unsatis_prd rb_unsatis_prd
rb_satis_prd rb_satis_prd
st_prdr_status st_prdr_status
st_mon_one st_mon_one
st_yr_one st_yr_one
cbx_regen cbx_regen
ddlb_month_one ddlb_month_one
dw_cntrfy_one dw_cntrfy_one
cbx_all_prdr cbx_all_prdr
st_one_prdr st_one_prdr
st_mon_all st_mon_all
dw_cntrfy_all dw_cntrfy_all
dw_one_prdr dw_one_prdr
ddlb_month_all ddlb_month_all
cb_ok cb_ok
cb_cancel cb_cancel
st_yr_all st_yr_all
st_qa_status st_qa_status
gb_qa gb_qa
gb_pd gb_pd
end type
global w_response_monthly_reports w_response_monthly_reports

on w_response_monthly_reports.create
int iCurrent
call super::create
this.cbx_backall=create cbx_backall
this.r_all_prdr=create r_all_prdr
this.st_back=create st_back
this.st_all=create st_all
this.cbx_back=create cbx_back
this.cb_save=create cb_save
this.cbx_one_prdr_incld=create cbx_one_prdr_incld
this.cbx_all_prdr_incld=create cbx_all_prdr_incld
this.dw_rpt_for_cntrmed=create dw_rpt_for_cntrmed
this.st_med=create st_med
this.cbx_one_prdr=create cbx_one_prdr
this.mle_qa=create mle_qa
this.mle_prd=create mle_prd
this.st_qa_commts=create st_qa_commts
this.st_prd_commts=create st_prd_commts
this.r_one_prdr=create r_one_prdr
this.rb_unsatisqa=create rb_unsatisqa
this.rb_satisqa=create rb_satisqa
this.rb_unsatis_prd=create rb_unsatis_prd
this.rb_satis_prd=create rb_satis_prd
this.st_prdr_status=create st_prdr_status
this.st_mon_one=create st_mon_one
this.st_yr_one=create st_yr_one
this.cbx_regen=create cbx_regen
this.ddlb_month_one=create ddlb_month_one
this.dw_cntrfy_one=create dw_cntrfy_one
this.cbx_all_prdr=create cbx_all_prdr
this.st_one_prdr=create st_one_prdr
this.st_mon_all=create st_mon_all
this.dw_cntrfy_all=create dw_cntrfy_all
this.dw_one_prdr=create dw_one_prdr
this.ddlb_month_all=create ddlb_month_all
this.cb_ok=create cb_ok
this.cb_cancel=create cb_cancel
this.st_yr_all=create st_yr_all
this.st_qa_status=create st_qa_status
this.gb_qa=create gb_qa
this.gb_pd=create gb_pd
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.cbx_backall
this.Control[iCurrent+2]=this.r_all_prdr
this.Control[iCurrent+3]=this.st_back
this.Control[iCurrent+4]=this.st_all
this.Control[iCurrent+5]=this.cbx_back
this.Control[iCurrent+6]=this.cb_save
this.Control[iCurrent+7]=this.cbx_one_prdr_incld
this.Control[iCurrent+8]=this.cbx_all_prdr_incld
this.Control[iCurrent+9]=this.dw_rpt_for_cntrmed
this.Control[iCurrent+10]=this.st_med
this.Control[iCurrent+11]=this.cbx_one_prdr
this.Control[iCurrent+12]=this.mle_qa
this.Control[iCurrent+13]=this.mle_prd
this.Control[iCurrent+14]=this.st_qa_commts
this.Control[iCurrent+15]=this.st_prd_commts
this.Control[iCurrent+16]=this.r_one_prdr
this.Control[iCurrent+17]=this.rb_unsatisqa
this.Control[iCurrent+18]=this.rb_satisqa
this.Control[iCurrent+19]=this.rb_unsatis_prd
this.Control[iCurrent+20]=this.rb_satis_prd
this.Control[iCurrent+21]=this.st_prdr_status
this.Control[iCurrent+22]=this.st_mon_one
this.Control[iCurrent+23]=this.st_yr_one
this.Control[iCurrent+24]=this.cbx_regen
this.Control[iCurrent+25]=this.ddlb_month_one
this.Control[iCurrent+26]=this.dw_cntrfy_one
this.Control[iCurrent+27]=this.cbx_all_prdr
this.Control[iCurrent+28]=this.st_one_prdr
this.Control[iCurrent+29]=this.st_mon_all
this.Control[iCurrent+30]=this.dw_cntrfy_all
this.Control[iCurrent+31]=this.dw_one_prdr
this.Control[iCurrent+32]=this.ddlb_month_all
this.Control[iCurrent+33]=this.cb_ok
this.Control[iCurrent+34]=this.cb_cancel
this.Control[iCurrent+35]=this.st_yr_all
this.Control[iCurrent+36]=this.st_qa_status
this.Control[iCurrent+37]=this.gb_qa
this.Control[iCurrent+38]=this.gb_pd
end on

on w_response_monthly_reports.destroy
call super::destroy
destroy(this.cbx_backall)
destroy(this.r_all_prdr)
destroy(this.st_back)
destroy(this.st_all)
destroy(this.cbx_back)
destroy(this.cb_save)
destroy(this.cbx_one_prdr_incld)
destroy(this.cbx_all_prdr_incld)
destroy(this.dw_rpt_for_cntrmed)
destroy(this.st_med)
destroy(this.cbx_one_prdr)
destroy(this.mle_qa)
destroy(this.mle_prd)
destroy(this.st_qa_commts)
destroy(this.st_prd_commts)
destroy(this.r_one_prdr)
destroy(this.rb_unsatisqa)
destroy(this.rb_satisqa)
destroy(this.rb_unsatis_prd)
destroy(this.rb_satis_prd)
destroy(this.st_prdr_status)
destroy(this.st_mon_one)
destroy(this.st_yr_one)
destroy(this.cbx_regen)
destroy(this.ddlb_month_one)
destroy(this.dw_cntrfy_one)
destroy(this.cbx_all_prdr)
destroy(this.st_one_prdr)
destroy(this.st_mon_all)
destroy(this.dw_cntrfy_all)
destroy(this.dw_one_prdr)
destroy(this.ddlb_month_all)
destroy(this.cb_ok)
destroy(this.cb_cancel)
destroy(this.st_yr_all)
destroy(this.st_qa_status)
destroy(this.gb_qa)
destroy(this.gb_pd)
end on

event key;call super::key;IF KeyDown(KeyEnter!) THEN 
  cb_ok.TriggerEvent(Clicked!)
ELSEIF KeyDown(KeyEscape!) THEN 
  cb_cancel.TriggerEvent(Clicked!)
END IF


end event

event open;call super::open;//em_date.SetFocus()
this.of_SetBase(true)
this.inv_base.of_Center()

 	cbx_all_prdr.visible=true
	cbx_one_prdr.visible=true
	cb_cancel.visible=true
	cb_ok.visible=false
	cb_save.visible= false
   cbx_backall.visible=false
	cbx_all_prdr_incld.visible=false
//	cbx_one_prdr.visible=false
	cbx_one_prdr_incld.visible=false
	cbx_regen.visible=false
	cbx_back.visible=false
	ddlb_month_all.visible=false
	ddlb_month_one.visible=false
	dw_cntrfy_all.visible=false
	dw_cntrfy_one.visible=false
	dw_one_prdr.visible=false
	dw_rpt_for_cntrmed.visible=false
	mle_prd.visible=false
	mle_qa.visible=false
	r_all_prdr.visible=false
	r_one_prdr.visible=false
	gb_pd.visible=false
	gb_qa.visible=false
	rb_satis_prd.visible=false
	rb_satisqa.visible=false
	rb_unsatis_prd.visible=false
	rb_unsatisqa.visible=false
	st_med.visible=false
	st_mon_all.visible=false
	st_mon_one.visible=false
	st_one_prdr.visible=false
	st_prd_commts.visible=false
	st_prdr_status.visible=false
	st_qa_commts.visible=false
	st_qa_status.visible=false
	st_yr_all.visible=false
	st_yr_one.visible=false
	
	
end event

event pfc_postopen;call super::pfc_postopen;datawindowchild dwc_cntrfy,dwc_cntrfy_1,dwc_cntrmed
datawindowchild dwc_prdr
date ld_today,ld_before_start30,ld_start_date
int li_month, li_fy,li_yr,li_pre_yr, li_row, i
string ls_month, ls_today, ls_fy, ls_pre_yr,ls_yr,ls_prdr,ls_med, ls_sum
datetime ld_dt, ld_bef_30dt


ib_disableclosequery =true
ld_today = Today()
ls_today = string( ld_today,'mm/dd/yyyy')
ls_yr=Right(ls_today, 4)
li_yr =integer(ls_yr)
ls_month =left( ls_today, 2 )
if ls_month='01' then
	li_yr= li_yr - 1
end if
li_month = integer( ls_month )
li_month=li_month - 1
if li_month<=0 then
	li_month=li_month + 12
end if
choose case li_month
	case 1
		ddlb_month_all.text ='January'
	case 2
		ddlb_month_all.text ='February'
	case 3
		ddlb_month_all.text ='March'
	case 4
		ddlb_month_all.text ='April'
	case 5 
		ddlb_month_all.text ='May'
	case 6
		ddlb_month_all.text ='June'
	case 7
		ddlb_month_all.text ='July'
	case 8
		ddlb_month_all.text ='August'
	case 9
		ddlb_month_all.text ='September'
	case 10
		ddlb_month_all.text ='October'
	case 11
		ddlb_month_all.text ='November'
	case 12
		ddlb_month_all.text ='December'
end choose	
choose case li_month
	case 1
		ddlb_month_one.text ='January'
	case 2
		ddlb_month_one.text ='February'
	case 3
		ddlb_month_one.text ='March'
	case 4
		ddlb_month_one.text ='April'
	case 5 
		ddlb_month_one.text ='May'
	case 6
		ddlb_month_one.text ='June'
	case 7
		ddlb_month_one.text ='July'
	case 8
		ddlb_month_one.text ='August'
	case 9
		ddlb_month_one.text ='September'
	case 10
		ddlb_month_one.text ='October'
	case 11
		ddlb_month_one.text ='November'
	case 12
		ddlb_month_one.text ='December'
end choose	
ls_month=ddlb_month_one.text
choose case ls_month
	case 'January'
		ld_start_date =date('01/01/'+string(li_yr))
	case 'February'
		ld_start_date =date('02/01/'+string(li_yr))
	case 'March'
		ld_start_date =date('03/01/'+string(li_yr))
	case 'April'
		ld_start_date =date('04/01/'+string(li_yr))
	case 'May'
		ld_start_date =date('05/01/'+string(li_yr))
	case 'June'
		ld_start_date =date('06/01/'+string(li_yr))
	case 'July'
		ld_start_date =date('07/01/'+string(li_yr))
	case 'August'
		ld_start_date =date('08/01/'+string(li_yr))
	case 'September'
		ld_start_date =date('09/01/'+string(li_yr))
	case 'October'
		ld_start_date =date('10/01/'+string(li_yr ) )
	case 'November'
		ld_start_date =date('11/01/'+string(li_yr))
	case 'December'
		ld_start_date =date('12/01/'+string(li_yr))
end choose
ld_before_start30=RelativeDate(ld_start_date, - 30 )
this.title= 'Monthly Reports Option'
dw_cntrfy_all.SetTransObject( SqlServerTrans )
dw_cntrfy_all.GetChild('cntrfy', dwc_cntrfy)
dwc_cntrfy.SetTransObject( SqlServerTrans)
dwc_cntrfy.Retrieve()
dw_cntrfy_all.Retrieve()
dw_cntrfy_all.InsertRow(0 )
dw_cntrfy_all.SetItem(1,'cntrfy',li_yr)
//dw_prdr.visible =false
dw_cntrfy_one.SetTransObject( SqlServerTrans )
dw_cntrfy_one.GetChild('cntrfy', dwc_cntrfy_1)
dwc_cntrfy_1.SetTransObject( SqlServerTrans)
dwc_cntrfy_1.Retrieve()
dw_cntrfy_one.Retrieve()
dw_cntrfy_one.InsertRow(0 )
dw_cntrfy_one.SetItem(1,'cntrfy',li_yr)
dw_one_prdr.SetTransObject( SqlServerTrans )
dw_one_prdr.GetChild('prdr', dwc_prdr)
ld_bef_30dt=datetime(ld_before_start30, time('00:00:00'))
dwc_prdr.SetTransObject( SqlServerTrans )
dwc_prdr.Retrieve(ld_bef_30dt)

dw_one_prdr.Retrieve(ld_bef_30dt)
dw_one_prdr.InsertRow(0)
ls_prdr=dwc_prdr.GetItemString(1,'prdr')
dw_one_prdr.SetItem(1,'prdr',ls_prdr)

dw_rpt_for_cntrmed.SetTransObject( SqlServerTrans )
dw_rpt_for_cntrmed.GetChild('cntrmed', dwc_cntrmed)
dwc_cntrmed.SetTransObject( SqlServerTrans )
dwc_cntrmed.Retrieve(ld_bef_30dt,ls_prdr)
ls_med =dwc_cntrmed.GetItemString(1,'cntrmed')
dw_rpt_for_cntrmed.Retrieve(ld_bef_30dt,ls_prdr)
dw_rpt_for_cntrmed.InsertRow(0)
dw_rpt_for_cntrmed.SetItem(1,'cntrmed', ls_med)
end event

type cbx_backall from u_cbx within w_response_monthly_reports
integer x = 1349
integer y = 232
integer width = 590
integer height = 68
integer taborder = 80
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
string text = "Last 12 months"
end type

event clicked;call super::clicked;string ls_month, ls_start_date, ls_end_date, ls_m, ls_yr, ls_preyr, ls_mold
long li_yr, li_m, li_mold
date ld_start_date, ld_end_date


li_yr =dw_cntrfy_all.GetItemNumber(1,'cntrfy')
ls_month=ddlb_month_all.text
ls_preyr=string(li_yr - 1 )
choose case ls_month
	case 'January'
		ld_start_date =date('01/01/'+string(li_yr))
		ld_end_date =date('01/31/'+string(li_yr))
	case 'February'
		ld_start_date =date('02/01/'+string(li_yr))
	if mod(li_yr,4 ) =0 then
		ld_end_date =date('02/29/'+string(li_yr))
	else
		ld_end_date =date('02/28/'+string(li_yr))
	end if
	case 'March'
		ld_start_date =date('03/01/'+string(li_yr))
		ld_end_date =date('03/31/'+string(li_yr))
	case 'April'
		ld_start_date =date('04/01/'+string(li_yr))
		ld_end_date =date('04/30/'+string(li_yr))
	case 'May'
		ld_start_date =date('05/01/'+string(li_yr))
		ld_end_date =date('05/31/'+string(li_yr))
	case 'June'
		ld_start_date =date('06/01/'+string(li_yr))
		ld_end_date =date('06/30/'+string(li_yr))
	case 'July'
		ld_start_date =date('07/01/'+string(li_yr))
		ld_end_date =date('07/31/'+string(li_yr))
	case 'August'
		ld_start_date =date('08/01/'+string(li_yr))
		ld_end_date =date('08/31/'+string(li_yr))
	case 'September'
		ld_start_date =date('09/01/'+string(li_yr))
		ld_end_date =date('09/30/'+string(li_yr))
	case 'October'
		ld_start_date =date('10/01/'+string(li_yr))
		ld_end_date =date('10/31/'+string(li_yr))
	case 'November'
		ld_start_date =date('11/01/'+string(li_yr ))
		ld_end_date =date('11/30/'+string(li_yr ))
	case 'December'
		ld_start_date =date('12/01/'+string(li_yr ))
		ld_end_date =date('12/31/'+string(li_yr ))
end choose

ls_start_date=string(ld_start_date,'mm/dd/yyyy')
ls_end_date=string(ld_end_date,'mm/dd/yyyy')
ls_m=mid(ls_start_date,1, 2)
ls_yr=mid(ls_start_date,7, 4)
st_all.visible=true
st_back.visible=false
li_m=long(ls_m)
li_mold=li_m
ls_mold=string(li_mold,'00')
if cbx_backall.checked then
	cbx_regen.checked=false		
	li_m++
	ls_m=string(li_m,'00')
	if li_m>12 then
		ls_start_date='01/01/'+ls_yr
		ls_end_date='12/31/'+ls_yr
		st_all.text='This choice will produce report for all producer that choosen from '+ls_start_date+&
		' to '+ls_end_date
	else
		st_all.text='This choice will produce report for all producer that choosen from '+ls_m+'/01/'+ls_preyr+&
		' to '+ls_end_date
	end if
elseif cbx_backall.checked=false then
	if ls_mold>='10' then
		st_all.text='This choice will produce report for all producer that choosen from 10/01/'+ls_yr+&
		' to '+ls_end_date
	else
		st_all.text='This choice will produce report for one producer that choosen from 10/01/'+ls_preyr+&
		' to '+ls_end_date
	end if
end if
//if ls_back='Y' then
//	li_m=long(ls_m)
//	li_mold=li_m
//	li_m++
//	ls_m=string(li_m,'00')
//	if li_m>12 then
//		ls_start_date='01/01/'+ls_yr
//		ls_end_date='12/31/'+ls_yr
//	elseif li_m<=12 then
//		ls_start_date=ls_m+'/01/'+string(li_yr - 1)
//		ls_end_date=ls_end_date
//	end if
//	st_all.text='This choice will produce report for all producer from '+ls_start_date+&
//		' to '+ls_end_date
//end if
end event

type r_all_prdr from rectangle within w_response_monthly_reports
integer linethickness = 1
long fillcolor = 12632256
integer x = 59
integer y = 48
integer width = 1915
integer height = 280
end type

type st_back from statictext within w_response_monthly_reports
integer x = 137
integer y = 872
integer width = 1925
integer height = 104
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 33554432
long backcolor = 80263581
boolean focusrectangle = false
end type

type st_all from statictext within w_response_monthly_reports
integer x = 137
integer y = 352
integer width = 1920
integer height = 104
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 33554432
long backcolor = 80263581
boolean focusrectangle = false
end type

type cbx_back from u_cbx within w_response_monthly_reports
integer x = 1115
integer y = 792
integer width = 590
integer height = 68
integer taborder = 80
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
string text = "Last 12 months"
end type

event clicked;call super::clicked;string ls_month, ls_start_date, ls_end_date, ls_m, ls_yr, ls_preyr, ls_mold
long li_yr, li_m, li_mold
date ld_start_date, ld_end_date


li_yr =dw_cntrfy_one.GetItemNumber(1,'cntrfy')
ls_month=ddlb_month_one.text
ls_preyr=string(li_yr - 1 )
choose case ls_month
	case 'January'
		ld_start_date =date('01/01/'+string(li_yr))
		ld_end_date =date('01/31/'+string(li_yr))
	case 'February'
		ld_start_date =date('02/01/'+string(li_yr))
	if mod(li_yr,4 ) =0 then
		ld_end_date =date('02/29/'+string(li_yr))
	else
		ld_end_date =date('02/28/'+string(li_yr))
	end if
	case 'March'
		ld_start_date =date('03/01/'+string(li_yr))
		ld_end_date =date('03/31/'+string(li_yr))
	case 'April'
		ld_start_date =date('04/01/'+string(li_yr))
		ld_end_date =date('04/30/'+string(li_yr))
	case 'May'
		ld_start_date =date('05/01/'+string(li_yr))
		ld_end_date =date('05/31/'+string(li_yr))
	case 'June'
		ld_start_date =date('06/01/'+string(li_yr))
		ld_end_date =date('06/30/'+string(li_yr))
	case 'July'
		ld_start_date =date('07/01/'+string(li_yr))
		ld_end_date =date('07/31/'+string(li_yr))
	case 'August'
		ld_start_date =date('08/01/'+string(li_yr))
		ld_end_date =date('08/31/'+string(li_yr))
	case 'September'
		ld_start_date =date('09/01/'+string(li_yr))
		ld_end_date =date('09/30/'+string(li_yr))
	case 'October'
		ld_start_date =date('10/01/'+string(li_yr))
		ld_end_date =date('10/31/'+string(li_yr))
	case 'November'
		ld_start_date =date('11/01/'+string(li_yr ))
		ld_end_date =date('11/30/'+string(li_yr ))
	case 'December'
		ld_start_date =date('12/01/'+string(li_yr ))
		ld_end_date =date('12/31/'+string(li_yr ))
end choose

ls_start_date=string(ld_start_date,'mm/dd/yyyy')
ls_end_date=string(ld_end_date,'mm/dd/yyyy')
ls_m=mid(ls_start_date,1, 2)
ls_yr=mid(ls_start_date,7, 4)
st_all.visible=false
st_back.visible=true
li_m=long(ls_m)
li_mold=li_m
ls_mold=string(li_mold,'00')
if cbx_back.checked then
	cbx_regen.checked=false		
	li_m++
	ls_m=string(li_m,'00')
	if li_m>12 then
		ls_start_date='01/01/'+ls_yr
		ls_end_date='12/31/'+ls_yr
		st_back.text='This choice will produce report for one producer that choosen from '+ls_start_date+&
		' to '+ls_end_date
	else
		st_back.text='This choice will produce report for one producer that choosen from '+ls_m+'/01/'+ls_preyr+&
		' to '+ls_end_date
	end if
else
	if ls_mold>='10' then
		st_back.text='This choice will produce report for one producer that choosen from 10/01/'+ls_yr+&
		' to '+ls_end_date
	else
		st_back.text='This choice will produce report for one producer that choosen from 10/01/'+ls_preyr+&
		' to '+ls_end_date
	end if
end if
end event

type cb_save from u_cb within w_response_monthly_reports
integer x = 704
integer y = 1776
integer width = 558
integer height = 72
integer taborder = 180
integer textsize = -10
string text = "&Save Comments"
end type

event clicked;call super::clicked;Long li_yr ,li_count, li_re, li_start_month,li_fy
String ls_prdr, ls_month, ls_yr, ls_start_date, ls_like, ls_num_month, ls_yr2,&
		ls_oneprdr,ls_cntrmed,ls_incldavg, ls_regen,ls_prd_commts, ls_qa_commts,&
		ls_qastat, ls_prdstat,ls_cntrlc,ls_done,ls_max, ls_min,ls_med,ls_forceregen,&
		ls_lang
str_voucher_report lstr
Date ld_start_date, ld_end_date, ld_fy_start_date, ld_fy_end_date
Boolean lb_extract_checked 

ls_prdr =dw_one_prdr.GetItemString(1,'prdr')//if all prdr choose first prdr to make
ls_cntrmed=dw_rpt_for_cntrmed.GetItemString(1,'cntrmed')//out put show instead plank paper
ls_prdr= Trim(ls_prdr)
ls_cntrmed= Trim(ls_cntrmed)
lstr.array[3] = ls_prdr
lstr.array[13] =ls_cntrmed
IF cbx_one_prdr.checked THEN
	li_yr =dw_cntrfy_one.GetItemNumber(1, 'cntrfy')
	IF IsNull(li_yr) THEN
		Messagebox('','You must choose a calendar year',exclamation!)
		RETURN
	END IF
	ls_yr = String(li_yr)
	ls_month =ddlb_month_one.text
ELSE
	li_yr =dw_cntrfy_all.GetItemNumber(1, 'cntrfy')
	IF IsNull(li_yr) THEN
		Messagebox('','You must choose a calendar year',exclamation!)
		RETURN
	END IF
	ls_yr = String(li_yr)
	ls_month =ddlb_month_all.text
END IF
CHOOSE CASE ls_month
	CASE 'January'
		ld_start_date =Date('01/01/'+String(li_yr))
		ld_end_date =Date('01/31/'+String(li_yr))
	CASE 'February'
		ld_start_date =Date('02/01/'+String(li_yr))
		IF Mod(li_yr,4) =0 THEN
			ld_end_date =Date('02/29/'+String(li_yr))
		ELSE
			ld_end_date =Date('02/28/'+String(li_yr))
		END IF
	CASE 'March'
		ld_start_date =Date('03/01/'+String(li_yr))
		ld_end_date =Date('03/31/'+String(li_yr))
	CASE 'April'
		ld_start_date =Date('04/01/'+String(li_yr))
		ld_end_date =Date('04/30/'+String(li_yr))
	CASE 'May'
		ld_start_date =Date('05/01/'+String(li_yr))
		ld_end_date =Date('05/31/'+String(li_yr))
	CASE 'June'
		ld_start_date =Date('06/01/'+String(li_yr))
		ld_end_date =Date('06/30/'+String(li_yr))
	CASE 'July'
		ld_start_date =Date('07/01/'+String(li_yr))
		ld_end_date =Date('07/31/'+String(li_yr))
	CASE 'August'
		ld_start_date =Date('08/01/'+String(li_yr))
		ld_end_date =Date('08/31/'+String(li_yr))
	CASE 'September'
		ld_start_date =Date('09/01/'+String(li_yr))
		ld_end_date =Date('09/30/'+String(li_yr))
	CASE 'October'
		ld_start_date =Date('10/01/'+String(li_yr))
		ld_end_date =Date('10/31/'+String(li_yr))
	CASE 'November'
		ld_start_date =Date('11/01/'+String(li_yr))
		ld_end_date =Date('11/30/'+String(li_yr))
	CASE 'December'
		ld_start_date =Date('12/01/'+String(li_yr))
		ld_end_date =Date('12/31/'+String(li_yr))
END CHOOSE
IF cbx_one_prdr.checked THEN
	ls_oneprdr='Y'
	ls_prdr =dw_one_prdr.GetItemString(1,'prdr')
	ls_cntrmed=dw_rpt_for_cntrmed.GetItemString(1,'cntrmed')
	ls_prdr= Trim(ls_prdr)
	ls_cntrmed= Trim(ls_cntrmed)
	lstr.array[3] = ls_prdr
	lstr.array[13] =ls_cntrmed
	IF rb_satis_prd.checked THEN
		ls_prdstat='Y'
	ELSE
		ls_prdstat='N'
	END IF
	IF rb_satisqa.checked THEN
		ls_qastat='Y'
	ELSE
		ls_qastat='N'
	END IF
	ls_prd_commts=mle_prd.text
	ls_qa_commts=mle_qa.text
   IF ls_prdr='PTB' AND ls_cntrmed='FL' THEN
		ls_cntrmed='RTB'
		ls_lang='Y'
	ELSEIF ls_prdr='PTB' AND (ls_cntrmed='RC'  OR ls_cntrmed = 'RTB') THEN
		ls_lang='N'
	END IF
	IF ls_prdr<>'PTB' THEN
		UPDATE monrpt
		SET prd_commts=:ls_prd_commts ,qa_commts=:ls_qa_commts,prdstat=:ls_prdstat,&
			qastat=:ls_qastat	
		WHERE prdr=:ls_prdr AND cntrmed=:ls_cntrmed AND start_date=:ld_start_date  
		USING sqlservertrans;
	ELSEIF ls_prdr='PTB' THEN
		UPDATE monrpt
		SET prd_commts=:ls_prd_commts ,qa_commts=:ls_qa_commts,prdstat=:ls_prdstat,&
			qastat=:ls_qastat	
		WHERE prdr=:ls_prdr AND cntrmed=:ls_cntrmed AND start_date=:ld_start_date AND &
		      foreign_lang=:ls_lang
		USING sqlservertrans;
	END IF
	IF NOT f_check_dberror(sqlservertrans, 'update monrpt set prd_commts,qa_commts') THEN
		RETURN
	ELSE
		COMMIT USING sqlservertrans;
		cb_save.enabled=FALSE
		RETURN
	END IF
	IF cbx_one_prdr_incld.checked THEN
		ls_incldavg='Y'
	ELSE
		ls_incldavg='N'
	END IF
ELSE
	ls_oneprdr='N'
	IF cbx_all_prdr_incld.checked THEN
		ls_incldavg='Y'
	ELSE
		ls_incldavg='N'
	END IF
END IF
lstr.array[1] = ls_month
lstr.array[2] = ls_yr //*	**the year hear still regular year	
lstr.array[7] ='ok'
ls_start_date = String(ld_start_date,'mm/dd/yyyy')
lstr.array[8] = ls_start_date
lstr.array[9] = String(ld_end_date,'mm/dd/yyyy')

ls_num_month =Left(ls_start_date, 2)
li_start_month =Integer(ls_num_month)
IF li_start_month >=10 THEN
	ld_fy_start_date=Date('10/01/'+ls_yr)
	ld_fy_end_date =Date('09/30/'+ String(li_yr + 1))
ELSE
	ld_fy_start_date=Date('10/01/'+String(li_yr - 1))
	ld_fy_end_date =Date('09/30/'+ ls_yr)
END IF

IF cbx_one_prdr.checked THEN

	IF cbx_regen.checked THEN
		ls_forceregen='Y'
		lstr.array[4] =ls_forceregen
	ELSE
		ls_forceregen='N'
		lstr.array[4] =ls_forceregen
	END IF

END IF//if cbx_oneprdr

lstr.array[10] =String(ld_fy_start_date,'mm/dd/yyyy')
lstr.array[11] =String(ld_fy_end_date,'mm/dd/yyyy')
//lstr.array[13] =ls_cntrmed
lstr.array[14] =ls_oneprdr
lstr.array[15] =ls_incldavg
CloseWithReturn(w_response_monthly_reports,lstr)

end event

type cbx_one_prdr_incld from u_cbx within w_response_monthly_reports
integer x = 59
integer y = 792
integer width = 1024
integer height = 68
integer taborder = 110
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
string text = "Include Average Deviation Report"
end type

event clicked;call super::clicked;//string ls_month,  ls_fy
//int li_fy, li_count, li_re
//date ld_start_date
//
//ls_month =ddlb_month.text
//li_fy =dw_cntrfy.GetItemNumber( 1, 'cntrfy')
//if Isnull(li_fy ) then
//	messagebox('','You must choose the year from drop down datawindow')
//	return
//end if
//choose case ls_month
//	case 'January'
//		ld_start_date =date('01/01/'+string(li_fy))
//	case 'February'
//		ld_start_date =date('02/01/'+string(li_fy))
//	case 'March'
//		ld_start_date =date('03/01/'+string(li_fy))
//	case 'April'
//		ld_start_date =date('04/01/'+string(li_fy))
//	case 'May'
//		ld_start_date =date('05/01/'+string(li_fy))
//	case 'June'
//		ld_start_date =date('06/01/'+string(li_fy))
//	case 'July'
//		ld_start_date =date('07/01/'+string(li_fy))
//	case 'August'
//		ld_start_date =date('08/01/'+string(li_fy))
//	case 'September'
//		ld_start_date =date('09/01/'+string(li_fy))
//	case 'October'
//		ld_start_date =date('10/01/'+string(li_fy ))
//	case 'November'
//		ld_start_date =date('11/01/'+string(li_fy ))
//	case 'December'
//		ld_start_date =date('12/01/'+string(li_fy))
//end choose
//select sum(awadtitles)+sum(asgntitles)+sum(shiptitles)+sum(latetottitles) into :li_count
//	from monrpt
//	where start_date = :ld_start_date
//	using SqlServerTrans;
//	if not f_check_dberror(SqlServerTrans, 'select from monrpt to find count') then
//		return
//	end if
//if cbx_extract.checked then
//	if li_count >0 then
//		li_re =messagebox('Producer Monthly Reports',&
//			'~nData for this month has aready been extracted'+&
//			'~nWould you like to refresh the data  ?',Question!, YesNo!,2)
//		if li_re =2 then
//			cbx_extract.checked = false
//		end if
//	end if
//else
//	if li_count<= 0 or IsNull( li_count ) then
//		messagebox('Producer Monthly Reports',&
//			'~nData for this month has not been extracted'+&
//			'~nYou must extract data first')
//		this.checked =true
//	end if
//end if
//			
end event

type cbx_all_prdr_incld from u_cbx within w_response_monthly_reports
integer x = 137
integer y = 228
integer width = 1184
integer height = 68
integer taborder = 40
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
string text = "Include Average Deviation Report"
end type

event clicked;call super::clicked;//string ls_month,  ls_fy
//int li_fy, li_count, li_re
//date ld_start_date
//
//ls_month =ddlb_month.text
//li_fy =dw_cntrfy.GetItemNumber( 1, 'cntrfy')
//if Isnull(li_fy ) then
//	messagebox('','You must choose the year from drop down datawindow')
//	return
//end if
//choose case ls_month
//	case 'January'
//		ld_start_date =date('01/01/'+string(li_fy))
//	case 'February'
//		ld_start_date =date('02/01/'+string(li_fy))
//	case 'March'
//		ld_start_date =date('03/01/'+string(li_fy))
//	case 'April'
//		ld_start_date =date('04/01/'+string(li_fy))
//	case 'May'
//		ld_start_date =date('05/01/'+string(li_fy))
//	case 'June'
//		ld_start_date =date('06/01/'+string(li_fy))
//	case 'July'
//		ld_start_date =date('07/01/'+string(li_fy))
//	case 'August'
//		ld_start_date =date('08/01/'+string(li_fy))
//	case 'September'
//		ld_start_date =date('09/01/'+string(li_fy))
//	case 'October'
//		ld_start_date =date('10/01/'+string(li_fy ))
//	case 'November'
//		ld_start_date =date('11/01/'+string(li_fy ))
//	case 'December'
//		ld_start_date =date('12/01/'+string(li_fy))
//end choose
//select sum(awadtitles)+sum(asgntitles)+sum(shiptitles)+sum(latetottitles) into :li_count
//	from monrpt
//	where start_date = :ld_start_date
//	using SqlServerTrans;
//	if not f_check_dberror(SqlServerTrans, 'select from monrpt to find count') then
//		return
//	end if
//if cbx_extract.checked then
//	if li_count >0 then
//		li_re =messagebox('Producer Monthly Reports',&
//			'~nData for this month has aready been extracted'+&
//			'~nWould you like to refresh the data  ?',Question!, YesNo!,2)
//		if li_re =2 then
//			cbx_extract.checked = false
//		end if
//	end if
//else
//	if li_count<= 0 or IsNull( li_count ) then
//		messagebox('Producer Monthly Reports',&
//			'~nData for this month has not been extracted'+&
//			'~nYou must extract data first')
//		this.checked =true
//	end if
//end if
//			
end event

type dw_rpt_for_cntrmed from u_pics_dw within w_response_monthly_reports
integer x = 1138
integer y = 688
integer width = 256
integer height = 88
integer taborder = 70
string dataobject = "d_rpt_for_cntrmed"
boolean vscrollbar = false
boolean livescroll = false
end type

event itemchanged;call super::itemchanged;
string ls_prdr, ls_cntrmed, ls_qastat, ls_prdstat, ls_prd_commts, ls_qa_commts,&
		ls_month, ls_lang
DatawindowChild dwc_cntrmed
date ld_start_date, ld_end_date
datetime ld_stdt, ld_enddt
int li_yr

ls_cntrmed= data
//dw_rpt_for_cntrmed.SetTransObject( SqlServerTrans )
//dw_rpt_for_cntrmed.GetChild('cntrmed', dwc_cntrmed)
//dwc_cntrmed.SetTransObject( SqlServerTrans )
//dwc_cntrmed.Retrieve(ls_prdr)
//dw_rpt_for_cntrmed.Retrieve(ls_prdr)
li_yr =dw_cntrfy_one.GetItemNumber(1,'cntrfy')
//ls_cntrmed=dw_rpt_for_cntrmed.GetItemString(1,'cntrmed')
ls_prdr=dw_one_prdr.GetItemString(1,'prdr')
ls_month=ddlb_month_one.text
choose case ls_month
	case 'January'
		ld_start_date =date('01/01/'+string(li_yr))
		ld_end_date =date('01/31/'+string(li_yr))
	case 'February'
		ld_start_date =date('02/01/'+string(li_yr))
		if mod(li_yr,4 ) =0 then
			ld_end_date =date('02/29/'+string(li_yr))
		else
			ld_end_date =date('02/28/'+string(li_yr))
		end if
	case 'March'
		ld_start_date =date('03/01/'+string(li_yr))
		ld_end_date =date('03/31/'+string(li_yr))
	case 'April'
		ld_start_date =date('04/01/'+string(li_yr))
		ld_end_date =date('04/30/'+string(li_yr))
	case 'May'
		ld_start_date =date('05/01/'+string(li_yr))
		ld_end_date =date('05/31/'+string(li_yr))
	case 'June'
		ld_start_date =date('06/01/'+string(li_yr))
		ld_end_date =date('06/30/'+string(li_yr))
	case 'July'
		ld_start_date =date('07/01/'+string(li_yr))
		ld_end_date =date('07/31/'+string(li_yr))
	case 'August'
		ld_start_date =date('08/01/'+string(li_yr))
		ld_end_date =date('08/31/'+string(li_yr))
	case 'September'
		ld_start_date =date('09/01/'+string(li_yr))
		ld_end_date =date('09/30/'+string(li_yr))
	case 'October'
		ld_start_date =date('10/01/'+string(li_yr))
		ld_end_date =date('10/31/'+string(li_yr))
	case 'November'
		ld_start_date =date('11/01/'+string(li_yr ))
		ld_end_date =date('11/30/'+string(li_yr ))
	case 'December'
		ld_start_date =date('12/01/'+string(li_yr ))
		ld_end_date =date('12/31/'+string(li_yr ))
end choose
ld_stdt=datetime(ld_start_date,time('00:00:00'))
ld_enddt=datetime(ld_end_date,time('00:00:00'))
if ls_prdr='PTB' AND ls_cntrmed='FL' then
	ls_cntrmed='RTB'
	ls_lang='Y'
elseif ls_prdr='PTB' AND ls_cntrmed='RC' then
	ls_lang='N'
end if
if ls_prdr<>'PTB' then
	select prd_commts, qa_commts,prdstat,qastat into :ls_prd_commts, :ls_qa_commts,
			:ls_prdstat, :ls_qastat
	from monrpt
	where start_date=:ld_stdt and end_date=:ld_enddt and prdr=:ls_prdr and
			cntrmed=:ls_cntrmed
	using SqlServerTrans;
elseif ls_prdr='PTB' then
	select prd_commts, qa_commts,prdstat,qastat into :ls_prd_commts, :ls_qa_commts,
			:ls_prdstat, :ls_qastat
	from monrpt
	where start_date=:ld_stdt and end_date=:ld_enddt and prdr=:ls_prdr and
			cntrmed=:ls_cntrmed and foreign_lang=:ls_lang
	using SqlServerTrans;
end if
mle_prd.text= ls_prd_commts
mle_qa.text=ls_qa_commts
if ls_prdstat='Y' then
	rb_satis_prd.checked=true
else
	rb_unsatis_prd.checked=true
end if
if ls_qastat='Y' then
	rb_satisqa.checked=true
else
	rb_unsatisqa.checked=true
end if
cb_save.enabled=true









end event

type st_med from statictext within w_response_monthly_reports
integer x = 805
integer y = 696
integer width = 229
integer height = 56
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 33554432
long backcolor = 80263581
string text = "Media:"
boolean focusrectangle = false
end type

type cbx_one_prdr from u_cbx within w_response_monthly_reports
integer x = 137
integer y = 480
integer width = 1911
integer height = 68
integer taborder = 50
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
string text = "Regenerate/Print for specific Producer"
end type

event clicked;call super::clicked;string ls_prdr, ls_cntrmed, ls_qastat, ls_prdstat, ls_prd_commts, ls_qa_commts,&
		ls_month, ls_yr, ls_start_date, ls_end_date, ls_m, ls_preyr
DatawindowChild dwc_cntrmed
date ld_start_date, ld_end_date
datetime ld_stdt, ld_enddt
int li_yr, li_m, li_mold

if cbx_one_prdr.checked then
	cbx_back.visible=true
	cb_save.visible=true
	cbx_all_prdr.checked=false
	cbx_backall.checked=false
	cb_cancel.visible=true
	cb_ok.visible=true
	cb_ok.text='Save/&View'
	cbx_all_prdr.visible=false
	cbx_all_prdr_incld.visible=false
	cbx_one_prdr.visible=true
	cbx_one_prdr_incld.visible=true
	cbx_regen.visible=true
	ddlb_month_all.visible=false
	ddlb_month_one.visible=true
	dw_cntrfy_all.visible=false
	dw_cntrfy_one.visible=true
	dw_one_prdr.visible=true
	dw_rpt_for_cntrmed.visible=true
	mle_prd.visible=true
	mle_qa.visible=false
//	mle_prd.taborder=130
//	mle_qa.taborder=150
	r_all_prdr.visible=false
	r_one_prdr.visible=true
	gb_pd.visible=true
	gb_qa.visible=true
	rb_satis_prd.visible=true
	rb_satisqa.visible=true
	rb_unsatis_prd.visible=true
	rb_unsatisqa.visible=true
	st_med.visible=true
	st_mon_all.visible=false
	st_mon_one.visible=true
	st_one_prdr.visible=true
	st_prd_commts.visible=true
	st_prdr_status.visible=true
	st_qa_commts.visible=false
	st_qa_status.visible=true
	st_yr_all.visible=false
	st_yr_one.visible=true
	
	li_yr =dw_cntrfy_one.GetItemNumber(1,'cntrfy')
	//li_yr = integer(ls_yr)
	ls_cntrmed=dw_rpt_for_cntrmed.GetItemString(1,'cntrmed')
	ls_prdr=dw_one_prdr.GetItemString(1,'prdr')
	ls_month=ddlb_month_one.text
	choose case ls_month
		case 'January'
			ld_start_date =date('01/01/'+string(li_yr))
			ld_end_date =date('01/31/'+string(li_yr))
		case 'February'
			ld_start_date =date('02/01/'+string(li_yr))
			if mod(li_yr,4 ) =0 then
				ld_end_date =date('02/29/'+string(li_yr))
			else
				ld_end_date =date('02/28/'+string(li_yr))
			end if
		case 'March'
			ld_start_date =date('03/01/'+string(li_yr))
			ld_end_date =date('03/31/'+string(li_yr))
		case 'April'
			ld_start_date =date('04/01/'+string(li_yr))
			ld_end_date =date('04/30/'+string(li_yr))
		case 'May'
			ld_start_date =date('05/01/'+string(li_yr))
			ld_end_date =date('05/31/'+string(li_yr))
		case 'June'
			ld_start_date =date('06/01/'+string(li_yr))
			ld_end_date =date('06/30/'+string(li_yr))
		case 'July'
			ld_start_date =date('07/01/'+string(li_yr))
			ld_end_date =date('07/31/'+string(li_yr))
		case 'August'
			ld_start_date =date('08/01/'+string(li_yr))
			ld_end_date =date('08/31/'+string(li_yr))
		case 'September'
			ld_start_date =date('09/01/'+string(li_yr))
			ld_end_date =date('09/30/'+string(li_yr))
		case 'October'
			ld_start_date =date('10/01/'+string(li_yr))
			ld_end_date =date('10/31/'+string(li_yr))
		case 'November'
			ld_start_date =date('11/01/'+string(li_yr ))
			ld_end_date =date('11/30/'+string(li_yr ))
		case 'December'
			ld_start_date =date('12/01/'+string(li_yr ))
			ld_end_date =date('12/31/'+string(li_yr ))
	end choose
	select prd_commts, qa_commts,prdstat,qastat into :ls_prd_commts, :ls_qa_commts,
			:ls_prdstat, :ls_qastat
	from monrpt
	where start_date=:ld_start_date and end_date=:ld_end_date and prdr=:ls_prdr and
			cntrmed=:ls_cntrmed
	using SqlServerTrans;
	mle_prd.text= ls_prd_commts
	mle_qa.text=ls_qa_commts
	if ls_prdstat='Y' then
		rb_satis_prd.checked=true
	else
		rb_unsatis_prd.checked=true
	end if
	if ls_qastat='Y' then
		rb_satisqa.checked=true
	else
		rb_unsatisqa.checked=true
	end if
	
	ls_start_date=string(ld_start_date,'mm/dd/yyyy')
	ls_end_date=string(ld_end_date,'mm/dd/yyyy')
	ls_m=mid(ls_start_date,1, 2)
	ls_yr=mid(ls_start_date,7, 4)
	st_all.visible=false
	st_back.visible=true
	if ls_m>='10' and cbx_back.checked=false then
		st_back.text='This choice will produce report for one producer from 10/01/'+ls_yr+&
		' to '+ls_end_date
	elseif ls_m<'10' and cbx_back.checked=false then
		ls_preyr=string(li_yr -1 )
		st_back.text='This choice will produce report for one producer from 10/01/'+ls_preyr+&
		' to '+ls_end_date
	elseif cbx_back.checked=true then
		li_m=long(ls_m)
		li_mold=li_m
		li_m++
		if li_m >12 then
			ls_start_date='01/01/'+ls_yr
			ls_end_date='12/31/'+ls_yr
		elseif li_m<= 12 then
			ls_start_date=string(li_mold,'00')+'/01/'+string(li_yr - 1)
			ls_end_date=ls_end_date
		end if
		st_back.text='This choice will produce report for one producer from '+ls_start_date+&
		' to '+ls_end_date
	end if
elseif cbx_one_prdr.checked=false then
	cbx_back.visible=false
	cbx_all_prdr.checked=true
	cb_cancel.visible=true
	cb_ok.visible=true
	cb_ok.text='&OK'
	cb_save.visiblE=false
	cbx_all_prdr.visible=true
	cbx_all_prdr_incld.visible=true
	cbx_backall.visible=true
	cbx_one_prdr.visible=false
	cbx_one_prdr_incld.visible=false
	cbx_regen.visible=false
	ddlb_month_all.visible=true
	ddlb_month_one.visible=false
	dw_cntrfy_all.visible=true
	dw_cntrfy_one.visible=false
	dw_one_prdr.visible=false
	dw_rpt_for_cntrmed.visible=false
	mle_prd.visible=false
	mle_qa.visible=false
	r_all_prdr.visible=true
	r_one_prdr.visible=false
	gb_pd.visible=false
	gb_qa.visible=false
	rb_satis_prd.visible=false
	rb_satisqa.visible=false
	rb_unsatis_prd.visible=false
	rb_unsatisqa.visible=false
	st_med.visible=false
	st_mon_all.visible=true
	st_mon_one.visible=false
	st_one_prdr.visible=false
	st_prd_commts.visible=false
	st_prdr_status.visible=false
	st_qa_commts.visible=false
	st_qa_status.visible=false
	st_yr_all.visible=true
	st_yr_one.visible=false
	li_yr =dw_cntrfy_all.GetItemNumber(1,'cntrfy')
	//li_yr = integer(ls_yr)
	ls_month=ddlb_month_all.text
	choose case ls_month
		case 'January'
			ld_start_date =date('01/01/'+string(li_yr))
			ld_end_date =date('01/31/'+string(li_yr))
		case 'February'
			ld_start_date =date('02/01/'+string(li_yr))
			if mod(li_yr,4 ) =0 then
				ld_end_date =date('02/29/'+string(li_yr))
			else
				ld_end_date =date('02/28/'+string(li_yr))
			end if
		case 'March'
			ld_start_date =date('03/01/'+string(li_yr))
			ld_end_date =date('03/31/'+string(li_yr))
		case 'April'
			ld_start_date =date('04/01/'+string(li_yr))
			ld_end_date =date('04/30/'+string(li_yr))
		case 'May'
			ld_start_date =date('05/01/'+string(li_yr))
			ld_end_date =date('05/31/'+string(li_yr))
		case 'June'
			ld_start_date =date('06/01/'+string(li_yr))
			ld_end_date =date('06/30/'+string(li_yr))
		case 'July'
			ld_start_date =date('07/01/'+string(li_yr))
			ld_end_date =date('07/31/'+string(li_yr))
		case 'August'
			ld_start_date =date('08/01/'+string(li_yr))
			ld_end_date =date('08/31/'+string(li_yr))
		case 'September'
			ld_start_date =date('09/01/'+string(li_yr))
			ld_end_date =date('09/30/'+string(li_yr))
		case 'October'
			ld_start_date =date('10/01/'+string(li_yr))
			ld_end_date =date('10/31/'+string(li_yr))
		case 'November'
			ld_start_date =date('11/01/'+string(li_yr ))
			ld_end_date =date('11/30/'+string(li_yr ))
		case 'December'
			ld_start_date =date('12/01/'+string(li_yr ))
			ld_end_date =date('12/31/'+string(li_yr ))
	end choose
	ls_start_date=string(ld_start_date,'mm/dd/yyyy')
	ls_end_date=string(ld_end_date,'mm/dd/yyyy')
	ls_m=mid(ls_start_date,1, 2)
	ls_yr=mid(ls_start_date,7, 4)
	st_all.visible=true
	st_back.visible=false
	if ls_m>='10' and cbx_backall.checked =false then
		st_all.visible=true
		st_all.text='This choice will produce report for all producer from 10/01/'+ls_yr+&
		' to '+ls_end_date
	elseif ls_m<'10' and cbx_backall.checked =false then
		ls_preyr=string(li_yr -1 )
		st_all.text='This choice will produce report for all producer from 10/01/'+ls_preyr+&
		' to '+ls_end_date
	elseif cbx_backall.checked =true then
		li_m=long(ls_m)
		li_mold=li_m
		li_m++
		if li_m >12 then
			ls_start_date='01/01/'+ls_yr
			ls_end_date='12/31/'+ls_yr
		elseif li_m<= 12 then
			ls_start_date=string(li_m,'00')+'/01/'+string(li_yr - 1)
			ls_end_date=ls_end_date
		end if
		st_all.text='This choice will produce report for all producer from '+ls_start_date+&
		' to '+ls_end_date
	end if
end if

end event

type mle_qa from u_mle within w_response_monthly_reports
integer x = 585
integer y = 1496
integer width = 1335
integer height = 172
integer taborder = 170
end type

event modified;call super::modified;cb_save.enabled=true
end event

event getfocus;call super::getfocus;long li_yr ,li_count, li_re, li_start_month,li_fy
string ls_prdr, ls_month, ls_yr, ls_start_date, ls_like, ls_num_month, ls_yr2,&
		ls_oneprdr,ls_cntrmed,ls_incldavg, ls_regen,ls_prd_commts, ls_qa_commts,&
		ls_qastat, ls_prdstat,ls_cntrlc,ls_done,ls_max, ls_min,ls_med,ls_forceregen
str_voucher_report lstr
date ld_start_date, ld_end_date, ld_fy_start_date, ld_fy_end_date
boolean lb_extract_checked 
datetime ld_stdt, ld_enddt

ls_prdr =dw_one_prdr.GetItemString( 1,'prdr')//if all prdr choose first prdr to make
ls_cntrmed=dw_rpt_for_cntrmed.GetItemString(1,'cntrmed')//out put show instead plank paper
ls_prdr= trim(ls_prdr)
ls_cntrmed= trim(ls_cntrmed)
ls_regen='N'
if cbx_one_prdr.checked then
	li_yr =dw_cntrfy_one.GetItemNumber(1, 'cntrfy')
	if IsNull( li_yr) then
		messagebox('','You must choose a calendar year',Exclamation!)
		return
	end if
	ls_yr = string( li_yr )
	ls_month =ddlb_month_one.text
end if
choose case ls_month
	case 'January'
		ld_start_date =date('01/01/'+string(li_yr))
		ld_end_date =date('01/31/'+string(li_yr))
	case 'February'
		ld_start_date =date('02/01/'+string(li_yr))
		if mod(li_yr,4 ) =0 then
			ld_end_date =date('02/29/'+string(li_yr))
		else
			ld_end_date =date('02/28/'+string(li_yr))
		end if
	case 'March'
		ld_start_date =date('03/01/'+string(li_yr))
		ld_end_date =date('03/31/'+string(li_yr))
	case 'April'
		ld_start_date =date('04/01/'+string(li_yr))
		ld_end_date =date('04/30/'+string(li_yr))
	case 'May'
		ld_start_date =date('05/01/'+string(li_yr))
		ld_end_date =date('05/31/'+string(li_yr))
	case 'June'
		ld_start_date =date('06/01/'+string(li_yr))
		ld_end_date =date('06/30/'+string(li_yr))
	case 'July'
		ld_start_date =date('07/01/'+string(li_yr))
		ld_end_date =date('07/31/'+string(li_yr))
	case 'August'
		ld_start_date =date('08/01/'+string(li_yr))
		ld_end_date =date('08/31/'+string(li_yr))
	case 'September'
		ld_start_date =date('09/01/'+string(li_yr))
		ld_end_date =date('09/30/'+string(li_yr))
	case 'October'
		ld_start_date =date('10/01/'+string(li_yr))
		ld_end_date =date('10/31/'+string(li_yr))
	case 'November'
		ld_start_date =date('11/01/'+string(li_yr ))
		ld_end_date =date('11/30/'+string(li_yr ))
	case 'December'
		ld_start_date =date('12/01/'+string(li_yr ))
		ld_end_date =date('12/31/'+string(li_yr ))
end choose
ld_stdt=datetime(ld_start_date,time('00:00:00'))
if cbx_regen.checked then
	ls_regen='Y'
end if
if cbx_one_prdr.checked then
	select max(done), min(done) into :ls_max,  :ls_min
	from monrpt
	where prdr=:ls_prdr and cntrmed=:ls_cntrmed and start_date=:ld_stdt
	using SqlServerTrans;
	if not f_check_dberror(SqlServerTrans, 'select max and min of done from monrpt') then
		return
	else
		if (ls_min='N'  or isnull(ls_min)) and ls_regen='N' then
			messagebox('Warnning','The monthly report data for producer '+ls_prdr+&
				'~n'+'for the month of '+ls_month+', '+ls_yr+' has not yet been generated. '+&
				'~nYou need to generate the monthly report data '+&
				'~nbefore you enter comments.')
			rb_satisqa.SetFocus()
		end if
	end if
end if

end event

type mle_prd from u_mle within w_response_monthly_reports
integer x = 585
integer y = 1292
integer width = 1335
integer height = 172
integer taborder = 160
end type

event modified;call super::modified;cb_save.enabled=true
end event

event getfocus;call super::getfocus;long li_yr ,li_count, li_re, li_start_month,li_fy
string ls_prdr, ls_month, ls_yr, ls_start_date, ls_like, ls_num_month, ls_yr2,&
		ls_oneprdr,ls_cntrmed,ls_incldavg, ls_regen,ls_prd_commts, ls_qa_commts,&
		ls_qastat, ls_prdstat,ls_cntrlc,ls_done,ls_max, ls_min,ls_med,ls_forceregen,&
		ls_lang
str_voucher_report lstr
date ld_start_date, ld_end_date, ld_fy_start_date, ld_fy_end_date
boolean lb_extract_checked 
datetime ld_stdt

ls_prdr =dw_one_prdr.GetItemString( 1,'prdr')//if all prdr choose first prdr to make
ls_cntrmed=dw_rpt_for_cntrmed.GetItemString(1,'cntrmed')//out put show instead plank paper
ls_prdr= trim(ls_prdr)
ls_cntrmed= trim(ls_cntrmed)
ls_regen='N'
if cbx_one_prdr.checked then
	li_yr =dw_cntrfy_one.GetItemNumber(1, 'cntrfy')
	if IsNull( li_yr) then
		messagebox('','You must choose a calendar year',Exclamation!)
		return
	end if
	ls_yr = string( li_yr )
	ls_month =ddlb_month_one.text
end if
choose case ls_month
	case 'January'
		ld_start_date =date('01/01/'+string(li_yr))
		ld_end_date =date('01/31/'+string(li_yr))
	case 'February'
		ld_start_date =date('02/01/'+string(li_yr))
		if mod(li_yr,4 ) =0 then
			ld_end_date =date('02/29/'+string(li_yr))
		else
			ld_end_date =date('02/28/'+string(li_yr))
		end if
	case 'March'
		ld_start_date =date('03/01/'+string(li_yr))
		ld_end_date =date('03/31/'+string(li_yr))
	case 'April'
		ld_start_date =date('04/01/'+string(li_yr))
		ld_end_date =date('04/30/'+string(li_yr))
	case 'May'
		ld_start_date =date('05/01/'+string(li_yr))
		ld_end_date =date('05/31/'+string(li_yr))
	case 'June'
		ld_start_date =date('06/01/'+string(li_yr))
		ld_end_date =date('06/30/'+string(li_yr))
	case 'July'
		ld_start_date =date('07/01/'+string(li_yr))
		ld_end_date =date('07/31/'+string(li_yr))
	case 'August'
		ld_start_date =date('08/01/'+string(li_yr))
		ld_end_date =date('08/31/'+string(li_yr))
	case 'September'
		ld_start_date =date('09/01/'+string(li_yr))
		ld_end_date =date('09/30/'+string(li_yr))
	case 'October'
		ld_start_date =date('10/01/'+string(li_yr))
		ld_end_date =date('10/31/'+string(li_yr))
	case 'November'
		ld_start_date =date('11/01/'+string(li_yr ))
		ld_end_date =date('11/30/'+string(li_yr ))
	case 'December'
		ld_start_date =date('12/01/'+string(li_yr ))
		ld_end_date =date('12/31/'+string(li_yr ))
end choose
ld_stdt=datetime(ld_start_date,time('00:00:00'))
if cbx_regen.checked=true then
	ls_regen='Y'
end if

if cbx_one_prdr.checked then
	if ls_prdr<>'PTB' THEN
		select max(done), min(done) into :ls_max,  :ls_min
		from monrpt
		where prdr=:ls_prdr and cntrmed=:ls_cntrmed and start_date=:ld_stdt
		using SqlServerTrans;
	ELSEIF ls_prdr='PTB' AND ls_cntrmed='FL' THEN
		ls_cntrmed='RTB'
		ls_lang='Y'
	elseif ls_prdr='PTB' AND (ls_cntrmed='RC' OR  ls_cntrmed='RTB') then
		ls_lang='N'
	end if
	if ls_prdr='PTB' THEN
		select max(done), min(done) into :ls_max,  :ls_min
		from monrpt
		where prdr=:ls_prdr and cntrmed=:ls_cntrmed and start_date=:ld_stdt AND
		      foreign_lang=:ls_lang
		using SqlServerTrans;
	END IF
	if not f_check_dberror(SqlServerTrans, 'select max and min of done from monrpt') then
		return
	else
		if (ls_min='N'  or isnull(ls_min)) and ls_regen='N' then
			messagebox('Warnning','The monthly report data for producer '+ls_prdr+&
				'~n'+'for the month of '+ls_month+', '+ls_yr+' has not yet been generated. '+&
				'~nYou need to generate the monthly report data '+&
				'~nbefore you enter comments.')
			rb_satisqa.SetFocus()
		end if
	end if
end if

end event

type st_qa_commts from statictext within w_response_monthly_reports
integer x = 101
integer y = 1492
integer width = 375
integer height = 172
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 33554432
long backcolor = 80263581
string text = "Quality Assurance Comments:"
boolean focusrectangle = false
end type

type st_prd_commts from statictext within w_response_monthly_reports
integer x = 101
integer y = 1292
integer width = 375
integer height = 168
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 33554432
long backcolor = 80263581
string text = "Production Control Comments:"
boolean focusrectangle = false
end type

type r_one_prdr from rectangle within w_response_monthly_reports
integer linethickness = 1
long fillcolor = 12632256
integer x = 59
integer y = 1012
integer width = 1920
integer height = 708
end type

type rb_unsatisqa from u_rb within w_response_monthly_reports
integer x = 1147
integer y = 1160
integer width = 416
integer height = 68
integer taborder = 150
string text = "Unsatisfactory"
end type

type rb_satisqa from u_rb within w_response_monthly_reports
integer x = 1147
integer y = 1092
integer width = 416
integer height = 68
integer taborder = 140
string text = "Satisfactory"
end type

type rb_unsatis_prd from u_rb within w_response_monthly_reports
integer x = 210
integer y = 1160
integer width = 416
integer height = 68
integer taborder = 130
string text = "Unsatisfactory"
end type

type rb_satis_prd from u_rb within w_response_monthly_reports
integer x = 215
integer y = 1092
integer width = 416
integer height = 88
integer taborder = 120
string text = "Satisfactory"
end type

type st_prdr_status from statictext within w_response_monthly_reports
integer x = 219
integer y = 1040
integer width = 571
integer height = 52
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 33554432
long backcolor = 80263581
string text = "Delivery Performance"
boolean focusrectangle = false
end type

type st_mon_one from statictext within w_response_monthly_reports
integer x = 1257
integer y = 588
integer width = 242
integer height = 56
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 33554432
long backcolor = 80263581
string text = "Month:"
boolean focusrectangle = false
end type

type st_yr_one from statictext within w_response_monthly_reports
integer x = 59
integer y = 580
integer width = 567
integer height = 48
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 33554432
long backcolor = 80263581
string text = "Calendar Year:"
alignment alignment = right!
boolean focusrectangle = false
end type

type cbx_regen from u_cbx within w_response_monthly_reports
integer x = 1499
integer y = 692
integer width = 421
integer height = 68
integer taborder = 80
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
string text = "Regenerate"
end type

event clicked;call super::clicked;//long li_fy
//
//li_fy =dw_cntrfy.GetItemNumber( 1, 'cntrfy')
//if Isnull(li_fy ) then
//	messagebox('','You must choose the year from drop down datawindow')
//	return
//end if
end event

type ddlb_month_one from u_ddlb within w_response_monthly_reports
integer x = 1573
integer y = 576
integer width = 357
integer height = 640
integer taborder = 100
boolean sorted = false
string item[] = {"January","February","March","April","May","June","July","August","September","October","November","December"}
end type

event selectionchanged;call super::selectionchanged;
string ls_prdr, ls_cntrmed, ls_qastat, ls_prdstat, ls_prd_commts, ls_qa_commts,&
		ls_month, ls_yr, ls_start_date, ls_end_date, ls_m, ls_preyr, ls_mold
DatawindowChild dwc_cntrmed,dwc_prdr
date ld_start_date, ld_end_date,ld_before_start30
int li_yr, li_m, li_mold
datetime ld_stdt, ld_enddt, ld_bef_30dt

li_yr =dw_cntrfy_one.GetItemNumber(1,'cntrfy')
ls_month=ddlb_month_one.text
choose case ls_month
	case 'January'
		ld_start_date =date('01/01/'+string(li_yr))
		ld_end_date =date('01/31/'+string(li_yr))
	case 'February'
		ld_start_date =date('02/01/'+string(li_yr))
		if mod(li_yr,4 ) =0 then
			ld_end_date =date('02/29/'+string(li_yr))
		else
			ld_end_date =date('02/28/'+string(li_yr))
		end if
	case 'March'
		ld_start_date =date('03/01/'+string(li_yr))
		ld_end_date =date('03/31/'+string(li_yr))
	case 'April'
		ld_start_date =date('04/01/'+string(li_yr))
		ld_end_date =date('04/30/'+string(li_yr))
	case 'May'
		ld_start_date =date('05/01/'+string(li_yr))
		ld_end_date =date('05/31/'+string(li_yr))
	case 'June'
		ld_start_date =date('06/01/'+string(li_yr))
		ld_end_date =date('06/30/'+string(li_yr))
	case 'July'
		ld_start_date =date('07/01/'+string(li_yr))
		ld_end_date =date('07/31/'+string(li_yr))
	case 'August'
		ld_start_date =date('08/01/'+string(li_yr))
		ld_end_date =date('08/31/'+string(li_yr))
	case 'September'
		ld_start_date =date('09/01/'+string(li_yr))
		ld_end_date =date('09/30/'+string(li_yr))
	case 'October'
		ld_start_date =date('10/01/'+string(li_yr))
		ld_end_date =date('10/31/'+string(li_yr))
	case 'November'
		ld_start_date =date('11/01/'+string(li_yr ))
		ld_end_date =date('11/30/'+string(li_yr ))
	case 'December'
		ld_start_date =date('12/01/'+string(li_yr ))
		ld_end_date =date('12/31/'+string(li_yr ))
end choose
ld_stdt=datetime(ld_start_date,time('00:00:00'))
ld_enddt=datetime(ld_end_date,time('00:00:00'))
ld_before_start30= RelativeDate(ld_start_date, - 30 )
ld_bef_30dt=datetime(ld_before_start30,time('00:00:00'))
dw_one_prdr.SetTransObject( SqlServerTrans )
dw_one_prdr.GetChild('prdr', dwc_prdr)
dwc_prdr.SetTransObject( SqlServerTrans )
dwc_prdr.Retrieve(ld_bef_30dt)
dw_one_prdr.Retrieve(ld_bef_30dt)
dw_one_prdr.InsertRow(0)
ls_prdr=dwc_prdr.GetItemString(1,'prdr')
dw_one_prdr.SetItem(1,'prdr',ls_prdr)

dw_rpt_for_cntrmed.GetChild('cntrmed', dwc_cntrmed)
dwc_cntrmed.SetTransObject( SqlServerTrans )
dwc_cntrmed.Retrieve(ld_bef_30dt,ls_prdr)
ls_cntrmed =dwc_cntrmed.GetItemString(1,'cntrmed')
dw_rpt_for_cntrmed.Retrieve(ld_bef_30dt,ls_prdr)
dw_rpt_for_cntrmed.InsertRow(0)
dw_rpt_for_cntrmed.SetItem(1,'cntrmed', ls_cntrmed)
select prd_commts, qa_commts,prdstat,qastat into :ls_prd_commts, :ls_qa_commts,
		:ls_prdstat, :ls_qastat
from monrpt
where start_date=:ld_stdt and end_date=:ld_enddt and prdr=:ls_prdr and
		cntrmed=:ls_cntrmed
using SqlServerTrans;
mle_prd.text= ls_prd_commts
mle_qa.text=ls_qa_commts
if ls_prdstat='Y' then
	rb_satis_prd.checked=true
else
	rb_unsatis_prd.checked=true
end if
if ls_qastat='Y' then
	rb_satisqa.checked=true
else
	rb_unsatisqa.checked=true
end if

ls_start_date=string(ld_start_date,'mm/dd/yyyy')
ls_end_date=string(ld_end_date,'mm/dd/yyyy')
ls_m=mid(ls_start_date,1, 2)
ls_yr=mid(ls_start_date,7, 4)
st_all.visible=false
st_back.visible=true

ls_preyr=string(li_yr - 1 )

li_m=long(ls_m)
li_mold=li_m
ls_mold=string(li_mold,'00')
if cbx_back.checked then
	cbx_regen.checked=false		
	li_m++
	ls_m=string(li_m,'00')
	if li_m>12 then
		ls_start_date='01/01/'+ls_yr
		ls_end_date='12/31/'+ls_yr
		st_back.text='This choice will produce report for one producer that choosen from '+ls_start_date+&
		' to '+ls_end_date
	else
		st_back.text='This choice will produce report for one producer that choosen from '+ls_m+'/01/'+ls_preyr+&
		' to '+ls_end_date
	end if
else
	if ls_mold>='10' then
		st_back.text='This choice will produce report for one producer that choosen from 10/01/'+ls_yr+&
		' to '+ls_end_date
	else
		st_back.text='This choice will produce report for one producer that choosen from 10/01/'+ls_preyr+&
		' to '+ls_end_date
	end if
end if










end event

type dw_cntrfy_one from u_pics_dw within w_response_monthly_reports
integer x = 690
integer y = 576
integer width = 357
integer height = 88
integer taborder = 90
string dataobject = "d_month_for_dddw_cntrfy"
boolean vscrollbar = false
boolean livescroll = false
end type

event itemchanged;call super::itemchanged;
string ls_prdr, ls_cntrmed, ls_qastat, ls_prdstat, ls_prd_commts, ls_qa_commts,&
		ls_month, ls_yr
DatawindowChild dwc_cntrmed, dwc_prdr
date ld_start_date, ld_end_date,ld_before_start30
int li_yr
datetime ld_stdt, ld_enddt, ld_bef_30dt

ls_yr= data
li_yr = integer(ls_yr)
ls_month=ddlb_month_one.text
choose case ls_month
	case 'January'
		ld_start_date =date('01/01/'+string(li_yr))
		ld_end_date =date('01/31/'+string(li_yr))
	case 'February'
		ld_start_date =date('02/01/'+string(li_yr))
		if mod(li_yr,4 ) =0 then
			ld_end_date =date('02/29/'+string(li_yr))
		else
			ld_end_date =date('02/28/'+string(li_yr))
		end if
	case 'March'
		ld_start_date =date('03/01/'+string(li_yr))
		ld_end_date =date('03/31/'+string(li_yr))
	case 'April'
		ld_start_date =date('04/01/'+string(li_yr))
		ld_end_date =date('04/30/'+string(li_yr))
	case 'May'
		ld_start_date =date('05/01/'+string(li_yr))
		ld_end_date =date('05/31/'+string(li_yr))
	case 'June'
		ld_start_date =date('06/01/'+string(li_yr))
		ld_end_date =date('06/30/'+string(li_yr))
	case 'July'
		ld_start_date =date('07/01/'+string(li_yr))
		ld_end_date =date('07/31/'+string(li_yr))
	case 'August'
		ld_start_date =date('08/01/'+string(li_yr))
		ld_end_date =date('08/31/'+string(li_yr))
	case 'September'
		ld_start_date =date('09/01/'+string(li_yr))
		ld_end_date =date('09/30/'+string(li_yr))
	case 'October'
		ld_start_date =date('10/01/'+string(li_yr))
		ld_end_date =date('10/31/'+string(li_yr))
	case 'November'
		ld_start_date =date('11/01/'+string(li_yr ))
		ld_end_date =date('11/30/'+string(li_yr ))
	case 'December'
		ld_start_date =date('12/01/'+string(li_yr ))
		ld_end_date =date('12/31/'+string(li_yr ))
end choose
ld_before_start30= RelativeDate(ld_start_date, - 30 )
ld_stdt=datetime(ld_start_date,time('00:00:00'))
ld_enddt=datetime(ld_end_date,time('00:00:00'))
ld_bef_30dt=datetime(ld_before_start30,time('00:00:00'))
dw_one_prdr.SetTransObject( SqlServerTrans )
dw_one_prdr.GetChild('prdr', dwc_prdr)
dwc_prdr.SetTransObject( SqlServerTrans )
dwc_prdr.Retrieve(ld_bef_30dt)
dw_one_prdr.Retrieve(ld_bef_30dt)
dw_one_prdr.InsertRow(0)
ls_prdr=dwc_prdr.GetItemString(1,'prdr')
dw_one_prdr.SetItem(1,'prdr',ls_prdr)

dw_rpt_for_cntrmed.GetChild('cntrmed', dwc_cntrmed)
dwc_cntrmed.SetTransObject( SqlServerTrans )
dwc_cntrmed.Retrieve(ld_bef_30dt,ls_prdr)
ls_cntrmed =dwc_cntrmed.GetItemString(1,'cntrmed')
dw_rpt_for_cntrmed.Retrieve(ld_bef_30dt,ls_prdr)
dw_rpt_for_cntrmed.InsertRow(0)
dw_rpt_for_cntrmed.SetItem(1,'cntrmed', ls_cntrmed)
select prd_commts, qa_commts,prdstat,qastat into :ls_prd_commts, :ls_qa_commts,
		:ls_prdstat, :ls_qastat
from monrpt
where start_date=:ld_stdt and end_date=:ld_enddt and prdr=:ls_prdr and
		cntrmed=:ls_cntrmed
using SqlServerTrans;
mle_prd.text= ls_prd_commts
mle_qa.text=ls_qa_commts
if ls_prdstat='Y' then
	rb_satis_prd.checked=true
else
	rb_unsatis_prd.checked=true
end if
if ls_qastat='Y' then
	rb_satisqa.checked=true
else
	rb_unsatisqa.checked=true
end if










end event

type cbx_all_prdr from u_cbx within w_response_monthly_reports
integer x = 137
integer y = 20
integer width = 1394
integer height = 68
integer taborder = 10
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
string text = "Generate/Print for all Producers"
end type

event clicked;call super::clicked;string ls_prdr, ls_cntrmed, ls_qastat, ls_prdstat, ls_prd_commts, ls_qa_commts,&
		ls_month, ls_yr, ls_preyr, ls_start_date, ls_end_date, ls_m, ls_back='N'
DatawindowChild dwc_cntrmed
date ld_start_date, ld_end_date
datetime ld_stdt, ld_enddt
int li_yr, li_m, li_mold

if cbx_all_prdr.checked then
	cbx_back.visible=false
	cb_save.visible=false
	cbx_one_prdr.checked=false
	cb_cancel.visible=true
	cb_ok.visible=true
	cb_ok.text='&OK'
	cbx_all_prdr.visible=true
	cbx_all_prdr_incld.visible=true
	cbx_backall.visible=true
	if cbx_backall.checked then
		ls_back='Y'
	end if
	cbx_one_prdr.visible=false
	cbx_one_prdr_incld.visible=false
	cbx_regen.visible=false
	ddlb_month_all.visible=true
	ddlb_month_one.visible=false
	dw_cntrfy_all.visible=true
	dw_cntrfy_one.visible=false
	dw_one_prdr.visible=false
	dw_rpt_for_cntrmed.visible=false
	mle_prd.visible=false
	mle_qa.visible=false
	r_all_prdr.visible=true
	r_one_prdr.visible=false
	gb_pd.visible=false
	gb_qa.visible=false
	rb_satis_prd.visible=false
	rb_satisqa.visible=false
	rb_unsatis_prd.visible=false
	rb_unsatisqa.visible=false
	st_med.visible=false
	st_mon_all.visible=true
	st_mon_one.visible=false
	st_one_prdr.visible=false
	st_prd_commts.visible=false
	st_prdr_status.visible=false
	st_qa_commts.visible=false
	st_qa_status.visible=false
	st_yr_all.visible=true
	st_yr_one.visible=false
	li_yr =dw_cntrfy_all.GetItemNumber(1,'cntrfy')
	//li_yr = integer(ls_yr)
	ls_month=ddlb_month_all.text
	choose case ls_month
		case 'January'
			ld_start_date =date('01/01/'+string(li_yr))
			ld_end_date =date('01/31/'+string(li_yr))
		case 'February'
			ld_start_date =date('02/01/'+string(li_yr))
			if mod(li_yr,4 ) =0 then
				ld_end_date =date('02/29/'+string(li_yr))
			else
				ld_end_date =date('02/28/'+string(li_yr))
			end if
		case 'March'
			ld_start_date =date('03/01/'+string(li_yr))
			ld_end_date =date('03/31/'+string(li_yr))
		case 'April'
			ld_start_date =date('04/01/'+string(li_yr))
			ld_end_date =date('04/30/'+string(li_yr))
		case 'May'
			ld_start_date =date('05/01/'+string(li_yr))
			ld_end_date =date('05/31/'+string(li_yr))
		case 'June'
			ld_start_date =date('06/01/'+string(li_yr))
			ld_end_date =date('06/30/'+string(li_yr))
		case 'July'
			ld_start_date =date('07/01/'+string(li_yr))
			ld_end_date =date('07/31/'+string(li_yr))
		case 'August'
			ld_start_date =date('08/01/'+string(li_yr))
			ld_end_date =date('08/31/'+string(li_yr))
		case 'September'
			ld_start_date =date('09/01/'+string(li_yr))
			ld_end_date =date('09/30/'+string(li_yr))
		case 'October'
			ld_start_date =date('10/01/'+string(li_yr))
			ld_end_date =date('10/31/'+string(li_yr))
		case 'November'
			ld_start_date =date('11/01/'+string(li_yr ))
			ld_end_date =date('11/30/'+string(li_yr ))
		case 'December'
			ld_start_date =date('12/01/'+string(li_yr ))
			ld_end_date =date('12/31/'+string(li_yr ))
	end choose
	ls_start_date=string(ld_start_date,'mm/dd/yyyy')
	ls_end_date=string(ld_end_date,'mm/dd/yyyy')
	ls_m=mid(ls_start_date,1, 2)
	ls_yr=mid(ls_start_date,7, 4)
	st_all.visible=true
	st_back.visible=false
	if ls_m>='10' and cbx_backall.checked=false then
		st_all.visible=true
		st_all.text='This choice will produce report for all producer from 10/01/'+ls_yr+&
		' to '+ls_end_date
	elseif ls_m<'10' and cbx_backall.checked=false then
		ls_preyr=string(li_yr -1 )
		st_all.text='This choice will produce report for all producer from 10/01/'+ls_preyr+&
		' to '+ls_end_date
	elseif cbx_backall.checked=true then
		li_m=long(ls_m)
		li_mold=li_m
		li_m++
		ls_m=string(li_m,'00')
		if li_m>12 then
			ls_start_date='01/01/'+ls_yr
			ls_end_date='12/31/'+ls_yr
		else
			ls_start_date=string(li_m,'00')+'/01/'+string(li_yr - 1)
			ls_end_date=ls_end_date
		end if
		st_all.text='This choice will produce report for all producer from '+ls_start_date+&
		' to '+ls_end_date
   end if
elseif cbx_all_prdr.checked=false then
	cbx_back.visible=true
	cb_save.visible=true
	cbx_one_prdr.checked=true
	cb_cancel.visible=true
	cb_ok.visible=true
	cb_ok.text='Save/&View'
	cbx_all_prdr.visible=false
	cbx_all_prdr_incld.visible=false
	cbx_backall.visible=false
	cbx_one_prdr.visible=true
	cbx_one_prdr_incld.visible=true
	cbx_regen.visible=true
	ddlb_month_all.visible=false
	ddlb_month_one.visible=true
	dw_cntrfy_all.visible=false
	dw_cntrfy_one.visible=true
	dw_one_prdr.visible=true
	dw_rpt_for_cntrmed.visible=true
	mle_prd.visible=true
	mle_qa.visible=false

	r_all_prdr.visible=false
	r_one_prdr.visible=true
	gb_pd.visible=true
	gb_qa.visible=true
	rb_satis_prd.visible=true
	rb_satisqa.visible=true
	rb_unsatis_prd.visible=true
	rb_unsatisqa.visible=true
	st_med.visible=true
	st_mon_all.visible=false
	st_mon_one.visible=true
	st_one_prdr.visible=true
	st_prd_commts.visible=true
	st_prdr_status.visible=true
	st_qa_commts.visible=false
	st_qa_status.visible=true
	st_yr_all.visible=false
	st_yr_one.visible=true
	li_yr =dw_cntrfy_one.GetItemNumber(1,'cntrfy')
	//li_yr = integer(ls_yr)
	ls_cntrmed=dw_rpt_for_cntrmed.GetItemString(1,'cntrmed')
	ls_prdr=dw_one_prdr.GetItemString(1,'prdr')
	ls_month=ddlb_month_one.text
	choose case ls_month
		case 'January'
			ld_start_date =date('01/01/'+string(li_yr))
			ld_end_date =date('01/31/'+string(li_yr))
		case 'February'
			ld_start_date =date('02/01/'+string(li_yr))
			if mod(li_yr,4 ) =0 then
				ld_end_date =date('02/29/'+string(li_yr))
			else
				ld_end_date =date('02/28/'+string(li_yr))
			end if
		case 'March'
			ld_start_date =date('03/01/'+string(li_yr))
			ld_end_date =date('03/31/'+string(li_yr))
		case 'April'
			ld_start_date =date('04/01/'+string(li_yr))
			ld_end_date =date('04/30/'+string(li_yr))
		case 'May'
			ld_start_date =date('05/01/'+string(li_yr))
			ld_end_date =date('05/31/'+string(li_yr))
		case 'June'
			ld_start_date =date('06/01/'+string(li_yr))
			ld_end_date =date('06/30/'+string(li_yr))
		case 'July'
			ld_start_date =date('07/01/'+string(li_yr))
			ld_end_date =date('07/31/'+string(li_yr))
		case 'August'
			ld_start_date =date('08/01/'+string(li_yr))
			ld_end_date =date('08/31/'+string(li_yr))
		case 'September'
			ld_start_date =date('09/01/'+string(li_yr))
			ld_end_date =date('09/30/'+string(li_yr))
		case 'October'
			ld_start_date =date('10/01/'+string(li_yr))
			ld_end_date =date('10/31/'+string(li_yr))
		case 'November'
			ld_start_date =date('11/01/'+string(li_yr ))
			ld_end_date =date('11/30/'+string(li_yr ))
		case 'December'
			ld_start_date =date('12/01/'+string(li_yr ))
			ld_end_date =date('12/31/'+string(li_yr ))
	end choose
	select prd_commts, qa_commts,prdstat,qastat into :ls_prd_commts, :ls_qa_commts,
			:ls_prdstat, :ls_qastat
	from monrpt
	where start_date=:ld_start_date and end_date=:ld_end_date and prdr=:ls_prdr and
			cntrmed=:ls_cntrmed
	using SqlServerTrans;
	mle_prd.text= ls_prd_commts
	mle_qa.text=ls_qa_commts
	if ls_prdstat='Y' then
		rb_satis_prd.checked=true
	else
		rb_unsatis_prd.checked=true
	end if
	if ls_qastat='Y' then
		rb_satisqa.checked=true
	else
		rb_unsatisqa.checked=true
	end if
	ls_start_date=string(ld_start_date,'mm/dd/yyyy')
	ls_end_date=string(ld_end_date,'mm/dd/yyyy')
	ls_m=mid(ls_start_date,1, 2)
	ls_yr=mid(ls_start_date,7, 4)
	st_all.visible=false
	st_back.visible=true
	if ls_m>='10' and cbx_back.checked=false then
		st_back.text='This choice will produce report for one producer that choosen from 10/01/'+ls_yr+&
		' to '+ls_end_date
	elseif ls_m<'10' and cbx_back.checked=false then
		ls_preyr=string(li_yr -1 )
		st_back.text='This choice will produce report for one producer that choosen from 10/01/'+ls_preyr+&
		' to '+ls_end_date
	elseif cbx_back.checked then
		li_m=long(ls_m)
		li_mold=li_m
		li_m++
		if li_m>12 then
			ls_start_date='01/01/'+ls_yr
			ls_end_date=ls_end_date
		elseif li_m<=12 then
			ls_start_date=string(li_m,'00')+'/01/'+string(li_yr - 1)
			ls_end_date=ls_end_date
		end if
		st_back.text='This choice will produce report for one producer that choosen from '+ls_start_date +&
		' to '+ls_end_date
	end if
end if

end event

type st_one_prdr from statictext within w_response_monthly_reports
integer x = 78
integer y = 704
integer width = 270
integer height = 52
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 33554432
long backcolor = 80263581
string text = "Producer:"
boolean focusrectangle = false
end type

type st_mon_all from statictext within w_response_monthly_reports
integer x = 1129
integer y = 136
integer width = 197
integer height = 52
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 33554432
long backcolor = 80263581
string text = "Month:"
boolean focusrectangle = false
end type

type dw_cntrfy_all from u_pics_dw within w_response_monthly_reports
integer x = 658
integer y = 124
integer width = 357
integer height = 88
integer taborder = 20
string dataobject = "d_month_for_dddw_cntrfy"
boolean vscrollbar = false
boolean livescroll = false
end type

type dw_one_prdr from u_pics_dw within w_response_monthly_reports
integer x = 347
integer y = 692
integer width = 357
integer height = 88
integer taborder = 60
string dataobject = "d_rpt_for_dddw_prdr"
boolean vscrollbar = false
boolean livescroll = false
end type

event itemchanged;call super::itemchanged;string ls_prdr, ls_cntrmed, ls_qastat, ls_prdstat, ls_prd_commts, ls_qa_commts,&
		ls_month
DatawindowChild dwc_cntrmed
date ld_start_date, ld_end_date,ld_before_start30
int li_yr, li_cur
datetime ld_stdt, ld_enddt, ld_bef_30dt

ls_prdr= data
li_yr =dw_cntrfy_one.GetItemNumber(1,'cntrfy')
ls_month=ddlb_month_one.text
choose case ls_month
	case 'January'
		ld_start_date =date('01/01/'+string(li_yr))
		ld_end_date =date('01/31/'+string(li_yr))
	case 'February'
		ld_start_date =date('02/01/'+string(li_yr))
		if mod(li_yr,4 ) =0 then
			ld_end_date =date('02/29/'+string(li_yr))
		else
			ld_end_date =date('02/28/'+string(li_yr))
		end if
	case 'March'
		ld_start_date =date('03/01/'+string(li_yr))
		ld_end_date =date('03/31/'+string(li_yr))
	case 'April'
		ld_start_date =date('04/01/'+string(li_yr))
		ld_end_date =date('04/30/'+string(li_yr))
	case 'May'
		ld_start_date =date('05/01/'+string(li_yr))
		ld_end_date =date('05/31/'+string(li_yr))
	case 'June'
		ld_start_date =date('06/01/'+string(li_yr))
		ld_end_date =date('06/30/'+string(li_yr))
	case 'July'
		ld_start_date =date('07/01/'+string(li_yr))
		ld_end_date =date('07/31/'+string(li_yr))
	case 'August'
		ld_start_date =date('08/01/'+string(li_yr))
		ld_end_date =date('08/31/'+string(li_yr))
	case 'September'
		ld_start_date =date('09/01/'+string(li_yr))
		ld_end_date =date('09/30/'+string(li_yr))
	case 'October'
		ld_start_date =date('10/01/'+string(li_yr))
		ld_end_date =date('10/31/'+string(li_yr))
	case 'November'
		ld_start_date =date('11/01/'+string(li_yr ))
		ld_end_date =date('11/30/'+string(li_yr ))
	case 'December'
		ld_start_date =date('12/01/'+string(li_yr ))
		ld_end_date =date('12/31/'+string(li_yr ))
end choose
ld_before_start30= RelativeDate(ld_start_date, - 30 )
ld_stdt=datetime(ld_start_date,time('00:00:00'))
ld_enddt=datetime(ld_end_date,time('00:00:00'))
ld_bef_30dt=datetime(ld_before_start30,time('00:00:00'))
dw_rpt_for_cntrmed.GetChild('cntrmed', dwc_cntrmed)
dwc_cntrmed.SetTransObject( SqlServerTrans )
dwc_cntrmed.Retrieve(ld_bef_30dt,ls_prdr)
ls_cntrmed =dwc_cntrmed.GetItemString(1,'cntrmed')
IF ls_prdr='PTB' then
	li_cur=dwc_cntrmed.InsertRow(0)
	dwc_cntrmed.SetItem(li_cur,'cntrmed','FL')
END IF
dw_rpt_for_cntrmed.Retrieve(ld_bef_30dt,ls_prdr)
dw_rpt_for_cntrmed.InsertRow(0)
dw_rpt_for_cntrmed.SetItem(1,'cntrmed', ls_cntrmed)
select prd_commts, qa_commts,prdstat,qastat into :ls_prd_commts, :ls_qa_commts,
		:ls_prdstat, :ls_qastat
from monrpt
where start_date=:ld_stdt and end_date=:ld_enddt and prdr=:ls_prdr and
		cntrmed=:ls_cntrmed
using SqlServerTrans;
mle_prd.text= ls_prd_commts
mle_qa.text=ls_qa_commts
if ls_prdstat='Y' then
	rb_satis_prd.checked=true
else
	rb_unsatis_prd.checked=true
end if
if ls_qastat='Y' then
	rb_satisqa.checked=true
else
	rb_unsatisqa.checked=true
end if
cb_save.enabled=true









end event

type ddlb_month_all from u_ddlb within w_response_monthly_reports
integer x = 1376
integer y = 124
integer width = 357
integer height = 600
integer taborder = 30
boolean sorted = false
string item[] = {"January","February","March","April","May","June","July","August","September","October","November","December"}
end type

event selectionchanged;call super::selectionchanged;long li_index, li_yr, li_m, li_mold
date ld_start_date, ld_end_date
string ls_start_date, ls_end_date, ls_m, ls_yr, ls_preyr, ls_month, ls_back='N'


li_yr =dw_cntrfy_all.GetItemNumber(1,'cntrfy')
	//li_yr = integer(ls_yr)
//	ls_month=ddlb_month_all.text
if cbx_backall.checked then
	ls_back='Y'
end if
li_index=index
	choose case index
		case 1
			ld_start_date =date('01/01/'+string(li_yr))
			ld_end_date =date('01/31/'+string(li_yr))
		case 2
			ld_start_date =date('02/01/'+string(li_yr))
			if mod(li_yr,4 ) =0 then
				ld_end_date =date('02/29/'+string(li_yr))
			else
				ld_end_date =date('02/28/'+string(li_yr))
			end if
		case 3
			ld_start_date =date('03/01/'+string(li_yr))
			ld_end_date =date('03/31/'+string(li_yr))
		case 4
			ld_start_date =date('04/01/'+string(li_yr))
			ld_end_date =date('04/30/'+string(li_yr))
		case 5
			ld_start_date =date('05/01/'+string(li_yr))
			ld_end_date =date('05/31/'+string(li_yr))
		case 6
			ld_start_date =date('06/01/'+string(li_yr))
			ld_end_date =date('06/30/'+string(li_yr))
		case 7
			ld_start_date =date('07/01/'+string(li_yr))
			ld_end_date =date('07/31/'+string(li_yr))
		case 8
			ld_start_date =date('08/01/'+string(li_yr))
			ld_end_date =date('08/31/'+string(li_yr))
		case 9
			ld_start_date =date('09/01/'+string(li_yr))
			ld_end_date =date('09/30/'+string(li_yr))
		case 10
			ld_start_date =date('10/01/'+string(li_yr))
			ld_end_date =date('10/31/'+string(li_yr))
		case 11
			ld_start_date =date('11/01/'+string(li_yr ))
			ld_end_date =date('11/30/'+string(li_yr ))
		case 12
			ld_start_date =date('12/01/'+string(li_yr ))
			ld_end_date =date('12/31/'+string(li_yr ))
	end choose
	ls_start_date=string(ld_start_date,'mm/dd/yyyy')
	ls_end_date=string(ld_end_date,'mm/dd/yyyy')
	ls_m=mid(ls_start_date,1, 2)
	ls_yr=mid(ls_start_date,7, 4)
	st_all.visible=true
	st_back.visible=false
	if ls_m>='10' and ls_back='N' then
		st_all.visible=true
		st_all.text='This choice will produce report for all producer from 10/01/'+ls_yr+&
		' to '+ls_end_date
	elseif ls_m<'10' and ls_back='N' then
		ls_preyr=string(li_yr -1 )
		st_all.text='This choice will produce report for all producer from 10/01/'+ls_preyr+&
		' to '+ls_end_date
	end if
	if ls_back='Y' then
		li_m=long(ls_m)
		li_mold=li_m
		li_m++
		ls_m=string(li_m,'00')
		if li_m>12 then
			ls_start_date='01/01/'+ls_yr
			ls_end_date='12/31/'+ls_yr
		elseif li_m<=12 then
			ls_start_date=ls_m+'/01/'+string(li_yr - 1)
			ls_end_date=ls_end_date
		end if
		st_all.text='This choice will produce report for all producer from '+ls_start_date+&
			' to '+ls_end_date
	end if
end event

type cb_ok from u_cb within w_response_monthly_reports
integer x = 128
integer y = 1784
integer width = 402
integer height = 72
integer taborder = 0
integer textsize = -10
string text = "&Save/View"
end type

event clicked;call super::clicked;Long li_yr ,li_count, li_re, li_start_month,li_fy, li_m, li_mold
String ls_prdr, ls_month, ls_yr, ls_start_date, ls_like, ls_num_month, ls_yr2,&
		ls_oneprdr,ls_cntrmed,ls_incldavg, ls_regen,ls_prd_commts, ls_qa_commts,&
		ls_qastat, ls_prdstat,ls_cntrlc,ls_done,ls_max, ls_min,ls_med,ls_forceregen, &
		ls_lang, ls_back='N', ls_m, ls_preyr, ls_stdate, ls_enddate
str_voucher_report lstr
Date ld_start_date, ld_end_date, ld_fy_start_date, ld_fy_end_date, ld_stdate, ld_enddate
Boolean lb_extract_checked 
DateTime ld_stdt

ls_prdr =dw_one_prdr.GetItemString(1,'prdr')//if all prdr choose first prdr to make
ls_cntrmed=dw_rpt_for_cntrmed.GetItemString(1,'cntrmed')//out put show instead plank paper
ls_prdr= Trim(ls_prdr)
ls_cntrmed= Trim(ls_cntrmed)
lstr.array[3] = ls_prdr
lstr.array[13] =ls_cntrmed
IF cbx_one_prdr.checked THEN
	li_yr =dw_cntrfy_one.GetItemNumber(1, 'cntrfy')
	IF IsNull(li_yr) THEN
		Messagebox('','You must choose a calendar year',exclamation!)
		RETURN
	END IF
	ls_yr = String(li_yr)
	ls_month =ddlb_month_one.text
ELSE
	li_yr =dw_cntrfy_all.GetItemNumber(1, 'cntrfy')
	IF IsNull(li_yr) THEN
		Messagebox('','You must choose a calendar year',exclamation!)
		RETURN
	END IF
	ls_yr = String(li_yr)
	ls_month =ddlb_month_all.text
END IF
CHOOSE CASE ls_month
	CASE 'January'
		ld_start_date =Date('01/01/'+String(li_yr))
		ld_end_date =Date('01/31/'+String(li_yr))
	CASE 'February'
		ld_start_date =Date('02/01/'+String(li_yr))
		IF Mod(li_yr,4) =0 THEN
			ld_end_date =Date('02/29/'+String(li_yr))
		ELSE
			ld_end_date =Date('02/28/'+String(li_yr))
		END IF
	CASE 'March'
		ld_start_date =Date('03/01/'+String(li_yr))
		ld_end_date =Date('03/31/'+String(li_yr))
	CASE 'April'
		ld_start_date =Date('04/01/'+String(li_yr))
		ld_end_date =Date('04/30/'+String(li_yr))
	CASE 'May'
		ld_start_date =Date('05/01/'+String(li_yr))
		ld_end_date =Date('05/31/'+String(li_yr))
	CASE 'June'
		ld_start_date =Date('06/01/'+String(li_yr))
		ld_end_date =Date('06/30/'+String(li_yr))
	CASE 'July'
		ld_start_date =Date('07/01/'+String(li_yr))
		ld_end_date =Date('07/31/'+String(li_yr))
	CASE 'August'
		ld_start_date =Date('08/01/'+String(li_yr))
		ld_end_date =Date('08/31/'+String(li_yr))
	CASE 'September'
		ld_start_date =Date('09/01/'+String(li_yr))
		ld_end_date =Date('09/30/'+String(li_yr))
	CASE 'October'
		ld_start_date =Date('10/01/'+String(li_yr))
		ld_end_date =Date('10/31/'+String(li_yr))
	CASE 'November'
		ld_start_date =Date('11/01/'+String(li_yr))
		ld_end_date =Date('11/30/'+String(li_yr))
	CASE 'December'
		ld_start_date =Date('12/01/'+String(li_yr))
		ld_end_date =Date('12/31/'+String(li_yr))
END CHOOSE
ld_stdt=DateTime(ld_start_date,Time('00:00:00'))
IF cbx_one_prdr.checked THEN
	IF cbx_back.checked THEN
		ls_back='Y'
	ELSE
		ls_back='N'
	END IF
	ls_oneprdr='Y'
	ls_prdr =dw_one_prdr.GetItemString(1,'prdr')
	ls_cntrmed=dw_rpt_for_cntrmed.GetItemString(1,'cntrmed')
	ls_prdr= Trim(ls_prdr)
	ls_cntrmed= Trim(ls_cntrmed)
	lstr.array[3] = ls_prdr
	lstr.array[13] =ls_cntrmed
	IF rb_satis_prd.checked THEN
		ls_prdstat='Y'
	ELSE
		ls_prdstat='N'
	END IF
	IF rb_satisqa.checked THEN
		ls_qastat='Y'
	ELSE
		ls_qastat='N'
	END IF
	ls_prd_commts=mle_prd.text
	ls_qa_commts=mle_qa.text
	IF ls_prdr<>'PTB' THEN
		UPDATE monrpt
		SET prd_commts=:ls_prd_commts ,qa_commts=:ls_qa_commts,prdstat=:ls_prdstat,
		    qastat=:ls_qastat	
		WHERE prdr=:ls_prdr AND cntrmed=:ls_cntrmed AND start_date=:ld_stdt
		USING sqlservertrans;
	ELSEIF ls_prdr='PTB' AND ls_cntrmed='FL' THEN
		ls_cntrmed='RTB'
		ls_lang='Y'
	ELSEIF ls_prdr='PTB' AND (ls_cntrmed='RC' OR  ls_cntrmed='RTB' ) THEN
		ls_lang='N'
	END IF
	IF ls_prdr='PTB' THEN
		UPDATE monrpt
		SET prd_commts=:ls_prd_commts ,qa_commts=:ls_qa_commts,prdstat=:ls_prdstat,&
		    qastat=:ls_qastat	
		WHERE prdr=:ls_prdr AND cntrmed=:ls_cntrmed AND start_date=:ld_stdt AND
		      foreign_lang=:ls_lang
		USING sqlservertrans;
	END IF
	IF NOT f_check_dberror(sqlservertrans, 'update monrpt set prd_commts,qa_commts') THEN
		RETURN
	ELSE
		COMMIT USING sqlservertrans;
	END IF
	IF cbx_one_prdr_incld.checked THEN
		ls_incldavg='Y'
	ELSE
		ls_incldavg='N'
	END IF
ELSEIF cbx_all_prdr.checked THEN
	ls_oneprdr='N'
	IF cbx_all_prdr_incld.checked THEN
		ls_incldavg='Y'
	ELSE
		ls_incldavg='N'
	END IF
	IF cbx_backall.checked THEN
		ls_back='Y'
	ELSE
		ls_back='N'
	END IF
END IF
lstr.array[1] = ls_month
lstr.array[2] = ls_yr //*	**the year hear still regular year	
lstr.array[7] ='ok'
ls_start_date = String(ld_start_date,'mm/dd/yyyy')
lstr.array[8] = ls_start_date
lstr.array[9] = String(ld_end_date,'mm/dd/yyyy')

ls_num_month =Left(ls_start_date, 2)
li_start_month =Integer(ls_num_month)
IF li_start_month >=10 THEN
	ld_fy_start_date=Date('10/01/'+ls_yr)
	ld_fy_end_date =Date('09/30/'+ String(li_yr + 1))
ELSE
	ld_fy_start_date=Date('10/01/'+String(li_yr - 1))
	ld_fy_end_date =Date('09/30/'+ ls_yr)
END IF

IF cbx_one_prdr.checked THEN


	IF cbx_regen.checked THEN
		ls_forceregen='Y'
		lstr.array[4] =ls_forceregen
	ELSE
		ls_forceregen='N'
		lstr.array[4] =ls_forceregen
	END IF

END IF//if cbx_oneprdr

lstr.array[10] =String(ld_fy_start_date,'mm/dd/yyyy')
lstr.array[11] =String(ld_fy_end_date,'mm/dd/yyyy')
//lstr.array[13] =ls_cntrmed
lstr.array[14] =ls_oneprdr
lstr.array[15] =ls_incldavg
lstr.array[17]=ls_back
ls_start_date=String(ld_start_date,'mm/dd/yyyy')
ls_m=Left(ls_start_date,2)
ls_yr=Mid(ls_start_date,7,4)
li_yr=Long(ls_yr)
ls_preyr=String(li_yr - 1)
li_m=Long(ls_m)
li_mold=li_m
li_m++
ls_m=String(li_m,'00')
IF li_m>12 THEN
	ls_stdate='01/01/'+ls_yr
	ls_enddate='12/01/'+ls_yr
ELSE
	ls_stdate=ls_m+'/01/'+ls_preyr
	ls_enddate=String(li_mold,'00')+'/01/'+ls_yr
END IF
ld_stdate=Date(ls_stdate)
ld_enddate=Date(ls_enddate)
	
IF cbx_back.checked AND cbx_one_prdr.checked THEN
	
	SELECT Max(done), Min(done) INTO :ls_max, :ls_min
	FROM monrpt
	WHERE prdr=:ls_prdr AND cntrmed=:ls_cntrmed AND start_date>=:ld_stdate AND start_date<=:ld_enddate
	USING sqlservertrans;
	IF f_check_dberror(sqlservertrans,'select from monrpt find max(done) and min(done)')=FALSE THEN
		RETURN
	END IF
	IF ls_max<>'Y' OR ls_min<'Y' THEN
		Messagebox('Error','You do not have backward last 12 month data, can not go backward to 12 months'+&
		' for the month '+ls_month, stopSign!)
		RETURN
	END IF
	IF cbx_regen.checked THEN
		Messagebox('Error','can not regenarate data for past 12 months',stopSign!)
		RETURN
   END IF
ELSEIF cbx_backall.checked AND cbx_all_prdr.checked THEN
	SELECT Max(done), Min(done) INTO :ls_max, :ls_min
	FROM monrpt
	WHERE  start_date>=:ld_stdate AND start_date<=:ld_enddate
	USING sqlservertrans;
	IF f_check_dberror(sqlservertrans,'select from monrpt find max(done) and min(done)')=FALSE THEN
		RETURN
	END IF
	IF ls_max<>'Y' OR ls_min<'Y' THEN
		Messagebox('Error','You do not have last 12 month data, can not go backward to 12 months'+&
		' for the month '+ls_month, stopSign!)
		RETURN
	END IF
END IF
CloseWithReturn(w_response_monthly_reports,lstr)

end event

type cb_cancel from u_cb within w_response_monthly_reports
integer x = 1490
integer y = 1784
integer width = 306
integer height = 72
integer taborder = 0
integer textsize = -10
string text = "&Cancel"
end type

event clicked;call super::clicked;str_voucher_report lstr

lstr.array[7] = 'cancel'
CloseWithReturn(w_response_monthly_reports,lstr)
//Close(w_response_monthly_reports)
end event

type st_yr_all from statictext within w_response_monthly_reports
integer x = 219
integer y = 140
integer width = 434
integer height = 52
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 33554432
long backcolor = 80263581
string text = "Calendar Year:"
boolean focusrectangle = false
end type

type st_qa_status from statictext within w_response_monthly_reports
integer x = 1093
integer y = 1040
integer width = 539
integer height = 52
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 33554432
long backcolor = 80263581
string text = "Quality Performance"
boolean focusrectangle = false
end type

type gb_qa from groupbox within w_response_monthly_reports
integer x = 1038
integer y = 1032
integer width = 882
integer height = 220
integer textsize = -8
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 33554432
long backcolor = 67108864
end type

type gb_pd from groupbox within w_response_monthly_reports
integer x = 101
integer y = 1032
integer width = 882
integer height = 220
integer textsize = -8
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 33554432
long backcolor = 67108864
end type


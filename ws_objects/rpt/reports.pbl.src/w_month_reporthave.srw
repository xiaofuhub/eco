$PBExportHeader$w_month_reporthave.srw
forward
global type w_month_reporthave from w_sheet
end type
type dw_rpt_all_colhave from u_pics_dw within w_month_reporthave
end type
type dw_rpt_qc_apr_rej_showhave from u_pics_dw within w_month_reporthave
end type
type dw_rpt_detl_total_pcthave from u_pics_dw within w_month_reporthave
end type
type dw_rpt_sum_cmpt_pcthave from u_pics_dw within w_month_reporthave
end type
type cb_prev from commandbutton within w_month_reporthave
end type
type cb_next from commandbutton within w_month_reporthave
end type
type cb_printall from commandbutton within w_month_reporthave
end type
type cb_option from commandbutton within w_month_reporthave
end type
type dw_deviate_sum_sp_no_detail from u_pics_dw within w_month_reporthave
end type
type dw_rpt_cntr_cntrlc from u_pics_dw within w_month_reporthave
end type
type dw_rpt_commts_in_amonth from u_pics_dw within w_month_reporthave
end type
type dw_rpt_qc_apr_rej_show_total from u_pics_dw within w_month_reporthave
end type
type dw_rpt_detl_total_pct from u_pics_dw within w_month_reporthave
end type
type dw_rpt_sum_cmpt_pct from u_pics_dw within w_month_reporthave
end type
type cb_print from commandbutton within w_month_reporthave
end type
type cb_exit from commandbutton within w_month_reporthave
end type
type dw_storeproc from u_pics_dw within w_month_reporthave
end type
type dw_rpt_fram_prod_qc_tbl_and_bar from u_pics_dw within w_month_reporthave
end type
type dw_rpt_sum_prod_plan2 from u_pics_dw within w_month_reporthave
end type
type dw_rpt_fram_prod_qc_tbl_and_barhave from u_pics_dw within w_month_reporthave
end type
type dw_pcdeviaten_ace_report from u_pics_dw within w_month_reporthave
end type
end forward

global type w_month_reporthave from w_sheet
integer x = 5
integer y = 4
integer width = 3584
integer height = 1844
string title = "Contract Annual Initialization"
dw_rpt_all_colhave dw_rpt_all_colhave
dw_rpt_qc_apr_rej_showhave dw_rpt_qc_apr_rej_showhave
dw_rpt_detl_total_pcthave dw_rpt_detl_total_pcthave
dw_rpt_sum_cmpt_pcthave dw_rpt_sum_cmpt_pcthave
cb_prev cb_prev
cb_next cb_next
cb_printall cb_printall
cb_option cb_option
dw_deviate_sum_sp_no_detail dw_deviate_sum_sp_no_detail
dw_rpt_cntr_cntrlc dw_rpt_cntr_cntrlc
dw_rpt_commts_in_amonth dw_rpt_commts_in_amonth
dw_rpt_qc_apr_rej_show_total dw_rpt_qc_apr_rej_show_total
dw_rpt_detl_total_pct dw_rpt_detl_total_pct
dw_rpt_sum_cmpt_pct dw_rpt_sum_cmpt_pct
cb_print cb_print
cb_exit cb_exit
dw_storeproc dw_storeproc
dw_rpt_fram_prod_qc_tbl_and_bar dw_rpt_fram_prod_qc_tbl_and_bar
dw_rpt_sum_prod_plan2 dw_rpt_sum_prod_plan2
dw_rpt_fram_prod_qc_tbl_and_barhave dw_rpt_fram_prod_qc_tbl_and_barhave
dw_pcdeviaten_ace_report dw_pcdeviaten_ace_report
end type
global w_month_reporthave w_month_reporthave

type variables
str_voucher_report istr
int i_count=0, i_prdr_med=1,i_1_prdr=1
dw_rpt_sum_prod_plan2 idw
end variables

forward prototypes
public function integer wf_rpt_create_plan_act_reg_late_ship_1_prdr_1_mon (datetime cur_start_date, datetime cur_end_date)
public function integer wf_rpt_create_plan_act_reg_late_ship_all_prdr_1_mon (datetime cur_start_date, datetime cur_end_date)
public function integer wf_rpt_ini_all_col_1_prdr_1_year0 (datetime cur_start_date, datetime cur_end_date)
public function integer wf_rpt_ini_all_col_all_prdr_1_year0 (datetime cur_start_date, datetime cur_end_date)
public function integer wf_rpt_text_obj_total_pct_1_prdr ()
public function integer wf_rpt_text_obj_total_pct_all_prdr ()
public function integer wf_rpt_text_obj_total_pct_find_1_prdr ()
public function integer wf_create_all_col_all_prdr_uptonow ()
public function integer wf_rpt_apr_rej_ytd_laterejpct_1_prdr_1_mon (datetime cur_start_date, datetime cur_end_date)
public function integer wf_rpt_apr_rej_ytd_laterejpct_all_prdr_1_mon (datetime cur_start_date, datetime cur_end_date)
public function integer wf_create_all_col_1_prdr_uptonow ()
public function integer wf_rpt_create_awad_asgn_late_ship_all_prdr_1_mon (datetime cur_start_date, datetime cur_end_date)
public function integer wf_rpt_create_awad_asgn_late_ship_1_prdr_1_mon (datetime cur_start_date, datetime cur_end_date)
public function integer wf_rpt_text_obj_total_pct_1_prdr_sub (string as_lang)
public function integer wf_rpt_text_obj_total_pct_all_prdr_sub (string as_prdr, string as_med, string as_lang)
public function integer wf_rpt_text_obj_total_pct_1_prdr_subhave (string as_lang)
public function integer wf_rpt_apr_rej_ytdlaterejpct1prdr1mhave (date cur_start_date)
public function integer wf_rpt_text_obj_change_mon_header ()
public function integer wf_rpt_text_obj_total_pct_allprdrsubhave (string as_prdr, string as_med, string as_lang)
public function integer wf_rpt_apr_rej_ytdltrejpctallprdr1mhave (string as_prdr, string as_med, string as_lang, date cur_start_date)
end prototypes

public function integer wf_rpt_create_plan_act_reg_late_ship_1_prdr_1_mon (datetime cur_start_date, datetime cur_end_date);
return 1
end function

public function integer wf_rpt_create_plan_act_reg_late_ship_all_prdr_1_mon (datetime cur_start_date, datetime cur_end_date);
return 1
end function

public function integer wf_rpt_ini_all_col_1_prdr_1_year0 (datetime cur_start_date, datetime cur_end_date);
	return 1

end function

public function integer wf_rpt_ini_all_col_all_prdr_1_year0 (datetime cur_start_date, datetime cur_end_date);
	return 1

end function

public function integer wf_rpt_text_obj_total_pct_1_prdr ();
date ld_start_date, ld_end_date		
long n
string  ls_med, ls_prdr,ls_start_date, ls_end_date,	ls_max,ls_min,ls_lang
datetime ld_stdt, ld_enddt		

str_voucher_report lstr
n_ds lds_agm

lstr =istr
this.SetRedraw( false )

ls_prdr =lstr.array[3]
ls_med =lstr.array[13]

ls_start_date = lstr.array[8]
ls_end_date = (lstr.array[9])

ld_start_date= date( ls_start_date)
ld_end_date =date( ls_end_date )
ld_stdt=datetime(ld_start_date,time('00:00:00'))
ls_prdr =trim( ls_prdr )
ls_med =trim( ls_med )
if ls_prdr='PTB' then
	if ls_med='FL' THEN
		ls_med='RC'
		ls_lang='Y'
	elseif ls_med='RC' THEN
		ls_lang='N' 
	end if
elseif ls_prdr<>'PTB' THEN
	ls_lang='N'
end if
wf_rpt_text_obj_total_pct_1_prdr_subhave(ls_lang)	
return 1

end function

public function integer wf_rpt_text_obj_total_pct_all_prdr ();//this function is extract current producer ls_prdr list and cntrmed ls_med and foreign language ls_lang.
//data store lds_agm is used for this purpose. after get prdr list the call fucntion wf_...allprdrsubhave()
//the function wf_rpt_text_obj_total_pct_all_prdr_sub not used here. Since this window only for last 12 months

date ld_start_date, ld_end_date, &
		ld_fy_start_date, ld_fy_end_date, ld_before_start30
long li_row_count, i, k,i2,n , &
		li_apr1,li_apr2, li_apr3, li_sum_apr1,li_sum_apr2, li_sum_apr3, li_rej1, li_rej2,&
	  j, li_count,li_apr, li_sum_apr, li_row, li_count2
long  li_acttitles, li_yr, li_fy, li_cntrfy,&		
		li_rej3, li_sum_rej1,li_sum_rej2, li_sum_rej3, li_rej,li_sum_rej,li_allowed, &
		li_ship,li_sum_ship,li_late, li_sum_late,li_awad, li_sum_awad,li_asgn,li_sum_asgn,&
		li_max_fy
string  ls_med, ls_prdr,ls_prdr_yes, ls_cntrt, ls_cntrlc, &
		ls_start_date, ls_end_date, ls_fy_start_date, ls_fy_end_date, &
		 ls_yr, ls_fy, ls_month, ls_filter,ls_pre_fy, ls_start_date_left,&		
		ls_pct,ls_apr_rej, ls_pcs_rej, ls_print, ls_qa_commts, ls_prd_commts, &
		ls_qasum_commts, ls_prdsum_commts, ls_pd_yes, ls_qa_yes, ls_cntr, ls_pd, ls_qa,&
		ls_year,ls_max,ls_min,ls_lang, ls_back
		
boolean lb_zero= false
real lr_pct, lr_allowed,lr_pcs_rej, lr_qas_rej
str_voucher_report lstr
n_ds lds_agm
datetime ld_stdt, ld_enddt



lstr =istr
//this.SetRedraw( false )
//Setpointer( Hourglass!)
ls_month =lstr.array[1]
ls_yr =lstr.array[2]
//ls_prdr =lstr.array[3]
//ls_print =lstr.array[5]
//ls_prdr_yes =lstr.array[6]

ls_start_date = lstr.array[8]
ls_end_date = (lstr.array[9])
ls_fy_start_date = lstr.array[10]
ls_fy_end_date = (lstr.array[11])

ld_start_date= date( ls_start_date)
ld_end_date =date( ls_end_date )
ld_before_start30 = RelativeDate( ld_start_date, - 30 )
ls_back = (lstr.array[17])
//ld_fy_start_date =date( ls_fy_start_date )
//ld_fy_end_date =date( ls_fy_end_date )
ls_pre_fy= right(ls_fy_start_date, 4)
ls_fy= right(ls_fy_end_date, 4)
li_fy =integer( ls_fy )
ls_start_date_left= left(ls_start_date,2 )
lds_agm=create n_ds
lds_agm.dataobject ='d_rpt_retrieve_agt'
lds_agm.SetTransObject( SqlServerTrans )
ld_stdt=datetime(ld_start_date,time('00:00:00'))
ld_enddt=datetime(ld_end_date,time('00:00:00'))
lds_agm.Retrieve(ld_stdt,ld_enddt)

if ls_start_date_left>='10' then
	ls_year=ls_pre_fy
else
	ls_year=ls_fy
end if
li_row_count=lds_agm.RowCount()
dw_rpt_fram_prod_qc_tbl_and_bar.SetTransObject( SqlServerTrans )
dw_rpt_sum_cmpt_pct.SetTransObject( SqlServerTrans )
dw_rpt_detl_total_pct.SetTransObject( SqlServerTrans )
dw_rpt_qc_apr_rej_show_total.SetTransObject( SqlServerTrans )
for i=1 to li_row_count
	ls_prdr =lds_agm.GetItemString(i,'prdr')
	ls_med =lds_agm.GetItemString(i,'cntrmed')
	ls_prdr =trim( ls_prdr )
	ls_med =trim( ls_med )
	select max(foreign_lang),min(foreign_lang) into :ls_max, :ls_min
	from monrpt
	where prdr=:ls_prdr and cntrmed=:ls_med and start_date=:ld_stdt and
		end_date=:ld_enddt
	using SqlServerTrans;
	if not f_check_dberror(SqlServerTrans,'select max of foreign_lang from monrpt') then
		return -1
	end if
	if ls_max<> ls_min and ls_back='N' then
//		for n=1 to 2
//			if n=1 then ls_lang=ls_max
//			if n=2 then ls_lang=ls_min
//			wf_rpt_text_obj_total_pct_all_prdr_sub(ls_prdr,ls_med,ls_lang)							
//		next
	elseif ls_max=ls_min and ls_back='N' then
//		ls_lang=ls_max
//		wf_rpt_text_obj_total_pct_all_prdr_sub(ls_prdr,ls_med,ls_lang)
	elseif ls_max<>ls_min and ls_back='Y' then
		for n=1 to 2
			if n=1 then ls_lang=ls_max
			if n=2 then ls_lang=ls_min
			wf_rpt_text_obj_total_pct_allprdrsubhave(ls_prdr,ls_med,ls_lang)						
		next	
	elseif ls_max=ls_min and ls_back='Y' then
		ls_lang=ls_max
		wf_rpt_text_obj_total_pct_allprdrsubhave(ls_prdr,ls_med,ls_lang)
	end if
next
i_count=0
Setpointer( Arrow!)
this.setredraw( true)
return 1

end function

public function integer wf_rpt_text_obj_total_pct_find_1_prdr ();//this function for find next producer when one choose to show all producer. The first producer is by default set the
//the first prdr in dddw list. instant variable i_prdr_med used to control choose the next one prdr. data store
//lds_agm is for retrieve current available prdr list. For some prdr there are two languages for example 'PTB', that
//need to set variable ls_lang, variable ls_back for show last 12 months data
date ld_start_date, ld_end_date, &
		ld_fy_start_date, ld_fy_end_date, ld_before_start30
long li_row_count, i, k,i2,n , &
		li_apr1,li_apr2, li_apr3, li_sum_apr1,li_sum_apr2, li_sum_apr3, li_rej1, li_rej2,&
	  j, li_count,li_apr, li_sum_apr, li_row, li_count2
long  li_acttitles, li_yr, li_fy, li_cntrfy,&		
		li_rej3, li_sum_rej1,li_sum_rej2, li_sum_rej3, li_rej,li_sum_rej,li_allowed, &
		li_ship,li_sum_ship,li_late, li_sum_late,li_awad, li_sum_awad,li_asgn,li_sum_asgn,&
		li_max_fy
string  ls_med, ls_prdr,ls_prdr_yes, ls_cntrt, ls_cntrlc, &
		ls_start_date, ls_end_date, ls_fy_start_date, ls_fy_end_date, &
		 ls_yr, ls_fy, ls_month, ls_filter,ls_pre_fy, ls_start_date_left,&		
		ls_pct,ls_apr_rej, ls_pcs_rej, ls_print, ls_qa_commts, ls_prd_commts, &
		ls_qasum_commts, ls_prdsum_commts, ls_pd_yes, ls_qa_yes, ls_cntr, ls_pd, ls_qa,&
		ls_year,ls_max,ls_min,ls_lang,ls_next_or_prev, ls_back
		
boolean lb_zero= false
real lr_pct, lr_allowed,lr_pcs_rej, lr_qas_rej
str_voucher_report lstr
n_ds lds_agm
datetime ld_stdt, ld_enddt



lstr =istr
//this.SetRedraw( false )
//Setpointer( Hourglass!)
ls_month =lstr.array[1]
ls_yr =lstr.array[2]
//ls_prdr =lstr.array[3]
//ls_print =lstr.array[5]
//ls_prdr_yes =lstr.array[6]

ls_start_date = lstr.array[8]
ls_end_date = (lstr.array[9])
ls_fy_start_date = lstr.array[10]
ls_fy_end_date = (lstr.array[11])
ls_back = lstr.array[17]
ld_start_date= date( ls_start_date)
ld_end_date =date( ls_end_date )
ld_before_start30 = RelativeDate( ld_start_date, - 30 )
//ld_fy_start_date =date( ls_fy_start_date )
//ld_fy_end_date =date( ls_fy_end_date )
ls_pre_fy= right(ls_fy_start_date, 4)
ls_fy= right(ls_fy_end_date, 4)
li_fy =integer( ls_fy )
ls_start_date_left= left(ls_start_date,2 )
lds_agm=create n_ds
lds_agm.dataobject ='d_rpt_retrieve_agt'
lds_agm.SetTransObject( SqlServerTrans )
ld_stdt=datetime(ld_start_date,time('00:00:00'))
ld_enddt=datetime(ld_end_date,time('00:00:00'))
lds_agm.Retrieve(ld_stdt,ld_enddt)

if ls_start_date_left>='10' then
	ls_year=ls_pre_fy
else
	ls_year=ls_fy
end if
li_row_count=lds_agm.RowCount()
dw_rpt_fram_prod_qc_tbl_and_bar.SetTransObject( SqlServerTrans )
dw_rpt_sum_cmpt_pct.SetTransObject( SqlServerTrans )
dw_rpt_detl_total_pct.SetTransObject( SqlServerTrans )
dw_rpt_qc_apr_rej_show_total.SetTransObject( SqlServerTrans )
ls_next_or_prev= istr.array[16]
if ls_next_or_prev ='next' then
	i_prdr_med +=1
elseif ls_next_or_prev='prev' then
	i_prdr_med -=1
elseif ls_next_or_prev='current' then
end if
if i_prdr_med <=li_row_count and i_prdr_med >= 1 then
	ls_prdr =lds_agm.GetItemString(i_prdr_med,'prdr')
	ls_med =lds_agm.GetItemString(i_prdr_med,'cntrmed')
	ls_prdr =trim( ls_prdr )
	ls_med =trim( ls_med )
	istr.array[3]= ls_prdr
	istr.array[13] =ls_med
	select max(foreign_lang),min(foreign_lang) into :ls_max, :ls_min
	from monrpt
	where prdr=:ls_prdr and cntrmed=:ls_med and start_date=:ld_stdt and
		end_date=:ld_enddt
	using SqlServerTrans;
	if not f_check_dberror(SqlServerTrans,'select max of foreign_lang from monrpt') then
		return -1
	end if
	if ls_max<> ls_min then
		for n=1 to 2
			if n=1 then ls_lang=ls_max
			if n=2 then ls_lang=ls_min
			if ls_back='N' then
//				wf_rpt_text_obj_total_pct_all_prdr_sub(ls_prdr,ls_med,ls_lang)		
			elseif ls_back='Y' then
				wf_rpt_text_obj_total_pct_allprdrsubhave(ls_prdr,ls_med,ls_lang)		
			end if
		next
	else
		ls_lang=ls_max
		if ls_back='N' then
//			wf_rpt_text_obj_total_pct_all_prdr_sub(ls_prdr,ls_med,ls_lang)// this case in this window never happen.
		elseif ls_back='Y' then
			wf_rpt_text_obj_total_pct_allprdrsubhave(ls_prdr,ls_med,ls_lang)	//this show last 12 months data for all
			// producer
		end if
	end if
	if i_prdr_med =li_row_count then
		cb_next.enabled=false
	else
		cb_next.enabled=true
	end if
	if i_prdr_med =1 then
		cb_prev.enabled=false
	else
		cb_prev.enabled=true
	end if
elseif i_prdr_med= 0 then
	i_prdr_med=1
	messagebox('','Error ,no more previous')
	cb_prev.enabled= false
elseif i_prdr_med= li_row_count +1 then
	i_prdr_med= li_row_count
	messagebox('','Error ,no more next')
	cb_next.enabled= false
end if
	
i_count=0
Setpointer( Arrow!)
this.setredraw( true)
return 1

end function

public function integer wf_create_all_col_all_prdr_uptonow ();
return 1


end function

public function integer wf_rpt_apr_rej_ytd_laterejpct_1_prdr_1_mon (datetime cur_start_date, datetime cur_end_date);
return 1

end function

public function integer wf_rpt_apr_rej_ytd_laterejpct_all_prdr_1_mon (datetime cur_start_date, datetime cur_end_date);
return 1

end function

public function integer wf_create_all_col_1_prdr_uptonow ();
return 1


end function

public function integer wf_rpt_create_awad_asgn_late_ship_all_prdr_1_mon (datetime cur_start_date, datetime cur_end_date);
return 1

end function

public function integer wf_rpt_create_awad_asgn_late_ship_1_prdr_1_mon (datetime cur_start_date, datetime cur_end_date);
return 1

end function

public function integer wf_rpt_text_obj_total_pct_1_prdr_sub (string as_lang);

return 1

end function

public function integer wf_rpt_text_obj_total_pct_all_prdr_sub (string as_prdr, string as_med, string as_lang);
return 1

end function

public function integer wf_rpt_text_obj_total_pct_1_prdr_subhave (string as_lang);//this function is pararall the fucntion wf_...1_prdr_sub, the difference is that it compute data for last 12 month, 
//but the other one is compute for fiscal year
date ld_start_date, ld_end_date, ld_stdate,ld_enddate

datetime ldt_datetime
long li_row_count, i, k,i2, &
		li_apr1,li_apr2, li_apr3, li_apr4, li_sum_apr1,li_sum_apr2, li_sum_apr3, li_sum_apr4, li_rej1, li_rej2,&
	  j, li_count,li_count2,li_apr, li_sum_apr
long  li_acttitles, li_fy,&		
		li_rej3, li_rej4, li_sum_rej1,li_sum_rej2, li_sum_rej3, li_sum_rej4, li_rej,li_sum_rej,li_allowed, &
		li_ship,li_sum_ship,li_late, li_sum_late,li_awad, li_sum_awad,li_asgn,li_sum_asgn,&
		li_max_fy
string  ls_med, ls_prdr,ls_cntrlc,ls_cntr, ls_pct,ls_apr_rej, ls_pcs_rej,ls_print,  &
		ls_start_date, ls_end_date, ls_fy_start_date, ls_fy_end_date, ls_fy,ls_yr, ls_month, &
		ls_pd_yes, ls_qa_yes, ls_pd, ls_qa,ls_max,ls_min,ls_lang	,	ls_incldavg,ls_datetime,&
		ls_cntrtype, ls_back, ls_one_prdr
datetime ld_stdt, ld_enddt		
		 
		
		
boolean lb_zero= false
real lr_pct, lr_allowed,lr_pcs_rej, lr_qas_rej
str_voucher_report lstr
n_ds lds_agm

lstr =istr
this.SetRedraw( false )
dw_rpt_fram_prod_qc_tbl_and_bar.visible=false
//dw_rpt_fram_prod_qc_tbl_and_barhave.visible=true
ldt_datetime= datetime(Today(),Now())
ls_datetime= string( ldt_datetime,'mm/dd/yyyy hh:mm AM/PM')
//Setpointer( Hourglass!)
ls_month =lstr.array[1]
ls_yr =lstr.array[2]
ls_prdr =lstr.array[3]
ls_print =lstr.array[5]
ls_med =lstr.array[13]
ls_incldavg=lstr.array[15]
ls_back =lstr.array[17]
//ls_prdr_yes =lstr.array[6]
if ls_prdr='PTB' and ls_med='FL' THEN
	ls_med='RC'
	ls_lang='Y'
elseif ls_prdr='PTB' and ls_med='RC' THEN
	ls_lang='N'
end if

ls_start_date = lstr.array[8]
ls_end_date = (lstr.array[9])
ls_fy_start_date = lstr.array[10]
ls_fy_end_date = (lstr.array[11])
ls_one_prdr = (lstr.array[14])
ls_incldavg = lstr.array[15]
ld_start_date= date( ls_start_date)
ld_end_date =date( ls_end_date )

ls_fy= right(ls_fy_end_date, 4)
li_fy =integer( ls_fy )
//this function for create new data for new column in table monrpt. the new column is ytdapprv0, ytdreject0, ytdrejpct0
//this columns only for computing data for last 12 months. These are extra columns
wf_rpt_apr_rej_ytdlaterejpct1prdr1mhave(ld_start_date)

dw_rpt_fram_prod_qc_tbl_and_barhave.SetTransObject( SqlServerTrans )
dw_rpt_sum_cmpt_pcthave.SetTransObject( SqlServerTrans )
dw_rpt_detl_total_pcthave.SetTransObject( SqlServerTrans )
dw_rpt_qc_apr_rej_showhave.SetTransObject( SqlServerTrans )
ld_stdt=datetime(ld_start_date,time('00:00:00'))
ld_enddt=datetime(ld_end_date,time('00:00:00'))
//this is new fram complex datawindow.
j=dw_rpt_fram_prod_qc_tbl_and_barhave.Retrieve&
						(ls_prdr,ls_med,ld_stdt,as_lang)
Setpointer( Hourglass!)
//this is for computing text object in sub datacontrol dw2 such as percentage of late in each month
k=dw_rpt_sum_cmpt_pcthave.Retrieve&
						(ls_prdr,ls_med,ld_stdt,as_lang)
//this is for computing text object in sub datacontrol dw1 such as total title shipped and average reject percent
//for the month you choose to specific prdr and cntrmed.
i=dw_rpt_detl_total_pcthave.Retrieve&
						(ls_prdr,ls_med,ld_stdt,as_lang)
						
Setpointer( Hourglass!)
//this is for computing text object in sub datacontrol dw4 such as total reject for each month and average reject
//percent for each stage for whole last 12 months.
i2=dw_rpt_qc_apr_rej_showhave.Retrieve&
						(ls_prdr,ls_med,ld_stdt,as_lang)
Setpointer( Hourglass!)
//this loop is for computing text object in dw2 late percent for each month in last 12 months.
for j=2 to 14
	li_acttitles=dw_rpt_sum_cmpt_pcthave.GetItemNumber(2,j)
	if isnull(li_acttitles) then li_acttitles=0
		
	li_late=dw_rpt_sum_cmpt_pcthave.GetItemNumber(4,j)
	if li_acttitles =0 then
		lr_pct=0
	else
		lr_pct=(li_late/li_acttitles)*100
	end if
	ls_pct=string(lr_pct,'##0,###.0')+'%'
	choose case j
		case 2
			dw_rpt_fram_prod_qc_tbl_and_barhave.object.dw_2.object.t_oct.text=ls_pct
		case 3
			dw_rpt_fram_prod_qc_tbl_and_barhave.object.dw_2.object.t_nov.text=ls_pct
		case 4
			dw_rpt_fram_prod_qc_tbl_and_barhave.object.dw_2.object.t_dec.text=ls_pct
		case 5
			dw_rpt_fram_prod_qc_tbl_and_barhave.object.dw_2.object.t_jan.text=ls_pct
		case 6
			dw_rpt_fram_prod_qc_tbl_and_barhave.object.dw_2.object.t_feb.text=ls_pct
		case 7
			dw_rpt_fram_prod_qc_tbl_and_barhave.object.dw_2.object.t_mar.text=ls_pct
		case 8
			dw_rpt_fram_prod_qc_tbl_and_barhave.object.dw_2.object.t_apr.text=ls_pct
		case 9
			dw_rpt_fram_prod_qc_tbl_and_barhave.object.dw_2.object.t_may.text=ls_pct	
		case 10
			dw_rpt_fram_prod_qc_tbl_and_barhave.object.dw_2.object.t_jun.text=ls_pct
		case 11
			dw_rpt_fram_prod_qc_tbl_and_barhave.object.dw_2.object.t_jul.text=ls_pct
		case 12
			dw_rpt_fram_prod_qc_tbl_and_barhave.object.dw_2.object.t_agu.text=ls_pct
		case 13
			dw_rpt_fram_prod_qc_tbl_and_barhave.object.dw_2.object.t_sep.text=ls_pct
		case 14
			dw_rpt_fram_prod_qc_tbl_and_barhave.object.dw_2.object.t_ytd.text=ls_pct		
	end choose
next// for j=2 to 14
li_count =dw_rpt_detl_total_pcthave.RowCount()
li_sum_awad=0
li_sum_asgn=0
li_sum_ship=0
li_sum_late=0
li_sum_apr1=0
li_sum_apr2=0
li_sum_apr3=0
li_sum_apr4=0
li_sum_rej1=0
li_sum_rej2=0
li_sum_rej3=0
li_sum_rej4=0
//this loop is for computing text object in datacontrol dw1 such as reject percent for specific prdr and cntrmed and
//such as total title shipped etc.
for j=1 to li_count
	li_apr1=dw_rpt_detl_total_pcthave.GetItemNumber(j,'ctdqc1apprv0')
	if isnull(li_apr1) then li_apr1=0
	li_sum_apr1 +=li_apr1
	li_apr2=dw_rpt_detl_total_pcthave.GetItemNumber(j,'ctdqc2apprv0')
	if isnull(li_apr2) then li_apr2=0
	li_sum_apr2 +=li_apr2
	li_apr3=dw_rpt_detl_total_pcthave.GetItemNumber(j,'ctdqc3apprv0')
	if isnull(li_apr3) then li_apr3=0
	li_sum_apr3 +=li_apr3
	li_apr4=dw_rpt_detl_total_pcthave.GetItemNumber(j,'ctdqc4apprv0')
	if isnull(li_apr4) then li_apr4=0
	li_sum_apr4 +=li_apr4
	li_rej1=dw_rpt_detl_total_pcthave.GetItemNumber(j,'ctdqc1reject0')
	if isnull(li_rej1) then li_rej1=0
	li_sum_rej1 +=li_rej1
	li_rej2=dw_rpt_detl_total_pcthave.GetItemNumber(j,'ctdqc2reject0')
	if isnull(li_rej2) then li_rej2=0
	li_sum_rej2 +=li_rej2
	li_rej3=dw_rpt_detl_total_pcthave.GetItemNumber(j,'ctdqc3reject0')
	if isnull(li_rej3) then li_rej3=0
	li_sum_rej3 +=li_rej3
	li_rej4=dw_rpt_detl_total_pcthave.GetItemNumber(j,'ctdqc4reject0')
	if isnull(li_rej4) then li_rej4=0
	li_sum_rej4 +=li_rej4
	li_awad=dw_rpt_detl_total_pcthave.GetItemNumber(j,'awadtitles')
	li_sum_awad +=li_awad
	li_asgn=dw_rpt_detl_total_pcthave.GetItemNumber(j,'asgntitles')
	li_sum_asgn +=li_asgn
	li_ship=dw_rpt_detl_total_pcthave.GetItemNumber(j,'shiptitles')
	li_sum_ship +=li_ship
	li_late=dw_rpt_detl_total_pcthave.GetItemNumber(j,'latetottitles')
	li_sum_late +=li_late	
next//for j=1 to li_count total pct at bottom
if li_sum_rej1+ li_sum_apr1=0 then
	lr_pct=0
else
	lr_pct=li_sum_rej1/(li_sum_rej1 + li_sum_apr1)*100
end if
ls_pct=string(lr_pct,'##0,###.0')+'%'
dw_rpt_fram_prod_qc_tbl_and_barhave.object.dw_1.object.t_rej1.text=ls_pct
if li_sum_rej2 +li_sum_apr2=0 then
	lr_pct=0
else
	lr_pct=li_sum_rej2/(li_sum_rej2 + li_sum_apr2)*100
end if
ls_pct=string(lr_pct,'##0,###.0')+'%'
dw_rpt_fram_prod_qc_tbl_and_barhave.object.dw_1.object.t_rej2.text=ls_pct	
if li_sum_rej3 +li_sum_apr3=0 then
	lr_pct=0
else
	lr_pct=li_sum_rej3/(li_sum_rej3 + li_sum_apr3)*100
end if
ls_pct=string(lr_pct,'##0,###.0')+'%'
dw_rpt_fram_prod_qc_tbl_and_barhave.object.dw_1.object.t_rej3.text=ls_pct
if li_sum_rej4 +li_sum_apr4=0 then
	lr_pct=0
else
	lr_pct=li_sum_rej4/(li_sum_rej4 + li_sum_apr4)*100
end if
ls_pct=string(lr_pct,'##0,###.0')+'%'
dw_rpt_fram_prod_qc_tbl_and_barhave.object.dw_1.object.t_rej4.text=ls_pct
dw_rpt_fram_prod_qc_tbl_and_barhave.object.dw_1.object.t_awad.text=string(li_sum_awad)
dw_rpt_fram_prod_qc_tbl_and_barhave.object.dw_1.object.t_asgn.text=string(li_sum_asgn)
dw_rpt_fram_prod_qc_tbl_and_barhave.object.dw_1.object.t_ship.text=string(li_sum_ship)
if li_sum_ship=0 then
	lr_pct=0
else
	lr_pct=(li_sum_late/li_sum_ship)* 100
end if
ls_pct=string(lr_pct,'##0,###.0')+'%'
dw_rpt_fram_prod_qc_tbl_and_barhave.object.dw_1.object.t_late.text=ls_pct
dw_rpt_fram_prod_qc_tbl_and_barhave.object.dw_4.object.t_awad.text=string(li_sum_awad)
li_sum_apr=0
//this loop is for computing text object in datacontrol dw4 such as total reject for each month in last 12 months and
//reject percent for each stage in last 12 months
for k=3 to 15
	li_sum_rej=0
	for j=1 to 8
		ls_apr_rej=dw_rpt_qc_apr_rej_showhave.GetItemString(j,'apr_rej')
		ls_apr_rej=trim( ls_apr_rej )
		if ls_apr_rej='Rejected' then
			li_rej=dw_rpt_qc_apr_rej_showhave.GetItemNumber(j, k)
			if IsNull(li_rej) then li_rej=0
				li_sum_rej +=li_rej
		end if
		if k=15 and ls_apr_rej='Approved' then
			li_apr=dw_rpt_qc_apr_rej_showhave.GetItemNumber(j, k)
			if IsNull(li_apr) then li_apr=0
			li_sum_apr +=li_apr
		end if		
	next //for j=1 to 8
	choose case k
	case 3
		dw_rpt_fram_prod_qc_tbl_and_barhave.object.dw_4.object.t_oct.text=string(li_sum_rej)		
	case 4
		dw_rpt_fram_prod_qc_tbl_and_barhave.object.dw_4.object.t_nov.text=string(li_sum_rej)
	case 5
		dw_rpt_fram_prod_qc_tbl_and_barhave.object.dw_4.object.t_dec.text=string(li_sum_rej)		
	case 6
		dw_rpt_fram_prod_qc_tbl_and_barhave.object.dw_4.object.t_jan.text=string(li_sum_rej)	
	case 7
		dw_rpt_fram_prod_qc_tbl_and_barhave.object.dw_4.object.t_feb.text=string(li_sum_rej)		
	case 8
		dw_rpt_fram_prod_qc_tbl_and_barhave.object.dw_4.object.t_mar.text=string(li_sum_rej)
	case 9
		dw_rpt_fram_prod_qc_tbl_and_barhave.object.dw_4.object.t_apr.text=string(li_sum_rej)		
	case 10
		dw_rpt_fram_prod_qc_tbl_and_barhave.object.dw_4.object.t_may.text=string(li_sum_rej)				
	case 11
		dw_rpt_fram_prod_qc_tbl_and_barhave.object.dw_4.object.t_jun.text=string(li_sum_rej)		
	case 12
		dw_rpt_fram_prod_qc_tbl_and_barhave.object.dw_4.object.t_jul.text=string(li_sum_rej)
	case 13
		dw_rpt_fram_prod_qc_tbl_and_barhave.object.dw_4.object.t_agu.text=string(li_sum_rej)		
	case 14
		dw_rpt_fram_prod_qc_tbl_and_barhave.object.dw_4.object.t_sep.text=string(li_sum_rej)	
	case 15
		dw_rpt_fram_prod_qc_tbl_and_barhave.object.dw_4.object.t_ytd.text=string(li_sum_rej)		
		if li_sum_rej+ li_sum_apr=0 then
			lr_pct=0
		else
			lr_pct=(li_sum_rej/(li_sum_rej+ li_sum_apr))* 100
		end if
		ls_pct=string(lr_pct,'##0,###.0')+'%'
		dw_rpt_fram_prod_qc_tbl_and_barhave.object.dw_4.object.t_ytdpct.text=ls_pct	
	end choose
next // for k=1 to 15		
select max(cntrfy) into :li_max_fy
from ancntr
where prdr=:ls_prdr and cntrmed=:ls_med and
		cntrlc is not null and cntr_status='A'
using SqlServerTrans;
select max(cntrlc) into :ls_cntrlc
from ancntr
where prdr=:ls_prdr and cntrmed=:ls_med and
		cntrfy=:li_max_fy and cntrlc is not null and
		cntr_status='A'
using SqlServerTrans;
select pcs_reject, qas_reject into :lr_pcs_rej, :lr_qas_rej
from ancntr
where cntrlc=:ls_cntrlc and cntrmed= :ls_med
using SqlServerTrans;
lr_pcs_rej= lr_pcs_rej * 100
ls_pcs_rej= string(lr_pcs_rej,'###,##0.00')+'%'
dw_rpt_fram_prod_qc_tbl_and_barhave.object.dw_4.object.t_allowed.text=&
																		string(lr_qas_rej,'###,##0')	
dw_rpt_fram_prod_qc_tbl_and_barhave.object.dw_2.object.t_allowed.text=ls_pcs_rej
//dw_rpt_fram_prod_qc_tbl_and_barhave.object.dw_2.object.oct_t.text='Mar'

select max( qastat), max(prdstat) into :ls_qa_yes, :ls_pd_yes
from monrpt
where prdr= :ls_prdr and cntrmed= :ls_med and
	start_date= :ld_stdt and end_date=:ld_enddt and foreign_lang=:as_lang
using SqlServerTrans;
if not f_check_dberror(SqlServerTrans,'select qastat and prdstat from monrpt') then
	return -1
end if
if ls_qa_yes='Y' then
//	cbx_qa.checked= true
	ls_qa='Satisfactory'
else
//	cbx_qa.checked=false
	ls_qa='Not Satisfactory'
end if
if ls_pd_yes='Y' then
//	cbx_delv.checked= true
	ls_pd='Satisfactory'
else
//	cbx_delv.checked=false
	ls_pd='Not Satisfactory'
end if
dw_rpt_fram_prod_qc_tbl_and_barhave.object.t_pd_yes.text= ls_pd
dw_rpt_fram_prod_qc_tbl_and_barhave.object.t_qa_yes.text= ls_qa
dw_rpt_fram_prod_qc_tbl_and_barhave.object.t_myr.text= ls_month+' '+ls_yr+' for last 12 months'
choose case ls_med
	case 'RC'
		if as_lang='N' then
			dw_rpt_fram_prod_qc_tbl_and_barhave.object.t_book.text= 'Recorded Books'
		else
			dw_rpt_fram_prod_qc_tbl_and_barhave.object.t_book.text= 'Recorded Books (Foreign Language)'
		end if
	case 'BR'
		if as_lang='N' then
			dw_rpt_fram_prod_qc_tbl_and_barhave.object.t_book.text= 'Press Braille'
		else
			dw_rpt_fram_prod_qc_tbl_and_barhave.object.t_book.text= 'Press Braille (Foreign Language)'
		end if
		
	CASE 'P/B'
		if as_lang='N' then
			dw_rpt_fram_prod_qc_tbl_and_barhave.object.t_book.text= 'Print Braille'
		else
			dw_rpt_fram_prod_qc_tbl_and_barhave.object.t_book.text= 'Print Braille (Foreign Language)'
		end if
	case 'RTB'
		if as_lang='N' then
			dw_rpt_fram_prod_qc_tbl_and_barhave.object.t_book.text= 'Recorded Talking Books'
		else
			dw_rpt_fram_prod_qc_tbl_and_barhave.object.t_book.text= 'Recorded Talking Books (Foreign Language)'
		end if
END CHOOSE
dw_rpt_fram_prod_qc_tbl_and_barhave.object.t_datetime.text= ls_datetime
wf_rpt_text_obj_change_mon_header()
if ls_print='Y' then
	dw_rpt_fram_prod_qc_tbl_and_barhave.triggerevent('pfc_print') 
end if
//the following parts are borrow from w_pcs_reports to find average deviation
if ls_incldavg='Y' and ls_print ='Y' then
	dw_rpt_cntr_cntrlc.SetTransObject( SqlServerTrans )
	////following two datawindows come from pcsrpts.pbl
	dw_pcdeviaten_ace_report.SetTransObject( SqlServerTrans )
	dw_deviate_sum_sp_no_detail.SetTransObject( SqlServerTrans )
	dw_rpt_cntr_cntrlc.Retrieve( ls_prdr, ls_med, ld_stdt, ld_enddt)
	li_count2=dw_rpt_cntr_cntrlc.RowCount()
	if li_count2=0 then
		return 1
	end if
	ld_stdate=date(ld_stdt)
	ld_enddate=date(ld_enddt)
	for j=1 to li_count2
		ls_cntr=dw_rpt_cntr_cntrlc.GetItemString(j,'ancntr_cntr')
		li_count=dw_pcdeviaten_ace_report.Retrieve(ld_stdate, ld_enddate, ls_cntr)
		if li_count >0 then		
			dw_pcdeviaten_ace_report.object.st_stdt.text = string(ld_start_date,'mm/dd/yyyy')
			dw_pcdeviaten_ace_report.object.st_enddt.text = string(ld_end_date,'mm/dd/yyyy')
			dw_pcdeviaten_ace_report.object.st_enddt1.text = string(ld_end_date,'mm/dd/yyyy')
			dw_pcdeviaten_ace_report.print()
		ELSE
//			MessageBox("Deviation Report","There are no books matching this criteria, "+&
//					"~ndisplaying summary only.")
				//following one datawindow come from pcsrpts.pbl
			select cntrtype into :ls_cntrtype
			from ancntr
			where cntr=:ls_cntr
			using SqlServerTrans;
			if f_check_dberror(SqlServerTrans,'select cntrtype from ancntr')=false then
				return -1
			end if
			ls_end_date=string(ld_end_date,'mm/dd/yyyy')
			dw_deviate_sum_sp_no_detail.Retrieve (ls_cntr, ls_end_date, ls_cntrtype)	
			dw_deviate_sum_sp_no_detail.print() 
		END IF		
	next //for j=1 to li_count2
end if
if ls_one_prdr='N' then
	cb_next.visible=true
	cb_printall.visible=true
	cb_prev.visible=true
elseif ls_one_prdr='Y' then
	cb_next.visible=false
	cb_printall.visible=false
	cb_prev.visible=false
end if

Setpointer( Arrow!)
this.setredraw( true)
return 1

end function

public function integer wf_rpt_apr_rej_ytdlaterejpct1prdr1mhave (date cur_start_date);//this function is parallel w_rpt_apr_rej_ytdlatepct_1_prdr_1_mon, this one is for computing data for 9 new columns
//in monrpt table, such as ctdqc1reject0, ctdqc2reject0, ctdqc3reject0, all these are accumulative values

date ld_start_date, ld_end_date
datetime ldt_st, ldt_endt	
long li_row_count, i, k, li_re, rtn ,&
	 j, li_count
long 		li_yr,&
		li_apr1,li_apr2, li_apr3, li_apr4,  li_sum_apr1,li_sum_apr2, li_sum_apr3, li_sum_apr4, li_rej1, li_rej2,&
		li_rej3, li_rej4,  li_sum_rej1,li_sum_rej2, li_sum_rej3, li_sum_rej4, li_m, li_mold
string  ls_med, ls_prdr, ls_cntr,  ls_cntrlc, &
		ls_start_date, ls_end_date, &
		 ls_yr,  &
		 ls_prdr_old='', ls_med_old='', &
		ls_cntr_old='',&
		ls_cur_start_date ,ls_m,  ls_preyr, ls_print, ls_back, ls_lang='N'
boolean lb_zero= false, lb_first=false
real lr_pct
str_voucher_report lstr



lstr =istr

ls_prdr =lstr.array[3]
ls_print =lstr.array[5]
ls_med =lstr.array[13]
ls_back =lstr.array[17]
//ls_prdr_yes =lstr.array[6]
if ls_prdr='PTB' and ls_med='FL' THEN
	ls_med='RC'
	ls_lang='Y'
elseif ls_prdr='PTB' and ls_med='RC' THEN
	ls_lang='N'
end if
dw_rpt_all_colhave.settransobject(sqlservertrans)

//**********
ls_cur_start_date=string(cur_start_date,'mm/dd/yyyy')
ls_m=mid(ls_cur_start_date,1, 2)
ls_yr=mid(ls_cur_start_date,7, 4)
li_m=integer(ls_m)
li_yr=integer(ls_yr)
ls_preyr=string(li_yr - 1)
li_mold=li_m// this for end of month for last 12 months of selected year li_mold is selected month
li_m+=1 //this start month of last 12 months
if li_m> 12 then
	li_m=li_m - 12
	ls_start_date='01/01/'+ls_yr
	ls_end_date='12/01/'+ls_yr
elseif li_m<=12 then
	ls_start_date=string(li_m,'00')+'/01/'+ls_preyr
	ls_end_date=string(li_mold,'00')+'/01/'+ls_yr
end if
ld_start_date=date(ls_start_date) //pay attetion start_date is not original start_date !!!!!!!
ld_end_date=date(ls_end_date)
ldt_st=datetime(ld_start_date, time('00:00:00'))
ldt_endt=datetime(ld_end_date, time('00:00:00'))
li_row_count=dw_rpt_all_colhave.Retrieve(ls_prdr, ls_med,ldt_st, ldt_endt, ls_lang)

for i=1 to li_row_count
	dw_rpt_all_colhave.SetItem(i ,'ctdqc1reject0',0)
	dw_rpt_all_colhave.SetItem(i ,'ctdqc2reject0', 0)
	dw_rpt_all_colhave.SetItem(i ,'ctdqc3reject0', 0)
	dw_rpt_all_colhave.SetItem(i ,'ctdqc4reject0', 0)
	dw_rpt_all_colhave.SetItem(i ,'ctdqc1apprv0', 0)
	dw_rpt_all_colhave.SetItem(i ,'ctdqc2apprv0', 0)
	dw_rpt_all_colhave.SetItem(i  ,'ctdqc3apprv0', 0)
	dw_rpt_all_colhave.SetItem(i  ,'ctdqc4apprv0', 0)
next
dw_rpt_all_colhave.Sort()
li_row_count= dw_rpt_all_colhave.RowCount()
ls_prdr_old=''
ls_med_old =''
ls_cntr_old =''
li_sum_apr1=0
li_sum_apr2=0
li_sum_apr3=0
li_sum_apr4=0
li_sum_rej1=0
li_sum_rej2=0
li_sum_rej3=0
li_sum_rej4=0
//this is for computing accumulative values such as ctdqc1reject0, ctdqc2reject0 and rejpct10, rejpct20, rejpct30
for i=1 to li_row_count
	ls_med =dw_rpt_all_colhave.GetItemString(i,'cntrmed')
	ls_cntr=dw_rpt_all_colhave.GetItemString(i,'cntr')
	ls_cntrlc=trim(dw_rpt_all_colhave.GetItemString(i,'cntrlc'))
	ls_prdr =dw_rpt_all_colhave.GetItemString(i,'prdr')
	li_apr1= dw_rpt_all_colhave.GetItemNumber(i,'qc1apprv')
	li_apr2= dw_rpt_all_colhave.GetItemNumber(i,'qc2apprv')
	li_apr3= dw_rpt_all_colhave.GetItemNumber(i,'qc3apprv')
	li_apr4= dw_rpt_all_colhave.GetItemNumber(i,'qc4apprv')
	li_rej1=dw_rpt_all_colhave.GetItemNumber(i,'qc1reject')
	li_rej2=dw_rpt_all_colhave.GetItemNumber(i,'qc2reject')
	li_rej3=dw_rpt_all_colhave.GetItemNumber(i,'qc3reject')
	li_rej4=dw_rpt_all_colhave.GetItemNumber(i,'qc4reject')
	if  ls_med <>ls_med_old or ls_cntr<> ls_cntr_old or ls_prdr<>ls_prdr_old then				
		li_sum_apr1=0
		li_sum_apr2=0
		li_sum_apr3=0
		li_sum_apr4=0
		li_sum_rej1=0
		li_sum_rej2=0
		li_sum_rej3=0
		li_sum_rej4=0
	   li_sum_apr1+=li_apr1
		li_sum_apr2+=li_apr2
		li_sum_apr3+=li_apr3
		li_sum_apr4+=li_apr4
		li_sum_rej1 += li_rej1
		li_sum_rej2 += li_rej2
		li_sum_rej3 += li_rej3
		li_sum_rej4 += li_rej4
	end if //end if ls_prdr<>ls_prdrold or ls_med<>ls_medold
	if (ls_med =ls_med_old and ls_cntr= ls_cntr_old and ls_prdr=ls_prdr_old) then
		li_sum_apr1 += li_apr1
		li_sum_apr2 += li_apr2
		li_sum_apr3 += li_apr3
		li_sum_apr4 += li_apr4
		li_sum_rej1 += li_rej1
		li_sum_rej2 += li_rej2
		li_sum_rej3 += li_rej3
		li_sum_rej4 += li_rej4
	end if
	dw_rpt_all_colhave.SetItem(i ,'ctdqc1reject0', li_sum_rej1)
	dw_rpt_all_colhave.SetItem(i ,'ctdqc2reject0', li_sum_rej2)
	dw_rpt_all_colhave.SetItem(i ,'ctdqc3reject0', li_sum_rej3)
	dw_rpt_all_colhave.SetItem(i ,'ctdqc4reject0', li_sum_rej4)
	dw_rpt_all_colhave.SetItem(i ,'ctdqc1apprv0', li_sum_apr1)
	dw_rpt_all_colhave.SetItem(i ,'ctdqc2apprv0', li_sum_apr2)
	dw_rpt_all_colhave.SetItem(i ,'ctdqc3apprv0', li_sum_apr3)
	dw_rpt_all_colhave.SetItem(i ,'ctdqc4apprv0', li_sum_apr4)
	if li_sum_apr1+li_sum_rej1= 0 then
		dw_rpt_all_colhave.SetItem(i ,'rejpct10', 0)
	else
		lr_pct=li_sum_rej1/(li_sum_apr1+li_sum_rej1)
		dw_rpt_all_colhave.SetItem(i ,'rejpct10', lr_pct)
	end if
	if li_sum_apr2+li_sum_rej2=0 then
		dw_rpt_all_colhave.SetItem(i ,'rejpct20', 0)
	else
		lr_pct=li_sum_rej2/(li_sum_apr2+li_sum_rej2)
		dw_rpt_all_colhave.SetItem(i ,'rejpct20', lr_pct)
	end if
	if li_sum_apr3+li_sum_rej3=0 then
		dw_rpt_all_colhave.SetItem(i ,'rejpct30', 0)
	else
		lr_pct=li_sum_rej3/(li_sum_apr3+li_sum_rej3)
		dw_rpt_all_colhave.SetItem(i ,'rejpct30', lr_pct)
	end if
	if li_sum_apr4+li_sum_rej4= 0 then
		dw_rpt_all_colhave.SetItem(i ,'rejpct40', 0)
	else
		lr_pct=li_sum_rej4/(li_sum_apr4+li_sum_rej4)
		dw_rpt_all_colhave.SetItem(i ,'rejpct40', lr_pct)
	end if
	if ls_cntrlc ='02CLCSP1638' or ls_cntrlc='C-LC030051' THEN
		rtn=100
	end if
	ldt_st =dw_rpt_all_colhave.GetItemdatetime(i,'start_date')
	if  ls_med <>ls_med_old or ls_cntr<> ls_cntr_old or ls_prdr<>ls_prdr_old then
		ls_med_old = ls_med
		ls_cntr_old = ls_cntr
		ls_prdr_old=ls_prdr
	end if //end if ls_prdr<>ls_prdrold or ls_med<>ls_medold
next
li_re =dw_rpt_all_colhave.update()
if li_re =1 then
	commit using SqlServerTrans;
else
	Rollback using SqlServerTrans;
end if

return 1

end function

public function integer wf_rpt_text_obj_change_mon_header ();//this function is for changing the header of each month name, each run must dynamicly set each month header name
//for last 12 months.

str_voucher_report lstr
string ls_start_date, ls_m, ls_yr, ls_preyr, ls_oct, ls_nov, ls_dec, ls_jan, ls_feb, ls_mar, ls_apr, ls_may, &
		ls_jun, ls_jul, ls_aug, ls_sep
int li_m

lstr =istr

ls_start_date = lstr.array[8]
ls_m=mid(ls_start_date, 1,2)
li_m=integer(ls_m)
li_m++
ls_m=string(li_m,'00')
if li_m>12 then// this for last year start month(earliest date) go forward to this selected month of this selected year as end of
//last 12 months( latest date)
	li_m=li_m -1
end if
choose case li_m
	case 1
		ls_oct='Jan'
		ls_nov='Feb'
		ls_dec='Mar'
		ls_jan='Apr'
		ls_feb='May'
		ls_mar='Jun'
		ls_apr='Jul'
		ls_may='Aug'
		ls_jun='Sep'
		ls_jul='Oct'
		ls_aug='Nov'
		ls_sep='Dec'
	case 2	
		ls_oct='Feb'// last year feb
		ls_nov='Mar'
		ls_dec='Apr'
		ls_jan='May'
		ls_feb='Jun'
		ls_mar='Jul'
		ls_apr='Aug'
		ls_may='Sep'
		ls_jun='Oct'
		ls_jul='Nov'
		ls_aug='Dec'
		ls_sep='Jan'//this selected month of this selected year
	case 3	
		ls_oct='Mar'
		ls_nov='Apr'
		ls_dec='May'
		ls_jan='Jun'
		ls_feb='Jul'
		ls_mar='Aug'
		ls_apr='Sep'
		ls_may='Oct'
		ls_jun='Nov'
		ls_jul='Dec'
		ls_aug='Jan'
		ls_sep='Feb'
	case 4	
		ls_oct='Apr'
		ls_nov='May'
		ls_dec='Jun'
		ls_jan='Jul'
		ls_feb='Aug'
		ls_mar='Sep'
		ls_apr='Oct'
		ls_may='Nov'
		ls_jun='Dec'
		ls_jul='Jan'
		ls_aug='Feb'
		ls_sep='Mar'
	case 5	
		ls_oct='May'
		ls_nov='Jun'
		ls_dec='Jul'
		ls_jan='Aug'
		ls_feb='Sep'
		ls_mar='Oct'
		ls_apr='Nov'
		ls_may='Dec'
		ls_jun='Jan'
		ls_jul='Feb'
		ls_aug='Mar'
		ls_sep='Apr'
	case 6	
		ls_oct='Jun'
		ls_nov='Jul'
		ls_dec='Aug'
		ls_jan='Sep'
		ls_feb='Oct'
		ls_mar='Nov'
		ls_apr='Dec'
		ls_may='Jan'
		ls_jun='Feb'
		ls_jul='Mar'
		ls_aug='Apr'	
		ls_sep='May'
	case 7	
		ls_oct='Jul'
		ls_nov='Aug'
		ls_dec='Sep'
		ls_jan='Oct'
		ls_feb='Nov'
		ls_mar='Dec'
		ls_apr='Jan'
		ls_may='Feb'
		ls_jun='Mar'
		ls_jul='Apr'
		ls_aug='May'
		ls_sep='Jun'
	case 8	
		ls_oct='Aug'
		ls_nov='Sep'
		ls_dec='Oct'
		ls_jan='Nov'
		ls_feb='Dec'
		ls_mar='Jan'
		ls_apr='Feb'
		ls_may='Mar'
		ls_jun='Apr'
		ls_jul='May'
		ls_aug='Jun'
		ls_sep='Jul'
	case 9
		ls_oct='Sep'
		ls_nov='Oct'
		ls_dec='Nov'
		ls_jan='Dec'
		ls_feb='Jan'
		ls_mar='Feb'
		ls_apr='Mar'
		ls_may='Apr'
		ls_jun='May'
		ls_jul='Jun'
		ls_aug='Jul'
		ls_sep='Aug'
	case 10
		ls_oct='Oct'
		ls_nov='Nov'
		ls_dec='Dec'
		ls_jan='Jan'
		ls_feb='Feb'
		ls_mar='Mar'
		ls_apr='Apr'
		ls_may='May'
		ls_jun='Jun'
		ls_jul='Jul'
		ls_aug='Aug'
		ls_sep='Sep'
	case 11
		ls_oct='Nov'
		ls_nov='Dec'
		ls_dec='Jan'
		ls_jan='Feb'
		ls_feb='Mar'
		ls_mar='Apr'
		ls_apr='May'
		ls_may='Jun'
		ls_jun='Jul'
		ls_jul='Aug'
		ls_aug='Sep'
		ls_sep='Oct'
	case 12
		ls_oct='Dec'
		ls_nov='Jan'
		ls_dec='Feb'
		ls_jan='Mar'
		ls_feb='Apr'
		ls_mar='May'
		ls_apr='Jun'
		ls_may='Jul'
		ls_jun='Aug'
		ls_jul='Sep'
		ls_aug='Oct'
		ls_sep='Nov'
end choose
		
	
dw_rpt_fram_prod_qc_tbl_and_barhave.object.dw_2.object.oct_t.text=ls_oct
dw_rpt_fram_prod_qc_tbl_and_barhave.object.dw_2.object.nov_t.text=ls_nov
dw_rpt_fram_prod_qc_tbl_and_barhave.object.dw_2.object.dec_t.text=ls_dec
dw_rpt_fram_prod_qc_tbl_and_barhave.object.dw_2.object.jan_t.text=ls_jan
dw_rpt_fram_prod_qc_tbl_and_barhave.object.dw_2.object.feb_t.text=ls_feb
dw_rpt_fram_prod_qc_tbl_and_barhave.object.dw_2.object.mar_t.text=ls_mar
dw_rpt_fram_prod_qc_tbl_and_barhave.object.dw_2.object.apr_t.text=ls_apr
dw_rpt_fram_prod_qc_tbl_and_barhave.object.dw_2.object.may_t.text=ls_may
dw_rpt_fram_prod_qc_tbl_and_barhave.object.dw_2.object.jun_t.text=ls_jun
dw_rpt_fram_prod_qc_tbl_and_barhave.object.dw_2.object.jul_t.text=ls_jul
dw_rpt_fram_prod_qc_tbl_and_barhave.object.dw_2.object.agu_t.text=ls_aug
dw_rpt_fram_prod_qc_tbl_and_barhave.object.dw_2.object.sep_t.text=ls_sep
//****
dw_rpt_fram_prod_qc_tbl_and_barhave.object.dw_4.object.oct_t.text=ls_oct
dw_rpt_fram_prod_qc_tbl_and_barhave.object.dw_4.object.nov_t.text=ls_nov
dw_rpt_fram_prod_qc_tbl_and_barhave.object.dw_4.object.dec_t.text=ls_dec
dw_rpt_fram_prod_qc_tbl_and_barhave.object.dw_4.object.jan_t.text=ls_jan
dw_rpt_fram_prod_qc_tbl_and_barhave.object.dw_4.object.feb_t.text=ls_feb
dw_rpt_fram_prod_qc_tbl_and_barhave.object.dw_4.object.mar_t.text=ls_mar
dw_rpt_fram_prod_qc_tbl_and_barhave.object.dw_4.object.apr_t.text=ls_apr
dw_rpt_fram_prod_qc_tbl_and_barhave.object.dw_4.object.may_t.text=ls_may
dw_rpt_fram_prod_qc_tbl_and_barhave.object.dw_4.object.jun_t.text=ls_jun
dw_rpt_fram_prod_qc_tbl_and_barhave.object.dw_4.object.jul_t.text=ls_jul
dw_rpt_fram_prod_qc_tbl_and_barhave.object.dw_4.object.agu_t.text=ls_aug
dw_rpt_fram_prod_qc_tbl_and_barhave.object.dw_4.object.sep_t.text=ls_sep
return 1
end function

public function integer wf_rpt_text_obj_total_pct_allprdrsubhave (string as_prdr, string as_med, string as_lang);//this function for show all prdr(as_prdr, as_med are input in) for last 12 months
date ld_start_date, ld_end_date, ld_stdate,ld_enddate

datetime ldt_datetime
long li_row_count, i, k,i2, &
		li_apr1,li_apr2, li_apr3, li_apr4, li_sum_apr1,li_sum_apr2, li_sum_apr3,li_sum_apr4, li_rej1, li_rej2,&
	  j, li_count,li_count2,li_apr, li_sum_apr
long  li_acttitles, li_fy,&		
		li_rej3, li_rej4,  li_sum_rej1,li_sum_rej2, li_sum_rej3,li_sum_rej4,  li_rej,li_sum_rej,li_allowed, &
		li_ship,li_sum_ship,li_late, li_sum_late,li_awad, li_sum_awad,li_asgn,li_sum_asgn,&
		li_max_fy
string  ls_med, ls_prdr,ls_cntrlc,ls_cntr, ls_pct,ls_apr_rej, ls_pcs_rej,ls_print,  &
		ls_start_date, ls_end_date, ls_fy_start_date, ls_fy_end_date, ls_fy,ls_yr, ls_month, &
		ls_pd_yes, ls_qa_yes, ls_pd, ls_qa,ls_max,ls_min,ls_lang	,	ls_incldavg,ls_datetime,&
		ls_cntrtype, ls_back
datetime ld_stdt, ld_enddt		
		 
		
		
boolean lb_zero= false
real lr_pct, lr_allowed,lr_pcs_rej, lr_qas_rej
str_voucher_report lstr
n_ds lds_agm

lstr =istr
this.SetRedraw( false )
dw_rpt_fram_prod_qc_tbl_and_bar.visible=false
dw_rpt_fram_prod_qc_tbl_and_barhave.visible=true
ldt_datetime= datetime(Today(),Now())
ls_datetime= string( ldt_datetime,'mm/dd/yyyy hh:mm AM/PM')
//Setpointer( Hourglass!)
ls_month =lstr.array[1]
ls_yr =lstr.array[2]
//ls_prdr =lstr.array[3]
ls_print =lstr.array[5]
//ls_med =lstr.array[13]
ls_prdr=as_prdr
ls_med=as_med
ls_lang=as_lang
ls_incldavg=lstr.array[15]
ls_back =lstr.array[17]


ls_start_date = lstr.array[8]
ls_end_date = (lstr.array[9])
ls_fy_start_date = lstr.array[10]
ls_fy_end_date = (lstr.array[11])
ls_incldavg = lstr.array[15]
ld_start_date= date( ls_start_date)
ld_end_date =date( ls_end_date )

ls_fy= right(ls_fy_end_date, 4)
li_fy =integer( ls_fy )
//this function for create accumulative data for last 12 month such as ytdappr0, ytdreject0, ytdrejpct0 etc.
//this function only for last 12 months. for fiscal year do not need this function
wf_rpt_apr_rej_ytdltrejpctallprdr1mhave(ls_prdr, ls_med, ls_lang,ld_start_date)

dw_rpt_fram_prod_qc_tbl_and_barhave.SetTransObject( SqlServerTrans )
dw_rpt_sum_cmpt_pcthave.SetTransObject( SqlServerTrans )
dw_rpt_detl_total_pcthave.SetTransObject( SqlServerTrans )
dw_rpt_qc_apr_rej_showhave.SetTransObject( SqlServerTrans )
ld_stdt=datetime(ld_start_date,time('00:00:00'))
ld_enddt=datetime(ld_end_date,time('00:00:00'))
//this is new datawindow fram d_
j=dw_rpt_fram_prod_qc_tbl_and_barhave.Retrieve&
						(ls_prdr,ls_med,ld_stdt,as_lang)
Setpointer( Hourglass!)
//this is for computing text object of dw2 sub datacontrol percentage new datawindow d_
k=dw_rpt_sum_cmpt_pcthave.Retrieve&
						(ls_prdr,ls_med,ld_stdt,as_lang)
//this is for computing text object of total and percentage number in dw1 sub datacotrol 
i=dw_rpt_detl_total_pcthave.Retrieve&
						(ls_prdr,ls_med,ld_stdt,as_lang)
						
Setpointer( Hourglass!)
//this is for computing text object of total and percentage number in dw4 sub datacontrol
i2=dw_rpt_qc_apr_rej_showhave.Retrieve&
						(ls_prdr,ls_med,ld_stdt,as_lang)
Setpointer( Hourglass!)
//this for loop for computing reject percentage of last 12 months
for j=2 to 14
	li_acttitles=dw_rpt_sum_cmpt_pcthave.GetItemNumber(2,j)
	if isnull(li_acttitles) then li_acttitles=0
		
	li_late=dw_rpt_sum_cmpt_pcthave.GetItemNumber(4,j)
	if li_acttitles =0 then
		lr_pct=0
	else
		lr_pct=(li_late/li_acttitles)*100
	end if
	ls_pct=string(lr_pct,'##0,###.0')+'%'
	choose case j
		case 2
			dw_rpt_fram_prod_qc_tbl_and_barhave.object.dw_2.object.t_oct.text=ls_pct
		case 3
			dw_rpt_fram_prod_qc_tbl_and_barhave.object.dw_2.object.t_nov.text=ls_pct
		case 4
			dw_rpt_fram_prod_qc_tbl_and_barhave.object.dw_2.object.t_dec.text=ls_pct
		case 5
			dw_rpt_fram_prod_qc_tbl_and_barhave.object.dw_2.object.t_jan.text=ls_pct
		case 6
			dw_rpt_fram_prod_qc_tbl_and_barhave.object.dw_2.object.t_feb.text=ls_pct
		case 7
			dw_rpt_fram_prod_qc_tbl_and_barhave.object.dw_2.object.t_mar.text=ls_pct
		case 8
			dw_rpt_fram_prod_qc_tbl_and_barhave.object.dw_2.object.t_apr.text=ls_pct
		case 9
			dw_rpt_fram_prod_qc_tbl_and_barhave.object.dw_2.object.t_may.text=ls_pct	
		case 10
			dw_rpt_fram_prod_qc_tbl_and_barhave.object.dw_2.object.t_jun.text=ls_pct
		case 11
			dw_rpt_fram_prod_qc_tbl_and_barhave.object.dw_2.object.t_jul.text=ls_pct
		case 12
			dw_rpt_fram_prod_qc_tbl_and_barhave.object.dw_2.object.t_agu.text=ls_pct
		case 13
			dw_rpt_fram_prod_qc_tbl_and_barhave.object.dw_2.object.t_sep.text=ls_pct
		case 14
			dw_rpt_fram_prod_qc_tbl_and_barhave.object.dw_2.object.t_ytd.text=ls_pct		
	end choose
next// for j=2 to 14
li_count =dw_rpt_detl_total_pcthave.RowCount()
li_sum_awad=0
li_sum_asgn=0
li_sum_ship=0
li_sum_late=0
li_sum_apr1=0
li_sum_apr2=0
li_sum_apr3=0
li_sum_apr4=0
li_sum_rej1=0
li_sum_rej2=0
li_sum_rej3=0
li_sum_rej4=0
//this loop for computing total and percentage of exst object in dw1 sub datacontrol
for j=1 to li_count
	li_apr1=dw_rpt_detl_total_pcthave.GetItemNumber(j,'ctdqc1apprv0')
	if isnull(li_apr1) then li_apr1=0
	li_sum_apr1 +=li_apr1
	li_apr2=dw_rpt_detl_total_pcthave.GetItemNumber(j,'ctdqc2apprv0')
	if isnull(li_apr2) then li_apr2=0
	li_sum_apr2 +=li_apr2
	li_apr3=dw_rpt_detl_total_pcthave.GetItemNumber(j,'ctdqc3apprv0')
	if isnull(li_apr3) then li_apr3=0
	li_sum_apr3 +=li_apr3
	li_apr4=dw_rpt_detl_total_pcthave.GetItemNumber(j,'ctdqc4apprv0')
	if isnull(li_apr4) then li_apr4=0
	li_sum_apr4 +=li_apr4
	li_rej1=dw_rpt_detl_total_pcthave.GetItemNumber(j,'ctdqc1reject0')
	if isnull(li_rej1) then li_rej1=0
	li_sum_rej1 +=li_rej1
	li_rej2=dw_rpt_detl_total_pcthave.GetItemNumber(j,'ctdqc2reject0')
	if isnull(li_rej2) then li_rej2=0
	li_sum_rej2 +=li_rej2
	li_rej3=dw_rpt_detl_total_pcthave.GetItemNumber(j,'ctdqc3reject0')
	if isnull(li_rej3) then li_rej3=0
	li_sum_rej3 +=li_rej3
	li_rej4=dw_rpt_detl_total_pcthave.GetItemNumber(j,'ctdqc4reject0')
	if isnull(li_rej4) then li_rej4=0
	li_sum_rej4 +=li_rej4
	li_awad=dw_rpt_detl_total_pcthave.GetItemNumber(j,'awadtitles')
	li_sum_awad +=li_awad
	li_asgn=dw_rpt_detl_total_pcthave.GetItemNumber(j,'asgntitles')
	li_sum_asgn +=li_asgn
	li_ship=dw_rpt_detl_total_pcthave.GetItemNumber(j,'shiptitles')
	li_sum_ship +=li_ship
	li_late=dw_rpt_detl_total_pcthave.GetItemNumber(j,'latetottitles')
	li_sum_late +=li_late	
next//for j=1 to li_count total pct at bottom
if li_sum_rej1+ li_sum_apr1=0 then
	lr_pct=0
else
	lr_pct=li_sum_rej1/(li_sum_rej1 + li_sum_apr1)*100
end if
ls_pct=string(lr_pct,'##0,###.0')+'%'
dw_rpt_fram_prod_qc_tbl_and_barhave.object.dw_1.object.t_rej1.text=ls_pct
if li_sum_rej2 +li_sum_apr2=0 then
	lr_pct=0
else
	lr_pct=li_sum_rej2/(li_sum_rej2 + li_sum_apr2)*100
end if
ls_pct=string(lr_pct,'##0,###.0')+'%'
dw_rpt_fram_prod_qc_tbl_and_barhave.object.dw_1.object.t_rej2.text=ls_pct	
if li_sum_rej3 +li_sum_apr3=0 then
	lr_pct=0
else
	lr_pct=li_sum_rej3/(li_sum_rej3 + li_sum_apr3)*100
end if
ls_pct=string(lr_pct,'##0,###.0')+'%'
dw_rpt_fram_prod_qc_tbl_and_barhave.object.dw_1.object.t_rej3.text=ls_pct
if li_sum_rej4 +li_sum_apr4=0 then
	lr_pct=0
else
	lr_pct=li_sum_rej4/(li_sum_rej4 + li_sum_apr4)*100
end if
ls_pct=string(lr_pct,'##0,###.0')+'%'
dw_rpt_fram_prod_qc_tbl_and_barhave.object.dw_1.object.t_rej4.text=ls_pct
dw_rpt_fram_prod_qc_tbl_and_barhave.object.dw_1.object.t_awad.text=string(li_sum_awad)
dw_rpt_fram_prod_qc_tbl_and_barhave.object.dw_1.object.t_asgn.text=string(li_sum_asgn)
dw_rpt_fram_prod_qc_tbl_and_barhave.object.dw_1.object.t_ship.text=string(li_sum_ship)
if li_sum_ship=0 then
	lr_pct=0
else
	lr_pct=(li_sum_late/li_sum_ship)* 100
end if
ls_pct=string(lr_pct,'##0,###.0')+'%'
dw_rpt_fram_prod_qc_tbl_and_barhave.object.dw_1.object.t_late.text=ls_pct
//this is set text object in dw4 as total title awarted
dw_rpt_fram_prod_qc_tbl_and_barhave.object.dw_4.object.t_awad.text=string(li_sum_awad)
li_sum_apr=0
//this loop is for computing text object in dw4 sub datacontrol to compute total reject and reject percent
for k=3 to 15
	li_sum_rej=0
	for j=1 to 8
		ls_apr_rej=dw_rpt_qc_apr_rej_showhave.GetItemString(j,'apr_rej')
		ls_apr_rej=trim( ls_apr_rej )
		if ls_apr_rej='Rejected' then
			li_rej=dw_rpt_qc_apr_rej_showhave.GetItemNumber(j, k)
			if IsNull(li_rej) then li_rej=0
				li_sum_rej +=li_rej
		end if
		if k=15 and ls_apr_rej='Approved' then
			li_apr=dw_rpt_qc_apr_rej_showhave.GetItemNumber(j, k)
			if IsNull(li_apr) then li_apr=0
			li_sum_apr +=li_apr
		end if		
	next //for j=1 to 8
	choose case k
	case 3
		dw_rpt_fram_prod_qc_tbl_and_barhave.object.dw_4.object.t_oct.text=string(li_sum_rej)		
	case 4
		dw_rpt_fram_prod_qc_tbl_and_barhave.object.dw_4.object.t_nov.text=string(li_sum_rej)
	case 5
		dw_rpt_fram_prod_qc_tbl_and_barhave.object.dw_4.object.t_dec.text=string(li_sum_rej)		
	case 6
		dw_rpt_fram_prod_qc_tbl_and_barhave.object.dw_4.object.t_jan.text=string(li_sum_rej)	
	case 7
		dw_rpt_fram_prod_qc_tbl_and_barhave.object.dw_4.object.t_feb.text=string(li_sum_rej)		
	case 8
		dw_rpt_fram_prod_qc_tbl_and_barhave.object.dw_4.object.t_mar.text=string(li_sum_rej)
	case 9
		dw_rpt_fram_prod_qc_tbl_and_barhave.object.dw_4.object.t_apr.text=string(li_sum_rej)		
	case 10
		dw_rpt_fram_prod_qc_tbl_and_barhave.object.dw_4.object.t_may.text=string(li_sum_rej)				
	case 11
		dw_rpt_fram_prod_qc_tbl_and_barhave.object.dw_4.object.t_jun.text=string(li_sum_rej)		
	case 12
		dw_rpt_fram_prod_qc_tbl_and_barhave.object.dw_4.object.t_jul.text=string(li_sum_rej)
	case 13
		dw_rpt_fram_prod_qc_tbl_and_barhave.object.dw_4.object.t_agu.text=string(li_sum_rej)		
	case 14
		dw_rpt_fram_prod_qc_tbl_and_barhave.object.dw_4.object.t_sep.text=string(li_sum_rej)	
	case 15
		dw_rpt_fram_prod_qc_tbl_and_barhave.object.dw_4.object.t_ytd.text=string(li_sum_rej)		
		if li_sum_rej+ li_sum_apr=0 then
			lr_pct=0
		else
			lr_pct=(li_sum_rej/(li_sum_rej+ li_sum_apr))* 100
		end if
		ls_pct=string(lr_pct,'##0,###.0')+'%'
		dw_rpt_fram_prod_qc_tbl_and_barhave.object.dw_4.object.t_ytdpct.text=ls_pct	
	end choose
next // for k=1 to 15		
select max(cntrfy) into :li_max_fy
from ancntr
where prdr=:ls_prdr and cntrmed=:ls_med and
		cntrlc is not null and cntr_status='A'
using SqlServerTrans;
select max(cntrlc) into :ls_cntrlc
from ancntr
where prdr=:ls_prdr and cntrmed=:ls_med and
		cntrfy=:li_max_fy and cntrlc is not null and
		cntr_status='A'
using SqlServerTrans;
select pcs_reject, qas_reject into :lr_pcs_rej, :lr_qas_rej
from ancntr
where cntrlc=:ls_cntrlc and cntrmed= :ls_med
using SqlServerTrans;
lr_pcs_rej= lr_pcs_rej * 100
ls_pcs_rej= string(lr_pcs_rej,'###,##0.00')+'%'
dw_rpt_fram_prod_qc_tbl_and_barhave.object.dw_4.object.t_allowed.text=&
																		string(lr_qas_rej,'###,##0')	
dw_rpt_fram_prod_qc_tbl_and_barhave.object.dw_2.object.t_allowed.text=ls_pcs_rej


select max( qastat), max(prdstat) into :ls_qa_yes, :ls_pd_yes
from monrpt
where prdr= :ls_prdr and cntrmed= :ls_med and
	start_date= :ld_stdt and end_date=:ld_enddt and foreign_lang=:as_lang
using SqlServerTrans;
if not f_check_dberror(SqlServerTrans,'select qastat and prdstat from monrpt') then
	return -1
end if
if ls_qa_yes='Y' then
//	cbx_qa.checked= true
	ls_qa='Satisfactory'
else
//	cbx_qa.checked=false
	ls_qa='Not Satisfactory'
end if
if ls_pd_yes='Y' then
//	cbx_delv.checked= true
	ls_pd='Satisfactory'
else
//	cbx_delv.checked=false
	ls_pd='Not Satisfactory'
end if
dw_rpt_fram_prod_qc_tbl_and_barhave.object.t_pd_yes.text= ls_pd
dw_rpt_fram_prod_qc_tbl_and_barhave.object.t_qa_yes.text= ls_qa
dw_rpt_fram_prod_qc_tbl_and_barhave.object.t_myr.text= ls_month+' '+ls_yr+' for last 12 months'
choose case ls_med
	case 'RC'
		if as_lang='N' then
			dw_rpt_fram_prod_qc_tbl_and_barhave.object.t_book.text= 'Recorded Books'
		else
			dw_rpt_fram_prod_qc_tbl_and_barhave.object.t_book.text= 'Recorded Books (Foreign Language)'
		end if
	case 'BR'
		if as_lang='N' then
			dw_rpt_fram_prod_qc_tbl_and_barhave.object.t_book.text= 'Press Braille'
		else
			dw_rpt_fram_prod_qc_tbl_and_barhave.object.t_book.text= 'Press Braille (Foreign Language)'
		end if
		
	CASE 'P/B'
		if as_lang='N' then
			dw_rpt_fram_prod_qc_tbl_and_barhave.object.t_book.text= 'Print Braille'
		else
			dw_rpt_fram_prod_qc_tbl_and_barhave.object.t_book.text= 'Print Braille (Foreign Language)'
		end if
	case 'RTB'
		if as_lang='N' then
			dw_rpt_fram_prod_qc_tbl_and_barhave.object.t_book.text= 'Recorded Talking Books'
		else
			dw_rpt_fram_prod_qc_tbl_and_barhave.object.t_book.text= 'Recorded Talking Books (Foreign Language)'
		end if
END CHOOSE
dw_rpt_fram_prod_qc_tbl_and_barhave.object.t_datetime.text= ls_datetime
//this function for dynamicly set column head text
wf_rpt_text_obj_change_mon_header()
i_count++
if ls_print='Y' and i_count=1 then
	dw_rpt_fram_prod_qc_tbl_and_barhave.triggerevent('pfc_print') 
elseif ls_print='Y' and i_count>1 then
	dw_rpt_fram_prod_qc_tbl_and_barhave.print()
end if
//the following parts come from w_pcs_reports
if ls_incldavg='Y' and ls_print ='Y' then
	dw_rpt_cntr_cntrlc.SetTransObject( SqlServerTrans )
	////following two datawindows come from pcsrpts.pbl
	dw_pcdeviaten_ace_report.SetTransObject( SqlServerTrans )
	dw_deviate_sum_sp_no_detail.SetTransObject( SqlServerTrans )
	dw_rpt_cntr_cntrlc.Retrieve( ls_prdr, ls_med, ld_stdt, ld_enddt)
	li_count2=dw_rpt_cntr_cntrlc.RowCount()
	if li_count2=0 then
		return 1
	end if
	ld_stdate=date(ld_stdt)
	ld_enddate=date(ld_enddt)
	for j=1 to li_count2
		ls_cntr=dw_rpt_cntr_cntrlc.GetItemString(j,'ancntr_cntr')
		li_count=dw_pcdeviaten_ace_report.Retrieve(ld_stdate, ld_enddate, ls_cntr)
		if li_count >0 then		
			dw_pcdeviaten_ace_report.object.st_stdt.text = string(ld_start_date,'mm/dd/yyyy')
			dw_pcdeviaten_ace_report.object.st_enddt.text = string(ld_end_date,'mm/dd/yyyy')
			dw_pcdeviaten_ace_report.object.st_enddt1.text = string(ld_end_date,'mm/dd/yyyy')
			dw_pcdeviaten_ace_report.print()
		ELSE
//			MessageBox("Deviation Report","There are no books matching this criteria, "+&
//					"~ndisplaying summary only.")
				//following one datawindow come from pcsrpts.pbl
			select cntrtype into :ls_cntrtype
			from ancntr
			where cntr=:ls_cntr
			using SqlServerTrans;
			if f_check_dberror(SqlServerTrans,'select cntrtype from ancntr')=false then
				return -1
			end if
			ls_end_date=string(ld_end_date,'mm/dd/yyyy')
			dw_deviate_sum_sp_no_detail.Retrieve (ls_cntr, ls_end_date, ls_cntrtype)	
			dw_deviate_sum_sp_no_detail.print() 
		END IF		
	next //for j=1 to li_count2
end if

cb_next.visible=true
cb_printall.visible=true
cb_prev.visible=true

Setpointer( Arrow!)
this.setredraw( true)
return 1

end function

public function integer wf_rpt_apr_rej_ytdltrejpctallprdr1mhave (string as_prdr, string as_med, string as_lang, date cur_start_date);//this is for computing new data for new column in monrpt such as ytdapprv0, ytdreject0, ytdrejpct0 etc.
//the new data only created for 9 new columns. This data user as text object in sub datacontrol dw1, dw2, dw4
//this function is pararell the function w_rpt_apr_rej_ytd_laterejpct_all_prdr_1_mon(). the as_prdr, as_med,
//as_lang, cur_start_date is argument list. here only compute the accumulative values, not interested individual
//value for each month

date ld_start_date, ld_end_date
datetime ldt_st, ldt_endt	
long li_row_count, i, k, li_re, rtn ,&
	 j, li_count
long 		li_yr,&
		li_apr1,li_apr2, li_apr3, li_apr4, li_sum_apr1,li_sum_apr2, li_sum_apr3, li_sum_apr4,  li_rej1, li_rej2,&
		li_rej3, li_rej4,  li_sum_rej1,li_sum_rej2, li_sum_rej3,li_sum_rej4, li_m, li_mold
string  ls_med, ls_prdr, ls_cntr,  ls_cntrlc, &
		ls_start_date, ls_end_date, &
		 ls_yr,  &
		 ls_prdr_old='', ls_med_old='', &
		ls_cntr_old='',&
		ls_cur_start_date ,ls_m,  ls_preyr, ls_print, ls_back, ls_lang='N'
boolean lb_zero= false, lb_first=false
real lr_pct
str_voucher_report lstr



lstr =istr
ls_prdr=as_prdr
ls_med=as_med
ls_lang=as_lang
//ls_prdr =lstr.array[3]
ls_print =lstr.array[5]
//ls_med =lstr.array[13]
ls_back =lstr.array[17]

dw_rpt_all_colhave.settransobject(sqlservertrans)

//**********
ls_cur_start_date=string(cur_start_date,'mm/dd/yyyy')
ls_m=mid(ls_cur_start_date,1, 2)
ls_yr=mid(ls_cur_start_date,7, 4)
li_m=integer(ls_m)
li_yr=integer(ls_yr)
ls_preyr=string(li_yr - 1)
li_mold=li_m
li_m+=1
//this is set last 12 month start month li_m +1 means of one year ago momth number as start month, this is set automaticlly
//for ls_start_date and ls_end_date for last 12 month. The ls_end_date is you choose the month and you choose the year.
if li_m> 12 then
	li_m=li_m - 12
	ls_start_date='01/01/'+ls_yr
	ls_end_date='12/01/'+ls_yr
elseif li_m<=12 then
	ls_start_date=string(li_m,'00')+'/01/'+ls_preyr
	ls_end_date=string(li_mold,'00')+'/01/'+ls_yr
end if
ld_start_date=date(ls_start_date) //pay attetion start_date is not original start_date !!!!!!!
ld_end_date=date(ls_end_date)
ldt_st=datetime(ld_start_date, time('00:00:00'))
ldt_endt=datetime(ld_end_date, time('00:00:00'))
li_row_count=dw_rpt_all_colhave.Retrieve(ls_prdr, ls_med,ldt_st, ldt_endt, ls_lang)
//this for loop is for computing ytd accumulative values such as ctdqc1reject0, ctdqc2reject0...etc.
for i=1 to li_row_count
	dw_rpt_all_colhave.SetItem(i ,'ctdqc1reject0',0)
	dw_rpt_all_colhave.SetItem(i ,'ctdqc2reject0', 0)
	dw_rpt_all_colhave.SetItem(i ,'ctdqc3reject0', 0)
	dw_rpt_all_colhave.SetItem(i ,'ctdqc4reject0', 0)
	dw_rpt_all_colhave.SetItem(i ,'ctdqc1apprv0', 0)
	dw_rpt_all_colhave.SetItem(i ,'ctdqc2apprv0', 0)
	dw_rpt_all_colhave.SetItem(i  ,'ctdqc3apprv0', 0)
	dw_rpt_all_colhave.SetItem(i  ,'ctdqc4apprv0', 0)
next
dw_rpt_all_colhave.Sort()
li_row_count= dw_rpt_all_colhave.RowCount()
ls_prdr_old=''
ls_med_old =''
ls_cntr_old =''
li_sum_apr1=0
li_sum_apr2=0
li_sum_apr3=0
li_sum_apr4=0
li_sum_rej1=0
li_sum_rej2=0
li_sum_rej3=0
li_sum_rej4=0
for i=1 to li_row_count
	ls_med =dw_rpt_all_colhave.GetItemString(i,'cntrmed')
	ls_cntr=dw_rpt_all_colhave.GetItemString(i,'cntr')
	ls_cntrlc=trim(dw_rpt_all_colhave.GetItemString(i,'cntrlc'))
	ls_prdr =dw_rpt_all_colhave.GetItemString(i,'prdr')
	li_apr1= dw_rpt_all_colhave.GetItemNumber(i,'qc1apprv')
	li_apr2= dw_rpt_all_colhave.GetItemNumber(i,'qc2apprv')
	li_apr3= dw_rpt_all_colhave.GetItemNumber(i,'qc3apprv')
	li_apr4= dw_rpt_all_colhave.GetItemNumber(i,'qc4apprv')
	li_rej1=dw_rpt_all_colhave.GetItemNumber(i,'qc1reject')
	li_rej2=dw_rpt_all_colhave.GetItemNumber(i,'qc2reject')
	li_rej3=dw_rpt_all_colhave.GetItemNumber(i,'qc3reject')
	li_rej4=dw_rpt_all_colhave.GetItemNumber(i,'qc4reject')
	if  ls_med <>ls_med_old or ls_cntr<> ls_cntr_old or ls_prdr<>ls_prdr_old then				
		li_sum_apr1=0
		li_sum_apr2=0
		li_sum_apr3=0
		li_sum_apr4=0
		li_sum_rej1=0
		li_sum_rej2=0
		li_sum_rej3=0
		li_sum_rej4=0
	   li_sum_apr1+=li_apr1
		li_sum_apr2+=li_apr2
		li_sum_apr3+=li_apr3
		li_sum_apr4+=li_apr4
		li_sum_rej1 += li_rej1
		li_sum_rej2 += li_rej2
		li_sum_rej3 += li_rej3
		li_sum_rej4 += li_rej4
	end if //end if ls_prdr<>ls_prdrold or ls_med<>ls_medold
	if (ls_med =ls_med_old and ls_cntr= ls_cntr_old and ls_prdr=ls_prdr_old) then
		li_sum_apr1 += li_apr1
		li_sum_apr2 += li_apr2
		li_sum_apr3 += li_apr3
		li_sum_apr4 += li_apr4
		li_sum_rej1 += li_rej1
		li_sum_rej2 += li_rej2
		li_sum_rej3 += li_rej3
		li_sum_rej4 += li_rej4
	end if
	dw_rpt_all_colhave.SetItem(i ,'ctdqc1reject0', li_sum_rej1)
	dw_rpt_all_colhave.SetItem(i ,'ctdqc2reject0', li_sum_rej2)
	dw_rpt_all_colhave.SetItem(i ,'ctdqc3reject0', li_sum_rej3)
	dw_rpt_all_colhave.SetItem(i ,'ctdqc4reject0', li_sum_rej4)
	dw_rpt_all_colhave.SetItem(i ,'ctdqc1apprv0', li_sum_apr1)
	dw_rpt_all_colhave.SetItem(i ,'ctdqc2apprv0', li_sum_apr2)
	dw_rpt_all_colhave.SetItem(i ,'ctdqc3apprv0', li_sum_apr3)
	dw_rpt_all_colhave.SetItem(i ,'ctdqc4apprv0', li_sum_apr4)
	if li_sum_apr1+li_sum_rej1= 0 then
		dw_rpt_all_colhave.SetItem(i ,'rejpct10', 0)
	else
		lr_pct=li_sum_rej1/(li_sum_apr1+li_sum_rej1)
		dw_rpt_all_colhave.SetItem(i ,'rejpct10', lr_pct)
	end if
	if li_sum_apr2+li_sum_rej2=0 then
		dw_rpt_all_colhave.SetItem(i ,'rejpct20', 0)
	else
		lr_pct=li_sum_rej2/(li_sum_apr2+li_sum_rej2)
		dw_rpt_all_colhave.SetItem(i ,'rejpct20', lr_pct)
	end if
	if li_sum_apr3+li_sum_rej3=0 then
		dw_rpt_all_colhave.SetItem(i ,'rejpct30', 0)
	else
		lr_pct=li_sum_rej3/(li_sum_apr3+li_sum_rej3)
		dw_rpt_all_colhave.SetItem(i ,'rejpct30', lr_pct)
	end if
	if li_sum_apr4+li_sum_rej4= 0 then
		dw_rpt_all_colhave.SetItem(i ,'rejpct40', 0)
	else
		lr_pct=li_sum_rej4/(li_sum_apr4+li_sum_rej4)
		dw_rpt_all_colhave.SetItem(i ,'rejpct40', lr_pct)
	end if
	if ls_cntrlc ='02CLCSP1638' or ls_cntrlc='C-LC030051' THEN
		rtn=100
	end if
	ldt_st =dw_rpt_all_colhave.GetItemdatetime(i,'start_date')
	if  ls_med <>ls_med_old or ls_cntr<> ls_cntr_old or ls_prdr<>ls_prdr_old then
		ls_med_old = ls_med
		ls_cntr_old = ls_cntr
		ls_prdr_old=ls_prdr
	end if //end if ls_prdr<>ls_prdrold or ls_med<>ls_medold
next
li_re =dw_rpt_all_colhave.update()
if li_re =1 then
	commit using SqlServerTrans;
else
	Rollback using SqlServerTrans;
end if

return 1

end function

on w_month_reporthave.create
int iCurrent
call super::create
this.dw_rpt_all_colhave=create dw_rpt_all_colhave
this.dw_rpt_qc_apr_rej_showhave=create dw_rpt_qc_apr_rej_showhave
this.dw_rpt_detl_total_pcthave=create dw_rpt_detl_total_pcthave
this.dw_rpt_sum_cmpt_pcthave=create dw_rpt_sum_cmpt_pcthave
this.cb_prev=create cb_prev
this.cb_next=create cb_next
this.cb_printall=create cb_printall
this.cb_option=create cb_option
this.dw_deviate_sum_sp_no_detail=create dw_deviate_sum_sp_no_detail
this.dw_rpt_cntr_cntrlc=create dw_rpt_cntr_cntrlc
this.dw_rpt_commts_in_amonth=create dw_rpt_commts_in_amonth
this.dw_rpt_qc_apr_rej_show_total=create dw_rpt_qc_apr_rej_show_total
this.dw_rpt_detl_total_pct=create dw_rpt_detl_total_pct
this.dw_rpt_sum_cmpt_pct=create dw_rpt_sum_cmpt_pct
this.cb_print=create cb_print
this.cb_exit=create cb_exit
this.dw_storeproc=create dw_storeproc
this.dw_rpt_fram_prod_qc_tbl_and_bar=create dw_rpt_fram_prod_qc_tbl_and_bar
this.dw_rpt_sum_prod_plan2=create dw_rpt_sum_prod_plan2
this.dw_rpt_fram_prod_qc_tbl_and_barhave=create dw_rpt_fram_prod_qc_tbl_and_barhave
this.dw_pcdeviaten_ace_report=create dw_pcdeviaten_ace_report
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_rpt_all_colhave
this.Control[iCurrent+2]=this.dw_rpt_qc_apr_rej_showhave
this.Control[iCurrent+3]=this.dw_rpt_detl_total_pcthave
this.Control[iCurrent+4]=this.dw_rpt_sum_cmpt_pcthave
this.Control[iCurrent+5]=this.cb_prev
this.Control[iCurrent+6]=this.cb_next
this.Control[iCurrent+7]=this.cb_printall
this.Control[iCurrent+8]=this.cb_option
this.Control[iCurrent+9]=this.dw_deviate_sum_sp_no_detail
this.Control[iCurrent+10]=this.dw_rpt_cntr_cntrlc
this.Control[iCurrent+11]=this.dw_rpt_commts_in_amonth
this.Control[iCurrent+12]=this.dw_rpt_qc_apr_rej_show_total
this.Control[iCurrent+13]=this.dw_rpt_detl_total_pct
this.Control[iCurrent+14]=this.dw_rpt_sum_cmpt_pct
this.Control[iCurrent+15]=this.cb_print
this.Control[iCurrent+16]=this.cb_exit
this.Control[iCurrent+17]=this.dw_storeproc
this.Control[iCurrent+18]=this.dw_rpt_fram_prod_qc_tbl_and_bar
this.Control[iCurrent+19]=this.dw_rpt_sum_prod_plan2
this.Control[iCurrent+20]=this.dw_rpt_fram_prod_qc_tbl_and_barhave
this.Control[iCurrent+21]=this.dw_pcdeviaten_ace_report
end on

on w_month_reporthave.destroy
call super::destroy
destroy(this.dw_rpt_all_colhave)
destroy(this.dw_rpt_qc_apr_rej_showhave)
destroy(this.dw_rpt_detl_total_pcthave)
destroy(this.dw_rpt_sum_cmpt_pcthave)
destroy(this.cb_prev)
destroy(this.cb_next)
destroy(this.cb_printall)
destroy(this.cb_option)
destroy(this.dw_deviate_sum_sp_no_detail)
destroy(this.dw_rpt_cntr_cntrlc)
destroy(this.dw_rpt_commts_in_amonth)
destroy(this.dw_rpt_qc_apr_rej_show_total)
destroy(this.dw_rpt_detl_total_pct)
destroy(this.dw_rpt_sum_cmpt_pct)
destroy(this.cb_print)
destroy(this.cb_exit)
destroy(this.dw_storeproc)
destroy(this.dw_rpt_fram_prod_qc_tbl_and_bar)
destroy(this.dw_rpt_sum_prod_plan2)
destroy(this.dw_rpt_fram_prod_qc_tbl_and_barhave)
destroy(this.dw_pcdeviaten_ace_report)
end on

event resize;call super::resize;long ll_height

This.X = w_pics_main.X
This.Y = w_pics_main.Y
//ll_height = w_pics_main.mdi_1.Height
ll_height = w_pics_main.height
This.Resize(w_pics_main.WorkSpaceWidth(), ll_height)

end event

event pfc_preopen;call super::pfc_preopen;this.windowstate =maximized!
this.of_SetBase(TRUE)
this.inv_base.of_Center()
this.title='Producer Monthly Report for last 12 months'
this.of_SetPreference(TRUE)
this.inv_preference.of_SetToolBars(TRUE)
this.inv_preference.of_SetWindow(TRUE)
this.of_SetResize(TRUE) 

//this.inv_resize.of_SetOrigSize(this.workspacewidth(),this.workspaceheight())
//inv_resize.of_Register(dw_rpt_fram_prod_qc_tbl_and_bar, "Scale")
inv_resize.of_Register(dw_rpt_fram_prod_qc_tbl_and_barhave, "Scale")
//inv_resize.of_Register(dw_rpt_sum_prod_plan2, "Scale")

inv_resize.of_Register(cb_exit, "Scale")
inv_resize.of_Register(cb_print, "Scale")
inv_resize.of_Register(cb_option, "Scale")
inv_resize.of_Register(cb_next, "Scale")
inv_resize.of_Register(cb_prev, "Scale")
inv_resize.of_Register(cb_printall, "Scale")




end event

event pfc_postopen;date ld_start_date, ld_end_date, &
		ld_fy_start_date, ld_fy_end_date,ld_before_start30,ld_orig
long li_re,li_fy,li_count, i, li_code
			
string  ls_med, ls_prdr, ls_cntrlc,ls_print,ls_cntrmed,&
		ls_start_date, ls_end_date, ls_fy_start_date, ls_fy_end_date, &
		 ls_yr, ls_fy, ls_month, ls_regen,&
		ls_pre_fy, ls_start_date_left,&
		ls_oneprdr,ls_max,ls_min, ls_lang, ls_back
datetime ldt_start_date
str_voucher_report lstr


//Open(w_response_monthly_reports)  no need open response window, that already open
// this parts is borrowed from w_month_report, some parts never happen to save time I do not delete them as it is.
SetPointer( Hourglass!)
this.setRedraw( false)
if IsNull(message.PowerObjectParm) then
	return
end if
istr =message.PowerObjectParm
ls_print='N'
istr.array[5]=ls_print
lstr =istr
ls_month =lstr.array[1]
ls_yr =lstr.array[2]
ls_prdr = lstr.array[3]
ls_regen = lstr.array[4]
ls_start_date = lstr.array[8]
ls_end_date = (lstr.array[9])
ls_fy_start_date = lstr.array[10]
ls_fy_end_date = (lstr.array[11])
ls_cntrmed = lstr.array[13]
ls_oneprdr = (lstr.array[14])
//ls_incldavg = lstr.array[15]
ls_back = lstr.array[17]
dw_rpt_fram_prod_qc_tbl_and_barhave.visible=true
dw_rpt_fram_prod_qc_tbl_and_bar.visible=false
ld_start_date= date( ls_start_date)
ldt_start_date=datetime(ld_start_date,time('00:00:00'))
ld_end_date =date( ls_end_date )
ld_before_start30 = RelativeDate( ld_start_date, - 30 )
ld_fy_start_date =date( ls_fy_start_date )
ld_fy_end_date =date( ls_fy_end_date )
ls_pre_fy= right(ls_fy_start_date, 4)
ls_fy= right(ls_fy_end_date, 4)
li_fy =integer( ls_fy )
if lstr.array[7] ='cancel' then
	this.setRedraw( true)
	Close(w_month_report )
	m_pics_main.m_menu.PopMenu(300, 0)
	return 
end if //if ok
ls_regen = lstr.array[4]

if ls_oneprdr='Y' then // one producer only handle create a report for one pecific producer and suppose that the data
// is already in the database, there is no need to recreated
	cb_print.enabled=true
	cb_printall.enabled=false
	cb_next.enabled=false
	cb_prev.enabled=false
	
	openwithparm(w_pics_rpt_msg_box,'Retrieving data...')
	li_re=wf_rpt_text_obj_total_pct_1_prdr()// only this statement need work all other can delete
	if li_re= -1 then
		return
	end if

elseif ls_oneprdr='N' then
	cb_print.enabled=true
	cb_printall.enabled=true
	cb_next.enabled=true
	cb_prev.enabled=true
	cb_prev.enabled=false
	openwithparm(w_pics_rpt_msg_box, 'Checking existing data. Please wait....')	
	i_prdr_med=1
	//there is no need checking data, since response window already checked.
	//for all producer only handle in the button cb_printall and cb_next, cb_privious
	//for all producer in response window aleady set first producer in dddw list producer as defaut producer
	li_re=wf_rpt_text_obj_total_pct_1_prdr()
	if li_re= -1 then
		return
	end if
end if// if ls_oneprdr='Y'
close( w_pics_rpt_msg_box)
this.setredraw( true)
return

end event

type dw_rpt_all_colhave from u_pics_dw within w_month_reporthave
boolean visible = false
integer x = 1673
integer y = 1580
integer width = 50
integer height = 36
integer taborder = 90
boolean bringtotop = true
string dataobject = "d_rpt_all_colhave"
end type

type dw_rpt_qc_apr_rej_showhave from u_pics_dw within w_month_reporthave
boolean visible = false
integer x = 1577
integer y = 1584
integer width = 50
integer height = 36
integer taborder = 90
boolean bringtotop = true
string dataobject = "d_rpt_qc_apr_rej_showhave"
end type

type dw_rpt_detl_total_pcthave from u_pics_dw within w_month_reporthave
boolean visible = false
integer x = 1650
integer y = 1524
integer width = 55
integer height = 40
integer taborder = 70
boolean bringtotop = true
string dataobject = "d_rpt_detl_total_pcthave"
end type

type dw_rpt_sum_cmpt_pcthave from u_pics_dw within w_month_reporthave
boolean visible = false
integer x = 1559
integer y = 1524
integer width = 50
integer height = 36
integer taborder = 60
boolean bringtotop = true
string dataobject = "d_rpt_sum_cmpt_pcthave"
end type

type cb_prev from commandbutton within w_month_reporthave
event mousemove pbm_mousemove
string tag = "Print the record"
integer x = 1742
integer y = 1544
integer width = 498
integer height = 108
integer taborder = 80
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "Pre&vious Producer"
boolean default = true
end type

event mousemove;SetMicroHelp(w_pics_main,this.tag)
end event

event clicked;

string ls_prdr, ls_med,ls_month, ls_start_date, ls_end_date,ls_fy_start_date,&
			ls_fy_end_date, ls_oneprdr,ls_fy,ls_pre_fy, ls_start_date_left,&
			ls_year,ls_yr, ls_print,ls_next_or_prev
int li_re	

str_voucher_report lstr


ls_print='N'
istr.array[5]=ls_print
//istr =message.PowerObjectParm
lstr =istr
ls_month =lstr.array[1]
ls_prdr =lstr.array[3]
ls_print = lstr.array[5]
ls_start_date = lstr.array[8]
ls_end_date = (lstr.array[9])
ls_fy_start_date = lstr.array[10]
ls_fy_end_date = (lstr.array[11])
ls_med = lstr.array[13]
ls_oneprdr = (lstr.array[14])

ls_pre_fy= right(ls_fy_start_date, 4)
ls_fy= right(ls_fy_end_date, 4)
//li_fy =integer( ls_fy )
if ls_start_date_left >='10' then
	ls_year= ls_pre_fy
else
	ls_year= ls_fy
end if
open(w_pics_rpt_msg_box)
ls_next_or_prev='prev'
istr.array[16]=ls_next_or_prev
if ls_oneprdr='N' then

	w_pics_rpt_msg_box.sle_retrieve.text=&
		'Retrieving data, Please wait...'
	li_re=wf_rpt_text_obj_total_pct_find_1_prdr()
	if li_re= - 1 then
		ls_prdr=istr.array[3]
		ls_med= istr.array[13]
		messagebox('','Error in create text object items for previous prdr/med'+' '+&
		ls_prdr+'/'+ls_med +' upto '+ls_month+',' +ls_year)
						
		return
	end if
end if
close(w_pics_rpt_msg_box)
end event

type cb_next from commandbutton within w_month_reporthave
event mousemove pbm_mousemove
string tag = "Print the record"
integer x = 1138
integer y = 1544
integer width = 384
integer height = 108
integer taborder = 70
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "&Next Producer"
boolean default = true
end type

event mousemove;SetMicroHelp(w_pics_main,this.tag)
end event

event clicked;string ls_prdr, ls_med,ls_month, ls_start_date, ls_end_date,ls_fy_start_date,&
			ls_fy_end_date, ls_oneprdr,ls_fy,ls_pre_fy, ls_start_date_left,&
			ls_year,ls_yr, ls_print,ls_next_or_prev
int li_re	

str_voucher_report lstr


ls_print='N'
istr.array[5]=ls_print
//istr =message.PowerObjectParm
lstr =istr
ls_month =lstr.array[1]
ls_prdr =lstr.array[3]
ls_print = lstr.array[5]
ls_start_date = lstr.array[8]
ls_end_date = (lstr.array[9])
ls_fy_start_date = lstr.array[10]
ls_fy_end_date = (lstr.array[11])
ls_med = lstr.array[13]
ls_oneprdr = (lstr.array[14])

ls_pre_fy= right(ls_fy_start_date, 4)
ls_fy= right(ls_fy_end_date, 4)
//li_fy =integer( ls_fy )
if ls_start_date_left >='10' then
	ls_year= ls_pre_fy
else
	ls_year= ls_fy
end if
open(w_pics_rpt_msg_box)
ls_next_or_prev='next'
istr.array[16]=ls_next_or_prev
if ls_oneprdr='N' then

	w_pics_rpt_msg_box.sle_retrieve.text=&
		'Retieving data, Please wait...'	
	li_re=wf_rpt_text_obj_total_pct_find_1_prdr()
	if li_re= - 1 then
		ls_prdr=istr.array[3]
		ls_med= istr.array[13]
		messagebox('','Error in create text object items for next prdr/med'+' '+ls_prdr+'/'+ls_med +&
						' upto '+ls_month+',' +ls_year)
		return
	end if
end if
close(w_pics_rpt_msg_box)
end event

type cb_printall from commandbutton within w_month_reporthave
event mousemove pbm_mousemove
string tag = "Print the record"
integer x = 617
integer y = 1544
integer width = 325
integer height = 108
integer taborder = 60
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "P&rint All"
boolean default = true
end type

event mousemove;SetMicroHelp(w_pics_main,this.tag)
end event

event clicked;string ls_prdr, ls_med,ls_month, ls_start_date, ls_end_date,ls_fy_start_date,&
			ls_fy_end_date, ls_oneprdr,ls_fy,ls_pre_fy, ls_start_date_left,&
			ls_year,ls_yr, ls_print
int li_re	

str_voucher_report lstr


ls_print='Y'
istr.array[5]=ls_print
//istr =message.PowerObjectParm
lstr =istr
ls_month =lstr.array[1]
ls_prdr =lstr.array[3]
ls_print = lstr.array[5]
ls_start_date = lstr.array[8]
ls_end_date = (lstr.array[9])
//ls_fy_start_date = lstr.array[10]
//ls_fy_end_date = (lstr.array[11])
ls_med = lstr.array[13]
ls_oneprdr = (lstr.array[14])

//ls_pre_fy= right(ls_fy_start_date, 4)
//ls_fy= right(ls_fy_end_date, 4)
////li_fy =integer( ls_fy )
//if ls_start_date_left >='10' then
//	ls_year= ls_pre_fy
//else
//	ls_year= ls_fy
//end if
open(w_pics_rpt_msg_box)
if ls_oneprdr='Y' then
else
	li_re=wf_rpt_text_obj_total_pct_all_prdr()
	if li_re= - 1 then
		messagebox('','Error in create text object items for all prdr')
		return
	end if
end if
close(w_pics_rpt_msg_box)
end event

type cb_option from commandbutton within w_month_reporthave
event mousemove pbm_mousemove
string tag = "Print the record"
integer x = 2459
integer y = 1544
integer width = 430
integer height = 108
integer taborder = 60
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "Previous &Screen"
boolean default = true
end type

event mousemove;SetMicroHelp(w_pics_main,this.tag)
end event

event clicked;i_prdr_med=1
//parent.triggerevent('pfc_postopen')
close(parent)
OpenSheetWithParm(w_month_report," ",w_pics_main, 0, Original!)

end event

type dw_deviate_sum_sp_no_detail from u_pics_dw within w_month_reporthave
boolean visible = false
integer x = 2066
integer y = 1520
integer width = 50
integer height = 36
integer taborder = 60
boolean bringtotop = true
string dataobject = "d_deviate_sum_sp_no_detail"
end type

type dw_rpt_cntr_cntrlc from u_pics_dw within w_month_reporthave
boolean visible = false
integer x = 2144
integer y = 1524
integer width = 50
integer height = 36
integer taborder = 50
boolean bringtotop = true
string dataobject = "d_rpt_cntr_cntrlc"
end type

type dw_rpt_commts_in_amonth from u_pics_dw within w_month_reporthave
boolean visible = false
integer x = 2231
integer y = 1516
integer width = 50
integer height = 36
integer taborder = 30
boolean bringtotop = true
string dataobject = "d_rpt_commts_in_amonth"
end type

type dw_rpt_qc_apr_rej_show_total from u_pics_dw within w_month_reporthave
boolean visible = false
integer x = 2341
integer y = 1516
integer width = 50
integer height = 36
integer taborder = 30
boolean bringtotop = true
string dataobject = "d_rpt_qc_apr_rej_show"
end type

type dw_rpt_detl_total_pct from u_pics_dw within w_month_reporthave
boolean visible = false
integer x = 2432
integer y = 1516
integer width = 50
integer height = 36
integer taborder = 60
boolean bringtotop = true
string dataobject = "d_rpt_detl_total_pct"
end type

type dw_rpt_sum_cmpt_pct from u_pics_dw within w_month_reporthave
boolean visible = false
integer x = 2537
integer y = 1520
integer width = 46
integer height = 40
integer taborder = 30
boolean bringtotop = true
string dataobject = "d_rpt_sum_cmpt_pct"
end type

type cb_print from commandbutton within w_month_reporthave
event clicked pbm_bnclicked
event mousemove pbm_mousemove
string tag = "Print the record"
integer x = 55
integer y = 1544
integer width = 325
integer height = 108
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "&Print"
boolean default = true
end type

event clicked;string ls_prdr, ls_med,ls_month, ls_start_date, ls_end_date,ls_fy_start_date,&
			ls_fy_end_date, ls_oneprdr,ls_fy,ls_pre_fy, ls_start_date_left,&
			ls_year,ls_yr, ls_print, ls_next_prev, ls_back
int li_re	

str_voucher_report lstr


ls_print='Y'
istr.array[5]=ls_print
//istr =message.PowerObjectParm
lstr =istr
ls_month =lstr.array[1]
ls_prdr =lstr.array[3]
ls_print = lstr.array[5]
ls_start_date = lstr.array[8]
ls_end_date = (lstr.array[9])
ls_fy_start_date = lstr.array[10]
ls_fy_end_date = (lstr.array[11])
ls_med = lstr.array[13]
ls_oneprdr = (lstr.array[14])
ls_back = lstr.array[17]
ls_pre_fy= right(ls_fy_start_date, 4)
ls_fy= right(ls_fy_end_date, 4)
//li_fy =integer( ls_fy )
if ls_start_date_left >='10' then
	ls_year= ls_pre_fy
else
	ls_year= ls_fy
end if

open(w_pics_rpt_msg_box)
if ls_oneprdr='Y' then
//	w_pics_rpt_msg_box.sle_retrieve.text=&
//		'Create Total Text Object for one prdr,med '+ls_prdr+','+ls_med+' upto '+ls_month+','+ls_year
	li_re=wf_rpt_text_obj_total_pct_1_prdr()
	if li_re= - 1 then
		messagebox('','Error in create text object items for producer/media '+ls_prdr+'/'+ls_med)
		return -1
	end if
else
	w_pics_rpt_msg_box.sle_retrieve.text=&
		'Create Total Text Object for current one prdr,med,cntrlc upto the '+ls_month+','+ls_year
	ls_next_prev='current'
	istr.array[16]= ls_next_prev
	li_re=wf_rpt_text_obj_total_pct_find_1_prdr()
	if li_re= - 1 then
		messagebox('','Error in create text object items for all prdr')
		return
	end if
end if
close(w_pics_rpt_msg_box)
end event

event mousemove;SetMicroHelp(w_pics_main,this.tag)
end event

type cb_exit from commandbutton within w_month_reporthave
event clicked pbm_bnclicked
event mousemove pbm_mousemove
string tag = "Exit the screen"
integer x = 3109
integer y = 1544
integer width = 325
integer height = 108
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "E&xit"
end type

event clicked;

dw_deviate_sum_sp_no_detail.ResetUpdate()
dw_pcdeviaten_ace_report.ResetUpdate()


dw_rpt_cntr_cntrlc.ResetUpdate()
dw_rpt_commts_in_amonth.ResetUpdate()


dw_rpt_detl_total_pct.ResetUpdate()
dw_rpt_fram_prod_qc_tbl_and_bar.ResetUpdate()

dw_rpt_qc_apr_rej_show_total.ResetUpdate()


dw_rpt_sum_cmpt_pct.ResetUpdate()

dw_rpt_sum_prod_plan2.ResetUpdate()

dw_storeproc.ResetUpdate()
dw_rpt_all_colhave.ResetUpdate()
dw_rpt_detl_total_pcthave.ResetUpdate()
dw_rpt_fram_prod_qc_tbl_and_barhave.ResetUpdate()
dw_rpt_qc_apr_rej_showhave.ResetUpdate()
dw_rpt_sum_cmpt_pcthave.ResetUpdate()

parent.Event pfc_close()






end event

event mousemove;SetMicroHelp(w_pics_main,this.tag)
end event

type dw_storeproc from u_pics_dw within w_month_reporthave
boolean visible = false
integer x = 2715
integer y = 1520
integer width = 46
integer height = 40
integer taborder = 50
boolean bringtotop = true
string dataobject = "d_deviate_sum_storeproc"
end type

type dw_rpt_fram_prod_qc_tbl_and_bar from u_pics_dw within w_month_reporthave
integer x = 5
integer width = 3515
integer height = 1484
integer taborder = 10
string dataobject = "d_rpt_fram_prod_qc_tbl_and_bar"
boolean hscrollbar = true
boolean hsplitscroll = true
end type

event clicked;call super::clicked;this.SelectRow(0, false)
this.selectRow(row,true )
end event

event rowfocuschanged;call super::rowfocuschanged;long ll_curr_row


ll_curr_row = This.getrow ()
this.SelectRow(0, false )
this.SelectRow(ll_curr_row , true )
this.ScrollToRow( ll_curr_row )
this.SetRow( ll_curr_row )
this.SetFocus()
end event

type dw_rpt_sum_prod_plan2 from u_pics_dw within w_month_reporthave
boolean visible = false
integer x = 1070
integer y = 1528
integer width = 50
integer height = 36
integer taborder = 10
boolean bringtotop = true
string dataobject = "d_rpt_sum_prod_plan2"
boolean hscrollbar = true
boolean hsplitscroll = true
end type

event clicked;call super::clicked;if Isnull( row ) then
	return
end if
this.selectRow(0, false )
this.selectRow(row, true )
end event

event rowfocuschanged;call super::rowfocuschanged;long ll_curr_row


ll_curr_row = This.getrow ()
this.SelectRow(0, false )
this.SelectRow(ll_curr_row , true )
this.ScrollToRow( ll_curr_row )
this.SetRow( ll_curr_row )
this.SetFocus()
end event

event constructor;call super::constructor;this.of_Setfind(TRUE)
This.of_Setfilter(TRUE)
This.of_Setsort(TRUE)
end event

type dw_rpt_fram_prod_qc_tbl_and_barhave from u_pics_dw within w_month_reporthave
integer x = 5
integer width = 3515
integer height = 1484
integer taborder = 30
boolean bringtotop = true
string dataobject = "d_rpt_fram_prod_qc_tbl_and_barhave"
boolean hscrollbar = true
boolean hsplitscroll = true
end type

event clicked;call super::clicked;this.SelectRow(0, false)
this.selectRow(row,true )
end event

event rowfocuschanged;call super::rowfocuschanged;long ll_curr_row


ll_curr_row = This.getrow ()
this.SelectRow(0, false )
this.SelectRow(ll_curr_row , true )
this.ScrollToRow( ll_curr_row )
this.SetRow( ll_curr_row )
this.SetFocus()
end event

type dw_pcdeviaten_ace_report from u_pics_dw within w_month_reporthave
boolean visible = false
integer x = 1047
integer y = 1576
integer width = 55
integer height = 48
integer taborder = 10
boolean bringtotop = true
string dataobject = "d_pcdeviaten_ace_report"
boolean hscrollbar = true
boolean hsplitscroll = true
end type

event clicked;call super::clicked;this.SelectRow(0, false)
this.selectRow(row,true )
end event

event rowfocuschanged;call super::rowfocuschanged;long ll_curr_row


ll_curr_row = This.getrow ()
this.SelectRow(0, false )
this.SelectRow(ll_curr_row , true )
this.ScrollToRow( ll_curr_row )
this.SetRow( ll_curr_row )
this.SetFocus()
end event


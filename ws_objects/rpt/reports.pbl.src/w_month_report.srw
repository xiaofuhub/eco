$PBExportHeader$w_month_report.srw
forward
global type w_month_report from w_sheet
end type
type cb_printall from commandbutton within w_month_report
end type
type cb_prev from commandbutton within w_month_report
end type
type cb_next from commandbutton within w_month_report
end type
type dw_deviate_sum_sp_no_detail from u_pics_dw within w_month_report
end type
type dw_rpt_cntr_cntrlc from u_pics_dw within w_month_report
end type
type dw_rpt_qc_apr_rej_show_total from u_pics_dw within w_month_report
end type
type dw_rpt_detl_total_pct from u_pics_dw within w_month_report
end type
type dw_rpt_sum_cmpt_pct from u_pics_dw within w_month_report
end type
type cb_print from commandbutton within w_month_report
end type
type cb_exit from commandbutton within w_month_report
end type
type dw_rpt_fram_prod_qc_tbl_and_bar from u_pics_dw within w_month_report
end type
type dw_rpt_all_col from u_pics_dw within w_month_report
end type
type dw_rpt_qc_apr_rej_get from u_pics_dw within w_month_report
end type
type dw_rpt_sum_prod_plan2 from u_pics_dw within w_month_report
end type
type dw_comp_init_3_columns from u_pics_dw within w_month_report
end type
type dw_pcdeviaten_ace_report from u_pics_dw within w_month_report
end type
type dw_rpt_open_cmp_tottitles from u_pics_dw within w_month_report
end type
type dw_rpt_open_cmp_shiptitles from u_pics_dw within w_month_report
end type
type dw_rpt_open_cmp_latetitles from u_pics_dw within w_month_report
end type
type dw_rpt_open_cmp_asigntitles from u_pics_dw within w_month_report
end type
type dw_rpt_sum_prod_act_ship from u_pics_dw within w_month_report
end type
type dw_rpt_sum_prod_reg_ship from u_pics_dw within w_month_report
end type
type dw_rpt_init_3_columns_open from u_pics_dw within w_month_report
end type
type cb_option from commandbutton within w_month_report
end type
type dw_rpt_sum_prod_late_ship from u_pics_dw within w_month_report
end type
end forward

global type w_month_report from w_sheet
integer x = 5
integer y = 4
integer width = 3584
integer height = 1832
string title = "Contract Annual Initialization"
cb_printall cb_printall
cb_prev cb_prev
cb_next cb_next
dw_deviate_sum_sp_no_detail dw_deviate_sum_sp_no_detail
dw_rpt_cntr_cntrlc dw_rpt_cntr_cntrlc
dw_rpt_qc_apr_rej_show_total dw_rpt_qc_apr_rej_show_total
dw_rpt_detl_total_pct dw_rpt_detl_total_pct
dw_rpt_sum_cmpt_pct dw_rpt_sum_cmpt_pct
cb_print cb_print
cb_exit cb_exit
dw_rpt_fram_prod_qc_tbl_and_bar dw_rpt_fram_prod_qc_tbl_and_bar
dw_rpt_all_col dw_rpt_all_col
dw_rpt_qc_apr_rej_get dw_rpt_qc_apr_rej_get
dw_rpt_sum_prod_plan2 dw_rpt_sum_prod_plan2
dw_comp_init_3_columns dw_comp_init_3_columns
dw_pcdeviaten_ace_report dw_pcdeviaten_ace_report
dw_rpt_open_cmp_tottitles dw_rpt_open_cmp_tottitles
dw_rpt_open_cmp_shiptitles dw_rpt_open_cmp_shiptitles
dw_rpt_open_cmp_latetitles dw_rpt_open_cmp_latetitles
dw_rpt_open_cmp_asigntitles dw_rpt_open_cmp_asigntitles
dw_rpt_sum_prod_act_ship dw_rpt_sum_prod_act_ship
dw_rpt_sum_prod_reg_ship dw_rpt_sum_prod_reg_ship
dw_rpt_init_3_columns_open dw_rpt_init_3_columns_open
cb_option cb_option
dw_rpt_sum_prod_late_ship dw_rpt_sum_prod_late_ship
end type
global w_month_report w_month_report

type variables
str_voucher_report istr
int i_count=0, i_prdr_med=1,i_1_prdr=1
dw_rpt_sum_prod_plan2 idw
end variables

forward prototypes
public function integer wf_rpt_create_plan_act_reg_late_ship_1_prdr_1_mon (datetime cur_start_date, datetime cur_end_date)
public function integer wf_rpt_create_plan_act_reg_late_ship_all_prdr_1_mon (datetime cur_start_date, datetime cur_end_date)
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
public function integer wf_rpt_ini_all_col_all_prdr_1_year0 ()
public function integer wf_rpt_ini_all_col_1_prdr_1_year0 ()
end prototypes

public function integer wf_rpt_create_plan_act_reg_late_ship_1_prdr_1_mon (datetime cur_start_date, datetime cur_end_date);// save the change must commit all at one time
//this function is for createing planship, actship, regular ship and lateship for one specific producer and month
//temporary not commit jet

date ld_start_date, ld_end_date, ld_before_start30	 
long li_row_count, i, li_len,li_re ,li_month,j, li_count, li_cur, li_plan, li_row
string  ls_med, ls_prdr, ls_cntr,ls_filter,ls_cur_start_date,ls_cur_end_date, ls_cntr_old, ls_lang
		
str_voucher_report lstr
datastore lds


lds =create datastore
lstr =istr
ls_prdr =lstr.array[3]
ls_med = lstr.array[13]

//Don't need to retrieve again for dw_rpt_all_col . The data
//aready come out for one year and filter data for this month left in primary buffer waiting
//for update
ld_before_start30= RelativeDate(date(cur_start_date), - 30)
ls_cur_start_date=string(date(cur_start_date))
ls_cur_end_date= string(date(cur_end_date))
if ls_prdr='PTB' AND ls_med='FL' THEN
	ls_med='RC'
	ls_lang='Y'
elseif ls_prdr='PTB' and (ls_med='RC' OR ls_med='RTB') THEN
	ls_lang='N'
end if
if ls_prdr<>'PTB' THEN
	ls_filter="prdr = '"+ls_prdr+"'"+" and cntrmed= '"+ls_med+"'"+" and "
	ls_filter=ls_filter +"date(start_date) >=date('"+ls_cur_start_date+"')"+&
												" and date(end_date)<= date('"+ls_cur_end_date+"')"
elseif ls_prdr='PTB' THEN
	ls_filter="prdr = '"+ls_prdr+"'"+" and cntrmed= '"+ls_med+"'"+" and "
	ls_filter=ls_filter +"date(start_date) >=date('"+ls_cur_start_date+"')"+&
		" and date(end_date)<= date('"+ls_cur_end_date+"')"+" AND foreign_lang='"+ls_lang+"'"
END IF
dw_rpt_all_col.SetFilter(ls_filter)
dw_rpt_all_col.Filter()
dw_rpt_all_col.Sort()
li_row_count =dw_rpt_all_col.RowCount()

dw_rpt_sum_prod_act_ship.SetTransObject( SqlServerTrans )
dw_rpt_sum_prod_reg_ship.SetTransObject( SqlServerTrans )
dw_rpt_sum_prod_late_ship.SetTransObject( SqlServerTrans )

dw_rpt_sum_prod_act_ship.Retrieve(ls_prdr, cur_start_date, cur_end_date )
dw_rpt_sum_prod_reg_ship.Retrieve(ls_prdr, cur_start_date, cur_end_date )
dw_rpt_sum_prod_late_ship.Retrieve(ls_prdr, cur_start_date, cur_end_date )


lds.dataobject='d_rpt_sum_prod_plan2'
lds.SetTransObject( SqlServerTrans )
li_count=lds.Retrieve(ls_prdr)


li_count=lds.RowCount(  )
//this is for computing plan ship
for i=1 to li_row_count				

	ls_cntr =trim(dw_rpt_all_col.GetItemString( i,'cntr'))
	if ls_cntr<>ls_cntr_old then
		ls_filter =  "cntrmed= '"+ls_med+"'"+&
							" and cntr= '"+ls_cntr+"'"
		ls_filter=ls_filter +" and origenddt >=date('"+ls_cur_start_date+"')"+&
												" and origenddt<= date('"+ls_cur_end_date+"')"
		//filter means the original schenddt is between ld_start_date and ld_end_date
		//originenddt is date type not datetime type
		lds.SetFilter( ls_filter )
		lds.Filter()
		li_row=lds.RowCount()
		ls_filter =  "cntrmed= '"+ls_med+"'"+&
							" and cntr= '"+ls_cntr+"'"
		dw_rpt_sum_prod_act_ship.SetFilter(ls_filter )
		//this is for act ship
		dw_rpt_sum_prod_act_ship.Filter()
		li_re =dw_rpt_sum_prod_act_ship.RowCount()
		dw_rpt_sum_prod_reg_ship.SetFilter( ls_filter )
		//this is for regular ship
		dw_rpt_sum_prod_reg_ship.Filter()
		li_count =dw_rpt_sum_prod_reg_ship.RowCount()
		dw_rpt_sum_prod_late_ship.SetFilter( ls_filter )
		//this is for late ship
		dw_rpt_sum_prod_late_ship.Filter()
		li_len =dw_rpt_sum_prod_late_ship.RowCount()
		li_plan= li_row
		
		dw_rpt_all_col.SetItem( i,'planship', li_plan)
		dw_rpt_all_col.SetItem( i,'actship', li_re)
		dw_rpt_all_col.SetItem( i,'regship', li_count)
		dw_rpt_all_col.SetItem( i,'lateship', li_len)
		ls_cntr_old=ls_cntr
	end if
next// for i=1 to li_row_count
li_re =dw_rpt_all_col.update()
if li_re =1 then
//	commit using SqlServerTrans;
else
	Rollback using SqlServerTrans;
	return -1
end if
return 1
end function

public function integer wf_rpt_create_plan_act_reg_late_ship_all_prdr_1_mon (datetime cur_start_date, datetime cur_end_date);//this function will create all prdr, med, cntr for on specific month. The cur_start_date is argument will change from time 
//to time. Each month there are different prdr, med, cntr. In each month it create plan, act, regular, late ship for
//all prdr,med, cntr that availabel in that month.

date ld_start_date, ld_end_date, ld_before_start30	 
long li_row_count, i, li_len,li_re ,li_month,j, li_count, li_cur, li_plan, li_row
string  ls_med, ls_prdr, ls_cntr,ls_filter,ls_cur_start_date,ls_cur_end_date, ls_cntr_old, ls_lang, ls_fy_start_date,&
		ls_fy_end_date, ls_pre_fy, ls_fy, ls_curmonyr, ls_prdr_old, ls_med_old, ls_max, ls_min
		
str_voucher_report lstr
datastore lds_ds

lds_ds= create datastore
lds_ds.dataobject='d_rpt_sum_prod_plan2'
lds_ds.SetTransObject( SqlServerTrans )
lstr =istr
ls_fy_start_date = lstr.array[10]
ls_fy_end_date = (lstr.array[11])
ls_pre_fy= right(ls_fy_start_date, 4)
ls_fy= right(ls_fy_end_date, 4)
ld_before_start30= RelativeDate(date(cur_start_date), - 30)
//Don't need to retrieve again for dw_rpt_all_col. The data
//aready come out for one year and filter data for this month left in primary buffer waiting
//for update
ls_cur_start_date=string(cur_start_date,'mm/dd/yyyy')
ls_cur_end_date= string(cur_end_date,'mm/dd/yyyy')
//ls_filter="prdr = '"+ls_prdr+"'"+" and cntrmed= '"+ls_med+"'"+" and "
ls_filter=ls_filter +"date(start_date) >=date('"+ls_cur_start_date+"')"+&
											" and date(end_date)<= date('"+ls_cur_end_date+"')"
dw_rpt_all_col.SetFilter(ls_filter)
dw_rpt_all_col.Filter()
dw_rpt_all_col.Sort()
li_row_count =dw_rpt_all_col.RowCount()

//lds_ds.SetTransObject( SqlServerTrans )
dw_rpt_sum_prod_act_ship.SetTransObject( SqlServerTrans )
dw_rpt_sum_prod_reg_ship.SetTransObject( SqlServerTrans )
dw_rpt_sum_prod_late_ship.SetTransObject( SqlServerTrans )
choose case ls_cur_start_date
	case '10/01/'+ls_pre_fy
		ls_curmonyr='October,'+ls_pre_fy
	case '11/01/'+ls_pre_fy
		ls_curmonyr='November,'+ls_pre_fy
	case '12/01/'+ls_pre_fy
		ls_curmonyr='December,'+ls_pre_fy
	case '01/01/'+ls_fy
		ls_curmonyr='January,'+ls_fy
	case '02/01/'+ls_fy
		ls_curmonyr='February,'+ls_fy
	case '03/01/'+ls_fy
		ls_curmonyr='March,'+ls_fy
	case '04/01/'+ls_fy
		ls_curmonyr='April,'+ls_fy
	case '05/01/'+ls_fy
		ls_curmonyr='May,'+ls_fy
	case '06/01/'+ls_fy
		ls_curmonyr='June,'+ls_fy
	case '07/01/'+ls_fy
		ls_curmonyr='July,'+ls_fy
	case '08/01/'+ls_fy
		ls_curmonyr='August,'+ls_fy
	case '09/01/'+ls_fy
		ls_curmonyr='September,'+ls_fy
end choose
for i=1 to li_row_count
	ls_prdr=trim(dw_rpt_all_col.GetItemString(i,'prdr'))
	ls_med =trim(dw_rpt_all_col.GetItemString( i,'cntrmed'))
	ls_cntr =trim(dw_rpt_all_col.GetItemString( i,'cntr'))
	
	if ls_prdr<>ls_prdr_old or ls_med<>ls_med_old then
//		w_pics_rpt_msg_box.sle_retrieve.text=&
//		'Dulicate check of Plan,Act,Late Ship data for prdr,med '+ls_prdr+','+ls_med+&
//		' in '+ls_curmonyr
		select max(done),min(done) into :ls_max,:ls_min
		from monrpt
		where start_date=:cur_start_date and end_date=:cur_end_date and
				prdr=:ls_prdr and cntrmed=:ls_med 
		using SqlServerTrans;
		if not f_check_dberror(SqlServerTrans,'select from monrpt to find done column') then
			return -1
		end if
		if ls_max='Y' and ls_min='Y' then continue
		// if data already over there then jump to next combination of ls_prdr, ls_med
		if ls_prdr<> ls_prdr_old then
			if ls_max<>'Y' or ls_min<>'Y' then
				//if data need be generated for this ls_prdr, ls_med then create planship, actship, lateship, regship
				destroy lds_ds
				lds_ds =create datastore
				lds_ds.dataobject='d_rpt_sum_prod_plan2'
				lds_ds.SetTransObject( SqlServerTrans )		
				lds_ds.Retrieve(ls_prdr )
				li_count=lds_ds.RowCount( )

//				for j=1 to li_count
//					li_ext=lds_ds.GetItemNumber(j,'sum_ext')
//					if IsNull( li_ext)  then li_ext =0 //get totatl extended days
//					ld_schenddt= lds_ds.GetItemDate(j,'schenddt')
//					if IsNull(ld_schenddt) then continue
//					if li_ext<> 0 then
//						ld_originschenddt= RelativeDate( ld_schenddt, - li_ext )
//						// this is original schedue end date
//					else	// here only borrow the column schstdt column to save original schenddt
//						ld_originschenddt = ld_schenddt
//					end if
//					lds_ds.SetItem( j,'schstdt', ld_originschenddt)
//				next

				dw_rpt_sum_prod_act_ship.Retrieve(ls_prdr, cur_start_date, cur_end_date )
				dw_rpt_sum_prod_reg_ship.Retrieve(ls_prdr, cur_start_date, cur_end_date )
				dw_rpt_sum_prod_late_ship.Retrieve(ls_prdr, cur_start_date, cur_end_date )
				
										
			end if// if ls_max<>'Y' or ls_min
			
		end if//if ls_prdr<>ls_prdr_old
		
	end if// if ls_prdr<> ls_prdr_old and ls_med<>ls_med_old
	if ls_max='Y' and ls_min='Y' then continue

	if ls_prdr<>ls_prdr_old or ls_med<>ls_med_old or ls_cntr<>ls_cntr_old then
		ls_filter =  "cntrmed= '"+ls_med+"'"+&
							" and cntr= '"+ls_cntr+"'"
		// origenddt is date type not datetime type
		ls_filter=ls_filter +"and origenddt >=date('"+ls_cur_start_date+"')"+&
												" and origenddt<= date('"+ls_cur_end_date+"')"
		//filter means the original schenddt is between ld_start_date and ld_end_date
		lds_ds.SetFilter( ls_filter )
		lds_ds.Filter()
		li_row=lds_ds.RowCount()
		//this is plan ship number
		ls_filter =  "cntrmed= '"+ls_med+"'"+&
							" and cntr= '"+ls_cntr+"'"
		//filter means the original schenddt is between ld_start_date and ld_end_date
		dw_rpt_sum_prod_act_ship.SetFilter(ls_filter )
		dw_rpt_sum_prod_act_ship.Filter()
		//this is act ship number
		li_re =dw_rpt_sum_prod_act_ship.RowCount()
		dw_rpt_sum_prod_reg_ship.SetFilter( ls_filter )
		dw_rpt_sum_prod_reg_ship.Filter()
		li_count =dw_rpt_sum_prod_reg_ship.RowCount()
		//this is regular ship number
		dw_rpt_sum_prod_late_ship.SetFilter( ls_filter )
		dw_rpt_sum_prod_late_ship.Filter()
		li_len =dw_rpt_sum_prod_late_ship.RowCount()
		//this is late ship number
		li_plan= li_row
		
		dw_rpt_all_col.SetItem( i,'planship', li_plan)
		dw_rpt_all_col.SetItem( i,'actship', li_re)
		dw_rpt_all_col.SetItem( i,'regship', li_count)
		dw_rpt_all_col.SetItem( i,'lateship', li_len)
		if ls_prdr<>ls_prdr_old then
			ls_prdr_old=ls_prdr
		end if
		if ls_med<>ls_med_old then
			ls_med_old= ls_med
		end if
		if ls_cntr<>ls_cntr_old then
			ls_cntr_old=ls_cntr
		end if
	end if
next// for i=1 to li_row_count
li_re =dw_rpt_all_col.update()
if li_re =1 then
//	commit using SqlServerTrans;
else
	Rollback using SqlServerTrans;
	return -1
end if
return 1
end function

public function integer wf_rpt_text_obj_total_pct_1_prdr ();//this function is stright just for prdr='PTB' find where is foreign language or not, then call
//wf_rpt_text_obj_total_pct_1_prdr_sub(ls_lang)	

string  ls_med, ls_prdr, ls_lang	
str_voucher_report lstr

lstr =istr
ls_prdr =lstr.array[3]
ls_med =lstr.array[13]
ls_prdr =trim( ls_prdr )
ls_med =trim( ls_med )
//this for determining there is foreign language or not
if ls_prdr='PTB' then
	if ls_med='FL' THEN
		ls_med='RC'
		ls_lang='Y'
	elseif (ls_med='RC' OR ls_med='RTB') THEN
		ls_lang='N' 
	end if
elseif ls_prdr<>'PTB' THEN
	ls_lang='N'
end if
wf_rpt_text_obj_total_pct_1_prdr_sub(ls_lang)	
return 1

end function

public function integer wf_rpt_text_obj_total_pct_all_prdr ();// this function will show all producer and median, It user loop to retrieve each prdr and med, then
// to find it has two languages or just one language, then for correspondent language show each prdr, med and
// loop will make it to show all prdr, med list

date ld_start_date, ld_end_date
long li_row_count, i, n

string		ls_start_date, ls_end_date, ls_prdr,ls_med, &
		ls_max,ls_min,ls_lang
		

str_voucher_report lstr
n_ds lds_agm
datetime ld_stdt, ld_enddt

lstr =istr

ls_start_date = lstr.array[8]
ls_end_date = (lstr.array[9])

ld_start_date= date( ls_start_date)
ld_end_date =date( ls_end_date )

lds_agm=create n_ds
//this datastore is for retrieving all producer and median list in alphabetic order of current selected date
lds_agm.dataobject ='d_rpt_retrieve_agt'
lds_agm.SetTransObject( SqlServerTrans )
ld_stdt=datetime(ld_start_date,time('00:00:00'))
ld_enddt=datetime(ld_end_date,time('00:00:00'))

lds_agm.Retrieve(ld_stdt,ld_enddt)

li_row_count=lds_agm.RowCount()

for i=1 to li_row_count // this loop will show all produrcer and median
	//retrieve each prdr and med
	ls_prdr =lds_agm.GetItemString(i,'prdr')
	ls_med =lds_agm.GetItemString(i,'cntrmed')
	ls_prdr =trim( ls_prdr )
	ls_med =trim( ls_med )
	// whether the pair of prdr and med have two languages or just have one
	select max(foreign_lang),min(foreign_lang) into :ls_max, :ls_min
	from monrpt
	where prdr=:ls_prdr and cntrmed=:ls_med and start_date=:ld_stdt and
		end_date=:ld_enddt
	using SqlServerTrans;
	if not f_check_dberror(SqlServerTrans,'select max of foreign_lang from monrpt') then
		return -1
	end if
	if ls_max<> ls_min then
		//if have two languages show each as one report
		for n=1 to 2
			if n=1 then ls_lang=ls_max
			if n=2 then ls_lang=ls_min
			wf_rpt_text_obj_total_pct_all_prdr_sub(ls_prdr,ls_med,ls_lang)		
		next
	elseif ls_max=ls_min then
		// if just one language show the one
		ls_lang=ls_max
		wf_rpt_text_obj_total_pct_all_prdr_sub(ls_prdr,ls_med,ls_lang)
	end if
next
i_count=0
Setpointer( Arrow!)
this.setredraw( true)
return 1

end function

public function integer wf_rpt_text_obj_total_pct_find_1_prdr ();//this function is for find correspondent prdr, med in producer and median list, then find the language property,
//to decide whether this pair of prdr, med have two languages or just one, then call function
// wf_rpt_text_total_pct_all_prdr_sub()


date ld_start_date, ld_end_date
long li_row_count, i, k,i2,n 

string  ls_med, ls_prdr,ls_start_date, ls_end_date,  &
		ls_max,ls_min,ls_lang,ls_next_or_prev
		
str_voucher_report lstr
n_ds lds_agm
datetime ld_stdt, ld_enddt

lstr =istr

ls_start_date = lstr.array[8]
ls_end_date = lstr.array[9]

//
ld_start_date= date( ls_start_date)
ld_end_date =date( ls_end_date )


lds_agm=create n_ds
// this is the data store for showing producer list of curent choosed date
lds_agm.dataobject ='d_rpt_retrieve_agt'
lds_agm.SetTransObject( SqlServerTrans )
ld_stdt=datetime(ld_start_date,time('00:00:00'))
ld_enddt=datetime(ld_end_date,time('00:00:00'))
lds_agm.Retrieve(ld_stdt,ld_enddt)


li_row_count=lds_agm.RowCount()

ls_next_or_prev= istr.array[16] // transfer variable next or previous button be clicked
if ls_next_or_prev ='next' then
	// i_prdr_med is instant variable to count the position of prdr in the producer list
	i_prdr_med +=1
elseif ls_next_or_prev='prev' then
	i_prdr_med -=1
elseif ls_next_or_prev='current' then
end if
if i_prdr_med <=li_row_count and i_prdr_med >= 1 then
	// usering instant i_prdr_med variable to get correspondent producer, median in the producer and median list
	ls_prdr =lds_agm.GetItemString(i_prdr_med,'prdr')
	ls_med =lds_agm.GetItemString(i_prdr_med,'cntrmed')
	ls_prdr =trim( ls_prdr )
	ls_med =trim( ls_med )
	istr.array[3]= ls_prdr
	istr.array[13] =ls_med
	// make judgement whether this producer has two languages or just one
	select max(foreign_lang),min(foreign_lang) into :ls_max, :ls_min
	from monrpt
	where prdr=:ls_prdr and cntrmed=:ls_med and start_date=:ld_stdt and
		end_date=:ld_enddt
	using SqlServerTrans;
	if not f_check_dberror(SqlServerTrans,'select max of foreign_lang from monrpt') then
		return -1
	end if
	if ls_max<> ls_min then 
		// if have two languages show each one of each report
		for n=1 to 2
			if n=1 then ls_lang=ls_max
			if n=2 then ls_lang=ls_min
			wf_rpt_text_obj_total_pct_all_prdr_sub(ls_prdr,ls_med,ls_lang)							
		next
	else// if only one just show one report
		ls_lang=ls_max
		wf_rpt_text_obj_total_pct_all_prdr_sub(ls_prdr,ls_med,ls_lang)
	end if
	// the follwing parts just show some correspondent buttons enabled or disabled
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

public function integer wf_create_all_col_all_prdr_uptonow ();//this function is for computing all data for all prdr, med, cntr from the fiscal year start_date October untill
//the date you choosed in reponse window

date ld_start_date, ld_end_date
long  i, li_re ,li_month, li_fy, j, li_count
string ls_start_date, ls_fy_start_date, ls_fy_end_date, ls_fy, ls_pre_fy, ls_start_date_left, &
		ls_curmon,ls_max,ls_min	 
		
str_voucher_report lstr
datetime ld_stdt, ld_enddt


lstr =istr
ls_start_date = lstr.array[8]
ls_fy_start_date = lstr.array[10]
ls_fy_end_date = (lstr.array[11])

ls_pre_fy= right(ls_fy_start_date, 4)
ls_fy= right(ls_fy_end_date, 4)
li_fy =integer( ls_fy )
ls_start_date_left=left(ls_start_date,2)
choose case ls_start_date_left
//this is control the data will be created from October untill the date you have choosed in response window
//li_month will control the number of months in which data will be created
	case '10'
		li_month=1
	case '11'
		li_month=2
	case '12'
		li_month=3
	case '01'
		li_month=4
	case '02'
		li_month=5
	case '03'
		li_month=6
	case '04'
		li_month=7
	case '05'
		li_month=8
	case '06'
		li_month=9
	case '07'
		li_month=10
	case '08'
		li_month=11
	case '09'
		li_month=12
end choose
for j=1 to li_month
	choose case j
		case 1
			ld_start_date= date('10/01/'+ls_pre_fy)
			ld_end_date= date('10/31/'+ ls_pre_fy )
			ls_curmon='October, '+ls_pre_fy
		case 2
			ld_start_date= date('11/01/'+ls_pre_fy)
			ld_end_date= date('11/30/'+ ls_pre_fy )
			ls_curmon='November, '+ls_pre_fy
		case 3
			ld_start_date= date('12/01/'+ls_pre_fy)
			ld_end_date= date('12/31/'+ ls_pre_fy )
			ls_curmon='December, ' +ls_pre_fy
		case 4
			ld_start_date= date('01/01/'+ls_fy)
			ld_end_date= date('01/31/'+ ls_fy )
			ls_curmon='January, '+ls_fy
		case 5
			ld_start_date= date('02/01/'+ls_fy)
			ls_curmon='February, '+ls_fy
			if mod(li_fy, 4)=0 then
				ld_end_date= date('02/29/'+ ls_fy )
			else
				ld_end_date= date('02/28/'+ ls_fy )
			end if
		case 6
			ld_start_date= date('03/01/'+ls_fy)
			ld_end_date= date('03/31/'+ ls_fy )
			ls_curmon='March, '+ls_fy
		case 7
			ld_start_date= date('04/01/'+ls_fy)
			ld_end_date= date('04/30/'+ ls_fy )
			ls_curmon='April, '+ls_fy
		case 8
			ld_start_date= date('05/01/'+ls_fy)
			ld_end_date= date('05/31/'+ ls_fy )
			ls_curmon='May, '+ls_fy
		case 9
			ld_start_date= date('06/01/'+ls_fy)
			ld_end_date= date('06/30/'+ ls_fy )
			ls_curmon='June, '+ls_fy
		case 10
			ld_start_date= date('07/01/'+ls_fy)
			ld_end_date= date('07/31/'+ ls_fy )
			ls_curmon='July, '+ls_fy
		case 11
			ld_start_date= date('08/01/'+ls_fy)
			ld_end_date= date('08/31/'+ ls_fy )
			ls_curmon='August, '+ls_fy
		case 12
			ld_start_date= date('09/01/'+ls_fy)
			ld_end_date= date('09/30/'+ ls_fy )
			ls_curmon='September, '+ls_fy
	end choose
	openwithparm(w_pics_rpt_msg_box,&
		'Generating data for new Fiscal Year: '+ls_fy)
	ld_stdt=datetime(ld_start_date,time('00:00:00'))
	ld_enddt=datetime(ld_end_date,time('00:00:00'))
	if j=1 then
		li_re=wf_rpt_ini_all_col_all_prdr_1_year0()
	end if
	if li_re= - 1 then

		return -1
	end if

	select max(done),min(done) into :ls_max,:ls_min
	from monrpt
	where (start_date)=:ld_start_date and (end_date)=:ld_end_date 		
	using SqlServerTrans;
	if not f_check_dberror(SqlServerTrans,'select from monrpt to find done column') then
		return -1
	end if
	if ls_max='Y' and ls_min='Y' then continue
	w_pics_rpt_msg_box.sle_retrieve.text=&
	'Generating data for all Producers for '+ls_curmon + '. Please wait...'
	li_re=wf_rpt_create_awad_asgn_late_ship_all_prdr_1_mon(ld_stdt,ld_enddt)
	//since data not created jet, create awad tiltes, assign titles, late titles, ship titles for the current month
	//for all prdr, med, cntr
	if li_re= - 1 then
		messagebox('','Error in generating data. Please contact your system administator.')
		return -1
	end if

	li_re=wf_rpt_create_plan_act_reg_late_ship_all_prdr_1_mon(ld_stdt,ld_enddt)
	//since data not created jet, create plan titles, act titles, regular titles, ship titles for the current month
	//for all prdr, med, cntr
	if li_re= - 1 then
		messagebox('','Error in generating data. Please contact your system administator.')
		return -1
	end if

	li_re=wf_rpt_apr_rej_ytd_laterejpct_all_prdr_1_mon(ld_stdt,ld_enddt)
	//since data not created jet, create approved titles, reject titles, late, reject percent for the current month
	//for all prdr, med, cntr
	if li_re= - 1 then
		messagebox('','Error in generating data. Please contact your system administator.')
		return -1
	end if
next// for j=1 to li_month

close( w_pics_rpt_msg_box)
return 1


end function

public function integer wf_rpt_apr_rej_ytd_laterejpct_1_prdr_1_mon (datetime cur_start_date, datetime cur_end_date);//this function is for coputing ytdapprv, ytdreject, late and reject percentage for specific prdr, med for
//specific month. The argument cur_start_date will change from time to time. This function for fiscal year computing, not
//for last 12 months computing.

date   ld_before_start30

long li_row_count, i,j,  k,li_re, li_ship,li_row,li_count, li_late, &
		li_apr1,li_apr2, li_apr3,li_apr4, li_sum_apr1,li_sum_apr2, li_sum_apr3,li_sum_apr4,&
		li_rej1, li_rej2,li_rej3, li_rej4, li_sum_rej1,li_sum_rej2, li_sum_rej3, li_sum_rej4
		
		
string  ls_med, ls_prdr, ls_cntr, ls_fy,ls_filter, ls_pre_fy,  ls_qastg,&
		 ls_fy_start_date, ls_fy_end_date, ls_qastatcd, ls_qastg_old='',ls_lang,&
		ls_qastatcd_old='',ls_cntr_old,ls_cur_start_date, ls_cur_end_date,ls_max,ls_min,ls_curmonyr
		
real lr_pct
str_voucher_report lstr
datetime  ld_bef_30dt, ld_curstdt, ld_curenddt	



lstr =istr


ls_prdr =lstr.array[3]

ls_med = (lstr.array[13])



//don't need retrieve again for dw_rpt_all_col
//one year data aready retrieved out and filter only this month that left in primary buffer
//waiting to update
ls_cur_start_date=string(date(cur_start_date))
ls_cur_end_date= string(date(cur_end_date))
ld_before_start30 = RelativeDate( date(cur_start_date), - 30 )
//***
if ls_prdr='PTB' AND ls_med='FL' THEN
	ls_med='RC'
	ls_lang='Y'
elseif ls_prdr='PTB' and (ls_med='RC' OR ls_med='RTB') THEN
	ls_lang='N'
end if
if ls_prdr<>'PTB' THEN
	ls_filter="prdr = '"+ls_prdr+"'"+" and cntrmed= '"+ls_med+"'"+" and "
	ls_filter=ls_filter +"date(start_date) >=date('"+ls_cur_start_date+"')"+&
												" and date(end_date)<= date('"+ls_cur_end_date+"')"
elseif ls_prdr='PTB' THEN
	ls_filter="prdr = '"+ls_prdr+"'"+" and cntrmed= '"+ls_med+"'"+" and "
	ls_filter=ls_filter +"date(start_date) >=date('"+ls_cur_start_date+"')"+&
		" and date(end_date)<= date('"+ls_cur_end_date+"')"+" AND foreign_lang='"+ls_lang+"'"
END IF
//***
//ls_filter= "prdr= '"+ls_prdr+"'"+" and cntrmed= '"+ls_med+"'"+" and "
//ls_filter=ls_filter +"date(start_date) =date('"+ls_cur_start_date+"')"+&
//											" and date(end_date)= date('"+ls_cur_end_date+"')"
dw_rpt_all_col.SetFilter(ls_filter)
//get data for specific prdr, med, cur_start_date
dw_rpt_all_col.Filter()
dw_rpt_all_col.Sort()
li_row_count =dw_rpt_all_col.RowCount()
dw_rpt_qc_apr_rej_get.SetTransObject( SqlServerTrans )
ld_curstdt=cur_start_date
ld_curenddt=cur_end_date
ld_bef_30dt=datetime(ld_before_start30,time('00:00:00'))
dw_rpt_qc_apr_rej_get.Retrieve(ls_prdr, ld_curstdt,&
														ld_curenddt, ld_bef_30dt)
for i =1 to li_row_count
	ls_cntr=dw_rpt_all_col.GetItemString( i,'cntr')
	ls_filter= "cntrmed = '"+ls_med+"'"+" and cntr = '"+ls_cntr+"'" 
	dw_rpt_qc_apr_rej_get.SetFilter( ls_filter )
	dw_rpt_qc_apr_rej_get.Filter()
	dw_rpt_qc_apr_rej_get.GroupCalc()
	li_row=dw_rpt_qc_apr_rej_get.RowCount()
	if li_row =0 then continue
	for k=1 to li_row
		//this loop is for computing qc1apprv, qc2apprv, qc3apprv, qc1reject, qc2reject, qc3reject
		ls_qastg=dw_rpt_qc_apr_rej_get.GetItemString(k,'qastg')
		ls_qastatcd =dw_rpt_qc_apr_rej_get.GetItemString(k,'qastatcd')
		if ls_qastg<> ls_qastg_old or ls_qastatcd <>ls_qastatcd_old or &
																	ls_cntr<> ls_cntr_old then	
			li_count=dw_rpt_qc_apr_rej_get.GetItemNumber(k,'counter')
			if IsNull( li_count) then li_count =0
			if ls_qastatcd='A' then
				choose case ls_qastg
						case '1' 
							dw_rpt_all_col.SetItem(i,'qc1apprv', li_count)
						case '2'
							dw_rpt_all_col.SetItem(i,'qc2apprv', li_count)
						case '3'
							dw_rpt_all_col.SetItem(i,'qc3apprv', li_count)
						case '4'
							dw_rpt_all_col.SetItem(i,'qc4apprv', li_count)
					end choose
			elseif ls_qastatcd ='R' then
				choose case ls_qastg
						case '1' 
							dw_rpt_all_col.SetItem(i,'qc1reject', li_count)
						case '2'
							dw_rpt_all_col.SetItem(i,'qc2reject', li_count)
						case '3'
							dw_rpt_all_col.SetItem(i,'qc3reject', li_count)
						case '4'
							dw_rpt_all_col.SetItem(i,'qc4reject', li_count)
				end choose
			end if
			ls_qastg_old= ls_qastg
			ls_qastatcd_old = ls_qastatcd
		end if//end of ls_qastg <> ls_qastg_old....
	next// for k=1 to li_row
	if ls_cntr<>ls_cntr_old then
		ls_cntr_old=ls_cntr
	end if
next//for i=1 to li_row_count

li_re =dw_rpt_all_col.update()
if li_re =1 then
//	commit using SqlServerTrans;
else
	Rollback using SqlServerTrans;
end if
// this is for all year ytd commpute and for fixed producer prdr. Getoff the
//filter, compute the whole year ytd for each month untill to this month. for producer
//PTB MUST CONSIdER foreign language. the ls_lang is set at the start part of this function
if ls_prdr<>'PTB' THEN
	ls_filter= "prdr= '"+ls_prdr+"'"+" and cntrmed= '"+ls_med+"'"
elseif ls_prdr='PTB' then
	ls_filter= "prdr= '"+ls_prdr+"'"+" and cntrmed= '"+ls_med+"'"+&
	   " and foreign_lang='"+ls_lang+"'"
end if
	
dw_rpt_all_col.SetFilter( ls_filter )
dw_rpt_all_col.Filter()
dw_rpt_all_col.Sort()
li_row_count= dw_rpt_all_col.RowCount()

for i=1 to li_row_count
	//this loop is for computiong ctdqc1apprv, ctdqc2apprv, ctdqc3apprv, ctdqc1reject, ctdqc2reject, ctdqc3reject
	ls_cntr =dw_rpt_all_col.GetItemString(i,'cntr')
	if  ls_cntr<> ls_cntr_old  then
		li_sum_apr1=0
		li_sum_apr2=0
		li_sum_apr3=0
		li_sum_apr4=0
		li_sum_rej1=0
		li_sum_rej2=0
		li_sum_rej3=0
		li_sum_rej4=0
		ls_cntr_old = ls_cntr

	end if
	li_apr1= dw_rpt_all_col.GetItemNumber(i,'qc1apprv')
	li_apr2= dw_rpt_all_col.GetItemNumber(i,'qc2apprv')
	li_apr3= dw_rpt_all_col.GetItemNumber(i,'qc3apprv')
	li_apr4= dw_rpt_all_col.GetItemNumber(i,'qc4apprv')
	if isnull(li_apr4) then 
		li_apr4=0
		dw_rpt_all_col.SetItem(i,'qc4apprv', 0)
	end if
	li_rej1=dw_rpt_all_col.GetItemNumber(i,'qc1reject')
	li_rej2=dw_rpt_all_col.GetItemNumber(i,'qc2reject')
	li_rej3=dw_rpt_all_col.GetItemNumber(i,'qc3reject')
	li_rej4=dw_rpt_all_col.GetItemNumber(i,'qc4reject')
	if isnull(li_rej4) then
		li_rej4=0
		dw_rpt_all_col.SetItem(i,'qc4reject', 0)
	end if
	li_sum_apr1 += li_apr1
	li_sum_apr2 += li_apr2
	li_sum_apr3 += li_apr3
	li_sum_apr4 += li_apr4
	li_sum_rej1 += li_rej1
	li_sum_rej2 += li_rej2
	li_sum_rej3 += li_rej3
	li_sum_rej4 += li_rej4
	dw_rpt_all_col.SetItem(i,'ctdqc1apprv' ,li_sum_apr1)
	dw_rpt_all_col.SetItem(i,'ctdqc2apprv' ,li_sum_apr2)
	dw_rpt_all_col.SetItem(i,'ctdqc3apprv' ,li_sum_apr3)
	dw_rpt_all_col.SetItem(i,'ctdqc4apprv' ,li_sum_apr4)
	dw_rpt_all_col.SetItem(i,'ctdqc1reject' ,li_sum_rej1)
	dw_rpt_all_col.SetItem(i,'ctdqc2reject' ,li_sum_rej2)
	dw_rpt_all_col.SetItem(i,'ctdqc3reject' ,li_sum_rej3)
	dw_rpt_all_col.SetItem(i,'ctdqc4reject' ,li_sum_rej4)
next
li_re =dw_rpt_all_col.update()
if li_re =1 then
//	commit using SqlServerTrans;
else
	Rollback using SqlServerTrans;
end if

ls_med = (lstr.array[13])
if ls_prdr='PTB' AND ls_med='FL' THEN
	ls_med='RC'
	ls_lang='Y'
elseif ls_prdr='PTB' and (ls_med='RC' OR ls_med='RTB') THEN
	ls_lang='N'
end if
if ls_prdr<>'PTB' THEN
	ls_filter="prdr = '"+ls_prdr+"'"+" and cntrmed= '"+ls_med+"'"+" and "
	ls_filter=ls_filter +"date(start_date) >=date('"+ls_cur_start_date+"')"+&
												" and date(end_date)<= date('"+ls_cur_end_date+"')"
elseif ls_prdr='PTB' THEN
	ls_filter="prdr = '"+ls_prdr+"'"+" and cntrmed= '"+ls_med+"'"+" and "
	ls_filter=ls_filter +"date(start_date) >=date('"+ls_cur_start_date+"')"+&
		" and date(end_date)<= date('"+ls_cur_end_date+"')"+" AND foreign_lang='"+ls_lang+"'"
END IF

//now use filter only update data for this month. The data is latepct and
//reject percent for each stage QC1,QC2,QC3
dw_rpt_all_col.SetFilter( ls_filter )
dw_rpt_all_col.Filter()
dw_rpt_all_col.Sort()
li_row_count =dw_rpt_all_col.RowCount() 
// for only this month commpute the late percent and the reject percent
			
for i=1 to li_row_count
	li_late= dw_rpt_all_col.GetItemNumber(i,'latetottitles')
	li_ship=dw_rpt_all_col.GetItemNumber(i,'shiptitles')
	if ( li_ship) <>0 then
		lr_pct= li_late/( li_ship )
	else
		lr_pct =0
	end if
	dw_rpt_all_col.SetItem(i,'latepct', lr_pct)
	li_sum_rej1= dw_rpt_all_col.GetItemNumber(i,'ctdqc1reject' )
	li_sum_rej2= dw_rpt_all_col.GetItemNumber(i,'ctdqc2reject' )
	li_sum_rej3= dw_rpt_all_col.GetItemNumber(i,'ctdqc3reject' )
	li_sum_rej4= dw_rpt_all_col.GetItemNumber(i,'ctdqc4reject' )
	li_sum_apr1= dw_rpt_all_col.GetItemNumber(i,'ctdqc1apprv' )
	li_sum_apr2= dw_rpt_all_col.GetItemNumber(i,'ctdqc2apprv' )
	li_sum_apr3= dw_rpt_all_col.GetItemNumber(i,'ctdqc3apprv' )
	li_sum_apr4= dw_rpt_all_col.GetItemNumber(i,'ctdqc4apprv' )
	if (li_sum_rej1 + li_sum_apr1 = 0 ) then
		lr_pct= 0
	else
		lr_pct =li_sum_rej1/( li_sum_rej1 + li_sum_apr1 )
	end if
	dw_rpt_all_col.SetItem(i,'rejpct1', lr_pct)
	if (li_sum_rej2 + li_sum_apr2 = 0 ) then
		lr_pct= 0
	else
		lr_pct =li_sum_rej2/( li_sum_rej2 + li_sum_apr2 )
	end if
	dw_rpt_all_col.SetItem(i,'rejpct2', lr_pct)
	if (li_sum_rej3 + li_sum_apr3 = 0 ) then
		lr_pct= 0
	else
		lr_pct =li_sum_rej3/( li_sum_rej3 + li_sum_apr3 )
	end if
	dw_rpt_all_col.SetItem(i,'rejpct3', lr_pct)
	if (li_sum_rej4 + li_sum_apr4 = 0 ) then
		lr_pct= 0
	else
		lr_pct =li_sum_rej4/( li_sum_rej4 + li_sum_apr4 )
	end if
	dw_rpt_all_col.SetItem(i,'rejpct4', lr_pct)
	dw_rpt_all_col.SetItem(i,'done', 'Y')
next
li_re =dw_rpt_all_col.update()
if li_re =1 then
	commit using SqlServerTrans;
else
	Rollback using SqlServerTrans;
end if

this.setredraw( true)
return 1

end function

public function integer wf_rpt_apr_rej_ytd_laterejpct_all_prdr_1_mon (datetime cur_start_date, datetime cur_end_date);//this function is for copumting approved, rejected accumulation ytdaprv, ytdreject, and compute late and reject percent
//for all prdr for specific month. This for fiscal year not for last 12 months. The cur_start_date is argument, therefore
//will change from time to time.

date  ld_before_start30	

long li_row_count, i,j,  k,li_re, li_ship,li_row,li_count, li_late, &
		li_apr1,li_apr2, li_apr3, li_sum_apr1,li_sum_apr2, li_sum_apr3, li_rej1, li_rej2,li_rej3,li_rej4,li_apr4, &
		li_sum_rej1,li_sum_rej2, li_sum_rej3, li_sum_rej4, li_sum_apr4
		
string  ls_med, ls_prdr, ls_cntr, ls_fy,ls_filter, ls_pre_fy,  ls_prdr_old='', ls_med_old='', ls_qastg,&
		 ls_fy_start_date, ls_fy_end_date, ls_qastatcd, ls_qastg_old='',&
		ls_qastatcd_old='',ls_cntr_old,ls_cur_start_date, ls_cur_end_date,ls_max,ls_min,ls_curmonyr
		
real lr_pct
str_voucher_report lstr
datetime  ld_bef_30dt

lstr =istr
ls_fy_start_date = lstr.array[10]
ls_fy_end_date = (lstr.array[11])
ls_pre_fy= right(ls_fy_start_date, 4)
ls_fy= right(ls_fy_end_date, 4)
ld_before_start30 = RelativeDate( date(cur_start_date), - 30 )

//don't need retrieve again for dw_rpt_all_col
//one year data aready retrieved out and filter only this month that left in primary buffer
//waiting to update
ls_cur_start_date=string(cur_start_date,'mm/dd/yyyy')
ls_cur_end_date= string(cur_end_date,'mm/dd/yyyy')
ls_filter="date(start_date) =date('"+ls_cur_start_date+"')"+&
											" and date(end_date)= date('"+ls_cur_end_date+"')"
dw_rpt_all_col.SetFilter(ls_filter)
//get all prdr, med, cntr combination for one specific month
dw_rpt_all_col.Filter()
dw_rpt_all_col.Sort()
choose case ls_cur_start_date
	case '10/01/'+ls_pre_fy
		ls_curmonyr='October,'+ls_pre_fy
	case '11/01/'+ls_pre_fy
		ls_curmonyr='November,'+ls_pre_fy
	case '12/01/'+ls_pre_fy
		ls_curmonyr='December,'+ls_pre_fy
	case '01/01/'+ls_fy
		ls_curmonyr='January,'+ls_fy
	case '02/01/'+ls_fy
		ls_curmonyr='February,'+ls_fy
	case '03/01/'+ls_fy
		ls_curmonyr='March,'+ls_fy
	case '04/01/'+ls_fy
		ls_curmonyr='April,'+ls_fy
	case '05/01/'+ls_fy
		ls_curmonyr='May,'+ls_fy
	case '06/01/'+ls_fy
		ls_curmonyr='June,'+ls_fy
	case '07/01/'+ls_fy
		ls_curmonyr='July,'+ls_fy
	case '08/01/'+ls_fy
		ls_curmonyr='August,'+ls_fy
	case '09/01/'+ls_fy
		ls_curmonyr='September,'+ls_fy
end choose
li_row_count =dw_rpt_all_col.RowCount()
dw_rpt_qc_apr_rej_get.SetTransObject( SqlServerTrans )

ld_bef_30dt=datetime(ld_before_start30,time('00:00:00'))
for i =1 to li_row_count
	ls_prdr =dw_rpt_all_col.GetItemString( i,'prdr')
	ls_cntr=dw_rpt_all_col.GetItemString( i,'cntr')
	ls_med =dw_rpt_all_col.GetItemString( i,'cntrmed')
	if ls_prdr<>ls_prdr_old or ls_med<>ls_med_old then
//		w_pics_rpt_msg_box.sle_retrieve.text=&
//			'Dulicate check of approve,reject,ytd,percent data for prdr,med '+ls_prdr+','+ls_med+&
//			' in '+ls_curmonyr
		select max(done),min(done) into :ls_max,:ls_min
		from monrpt
		where (start_date)=:cur_start_date and (end_date)=:cur_end_date and
				prdr=:ls_prdr and cntrmed=:ls_med 
		using SqlServerTrans;
		if not f_check_dberror(SqlServerTrans,'select from monrpt to find done column') then
			return -1
		end if
		if ls_prdr<>ls_prdr_old then
//			w_pics_rpt_msg_box.sle_retrieve.text=&
//			'Create data of approve,reject,ytd,percent data for prdr,med '+ls_prdr+','+ls_med+&
//														' in '+ls_curmonyr
			dw_rpt_qc_apr_rej_get.Retrieve(ls_prdr, cur_start_date,&
														cur_end_date, ld_bef_30dt )	
		end if

	end if// if ls_med<>ls_med_old ,ls_prdr<>
	if ls_max='Y' and ls_min='Y' then continue
	//if ls_prdr, ls_med data aready have then jumt it over
	ls_filter= "cntrmed = '"+ls_med+"'"+" and cntr = '"+ls_cntr+"'" 
	dw_rpt_qc_apr_rej_get.SetFilter( ls_filter )
	//get data for ls_prdr, ls_med, ls_cntr
	dw_rpt_qc_apr_rej_get.Filter()
	dw_rpt_qc_apr_rej_get.GroupCalc()
	li_row=dw_rpt_qc_apr_rej_get.RowCount()
	if li_row =0 then continue
	for k=1 to li_row
		ls_qastg=dw_rpt_qc_apr_rej_get.GetItemString(k,'qastg')
		ls_qastatcd =dw_rpt_qc_apr_rej_get.GetItemString(k,'qastatcd')
		if ls_qastg<> ls_qastg_old or ls_qastatcd <>ls_qastatcd_old or &
			ls_prdr<>ls_prdr_old  or ls_med<> ls_med_old or	ls_cntr<>ls_cntr_old then	
			li_count=dw_rpt_qc_apr_rej_get.GetItemNumber(k,'counter')
			//li_count means how many approved, rejected in each stage
			if IsNull( li_count) then li_count =0
			if ls_qastatcd='A' then
				//this means approved
				choose case ls_qastg
						case '1' 
							//this means in stage 1
							dw_rpt_all_col.SetItem(i,'qc1apprv', li_count)
						case '2'
							dw_rpt_all_col.SetItem(i,'qc2apprv', li_count)
						case '3'
							dw_rpt_all_col.SetItem(i,'qc3apprv', li_count)
						case '4'
							dw_rpt_all_col.SetItem(i,'qc4apprv', li_count)
					end choose
			elseif ls_qastatcd ='R' then
				//this means rejected
				choose case ls_qastg
						case '1' 
							//this means stage 1
							dw_rpt_all_col.SetItem(i,'qc1reject', li_count)
						case '2'
							dw_rpt_all_col.SetItem(i,'qc2reject', li_count)
						case '3'
							dw_rpt_all_col.SetItem(i,'qc3reject', li_count)
						case '4'
							dw_rpt_all_col.SetItem(i,'qc4reject', li_count)
				end choose
			end if
			ls_qastg_old= ls_qastg
			ls_qastatcd_old = ls_qastatcd
		end if//end of ls_qastg <> ls_qastg_old....
	next// for k=1 to li_row
	if ls_prdr<>ls_prdr_old then
		ls_prdr_old=ls_prdr
	end if
	if ls_med<> ls_med_old then
			ls_med_old=ls_med
	end if
	IF ls_cntr<>ls_cntr_old then
		ls_cntr_old=ls_cntr
	end if
next//for i=1 to li_row_count

li_re =dw_rpt_all_col.update()
if li_re =1 then
//	commit using SqlServerTrans;
else
	Rollback using SqlServerTrans;
end if
ls_filter ='' 
// this is for all year ytd commpute and for fixed producer prdr. Getoff the
		//filter, compute the whole year ytd for each month untill to this month
dw_rpt_all_col.SetFilter( ls_filter )
dw_rpt_all_col.Filter()
dw_rpt_all_col.Sort()
li_row_count= dw_rpt_all_col.RowCount()
ls_prdr_old=''
ls_med_old =''
ls_cntr_old =''
for i=1 to li_row_count
	//this loop for computing accumulation values of qc1apprv, qc2apprv, qc3apprv and ctdqc1apprv, ctdqc2apprv... 
	ls_med =dw_rpt_all_col.GetItemString(i,'cntrmed')
	ls_cntr=dw_rpt_all_col.GetItemString(i,'cntr')
	ls_prdr =dw_rpt_all_col.GetItemString(i,'prdr')
	if  ls_med <>ls_med_old or ls_cntr<> ls_cntr_old or ls_prdr<>ls_prdr_old then
		li_sum_apr1=0
		li_sum_apr2=0
		li_sum_apr3=0
		li_sum_apr4=0
		li_sum_rej1=0
		li_sum_rej2=0
		li_sum_rej3=0
		li_sum_rej4=0
		ls_med_old = ls_med
		ls_cntr_old = ls_cntr
		ls_prdr_old=ls_prdr
	end if
	li_apr1= dw_rpt_all_col.GetItemNumber(i,'qc1apprv')
	li_apr2= dw_rpt_all_col.GetItemNumber(i,'qc2apprv')
	li_apr3= dw_rpt_all_col.GetItemNumber(i,'qc3apprv')
	li_apr4= dw_rpt_all_col.GetItemNumber(i,'qc4apprv')
	if isnull(li_apr4) then 
		li_apr4=0
		dw_rpt_all_col.SetItem(i,'qc4apprv',0)
	end if
	li_rej1=dw_rpt_all_col.GetItemNumber(i,'qc1reject')
	li_rej2=dw_rpt_all_col.GetItemNumber(i,'qc2reject')
	li_rej3=dw_rpt_all_col.GetItemNumber(i,'qc3reject')
	li_rej4=dw_rpt_all_col.GetItemNumber(i,'qc4reject')
	if isnull(li_rej4) then 
		li_rej4=0
		dw_rpt_all_col.SetItem(i,'qc4reject',0)
	end if
	li_sum_apr1 += li_apr1
	li_sum_apr2 += li_apr2
	li_sum_apr3 += li_apr3
	li_sum_apr4 += li_apr4
	li_sum_rej1 += li_rej1
	li_sum_rej2 += li_rej2
	li_sum_rej3 += li_rej3
	li_sum_rej4 += li_rej4
	dw_rpt_all_col.SetItem(i,'ctdqc1apprv' ,li_sum_apr1)
	dw_rpt_all_col.SetItem(i,'ctdqc2apprv' ,li_sum_apr2)
	dw_rpt_all_col.SetItem(i,'ctdqc3apprv' ,li_sum_apr3)
	dw_rpt_all_col.SetItem(i,'ctdqc4apprv' ,li_sum_apr4)
	dw_rpt_all_col.SetItem(i,'ctdqc1reject' ,li_sum_rej1)
	dw_rpt_all_col.SetItem(i,'ctdqc2reject' ,li_sum_rej2)
	dw_rpt_all_col.SetItem(i,'ctdqc3reject' ,li_sum_rej3)
	dw_rpt_all_col.SetItem(i,'ctdqc4reject' ,li_sum_rej4)
next
li_re =dw_rpt_all_col.update()
if li_re =1 then
//	commit using SqlServerTrans;
else
	Rollback using SqlServerTrans;
end if

ls_filter ="date(end_date) <=date('"+ls_cur_end_date+"')"+&
									" and date(start_date) >= date('"+ls_cur_start_date+"')"
//now use filter only update data for this current month ls_cur_start_date. the data is latepct and
//reject percent for each stage QC1,QC2,QC3
dw_rpt_all_col.SetFilter( ls_filter )
dw_rpt_all_col.Filter()
dw_rpt_all_col.Sort()
li_row_count =dw_rpt_all_col.RowCount() // for only this month commpute the late percent
				//and the reject percent
for i=1 to li_row_count
	li_late= dw_rpt_all_col.GetItemNumber(i,'latetottitles')
	li_ship=dw_rpt_all_col.GetItemNumber(i,'shiptitles')
	if (li_ship) <>0 then
		lr_pct= li_late/( li_ship )
	else
		lr_pct =0
	end if
	dw_rpt_all_col.SetItem(i,'latepct', lr_pct)
	li_sum_rej1= dw_rpt_all_col.GetItemNumber(i,'ctdqc1reject' )
	li_sum_rej2= dw_rpt_all_col.GetItemNumber(i,'ctdqc2reject' )
	li_sum_rej3= dw_rpt_all_col.GetItemNumber(i,'ctdqc3reject' )
	li_sum_rej4= dw_rpt_all_col.GetItemNumber(i,'ctdqc4reject' )
	li_sum_apr1= dw_rpt_all_col.GetItemNumber(i,'ctdqc1apprv' )
	li_sum_apr2= dw_rpt_all_col.GetItemNumber(i,'ctdqc2apprv' )
	li_sum_apr3= dw_rpt_all_col.GetItemNumber(i,'ctdqc3apprv' )
	li_sum_apr4= dw_rpt_all_col.GetItemNumber(i,'ctdqc4apprv' )
	if (li_sum_rej1 + li_sum_apr1 = 0 ) then
		lr_pct= 0
	else
		lr_pct =li_sum_rej1/( li_sum_rej1 + li_sum_apr1 )
	end if
	dw_rpt_all_col.SetItem(i,'rejpct1', lr_pct)
	if (li_sum_rej2 + li_sum_apr2 = 0 ) then
		lr_pct= 0
	else
		lr_pct =li_sum_rej2/( li_sum_rej2 + li_sum_apr2 )
	end if
	dw_rpt_all_col.SetItem(i,'rejpct2', lr_pct)
	if (li_sum_rej3 + li_sum_apr3 = 0 ) then
		lr_pct= 0
	else
		lr_pct =li_sum_rej3/( li_sum_rej3 + li_sum_apr3 )
	end if
	dw_rpt_all_col.SetItem(i,'rejpct3', lr_pct)
	if (li_sum_rej4 + li_sum_apr4 = 0 ) then
		lr_pct= 0
	else
		lr_pct =li_sum_rej4/( li_sum_rej4 + li_sum_apr4 )
	end if
	dw_rpt_all_col.SetItem(i,'rejpct4', lr_pct)
	dw_rpt_all_col.SetItem(i,'done', 'Y')
next
li_re =dw_rpt_all_col.update()
if li_re =1 then
	commit using SqlServerTrans;
else
	Rollback using SqlServerTrans;
end if

this.setredraw( true)
return 1

end function

public function integer wf_create_all_col_1_prdr_uptonow ();//this function is for create data for you selected prdr, median, for selected fiscal year fy that usually calcurated.
//the data will be created from October untill the date you 
//selected in response window. This is only for one prdr, med combination, the function only for create data for fiscal
//year. If for last 12 months you must use other parallel function in other window w_month_reporthave


date ld_start_date, ld_end_date
long  i,li_re ,li_month, li_fy, j, li_count
string ls_start_date, ls_fy_start_date, ls_fy_end_date, ls_fy,ls_regen,  &		 
		ls_pre_fy, ls_start_date_left,ls_curmon,ls_max,ls_min, ls_prdr,  ls_med, ls_lang
str_voucher_report lstr
datetime ld_stdt, ld_enddt

lstr =istr

ls_prdr=lstr.array[3]
ls_regen = lstr.array[4]
ls_start_date = lstr.array[8]

ls_fy_start_date = lstr.array[10]
ls_fy_end_date = (lstr.array[11])
ls_med = lstr.array[13]

ls_pre_fy= right(ls_fy_start_date, 4)
ls_fy= right(ls_fy_end_date, 4)
li_fy =integer( ls_fy )
ls_start_date_left=left(ls_start_date,2)

choose case ls_start_date_left
	//this constrol the number of months in which the new data will be created if there do not exists in database jet.
	case '10'
		li_month=1
	case '11'
		li_month=2
	case '12'
		li_month=3
	case '01'
		li_month=4
	case '02'
		li_month=5
	case '03'
		li_month=6
	case '04'
		li_month=7
	case '05'
		li_month=8
	case '06'
		li_month=9
	case '07'
		li_month=10
	case '08'
		li_month=11
	case '09'
		li_month=12
end choose

for j=1 to li_month
	choose case j
		case 1
			ld_start_date= date('10/01/'+ls_pre_fy)
			ld_end_date= date('10/31/'+ ls_pre_fy )
			ls_curmon='October, '+ls_pre_fy
		case 2
			ld_start_date= date('11/01/'+ls_pre_fy)
			ld_end_date= date('11/30/'+ ls_pre_fy )
			ls_curmon='November, '+ls_pre_fy
		case 3
			ld_start_date= date('12/01/'+ls_pre_fy)
			ld_end_date= date('12/31/'+ ls_pre_fy )
			ls_curmon='December, ' +ls_pre_fy
		case 4
			ld_start_date= date('01/01/'+ls_fy)
			ld_end_date= date('01/31/'+ ls_fy )
			ls_curmon='January, '+ls_fy
		case 5
			ld_start_date= date('02/01/'+ls_fy)
			ls_curmon='February, '+ls_fy
			if mod(li_fy, 4)=0 then
				ld_end_date= date('02/29/'+ ls_fy )
			else
				ld_end_date= date('02/28/'+ ls_fy )
			end if
		case 6
			ld_start_date= date('03/01/'+ls_fy)
			ld_end_date= date('03/31/'+ ls_fy )
			ls_curmon='March, '+ls_fy
		case 7
			ld_start_date= date('04/01/'+ls_fy)
			ld_end_date= date('04/30/'+ ls_fy )
			ls_curmon='April, '+ls_fy
		case 8
			ld_start_date= date('05/01/'+ls_fy)
			ld_end_date= date('05/31/'+ ls_fy )
			ls_curmon='May, '+ls_fy
		case 9
			ld_start_date= date('06/01/'+ls_fy)
			ld_end_date= date('06/30/'+ ls_fy )
			ls_curmon='June, '+ls_fy
		case 10
			ld_start_date= date('07/01/'+ls_fy)
			ld_end_date= date('07/31/'+ ls_fy )
			ls_curmon='July, '+ls_fy
		case 11
			ld_start_date= date('08/01/'+ls_fy)
			ld_end_date= date('08/31/'+ ls_fy )
			ls_curmon='August, '+ls_fy
		case 12
			ld_start_date= date('09/01/'+ls_fy)
			ld_end_date= date('09/30/'+ ls_fy )
			ls_curmon='September, '+ls_fy
	end choose
	ld_stdt=datetime(ld_start_date,time('00:00:00'))
	ld_enddt=datetime(ld_end_date,time('00:00:00'))
	openwithparm(w_pics_rpt_msg_box,&
		'Generating data for new Fiscal Year '+ls_fy)
	li_re=wf_rpt_ini_all_col_1_prdr_1_year0()
	//if the ls_prdr, med combination have not initialized jet then initialize for this combination
	if li_re= - 1 then
		messagebox('','Error in generating data. Please contact your system administator.')
		return -1
	end if
	if ls_regen='N' or ls_regen ='' then
//		w_pics_rpt_msg_box.sle_retrieve.text=&
//			'Duplicate Check all data for one prdr,med '+ls_prdr+','+ls_med+' in '+ls_curmon
      IF ls_prdr<>'PTB' THEN
			//check ls_prdr, ls_med combination data is ready in the database or not jet
			select  max(done), min(done) into :ls_max, :ls_min
			from monrpt
			where prdr=:ls_prdr and cntrmed=:ls_med and 
					(start_date)=:ld_start_date and (end_date) =:ld_end_date
			using SqlServerTrans;
		elseif ls_prdr='PTB' THEN
			//check ls_prdr, ls_med combination data is ready or not jet, for ls_prdr='PTB' MUST consider there are two ls_lang
			if ls_med='FL' THEN
				ls_med='RC'
				ls_lang='Y'
			elseif (ls_med='RC' OR ls_med='RTB') THEN
				ls_lang='N'
			end if
			select  max(done), min(done) into :ls_max, :ls_min
			from monrpt
			where prdr=:ls_prdr and cntrmed=:ls_med and 
					(start_date)=:ld_start_date and (end_date) =:ld_end_date and
					foreign_lang=:ls_lang
			using SqlServerTrans;
		end if
		if not f_check_dberror(SqlServerTrans,'select max(done),min(done) from monrpt ') then
			return -1
		end if
		if ls_max='N' or ls_min='N' or Isnull(ls_max) or IsNull(ls_min) then
			//the data not ready then create it
			w_pics_rpt_msg_box.sle_retrieve.text=&
			'Generating data for Producer/Media: '+ls_prdr+'/'+ls_med+' for '+ls_curmon
			li_re=wf_rpt_create_awad_asgn_late_ship_1_prdr_1_mon(ld_stdt,ld_enddt)
			if li_re= - 1 then
				messagebox('','Error in generating data. Please contact your system administator.')
				return -1
			end if
//			w_pics_rpt_msg_box.sle_retrieve.text=&
//			'Create Plan,Act,late data for one prdr,med :'+ls_prdr+','+ls_med+' in '+ls_curmon
			li_re=wf_rpt_create_plan_act_reg_late_ship_1_prdr_1_mon(ld_stdt,ld_enddt)
			if li_re= - 1 then
				messagebox('','Error in generating data. Please contact your system administator.')
				return -1
			end if
//			w_pics_rpt_msg_box.sle_retrieve.text=&
//			'Create approve,rejcet,ytd for one prdr,med :'+ls_prdr+','+ls_med+' in '+ls_curmon
			li_re=wf_rpt_apr_rej_ytd_laterejpct_1_prdr_1_mon(ld_stdt,ld_enddt)
			if li_re= - 1 then
				messagebox('','Error in generating data. Please contact your system administator.')
				return -1
			end if
		end if//if ls_max='N'
	elseif ls_regen='Y' then
		//if you select regenarate data then regenerate for this combination of ls_prdr, ls_med
		w_pics_rpt_msg_box.sle_retrieve.text=&
		'Generating data for Producer/Media: '+ls_prdr+'/'+ls_med+' for '+ls_curmon
		li_re=wf_rpt_create_awad_asgn_late_ship_1_prdr_1_mon(ld_stdt,ld_enddt)
		if li_re= - 1 then
			messagebox('','Error in generating data. Please contact your system administator.')
			return -1
		end if
//		w_pics_rpt_msg_box.sle_retrieve.text=&
//		'Create Plan,Act,late data for one prdr,med :'+ls_prdr+','+ls_med+' in '+ls_curmon
		li_re=wf_rpt_create_plan_act_reg_late_ship_1_prdr_1_mon(ld_stdt,ld_enddt)
		if li_re= - 1 then
			messagebox('','Error in generating data. Please contact your system administator.')
			return -1
		end if
//		w_pics_rpt_msg_box.sle_retrieve.text=&
//		'Create approve,rejcet,ytd for one prdr,med :'+ls_prdr+','+ls_med+' in '+ls_curmon
		li_re=wf_rpt_apr_rej_ytd_laterejpct_1_prdr_1_mon(ld_stdt,ld_enddt)
		if li_re= - 1 then
			messagebox('','Error in generating data. Please contact your system administator.')
			return -1
		end if
	end if//if ls_forceregen='N'
	
next// FOR J=1 TO li_month

close( w_pics_rpt_msg_box)
return 1


end function

public function integer wf_rpt_create_awad_asgn_late_ship_all_prdr_1_mon (datetime cur_start_date, datetime cur_end_date);//if fiscal year fy is new one then insert all column data for the whole year
//if fiscal year aready established .The job only refresh the data of this one month
date ld_enddate, ld_fy_start_date, ld_fy_end_date 
long   li_shiptitles,li_titles,li_row_count, i,li_re,j, li_latetitles, li_row,k, li_cntrttl, li_sumttl=0	,&
       li_chttl

string  ls_med, ls_prdr, ls_cntr, ls_cntrlc,  ls_cur_start_date,ls_cur_end_date,ls_filter,ls_curmonyr,&
		ls_start_date, ls_end_date, ls_fy_start_date, ls_fy_end_date, ls_type, ls_enddate, ls_lang, ls_pre_fy	,&
		ls_fy	, ls_prdr_old, ls_med_old, ls_cntr_old, ls_max, ls_min
datetime ld_fystdt,ld_fyenddt
str_voucher_report lstr

lstr =istr


ls_fy_start_date = lstr.array[10]
ls_fy_end_date = (lstr.array[11])



ld_fy_start_date =date( ls_fy_start_date )
ld_fy_end_date =date( ls_fy_end_date )
ls_pre_fy= right(ls_fy_start_date, 4)
ls_fy= right(ls_fy_end_date, 4)

ld_fystdt=datetime(ld_fy_start_date,time('00:00:00'))
ld_fyenddt=datetime(ld_fy_end_date,time('00:00:00'))
dw_rpt_all_col.retrieve(ld_fystdt, ld_fyenddt)
//get all prdr, med,cntr for whole fiscal year
ls_cur_start_date=string(date(cur_start_date),'mm/dd/yyyy')
ls_cur_end_date=string(date(cur_end_date),'mm/dd/yyyy')
ls_filter="date(start_date) >=date('"+ls_cur_start_date+"')"+&
											" and date(end_date)<= date('"+ls_cur_end_date+"')"

dw_rpt_all_col.SetFilter(ls_filter)
//get all prdr, med, cntr for specific month
dw_rpt_all_col.Filter()
dw_rpt_all_col.Sort()
li_row_count =dw_rpt_all_col.Rowcount()
choose case ls_cur_start_date
	case '10/01/'+ls_pre_fy
		ls_curmonyr='October,'+ls_pre_fy
	case '11/01/'+ls_pre_fy
		ls_curmonyr='November,'+ls_pre_fy
	case '12/01/'+ls_pre_fy
		ls_curmonyr='December,'+ls_pre_fy
	case '01/01/'+ls_fy
		ls_curmonyr='January,'+ls_fy
	case '02/01/'+ls_fy
		ls_curmonyr='February,'+ls_fy
	case '03/01/'+ls_fy
		ls_curmonyr='March,'+ls_fy
	case '04/01/'+ls_fy
		ls_curmonyr='April,'+ls_fy
	case '05/01/'+ls_fy
		ls_curmonyr='May,'+ls_fy
	case '06/01/'+ls_fy
		ls_curmonyr='June,'+ls_fy
	case '07/01/'+ls_fy
		ls_curmonyr='July,'+ls_fy
	case '08/01/'+ls_fy
		ls_curmonyr='August,'+ls_fy
	case '09/01/'+ls_fy
		ls_curmonyr='September,'+ls_fy
end choose
dw_deviate_sum_sp_no_detail.SetTransObject(SqlServerTrans)

for i=1 to li_row_count
	ls_prdr=dw_rpt_all_col.GetItemString(i,'prdr')
	ls_med= dw_rpt_all_col.GetItemString(i,'cntrmed')
	ls_cntr= dw_rpt_all_col.GetItemString(i,'cntr')
	
	if ls_prdr<>ls_prdr_old or ls_med<>ls_med_old then
//		w_pics_rpt_msg_box.sle_retrieve.text=&
//		'Duplicate check awadtitles,assigntitles data for prdr,med '+ls_prdr+','+ls_med+&
//			' in '+ls_curmonyr
		select max(done),min(done) into :ls_max,:ls_min
		from monrpt
		where start_date=:cur_start_date and end_date=:cur_end_date and
				prdr=:ls_prdr and cntrmed=:ls_med 
		using SqlServerTrans;
		if not f_check_dberror(SqlServerTrans,'select from monrpt to find done column') then
			return -1
		end if
		if (ls_prdr<> ls_prdr_old)  then
			if	 (not(ls_max='Y' and ls_min='Y' )) then
				//if not created yet then create them
//				w_pics_rpt_msg_box.sle_retrieve.text=&
//					'Create awadtitles,assigntitles,shiptitles for prdr,med '+ls_prdr+','+ls_med+&
//						' in '+ls_curmonyr
				dw_rpt_open_cmp_tottitles.SetTransObject(SqlServerTrans)
				dw_rpt_open_cmp_tottitles.Retrieve(ls_prdr)
				dw_rpt_open_cmp_asigntitles.SetTransObject(SqlServerTrans)
				dw_rpt_open_cmp_asigntitles.Retrieve(ls_prdr)
				dw_rpt_open_cmp_latetitles.SetTransObject(SqlServerTrans)
				//this borrow stored procedure avarage deviation
				dw_rpt_open_cmp_latetitles.Retrieve(ls_prdr )
				dw_rpt_open_cmp_shiptitles.SetTransObject(SqlServerTrans)
				//this borrow stored procedure avarage deviation
				dw_rpt_open_cmp_shiptitles.Retrieve(ls_prdr, cur_end_date )
				
			end if
			ls_prdr_old= ls_prdr
		end if
		if ls_med<>ls_med_old then
			ls_med_old= ls_med
		end if
	end if//ls_prdr<>ls_prdr_old and ls_med<>
	if ls_max='Y' and ls_min='Y' then continue
		//if data aready exists then jump it over
		//now prepare to update the data in this one month
	ls_filter =  "cntrmed= '"+ls_med+"'"+&
						" and cntr= '"+ls_cntr+"'"
	dw_rpt_open_cmp_tottitles.SetFilter( ls_filter )
	//this is for compution awarded titles
	dw_rpt_open_cmp_tottitles.Filter()
	dw_rpt_open_cmp_tottitles.GroupCalc()
	li_row=dw_rpt_open_cmp_tottitles.RowCount()
	li_sumttl=0
	if li_row>0 then
		for k=1 to li_row
			li_cntrttl= dw_rpt_open_cmp_tottitles.GetItemNumber(1,'cntrttl')
			li_chttl= dw_rpt_open_cmp_tottitles.GetItemNumber(k,'chttl')
			if IsNull( li_chttl) then li_chttl=0
			li_sumttl+= li_chttl
		next
		li_cntrttl= li_cntrttl+ li_sumttl
		dw_rpt_all_col.SetItem( i,'awadtitles', li_cntrttl)
	end if					
	dw_rpt_open_cmp_asigntitles.SetFilter( ls_filter )
	//this is for assigned titles
	dw_rpt_open_cmp_asigntitles.Filter()
	li_row=dw_rpt_open_cmp_asigntitles.RowCount()
	dw_rpt_all_col.SetItem( i,'asgntitles', li_row)
	select cntrtype into :ls_type
	from ancntr
	where cntr=:ls_cntr and cntrmed=:ls_med
	using SqlServerTrans;
	if f_check_dberror(SqlServerTrans,'select cntrtype from ancntr')=false then
		return -1
	end if
	ld_enddate=date(cur_end_date)
	ls_enddate=string(ld_enddate,'mm/dd/yyyy')
	dw_deviate_sum_sp_no_detail.Retrieve(ls_cntr, ls_enddate,ls_type)
	li_shiptitles=dw_deviate_sum_sp_no_detail.GetItemNumber(1,'compute_0002')
	//this is borrowed from stored procedure average deviation
	if IsNull( li_shiptitles ) then li_shiptitles=0
	dw_rpt_all_col.SetItem( i,'shiptitles', li_shiptitles)
	li_latetitles=dw_deviate_sum_sp_no_detail.GetItemNumber(1,'compute_0004_1')
	//this is borrowed from stored procedure average deviation
	if IsNull( li_latetitles ) then li_latetitles=0
	dw_rpt_all_col.SetItem( i,'latetottitles', li_latetitles)
next// for i=1 to li_row_count

li_re =dw_rpt_all_col.update()
if li_re =1 then
//	commit using SqlServerTrans;
else
	RollBack using SqlServerTrans;
end if
return 1

end function

public function integer wf_rpt_create_awad_asgn_late_ship_1_prdr_1_mon (datetime cur_start_date, datetime cur_end_date);//this function is for creating awad titles, assigned titles late titles, ship titles for specific prdr, month
//for late ship and shipped titles. At we borrowed stored procedure average deviation

date ld_enddate, ld_fy_start_date, ld_fy_end_date 
long   li_shiptitles,li_titles,li_row_count, i,li_re,j, li_latetitles, li_row,k, li_cntrttl, li_sumttl=0	

string  ls_med, ls_prdr, ls_cntr, ls_cntrlc,  ls_cur_start_date,ls_cur_end_date,ls_filter,&
		ls_start_date, ls_end_date, ls_fy_start_date, ls_fy_end_date, ls_type, ls_enddate, ls_lang		
datetime ld_fystdt,ld_fyenddt
str_voucher_report lstr


lstr =istr

ls_prdr = lstr.array[3]
ls_med = (lstr.array[13])
ls_fy_start_date = lstr.array[10]
ls_fy_end_date = (lstr.array[11])

//ld_before_start30 = RelativeDate( cur_start_date, - 30 )
ld_fy_start_date =date( ls_fy_start_date )
ld_fy_end_date =date( ls_fy_end_date )


//remember in the following wf function about dw_rpt_all_col
//the data in dw all the same and the change only filter
//this dw aways keep the whole year data and use this month as filter to update only one month
ld_fystdt=datetime(ld_fy_start_date,time('00:00:00'))
ld_fyenddt=datetime(ld_fy_end_date,time('00:00:00'))
dw_rpt_all_col.retrieve(ld_fystdt, ld_fyenddt)
ls_cur_start_date=string(cur_start_date,'mm/dd/yyyy')
ls_cur_end_date=string(cur_end_date,'mm/dd/yyyy')
if ls_prdr<>'PTB' THEN
	ls_filter="prdr = '"+ls_prdr+"'"+" and cntrmed= '"+ls_med+"'"+" and "
	ls_filter=ls_filter +"date(start_date) >=date('"+ls_cur_start_date+"')"+&
												" and date(end_date)<= date('"+ls_cur_end_date+"')"
ELSEIF ls_prdr='PTB'  then
	if ls_med='FL' THEN
		ls_med='RC'
		ls_lang='Y'
	elseIF (ls_med='RC' OR ls_med='RTB') THEN
		ls_lang='N'
	end if
	ls_filter="prdr = '"+ls_prdr+"'"+" and cntrmed= '"+ls_med+"'"+" and "
	ls_filter=ls_filter +"date(start_date) >=date('"+ls_cur_start_date+"')"+&
		" and date(end_date)<= date('"+ls_cur_end_date+"')"+" and foreign_lang='"+ls_lang+"'"
end if
dw_rpt_all_col.SetFilter(ls_filter)
//for one comibination of prdr, med, get cntr for specific month
dw_rpt_all_col.Filter()
dw_rpt_all_col.Sort()
dw_rpt_open_cmp_tottitles.SetTransObject(SqlServerTrans)
dw_rpt_open_cmp_tottitles.Retrieve(ls_prdr)
dw_rpt_open_cmp_asigntitles.SetTransObject(SqlServerTrans)
dw_rpt_open_cmp_asigntitles.Retrieve(ls_prdr)
dw_rpt_open_cmp_latetitles.SetTransObject(SqlServerTrans)
dw_rpt_open_cmp_latetitles.Retrieve(ls_prdr )
dw_rpt_open_cmp_shiptitles.SetTransObject(SqlServerTrans)
dw_rpt_open_cmp_shiptitles.Retrieve(ls_prdr, cur_end_date )
		
li_row_count =dw_rpt_all_col.Rowcount()
dw_deviate_sum_sp_no_detail.SetTransObject(SqlServerTrans)

for i=1 to li_row_count
	ls_cntr=trim( dw_rpt_all_col.GetItemString(i,'cntr'))

	ls_filter = "cntrmed = '"+ls_med+"'"+" and cntr= '"+ls_cntr+"'"					
	dw_rpt_open_cmp_tottitles.SetFilter( ls_filter )
	//get for specific prdr, med, cntr compute total titles
	dw_rpt_open_cmp_tottitles.Filter()
	li_row=dw_rpt_open_cmp_tottitles.RowCount()
	li_sumttl=0
	if li_row >0 then
		for k=1 to li_row
			li_cntrttl= dw_rpt_open_cmp_tottitles.GetItemNumber(1,'cntrttl')
			li_titles= dw_rpt_open_cmp_tottitles.GetItemNumber(k,'chttl')
			if isnull(li_titles) then li_titles=0
			li_sumttl+=li_titles
		next
		li_cntrttl+=li_sumttl
	end if
	dw_rpt_all_col.SetItem( i,'awadtitles', li_cntrttl)	
	//set awad titles
	dw_rpt_open_cmp_asigntitles.SetFilter( ls_filter )
	//for specific prdr, med, cntr get asigned titles
	dw_rpt_open_cmp_asigntitles.Filter()
	li_row= dw_rpt_open_cmp_asigntitles.RowCount()
	dw_rpt_all_col.SetItem( i,'asgntitles', li_row)

	select cntrtype into :ls_type
	from ancntr
	where cntr=:ls_cntr
	using sqlservertrans;
	if f_check_dberror(sqlservertrans,'select cntrtype from ancntr')=false then
		return -1
	end if
	ld_enddate=date(cur_end_date)
	ls_enddate=string(ld_enddate,'mm/dd/yyyy')
	dw_deviate_sum_sp_no_detail.Retrieve(ls_cntr, ls_enddate,ls_type)
	li_shiptitles=dw_deviate_sum_sp_no_detail.GetItemNumber(1,'compute_0002')
	//borrowed from procedure average deviation
	if IsNull( li_shiptitles ) then li_shiptitles =0
	dw_rpt_all_col.SetItem( i,'shiptitles', li_shiptitles)
	li_latetitles=dw_deviate_sum_sp_no_detail.GetItemNumber(1,'compute_0004')
	//borrowed from procedure average deviation
	if IsNull( li_latetitles ) then li_latetitles=0
	dw_rpt_all_col.SetItem( i,'latetottitles', li_latetitles)
//***borrow is end
next// for i=1 to li_row_count

li_re =dw_rpt_all_col.update()
if li_re =1 then
//	commit using SqlServerTrans;
else
	RollBack using SqlServerTrans;
end if
return 1

end function

public function integer wf_rpt_text_obj_total_pct_1_prdr_sub (string as_lang);//this function compute all text object of total and percentage values in complex datawindows, dw_from... . in 
//dw_from... , there are seven sub datawindows dw_1, dw_2, d_w4..., in these sub datawindows there are lot of
//text object of total and percentage values. All there values are not save in database table. They must be computed,
//this function just do it. all computation are base on fiscal year data not base last 12 months data.
//if base on last 12 data, the other function named w_rpt_obj_total_pct1prdrsubhave will do parallel job in other
//window w_month_reporthave

date ld_start_date, ld_end_date, ld_stdate,ld_enddate
datetime ldt_datetime
long  i, k,i2, &
		li_apr1,li_apr2, li_apr3,li_apr4, li_sum_apr1,li_sum_apr2, li_sum_apr3,li_sum_apr4, li_rej1, li_rej2,&
	  j, li_count,li_count2,li_apr, li_sum_apr
long  li_acttitles, li_fy		
long		li_rej3, li_rej4, li_sum_rej1,li_sum_rej2, li_sum_rej3, li_sum_rej4, li_rej, li_sum_rej ,li_allowed
long	li_ship,li_sum_ship,li_late, li_sum_late,li_awad, li_sum_awad,li_asgn,li_sum_asgn,&
		li_max_fy
string  ls_med, ls_prdr,ls_cntrlc,ls_cntr, ls_pct,ls_apr_rej, ls_pcs_rej,ls_print,  &
		ls_start_date, ls_end_date,ls_fy_end_date,  ls_fy,ls_yr, ls_month, &
		ls_pd_yes, ls_qa_yes, ls_pd, ls_qa,ls_max,ls_min,ls_lang	,	ls_incldavg,ls_datetime,&
		ls_cntrtype
datetime ld_stdt, ld_enddt		
		 
		
		

real lr_pct, lr_allowed,lr_pcs_rej, lr_qas_rej
str_voucher_report lstr
n_ds lds_agm

lstr =istr
this.SetRedraw( false )

ldt_datetime= datetime(Today(),Now())
ls_datetime= string( ldt_datetime,'mm/dd/yyyy hh:mm AM/PM')
//Setpointer( Hourglass!)
ls_month =lstr.array[1]
ls_yr =lstr.array[2]
ls_prdr =lstr.array[3]
ls_print =lstr.array[5]
ls_med =lstr.array[13]

ls_lang=as_lang

ls_start_date = lstr.array[8]
ls_end_date = (lstr.array[9])
ls_fy_end_date=lstr.array[11]

ls_incldavg = lstr.array[15]
ld_start_date= date( ls_start_date)
ld_end_date =date( ls_end_date )

ls_fy=right(ls_fy_end_date,4)
li_fy =integer( ls_fy )

dw_rpt_fram_prod_qc_tbl_and_bar.SetTransObject( SqlServerTrans )
dw_rpt_sum_cmpt_pct.SetTransObject( SqlServerTrans )
dw_rpt_detl_total_pct.SetTransObject( SqlServerTrans )
dw_rpt_qc_apr_rej_show_total.SetTransObject( SqlServerTrans )
ld_stdt=datetime(ld_start_date,time('00:00:00'))
ld_enddt=datetime(ld_end_date,time('00:00:00'))
//this is complex datawindow, there are seven sub datawindows
j=dw_rpt_fram_prod_qc_tbl_and_bar.Retrieve&
						(ls_prdr,ls_med,ld_stdt, ld_enddt,li_fy,as_lang)
	
IF j = 0 THEN
	dw_rpt_fram_prod_qc_tbl_and_bar.InsertRow(0)
	dw_rpt_fram_prod_qc_tbl_and_bar.object.prdr[1] = ls_prdr
	dw_rpt_fram_prod_qc_tbl_and_bar.object.cntrmed[1] = ls_med
	dw_rpt_fram_prod_qc_tbl_and_bar.object.start_date[1] = ld_stdt
	dw_rpt_fram_prod_qc_tbl_and_bar.object.end_date[1] = ld_enddt
	dw_rpt_fram_prod_qc_tbl_and_bar.object.fy[1] = li_fy
	dw_rpt_fram_prod_qc_tbl_and_bar.object.foreign_lang[1] = as_lang	
END IF
	
Setpointer( Hourglass!)
//this dw_... is for compute total and percentage valuse of text object in dw_2 sub datawindow
k=dw_rpt_sum_cmpt_pct.Retrieve&
						(ls_prdr,ls_med,li_fy,as_lang)
//this dw_... is for compute total and percentage valuse of text object in dw_1 sub datawindow
i=dw_rpt_detl_total_pct.Retrieve&
						(ls_prdr,ls_med,ld_stdt, ld_enddt, as_lang)
						
Setpointer( Hourglass!)
//this dw_... is for compute total and percentage valuse of text object in dw_4 sub datawindow
i2=dw_rpt_qc_apr_rej_show_total.Retrieve&
						(ls_prdr,ls_med, li_fy,as_lang)
Setpointer( Hourglass!)
//this loop is for compute percentage of text object in dw_2 sub datawindows
for j=2 to 14
	li_acttitles=dw_rpt_sum_cmpt_pct.GetItemNumber(2,j)
	if isnull(li_acttitles) then li_acttitles=0
		
	li_late=dw_rpt_sum_cmpt_pct.GetItemNumber(4,j)
	if li_acttitles =0 then
		lr_pct=0
	else
		lr_pct=(li_late/li_acttitles)*100
	end if
	ls_pct=string(lr_pct,'##0,###.0')+'%'
	choose case j
		case 2
			dw_rpt_fram_prod_qc_tbl_and_bar.object.dw_2.object.t_oct.text=ls_pct
		case 3
			dw_rpt_fram_prod_qc_tbl_and_bar.object.dw_2.object.t_nov.text=ls_pct
		case 4
			dw_rpt_fram_prod_qc_tbl_and_bar.object.dw_2.object.t_dec.text=ls_pct
		case 5
			dw_rpt_fram_prod_qc_tbl_and_bar.object.dw_2.object.t_jan.text=ls_pct
		case 6
			dw_rpt_fram_prod_qc_tbl_and_bar.object.dw_2.object.t_feb.text=ls_pct
		case 7
			dw_rpt_fram_prod_qc_tbl_and_bar.object.dw_2.object.t_mar.text=ls_pct
		case 8
			dw_rpt_fram_prod_qc_tbl_and_bar.object.dw_2.object.t_apr.text=ls_pct
		case 9
			dw_rpt_fram_prod_qc_tbl_and_bar.object.dw_2.object.t_may.text=ls_pct	
		case 10
			dw_rpt_fram_prod_qc_tbl_and_bar.object.dw_2.object.t_jun.text=ls_pct
		case 11
			dw_rpt_fram_prod_qc_tbl_and_bar.object.dw_2.object.t_jul.text=ls_pct
		case 12
			dw_rpt_fram_prod_qc_tbl_and_bar.object.dw_2.object.t_agu.text=ls_pct
		case 13
			dw_rpt_fram_prod_qc_tbl_and_bar.object.dw_2.object.t_sep.text=ls_pct
		case 14
			dw_rpt_fram_prod_qc_tbl_and_bar.object.dw_2.object.t_ytd.text=ls_pct		
	end choose
next// for j=2 to 14
li_count =dw_rpt_detl_total_pct.RowCount()
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
//this loop is for compute total of text object in dw_1 sub datawindows
for j=1 to li_count
	li_apr1=dw_rpt_detl_total_pct.GetItemNumber(j,'ctdqc1apprv')
	li_sum_apr1 +=li_apr1
	li_apr2=dw_rpt_detl_total_pct.GetItemNumber(j,'ctdqc2apprv')
	li_sum_apr2 +=li_apr2
	li_apr3=dw_rpt_detl_total_pct.GetItemNumber(j,'ctdqc3apprv')
	li_sum_apr3 +=li_apr3
	li_apr4=dw_rpt_detl_total_pct.GetItemNumber(j,'ctdqc4apprv')
	li_sum_apr4 +=li_apr4
	li_rej1=dw_rpt_detl_total_pct.GetItemNumber(j,'ctdqc1reject')
	li_sum_rej1 +=li_rej1
	li_rej2=dw_rpt_detl_total_pct.GetItemNumber(j,'ctdqc2reject')
	li_sum_rej2 +=li_rej2
	li_rej3=dw_rpt_detl_total_pct.GetItemNumber(j,'ctdqc3reject')
	li_sum_rej3 +=li_rej3
	li_rej4=dw_rpt_detl_total_pct.GetItemNumber(j,'ctdqc4reject')
	li_sum_rej4 +=li_rej4

	li_awad=dw_rpt_detl_total_pct.GetItemNumber(j,'awadtitles')
	li_sum_awad +=li_awad
	li_asgn=dw_rpt_detl_total_pct.GetItemNumber(j,'asgntitles')
	li_sum_asgn +=li_asgn
	li_ship=dw_rpt_detl_total_pct.GetItemNumber(j,'shiptitles')
	li_sum_ship +=li_ship
	li_late=dw_rpt_detl_total_pct.GetItemNumber(j,'latetottitles')
	li_sum_late +=li_late	
next//for j=1 to li_count total pct at bottom
//this section is for computing, setting percentage and total of text object in dw_1 sub datawindows
if li_sum_rej1+ li_sum_apr1=0 then
	lr_pct=0
else
	lr_pct=li_sum_rej1/(li_sum_rej1 + li_sum_apr1)*100
end if
ls_pct=string(lr_pct,'##0,###.0')+'%'
dw_rpt_fram_prod_qc_tbl_and_bar.object.dw_1.object.t_rej1.text=ls_pct
if li_sum_rej2 +li_sum_apr2=0 then
	lr_pct=0
else
	lr_pct=li_sum_rej2/(li_sum_rej2 + li_sum_apr2)*100
end if
ls_pct=string(lr_pct,'##0,###.0')+'%'
dw_rpt_fram_prod_qc_tbl_and_bar.object.dw_1.object.t_rej2.text=ls_pct	
if li_sum_rej3 +li_sum_apr3=0 then
	lr_pct=0
else
	lr_pct=li_sum_rej3/(li_sum_rej3 + li_sum_apr3)*100
end if
ls_pct=string(lr_pct,'##0,###.0')+'%'
dw_rpt_fram_prod_qc_tbl_and_bar.object.dw_1.object.t_rej3.text=ls_pct
if li_sum_rej4 +li_sum_apr4=0 then
	lr_pct=0
else
	lr_pct=li_sum_rej4/(li_sum_rej4 + li_sum_apr4)*100
end if
ls_pct=string(lr_pct,'##0,###.0')+'%'
dw_rpt_fram_prod_qc_tbl_and_bar.object.dw_1.object.t_rej4.text=ls_pct
dw_rpt_fram_prod_qc_tbl_and_bar.object.dw_1.object.t_awad.text=string(li_sum_awad)
dw_rpt_fram_prod_qc_tbl_and_bar.object.dw_1.object.t_asgn.text=string(li_sum_asgn)
dw_rpt_fram_prod_qc_tbl_and_bar.object.dw_1.object.t_ship.text=string(li_sum_ship)
if li_sum_ship=0 then
	lr_pct=0
else
	lr_pct=(li_sum_late/li_sum_ship)* 100
end if
ls_pct=string(lr_pct,'##0,###.0')+'%'
dw_rpt_fram_prod_qc_tbl_and_bar.object.dw_1.object.t_late.text=ls_pct
dw_rpt_fram_prod_qc_tbl_and_bar.object.dw_4.object.t_awad.text=string(li_sum_awad)
li_sum_apr=0
//this loop is for computing, setting of total and percentage of text object in dw_4 sub datawindows
for k=3 to 15
	li_sum_rej=0
	for j=1 to 8
		ls_apr_rej=dw_rpt_qc_apr_rej_show_total.GetItemString(j,'apr_rej')
		ls_apr_rej=trim( ls_apr_rej )
		if ls_apr_rej='Rejected' then
			li_rej=dw_rpt_qc_apr_rej_show_total.GetItemNumber(j, k)
			if IsNull(li_rej) then li_rej=0
				li_sum_rej +=li_rej
		end if
		if k=15 and ls_apr_rej='Approved' then
			li_apr=dw_rpt_qc_apr_rej_show_total.GetItemNumber(j, k)
			if IsNull(li_apr) then li_apr=0
			li_sum_apr +=li_apr
		end if		
	next //for j=1 to 8
	choose case k
	case 3
		dw_rpt_fram_prod_qc_tbl_and_bar.object.dw_4.object.t_oct.text=string(li_sum_rej)		
	case 4
		dw_rpt_fram_prod_qc_tbl_and_bar.object.dw_4.object.t_nov.text=string(li_sum_rej)
	case 5
		dw_rpt_fram_prod_qc_tbl_and_bar.object.dw_4.object.t_dec.text=string(li_sum_rej)		
	case 6
		dw_rpt_fram_prod_qc_tbl_and_bar.object.dw_4.object.t_jan.text=string(li_sum_rej)	
	case 7
		dw_rpt_fram_prod_qc_tbl_and_bar.object.dw_4.object.t_feb.text=string(li_sum_rej)		
	case 8
		dw_rpt_fram_prod_qc_tbl_and_bar.object.dw_4.object.t_mar.text=string(li_sum_rej)
	case 9
		dw_rpt_fram_prod_qc_tbl_and_bar.object.dw_4.object.t_apr.text=string(li_sum_rej)		
	case 10
		dw_rpt_fram_prod_qc_tbl_and_bar.object.dw_4.object.t_may.text=string(li_sum_rej)				
	case 11
		dw_rpt_fram_prod_qc_tbl_and_bar.object.dw_4.object.t_jun.text=string(li_sum_rej)		
	case 12
		dw_rpt_fram_prod_qc_tbl_and_bar.object.dw_4.object.t_jul.text=string(li_sum_rej)
	case 13
		dw_rpt_fram_prod_qc_tbl_and_bar.object.dw_4.object.t_agu.text=string(li_sum_rej)		
	case 14
		dw_rpt_fram_prod_qc_tbl_and_bar.object.dw_4.object.t_sep.text=string(li_sum_rej)	
	case 15
		dw_rpt_fram_prod_qc_tbl_and_bar.object.dw_4.object.t_ytd.text=string(li_sum_rej)		
		if li_sum_rej+ li_sum_apr=0 then
			lr_pct=0
		else
			lr_pct=(li_sum_rej/(li_sum_rej+ li_sum_apr))* 100
		end if
		ls_pct=string(lr_pct,'##0,###.0')+'%'
		dw_rpt_fram_prod_qc_tbl_and_bar.object.dw_4.object.t_ytdpct.text=ls_pct	
	end choose
next // for k=1 to 15	
//this section is for computing and setting text object in the sub datawindow such as dw_2..., dw_4... .
 
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
dw_rpt_fram_prod_qc_tbl_and_bar.object.dw_4.object.t_allowed.text=&
																		string(lr_qas_rej,'###,##0')	
dw_rpt_fram_prod_qc_tbl_and_bar.object.dw_2.object.t_allowed.text=ls_pcs_rej

//this section is for computing and setting satisfaction property of production and assurance and other properties
//in fram datawindow dw_fram... 

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
dw_rpt_fram_prod_qc_tbl_and_bar.object.t_pd_yes.text= ls_pd
dw_rpt_fram_prod_qc_tbl_and_bar.object.t_qa_yes.text= ls_qa
dw_rpt_fram_prod_qc_tbl_and_bar.object.t_myr.text= ls_month+' '+ls_yr
choose case ls_med
	case 'RC'
		if as_lang='N' then
			dw_rpt_fram_prod_qc_tbl_and_bar.object.t_book.text= 'Recorded Books'
		else
			dw_rpt_fram_prod_qc_tbl_and_bar.object.t_book.text= 'Recorded Books (Foreign Language)'
		end if
	case 'BR'
		if as_lang='N' then
			dw_rpt_fram_prod_qc_tbl_and_bar.object.t_book.text= 'Press Braille'
		else
			dw_rpt_fram_prod_qc_tbl_and_bar.object.t_book.text= 'Press Braille (Foreign Language)'
		end if
		
	CASE 'P/B'
		if as_lang='N' then
			dw_rpt_fram_prod_qc_tbl_and_bar.object.t_book.text= 'Print Braille'
		else
			dw_rpt_fram_prod_qc_tbl_and_bar.object.t_book.text= 'Print Braille (Foreign Language)'
		end if
	case 'RTB'
		if as_lang='N' then
			dw_rpt_fram_prod_qc_tbl_and_bar.object.t_book.text= 'Recorded Talking Books'
		else
			dw_rpt_fram_prod_qc_tbl_and_bar.object.t_book.text= 'Recorded Talking Books (Foreign Language)'
		end if
END CHOOSE
dw_rpt_fram_prod_qc_tbl_and_bar.object.t_datetime.text= ls_datetime
if ls_print='Y' then
	dw_rpt_fram_prod_qc_tbl_and_bar.triggerevent('pfc_print') 
end if
//this parts are borrowed from w_pcs_reports
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
Setpointer( Arrow!)
this.setredraw( true)
return 1

end function

public function integer wf_rpt_text_obj_total_pct_all_prdr_sub (string as_prdr, string as_med, string as_lang);//	THIS function is used for compute total or percent. All this is not in the database, 
//is not in the column of a table. It must be computed and then use text object of sub datawidow of complex datawindow
// to represent these numbers. First to retrieve the complext datawindow.It has sub datawinows,
//total together there are seven of them. For this sub datawinows there are lot of text object to represent the total
// and percent. For the prod and qa comments we use string's add operation to get total 
// comments for this month for specific prdr
// for each prdr ,cntrmed, cntr combination to get deviation reports which come 
//from pcsrpt.pbl and if no book come out then print summary infomation
date ld_start_date, ld_end_date, &
		  ld_stdate,ld_enddate
datetime ldt_datetime
long i, k, &
		li_apr1,li_apr2, li_apr3, li_apr4, li_sum_apr1,li_sum_apr2, li_sum_apr3, li_sum_apr4, li_rej1, li_rej2,&
	  j, li_count,li_apr, li_sum_apr, li_row, li_count2
long  li_acttitles,  li_fy,&		
		li_rej3, li_rej4, li_sum_rej1,li_sum_rej2, li_sum_rej3, li_sum_rej4, li_rej,li_sum_rej,li_allowed, &
		li_ship,li_sum_ship,li_late, li_sum_late,li_awad, li_sum_awad,li_asgn,li_sum_asgn,&
		li_max_fy
string  ls_cntrt, ls_cntrlc, &
		ls_start_date, ls_end_date, ls_datetime,ls_fy_end_date, ls_fy, &
		 ls_yr, ls_month,&		
		ls_pct,ls_apr_rej, ls_pcs_rej, ls_print,  &
		 ls_pd_yes, ls_qa_yes, ls_cntr, ls_pd, ls_qa,&
		ls_incldavg, ls_cntrtype
datetime ld_stdt, ld_enddt
		
real lr_pct, lr_allowed,lr_pcs_rej, lr_qas_rej
str_voucher_report lstr

lstr =istr
this.SetRedraw( false )
ldt_datetime= datetime(Today(),Now())
ls_datetime= string( ldt_datetime,'mm/dd/yyyy hh:mm AM/PM')
Setpointer( Hourglass!)
ls_month =lstr.array[1]
ls_yr =lstr.array[2]
ls_print =lstr.array[5]
ls_start_date = lstr.array[8]
ls_end_date = (lstr.array[9])
ls_fy_end_date = (lstr.array[11])
ls_incldavg= lstr.array[15]
//
ld_start_date= date( ls_start_date)
ld_end_date =date( ls_end_date )
ls_fy=right(ls_fy_end_date,4)
li_fy=long(ls_fy)

dw_rpt_fram_prod_qc_tbl_and_bar.SetTransObject( SqlServerTrans )
dw_rpt_sum_cmpt_pct.SetTransObject( SqlServerTrans )
dw_rpt_detl_total_pct.SetTransObject( SqlServerTrans )
dw_rpt_qc_apr_rej_show_total.SetTransObject( SqlServerTrans )


ld_stdt=datetime(ld_start_date,time('00:00:00'))
ld_enddt=datetime(ld_end_date,time('00:00:00'))
//this datawindow is complext datawidow, in which threre are seven sub datawindows, all the following dw_...s are
//used to compute total and percentage valuse to set text object of sub_dw...s( dw1, dw2, dw4 ...)
dw_rpt_fram_prod_qc_tbl_and_bar.Retrieve&
						(as_prdr,as_med,ld_stdt,ld_enddt,li_fy,as_lang)
Setpointer( Hourglass!)
//this dw_ is for compute text object of total and percentage in dw_2 sub datawindow
dw_rpt_sum_cmpt_pct.Retrieve&
						(as_prdr,as_med,li_fy,as_lang)
//this dw_ is for compute text object of total and percentage in dw_1 sub datawindow
dw_rpt_detl_total_pct.Retrieve&
						(as_prdr,as_med,ld_stdt,ld_enddt,as_lang)
Setpointer( Hourglass!)
//this dw_ is for compute text object of total and percentage in dw_4 sub datawindow
dw_rpt_qc_apr_rej_show_total.Retrieve&
						(as_prdr,as_med,li_fy,as_lang)
Setpointer( Hourglass!)
//this loop is set text object of percent in dw_2
for j=2 to 14
	li_acttitles=dw_rpt_sum_cmpt_pct.GetItemNumber(2,j)
	if isnull(li_acttitles) then li_acttitles=0
	li_late=dw_rpt_sum_cmpt_pct.GetItemNumber(4,j)
	if li_acttitles =0 then
		lr_pct=0
	else
		lr_pct=(li_late/li_acttitles)*100
	end if
	ls_pct=string(lr_pct,'##0,###.0')+'%'
	choose case j
		case 2
			dw_rpt_fram_prod_qc_tbl_and_bar.object.dw_2.object.t_oct.text=ls_pct
		case 3
			dw_rpt_fram_prod_qc_tbl_and_bar.object.dw_2.object.t_nov.text=ls_pct
		case 4
			dw_rpt_fram_prod_qc_tbl_and_bar.object.dw_2.object.t_dec.text=ls_pct
		case 5
			dw_rpt_fram_prod_qc_tbl_and_bar.object.dw_2.object.t_jan.text=ls_pct
		case 6
			dw_rpt_fram_prod_qc_tbl_and_bar.object.dw_2.object.t_feb.text=ls_pct
		case 7
			dw_rpt_fram_prod_qc_tbl_and_bar.object.dw_2.object.t_mar.text=ls_pct
		case 8
			dw_rpt_fram_prod_qc_tbl_and_bar.object.dw_2.object.t_apr.text=ls_pct
		case 9
			dw_rpt_fram_prod_qc_tbl_and_bar.object.dw_2.object.t_may.text=ls_pct	
		case 10
			dw_rpt_fram_prod_qc_tbl_and_bar.object.dw_2.object.t_jun.text=ls_pct
		case 11
			dw_rpt_fram_prod_qc_tbl_and_bar.object.dw_2.object.t_jul.text=ls_pct
		case 12
			dw_rpt_fram_prod_qc_tbl_and_bar.object.dw_2.object.t_agu.text=ls_pct
		case 13
			dw_rpt_fram_prod_qc_tbl_and_bar.object.dw_2.object.t_sep.text=ls_pct
		case 14
			dw_rpt_fram_prod_qc_tbl_and_bar.object.dw_2.object.t_ytd.text=ls_pct		
	end choose
next// for j=2 to 14
li_count =dw_rpt_detl_total_pct.RowCount()
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
//this loop is computing values of text object of total in dw_1
for j=1 to li_count
	li_apr1=dw_rpt_detl_total_pct.GetItemNumber(j,'ctdqc1apprv')
	if isnull(li_apr1) then li_apr1=0
	li_sum_apr1 +=li_apr1
	li_apr2=dw_rpt_detl_total_pct.GetItemNumber(j,'ctdqc2apprv')
	if isnull(li_apr2) then li_apr2=0
	li_sum_apr2 +=li_apr2
	li_apr3=dw_rpt_detl_total_pct.GetItemNumber(j,'ctdqc3apprv')
	if isnull(li_apr3) then li_apr3=0
	li_sum_apr3 +=li_apr3
	li_apr4=dw_rpt_detl_total_pct.GetItemNumber(j,'ctdqc4apprv')
	if isnull(li_apr4) then li_apr4=0
	li_sum_apr4 +=li_apr4
	li_rej1=dw_rpt_detl_total_pct.GetItemNumber(j,'ctdqc1reject')
	if isnull(li_rej1) then li_rej1=0
	li_sum_rej1 +=li_rej1
	li_rej2=dw_rpt_detl_total_pct.GetItemNumber(j,'ctdqc2reject')
	if isnull(li_rej2) then li_rej2=0
	li_sum_rej2 +=li_rej2
	li_rej3=dw_rpt_detl_total_pct.GetItemNumber(j,'ctdqc3reject')
	if isnull(li_rej3) then li_rej3=0
	li_sum_rej3 +=li_rej3
	li_rej4=dw_rpt_detl_total_pct.GetItemNumber(j,'ctdqc4reject')
	if isnull(li_rej4) then li_rej4=0
	li_sum_rej4 +=li_rej4
	li_awad=dw_rpt_detl_total_pct.GetItemNumber(j,'awadtitles')
	li_sum_awad +=li_awad
	li_asgn=dw_rpt_detl_total_pct.GetItemNumber(j,'asgntitles')
	li_sum_asgn +=li_asgn
	li_ship=dw_rpt_detl_total_pct.GetItemNumber(j,'shiptitles')
	li_sum_ship +=li_ship
	li_late=dw_rpt_detl_total_pct.GetItemNumber(j,'latetottitles')
	li_sum_late +=li_late	
next//for j=1 to li_count total pct at bottom
//this is set text object of percent in dw_1
if li_sum_rej1+ li_sum_apr1=0 then
	lr_pct=0
else
	lr_pct=li_sum_rej1/(li_sum_rej1 + li_sum_apr1)*100
end if
ls_pct=string(lr_pct,'##0,###.0')+'%'
dw_rpt_fram_prod_qc_tbl_and_bar.object.dw_1.object.t_rej1.text=ls_pct
if li_sum_rej2 +li_sum_apr2=0 then
	lr_pct=0
else
	lr_pct=li_sum_rej2/(li_sum_rej2 + li_sum_apr2)*100
end if
ls_pct=string(lr_pct,'##0,###.0')+'%'
dw_rpt_fram_prod_qc_tbl_and_bar.object.dw_1.object.t_rej2.text=ls_pct	
if li_sum_rej3 +li_sum_apr3=0 then
	lr_pct=0
else
	lr_pct=li_sum_rej3/(li_sum_rej3 + li_sum_apr3)*100
end if
//this is set values of text object of total and percent in dw_1
ls_pct=string(lr_pct,'##0,###.0')+'%'
dw_rpt_fram_prod_qc_tbl_and_bar.object.dw_1.object.t_rej3.text=ls_pct
if li_sum_rej4 +li_sum_apr4=0 then
	lr_pct=0
else
	lr_pct=li_sum_rej4/(li_sum_rej4 + li_sum_apr4)*100
end if
ls_pct=string(lr_pct,'##0,###.0')+'%'
dw_rpt_fram_prod_qc_tbl_and_bar.object.dw_1.object.t_rej4.text=ls_pct	
dw_rpt_fram_prod_qc_tbl_and_bar.object.dw_1.object.t_awad.text=string(li_sum_awad)
dw_rpt_fram_prod_qc_tbl_and_bar.object.dw_1.object.t_asgn.text=string(li_sum_asgn)
dw_rpt_fram_prod_qc_tbl_and_bar.object.dw_1.object.t_ship.text=string(li_sum_ship)
if li_sum_ship=0 then
	lr_pct=0
else
	lr_pct=(li_sum_late/li_sum_ship)* 100
end if
ls_pct=string(lr_pct,'##0,###.0')+'%'
dw_rpt_fram_prod_qc_tbl_and_bar.object.dw_1.object.t_late.text=ls_pct
dw_rpt_fram_prod_qc_tbl_and_bar.object.dw_4.object.t_awad.text=string(li_sum_awad)
li_sum_apr=0
//this loop is computing total and percent values in dw_4
for k=3 to 15
	li_sum_rej=0
	for j=1 to 8
		ls_apr_rej=dw_rpt_qc_apr_rej_show_total.GetItemString(j,'apr_rej')
		ls_apr_rej=trim( ls_apr_rej )
		if ls_apr_rej='Rejected' then
			li_rej=dw_rpt_qc_apr_rej_show_total.GetItemNumber(j, k)
			if IsNull(li_rej) then li_rej=0
				li_sum_rej +=li_rej
		end if
		if k=15 and ls_apr_rej='Approved' then
			li_apr=dw_rpt_qc_apr_rej_show_total.GetItemNumber(j, k)
			if IsNull(li_apr) then li_apr=0
			li_sum_apr +=li_apr
		end if		
	next //for j=1 to 8
	//this is set values of total and percent to text object  in dw_4
	choose case k
	case 3
		dw_rpt_fram_prod_qc_tbl_and_bar.object.dw_4.object.t_oct.text=string(li_sum_rej)		
	case 4
		dw_rpt_fram_prod_qc_tbl_and_bar.object.dw_4.object.t_nov.text=string(li_sum_rej)
	case 5
		dw_rpt_fram_prod_qc_tbl_and_bar.object.dw_4.object.t_dec.text=string(li_sum_rej)		
	case 6
		dw_rpt_fram_prod_qc_tbl_and_bar.object.dw_4.object.t_jan.text=string(li_sum_rej)	
	case 7
		dw_rpt_fram_prod_qc_tbl_and_bar.object.dw_4.object.t_feb.text=string(li_sum_rej)		
	case 8
		dw_rpt_fram_prod_qc_tbl_and_bar.object.dw_4.object.t_mar.text=string(li_sum_rej)
	case 9
		dw_rpt_fram_prod_qc_tbl_and_bar.object.dw_4.object.t_apr.text=string(li_sum_rej)		
	case 10
		dw_rpt_fram_prod_qc_tbl_and_bar.object.dw_4.object.t_may.text=string(li_sum_rej)				
	case 11
		dw_rpt_fram_prod_qc_tbl_and_bar.object.dw_4.object.t_jun.text=string(li_sum_rej)		
	case 12
		dw_rpt_fram_prod_qc_tbl_and_bar.object.dw_4.object.t_jul.text=string(li_sum_rej)
	case 13
		dw_rpt_fram_prod_qc_tbl_and_bar.object.dw_4.object.t_agu.text=string(li_sum_rej)		
	case 14
		dw_rpt_fram_prod_qc_tbl_and_bar.object.dw_4.object.t_sep.text=string(li_sum_rej)	
	case 15
		dw_rpt_fram_prod_qc_tbl_and_bar.object.dw_4.object.t_ytd.text=string(li_sum_rej)		
		if li_sum_rej+ li_sum_apr=0 then
			lr_pct=0
		else
			lr_pct=(li_sum_rej/(li_sum_rej+ li_sum_apr))* 100
		end if
		ls_pct=string(lr_pct,'##0,###.0')+'%'
		dw_rpt_fram_prod_qc_tbl_and_bar.object.dw_4.object.t_ytdpct.text=ls_pct	
	end choose
next // for k=1 to 15		
select max(cntrfy) into :li_max_fy
from ancntr
where prdr=:as_prdr and cntrmed=:as_med and
		cntrlc is not null and cntr_status='A'
using SqlServerTrans;
select max(cntrlc) into :ls_cntrlc
from ancntr
where prdr=:as_prdr and cntrmed=:as_med and
		cntrfy=:li_max_fy and cntrlc is not null and
		cntr_status='A'
using SqlServerTrans;
select pcs_reject, qas_reject into :lr_pcs_rej, :lr_qas_rej
from ancntr
where cntrlc=:ls_cntrlc and cntrmed= :as_med
using SqlServerTrans;
lr_pcs_rej=lr_pcs_rej*100
ls_pcs_rej= string(lr_pcs_rej,'###,##0.00')+'%'
dw_rpt_fram_prod_qc_tbl_and_bar.object.dw_4.object.t_allowed.text=&
																		string(lr_qas_rej,'###,##0')	
dw_rpt_fram_prod_qc_tbl_and_bar.object.dw_2.object.t_allowed.text=ls_pcs_rej
// this is computing values of satisfation with production and quality assurance
if as_prdr<>'PTB' THEN
	select max( qastat), max(prdstat) into :ls_qa_yes, :ls_pd_yes
	from monrpt
	where prdr= :as_prdr and cntrmed= :as_med and
		start_date= :ld_stdt and end_date=:ld_enddt
	using SqlServerTrans;
ELSEif as_prdr='PTB' then
	k=100
	select max( qastat), max(prdstat) into :ls_qa_yes, :ls_pd_yes
	from monrpt
	where prdr= :as_prdr and cntrmed= :as_med and
		start_date= :ld_stdt and end_date=:ld_enddt and foreign_lang=:as_lang
	using SqlServerTrans;
end if
if not f_check_dberror(SqlServerTrans,'select qastat and prdstat from monrpt') then
	return -1
end if
//this is for computing and setting valuse of satisfaction to text object  in dw_fram... complex datawindow
if ls_qa_yes='Y' then
//		cbx_qa.checked= true
	ls_qa='Satisfactory'
else
//		cbx_qa.checked=false
	ls_qa='Not Satisfactory'
end if
if ls_pd_yes='Y' then
//		cbx_delv.checked= true
	ls_pd='Satisfactory'
else
//		cbx_delv.checked=false
	ls_pd='Not Satisfactory'
end if
dw_rpt_fram_prod_qc_tbl_and_bar.object.t_pd_yes.text= ls_pd
dw_rpt_fram_prod_qc_tbl_and_bar.object.t_qa_yes.text= ls_qa
dw_rpt_fram_prod_qc_tbl_and_bar.object.t_myr.text= ls_month+' '+ls_yr
choose case as_med
	case 'RC'
		if as_lang='N' then
			dw_rpt_fram_prod_qc_tbl_and_bar.object.t_book.text= 'Recorded Books'
		else
			dw_rpt_fram_prod_qc_tbl_and_bar.object.t_book.text= 'Recorded Books (Foreign Language)'
		end if
	case 'BR'
		if as_lang='N' then
			dw_rpt_fram_prod_qc_tbl_and_bar.object.t_book.text= 'Press Braille'
		else
			dw_rpt_fram_prod_qc_tbl_and_bar.object.t_book.text= 'Press Braille (Foreign Language)'
		end if
	CASE 'P/B'
		if as_lang='N' then
			dw_rpt_fram_prod_qc_tbl_and_bar.object.t_book.text= 'Print Braille'
		else
			dw_rpt_fram_prod_qc_tbl_and_bar.object.t_book.text= 'Print Braille (Foreign Language)'
		end if
	case 'RTB'
		if as_lang='N' then
			dw_rpt_fram_prod_qc_tbl_and_bar.object.t_book.text= 'Recorded Talking Books'
		else
			dw_rpt_fram_prod_qc_tbl_and_bar.object.t_book.text= 'Recorded Talking Books (Foreign Language)'
		end if
END CHOOSE
dw_rpt_fram_prod_qc_tbl_and_bar.object.t_datetime.text=ls_datetime
i_count+=1
if i_count=1 and ls_print='Y' then
	dw_rpt_fram_prod_qc_tbl_and_bar.triggerevent('pfc_print') 
elseif i_count<=1000 and ls_print='Y'  then
	dw_rpt_fram_prod_qc_tbl_and_bar.print()
end if
//the following is borrow from w_pcs_reports
if ls_incldavg='Y'  and i_count<=1000 and ls_print ='Y' then
	dw_rpt_cntr_cntrlc.SetTransObject( SqlServerTrans )
	////following two datawindows come from pcsrpts.pbl
	dw_pcdeviaten_ace_report.SetTransObject( SqlServerTrans )
	dw_deviate_sum_sp_no_detail.SetTransObject( SqlServerTrans )
	dw_rpt_cntr_cntrlc.Retrieve( as_prdr,as_med, ld_stdt, ld_enddt )
	ld_stdate=date(ld_stdt)
	ld_enddate=date(ld_enddt)
	li_count2=dw_rpt_cntr_cntrlc.RowCount()
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
			if f_check_dberror(SqlServerTrans,'select cntrtype from ancntr') =false then
				return -1
			end if
			ls_end_date=string(ld_end_date,'mm/dd/yyyy')
			dw_deviate_sum_sp_no_detail.Retrieve (ls_cntr, ls_end_date,ls_cntrtype)
			dw_deviate_sum_sp_no_detail.print() 
		END IF		
	next //for j=1 to li_count2
end if
return 1

end function

public function integer wf_rpt_ini_all_col_all_prdr_1_year0 ();//if fiscal year fy is new one then insert all column data for the whole year
//if fiscal year aready established .The job only refresh the data of this one month

date ld_start_date, ld_end_date,ld_fy_start_date, ld_fy_end_date, ld_before_start30
long li_row_count, i, li_re ,li_count2,j ,k , li_count, li_cur, li_fy	
string  ls_med, ls_prdr, ls_cntr, ls_cntrlc, ls_fy,ls_filter,ls_pre_fy, ls_lang, ls_start_date, ls_end_date, &
		ls_fy_start_date, ls_fy_end_date, ls_prdr2, ls_med2, ls_cntr2
		
str_voucher_report lstr
datetime ld_stdt, ld_enddt, ld_bef_30dt, ld_fystdt, ld_fyenddt
boolean lb_find	

lstr =istr
ls_fy_start_date = lstr.array[10]
ls_fy_end_date = (lstr.array[11])
ld_fy_start_date =date( ls_fy_start_date )
ld_fy_end_date =date( ls_fy_end_date )
ls_pre_fy= right(ls_fy_start_date, 4)
ls_fy= right(ls_fy_end_date, 4)
li_fy =integer( ls_fy )
dw_rpt_all_col.SetTransObject(SqlServerTrans)
ld_fystdt=datetime(ld_fy_start_date,time('00:00:00'))
ld_fyenddt=datetime(ld_fy_end_date,time('00:00:00'))
dw_rpt_all_col.Retrieve(ld_fystdt,ld_fyenddt )
dw_rpt_init_3_columns_open.SetTransObject(SqlServerTrans)

select count(*) into :li_count
from monrpt
where fy= :li_fy 
using SqlServerTrans;
for j =1 to 12
	choose case j
		case 1
			ld_start_date= date('10/01/'+ls_pre_fy)
			ld_end_date= date('10/31/'+ ls_pre_fy )
		case 2
			ld_start_date= date('11/01/'+ls_pre_fy)
			ld_end_date= date('11/30/'+ ls_pre_fy )
		case 3
			ld_start_date= date('12/01/'+ls_pre_fy)
			ld_end_date= date('12/31/'+ ls_pre_fy )
		case 4
			ld_start_date= date('01/01/'+ls_fy)
			ld_end_date= date('01/31/'+ ls_fy )
		case 5
			ld_start_date= date('02/01/'+ls_fy)
			if mod(li_fy, 4)=0 then
				ld_end_date= date('02/29/'+ ls_fy )
			else
				ld_end_date= date('02/28/'+ ls_fy )
			end if
		case 6
			ld_start_date= date('03/01/'+ls_fy)
			ld_end_date= date('03/31/'+ ls_fy )
		case 7
			ld_start_date= date('04/01/'+ls_fy)
			ld_end_date= date('04/30/'+ ls_fy )
		case 8
			ld_start_date= date('05/01/'+ls_fy)
			ld_end_date= date('05/31/'+ ls_fy )
		case 9
			ld_start_date= date('06/01/'+ls_fy)
			ld_end_date= date('06/30/'+ ls_fy )
		case 10
			ld_start_date= date('07/01/'+ls_fy)
			ld_end_date= date('07/31/'+ ls_fy )
		case 11
			ld_start_date= date('08/01/'+ls_fy)
			ld_end_date= date('08/31/'+ ls_fy )
		case 12
			ld_start_date= date('09/01/'+ls_fy)
			ld_end_date= date('09/30/'+ ls_fy )
	end choose
	ld_before_start30 = RelativeDate( ld_start_date, - 30 )
	ld_stdt=datetime(ld_start_date,time('00:00:00'))
	ld_enddt=datetime(ld_end_date,time('00:00:00'))
	ld_bef_30dt=datetime(ld_before_start30,time('00:00:00'))
	li_row_count =dw_rpt_init_3_columns_open.Retrieve(ld_bef_30dt)
	//retrieve active prdr, cntrmed, cntrlc, cntr at before 30 days of current date ld_start_date, pay attention
	// ld_start_date therefore ld_bef30dt will change values for each month in fiscal year
	for i=1 to li_row_count
		ls_prdr= dw_rpt_init_3_columns_open.GetItemString(i,'prdr')
		ls_med= dw_rpt_init_3_columns_open.GetItemString(i,'cntrmed')
		ls_cntr= dw_rpt_init_3_columns_open.GetItemString(i,'cntr')
		ls_prdr=trim(ls_prdr)
		ls_med=trim(ls_med)
		ls_cntr=trim(ls_cntr)
		ls_cntrlc= dw_rpt_init_3_columns_open.GetItemString(i,'cntrlc')
		ls_lang= dw_rpt_init_3_columns_open.GetItemString(i,'foreign_lang')
		if IsNull(ls_lang) then ls_lang='N'
		// if table monrpt no data at all for this fiscal year then insert every retrieved combinations prdr, cntrmed, cntr,
		// and each correspondent ld_start_date, ld_end_date. Pay attlention the combinations will change with ld_start_date,
		//ld_end_date changing. For some month have this prdr, med but for next month may be there is no such prdr, med.
		// The setting data means set to 0, only set not null values for primary key for each month in table monrpt.
		if li_count =0 then
			li_cur=dw_rpt_all_col.InsertRow(0)
			dw_rpt_all_col.SetItem(li_cur,'prdr',ls_prdr)
			dw_rpt_all_col.SetItem(li_cur,'cntrmed',ls_med)
			dw_rpt_all_col.SetItem(li_cur,'cntr',ls_cntr)
			dw_rpt_all_col.SetItem(li_cur,'cntrlc',ls_cntrlc)
			dw_rpt_all_col.SetItem(li_cur,'foreign_lang',ls_lang)
			dw_rpt_all_col.SetItem(li_cur,'start_date',ld_stdt)
			dw_rpt_all_col.SetItem(li_cur,'end_date',ld_enddt)
			dw_rpt_all_col.SetItem(li_cur,'awadtitles', 0)
			dw_rpt_all_col.SetItem(li_cur,'asgntitles', 0)
			dw_rpt_all_col.SetItem(li_cur,'shiptitles', 0)
			dw_rpt_all_col.SetItem(li_cur,'latetottitles',0)
			dw_rpt_all_col.SetItem(li_cur,'planship',0)
			dw_rpt_all_col.SetItem(li_cur,'actship',0)
			dw_rpt_all_col.SetItem(li_cur,'regship',0)
			dw_rpt_all_col.SetItem(li_cur,'lateship',0)
			dw_rpt_all_col.SetItem(li_cur,'qc1apprv',0)
			dw_rpt_all_col.SetItem(li_cur,'qc2apprv',0)
			dw_rpt_all_col.SetItem(li_cur,'qc3apprv',0)
			dw_rpt_all_col.SetItem(li_cur,'qc1reject',0)
			dw_rpt_all_col.SetItem(li_cur,'qc2reject',0)
			dw_rpt_all_col.SetItem(li_cur,'qc3reject',0)
			dw_rpt_all_col.SetItem(li_cur,'ctdqc1apprv',0)
			dw_rpt_all_col.SetItem(li_cur,'ctdqc2apprv',0)
			dw_rpt_all_col.SetItem(li_cur,'ctdqc3apprv',0)
			dw_rpt_all_col.SetItem(li_cur,'ctdqc1reject',0)
			dw_rpt_all_col.SetItem(li_cur,'ctdqc2reject',0)
			dw_rpt_all_col.SetItem(li_cur,'ctdqc3reject',0)
			dw_rpt_all_col.SetItem(li_cur,'ctdqc3reject',0)
			//
			dw_rpt_all_col.SetItem(li_cur,'fy',li_fy)
			dw_rpt_all_col.SetItem(li_cur,'latepct',0)
			dw_rpt_all_col.SetItem(li_cur,'rejpct1',0)
			dw_rpt_all_col.SetItem(li_cur,'rejpct2',0)
			dw_rpt_all_col.SetItem(li_cur,'rejpct3',0)
			dw_rpt_all_col.SetItem(li_cur,'done','N')
			dw_rpt_all_col.SetItem(li_cur,'qastat','Y')
			dw_rpt_all_col.SetItem(li_cur,'prdstat','Y')
		else//if li_count<>0 means fiscal year have some data for some prdr, cntrmed, cntr but not sure for this prdr,med, cntr
		//then check for this ls_prdr, ls_med, ls_cntr for current ld_start_date have data or not
			select count(*) into :li_count2
			from monrpt
			where start_date=:ld_stdt and end_date=:ld_enddt and
				prdr=:ls_prdr and cntrmed=:ls_med and cntr=:ls_cntr
			using SqlServerTrans;
			if not f_check_dberror(SqlServerTrans,'select from monrpt find count2,ls_cntrlc') then
				return -1
			end if
			//if no data for this specic ls_prdr, ls_med, ls_cntr, ld_start_date then insert into monrpt table, actual
			// data still set 0
			if li_count2=0 then
				li_cur=dw_rpt_all_col.InsertRow(0)
				dw_rpt_all_col.SetItem(li_cur,'prdr',ls_prdr)
				dw_rpt_all_col.SetItem(li_cur,'cntrmed',ls_med)
				dw_rpt_all_col.SetItem(li_cur,'cntr',ls_cntr)
				dw_rpt_all_col.SetItem(li_cur,'cntrlc',ls_cntrlc)
				dw_rpt_all_col.SetItem(li_cur,'foreign_lang',ls_lang)
				dw_rpt_all_col.SetItem(li_cur,'start_date',ld_stdt)
				dw_rpt_all_col.SetItem(li_cur,'end_date',ld_enddt)
				dw_rpt_all_col.SetItem(li_cur,'awadtitles', 0)
				dw_rpt_all_col.SetItem(li_cur,'asgntitles', 0)
				dw_rpt_all_col.SetItem(li_cur,'shiptitles', 0)
				dw_rpt_all_col.SetItem(li_cur,'latetottitles',0)
				dw_rpt_all_col.SetItem(li_cur,'planship',0)
				dw_rpt_all_col.SetItem(li_cur,'actship',0)
				dw_rpt_all_col.SetItem(li_cur,'regship',0)
				dw_rpt_all_col.SetItem(li_cur,'lateship',0)
				dw_rpt_all_col.SetItem(li_cur,'qc1apprv',0)
				dw_rpt_all_col.SetItem(li_cur,'qc2apprv',0)
				dw_rpt_all_col.SetItem(li_cur,'qc3apprv',0)
				dw_rpt_all_col.SetItem(li_cur,'qc1reject',0)
				dw_rpt_all_col.SetItem(li_cur,'qc2reject',0)
				dw_rpt_all_col.SetItem(li_cur,'qc3reject',0)
				dw_rpt_all_col.SetItem(li_cur,'ctdqc1apprv',0)
				dw_rpt_all_col.SetItem(li_cur,'ctdqc2apprv',0)
				dw_rpt_all_col.SetItem(li_cur,'ctdqc3apprv',0)
				dw_rpt_all_col.SetItem(li_cur,'ctdqc1reject',0)
				dw_rpt_all_col.SetItem(li_cur,'ctdqc2reject',0)
				dw_rpt_all_col.SetItem(li_cur,'ctdqc3reject',0)
				dw_rpt_all_col.SetItem(li_cur,'ctdqc3reject',0)
				//
				dw_rpt_all_col.SetItem(li_cur,'fy',li_fy)
				dw_rpt_all_col.SetItem(li_cur,'latepct',0)
				dw_rpt_all_col.SetItem(li_cur,'rejpct1',0)
				dw_rpt_all_col.SetItem(li_cur,'rejpct2',0)
				dw_rpt_all_col.SetItem(li_cur,'rejpct3',0)
				dw_rpt_all_col.SetItem(li_cur,'done','N')
				dw_rpt_all_col.SetItem(li_cur,'qastat','Y')
			dw_rpt_all_col.SetItem(li_cur,'prdstat','Y')
			end if// end if li_count2=0, no this prdr,cntrmed, cntrlc for cur_start_date
		end if// li_count<>0 no fy here
	next // for i=1 to li_row_count
next //for j=1 to 12
//this parts is for dubouble checking if some row is not in dw_rpt_init_3_columns_open, this means the correspondent
//combination should be deleted from dw_rpt_all_col
for j =1 to 12
	choose case j
		case 1
			ld_start_date= date('10/01/'+ls_pre_fy)
			ld_end_date= date('10/31/'+ ls_pre_fy )
		case 2
			ld_start_date= date('11/01/'+ls_pre_fy)
			ld_end_date= date('11/30/'+ ls_pre_fy )
		case 3
			ld_start_date= date('12/01/'+ls_pre_fy)
			ld_end_date= date('12/31/'+ ls_pre_fy )
		case 4
			ld_start_date= date('01/01/'+ls_fy)
			ld_end_date= date('01/31/'+ ls_fy )
		case 5
			ld_start_date= date('02/01/'+ls_fy)
			if mod(li_fy, 4)=0 then
				ld_end_date= date('02/29/'+ ls_fy )
			else
				ld_end_date= date('02/28/'+ ls_fy )
			end if
		case 6
			ld_start_date= date('03/01/'+ls_fy)
			ld_end_date= date('03/31/'+ ls_fy )
		case 7
			ld_start_date= date('04/01/'+ls_fy)
			ld_end_date= date('04/30/'+ ls_fy )
		case 8
			ld_start_date= date('05/01/'+ls_fy)
			ld_end_date= date('05/31/'+ ls_fy )
		case 9
			ld_start_date= date('06/01/'+ls_fy)
			ld_end_date= date('06/30/'+ ls_fy )
		case 10
			ld_start_date= date('07/01/'+ls_fy)
			ld_end_date= date('07/31/'+ ls_fy )
		case 11
			ld_start_date= date('08/01/'+ls_fy)
			ld_end_date= date('08/31/'+ ls_fy )
		case 12
			ld_start_date= date('09/01/'+ls_fy)
			ld_end_date= date('09/30/'+ ls_fy )
	end choose
	
	ls_start_date=string(ld_start_date,'mm/dd/yyyy')
	ls_end_date =string(ld_end_date,'mm/dd/yyyy')
//	ls_filter="prdr = '"+ls_prdr+"'"+" and cntrmed= '"+ls_med+"'"+" and "
	ls_filter="date(start_date) =date('"+ls_start_date+"')"+&
												" and date(end_date)= date('"+ls_end_date+"')"
	dw_rpt_all_col.SetFilter(ls_filter)
	dw_rpt_all_col.Filter()
	dw_rpt_all_col.Sort()
	li_row_count =dw_rpt_all_col.RowCount()
	
	ld_before_start30 = RelativeDate( ld_start_date, - 30 )
	ld_bef_30dt=datetime(ld_before_start30,time('00:00:00'))
	li_count =dw_rpt_init_3_columns_open.Retrieve(ld_bef_30dt)
	//from last row to check each row coincide if not coincidence then delete that row
	for i=1 to li_row_count
		ls_prdr= dw_rpt_all_col.GetItemString(li_row_count+1 -  i,'prdr')
		ls_med= dw_rpt_all_col.GetItemString(li_row_count+1 -  i,'cntrmed')
		ls_cntr= dw_rpt_all_col.GetItemString(li_row_count+1 -  i,'cntr')
		ls_prdr=trim(ls_prdr)
		ls_med=trim(ls_med)
		ls_cntr=trim(ls_cntr)
		lb_find=false
		for k=1 to li_count
			ls_prdr2= dw_rpt_init_3_columns_open.GetItemString(li_count+1 - k,'prdr')
			ls_med2= dw_rpt_init_3_columns_open.GetItemString(li_count+1 - k,'cntrmed')
			ls_cntr2= dw_rpt_init_3_columns_open.GetItemString(li_count+1 - k,'cntr')
			ls_prdr2=trim(ls_prdr2)
			ls_med2=trim(ls_med2)
			ls_cntr2=trim(ls_cntr2)
			if ls_prdr2=ls_prdr and ls_med2=ls_med and ls_cntr2=ls_cntr then
				lb_find=true
				exit
			end if
			if ls_prdr2<ls_prdr and lb_find= false then
				dw_rpt_all_col.DeleteRow(li_row_count+1 -  i)
				li_row_count -= 1
				exit
			end if
		next	//for k dw_ini_3_col
	next// for i dw_rpt_all_col
next// for j=1 to 12
ls_filter=''
dw_rpt_all_col.SetFilter(ls_filter)
dw_rpt_all_col.Filter()
li_row_count =dw_rpt_all_col.RowCount()
dw_rpt_all_col.sort()

li_re =dw_rpt_all_col.update()
if li_re =1 then
	commit using SqlServerTrans;
	return 1
else
	RollBack using SqlServerTrans;
	return -1
end if


end function

public function integer wf_rpt_ini_all_col_1_prdr_1_year0 ();//if fiscal year fy is new one then insert all column data for the correspondent month. Since ls_prdr, ls_med, ls_cntr 
//combination dependent upon ld_start_date -30, for each month must update this combination
//if fiscal year aready established. The job only refresh the data of this one month
// to save the change must commit first

date ld_start_date, ld_end_date,ld_fy_start_date, ld_fy_end_date, ld_before_start30	
long li_row_count, i, li_re ,li_count2,j ,k , li_count, li_cur, li_fy	
string  ls_med, ls_prdr, ls_cntr, ls_cntrlc, ls_fy,ls_filter,ls_pre_fy, ls_lang, ls_start_date, ls_end_date, &
		ls_fy_start_date, ls_fy_end_date
		
str_voucher_report lstr
datetime ld_stdt, ld_enddt, ld_bef_30dt


lstr =istr
ls_prdr =lstr.array[3]
ls_med = lstr.array[13]

ls_fy_start_date = lstr.array[10]
ls_fy_end_date = (lstr.array[11])

ls_pre_fy= right(ls_fy_start_date, 4)
ls_fy= right(ls_fy_end_date, 4)
li_fy =integer( ls_fy )
dw_rpt_all_col.SetTransObject(SqlServerTrans)
dw_rpt_init_3_columns_open.SetTransObject(SqlServerTrans)
if ls_prdr<>'PTB' THEN
	select count(*) into :li_count
	from monrpt
	where fy= :li_fy and prdr=:ls_prdr and cntrmed=:ls_med
	using SqlServerTrans;
elseif ls_prdr='PTB' THEN
	if ls_med='FL' THEN
		ls_med='RC'
		ls_lang='Y'
	elseif (ls_med='RC' OR ls_med='RTB') THEN
		ls_lang='N'
	end if
	select count(*) into :li_count
	from monrpt
	where fy= :li_fy and prdr=:ls_prdr and cntrmed=:ls_med and foreign_lang=:ls_lang
	using SqlServerTrans;
end if
if not f_check_dberror(SqlServerTrans,'select count from monrpt ') then
	return -1
end if

for j =1 to 12	
	choose case j
		case 1
			ld_start_date= date('10/01/'+ls_pre_fy)
			ld_end_date= date('10/31/'+ ls_pre_fy )
		case 2
			ld_start_date= date('11/01/'+ls_pre_fy)
			ld_end_date= date('11/30/'+ ls_pre_fy )
		case 3
			ld_start_date= date('12/01/'+ls_pre_fy)
			ld_end_date= date('12/31/'+ ls_pre_fy )
		case 4
			ld_start_date= date('01/01/'+ls_fy)
			ld_end_date= date('01/31/'+ ls_fy )
		case 5
			ld_start_date= date('02/01/'+ls_fy)
			if mod(li_fy, 4)=0 then
				ld_end_date= date('02/29/'+ ls_fy )
			else
				ld_end_date= date('02/28/'+ ls_fy )
			end if
		case 6
			ld_start_date= date('03/01/'+ls_fy)
			ld_end_date= date('03/31/'+ ls_fy )
		case 7
			ld_start_date= date('04/01/'+ls_fy)
			ld_end_date= date('04/30/'+ ls_fy )
		case 8
			ld_start_date= date('05/01/'+ls_fy)
			ld_end_date= date('05/31/'+ ls_fy )
		case 9
			ld_start_date= date('06/01/'+ls_fy)
			ld_end_date= date('06/30/'+ ls_fy )
		case 10
			ld_start_date= date('07/01/'+ls_fy)
			ld_end_date= date('07/31/'+ ls_fy )
		case 11
			ld_start_date= date('08/01/'+ls_fy)
			ld_end_date= date('08/31/'+ ls_fy )
		case 12
			ld_start_date= date('09/01/'+ls_fy)
			ld_end_date= date('09/30/'+ ls_fy )
	end choose
	ld_stdt=datetime(ld_start_date,time('00:00:00'))
	ld_enddt=datetime(ld_end_date,time('00:00:00'))
	
	ld_before_start30 = RelativeDate( ld_start_date, - 30 )
	ld_bef_30dt=datetime(ld_before_start30,time('00:00:00'))
	//this will retrieve current month ld_start_date -30 all prdr, med, cntr combinations, after filter one get
	//current ls_prdr, ls_med, ls_cntr list
	dw_rpt_init_3_columns_open.Retrieve(ld_bef_30dt)
	ls_filter="prdr= '"+ls_prdr+"'"+" and cntrmed= '"+ls_med+"'"+&
													" and foreign_lang='"+ls_lang+"'"
	dw_rpt_init_3_columns_open.SetFilter(ls_filter)
	dw_rpt_init_3_columns_open.Filter()
	dw_rpt_init_3_columns_open.Sort()
	li_row_count=dw_rpt_init_3_columns_open.RowCount()
	for i=1 to li_row_count
		ls_cntr= dw_rpt_init_3_columns_open.GetItemString(i,'cntr')
		ls_cntrlc= dw_rpt_init_3_columns_open.GetItemString(i,'cntrlc')
		ls_lang= dw_rpt_init_3_columns_open.GetItemString(i,'foreign_lang')
		if IsNull(ls_lang) then ls_lang='N'
		if li_count=0 then// this means never initiated for this whole fiscal year
			li_cur=dw_rpt_all_col.InsertRow(0)
			//insert into monrpt for this ls_prdr, ls_med, ls_cntr combinations and ld_start_date
			dw_rpt_all_col.SetItem(li_cur,'prdr',ls_prdr)
			dw_rpt_all_col.SetItem(li_cur,'cntrmed',ls_med)
			dw_rpt_all_col.SetItem(li_cur,'cntr',ls_cntr)
			dw_rpt_all_col.SetItem(li_cur,'cntrlc',ls_cntrlc)
			dw_rpt_all_col.SetItem(li_cur,'foreign_lang',ls_lang)
			dw_rpt_all_col.SetItem(li_cur,'start_date',ld_stdt)
			dw_rpt_all_col.SetItem(li_cur,'end_date',ld_enddt)
			dw_rpt_all_col.SetItem(li_cur,'awadtitles', 0)
			dw_rpt_all_col.SetItem(li_cur,'asgntitles', 0)
			dw_rpt_all_col.SetItem(li_cur,'shiptitles', 0)
			dw_rpt_all_col.SetItem(li_cur,'latetottitles',0)
			dw_rpt_all_col.SetItem(li_cur,'planship',0)
			dw_rpt_all_col.SetItem(li_cur,'actship',0)
			dw_rpt_all_col.SetItem(li_cur,'regship',0)
			dw_rpt_all_col.SetItem(li_cur,'lateship',0)
			dw_rpt_all_col.SetItem(li_cur,'qc1apprv',0)
			dw_rpt_all_col.SetItem(li_cur,'qc2apprv',0)
			dw_rpt_all_col.SetItem(li_cur,'qc3apprv',0)
			dw_rpt_all_col.SetItem(li_cur,'qc1reject',0)
			dw_rpt_all_col.SetItem(li_cur,'qc2reject',0)
			dw_rpt_all_col.SetItem(li_cur,'qc3reject',0)
			dw_rpt_all_col.SetItem(li_cur,'ctdqc1apprv',0)
			dw_rpt_all_col.SetItem(li_cur,'ctdqc2apprv',0)
			dw_rpt_all_col.SetItem(li_cur,'ctdqc3apprv',0)
			dw_rpt_all_col.SetItem(li_cur,'ctdqc1reject',0)
			dw_rpt_all_col.SetItem(li_cur,'ctdqc2reject',0)
			dw_rpt_all_col.SetItem(li_cur,'ctdqc3reject',0)
			dw_rpt_all_col.SetItem(li_cur,'ctdqc3reject',0)
			//
			dw_rpt_all_col.SetItem(li_cur,'fy',li_fy)
			dw_rpt_all_col.SetItem(li_cur,'latepct',0)
			dw_rpt_all_col.SetItem(li_cur,'rejpct1',0)
			dw_rpt_all_col.SetItem(li_cur,'rejpct2',0)
			dw_rpt_all_col.SetItem(li_cur,'rejpct3',0)
			dw_rpt_all_col.SetItem(li_cur,'done','N')
			dw_rpt_all_col.SetItem(li_cur,'qastat','Y')
			dw_rpt_all_col.SetItem(li_cur,'prdstat','Y')
		else// if for some ls_prdr, ls_med, ls_cntr have data for some month then check for each ld_start_date 
			//have data or not
			select count(*) into :li_count2
			from monrpt
			where start_date=:ld_stdt and end_date=:ld_enddt and
				prdr=:ls_prdr and cntrmed=:ls_med and cntr=:ls_cntr
			using SqlServerTrans;
			if not f_check_dberror(SqlServerTrans,'select count2 from monrpt ') then
				return -1
			end if
			if li_count2=0 then// this means ls_prdr, ls_med, ls_cntr is new combination for ld_start_date should be initiated
				li_cur=dw_rpt_all_col.InsertRow(0)
				dw_rpt_all_col.SetItem(li_cur,'prdr',ls_prdr)
				dw_rpt_all_col.SetItem(li_cur,'cntrmed',ls_med)
				dw_rpt_all_col.SetItem(li_cur,'cntr',ls_cntr)
				dw_rpt_all_col.SetItem(li_cur,'cntrlc',ls_cntrlc)
				dw_rpt_all_col.SetItem(li_cur,'foreign_lang',ls_lang)
				dw_rpt_all_col.SetItem(li_cur,'start_date',ld_stdt)
				dw_rpt_all_col.SetItem(li_cur,'end_date',ld_enddt)
				dw_rpt_all_col.SetItem(li_cur,'awadtitles', 0)
				dw_rpt_all_col.SetItem(li_cur,'asgntitles', 0)
				dw_rpt_all_col.SetItem(li_cur,'shiptitles', 0)
				dw_rpt_all_col.SetItem(li_cur,'latetottitles',0)
				dw_rpt_all_col.SetItem(li_cur,'planship',0)
				dw_rpt_all_col.SetItem(li_cur,'actship',0)
				dw_rpt_all_col.SetItem(li_cur,'regship',0)
				dw_rpt_all_col.SetItem(li_cur,'lateship',0)
				dw_rpt_all_col.SetItem(li_cur,'qc1apprv',0)
				dw_rpt_all_col.SetItem(li_cur,'qc2apprv',0)
				dw_rpt_all_col.SetItem(li_cur,'qc3apprv',0)
				dw_rpt_all_col.SetItem(li_cur,'qc1reject',0)
				dw_rpt_all_col.SetItem(li_cur,'qc2reject',0)
				dw_rpt_all_col.SetItem(li_cur,'qc3reject',0)
				dw_rpt_all_col.SetItem(li_cur,'ctdqc1apprv',0)
				dw_rpt_all_col.SetItem(li_cur,'ctdqc2apprv',0)
				dw_rpt_all_col.SetItem(li_cur,'ctdqc3apprv',0)
				dw_rpt_all_col.SetItem(li_cur,'ctdqc1reject',0)
				dw_rpt_all_col.SetItem(li_cur,'ctdqc2reject',0)
				dw_rpt_all_col.SetItem(li_cur,'ctdqc3reject',0)
				dw_rpt_all_col.SetItem(li_cur,'ctdqc3reject',0)
				//
				dw_rpt_all_col.SetItem(li_cur,'fy',li_fy)
				dw_rpt_all_col.SetItem(li_cur,'latepct',0)
				dw_rpt_all_col.SetItem(li_cur,'rejpct1',0)
				dw_rpt_all_col.SetItem(li_cur,'rejpct2',0)
				dw_rpt_all_col.SetItem(li_cur,'rejpct3',0)
				dw_rpt_all_col.SetItem(li_cur,'done','N')
				dw_rpt_all_col.SetItem(li_cur,'qastat','Y')
			dw_rpt_all_col.SetItem(li_cur,'prdstat','Y')
			end if//li_count2=0
		end if//li_count=0
	next // for i=1 to li_row_count
next //for j=1 to 12

li_re =dw_rpt_all_col.update()
if li_re =1 then
	commit using SqlServerTrans;
	return 1
else
	RollBack using SqlServerTrans;
	return -1
end if



end function

on w_month_report.create
int iCurrent
call super::create
this.cb_printall=create cb_printall
this.cb_prev=create cb_prev
this.cb_next=create cb_next
this.dw_deviate_sum_sp_no_detail=create dw_deviate_sum_sp_no_detail
this.dw_rpt_cntr_cntrlc=create dw_rpt_cntr_cntrlc
this.dw_rpt_qc_apr_rej_show_total=create dw_rpt_qc_apr_rej_show_total
this.dw_rpt_detl_total_pct=create dw_rpt_detl_total_pct
this.dw_rpt_sum_cmpt_pct=create dw_rpt_sum_cmpt_pct
this.cb_print=create cb_print
this.cb_exit=create cb_exit
this.dw_rpt_fram_prod_qc_tbl_and_bar=create dw_rpt_fram_prod_qc_tbl_and_bar
this.dw_rpt_all_col=create dw_rpt_all_col
this.dw_rpt_qc_apr_rej_get=create dw_rpt_qc_apr_rej_get
this.dw_rpt_sum_prod_plan2=create dw_rpt_sum_prod_plan2
this.dw_comp_init_3_columns=create dw_comp_init_3_columns
this.dw_pcdeviaten_ace_report=create dw_pcdeviaten_ace_report
this.dw_rpt_open_cmp_tottitles=create dw_rpt_open_cmp_tottitles
this.dw_rpt_open_cmp_shiptitles=create dw_rpt_open_cmp_shiptitles
this.dw_rpt_open_cmp_latetitles=create dw_rpt_open_cmp_latetitles
this.dw_rpt_open_cmp_asigntitles=create dw_rpt_open_cmp_asigntitles
this.dw_rpt_sum_prod_act_ship=create dw_rpt_sum_prod_act_ship
this.dw_rpt_sum_prod_reg_ship=create dw_rpt_sum_prod_reg_ship
this.dw_rpt_init_3_columns_open=create dw_rpt_init_3_columns_open
this.cb_option=create cb_option
this.dw_rpt_sum_prod_late_ship=create dw_rpt_sum_prod_late_ship
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.cb_printall
this.Control[iCurrent+2]=this.cb_prev
this.Control[iCurrent+3]=this.cb_next
this.Control[iCurrent+4]=this.dw_deviate_sum_sp_no_detail
this.Control[iCurrent+5]=this.dw_rpt_cntr_cntrlc
this.Control[iCurrent+6]=this.dw_rpt_qc_apr_rej_show_total
this.Control[iCurrent+7]=this.dw_rpt_detl_total_pct
this.Control[iCurrent+8]=this.dw_rpt_sum_cmpt_pct
this.Control[iCurrent+9]=this.cb_print
this.Control[iCurrent+10]=this.cb_exit
this.Control[iCurrent+11]=this.dw_rpt_fram_prod_qc_tbl_and_bar
this.Control[iCurrent+12]=this.dw_rpt_all_col
this.Control[iCurrent+13]=this.dw_rpt_qc_apr_rej_get
this.Control[iCurrent+14]=this.dw_rpt_sum_prod_plan2
this.Control[iCurrent+15]=this.dw_comp_init_3_columns
this.Control[iCurrent+16]=this.dw_pcdeviaten_ace_report
this.Control[iCurrent+17]=this.dw_rpt_open_cmp_tottitles
this.Control[iCurrent+18]=this.dw_rpt_open_cmp_shiptitles
this.Control[iCurrent+19]=this.dw_rpt_open_cmp_latetitles
this.Control[iCurrent+20]=this.dw_rpt_open_cmp_asigntitles
this.Control[iCurrent+21]=this.dw_rpt_sum_prod_act_ship
this.Control[iCurrent+22]=this.dw_rpt_sum_prod_reg_ship
this.Control[iCurrent+23]=this.dw_rpt_init_3_columns_open
this.Control[iCurrent+24]=this.cb_option
this.Control[iCurrent+25]=this.dw_rpt_sum_prod_late_ship
end on

on w_month_report.destroy
call super::destroy
destroy(this.cb_printall)
destroy(this.cb_prev)
destroy(this.cb_next)
destroy(this.dw_deviate_sum_sp_no_detail)
destroy(this.dw_rpt_cntr_cntrlc)
destroy(this.dw_rpt_qc_apr_rej_show_total)
destroy(this.dw_rpt_detl_total_pct)
destroy(this.dw_rpt_sum_cmpt_pct)
destroy(this.cb_print)
destroy(this.cb_exit)
destroy(this.dw_rpt_fram_prod_qc_tbl_and_bar)
destroy(this.dw_rpt_all_col)
destroy(this.dw_rpt_qc_apr_rej_get)
destroy(this.dw_rpt_sum_prod_plan2)
destroy(this.dw_comp_init_3_columns)
destroy(this.dw_pcdeviaten_ace_report)
destroy(this.dw_rpt_open_cmp_tottitles)
destroy(this.dw_rpt_open_cmp_shiptitles)
destroy(this.dw_rpt_open_cmp_latetitles)
destroy(this.dw_rpt_open_cmp_asigntitles)
destroy(this.dw_rpt_sum_prod_act_ship)
destroy(this.dw_rpt_sum_prod_reg_ship)
destroy(this.dw_rpt_init_3_columns_open)
destroy(this.cb_option)
destroy(this.dw_rpt_sum_prod_late_ship)
end on

event resize;call super::resize;Long ll_height

this.X = w_pics_main.X
this.Y = w_pics_main.Y
//ll_height = w_pics_main.mdi_1.Height
ll_height = w_pics_main.height
this.Resize(w_pics_main.WorkSpaceWidth(), ll_height)

end event

event pfc_preopen;call super::pfc_preopen;this.windowState =maximized!
this.of_SetBase(TRUE)
this.inv_base.of_Center()
this.title='Producer Monthly Report'
this.of_SetPreference(TRUE)
this.inv_preference.of_SetToolBars(TRUE)
this.inv_preference.of_SetWindow(TRUE)
this.of_SetResize(TRUE) 


inv_resize.of_Register(dw_rpt_fram_prod_qc_tbl_and_bar, "Scale")
//inv_resize.of_Register(dw_rpt_fram_prod_qc_tbl_and_barhave, "Scale")
inv_resize.of_Register(dw_rpt_sum_prod_plan2, "Scale")

inv_resize.of_Register(cb_exit, "Scale")
inv_resize.of_Register(cb_print, "Scale")
inv_resize.of_Register(cb_option, "Scale")
inv_resize.of_Register(cb_next, "Scale")
inv_resize.of_Register(cb_prev, "Scale")
inv_resize.of_Register(cb_printall, "Scale")




end event

event pfc_postopen;Date ld_start_date, ld_end_date, &
		ld_fy_start_date, ld_fy_end_date,ld_before_start30,ld_orig
Long li_re,li_fy,li_count, i, li_code
			
String  ls_med, ls_prdr, ls_cntrlc,ls_print,ls_cntrmed,ls_oneprdr,ls_max,ls_min, ls_lang, ls_back, &
		ls_start_date, ls_end_date, ls_fy_start_date, ls_fy_end_date, ls_pre_fy, ls_start_date_left,&
		 ls_yr, ls_fy, ls_month, ls_regen	
DateTime ldt_start_date
str_voucher_report lstr


Open(w_response_monthly_reports)
SetPointer(hourglass!)
this.SetRedraw(FALSE)
IF IsNull(message.powerObjectParm) THEN
	RETURN
END IF
istr =message.powerObjectParm
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
//dw_rpt_fram_prod_qc_tbl_and_barhave.visible=false
dw_rpt_fram_prod_qc_tbl_and_bar.visible=TRUE
IF ls_back='Y' THEN
//	dw_rpt_fram_prod_qc_tbl_and_barhave.visible=true
//	dw_rpt_fram_prod_qc_tbl_and_bar.visible=false
END IF
ld_start_date= Date(ls_start_date)
ldt_start_date=DateTime(ld_start_date,Time('00:00:00'))
ld_end_date =Date(ls_end_date)
ld_before_start30 = RelativeDate(ld_start_date, - 30)
ld_fy_start_date =Date(ls_fy_start_date)
ld_fy_end_date =Date(ls_fy_end_date)
ls_pre_fy= Right(ls_fy_start_date, 4)
ls_fy= Right(ls_fy_end_date, 4)
li_fy =Integer(ls_fy)
IF lstr.array[7] ='cancel' THEN
	this.SetRedraw(TRUE)
	Close(w_month_report)
	m_pics_main.m_menu.PopMenu(300, 0)
	RETURN 
END IF //if ok
ls_regen = lstr.array[4]
IF ls_back='Y' THEN
	// here handle report for last 12 months, close this window, then open another window specially
	// handle report for last 12 months, this event will end, start another window w_month_reporthave application
	Close(this)
	OpenSheetWithParm(w_month_reporthave,lstr,w_pics_main, 0, original!)
	RETURN
END IF
 // here handle only one specific producer for one fiscal year not for last 12 months
IF ls_oneprdr='Y' THEN
	cb_print.enabled=TRUE
	cb_printall.enabled=FALSE
	cb_next.enabled=FALSE
	cb_prev.enabled=FALSE
	i_1_prdr=1
	IF ls_cntrmed='FL' AND ls_prdr='PTB' THEN
		ls_cntrmed='RC'
		ls_lang='Y'
	ELSEIF ls_cntrmed='RC' AND ls_prdr='PTB' THEN
		ls_cntrmed='RC'
		ls_lang='N'
	ELSEIF ls_cntrmed='RTB' AND ls_prdr='PTB' THEN
		ls_cntrmed='RTB'
		ls_lang='N'
	END IF
	IF ls_regen='N' THEN
		//if not regenerate first check the where have data in database for specific prdr and cntrmed, if have then go 
		//head otherwise must generate
      IF ls_prdr<>'PTB' THEN
			SELECT Max(done),Min(done) INTO :ls_max,:ls_min
			FROM monrpt
			WHERE (start_date) >=:ld_fy_start_date AND (end_date)<=:ld_end_date AND prdr=:ls_prdr AND
					cntrmed=:ls_cntrmed
			USING sqlservertrans;
		ELSEIF ls_prdr='PTB' THEN// since prdr 'PTB' have two different languages, must user ls_lang
			SELECT Max(done),Min(done) INTO :ls_max,:ls_min
			FROM monrpt
			WHERE (start_date) >=:ld_fy_start_date AND (end_date)<=:ld_end_date AND prdr=:ls_prdr AND
					cntrmed=:ls_cntrmed AND foreign_lang=:ls_lang
			USING sqlservertrans;
		END IF
		IF NOT f_check_dberror(sqlservertrans,'select from monrpt to find max,min of done value') THEN
			RETURN
		END IF
	
		IF ls_max='Y' AND ls_min='Y' THEN // if have data then just retrieve
			OpenWithParm(w_pics_rpt_msg_box,'Retrieving data...')
		ELSE// otherwise must generate the new data
			li_re=wf_create_all_col_1_prdr_uptonow()
			IF li_re= -1 THEN
				RETURN
			END IF
		END IF
		// now you must have data, then diplay it
		li_re=wf_rpt_text_obj_total_pct_1_prdr()
		IF li_re= -1 THEN
			RETURN
		END IF
		
	ELSE //if ls_regen='Y' then must generate the data regardless whether have data or not
		OpenWithParm(w_pics_rpt_msg_box,'')
		w_pics_rpt_msg_box.sle_retrieve.text=&
		'Generating data for Producer/Media: '+ls_prdr+'/'+ls_cntrmed+' for '+ls_month+', '+ls_yr
		li_re=wf_create_all_col_1_prdr_uptonow()
		IF li_re= -1 THEN
			RETURN
		END IF
		li_re=wf_rpt_text_obj_total_pct_1_prdr()
		IF li_re= -1 THEN
			RETURN
		END IF
	END IF
ELSEIF ls_oneprdr='N' THEN //in the following for all user not just for only one specific check the data in the
	//period from start of fiscal year untill one choosed date, the data is ready or not
	cb_print.enabled=TRUE
	cb_printall.enabled=TRUE
	cb_next.enabled=TRUE
	cb_prev.enabled=TRUE
	cb_prev.enabled=FALSE
	OpenWithParm(w_pics_rpt_msg_box, 'Checking existing data. Please wait....')		
	SELECT Max(done),Min(done) INTO :ls_max,:ls_min
	FROM monrpt
	WHERE (start_date) >=:ld_fy_start_date AND (end_date)<=:ld_end_date 		
	USING sqlservertrans;
	IF NOT f_check_dberror(sqlservertrans,'select from monrpt to find max,min of done value') THEN
		RETURN
	END IF
	IF ls_max='Y' AND ls_min='Y' THEN // if data is ready ask regenerate or not
		li_re=Messagebox('','Data for '+ls_month+', '+ls_yr+' '+'has already been '+&
			'~generated. Please note regeneration of data can take several minutes. '+&
			'~Do you wish to regenerate this data? ',question!,yesNo!,2)
		IF li_re= 1 THEN // if regenerate then update control column done to 'N'( not ) then recreate data
			UPDATE monrpt
			SET done='N'
			WHERE start_date<=:ldt_start_date AND fy=:li_fy
			USING sqlservertrans;
			li_code=sqlservertrans.sqlCode
			IF NOT f_check_dberror(sqlservertrans,'update to reset done to N') THEN
				RETURN
			ELSE
				COMMIT USING sqlservertrans;
			END IF
			li_re=wf_create_all_col_all_prdr_uptonow()
			IF li_re= -1 THEN
				RETURN
			END IF
		END IF
	ELSE//if (ls_max='Y' and ls_min='Y')=false then regenerate
		li_re=wf_create_all_col_all_prdr_uptonow()
		IF li_re= -1 THEN
			Close(w_pics_rpt_msg_box)
			this.SetRedraw(TRUE)
			RETURN
		END IF
	END IF//if ls_max='Y' and ls_min='Y'
	// after you get data redeady then show the first prdr by default, the first prdr is set at response window
	//at w_response_monthly_reports through structure istr =message.PowerObjectParm
	li_re=wf_rpt_text_obj_total_pct_1_prdr()
	IF li_re= -1 THEN
		RETURN
	END IF
END IF// if ls_oneprdr='Y'
Close(w_pics_rpt_msg_box)
this.SetRedraw(TRUE)
RETURN

end event

type cb_printall from commandbutton within w_month_report
event mousemove pbm_mousemove
string tag = "Exit the screen"
integer x = 613
integer y = 1544
integer width = 325
integer height = 108
integer taborder = 20
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "P&rint All"
end type

event mousemove;SetMicroHelp(w_pics_main,this.tag)
end event

event clicked;string ls_prdr, ls_med,ls_month, ls_start_date, ls_end_date,ls_fy_start_date,&
			ls_fy_end_date, ls_oneprdr,ls_fy,ls_pre_fy, ls_start_date_left,&
			ls_year,ls_yr, ls_print
int li_re	

str_voucher_report lstr


ls_print='Y'
// save need print info to instant variable structure istr.array[5]
istr.array[5]=ls_print

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

if ls_start_date_left >='10' then
	ls_year= ls_pre_fy
else
	ls_year= ls_fy
end if
open(w_pics_rpt_msg_box)
if ls_oneprdr='Y' then

else // if all producer not just one then call function to show all producer and median

	li_re=wf_rpt_text_obj_total_pct_all_prdr()
	if li_re= - 1 then
		messagebox('','Error in create text object items for all prdr')
		return
	end if
end if
close(w_pics_rpt_msg_box)
end event

type cb_prev from commandbutton within w_month_report
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
ls_next_or_prev='prev'
//this is control to show which producer in the producer and median list, save this info to instant variable
//structure istr.array
istr.array[16]=ls_next_or_prev
if ls_oneprdr='N' then

	w_pics_rpt_msg_box.sle_retrieve.text=&
		'Retrieving data, Please wait...'
	//find correspondent producer and median in the producer and median list and show it
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

type cb_next from commandbutton within w_month_report
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
ls_next_or_prev='next' // this is control go to next producer of prdr list save to instant variable istr_array[16]
istr.array[16]=ls_next_or_prev
if ls_oneprdr='N' then

	w_pics_rpt_msg_box.sle_retrieve.text=&
		'Retieving data, Please wait...'	
	// find next producer and show it
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

type dw_deviate_sum_sp_no_detail from u_pics_dw within w_month_report
boolean visible = false
integer x = 407
integer y = 1580
integer width = 50
integer height = 36
integer taborder = 60
boolean bringtotop = true
string dataobject = "d_deviate_sum_sp_no_detail"
end type

type dw_rpt_cntr_cntrlc from u_pics_dw within w_month_report
boolean visible = false
integer x = 407
integer y = 1532
integer width = 50
integer height = 36
integer taborder = 50
boolean bringtotop = true
string dataobject = "d_rpt_cntr_cntrlc"
end type

type dw_rpt_qc_apr_rej_show_total from u_pics_dw within w_month_report
boolean visible = false
integer x = 1522
integer y = 1572
integer width = 50
integer height = 36
integer taborder = 30
boolean bringtotop = true
string dataobject = "d_rpt_qc_apr_rej_show"
end type

type dw_rpt_detl_total_pct from u_pics_dw within w_month_report
boolean visible = false
integer x = 462
integer y = 1612
integer width = 50
integer height = 36
integer taborder = 60
boolean bringtotop = true
string dataobject = "d_rpt_detl_total_pct"
end type

type dw_rpt_sum_cmpt_pct from u_pics_dw within w_month_report
boolean visible = false
integer x = 1586
integer y = 1620
integer width = 46
integer height = 40
integer taborder = 30
boolean bringtotop = true
string dataobject = "d_rpt_sum_cmpt_pct"
end type

type cb_print from commandbutton within w_month_report
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

if ls_start_date_left >='10' then
	ls_year= ls_pre_fy
else
	ls_year= ls_fy
end if

open(w_pics_rpt_msg_box)
if ls_oneprdr='Y' then

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

type cb_exit from commandbutton within w_month_report
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
dw_comp_init_3_columns.ResetUpdate()
dw_deviate_sum_sp_no_detail.ResetUpdate()
dw_pcdeviaten_ace_report.ResetUpdate()
dw_rpt_all_col.ResetUpdate()

dw_rpt_cntr_cntrlc.ResetUpdate()



dw_rpt_detl_total_pct.ResetUpdate()
dw_rpt_fram_prod_qc_tbl_and_bar.ResetUpdate()
//dw_rpt_init_2_columns_open.ResetUpdate()
dw_rpt_init_3_columns_open.ResetUpdate()

dw_rpt_open_cmp_asigntitles.ResetUpdate()
dw_rpt_open_cmp_latetitles.ResetUpdate()
dw_rpt_open_cmp_shiptitles.ResetUpdate()
dw_rpt_open_cmp_tottitles.ResetUpdate()
dw_rpt_qc_apr_rej_get.ResetUpdate()

dw_rpt_qc_apr_rej_show_total.ResetUpdate()


dw_rpt_sum_cmpt_pct.ResetUpdate()
dw_rpt_sum_prod_act_ship.ResetUpdate()

dw_rpt_sum_prod_late_ship.ResetUpdate()
dw_rpt_sum_prod_plan2.ResetUpdate()
dw_rpt_sum_prod_reg_ship.ResetUpdate()



parent.Event pfc_close()






end event

event mousemove;SetMicroHelp(w_pics_main,this.tag)
end event

type dw_rpt_fram_prod_qc_tbl_and_bar from u_pics_dw within w_month_report
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

type dw_rpt_all_col from u_pics_dw within w_month_report
integer x = 23
integer y = 12
integer width = 3401
integer height = 1460
integer taborder = 10
string dataobject = "d_rpt_all_col"
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

type dw_rpt_qc_apr_rej_get from u_pics_dw within w_month_report
integer x = 23
integer y = 8
integer width = 3406
integer height = 1464
integer taborder = 10
string dataobject = "d_rpt_qc_apr_rej_get"
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

type dw_rpt_sum_prod_plan2 from u_pics_dw within w_month_report
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

type dw_comp_init_3_columns from u_pics_dw within w_month_report
integer x = 1065
integer y = 1568
integer width = 50
integer height = 36
integer taborder = 20
boolean bringtotop = true
string dataobject = "d_comp_init_3_columns2"
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

type dw_pcdeviaten_ace_report from u_pics_dw within w_month_report
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

type dw_rpt_open_cmp_tottitles from u_pics_dw within w_month_report
boolean visible = false
integer x = 983
integer y = 1576
integer width = 50
integer height = 36
integer taborder = 10
boolean bringtotop = true
string dataobject = "d_rpt_open_cmp_tottitles"
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

type dw_rpt_open_cmp_shiptitles from u_pics_dw within w_month_report
boolean visible = false
integer x = 1042
integer y = 1628
integer width = 50
integer height = 36
integer taborder = 10
boolean bringtotop = true
string dataobject = "d_rpt_open_cmp_shiptitles"
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

type dw_rpt_open_cmp_latetitles from u_pics_dw within w_month_report
boolean visible = false
integer x = 978
integer y = 1620
integer width = 50
integer height = 36
integer taborder = 10
boolean bringtotop = true
string dataobject = "d_rpt_open_cmp_latetitles"
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

type dw_rpt_open_cmp_asigntitles from u_pics_dw within w_month_report
boolean visible = false
integer x = 1673
integer y = 1620
integer width = 50
integer height = 36
integer taborder = 10
boolean bringtotop = true
string dataobject = "d_rpt_open_cmp_asigntitles"
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

type dw_rpt_sum_prod_act_ship from u_pics_dw within w_month_report
boolean visible = false
integer x = 535
integer y = 1536
integer width = 50
integer height = 36
integer taborder = 40
boolean bringtotop = true
string dataobject = "d_rpt_sum_prod_act_ship"
end type

type dw_rpt_sum_prod_reg_ship from u_pics_dw within w_month_report
boolean visible = false
integer x = 535
integer y = 1576
integer width = 50
integer height = 36
integer taborder = 10
boolean bringtotop = true
string dataobject = "d_rpt_sum_prod_reg_ship"
boolean hscrollbar = true
boolean hsplitscroll = true
end type

event clicked;call super::clicked;if Isnull( row ) then
	return
end if
this.selectRow(0, false )
this.selectRow(row, true )
end event

event constructor;call super::constructor;this.of_Setfind(TRUE)
This.of_Setfilter(TRUE)
This.of_Setsort(TRUE)
end event

event rowfocuschanged;call super::rowfocuschanged;long ll_curr_row


ll_curr_row = This.getrow ()
this.SelectRow(0, false )
this.SelectRow(ll_curr_row , true )
this.ScrollToRow( ll_curr_row )
this.SetRow( ll_curr_row )
this.SetFocus()
end event

type dw_rpt_init_3_columns_open from u_pics_dw within w_month_report
boolean visible = false
integer x = 526
integer y = 1624
integer width = 50
integer height = 36
integer taborder = 20
boolean bringtotop = true
string dataobject = "d_rpt_init_3_columns_open"
boolean hscrollbar = true
boolean hsplitscroll = true
end type

event clicked;call super::clicked;if Isnull( row ) then
	return
end if
this.selectRow(0, false )
this.selectRow(row, true )
end event

event constructor;call super::constructor;this.of_Setfind(TRUE)
This.of_Setfilter(TRUE)
This.of_Setsort(TRUE)
end event

event rowfocuschanged;call super::rowfocuschanged;long ll_curr_row


ll_curr_row = This.getrow ()
this.SelectRow(0, false )
this.SelectRow(ll_curr_row , true )
this.ScrollToRow( ll_curr_row )
this.SetRow( ll_curr_row )
this.SetFocus()
end event

type cb_option from commandbutton within w_month_report
event mousemove pbm_mousemove
string tag = "Print the record"
integer x = 2459
integer y = 1544
integer width = 430
integer height = 108
integer taborder = 60
boolean bringtotop = true
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
parent.triggerevent('pfc_postopen')

end event

type dw_rpt_sum_prod_late_ship from u_pics_dw within w_month_report
boolean visible = false
integer x = 407
integer y = 1624
integer width = 50
integer height = 36
integer taborder = 10
boolean bringtotop = true
string dataobject = "d_rpt_sum_prod_late_ship"
boolean hscrollbar = true
boolean hsplitscroll = true
end type

event clicked;call super::clicked;if Isnull( row ) then
	return
end if
this.selectRow(0, false )
this.selectRow(row, true )
end event

event constructor;call super::constructor;this.of_Setfind(TRUE)
This.of_Setfilter(TRUE)
This.of_Setsort(TRUE)
end event

event rowfocuschanged;call super::rowfocuschanged;long ll_curr_row


ll_curr_row = This.getrow ()
this.SelectRow(0, false )
this.SelectRow(ll_curr_row , true )
this.ScrollToRow( ll_curr_row )
this.SetRow( ll_curr_row )
this.SetFocus()
end event


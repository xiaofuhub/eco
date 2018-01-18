$PBExportHeader$w_special_distribution_schedule.srw
$PBExportComments$Special Distribution schedule used only for Rush books
forward
global type w_special_distribution_schedule from w_sheet
end type
type cb_deselect from u_cb within w_special_distribution_schedule
end type
type dw_bklist from u_pics_dw within w_special_distribution_schedule
end type
type dw_dist from u_pics_dw within w_special_distribution_schedule
end type
type st_dist from statictext within w_special_distribution_schedule
end type
type st_file from statictext within w_special_distribution_schedule
end type
type st_imp from statictext within w_special_distribution_schedule
end type
type dw_batch_process from u_pics_dw within w_special_distribution_schedule
end type
type cbx_distsched from u_cbx within w_special_distribution_schedule
end type
type cbx_file from u_cbx within w_special_distribution_schedule
end type
type cbx_imp from u_cbx within w_special_distribution_schedule
end type
type dw_dsdtdsflag from u_pics_dw within w_special_distribution_schedule
end type
type dw_import_cp_allotbatch_sql from u_pics_dw within w_special_distribution_schedule
end type
type st_insertupdate from statictext within w_special_distribution_schedule
end type
type rb_batch from u_rb within w_special_distribution_schedule
end type
type rb_include from u_rb within w_special_distribution_schedule
end type
type st_bkorrow from statictext within w_special_distribution_schedule
end type
type sle_book# from u_sle within w_special_distribution_schedule
end type
type cb_new from u_cb within w_special_distribution_schedule
end type
type cbx_include from u_cbx within w_special_distribution_schedule
end type
type em_schdate from u_em within w_special_distribution_schedule
end type
type st_1 from statictext within w_special_distribution_schedule
end type
type cb_include from u_cb within w_special_distribution_schedule
end type
type cb_cancel from u_cb within w_special_distribution_schedule
end type
type cb_find from u_cb within w_special_distribution_schedule
end type
type dw_update_selcd_sched from u_dw within w_special_distribution_schedule
end type
type uo_progress from u_progressbar within w_special_distribution_schedule
end type
type cb_print from u_cb within w_special_distribution_schedule
end type
type gb_progress from groupbox within w_special_distribution_schedule
end type
type cbx_import from u_cbx within w_special_distribution_schedule
end type
type st_include from statictext within w_special_distribution_schedule
end type
type cb_file from u_cb within w_special_distribution_schedule
end type
type cb_create from u_cb within w_special_distribution_schedule
end type
type cb_import from u_cb within w_special_distribution_schedule
end type
type dw_no_master from u_dw within w_special_distribution_schedule
end type
type dw_distribution_schedule from u_dw within w_special_distribution_schedule
end type
type dw_import_cp_allotbatch from u_dw within w_special_distribution_schedule
end type
end forward

global type w_special_distribution_schedule from w_sheet
integer width = 2757
string title = "Special Distribution Schedule (RUSH Books)"
cb_deselect cb_deselect
dw_bklist dw_bklist
dw_dist dw_dist
st_dist st_dist
st_file st_file
st_imp st_imp
dw_batch_process dw_batch_process
cbx_distsched cbx_distsched
cbx_file cbx_file
cbx_imp cbx_imp
dw_dsdtdsflag dw_dsdtdsflag
dw_import_cp_allotbatch_sql dw_import_cp_allotbatch_sql
st_insertupdate st_insertupdate
rb_batch rb_batch
rb_include rb_include
st_bkorrow st_bkorrow
sle_book# sle_book#
cb_new cb_new
cbx_include cbx_include
em_schdate em_schdate
st_1 st_1
cb_include cb_include
cb_cancel cb_cancel
cb_find cb_find
dw_update_selcd_sched dw_update_selcd_sched
uo_progress uo_progress
cb_print cb_print
gb_progress gb_progress
cbx_import cbx_import
st_include st_include
cb_file cb_file
cb_create cb_create
cb_import cb_import
dw_no_master dw_no_master
dw_distribution_schedule dw_distribution_schedule
dw_import_cp_allotbatch dw_import_cp_allotbatch
end type
global w_special_distribution_schedule w_special_distribution_schedule

type variables
str_distrib_schedule istr_add
string is_add='N', is_cntr1[],is_cntr2[], is_bkmed[], is_libcd[], is_med[]
long i_count=0, i_remainder, ii_bkseq[]
datetime id_cabdt[], id_assigndt[], id_cabdate
boolean ib_web_done=false, ib_old_batch_exist=FALSE
end variables

forward prototypes
public subroutine wf_intrests_cpt ()
public subroutine wf_distrib_schedule_icd_no_icd ()
public subroutine wf_distrib_sched_simple ()
public function integer wf_quota_check (date ad_cabdt)
public function date wf_get_cabdt ()
public function string wf_textcheck (string as_lib, string as_med)
public subroutine wf_sendemail (string as_text, date ad_cabdt)
public function integer wf_reset_includecheck (date ad_cabdt)
public function integer wf_cleanarray ()
end prototypes

public subroutine wf_intrests_cpt ();long i,j
dec lr_rate0=0.06,lr_rate1=0.0625, lr_rate2=0.07125, lr_sum0,lr_sum1,lr_sum2, lr_sum3,&
		lr0_15, lr1_15,lr2_15, lr3_30, lr_prod,lr_pct0,lr_pct1, lr_pct2, lr_pct3,&
		lr_rate3=0.07125
		
string ls_pct1, ls_pct2, ls_pct0, ls_pct3

lr_prod=1
lr_sum0=0
for i=1 to 180
	lr_prod=lr_prod*(1+lr_rate0/12)
	lr_sum0=lr_sum0+ lr_prod
next

lr_prod=1
lr_sum1=0
for i=1 to 180
	lr_prod=lr_prod*(1+lr_rate1/12)
	lr_sum1=lr_sum1+ lr_prod
next

lr_sum2=0
lr_prod=1
for i=1 to 180
	lr_prod=lr_prod*(1+lr_rate2/12)
	lr_sum2=lr_sum2+ lr_prod
next

lr_sum3=0
lr_prod=1
for i=1 to 360
	lr_prod=lr_prod*(1+lr_rate2/12)
	lr_sum3=lr_sum3+ lr_prod
next

lr0_15=1
for i=1 to 15
	lr0_15=lr0_15 * (1+lr_rate0)
next

lr1_15=1
for i=1 to 15
	lr1_15=lr1_15 * (1+lr_rate1)
next

lr2_15=1
for i=1 to 15
	lr2_15=lr2_15 * (1+lr_rate2)
next


lr3_30=1
for i=1 to 30
	lr3_30=lr3_30 * (1+lr_rate3)
next
lr_pct0= (lr0_15/lr_sum0 )* 100
lr_pct1= (lr1_15/lr_sum1 )* 100
lr_pct2= (lr2_15/lr_sum2 )* 100
lr_pct3= (lr3_30/lr_sum3 )* 100
ls_pct1=string(lr_pct1,'###,##0.00000')
ls_pct2=string(lr_pct2,'###,##0.00000')
ls_pct0=string(lr_pct0,'###,##0.00000')
ls_pct3=string(lr_pct3,'###,##0.00000')
messagebox('pct1 and pct2: ','pct1 = : '+ls_pct1+'   pct2 =  : '+ls_pct2+&
			'~n~n6% pct0= : '+ls_pct0+' 30 years pct3= : '+ls_pct3)
end subroutine

public subroutine wf_distrib_schedule_icd_no_icd ();//string ls_today, ls_bkmed,ls_conno, ls_prodstg,ls_cntr,ls_bkmedold,ls_connoold, ls_cntrold,&
//		ls_null, ls_prodstgold, ls_cntr2
//date ld_today, ld_cabdt,ld_assigndt,ld_cabdtold,ld_assigndtold,ld_null
//long i,li_bkseq,li_cur, li_row_count,li_bkseqold, li_bound
//
//
//li_bound=UpperBound(ii_bkseq[])
//ld_today= Today()
//ls_today= string(ld_today,'mm/dd/yyyy')
////em_schdate.text=ls_today
//dw_import_cp_allotbatch.visible=false
//if cbx_include.checked=true then
//	if cb_new.text='&New' then
//		cb_find.visible=false
//		cb_new.text='&Clear'
//		cb_file.visible=true
//		cb_include.visible=true
////		cb_print.visible=true
//		dw_distribution_schedule.dataobject='d_distribution_schedule_null'
//		dw_distribution_schedule.settransobject(sqlservertrans)
//		dw_distribution_schedule.Retrieve()
//		if ib_web_done=false then
//			for i=1 to li_bound
//				li_cur=dw_distribution_schedule.InsertRow(0)
//				dw_distribution_schedule.object.bkmed[li_cur]=is_bkmed[i]
//				dw_distribution_schedule.object.bkseq[li_cur]=ii_bkseq[i]
//				dw_distribution_schedule.object.cntr1[li_cur]=is_cntr1[i]
//				dw_distribution_schedule.object.cntr2[li_cur]=is_cntr2[i]
//				dw_distribution_schedule.object.cabdt[li_cur]=id_cabdt[i]
//				dw_distribution_schedule.object.assigndt[li_cur]=id_assigndt[i]
//				dw_distribution_schedule.object.repeat[li_cur]='Y'
//				dw_distribution_schedule.object.include1[li_cur]='Y'
//			next
//		end if
//		dw_distribution_schedule.Sort()
//		li_row_count=dw_distribution_schedule.RowCount()
//		sle_book#.text=string(li_row_count)
//		cb_cancel.visible=true
//		if li_row_count >0 then
//			dw_distribution_schedule.object.t_bkno.visible=true
//			dw_distribution_schedule.object.t_cntr.visible=true
//			dw_distribution_schedule.object.t_cabdt.visible=true
//			dw_distribution_schedule.object.t_dupcode.visible=true
//			dw_distribution_schedule.object.t_assigndt.visible=true
//			dw_distribution_schedule.object.t_repeat.visible=true
//			dw_distribution_schedule.object.t_include.visible=true
//		else
//			dw_distribution_schedule.object.t_bkno.visible=false
//			dw_distribution_schedule.object.t_cntr.visible=false
//			dw_distribution_schedule.object.t_cabdt.visible=false
//			dw_distribution_schedule.object.t_dupcode.visible= false
//			dw_distribution_schedule.object.t_assigndt.visible=false
//			dw_distribution_schedule.object.t_repeat.visible=false
//			dw_distribution_schedule.object.t_include.visible=false
//		end if
//	elseif cb_new.text='&Clear' then
//		dw_distribution_schedule.settransobject(sqlservertrans)
//		dw_distribution_schedule.Reset()
//		li_row_count=dw_distribution_schedule.RowCount()
//		sle_book#.text=string(li_row_count)
//		cb_find.visible=true
//		em_schdate.SetFocus()
//		cb_new.text='&New'
//		cb_file.visible=false
//		cb_include.visible=false
//		cb_print.visible=false
//	end if
//	dw_distribution_schedule_null_incld_cpy.visible=false
//	dw_distribution_schedule.visible=true
//else //cbx_include.checked=false
//	if cb_new.text='&New' then
//		cb_find.visible=false
//		cb_new.text='&Clear'
//		cb_file.visible=true
//		cb_include.visible=true
////		cb_print.visible=true
//		dw_distribution_schedule.dataobject='d_distribution_schedule_null_incld'
//		dw_distribution_schedule.settransobject(sqlservertrans)
//		dw_distribution_schedule.Retrieve()
//		li_row_count=dw_distribution_schedule.RowCount()
//		boolean lb_start=true
//		for i=1 to li_row_count
//			li_bkseq=dw_distribution_schedule.GetItemNumber(i,'bkseq')
//			ls_bkmed=dw_distribution_schedule.GetItemString(i,'bkmed')
//			ls_conno=dw_distribution_schedule.GetItemString(i,'conno')
//			ls_prodstg=dw_distribution_schedule.GetItemString(i,'prodstage')
//			ls_cntr=dw_distribution_schedule.GetItemString(i,'cntr')
//			ld_cabdt=dw_distribution_schedule.GetItemDate(i,'cabdt')
//			ld_assigndt=dw_distribution_schedule.GetItemDate(i,'assigndt')
//			if lb_start=true then
//				li_bkseqold=li_bkseq
//				ls_bkmedold=ls_bkmed
//				ls_connoold=ls_conno
//				ls_prodstgold=ls_prodstg
//				ls_cntrold=ls_cntr
//				ld_cabdtold=ld_cabdt
//				ld_assigndtold=ld_assigndt
//			end if
//			if li_bkseq<> li_bkseqold  then
//				if lb_start=false and (ls_prodstgold='DU' OR ls_prodstgold='PR') THEN
//					li_cur=dw_distribution_schedule_null_incld_cpy.InsertRow(0)
//					dw_distribution_schedule_null_incld_cpy.SetItem(li_cur,'bkseq',li_bkseqold)
//					dw_distribution_schedule_null_incld_cpy.SetItem(li_cur,'bkmed',ls_bkmedold)
//					dw_distribution_schedule_null_incld_cpy.SetItem(li_cur,'conno',ls_connoold)
//					dw_distribution_schedule_null_incld_cpy.SetItem(li_cur,'cntr1',ls_cntrold)
//					dw_distribution_schedule_null_incld_cpy.SetItem(li_cur,'cntr2',ls_null)
//					dw_distribution_schedule_null_incld_cpy.SetItem(li_cur,'cabdt',ld_cabdtold)
//					dw_distribution_schedule_null_incld_cpy.SetItem(li_cur,'assigndt',ld_null)
//					li_bkseqold=li_bkseq
//					ls_bkmedold=ls_bkmed
//					ls_connoold=ls_conno
//					ls_prodstgold=ls_prodstg
//					ls_cntrold=ls_cntr
//					ld_cabdtold=ld_cabdt
//					ld_assigndtold=ld_assigndt
//					lb_start=false
//					continue
//				elseif lb_start=false and (ls_prodstgold='MA' OR ls_prodstgold='AB') then
//					li_bkseqold=li_bkseq
//					ls_bkmedold=ls_bkmed
//					ls_connoold=ls_conno
//					ls_prodstgold=ls_prodstg
//					ls_cntrold=ls_cntr
//					ld_cabdtold=ld_cabdt
//					ld_assigndtold=ld_assigndt
//					lb_start=false
//					continue
//				end if
//			elseif lb_start=false  and (ls_bkmed=ls_bkmedold) then//if li_bkseq=li_bkseqold
//				if (IsNull(ls_cntr) or IsNull(ls_cntrold)) then 
//					li_bkseqold=li_bkseq
//					ls_bkmedold=ls_bkmed
//					ls_connoold=ls_conno
//					ls_prodstgold=ls_prodstg
//					ls_cntrold=ls_cntr
//					ld_cabdtold=ld_cabdt
//					ld_assigndtold=ld_assigndt
//					lb_start=true
//					continue
//				end if
//				li_cur=dw_distribution_schedule_null_incld_cpy.InsertRow(0)
//				dw_distribution_schedule_null_incld_cpy.SetItem(li_cur,'bkseq',li_bkseq)
//				dw_distribution_schedule_null_incld_cpy.SetItem(li_cur,'bkmed',ls_bkmed)
//				dw_distribution_schedule_null_incld_cpy.SetItem(li_cur,'conno',ls_conno)
//				dw_distribution_schedule_null_incld_cpy.SetItem(li_cur,'cabdt',ld_cabdt)
//							
//				if ls_prodstgold='AB' AND ls_prodstg='DU' THEN 
//					dw_distribution_schedule_null_incld_cpy.SetItem(li_cur,'cntr2',ls_cntr)
//					dw_distribution_schedule_null_incld_cpy.SetItem(li_cur,'cntr1',ls_cntrold)
//					dw_distribution_schedule_null_incld_cpy.SetItem(li_cur,'assigndt',ld_assigndt)
//				end if	
//				if	ls_prodstgold='DU' AND ls_prodstg='MA' then
//					dw_distribution_schedule_null_incld_cpy.SetItem(li_cur,'cntr1',ls_cntr)
//					dw_distribution_schedule_null_incld_cpy.SetItem(li_cur,'cntr2',ls_cntrold)
//					dw_distribution_schedule_null_incld_cpy.SetItem(li_cur,'assigndt',ld_assigndtold)
//				end if
//				if (ls_prodstgold='AB' or ls_prodstgold='MA' ) AND ls_prodstg='PR' THEN 
//					dw_distribution_schedule_null_incld_cpy.SetItem(li_cur,'cntr2',ls_cntr)
//					dw_distribution_schedule_null_incld_cpy.SetItem(li_cur,'cntr1',ls_cntrold)
//					dw_distribution_schedule_null_incld_cpy.SetItem(li_cur,'assigndt',ld_assigndt)
//				end if
//				if (ls_prodstgold='PB'  ) AND ls_prodstg='PU' THEN 
//					dw_distribution_schedule_null_incld_cpy.SetItem(li_cur,'cntr1',ls_cntr)
//					dw_distribution_schedule_null_incld_cpy.SetItem(li_cur,'cntr2',ls_cntrold)
//					dw_distribution_schedule_null_incld_cpy.SetItem(li_cur,'assigndt',ld_assigndtold)
//				end if
//				li_bkseqold=li_bkseq
//				ls_bkmedold=ls_bkmed
//				ls_connoold=ls_conno
//				ls_prodstgold=ls_prodstg
//				ls_cntrold=ls_cntr
//				ld_cabdtold=ld_cabdt
//				ld_assigndtold=ld_assigndt
//				lb_start=true
//				continue
//			end if //end if li_bkseq=li_bkseqold and ls_bkmed=ls....
//			lb_start=false
//		next
//		li_row_count=dw_distribution_schedule_null_incld_cpy.Rowcount()
//		if li_row_count>0 then
//			for i=1 to li_row_count
//				dw_distribution_schedule_null_incld_cpy.SetItem(i,'repeat','N')
//				dw_distribution_schedule_null_incld_cpy.SetItem(i,'include1','Y')	
//			next
//		end if
//		dw_distribution_schedule_null_incld_cpy.visible=true
//		dw_distribution_schedule.visible=false
//		if ib_web_done= false then
//			for i=1 to li_bound
//				li_cur=dw_distribution_schedule_null_incld_cpy.InsertRow(0)
//				dw_distribution_schedule_null_incld_cpy.object.bkmed[li_cur]=is_bkmed[i]
//				dw_distribution_schedule_null_incld_cpy.object.bkseq[li_cur]=ii_bkseq[i]
//				dw_distribution_schedule_null_incld_cpy.object.cntr1[li_cur]=is_cntr1[i]
//				dw_distribution_schedule_null_incld_cpy.object.cntr2[li_cur]=is_cntr2[i]
//				dw_distribution_schedule_null_incld_cpy.object.cabdt[li_cur]=id_cabdt[i]
//				dw_distribution_schedule_null_incld_cpy.object.assigndt[li_cur]=id_assigndt[i]
//				dw_distribution_schedule_null_incld_cpy.object.repeat[li_cur]='Y'
//				dw_distribution_schedule_null_incld_cpy.object.include1[li_cur]='Y'
//			next
//		end if
//		dw_distribution_schedule_null_incld_cpy.Sort()
//		li_row_count=dw_distribution_schedule_null_incld_cpy.RowCount()
//		for i=1 to li_row_count
//			ls_cntr2=dw_distribution_schedule_null_incld_cpy.object.cntr2[li_row_count+1 - i]
//			ls_cntr2=trim(ls_cntr)
//			if IsNull(ls_cntr2) or ls_cntr2='' then
//				dw_distribution_schedule_null_incld_cpy.DeleteRow(li_row_count+1 - i )
//			end if
//		next
//		li_row_count=dw_distribution_schedule_null_incld_cpy.RowCount()
//		sle_book#.text=string(li_row_count)
//		ls_today= string(ld_today,'mm/dd/yyyy')
////		em_schdate.text=ls_today
//		cb_cancel.visible=true
//		if li_row_count >0 then
//			dw_distribution_schedule_null_incld_cpy.object.t_bkno.visible=true
//			dw_distribution_schedule_null_incld_cpy.object.t_cntr.visible=true
//			dw_distribution_schedule_null_incld_cpy.object.t_cabdt.visible=true
//			dw_distribution_schedule_null_incld_cpy.object.t_dupcode.visible=true
//			dw_distribution_schedule_null_incld_cpy.object.t_assigndt.visible=true
//			dw_distribution_schedule_null_incld_cpy.object.t_repeat.visible=true
//			dw_distribution_schedule_null_incld_cpy.object.t_include.visible=true
//		else
//			dw_distribution_schedule_null_incld_cpy.object.t_bkno.visible=false
//			dw_distribution_schedule_null_incld_cpy.object.t_cntr.visible=false
//			dw_distribution_schedule_null_incld_cpy.object.t_cabdt.visible=false
//			dw_distribution_schedule_null_incld_cpy.object.t_dupcode.visible=false
//			dw_distribution_schedule_null_incld_cpy.object.t_assigndt.visible=false
//			dw_distribution_schedule_null_incld_cpy.object.t_repeat.visible=false
//			dw_distribution_schedule_null_incld_cpy.object.t_include.visible=false
//		end if
//	elseif cb_new.text='&Clear' then
//		dw_distribution_schedule_null_incld_cpy.ReSet()
//		dw_distribution_schedule.settransobject(sqlservertrans)
//		dw_distribution_schedule.Reset()
//		li_row_count=dw_distribution_schedule_null_incld_cpy.RowCount()
//		sle_book#.text=string(li_row_count)
//		cb_find.visible=true
//		em_schdate.SetFocus()
//		cb_new.text='&New'
//		cb_file.visible=false
//		cb_include.visible=false
//		cb_print.visible=false
//	end if
//end if
end subroutine

public subroutine wf_distrib_sched_simple ();String  ls_bkmed,ls_cntr, ls_cntr1,ls_repeat, ls_include1,&
		ls_null, ls_cntr2, ls_cntrtype, ls_bkmedold, ls_del
Date ld_today, ld_cabdt,ld_assigndt,ld_null,ld_max
DateTime ld_cabdt_dt, ld_assigndt_dt, ld_max_dt
Long i,k,J,li_bkseq,li_cur, li_row_count, li_bound, li_re, li_cnt,&
		li_bkseqold

ld_max=Date('01/01/1900')
ld_max_dt=DateTime(ld_max,Time('00:00:00'))
li_bound=UpperBound(ii_bkseq[])
dw_import_cp_allotbatch.visible=FALSE
//this datawindow for retrieve the rows in sched table all the rows one have two stages the
// other just one stage. in second case cntr2 is null
// 04/08/2008 special processing for rush books
dw_distribution_schedule.dataObject='d_distribution_schedule_null_rush'
dw_distribution_schedule.SetTransObject(SqlServerTrans)
dw_no_master.SetTransObject(SqlServerTrans)
IF cb_new.text='&New' THEN
	li_cnt=dw_distribution_schedule.Retrieve()
	J=0
	ls_bkmedold=''
	li_bkseqold=0
	// the datawindow difinition hand self join of prod considered if cntr1=cntr2 case
	// this already considered cntrtype in ('T','M') case but there is cntr1<>cntr2 case
	// ie. one book associate two cntrtype one is 'M', the other is 'D' each one just one row
	// here also consider one book have 3 rows in prod table for example 'M' type have two prodstage
	// and cntr1=cntr2 and have 3rd row is 'D' type these 3 rows must written 1 row. datawidow olse
	// consider one book have 4 rows case have double 'T' cntrtype each has 2 rows.
	// use select statement to analyze prod rows group by bkmed,bkseq and count(*) each group have
	// how many rows, you have 4 rows case, 3 rows case, 2 rows case ( cntr1<>cntr2 or cntr1=cntr2)
	// very complicate, this first retrieve don't delete one 1 row mastering book.
	// keep in mind one book only one row, choose greatest assigndt and associated cntr2
	FOR i=1 TO li_cnt
		li_bkseq=dw_distribution_schedule.object.bkseq[i]
		ls_bkmed=dw_distribution_schedule.object.bkmed[i]
		IF li_bkseq=49342 OR li_bkseq=50551 OR li_bkseq=50559 THEN
			li_re=1000
		END IF
		IF li_bkseq<>li_bkseqold OR ls_bkmed<>ls_bkmedold THEN
//			j=1
			ls_bkmedold=ls_bkmed
			li_bkseqold=li_bkseq
			ld_max=Date('01/01/1900')
			ld_max_dt=DateTime(ld_max,Time('00:00:00'))
			IF J>1 THEN
				FOR k=1 TO J
					ls_cntr1=dw_distribution_schedule.object.cntr1[i+k - J - 1 ]
					ls_cntr2=dw_distribution_schedule.object.cntr2[i+k - J - 1 ]
					ld_assigndt_dt=dw_distribution_schedule.object.assigndt[i+k - J - 1 ]
					IF ld_assigndt_dt>ld_max_dt THEN
						ld_max_dt=ld_assigndt_dt
					END IF
					IF Trim(ls_cntr1)<>'' AND Trim(ls_cntr2)<>'' THEN
						dw_distribution_schedule.object.del[i+k - J - 1 ]='N'
					ELSEIF Trim(ls_cntr1)='' AND J>=2 THEN
						dw_distribution_schedule.object.del[i+k - J - 1 ]='Y'
					ELSEIF Trim(ls_cntr2)='' AND J>=2 THEN
						dw_distribution_schedule.object.del[i+k - J - 1 ]='Y'
					END IF
				NEXT
				FOR k=1 TO J
					ld_assigndt_dt=dw_distribution_schedule.object.assigndt[i+k - J - 1 ]
					IF ld_assigndt_dt=ld_max_dt THEN
						ls_cntr2=dw_distribution_schedule.object.cntr2[i+k - J - 1 ]
					END IF
				NEXT
				FOR k=1 TO J
					ls_del=dw_distribution_schedule.object.del[i+k - J - 1 ]
					IF ls_del='N' THEN
						dw_distribution_schedule.object.cntr2[i+k - J - 1 ]=ls_cntr2
						dw_distribution_schedule.object.assigndt[i+k - J - 1 ]=ld_max_dt
					END IF
				NEXT
			END IF// if j>1
			J=1 //remember only here set j=1
		ELSE	
			J++
		END IF //end if ls_bkmed<>ls_bkmedold
	NEXT
	FOR i=1 TO li_cnt
		li_bkseq=dw_distribution_schedule.object.bkseq[li_cnt+1 - i]
		ls_bkmed=dw_distribution_schedule.object.bkmed[li_cnt+1 - i]
		IF li_bkseq=49342 OR li_bkseq=50551 OR li_bkseq=50559 THEN
			li_re=1000
		END IF
		ls_del=dw_distribution_schedule.object.del[li_cnt+1 - i]
		IF ls_del='Y' THEN
			dw_distribution_schedule.DeleteRow(li_cnt+1 - i)
		END IF
	NEXT
	li_cnt=dw_distribution_schedule.RowCount()
END IF// end if cb_new.text='&New'


IF cbx_include.checked=TRUE THEN
	IF cb_new.text='&New' THEN
		cb_find.visible=FALSE
		cb_new.text='&Clear'
		cb_file.visible=TRUE
//		cb_include.visible=TRUE
//		cb_print.visible=true
		IF ib_web_done=FALSE AND li_bound>0 THEN
			FOR i=1 TO li_bound
				li_cur=dw_distribution_schedule.InsertRow(0)
				dw_distribution_schedule.object.bkmed[li_cur]=is_bkmed[i]
				dw_distribution_schedule.object.bkseq[li_cur]=ii_bkseq[i]
				dw_distribution_schedule.object.cntr1[li_cur]=is_cntr1[i]
				dw_distribution_schedule.object.cntr2[li_cur]=is_cntr2[i]
				dw_distribution_schedule.object.cabdt[li_cur]=id_cabdt[i]
				dw_distribution_schedule.object.assigndt[li_cur]=id_assigndt[i]
				dw_distribution_schedule.object.repeat[li_cur]='Y'
				dw_distribution_schedule.object.include1[li_cur]='Y'
			NEXT
		END IF
		dw_distribution_schedule.Sort()
		li_row_count=dw_distribution_schedule.RowCount()
		sle_book#.text=String(li_row_count)
		cb_cancel.visible=TRUE
		IF li_row_count >0 THEN
			dw_distribution_schedule.object.t_bkno.visible=TRUE
			dw_distribution_schedule.object.t_cntr.visible=TRUE
			dw_distribution_schedule.object.t_cabdt.visible=TRUE
			dw_distribution_schedule.object.t_dupcode.visible=TRUE
			dw_distribution_schedule.object.t_assigndt.visible=TRUE
			dw_distribution_schedule.object.t_repeat.visible=TRUE
			dw_distribution_schedule.object.t_include.visible=TRUE
		ELSE
			dw_distribution_schedule.object.t_bkno.visible=FALSE
			dw_distribution_schedule.object.t_cntr.visible=FALSE
			dw_distribution_schedule.object.t_cabdt.visible=FALSE
			dw_distribution_schedule.object.t_dupcode.visible= FALSE
			dw_distribution_schedule.object.t_assigndt.visible=FALSE
			dw_distribution_schedule.object.t_repeat.visible=FALSE
			dw_distribution_schedule.object.t_include.visible=FALSE
		END IF
	ELSEIF cb_new.text='&Clear' THEN
		dw_distribution_schedule.SetTransObject(SqlServerTrans)
		dw_distribution_schedule.Reset()
		li_row_count=dw_distribution_schedule.RowCount()
		sle_book#.text=String(li_row_count)
		cb_find.visible=TRUE
		em_schdate.SetFocus()
		cb_new.text='&New'
		cb_file.visible=FALSE
		cb_include.visible=FALSE
		cb_print.visible=FALSE
	END IF
	dw_no_master.visible=FALSE

	dw_distribution_schedule.visible=TRUE
ELSE //cbx_include.checked=false
	// in this case will get rid off the row where cntr2 is null but include the cntrtype='D'
	// even ls_cntr=null
	dw_no_master.Reset()
	IF cb_new.text='&New' THEN
		cb_find.visible=FALSE
		cb_new.text='&Clear'
		cb_file.visible=TRUE
//		cb_include.visible=TRUE
//		cb_print.visible=true
		li_row_count=dw_distribution_schedule.RowCount()
		FOR i=1 TO li_row_count	
			ls_cntrtype=dw_distribution_schedule.object.cntrtype[i]
			ls_cntr2=dw_distribution_schedule.object.cntr2[i]
			ls_cntr2=Trim(ls_cntr2)
			IF (ls_cntr2='' OR IsNull(ls_cntr2)) AND ls_cntrtype<>'D' THEN CONTINUE
			// for cntrtype='D' the ls_cntr2 can be null, see dw object definition
			ls_bkmed=dw_distribution_schedule.object.bkmed[i]
			li_bkseq=dw_distribution_schedule.object.bkseq[i]
			ls_cntr1=dw_distribution_schedule.object.cntr1[i]
			ls_cntr2=dw_distribution_schedule.object.cntr2[i]
			ld_cabdt_dt=dw_distribution_schedule.object.cabdt[i]
			ld_assigndt_dt=dw_distribution_schedule.object.assigndt[i]
			ls_repeat=dw_distribution_schedule.object.repeat[i]
			ls_include1=dw_distribution_schedule.object.include1[i]
			li_cur=dw_no_master.InsertRow(0)
			
			dw_no_master.object.bkmed[li_cur]= ls_bkmed
			dw_no_master.object.bkseq[li_cur]=li_bkseq
			dw_no_master.object.cntr1[li_cur]=ls_cntr1
			dw_no_master.object.cntr2[li_cur]= ls_cntr2
			dw_no_master.object.cabdt[li_cur]= ld_cabdt_dt
			dw_no_master.object.assigndt[li_cur]= ld_assigndt_dt
			dw_no_master.object.repeat[li_cur]= ls_repeat
			dw_no_master.object.include1[li_cur]=ls_include1
			dw_no_master.object.del[li_cur]='N'
		NEXT
		IF ib_web_done=FALSE AND li_bound>0 THEN// for repeat include every one is included
			FOR i=1 TO li_bound
				ls_cntr2=is_cntr2[i]
				ls_cntr2=Trim(ls_cntr2)
				
//				if ls_cntr2='' or IsNull(ls_cntr2) then
//					li_re=messagebox('Include Books','This book has not yet been assigned to'+&
//					'a duplicator. Do you still wish to include this book?',Exclamation!,YesNo!,1)
//					if li_re=2 then continue
//				end if
				
				li_cur=dw_no_master.InsertRow(0)
				dw_no_master.object.bkmed[li_cur]=is_bkmed[i]
				dw_no_master.object.bkseq[li_cur]=ii_bkseq[i]
				dw_no_master.object.cntr1[li_cur]=is_cntr1[i]
				dw_no_master.object.cntr2[li_cur]=is_cntr2[i]
				dw_no_master.object.cabdt[li_cur]=id_cabdt[i]
				dw_no_master.object.assigndt[li_cur]=id_assigndt[i]
				dw_no_master.object.repeat[li_cur]='Y'
				dw_no_master.object.include1[li_cur]='Y'
			NEXT
		END IF
		dw_no_master.Sort()
		dw_no_master.visible=TRUE
		dw_distribution_schedule.visible=FALSE

		li_row_count=dw_no_master.RowCount()
		sle_book#.text=String(li_row_count)
	
		cb_cancel.visible=TRUE
		IF li_row_count >0 THEN
			dw_no_master.object.t_bkno.visible=TRUE
			dw_no_master.object.t_cntr.visible=TRUE
			dw_no_master.object.t_cabdt.visible=TRUE
			dw_no_master.object.t_dupcode.visible=TRUE
			dw_no_master.object.t_assigndt.visible=TRUE
			dw_no_master.object.t_repeat.visible=TRUE
			dw_no_master.object.t_include.visible=TRUE
		ELSE
			dw_no_master.object.t_bkno.visible=FALSE
			dw_no_master.object.t_cntr.visible=FALSE
			dw_no_master.object.t_cabdt.visible=FALSE
			dw_no_master.object.t_dupcode.visible=FALSE
			dw_no_master.object.t_assigndt.visible=FALSE
			dw_no_master.object.t_repeat.visible=FALSE
			dw_no_master.object.t_include.visible=FALSE
		END IF
	ELSEIF cb_new.text='&Clear' THEN
		dw_no_master.Reset()
		dw_distribution_schedule.SetTransObject(SqlServerTrans)
		dw_distribution_schedule.Reset()
		li_row_count=dw_no_master.RowCount()
		sle_book#.text=String(li_row_count)
		cb_find.visible=TRUE
		em_schdate.SetFocus()
		cb_new.text='&New'
		cb_file.visible=FALSE
		cb_include.visible=FALSE
		cb_print.visible=FALSE
	END IF
END IF// end of cbx_include.checked=true
end subroutine

public function integer wf_quota_check (date ad_cabdt);//////////////////////////////////////////////////////////////////////////////////////////////////////////
//
//	Function: wf_quota_check
//  Args: Batch Date
//	Description: Validate against QUOTAS to determine if any of the libraries lack quota or
//					0 quota during special distribution process. Do not allow to proceed if
// 					quota check fails.
//	
//
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//	
//	Revision History
//
//	Developed by 	Date 				Version						Tracking#
//									
// Murali K.			06/20/2008      Phase-2
// Murali K.        	07/11/2008 OLD BATCH BOOK EXIST IN THE MASTER LIST, so need to be allowed
//										distribution irrespective of current batch quota validation
// Murali K.			10/21/2008 if disconnected connect the transaction again.
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//

string ls_med, ls_libcd, ls_bkmed,ls_text, ls_oldmed, ls_textlibcd[], ls_textmed[]
int li_loop, li_year
long ll_bkseq, ll_qty
date ld_today, ld_cabdt
boolean lb_no_quota=FALSE


OpenWithParm(w_pics_retrieve_msg_box,"Quota Validation in Progress, Please wait...")
setpointer(hourglass!)


li_year = Year(ad_cabdt)
ld_today = today()

wf_cleanarray()

//10/21/2008 if disconnected connect the transaction again.
IF NOT SQLserverOracleTrans.DbHandle() >0 THEN
	SQLserverOracleTrans.of_connect() 
END IF

// Cursor to fetch all valid libraries in that batch
DECLARE batch_curs CURSOR FOR
select distinct libcd
from batch
where cabdt =:ad_cabdt using sqlserveroracletrans;
			
IF f_check_dberror(SqlServerOracleTrans,"Retrieving Libcd from Batch table") = FALSE THEN
	IF IsValid(w_pics_retrieve_msg_box) THEN
		Close(w_pics_retrieve_msg_box)
	END IF
	RETURN -1
END IF
	
FOR li_loop = 1 TO dw_no_master.RowCount()
	IF dw_no_master.object.include1[li_loop] = 'Y' THEN
		ll_bkseq	 = dw_no_master.object.bkseq[li_loop]
		ls_bkmed = dw_no_master.object.bkmed[li_loop]
		ld_cabdt =  date(dw_no_master.object.cabdt[li_loop])
		
		 // 07/11/2008 OLD BATCH BOOK EXIST IN THE MASTER LIST, so need to be allowed distribution irrespective of current batch quota validation
		IF ld_cabdt <> ad_cabdt THEN
			ib_old_batch_exist=TRUE
		END IF
		
		select med
		into :ls_med
		from mchar
		where bkseq = :ll_bkseq and bkmed = :ls_bkmed using sqlservertrans;
		
		IF ls_med <> 'P/B'  THEN
			ls_med = ls_bkmed
		END IF
		
		IF ls_med = ls_oldmed THEN CONTINUE // do not repeat for the same media
		
		ls_oldmed = ls_med 
	
		OPEN batch_curs; 
		
		// Get the first row from the result set.
		FETCH batch_curs INTO :ls_libcd;
		
		// for every library for that batch
		DO WHILE sqlserveroracletrans.sqlcode = 0
			
				// 07/08/2008
				setnull(ll_qty)
				
				// check the quotas table along with the media type
				select annual_quantity
				into :ll_qty
				from quotas
				where libcd = :ls_libcd
						 and media_type_code = :ls_med
						 and active_status_code = 'A' 
						 and copy_allot_year = :li_year
						 and effective_date in ( select max(effective_date) 
														from quotas 
														 where   libcd =:ls_libcd
																	and media_type_code = :ls_med
																	 and active_status_code = 'A' 
																	 and copy_allot_year =:li_year
																	 and effective_date <= :ad_cabdt // batch date
																	 )   using sqlserveroracletrans;
					IF Isnull(ll_qty) OR ll_qty <= 0 THEN
						lb_no_quota = TRUE
						ls_text +=wf_textcheck(ls_libcd, ls_med) 
					END IF
				FETCH batch_curs INTO :ls_libcd;
		LOOP
END IF
	
	close batch_curs ;
	
NEXT // next record 

IF IsValid(w_pics_retrieve_msg_box) THEN
	Close(w_pics_retrieve_msg_box)
END IF
setpointer(arrow!)
IF lb_no_quota THEN
	ls_text = 'Quota Validation failed for Special Distribution ' + '~r~n' + 'Please check the Quota  for the following Libraries - >' +  '~r~n' +  ls_text +  '~r~n' + ' Please fix the quota before proceeding with Special Distribution Schedule'
	IF ib_old_batch_exist THEN
		ls_text +='~r~n' + ' Some Old Batch Books exist in the list and it will be alllowed for Distribution.' + '~r~n' 
		wf_reset_includecheck(ad_cabdt)
	END IF
	wf_sendemail(ls_text,ad_cabdt)	
	Messagebox('Error',  ls_text + ' , Contact PCS Administrator')
	RETURN -1
END IF
RETURN 1

end function

public function date wf_get_cabdt ();//////////////////////////////////////////////////////////////////////////////////////////////////////////
//
//	Function: wf_get_cabdt
//  Args: None
//	Description: Returns the latest Open Batch Date
//	
//
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//	
//	Revision History
//
//	Developed by 	Date 				Version						Tracking#
//									
// Murali K.			06/20/2008      Phase-2
// Murali				07/08/2008		get the recent date without status code check
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//
date ld_date

IF NOT SQLserverOracleTrans.DbHandle() >0 THEN
	SQLserverOracleTrans.of_connect() 
END IF

SELECT Max(CABDT)
INTO :ld_date
FROM BATCH_DATA   using sqlserveroracletrans ; //WHERE BATCH_STATUS_CODE  in ( 'O')

RETURN ld_date

end function

public function string wf_textcheck (string as_lib, string as_med);//////////////////////////////////////////////////////////////////////////////////////////////////////////
//
//	Function: wf_textcheck
//  Args:  libcd, media
//	Description: Set and return the quota check text avoid duplicates
//	
//
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//	
//	Revision History
//
//	Developed by 	Date 				Version						Tracking#
//	Murali K.			07/08/2008			
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//

integer li_upperbound, li_loop
STRING LS_TEXT
boolean lb_found=FALSE

li_upperbound = Upperbound(is_libcd[])

IF li_upperbound = 0 THEN
	ls_text = as_lib +' ( Media: ' + as_med +')' +  '~n'
	is_libcd[li_upperbound+1] = as_lib
	is_med[li_upperbound+1] = as_med
ELSE
	FOR li_loop = 1 TO li_upperbound
		IF as_lib = is_libcd[li_loop] AND as_med = is_med[li_loop] THEN
			lb_found=TRUE
			EXIT
		END IF
	NEXT
	IF NOt lb_found THEN
		ls_text = as_lib +' ( Media: ' + as_med +')' +  '~n'
		is_libcd[li_upperbound+1] = as_lib
		is_med[li_upperbound+1] = as_med
//	ELSE
//		ls_text =  is_libcd[li_loop] +' ( Media: ' + is_med[li_loop] +')' +  '~n'
	END IF
END IF
RETURN ls_text
end function

public subroutine wf_sendemail (string as_text, date ad_cabdt);// send email if the quota validation fails to PCS administrators
mailSession				mSes
mailReturnCode			mRet
mailMessage			mMsg
mailFileDescription		mAttach
string					ls_ret, ls_syntax, ls_name, ls_open_pathname, ls_filename
string					ls_attach_name='marc0801.dat.txt', ls_adr
int						li_index, li_nret, li_nrecipients, li_nfile
datastore			lds_email
long 					ll_rc, ll_row


mRet=mailReturnFailure!
mSes = CREATE mailSession
mRet = mSes.mailLogon ( mailNewSession! )
ls_ret = f_mail_error_to_string ( mRet, 'Logon:', FALSE )

If mRet <> mailReturnSuccess! Then
	MessageBox ("Mail Logon failed", 'Return Code <> mailReturnSuccess! '+ls_ret )
	mSes.mailLogoff()
	DESTROY mSes
	return
End If
SetPointer(HourGlass!)
mMsg.Subject = 'Special Distribution Schedule Quota Validation Failure - Batch:'  + string(ad_cabdt,'mm/dd/yyyy')
mMsg.NoteText = as_text

// 07/09/2008 no hard coding of email addresses read from CONTACT table
IF NOT Isvalid(lds_email) THEN
	lds_email = create datastore
	lds_email.dataobject = 'd_cp_email_addresses'
	lds_email.settransobject(sqlserveroracletrans)
END IF
ll_rc = lds_email.Retrieve()
IF ll_rc > 0 THEN
	FOR ll_row = 1 to ll_rc
		 ls_adr = lds_email.object.pvt_email[ll_row] 
		 mMsg.Recipient[ll_row].Name = ls_adr
	NEXT
ELSE
	Messagebox('Error', ' There are no PCS administrators email defined in the CONTACT table')
	RETURN
END IF

mRet = mSes.mailsend ( mMsg )
ls_ret = f_mail_error_to_string ( mRet, 'Send mail:', FALSE )
IF mRet <> mailreturnsuccess! THEN
		MessageBox ("Mail Send",'Return Code <> mailReturnSuccess! '+ls_ret )
	RETURN
END IF	

mRet=mSes.mailLogoff()
DESTROY mSes


end subroutine

public function integer wf_reset_includecheck (date ad_cabdt);//////////////////////////////////////////////////////////////////////////////////////////////////////////
//
//	Function:wf_reset_includecheck
//  Args: Batch Date
//	Description: If any old batch book exist in the no master list allow it to be distributed
//					Reset include check box for the current batch books
//
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//	
//	Revision History
//
//	Developed by 	Date 				Version						Tracking#
//									
// Murali K.        	07/11/2008 OLD BATCH BOOK EXIST IN THE MASTER LIST, so need to be allowed
//										distribution irrespective of current batch quota validation
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//
date  ld_cabdt
integer li_loop

FOR li_loop = 1 TO dw_no_master.RowCount()
	ld_cabdt = date(dw_no_master.object.cabdt[li_loop])
	 // 07/11/2008 OLD BATCH BOOK EXIST IN THE MASTER LIST, so need to be allowed distribution irrespective of current batch quota validation
	IF ld_cabdt = ad_cabdt THEN // current batch reset the include swich
		dw_no_master.object.include1[li_loop] = 'N'
	END IF
NEXT // next record 

RETURN 1

end function

public function integer wf_cleanarray ();int li_upperbound, LI_LOOP

li_upperbound = Upperbound(is_libcd)

FOR li_loop = 1 TO li_upperbound
	is_libcd[li_loop] = ''
	is_med[li_loop] = ''
NEXT
RETURN 1
end function

on w_special_distribution_schedule.create
int iCurrent
call super::create
this.cb_deselect=create cb_deselect
this.dw_bklist=create dw_bklist
this.dw_dist=create dw_dist
this.st_dist=create st_dist
this.st_file=create st_file
this.st_imp=create st_imp
this.dw_batch_process=create dw_batch_process
this.cbx_distsched=create cbx_distsched
this.cbx_file=create cbx_file
this.cbx_imp=create cbx_imp
this.dw_dsdtdsflag=create dw_dsdtdsflag
this.dw_import_cp_allotbatch_sql=create dw_import_cp_allotbatch_sql
this.st_insertupdate=create st_insertupdate
this.rb_batch=create rb_batch
this.rb_include=create rb_include
this.st_bkorrow=create st_bkorrow
this.sle_book#=create sle_book#
this.cb_new=create cb_new
this.cbx_include=create cbx_include
this.em_schdate=create em_schdate
this.st_1=create st_1
this.cb_include=create cb_include
this.cb_cancel=create cb_cancel
this.cb_find=create cb_find
this.dw_update_selcd_sched=create dw_update_selcd_sched
this.uo_progress=create uo_progress
this.cb_print=create cb_print
this.gb_progress=create gb_progress
this.cbx_import=create cbx_import
this.st_include=create st_include
this.cb_file=create cb_file
this.cb_create=create cb_create
this.cb_import=create cb_import
this.dw_no_master=create dw_no_master
this.dw_distribution_schedule=create dw_distribution_schedule
this.dw_import_cp_allotbatch=create dw_import_cp_allotbatch
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.cb_deselect
this.Control[iCurrent+2]=this.dw_bklist
this.Control[iCurrent+3]=this.dw_dist
this.Control[iCurrent+4]=this.st_dist
this.Control[iCurrent+5]=this.st_file
this.Control[iCurrent+6]=this.st_imp
this.Control[iCurrent+7]=this.dw_batch_process
this.Control[iCurrent+8]=this.cbx_distsched
this.Control[iCurrent+9]=this.cbx_file
this.Control[iCurrent+10]=this.cbx_imp
this.Control[iCurrent+11]=this.dw_dsdtdsflag
this.Control[iCurrent+12]=this.dw_import_cp_allotbatch_sql
this.Control[iCurrent+13]=this.st_insertupdate
this.Control[iCurrent+14]=this.rb_batch
this.Control[iCurrent+15]=this.rb_include
this.Control[iCurrent+16]=this.st_bkorrow
this.Control[iCurrent+17]=this.sle_book#
this.Control[iCurrent+18]=this.cb_new
this.Control[iCurrent+19]=this.cbx_include
this.Control[iCurrent+20]=this.em_schdate
this.Control[iCurrent+21]=this.st_1
this.Control[iCurrent+22]=this.cb_include
this.Control[iCurrent+23]=this.cb_cancel
this.Control[iCurrent+24]=this.cb_find
this.Control[iCurrent+25]=this.dw_update_selcd_sched
this.Control[iCurrent+26]=this.uo_progress
this.Control[iCurrent+27]=this.cb_print
this.Control[iCurrent+28]=this.gb_progress
this.Control[iCurrent+29]=this.cbx_import
this.Control[iCurrent+30]=this.st_include
this.Control[iCurrent+31]=this.cb_file
this.Control[iCurrent+32]=this.cb_create
this.Control[iCurrent+33]=this.cb_import
this.Control[iCurrent+34]=this.dw_no_master
this.Control[iCurrent+35]=this.dw_distribution_schedule
this.Control[iCurrent+36]=this.dw_import_cp_allotbatch
end on

on w_special_distribution_schedule.destroy
call super::destroy
destroy(this.cb_deselect)
destroy(this.dw_bklist)
destroy(this.dw_dist)
destroy(this.st_dist)
destroy(this.st_file)
destroy(this.st_imp)
destroy(this.dw_batch_process)
destroy(this.cbx_distsched)
destroy(this.cbx_file)
destroy(this.cbx_imp)
destroy(this.dw_dsdtdsflag)
destroy(this.dw_import_cp_allotbatch_sql)
destroy(this.st_insertupdate)
destroy(this.rb_batch)
destroy(this.rb_include)
destroy(this.st_bkorrow)
destroy(this.sle_book#)
destroy(this.cb_new)
destroy(this.cbx_include)
destroy(this.em_schdate)
destroy(this.st_1)
destroy(this.cb_include)
destroy(this.cb_cancel)
destroy(this.cb_find)
destroy(this.dw_update_selcd_sched)
destroy(this.uo_progress)
destroy(this.cb_print)
destroy(this.gb_progress)
destroy(this.cbx_import)
destroy(this.st_include)
destroy(this.cb_file)
destroy(this.cb_create)
destroy(this.cb_import)
destroy(this.dw_no_master)
destroy(this.dw_distribution_schedule)
destroy(this.dw_import_cp_allotbatch)
end on

event closequery;////////////////////////////////////////////////////////////////////////////////
////
////	Event:  closequery
////
////	Description:
////	Search for unsaved datawindows prompting the user if any
////	pending updates are found.
////
////////////////////////////////////////////////////////////////////////////////
////	
////	Revision History
////
////	Version
////	5.0   Initial version
////
////////////////////////////////////////////////////////////////////////////////
////
////	Copyright © 1996-1997 Sybase, Inc. and its subsidiaries.  All rights reserved.
////	Any distribution of the PowerBuilder Foundation Classes (PFC)
////	source code by other than Sybase, Inc. and its subsidiaries is prohibited.
////
////////////////////////////////////////////////////////////////////////////////
//
//Integer	li_pendingrc
//Integer	li_validationrc
//Integer	li_accepttextrc
//Integer	li_msg
//Integer	li_rc,rtn
//String	ls_msgparms[]
//
//// Check if the CloseQuery process has been disabled
//If ib_disableclosequery Then
//	Return 0
//End If
//
//// Call event to perform any pre-CloseQuery processing
//If This.Event pfc_preclose ( ) <> 1 Then
//	// Prevent the window from closing
//	Return 1  
//End If
//
//// Prevent validation error messages from appearing while the window is closing
//// and allow others to check if the  CloseQuery process is in progress
//ib_closestatus = True
//
//// Check for any pending updates
//li_rc = of_UpdateChecks()
//If li_rc = 0 Then
//	// Updates are NOT pending, allow the window to be closed.
//	Return 0
//ElseIf li_rc < 0 Then
//	// There are Updates pending, but at least one data entry error was found.
//	// Give the user an opportunity to close the window without saving changes
//	If IsValid(gnv_app.inv_error) Then
//		li_msg = gnv_app.inv_error.of_Message('pfc_closequery_failsvalidation', &
//					 ls_msgparms, gnv_app.iapp_object.DisplayName)
//	Else
//		li_msg = MessageBox (gnv_app.iapp_object.DisplayName, &
//					"The information entered does not pass validation and "  + &
//					"must be corrected before changes can be saved.~r~n~r~n" + &
//					"Close without saving changes?", &
//					exclamation!, YesNo!, 2)
//					dw_select_deselect.Setfocus()
//	End If
//	If li_msg = 1 Then
//		Return 0
//	End If
//Else
//	// Changes are pending, prompt the user to determine if they should be saved
//	If IsValid(gnv_app.inv_error) Then
//		li_msg = gnv_app.inv_error.of_Message('pfc_closequery_savechanges',  &
//					ls_msgparms, gnv_app.iapp_object.DisplayName)		
//	Else
//		li_msg = MessageBox ( gnv_app.iapp_object.DisplayName, &
//					"Do you want to save changes?", exclamation!, YesNoCancel!)
//	End If
//	Choose Case li_msg
//		Case 1
//			// YES - Update
//			// If the update fails, prevent the window from closing
//			rtn = cb_update.Event Clicked()
//			IF rtn = 1 THEN
//				RETURN 0
//			END IF
////			If This.Event pfc_save() >= 1 Then
////				// Successful update, allow the window to be closed
////				Return 0
////			End If
//		Case 2
//			// NO - Allow the window to be closed without saving changes
//			Return 0
//		Case 3
//			dw_select_deselect.Setfocus()
//			// CANCEL -  Prevent the window from closing
//	End Choose
//End If
//
//// Prevent the window from closing
//ib_closestatus = False
//Return 1
end event

event mousemove;call super::mousemove;w_pics_main.setmicrohelp("Ready")
end event

event open;call super::open;
THIS.Windowstate = maximized!


end event

event pfc_postopen;call super::pfc_postopen;long i,li_row_count
date ld_today
string ls_today,ls_incd
m_pics_main.m_Edit.m_Addrow.Enabled = FALSE
m_pics_main.m_Edit.m_Deleterow.Enabled = FALSE
m_pics_main.m_Edit.m_Cut.Enabled = FALSE

ld_today= today()
ls_today=string(ld_today,'mm/dd/yyyy')
em_schdate.text= ls_today
cbx_include.checked=false
cbx_import.checked= true
dw_distribution_schedule.settransobject(sqlservertrans)
cb_cancel.visible=true
cb_find.visible=true
cb_new.enabled=true
cb_import.visible=true
cb_file.visible=false
cb_include.visible=false
cb_print.visible=false
st_bkorrow.visible=false
sle_book#.visible=false
uo_progress.visible=false
dw_import_cp_allotbatch.visible=true
dw_distribution_schedule.visible=false
dw_no_master.visible=false
dw_import_cp_allotbatch.object.batch_libcd_t.visible=false
dw_import_cp_allotbatch.object.t_1.visible=false
dw_import_cp_allotbatch.object.books_bkno_t.visible=false
dw_import_cp_allotbatch.object.batch_selqty_t.visible=false
cbx_imp.enabled=false
cbx_imp.checked=false
cbx_file.enabled=false
cbx_file.checked=false
cbx_distsched.enabled=false
cbx_distsched.checked=false
cb_create.visible=false

//dw_import_cp_allotbatch.object.t_defqty.visible=false
//wf_intrests_cpt()
SetPointer(Arrow!)
end event

event pfc_preopen;call super::pfc_preopen;this.of_SetPreference(TRUE)
this.inv_preference.of_SetToolBars(TRUE)
this.inv_preference.of_SetWindow(TRUE)
this.of_SetResize(TRUE)
this.inv_resize.of_SetOrigSize(this.workspacewidth(),this.workspaceheight())
inv_resize.of_Register(cb_cancel, "scale")
inv_resize.of_Register(cb_file, "scale")
inv_resize.of_Register(cb_find, "scale")
inv_resize.of_Register(cb_include, "scale")
inv_resize.of_Register(cb_new, "scale")
inv_resize.of_Register(cb_print, "scale")
inv_resize.of_Register(dw_distribution_schedule, "scale")
inv_resize.of_Register(gb_progress, "scale")
inv_resize.of_Register(dw_no_master, "scale")
inv_resize.of_Register(dw_import_cp_allotbatch, "scale")
inv_resize.of_Register(cbx_import, "scale")
inv_resize.of_Register(cbx_include, "scale")
inv_resize.of_Register(cbx_distsched, "scale")
inv_resize.of_Register(cbx_file, "scale")
inv_resize.of_Register(cbx_imp, "scale")
inv_resize.of_Register(st_1, "scale")
inv_resize.of_Register(st_include, "scale")
inv_resize.of_Register(em_schdate, "scale")
inv_resize.of_Register(st_bkorrow, "scale")
inv_resize.of_Register(sle_book#, "scale")
inv_resize.of_Register(dw_batch_process, "scale")
inv_resize.of_Register(rb_include, "scale")
inv_resize.of_Register(rb_batch, "scale")
inv_resize.of_Register(dw_update_selcd_sched, "scale")
inv_resize.of_Register(uo_progress, "scale")
inv_resize.of_Register(cb_import, "scale")
inv_resize.of_Register(st_insertupdate, "scale")
inv_resize.of_Register(st_imp, "scale")
inv_resize.of_Register(st_file, "scale")
inv_resize.of_Register(st_dist, "scale")
inv_resize.of_Register(cb_create, "scale")
inv_resize.of_Register(cb_deselect, "scale")














end event

event resize;call super::resize;//long ll_height
//
//This.X = w_pics_main.X
//This.Y = w_pics_main.Y
//ll_height = w_pics_main.mdi_1.Height
//This.Resize(w_pics_main.WorkSpaceWidth(), ll_height)
end event

type cb_deselect from u_cb within w_special_distribution_schedule
string tag = "Deselects all the menu items"
integer x = 1792
integer y = 1152
integer width = 402
integer height = 64
integer taborder = 30
integer textsize = -10
string text = "Unselect All"
end type

event clicked;call super::clicked;Integer li_loop, li_maxrows

//This will enable all the items in the datawindow
IF dw_no_master.visible = TRUE THEN
	li_maxrows = dw_no_master.RowCount()
	IF cb_deselect.text = 'Unselect All' THEN
		For li_loop = 1 to li_maxrows
			dw_no_master.object.include1[li_loop]='N'
		NEXT
		cb_deselect.text ='Select All'
		sle_book#.text = ""
	ELSEIF cb_deselect.text = 'Select All' THEN
		For li_loop = 1 to li_maxrows
			dw_no_master.object.include1[li_loop]='Y'
		NEXT
		cb_deselect.text ='Unselect All'
		sle_book#.text = string(li_maxrows)
	END IF		
ELSEIF dw_distribution_schedule.visible = TRUE THEN
	li_maxrows = dw_distribution_schedule.RowCount()
	IF cb_deselect.text = 'Unselect All' THEN
		For li_loop = 1 to li_maxrows
			dw_distribution_schedule.object.include1[li_loop]='N'
		NEXT
		cb_deselect.text ='Select All'
		sle_book#.text = ""
	ELSEIF cb_deselect.text = 'Select All' THEN
		For li_loop = 1 to li_maxrows
			dw_distribution_schedule.object.include1[li_loop]='Y'
		NEXT
		cb_deselect.text ='Unselect All'
		sle_book#.text = string(li_maxrows)
	END IF		
END IF
end event

type dw_bklist from u_pics_dw within w_special_distribution_schedule
boolean visible = false
integer x = 882
integer y = 124
integer width = 78
integer height = 76
integer taborder = 30
string dataobject = "d_bklist"
end type

type dw_dist from u_pics_dw within w_special_distribution_schedule
boolean visible = false
integer x = 951
integer y = 32
integer width = 78
integer height = 76
integer taborder = 20
string dataobject = "d_dist"
end type

type st_dist from statictext within w_special_distribution_schedule
boolean visible = false
integer x = 1801
integer y = 192
integer width = 864
integer height = 52
integer textsize = -8
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 33554432
long backcolor = 80263581
boolean focusrectangle = false
end type

type st_file from statictext within w_special_distribution_schedule
boolean visible = false
integer x = 1801
integer y = 116
integer width = 864
integer height = 52
integer textsize = -8
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 33554432
long backcolor = 80263581
boolean focusrectangle = false
end type

type st_imp from statictext within w_special_distribution_schedule
boolean visible = false
integer x = 1801
integer y = 44
integer width = 864
integer height = 52
integer textsize = -8
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 33554432
long backcolor = 80263581
boolean focusrectangle = false
end type

type dw_batch_process from u_pics_dw within w_special_distribution_schedule
boolean visible = false
integer x = 1234
integer y = 124
integer width = 78
integer height = 76
integer taborder = 20
string dataobject = "d_batch_process"
end type

type cbx_distsched from u_cbx within w_special_distribution_schedule
boolean visible = false
integer x = 997
integer y = 124
integer width = 288
integer height = 68
string text = "distsched "
end type

type cbx_file from u_cbx within w_special_distribution_schedule
boolean visible = false
integer x = 997
integer y = 64
integer width = 315
integer height = 72
string text = "create file"
end type

type cbx_imp from u_cbx within w_special_distribution_schedule
boolean visible = false
integer x = 997
integer width = 233
integer height = 68
string text = "import"
end type

type dw_dsdtdsflag from u_pics_dw within w_special_distribution_schedule
boolean visible = false
integer x = 1239
integer y = 28
integer width = 78
integer height = 76
integer taborder = 40
string dataobject = "d_ds_dist_qnty_update_mchar_dsdtdsflag"
end type

type dw_import_cp_allotbatch_sql from u_pics_dw within w_special_distribution_schedule
boolean visible = false
integer x = 1143
integer y = 32
integer width = 78
integer height = 76
integer taborder = 10
string dataobject = "d_import_cp_allotbatch_sql"
end type

type st_insertupdate from statictext within w_special_distribution_schedule
integer x = 1376
integer y = 1144
integer width = 530
integer height = 92
integer textsize = -8
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 33554432
long backcolor = 67108864
boolean focusrectangle = false
end type

type rb_batch from u_rb within w_special_distribution_schedule
boolean visible = false
integer x = 2459
integer y = 1148
integer width = 197
integer height = 68
string text = "batch"
end type

event clicked;call super::clicked;dw_distribution_schedule.visible=false

dw_import_cp_allotbatch.visible=true

end event

type rb_include from u_rb within w_special_distribution_schedule
boolean visible = false
integer x = 2455
integer y = 1140
integer width = 178
integer height = 68
string text = "icd"
end type

event clicked;call super::clicked;dw_distribution_schedule.visible=true

dw_import_cp_allotbatch.visible=false

end event

type st_bkorrow from statictext within w_special_distribution_schedule
integer x = 14
integer y = 1148
integer width = 498
integer height = 52
integer textsize = -8
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 33554432
long backcolor = 67108864
string text = "Total Books Included:"
alignment alignment = right!
boolean focusrectangle = false
end type

type sle_book# from u_sle within w_special_distribution_schedule
integer x = 576
integer y = 1144
integer width = 265
integer height = 76
integer taborder = 0
end type

type cb_new from u_cb within w_special_distribution_schedule
integer x = 2235
integer y = 1248
integer width = 174
integer taborder = 90
fontcharset fontcharset = ansi!
string text = "&New"
end type

event clicked;call super::clicked;//////////////////////////////////////////////////////////////////////////////////////////////////////////
//
//	Event: clicked for cb_new
//
//	Description:
//	Batch needs to be open to do schedule, if closed can do only for next batch.
//
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//	
//	Revision History
//	Developed by 	Date 				Version						Tracking#
//									
// Murali K.			04/08/2008		Validate against batch_data for closed scenario
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//


Date ld_today, ld_null
String ls_today,ls_include, ls_bkmed, ls_prodstg,ls_cntr,ls_prodstgold,ls_cntrold,&
		ls_bkmedold, ls_conno, ls_connoold, ls_null, ls_bkno[], ls_bk#, ls_bk#old,&
		ls_libcd, ls_filter, ls_cascd, ls_bkseq, ls_30, ls_nearcabdt, ls_m, ls_d, &
		ls_yr, ls_del
		
Long li_row_count, i, li_count, li_bkseq,li_bkseqold, li_cur,J, li_selqty,li_row,&
		li_re, li_curmchar, li_count2, li_m,li_yr, li_rowmax, k
Date ld_dsdt, ld_assigndt,ld_assigndtold,ld_cabdt, ld_cabdtold, ld_30, ld_nearcabdt,&
	 ld_max=Date('01/01/1900')
	
str_distrib_schedule lstr
String ls_ok_cancel
Boolean lb_skip=FALSE, lb_exit=FALSE
DateTime ldt_distfile, ldt_distsched, ldt_batchimp, ld_nearcabdt_dt, ld_dsdt_dt

SetPointer(hourglass!)
SetNull(ld_null)
ld_today= Today()
ld_30= RelativeDate(ld_today, - 30)
ls_30= String(ld_30,'mm/dd/yyyy')
ls_d=Mid(ls_30,4,2)
ls_m=Left(ls_30,2)
ls_yr=Right(ls_30,4)
li_yr=Long(ls_yr)
li_m=Long(ls_m)
IF ls_d>'15' THEN
	li_m= li_m +1
	IF li_m>12 THEN
		li_yr= li_yr + 1
		li_m= 1
	END IF
END IF
ls_yr=String(li_yr)
ls_m=String(li_m)
ls_nearcabdt=ls_m+'/01/'+ls_yr
ld_nearcabdt=Date(ls_nearcabdt)
ld_nearcabdt_dt=DateTime(ld_nearcabdt,Time('00:00:00'))
SELECT batchimp INTO :ldt_batchimp
FROM copyallot
where cabdt=:ld_nearcabdt_dt
USING SqlServerTrans;
IF NOT f_check_dberror(SqlServerTrans, 'select batchimp from copyallot') THEN
	RETURN
END IF
//if IsNull(ldt_batchimp)= true and cb_new.text='&New' then
//	li_re=messagebox('Warning','The copy allotment selection quantities have not been '+&
//		'~nimported for the given batch. The distribution schedule would be created'+&
//		'~nwith default quantities. Would you like to proceed?',Exclamation!,YesNo!,2)
//	if li_re=2 then return
//end if

/// 04/08/2008 Can add rush books only if the current batch is open
setnull(ld_cabdt)
select cabdt
into :ld_cabdt
from batch_data@pic_link
where batch_status_code <> 'C'  and cabdt = (select max(cabdt) from batch_data@pic_link ) using sqlservertrans ;

IF Isnull(ld_cabdt) THEN
	Messagebox("", "Batch is closed, Cannot  create new schedule")
	RETURN
END IF
////////////////////////


ld_dsdt=Date(em_schdate.text)
ld_dsdt_dt=DateTime(ld_dsdt,Time('00:00:00'))
dw_dist.SetTransObject(SqlServerTrans)
li_count=dw_dist.Retrieve(ld_dsdt_dt)
IF li_count>0 THEN
	ldt_distfile=dw_dist.object.distfile[1]
	ldt_distsched=dw_dist.object.distsched[1]
ELSEIF li_count=0 THEN
	dw_dist.InsertRow(0)
	dw_dist.object.dsdt[1]= ld_dsdt_dt
	SetNull(ldt_distfile)
	SetNull(ldt_distsched)
END IF
//following job is that: after you have finish batch formation now check what step you are in the
//distribution schedule

//if IsNull(ldt_batchimp)=false then
//	cbx_imp.enabled=true
//	cbx_imp.checked=true// check the batch import step
//end if
IF IsNull(ldt_distfile)=FALSE THEN
	cbx_file.enabled=TRUE
	cbx_file.checked=TRUE
END IF// check the distfile step
IF IsNull(ldt_distsched)=FALSE THEN
	cbx_distsched.enabled=TRUE
	cbx_distsched.checked=TRUE
END IF// check the last step i.e. insert rows into distsched in oracle		
//em_schdate.text=''
sle_book#.text=''
IF cbx_include.checked=TRUE THEN
	IF cb_new.text='&Clear' THEN
		dw_distribution_schedule.SetTransObject(SqlServerTrans)
		dw_distribution_schedule.Reset()
		li_row_count=dw_distribution_schedule.RowCount()
		dw_no_master.Reset()
		dw_import_cp_allotbatch.visible=FALSE
		dw_distribution_schedule.visible=TRUE
		dw_distribution_schedule.object.t_bkno.visible=FALSE
		dw_distribution_schedule.object.t_cntr.visible=FALSE
		dw_distribution_schedule.object.t_cabdt.visible=FALSE
		dw_distribution_schedule.object.t_dupcode.visible=FALSE
		dw_distribution_schedule.object.t_assigndt.visible=FALSE
		dw_distribution_schedule.object.t_repeat.visible=FALSE
		dw_distribution_schedule.object.t_include.visible=FALSE
		dw_no_master.visible=FALSE
//		sle_book#.text=string(li_row_count)
		sle_book#.visible=FALSE
		st_bkorrow.visible=FALSE
		st_insertupdate.text=''
		cb_find.visible=TRUE
		em_schdate.SetFocus()
		cb_new.text='&New'
//		cb_import.visible=true
		cb_file.visible=FALSE
		cb_include.visible=FALSE
		cb_print.visible=FALSE
//		cbx_import.visible=true
		cbx_include.visible=TRUE
		RETURN
	END IF
ELSE //cbx_include.checked=false
	IF cb_new.text='&Clear' THEN
		dw_no_master.Reset()
		dw_distribution_schedule.SetTransObject(SqlServerTrans)
		dw_distribution_schedule.Reset()
		li_row_count=dw_no_master.RowCount()
		dw_import_cp_allotbatch.visible=FALSE
		dw_distribution_schedule.visible=FALSE
		dw_no_master.visible=TRUE
		dw_no_master.object.t_bkno.visible=FALSE
		dw_no_master.object.t_cntr.visible=FALSE
		dw_no_master.object.t_cabdt.visible=FALSE
		dw_no_master.object.t_dupcode.visible=FALSE
		dw_no_master.object.t_assigndt.visible=FALSE
		dw_no_master.object.t_repeat.visible=FALSE
		dw_no_master.object.t_include.visible=FALSE
		sle_book#.visible=FALSE
		st_bkorrow.visible=FALSE
		st_insertupdate.text=''
//		sle_book#.text=string(li_row_count)
		cb_find.visible=TRUE
		em_schdate.SetFocus()
		cb_new.text='&New'
//		cb_import.visible=true
		cb_file.visible=FALSE
		cb_include.visible=FALSE
		cb_print.visible=FALSE
//		cbx_import.visible=true
		cbx_include.visible=TRUE
		RETURN
	END IF
END IF
IF cb_new.text='&New' THEN
	dw_import_cp_allotbatch.visible=FALSE
	cb_import.visible=FALSE
	cb_file.visible=TRUE
	//the function is main horse to determine the different rows will be retrieved in the
	//two different case. one is cbx_include checked; two is cbx_include is unchecked.
	//when cbx_include.checked=false you will get rid off the rows where cntr2 is null.
	//so you can see different row will show in different case
	wf_distrib_sched_simple()
	st_bkorrow.visible=TRUE
	st_bkorrow.text='Total Books Included:'
	sle_book#.visible=TRUE
	uo_progress.visible=FALSE
	st_insertupdate.text=''
	IF dw_distribution_schedule.visible=TRUE THEN
		li_count=dw_distribution_schedule.RowCount()
		sle_book#.text=String(li_count)
	ELSEIF dw_no_master.visible=TRUE THEN
		li_count=dw_no_master.RowCount()
		sle_book#.text=String(li_count)
	END IF// end of dw_distribution_schedule.visible=true
END IF// end of cb_new.text='&New'
			



SetPointer(arrow!)
end event

type cbx_include from u_cbx within w_special_distribution_schedule
integer x = 2203
integer y = 52
integer width = 87
integer height = 68
integer taborder = 30
string text = ""
boolean lefttext = true
end type

event clicked;call super::clicked;string  ls_bkmed, ls_cntr1,ls_cntr2, ls_repeat, ls_include1, ls_today, &
	ls_null, ls_prodstgold, ls_cntrtype, ls_del, ls_bkmedold
date ld_today, ld_cabdt,ld_assigndt,ld_cabdtold,ld_assigndtold,ld_null, ld_max
datetime ld_cabdt_dt, ld_assigndt_dt, ld_max_dt
long i,li_bkseq,li_cur, li_row_count,li_bkseqold, li_re, li_bound, li, li_count,k, j,li_cnt
	
if cb_new.text='&Clear' then
	dw_distribution_schedule.settransobject(sqlservertrans)
	li_cnt=dw_distribution_schedule.Retrieve()
	j=0
	ls_bkmedold=''
	li_bkseqold=0
	// the datawindow difinition hand self join of prod considered if cntr1=cntr2 case
	// this already considered cntrtype in ('T','M') case but there is cntr1<>cntr2 case
	// ie. one book associate two cntrtype one is 'M', the other is 'D' each one just one row
	// here also consider one book have 3 rows in prod table for example 'M' type have two prodstage
	// and cntr1=cntr2 and have 3rd row is 'D' type these 3 rows must written 1 row. datawidow olse
	// consider one book have 4 rows case have double 'T' cntrtype each has 2 rows.
	// use select statement to analyze prod rows group by bkmed,bkseq and count(*) each group have
	// how many rows, you have 4 rows case, 3 rows case, 2 rows case ( cntr1<>cntr2 or cntr1=cntr2)
	// very complicate, this first retrieve don't delete one 1 row mastering book.
	// keep in mind one book only one row, choose greatest assigndt and associated cntr2
	for i=1 to li_cnt
		li_bkseq=dw_distribution_schedule.object.bkseq[i]
		ls_bkmed=dw_distribution_schedule.object.bkmed[i]
		if li_bkseq=49342 or li_bkseq=50551 or li_bkseq=50559 then
			li_re=1000
		end if
		if li_bkseq<>li_bkseqold or ls_bkmed<>ls_bkmedold then
//			j=1
			ls_bkmedold=ls_bkmed
			li_bkseqold=li_bkseq
			ld_max=date('01/01/1900')
			ld_max_dt=datetime(ld_max,time('00:00:00'))
			if j>1 then
				for k=1 to j
					ls_cntr1=dw_distribution_schedule.object.cntr1[i+k - j - 1 ]
					ls_cntr2=dw_distribution_schedule.object.cntr2[i+k - j - 1 ]
					ld_assigndt_dt=dw_distribution_schedule.object.assigndt[i+k - j - 1 ]
					if ld_assigndt_dt>ld_max_dt then
						ld_max_dt=ld_assigndt_dt
					end if
					if trim(ls_cntr1)<>'' and trim(ls_cntr2)<>'' then
						dw_distribution_schedule.object.del[i+k - j - 1 ]='N'
					elseif trim(ls_cntr1)='' and j>=2 then
						dw_distribution_schedule.object.del[i+k - j - 1 ]='Y'
					elseif trim(ls_cntr2)='' and j>=2 then
						dw_distribution_schedule.object.del[i+k - j - 1 ]='Y'
					end if
				next
				for k=1 to j
					ld_assigndt_dt=dw_distribution_schedule.object.assigndt[i+k - j - 1 ]
					if ld_assigndt_dt=ld_max_dt then
						ls_cntr2=dw_distribution_schedule.object.cntr2[i+k - j - 1 ]
					end if
				next
				for k=1 to j
					ls_del=dw_distribution_schedule.object.del[i+k - j - 1 ]
					if ls_del='N' then
						dw_distribution_schedule.object.cntr2[i+k - j - 1 ]=ls_cntr2
						dw_distribution_schedule.object.assigndt[i+k - j - 1 ]=ld_max_dt
					end if
				next
			end if// if j>1
			j=1 //remember only here set j=1
		else	
			j++
		end if //end if ls_bkmed<>ls_bkmedold
	next
	for i=1 to li_cnt
		li_bkseq=dw_distribution_schedule.object.bkseq[li_cnt+1 - i]
		ls_bkmed=dw_distribution_schedule.object.bkmed[li_cnt+1 - i]
		if li_bkseq=49342 or li_bkseq=50551 or li_bkseq=50559 then
			li_re=1000
		end if
		ls_del=dw_distribution_schedule.object.del[li_cnt+1 - i]
		if ls_del='Y' then
			dw_distribution_schedule.deleteRow(li_cnt+1 - i)
		end if
	next
	li_cnt=dw_distribution_schedule.Rowcount()
end if
li_bound=UpperBound(ii_bkseq[])
if cbx_include.checked=true then
	dw_import_cp_allotbatch.visible=false
	dw_no_master.visible=false
	dw_distribution_schedule.visible=true
	if cb_new.text='&New' then
		dw_distribution_schedule.Reset()
		dw_distribution_schedule.object.t_bkno.visible=false
		dw_distribution_schedule.object.t_cntr.visible=false
		dw_distribution_schedule.object.t_cabdt.visible=false
		dw_distribution_schedule.object.t_dupcode.visible= false
		dw_distribution_schedule.object.t_assigndt.visible=false
		dw_distribution_schedule.object.t_repeat.visible=false
		dw_distribution_schedule.object.t_include.visible=false
	else //'&Clear'
//		dw_distribution_schedule.Retrieve()
		if ib_web_done= false  and li_bound >0 then
			for i=1 to li_bound
				li_cur=dw_distribution_schedule.InsertRow(0)// add every one in the specific list
				//books, since every ls_cntr2<>null
				dw_distribution_schedule.object.bkmed[li_cur]=is_bkmed[i]
				dw_distribution_schedule.object.bkseq[li_cur]=ii_bkseq[i]
				dw_distribution_schedule.object.cntr1[li_cur]=is_cntr1[i]
				dw_distribution_schedule.object.cntr2[li_cur]=is_cntr2[i]
				dw_distribution_schedule.object.cabdt[li_cur]=id_cabdt[i]
				dw_distribution_schedule.object.assigndt[li_cur]=id_assigndt[i]
				dw_distribution_schedule.object.repeat[li_cur]='Y'
				dw_distribution_schedule.object.include1[li_cur]='Y'
			next
		end if
		dw_distribution_schedule.Sort()
		li_row_count=dw_distribution_schedule.RowCount()
		sle_book#.text=string(li_row_count)
		cb_cancel.visible=true
		if li_row_count >0 then
			dw_distribution_schedule.object.t_bkno.visible=true
			dw_distribution_schedule.object.t_cntr.visible=true
			dw_distribution_schedule.object.t_cabdt.visible=true
			dw_distribution_schedule.object.t_dupcode.visible=true
			dw_distribution_schedule.object.t_assigndt.visible=true
			dw_distribution_schedule.object.t_repeat.visible=true
			dw_distribution_schedule.object.t_include.visible=true
		else
			dw_distribution_schedule.object.t_bkno.visible=false
			dw_distribution_schedule.object.t_cntr.visible=false
			dw_distribution_schedule.object.t_cabdt.visible=false
			dw_distribution_schedule.object.t_dupcode.visible= false
			dw_distribution_schedule.object.t_assigndt.visible=false
			dw_distribution_schedule.object.t_repeat.visible=false
			dw_distribution_schedule.object.t_include.visible=false
		end if
	end if//end if cb_new.text='&New'
	li_row_count=dw_distribution_schedule.RowCount()
	sle_book#.text=string(li_row_count)
else// cbx_include.checked =false
	dw_no_master.Reset()
	dw_no_master.visible=true
	dw_distribution_schedule.visible=false
	dw_import_cp_allotbatch.visible=false
	if cb_new.text='&New' then
		dw_no_master.Reset()
		dw_no_master.object.t_bkno.visible=false
		dw_no_master.object.t_cntr.visible=false
		dw_no_master.object.t_cabdt.visible=false
		dw_no_master.object.t_dupcode.visible= false
		dw_no_master.object.t_assigndt.visible=false
		dw_no_master.object.t_repeat.visible=false
		dw_no_master.object.t_include.visible=false
	elseif cb_new.text='&Clear' then
		cb_find.visible=false
		cb_file.visible=true
//		cb_include.visible=true
		li_row_count=dw_distribution_schedule.RowCount()
		for i=1 to li_row_count
			ls_cntrtype=dw_distribution_schedule.object.cntrtype[i]
			ls_cntr2=dw_distribution_schedule.object.cntr2[i]
			ls_cntr2=trim(ls_cntr2)// only excluse ls_cntr2=null and cntrtype='M'
			// in the dw object when cntrtype='D' ls_cntr2 can be null, this record
			// should be included
			if (ls_cntr2='' or IsNull(ls_cntr2)) and ls_cntrtype<>'D' then continue
			ls_bkmed=dw_distribution_schedule.object.bkmed[i]
			li_bkseq=dw_distribution_schedule.object.bkseq[i]
			ls_cntr1=dw_distribution_schedule.object.cntr1[i]
			ls_cntr2=dw_distribution_schedule.object.cntr2[i]
			ld_cabdt_dt=dw_distribution_schedule.object.cabdt[i]
			ld_assigndt_dt=dw_distribution_schedule.object.assigndt[i]
			ls_repeat=dw_distribution_schedule.object.repeat[i]
			ls_include1=dw_distribution_schedule.object.include1[i]
			li_cur=dw_no_master.InsertRow(0)
			
			dw_no_master.object.bkmed[li_cur]= ls_bkmed
			dw_no_master.object.bkseq[li_cur]=li_bkseq
			dw_no_master.object.cntr1[li_cur]=ls_cntr1
			dw_no_master.object.cntr2[li_cur]= ls_cntr2
			dw_no_master.object.cabdt[li_cur]= ld_cabdt_dt
			dw_no_master.object.assigndt[li_cur]= ld_assigndt_dt
			dw_no_master.object.repeat[li_cur]= ls_repeat
			dw_no_master.object.include1[li_cur]=ls_include1
		next
		if ib_web_done= false  and li_bound >0 then
			for i=1 to li_bound
				ls_cntr2=is_cntr2[i]
				ls_cntr2=trim(ls_cntr2)
				// every one in the sepecific list is included, since we aready give the choice
				// in the list each ls_cntr2<>null now
//				if IsNull(ls_cntr2) and ls_cntr2='' then
//					li=messagebox('Include Books','This book has not yet been assigned to'+&
//					'a duplicator. Do you still wish to include this book?',Exclamation!,YesNo!,1)
//						
//					if li=2 then continue
//				end if
				li_cur=dw_no_master.InsertRow(0)
				dw_no_master.object.bkmed[li_cur]=is_bkmed[i]
				dw_no_master.object.bkseq[li_cur]=ii_bkseq[i]
				dw_no_master.object.cntr1[li_cur]=is_cntr1[i]
				dw_no_master.object.cntr2[li_cur]=is_cntr2[i]
				dw_no_master.object.cabdt[li_cur]=id_cabdt[i]
				dw_no_master.object.assigndt[li_cur]=id_assigndt[i]
				dw_no_master.object.repeat[li_cur]='Y'
				dw_no_master.object.include1[li_cur]='Y'
			next
		end if
		dw_no_master.Sort()
		li_row_count=dw_no_master.Rowcount()
		dw_no_master.visible=true
		dw_distribution_schedule.visible=false
		li_row_count=dw_no_master.RowCount()
		sle_book#.text=string(li_row_count)
		ls_today= string(ld_today,'mm/dd/yyyy')
		cb_cancel.visible=true
		if li_row_count >0 then
			dw_no_master.object.t_bkno.visible=true
			dw_no_master.object.t_cntr.visible=true
			dw_no_master.object.t_cabdt.visible=true
			dw_no_master.object.t_dupcode.visible=true
			dw_no_master.object.t_assigndt.visible=true
			dw_no_master.object.t_repeat.visible=true
			dw_no_master.object.t_include.visible=true
		else
			dw_no_master.object.t_bkno.visible=false
			dw_no_master.object.t_cntr.visible=false
			dw_no_master.object.t_cabdt.visible=false
			dw_no_master.object.t_dupcode.visible=false
			dw_no_master.object.t_assigndt.visible=false
			dw_no_master.object.t_repeat.visible=false
			dw_no_master.object.t_include.visible=false
		end if
	end if// end if cb_new.text='&New' then
	li_row_count=dw_no_master.RowCount()
	sle_book#.text=string(li_row_count)
end if// end if cbx_include.checked
if cb_new.text='&Clear' then
	if dw_distribution_schedule.visible=true then
		li_count=dw_distribution_schedule.RowCount()
		sle_book#.text=string(li_count)
	elseif dw_no_master.visible=true then
		li_count=dw_no_master.RowCount()
		sle_book#.text=string(li_count)
	end if// end of dw_distribution_schedule.visible=true
end if // end of cb_new.text

end event

type em_schdate from u_em within w_special_distribution_schedule
integer x = 571
integer y = 52
integer width = 325
integer height = 68
integer taborder = 20
maskdatatype maskdatatype = datemask!
string mask = "mm/dd/yyyy"
end type

type st_1 from statictext within w_special_distribution_schedule
integer x = 32
integer y = 52
integer width = 613
integer height = 64
integer textsize = -8
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 33554432
long backcolor = 67108864
string text = "Distribution Schedule Date:"
boolean focusrectangle = false
end type

type cb_include from u_cb within w_special_distribution_schedule
event pfc_hinttext pbm_mousemove
string tag = "Open respose window to get user input book no"
boolean visible = false
integer x = 23
integer y = 1248
integer width = 613
integer taborder = 60
fontcharset fontcharset = ansi!
string text = "&Include Specific Books..."
end type

event pfc_hinttext;//parent.Event pfc_close()
//m_pics_main.m_menu.PopMenu(300, 0)
//
end event

event clicked;call super::clicked;str_distrib_schedule lstr_add
String ls_bkmed, ls_repeat, ls_cntrmax,ls_cntrmin,ls_pdstgmax,ls_pdstgmin, ls_incld,&
		ls_null, ls_cntr, ls_conno, ls_cntr1, ls_cntr2
Long li_bkseq, i,li_cur, li_count,li_count2=0, li_row_count, li_bound, J=0, li
Date ld_cabdt, ld_assigndt
DateTime ld_cabdt_dt, ld_assigndt_dt

w_pics_main.SetMicroHelp(this.tag)
Open(w_response_specific_books)
lstr_add=message.powerObjectParm

IF lstr_add.arraylong[1]= -1 THEN
	is_add='N'
	RETURN
END IF
SetPointer(hourglass!)
SetNull(ls_null)

li_bound=UpperBound(lstr_add.arraylong[])
IF li_bound >0 THEN
	is_add='Y'
	istr_add=lstr_add
ELSE
	is_add='N'
	RETURN
END IF
FOR i=1 TO li_bound
	li_bkseq=lstr_add.arraylong[i]
	ls_bkmed=lstr_add.arraymed[i]
	ls_repeat=lstr_add.arrayrpt[i]
	IF li_bkseq>0 THEN 
		ls_cntr2=""
		SELECT cabdt, conno INTO :ld_cabdt_dt, :ls_conno
		FROM mchar
		where bkseq=:li_bkseq AND bkmed=:ls_bkmed
		USING SqlServerTrans;
		IF NOT f_check_dberror(SqlServerTrans,'select cabdt, conno from mchar '+&
																	'using bkseq,bkmed') THEN
			RETURN
		END IF
		SELECT cntr INTO :ls_cntr1
		FROM prod
		where bkseq=:li_bkseq AND bkmed=:ls_bkmed AND prodstage in ('MA','AB','PU')
		USING SqlServerTrans;
		IF NOT f_check_dberror(SqlServerTrans,'select ls_cntr1 mastering contract'+&
																'from prod using bkseq,bkmed') THEN														
			RETURN
		END IF
		SELECT cntr , assigndt INTO :ls_cntr2, :ld_assigndt_dt
		FROM prod
		where bkseq=:li_bkseq AND bkmed=:ls_bkmed AND prodstage in ('PR','PB','DU')
		USING SqlServerTrans;
		IF NOT f_check_dberror(SqlServerTrans,'select ls_cntr2 duplication contract'+&
																'from prod using bkseq,bkmed') THEN														
			RETURN
		END IF
		ls_cntr2=Trim(ls_cntr2)// if ls_cntr2=null then cntrtype='M';if cntrtype='D' then 
		// ls_cntr2<>null
		IF w_distribution_schedule.cbx_include.checked THEN
			IF IsNull(ls_cntr2) OR ls_cntr2="" THEN
				li=Messagebox('Include Books','This book has not yet been assigned to '+&
					'a duplicator. Do you still wish to include this book?',exclamation!,yesNo!,1)
				IF li=2 THEN CONTINUE
				ls_cntr2='OKOKOK'// so if included then ls_cntr2<>null now, if not included then
				// not in the list
			END IF
			li_cur=dw_distribution_schedule.InsertRow(0)
			dw_distribution_schedule.SetItem(li_cur,'conno',ls_conno)
			dw_distribution_schedule.SetItem(li_cur,'bkseq',li_bkseq)
			dw_distribution_schedule.SetItem(li_cur,'bkmed',ls_bkmed)
			dw_distribution_schedule.SetItem(li_cur,'repeat',ls_repeat)
			dw_distribution_schedule.SetItem(li_cur,'include1','Y')
			dw_distribution_schedule.SetItem(li_cur,'cabdt',ld_cabdt_dt)
			dw_distribution_schedule.SetItem(li_cur,'cntr1',ls_cntr1)
			dw_distribution_schedule.SetItem(li_cur,'cntr2',ls_cntr2)
			dw_distribution_schedule.SetItem(li_cur,'assigndt',ld_assigndt_dt)
		ELSE //cbx_include is not checked
		
			IF IsNull(ls_cntr2) OR ls_cntr2="" THEN
				li=Messagebox('Include Books','This book has not yet been assigned to '+&
					'a duplicator. Do you still wish to include this book?',exclamation!,yesNo!,1)
				IF li=2 THEN CONTINUE
				ls_cntr2='OKOKOK'// if included then ls_cntr2<>null else this record will jump off
				//obvious if cntrtype='D' then ls_cntr2<>null
			END IF
				
				li_cur=dw_no_master.InsertRow(0)
				dw_no_master.SetItem(li_cur,'conno',ls_conno)
				dw_no_master.SetItem(li_cur,'bkseq',li_bkseq)
				dw_no_master.SetItem(li_cur,'bkmed',ls_bkmed)
				dw_no_master.SetItem(li_cur,'repeat',ls_repeat)
				dw_no_master.SetItem(li_cur,'include1','Y')
				dw_no_master.SetItem(li_cur,'cabdt',ld_cabdt_dt)
				dw_no_master.SetItem(li_cur,'cntr1',ls_cntr1)
				dw_no_master.SetItem(li_cur,'cntr2',ls_cntr2)
				dw_no_master.SetItem(li_cur,'assigndt',ld_assigndt_dt)
		END IF//end of cbx_include.checked
	ELSE // if li_bkseq <=0
		EXIT
	END IF //end if li_bkseq>0
NEXT // for i=1 to li_bound
IF w_distribution_schedule.cbx_include.checked THEN
	dw_distribution_schedule.ScrollToRow(li_cur)
	dw_distribution_schedule.SetRow(li_cur)
	dw_distribution_schedule.SetFocus()
	li_row_count=dw_distribution_schedule.RowCount()
	FOR i=1 TO li_row_count
		ls_incld=dw_distribution_schedule.GetItemString(i,'include1')
		IF ls_incld='Y' THEN li_count2++
	NEXT
	dw_distribution_schedule.Sort()
	li_row_count=dw_distribution_schedule.RowCount()
	FOR i=1 TO li_row_count
		ls_cntr2=dw_distribution_schedule.object.cntr2[i]
		ls_repeat=dw_distribution_schedule.object.repeat[i]
		IF ls_repeat='Y' AND IsNull(ls_cntr2)=FALSE THEN// this is included specific list books
			J++
			is_bkmed[J]=dw_distribution_schedule.object.bkmed[i]
			ii_bkseq[J]=dw_distribution_schedule.object.bkseq[i]
			is_cntr1[J]=dw_distribution_schedule.object.cntr1[i]
			is_cntr2[J]=dw_distribution_schedule.object.cntr2[i]
			id_cabdt[J]=dw_distribution_schedule.object.cabdt[i]
			id_assigndt[J]=dw_distribution_schedule.object.assigndt[i]
		END IF
	NEXT
ELSE//if cbx_include.checked=false
	dw_no_master.ScrollToRow(li_cur)
	dw_no_master.SetRow(li_cur)
	dw_no_master.SetFocus()
	li_row_count=dw_no_master.RowCount()
	FOR i=1 TO li_row_count
		ls_incld=dw_no_master.GetItemString(i,'include1')
		IF ls_incld='Y' THEN li_count2++
	NEXT
	dw_no_master.Sort()
	li_row_count=dw_no_master.RowCount()
	FOR i=1 TO li_row_count
		ls_cntr2=dw_no_master.object.cntr2[i]
		ls_repeat=dw_no_master.object.repeat[i]
		IF ls_repeat='Y' AND IsNull(ls_cntr2)=FALSE THEN// this is included specific list books
			J++
			is_bkmed[J]=dw_no_master.object.bkmed[i]
			ii_bkseq[J]=dw_no_master.object.bkseq[i]
			is_cntr1[J]=dw_no_master.object.cntr1[i]
			is_cntr2[J]=dw_no_master.object.cntr2[i]
			id_cabdt[J]=dw_no_master.object.cabdt[i]
			id_assigndt[J]=dw_no_master.object.assigndt[i]
		END IF
	NEXT
END IF

sle_book#.text=String(li_count2)
SetPointer(arrow!)
end event

type cb_cancel from u_cb within w_special_distribution_schedule
event pfc_hinttext pbm_mousemove
string tag = "Exits the screen"
integer x = 2432
integer y = 1248
integer width = 238
integer taborder = 100
fontcharset fontcharset = ansi!
string text = "&Cancel"
end type

event pfc_hinttext;call super::pfc_hinttext;w_pics_main.setmicrohelp(This.tag)
end event

event clicked;call super::clicked;parent.Event pfc_close()
m_pics_main.m_menu.PopMenu(300, 0)

end event

type cb_find from u_cb within w_special_distribution_schedule
integer x = 1792
integer y = 1248
integer width = 421
integer taborder = 80
fontcharset fontcharset = ansi!
string text = "&Find by DS date"
end type

event clicked;call super::clicked;Date ld_date, ld_cabdt, ld_assigndt
DateTime ld_date_dt, ld_cabdt_dt, ld_assigndt_dt
String ls_date,ls_dsflag, ls_cntr2, ls_bkmed, ls_cntr1,  ls_repeat, ls_include1,&
		ls_cntrtype
Long li_row_count, i, li_cur, li_bkseq

ls_date=em_schdate.text
IF (NOT IsDate(ls_date)) OR IsNull(ls_date) THEN
	Messagebox('Enter Date ','Please enter a Distribution Schedule Date.',stopSign!)
	em_schdate.SetFocus()
	RETURN
END IF
ld_date=Date(ls_date)
ld_date_dt=DateTime(ld_date,Time('00:00:00'))
//dw_distribution_schedule.visible=true
//dw_no_master.visible=false
dw_import_cp_allotbatch.visible=FALSE
dw_distribution_schedule.dataObject='d_distribution_schedule'
dw_distribution_schedule.SetTransObject(SqlServerTrans)
dw_distribution_schedule.Retrieve(ld_date_dt)
li_row_count=dw_distribution_schedule.RowCount()
IF li_row_count<=0 THEN
	Messagebox('No Data','No books found for this date. Please check the date.')
	em_schdate.SetFocus()
	cb_create.visible=FALSE
//	dw_distribution_schedule.object.t_bkno.visible=false
//	dw_distribution_schedule.object.t_cntr.visible=false
//	dw_distribution_schedule.object.t_cabdt.visible=false
//	dw_distribution_schedule.object.t_dupcode.visible=false
//	dw_distribution_schedule.object.t_assigndt.visible=false
//	dw_distribution_schedule.object.t_repeat.visible=false
//	dw_distribution_schedule.object.t_include.visible=false
	RETURN
END IF
FOR i=1 TO li_row_count
	ls_dsflag=dw_distribution_schedule.GetItemString(i,'dsflag')
	IF ls_dsflag='R' THEN
		dw_distribution_schedule.SetItem(i,'repeat','Y')
	END IF
NEXT
dw_no_master.Reset()
li_row_count=dw_distribution_schedule.RowCount()
FOR i=1 TO li_row_count	
	ls_cntrtype=dw_distribution_schedule.object.cntrtype[i]
	ls_cntr2=dw_distribution_schedule.object.cntr2[i]
	ls_cntr2=Trim(ls_cntr2)
	// cntrtype="D", ls_cntr2 can be null, must included. only get ride off ls_cntr2=null and
	// (cntrtype="M", or cntrtype='T') 
	IF (ls_cntr2='' OR IsNull(ls_cntr2)) AND ls_cntrtype<>'D' THEN CONTINUE
	ls_bkmed=dw_distribution_schedule.object.bkmed[i]
	li_bkseq=dw_distribution_schedule.object.bkseq[i]
	ls_cntr1=dw_distribution_schedule.object.cntr1[i]
	ls_cntr2=dw_distribution_schedule.object.cntr2[i]
	ld_cabdt_dt=dw_distribution_schedule.object.cabdt[i]
	ld_assigndt_dt=dw_distribution_schedule.object.assigndt[i]
	ls_repeat=dw_distribution_schedule.object.repeat[i]
	ls_include1=dw_distribution_schedule.object.include1[i]
	li_cur=dw_no_master.InsertRow(0)
	dw_no_master.object.bkmed[li_cur]= ls_bkmed
	dw_no_master.object.bkseq[li_cur]=li_bkseq
	dw_no_master.object.cntr1[li_cur]=ls_cntr1
	dw_no_master.object.cntr2[li_cur]= ls_cntr2
	dw_no_master.object.cabdt[li_cur]= ld_cabdt_dt
	dw_no_master.object.assigndt[li_cur]= ld_assigndt_dt
	dw_no_master.object.repeat[li_cur]= ls_repeat
	dw_no_master.object.include1[li_cur]=ls_include1
	dw_no_master.object.cntrtype[li_cur]=ls_cntrtype
NEXT
dw_no_master.visible=TRUE
dw_distribution_schedule.visible=FALSE
IF li_cur >0 THEN
	cb_create.visible=TRUE
ELSE
	cb_create.visible=FALSE
	dw_no_master.object.t_bkno.visible=FALSE
	dw_no_master.object.t_cntr.visible=FALSE
	dw_no_master.object.t_cabdt.visible=FALSE
	dw_no_master.object.t_dupcode.visible=FALSE
	dw_no_master.object.t_assigndt.visible=FALSE
	dw_no_master.object.t_repeat.visible=FALSE
	dw_no_master.object.t_include.visible=FALSE
	Messagebox('No Data','No books found for this date. Please check the date.')
	em_schdate.SetFocus()
	RETURN
END IF
sle_book#.text=String(li_cur)
st_bkorrow.visible=TRUE
st_bkorrow.text='Total books found:'
sle_book#.visible=TRUE
st_insertupdate.text=''
uo_progress.visible=FALSE
cb_cancel.visible=TRUE
cb_find.visible=FALSE
cb_new.text='&Clear'
cb_file.visible=FALSE
cb_include.visible= FALSE
//cb_print.visible=true
cb_import.visible=FALSE
cbx_import.visible=FALSE
cbx_include.visible=FALSE
st_include.visible=FALSE





end event

type dw_update_selcd_sched from u_dw within w_special_distribution_schedule
boolean visible = false
integer x = 1984
integer y = 140
integer width = 151
integer height = 48
integer taborder = 10
boolean bringtotop = true
string dataobject = "d_update_selcd_sched"
boolean vscrollbar = false
boolean border = false
boolean livescroll = false
borderstyle borderstyle = stylebox!
boolean ib_isupdateable = false
boolean ib_rmbmenu = false
boolean ib_rmbfocuschange = false
string is_updatesallowed = ""
end type

event sqlpreview;call super::sqlpreview;long li_row_count

i_count++
if uo_progress.visible=true then
	uo_progress.of_Increment(1)
end if
if mod(i_count,100)=0 then
	parent.SetRedraw(TRUE)
else
	parent.SetRedraw(false)
end if

end event

type uo_progress from u_progressbar within w_special_distribution_schedule
integer x = 896
integer y = 1140
integer width = 457
integer height = 88
integer taborder = 20
boolean bringtotop = true
boolean border = true
borderstyle borderstyle = stylelowered!
end type

on uo_progress.destroy
call u_progressbar::destroy
end on

type cb_print from u_cb within w_special_distribution_schedule
event pfc_hinttext pbm_mousemove
string tag = "Print the data"
boolean visible = false
integer x = 2560
integer y = 1140
integer width = 151
integer taborder = 50
boolean bringtotop = true
fontcharset fontcharset = ansi!
string text = "&Print"
end type

event pfc_hinttext;call super::pfc_hinttext;w_pics_main.setmicrohelp(This.tag)
end event

event clicked;call super::clicked;//parent.Event pfc_close()
//m_pics_main.m_menu.PopMenu(300, 0)
if dw_distribution_schedule.visible then
	dw_distribution_schedule.triggerEvent('pfc_print')
elseif dw_no_master.visible then
	dw_no_master.triggerEvent('pfc_print')
end if 

end event

type gb_progress from groupbox within w_special_distribution_schedule
boolean visible = false
integer x = 1070
integer y = 8
integer width = 1614
integer height = 268
integer taborder = 40
integer textsize = -8
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 33554432
long backcolor = 80263581
string text = "job progress"
end type

type cbx_import from u_cbx within w_special_distribution_schedule
boolean visible = false
integer x = 46
integer y = 136
integer width = 745
integer height = 68
boolean bringtotop = true
string text = "Import Copy Allotment Batch?   "
boolean lefttext = true
end type

type st_include from statictext within w_special_distribution_schedule
integer x = 1189
integer y = 52
integer width = 997
integer height = 56
integer textsize = -8
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 33554432
long backcolor = 80263581
string text = "Include Books without Duplication Assigned?"
boolean focusrectangle = false
end type

type cb_file from u_cb within w_special_distribution_schedule
event pfc_hinttext pbm_mousemove
string tag = "Create two file"
integer x = 1458
integer y = 1248
integer width = 306
integer taborder = 70
fontcharset fontcharset = ansi!
string text = "Cr&eate DS..."
end type

event pfc_hinttext;call super::pfc_hinttext;w_pics_main.setmicrohelp(This.tag)
end event

event clicked;call super::clicked;//////////////////////////////////////////////////////////////////////////////////////////////////////////
//
//	EVENT: clicked for cb_file
//  
//	Description: Before creating special distribution schedule check the quota
//	
//
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//	
//	Revision History
//
//	Developed by 	Date 				Version						Tracking#
//									
// Murali K.			06/23/2008      Phase-2
// Murali K.			10/29/2008 		Update order qty with selected or default qty before schedule creation
//Murali K.			10/30/2008 		update order qty with selected or default qty before 
//											schedule creation for distinct batch dates in the list
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//

Long li_row_count, li_count=0,i, J,li_bkseq, li_re, li_bound, li_cur, li_cur2
String ls_repeat, ls_cld,ls_date, ls_bkno_list[],ls_bkmed, ls_bkno, ls_dsflag,ls_cascd,&
		ls_rtn, ls_cntr2, ls_sum_bkno, ls_null, ls_cntrtype, ls_msg
		
Date ld_dsdt, ld_cabdt, ld_prev_cabdt
DateTime ld_dsdt_dt
int li_loop

str_distrib_schedule lstr

DwItemStatus ldwstat

SetNull(ls_null)
dw_bklist.SetTransObject(SqlServerTrans)
//dw_dsdtdsflag=create datastore
//dw_dsdtdsflag.dataobject='d_ds_dist_qnty_update_mchar_dsdtdsflag'
dw_dsdtdsflag.SetTransObject(SqlServerTrans)
ls_date=em_schdate.text
IF NOT IsDate(ls_date) THEN
	Messagebox('File Error','You should enter valid date.')	
	em_schdate.SetFocus()
	RETURN
ELSE
	ld_dsdt=Date(ls_date)
	ld_dsdt_dt=DateTime(ld_dsdt,Time('00:00:00'))
END IF
SELECT count(*) INTO:li_count
FROM mchar
where dsdt=:ld_dsdt_dt
USING SqlServerTrans;
IF NOT f_check_dberror(SqlServerTrans,'Select count from mchar where dsdt=:ld_dsdt') THEN
	RETURN
END IF
IF li_count>0 THEN
	li_re=Messagebox('','A distribution schedule for this date ('+ls_date+') already exists with'+&
		'~n '+String(li_count)+' books. Do you wish to create another distribution schedule'+&
		'~n for the same date?',exclamation!,yesNo!,2)
	IF li_re= 2 THEN
		em_schdate.SetFocus()
		RETURN
	END IF
END IF

// 06/23/08 Do Quota Check
IF wf_quota_check(wf_get_cabdt()) < 0 AND ( NOT ib_old_batch_exist) THEN
	RETURN
END IF

J=0
IF parent.cbx_include.checked=FALSE THEN
	li_row_count=dw_no_master.RowCount()
	FOR i=1 TO li_row_count
		ls_cld=dw_no_master.object.include1[i]
		ls_repeat=dw_no_master.object.repeat[i]
		ls_bkmed=dw_no_master.object.bkmed[i]
		li_bkseq=dw_no_master.object.bkseq[i]
		ls_cntr2=dw_no_master.object.cntr2[i]
		ls_cntr2=Trim(ls_cntr2)
		IF ls_cld='Y' THEN
			dw_no_master.object.dsdt[i]= ld_dsdt_dt
			dw_no_master.object.cascd[i]= 'R'

			li_cur =dw_dsdtdsflag.InsertRow(0)
			dw_dsdtdsflag.SetItem(li_cur,'bkseq',li_bkseq)
			dw_dsdtdsflag.SetItem(li_cur,'bkmed',ls_bkmed)
			dw_dsdtdsflag.SetItem(li_cur,'dsdt',ld_dsdt_dt)
			dw_dsdtdsflag.SetItem(li_cur,'cascd','R')
			IF ls_repeat='Y'  THEN
				dw_no_master.object.dsflag[i]= 'R'
				dw_dsdtdsflag.SetItem(li_cur,'dsflag',ls_null)
			END IF
			// only add these bkno in the bklist whose cntr2 is not null
			J++
			ls_bkmed=Trim(ls_bkmed)
			ls_bkno=ls_bkmed+String(li_bkseq)
			ls_bkno_list[J]= ls_bkno
//			li_cur2=dw_bklist.insertRow(0)
//			dw_bklist.object.bkno[li_cur2]=ls_bkno
		END IF//end of ls_icd="Y"
	NEXT
ELSE// cbx_include.checked =true
	li_row_count=dw_distribution_schedule.RowCount()
	FOR i=1 TO li_row_count
		ls_cld=dw_distribution_schedule.object.include1[i]
		ls_repeat=dw_distribution_schedule.object.repeat[i]
		ls_bkmed=dw_distribution_schedule.object.bkmed[i]
		li_bkseq=dw_distribution_schedule.object.bkseq[i]
		ls_cntr2=dw_distribution_schedule.object.cntr2[i]
		ls_cntrtype=dw_distribution_schedule.object.cntrtype[i]
		ls_cntr2=Trim(ls_cntr2)
		IF ls_cld='Y' THEN
			dw_distribution_schedule.object.dsdt[i]= ld_dsdt_dt
			dw_distribution_schedule.object.cascd[i]= 'R'
			
			IF (NOT(IsNull(ls_cntr2)) AND ls_cntr2<>'') OR &
											(IsNull(ls_cntr2) AND ls_cntrtype<>'D') THEN
				li_cur =dw_dsdtdsflag.InsertRow(0)
				dw_dsdtdsflag.SetItem(li_cur,'bkseq',li_bkseq)
				dw_dsdtdsflag.SetItem(li_cur,'bkmed',ls_bkmed)
				dw_dsdtdsflag.SetItem(li_cur,'dsdt',ld_dsdt_dt)
				dw_dsdtdsflag.SetItem(li_cur,'cascd','R')
				IF ls_repeat='Y'  THEN
					dw_distribution_schedule.object.dsflag[i]= 'R'
					dw_dsdtdsflag.SetItem(li_cur,'dsflag',ls_null)
				END IF
				// only add these bkno in the bklist whose cntr2 is not null
				J++
				ls_bkmed=Trim(ls_bkmed)
				ls_bkno=ls_bkmed+String(li_bkseq)
				ls_bkno_list[J]= ls_bkno
				
			END IF
		END IF
	NEXT
END IF
li_row_count= dw_dsdtdsflag.RowCount()
IF li_row_count=0 THEN 
	Messagebox('','You should choose some bkno.')
	RETURN
END IF
//set each row updatstatus for update later in the w_create_file, not in this window
//leave dw_dsdtdsflag temperay not update jet. wait untill the stage of update distsched
//table.
FOR i=1 TO li_row_count
	dw_dsdtdsflag.SetItemStatus(i , 0, primary!, dataModified!)
	dw_dsdtdsflag.SetItemStatus(i , 1, primary!, notModified!)
	dw_dsdtdsflag.SetItemStatus(i , 2, primary!, notModified!)
	ls_cascd=dw_dsdtdsflag.GetItemString(i ,'cascd')
	IF ls_cascd='R' THEN
		dw_dsdtdsflag.SetItemStatus(i , 3, primary!, dataModified!)
	ELSE
		dw_dsdtdsflag.SetItemStatus(i , 3, primary!, notModified!)
	END IF
//	ls_dsflag=dw_dsdtdsflag.GetItemString(i ,'dsflag')
//	if ls_dsflag='R' then
//		dw_dsdtdsflag.SetItemStatus(i , 4, Primary!, DataModified!)
//	else
//		dw_dsdtdsflag.SetItemStatus(i , 4, Primary!, NotModified!)
//	end if
	ld_dsdt_dt=dw_dsdtdsflag.GetItemDateTime(i ,'dsdt')
	ld_dsdt=Date(ld_dsdt_dt)
	ls_date=String(ld_dsdt,'mm/dd/yyyy')
	IF IsDate(ls_date) THEN
		dw_dsdtdsflag.SetItemStatus(i , 5, primary!, dataModified!)
	ELSE
		dw_dsdtdsflag.SetItemStatus(i , 5, primary!, notModified!)
	END IF
	ldwstat = dw_dsdtdsflag.GetItemStatus(i , 1, primary!)
	IF ldwstat <> notModified! THEN
		Messagebox("Bad Status","row " + String(i) + " col 1")
	END IF
	ldwstat = dw_dsdtdsflag.GetItemStatus(i , 2, primary!)
	IF ldwstat <> notModified! THEN
		Messagebox("Bad Status","row " + String(i) + " col 2")
	END IF
	ldwstat = dw_dsdtdsflag.GetItemStatus(i , 3, primary!)
	IF ldwstat <> dataModified! THEN
		Messagebox("Bad Status","row " + String(i) + " col 3")
	END IF
	
	ldwstat = dw_dsdtdsflag.GetItemStatus(i , 5, primary!)
	IF ldwstat <> dataModified! THEN
		Messagebox("Bad Status","row " + String(i) + " col 5")
	END IF
NEXT
// bkno_list is same in either case cbx_include checked or unchecked
lstr.arraymed[]= ls_bkno_list[]
//lstr.ld_date=id_cabdate
lstr.ld_date1=ld_dsdt

// 10/30/2008 update order qty with selected or default qty before schedule creation for distinct batch dates in the list
ls_msg = "Updating Order Quantity, Please wait..."
OpenWithParm(w_pics_retrieve_msg_box,ls_msg)	
FOR li_loop = 1 TO dw_no_master.rowcount()

	ls_cld = dw_no_master.object.include1[li_loop]
	IF ls_cld = 'Y' THEN
		ld_prev_cabdt = date(dw_no_master.object.cabdt[li_loop])
		IF ld_cabdt <> ld_prev_cabdt THEN
			ld_cabdt = ld_prev_cabdt
				update sched a
				set a.ordqty = (select nvl(selqty,defqty) as ordqty from batch@pic_link
						where batch.bkseq = a.bkseq
								and batch.bkmed = a.bkmed
								and batch.libcd = a.libcd
								and batch.cabdt = a.cabdt)
				where a.cabdt = :ld_cabdt  using sqlservertrans;
				commit using sqlservertrans ;
		END IF
	END IF
NEXT
Close(w_pics_retrieve_msg_box)	
/// 10/30/2008

// 04/08/2008 new screen to reference new special distribution schedle screen
OpenWithParm(w_create_files_special, lstr)


end event

type cb_create from u_cb within w_special_distribution_schedule
integer x = 1175
integer y = 1248
integer width = 402
integer taborder = 30
boolean bringtotop = true
fontcharset fontcharset = ansi!
string text = "&Recreate DS..."
end type

event clicked;call super::clicked;Long li_row_count, li_count=0,i, J,li_bkseq, li_re, li_bound, li_cur
String ls_repeat, ls_cld,ls_date, ls_bkno_list[],ls_bkmed, ls_bkno, ls_dsflag,ls_cascd,&
		ls_rtn, ls_cntr2, ls_sum_bkno, ls_null, ls_flag, ls_cntrtype
DateTime  ld_dsdt_dt
Date ld_dsdt
str_distrib_schedule lstr

DwItemStatus ldwstat

SetNull(ls_null)
//dw_dsdtdsflag=create datastore
//dw_dsdtdsflag.dataobject='d_ds_dist_qnty_update_mchar_dsdtdsflag'
dw_dsdtdsflag.SetTransObject(SqlServerTrans)
ls_date=em_schdate.text
IF NOT IsDate(ls_date) THEN
	Messagebox('File Error','You should enter valid date.')	
	em_schdate.SetFocus()
	RETURN
ELSE
	ld_dsdt=Date(ls_date)
	ld_dsdt_dt=DateTime(ld_dsdt,Time('00:00:00'))
END IF
SELECT count(*) INTO:li_count
FROM mchar
where dsdt=:ld_dsdt_dt
USING SqlServerTrans;
IF NOT f_check_dberror(SqlServerTrans,'Select count from mchar where dsdt=:ld_dsdt') THEN
	RETURN
END IF
IF li_count>0 THEN
	li_re=Messagebox('','A distribution schedule for this date ('+ls_date+') already exists with'+&
		'~n '+String(li_count)+' books. Do you wish to recreate distribution schedule'+&
		'~n for the same date?',exclamation!,yesNo!,2)
	IF li_re= 2 THEN
		em_schdate.SetFocus()
		RETURN
	END IF
END IF

// 06/23/08 Do Quota Check
IF wf_quota_check(wf_get_cabdt()) < 0 THEN
	RETURN
END IF

J=0
li_row_count=dw_no_master.RowCount()
FOR i=1 TO li_row_count
	ls_cld=dw_no_master.object.include1[i]
	ls_repeat=dw_no_master.object.repeat[i]
	ls_bkmed=dw_no_master.object.bkmed[i]
	li_bkseq=dw_no_master.object.bkseq[i]
	ls_cntrtype=dw_no_master.object.cntrtype[i]
	ls_cntr2=dw_no_master.object.cntr2[i]
	ls_cntr2=Trim(ls_cntr2)
	IF ls_cld='Y' THEN
		dw_no_master.object.dsdt[i]= ld_dsdt_dt
		dw_no_master.object.cascd[i]= 'R'
		IF (NOT(IsNull(ls_cntr2)) AND ls_cntr2<>"") OR (IsNull(ls_cntr2) OR ls_cntr2="")&
																AND	ls_cntrtype='D' THEN
			// update mchar only bkseq,bkmed whose cntr2  is not null
			li_cur =dw_dsdtdsflag.InsertRow(0)
			dw_dsdtdsflag.SetItem(li_cur,'bkseq',li_bkseq)
			dw_dsdtdsflag.SetItem(li_cur,'bkmed',ls_bkmed)
			dw_dsdtdsflag.SetItem(li_cur,'cascd','R')
			IF ls_repeat='Y'  THEN
				dw_distribution_schedule.object.dsflag[i]= 'R'
				dw_dsdtdsflag.SetItem(li_cur,'dsflag',ls_null)
			END IF
			dw_dsdtdsflag.SetItem(li_cur,'dsdt',ld_dsdt_dt)
			// only add these bkno in the bklist whose cntr2 is not null
			J++
			ls_bkmed=Trim(ls_bkmed)
			ls_bkno=ls_bkmed+String(li_bkseq)
			ls_bkno_list[J]= ls_bkno
		END IF
	END IF
NEXT
li_row_count= dw_dsdtdsflag.RowCount()
IF li_row_count=0 THEN 
	Messagebox('','You should choose some bkno.')
	RETURN
END IF
//set each row updatstatus for update later in the w_create_file, not in this window
//leave dw_dsdtdsflag temperay not update jet. wait untill the stage of update distsched
//table.
FOR i=1 TO li_row_count
	dw_dsdtdsflag.SetItemStatus(i , 0, primary!, dataModified!)
	dw_dsdtdsflag.SetItemStatus(i , 1, primary!, notModified!)
	dw_dsdtdsflag.SetItemStatus(i , 2, primary!, notModified!)
	ls_cascd=dw_dsdtdsflag.GetItemString(i ,'cascd')
	IF ls_cascd='R' THEN
		dw_dsdtdsflag.SetItemStatus(i , 3, primary!, dataModified!)
	ELSE
		dw_dsdtdsflag.SetItemStatus(i , 3, primary!, notModified!)
	END IF
	ls_flag=dw_dsdtdsflag.GetItemString(i ,'dsflag')
	IF IsNull(ls_flag)=TRUE THEN
		dw_dsdtdsflag.SetItemStatus(i , 4, primary!, dataModified!)
	ELSE
		dw_dsdtdsflag.SetItemStatus(i , 4, primary!, notModified!)
	END IF
	ld_dsdt_dt=dw_dsdtdsflag.GetItemDateTime(i ,'dsdt')
	ld_dsdt=Date(ld_dsdt_dt)
	ls_date=String(ld_dsdt,'mm/dd/yyyy')
	IF IsDate(ls_date) THEN
		dw_dsdtdsflag.SetItemStatus(i , 5, primary!, dataModified!)
	ELSE
		dw_dsdtdsflag.SetItemStatus(i , 5, primary!, notModified!)
	END IF
	ldwstat = dw_dsdtdsflag.GetItemStatus(i , 1, primary!)
	IF ldwstat <> notModified! THEN
		Messagebox("Bad Status","row " + String(i) + " col 1")
	END IF
	ldwstat = dw_dsdtdsflag.GetItemStatus(i , 2, primary!)
	IF ldwstat <> notModified! THEN
		Messagebox("Bad Status","row " + String(i) + " col 2")
	END IF
	ldwstat = dw_dsdtdsflag.GetItemStatus(i , 3, primary!)
	IF ldwstat <> dataModified! THEN
		Messagebox("Bad Status","row " + String(i) + " col 3")
	END IF
	ldwstat = dw_dsdtdsflag.GetItemStatus(i , 4, primary!)
	IF ldwstat <> dataModified! THEN
		Messagebox("Bad Status","row " + String(i) + " col 4")
	END IF
	ldwstat = dw_dsdtdsflag.GetItemStatus(i , 5, primary!)
	IF ldwstat <> dataModified! THEN
		Messagebox("Bad Status","row " + String(i) + " col 5")
	END IF
NEXT
// bkno_list is same in either case cbx_include checked or unchecked
lstr.arraymed[]= ls_bkno_list[]
//lstr.ld_date=ld_cabdt
lstr.ld_date1=ld_dsdt
lstr.ls_contract='Recreate'
// 04/08/2008 new screen
OpenWithParm(w_create_files_special, lstr)


end event

type cb_import from u_cb within w_special_distribution_schedule
event pfc_hinttext pbm_mousemove
string tag = "Create two file"
integer x = 649
integer y = 1248
integer width = 681
integer taborder = 30
boolean bringtotop = true
fontcharset fontcharset = ansi!
string text = "I&mport Copy Allotment Batch"
end type

event pfc_hinttext;call super::pfc_hinttext;w_pics_main.setmicrohelp(This.tag)
end event

event clicked;call super::clicked;
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//
//	Event: clicked for cb_import
//
//	Description:
//	Selected qty replaced with actual qty - calculations now looks into actual qty
//
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//	
//	Revision History
//	Developed by 	Date 				Version						Tracking#
//									
// Murali K.			04/02/2008      PICS 2.5 Modifications	 Reqs: CA.9
// Murali K.			04/08/2008		Validate against batch_data for closed scenario
// Murali K.		     06/23/2008       check selected qty, actual qty is updated later on - 
//											actual qty value will be null, since it is not set in batch formation
// Murali K.			10/31/2008		Transaction disconnect and import message corrected
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//

Date ld_today, ld_null
String ls_today,ls_include, ls_bkmed, ls_prodstg,ls_cntr,ls_prodstgold,ls_cntrold,&
		ls_bkmedold, ls_conno, ls_connoold, ls_null, ls_bkno[], ls_bk#, ls_bk#old,&
		ls_libcd, ls_filter, ls_cascd, ls_bkseq, ls_usid,ls_oracle_ext_env
		
Long li_row_count, i, li_count, li_bkseq,li_bkseqold, li_cur,J, li_selqty,li_row,&
		li_re, li_curmchar, li_count2, li_defqty, li_batchimprows,li_distfilerows, &
		li_schedupdrows, li_distschedrows, l_bkseq[]
Date ld_dsdt, ld_assigndt,ld_assigndtold,ld_cabdt, ld_cabdtold
DateTime ld_cabdt_dt, ld_dsdt_dt
str_distrib_schedule lstr
String ls_ok_cancel
Boolean lb_skip=FALSE, lb_exit=FALSE
DateTime ldt_defimp, ldt_schedins, ldt_webupd, ldt_marcexp, ldt_marcemail,&
		ldt_batchimp,ldt_distfile, ldt_distsched
SetPointer(hourglass!)

/////////////////////////////////////////////////////////////////////////////// 04/08/2008 get the latest open batch to import
/// 04/08/2008 Can import rush books only if the current batch is open
setnull(ld_cabdt)
select cabdt
into :ld_cabdt
from batch_data@pic_link
where batch_status_code <> 'C'  and cabdt = (select max(cabdt) from batch_data@pic_link ) using sqlservertrans ;

IF Isnull(ld_cabdt) THEN
	Messagebox("", "Batch is closed, Cannot  import batch")
	RETURN
END IF
////////////////////////

//li_re=Messagebox('','This process will import the copy allotment quantities into the current batch: ' + String(ld_cabdt,'mm/dd/yyyy') + '.~nDo you wish to proceed?',exclamation!,yesNo!,1)
// 10/31/2008 message to read properly
li_re=Messagebox('','This process will import the copy allotment quantities for RUSH books not distributed.' + '.~nDo you wish to proceed?',exclamation!,yesNo!,1)

IF li_re= 2 THEN
	RETURN
END IF

//***this must remember to uncomment and comment
lstr=message.powerObjectParm
ld_cabdt_dt=DateTime(ld_cabdt,Time('00:00:00'))
ls_bk#old=''
J=0

OpenWithParm(w_pics_retrieve_msg_box,"Retrieving Data, Please wait...")

dw_batch_process.SetTransObject(SqlServerTrans)
li_count=dw_batch_process.Retrieve(ld_cabdt_dt)
// check what step you are lacated
IF li_count=0 THEN
	Messagebox('Error','The batch formation process for this copy allotment batch'+&
	'~nwas not successfully completed. Can not continue.')
	IF IsValid(w_pics_retrieve_msg_box) THEN
		Close(w_pics_retrieve_msg_box)
	END IF
	RETURN
ELSE
	ldt_defimp=dw_batch_process.object.defimp[1]
	ldt_schedins=dw_batch_process.object.schedins[1]
	ldt_webupd=dw_batch_process.object.webupd[1]
	ldt_marcexp=dw_batch_process.object.marcexp[1]
	ldt_marcemail=dw_batch_process.object.marcemail[1]
	ldt_batchimp=dw_batch_process.object.batchimp[1]
END IF

ld_dsdt=Date(em_schdate.text)
ld_dsdt_dt=DateTime(ld_dsdt,Time('00:00:00'))

dw_dist.SetTransObject(SqlServerTrans)
// Check the DIST table . This table is used to keep track of the distribution schedule statistics
li_count=dw_dist.Retrieve(ld_dsdt_dt)
IF li_count>0 THEN
	ldt_distfile=dw_dist.object.distfile[1]
	ldt_distsched=dw_dist.object.distsched[1]
ELSEIF li_count=0 THEN
	dw_dist.InsertRow(0)
	dw_dist.object.dsdt[1]= ld_dsdt_dt
	SetNull(ldt_distfile)
	SetNull(ldt_distsched)
END IF
//following job is that: after you have finish batch formation now check what step you are in the
//distribution schedule
IF IsNull(ldt_batchimp)=FALSE THEN
	cbx_imp.enabled=TRUE
	cbx_imp.checked=TRUE// check the batch import step
END IF
IF IsNull(ldt_distfile)=FALSE THEN
	cbx_file.enabled=TRUE
	cbx_file.checked=TRUE
END IF// check the distfile step
IF IsNull(ldt_distsched)=FALSE THEN
	cbx_distsched.enabled=TRUE
	cbx_distsched.checked=TRUE
END IF// check the last step i.e. insert rows into distsched in oracle

cbx_imp.enabled=TRUE

//10/31/2008 if disconnected connect the transaction again. - error prevented
IF NOT SQLserverOracleTrans.DbHandle() >0 THEN
	SQLserverOracleTrans.of_connect() 
END IF

dw_import_cp_allotbatch.SetTransObject(SQLserverOracleTrans)

//select rows from batch oracle where selqty is not null and selqty<>defqty 
//and cabdt=ld_cabdt for update sched table
dw_import_cp_allotbatch.Retrieve(ld_cabdt_dt)
li_row_count=dw_import_cp_allotbatch.RowCount()

// why would you give this message and allow to proceed importing??? 4/9/2008 may be oracle db is disconnected
//IF li_row_count<=0 THEN
//	Messagebox('Error','System error while retrieving. '+&
//	'~nPlease contact system administrator.')
//	RETURN
//END IF

sle_book#.text=String(li_row_count)
st_bkorrow.text='Total Rows:'
dw_import_cp_allotbatch.visible=TRUE
dw_distribution_schedule.visible=FALSE
//dw_distribution_schedule_null_incld_cpy.visible=false
dw_import_cp_allotbatch.object.batch_libcd_t.visible=TRUE
dw_import_cp_allotbatch.object.t_1.visible=TRUE
dw_import_cp_allotbatch.object.books_bkno_t.visible=TRUE
dw_import_cp_allotbatch.object.batch_selqty_t.visible=TRUE

FOR i=1 TO li_row_count
	li_bkseq=dw_import_cp_allotbatch.object.bkseq[i]
	IF li_bkseq<>li_bkseqold THEN
		J++
		l_bkseq[J]= li_bkseq
		li_bkseqold=li_bkseq
	END IF
NEXT

li_count=UpperBound(l_bkseq[])
n_ds lds_cascd
lds_cascd=CREATE n_ds
lds_cascd.dataObject='d_bkseq_bkmed_cascd'
//decomposition bkno into bkseq, bkmed

lds_cascd.SetTransObject(SqlServerTrans)
IF li_row_count >0 THEN
	lds_cascd.Retrieve(l_bkseq[])
END IF

dw_update_selcd_sched.SetTransObject(SqlServerTrans)
li_count2=0
FOR i=1 TO li_row_count
	li_bkseq=dw_import_cp_allotbatch.object.bkseq[i]
	ls_bkmed=Trim(dw_import_cp_allotbatch.object.bkmed[i])
	ls_bk#=Trim(dw_import_cp_allotbatch.object.bkno[i])
	ls_libcd=Trim(dw_import_cp_allotbatch.object.libcd[i])
	// 06/23/2008 check selected qty, actual qty is updated later on - actual qty value will be null, since it is not set in batch formation
	li_selqty=dw_import_cp_allotbatch.object.selqty[i]
	// 04/02/2008 use actual qty from 2.5
//	li_selqty=dw_import_cp_allotbatch.object.actual_qty[i] // Actual qty need to be updated while creating DS 06/23/2008
	
	li_defqty=dw_import_cp_allotbatch.object.defqty[i]
	IF li_bkseq<>li_bkseqold THEN
		ls_filter="bkseq = "+String(li_bkseq)
		lds_cascd.SetFilter(ls_filter)
		lds_cascd.Filter()
		li_row= lds_cascd.RowCount()
		IF li_row<=0 THEN
			Messagebox('Database Error','Database error contact with database administrator. ~r~n Error in statement: SELECT DISTINCT bkseq, bkmed, cascd  FROM mchar   WHERE '+ls_filter)
	        CONTINUE // 03/24/2010 unsupported appeon feature
		END IF
		li_bkseq=lds_cascd.object.bkseq[1]
		ls_cascd=lds_cascd.object.cascd[1]
		IF ls_cascd='R' OR ls_cascd='P'  THEN
			li_re=Messagebox('Warning','Book Number '+ls_bkmed + String(li_bkseq)+' has already been imported.'+&
				'~nDo you wish to import it again?',exclamation!,yesNoCancel!,1)
			IF li_re= 1 THEN
				lb_skip=FALSE
			ELSEIF li_re=2 THEN
				lb_skip=TRUE
				li_count2 +=1//this bkseq,bkmed in sched table will not updated
			ELSEIF li_re=3 THEN
				lb_exit= TRUE
				EXIT
			END IF
		ELSE
			lb_skip=FALSE
		END IF
		li_bkseqold=li_bkseq
	END IF
	IF lb_skip =FALSE THEN//if lb_skip=true do not update for this bkseq,bkmed
		li_cur= dw_update_selcd_sched.InsertRow(0)
		dw_update_selcd_sched.object.bkseq[li_cur]=li_bkseq
		dw_update_selcd_sched.object.bkmed[li_cur]=ls_bkmed
		dw_update_selcd_sched.object.libcd[li_cur]=ls_libcd
		dw_update_selcd_sched.object.bkno[li_cur]=ls_bk#
		dw_update_selcd_sched.object.selcd[li_cur]='S'
		dw_update_selcd_sched.object.selqty[li_cur]=li_selqty
		IF li_selqty<>0 AND IsNull(li_selqty)=FALSE AND li_selqty<> li_defqty THEN
			dw_update_selcd_sched.object.ordqty[li_cur]=li_selqty
		END IF
		dw_update_selcd_sched.object.selrtndt[li_cur]=DateTime(Today(),Time('00:00:00'))
		// 11/20/2008 update audit columns
		dw_update_selcd_sched.object.modified_by[li_cur]=gnv_app.of_getuserid()
		dw_update_selcd_sched.object.modified_date[li_cur]=today()

		dw_update_selcd_sched.SetItemStatus(li_cur, 0, primary!, dataModified!)
		dw_update_selcd_sched.SetItemStatus(li_cur, 1, primary!, notModified!)
		dw_update_selcd_sched.SetItemStatus(li_cur, 2, primary!, notModified!)
		dw_update_selcd_sched.SetItemStatus(li_cur, 3, primary!, notModified!)
		dw_update_selcd_sched.SetItemStatus(li_cur, 4, primary!, notModified!)
		dw_update_selcd_sched.SetItemStatus(li_cur, 5, primary!, dataModified!)
		dw_update_selcd_sched.SetItemStatus(li_cur, 6, primary!, dataModified!)
		dw_update_selcd_sched.SetItemStatus(li_cur, 7, primary!, dataModified!)
		dw_update_selcd_sched.SetItemStatus(li_cur, 8, primary!, dataModified!)
			//11/20/2008 make audit columns updatable
		dw_update_selcd_sched.SetItemStatus(li_cur, 9, primary!, dataModified!)
		dw_update_selcd_sched.SetItemStatus(li_cur, 10, primary!, dataModified!)
	END IF

NEXT
li_count=li_count - li_count2
Close(w_pics_retrieve_msg_box)
st_insertupdate.text='Updating data. This process will take several minutes.'
String ls_msgtext[ ] = {" 0 %","10 %", "20 %", "30 %", "40 %", "50 %", "60 %", &
										"70 %", "80 %", "90 %", "100 %"}
										
st_bkorrow.visible=TRUE
sle_book#.visible=TRUE								
uo_progress.of_SetTextColor(Rgb(255, 255, 255))
parent.SetRedraw(TRUE)
uo_progress.visible=TRUE
uo_progress.of_SetMinimum(0)
uo_progress.of_SetMaximum(li_cur)
uo_progress.of_SetDisplayStyle(3)
uo_progress.of_SetMessageText(ls_msgtext)
uo_progress.of_SetPosition(0)

//li_re=lds_cascd.update()
ls_usid=SqlServerTrans.userId
ldt_batchimp=DateTime(Today(),Now())
li_re=dw_update_selcd_sched.Update()
IF li_re= 1 THEN
	//prepare for click cb_new
	// 10/27/2008
	UPDATE mchar
	set cascd='P', dsdt=NULL, dsflag=NULL 
	where cabdt <= :ld_cabdt_dt
	and dsdt is null 
	and priority = 'R'
	USING SqlServerTrans;
	IF NOT f_check_dberror(SqlServerTrans,'update mchar set cascd=P') THEN
		RETURN
	END IF
	dw_batch_process.object.batchimp[1]= ldt_batchimp
	dw_batch_process.object.batchimprows[1]= li_row_count
	//rows in batch of oracle are imported
	dw_batch_process.object.booksimprows[1]= li_count
	// the number of different bkseq imported - the number of skiped bkseq
	dw_batch_process.object.schedupdrows[1]= li_cur
	//rows in sched table have been updated
	dw_batch_process.object.batchimpuser[1]= ls_usid
	li_re=dw_batch_process.Update()
	IF li_re=1 THEN
		COMMIT USING SQLserverOracleTrans;
		COMMIT USING SqlServerTrans;
		st_imp.text=String(ldt_batchimp,'mm/dd/yyyy hh:mm:ss')
		cbx_imp.checked=TRUE
	ELSE
		ROLLBACK USING SQLserverOracleTrans;
		ROLLBACK USING SqlServerTrans;
	END IF
ELSE
	ROLLBACK USING SQLserverOracleTrans;
	ROLLBACK USING SqlServerTrans;
END IF


parent.SetRedraw(TRUE)
uo_progress.visible=FALSE
cb_file.visible=FALSE
cb_include.visible=FALSE
cb_print.visible=FALSE
cb_import.visible=FALSE

Messagebox('Batch Import','Data successfully imported.')

st_bkorrow.visible=FALSE
sle_book#.visible=FALSE
uo_progress.visible=FALSE
st_insertupdate.text=''
dw_import_cp_allotbatch.visible=TRUE
dw_import_cp_allotbatch.Reset()
dw_distribution_schedule.visible=FALSE

dw_import_cp_allotbatch.object.batch_libcd_t.visible=FALSE
dw_import_cp_allotbatch.object.t_1.visible=FALSE
dw_import_cp_allotbatch.object.books_bkno_t.visible=FALSE
dw_import_cp_allotbatch.object.batch_selqty_t.visible=FALSE

cb_import.visible=FALSE

SetPointer(arrow!)

end event

type dw_no_master from u_dw within w_special_distribution_schedule
event pfc_hinttext pbm_mousemove
event ue_enterkey pbm_dwnprocessenter
event pfc_keydown pbm_dwnkey
integer x = 46
integer y = 84
integer width = 2670
integer height = 908
integer taborder = 70
string dataobject = "d_distribution_schedule_null_no_master_rush"
boolean hscrollbar = true
boolean hsplitscroll = true
boolean livescroll = false
boolean ib_isupdateable = false
boolean ib_rmbmenu = false
boolean ib_rmbfocuschange = false
string is_updatesallowed = ""
end type

event pfc_hinttext;//string ls_object, ls_column, ls_column_tag
//long ll_pos
//
////This script set's microhelp at the bottom of the screen dw_vol_selection Datawindow
//ls_object = THIS.getobjectatpointer()
//ll_pos = pos(ls_object, "~t")
//IF NOT pos(ls_object, "_t~t") > 0 THEN
//	IF ll_pos > 0 THEN
//		ll_pos = ll_pos -1
//		ls_column = mid(ls_object,1,ll_pos)
//		ls_column_tag = THIS.Describe(ls_column + ".tag")
//		w_pics_main.setmicrohelp(ls_column_tag)
//	ELSE
//		w_pics_main.setmicrohelp("Ready")
//	END IF
//END IF
end event

event ue_enterkey;//Send(Handle(dw_select_deselect),256,9,Long(0,0))
//return(1)
end event

event constructor;call super::constructor;long li_row_count, li_count2, i
string ls_incld

this.AcceptText()
li_row_count=this.RowCount()
for i=1 to li_row_count
	ls_incld=this.GetItemString(i,'include1')
	if ls_incld='Y' then li_count2++
next

sle_book#.text=string(li_count2)

ib_rmbmenu = FALSE


end event

event getfocus;call super::getfocus;m_pics_main.m_Edit.m_Addrow.Enabled = FALSE
m_pics_main.m_Edit.m_Deleterow.Enabled = FALSE
m_pics_main.m_Edit.m_Cut.Enabled = FALSE
end event

event itemchanged;long li_row_count, li_count=0,i
string  ls_cld

//messagebox('','itemchanged')

if dwo.name ='include1' then
	ls_cld=data
	this.object.include1[row]= ls_cld

	li_row_count=this.RowCount()
	for i=1 to li_row_count
		ls_cld=this.object.include1[i]
		if ls_cld='Y' then
	//		this.object.dsdt[row]= ld_dsdt
			li_count++
		end if
		sle_book#.text=string(li_count)
	next
end if

end event

event retrieveend;call super::retrieveend;integer i,j
long Lbkseq
string Lbkmed,Lcntr
date Lschstdt,Lactstdt,Lactenddt
//

	close(w_pics_retrieve_msg_box)

end event

event retrievestart;call super::retrievestart;OpenWithParm(w_pics_retrieve_msg_box,"Retrieving records, Please wait...")

end event

event rowfocuschanged;call super::rowfocuschanged;currentrow = currentrow 



end event

event sqlpreview;call super::sqlpreview;//MessageBox("sql",sqlsyntax)
end event

event updateend;call super::updateend;Close(w_pics_update_msg_box)
end event

event updatestart;call super::updatestart;Open(w_pics_update_msg_box)
end event

event pfc_retrieve;call super::pfc_retrieve;RETURN THIS.Retrieve()

end event

type dw_distribution_schedule from u_dw within w_special_distribution_schedule
event pfc_hinttext pbm_mousemove
event ue_enterkey pbm_dwnprocessenter
event pfc_keydown pbm_dwnkey
integer x = 9
integer y = 208
integer width = 2670
integer height = 920
integer taborder = 40
string dataobject = "d_distribution_schedule_rush"
boolean hscrollbar = true
boolean hsplitscroll = true
boolean livescroll = false
boolean ib_isupdateable = false
boolean ib_rmbmenu = false
boolean ib_rmbfocuschange = false
string is_updatesallowed = ""
end type

event pfc_hinttext;//string ls_object, ls_column, ls_column_tag
//long ll_pos
//
////This script set's microhelp at the bottom of the screen dw_vol_selection Datawindow
//ls_object = THIS.getobjectatpointer()
//ll_pos = pos(ls_object, "~t")
//IF NOT pos(ls_object, "_t~t") > 0 THEN
//	IF ll_pos > 0 THEN
//		ll_pos = ll_pos -1
//		ls_column = mid(ls_object,1,ll_pos)
//		ls_column_tag = THIS.Describe(ls_column + ".tag")
//		w_pics_main.setmicrohelp(ls_column_tag)
//	ELSE
//		w_pics_main.setmicrohelp("Ready")
//	END IF
//END IF
end event

event ue_enterkey;//Send(Handle(dw_select_deselect),256,9,Long(0,0))
//return(1)
end event

event constructor;call super::constructor;long li_row_count, li_count2, i
string ls_incld

this.AcceptText()
li_row_count=this.RowCount()
for i=1 to li_row_count
	ls_incld=this.GetItemString(i,'include1')
	if ls_incld='Y' then li_count2++
next

sle_book#.text=string(li_count2)

ib_rmbmenu = FALSE


end event

event getfocus;call super::getfocus;m_pics_main.m_Edit.m_Addrow.Enabled = FALSE
m_pics_main.m_Edit.m_Deleterow.Enabled = FALSE
m_pics_main.m_Edit.m_Cut.Enabled = FALSE
end event

event itemchanged;long li_row_count, li_count=0,i
string  ls_cld

//messagebox('','itemchanged')

if dwo.name ='include1' then
	ls_cld=data
	this.object.include1[row]= ls_cld

	li_row_count=this.RowCount()
	for i=1 to li_row_count
		ls_cld=this.object.include1[i]
		if ls_cld='Y' then
	//		this.object.dsdt[row]= ld_dsdt
			li_count++
		end if
		sle_book#.text=string(li_count)
	next
end if

end event

event pfc_retrieve;call super::pfc_retrieve;RETURN THIS.Retrieve()

end event

event retrieveend;call super::retrieveend;integer i,j
long Lbkseq
string Lbkmed,Lcntr
date Lschstdt,Lactstdt,Lactenddt
//

	close(w_pics_retrieve_msg_box)

end event

event retrievestart;call super::retrievestart;OpenWithParm(w_pics_retrieve_msg_box,"Retrieving records, Please wait...")

end event

event rowfocuschanged;call super::rowfocuschanged;currentrow = currentrow 



end event

event updatestart;call super::updatestart;Open(w_pics_update_msg_box)
end event

event updateend;call super::updateend;Close(w_pics_update_msg_box)
end event

event sqlpreview;call super::sqlpreview;//MessageBox("sql",sqlsyntax)
end event

type dw_import_cp_allotbatch from u_dw within w_special_distribution_schedule
event pfc_hinttext pbm_mousemove
event ue_enterkey pbm_dwnprocessenter
event pfc_keydown pbm_dwnkey
integer x = 27
integer y = 204
integer width = 2633
integer height = 920
integer taborder = 60
string dataobject = "d_import_cp_allotbatch_rush"
boolean hscrollbar = true
boolean hsplitscroll = true
boolean livescroll = false
boolean ib_isupdateable = false
boolean ib_rmbmenu = false
boolean ib_rmbfocuschange = false
string is_updatesallowed = ""
end type

event ue_enterkey;//Send(Handle(dw_select_deselect),256,9,Long(0,0))
//return(1)
end event

event constructor;call super::constructor;ib_rmbmenu = FALSE


end event

event retrieveend;call super::retrieveend;//integer i,j
//long Lbkseq
//string Lbkmed,Lcntr
//date Lschstdt,Lactstdt,Lactenddt
////
//
//	close(w_pics_retrieve_msg_box)
//
end event

event retrievestart;call super::retrievestart;//OpenWithParm(w_pics_retrieve_msg_box,"Retrieving batch records from oracle, Please wait...")
//
end event

event sqlpreview;call super::sqlpreview;long li_row_count


//if uo_progress2.visible=true then
//	uo_progress2.of_Increment(1)
//end if
//if mod(i_count,100)=0 then
//	w_create_files.SetRedraw(TRUE)
//else
//	w_create_files.SetRedraw(false)
//end if

end event


$PBExportHeader$w_cmpare_orcl_infx.srw
forward
global type w_cmpare_orcl_infx from w_main
end type
type st_3stage from statictext within w_cmpare_orcl_infx
end type
type cb_orc from commandbutton within w_cmpare_orcl_infx
end type
type cb_inx from commandbutton within w_cmpare_orcl_infx
end type
type cb_inxorc from commandbutton within w_cmpare_orcl_infx
end type
type tab from u_tb within w_cmpare_orcl_infx
end type
type tab from u_tb within w_cmpare_orcl_infx
end type
type rb_prdrbk from radiobutton within w_cmpare_orcl_infx
end type
type dw_prod_oracle_bkno from u_pics_dw within w_cmpare_orcl_infx
end type
type rb_mod_date from radiobutton within w_cmpare_orcl_infx
end type
type rb_upd_date from radiobutton within w_cmpare_orcl_infx
end type
type rb_bkno from radiobutton within w_cmpare_orcl_infx
end type
type st_2 from statictext within w_cmpare_orcl_infx
end type
type rb_both from radiobutton within w_cmpare_orcl_infx
end type
type rb_infx from radiobutton within w_cmpare_orcl_infx
end type
type rb_oracle from radiobutton within w_cmpare_orcl_infx
end type
type st_1 from statictext within w_cmpare_orcl_infx
end type
type cb_update from commandbutton within w_cmpare_orcl_infx
end type
type uo_progress from u_progressbar within w_cmpare_orcl_infx
end type
type cb_print from commandbutton within w_cmpare_orcl_infx
end type
type st_total_inx_orc from statictext within w_cmpare_orcl_infx
end type
type st_total_inx from statictext within w_cmpare_orcl_infx
end type
type st_total_orc from statictext within w_cmpare_orcl_infx
end type
type st_process from statictext within w_cmpare_orcl_infx
end type
type cb_exit from commandbutton within w_cmpare_orcl_infx
end type
end forward

global type w_cmpare_orcl_infx from w_main
integer width = 3593
integer height = 1748
boolean enabled = false
string title = "Web and PICS table comparison"
event ue_enterkey pbm_dwnprocessenter
st_3stage st_3stage
cb_orc cb_orc
cb_inx cb_inx
cb_inxorc cb_inxorc
tab tab
rb_prdrbk rb_prdrbk
dw_prod_oracle_bkno dw_prod_oracle_bkno
rb_mod_date rb_mod_date
rb_upd_date rb_upd_date
rb_bkno rb_bkno
st_2 st_2
rb_both rb_both
rb_infx rb_infx
rb_oracle rb_oracle
st_1 st_1
cb_update cb_update
uo_progress uo_progress
cb_print cb_print
st_total_inx_orc st_total_inx_orc
st_total_inx st_total_inx
st_total_orc st_total_orc
st_process st_process
cb_exit cb_exit
end type
global w_cmpare_orcl_infx w_cmpare_orcl_infx

type variables
str_distrib_schedule istr
datastore ids_distsched
long i_count=0,  i_index, i_inxprd, i_orcprd, i_inxqa, i_orcqa, i_inxbk, i_orcbk, i_difprd,&
		i_difqa, i_difbk, i_event=0
string is_yes_no='N'
boolean ib_ask_yn= false, ib_qaretrieved=false, ib_bkretrieved=false, ib_pdretrieved

end variables

forward prototypes
public function string wf_remove_chars (string sndx_in)
public subroutine wf_qa2 ()
public function integer wf_qa ()
public function integer wf_prod ()
public function integer wf_book ()
end prototypes

event ue_enterkey;//Send(Handle(this),256,9,Long(0,0))
//return(1)
end event

public function string wf_remove_chars (string sndx_in);////************************************************************
////wf_remove_chars
////************************************************************
// 
//string  sndx_out
//integer  i_len, idx, i, j, alpha_char, li_pos
//boolean  b_exit = FALSE
//n_cst_string 	inv_string
//
//sndx_in =  inv_string.of_GlobalReplace(sndx_in, "[braille]", "")
//sndx_in =  inv_string.of_GlobalReplace(sndx_in, "[sound recording]", "")
////sndx_in =  inv_string.of_GlobalReplace(sndx_in, "(et al.)", "")
////sndx_in =  inv_string.of_GlobalReplace(sndx_in, ",", "")
//sndx_in =  inv_string.of_GlobalReplace(sndx_in, "#", "")
//sndx_in =  inv_string.of_GlobalReplace(sndx_in, ";", "")
//sndx_in =  inv_string.of_GlobalReplace(sndx_in, "/", "")
//sndx_in =  inv_string.of_GlobalReplace(sndx_in, "\", "")
//sndx_in =  inv_string.of_GlobalReplace(sndx_in, "|", "")
//sndx_in =  inv_string.of_GlobalReplace(sndx_in, "~~", "")
////sndx_in =  inv_string.of_GlobalReplace(sndx_in, "(", "")
////sndx_in =  inv_string.of_GlobalReplace(sndx_in, ")", "")
////sndx_in =  inv_string.of_GlobalReplace(sndx_in, "{", "")
////sndx_in =  inv_string.of_GlobalReplace(sndx_in, "}", "")
//sndx_in =  inv_string.of_GlobalReplace(sndx_in, "+", "")
////sndx_in =  inv_string.of_GlobalReplace(sndx_in, "-", "")
//sndx_in =  inv_string.of_GlobalReplace(sndx_in, "_", "")
//sndx_in =  inv_string.of_GlobalReplace(sndx_in, "=", "")
//sndx_in =  inv_string.of_GlobalReplace(sndx_in, "/", "")
//sndx_in =  inv_string.of_GlobalReplace(sndx_in, "\\", "")
////sndx_in =  inv_string.of_GlobalReplace(sndx_in, "'", "")
//
//sndx_out = sndx_in
//
//RETURN sndx_out
return ""
end function

public subroutine wf_qa2 ();string ls_bklist[],  ls_bkmed,  ls_stage_orc, ls_stage
string		 ls_bkmed_orc, ls_arflag, ls_cntr, ls_cntr_orc, ls_null, ls_bkmed_old
long li_bkseq,li_bkseq_orc, li_row, li_count, i, j=0,k, li_re, li_cur,rtn, li_bkseq_old
datetime ldt_schstdt, ldt_schenddt, ldt_actstdt, ldt_actenddt
date	ld_schstdt, ld_schenddt, ld_actstdt, ld_actenddt, ld_null, ld_update_date
date	ld_schstdt_orc, ld_schenddt_orc, ld_actstdt_orc, ld_actenddt_orc, ld_mod_date
boolean lb_find, lb_insert

String ls_msgtext[ ] = {" 0 %","10 %", "20 %", "30 %", "40 %", "50 %", "60 %", &
									"70 %", "80 %", "90 %", "100 %"}
uo_progress.of_SetTextColor(RGB(255, 255, 255))	
st_process.text='Retrieving data from Oracle...'
//****
li_count=tab.pgprod.dw_prod_oracle_bkno.Retrieve()
li_row=tab.pgprod.dw_prod_oracle.Retrieve()
st_total_orc.text='Total row in oracle: '+string(li_row)
SetNull(ld_null)
SetNull(ls_null)
if li_count=0  or li_row=0 then
	messagebox('Error','There is no book in oracle database, '+&
		'~nContact database administrator.')
	return
end if
st_process.text='Copy bkseq from Web to PICS prdrbk_compare table and update...'
this.SetRedraw(true)
uo_progress.Visible=TRUE
uo_progress.of_SetMinimum(0)
uo_progress.of_SetMaximum(li_count * 0.1)
uo_progress.of_SetDisplayStyle(3)
uo_progress.of_SetMessageText(ls_msgtext)
uo_progress.of_SetPosition(0)
ls_bkmed_old=""
li_bkseq_old=0
SetPointer(HourGlass!)
delete
from prdrbk_compare
using SqlServerTrans;
if f_check_dberror(SqlServerTrans,'delete from prdrbk_compare')=false then
	return
else
	commit using SqlServerTrans;
	select count(*) into :li_re
	from prdrbk_compare
	using SqlServerTrans;
//	messagebox('li_re','count = '+string(li_re))
end if
rtn=tab.pgprod.dw_prod_oracle_bkno.RowsCopy(1, li_count, Primary!,tab.pgprod.dw_prdrbk_cmp, 1, Primary!)
tab.pgprod.dw_prdrbk_cmp.Sort()


if rtn=1 then
	li_re=tab.pgprod.dw_prdrbk_cmp.update()
	if li_re=1 then
		commit using SqlServerTrans;
	else
		RollBack using SqlServerTrans;
		return
	end if
else
	return
end if
SetPointer(HourGlass!)
  SELECT count(*) into :li_count
    FROM prod a, mchar b 
where a.bkseq in (select distinct bkseq from prdrbk_compare) and
		a.bkmed=b.bkmed and a.bkseq=b.bkseq
using SqlServerTrans;
if f_check_dberror(SqlServerTrans,'select count(*) from joint of prod and mchar')=false then
	return
end if
this.SetRedraw(true)
uo_progress.Visible=TRUE
uo_progress.of_SetMinimum(0)
uo_progress.of_SetMaximum(li_count * 0.1)
uo_progress.of_SetDisplayStyle(3)
uo_progress.of_SetMessageText(ls_msgtext)
uo_progress.of_SetPosition(0)
st_process.text='Retrieving data from PICS where bkseq in prdrbk_compare...'
//****
tab.pgprod.dw_prod_inx.Retrieve()
li_count=tab.pgprod.dw_prod_oracle.RowCount()
this.SetRedraw(true)
uo_progress.Visible=TRUE
uo_progress.of_SetMinimum(0)
uo_progress.of_SetMaximum(li_count * 0.1)
uo_progress.of_SetDisplayStyle(3)
uo_progress.of_SetMessageText(ls_msgtext)
uo_progress.of_SetPosition(0)
tab.pgprod.dw_prod_inx.Sort()
li_row=tab.pgprod.dw_prod_inx.RowCount()
st_total_inx.text='Total row in PICS: '+string(li_row)
if li_row=0 then
	messagebox('Error','There is no book in PICS database, '+&
		'~nContact database administrator.')
	return
end if
st_process.text='Compare Web and PICS'
k=1
rtn=0
FOR i=1 to li_count
	if mod(i, 10)=0 then
		uo_progress.of_Increment(1)
		this.SetRedraw(TRUE)
	else
		this.SetRedraw(false)
	end if	
//	ls_bkno_orc=trim(tab.pgprod.dw_prod_oracle.object.bkno[i])
	ls_stage_orc=trim(tab.pgprod.dw_prod_oracle.object.prodstage[i])
	ls_bkmed_orc=trim(tab.pgprod.dw_prod_oracle.object.bkmed[i])
	li_bkseq_orc=tab.pgprod.dw_prod_oracle.object.bkseq[i]
	
	ls_cntr_orc=trim(tab.pgprod.dw_prod_oracle.object.cntr[i])
	ld_schstdt_orc=date(tab.pgprod.dw_prod_oracle.object.schstdt[i])
	ld_schenddt_orc=date(tab.pgprod.dw_prod_oracle.object.schenddt[i])
	ld_actstdt_orc=date(tab.pgprod.dw_prod_oracle.object.actstdt[i])
	ld_actenddt_orc=date(tab.pgprod.dw_prod_oracle.object.actenddt[i])
	ld_mod_date=date(tab.pgprod.dw_prod_oracle.object.prdrbk_mod_date[i])
	lb_find=false
	lb_insert=false
	for j=k to li_row
//		ls_bkno=trim(tab.pgprod.dw_prod_inx.object.bkno[j])
		ls_stage=trim(tab.pgprod.dw_prod_inx.object.prodstage[j])
		ls_bkmed=trim(tab.pgprod.dw_prod_inx.object.bkmed[j])
		li_bkseq=tab.pgprod.dw_prod_inx.object.bkseq[j]
		ls_cntr=trim(tab.pgprod.dw_prod_inx.object.cntr[j])
		ls_arflag=trim(tab.pgprod.dw_prod_inx.object.arflag[j])
		ld_schstdt=tab.pgprod.dw_prod_inx.object.schstdt[j]
		ld_schenddt=tab.pgprod.dw_prod_inx.object.schenddt[j]
		ld_actstdt=tab.pgprod.dw_prod_inx.object.actstdt[j]
		ld_actenddt=tab.pgprod.dw_prod_inx.object.actenddt[j]
		ld_update_date=date(tab.pgprod.dw_prod_inx.object.update_date[j])
		if ls_bkmed_orc=ls_bkmed and li_bkseq_orc= li_bkseq and ls_stage_orc=ls_stage then
			lb_find=true
			k= j+1
			if isnull(ld_schstdt_orc) and isnull(ld_schstdt)=false or &
				isnull(ld_schstdt_orc)=false and isnull(ld_schstdt)=true or &
				ld_schstdt_orc<> ld_schstdt or &
				isnull(ld_schenddt_orc) and isnull(ld_schenddt)=false or &
				isnull(ld_schenddt_orc)=false and isnull(ld_schenddt)=true or &
				ld_schenddt_orc<> ld_schenddt or &
				isnull(ld_actstdt_orc) and isnull(ld_actstdt)=false or &
				isnull(ld_actstdt_orc)=false and isnull(ld_actstdt)=true or &
				ld_actstdt_orc<> ld_actstdt or &
				isnull(ld_actenddt_orc) and isnull(ld_actenddt)=false or &
				isnull(ld_actenddt_orc)=false and isnull(ld_actenddt)=true or &
				ld_actenddt_orc<> ld_actenddt or ls_cntr<> ls_cntr_orc then
				if ls_bkmed<>ls_bkmed_old or li_bkseq<>li_bkseq_old then
					tab.pgprod.dw_prod_inx.object.diff[j]='Y'
					rtn++
					ls_bkmed_old=ls_bkmed
					li_bkseq_old=li_bkseq
				end if
				lb_insert=true
				li_cur=tab.pgprod.dw_prod_inx_orc.InsertRow(0)
				tab.pgprod.dw_prod_inx_orc.object.bkmed[li_cur]=ls_bkmed
				tab.pgprod.dw_prod_inx_orc.object.bkseq[li_cur]=li_bkseq
				tab.pgprod.dw_prod_inx_orc.object.prodstage[li_cur]=ls_stage
				tab.pgprod.dw_prod_inx_orc.object.cntr[li_cur]=ls_cntr
				tab.pgprod.dw_prod_inx_orc.object.cntr_orc[li_cur]=ls_cntr_orc
				tab.pgprod.dw_prod_inx_orc.object.schstdt[li_cur]=ld_schstdt
				tab.pgprod.dw_prod_inx_orc.object.schstdt_orc[li_cur]=ld_schstdt_orc
				tab.pgprod.dw_prod_inx_orc.object.schenddt[li_cur]=ld_schenddt
				tab.pgprod.dw_prod_inx_orc.object.schenddt_orc[li_cur]=ld_schenddt_orc
				tab.pgprod.dw_prod_inx_orc.object.actstdt[li_cur]=ld_actstdt
				tab.pgprod.dw_prod_inx_orc.object.actstdt_orc[li_cur]=ld_actstdt_orc
				tab.pgprod.dw_prod_inx_orc.object.actenddt[li_cur]=ld_actenddt
				tab.pgprod.dw_prod_inx_orc.object.actenddt_orc[li_cur]=ld_actenddt_orc
				tab.pgprod.dw_prod_inx_orc.object.update_date[li_cur]=ld_update_date
				tab.pgprod.dw_prod_inx_orc.object.mod_date[li_cur]=ld_mod_date
				if relativedate(ld_schenddt_orc,1)=ld_schenddt or &
								relativedate(ld_schenddt, 1)=ld_schenddt_orc then
					tab.pgprod.dw_prod_inx_orc.object.comment[li_cur]='Only one day'
				end if
				if ls_cntr<> ls_cntr_orc then
					tab.pgprod.dw_prod_inx_orc.object.comment[li_cur]='Cntr is different'
				END IF
				if ls_arflag='Y' then
					tab.pgprod.dw_prod_inx_orc.object.comment[li_cur]='ARCHIVES'
				END IF
				EXIT
			end if 
		elseif (ls_bkmed_orc<ls_bkmed or ls_bkmed_orc=ls_bkmed and li_bkseq_orc< li_bkseq ) or &
				(ls_bkmed_orc=ls_bkmed and li_bkseq_orc=li_bkseq and ls_stage_orc<ls_stage) then
			if lb_find=false then
				
				if (ls_bkmed_orc=ls_bkmed and li_bkseq_orc=li_bkseq and ls_stage_orc<ls_stage) then
					k=j 
					li_cur=tab.pgprod.dw_prod_inx_orc.InsertRow(0)
					tab.pgprod.dw_prod_inx_orc.object.bkmed[li_cur]=ls_bkmed_orc
					tab.pgprod.dw_prod_inx_orc.object.bkseq[li_cur]=li_bkseq_orc
					tab.pgprod.dw_prod_inx_orc.object.prodstage[li_cur]=ls_stage_orc
					tab.pgprod.dw_prod_inx_orc.object.cntr[li_cur]=ls_null
					tab.pgprod.dw_prod_inx_orc.object.cntr_orc[li_cur]=ls_cntr_orc
					tab.pgprod.dw_prod_inx_orc.object.schstdt[li_cur]=ld_null
					tab.pgprod.dw_prod_inx_orc.object.schstdt_orc[li_cur]=ld_schstdt_orc
					tab.pgprod.dw_prod_inx_orc.object.schenddt[li_cur]=ld_null
					tab.pgprod.dw_prod_inx_orc.object.schenddt_orc[li_cur]=ld_schenddt_orc
					tab.pgprod.dw_prod_inx_orc.object.actstdt[li_cur]=ld_null
					tab.pgprod.dw_prod_inx_orc.object.actstdt_orc[li_cur]=ld_actstdt_orc
					tab.pgprod.dw_prod_inx_orc.object.actenddt[li_cur]=ld_null
					tab.pgprod.dw_prod_inx_orc.object.actenddt_orc[li_cur]=ld_actenddt_orc
					tab.pgprod.dw_prod_inx_orc.object.update_date[li_cur]=ld_null
					tab.pgprod.dw_prod_inx_orc.object.mod_date[li_cur]=ld_mod_date
					tab.pgprod.dw_prod_inx_orc.object.comment[li_cur]='No Data in PICS'
					EXIT
				elseif (ls_bkmed_orc<ls_bkmed) then
					k= j
					li_cur=tab.pgprod.dw_prod_inx_orc.InsertRow(0)
					tab.pgprod.dw_prod_inx_orc.object.bkmed[li_cur]=ls_bkmed_orc
					tab.pgprod.dw_prod_inx_orc.object.bkseq[li_cur]=li_bkseq_orc
					tab.pgprod.dw_prod_inx_orc.object.prodstage[li_cur]=ls_stage_orc
					tab.pgprod.dw_prod_inx_orc.object.cntr[li_cur]=ls_null
					tab.pgprod.dw_prod_inx_orc.object.cntr_orc[li_cur]=ls_cntr_orc
					tab.pgprod.dw_prod_inx_orc.object.schstdt[li_cur]=ld_null
					tab.pgprod.dw_prod_inx_orc.object.schstdt_orc[li_cur]=ld_schstdt_orc
					tab.pgprod.dw_prod_inx_orc.object.schenddt[li_cur]=ld_null
					tab.pgprod.dw_prod_inx_orc.object.schenddt_orc[li_cur]=ld_schenddt_orc
					tab.pgprod.dw_prod_inx_orc.object.actstdt[li_cur]=ld_null
					tab.pgprod.dw_prod_inx_orc.object.actstdt_orc[li_cur]=ld_actstdt_orc
					tab.pgprod.dw_prod_inx_orc.object.actenddt[li_cur]=ld_null
					tab.pgprod.dw_prod_inx_orc.object.actenddt_orc[li_cur]=ld_actenddt_orc
					tab.pgprod.dw_prod_inx_orc.object.update_date[li_cur]=ld_null
					tab.pgprod.dw_prod_inx_orc.object.mod_date[li_cur]=ld_mod_date
					tab.pgprod.dw_prod_inx_orc.object.comment[li_cur]='No Data in PICS'
					EXIT
				elseif (ls_bkmed_orc=ls_bkmed and li_bkseq_orc< li_bkseq ) then
					k= j
					li_cur=tab.pgprod.dw_prod_inx_orc.InsertRow(0)
					tab.pgprod.dw_prod_inx_orc.object.bkmed[li_cur]=ls_bkmed_orc
					tab.pgprod.dw_prod_inx_orc.object.bkseq[li_cur]=li_bkseq_orc
					tab.pgprod.dw_prod_inx_orc.object.prodstage[li_cur]=ls_stage_orc
					tab.pgprod.dw_prod_inx_orc.object.cntr[li_cur]=ls_null
					tab.pgprod.dw_prod_inx_orc.object.cntr_orc[li_cur]=ls_cntr_orc
					tab.pgprod.dw_prod_inx_orc.object.schstdt[li_cur]=ld_null
					tab.pgprod.dw_prod_inx_orc.object.schstdt_orc[li_cur]=ld_schstdt_orc
					tab.pgprod.dw_prod_inx_orc.object.schenddt[li_cur]=ld_null
					tab.pgprod.dw_prod_inx_orc.object.schenddt_orc[li_cur]=ld_schenddt_orc
					tab.pgprod.dw_prod_inx_orc.object.actstdt[li_cur]=ld_null
					tab.pgprod.dw_prod_inx_orc.object.actstdt_orc[li_cur]=ld_actstdt_orc
					tab.pgprod.dw_prod_inx_orc.object.actenddt[li_cur]=ld_null
					tab.pgprod.dw_prod_inx_orc.object.actenddt_orc[li_cur]=ld_actenddt_orc
					tab.pgprod.dw_prod_inx_orc.object.update_date[li_cur]=ld_null
					tab.pgprod.dw_prod_inx_orc.object.mod_date[li_cur]=ld_mod_date
					tab.pgprod.dw_prod_inx_orc.object.comment[li_cur]='No Data in PICS'
					EXIT
				end if
			end if
			exit
		end if
		if lb_find=true and lb_insert=false then
			exit
		end if
	next

next
this.SetRedraw(true)
uo_progress.Visible=TRUE
uo_progress.of_SetMinimum(0)
//uo_progress.of_SetMaximum(li_count)
uo_progress.of_SetDisplayStyle(3)
uo_progress.of_SetMessageText(ls_msgtext)
uo_progress.of_SetPosition(0)
li_count=tab.pgprod.dw_prod_inx_orc.RowCount()
st_total_inx_orc.text='Difference book #: '+string(rtn)
st_process.text='Done'	
SetPointer(Arrow!)
tab.pgprod.dw_prod_inx_orc.visible=true
tab.pgprod.dw_prdrbk_cmp.visible=false
tab.pgprod.dw_prod_inx.visible=false	
tab.pgprod.dw_prod_oracle.visible=false
tab.pgprod.dw_prod_oracle_bkno.visible=false	
rb_both.visible=false
rb_infx.visible=false
rb_oracle.visible=false	
rb_prdrbk.visible=false
st_1.visible=false	

end subroutine

public function integer wf_qa ();long li_count, i,j, k, li_bkseq, li_cur, li_count2, li_row, li_re, rtn=0
string ls_med,  ls_cntr,ls_cntr_orc, ls_med_orc, ls_stg, ls_stg_orc, ls_stat, ls_stat_orc,&
		ls_null
long li_seq, li_seq_orc	
date ld_recdt, ld_recdt_orc, ld_comdt, ld_comdt_orc, ld_null		
boolean lb_find=false
This.windowstate = maximized!

SetNull(ls_null)
setNull(ld_null)
String ls_msgtext[ ] = {" 0 %","10 %", "20 %", "30 %", "40 %", "50 %", "60 %", &
									"70 %", "80 %", "90 %", "100 %"}
uo_progress.of_SetTextColor(RGB(255, 255, 255))	
st_process.text='QA retrieving data from Oracle...'
tab.pgqa.dw_prdrbk_cmp.Reset()
tab.pgqa.dw_prdrbk_cmp.SettransObject(SqlserverTrans)
//****
li_count=tab.pgqa.dw_qastg_oracle_bkno.Retrieve()

SELECT  count(*) into :li_count
 FROM PRDRQASTG a, prdrbk b
where a.bkseq=b.bkseq and a.bkmed=b.bkmed and
		(b.arflag is null or b.arflag<>'A' ) 
using SqlServerOracleTrans;
if f_check_dberror(SqlServerTrans,'select count(*) from joint of qastg and mchar')=false then
	return -1
end if
this.SetRedraw(true)
uo_progress.Visible=TRUE
uo_progress.of_SetMinimum(0)
uo_progress.of_SetMaximum(li_count * 0.1)
uo_progress.of_SetDisplayStyle(3)
uo_progress.of_SetMessageText(ls_msgtext)
uo_progress.of_SetPosition(0)


li_row=tab.pgqa.dw_qastg_orc.Retrieve()
i_orcqa= li_row
this.SetRedraw(true)
uo_progress.Visible=TRUE
uo_progress.of_SetMinimum(0)
uo_progress.of_SetMaximum(li_count * 0.1)
uo_progress.of_SetDisplayStyle(3)
uo_progress.of_SetMessageText(ls_msgtext)
uo_progress.of_SetPosition(0)
st_total_orc.text='Total in oracle: '+string(li_row)
SetNull(ld_null)
SetNull(ls_null)
if li_count=0  or li_row=0 then
	messagebox('Error','There is no book in oracle database, '+&
		'~nContact database administrator.')
	return -1
end if
st_process.text='Copy bkseq from Oracle to prdrbk_compare table and update...'


SetPointer(HourGlass!)
delete
from prdrbk_compare
using SqlServerTrans;
if f_check_dberror(SqlServerTrans,'delete from prdrbk_compare')=false then
	return -1
else
	commit using SqlServerTrans;
	select count(*) into :li_re
	from prdrbk_compare
	using SqlServerTrans;
//	messagebox('li_re','count = '+string(li_re))
end if

//rtn=tab.pgqa.dw_qastg_oracle_bkno.RowsCopy(1, li_count, Primary!,tab.pgqa.dw_prdrbk_cmp, 1, Primary!)
//tab.pgqa.dw_prdrbk_cmp.Sort()
li_count=tab.pgqa.dw_qastg_oracle_bkno.RowCount()
for i=1 to li_count
	li_bkseq=tab.pgqa.dw_qastg_oracle_bkno.object.bkseq[i]
	li_cur=tab.pgqa.dw_prdrbk_cmp.InsertRow(0)
	tab.pgqa.dw_prdrbk_cmp.object.bkseq[li_cur]= li_bkseq
next
this.SetRedraw(true)
uo_progress.Visible=TRUE
uo_progress.of_SetMinimum(0)
uo_progress.of_SetMaximum(li_count * 0.1)
uo_progress.of_SetDisplayStyle(3)
uo_progress.of_SetMessageText(ls_msgtext)
uo_progress.of_SetPosition(0)
li_re=tab.pgqa.dw_prdrbk_cmp.update()
if li_re=1 then
	commit using SqlServerTrans;
else
	RollBack using SqlServerTrans;
	return -1
end if
SetPointer(HourGlass!)
  SELECT count(*) into :li_count
    FROM qastg a, mchar b 
where a.bkseq in (select distinct bkseq from prdrbk_compare) and
		a.bkmed=b.bkmed and a.bkseq=b.bkseq and (b.arflag is null or b.arflag<>'A')
using SqlServerTrans;
if f_check_dberror(SqlServerTrans,'select count(*) from joint of qastg and mchar')=false then
	return -1
end if
this.SetRedraw(true)
uo_progress.Visible=TRUE
uo_progress.of_SetMinimum(0)
uo_progress.of_SetMaximum(li_count * 0.1)
uo_progress.of_SetDisplayStyle(3)
uo_progress.of_SetMessageText(ls_msgtext)
uo_progress.of_SetPosition(0)
st_process.text='QA retrieving data from PICS where bkseq in prdrbk_compare...'

li_row=tab.pgqa.dw_qastg_inx.Retrieve()
i_inxqa= li_row
st_total_inx.text='Total in PICS: '+string(li_row)
li_count=tab.pgqa.dw_qastg_orc.RowCount()
li_row=tab.pgqa.dw_qastg_inx.RowCount()
this.SetRedraw(true)
uo_progress.Visible=TRUE
uo_progress.of_SetMinimum(0)
uo_progress.of_SetMaximum(li_count * 00.1)
uo_progress.of_SetDisplayStyle(3)
uo_progress.of_SetMessageText(ls_msgtext)
uo_progress.of_SetPosition(0)
rtn=0
k=1
st_process.text='QA compare data of PICS and Oracle...'
for i=1 to li_count
	ls_med_orc=trim(tab.pgqa.dw_qastg_orc.object.bkmed[i])
	li_seq_orc=tab.pgqa.dw_qastg_orc.object.bkseq[i]
	ls_cntr_orc=trim(tab.pgqa.dw_qastg_orc.object.cntr[i])
	ls_stg_orc=trim(tab.pgqa.dw_qastg_orc.object.qastg[i])
	ls_stat_orc=trim(tab.pgqa.dw_qastg_orc.object.qastatcd[i])
	if ls_stat_orc='I' then continue
	ld_recdt_orc=date(tab.pgqa.dw_qastg_orc.object.qarecdt[i])
	ld_comdt_orc=date(tab.pgqa.dw_qastg_orc.object.qacompdt[i])
	if mod(i,100)=0 then
		uo_progress.of_Increment(1)
		this.SetRedraw(TRUE)
	else
		this.SetRedraw(false)
	end if
	lb_find=false
	for j=k to li_row
		ls_med=trim(tab.pgqa.dw_qastg_inx.object.bkmed[j])
		li_seq=tab.pgqa.dw_qastg_inx.object.bkseq[j]
		ls_cntr=trim(tab.pgqa.dw_qastg_inx.object.cntr[j])
		ls_stg=trim(tab.pgqa.dw_qastg_inx.object.qastg[j])
		ls_stat=trim(tab.pgqa.dw_qastg_inx.object.qastatcd[j])
		ld_recdt=(tab.pgqa.dw_qastg_inx.object.qarecdt[j])
		ld_comdt=(tab.pgqa.dw_qastg_inx.object.qacompdt[j])
		if ls_med_orc=ls_med and li_seq_orc=li_seq and ls_cntr_orc= ls_cntr then
			lb_find=true
			k=j +1
			if isNull(ls_stg) and Isnull(ls_stg_orc)=false or &
				isnull(ls_stg)=false and isnull(ls_stg_orc) or &
				ls_stg<> ls_stg_orc or &
				isNull(ls_stat) and Isnull(ls_stat_orc)=false or &
				isnull(ls_stat)=false and isnull(ls_stat_orc) or &
				ls_stat<> ls_stat_orc or &
				isNull(ld_recdt) and Isnull(ld_recdt_orc)=false or &
				isnull(ld_recdt)=false and isnull(ld_recdt_orc) or &
				ld_recdt<> ld_recdt_orc or &
				isNull(ld_comdt) and Isnull(ld_comdt_orc)=false or &
				isnull(ld_comdt)=false and isnull(ld_comdt_orc) or &
				ld_comdt<> ld_comdt_orc 											then
				li_cur=tab.pgqa.dw_qastg_inx_orc.InsertRow(0)
				tab.pgqa.dw_qastg_inx_orc.object.bkmed[li_cur]=ls_med_orc
				tab.pgqa.dw_qastg_inx_orc.object.bkseq[li_cur]=li_seq_orc
				tab.pgqa.dw_qastg_inx_orc.object.cntr_orc[li_cur]=ls_cntr_orc
				tab.pgqa.dw_qastg_inx_orc.object.qastg_orc[li_cur]=ls_stg_orc
				tab.pgqa.dw_qastg_inx_orc.object.qastatcd_orc[li_cur]=ls_stat_orc
				tab.pgqa.dw_qastg_inx_orc.object.qarecdt_orc[li_cur]=ld_recdt_orc
				tab.pgqa.dw_qastg_inx_orc.object.qacompdt_orc[li_cur]=ld_comdt_orc
				tab.pgqa.dw_qastg_inx_orc.object.cntr[li_cur]=ls_cntr
				tab.pgqa.dw_qastg_inx_orc.object.qastg[li_cur]=ls_stg
				tab.pgqa.dw_qastg_inx_orc.object.qastatcd[li_cur]=ls_stat
				tab.pgqa.dw_qastg_inx_orc.object.qarecdt[li_cur]=ld_recdt
				tab.pgqa.dw_qastg_inx_orc.object.qacompdt[li_cur]=ld_comdt
				rtn++
			end if
			exit
		elseif ls_med< ls_med_orc then
			continue
		elseif ls_med= ls_med_orc and li_seq< li_seq_orc then
			continue
		elseif ls_med=ls_med_orc and li_seq=li_seq_orc and ls_cntr< ls_cntr_orc then
			continue
		elseif (ls_med> ls_med_orc or (ls_med=ls_med_orc and li_seq> li_seq_orc) or &
			(ls_med=ls_med_orc and li_seq= li_seq_orc and ls_cntr> ls_cntr_orc)) and &
			 																					lb_find=false then
			li_cur=tab.pgqa.dw_qastg_inx_orc.InsertRow(0)
			tab.pgqa.dw_qastg_inx_orc.object.bkmed[li_cur]=ls_med_orc
			tab.pgqa.dw_qastg_inx_orc.object.bkseq[li_cur]=li_seq_orc
			tab.pgqa.dw_qastg_inx_orc.object.cntr_orc[li_cur]=ls_cntr_orc
			tab.pgqa.dw_qastg_inx_orc.object.qastg[li_cur]=ls_null
			tab.pgqa.dw_qastg_inx_orc.object.qastatcd[li_cur]=ls_null
			tab.pgqa.dw_qastg_inx_orc.object.qarecdt[li_cur]=ld_null
			tab.pgqa.dw_qastg_inx_orc.object.qacompdt[li_cur]=ld_null
			
			tab.pgqa.dw_qastg_inx_orc.object.qastg_orc[li_cur]=ls_stg_orc
			tab.pgqa.dw_qastg_inx_orc.object.qastatcd_orc[li_cur]=ls_stat_orc
			tab.pgqa.dw_qastg_inx_orc.object.qarecdt_orc[li_cur]=ld_recdt_orc
			tab.pgqa.dw_qastg_inx_orc.object.qacompdt_orc[li_cur]=ld_comdt_orc
			tab.pgqa.dw_qastg_inx_orc.object.cmts[li_cur]='No data PICS'
			k=j
			exit
		elseif (ls_med> ls_med_orc or (ls_med=ls_med_orc and li_seq> li_seq_orc) or &
			(ls_med=ls_med_orc and li_seq= li_seq_orc and ls_cntr> ls_cntr_orc)) and &
			 																					lb_find=true then
			k=j
			exit
		end if
	next
next
//li_row=tab.pgqa.dw_qastg_inx_orc.RowCount()

this.SetRedraw(true)
uo_progress.Visible=TRUE
uo_progress.of_SetMinimum(0)
uo_progress.of_SetMaximum(li_count * 0.1)
uo_progress.of_SetDisplayStyle(3)
uo_progress.of_SetMessageText(ls_msgtext)
uo_progress.of_SetPosition(0)
st_process.text='QA compare data done...'
st_total_inx_orc.text='# of Different book: '+string(rtn)
i_difqa= rtn
tab.pgqa.dw_qastg_inx.visible=false
tab.pgqa.dw_qastg_orc.visible=false
tab.pgqa.dw_qastg_inx_orc.visible=true
return 1

end function

public function integer wf_prod ();string ls_bklist[],  ls_bkmed,  ls_stage_orc, ls_stage
string		 ls_bkmed_orc, ls_arflag, ls_cntr, ls_cntr_orc, ls_null, ls_bkmed_old
long li_bkseq,li_bkseq_orc, li_row, li_count, i, j=0,k, li_re, li_cur,rtn, li_bkseq_old
//datetime ldt_schstdt, ldt_schenddt, ldt_actstdt, ldt_actenddt
date	ld_schstdt, ld_schenddt, ld_actstdt, ld_actenddt, ld_null, ld_update_date
date	ld_schstdt_orc, ld_schenddt_orc, ld_actstdt_orc, ld_actenddt_orc, ld_mod_date
boolean lb_find, lb_insert

String ls_msgtext[ ] = {" 0 %","10 %", "20 %", "30 %", "40 %", "50 %", "60 %", &
									"70 %", "80 %", "90 %", "100 %"}
uo_progress.of_SetTextColor(RGB(255, 255, 255))	
st_process.text='Prod retrieving data from Oracle...'

tab.pgprod.dw_prdrbk_cmp.Reset()
tab.pgprod.dw_prdrbk_cmp.SettransObject(SqlserverTrans)
li_count=tab.pgprod.dw_prod_oracle_bkno.Retrieve()
li_row=tab.pgprod.dw_prod_oracle.Retrieve()
i_orcprd=li_row
st_total_orc.text='Total in oracle: '+string(li_row)
SetNull(ld_null)
SetNull(ls_null)
if li_count=0  or li_row=0 then
	messagebox('Error','There is no book in oracle database, '+&
		'~nContact database administrator.')
	return -1
end if
st_process.text='Copy bkseq from Oracle to prdrbk_compare table and update...'
this.SetRedraw(true)
uo_progress.Visible=TRUE
uo_progress.of_SetMinimum(0)
uo_progress.of_SetMaximum(li_count * 0.1)
uo_progress.of_SetDisplayStyle(3)
uo_progress.of_SetMessageText(ls_msgtext)
uo_progress.of_SetPosition(0)
ls_bkmed_old=""
li_bkseq_old=0
SetPointer(HourGlass!)
delete
from prdrbk_compare
using SqlServerTrans;
if f_check_dberror(SqlServerTrans,'delete from prdrbk_compare')=false then
	return -1
else
	commit using SqlServerTrans;
	select count(*) into :li_re
	from prdrbk_compare
	using SqlServerTrans;
//	messagebox('li_re','count = '+string(li_re))
end if
rtn=tab.pgprod.dw_prod_oracle_bkno.RowsCopy(1, li_count, Primary!,tab.pgprod.dw_prdrbk_cmp, 1, Primary!)
tab.pgprod.dw_prdrbk_cmp.Sort()


if rtn=1 then
	li_re=tab.pgprod.dw_prdrbk_cmp.update()
	if li_re=1 then
		commit using SqlServerTrans;
	else
		RollBack using SqlServerTrans;
		return -1
	end if
else
	return -1
end if
SetPointer(HourGlass!)
  SELECT count(*) into :li_count
    FROM prod a, mchar b 
where a.bkseq in (select distinct bkseq from prdrbk_compare) and
		a.bkmed=b.bkmed and a.bkseq=b.bkseq
using SqlServerTrans;
if f_check_dberror(SqlServerTrans,'select count(*) from joint of prod and mchar')=false then
	return -1
end if
this.SetRedraw(true)
uo_progress.Visible=TRUE
uo_progress.of_SetMinimum(0)
uo_progress.of_SetMaximum(li_count * 0.1)
uo_progress.of_SetDisplayStyle(3)
uo_progress.of_SetMessageText(ls_msgtext)
uo_progress.of_SetPosition(0)
st_process.text='Prod retrieving data from PICS where bkseq in prdrbk_compare...'
tab.pgprod.dw_prod_inx.Retrieve()
li_count=tab.pgprod.dw_prod_oracle.RowCount()
this.SetRedraw(true)
uo_progress.Visible=TRUE
uo_progress.of_SetMinimum(0)
uo_progress.of_SetMaximum(li_count * 0.1)
uo_progress.of_SetDisplayStyle(3)
uo_progress.of_SetMessageText(ls_msgtext)
uo_progress.of_SetPosition(0)
tab.pgprod.dw_prod_inx.Sort()
li_row=tab.pgprod.dw_prod_inx.RowCount()
i_inxprd= li_row
st_total_inx.text='Total in PICS: '+string(li_row)
if li_row=0 then
	messagebox('Error','There is no book in PICS database, '+&
		'~nContact database administrator.')
	return -1
end if
st_process.text='Prod Compare Oracle and PICS'
k=1
rtn=0
FOR i=1 to li_count
	if mod(i, 10)=0 then
		uo_progress.of_Increment(1)
		this.SetRedraw(TRUE)
	else
		this.SetRedraw(false)
	end if	
//	ls_bkno_orc=trim(tab.pgprod.dw_prod_oracle.object.bkno[i])
	ls_stage_orc=trim(tab.pgprod.dw_prod_oracle.object.prodstage[i])
	ls_bkmed_orc=trim(tab.pgprod.dw_prod_oracle.object.bkmed[i])
	li_bkseq_orc=tab.pgprod.dw_prod_oracle.object.bkseq[i]
	
	ls_cntr_orc=trim(tab.pgprod.dw_prod_oracle.object.cntr[i])
	ld_schstdt_orc=date(tab.pgprod.dw_prod_oracle.object.schstdt[i])
	ld_schenddt_orc=date(tab.pgprod.dw_prod_oracle.object.schenddt[i])
	ld_actstdt_orc=date(tab.pgprod.dw_prod_oracle.object.actstdt[i])
	ld_actenddt_orc=date(tab.pgprod.dw_prod_oracle.object.actenddt[i])
	ld_mod_date=date(tab.pgprod.dw_prod_oracle.object.mod_date[i])
	lb_find=false
	lb_insert=false
	for j=k to li_row
//		ls_bkno=trim(tab.pgprod.dw_prod_inx.object.bkno[j])
		ls_stage=trim(tab.pgprod.dw_prod_inx.object.prodstage[j])
		ls_bkmed=trim(tab.pgprod.dw_prod_inx.object.bkmed[j])
		li_bkseq=tab.pgprod.dw_prod_inx.object.bkseq[j]
		ls_cntr=trim(tab.pgprod.dw_prod_inx.object.cntr[j])
		ls_arflag=trim(tab.pgprod.dw_prod_inx.object.arflag[j])
		ld_schstdt=date(tab.pgprod.dw_prod_inx.object.schstdt[j])
		ld_schenddt=date(tab.pgprod.dw_prod_inx.object.schenddt[j])
		ld_actstdt=date(tab.pgprod.dw_prod_inx.object.actstdt[j])
		ld_actenddt=date(tab.pgprod.dw_prod_inx.object.actenddt[j])
		ld_update_date=date(tab.pgprod.dw_prod_inx.object.update_date[j])
		if ls_bkmed_orc=ls_bkmed and li_bkseq_orc= li_bkseq and ls_stage_orc=ls_stage then
			lb_find=true
			k= j+1
			if isnull(ld_schstdt_orc) and isnull(ld_schstdt)=false or &
				isnull(ld_schstdt_orc)=false and isnull(ld_schstdt)=true or &
				ld_schstdt_orc<> ld_schstdt or &
				isnull(ld_schenddt_orc) and isnull(ld_schenddt)=false or &
				isnull(ld_schenddt_orc)=false and isnull(ld_schenddt)=true or &
				ld_schenddt_orc<> ld_schenddt or &
				isnull(ld_actstdt_orc) and isnull(ld_actstdt)=false or &
				isnull(ld_actstdt_orc)=false and isnull(ld_actstdt)=true or &
				ld_actstdt_orc<> ld_actstdt or &
				isnull(ld_actenddt_orc) and isnull(ld_actenddt)=false or &
				isnull(ld_actenddt_orc)=false and isnull(ld_actenddt)=true or &
				ld_actenddt_orc<> ld_actenddt or ls_cntr<> ls_cntr_orc then
				if ls_bkmed<>ls_bkmed_old or li_bkseq<>li_bkseq_old then
					tab.pgprod.dw_prod_inx.object.diff[j]='Y'
					rtn++
					ls_bkmed_old=ls_bkmed
					li_bkseq_old=li_bkseq
				end if
				lb_insert=true
				li_cur=tab.pgprod.dw_prod_inx_orc.InsertRow(0)
				tab.pgprod.dw_prod_inx_orc.object.bkmed[li_cur]=ls_bkmed
				tab.pgprod.dw_prod_inx_orc.object.bkseq[li_cur]=li_bkseq
				tab.pgprod.dw_prod_inx_orc.object.prodstage[li_cur]=ls_stage
				tab.pgprod.dw_prod_inx_orc.object.cntr[li_cur]=ls_cntr
				tab.pgprod.dw_prod_inx_orc.object.cntr_orc[li_cur]=ls_cntr_orc
				tab.pgprod.dw_prod_inx_orc.object.schstdt[li_cur]=ld_schstdt
				tab.pgprod.dw_prod_inx_orc.object.schstdt_orc[li_cur]=ld_schstdt_orc
				tab.pgprod.dw_prod_inx_orc.object.schenddt[li_cur]=ld_schenddt
				tab.pgprod.dw_prod_inx_orc.object.schenddt_orc[li_cur]=ld_schenddt_orc
				tab.pgprod.dw_prod_inx_orc.object.actstdt[li_cur]=ld_actstdt
				tab.pgprod.dw_prod_inx_orc.object.actstdt_orc[li_cur]=ld_actstdt_orc
				tab.pgprod.dw_prod_inx_orc.object.actenddt[li_cur]=ld_actenddt
				tab.pgprod.dw_prod_inx_orc.object.actenddt_orc[li_cur]=ld_actenddt_orc
				tab.pgprod.dw_prod_inx_orc.object.update_date[li_cur]=ld_update_date
				tab.pgprod.dw_prod_inx_orc.object.mod_date[li_cur]=ld_mod_date
				if relativedate(ld_schenddt_orc,1)=ld_schenddt or &
								relativedate(ld_schenddt, 1)=ld_schenddt_orc then
					tab.pgprod.dw_prod_inx_orc.object.comment[li_cur]='Only one day'
				end if
				if ls_cntr<> ls_cntr_orc then
					tab.pgprod.dw_prod_inx_orc.object.comment[li_cur]='Cntr is different'
				END IF
				if ls_arflag='Y' then
					tab.pgprod.dw_prod_inx_orc.object.comment[li_cur]='ARCHIVES'
				END IF
				EXIT
			end if 
		elseif (ls_bkmed_orc<ls_bkmed or ls_bkmed_orc=ls_bkmed and li_bkseq_orc< li_bkseq ) or &
				(ls_bkmed_orc=ls_bkmed and li_bkseq_orc=li_bkseq and ls_stage_orc<ls_stage) then
			if lb_find=false then
				
				if (ls_bkmed_orc=ls_bkmed and li_bkseq_orc=li_bkseq and ls_stage_orc<ls_stage) then
					k=j 
					li_cur=tab.pgprod.dw_prod_inx_orc.InsertRow(0)
					tab.pgprod.dw_prod_inx_orc.object.bkmed[li_cur]=ls_bkmed_orc
					tab.pgprod.dw_prod_inx_orc.object.bkseq[li_cur]=li_bkseq_orc
					tab.pgprod.dw_prod_inx_orc.object.prodstage[li_cur]=ls_stage_orc
					tab.pgprod.dw_prod_inx_orc.object.cntr[li_cur]=ls_null
					tab.pgprod.dw_prod_inx_orc.object.cntr_orc[li_cur]=ls_cntr_orc
					tab.pgprod.dw_prod_inx_orc.object.schstdt[li_cur]=ld_null
					tab.pgprod.dw_prod_inx_orc.object.schstdt_orc[li_cur]=ld_schstdt_orc
					tab.pgprod.dw_prod_inx_orc.object.schenddt[li_cur]=ld_null
					tab.pgprod.dw_prod_inx_orc.object.schenddt_orc[li_cur]=ld_schenddt_orc
					tab.pgprod.dw_prod_inx_orc.object.actstdt[li_cur]=ld_null
					tab.pgprod.dw_prod_inx_orc.object.actstdt_orc[li_cur]=ld_actstdt_orc
					tab.pgprod.dw_prod_inx_orc.object.actenddt[li_cur]=ld_null
					tab.pgprod.dw_prod_inx_orc.object.actenddt_orc[li_cur]=ld_actenddt_orc
					tab.pgprod.dw_prod_inx_orc.object.update_date[li_cur]=ld_null
					tab.pgprod.dw_prod_inx_orc.object.mod_date[li_cur]=ld_mod_date
					tab.pgprod.dw_prod_inx_orc.object.comment[li_cur]='PICS no data'
					EXIT
				elseif (ls_bkmed_orc<ls_bkmed) then
					k= j
					li_cur=tab.pgprod.dw_prod_inx_orc.InsertRow(0)
					tab.pgprod.dw_prod_inx_orc.object.bkmed[li_cur]=ls_bkmed_orc
					tab.pgprod.dw_prod_inx_orc.object.bkseq[li_cur]=li_bkseq_orc
					tab.pgprod.dw_prod_inx_orc.object.prodstage[li_cur]=ls_stage_orc
					tab.pgprod.dw_prod_inx_orc.object.cntr[li_cur]=ls_null
					tab.pgprod.dw_prod_inx_orc.object.cntr_orc[li_cur]=ls_cntr_orc
					tab.pgprod.dw_prod_inx_orc.object.schstdt[li_cur]=ld_null
					tab.pgprod.dw_prod_inx_orc.object.schstdt_orc[li_cur]=ld_schstdt_orc
					tab.pgprod.dw_prod_inx_orc.object.schenddt[li_cur]=ld_null
					tab.pgprod.dw_prod_inx_orc.object.schenddt_orc[li_cur]=ld_schenddt_orc
					tab.pgprod.dw_prod_inx_orc.object.actstdt[li_cur]=ld_null
					tab.pgprod.dw_prod_inx_orc.object.actstdt_orc[li_cur]=ld_actstdt_orc
					tab.pgprod.dw_prod_inx_orc.object.actenddt[li_cur]=ld_null
					tab.pgprod.dw_prod_inx_orc.object.actenddt_orc[li_cur]=ld_actenddt_orc
					tab.pgprod.dw_prod_inx_orc.object.update_date[li_cur]=ld_null
					tab.pgprod.dw_prod_inx_orc.object.mod_date[li_cur]=ld_mod_date
					tab.pgprod.dw_prod_inx_orc.object.comment[li_cur]='PICS no data'
					EXIT
				elseif (ls_bkmed_orc=ls_bkmed and li_bkseq_orc< li_bkseq ) then
					k= j
					li_cur=tab.pgprod.dw_prod_inx_orc.InsertRow(0)
					tab.pgprod.dw_prod_inx_orc.object.bkmed[li_cur]=ls_bkmed_orc
					tab.pgprod.dw_prod_inx_orc.object.bkseq[li_cur]=li_bkseq_orc
					tab.pgprod.dw_prod_inx_orc.object.prodstage[li_cur]=ls_stage_orc
					tab.pgprod.dw_prod_inx_orc.object.cntr[li_cur]=ls_null
					tab.pgprod.dw_prod_inx_orc.object.cntr_orc[li_cur]=ls_cntr_orc
					tab.pgprod.dw_prod_inx_orc.object.schstdt[li_cur]=ld_null
					tab.pgprod.dw_prod_inx_orc.object.schstdt_orc[li_cur]=ld_schstdt_orc
					tab.pgprod.dw_prod_inx_orc.object.schenddt[li_cur]=ld_null
					tab.pgprod.dw_prod_inx_orc.object.schenddt_orc[li_cur]=ld_schenddt_orc
					tab.pgprod.dw_prod_inx_orc.object.actstdt[li_cur]=ld_null
					tab.pgprod.dw_prod_inx_orc.object.actstdt_orc[li_cur]=ld_actstdt_orc
					tab.pgprod.dw_prod_inx_orc.object.actenddt[li_cur]=ld_null
					tab.pgprod.dw_prod_inx_orc.object.actenddt_orc[li_cur]=ld_actenddt_orc
					tab.pgprod.dw_prod_inx_orc.object.update_date[li_cur]=ld_null
					tab.pgprod.dw_prod_inx_orc.object.mod_date[li_cur]=ld_mod_date
					tab.pgprod.dw_prod_inx_orc.object.comment[li_cur]='PICS no data'
					EXIT
				end if
			end if
			exit
		end if
		if lb_find=true and lb_insert=false then
			exit
		end if
	next

next
this.SetRedraw(true)
uo_progress.Visible=TRUE
uo_progress.of_SetMinimum(0)
//uo_progress.of_SetMaximum(li_count)
uo_progress.of_SetDisplayStyle(3)
uo_progress.of_SetMessageText(ls_msgtext)
uo_progress.of_SetPosition(0)
li_count=tab.pgprod.dw_prod_inx_orc.RowCount()
st_total_inx_orc.text='# of Different book: '+string(rtn)
i_difprd= rtn
st_process.text='Prod Done'	
SetPointer(Arrow!)
tab.pgprod.dw_prod_inx_orc.visible=true
tab.pgprod.dw_prdrbk_cmp.visible=false
tab.pgprod.dw_prod_inx.visible=false	
tab.pgprod.dw_prod_oracle.visible=false
tab.pgprod.dw_prod_oracle_bkno.visible=false	
rb_both.visible=false
rb_infx.visible=false
rb_oracle.visible=false	
rb_prdrbk.visible=false
st_1.visible=false	
return 1

end function

public function integer wf_book ();long li_count, i,j, k, li_cur, li_count2, li_row, li_re, rtn=0,&
	li_vols, li_vols_orc, li_aplen, li_aplen_orc, li_len, li_len_orc, li_qnty, li_qnty_orc,&
	li_side, li_side_orc, li_null, li_bkseq
string ls_med,  ls_med_orc 
long li_seq, li_seq_orc	
date ld_mod_date
datetime ldt_updt, ldt_null
boolean lb_find=false
This.windowstate = maximized!

SetNull(li_null)
SetNull(ldt_null)
String ls_msgtext[ ] = {" 0 %","10 %", "20 %", "30 %", "40 %", "50 %", "60 %", &
									"70 %", "80 %", "90 %", "100 %"}
uo_progress.of_SetTextColor(RGB(255, 255, 255))	
st_process.text='Book retrieving data from Oracle...'
tab.pgbk.dw_prdrbk_cmp.Reset()
tab.pgbk.dw_prdrbk_cmp.SettransObject(SqlserverTrans)
//*****
li_count=tab.pgbk.dw_book_oracle_bkno.Retrieve()
li_row=tab.pgbk.dw_book_orc.Retrieve()
i_orcbk= li_row
st_total_orc.text='Total in oracle: '+string(li_row)

if li_count=0  or li_row=0 then
	messagebox('Error','There is no book in oracle database, '+&
		'~nContact database administrator.')
	return - 1
end if
st_process.text='Copy bkseq from Oracle to prdrbk_compare table and update...'


SetPointer(HourGlass!)
delete
from prdrbk_compare
using SqlServerTrans;
if f_check_dberror(SqlServerTrans,'delete from prdrbk_compare')=false then
	return - 1
else
	commit using SqlServerTrans;
	select count(*) into :li_re
	from prdrbk_compare
	using SqlServerTrans;

end if 

li_count=tab.pgbk.dw_book_oracle_bkno.RowCount()
for i=1 to li_count
	li_bkseq=tab.pgbk.dw_book_oracle_bkno.object.bkseq[i]
	li_cur=tab.pgbk.dw_prdrbk_cmp.InsertRow(0)
	tab.pgbk.dw_prdrbk_cmp.object.bkseq[li_cur]= li_bkseq
next

this.SetRedraw(true)
uo_progress.Visible=TRUE
uo_progress.of_SetMinimum(0)
uo_progress.of_SetMaximum(li_count * 0.1)
uo_progress.of_SetDisplayStyle(3)
uo_progress.of_SetMessageText(ls_msgtext)
uo_progress.of_SetPosition(0)
li_re=tab.pgbk.dw_prdrbk_cmp.update()
if li_re=1 then
	commit using SqlServerTrans;
else
	RollBack using SqlServerTrans;
	return - 1
end if
SetPointer(HourGlass!)
  SELECT count(*) into :li_count
    FROM mchar 
where bkseq in (select distinct bkseq from prdrbk_compare) and
		 (arflag is null or arflag<>'A')
using SqlServerTrans;
if f_check_dberror(SqlServerTrans,'select count(*) from mchar bkseq in...')=false then
	return - 1
end if

st_process.text='Book retrieving data from mchar where bkseq in prdrbk_compare...'
//******
this.SetRedraw(true)
uo_progress.Visible=TRUE
uo_progress.of_SetMinimum(0)
uo_progress.of_SetMaximum(li_count * 0.1)
uo_progress.of_SetDisplayStyle(3)
uo_progress.of_SetMessageText(ls_msgtext)
uo_progress.of_SetPosition(0)

li_row=tab.pgbk.dw_book_inx.Retrieve()
st_total_inx.text='Total in PICS: '+string(li_row)
li_count=tab.pgbk.dw_book_orc.RowCount()
li_row=tab.pgbk.dw_book_inx.RowCount()
i_inxbk= li_row
this.SetRedraw(true)
uo_progress.Visible=TRUE
uo_progress.of_SetMinimum(0)
uo_progress.of_SetMaximum(li_count * 00.1)
uo_progress.of_SetDisplayStyle(3)
uo_progress.of_SetMessageText(ls_msgtext)
uo_progress.of_SetPosition(0)
rtn=0
k=1
st_process.text='Book compare data of PICS and Oracle...'
for i=1 to li_count
	ls_med_orc=trim(tab.pgbk.dw_book_orc.object.bkmed[i])
	li_seq_orc=tab.pgbk.dw_book_orc.object.bkseq[i]
	li_vols_orc=(tab.pgbk.dw_book_orc.object.vols[i])
	li_len_orc=(tab.pgbk.dw_book_orc.object.len[i])
	li_aplen_orc=(tab.pgbk.dw_book_orc.object.applen[i])
	li_side_orc=(tab.pgbk.dw_book_orc.object.minlastside[i])
	li_qnty_orc=(tab.pgbk.dw_book_orc.object.qnty[i])
	ld_mod_date=date(tab.pgbk.dw_book_orc.object.mod_date[i])
	if mod(i,100)=0 then
		uo_progress.of_Increment(1)
		this.SetRedraw(TRUE)
	else
		this.SetRedraw(false)
	end if
	lb_find=false
	for j=k to li_row
		ls_med=trim(tab.pgbk.dw_book_inx.object.bkmed[j])
		li_seq=tab.pgbk.dw_book_inx.object.bkseq[j]
		li_vols=tab.pgbk.dw_book_inx.object.vols[j]
		li_len=tab.pgbk.dw_book_inx.object.len[j]
		li_aplen=tab.pgbk.dw_book_inx.object.applen[j]
		li_side=tab.pgbk.dw_book_inx.object.minlastside[j]
		li_qnty=tab.pgbk.dw_book_inx.object.qnty[j]
		ldt_updt=tab.pgbk.dw_book_inx.object.update_date[j]
		if ls_med_orc=ls_med and li_seq_orc=li_seq  then
			lb_find=true
			if Isnull(li_vols_orc) and Isnull(li_vols)=false or &
				Isnull(li_vols_orc)=false and Isnull(li_vols) or &
				li_vols_orc<> li_vols or &
				Isnull(li_len_orc) and Isnull(li_len)=false or &
				Isnull(li_len_orc)=false and Isnull(li_len) or &
				li_len_orc<> li_len or &
				Isnull(li_aplen_orc) and Isnull(li_aplen)=false or &
				Isnull(li_aplen_orc)=false and Isnull(li_aplen) or &
				li_aplen_orc<> li_aplen or &
				Isnull(li_side_orc) and Isnull(li_side)=false or &
				Isnull(li_side_orc)=false and Isnull(li_side) or &
				li_side_orc<> li_side or &
				Isnull(li_qnty_orc) and Isnull(li_qnty)=false or &
				Isnull(li_qnty_orc)=false and Isnull(li_qnty) or &
				li_qnty_orc<> li_qnty 												then
				li_cur=tab.pgbk.dw_book_inx_orc.InsertRow(0)
				tab.pgbk.dw_book_inx_orc.object.bkmed[li_cur]=ls_med_orc
				tab.pgbk.dw_book_inx_orc.object.bkseq[li_cur]=li_seq_orc
				tab.pgbk.dw_book_inx_orc.object.vols_orc[li_cur]=li_vols_orc
				tab.pgbk.dw_book_inx_orc.object.len_orc[li_cur]=li_len_orc
				tab.pgbk.dw_book_inx_orc.object.applen_orc[li_cur]=li_aplen_orc
				tab.pgbk.dw_book_inx_orc.object.minlastside_orc[li_cur]=li_side_orc
				tab.pgbk.dw_book_inx_orc.object.qnty_orc[li_cur]=li_qnty_orc
				tab.pgbk.dw_book_inx_orc.object.mod_date[li_cur]=ld_mod_date
				
				tab.pgbk.dw_book_inx_orc.object.vols[li_cur]=li_vols
				tab.pgbk.dw_book_inx_orc.object.len[li_cur]=li_len
				tab.pgbk.dw_book_inx_orc.object.applen[li_cur]=li_aplen
				tab.pgbk.dw_book_inx_orc.object.minlastside[li_cur]=li_side
				tab.pgbk.dw_book_inx_orc.object.qnty[li_cur]=li_qnty
				tab.pgbk.dw_book_inx_orc.object.update_date[li_cur]=ldt_updt
				tab.pgbk.dw_book_inx_orc.object.diff[li_cur]='Y'
				rtn++
			end if
			k=j +1
			exit
		elseif ls_med< ls_med_orc then
			continue
		elseif ls_med= ls_med_orc and li_seq< li_seq_orc then
			continue
		elseif (ls_med> ls_med_orc or (ls_med=ls_med_orc and li_seq> li_seq_orc)) AND &
																			lb_find=false then
			li_cur=tab.pgbk.dw_book_inx_orc.InsertRow(0)	
			tab.pgbk.dw_book_inx_orc.object.bkmed[li_cur]=ls_med_orc
			tab.pgbk.dw_book_inx_orc.object.bkseq[li_cur]=li_seq_orc
			tab.pgbk.dw_book_inx_orc.object.vols_orc[li_cur]=li_vols_orc
			tab.pgbk.dw_book_inx_orc.object.len_orc[li_cur]=li_len_orc
			tab.pgbk.dw_book_inx_orc.object.applen_orc[li_cur]=li_aplen_orc
			tab.pgbk.dw_book_inx_orc.object.minlastside_orc[li_cur]=li_side_orc
			tab.pgbk.dw_book_inx_orc.object.qnty_orc[li_cur]=li_qnty_orc
			tab.pgbk.dw_book_inx_orc.object.mod_date[li_cur]=ld_mod_date
			
			tab.pgbk.dw_book_inx_orc.object.vols[li_cur]=li_null
			tab.pgbk.dw_book_inx_orc.object.len[li_cur]=li_null
			tab.pgbk.dw_book_inx_orc.object.applen[li_cur]=li_null
			tab.pgbk.dw_book_inx_orc.object.minlastside[li_cur]=li_null
			tab.pgbk.dw_book_inx_orc.object.qnty[li_cur]=li_null
			tab.pgbk.dw_book_inx_orc.object.update_date[li_cur]=ldt_null
			tab.pgbk.dw_book_inx_orc.object.cmts[li_cur]='PICS no data'
			k=j
			exit

		end if
	next
next

this.SetRedraw(true)
uo_progress.Visible=TRUE
uo_progress.of_SetMinimum(0)
uo_progress.of_SetMaximum(li_count * 0.1)
uo_progress.of_SetDisplayStyle(3)
uo_progress.of_SetMessageText(ls_msgtext)
uo_progress.of_SetPosition(0)
st_process.text='Book compare data done...'
st_total_inx_orc.text='# of Different book: '+string(rtn)
i_difbk= rtn
tab.pgbk.dw_book_inx.visible=false
tab.pgbk.dw_book_orc.visible=false
tab.pgbk.dw_book_inx_orc.visible=true
return 1

end function

on w_cmpare_orcl_infx.create
int iCurrent
call super::create
this.st_3stage=create st_3stage
this.cb_orc=create cb_orc
this.cb_inx=create cb_inx
this.cb_inxorc=create cb_inxorc
this.tab=create tab
this.rb_prdrbk=create rb_prdrbk
this.dw_prod_oracle_bkno=create dw_prod_oracle_bkno
this.rb_mod_date=create rb_mod_date
this.rb_upd_date=create rb_upd_date
this.rb_bkno=create rb_bkno
this.st_2=create st_2
this.rb_both=create rb_both
this.rb_infx=create rb_infx
this.rb_oracle=create rb_oracle
this.st_1=create st_1
this.cb_update=create cb_update
this.uo_progress=create uo_progress
this.cb_print=create cb_print
this.st_total_inx_orc=create st_total_inx_orc
this.st_total_inx=create st_total_inx
this.st_total_orc=create st_total_orc
this.st_process=create st_process
this.cb_exit=create cb_exit
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_3stage
this.Control[iCurrent+2]=this.cb_orc
this.Control[iCurrent+3]=this.cb_inx
this.Control[iCurrent+4]=this.cb_inxorc
this.Control[iCurrent+5]=this.tab
this.Control[iCurrent+6]=this.rb_prdrbk
this.Control[iCurrent+7]=this.dw_prod_oracle_bkno
this.Control[iCurrent+8]=this.rb_mod_date
this.Control[iCurrent+9]=this.rb_upd_date
this.Control[iCurrent+10]=this.rb_bkno
this.Control[iCurrent+11]=this.st_2
this.Control[iCurrent+12]=this.rb_both
this.Control[iCurrent+13]=this.rb_infx
this.Control[iCurrent+14]=this.rb_oracle
this.Control[iCurrent+15]=this.st_1
this.Control[iCurrent+16]=this.cb_update
this.Control[iCurrent+17]=this.uo_progress
this.Control[iCurrent+18]=this.cb_print
this.Control[iCurrent+19]=this.st_total_inx_orc
this.Control[iCurrent+20]=this.st_total_inx
this.Control[iCurrent+21]=this.st_total_orc
this.Control[iCurrent+22]=this.st_process
this.Control[iCurrent+23]=this.cb_exit
end on

on w_cmpare_orcl_infx.destroy
call super::destroy
destroy(this.st_3stage)
destroy(this.cb_orc)
destroy(this.cb_inx)
destroy(this.cb_inxorc)
destroy(this.tab)
destroy(this.rb_prdrbk)
destroy(this.dw_prod_oracle_bkno)
destroy(this.rb_mod_date)
destroy(this.rb_upd_date)
destroy(this.rb_bkno)
destroy(this.st_2)
destroy(this.rb_both)
destroy(this.rb_infx)
destroy(this.rb_oracle)
destroy(this.st_1)
destroy(this.cb_update)
destroy(this.uo_progress)
destroy(this.cb_print)
destroy(this.st_total_inx_orc)
destroy(this.st_total_inx)
destroy(this.st_total_orc)
destroy(this.st_process)
destroy(this.cb_exit)
end on

event open;call super::open;
this.of_SetBase(true)
this.inv_base.of_Center()

end event

event pfc_preopen();call super::pfc_preopen;this.windowstate = maximized!

this.of_SetResize(TRUE)
inv_resize.of_Register(tab, "Scale")
inv_resize.of_Register(tab.pgprod, "Scale")
inv_resize.of_Register(tab.pgqa, "Scale")
inv_resize.of_Register(tab.pgbk, "Scale")
inv_resize.of_Register(tab.pgprod.dw_prod_inx_orc, "Scale")
inv_resize.of_Register(tab.pgprod.dw_prod_oracle, "Scale")
inv_resize.of_Register(tab.pgprod.dw_prod_inx, "Scale")
inv_resize.of_Register(tab.pgprod.dw_prod_oracle_bkno, "Scale")
inv_resize.of_Register(tab.pgprod.dw_prdrbk_cmp, "Scale")

inv_resize.of_Register(tab.pgqa.dw_qastg_inx, "scale")
inv_resize.of_Register(tab.pgqa.dw_qastg_orc, "scale")
inv_resize.of_Register(tab.pgqa.dw_qastg_inx_orc, "scale")

inv_resize.of_Register(tab.pgbk.dw_book_inx, "scale")
inv_resize.of_Register(tab.pgbk.dw_book_orc, "scale")
inv_resize.of_Register(tab.pgbk.dw_book_inx_orc, "scale")

inv_resize.of_Register(rb_upd_date, "Scale")
inv_resize.of_Register(rb_mod_date, "Scale")
inv_resize.of_Register(rb_bkno, "Scale")
inv_resize.of_Register(rb_infx, "Scale")
inv_resize.of_Register(rb_oracle, "Scale")
inv_resize.of_Register(rb_both, "Scale")
inv_resize.of_Register(rb_prdrbk, "Scale")
inv_resize.of_Register(cb_exit, "Scale")
inv_resize.of_Register(cb_print, "Scale")
inv_resize.of_Register(cb_update, "Scale")
inv_resize.of_Register(cb_inx, "Scale")
inv_resize.of_Register(cb_inxorc, "Scale")
inv_resize.of_Register(cb_orc, "Scale")
inv_resize.of_Register(uo_progress, "Scale")
inv_resize.of_Register(st_1, "Scale")
inv_resize.of_Register(st_2, "Scale")
inv_resize.of_Register(st_3stage, "Scale")
inv_resize.of_Register(st_process, "Scale")
inv_resize.of_Register(st_total_inx, "Scale")
inv_resize.of_Register(st_total_orc, "Scale")
inv_resize.of_Register(st_total_inx_orc, "Scale")
end event

event pfc_postopen();call super::pfc_postopen;long rtn

st_3stage.text='In process Prod'
rtn =wf_prod()

if rtn = -1 then
	return
//else
//	st_3stage.text='In process QA'
//	rtn=wf_qa()
//	if rtn= - 1 then
//		return
//	else
//		st_3stage.text='In process Book'
//		rtn=wf_book()
//		if rtn= - 1 then
//			return
//		end if
//	end if
end if
//st_3stage.text='Prod done'
rb_both.visible=false
rb_infx.visible=false
rb_oracle.visible=false	
rb_prdrbk.visible=false
st_1.visible=false
cb_update.visible=false
rb_bkno.visible=true
rb_mod_date.visible=true
rb_upd_date.visible=true
st_2.visible=true
end event

type st_3stage from statictext within w_cmpare_orcl_infx
integer x = 78
integer y = 1428
integer width = 631
integer height = 60
integer textsize = -8
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 33554432
long backcolor = 67108864
alignment alignment = center!
boolean focusrectangle = false
end type

type cb_orc from commandbutton within w_cmpare_orcl_infx
integer x = 2656
integer y = 1464
integer width = 178
integer height = 96
integer taborder = 90
boolean bringtotop = true
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "&Web"
boolean cancel = true
end type

event clicked;long indx

indx= i_index
choose case indx
	case 1
		tab.pgprod.dw_prod_inx_orc.visible=false
		tab.pgprod.dw_prdrbk_cmp.visible=false
		tab.pgprod.dw_prod_inx.visible=false	
		tab.pgprod.dw_prod_oracle.visible=true
		tab.pgprod.dw_prod_oracle_bkno.visible=false	
		cb_update.visible=false
		rb_bkno.visible=false
		rb_mod_date.visible=false
		rb_upd_date.visible=false
		st_2.visible=false
	case 2
		tab.pgqa.dw_qastg_inx_orc.visible=false
		tab.pgqa.dw_prdrbk_cmp.visible=false
		tab.pgqa.dw_qastg_inx.visible=false	
		tab.pgqa.dw_qastg_orc.visible=true
		tab.pgqa.dw_qastg_oracle_bkno.visible=false
	case 3
		tab.pgbk.dw_book_inx_orc.visible=false
		tab.pgbk.dw_prdrbk_cmp.visible=false
		tab.pgbk.dw_book_inx.visible=false	
		tab.pgbk.dw_book_orc.visible=true
		tab.pgbk.dw_book_oracle_bkno.visible=false
end choose
end event

type cb_inx from commandbutton within w_cmpare_orcl_infx
integer x = 2437
integer y = 1464
integer width = 183
integer height = 96
integer taborder = 80
boolean bringtotop = true
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "&PICS"
boolean cancel = true
end type

event clicked;long indx

indx=i_index
choose case indx
	case 1
		tab.pgprod.dw_prod_inx_orc.visible=false
		tab.pgprod.dw_prdrbk_cmp.visible=false
		tab.pgprod.dw_prod_inx.visible=true	
		tab.pgprod.dw_prod_oracle.visible=false
		tab.pgprod.dw_prod_oracle_bkno.visible=false	
		cb_update.visible=false
		rb_bkno.visible=false
		rb_mod_date.visible=false
		rb_upd_date.visible=false
		st_2.visible=false
	case 2
		tab.pgqa.dw_qastg_inx_orc.visible=false
		tab.pgqa.dw_prdrbk_cmp.visible=false
		tab.pgqa.dw_qastg_inx.visible=true	
		tab.pgqa.dw_qastg_orc.visible=false
		tab.pgqa.dw_qastg_oracle_bkno.visible=false
		cb_update.visible=false
		rb_bkno.visible=false
		rb_mod_date.visible=false
		rb_upd_date.visible=false
		st_2.visible=false
	case 3
		tab.pgbk.dw_book_inx_orc.visible=false
		tab.pgbk.dw_prdrbk_cmp.visible=false
		tab.pgbk.dw_book_inx.visible=true	
		tab.pgbk.dw_book_orc.visible=false
		tab.pgbk.dw_book_oracle_bkno.visible=false
		cb_update.visible=false
		rb_bkno.visible=false
		rb_mod_date.visible=false
		rb_upd_date.visible=false
		st_2.visible=false
end choose
end event

type cb_inxorc from commandbutton within w_cmpare_orcl_infx
integer x = 2089
integer y = 1464
integer width = 311
integer height = 96
integer taborder = 70
boolean bringtotop = true
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "&PICS/Web"
boolean cancel = true
end type

event clicked;long indx

indx=i_index

choose case indx
	case 1
		tab.pgprod.dw_prod_inx_orc.visible=true
		tab.pgprod.dw_prdrbk_cmp.visible=false
		tab.pgprod.dw_prod_inx.visible=false	
		tab.pgprod.dw_prod_oracle.visible=false
		tab.pgprod.dw_prod_oracle_bkno.visible=false	
		cb_update.visible=false
		rb_bkno.visible=true
		rb_mod_date.visible=true
		rb_upd_date.visible=true
		st_2.visible=true
	case 2
		tab.pgqa.dw_qastg_inx_orc.visible=true
		tab.pgqa.dw_prdrbk_cmp.visible=false
		tab.pgqa.dw_qastg_inx.visible=false	
		tab.pgqa.dw_qastg_orc.visible=false
		tab.pgqa.dw_qastg_oracle_bkno.visible=false
		cb_update.visible=false
		rb_bkno.visible=false
		rb_mod_date.visible=false
		rb_upd_date.visible=false
		st_2.visible=false
	case 3
		tab.pgbk.dw_book_inx_orc.visible=true
		tab.pgbk.dw_prdrbk_cmp.visible=false
		tab.pgbk.dw_book_inx.visible=false	
		tab.pgbk.dw_book_orc.visible=false
		tab.pgbk.dw_book_oracle_bkno.visible=false
		cb_update.visible=false
		rb_bkno.visible=false
		rb_mod_date.visible=false
		rb_upd_date.visible=false
		st_2.visible=false
end choose


end event

type tab from u_tb within w_cmpare_orcl_infx
string tag = ""
integer x = 23
integer y = 16
integer height = 1264
integer taborder = 11
end type

event selectionchanged;call super::selectionchanged;long li_re, rtn


i_index=newindex
i_event++
rb_bkno.checked=false
rb_mod_date.checked=false
rb_upd_date.checked=false

//if i_event >1 then
//	st_process.visible=false
//	st_3stage.visible=false
//end if
choose case i_index
	case 1
		st_process.text=""
		rb_bkno.visible=true
		rb_mod_date.visible=true
		rb_upd_date.visible=true
		rb_upd_date.text='Update date'
		st_2.visible=true
		tab.pgprod.dw_prod_inx_orc.visible=true
		tab.pgprod.dw_prdrbk_cmp.visible=false
		tab.pgprod.dw_prod_inx.visible=false	
		tab.pgprod.dw_prod_oracle.visible=false
		tab.pgprod.dw_prod_oracle_bkno.visible=false	
		if i_event >1 then
//			cb_update.visible=true
			
			st_total_orc.text='Total in Web: '+string(i_orcprd)
			st_total_inx.text='Total in PICS: '+string(i_inxprd)
			st_total_inx_orc.text='# of Different book : '+string(i_difprd)
		end if
	case 2
		st_total_orc.text=""
		st_total_inx.text=""
		st_total_inx_orc.text=""
		st_process.text=""
		cb_update.visible=false
		rb_bkno.visible=true
		rb_mod_date.visible=false
		rb_upd_date.visible=true
		rb_upd_date.text='QA Compdt'
		st_2.visible=false
		if ib_qaretrieved=false then
			li_re=messagebox(' ','Do you want to retrieve QA information?',Question!,YesNo!,1)
		end if
		if li_re=2   then
			return
		elseif li_re=1 and  ib_qaretrieved =false then
			st_3stage.text='In process QA'
			rtn=wf_qa()
			st_3stage.text=""
		end if
		if rtn= - 1 then
			return
		elseif rtn=1 then
			ib_qaretrieved=true
		end if
		tab.pgqa.dw_qastg_inx_orc.visible=true
		tab.pgqa.dw_prdrbk_cmp.visible=false
		tab.pgqa.dw_qastg_inx.visible=false	
		tab.pgqa.dw_qastg_orc.visible=false
		tab.pgqa.dw_qastg_oracle_bkno.visible=false
		
		if ib_qaretrieved=true then
			st_total_orc.text='Total in Web: '+string(i_orcqa)
			st_total_inx.text='Total in PICS: '+string(i_inxqa)
			st_total_inx_orc.text='# of Different book : '+string(i_difqa)
		else
			st_total_orc.text='Total in Web: '+'0'
			st_total_inx.text='Total in PICS: '+'0'
			st_total_inx_orc.text='# of Different book : '+'0'
		end if
		
	case 3
		st_process.text=""
		st_total_orc.text=""
		st_total_inx.text=""
		st_total_inx_orc.text=""
		cb_update.visible=false
		rb_mod_date.visible=false
		rb_bkno.visible=true
		rb_upd_date.visible=true
		rb_upd_date.text='Update date'
		st_2.visible=true
		if ib_bkretrieved= false then
			li_re=messagebox(' ','Do you want to retrieve Book information?',Question!,YesNo!,1)
		end if
		if li_re=2   then
			return
		elseif li_re=1 and  ib_bkretrieved =false then
			st_3stage.text='In process Book'
			rtn=wf_book()
			st_3stage.text=""
		end if
		if rtn= - 1 then
			return
		elseif  rtn=1 then
			ib_bkretrieved =true
		end if
		tab.pgbk.dw_book_inx_orc.visible=true
		tab.pgbk.dw_prdrbk_cmp.visible=false
		tab.pgbk.dw_book_inx.visible=false	
		tab.pgbk.dw_book_orc.visible=false
		tab.pgbk.dw_book_oracle_bkno.visible=false
		
		if ib_bkretrieved =true then
			st_total_orc.text='Total in Web: '+string(i_orcbk)
			st_total_inx.text='Total in PICS: '+string(i_inxbk)
			st_total_inx_orc.text='# of Different book : '+string(i_difbk)
		else
			st_total_orc.text='Total in Web: '+'0'
			st_total_inx.text='Total in PICS: '+'0'
			st_total_inx_orc.text='# of Different book : '+'0'
		end if
end choose
	


end event

type rb_prdrbk from radiobutton within w_cmpare_orcl_infx
boolean visible = false
integer x = 251
integer y = 1564
integer width = 306
integer height = 60
integer textsize = -8
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 33554432
long backcolor = 67108864
boolean enabled = false
string text = "prdrbkcmp"
boolean automatic = false
end type

event clicked;tab.pgprod.dw_prod_inx_orc.visible=false
tab.pgprod.dw_prod_inx.visible=false
tab.pgprod.dw_prod_oracle.visible=false
tab.pgprod.dw_prdrbk_cmp.visible=true
tab.pgprod.dw_prod_oracle_bkno.visible=false
rb_infx.Checked=FALSE
rb_both.Checked=FALSE

end event

type dw_prod_oracle_bkno from u_pics_dw within w_cmpare_orcl_infx
boolean visible = false
integer x = 1669
integer y = 1468
integer width = 55
integer height = 64
integer taborder = 0
string dataobject = "d_prod_oracle_bkno"
boolean vscrollbar = false
end type

event constructor;call super::constructor;this.SetTransobject(SqlServerOracleTrans)
end event

type rb_mod_date from radiobutton within w_cmpare_orcl_infx
boolean visible = false
integer x = 960
integer y = 1564
integer width = 398
integer height = 64
integer textsize = -8
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 33554432
long backcolor = 67108864
string text = "Modified Date"
end type

event clicked;string ls_sort

if tab.pgprod.dw_prod_inx_orc.visible=true then
	ls_sort='mod_date A, bkmed A, bkseq A, prodstage A'
	tab.pgprod.dw_prod_inx_orc.SetSort( ls_sort)
	tab.pgprod.dw_prod_inx_orc.Sort()
else
	messagebox(' ','You must click PICS/Web button or PICS button first.')
	return
end if

rb_bkno.Checked=FALSE
rb_upd_date.Checked=FALSE
end event

type rb_upd_date from radiobutton within w_cmpare_orcl_infx
boolean visible = false
integer x = 960
integer y = 1500
integer width = 677
integer height = 64
integer textsize = -8
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 33554432
long backcolor = 67108864
string text = "Update Date"
end type

event clicked;string ls_sort

if tab.pgprod.dw_prod_inx_orc.visible=true and i_index=1 then
	ls_sort='update_date A, bkmed A, bkseq A, prodstage A'
	tab.pgprod.dw_prod_inx_orc.SetSort( ls_sort)
	tab.pgprod.dw_prod_inx_orc.Sort()
elseif tab.pgqa.dw_qastg_inx_orc.visible=true and i_index=2 then
	ls_sort='qacompdt_orc A, bkmed A, bkseq A'
	tab.pgqa.dw_qastg_inx_orc.SetSort(ls_sort)
	tab.pgqa.dw_qastg_inx_orc.Sort()
elseif tab.pgbk.dw_book_inx_orc.visible=true and i_index=3 then
	ls_sort='update_date A, bkmed A, bkseq A'
	tab.pgbk.dw_book_inx_orc.SetSort(ls_sort)
	tab.pgbk.dw_book_inx_orc.Sort()
end if
rb_bkno.Checked=FALSE
rb_mod_date.Checked=FALSE
end event

type rb_bkno from radiobutton within w_cmpare_orcl_infx
boolean visible = false
integer x = 960
integer y = 1432
integer width = 398
integer height = 64
integer textsize = -8
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 33554432
long backcolor = 67108864
string text = "Book no"
end type

event clicked;string ls_sort
if tab.pgprod.dw_prod_inx_orc.visible=true and i_index=1 then
	ls_sort= 'bkmed A, bkseq A, prodstage A'
	tab.pgprod.dw_prod_inx_orc.SetSort( ls_sort)
	tab.pgprod.dw_prod_inx_orc.Sort()
elseif tab.pgqa.dw_qastg_inx_orc.visible=true and i_index=2 then
	ls_sort= 'bkmed A, bkseq A, qacompdt A'
	tab.pgqa.dw_qastg_inx_orc.SetSort( ls_sort)
	tab.pgqa.dw_qastg_inx_orc.Sort()
	
elseif tab.pgbk.dw_book_inx_orc.visible=true and i_index=3 then
	ls_sort= 'bkmed A, bkseq A, update_date A'
	tab.pgbk.dw_book_inx_orc.SetSort( ls_sort)
	tab.pgbk.dw_book_inx_orc.Sort()
	
end if
rb_upd_date.Checked=FALSE
rb_mod_date.Checked=FALSE
end event

type st_2 from statictext within w_cmpare_orcl_infx
boolean visible = false
integer x = 759
integer y = 1456
integer width = 174
integer height = 52
integer textsize = -8
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 33554432
long backcolor = 67108864
string text = "Sort by:"
boolean focusrectangle = false
end type

type rb_both from radiobutton within w_cmpare_orcl_infx
boolean visible = false
integer x = 251
integer y = 1500
integer width = 434
integer height = 68
integer textsize = -8
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 33554432
long backcolor = 67108864
boolean enabled = false
string text = "Web/PICS"
boolean automatic = false
end type

event clicked;string ls_sort

tab.pgprod.dw_prod_inx_orc.visible=true
tab.pgprod.dw_prod_inx.visible=false
tab.pgprod.dw_prod_oracle.visible=false
tab.pgprod.dw_prdrbk_cmp.visible=false
tab.pgprod.dw_prod_oracle_bkno.visible=false
rb_infx.Checked=FALSE
rb_oracle.Checked=FALSE
end event

type rb_infx from radiobutton within w_cmpare_orcl_infx
boolean visible = false
integer x = 59
integer y = 1436
integer width = 631
integer height = 64
integer textsize = -8
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 33554432
long backcolor = 67108864
boolean enabled = false
string text = "PICS"
boolean automatic = false
end type

event clicked;tab.pgprod.dw_prod_inx_orc.visible=false
tab.pgprod.dw_prod_inx.visible=true
tab.pgprod.dw_prod_oracle.visible=false
tab.pgprod.dw_prdrbk_cmp.visible=false
tab.pgprod.dw_prod_oracle_bkno.visible=false
rb_oracle.Checked=FALSE
rb_both.Checked=FALSE
end event

type rb_oracle from radiobutton within w_cmpare_orcl_infx
boolean visible = false
integer x = 251
integer y = 1368
integer width = 398
integer height = 64
integer textsize = -8
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 33554432
long backcolor = 67108864
boolean enabled = false
string text = "Web"
boolean automatic = false
end type

event clicked;tab.pgprod.dw_prod_inx_orc.visible=false
tab.pgprod.dw_prod_inx.visible=false
tab.pgprod.dw_prod_oracle.visible=true
tab.pgprod.dw_prdrbk_cmp.visible=false
tab.pgprod.dw_prod_oracle_bkno.visible=false
rb_infx.Checked=FALSE
rb_both.Checked=FALSE
end event

type st_1 from statictext within w_cmpare_orcl_infx
boolean visible = false
integer x = 50
integer y = 1364
integer width = 174
integer height = 52
integer textsize = -8
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 33554432
long backcolor = 67108864
boolean enabled = false
string text = "Show:"
boolean focusrectangle = false
end type

type cb_update from commandbutton within w_cmpare_orcl_infx
boolean visible = false
integer x = 1655
integer y = 1460
integer width = 128
integer height = 96
boolean bringtotop = true
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
boolean enabled = false
string text = "&Update Update_date"
end type

event clicked;long li_row, i, rtn, li_re, rtn2=0
datetime ldt_cur
string ls_diff


String ls_msgtext[ ] = {" 0 %","10 %", "20 %", "30 %", "40 %", "50 %", "60 %", &
									"70 %", "80 %", "90 %", "100 %"}
ldt_cur=datetime(today(),Now())

li_re=messagebox(' ','Do you want to update the mchar update_date with current date and time?',&
		Question!,YesNo!, 1)
if li_re=1 then
	
	li_row=tab.pgprod.dw_prod_inx.RowCount()
	for i=1 to li_row
		ls_diff=tab.pgprod.dw_prod_inx.object.diff[i]
		if ls_diff='Y' then
			rtn2++
			tab.pgprod.dw_prod_inx.SetItem(i,'update_date', ldt_cur)
		end if
	next
	uo_progress.of_SetTextColor(RGB(255, 255, 255))
	this.SetRedraw(true)
	uo_progress.Visible=TRUE
	uo_progress.of_SetMinimum(0)
	uo_progress.of_SetMaximum(rtn2)
	uo_progress.of_SetDisplayStyle(3)
	uo_progress.of_SetMessageText(ls_msgtext)
	uo_progress.of_SetPosition(0)
	st_process.text='Update the date with current date'
	rtn=tab.pgprod.dw_prod_inx.update()
	if rtn=1 then
		commit using SqlServerTrans;
		parent.SetRedraw(true)
		uo_progress.Visible=TRUE
		uo_progress.of_SetMinimum(0)
		uo_progress.of_SetMaximum(1000)
		uo_progress.of_SetDisplayStyle(3)
		uo_progress.of_SetMessageText(ls_msgtext)
		uo_progress.of_SetPosition(0)
		st_total_inx_orc.text='Total row: '+string(rtn2)+' updated'
		st_process.text='Update done'
	else
		RollBack using SqlServerTrans;
	end if
end if
end event

type uo_progress from u_progressbar within w_cmpare_orcl_infx
integer x = 1541
integer y = 1304
integer width = 649
boolean border = true
borderstyle borderstyle = stylelowered!
end type

on uo_progress.destroy
call u_progressbar::destroy
end on

type cb_print from commandbutton within w_cmpare_orcl_infx
integer x = 2875
integer y = 1464
integer width = 261
integer height = 96
integer taborder = 60
boolean bringtotop = true
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "&Print"
boolean cancel = true
end type

event clicked;long indx

indx= i_index
choose case indx
	case 1
		if tab.pgprod.dw_prod_inx_orc.visible=true then
			nvo_PowerPrn.of_SetPrinterOrientation(2)
			tab.pgprod.dw_prod_inx_orc.triggerEvent('pfc_print')
			nvo_PowerPrn.of_SetPrinterOrientation(1)
		elseif tab.pgprod.dw_prod_oracle.visible=true then
			tab.pgprod.dw_prod_oracle.triggerEvent('pfc_print')
		elseif tab.pgprod.dw_prod_inx.visible=true then
			tab.pgprod.dw_prod_inx.triggerEvent('pfc_print')
		end if
	case 2
		if tab.pgqa.dw_qastg_inx_orc.visible=true then
			nvo_PowerPrn.of_SetPrinterOrientation(2)
			tab.pgqa.dw_qastg_inx_orc.triggerEvent('pfc_print')
			nvo_PowerPrn.of_SetPrinterOrientation(1)
		elseif tab.pgqa.dw_qastg_orc.visible=true then
			tab.pgqa.dw_qastg_orc.triggerEvent('pfc_print')
		elseif tab.pgqa.dw_qastg_inx.visible=true then
			tab.pgqa.dw_qastg_inx.triggerEvent('pfc_print')
		end if
	case 3
		if tab.pgbk.dw_book_inx_orc.visible=true then
			nvo_PowerPrn.of_SetPrinterOrientation(2)
			tab.pgbk.dw_book_inx_orc.triggerEvent('pfc_print')
			nvo_PowerPrn.of_SetPrinterOrientation(1)
		elseif tab.pgbk.dw_book_orc.visible=true then
			tab.pgbk.dw_book_orc.triggerEvent('pfc_print')
		elseif tab.pgbk.dw_book_inx.visible=true then
			tab.pgbk.dw_book_inx.triggerEvent('pfc_print')
		end if
end choose

end event

type st_total_inx_orc from statictext within w_cmpare_orcl_infx
integer x = 2871
integer y = 1304
integer width = 608
integer height = 48
integer textsize = -8
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 33554432
long backcolor = 67108864
alignment alignment = center!
boolean focusrectangle = false
end type

type st_total_inx from statictext within w_cmpare_orcl_infx
integer x = 2226
integer y = 1304
integer width = 608
integer height = 52
integer textsize = -8
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 33554432
long backcolor = 67108864
alignment alignment = center!
boolean focusrectangle = false
end type

type st_total_orc from statictext within w_cmpare_orcl_infx
integer x = 78
integer y = 1304
integer width = 608
integer height = 52
integer textsize = -8
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 33554432
long backcolor = 67108864
alignment alignment = center!
boolean focusrectangle = false
end type

type st_process from statictext within w_cmpare_orcl_infx
integer x = 699
integer y = 1304
integer width = 800
integer height = 100
integer textsize = -8
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 33554432
long backcolor = 67108864
alignment alignment = center!
boolean focusrectangle = false
end type

type cb_exit from commandbutton within w_cmpare_orcl_infx
integer x = 3200
integer y = 1464
integer width = 261
integer height = 96
integer taborder = 30
boolean bringtotop = true
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "&Exit"
boolean cancel = true
end type

event clicked;ib_disableclosequery=true
close(parent)
end event


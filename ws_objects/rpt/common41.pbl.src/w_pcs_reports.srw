$PBExportHeader$w_pcs_reports.srw
forward
global type w_pcs_reports from w_sheet
end type
type ole_ezftp from u_ezftp within w_pcs_reports
end type
type dw_dtbbkseq from u_pics_dw within w_pcs_reports
end type
type cb_opf from commandbutton within w_pcs_reports
end type
type dw_prod from u_pics_dw within w_pcs_reports
end type
type dw_par_on_web from u_pics_dw within w_pcs_reports
end type
type cb_print from u_cb within w_pcs_reports
end type
type cb_print_to_file from u_cb within w_pcs_reports
end type
type cb_cancel from u_cb within w_pcs_reports
end type
type rte_1 from u_rte within w_pcs_reports
end type
type st_rows from statictext within w_pcs_reports
end type
type sle_rows from singlelineedit within w_pcs_reports
end type
type dw_qa_cntr from u_dw within w_pcs_reports
end type
type cb_clear from commandbutton within w_pcs_reports
end type
type cb_dbcancel from commandbutton within w_pcs_reports
end type
type dw_pubs1_ec_report from u_dw within w_pcs_reports
end type
type dw_pcarchive_ace_report from u_dw within w_pcs_reports
end type
type cb_addcmts from commandbutton within w_pcs_reports
end type
type dw_unable_process_inv from u_dw within w_pcs_reports
end type
type dw_ttlist_ec_prod from u_dw within w_pcs_reports
end type
type dw_stage1_annotation from u_dw within w_pcs_reports
end type
type dw_ttlist_ec_controller from u_dw within w_pcs_reports
end type
type dw_pcparts_txt_report from u_dw within w_pcs_reports
end type
type dw_cc_par_report_bkno_med from u_dw within w_pcs_reports
end type
type dw_qa_receive_date_bkseq from u_pics_dw within w_pcs_reports
end type
type dw_receive_date from u_dw within w_pcs_reports
end type
type dw_qa_receive_date_bkmed from u_pics_dw within w_pcs_reports
end type
type dw_s1check_report from u_dw within w_pcs_reports
end type
type dw_pcnofishe_ace_report from u_dw within w_pcs_reports
end type
type dw_catalogcheck_sql from u_dw within w_pcs_reports
end type
type dw_pcdeviaten_sum_only from u_dw within w_pcs_reports
end type
type dw_pcdeviaten_ace_report_active from u_pics_dw within w_pcs_reports
end type
type dw_pcdeviaten_ace_report from u_dw within w_pcs_reports
end type
type dw_pctipauth_ace_report from u_dw within w_pcs_reports
end type
type dw_pctipttl_ec_report from u_dw within w_pcs_reports
end type
type dw_pcinstr_ace from u_dw within w_pcs_reports
end type
type dw_cc_par_report_conno_cntr from u_dw within w_pcs_reports
end type
type dw_cc_par_report_conno from u_dw within w_pcs_reports
end type
type dw_pcs1chk_ace_report from u_dw within w_pcs_reports
end type
end forward

global type w_pcs_reports from w_sheet
boolean visible = false
integer x = 87
integer y = 472
integer width = 2834
integer height = 1452
string title = "PCS Reports"
windowstate windowstate = maximized!
ole_ezftp ole_ezftp
dw_dtbbkseq dw_dtbbkseq
cb_opf cb_opf
dw_prod dw_prod
dw_par_on_web dw_par_on_web
cb_print cb_print
cb_print_to_file cb_print_to_file
cb_cancel cb_cancel
rte_1 rte_1
st_rows st_rows
sle_rows sle_rows
dw_qa_cntr dw_qa_cntr
cb_clear cb_clear
cb_dbcancel cb_dbcancel
dw_pubs1_ec_report dw_pubs1_ec_report
dw_pcarchive_ace_report dw_pcarchive_ace_report
cb_addcmts cb_addcmts
dw_unable_process_inv dw_unable_process_inv
dw_ttlist_ec_prod dw_ttlist_ec_prod
dw_stage1_annotation dw_stage1_annotation
dw_ttlist_ec_controller dw_ttlist_ec_controller
dw_pcparts_txt_report dw_pcparts_txt_report
dw_cc_par_report_bkno_med dw_cc_par_report_bkno_med
dw_qa_receive_date_bkseq dw_qa_receive_date_bkseq
dw_receive_date dw_receive_date
dw_qa_receive_date_bkmed dw_qa_receive_date_bkmed
dw_s1check_report dw_s1check_report
dw_pcnofishe_ace_report dw_pcnofishe_ace_report
dw_catalogcheck_sql dw_catalogcheck_sql
dw_pcdeviaten_sum_only dw_pcdeviaten_sum_only
dw_pcdeviaten_ace_report_active dw_pcdeviaten_ace_report_active
dw_pcdeviaten_ace_report dw_pcdeviaten_ace_report
dw_pctipauth_ace_report dw_pctipauth_ace_report
dw_pctipttl_ec_report dw_pctipttl_ec_report
dw_pcinstr_ace dw_pcinstr_ace
dw_cc_par_report_conno_cntr dw_cc_par_report_conno_cntr
dw_cc_par_report_conno dw_cc_par_report_conno
dw_pcs1chk_ace_report dw_pcs1chk_ace_report
end type
global w_pcs_reports w_pcs_reports

type variables
date id_stdt, id_enddt, cntr_st_dt
string is_specattn,cntr_number
datawindow idw
boolean ib_cbx_checked =false,ib_cancel=FALSE
int il_file

end variables

forward prototypes
public subroutine wf_pcs_save_nofishe ()
public subroutine wf_pcs_save_multipartbooks ()
public subroutine wf_create_par_on_web (string lconno)
public function integer wf_calculate_estpt (string lmed, string lpriority, integer lapplen, string lcntrcvcd, decimal ldf)
public function integer wf_connect_check ()
end prototypes

public subroutine wf_pcs_save_nofishe ();integer li_filenum, li_wpfile

long ll_rows,i
string ls_filename, ls_path,Lttl,Ls_today,Lauthor
string Lauth_fn,Lbkno,Lbkmed,Lahonorific,Lmsg

// Get the filename
ls_filename = 'nofiche'
ls_path = "c:\picsorcl9i\" + ls_filename
IF GetFileSaveName("Select File",ls_path,ls_filename, "DOC","Text Files (*.TXT),*.TXT," + " Doc Files (*.DOC), *.DOC") <> 1 THEN
	messagebox("Incorrect file name","Please try again")
	return
end if
lmsg = "Saving the file "+ls_path+", Please Wait..."
OpenWithParm(w_pics_retrieve_msg_box,lmsg)
li_fileNum = FileOpen(ls_filename,LineMode!, Write!, LockWrite!, Replace!)
Ls_today = string(today(),'mm/dd/yyyy')
filewrite(li_filenum,'              BOOKS SHIPPED BUT NOT YET ON CD BLND')
filewrite(li_filenum,'                        AS OF '+Ls_today)
//filewrite(li_filenum,'~t~t(Arranged By Book Titles)')
filewrite(li_filenum," ")
filewrite(li_filenum," ")
filewrite(li_filenum,'~t~tTitle'+'~t~t             Author~t~t     Book No')
filewrite(li_filenum," ")
filewrite(li_filenum," ")
ll_rows = dw_pcnofishe_ace_report.rowcount()
FOR i=1 TO ll_rows
	Lttl = f_remove_pipe(TRIM(dw_pcnofishe_ace_report.GetItemString(i, "ttlinit_ttl")))
	Lauthor = TRIM(dw_pcnofishe_ace_report.GetItemString(i, "ttlinit_auth"))
	Lauth_fn = TRIM(dw_pcnofishe_ace_report.GetItemString(i, "ttlinit_authfn"))
	Lahonorific = TRIM(dw_pcnofishe_ace_report.GetItemString(i, "ttlinit_ahonorific"))
	Lauthor = f_combine_auth_authfn(Lauthor,Lauth_fn,Lahonorific)
	Lbkmed = TRIM(dw_pcnofishe_ace_report.GetItemString(i, "prod_bkmed"))
	Lbkno = Lbkmed +TRIM(string(dw_pcnofishe_ace_report.GetItemNumber(i, "prod_bkseq")))
	lttl +='                                                                              '
	lttl =left(lttl, 30 )
	lauthor +='                                                                            '
	lauthor =left(lauthor, 20)
	lbkno +='                                                                              '
	lbkno =left(lbkno, 8)
	filewrite(li_filenum,Lttl+'     '+Lauthor+'    '+Lbkno)
NEXT
Fileclose(li_filenum)
setpointer(arrow!)
close(w_pics_retrieve_msg_box)
end subroutine

public subroutine wf_pcs_save_multipartbooks ();integer li_filenum, li_wpfile, li_digits

long ll_rows,i
string ls_filename, ls_path,Ls_today,ls_volums_or_container, ls_vols
string Lauth_fn,Lbkno,Lbkmed,Lmsg


// Get the filename
ls_filename = 'multipartbooks'
ls_path = "c:\picsorcl9i\" + ls_filename
IF GetFileSaveName("Select File",ls_path,ls_filename, "DOC","Text Files (*.TXT),*.TXT," + " Doc Files (*.DOC), *.DOC") <> 1 THEN
	messagebox("Incorrect file name","Please try again")
	return
end if
lmsg = "Saving the file "+ls_path+", Please Wait..."
OpenWithParm(w_pics_retrieve_msg_box,lmsg)
li_fileNum = FileOpen(ls_filename,LineMode!, Write!, LockWrite!, Replace!)
Ls_today = string(today(),'mm/dd/yyyy')
filewrite(li_filenum,'~t~tMulti-Part Books Nearing Shipment ')
filewrite(li_filenum,'~t~t          As Of '+Ls_today)
filewrite(li_filenum,'~t~t(Arranged By Book Sequence Number)')
filewrite(li_filenum," ")
filewrite(li_filenum,'~t~tBook No'+'~t~t~t       Volumes')
filewrite(li_filenum," ")
ll_rows = dw_pcparts_txt_report.rowcount()
FOR i=1 TO ll_rows
	Lbkmed = TRIM(dw_pcparts_txt_report.GetItemString(i, "mchar_med"))
	Lbkno = Lbkmed +TRIM(string(dw_pcparts_txt_report.GetItemNumber(i, "mchar_bkseq")))
	IF lbkmed ='RC' THEN
		ls_volums_or_container= 'CONTAINERS'
		ls_vols = string(dw_pcparts_txt_report.GetItemNumber(i, "ccontain"))
	ELSE
		ls_volums_or_container= 'VOLUMES'
		ls_vols = string(dw_pcparts_txt_report.GetItemNumber(i, "mchar_vols"))
	END IF
	lbkno +='                                                                              '
	lbkno =left(lbkno, 8)
	li_digits =len(ls_vols )
	CHOOSE CASE li_digits
		CASE 1
			filewrite(li_filenum,'~t~t'+lbkno+'~t~t  '+ls_vols+'    '+ls_volums_or_container)
		CASE 2
			filewrite(li_filenum,'~t~t'+lbkno+'~t~t  '+ls_vols+'   '+ls_volums_or_container)
		CASE 3
			filewrite(li_filenum,'~t~t'+lbkno+'~t~t  '+ls_vols+'  '+ls_volums_or_container)
	END CHOOSE
	//filewrite(li_filenum,'~t~t'+lbkno+'~t~t  '+ls_vols+'  '+ls_volums_or_container)
NEXT
Fileclose(li_filenum)
setpointer(arrow!)
close(w_pics_retrieve_msg_box)
end subroutine

public subroutine wf_create_par_on_web (string lconno);long lbkseq
string lbkno,lttl,lauth,lauthfn,lahonorific,lcoauth,lcoauthfn,lchonorific,lbkmed,lchno,laepcd,ls_navstr,ls_priority
int lcnt=0,rtn,ll_rows
datetime ldt_DateTime,ldt_Datetime2

ldt_DateTime = DateTime(today(),now())
ldt_DateTime2 = DateTime(today(),RelativeTime(now(),1000))

SETNULL(lcoauth)

OpenWithParm(w_pics_retrieve_msg_box,"Building PAR record on the WEB, Please Wait...")

lchno = dw_cc_par_report_conno.object.cchno[1]
//Messagebox("conno","conno = "+lconno+ " chno = "+lchno)

select count(*)
into :lcnt
from coauth
where chno = :lchno
using sqlservertrans;
	
IF lcnt >= 2 THEN
	lcoauth = "And Others"
ELSEIF lcnt = 1 THEN
	Select coauth,coauthfn,chonorific
	into :lcoauth,:lcoauthfn,:lchonorific
	from coauth
	where chno = :lchno
	using sqlservertrans;
	lcoauth = f_add_coauth_coauthfn(lcoauth,lcoauthfn,lchonorific)
END IF

dw_par_on_web.of_SetTransObject(sqlserveroracletrans)
// Insert par on the web.

ll_rows = dw_cc_par_report_conno.rowcount()
IF ll_rows = 1 THEN
	dw_par_on_web.InsertRow(0)
	dw_par_on_web.object.bkseq[1] 	= dw_cc_par_report_conno.object.mchar_bkseq[1]
	dw_par_on_web.object.bkmed[1] 	= TRIM(dw_cc_par_report_conno.object.prod_bkmed[1])
	dw_par_on_web.object.dt_tim[1] 	= ldt_DateTime
	dw_par_on_web.object.conno[1] 	= TRIM(lconno)
	dw_par_on_web.object.bkno[1] 		= TRIM(dw_cc_par_report_conno.object.prod_bkmed[1]) + TRIM(string(dw_cc_par_report_conno.object.mchar_bkseq[1]))
	dw_par_on_web.object.auth[1] 		= f_change_pipe_html_tag(TRIM(dw_cc_par_report_conno.object.cc_auth[1]))
	dw_par_on_web.object.coauth[1] 	= TRIM(lcoauth)
	dw_par_on_web.object.ajyfn[1] 	= TRIM(dw_cc_par_report_conno.object.ttlinit_ajyfn[1])
	dw_par_on_web.object.bk_color[1] = TRIM(dw_cc_par_report_conno.object.cc_bk_color[1])
	dw_par_on_web.object.dewey[1] 	= TRIM(dw_cc_par_report_conno.object.ttlinit_dewey[1])
	dw_par_on_web.object.copyright_info[1] = TRIM(dw_cc_par_report_conno.object.cc_copyright_info[1])
	dw_par_on_web.object.med[1] 		= TRIM(dw_cc_par_report_conno.object.mchar_med[1])
	dw_par_on_web.object.narrator[1] = TRIM(dw_cc_par_report_conno.object.cc_narr[1])
	dw_par_on_web.object.vols[1] 		= dw_cc_par_report_conno.object.mchar_vols[1]
	dw_par_on_web.object.container[1] = dw_cc_par_report_conno.object.cc_cc4[1]
	dw_par_on_web.object.anno_foreign[1] = TRIM(dw_cc_par_report_conno.object.anno_foreign[1])
	dw_par_on_web.object.prv_narrator[1] = TRIM(dw_cc_par_report_conno.object.cc_prev_narr[1])
	dw_par_on_web.object.andigcd[1] 	= TRIM(dw_cc_par_report_conno.object.mchar_andigcd[1])
	dw_par_on_web.object.pmsub[1] 	= TRIM(dw_cc_par_report_conno.object.cc_pmsub[1])
	dw_par_on_web.object.oneliner[1] = TRIM(dw_cc_par_report_conno.object.cc_oneliner[1])
	dw_par_on_web.object.serttl[1] 	= TRIM(dw_cc_par_report_conno.object.cc_serttl[1])
		
	dw_par_on_web.object.anno[1] 		= f_change_pipe_html_tag( dw_cc_par_report_conno.object.annotation_anno[1] )
	dw_par_on_web.object.oneliner[1] = f_change_pipe_html_tag( dw_cc_par_report_conno.object.ttlinit_oneliner[1])
	dw_par_on_web.object.serttl[1] 	= f_change_pipe_html_tag( dw_cc_par_report_conno.object.cserttl[1])
	dw_par_on_web.object.seqnote[1] 	= f_change_pipe_html_tag( dw_cc_par_report_conno.object.cseqnote[1])
	dw_par_on_web.object.ttl[1] 		= f_change_pipe_html_tag( TRIM(dw_cc_par_report_conno.object.cc_ttl_with_ttlart[1]) )
	
	dw_par_on_web.object.cdinit[1] 	= TRIM(dw_cc_par_report_conno.object.ttlinit_cdinit[1])
	dw_par_on_web.object.pminit[1] 	= TRIM(dw_cc_par_report_conno.object.ttlinit_pminit[1])
	dw_par_on_web.object.prdr[1] 		= TRIM(dw_cc_par_report_conno.object.ancntr_prdr[1])
		
	dw_par_on_web.object.cntr[1] 		= TRIM(dw_cc_par_report_conno.object.ancntr_cntrlc[1])
	dw_par_on_web.object.len[1] 		= dw_cc_par_report_conno.object.mchar_len[1]
	dw_par_on_web.object.pbpage[1] 	= dw_cc_par_report_conno.object.acquist_pbpage[1]
		
	dw_par_on_web.object.schstdt[1] 	= datetime(date(string(dw_cc_par_report_conno.object.prod_schstdt[1],'MM/DD/YYYY')),now())
	dw_par_on_web.object.schenddt[1] = datetime(date(string(dw_cc_par_report_conno.object.prod_schenddt[1],'MM/DD/YYYY')),now())
	dw_par_on_web.object.assigndt[1] = datetime(date(string(dw_cc_par_report_conno.object.prod_assigndt[1],'MM/DD/YYYY')),now())
		
	dw_par_on_web.object.reissue_code[1] = TRIM(dw_cc_par_report_conno.object.mchar_ricd[1])
	dw_par_on_web.object.applen[1] = dw_cc_par_report_conno.object.mchar_applen[1]
	dw_par_on_web.object.reissue[1] 	= TRIM(dw_cc_par_report_conno.object.cc_reissue[1])
	
	dw_par_on_web.object.specialinstruction[1] 	= TRIM(dw_cc_par_report_conno.object.specinst_sitxt[1])
	dw_par_on_web.object.nav_instr[1] 	= TRIM(dw_cc_par_report_conno.object.mchar_nav_instr[1])
	// Priority was added to PAR 3/25/04
	dw_par_on_web.object.priority[1] 	= dw_cc_par_report_conno.object.mchar_priority[1]
ELSEIF ll_rows = 2 THEN
// Rows 2
	dw_par_on_web.InsertRow(0)
	dw_par_on_web.object.bkseq[1] 	= dw_cc_par_report_conno.object.mchar_bkseq[2]
	dw_par_on_web.object.bkmed[1] 	= TRIM(dw_cc_par_report_conno.object.prod_bkmed[2])
	dw_par_on_web.object.dt_tim[1] 	= ldt_DateTime2
	dw_par_on_web.object.conno[1] 	= TRIM(lconno)
	dw_par_on_web.object.bkno[1] 		= TRIM(dw_cc_par_report_conno.object.prod_bkmed[2]) + TRIM(string(dw_cc_par_report_conno.object.mchar_bkseq[2]))
	dw_par_on_web.object.auth[1] 		= f_change_pipe_html_tag(TRIM(dw_cc_par_report_conno.object.cc_auth[2]))
	dw_par_on_web.object.coauth[1] 	= TRIM(lcoauth)
	dw_par_on_web.object.ajyfn[1] 	= TRIM(dw_cc_par_report_conno.object.ttlinit_ajyfn[2])
	dw_par_on_web.object.bk_color[1] = TRIM(dw_cc_par_report_conno.object.cc_bk_color[2])
	dw_par_on_web.object.dewey[1] 	= TRIM(dw_cc_par_report_conno.object.ttlinit_dewey[2])
	dw_par_on_web.object.copyright_info[1] = TRIM(dw_cc_par_report_conno.object.cc_copyright_info[2])
	dw_par_on_web.object.med[1] 		= TRIM(dw_cc_par_report_conno.object.mchar_med[2])
	dw_par_on_web.object.narrator[1] = TRIM(dw_cc_par_report_conno.object.cc_narr[2])
	dw_par_on_web.object.vols[1] 		= dw_cc_par_report_conno.object.mchar_vols[2]
	dw_par_on_web.object.container[1] = dw_cc_par_report_conno.object.cc_cc4[2]
	dw_par_on_web.object.anno_foreign[1] = TRIM(dw_cc_par_report_conno.object.anno_foreign[2])
	dw_par_on_web.object.prv_narrator[1] = TRIM(dw_cc_par_report_conno.object.cc_prev_narr[2])
	dw_par_on_web.object.andigcd[1] 	= TRIM(dw_cc_par_report_conno.object.mchar_andigcd[2])
	dw_par_on_web.object.pmsub[1] 	= TRIM(dw_cc_par_report_conno.object.cc_pmsub[2])
	dw_par_on_web.object.oneliner[1] = TRIM(dw_cc_par_report_conno.object.cc_oneliner[2])
	dw_par_on_web.object.serttl[1] 	= TRIM(dw_cc_par_report_conno.object.cc_serttl[2])
		
	dw_par_on_web.object.anno[1] 		= f_change_pipe_html_tag( dw_cc_par_report_conno.object.annotation_anno[2] )
	dw_par_on_web.object.oneliner[1] = f_change_pipe_html_tag( dw_cc_par_report_conno.object.ttlinit_oneliner[2])
	dw_par_on_web.object.serttl[1] 	= f_change_pipe_html_tag( dw_cc_par_report_conno.object.cserttl[2])
	dw_par_on_web.object.seqnote[1] 	= f_change_pipe_html_tag( dw_cc_par_report_conno.object.cseqnote[2])
	dw_par_on_web.object.ttl[1] 		= f_change_pipe_html_tag( TRIM(dw_cc_par_report_conno.object.cc_ttl_with_ttlart[2]) )
	
	dw_par_on_web.object.cdinit[1] 	= TRIM(dw_cc_par_report_conno.object.ttlinit_cdinit[2])
	dw_par_on_web.object.pminit[1] 	= TRIM(dw_cc_par_report_conno.object.ttlinit_pminit[2])
	dw_par_on_web.object.prdr[1] 		= TRIM(dw_cc_par_report_conno.object.ancntr_prdr[2])
		
	dw_par_on_web.object.cntr[1] 		= TRIM(dw_cc_par_report_conno.object.ancntr_cntrlc[2])
	dw_par_on_web.object.len[1] 		= dw_cc_par_report_conno.object.mchar_len[2]
	dw_par_on_web.object.pbpage[1] 	= dw_cc_par_report_conno.object.acquist_pbpage[2]
		
	dw_par_on_web.object.schstdt[1] 	= datetime(date(string(dw_cc_par_report_conno.object.prod_schstdt[2],'MM/DD/YYYY')),now())
	dw_par_on_web.object.schenddt[1] = datetime(date(string(dw_cc_par_report_conno.object.prod_schenddt[2],'MM/DD/YYYY')),now())
	dw_par_on_web.object.assigndt[1] = datetime(date(string(dw_cc_par_report_conno.object.prod_assigndt[2],'MM/DD/YYYY')),now())
		
	dw_par_on_web.object.reissue_code[1] = TRIM(dw_cc_par_report_conno.object.mchar_ricd[2])
	dw_par_on_web.object.applen[1] = dw_cc_par_report_conno.object.mchar_applen[2]
	dw_par_on_web.object.reissue[1] 	= TRIM(dw_cc_par_report_conno.object.cc_reissue[2])
	
	dw_par_on_web.object.specialinstruction[1] 	= TRIM(dw_cc_par_report_conno.object.specinst_sitxt[2])
	dw_par_on_web.object.nav_instr[1] 	= TRIM(dw_cc_par_report_conno.object.mchar_nav_instr[2])
	
	// Priority was added to PAR 3/25/04
	dw_par_on_web.object.priority[1] 	= dw_cc_par_report_conno.object.mchar_priority[2]
	
	
END IF
	
//string lm,lbks,ld,d1,d2,d3
//lm = dw_par_on_web.object.bkmed[1]
//lbks = string(dw_par_on_web.object.bkseq[1])
//ld = string(dw_par_on_web.object.dt_tim[1])
//d1 = string(dw_par_on_web.object.schstdt[1])
//d2 = string(dw_par_on_web.object.schenddt[1])
//d3 = string(dw_par_on_web.object.assigndt[1])
//ls_navstr = string(dw_par_on_web.object.nav_instr[1])
//
//Messagebox("data getting inserted","bkseq = "+lbks+"bkmed = "+lm+" date = "+ld+" "+d1+" "+d2+" "+d3)
//Messagebox("nac str","nav_str = "+ls_navstr)

rtn = dw_par_on_web.Event pfc_update(TRUE,TRUE)
IF rtn = 1 THEN
	Commit Using SqlServeroracleTrans;
	MessageBox("Update","PAR updated on the web.")
ELSE
	Rollback Using Sqlserveroracletrans;
	MessageBox("ERROR","Update failed in the web.")
END IF
close(w_pics_retrieve_msg_box)
	

end subroutine

public function integer wf_calculate_estpt (string lmed, string lpriority, integer lapplen, string lcntrcvcd, decimal ldf);double Ltrk_pg
integer Lestpt,Ltrk_pg_int
string Lvol_nls

IF (Lmed="P/B") THEN
	Lestpt = 90
	RETURN Lestpt
ELSEIF (Lmed="BR") THEN
	Ltrk_pg = Lapplen / 100
	IF (Ltrk_pg < 10) THEN
		IF (Ltrk_pg - Ceiling(Ltrk_pg)) > 0 THEN
			Ltrk_pg = Ltrk_pg + 1
		END IF
	ELSEIF (Ltrk_pg >= 10 AND Ltrk_pg <= 50) THEN
		IF (Ltrk_pg - Ceiling(Ltrk_pg)) > 0 THEN
			Ltrk_pg = Ltrk_pg + 10
		END IF
	END IF
	Ltrk_pg_int = Ceiling(Ltrk_pg)
	Lvol_nls = "C"
ELSEIF (Lmed = "RC") THEN
	IF Lcntrcvcd = "V" THEN
		Lestpt = 120
	END IF
	Ltrk_pg_int = Lapplen
	IF Lcntrcvcd = "C" THEN
		Lvol_nls = "C"
	ELSE
		Lvol_nls = "N"
	END IF
ELSEIF(Lmed = "FD") THEN
	IF Lapplen < 16 THEN
		Ltrk_pg = 16
	ELSE
		Ltrk_pg = Lapplen
	END IF
	Ltrk_pg_int = Ceiling(Ltrk_pg)
	Lvol_nls = "C"
END IF
IF Lcntrcvcd <> "V" THEN
	IF (Ldf=1.00) THEN
		SELECT estpt1 INTO :Lestpt 
		FROM estpt
		WHERE med=:Lmed AND priority=:Lpriority AND cntrcvcd=:Lvol_nls AND applen=:Ltrk_pg_int
		USING SQLServerTrans;
	ELSEIF (Ldf=1.25) THEN
		SELECT estpt2 INTO :Lestpt 
		FROM estpt
		WHERE med=:Lmed AND priority=:Lpriority AND cntrcvcd=:Lvol_nls AND applen=:Ltrk_pg_int
		USING SQLServerTrans;
	ELSEIF (Ldf=1.50) THEN
		SELECT estpt3 INTO :Lestpt 
		FROM estpt
		WHERE med=:Lmed AND priority=:Lpriority AND cntrcvcd=:Lvol_nls AND applen=:Ltrk_pg_int
		USING SQLServerTrans;
	ELSEIF (Ldf=2.00) THEN
		SELECT estpt3 INTO :Lestpt 
		FROM estpt
		WHERE med=:Lmed AND priority=:Lpriority AND cntrcvcd=:Lvol_nls AND applen=:Ltrk_pg_int
		USING SQLServerTrans;
	END IF	
END IF
RETURN Lestpt

end function

public function integer wf_connect_check ();string ls_message

Filewrite(il_file,'in wf_connect_check of w_pcs_reports')
//If we have lost our connection to oracle, reconnect.
IF not SQLServerOracleTrans.DBHandle() >0 THEN
	SQLServerOracleTrans.of_connect() 
	IF SqlServerOracleTrans.sqlcode <> 0 THEN
		IF SqlServerOracleTrans.SqlDbcode = -951 THEN            //check for invalid userid
			MessageBox("Login Error","Invalid User ID/Password using net9web.",StopSign!)
			Return -1	
		ELSEIF SqlServerOracleTrans.SqlDBcode = -952 THEN       //check for invalid password
			MessageBox("Login Error","Invalid User ID/Password using net9web.",StopSign!)
			Return -1
		Else                                             //check for other error messages
			MessageBox("Database Connection Error","Unable to Connect using net9web." +& 
					string(SqlServerOracleTrans.sqldbcode) + " " +&
					SqlServerOracleTrans.SQLErrText, &
					StopSign!)
					Return -1
		END IF
	end if
END IF

IF not SQLServerTrans.DBHandle() >0 THEN
	SQLServerTrans.of_connect() 
	IF SqlServerTrans.sqlcode <> 0 THEN
		IF SqlServerOracleTrans.SqlDbcode = -951 THEN            //check for invalid userid
		  MessageBox("Login Error","Invalid User ID/Password using net9pics.",StopSign!)
		  Return -1
		ELSEIF SqlServerOracleTrans.SqlDBcode = -952 THEN       //check for invalid password
		  MessageBox("Login Error","Invalid User ID/Password using net9pics.",StopSign!)
		  Return -1
		Else                                             //check for other error messages
		ls_message = "Unable to Connect to Oracle using net9pics. "
		MessageBox("Database Connection Error",ls_message+string(SQLserverTrans.sqldbcode) + " " +&
		SQLserverTrans.SQLErrText, &
		StopSign!)
		Return -1
	  END IF
	end if
END IF
Filewrite(il_file,'exiting  wf_connect_check of w_pcs_reports')
return 1

end function

on w_pcs_reports.create
int iCurrent
call super::create
this.ole_ezftp=create ole_ezftp
this.dw_dtbbkseq=create dw_dtbbkseq
this.cb_opf=create cb_opf
this.dw_prod=create dw_prod
this.dw_par_on_web=create dw_par_on_web
this.cb_print=create cb_print
this.cb_print_to_file=create cb_print_to_file
this.cb_cancel=create cb_cancel
this.rte_1=create rte_1
this.st_rows=create st_rows
this.sle_rows=create sle_rows
this.dw_qa_cntr=create dw_qa_cntr
this.cb_clear=create cb_clear
this.cb_dbcancel=create cb_dbcancel
this.dw_pubs1_ec_report=create dw_pubs1_ec_report
this.dw_pcarchive_ace_report=create dw_pcarchive_ace_report
this.cb_addcmts=create cb_addcmts
this.dw_unable_process_inv=create dw_unable_process_inv
this.dw_ttlist_ec_prod=create dw_ttlist_ec_prod
this.dw_stage1_annotation=create dw_stage1_annotation
this.dw_ttlist_ec_controller=create dw_ttlist_ec_controller
this.dw_pcparts_txt_report=create dw_pcparts_txt_report
this.dw_cc_par_report_bkno_med=create dw_cc_par_report_bkno_med
this.dw_qa_receive_date_bkseq=create dw_qa_receive_date_bkseq
this.dw_receive_date=create dw_receive_date
this.dw_qa_receive_date_bkmed=create dw_qa_receive_date_bkmed
this.dw_s1check_report=create dw_s1check_report
this.dw_pcnofishe_ace_report=create dw_pcnofishe_ace_report
this.dw_catalogcheck_sql=create dw_catalogcheck_sql
this.dw_pcdeviaten_sum_only=create dw_pcdeviaten_sum_only
this.dw_pcdeviaten_ace_report_active=create dw_pcdeviaten_ace_report_active
this.dw_pcdeviaten_ace_report=create dw_pcdeviaten_ace_report
this.dw_pctipauth_ace_report=create dw_pctipauth_ace_report
this.dw_pctipttl_ec_report=create dw_pctipttl_ec_report
this.dw_pcinstr_ace=create dw_pcinstr_ace
this.dw_cc_par_report_conno_cntr=create dw_cc_par_report_conno_cntr
this.dw_cc_par_report_conno=create dw_cc_par_report_conno
this.dw_pcs1chk_ace_report=create dw_pcs1chk_ace_report
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.ole_ezftp
this.Control[iCurrent+2]=this.dw_dtbbkseq
this.Control[iCurrent+3]=this.cb_opf
this.Control[iCurrent+4]=this.dw_prod
this.Control[iCurrent+5]=this.dw_par_on_web
this.Control[iCurrent+6]=this.cb_print
this.Control[iCurrent+7]=this.cb_print_to_file
this.Control[iCurrent+8]=this.cb_cancel
this.Control[iCurrent+9]=this.rte_1
this.Control[iCurrent+10]=this.st_rows
this.Control[iCurrent+11]=this.sle_rows
this.Control[iCurrent+12]=this.dw_qa_cntr
this.Control[iCurrent+13]=this.cb_clear
this.Control[iCurrent+14]=this.cb_dbcancel
this.Control[iCurrent+15]=this.dw_pubs1_ec_report
this.Control[iCurrent+16]=this.dw_pcarchive_ace_report
this.Control[iCurrent+17]=this.cb_addcmts
this.Control[iCurrent+18]=this.dw_unable_process_inv
this.Control[iCurrent+19]=this.dw_ttlist_ec_prod
this.Control[iCurrent+20]=this.dw_stage1_annotation
this.Control[iCurrent+21]=this.dw_ttlist_ec_controller
this.Control[iCurrent+22]=this.dw_pcparts_txt_report
this.Control[iCurrent+23]=this.dw_cc_par_report_bkno_med
this.Control[iCurrent+24]=this.dw_qa_receive_date_bkseq
this.Control[iCurrent+25]=this.dw_receive_date
this.Control[iCurrent+26]=this.dw_qa_receive_date_bkmed
this.Control[iCurrent+27]=this.dw_s1check_report
this.Control[iCurrent+28]=this.dw_pcnofishe_ace_report
this.Control[iCurrent+29]=this.dw_catalogcheck_sql
this.Control[iCurrent+30]=this.dw_pcdeviaten_sum_only
this.Control[iCurrent+31]=this.dw_pcdeviaten_ace_report_active
this.Control[iCurrent+32]=this.dw_pcdeviaten_ace_report
this.Control[iCurrent+33]=this.dw_pctipauth_ace_report
this.Control[iCurrent+34]=this.dw_pctipttl_ec_report
this.Control[iCurrent+35]=this.dw_pcinstr_ace
this.Control[iCurrent+36]=this.dw_cc_par_report_conno_cntr
this.Control[iCurrent+37]=this.dw_cc_par_report_conno
this.Control[iCurrent+38]=this.dw_pcs1chk_ace_report
end on

on w_pcs_reports.destroy
call super::destroy
destroy(this.ole_ezftp)
destroy(this.dw_dtbbkseq)
destroy(this.cb_opf)
destroy(this.dw_prod)
destroy(this.dw_par_on_web)
destroy(this.cb_print)
destroy(this.cb_print_to_file)
destroy(this.cb_cancel)
destroy(this.rte_1)
destroy(this.st_rows)
destroy(this.sle_rows)
destroy(this.dw_qa_cntr)
destroy(this.cb_clear)
destroy(this.cb_dbcancel)
destroy(this.dw_pubs1_ec_report)
destroy(this.dw_pcarchive_ace_report)
destroy(this.cb_addcmts)
destroy(this.dw_unable_process_inv)
destroy(this.dw_ttlist_ec_prod)
destroy(this.dw_stage1_annotation)
destroy(this.dw_ttlist_ec_controller)
destroy(this.dw_pcparts_txt_report)
destroy(this.dw_cc_par_report_bkno_med)
destroy(this.dw_qa_receive_date_bkseq)
destroy(this.dw_receive_date)
destroy(this.dw_qa_receive_date_bkmed)
destroy(this.dw_s1check_report)
destroy(this.dw_pcnofishe_ace_report)
destroy(this.dw_catalogcheck_sql)
destroy(this.dw_pcdeviaten_sum_only)
destroy(this.dw_pcdeviaten_ace_report_active)
destroy(this.dw_pcdeviaten_ace_report)
destroy(this.dw_pctipauth_ace_report)
destroy(this.dw_pctipttl_ec_report)
destroy(this.dw_pcinstr_ace)
destroy(this.dw_cc_par_report_conno_cntr)
destroy(this.dw_cc_par_report_conno)
destroy(this.dw_pcs1chk_ace_report)
end on

event close;call super::close;m_pics_main.m_file.m_print.Enabled = False
m_pics_main.m_file.m_pagesetup.Enabled = False
m_pics_main.m_file.m_printimmediate.Enabled = False
m_pics_main.m_edit.m_deleterow.Enabled = False
m_pics_main.m_edit.m_addrow.Enabled = False

//debug
Filewrite(il_file,'Closing window w_pcs_reports')
fileclose(il_file)
//debug
end event

event closequery;call super::closequery;Integer	li_pendingrc
Integer	li_validationrc
Integer	li_accepttextrc
Integer	li_msg
Integer	li_rc,rtn
String	ls_msgparms[]

f_pics_set_def_prn_setting()
ib_disableclosequery = TRUE

// Check if the CloseQuery process has been disabled
If ib_disableclosequery Then
	Return 0
End If

// Call event to perform any pre-CloseQuery processing
If This.Event pfc_preclose ( ) <> 1 Then
	// Prevent the window from closing
	Return 1  
End If

// Prevent validation error messages from appearing while the window is closing
// and allow others to check if the  CloseQuery process is in progress
ib_closestatus = True

// Check for any pending updates
li_rc = of_UpdateChecks()
If li_rc = 0 Then
	// Updates are NOT pending, allow the window to be closed.
	Return 0
ElseIf li_rc < 0 Then
	// There are Updates pending, but at least one data entry error was found.
	// Give the user an opportunity to close the window without saving changes
	If IsValid(gnv_app.inv_error) Then
		li_msg = gnv_app.inv_error.of_Message('pfc_closequery_failsvalidation', &
					 ls_msgparms, gnv_app.iapp_object.DisplayName)
	Else
		li_msg = MessageBox (gnv_app.iapp_object.DisplayName, &
					"The information entered does not pass validation and "  + &
					"must be corrected before changes can be saved.~r~n~r~n" + &
					"Close without saving changes?", &
					exclamation!, YesNo!, 2)
					dw_cc_par_report_conno.Setfocus()
	End If
	If li_msg = 1 Then
		Return 0
	End If
Else
	// Changes are pending, prompt the user to determine if they should be saved
	If IsValid(gnv_app.inv_error) Then
		li_msg = gnv_app.inv_error.of_Message('pfc_closequery_savechanges',  &
					ls_msgparms, gnv_app.iapp_object.DisplayName)		
	Else
		li_msg = MessageBox ( gnv_app.iapp_object.DisplayName, &
					"Do you want to save changes?", exclamation!, YesNoCancel!)
	End If
	Choose Case li_msg
		Case 1
			// YES - Update
			// If the update fails, prevent the window from closing
			rtn = cb_print.Triggerevent(CLICKED!)
			if rtn = 1 THEN
				RETURN 0
			end if
		Case 2
			// NO - Allow the window to be closed without saving changes
			Return 0
		Case 3
			dw_cc_par_report_conno.Setfocus()
			// CANCEL -  Prevent the window from closing
	End Choose
End If

// Prevent the window from closing
ib_closestatus = False
Return 1
end event

event pfc_preopen;this.of_SetPreference(TRUE)
this.inv_preference.of_SetToolBars(TRUE)
this.inv_preference.of_SetWindow(TRUE)
this.of_SetResize(TRUE)
this.inv_resize.of_SetOrigSize(this.workspacewidth(),this.workspaceheight())
inv_resize.of_Register(dw_cc_par_report_conno, "scale")
inv_resize.of_Register(dw_cc_par_report_conno_cntr, "scale")
inv_resize.of_Register(dw_pctipauth_ace_report, "scale")
inv_resize.of_Register(dw_pctipttl_ec_report, "scale")
inv_resize.of_Register(dw_pcdeviaten_ace_report, "scale")
inv_resize.of_Register(dw_pcdeviaten_ace_report_active, "scale")
inv_resize.of_Register(dw_pcdeviaten_sum_only, "scale")
inv_resize.of_Register(dw_pcparts_txt_report, "scale")
inv_resize.of_Register(dw_pubs1_ec_report, "scale")
inv_resize.of_Register(dw_ttlist_ec_controller, "scale")
inv_resize.of_Register(dw_ttlist_ec_prod, "scale")
inv_resize.of_Register(dw_pcs1chk_ace_report, "scale")
inv_resize.of_Register(dw_pcarchive_ace_report, "scale")
inv_resize.of_Register(dw_cc_par_report_bkno_med, "scale")
inv_resize.of_Register(dw_catalogcheck_sql, "scale")
inv_resize.of_Register(dw_s1check_report, "scale")
inv_resize.of_Register(dw_stage1_annotation, "scale")
inv_resize.of_Register(dw_pcinstr_ace, "scale")
inv_resize.of_Register(dw_receive_date, "scale")
inv_resize.of_Register(dw_qa_receive_date_bkseq, "scale")
inv_resize.of_Register(dw_qa_receive_date_bkmed, "scale")
inv_resize.of_Register(dw_pcnofishe_ace_report, "scale")
inv_resize.of_Register(dw_unable_process_inv, "scale")
inv_resize.of_Register(dw_prod, "scale")
inv_resize.of_Register(cb_cancel, "scale")
inv_resize.of_Register(cb_opf, "scale")
inv_resize.of_Register(cb_print, "scale")
inv_resize.of_Register(cb_addcmts, "scale")
inv_resize.of_Register(cb_print_to_file, "scale")
//inv_resize.of_Register(cb_prod, "scale")
inv_resize.of_Register(sle_rows, "scale")
inv_resize.of_Register(st_rows, "scale")
inv_resize.of_Register(cb_clear, "scale")
//inv_resize.of_Register(cb_par, "scale")
inv_resize.of_Register(cb_dbcancel, "scale")

//debug


il_file = FileOpen("C:\pcsrep.txt", LineMode!, Write!, LockWrite!, Replace!)
Filewrite(il_file,string(today()) + ' ' + string(now()))
Filewrite(il_file,'Preopen of w_pcs_reports')

//debug



end event

event resize;call super::resize;long ll_height

//This script is not commented because when we do minimize this screen then all the DW'S are not resizing 
This.X = w_pics_main.X
This.Y = w_pics_main.Y
ll_height = w_pics_main.mdi_1.Height
This.Resize(w_pics_main.WorkSpaceWidth(), ll_height)


end event

event pfc_postopen;m_pics_main.m_file.m_print.enabled = TRUE
m_pics_main.m_file.m_pagesetup.enabled = TRUE
m_pics_main.m_file.m_printimmediate.enabled = TRUE
this.windowState = maximized!
Date ld_stdt, ld_enddt
Long ll_count, ll_bkseq
String lchno, lcontr_no, ls_cntr,lcntrtype, lps,lps2,lcntrmed,lprdr,ls_bkmed,lqadw,&
		ls_bkmedpre, ls_ricd, ls_html, db_inst, ls_flash
DateTime ldt_stdt, ldt_enddt		
Int i,rtn
Date ld_schenddt, ld_schstdt
Inet linet_base


Boolean multi_contract=FALSE
str_cds_report lstr_cds_report
str_pcs_report lstr_pcs_report
str_qa_report lstr_qa_report
str_prod_cntr lstr_prod_cntr

Filewrite(il_file,'Postopen before wf_connect_check of w_pcs_reports')

rtn=wf_connect_check()
IF rtn<>1 THEN
	Filewrite(il_file,'Postopen wf_connect_check did not work of w_pcs_reports')
	RETURN
END IF
Filewrite(il_file,'Postopen after wf_connect_check of w_pcs_reports')

db_inst = w_pics_main.web_db_inst
Filewrite(il_file,'Postopen-> message stringparm is  of w_pcs_reports ' +  message.stringparm)

CHOOSE CASE message.StringParm
CASE "Par"
	w_pcs_reports.title = "Production Authorization Record"
	dw_cc_par_report_conno.visible = TRUE
	dw_cc_par_report_conno.SetFocus()
	Filewrite(il_file,'Postopen-> Opening dialog box w_pcs_gets_conno -> of w_pcs_reports')
	Open(w_pcs_gets_conno)
	Filewrite(il_file,'Postopen-> After opening dialog box w_pcs_gets_conno -> of w_pcs_reports')
	IF (IsNull(message.StringParm)=FALSE AND message.StringParm<>"") THEN
		lcontr_no = message.StringParm
		IF IsValid(w_pcs_reports) THEN
			SELECT bkseq, bkmed, flash_indicator  INTO :ll_bkseq, :ls_bkmed, :ls_flash
			FROM mchar
			where conno = :lcontr_no USING SqlServerTrans;
			IF f_check_dberror(SqlServerTrans,"MCHAR")=FALSE THEN RETURN
			IF IsNull(ll_bkseq) OR IsNull (ls_bkmed) OR Trim(ls_bkmed)= '' THEN
			//	messageBox('check','ll_bkseq =   '+string(ll_bkseq)+'ls_bkmed =   '+ls_bkmed )
				Messagebox('Error','Cannot print PAR report until book has been assigned to a producer.')
				RETURN
			END IF
			
			// 10/20/2008 
			IF ls_bkmed = 'DB' AND Isnull(ls_flash) THEN
				ls_bkmed = 'RC'
			END IF
				
//			ls_html = 'https://oraserve.loc.gov:4446/'+lower(db_inst)+'/par_report.report4?form_choice=report&form_font=11&form_prodcode=ALL&form_booklist='+string(ll_bkseq)
			// 10/17/2008  Package was expecting book media as arg, not working in TEST environment, book media arg added to url
			Filewrite(il_file,'Postopen-> opening url for the book-> of w_pcs_reports-> ' +  ls_bkmed + string(ll_bkseq))
				
			ls_html = 'https://oraserve.loc.gov:4446/'+lower(db_inst)+'/par_report.report4?form_choice=report&form_font=11&form_prodcode=ALL&form_mid=' + ls_bkmed + '&form_booklist='+string(ll_bkseq)
	
			this.GetContextService("Internet",linet_base)
			Filewrite(il_file,'Postopen-> hyperlink to url opening -> of w_pcs_reports-> ' +  ls_bkmed + string(ll_bkseq))			
			linet_base.HyperlinkToURL(ls_html)
			Filewrite(il_file,'Postopen-> after hyperlink to url opening -> of w_pcs_reports-> ' +  ls_bkmed + string(ll_bkseq))			
			
			If Isvalid(linet_base) Then destroy linet_base
		END IF
	END IF
			
CASE "Titles"
	w_pcs_reports.title = "Titles in Process Report"
	dw_pctipttl_ec_report.visible = TRUE
	dw_pctipttl_ec_report.SetFocus()
//	Open(w_pcs_gets_cat_date)
//	IF (IsNull(Message.StringParm)=False and Message.StringParm<>"") THEN
//		ld_stdt = date(Message.StringParm)
	IF IsValid(w_pcs_reports) THEN
		ll_count = w_pcs_reports.dw_pctipttl_ec_report.Retrieve()	
		IF ll_count = 0 THEN 
			Close(w_pcs_reports)
			m_pics_main.m_menu.m_report.m_pcsreports.m_titlesinprocess.TriggerEvent(clicked!)
			RETURN
		ELSE
			sle_rows.visible=TRUE
			st_rows.visible=TRUE
			sle_rows.text = String(ll_count)
		END IF
	ELSE
		Close(w_pcs_reports)
		Close(w_pcs_gets_cat_date)
		RETURN
	END IF
	
CASE "Author"
	w_pcs_reports.title = "Titles in Process - by Author"
	dw_pctipauth_ace_report.visible = TRUE
	dw_pctipauth_ace_report.SetFocus()
	
	IF IsValid(w_pcs_reports) THEN
		ll_count = w_pcs_reports.dw_pctipauth_ace_report.Retrieve()
		IF ll_count = 0 THEN 
			Close(w_pcs_reports)
			m_pics_main.m_menu.m_report.m_pcsreports.m_titlesinprocessbyauthor.TriggerEvent(clicked!)
		ELSE
			sle_rows.visible=TRUE
			st_rows.visible=TRUE
			sle_rows.text = String(ll_count)
		END IF
	ELSE
		Close(w_pcs_reports)
		Close(w_pcs_gets_dates_title_in_process)
		RETURN
	END IF			
			
CASE "Shipment"
	w_pcs_reports.title = "Multipart books nearing shipment"
	dw_pcparts_txt_report.visible = TRUE
	dw_pcparts_txt_report.SetFocus()
		IF IsValid(w_pcs_reports) THEN
			ll_count = w_pcs_reports.dw_pcparts_txt_report.Retrieve()
			IF ll_count = 0 THEN 
				Close(w_pcs_reports)
				RETURN
			END IF
		ELSE
			Close(w_pcs_reports)
			RETURN
		END IF
			
CASE	"Deviation Act Cntr"
	Boolean all_cntr=FALSE
	w_pcs_reports.title = "Average Deviation"
	dw_pcdeviaten_ace_report.visible = TRUE
	dw_pcdeviaten_ace_report.SetFocus()
	Open(w_pcs_gets_dates_cntr_option_act_cntr)
	lstr_pcs_report = message.powerObjectParm
	IF lstr_pcs_report.b_ok =FALSE THEN
		Close(w_pcs_reports)
		m_pics_main.m_menu.PopMenu(300, 0)
		RETURN
	END IF
	ld_stdt = lstr_pcs_report.ld_stdt
	ld_enddt = lstr_pcs_report.ld_enddt
	IF NOT (lstr_pcs_report.b_cbx_checked)	THEN
		Char lc_cntrtype
		
	   cb_addcmts.visible=TRUE
		cntr_number = lstr_pcs_report.ls_cntr
		cntr_st_dt = lstr_pcs_report.ld_stdt
		
		// Get the contract type
		SELECT cntrtype 
		INTO :lc_cntrtype
		FROM ancntr
		where cntr = :cntr_number
		USING SQLserverOracleTrans;
		IF f_check_dberror(SQLserverOracleTrans,"ANCNTR")=FALSE THEN
			Close(w_pcs_reports)
			RETURN 
		END IF
//		ldt_stdt=datetime(ld_stdt,time('00:00:00'))
//		ldt_enddt=datetime(ldt_enddt,time('00:00:00'))
		IF IsValid(w_pcs_reports) THEN
			ll_count = w_pcs_reports.dw_pcdeviaten_ace_report.Retrieve &
				(ld_stdt, ld_enddt, lstr_pcs_report.ls_cntr, lc_cntrtype)
		ELSE
			Close(w_pcs_reports)
			RETURN
		END IF
		IF (ll_count<>0 OR all_cntr)THEN
			ld_stdt = lstr_pcs_report.ld_stdt
			ld_enddt = lstr_pcs_report.ld_enddt
			dw_pcdeviaten_ace_report.object.st_stdt.text = String(ld_stdt,'mm/dd/yyyy')
			dw_pcdeviaten_ace_report.object.st_enddt.text = String(ld_enddt,'mm/dd/yyyy')
			dw_pcdeviaten_ace_report.object.st_enddt1.text = String(ld_enddt,'mm/dd/yyyy')
		ELSE
			Messagebox("Deviation Report","There are no books matching this criteria, displaying summary only.")
			dw_pcdeviaten_ace_report.visible = FALSE
			dw_pcdeviaten_sum_only.visible = TRUE
			dw_pcdeviaten_sum_only.SetFocus()
			w_pcs_reports.dw_pcdeviaten_sum_only.Retrieve &
			(lstr_pcs_report.ls_cntr, String(lstr_pcs_report.ld_enddt,'mm/dd/yyyy'), lc_cntrtype)
		END IF		
	ELSE // All contracts
		dw_pcdeviaten_ace_report.visible = FALSE
		dw_pcdeviaten_ace_report_active.visible = TRUE
	   cb_addcmts.visible=FALSE
		all_cntr=TRUE
		IF NOT(lstr_pcs_report.b_bks_sorted) THEN
			dw_pcdeviaten_ace_report_active.dataObject ='d_pcdeviaten_ace_report_active_no_sort'
		END IF
		dw_pcdeviaten_ace_report_active.SetTransObject(SqlServerTrans)
		
		dw_pcdeviaten_ace_report_active.Retrieve &
			(lstr_pcs_report.ld_stdt, lstr_pcs_report.ld_enddt)
		
		dw_pcdeviaten_ace_report_active.GroupCalc()
		dw_pcdeviaten_ace_report_active.Sort()
		ld_stdt = lstr_pcs_report.ld_stdt
		ld_enddt = lstr_pcs_report.ld_enddt
		dw_pcdeviaten_ace_report_active.object.st_stdt.text = String(ld_stdt,'mm/dd/yyyy')
		dw_pcdeviaten_ace_report_active.object.st_enddt.text = String(ld_enddt,'mm/dd/yyyy')
		dw_pcdeviaten_ace_report_active.object.st_enddt1.text = String(ld_enddt,'mm/dd/yyyy')
		dw_pcdeviaten_ace_report_active.SetFocus()
		
	END IF
	
CASE "TBT/BBR"
	w_pcs_reports.title = "Annotation for TBT/BBR"
	dw_pubs1_ec_report.visible = TRUE
	dw_pubs1_ec_report.SetFocus()
	Open(w_pcs_gets_date)
	IF (IsNull(message.StringParm)=FALSE AND message.StringParm<>"") THEN
		ld_stdt = Date(message.StringParm)
		ldt_stdt=DateTime(ld_stdt,Time('00:00:00'))
		IF IsValid(w_pcs_reports) THEN
			ll_count = w_pcs_reports.dw_pubs1_ec_report.Retrieve(ldt_stdt)
			IF ll_count = 0 THEN 
				Close(w_pcs_reports)
				m_pics_main.m_menu.m_report.m_pcsreports.m_annotationfortbtbbr.TriggerEvent(clicked!)
				RETURN
			END IF
		ELSE
			Close(w_pcs_reports)
			Close(w_pcs_gets_date)
			RETURN
		END IF
	END IF	
CASE "Exception report"
	w_pcs_reports.title = "Exception report for titles passing final review"
	dw_pcs1chk_ace_report.visible = TRUE
	dw_pcs1chk_ace_report.SetFocus()
	Open(w_pcs_gets_fr_date)
	IF (IsNull(message.StringParm)=FALSE AND message.StringParm<>"") THEN
		ld_stdt = Date(message.StringParm)
		IF IsValid(w_pcs_reports) THEN
			ll_count = w_pcs_reports.dw_pcs1chk_ace_report.Retrieve(ld_stdt)	
			IF ll_count = 0 THEN 
				Close(w_pcs_reports)
				m_pics_main.m_menu.m_report.m_pcsreports.m_exceptionreportfortitlespassingf.TriggerEvent(clicked!)
				RETURN
			END IF
		ELSE
			Close(w_pcs_reports)
			Close(w_pcs_gets_fr_date)
			RETURN
		END IF
	END IF
	
CASE "Producer"
	w_pcs_reports.title = "Titles Assigned to Producer"
	dw_ttlist_ec_prod.visible = TRUE
	dw_ttlist_ec_prod.SetFocus()
	IF IsValid(w_pcs_gets_date_cntr) = FALSE THEN Open(w_pcs_gets_date_cntr)
		lstr_prod_cntr = message.powerObjectParm
		IF IsValid(w_pcs_reports) THEN
			ld_stdt=lstr_prod_cntr.ld_stdt
			ldt_stdt=DateTime(ld_stdt,Time('00:00:00'))
			OpenWithParm(w_pics_retrieve_msg_box,"Retrieving Title Listing for Producer, Please Wait...")
			ll_count = w_pcs_reports.dw_ttlist_ec_prod.Retrieve(ldt_stdt ,lstr_prod_cntr.ls_cntr)
			IF ll_count = 0 THEN 
				Close(w_pics_retrieve_msg_box)
				Close(w_pcs_reports)
				m_pics_main.m_menu.m_report.m_pcsreports.m_titlesassignedtoproducer.TriggerEvent(clicked!)
				RETURN
			ELSE
				ls_cntr = lstr_prod_cntr.ls_cntr
				SELECT cntrtype,cntrmed,prdr INTO :lcntrtype,:lcntrmed,:lprdr FROM ancntr where cntr = :ls_cntr USING SqlServerTrans;
				IF f_check_dberror(SqlServerTrans,"ANCNTR")=TRUE THEN
					IF lcntrtype = "T" THEN
						IF lcntrmed = 'BR' THEN
							lps = 'PR'
						ELSEIF lcntrmed = 'P/B' THEN
							lps = 'PB'
						ELSEIF lcntrmed = 'RC' THEN
							lps = 'DU'
						ELSE
							lps = 'DU'
						END IF
					ELSEIF lcntrtype = "D" THEN
						IF lcntrmed = 'P/B ' THEN
							lps = 'PB'
						ELSE
							lps = 'DU'
						END IF
					ELSEIF lcntrtype = "M" THEN
						IF lcntrmed = 'P/B ' THEN
							lps = 'PU'
						ELSEIF lcntrmed = 'RC' THEN
							lps = 'MA'
							lps2 = 'AB'
						ELSEIF lcntrmed = 'BR' THEN
							lps = 'MA'
							lps2 = 'AB'
						END IF
					ELSE
						lps = 'MA'
						lps2 = 'AB'
					END IF
				ELSE
					Close(w_pics_retrieve_msg_box)
					RETURN
				END IF					
				ll_count = dw_ttlist_ec_prod.RowCount()
//				MessageBox("values","row count="+string(ll_count)+" prodstage= "+Lps+" contract type= "+Lcntrtype+" contract = "+ls_cntr+" media= "+Lcntrmed)
				FOR i=1 TO ll_count 
					ll_bkseq = dw_ttlist_ec_prod.object.prod_bkseq[i]
					SELECT schenddt INTO :ld_schenddt FROM prod 
					where bkseq = :ll_bkseq AND prodstage in (:lps,:lps2)
					USING SqlServerTrans;
					dw_ttlist_ec_prod.object.prod_schenddt[i] = String(ld_schenddt,'MM/DD/YYYY')
					dw_ttlist_ec_prod.object.mchar_chno[i] = lprdr
				NEXT
				Close(w_pics_retrieve_msg_box)
			END IF
		ELSE
			Close(w_pcs_reports)
			Close(w_pcs_gets_date_cntr)
			RETURN
		END IF	
CASE "Controller"
	w_pcs_reports.title = "Titles Assigned to Controller"
	dw_ttlist_ec_controller.visible = TRUE
	dw_ttlist_ec_controller.SetFocus()
	IF IsValid(w_pcs_gets_date_cntr) = FALSE THEN Open(w_pcs_gets_date_cntr)
		lstr_prod_cntr = message.powerObjectParm
		IF IsValid(w_pcs_reports) THEN
			OpenWithParm(w_pics_retrieve_msg_box,"Retrieving Title Listing for Controller, Please Wait...")
			ll_count = w_pcs_reports.dw_ttlist_ec_controller.Retrieve(lstr_prod_cntr.ld_stdt ,lstr_prod_cntr.ls_cntr)
			IF ll_count = 0 THEN 
				Close(w_pics_retrieve_msg_box)
				Close(w_pcs_reports)
				m_pics_main.m_menu.m_report.m_pcsreports.m_titlesassignedtocontroller.TriggerEvent(clicked!)
				RETURN
			ELSE
				ls_cntr = lstr_prod_cntr.ls_cntr
				SELECT cntrtype,cntrmed,prdr INTO :lcntrtype,:lcntrmed,:lprdr FROM ancntr where cntr = :ls_cntr USING SqlServerTrans;
				IF f_check_dberror(SqlServerTrans,"ANCNTR")=TRUE THEN
					IF lcntrtype = "T" THEN
						IF lcntrmed = 'BR' THEN
							lps = 'PR'
						ELSEIF lcntrmed = 'P/B' THEN
							lps = 'PB'
						ELSEIF lcntrmed = 'RC' THEN
							lps = 'DU'
						ELSE
							lps = 'DU'
						END IF
					ELSEIF lcntrtype = "D" THEN
						IF lcntrmed = 'P/B ' THEN
							lps = 'PB'
						ELSE
							lps = 'DU'
						END IF
					ELSEIF lcntrtype = "M" THEN
						IF lcntrmed = 'P/B ' THEN
							lps = 'PU'
						ELSEIF lcntrmed = 'RC' THEN
							lps = 'MA'
							lps2 = 'AB'
						ELSEIF lcntrmed = 'BR' THEN
							lps = 'MA'
							lps2 = 'AB'
						END IF
					ELSE
						lps = 'MA'
						lps2 = 'AB'
					END IF
				ELSE
					Close(w_pics_retrieve_msg_box)
					RETURN
				END IF					
				ll_count = dw_ttlist_ec_controller.RowCount()
//				MessageBox("values","row count="+string(ll_count)+" prodstage= "+Lps+" contract type= "+Lcntrtype+" contract = "+ls_cntr+" media= "+Lcntrmed)
				FOR i=1 TO ll_count 
					ll_bkseq = dw_ttlist_ec_controller.object.prod_bkseq[i]
					SELECT schenddt INTO :ld_schenddt FROM prod 
					where bkseq = :ll_bkseq AND prodstage in (:lps,:lps2)
					USING SqlServerTrans;
					dw_ttlist_ec_controller.object.prod_schenddt[i] = String(ld_schenddt,'MM/DD/YYYY')
					dw_ttlist_ec_controller.object.mchar_chno[i] = lprdr
				NEXT
				Close(w_pics_retrieve_msg_box)
			END IF
		ELSE
			Close(w_pcs_reports)
			Close(w_pcs_gets_date_cntr)
			RETURN
		END IF	
CASE "Not in Microfiche"
	w_pcs_reports.title = "Titles Shipped but Yet in Microfiche"
	dw_pcnofishe_ace_report.visible = TRUE
	dw_pcnofishe_ace_report.SetFocus()
	Open(w_pcs_gets_dates_cd_blnd)
		lstr_cds_report = message.powerObjectParm
		IF IsValid(w_pcs_reports) THEN
			ll_count = w_pcs_reports.dw_pcnofishe_ace_report.Retrieve(lstr_cds_report.ld_stdt ,lstr_cds_report.ld_enddt)
			IF ll_count = 0 THEN 
				Close(w_pcs_reports)
				m_pics_main.m_menu.m_report.m_pcsreports.m_titlesshippedbutnotyetinmicrof.TriggerEvent(clicked!)
				RETURN
			END IF
		ELSE
			Close(w_pcs_reports)
			Close(w_pcs_gets_dates_cd_blnd)
			RETURN
		END IF	
CASE "Archived"
	w_pcs_reports.title = "Books archived"
	dw_pcarchive_ace_report.visible = TRUE
	dw_pcarchive_ace_report.SetFocus()
		IF IsValid(w_pcs_reports) THEN
			ll_count = w_pcs_reports.dw_pcarchive_ace_report.Retrieve()
			IF ll_count = 0 THEN 
				Close(w_pcs_reports)
				RETURN
			END IF
		ELSE
			Close(w_pcs_reports)
			RETURN
		END IF	
CASE "Stage 1"
	w_pcs_reports.title = "Stage 1 Check Report"
	dw_s1check_report.visible = TRUE
	dw_s1check_report.SetFocus()
	Open(w_pcs_gets_date_s1)
	IF (IsNull(message.StringParm)=FALSE AND message.StringParm<>"") THEN
		ld_stdt = Date(message.StringParm)
		IF IsValid(w_pcs_reports) THEN
			ll_count = w_pcs_reports.dw_s1check_report.Retrieve(ld_stdt)	
			IF ll_count = 0 THEN 
				Close(w_pcs_reports)
				m_pics_main.m_menu.m_report.m_pcsreports.m_stage1checkreport.TriggerEvent(clicked!)
				RETURN
			END IF
		ELSE
			Close(w_pcs_reports)
		END IF
	ELSE
		Close(w_pcs_reports)
		Close(w_pcs_gets_date_s1)
		RETURN
	END IF
CASE "Annotation"
	w_pcs_reports.title = "Stage 1 Annotation Report"
	dw_stage1_annotation.visible = TRUE
	dw_stage1_annotation.SetFocus()
	Open(w_pcs_gets_date_s1)
	IF (IsNull(message.StringParm)=FALSE AND message.StringParm<>"") THEN
		ld_stdt = Date(message.StringParm)
			IF IsValid(w_pcs_reports) THEN
				ll_count = w_pcs_reports.dw_stage1_annotation.Retrieve(ld_stdt)	
				IF ll_count = 0 THEN 
					Close(w_pcs_reports)
					m_pics_main.m_menu.m_report.m_pcsreports.m_stage1annotation.TriggerEvent(clicked!)
					RETURN
				END IF
			ELSE
				Close(w_pcs_reports)
			END IF
		ELSE
			Close(w_pcs_reports)
			Close(w_pcs_gets_date_s1)
			RETURN
		END IF
		RETURN		
CASE "Catalog"
	w_pcs_reports.title = "Catalog Check Report"
	dw_catalogcheck_sql.visible = TRUE
	dw_catalogcheck_sql.SetFocus()
	Open(w_pcs_gets_date_s1)
		IF (IsNull(message.StringParm)=FALSE AND message.StringParm<>"") THEN
			ld_stdt = Date(message.StringParm)
			IF IsValid(w_pcs_reports) THEN
				ll_count = w_pcs_reports.dw_catalogcheck_sql.Retrieve(ld_stdt)	
				IF ll_count = 0 THEN 
					Close(w_pcs_reports)
					m_pics_main.m_menu.m_report.m_pcsreports.m_catalogcheckreport.TriggerEvent(clicked!)
					RETURN
				END IF
			ELSE
				Close(w_pcs_reports)
			END IF
		ELSE
			Close(w_pcs_reports)
			Close(w_pcs_gets_date_s1)
			RETURN
		END IF
			
CASE "Instruction"
	w_pcs_reports.title = "Special Instruction Report"
	dw_pcinstr_ace.visible = TRUE
	dw_pcinstr_ace.SetFocus()
	Open(w_pcs_gets_conno)
	IF (IsNull(message.StringParm)=FALSE AND message.StringParm<>"") THEN
		lcontr_no = message.StringParm
		IF IsValid(w_pcs_reports) THEN
			ll_count = w_pcs_reports.dw_pcinstr_ace.Retrieve(lcontr_no)
			IF ll_count = 0 THEN 
				Close(w_pcs_reports)
				m_pics_main.m_menu.m_report.m_pcsreports.m_specialinstruction.TriggerEvent(clicked!)
				RETURN
			END IF
		ELSE
			Close(w_pcs_reports)
		END IF
	ELSE
		Close(w_pcs_reports)
		Close(w_pcs_gets_conno)
		RETURN
	END IF
CASE "QA"
	w_pcs_reports.title = "Quality Assurance Report"
	dw_receive_date.visible = TRUE
	dw_receive_date.SetFocus()
	Open(w_pcs_gets_qarec_date)
	lstr_qa_report = message.powerObjectParm
	IF IsValid(w_pcs_reports) THEN
		lqadw = lstr_qa_report.ls_qamode
		CHOOSE CASE lqadw
			CASE 'dateonly'
				//messagebox("qa","mode is: "+lqadw )
				ll_count = w_pcs_reports.dw_receive_date.Retrieve(lstr_qa_report.ld_recdt)	
				IF ll_count = 0 THEN 
					Close(w_pcs_reports)
//					m_pics_main.m_menu.m_report.m_qareports.m_qareceivedcards.TriggerEvent(Clicked!)
					RETURN
				ELSE
					sle_rows.visible=TRUE
					st_rows.visible=TRUE
					sle_rows.text = String(ll_count)
				END IF				
			CASE 'date_bkseq'
				dw_receive_date.visible = FALSE
				dw_qa_receive_date_bkseq.visible = TRUE
				//messagebox("qa","mode is: "+lqadw )
				ll_count = w_pcs_reports.dw_qa_receive_date_bkseq.Retrieve(lstr_qa_report.ld_recdt,lstr_qa_report.ln_bkseq)
				IF ll_count = 0 THEN 	 //dw_qa_receive_date_bkseq
					Close(w_pcs_reports)
//					m_pics_main.m_menu.m_report.m_qareports.m_qareceivedcards.TriggerEvent(Clicked!)
					RETURN
				ELSE
					sle_rows.visible=TRUE
					st_rows.visible=TRUE
					sle_rows.text = String(ll_count)
				END IF				
				dw_qa_receive_date_bkseq.SetFocus()				
			CASE 'date_bkmed'
				dw_receive_date.visible = FALSE
				dw_qa_receive_date_bkmed.visible = TRUE
				//messagebox("qa","mode is: "+lqadw )
				ll_count = w_pcs_reports.dw_qa_receive_date_bkmed.Retrieve(lstr_qa_report.ld_recdt,lstr_qa_report.ls_bkmed)	
				IF ll_count = 0 THEN 
					Close(w_pcs_reports)
//					m_pics_main.m_menu.m_report.m_qareports.m_qareceivedcards.TriggerEvent(Clicked!)
					RETURN
				ELSE
					sle_rows.visible=TRUE
					st_rows.visible=TRUE
					sle_rows.text = String(ll_count)
				END IF				
				dw_qa_receive_date_bkmed.SetFocus()
			CASE 'alldata'
				dw_receive_date.visible = FALSE
				dw_qa_receive_date_bkseq.visible = TRUE
				//messagebox("qa","mode is: "+lqadw )
				ll_count = w_pcs_reports.dw_qa_receive_date_bkseq.Retrieve(lstr_qa_report.ld_recdt,lstr_qa_report.ln_bkseq)	
				IF ll_count = 0 THEN 
					Close(w_pcs_reports)
//					m_pics_main.m_menu.m_report.m_qareports.m_qareceivedcards.TriggerEvent(Clicked!)
					RETURN
				ELSE
					sle_rows.visible=TRUE
					st_rows.visible=TRUE
					sle_rows.text = String(ll_count)
				END IF				
				dw_qa_receive_date_bkseq.SetFocus()	
			
			CASE ELSE
		END CHOOSE			
	ELSE
		Close(w_pcs_reports)
		Close(w_pcs_gets_qarec_date)
		RETURN
	END IF
CASE "Unable To Process"
	Int rowcnt
	cb_dbcancel.visible=FALSE
	cb_clear.visible=FALSE
	cb_print_to_file.visible=FALSE
	dw_prod.visible=TRUE	
	dw_unable_process_inv.visible=TRUE
	dw_unable_process_inv.SetTransObject(SqlServerTrans)
	rowcnt = dw_unable_process_inv.Retrieve()
	FOR i= 1 TO rowcnt
		dw_unable_process_inv.object.inv_nos[i]=""
		dw_unable_process_inv.object.org_inv[i]=""
		dw_unable_process_inv.object.other[i]=""
		dw_unable_process_inv.object.chk1[i]=""
		dw_unable_process_inv.object.chk2[i]=""
		dw_unable_process_inv.object.chk3[i]=""
		dw_unable_process_inv.object.chk4[i]=""
		dw_unable_process_inv.object.chk5[i]=""
		dw_unable_process_inv.object.chk6[i]=""
		dw_unable_process_inv.object.chk7[i]=""
		dw_unable_process_inv.object.chk8[i]=""
		dw_unable_process_inv.object.chk9[i]=""
		dw_unable_process_inv.object.chk10[i]=""
		dw_unable_process_inv.object.chk11[i]=""
		dw_unable_process_inv.object.chk12[i]=""
		dw_unable_process_inv.object.chk13[i]=""
	NEXT		
	dw_prod.SetTransObject(SqlServerTrans)
	dw_prod.Retrieve()
END CHOOSE





end event

event open;call super::open;messagebox('open','in open evewnt')
end event

type ole_ezftp from u_ezftp within w_pcs_reports
boolean visible = false
integer x = 343
integer y = 1268
integer width = 1317
integer height = 768
integer taborder = 40
string binarykey = "w_pcs_reports.win"
end type

type dw_dtbbkseq from u_pics_dw within w_pcs_reports
boolean visible = false
integer x = 192
integer y = 1260
integer width = 123
integer height = 56
integer taborder = 20
string dataobject = "d_dtbbkseq"
boolean vscrollbar = false
boolean border = false
boolean livescroll = false
end type

event constructor;call super::constructor;This.of_SetTransObject(sqlservertrans)

end event

type cb_opf from commandbutton within w_pcs_reports
boolean visible = false
integer x = 887
integer y = 1180
integer width = 325
integer height = 80
integer taborder = 40
boolean bringtotop = true
integer textsize = -8
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
boolean enabled = false
string text = "&Create OPF"
end type

event clicked;n_ds lds
String Lcntrtype,ls_cfile,ls_dc_url,  ls_oebpackage, ls_ttl, ls_anno, ls_pub,	 ls_t_date, ls_creator, ls_subject, ls_manifest, ls_spine, ls_remotefile, ls_remoteloc,ls_prdr	   
String ls_forma, ls_Identifier, ls_isbn, ls_lang, ls_t_rights, ls_pubyr, ls_sourcepublisher, ls_sourcerights
String ls_aType, ls_dtbnarrator, ls_dtbproducer, ls_dtbproducedate, ls_dtbrevision, ls_dtbrevisiondate, ls_dtbrevisiondescription
string ls_t_metadata, ls_t_dcmetadata, ls_t_dcmetadata2, ls_t_x_metadata, ls_t_xx_metadata, ls_t_close_metadata
String ls_dtbtotaltime,	 ls_dtbaudioformat , lmsg, ls_xmlver, ls_doctype, ls_xmlnls, ls_package, ls_text, ls_chno    
String ls_ftpsite, ls_ftpuid, ls_ftppwd

int ll_rows,ll_rows2,rtn,i,j,li_ftp_rc
long l_bkseq[],li_filenum,li_bkseq

lds = create n_ds
lds.dataobject = "d_dtbcreators"

SELECT ftp_site, ftp_uid, ftp_pwd
INTO :ls_ftpsite, :ls_ftpuid, :ls_ftppwd
FROM PCS_FTP_INFO
USING SQLServerTrans;
IF f_check_dberror(SQLServerTrans,"SELECTING FROM PCS_FTP_INFO ") THEN

	li_ftp_rc = w_pcs_reports.ole_ezftp.uf_login ( ls_ftpsite, ls_ftpuid, ls_ftppwd)
	
	IF li_ftp_rc = -1 THEN
		messagebox("FTP Error", "Unable to connect to ftp server.")
	ELSE
		ls_prdr = lower(trim(dw_cc_par_report_conno.object.ancntr_prdr[1]))
		ls_remoteloc = "/pics/prd/opffiles/"+ls_prdr
		w_pcs_reports.ole_ezftp.uf_set_currentdirectory(ls_remoteloc)
	END IF
ELSE
	messagebox("FTP Error", "Unable to get ftp information.")
	li_ftp_rc = -1
	
END IF


l_bkseq[1] = dw_cc_par_report_conno.object.mchar_bkseq[1]

	
OpenWithParm(w_pics_retrieve_msg_box,"Getting DTD records, Please Wait...")
dw_dtbbkseq.SetTransObject(SQLServerTrans)
ll_rows = dw_dtbbkseq.retrieve(l_bkseq[])
	
Close(w_pics_retrieve_msg_box)

if ll_rows = 0 THEN
	MessageBox("ERROR", "No DTB record(s) for OPF was found. Check the book number.")
	return
else
	// record found and now create the file from the datawindow dtbbkseqs
	//messagebox('books',string(ll_rows))
	FOR i= 1 TO ll_rows
				 ls_xmlver = ""
				 ls_doctype = ""
				 ls_xmlnls = ""
				 ls_dc_url = ""
				 ls_ttl = ""
				 ls_anno = ""			   
				 ls_pub = ""
				 ls_t_date = ""			   
				 ls_forma = ""
				 ls_Identifier = ""
				 ls_isbn= ""			   
				 ls_lang= ""
				 ls_t_rights= ""
				 ls_pubyr= ""
				 ls_sourcepublisher= ""
				 ls_sourcerights= ""
				 ls_aType= ""
				 ls_dtbnarrator= ""
				 ls_dtbproducer= ""
				 ls_dtbproducedate= ""
				 ls_dtbrevision= ""
				 ls_dtbrevisiondate= ""
				 ls_dtbrevisiondescription= ""
				 ls_dtbtotaltime= ""
				 ls_dtbaudioformat= ""          
				 ls_manifest = ""
				 ls_spine = ""
				 ls_package = ""
				 ls_creator = ""
				 ls_text = ""
				 ls_subject = ""
				 ls_chno = ""
				
				 ls_cfile= ""
				 li_filenum = -1
				
			 	 li_bkseq = dw_dtbbkseq.object.mchar_bkseq[i]
//				MessageBox("data",string(l_bkseq[i])+" i = "+string(i)+" ll_rows = "+string(ll_rows))
				ls_cfile="P:\opffiles\"+string(li_bkseq)+".opf"
				ls_remotefile = string(li_bkseq)+".opf"
//				MessageBox("data",ls_cfile)
				li_filenum = Fileopen(ls_cfile,streammode!,write!,lockwrite!,Replace!)
				if li_filenum = -1 then
					messagebox("File Error","This file could not be opened. Make sure the directory p:\opffiles exist.")
					Close(w_pics_retrieve_msg_box)
					RollBack using SqlServerTrans;
				
					RETURN -1
				end if
	
				lmsg = "Getting DTD file"+ls_cfile+", Please Wait..." 
				OpenWithParm(w_pics_retrieve_msg_box,lmsg)
	
			 	 ls_chno = dw_dtbbkseq.object.chno[i]
				 ls_xmlver = dw_dtbbkseq.object.t_xmlver[i]+"~n"
				 ls_doctype = dw_dtbbkseq.object.t_doctype[i]+"~n"
				 ls_xmlnls = dw_dtbbkseq.object.t_xmlnls[i]+"~n"
				 ls_t_metadata = dw_dtbbkseq.object.t_metadata[i]+"~n"
				 ls_dc_url = dw_dtbbkseq.object.dc_url[i]+"~n"
//				 ls_ttl = f_change_pipe_italics_tags(f_replace_nonprn_html_tags(dw_dtbbkseq.object.ttl[i]))+"~n"
				 ls_ttl = f_replace_nonprn_html_tags(dw_dtbbkseq.object.ttl[i])+"~n"
			 
				 ll_rows2 = lds.settransobject(sqlservertrans)
				 ll_rows2 = lds.retrieve(ls_chno)
				 
				 ls_creator = ' '
				 FOR j = 1 to ll_rows2
					ls_text = lds.object.t_creators[j]+"~n"
					ls_creator = ls_creator + ls_text 			
				 NEXT
				 ls_creator = Trim(ls_creator)
			 
				 ls_subject = dw_dtbbkseq.object.t_subject[i]+"~n"
//				 ls_anno = f_change_pipe_italics_tags(f_replace_nonprn_html_tags(dw_dtbbkseq.object.anno[i]))+"~n"   
				 ls_anno = f_replace_nonprn_html_tags(dw_dtbbkseq.object.anno[i])+"~n"   
				 ls_pub = dw_dtbbkseq.object.pub[i]+"~n"
				 ls_t_date = dw_dtbbkseq.object.t_date[i]+"~n"
				 ls_forma = dw_dtbbkseq.object.forma[i]+"~n"
				 ls_Identifier = dw_dtbbkseq.object.Identifier[i]+"~n"
				 ls_isbn= dw_dtbbkseq.object.isbn[i]+"~n"
				 ls_lang= dw_dtbbkseq.object.lang[i]+"~n"
				 ls_t_rights= dw_dtbbkseq.object.t_rights[i]+"~n"
				 ls_t_dcmetadata = dw_dtbbkseq.object.t_dcmetadata[i]+"~n"
				 ls_t_x_metadata = dw_dtbbkseq.object.t_x_metadata[i]+"~n"
				 ls_pubyr= dw_dtbbkseq.object.pubyr[i]+"~n"
				 ls_sourcepublisher= dw_dtbbkseq.object.sourcepublisher[i]+"~n"
				 ls_sourcerights= dw_dtbbkseq.object.sourcerights[i]+"~n"
				 ls_aType= dw_dtbbkseq.object.aType[i]+"~n"
				 ls_dtbnarrator= dw_dtbbkseq.object.dtbnarrator[i]+"~n"
				 ls_dtbproducer= dw_dtbbkseq.object.dtbproducer[i]+"~n"
				 ls_dtbproducedate= dw_dtbbkseq.object.dtbproducedate[i]+"~n"
				 ls_dtbrevision= dw_dtbbkseq.object.dtbrevision[i]+"~n"
				 ls_dtbrevisiondate= dw_dtbbkseq.object.dtbrevisiondate[i]+"~n"
				 ls_dtbrevisiondescription= dw_dtbbkseq.object.dtbrevisiondescription[i]+"~n"
				 ls_dtbtotaltime= dw_dtbbkseq.object.dtbtotaltime[i]+"~n"
				 ls_dtbaudioformat= dw_dtbbkseq.object.dtbaudioformat[i]+"~n"
				 ls_t_xx_metadata = dw_dtbbkseq.object.t_xx_metadata[i]+"~n"
				 ls_t_close_metadata = dw_dtbbkseq.object.t_close_metadata[i]+"~n"
				 ls_manifest = dw_dtbbkseq.object.t_manifest[i]+"~n"
				 ls_spine = dw_dtbbkseq.object.t_spine[i]+"~n"
				 ls_package = dw_dtbbkseq.object.t_package[i]+"~n"
				 
				filewrite(li_filenum, ls_xmlver)
				filewrite(li_filenum, ls_doctype)
				filewrite(li_filenum, ls_xmlnls)
				filewrite(li_filenum, ls_t_metadata)
				filewrite(li_filenum, ls_dc_url)
				filewrite(li_filenum, ls_ttl)
				filewrite(li_filenum, ls_creator)
				filewrite(li_filenum, ls_subject)
				filewrite(li_filenum, ls_anno)
				filewrite(li_filenum, ls_pub)
				filewrite(li_filenum, ls_t_date)
				filewrite(li_filenum, ls_forma)
				filewrite(li_filenum, ls_Identifier)
				filewrite(li_filenum, ls_isbn)
				filewrite(li_filenum, ls_lang)
				filewrite(li_filenum, ls_t_rights)
				filewrite(li_filenum, ls_t_dcmetadata)
				filewrite(li_filenum, ls_t_x_metadata)
				filewrite(li_filenum, ls_pubyr)
				filewrite(li_filenum, ls_sourcepublisher)
				filewrite(li_filenum, ls_sourcerights)
				filewrite(li_filenum, ls_aType)
				filewrite(li_filenum, ls_dtbnarrator)
				filewrite(li_filenum, ls_dtbproducer)
				filewrite(li_filenum, ls_dtbproducedate)
				filewrite(li_filenum, ls_dtbrevision)
				filewrite(li_filenum, ls_dtbrevisiondate)
				filewrite(li_filenum, ls_dtbrevisiondescription)
				filewrite(li_filenum, ls_dtbtotaltime)
				filewrite(li_filenum, ls_dtbaudioformat)
				filewrite(li_filenum, ls_t_xx_metadata)
				filewrite(li_filenum, ls_t_close_metadata)
				filewrite(li_filenum, ls_manifest)
				filewrite(li_filenum, ls_spine)
				filewrite(li_filenum, ls_package)
	
				fileclose(li_filenum)
	
				IF li_ftp_rc <> -1 THEN
					w_pcs_reports.ole_ezftp.uf_upload ( ls_cfile, ls_remotefile , FALSE )
				END IF
	NEXT
	Close(w_pics_retrieve_msg_box)
		
	IF li_ftp_rc <> -1 THEN
		w_pcs_reports.ole_ezftp.uf_logout()
	END IF
	
end if
end event

type dw_prod from u_pics_dw within w_pcs_reports
boolean visible = false
integer x = 558
integer y = 1184
integer width = 306
integer height = 64
integer taborder = 30
string dataobject = "d_producer"
boolean vscrollbar = false
boolean livescroll = false
end type

event itemchanged;call super::itemchanged;string ls_name, ls_orgcd, ls_filter

ls_name=dwo.name
if ls_name="orgcd" then
	ls_orgcd=data
	ls_filter="orgcd ='"+ls_orgcd+"'"
	dw_unable_process_inv.SetFilter(ls_filter)
	dw_unable_process_inv.Filter()
end if
	
end event

type dw_par_on_web from u_pics_dw within w_pcs_reports
boolean visible = false
integer x = 87
integer y = 1256
integer width = 69
integer height = 48
integer taborder = 20
string dataobject = "d_create_par_record_web"
boolean vscrollbar = false
boolean livescroll = false
end type

event sqlpreview;call super::sqlpreview;//messagebox("sql",sqlsyntax)
end event

type cb_print from u_cb within w_pcs_reports
integer x = 2139
integer y = 1180
integer width = 197
integer height = 80
integer taborder = 0
string text = "&Print"
end type

event clicked;call super::clicked;int rtn
string lconno,ldefprinter,prntcmd,db_inst,lmed
Inet linet_base

db_inst = w_pics_main.web_db_inst

//ldefprinter = nvo_PowerPrn.of_GetDefaultPrinterName()
//prntcmd = "C:\PICSORCL9i\picsul "+"~""+ldefprinter+"~""


//IF dw_cc_par_report_conno.Visible = True THEN
//	
//	lconno = dw_cc_par_report_conno.object.mchar_conno[1]
//	lmed = dw_cc_par_report_conno.object.mchar_med[1]
//	
//	//This part of code is disabled since PAR are printed from WEB screens
//	//w_pcs_reports.wf_create_par_on_web(lconno)
//	
//	rtn = MessageBox("PAR on the WEB","Would you like to display PAR on the web?",Question!,YesNo!,1)
//	
//	IF rtn = 2 THEN
//		rtn = dw_cc_par_report_conno.Event pfc_Print()
//		IF rtn = 1 THEN
//			//messagebox("command is",prntcmd)
//			//rtn = Run(prntcmd,Minimized!)
//			//IF rtn = -1 THEN
//			//	MessageBox("ERROR","Error in running:  "+prntcmd)
//			//END IF	
//		END IF
//		
//		// If the book is RC then send the opf file to the producer directory
//		IF trim(lmed) = 'RC' THEN
//			//messagebox("media","media is RC send the opf.")
//			cb_opf.TriggerEvent(Clicked!)
//		END IF
//	ELSE
//		long lbkseq
//		string ls_html
//				
//		lbkseq = dw_cc_par_report_conno.object.mchar_bkseq[1]
//	
//		//ls_html = 'http://rs21.loc.gov:7776/pls/picp/report4?form_choice=console&form_font=12&form_prodcode=ALL&form_id='+string(lparid)
//			 
//		ls_html = 'http://rs21.loc.gov:7776/pls/'+db_inst+'/report4?form_choice=report&form_font=14&form_prodcode=ALL&form_booklist='+string(lbkseq)
// 
//		
//		//messagebox('ls_html',ls_html)
//	
//		this.GetContextService("Internet",linet_base)
//		linet_base.HyperlinkToURL(ls_html)
//		
//		If Isvalid(linet_base) Then destroy linet_base
//		
//		// If the book is RC then send the opf file to the producer directory
//		IF trim(lmed) = 'RC' THEN
//			//messagebox("media","media is RC send the opf.")
//			cb_opf.TriggerEvent(Clicked!)
//		END IF
//	
//	END IF
//	lconno = dw_cc_par_report_conno.object.mchar_conno[1]
//	f_set_parprt(lconno,'P')
//	
//	dw_cc_par_report_conno.GroupCalc()
//	dw_cc_par_report_conno.SetRedraw(true)
//	
//ELSEIF dw_cc_par_report_conno_cntr.Visible = True THEN
//	rtn = dw_cc_par_report_conno_cntr.Event pfc_Print()
//	IF rtn = 1 THEN
//		rtn = Run(prntcmd,Minimized!)
//		IF rtn = -1 THEN
//			MessageBox("ERROR","Error in running:  "+prntcmd)
//		END IF	
//	END IF	
//	lconno = dw_cc_par_report_conno_cntr.object.mchar_conno[1]
//	f_set_parprt(lconno,'P')
//	
//ELSEIF dw_cc_par_report_bkno_med.Visible = True THEN
//	rtn = dw_cc_par_report_bkno_med.Event pfc_Print()
//	IF rtn = 1 THEN
//		rtn = Run(prntcmd,Minimized!)
//		IF rtn = -1 THEN
//			MessageBox("ERROR","Error in running:  "+prntcmd)
//		END IF	
//	END IF	
//	dw_cc_par_report_conno.Visible = False	
//	lconno = dw_cc_par_report_bkno_med.object.mchar_conno[1]
//	f_set_parprt(lconno,'P')
//	
IF dw_pctipttl_ec_report.Visible = True THEN
	
	nvo_PowerPrn.of_SetPrinterOrientation(2)
	dw_pctipttl_ec_report.TriggerEvent("pfc_Print")
	dw_cc_par_report_bkno_med.Visible = False
	
ELSEIF dw_pctipauth_ace_report.Visible = True THEN
	nvo_PowerPrn.of_SetPrinterOrientation(2)
	dw_pctipauth_ace_report.TriggerEvent("pfc_Print")
	dw_pctipttl_ec_report.Visible = False
	
//ELSEIF dw_pctipttl_ec_report.Visible = True THEN
//	nvo_PowerPrn.of_SetPrinterOrientation(2)
////	nvo_PowerPrn.of_SetPaperSource(4)
////	nvo_PowerPrn.of_SetPaperSize(20)
//	dw_pctipttl_ec_report.TriggerEvent("pfc_Print")
//	dw_pctipauth_ace_report.Visible = False
	
ELSEIF dw_pcparts_txt_report.Visible = True THEN
	dw_pcparts_txt_report.TriggerEvent("pfc_Print")
	dw_pctipauth_ace_report.Visible = False
	
ELSEIF dw_pcdeviaten_ace_report.Visible = True THEN
	dw_pcdeviaten_ace_report.TriggerEvent("pfc_Print")
	dw_pcparts_txt_report.Visible = False	

ELSEIF dw_pcdeviaten_ace_report_active.Visible = True THEN
	dw_pcdeviaten_ace_report_active.TriggerEvent("pfc_Print")
	dw_pcparts_txt_report.Visible = False	
	
ELSEIF dw_pcdeviaten_sum_only.Visible = True THEN
	dw_pcdeviaten_sum_only.TriggerEvent("pfc_Print")
	dw_pcparts_txt_report.Visible = False	

ELSEIF dw_pubs1_ec_report.Visible = True THEN
	dw_pubs1_ec_report.TriggerEvent("pfc_Print")
	dw_pcdeviaten_ace_report.Visible = False
	
ELSEIF dw_ttlist_ec_controller.Visible = True THEN
	dw_ttlist_ec_controller.TriggerEvent("pfc_Print")
	dw_pubs1_ec_report.Visible = False
	
ELSEIF dw_ttlist_ec_prod.Visible = True THEN
	dw_ttlist_ec_prod.TriggerEvent("pfc_Print")
	
ELSEIF dw_pcs1chk_ace_report.Visible = True THEN
	dw_pcs1chk_ace_report.TriggerEvent("pfc_Print")
	dw_ttlist_ec_controller.Visible = False
	
ELSEIF dw_pcarchive_ace_report.Visible = True THEN
	dw_pcarchive_ace_report.TriggerEvent("pfc_Print")
	dw_pcs1chk_ace_report.Visible = False
	
ELSEIF dw_catalogcheck_sql.Visible = True THEN
	dw_catalogcheck_sql.TriggerEvent("pfc_Print")
	dw_pcarchive_ace_report.Visible = False
	
ELSEIF dw_s1check_report.Visible = True THEN
	dw_s1check_report.TriggerEvent("pfc_Print")
	dw_catalogcheck_sql.Visible = False
	
ELSEIF dw_stage1_annotation.Visible = True THEN
	
	dw_stage1_annotation.TriggerEvent("pfc_Print")
	
	dw_s1check_report.Visible = False
	
ELSEIF dw_pcnofishe_ace_report.Visible = True THEN
	dw_pcnofishe_ace_report.TriggerEvent("pfc_Print")
	dw_pcinstr_ace.Visible = False
	
ELSEIF dw_receive_date.Visible = True THEN
	nvo_PowerPrn.of_SetPrinterOrientation(2)
	nvo_PowerPrn.of_SetPaperSource(4)
	nvo_PowerPrn.of_SetPaperSize(20)
	dw_receive_date.TriggerEvent("pfc_Print")
	dw_stage1_annotation.Visible = False
	
ELSEIF dw_qa_receive_date_bkseq.Visible = True THEN
	nvo_PowerPrn.of_SetPrinterOrientation(2)
	nvo_PowerPrn.of_SetPaperSource(4)
	nvo_PowerPrn.of_SetPaperSize(20)
	dw_qa_receive_date_bkseq.TriggerEvent("pfc_Print")
	dw_stage1_annotation.Visible = False
	
ELSEIF dw_qa_receive_date_bkmed.Visible = True THEN
	nvo_PowerPrn.of_SetPrinterOrientation(2)
	nvo_PowerPrn.of_SetPaperSource(4)
	nvo_PowerPrn.of_SetPaperSize(20)
	dw_qa_receive_date_bkmed.TriggerEvent("pfc_Print")
	dw_stage1_annotation.Visible = False
	
ELSEIF dw_pcinstr_ace.Visible = True THEN
	dw_pcinstr_ace.TriggerEvent("pfc_Print")
	dw_receive_date.Visible = False	
ELSEIF dw_unable_process_inv.Visible = True THEN
	dw_unable_process_inv.AcceptText()
	dw_unable_process_inv.TriggerEvent("pfc_Print")
	dw_pcinstr_ace.Visible = False		
END IF

// Reset the default printer setting
f_pics_set_def_prn_setting()

end event

type cb_print_to_file from u_cb within w_pcs_reports
integer x = 2354
integer y = 1180
integer width = 197
integer height = 80
integer taborder = 0
boolean bringtotop = true
string text = "&Save"
end type

event clicked;call super::clicked;	IF dw_cc_par_report_conno.Visible = True THEN   
		// (for save dw_cc_par_report_conno)
		dw_cc_par_report_conno.SaveAs()
		
	ELSEIF dw_cc_par_report_conno_cntr.Visible = True THEN   
		dw_cc_par_report_conno_cntr.SaveAs()
		
	ELSEIF dw_ttlist_ec_prod.Visible = True THEN   
		dw_ttlist_ec_prod.SaveAs()
		
	Elseif dw_pctipttl_ec_report.Visible = True THEN 
		dw_pctipttl_ec_report.SaveAs()
		
	Elseif dw_pctipauth_ace_report.Visible = True THEN 
		dw_pctipauth_ace_report.SaveAs()
		
	Elseif dw_pcparts_txt_report.Visible = True THEN	 
		// (for save dw_pcparts_txt_report)
		parent.wf_pcs_save_multipartbooks()
		
	Elseif dw_pcdeviaten_ace_report.Visible = True THEN	 
		// (for save dw_pcdeviaten_ace_report)
		dw_pcdeviaten_ace_report.SaveAs()
		
	Elseif dw_pcdeviaten_ace_report_active.Visible = True THEN	 
		// (for save dw_pcdeviaten_ace_report)
		dw_pcdeviaten_ace_report_active.SaveAs()
		
	Elseif dw_pcdeviaten_sum_only.Visible = True THEN	 
		// (for save dw_pcdeviaten_ace_report)
		dw_pcdeviaten_sum_only.SaveAs()
		
	Elseif dw_pubs1_ec_report.Visible = True THEN		
		// (for save dw_pubs1_ec_report)
		dw_pubs1_ec_report.SaveAs()
		
	Elseif dw_pcs1chk_ace_report.Visible = True THEN			
		// (for save dw_pcs1chk_ace_report)
		dw_pcs1chk_ace_report.SaveAs()
		
	Elseif dw_ttlist_ec_controller.Visible = True THEN		
		// (for save dw_ttlist_ec_controller)
		dw_ttlist_ec_controller.SaveAs()
		
	Elseif dw_pcarchive_ace_report.Visible = True THEN		
		// (for save dw_pcarchive_ace_report)
		dw_pcarchive_ace_report.SaveAs()
		
	Elseif dw_s1check_report.Visible = True THEN	
		// (for save dw_s1check_report)
		dw_s1check_report.SaveAs()
		
	Elseif dw_catalogcheck_sql.Visible = True THEN 	
		// (for save dw_catalogcheck_sql)
		dw_catalogcheck_sql.SaveAs()
		
	Elseif dw_stage1_annotation.Visible = True THEN 	
		dw_stage1_annotation.SaveAs()
		
	Elseif dw_pcinstr_ace.Visible = True THEN 	
		dw_pcinstr_ace.SaveAs()	
		
	Elseif dw_receive_date.Visible = True THEN		
		// (for save dw_receive_date)
		dw_receive_date.SaveAs()
		
	Elseif dw_qa_receive_date_bkseq.Visible = True THEN		
		// (for save dw_qa_receive_date_bkseq)
		dw_qa_receive_date_bkseq.SaveAs()
		
	Elseif dw_qa_receive_date_bkmed.Visible = True THEN		
		// (for save dw_qa_receive_date_bkmed)
		dw_qa_receive_date_bkmed.SaveAs()
		
	Elseif dw_pcnofishe_ace_report.Visible = True THEN 	
		parent.wf_pcs_save_nofishe()
END IF
end event

type cb_cancel from u_cb within w_pcs_reports
integer x = 2565
integer y = 1180
integer width = 197
integer height = 80
integer taborder = 0
boolean bringtotop = true
string text = "&Cancel"
end type

event clicked;call super::clicked;//IF NOT SQLServerOracleTrans.DBHandle() =0 THEN
//	IF SQLServerOracleTrans.of_disconnect() < 0 THEN
//		MessageBox("Error","Oracle Database Disconnect Error.",StopSign!)
//	END IF
//END IF
f_pics_set_def_prn_setting()
ib_disableclosequery = TRUE
parent.Event pfc_close()
m_pics_main.m_menu.PopMenu(300, 0)
end event

type rte_1 from u_rte within w_pcs_reports
boolean visible = false
integer y = 1444
integer width = 41
integer height = 36
integer taborder = 0
long init_backcolor = 1090519039
end type

on rte_1.create
call super::create
BackColor=1090519039
end on

type st_rows from statictext within w_pcs_reports
boolean visible = false
integer x = 9
integer y = 1180
integer width = 357
integer height = 68
boolean bringtotop = true
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long backcolor = 78164112
boolean enabled = false
string text = "Rows Counted:"
boolean focusrectangle = false
end type

type sle_rows from singlelineedit within w_pcs_reports
boolean visible = false
integer x = 366
integer y = 1180
integer width = 165
integer height = 72
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long backcolor = 16777215
boolean autohscroll = false
borderstyle borderstyle = stylelowered!
end type

type dw_qa_cntr from u_dw within w_pcs_reports
boolean visible = false
integer y = 1240
integer width = 55
integer height = 60
integer taborder = 0
string dataobject = "dddw_qa_cntr"
boolean vscrollbar = false
boolean livescroll = false
boolean ib_isupdateable = false
boolean ib_rmbmenu = false
boolean ib_rmbfocuschange = false
string is_updatesallowed = ""
end type

event constructor;call super::constructor;dw_qa_cntr.of_SetTransObject(sqlservertrans) 
dw_qa_cntr.of_setupdateable(FALSE)

end event

type cb_clear from commandbutton within w_pcs_reports
integer x = 1925
integer y = 1180
integer width = 197
integer height = 80
boolean bringtotop = true
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "C&lear"
end type

event clicked;string window_title
ib_cancel=FALSE
window_title = w_pcs_reports.Title
close(w_pcs_reports)
CHOOSE CASE window_title
	CASE "Production Authorization Record"
		m_pics_main.m_menu.m_report.m_pcsreports.m_productionauthorizationrecord.TriggerEvent(Clicked!)
	CASE "Titles in Process Report"		
		m_pics_main.m_menu.m_report.m_pcsreports.m_titlesinprocess.TriggerEvent(Clicked!)
	CASE "Titles in Process - by Author"
		m_pics_main.m_menu.m_report.m_pcsreports.m_titlesinprocessbyauthor.TriggerEvent(Clicked!)
	CASE "Multipart books nearing shipment"
		m_pics_main.m_menu.m_report.m_pcsreports.m_multipartbooksnearingshipment.TriggerEvent(Clicked!)
	CASE "Average Deviation"
		m_pics_main.m_menu.m_report.m_pcsreports.m_averagedeviation.TriggerEvent(Clicked!)
	CASE "Annotation for TBT/BBR"
		m_pics_main.m_menu.m_report.m_pcsreports.m_annotationfortbtbbr.TriggerEvent(Clicked!)
	CASE "Exception report for titles passing final review"
		m_pics_main.m_menu.m_report.m_pcsreports.m_exceptionreportfortitlespassingf.TriggerEvent(Clicked!)
	CASE "Titles Assigned to Producer"
		m_pics_main.m_menu.m_report.m_pcsreports.m_titlesassignedtoproducer.TriggerEvent(Clicked!)
	CASE "Titles Assigned to Controller"
		m_pics_main.m_menu.m_report.m_pcsreports.m_titlesassignedtocontroller.TriggerEvent(Clicked!)
	CASE "Titles Shipped but Yet in Microfiche"
		m_pics_main.m_menu.m_report.m_pcsreports.m_titlesshippedbutnotyetinmicrof.TriggerEvent(Clicked!)
	CASE "Books archived"
		m_pics_main.m_menu.m_report.m_pcsreports.m_booksarchived.TriggerEvent(Clicked!)
	CASE "Stage 1 Check Report"
		m_pics_main.m_menu.m_report.m_pcsreports.m_stage1checkreport.TriggerEvent(Clicked!)
	CASE "Stage 1 Annotation Report"
		m_pics_main.m_menu.m_report.m_pcsreports.m_stage1annotation.TriggerEvent(Clicked!)
	CASE "Catalog Check Report"
		m_pics_main.m_menu.m_report.m_pcsreports.m_catalogcheckreport.TriggerEvent(Clicked!)
	CASE "Special Instruction Report"
		m_pics_main.m_menu.m_report.m_pcsreports.m_specialinstruction.TriggerEvent(Clicked!)
	CASE "Quality Assurance Report"
//		m_pics_main.m_menu.m_report.m_qareports.m_qareceivedcards.TriggerEvent(Clicked!)
	CASE ELSE
END CHOOSE
end event

type cb_dbcancel from commandbutton within w_pcs_reports
integer x = 1600
integer y = 1180
integer width = 306
integer height = 80
boolean bringtotop = true
integer textsize = -8
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "&DB Cancel"
end type

event clicked;ib_cancel = TRUE
IF dw_cc_par_report_conno.Visible = True THEN
	dw_cc_par_report_conno.DBCancel()
ELSEIF dw_cc_par_report_conno_cntr.Visible = True THEN
	dw_cc_par_report_conno_cntr.DBCancel()
ELSEIF dw_cc_par_report_bkno_med.Visible = True THEN
	dw_cc_par_report_bkno_med.DBCancel()
ELSEIF dw_pctipttl_ec_report.Visible = True THEN
	dw_pctipttl_ec_report.DBCancel()
ELSEIF dw_pctipauth_ace_report.Visible = True THEN
	dw_pctipauth_ace_report.DBCancel()
ELSEIF dw_pctipttl_ec_report.Visible = True THEN
	dw_pctipttl_ec_report.DBCancel()
ELSEIF dw_pcparts_txt_report.Visible = True THEN
	dw_pcparts_txt_report.DBCancel()
ELSEIF dw_pcdeviaten_ace_report.Visible = True THEN
	dw_pcdeviaten_ace_report.DBCancel()
ELSEIF dw_pcdeviaten_ace_report_active.Visible = True THEN
	dw_pcdeviaten_ace_report_active.DBCancel()
ELSEIF dw_pubs1_ec_report.Visible = True THEN
	dw_pubs1_ec_report.DBCancel()
ELSEIF dw_ttlist_ec_controller.Visible = True THEN
	dw_ttlist_ec_controller.DBCancel()
ELSEIF dw_ttlist_ec_prod.Visible = True THEN
	dw_ttlist_ec_prod.DBCancel()	
ELSEIF dw_pcs1chk_ace_report.Visible = True THEN
	dw_pcs1chk_ace_report.DBCancel()
ELSEIF dw_pcarchive_ace_report.Visible = True THEN
	dw_pcarchive_ace_report.DBCancel()
ELSEIF dw_catalogcheck_sql.Visible = True THEN
	dw_catalogcheck_sql.DBCancel()
ELSEIF dw_s1check_report.Visible = True THEN
	dw_s1check_report.DBCancel()
ELSEIF dw_stage1_annotation.Visible = True THEN
	dw_stage1_annotation.DBCancel()
ELSEIF dw_pcnofishe_ace_report.Visible = True THEN
	dw_pcnofishe_ace_report.DBCancel()
ELSEIF dw_receive_date.Visible = True THEN
	dw_receive_date.DBCancel()
ELSEIF dw_qa_receive_date_bkseq.Visible = True THEN
	dw_qa_receive_date_bkseq.DBCancel()
ELSEIF dw_qa_receive_date_bkmed.Visible = True THEN
	dw_qa_receive_date_bkmed.DBCancel()
ELSEIF dw_pcinstr_ace.Visible = True THEN
	dw_pcinstr_ace.DBCancel()
END IF
end event

type dw_pubs1_ec_report from u_dw within w_pcs_reports
boolean visible = false
integer x = 14
integer width = 2743
integer height = 1164
integer taborder = 140
string dataobject = "d_pubs1_ec_report"
boolean hscrollbar = true
boolean livescroll = false
boolean ib_isupdateable = false
boolean ib_rmbmenu = false
boolean ib_rmbfocuschange = false
string is_updatesallowed = ""
end type

event constructor;call super::constructor;this.of_SetFilter(TRUE)
this.of_SetFind(TRUE)
this.inv_filter.of_SetStyle(1)
this.inv_filter.of_SetColumnNameSource(2)
this.of_SetSort(TRUE)
this.inv_sort.of_SetStyle(1)
this.inv_sort.of_SetColumnNameSource(2)
This.of_SetTransObject(sqlservertrans)
ib_rmbmenu = False
//This.of_SetRowManager(True)
//This.of_SetPrintPreview(True)
end event

event getfocus;call super::getfocus;m_pics_main.m_edit.m_deleterow.Enabled =	FALSE
m_pics_main.m_edit.m_addrow.Enabled =	FALSE
m_pics_main.m_edit.m_cut.Enabled =	FALSE
end event

event pfc_retrieve;call super::pfc_retrieve;THIS.Reset()
RETURN THIS.Retrieve(dw_pubs1_ec_report)
end event

event retrieveend;call super::retrieveend;IF rowcount = 0 THEN
	Close(w_pics_retrieve_msg_box)
	MessageBox("ERROR","No data found.")	
Else
	Close(w_pics_retrieve_msg_box)
	Close(w_pcs_gets_date)
End IF
end event

event retrievestart;call super::retrievestart;OpenWithParm(w_pics_retrieve_msg_box,"Retrieving Annotation for TBT/BBR, Please Wait...")
end event

event retrieverow;IF ib_cancel = TRUE THEN
	RETURN 1
END IF

end event

type dw_pcarchive_ace_report from u_dw within w_pcs_reports
boolean visible = false
integer x = 14
integer width = 2743
integer height = 1164
integer taborder = 190
string dataobject = "d_pcarchive_ace_report"
boolean hscrollbar = true
boolean livescroll = false
boolean ib_isupdateable = false
boolean ib_rmbmenu = false
boolean ib_rmbfocuschange = false
string is_updatesallowed = ""
end type

event constructor;call super::constructor;this.of_SetFilter(TRUE)
this.of_SetFind(TRUE)
this.inv_filter.of_SetStyle(1)
this.inv_filter.of_SetColumnNameSource(2)
this.of_SetSort(TRUE)
this.inv_sort.of_SetStyle(1)
this.inv_sort.of_SetColumnNameSource(2)
This.of_SetTransObject(sqlservertrans)
ib_rmbmenu = False
//This.of_SetRowManager(True)
//This.of_SetPrintPreview(True)
end event

event getfocus;call super::getfocus;m_pics_main.m_edit.m_deleterow.Enabled =	FALSE
m_pics_main.m_edit.m_addrow.Enabled =	FALSE
m_pics_main.m_edit.m_cut.Enabled =	FALSE
end event

event pfc_retrieve;call super::pfc_retrieve;THIS.Reset()
RETURN THIS.Retrieve(dw_pcarchive_ace_report)
end event

event retrieveend;call super::retrieveend;IF rowcount = 0 THEN
	Close(w_pics_retrieve_msg_box)
	MessageBox("ERROR","No data found.")
ELSE
	Close(w_pics_retrieve_msg_box)	
End IF

end event

event retrievestart;call super::retrievestart;OpenWithParm(w_pics_retrieve_msg_box,"Retrieving Archived Books, Please Wait...")
end event

event retrieverow;IF ib_cancel = TRUE THEN
	RETURN 1
END IF

end event

type cb_addcmts from commandbutton within w_pcs_reports
boolean visible = false
integer x = 1225
integer y = 1180
integer width = 361
integer height = 80
integer taborder = 20
integer textsize = -8
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "Add C&omments"
end type

event clicked;open(w_add_cntr_cmts)
end event

type dw_unable_process_inv from u_dw within w_pcs_reports
boolean visible = false
integer x = 14
integer width = 2743
integer height = 1164
integer taborder = 30
string dataobject = "d_unable_process_inv"
boolean hscrollbar = true
boolean livescroll = false
boolean ib_isupdateable = false
boolean ib_rmbmenu = false
boolean ib_rmbfocuschange = false
string is_updatesallowed = ""
end type

event constructor;call super::constructor;This.of_SetTransObject(sqlservertrans)
ib_rmbmenu = False

end event

event getfocus;call super::getfocus;m_pics_main.m_edit.m_deleterow.Enabled =	FALSE
m_pics_main.m_edit.m_addrow.Enabled =	FALSE
m_pics_main.m_edit.m_cut.Enabled =	FALSE
end event

event retrieveend;call super::retrieveend;IF rowcount = 0 THEN
	Close(w_pics_retrieve_msg_box)
	MessageBox("ERROR","No data found.")	
Else
	Close(w_pics_retrieve_msg_box)
	Close(w_pcs_gets_date_cntr)
End IF
end event

type dw_ttlist_ec_prod from u_dw within w_pcs_reports
boolean visible = false
integer x = 14
integer width = 2743
integer height = 1164
integer taborder = 70
string dataobject = "d_ttlist_ec_prod"
boolean hscrollbar = true
boolean livescroll = false
boolean ib_isupdateable = false
boolean ib_rmbmenu = false
boolean ib_rmbfocuschange = false
string is_updatesallowed = ""
end type

event constructor;call super::constructor;This.of_SetTransObject(sqlservertrans)
ib_rmbmenu = False

end event

event getfocus;call super::getfocus;m_pics_main.m_edit.m_deleterow.Enabled =	FALSE
m_pics_main.m_edit.m_addrow.Enabled =	FALSE
m_pics_main.m_edit.m_cut.Enabled =	FALSE
end event

event retrieveend;call super::retrieveend;IF rowcount = 0 THEN
	Close(w_pics_retrieve_msg_box)
	MessageBox("ERROR","No data found.")	
Else
	Close(w_pics_retrieve_msg_box)
	Close(w_pcs_gets_date_cntr)
End IF
end event

type dw_stage1_annotation from u_dw within w_pcs_reports
boolean visible = false
integer x = 14
integer width = 2743
integer height = 1164
integer taborder = 60
string dataobject = "d_stage1_annotation"
boolean hscrollbar = true
boolean livescroll = false
boolean ib_isupdateable = false
boolean ib_rmbmenu = false
boolean ib_rmbfocuschange = false
string is_updatesallowed = ""
end type

event constructor;call super::constructor;this.of_SetFilter(TRUE)
this.of_SetFind(TRUE)
this.inv_filter.of_SetStyle(1)
this.inv_filter.of_SetColumnNameSource(2)
this.of_SetSort(TRUE)
this.inv_sort.of_SetStyle(1)
this.inv_sort.of_SetColumnNameSource(2)
This.of_SetTransObject(sqlservertrans)
ib_rmbmenu = False
//This.of_SetRowManager(True)
//This.of_SetPrintPreview(True)
end event

event getfocus;call super::getfocus;m_pics_main.m_edit.m_deleterow.Enabled =	FALSE
m_pics_main.m_edit.m_addrow.Enabled =	FALSE
m_pics_main.m_edit.m_cut.Enabled =	FALSE
end event

event pfc_retrieve;call super::pfc_retrieve;THIS.Reset()
RETURN THIS.Retrieve(dw_s1check_report)
end event

event retrieveend;call super::retrieveend;IF rowcount = 0 THEN
	Close(w_pics_retrieve_msg_box)
	MessageBox("ERROR","No data found.")
	
Else
	Close(w_pics_retrieve_msg_box)
	Close(w_pcs_gets_date_s1)
End IF
end event

event retrievestart;call super::retrievestart;OpenWithParm(w_pics_retrieve_msg_box,"Retrieving Stage 1 Annotation Report, Please Wait...")
end event

event retrieverow;IF ib_cancel = TRUE THEN
	RETURN 1
END IF

end event

type dw_ttlist_ec_controller from u_dw within w_pcs_reports
boolean visible = false
integer x = 14
integer width = 2743
integer height = 1164
integer taborder = 150
string dataobject = "d_ttlist_ec_controller"
boolean hscrollbar = true
boolean livescroll = false
boolean ib_isupdateable = false
boolean ib_rmbmenu = false
boolean ib_rmbfocuschange = false
string is_updatesallowed = ""
end type

event getfocus;call super::getfocus;m_pics_main.m_edit.m_deleterow.Enabled =	FALSE
m_pics_main.m_edit.m_addrow.Enabled =	FALSE
m_pics_main.m_edit.m_cut.Enabled =	FALSE
end event

event pfc_retrieve;call super::pfc_retrieve;THIS.Reset()
RETURN THIS.Retrieve(dw_ttlist_ec_controller)
end event

event retrieveend;call super::retrieveend;IF rowcount = 0 THEN
	Close(w_pics_retrieve_msg_box)
	MessageBox("ERROR","No data found.")
	
Else
	Close(w_pics_retrieve_msg_box)
	Close(w_pcs_gets_date_cntr)
End IF
end event

event retrievestart;call super::retrievestart;OpenWithParm(w_pics_retrieve_msg_box,"Retrieving Title Listing for Controller, Please Wait...")
end event

event constructor;call super::constructor;this.of_SetFilter(TRUE)
this.of_SetFind(TRUE)
this.inv_filter.of_SetStyle(1)
this.inv_filter.of_SetColumnNameSource(2)
this.of_SetSort(TRUE)
this.inv_sort.of_SetStyle(1)
this.inv_sort.of_SetColumnNameSource(2)
This.of_SetTransObject(sqlservertrans)
ib_rmbmenu = False
//This.of_SetRowManager(True)
//This.of_SetPrintPreview(True)
end event

event retrieverow;IF ib_cancel = TRUE THEN
	RETURN 1
END IF

end event

type dw_pcparts_txt_report from u_dw within w_pcs_reports
boolean visible = false
integer x = 14
integer width = 2743
integer height = 1164
integer taborder = 120
string dataobject = "d_pcparts_txt_report"
boolean hscrollbar = true
boolean livescroll = false
boolean ib_isupdateable = false
boolean ib_rmbmenu = false
boolean ib_rmbfocuschange = false
string is_updatesallowed = ""
end type

event constructor;call super::constructor;this.of_SetFilter(TRUE)
this.of_SetFind(TRUE)
this.inv_filter.of_SetStyle(1)
this.inv_filter.of_SetColumnNameSource(2)
this.of_SetSort(TRUE)
this.inv_sort.of_SetStyle(1)
this.inv_sort.of_SetColumnNameSource(2)
This.of_SetTransObject(sqlservertrans)
ib_rmbmenu = False
//This.of_SetRowManager(True)
//This.of_SetPrintPreview(True)
end event

event getfocus;call super::getfocus;m_pics_main.m_edit.m_deleterow.Enabled =	FALSE
m_pics_main.m_edit.m_addrow.Enabled =	FALSE
m_pics_main.m_edit.m_cut.Enabled =	FALSE
end event

event pfc_retrieve;call super::pfc_retrieve;THIS.Reset()
RETURN THIS.Retrieve(dw_pcparts_txt_report)
end event

event retrieveend;call super::retrieveend;IF rowcount = 0 THEN
	Close(w_pics_retrieve_msg_box)
	MessageBox("ERROR","No data found.")
	
	Return
END IF
	Close(w_pics_retrieve_msg_box)
	
end event

event retrievestart;call super::retrievestart;OpenWithParm(w_pics_retrieve_msg_box,"Retrieving Multipart Books Nearing Shipment, Please Wait...")
end event

event retrieverow;IF ib_cancel = TRUE THEN
	RETURN 1
END IF

end event

type dw_cc_par_report_bkno_med from u_dw within w_pcs_reports
boolean visible = false
integer x = 14
integer width = 2743
integer height = 1164
integer taborder = 200
string dataobject = "d_cc_par_report_bkno_med"
boolean hscrollbar = true
boolean livescroll = false
boolean ib_isupdateable = false
boolean ib_rmbmenu = false
boolean ib_rmbfocuschange = false
string is_updatesallowed = ""
end type

event constructor;call super::constructor;This.of_SetTransObject(sqlservertrans)
ib_rmbmenu = False
//This.of_SetRowManager(True)
//This.of_SetPrintPreview(True)
end event

event getfocus;call super::getfocus;m_pics_main.m_edit.m_deleterow.Enabled =	FALSE
m_pics_main.m_edit.m_addrow.Enabled =	FALSE
m_pics_main.m_edit.m_cut.Enabled =	FALSE
end event

event pfc_retrieve;call super::pfc_retrieve;THIS.Reset()
RETURN THIS.Retrieve(dw_cc_par_report_bkno_med)
end event

event retrieveend;call super::retrieveend;IF rowcount = 0 THEN
	MessageBox("Error","Invalid data.")
	Close(w_pics_retrieve_msg_box)
	Return
END IF
	Close(w_pics_retrieve_msg_box)
	
end event

event retrievestart;call super::retrievestart;Open(w_pics_retrieve_msg_box)
end event

type dw_qa_receive_date_bkseq from u_pics_dw within w_pcs_reports
boolean visible = false
integer x = 14
integer width = 2743
integer height = 1164
integer taborder = 30
string dataobject = "d_qa_receive_date_bkseq"
boolean hscrollbar = true
boolean livescroll = false
boolean ib_isupdateable = false
boolean ib_rmbmenu = false
boolean ib_rmbfocuschange = false
string is_updatesallowed = ""
end type

event constructor;call super::constructor;this.of_SetFilter(TRUE)
this.of_SetFind(TRUE)
this.inv_filter.of_SetStyle(1)
this.inv_filter.of_SetColumnNameSource(2)
this.of_SetSort(TRUE)
this.inv_sort.of_SetStyle(1)
this.inv_sort.of_SetColumnNameSource(2)
This.of_SetTransObject(sqlservertrans)
ib_rmbmenu = False
//This.of_SetRowManager(True)
//This.of_SetPrintPreview(True)
end event

event getfocus;call super::getfocus;m_pics_main.m_edit.m_deleterow.Enabled =	FALSE
m_pics_main.m_edit.m_addrow.Enabled =	FALSE
m_pics_main.m_edit.m_cut.Enabled =	FALSE
end event

event retrievestart;call super::retrievestart;Open(w_pics_retrieve_msg_box)
end event

event retrieveend;call super::retrieveend;IF rowcount = 0 THEN
	Close(w_pics_retrieve_msg_box)
	MessageBox("ERROR","No data found. ~rMake sure this book has not been approved or rejected on any of QAS stages.")
	
Else
	Close(w_pics_retrieve_msg_box)
	Close(w_pcs_gets_qarec_date)
End IF
end event

event retrieverow;IF ib_cancel = TRUE THEN
	RETURN 1
END IF

end event

type dw_receive_date from u_dw within w_pcs_reports
boolean visible = false
integer x = 14
integer width = 2743
integer height = 1164
integer taborder = 90
string dataobject = "d_qa_receive_date"
boolean hscrollbar = true
boolean livescroll = false
boolean ib_isupdateable = false
boolean ib_rmbmenu = false
boolean ib_rmbfocuschange = false
string is_updatesallowed = ""
end type

event retrieveend;call super::retrieveend;IF rowcount = 0 THEN
	Close(w_pics_retrieve_msg_box)
	MessageBox("ERROR","No data found. ~rMake sure this book has not been approved or rejected on any of QAS stages.")
	
Else
	Close(w_pics_retrieve_msg_box)
	Close(w_pcs_gets_qarec_date)
End IF
end event

event retrievestart;call super::retrievestart;Open(w_pics_retrieve_msg_box)
end event

event constructor;call super::constructor;this.of_SetFilter(TRUE)
this.of_SetFind(TRUE)
this.inv_filter.of_SetStyle(1)
this.inv_filter.of_SetColumnNameSource(2)
this.of_SetSort(TRUE)
this.inv_sort.of_SetStyle(1)
this.inv_sort.of_SetColumnNameSource(2)
This.of_SetTransObject(sqlservertrans)
ib_rmbmenu = False
//This.of_SetRowManager(True)
//This.of_SetPrintPreview(True)
end event

event getfocus;call super::getfocus;m_pics_main.m_edit.m_deleterow.Enabled =	FALSE
m_pics_main.m_edit.m_addrow.Enabled =	FALSE
m_pics_main.m_edit.m_cut.Enabled =	FALSE
end event

event pfc_retrieve;call super::pfc_retrieve;THIS.Reset()
RETURN THIS.Retrieve(dw_receive_date)
end event

event retrieverow;IF ib_cancel = TRUE THEN
	RETURN 1
END IF

end event

type dw_qa_receive_date_bkmed from u_pics_dw within w_pcs_reports
boolean visible = false
integer x = 14
integer width = 2743
integer height = 1164
integer taborder = 20
string dataobject = "d_qa_receive_date_bkmed"
boolean hscrollbar = true
boolean livescroll = false
boolean ib_isupdateable = false
boolean ib_rmbmenu = false
boolean ib_rmbfocuschange = false
string is_updatesallowed = ""
end type

event constructor;call super::constructor;this.of_SetFilter(TRUE)
this.of_SetFind(TRUE)
this.inv_filter.of_SetStyle(1)
this.inv_filter.of_SetColumnNameSource(2)
this.of_SetSort(TRUE)
this.inv_sort.of_SetStyle(1)
this.inv_sort.of_SetColumnNameSource(2)
This.of_SetTransObject(sqlservertrans)
ib_rmbmenu = False
//This.of_SetRowManager(True)
//This.of_SetPrintPreview(True)
end event

event getfocus;call super::getfocus;m_pics_main.m_edit.m_deleterow.Enabled =	FALSE
m_pics_main.m_edit.m_addrow.Enabled =	FALSE
m_pics_main.m_edit.m_cut.Enabled =	FALSE
end event

event retrievestart;call super::retrievestart;Open(w_pics_retrieve_msg_box)
end event

event retrieveend;call super::retrieveend;IF rowcount = 0 THEN
	Close(w_pics_retrieve_msg_box)
	MessageBox("ERROR","No data found. ~rMake sure this book has not been approved or rejected on any of QAS stages.")
	
Else
	Close(w_pics_retrieve_msg_box)
	Close(w_pcs_gets_qarec_date)
End IF
end event

event retrieverow;IF ib_cancel = TRUE THEN
	RETURN 1
END IF

end event

type dw_s1check_report from u_dw within w_pcs_reports
boolean visible = false
integer x = 14
integer width = 2743
integer height = 1164
integer taborder = 170
string dataobject = "d_s1check_report"
boolean hscrollbar = true
boolean livescroll = false
boolean ib_isupdateable = false
boolean ib_rmbmenu = false
boolean ib_rmbfocuschange = false
string is_updatesallowed = ""
end type

event constructor;call super::constructor;this.of_SetFilter(TRUE)
this.of_SetFind(TRUE)
this.inv_filter.of_SetStyle(1)
this.inv_filter.of_SetColumnNameSource(2)
this.of_SetSort(TRUE)
this.inv_sort.of_SetStyle(1)
this.inv_sort.of_SetColumnNameSource(2)
This.of_SetTransObject(sqlservertrans)
ib_rmbmenu = False
//This.of_SetRowManager(True)
//This.of_SetPrintPreview(True)
end event

event getfocus;call super::getfocus;m_pics_main.m_edit.m_deleterow.Enabled =	FALSE
m_pics_main.m_edit.m_addrow.Enabled =	FALSE
m_pics_main.m_edit.m_cut.Enabled =	FALSE
end event

event pfc_retrieve;call super::pfc_retrieve;THIS.Reset()
RETURN THIS.Retrieve(dw_s1check_report)
end event

event retrieveend;call super::retrieveend;IF rowcount = 0 THEN
	Close(w_pics_retrieve_msg_box)
	MessageBox("ERROR","No data found.")
	
Else
	Close(w_pics_retrieve_msg_box)
	Close(w_pcs_gets_date_s1)
End IF
end event

event retrievestart;call super::retrievestart;OpenWithParm(w_pics_retrieve_msg_box,"Retrieving Stage 1 Check Report, Please Wait...")
end event

event retrieverow;IF ib_cancel = TRUE THEN
	RETURN 1
END IF

end event

type dw_pcnofishe_ace_report from u_dw within w_pcs_reports
boolean visible = false
integer x = 14
integer width = 2743
integer height = 1164
integer taborder = 40
string dataobject = "d_pcnofishe_ace_report"
boolean hscrollbar = true
boolean livescroll = false
boolean ib_isupdateable = false
boolean ib_rmbmenu = false
boolean ib_rmbfocuschange = false
string is_updatesallowed = ""
end type

event retrieveend;call super::retrieveend;IF rowcount = 0 THEN
	Close(w_pics_retrieve_msg_box)
	MessageBox("ERROR","No data found.")
	
Else
	Close(w_pics_retrieve_msg_box)
	Close(w_pcs_gets_date_s1)
End IF
end event

event constructor;call super::constructor;this.of_SetFilter(TRUE)
this.of_SetFind(TRUE)
this.inv_filter.of_SetStyle(1)
this.inv_filter.of_SetColumnNameSource(2)
this.of_SetSort(TRUE)
this.inv_sort.of_SetStyle(1)
this.inv_sort.of_SetColumnNameSource(2)
This.of_SetTransObject(sqlservertrans)
ib_rmbmenu = False
//This.of_SetRowManager(True)
//This.of_SetPrintPreview(True)
end event

event getfocus;call super::getfocus;m_pics_main.m_edit.m_deleterow.Enabled =	FALSE
m_pics_main.m_edit.m_addrow.Enabled =	FALSE
m_pics_main.m_edit.m_cut.Enabled =	FALSE
end event

event pfc_retrieve;call super::pfc_retrieve;THIS.Reset()
RETURN THIS.Retrieve(dw_pcnofishe_ace_report)
end event

event retrievestart;call super::retrievestart;OpenWithParm(w_pics_retrieve_msg_box,"Retrieving Titles Shipped but Not yet on blnd, Please Wait...")
end event

event retrieverow;IF ib_cancel = TRUE THEN
	RETURN 1
END IF

end event

type dw_catalogcheck_sql from u_dw within w_pcs_reports
boolean visible = false
integer x = 14
integer width = 2743
integer height = 1164
integer taborder = 180
string dataobject = "d_catalogcheck_sql"
boolean hscrollbar = true
boolean livescroll = false
boolean ib_isupdateable = false
boolean ib_rmbmenu = false
boolean ib_rmbfocuschange = false
string is_updatesallowed = ""
end type

event constructor;call super::constructor;this.of_SetFilter(TRUE)
this.of_SetFind(TRUE)
this.inv_filter.of_SetStyle(1)
this.inv_filter.of_SetColumnNameSource(2)
this.of_SetSort(TRUE)
this.inv_sort.of_SetStyle(1)
this.inv_sort.of_SetColumnNameSource(2)
This.of_SetTransObject(sqlservertrans)
ib_rmbmenu = False
//This.of_SetRowManager(True)
//This.of_SetPrintPreview(True)
end event

event getfocus;call super::getfocus;m_pics_main.m_edit.m_deleterow.Enabled =	FALSE
m_pics_main.m_edit.m_addrow.Enabled =	FALSE
m_pics_main.m_edit.m_cut.Enabled =	FALSE
end event

event retrieveend;call super::retrieveend;IF rowcount = 0 THEN
	Close(w_pics_retrieve_msg_box)
	MessageBox("ERROR","No data found.")
	
Else
	Close(w_pics_retrieve_msg_box)
	Close(w_pcs_gets_date_s1)
End IF
end event

event retrievestart;call super::retrievestart;OpenWithParm(w_pics_retrieve_msg_box,"Retrieving Catalog Check Report, Please Wait...")
end event

event pfc_retrieve;call super::pfc_retrieve;THIS.Reset()
RETURN THIS.Retrieve(dw_catalogcheck_sql)
end event

event retrieverow;IF ib_cancel = TRUE THEN
	RETURN 1
END IF

end event

type dw_pcdeviaten_sum_only from u_dw within w_pcs_reports
boolean visible = false
integer x = 18
integer y = 8
integer width = 2743
integer height = 1164
integer taborder = 10
string dataobject = "d_deviate_sum_sp_no_detail"
boolean hscrollbar = true
boolean livescroll = false
boolean ib_isupdateable = false
boolean ib_rmbmenu = false
boolean ib_rmbfocuschange = false
string is_updatesallowed = ""
end type

event constructor;call super::constructor;this.of_SetFilter(TRUE)
this.of_SetFind(TRUE)
this.inv_filter.of_SetStyle(1)
this.inv_filter.of_SetColumnNameSource(2)
this.of_SetSort(TRUE)
this.inv_sort.of_SetStyle(1)
this.inv_sort.of_SetColumnNameSource(2)
This.of_SetTransObject(sqlservertrans)
ib_rmbmenu = False

end event

event getfocus;call super::getfocus;m_pics_main.m_edit.m_deleterow.Enabled =	FALSE
m_pics_main.m_edit.m_addrow.Enabled =	FALSE
m_pics_main.m_edit.m_cut.Enabled =	FALSE
end event

event retrieveend;call super::retrieveend;Close(w_pics_retrieve_msg_box)



end event

event retrieverow;IF ib_cancel = TRUE THEN
	RETURN 1
END IF

end event

event retrievestart;call super::retrievestart;OpenWithParm(w_pics_retrieve_msg_box,"Retrieving Average Deviation, Please Wait...")
end event

event pfc_retrieve;call super::pfc_retrieve;THIS.Reset()
RETURN THIS.Retrieve(dw_pcdeviaten_sum_only)
end event

type dw_pcdeviaten_ace_report_active from u_pics_dw within w_pcs_reports
boolean visible = false
integer x = 14
integer y = 4
integer width = 2747
integer height = 1172
integer taborder = 20
string dataobject = "d_pcdeviaten_ace_report_active"
boolean hscrollbar = true
boolean livescroll = false
end type

event constructor;call super::constructor;this.of_SetFilter(TRUE)
this.of_SetFind(TRUE)
this.inv_filter.of_SetStyle(1)
this.inv_filter.of_SetColumnNameSource(2)
this.of_SetSort(TRUE)
this.inv_sort.of_SetStyle(1)
this.inv_sort.of_SetColumnNameSource(2)
This.of_SetTransObject(sqlservertrans)
ib_rmbmenu = False

end event

event getfocus;call super::getfocus;m_pics_main.m_edit.m_deleterow.Enabled =	FALSE
m_pics_main.m_edit.m_addrow.Enabled =	FALSE
m_pics_main.m_edit.m_cut.Enabled =	FALSE
end event

event pfc_retrieve;call super::pfc_retrieve;THIS.Reset()
RETURN THIS.Retrieve(dw_pcdeviaten_ace_report)
end event

event retrieveend;call super::retrieveend;Close(w_pics_retrieve_msg_box)
Close(w_pcs_gets_dates_cntr)


end event

event retrieverow;call super::retrieverow;IF ib_cancel = TRUE THEN
	RETURN 1
END IF

end event

event retrievestart;call super::retrievestart;OpenWithParm(w_pics_retrieve_msg_box,"Retrieving Average Deviation, Please Wait...")
end event

type dw_pcdeviaten_ace_report from u_dw within w_pcs_reports
boolean visible = false
integer x = 14
integer width = 2743
integer height = 1164
integer taborder = 130
string dataobject = "d_pcdeviaten_ace_report"
boolean hscrollbar = true
boolean livescroll = false
boolean ib_isupdateable = false
boolean ib_rmbmenu = false
boolean ib_rmbfocuschange = false
string is_updatesallowed = ""
end type

event getfocus;call super::getfocus;m_pics_main.m_edit.m_deleterow.Enabled =	FALSE
m_pics_main.m_edit.m_addrow.Enabled =	FALSE
m_pics_main.m_edit.m_cut.Enabled =	FALSE
end event

event pfc_retrieve;call super::pfc_retrieve;THIS.Reset()
RETURN THIS.Retrieve(dw_pcdeviaten_ace_report)
end event

event retrieveend;call super::retrieveend;Close(w_pics_retrieve_msg_box)
Close(w_pcs_gets_dates_cntr)


end event

event retrievestart;call super::retrievestart;OpenWithParm(w_pics_retrieve_msg_box,"Retrieving Average Deviation, Please Wait...")
end event

event constructor;call super::constructor;this.of_SetFilter(TRUE)
this.of_SetFind(TRUE)
this.inv_filter.of_SetStyle(1)
this.inv_filter.of_SetColumnNameSource(2)
this.of_SetSort(TRUE)
this.inv_sort.of_SetStyle(1)
this.inv_sort.of_SetColumnNameSource(2)
This.of_SetTransObject(sqlservertrans)
ib_rmbmenu = False

end event

event retrieverow;IF ib_cancel = TRUE THEN
	RETURN 1
END IF

end event

type dw_pctipauth_ace_report from u_dw within w_pcs_reports
boolean visible = false
integer x = 14
integer width = 2743
integer height = 1164
integer taborder = 80
string dataobject = "d_pctipauth_ace_report"
boolean hscrollbar = true
boolean livescroll = false
boolean ib_isupdateable = false
boolean ib_rmbmenu = false
boolean ib_rmbfocuschange = false
string is_updatesallowed = ""
end type

event constructor;call super::constructor;this.of_SetFilter(TRUE)
this.of_SetFind(TRUE)
this.inv_filter.of_SetStyle(1)
this.inv_filter.of_SetColumnNameSource(2)
this.of_SetSort(TRUE)
this.inv_sort.of_SetStyle(1)
this.inv_sort.of_SetColumnNameSource(2)
This.of_SetTransObject(sqlservertrans)
ib_rmbmenu = False
//This.of_SetRowManager(True)
//This.of_SetPrintPreview(True)
end event

event getfocus;call super::getfocus;m_pics_main.m_edit.m_deleterow.Enabled =	FALSE
m_pics_main.m_edit.m_addrow.Enabled =	FALSE
m_pics_main.m_edit.m_cut.Enabled =	FALSE
end event

event pfc_retrieve;call super::pfc_retrieve;THIS.Reset()
RETURN THIS.Retrieve(dw_pctipttl_ec_report)
end event

event retrieveend;call super::retrieveend;IF rowcount = 0 THEN
	Close(w_pics_retrieve_msg_box)
	MessageBox("ERROR","No data found.")
ELSE
	Close(w_pics_retrieve_msg_box)
	Close(w_pcs_gets_dates)
END IF

end event

event retrievestart;call super::retrievestart;OpenWithParm(w_pics_retrieve_msg_box,"Retrieving Titles in Process by Author Report, Please Wait...")
end event

event retrieverow;IF ib_cancel = TRUE THEN
	RETURN 1
END IF

end event

type dw_pctipttl_ec_report from u_dw within w_pcs_reports
boolean visible = false
integer x = 14
integer width = 2743
integer height = 1164
integer taborder = 110
string dataobject = "d_pctipttl_ec_report"
boolean hscrollbar = true
boolean livescroll = false
boolean ib_isupdateable = false
boolean ib_rmbmenu = false
boolean ib_rmbfocuschange = false
string is_updatesallowed = ""
end type

event pfc_retrieve;call super::pfc_retrieve;THIS.Reset()
RETURN THIS.Retrieve(dw_pctipttl_ec_report)
end event

event retrieveend;call super::retrieveend;IF rowcount = 0 THEN
	Close(w_pics_retrieve_msg_box)
	MessageBox("ERROR","No data found.")
ELSE
	Close(w_pics_retrieve_msg_box)
END IF
	
end event

event retrievestart;call super::retrievestart;OpenWithParm(w_pics_retrieve_msg_box,"Retrieving Titles in Process Report, Please Wait...")
end event

event constructor;call super::constructor;this.of_SetFilter(TRUE)
this.of_SetFind(TRUE)
this.inv_filter.of_SetStyle(1)
this.inv_filter.of_SetColumnNameSource(2)
this.of_SetSort(TRUE)
this.inv_sort.of_SetStyle(1)
this.inv_sort.of_SetColumnNameSource(2)
This.of_SetTransObject(sqlservertrans)
ib_rmbmenu = False
//This.of_SetRowManager(True)
//This.of_SetPrintPreview(True)
end event

event retrieverow;IF ib_cancel = TRUE THEN
	RETURN 1
END IF


end event

type dw_pcinstr_ace from u_dw within w_pcs_reports
boolean visible = false
integer x = 14
integer width = 2743
integer height = 1164
integer taborder = 50
string dataobject = "d_pcinstr_ace"
boolean hscrollbar = true
boolean livescroll = false
boolean ib_isupdateable = false
boolean ib_rmbmenu = false
boolean ib_rmbfocuschange = false
string is_updatesallowed = ""
end type

event constructor;call super::constructor;This.of_SetTransObject(sqlservertrans)
ib_rmbmenu = False
//This.of_SetRowManager(True)
//This.of_SetPrintPreview(True)
end event

event getfocus;call super::getfocus;m_pics_main.m_edit.m_deleterow.Enabled =	FALSE
m_pics_main.m_edit.m_addrow.Enabled =	FALSE
m_pics_main.m_edit.m_cut.Enabled =	FALSE
end event

event pfc_retrieve;call super::pfc_retrieve;THIS.Reset()
RETURN THIS.Retrieve(dw_pcinstr_ace)
end event

event retrieveend;call super::retrieveend;IF rowcount = 0 THEN
	Close(w_pics_retrieve_msg_box)
	MessageBox("Invalid Data","Invalid Control Number, please choose another.")
END IF
	Close(w_pics_retrieve_msg_box)
	Close(w_pcs_gets_conno)



	
	
	
	
end event

event retrievestart;call super::retrievestart;OpenWithParm(w_pics_retrieve_msg_box,"Retrieving Special Instuction Report, Please Wait...")
end event

event retrieverow;IF ib_cancel = TRUE THEN
	RETURN 1
END IF

end event

type dw_cc_par_report_conno_cntr from u_dw within w_pcs_reports
boolean visible = false
integer x = 18
integer width = 2738
integer height = 1176
integer taborder = 10
string dataobject = "d_cc_par_report_conno_cntr"
boolean hscrollbar = true
boolean ib_isupdateable = false
boolean ib_rmbmenu = false
boolean ib_rmbfocuschange = false
string is_updatesallowed = ""
end type

event constructor;call super::constructor;this.of_SetFilter(TRUE)
this.of_SetFind(TRUE)
this.inv_filter.of_SetStyle(1)
this.inv_filter.of_SetColumnNameSource(2)
this.of_SetSort(TRUE)
this.inv_sort.of_SetStyle(1)
this.inv_sort.of_SetColumnNameSource(2)
This.of_SetTransObject(sqlservertrans)
ib_rmbmenu = False

end event

event getfocus;call super::getfocus;m_pics_main.m_edit.m_deleterow.Enabled =	FALSE
m_pics_main.m_edit.m_addrow.Enabled =	FALSE
m_pics_main.m_edit.m_cut.Enabled =	FALSE
end event

event pfc_retrieve;call super::pfc_retrieve;THIS.Reset()
RETURN THIS.Retrieve(dw_cc_par_report_conno)
end event

event retrieveend;call super::retrieveend;IF rowcount = 0 THEN
	Close(w_pics_retrieve_msg_box)
	MessageBox("Invalid Data","No data found.")	
Else
	Close(w_pics_retrieve_msg_box)
	Close(w_pcs_gets_conno)
End IF

end event

event retrievestart;call super::retrievestart;OpenWithParm(w_pics_retrieve_msg_box,"Retrieving PAR Report, Please Wait...")
end event

event retrieverow;IF ib_cancel = TRUE THEN
	RETURN 1
END IF

end event

type dw_cc_par_report_conno from u_dw within w_pcs_reports
boolean visible = false
integer x = 14
integer width = 2743
integer height = 1164
integer taborder = 100
string dataobject = "d_cc_par_report_conno1"
boolean hscrollbar = true
boolean ib_isupdateable = false
boolean ib_rmbmenu = false
boolean ib_rmbfocuschange = false
string is_updatesallowed = ""
end type

event constructor;call super::constructor;this.of_SetFilter(TRUE)
this.of_SetFind(TRUE)
this.inv_filter.of_SetStyle(1)
this.inv_filter.of_SetColumnNameSource(2)
this.of_SetSort(TRUE)
this.inv_sort.of_SetStyle(1)
this.inv_sort.of_SetColumnNameSource(2)
This.of_SetTransObject(sqlservertrans)
ib_rmbmenu = False
//This.of_SetRowManager(True)
//This.of_SetPrintPreview(True)
end event

event getfocus;call super::getfocus;m_pics_main.m_edit.m_deleterow.Enabled =	FALSE
m_pics_main.m_edit.m_addrow.Enabled =	FALSE
m_pics_main.m_edit.m_cut.Enabled =	FALSE
end event

event pfc_retrieve;call super::pfc_retrieve;THIS.Reset()
RETURN THIS.Retrieve(dw_cc_par_report_conno)
end event

event retrieveend;call super::retrieveend;IF rowcount = 0 THEN
	Close(w_pics_retrieve_msg_box)
	MessageBox("Invalid Data","No data found.")	
Else
	Close(w_pics_retrieve_msg_box)
	Close(w_pcs_gets_conno)
End IF
	
	
	
	
	
end event

event retrievestart;call super::retrievestart;OpenWithParm(w_pics_retrieve_msg_box,"Retrieving PAR Report, Please Wait...")
end event

event retrieverow;IF ib_cancel = TRUE THEN
	RETURN 1
END IF

end event

type dw_pcs1chk_ace_report from u_dw within w_pcs_reports
boolean visible = false
integer x = 14
integer width = 2743
integer height = 1164
integer taborder = 160
string dataobject = "d_pcs1chk_ace_report"
boolean hscrollbar = true
boolean livescroll = false
boolean ib_isupdateable = false
boolean ib_rmbmenu = false
boolean ib_rmbfocuschange = false
string is_updatesallowed = ""
end type

event constructor;call super::constructor;this.of_SetFilter(TRUE)
this.of_SetFind(TRUE)
this.inv_filter.of_SetStyle(1)
this.inv_filter.of_SetColumnNameSource(2)
this.of_SetSort(TRUE)
this.inv_sort.of_SetStyle(1)
this.inv_sort.of_SetColumnNameSource(2)
This.of_SetTransObject(sqlservertrans)
ib_rmbmenu = False
//This.of_SetRowManager(True)
//This.of_SetPrintPreview(True)
end event

event getfocus;call super::getfocus;m_pics_main.m_edit.m_deleterow.Enabled =	FALSE
m_pics_main.m_edit.m_addrow.Enabled =	FALSE
m_pics_main.m_edit.m_cut.Enabled =	FALSE
end event

event pfc_retrieve;call super::pfc_retrieve;THIS.Reset()
RETURN THIS.Retrieve(dw_pcs1chk_ace_report)


end event

event retrieveend;call super::retrieveend;IF rowcount = 0 THEN
	Close(w_pics_retrieve_msg_box)
	MessageBox("ERROR","No data found.")
	
Else
	Close(w_pics_retrieve_msg_box)
	Close(w_pcs_gets_fr_date)	
End IF




end event

event retrievestart;call super::retrievestart;OpenWithParm(w_pics_retrieve_msg_box,"Retrieving Exception Report, Please Wait...")
end event

event retrieverow;IF ib_cancel = TRUE THEN
	RETURN 1
END IF

end event


Start of PowerBuilder Binary Data Section : Do NOT Edit
01w_pcs_reports.bin 
2300000c00e011cfd0e11ab1a1000000000000000000000000000000000003003e0009fffe000000060000000000000000000000010000000100000000000010000000000200000001fffffffe0000000000000000fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffd00000004fffffffefffffffefffffffeffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff006f00520074006f004500200074006e00790072000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000050016ffffffffffffffff00000001000000000000000000000000000000000000000000000000000000007621531001ca176d00000003000000800000000000500003004f0042005800430054005300450052004d0041000000000000000000000000000000000000000000000000000000000000000000000000000000000102001affffffff00000002ffffffff000000000000000000000000000000000000000000000000000000000000000000000000fffffffe0000000000000000004200500043004f00530058004f00540041005200450047000000000000000000000000000000000000000000000000000000000000000000000000000000000001001affffffffffffffff000000036580f76711cf781945446cb800005453000000007621531001ca176d7621531001ca176d000000000000000000000000004f00010065006c00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000102000affffffff00000004ffffffff000000000000000000000000000000000000000000000000000000000000000000000000000000000000001400000000fffffffefffffffeffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
2Dffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff0200000100000008000000000000000000000000001301780068007400610050006d0072007700280076005f0075006f00680063007200650072005f00740065000100000000032000000320000000000000000000000000006e005f00770065006c002c00740073005f0072006f00760063007500650068005f007200650072006f0070007400720077002c0070005f00630069005f00730061006d006e00690030002c006f002c00690072006900670061006e0021006c0000002900650072006f007000740072002e0073006200700028006c005f0077006f00760063007500650068005f007200650072007200740065006900650076002e0029006200630066005f006e0069002e0064006c0063006300690065006b002e006400300030003500360020003a002f00200020002f00200020004f0020006500700053006e0065006800740065006900570068007400610050006d0072007700280076005f0075006f00680063007200650072005f007400650069007200760065005f0065007000750061006400650074006e005f00770065006c002c00740073005f0072006f00760063007500650068005f007200650072006f0070007400720077002c0070005f00630069005f00730061006d006e00690030002c006f002c00690072006900670061006e0021006c0000002900660070006d006300690061002e006e006f00430074006e006e00650073007400000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000020012ffffffffffffffffffffffff0000000000000000000000000000000000000000000000000000000000000000000000000000000100000018000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000ffffffffffffffffffffffff0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000ffffffffffffffffffffffff0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000ffffffffffffffffffffffff00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
11w_pcs_reports.bin 
End of PowerBuilder Binary Data Section : No Source Expected After This Point

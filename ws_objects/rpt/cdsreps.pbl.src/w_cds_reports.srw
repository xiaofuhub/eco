$PBExportHeader$w_cds_reports.srw
forward
global type w_cds_reports from w_sheet
end type
type cb_print from u_cb within w_cds_reports
end type
type cb_print_to_file from u_cb within w_cds_reports
end type
type cb_cancel from u_cb within w_cds_reports
end type
type cb_clear from commandbutton within w_cds_reports
end type
type dw_cdfrlist_ace from u_dw within w_cds_reports
end type
type dw_cdconsider_ace from u_dw within w_cds_reports
end type
type dw_cdpasscd from u_dw within w_cds_reports
end type
type dw_annotrpt_ec from u_dw within w_cds_reports
end type
type dw_cds_label_mchar_ttlinit from u_dw within w_cds_reports
end type
type dw_voucher_contract_update from u_dw within w_cds_reports
end type
type dw_annotation_writer_print from u_dw within w_cds_reports
end type
type dw_cdforsale1_ace_report_composite from u_dw within w_cds_reports
end type
type dw_cdforsale1_ace_report from u_dw within w_cds_reports
end type
type dw_cdforsale1_ace_report_br from u_dw within w_cds_reports
end type
type dw_pctil_ec_report_title from u_pics_dw within w_cds_reports
end type
type dw_pctil_ec_report_conno from u_pics_dw within w_cds_reports
end type
type dw_pctil_ec_report_bkno from u_pics_dw within w_cds_reports
end type
type dw_annotation_writer from u_dw within w_cds_reports
end type
type dw_pctil_ec_report from u_dw within w_cds_reports
end type
end forward

global type w_cds_reports from w_sheet
integer x = 69
integer y = 360
integer width = 2789
integer height = 1552
string title = "CDS Reports"
cb_print cb_print
cb_print_to_file cb_print_to_file
cb_cancel cb_cancel
cb_clear cb_clear
dw_cdfrlist_ace dw_cdfrlist_ace
dw_cdconsider_ace dw_cdconsider_ace
dw_cdpasscd dw_cdpasscd
dw_annotrpt_ec dw_annotrpt_ec
dw_cds_label_mchar_ttlinit dw_cds_label_mchar_ttlinit
dw_voucher_contract_update dw_voucher_contract_update
dw_annotation_writer_print dw_annotation_writer_print
dw_cdforsale1_ace_report_composite dw_cdforsale1_ace_report_composite
dw_cdforsale1_ace_report dw_cdforsale1_ace_report
dw_cdforsale1_ace_report_br dw_cdforsale1_ace_report_br
dw_pctil_ec_report_title dw_pctil_ec_report_title
dw_pctil_ec_report_conno dw_pctil_ec_report_conno
dw_pctil_ec_report_bkno dw_pctil_ec_report_bkno
dw_annotation_writer dw_annotation_writer
dw_pctil_ec_report dw_pctil_ec_report
end type
global w_cds_reports w_cds_reports

type variables
date id_stdt, id_enddt, id_cabdt
string it_chno, is_parm
string which_rep,Title_type
end variables

forward prototypes
public function boolean wf_validate_chno (string Lchno)
public function boolean wf_validate_conno (string Lconno)
public function boolean wf_validate_bkno (long Lbkno)
public subroutine wf_cds_save_cdforsale ()
end prototypes

public function boolean wf_validate_chno (string Lchno);int rtn=0
select count(*) into :rtn from ttlinit where chno=:Lchno using sqlservertrans;
if rtn > 0 then
	return true
else
	return false
end if

end function

public function boolean wf_validate_conno (string Lconno);int rtn=0
select count(*) into :rtn from mchar where conno=:Lconno using sqlservertrans;
if rtn > 0 then
	return true
else
	return false
end if

end function

public function boolean wf_validate_bkno (long Lbkno);int rtn=0
select count(*) into :rtn from prod where bkseq=:Lbkno using sqlservertrans;
if rtn > 0 then
	return true
else
	return false
end if

end function

public subroutine wf_cds_save_cdforsale ();integer li_filenum, li_wpfile

long ll_rows,i
string ls_filename, ls_path,Lttl,Ls_today,Lprdr,Lauthor,Lpublisher,Lpages,Loldprdr,Lapplen
string Lauth_fn,Lbkno,Lbkmed,Lsubj,Loneliner,Lanno,Lahonorific,Lmsg ,ls_cabdt
date ld_cabdt
// Get the filename
ls_filename = 'cdforsale'
ls_path = "c:\" + ls_filename
IF GetFileSaveName("Select File",ls_path,ls_filename, "DOC","Text Files (*.TXT),*.TXT," + " Doc Files (*.DOC), *.DOC") <> 1 THEN
	messagebox("Incorrect file name","Please try again")
	return
end if
lmsg = "Saving the file "+ls_path+", Please Wait..."
OpenWithParm(w_pics_retrieve_msg_box,lmsg)
li_fileNum = FileOpen(ls_filename,LineMode!, Write!, LockWrite!, Replace!)
ld_cabdt =id_cabdt
Ls_cabdt = string(ld_cabdt,'mmmm yyyy')

DataWindowChild dwc_braille,dwc_rc,dwc_anno

dw_cdforsale1_ace_report_composite.GetChild("d_cdforsale1_ace_report_br", dwc_braille)

filewrite(li_filenum,'~tBraille Titles Assigned to NLS Producers -- '+Ls_cabdt)
//filewrite(li_filenum,'~t(Group by Producer)')
filewrite(li_filenum," ")
filewrite(li_filenum,'~tPRDR..Title...Author...PUBLISHER...Pages')

ll_rows = dwc_braille.rowcount()
FOR i=1 TO ll_rows
	Lprdr = TRIM(dwc_braille.GetItemString(i, "ancntr_prdr"))
	IF Loldprdr <> Lprdr THEN
		filewrite(li_filenum," ")
	END IF
	Loldprdr = Lprdr
	Lttl = f_remove_pipe(TRIM(dwc_braille.GetItemString(i, "ttlinit_ttl")))
	Lttl = f_anno_with_ss(Lttl)
	Lauthor = TRIM(dwc_braille.GetItemString(i, "ttlinit_auth"))
	Lpublisher = TRIM(dwc_braille.GetItemString(i, "cpublisher"))
	Lpages = TRIM(string(dwc_braille.GetItemNumber(i, "cpbpage")))
	filewrite(li_filenum,'~t'+Lprdr+'...'+Lttl+'...'+Lauthor+'...'+Lpublisher+'...'+string(Lpages))
NEXT

dw_cdforsale1_ace_report_composite.GetChild("d_cdforsale1_ace_report", dwc_rc)

filewrite(li_filenum," ")
filewrite(li_filenum,'~tRecorded Titles Assigned to NLS Producers -- '+Ls_cabdt)
filewrite(li_filenum,'~t(Group by Producer)')
filewrite(li_filenum," ")
filewrite(li_filenum,'~tPRDR..Title...Author...PUBLISHER...Tracks')

ll_rows = dwc_rc.rowcount()
FOR i=1 TO ll_rows
	Lprdr = TRIM(dwc_rc.GetItemString(i, "ancntr_prdr"))
	IF Loldprdr <> Lprdr THEN
		filewrite(li_filenum," ")
	END IF
	Loldprdr = Lprdr
	Lttl = f_remove_pipe(TRIM(dwc_rc.GetItemString(i, "ttlinit_ttl")))
	Lttl = f_anno_with_ss(Lttl)
	Lauthor = TRIM(dwc_rc.GetItemString(i, "ttlinit_auth"))
	Lpublisher = TRIM(dwc_rc.GetItemString(i, "cpublisher"))
	Lapplen = TRIM(string(dwc_rc.GetItemNumber(i, "capplen")))
	filewrite(li_filenum,'~t'+Lprdr+'...'+Lttl+'...'+Lauthor+'...'+Lpublisher+'...'+string(Lapplen))
NEXT

dw_cdforsale1_ace_report_composite.GetChild("d_cdforsale1_ace_report_with_anno", dwc_anno)

filewrite(li_filenum," ")
filewrite(li_filenum,'~tRecorded and Braille Titles Assigned to NLS Producers -- '+Ls_cabdt)
//filewrite(li_filenum,'~t(Arranged alphabetically by title)')
filewrite(li_filenum," ")

ll_rows = dwc_anno.rowcount()
FOR i=1 TO ll_rows
	Lprdr = TRIM(dwc_anno.GetItemString(i, "cprdr"))
	Lttl = f_remove_pipe(TRIM(dwc_anno.GetItemString(i, "cttl")))
	Lttl = f_anno_with_ss(Lttl)
	Lauthor = TRIM(dwc_anno.GetItemString(i, "cauth"))
	Lauth_fn = TRIM(dwc_anno.GetItemString(i, "cauthfn"))
	Lahonorific = TRIM(dwc_anno.GetItemString(i, "cahonorific"))
	Lauthor = f_combine_auth_authfn(Lauthor,Lauth_fn,Lahonorific)
	Lpublisher = TRIM(dwc_anno.GetItemString(i, "cpublisher"))
	Lbkmed = TRIM(dwc_anno.GetItemString(i, "cbkmed"))
	Lapplen = TRIM(string(dwc_anno.GetItemNumber(i, "mchar_applen")))
	Lpages = TRIM(string(dwc_anno.GetItemNumber(i, "pb_page")))
	Lbkno = Lbkmed +TRIM(string(dwc_anno.GetItemNumber(i, "mchar_bkseq")))
	Loneliner = TRIM(dwc_anno.GetItemString(i, "ttlinit_oneliner"))
	Loneliner = f_anno_with_ss(Loneliner)
	Lanno = f_anno_with_ss(TRIM(dwc_anno.GetItemString(i, "canno")))
	Lsubj = TRIM(dwc_anno.GetItemString(i, "ttlinit_casub"))
	
	filewrite(li_filenum,'~t TITLE: '+Lttl)
	filewrite(li_filenum,'~t AUTHOR: '+Lauthor)
	filewrite(li_filenum,'~t PUBLISHER: '+Lpublisher+'~t SUBJ: '+Lsubj)
	IF Lbkmed = 'RC' OR Lbkmed = 'FD' THEN
		filewrite(li_filenum,'~t BKNO: '+Lbkno+'  PRDR: '+Lprdr+'  TRACKS: '+Lapplen)
	ELSE
		filewrite(li_filenum,'~t BKNO: '+Lbkno+'  PRDR: '+Lprdr+'  PAGES: '+Lpages)
	END IF		
	filewrite(li_filenum,'~t ONELINER: '+Loneliner)
	filewrite(li_filenum,'~t ANNOT: '+Lanno)
	filewrite(li_filenum,'~t ...')
NEXT

Fileclose(li_filenum)
setpointer(arrow!)
close(w_pics_retrieve_msg_box)
end subroutine

on w_cds_reports.create
int iCurrent
call super::create
this.cb_print=create cb_print
this.cb_print_to_file=create cb_print_to_file
this.cb_cancel=create cb_cancel
this.cb_clear=create cb_clear
this.dw_cdfrlist_ace=create dw_cdfrlist_ace
this.dw_cdconsider_ace=create dw_cdconsider_ace
this.dw_cdpasscd=create dw_cdpasscd
this.dw_annotrpt_ec=create dw_annotrpt_ec
this.dw_cds_label_mchar_ttlinit=create dw_cds_label_mchar_ttlinit
this.dw_voucher_contract_update=create dw_voucher_contract_update
this.dw_annotation_writer_print=create dw_annotation_writer_print
this.dw_cdforsale1_ace_report_composite=create dw_cdforsale1_ace_report_composite
this.dw_cdforsale1_ace_report=create dw_cdforsale1_ace_report
this.dw_cdforsale1_ace_report_br=create dw_cdforsale1_ace_report_br
this.dw_pctil_ec_report_title=create dw_pctil_ec_report_title
this.dw_pctil_ec_report_conno=create dw_pctil_ec_report_conno
this.dw_pctil_ec_report_bkno=create dw_pctil_ec_report_bkno
this.dw_annotation_writer=create dw_annotation_writer
this.dw_pctil_ec_report=create dw_pctil_ec_report
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.cb_print
this.Control[iCurrent+2]=this.cb_print_to_file
this.Control[iCurrent+3]=this.cb_cancel
this.Control[iCurrent+4]=this.cb_clear
this.Control[iCurrent+5]=this.dw_cdfrlist_ace
this.Control[iCurrent+6]=this.dw_cdconsider_ace
this.Control[iCurrent+7]=this.dw_cdpasscd
this.Control[iCurrent+8]=this.dw_annotrpt_ec
this.Control[iCurrent+9]=this.dw_cds_label_mchar_ttlinit
this.Control[iCurrent+10]=this.dw_voucher_contract_update
this.Control[iCurrent+11]=this.dw_annotation_writer_print
this.Control[iCurrent+12]=this.dw_cdforsale1_ace_report_composite
this.Control[iCurrent+13]=this.dw_cdforsale1_ace_report
this.Control[iCurrent+14]=this.dw_cdforsale1_ace_report_br
this.Control[iCurrent+15]=this.dw_pctil_ec_report_title
this.Control[iCurrent+16]=this.dw_pctil_ec_report_conno
this.Control[iCurrent+17]=this.dw_pctil_ec_report_bkno
this.Control[iCurrent+18]=this.dw_annotation_writer
this.Control[iCurrent+19]=this.dw_pctil_ec_report
end on

on w_cds_reports.destroy
call super::destroy
destroy(this.cb_print)
destroy(this.cb_print_to_file)
destroy(this.cb_cancel)
destroy(this.cb_clear)
destroy(this.dw_cdfrlist_ace)
destroy(this.dw_cdconsider_ace)
destroy(this.dw_cdpasscd)
destroy(this.dw_annotrpt_ec)
destroy(this.dw_cds_label_mchar_ttlinit)
destroy(this.dw_voucher_contract_update)
destroy(this.dw_annotation_writer_print)
destroy(this.dw_cdforsale1_ace_report_composite)
destroy(this.dw_cdforsale1_ace_report)
destroy(this.dw_cdforsale1_ace_report_br)
destroy(this.dw_pctil_ec_report_title)
destroy(this.dw_pctil_ec_report_conno)
destroy(this.dw_pctil_ec_report_bkno)
destroy(this.dw_annotation_writer)
destroy(this.dw_pctil_ec_report)
end on

event close;call super::close;m_pics_main.m_file.m_print.Enabled = False
m_pics_main.m_file.m_pagesetup.Enabled = False
m_pics_main.m_file.m_printimmediate.Enabled = False
m_pics_main.m_edit.m_deleterow.Enabled = False
m_pics_main.m_edit.m_addrow.Enabled = False
end event

event closequery;call super::closequery;Integer	li_pendingrc
Integer	li_validationrc
Integer	li_accepttextrc
Integer	li_msg
Integer	li_rc,rtn
String	ls_msgparms[]


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
					dw_cdconsider_ace.Setfocus()
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
			dw_cdconsider_ace.Setfocus()
			// CANCEL -  Prevent the window from closing
	End Choose
End If

// Prevent the window from closing
ib_closestatus = False
Return 1
end event

event mousemove;call super::mousemove;w_pics_main.setmicrohelp("Ready")
end event

event pfc_preopen;call super::pfc_preopen;this.of_SetPreference(TRUE)
this.inv_preference.of_SetToolBars(TRUE)
this.inv_preference.of_SetWindow(TRUE)
this.of_SetResize(TRUE)
this.inv_resize.of_SetOrigSize(this.workspacewidth(),this.workspaceheight())
inv_resize.of_Register(dw_cdforsale1_ace_report_composite, "scale")
inv_resize.of_Register(dw_cdforsale1_ace_report, "scale")
inv_resize.of_Register(dw_cdforsale1_ace_report_br, "scale")
inv_resize.of_Register(dw_pctil_ec_report, "scale")
//inv_resize.of_Register(dw_pctil_ec_report_fram1, "scale")
//inv_resize.of_Register(dw_pctil_ec_report_fram2, "scale")
inv_resize.of_Register(dw_pctil_ec_report_bkno, "scale")
inv_resize.of_Register(dw_pctil_ec_report_conno, "scale")
inv_resize.of_Register(dw_pctil_ec_report_title, "scale")
inv_resize.of_Register(dw_cdfrlist_ace, "scale")
inv_resize.of_Register(dw_cdconsider_ace, "scale")
inv_resize.of_Register(dw_cdpasscd, "scale")
inv_resize.of_Register(dw_annotrpt_ec, "scale")
inv_resize.of_Register(dw_voucher_contract_update, "scale")
inv_resize.of_Register(dw_cds_label_mchar_ttlinit, "scale")
inv_resize.of_Register(cb_cancel, "scale")
inv_resize.of_Register(cb_print, "scale")
inv_resize.of_Register(cb_clear, "scale")
inv_resize.of_Register(cb_print_to_file, "scale")
inv_resize.of_Register(dw_annotation_writer, "scale")
inv_resize.of_Register(dw_annotation_writer_print, "scale")

end event

event resize;call super::resize;long ll_height

//This script is not commented because when we do minimize this screen then all the DW'S are not resizing 
This.X = w_pics_main.X
This.Y = w_pics_main.Y
ll_height = w_pics_main.mdi_1.Height
This.Resize(w_pics_main.WorkSpaceWidth(), ll_height)


end event

event pfc_postopen;call super::pfc_postopen;m_pics_main.m_file.m_print.Enabled = TRUE
m_pics_main.m_file.m_pagesetup.Enabled = TRUE
m_pics_main.m_file.m_printimmediate.Enabled = TRUE

This.windowstate = maximized!
date ld_stdt, ld_enddt,ld_date
datetime ld_stdt_dt, ld_enddt_dt, ld_date_dt
long ll_count, ll_count1, ll_count2
string lchart_no, ls_contract, DWfilter
str_cds_report lstr_cds_report
str_print_report lstr_print_report
str_voucher_report lstr_voucher_report
str_prod_cntr lstr_prod_cntr
CHOOSE CASE Message.StringParm
	CASE "Sale anno"                // (for open dw_sdforsale_with_anno)
		which_rep = "Sale anno" 
		w_cds_reports.Title = "Recorded and Braille Titles Assigned to Producers"
		dw_cdforsale1_ace_report_composite.Visible = True
		dw_cdforsale1_ace_report_composite.SetFocus()
		Open(w_cds_gets_date_sale)
			IF (IsNull(Message.StringParm)=FALSE AND Message.StringParm<>"") THEN
				ld_stdt = date(Message.StringParm)
				id_cabdt= ld_stdt
				IF IsValid(w_cds_reports) THEN
					ld_stdt_dt=datetime(ld_stdt,time('00:00:00'))
					ll_count = w_cds_reports.dw_cdforsale1_ace_report_composite.Retrieve(ld_stdt_dt)
					IF ll_count > 0 THEN 
						dw_cdforsale1_ace_report_composite.ResetUpdate()
					ELSE
						Close(w_cds_gets_date_sale)
						close(w_cds_reports)
						m_pics_main.m_menu.m_report.m_cdsreports.m_titlesavailableforforeignpurchase.TriggerEvent(Clicked!)
						return						
					End IF
				Else
					Close(w_cds_reports)
					Close(w_cds_gets_date_sale)
					RETURN
				End IF
			Else
				Close(w_cds_reports)
			End IF
	CASE "Title info"            // (for open dw_pctil_ec_report)
		which_rep = "Title info" 
		w_cds_reports.Title = "Title information list"
		is_parm="Title info"
		cb_clear.Visible = True
		Open(w_cds_gets_title_info)
		IF (IsNull(Message.StringParm)=FALSE AND Message.StringParm<>"") THEN
			Title_type = mid(Message.StringParm,1,1)
			lchart_no = mid(Message.StringParm,2)
			CHOOSE CASE Title_type
				CASE '1'
					IF wf_validate_chno(lchart_no) THEN
						dw_pctil_ec_report.Visible = True
						ll_count = w_cds_reports.dw_pctil_ec_report.Retrieve(lchart_no)
						IF ll_count = 0 THEN 
							Close(w_cds_gets_title_info)
							Close(w_cds_reports)
							m_pics_main.m_menu.m_report.m_cdsreports.m_titleinformationlist.TriggerEvent(Clicked!)
							return
						ELSE
							dw_pctil_ec_report.SetFocus()
						END IF
					ELSE
						MessageBox("ERROR","Invalid Chart Number.")
						Close(w_cds_reports)
					END IF
				CASE '2'
					//Title_Type = 'Conno'
					//MessageBox("Title Type",Title_type+" "+lchart_no)
					IF wf_validate_conno(lchart_no) THEN
						dw_pctil_ec_report_conno.Visible = True
						ll_count = w_cds_reports.dw_pctil_ec_report_conno.Retrieve(lchart_no)
						IF ll_count = 0 THEN
							Close(w_cds_gets_title_info)
							Close(w_cds_reports)
							m_pics_main.m_menu.m_report.m_cdsreports.m_titleinformationlist.TriggerEvent(Clicked!)
							return
						ELSE
							dw_pctil_ec_report_conno.SetFocus()
						END IF
					ELSE
						MessageBox("ERROR","Invalid Control Number.")
						Close(w_cds_reports)
					END IF
				CASE '3'
					IF wf_validate_bkno(long(lchart_no)) THEN
						dw_pctil_ec_report_bkno.Visible = True
						ll_count = w_cds_reports.dw_pctil_ec_report_bkno.Retrieve(long(lchart_no))
						IF ll_count = 0 THEN
							Close(w_cds_gets_title_info)
							Close(w_cds_reports)
							m_pics_main.m_menu.m_report.m_cdsreports.m_titleinformationlist.TriggerEvent(Clicked!)
							return
						ELSE
							dw_pctil_ec_report_bkno.SetFocus()
						END IF
					ELSE
						MessageBox("ERROR","Invalid Book Number.")
						Close(w_cds_reports)
					END IF
				CASE '4'
					dw_pctil_ec_report_Title.Visible = True
					ll_count = w_cds_reports.dw_pctil_ec_report_title.Retrieve(lchart_no)
					IF ll_count = 0 THEN
						Close(w_cds_gets_title_info)
						Close(w_cds_reports)
						m_pics_main.m_menu.m_report.m_cdsreports.m_titleinformationlist.TriggerEvent(Clicked!)
						return
					ELSE
						dw_pctil_ec_report_title.SetFocus()
					END IF
			END CHOOSE
		END IF
CASE "Consider"						// (for open dw_cdconsider_ace)
		which_rep = "Consider" 
		w_cds_reports.Title = "Title for consideration"
		dw_cdconsider_ace.Visible = True
		dw_cdconsider_ace.SetFocus()
		Open(w_cds_gets_double_chno)
		lstr_print_report = Message.PowerObjectParm
		IF IsValid(w_cds_reports) THEN
			ll_count = w_cds_reports.dw_cdconsider_ace.Retrieve(lstr_print_report.lchart_no, lstr_print_report.lchart_no1)
			IF ll_count = 0 THEN 
				Close(w_cds_gets_double_chno)
				Close(w_cds_reports)
				m_pics_main.m_menu.m_report.m_cdsreports.m_titlesforconsideration.TriggerEvent(Clicked!)
				Return
			END IF
		Else
			Close(w_cds_reports)
			Close(w_cds_gets_double_chno)
			Return
		END IF
CASE "Review"							// (for open dw_cdfrlist_ace)
		which_rep = "Review" 
		w_cds_reports.Title = "Weekly final review"
		dw_cdfrlist_ace.Visible = True
		dw_cdfrlist_ace.SetFocus()
		Open(w_cds_gets_date)
		IF (IsNull(Message.StringParm)=FALSE AND Message.StringParm<>"") THEN
			ld_stdt = date(Message.StringParm)
			IF IsValid(w_cds_reports) THEN
				ld_stdt_dt=datetime(ld_stdt,time('00:00:00'))
				ll_count = w_cds_reports.dw_cdfrlist_ace.Retrieve(ld_stdt_dt)
				IF ll_count = 0 THEN 
					close(w_cds_gets_date)
					Close(w_cds_reports)
					m_pics_main.m_menu.m_report.m_cdsreports.m_weeklyfinalreview.TriggerEvent(Clicked!)
					return
				END IF
			Else
				Close(w_cds_reports)
				Close(w_cds_gets_date)
				Return
			END IF
		Else
			Close(w_cds_reports)
		End IF
CASE "Title anno"						// (for open dw_cdpasscd)
		which_rep = "Title anno" 
		w_cds_reports.Title = "Title annotated"
		dw_cdpasscd.Visible = True
		dw_cdpasscd.SetFocus()
		Open(w_cds_gets_dates)
		lstr_cds_report = Message.PowerObjectParm
	if lstr_cds_report.ls_string='CANCEL' THEN
		Close(w_cds_reports)
		RETURN
	ELSE	
		ld_stdt=lstr_cds_report.ld_stdt
		ld_stdt_dt=datetime(ld_stdt,time('00:00:00'))
		ld_enddt=lstr_cds_report.ld_enddt
		ld_enddt_dt=datetime(ld_enddt,time('00:00:00'))
		IF IsValid(w_cds_reports) THEN
			ll_count = w_cds_reports.dw_cdpasscd.Retrieve(ld_stdt_dt,ld_enddt_dt )
			IF ll_count = 0 THEN 
				Close(w_cds_gets_dates)
				Close(w_cds_reports)
				m_pics_main.m_menu.m_report.m_cdsreports.m_titleannotated.TriggerEvent(Clicked!)
				return
			END IF
		ELSE
			Close(w_cds_reports)
			Close(w_cds_gets_dates)
			Return
		END IF
		IF ll_count > 0 THEN
			ld_stdt = lstr_cds_report.ld_stdt
			ld_enddt = lstr_cds_report.ld_enddt
			dw_cdpasscd.object.st_start_date.text = string(ld_stdt,'mm/dd/yyyy')
			dw_cdpasscd.object.st_end_date.text = string(ld_enddt,'mm/dd/yyyy')
		END IF
	END IF
CASE "Edit anno"						// (for open dw_annotrpt_ec)
		which_rep = "Edit anno" 
		w_cds_reports.Title = "Edit annotations"
		dw_annotrpt_ec.Visible = True
		dw_annotrpt_ec.SetFocus()
		Open(w_cds_gets_chno_anno)
		IF (IsNull(Message.StringParm)=FALSE AND Message.StringParm<>"") THEN
			lchart_no = Message.StringParm
			IF IsValid(w_cds_reports) THEN
				ll_count = w_cds_reports.dw_annotrpt_ec.Retrieve(lchart_no)
				IF ll_count = 0 THEN 
					Close(w_cds_gets_chno_anno)
					Close(w_cds_reports)
					m_pics_main.m_menu.m_report.m_cdsreports.m_editannotation.TriggerEvent(Clicked!)
					return
				END IF
			ELSE
				Close(w_cds_reports)
				Close(w_cds_gets_chno_anno)
				Return
			END IF
		Else
			Close(w_cds_reports)
		End IF
CASE "Voucher"						// (for open dw_voucher_contract_update)
		which_rep = "Voucher" 
		w_cds_reports.Title = "Voucher Report"
		dw_voucher_contract_update.Visible = True
		dw_voucher_contract_update.SetFocus()
		Open(w_cds_gets_date_cntr)
		lstr_voucher_report = Message.PowerObjectParm
		IF IsValid(w_cds_reports) THEN
			ll_count = w_cds_reports.dw_voucher_contract_update.Retrieve(lstr_voucher_report.ld_date, lstr_voucher_report.ls_contract)
			IF ll_count = 0 THEN return
		ELSE
			Close(w_cds_reports)
			Close(w_cds_gets_date_cntr)
			Return
		END IF
CASE "Labels"						// (for open dw_cdconsider_ace)
	//	which_rep = "Consider" 
		w_cds_reports.Title = "Labels"
		dw_cds_label_mchar_ttlinit.SetTransObject(sqlservertrans)
		dw_cds_label_mchar_ttlinit.SetFocus()
		OpenWithParm(w_cds_gets_double_chno,'Labels')
		lstr_print_report = Message.PowerObjectParm
		IF IsValid(w_cds_reports) THEN
			ll_count = w_cds_reports.dw_cds_label_mchar_ttlinit.Retrieve &
							(lstr_print_report.lchart_no, lstr_print_report.lchart_no1)
			IF ll_count = 0 THEN 
				Close(w_cds_gets_double_chno)
				Close(w_cds_reports)
				MessageBox('Error','Try another pare of chno again!')
				m_pics_main.m_menu.m_report.m_cdsreports.m_labels.TriggerEvent(Clicked!)
				Return
			END IF
		dw_cds_label_mchar_ttlinit.Visible = True
		Else
			Close(w_cds_reports)
			Close(w_cds_gets_double_chno)
			Return
		END IF
CASE "Writer"
//		which_rep = "Voucher"
		string ls_init, ls_date
//		date ld_date
		w_cds_reports.Title = "Annotation Writer"
		dw_annotation_writer.Visible = True
		dw_annotation_writer.SetFocus()
		Open(w_cds_gets_date_init)
		IF not isvalid(Message.PowerObjectParm) THEN
			Close(w_cds_reports)
			Return
		END IF
		
		lstr_prod_cntr = Message.PowerObjectParm
		ld_date = lstr_prod_cntr.ld_stdt
		ld_date_dt=datetime(ld_date,time('00:00:00'))
		ls_init = UPPER(TRIM(lstr_prod_cntr.ls_cntr))
		IF IsValid(w_cds_reports) THEN
			dw_annotation_writer.SetTransObject(SqlServerTrans )
			ll_count = dw_annotation_writer.Retrieve( ld_date_dt,ls_init)
			//Messagebox("data"," date = "+string(ld_date)+" initial = "+ls_init+" count = "+string(ll_count))
			dw_annotation_writer_print.SetTransObject(SqlServerTrans )
			dw_annotation_writer_print.Retrieve( ld_date_dt,ls_init)
			IF ll_count = 0 THEN return
		ELSE
			Close(w_cds_reports)
			Close(w_cds_gets_date_init)
			Return
		END IF
		
		
		
END CHOOSE
end event

type cb_print from u_cb within w_cds_reports
integer x = 1723
integer y = 1348
integer width = 274
integer height = 72
integer taborder = 0
string text = "&Print"
end type

event clicked;call super::clicked;string ls_auth, ls_filter
long li_cnt

IF	dw_cdforsale1_ace_report_composite.Visible = True THEN
	dw_cdforsale1_ace_report_composite.Triggerevent("pfc_Print")
ELSEIF  dw_voucher_contract_update.Visible = True THEN
	dw_voucher_contract_update.Triggerevent("pfc_Print")
ELSEIF 	(dw_pctil_ec_report.Visible = True OR & 
			dw_pctil_ec_report_conno.Visible = True OR &
			dw_pctil_ec_report_title.Visible = True OR &
			dw_pctil_ec_report_bkno.Visible = True ) THEN
	CHOOSE CASE Title_type
	CASE '1'
		li_cnt=dw_pctil_ec_report.RowCount()
		if li_cnt>1 then
			ls_auth=trim(dw_pctil_ec_report.object.ccoauth[1])
			ls_filter="ccoauth ='"+ls_auth+"'"
			dw_pctil_ec_report.Setfilter(ls_filter)
			dw_pctil_ec_report.Filter()
			li_cnt=dw_pctil_ec_report.RowCount()
			if li_cnt<>1 then
				messagebox('Error','need replace filter',Stopsign!)
				return
			end if
		end if
		dw_pctil_ec_report.Triggerevent("pfc_Print")		
	CASE '2'
		li_cnt=dw_pctil_ec_report_conno.RowCount()
		if li_cnt>1 then
			ls_auth=trim(dw_pctil_ec_report_conno.object.ccoauth[1])
			ls_filter="ccoauth ='"+ls_auth+"'"
			dw_pctil_ec_report_conno.Setfilter(ls_filter)
			dw_pctil_ec_report_conno.Filter()
			li_cnt=dw_pctil_ec_report_conno.RowCount()
			if li_cnt<>1 then
				messagebox('Error','need replace filter',Stopsign!)
				return
			end if
		end if
		dw_pctil_ec_report_conno.Triggerevent("pfc_Print")
	CASE '3'
		li_cnt=dw_pctil_ec_report_bkno.RowCount()
		if li_cnt>1 then
			ls_auth=trim(dw_pctil_ec_report_bkno.object.ccoauth[1])
			ls_filter="ccoauth ='"+ls_auth+"'"
			dw_pctil_ec_report_bkno.Setfilter(ls_filter)
			dw_pctil_ec_report_bkno.Filter()
			li_cnt=dw_pctil_ec_report_bkno.RowCount()
			if li_cnt<>1 then
				messagebox('Error','need replace filter',Stopsign!)
				return
			end if
		end if
		dw_pctil_ec_report_bkno.Triggerevent("pfc_Print")
	CASE '4'
		li_cnt=dw_pctil_ec_report_title.RowCount()
		if li_cnt>1 then
			ls_auth=trim(dw_pctil_ec_report_title.object.ccoauth[1])
			ls_filter="ccoauth ='"+ls_auth+"'"
			dw_pctil_ec_report_title.Setfilter(ls_filter)
			dw_pctil_ec_report_title.Filter()
			li_cnt=dw_pctil_ec_report_title.RowCount()
			if li_cnt<>1 then
				messagebox('Error','need replace filter',Stopsign!)
				return
			end if
		end if
		dw_pctil_ec_report_title.Triggerevent("pfc_Print")
	END CHOOSE

ELSEIF dw_cdconsider_ace.Visible = True THEN
	nvo_PowerPrn.of_SetPrinterOrientation(2)
	dw_cdconsider_ace.Triggerevent("pfc_Print")
ELSEIF dw_cds_label_mchar_ttlinit.Visible = True THEN
	dw_cds_label_mchar_ttlinit.Triggerevent("pfc_Print")
ELSEIF dw_cdfrlist_ace.Visible = True THEN
	nvo_PowerPrn.of_SetPrinterOrientation(2)
	dw_cdfrlist_ace.Triggerevent("pfc_Print")
ELSEIF dw_cdpasscd.Visible = True THEN
	nvo_PowerPrn.of_SetPrinterOrientation(2)
	dw_cdpasscd.Triggerevent("pfc_Print")
ELSEIF dw_annotrpt_ec.Visible = True THEN
	dw_annotrpt_ec.Triggerevent("pfc_Print")
ELSEIF dw_annotation_writer.Visible = True THEN
	dw_annotation_writer_print.Triggerevent("pfc_Print")	
END IF
f_pics_set_def_prn_setting()

end event

type cb_print_to_file from u_cb within w_cds_reports
integer x = 2066
integer y = 1348
integer width = 288
integer height = 72
integer taborder = 0
string text = "&Save"
end type

event clicked;call super::clicked;CHOOSE CASE which_rep
	CASE "Sale anno"                // (for save dw_sdforsale_reports)
		wf_cds_save_cdforsale()
	CASE "Title info"  					// (for save dw_pctil_ec_report)
		CHOOSE CASE Title_type
			CASE '1'
				dw_pctil_ec_report.SaveAs()
			CASE '2'
				dw_pctil_ec_report_conno.SaveAs()
			CASE '3'
				dw_pctil_ec_report_bkno.SaveAs()
			CASE '4'
				dw_pctil_ec_report_title.SaveAs()
		END CHOOSE						
	CASE "Consider"						// (for save dw_cdconsider_ace)
		dw_cdconsider_ace.SaveAs()
	CASE "Review"							// (for save dw_cdfrlist_ace)
		dw_cdfrlist_ace.SaveAs()
	CASE "Title anno"						// (for save dw_cdpasscd)
		dw_cdpasscd.SaveAs()
	CASE "Edit anno"						// (for save dw_annotrpt_ec)
		dw_annotrpt_ec.SaveAs()
	CASE "Voucher"						// (for save dw_voucher_contract_update)
		dw_voucher_contract_update.SaveAs()
END CHOOSE

end event

type cb_cancel from u_cb within w_cds_reports
integer x = 2427
integer y = 1348
integer width = 274
integer height = 72
integer taborder = 0
string text = "E&xit"
end type

event clicked;call super::clicked;ib_disableclosequery = TRUE

parent.Event pfc_close()
m_pics_main.m_menu.PopMenu(300, 0)
//close(w_cds_reports)
end event

type cb_clear from commandbutton within w_cds_reports
event postclear ( long as_parm )
boolean visible = false
integer x = 1390
integer y = 1348
integer width = 270
integer height = 72
integer taborder = 21
boolean bringtotop = true
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "C&lear"
end type

event clicked;ib_disableclosequery = TRUE
string ls_parm
long rtn, ll_count
string lchart_no


ls_parm=is_parm
if ls_parm="Title info" then
	which_rep = "Title info" 
		w_cds_reports.Title = "Title information list"
		is_parm="Title info"
		cb_clear.Visible = True
		Open(w_cds_gets_title_info)
		IF (IsNull(Message.StringParm)=FALSE AND Message.StringParm<>"") THEN
			Title_type = mid(Message.StringParm,1,1)
			lchart_no = mid(Message.StringParm,2)
			CHOOSE CASE Title_type
				CASE '1'
					IF wf_validate_chno(lchart_no) THEN
						dw_pctil_ec_report.Visible = True
						ll_count = w_cds_reports.dw_pctil_ec_report.Retrieve(lchart_no)
						IF ll_count = 0 THEN 
							Close(w_cds_gets_title_info)
							Close(w_cds_reports)
							m_pics_main.m_menu.m_report.m_cdsreports.m_titleinformationlist.TriggerEvent(Clicked!)
							return
						ELSE
							dw_pctil_ec_report.SetFocus()
						END IF
					ELSE
						MessageBox("ERROR","Invalid Chart Number.")
						Close(w_cds_reports)
					END IF
				CASE '2'
					//Title_Type = 'Conno'
					//MessageBox("Title Type",Title_type+" "+lchart_no)
					IF wf_validate_conno(lchart_no) THEN
						dw_pctil_ec_report_conno.Visible = True
						ll_count = w_cds_reports.dw_pctil_ec_report_conno.Retrieve(lchart_no)
						IF ll_count = 0 THEN
							Close(w_cds_gets_title_info)
							Close(w_cds_reports)
							m_pics_main.m_menu.m_report.m_cdsreports.m_titleinformationlist.TriggerEvent(Clicked!)
							return
						ELSE
							dw_pctil_ec_report_conno.SetFocus()
						END IF
					ELSE
						MessageBox("ERROR","Invalid Control Number.")
						Close(w_cds_reports)
					END IF
				CASE '3'
					IF wf_validate_bkno(long(lchart_no)) THEN
						dw_pctil_ec_report_bkno.Visible = True
						ll_count = w_cds_reports.dw_pctil_ec_report_bkno.Retrieve(long(lchart_no))
						IF ll_count = 0 THEN
							Close(w_cds_gets_title_info)
							Close(w_cds_reports)
							m_pics_main.m_menu.m_report.m_cdsreports.m_titleinformationlist.TriggerEvent(Clicked!)
							return
						ELSE
							dw_pctil_ec_report_bkno.SetFocus()
						END IF
					ELSE
						MessageBox("ERROR","Invalid Book Number.")
						Close(w_cds_reports)
					END IF
				CASE '4'
					dw_pctil_ec_report_Title.Visible = True
					ll_count = w_cds_reports.dw_pctil_ec_report_title.Retrieve(lchart_no)
					IF ll_count = 0 THEN
						Close(w_cds_gets_title_info)
						Close(w_cds_reports)
						m_pics_main.m_menu.m_report.m_cdsreports.m_titleinformationlist.TriggerEvent(Clicked!)
						return
					ELSE
						dw_pctil_ec_report_title.SetFocus()
					END IF
			END CHOOSE
		END IF
	return
end if

close(w_cds_reports)

m_pics_main.m_menu.PopMenu(300, 0)	


end event

type dw_cdfrlist_ace from u_dw within w_cds_reports
boolean visible = false
integer x = 14
integer y = 12
integer width = 2715
integer height = 1316
integer taborder = 90
string dataobject = "d_cdfrlist_ace"
boolean hscrollbar = true
boolean border = false
borderstyle borderstyle = stylebox!
end type

event constructor;call super::constructor;This.of_SetTransObject(sqlservertrans)
ib_rmbmenu = False
This.of_SetRowManager(True)
This.of_SetPrintPreview(True)
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
	close(w_cds_gets_date)
End IF

end event

event retrievestart;call super::retrievestart;OpenWithParm(w_pics_retrieve_msg_box,"Retrieving Titles Passing Final Review, Please Wait...")
end event

type dw_cdconsider_ace from u_dw within w_cds_reports
boolean visible = false
integer x = 14
integer y = 12
integer width = 2715
integer height = 1316
integer taborder = 0
string dataobject = "d_cdconsider_ace_report1"
boolean hscrollbar = true
boolean border = false
borderstyle borderstyle = stylebox!
end type

event constructor;call super::constructor;This.of_SetTransObject(sqlservertrans)
ib_rmbmenu = False
This.of_SetRowManager(True)
This.of_SetPrintPreview(True)
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
	Close(w_cds_gets_double_chno)
End IF

end event

event retrievestart;call super::retrievestart;OpenWithParm(w_pics_retrieve_msg_box,"Retrieving Titles for Consideration, Please Wait...")
end event

type dw_cdpasscd from u_dw within w_cds_reports
boolean visible = false
integer x = 14
integer y = 12
integer width = 2715
integer height = 1316
integer taborder = 0
string dataobject = "d_cdpasscd_report"
boolean hscrollbar = true
boolean border = false
borderstyle borderstyle = stylebox!
end type

event constructor;call super::constructor;This.of_SetTransObject(sqlservertrans)
ib_rmbmenu = False
This.of_SetRowManager(True)
This.of_SetPrintPreview(True)
end event

event getfocus;call super::getfocus;m_pics_main.m_edit.m_deleterow.Enabled =	FALSE
m_pics_main.m_edit.m_addrow.Enabled =	FALSE
m_pics_main.m_edit.m_cut.Enabled =	FALSE
end event

event retrievestart;call super::retrievestart;OpenWithParm(w_pics_retrieve_msg_box,"Retrieving CDS Titles Annotated, Please Wait...")
end event

event retrieveend;call super::retrieveend;IF rowcount = 0 THEN
	Close(w_pics_retrieve_msg_box)
	MessageBox("ERROR","No data found.")	
Else
	Close(w_pics_retrieve_msg_box)
	Close(w_cds_gets_dates)
End IF

end event

type dw_annotrpt_ec from u_dw within w_cds_reports
boolean visible = false
integer x = 14
integer y = 12
integer width = 2715
integer height = 1316
integer taborder = 0
string dataobject = "d_annotrpt_ec_report"
boolean hscrollbar = true
boolean border = false
borderstyle borderstyle = stylebox!
end type

event constructor;call super::constructor;This.of_SetTransObject(sqlservertrans)
ib_rmbmenu = False
This.of_SetRowManager(True)
This.of_SetPrintPreview(True)
end event

event getfocus;call super::getfocus;m_pics_main.m_edit.m_deleterow.Enabled =	FALSE
m_pics_main.m_edit.m_addrow.Enabled =	FALSE
m_pics_main.m_edit.m_cut.Enabled =	FALSE
end event

event retrievestart;call super::retrievestart;OpenWithParm(w_pics_retrieve_msg_box,"Retrieving Edit Annotations, Please Wait...")
end event

event retrieveend;call super::retrieveend;IF rowcount = 0 THEN
	Close(w_pics_retrieve_msg_box)
	MessageBox("ERROR","No data found.")	
Else
	Close(w_pics_retrieve_msg_box)
	Close(w_cds_gets_chno_anno)
End IF

end event

type dw_cds_label_mchar_ttlinit from u_dw within w_cds_reports
boolean visible = false
integer x = 9
integer y = 24
integer width = 2729
integer height = 1308
integer taborder = 40
string dataobject = "d_cds_label_mchar_ttlinit"
boolean hscrollbar = true
boolean border = false
borderstyle borderstyle = stylebox!
end type

type dw_voucher_contract_update from u_dw within w_cds_reports
boolean visible = false
integer x = 14
integer y = 12
integer width = 2715
integer height = 1316
integer taborder = 0
string dataobject = "d_voucher_contract_update"
boolean hscrollbar = true
boolean border = false
borderstyle borderstyle = stylebox!
end type

event constructor;call super::constructor;This.of_SetTransObject(sqlservertrans)
ib_rmbmenu = False
This.of_SetRowManager(True)
This.of_SetPrintPreview(True)
end event

event getfocus;call super::getfocus;m_pics_main.m_edit.m_deleterow.Enabled =	FALSE
m_pics_main.m_edit.m_addrow.Enabled =	FALSE
m_pics_main.m_edit.m_cut.Enabled =	FALSE
end event

event retrievestart;call super::retrievestart;OpenWithParm(w_pics_retrieve_msg_box,"Retrieving Voucher Report, Please Wait...")
end event

event retrieveend;call super::retrieveend;Close(w_pics_retrieve_msg_box)

end event

type dw_annotation_writer_print from u_dw within w_cds_reports
boolean visible = false
integer x = 27
integer y = 8
integer width = 2679
integer height = 1296
integer taborder = 20
string dataobject = "d_annotation_writer_print"
boolean hscrollbar = true
boolean border = false
borderstyle borderstyle = stylebox!
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

event retrieveend;call super::retrieveend;this.SetSort("ttlinit.ttl")
this.Sort()

end event

type dw_cdforsale1_ace_report_composite from u_dw within w_cds_reports
boolean visible = false
integer x = 14
integer y = 12
integer width = 2715
integer height = 1316
integer taborder = 0
string dataobject = "d_cdforsale1_ace_report_composite"
boolean hscrollbar = true
boolean border = false
borderstyle borderstyle = stylebox!
end type

event constructor;call super::constructor;This.of_SetTransObject(sqlservertrans)
ib_rmbmenu = False
This.of_SetRowManager(True)
This.of_SetPrintPreview(True)
This.of_SetReport(TRUE)
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
	Close(w_cds_gets_date_sale)
End IF

end event

event retrievestart;call super::retrievestart;OpenWithParm(w_pics_retrieve_msg_box,"Retrieving Recorded and Braille Titles, Please Wait...")
end event

type dw_cdforsale1_ace_report from u_dw within w_cds_reports
boolean visible = false
integer x = 14
integer y = 12
integer width = 2715
integer height = 1316
integer taborder = 80
string dataobject = "d_cdforsale1_ace_report"
boolean hscrollbar = true
boolean border = false
borderstyle borderstyle = stylebox!
end type

event constructor;call super::constructor;This.of_SetTransObject(sqlservertrans)
ib_rmbmenu = False
This.of_SetRowManager(True)
This.of_SetPrintPreview(True)
end event

event getfocus;call super::getfocus;m_pics_main.m_edit.m_deleterow.Enabled =	FALSE
m_pics_main.m_edit.m_addrow.Enabled =	FALSE
m_pics_main.m_edit.m_cut.Enabled =	FALSE
end event

event retrieveend;call super::retrieveend;Close(w_pics_retrieve_msg_box)

end event

event retrievestart;call super::retrievestart;Open(w_pics_retrieve_msg_box)
end event

type dw_cdforsale1_ace_report_br from u_dw within w_cds_reports
boolean visible = false
integer x = 14
integer y = 12
integer width = 2715
integer height = 1316
integer taborder = 70
string dataobject = "d_cdforsale1_ace_report_br"
boolean hscrollbar = true
boolean vscrollbar = false
boolean border = false
boolean livescroll = false
borderstyle borderstyle = stylebox!
end type

event constructor;call super::constructor;This.of_SetTransObject(sqlservertrans)
ib_rmbmenu = False
of_SetRowManager(True)
This.of_SetPrintPreview(True)
end event

event getfocus;call super::getfocus;m_pics_main.m_edit.m_deleterow.Enabled =	FALSE
m_pics_main.m_edit.m_addrow.Enabled =	FALSE
m_pics_main.m_edit.m_cut.Enabled =	FALSE
end event

event retrievestart;call super::retrievestart;Open(w_pics_retrieve_msg_box)
end event

event retrieveend;call super::retrieveend;Close(w_pics_retrieve_msg_box)

end event

type dw_pctil_ec_report_title from u_pics_dw within w_cds_reports
boolean visible = false
integer x = 18
integer y = 16
integer width = 2725
integer height = 1320
integer taborder = 10
string dataobject = "d_pctil_ec_report_title"
boolean hscrollbar = true
end type

event constructor;call super::constructor;This.of_SetTransObject(sqlservertrans)
ib_rmbmenu = False
This.of_SetRowManager(True)
This.of_SetPrintPreview(True)
end event

event getfocus;call super::getfocus;m_pics_main.m_edit.m_deleterow.Enabled =	FALSE
m_pics_main.m_edit.m_addrow.Enabled =	FALSE
m_pics_main.m_edit.m_cut.Enabled =	FALSE
end event

event retrieveend;call super::retrieveend;string setting
setting = this.Describe("DataWindow.Storage")
	
IF Long(setting) > 120000 THEN RETURN 1

IF rowcount = 0 THEN
	Close(w_pics_retrieve_msg_box)
	MessageBox("ERROR","No data found.")	
Else
	Close(w_pics_retrieve_msg_box)
	Close(w_cds_gets_title_info)
End IF

end event

event retrievestart;call super::retrievestart;OpenWithParm(w_pics_retrieve_msg_box,"Retrieving Title Information List, Please Wait...")

end event

type dw_pctil_ec_report_conno from u_pics_dw within w_cds_reports
boolean visible = false
integer x = 14
integer y = 12
integer width = 2715
integer height = 1316
integer taborder = 60
string dataobject = "d_pctil_ec_report_conno"
boolean hscrollbar = true
boolean border = false
borderstyle borderstyle = stylebox!
end type

event constructor;call super::constructor;This.of_SetTransObject(sqlservertrans)
ib_rmbmenu = False
This.of_SetRowManager(True)
This.of_SetPrintPreview(True)
end event

event getfocus;call super::getfocus;m_pics_main.m_edit.m_deleterow.Enabled =	FALSE
m_pics_main.m_edit.m_addrow.Enabled =	FALSE
m_pics_main.m_edit.m_cut.Enabled =	FALSE
end event

event retrievestart;call super::retrievestart;OpenWithParm(w_pics_retrieve_msg_box,"Retrieving Title Information List, Please Wait...")

end event

event retrieveend;call super::retrieveend;string setting
setting = this.Describe("DataWindow.Storage")
	
IF Long(setting) > 120000 THEN RETURN 1


IF rowcount = 0 THEN
	Close(w_pics_retrieve_msg_box)
	MessageBox("ERROR","No data found.")	
Else
	Close(w_pics_retrieve_msg_box)
	Close(w_cds_gets_title_info)
End IF

end event

type dw_pctil_ec_report_bkno from u_pics_dw within w_cds_reports
boolean visible = false
integer x = 14
integer y = 12
integer width = 2715
integer height = 1316
integer taborder = 50
string dataobject = "d_pctil_ec_report_bkno"
boolean hscrollbar = true
boolean border = false
borderstyle borderstyle = stylebox!
end type

event constructor;call super::constructor;This.of_SetTransObject(sqlservertrans)
ib_rmbmenu = False
This.of_SetRowManager(True)
This.of_SetPrintPreview(True)
end event

event getfocus;call super::getfocus;m_pics_main.m_edit.m_deleterow.Enabled =	FALSE
m_pics_main.m_edit.m_addrow.Enabled =	FALSE
m_pics_main.m_edit.m_cut.Enabled =	FALSE
end event

event retrievestart;call super::retrievestart;OpenWithParm(w_pics_retrieve_msg_box,"Retrieving Title Information List, Please Wait...")
end event

event retrieveend;call super::retrieveend;string setting
setting = this.Describe("DataWindow.Storage")
	
IF Long(setting) > 120000 THEN RETURN 1

IF rowcount = 0 THEN
	Close(w_pics_retrieve_msg_box)
	MessageBox("ERROR","No data found.")	
Else
	Close(w_pics_retrieve_msg_box)
	Close(w_cds_gets_title_info)
End IF

end event

type dw_annotation_writer from u_dw within w_cds_reports
boolean visible = false
integer x = 18
integer y = 24
integer width = 2725
integer height = 1288
integer taborder = 30
boolean bringtotop = true
string dataobject = "d_annotation_writer"
boolean hscrollbar = true
boolean border = false
borderstyle borderstyle = stylebox!
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

event retrieveend;call super::retrieveend;this.SetSort("ttlinit.ttl")
this.Sort()


end event

type dw_pctil_ec_report from u_dw within w_cds_reports
boolean visible = false
integer x = 14
integer y = 16
integer width = 2715
integer height = 1316
integer taborder = 0
boolean bringtotop = true
string dataobject = "d_pctil_ec_report"
boolean hscrollbar = true
boolean border = false
borderstyle borderstyle = stylebox!
end type

event constructor;call super::constructor;This.of_SetTransObject(sqlservertrans)
ib_rmbmenu = False
This.of_SetRowManager(True)
This.of_SetPrintPreview(True)
end event

event getfocus;call super::getfocus;m_pics_main.m_edit.m_deleterow.Enabled =	FALSE
m_pics_main.m_edit.m_addrow.Enabled =	FALSE
m_pics_main.m_edit.m_cut.Enabled =	FALSE
end event

event pfc_retrieve;call super::pfc_retrieve;THIS.Reset()
RETURN THIS.Retrieve(dw_pctil_ec_report)
end event

event retrievestart;call super::retrievestart;OpenWithParm(w_pics_retrieve_msg_box,"Retrieving Title Information List, Please Wait...")
end event

event retrieveend;call super::retrieveend;string setting
setting = this.Describe("DataWindow.Storage")
	
IF Long(setting) > 120000 THEN RETURN 1

IF rowcount = 0 THEN
	Close(w_pics_retrieve_msg_box)
	MessageBox("ERROR","No data found.")	
Else
	Close(w_pics_retrieve_msg_box)
	Close(w_cds_gets_title_info)
End IF










end event


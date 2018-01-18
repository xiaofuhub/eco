$PBExportHeader$w_qa_zedval.srw
forward
global type w_qa_zedval from w_sheet
end type
type shl_1 from statichyperlink within w_qa_zedval
end type
type mle_comments from u_mle within w_qa_zedval
end type
type cb_spell from commandbutton within w_qa_zedval
end type
type st_opf_ncx from statictext within w_qa_zedval
end type
type mle_opf_ncx from multilineedit within w_qa_zedval
end type
type sle_multidisc from singlelineedit within w_qa_zedval
end type
type cbx_multidisc from checkbox within w_qa_zedval
end type
type cb_qa from commandbutton within w_qa_zedval
end type
type lb_cdrom from listbox within w_qa_zedval
end type
type st_cdrom from statictext within w_qa_zedval
end type
type sle_cntryr from singlelineedit within w_qa_zedval
end type
type st_cntryr from statictext within w_qa_zedval
end type
type sle_search from singlelineedit within w_qa_zedval
end type
type cb_search from commandbutton within w_qa_zedval
end type
type cb_later from commandbutton within w_qa_zedval
end type
type st_smil from statictext within w_qa_zedval
end type
type st_status from statictext within w_qa_zedval
end type
type st_comments from statictext within w_qa_zedval
end type
type cbx_playbook_check from checkbox within w_qa_zedval
end type
type cb_t from commandbutton within w_qa_zedval
end type
type cb_refresh from commandbutton within w_qa_zedval
end type
type lb_smil from listbox within w_qa_zedval
end type
type cb_play from commandbutton within w_qa_zedval
end type
type cb_reject from commandbutton within w_qa_zedval
end type
type cb_approve from commandbutton within w_qa_zedval
end type
type cb_exit from commandbutton within w_qa_zedval
end type
type tab_dtb from tab within w_qa_zedval
end type
type tabpage_reportxml from userobject within tab_dtb
end type
type cbx_reportxml_check from checkbox within tabpage_reportxml
end type
type mle_reportxml from multilineedit within tabpage_reportxml
end type
type tabpage_reportxml from userobject within tab_dtb
cbx_reportxml_check cbx_reportxml_check
mle_reportxml mle_reportxml
end type
type tabpage_nlsreportxml from userobject within tab_dtb
end type
type cbx_nlsreportxml_check from checkbox within tabpage_nlsreportxml
end type
type mle_nlsreportxml from multilineedit within tabpage_nlsreportxml
end type
type tabpage_nlsreportxml from userobject within tab_dtb
cbx_nlsreportxml_check cbx_nlsreportxml_check
mle_nlsreportxml mle_nlsreportxml
end type
type tabpage_opf from userobject within tab_dtb
end type
type cbx_opf_check from checkbox within tabpage_opf
end type
type mle_opf from multilineedit within tabpage_opf
end type
type tabpage_opf from userobject within tab_dtb
cbx_opf_check cbx_opf_check
mle_opf mle_opf
end type
type tabpage_ncx from userobject within tab_dtb
end type
type cbx_ncx_check from checkbox within tabpage_ncx
end type
type mle_ncx from multilineedit within tabpage_ncx
end type
type tabpage_ncx from userobject within tab_dtb
cbx_ncx_check cbx_ncx_check
mle_ncx mle_ncx
end type
type tabpage_all_smil from userobject within tab_dtb
end type
type cbx_smil_check from checkbox within tabpage_all_smil
end type
type mle_allsmil from multilineedit within tabpage_all_smil
end type
type tabpage_all_smil from userobject within tab_dtb
cbx_smil_check cbx_smil_check
mle_allsmil mle_allsmil
end type
type tabpage_selected_smil from userobject within tab_dtb
end type
type cbx_selsmil_chk from checkbox within tabpage_selected_smil
end type
type mle_selsmils from multilineedit within tabpage_selected_smil
end type
type tabpage_selected_smil from userobject within tab_dtb
cbx_selsmil_chk cbx_selsmil_chk
mle_selsmils mle_selsmils
end type
type tabpage_dirlist from userobject within tab_dtb
end type
type cbx_dirlist from checkbox within tabpage_dirlist
end type
type lb_dirlist from listbox within tabpage_dirlist
end type
type tabpage_dirlist from userobject within tab_dtb
cbx_dirlist cbx_dirlist
lb_dirlist lb_dirlist
end type
type tabpage_shortresult from userobject within tab_dtb
end type
type mle_shortresult from multilineedit within tabpage_shortresult
end type
type tabpage_shortresult from userobject within tab_dtb
mle_shortresult mle_shortresult
end type
type tabpage_mediumresult from userobject within tab_dtb
end type
type mle_mediumresult from multilineedit within tabpage_mediumresult
end type
type tabpage_mediumresult from userobject within tab_dtb
mle_mediumresult mle_mediumresult
end type
type tabpage_longresult from userobject within tab_dtb
end type
type mle_longresult from multilineedit within tabpage_longresult
end type
type tabpage_longresult from userobject within tab_dtb
mle_longresult mle_longresult
end type
type tab_dtb from tab within w_qa_zedval
tabpage_reportxml tabpage_reportxml
tabpage_nlsreportxml tabpage_nlsreportxml
tabpage_opf tabpage_opf
tabpage_ncx tabpage_ncx
tabpage_all_smil tabpage_all_smil
tabpage_selected_smil tabpage_selected_smil
tabpage_dirlist tabpage_dirlist
tabpage_shortresult tabpage_shortresult
tabpage_mediumresult tabpage_mediumresult
tabpage_longresult tabpage_longresult
end type
end forward

global type w_qa_zedval from w_sheet
integer x = 214
integer y = 221
integer width = 3817
integer height = 2552
string title = "DTB QA Test Process"
boolean ib_disableclosequery = true
shl_1 shl_1
mle_comments mle_comments
cb_spell cb_spell
st_opf_ncx st_opf_ncx
mle_opf_ncx mle_opf_ncx
sle_multidisc sle_multidisc
cbx_multidisc cbx_multidisc
cb_qa cb_qa
lb_cdrom lb_cdrom
st_cdrom st_cdrom
sle_cntryr sle_cntryr
st_cntryr st_cntryr
sle_search sle_search
cb_search cb_search
cb_later cb_later
st_smil st_smil
st_status st_status
st_comments st_comments
cbx_playbook_check cbx_playbook_check
cb_t cb_t
cb_refresh cb_refresh
lb_smil lb_smil
cb_play cb_play
cb_reject cb_reject
cb_approve cb_approve
cb_exit cb_exit
tab_dtb tab_dtb
end type
global w_qa_zedval w_qa_zedval

type variables
Char dtb_status
// 03/06/2008 naming conventions for variables should be followed these are not local variables to begin with l...
String lc_cdrom,ls_cntr,ls_multidisk_dirlist,ls_dcdate,ls_dcformat,ls_multimediatype,ls_audioformat,ls_depth,ls_allnavlist,Luserid
Boolean Multi_disc = FALSE, Old_DTB_book = FALSE, ib_checkedin=FALSE
Integer lcnt_navpoint,TotalTime
long Old_DTB_bkseq
Date ld_produceddate
str_dtb_data lstr_db_data
string is_docname // 03/07/2008
n_qa_services inv_qa // 10/23/2008
string is_readonly // 10/27/2008
end variables

forward prototypes
public function boolean wf_isjawsrunning ()
public subroutine wf_extract_opf ()
public function integer wf_updatedb (long ll_bkseq)
public subroutine wf_get_blobs (long ll_bkseq)
public function integer wf_calc_tot_time (string ls_totaltime)
public subroutine wf_clear_tabs ()
public function integer of_refreshnewbook ()
public function integer of_checkincheck (long al_bkseq)
end prototypes

public function boolean wf_isjawsrunning ();boolean lb_return
String  	ls_WindowName

lb_return = false

//Check if Window Eyes is Running
ls_windowName = 'Window-Eyes'
IF FindWindowW (0, ls_windowName) <> 0 THEN 	
	lb_return = TRUE
END IF

Return lb_Return
end function

public subroutine wf_extract_opf ();//////////////////////////////////////////////////////////////////////////////////////////////////////////
//
//	Function: wf_extract_opf
// Args: None
// Returns : 1 for success, -1 for error
//	Description:
//	When the user selects a new book from a specified folder or path set that book as the
// book seq
//
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//	
//	Revision History
//
//	Developed by 	Date 				Version						Tracking#
//									
// Murali K.			03/07/2008      005 PICS Modifications	 Reqs: QAS a.4b.1, a.4b.2
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//

Integer li_opffile,li_ncxfile,rtn,lcnt = 0, lcnt_navlist = 0,li_revs=0,lcnt_navtarget = 0
Long li_startpos, li_endpos, li_pos
String ls_fletext ,ls_totaltime, ls_opffile,ls_hrs,ls_min,ls_narrator,ls_source
Date nulldate,tday,ld_revisiondate
String vs_source, vs_ncx,ls_navlist, ls_temp,ls_bkseq
Blob ncx_blob
n_cst_string 	inv_string

SetNull(nulldate)
tday = Today()

ls_bkseq = Mid(lb_smil.SelectedItem(), 1 , 5)

// 03/07/2008 if a new book set from selection
IF Isnull(ls_bkseq) OR Len(Trim(ls_bkseq)) = 0 THEN
	ls_bkseq = is_docname
END IF
///////////////////

IF Multi_disc = TRUE THEN
	vs_source = ls_multidisk_dirlist +"\"+ls_bkseq+".opf"
	vs_ncx = ls_multidisk_dirlist +"\"+ls_bkseq+".ncx"
ELSE
	vs_source = lc_cdrom +"\"+ls_bkseq+".opf"
	vs_ncx = lc_cdrom +"\"+ls_bkseq+".ncx"
END IF	

IF NOT(FileExists(vs_source)) THEN 
	Messagebox("ERROR in file open","OPF file does not exist")
	RETURN
END IF 
IF NOT(FileExists(vs_ncx)) THEN 
        Messagebox("ERROR in file open","NCX file does not exist")
	 RETURN
END IF 

li_opffile = FileOpen(vs_source, LineMode!  , read! , shared!, replace!)
li_ncxfile = FileOpen(vs_ncx, LineMode!  , read! , shared!, replace!)


DO WHILE (FileRead(li_opffile, ls_fletext) >= 0) 
     IF Pos (ls_fletext,'<dc:Date>') > 1 THEN 
	    li_startpos = Pos (ls_fletext,'<dc:Date>') + 9
	    li_endpos = Pos (ls_fletext,'</dc:Date>')
	    li_pos = li_endpos - li_startpos
           ls_dcdate = Mid (ls_fletext, li_startpos , li_pos) 
//	    Messagebox("dc:Date",ls_dcdate)
    ELSEIF Pos (ls_fletext,'<dc:Format>') > 1 THEN 
	    li_startpos = Pos (ls_fletext,'<dc:Format>') + 11
	    li_endpos = Pos (ls_fletext,'</dc:Format>')
	    li_pos = li_endpos - li_startpos
           ls_dcformat = Mid (ls_fletext, li_startpos , li_pos) 
//	    Messagebox("dc:Format",ls_dcformat)
    ELSEIF Pos (ls_fletext,'multimediaType') > 1 THEN 
	    li_startpos = Pos (ls_fletext,'multimediaType') + 25
	    li_endpos = Pos (ls_fletext,'~"/>')
	    li_pos = li_endpos - li_startpos
           ls_multimediatype = Mid (ls_fletext, li_startpos , li_pos) 
//	    Messagebox("dtb:multimediatype",ls_multimediatype)
    ELSEIF Pos (ls_fletext,'dtb:narrator') > 1 THEN 
	    li_startpos = Pos (ls_fletext,'narrator') + 19
	    li_endpos = Pos (ls_fletext,'~"/>')
	    li_pos = li_endpos - li_startpos
           ls_narrator = Mid (ls_fletext, li_startpos , li_pos) 
//	    Messagebox("dtb:narrator",ls_narrator)
    ELSEIF Pos (ls_fletext,'producedDate') > 1 THEN 
	    li_startpos = Pos (ls_fletext,'producedDate') + 23
	    li_endpos = Pos (ls_fletext,'~"/>')
	    li_pos = li_endpos - li_startpos
           ld_produceddate = Date(Mid (ls_fletext, li_startpos , li_pos))
//	    Messagebox("dtb:produceddate",string(ld_produceddate,'mm/dd/yyyy'))
    ELSEIF Pos (ls_fletext,'revisionDate') > 1 THEN 
	    li_startpos = Pos (ls_fletext,'revisionDate') + 23
	    li_endpos = Pos (ls_fletext,'~"/>')
	    li_pos = li_endpos - li_startpos
           ld_revisiondate = Date(Mid (ls_fletext, li_startpos , li_pos))
// "dtb:revision" content="0"
    ELSEIF Pos (ls_fletext,'dtb:revision~"') > 1 THEN 
	    li_startpos = Pos (ls_fletext,'dtb:revision~"') + 23
	    li_endpos = Pos (ls_fletext,'~"/>')
	    li_pos = li_endpos - li_startpos
           li_revs = Integer(Mid (ls_fletext, li_startpos , li_pos))
//   	    Messagebox("dtb:revision",string(li_revs))
    ELSEIF Pos (ls_fletext,'totalTime') > 1 THEN 	
	    li_startpos = Pos (ls_fletext,'totalTime') + 20
	    li_endpos = Pos (ls_fletext,'~"/>')
	    li_pos = li_endpos - li_startpos
           ls_totaltime = Mid (ls_fletext, li_startpos , 11) 
	    TotalTime = wf_calc_tot_time(ls_totaltime)
    ELSEIF Pos (ls_fletext,'audioFormat') > 1 THEN 
	    li_startpos = Pos (ls_fletext,'audioFormat') + 22
	    li_endpos = Pos (ls_fletext,'~"/>')
	    li_pos = li_endpos - li_startpos
           ls_audioformat = Mid (ls_fletext, li_startpos , li_pos) 
//	    Messagebox("dtb:audioFormat",ls_audioformat)
     END IF 
LOOP 

DO WHILE (FileRead(li_ncxfile, ls_fletext) >= 0) 
     IF Pos (ls_fletext,'depth') > 1 THEN 
	    li_startpos = Pos (ls_fletext,'depth') + 16
           ls_depth = Mid (ls_fletext, li_startpos , 1) 
     END IF 
LOOP 

// Navigation Point count
FileClose(li_ncxfile) 
li_ncxfile = FileOpen(vs_ncx, StreamMode!  , read! , LockRead!, replace!)

ncx_blob = Blob(vs_ncx, EncodingUTF16LE!)    // convert 

FileRead(li_ncxfile, ncx_blob)
ls_source = lower(String(ncx_blob, EncodingANSI!))
lcnt_navpoint = inv_string.of_CountOccurrences(ls_source,  '/navpoint', TRUE)
lcnt_navlist = inv_string.of_CountOccurrences(ls_source,  '/navlist', TRUE)
lcnt_navtarget = inv_string.of_CountOccurrences(ls_source,  '/navtarget', TRUE)

FileClose(li_ncxfile) 

// Navigation List count
//FileClose(li_ncxfile) 
//li_ncxfile = FileOpen(vs_ncx, LineMode!  , read! , shared!, replace!)
//
//DO WHILE (FileRead(li_ncxfile, ls_fletext) >= 0) 
//     IF Pos (lower(ls_fletext),'/navlist') > 1 THEN 
//	    lcnt_navlist += 1
//     END IF 
//LOOP 

// Navigation Target count
//FileClose(li_ncxfile) 
//li_ncxfile = FileOpen(vs_ncx, LineMode!  , read! , shared!, replace!)
//
//DO WHILE (FileRead(li_ncxfile, ls_fletext) >= 0) 
//     IF Pos (lower(ls_fletext),'/navtarget') > 1 THEN 
//	    lcnt_navtarget += 1
//     END IF 
//LOOP 



mle_opf_ncx.text += 'dc:Date = '+ ls_dcdate + '~r~n'
mle_opf_ncx.text += 'dc:Format = '+ ls_dcformat + '~r~n'
mle_opf_ncx.text += 'dtb:multimediaType = '+ ls_multimediatype + '~r~n'
mle_opf_ncx.text += 'dtb:narrator = '+ ls_narrator + '~r~n'
mle_opf_ncx.text += 'dtb:producedDate = '+ String(ld_produceddate,'mm/dd/yyyy') + '~r~n'
IF li_revs > 0  THEN
	mle_opf_ncx.text += 'dtb:revision = '+ String(li_revs) + '~r~n'
	mle_opf_ncx.text += 'dtb:revisionDate = '+ String(ld_revisiondate,'mm/dd/yyyy') + '~r~n'
END IF
//IF String(ld_revisiondate,'mm/dd/yyyy') = '01/01/1900' THEN
//	mle_opf_ncx.text+= 'dtb:revisionDate = '+ ls_dcdate + '~r~n'
//ELSE
//	mle_opf_ncx.text += 'dtb:revisionDate = '+ String(ld_revisiondate,'mm/dd/yyyy') + '~r~n'
//END IF
//mle_opf_ncx.text += 'dtb:totalTime = '+ ls_totaltime + '~r~n'
mle_opf_ncx.text += 'dtb:totalTime = '+ String(TotalTime) + ' Minutes ~r~n'
mle_opf_ncx.text += 'dtb:audioFormat = '+ ls_audioformat + '~r~n'
mle_opf_ncx.text += 'dtb:depth = '+ ls_depth + '~r~n'
//IF IsNull (ls_allnavlist) OR ls_allnavlist = "" THEN
//	mle_opf_ncx.text += 'Navigation List = None~r~n'
//ELSE
mle_opf_ncx.text += 'Number of navigation points = '+ String(lcnt_navpoint) + '~r~n'
mle_opf_ncx.text += 'Number of navigation lists = '+ String(lcnt_navlist) + '~r~n'
mle_opf_ncx.text += 'Number of navigation targets = '+ String(lcnt_navtarget) + '~r~n'
//END IF

FileClose(li_opffile) 
FileClose(li_ncxfile) 

end subroutine

public function integer wf_updatedb (long ll_bkseq);//////////////////////////////////////////////////////////////////////////////////////////////////////////
//
//	Function: wf_updatedb
//
//	Description: Update or Insert DTB Data 
//	
//
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//	
//	Revision History
//
//	Developed by 	Date 				Version						Tracking#
//									
// Murali K.			06/30/2008      Process short, med and long results. Phase-2
// Murali K.			11/06/2008    Update limited info for checked in books, for older books update full data
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//
Integer fh_opffile,fh_ncxfile,fh_report,fh_nlsreport,fh_dirlist,fh_smil,fh_sum,rtn,lcnt = 0,total, n
Date nulldate,tday, ld_audit
String vs_source, vs_ncx, vs_report, vs_nlsreport, vs_dirlist[], all_dirlist, vs_smil, vs_sum, ls_comments,ls_bkseq, ls_short_result, ls_medium_result, ls_long_result, ls_user
Blob opf_blob,ncx_blob,report_blob,nlsreport_blob,smil_blob,sum_blob
blob med_blob, long_blob
Char lc_opf_chk,lc_ncx_chk,lc_smil_chk,lc_reportxml_chk,lc_nlsreportxml_chk,lc_playbook_chk,lc_dirlist_chk,lc_dtb_status
Long lrtn


//MessageBox('bkseq',string(ll_bkseq))
rtn = Messagebox("DTB Update","Do you want to update database? If yes, please view tab pages (Report XML, NLS Report XML, OPF File, NCX File and All SMILs), prior to updating database. ",question!,yesNo!,1)
IF rtn = 1 THEN
	
	ls_comments = mle_comments.text
	ls_user = gnv_app.of_getuserid()
	ld_audit = today()
	
	// 06/30/2008 Update short result
	ls_short_result = tab_dtb.tabpage_shortresult.mle_shortresult.text
	
	IF dtb_status = 'A' THEN
		lc_dtb_status = 'A'
	ELSEIF dtb_status = 'R' THEN
		lc_dtb_status = 'R'
	ELSEIF  dtb_status = 'H' THEN
		lc_dtb_status = 'H'
	END IF 

	IF tab_dtb.tabpage_opf.cbx_opf_check.checked = TRUE THEN
		lc_opf_chk = 'Y'
	ELSE
		lc_opf_chk = 'N'
	END IF
		
	IF cbx_playbook_check.checked = TRUE THEN
		lc_playbook_chk = 'Y'
	ELSE
		lc_playbook_chk = 'N'
	END IF

	IF  tab_dtb.tabpage_all_smil.cbx_smil_check.checked = TRUE THEN
		lc_smil_chk = 'Y'
	ELSE
		lc_smil_chk = 'N'
	END IF

	IF  tab_dtb.tabpage_ncx.cbx_ncx_check.checked = TRUE THEN
		lc_ncx_chk = 'Y'
	ELSE
		lc_ncx_chk = 'N'
	END IF

	IF  tab_dtb.tabpage_nlsreportxml.cbx_nlsreportxml_check.checked = TRUE THEN
		lc_nlsreportxml_chk = 'Y'
	ELSE
		lc_nlsreportxml_chk = 'N'
	END IF

	IF  tab_dtb.tabpage_reportxml.cbx_reportxml_check.checked = TRUE THEN
		lc_reportxml_chk = 'Y'
	ELSE
		lc_reportxml_chk = 'N'
	END IF

	IF  tab_dtb.tabpage_dirlist.cbx_dirlist.checked = TRUE THEN
		lc_dirlist_chk = 'Y'
	ELSE
		lc_dirlist_chk = 'N'
	END IF

	tday = Today()

//	IF Old_DTB_book = FALSE THEN
//		ls_bkseq = Mid(lb_smil.SelectedItem(), 1 , 5)
//	END IF
//	
	// If Multi disk is true and it is not an old book
	//IF Multi_disc = TRUE  AND Old_DTB_book = FALSE THEN
	//	vs_source = ls_multidisk_dirlist +"\"+ls_bkseq+".opf"
	//	vs_ncx = ls_multidisk_dirlist +"\"+ls_bkseq+".ncx"
	//ELSEIF Multi_disc = FALSE  AND Old_DTB_book = FALSE THEN
		//lc_cdrom = lb_cdrom.SelectedItem()
		
		// If the cdrom still not selected then
		//IF IsNull(lc_cdrom) THEN
		//	lb_cdrom.SelectItem('E:', 1)
		//	lc_cdrom = 'E:'
		//END IF
		//Assign the sources for OPF and NCX files
		//vs_source = lc_cdrom +"\"+ls_bkseq+".opf"
		//vs_ncx = lc_cdrom +"\"+ls_bkseq+".ncx"
		vs_source =tab_dtb.tabpage_opf.mle_opf.text
		IF ISNull(vs_source) OR vs_source = "" THEN
			MessageBox("OPF File"," The content of the OPF file is empty. Do you want to continue with update?",Question!,YesNo!,1)
			IF rtn = 2 THEN
				RETURN -1
			END IF
		END IF			
		vs_ncx =tab_dtb.tabpage_ncx.mle_ncx.text
		IF ISNull(vs_ncx) OR vs_ncx = "" THEN
			MessageBox("NCX File"," The content of the NCX file is empty. Do you want to continue with update?",Question!,YesNo!,1)
			IF rtn = 2 THEN
				RETURN -1
			END IF
		END IF			
	//END IF	

	// Content of the directory list
	all_dirlist = ' '
	total = tab_dtb.tabpage_dirlist.lb_dirlist.TotalItems()
	FOR n = 1 TO total
		vs_dirlist[n] = tab_dtb.tabpage_dirlist.lb_dirlist.Text(n)
		all_dirlist = all_dirlist + vs_dirlist[n] + "~n"
	NEXT
       all_dirlist = TRIM(all_dirlist)	
	//MessageBox("dirlist",all_dirlist)
	
     // If this is not a book selected from the list of the existing books then start assigning the sources 
     // IF Old_dTB_book = FALSE THEN
       	// Assign the sources for ZEDVAL and NLSVAL reports
		//vs_report = "C:\report.xml"
		//vs_nlsreport = "C:\nlsreport.xml"
		vs_report = tab_dtb.tabpage_reportxml.mle_reportxml.text
		IF ISNull(vs_report) OR vs_report = "" THEN
			MessageBox("Tap page"," The Report XML tab page is empty, Do you want to continue with update?",Question!,YesNo!,1)
			IF rtn = 2 THEN
				RETURN -1
			END IF
		END IF			
		vs_nlsreport = tab_dtb.tabpage_nlsreportxml.mle_nlsreportxml.text
		IF ISNull(vs_nlsreport) OR vs_nlsreport = "" THEN
			MessageBox("Tap page"," The NLS Report XML tab page is empty, Do you want to continue with update?",Question!,YesNo!,1)
			IF rtn = 2 THEN
				RETURN -1
			END IF
		END IF			
		// Content of the Extract OPF and NCX file
		vs_sum = mle_opf_ncx.text		
		IF ISNull(vs_sum) OR vs_sum = "" THEN
			rtn = MessageBox("Extracted Data"," The extracted data from OPF and NCX is empty, Do you want to continue with update?",Question!,YesNo!,1)
			IF rtn = 2 THEN
				RETURN -1
			END IF
		END IF			
		// Content of the SMIL files
		vs_smil = tab_dtb.tabpage_all_smil.mle_allsmil.text
		IF ISNull(vs_smil) OR vs_smil = "" THEN
			MessageBox("Smil File"," The content of the All SMILs tab page is empty. Do you want to continue with update?",Question!,YesNo!,1)
			IF rtn = 2 THEN
				RETURN -1
			END IF
		END IF			
		
//		IF NOT(FileExists(vs_source)) THEN 
//			Messagebox("ERROR in file open","OPF file does not exist. CD may not be in place.")
//			RETURN -1
//		END IF 
//		IF NOT(FileExists(vs_ncx)) THEN 
//				  Messagebox("ERROR in file open","NCX file does not exist. CD may not be in place.")
//			 RETURN -1
//		END IF 
//		IF NOT(FileExists(vs_report)) THEN 
//				  Messagebox("ERROR in file open","Report.XML file does not exist")
//			 RETURN -1
//		END IF 
//		IF NOT(FileExists(vs_nlsreport)) THEN 
//				  Messagebox("ERROR in file open","NLSReprot.XML file does not exist")
//			 RETURN -1
//		END IF 
	//END IF

	// 06/30/2008 set medium and long result for updates
	ls_medium_result = tab_dtb.tabpage_mediumresult.mle_mediumresult.text
	ls_long_result 	   = tab_dtb.tabpage_longresult.mle_longresult.text
	
	IF IsNull(ls_dcdate) or ls_dcdate = "" THEN
		ls_dcdate = lstr_db_data.dc_date
	END IF
	
	IF IsNull(ls_dcformat) or ls_dcformat = "" THEN
		ls_dcformat = lstr_db_data.dc_format
	END IF	
	
	IF IsNull(ls_multimediatype) or ls_multimediatype = "" THEN
		ls_multimediatype = lstr_db_data.dtb_multimediatype
	END IF	

	IF IsNull(ld_produceddate)  THEN
		ld_produceddate = lstr_db_data.dtb_produceddate
	END IF	

	IF IsNull(TotalTime) THEN
		TotalTime = lstr_db_data.dtb_totaltime
	END IF	

	IF IsNull(ls_audioformat) or ls_audioformat = "" THEN
		ls_audioformat = lstr_db_data.dtb_audioformat
	END IF	

	IF IsNull(ls_depth) or ls_depth = "" THEN
		ls_depth = lstr_db_data.dtb_depth
	END IF	

	IF IsNull(lcnt_navpoint) THEN
		lcnt_navpoint = lstr_db_data.navpoint
	END IF	


	// 02/15/2008 test
     // Check to see if the book exist
     SELECT count(*) INTO :lcnt
	FROM dtb_data
  	where bkseq=:ll_bkseq and bkmed='DB'
	USING SqlServerTrans;


	// Book does not exist at all, insert it.
	IF lcnt = 0 THEN		
		// 06/30/2008 Add short result to the insert
		INSERT INTO dtb_data
			 (bkseq, bkmed, dc_date, dc_format,  dtb_multimediaType, dtb_producedDate, &
			 dtb_totalTime, dtb_audioFormat, dtb_depth, navpoints, dt_time , &
			 opf_chk, ncx_chk, smil_chk, reportxml_chk, nlsreportxml_chk, playbook_chk, dirlist_chk, comments, status, dtb_navlist, qainit,short_result, created_by,created_date)
		values
			 (:ll_bkseq,  'DB', :ls_dcdate, :ls_dcformat, :ls_multimediatype, :ld_produceddate, &
			 :TotalTime, :ls_audioformat, :ls_depth, :lcnt_navpoint, :tday, &
			 :lc_opf_chk, :lc_ncx_chk, :lc_smil_chk, :lc_reportxml_chk, :lc_nlsreportxml_chk, :lc_playbook_chk, :lc_dirlist_chk, :ls_comments, :lc_dtb_status, :ls_allnavlist, :Luserid, :ls_short_result, & 
			 :ls_user, sysdate)
		 USING SqlServerTrans;
		 IF f_check_dberror(SqlServerTrans,"Insert into dtb_data ") = FALSE THEN
			ROLLBACK USING SqlServerTrans;
			Messagebox("Error in Insert","Record was not inserted into dtb_data table")
			RETURN -1
		END IF
	// Book exist in the DTB_DATA table 
	// 11/06/2008 
	ELSEIF		(NOT ib_checkedin) THEN
		// Row exist, update the dtb data first and then update blob fields
		// 06/30/2008 add short result to the updates
		UPDATE dtb_data
		set 	dc_date = :ls_dcdate , dc_format = :ls_dcformat,  dtb_multimediaType = :ls_multimediatype, dtb_producedDate = :ld_produceddate,  	
			dtb_totalTime = :TotalTime, dtb_audioFormat = :ls_audioformat, dtb_depth = :ls_depth, navpoints = :lcnt_navpoint, dt_time = :tday,
			opf_chk = :lc_opf_chk, ncx_chk = :lc_ncx_chk, smil_chk = :lc_smil_chk, reportxml_chk = :lc_reportxml_chk, nlsreportxml_chk = :lc_nlsreportxml_chk, 
			playbook_chk = :lc_playbook_chk, dirlist_chk = :lc_dirlist_chk, comments = :ls_comments, status = :lc_dtb_status, dtb_navlist = :ls_allnavlist, qainit = :Luserid,
			short_result = :ls_short_result, modified_by = :ls_user, modified_date = sysdate
		where bkseq = :ll_bkseq
		AND bkmed = 'DB'
		 USING SqlServerTrans;
		 IF f_check_dberror(SqlServerTrans,"Update into dtb_data ") = FALSE THEN
			ROLLBACK USING SqlServerTrans;
			Messagebox("Error in Update","Record was not updated into dtb_data table (ERROR in Update dtb_data)")
			RETURN -1
//		ELSE
//			COMMIT USING SQLSERVERTRANS ;
//			Messagebox('Update','Update successful')
		END IF
	ELSEIF ib_checkedin THEN // 11/06/2008 UPDATE STATUS AND DATE FOR CHECKED IN BOOKS
			UPDATE dtb_data
		set 	dt_time = sysdate , status = :lc_dtb_status,  qainit = :Luserid,  modified_by = :ls_user, modified_date = sysdate
		where bkseq = :ll_bkseq
		AND bkmed = 'DB'
		 USING SqlServerTrans;
		 IF f_check_dberror(SqlServerTrans,"Update into dtb_data ") = FALSE THEN
			ROLLBACK USING SqlServerTrans;
			Messagebox("Error in Update","Record was not updated into dtb_data table (ERROR in Update dtb_data)")
			RETURN -1
		ELSE
			COMMIT USING SQLSERVERTRANS ;
			Messagebox('Update','Update successful')
		END IF
	END IF // IF lcnt = 0
	
			// Blob Updates
	// 11/6/2008 UPDATE ONLY FOR OLD BOOKS			
	 IF NOT ib_checkedin THEN
	
			opf_blob = Blob(vs_source, EncodingUTF16LE!)    // convert 
			// ANSI blob to Unicode 

	       	UPDATEBLOB dtb_data SET opf_file = :opf_blob
			where bkseq = :ll_bkseq
			AND bkmed = 'DB'
			USING SqlServerTrans;
			IF SqlServerTrans.sqlNRows > 0 THEN
				//fh_ncxfile = FileOpen(vs_ncx, streamMode!)
				//IF fh_opffile <> -1 THEN
					//FileRead(fh_ncxfile, ncx_blob)
					//FileClose(fh_ncxfile)
					
					//ncx_blob = Blob(String(ncx_blob, EncodingANSI!), EncodingUTF16LE!)    // convert 
					ncx_blob = Blob(vs_ncx, EncodingUTF16LE!)    // convert 
					// ANSI blob to Unicode 
					
					UPDATEBLOB dtb_data SET ncx_file = :ncx_blob
					where bkseq = :ll_bkseq
					AND bkmed = 'DB'
					USING SqlServerTrans;
					IF SqlServerTrans.sqlNRows > 0 THEN
						//fh_report = FileOpen(vs_report, streamMode!)
						//IF fh_report <> -1 THEN
							//FileRead(fh_report, report_blob)
							//FileClose(fh_report)
							
							//report_blob = Blob(String(report_blob, EncodingANSI!), EncodingUTF16LE!)    // convert 
							report_blob = Blob(vs_report, EncodingUTF16LE!)    // convert 
							// ANSI blob to Unicode 
							
							UPDATEBLOB dtb_data SET zedval = :report_blob
							where bkseq = :ll_bkseq
							AND bkmed = 'DB'
							USING SqlServerTrans;
							IF SqlServerTrans.sqlNRows > 0 THEN
								//fh_nlsreport = FileOpen(vs_nlsreport, streamMode!)
								//IF fh_nlsreport <> -1 THEN
									//FileRead(fh_nlsreport, nlsreport_blob)
									//FileClose(fh_nlsreport)
									
									//nlsreport_blob = Blob(String(nlsreport_blob, EncodingANSI!), EncodingUTF16LE!)    // convert 
									nlsreport_blob = Blob(vs_nlsreport, EncodingUTF16LE!)    // convert 
									// ANSI blob to Unicode 
									
									UPDATEBLOB dtb_data SET nlsval = :nlsreport_blob
									where bkseq = :ll_bkseq
									AND bkmed = 'DB'
									USING SqlServerTrans;
										IF SqlServerTrans.sqlNRows > 0 THEN
											sum_blob = Blob(vs_sum, EncodingUTF16LE!)    // convert 
											// ANSI blob to Unicode 
											UPDATEBLOB dtb_data SET opf_ncx_sum = :sum_blob
											where bkseq = :ll_bkseq
											AND bkmed = 'DB'
											USING SqlServerTrans;
											IF SqlServerTrans.sqlNRows > 0 THEN
												smil_blob = Blob(vs_smil, EncodingUTF16LE!)    // convert 
												// ANSI blob to Unicode 
													
												UPDATEBLOB dtb_data SET smil_file = :smil_blob
												where bkseq = :ll_bkseq
												AND bkmed = 'DB'
												USING SqlServerTrans;
												IF SqlServerTrans.sqlNRows > 0 THEN
													// Directory listing
//													dirlist_blob = Blob(all_dirlist, EncodingUTF16LE!)    // convert 
													// ANSI blob to Unicode 
														
													UPDATE dtb_data SET dirlist_file = :all_dirlist
													where bkseq = :ll_bkseq
													AND bkmed = 'DB'
													USING SqlServerTrans;
														
			////////////////// 06/30/2008 Update medium and long result clob columns
														med_blob = Blob(ls_medium_result, EncodingUTF16LE!)    // convert 
													
														UPDATEBLOB dtb_data SET medium_result = :med_blob
														where bkseq = :ll_bkseq
														AND bkmed = 'DB'
														USING SqlServerTrans;
														
														long_blob = Blob(ls_long_result, EncodingUTF16LE!)    // convert 
													
														UPDATEBLOB dtb_data SET long_result = :long_blob
														where bkseq = :ll_bkseq
														AND bkmed = 'DB'
														USING SqlServerTrans;

				////////////////////// 06/30/2008
																											
														IF SqlServerTrans.sqlNRows > 0 THEN
										
														// Commit all works													
														COMMIT USING SqlServerTrans;
														Messagebox("Insert","Record inserted/updated into dtb_data table")
													ELSE
														// Rollback from updating DIRLIST file
														ROLLBACK USING SqlServerTrans;
														Messagebox("Error in Insert","Record was not inserted/updated into dtb_data table (DIRLIST File ERROR)")
														RETURN -1
													END IF
												ELSE
													// Rollback from updating SMIL file
													ROLLBACK USING SqlServerTrans;
													Messagebox("Error in Insert","Record was not inserted/updated into dtb_data table (SMIL File ERROR)")
													RETURN -1
												END IF
											ELSE
												// Rollback from updating OPF and NCX summary file
												ROLLBACK USING SqlServerTrans;
												Messagebox("Error in Insert","Record was not inserted/updated into dtb_data table (EXTRACTED DATA ERROR)")
												RETURN -1
											END IF
										ELSE
											// Rollback from updating dirlist file
											ROLLBACK USING SqlServerTrans;
											Messagebox("Error in Insert","Record was not inserted/updated into dtb_data table (NLSREPORT ERROR)")
											RETURN -1
										END IF
								//END IF
							ELSE
								// Rollback from updating ZEDVAL file
								ROLLBACK USING SqlServerTrans;
								Messagebox("Error in Insert","Record was not inserted/updated into dtb_data table (ZEDVAL ERROR)")
								RETURN -1
							END IF
						//END IF
					ELSE 
						// Rollback from updating NCX file
						ROLLBACK USING SqlServerTrans;
						Messagebox("Error in Insert","Record was not inserted/updated into dtb_data table (NCX ERROR)")
						RETURN -1
					END IF
				//END IF
		 	ELSE
				// Rollback from updating OPF file
				ROLLBACK USING SqlServerTrans;
				Messagebox("Error in Insert","Record was not inserted/updated into dtb_data table (OPF ERROR)")
				RETURN -1
			END IF
		//END IF
END IF  // if not checked in

END IF // Do you want to update

// Close the file in case these files are still open
//FileClose(fh_opffile) 
//FileClose(fh_ncxfile) 
//FileClose(fh_report) 
//FileClose(fh_nlsreport) 

RETURN 1
end function

public subroutine wf_get_blobs (long ll_bkseq);//////////////////////////////////////////////////////////////////////////////////////////////////////////
//
//	Function: wf_get_blobs
//
//	Description: Select blob field values from DTB Data for a given book
//	
//
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//	
//	Revision History
//
//	Developed by 	Date 				Version						Tracking#
//									
// Murali K.			06/30/2008      Process medium and long results blobs. Phase-2
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//

Blob report_blob, nlsreport_blob, opf_blob, ncx_blob, sum_blob, smil_blob, medium_result_blob, long_result_blob
Boolean Dlist = FALSE,bjaws
String ls_text,ls_txtlines[ ], dirlist_file,ls_null
Integer li_numstats,i,rtn
n_cst_string 	inv_string

SetNull(ls_text)
setnull(ls_null)

bjaws = wf_isjawsrunning()
IF bjaws = TRUE THEN
	of_SetJaws(TRUE)
ELSE
	of_SetJaws(FALSE)
END IF

// ZEDVAL Report
SELECTBLOB ZEDVAL
        INTO  :report_blob
        FROM DTB_DATA
        where BKSEQ = :ll_bkseq
        USING SQLServerTrans ;
IF SqlServerTrans.sqlNRows > 0 AND NOT(IsNull(report_blob)) THEN
	// 07/16/2008 remove special characters
	ls_text = String(report_blob)
	li_numstats = inv_string.of_Parsetoarray (ls_text, "~n", ls_txtlines)
	FOR i = 1 TO li_numstats
		ls_txtlines[i] = inv_string.of_RemoveNonPrint(ls_txtlines[i])
		ls_txtlines[i] = inv_string.of_Trim(ls_txtlines[i])
	NEXT
	rtn = inv_string.of_ArrayToString(ls_txtlines,"~r~n",ls_text)

	tab_dtb.tabpage_reportxml.mle_reportxml.text = ls_text // String(report_blob)
	
ELSEIF 	IsNull(report_blob) THEN
	tab_dtb.tabpage_reportxml.mle_reportxml.text = ls_null
	Messagebox("ERROR SELECTING BLOB"," ZEDVAL report.is NULL")
ELSE
	Messagebox("ERROR SELECTING BLOB"," Error in selecting ZEDVAL report.")
END IF

// NLSVAL Report
SELECTBLOB NLSVAL
        INTO  :nlsreport_blob
        FROM DTB_DATA
        where BKSEQ = :ll_bkseq
        USING SQLServerTrans ;
IF SqlServerTrans.sqlNRows > 0 AND NOT(IsNull(nlsreport_blob))THEN
	// 07/16/2008 remove special characters
	ls_text = String(nlsreport_blob)
	li_numstats = inv_string.of_Parsetoarray (ls_text, "~n", ls_txtlines)
	FOR i = 1 TO li_numstats
		ls_txtlines[i] = inv_string.of_RemoveNonPrint(ls_txtlines[i])
		ls_txtlines[i] = inv_string.of_Trim(ls_txtlines[i])
	NEXT
	rtn = inv_string.of_ArrayToString(ls_txtlines,"~r~n",ls_text)


	tab_dtb.tabpage_nlsreportxml.mle_nlsreportxml.text = ls_text // String(nlsreport_blob)
ELSEIF 	IsNull(nlsreport_blob) THEN
	tab_dtb.tabpage_nlsreportxml.mle_nlsreportxml.text = ls_null 
	Messagebox("ERROR SELECTING BLOB"," NLSVAL report.is NULL")
ELSE
	Messagebox("ERROR SELECTING BLOB"," Error in selecting NLSVAL report.")
END IF

// OPF File
SELECTBLOB OPF_FILE
        INTO  :opf_blob
        FROM DTB_DATA
        where BKSEQ = :ll_bkseq
        USING SQLServerTrans ;
IF SqlServerTrans.sqlNRows > 0 AND NOT(IsNull(opf_blob))THEN
	
	ls_text = String(opf_blob)
	li_numstats = inv_string.of_Parsetoarray (ls_text, "~n", ls_txtlines)
	FOR i = 1 TO li_numstats
		ls_txtlines[i] = inv_string.of_RemoveNonPrint(ls_txtlines[i])
		ls_txtlines[i] = inv_string.of_Trim(ls_txtlines[i])
	NEXT
	rtn = inv_string.of_ArrayToString(ls_txtlines,"~r~n",ls_text)
	tab_dtb.tabpage_opf.mle_opf.text = ls_text

	FOR i = 1 TO li_numstats
		SetNull(ls_txtlines[i])
	NEXT
	
ELSEIF 	IsNull(opf_blob) THEN
	tab_dtb.tabpage_opf.mle_opf.text = ls_null
	Messagebox("ERROR SELECTING BLOB"," OPF File.is NULL")
ELSE
	Messagebox("ERROR SELECTING BLOB"," Error in selecting OPF File.")
END IF

SetNull(ls_text)
li_numstats = 0

// NCX File
SELECTBLOB NCX_FILE
        INTO  :ncx_blob
        FROM DTB_DATA
        where BKSEQ = :ll_bkseq
        USING SQLServerTrans ;
IF SqlServerTrans.sqlNRows > 0 AND NOT(IsNull(ncx_blob))THEN
	ls_text = String(ncx_blob)
	li_numstats = inv_string.of_Parsetoarray (ls_text, "~n", ls_txtlines)
	FOR i = 1 TO li_numstats
		ls_txtlines[i] = inv_string.of_RemoveNonPrint(ls_txtlines[i])
		ls_txtlines[i] = inv_string.of_Trim(ls_txtlines[i])
	NEXT
	rtn = inv_string.of_ArrayToString(ls_txtlines,"~r~n",ls_text)
	tab_dtb.tabpage_ncx.mle_ncx.text = ls_text
//Reset the arrary of text
	FOR i = 1 TO li_numstats
		SetNull(ls_txtlines[i])
	NEXT

ELSEIF 	IsNull(ncx_blob) THEN
	tab_dtb.tabpage_ncx.mle_ncx.text = ls_null
	Messagebox("ERROR SELECTING BLOB"," NCX File is NULL")
ELSE
	Messagebox("ERROR SELECTING BLOB"," Error in selecting NCX File.")
END IF

// DirList File
SELECT DIRLIST_FILE
        INTO  :dirlist_file
        FROM DTB_DATA
        where BKSEQ = :ll_bkseq
        USING SQLServerTrans ;
IF SqlServerTrans.sqlNRows > 0 AND NOT(IsNull(dirlist_file))THEN
	li_numstats = inv_string.of_Parsetoarray (dirlist_file, "~n", ls_txtlines)
	FOR i = 1 TO li_numstats
		ls_txtlines[i] = inv_string.of_RemoveNonPrint(ls_txtlines[i])
		ls_txtlines[i] = inv_string.of_Trim(ls_txtlines[i])
		tab_dtb.tabpage_dirlist.lb_dirlist.InsertItem(ls_txtlines[i], i)
	NEXT
	rtn = inv_string.of_ArrayToString(ls_txtlines,"~r~n",dirlist_file)
END IF

// OPF_NCX_SUM File
SELECTBLOB OPF_NCX_SUM
        INTO  :sum_blob
        FROM DTB_DATA
        where BKSEQ = :ll_bkseq
        USING SQLServerTrans ;
IF SqlServerTrans.sqlNRows > 0 AND NOT(IsNull(sum_blob))THEN
	mle_opf_ncx.text = String(sum_blob)
ELSEIF 	IsNull(sum_blob) THEN
	mle_opf_ncx.text = ls_null
	Messagebox("ERROR SELECTING BLOB"," Extracted data from OPF and NCX files is NULL")
ELSE
	Messagebox("ERROR SELECTING BLOB"," Error in Extracted data from OPF and NCX files.")
END IF

SetNull(ls_text)
li_numstats = 0

// SMIL File
SELECTBLOB SMIL_FILE
        INTO  :smil_blob
        FROM DTB_DATA
        where BKSEQ = :ll_bkseq
        USING SQLServerTrans ;
IF SqlServerTrans.sqlNRows > 0 AND NOT(IsNull(smil_blob))THEN
	ls_text = String(smil_blob)
	tab_dtb.tabpage_all_smil.mle_allsmil.text = ls_text
ELSEIF 	IsNull(smil_blob) THEN
	tab_dtb.tabpage_all_smil.mle_allsmil.text = ls_null
	Messagebox("ERROR SELECTING BLOB"," SMIL File is NULL")
ELSE
	Messagebox("ERROR SELECTING BLOB"," Error in selecting SMIL File.")
END IF

// 06/30/2008 Medium Result  
SELECTBLOB MEDIUM_RESULT
        INTO  :medium_result_blob
        FROM DTB_DATA
        where BKSEQ = :ll_bkseq
        USING SQLServerTrans ;
IF SqlServerTrans.sqlNRows > 0 AND NOT(IsNull(medium_result_blob))THEN
	// 07/16/2008 remove special characters
	ls_text = String(medium_result_blob)
	li_numstats = inv_string.of_Parsetoarray (ls_text, "~n", ls_txtlines)
	FOR i = 1 TO li_numstats
		ls_txtlines[i] = inv_string.of_RemoveNonPrint(ls_txtlines[i])
		ls_txtlines[i] = inv_string.of_Trim(ls_txtlines[i])
	NEXT
	rtn = inv_string.of_ArrayToString(ls_txtlines,"~r~n",ls_text)
	tab_dtb.tabpage_mediumresult.mle_mediumresult.text = ls_text
ELSEIF 	IsNull(medium_result_blob) THEN
	tab_dtb.tabpage_mediumresult.mle_mediumresult.text = ls_null
	Messagebox("ERROR SELECTING BLOB"," Medium Result  is NULL")
ELSE
	Messagebox("ERROR SELECTING BLOB"," Error in selecting Medium Result.")
END IF

// Long result
SELECTBLOB LONG_RESULT
        INTO  :long_result_blob
        FROM DTB_DATA
        where BKSEQ = :ll_bkseq
        USING SQLServerTrans ;
IF SqlServerTrans.sqlNRows > 0 AND NOT(IsNull(long_result_blob))THEN
	// 07/16/2008 remove special characters
	ls_text = String(long_result_blob)
	li_numstats = inv_string.of_Parsetoarray (ls_text, "~n", ls_txtlines)
	FOR i = 1 TO li_numstats
		ls_txtlines[i] = inv_string.of_RemoveNonPrint(ls_txtlines[i])
		ls_txtlines[i] = inv_string.of_Trim(ls_txtlines[i])
	NEXT
	rtn = inv_string.of_ArrayToString(ls_txtlines,"~r~n",ls_text)
	tab_dtb.tabpage_longresult.mle_longresult.text = ls_text
ELSEIF 	IsNull(long_result_blob) THEN
	tab_dtb.tabpage_longresult.mle_longresult.text = ls_null
	Messagebox("ERROR SELECTING BLOB"," Long Result  is NULL")
ELSE
	Messagebox("ERROR SELECTING BLOB"," Error in selecting Long Result.")
END IF


RETURN

end subroutine

public function integer wf_calc_tot_time (string ls_totaltime);int li_hrs_pos,li_min_pos
string ls_hrsstr, ls_minsstr,ls_str

li_hrs_pos = Pos (ls_totaltime,':')
ls_hrsstr = Mid(ls_totaltime, 1, li_hrs_pos - 1 )
ls_str = Mid(ls_totaltime, li_hrs_pos + 1, len(ls_totaltime))
li_min_pos = Pos(ls_str,':') 
ls_minsstr = Mid(ls_str, 1 , li_min_pos - 1 )
//Messagebox("ls_hrs",' Hours = '+ ls_hrsstr  + ' Minutes = ' + ls_minsstr)	   
TotalTime = Integer(ls_hrsstr) * 60 + Integer(ls_minsstr)
return TotalTime
end function

public subroutine wf_clear_tabs ();tab_dtb.tabpage_reportxml.mle_reportxml.text=""
tab_dtb.tabpage_nlsreportxml.mle_nlsreportxml.text = ""
tab_dtb.tabpage_all_smil.mle_allsmil.text=""
tab_dtb.tabpage_selected_smil.mle_selsmils.text=""
tab_dtb.tabpage_opf.mle_opf.text=""
tab_dtb.tabpage_ncx.mle_ncx.text=""

end subroutine

public function integer of_refreshnewbook ();//////////////////////////////////////////////////////////////////////////////////////////////////////////
//
//	Function: of_refreshnewbook
// Args: None
// Returns : 1 for success, -1 for error
//	Description:
//	When the user selects a new book from a specified folder or path refresh the opf,ncx and smil files
// This behavior imitates when the user chooses a new book
//
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//	
//	Revision History
//
//	Developed by 	Date 				Version						Tracking#
//									
// Murali K.			02/28/2008      005 PICS Modifications	 Reqs: QAS a.4b.1, a.4b.2
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//

//		Refresh New books
n_runandwait in_rwait
string ls_command,ls_del_xml_file1,ls_del_xml_file2, ls_msg,ls_comments,ls_old_comments,ls_dirlist,ls_userid, qas_init
ULong lul_rc
Integer li_report,li_nlsreport, lcnt, rtn, li_cntrfy, bkcnt = 0
Long ll_bkseq,li_bytes
blob report_blob,nlsreport_blob
Boolean Dlist = FALSE,bjaws
Char lc_opf_chk,lc_ncx_chk,lc_smil_chk,lc_reportxml_chk,lc_nlsreportxml_chk,lc_playbook_chk, lc_status,lc_dirlist_chk
Date date_reviewed


ls_del_xml_file1 = "delete c:\report.xml"
ls_del_xml_file2 = "delete c:\nlsreport.xml"

Run(ls_del_xml_file1)
Run(ls_del_xml_file2)

IF Multi_Disc THEN
	ls_dirlist = sle_multidisc.Text + "\*.SMIL"
ELSE
	ls_dirlist = lc_cdrom + "\*.SMIL"
END IF

Dlist = lb_smil.DirList(ls_dirlist, 1)

IF NOT(IsNull(Dlist)) THEN
	lb_smil.SetState(1, TRUE)
ELSE
	ls_msg = "Error in reading the "+ls_dirlist+" drive. Please make sure that "+ls_dirlist+" exist."
	MessageBox ("ERROR READING", ls_msg ,StopSign!)
	RETURN -1
END IF

// Get the book number
ll_bkseq = Long(Mid(lb_smil.Text(1), 1, 5))

SELECT COUNT(*) , QAINIT, DT_TIME,COMMENTS
INTO :bkcnt, :qas_init, :date_reviewed,:ls_old_comments
FROM DTB_DATA
WHERE bkseq = :ll_bkseq
GROUP BY QAINIT, DT_TIME,COMMENTS
USING SqlServerTrans;

IF bkcnt > 0 THEN
	ls_msg = "Book number " + string(ll_bkseq) + " has already been reviewed by "+qas_init+" on "+ string(date_reviewed,'MM/DD/YY')
	MessageBox("Books exist",ls_msg,Information!)
	w_qa_zedval.mle_comments.text = "***Old Comments start here*** "+ls_old_comments+" ***Old Comments end here ***"
END IF	

IF Multi_Disc THEN
	ls_dirlist = sle_multidisc.Text + "\*.*"
ELSE
	ls_dirlist = lc_cdrom + "\*.*"
END IF
Dlist = tab_dtb.tabpage_dirlist.lb_dirlist.DirList(ls_dirlist, 1)

IF NOT(IsNull(Dlist)) THEN
	tab_dtb.tabpage_dirlist.lb_dirlist.SetState(1, TRUE)
ELSE
	ls_msg = "Error in reading the "+ls_dirlist+" drive. Please make sure that "+ls_dirlist+" exist."
	MessageBox ("ERROR READING", ls_msg ,StopSign!)
	RETURN -1
END IF

IF Multi_Disc THEN
	ls_command = "c:\runzv.bat "+ sle_multidisc.Text  + "\*.opf"
ELSE
	ls_command = "c:\runzv.bat "+ lc_cdrom + "\*.opf"
END IF

SetPointer(HourGlass!)
// Make sure you are in C drive
lul_rc = in_rwait.of_run("C:", Normal!)
// Make sure you are running runzv.bat at the root of C drive
lul_rc = in_rwait.of_run("CD\", Normal!)
// Run the command 
lul_rc = in_rwait.of_run(ls_command, Normal!)


// check return code
CHOOSE CASE lul_rc
	CASE in_rwait.WAIT_COMPLETE
		ls_msg = "The process completed normally!"
	CASE in_rwait.WAIT_TIMEOUT
		ls_msg = "The process was terminated after 10 seconds!"
	CASE ELSE
		ls_msg = "The process completed with return code: " + String(lul_rc)
END CHOOSE


li_report = FileOpen('c:\report.xml', StreamMode!)
IF li_report <> -1 THEN
	li_bytes = FileRead(li_report, report_blob)
END IF 

li_nlsreport = FileOpen('c:\nlsreport.xml', StreamMode!)
IF li_nlsreport <> -1 THEN
	li_bytes = FileRead(li_nlsreport, nlsreport_blob)
END IF

FileClose(li_report)
FileClose(li_nlsreport)

tab_dtb.tabpage_nlsreportxml.mle_nlsreportxml.text =  string(nlsreport_blob, EncodingANSI!)
tab_dtb.tabpage_reportxml.mle_reportxml.text = string(report_blob, EncodingANSI!)

ll_bkseq = Long(Mid(lb_smil.Text(1), 1, 5))

SELECT cntr, cntrfy
INTO :ls_cntr, :li_cntrfy
FROM ancntr
WHERE cntr IN ( SELECT cntr 
					FROM prod
					WHERE bkseq=:ll_bkseq
					AND prodstage in ('ZM', 'DT'))
USING SqlServerTrans;

sle_cntryr.text = string(li_cntrfy)

wf_extract_opf()

tab_dtb.tabpage_reportxml.mle_reportxml.SetFocus()

Old_DTB_book = FALSE

RETURN 1
end function

public function integer of_checkincheck (long al_bkseq);//////////////////////////////////////////////////////////////////////////////////////////////////////////
//
//	Function: of_refreshnewbook
// Args:  bk seq
// Returns : 1 for success, -1 for error
//	Description: If checked in make tabs readonly
//
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//	
//	Revision History
//
//	Developed by 	Date 				Version						Tracking#
//									
// Murali K.			10/23/2008      005 PICS Modifications	 Reqs: QAS a.4b.1, a.4b.2
// Murali K.			10/27/2008		use readonly indicator to enable disable tab.
//////////////////////////////////////////////////////////////////////////////////////////////////////////


IF NOT inv_qa.of_ischeckedin(al_bkseq,'') THEN
	ib_checkedin=FALSE
ELSE
	ib_checkedin=TRUE
END IF
//IF  ib_checkedin THEN // readonly tabs
IF  is_readonly = 'Y'  THEN //ib_checkedin THEN // readonly tabs
		tab_dtb.tabpage_shortresult.mle_shortresult.displayonly = TRUE
		tab_dtb.tabpage_mediumresult.mle_mediumresult.displayonly = TRUE
		tab_dtb.tabpage_longresult.mle_longresult.displayonly = TRUE
		tab_dtb.tabpage_reportxml.mle_reportxml.displayonly=TRUE
		tab_dtb.tabpage_reportxml.cbx_reportxml_check.enabled=FALSE
		tab_dtb.tabpage_nlsreportxml.mle_nlsreportxml.displayonly=TRUE
		tab_dtb.tabpage_nlsreportxml.cbx_nlsreportxml_check.enabled = FALSE
		tab_dtb.tabpage_opf.mle_opf.displayonly=TRUE
		tab_dtb.tabpage_opf.cbx_opf_check.enabled=FALSE
		tab_dtb.tabpage_ncx.mle_ncx.displayonly=TRUE
		tab_dtb.tabpage_ncx.cbx_ncx_check.enabled=FALSE
		tab_dtb.tabpage_all_smil.mle_allsmil.displayonly=TRUE
		tab_dtb.tabpage_all_smil.cbx_smil_check.enabled=FALSE
		tab_dtb.tabpage_selected_smil.mle_selsmils.displayonly=TRUE
		tab_dtb.tabpage_selected_smil.cbx_selsmil_chk.enabled=FALSE
		tab_dtb.tabpage_dirlist.lb_dirlist.enabled=FALSE
		tab_dtb.tabpage_dirlist.cbx_dirlist.enabled=FALSE
		cbx_playbook_check.enabled=FALSE
		cb_play.enabled=FALSE
ELSE
		tab_dtb.tabpage_shortresult.mle_shortresult.displayonly = FALSE
		tab_dtb.tabpage_mediumresult.mle_mediumresult.displayonly = FALSE
		tab_dtb.tabpage_longresult.mle_longresult.displayonly = FALSE
		
		tab_dtb.tabpage_reportxml.mle_reportxml.displayonly=FALSE
		tab_dtb.tabpage_reportxml.cbx_reportxml_check.enabled=TRUE
		tab_dtb.tabpage_nlsreportxml.mle_nlsreportxml.displayonly=FALSE
		tab_dtb.tabpage_nlsreportxml.cbx_nlsreportxml_check.enabled = TRUE
		tab_dtb.tabpage_opf.mle_opf.displayonly=FALSE
		tab_dtb.tabpage_opf.cbx_opf_check.enabled=TRUE
		tab_dtb.tabpage_ncx.mle_ncx.displayonly=FALSE
		tab_dtb.tabpage_ncx.cbx_ncx_check.enabled=TRUE
		
		tab_dtb.tabpage_all_smil.mle_allsmil.displayonly=FALSE
		tab_dtb.tabpage_all_smil.cbx_smil_check.enabled=TRUE
		
		tab_dtb.tabpage_selected_smil.mle_selsmils.displayonly=FALSE
		tab_dtb.tabpage_selected_smil.cbx_selsmil_chk.enabled=TRUE
		tab_dtb.tabpage_dirlist.lb_dirlist.enabled=TRUE
		tab_dtb.tabpage_dirlist.cbx_dirlist.enabled=TRUE
		cbx_playbook_check.enabled=TRUE
		cb_play.enabled=TRUE
		
END IF
RETURN 1
end function

on w_qa_zedval.create
int iCurrent
call super::create
this.shl_1=create shl_1
this.mle_comments=create mle_comments
this.cb_spell=create cb_spell
this.st_opf_ncx=create st_opf_ncx
this.mle_opf_ncx=create mle_opf_ncx
this.sle_multidisc=create sle_multidisc
this.cbx_multidisc=create cbx_multidisc
this.cb_qa=create cb_qa
this.lb_cdrom=create lb_cdrom
this.st_cdrom=create st_cdrom
this.sle_cntryr=create sle_cntryr
this.st_cntryr=create st_cntryr
this.sle_search=create sle_search
this.cb_search=create cb_search
this.cb_later=create cb_later
this.st_smil=create st_smil
this.st_status=create st_status
this.st_comments=create st_comments
this.cbx_playbook_check=create cbx_playbook_check
this.cb_t=create cb_t
this.cb_refresh=create cb_refresh
this.lb_smil=create lb_smil
this.cb_play=create cb_play
this.cb_reject=create cb_reject
this.cb_approve=create cb_approve
this.cb_exit=create cb_exit
this.tab_dtb=create tab_dtb
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.shl_1
this.Control[iCurrent+2]=this.mle_comments
this.Control[iCurrent+3]=this.cb_spell
this.Control[iCurrent+4]=this.st_opf_ncx
this.Control[iCurrent+5]=this.mle_opf_ncx
this.Control[iCurrent+6]=this.sle_multidisc
this.Control[iCurrent+7]=this.cbx_multidisc
this.Control[iCurrent+8]=this.cb_qa
this.Control[iCurrent+9]=this.lb_cdrom
this.Control[iCurrent+10]=this.st_cdrom
this.Control[iCurrent+11]=this.sle_cntryr
this.Control[iCurrent+12]=this.st_cntryr
this.Control[iCurrent+13]=this.sle_search
this.Control[iCurrent+14]=this.cb_search
this.Control[iCurrent+15]=this.cb_later
this.Control[iCurrent+16]=this.st_smil
this.Control[iCurrent+17]=this.st_status
this.Control[iCurrent+18]=this.st_comments
this.Control[iCurrent+19]=this.cbx_playbook_check
this.Control[iCurrent+20]=this.cb_t
this.Control[iCurrent+21]=this.cb_refresh
this.Control[iCurrent+22]=this.lb_smil
this.Control[iCurrent+23]=this.cb_play
this.Control[iCurrent+24]=this.cb_reject
this.Control[iCurrent+25]=this.cb_approve
this.Control[iCurrent+26]=this.cb_exit
this.Control[iCurrent+27]=this.tab_dtb
end on

on w_qa_zedval.destroy
call super::destroy
destroy(this.shl_1)
destroy(this.mle_comments)
destroy(this.cb_spell)
destroy(this.st_opf_ncx)
destroy(this.mle_opf_ncx)
destroy(this.sle_multidisc)
destroy(this.cbx_multidisc)
destroy(this.cb_qa)
destroy(this.lb_cdrom)
destroy(this.st_cdrom)
destroy(this.sle_cntryr)
destroy(this.st_cntryr)
destroy(this.sle_search)
destroy(this.cb_search)
destroy(this.cb_later)
destroy(this.st_smil)
destroy(this.st_status)
destroy(this.st_comments)
destroy(this.cbx_playbook_check)
destroy(this.cb_t)
destroy(this.cb_refresh)
destroy(this.lb_smil)
destroy(this.cb_play)
destroy(this.cb_reject)
destroy(this.cb_approve)
destroy(this.cb_exit)
destroy(this.tab_dtb)
end on

event pfc_postopen;call super::pfc_postopen;//////////////////////////////////////////////////////////////////////////////////////////////////////////
//
//	Event:  pfc_postopen event
//
//	Description: Retrieve old book or process new book
//	
//
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//	
//	Revision History
//
//	Developed by 	Date 				Version						Tracking#
//									
// Murali K.			06/30/2008      Process short, med and long results. Phase-2
// Murali K.			 07/03/2008     SHORT, MEDIUM, LONG RESULTS READONLY IF POPULATED FROM AUTOTEST 
// Murali K.			10/23/2008 		if checked in make tabs readonly
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//

n_runandwait in_rwait
string ls_command,ls_del_xml_file1,ls_del_xml_file2, ls_msg,ls_comments,ls_old_comments,ls_dirlist,ls_userid, qas_init,ls_shortresult, ls_text
ULong lul_rc
Integer li_report,li_nlsreport, lcnt, rtn, li_cntrfy, bkcnt = 0
Long ll_bkseq
blob report_blob,nlsreport_blob
Boolean Dlist = FALSE,bjaws
Char lc_opf_chk,lc_ncx_chk,lc_smil_chk,lc_reportxml_chk,lc_nlsreportxml_chk,lc_playbook_chk, lc_dirlist_chk, lc_status
Date date_reviewed

String ls_txtlines[ ], dirlist_file
Integer li_numstats,i
n_cst_string 	inv_string

bjaws = wf_isjawsrunning()
IF bjaws = TRUE THEN
	of_SetJaws(TRUE)
ELSE
	of_SetJaws(FALSE)
END IF
//Clear all the tabs
wf_clear_tabs()

// Display a message for new book.
rtn = Messagebox("DTB Books","Is this a new book?",question!, yesNoCancel!, 1)
IF rtn=1 THEN
	// New books
	ls_del_xml_file1 = "delete c:\report.xml"
	ls_del_xml_file2 = "delete c:\nlsreport.xml"
	
	Run(ls_del_xml_file1)
	Run(ls_del_xml_file2)
	
	ls_dirlist = lc_cdrom + "\*.SMIL"
	
	Dlist = lb_smil.DirList(ls_dirlist, 1)
	
	IF NOT(IsNull(Dlist)) THEN
		lb_smil.SetState(1, TRUE)
	ELSE
		ls_msg = "Error in reading the CD in "+ls_dirlist+" drive. Please make sure that CD is placed in your "+ls_dirlist+" Drive."
		MessageBox ("ERROR READING CD", ls_msg ,StopSign!)
		RETURN
	END IF
	
       // Get the book number
	ll_bkseq = Long(Mid(lb_smil.Text(1), 1, 5))
	
	SELECT COUNT(*) , QAINIT, DT_TIME,COMMENTS
	INTO :bkcnt, :qas_init, :date_reviewed,:ls_old_comments
	FROM DTB_DATA
	WHERE bkseq = :ll_bkseq
	GROUP BY QAINIT, DT_TIME,COMMENTS
	USING SqlServerTrans;
	
	IF bkcnt > 0 THEN
		ls_msg = "Book number " + string(ll_bkseq) + " has already been reviewed by "+qas_init+" on "+ string(date_reviewed,'MM/DD/YY')
		MessageBox("Books exist",ls_msg,Information!)
		This.mle_comments.text = "***Old Comments start here*** "+ls_old_comments+" ***Old Comments end here ***"
	END IF	

	
	ls_dirlist = lc_cdrom + "\*.*"
	Dlist = tab_dtb.tabpage_dirlist.lb_dirlist.DirList(ls_dirlist, 1)
	
	IF NOT(IsNull(Dlist)) THEN
		tab_dtb.tabpage_dirlist.lb_dirlist.SetState(1, TRUE)
	ELSE
		ls_msg = "Error in reading the CD in "+ls_dirlist+" drive. Please make sure that CD is placed in your "+ls_dirlist+" Drive."
		MessageBox ("ERROR READING CD", ls_msg ,StopSign!)
		RETURN
	END IF
	
	ls_command = "c:\runzv.bat "+ lc_cdrom + "\*.opf"
	
	SetPointer(HourGlass!)
	// Make sure you are in C drive
	lul_rc = in_rwait.of_run("C:", Normal!)
	// Make sure you are running runzv.bat at the root of C drive
	lul_rc = in_rwait.of_run("CD\", Normal!)
	// Run the command 
	lul_rc = in_rwait.of_run(ls_command, Normal!)
	
	// check return code
	CHOOSE CASE lul_rc
		CASE in_rwait.WAIT_COMPLETE
			ls_msg = "The process completed normally!"
		CASE in_rwait.WAIT_TIMEOUT
			ls_msg = "The process was terminated after 10 seconds!"
		CASE ELSE
			ls_msg = "The process completed with return code: " + String(lul_rc)
	END CHOOSE
	//MessageBox('DOS Command', ls_msg)
	
	li_report = FileOpen('c:\report.xml', StreamMode!)
	IF li_report <> -1 THEN
		FileRead(li_report, report_blob)
	END IF 
	
	li_nlsreport = FileOpen('c:\nlsreport.xml', StreamMode!)
	IF li_nlsreport <> -1 THEN
		FileRead(li_nlsreport, nlsreport_blob)
	END IF
	
	FileClose(li_report)
	FileClose(li_nlsreport)
	
	tab_dtb.tabpage_nlsreportxml.mle_nlsreportxml.text =  string(nlsreport_blob, EncodingANSI!)
	tab_dtb.tabpage_reportxml.mle_reportxml.text = string(report_blob, EncodingANSI!)
	
	ll_bkseq = Long(Mid(lb_smil.Text(1), 1, 5))
	
	SELECT cntr, cntrfy
	INTO :ls_cntr, :li_cntrfy
	FROM ancntr
	WHERE cntr IN ( SELECT cntr 
						FROM prod
						WHERE bkseq=:ll_bkseq
						AND prodstage in ('ZM', 'DT'))
	USING SqlServerTrans;
	
	sle_cntryr.text = string(li_cntrfy)
	
	wf_extract_opf()
	
	tab_dtb.tabpage_reportxml.mle_reportxml.SetFocus()
	
	Old_DTB_book = FALSE
	
	// 10/23/2008 if checked in make tabs readonly
	of_checkincheck(ll_bkseq)
ELSEIF rtn=2 THEN
	Open(w_qa_select_dtb_book)
	// If book was selected
	IF IsNull(message.StringParm)=FALSE AND message.StringParm<>"" THEN
		ll_bkseq = Long(message.StringParm)
		Old_DTB_bkseq = ll_bkseq

		// 06/30/2008 short result process
		
		SELECT opf_chk, ncx_chk, smil_chk, reportxml_chk, nlsreportxml_chk, playbook_chk, dirlist_chk, comments, 
		dc_date, dc_format, dtb_multimediatype, dtb_audioformat, dtb_depth, navpoints, dtb_totaltime, dtb_produceddate,
		UPPER(status), UPPER(qainit), count(*) , short_result ,read_only_yn
		INTO :lc_opf_chk, :lc_ncx_chk, :lc_smil_chk, :lc_reportxml_chk, :lc_nlsreportxml_chk, :lc_playbook_chk, :lc_dirlist_chk, :ls_comments,
		:ls_dcdate, :ls_dcformat, :ls_multimediatype, :ls_audioformat, :ls_depth, :lcnt_navpoint, :TotalTime, :ld_produceddate,
		:lc_status, :ls_userid, :lcnt, :ls_shortresult, :is_readonly
		FROM dtb_data
		WHERE bkseq=:ll_bkseq
		GROUP BY opf_chk, ncx_chk, smil_chk, reportxml_chk, nlsreportxml_chk, playbook_chk, dirlist_chk, comments, 
		dc_date, dc_format, dtb_multimediatype, dtb_audioformat, dtb_depth, navpoints, dtb_totaltime, dtb_produceddate,
		status, qainit, short_result, read_only_yn
		USING SqlServerTrans;
			
		IF lcnt > 0 THEN

			 lstr_db_data.dc_date = ls_dcdate
			 lstr_db_data.dc_format = ls_dcformat
			 lstr_db_data.dtb_multimediatype = ls_multimediatype
			 lstr_db_data.dtb_produceddate = ld_produceddate
			 lstr_db_data.dtb_totaltime = TotalTime
			 lstr_db_data.dtb_audioformat = ls_audioformat
			 lstr_db_data.dtb_depth = ls_depth
			 lstr_db_data.navpoint = lcnt_navpoint
		
		
		// Do something
			IF  lc_opf_chk = 'Y'  THEN
				tab_dtb.tabpage_opf.cbx_opf_check.checked = TRUE
			ELSE
				tab_dtb.tabpage_opf.cbx_opf_check.checked = FALSE
			END IF
				
			IF  lc_playbook_chk = 'Y'  THEN
				This.cbx_playbook_check.checked = TRUE
			ELSE
				This.cbx_playbook_check.checked = FALSE
			END IF
		
			IF  lc_smil_chk = 'Y'  THEN
				tab_dtb.tabpage_all_smil.cbx_smil_check.checked = TRUE
				tab_dtb.tabpage_selected_smil.cbx_selsmil_chk.checked = TRUE
			ELSE
				tab_dtb.tabpage_all_smil.cbx_smil_check.checked = FALSE
				tab_dtb.tabpage_selected_smil.cbx_selsmil_chk.checked = FALSE
			END IF
		
			IF  lc_ncx_chk = 'Y'  THEN
				tab_dtb.tabpage_ncx.cbx_ncx_check.checked = TRUE
			ELSE
				tab_dtb.tabpage_ncx.cbx_ncx_check.checked = FALSE
			END IF
		
			IF  lc_nlsreportxml_chk = 'Y'  THEN
				tab_dtb.tabpage_nlsreportxml.cbx_nlsreportxml_check.checked = TRUE
			ELSE
				tab_dtb.tabpage_nlsreportxml.cbx_nlsreportxml_check.checked = FALSE
			END IF
		
			IF  lc_reportxml_chk = 'Y'  THEN
				tab_dtb.tabpage_reportxml.cbx_reportxml_check.checked = TRUE
			ELSE
				tab_dtb.tabpage_reportxml.cbx_reportxml_check.checked = FALSE
			END IF
		
			IF  lc_dirlist_chk = 'Y'  THEN
				tab_dtb.tabpage_dirlist.cbx_dirlist.checked = TRUE
			ELSE
				tab_dtb.tabpage_dirlist.cbx_dirlist.checked = FALSE
			END IF

			This.mle_comments.text = ls_comments
			// 06/30/2008 set short result
			// 07/16/2008 remove special characters
			ls_text = ls_shortresult
			li_numstats = inv_string.of_Parsetoarray (ls_text, "~n", ls_txtlines)
			FOR i = 1 TO li_numstats
				ls_txtlines[i] = inv_string.of_RemoveNonPrint(ls_txtlines[i])
				ls_txtlines[i] = inv_string.of_Trim(ls_txtlines[i])
			NEXT
			rtn = inv_string.of_ArrayToString(ls_txtlines,"~r~n",ls_text)

			tab_dtb.tabpage_shortresult.mle_shortresult.text = ls_text
		
			// 07/03/2008 SHORT, MEDIUM, LONG RESULTS READONLY IF POPULATED FROM AUTOTEST 
			IF is_readonly = 'Y' THEN
				tab_dtb.tabpage_shortresult.mle_shortresult.displayonly = TRUE
				tab_dtb.tabpage_mediumresult.mle_mediumresult.displayonly = TRUE
				tab_dtb.tabpage_longresult.mle_longresult.displayonly = TRUE

				tab_dtb.tabpage_reportxml.mle_reportxml.displayonly=TRUE
				tab_dtb.tabpage_reportxml.cbx_reportxml_check.enabled=FALSE
				tab_dtb.tabpage_nlsreportxml.mle_nlsreportxml.displayonly=TRUE
				tab_dtb.tabpage_nlsreportxml.cbx_nlsreportxml_check.enabled = FALSE
				tab_dtb.tabpage_opf.mle_opf.displayonly=TRUE
				tab_dtb.tabpage_opf.cbx_opf_check.enabled=FALSE
				tab_dtb.tabpage_ncx.mle_ncx.displayonly=TRUE
				tab_dtb.tabpage_ncx.cbx_ncx_check.enabled=FALSE
				
				tab_dtb.tabpage_all_smil.mle_allsmil.displayonly=TRUE
				tab_dtb.tabpage_all_smil.cbx_smil_check.enabled=FALSE
				
				tab_dtb.tabpage_selected_smil.mle_selsmils.displayonly=TRUE
				tab_dtb.tabpage_selected_smil.cbx_selsmil_chk.enabled=FALSE
				tab_dtb.tabpage_dirlist.lb_dirlist.enabled=FALSE
				tab_dtb.tabpage_dirlist.cbx_dirlist.enabled=FALSE


			ELSE
				tab_dtb.tabpage_shortresult.mle_shortresult.displayonly = FALSE
				tab_dtb.tabpage_mediumresult.mle_mediumresult.displayonly = FALSE
				tab_dtb.tabpage_longresult.mle_longresult.displayonly = FALSE
				
				tab_dtb.tabpage_reportxml.mle_reportxml.displayonly=FALSE
				tab_dtb.tabpage_reportxml.cbx_reportxml_check.enabled=TRUE
				tab_dtb.tabpage_nlsreportxml.mle_nlsreportxml.displayonly=FALSE
				tab_dtb.tabpage_nlsreportxml.cbx_nlsreportxml_check.enabled = TRUE
				tab_dtb.tabpage_opf.mle_opf.displayonly=FALSE
				tab_dtb.tabpage_opf.cbx_opf_check.enabled=TRUE
				tab_dtb.tabpage_ncx.mle_ncx.displayonly=FALSE
				tab_dtb.tabpage_ncx.cbx_ncx_check.enabled=TRUE
				
				tab_dtb.tabpage_all_smil.mle_allsmil.displayonly=FALSE
				tab_dtb.tabpage_all_smil.cbx_smil_check.enabled=TRUE
				
				tab_dtb.tabpage_selected_smil.mle_selsmils.displayonly=FALSE
				tab_dtb.tabpage_selected_smil.cbx_selsmil_chk.enabled=TRUE
				tab_dtb.tabpage_dirlist.lb_dirlist.enabled=TRUE
				tab_dtb.tabpage_dirlist.cbx_dirlist.enabled=TRUE

				
			END IF
		
			IF lc_status = 'A' THEN
				st_status.text = 'DTB Status is approved by ' + ls_userid
				st_status.TextColor = RGB(0,0,255)
			ELSEIF lc_status = 'R' THEN
				st_status.Text = 'DTB Status is rejected by ' + ls_userid
				st_status.TextColor = RGB(255,0,0)
			ELSEIF lc_status = 'H' THEN
				st_status.Text = 'DTB Status is finish later by ' + ls_userid
				st_status.TextColor = RGB(0,0,0)
			ELSE
				st_status.Text = 'DTB Status '		
			END IF 
			
			// get the XML reports which has already been extracted and saved in DTB_DATA table
			wf_get_blobs(ll_bkseq)
		
			tab_dtb.tabpage_reportxml.mle_reportxml.SetFocus()
			
			// Set this variable to True since this book is an old book
			Old_DTB_book = TRUE
			
			// 10/23/2008 if checked in make tabs readonly
			of_checkincheck(ll_bkseq)
			
		ELSE
			
			// Book does not exist in database.
			MessageBox("DTB Error","This book does not exist in database.")
			RETURN
		
		END IF
	ELSE
		cb_exit.TriggerEvent(clicked!)
		RETURN
	END IF	
ELSE
	RETURN
END IF
end event

event resize;call super::resize;long ll_height

This.X = w_pics_main.X
This.Y = w_pics_main.Y
ll_height = w_pics_main.mdi_1.Height
This.Resize(w_pics_main.WorkSpaceWidth(), ll_height)

end event

event pfc_preopen;call super::pfc_preopen;this.of_SetPreference(TRUE)
this.inv_preference.of_SetToolBars(TRUE)
this.inv_preference.of_SetWindow(TRUE)
this.of_SetResize(TRUE)
this.inv_resize.of_SetOrigSize(this.workspacewidth(),this.workspaceheight())
This.WindowState = Maximized!

inv_resize.of_Register(cb_approve, "Scale")
inv_resize.of_Register(cb_reject, "Scale")
inv_resize.of_Register(cb_exit, "Scale")
inv_resize.of_Register(cb_refresh, "Scale")
inv_resize.of_Register(cb_later, "Scale")
inv_resize.of_Register(cb_search, "Scale")
inv_resize.of_Register(cb_qa, "Scale")
inv_resize.of_Register(cb_spell, "Scale")
inv_resize.of_Register(sle_search, "Scale")

inv_resize.of_Register(lb_smil, "Scale")
inv_resize.of_Register(cb_play, "Scale")

inv_resize.of_Register(mle_comments, "Scale")
inv_resize.of_Register(mle_opf_ncx, "Scale")
inv_resize.of_Register(st_comments, "Scale")
inv_resize.of_Register(st_status, "Scale")
inv_resize.of_Register(st_smil, "Scale")
inv_resize.of_Register(st_opf_ncx, "Scale")

inv_resize.of_Register(sle_cntryr, "Scale")
inv_resize.of_Register(st_cntryr, "Scale")

inv_resize.of_Register(lb_cdrom, "Scale")
lb_cdrom.SelectItem('E:', 1)
lc_cdrom = 'E:'
inv_resize.of_Register(st_cdrom, "Scale")

// 02/22/2008 Set the opf/smil file path, remove hard coded drive names
inv_resize.of_Register(shl_1, "Scale")
shl_1.text = lc_cdrom

inv_resize.of_Register(tab_dtb, "Scale")

inv_resize.of_Register(tab_dtb.tabpage_nlsreportxml, "Scale")
inv_resize.of_Register(tab_dtb.tabpage_nlsreportxml.mle_nlsreportxml, "Scale")
inv_resize.of_Register(tab_dtb.tabpage_nlsreportxml.cbx_nlsreportxml_check, "Scale")

inv_resize.of_Register(tab_dtb.tabpage_reportxml, "Scale")
inv_resize.of_Register(tab_dtb.tabpage_reportxml.mle_reportxml, "Scale")
inv_resize.of_Register(tab_dtb.tabpage_reportxml.cbx_reportxml_check, "Scale")

inv_resize.of_Register(tab_dtb.tabpage_ncx, "Scale")
inv_resize.of_Register(tab_dtb.tabpage_ncx.mle_ncx, "Scale")
inv_resize.of_Register(tab_dtb.tabpage_ncx.cbx_ncx_check, "Scale")

inv_resize.of_Register(tab_dtb.tabpage_opf, "Scale")
inv_resize.of_Register(tab_dtb.tabpage_opf.mle_opf, "Scale")
inv_resize.of_Register(tab_dtb.tabpage_opf.cbx_opf_check, "Scale")

inv_resize.of_Register(cbx_playbook_check, "Scale")

inv_resize.of_Register(tab_dtb.tabpage_all_smil, "Scale")
inv_resize.of_Register(tab_dtb.tabpage_all_smil.mle_allsmil, "Scale")
inv_resize.of_Register(tab_dtb.tabpage_all_smil.cbx_smil_check, "Scale")

inv_resize.of_Register(tab_dtb.tabpage_selected_smil, "Scale")
inv_resize.of_Register(tab_dtb.tabpage_selected_smil.mle_selsmils, "Scale")
inv_resize.of_Register(tab_dtb.tabpage_selected_smil.cbx_selsmil_chk, "Scale")

inv_resize.of_Register(cbx_multidisc, "Scale")
inv_resize.of_Register(sle_multidisc, "Scale")

inv_resize.of_Register(tab_dtb.tabpage_dirlist, "Scale")
inv_resize.of_Register(tab_dtb.tabpage_dirlist.lb_dirlist, "Scale")
inv_resize.of_Register(tab_dtb.tabpage_dirlist.cbx_dirlist, "Scale")

// 06/30/2008
inv_resize.of_Register(tab_dtb.tabpage_shortresult, "Scale")
inv_resize.of_Register(tab_dtb.tabpage_shortresult.mle_shortresult, "Scale")

inv_resize.of_Register(tab_dtb.tabpage_mediumresult, "Scale")
inv_resize.of_Register(tab_dtb.tabpage_mediumresult.mle_mediumresult, "Scale")

inv_resize.of_Register(tab_dtb.tabpage_longresult, "Scale")
inv_resize.of_Register(tab_dtb.tabpage_longresult.mle_longresult, "Scale")

end event

type shl_1 from statichyperlink within w_qa_zedval
integer x = 2757
integer y = 1812
integer width = 946
integer height = 88
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean underline = true
string pointer = "Icon!"
long textcolor = 134217856
long backcolor = 67108864
string text = "E:"
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

event clicked;//////////////////////////////////////////////////////////////////////////////////////////////////////////
//
//	Event: clicked for shl_1
//
//	Description:
//	Open the file open dialog to set the file path
//
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//	
//	Revision History
//
//	Developed by 	Date 				Version						Tracking#
//									
// Murali K.			02/22/2008      005 PICS Modifications	 Reqs: QAS a.4b.1, a.4b.2
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//

string ls_docpath, ls_docname
integer li_rtn
long ll_len

li_rtn = GetFileOpenName("Set File Path ( Click on a OPF,NCX or SMIL file to set the folder)", &
   ls_docpath, ls_docname, "OPF", &
   + "OPF Files (*.OPF),*.OPF," &
   + "SMIL Files (*.SMIL),*.SMIL," &
  + "NCX Files (*.NCX),*.NCX", &
   lc_cdrom + '\', 512)
	
//   + "All Files (*.*), *.*", &
IF li_rtn = 1 THEN
	ll_len = Len(ls_docname)
	ll_len++ // for '\'
	lc_cdrom  = Mid(ls_docpath, 1, Len(ls_docpath) - ll_len)
	This.text = lc_cdrom 
	is_docname = Mid(ls_docname, 1, Len(ls_docname) - 4)
	// 02/28/2008	if the path is changed refresh for the selected new book from that folder
	of_refreshnewbook()
END IF

//
end event

type mle_comments from u_mle within w_qa_zedval
integer x = 37
integer y = 1984
integer width = 2450
integer height = 256
integer taborder = 70
integer textsize = -10
string facename = "Arial"
boolean vscrollbar = true
end type

type cb_spell from commandbutton within w_qa_zedval
integer x = 1573
integer y = 2272
integer width = 439
integer height = 96
integer taborder = 130
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Spell Chec&k"
end type

event clicked;qa_nca_word lnca_word
String ls_S
	
//comments
ls_S = mle_comments.Text
IF NOT(IsNull(ls_S)) THEN
	lnca_Word.SpellCheck( ls_S )
	mle_comments.Text = ls_S
END IF


end event

type st_opf_ncx from statictext within w_qa_zedval
string tag = "Extracted data from OPF and NCX files"
integer x = 2592
integer y = 1908
integer width = 1006
integer height = 60
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "Extracted data from OPF and NCX files"
boolean focusrectangle = false
end type

type mle_opf_ncx from multilineedit within w_qa_zedval
integer x = 2487
integer y = 1984
integer width = 1207
integer height = 256
integer taborder = 90
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
boolean vscrollbar = true
boolean autovscroll = true
borderstyle borderstyle = stylelowered!
end type

type sle_multidisc from singlelineedit within w_qa_zedval
boolean visible = false
integer x = 2592
integer y = 1808
integer width = 1125
integer height = 96
integer taborder = 100
integer textsize = -9
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
borderstyle borderstyle = stylelowered!
end type

event modified;	ls_multidisk_dirlist = this.Text

end event

type cbx_multidisc from checkbox within w_qa_zedval
string tag = "Multiple Disk DTB"
integer x = 1865
integer y = 1824
integer width = 498
integer height = 64
integer taborder = 60
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "Multi-disc DTB"
boolean lefttext = true
boolean righttoleft = true
end type

event clicked;//////////////////////////////////////////////////////////////////////////////////////////////////////////
//
//	Event: clicked for cbx_multidisc
//
//	Description:
//	Remove references to lb_cdrom
//
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//	
//	Revision History
//
//	Developed by 	Date 				Version						Tracking#
//									
// Murali K.			02/22/2008      005 PICS Modifications	 Reqs: QAS a.4b.1, a.4b.2
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//
string ls_msg,ls_dirlist
boolean Dlist

IF This.checked = TRUE THEN

       Multi_Disc = TRUE
	
	st_cdrom.Visible = FALSE
//	lb_cdrom.Visible = FALSE // 02/22/2008
	sle_multidisc.Visible = TRUE
	sle_multidisc.Text = "c:\multidsk"
		 
	ls_multidisk_dirlist = sle_multidisc.Text
	ls_dirlist = ls_multidisk_dirlist + "\*.*"
	Dlist = tab_dtb.tabpage_dirlist.lb_dirlist.DirList(ls_dirlist, 1)
	
	IF IsNull(Dlist) THEN
		ls_msg = "Error in reading the Multi-dist DTB "+ls_dirlist+" drive. Please make sure that directory exist and files are placed in it."
		MessageBox ("ERROR", ls_msg ,StopSign!)
		RETURN
	END IF

ELSE
       Multi_Disc = FALSE

	st_cdrom.Visible = TRUE
//	lb_cdrom.Visible = TRUE // 02/22/2008
	sle_multidisc.Visible = FALSE
	sle_multidisc.Text = ""
	
	ls_dirlist = lc_cdrom + "\*.*"
	Dlist = tab_dtb.tabpage_dirlist.lb_dirlist.DirList(ls_dirlist, 1)
	
	IF NOT(IsNull(Dlist)) THEN
		tab_dtb.tabpage_dirlist.lb_dirlist.SetState(1, TRUE)
	ELSE
		ls_msg = "Error in reading the CD in "+ls_dirlist+" drive. Please make sure that CD is placed in your "+ls_dirlist+" Drive."
		MessageBox ("ERROR READING CD", ls_msg ,StopSign!)
		RETURN
	END IF

END IF



end event

type cb_qa from commandbutton within w_qa_zedval
integer x = 1134
integer y = 2272
integer width = 402
integer height = 96
integer taborder = 140
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&QA Review"
end type

event clicked;OpenSheet(w_qa_product_review, w_pics_main, 0, Original!)
end event

type lb_cdrom from listbox within w_qa_zedval
string tag = "CD ROM Drive Letter"
boolean visible = false
integer x = 2962
integer y = 1792
integer width = 183
integer height = 96
integer taborder = 70
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
boolean vscrollbar = true
string item[] = {"D:","E:","F:","G:","H:","I:","J:","K:","L:","M:","N:","O:","P:","Q:"}
borderstyle borderstyle = stylelowered!
end type

event selectionchanged;int li_ItemCount,li_ItemTotal

// Get the number of items in the ListBox.

li_ItemTotal = lb_cdrom.TotalItems( )


FOR li_ItemCount = 1 to li_ItemTotal

   IF lb_cdrom.State(li_ItemCount) = 1 THEN   		
	lc_cdrom = lb_cdrom.text(li_ItemCount)
   END IF

NEXT

Multi_disc = FALSE
end event

type st_cdrom from statictext within w_qa_zedval
integer x = 2446
integer y = 1824
integer width = 297
integer height = 64
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "Set Folder"
alignment alignment = right!
boolean focusrectangle = false
end type

type sle_cntryr from singlelineedit within w_qa_zedval
string tag = "Contract Year"
integer x = 585
integer y = 1792
integer width = 293
integer height = 96
integer taborder = 40
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
boolean displayonly = true
borderstyle borderstyle = stylelowered!
end type

type st_cntryr from statictext within w_qa_zedval
string tag = "Contract Year"
integer x = 183
integer y = 1824
integer width = 402
integer height = 60
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "Contract Year"
alignment alignment = right!
boolean focusrectangle = false
end type

type sle_search from singlelineedit within w_qa_zedval
string tag = "Search the string"
integer x = 329
integer y = 1664
integer width = 658
integer height = 96
integer taborder = 30
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
borderstyle borderstyle = stylelowered!
end type

type cb_search from commandbutton within w_qa_zedval
string tag = "search"
integer x = 37
integer y = 1664
integer width = 283
integer height = 96
integer taborder = 20
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&Search"
end type

event clicked;long search_pos = 1
int search_len,tab_order,li_start_pos=1,rtn
string ls_text, ls_search,ls_search_title

ls_search = sle_search.text
tab_order = tab_dtb.SelectedTab
search_len = len(ls_search)

IF tab_order = 1 THEN
	ls_text = tab_dtb.tabpage_reportxml.mle_reportxml.Text
	ls_search_title = "Searching Report XML File"
	DO WHILE li_start_pos > 0 
		search_pos = Pos(ls_text,ls_search,li_start_pos)
		IF search_pos = 0 THEN
			MessageBox(ls_search_title,ls_search+" Not Found",Information!)
			EXIT
		END IF
		tab_dtb.tabpage_reportxml.mle_reportxml.SelectText(search_pos,search_len)
		rtn = MessageBox(ls_search_title,"Search again?",Question!,YesNo!,1)
		IF rtn = 2 THEN
			EXIT
		ELSE
			li_start_pos = search_pos + search_len
			li_start_pos = Pos(ls_text,ls_search,li_start_pos)
		END IF
	LOOP 
ELSEIF tab_order = 2 THEN
	ls_text = tab_dtb.tabpage_nlsreportxml.mle_nlsreportxml.Text
	ls_search_title = "Searching NLS Report XML File"
	DO WHILE li_start_pos > 0 
		search_pos = Pos(ls_text,ls_search,li_start_pos)
		IF search_pos = 0 THEN
			MessageBox(ls_search_title,ls_search+" Not Found",Information!)
			EXIT
		END IF
		tab_dtb.tabpage_nlsreportxml.mle_nlsreportxml.SelectText(search_pos,search_len)
		rtn = MessageBox(ls_search_title,"Search again?",Question!,YesNo!,1)
		IF rtn = 2 THEN
			EXIT
		ELSE
			li_start_pos = search_pos + search_len
			li_start_pos = Pos(ls_text,ls_search,li_start_pos)
		END IF
	LOOP 
ELSEIF tab_order = 3 THEN
	ls_text = tab_dtb.tabpage_opf.mle_opf.Text
	ls_search_title = "Searching OPF File"
	DO WHILE li_start_pos > 0 
		search_pos = Pos(ls_text,ls_search,li_start_pos)
		IF search_pos = 0 THEN
			MessageBox(ls_search_title,ls_search+" Not Found",Information!)
			EXIT
		END IF
		tab_dtb.tabpage_opf.mle_opf.SelectText(search_pos,search_len)
		rtn = MessageBox(ls_search_title,"Search again?",Question!,YesNo!,1)
		IF rtn = 2 THEN
			EXIT
		ELSE
			li_start_pos = search_pos + search_len
			li_start_pos = Pos(ls_text,ls_search,li_start_pos)
		END IF
	LOOP 
ELSEIF tab_order = 4 THEN
	ls_text = tab_dtb.tabpage_ncx.mle_ncx.Text
	ls_search_title = "Searching NCX File"
	DO WHILE li_start_pos > 0 
		search_pos = Pos(ls_text,ls_search,li_start_pos)
		IF search_pos = 0 THEN
			MessageBox(ls_search_title,ls_search+" Not Found",Information!)
			EXIT
		END IF
		tab_dtb.tabpage_ncx.mle_ncx.SelectText(search_pos,search_len)
		rtn = MessageBox(ls_search_title,"Search again?",Question!,YesNo!,1)
		IF rtn = 2 THEN
			EXIT
		ELSE
			li_start_pos = search_pos + search_len
			li_start_pos = Pos(ls_text,ls_search,li_start_pos)
		END IF
	LOOP 
ELSEIF tab_order = 5 THEN
	ls_text = tab_dtb.tabpage_all_smil.mle_allsmil.Text
	ls_search_title = "Searching SMIL Files"
	DO WHILE li_start_pos > 0 
		search_pos = Pos(ls_text,ls_search,li_start_pos)
		IF search_pos = 0 THEN
			MessageBox(ls_search_title,ls_search+" Not Found",Information!)
			EXIT
		END IF
		tab_dtb.tabpage_all_smil.mle_allsmil.SelectText(search_pos,search_len)
		rtn = MessageBox(ls_search_title,"Search again?",Question!,YesNo!,1)
		IF rtn = 2 THEN
			EXIT
		ELSE
			li_start_pos = search_pos + search_len
			li_start_pos = Pos(ls_text,ls_search,li_start_pos)
		END IF
	LOOP 
ELSEIF tab_order = 6 THEN
	ls_text = tab_dtb.tabpage_selected_smil.mle_selsmils.Text
	ls_search_title = "Searching SMIL Files"
	DO WHILE li_start_pos > 0 
		search_pos = Pos(ls_text,ls_search,li_start_pos)
		IF search_pos = 0 THEN
			MessageBox(ls_search_title,ls_search+" Not Found",Information!)
			EXIT
		END IF
		tab_dtb.tabpage_selected_smil.mle_selsmils.SelectText(search_pos,search_len)
		rtn = MessageBox(ls_search_title,"Search again?",Question!,YesNo!,1)
		IF rtn = 2 THEN
			EXIT
		ELSE
			li_start_pos = search_pos + search_len
			li_start_pos = Pos(ls_text,ls_search,li_start_pos)
		END IF
	LOOP 
END IF






end event

type cb_later from commandbutton within w_qa_zedval
integer x = 2341
integer y = 2272
integer width = 407
integer height = 96
integer taborder = 160
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&Finish Later"
end type

event clicked;string ls_bkseq
long ll_bkseq

Luserid = UPPER(SQLserverTrans.userid)

dtb_status = 'H'
st_status.text = 'DTB Status is finish later by '+Luserid
st_status.TextColor = RGB(0,0,0)


IF Old_DTB_book = FALSE THEN
	lb_smil.SetState(1, TRUE)
	ls_bkseq = Mid(lb_smil.SelectedItem(), 1 , 5)
	ll_bkseq = Long(Mid(lb_smil.SelectedItem(), 1 , 5))
ELSE
	ll_bkseq = Old_DTB_bkseq
END IF

wf_updatedb(ll_bkseq)

end event

type st_smil from statictext within w_qa_zedval
integer x = 878
integer y = 1824
integer width = 402
integer height = 60
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "SMIL Files"
alignment alignment = right!
boolean focusrectangle = false
end type

type st_status from statictext within w_qa_zedval
string tag = "DTB Status"
integer x = 2523
integer y = 1696
integer width = 951
integer height = 64
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean underline = true
long textcolor = 33554432
long backcolor = 67108864
string text = "DTB Status"
boolean focusrectangle = false
end type

type st_comments from statictext within w_qa_zedval
string tag = "Comments"
integer x = 37
integer y = 1920
integer width = 2377
integer height = 64
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "Comments (double click on this text to edit comment in a bigger box)"
boolean focusrectangle = false
end type

event doubleclicked;String QAComments
QAComments = mle_comments.text
OpenWithParm(w_qa_popup_comments, QAComments)
 mle_comments.text=Message.StringParm

end event

type cbx_playbook_check from checkbox within w_qa_zedval
string tag = "Play Book Check"
integer x = 512
integer y = 2304
integer width = 549
integer height = 64
integer taborder = 120
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "Play Book Check"
boolean righttoleft = true
end type

type cb_t from commandbutton within w_qa_zedval
boolean visible = false
integer x = 3438
integer y = 1760
integer width = 73
integer height = 64
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
end type

event clicked;//////////////////////////////////////////////////////////////////////////////////////////////////////////
//
//	Event:  clicked for cb_t
//
//	Description: Retrieve old book or process new book
//	
//
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//	
//	Revision History
//
//	Developed by 	Date 				Version						Tracking#
//									
// Murali K.			06/30/2008      Process short, med and long results. Phase-2
// Murali K.			 07/03/2008     SHORT, MEDIUM, LONG RESULTS READONLY IF POPULATED FROM AUTOTEST 
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//

n_runandwait in_rwait
string ls_command,ls_del_xml_file1,ls_del_xml_file2, ls_msg,ls_comments,ls_old_comments,ls_dirlist,ls_userid, qas_init, ls_shortresult, ls_readonly, ls_text
ULong lul_rc
Integer li_report,li_nlsreport, lcnt, rtn, li_cntrfy, bkcnt = 0
Long ll_bkseq,li_bytes
blob report_blob,nlsreport_blob
Boolean Dlist = FALSE,bjaws
Char lc_opf_chk,lc_ncx_chk,lc_smil_chk,lc_reportxml_chk,lc_nlsreportxml_chk,lc_playbook_chk, lc_status,lc_dirlist_chk
Date date_reviewed

String ls_txtlines[ ], dirlist_file
Integer li_numstats,i
n_cst_string 	inv_string

bjaws = wf_isjawsrunning()
IF bjaws = TRUE THEN
	of_SetJaws(TRUE)
ELSE
	of_SetJaws(FALSE)
END IF

//Clear all the tabs
wf_clear_tabs()

// Display a message for new book.
rtn = Messagebox("DTB Books","Is this a new book?",question!, yesNoCancel!, 1)
IF rtn=1 THEN
	
	// New books

	ls_del_xml_file1 = "delete c:\report.xml"
	ls_del_xml_file2 = "delete c:\nlsreport.xml"
	
	Run(ls_del_xml_file1)
	Run(ls_del_xml_file2)
	
	IF Multi_Disc THEN
		ls_dirlist = sle_multidisc.Text + "\*.SMIL"
	ELSE
		ls_dirlist = lc_cdrom + "\*.SMIL"
	END IF
	
	Dlist = lb_smil.DirList(ls_dirlist, 1)
	
	IF NOT(IsNull(Dlist)) THEN
		lb_smil.SetState(1, TRUE)
	ELSE
		ls_msg = "Error in reading the "+ls_dirlist+" drive. Please make sure that "+ls_dirlist+" exist."
		MessageBox ("ERROR READING", ls_msg ,StopSign!)
		RETURN
	END IF

       // Get the book number
	ll_bkseq = Long(Mid(lb_smil.Text(1), 1, 5))
	
	SELECT COUNT(*) , QAINIT, DT_TIME,COMMENTS
	INTO :bkcnt, :qas_init, :date_reviewed,:ls_old_comments
	FROM DTB_DATA
	WHERE bkseq = :ll_bkseq
	GROUP BY QAINIT, DT_TIME,COMMENTS
	USING SqlServerTrans;
	
	IF bkcnt > 0 THEN
		ls_msg = "Book number " + string(ll_bkseq) + " has already been reviewed by "+qas_init+" on "+ string(date_reviewed,'MM/DD/YY')
		MessageBox("Books exist",ls_msg,Information!)
		w_qa_zedval.mle_comments.text = "***Old Comments start here*** "+ls_old_comments+" ***Old Comments end here ***"
	END IF	
	
	IF Multi_Disc THEN
		ls_dirlist = sle_multidisc.Text + "\*.*"
	ELSE
		ls_dirlist = lc_cdrom + "\*.*"
	END IF
	Dlist = tab_dtb.tabpage_dirlist.lb_dirlist.DirList(ls_dirlist, 1)
	
	IF NOT(IsNull(Dlist)) THEN
		tab_dtb.tabpage_dirlist.lb_dirlist.SetState(1, TRUE)
	ELSE
		ls_msg = "Error in reading the "+ls_dirlist+" drive. Please make sure that "+ls_dirlist+" exist."
		MessageBox ("ERROR READING", ls_msg ,StopSign!)
		RETURN
	END IF
	
	IF Multi_Disc THEN
		ls_command = "c:\runzv.bat "+ sle_multidisc.Text  + "\*.opf"
	ELSE
		ls_command = "c:\runzv.bat "+ lc_cdrom + "\*.opf"
	END IF

	SetPointer(HourGlass!)
	// Make sure you are in C drive
	lul_rc = in_rwait.of_run("C:", Normal!)
	// Make sure you are running runzv.bat at the root of C drive
	lul_rc = in_rwait.of_run("CD\", Normal!)
	// Run the command 
	lul_rc = in_rwait.of_run(ls_command, Normal!)
	
	
	// check return code
	CHOOSE CASE lul_rc
		CASE in_rwait.WAIT_COMPLETE
			ls_msg = "The process completed normally!"
		CASE in_rwait.WAIT_TIMEOUT
			ls_msg = "The process was terminated after 10 seconds!"
		CASE ELSE
			ls_msg = "The process completed with return code: " + String(lul_rc)
	END CHOOSE
	//MessageBox('DOS Command', ls_msg)
	
	li_report = FileOpen('c:\report.xml', StreamMode!)
	IF li_report <> -1 THEN
		li_bytes = FileRead(li_report, report_blob)
	END IF 
	
	li_nlsreport = FileOpen('c:\nlsreport.xml', StreamMode!)
	IF li_nlsreport <> -1 THEN
		li_bytes = FileRead(li_nlsreport, nlsreport_blob)
	END IF
	
	FileClose(li_report)
	FileClose(li_nlsreport)
	
	tab_dtb.tabpage_nlsreportxml.mle_nlsreportxml.text =  string(nlsreport_blob, EncodingANSI!)
	tab_dtb.tabpage_reportxml.mle_reportxml.text = string(report_blob, EncodingANSI!)
	
	ll_bkseq = Long(Mid(lb_smil.Text(1), 1, 5))
	
	SELECT cntr, cntrfy
	INTO :ls_cntr, :li_cntrfy
	FROM ancntr
	WHERE cntr IN ( SELECT cntr 
						FROM prod
						WHERE bkseq=:ll_bkseq
						AND prodstage in ('ZM', 'DT'))
	USING SqlServerTrans;
	
	sle_cntryr.text = string(li_cntrfy)
	
	wf_extract_opf()
	
	tab_dtb.tabpage_reportxml.mle_reportxml.SetFocus()
	
	Old_DTB_book = FALSE
	
		// 10/23/2008 if checked in make tabs readonly
		of_checkincheck(ll_bkseq)
		
ELSEIF rtn=2 THEN

	Open(w_qa_select_dtb_book)
	// If book was selected
	IF IsNull(message.StringParm)=FALSE AND message.StringParm<>"" THEN
		Old_DTB_book = TRUE
		ll_bkseq = Long(message.StringParm)
		
		// 06/30/2008 short result
		
		
		SELECT opf_chk, ncx_chk, smil_chk, reportxml_chk, nlsreportxml_chk, playbook_chk, dirlist_chk, comments, 
		dc_date, dc_format, dtb_multimediatype, dtb_audioformat, dtb_depth, navpoints, dtb_totaltime, dtb_produceddate,
		UPPER(status), UPPER(qainit), count(*) , short_result, read_only_yn
		INTO :lc_opf_chk, :lc_ncx_chk, :lc_smil_chk, :lc_reportxml_chk, :lc_nlsreportxml_chk, :lc_playbook_chk, :lc_dirlist_chk, :ls_comments,
		:ls_dcdate, :ls_dcformat, :ls_multimediatype, :ls_audioformat, :ls_depth, :lcnt_navpoint, :TotalTime, :ld_produceddate,
		:lc_status, :ls_userid, :lcnt , :ls_shortresult, :is_readonly
		FROM dtb_data
		WHERE bkseq=:ll_bkseq
		GROUP BY opf_chk, ncx_chk, smil_chk, reportxml_chk, nlsreportxml_chk, playbook_chk, dirlist_chk, comments, 
		dc_date, dc_format, dtb_multimediatype, dtb_audioformat, dtb_depth, navpoints, dtb_totaltime, dtb_produceddate,
		status, qainit, short_result, read_only_yn
		USING SqlServerTrans;
			
		IF lcnt > 0 THEN

			 lstr_db_data.dc_date = ls_dcdate
			 lstr_db_data.dc_format = ls_dcformat
			 lstr_db_data.dtb_multimediatype = ls_multimediatype
			 lstr_db_data.dtb_produceddate = ld_produceddate
			 lstr_db_data.dtb_totaltime = TotalTime
			 lstr_db_data.dtb_audioformat = ls_audioformat
			 lstr_db_data.dtb_depth = ls_depth
			 lstr_db_data.navpoint = lcnt_navpoint
		
		// Do something
			IF  lc_opf_chk = 'Y'  THEN
				tab_dtb.tabpage_opf.cbx_opf_check.checked = TRUE
			ELSE
				tab_dtb.tabpage_opf.cbx_opf_check.checked = FALSE
			END IF
				
			IF  lc_playbook_chk = 'Y'  THEN
				w_qa_zedval.cbx_playbook_check.checked = TRUE
			ELSE
				w_qa_zedval.cbx_playbook_check.checked = FALSE
			END IF
		
			IF  lc_smil_chk = 'Y'  THEN
				tab_dtb.tabpage_all_smil.cbx_smil_check.checked = TRUE
				tab_dtb.tabpage_selected_smil.cbx_selsmil_chk.checked = TRUE
			ELSE
				tab_dtb.tabpage_all_smil.cbx_smil_check.checked = FALSE
				tab_dtb.tabpage_selected_smil.cbx_selsmil_chk.checked = FALSE
			END IF
		
			IF  lc_ncx_chk = 'Y'  THEN
				tab_dtb.tabpage_ncx.cbx_ncx_check.checked = TRUE
			ELSE
				tab_dtb.tabpage_ncx.cbx_ncx_check.checked = FALSE
			END IF
		
			IF  lc_nlsreportxml_chk = 'Y'  THEN
				tab_dtb.tabpage_nlsreportxml.cbx_nlsreportxml_check.checked = TRUE
			ELSE
				tab_dtb.tabpage_nlsreportxml.cbx_nlsreportxml_check.checked = FALSE
			END IF
		
			IF  lc_reportxml_chk = 'Y'  THEN
				tab_dtb.tabpage_reportxml.cbx_reportxml_check.checked = TRUE
			ELSE
				tab_dtb.tabpage_reportxml.cbx_reportxml_check.checked = FALSE
			END IF
		
			w_qa_zedval.mle_comments.text = ls_comments
			// 06/30/2008 set short result 
			// 07/16/2008 remove special characters
			ls_text = ls_shortresult
			li_numstats = inv_string.of_Parsetoarray (ls_text, "~n", ls_txtlines)
			FOR i = 1 TO li_numstats
				ls_txtlines[i] = inv_string.of_RemoveNonPrint(ls_txtlines[i])
				ls_txtlines[i] = inv_string.of_Trim(ls_txtlines[i])
			NEXT
			rtn = inv_string.of_ArrayToString(ls_txtlines,"~r~n",ls_text)
			tab_dtb.tabpage_shortresult.mle_shortresult.text = ls_text

			IF lc_status = 'A' THEN
				st_status.text = 'DTB Status is approved by ' + ls_userid
				st_status.TextColor = RGB(0,0,255)
			ELSEIF lc_status = 'R' THEN
				st_status.Text = 'DTB Status is rejected by ' + ls_userid
				st_status.TextColor = RGB(255,0,0)
			ELSEIF lc_status = 'H' THEN
				st_status.Text = 'DTB Status is finish later by ' + ls_userid
				st_status.TextColor = RGB(0,0,0)
			ELSE
				st_status.Text = 'DTB Status '		
			END IF 
			
			// get the XML reports which has already been extracted and saved in DTB_DATA table
			wf_get_blobs(ll_bkseq)

			tab_dtb.tabpage_reportxml.mle_reportxml.SetFocus()
		
			// Set this variable to True since this book is an old book
			Old_DTB_book = TRUE
			
			// 10/23/2008 if checked in make tabs readonly
			of_checkincheck(ll_bkseq)
	
		ELSE
			
			// Book does not exist in database.
			MessageBox("DTB Error","This book does not exist in database.")
			RETURN
		
		END IF
	ELSE
		cb_refresh.TriggerEvent(clicked!)
		RETURN
	END IF	
ELSE
	RETURN
END IF
end event

type cb_refresh from commandbutton within w_qa_zedval
integer x = 3145
integer y = 2272
integer width = 256
integer height = 96
integer taborder = 180
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&Refresh"
end type

event clicked;lb_smil.Reset ( )
tab_dtb.tabpage_dirlist.lb_dirlist.Reset ( )
sle_cntryr.text = ""
st_status.text = "DTB Status"
st_status.TextColor = RGB(0,0,0)
tab_dtb.tabpage_opf.cbx_opf_check.checked = FALSE
w_qa_zedval.cbx_playbook_check.checked = FALSE
tab_dtb.tabpage_all_smil.cbx_smil_check.checked = FALSE
tab_dtb.tabpage_selected_smil.cbx_selsmil_chk.checked = FALSE
tab_dtb.tabpage_ncx.cbx_ncx_check.checked = FALSE
tab_dtb.tabpage_nlsreportxml.cbx_nlsreportxml_check.checked = FALSE
tab_dtb.tabpage_reportxml.cbx_reportxml_check.checked = FALSE

mle_comments.text = ""
mle_opf_ncx.text = ""
tab_dtb.tabpage_nlsreportxml.mle_nlsreportxml.text = ""
tab_dtb.tabpage_reportxml.mle_reportxml.text = ""

ls_dcdate=""
ls_dcformat=""
ls_multimediatype=""
ls_audioformat=""
ls_depth=""
ls_allnavlist=""
TotalTime = 0
lcnt_navpoint = 0
w_pics_main.setmicrohelp("")

Old_DTB_book = FALSE

cb_t.TriggerEvent(Clicked!)

end event

type lb_smil from listbox within w_qa_zedval
string tag = "SMIL Files"
integer x = 1280
integer y = 1792
integer width = 512
integer height = 96
integer taborder = 50
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
boolean vscrollbar = true
boolean multiselect = true
borderstyle borderstyle = stylelowered!
end type

type cb_play from commandbutton within w_qa_zedval
integer x = 37
integer y = 2272
integer width = 439
integer height = 96
integer taborder = 110
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&Play the book"
end type

event clicked;//n_runandwait in_rwait
//ULong lul_rc
//String ls_command,ls_msg
Inet linet_base

GetContextService("Internet",linet_base)
linet_base.HyperlinkToURL("C:\nls-player\frame2.html")

If Isvalid(linet_base) Then destroy linet_base

// Run the command 
//ls_command = "C:\Program Files\aph\Book Wizard Reader\bwr.exe"
//
//lul_rc = in_rwait.of_run(ls_command, Normal!)
//
//// check return code
//CHOOSE CASE lul_rc
//	CASE in_rwait.WAIT_COMPLETE
//		ls_msg = "The process completed normally!"
//	CASE in_rwait.WAIT_TIMEOUT
//		ls_msg = "The process was terminated after 10 seconds!"
//	CASE ELSE
//		ls_msg = "The process completed with return code: " + String(lul_rc)
//END CHOOSE

end event

type cb_reject from commandbutton within w_qa_zedval
integer x = 2048
integer y = 2272
integer width = 265
integer height = 96
integer taborder = 150
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Re&ject"
end type

event clicked;string ls_bkseq
long ll_bkseq

Luserid = UPPER(SQLserverTrans.userid)

dtb_status = 'R'
st_status.Text = 'DTB Status is rejected by '+Luserid
st_status.TextColor = RGB(255,0,0)

IF Old_DTB_book = FALSE THEN
	lb_smil.SetState(1, TRUE)
	ls_bkseq = Mid(lb_smil.SelectedItem(), 1 , 5)
	ll_bkseq = Long(Mid(lb_smil.SelectedItem(), 1 , 5))
ELSE
	ll_bkseq = Old_DTB_bkseq
END IF

wf_updatedb(ll_bkseq)

end event

type cb_approve from commandbutton within w_qa_zedval
integer x = 2779
integer y = 2272
integer width = 338
integer height = 96
integer taborder = 170
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&Approve"
end type

event clicked;string ls_bkseq
long ll_bkseq

Luserid = UPPER(SQLserverTrans.userid)

dtb_status = 'A'
st_status.text = 'DTB Status is approved by '+Luserid
st_status.TextColor = RGB(0,0,255)

IF Old_DTB_book = FALSE THEN
	lb_smil.SetState(1, TRUE)
	ls_bkseq = Mid(lb_smil.SelectedItem(), 1 , 5)
	ll_bkseq = Long(Mid(lb_smil.SelectedItem(), 1 , 5))
ELSE
	ll_bkseq = Old_DTB_bkseq
END IF


wf_updatedb(ll_bkseq)

end event

type cb_exit from commandbutton within w_qa_zedval
integer x = 3438
integer y = 2272
integer width = 256
integer height = 96
integer taborder = 190
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "E&xit"
end type

event clicked;w_pics_main.setmicrohelp("")
close (parent)
end event

type tab_dtb from tab within w_qa_zedval
string tag = "DTB Tab Control"
integer width = 3694
integer height = 1664
integer taborder = 10
boolean bringtotop = true
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 79741120
boolean raggedright = true
boolean focusonbuttondown = true
boolean boldselectedtext = true
boolean pictureonright = true
integer selectedtab = 1
tabpage_reportxml tabpage_reportxml
tabpage_nlsreportxml tabpage_nlsreportxml
tabpage_opf tabpage_opf
tabpage_ncx tabpage_ncx
tabpage_all_smil tabpage_all_smil
tabpage_selected_smil tabpage_selected_smil
tabpage_dirlist tabpage_dirlist
tabpage_shortresult tabpage_shortresult
tabpage_mediumresult tabpage_mediumresult
tabpage_longresult tabpage_longresult
end type

on tab_dtb.create
this.tabpage_reportxml=create tabpage_reportxml
this.tabpage_nlsreportxml=create tabpage_nlsreportxml
this.tabpage_opf=create tabpage_opf
this.tabpage_ncx=create tabpage_ncx
this.tabpage_all_smil=create tabpage_all_smil
this.tabpage_selected_smil=create tabpage_selected_smil
this.tabpage_dirlist=create tabpage_dirlist
this.tabpage_shortresult=create tabpage_shortresult
this.tabpage_mediumresult=create tabpage_mediumresult
this.tabpage_longresult=create tabpage_longresult
this.Control[]={this.tabpage_reportxml,&
this.tabpage_nlsreportxml,&
this.tabpage_opf,&
this.tabpage_ncx,&
this.tabpage_all_smil,&
this.tabpage_selected_smil,&
this.tabpage_dirlist,&
this.tabpage_shortresult,&
this.tabpage_mediumresult,&
this.tabpage_longresult}
end on

on tab_dtb.destroy
destroy(this.tabpage_reportxml)
destroy(this.tabpage_nlsreportxml)
destroy(this.tabpage_opf)
destroy(this.tabpage_ncx)
destroy(this.tabpage_all_smil)
destroy(this.tabpage_selected_smil)
destroy(this.tabpage_dirlist)
destroy(this.tabpage_shortresult)
destroy(this.tabpage_mediumresult)
destroy(this.tabpage_longresult)
end on

event selectionchanged;//////////////////////////////////////////////////////////////////////////////////////////////////////////
//
//	Event: selectionchanged for tab
//
//	Description:
//	 Set the cd rom or path appropriately. lc_cdrom var already set and can be changed
//  by user
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//	
//	Revision History
//
//	Developed by 	Date 				Version						Purpose/Tracking#
//									
// Murali K.			02/22/2008      005 PICS Modifications	 Reqs: QAS a.4b.1, a.4b.2
// Murali K.			03/07/2008		2.0							If a new book set from selection 
//																			from the file path dialog box
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//

Int ll_rows,rtn,lcnt, li_numstats,i,li_smilfile,total, n
Long ll_bkseq
String ls_bkseq,ls_text,vs_smil, ls_smilfile,ls_txtlines[ ],ls_msg
Blob smil_blob_one, smil_blob_all
n_cst_string 	inv_string

lb_smil.SetState(1, TRUE)
ls_bkseq = Mid(lb_smil.SelectedItem(), 1 , 5)
ll_bkseq = Long(Mid(lb_smil.SelectedItem(), 1 , 5))

// 03/07/2008 if a new book set from selection from the file path dialog box
IF Isnull(ls_bkseq) OR Len(Trim(ls_bkseq)) = 0 THEN
	ls_bkseq = is_docname
END IF
///////////////////


IF newindex = 2 THEN
	// NLS Report.xml
	
ELSEIF newindex = 3 THEN
	// If the book is an old one we don't need to get the OPF file
	IF Old_DTB_book = FALSE THEN
		//	OPF tab
		Integer li_opffile
		String vs_source
		Blob opf_blob
		
		SetNull(ls_text)
		
		IF Multi_disc = TRUE THEN
			vs_source = ls_multidisk_dirlist +"\"+ls_bkseq+".opf"
		ELSE
//			lc_cdrom = lb_cdrom.SelectedItem() // 02/22/2008
			vs_source = lc_cdrom +"\"+ls_bkseq+".opf"
		END IF	
	
		IF NOT(FileExists(vs_source)) THEN 
				  Messagebox("ERROR in file open",vs_source + " OPF file does not exist")
			 RETURN
		END IF 
		
		li_opffile = FileOpen(vs_source, streamMode!)
		IF li_opffile <> -1 THEN
			FileRead(li_opffile, opf_blob)
			FileClose(li_opffile)
		ELSEIF li_opffile = -1 THEN
				  Messagebox("ERROR in reading","Can not read the OPF file")
			 RETURN
		END IF
		
		ls_text = String(opf_blob, EncodingANSI!)
	
		li_numstats = inv_string.of_Parsetoarray (ls_text, "~n", ls_txtlines)
		FOR i = 1 TO li_numstats
			ls_txtlines[i] = inv_string.of_RemoveNonPrint(ls_txtlines[i])
			ls_txtlines[i] = inv_string.of_Trim(ls_txtlines[i])
		NEXT
		rtn = inv_string.of_ArrayToString(ls_txtlines,"~r~n",ls_text)
		
//		MessageBox("length of OPF file",string(len(ls_text))+' characters')		
              ls_msg = 'length of OPF file is '+string(len(ls_text))+' characters'
		w_pics_main.setmicrohelp(ls_msg)		
		
		tab_dtb.tabpage_opf.mle_opf.text = ""
		tab_dtb.tabpage_opf.mle_opf.text = ls_text
	END IF

	tab_dtb.tabpage_opf.mle_opf.SetFocus()
ELSEIF newindex = 4 THEN
	// If the book is an old one we don't need to get the NCX file
	IF Old_DTB_book = FALSE THEN
		//	NCX tab
		Integer li_ncxfile
		String vs_ncx
		Blob ncx_blob
		
		SetNull(ls_text)
		
		IF Multi_disc = TRUE THEN
			vs_ncx = ls_multidisk_dirlist +"\"+ls_bkseq+".ncx"
		ELSE
//			lc_cdrom = lb_cdrom.SelectedItem() // 02/22/2008
			vs_ncx = lc_cdrom +"\"+ls_bkseq+".ncx"
		END IF	
	
		IF NOT(FileExists(vs_ncx)) THEN 
			  Messagebox("ERROR in file open",vs_ncx + " NCX file does not exist")
			 RETURN
		END IF 
		
		li_ncxfile = FileOpen(vs_ncx, streamMode!)
		IF li_ncxfile <> -1 THEN
			FileRead(li_ncxfile, ncx_blob)
			FileClose(li_ncxfile)
		ELSEIF li_ncxfile = -1 THEN
				  Messagebox("ERROR in reading","Can not read the NCX file")
			 RETURN
		END IF
		
		ls_text = String(ncx_blob, EncodingANSI!)
	
		li_numstats = inv_string.of_Parsetoarray (ls_text, "~n", ls_txtlines)
		FOR i = 1 TO li_numstats
			ls_txtlines[i] = inv_string.of_RemoveNonPrint(ls_txtlines[i])
			ls_txtlines[i] = inv_string.of_Trim(ls_txtlines[i])
		NEXT
		rtn = inv_string.of_ArrayToString(ls_txtlines,"~r~n",ls_text)
		
//		MessageBox("length of NCX file",string(len(ls_text))+' characters')		
              ls_msg = 'length of NCX file is '+string(len(ls_text))+' characters'
		w_pics_main.setmicrohelp(ls_msg)		
		
		tab_dtb.tabpage_ncx.mle_ncx.text = ""
		tab_dtb.tabpage_ncx.mle_ncx.text = ls_text
	END IF

	tab_dtb.tabpage_ncx.mle_ncx.SetFocus()
	
ELSEIF newindex = 5 THEN
	
	// If the book is an old one we don't need to get the SMIL file
	IF Old_DTB_book = FALSE THEN

		//	All SMIL files
		SetNull(ls_text)
		
		total = lb_smil.TotalItems()
		
		FOR n = 1 TO total
			ls_smilfile = lb_smil.Text(n)
			IF Multi_disc = TRUE THEN
				vs_smil = ls_multidisk_dirlist +"\"+ls_smilfile
			ELSE
//				lc_cdrom = lb_cdrom.SelectedItem() // 02/22/2008
				vs_smil = lc_cdrom +"\"+ls_smilfile
			END IF	
			li_smilfile = FileOpen(vs_smil, streamMode!)
			IF li_smilfile <> -1 THEN
				FileRead(li_smilfile, smil_blob_one)
				FileClose(li_smilfile)
			END IF
			smil_blob_all += smil_blob_one
		NEXT
		
		
		ls_text = String(smil_blob_all, EncodingANSI!)
	
//		li_numstats = inv_string.of_Parsetoarray (ls_text, "~n", ls_txtlines)
//		FOR i = 1 TO li_numstats
//			ls_txtlines[i] = inv_string.of_RemoveNonPrint(ls_txtlines[i])
//			ls_txtlines[i] = inv_string.of_Trim(ls_txtlines[i])
//		NEXT
//		rtn = inv_string.of_ArrayToString(ls_txtlines,"~r~n",ls_text)
		
//		MessageBox("length of All SMIL files",string(len(ls_text))+' characters')		
              ls_msg = 'length of All SMIL files are '+string(len(ls_text))+' characters'
		w_pics_main.setmicrohelp(ls_msg)		
		
		tab_dtb.tabpage_all_smil.mle_allsmil.text = ""
		tab_dtb.tabpage_all_smil.mle_allsmil.text = ls_text
	END IF

	tab_dtb.tabpage_all_smil.mle_allsmil.SetFocus()


ELSEIF newindex = 6 THEN
	// If the book is an old one we don't need to get the SMIL file
	IF Old_DTB_book = FALSE THEN
		//	Selected SMIL file(s)
		
		total = lb_smil.TotalItems()

		SetNull(ls_text)
		
		FOR n = 1 TO total
			IF lb_smil.State(n) = 1 THEN 
				ls_smilfile = lb_smil.Text(n)
				IF Multi_disc = TRUE THEN
					vs_smil = ls_multidisk_dirlist +"\"+ls_smilfile
				ELSE
//					lc_cdrom = lb_cdrom.SelectedItem() //02/22/2008
					vs_smil = lc_cdrom +"\"+ls_smilfile
				END IF	
				li_smilfile = FileOpen(vs_smil, streamMode!)
				IF li_smilfile <> -1 THEN
					FileRead(li_smilfile, smil_blob_one)
					FileClose(li_smilfile)
				END IF
				smil_blob_all += smil_blob_one
			END IF
		NEXT
		
		ls_text = String(smil_blob_all, EncodingANSI!)
	
//		li_numstats = inv_string.of_Parsetoarray (ls_text, "~n", ls_txtlines)
//		FOR i = 1 TO li_numstats
//			ls_txtlines[i] = inv_string.of_RemoveNonPrint(ls_txtlines[i])
//			ls_txtlines[i] = inv_string.of_Trim(ls_txtlines[i])
//		NEXT
//		rtn = inv_string.of_ArrayToString(ls_txtlines,"~r~n",ls_text)
		
//		MessageBox("length of selected SMIL file",string(len(ls_text))+' characters')		
              ls_msg = 'length of selected SMIL file is '+string(len(ls_text))+' characters'
		w_pics_main.setmicrohelp(ls_msg)		
		
		tab_dtb.tabpage_selected_smil.mle_selsmils.text  = ""
		tab_dtb.tabpage_selected_smil.mle_selsmils.text  = ls_text
	END IF
	tab_dtb.tabpage_selected_smil.mle_selsmils.SetFocus()

END IF	
end event

type tabpage_reportxml from userobject within tab_dtb
event create ( )
event destroy ( )
integer x = 18
integer y = 112
integer width = 3657
integer height = 1536
long backcolor = 79741120
string text = "Report XML"
long tabtextcolor = 33554432
long tabbackcolor = 79741120
long picturemaskcolor = 536870912
cbx_reportxml_check cbx_reportxml_check
mle_reportxml mle_reportxml
end type

on tabpage_reportxml.create
this.cbx_reportxml_check=create cbx_reportxml_check
this.mle_reportxml=create mle_reportxml
this.Control[]={this.cbx_reportxml_check,&
this.mle_reportxml}
end on

on tabpage_reportxml.destroy
destroy(this.cbx_reportxml_check)
destroy(this.mle_reportxml)
end on

type cbx_reportxml_check from checkbox within tabpage_reportxml
string tag = "ReportXMLCheck"
integer x = 160
integer y = 1404
integer width = 549
integer height = 64
integer taborder = 60
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "ReportXMLCheck"
boolean lefttext = true
boolean righttoleft = true
end type

type mle_reportxml from multilineedit within tabpage_reportxml
string tag = "Report. XML"
integer x = 18
integer y = 16
integer width = 3621
integer height = 1344
integer taborder = 30
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
boolean vscrollbar = true
boolean autovscroll = true
integer tabstop[] = {0}
borderstyle borderstyle = stylelowered!
boolean hideselection = false
end type

type tabpage_nlsreportxml from userobject within tab_dtb
event create ( )
event destroy ( )
integer x = 18
integer y = 112
integer width = 3657
integer height = 1536
long backcolor = 79741120
string text = "NLS Report XML"
long tabtextcolor = 33554432
long tabbackcolor = 79741120
long picturemaskcolor = 536870912
cbx_nlsreportxml_check cbx_nlsreportxml_check
mle_nlsreportxml mle_nlsreportxml
end type

on tabpage_nlsreportxml.create
this.cbx_nlsreportxml_check=create cbx_nlsreportxml_check
this.mle_nlsreportxml=create mle_nlsreportxml
this.Control[]={this.cbx_nlsreportxml_check,&
this.mle_nlsreportxml}
end on

on tabpage_nlsreportxml.destroy
destroy(this.cbx_nlsreportxml_check)
destroy(this.mle_nlsreportxml)
end on

type cbx_nlsreportxml_check from checkbox within tabpage_nlsreportxml
string tag = "NLSReportXML Check"
integer x = 69
integer y = 1396
integer width = 731
integer height = 64
integer taborder = 50
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "NLSReportXML Check"
boolean lefttext = true
boolean righttoleft = true
end type

type mle_nlsreportxml from multilineedit within tabpage_nlsreportxml
string tag = "NLS Report XML"
integer x = 18
integer y = 16
integer width = 3584
integer height = 1344
integer taborder = 30
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
boolean vscrollbar = true
boolean autovscroll = true
integer tabstop[] = {0}
borderstyle borderstyle = stylelowered!
boolean hideselection = false
end type

type tabpage_opf from userobject within tab_dtb
integer x = 18
integer y = 112
integer width = 3657
integer height = 1536
long backcolor = 79741120
string text = "OPF File"
long tabtextcolor = 33554432
long tabbackcolor = 79741120
long picturemaskcolor = 536870912
cbx_opf_check cbx_opf_check
mle_opf mle_opf
end type

on tabpage_opf.create
this.cbx_opf_check=create cbx_opf_check
this.mle_opf=create mle_opf
this.Control[]={this.cbx_opf_check,&
this.mle_opf}
end on

on tabpage_opf.destroy
destroy(this.cbx_opf_check)
destroy(this.mle_opf)
end on

type cbx_opf_check from checkbox within tabpage_opf
string tag = "OPF Check"
integer x = 137
integer y = 1388
integer width = 439
integer height = 64
integer taborder = 20
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "OPF Check"
boolean lefttext = true
boolean righttoleft = true
end type

type mle_opf from multilineedit within tabpage_opf
string tag = "OPF File"
integer x = 18
integer y = 16
integer width = 3584
integer height = 1344
integer taborder = 40
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
boolean vscrollbar = true
boolean autovscroll = true
integer tabstop[] = {0}
borderstyle borderstyle = stylelowered!
boolean hideselection = false
end type

type tabpage_ncx from userobject within tab_dtb
integer x = 18
integer y = 112
integer width = 3657
integer height = 1536
long backcolor = 79741120
string text = "NCX File"
long tabtextcolor = 33554432
long tabbackcolor = 79741120
long picturemaskcolor = 536870912
cbx_ncx_check cbx_ncx_check
mle_ncx mle_ncx
end type

on tabpage_ncx.create
this.cbx_ncx_check=create cbx_ncx_check
this.mle_ncx=create mle_ncx
this.Control[]={this.cbx_ncx_check,&
this.mle_ncx}
end on

on tabpage_ncx.destroy
destroy(this.cbx_ncx_check)
destroy(this.mle_ncx)
end on

type cbx_ncx_check from checkbox within tabpage_ncx
string tag = "NCX Check"
integer x = 183
integer y = 1384
integer width = 443
integer height = 64
integer taborder = 30
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "NCX Check"
boolean lefttext = true
boolean righttoleft = true
end type

type mle_ncx from multilineedit within tabpage_ncx
string tag = "NCX File"
integer x = 18
integer y = 16
integer width = 3584
integer height = 1344
integer taborder = 50
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
boolean vscrollbar = true
boolean autovscroll = true
integer tabstop[] = {0}
borderstyle borderstyle = stylelowered!
boolean hideselection = false
end type

type tabpage_all_smil from userobject within tab_dtb
integer x = 18
integer y = 112
integer width = 3657
integer height = 1536
long backcolor = 79741120
string text = "All SMILs"
long tabtextcolor = 33554432
long tabbackcolor = 79741120
long picturemaskcolor = 536870912
cbx_smil_check cbx_smil_check
mle_allsmil mle_allsmil
end type

on tabpage_all_smil.create
this.cbx_smil_check=create cbx_smil_check
this.mle_allsmil=create mle_allsmil
this.Control[]={this.cbx_smil_check,&
this.mle_allsmil}
end on

on tabpage_all_smil.destroy
destroy(this.cbx_smil_check)
destroy(this.mle_allsmil)
end on

type cbx_smil_check from checkbox within tabpage_all_smil
string tag = "SMIL(s) Check"
integer x = 219
integer y = 1368
integer width = 475
integer height = 64
integer taborder = 40
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "SMIL(s) Check"
boolean lefttext = true
boolean righttoleft = true
end type

type mle_allsmil from multilineedit within tabpage_all_smil
string tag = "ALL SMIL Files"
integer x = 18
integer y = 16
integer width = 3584
integer height = 1344
integer taborder = 20
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
boolean vscrollbar = true
boolean autovscroll = true
integer tabstop[] = {0}
borderstyle borderstyle = stylelowered!
boolean hideselection = false
end type

type tabpage_selected_smil from userobject within tab_dtb
integer x = 18
integer y = 112
integer width = 3657
integer height = 1536
long backcolor = 79741120
string text = "Selected SMILs"
long tabtextcolor = 33554432
long tabbackcolor = 79741120
long picturemaskcolor = 536870912
cbx_selsmil_chk cbx_selsmil_chk
mle_selsmils mle_selsmils
end type

on tabpage_selected_smil.create
this.cbx_selsmil_chk=create cbx_selsmil_chk
this.mle_selsmils=create mle_selsmils
this.Control[]={this.cbx_selsmil_chk,&
this.mle_selsmils}
end on

on tabpage_selected_smil.destroy
destroy(this.cbx_selsmil_chk)
destroy(this.mle_selsmils)
end on

type cbx_selsmil_chk from checkbox within tabpage_selected_smil
string tag = "SMIL(s) Check"
integer x = 279
integer y = 1392
integer width = 475
integer height = 64
integer taborder = 40
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "SMIL(s) Check"
boolean lefttext = true
boolean righttoleft = true
end type

type mle_selsmils from multilineedit within tabpage_selected_smil
string tag = "Selected SMIL Files"
integer x = 18
integer y = 16
integer width = 3584
integer height = 1344
integer taborder = 30
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
boolean vscrollbar = true
boolean autovscroll = true
integer tabstop[] = {0}
borderstyle borderstyle = stylelowered!
boolean hideselection = false
end type

type tabpage_dirlist from userobject within tab_dtb
integer x = 18
integer y = 112
integer width = 3657
integer height = 1536
long backcolor = 79741120
string text = "Directory List"
long tabtextcolor = 33554432
long tabbackcolor = 79741120
long picturemaskcolor = 536870912
cbx_dirlist cbx_dirlist
lb_dirlist lb_dirlist
end type

on tabpage_dirlist.create
this.cbx_dirlist=create cbx_dirlist
this.lb_dirlist=create lb_dirlist
this.Control[]={this.cbx_dirlist,&
this.lb_dirlist}
end on

on tabpage_dirlist.destroy
destroy(this.cbx_dirlist)
destroy(this.lb_dirlist)
end on

type cbx_dirlist from checkbox within tabpage_dirlist
string tag = "DirList Check"
integer x = 91
integer y = 1424
integer width = 475
integer height = 64
integer taborder = 40
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "DirList Check"
boolean lefttext = true
boolean righttoleft = true
end type

type lb_dirlist from listbox within tabpage_dirlist
string tag = "Directory List"
integer x = 18
integer y = 16
integer width = 3438
integer height = 1376
integer taborder = 40
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
boolean hscrollbar = true
boolean vscrollbar = true
integer tabstop[] = {0}
borderstyle borderstyle = stylelowered!
end type

type tabpage_shortresult from userobject within tab_dtb
integer x = 18
integer y = 112
integer width = 3657
integer height = 1536
long backcolor = 79741120
string text = "Short Result"
long tabtextcolor = 33554432
long tabbackcolor = 79741120
long picturemaskcolor = 536870912
mle_shortresult mle_shortresult
end type

on tabpage_shortresult.create
this.mle_shortresult=create mle_shortresult
this.Control[]={this.mle_shortresult}
end on

on tabpage_shortresult.destroy
destroy(this.mle_shortresult)
end on

type mle_shortresult from multilineedit within tabpage_shortresult
integer x = 27
integer y = 32
integer width = 3593
integer height = 1492
integer taborder = 40
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
boolean vscrollbar = true
boolean autovscroll = true
borderstyle borderstyle = stylelowered!
boolean hideselection = false
end type

type tabpage_mediumresult from userobject within tab_dtb
integer x = 18
integer y = 112
integer width = 3657
integer height = 1536
long backcolor = 79741120
string text = "Medium Result"
long tabtextcolor = 33554432
long tabbackcolor = 79741120
long picturemaskcolor = 536870912
mle_mediumresult mle_mediumresult
end type

on tabpage_mediumresult.create
this.mle_mediumresult=create mle_mediumresult
this.Control[]={this.mle_mediumresult}
end on

on tabpage_mediumresult.destroy
destroy(this.mle_mediumresult)
end on

type mle_mediumresult from multilineedit within tabpage_mediumresult
integer x = 18
integer y = 20
integer width = 3598
integer height = 1500
integer taborder = 50
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
boolean vscrollbar = true
boolean autovscroll = true
borderstyle borderstyle = stylelowered!
boolean hideselection = false
end type

type tabpage_longresult from userobject within tab_dtb
integer x = 18
integer y = 112
integer width = 3657
integer height = 1536
long backcolor = 79741120
string text = "Long Result"
long tabtextcolor = 33554432
long tabbackcolor = 79741120
long picturemaskcolor = 536870912
mle_longresult mle_longresult
end type

on tabpage_longresult.create
this.mle_longresult=create mle_longresult
this.Control[]={this.mle_longresult}
end on

on tabpage_longresult.destroy
destroy(this.mle_longresult)
end on

type mle_longresult from multilineedit within tabpage_longresult
integer x = 23
integer y = 12
integer width = 3607
integer height = 1524
integer taborder = 50
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
boolean vscrollbar = true
boolean autovscroll = true
borderstyle borderstyle = stylelowered!
boolean hideselection = false
end type


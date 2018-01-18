$PBExportHeader$w_qa_autotest_results.srw
forward
global type w_qa_autotest_results from w_response
end type
type cb_exit from commandbutton within w_qa_autotest_results
end type
type tab_dtb from tab within w_qa_autotest_results
end type
type tabpage_reportxml from userobject within tab_dtb
end type
type mle_reportxml from multilineedit within tabpage_reportxml
end type
type tabpage_reportxml from userobject within tab_dtb
mle_reportxml mle_reportxml
end type
type tabpage_nlsreportxml from userobject within tab_dtb
end type
type mle_nlsreportxml from multilineedit within tabpage_nlsreportxml
end type
type tabpage_nlsreportxml from userobject within tab_dtb
mle_nlsreportxml mle_nlsreportxml
end type
type tabpage_opf from userobject within tab_dtb
end type
type mle_opf from multilineedit within tabpage_opf
end type
type tabpage_opf from userobject within tab_dtb
mle_opf mle_opf
end type
type tabpage_ncx from userobject within tab_dtb
end type
type mle_ncx from multilineedit within tabpage_ncx
end type
type tabpage_ncx from userobject within tab_dtb
mle_ncx mle_ncx
end type
type tabpage_dirlist from userobject within tab_dtb
end type
type mle_smil from multilineedit within tabpage_dirlist
end type
type tabpage_dirlist from userobject within tab_dtb
mle_smil mle_smil
end type
type tabpage_opfncx from userobject within tab_dtb
end type
type mle_opfncx from multilineedit within tabpage_opfncx
end type
type tabpage_opfncx from userobject within tab_dtb
mle_opfncx mle_opfncx
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
type tab_dtb from tab within w_qa_autotest_results
tabpage_reportxml tabpage_reportxml
tabpage_nlsreportxml tabpage_nlsreportxml
tabpage_opf tabpage_opf
tabpage_ncx tabpage_ncx
tabpage_dirlist tabpage_dirlist
tabpage_opfncx tabpage_opfncx
tabpage_shortresult tabpage_shortresult
tabpage_mediumresult tabpage_mediumresult
tabpage_longresult tabpage_longresult
end type
end forward

global type w_qa_autotest_results from w_response
integer x = 214
integer y = 221
integer width = 4091
integer height = 2008
string title = "Autotest Results"
boolean ib_isupdateable = false
cb_exit cb_exit
tab_dtb tab_dtb
end type
global w_qa_autotest_results w_qa_autotest_results

type variables
Char dtb_status
// 03/06/2008 naming conventions for variables should be followed these are not local variables to begin with l...
String lc_cdrom,ls_cntr,ls_multidisk_dirlist,ls_dcdate,ls_dcformat,ls_multimediatype,ls_audioformat,ls_depth,ls_allnavlist,Luserid
Boolean Multi_disc = FALSE, Old_DTB_book = FALSE
Integer lcnt_navpoint,TotalTime
long Old_DTB_bkseq, il_test_no
Date ld_produceddate
str_dtb_data lstr_db_data
string is_docname // 03/07/2008
end variables

forward prototypes
public subroutine wf_get_blobs (long ll_bkseq)
public function string wf_formattext (blob abl)
end prototypes

public subroutine wf_get_blobs (long ll_bkseq);////////////////////////////////////////////////////////////////////////////////////////////////////////////
////
////	Function: wf_get_blobs
////
////	Description: Select blob field values from DTB Data for a given book
////	
////
////////////////////////////////////////////////////////////////////////////////////////////////////////////
////	
////	Revision History
////
////	Developed by 	Date 				Version						Tracking#
////									
//// Murali K.			06/30/2008      Process medium and long results blobs. Phase-2
////////////////////////////////////////////////////////////////////////////////////////////////////////////
////
//
Blob report_blob, nlsreport_blob, opf_blob, ncx_blob, sum_blob, smil_blob, medium_result_blob, long_result_blob
Boolean Dlist = FALSE,bjaws
String ls_text,ls_txtlines[ ], dirlist_file
Integer li_numstats,i,rtn
n_cst_string 	inv_string




//// ZEDVAL 
SELECTBLOB FILE_DATA
        INTO  :report_blob
        FROM AUTOTEST_FILES
        where TEST_NO= :il_test_no 
		  	AND FTYPE = 'ZEDVAL'        USING SQLServerOracleTrans ;
IF SQLServerOracleTrans.sqlNRows > 0 AND NOT(IsNull(report_blob))THEN
	// 07/16/2008 remove special characters
//	ls_text = String(report_blob)
//	li_numstats = inv_string.of_Parsetoarray (ls_text, "~n", ls_txtlines)
//	FOR i = 1 TO li_numstats
//		ls_txtlines[i] = inv_string.of_RemoveNonPrint(ls_txtlines[i])
//		ls_txtlines[i] = inv_string.of_Trim(ls_txtlines[i])
//	NEXT
//	rtn = inv_string.of_ArrayToString(ls_txtlines,"~r~n",ls_text)
// 08/19/2008 format text
	ls_text = wf_formattext(report_blob)
	tab_dtb.tabpage_reportxml.mle_reportxml.text = ls_text //String(report_blob)
END IF

//// NLSVAL
SELECTBLOB FILE_DATA
        INTO  :report_blob
        FROM AUTOTEST_FILES
        where TEST_NO= :il_test_no 
		  	AND FTYPE = 'NLSVAL'        USING SQLServerOracleTrans ;
IF SQLServerOracleTrans.sqlNRows > 0 AND NOT(IsNull(report_blob))THEN
	ls_text = wf_formattext(report_blob)
	tab_dtb.tabpage_nlsreportxml.mle_nlsreportxml.text = ls_text //String(report_blob)
END IF

//// OPF
SELECTBLOB FILE_DATA
        INTO  :report_blob
        FROM AUTOTEST_FILES
        where TEST_NO= :il_test_no 
		  	AND FTYPE = 'OPF'        USING SQLServerOracleTrans ;
IF SQLServerOracleTrans.sqlNRows > 0 AND NOT(IsNull(report_blob))THEN
	ls_text = wf_formattext(report_blob)
	tab_dtb.tabpage_opf.mle_opf.text =ls_text // String(report_blob)
END IF

//// NCX
SELECTBLOB FILE_DATA
        INTO  :report_blob
        FROM AUTOTEST_FILES
        where TEST_NO= :il_test_no 
		  	AND FTYPE = 'NCX'        USING SQLServerOracleTrans ;
IF SQLServerOracleTrans.sqlNRows > 0 AND NOT(IsNull(report_blob))THEN
	ls_text = wf_formattext(report_blob)
	tab_dtb.tabpage_ncx.mle_ncx.text  = ls_text //  String(report_blob)
END IF

//// SMIL
SELECTBLOB FILE_DATA
        INTO  :report_blob
        FROM AUTOTEST_FILES
        where TEST_NO= :il_test_no 
		  	AND FTYPE = 'SMIL'        USING SQLServerOracleTrans ;
IF SQLServerOracleTrans.sqlNRows > 0 AND NOT(IsNull(report_blob))THEN
	ls_text = wf_formattext(report_blob)
	tab_dtb.tabpage_dirlist.mle_smil.text  =  ls_text // String(report_blob)
END IF

////OPFNCX
SELECTBLOB FILE_DATA
        INTO  :report_blob
        FROM AUTOTEST_FILES
        where TEST_NO= :il_test_no 
		  	AND FTYPE = 'OPFNCX'        USING SQLServerOracleTrans ;
IF SQLServerOracleTrans.sqlNRows > 0 AND NOT(IsNull(report_blob))THEN
	ls_text = wf_formattext(report_blob)
	tab_dtb.tabpage_opfncx.mle_opfncx.text  =  ls_text // String(report_blob)
END IF

////SHORT 09/19/2008
SELECT SHORT_RESULT
        INTO  :ls_text
        FROM BOOK_TESTS
        where TEST_NO= :il_test_no   USING SQLServerOracleTrans ;
IF SQLServerOracleTrans.sqlNRows > 0 AND NOT(IsNull(report_blob)) THEN
	report_blob = blob(ls_text)
	ls_text = wf_formattext(report_blob)
	tab_dtb.tabpage_shortresult.mle_shortresult.text =  ls_text 
END IF


////MEDIUM
SELECTBLOB FILE_DATA
        INTO  :report_blob
        FROM AUTOTEST_FILES
        where TEST_NO= :il_test_no 
		  	AND FTYPE = 'MEDIUM'        USING SQLServerOracleTrans ;
IF SQLServerOracleTrans.sqlNRows > 0 AND NOT(IsNull(report_blob))THEN
	ls_text = wf_formattext(report_blob)
	tab_dtb.tabpage_mediumresult.mle_mediumresult.text =  ls_text // String(report_blob)
END IF

////LONG
SELECTBLOB FILE_DATA
        INTO  :report_blob
        FROM AUTOTEST_FILES
        where TEST_NO= :il_test_no 
		  	AND FTYPE = 'LONG'        USING SQLServerOracleTrans ;
IF SQLServerOracleTrans.sqlNRows > 0 AND NOT(IsNull(report_blob))THEN
	ls_text = wf_formattext(report_blob)
	tab_dtb.tabpage_longresult.mle_longresult.text  =  ls_text // String(report_blob)
END IF
RETURN

end subroutine

public function string wf_formattext (blob abl);////////////////////////////////////////////////////////////////////////////////////////////////////////////
//
//	Function: wf_formattext
//   Args		: Blob report xml
//	Return	: formatted text
//	Description: Select blob field values from DTB Data for a given book
//	
//
////////////////////////////////////////////////////////////////////////////////////////////////////////////
//	
//	Revision History
//
//	Developed by 	Date 				Version						Tracking#
//									
// Murali K.			08/19/2008      Format results into word wrapped text
////////////////////////////////////////////////////////////////////////////////////////////////////////////
//

String ls_text,ls_txtlines[ ], dirlist_file
Integer li_numstats,i,rtn
n_cst_string 	inv_string

ls_text = String(abl)
li_numstats = inv_string.of_Parsetoarray (ls_text, "~n", ls_txtlines)
FOR i = 1 TO li_numstats
	ls_txtlines[i] = inv_string.of_RemoveNonPrint(ls_txtlines[i])
	ls_txtlines[i] = inv_string.of_Trim(ls_txtlines[i])
NEXT
rtn = inv_string.of_ArrayToString(ls_txtlines,"~r~n",ls_text)

RETURN ls_text

end function

on w_qa_autotest_results.create
int iCurrent
call super::create
this.cb_exit=create cb_exit
this.tab_dtb=create tab_dtb
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.cb_exit
this.Control[iCurrent+2]=this.tab_dtb
end on

on w_qa_autotest_results.destroy
call super::destroy
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
// Murali K.			 07/08/2008    Retrieve autotest results from AUTOTEST_FILES and display
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//

// Retrieve and display results
wf_get_blobs(il_test_no)

end event

event open;call super::open;//////////////////////////////////////////////////////////////////////////////////////////////////////////
//
//	Event: open event
//
//	Description: Set the message parameters
//	
//
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//	
//	Revision History
//
//	Developed by 	Date 				Version						Tracking#
//								
//// Murali K.			 07/08/2008    Autotest results 
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//

il_test_no = Message.DoubleParm
this.center=TRUE
this.title = this.title + ' for Test #'+ string(il_test_no)
end event

type cb_exit from commandbutton within w_qa_autotest_results
integer x = 3785
integer y = 1788
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

type tab_dtb from tab within w_qa_autotest_results
string tag = "DTB Tab Control"
integer width = 4037
integer height = 1772
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
tabpage_dirlist tabpage_dirlist
tabpage_opfncx tabpage_opfncx
tabpage_shortresult tabpage_shortresult
tabpage_mediumresult tabpage_mediumresult
tabpage_longresult tabpage_longresult
end type

on tab_dtb.create
this.tabpage_reportxml=create tabpage_reportxml
this.tabpage_nlsreportxml=create tabpage_nlsreportxml
this.tabpage_opf=create tabpage_opf
this.tabpage_ncx=create tabpage_ncx
this.tabpage_dirlist=create tabpage_dirlist
this.tabpage_opfncx=create tabpage_opfncx
this.tabpage_shortresult=create tabpage_shortresult
this.tabpage_mediumresult=create tabpage_mediumresult
this.tabpage_longresult=create tabpage_longresult
this.Control[]={this.tabpage_reportxml,&
this.tabpage_nlsreportxml,&
this.tabpage_opf,&
this.tabpage_ncx,&
this.tabpage_dirlist,&
this.tabpage_opfncx,&
this.tabpage_shortresult,&
this.tabpage_mediumresult,&
this.tabpage_longresult}
end on

on tab_dtb.destroy
destroy(this.tabpage_reportxml)
destroy(this.tabpage_nlsreportxml)
destroy(this.tabpage_opf)
destroy(this.tabpage_ncx)
destroy(this.tabpage_dirlist)
destroy(this.tabpage_opfncx)
destroy(this.tabpage_shortresult)
destroy(this.tabpage_mediumresult)
destroy(this.tabpage_longresult)
end on

type tabpage_reportxml from userobject within tab_dtb
event create ( )
event destroy ( )
integer x = 18
integer y = 112
integer width = 4000
integer height = 1644
long backcolor = 79741120
string text = "ZEDVAL"
long tabtextcolor = 33554432
long tabbackcolor = 79741120
long picturemaskcolor = 536870912
mle_reportxml mle_reportxml
end type

on tabpage_reportxml.create
this.mle_reportxml=create mle_reportxml
this.Control[]={this.mle_reportxml}
end on

on tabpage_reportxml.destroy
destroy(this.mle_reportxml)
end on

type mle_reportxml from multilineedit within tabpage_reportxml
string tag = "Report. XML"
integer x = 5
integer y = 28
integer width = 3963
integer height = 1604
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
boolean displayonly = true
borderstyle borderstyle = stylelowered!
boolean hideselection = false
end type

type tabpage_nlsreportxml from userobject within tab_dtb
event create ( )
event destroy ( )
integer x = 18
integer y = 112
integer width = 4000
integer height = 1644
long backcolor = 79741120
string text = "NLSVAL"
long tabtextcolor = 33554432
long tabbackcolor = 79741120
long picturemaskcolor = 536870912
mle_nlsreportxml mle_nlsreportxml
end type

on tabpage_nlsreportxml.create
this.mle_nlsreportxml=create mle_nlsreportxml
this.Control[]={this.mle_nlsreportxml}
end on

on tabpage_nlsreportxml.destroy
destroy(this.mle_nlsreportxml)
end on

type mle_nlsreportxml from multilineedit within tabpage_nlsreportxml
string tag = "NLS Report XML"
integer x = 18
integer y = 16
integer width = 3963
integer height = 1616
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
boolean displayonly = true
borderstyle borderstyle = stylelowered!
boolean hideselection = false
end type

type tabpage_opf from userobject within tab_dtb
integer x = 18
integer y = 112
integer width = 4000
integer height = 1644
long backcolor = 79741120
string text = "OPF File"
long tabtextcolor = 33554432
long tabbackcolor = 79741120
long picturemaskcolor = 536870912
mle_opf mle_opf
end type

on tabpage_opf.create
this.mle_opf=create mle_opf
this.Control[]={this.mle_opf}
end on

on tabpage_opf.destroy
destroy(this.mle_opf)
end on

type mle_opf from multilineedit within tabpage_opf
string tag = "OPF File"
integer x = 18
integer y = 16
integer width = 3968
integer height = 1620
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
boolean displayonly = true
borderstyle borderstyle = stylelowered!
boolean hideselection = false
end type

type tabpage_ncx from userobject within tab_dtb
integer x = 18
integer y = 112
integer width = 4000
integer height = 1644
long backcolor = 79741120
string text = "NCX File"
long tabtextcolor = 33554432
long tabbackcolor = 79741120
long picturemaskcolor = 536870912
mle_ncx mle_ncx
end type

on tabpage_ncx.create
this.mle_ncx=create mle_ncx
this.Control[]={this.mle_ncx}
end on

on tabpage_ncx.destroy
destroy(this.mle_ncx)
end on

type mle_ncx from multilineedit within tabpage_ncx
string tag = "NCX File"
integer x = 18
integer y = 16
integer width = 3973
integer height = 1620
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
boolean displayonly = true
borderstyle borderstyle = stylelowered!
boolean hideselection = false
end type

type tabpage_dirlist from userobject within tab_dtb
integer x = 18
integer y = 112
integer width = 4000
integer height = 1644
long backcolor = 79741120
string text = "SMIL"
long tabtextcolor = 33554432
long tabbackcolor = 79741120
long picturemaskcolor = 536870912
mle_smil mle_smil
end type

on tabpage_dirlist.create
this.mle_smil=create mle_smil
this.Control[]={this.mle_smil}
end on

on tabpage_dirlist.destroy
destroy(this.mle_smil)
end on

type mle_smil from multilineedit within tabpage_dirlist
integer x = 23
integer y = 28
integer width = 3968
integer height = 1592
integer taborder = 60
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
boolean vscrollbar = true
boolean displayonly = true
borderstyle borderstyle = stylelowered!
end type

type tabpage_opfncx from userobject within tab_dtb
integer x = 18
integer y = 112
integer width = 4000
integer height = 1644
long backcolor = 79741120
string text = "OPF/NCX"
long tabtextcolor = 33554432
long tabbackcolor = 79741120
long picturemaskcolor = 536870912
mle_opfncx mle_opfncx
end type

on tabpage_opfncx.create
this.mle_opfncx=create mle_opfncx
this.Control[]={this.mle_opfncx}
end on

on tabpage_opfncx.destroy
destroy(this.mle_opfncx)
end on

type mle_opfncx from multilineedit within tabpage_opfncx
integer x = 18
integer y = 12
integer width = 3968
integer height = 1612
integer taborder = 70
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
boolean vscrollbar = true
boolean displayonly = true
borderstyle borderstyle = stylelowered!
end type

type tabpage_shortresult from userobject within tab_dtb
integer x = 18
integer y = 112
integer width = 4000
integer height = 1644
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
integer x = 18
integer y = 28
integer width = 3963
integer height = 1588
integer taborder = 70
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
boolean vscrollbar = true
boolean displayonly = true
borderstyle borderstyle = stylelowered!
end type

type tabpage_mediumresult from userobject within tab_dtb
integer x = 18
integer y = 112
integer width = 4000
integer height = 1644
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
integer width = 3977
integer height = 1616
integer taborder = 50
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
boolean vscrollbar = true
boolean displayonly = true
borderstyle borderstyle = stylelowered!
end type

type tabpage_longresult from userobject within tab_dtb
integer x = 18
integer y = 112
integer width = 4000
integer height = 1644
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
integer width = 3963
integer height = 1620
integer taborder = 50
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
boolean vscrollbar = true
boolean displayonly = true
borderstyle borderstyle = stylelowered!
end type


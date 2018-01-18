$PBExportHeader$w_qa_zedval_reports.srw
forward
global type w_qa_zedval_reports from w_sheet
end type
type cb_print from commandbutton within w_qa_zedval_reports
end type
type cb_exit from commandbutton within w_qa_zedval_reports
end type
type tab_dtb from tab within w_qa_zedval_reports
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
type tabpage_smil from userobject within tab_dtb
end type
type mle_smil from multilineedit within tabpage_smil
end type
type tabpage_smil from userobject within tab_dtb
mle_smil mle_smil
end type
type tabpage_extract from userobject within tab_dtb
end type
type mle_extract from multilineedit within tabpage_extract
end type
type tabpage_extract from userobject within tab_dtb
mle_extract mle_extract
end type
type tab_dtb from tab within w_qa_zedval_reports
tabpage_reportxml tabpage_reportxml
tabpage_nlsreportxml tabpage_nlsreportxml
tabpage_opf tabpage_opf
tabpage_ncx tabpage_ncx
tabpage_smil tabpage_smil
tabpage_extract tabpage_extract
end type
end forward

global type w_qa_zedval_reports from w_sheet
integer x = 214
integer y = 221
integer width = 3817
integer height = 2132
string title = "DTB QA Reports"
cb_print cb_print
cb_exit cb_exit
tab_dtb tab_dtb
end type
global w_qa_zedval_reports w_qa_zedval_reports

type variables
Char dtb_status
String lc_cdrom,ls_cntr,ls_multidisk_dirlist,ls_dcdate,ls_dcformat,ls_multimediatype,ls_audioformat,ls_depth,ls_allnavlist,Luserid
Boolean Multi_disc = FALSE
Integer lcnt_navpoint,TotalTime
Date ld_produceddate
Long ll_bkseq

end variables

forward prototypes
public function boolean wf_isjawsrunning ()
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

on w_qa_zedval_reports.create
int iCurrent
call super::create
this.cb_print=create cb_print
this.cb_exit=create cb_exit
this.tab_dtb=create tab_dtb
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.cb_print
this.Control[iCurrent+2]=this.cb_exit
this.Control[iCurrent+3]=this.tab_dtb
end on

on w_qa_zedval_reports.destroy
call super::destroy
destroy(this.cb_print)
destroy(this.cb_exit)
destroy(this.tab_dtb)
end on

event pfc_postopen;call super::pfc_postopen;Blob report_blob,nlsreport_blob,opf_blob,ncx_blob,smil_blob,extract_blob
Boolean Dlist = FALSE,bjaws
String ls_text,ls_txtlines[ ]
Integer li_numstats,i,rtn
n_cst_string 	inv_string

bjaws = wf_isjawsrunning()
IF bjaws = TRUE THEN
	of_SetJaws(TRUE)
ELSE
	of_SetJaws(FALSE)
END IF

ll_bkseq = Long(message.StringParm)

// ZEDVAL Report
SELECTBLOB ZEDVAL
        INTO  :report_blob
        FROM DTB_DATA
        where BKSEQ = :ll_bkseq
        USING SQLServerTrans ;
IF SqlServerTrans.sqlNRows > 0 AND NOT(IsNull(report_blob))THEN
	tab_dtb.tabpage_reportxml.mle_reportxml.text = String(report_blob)
ELSEIF 	IsNull(report_blob) THEN
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
	tab_dtb.tabpage_nlsreportxml.mle_nlsreportxml.text = String(nlsreport_blob)
ELSEIF 	IsNull(nlsreport_blob) THEN
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
	
ELSEIF 	IsNull(opf_blob) THEN
	Messagebox("ERROR SELECTING BLOB"," OPF File.is NULL")
ELSE
	Messagebox("ERROR SELECTING BLOB"," Error in selecting OPF File.")
END IF

// Initiate the array of string
FOR i = 1 TO li_numstats
	ls_txtlines[i] = ""
NEXT

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
	tab_dtb.tabpage_ncx.mle_ncx.text = ""
	tab_dtb.tabpage_ncx.mle_ncx.text = ls_text

ELSEIF 	IsNull(ncx_blob) THEN
	Messagebox("ERROR SELECTING BLOB"," NCX File.is NULL")
ELSE
	Messagebox("ERROR SELECTING BLOB"," Error in selecting NCX File.")
END IF

// Initiate the array of string
FOR i = 1 TO li_numstats
	ls_txtlines[i] = ""
NEXT


// SMIL File
SELECTBLOB SMIL_FILE
        INTO  :smil_blob
        FROM DTB_DATA
        where BKSEQ = :ll_bkseq
        USING SQLServerTrans ;
IF SqlServerTrans.sqlNRows > 0 AND NOT(IsNull(smil_blob))THEN
	ls_text = String(smil_blob)
	li_numstats = inv_string.of_Parsetoarray (ls_text, "~n", ls_txtlines)
	FOR i = 1 TO li_numstats
		ls_txtlines[i] = inv_string.of_RemoveNonPrint(ls_txtlines[i])
		ls_txtlines[i] = inv_string.of_Trim(ls_txtlines[i])
	NEXT
	rtn = inv_string.of_ArrayToString(ls_txtlines,"~r~n",ls_text)
	tab_dtb.tabpage_smil.mle_smil.text = ""
	tab_dtb.tabpage_smil.mle_smil.text = ls_text

ELSEIF 	IsNull(smil_blob) THEN
	Messagebox("ERROR SELECTING BLOB"," SMIL File.is NULL")
ELSE
	Messagebox("ERROR SELECTING BLOB"," Error in selecting SMIL File.")
END IF

// Initiate the array of string
FOR i = 1 TO li_numstats
	ls_txtlines[i] = ""
NEXT

// Extracted File
SELECTBLOB OPF_NCX_SUM
        INTO  :extract_blob
        FROM DTB_DATA
        where BKSEQ = :ll_bkseq
        USING SQLServerTrans ;
IF SqlServerTrans.sqlNRows > 0 AND NOT(IsNull(extract_blob))THEN
	ls_text = String(extract_blob)
	li_numstats = inv_string.of_Parsetoarray (ls_text, "~n", ls_txtlines)
	FOR i = 1 TO li_numstats
		ls_txtlines[i] = inv_string.of_RemoveNonPrint(ls_txtlines[i])
		ls_txtlines[i] = inv_string.of_Trim(ls_txtlines[i])
	NEXT
	rtn = inv_string.of_ArrayToString(ls_txtlines,"~r~n",ls_text)
	tab_dtb.tabpage_extract.mle_extract.text = ""
	tab_dtb.tabpage_extract.mle_extract.text = ls_text

ELSEIF 	IsNull(extract_blob) THEN
	Messagebox("ERROR SELECTING BLOB"," Extracted File.is NULL")
ELSE
	Messagebox("ERROR SELECTING BLOB"," Error in selecting Extracted File.")
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

inv_resize.of_Register(cb_exit, "Scale")
inv_resize.of_Register(cb_print, "Scale")

inv_resize.of_Register(tab_dtb, "Scale")

inv_resize.of_Register(tab_dtb.tabpage_nlsreportxml, "Scale")
inv_resize.of_Register(tab_dtb.tabpage_nlsreportxml.mle_nlsreportxml, "Scale")

inv_resize.of_Register(tab_dtb.tabpage_reportxml, "Scale")
inv_resize.of_Register(tab_dtb.tabpage_reportxml.mle_reportxml, "Scale")

inv_resize.of_Register(tab_dtb.tabpage_ncx, "Scale")
inv_resize.of_Register(tab_dtb.tabpage_ncx.mle_ncx, "Scale")

inv_resize.of_Register(tab_dtb.tabpage_opf, "Scale")
inv_resize.of_Register(tab_dtb.tabpage_opf.mle_opf, "Scale")

inv_resize.of_Register(tab_dtb.tabpage_smil, "Scale")
inv_resize.of_Register(tab_dtb.tabpage_smil.mle_smil, "Scale")

inv_resize.of_Register(tab_dtb.tabpage_extract, "Scale")
inv_resize.of_Register(tab_dtb.tabpage_extract.mle_extract, "Scale")

end event

type cb_print from commandbutton within w_qa_zedval_reports
integer x = 2962
integer y = 1888
integer width = 402
integer height = 96
integer taborder = 180
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&Print"
end type

event clicked;OpenSheetwithparm(w_sheet_pics_ole_crystal,"dtb_zedval_nlsval",w_pics_main, 0, Original!)

end event

type cb_exit from commandbutton within w_qa_zedval_reports
integer x = 3438
integer y = 1888
integer width = 256
integer height = 96
integer taborder = 170
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "E&xit"
end type

event clicked;close (parent)
end event

type tab_dtb from tab within w_qa_zedval_reports
string tag = "DTB Tab Control"
integer width = 3694
integer height = 1856
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
tabpage_smil tabpage_smil
tabpage_extract tabpage_extract
end type

on tab_dtb.create
this.tabpage_reportxml=create tabpage_reportxml
this.tabpage_nlsreportxml=create tabpage_nlsreportxml
this.tabpage_opf=create tabpage_opf
this.tabpage_ncx=create tabpage_ncx
this.tabpage_smil=create tabpage_smil
this.tabpage_extract=create tabpage_extract
this.Control[]={this.tabpage_reportxml,&
this.tabpage_nlsreportxml,&
this.tabpage_opf,&
this.tabpage_ncx,&
this.tabpage_smil,&
this.tabpage_extract}
end on

on tab_dtb.destroy
destroy(this.tabpage_reportxml)
destroy(this.tabpage_nlsreportxml)
destroy(this.tabpage_opf)
destroy(this.tabpage_ncx)
destroy(this.tabpage_smil)
destroy(this.tabpage_extract)
end on

event selectionchanged;//Int ll_rows,rtn,lcnt, li_numstats,i,li_smilfile,total, n
//Long ll_bkseq
//String ls_bkseq,ls_text,vs_smil, ls_smilfile,ls_txtlines[ ]
//Blob smil_blob_one, smil_blob_all
//n_cst_string 	inv_string
//
//lb_smil.SetState(1, TRUE)
//ls_bkseq = Mid(lb_smil.SelectedItem(), 1 , 5)
//ll_bkseq = Long(Mid(lb_smil.SelectedItem(), 1 , 5))
//
//IF newindex = 2 THEN
//	// NLS Report.xml
//	
//ELSEIF newindex = 3 THEN
//	//	OPF tab
//	Integer li_opffile
//	String vs_source
//	Blob opf_blob
//	
//	IF Multi_disc = TRUE THEN
//		vs_source = ls_multidisk_dirlist +"\"+ls_bkseq+".opf"
//	ELSE
//		lc_cdrom = lb_cdrom.SelectedItem()
//		vs_source = lc_cdrom +"\"+ls_bkseq+".opf"
//	END IF	
//
//	IF NOT(FileExists(vs_source)) THEN 
//			  Messagebox("ERROR in file open","OPF file does not exist")
//		 RETURN
//	END IF 
//	
//	li_opffile = FileOpen(vs_source, streamMode!)
//	IF li_opffile <> -1 THEN
//		FileReadEx(li_opffile, opf_blob)
//		FileClose(li_opffile)
//	ELSEIF li_opffile = -1 THEN
//			  Messagebox("ERROR in reading","Can not read the OPF file")
//		 RETURN
//	END IF
//	
//	ls_text = String(opf_blob, EncodingANSI!)
//
//	li_numstats = inv_string.of_Parsetoarray (ls_text, "~n", ls_txtlines)
//	FOR i = 1 TO li_numstats
//		ls_txtlines[i] = inv_string.of_RemoveNonPrint(ls_txtlines[i])
//		ls_txtlines[i] = inv_string.of_Trim(ls_txtlines[i])
//	NEXT
//	rtn = inv_string.of_ArrayToString(ls_txtlines,"~r~n",ls_text)
//	
//	tab_dtb.tabpage_opf.mle_opf.text = ls_text
//
//ELSEIF newindex = 4 THEN
//	//	NCX tab
//	Integer li_ncxfile
//	String vs_ncx
//	Blob ncx_blob
//	
//	IF Multi_disc = TRUE THEN
//		vs_ncx = ls_multidisk_dirlist +"\"+ls_bkseq+".ncx"
//	ELSE
//		lc_cdrom = lb_cdrom.SelectedItem()
//		vs_ncx = lc_cdrom +"\"+ls_bkseq+".ncx"
//	END IF	
//
//	IF NOT(FileExists(vs_ncx)) THEN 
//			  Messagebox("ERROR in file open","NCX file does not exist")
//		 RETURN
//	END IF 
//	
//	li_ncxfile = FileOpen(vs_ncx, streamMode!)
//	IF li_ncxfile <> -1 THEN
//		FileRead(li_ncxfile, ncx_blob)
//		FileClose(li_ncxfile)
//	ELSEIF li_ncxfile = -1 THEN
//			  Messagebox("ERROR in reading","Can not read the NCX file")
//		 RETURN
//	END IF
//	
//	ls_text = String(ncx_blob, EncodingANSI!)
//
//	li_numstats = inv_string.of_Parsetoarray (ls_text, "~n", ls_txtlines)
//	FOR i = 1 TO li_numstats
//		ls_txtlines[i] = inv_string.of_RemoveNonPrint(ls_txtlines[i])
//		ls_txtlines[i] = inv_string.of_Trim(ls_txtlines[i])
//	NEXT
//	rtn = inv_string.of_ArrayToString(ls_txtlines,"~r~n",ls_text)
//	
//	tab_dtb.tabpage_ncx.mle_ncx.text = ls_text
//
//ELSEIF newindex = 5 THEN
//	//	All SMIL files
//	
//	total = lb_smil.TotalItems()
//	
//	FOR n = 1 TO total
//		ls_smilfile = lb_smil.Text(n)
//		IF Multi_disc = TRUE THEN
//			vs_smil = ls_multidisk_dirlist +"\"+ls_smilfile
//		ELSE
//			lc_cdrom = lb_cdrom.SelectedItem()
//			vs_smil = lc_cdrom +"\"+ls_smilfile
//		END IF	
//		li_smilfile = FileOpen(vs_smil, streamMode!)
//		IF li_smilfile <> -1 THEN
//			FileRead(li_smilfile, smil_blob_one)
//			FileClose(li_smilfile)
//		END IF
//		smil_blob_all += smil_blob_one
//	NEXT
//	
//	
//	ls_text = String(smil_blob_all, EncodingANSI!)
//
//	li_numstats = inv_string.of_Parsetoarray (ls_text, "~n", ls_txtlines)
//	FOR i = 1 TO li_numstats
//		ls_txtlines[i] = inv_string.of_RemoveNonPrint(ls_txtlines[i])
//		ls_txtlines[i] = inv_string.of_Trim(ls_txtlines[i])
//	NEXT
//	rtn = inv_string.of_ArrayToString(ls_txtlines,"~r~n",ls_text)
//	
//	tab_dtb.tabpage_all_smil.mle_allsmil.text = ls_text
//
//
//
//ELSEIF newindex = 6 THEN
//	//	Selected SMIL file(s)
//	
//	total = lb_smil.TotalItems()
//	
//	FOR n = 1 TO total
//		IF lb_smil.State(n) = 1 THEN 
//			ls_smilfile = lb_smil.Text(n)
//			IF Multi_disc = TRUE THEN
//				vs_smil = ls_multidisk_dirlist +"\"+ls_smilfile
//			ELSE
//				lc_cdrom = lb_cdrom.SelectedItem()
//				vs_smil = lc_cdrom +"\"+ls_smilfile
//			END IF	
//			li_smilfile = FileOpen(vs_smil, streamMode!)
//			IF li_smilfile <> -1 THEN
//				FileRead(li_smilfile, smil_blob_one)
//				FileClose(li_smilfile)
//			END IF
//			smil_blob_all += smil_blob_one
//		END IF
//	NEXT
//	
//	ls_text = String(smil_blob_all, EncodingANSI!)
//
//	li_numstats = inv_string.of_Parsetoarray (ls_text, "~n", ls_txtlines)
//	FOR i = 1 TO li_numstats
//		ls_txtlines[i] = inv_string.of_RemoveNonPrint(ls_txtlines[i])
//		ls_txtlines[i] = inv_string.of_Trim(ls_txtlines[i])
//	NEXT
//	rtn = inv_string.of_ArrayToString(ls_txtlines,"~r~n",ls_text)
//	
//	tab_dtb.tabpage_selected_smil.mle_selsmils.text  = ls_text
//
//END IF	
end event

type tabpage_reportxml from userobject within tab_dtb
event create ( )
event destroy ( )
integer x = 18
integer y = 112
integer width = 3657
integer height = 1728
long backcolor = 79741120
string text = "Report XML"
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
integer x = 18
integer y = 16
integer width = 3621
integer height = 1696
integer taborder = 30
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
boolean hscrollbar = true
boolean vscrollbar = true
boolean autohscroll = true
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
integer height = 1728
long backcolor = 79741120
string text = "NLS Report XML"
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
integer width = 3621
integer height = 1664
integer taborder = 30
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
boolean hscrollbar = true
boolean vscrollbar = true
boolean autohscroll = true
boolean autovscroll = true
integer tabstop[] = {0}
borderstyle borderstyle = stylelowered!
boolean hideselection = false
end type

type tabpage_opf from userobject within tab_dtb
integer x = 18
integer y = 112
integer width = 3657
integer height = 1728
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
integer width = 3621
integer height = 1664
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
boolean autohscroll = true
boolean autovscroll = true
integer tabstop[] = {0}
borderstyle borderstyle = stylelowered!
boolean hideselection = false
end type

type tabpage_ncx from userobject within tab_dtb
integer x = 18
integer y = 112
integer width = 3657
integer height = 1728
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
integer width = 3584
integer height = 1664
integer taborder = 50
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
boolean hscrollbar = true
boolean vscrollbar = true
boolean autohscroll = true
boolean autovscroll = true
integer tabstop[] = {0}
borderstyle borderstyle = stylelowered!
boolean hideselection = false
end type

type tabpage_smil from userobject within tab_dtb
integer x = 18
integer y = 112
integer width = 3657
integer height = 1728
long backcolor = 79741120
string text = "SMIL File"
long tabtextcolor = 33554432
long tabbackcolor = 79741120
long picturemaskcolor = 536870912
mle_smil mle_smil
end type

on tabpage_smil.create
this.mle_smil=create mle_smil
this.Control[]={this.mle_smil}
end on

on tabpage_smil.destroy
destroy(this.mle_smil)
end on

type mle_smil from multilineedit within tabpage_smil
string tag = "SMIL File"
integer x = 18
integer y = 16
integer width = 3584
integer height = 1664
integer taborder = 60
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
boolean hscrollbar = true
boolean vscrollbar = true
boolean autohscroll = true
boolean autovscroll = true
integer tabstop[] = {0}
borderstyle borderstyle = stylelowered!
boolean hideselection = false
end type

type tabpage_extract from userobject within tab_dtb
integer x = 18
integer y = 112
integer width = 3657
integer height = 1728
long backcolor = 79741120
string text = "Extracted Data"
long tabtextcolor = 33554432
long tabbackcolor = 79741120
long picturemaskcolor = 536870912
mle_extract mle_extract
end type

on tabpage_extract.create
this.mle_extract=create mle_extract
this.Control[]={this.mle_extract}
end on

on tabpage_extract.destroy
destroy(this.mle_extract)
end on

type mle_extract from multilineedit within tabpage_extract
string tag = "EXTRACT File"
integer x = 18
integer y = 16
integer width = 3584
integer height = 1664
integer taborder = 60
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
boolean hscrollbar = true
boolean vscrollbar = true
boolean autohscroll = true
boolean autovscroll = true
integer tabstop[] = {0}
borderstyle borderstyle = stylelowered!
boolean hideselection = false
end type


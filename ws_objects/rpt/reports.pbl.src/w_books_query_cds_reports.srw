$PBExportHeader$w_books_query_cds_reports.srw
forward
global type w_books_query_cds_reports from w_sheet
end type
type cb_print from u_cb within w_books_query_cds_reports
end type
type cb_print_to_file from u_cb within w_books_query_cds_reports
end type
type cb_cancel from u_cb within w_books_query_cds_reports
end type
type dw_pctil_ec_report_conno from u_pics_dw within w_books_query_cds_reports
end type
end forward

global type w_books_query_cds_reports from w_sheet
integer x = 69
integer y = 360
integer width = 2789
integer height = 1552
string title = "CDS Reports"
cb_print cb_print
cb_print_to_file cb_print_to_file
cb_cancel cb_cancel
dw_pctil_ec_report_conno dw_pctil_ec_report_conno
end type
global w_books_query_cds_reports w_books_query_cds_reports

type variables
datetime id_stdt, id_enddt, id_cabdt
string it_chno
string which_rep,Title_type
end variables

forward prototypes
public function boolean wf_validate_conno (string Lconno)
end prototypes

public function boolean wf_validate_conno (string Lconno);int rtn=0
select count(*) into :rtn from mchar where conno=:Lconno using sqlservertrans;
if rtn > 0 then
	return true
else
	return false
end if

end function

on w_books_query_cds_reports.create
int iCurrent
call super::create
this.cb_print=create cb_print
this.cb_print_to_file=create cb_print_to_file
this.cb_cancel=create cb_cancel
this.dw_pctil_ec_report_conno=create dw_pctil_ec_report_conno
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.cb_print
this.Control[iCurrent+2]=this.cb_print_to_file
this.Control[iCurrent+3]=this.cb_cancel
this.Control[iCurrent+4]=this.dw_pctil_ec_report_conno
end on

on w_books_query_cds_reports.destroy
call super::destroy
destroy(this.cb_print)
destroy(this.cb_print_to_file)
destroy(this.cb_cancel)
destroy(this.dw_pctil_ec_report_conno)
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
//					dw_cdconsider_ace.Setfocus()
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
//			dw_cdconsider_ace.Setfocus()
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

inv_resize.of_Register(dw_pctil_ec_report_conno, "scale")

inv_resize.of_Register(cb_cancel, "scale")
inv_resize.of_Register(cb_print, "scale")

inv_resize.of_Register(cb_print_to_file, "scale")

end event

event resize;call super::resize;long ll_height

//This script is not commented because when we do minimize this screen then all the DW'S are not resizing 
This.X = w_pics_main.X
This.Y = w_pics_main.Y
ll_height = w_pics_main.mdi_1.Height
This.Resize(w_pics_main.WorkSpaceWidth(), ll_height)


end event

event pfc_postopen;call super::pfc_postopen;string ls_conno, ls_infm
long ll_count
m_pics_main.m_file.m_print.Enabled = TRUE
m_pics_main.m_file.m_pagesetup.Enabled = TRUE
m_pics_main.m_file.m_printimmediate.Enabled = TRUE

This.windowstate = maximized!
ls_conno=Message.StringParm
w_books_query_cds_reports.Title = "Title information list"

IF wf_validate_conno(ls_conno) THEN
	dw_pctil_ec_report_conno.Visible = True
	ll_count = w_books_query_cds_reports.dw_pctil_ec_report_conno.Retrieve(ls_conno)
	IF ll_count = 0 THEN
		Close(w_books_query_cds_reports)
		m_pics_main.m_menu.m_report.m_cdsreports.m_titleinformationlist.TriggerEvent(Clicked!)
		return
	ELSE
		dw_pctil_ec_report_conno.SetFocus()
	END IF
ELSE
	MessageBox("ERROR","Invalid Control Number.")
	Close(w_books_query_cds_reports)
END IF

end event

type cb_print from u_cb within w_books_query_cds_reports
integer x = 1723
integer y = 1348
integer width = 274
integer height = 72
integer taborder = 0
string text = "&Print"
end type

event clicked;call super::clicked;

dw_pctil_ec_report_conno.Triggerevent("pfc_Print")


f_pics_set_def_prn_setting()

end event

type cb_print_to_file from u_cb within w_books_query_cds_reports
integer x = 2066
integer y = 1348
integer width = 288
integer height = 72
integer taborder = 0
string text = "&Save"
end type

event clicked;call super::clicked;

dw_pctil_ec_report_conno.SaveAs()

end event

type cb_cancel from u_cb within w_books_query_cds_reports
integer x = 2427
integer y = 1348
integer width = 274
integer height = 72
integer taborder = 0
string text = "E&xit"
end type

event clicked;call super::clicked;ib_disableclosequery = TRUE
parent.Event pfc_close()
w_books_query.show()
//m_pics_main.m_menu.PopMenu(300, 0)
end event

type dw_pctil_ec_report_conno from u_pics_dw within w_books_query_cds_reports
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


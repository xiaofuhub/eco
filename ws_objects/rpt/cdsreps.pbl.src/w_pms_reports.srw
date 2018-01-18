$PBExportHeader$w_pms_reports.srw
forward
global type w_pms_reports from w_sheet
end type
type cb_print from u_cb within w_pms_reports
end type
type cb_print_to_file from u_cb within w_pms_reports
end type
type cb_cancel from u_cb within w_pms_reports
end type
type dw_annotrpt_ec_report from u_dw within w_pms_reports
end type
type dw_annotrpt_ec_report_pms from u_dw within w_pms_reports
end type
end forward

global type w_pms_reports from w_sheet
integer x = 69
integer y = 360
integer width = 2779
integer height = 1544
string title = "PMS Reports"
cb_print cb_print
cb_print_to_file cb_print_to_file
cb_cancel cb_cancel
dw_annotrpt_ec_report dw_annotrpt_ec_report
dw_annotrpt_ec_report_pms dw_annotrpt_ec_report_pms
end type
global w_pms_reports w_pms_reports

type variables
date id_stdt, id_enddt
string it_chno
end variables

on w_pms_reports.create
int iCurrent
call super::create
this.cb_print=create cb_print
this.cb_print_to_file=create cb_print_to_file
this.cb_cancel=create cb_cancel
this.dw_annotrpt_ec_report=create dw_annotrpt_ec_report
this.dw_annotrpt_ec_report_pms=create dw_annotrpt_ec_report_pms
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.cb_print
this.Control[iCurrent+2]=this.cb_print_to_file
this.Control[iCurrent+3]=this.cb_cancel
this.Control[iCurrent+4]=this.dw_annotrpt_ec_report
this.Control[iCurrent+5]=this.dw_annotrpt_ec_report_pms
end on

on w_pms_reports.destroy
call super::destroy
destroy(this.cb_print)
destroy(this.cb_print_to_file)
destroy(this.cb_cancel)
destroy(this.dw_annotrpt_ec_report)
destroy(this.dw_annotrpt_ec_report_pms)
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
					dw_annotrpt_ec_report_pms.Setfocus()
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
			dw_annotrpt_ec_report_pms.Setfocus()
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
inv_resize.of_Register(dw_annotrpt_ec_report_pms, "scale")
inv_resize.of_Register(dw_annotrpt_ec_report, "scale")
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

event pfc_postopen;call super::pfc_postopen;string ls_return

m_pics_main.m_file.m_print.Enabled = TRUE
m_pics_main.m_file.m_pagesetup.Enabled = TRUE
m_pics_main.m_file.m_printimmediate.Enabled = TRUE

This.windowstate = maximized!
date ld_stdt, ld_enddt
long ll_count
string lchart_no
str_cds_report lstr_cds_report
datetime ld_stdt_dt, ld_enddt_dt

CHOOSE CASE Message.StringParm
	
	CASE "Ed Anno"						// (for open dw_annotrpt_ec_report_pms)
		w_pms_reports.Title = "Edit Annotations PMS"
		dw_annotrpt_ec_report_pms.Visible = True
		dw_annotrpt_ec_report_pms.SetFocus()
		Open(w_pms_gets_dates)
		lstr_cds_report = Message.PowerObjectParm
		ls_return=lstr_cds_report.ls_string
		if ls_return='CANCEL' then
			if isvalid(w_pms_reports) then
				close(w_pms_reports)
				return
			end if
		end if
		 ld_stdt=lstr_cds_report.ld_stdt
		 ld_stdt_dt=datetime(ld_stdt,time('00:00:00'))
		 ld_enddt=lstr_cds_report.ld_enddt
		 ld_enddt_dt=datetime(ld_enddt,time('00:00:00'))
			IF IsValid(w_pms_reports) THEN
				ll_count = w_pms_reports.dw_annotrpt_ec_report_pms.Retrieve(ld_stdt_dt,ld_enddt_dt )
				IF ll_count = 0 THEN
					Close(w_pms_reports)
					Close(w_pms_gets_dates)
					m_pics_main.m_menu.m_report.m_pmsreports.m_editannotation1.m_bypmeditdate.TriggerEvent(Clicked!)
					return
				END IF
			ELSE
				Close(w_pms_reports)
				Close(w_pms_gets_dates)
				Return
			END IF
		
	CASE "Edit Anno"						// (for open dw_annotrpt_ec_report)
		w_pms_reports.Title = "Edit Annotations Report"
		dw_annotrpt_ec_report.Visible = True
		dw_annotrpt_ec_report.SetFocus()
		Open(w_cds_gets_chno)
		 IF (IsNull(Message.StringParm)=FALSE AND Message.StringParm<>"")THEN
			lchart_no = Message.StringParm
			IF IsValid(w_pms_reports) THEN
				ll_count = w_pms_reports.dw_annotrpt_ec_report.Retrieve(lchart_no)
				IF ll_count = 0 THEN	return
			ELSE
				Close(w_pms_reports)
				Close(w_cds_gets_chno)
				Return
			END IF
		Else
			Close(w_pms_reports)
		End IF
		
END CHOOSE




end event

type cb_print from u_cb within w_pms_reports
integer x = 1778
integer y = 1344
integer width = 274
integer height = 72
integer taborder = 0
string text = "&Print"
end type

event clicked;call super::clicked;IF dw_annotrpt_ec_report_pms.Visible = True THEN
	dw_annotrpt_ec_report_pms.Triggerevent("pfc_Print")
ElseIF dw_annotrpt_ec_report.Visible = True THEN
	dw_annotrpt_ec_report.Triggerevent("pfc_Print")	
END IF
// Reset the default printer setting
f_pics_set_def_prn_setting()

end event

type cb_print_to_file from u_cb within w_pms_reports
integer x = 2103
integer y = 1344
integer width = 274
integer height = 72
integer taborder = 0
string text = "&Save"
end type

event clicked;call super::clicked;IF dw_annotrpt_ec_report_pms.Visible = True THEN
	dw_annotrpt_ec_report_pms.SaveAs()
ElseIF dw_annotrpt_ec_report.Visible = True THEN
	dw_annotrpt_ec_report.SaveAs()	
END IF

end event

type cb_cancel from u_cb within w_pms_reports
integer x = 2427
integer y = 1344
integer width = 274
integer height = 72
integer taborder = 0
string text = "&Cancel"
end type

event clicked;call super::clicked;ib_disableclosequery = TRUE
parent.Event pfc_close()
m_pics_main.m_menu.PopMenu(300, 0)
end event

type dw_annotrpt_ec_report from u_dw within w_pms_reports
boolean visible = false
integer width = 2747
integer height = 1308
integer taborder = 0
string dataobject = "d_annotrpt_ec_report"
boolean hscrollbar = true
boolean border = false
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

event pfc_retrieve;call super::pfc_retrieve;THIS.Reset()
RETURN THIS.Retrieve(dw_annotrpt_ec_report)
end event

event retrieveend;call super::retrieveend;IF rowcount = 0 THEN
	Close(w_pics_retrieve_msg_box)
	MessageBox("ERROR","No data found.")	
Else
	Close(w_pics_retrieve_msg_box)
	Close(w_cds_gets_chno)
End IF

end event

event retrievestart;call super::retrievestart;OpenWithParm(w_pics_retrieve_msg_box,"Retrieving Edit Annotation Report, Please Wait...")
end event

type dw_annotrpt_ec_report_pms from u_dw within w_pms_reports
boolean visible = false
integer width = 2757
integer height = 1300
integer taborder = 0
string dataobject = "d_annotrpt_ec_report_pms"
boolean hscrollbar = true
boolean border = false
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

event pfc_retrieve;call super::pfc_retrieve;THIS.Reset()
RETURN THIS.Retrieve(dw_annotrpt_ec_report_pms)
end event

event retrievestart;call super::retrievestart;OpenWithParm(w_pics_retrieve_msg_box,"Retrieving Edit Annotation Report, Please Wait...")
end event

event retrieveend;call super::retrieveend;IF rowcount = 0 THEN
	Close(w_pics_retrieve_msg_box)
	MessageBox("ERROR","No data found.")	
Else
	Close(w_pics_retrieve_msg_box)
	Close(w_pms_gets_dates)
End IF

end event


$PBExportHeader$w_cc_lang.srw
forward
global type w_cc_lang from w_response
end type
type dw_cc_lang from u_pics_dw within w_cc_lang
end type
type cb_exit from u_cb within w_cc_lang
end type
type cb_update from u_cb within w_cc_lang
end type
end forward

global type w_cc_lang from w_response
integer x = 443
integer y = 452
integer width = 1522
integer height = 1020
string title = "Language Code/Description"
dw_cc_lang dw_cc_lang
cb_exit cb_exit
cb_update cb_update
end type
global w_cc_lang w_cc_lang

forward prototypes
public function boolean wf_check_duplicates (string ls_dealer_code)
end prototypes

public function boolean wf_check_duplicates (string ls_dealer_code);return true
end function

on w_cc_lang.create
int iCurrent
call super::create
this.dw_cc_lang=create dw_cc_lang
this.cb_exit=create cb_exit
this.cb_update=create cb_update
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_cc_lang
this.Control[iCurrent+2]=this.cb_exit
this.Control[iCurrent+3]=this.cb_update
end on

on w_cc_lang.destroy
call super::destroy
destroy(this.dw_cc_lang)
destroy(this.cb_exit)
destroy(this.cb_update)
end on

event closequery;//////////////////////////////////////////////////////////////////////////////
//
//	Event:  closequery
//
//	Description:
//	Search for unsaved datawindows prompting the user if any
//	pending updates are found.
//
//////////////////////////////////////////////////////////////////////////////
//	
//	Revision History
//
//	Version
//	5.0   Initial version
//
//////////////////////////////////////////////////////////////////////////////
//
//	Copyright © 1996-1997 Sybase, Inc. and its subsidiaries.  All rights reserved.
//	Any distribution of the PowerBuilder Foundation Classes (PFC)
//	source code by other than Sybase, Inc. and its subsidiaries is prohibited.
//
//////////////////////////////////////////////////////////////////////////////

Integer	li_pendingrc
Integer	li_validationrc
Integer	li_accepttextrc
Integer	li_msg
Integer	li_rc, li_rtn
String	ls_msgparms[]

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
			li_rtn = cb_update.Event clicked()
			IF li_rtn = 1 THEN
				RETURN 0
			END IF
			
			
			
//			// YES - Update
//			// If the update fails, prevent the window from closing
//			If This.Event pfc_save() >= 1 Then
//				// Successful update, allow the window to be closed
//				Return 0
//			End If
		Case 2
			// NO - Allow the window to be closed without saving changes
			Return 0
		Case 3
			// CANCEL -  Prevent the window from closing
	End Choose
End If

// Prevent the window from closing
ib_closestatus = False
Return 1
end event

type dw_cc_lang from u_pics_dw within w_cc_lang
integer x = 46
integer y = 24
integer width = 1435
integer height = 748
integer taborder = 20
string dataobject = "d_cc_lang"
end type

event constructor;call super::constructor;SetTransObject(SqlServerTrans)
this.Retrieve()
this.Event pfc_addrow()
end event

type cb_exit from u_cb within w_cc_lang
integer x = 1120
integer y = 804
integer taborder = 0
integer textsize = -10
string text = "E&xit"
end type

event clicked;close(parent)
end event

type cb_update from u_cb within w_cc_lang
event clicked pbm_bnclicked
integer x = 686
integer y = 804
integer taborder = 0
integer textsize = -10
string text = "&Update"
end type

event clicked;Long li_rtn_code
Integer li_loop, li_max_rows
String ls_lang_code

li_max_rows = dw_cc_lang.RowCount()

//Delete blank lines from the datawindow
FOR li_loop = li_max_rows TO 1 step -1
	ls_lang_code = dw_cc_lang.object.lang_code[li_loop]
	IF IsNull(ls_lang_code) THEN
		dw_cc_lang.DeleteRow(li_loop)
	END IF
NEXT

li_rtn_code = parent.Event pfc_save() 
	
IF li_rtn_code = 1 THEN
	COMMIT USING SQLServerTrans;										
	MessageBox('Update','Database updated.')
ELSE
	ROLLBACK USING SqlServerTrans;
	MessageBox('Error','Update Error .. Contact Your DBA')
END IF//IF li_rtn_code = 1 THEN


end event


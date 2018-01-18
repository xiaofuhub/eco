$PBExportHeader$w_nls_add_narrator.srw
forward
global type w_nls_add_narrator from w_response
end type
type dw_nls_narr_dataentry from u_pics_dw within w_nls_add_narrator
end type
type cb_delete from commandbutton within w_nls_add_narrator
end type
type cb_exit from u_cb within w_nls_add_narrator
end type
type cb_update from u_cb within w_nls_add_narrator
end type
end forward

global type w_nls_add_narrator from w_response
integer x = 681
integer y = 268
integer width = 2651
integer height = 1216
string title = "Add/Update Narrators"
dw_nls_narr_dataentry dw_nls_narr_dataentry
cb_delete cb_delete
cb_exit cb_exit
cb_update cb_update
end type
global w_nls_add_narrator w_nls_add_narrator

type variables


end variables

on w_nls_add_narrator.create
int iCurrent
call super::create
this.dw_nls_narr_dataentry=create dw_nls_narr_dataentry
this.cb_delete=create cb_delete
this.cb_exit=create cb_exit
this.cb_update=create cb_update
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_nls_narr_dataentry
this.Control[iCurrent+2]=this.cb_delete
this.Control[iCurrent+3]=this.cb_exit
this.Control[iCurrent+4]=this.cb_update
end on

on w_nls_add_narrator.destroy
call super::destroy
destroy(this.dw_nls_narr_dataentry)
destroy(this.cb_delete)
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

type dw_nls_narr_dataentry from u_pics_dw within w_nls_add_narrator
event ue_enterkey pbm_dwnprocessenter
integer y = 4
integer width = 2629
integer height = 968
integer taborder = 20
string dataobject = "d_nls_narr_dataentry"
end type

event ue_enterkey;SEND(Handle(this), 256, 9, Long(0,0))
RETURN(1)
end event

event constructor;call super::constructor;SetTransObject(SqlServerTrans)
//Retrieve the dealer codes
dw_nls_narr_dataentry.Retrieve()
dw_nls_narr_dataentry.Event pfc_addrow()
end event

type cb_delete from commandbutton within w_nls_add_narrator
integer x = 1929
integer y = 1012
integer width = 297
integer height = 88
integer taborder = 20
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "&Delete"
end type

event clicked;Int rtn
String lnarr

lnarr = TRIM(dw_nls_narr_dataentry.object.narr[dw_nls_narr_dataentry.GetRow()])+" "+ &
		  TRIM(dw_nls_narr_dataentry.object.narrfn[dw_nls_narr_dataentry.GetRow()])

rtn = MessageBox("Delete","Do you want to delete narrator :"+lnarr+" from database?",Question!,YesNo!,2)
IF rtn = 1 THEN
	rtn = MessageBox("Delete","Deleting this narrator, will remove all references to it. Continue?",Question!,YesNo!,2)
	IF rtn = 1 THEN
		dw_nls_narr_dataentry.DeleteRow(dw_nls_narr_dataentry.GetRow())
		rtn = dw_nls_narr_dataentry.Event pfc_update(TRUE, TRUE)
		IF rtn = 1 THEN
			COMMIT USING SqlServerTrans;
		ELSE
			ROLLBACK USING SqlServerTrans;
			MessageBox("ERROR","Delete failed.",StopSign!)
			dw_nls_narr_dataentry.Resetupdate()
		END IF
	END IF
END IF
end event

type cb_exit from u_cb within w_nls_add_narrator
integer x = 2277
integer y = 1008
integer width = 297
integer height = 88
integer taborder = 0
integer textsize = -10
string text = "E&xit"
end type

event clicked;call super::clicked;ib_disableclosequery=TRUE
Parent.Event pfc_close()
end event

type cb_update from u_cb within w_nls_add_narrator
event clicked pbm_bnclicked
integer x = 1577
integer y = 1008
integer width = 297
integer taborder = 0
integer textsize = -10
string text = "&Update"
end type

event clicked;Long li_rtn_code
Integer li_loop, li_max_rows
String lnarr

dw_nls_narr_dataentry.AcceptText()

li_max_rows = dw_nls_narr_dataentry.RowCount()

// delete blank lines from the datawindow
FOR li_loop = li_max_rows TO 1 step -1
	lnarr = dw_nls_narr_dataentry.GetItemString(li_loop,"narr")
	IF IsNull(lnarr) OR lnarr="" THEN
		dw_nls_narr_dataentry.DeleteRow(li_loop)
	END IF
NEXT


li_rtn_code = dw_nls_narr_dataentry.Event pfc_update(TRUE,FALSE)
IF li_rtn_code = 1 THEN
	COMMIT USING SqlServerTrans;
ELSE
	ROLLBACK USING SqlServerTrans;
END IF
dw_nls_narr_dataentry.ResetUpdate()


end event


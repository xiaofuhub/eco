$PBExportHeader$w_add_override.srw
forward
global type w_add_override from w_response
end type
type dw_override from u_dw within w_add_override
end type
type cb_exit from u_cb within w_add_override
end type
type cb_update from u_cb within w_add_override
end type
end forward

global type w_add_override from w_response
integer x = 681
integer y = 268
integer width = 1573
integer height = 1248
string title = "Override Code"
dw_override dw_override
cb_exit cb_exit
cb_update cb_update
end type
global w_add_override w_add_override

forward prototypes
public function boolean wf_check_duplicates (string ls_override_code)
end prototypes

public function boolean wf_check_duplicates (string ls_override_code);String Lovr
Integer li_loop, li_max_rows
Boolean Found
li_max_rows = dw_override.RowCount()
Found = FALSE
FOR li_loop = 1 TO li_max_rows
	Lovr = dw_override.object.ovrcd[li_loop]
	Lovr = RightTrim(Lovr)
	IF ls_override_code = Lovr THEN
	   FOUND = TRUE
		EXIT
   END IF		
NEXT//FOR li_loop = 1 TO li_max_rows
RETURN FOUND
end function

on w_add_override.create
int iCurrent
call super::create
this.dw_override=create dw_override
this.cb_exit=create cb_exit
this.cb_update=create cb_update
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_override
this.Control[iCurrent+2]=this.cb_exit
this.Control[iCurrent+3]=this.cb_update
end on

on w_add_override.destroy
call super::destroy
destroy(this.dw_override)
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

type dw_override from u_dw within w_add_override
event ue_enter_to_tab pbm_dwnprocessenter
integer x = 32
integer y = 32
integer width = 1467
integer height = 936
integer taborder = 10
string dataobject = "d_override"
end type

event ue_enter_to_tab;call super::ue_enter_to_tab;
SEND(Handle(this), 256, 9, Long(0,0))
RETURN(1)
end event

event constructor;call super::constructor;SetTransObject(SqlServerTrans)
//Retrieve the dealer codes
dw_override.Retrieve()
dw_override.Event pfc_addrow()
end event

event itemchanged;call super::itemchanged;String Null_string,Loverride
SetNull(Null_string)
Loverride = RightTrim(data)
IF DWO.name = "ovrcd" THEN
	IF wf_check_duplicates(Loverride) = TRUE THEN
	  dw_override.Object.ovrcd.validationmsg = "Duplicate override Code"
	  dw_override.SetText(Null_string)
	  dw_override.Object.ovrcd[row] = Null_string
	  RETURN 1
    END IF
	cb_update.Enabled = TRUE
	dw_override.Event pfc_addrow()
END IF//IF DWO.name = "ovrcd" THEN
end event

event updateend;call super::updateend;Long ll_total

ll_total = rowsinserted + rowsupdated

IF ll_total > 0 THEN
	MessageBox('Update','Updated Row(s) Successfully')
END IF
end event

type cb_exit from u_cb within w_add_override
integer x = 1152
integer y = 1020
integer taborder = 0
integer textsize = -10
string text = "E&xit"
end type

event clicked;call super::clicked;Parent.Event pfc_close()
end event

type cb_update from u_cb within w_add_override
event clicked pbm_bnclicked
integer x = 713
integer y = 1020
integer taborder = 0
integer textsize = -10
boolean enabled = false
string text = "&Update"
end type

event clicked;call super::clicked;Long li_rtn_code
Integer li_loop, li_max_rows
String ls_override

li_max_rows = dw_override.RowCount()

//Delete blank lines from the datawindow
FOR li_loop = li_max_rows TO 1 step -1
	ls_override = dw_override.Object.ovrcd[li_loop]
	IF IsNull(ls_override) OR ls_override="" THEN
		dw_override.DeleteRow(li_loop)
	END IF
NEXT


//IF No modifications then do not update.
IF dw_override.ModifiedCount() > 0 THEN
		li_rtn_code = dw_override.Event pfc_update(TRUE,FALSE)
		IF li_rtn_code = 1 THEN
			DataWindowChild ldwc_ovrd
			w_sheet_mm_bk_new_invoicing.dw_mm_inv.GetChild ("ovrcd", ldwc_ovrd)
			ldwc_ovrd.SetTransObject(sqlservertrans)
			ldwc_ovrd.Retrieve()
			dw_override.ResetUpdate()
			return 1
		ELSE
			ROLLBACK USING SqlServerTrans;
			MessageBox('Error','Update Error .. Contact Your DBA')
		END IF//IF li_rtn_code = 1 THEN

END IF//IF dw_override.ModifiedCount() > 0 THEN
end event


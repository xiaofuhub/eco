$PBExportHeader$w_pa_add_ext_rsn.srw
forward
global type w_pa_add_ext_rsn from w_response
end type
type dw_pa_ext_rsn from u_pics_dw within w_pa_add_ext_rsn
end type
type cb_exit from u_cb within w_pa_add_ext_rsn
end type
type cb_update from u_cb within w_pa_add_ext_rsn
end type
end forward

global type w_pa_add_ext_rsn from w_response
integer x = 443
integer y = 452
integer width = 2066
integer height = 1020
string title = "Extension Reason and Descriptions"
dw_pa_ext_rsn dw_pa_ext_rsn
cb_exit cb_exit
cb_update cb_update
end type
global w_pa_add_ext_rsn w_pa_add_ext_rsn

forward prototypes
public function boolean wf_check_duplicates (string ls_dealer_code)
end prototypes

public function boolean wf_check_duplicates (string ls_dealer_code);return true
end function

on w_pa_add_ext_rsn.create
int iCurrent
call super::create
this.dw_pa_ext_rsn=create dw_pa_ext_rsn
this.cb_exit=create cb_exit
this.cb_update=create cb_update
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_pa_ext_rsn
this.Control[iCurrent+2]=this.cb_exit
this.Control[iCurrent+3]=this.cb_update
end on

on w_pa_add_ext_rsn.destroy
call super::destroy
destroy(this.dw_pa_ext_rsn)
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

type dw_pa_ext_rsn from u_pics_dw within w_pa_add_ext_rsn
event ue_enterkey pbm_dwnprocessenter
integer x = 37
integer y = 28
integer width = 1993
integer height = 724
integer taborder = 20
string dataobject = "d_pa_ext_rsn"
end type

event ue_enterkey;SEND(Handle(this), 256, 9, Long(0,0))
RETURN(1)
end event

event constructor;call super::constructor;SetTransObject(SqlServerTrans)
This.Retrieve()
This.Event pfc_addrow()
end event

event sqlpreview;call super::sqlpreview;//
end event

event rbuttondown;call super::rbuttondown;//////////////////////////////////////////////////////////////////////////////
//	Event:			rbuttondown
//	Description:	Allow for focus change on rbuttondown
//////////////////////////////////////////////////////////////////////////////
//	Rev. History	Version
//						5.0   Initial version
// 					6.0 	Added Linkage service notification.
//////////////////////////////////////////////////////////////////////////////
//	Copyright © 1996-1999 Sybase, Inc. and its subsidiaries.  All rights reserved.  Any distribution of the 
// PowerBuilder Foundation Classes (PFC) source code by other than Sybase, Inc. and its subsidiaries is prohibited.
//////////////////////////////////////////////////////////////////////////////
integer	li_rc
long		ll_currow
string	ls_colname
string	ls_curcolname

// Validate arguments.
if not ib_RMBfocuschange or IsNull (dwo) or row <= 0 then return

if IsValid (inv_Linkage) then
	If inv_Linkage.event pfc_rbuttondown (xpos, ypos, row, dwo) <> &
		inv_Linkage.CONTINUE_ACTION then
		// The user or an error prevents changing to a new row.
		return
	end if
end if

if IsValid (inv_RowSelect) then inv_RowSelect.event pfc_rbuttondown (xpos, ypos, row, dwo)

if dwo.type <> "column" then return

// Perform no action if already over current row/column.
ls_colname = dwo.name
ls_curcolname = this.GetColumnName()
ll_currow = this.GetRow()
if (ls_colname = ls_curcolname) and (row = ll_currow) then return

// Set row & column.
if this.SetRow (row) = 1 then	this.SetColumn (ls_colname)
end event

type cb_exit from u_cb within w_pa_add_ext_rsn
integer x = 1669
integer y = 780
integer taborder = 0
integer textsize = -10
string text = "E&xit"
end type

event clicked;close(parent)
end event

type cb_update from u_cb within w_pa_add_ext_rsn
event clicked pbm_bnclicked
integer x = 1257
integer y = 780
integer taborder = 0
integer textsize = -10
string text = "&Update"
end type

event clicked;Long li_rtn_code
Integer li_loop, li_max_rows
String ls_grade_code

li_max_rows = dw_pa_ext_rsn.RowCount()

//Delete blank lines from the datawindow
FOR li_loop = li_max_rows TO 1 step -1
	ls_grade_code = dw_pa_ext_rsn.GetItemString(li_loop,"ext_rsn_cd")
	IF IsNull(ls_grade_code) THEN
		dw_pa_ext_rsn.DeleteRow(li_loop)
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


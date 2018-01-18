$PBExportHeader$w_add_mag_code.srw
forward
global type w_add_mag_code from w_response
end type
type cb_delete from commandbutton within w_add_mag_code
end type
type dw_magcode from u_dw within w_add_mag_code
end type
type cb_exit from u_cb within w_add_mag_code
end type
type cb_update from u_cb within w_add_mag_code
end type
end forward

global type w_add_mag_code from w_response
integer x = 681
integer y = 268
integer width = 1573
integer height = 1248
string title = "Magazine Code and Titles"
cb_delete cb_delete
dw_magcode dw_magcode
cb_exit cb_exit
cb_update cb_update
end type
global w_add_mag_code w_add_mag_code

type variables
string lparm

end variables

forward prototypes
public function boolean wf_check_duplicates (string ls_mag_code)
end prototypes

public function boolean wf_check_duplicates (string ls_mag_code);String Lmag
Integer li_loop, li_max_rows
Boolean Found
li_max_rows = dw_magcode.RowCount()
Found = FALSE
FOR li_loop = 1 TO li_max_rows
	Lmag = dw_magcode.object.magcd[li_loop]
	Lmag = RightTrim(Lmag)
	IF ls_mag_code = Lmag THEN
	   FOUND = TRUE
		EXIT
   END IF		
NEXT//FOR li_loop = 1 TO li_max_rows
RETURN FOUND
end function

on w_add_mag_code.create
int iCurrent
call super::create
this.cb_delete=create cb_delete
this.dw_magcode=create dw_magcode
this.cb_exit=create cb_exit
this.cb_update=create cb_update
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.cb_delete
this.Control[iCurrent+2]=this.dw_magcode
this.Control[iCurrent+3]=this.cb_exit
this.Control[iCurrent+4]=this.cb_update
end on

on w_add_mag_code.destroy
call super::destroy
destroy(this.cb_delete)
destroy(this.dw_magcode)
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

event open;call super::open;lparm = Message.StringParm
end event

type cb_delete from commandbutton within w_add_mag_code
integer x = 896
integer y = 1024
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
String lmagcd

lmagcd = dw_magcode.object.magcd[dw_magcode.GetRow()]


rtn = MessageBox("Delete","Do you want to delete magazine code :"+lmagcd+" from database?",Question!,YesNo!,2)
IF rtn = 1 THEN
	MessageBox("Delete","Deleting this magazine code will remove all references to it. Continue?",Question!,YesNo!,2)
	IF rtn = 1 THEN
		dw_magcode.DeleteRow(dw_magcode.GetRow())
		rtn = dw_magcode.Event pfc_update(TRUE, TRUE)
		if rtn = 1 THEN
			COMMIT USING SQLServerTrans;
			MessageBox("Delete","Delete Successful.",Information!)
			DataWindowChild ldwc_magcd
			IF lparm="RC" THEN
				w_magazine_maintenance.dw_magazine_maintenance_rc.GetChild ("magcd", ldwc_magcd)				
			ELSE
				w_magazine_maintenance.dw_magazine_maintenance.GetChild ("mag_magcd", ldwc_magcd)
			END IF
			ldwc_magcd.SetTransObject(sqlservertrans)
			ldwc_magcd.Retrieve()
			dw_magcode.Resetupdate()
			return 1
		else
			MessageBox("ERROR","Delete failed.",StopSign!)
			dw_magcode.Resetupdate()
			RETURN 0
		end if
	END IF
END IF
end event

type dw_magcode from u_dw within w_add_mag_code
event ue_enter_to_tab pbm_dwnprocessenter
integer x = 32
integer y = 32
integer width = 1467
integer height = 936
integer taborder = 10
string dataobject = "d_mag_code"
end type

event ue_enter_to_tab;call super::ue_enter_to_tab;
SEND(Handle(this), 256, 9, Long(0,0))
RETURN(1)
end event

event constructor;call super::constructor;SetTransObject(SqlServerTrans)
//Retrieve the dealer codes
dw_magcode.Retrieve()
dw_magcode.Event pfc_addrow()
end event

event itemchanged;call super::itemchanged;String Null_string,Lmagcode
SetNull(Null_string)
Lmagcode = RightTrim(data)
IF DWO.name = "magcd" THEN
	IF wf_check_duplicates(Lmagcode) = TRUE THEN
	  dw_magcode.Object.ovrcd.validationmsg = "Duplicate magazine Code"
	  dw_magcode.SetText(Null_string)
	  dw_magcode.SetItem(row,"magcd",Null_string)
	  RETURN 1
    END IF
	cb_update.Enabled = TRUE
	dw_magcode.Event pfc_addrow()
END IF//IF DWO.name = "ovrcd" THEN
end event

event updateend;call super::updateend;Long ll_total

ll_total = rowsinserted + rowsupdated

IF ll_total > 0 THEN
	MessageBox('Update','Updated Row(s) Successfully')
END IF
end event

type cb_exit from u_cb within w_add_mag_code
integer x = 1243
integer y = 1020
integer width = 297
integer taborder = 0
integer textsize = -10
string text = "E&xit"
end type

event clicked;call super::clicked;Parent.Event pfc_close()
end event

type cb_update from u_cb within w_add_mag_code
event clicked pbm_bnclicked
integer x = 544
integer y = 1020
integer width = 297
integer taborder = 0
integer textsize = -10
string text = "&Update"
end type

event clicked;Long li_rtn_code
Integer li_loop, li_max_rows
String ls_magcode

dw_magcode.AcceptText()

li_max_rows = dw_magcode.RowCount()

//Delete blank lines from the datawindow
FOR li_loop = li_max_rows TO 1 step -1
	ls_magcode = dw_magcode.GetItemString(li_loop,"magcd")
	IF IsNull(ls_magcode) OR ls_magcode="" THEN
		dw_magcode.DeleteRow(li_loop)
	END IF
NEXT


//IF No modifications then do not update.
IF dw_magcode.ModifiedCount() > 0 THEN
		li_rtn_code = dw_magcode.Event pfc_update(TRUE,FALSE)
		IF li_rtn_code = 1 THEN
			DataWindowChild ldwc_magcd
			IF lparm="RC" THEN
				w_magazine_maintenance.dw_magazine_maintenance_rc.GetChild ("magcd", ldwc_magcd)				
			ELSE
				w_magazine_maintenance.dw_magazine_maintenance.GetChild ("mag_magcd", ldwc_magcd)
			END IF
			ldwc_magcd.SetTransObject(sqlservertrans)
			ldwc_magcd.Retrieve()
			dw_magcode.ResetUpdate()
			return 1
		ELSE
			ROLLBACK USING SqlServerTrans;
			MessageBox('Error','Update Error .. Contact Your DBA')
		END IF//IF li_rtn_code = 1 THEN

END IF
end event


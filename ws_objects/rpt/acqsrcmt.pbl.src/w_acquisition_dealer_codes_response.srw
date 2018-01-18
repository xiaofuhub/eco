$PBExportHeader$w_acquisition_dealer_codes_response.srw
$PBExportComments$Window to maintain dealer codes
forward
global type w_acquisition_dealer_codes_response from w_response
end type
type dw_dealer_codes from u_dw within w_acquisition_dealer_codes_response
end type
type cb_exit from u_cb within w_acquisition_dealer_codes_response
end type
type cb_update from u_cb within w_acquisition_dealer_codes_response
end type
end forward

global type w_acquisition_dealer_codes_response from w_response
int X=444
int Y=453
int Width=2039
int Height=1021
boolean TitleBar=true
string Title="Maintain Acquisition Dealer Codes"
dw_dealer_codes dw_dealer_codes
cb_exit cb_exit
cb_update cb_update
end type
global w_acquisition_dealer_codes_response w_acquisition_dealer_codes_response

forward prototypes
public function boolean wf_check_duplicates (string ls_dealer_code)
end prototypes

public function boolean wf_check_duplicates (string ls_dealer_code);Integer li_loop, li_max_rows
Boolean Found

li_max_rows = dw_dealer_codes.RowCount()
Found = FALSE
FOR li_loop = 1 TO li_max_rows
	
	IF ls_dealer_code = dw_dealer_codes.GetItemString(li_loop,"dlrcd") THEN
	   FOUND = TRUE
		EXIT
   END IF		
	
	
NEXT//FOR li_loop = 1 TO li_max_rows
RETURN FOUND
end function

on w_acquisition_dealer_codes_response.create
int iCurrent
call w_response::create
this.dw_dealer_codes=create dw_dealer_codes
this.cb_exit=create cb_exit
this.cb_update=create cb_update
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=dw_dealer_codes
this.Control[iCurrent+2]=cb_exit
this.Control[iCurrent+3]=cb_update
end on

on w_acquisition_dealer_codes_response.destroy
call w_response::destroy
destroy(this.dw_dealer_codes)
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

type dw_dealer_codes from u_dw within w_acquisition_dealer_codes_response
event ue_enter_to_tab pbm_dwnprocessenter
int X=247
int Y=97
int Width=1468
int Height=485
int TabOrder=10
string DataObject="d_acquisition_dealer_code_maintenance"
end type

event ue_enter_to_tab;call super::ue_enter_to_tab;
SEND(Handle(this), 256, 9, Long(0,0))
RETURN(1)
end event

event constructor;call super::constructor;SetTransObject(SqlServerTrans)
//Retrieve the dealer codes
dw_dealer_codes.Retrieve()
dw_dealer_codes.Event pfc_addrow()
end event

event itemchanged;call super::itemchanged;String Null_string

SetNull(Null_string)



IF DWO.name = "dlrcd" THEN
	IF wf_check_duplicates(Data) = TRUE THEN
	  dw_dealer_codes.Object.dlrcd.validationmsg = "Duplicate Dealer Code"
	  dw_dealer_codes.SetText(Null_string)
	  dw_dealer_codes.SetItem(row,"dlrcd",Null_string)
	  RETURN 1
    END IF
	
	cb_update.Enabled = TRUE
	dw_dealer_codes.Event pfc_addrow()
	
	
END IF//IF DWO.name = "dlrcd" THEN
end event

event updateend;call super::updateend;Long ll_total

ll_total = rowsinserted + rowsupdated

IF ll_total > 0 THEN
	MessageBox('Update','Update successful.')
END IF
end event

type cb_exit from u_cb within w_acquisition_dealer_codes_response
int X=1345
int Y=749
int TabOrder=0
string Text="E&xit"
int TextSize=-10
end type

event clicked;call super::clicked;
Parent.Event pfc_close()
end event

type cb_update from u_cb within w_acquisition_dealer_codes_response
event clicked pbm_bnclicked
int X=906
int Y=749
int TabOrder=0
boolean Enabled=false
string Text="&Update"
int TextSize=-10
end type

event clicked;call super::clicked;Long li_rtn_code
Integer li_loop, li_max_rows
String ls_dealer_code

li_max_rows = dw_dealer_codes.RowCount()

//Delete blank lines from the datawindow
FOR li_loop = li_max_rows TO 1 step -1
	ls_dealer_code = dw_dealer_codes.GetItemString(li_loop,"dlrcd")
	IF IsNull(ls_dealer_code) THEN
		dw_dealer_codes.DeleteRow(li_loop)
	END IF
NEXT


//IF No modifications then do not update.
IF dw_dealer_codes.ModifiedCount() > 0 THEN

		li_rtn_code = dw_dealer_codes.Update()
		
		IF li_rtn_code = 1 THEN
			
					DataWindowChild ldwc_pub
					w_acquisition_print_book_orders.dw_acquisition_orders.GetChild ("acquist_pbdlrcd", ldwc_pub)
					ldwc_pub.SetTransObject(sqlservertrans)
					ldwc_pub.Retrieve()
					
					RETURN 1					
										
		ELSE
			ROLLBACK USING SqlServerTrans;
			MessageBox('Error','Update Error .. Contact Your DBA')
		END IF//IF li_rtn_code = 1 THEN

END IF//IF dw_dealer_codes.ModifiedCount() > 0 THEN
end event


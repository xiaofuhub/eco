$PBExportHeader$w_acquisition_print_book_receive.srw
$PBExportComments$Window updating print book receive
forward
global type w_acquisition_print_book_receive from w_sheet
end type
type cb_exit from u_cb within w_acquisition_print_book_receive
end type
type cb_clear from u_cb within w_acquisition_print_book_receive
end type
type cb_update from u_cb within w_acquisition_print_book_receive
end type
type dw_acquisition_receive from u_dw within w_acquisition_print_book_receive
end type
type cb_find from u_cb within w_acquisition_print_book_receive
end type
end forward

global type w_acquisition_print_book_receive from w_sheet
integer width = 3461
integer height = 1716
string title = "Acquisition Of Source Material - Print Book Receive"
cb_exit cb_exit
cb_clear cb_clear
cb_update cb_update
dw_acquisition_receive dw_acquisition_receive
cb_find cb_find
end type
global w_acquisition_print_book_receive w_acquisition_print_book_receive

forward prototypes
public subroutine wf_disable_fields ()
public subroutine wf_enable_fields ()
end prototypes

public subroutine wf_disable_fields ();dw_acquisition_receive.Object.acquist_chno.tabsequence = '10'
dw_acquisition_receive.Object.acquist_pbrecdt.tabsequence = '0'
dw_acquisition_receive.Object.acquist_pboh.tabsequence = '0'
dw_acquisition_receive.Object.acquist_pbdlrcd.tabsequence = '0'

end subroutine

public subroutine wf_enable_fields ();dw_acquisition_receive.Object.acquist_chno.tabsequence = '0'
dw_acquisition_receive.Object.acquist_pbrecdt.tabsequence = '20'
dw_acquisition_receive.Object.acquist_pboh.tabsequence = '30'
dw_acquisition_receive.Object.acquist_pbdlrcd.tabsequence = '40'

end subroutine

on w_acquisition_print_book_receive.create
int iCurrent
call super::create
this.cb_exit=create cb_exit
this.cb_clear=create cb_clear
this.cb_update=create cb_update
this.dw_acquisition_receive=create dw_acquisition_receive
this.cb_find=create cb_find
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.cb_exit
this.Control[iCurrent+2]=this.cb_clear
this.Control[iCurrent+3]=this.cb_update
this.Control[iCurrent+4]=this.dw_acquisition_receive
this.Control[iCurrent+5]=this.cb_find
end on

on w_acquisition_print_book_receive.destroy
call super::destroy
destroy(this.cb_exit)
destroy(this.cb_clear)
destroy(this.cb_update)
destroy(this.dw_acquisition_receive)
destroy(this.cb_find)
end on

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

inv_resize.of_Register(cb_update, "scale")
inv_resize.of_Register(cb_clear, "scale")
inv_resize.of_Register(cb_exit, "scale")
inv_resize.of_Register(cb_find, "scale")
inv_resize.of_Register(dw_acquisition_receive, "scale")
end event

event open;call super::open;//open the sheet in maximized state
this.windowstate = maximized!

//disable the datawindow fields
wf_disable_fields()
end event

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

Integer	li_pendingrc,rtn
Integer	li_validationrc
Integer	li_accepttextrc
Integer	li_msg
Integer	li_rc
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
			cb_update.TriggerEvent(clicked!)
			IF rtn = 0 THEN
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

event mousemove;call super::mousemove;w_pics_main.Event pfc_microhelp("Ready")
end event

type cb_exit from u_cb within w_acquisition_print_book_receive
event ue_hint_text pbm_mousemove
string tag = "Exits the screen"
integer x = 3054
integer y = 1340
integer width = 297
integer taborder = 0
integer textsize = -10
string text = "E&xit"
end type

event ue_hint_text;call super::ue_hint_text;w_pics_main.Event pfc_microhelp(this.tag)
end event

event clicked;call super::clicked;
Parent.Event pfc_close()
end event

type cb_clear from u_cb within w_acquisition_print_book_receive
event ue_hint_text pbm_mousemove
string tag = "Clears the screen for input"
integer x = 2720
integer y = 1340
integer width = 297
integer taborder = 0
boolean bringtotop = true
integer textsize = -10
string text = "&Clear"
end type

event ue_hint_text;call super::ue_hint_text;w_pics_main.Event pfc_microhelp(this.tag)
end event

event clicked;call super::clicked;ib_disableclosequery=TRUE
//reset the datawindow
dw_acquisition_receive.Reset()

//remove the where clause and set query mode to yes
dw_acquisition_receive.Object.DataWindow.QueryClear = "yes"
dw_acquisition_receive.Object.DataWindow.QueryMode = "yes"

//Disable the Find button
cb_find.Enabled = FALSE
cb_update.Enabled = FALSE



//Disable fields and Set focus to the datawindow
wf_disable_fields()
dw_acquisition_receive.SetFocus()
dw_acquisition_receive.setcolumn("acquist_chno")


end event

type cb_update from u_cb within w_acquisition_print_book_receive
event ue_hint_text pbm_mousemove
string tag = "Update the database"
integer x = 2386
integer y = 1340
integer width = 297
integer taborder = 0
boolean bringtotop = true
integer textsize = -10
boolean enabled = false
string text = "&Update"
end type

event ue_hint_text;call super::ue_hint_text;w_pics_main.Event pfc_microhelp(this.tag)
end event

event clicked;call super::clicked;Integer li_rtn_code,i,row_cnt
string li_pboh,ls_receive_date,null_date

SetNull(null_date)
SetNull(li_pboh)

dw_acquisition_receive.AcceptText()
row_cnt = dw_acquisition_receive.RowCount()

FOR i=1 TO row_cnt

	ls_receive_date 	= 	string(dw_acquisition_receive.object.acquist_pbrecdt[i],'MM/DD/YYYY')
	li_pboh				=	TRIM(STRING(dw_acquisition_receive.object.acquist_pboh[i]))
	
	IF (NOT(IsNull(ls_receive_date)) AND ls_receive_date<>"") AND (IsNull(li_pboh) OR li_pboh='0') THEN
		MessageBox("ERROR","~'Received date~' and ~'Copies received~' must have values.",StopSign!)
		RETURN
	ELSEIF (IsNull(ls_receive_date) OR ls_receive_date="") AND NOT(IsNull(li_pboh)) THEN
		MessageBox("ERROR","~'Received date~' and ~'Copies received~' must have values.",StopSign!)
		RETURN
	END IF
NEXT

li_rtn_code = dw_acquisition_receive.Event pfc_update(TRUE,FALSE)

IF li_rtn_code = 1 THEN 
	COMMIT USING SqlServerTrans;
	dw_acquisition_receive.ResetUpdate()
	RETURN 0
ELSE
	ROLLBACK USING SqlServerTrans;
	MessageBox('Error','Error Updating the Database .. Contact Your DBA')
END IF

end event

type dw_acquisition_receive from u_dw within w_acquisition_print_book_receive
event ue_enter_to_tab pbm_dwnprocessenter
event ue_hint_text pbm_mousemove
integer x = 23
integer y = 48
integer width = 3328
integer height = 1228
integer taborder = 10
string dataobject = "d_acquisition_receive"
boolean hscrollbar = true
end type

event ue_enter_to_tab;call super::ue_enter_to_tab;
Send(Handle(this), 256, 9, Long(0,0))
Return (1)
end event

event ue_hint_text;//string ls_object, ls_column, ls_column_tag
//long ll_pos
//
////This script set's microhelp at the bottom of the screen for the Archive Title Datawindow
//ls_object = THIS.getobjectatpointer()
//ll_pos = pos(ls_object, "~t")
//IF NOT pos(ls_object, "_t~t") > 0 THEN
//	IF ll_pos > 0 THEN
//		ll_pos = ll_pos -1
//		ls_column = mid(ls_object,1,ll_pos)
//		ls_column_tag = THIS.Describe(ls_column + ".tag")
//		w_pics_main.setmicrohelp(ls_column_tag)
//	ELSE
//		w_pics_main.setmicrohelp("Ready")
//	END IF
//END IF
end event

event constructor;SetTransObject(SqlServerTrans)
this.of_SetDropDownCalendar(TRUE)
this.iuo_calendar.of_Register("acquist_pbordt",this.iuo_calendar.DDLB)
this.iuo_calendar.of_Register("acquist_pbrecdt",this.iuo_calendar.DDLB)

dw_acquisition_receive.Object.DataWindow.QueryMode = "yes"

end event

event itemchanged;call super::itemchanged;String ls_receive_date
Integer null_value
date null_date

SetNull(null_date)
SetNull(null_value)


//Enable the find and update button after chart number has been input
IF DWO.Name = 'acquist_chno'   THEN
	cb_find.Enabled = TRUE
	dw_acquisition_receive.Event pfc_addrow()
//IF the receive date has been cleared then null the copies received field.
ELSEIF DWO.Name = "acquist_pbrecdt" THEN
	ls_receive_date = GetText()
	
	IF IsNull(ls_receive_date) OR Trim(ls_receive_date) = "" THEN
		dw_acquisition_receive.object.acquist_pboh[row]=null_value
		dw_acquisition_receive.object.acquist_pbrecdt[row]=today()
	END IF
END IF//IF DWO.Name = "acquist_pbrecdt" THEN


end event

event doubleclicked;call super::doubleclicked;Date null_date
Integer null_number

SetNull(null_date)
SetNull(null_number)

//IF the receive date field is double clicked it clears the receive date and
//the copies received fields.
IF DWO.Name = "acquist_pbrecdt" THEN
	this.SetText('')
	this.Object.acquist_pbrecdt[row] = null_date
	this.Object.acquist_pboh[row] = null_number
END IF
	

end event

event updateend;call super::updateend;//Output message if row(s) were successfully updated
IF rowsupdated > 0 THEN
		MessageBox('Status','Row(s) Successfully Updated')
ELSE
		MessageBox('Status','No updates')
END IF
end event

type cb_find from u_cb within w_acquisition_print_book_receive
event ue_hint_text pbm_mousemove
string tag = "Get data"
integer x = 2062
integer y = 1340
integer width = 297
integer taborder = 0
integer textsize = -10
boolean enabled = false
string text = "F&ind"
boolean default = true
end type

event ue_hint_text;call super::ue_hint_text;w_pics_main.Event pfc_microhelp(this.tag)
end event

event clicked;call super::clicked;Integer li_rtn_code, li_row_numbers, li_loop, lpbneed, lpboh



//Disable the query mode and retrieve data
dw_acquisition_receive.Object.DataWindow.QueryMode = "no"
li_rtn_code = dw_acquisition_receive.Retrieve()

IF li_rtn_code = 0 THEN
	MessageBox('Error','No Data Found')
	cb_clear.TriggerEvent(clicked!)
	Return
ELSE
	//Set focus to the datawindow
	wf_enable_fields()
	dw_acquisition_receive.SetFocus()
	
	//If the recieve date is null then put today's date
	li_row_numbers = dw_acquisition_receive.RowCount()
	FOR li_loop = 1 TO li_row_numbers
		
		IF IsNull(dw_acquisition_receive.object.acquist_pbrecdt[li_loop]) THEN
			dw_acquisition_receive.object.acquist_pbrecdt[li_loop] = today()
		END IF
		lpbneed = dw_acquisition_receive.object.acquist_pbneed[li_loop]
		lpboh = dw_acquisition_receive.object.acquist_pboh[li_loop]
		IF IsNull(lpbneed)=FALSE AND IsNull(lpboh) THEN
			dw_acquisition_receive.object.acquist_pboh[li_loop] = lpbneed
		END IF
		
	NEXT
	
	
	//Set focus to the chno field and enable the update button
	dw_acquisition_receive.setcolumn("acquist_chno")
	cb_update.Enabled = TRUE
END IF
ib_disableclosequery=TRUE

end event


$PBExportHeader$w_dtbformat.srw
forward
global type w_dtbformat from w_response
end type
type dw_dtbformat from u_pics_dw within w_dtbformat
end type
type cb_exit from commandbutton within w_dtbformat
end type
type cb_update from commandbutton within w_dtbformat
end type
end forward

global type w_dtbformat from w_response
integer x = 142
integer y = 168
integer width = 3095
integer height = 1068
string title = "DTB Format"
dw_dtbformat dw_dtbformat
cb_exit cb_exit
cb_update cb_update
end type
global w_dtbformat w_dtbformat

type variables
string lcntr,lcntrmed,lcntrtype

end variables

on w_dtbformat.create
int iCurrent
call super::create
this.dw_dtbformat=create dw_dtbformat
this.cb_exit=create cb_exit
this.cb_update=create cb_update
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_dtbformat
this.Control[iCurrent+2]=this.cb_exit
this.Control[iCurrent+3]=this.cb_update
end on

on w_dtbformat.destroy
call super::destroy
destroy(this.dw_dtbformat)
destroy(this.cb_exit)
destroy(this.cb_update)
end on

event closequery;Integer	li_pendingrc
Integer	li_validationrc
Integer	li_accepttextrc
Integer	li_msg,rtn
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
			// YES - Update
			// If the update fails, prevent the window from closing
			rtn = cb_update.Event Clicked()
			If rtn = 1 Then
				// Successful update, allow the window to be closed
				Return 0
			End If
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

type dw_dtbformat from u_pics_dw within w_dtbformat
integer x = 27
integer y = 28
integer width = 2990
integer height = 692
integer taborder = 10
string dataobject = "d_dtbformat"
boolean vscrollbar = false
boolean livescroll = false
end type

event constructor;call super::constructor;// set the transaction object.
this.SetTransObject(SQLServerTrans)

this.retrieve()
end event

type cb_exit from commandbutton within w_dtbformat
integer x = 2798
integer y = 788
integer width = 229
integer height = 108
boolean bringtotop = true
integer textsize = -10
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "E&xit"
end type

event clicked;parent.Event pfc_close()

end event

type cb_update from commandbutton within w_dtbformat
integer x = 2478
integer y = 788
integer width = 265
integer height = 112
boolean bringtotop = true
integer textsize = -10
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "&Update"
end type

event clicked;int rc,i

dw_dtbformat.accepttext()


rc = dw_dtbformat.Event pfc_update(TRUE,TRUE)
		
if rc=1 THEN
	COMMIT USING SQLServerTrans;
	MessageBox("Update","update successful.",StopSign!)
	return 1
else 
	ROLLBACK USING SQLServerTrans;
	MessageBox("ERROR","Update failed.",StopSign!)
	return 0
end if
end event


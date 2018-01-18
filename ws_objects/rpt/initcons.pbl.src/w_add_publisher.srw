$PBExportHeader$w_add_publisher.srw
$PBExportComments$This scree will allow CDS section to add publishers to the system
forward
global type w_add_publisher from w_response
end type
type cb_exit from commandbutton within w_add_publisher
end type
type cb_update from commandbutton within w_add_publisher
end type
type dw_add_publisher from u_pics_dw within w_add_publisher
end type
type cb_clear from commandbutton within w_add_publisher
end type
type cb_find from commandbutton within w_add_publisher
end type
end forward

global type w_add_publisher from w_response
integer x = 165
integer y = 452
integer width = 2542
integer height = 820
string title = "Add/Update Publisher"
cb_exit cb_exit
cb_update cb_update
dw_add_publisher dw_add_publisher
cb_clear cb_clear
cb_find cb_find
end type
global w_add_publisher w_add_publisher

on w_add_publisher.create
int iCurrent
call super::create
this.cb_exit=create cb_exit
this.cb_update=create cb_update
this.dw_add_publisher=create dw_add_publisher
this.cb_clear=create cb_clear
this.cb_find=create cb_find
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.cb_exit
this.Control[iCurrent+2]=this.cb_update
this.Control[iCurrent+3]=this.dw_add_publisher
this.Control[iCurrent+4]=this.cb_clear
this.Control[iCurrent+5]=this.cb_find
end on

on w_add_publisher.destroy
call super::destroy
destroy(this.cb_exit)
destroy(this.cb_update)
destroy(this.dw_add_publisher)
destroy(this.cb_clear)
destroy(this.cb_find)
end on

event open;call super::open;dw_add_publisher.Object.pubname.TabSequence='0'
dw_add_publisher.Object.changed.TabSequence='0'

dw_add_publisher.Object.DataWindow.QueryMode='Yes'
w_add_publisher.cb_update.Enabled=FALSE
w_add_publisher.cb_clear.Enabled=FALSE
dw_add_publisher.SetFocus()
end event

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

type cb_exit from commandbutton within w_add_publisher
integer x = 2245
integer y = 584
integer width = 247
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

type cb_update from commandbutton within w_add_publisher
integer x = 1609
integer y = 584
integer width = 311
integer height = 108
boolean bringtotop = true
integer textsize = -10
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "&Update"
end type

event clicked;int rc

// Accept the text that was put on the screen.
rc = dw_add_publisher.AcceptText()

// If there were no changes to the screen, don't try to update the screen.
IF rc=1 THEN
	IF dw_add_publisher.ModifiedCount() > 0 THEN
		rc = dw_add_publisher.Event pfc_update(TRUE,FALSE)
		if rc = 1 THEN
			COMMIT USING SqlServerTrans;
			DataWindowChild ldwc_pub
			w_sheet_initial_title.dw_title.GetChild ("ttlinit_publisher", ldwc_pub)
			ldwc_pub.SetTransObject(sqlservertrans)
			ldwc_pub.Retrieve()
			dw_add_publisher.ResetUpdate()
			MessageBox("Update","Update Successful.",Information!)
			return 1
		else
			ROLLBACK USING SqlServerTrans;
			dw_add_publisher.ResetUpdate()
			MessageBox("ERROR","Update failed.",StopSign!)
			RETURN 0
		end if
	END IF
END IF
end event

type dw_add_publisher from u_pics_dw within w_add_publisher
integer x = 41
integer y = 36
integer width = 2450
integer height = 508
integer taborder = 10
string dataobject = "d_add_publisher"
boolean livescroll = false
end type

event ue_postconstructor();call super::ue_postconstructor;// set the transaction object.
dw_add_publisher.SetTransObject(SQLServerTrans)

dw_add_publisher.of_SetDropDownSearch(TRUE)
dw_add_publisher.inv_dropdownsearch.of_AddColumn("pubabb")

end event

type cb_clear from commandbutton within w_add_publisher
integer x = 1961
integer y = 584
integer width = 251
integer height = 108
boolean bringtotop = true
integer textsize = -10
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "&Clear"
end type

event clicked;long ll_rows,rtn

dw_add_publisher.AcceptText()

IF dw_add_publisher.ModifiedCount( ) > 0 THEN
	rtn=MessageBox("Clear","Save the changes before clearing the screen?",Question!,OkCancel!,2)
	IF rtn = 1 THEN
		w_add_publisher.cb_update.TriggerEvent(Clicked!)
	END IF
END IF
w_add_publisher.cb_find.Enabled=TRUE
w_add_publisher.cb_find.Default=TRUE
w_add_publisher.cb_update.Enabled=FALSE
w_add_publisher.cb_clear.Enabled=FALSE
dw_add_publisher.ResetUpdate()
dw_add_publisher.Object.DataWindow.QueryClear='Yes'
dw_add_publisher.Object.DataWindow.QueryMode='Yes'
dw_add_publisher.SetFocus()

end event

type cb_find from commandbutton within w_add_publisher
event clicked pbm_bnclicked
integer x = 1326
integer y = 584
integer width = 247
integer height = 108
integer taborder = 20
boolean bringtotop = true
integer textsize = -10
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "F&ind"
boolean default = true
end type

event clicked;int ll_rows,rtn
String Lpub

Lpub = dw_add_publisher.GetText()
IF Lpub <> "" THEN
	dw_add_publisher.Object.DataWindow.QueryMode='No'
	ll_rows = dw_add_publisher.Retrieve(Lpub)
  	IF ll_rows < 1 THEN
      rtn = MessageBox("Find Error", "Publisher does not exist. Insert new record?" ,Question!, OkCancel!, 1)
		IF rtn=1 THEN
     		dw_add_publisher.InsertRow(0)
			dw_add_publisher.object.pubabb[1]=Lpub
			dw_add_publisher.Object.pubname.TabSequence='10'
			dw_add_publisher.Object.changed.TabSequence='20'
			dw_add_publisher.Object.changed[1]=Today()
			w_add_publisher.cb_update.Enabled=TRUE
			w_add_publisher.cb_clear.Enabled=TRUE
			w_add_publisher.cb_find.Enabled=FALSE
		   dw_add_publisher.setfocus()
		ELSE
     		dw_add_publisher.InsertRow(0)
			dw_add_publisher.Object.DataWindow.QueryMode='Yes'
		   dw_add_publisher.setfocus()
		END IF
	ELSE
		dw_add_publisher.Object.pubabb.TabSequence='10'
		dw_add_publisher.Object.pubname.TabSequence='20'
		dw_add_publisher.Object.changed.TabSequence='30'
		w_add_publisher.cb_clear.Enabled=TRUE
		w_add_publisher.cb_update.Enabled=TRUE
		w_add_publisher.cb_find.Enabled=FALSE		
		dw_add_publisher.setfocus()
  	END IF
ELSE
   MessageBox("Error", "Please enter the publisher code." ,StopSign!, OK!, 2)
	dw_add_publisher.setfocus()
END IF

end event


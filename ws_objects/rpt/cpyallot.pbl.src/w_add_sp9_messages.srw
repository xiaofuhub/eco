$PBExportHeader$w_add_sp9_messages.srw
forward
global type w_add_sp9_messages from w_response
end type
type cb_clear from commandbutton within w_add_sp9_messages
end type
type pb_find from picturebutton within w_add_sp9_messages
end type
type cb_new from commandbutton within w_add_sp9_messages
end type
type dw_sp9_messages from u_pics_dw within w_add_sp9_messages
end type
type cb_exit from commandbutton within w_add_sp9_messages
end type
type cb_update from commandbutton within w_add_sp9_messages
end type
end forward

global type w_add_sp9_messages from w_response
integer x = 110
integer y = 136
integer width = 2482
integer height = 1336
string title = "Add Messages"
cb_clear cb_clear
pb_find pb_find
cb_new cb_new
dw_sp9_messages dw_sp9_messages
cb_exit cb_exit
cb_update cb_update
end type
global w_add_sp9_messages w_add_sp9_messages

forward prototypes
public function long wf_calc_max_msgid ()
end prototypes

public function long wf_calc_max_msgid ();long msgcnt

select count(*) into :msgcnt
from message
using sqlserveroracletrans;

return msgcnt+1
end function

on w_add_sp9_messages.create
int iCurrent
call super::create
this.cb_clear=create cb_clear
this.pb_find=create pb_find
this.cb_new=create cb_new
this.dw_sp9_messages=create dw_sp9_messages
this.cb_exit=create cb_exit
this.cb_update=create cb_update
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.cb_clear
this.Control[iCurrent+2]=this.pb_find
this.Control[iCurrent+3]=this.cb_new
this.Control[iCurrent+4]=this.dw_sp9_messages
this.Control[iCurrent+5]=this.cb_exit
this.Control[iCurrent+6]=this.cb_update
end on

on w_add_sp9_messages.destroy
call super::destroy
destroy(this.cb_clear)
destroy(this.pb_find)
destroy(this.cb_new)
destroy(this.dw_sp9_messages)
destroy(this.cb_exit)
destroy(this.cb_update)
end on

event open;call super::open;SqlServerOracleTrans.DBMS = "O73 ORACLE 7.3"
SqlServerOracleTrans.LogPass = "picadmin"
SqlServerOracleTrans.ServerName = "@TNS:ORAPICS"
SqlServerOracleTrans.LogId = "picadmin"
SqlServerOracleTrans.AutoCommit = False
SqlServerOracleTrans.DBParm = "PBCatalogOwner='PICADMIN'"
SqlServerOracleTrans.of_connect()
IF SqlServerOracleTrans.sqlcode <> 0 THEN
	IF SqlServerOracleTrans.SqlDbcode = -951 THEN            //check for invalid userid
	  MessageBox("Login Error","Invalid User ID/Password. Please re-enter.",StopSign!)
	  Return -1	
	ELSEIF SqlServerOracleTrans.SqlDBcode = -952 THEN       //check for invalid password
	  MessageBox("Login Error","Invalid User ID/Password. Please re-enter.",StopSign!)
	  Return -1
	Else                                             //check for other error messages
		MessageBox("Database Connection Error","Unable to Connect. " +& 
		string(SqlServerOracleTrans.sqldbcode) + " " +&
		SqlServerOracleTrans.SQLErrText, &
		StopSign!)
		Return -1
  END IF
ELSE
	dw_sp9_messages.of_SetTransObject(SQLServerOracleTrans)

	dw_sp9_messages.SetFocus()

END IF
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

type cb_clear from commandbutton within w_add_sp9_messages
integer x = 1472
integer y = 1092
integer width = 251
integer height = 116
integer taborder = 40
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "&Clear"
end type

event clicked;dw_sp9_messages.Reset()
dw_sp9_messages.InsertRow(0)

end event

type pb_find from picturebutton within w_add_sp9_messages
integer x = 1093
integer y = 1092
integer width = 283
integer height = 116
integer taborder = 30
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "&Find"
boolean originalsize = true
end type

event clicked;long ll_rows


// IF find is dispalyed on the push button
IF pb_find.BringToTop = FALSE THEN
	// Turn on query mode so user can specify data
	dw_sp9_messages.Object.DataWindow.QueryMode='Yes'
	// If Modify succeeds, show Execute,
	// Query mode is on and display sort CheckBox
	This.BringToTop = TRUE
	This.Text = "Ex&ecute"
	dw_sp9_messages.Object.Messageid.TabSequence='10'
	dw_sp9_messages.Object.createdby.TabSequence='0'
	dw_sp9_messages.Object.create_date.TabSequence='0'
	dw_sp9_messages.Object.start_date.TabSequence='0'
	dw_sp9_messages.Object.end_date.TabSequence='0'
	dw_sp9_messages.Object.messagettl.TabSequence='0'
	dw_sp9_messages.Object.messagetxt.TabSequence='0'	
	dw_sp9_messages.SetFocus()
ELSE
	dw_sp9_messages.AcceptText()
	// Turn off Query mode and retrieve data 
	// based on user's choices
	dw_sp9_messages.Object.DataWindow.QueryMode='No'
	// If Modify succeeds, show Find,
	// Query mode is off, and retrieve data
	This.BringToTop = FALSE
	This.Text = "F&ind"
	ll_rows = dw_sp9_messages.Retrieve()
	IF ll_rows > 0 THEN 
		// If any rows were retrieved.
		dw_sp9_messages.SetFocus()
	ELSE
		MessageBox("Error","No record found.")
	END IF
	dw_sp9_messages.Object.Messageid.TabSequence='0'
	dw_sp9_messages.Object.createdby.TabSequence='0'
	dw_sp9_messages.Object.create_date.TabSequence='0'
	dw_sp9_messages.Object.start_date.TabSequence='10'
	dw_sp9_messages.Object.end_date.TabSequence='20'
	dw_sp9_messages.Object.messagettl.TabSequence='30'
	dw_sp9_messages.Object.messagetxt.TabSequence='40'	
END IF // if pb_find...
end event

type cb_new from commandbutton within w_add_sp9_messages
integer x = 731
integer y = 1092
integer width = 261
integer height = 116
integer taborder = 20
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "&New"
end type

event clicked;int row_num
time lt_time=time('00:00:00')
datetime ldt_today

ldt_today=datetime(today(),lt_time)

dw_sp9_messages.event pfc_addrow()
row_num = dw_sp9_messages.RowCount()
dw_sp9_messages.ScrollToRow(row_num)
dw_sp9_messages.object.create_date[row_num]=ldt_today
dw_sp9_messages.object.messageid[row_num]=mid(string(year(Today())),3,2)+string(wf_calc_max_msgid())
dw_sp9_messages.object.createdby[row_num]=gnv_app.of_GetUserID()

dw_sp9_messages.SetFocus()
end event

type dw_sp9_messages from u_pics_dw within w_add_sp9_messages
event ue_enterkey pbm_dwnprocessenter
integer x = 41
integer y = 20
integer width = 2391
integer height = 1040
integer taborder = 20
string dataobject = "d_sp9_messages"
boolean vscrollbar = false
boolean livescroll = false
end type

event ue_enterkey;Send(Handle(this),256,9,Long(0,0))
return(1)

end event

event constructor;call super::constructor;// set the transaction object.
This.of_SetTransObject(SQLServerOracleTrans)
this.of_SetDropDownCalendar(TRUE)
this.iuo_calendar.of_Register("start_date",this.iuo_calendar.DDLB)
this.iuo_calendar.of_Register("end_date",this.iuo_calendar.DDLB)

end event

type cb_exit from commandbutton within w_add_sp9_messages
integer x = 2181
integer y = 1092
integer width = 247
integer height = 116
boolean bringtotop = true
integer textsize = -10
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "E&xit"
end type

event clicked;dw_sp9_messages.ResetUpdate()
IF NOT SQLServerOracleTrans.DBHandle() =0 THEN
	IF SQLServerOracleTrans.of_disconnect() < 0 THEN
		MessageBox("Error","Oracle Database Disconnect Error.",StopSign!)
	END IF
END IF
parent.Event pfc_close()

end event

type cb_update from commandbutton within w_add_sp9_messages
integer x = 1792
integer y = 1092
integer width = 311
integer height = 116
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
dw_sp9_messages.AcceptText()

// If there were no changes to the screen, don't try to update the screen.
IF dw_sp9_messages.ModifiedCount( ) > 0 THEN
	rc = dw_sp9_messages.Event pfc_update(TRUE, TRUE)
	if rc = 1 THEN
		COMMIT USING SQLServerOracleTrans;
		MessageBox("Update","Update Successful.",Information!)
		dw_sp9_messages.Resetupdate()
		return 1
	else
		MessageBox("ERROR","Update failed.",StopSign!)
		dw_sp9_messages.Resetupdate()
		RETURN 0
	end if
else // if Modifiedcount > 1
 	MessageBox("Update","There are no changes to the record.",Information!)
	RETURN 0
end if	
end event


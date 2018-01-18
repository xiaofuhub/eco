$PBExportHeader$w_add_org_producer.srw
$PBExportComments$Window to Add/Update Organization Producers
forward
global type w_add_org_producer from w_response
end type
type cb_exit from commandbutton within w_add_org_producer
end type
type cb_update from commandbutton within w_add_org_producer
end type
type dw_add_producer from u_pics_dw within w_add_org_producer
end type
type cb_find from commandbutton within w_add_org_producer
end type
type cb_clear from commandbutton within w_add_org_producer
end type
end forward

global type w_add_org_producer from w_response
integer x = 110
integer y = 136
integer width = 2912
integer height = 1708
string title = "Add Producer"
cb_exit cb_exit
cb_update cb_update
dw_add_producer dw_add_producer
cb_find cb_find
cb_clear cb_clear
end type
global w_add_org_producer w_add_org_producer

on w_add_org_producer.create
int iCurrent
call super::create
this.cb_exit=create cb_exit
this.cb_update=create cb_update
this.dw_add_producer=create dw_add_producer
this.cb_find=create cb_find
this.cb_clear=create cb_clear
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.cb_exit
this.Control[iCurrent+2]=this.cb_update
this.Control[iCurrent+3]=this.dw_add_producer
this.Control[iCurrent+4]=this.cb_find
this.Control[iCurrent+5]=this.cb_clear
end on

on w_add_org_producer.destroy
call super::destroy
destroy(this.cb_exit)
destroy(this.cb_update)
destroy(this.dw_add_producer)
destroy(this.cb_find)
destroy(this.cb_clear)
end on

event open;call super::open;dw_add_producer.Object.DataWindow.QueryMode='Yes'

dw_add_producer.SetFocus()

w_add_producer.cb_update.Enabled=FALSE
w_add_producer.cb_clear.Enabled=FALSE

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

type cb_exit from commandbutton within w_add_org_producer
integer x = 2610
integer y = 1472
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

event clicked;IF NOT SQLServerOracleTrans.DBHandle() =0 THEN
	IF SQLServerOracleTrans.of_disconnect() < 0 THEN
		MessageBox("Error","Oracle Database Disconnect Error.",StopSign!)
	END IF
END IF
ib_disableclosequery=TRUE
dw_add_producer.ResetUpdate()
parent.Event pfc_close()

end event

type cb_update from commandbutton within w_add_org_producer
integer x = 1938
integer y = 1472
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
dw_add_producer.AcceptText()

// If there were no changes to the screen, don't try to update the screen.
IF dw_add_producer.ModifiedCount( ) > 0 THEN
	rc = dw_add_producer.Event pfc_update(TRUE, TRUE)
	if rc = 1 THEN
		COMMIT USING SQLServerTrans;
//		IF IsValid(w_sheet_annual_contract_init)=TRUE THEN
//			DataWindowChild ldwc_pub
//			w_sheet_annual_contract_init.dw_contract_init.GetChild ("ancntr_prdr", ldwc_pub)
//			ldwc_pub.SetTransObject(sqlservertrans)
//			ldwc_pub.Retrieve()
//			w_sheet_annual_contract_init.dw_contract_init.object.ancntr_prdr[1]=dw_add_producer.object.prdr[1]
//			w_sheet_annual_contract_init.dw_contract_init.object.producer_prdr_name[1]=dw_add_producer.object.prdr_name[1]
//			w_sheet_annual_contract_init.dw_contract_init.object.producer_prdr_addr[1]=dw_add_producer.object.prdr_addr[1]
//			w_sheet_annual_contract_init.dw_contract_init.object.producer_prdr_city[1]=dw_add_producer.object.prdr_city[1]
//			w_sheet_annual_contract_init.dw_contract_init.object.producer_prdr_state[1]=dw_add_producer.object.prdr_state[1]
//			w_sheet_annual_contract_init.dw_contract_init.object.producer_prdr_zip[1]=dw_add_producer.object.prdr_zip[1]
//		END IF
		MessageBox("Update","Update Successful.",Information!)
		dw_add_producer.Resetupdate()
		return 1
	else
		MessageBox("ERROR","Update failed.",StopSign!)
		dw_add_producer.Resetupdate()
		RETURN 0
	end if
else // if Modifiedcount > 1
 	MessageBox("Update","There are no changes to the record.",Information!)
	RETURN 0
end if	
end event

type dw_add_producer from u_pics_dw within w_add_org_producer
event ue_enterkey pbm_dwnprocessenter
integer x = 32
integer y = 24
integer width = 2821
integer height = 1428
integer taborder = 10
string dataobject = "d_org_producer"
boolean vscrollbar = false
boolean livescroll = false
end type

event ue_enterkey;IF (dw_add_producer.GetColumn() <> 1 ) THEN
	Send(Handle(this),256,9,Long(0,0))
	return(1)
END IF

end event

event ue_postconstructor();call super::ue_postconstructor;// set the transaction object.
dw_add_producer.of_SetTransObject(SQLServeroracleTrans)

end event

type cb_find from commandbutton within w_add_org_producer
integer x = 1641
integer y = 1472
integer width = 247
integer height = 108
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
String Lprdr
Lprdr = dw_add_producer.GetText()
IF Lprdr <> "" THEN

	dw_add_producer.of_SetTransObject(SQLServerOracleTrans)
	dw_add_producer.Object.DataWindow.QueryMode='No'
	ll_rows = dw_add_producer.Retrieve(Lprdr)
	IF ll_rows = 0 THEN
		rtn = MessageBox("Find Error", "Producer does not exist. Insert new record?" ,Question!, OkCancel!, 1)
		IF rtn=1 THEN
			dw_add_producer.InsertRow(0)
			dw_add_producer.SetItem(dw_add_producer.GetRow(),"orgcd",Lprdr)
			dw_add_producer.SetItem(dw_add_producer.GetRow(),"orgtype","PRD")
			w_add_producer.cb_update.Enabled=TRUE
			w_add_producer.cb_clear.Enabled=TRUE
			w_add_producer.cb_find.Enabled=FALSE
		ELSE
			dw_add_producer.InsertRow(0)
			dw_add_producer.Object.DataWindow.QueryMode='Yes'
			dw_add_producer.SetItem(dw_add_producer.GetRow(),"orgcd",Lprdr)
		END IF
		dw_add_producer.setfocus()
	ELSE
		dw_add_producer.Object.orgcd.TabSequence='0'
		w_add_producer.cb_clear.Enabled=TRUE
		w_add_producer.cb_update.Enabled=TRUE
		w_add_producer.cb_find.Enabled=FALSE		
		dw_add_producer.setcolumn(2)
		dw_add_producer.setfocus()
	END IF
ELSE
   MessageBox("Producer code", "Please enter the producer code." ,StopSign!, OK!, 2)
	dw_add_producer.setfocus()
END IF

end event

type cb_clear from commandbutton within w_add_org_producer
integer x = 2299
integer y = 1472
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

//dw_add_producer.of_SetTransObject(SQLServerOracleTrans)
dw_add_producer.Reset()
ll_rows = dw_add_producer.InsertRow(0)
dw_add_producer.ScrolltoRow(ll_rows)

dw_add_producer.Object.DataWindow.QueryMode='Yes'

dw_add_producer.Object.orgcd.TabSequence='10'
//dw_add_producer.Object.prdr_name.TabSequence='0'
//dw_add_producer.Object.prdr_addr.TabSequence='0'
//dw_add_producer.Object.prdr_city.TabSequence='0'
//dw_add_producer.Object.prdr_state.TabSequence='0'
//dw_add_producer.Object.prdr_zip.TabSequence='0'
//dw_add_producer.Object.contact_name.TabSequence='0'
//dw_add_producer.Object.contact_phone.TabSequence='0'
//dw_add_producer.Object.contact_fax.TabSequence='0'
//dw_add_producer.Object.contact_email.TabSequence='0'

dw_add_producer.SetFocus()

w_add_producer.cb_find.Enabled=TRUE
w_add_producer.cb_find.Default=TRUE
w_add_producer.cb_update.Enabled=FALSE
w_add_producer.cb_clear.Enabled=FALSE

end event


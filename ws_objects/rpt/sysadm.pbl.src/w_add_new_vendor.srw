$PBExportHeader$w_add_new_vendor.srw
$PBExportComments$Window to Add New Commercial Vendor
forward
global type w_add_new_vendor from w_response
end type
type dw_vendor from u_pics_dw within w_add_new_vendor
end type
type cb_exit from commandbutton within w_add_new_vendor
end type
type cb_update from commandbutton within w_add_new_vendor
end type
type dw_add_comm_vendor from u_pics_dw within w_add_new_vendor
end type
type cb_find from commandbutton within w_add_new_vendor
end type
type cb_clear from commandbutton within w_add_new_vendor
end type
type gb_1 from groupbox within w_add_new_vendor
end type
end forward

global type w_add_new_vendor from w_response
integer x = 110
integer y = 136
integer width = 3433
integer height = 1532
string title = "Add New Commercial Audio Vendor"
boolean ib_alwaysvalidate = true
dw_vendor dw_vendor
cb_exit cb_exit
cb_update cb_update
dw_add_comm_vendor dw_add_comm_vendor
cb_find cb_find
cb_clear cb_clear
gb_1 gb_1
end type
global w_add_new_vendor w_add_new_vendor

type variables
Long il_vendor_row, il_org_row, il_seq
end variables

forward prototypes
public function integer of_validatevendor (string as_vendor)
end prototypes

public function integer of_validatevendor (string as_vendor);//////////////////////////////////////////////////////////////////////////////////////////////////////////
//
//	Function :  of_validatevendor
// Args		: string as_vendor
//	Description:
//	Validate vendor against the db to see if it exists, if exists return -1
//
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//	
//	Revision History
//
//	Developed by 	Date 				Version						Tracking#
//									
// Murali K.			01/24/2008      005 PICS Modifications	 Reqs: CDS.A.PTB2,ptb2.1
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//
long ll_count			
STRING LS_MESSAGE,  ls_msgparm[]

Select count(*)
into :ll_count
from vendabbr
where vendabbr = :as_vendor using SQLserverOracleTrans ;

IF SQLserverOracleTrans.sqlCode < 0 THEN
		ls_message = "A database error has occurred in select.~n" + &
						 "Database error code:  " + String (SqlServerTrans.sqlDbCode) + "~r~n" + &
						 "Database error message:~r~n" + SqlServerTrans.sqlErrText
		IF IsValid(gnv_app.inv_error) THEN
			ls_msgparm[1] = ls_message
			gnv_app.inv_error.of_Message ('pfc_dwdberror', ls_msgparm, &
			gnv_app.iapp_object.displayName)
		ELSE
			Messagebox (gnv_app.iapp_object.displayName, ls_message, stopSign!, ok!)
		END IF
		RETURN -1
END IF

IF ll_count > 0 THEN
	RETURN -1
END IF

RETURN 1


end function

on w_add_new_vendor.create
int iCurrent
call super::create
this.dw_vendor=create dw_vendor
this.cb_exit=create cb_exit
this.cb_update=create cb_update
this.dw_add_comm_vendor=create dw_add_comm_vendor
this.cb_find=create cb_find
this.cb_clear=create cb_clear
this.gb_1=create gb_1
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_vendor
this.Control[iCurrent+2]=this.cb_exit
this.Control[iCurrent+3]=this.cb_update
this.Control[iCurrent+4]=this.dw_add_comm_vendor
this.Control[iCurrent+5]=this.cb_find
this.Control[iCurrent+6]=this.cb_clear
this.Control[iCurrent+7]=this.gb_1
end on

on w_add_new_vendor.destroy
call super::destroy
destroy(this.dw_vendor)
destroy(this.cb_exit)
destroy(this.cb_update)
destroy(this.dw_add_comm_vendor)
destroy(this.cb_find)
destroy(this.cb_clear)
destroy(this.gb_1)
end on

event open;call super::open;//////////////////////////////////////////////////////////////////////////////////////////////////////////
//
//	Event:  open event
//
//	Description:
//	Set focus
//
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//	
//	Revision History
//
//	Developed by 	Date 				Version						Tracking#
//									
// Murali K.			01/24/2008      005 PICS Modifications	 Reqs: CDS.A.PTB2,ptb2.1
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//
dw_add_comm_vendor.SetFocus()


end event

event closequery;//////////////////////////////////////////////////////////////////////////////////////////////////////////
//
//	Event:  closequery event
//
//	Description:
//	Prompt to save updatable datawindows
//
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//	
//	Revision History
//
//	Developed by 	Date 				Version						Tracking#
//									
// Murali K.			01/24/2008      005 PICS Modifications	 Reqs: CDS.A.PTB2,ptb2.1
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//

Integer	li_pendingrc
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

event pfc_postopen;call super::pfc_postopen;//////////////////////////////////////////////////////////////////////////////////////////////////////////
//
//	Event: postopen event
//
//	Description:
//	Set focus, reset for save prompt
//
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//	
//	Revision History
//
//	Developed by 	Date 				Version						Tracking#
//									
// Murali K.			01/24/2008      005 PICS Modifications	 Reqs: CDS.A.PTB2,ptb2.1
//////////////////////////////////////////////////////////////////////////////////////////////////////////

dw_add_comm_vendor.Resetupdate()
dw_vendor.Resetupdate()
This.Center=TRUE
end event

type dw_vendor from u_pics_dw within w_add_new_vendor
event ue_postconstructor ( )
integer x = 41
integer y = 656
integer width = 3319
integer height = 584
integer taborder = 20
string dataobject = "d_add_new_commercial_vendor_vend"
boolean vscrollbar = false
boolean border = false
boolean livescroll = false
borderstyle borderstyle = stylebox!
end type

event ue_postconstructor;call super::ue_postconstructor;//////////////////////////////////////////////////////////////////////////////////////////////////////////
//
//	Event: ue_postconstructor
//
//	Description:
//	Set transaction object, insert a row and set default rows
//
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//	
//	Revision History
//
//	Developed by 	Date 				Version						Tracking#
//									
// Murali K.			01/24/2008      005 PICS Modifications	 Reqs: CDS.A.PTB2,ptb2.1
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//

//Set transobject 
This.of_SetTransObject(SQLserverOracleTrans)
il_vendor_row = This.Event pfc_insertrow()

end event

event pfc_preupdate;call super::pfc_preupdate;//////////////////////////////////////////////////////////////////////////////////////////////////////////
//
//	Event: pfc_preupdate
//
//	Description:
//	Set column default values before updates
//
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//	
//	Revision History
//
//	Developed by 	Date 				Version						Tracking#
//									
// Murali K.			01/24/2008      005 PICS Modifications	 Reqs: CDS.A.PTB2,ptb2.1
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//

long ll_rc
string ls_vendor

ll_rc = This.Rowcount()
IF ll_rc > 0 THEN
	ls_vendor = dw_add_comm_vendor.object.vcd[1]
	This.object.vendabbr_vendabbr[1] = ls_vendor // Commercial Audio Vendor
	This.object.vendabbr_vendcd[1] =string(il_seq) // vend cd is same as orgcd system generated ??
END IF
RETURN 1

RETURN 1
end event

event pfc_postinsertrow;call super::pfc_postinsertrow;//////////////////////////////////////////////////////////////////////////////////////////////////////////
//
//	Event: pfc_postinsertrow
//
//	Description:
//	Set default column values after insert
//
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//	
//	Revision History
//
//	Developed by 	Date 				Version						Tracking#
//									
// Murali K.			01/24/2008      005 PICS Modifications	 Reqs: CDS.A.PTB2,ptb2.1
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//
IF il_vendor_row > 0 THEN
	This.object.vendabbr_created_by[il_vendor_row] = gnv_app.of_getuserid()
	This.object.vendabbr_created_date[il_vendor_row] =Today()
END IF
end event

type cb_exit from commandbutton within w_add_new_vendor
integer x = 3141
integer y = 1284
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

event clicked;//////////////////////////////////////////////////////////////////////////////////////////////////////////
//
//	Event:  clicked for exit  button
//
//	Description:
//	exit screen
//
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//	
//	Revision History
//
//	Developed by 	Date 				Version						Tracking#
//									
// Murali K.			01/24/2008      005 PICS Modifications	 Reqs: CDS.A.PTB2,ptb2.1
// Murali K.			01/31/2008		If no changes do not prompt to save 
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//
dw_add_comm_vendor.ResetUpdate()
dw_vendor.ResetUpdate()
parent.Event pfc_close()

end event

type cb_update from commandbutton within w_add_new_vendor
integer x = 2441
integer y = 1284
integer width = 311
integer height = 108
integer taborder = 30
boolean bringtotop = true
integer textsize = -10
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "&Add"
end type

event clicked;//////////////////////////////////////////////////////////////////////////////////////////////////////////
//
//	Event:  clicked for update button
//
//	Description:
//	Save updatable datawindows. Add a new commercial vendor into the system
//
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//	
//	Revision History
//
//	Developed by 	Date 				Version						Tracking#
//									
// Murali K.			01/24/2008      005 PICS Modifications	 Reqs: CDS.A.PTB2,ptb2.1
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//

int li_rc
string ls_vendor, ls_name

// Accept the text that was put on the screen.
dw_add_comm_vendor.AcceptText()

li_rc = dw_add_comm_vendor.Event pfc_preupdate()
IF li_rc = -1 THEN
	RETURN 0
END IF

// If there were no changes to the screen, don't try to update the screen.
IF dw_add_comm_vendor.ModifiedCount( ) > 0 THEN
	li_rc = dw_add_comm_vendor.Event pfc_update(TRUE, TRUE)
	if li_rc = 1 THEN
		COMMIT USING SQLServeroracletrans;
		
		// Need to set the orgcd before from org before update do it in preupdate
		// Accept the text that was put on the screen.
		dw_vendor.AcceptText()
		li_rc = dw_vendor.Event pfc_update(TRUE, TRUE)
		IF li_rc = 1 THEN		
			COMMIT USING SQLServeroracletrans;
			ls_vendor = dw_add_comm_vendor.object.vcd[1]
			ls_name = dw_add_comm_vendor.object.org_name[1]
			MessageBox("Update","New Commercial Audio Vendor " + ls_name + '(' + ls_vendor + ")  has been added",Information!)
			dw_add_comm_vendor.Resetupdate()
			dw_vendor.Resetupdate()
			dw_add_comm_vendor.Reset()
			dw_vendor.Reset()
			dw_add_comm_vendor.event pfc_insertrow()
			dw_vendor.event pfc_insertrow()
			dw_add_comm_vendor.Resetupdate()
			dw_vendor.Resetupdate()
			dw_add_comm_vendor.Setfocus()
			dw_add_comm_vendor.setcolumn('vcd')
			Return 1
		END IF
	else
		MessageBox("ERROR","Update failed.",StopSign!)
		dw_add_comm_vendor.Resetupdate()
		dw_vendor.Resetupdate()
		RETURN 0
	end if
else // if Modifiedcount > 1
 	MessageBox("Update","There are no changes to the record.",Information!)
	RETURN 0
end if	
end event

type dw_add_comm_vendor from u_pics_dw within w_add_new_vendor
event ue_enterkey pbm_dwnprocessenter
integer x = 37
integer y = 64
integer width = 3319
integer height = 584
integer taborder = 10
string dataobject = "d_add_new_commercial_vendor_org"
boolean vscrollbar = false
boolean border = false
boolean livescroll = false
borderstyle borderstyle = stylebox!
end type

event ue_enterkey;//////////////////////////////////////////////////////////////////////////////////////////////////////////
//
//	Event:  enter key for dw
//
//	Description:
//	Simulate tab
//
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//	
//	Revision History
//
//	Developed by 	Date 				Version						Tracking#
//									
// Murali K.			01/24/2008      005 PICS Modifications	 Reqs: CDS.A.PTB2,ptb2.1
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//
IF (dw_add_comm_vendor.GetColumn() <> 1 ) THEN
	Send(Handle(this),256,9,Long(0,0))
	return(1)
END IF

end event

event ue_postconstructor;call super::ue_postconstructor;//////////////////////////////////////////////////////////////////////////////////////////////////////////
//
//	Event: ue_postconstructor
//
//	Description:
//	Set transaction object, insert a row and set default rows
//
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//	
//	Revision History
//
//	Developed by 	Date 				Version						Tracking#
//									
// Murali K.			01/24/2008      005 PICS Modifications	 Reqs: CDS.A.PTB2,ptb2.1
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//

//Set transobject 
This.of_SetTransObject(SQLserverOracleTrans)
il_org_row = This.Event pfc_insertrow()

end event

event pfc_preupdate;call super::pfc_preupdate;//////////////////////////////////////////////////////////////////////////////////////////////////////////
//
//	Event: pfc_preupdate
//
//	Description:
//	Set column default values before updates
//
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//	
//	Revision History
//
//	Developed by 	Date 				Version						Tracking#
//									
// Murali K.			01/24/2008      005 PICS Modifications	 Reqs: CDS.A.PTB2,ptb2.1
//						01/31/2008										Check for isnull conditions
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//
long ll_rc
string ls_message, ls_msgparm[], ls_vendor, ls_name

//Validate mandatory fields
ls_vendor = dw_add_comm_vendor.object.vcd[1]
ls_name = dw_add_comm_vendor.object.org_name[1]
IF isnull(ls_vendor) OR Len(Trim(ls_vendor))  = 0 THEN
	Messagebox('Error','Please enter a vendor code')
	dw_add_comm_vendor.Setfocus()
	this.setcolumn('vcd')
	RETURN -1
END IF
// 01/31/2008 check for isnull
IF isnull(ls_name) OR Len(Trim(ls_name))  = 0 THEN
	Messagebox('Error','Please enter a vendor name')
	dw_add_comm_vendor.Setfocus()
	this.setcolumn('org_name')
	RETURN -1
END IF

ll_rc = This.Rowcount()
IF ll_rc > 0 THEN
	
	//select the next primary key sequence number
	SELECT ORG_SEQ.NEXTVAL 
	INTO :il_seq 
	FROM DUAL using SQLserverOracleTrans;
	
	IF SQLserverOracleTrans.sqlCode < 0 THEN
		ls_message = "A database error has occurred in select.~n" + &
						 "Database error code:  " + String (SqlServerTrans.sqlDbCode) + "~r~n" + &
						 "Database error message:~r~n" + SqlServerTrans.sqlErrText
		IF IsValid(gnv_app.inv_error) THEN
			ls_msgparm[1] = ls_message
			gnv_app.inv_error.of_Message ('pfc_dwdberror', ls_msgparm, &
			gnv_app.iapp_object.displayName)
		ELSE
			Messagebox (gnv_app.iapp_object.displayName, ls_message, stopSign!, ok!)
		END IF
			RETURN -1
	END IF

	This.object.org_orgcd[1] = string(il_seq) // system generated
	This.object.org_orgtype[1] = 'CAV' // Commercial Audio Vendor
	This.object.active_status_code[1] = 'A' // Active
	This.object.org_created_by[1] = gnv_app.of_getuserid()
	This.object.org_created_date[1] =Today()
	dw_vendor.object.vendabbr_created_by[1] = gnv_app.of_getuserid()
	dw_vendor.object.vendabbr_created_date[1] =Today()
	dw_vendor.object.vendabbr_modified_by[1] = gnv_app.of_getuserid()
	dw_vendor.object.vendabbr_modified_date[1] =Today()
	
END IF
RETURN 1
end event

event itemchanged;call super::itemchanged;
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//
//	Event: ITEMCHANGED
//
//	Description:
//	Validate vendor code 
//
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//	
//	Revision History
//
//	Developed by 	Date 				Version						Tracking#
//									
// Murali K.			01/24/2008      005 PICS Modifications	 Reqs: CDS.A.PTB2,ptb2.1
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//

string ls_vendcode
long ll_count

CHOOSE CASE dwo.name
		
	CASE 'vcd'
			IF of_validatevendor(data) = -1 THEN
				This.Object.vcd.validationmsg =  "Vendor already exist, a new vendor code should be chosen. "
				RETURN 1
			ELSE
				cb_update.enabled=TRUE
			END IF
				
END CHOOSE

RETURN 0
end event

type cb_find from commandbutton within w_add_new_vendor
integer x = 2126
integer y = 1284
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

event clicked;//////////////////////////////////////////////////////////////////////////////////////////////////////////
//
//	Event:  clicked for find button
//
//	Description:
//	Find the vendor, if doesn't exist add new
//
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//	
//	Revision History
//
//	Developed by 	Date 				Version						Tracking#
//									
// Murali K.			01/24/2008      005 PICS Modifications	 Reqs: CDS.A.PTB2,ptb2.1
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//

int ll_rows,rtn
String Lprdr
long ll_rc

Lprdr = dw_add_comm_vendor.GetText()
IF Lprdr <> "" THEN
	dw_vendor.Object.DataWindow.QueryMode='No'
	ll_rows = dw_vendor.Retrieve(Lprdr) // vendor code
  	IF ll_rows = 0 THEN
   	   rtn = MessageBox("Find Error", "Commercial Audio Vendor does not exist. Add new record?" ,Question!, OkCancel!, 1)
		IF rtn=1 THEN
			Parent.cb_update.Enabled=TRUE
			Parent.cb_clear.Enabled=TRUE
//			Parent.cb_find.Enabled=FALSE
			dw_add_comm_vendor.reset()
			dw_vendor.reset()
			dw_add_comm_vendor.event pfc_insertrow()
			dw_vendor.event pfc_insertrow()
			dw_add_comm_vendor.SetItem(dw_add_comm_vendor.GetRow(),"vcd",Lprdr)
		ELSE
     		dw_add_comm_vendor.InsertRow(0)
			dw_add_comm_vendor.Object.DataWindow.QueryMode='Yes'
			dw_add_comm_vendor.SetItem(dw_add_comm_vendor.GetRow(),"vcd",Lprdr)
			dw_add_comm_vendor.setfocus()
		END IF
	   dw_add_comm_vendor.setfocus()
	ELSE
		MessageBox("Find Error", "Commercial Audio Vendor already exist. Please enter a new vendor code." ,Question!, OkCancel!, 1)
//			Parent.cb_update.Enabled=FALSE
			Parent.cb_clear.Enabled=TRUE
			Parent.cb_find.Enabled=TRUE
			dw_add_comm_vendor.reset()
			dw_vendor.reset()
			dw_add_comm_vendor.event pfc_insertrow()
			dw_vendor.event pfc_insertrow()
			dw_add_comm_vendor.setfocus()
  	END IF
ELSE
//	 MessageBox("Database Error", "Please enter the vendor code." ,StopSign!, OK!, 2)
	dw_add_comm_vendor.setfocus()
//	dw_add_comm_vendor.setcolumn('vcd')
END IF


end event

type cb_clear from commandbutton within w_add_new_vendor
integer x = 2821
integer y = 1284
integer width = 251
integer height = 108
integer taborder = 40
boolean bringtotop = true
integer textsize = -10
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "&Clear"
end type

event clicked;//////////////////////////////////////////////////////////////////////////////////////////////////////////
//
//	Event:  clicked for clear button
//
//	Description:
//	Clear datawindows and set the focus to the vendor code
//
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//	
//	Revision History
//
//	Developed by 	Date 				Version						Tracking#
//									
// Murali K.			01/24/2008      005 PICS Modifications	 Reqs: CDS.A.PTB2,ptb2.1
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//

dw_add_comm_vendor.reset()
dw_vendor.reset()
dw_add_comm_vendor.event pfc_insertrow()
dw_vendor.event pfc_insertrow()
dw_add_comm_vendor.setfocus()
end event

type gb_1 from groupbox within w_add_new_vendor
integer x = 18
integer y = 12
integer width = 3365
integer height = 1260
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 33554432
long backcolor = 67108864
end type


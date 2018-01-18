$PBExportHeader$w_qa_administrator_assignment_view.srw
$PBExportComments$Reviewer Assignment - Reviewer View
forward
global type w_qa_administrator_assignment_view from w_response
end type
type cb_refresh from commandbutton within w_qa_administrator_assignment_view
end type
type st_1 from statictext within w_qa_administrator_assignment_view
end type
type dw_assignment from u_pics_dw within w_qa_administrator_assignment_view
end type
type cb_exit from commandbutton within w_qa_administrator_assignment_view
end type
type cb_update from commandbutton within w_qa_administrator_assignment_view
end type
type cb_clear from commandbutton within w_qa_administrator_assignment_view
end type
end forward

global type w_qa_administrator_assignment_view from w_response
integer x = 110
integer y = 136
integer width = 5010
integer height = 2760
string title = "Reviewer Assignment -Administrator View"
boolean minbox = true
boolean maxbox = true
boolean resizable = true
windowtype windowtype = main!
windowstate windowstate = maximized!
boolean ib_isupdateable = false
boolean ib_alwaysvalidate = true
cb_refresh cb_refresh
st_1 st_1
dw_assignment dw_assignment
cb_exit cb_exit
cb_update cb_update
cb_clear cb_clear
end type
global w_qa_administrator_assignment_view w_qa_administrator_assignment_view

type prototypes
FUNCTION long GetLogicalDrives() LIBRARY "KERNEL32.DLL"
FUNCTION long GetLogicalDriveStringsW(ulong lbuflen, ref string sbuffer) LIBRARY "KERNEL32.DLL"
end prototypes

type variables
Long il_bkseq
string is_bkmed
n_cst_filesrv inv_filesrv
n_qa_services inv_qa
end variables

forward prototypes
public function integer of_validatevendor (string as_vendor)
public function integer of_resetdws ()
public function integer of_retrievebook (long al_book)
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

public function integer of_resetdws ();//////////////////////////////////////////////////////////////////////////////////////////////////////////
//
//	Event:  of_resetdws
//
//	Description:
//	Reset the DW
//
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//	
//	Revision History
//
//	Developed by 	Date 				Version						Tracking#
//									
// Murali K.			02/13/2008      005 PICS Modifications	 Reqs: QAS A.6.2 - 6.4
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//
dw_assignment.reset()
cb_update.Enabled=FALSE
RETURN 1
end function

public function integer of_retrievebook (long al_book);//////////////////////////////////////////////////////////////////////////////////////////////////////////
//
//	Event: of_retrievevendor
//  Args: number al_book
//	Description:
//	Retrieve book  info selected or entered from dropdown
//
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//	
//	Revision History
//
//	Developed by 	Date 				Version						Tracking#
//									
// Murali K.			02/13/2008      005 PICS Modifications	 Reqs: QAS 6.2
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//
long ll_rc

ll_rc = dw_assignment.Retrieve()
IF ll_rc < 1 THEN
	dw_assignment.Event pfc_insertrow()
	Messagebox('Error', 'No review assignment records found')
END IF		
dw_assignment.setfocus()
RETURN 1
end function

on w_qa_administrator_assignment_view.create
int iCurrent
call super::create
this.cb_refresh=create cb_refresh
this.st_1=create st_1
this.dw_assignment=create dw_assignment
this.cb_exit=create cb_exit
this.cb_update=create cb_update
this.cb_clear=create cb_clear
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.cb_refresh
this.Control[iCurrent+2]=this.st_1
this.Control[iCurrent+3]=this.dw_assignment
this.Control[iCurrent+4]=this.cb_exit
this.Control[iCurrent+5]=this.cb_update
this.Control[iCurrent+6]=this.cb_clear
end on

on w_qa_administrator_assignment_view.destroy
call super::destroy
destroy(this.cb_refresh)
destroy(this.st_1)
destroy(this.dw_assignment)
destroy(this.cb_exit)
destroy(this.cb_update)
destroy(this.cb_clear)
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
// Murali K.			02/13/2008      005 PICS Modifications	 Reqs: QAS 6.2
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//
string ls_user

dw_assignment.SetFocus()


ls_user = gnv_app.of_getuserid()
IF ( inv_qa.of_qaauthority(ls_user) = 'R') OR &
	Len(Trim( inv_qa.of_qaauthority(ls_user))) = 0 THEN
	Messagebox('Error','Access Denied.')
	close(this)
END IF


end event

event closequery;call super::closequery;//////////////////////////////////////////////////////////////////////////////////////////////////////////
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
// Murali K.			01/24/2008      005 PICS Modifications	 Reqs: QAS 6.2
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

type cb_refresh from commandbutton within w_qa_administrator_assignment_view
integer x = 3721
integer y = 2340
integer width = 247
integer height = 84
integer taborder = 40
boolean bringtotop = true
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&Refresh"
end type

event clicked;//////////////////////////////////////////////////////////////////////////////////////////////////////////
//
//	Event: clicked for cb_refresh
//
//	Description:
//	Refresh the list after any QC5 approvals
//
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//	
//	Revision History
//
//	Developed by 	Date 				Version						Tracking#
//									
// Murali K.			07/18/2008   	Phase - 2
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//
dw_assignment.Retrieve()
end event

type st_1 from statictext within w_qa_administrator_assignment_view
integer x = 73
integer y = 2336
integer width = 1170
integer height = 156
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "A default number of books will be displayed Default can be modified by QAS"
boolean focusrectangle = false
end type

type dw_assignment from u_pics_dw within w_qa_administrator_assignment_view
event ue_postconstructor ( )
integer x = 32
integer y = 40
integer width = 4571
integer height = 2272
integer taborder = 40
string dataobject = "d_qa_admin_assignment"
boolean hscrollbar = true
boolean livescroll = false
borderstyle borderstyle = stylebox!
boolean ib_rmbmenu = false
string is_updatesallowed = "I"
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
This.of_SetTransObject(SQLserverTrans)
This.Retrieve()
end event

event retrievestart;call super::retrievestart;//////////////////////////////////////////////////////////////////////////////////////////////////////////
//
//	Event: ue_postconstructor
//
//	Description:
//	Indicate retrieve is happening
//
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//	
//	Revision History
//
//	Developed by 	Date 				Version						Tracking#
//									
// Murali K.			02/13/2008      005 PICS Modifications	 Reqs: QAS 6.2
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//
openwithparm(w_pics_retrieve_msg_box,"Retrieving data , Please Wait...")
end event

event retrieveend;call super::retrieveend;//////////////////////////////////////////////////////////////////////////////////////////////////////////
//
//	Event: ue_postconstructor
//
//	Description:
//	close retrieve indicator
//
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//	
//	Revision History
//
//	Developed by 	Date 				Version						Tracking#
//									
// Murali K.			02/13/2008      005 PICS Modifications	 Reqs: QAS 6.2
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//
close(w_pics_retrieve_msg_box)
This.Setsort('sort_order asc')
This.Sort()
//12/03/2008
this.Object.curr_reviewer.dddw.vscrollbar='yes'
end event

event sqlpreview;call super::sqlpreview;//messagebox('sql', sqlsyntax)
end event

type cb_exit from commandbutton within w_qa_administrator_assignment_view
integer x = 4352
integer y = 2340
integer width = 247
integer height = 84
integer taborder = 30
boolean bringtotop = true
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Exit"
boolean cancel = true
end type

event clicked;//////////////////////////////////////////////////////////////////////////////////////////////////////////
//
//	Event:  clicked for exit button
//
//	Description:
//	exit prompt for save if any changes
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
parent.Event pfc_close()

end event

type cb_update from commandbutton within w_qa_administrator_assignment_view
integer x = 3406
integer y = 2340
integer width = 247
integer height = 84
integer taborder = 10
boolean bringtotop = true
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&Update"
end type

event clicked;//////////////////////////////////////////////////////////////////////////////////////////////////////////
//
//	Event:  clicked for Assign Selected Books button
//
//	Description:
//	Update qa reviewer assignment tables
//
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//	
//	Revision History
//
//	Developed by 	Date 				Version						Tracking#
//									
// Murali K.			02/13/2008      005 PICS Modifications	 Reqs: QAS 6.2
// Murali K.			07/23/2008    new view vw_qa_admin_assign used, changed update code
// Murali K.			10/06/2008		date assigned must be assigned by batch program
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//

int li_rc
long 				ll_rc, ll_seq, ll_bkseq
string 			ls_user, ls_check_box, ls_bkmed, ls_reviewer
int 				li_loop
dwItemStatus 	l_status

dw_assignment.accepttext()
// If there were no info on the the screen, don't try to update the screen.
IF dw_assignment.ModifiedCount( ) > 0 THEN
		ll_rc = dw_assignment.Rowcount()
		
		IF ll_rc > 0 THEN
		
			FOR li_loop = 1 	TO ll_rc
			
					l_status = dw_assignment.GetItemStatus(li_loop,0, Primary!)
				  
					  IF l_status <> notmodified! THEN
					
						// insert a record in qa reviewer assignment table
						select max(assignment_no)
						into :ll_seq
						from qa_reviewer_assignment using sqlservertrans;

						IF Isnull(ll_seq) THEN
							ll_seq = 1
						ELSE
							ll_seq++;
						END IF
				
						ls_user = gnv_app.of_getuserid()
						ll_bkseq = dw_assignment.object.bkseq1[li_loop]
						ls_bkmed = 'DB' 
						ls_reviewer = dw_assignment.object.curr_reviewer[li_loop]
						
						// Terminate old assignments only one active assignment can exist					
						UPDATE QA_REVIEWER_ASSIGNMENT
						SET ACTIVE_STATUS_CODE = 'T',
							 MODIFIED_BY = :ls_user,
							 MODIFIED_DATE = sysdate
						WHERE BKSEQ = :ll_bkseq and bkmed = :ls_bkmed and active_status_code = 'A' using sqlservertrans ;
		
							// insert new active assignment
						  INSERT INTO "QA_REVIEWER_ASSIGNMENT"  
										( "ASSIGNMENT_NO",   
										  "BKMED",   
										  "BKSEQ",   
										  "ASSIGNED_TO",   
										  "ASSIGNED_BY",   
										  "DATE_ASSIGNED",   
										  "ACTIVE_STATUS_CODE",   
										  "CREATED_BY",   
										  "CREATED_DATE",   
										  "MODIFIED_BY",   
										  "MODIFIED_DATE" )  
						  VALUES (:ll_seq,   
									  'DB',   
									  :ll_bkseq,   
									  :ls_reviewer,   
									  :ls_user,   
									sysdate,   
									 'A',   
									 :ls_user,   
									  sysdate,   
									  :ls_user,   
									  sysdate )  using sqlservertrans ;
			
				END IF // notmodified check
			NEXT
				COMMIT USING SQLServertrans;
				MessageBox("Update","Records Updated/Inserted into Assignment table.",Information! )
				cb_refresh.Triggerevent('clicked')
		END IF
ELSE
		MessageBox("ERROR","No records found to Insert.",StopSign!)
		RETURN 0
END IF // modified count check
end event

type cb_clear from commandbutton within w_qa_administrator_assignment_view
integer x = 4037
integer y = 2340
integer width = 247
integer height = 84
integer taborder = 20
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
//	Clear datawindow values
//
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//	
//	Revision History
//
//	Developed by 	Date 				Version						Tracking#
//									
// Murali K.			02/13/2008      005 PICS Modifications	 Reqs: QAS 6.2
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//
long ll_rc
int li_loop
string ls_check_box, ls_value
dwitemstatus l_status

ll_rc = dw_assignment.Rowcount()
IF ll_rc > 0 THEN
	FOR li_loop = 1 	TO ll_rc
		l_status = dw_assignment.GetItemStatus(li_loop, "curr_reviewer", Primary!)
		ls_value = dw_assignment.GetItemString(li_loop,"curr_reviewer",Primary!,TRUE)
		IF l_status = DataModified! THEN
			dw_assignment.object.check_box[li_loop] = 'N'
			IF Len(Trim(ls_value )) > 0 THEN
				dw_assignment.object.curr_reviewer[li_loop] = LS_VALUE
			ELSE
				dw_assignment.object.curr_reviewer[li_loop] = ' '
			END IF
			dw_assignment.SetItemStatus(li_loop, 0,  Primary!, NotModified!)
		END IF
	NEXT
END IF
end event


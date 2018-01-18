$PBExportHeader$w_qa_prob_books_move_to_quarantine.srw
$PBExportComments$Window to Edit/Terminate Commercial Vendor
forward
global type w_qa_prob_books_move_to_quarantine from w_response
end type
type tab_1 from tab within w_qa_prob_books_move_to_quarantine
end type
type tabpage_pending from userobject within tab_1
end type
type dw_1 from u_pics_dw within tabpage_pending
end type
type gb_1 from groupbox within tabpage_pending
end type
type tabpage_pending from userobject within tab_1
dw_1 dw_1
gb_1 gb_1
end type
type tabpage_moved from userobject within tab_1
end type
type dw_2 from u_pics_dw within tabpage_moved
end type
type tabpage_moved from userobject within tab_1
dw_2 dw_2
end type
type tab_1 from tab within w_qa_prob_books_move_to_quarantine
tabpage_pending tabpage_pending
tabpage_moved tabpage_moved
end type
type cb_fetch from commandbutton within w_qa_prob_books_move_to_quarantine
end type
type dw_lookup from u_pics_dw within w_qa_prob_books_move_to_quarantine
end type
type cb_exit from commandbutton within w_qa_prob_books_move_to_quarantine
end type
type cb_update from commandbutton within w_qa_prob_books_move_to_quarantine
end type
type dw_header from u_pics_dw within w_qa_prob_books_move_to_quarantine
end type
type cb_clear from commandbutton within w_qa_prob_books_move_to_quarantine
end type
end forward

global type w_qa_prob_books_move_to_quarantine from w_response
integer x = 110
integer y = 136
integer width = 5010
integer height = 2760
string title = "Problem Books - Move to Quarantine"
boolean minbox = true
boolean maxbox = true
boolean resizable = true
windowtype windowtype = main!
windowstate windowstate = maximized!
boolean ib_isupdateable = false
boolean ib_alwaysvalidate = true
tab_1 tab_1
cb_fetch cb_fetch
dw_lookup dw_lookup
cb_exit cb_exit
cb_update cb_update
dw_header dw_header
cb_clear cb_clear
end type
global w_qa_prob_books_move_to_quarantine w_qa_prob_books_move_to_quarantine

type prototypes
FUNCTION long GetLogicalDrives() LIBRARY "KERNEL32.DLL"
FUNCTION long GetLogicalDriveStringsW(ulong lbuflen, ref string sbuffer) LIBRARY "KERNEL32.DLL"
end prototypes

type variables
Long il_bkseq
string is_bkmed
n_cst_filesrv inv_filesrv

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
//	Reset the dropdown with active vendors 
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
DataWindowChild ldwc
integer li_rtncode

dw_header.reset()
tab_1.tabpage_pending.dw_1.reset()
dw_lookup.reset()
dw_lookup.Event pfc_insertrow()
li_rtncode = dw_lookup.GetChild('vendor_code', ldwc)
IF li_rtncode = -1 THEN MessageBox("Error", "Error Accessing Select Vendor Dropdown")
ldwc.SetTransObject(SQLserverOracleTrans)
ldwc.Retrieve()
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
// Murali K.			02/11/2008      005 PICS Modifications	 Reqs: QAS 2.2.1
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//
long ll_rc

This.setredraw(FALSE)
dw_header.reset()
ll_rc = dw_header.Retrieve(al_book)
IF ll_rc > 0 THEN
	is_bkmed = dw_header.object.mchar_bkmed[ll_rc]
	il_bkseq	= dw_header.object.bkseq[ll_rc]
	tab_1.tabpage_pending.dw_1.reset()
	ll_rc = tab_1.tabpage_pending.dw_1.Retrieve(al_book, 'DB')
	IF ll_rc < 1 THEN
		Messagebox('Error', 'Autotest Rejection History information not found')
	ELSE
	END IF		
	tab_1.tabpage_pending.dw_1.setfocus()
ELSE
	dw_header.Event pfc_insertrow()
	Messagebox('Error', 'QA Header Info not found, Please check if a producer is assigned.')
	cb_update.enabled=FALSE
	tab_1.tabpage_pending.dw_1.reset()
END IF
This.setredraw(TRUE)
RETURN 1
end function

on w_qa_prob_books_move_to_quarantine.create
int iCurrent
call super::create
this.tab_1=create tab_1
this.cb_fetch=create cb_fetch
this.dw_lookup=create dw_lookup
this.cb_exit=create cb_exit
this.cb_update=create cb_update
this.dw_header=create dw_header
this.cb_clear=create cb_clear
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.tab_1
this.Control[iCurrent+2]=this.cb_fetch
this.Control[iCurrent+3]=this.dw_lookup
this.Control[iCurrent+4]=this.cb_exit
this.Control[iCurrent+5]=this.cb_update
this.Control[iCurrent+6]=this.dw_header
this.Control[iCurrent+7]=this.cb_clear
end on

on w_qa_prob_books_move_to_quarantine.destroy
call super::destroy
destroy(this.tab_1)
destroy(this.cb_fetch)
destroy(this.dw_lookup)
destroy(this.cb_exit)
destroy(this.cb_update)
destroy(this.dw_header)
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
// Murali K.			01/24/2008      005 PICS Modifications	 Reqs: CDS.A.PTB2,ptb2.1
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//
dw_lookup.SetFocus()


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

type tab_1 from tab within w_qa_prob_books_move_to_quarantine
integer x = 55
integer y = 348
integer width = 4187
integer height = 2008
integer taborder = 40
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 79741120
boolean raggedright = true
boolean focusonbuttondown = true
boolean boldselectedtext = true
integer selectedtab = 1
tabpage_pending tabpage_pending
tabpage_moved tabpage_moved
end type

on tab_1.create
this.tabpage_pending=create tabpage_pending
this.tabpage_moved=create tabpage_moved
this.Control[]={this.tabpage_pending,&
this.tabpage_moved}
end on

on tab_1.destroy
destroy(this.tabpage_pending)
destroy(this.tabpage_moved)
end on

event selectionchanged;//////////////////////////////////////////////////////////////////////////////////////////////////////////
//
//	Event: selectionchanged
//
//	Description:
//	Retrieve moved to quarantine rows
//
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//	
//	Revision History
//
//	Developed by 	Date 				Version						Tracking#
//									
// Murali K.			08/25/2008		1.0
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//
int li_ret
datawindowchild ldwc

IF newindex = 2 THEN
	tab_1.tabpage_moved.dw_2.retrieve()
	// Books might have moved, refresh book # dropdown
	li_ret = dw_lookup.GetChild('bkseq',ldwc)
	IF li_ret > 0 THEN
		ldwc.settransobject(sqlservertrans)
		ldwc.Retrieve()
	END IF
END IF

end event

type tabpage_pending from userobject within tab_1
integer x = 18
integer y = 112
integer width = 4151
integer height = 1880
long backcolor = 79741120
string text = "Not Moved/Pending"
long tabtextcolor = 33554432
long tabbackcolor = 79741120
long picturemaskcolor = 536870912
dw_1 dw_1
gb_1 gb_1
end type

on tabpage_pending.create
this.dw_1=create dw_1
this.gb_1=create gb_1
this.Control[]={this.dw_1,&
this.gb_1}
end on

on tabpage_pending.destroy
destroy(this.dw_1)
destroy(this.gb_1)
end on

type dw_1 from u_pics_dw within tabpage_pending
integer x = 23
integer y = 60
integer width = 4069
integer height = 1808
integer taborder = 20
string dataobject = "d_autotest_rejection_history"
end type

event clicked;call super::clicked;//////////////////////////////////////////////////////////////////////////////////////////////////////////
//
//	Event:  clicked for Move to Quarantine  button
//
//	Description:
//	Update qa tables
//
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//	
//	Revision History
//
//	Developed by 	Date 				Version						Tracking#
//									
// Murali K.			02/11/2008      005 PICS Modifications	 Reqs: QAS a.2.1.
// Murali K.			07/08/08			Open a new screen with results from autotest_files table
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//
long ll_test_no

IF dwo.name = 'b_results' THEN
	ll_test_no = This.object.test_no[row]
	OpenWithParm(w_qa_autotest_results, ll_test_no)
END IF
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
	This.object.vendabbr_modified_by[1] = gnv_app.of_getuserid()
	This.object.vendabbr_modified_date[1] =today()
END IF
RETURN 1


end event

event retrieveend;call super::retrieveend;//////////////////////////////////////////////////////////////////////////////////////////////////////////
//
//	Event: retrieveend
//  Args: rowcount
//	Description: If the status is QRS disable move to quarantine button
//	
//
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//	
//	Revision History
//
//	Developed by 	Date 				Version						Tracking#
//									
// Murali K.			08/19/2008      005 PICS Modifications	 Reqs: QAS 2.2.1
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//

string ls_server
IF THIS.Rowcount() > 0 THEN
	IF rowcount > 0 THEN
		ls_server = this.object.request_status_code[rowcount]
		IF Isnull(ls_server) OR Len(trim(ls_server)) = 0 OR ls_server <> 'P' THEN
			cb_update.enabled = TRUE
		ELSE
			cb_update.enabled = FALSE
		END IF
	END IF
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
// Murali K.			07/02/2008		BOOK_TESTs maintained in PIC instance
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//

//Set transobject 
This.of_SetTransObject(SQLserverTrans)


end event

type gb_1 from groupbox within tabpage_pending
integer x = 5
integer width = 4133
integer height = 1880
integer taborder = 20
integer textsize = -8
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Microsoft Sans Serif"
long textcolor = 33554432
long backcolor = 67108864
string text = "Autotest Rejection History"
end type

type tabpage_moved from userobject within tab_1
integer x = 18
integer y = 112
integer width = 4151
integer height = 1880
long backcolor = 79741120
string text = "Moved to Quarantine"
long tabtextcolor = 33554432
long tabbackcolor = 79741120
long picturemaskcolor = 536870912
dw_2 dw_2
end type

on tabpage_moved.create
this.dw_2=create dw_2
this.Control[]={this.dw_2}
end on

on tabpage_moved.destroy
destroy(this.dw_2)
end on

type dw_2 from u_pics_dw within tabpage_moved
integer x = 14
integer y = 32
integer width = 4128
integer height = 1828
integer taborder = 11
string dataobject = "d_autotest_rejection_history_moved"
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
// Murali K.			07/02/2008		BOOK_TESTs maintained in PIC instance
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//

//Set transobject 
This.of_SetTransObject(SQLserverTrans)
//this.retrieve()

end event

event clicked;call super::clicked;//////////////////////////////////////////////////////////////////////////////////////////////////////////
//
//	Event:  clicked for Move to Quarantine  button
//
//	Description:
//	Update qa tables
//
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//	
//	Revision History
//
//	Developed by 	Date 				Version						Tracking#
//									
// Murali K.			02/11/2008      005 PICS Modifications	 Reqs: QAS a.2.1.
// Murali K.			07/08/08			Open a new screen with results from autotest_files table
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//
long ll_test_no

IF dwo.name = 'b_results' THEN
	ll_test_no = This.object.test_no[row]
	OpenWithParm(w_qa_autotest_results, ll_test_no)
END IF
end event

type cb_fetch from commandbutton within w_qa_prob_books_move_to_quarantine
integer x = 1207
integer y = 24
integer width = 608
integer height = 108
integer taborder = 50
boolean bringtotop = true
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&Get Problem Book"
end type

event clicked;//////////////////////////////////////////////////////////////////////////////////////////////////////////
//
//	Event:  clicked for get problem book btn
//
//	Description:
//	Retrieve selected book
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
long ll_bkseq

dw_lookup.accepttext()
ll_bkseq = dw_lookup.object.bkseq[1]
IF ll_bkseq > 0 THEN
	of_retrievebook(long(ll_bkseq))
END IF

end event

type dw_lookup from u_pics_dw within w_qa_prob_books_move_to_quarantine
integer x = 69
integer y = 28
integer width = 1038
integer height = 128
integer taborder = 10
string dataobject = "d_select_book_number"
boolean vscrollbar = false
boolean border = false
boolean livescroll = false
borderstyle borderstyle = stylebox!
boolean ib_isupdateable = false
end type

event itemchanged;call super::itemchanged;//////////////////////////////////////////////////////////////////////////////////////////////////////////
//
//	Event:  itemchanged
//
//	Description:
//	Retrieve selected book
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

CHOOSE CASE dwo.name
	CASE 'bkseq'
			of_retrievebook(long(data))
END CHOOSE
end event

event constructor;call super::constructor;//////////////////////////////////////////////////////////////////////////////////////////////////////////
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
This.Event pfc_insertrow()

end event

type cb_exit from commandbutton within w_qa_prob_books_move_to_quarantine
integer x = 3977
integer y = 2380
integer width = 265
integer height = 108
integer taborder = 60
boolean bringtotop = true
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Close"
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

type cb_update from commandbutton within w_qa_prob_books_move_to_quarantine
integer x = 3273
integer y = 2380
integer width = 635
integer height = 108
integer taborder = 50
boolean bringtotop = true
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&Move to Quarantine"
end type

event clicked;//////////////////////////////////////////////////////////////////////////////////////////////////////////
//
//	Event:  clicked for Move to Quarantine  button
//
//	Description:
//	Update qa tables
//
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//	
//	Revision History
//
//	Developed by 	Date 				Version						Tracking#
//									
// Murali K.			02/11/2008      005 PICS Modifications	 Reqs: QAS a.2.1.
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//

int li_rc, li_ret
string ls_vendor, ls_name, ls_user
long ll_seq
date ldt_today

ldt_today = Today()


li_ret = MessageBox("Confirm", 'Are you sure you want to initiate the move for  book ' + is_bkmed + string(il_bkseq) + ' to the quarantine area?',   Exclamation!, YesNo!, 2)

IF li_ret = 2 THEN
	RETURN
END IF

// If there were no info on the the screen, don't try to update the screen.
IF dw_header.Rowcount()  > 0 and  &
tab_1.tabpage_pending.dw_1.Rowcount( ) > 0 THEN

	// insert a record in qa mover requests table
	select max(request_no)
	into :ll_seq
	from qa_mover_requests using sqlservertrans;
	
	IF Isnull(ll_seq) THEN
		ll_seq = 1
	ELSE
			ll_seq++;
	END IF
	
	ls_user = gnv_app.of_getuserid()
	
	INSERT INTO QA_MOVER_REQUESTS 
					(REQUEST_NO,
					BKMED,
					BKSEQ,
					PROGRAM_ID,
					CREATED_BY,
					CREATED_DATE,
					REQUEST_STATUS_CODE) 
	VALUES
					(:ll_seq,
					:is_bkmed,
					:il_bkseq,
					'UQRM',
					:ls_user,
					sysdate,
					'P' ) using sqlservertrans;
					
		IF f_check_dberror(SqlServerTrans,"Inserting into QA MOVER REQUESTS")=FALSE THEN
				ROLLBACK USING SqlServerTrans;
				Messagebox("INSERT ERROR","Failed to insert QA MOVER REQUESTS table for book number: "+ String(il_bkseq))
				RETURN -1
		ELSE
				COMMIT USING SQLServertrans;
				Messagebox('Update', ' Flags are set for the book number to be moved to Quarantine area.')
				of_retrievebook(il_bkseq)
		END IF
ELSE
		MessageBox("ERROR","No records found to Insert. Contact system administrator",StopSign!)
		RETURN 0
END IF
end event

type dw_header from u_pics_dw within w_qa_prob_books_move_to_quarantine
event ue_enterkey pbm_dwnprocessenter
integer x = 69
integer y = 192
integer width = 4050
integer height = 116
integer taborder = 20
string dataobject = "d_problem_books_header"
boolean vscrollbar = false
boolean border = false
boolean livescroll = false
borderstyle borderstyle = stylebox!
boolean ib_isupdateable = false
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
IF (dw_header.GetColumn() <> 1 ) THEN
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
This.of_SetTransObject(SQLserverTrans)
 This.Event pfc_insertrow()

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
				This.Object.vcd.validationmsg = "Vendor already exist, a new vendor code should be chosen. "
				RETURN 1
			END IF
				
END CHOOSE

RETURN 0
end event

type cb_clear from commandbutton within w_qa_prob_books_move_to_quarantine
boolean visible = false
integer x = 3657
integer y = 2380
integer width = 251
integer height = 108
integer taborder = 70
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

dw_header.reset()
tab_1.tabpage_pending.dw_1.reset()
dw_header.event pfc_insertrow()
tab_1.tabpage_pending.dw_1.event pfc_insertrow()
dw_header.setfocus()
end event


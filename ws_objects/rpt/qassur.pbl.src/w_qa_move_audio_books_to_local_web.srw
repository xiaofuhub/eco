$PBExportHeader$w_qa_move_audio_books_to_local_web.srw
$PBExportComments$Window to Edit/Terminate Commercial Vendor
forward
global type w_qa_move_audio_books_to_local_web from w_response
end type
type tab_1 from tab within w_qa_move_audio_books_to_local_web
end type
type tabpage_pending from userobject within tab_1
end type
type dw_1 from u_pics_dw within tabpage_pending
end type
type tabpage_pending from userobject within tab_1
dw_1 dw_1
end type
type tabpage_moved from userobject within tab_1
end type
type dw_2 from u_pics_dw within tabpage_moved
end type
type tabpage_moved from userobject within tab_1
dw_2 dw_2
end type
type tab_1 from tab within w_qa_move_audio_books_to_local_web
tabpage_pending tabpage_pending
tabpage_moved tabpage_moved
end type
type cb_exit from commandbutton within w_qa_move_audio_books_to_local_web
end type
type cb_update from commandbutton within w_qa_move_audio_books_to_local_web
end type
type cb_clear from commandbutton within w_qa_move_audio_books_to_local_web
end type
end forward

global type w_qa_move_audio_books_to_local_web from w_response
integer x = 110
integer y = 136
integer width = 5010
integer height = 2760
string title = "Move Audio Books to Local Web"
boolean minbox = true
boolean maxbox = true
boolean resizable = true
windowtype windowtype = main!
windowstate windowstate = maximized!
boolean ib_isupdateable = false
boolean ib_alwaysvalidate = true
tab_1 tab_1
cb_exit cb_exit
cb_update cb_update
cb_clear cb_clear
end type
global w_qa_move_audio_books_to_local_web w_qa_move_audio_books_to_local_web

type prototypes
FUNCTION long GetLogicalDrives() LIBRARY "KERNEL32.DLL"
FUNCTION long GetLogicalDriveStringsW(ulong lbuflen, ref string sbuffer) LIBRARY "KERNEL32.DLL"
end prototypes

type variables
Long il_bkseq
string is_bkmed
n_cst_filesrv inv_filesrv

end variables

on w_qa_move_audio_books_to_local_web.create
int iCurrent
call super::create
this.tab_1=create tab_1
this.cb_exit=create cb_exit
this.cb_update=create cb_update
this.cb_clear=create cb_clear
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.tab_1
this.Control[iCurrent+2]=this.cb_exit
this.Control[iCurrent+3]=this.cb_update
this.Control[iCurrent+4]=this.cb_clear
end on

on w_qa_move_audio_books_to_local_web.destroy
call super::destroy
destroy(this.tab_1)
destroy(this.cb_exit)
destroy(this.cb_update)
destroy(this.cb_clear)
end on

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
// Murali K.			02/22/2008      005 PICS Modifications	 Reqs: QAS A.10.1
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

type tab_1 from tab within w_qa_move_audio_books_to_local_web
integer x = 32
integer y = 28
integer width = 4366
integer height = 2036
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
//	Retrieve moved to local web rows
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
	cb_update.enabled=FALSE
	cb_clear.enabled=FALSE
ELSE
	cb_update.enabled= TRUE
	cb_clear.enabled=TRUE
END IF

end event

type tabpage_pending from userobject within tab_1
integer x = 18
integer y = 112
integer width = 4329
integer height = 1908
long backcolor = 79741120
string text = "Not Moved/Pending"
long tabtextcolor = 33554432
long tabbackcolor = 79741120
long picturemaskcolor = 536870912
dw_1 dw_1
end type

on tabpage_pending.create
this.dw_1=create dw_1
this.Control[]={this.dw_1}
end on

on tabpage_pending.destroy
destroy(this.dw_1)
end on

type dw_1 from u_pics_dw within tabpage_pending
integer x = 14
integer y = 36
integer width = 4274
integer height = 1856
integer taborder = 40
string dataobject = "d_move_audio_books"
end type

event clicked;call super::clicked;//////////////////////////////////////////////////////////////////////////////////////////////////////////
//
//	Event: clicked
//
//	Description:
//	Set the select checkboxes
//
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//	
//	Revision History
//
//	Developed by 	Date 				Version						Tracking#
//									
// Murali K.			02/24/2008      005 PICS Modifications	 Reqs: QAS A.10.1 - A.11.2
//////////////////////////////////////////////////////////////////////////////////////////////////////////
LONG ll_rc,ll_loop

IF dwo.Name = 'b_all' THEN // button All is clicked
	ll_rc = this.rowcount()
	IF ll_rc > 0 THEN
		FOR ll_loop = 1 TO ll_rc
			IF isnull(this.object.request_status_code [ll_loop]) THEN
				This.object.check_box[LL_LOOP]='Y'
			END IF
		NEXT
	END IF
END IF
end event

event retrieveend;call super::retrieveend;//////////////////////////////////////////////////////////////////////////////////////////////////////////
//
//	Event: retrievestat
//
//	Description:
//	close Display indicator
//
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//	
//	Revision History
//
//	Developed by 	Date 				Version						Tracking#
//									
// Murali K.			02/24/2008      005 PICS Modifications	 Reqs: QAas A.10.1
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//


close(w_pics_retrieve_msg_box)
end event

event retrievestart;call super::retrievestart;//////////////////////////////////////////////////////////////////////////////////////////////////////////
//
//	Event: retrievestat
//
//	Description:
//	Display indicator
//
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//	
//	Revision History
//
//	Developed by 	Date 				Version						Tracking#
//									
// Murali K.			02/24/2008      005 PICS Modifications	 Reqs: QAas A.10.1
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//


openwithparm(w_pics_retrieve_msg_box,"Retrieving QA Information, Please Wait...")
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
// Murali K.			01/24/2008      005 PICS Modifications	 Reqs: QAas A.10.1
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//

//Set transobject 
This.of_SetTransObject(SQLserverTrans)
This.Retrieve()


end event

type tabpage_moved from userobject within tab_1
integer x = 18
integer y = 112
integer width = 4329
integer height = 1908
long backcolor = 79741120
string text = "Moved to Local Web"
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
integer y = 28
integer width = 4288
integer height = 1864
integer taborder = 11
string dataobject = "d_local_web_moved"
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


end event

type cb_exit from commandbutton within w_qa_move_audio_books_to_local_web
integer x = 4146
integer y = 2116
integer width = 265
integer height = 96
integer taborder = 60
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
// Murali K.			02/24/2008      005 PICS Modifications	 Reqs: QAS A.10.1
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//
parent.Event pfc_close()

end event

type cb_update from commandbutton within w_qa_move_audio_books_to_local_web
integer x = 3209
integer y = 2116
integer width = 603
integer height = 96
integer taborder = 50
boolean bringtotop = true
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&Move to Local Web"
end type

event clicked;//////////////////////////////////////////////////////////////////////////////////////////////////////////
//
//	Event:  clicked for Move  button
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
// Murali K.			02/22/2008      005 PICS Modifications	 Reqs: QAS a.10.1 - a.11.2
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//


string ls_vendor, ls_name, ls_user, ls_bkmed, ls_checkbox
int li_ret
long ll_seq, ll_loop, ll_rc, ll_bkseq
date ldt_today
boolean lb_insert=FALSE

ldt_today = Today()


li_ret = MessageBox("Confirm", 'Are you sure you want to initiate the move  to the local web?',   Exclamation!, YesNo!, 2)

IF li_ret = 2 THEN
	RETURN
END IF

// If there were no info on the the screen, don't try to update the screen.
ll_rc = tab_1.tabpage_pending.dw_1.Rowcount()

IF ll_rc  > 0 THEN
	FOR ll_loop = 1 TO ll_rc
		
		ll_bkseq  = tab_1.tabpage_pending.dw_1.object.bkseq[ll_loop]
		ls_bkmed =  tab_1.tabpage_pending.dw_1.object.bkmed[ll_loop]
		ls_checkbox = tab_1.tabpage_pending.dw_1.object.check_box[ll_loop]
		
		// insert a record in qa mover requests table
		IF ls_checkbox = 'Y' THEN
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
							:ls_bkmed,
							:ll_bkseq,
							'ARM',
							:ls_user,
							sysdate,
							'P') using sqlservertrans;
							
				lb_insert=TRUE
			
			END IF // ONLY IF SELECTED
	NEXT
	IF lb_insert THEN
		IF f_check_dberror(SqlServerTrans,"Inserting into QA MOVER REQUESTS")=FALSE THEN
				ROLLBACK USING SqlServerTrans;
				Messagebox("INSERT ERROR","Failed to insert QA MOVER REQUESTS table for book number: "+ String(il_bkseq))
				RETURN -1
		ELSE
				COMMIT USING SQLServertrans;
				Messagebox('Update', ' Flags are set for the book(s) to be moved to local web.')
				tab_1.tabpage_pending.dw_1.Retrieve() // refresh
		END IF
	ELSE
		MessageBox("Warning","No records selected",StopSign!)
	END IF
ELSE
		MessageBox("ERROR","No records found to Insert. Contact system administrator",StopSign!)
		RETURN 0
END IF
end event

type cb_clear from commandbutton within w_qa_move_audio_books_to_local_web
integer x = 3845
integer y = 2116
integer width = 251
integer height = 96
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
//	Event:  clicked for Move  button
//
//	Description:
//	Clear check boxes
//
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//	
//	Revision History
//
//	Developed by 	Date 				Version						Tracking#
//									
// Murali K.			02/22/2008      005 PICS Modifications	 Reqs: QAS a.10.1 - a.11.2
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//


LONG ll_rc,ll_loop
ll_rc = tab_1.tabpage_pending.dw_1.rowcount()
IF ll_rc > 0 THEN
	FOR ll_loop = 1 TO ll_rc
		tab_1.tabpage_pending.dw_1.object.check_box[LL_LOOP]='N'
	NEXT
END IF

end event


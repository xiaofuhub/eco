$PBExportHeader$w_qa_email_resend.srw
forward
global type w_qa_email_resend from w_response
end type
type dw_ds from u_pics_dw within w_qa_email_resend
end type
type cb_spellcheck from u_cb within w_qa_email_resend
end type
type dw_resend from u_pics_dw within w_qa_email_resend
end type
type cb_ok from u_cb within w_qa_email_resend
end type
type cb_cancel from u_cb within w_qa_email_resend
end type
type cb_clear from u_cb within w_qa_email_resend
end type
end forward

global type w_qa_email_resend from w_response
integer x = 613
integer y = 348
integer width = 3959
integer height = 1988
string title = "Resend Email"
boolean center = true
boolean ib_disableclosequery = true
dw_ds dw_ds
cb_spellcheck cb_spellcheck
dw_resend dw_resend
cb_ok cb_ok
cb_cancel cb_cancel
cb_clear cb_clear
end type
global w_qa_email_resend w_qa_email_resend

type variables
Long il_id
datastore ids_resend
n_cst_filesrv inv_filesrv
string is_filename
end variables

forward prototypes
public function integer of_send_email ()
public function integer of_process_attachment ()
end prototypes

public function integer of_send_email ();//////////////////////////////////////////////////////////////////////////////////////////////////////////
//
//	function: of_send_email
// Args: none
//	Description:
//	Resend email to producer
// Returns : Integer 1 for success, -1 for error
//
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//	
//	Revision History
//
//	Developed by 	Date 				Version						Tracking#
//									
// Murali K.			02/08/2008      005 PICS Modifications	 Reqs: QAS 2.2.1
// Murali K.			05/12/2008 		2.0 				Process Attachments if any 
// Murali K. 			07/15/2008 		Process cc from email_cc_log
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//

mailSession				mSes
mailReturnCode			mRet
mailMessage			mMsg
mailFileDescription		mAttach
string					ls_ret, ls_syntax, ls_name, ls_open_pathname, ls_filename
string					ls_attach_name//'c:\marc0701.dat.txt'
int						li_index, li_nret, li_nrecipients, li_nfile, li_pos, li_loop, ll_count=2
datastore			lds
long 					ll_message_id, ll_rc

mRet=mailReturnFailure!
mSes = CREATE mailSession

mRet = mSes.mailLogon ( mailNewSession! )
ls_ret = f_mail_error_to_string ( mRet, 'Logon:', FALSE )

If mRet <> mailReturnSuccess! Then
	MessageBox ("Mail Logon failed", 'Return Code <> mailReturnSuccess! '+ls_ret )
	mSes.mailLogoff()
	DESTROY mSes
	return -1 
End If

SetPointer(HourGlass!)

dw_resend.accepttext()
mMsg.Subject = dw_resend.object.subject[1]
mMsg.NoteText = dw_resend.object.message[1]

// Process Attachments if any 05/12/2008 
of_process_attachment()
IF Len(Trim(is_filename)) > 0  THEN
	mAttach.FileType = mailAttach!
	mAttach.PathName ='c:\temp\' + is_filename 
	mAttach.FileName = is_filename
	mMsg.AttachmentFile[1] = mAttach
END IF

// Send it to producer
mMsg.Recipient[1].Name = dw_resend.object.email_to[1] // to the producer 

// 07/15/2008 process cc from email_cc_log
lds = create datastore
lds.dataobject = 'd_email_cc_log'
lds.settransobject(sqlserveroracletrans)
ll_message_id = dw_resend.object.message_id[1]
ll_rc = lds.Retrieve(ll_message_id)
FOR li_loop = 1 to ll_rc
	mMsg.Recipient[ll_count].Name = lds.object.email_cc[li_loop] 
	mMsg.Recipient[ll_count].RecipientType = Mailcc!
	ll_count++
NEXT
////////

mRet = mSes.mailsend ( mMsg )
ls_ret = f_mail_error_to_string ( mRet, 'Send mail:', FALSE )
IF mRet <> mailreturnsuccess! THEN
		MessageBox ("Mail Send",'Return Code <> mailReturnSuccess! '+ls_ret )
	RETURN -1
END IF	

mRet=mSes.mailLogoff()
ls_ret = f_mail_error_to_string ( mRet, 'Logoff:', FALSE )
IF mRet <> mailreturnsuccess! THEN
		MessageBox ("Logoff",'Return Code <> mailReturnSuccess! '+ls_ret )
	RETURN -1
END IF	
DESTROY mSes
return 1

end function

public function integer of_process_attachment ();//////////////////////////////////////////////////////////////////////////////////////////////////////////
//
//	Function:  of_process_attachment
//  Args: None
//	Description:
//	If there was an attachment with the email, while resending the email process
// the attachment as well. Attachment needs to be sent also.
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//	
//	Revision History
//
//	Developed by 	Date 				Version						Tracking#
//									
// Murali K.			05/12/2008      005 PICS Modifications	 Reqs: QAS 2.2.1
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//

blob 		lbl_blob
string 	ls_name, ls_attach_name, ls_filename
integer	li_pos

//Get the value of the name 
select name
into :ls_name
from qa_email_log
where message_id = : il_id using sqlservertrans;

 IF f_check_dberror(SqlServerTrans,"Select Name from QA_EMAIL_LOG ") = FALSE THEN
	Messagebox("Error in select","Select Name column from QA_EMAIL_LOG")
	RETURN -1
END IF

// No attachments
IF Isnull(ls_name) or Len(trim(ls_name)) = 0 THEN
	RETURN -1
END IF

// Get the blob content
SELECTBLOB Blob_Content
 INTO  :lbl_blob
FROM NLS_DOCUMENTS
WHERE NAME = :ls_name using sqlserveroracletrans;

 IF f_check_dberror(SqlServeroracleTrans,"Select blob content  from view nls_documents ") = FALSE THEN
	Messagebox("Error in select","Select blob content  from view nls_documents")
	RETURN -1
END IF

// No content
IF Isnull(lbl_blob)  THEN
	RETURN -1
END IF

// set the filename
ls_attach_name=ls_name
li_pos=pos(ls_attach_name,'/')
if li_pos >0 then
	ls_filename=mid(ls_attach_name,li_pos +1)
end if

do while pos(ls_filename,'/') > 0
	li_pos=pos(ls_filename,'/')
	ls_filename=mid(ls_filename, li_pos+1)
loop

// write the file with the content
IF inv_filesrv.of_filewrite('c:\temp\' +  ls_filename,lbl_blob,FALSE) = -1 THEN
	RETURN -1
END IF
is_filename = ls_filename
RETURN 1
end function

on w_qa_email_resend.create
int iCurrent
call super::create
this.dw_ds=create dw_ds
this.cb_spellcheck=create cb_spellcheck
this.dw_resend=create dw_resend
this.cb_ok=create cb_ok
this.cb_cancel=create cb_cancel
this.cb_clear=create cb_clear
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_ds
this.Control[iCurrent+2]=this.cb_spellcheck
this.Control[iCurrent+3]=this.dw_resend
this.Control[iCurrent+4]=this.cb_ok
this.Control[iCurrent+5]=this.cb_cancel
this.Control[iCurrent+6]=this.cb_clear
end on

on w_qa_email_resend.destroy
call super::destroy
destroy(this.dw_ds)
destroy(this.cb_spellcheck)
destroy(this.dw_resend)
destroy(this.cb_ok)
destroy(this.cb_cancel)
destroy(this.cb_clear)
end on

event open;call super::open;//////////////////////////////////////////////////////////////////////////////////////////////////////////
//
//	Event:open for window
//
//	Description:
//	Set parameter variable and focus on the resend datawindow
//
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//	
//	Revision History
//
//	Developed by 	Date 				Version						Tracking#
//									
// Murali K.			02/08/2008      005 PICS 2.0 Modifications	 QAS A.2.1 , A2.2 
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//

dw_resend.SetFocus()
il_id = Message.doubleparm
inv_filesrv = create n_cst_filesrv 
end event

event pfc_postopen;call super::pfc_postopen;//////////////////////////////////////////////////////////////////////////////////////////////////////////
//
//	Event:open for window
//
//	Description:
//	Set parameter variable and focus on the resend datawindow
//
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//	
//	Revision History
//
//	Developed by 	Date 				Version						Tracking#
//									
// Murali K.			02/08/2008      005 PICS 2.0 Modifications	 QAS A.2.1 , A2.2 
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//
string ls_title
long ll_rc

dw_resend.event pfc_retrieve()
ll_rc =  dw_resend.Rowcount() 
IF ll_rc >  0 THEN
   ls_title = dw_resend.object.prdr[ll_rc] + string(	dw_resend.object.bkseq[ll_rc]) + '_' + string(il_id)
	This.title += '-' +  ls_title
END IF
end event

type dw_ds from u_pics_dw within w_qa_email_resend
boolean visible = false
integer x = 242
integer y = 1752
integer width = 398
integer height = 148
integer taborder = 40
string dataobject = "d_ds_qa_resend_log"
boolean vscrollbar = false
boolean livescroll = false
borderstyle borderstyle = stylebox!
boolean ib_rmbmenu = false
boolean ib_rmbfocuschange = false
end type

event pfc_preupdate;call super::pfc_preupdate;//////////////////////////////////////////////////////////////////////////////////////////////////////////
//
//	Event: pfc_preupdate
//
//	Description:
//	Set column default values before updates and set new record for resend log
//
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//	
//	Revision History
//
//	Developed by 	Date 				Version						Tracking#
//									
// Murali K.			02/08/2008      005 PICS Modifications	 Reqs: QAS 2.2.1
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//
long ll_rc, ll_seq, ll_row
string ls_message, ls_msgparm[], ls_vendor, ls_name

//select the next primary key sequence number
	SELECT max(resend_no) 
	INTO :ll_seq 
	FROM QA_EMAIL_RESEND_LOG  using SQLserverTrans;
	
	IF SQLserverTrans.sqlCode < 0 THEN
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

	// set the new record for the qa resend log table
	IF isnull(ll_seq) THEN
		ll_seq = 1
	END IF
	
	ll_seq++
	ll_row = dw_ds.insertrow(0)
	dw_ds.object.resend_no[ll_row] = ll_seq
	dw_ds.object.message_id[ll_row] = il_id
	dw_ds.object.resend_date[ll_row] = today()
	dw_ds.object.resend_by[ll_row] = gnv_app.of_getuserid()
	dw_ds.object.created_date[ll_row] = today()
	dw_ds.object.created_by[ll_row] = gnv_app.of_getuserid()

RETURN 1
end event

type cb_spellcheck from u_cb within w_qa_email_resend
integer x = 3406
integer y = 1588
integer width = 448
integer taborder = 30
string text = "S&pellcheck"
boolean default = true
end type

event clicked;call super::clicked;//////////////////////////////////////////////////////////////////////////////////////////////////////////
//
//	Event:  clicked fo spellcheck button
//
//	Description:
//	Perform spellcheck on message text before resending email
//
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//	
//	Revision History
//
//	Developed by 	Date 				Version						Tracking#
//									
// Murali K.			02/08/2008      005 PICS Modifications	 Reqs: QAS 2.2.1
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//

nca_word lnca_word
String ls_message
	
// Check for any pending updates
IF of_UpdateChecks( ) < 0 THEN Return -1
	
dw_resend.accepttext()
ls_message = dw_resend.object.message[1]
IF NOT(IsNull( ls_message)) THEN
	lnca_Word.SpellCheck( ls_message )
	dw_resend.object.message[1] = ls_message
END IF


end event

type dw_resend from u_pics_dw within w_qa_email_resend
event ue_enterkey pbm_dwnprocessenter
integer x = 41
integer y = 36
integer width = 3872
integer height = 1708
integer taborder = 10
string dataobject = "d_qa_email_resend"
boolean vscrollbar = false
boolean livescroll = false
borderstyle borderstyle = stylebox!
end type

event ue_enterkey;//////////////////////////////////////////////////////////////////////////////////////////////////////////
//
//	Event:  enter key for dw
//
//	Description:
//	Simulate resend or update button
//
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//	
//	Revision History
//
//	Developed by 	Date 				Version						Tracking#
//									
// Murali K.			02/08/2008      005 PICS Modifications	 Reqs: QAS 2.2.1
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//
Send(Handle(Parent), 273, 0, Handle(cb_ok)) // clicked event of resend button
end event

event constructor;call super::constructor;//////////////////////////////////////////////////////////////////////////////////////////////////////////
//
//	Event: constructor
//
//	Description:
//	Set transaction object for the datawindow
//
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//	
//	Revision History
//
//	Developed by 	Date 				Version						Tracking#
//									
// Murali K.			02/08/2008      005 PICS 2.0 Modifications	 QAS A.2.1 , A2.2 
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//
This.SetTransObject(sqlservertrans)
dw_ds.SetTransObject(sqlservertrans)

end event

event pfc_retrieve;call super::pfc_retrieve;//////////////////////////////////////////////////////////////////////////////////////////////////////////
//
//	Event: constructor
//
//	Description:
//	Set transaction object for the datawindow
//
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//	
//	Revision History
//
//	Developed by 	Date 				Version						Tracking#
//									
// Murali K.			02/08/2008      005 PICS 2.0 Modifications	 QAS A.2.1 , A2.2 
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//
RETURN This.Retrieve(il_id)

end event

type cb_ok from u_cb within w_qa_email_resend
integer x = 2935
integer y = 1776
integer width = 448
integer taborder = 0
string text = "&Resend"
boolean default = true
end type

event clicked;call super::clicked;//////////////////////////////////////////////////////////////////////////////////////////////////////////
//
//	Event:  clicked for resend button
//
//	Description:
//	Save updatable datawindows. Add a new record in qa_resend_log table
//
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//	
//	Revision History
//
//	Developed by 	Date 				Version						Tracking#
//									
// Murali K.			02/08/2008      005 PICS Modifications	 Reqs: QAS 2.2.1
// Murali K.			12/18/2008		 resend even if there are no changes Tracker 2147
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//

int li_rc, li_ret
string ls_vendor, ls_name

// Accept the text that was put on the screen.
dw_resend.AcceptText()
dw_ds.AcceptText()

IF dw_resend.ModifiedCount( ) > 0 OR  &
	dw_ds.ModifiedCount( ) > 0 THEN
	li_ret = 1
ELSE
 	li_ret = MessageBox("Update","There are no changes to the email text. Do you still want to resend the email",Exclamation!,YesNo!,1)
END IF

// If there were no changes to the screen, don't try to update the screen.
//IF dw_resend.ModifiedCount( ) > 0 OR  &
//	dw_ds.ModifiedCount( ) > 0 THEN
	// Resend the email only if data changed Tracker 2147 12/18/2008
IF li_ret = 1 THEN
	lI_rc = of_send_email()
	IF li_rc < 1 	THEN
		RETURN 0
	END IF
	
	li_rc = dw_resend.Event pfc_update(TRUE, TRUE)
	IF li_rc = 1 THEN
		// Insert a record in qa resend log table
		li_rc = dw_ds.Event pfc_update(TRUE, TRUE)
		IF li_rc = 1 THEN
			COMMIT USING SQLServertrans;
			MessageBox("Update","Email Resend to producer",Information!)
			close(parent)
			RETURN 1
		ELSE
			MessageBox("ERROR","Update failed.",StopSign!)
			dw_resend.Resetupdate()
		END IF
	ELSE
		MessageBox("ERROR","Update failed.",StopSign!)
		dw_resend.Resetupdate()
		RETURN 0
	END IF
//ELSE 
// 	MessageBox("Update","There are no changes to the email text.",Information!)
//	RETURN 0
END IF
end event

type cb_cancel from u_cb within w_qa_email_resend
integer x = 3461
integer y = 1776
integer width = 448
integer taborder = 0
boolean bringtotop = true
string text = "Exit"
end type

event clicked;call super::clicked;Close(parent)
end event

type cb_clear from u_cb within w_qa_email_resend
boolean visible = false
integer x = 2930
integer y = 1776
integer width = 448
integer taborder = 20
string text = "Clear"
end type


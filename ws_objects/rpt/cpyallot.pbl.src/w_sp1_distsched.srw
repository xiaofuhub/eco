$PBExportHeader$w_sp1_distsched.srw
forward
global type w_sp1_distsched from w_response
end type
type sle_rows from singlelineedit within w_sp1_distsched
end type
type st_1 from statictext within w_sp1_distsched
end type
type dw_sp1_distsched from u_pics_dw within w_sp1_distsched
end type
type cb_exit from commandbutton within w_sp1_distsched
end type
type cb_update from commandbutton within w_sp1_distsched
end type
type cb_find from commandbutton within w_sp1_distsched
end type
type cb_clear from commandbutton within w_sp1_distsched
end type
end forward

global type w_sp1_distsched from w_response
integer x = 110
integer y = 136
integer width = 1938
integer height = 1252
string title = "Update Producer Quantity on the WEB"
sle_rows sle_rows
st_1 st_1
dw_sp1_distsched dw_sp1_distsched
cb_exit cb_exit
cb_update cb_update
cb_find cb_find
cb_clear cb_clear
end type
global w_sp1_distsched w_sp1_distsched

on w_sp1_distsched.create
int iCurrent
call super::create
this.sle_rows=create sle_rows
this.st_1=create st_1
this.dw_sp1_distsched=create dw_sp1_distsched
this.cb_exit=create cb_exit
this.cb_update=create cb_update
this.cb_find=create cb_find
this.cb_clear=create cb_clear
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.sle_rows
this.Control[iCurrent+2]=this.st_1
this.Control[iCurrent+3]=this.dw_sp1_distsched
this.Control[iCurrent+4]=this.cb_exit
this.Control[iCurrent+5]=this.cb_update
this.Control[iCurrent+6]=this.cb_find
this.Control[iCurrent+7]=this.cb_clear
end on

on w_sp1_distsched.destroy
call super::destroy
destroy(this.sle_rows)
destroy(this.st_1)
destroy(this.dw_sp1_distsched)
destroy(this.cb_exit)
destroy(this.cb_update)
destroy(this.cb_find)
destroy(this.cb_clear)
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
	dw_sp1_distsched.of_SetTransObject(SQLServerOracleTrans)
	dw_sp1_distsched.Object.DataWindow.QueryMode='Yes'

	dw_sp1_distsched.SetFocus()

	cb_update.Enabled=FALSE
	cb_clear.Enabled=FALSE
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

type sle_rows from singlelineedit within w_sp1_distsched
integer x = 466
integer y = 1004
integer width = 233
integer height = 96
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 33554432
borderstyle borderstyle = stylelowered!
end type

type st_1 from statictext within w_sp1_distsched
integer x = 37
integer y = 1012
integer width = 457
integer height = 64
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 33554432
long backcolor = 67108864
string text = "Rows Selected:"
boolean focusrectangle = false
end type

type dw_sp1_distsched from u_pics_dw within w_sp1_distsched
event ue_entrkey pbm_dwnprocessenter
integer x = 27
integer y = 20
integer width = 1870
integer height = 948
integer taborder = 10
string dataobject = "d_sp1_distsched_update_qnty"
end type

event ue_entrkey;Send(Handle(this),256,9,Long(0,0))
return(1)
end event

event constructor;call super::constructor;// set the transaction object.
This.of_SetTransObject(SQLServerOracleTrans)

end event

event retrievestart;call super::retrievestart;openwithparm(w_pics_retrieve_msg_box,"Retrieving data, Please Wait...")
end event

event retrieveend;call super::retrieveend;close(w_pics_retrieve_msg_box)
end event

event sqlpreview;call super::sqlpreview;//MessageBox("sql",sqlsyntax)
end event

event updatestart;call super::updatestart;openwithparm(w_pics_retrieve_msg_box,"Updating data in sp1, Please Wait...")

end event

event updateend;call super::updateend;close(w_pics_retrieve_msg_box)

end event

type cb_exit from commandbutton within w_sp1_distsched
integer x = 1646
integer y = 1004
integer width = 247
integer height = 108
boolean bringtotop = true
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "E&xit"
end type

event clicked;dw_sp1_distsched.ResetUpdate()
IF NOT SQLServerOracleTrans.DBHandle() =0 THEN
	IF SQLServerOracleTrans.of_disconnect() < 0 THEN
		MessageBox("Error","Oracle Database Disconnect Error.",StopSign!)
	END IF
END IF
parent.Event pfc_close()

end event

type cb_update from commandbutton within w_sp1_distsched
integer x = 1019
integer y = 1004
integer width = 311
integer height = 108
boolean bringtotop = true
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "&Update"
end type

event clicked;int rc,lordqty
long lbkseq,i
string lbkmed,llibcd
datetime lcabdt


dw_sp1_distsched.of_SetTransObject(SQLServerOracleTrans)
// Accept the text that was put on the screen.
dw_sp1_distsched.AcceptText()

// If there were no changes to the screen, don't try to update the screen.
IF dw_sp1_distsched.ModifiedCount( ) > 0 THEN
	rc = dw_sp1_distsched.of_update(TRUE, TRUE)
	if rc = 1 THEN
		COMMIT USING SQLServerOracleTrans;
		openwithparm(w_pics_retrieve_msg_box,"Updating sched in sp5, Please Wait...")
		FOR i=1 TO dw_sp1_distsched.RowCount()
			lbkmed = mid(dw_sp1_distsched.object.bkno[i],1,2)
			lbkseq = long(mid(dw_sp1_distsched.object.bkno[i],3))
			llibcd = trim(dw_sp1_distsched.object.libcd[i])
			lcabdt = (dw_sp1_distsched.object.cabdt[i])
			lordqty = dw_sp1_distsched.object.ordqty[i]
			UPDATE sched  
				SET ordqty = :lordqty  
			WHERE ( sched.bkseq = :lbkseq ) AND  
					( sched.bkmed = :lbkmed ) AND  
					( sched.libcd = :llibcd ) AND  
					( sched.cabdt = :lcabdt )   
			USING SQLServerTrans;
			IF f_check_dberror(SQLServerTrans,"SCHED")=FALSE THEN
				close(w_pics_retrieve_msg_box)
				MessageBox("ERROR","Updating sched table failed. ~r~n Error in row: bkseq= "+string(lbkseq)+" bkmed= "+lbkmed+" libcd= "+llibcd+ " cabdt= "+string(date(lcabdt)),StopSign!)
				Rollback using sqlservertrans;
				RETURN 1	
			END IF
		NEXT
		close(w_pics_retrieve_msg_box)
		COMMIT USING SQLServerTrans;
		MessageBox("Update","Update Successful.",Information!)
		return 1
	else
		MessageBox("ERROR","Update failed.",StopSign!)
		Rollback using sqlserveroracletrans;
		RETURN 0
	end if
else // if Modifiedcount > 1
 	MessageBox("Update","There are no changes to the record.",Information!)
	RETURN 0
end if	
end event

type cb_find from commandbutton within w_sp1_distsched
integer x = 745
integer y = 1004
integer width = 247
integer height = 108
boolean bringtotop = true
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "F&ind"
end type

event clicked;long ll_rows

dw_sp1_distsched.of_SetTransObject(SQLServerOracleTrans)
dw_sp1_distsched.AcceptText()
dw_sp1_distsched.Object.DataWindow.QueryMode='No'
ll_rows = dw_sp1_distsched.Retrieve()
IF ll_rows = 0 THEN
   MessageBox("Find Error", "Records does not exist.?" ,Information!, Ok!)
	dw_sp1_distsched.InsertRow(0)
	dw_sp1_distsched.Object.DataWindow.QueryMode='Yes'
	cb_clear.Enabled=TRUE
   dw_sp1_distsched.setfocus()
ELSE
	sle_rows.Text=string(ll_rows)
	cb_clear.Enabled=TRUE
	cb_update.Enabled=TRUE
	cb_find.Enabled=FALSE		
   dw_sp1_distsched.setfocus()
END IF

end event

type cb_clear from commandbutton within w_sp1_distsched
integer x = 1362
integer y = 1004
integer width = 251
integer height = 108
boolean bringtotop = true
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "&Clear"
end type

event clicked;long ll_rows,rtn

dw_sp1_distsched.Object.DataWindow.QueryClear = "Yes"
dw_sp1_distsched.Reset()
ll_rows = dw_sp1_distsched.InsertRow(0)
dw_sp1_distsched.ScrolltoRow(ll_rows)
sle_rows.text=""

dw_sp1_distsched.Object.DataWindow.QueryMode='Yes'



dw_sp1_distsched.SetFocus()

cb_find.Enabled=TRUE
cb_find.Default=TRUE
cb_update.Enabled=FALSE
cb_clear.Enabled=FALSE

end event


$PBExportHeader$w_user_change_passwrd.srw
forward
global type w_user_change_passwrd from w_sheet
end type
type dw_user_change_password from u_dw within w_user_change_passwrd
end type
type cb_exit from u_cb within w_user_change_passwrd
end type
type cb_clear from u_cb within w_user_change_passwrd
end type
type cb_update from u_cb within w_user_change_passwrd
end type
end forward

global type w_user_change_passwrd from w_sheet
integer x = 283
integer y = 328
integer width = 2615
integer height = 1096
string title = "User Change Password"
boolean controlmenu = false
boolean minbox = false
boolean maxbox = false
boolean resizable = false
windowtype windowtype = response!
dw_user_change_password dw_user_change_password
cb_exit cb_exit
cb_clear cb_clear
cb_update cb_update
end type
global w_user_change_passwrd w_user_change_passwrd

type variables
boolean password_changed=FALSE
string is_password, is_newpassworddata, is_passworddata
n_cst_platformwin32 inv_32
end variables

forward prototypes
public subroutine wf_disable_objects ()
public function integer wf_checkusers (string l_username)
public function integer wf_checksysusers (string l_username)
public function integer wf_addmenu ()
end prototypes

public subroutine wf_disable_objects ();cb_clear.Enabled = FALSE

cb_update.Enabled = FALSE
dw_user_change_password.setfocus()
//dw_user_change_password.Setcolumn('passwrd')
//12/15/2008 #2119
dw_user_change_password.Setcolumn('new_passwrd')
end subroutine

public function integer wf_checkusers (string l_username);string ls_userid


 SELECT picsuser.userid
    INTO :ls_userid
    FROM picsuser  
   WHERE picsuser.userid = :l_username
   USING SQLserverTrans;

  
RETURN sqlservertrans.SQLcode
end function

public function integer wf_checksysusers (string l_username);
string ls_username

  SELECT sysusers.username  
    INTO :ls_username  
    FROM sysusers
   WHERE sysusers.username = :l_username
	USING sqlservertrans;
	
	RETURN sqlservertrans.SQLcode

end function

public function integer wf_addmenu ();//This function will add the menus that the group has to the 
//user. The useraccess table will be updated with all the menu
//items that the group will have.


String ls_group, ls_menu[], ls_access[], ls_userid, ls_temp
Integer li_loop,lcount=0
boolean group_exist=FALSE


//ls_group = dw_user_maintenance.GetItemString(1,"group_")
//ls_userid = dw_user_maintenance.GetItemString(1,"userid")
//
//select count(*)
//into 	:lcount
//from 	picsaccess
//where userid = :ls_userid
//using sqlservertrans;
//
//// IF userid does not exist create its accesses.
//
//IF lcount = 0 THEN
//	//create a data store and read in values for the specific group
//	datastore lds_datastore
//	lds_datastore = CREATE datastore
//	lds_datastore.DataObject = "d_group_access_datastore"
//	lds_datastore.SetTransObject (SqlServerTrans)
//	lds_datastore.Retrieve(ls_group)
//	
//	//FOR li_loop = 1 TO lds_datastore.RowCount()
//	
//	FOR li_loop = 1 TO lds_datastore.RowCount()
//	 ls_menu[li_loop] = lds_datastore.GetItemString(li_loop,"menu")
//	 ls_access[li_loop] = lds_datastore.GetItemString(li_loop,"access_")
//	NEXT
//	
//	//Destroy the datastore
//	DESTROY lds_datastore
//	
//	//First check to see if there is any data in the picsaccess table.
//	//If data exists then delete.
//			SELECT DISTINCT userid
//			INTO :ls_temp
//			FROM picsaccess
//			WHERE userid = :ls_userid
//			USING SqlServerTrans;
//			
//	IF SqlServerTrans.SqlCode = 0 THEN  //Delete the menu options
//		  DELETE FROM picsaccess
//		  WHERE userid = :ls_userid
//		  USING SqlServerTrans;
//	END IF
//	
//	
//	
//	//INSERT the menu items to the picsaccess table
//	FOR li_loop = 1 TO UpperBound(ls_menu[])
//		INSERT INTO picsaccess
//						(userid, menu, access_)
//		VALUES (:ls_userid, :ls_menu[li_loop], :ls_access[li_loop])
//		USING SqlServerTrans;
//		
//	NEXT
//END IF
//

RETURN 1




end function

on w_user_change_passwrd.create
int iCurrent
call super::create
this.dw_user_change_password=create dw_user_change_password
this.cb_exit=create cb_exit
this.cb_clear=create cb_clear
this.cb_update=create cb_update
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_user_change_password
this.Control[iCurrent+2]=this.cb_exit
this.Control[iCurrent+3]=this.cb_clear
this.Control[iCurrent+4]=this.cb_update
end on

on w_user_change_passwrd.destroy
call super::destroy
destroy(this.dw_user_change_password)
destroy(this.cb_exit)
destroy(this.cb_clear)
destroy(this.cb_update)
end on

event closequery;//////////////////////////////////////////////////////////////////////////////
//
//	Event:  closequery 
//
//	Description:
//	Search for unsaved datawindows prompting the user if any
//	pending updates are found.
//
//////////////////////////////////////////////////////////////////////////////
//	
//	Revision History
//
//	Version
//	5.0   Initial version
//
//////////////////////////////////////////////////////////////////////////////
//
//	Copyright © 1996-1997 Sybase, Inc. and its subsidiaries.  All rights reserved.
//	Any distribution of the PowerBuilder Foundation Classes (PFC)
//	source code by other than Sybase, Inc. and its subsidiaries is prohibited.
//
//////////////////////////////////////////////////////////////////////////////

Integer	li_pendingrc
Integer	li_validationrc
Integer	li_accepttextrc
Integer	li_msg
Integer	li_rc, li_rtn_code
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
			li_rtn_code = cb_update.Event clicked()
			IF li_rtn_code = 1 THEN
				RETURN 0
			END IF
			
			
//			// YES - Update
//			// If the update fails, prevent the window from closing
//			If This.Event pfc_save() >= 1 Then
//				// Successful update, allow the window to be closed
//				Return 0
//			End If
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

event pfc_postopen;call super::pfc_postopen;dw_user_change_password.SetFocus()
wf_disable_objects()

//Disable the addrow and delete row menu items
m_pics_main.m_edit.m_deleterow.Enabled = FALSE
m_pics_main.m_edit.m_addrow.enabled = FALSE

end event

event pfc_preopen;call super::pfc_preopen;this.of_SetPreference(TRUE)
this.inv_preference.of_SetToolBars(TRUE)
this.inv_preference.of_SetWindow(TRUE)
this.of_SetResize(TRUE)
this.inv_resize.of_SetOrigSize(this.workspacewidth(),this.workspaceheight())
inv_resize.of_Register(dw_user_change_password, "scale")
inv_resize.of_Register(cb_clear, "scale")
inv_resize.of_Register(cb_exit, "scale")
inv_resize.of_Register(cb_update, "scale")
//inv_resize.of_Register(cb_delete, "scale")



end event

event open;call super::open;this.of_SetBase(true)
this.inv_base.of_Center()
//This.X = 302
This.Y = 550
//This.Width = 2455
//This.Height = 1249
end event

type dw_user_change_password from u_dw within w_user_change_passwrd
event ue_enter_to_tab pbm_dwnprocessenter
integer x = 37
integer y = 32
integer width = 2523
integer height = 768
integer taborder = 0
string dataobject = "d_user_change_password"
boolean vscrollbar = false
boolean livescroll = false
end type

event ue_enter_to_tab;call super::ue_enter_to_tab;Send(Handle(this),256,9,Long(0,0))
RETURN 1
end event

event constructor;call super::constructor;string ls_userid
long li_cnt
String ls_null
setnull(ls_null)

ls_userid=trim(SQLserverTrans.userid)
this.of_settransobject(SqlServerTrans)
li_cnt=this.Retrieve(ls_userid)
if li_cnt=0 then
	messagebox('Error.','Invalid user.', Stopsign!)
	close(parent)
else
//	this.object.passwrd[1]=""
	// 12/15/2008 Tracker 2119
	this.object.new_passwrd[1]=ls_null
	this.object.passwrd1[1]=ls_null
	this.object.passwrd2[1]=ls_null
end if



end event

event itemchanged;call super::itemchanged;n_ds lds,lds2
String ls_userid, ls_passwordold,ls_pass, ls_char, ls_locked,ls_filter,check_password,ls_passchk
Boolean num_exist = FALSE, sp_char_exist = FALSE, last_try= FALSE,last_eleven_pwds=FALSE,repeated_chars=FALSE, lower_exist=FALSE,upper_exist=FALSE
Date ld_passwordchg,ld_today, ld_admin_date
DateTime ldt_last_logon,ldt_today
Time lt_last_logon,lt_current_time
Integer i, li_counter=0,rtn,ll_rows
int li_char
	
ld_today = Today()
ldt_today = datetime(Today(),Now())
lt_current_time = Now()

// Get the userid 
ls_userid=Trim(dw_user_change_password.object.userId[row])

//IF DWO.Name = 'passwrd' THEN
//12/15/2008 Tracker #2119 new password column and password rules implementation
IF DWO.Name = 'new_passwrd' THEN

	// Create and load the datastore for nls_password to get the previouse password and the date that was changed
	lds2 = CREATE n_ds
	lds2.dataObject = "d_nls_password"
	lds2.SetTransObject(SqlServerTrans)
 	lds2.Retrieve(ls_userid)
	lds2.SetSort('passwrdchg DESC')
	lds2.sort()
//       ls_passwordold = lds2.object.passwrd[1]

		// 02/26/2009 validate 
		select new_passwrd,passwrdchg, admin_passwrdchg
		into :ls_passwordold, :ld_passwordchg, :ld_admin_date
		from picsuser
		where userid = :ls_userid using sqlservertrans ;
		
		IF f_check_dberror(SqlServerTrans,"Selecting new password and passwrdchg from picsuser failed")=FALSE THEN
			RETURN 1
		END IF
		
   //   ld_passwordchg = date(lds2.object.passwrdchg[1]) // 04/15/09
		
	// If the password date change plus one day greater than todays date and also password date change less than or equal to todays date then 
	// you have tried to change your password twice in one day
	// 04/28/09 if admin reset the password allow them to change for that day.
	IF RelativeDate(ld_passwordchg, 1) > ld_today AND ld_passwordchg <= ld_today AND & 
		Isnull(ld_admin_date) THEN
		dw_user_change_password.object.new_passwrd.Validationmsg='Password failed.'+&
		'~nYou can not change your password more than once a day '
		RETURN 1
	END IF		
		 
	// Create and load the datastore for NLS_LOGON_HIST
	lds = CREATE n_ds
	lds.dataObject = "d_nls_lock"
	lds.SetTransObject(SqlServerTrans)
 	ll_rows = lds.Retrieve(ls_userid)	

	 
	IF ll_rows = -1 THEN
		Messagebox("Database Error","There was an error during the retrieve.  Please try again.")
		SetPointer(arrow!)
		RETURN
	ELSEIF ll_rows > 0 THEN
		// If records were found, sort them based on last logon dates and the counter
		lds.SetSort("date_logon Desc, counter Desc")	
		lds.Sort()
		ldt_last_logon = lds.object.date_logon[1]
		li_counter = lds.object.counter[1]
	END IF

       // save the old password 
	is_passworddata=Trim(Data)
	
	// Get the lock status of the user
	string ls_sha
	
	SELECT locked, SHA1_YN
	INTO :ls_locked,:ls_sha
	FROM picsuser
	WHERE userId=:ls_userid
	USING SqlServerTrans;
	
	// Select from dual the value that is type as current password
//	SELECT Trim(hash(:Data))

	IF ls_sha = 'Y' THEN
		// 06/24/09 #2214
		is_password = f_encrypt(data,data)
	ELSE
			SELECT Trim(f_hash(:Data))
			INTO :is_password
			FROM dual
			USING SqlServerTrans;
	END IF
	
       // If the last attempt was the third attempt then check the 5 minute interval
//       IF li_counter > 3 THEN
//		// Extract the time from the last logon attempt
//		lt_last_logon = Time(ldt_last_logon)
//		// add 5 minutes to last logon and check to see if it is bigger than current time
//		IF SecondsAfter(RelativeTime(lt_last_logon, 300), lt_current_time) > 0 THEN
//			last_try = TRUE
//		ELSE
//			last_try = FALSE			
//		END IF
//	END IF
		
	// If Account is locked and you may not have another shot at it.
	IF ls_locked = 'Y' AND last_try = FALSE THEN
		dw_user_change_password.object.new_passwrd.Validationmsg='Password failed.'+&
		'~nYour accout is locked. Please contact your system administrator. '
		RETURN 1
	// Compare what is typed against what exist in database
	ELSEIF is_password <> ls_passwordold THEN
		// If record exist in NLS_LOGON_HIST table and date_logon has a value and counter > 0
		IF ll_rows > 0  AND li_counter > 0 THEN
			// Check to see if last logon time is less then 5 minutes ago 
			IF li_counter >= 3 THEN
				// Increment the counter (more than three attempts)
				li_counter++
				ldt_today = datetime(Today(),Now())
				IF w_pics_main.password_must_change THEN
					INSERT INTO NLS_LOGON_HIST
					VALUES(:ls_userid,:is_password,:ldt_today,:ldt_today,:li_counter,'Denied')
					USING SqlServerTrans;
					IF f_check_dberror(SqlServerTrans,"Inserting rows into NLS_LOGON_HIST")=FALSE THEN
						ROLLBACK USING SqlServerTrans;
					ELSE
//						UPDATE PICSUSER
//						SET locked = 'Y'
//						WHERE userid = :ls_userid
//						USING SqlServerTrans;
//						IF f_check_dberror(SqlServerTrans,"Updating PICSUSER for user "+ls_userid)=FALSE THEN
//							ROLLBACK USING SqlServerTrans;
//						ELSE
//							COMMIT USING SqlServerTrans;
//						END IF
						COMMIT USING SqlServerTrans;
					END IF
//					dw_user_change_password.object.new_passwrd.Validationmsg='Password failed.'+&
//					'~nYour current password is incorrect. You have exceeded number of logon attempts. Account if locked, please contact your system administrator. '
					dw_user_change_password.object.new_passwrd.Validationmsg='Password failed.'+&
					'~nYour current password is incorrect. '
					RETURN 1
				ELSE
					INSERT INTO NLS_LOGON_HIST
					VALUES(:ls_userid,:is_password,:ldt_today,:ldt_today,:li_counter,'Denied')
					USING SqlServerTrans;
					IF f_check_dberror(SqlServerTrans,"Inserting rows into NLS_LOGON_HIST")=FALSE THEN
						ROLLBACK USING SqlServerTrans;
					ELSE
						COMMIT USING SqlServerTrans;
					END IF
					dw_user_change_password.object.new_passwrd.Validationmsg='Password failed.'+&
					'~nYour current password is incorrect. '
					RETURN 1					
				END IF
			ELSEIF li_counter = 2 THEN
				// Increment the counter (Third attempt)
				li_counter++
				ldt_today = datetime(Today(),Now())
//				IF w_pics_main.password_must_change THEN
//					INSERT INTO NLS_LOGON_HIST
//					VALUES(:ls_userid,:is_password,:ldt_today,NULL,:li_counter,'Denied')
//					USING SqlServerTrans;
//					IF f_check_dberror(SqlServerTrans,"Inserting rows into NLS_LOGON_HIST")=FALSE THEN
//						ROLLBACK USING SqlServerTrans;
//					ELSE
//						COMMIT USING SqlServerTrans;
//					END IF
//					dw_user_change_password.object.new_passwrd.Validationmsg='Password failed.'+&
//					'~nYour current password is incorrect. This is your third attempt. '
//					RETURN 1
//				ELSE
					INSERT INTO NLS_LOGON_HIST
					VALUES(:ls_userid,:is_password,:ldt_today,:ldt_today,:li_counter,'Denied')
					USING SqlServerTrans;
					IF f_check_dberror(SqlServerTrans,"Inserting rows into NLS_LOGON_HIST")=FALSE THEN
						ROLLBACK USING SqlServerTrans;
					ELSE
						COMMIT USING SqlServerTrans;
					END IF
					dw_user_change_password.object.new_passwrd.Validationmsg='Password failed.'+&
					'~nYour current password is incorrect.'
					RETURN 1
//				END IF					
			ELSEIF li_counter = 1 THEN
					// Increment the counter (Second attempt)
					li_counter++
//					IF w_pics_main.password_must_change THEN
//						INSERT INTO NLS_LOGON_HIST
//						VALUES(:ls_userid,:is_password,:ldt_today,NULL,:li_counter,'Denied')
//						USING SqlServerTrans;
//						IF f_check_dberror(SqlServerTrans,"Inserting rows into NLS_LOGON_HIST")=FALSE THEN
//							ROLLBACK USING SqlServerTrans;
//						ELSE
//							COMMIT USING SqlServerTrans;
//						END IF
//						dw_user_change_password.object.new_passwrd.Validationmsg='Password failed.'+&
//						'~nYour current password is incorrect. This is your second attempt. '
//						RETURN 1
//					ELSE
						INSERT INTO NLS_LOGON_HIST
						VALUES(:ls_userid,:is_password,:ldt_today,:ldt_today,:li_counter,'Denied')
						USING SqlServerTrans;
						IF f_check_dberror(SqlServerTrans,"Inserting rows into NLS_LOGON_HIST")=FALSE THEN
							ROLLBACK USING SqlServerTrans;
						ELSE
							COMMIT USING SqlServerTrans;
						END IF
						dw_user_change_password.object.new_passwrd.Validationmsg='Password failed.'+&
						'~nYour current password is incorrect.'
						RETURN 1					
//					END IF
			END IF
		ELSE // No records exist in NLS_LOGON_HIST table of the counter is 0
			// Increment the counter (First attempt)
			li_counter++
//			IF w_pics_main.password_must_change THEN
//				INSERT INTO NLS_LOGON_HIST
//				VALUES(:ls_userid,:is_password,:ldt_today,NULL,:li_counter,'Denied')
//				USING SqlServerTrans;
//				IF f_check_dberror(SqlServerTrans,"Inserting rows into NLS_LOGON_HIST")=FALSE THEN
//					ROLLBACK USING SqlServerTrans;
//				ELSE
//					COMMIT USING SqlServerTrans;
//				END IF
//				dw_user_change_password.object.new_passwrd.Validationmsg='Password failed.'+&
//				'~nYour current password is incorrect.'
//				RETURN 1
//			ELSE
				INSERT INTO NLS_LOGON_HIST
				VALUES(:ls_userid,:is_password,:ldt_today,:ldt_today,:li_counter,'Denied')
				USING SqlServerTrans;
				IF f_check_dberror(SqlServerTrans,"Inserting rows into NLS_LOGON_HIST")=FALSE THEN
					ROLLBACK USING SqlServerTrans;
				ELSE
					COMMIT USING SqlServerTrans;
				END IF
				dw_user_change_password.object.new_passwrd.Validationmsg='Password failed.'+&
				'~nYour current password is incorrect.'
				RETURN 1			
//			END IF
		END IF
		dw_user_change_password.SetColumn('passwrd')
	END IF

ELSEIF DWO.Name = 'passwrd1' THEN

       // If the length of new password less than 8
	IF Len(Data) < 8 THEN
		dw_user_change_password.object.passwrd1.Validationmsg='Password failed.'+&
		'~nPassword can not be less than 8 characters.'
		RETURN 1
		dw_user_change_password.SetColumn('passwrd')
	// If the password was changed today
	ELSEIF DaysAfter(ld_passwordchg, date(ldt_today)) <= 1 THEN
		dw_user_change_password.object.passwrd1.Validationmsg='Password failed.'+&
		'~nYou may change your password once a day.'
		RETURN 1
		dw_user_change_password.SetColumn('passwrd')
	END IF
	
	ls_pass = Trim(data) // 12/15/2008 password can have lower, upper case numeric Trim(Upper(Data))
	
	// If there exist at least 1 number in password
	FOR i=1 TO Len(ls_pass)
		ls_char =Mid(ls_pass,i,1)
		IF  (ls_char>= '0' AND ls_char<='9') THEN
			num_exist = TRUE
			EXIT
		END IF
	NEXT
	IF num_exist = FALSE THEN
		dw_user_change_password.object.passwrd1.Validationmsg ='Password failed.'+&
		'~nThere must be at least one numeric  in the password.' 
		RETURN 1
	END IF
	
	// If there exist at least 1 lower case in password 12/15/2008
	i=1
	FOR i=1 TO Len(ls_pass)
		ls_char =Mid(ls_pass,i,1)
		li_char = Asc(ls_char)
//		IF NOT Isnumber(ls_char) THEN
		IF li_char > 96 and li_char < 123 THEN // lower case values
//			IF  ls_char = Lower(ls_char) THEN
				lower_exist = TRUE
				EXIT
//			END IF
		END IF
	NEXT
	IF lower_exist = FALSE THEN
		dw_user_change_password.object.passwrd1.Validationmsg ='Password failed.'+&
		'~nThere must be at least one lower case character  in the password.' 
		RETURN 1
	END IF

	// If there exist at least 1 upper case in password 12/15/2008
	i=1

	FOR i=1 TO Len(ls_pass)
		ls_char =Mid(ls_pass,i,1)
		li_char = Asc(ls_char)
//		IF NOT Isnumber(ls_char) THEN // there might be special chars also it treats as upper case
		IF li_char > 64 and li_char < 91 THEN // upper case values
			upper_exist = TRUE
			Exit
//		ELSE
//			IF  ls_char = Upper(ls_char) THEN // there might be special chars also it treats as upper case 02/09/09
//				upper_exist = TRUE
//				EXIT
//			END IF
		END IF
	NEXT
	IF upper_exist = FALSE THEN
		dw_user_change_password.object.passwrd1.Validationmsg ='Password failed.'+&
		'~nThere must be at least one upper case character  in the password.' 
		RETURN 1
	END IF


//	// If there exist at least 1 special character in password
//	FOR i=1 TO Len(ls_pass)
//		ls_char =Mid(ls_pass,i,1)
//		IF  (ls_char >'Z' OR ls_char <'A') AND (ls_char >'9' OR ls_char< '0')THEN
//			sp_char_exist = TRUE
//			EXIT
//		END IF
//	NEXT
//	IF sp_char_exist = FALSE THEN
//		dw_user_change_password.object.passwrd1.Validationmsg ='Password failed.'+&
//		'~nThere must be at least one special character in the password.' 
//		RETURN 1
//	END IF
	
	// repeated characters
	repeated_chars=FALSE
	i=1
	FOR i=1 TO Len(ls_pass) - 1
		IF  Mid(ls_pass,i,1) = Mid(ls_pass,i+1,1)THEN
			repeated_chars = TRUE
			EXIT
		END IF
	NEXT
	IF repeated_chars = TRUE THEN
		dw_user_change_password.object.passwrd1.Validationmsg ='Password failed.'+&
		'~nPassword must not have repeated charaters.' 
		RETURN 1
	END IF

	// Select from dual the value that is type as new password
//	SELECT Trim(hash(:Data))

//	SELECT Trim(f_hash(:Data))
//	INTO :check_password
//	FROM dual
//	USING SqlServerTrans;

	// 06/24/09 #2214
	check_password = f_encrypt(data,data)
	
	// Create and load the datastore for nls_password
	lds2 = CREATE n_ds
	lds2.dataObject = "d_nls_password"
	lds2.SetTransObject(SqlServerTrans)
 	ll_rows = lds2.Retrieve(ls_userid)	
	i=1
//	IF ll_rows > 1 THEN
//		FOR i= 1 TO ll_rows
//			IF ll_rows <= 11 THEN
//			    ls_passchk = lds2.object.passwrd[i] 
//			    IF ls_passchk = check_password THEN
//					last_eleven_pwds=TRUE
//					EXIT
//			   END IF
//			END IF
//		NEXT
//	ELSEIF ll_rows = 1 THEN
//		ls_passchk = lds2.object.passwrd[1] 
//		IF ls_passchk = check_password THEN
//			last_eleven_pwds=TRUE
//		END IF
//	END IF

// 07/14/09 Tracker 2223 Password last 11 password rule implementation
integer li_password_count, li_cnt

SELECT count(*) into :li_cnt FROM
    (
     select *  from
       (select * from nls_passwrd_hist where userid=:ls_userid order by passwrdchg desc)
     where rownum < 12
    )
   WHERE sha1_yn = 'N' using sqlservertrans ;
	
	IF LI_CNT = 0 THEN
		 select count(*)  into :li_password_count
		 from
		  (
			select * from (
			select * from nls_passwrd_hist where userid = :ls_userid order by passwrdchg desc
			 ) where rownum < 12
			)
		  where passwrd = :check_password using sqlservertrans ;
	ELSE
		
		string ls_hash
		select f_hash(:data) into :ls_hash from dual using sqlservertrans ;
		ls_hash = trim(ls_hash)
		
		 select count(*)  into :li_password_count
		 from
		  (
			select * from (
			select * from nls_passwrd_hist where userid = :ls_userid order by passwrdchg desc
			 ) where rownum < 12
			)
		  where passwrd = :check_password OR Trim(passwrd) = :ls_hash using sqlservertrans ;
	END IF
		
	  IF li_password_count > 0 THEN
		last_eleven_pwds = TRUE
	  END IF
	  ////////////////////// 07/14/09 #2223 
	  
	IF last_eleven_pwds = TRUE THEN
		dw_user_change_password.object.passwrd1.Validationmsg ='Password failed.'+&
		'~nYou may not use any of the last eleven passwords that you have assigned.' 
		RETURN 1
	END IF
		
	
	cb_clear.enabled=TRUE
	is_passworddata=Trim(Data)
	
ELSEIF DWO.Name = 'passwrd2' THEN
	cb_clear.enabled = TRUE
	is_newpassworddata = Trim(Data)
	// if the retype of the new password fails
	IF is_newpassworddata <> is_passworddata THEN
	dw_user_change_password.object.passwrd2.Validationmsg='Password failed.'+&
		'~nIn order to confirm the new password, you need to type the correct password.'
		RETURN  1
	ELSE	
		// Select from dual the value that is type as new password
//		SELECT Trim(hash(:Data))

//		SELECT Trim(f_hash(:Data))
//		INTO :is_newpassworddata
//		FROM dual
//		USING SqlServerTrans;	

		// 06/24/09 #2214
		is_newpassworddata = f_encrypt(data,data)
	
		// Allow update to the datawindow
		password_changed = TRUE
		// Enable the update button
		cb_update.enabled=TRUE
	END IF
END IF
end event

event updatestart;call super::updatestart;OpenWithParm(w_pics_update_msg_box,"Updating PICS User Table, Please Wait...")

end event

event updateend;call super::updateend;close(w_pics_update_msg_box)
end event

event getfocus;call super::getfocus;//this.Object.new_passwrd.Edit.Password = "yes"
//this.Object.passwrd1.Edit.Password = "yes"
//this.Object.passwrd2.Edit.Password = "yes"
end event

type cb_exit from u_cb within w_user_change_passwrd
integer x = 1632
integer y = 816
integer taborder = 0
integer textsize = -10
string text = "E&xit"
end type

event clicked;call super::clicked;Int rtn
IF w_pics_main.password_must_change THEN
	rtn = Messagebox("Password Change","You have not changed your password yet. Do you want to exit application",question!,yesNo!,1)
	IF rtn = 1 THEN
		HALT
	ELSE
		Open(w_user_change_passwrd)
	END IF
ELSE
	ib_disableclosequery = TRUE
	Close(parent)
END IF



end event

type cb_clear from u_cb within w_user_change_passwrd
integer x = 1061
integer y = 816
integer taborder = 0
integer textsize = -10
string text = "&Clear"
end type

event clicked;call super::clicked;string ls_userid

ls_userid=sqlservertrans.userid
//dw_user_change_password.Reset()
//dw_user_change_password.Event pfc_addrow()
dw_user_change_password.Retrieve(ls_userid )
dw_user_change_password.SetFocus()
//dw_user_change_password.object.passwrd[1]=""
dw_user_change_password.object.new_passwrd[1]=""
dw_user_change_password.object.passwrd1[1]=""
dw_user_change_password.object.passwrd2[1]=""
//dw_user_change_password.object.userid[1]=sqlservertrans.userid
wf_disable_objects()
end event

type cb_update from u_cb within w_user_change_passwrd
integer x = 489
integer y = 816
integer textsize = -10
boolean enabled = false
string text = "&Update"
end type

event clicked;call super::clicked;string lmsg,null_str,ls_userid,ls_old_password
DateTime ldt_date
ldt_date = datetime(Today(),Now())

SetNull(lmsg)
SetNull(null_str)

dw_user_change_password.AcceptText()
//popup the message box and set pointer to hourglass
SetPointer(Hourglass!)

// If the flag is enabled set the datawindow field to what the new password is
IF password_changed THEN

	// Select from dual the value that is type as new password
//	SELECT Trim(hash(:is_passworddata))

//	SELECT Trim(f_hash(:is_passworddata))
//	INTO :ls_old_password
//	FROM dual
//	USING SqlServerTrans;
//	

// 06/24/09 #2214
	ls_old_password = f_encrypt(is_passworddata,is_passworddata)
	
	ls_userid = dw_user_change_password.object.userid[1]
//	dw_user_change_password.object.passwrd[1] = is_newpassworddata
	// 12/15/2008 Tracker Item 2119 NEW_PASSWRD	 column implementation
	dw_user_change_password.object.new_passwrd[1] = is_newpassworddata
END IF
IF dw_user_change_password.Update() = 1 THEN
	// Insert a row in NLS_LOGON_HIST with the new password
	INSERT INTO NLS_LOGON_HIST
	VALUES(:ls_userid,:is_newpassworddata,:ldt_date,NULL,0,'Success')
	USING SqlServerTrans;
	IF f_check_dberror(SqlServerTrans,"Inserting rows into NLS_LOGON_HIST")=FALSE THEN
		ROLLBACK USING SqlServerTrans;
	ELSE
		// Insert a row in NLS_PASSWRD_HIST table with the old password
		INSERT INTO NLS_PASSWRD_HIST
		VALUES(:ls_userid,:ls_old_password,:ldt_date,'Y') // #2223 07/21/09 SHA1_YN 
		USING SqlServerTrans;
		IF f_check_dberror(SqlServerTrans,"Inserting rows into NLS_PASSWRD_HIST")=FALSE THEN
			ROLLBACK USING SqlServerTrans;		
		ELSE
			update picsuser
			set passwrdchg =(select sysdate 	from dual), admin_passwrdchg=null, sha1_yn='Y'  // 06/26/09 2214 SHA1 04/28/09 admin reset date to null
			where userid = :ls_userid using sqlservertrans;

			COMMIT USING SqlServerTrans;
			MessageBox('Status','Password Update successful')
			 w_pics_main.password_must_change = FALSE
			 cb_exit.TriggerEvent(Clicked!)
			RETURN 1
		END IF		
	END IF
ELSE
	MessageBox('Status','Update Failed .. Contact Your DBA')
	ROLLBACK USING SqlServerTrans;
	RETURN 2
END IF
cb_clear.TriggerEvent(clicked!)


end event


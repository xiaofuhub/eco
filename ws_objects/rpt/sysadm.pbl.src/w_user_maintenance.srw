$PBExportHeader$w_user_maintenance.srw
forward
global type w_user_maintenance from w_sheet
end type
type cb_reset from u_cb within w_user_maintenance
end type
type dw_pics_release_notes from u_pics_dw within w_user_maintenance
end type
type dw_user_maintenance from u_dw within w_user_maintenance
end type
type cb_exit from u_cb within w_user_maintenance
end type
type cb_clear from u_cb within w_user_maintenance
end type
type cb_update from u_cb within w_user_maintenance
end type
type cb_delete from u_cb within w_user_maintenance
end type
end forward

global type w_user_maintenance from w_sheet
integer x = 283
integer y = 328
integer width = 2706
integer height = 2004
string title = "User Maintenance"
windowstate windowstate = maximized!
cb_reset cb_reset
dw_pics_release_notes dw_pics_release_notes
dw_user_maintenance dw_user_maintenance
cb_exit cb_exit
cb_clear cb_clear
cb_update cb_update
cb_delete cb_delete
end type
global w_user_maintenance w_user_maintenance

type variables
boolean password_changed=FALSE
string ls_password
end variables

forward prototypes
public subroutine wf_disable_objects ()
public function integer wf_checkusers (string l_username)
public function integer wf_checksysusers (string l_username)
public function integer wf_addmenu ()
public function string of_randompassword (integer al_len)
public function string of_randomconsonent ()
public function string of_randomspecial ()
public function string of_randomvowel ()
public function string of_randomnumber ()
public function integer of_securityrules (string as_password)
end prototypes

public subroutine wf_disable_objects ();cb_clear.Enabled = FALSE
cb_delete.Enabled = FALSE
cb_update.Enabled = FALSE
cb_reset.Enabled = FALSE
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


String ls_group, ls_menu[], ls_access[], ls_userid, ls_temp,ls_newpasswrd
Integer li_loop,lcount=0
boolean group_exist=FALSE
Date tday

tday = Today()

ls_group = dw_user_maintenance.GetItemString(1,"group_")
ls_userid = dw_user_maintenance.GetItemString(1,"userid")

select count(*)
into 	:lcount
from 	picsaccess
where userid = :ls_userid
using sqlservertrans;

// IF userid does not exist create its accesses.

IF lcount = 0 THEN
	//create a data store and read in values for the specific group
	datastore lds_datastore
	lds_datastore = CREATE datastore
	lds_datastore.DataObject = "d_group_access_datastore"
	lds_datastore.SetTransObject (SqlServerTrans)
	lds_datastore.Retrieve(ls_group)
	
	//FOR li_loop = 1 TO lds_datastore.RowCount()
	
	FOR li_loop = 1 TO lds_datastore.RowCount()
	 ls_menu[li_loop] = lds_datastore.GetItemString(li_loop,"menu")
	 ls_access[li_loop] = lds_datastore.GetItemString(li_loop,"access_")
	NEXT
	
	//Destroy the datastore
	DESTROY lds_datastore
	
	//First check to see if there is any data in the picsaccess table.
	//If data exists then delete.
			SELECT DISTINCT userid
			INTO :ls_temp
			FROM picsaccess
			WHERE userid = :ls_userid
			USING SqlServerTrans;
			
	IF SqlServerTrans.SqlCode = 0 THEN  //Delete the menu options
		  DELETE FROM picsaccess
		  WHERE userid = :ls_userid
		  USING SqlServerTrans;
	END IF
	
	
	
	//INSERT the menu items to the picsaccess table
	FOR li_loop = 1 TO UpperBound(ls_menu[])
		INSERT INTO picsaccess
						(userid, menu, access_)
		VALUES (:ls_userid, :ls_menu[li_loop], :ls_access[li_loop])
		USING SqlServerTrans;
		
	NEXT
	IF f_check_dberror(SqlServerTrans,'insert into picsaccess table')=true then
		COMMIT USING SqlServerTrans;
	end if
	
	// If this is a new user into PICS update the nls_passwrd_hist for the first time
	
//	ls_newpasswrd = dw_user_maintenance.object.passwrd[1]
	ls_newpasswrd = dw_user_maintenance.object.new_passwrd[1] //12/15/2008 #2119
	
//	insert into nls_passwrd_hist
//	values(lower(:ls_userid),trim(hash(:ls_newpasswrd)), :tday)
//	using sqlservertrans;
	// new password already hashed 12/15/2008
	insert into nls_passwrd_hist
	values(lower(:ls_userid),:ls_newpasswrd, :tday, 'Y') //#2223 SHA1_YN ADDITION 7/21/09
	using sqlservertrans;
	IF f_check_dberror(SqlServerTrans,'insert into nls_passwrd_hist table')=true then
		COMMIT USING SqlServerTrans;
	end if

	
END IF


RETURN 1




end function

public function string of_randompassword (integer al_len);// 06/29/09 Murali K. Random password generation 2214
// This logic is from the BARD
//1) name Randomchar:  input: string of chars output: random character from string
//2) name Randomupper:  input: char output: random upper or lower case char
//3) name: Randomvowel : output Randomupper(Randomchar("aeiouy"))
//4) name: Randomnumber: output Randomchar("0123456789")
//5) name: Randomspecial: output Randomchar("!@#$%^&*()><?":")
//6) name: Randomconsonent: output Randomupper(Randomchar("bcdfghjklmnpqrstvwxz"))
//7) name: Randompassword: output: return the following:
//case int(random number(bound 1-4)
//1:
// Randomspecial + Randomnumber + Randomconsonent+ Randomvowel  + Randomconsonent+ Randomconsonent+ Randomvowel  + Randomconsonent
//2: Randomconsonent+ Randomvowel  + Randomconsonent+ Randomconsonent+ Randomvowel  +
//Randomconsonent+ Randomspecial + Randomnumber
//3: Randomconsonent+ Randomvowel  + Randomconsonent+ Randomspecial + Randomnumber+ Randomconsonent+
//Randomvowel  + Randomconsonent

//4: Randomconsonent+ Randomvowel  + Randomconsonent+ Randomconsonent+ Randomvowel  + Randomconsonent+
//Randomnumber+ Randomspecial 

string ls_random_password



CHOOSE CASE int(Rand(4))
CASE 1
 	ls_random_password =  of_Randomspecial() + of_Randomnumber() + of_Randomconsonent()+ of_Randomvowel()  + of_Randomconsonent()+ of_Randomconsonent()+ of_Randomvowel()  + of_Randomconsonent()
CASE 2 
	ls_random_password = of_Randomconsonent()+ of_Randomvowel()  + of_Randomconsonent()+ of_Randomconsonent()+ of_Randomvowel()  + &
									of_Randomconsonent()+ of_Randomspecial() + of_Randomnumber()
CASE 3 
	ls_random_password = of_Randomconsonent()+ of_Randomvowel()  + of_Randomconsonent()+ of_Randomspecial() + of_Randomnumber()+ of_Randomconsonent()+ &
									of_Randomvowel()  + of_Randomconsonent()

CASE 4
	ls_random_password = of_Randomconsonent()+ of_Randomvowel()  + of_Randomconsonent()+ of_Randomconsonent()+ of_Randomvowel()  + of_Randomconsonent()+ &
									of_Randomnumber()+ of_Randomspecial() 
END CHOOSE

return ls_random_password
end function

public function string of_randomconsonent ();//Randomconsonent: output Randomupper(Randomchar("bcdfghjklmnpqrstvwxz"))
CHAR conso[1 TO 20] = &
{'b','c','d','f','g','h','j','k','l','m','n','p','q','r','s','t','v','w','x','z'}

int li_rand

li_rand = Rand(20)

RETURN Upper( conso[li_rand])
end function

public function string of_randomspecial ();
CHAR special[1 TO 13] = &
{'!','@','#','$','%','^','&','*','(',')','>','<','?'}

int li_rand

li_rand = Rand(13)

RETURN special[li_rand]
end function

public function string of_randomvowel ();
CHAR vowel[1 TO 6] = &
{'a','e','i','o','u','y'}

int li_rand

li_rand = Rand(6)

RETURN vowel[li_rand]
end function

public function string of_randomnumber ();
CHAR vowel[1 TO 10] = &
{'0','1','2','3','4','5','6','7','8','9'}

int li_rand

li_rand = Rand(10)

RETURN vowel[li_rand]
end function

public function integer of_securityrules (string as_password);String ls_passwordold,ls_pass, ls_char, ls_locked,ls_filter,check_password,ls_passchk
Boolean num_exist = FALSE, sp_char_exist = FALSE, last_try= FALSE,last_eleven_pwds=FALSE,repeated_chars=FALSE, lower_exist=FALSE,upper_exist=FALSE
n_ds lds2
String ls_userid
Integer li_rtn_code, i, li_char
long ll_rows



	ls_pass = Trim(as_password) 
	
	// If there exist at least 1 number in password
	i=1
	FOR i=1 TO Len(ls_pass)
		ls_char =Mid(ls_pass,i,1)
		IF  (ls_char>= '0' AND ls_char<='9') THEN
			num_exist = TRUE
			EXIT
		END IF
	NEXT
	IF num_exist = FALSE THEN
//		dw_user_maintenance.object.new_passwrd.Validationmsg ='Password failed.'+&
//		'~nThere must be at least one numeric  in the password.' 
		RETURN -1
	END IF
	
	// If there exist at least 1 lower case in password 12/15/2008
	i=1
	FOR i=1 TO Len(ls_pass)
		ls_char =Mid(ls_pass,i,1)
		li_char = Asc(ls_char)
		IF li_char > 96 and li_char < 123 THEN // lower case values
//		IF NOT Isnumber(ls_char) THEN
//			IF  ls_char = Lower(ls_char) THEN
				lower_exist = TRUE
				EXIT
	//		END IF
		END IF
	NEXT
	IF lower_exist = FALSE THEN
//		dw_user_maintenance.object.new_passwrd.Validationmsg ='Password failed.'+&
//		'~nThere must be at least one lower case character  in the password.' 
		RETURN -1
	END IF

	// If there exist at least 1 upper case in password 12/15/2008
	i=1
	FOR i=1 TO Len(ls_pass)
		ls_char =Mid(ls_pass,i,1)
		li_char = Asc(ls_char)
		IF li_char > 64 and li_char < 91 THEN // upper case values
//		IF NOT Isnumber(ls_char) THEN
	//		IF  ls_char = Upper(ls_char) THEN
				upper_exist = TRUE
				EXIT
		//	END IF
		END IF
	NEXT
	IF upper_exist = FALSE THEN
//		dw_user_maintenance.object.new_passwrd.Validationmsg ='Password failed.'+&
//		'~nThere must be at least one upper case character  in the password.' 
		RETURN -1
	END IF

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
//		dw_user_maintenance.object.new_passwrd.Validationmsg ='Password failed.'+&
//		'~nPassword must not have repeated charaters.' 
		RETURN -1
	END IF

	// 06/24/09 #2214
//	check_password = f_encrypt(ls_pass,ls_pass)
//	
//	// Create and load the ls_passstore for nls_password
//	lds2 = CREATE n_ds
//	lds2.dataObject = "d_nls_password"
//	lds2.SetTransObject(SqlServerTrans)
// 	ll_rows = lds2.Retrieve(ls_userid)	
//
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
//	IF last_eleven_pwds = TRUE THEN
////		dw_user_maintenance.object.new_passwrd.Validationmsg ='Password failed.'+&
////		'~nYou may not use any of the last eleven passwords that you have assigned.' 
//		RETURN -1
//	END IF
//

RETURN 1
end function

on w_user_maintenance.create
int iCurrent
call super::create
this.cb_reset=create cb_reset
this.dw_pics_release_notes=create dw_pics_release_notes
this.dw_user_maintenance=create dw_user_maintenance
this.cb_exit=create cb_exit
this.cb_clear=create cb_clear
this.cb_update=create cb_update
this.cb_delete=create cb_delete
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.cb_reset
this.Control[iCurrent+2]=this.dw_pics_release_notes
this.Control[iCurrent+3]=this.dw_user_maintenance
this.Control[iCurrent+4]=this.cb_exit
this.Control[iCurrent+5]=this.cb_clear
this.Control[iCurrent+6]=this.cb_update
this.Control[iCurrent+7]=this.cb_delete
end on

on w_user_maintenance.destroy
call super::destroy
destroy(this.cb_reset)
destroy(this.dw_pics_release_notes)
destroy(this.dw_user_maintenance)
destroy(this.cb_exit)
destroy(this.cb_clear)
destroy(this.cb_update)
destroy(this.cb_delete)
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

event resize;call super::resize;This.X = 302
This.Y = 105
This.Width = 2455
This.Height = 1249
end event

event pfc_postopen;call super::pfc_postopen;dw_user_maintenance.SetFocus()
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
inv_resize.of_Register(dw_user_maintenance, "scale")
inv_resize.of_Register(cb_clear, "scale")
inv_resize.of_Register(cb_exit, "scale")
inv_resize.of_Register(cb_update, "scale")
inv_resize.of_Register(cb_delete, "scale")
inv_resize.of_Register(cb_reset, "scale")



end event

type cb_reset from u_cb within w_user_maintenance
integer x = 786
integer y = 1720
integer width = 558
integer taborder = 30
integer textsize = -10
string text = "&Reset Password"
end type

event clicked;call super::clicked;//////////////////////////////////////////////////////////////////////////////
//
//   Event :	clicked for cb_reset button
//
//	Arguments:  none
//
//	Returns:  integer
//
//	Description: Reset password to a generic password for a locked user
//////////////////////////////////////////////////////////////////////////////
//	
//	By					Date 			Version 		Tracker
//  Murali K.		   02/10/2009					2119
//////////////////////////////////////////////////////////////////////////////
//

integer li_return
string ls_pwd, ls_user, ls_random
date ld_today

li_return = MessageBox("Confirm", 'Do you want to reset the password?',  Exclamation!, YesNo!, 2)

IF li_return = 1 THEN
//	select trim(f_hash('Pasword1')) into : ls_pwd from dual using sqlservertrans ;
	// 06/24/09 #2214
	int li_cnt=-1
	DO WHILE li_cnt = -1
			ls_random = of_randompassword(8)
			IF of_securityrules(ls_random) <> 1 THEN
				CONTINUE
			ELSE
				li_cnt=1
			END IF
	LOOP

	ls_pwd = f_encrypt(ls_random,ls_random)
	
	dw_user_maintenance.object.new_passwrd[1] = ls_pwd
	ls_user = dw_user_maintenance.object.userid[1]
	ld_today = Date(gnv_app.of_getsystemdatetime())
	
	//04/28/09 
	UPDATE PICSUSER
	set ADMIN_PASSWRDCHG = :ld_today, SHA1_YN='Y'
	where userid = :ls_user using sqlservertrans ;
	
//	Messagebox('Information','Password reset to ' + ls_random + '~r~n' + ' Click Update button to save and inform user')
	Openwithparm(w_reset_message, ls_random)
END IF

RETURN li_return
end event

type dw_pics_release_notes from u_pics_dw within w_user_maintenance
boolean visible = false
integer x = 73
integer y = 1696
integer width = 146
integer height = 96
integer taborder = 20
string dataobject = "d_pics_release_notes"
boolean vscrollbar = false
boolean livescroll = false
end type

event constructor;call super::constructor;this.of_settransobject(SqlServerTrans)

end event

type dw_user_maintenance from u_dw within w_user_maintenance
event ue_enter_to_tab pbm_dwnprocessenter
integer x = 41
integer y = 40
integer width = 2601
integer height = 1656
integer taborder = 10
string dataobject = "d_user_maintenance"
boolean vscrollbar = false
boolean livescroll = false
end type

event ue_enter_to_tab;call super::ue_enter_to_tab;Send(Handle(this),256,9,Long(0,0))
RETURN 1
end event

event constructor;call super::constructor;Datawindowchild	ldwc_userid

this.of_settransobject(SqlServerTrans)
this.Event pfc_addrow()

this.GetChild("userid",ldwc_userid)
this.Event pfc_PopulateDDDW("userid", ldwc_userid)

this.of_SetDropDownSearch(TRUE)
this.inv_dropdownsearch.of_AddColumn("userid")

end event

event itemchanged;call super::itemchanged;// 12/15/2008 Tracker 2119 - password rules validation use new_passwrd column
String ls_passwordold,ls_pass, ls_char, ls_locked,ls_filter,check_password,ls_passchk
Boolean num_exist = FALSE, sp_char_exist = FALSE, last_try= FALSE,last_eleven_pwds=FALSE,repeated_chars=FALSE, lower_exist=FALSE,upper_exist=FALSE

n_ds lds2
String ls_userid
Integer li_rtn_code, i, li_char
long ll_rows


IF DWO.Name = 'userid' THEN
	cb_clear.Enabled = TRUE
	ls_userid = TRIM(DATA)
   
	IF ls_userid<>'system' THEN
		IF wf_checksysusers(ls_userid) = 100 THEN
			dw_user_maintenance.Object.userid.validationmsg = " User not found "
			dw_user_maintenance.Event pfc_selectall()
			Return 1 
		ELSEIF wf_checkusers(ls_userid) =0 THEN
			cb_delete.Enabled = TRUE
			li_rtn_code = dw_user_maintenance.Retrieve(ls_userid)
			// If the user does exist DO NOT allow them to set the password
			IF li_rtn_code > 0 THEN
//				dw_user_maintenance.Object.passwrd.TabSequence = 0
				// 12/15/2008 Tracker Item 2119 - New passwrd change 
				dw_user_maintenance.Object.new_passwrd.TabSequence = 0
			ELSE
//				dw_user_maintenance.Object.passwrd.TabSequence = 20
				// #2119
				dw_user_maintenance.Object.new_passwrd.TabSequence = 20
				this.object.locked.visible=0
			END IF			
		ELSE
			// If the user does not exist allow them to set the password
			select count(*)
			into :li_rtn_code
			from picsuser
			where userid = :ls_userid
			using sqlservertrans;
			IF li_rtn_code > 0 THEN
//				dw_user_maintenance.Object.passwrd.TabSequence = 0
				// 12/15/2008 Tracker Item 2119 - New passwrd change 
				dw_user_maintenance.Object.new_passwrd.TabSequence = 0
			ELSE
//				dw_user_maintenance.Object.passwrd.TabSequence = 20
				// #2119
				dw_user_maintenance.Object.new_passwrd.TabSequence = 20
			END IF			
		END IF
	ELSE
		dw_user_maintenance.Retrieve(ls_userid)
	END IF
			
ELSEIF DWO.Name = 'passwrd'  OR DWO.Name = 'new_passwrd' THEN //  12/15/2008 #2119 
	
       // If the length of new password less than 8
	IF Len(Data) < 8 THEN
		dw_user_maintenance.object.new_passwrd.Validationmsg='Password failed.'+&
		'~nPassword can not be less than 8 characters.'
		RETURN 1
		dw_user_maintenance.SetColumn('passwrd')
	END IF
	
	ls_pass = Trim(data) // 12/15/2008 password can have lower, upper case numeric Trim(Upper(Data))
	
	// If there exist at least 1 number in password
	i=1
	FOR i=1 TO Len(ls_pass)
		ls_char =Mid(ls_pass,i,1)
		IF  (ls_char>= '0' AND ls_char<='9') THEN
			num_exist = TRUE
			EXIT
		END IF
	NEXT
	IF num_exist = FALSE THEN
		dw_user_maintenance.object.new_passwrd.Validationmsg ='Password failed.'+&
		'~nThere must be at least one numeric  in the password.' 
		RETURN 1
	END IF
	
	// If there exist at least 1 lower case in password 12/15/2008
	i=1
	FOR i=1 TO Len(ls_pass)
		ls_char =Mid(ls_pass,i,1)
		li_char = Asc(ls_char)
		IF li_char > 96 and li_char < 123 THEN // lower case values
//		IF NOT Isnumber(ls_char) THEN
//			IF  ls_char = Lower(ls_char) THEN
				lower_exist = TRUE
				EXIT
	//		END IF
		END IF
	NEXT
	IF lower_exist = FALSE THEN
		dw_user_maintenance.object.new_passwrd.Validationmsg ='Password failed.'+&
		'~nThere must be at least one lower case character  in the password.' 
		RETURN 1
	END IF

	// If there exist at least 1 upper case in password 12/15/2008
	i=1
	FOR i=1 TO Len(ls_pass)
		ls_char =Mid(ls_pass,i,1)
		li_char = Asc(ls_char)
		IF li_char > 64 and li_char < 91 THEN // upper case values
//		IF NOT Isnumber(ls_char) THEN
	//		IF  ls_char = Upper(ls_char) THEN
				upper_exist = TRUE
				EXIT
		//	END IF
		END IF
	NEXT
	IF upper_exist = FALSE THEN
		dw_user_maintenance.object.new_passwrd.Validationmsg ='Password failed.'+&
		'~nThere must be at least one upper case character  in the password.' 
		RETURN 1
	END IF

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
		dw_user_maintenance.object.new_passwrd.Validationmsg ='Password failed.'+&
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
	check_password = f_encrypt(Data,Data)
	
	// Create and load the datastore for nls_password
//	lds2 = CREATE n_ds
//	lds2.dataObject = "d_nls_password"
//	lds2.SetTransObject(SqlServerTrans)
// 	ll_rows = lds2.Retrieve(ls_userid)	
//
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
		dw_user_maintenance.object.new_passwrd.Validationmsg ='Password failed.'+&
		'~nYou may not use any of the last eleven passwords that you have assigned.' 
		RETURN 1
	END IF
		
//	select trim(hash(:data))
//	select trim(f_hash(:data))
//	into :ls_password
//	from dual
//	using sqlservertrans;
	
	// 06/24/09 #2214
	ls_password = check_password // f_encrypt(Data,Data)
	
	password_changed = TRUE
	
	
END IF//IF DWO.Name = 'userid' THEN

cb_update.Enabled = TRUE
end event

event itemfocuschanged;call super::itemfocuschanged;String null_string

SetNull(null_string)

IF DWO.Name = "userid" THEN
	wf_disable_objects()	
	cb_update.Enabled = TRUE
	dw_user_maintenance.SetItem(1,"userid",null_string)
	dw_user_maintenance.SetItem(1,"group_",null_string)
	dw_user_maintenance.SetItem(1,"userdesc",null_string)
	dw_user_maintenance.SetItem(1,"pics_msgs",null_string)
	
END IF
end event

event retrieveend;call super::retrieveend;string ls_userdesc, ls_locked

ls_userdesc = dw_user_maintenance.getitemstring(1,"userdesc")
ls_locked = this.object.locked[1]
IF NOT ISNULL(ls_userdesc) THEN
	Righttrim(ls_userdesc)
END IF

//#2119 12/17/2008 ONLY IF LOCKED BY ATTEMPTS ENABLE TO CHANGE
//IF ls_locked = 'Y' THEN
	this.object.locked.visible=1
	this.object.locked_t.visible=1
	cb_reset.enabled=	TRUE
//ELSE
//	this.object.locked.visible=0
//	this.object.locked_t.visible=0
//	cb_reset.enabled=	FALSE
//END IF
end event

event updatestart;call super::updatestart;OpenWithParm(w_pics_update_msg_box,"Updating PICS User Table, Please Wait...")

end event

event updateend;call super::updateend;close(w_pics_update_msg_box)
end event

event pfc_preupdate;call super::pfc_preupdate;//////////////////////////////////////////////////////////////////////////////
//
//	Event:  pfc_preupdate
//
//	Access:  dw
//
//	Arguments:  none
//
//	Returns:  integer 1 for success, -1 for failure
//
//	Description:  Update password expiry date in PICSUSER
//
//  Developed by : Mural iK.
//
//  Date : 02/09/2009
//////////////////////////////////////////////////////////////////////////////

date ld_expiry

IF this.rowcount() < 1 THEN
	RETURN 1
END IF

ld_expiry = this.object.passwrdchg[1]
IF Isnull(ld_expiry) THEN
	ld_expiry = Relativedate(date(gnv_app.of_getsystemdatetime()), 180)
	this.object.passwrdchg[1] = ld_expiry
END IF
RETURN 1
end event

event sqlpreview;call super::sqlpreview;//messagebox('rc', sqlsyntax)
end event

type cb_exit from u_cb within w_user_maintenance
integer x = 2281
integer y = 1720
integer taborder = 0
integer textsize = -10
string text = "E&xit"
end type

event clicked;call super::clicked;string ls_userid

ls_userid = dw_user_maintenance.object.userid[1]

IF ISNULL(ls_userid) or ls_userid = "" THEN
	ib_disableclosequery = TRUE
END IF
Parent.Event pfc_close()


//dw_user_maintenance.Reset()
//close(w_user_maintenance)
end event

type cb_clear from u_cb within w_user_maintenance
integer x = 1842
integer y = 1720
integer taborder = 0
integer textsize = -10
string text = "&Clear"
end type

event clicked;call super::clicked;dw_user_maintenance.Reset()
dw_user_maintenance.Event pfc_addrow()
dw_user_maintenance.object.locked.visible=0
dw_user_maintenance.SetFocus()

wf_disable_objects()
end event

type cb_update from u_cb within w_user_maintenance
integer x = 361
integer y = 1720
integer taborder = 0
integer textsize = -10
string text = "&Update"
end type

event clicked;call super::clicked;string lmsg,null_str,lver,ls_userid,ls_newpasswrd,ls_user
int ll_rows,li_rtn_code
date tday, ld_expiry
dwitemstatus l_status

tday = Today()

SetNull(lmsg)
SetNull(null_str)

dw_user_maintenance.AcceptText()
//popup the message box and set pointer to hourglass
SetPointer(Hourglass!)

lmsg = dw_user_maintenance.object.pics_msgs[1]
lver = dw_user_maintenance.object.pics_version[1]
ls_userid = Upper(dw_user_maintenance.object.userid[1])

ls_user = gnv_app.of_getuserid()

l_status = dw_user_maintenance.GetItemStatus(1,0,Primary!)

IF lmsg="" OR Isnull(lmsg) THEN
	dw_user_maintenance.object.pics_msgs[1]=null_str
END IF

IF ls_userid = 'SYSTEM'  THEN
	dw_pics_release_notes.SetTransObject(SqlServerTrans)
	ll_rows = dw_pics_release_notes.retrieve(lver)
	IF ll_rows = 0 THEN
		dw_pics_release_notes.InsertRow(0)
		dw_pics_release_notes.object.pics_version[1] = lver
		dw_pics_release_notes.object.pics_release_notes[1] = lmsg
	ELSE
		dw_pics_release_notes.object.pics_version[ll_rows] = lver
		dw_pics_release_notes.object.pics_release_notes[ll_rows] = lmsg
	END IF		
	IF dw_pics_release_notes.Update() = 1 THEN
		COMMIT USING SqlServerTrans;
	ELSE
		MessageBox('Status','Update PICS Release Notes Failed .. Contact Your DBA')
	END IF		
END IF
	

IF password_changed THEN
	//dw_user_maintenance.object.passwrd[1] = ls_password
	//#12/15/2008  #2119 new password changes
	dw_user_maintenance.object.new_passwrd[1] = ls_password
END IF


//Update the picsuser table. Also call wf_addemnu which will update
//the picsaccess table.

// 02/09/09 UPDATE PASSWORD EXPIRY DATE
ld_expiry = date(dw_user_maintenance.object.passwrdchg[1])
IF Isnull(ld_expiry) THEN
	ld_expiry = Relativedate(date(gnv_app.of_getsystemdatetime()), 180)
	dw_user_maintenance.object.passwrdchg[1] = ld_expiry
END IF

IF dw_user_maintenance.Update() = 1 THEN
	COMMIT USING SqlServerTrans;
	wf_addmenu()
	//Refresh the datawindow
	DataWindowChild  ldwc_pub
	w_user_maintenance.dw_user_maintenance.GetChild("userid",ldwc_pub)
	ldwc_pub.SetTransObject(SqlServerTrans)
	ldwc_pub.Retrieve()
	
	// audit column updates 02/19/09
	IF l_status = new! OR l_status = newmodified! THEN
		// if a new user passwrdchg should be set to system date 08/20/09
		UPDATE PICSUSER
		set created_by = :ls_user, created_date = SYSDATE, modified_by = :ls_user, modified_date = SYSDATE, pics_version = :lver, SHA1_YN='Y', passwrdchg=SYSDATE
		WHERE Upper(USERID)  = :ls_userid using sqlservertrans ;
	ELSEIF l_status = datamodified! THEN
		UPDATE PICSUSER
			set  modified_by = :ls_user, modified_date = SYSDATE
			WHERE Upper(USERID)  = :ls_userid using sqlservertrans ;
	END IF 
	
	COMMIT using sqlservertrans;
	
	MessageBox('Status','Update successful')
	RETURN 1
ELSE
	MessageBox('Status','Update Failed .. Contact Your DBA')
	ROLLBACK USING SqlServerTrans;
	RETURN 2
END IF


cb_clear.TriggerEvent(clicked!)


end event

type cb_delete from u_cb within w_user_maintenance
integer x = 1417
integer y = 1720
integer taborder = 0
integer textsize = -10
string text = "&Delete"
end type

event clicked;call super::clicked;  String ls_userid
  ls_userid = dw_user_maintenance.GetItemString(1,'userid')
  
  
 //Delete the user from the picsaccess table
 DELETE FROM picsaccess
 WHERE  picsaccess.userid = :ls_userid
 USING SqlServerTrans;

  IF SqlServerTrans.SqlCode = 0 THEN  
    
		  //Deletes the user from the picsuser table
		  DELETE FROM picsuser  
			WHERE picsuser.userid = :ls_userid 
		  USING SqlServerTrans;
		  
		  IF SqlServerTrans.SqlCode = 0 THEN
		 	MessageBox('Status','Successfully Deleted User')
		 	COMMIT USING SqlServerTrans;
		  ELSE
			 MessageBox('Status','Delete Unsuccessfull .. Contact Your DBA')
			 ROLLBACK USING SqlServerTrans;
			 RETURN
		  END IF
		
	ELSE
		MessageBox('Status','Delete Unsuccessfull .. Contact Your DBA')
		ROLLBACK USING SqlServerTrans;
      RETURN
   END IF


//Refresh the child datawindow
DataWindowChild  ldwc_pub
w_user_maintenance.dw_user_maintenance.GetChild("userid",ldwc_pub)
ldwc_pub.SetTransObject(SqlServerTrans)
ldwc_pub.Retrieve()


//Clear the datawindow for next input
cb_clear.TriggerEvent(Clicked!)
end event


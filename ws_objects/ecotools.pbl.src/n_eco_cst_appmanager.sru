$PBExportHeader$n_eco_cst_appmanager.sru
forward
global type n_eco_cst_appmanager from n_cst_appmanager
end type
end forward

global type n_eco_cst_appmanager from n_cst_appmanager
end type
global n_eco_cst_appmanager n_eco_cst_appmanager

forward prototypes
public function integer of_logondlg ()
public function integer of_splash (integer ai_secondsvisible)
public function integer of_about ()
public subroutine wf_disconnect_all ()
public function integer of_checkversion (string as_userid, string as_password, string as_version)
public function integer of_validateuser (string as_userid, string as_password)
end prototypes

public function integer of_logondlg ();//////////////////////////////////////////////////////////////////////////////
//
//	Function:  of_LogonDlg
//
//	Access:  public
//
//	Arguments:  none
//
//	Returns:  integer
//	 1 = successful logon
//	 0 = User cancelled from the logon dialog
//	-1 = an error occurred opening the logon window
//
//	Description:  Obtain a User ID and password from the user
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

n_cst_logonattrib	lnv_logonattrib

//////////////////////////////////////////////////////////////////////////////
// Load logon object values
//////////////////////////////////////////////////////////////////////////////
this.event pfc_prelogondlg (lnv_logonattrib)

//////////////////////////////////////////////////////////////////////////////
// Open logon window
//////////////////////////////////////////////////////////////////////////////
if OpenWithParm (w_eco_logon, lnv_logonattrib) < 0 then
	return -1
end if

//////////////////////////////////////////////////////////////////////////////
// Get return logon object
//////////////////////////////////////////////////////////////////////////////
lnv_logonattrib = message.powerobjectparm

//////////////////////////////////////////////////////////////////////////////
// Store user id
//////////////////////////////////////////////////////////////////////////////
if LenA (lnv_logonattrib.is_userid) > 0 then
	if of_IsRegistryAvailable() then
		RegistrySet (is_userkey + "\logon", "userid", lnv_logonattrib.is_userid)
	else
		SetProfileString (is_userinifile, "logon", "userid", lnv_logonattrib.is_userid)
	end if

	of_SetUserID (lnv_logonattrib.is_userid)
end if

return lnv_logonattrib.ii_rc

end function

public function integer of_splash (integer ai_secondsvisible);//////////////////////////////////////////////////////////////////////////////
//
//	Function:  of_Splash
//
//	Access:  public
//
//	Arguments:		
//	ai_secondsvisible   the length of time to display the splash window.
//
//	Returns:integer
//	 1 = success
//	-1 = error
//
//	Description:  Display the splash window for a specified length of time.
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

n_cst_splashattrib lnv_splashattrib

// Check arguments
If IsNull(ai_secondsvisible) Then
	ai_secondsvisible = 0
End If

// Number of seconds for splash window to be visible
lnv_splashattrib.ii_secondsvisible = ai_secondsvisible

// Populate information passed to the Splash window.
this.Event pfc_presplash (lnv_splashattrib)

Return OpenWithParm (w_eco_splash, lnv_splashattrib)

end function

public function integer of_about ();//////////////////////////////////////////////////////////////////////////////
//
//	Function:  of_About
//
//	Access:  public
//
//	Arguments:  none
//
//	Returns:  integer
//	 1 = success
//	-1 = error
//
//	Description:  Display the about window.
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

n_cst_aboutattrib lnv_aboutattrib

// Populate information passed to the About window.
this.Event pfc_preabout (lnv_aboutattrib)

Return OpenWithParm (w_eco_about, lnv_aboutattrib)

end function

public subroutine wf_disconnect_all ();// Disconnect all transactions
SQLServerTrans.of_disconnect()
//SQLServerOracleTrans.of_disconnect()
//SQLServerTrackerTrans.of_disconnect()
end subroutine

public function integer of_checkversion (string as_userid, string as_password, string as_version);//////////////////////////////////////////////////////////////////////////////////////////////////////////
//
//	Function:  of_checkversion
//  Args: String user id
//			String as_password
//			String as_version ( Version from the client code) e.g. 'Version 3.7', Version 4.0'
//	Description:
//	Call db function f_auth_picsuser to validate version, user id and password
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//	
//	Revision History
//
//	Developed by 	Date 				Version						Tracking#
//											5.0							#2225
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//


string dbname
string ls_cur_version,  v_cur_version_no, vs_ver
string ls_message, ls_msg

int li_cnt

 SELECT lower(cur_version_no), cur_version_no
          INTO :v_cur_version_no , :vs_ver
	FROM nl_software
      WHERE application = 'ECO' and active_yn = 'Y' using sqlservertrans;
				 
// declare rpcfunc in pfemain.pbl n_tr transaction object
// #2225 changes validate only version

li_cnt = sqlservertrans.F_AUTH_ECOUSER( as_version) 

IF sqlservertrans.sqlcode <> 0  THEN
	IF sqlservertrans.sqldbcode <> 0 THEN
			CHOOSE CASE sqlservertrans.sqldbcode
				CASE 20001
					ls_msg = 'Invalid Input! You must provide userid and password.'
				CASE 20002
					ls_msg = 'Invalid Software Version! You must upgrade your machine to Version '+  vs_ver
					Messagebox('Error', ls_msg)
					RETURN -1
				CASE 20004
					ls_msg = 'Uknown PICS software Version in Database!'
				CASE  ELSE
					ls_msg = sqlservertrans.sqlerrtext
			END CHOOSE
			Messagebox('Error', ls_msg)
			RETURN -1
		END IF
END IF

RETURN 1
end function

public function integer of_validateuser (string as_userid, string as_password);//////////////////////////////////////////////////////////////////////////////////////////////////////////
//
//	Function:  of_validateuser
//  Args: String user id
//			String as_password

//	Description:
//	Validate new SHA-1 or Old Hash password
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//	
//	Revision History
//
//	Developed by 	Date 				Version						Tracking#
//									
// Murali K.			07/27/2009      5.0							2225
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//

string ls_message, ls_msg,ls_en_pwd, ls_sha1_yn, ls_plain_pwd, ls_server_pwd
Long ll_return


// FETCH NEW SHA1 OR OLD PASSWORD, PLUS THE SHA1 SWITCH
 SELECT PASSWRD , SHA1_YN
 INTO :ls_en_pwd , :ls_sha1_yn
 FROM NLUSER
 WHERE USERID = :as_userid using sqlservertrans ;


 IF Isnull( ls_en_pwd) THEN // user not found
 	ll_return = 20003
ELSE
	IF ls_sha1_yn = 'N' OR Isnull(ls_sha1_yn) OR Len(Trim(ls_sha1_yn)) = 0 THEN 
		// old hash password comparison
		ls_server_pwd = as_password
		IF Isnull(ls_server_pwd) THEN
			ll_return = 20003
		ELSE
			IF Trim(ls_en_pwd) = Trim(ls_server_pwd) THEN
				RETURN 1
			ELSE
				ll_return = 20003
			END IF
		END IF
	ELSE
		ls_plain_pwd = f_encrypt(as_password, as_password) // new sha-1 password comparison
		IF  Trim(ls_plain_pwd) = Trim(ls_en_pwd) THEN
			RETURN 1
		ELSE
			ll_return = 20003
		END IF
	END IF

END IF

CHOOSE CASE ll_return
	CASE 20001
		ls_msg = 'Invalid Input! You must provide userid and password.'
	CASE 20003
		ls_msg ='Invalid Username/Password!'
		Messagebox('Error', ls_msg)
		gi_attempts++
		RETURN 2
END CHOOSE

RETURN 1
end function

on n_eco_cst_appmanager.create
call super::create
end on

on n_eco_cst_appmanager.destroy
call super::destroy
end on

event constructor;SQLServerTrans = CREATE n_tr
//////////////////////////////////////////////////////////////////////////////
// Get a handle to the application object
//////////////////////////////////////////////////////////////////////////////
iapp_object = GetApplication()

//////////////////////////////////////////////////////////////////////////////
// Populate the environment object
//////////////////////////////////////////////////////////////////////////////
GetEnvironment (ienv_object)

//////////////////////////////////////////////////////////////////////////////
// The following code can be implemented in descendants
//////////////////////////////////////////////////////////////////////////////
//// Name of the application
iapp_object.DisplayName="ECO Application"
//
//// Microhelp functionality
of_SetMicroHelp (true)
//
//// The file name of the application INI file
//of_SetAppIniFile ("c:\pics.ini")
//
//// The file name of the user INI file
//of_SetUserIniFile ("")
//
//// Application registry key
//of_SetAppKey ("")
//
//// User registry key
//of_SetUserKey ("")
//
//// The file name of the application's online help file
//of_SetHelpFile ("")
//
// The application version
//of_SetVersion ("Version 1.7")
of_SetVersion ("2.0")
//
//// The application logo (bitmap file name)
of_SetLogo ("nlsbphh.bmp")
//
//// Application copyright message
//of_SetCopyright ("")
//// IF appliocation stays idle for 1 hours (3600 seconds) we will close it
//Idle(3600)

end event

event pfc_close;call super::pfc_close;IF NOT SQLServerTrans.DBHandle() =0 THEN
	IF SQLServerTrans.of_disconnect() < 0 THEN
		inv_error.of_message("Error","Database Disconnect Error.")
	END IF
END IF
// If by any chances main window is still open, close it.
IF IsValid(w_eco_main) THEN
	close(w_eco_main)
END IF
end event

event pfc_logon;String ls_uid,ls_pwd,ls_username,ls_eco_int_env,ls_cur_version,ls_eco_ext_env,ls_trk_ext_env
String ls_eco_latest_version,ls_eco_msg,ls_message,synccmd
int rtn,lcnt

//ls_eco_int_env = "RS21nNL" 	// Internal reference section staff

//SQLServerTrans.dbms="ODBC"
// PB Oracle Native connection (net9nldb)
SQLServerTrans.DBMS ="O90 Oracle9i (9.0.1)"
SQLServerTrans.LogPass = "nldadmin"
SQLServerTrans.ServerName = "net9nldp"
SQLServerTrans.LogId = "nldadmin"
SQLServerTrans.AutoCommit = False
//SQLServerTrans.DBParm = ""
//SQLservertrans.DBParm = "CacheName = 'session.db'"
SQLServerTrans.userid = "nldadmin"
SQLServerTrans.dbpass = "nldadmin"

SQLserverTrans.of_connect()
IF SQLserverTrans.sqlcode <> 0 THEN
	IF SQLserverTrans.SqlDbcode = -951 THEN            //check for invalid userid
	  MessageBox("Login Error","Invalid User ID/Password. Make sure that net8nldb exist.",StopSign!)
	  Return -1	
   ELSEIF SQLserverTrans.SqlDBcode = -952 THEN       //check for invalid password
	  MessageBox("Login Error","Invalid User ID/Password. Make sure that net8nldb exist.",StopSign!)
	  Return -1
   Else                                             //check for other error messages
  	MessageBox("Database Connection Error","Unable to Connect. Make sure that net8nldb exist" +& 
	string(SQLserverTrans.sqldbcode) + " " +&
	SQLserverTrans.SQLErrText, &
	StopSign!)
   Return -1
  END IF
ELSE
	gnv_app.of_SetUserId(as_userid)
	
   // Get the current version from the user application	
	ls_cur_version = gnv_app.of_GetVersion()
	
////////////////// 07/27/09 Security rules implementation #2225
int li_attempts, li_ret
string ls_locked
date ld_admin_date

	SELECT LOCKED, ADMIN_PASSWRDCHG
	INTO :ls_locked, :ld_admin_date
	FROM NLUSER
	WHERE USERID = :as_userid using sqlservertrans;
	
	IF ls_locked = 'Y' THEN // sqlservertrans.sqlcode = 0 THEN
		Messagebox('Error', 'Your account is locked, Please contact system administrator')
		  wf_disconnect_all()	
		halt close
	END IF
	
	li_ret =  of_checkversion(as_userid,as_password, ls_cur_version) 
	IF li_ret <> 1 THEN
	    wf_disconnect_all()	
		halt close
	END IF
	
	li_ret = of_validateuser(as_userid,as_password)
	IF li_ret = 2 THEN 
		
		select parameter_value into :li_attempts from ref_system_control_parameters where parameter_name='login_attempts' using sqlservertrans ;
		
		IF gi_attempts =  li_attempts  THEN
			UPDATE NLUSER
			SET LOCKED = 'Y'
			WHERE USERID = :as_userid using sqlservertrans;
			
			IF sqlservertrans.sqlcode = 0 THEN
				Messagebox('Error', 'Your account is locked, Please contact system administrator')
			     wf_disconnect_all()	
				halt close
			END IF
		END IF
		 wf_disconnect_all()	
		RETURN -1
	ELSEIF li_ret <> 1 THEN
	    wf_disconnect_all()	
		halt close
	END IF
	////////////////////////////// 07/27/09 #2225
	
	RETURN 1
END IF
end event

event pfc_open;call super::pfc_open;INT rtn 
rtn = gnv_app.of_LogonDlg()
IF rtn = 0 THEN
	this.Event pfc_exit( )
ELSEIF rtn = -1 THEN
	this.Event pfc_open(as_commandline)
ELSE
	Open (w_eco_main)
END IF
end event


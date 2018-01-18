$PBExportHeader$n_pics_cst_appmanager.sru
forward
global type n_pics_cst_appmanager from n_cst_appmanager
end type
end forward

global type n_pics_cst_appmanager from n_cst_appmanager
end type
global n_pics_cst_appmanager n_pics_cst_appmanager

type prototypes


end prototypes

type variables
Boolean	ib_jaws

end variables

forward prototypes
public function integer of_splash (integer ai_secondsvisible)
public function boolean of_getjaws ()
public function boolean of_isjawsrunning ()
public function integer of_logondlg ()
public function integer of_setjaws (integer ab_jaws)
public subroutine wf_disconnect_all ()
public function integer of_about ()
public function integer of_checkversion (string as_userid, string as_password, string as_version)
public function datetime of_getsystemdatetime ()
end prototypes

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

Return OpenWithParm (w_pics_splash, lnv_splashattrib)


end function

public function boolean of_getjaws ();//////////////////////////////////////////////////////////////////////////////
//
//	Function:  of_GetJaws
//
//	Access:  public
//
//	Arguments:  none
//
//	Returns:  Boolean
//	True - Application has turn jaws functionality on
//	False - Application does not turn Jaws functionality on
//
//	Description:  Returns the current application's Jaws behavior.
//
//////////////////////////////////////////////////////////////////////////////
//	Author:	Mike Chamanara
//
//	Date:		3/1/02
//
//=============================================================================
//
//	Revision History
//
//	Date			Initials		Description of Change
//	----   		--------		--------------------
//	3/28/02		MC				Initial creation
//=============================================================================

return ib_Jaws
end function

public function boolean of_isjawsrunning ();Boolean lb_return
String  	ls_windowName

lb_return = FALSE

//Check if Window Eyes is Running
ls_windowName = 'Window-Eyes'
IF FindWindowW (0, ls_windowName) <> 0 THEN 	
	lb_return = TRUE
END IF

RETURN lb_return

end function

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
if OpenWithParm (w_pics_logon, lnv_logonattrib) < 0 then
	return -1
end if

//////////////////////////////////////////////////////////////////////////////
// Get return logon object
//////////////////////////////////////////////////////////////////////////////
lnv_logonattrib = message.powerobjectparm

//////////////////////////////////////////////////////////////////////////////
// Store user id
//////////////////////////////////////////////////////////////////////////////
if Len (lnv_logonattrib.is_userid) > 0 then
	if of_IsRegistryAvailable() then
		RegistrySet (is_userkey + "\logon", "userid", lnv_logonattrib.is_userid)
	else
		SetProfileString (is_userinifile, "logon", "userid", lnv_logonattrib.is_userid)
	end if

	of_SetUserID (lnv_logonattrib.is_userid)
end if

return lnv_logonattrib.ii_rc

end function

public function integer of_setjaws (integer ab_jaws);//////////////////////////////////////////////////////////////////////////////
//
//	Function:  of_SetJaws
//
//	Access:  public
//
//	Arguments:		
//	ab_jaws   enable/disable jaws function
//
//	Returns:  integer
//	 1 = success
//	-1 = error
//
//	Description:
//	Enables/disables microhelp for the application
//
//////////////////////////////////////////////////////////////////////////////
//	Author:	Mike Chamanara
//
//	Date:		3/1/02
//
//=============================================================================
//
//	Revision History
//
//	Date			Initials		Description of Change
//	----   		--------		--------------------
//	3/12/02		MC		Initial creation
//=============================================================================

// Check arguments
if IsNull (ab_jaws) then
	return -1
end if

if ab_jaws = 1 then
	ib_jaws = TRUE
elseif ab_jaws = -1 then
	ib_jaws = FALSE
else 
	ib_jaws = FALSE
end if

return 1
end function

public subroutine wf_disconnect_all ();// Disconnect all transactions
SQLServerTrans.of_disconnect()
SQLServerOracleTrans.of_disconnect()
//SQLServerTrackerTrans.of_disconnect()
end subroutine

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

Return OpenWithParm (w_pics_about, lnv_aboutattrib)

end function

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
//									
// Murali K.			11/25/2008      Phase -2  Ver 4.0	 		Tracker Item 2120
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//


string dbname
string ls_cur_version,  v_cur_version_no
string ls_message, ls_msg

// 11/25/2008 introduction of DB function pics_security.f_auth_picsuser
int li_cnt

 SELECT lower(cur_version_no)
          INTO :v_cur_version_no 
	FROM pics_software
      WHERE active_yn = 'Y' using sqlservertrans;
				 
// declare rpcfunc in pfemain.pbl n_tr transaction object
//li_cnt = sqlservertrans.F_AUTH_PICSUSER(as_userid,as_password, as_version) 

IF sqlservertrans.sqlcode <> 0  THEN
	IF sqlservertrans.sqldbcode <> 0 THEN
			CHOOSE CASE sqlservertrans.sqldbcode
				CASE 20001
					ls_msg = 'Invalid Input! You must provide userid and password.'
				CASE 20002
					ls_msg = 'Invalid Software Version! You must upgrade your machine to Version '+  v_cur_version_no
				CASE 20003
					ls_msg ='Invalid Username/Password!'
					Messagebox('Error', ls_msg)
					gi_attempts++
					RETURN 2
				CASE 20004
					ls_msg = 'Uknown PICS software Version in Database!'
			END CHOOSE
			Messagebox('Error', ls_msg)
			RETURN -1
		END IF
END IF

RETURN 1
end function

public function datetime of_getsystemdatetime ();//////////////////////////////////////////////////////////////////////////////
//
//	Function:  of_getsystemdatetime
//
//	Access:  public
//
//	Arguments:  none
//
//	Returns:  datetime from the server
//
//	Description:  Returns the server's date time
//
//  Developed by : Mural iK.
//
//  Date : 01/08/2009
//////////////////////////////////////////////////////////////////////////////

datetime ldt_time

select sysdate
into :ldt_time
from dual using sqlservertrans ;

RETURN ldt_time 

end function

on n_pics_cst_appmanager.create
call super::create
end on

on n_pics_cst_appmanager.destroy
call super::destroy
end on

event constructor;SQLserverTrans = CREATE n_tr
SqlServerOracleTrans = CREATE n_tr
//SqlServerTrackerTrans = CREATE n_tr
SqlDisplayMessage = CREATE n_tr
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
iapp_object.DisplayName="NLS-PICS Application"
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
//// The application version
//of_SetVersion ("Version 3.7")

// 01/25/2008 changed to 4.0 for pics phase 2
of_SetVersion ("Version 4.1")

//
//// The application logo (bitmap file name)
of_SetLogo ("c:\nlsbphh.bmp")
//
//// Application copyright message
of_SetCopyright ("")
//// IF appliocation stays idle for 1 hours (3600 seconds) we will close it
//Idle(3600)

end event

event pfc_logon;//////////////////////////////////////////////////////////////////////////////
//
//	Event:  pfc_logon
//
//	Arguments:
//	as_userid   User ID attempting to logon
//	as_password   Password of user attempting to logon
//
//	Returns:  integer
//	 1 = successful logon
//	-1 = failure
//
//	Description:  Specific logon functionality for the application.
//	Perform logon processing based on User ID and password given.
//
//	Note:  this event will be responsible for displaying any error messages
//	if the logon fails for any reason.
// 05/30/2008 Version Validation for regular users and sync and update PICS version
// Murali K. 11/25/2008 call version/user id/password validation method Tracker item 2120
//////////////////////////////////////////////////////////////////////////////

String ls_uid,ls_username,ls_picsenv,ls_latest_version,ls_cur_version
string ls_user_version,ls_pics_msg,ls_message,lpics_type,ls_trk_ext_env
string ls_oracle_ext_env,ltoday,ls_guid,synccmd
string ls_user_response, ls_locked

int rtn,lcnt, li_attempts
date lpics_rel_date, ld_admin_date

ltoday = string(today(),"mm/dd/yyyy")

// PB Oracle Native connection (net8pics)
SQLServerTrans.DBMS = "O90 Oracle9i (9.0.1)"
SQLServerTrans.LogPass = "pcsadmin"
//SQLServerTrans.LogPass = "pcsadmin1" // Tracker 2120 12/1/2008
SQLServerTrans.ServerName = "net9pics"
SQLServerTrans.LogId = "pcsadmin"
SQLServerTrans.AutoCommit = False
SQLServerTrans.DBParm = ""
SQLserverTrans.userid = as_userid
SQLserverTrans.dbpass = as_password

// PB Oracle Native connection (net8web)
SQLserverOracleTrans.DBMS = "O90 Oracle9i (9.0.1)"
SQLserverOracleTrans.LogPass = "picadmin"
SQLserverOracleTrans.ServerName = "net9web"
SQLserverOracleTrans.LogId = "picadmin"
SQLserverOracleTrans.AutoCommit = False
SQLserverOracleTrans.DBParm = ""
SQLserverOracleTrans.userid = "picadmin"
SQLserverOracleTrans.dbpass = "picadmin"

// Connect to PB native
SQLserverTrans.of_connect()
IF SQLserverTrans.sqlcode <> 0 THEN
	IF SQLserverTrans.SqlDbcode = -951 THEN            //check for invalid userid
	  	MessageBox("Login Error","Invalid User ID/Password. Please re-enter."+"~r~n"+ &
		  			"Or your password has expired, Please check with your system administrator.",StopSign!)
	  	Return -1	
   ELSEIF SQLserverTrans.SqlDBcode = -952 THEN       //check for invalid password
		MessageBox("Login Error","Invalid User ID/Password. Please re-enter."+"~r~n"+ &
					"Or your password has expired, Please check with your system administrator.",StopSign!)
		Return -1
   Else                                             //check for other error messages
		ls_message = "Unable to Connect to Oracle using net9pics. "
		MessageBox("Database Connection Error",ls_message+string(SQLserverTrans.sqldbcode) + " " +&
				SQLserverTrans.SQLErrText, &
				StopSign!)
		Return -1
  	END IF
ELSE // DB login is successful
	
	// comment user id /password validation done in db function tracker item 2120
//	lcnt = 0
//	// Check the validity of userid and password you typed against picsuser table
//	select count(*)
//	into :lcnt
//	from picsuser
//	where userid = :as_userid
//	and passwrd = trim(hash(:as_password))
//	using SQLServerTrans;
//	IF lcnt = 0 THEN
//	  MessageBox("Login Error","Invalid User ID/Password in RS21n. Please re-enter."+"~r~n"+ &
//	  				"Please check with your system administrator.",StopSign!)
//	  wf_disconnect_all()	  
//	  Return -1			
//	END IF

	
	gnv_app.of_SetUserId(as_userid)
   // Get the current version from the user application	
	ls_cur_version = gnv_app.of_GetVersion() // 05/30/2008 get it for the user who logged in
	
	//11/25/2008 call version/user id/password validation method Tracker Item 2120
	int li_ret
	
	// 02/09/09
	SELECT LOCKED, ADMIN_PASSWRDCHG
	INTO :ls_locked, :ld_admin_date
	FROM PICSUSER
	WHERE USERID = :as_userid using sqlservertrans;
	
	IF ls_locked = 'Y' THEN // sqlservertrans.sqlcode = 0 THEN
		Messagebox('Error', 'Your account is locked, Please contact system administrator')
		  wf_disconnect_all()	
		halt close
	END IF
//	
//	// 04/28/09
//	IF NOT Isnull(ld_admin_date) THEN
//		Messagebox('Warning', ' Your password has been reset. Please change your password now and re-login.')
//		open(w_user_change_passwrd)
//		halt close
//	END IF
	
	//12/17/2008 Tracker 2119 3 login attempt failure
	li_ret =  of_checkversion(as_userid,as_password, ls_cur_version) 
	IF li_ret = 2 THEN 
		
		select parameter_value into :li_attempts from ref_system_control_parameters where parameter_name='login_attempts' using sqlservertrans ;
		
		IF gi_attempts =  li_attempts  THEN
			UPDATE PICSUSER
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
	//////////////////
	
//	// Successful logon to pcsadmin, load the messages for db error
	gnv_app.inv_error.of_SetPredefinedSource (SQLServerTrans)
	gnv_app.inv_error.of_SetLogFile("c:\picserrlog.txt")
	gnv_app.inv_error.of_SetNotifySeverity(5)
	gnv_app.inv_error.of_LoadPredefinedMsg()
	//gnv_app.inv_error.of_setstyle(0)
	gnv_app.inv_error.of_SetStyle(gnv_app.inv_error.PFCWINDOW)
	// Connect to other oracle databases (PICS Web)
	SqlServerOracleTrans.of_connect()
	IF SqlServerOracleTrans.sqlcode <> 0 THEN
		IF SqlServerOracleTrans.SqlDbcode = -951 THEN            //check for invalid userid
			MessageBox("Login Error","Invalid User ID/Password for Oracle net9web.",StopSign!)
			Return -1	
		ELSEIF SqlServerOracleTrans.SqlDBcode = -952 THEN       //check for invalid password
			MessageBox("Login Error","Invalid User ID/Password for Oracle net9web.",StopSign!)
			Return -1
		Else                                             //check for other error messages
			MessageBox("Database Connection Error","Unable to Connect to Oracle using net9web." +& 
					string(SqlServerOracleTrans.sqldbcode) + " " +&
					SqlServerOracleTrans.SQLErrText, &
					StopSign!)
			// Return -1
		END IF
	END IF

	
	// Get a guid from system table for later usage
	
	SELECT SYS_GUID() 
	INTO :ls_guid
	FROM DUAL
	using SQLServertrans;

	
	// Check to see if this user is a Beta tester.
	select pics_type
	//, pics_version
	into :lpics_type
	//, :ls_cur_version
	from picsuser
	where userid = :as_userid
	using SQLServertrans;
	
	IF NOT(IsNull(lpics_type)) AND TRIM(lpics_type)<>"" THEN
		// Get the system Information
//		select pics_version,pics_msgs,pics_rel_date 
//		into :ls_latest_version,:ls_pics_msg,:lpics_rel_date
//		from picsuser
//		where userid = 'system'
//		using SQLServertrans;
		
		// 09/17/2008 disable sync since it will be done by ITS
/*		
		IF lpics_type = "B" THEN
			// This user is the Beta tester check for the latest version
			IF TRIM(ls_latest_version)<>"" AND IsNull(TRIM(ls_latest_version))=FALSE THEN
				ls_latest_version = TRIM(ls_latest_version)
				ls_cur_version = TRIM(ls_cur_version)
				IF ls_latest_version > ls_cur_version THEN
					//w_pics_logon.cb_sync.Visible=TRUE
					ls_message="New version is available for download.~r~n"
					ls_message=ls_message+"Todays date is: "+ltoday+"~r~n"
					ls_message=ls_message+ls_pics_msg+"~r~n"
					ls_message=ls_message+"Would you like to synchronize your pics to "+ls_latest_version+" from "+ls_cur_version
					//Open the messagebox for synchronization
					OpenWithParm(w_pics_upgrade_message, ls_message)		
					// Check text returned in Message object
					ls_user_response = Message.StringParm

					IF ls_user_response = "Yes" THEN
						synccmd = "C:\PICSORCL9i\sync C:\PICSORCL9i\picsorcl.syc"
						rtn = Run(synccmd)
						IF rtn = -1 THEN
							MessageBox("ERROR","ERROR Running the sync command.")
						ELSE
							//06/03/2008
							// Update the pics_version
							Update picsuser
							set pics_version = :ls_latest_version, pics_guid = :ls_guid
							where userid = :as_userid
							using SQLServertrans;
							If f_check_dberror(sqlservertrans,"PICSUSER") THEN
								commit using sqlservertrans;
							else
								rollback using sqlservertrans;
								messagebox("ERROR","Could not set the version number: "+ls_cur_version+" for the user: "+as_userid+".")
								halt
							end if
							////////////////////////////
						END IF
						halt
					
					END IF
				END IF
			END IF		
		ELSEIF lpics_type = "R" THEN
			// This is a regular user, see is the pics release date is pass the beta testing
			// period ( 3 days).
			IF RelativeDate(lpics_rel_date, 3) <= Today() THEN
				IF TRIM(ls_latest_version)<>"" AND IsNull(TRIM(ls_latest_version))=FALSE THEN
					ls_latest_version = TRIM(ls_latest_version)
					ls_cur_version = TRIM(ls_cur_version)
					IF ls_latest_version > ls_cur_version THEN
						//w_pics_logon.cb_sync.Visible=TRUE
// 05/30/2008
						ls_message="New version is available for download.~r~n~n"
						ls_message=ls_message+"Todays date is: "+ltoday+"~r~n~n"
						ls_message=ls_message+"This version includes:~r~n"
						ls_message=ls_message+ls_pics_msg+"~r~n"
						ls_message=ls_message+"~r~n"
						ls_message=ls_message+"Please check to see if any of these problem reports belongs to you.~r~nIf it does, test it for closing or reopening.~r~nYou may see more information about these PRs under menu help/Pics Problem Reports.~nThank you."
						ls_message=ls_message+"~r~n~n"
						ls_message=ls_message+"Would you like to synchronize your pics to "+ls_latest_version+" from "+ls_cur_version
						//Open the messagebox for synchronization
						OpenWithParm(w_pics_upgrade_message, ls_message)		
						// Check text returned in Message object
						ls_user_response = Message.StringParm
	
						IF ls_user_response = "Yes" THEN

							synccmd = "C:\PICSORCL9i\sync C:\PICSORCL9i\picsorcl.syc"
							rtn = Run(synccmd)
							IF rtn = -1 THEN
								MessageBox("ERROR","ERROR Running the sync command.")
							ELSE
								//06/03/2008
								// Update the pics_version
								Update picsuser
								set pics_version = :ls_latest_version, pics_guid = :ls_guid
								where userid = :as_userid
								using SQLServertrans;
								If f_check_dberror(sqlservertrans,"PICSUSER") THEN
									commit using sqlservertrans;
								else
									rollback using sqlservertrans;
									messagebox("ERROR","Could not set the version number: "+ls_cur_version+" for the user: "+as_userid+".")
									halt
								end if
								////////////////////////////
	

							END IF
							halt
							////////////////// begin  05/30/2008
						ELSE
							Messagebox('Error', ' Newer Version of Application need to be synced for Regular Users before using the PICS application')
							halt
							/////////////// end 05/30/2008
						END IF
					END IF
					// If latest version greater than current version
				END IF	
				// If latest version is not NULL
			END IF
			// If beta testing time is over.
		ELSE
			MessageBox("Warning","This userid = "+as_userid+" has a invalid type. "+"~r~nPlease ask the system administrator to correct your type.",Information!)
		END IF

		// Update the pics_version
		Update picsuser
		set pics_version = :ls_cur_version, pics_guid = :ls_guid
		where userid = :as_userid
		using SQLServertrans;
		If f_check_dberror(sqlservertrans,"PICSUSER") THEN
			commit using sqlservertrans;
		else
			rollback using sqlservertrans;
			messagebox("ERROR","Could not set the version number: "+ls_cur_version+" for the user: "+as_userid+".")
		end if
		*/ // 09/17/2008
		
	ELSE
		MessageBox("Warning","This userid = "+as_userid+" does not have any type associated with it. "+"~r~nPlease ask the system administrator to assign you a type.",Information!)
	END IF
	
	// Update the pics_version 11/25/2008 after succesfull login

	Update picsuser
	set  pics_version = :ls_cur_version, 
		pics_guid = :ls_guid
	where userid = :as_userid
	using SQLServertrans;
	If f_check_dberror(sqlservertrans,"PICSUSER") THEN
		commit using sqlservertrans;
	else
		rollback using sqlservertrans;
		messagebox("ERROR","Could not set the guid string: "+ls_guid+" for the user: "+as_userid+".")
	end if

	// If pics_type in not NULL	
	RETURN 1
END IF




end event

event pfc_open;call super::pfc_open;INT rtn , ijaws
boolean bjaws
string ls_filename

gnv_app.of_SetError(True)

//Enable the debug service
//gnv_app.of_SetDebug(TRUE)
//Enable the SQL Spy service
//gnv_app.inv_debug.of_SetSQLSpy(TRUE)
//Specify a log file for the SQL Spy service
//ls_filename = "c:\picsorcl9i\pics_sql.log"
//gnv_app.inv_debug.inv_SQLSpy.of_SetLogFile(ls_filename)


rtn = gnv_app.of_LogonDlg()
IF rtn = 0 THEN
	this.Event pfc_exit( )
ELSEIF rtn = -1 THEN
	this.Event pfc_open(as_commandline)
ELSE
	bjaws = of_isjawsrunning()
	if bjaws = TRUE then
		ijaws = 1
	else
		ijaws = 0
	end if
	of_setjaws(ijaws)
	Open (w_pics_main)
END IF
end event

event pfc_prelogondlg;call super::pfc_prelogondlg;////////////////////////////////////////////////////////////////////////////////////////////////////////////
////
////	Event: pfc_prelogondlg
////  
////	Description:
////	On double clicking the PICS.exe validate the current version within the code with
////  the table version info. If doesn't match stop the user from accessing the application
////////////////////////////////////////////////////////////////////////////////////////////////////////////
////	
////	Revision History
////
////	Developed by 	Date 				Version						Tracking#
////									
//// Murali K.			11/17/2008      Phase -2  Ver 4.0	 		Version Validation
////////////////////////////////////////////////////////////////////////////////////////////////////////////
////
//
//IF of_checkversion('test') = -1 THEN
//	halt close
//end if
end event


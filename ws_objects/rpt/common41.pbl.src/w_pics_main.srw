$PBExportHeader$w_pics_main.srw
forward
global type w_pics_main from w_frame
end type
end forward

global type w_pics_main from w_frame
integer x = 457
integer y = 248
integer width = 2912
integer height = 1944
string title = "NLS Main Window"
string menuname = "m_pics_main"
windowstate windowstate = maximized!
string icon = "C:\picsorcl9i\nls.ico"
boolean clientedge = true
event pics_microhelp ( )
event syscommand pbm_syscommand
end type
global w_pics_main w_pics_main

type variables
uo_external_function iuo_external_function

boolean ib_close,password_must_change=FALSE
string la_menu_name[],web_db_inst
end variables

forward prototypes
public subroutine wf_modify (ref str_menu menu_structure, menu menu_id, string menu_text, ref integer array_limit)
end prototypes

event syscommand;//if commandtype = 65136 then
//	 	ib_close = false
//else
//		ib_close = true
//end if
//
end event

public subroutine wf_modify (ref str_menu menu_structure, menu menu_id, string menu_text, ref integer array_limit);int li_j,li_i,li_loop
string ls_menu_string
// This is a recursive routine called by w_pics_main. It reads the menu items in in
// m_pics_main and checks the users profile to see if the menu items should be
// enabled or disabled.

li_j = Upperbound(menu_id.item[])

FOR li_i = 1 TO li_j
  ls_menu_string = menu_id.item[li_i].text
  ls_menu_string = uof_strip_menu(ls_menu_string)
  
  
  IF ls_menu_string <>''THEN
	array_limit++
	menu_structure.menu_array[array_limit] = menu_text+'.'+ls_menu_string
	 FOR li_loop = 1 TO UpperBound(la_menu_name[])
		IF TRIM(la_menu_name[li_loop]) = TRIM(menu_structure.menu_array[array_limit]) THEN
			menu_id.item[li_i].Enabled = TRUE
			EXIT
		ELSE
			menu_id.item[li_i].Enabled = FALSE
		END IF
	 NEXT// FOR li_loop = 1 TO ...
	// Walk this branch and find its children
	wf_modify(menu_structure,menu_id.item[li_i],&
	   menu_structure.menu_array[array_limit],array_limit)

  END IF
   
NEXT// FOR li_i = 1 TO li_j

end subroutine

on w_pics_main.create
call super::create
if IsValid(this.MenuID) then destroy(this.MenuID)
if this.MenuName = "m_pics_main" then this.MenuID = create m_pics_main
end on

on w_pics_main.destroy
call super::destroy
if IsValid(MenuID) then destroy(MenuID)
end on

event open;call super::open;iuo_external_function = create uo_external_function 
m_pics_main.m_file.m_print.Enabled 			=	FALSE
m_pics_main.m_file.m_pagesetup.Enabled 	=	FALSE
m_pics_main.m_file.m_printimmediate.Enabled = FALSE
m_pics_main.m_edit.m_deleterow.Enabled 	=	FALSE
m_pics_main.m_edit.m_addrow.Enabled 		=	FALSE
m_pics_main.m_edit.m_cut.Enabled 			=	FALSE
m_pics_main.m_edit.m_cut.Enabled 			=	FALSE
m_pics_main.m_edit.m_copy.Enabled 			=	FALSE
m_pics_main.m_edit.m_paste.Enabled 			=	FALSE
m_pics_main.m_edit.m_pastespecial.Enabled =	FALSE
m_pics_main.m_edit.m_clear.Enabled 			=	FALSE

end event

event pfc_postopen;call super::pfc_postopen;///////////////////////////////////////////////////////////////
// This event will check the userid of the user and enable the
// the menu items. If the userid is informix then all menu items
// are enabled. If the userid exists in the system but not in the
// pics user table then the application will terminate.
///////////////////////////////////////////////////////////////
	
	
	
	String  menu_text = "", ls_userid
	Integer li_loop,li_inner_loop,li_array_limit, li_maxrows
	str_menu menu_structure, menu_check

   w_pics_main.Event pfc_microhelp('Please Wait ...')
  
   OpenWithParm (w_pics_splash, pics_splash)

   //IF Userid is Informix then enable all Items.
  	ls_userid = gnv_app.of_GetUserId()
	IF TRIM(ls_userid) <> 'oracle' THEN
		
		//Create and populate the datastore d_user_access_datastore.
		datastore lds_datastore
		lds_datastore = CREATE datastore
		lds_datastore.DataObject = 'd_pics_main_datastore'
		lds_datastore.SetTransObject(SqlServerTrans)
		lds_datastore.Retrieve(ls_userid)
		
		li_maxrows = lds_datastore.RowCount()
		// If li_maxrows is zero then the user is not in picsuser table,
		// application will terminate.
		IF li_maxrows = 0 THEN
			MessageBox('Login Error','You have no access to the PICS application .. Contact Your Administrator',Stopsign!)
		   Close(this)
			RETURN
		END IF
				
		//copy all items that are enabled from the datastore to la_menu[] which
		//is an instance variable.
		FOR li_loop = 1 TO li_maxrows
		  la_menu_name[li_loop] = lds_datastore.GetItemString(li_loop,'menu')
	   NEXT
		//Destroy the datastore
		DESTROY lds_datastore
		
		//Call the function that will recursively enable and disable the menu items.
		  w_pics_main.wf_modify(menu_check,m_pics_main,menu_text,li_array_limit)
		

	END IF // (TRIM(ls_userid) <> 'informix'
	
	this.of_SetPreference(TRUE)
	this.inv_preference.of_SetToolBars(TRUE)
	this.inv_preference.of_SetWindow(TRUE)
	this.of_SetResize(TRUE)
	this.inv_resize.of_SetOrigSize(this.WorkSpaceWidth(),this.WorkSpaceHeight())
	timer(10)


   w_pics_main.Event pfc_microhelp('Ready')
  

end event

event pfc_preopen;call super::pfc_preopen;//////////////////////////////////////////////////////////////////////////////////////////////////////////
//
//	Event:  pfc_preopen  event
//
//	Description:
//	Pre Open settings for the window
//
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//	
//	Revision History
//
//	Developed by 	Date 				Version						Tracking#
//									
// Murali K.			06/03/2008      Phase -2  Ver 4.0	 		Version Validation
//																			Tester DB instances validation and Status bar DB Insance setting
// Murali K.			04/15/2009		Get no of password expiry days from system table #2119
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//


n_ds lds
string Luid,Luname,Lgroupid,Ltotname,ls_current_passwrd
string dbname,dbname2,lmsg,ls_locked, ls_type, ls_db
int rtn,days_left_before_expire,days_expired, li_expire
boolean not_matched=FALSE
date ld_passwordchg,ld_today,date_expired
datetime ldt_today,ldt_passwordchg, ldt_admin_date

ld_today = Today()
ldt_today = datetime(Today(),Now())

this.of_SetSheetManager(TRUE)
this.of_SetStatusbar(TRUE)
this.inv_statusbar.of_SetTimer(TRUE)
this.inv_statusbar.of_SetTimerInterval(60000)
this.inv_statusbar.of_SetTimerWidth(600)
this.inv_statusbar.of_SetTimerFormat("mmm dd, yyyy hh:mm AM/PM")
Luid = gnv_app.of_GetUserId()
select group_,userdesc, pics_type into :Lgroupid,:Luname,:ls_type from picsuser where userid=:Luid using sqlservertrans;
	
// This part of the code was added to check the database which is logged on
// If it is development or test change the title and background color.
select upper(name) into :dbname from v$database using sqlservertrans;
select upper(name) into :dbname2 from v$database using sqlserveroracletrans;

// Development instances
IF dbname = 'PCSD' and dbname2 = 'PICD' THEN
	not_matched=FALSE
	ls_db = 'DEVELOPMENT'
// Test instances
ELSEIF dbname = 'PCST' and dbname2 = 'PICT' THEN
	not_matched=FALSE
		ls_db = 'TESTING'
// Production instances
ELSEIF dbname = 'PCSP' and dbname2 = 'PICP' THEN
	not_matched=FALSE
	ls_db = 'PRODUCTION'
// Production Mirroring instances
ELSEIF dbname = 'PCSM' and dbname2 = 'PICM' THEN
	not_matched=FALSE
	ls_db = 'MIRROR'
ELSE
	not_matched=TRUE
END IF

web_db_inst = dbname2

// 06/03/2008 Tester validation - Tester can point only to test instance
IF ls_type = 'B' AND dbname <> 'PCST'  THEN
	MessageBox("Warning",'Testers must be pointing to Test Instance only, Please contact System Administrator')
	halt
END IF


IF not_matched THEN
	lmsg = "Your database instances are conflicting, "+dbname+" and "+dbname2+", Do you want to continue?"
	rtn = MessageBox("DB Instance conflict",lmsg,Question!,YesNo!,1)
	IF rtn = 2 THEN
		halt
	END IF
END IF
	
// Format User name.
Ltotname = '('+Luid+')'+Luname
// Display User name and group in status bar.
this.inv_statusbar.of_Register('username','text',Ltotname,650)
// 06/03/2008 display db name in plain english
this.inv_statusbar.of_Register('groupid','text','Group:'+Lgroupid+'  DB Instances: '+ls_db + ','+ dbname+','+dbname2,1400)

// Create and load the datastore for nls_password_hist
lds = CREATE n_ds
lds.dataObject = "d_nls_password"
lds.SetTransObject(SqlServerTrans)
lds.Retrieve(Luid)	
lds.SetSort("passwrdchg DESC")	
lds.Sort()
//ld_passwordchg = date(lds.object.passwrdchg[1])

//select locked,passwrd
//12/15/2008 Tracker 2119 use new _passwrd column
select locked,new_passwrd, passwrdchg, admin_passwrdchg
into :ls_locked,:ls_current_passwrd, :ldt_passwordchg, :ldt_admin_date
from picsuser
where userid = :Luid
using sqlservertrans;

ld_passwordchg = date(ldt_passwordchg)

int li_daysafter
li_daysafter = DaysAfter(ld_passwordchg, ld_today) 

	
// 04/28/09
IF NOT Isnull(ldt_admin_date) THEN
	Messagebox('Warning', ' Your password has been reset. Please change your password now and re-login.')
	open(w_user_change_passwrd)
	halt 
END IF
	
// 04/15/09 get expiry days from system table #2119
select parameter_value
into :li_expire
from ref_system_control_parameters
where parameter_name = 'password_expire' using sqlservertrans ;


IF ls_locked = 'Y' THEN
	// Account is lock contact system administrator
	lmsg = "Your account  is locked. PLease contact your system administrator"
	MessageBox("Account locked",lmsg,StopSign!)
	// Halt from the application
	HALT
ELSE
	// 02/09/2009 validate expiry of password against the expiry date
	IF DaysAfter(ld_passwordchg, ld_today) >= li_expire  OR IsNull(ld_passwordchg)THEN
//	IF ld_today > ld_passwordchg OR IsNull(ld_passwordchg)THEN
		// Password expired
		// Set this flag to make sure password will be changed
		password_must_change =TRUE
		
//		IF IsNull(ld_passwordchg) THEN
		IF password_must_change THEN // 02/09/09
			lmsg = "Your password expired. You must change your password?"
			rtn = MessageBox("Password change",lmsg,Question!,YesNo!,1)
			IF rtn = 1 THEN
				// Open the user change password screen
				Open(w_user_change_passwrd)
			ELSE
				// Halt from the application
				HALT
			END IF	
		ELSE
			// Days that password was expired
			days_expired = DaysAfter(ld_passwordchg, ld_today)  - li_expire
			// Date that password was expired
			date_expired = RelativeDate ( ld_passwordchg, li_expire )
			
			lmsg = "Your password was expired on "+string(date_expired)+". You changed your password on "+ string(ld_passwordchg)+". You must change your password"
			rtn = MessageBox("Password must be changed",lmsg,StopSign!,YesNo!,1)
			IF rtn = 1 THEN
				// Open the user change password screen
				Open(w_user_change_passwrd)
			ELSE
				// Halt from the application
				HALT
			END IF	
		END IF
	ELSEIF 		DaysAfter(ld_passwordchg, ld_today) >= (li_expire - 7)  and  DaysAfter(ld_passwordchg, ld_today) < li_expire THEN
		//abs(li_daysafter) > 1 AND abs(li_daysafter) <=7 THEN // 02/20/2009 CODE FIXED  
		// Password will be expired in less than seven days
		// Days left before the password is expired
		days_left_before_expire = li_expire - (DaysAfter(ld_passwordchg, ld_today))
		lmsg = "Your password will expire in "+string(abs(days_left_before_expire))+" day(s) "+". Do you want to change your password now?"
		rtn = MessageBox("Change Password",lmsg,Question!,YesNo!,1)
		IF rtn = 1 THEN
			Open(w_user_change_passwrd)
		END IF
	ELSE
		// Insert a row into nls_logon_hist with success logon
		INSERT INTO NLS_LOGON_HIST
		VALUES(:Luid,:ls_current_passwrd,:ldt_today,NULL,0,'Success')
		USING SqlServerTrans;
		IF f_check_dberror(SqlServerTrans,"Inserting rows into NLS_LOGON_HIST")=FALSE THEN
			ROLLBACK USING SqlServerTrans;
		ELSE
			COMMIT USING SqlServerTrans;
		END IF
	END IF
END IF


end event

event timer;call super::timer;//string lmsg,lmessage
//setnull(lmsg)
//
//select pics_msgs into :lmsg 
//from picsuser
//where group = 'ADMIN'
//and userid = 'ijen'
//using SQLDisplayMessage;
//
//if f_check_dberror(SQLDisplayMessage,"picsmsgs") Then
//	if lmsg<>"" OR NOT(IsNull(lmsg)) then
//		lmessage = 	"Message from system administrator:"+"~r~n~n"+ &
//						lmsg +"~r~n~n"+ &
//						"This message appears every ten seconds."
//		
//		MessageBox("***URGENT***",lmessage,StopSign!)
//	end if
//end if
end event

event close;call super::close;gnv_App.Post Event pfc_Exit()
end event

event closequery;call super::closequery;if ib_close = true then
	return 1
else
	return 0
end if
end event


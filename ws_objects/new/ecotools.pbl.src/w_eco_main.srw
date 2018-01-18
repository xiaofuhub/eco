$PBExportHeader$w_eco_main.srw
forward
global type w_eco_main from w_frame
end type
end forward

global type w_eco_main from w_frame
integer x = 457
integer y = 248
integer width = 2912
integer height = 1944
string title = "NLS Main Window"
string menuname = "m_eco_main"
windowstate windowstate = maximized!
end type
global w_eco_main w_eco_main

type variables
string la_menu_name[]
boolean password_must_change
end variables

on w_eco_main.create
call super::create
if IsValid(this.MenuID) then destroy(this.MenuID)
if this.MenuName = "m_eco_main" then this.MenuID = create m_eco_main
end on

on w_eco_main.destroy
call super::destroy
if IsValid(MenuID) then destroy(MenuID)
end on

event pfc_preopen;string Luid,Luname,Ltotname,dbname,lmsg,lver
boolean not_matched
int rtn
string ls_current_passwrd
string ls_locked, ls_type, ls_db
int days_left_before_expire,days_expired, li_expire
date ld_passwordchg,ld_today,date_expired
datetime ldt_today,ldt_passwordchg, ldt_admin_date

ld_today = Today()
ldt_today = datetime(Today(),Now())

this.of_SetSheetManager(TRUE)
this.of_SetStatusbar(TRUE)
//this.of_SetStatusbar(FALSE) // unsupported appeon feature // hanging in middle
this.inv_statusbar.of_SetTimer(TRUE)
this.inv_statusbar.of_SetTimerInterval(60000)
this.inv_statusbar.of_SetTimerWidth(650)
this.inv_statusbar.of_SetTimerFormat("mmm dd, yyyy hh:mm AM/PM")
Luid = gnv_app.of_GetUserId()
	
// This part of the code was added to check the database which is logged on
// If it is development or test change the title and background color.
//select upper(name) into :dbname from vdatabase using sqlservertrans;
select userdesc into :Luname from nluser where userid=:Luid using sqlservertrans;

lver = gnv_app.of_GetVersion()

// Format User name.
Ltotname = '('+Luid+')'+Luname
// Display User name and group in status bar.
this.inv_statusbar.of_Register('username','text',Ltotname,650)
this.inv_statusbar.of_Register('groupid','text',' '+lver+'  DB Instance '+dbname,900)

//messagebox('1',ltotname)
//messagebox('2',lver)
//messagebox('3',dbname)
gs_statusbar = 'username ' + Ltotname + 'groupid'+ ' '+lver+'  DB Instance '+dbname
//messagebox('4',gs_statusbar)

//  'username ' + Ltotname + 'groupid'+ ' '+lver+'  DB Instance '+dbname
//#2225 07/27/09 security rule implementation
select locked,passwrd, passwrdchg, admin_passwrdchg
into :ls_locked,:ls_current_passwrd, :ldt_passwordchg, :ldt_admin_date
from NLuser
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
	int li
	li = DaysAfter(ld_passwordchg, ld_today)
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
		IF sqlservertrans.sqlcode <> 0  THEN
			ROLLBACK USING SqlServerTrans;
		ELSE
			COMMIT USING SqlServerTrans;
		END IF
	END IF
END IF



end event

event pfc_postopen;call super::pfc_postopen;OpenSheet(w_eco_patron, w_eco_main, 0, Original!)
end event


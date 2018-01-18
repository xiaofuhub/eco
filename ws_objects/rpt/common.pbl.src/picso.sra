$PBExportHeader$picso.sra
$PBExportComments$Generated Application Object
forward
global type picso from application
end type
global transaction sqlca
global dynamicdescriptionarea sqlda
global dynamicstagingarea sqlsa
global error error
global message message
end forward

global variables
n_tr SQLServerTrans,SQLServerTrackerTrans,SQLServerOracleTrans,SQLServerNLTrans,SQLDisplayMessage
n_pics_cst_appmanager gnv_app
n_cst_error gnv_err
n_cst_splashattrib pics_splash
n_PowerPrinter nvo_PowerPrn
integer gi_fiscal_year
string ls_current_active_window
int gi_attempts=0
string gs_catalog

end variables
global type picso from application
string appname = "picso"
end type
global picso picso

type prototypes
FUNCTION ulong FindWindowW(ulong classname,string windowname) LIBRARY "USER32.DLL"
FUNCTION integer F_AUTH_PICUSER(string version) RPCFUNC

end prototypes

on picso.create
appname="picso"
message=create message
sqlca=create transaction
sqlda=create dynamicdescriptionarea
sqlsa=create dynamicstagingarea
error=create error
end on

on picso.destroy
destroy(sqlca)
destroy(sqlda)
destroy(sqlsa)
destroy(error)
destroy(message)
end on

event open;u_mutex		lnv_mutex

// Determine if previous instance of application is running.
If lnv_mutex.of_IsRunning() Then
	MessageBox('Warning', 'PICS application is already running.', StopSign!)
	Halt
Else
	gnv_app = CREATE n_pics_cst_appmanager
	gnv_err = CREATE n_cst_error
	gnv_err.of_SetStyle(1)
	gnv_app.Event pfc_Open(commandline)
	pics_splash.ii_secondsvisible =  1
	gnv_app.Event pfc_PreSplash(pics_splash)
	nvo_PowerPrn = CREATE n_PowerPrinter
	IF (nvo_Powerprn.of_Unlock("Library of Congress",159472) = FALSE) THEN
		MessageBox("PowerPrinter","Invalid Key number.")
	END IF
End If
end event

event idle;gnv_app.Event pfc_Idle()

end event

event systemerror;gnv_app.Event pfc_SystemError()

end event

event close;gnv_app.Event pfc_Close( )
DESTROY n_pics_cst_appmanager

end event


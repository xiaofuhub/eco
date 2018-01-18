$PBExportHeader$econew.sra
$PBExportComments$Generated Application Object
forward
global type econew from application
end type
global transaction sqlca
global dynamicdescriptionarea sqlda
global dynamicstagingarea sqlsa
global error error
global message message
end forward

global variables
n_tr SQLServerTrans
n_eco_cst_appmanager gnv_app
n_cst_splashattrib eco_splash
integer gi_attempts
string gs_statusbar

///////////////////////////// APPEON BEGIN ////////////////////////////	
//<Modification reason> Shared Variable is unsupported.
n_cst_dwsrv_property 	snv_property
///////////////////////////// APPEON END //////////////////////////////



end variables
global type econew from application
string appname = "econew"
end type
global econew econew

on econew.create
appname="econew"
message=create message
sqlca=create transaction
sqlda=create dynamicdescriptionarea
sqlsa=create dynamicstagingarea
error=create error
end on

on econew.destroy
destroy(sqlca)
destroy(sqlda)
destroy(sqlsa)
destroy(error)
destroy(message)
end on

event open;gnv_app = CREATE n_eco_cst_appmanager
gnv_app.Event pfc_Open(commandline)

end event

event close;gnv_app.Event pfc_close()
DESTROY gnv_app

end event

event systemerror;gnv_app.Event pfc_SystemError()
end event

event idle;gnv_app.Event pfc_idle()
end event


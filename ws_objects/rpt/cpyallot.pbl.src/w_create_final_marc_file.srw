$PBExportHeader$w_create_final_marc_file.srw
forward
global type w_create_final_marc_file from w_main
end type
type cb_log from commandbutton within w_create_final_marc_file
end type
type sle_user from singlelineedit within w_create_final_marc_file
end type
type st_6 from u_st within w_create_final_marc_file
end type
type sle_server from singlelineedit within w_create_final_marc_file
end type
type st_5 from u_st within w_create_final_marc_file
end type
type sle_pwd from singlelineedit within w_create_final_marc_file
end type
type st_4 from u_st within w_create_final_marc_file
end type
type sle_loc from singlelineedit within w_create_final_marc_file
end type
type st_3 from u_st within w_create_final_marc_file
end type
type sle_desc from singlelineedit within w_create_final_marc_file
end type
type st_2 from u_st within w_create_final_marc_file
end type
type st_1 from u_st within w_create_final_marc_file
end type
type dw_mchar_cabdt from u_pics_dw within w_create_final_marc_file
end type
type st_marc_file from u_st within w_create_final_marc_file
end type
type cb_cancel from commandbutton within w_create_final_marc_file
end type
type cb_go from commandbutton within w_create_final_marc_file
end type
type cb_marc_file from commandbutton within w_create_final_marc_file
end type
type gb_1 from groupbox within w_create_final_marc_file
end type
end forward

global type w_create_final_marc_file from w_main
integer width = 1568
integer height = 1164
string title = "Place the Final MARC File"
event type integer ue_create_file ( )
event ue_cancel ( )
event ue_dist_file ( )
event ue_all ( )
event ue_single ( )
event ue_enterkey pbm_dwnprocessenter
event ue_sel_file ( )
event ue_bklist ( )
cb_log cb_log
sle_user sle_user
st_6 st_6
sle_server sle_server
st_5 st_5
sle_pwd sle_pwd
st_4 st_4
sle_loc sle_loc
st_3 st_3
sle_desc sle_desc
st_2 st_2
st_1 st_1
dw_mchar_cabdt dw_mchar_cabdt
st_marc_file st_marc_file
cb_cancel cb_cancel
cb_go cb_go
cb_marc_file cb_marc_file
gb_1 gb_1
end type
global w_create_final_marc_file w_create_final_marc_file

type variables
str_distrib_schedule istr
datastore ids_distsched
long i_count=0
string is_yes_no='N',ls_mday,ls_cfile2,is_filename
Date ld_cabdt

end variables

forward prototypes
public function string wf_trpipe_tilde (string as_str)
end prototypes

event ue_cancel;call super::ue_cancel;close(this)
end event

event ue_dist_file();string ls_filename, ls_path, ls_today, ls_m, ls_d, ls_y, ls_cabdt
datetime ld_cabdt2

//ld_cabdt2=dw_mchar_cabdt.object.cabdt[1]
//
//ls_cabdt=string(ld_cabdt2,'mm/dd/yyyy')
//ls_m=mid(ls_cabdt,1,2)
//ls_d=mid(ls_cabdt,4,2)
//ls_y=mid(ls_cabdt,7,4)
ls_filename="marc"+ls_m+ls_y
//ls_filename = "distrib.txt"
ls_path =  ls_filename
if GetFileSaveName("Select MARC File",ls_path,ls_filename, "Txt", "Txt Files (*.txt),*.txt") <> 1 then
	messagebox("File Error","Incorrect file name, Please try again.")
	return
end if
is_filename=ls_filename
st_marc_file.text = ls_path
end event

public function string wf_trpipe_tilde (string as_str);long li_len, i
string ls_char, ls_rtn=''
li_len=len(as_str)
for i=1 to li_len
	ls_char=mid(as_str,i,1)
	choose case ls_char
		case '|', '\n'
		case '\~\'
			ls_rtn+='"'
		case else
			ls_rtn+=ls_char
	end choose
next
return ls_rtn
		
	

end function

on w_create_final_marc_file.create
int iCurrent
call super::create
this.cb_log=create cb_log
this.sle_user=create sle_user
this.st_6=create st_6
this.sle_server=create sle_server
this.st_5=create st_5
this.sle_pwd=create sle_pwd
this.st_4=create st_4
this.sle_loc=create sle_loc
this.st_3=create st_3
this.sle_desc=create sle_desc
this.st_2=create st_2
this.st_1=create st_1
this.dw_mchar_cabdt=create dw_mchar_cabdt
this.st_marc_file=create st_marc_file
this.cb_cancel=create cb_cancel
this.cb_go=create cb_go
this.cb_marc_file=create cb_marc_file
this.gb_1=create gb_1
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.cb_log
this.Control[iCurrent+2]=this.sle_user
this.Control[iCurrent+3]=this.st_6
this.Control[iCurrent+4]=this.sle_server
this.Control[iCurrent+5]=this.st_5
this.Control[iCurrent+6]=this.sle_pwd
this.Control[iCurrent+7]=this.st_4
this.Control[iCurrent+8]=this.sle_loc
this.Control[iCurrent+9]=this.st_3
this.Control[iCurrent+10]=this.sle_desc
this.Control[iCurrent+11]=this.st_2
this.Control[iCurrent+12]=this.st_1
this.Control[iCurrent+13]=this.dw_mchar_cabdt
this.Control[iCurrent+14]=this.st_marc_file
this.Control[iCurrent+15]=this.cb_cancel
this.Control[iCurrent+16]=this.cb_go
this.Control[iCurrent+17]=this.cb_marc_file
this.Control[iCurrent+18]=this.gb_1
end on

on w_create_final_marc_file.destroy
call super::destroy
destroy(this.cb_log)
destroy(this.sle_user)
destroy(this.st_6)
destroy(this.sle_server)
destroy(this.st_5)
destroy(this.sle_pwd)
destroy(this.st_4)
destroy(this.sle_loc)
destroy(this.st_3)
destroy(this.sle_desc)
destroy(this.st_2)
destroy(this.st_1)
destroy(this.dw_mchar_cabdt)
destroy(this.st_marc_file)
destroy(this.cb_cancel)
destroy(this.cb_go)
destroy(this.cb_marc_file)
destroy(this.gb_1)
end on

event pfc_postopen;call super::pfc_postopen;int ll_rows

//dw_mchar_cabdt.SetTransObject(SqlServerTrans)
//ll_rows = dw_mchar_cabdt.Retrieve()
//
//IF ll_rows > 0 THEN
//	ld_cabdt = date(dw_mchar_cabdt.object.cabdt[1])
//	ls_mday = string(ld_cabdt,'mmmmyyyy')
//	ls_cfile2 = 'Copy Allotment Batch '+ls_mday
//	sle_desc.text = ls_cfile2
//END IF

end event

type cb_log from commandbutton within w_create_final_marc_file
boolean visible = false
integer x = 553
integer y = 940
integer width = 411
integer height = 96
integer taborder = 110
boolean bringtotop = true
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "&PSCP Error Log"
end type

event clicked;//run('notepad.exe pscperr.log')
integer li_FileNum
STRING lserr

li_FileNum = FileOpen("PSCPERR.LOG", LineMode!, Read!, Shared!, Replace!)

IF li_filenum > 0 THEN
	fileread(li_filenum,lserr)
	if Len(trim(lserr)) > 0 THEN
		messagebox('PSCP Error Log Details', lserr)
	END IF
END IF
end event

type sle_user from singlelineedit within w_create_final_marc_file
integer x = 146
integer y = 460
integer width = 768
integer height = 80
integer taborder = 50
integer textsize = -8
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 33554432
long backcolor = 65535
string text = "pics"
borderstyle borderstyle = stylelowered!
end type

type st_6 from u_st within w_create_final_marc_file
integer x = 146
integer y = 388
integer width = 443
integer height = 76
integer textsize = -10
string text = "Account"
end type

type sle_server from singlelineedit within w_create_final_marc_file
integer x = 146
integer y = 304
integer width = 1335
integer height = 80
integer taborder = 40
integer textsize = -8
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 33554432
long backcolor = 65535
string text = "rs21n.loc.gov"
borderstyle borderstyle = stylelowered!
end type

type st_5 from u_st within w_create_final_marc_file
integer x = 146
integer y = 232
integer width = 443
integer height = 76
integer textsize = -10
string text = "Target Server"
end type

type sle_pwd from singlelineedit within w_create_final_marc_file
integer x = 146
integer y = 632
integer width = 768
integer height = 80
integer taborder = 60
integer textsize = -8
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 33554432
long backcolor = 65535
boolean password = true
integer limit = 10
borderstyle borderstyle = stylelowered!
end type

type st_4 from u_st within w_create_final_marc_file
integer x = 146
integer y = 556
integer width = 443
integer height = 80
integer textsize = -10
string text = "Password"
end type

type sle_loc from singlelineedit within w_create_final_marc_file
integer x = 146
integer y = 808
integer width = 1335
integer height = 80
integer taborder = 70
integer textsize = -8
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 33554432
long backcolor = 65535
string text = "/pics/prd/htdocs/download/marcfile/"
borderstyle borderstyle = stylelowered!
end type

type st_3 from u_st within w_create_final_marc_file
integer x = 146
integer y = 732
integer width = 443
integer height = 80
integer textsize = -10
string text = "Target Location"
end type

type sle_desc from singlelineedit within w_create_final_marc_file
boolean visible = false
integer x = 146
integer y = 1136
integer width = 1349
integer height = 80
integer taborder = 80
integer textsize = -8
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 33554432
borderstyle borderstyle = stylelowered!
end type

type st_2 from u_st within w_create_final_marc_file
boolean visible = false
integer x = 146
integer y = 1060
integer width = 338
integer height = 80
integer textsize = -10
string text = "Description"
end type

type st_1 from u_st within w_create_final_marc_file
boolean visible = false
integer x = 247
integer y = 60
integer width = 338
integer height = 68
integer textsize = -10
string text = "Batch Date:"
end type

type dw_mchar_cabdt from u_pics_dw within w_create_final_marc_file
boolean visible = false
integer x = 590
integer y = 40
integer width = 421
integer height = 112
integer taborder = 10
string dataobject = "d_mchar_cabdt"
boolean vscrollbar = false
end type

event itemchanged;call super::itemchanged;this.accepttext()
IF dwo.name = "cabdt" THEN
	ld_cabdt = date(dw_mchar_cabdt.object.cabdt[row])
	ls_mday = string(ld_cabdt,'mmmmyyyy')
	ls_cfile2 = 'Copy Allotment Batch '+ls_mday
	sle_desc.text = ls_cfile2
END IF

end event

type st_marc_file from u_st within w_create_final_marc_file
integer x = 183
integer y = 116
integer width = 814
integer height = 72
integer taborder = 20
long backcolor = 1090519039
boolean border = true
borderstyle borderstyle = stylelowered!
end type

type cb_cancel from commandbutton within w_create_final_marc_file
integer x = 809
integer y = 944
integer width = 247
integer height = 96
integer taborder = 100
boolean bringtotop = true
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "&Close"
boolean cancel = true
end type

event clicked;ib_disableclosequery=TRUE
close(parent)
end event

type cb_go from commandbutton within w_create_final_marc_file
integer x = 352
integer y = 944
integer width = 247
integer height = 96
integer taborder = 90
boolean bringtotop = true
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "&Go"
end type

event clicked;DateTime ld_cur,ld_cabdt_dt
String ls_cfile,ls_cfile3
String ls_ftpsite, ls_ftpuid, ls_ftppwd,ls_remoteloc,lmsg
Int li_ftp_rc, lcnt=0, rtn
string ls_pscp, ls_file, ls_bfile

// Removed GOTO statement unsupported appeon feature 3/24/2010
//ld_cabdt_dt=dw_mchar_cabdt.object.cabdt[1]
//ld_cur=DateTime(Today(),Now())

ls_cfile = st_marc_file.text

IF (ls_cfile = "" OR ls_cfile = "none") THEN
	Messagebox('MARC extract','Please choose a MARC file name.')
	st_marc_file.SetFocus()
	RETURN 
END IF

//12/02/2009 validations #2246
IF (sle_server.text = "" ) THEN
	Messagebox('MARC extract','Please enter a server  name.')
	sle_server.SetFocus()
	RETURN 
END IF	

IF (sle_user.text = "" ) THEN
	Messagebox('MARC extract','Please enter an account name.')
	sle_user.SetFocus()
	RETURN 
END IF	

IF (sle_pwd.text = "" ) THEN
	Messagebox('MARC extract','Please enter  password.')
	sle_pwd.SetFocus()
	RETURN 
END IF	

IF (sle_loc.text = "" ) THEN
	Messagebox('MARC extract','Please enter a target location.')
	sle_loc.SetFocus()
	RETURN 
END IF	

////////////// 12/02/2009

//SELECT ftp_site, ftp_uid, ftp_pwd
//INTO :ls_ftpsite, :ls_ftpuid, :ls_ftppwd
//FROM PCS_FTP_INFO
//USING sqlservertrans;
//IF f_check_dberror(sqlservertrans,"SELECTING FROM PCS_FTP_INFO ") THEN

//	li_ftp_rc = w_create_final_marc_file.ole_ezftp.uf_login (ls_ftpsite, ls_ftpuid, ls_ftppwd)
	
//	IF li_ftp_rc = -1 THEN
//		Messagebox("FTP Error", "Unable to connect to ftp server for placing the MARC file.")
//	ELSE
		ls_remoteloc = Trim(sle_loc.text)  //  "/pics/prd/htdocs/download/marcfile/"
		ls_cfile2 = sle_desc.text
		// Marc filename
		ls_cfile3 = Trim(f_remove_all_spaces(sle_desc.text))

		SELECT COUNT(*) INTO :lcnt
		FROM MARCFILES 
		WHERE Trim(marcfilename) = :ls_cfile3
		USING sqlserveroracletrans;
		IF f_check_dberror(sqlserveroracletrans,'Selecting from MARCFILE table')=FALSE THEN 		
			ROLLBACK USING sqlserveroracletrans;
			RETURN
		END IF			
		
		IF lcnt > 0 THEN
			// Marcfile exist 
			lmsg = 'Marcfile '+ls_cfile3+' exist, do you want to replace it?'
			rtn = Messagebox("MARC FILE EXIST",lmsg,question!,yesNo!,1)
			IF rtn = 1 THEN
				DELETE FROM MARCFILES
				WHERE Trim(marcfilename) = :ls_cfile3
				USING sqlserveroracletrans;
				
				IF f_check_dberror(sqlserveroracletrans,'Deleteing from MARCFILE table')=FALSE THEN 		
					ROLLBACK USING sqlserveroracletrans;
					RETURN
				ELSE
					COMMIT USING sqlserveroracletrans;
				END IF			
				

// code commented 12/2/2009			
/*
			// Marcfile does not exist
			w_create_final_marc_file.ole_ezftp.uf_set_currentdirectory(ls_remoteloc)
			w_create_final_marc_file.ole_ezftp.uf_upload (ls_cfile, ls_cfile3 , FALSE)
*/
	/////
	
			// Add PSCP call 12/2/2009
			ls_file = 'c:\progra~~1\putty\pscp.exe'
			IF Fileexists(ls_file) THEN
				ls_file = 'c:\progra~~1\putty\pscp.exe'
			ELSE
				ls_file = 'pscp.exe'
				IF NOT Fileexists(ls_file) THEN
					Messagebox('Error', ' PUTTY ->PSCP File Transfer Utility not found, Make sure it is installed and in the system path. Contact System Administrator')
					RETURN
				END IF
			END IF
			
//			ls_pscp = ls_file + '  -pw ' + Trim(sle_pwd.text)  + ' ' + ls_cfile  + ' ' + Trim(sle_user.text) + '@' + Trim(sle_server.text) + ':' + ls_remoteloc   //+ ' > pscperr.log' //  ' pics@rs21n.loc.gov:' +  // Tr0uble1

			ls_bfile = 'c:\progra~~1\picsorcl9i\pscprun.bat'
			IF Fileexists(ls_bfile) THEN
			ELSE
				ls_bfile = 'pscprun.bat'
				IF NOT Fileexists(ls_bfile) THEN
					Messagebox('Error', 'PSCPRUN.BAT dos batch file not found. Contact System Administrator')
					RETURN
				END IF
			END IF
			
			ls_pscp = ls_bfile  +  ' ' + Trim(sle_pwd.text)  + '  "' +ls_cfile  + '"  ' + Trim(sle_user.text) +  ' ' + Trim(sle_server.text) + ' ' + ls_remoteloc

			Run(ls_pscp,minimized!)
			// PSCP 12/02/2009 #2246
			
//			DECLARE LOADMARC PROCEDURE FOR LOADMARC2(:ls_cfile3, :ld_cabdt_dt, :ld_cur ,:ls_cfile2)
//				USING sqlserveroracletrans; 
//				
//			EXECUTE LOADMARC;
//				
//			IF f_check_dberror(sqlserveroracletrans,'Inserting into MARCFILE table')=FALSE THEN 		
//				ROLLBACK USING sqlserveroracletrans;
//			ELSE
//				COMMIT USING sqlserveroracletrans;
				Messagebox("File Creation","Final MARC file created., Please check PSCPERR.Log in the MARC file folder for any transfer errors")
//			END IF			

				
				
			END IF
		ELSE
			
//			NextStep:
			
// code commented 12/2/2009			
/*
			// Marcfile does not exist
			w_create_final_marc_file.ole_ezftp.uf_set_currentdirectory(ls_remoteloc)
			w_create_final_marc_file.ole_ezftp.uf_upload (ls_cfile, ls_cfile3 , FALSE)
*/
	/////
	
			// Add PSCP call 12/2/2009
			ls_file = 'c:\progra~~1\putty\pscp.exe'
			IF Fileexists(ls_file) THEN
				ls_file = 'c:\progra~~1\putty\pscp.exe'
			ELSE
				ls_file = 'pscp.exe'
				IF NOT Fileexists(ls_file) THEN
					Messagebox('Error', ' PUTTY ->PSCP File Transfer Utility not found, Make sure it is installed and in the system path. Contact System Administrator')
					RETURN
				END IF
			END IF
			
//			ls_pscp = ls_file + '  -pw ' + Trim(sle_pwd.text)  + ' ' + ls_cfile  + ' ' + Trim(sle_user.text) + '@' + Trim(sle_server.text) + ':' + ls_remoteloc   //+ ' > pscperr.log' //  ' pics@rs21n.loc.gov:' +  // Tr0uble1

			ls_bfile = 'c:\progra~~1\picsorcl9i\pscprun.bat'
			IF Fileexists(ls_bfile) THEN
			ELSE
				ls_bfile = 'pscprun.bat'
				IF NOT Fileexists(ls_bfile) THEN
					Messagebox('Error', 'PSCPRUN.BAT dos batch file not found. Contact System Administrator')
					RETURN
				END IF
			END IF
			
			ls_pscp = ls_bfile  +  ' ' + Trim(sle_pwd.text)  + '  "' +ls_cfile  + '"  ' + Trim(sle_user.text) +  ' ' + Trim(sle_server.text) + ' ' + ls_remoteloc

			Run(ls_pscp,minimized!)
			// PSCP 12/02/2009 #2246
			
//			DECLARE LOADMARC PROCEDURE FOR LOADMARC2(:ls_cfile3, :ld_cabdt_dt, :ld_cur ,:ls_cfile2)
//				USING sqlserveroracletrans; 
//				
//			EXECUTE LOADMARC;
//				
//			IF f_check_dberror(sqlserveroracletrans,'Inserting into MARCFILE table')=FALSE THEN 		
//				ROLLBACK USING sqlserveroracletrans;
//			ELSE
//				COMMIT USING sqlserveroracletrans;
				Messagebox("File Creation","Final MARC file created., Please check PSCPERR.Log in the MARC file folder for any transfer errors")
//			END IF			
		END IF	
//	END IF
//ELSE
//	Messagebox("FTP Error", "Unable to get ftp information.")
//	li_ftp_rc = -1	
//END IF
 





end event

type cb_marc_file from commandbutton within w_create_final_marc_file
integer x = 1006
integer y = 120
integer width = 119
integer height = 68
integer taborder = 30
boolean bringtotop = true
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "Set"
end type

event clicked;Parent.triggerevent("ue_dist_file")
end event

type gb_1 from groupbox within w_create_final_marc_file
integer x = 133
integer y = 36
integer width = 1061
integer height = 192
integer textsize = -8
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 33554432
long backcolor = 67108864
string text = "Final Marc Filename"
end type


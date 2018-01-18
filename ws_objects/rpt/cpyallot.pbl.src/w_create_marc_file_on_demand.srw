$PBExportHeader$w_create_marc_file_on_demand.srw
forward
global type w_create_marc_file_on_demand from w_main
end type
type ole_ezftp from u_ezftp within w_create_marc_file_on_demand
end type
type st_1 from u_st within w_create_marc_file_on_demand
end type
type dw_mchar_cabdt from u_pics_dw within w_create_marc_file_on_demand
end type
type dw_record_cnt from u_pics_dw within w_create_marc_file_on_demand
end type
type dw_get_prevbkno from u_pics_dw within w_create_marc_file_on_demand
end type
type dw_get_pubname from u_pics_dw within w_create_marc_file_on_demand
end type
type dw_get_anno from u_pics_dw within w_create_marc_file_on_demand
end type
type dw_creat_marc_file from u_pics_dw within w_create_marc_file_on_demand
end type
type dw_distsched_inx from u_pics_dw within w_create_marc_file_on_demand
end type
type st_transupdate from statictext within w_create_marc_file_on_demand
end type
type dw_sp1_distsched_orl from u_pics_dw within w_create_marc_file_on_demand
end type
type dw_sp5_dsweb from u_pics_dw within w_create_marc_file_on_demand
end type
type dw_sp1_distsched from u_pics_dw within w_create_marc_file_on_demand
end type
type dw_rs20_batch_tbl_with_libcd from u_pics_dw within w_create_marc_file_on_demand
end type
type dw_rs20_batch_tbl from u_pics_dw within w_create_marc_file_on_demand
end type
type dw_libcode from u_pics_dw within w_create_marc_file_on_demand
end type
type uo_progress from u_progressbar within w_create_marc_file_on_demand
end type
type cb_oracle from commandbutton within w_create_marc_file_on_demand
end type
type st_marc_file from u_st within w_create_marc_file_on_demand
end type
type cb_cancel from commandbutton within w_create_marc_file_on_demand
end type
type cb_go from commandbutton within w_create_marc_file_on_demand
end type
type cb_marc_file from commandbutton within w_create_marc_file_on_demand
end type
type st_sel_file from u_st within w_create_marc_file_on_demand
end type
type gb_sel from groupbox within w_create_marc_file_on_demand
end type
type gb_1 from groupbox within w_create_marc_file_on_demand
end type
end forward

global type w_create_marc_file_on_demand from w_main
integer width = 1298
integer height = 864
string title = "Create MARC File"
event type integer ue_create_file ( )
event ue_cancel ( )
event ue_dist_file ( )
event ue_all ( )
event ue_single ( )
event ue_enterkey pbm_dwnprocessenter
event ue_sel_file ( )
event ue_bklist ( )
ole_ezftp ole_ezftp
st_1 st_1
dw_mchar_cabdt dw_mchar_cabdt
dw_record_cnt dw_record_cnt
dw_get_prevbkno dw_get_prevbkno
dw_get_pubname dw_get_pubname
dw_get_anno dw_get_anno
dw_creat_marc_file dw_creat_marc_file
dw_distsched_inx dw_distsched_inx
st_transupdate st_transupdate
dw_sp1_distsched_orl dw_sp1_distsched_orl
dw_sp5_dsweb dw_sp5_dsweb
dw_sp1_distsched dw_sp1_distsched
dw_rs20_batch_tbl_with_libcd dw_rs20_batch_tbl_with_libcd
dw_rs20_batch_tbl dw_rs20_batch_tbl
dw_libcode dw_libcode
uo_progress uo_progress
cb_oracle cb_oracle
st_marc_file st_marc_file
cb_cancel cb_cancel
cb_go cb_go
cb_marc_file cb_marc_file
st_sel_file st_sel_file
gb_sel gb_sel
gb_1 gb_1
end type
global w_create_marc_file_on_demand w_create_marc_file_on_demand

type variables

end variables

forward prototypes
public function string wf_trpipe_tilde (string as_str)
public function integer wf_send_mail ()
end prototypes

event type integer ue_create_file();return -1

end event

event ue_cancel;call super::ue_cancel;close(this)
end event

event ue_dist_file();string ls_filename, ls_path, ls_today, ls_m, ls_d, ls_y, ls_cabdt
datetime ld_cabdt

ld_cabdt=dw_mchar_cabdt.object.cabdt[1]

ls_cabdt=string(ld_cabdt,'mm/dd/yyyy')
ls_m=mid(ls_cabdt,1,2)
ls_d=mid(ls_cabdt,4,2)
ls_y=mid(ls_cabdt,7,4)
ls_filename="marc"+ls_m+ls_y
//ls_filename = "distrib.txt"
ls_path = ls_filename
if GetFileSaveName("Select MARC File",ls_path,ls_filename, "Txt", "Txt Files (*.txt),*.txt") <> 1 then
	messagebox("File Error","Incorrect file name, Please try again.")
	return
end if

st_marc_file.text = ls_path
end event

event ue_all;//dw_1.visible = false
//gb_sel.visible = true
//cb_sel_file.visible = true
//st_sel_file.visible = true
//
end event

event ue_single;call super::ue_single;//sle_bkno.enabled = true
end event

event ue_enterkey;//Send(Handle(this),256,9,Long(0,0))
//return(1)
end event

event ue_sel_file();string ls_filename, ls_path, ls_m, ls_d, ls_y, ls_today

ls_today=string(Today(),'mm/dd/yy')
ls_m=mid(ls_today,1,2)
ls_d=mid(ls_today,4,2)
ls_y=mid(ls_today,7,2)
ls_filename="selected"+ls_m+ls_d+ls_y+".txt"
//ls_filename = "selected.txt"
ls_path =  ls_filename
if GetFileSaveName("Select Quantities File",ls_path,ls_filename, "txt", "Text Files (*.txt),*.txt") <> 1 then
	messagebox("File Error","Incorrect file name, Please try again.")
	return
end if

st_sel_file.text = ls_path

//rb_bklist.checked = false
//rb_all.checked = true
end event

event ue_bklist;//dw_1.visible = true
//st_sel_file.text = "none"
//cb_sel_file.visible = false
//st_sel_file.visible = false
//gb_sel.visible = false
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

public function integer wf_send_mail ();mailSession				mSes
mailReturnCode			mRet
mailMessage			mMsg
mailFileDescription		mAttach
string					ls_ret, ls_syntax, ls_name, ls_open_pathname, ls_filename
string					ls_attach_name//'c:\marc0701.dat.txt'
int						li_index, li_nret, li_nrecipients, li_nfile, li_pos

//mailsession.mailLogon ( { userid, password } {, logonoption } )
mRet=mailReturnFailure!

mSes = CREATE mailSession

ls_attach_name=st_marc_file.text
li_pos=pos(ls_attach_name,'\')
if li_pos >0 then
	ls_filename=mid(ls_attach_name,li_pos +1)
end if

do while pos(ls_filename,'\') >0
	li_pos=pos(ls_filename,'\')
	ls_filename=mid(ls_filename, li_pos+1)
loop
mRet = mSes.mailLogon ( mailNewSession! )
ls_ret = f_mail_error_to_string ( mRet, 'Logon:', FALSE )

If mRet <> mailReturnSuccess! Then
	MessageBox ("Mail Logon failed", 'Return Code <> mailReturnSuccess! '+ls_ret )
	mSes.mailLogoff()
	
	DESTROY mSes
	return -1 
End If
SetPointer(HourGlass!)
mMsg.Subject = 'create MARC file'
mMsg.NoteText = 'this is text file'

mAttach.FileType = mailAttach!
mAttach.PathName = ls_attach_name
mAttach.FileName = ls_filename
	
mMsg.AttachmentFile[1] = mAttach
//mMsg.Recipient[1].Name = 'raxt@loc.gov'
mMsg.Recipient[2].Name = 'pmag@loc.gov' //deliver application should include, after
//deliver application one must comment this line
mMsg.Recipient[1].Name = 'murali.krishnamurthy@baesystems.com'
//mMsg.Recipient[4].Name = 'sandeep.kaul@getronicsgov.com'
//mMsg.Recipient[2].Name = 'Kaul, Sandeep'
mRet = mSes.mailsend ( mMsg )
ls_ret = f_mail_error_to_string ( mRet, 'Send mail:', FALSE )
IF mRet <> mailreturnsuccess! THEN
		MessageBox ("Mail Send",'Return Code <> mailReturnSuccess! '+ls_ret )
	RETURN -1
END IF	

mRet=mSes.mailLogoff()
ls_ret = f_mail_error_to_string ( mRet, 'Logoff:', FALSE )
IF mRet <> mailreturnsuccess! THEN
		MessageBox ("Logoff",'Return Code <> mailReturnSuccess! '+ls_ret )
	RETURN -1
END IF	
DESTROY mSes
return 1

end function

on w_create_marc_file_on_demand.create
int iCurrent
call super::create
this.ole_ezftp=create ole_ezftp
this.st_1=create st_1
this.dw_mchar_cabdt=create dw_mchar_cabdt
this.dw_record_cnt=create dw_record_cnt
this.dw_get_prevbkno=create dw_get_prevbkno
this.dw_get_pubname=create dw_get_pubname
this.dw_get_anno=create dw_get_anno
this.dw_creat_marc_file=create dw_creat_marc_file
this.dw_distsched_inx=create dw_distsched_inx
this.st_transupdate=create st_transupdate
this.dw_sp1_distsched_orl=create dw_sp1_distsched_orl
this.dw_sp5_dsweb=create dw_sp5_dsweb
this.dw_sp1_distsched=create dw_sp1_distsched
this.dw_rs20_batch_tbl_with_libcd=create dw_rs20_batch_tbl_with_libcd
this.dw_rs20_batch_tbl=create dw_rs20_batch_tbl
this.dw_libcode=create dw_libcode
this.uo_progress=create uo_progress
this.cb_oracle=create cb_oracle
this.st_marc_file=create st_marc_file
this.cb_cancel=create cb_cancel
this.cb_go=create cb_go
this.cb_marc_file=create cb_marc_file
this.st_sel_file=create st_sel_file
this.gb_sel=create gb_sel
this.gb_1=create gb_1
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.ole_ezftp
this.Control[iCurrent+2]=this.st_1
this.Control[iCurrent+3]=this.dw_mchar_cabdt
this.Control[iCurrent+4]=this.dw_record_cnt
this.Control[iCurrent+5]=this.dw_get_prevbkno
this.Control[iCurrent+6]=this.dw_get_pubname
this.Control[iCurrent+7]=this.dw_get_anno
this.Control[iCurrent+8]=this.dw_creat_marc_file
this.Control[iCurrent+9]=this.dw_distsched_inx
this.Control[iCurrent+10]=this.st_transupdate
this.Control[iCurrent+11]=this.dw_sp1_distsched_orl
this.Control[iCurrent+12]=this.dw_sp5_dsweb
this.Control[iCurrent+13]=this.dw_sp1_distsched
this.Control[iCurrent+14]=this.dw_rs20_batch_tbl_with_libcd
this.Control[iCurrent+15]=this.dw_rs20_batch_tbl
this.Control[iCurrent+16]=this.dw_libcode
this.Control[iCurrent+17]=this.uo_progress
this.Control[iCurrent+18]=this.cb_oracle
this.Control[iCurrent+19]=this.st_marc_file
this.Control[iCurrent+20]=this.cb_cancel
this.Control[iCurrent+21]=this.cb_go
this.Control[iCurrent+22]=this.cb_marc_file
this.Control[iCurrent+23]=this.st_sel_file
this.Control[iCurrent+24]=this.gb_sel
this.Control[iCurrent+25]=this.gb_1
end on

on w_create_marc_file_on_demand.destroy
call super::destroy
destroy(this.ole_ezftp)
destroy(this.st_1)
destroy(this.dw_mchar_cabdt)
destroy(this.dw_record_cnt)
destroy(this.dw_get_prevbkno)
destroy(this.dw_get_pubname)
destroy(this.dw_get_anno)
destroy(this.dw_creat_marc_file)
destroy(this.dw_distsched_inx)
destroy(this.st_transupdate)
destroy(this.dw_sp1_distsched_orl)
destroy(this.dw_sp5_dsweb)
destroy(this.dw_sp1_distsched)
destroy(this.dw_rs20_batch_tbl_with_libcd)
destroy(this.dw_rs20_batch_tbl)
destroy(this.dw_libcode)
destroy(this.uo_progress)
destroy(this.cb_oracle)
destroy(this.st_marc_file)
destroy(this.cb_cancel)
destroy(this.cb_go)
destroy(this.cb_marc_file)
destroy(this.st_sel_file)
destroy(this.gb_sel)
destroy(this.gb_1)
end on

event pfc_postopen;call super::pfc_postopen;dw_mchar_cabdt.SetTransObject(SqlServerTrans)
dw_mchar_cabdt.Retrieve()

end event

type ole_ezftp from u_ezftp within w_create_marc_file_on_demand
boolean visible = false
integer x = 1019
integer y = 576
integer taborder = 160
string binarykey = "w_create_marc_file_on_demand.win"
end type

type st_1 from u_st within w_create_marc_file_on_demand
integer x = 215
integer y = 192
integer width = 338
integer height = 68
integer textsize = -10
string text = "Batch Date:"
end type

type dw_mchar_cabdt from u_pics_dw within w_create_marc_file_on_demand
integer x = 558
integer y = 172
integer width = 421
integer height = 112
integer taborder = 70
string dataobject = "d_mchar_cabdt"
boolean vscrollbar = false
end type

type dw_record_cnt from u_pics_dw within w_create_marc_file_on_demand
boolean visible = false
integer x = 1074
integer y = 48
integer width = 50
integer height = 64
integer taborder = 240
string dataobject = "d_record_cnt"
boolean vscrollbar = false
boolean livescroll = false
end type

event constructor;call super::constructor;THIS.settransobject(sqlservertrans)

end event

type dw_get_prevbkno from u_pics_dw within w_create_marc_file_on_demand
boolean visible = false
integer x = 987
integer y = 48
integer width = 50
integer height = 64
integer taborder = 230
string dataobject = "d_get_prevbkno"
boolean vscrollbar = false
boolean livescroll = false
end type

event constructor;call super::constructor;THIS.settransobject(sqlservertrans)

end event

type dw_get_pubname from u_pics_dw within w_create_marc_file_on_demand
boolean visible = false
integer x = 910
integer y = 44
integer width = 50
integer height = 64
integer taborder = 220
string dataobject = "d_get_pubname"
boolean vscrollbar = false
boolean livescroll = false
end type

event constructor;call super::constructor;THIS.settransobject(sqlservertrans)

end event

type dw_get_anno from u_pics_dw within w_create_marc_file_on_demand
boolean visible = false
integer x = 850
integer y = 60
integer width = 50
integer height = 64
integer taborder = 210
string dataobject = "d_get_anno"
boolean vscrollbar = false
boolean livescroll = false
end type

event constructor;call super::constructor;THIS.settransobject(sqlservertrans)

end event

type dw_creat_marc_file from u_pics_dw within w_create_marc_file_on_demand
boolean visible = false
integer x = 759
integer y = 56
integer width = 50
integer height = 64
integer taborder = 200
string dataobject = "d_creat_marc_file"
boolean vscrollbar = false
boolean livescroll = false
end type

event constructor;call super::constructor;THIS.settransobject(sqlservertrans)

end event

type dw_distsched_inx from u_pics_dw within w_create_marc_file_on_demand
boolean visible = false
integer x = 178
integer y = 56
integer width = 50
integer height = 64
integer taborder = 200
string dataobject = "d_ds_pgm85_distsched_bklist_my"
boolean vscrollbar = false
boolean livescroll = false
end type

event constructor;call super::constructor;THIS.settransobject(sqlservertrans)

end event

type st_transupdate from statictext within w_create_marc_file_on_demand
boolean visible = false
integer x = 165
integer y = 52
integer width = 146
integer height = 76
integer textsize = -8
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 33554432
long backcolor = 67108864
boolean focusrectangle = false
end type

type dw_sp1_distsched_orl from u_pics_dw within w_create_marc_file_on_demand
event ue_addrow ( )
boolean visible = false
integer x = 311
integer y = 52
integer width = 50
integer height = 64
integer taborder = 10
string dataobject = "d_sp1_distsched_orl"
boolean hscrollbar = true
boolean hsplitscroll = true
boolean livescroll = false
end type

event ue_addrow();//long li_row_count
//
//i_count++
//if uo_progress.visible=true and mod(i_count,10)=0 then
//	uo_progress.of_Increment(1)
//end if
//if mod(i_count,100)=0 then
//	w_create_files.SetRedraw(TRUE)
//else
//	w_create_files.SetRedraw(false)
//end if
//
end event

event constructor;call super::constructor;THIS.settransobject(sqlserveroracletrans)

end event

event sqlpreview;call super::sqlpreview;//
//	i_count++
//	if uo_progress.visible=true and mod(i_count,10)=0 then
//		uo_progress.of_Increment(1)
//	end if
//	if mod(i_count,100)=0 then
//		parent.SetRedraw(TRUE)
//	else
//		parent.SetRedraw(false)
//	end if
//	
////	DO WHILE condition
////	statementblock
////	LOOP
//	Do While SqlServerOracleTrans.of_isconnected()=false 
//		SqlServerOracleTrans.of_connect()
//		IF SqlServerOracleTrans.sqlcode <> 0 THEN
//			IF SqlServerOracleTrans.SqlDbcode = -951 THEN            //check for invalid userid
//			  MessageBox("Login Error","Invalid User ID/Password. Please re-enter.",StopSign!)
//			ELSEIF SqlServerOracleTrans.SqlDBcode = -952 THEN       //check for invalid password
//			  MessageBox("Login Error","Invalid User ID/Password. Please re-enter.",StopSign!)
//			Else                                             //check for other error messages
//				MessageBox("Database Connection Error","Unable to Connect. Pleas try again" ,&
//				StopSign!)
//			end if
//		end if
//	LOOP
//
//////		SqlServerOracleTrans.DBParm = "PBCatalogOwner='PICADMIN'"
end event

event updateend;call super::updateend;//close(w_pics_retrieve_msg_box)
end event

event updatestart;call super::updatestart;//openwithparm(w_pics_retrieve_msg_box,"Updating distribution schedule table in RS21n, Please Wait...")
end event

type dw_sp5_dsweb from u_pics_dw within w_create_marc_file_on_demand
boolean visible = false
integer x = 1371
integer y = 268
integer width = 78
integer height = 104
integer taborder = 110
string dataobject = "d_sp5_dsweb"
boolean vscrollbar = false
boolean border = false
boolean livescroll = false
end type

event constructor;call super::constructor;THIS.settransobject(sqlservertrans)

end event

event retrieveend;call super::retrieveend;close(w_pics_retrieve_msg_box)
end event

event retrievestart;call super::retrievestart;openwithparm(w_pics_retrieve_msg_box,"Populating temporary tables, Please Wait...")
end event

type dw_sp1_distsched from u_pics_dw within w_create_marc_file_on_demand
boolean visible = false
integer x = 242
integer y = 56
integer width = 50
integer height = 64
integer taborder = 120
string dataobject = "d_sp1_distsched"
boolean vscrollbar = false
boolean livescroll = false
end type

event constructor;call super::constructor;THIS.settransobject(sqlserveroracletrans)

end event

event sqlpreview;call super::sqlpreview;IF This.GetRow() <= uo_progress.of_GetMaximum() THEN
	uo_progress.of_Increment(1)
	IF mod(This.GetRow(),100)=0 THEN
		w_create_dist_sched.SetRedraw(TRUE) 
		w_pics_retrieve_msg_box.SetRedraw(TRUE)
	END IF
END IF

end event

event updateend;call super::updateend;close(w_pics_retrieve_msg_box)
end event

event updatestart;call super::updatestart;openwithparm(w_pics_retrieve_msg_box,"Updating distribution schedule table in RS21n, Please Wait...")
end event

type dw_rs20_batch_tbl_with_libcd from u_pics_dw within w_create_marc_file_on_demand
boolean visible = false
integer x = 512
integer y = 56
integer width = 50
integer height = 64
integer taborder = 100
string dataobject = "d_rs20_batch_tbl_with_libcd"
boolean vscrollbar = false
boolean border = false
boolean livescroll = false
end type

event constructor;call super::constructor;THIS.settransobject(sqlservertrans)

end event

event retrievestart;call super::retrievestart;openwithparm(w_pics_retrieve_msg_box,"Populating temporary tables, Please Wait...")
end event

event retrieveend;call super::retrieveend;close(w_pics_retrieve_msg_box)
end event

type dw_rs20_batch_tbl from u_pics_dw within w_create_marc_file_on_demand
boolean visible = false
integer x = 581
integer y = 56
integer width = 50
integer height = 64
integer taborder = 170
string dataobject = "d_rs20_batch_tbl"
boolean vscrollbar = false
boolean border = false
boolean livescroll = false
end type

event constructor;call super::constructor;THIS.settransobject(sqlservertrans)

end event

event retrievestart;call super::retrievestart;openwithparm(w_pics_retrieve_msg_box,"Populating temporary tables, Please Wait...")
end event

event retrieveend;call super::retrieveend;close(w_pics_retrieve_msg_box)
end event

type dw_libcode from u_pics_dw within w_create_marc_file_on_demand
boolean visible = false
integer x = 658
integer y = 56
integer width = 50
integer height = 64
integer taborder = 190
string dataobject = "dddw_libcode"
boolean vscrollbar = false
boolean livescroll = false
end type

event constructor;call super::constructor;THIS.settransobject(sqlservertrans)

end event

type uo_progress from u_progressbar within w_create_marc_file_on_demand
boolean visible = false
integer x = 87
integer y = 48
integer width = 69
integer height = 92
integer taborder = 180
boolean border = true
borderstyle borderstyle = stylelowered!
end type

on uo_progress.destroy
call u_progressbar::destroy
end on

type cb_oracle from commandbutton within w_create_marc_file_on_demand
boolean visible = false
integer x = 443
integer y = 56
integer width = 50
integer height = 64
integer taborder = 150
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "&Load data into Web"
end type

event clicked;//int rtn, li_count
//date ld_distbdt
//string ls_bklist[], ls_date
//str_distrib_schedule lstr
//
//lstr=istr
//ls_bklist[]= lstr.arraymed[]
//ld_distbdt= lstr.ld_date
//li_count=UpperBound( ls_bklist[])
//
//ls_date= string( ld_distbdt,'mm/dd/yyyy')
//
//IF IsNull(ls_date) OR ls_date="" THEN
//	MessageBox("ERROR","Batch date is null, please enter the date.")
//	return
//ELSE		
//	ld_distbdt = date(ls_date)
//
//	rtn = MessageBox("Data Migration","This process will migrate selected records from ~"sched~" table(RS20) into ~"distsched~" table(RS21)." +&
//							" This process will take about 10-15 minutes. Continue?",Question!,YesNo!,1)
//	IF rtn = 1	THEN
//		//// Profile SP1PICS
//		SqlServerOracleTrans.DBMS = "O73 ORACLE 7.3"
//		SqlServerOracleTrans.LogPass = "picadmin"
//		SqlServerOracleTrans.ServerName = "@TNS:ORAPICS"
//		SqlServerOracleTrans.LogId = "picadmin"
//		SqlServerOracleTrans.AutoCommit = False
//		SqlServerOracleTrans.DBParm = "PBCatalogOwner='PICADMIN'"
//		SqlServerOracleTrans.of_connect()
//		IF SqlServerOracleTrans.sqlcode <> 0 THEN
//			IF SqlServerOracleTrans.SqlDbcode = -951 THEN            //check for invalid userid
//			  MessageBox("Login Error","Invalid User ID/Password. Please re-enter.",StopSign!)
//			  Return -1	
//			ELSEIF SqlServerOracleTrans.SqlDBcode = -952 THEN       //check for invalid password
//			  MessageBox("Login Error","Invalid User ID/Password. Please re-enter.",StopSign!)
//			  Return -1
//			Else                                             //check for other error messages
//			MessageBox("Database Connection Error","Unable to Connect. " +& 
//			string(SqlServerOracleTrans.sqldbcode) + " " +&
//			SqlServerOracleTrans.SQLErrText, &
//			StopSign!)
//			Return -1
//		  END IF
//		ELSE
//			String ls_msgtext[ ] = {" 0 %","10 %", "20 %", "30 %", "40 %", "50 %", "60 %", &
//											"70 %", "80 %", "90 %", "100 %"}
////			Integer li_count
//			long ll_rows
//			
//			ll_rows = dw_sp5_dsweb.retrieve(ld_distbdt)
//			IF ll_rows = 0 THEN
//				MessageBox("ERROR","No rows found.")
//			ELSE		
//				uo_progress.Visible=TRUE
//				uo_progress.of_SetMinimum(0)
//				uo_progress.of_SetMaximum(ll_rows)
//				uo_progress.of_SetDisplayStyle(3)
//				uo_progress.of_SetMessageText(ls_msgtext)
//				uo_progress.of_SetPosition(0)
//				
//				dw_sp5_dsweb.RowsCopy(1,ll_rows, Primary!, dw_sp1_distsched, 1, Primary!)
//				
//				dw_sp1_distsched.settransobject(sqlserveroracletrans)
//				
//				rtn = dw_sp1_distsched.of_Update(TRUE,TRUE)
//				IF f_check_dberror(sqlserveroracletrans,"distsched at SP9") THEN
//					IF rtn=1 THEN
//						Commit Using sqlserveroracletrans;
//						MessageBox("Update",string(dw_sp1_distsched.RowCount())+ " rows inserted into distsched table. ")
//					ELSE
//						MessageBox("Update"," Update distsched table failed.")
//						Rollback Using sqlserveroracletrans;
//						RETURN
//					END IF
//				END IF
//
//				uo_progress.Visible=FALSE
//			END IF		
//		END IF
//	END IF
//END IF
//
//
//
end event

type st_marc_file from u_st within w_create_marc_file_on_demand
integer x = 146
integer y = 400
integer width = 814
integer height = 72
integer taborder = 40
long backcolor = 1090519039
boolean border = true
borderstyle borderstyle = stylelowered!
end type

type cb_cancel from commandbutton within w_create_marc_file_on_demand
integer x = 704
integer y = 576
integer width = 247
integer height = 96
integer taborder = 140
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

type cb_go from commandbutton within w_create_marc_file_on_demand
integer x = 270
integer y = 572
integer width = 247
integer height = 96
integer taborder = 130
boolean bringtotop = true
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "&Go"
end type

event clicked;// 04/29/2008 3.7 changes Media should be RTB for RC and DB rows
Long ll_rows, ll_count,  li_applen, li_len, ll_err_cnt=0, rtn
String  ls_anno, ls_text, ls_usid,ls_prevbkno,ls_error_text
String ls_title, ls_prvchno, ls_format, ls_bkno, ls_blk , ls_temp, ls_left
String ls_date, ls_volumes, ls_boxes, ls_errorfile
Integer  li_filenum, li_filenum2, i, J,li_ftp_rc
String ls_bkmed, ls_bkseq, ls_auth, ls_authfn, ls_publisher
String ls_pubyr, ls_srcdoc, ls_reissue, ls_vols, ls_casub,  ls_libcd
String  ls_prevbkseq, ls_prevbkmed, ls_med
String ls_chno, ls_conno, ls_aepcd, ls_ttlart, ls_lang,ls_dewey, &
		ls_ricd, ls_oneliner, ls_bttl1, ls_bttl2, ls_crname, ls_pubname
Long ll_defqty,li_pubyr, li_bkseq, li_cryr, li_apllen, li_vols, li_cnt
Long  ll_selqty, ll_ordqty,li_row_count
Dec li_conno
Date ld_tday,ld_cabdt

DateTime ldt_cur,ld_cabdt_dt
String ls_cfile, ls_tday,ls_mday, ls_msg
String ls_ftpsite, ls_ftpuid, ls_ftppwd,ls_remoteloc,ls_marcfile

ld_cabdt_dt=dw_mchar_cabdt.object.cabdt[1]
ld_cabdt = Date(dw_mchar_cabdt.object.cabdt[1])

ls_tday = String(Today(),'mmyy')

//*****
ls_usid=SqlServerTrans.userId


ls_mday = String(ld_cabdt,'mmyy')

ls_cfile = st_marc_file.text
// Error file
ls_errorfile = 'c:\program files\picsorcl9i\error\marcerr'+ls_tday+'.txt'

ls_marcfile = 'IMarc'+ls_mday

IF (ls_cfile = "" OR ls_cfile = "none") THEN
	Messagebox('MARC extract','Please enter a MARC file name.')
	st_marc_file.SetFocus()
	RETURN 
END IF

ls_msg = "Creating MARC File, please wait..."
OpenWithParm(w_pics_retrieve_msg_box,ls_msg)	
	

dw_creat_marc_file.SetTransObject(SqlServerTrans)
	
ll_rows = dw_creat_marc_file.Retrieve(ld_cabdt)
IF (ll_rows = 0) THEN
	Messagebox("Error","System error while retrieving Marc records for batch date "+String(ld_cabdt)+". Please contact system administrator.")
	Close(w_pics_retrieve_msg_box)
	RETURN
END IF
FOR i=1 TO ll_rows
	ls_conno = Trim(String(dw_creat_marc_file.object.conno[i]))
	ls_left=Left(ls_conno,1)
	li_conno=Long(ls_conno)
	IF ls_left='0' THEN
		li_conno= li_conno+2000*1000000
	ELSE
		li_conno= li_conno+1900*1000000
	END IF
	dw_creat_marc_file.object.sort[i]=li_conno
NEXT
dw_creat_marc_file.Sort()
IF (ll_rows = 0) THEN
	Messagebox("Error","System error while sorting marc file. Please contact system administrator.")
	Close(w_pics_retrieve_msg_box)
	RETURN
END IF
	
IF ll_rows = -1 THEN
	Messagebox("Error","System error while retrieving. Please contact system administrator.")
	ROLLBACK USING SqlServerTrans;
	Close(w_pics_retrieve_msg_box)
	RETURN
END IF
	
li_filenum = FileOpen(ls_cfile,streamMode!,write!,lockWrite!,replace!)
IF li_filenum = -1 THEN
	Messagebox("File Error","The file "+ls_cfile+" could not be opened.")
	ROLLBACK USING SqlServerTrans;
	Close(w_pics_retrieve_msg_box)
	RETURN
ELSE
	li_filenum2 = FileOpen(ls_errorfile,streamMode!,write!,lockWrite!,replace!)
	IF li_filenum2 = -1 THEN
		Messagebox("File Error","The error file "+ls_errorfile+" could not be opened.")
		ROLLBACK USING SqlServerTrans;
	       Close(w_pics_retrieve_msg_box)
		RETURN
	END IF		
END IF

this.SetRedraw(TRUE)
ls_prvchno=''

ls_error_text = "Marc file missed records"
ls_error_text+='~n'
	
FOR ll_count = 1 TO ll_rows
	ls_text=''

		ls_chno = Trim(String(dw_creat_marc_file.object.chno[ll_count]))
		ls_conno = Trim(String(dw_creat_marc_file.object.conno[ll_count]))
		ls_aepcd = Trim(String(dw_creat_marc_file.object.aepcd[ll_count]))
		ls_auth = Trim(String(dw_creat_marc_file.object.auth[ll_count]))
		ls_authfn = Trim(String(dw_creat_marc_file.object.authfn[ll_count]))
		ls_ttlart = Trim(String(dw_creat_marc_file.object.ttlart[ll_count]))
		ls_title = Trim(dw_creat_marc_file.object.ttl[ll_count])
		ls_publisher = Trim(dw_creat_marc_file.object.publisher[ll_count])
		li_pubyr = Long(dw_creat_marc_file.object.pubyr[ll_count])
		ls_med = Trim(String(dw_creat_marc_file.object.med[ll_count]))
		ls_lang = Trim(String(dw_creat_marc_file.object.lang[ll_count]))
		ls_casub = Trim(String(dw_creat_marc_file.object.casub[ll_count]))
		ls_bkmed = Trim(String(dw_creat_marc_file.object.bkmed[ll_count]))
		li_bkseq = dw_creat_marc_file.object.bkseq[ll_count]
		ls_dewey = Trim(String(dw_creat_marc_file.object.dewey[ll_count]))
		ls_ricd = Trim(dw_creat_marc_file.object.ricd[ll_count]	)
		ls_oneliner = Trim(String(dw_creat_marc_file.object.oneliner[ll_count]))
		ls_bttl1 = Trim(String(dw_creat_marc_file.object.b_ttl1[ll_count]))
		ls_bttl2 = String(dw_creat_marc_file.object.b_ttl2[ll_count])
		li_cryr = (dw_creat_marc_file.object.cryr[ll_count])
		ls_crname = Trim(String(dw_creat_marc_file.object.crname[ll_count]))
		li_applen = (dw_creat_marc_file.object.applen[ll_count])
		li_vols = (dw_creat_marc_file.object.vols[ll_count])
		// 04/29/2008
//		IF ls_med='RC' OR ls_med = 'RTB' THEN
		IF ls_bkmed='RC' OR ls_bkmed = 'DB' THEN
			ls_format='RTB'
		ELSEIF ls_med='BR' THEN
			ls_format='BR'
		ELSEIF ls_med='P/B' THEN
			ls_format='PB'
		ELSE
			ls_format='NA'
		END IF
		
		IF ls_prvchno<> ls_chno THEN
			li_cnt=dw_get_anno.Retrieve(ls_chno)
			IF li_cnt>0 THEN
				ls_anno=Trim(dw_get_anno.object.anno[1])
				ls_anno= wf_trpipe_tilde(ls_anno)
				ls_anno+=Fill(' ',490)
				ls_anno=Mid(ls_anno,1,490)
			ELSE
				ls_anno=Fill(' ',490)
			END IF
		END IF
		ls_text=ls_conno
	
		IF IsNull(ls_bkmed)=FALSE AND IsNull(li_bkseq)=FALSE THEN
			ls_bkno=ls_bkmed+' '+String(li_bkseq)+'                '
			ls_bkno=Left(ls_bkno,8)
			ls_text+= ls_bkno
		ELSE
			ls_text+='        '
		END IF
		IF NOT IsNull(ls_casub) THEN
			ls_casub+='   '
			ls_casub=Left(ls_casub,3)
			ls_text+= ls_casub
		ELSE
			ls_text+='   '
		END IF
		IF NOT IsNull(ls_dewey) THEN
			ls_dewey+='    '
			ls_dewey=Left(ls_dewey,4)
			ls_text+= ls_dewey
		ELSE
			ls_text+='    '
		END IF
		
//		IF ls_format='RC' THEN
		// 04/29/2008
		IF ls_format='RTB' THEN
			ls_text+='|'
		ELSE
			ls_text+='f'
		END IF
		ls_lang=Lower(ls_lang)
		IF NOT IsNull(ls_lang) THEN
			ls_lang+='   '
			ls_lang=Left(ls_lang,3)
			ls_text+=ls_lang
		ELSE
			ls_text+='   '
		END IF
		IF NOT IsNull(ls_auth) THEN
			ls_auth= wf_trpipe_tilde(ls_auth)
		END IF
		IF NOT IsNull(ls_authfn) THEN
			ls_authfn= wf_trpipe_tilde(ls_authfn)
		END IF
		ls_blk=Fill(' ',70)
		CHOOSE CASE ls_aepcd
			CASE 'I'
				ls_auth+=' '
				IF NOT(IsNull(ls_authfn)) THEN
					ls_auth+=ls_authfn+ ls_blk
				ELSE
					ls_auth+= ls_blk
				END IF
				ls_auth=Left(ls_auth,70)
				ls_text+= ls_auth
			CASE 'N'
				ls_text+=ls_blk
			CASE 'L'
				ls_auth+=ls_blk
				ls_auth=Mid(ls_auth,1,70)
				ls_text+= ls_auth
			CASE 'P'
				ls_auth+=ls_blk
				ls_auth=Mid(ls_auth,1,70)
				ls_text+= ls_auth
			CASE 'E'
				ls_auth+=ls_blk
				ls_auth=Mid(ls_auth,1,70)
				ls_text+= ls_auth
			CASE ELSE
				IF NOT(IsNull(ls_authfn)) THEN
					ls_auth += ', '+ls_authfn+ls_blk
				ELSE
					ls_auth += ls_blk
				END IF
				ls_auth=Mid(ls_auth,1,70)
				ls_text+= ls_auth
		END CHOOSE
		li_len=0
		ls_temp=''
		li_len=Len(ls_ttlart)
		IF (IsNull(ls_ttlart)=FALSE) AND Len(ls_ttlart) >0 THEN
			ls_text+=String(li_len + 1)
			ls_temp=ls_ttlart+' '+ls_title
		ELSE
			ls_text+='0'
			ls_temp=ls_title
		END IF
		ls_title=wf_trpipe_tilde(ls_temp)
		ls_blk=Fill(' ',355)
		ls_title+=ls_blk
		ls_title=Mid(ls_title,1,355)
		ls_text+= ls_title
		li_cnt=dw_get_pubname.Retrieve(ls_chno)
		IF li_cnt>0 THEN
			ls_pubname=dw_get_pubname.object.pubname[1]
		ELSE
			ls_pubname=ls_publisher
		END IF
		ls_blk=Fill(' ',75)
		ls_pubname+=ls_blk
		ls_pubname=Mid(ls_pubname,1,75)
		ls_text+=ls_pubname
		IF NOT IsNull(li_pubyr) THEN
			ls_temp=String(li_pubyr)
			ls_temp+=ls_blk
			ls_temp=Left(ls_temp,4)
		ELSE
			ls_temp=Fill(' ',4)
		END IF
		ls_text+=ls_temp
		
		ls_text+= ls_anno // len=490
		ls_blk=Fill(' ',60)
		
		IF NOT IsNull(ls_oneliner) THEN
			ls_oneliner= wf_trpipe_tilde(ls_oneliner)
			ls_oneliner+= ls_blk
		ELSE
			ls_oneliner=ls_blk
		END IF
		ls_oneliner=Mid(ls_oneliner,1,60)
		ls_text+=ls_oneliner
		IF IsNull(ls_ricd)=FALSE THEN
			li_cnt=dw_get_prevbkno.Retrieve(ls_conno)
			IF li_cnt>0 THEN
				ls_prevbkno=dw_get_prevbkno.object.prevbkno[1]
				ls_prevbkno+=Fill(' ',8)
				ls_prevbkno=Mid(ls_prevbkno,1,8)
			ELSE
				ls_prevbkno=Fill(' ',8)
			END IF
			IF ls_ricd='RI' THEN
				ls_prevbkno+='reissue'+'    '
			ELSE
				ls_prevbkno+='rerecording'
			END IF
		ELSE
			ls_prevbkno=Fill(' ',19)
		END IF
		ls_text+= ls_prevbkno
		IF IsNull(li_cryr)=FALSE AND IsNull(ls_crname)=FALSE THEN
			ls_crname=String(li_cryr)+' '+ ls_crname+Fill(' ',34)
			ls_crname=Mid(ls_crname,1,34)
		ELSE
			ls_crname=Fill(' ',34)
		END IF
		ls_text+=ls_crname
		
		CHOOSE CASE ls_format
				// 04/29/2008
//			CASE 'RC'
		CASE 'RTB'
			IF IsNull(li_vols)=FALSE THEN
			ELSEIF IsNull(li_applen)=FALSE THEN
				IF Mod(li_applen,4)=0 THEN
					li_vols=Int(li_applen/4)
				ELSE
					li_vols=Int(li_applen/4) +1
				END IF
			ELSE
				li_vols=0
			END IF
			IF Mod(li_vols,6)=0 THEN
				ls_boxes=String(Int(li_vols/6))
			ELSE
				ls_boxes=String(Int(li_vols/6) +1)
			END IF
			ls_volumes=String(li_vols)+' vol'
			ls_boxes+=' box'
			CASE 'BR'
				IF IsNull(li_vols)=FALSE THEN
				ELSEIF IsNull(li_applen)=FALSE THEN
					IF Mod(li_applen,250)=0 THEN
						li_vols=Int(li_applen/250)
					ELSE
						li_vols=Int(li_applen/250) + 1
					END IF
				ELSE
					li_vols=0
				END IF
				ls_volumes= String(li_vols)+' vol'
				ls_boxes=''
			CASE 'PB'
				IF IsNull(li_vols)=FALSE THEN
				ELSE
					li_vols=1
				END IF
				ls_volumes=String(li_vols)+' vol'
				ls_boxes=''
			CASE ELSE 
				ls_volumes=''
				ls_boxes=''
		END CHOOSE
		ls_volumes+=Fill(' ',10)
		ls_boxes+= Fill(' ',10)
		ls_volumes=Left(ls_volumes,10)
		ls_boxes=Left(ls_boxes,10)
		ls_text+=ls_volumes
		ls_text+= ls_boxes
//		IF ls_format='RC' THEN
		// 04/29/2008
		IF ls_format='RTB' THEN
			ls_bttl1='abcdefg' +Fill(' ',7) 
			ls_bttl2='hijklmn' +Fill(' ',7)      
		ELSE
			ls_bttl1=Fill(' ',14)
			ls_bttl2=Fill(' ',14)
		END IF
		ls_text+= ls_bttl1
		ls_text+= ls_bttl2
//		IF ls_format='RC' THEN
		// 04/29/2008
		IF ls_format='RTB' THEN
			ls_text+='iss|kmnjlc|||ae'
		ELSE
			ls_text+='atc'+Fill(' ',12)           
		END IF
		ls_text+='~n'
		IF IsNull(ls_text) OR ls_text='' THEN
			ll_err_cnt++ // this means this row did not get inserted
			ls_error_text += "Control number : "+ls_conno+", book number : "+ls_bkmed+ls_bkseq+" did not make it to the list."
			ls_error_text+='~n'
		END IF
		FileWrite(li_filenum, ls_text)
		ls_text=''
		ls_prvchno= ls_chno

	NEXT

IF ll_err_cnt>0 THEN
	ls_error_text += "Total number of records missed = "+String(ll_err_cnt)
	FileWrite(li_filenum2, ls_error_text)
	Messagebox('Rows missed','The file '+ls_errorfile+' contains the error list.')
ELSE
	ls_error_text += "No error in this batch of books"
	FileWrite(li_filenum2, ls_error_text)	
END IF


rtn=FileClose(li_filenum)
rtn=FileClose(li_filenum2)

this.SetRedraw(TRUE)
//******
ib_disableclosequery=TRUE
IF ll_err_cnt = 0 THEN
	
//	SELECT ftp_site, ftp_uid, ftp_pwd
//	INTO :ls_ftpsite, :ls_ftpuid, :ls_ftppwd
//	FROM PCS_FTP_INFO
//	USING SQLServerTrans;
//	IF f_check_dberror(SQLServerTrans,"SELECTING FROM PCS_FTP_INFO ") THEN
//
//		li_ftp_rc = w_create_marc_file_on_demand.ole_ezftp.uf_login ( ls_ftpsite, ls_ftpuid, ls_ftppwd)
//	
//		IF li_ftp_rc = -1 THEN
//			messagebox("FTP Error", "Unable to connect to ftp server for placing the MARC file.")
//		ELSE
//			ls_remoteloc = "/pics/prd/htdocs/download/marcfile/"
//			w_create_marc_file_on_demand.ole_ezftp.uf_set_currentdirectory(ls_remoteloc)
//			w_create_marc_file_on_demand.ole_ezftp.uf_upload ( ls_cfile, ls_marcfile , FALSE )
//			ls_marcfile = TRIM(ls_marcfile)
//			
//			DECLARE LOADMARC PROCEDURE FOR LOADMARC2(:ls_marcfile,:ld_cabdt_dt,'I')
//			USING SQLServerOracleTrans; 
//			
//			EXECUTE LOADMARC;
//			
//			IF f_check_dberror(SQLServerOracleTrans,'Updating marcfiles table in PICP')=FALSE THEN 		
//				ROLLBACK USING SQLServerOracleTrans;
//			ELSE
//				COMMIT USING SqlServerOracleTrans;
//			end if
//		END IF
//	ELSE
//		messagebox("FTP Error", "Unable to get ftp information.")
//		li_ftp_rc = -1	
//	END IF
//	
	Messagebox("File Creation","MARC file created.")
	
   IF IsValid(w_batch_formation) THEN
		IF (w_batch_formation.cbx_email.checked=FALSE OR w_batch_formation.cbx_file.checked=FALSE) THEN
			w_batch_formation.cbx_email.enabled=TRUE
			w_batch_formation.cbx_file.enabled=TRUE
			rtn=wf_send_mail()// the last step( 5th step) is send email
			IF rtn=1 THEN
				ldt_cur= DateTime(Today(), Now())
				// finish step 5 and batch formation all done
				w_batch_formation.dw_batch_process.Retrieve(ld_cabdt_dt)
				w_batch_formation.dw_batch_process.object.marcemail[1]= ldt_cur
				w_batch_formation.dw_batch_process.object.marcexp[1]= ldt_cur
				w_batch_formation.dw_batch_process.object.marcrows[1]= ll_rows
				w_batch_formation.dw_batch_process.object.marcuser[1]= ls_usid
				rtn=w_batch_formation.dw_batch_process.Update(TRUE,TRUE)
				IF rtn=1 THEN
					COMMIT USING SqlServerTrans;
					w_batch_formation.st_email.text=String(ldt_cur, 'mm/dd/yyyy hh:mm:ss')
					w_batch_formation.cbx_email.checked=TRUE
					w_batch_formation.st_marc.text=String(ldt_cur, 'mm/dd/yyyy hh:mm:ss')
					w_batch_formation.cbx_file.checked=TRUE
				ELSE
					ROLLBACK USING SqlServerTrans;
					w_batch_formation.cbx_email.checked=FALSE
					w_batch_formation.cbx_email.checked=FALSE
				END IF
			ELSE
				w_batch_formation.cbx_email.enabled=FALSE
				w_batch_formation.cbx_email.checked=FALSE
			END IF
		END IF // end if cbx_email.checked=false
	END IF // If the call is from w_batch_formation 
	
ELSE
	Messagebox("File Creation","MARC file created. Please view the errors in c:\program files\picsorcl9i\error\marcerrMMYYYY.txt")
	Close(w_pics_retrieve_msg_box)
END IF	

ib_disableclosequery=TRUE
IF IsValid(w_create_marc_file_on_demand) THEN
	Close(parent)
	Close(w_pics_retrieve_msg_box)
END IF







end event

type cb_marc_file from commandbutton within w_create_marc_file_on_demand
integer x = 969
integer y = 404
integer width = 119
integer height = 68
integer taborder = 70
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

type st_sel_file from u_st within w_create_marc_file_on_demand
boolean visible = false
integer x = 910
integer y = 28
integer width = 64
integer taborder = 60
long backcolor = 1090519039
boolean border = true
borderstyle borderstyle = stylelowered!
end type

type gb_sel from groupbox within w_create_marc_file_on_demand
boolean visible = false
integer x = 1033
integer y = 56
integer width = 64
integer height = 52
integer taborder = 30
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long backcolor = 79741120
string text = "Quantities Selected Filename:"
end type

type gb_1 from groupbox within w_create_marc_file_on_demand
integer x = 101
integer y = 320
integer width = 1061
integer height = 192
integer taborder = 120
integer textsize = -8
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 33554432
long backcolor = 67108864
string text = "Marc Filename"
end type


Start of PowerBuilder Binary Data Section : Do NOT Edit
06w_create_marc_file_on_demand.bin 
2000000c00e011cfd0e11ab1a1000000000000000000000000000000000003003e0009fffe000000060000000000000000000000010000000100000000000010000000000200000001fffffffe0000000000000000fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffd00000004fffffffefffffffefffffffeffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff006f00520074006f004500200074006e00790072000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000050016ffffffffffffffff00000001000000000000000000000000000000000000000000000000000000003dcb53c001cacf4c00000003000000800000000000500003004f0042005800430054005300450052004d0041000000000000000000000000000000000000000000000000000000000000000000000000000000000102001affffffff00000002ffffffff000000000000000000000000000000000000000000000000000000000000000000000000fffffffe0000000000000000004200500043004f00530058004f00540041005200450047000000000000000000000000000000000000000000000000000000000000000000000000000000000001001affffffffffffffff000000036580f76711cf781945446cb800005453000000003dcb53c001cacf4c3dcb53c001cacf4c000000000000000000000000004f00010065006c00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000102000affffffff00000004ffffffff000000000000000000000000000000000000000000000000000000000000000000000000000000000000001400000000fffffffefffffffeffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
24ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff0200000100000008000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000010000000003200000032000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000006f00430074006e006e00650073007400000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000020012ffffffffffffffffffffffff0000000000000000000000000000000000000000000000000000000000000000000000000000000100000018000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000ffffffffffffffffffffffff0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000ffffffffffffffffffffffff0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000ffffffffffffffffffffffff00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
16w_create_marc_file_on_demand.bin 
End of PowerBuilder Binary Data Section : No Source Expected After This Point

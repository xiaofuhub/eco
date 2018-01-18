$PBExportHeader$w_create_marc_file.srw
forward
global type w_create_marc_file from w_main
end type
type ole_ezftp from u_ezftp within w_create_marc_file
end type
type dw_record_cnt from u_pics_dw within w_create_marc_file
end type
type dw_get_prevbkno from u_pics_dw within w_create_marc_file
end type
type dw_get_pubname from u_pics_dw within w_create_marc_file
end type
type dw_get_anno from u_pics_dw within w_create_marc_file
end type
type dw_creat_marc_file from u_pics_dw within w_create_marc_file
end type
type dw_distsched_inx from u_pics_dw within w_create_marc_file
end type
type st_transupdate from statictext within w_create_marc_file
end type
type cb_print from commandbutton within w_create_marc_file
end type
type dw_sp1_distsched_orl from u_pics_dw within w_create_marc_file
end type
type dw_sp5_dsweb from u_pics_dw within w_create_marc_file
end type
type dw_sp1_distsched from u_pics_dw within w_create_marc_file
end type
type sle_batch_date from singlelineedit within w_create_marc_file
end type
type dw_rs20_batch_tbl_with_libcd from u_pics_dw within w_create_marc_file
end type
type dw_rs20_batch_tbl from u_pics_dw within w_create_marc_file
end type
type dw_libcode from u_pics_dw within w_create_marc_file
end type
type cbx_lib from checkbox within w_create_marc_file
end type
type uo_progress from u_progressbar within w_create_marc_file
end type
type cb_oracle from commandbutton within w_create_marc_file
end type
type st_marc_file from u_st within w_create_marc_file
end type
type cb_cancel from commandbutton within w_create_marc_file
end type
type cb_go from commandbutton within w_create_marc_file
end type
type cb_marc_file from commandbutton within w_create_marc_file
end type
type gb_1 from groupbox within w_create_marc_file
end type
type st_sel_file from u_st within w_create_marc_file
end type
type cb_sel_file from commandbutton within w_create_marc_file
end type
type gb_sel from groupbox within w_create_marc_file
end type
end forward

global type w_create_marc_file from w_main
integer width = 1445
integer height = 1308
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
dw_record_cnt dw_record_cnt
dw_get_prevbkno dw_get_prevbkno
dw_get_pubname dw_get_pubname
dw_get_anno dw_get_anno
dw_creat_marc_file dw_creat_marc_file
dw_distsched_inx dw_distsched_inx
st_transupdate st_transupdate
cb_print cb_print
dw_sp1_distsched_orl dw_sp1_distsched_orl
dw_sp5_dsweb dw_sp5_dsweb
dw_sp1_distsched dw_sp1_distsched
sle_batch_date sle_batch_date
dw_rs20_batch_tbl_with_libcd dw_rs20_batch_tbl_with_libcd
dw_rs20_batch_tbl dw_rs20_batch_tbl
dw_libcode dw_libcode
cbx_lib cbx_lib
uo_progress uo_progress
cb_oracle cb_oracle
st_marc_file st_marc_file
cb_cancel cb_cancel
cb_go cb_go
cb_marc_file cb_marc_file
gb_1 gb_1
st_sel_file st_sel_file
cb_sel_file cb_sel_file
gb_sel gb_sel
end type
global w_create_marc_file w_create_marc_file

type variables
str_distrib_schedule istr
datastore ids_distsched
long i_count=0
string is_yes_no='N'
end variables

forward prototypes
public function string wf_trpipe_tilde (string as_str)
public function integer wf_send_mail ()
end prototypes

event type integer ue_create_file();//
//long ll_rows, ll_count,  li_applen, li_len, ll_count3
//date  ld_cabdt
//string  ls_anno, ls_text
//string ls_title, ls_prvchno, ls_format, ls_bkno, ls_blk , ls_temp, ls_left
//string ls_cfile,  ls_date, ls_volumes, ls_boxes
//integer  li_filenum,  i, j
//string ls_bkmed, ls_bkseq, ls_auth, ls_authfn, ls_publisher
//string ls_pubyr, ls_srcdoc, ls_reissue, ls_vols, ls_casub,  ls_libcd
//string  ls_prevbkseq, ls_prevbkmed, ls_med
//string  ls_ordqty, ls_defqty,ls_prevbkno
//string ls_cabdt,ls_text1
//string ls_bklist[], ls_oldbkseq, ls_oldbkmed
//string ls_chno, ls_conno, ls_aepcd, ls_ttlart, ls_lang,ls_dewey, &
//		ls_ricd, ls_oneliner, ls_bttl1, ls_bttl2, ls_crname, ls_pubname
//long ll_defqty,li_pubyr, li_bkseq, li_cryr, li_apllen, li_vols, li_cnt
//long  ll_selqty, ll_ordqty,li_Row_count
//
//
//str_distrib_schedule lstr
//String ls_msgtext[ ] = {" 0 %","10 %", "20 %", "30 %", "40 %", "50 %", "60 %", &
//										"70 %", "80 %", "90 %", "100 %"}
//
//
//uo_progress.of_SetTextColor(RGB(255, 255, 255))
//
//lstr=istr
//ls_bklist[]= lstr.arraymed[]
//ld_cabdt=lstr.ld_date
////ld_distbdt= lstr.ld_date
//UpperBound( ls_bklist[])
//// get files
//
//
//ls_cfile = st_marc_file.text
//
//
//st_transupdate.text= "                         Retrieving Data ... "
//dw_creat_marc_file.settransobject(sqlservertrans)
//
//ll_rows = dw_creat_marc_file.retrieve(ld_cabdt)
//if ( ll_rows = 0) THEN
//	messagebox("No records found","Please select another set of books and try again.")
//	goto error
//end if
//
//if ll_rows = -1 then
//	messagebox("Database Error","There was an error during the retrieve.  Please try again.")
//	RollBack using SqlServerTrans;
//
//	goto error
//end if
//
//li_filenum = Fileopen(ls_cfile,streammode!,write!,lockwrite!,Replace!)
//if li_filenum = -1 then
//	messagebox("File Error","This file could not be opened.")
//	RollBack using SqlServerTrans;
//
//	goto error
//end if
//
////ls_text = "              " + ls_text1 + "             " + ls_text2+"~n"
////if lb_lineavail = true then
////	filewrite(li_filenum, ls_text)
////end if
//w_create_marc_file.SetRedraw(TRUE)
//uo_progress.visible=true
//uo_progress.of_SetMinimum(0)
//uo_progress.of_SetMaximum(ll_rows)
//uo_progress.of_SetDisplayStyle(3)
//uo_progress.of_SetMessageText(ls_msgtext)
//uo_progress.of_SetPosition(0)
//st_transupdate.text="                             Creating Files ..."
//ls_prvchno=''
//for ll_count = 1 to ll_rows
//	ls_text=''
////	if mod(ll_count,20)=0 then
//	uo_progress.of_Increment(1)
////	end if
//	if mod(ll_count,5)=0 then
//		w_create_marc_file.SetRedraw(TRUE)
//	else
//		w_create_marc_file.SetRedraw(false)
//	end if
//
//	ls_chno = trim(string(dw_creat_marc_file.object.chno[ll_count]))
//	ls_conno = trim(string(dw_creat_marc_file.object.conno[ll_count]))
//	ls_aepcd = trim(string(dw_creat_marc_file.object.aepcd[ll_count]))
//	ls_auth = trim(string(dw_creat_marc_file.object.auth[ll_count]))
//	ls_authfn = trim(string(dw_creat_marc_file.object.authfn[ll_count]))
//	ls_ttlart = trim(string(dw_creat_marc_file.object.ttlart[ll_count]))
//	ls_title = trim(dw_creat_marc_file.object.ttl[ll_count])
//	ls_publisher = trim(dw_creat_marc_file.object.publisher[ll_count])
//	li_pubyr = long(dw_creat_marc_file.object.pubyr[ll_count])
//	ls_med = trim(string(dw_creat_marc_file.object.med[ll_count]))
//	ls_lang = trim(string(dw_creat_marc_file.object.lang[ll_count]))
//	ls_casub = trim(string(dw_creat_marc_file.object.casub[ll_count]))
//	ls_bkmed = trim(string(dw_creat_marc_file.object.bkmed[ll_count]))
//	li_bkseq = dw_creat_marc_file.object.bkseq[ll_count]
//	ls_dewey = trim(string(dw_creat_marc_file.object.dewey[ll_count]))
//	ls_ricd = trim(dw_creat_marc_file.object.ricd[ll_count]	)
//	ls_oneliner = trim(string(dw_creat_marc_file.object.oneliner[ll_count]))
//	ls_bttl1 = trim(string(dw_creat_marc_file.object.b_ttl1[ll_count]))
//	ls_bttl2 = string(dw_creat_marc_file.object.b_ttl2[ll_count])
//	li_cryr = (dw_creat_marc_file.object.cryr[ll_count])
//	ls_crname = trim(string(dw_creat_marc_file.object.crname[ll_count]))
//	li_applen = (dw_creat_marc_file.object.applen[ll_count])
//	li_vols = (dw_creat_marc_file.object.vols[ll_count])
//	if ls_med='RC' THEN
//		ls_format='RC'
//	elseif ls_med='BR' THEN
//		ls_format='BR'
//	elseif ls_med='P/B' THEN
//		ls_format='PB'
//	else
//		ls_format='NA'
//	end if
//	if ls_prvchno<> ls_chno then
//		li_cnt=dw_get_anno.Retrieve(ls_chno)
//		if li_cnt>0 then
//			ls_anno=trim(dw_get_anno.object.anno[1])
//			ls_anno+=fill(' ',490)
//			ls_anno=mid(ls_anno,1,490)
//		else
//			ls_anno=fill(' ',490)
//		end if
//	end if
//	ls_text=ls_conno
//	if isnull(ls_med)=false and isNull(li_bkseq)=false then
//		ls_bkno=ls_med+string(li_bkseq)+'                '
//		ls_bkno=left(ls_bkno,8)
//		ls_text+= ls_bkno
//	else
//		ls_text+=ls_text+'        '
//	end if
//	if not IsNull(ls_casub) then
//		ls_casub+=ls_casub+'   '
//		ls_casub=left(ls_casub,3)
//		ls_text+= ls_casub
//	else
//		ls_text+='   '
//	end if
//	if not IsNull(ls_dewey) then
//		ls_dewey+=ls_dewey+'    '
//		ls_dewey=left(ls_dewey,4)
//		ls_text+= ls_dewey
//	else
//		ls_text+='    '
//	end if
//	if ls_format='RC' THEN
//		ls_text+='|'
//	else
//		ls_text+='f'
//	end if
//	ls_lang=lower(ls_lang)
//	if not IsNull(ls_lang) then
//		ls_lang+='   '
//		ls_lang=left(ls_lang,3)
//		ls_text+=ls_lang
//	else
//		ls_text+='   '
//	end if
//	if not IsNull(ls_auth) then
//		ls_auth= wf_trpipe_tilde(ls_auth)
//	end if
//	if not IsNull(ls_authfn) then
//		ls_authfn= wf_trpipe_tilde(ls_authfn)
//	end if
//	ls_blk=fill(' ',70)
//	choose case ls_aepcd
//		case 'I'
//			ls_auth+=ls_auth+' '
//			ls_auth+=ls_authfn
//			ls_text+= ls_auth
//		case 'N'
//			ls_text+=ls_blk
//		case 'L'
//			ls_auth+=ls_blk
//			ls_auth=mid(ls_auth,1,70)
//			ls_text+= ls_auth
//		case else
//			ls_auth+=', '+ls_authfn+ls_blk
//			ls_auth=mid(ls_auth,1,70)
//			ls_text+= ls_auth
//	end choose
//	li_len=0
//	ls_temp=''
//	ls_left=left(ls_ttlart,1)
//	if ls_left<>' ' and len(ls_ttlart) >0 then
//		ls_temp+=string(li_len + 1)+ls_ttlart+' '+ls_title	
//	else
//		ls_temp+='0'+ls_title	
//	end if
//	ls_temp=wf_trpipe_tilde(ls_temp)
//	ls_blk=fill('',355)
//	ls_temp+=ls_blk
//	ls_title=mid(ls_temp,1,355)
//	ls_text+= ls_title
//	li_cnt=dw_get_pubname.Retrieve(ls_chno)
//	if li_cnt>0 then
//		ls_pubname=dw_get_pubname.object.pubname[1]
//	else
//		ls_pubname=ls_publisher
//	end if
//	ls_blk=fill(' ',75)
//	ls_pubname+=ls_blk
//	ls_pubname=mid(ls_pubname,1,75)
//	ls_text+=ls_pubname
//	if not Isnull(li_pubyr) then
//		ls_temp=string(li_pubyr)
//		ls_temp+=ls_blk
//		ls_temp=left(ls_temp,4)
//	else
//		ls_temp=fill(' ',4)
//	end if
//	ls_text+=ls_temp
//	ls_text+= ls_anno
//	ls_blk=fill(' ',60)
//	if not IsNull(ls_oneliner) then
//		ls_oneliner+= ls_blk
//	else
//		ls_oneliner=ls_blk
//	end if
//	ls_oneliner=mid(ls_oneliner,1,60)
//	ls_text+=ls_oneliner
//	if Isnull(ls_ricd)=false then
//		li_cnt=dw_get_prevbkno.Retrieve(ls_conno)
//		if li_cnt>0 then
//			ls_prevbkno=dw_get_prevbkno.object.prevbkno[1]
//			ls_prevbkno+=fill(' ',8)
//			ls_prevbkno=mid(ls_prevbkno,1,8)
//		else
//			ls_prevbkno=fill(' ',8)
//		end if
//		if ls_ricd='RI' THEN
//			ls_prevbkno+='reissue'+'    '
//		else
//			ls_prevbkno+='rerecording'
//		end if
//	else
//		ls_prevbkno=fill(' ',19)
//	end if
//	ls_text+= ls_prevbkno
//	if IsNull(li_cryr)=false and IsNull(ls_crname)=false then
//		ls_crname=string(li_cryr)+ ls_crname+fill(' ',33)
//		ls_crname=mid(ls_crname,1,33)
//	else
//		ls_crname=fill(' ',33)
//	end if
//	ls_text+=ls_crname
//	
//	choose case ls_format
//		case 'RC'
//		if isnull(li_vols)=false then
//		elseif IsNull(li_applen)=false then
//			if mod(li_applen,4)=0 then
//				li_vols=int(li_applen/4)
//			else
//				li_vols=int(li_applen/4) +1
//			end if
//		else
//			li_vols=0
//		end if
//		if mod(li_vols,6)=0 then
//			ls_boxes=string(li_vols/6)
//		else
//			ls_boxes=string(int(li_vols/6) +1 )
//		end if
//		ls_volumes=string(li_vols)+' vol'
//		ls_boxes+= ls_boxes+' box'
//		case 'BR'
//			if IsNull(li_vols)=false then
//			elseif IsNull(li_applen)=false then
//				if mod(li_applen,250)=0 then
//					li_vols=int(li_applen/250)
//				else
//					li_vols=int(li_applen/250) + 1
//				end if
//			else
//				li_vols=0
//			end if
//			ls_volumes= string(li_vols)+' vol'
//			ls_boxes=''
//		case 'PB'
//			if IsNull(li_vols)=false then
//			else
//				li_vols=1
//			end if
//			ls_volumes=string(li_vols)+' vol'
//			ls_boxes=''
//		case else 
//			ls_volumes=''
//			ls_boxes=''
//	end choose
//	ls_volumes+=fill(' ',10)
//	ls_boxes+= fill(' ',10)
//	ls_volumes=left(ls_volumes,10)
//	ls_boxes=left(ls_boxes,10)
//	ls_text+=ls_volumes
//	ls_text+= ls_boxes
//	if IsNull(ls_bttl1)=false and ls_format='RC' then
//		ls_bttl1+= fill(' ',14)
//		ls_bttl1=left(ls_bttl1,14)
//	else
//		ls_bttl1=fill(' ',14)
//	end if
//	if IsNull(ls_bttl2)=false and ls_format='RC' then
//		ls_bttl2+= fill(' ',14)
//		ls_bttl2=left(ls_bttl2, 14)
//	else
//		ls_bttl2=fill(' ',14)
//	end if
//	ls_text+= ls_bttl1
//	ls_text+= ls_bttl2
//	if ls_format='RC' then
//		ls_text+='iss|kmnjlc|||ae'
//	else
//		ls_text+='atc            '
//	end if
//	ls_text+='\n'
//	filewrite(li_filenum, ls_text)
//	ls_text=''
//	if ll_count=ll_rows then
//		messagebox('Processed','~n***marcex- processed: '+string(ll_rows)+' rows')
//	end if
//next
//ll_count3=fileclose(li_filenum)
//w_create_marc_file.SetRedraw(TRUE)
//uo_progress.visible=false
//return 1
//error:
//
return -1
//
//
//
end event

event ue_cancel;call super::ue_cancel;close(this)
end event

event ue_dist_file();string ls_filename, ls_path, ls_today, ls_m, ls_d, ls_y, ls_cabdt
date ld_cabdt
str_distrib_schedule lstr

lstr=istr
ld_cabdt=lstr.ld_date

ls_cabdt=string(ld_cabdt,'mm/dd/yyyy')
ls_m=mid(ls_cabdt,1,2)
ls_d=mid(ls_cabdt,4,2)
ls_y=mid(ls_cabdt,7,4)
ls_filename="marc"+ls_m+ls_y
//ls_filename = "distrib.txt"
ls_path =  ls_filename
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

on w_create_marc_file.create
int iCurrent
call super::create
this.ole_ezftp=create ole_ezftp
this.dw_record_cnt=create dw_record_cnt
this.dw_get_prevbkno=create dw_get_prevbkno
this.dw_get_pubname=create dw_get_pubname
this.dw_get_anno=create dw_get_anno
this.dw_creat_marc_file=create dw_creat_marc_file
this.dw_distsched_inx=create dw_distsched_inx
this.st_transupdate=create st_transupdate
this.cb_print=create cb_print
this.dw_sp1_distsched_orl=create dw_sp1_distsched_orl
this.dw_sp5_dsweb=create dw_sp5_dsweb
this.dw_sp1_distsched=create dw_sp1_distsched
this.sle_batch_date=create sle_batch_date
this.dw_rs20_batch_tbl_with_libcd=create dw_rs20_batch_tbl_with_libcd
this.dw_rs20_batch_tbl=create dw_rs20_batch_tbl
this.dw_libcode=create dw_libcode
this.cbx_lib=create cbx_lib
this.uo_progress=create uo_progress
this.cb_oracle=create cb_oracle
this.st_marc_file=create st_marc_file
this.cb_cancel=create cb_cancel
this.cb_go=create cb_go
this.cb_marc_file=create cb_marc_file
this.gb_1=create gb_1
this.st_sel_file=create st_sel_file
this.cb_sel_file=create cb_sel_file
this.gb_sel=create gb_sel
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.ole_ezftp
this.Control[iCurrent+2]=this.dw_record_cnt
this.Control[iCurrent+3]=this.dw_get_prevbkno
this.Control[iCurrent+4]=this.dw_get_pubname
this.Control[iCurrent+5]=this.dw_get_anno
this.Control[iCurrent+6]=this.dw_creat_marc_file
this.Control[iCurrent+7]=this.dw_distsched_inx
this.Control[iCurrent+8]=this.st_transupdate
this.Control[iCurrent+9]=this.cb_print
this.Control[iCurrent+10]=this.dw_sp1_distsched_orl
this.Control[iCurrent+11]=this.dw_sp5_dsweb
this.Control[iCurrent+12]=this.dw_sp1_distsched
this.Control[iCurrent+13]=this.sle_batch_date
this.Control[iCurrent+14]=this.dw_rs20_batch_tbl_with_libcd
this.Control[iCurrent+15]=this.dw_rs20_batch_tbl
this.Control[iCurrent+16]=this.dw_libcode
this.Control[iCurrent+17]=this.cbx_lib
this.Control[iCurrent+18]=this.uo_progress
this.Control[iCurrent+19]=this.cb_oracle
this.Control[iCurrent+20]=this.st_marc_file
this.Control[iCurrent+21]=this.cb_cancel
this.Control[iCurrent+22]=this.cb_go
this.Control[iCurrent+23]=this.cb_marc_file
this.Control[iCurrent+24]=this.gb_1
this.Control[iCurrent+25]=this.st_sel_file
this.Control[iCurrent+26]=this.cb_sel_file
this.Control[iCurrent+27]=this.gb_sel
end on

on w_create_marc_file.destroy
call super::destroy
destroy(this.ole_ezftp)
destroy(this.dw_record_cnt)
destroy(this.dw_get_prevbkno)
destroy(this.dw_get_pubname)
destroy(this.dw_get_anno)
destroy(this.dw_creat_marc_file)
destroy(this.dw_distsched_inx)
destroy(this.st_transupdate)
destroy(this.cb_print)
destroy(this.dw_sp1_distsched_orl)
destroy(this.dw_sp5_dsweb)
destroy(this.dw_sp1_distsched)
destroy(this.sle_batch_date)
destroy(this.dw_rs20_batch_tbl_with_libcd)
destroy(this.dw_rs20_batch_tbl)
destroy(this.dw_libcode)
destroy(this.cbx_lib)
destroy(this.uo_progress)
destroy(this.cb_oracle)
destroy(this.st_marc_file)
destroy(this.cb_cancel)
destroy(this.cb_go)
destroy(this.cb_marc_file)
destroy(this.gb_1)
destroy(this.st_sel_file)
destroy(this.cb_sel_file)
destroy(this.gb_sel)
end on

event open;call super::open;
this.of_SetBase(true)
this.inv_base.of_Center()
IF  SQLServerOracleTrans.DBHandle() >0 THEN
	IF SQLServerOracleTrans.of_disconnect() < 0 THEN
		MessageBox("Error","Disconnected from the Oracle Database. Please try again",StopSign!)
	END IF
END IF

end event

event pfc_postopen;call super::pfc_postopen;str_distrib_schedule lstr
long li_bound, i
string ls_bkno_list[]

lstr=Message.PowerObjectParm
istr= lstr
//ls_bkno_list[]= lstr.arraymed[]

end event

type ole_ezftp from u_ezftp within w_create_marc_file
integer x = 1143
integer y = 1032
integer width = 1317
integer height = 768
integer taborder = 150
string binarykey = "w_create_marc_file.win"
end type

type dw_record_cnt from u_pics_dw within w_create_marc_file
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

type dw_get_prevbkno from u_pics_dw within w_create_marc_file
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

type dw_get_pubname from u_pics_dw within w_create_marc_file
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

type dw_get_anno from u_pics_dw within w_create_marc_file
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

type dw_creat_marc_file from u_pics_dw within w_create_marc_file
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

type dw_distsched_inx from u_pics_dw within w_create_marc_file
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

event retrieveend;call super::retrieveend;int li_loop

For li_loop = 1 to rowcount
	i_count++
	if uo_progress.visible=true and mod(i_count,10)=0 then
		uo_progress.of_Increment(1)
	end if
	if mod(i_count,100)=0 then
		parent.SetRedraw(TRUE)
	else
		parent.SetRedraw(false)
	end if
next
end event

type st_transupdate from statictext within w_create_marc_file
integer x = 32
integer y = 824
integer width = 1285
integer height = 148
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

type cb_print from commandbutton within w_create_marc_file
boolean visible = false
integer x = 375
integer y = 56
integer width = 50
integer height = 64
integer taborder = 90
boolean bringtotop = true
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "&Print"
boolean cancel = true
end type

event clicked;long rtn

dw_sp1_distsched_orl.triggerevent("pfc_print")

end event

type dw_sp1_distsched_orl from u_pics_dw within w_create_marc_file
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

event ue_addrow;long li_row_count

i_count++
if uo_progress.visible=true and mod(i_count,10)=0 then
	uo_progress.of_Increment(1)
end if
if mod(i_count,100)=0 then
	w_create_files.SetRedraw(TRUE)
else
	w_create_files.SetRedraw(false)
end if

end event

event constructor;call super::constructor;THIS.settransobject(sqlserveroracletrans)

end event

event sqlpreview;call super::sqlpreview;
	i_count++
	if uo_progress.visible=true and mod(i_count,10)=0 then
		uo_progress.of_Increment(1)
	end if
	if mod(i_count,100)=0 then
		parent.SetRedraw(TRUE)
	else
		parent.SetRedraw(false)
	end if
	
//	DO WHILE condition
//	statementblock
//	LOOP
	Do While SqlServerOracleTrans.of_isconnected()=false 
		SqlServerOracleTrans.of_connect()
		IF SqlServerOracleTrans.sqlcode <> 0 THEN
			IF SqlServerOracleTrans.SqlDbcode = -951 THEN            //check for invalid userid
			  MessageBox("Login Error","Invalid User ID/Password. Please re-enter.",StopSign!)
			ELSEIF SqlServerOracleTrans.SqlDBcode = -952 THEN       //check for invalid password
			  MessageBox("Login Error","Invalid User ID/Password. Please re-enter.",StopSign!)
			Else                                             //check for other error messages
				MessageBox("Database Connection Error","Unable to Connect. Pleas try again" ,&
				StopSign!)
			end if
		end if
	LOOP

////		SqlServerOracleTrans.DBParm = "PBCatalogOwner='PICADMIN'"
end event

event updateend;call super::updateend;//close(w_pics_retrieve_msg_box)
end event

event updatestart;call super::updatestart;//openwithparm(w_pics_retrieve_msg_box,"Updating distribution schedule table in RS21n, Please Wait...")
end event

type dw_sp5_dsweb from u_pics_dw within w_create_marc_file
boolean visible = false
integer x = 1445
integer y = 280
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

type dw_sp1_distsched from u_pics_dw within w_create_marc_file
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

type sle_batch_date from singlelineedit within w_create_marc_file
boolean visible = false
integer x = 101
integer y = 56
integer width = 50
integer height = 64
integer taborder = 50
boolean bringtotop = true
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
boolean autohscroll = false
borderstyle borderstyle = stylelowered!
end type

type dw_rs20_batch_tbl_with_libcd from u_pics_dw within w_create_marc_file
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

type dw_rs20_batch_tbl from u_pics_dw within w_create_marc_file
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

type dw_libcode from u_pics_dw within w_create_marc_file
boolean visible = false
integer x = 1166
integer y = 48
integer width = 55
integer height = 64
integer taborder = 190
string dataobject = "dddw_libcode"
boolean vscrollbar = false
boolean livescroll = false
end type

event constructor;call super::constructor;THIS.settransobject(sqlservertrans)

end event

type cbx_lib from checkbox within w_create_marc_file
boolean visible = false
integer x = 777
integer y = 884
integer width = 416
integer height = 64
integer textsize = -8
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 33554432
long backcolor = 67108864
string text = "Choose Library"
boolean lefttext = true
end type

event clicked;IF this.Checked THEN
	dw_libcode.visible = TRUE
	dw_libcode.retrieve()
ELSE
	dw_libcode.visible = FALSE
END IF
end event

type uo_progress from u_progressbar within w_create_marc_file
integer x = 32
integer y = 668
integer width = 1207
integer height = 92
integer taborder = 180
boolean border = true
borderstyle borderstyle = stylelowered!
end type

on uo_progress.destroy
call u_progressbar::destroy
end on

type cb_oracle from commandbutton within w_create_marc_file
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

event clicked;int rtn, li_count
date ld_distbdt
string ls_bklist[], ls_date
str_distrib_schedule lstr

lstr=istr
ls_bklist[]= lstr.arraymed[]
ld_distbdt= lstr.ld_date
li_count=UpperBound( ls_bklist[])

ls_date= string( ld_distbdt,'mm/dd/yyyy')

IF IsNull(ls_date) OR ls_date="" THEN
	MessageBox("ERROR","Batch date is null, please enter the date.")
	return
ELSE		
	ld_distbdt = date(ls_date)

	rtn = MessageBox("Data Migration","This process will migrate selected records from ~"sched~" table(RS20) into ~"distsched~" table(RS21)." +&
							" This process will take about 10-15 minutes. Continue?",Question!,YesNo!,1)
	IF rtn = 1	THEN
		//// Profile SP1PICS
		SqlServerOracleTrans.DBMS = "O73 ORACLE 7.3"
		SqlServerOracleTrans.LogPass = "picadmin"
		SqlServerOracleTrans.ServerName = "@TNS:ORAPICS"
		SqlServerOracleTrans.LogId = "picadmin"
		SqlServerOracleTrans.AutoCommit = False
		SqlServerOracleTrans.DBParm = "PBCatalogOwner='PICADMIN'"
		SqlServerOracleTrans.of_connect()
		IF SqlServerOracleTrans.sqlcode <> 0 THEN
			IF SqlServerOracleTrans.SqlDbcode = -951 THEN            //check for invalid userid
			  MessageBox("Login Error","Invalid User ID/Password. Please re-enter.",StopSign!)
			  Return -1	
			ELSEIF SqlServerOracleTrans.SqlDBcode = -952 THEN       //check for invalid password
			  MessageBox("Login Error","Invalid User ID/Password. Please re-enter.",StopSign!)
			  Return -1
			Else                                             //check for other error messages
			MessageBox("Database Connection Error","Unable to Connect. " +& 
			string(SqlServerOracleTrans.sqldbcode) + " " +&
			SqlServerOracleTrans.SQLErrText, &
			StopSign!)
			Return -1
		  END IF
		ELSE
			String ls_msgtext[ ] = {" 0 %","10 %", "20 %", "30 %", "40 %", "50 %", "60 %", &
											"70 %", "80 %", "90 %", "100 %"}
//			Integer li_count
			long ll_rows
			
			ll_rows = dw_sp5_dsweb.retrieve(ld_distbdt)
			IF ll_rows = 0 THEN
				MessageBox("ERROR","No rows found.")
			ELSE		
				uo_progress.Visible=TRUE
				uo_progress.of_SetMinimum(0)
				uo_progress.of_SetMaximum(ll_rows)
				uo_progress.of_SetDisplayStyle(3)
				uo_progress.of_SetMessageText(ls_msgtext)
				uo_progress.of_SetPosition(0)
				
				dw_sp5_dsweb.RowsCopy(1,ll_rows, Primary!, dw_sp1_distsched, 1, Primary!)
				
				dw_sp1_distsched.settransobject(sqlserveroracletrans)
				
				rtn = dw_sp1_distsched.of_Update(TRUE,TRUE)
				IF f_check_dberror(sqlserveroracletrans,"distsched at SP9") THEN
					IF rtn=1 THEN
						Commit Using sqlserveroracletrans;
						MessageBox("Update",string(dw_sp1_distsched.RowCount())+ " rows inserted into distsched table. ")
					ELSE
						MessageBox("Update"," Update distsched table failed.")
						Rollback Using sqlserveroracletrans;
						RETURN
					END IF
				END IF

				uo_progress.Visible=FALSE
			END IF		
		END IF
	END IF
END IF



end event

type st_marc_file from u_st within w_create_marc_file
integer x = 46
integer y = 212
integer width = 1010
integer taborder = 40
long backcolor = 1090519039
boolean border = true
borderstyle borderstyle = stylelowered!
end type

type cb_cancel from commandbutton within w_create_marc_file
integer x = 768
integer y = 1036
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

event clicked;IF NOT SQLServerOracleTrans.DBHandle() =0 THEN
	IF SQLServerOracleTrans.of_disconnect() < 0 THEN
		MessageBox("Error","Disconnected from the Oracle Database. Pleas try again.",StopSign!)
	END IF
END IF
//
//Parent.triggerevent("ue_cancel")
close(parent)
end event

type cb_go from commandbutton within w_create_marc_file
integer x = 256
integer y = 1032
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

String ls_anno, ls_text,ls_error_text
String ls_title, ls_prvchno, ls_format, ls_bkno, ls_blk , ls_temp, ls_left
String ls_date, ls_volumes, ls_boxes
Integer  li_filenum, li_filenum2, i, j, li_ftp_rc
String ls_bkmed, ls_bkseq, ls_auth, ls_authfn, ls_publisher
String ls_pubyr, ls_srcdoc, ls_reissue, ls_vols, ls_casub,  ls_libcd
String ls_prevbkseq, ls_prevbkmed, ls_med
String ls_ordqty, ls_defqty,ls_prevbkno
String ls_cabdt,ls_text1, ls_text2
String ls_bklist[], ls_oldbkseq, ls_oldbkmed, ls_usid
String ls_chno, ls_conno, ls_aepcd, ls_ttlart, ls_lang,ls_dewey, &
		ls_ricd, ls_oneliner, ls_bttl1, ls_bttl2, ls_crname, ls_pubname
Long ll_defqty,li_pubyr, li_bkseq, li_cryr, li_apllen, li_vols, li_cnt
Long  ll_selqty, ll_ordqty,li_row_count
Dec li_conno
Date ld_cabdt,ld_tday

DateTime ldt_cur,ld_cabdt_dt
String ls_cfile, ls_errorfile,ls_tday,ls_mday
String ls_ftpsite, ls_ftpuid, ls_ftppwd,ls_remoteloc,ls_marcfile

str_distrib_schedule lstr

ls_tday = string(today(),'mmyy')

//*****
ls_usid=sqlservertrans.userId
lstr=istr
ld_cabdt=lstr.ld_date
ld_cabdt_dt=DateTime(ld_cabdt,Time('00:00:00'))
ls_mday = string(ld_cabdt,'mmyy')

ls_cfile = st_marc_file.text
IF (ls_cfile = "" OR ls_cfile = "none") THEN
	Messagebox('MARC extract','Please enter a MARC file name.')
	st_marc_file.SetFocus()
	RETURN 
END IF
IF w_batch_formation.cbx_file.checked=FALSE THEN
	w_batch_formation.cbx_file.enabled=TRUE
	
	String ls_msgtext[ ] = {" 0 %","10 %", "20 %", "30 %", "40 %", "50 %", "60 %", &
				"70 %", "80 %", "90 %", "100 %"}
	uo_progress.of_SetTextColor(Rgb(255, 255, 255))
	
	ls_cfile = st_marc_file.text
	// Error file
	ls_errorfile = 'c:\program files\picsorcl9i\error\marcerr'+ls_tday+'.txt' // 09/03/09

	
	st_transupdate.text= "                         Retrieving data ... "
	dw_creat_marc_file.SetTransObject(sqlservertrans)
	
	ll_rows = dw_creat_marc_file.Retrieve(ld_cabdt_dt)
	FOR i=1 TO ll_rows
		ls_conno = Trim(String(dw_creat_marc_file.object.conno[i]))
		ls_left=Left(ls_conno,1)
		li_conno=Long(ls_conno)
		//do following on the purpose of sorting by control number,'9799????','9899????'
		//'9999????,'0099????' and '0199????' and so on
		IF ls_left='0' THEN
			li_conno= li_conno+2000*1000000
		ELSE
			li_conno= li_conno+1900*1000000
		END IF
		dw_creat_marc_file.object.sort[i]=li_conno
	NEXT
	dw_creat_marc_file.Sort()
	IF (ll_rows = 0) THEN
		Messagebox("Error","System error while retrieving."+&
			"~nPlease contact system administrator.")
		RETURN
	END IF
	
	IF ll_rows = -1 THEN
		Messagebox("Error","System error while retrieving."+&
			"~nPlease contact system administrator.")
		ROLLBACK USING sqlservertrans;
	
		RETURN
	END IF
	
	li_filenum = FileOpen(ls_cfile,streamMode!,write!,lockWrite!,replace!)
	IF li_filenum = -1 THEN
		Messagebox("File Error","The file "+ls_cfile+" could not be opened.")
		ROLLBACK USING sqlservertrans;
		RETURN
	ELSE
		li_filenum2 = FileOpen(ls_errorfile,streamMode!,write!,lockWrite!,replace!)
		IF li_filenum2 = -1 THEN
			Messagebox("File Error","The error file "+ls_errorfile+" could not be opened.")
			ROLLBACK USING sqlservertrans;
			RETURN
		END IF		
	END IF
	w_create_marc_file.SetRedraw(TRUE)
	uo_progress.visible=TRUE
	uo_progress.of_SetMinimum(0)
	uo_progress.of_SetMaximum(ll_rows)
	uo_progress.of_SetDisplayStyle(3)
	uo_progress.of_SetMessageText(ls_msgtext)
	uo_progress.of_SetPosition(0)
	st_transupdate.text="                             Creating files ..."
	ls_prvchno=''
	ls_error_text = "Marc file missed records"
	ls_error_text+='~n'
		
	FOR ll_count = 1 TO ll_rows
		ls_text=''
		uo_progress.of_Increment(1)
		IF Mod(ll_count,5)=0 THEN
			w_create_marc_file.SetRedraw(TRUE)
		ELSE
			w_create_marc_file.SetRedraw(FALSE)
		END IF
	
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
//		IF ls_med='RC' OR ls_med='RTB' THEN
		// 04/29/2008 for RC and DB media must be RTB
		IF ls_bkmed='RC' OR ls_bkmed='DB' THEN
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
		
	//	IF ls_format='RC' THEN
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
				ls_auth+=ls_authfn+ ls_blk
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
				ls_auth+=', '+ls_authfn+ls_blk
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
			IF ls_bkmed = 'RC' THEN
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
			ELSE
			// PROCESS DB
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

			END IF
		
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
	//	if IsNull(ls_bttl1)=false and ls_format='RC' then
	//		ls_bttl1+= fill(' ',14)
	//		ls_bttl1=left(ls_bttl1,14)
	//	else
	//		ls_bttl1='abcdefgabcdefg'
	//	end if
	//	if IsNull(ls_bttl2)=false and ls_format='RC' then
	//		ls_bttl2+= fill(' ',14)
	//		ls_bttl2=left(ls_bttl2, 14)
	//	else
	//		ls_bttl2='hijklmnhijklmn'
	//	end if
	//	ls_text+= ls_bttl1
	//	ls_text+= ls_bttl2
		// 04/29/2008
//		IF ls_format='RC' THEN
		IF ls_format='RTB' THEN
			ls_bttl1='abcdefg' +Fill(' ',7) 
			ls_bttl2='hijklmn' +Fill(' ',7)      
		ELSE
			ls_bttl1=Fill(' ',14)
			ls_bttl2=Fill(' ',14)
		END IF
		ls_text+= ls_bttl1
		ls_text+= ls_bttl2
		
		// 04/29/2008
//		IF ls_format='RC' THEN
		IF ls_format='RTB' THEN
			ls_text+='iss|kmnjlc|||ae'
		ELSE
			ls_text+='atc'+Fill(' ',12)           
		END IF
		ls_text+='~n'
		IF IsNull(ls_text) OR ls_text='' THEN
			ll_err_cnt++ // this means this row did not get inserted
			ls_error_text += "Control number : "+ls_conno+", book number : "+ls_bkmed+ls_bkseq+" did not make it to the list. ~n"
		END IF
		FileWrite(li_filenum, ls_text)
		ls_text=''
		ls_prvchno= ls_chno
	NEXT
	if ll_err_cnt>0 then
		FileWrite(li_filenum2, ls_error_text)
		messagebox('Rows missed','The file '+ls_errorfile+' contains the error list.')
	else
		
		SELECT ftp_site, ftp_uid, ftp_pwd
		INTO :ls_ftpsite, :ls_ftpuid, :ls_ftppwd
		FROM PCS_FTP_INFO
		USING SQLServerTrans;
		IF f_check_dberror(SQLServerTrans,"SELECTING FROM PCS_FTP_INFO ") THEN
	
			li_ftp_rc = w_create_marc_file.ole_ezftp.uf_login ( ls_ftpsite, ls_ftpuid, ls_ftppwd)
	
			IF li_ftp_rc = -1 THEN
				messagebox("FTP Error", "Unable to connect to ftp server for placing the MARC file.")
			ELSE
				ls_cfile = st_marc_file.text
				ls_marcfile = 'IMarc'+ls_mday
				ls_remoteloc = "/pics/prd/htdocs/download/marcfile/"
				w_create_marc_file.ole_ezftp.uf_set_currentdirectory(ls_remoteloc)
				ls_marcfile = TRIM(ls_marcfile)
				ls_cfile = TRIM(ls_cfile)
				messagebox('file',' ls_marcfile = '+ls_marcfile+' ls_cfile ='+ls_cfile+' Remote location = '+ls_remoteloc+' -- cabdt='+string(ld_cabdt))
				w_create_marc_file.ole_ezftp.uf_upload ( ls_cfile, ls_marcfile , FALSE )
					
				DECLARE LOADMARC PROCEDURE FOR LOADMARC(:ls_marcfile,:ld_cabdt_dt,'I')
				USING SQLServerTrans; 
					
				EXECUTE LOADMARC;
					
				IF f_check_dberror(SQLServerTrans,'Updating copyallot table')=FALSE THEN 		
					ROLLBACK USING SQLServerTrans;
				ELSE
					COMMIT USING SqlServerTrans;
					ls_error_text += "No error in this batch of books"
					FileWrite(li_filenum2, ls_error_text)		
				end if
			END IF
		ELSE
			messagebox("FTP Error", "Unable to get ftp information.")
			li_ftp_rc = -1	
		END IF
	end if
	ldt_cur= DateTime(Today(), Now())
	// the 4th step is finished updat thd datetime marcexp
	w_batch_formation.dw_batch_process.Retrieve(ld_cabdt_dt)
	w_batch_formation.dw_batch_process.object.marcexp[1]= ldt_cur
	w_batch_formation.dw_batch_process.object.marcrows[1]= ll_rows
	w_batch_formation.dw_batch_process.object.marcuser[1]= ls_usid
	rtn=w_batch_formation.dw_batch_process.Update(TRUE,TRUE)
	IF rtn=1 THEN
		COMMIT USING sqlservertrans;
		w_batch_formation.st_marc.text=String(ldt_cur, 'mm/dd/yyyy hh:mm:ss')
		w_batch_formation.cbx_file.checked=TRUE
	ELSE
		ROLLBACK USING sqlservertrans;
		w_batch_formation.cbx_file.enabled=FALSE
		w_batch_formation.cbx_file.checked=FALSE
	END IF
END IF// end if cbx_file.checked=falae

// Close the two files
rtn=FileClose(li_filenum)
rtn=FileClose(li_filenum2)

w_create_marc_file.SetRedraw(TRUE)

uo_progress.of_SetMinimum(0)
uo_progress.of_SetMaximum(ll_rows)
uo_progress.of_SetDisplayStyle(3)
uo_progress.of_SetMessageText(ls_msgtext)
uo_progress.of_SetPosition(0)
w_create_marc_file.uo_progress.visible=FALSE
st_transupdate.text=''
//******
IF w_batch_formation.cbx_email.checked=FALSE THEN
	w_batch_formation.cbx_email.enabled=TRUE
	rtn=wf_send_mail()// the last step( 5th step) is send email
	IF rtn=1 THEN
		ldt_cur= DateTime(Today(), Now())
		// finish step 5 and batch formation all done
		w_batch_formation.dw_batch_process.Retrieve(ld_cabdt_dt)
		w_batch_formation.dw_batch_process.object.marcemail[1]= ldt_cur
		rtn=w_batch_formation.dw_batch_process.Update(TRUE,TRUE)
		IF rtn=1 THEN
			COMMIT USING sqlservertrans;
			w_batch_formation.st_email.text=String(ldt_cur, 'mm/dd/yyyy hh:mm:ss')
			w_batch_formation.cbx_email.checked=TRUE
		ELSE
			ROLLBACK USING sqlservertrans;
			w_batch_formation.cbx_email.checked=FALSE
			w_batch_formation.cbx_email.checked=FALSE
		END IF
	ELSE
		w_batch_formation.cbx_email.enabled=FALSE
		w_batch_formation.cbx_email.checked=FALSE
	END IF
END IF// end if cbx_email.checked=false
ib_disableclosequery=TRUE
IF IsValid(w_create_marc_file) THEN
	Close(parent)
END IF

 





end event

type cb_marc_file from commandbutton within w_create_marc_file
integer x = 1070
integer y = 216
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

type gb_1 from groupbox within w_create_marc_file
integer x = 23
integer y = 156
integer width = 1207
integer height = 148
integer taborder = 20
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long backcolor = 79741120
string text = "Distribution Schedule Filename:"
end type

type st_sel_file from u_st within w_create_marc_file
boolean visible = false
integer x = 46
integer y = 504
integer width = 1010
integer taborder = 60
long backcolor = 1090519039
boolean border = true
borderstyle borderstyle = stylelowered!
end type

type cb_sel_file from commandbutton within w_create_marc_file
boolean visible = false
integer x = 1070
integer y = 504
integer width = 119
integer height = 68
integer taborder = 80
boolean bringtotop = true
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "Set"
end type

event clicked;Parent.triggerevent("ue_sel_file")
end event

type gb_sel from groupbox within w_create_marc_file
boolean visible = false
integer x = 27
integer y = 444
integer width = 1207
integer height = 148
integer taborder = 30
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long backcolor = 79741120
string text = "Quantities Selected Filename:"
end type


Start of PowerBuilder Binary Data Section : Do NOT Edit
02w_create_marc_file.bin 
2000000c00e011cfd0e11ab1a1000000000000000000000000000000000003003e0009fffe000000060000000000000000000000010000000100000000000010000000000200000001fffffffe0000000000000000fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffd00000004fffffffefffffffefffffffeffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff006f00520074006f004500200074006e00790072000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000050016ffffffffffffffff00000001000000000000000000000000000000000000000000000000000000003f0f939001cacf4c00000003000000800000000000500003004f0042005800430054005300450052004d0041000000000000000000000000000000000000000000000000000000000000000000000000000000000102001affffffff00000002ffffffff000000000000000000000000000000000000000000000000000000000000000000000000fffffffe0000000000000000004200500043004f00530058004f00540041005200450047000000000000000000000000000000000000000000000000000000000000000000000000000000000001001affffffffffffffff000000036580f76711cf781945446cb800005453000000003f0f939001cacf4c3f0f939001cacf4c000000000000000000000000004f00010065006c00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000102000affffffff00000004ffffffff000000000000000000000000000000000000000000000000000000000000000000000000000000000000001400000000fffffffefffffffeffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
2Affffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff02000001000000080000000000000000000000000030003500320037003b003700320000003100300106003d003800310015036000150360003900300000002000010000000003200000032000000000000000000000000000730069007200740062006900740075006f0069005f006e0063007300650068007500640065006c0077002e006e006900280020002900780028002000320032003000340036003600200029002f0033003500320032002f0031003000200030003800310030003a003a00370030003100000020007700770062005f00740061006800630066005f0072006f0061006d00690074006e006f0077002e006e006900280020002900780028002000380031003200320038003300200029002f0033003500320032002f0031003000200030003800310030003a003a00370032003100000020007700770063005f005f0061006f006c006400610064005f006600650069006c002e0062006900770020006e007800280020002900370028003500380038003400200029002f0033003500320032002f0031003000200030003800310030003a003a0037003300310000002000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000006f00430074006e006e00650073007400000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000020012ffffffffffffffffffffffff0000000000000000000000000000000000000000000000000000000000000000000000000000000100000018000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000ffffffffffffffffffffffff0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000ffffffffffffffffffffffff0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000ffffffffffffffffffffffff00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
12w_create_marc_file.bin 
End of PowerBuilder Binary Data Section : No Source Expected After This Point

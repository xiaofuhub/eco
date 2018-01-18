$PBExportHeader$w_pa_create_ftp_opf_files.srw
forward
global type w_pa_create_ftp_opf_files from w_response
end type
type st_3 from statictext within w_pa_create_ftp_opf_files
end type
type dw_dtbbkseq from u_pics_dw within w_pa_create_ftp_opf_files
end type
type ole_ezftp from u_ezftp within w_pa_create_ftp_opf_files
end type
type dw_prdr from u_pics_dw within w_pa_create_ftp_opf_files
end type
type sle_bklist from singlelineedit within w_pa_create_ftp_opf_files
end type
type st_2 from statictext within w_pa_create_ftp_opf_files
end type
type st_1 from statictext within w_pa_create_ftp_opf_files
end type
type cb_opf from commandbutton within w_pa_create_ftp_opf_files
end type
type cb_close from commandbutton within w_pa_create_ftp_opf_files
end type
end forward

global type w_pa_create_ftp_opf_files from w_response
integer width = 1682
integer height = 1452
string title = "Create and FTP opf files to producers"
st_3 st_3
dw_dtbbkseq dw_dtbbkseq
ole_ezftp ole_ezftp
dw_prdr dw_prdr
sle_bklist sle_bklist
st_2 st_2
st_1 st_1
cb_opf cb_opf
cb_close cb_close
end type
global w_pa_create_ftp_opf_files w_pa_create_ftp_opf_files

on w_pa_create_ftp_opf_files.create
int iCurrent
call super::create
this.st_3=create st_3
this.dw_dtbbkseq=create dw_dtbbkseq
this.ole_ezftp=create ole_ezftp
this.dw_prdr=create dw_prdr
this.sle_bklist=create sle_bklist
this.st_2=create st_2
this.st_1=create st_1
this.cb_opf=create cb_opf
this.cb_close=create cb_close
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_3
this.Control[iCurrent+2]=this.dw_dtbbkseq
this.Control[iCurrent+3]=this.ole_ezftp
this.Control[iCurrent+4]=this.dw_prdr
this.Control[iCurrent+5]=this.sle_bklist
this.Control[iCurrent+6]=this.st_2
this.Control[iCurrent+7]=this.st_1
this.Control[iCurrent+8]=this.cb_opf
this.Control[iCurrent+9]=this.cb_close
end on

on w_pa_create_ftp_opf_files.destroy
call super::destroy
destroy(this.st_3)
destroy(this.dw_dtbbkseq)
destroy(this.ole_ezftp)
destroy(this.dw_prdr)
destroy(this.sle_bklist)
destroy(this.st_2)
destroy(this.st_1)
destroy(this.cb_opf)
destroy(this.cb_close)
end on

type st_3 from statictext within w_pa_create_ftp_opf_files
integer x = 96
integer y = 976
integer width = 1422
integer height = 148
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "(Note: Enter book numbers, separated by commas. Example: 52300, 45678,11232)"
boolean focusrectangle = false
end type

type dw_dtbbkseq from u_pics_dw within w_pa_create_ftp_opf_files
boolean visible = false
integer x = 325
integer y = 1228
integer width = 119
integer height = 84
integer taborder = 0
string dataobject = "d_dtbbkseq"
boolean vscrollbar = false
boolean livescroll = false
boolean ib_isupdateable = false
boolean ib_rmbmenu = false
boolean ib_rmbfocuschange = false
end type

event constructor;call super::constructor;this.of_SetTransObject(SQLServerTrans)
this.of_SetFilter(TRUE)
this.of_SetFind(TRUE)
this.inv_filter.of_SetStyle(1)
this.inv_filter.of_SetColumnNameSource(2)
this.of_SetSort(TRUE)
this.inv_sort.of_SetStyle(1)
this.inv_sort.of_SetColumnNameSource(2)

end event

type ole_ezftp from u_ezftp within w_pa_create_ftp_opf_files
boolean visible = false
integer x = 128
integer y = 1208
integer taborder = 0
string binarykey = "w_pa_create_ftp_opf_files.win"
end type

type dw_prdr from u_pics_dw within w_pa_create_ftp_opf_files
integer x = 581
integer y = 52
integer width = 329
integer height = 108
integer taborder = 10
string dataobject = "d_pa_opf_prdr"
boolean vscrollbar = false
boolean livescroll = false
end type

event constructor;call super::constructor;this.of_SetTransObject( SQLServerTrans )
this.retrieve()

end event

type sle_bklist from singlelineedit within w_pa_create_ftp_opf_files
integer x = 87
integer y = 336
integer width = 1458
integer height = 624
integer taborder = 20
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
borderstyle borderstyle = stylelowered!
end type

type st_2 from statictext within w_pa_create_ftp_opf_files
integer x = 69
integer y = 248
integer width = 562
integer height = 64
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "Book Number List: "
boolean focusrectangle = false
end type

type st_1 from statictext within w_pa_create_ftp_opf_files
integer x = 69
integer y = 72
integer width = 512
integer height = 80
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "Producer/Contract"
boolean focusrectangle = false
end type

type cb_opf from commandbutton within w_pa_create_ftp_opf_files
integer x = 503
integer y = 1220
integer width = 645
integer height = 112
integer taborder = 30
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Create/FTP OPF Files"
end type

event clicked;n_ds lds
n_cst_string 	inv_string

String Lcntrtype,ls_cfile,ls_dc_url, ls_ttl, ls_anno, ls_pub,	 ls_t_date, ls_creator, ls_subject, ls_remotefile, ls_remoteloc,ls_prdr 
String ls_forma, ls_Identifier, ls_isbn, ls_lang, ls_t_rights, ls_pubyr, ls_sourcepublisher, ls_sourcerights
String ls_aType, ls_dtbnarrator, ls_dtbproducer, ls_dtbproducedate, ls_dtbrevision, ls_dtbrevisiondate, ls_dtbrevisiondescription
string ls_t_metadata, ls_t_dcmetadata, ls_t_dcmetadata2, ls_t_x_metadata, ls_t_xx_metadata, ls_t_close_metadata
String ls_dtbtotaltime,	 ls_dtbaudioformat , lmsg , ls_xmlver, ls_doctype, ls_xmlnls, ls_package, ls_manifest, ls_spine,ls_chno,ls_text    
String ls_ftpsite, ls_ftpuid, ls_ftppwd
string ls_source,ls_comma[]

int ll_rows,ll_rows2,rtn,i,j,li_ftp_rc,cnt=0,cnt_comma
long l_bkseq[],li_filenum,li_bkseq

SetNull(ls_creator)
SetNull(ls_subject)

// Create and load the datastore

lds = create n_ds
lds.dataobject = "d_dtbcreators"

SELECT ftp_site, ftp_uid, ftp_pwd
INTO :ls_ftpsite, :ls_ftpuid, :ls_ftppwd
FROM PCS_FTP_INFO
USING SQLServerTrans;
IF f_check_dberror(SQLServerTrans,"SELECTING FROM PCS_FTP_INFO ") THEN

	li_ftp_rc = w_pa_create_ftp_opf_files.ole_ezftp.uf_login ( ls_ftpsite, ls_ftpuid, ls_ftppwd)
	
	IF li_ftp_rc = -1 THEN
		messagebox("FTP Error", "Unable to connect to ftp server.")
	ELSE
		ls_prdr = lower(dw_prdr.object.prdr[1])
		ls_remoteloc = "/pics/prd/opffiles/"+ls_prdr
		w_pa_create_ftp_opf_files.ole_ezftp.uf_set_currentdirectory(ls_remoteloc)
	END IF
ELSE
	messagebox("FTP Error", "Unable to get ftp information.")
	li_ftp_rc = -1
	
END IF


// Get the contract type
Lcntrtype = dw_prdr.object.cntrtype[1]

IF Lcntrtype="D" THEN
	
	// Popluate the array of bkseq
	ls_source = sle_bklist.text
	cnt_comma = inv_string.of_CountOccurrences(ls_source, ",", TRUE)
	cnt = inv_string.of_Parsetoarray	(ls_source, ",", ls_comma)
	IF cnt=0 THEN
		l_bkseq[1] = long(ls_source)
	ELSE
		FOR i=1 TO cnt_comma+1
			l_bkseq[i]=long(ls_comma[i])
			cnt = cnt_comma+1
		NEXT
	END IF	
	
	OpenWithParm(w_pics_retrieve_msg_box,"Getting DTD records, Please Wait...")
	dw_dtbbkseq.SetTransObject(SQLServerTrans)
	ll_rows = dw_dtbbkseq.retrieve(l_bkseq[])
	
	Close(w_pics_retrieve_msg_box)
	if ll_rows = 0 THEN
		MessageBox("ERROR", "No record(s) was found.")
		return
	else
		// record found and now create the file from the datawindow dtbbkseqs
		FOR i= 1 TO ll_rows

			 ls_xmlver = ""
			 ls_doctype = ""
			 ls_xmlnls = ""
			 ls_dc_url = ""
			 ls_ttl = ""
			 ls_anno = ""			   
			 ls_pub = ""
			 ls_t_date = ""			   
			 ls_forma = ""
			 ls_Identifier = ""
			 ls_isbn= ""			   
			 ls_lang= ""
			 ls_t_rights= ""
			 ls_pubyr= ""
			 ls_sourcepublisher= ""
			 ls_sourcerights= ""
			 ls_aType= ""
			 ls_dtbnarrator= ""
			 ls_dtbproducer= ""
			 ls_dtbproducedate= ""
			 ls_dtbrevision= ""
			 ls_dtbrevisiondate= ""
			 ls_dtbrevisiondescription= ""
			 ls_dtbtotaltime= ""
			 ls_dtbaudioformat= ""   
			 ls_manifest = ""
			 ls_spine = ""
			 ls_package = ""
			 ls_creator = ""
			 ls_text = ""
			 ls_subject = ""
			 ls_chno = ""
			
			 ls_cfile= ""
			 li_filenum = -1
			
		 	 li_bkseq = dw_dtbbkseq.object.mchar_bkseq[i]
			 ls_cfile="P:\opffiles\"+string(li_bkseq)+".opf"
			 ls_remotefile = string(li_bkseq)+".opf"
			 li_filenum = Fileopen(ls_cfile,streammode!,write!,shared!,Replace!)
			 if li_filenum = -1 then
				messagebox("File Error","This file could not be opened. Make sure the directory P:\opffiles exist.")
				Close(w_pics_retrieve_msg_box)
				RollBack using SqlServerTrans;
			
				RETURN -1
			 end if

			 lmsg = "Getting DTD file "+ls_cfile+", Please Wait..." 
			 OpenWithParm(w_pics_retrieve_msg_box,lmsg)
			
			 ls_chno = dw_dtbbkseq.object.chno[i]
			 ls_xmlver = dw_dtbbkseq.object.t_xmlver[i]+"~n"
			 ls_doctype = dw_dtbbkseq.object.t_doctype[i]+"~n"
			 ls_xmlnls = dw_dtbbkseq.object.t_xmlnls[i]+"~n"
			 ls_t_metadata = dw_dtbbkseq.object.t_metadata[i]+"~n"
			 ls_dc_url = dw_dtbbkseq.object.dc_url[i]+"~n"
//			 ls_ttl = f_change_pipe_italics_tags(f_replace_nonprn_html_tags(dw_dtbbkseq.object.ttl[i]))+"~n"
			 ls_ttl = f_replace_nonprn_html_tags(dw_dtbbkseq.object.ttl[i])+"~n"
			 
			 ll_rows2 = lds.settransobject(sqlservertrans)
			 ll_rows2 = lds.retrieve(ls_chno)
			 
			 ls_creator = ' '
			 FOR j = 1 to ll_rows2
				ls_text = lds.object.t_creators[j]+"~n"
				ls_creator = ls_creator + ls_text 			
			 NEXT
			 ls_creator = Trim(ls_creator)
			 
			 ls_subject = dw_dtbbkseq.object.t_subject[i]+"~n"
//			 ls_anno = f_change_pipe_italics_tags(f_replace_nonprn_html_tags(dw_dtbbkseq.object.anno[i]))+"~n"   
			 ls_anno = f_replace_nonprn_html_tags(dw_dtbbkseq.object.anno[i])+"~n"   
			 ls_pub = dw_dtbbkseq.object.pub[i]+"~n"
			 ls_t_date = dw_dtbbkseq.object.t_date[i]+"~n"
			 ls_forma = dw_dtbbkseq.object.forma[i]+"~n"
			 ls_Identifier = dw_dtbbkseq.object.Identifier[i]+"~n"
			 ls_isbn= dw_dtbbkseq.object.isbn[i]+"~n"
			 ls_lang= dw_dtbbkseq.object.lang[i]+"~n"
			 ls_t_rights= dw_dtbbkseq.object.t_rights[i]+"~n"
			 ls_t_dcmetadata = dw_dtbbkseq.object.t_dcmetadata[i]+"~n"
			 ls_t_x_metadata = dw_dtbbkseq.object.t_x_metadata[i]+"~n"
			 ls_pubyr= dw_dtbbkseq.object.pubyr[i]+"~n"
			 ls_sourcepublisher= dw_dtbbkseq.object.sourcepublisher[i]+"~n"
			 ls_sourcerights= dw_dtbbkseq.object.sourcerights[i]+"~n"
			 ls_aType= dw_dtbbkseq.object.aType[i]+"~n"
			 ls_dtbnarrator= dw_dtbbkseq.object.dtbnarrator[i]+"~n"
			 ls_dtbproducer= dw_dtbbkseq.object.dtbproducer[i]+"~n"
			 ls_dtbproducedate= dw_dtbbkseq.object.dtbproducedate[i]+"~n"
			 ls_dtbrevision= dw_dtbbkseq.object.dtbrevision[i]+"~n"
			 ls_dtbrevisiondate= dw_dtbbkseq.object.dtbrevisiondate[i]+"~n"
			 ls_dtbrevisiondescription= dw_dtbbkseq.object.dtbrevisiondescription[i]+"~n"
			 ls_dtbtotaltime= dw_dtbbkseq.object.dtbtotaltime[i]+"~n"
			 ls_dtbaudioformat= dw_dtbbkseq.object.dtbaudioformat[i]+"~n"
			 ls_t_xx_metadata = dw_dtbbkseq.object.t_xx_metadata[i]+"~n"
			 ls_t_close_metadata = dw_dtbbkseq.object.t_close_metadata[i]+"~n"
			 ls_manifest = dw_dtbbkseq.object.t_manifest[i]+"~n"
			 ls_spine = dw_dtbbkseq.object.t_spine[i]+"~n"
			 ls_package = dw_dtbbkseq.object.t_package[i]+"~n"
			 
			filewrite(li_filenum, ls_xmlver)
			filewrite(li_filenum, ls_doctype)
			filewrite(li_filenum, ls_xmlnls)
			filewrite(li_filenum, ls_t_metadata)
			filewrite(li_filenum, ls_dc_url)
			filewrite(li_filenum, ls_ttl)
			filewrite(li_filenum, ls_creator)
			filewrite(li_filenum, ls_subject)
			filewrite(li_filenum, ls_anno)
			filewrite(li_filenum, ls_pub)
			filewrite(li_filenum, ls_t_date)
			filewrite(li_filenum, ls_forma)
			filewrite(li_filenum, ls_Identifier)
			filewrite(li_filenum, ls_isbn)
			filewrite(li_filenum, ls_lang)
			filewrite(li_filenum, ls_t_rights)
			filewrite(li_filenum, ls_t_dcmetadata)
			filewrite(li_filenum, ls_t_x_metadata)
			filewrite(li_filenum, ls_pubyr)
			filewrite(li_filenum, ls_sourcepublisher)
			filewrite(li_filenum, ls_sourcerights)
			filewrite(li_filenum, ls_aType)
			filewrite(li_filenum, ls_dtbnarrator)
			filewrite(li_filenum, ls_dtbproducer)
			filewrite(li_filenum, ls_dtbproducedate)
			filewrite(li_filenum, ls_dtbrevision)
			filewrite(li_filenum, ls_dtbrevisiondate)
			filewrite(li_filenum, ls_dtbrevisiondescription)
			filewrite(li_filenum, ls_dtbtotaltime)
			filewrite(li_filenum, ls_dtbaudioformat)
			filewrite(li_filenum, ls_t_xx_metadata)
			filewrite(li_filenum, ls_t_close_metadata)
			filewrite(li_filenum, ls_manifest)
			filewrite(li_filenum, ls_spine)
			filewrite(li_filenum, ls_package)

			fileclose(li_filenum)

			IF li_ftp_rc <> -1 THEN
				w_pa_create_ftp_opf_files.ole_ezftp.uf_upload ( ls_cfile, ls_remotefile , FALSE )
			END IF

		NEXT
		Close(w_pics_retrieve_msg_box)
		
		MessageBox("opf files","OPF files has been created and placed in P:\opffiles network directory.")
	
	end if
	
ELSE // This is total contract or just narration
	
	// Popluate the array of bkseq
	ls_source = sle_bklist.text
	cnt_comma = inv_string.of_CountOccurrences(ls_source, ",", TRUE)
	cnt = inv_string.of_Parsetoarray	(ls_source, ",", ls_comma)
	IF cnt=0 THEN
		l_bkseq[1] = long(ls_source)
	ELSE
		FOR i=1 TO cnt_comma+1
			l_bkseq[i]=long(ls_comma[i])
		NEXT
		cnt = cnt_comma+1
	END IF
	
		
	OpenWithParm(w_pics_retrieve_msg_box,"Getting DTD records, Please Wait...")
	dw_dtbbkseq.SetTransObject(SQLServerTrans)
	ll_rows = dw_dtbbkseq.retrieve(l_bkseq[])
	
	Close(w_pics_retrieve_msg_box)
	if ll_rows = 0 THEN
		MessageBox("ERROR", "No record(s) was found.")
		return
	else
		lmsg = "Making DTD files, Please Wait..." 
		OpenWithParm(w_pics_retrieve_msg_box,lmsg)
		
		
		// record found and now create the file from the datawindow dtbbkseqs
		//messagebox('books',string(ll_rows))
		FOR i= 1 TO ll_rows
				 ls_xmlver = ""
				 ls_doctype = ""
				 ls_xmlnls = ""
				 ls_dc_url = ""
				 ls_ttl = ""
				 ls_anno = ""			   
				 ls_pub = ""
				 ls_t_date = ""			   
				 ls_forma = ""
				 ls_Identifier = ""
				 ls_isbn= ""			   
				 ls_lang= ""
				 ls_t_rights= ""
				 ls_pubyr= ""
				 ls_sourcepublisher= ""
				 ls_sourcerights= ""
				 ls_aType= ""
				 ls_dtbnarrator= ""
				 ls_dtbproducer= ""
				 ls_dtbproducedate= ""
				 ls_dtbrevision= ""
				 ls_dtbrevisiondate= ""
				 ls_dtbrevisiondescription= ""
				 ls_dtbtotaltime= ""
				 ls_dtbaudioformat= ""          
				 ls_manifest = ""
				 ls_spine = ""
				 ls_package = ""
				 ls_creator = ""
				 ls_text = ""
				 ls_subject = ""
				 ls_chno = ""
				
				 ls_cfile= ""
				 li_filenum = -1
				 
//	MessageBox ('TEST',' Commas = '+string(cnt_comma)+' number of books = '+string(cnt)+' row number is = '+string(i))
	
			 	 li_bkseq = dw_dtbbkseq.object.mchar_bkseq[i]
//				MessageBox("data",string(l_bkseq[i])+" i = "+string(i)+" ll_rows = "+string(ll_rows))
				ls_cfile="P:\opffiles\"+string(li_bkseq)+".opf"
				ls_remotefile = string(li_bkseq)+".opf"
//				MessageBox("data",ls_cfile)
				li_filenum = Fileopen(ls_cfile,streammode!,write!,shared!,Replace!)
				if li_filenum = -1 then
					messagebox("File Error","This file could not be opened. Make sure the directory p:\opffiles exist.")
					Close(w_pics_retrieve_msg_box)
					RollBack using SqlServerTrans;
				
					RETURN -1
				end if
	
	
			 	 ls_chno = dw_dtbbkseq.object.chno[i]
				 ls_xmlver = dw_dtbbkseq.object.t_xmlver[i]+"~n"
				 ls_doctype = dw_dtbbkseq.object.t_doctype[i]+"~n"
				 ls_xmlnls = dw_dtbbkseq.object.t_xmlnls[i]+"~n"
				 ls_t_metadata = dw_dtbbkseq.object.t_metadata[i]+"~n"
				 ls_dc_url = dw_dtbbkseq.object.dc_url[i]+"~n"
//				 ls_ttl = f_change_pipe_italics_tags(f_replace_nonprn_html_tags(dw_dtbbkseq.object.ttl[i]))+"~n"
				 ls_ttl = f_replace_nonprn_html_tags(dw_dtbbkseq.object.ttl[i])+"~n"
			 
				 ll_rows2 = lds.settransobject(sqlservertrans)
				 ll_rows2 = lds.retrieve(ls_chno)
				 
				 ls_creator = ' '
				 FOR j = 1 to ll_rows2
					ls_text = lds.object.t_creators[j]+"~n"
					ls_creator = ls_creator + ls_text 			
				 NEXT
				 ls_creator = Trim(ls_creator)
			 
				 ls_subject = dw_dtbbkseq.object.t_subject[i]+"~n"
				// ls_anno = f_change_pipe_italics_tags(f_replace_nonprn_html_tags(dw_dtbbkseq.object.anno[i]))+"~n"   
				 ls_anno = f_replace_nonprn_html_tags(dw_dtbbkseq.object.anno[i])+"~n"   
				 ls_pub = dw_dtbbkseq.object.pub[i]+"~n"
				 ls_t_date = dw_dtbbkseq.object.t_date[i]+"~n"
				 ls_forma = dw_dtbbkseq.object.forma[i]+"~n"
				 ls_Identifier = dw_dtbbkseq.object.Identifier[i]+"~n"
				 ls_isbn= dw_dtbbkseq.object.isbn[i]+"~n"
				 ls_lang= dw_dtbbkseq.object.lang[i]+"~n"
				 ls_t_rights= dw_dtbbkseq.object.t_rights[i]+"~n"
				 ls_t_dcmetadata = dw_dtbbkseq.object.t_dcmetadata[i]+"~n"
				 ls_t_x_metadata = dw_dtbbkseq.object.t_x_metadata[i]+"~n"
				 ls_pubyr= dw_dtbbkseq.object.pubyr[i]+"~n"
				 ls_sourcepublisher= dw_dtbbkseq.object.sourcepublisher[i]+"~n"
				 ls_sourcerights= dw_dtbbkseq.object.sourcerights[i]+"~n"
				 ls_aType= dw_dtbbkseq.object.aType[i]+"~n"
				 ls_dtbnarrator= dw_dtbbkseq.object.dtbnarrator[i]+"~n"
				 ls_dtbproducer= dw_dtbbkseq.object.dtbproducer[i]+"~n"
				 ls_dtbproducedate= dw_dtbbkseq.object.dtbproducedate[i]+"~n"
				 ls_dtbrevision= dw_dtbbkseq.object.dtbrevision[i]+"~n"
				 ls_dtbrevisiondate= dw_dtbbkseq.object.dtbrevisiondate[i]+"~n"
				 ls_dtbrevisiondescription= dw_dtbbkseq.object.dtbrevisiondescription[i]+"~n"
				 ls_dtbtotaltime= dw_dtbbkseq.object.dtbtotaltime[i]+"~n"
				 ls_dtbaudioformat= dw_dtbbkseq.object.dtbaudioformat[i]+"~n"
				 ls_t_xx_metadata = dw_dtbbkseq.object.t_xx_metadata[i]+"~n"
				 ls_t_close_metadata = dw_dtbbkseq.object.t_close_metadata[i]+"~n"
				 ls_manifest = dw_dtbbkseq.object.t_manifest[i]+"~n"
				 ls_spine = dw_dtbbkseq.object.t_spine[i]+"~n"
				 ls_package = dw_dtbbkseq.object.t_package[i]+"~n"
				 
				filewrite(li_filenum, ls_xmlver)
				filewrite(li_filenum, ls_doctype)
				filewrite(li_filenum, ls_xmlnls)
				filewrite(li_filenum, ls_t_metadata)
				filewrite(li_filenum, ls_dc_url)
				filewrite(li_filenum, ls_ttl)
				filewrite(li_filenum, ls_creator)
				filewrite(li_filenum, ls_subject)
				filewrite(li_filenum, ls_anno)
				filewrite(li_filenum, ls_pub)
				filewrite(li_filenum, ls_t_date)
				filewrite(li_filenum, ls_forma)
				filewrite(li_filenum, ls_Identifier)
				filewrite(li_filenum, ls_isbn)
				filewrite(li_filenum, ls_lang)
				filewrite(li_filenum, ls_t_rights)
				filewrite(li_filenum, ls_t_dcmetadata)
				filewrite(li_filenum, ls_t_x_metadata)
				filewrite(li_filenum, ls_pubyr)
				filewrite(li_filenum, ls_sourcepublisher)
				filewrite(li_filenum, ls_sourcerights)
				filewrite(li_filenum, ls_aType)
				filewrite(li_filenum, ls_dtbnarrator)
				filewrite(li_filenum, ls_dtbproducer)
				filewrite(li_filenum, ls_dtbproducedate)
				filewrite(li_filenum, ls_dtbrevision)
				filewrite(li_filenum, ls_dtbrevisiondate)
				filewrite(li_filenum, ls_dtbrevisiondescription)
				filewrite(li_filenum, ls_dtbtotaltime)
				filewrite(li_filenum, ls_dtbaudioformat)
				filewrite(li_filenum, ls_t_xx_metadata)
				filewrite(li_filenum, ls_t_close_metadata)
				filewrite(li_filenum, ls_manifest)
				filewrite(li_filenum, ls_spine)
				filewrite(li_filenum, ls_package)
	
				fileclose(li_filenum)
				
				IF li_ftp_rc <> -1 THEN
					w_pa_create_ftp_opf_files.ole_ezftp.uf_upload ( ls_cfile, ls_remotefile , FALSE )
				END IF


	
		NEXT
		Close(w_pics_retrieve_msg_box)
		
		MessageBox("opf files","OPF files has been created and placed in p:\opffiles network directory.")
		IF li_ftp_rc <> -1 THEN
			w_pa_create_ftp_opf_files.ole_ezftp.uf_logout()
		END IF
	
	end if
	
END IF

end event

type cb_close from commandbutton within w_pa_create_ftp_opf_files
integer x = 1179
integer y = 1224
integer width = 402
integer height = 112
integer taborder = 40
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&Close"
end type

event clicked;ib_disableclosequery = TRUE
parent.Event pfc_close()

end event


Start of PowerBuilder Binary Data Section : Do NOT Edit
00w_pa_create_ftp_opf_files.bin 
2F00000c00e011cfd0e11ab1a1000000000000000000000000000000000003003e0009fffe000000060000000000000000000000010000000100000000000010000000000200000001fffffffe0000000000000000fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffdfffffffefffffffe00000004fffffffeffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff006f00520074006f004500200074006e00790072000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000050016ffffffffffffffff0000000100000000000000000000000000000000000000000000000000000000f5ea514001c42ebc00000003000002400000000000500003004f0042005800430054005300450052004d0041000000000000000000000000000000000000000000000000000000000000000000000000000000000102001affffffff00000002ffffffff00000000000000000000000000000000000000000000000000000000000000000000000000000000000001c700000000004200500043004f00530058004f00540041005200450047000000000000000000000000000000000000000000000000000000000000000000000000000000000001001affffffffffffffff000000036580f76711cf781945446cb80000545300000000f5e5bd6001c42ebcf5ea514001c42ebc000000000000000000000000006f00430074006e006e00650073007400000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001020012ffffffffffffffffffffffff000000000000000000000000000000000000000000000000000000000000000000000000000000080000002f0000000000000001000000020000000300000004000000050000000600000007fffffffefffffffeffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
28ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff0000fffe000201056580f76711cf781945446cb80000545300000001fb8f0821101b01640008ed8413c72e2b00000030000001970000000b00000100000000600000010100000068000001020000007000000103000000780000010400000080000001050000008c000001060000009800000107000000b000000108000000c000000109000000d000000000000000d80000000300010000000000030000032000000003000003200000000300000000000000080000000100000000000000080000000100000000000000080000000d31327372636f6c2e766f672e000000000000000800000005736369700000000000000008000000086f6c6962003569780000000b000000000000000b000000000000000100010900000007006e69620000797261000001070000000972657375656d616e0001040000000a00636f6c0069666c610600656c0d00000172000000746f6d656464616500736572000001050000000b6f6d6572696665740300656c0c0000015f000000636f74736f72706b01007370090000015f000000657478650078746e00000102000000097478655f79746e65000108000000090073617000726f77730100006400090000765f00006973726500006e6f000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000010000000003200000032000000000720c00002e3132732e636f6c04766f67736369706c6962073569786f00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
10w_pa_create_ftp_opf_files.bin 
End of PowerBuilder Binary Data Section : No Source Expected After This Point

$PBExportHeader$w_pms_data_extract.srw
forward
global type w_pms_data_extract from w_response
end type
type rb_crc from radiobutton within w_pms_data_extract
end type
type rb_cbr from radiobutton within w_pms_data_extract
end type
type rb_arc from radiobutton within w_pms_data_extract
end type
type rb_abr from radiobutton within w_pms_data_extract
end type
type cbx_bypass from checkbox within w_pms_data_extract
end type
type hpb_1 from hprogressbar within w_pms_data_extract
end type
type rb_abridged from u_rb within w_pms_data_extract
end type
type st_1 from u_st within w_pms_data_extract
end type
type st_2 from u_st within w_pms_data_extract
end type
type st_3 from u_st within w_pms_data_extract
end type
type em_start_date from u_em within w_pms_data_extract
end type
type em_end_date from u_em within w_pms_data_extract
end type
type gb_st_end from u_gb within w_pms_data_extract
end type
type rb_recorded from u_rb within w_pms_data_extract
end type
type rb_braille from u_rb within w_pms_data_extract
end type
type cb_extract from u_cb within w_pms_data_extract
end type
type cb_clear from u_cb within w_pms_data_extract
end type
type cb_exit from u_cb within w_pms_data_extract
end type
type gb_wpcat from groupbox within w_pms_data_extract
end type
end forward

global type w_pms_data_extract from w_response
integer x = 407
integer y = 508
integer width = 2107
integer height = 1644
string title = "PMS Data Extract"
event ue_export ( date ad_start,  date ad_end,  string as_bkmed )
event ue_clear ( )
event ue_enterkey pbm_dwnprocessenter
rb_crc rb_crc
rb_cbr rb_cbr
rb_arc rb_arc
rb_abr rb_abr
cbx_bypass cbx_bypass
hpb_1 hpb_1
rb_abridged rb_abridged
st_1 st_1
st_2 st_2
st_3 st_3
em_start_date em_start_date
em_end_date em_end_date
gb_st_end gb_st_end
rb_recorded rb_recorded
rb_braille rb_braille
cb_extract cb_extract
cb_clear cb_clear
cb_exit cb_exit
gb_wpcat gb_wpcat
end type
global w_pms_data_extract w_pms_data_extract

type variables

end variables

forward prototypes
public function any wf_coauth_narr (datastore lds)
public function integer wf_create_talkingbooktopic_catalog (date ad_start, date ad_end, integer li_filenum_xml)
public function integer wf_create_braillebookreview_catalog (date ad_start, date ad_end, integer li_filenum_xml)
public function integer wf_create_cassette_catalog (date ad_start, date ad_end, integer li_filenum_xml)
public function integer wf_create_braille_catalog (date ad_start, date ad_end, integer li_filenum_xml)
public function integer wf_create_abridged_document (date ad_start, date ad_end)
public function any wf_coauth_narr_wp (datastore lds)
public function integer wf_create_romance_catalog (date ad_start, date ad_end, integer li_filenum_xml, string ls_pmsub, string ls_casub, string ls_med)
public function integer wf_create_younger_reader_catalog (date ad_rc_start, date ad_rc_end, date ad_br_start, date ad_br_end, integer li_filenum_xml)
public function integer of_dbrc (ref n_ds ads)
public function string of_parsecoauthors (string as_text, integer ai_count)
public function string of_setcoauthors (string as_first[], string as_last[], string as_honor[], integer ai_match)
public function boolean of_checkcoauthors (string as_first[], string as_last[], string as_f, string as_l)
public function string of_labelcategory (string as_sub_code, string as_section, string as_desc)
public function integer of_setbookmedia (ref n_ds ads)
end prototypes

event ue_export(date ad_start, date ad_end, string as_bkmed);//////////////////////////////////////////////////////////////////////////////////////////////////////////
//
//	Function:  wf_create_cassette_catalog
//
//	Description:
//	Set menu items
//
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//	
//	Revision History
//
//	Developed by 	Date 				Version						Tracking#
//									
// Murali K.			01/31/2008      002 PICS Modifications	 PICS PMS 2.0 Modifications
//																			PMS B.2, PMS B.2.1
//																Introduce flash books media
//																Pass RC and DB as arguments
//																string array as argument
// Murali K. 			03/11/2009 for recorded books retrieve both RC and DB records #2159
// Murali K			03/24/2010 unsupported appeon feature GOTO removed and code moved to its place
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//
n_ds lds
Integer li_filenum, li_wpfile, li_filenum_xml, rtn
String ls_current_chno, ls_narrstr, ls_coauthstr, ls_co, ls_narr, ls_return, ls_return_xml, ls_catalog, ls_criteria,ls_filter,ls_media,books_lang,ls_hangindent
Long ll_rows, ll_count, ll_chno_match, ll_rec
String ls_filename,ls_filename_xml, ls_path, ls_path_xml, ls_data, ls_wpheader, booksvars,booksvars2, lsmedia,ls_lang1, ls_lang2,ls_endfield,ls_msg,ls_endrecord

str_pms_data_extract lstr_pms_data_extract[]
str_pms_data_extract2 lstr_pms_data_extract2[]
str_catalog_info lstr_catalog_info
str_xml_export lstr_xml_export

Long ll_nomatch=0, ll_match=0, ll_len
Blob lb_blob, lb_blob2
Boolean catalog_created = FALSE

ls_endfield =  '~h5c~h70~h61~h72~h0d~h0a~h7d~h7b~h5c~h70~h6c~h61~h69~h6e~h20'
ls_endrecord = '~h5c~h70~h61~h72~h0d~h0a~h7d~h7b~h5c~h70~h6c~h61~h69~h6e~h20~h5c~h70~h61~h72~h64~h5c~h70~h61~h67~h65~h20'
ls_hangindent = '~hcc~hf1~h02~h02~h00~hf1~he0~h30~h0c~h00~h00~h00~h00~h08~h07~h0c~h00~he0~he0~h00~h0c~h00~h00~h00~h00~hb0~h04~h0c~h00~he0~hf1~h03~h02~h00~hf1'



//Check to see if they want to bypass wordperfect file creation
IF cbx_bypass.checked = TRUE THEN
// Open the catalog response window

Open(w_xml_catalogs)

// Check the structure returned in Message object
		
lstr_xml_export = message.powerObjectParm

ls_catalog = lstr_xml_export.ls_catalog

gs_catalog = ls_catalog // 01/26/2009 need to notify global function

IF ls_catalog = "youngerreaders" THEN
	ad_start = lstr_xml_export.ld_rc_stdate
	ad_end = lstr_xml_export.ld_rc_enddt
END IF


IF ls_catalog <> 'nocatalog' THEN
	
	// If XM file is for network section services(nss) prompt for additional cirteria
	IF ls_catalog = "nss" THEN
		
		SetNull(ls_criteria)
		
		Open(w_pms_reports_criteria)
		
		// Check the structure returned in Message object
		
		lstr_catalog_info = message.powerObjectParm
		
		IF IsNull(lstr_catalog_info) THEN
			SetPointer(arrow!)
			Messagebox("Extract Complete",String(ll_rec)+ " records written")
			// Destroy the datastore 
			 DESTROY lds
		ELSE
//			MessageBox("casub",lstr_catalog_info.casub)
//			MessageBox("pmsub",lstr_catalog_info.pmsub)
//			MessageBox("media",lstr_catalog_info.media)
                // assign the filename prefixes
			IF lstr_catalog_info.media='ALL' THEN
				ls_catalog = 'nss_all_media_'
			ELSEIF  lstr_catalog_info.media='RC' THEN
				ls_catalog = 'nss_rc_'
			ELSEIF  lstr_catalog_info.media='BR' THEN
				ls_catalog = 'nss_br_'
			END IF
		END IF
	
	END IF

	// Get the XML filename
	
	ls_filename_xml = ls_catalog + String(ad_end,"mmddyyyy")
	ls_path_xml = "c:\program files\picsorcl9i\extract\" + ls_filename_xml
	IF GetFileSaveName("Select Extract File",ls_path_xml,ls_filename_xml, "xml", "XML Files (*.XML),*.XML") <> 1 THEN
		Messagebox("Incorrect file name","Please try again")
		RETURN
	END IF
	
	// XML file
	ll_rec = 0
	li_filenum_xml = FileOpen(ls_path_xml,streamMode!,write!,lockWrite!,replace!)
	IF li_filenum_xml = -1 THEN
		Messagebox("File Error","the file "+ ls_filename_xml+" could not be opened for output.")
		SetPointer(arrow!)
		RETURN
	END IF
	
	OpenWithParm(w_pics_retrieve_msg_box,"Creating the XML file, Please Wait...")
	
// Talking Book Topic Catalog
	IF ls_catalog = "talkingbooktopic" THEN
		ll_rec = wf_create_talkingbooktopic_catalog(ad_start,ad_end,li_filenum_xml)	
		
// Braille Book Review Catalog
	ELSEIF ls_catalog = "braillebookreview" THEN
		ll_rec = wf_create_braillebookreview_catalog(ad_start,ad_end,li_filenum_xml)

// Cassettes Books Catalog		
//	ELSEIF ls_catalog = "cassettebooks" THEN
	ELSEIF ls_catalog = "dtbplus" THEN
		ll_rec  = wf_create_cassette_catalog(ad_start,ad_end,li_filenum_xml)
		
// Braille Books Catalog
	ELSEIF ls_catalog = "braillebooks" THEN
		ll_rec  = wf_create_braille_catalog(ad_start,ad_end,li_filenum_xml)
		
// For Younger Readers Catalog
	ELSEIF ls_catalog = "youngerreaders" THEN
		date ad_br_start, ad_br_end,ad_rc_start, ad_rc_end
		ad_rc_start = lstr_xml_export.ld_rc_stdate
		ad_rc_end = lstr_xml_export.ld_rc_enddt
		ad_br_start = lstr_xml_export.ld_br_stdate
		ad_br_end = lstr_xml_export.ld_br_enddt

//		ll_rec  = wf_create_younger_reader_catalog(ad_start,ad_end,li_filenum_xml)
//MessageBox("dates",string(ad_rc_start) + "   " + string(ad_rc_end) + "  " + string(ad_br_start) + "  " + string(ad_br_end))
		ll_rec  = wf_create_younger_reader_catalog(ad_rc_start, ad_rc_end,ad_br_start ,ad_br_end,li_filenum_xml)

// For Network Section Services  Catalogs
	ELSEIF ls_catalog = "nss_all_media_"  OR ls_catalog = "nss_rc_"  OR ls_catalog = "nss_br_" THEN
		
		ll_rec  = wf_create_romance_catalog(ad_start,ad_end,li_filenum_xml,lstr_catalog_info.pmsub,lstr_catalog_info.casub,lstr_catalog_info.media)

	END IF

	Close(w_pics_retrieve_msg_box)

END IF // No catalog selected


END IF

IF cbx_bypass.checked = FALSE THEN // 3/30/2010 removed goto statement appeon unsupported replaced code
	
	IF as_bkmed = 'RC_Abridged' THEN
		gs_catalog = 'WP'
		ll_rec = wf_create_abridged_document(ad_start,ad_end)	
		Messagebox("Extract for abridged document completed",String(ll_rec)+ " records written")
		RETURN
	END IF
	
	// Get the filename
	string ls_arg[] // 03/11/2009 for recorded books retrieve both RC and DB records
	
	IF rb_abr.checked  THEN
		ls_filename = String(ad_end,"mmddyyyy") + 'abr'
		ls_arg[1] = 'BR'
	ELSEIF rb_arc.checked  THEN
		ls_filename = String(ad_end,"mmddyyyy") + 'arc'
		ls_arg[1] = 'RC'
		ls_arg[2] = 'DB'
	ELSEIF rb_crc.checked  THEN
		ls_filename = String(ad_end,"mmddyyyy") + 'crc'
		ls_arg[1] = 'RC'
		ls_arg[2] = 'DB'
	ELSEIF rb_cbr.checked  THEN
		ls_filename = String(ad_end,"mmddyyyy") + 'cbr'
		ls_arg[1] = 'BR'
	ELSE
		ls_filename = String(ad_end,"mmddyyyy") + as_bkmed
	END IF
		
	//Set the path for the file
	ls_path = "c:\program files\picsorcl9i\extract\" + ls_filename
	
	IF GetFileSaveName("Select Extract File",ls_path,ls_filename, "wpd", "WPD Files (*.WPD),*.WPD") <> 1 THEN
		Messagebox("Incorrect file name","Please try again")
		RETURN
	END IF
	
	IF rb_abr.checked  OR  rb_arc.checked OR  rb_crc.checked OR  rb_cbr.checked THEN
		// IF adult braille book or adult recorded book or children recorded book or children braille book
		
		// Create and load the datastore
		lds = CREATE n_ds
		lds.dataObject = "d_adults_child_data_extract"
		
		ll_rows = lds.SetTransObject(SqlServerTrans)
		Open(w_pics_retrieve_msg_box)
	//	ll_rows = lds.Retrieve(ad_start,ad_end,as_bkmed)
		ll_rows = lds.Retrieve(ad_start,ad_end,ls_arg[])
		Close(w_pics_retrieve_msg_box)
	
		SetPointer(hourglass!)
		
		IF ll_rows = -1 THEN
			Messagebox("Database Error","There was an error during the retrieve.  Please try again.")
			SetPointer(arrow!)
			RETURN
		END IF
		
		IF ll_rows = 0 THEN
			Messagebox("No records found","Please adjust your date range and try again.")
			SetPointer(arrow!)
			RETURN
		ELSE
			// Copy the rows to a structure so we can work with it
	//		of_setbookmedia(lds) // 04/27/09
			//#2249  12/4/2009 for adult recorded and children recorded books db rc processing
			IF  rb_arc.checked OR  rb_crc.checked THEN
				of_dbrc(lds) // lds reference 03/12/2009
			END IF
			lstr_pms_data_extract2[] = lds.object.Data		
		END IF
	
		ll_rec = 0
		li_filenum = FileOpen(ls_path,streamMode!,write!,lockWrite!,replace!)
		IF li_filenum = -1 THEN
			Messagebox("File Error","the file "+ls_filename+" could not be opened for output.")
			SetPointer(arrow!)
			RETURN
		END IF
		
		// insert WP header
		
		li_wpfile = FileOpen("c:\program files\picsorcl9i\wptemp.rtf",streamMode!)
		IF FileRead(li_wpfile, lb_blob) < 1 THEN
			Messagebox("Template file error","Cannot open template file")
			SetPointer(arrow!)
			RETURN
		END IF
		ll_len = Len(lb_blob)
		lb_blob2 = BlobMid(lb_blob,1,721)
		FileClose(li_wpfile)
	
		FileWrite(li_filenum,lb_blob2)
		FileWrite(li_filenum," ")
		
		
		// If you check on Adult Braille Books ...
			 IF rb_abr.checked THEN
			ls_media = "BR"
			// Adult Braille NonFiction
			booksvars = "AN"
			booksvars2 = "YN"
			books_lang = "ENG"
			ls_filter = "(ttlinit_ajyfn = '"+ booksvars +"' or  ttlinit_ajyfn = '"+ booksvars2 +"') and mchar_bkmed = '"+ ls_media + "'"+ " and ttlinit_lang = '"+ books_lang +"'"
			lds.SetFilter(ls_filter)
			rtn = lds.Filter()
			IF rtn = 1 THEN
				lds.SetSort("mchar_bkseq asc")
				ll_rows = lds.RowCount()
				IF ll_rows > 0 THEN
					// Rows exist
					// Copy the filtered rows to the structure so we can work with it
					lstr_pms_data_extract2[] = lds.object.Data		
					ls_msg = 'Braille -- Adult Nonfiction' + ls_endfield
					FileWrite(li_filenum,ls_msg)
					FOR ll_count=1 TO ll_rows
						ls_return = lstr_pms_data_extract2[ll_count].bkmed + " "+ String(lstr_pms_data_extract2[ll_count].bkseq) + " " +  lstr_pms_data_extract2[ll_count].ttl+ ls_endfield
						FileWrite(li_filenum,ls_return)
						ll_rec++
					NEXT
				END IF
			END IF
			// Clear the filter for next set of criteria
			lds.SetFilter("")
			lds.Filter()
			ls_msg = '==========================================================' + ls_endfield
			FileWrite(li_filenum,ls_msg)		
			// Adult Braille Fiction
			booksvars = "AF"
			booksvars2 = "YF"
			books_lang = "ENG"
			ls_filter = "(ttlinit_ajyfn = '"+ booksvars +"' or  ttlinit_ajyfn = '"+ booksvars2 +"') and mchar_bkmed = '"+ ls_media + "'"+ " and ttlinit_lang = '"+ books_lang +"'"
			lds.SetFilter(ls_filter)
			rtn = lds.Filter()
			IF rtn = 1 THEN
				lds.SetSort("mchar_bkseq asc")
				ll_rows = lds.RowCount()
				IF ll_rows > 0 THEN
					// Rows exist
					// Copy the filtered rows to the structure so we can work with it
					lstr_pms_data_extract2[] = lds.object.Data		
					ls_msg = 'Braille -- Adult fiction' + ls_endfield
					FileWrite(li_filenum,ls_msg)
					FOR ll_count=1 TO ll_rows
						ls_return = lstr_pms_data_extract2[ll_count].bkmed + " "+ String(lstr_pms_data_extract2[ll_count].bkseq) + " " +  lstr_pms_data_extract2[ll_count].ttl + ls_endfield
						FileWrite(li_filenum,ls_return)
						ll_rec++
					NEXT
				END IF
			END IF
			// Clear the filter for next set of criteria
			lds.SetFilter("")
			lds.Filter()
			ls_msg = '==========================================================' + ls_endfield
			FileWrite(li_filenum,ls_msg)
			// Adult, Young Adults and children braille NonFiction and Fiction and NOT english (foreign)
			books_lang = "ENG"
			ls_filter = "ttlinit_lang <> '"+ books_lang +"' and mchar_bkmed = '"+ ls_media + "'"
			lds.SetFilter(ls_filter)
			rtn = lds.Filter()
			IF rtn = 1 THEN
				lds.SetSort("mchar_bkseq asc")
				ll_rows = lds.RowCount()
				IF ll_rows > 0 THEN
					// Rows exist
					// Copy the filtered rows to the structure so we can work with it
					lstr_pms_data_extract2[] = lds.object.Data		
					ls_msg = 'Braille -- Foreign books (Adults and Children) (Fiction or Nonfiction)' + ls_endfield
					FileWrite(li_filenum,ls_msg)
					FOR ll_count=1 TO ll_rows
						ls_return = lstr_pms_data_extract2[ll_count].bkmed + " "+ String(lstr_pms_data_extract2[ll_count].bkseq) + " " +  lstr_pms_data_extract2[ll_count].ttl+ ls_endfield
						FileWrite(li_filenum,ls_return)
						ll_rec++
					NEXT
				END IF
			END IF
			// end the record
			FileWrite(li_filenum,ls_endrecord)	
		// If you check on Childrens Braille Books ...
		ELSEIF rb_cbr.checked THEN
			ls_media = "BR"
			// Children Braille NonFiction
			booksvars = "JN"
			books_lang = "ENG"
			ls_filter = "ttlinit_ajyfn = '"+ booksvars +"' and mchar_bkmed = '"+ ls_media + "'" + " and ttlinit_lang = '"+ books_lang +"'"
			lds.SetFilter(ls_filter)
			rtn = lds.Filter()
			IF rtn = 1 THEN
				lds.SetSort("mchar_bkseq asc")
				ll_rows = lds.RowCount()
				IF ll_rows > 0 THEN
					// Rows exist
					// Copy the filtered rows to the structure so we can work with it
					lstr_pms_data_extract2[] = lds.object.Data		
					ls_msg = 'Braille -- Children Nonfiction' + ls_endfield
					FileWrite(li_filenum,ls_msg)
					FOR ll_count=1 TO ll_rows
						ls_return = lstr_pms_data_extract2[ll_count].bkmed + " "+ String(lstr_pms_data_extract2[ll_count].bkseq) + " " +  lstr_pms_data_extract2[ll_count].ttl+ ls_endfield
						FileWrite(li_filenum,ls_return)
						ll_rec++
					NEXT
				END IF
			END IF
			// Clear the filter for next set of criteria
			lds.SetFilter("")
			lds.Filter()
			ls_msg = '==========================================================' + ls_endfield
			FileWrite(li_filenum,ls_msg)
			// Children Braille Fiction
			booksvars = "JF"
			books_lang = "ENG"
			ls_filter = "ttlinit_ajyfn = '"+ booksvars +"' and mchar_bkmed = '"+ ls_media + "'" + " and ttlinit_lang = '"+ books_lang +"'"
			lds.SetFilter(ls_filter)
			rtn = lds.Filter()
			IF rtn = 1 THEN
				lds.SetSort("mchar_bkseq asc")
				ll_rows = lds.RowCount()
				IF ll_rows > 0 THEN
					// Rows exist
					// Copy the filtered rows to the structure so we can work with it
					lstr_pms_data_extract2[] = lds.object.Data		
					ls_msg = 'Braille -- Children Fiction' + ls_endfield
					FileWrite(li_filenum,ls_msg)
					FOR ll_count=1 TO ll_rows
						ls_return = lstr_pms_data_extract2[ll_count].bkmed + " "+ String(lstr_pms_data_extract2[ll_count].bkseq) + " " +  lstr_pms_data_extract2[ll_count].ttl + ls_endfield
						FileWrite(li_filenum,ls_return)
						ll_rec++
					NEXT
				END IF
			END IF
			// end the record
			FileWrite(li_filenum,ls_endrecord)
			
		// If you check on Children Recorded Books ...
		ELSEIF rb_crc.checked THEN
		//	ls_media = "RC"
			// 03/11/2009 process db records also
			ls_media = "('RC','DB')"
			// Children Recorded Books NonFiction
			booksvars = "JN"
			books_lang = "ENG"
	//		ls_filter = "ttlinit_ajyfn = '"+ booksvars +"' and mchar_bkmed = '"+ ls_media + "'" + " and ttlinit_lang = '"+ books_lang +"'"
			ls_filter = "ttlinit_ajyfn = '"+ booksvars +"' and mchar_bkmed in  "+ ls_media  + " and ttlinit_lang = '"+ books_lang +"'"
			lds.SetFilter(ls_filter)
			rtn = lds.Filter()
			IF rtn = 1 THEN
				lds.SetSort("mchar_bkseq asc")
				ll_rows = lds.RowCount()
				IF ll_rows > 0 THEN
					// Rows exist
					// Copy the filtered rows to the structure so we can work with it
					lstr_pms_data_extract2[] = lds.object.Data		
					ls_msg = 'Recorded -- Children Nonfiction' + ls_endfield
					FileWrite(li_filenum,ls_msg)
					FOR ll_count=1 TO ll_rows
					//	ls_return = lstr_pms_data_extract2[ll_count].bkmed + " "+ String(lstr_pms_data_extract2[ll_count].bkseq) + " " +  lstr_pms_data_extract2[ll_count].ttl+ ls_endfield
						ls_return = lstr_pms_data_extract2[ll_count].dbrc + " "+ String(lstr_pms_data_extract2[ll_count].bkseq) + " " +  lstr_pms_data_extract2[ll_count].ttl+ ls_endfield
						FileWrite(li_filenum,ls_return)
						ll_rec++
					NEXT
				END IF
			END IF
			// Clear the filter for next set of criteria
			lds.SetFilter("")
			lds.Filter()
			ls_msg = '==========================================================' + ls_endfield
			FileWrite(li_filenum,ls_msg)
			// Children Recorded Books Fiction
			booksvars = "JF"
			books_lang = "ENG"
	//		ls_filter = "ttlinit_ajyfn = '"+ booksvars +"' and mchar_bkmed = '"+ ls_media + "'" + " and ttlinit_lang = '"+ books_lang +"'"
			ls_filter = "ttlinit_ajyfn = '"+ booksvars +"' and mchar_bkmed in  "+ ls_media +  " and ttlinit_lang = '"+ books_lang +"'"
			lds.SetFilter(ls_filter)
			rtn = lds.Filter()
			IF rtn = 1 THEN
				lds.SetSort("mchar_bkseq asc")
				ll_rows = lds.RowCount()
				IF ll_rows > 0 THEN
					// Rows exist
					// Copy the filtered rows to the structure so we can work with it
					lstr_pms_data_extract2[] = lds.object.Data		
					ls_msg = 'Recorded -- Children Fiction' + ls_endfield
					FileWrite(li_filenum,ls_msg)
					FOR ll_count=1 TO ll_rows
	//					ls_return = lstr_pms_data_extract2[ll_count].bkmed + " "+ String(lstr_pms_data_extract2[ll_count].bkseq) + " " +  lstr_pms_data_extract2[ll_count].ttl + ls_endfield
						ls_return = lstr_pms_data_extract2[ll_count].dbrc + " "+ String(lstr_pms_data_extract2[ll_count].bkseq) + " " +  lstr_pms_data_extract2[ll_count].ttl + ls_endfield
						FileWrite(li_filenum,ls_return)
						ll_rec++
					NEXT
				END IF
			END IF
			// end the record
			FileWrite(li_filenum,ls_endrecord)
			
		// If you check on Adult Recorded Books ...
		ELSEIF rb_arc.checked THEN
	//		ls_media = "RC"
			ls_media = "('RC','DB')"
			//  Adults and Young Adults Recorded NonFiction
			booksvars = "AN"
			booksvars2 = "YN"
			books_lang = "ENG"
	//		ls_filter = "(ttlinit_ajyfn = '"+ booksvars +"' or  ttlinit_ajyfn = '"+ booksvars2 +"') and mchar_bkmed = '"+ ls_media + "'" + " and ttlinit_lang = '"+ books_lang +"'"
			ls_filter = "(ttlinit_ajyfn = '"+ booksvars +"' or  ttlinit_ajyfn = '"+ booksvars2 +"') and mchar_bkmed in  "+ ls_media +  " and ttlinit_lang = '"+ books_lang +"'"
	//		MessageBox('filter',ls_filter)
			lds.SetFilter(ls_filter)
			rtn = lds.Filter()
			IF rtn = 1 THEN
				lds.SetSort("mchar_bkseq asc")
				ll_rows = lds.RowCount()
				IF ll_rows > 0 THEN
					// Rows exist
					// Copy the filtered rows to the structure so we can work with it
					lstr_pms_data_extract2[] = lds.object.Data		
					ls_msg = 'Recorded -- Adults Nonfiction' + ls_endfield
					FileWrite(li_filenum,ls_msg)
					FOR ll_count=1 TO ll_rows
	//					ls_return = lstr_pms_data_extract2[ll_count].bkmed + " "+ String(lstr_pms_data_extract2[ll_count].bkseq) + " " +  lstr_pms_data_extract2[ll_count].ttl+ ls_endfield
						ls_return = lstr_pms_data_extract2[ll_count].dbrc + " "+ String(lstr_pms_data_extract2[ll_count].bkseq) + " " +  lstr_pms_data_extract2[ll_count].ttl+ ls_endfield
						FileWrite(li_filenum,ls_return)
						ll_rec++
					NEXT
				END IF
			END IF
			// Clear the filter for next set of criteria
			lds.SetFilter("")
			lds.Filter()
			ls_msg = '==========================================================' + ls_endfield
			FileWrite(li_filenum,ls_msg)
			// Adults and Young Adults recorded Books Fiction
			booksvars = "AF"
			booksvars2 = "YF"
			books_lang = "ENG"
	//		ls_filter = "(ttlinit_ajyfn = '"+ booksvars +"' or  ttlinit_ajyfn = '"+ booksvars2 +"') and mchar_bkmed = '"+ ls_media + "'" + " and ttlinit_lang = '"+ books_lang +"'"
			ls_filter = "(ttlinit_ajyfn = '"+ booksvars +"' or  ttlinit_ajyfn = '"+ booksvars2 +"') and mchar_bkmed in  "+ ls_media + " and ttlinit_lang = '"+ books_lang +"'"
	//		MessageBox('filter',ls_filter)
			lds.SetFilter(ls_filter)
			rtn = lds.Filter()
			IF rtn = 1 THEN
				lds.SetSort("mchar_bkseq asc")
				ll_rows = lds.RowCount()
				IF ll_rows > 0 THEN
					// Rows exist
					// Copy the filtered rows to the structure so we can work with it
					lstr_pms_data_extract2[] = lds.object.Data		
					ls_msg = 'Recorded -- Adults Fiction' + ls_endfield
					FileWrite(li_filenum,ls_msg)
					FOR ll_count=1 TO ll_rows
	//					ls_return = lstr_pms_data_extract2[ll_count].bkmed + " "+ String(lstr_pms_data_extract2[ll_count].bkseq) + " " +  lstr_pms_data_extract2[ll_count].ttl + ls_endfield
						ls_return = lstr_pms_data_extract2[ll_count].dbrc + " "+ String(lstr_pms_data_extract2[ll_count].bkseq) + " " +  lstr_pms_data_extract2[ll_count].ttl + ls_endfield
						FileWrite(li_filenum,ls_return)
						ll_rec++
					NEXT
				END IF
			END IF
			// Clear the filter for next set of criteria
			lds.SetFilter("")
			lds.Filter()
			ls_msg = '==========================================================' + ls_endfield
			FileWrite(li_filenum,ls_msg)
			// Adult, Young Adults and children recorded NonFiction and Fiction and NOT english (foreign)
			books_lang = "ENG"
	//		ls_filter = "ttlinit_lang <> '"+ books_lang +"' and mchar_bkmed = '"+ ls_media + "'"
			ls_filter = "ttlinit_lang <> '"+ books_lang +"' and mchar_bkmed in   "+ ls_media 
			lds.SetFilter(ls_filter)
			rtn = lds.Filter()
			IF rtn = 1 THEN
				lds.SetSort("mchar_bkseq asc")
				ll_rows = lds.RowCount()
				IF ll_rows > 0 THEN
					// Rows exist
					// Copy the filtered rows to the structure so we can work with it
					lstr_pms_data_extract2[] = lds.object.Data		
					ls_msg = 'Recorded -- Foreign books (Adults and Children) (Fiction or Nonfiction)' + ls_endfield
					FileWrite(li_filenum,ls_msg)
					FOR ll_count=1 TO ll_rows
	//					ls_return = lstr_pms_data_extract2[ll_count].bkmed + " "+ String(lstr_pms_data_extract2[ll_count].bkseq) + " " +  lstr_pms_data_extract2[ll_count].ttl+ ls_endfield
						ls_return = lstr_pms_data_extract2[ll_count].dbrc + " "+ String(lstr_pms_data_extract2[ll_count].bkseq) + " " +  lstr_pms_data_extract2[ll_count].ttl+ ls_endfield
						FileWrite(li_filenum,ls_return)
						ll_rec++
					NEXT
				END IF
			END IF
			// end the record
			FileWrite(li_filenum,ls_endrecord)
		END IF
		FileClose(li_filenum)
	
	ELSE
		// Create and load the datastore
		lds = CREATE n_ds
		lds.dataObject = "d_pms_data_extract_wp" // 03/11/2009
		
		ll_rows = lds.SetTransObject(SqlServerTrans)
		Open(w_pics_retrieve_msg_box)
	//	ll_rows = lds.Retrieve(ad_start,ad_end,as_bkmed)
		// 01/31/2008 Include DB also as media to string array as retrieval arguments
	
		ls_arg[1] = as_bkmed
		
		IF rb_recorded.checked = TRUE THEN
			ls_arg[2] = 'DB'
		END IF
		
		ll_rows = lds.Retrieve(ad_start,ad_end,ls_arg[])
		////////
		Close(w_pics_retrieve_msg_box)
		
		SetPointer(hourglass!)
		
		IF ll_rows = -1 THEN
			Messagebox("Database Error","There was an error during the retrieve.  Please try again.")
			SetPointer(arrow!)
			RETURN
		END IF
		
		IF ll_rows = 0 THEN
			Messagebox("No records found","Please adjust your date range and try again.")
			SetPointer(arrow!)
			RETURN
		END IF
		
		// Set db/rc book media records.
		of_dbrc(lds) // 03/11/2009
		ll_rows = lds.rowcount()
		///
		
		// load the coauthors and narrators for WP file
		lstr_pms_data_extract = wf_coauth_narr_wp(lds)
		
		// Pass two, for each live record, run the f_export function
		
		ll_rec = 0
		li_filenum = FileOpen(ls_path,streamMode!,write!,lockWrite!,replace!)
		IF li_filenum = -1 THEN
			Messagebox("File Error","the file "+ls_filename+" could not be opened for output.")
			SetPointer(arrow!)
			RETURN
		END IF
		
		
		// insert WP header
		
		li_wpfile = FileOpen("c:\program files\picsorcl9i\wptemp.rtf",streamMode!)
		IF FileRead(li_wpfile, lb_blob) < 1 THEN
			Messagebox("Template file error","Cannot open template file")
			SetPointer(arrow!)
			RETURN
		END IF
		ll_len = Len(lb_blob)
		lb_blob2 = BlobMid(lb_blob,1,721)
		FileClose(li_wpfile)
		
		FileWrite(li_filenum,lb_blob2)
		FileWrite(li_filenum," ")
		
		FOR ll_count=1 TO ll_rows
			IF lstr_pms_data_extract[ll_count].live = TRUE THEN
				ls_return = f_export(lstr_pms_data_extract[ll_count])
		//		lb_blob = f_blob_string(ls_return)
		//		Filewrite(li_filenum,lb_blob)
				FileWrite(li_filenum,ls_return)
				ll_rec++
			END IF
		NEXT
		
		FileClose(li_filenum)
		
	END IF
END IF // IF WORD PERFECT EXTRACT IS SELECTED 3/29/2010	

SetPointer(arrow!)
Messagebox("Extract Complete",String(ll_rec)+ " records written")

DESTROY lds
end event

event ue_clear;call super::ue_clear;em_end_date.text = ""
em_start_date.text = ""
rb_braille.checked = false
rb_recorded.checked = false
end event

event ue_enterkey;call super::ue_enterkey;Send(Handle(this),256,9,Long(0,0))
return 1

end event

public function any wf_coauth_narr (datastore lds);long ll_rows
int ll_count, ll_chno_match, ll_nomatch, li_match
string ls_current_chno, ls_co, ls_narr, ls_coauthor, ls_coauthstr, ls_narrstr, ls_temp, ls_first[], ls_last[], ls_honor[]
str_pms_data_extract lstr_pms_data_extract[]
Any    la_returnValue
boolean lb_more=FALSE

ll_rows = lds.rowcount()

IF ll_rows > 0 THEN
	// Copy the rows to a structure so we can work with it

	lstr_pms_data_extract[] = lds.object.data
	
	// Pass one, combine the duplicates
	// Go through the array, and find rows with duplicate chno
	// When we find them, go to the original matching row, and set
	// narrstr to "Various Narrators" or coauthstr to "and others"
	// depending on what matched, and then mark the second as dead.
	
		
	ls_current_chno = "none"
	ll_chno_match = 1
	FOR ll_count=1 TO ll_rows
				
		lstr_pms_data_extract[ll_count].bkmed = trim(lstr_pms_data_extract[ll_count].bkmed)
			
			if lstr_pms_data_extract[ll_count].bkseq = 64788  then
				ls_temp = ''
			end if
			
		if (lstr_pms_data_extract[ll_count].chno = ls_current_chno) then
			// it's a match so get to work
				
			// mark it as dead because we don't want it returned in the query
			lstr_pms_data_extract[ll_count].live = false
	
				
			ls_co = trim(lstr_pms_data_extract[ll_count].coauthfn) + ' ' + trim(lstr_pms_data_extract[ll_count].coauth) // 01/13/2009
			//01/13/2009 below
			
			if ls_co <> ls_coauthstr then
				boolean lb_found=FALSE
				lb_found = of_checkcoauthors(ls_first, ls_last,  trim(lstr_pms_data_extract[ll_count].coauthfn),trim(lstr_pms_data_extract[ll_count].coauth))
				IF NOT lb_found THEN
					li_match++
					IF not Isnull(trim(lstr_pms_data_extract[ll_count].chonorific)) THEN
							ls_first[li_match] = trim(lstr_pms_data_extract[ll_count].coauthfn)
							ls_last[li_match] = trim(lstr_pms_data_extract[ll_count].coauth)
							ls_honor[li_match] = trim(lstr_pms_data_extract[ll_count].chonorific)
					ELSE
							ls_first[li_match] =  trim(lstr_pms_data_extract[ll_count].coauthfn)
							ls_last[li_match] = trim(lstr_pms_data_extract[ll_count].coauth)
							ls_honor[li_match] = ''
					END IF
					ls_coauthstr = ls_co
				END IF
			END IF

			ls_narr = trim(lstr_pms_data_extract[ll_count].narrfn) + ' ' + trim(lstr_pms_data_extract[ll_count].narr)
				
			// Is ls_co different from ls_coauthorstr?  If so, fix the original coauthstr
//			if ls_co <> ls_coauthstr then
////				lstr_pms_data_extract[ll_chno_match].coauthstr = "others" // commented 01/13/2009
//				// 01/13/2009
//				IF li_match <=3 THEN
////					lstr_pms_data_extract[ll_chno_match].coauthstr += ', ' + ls_co
//					lstr_pms_data_extract[ll_chno_match].coauthstr +=  ls_co
//			//		li_match++
//				ELSE
////					li_match++
//				END IF
//			end if
				
			// Is ls_narr different from ls_narrstr?  If so, fix the original narrstr
			// set it to null if braille, or arious authors if recorded
			if ls_narr <> ls_narrstr then
				if lstr_pms_data_extract[ll_chno_match].bkmed = "RC"  OR & 
					lstr_pms_data_extract[ll_chno_match].bkmed = "DB" then
					lstr_pms_data_extract[ll_chno_match].narrstr = "Various Narrators"
				else
					setnull(lstr_pms_data_extract[ll_chno_match].narrstr)
				end if
			end if
		else 
			
			// 01/13/2009						
			IF ll_chno_match > 0 THEN
				lstr_pms_data_extract[ll_chno_match].coauthcnt += li_match
				// process string and update the structure element back.
//				ls_temp = of_parsecoauthors(lstr_pms_data_extract[ll_chno_match].coauthstr,lstr_pms_data_extract[ll_chno_match].coauthcnt )
				ls_temp = of_setcoauthors(ls_first[],ls_last[], ls_honor, li_match)
				lstr_pms_data_extract[ll_chno_match].coauthstr = ls_temp			
			END IF


			// Not a match so keep it live and record the information
			// in case the next one's a match
			
			lstr_pms_data_extract[ll_count].live = true
			ls_current_chno = lstr_pms_data_extract[ll_count].chno
			
			 // 01/13/2009
			li_match =1
			ls_coauthstr = trim(lstr_pms_data_extract[ll_count].coauthfn) + ' ' + trim(lstr_pms_data_extract[ll_count].coauth) // 01/13/2009
			//01/13/2009 below
			IF not Isnull(trim(lstr_pms_data_extract[ll_count].chonorific)) THEN
					ls_first[li_match]   =  trim(lstr_pms_data_extract[ll_count].coauthfn)
					ls_last[li_match]    = trim(lstr_pms_data_extract[ll_count].coauth)
					ls_honor[li_match] = trim(lstr_pms_data_extract[ll_count].chonorific)

			ELSE
					ls_first[li_match] =  trim(lstr_pms_data_extract[ll_count].coauthfn)
					ls_last[li_match] = trim(lstr_pms_data_extract[ll_count].coauth)
					ls_honor[li_match] = ''

			END IF
			////////// 01/13/2009
			
			ls_narrstr = trim(lstr_pms_data_extract[ll_count].narrfn) + ' ' + trim(lstr_pms_data_extract[ll_count].narr)
			lstr_pms_data_extract[ll_count].coauthstr =  ls_coauthstr // 1st occurence
			lstr_pms_data_extract[ll_count].narrstr = ls_narrstr
			ll_chno_match = ll_count
			ll_nomatch++
		end if
	NEXT
	
	IF ll_chno_match > 0 THEN
		lstr_pms_data_extract[ll_chno_match].coauthcnt += li_match
		// process string and update the structure element back.
		ls_temp = of_setcoauthors(ls_first[],ls_last[], ls_honor, li_match)
		lstr_pms_data_extract[ll_chno_match].coauthstr = ls_temp			
	END IF
			
END IF
// Set return value
la_returnValue = lstr_pms_data_extract
Return la_returnValue

end function

public function integer wf_create_talkingbooktopic_catalog (date ad_start, date ad_end, integer li_filenum_xml);//////////////////////////////////////////////////////////////////////////////////////////////////////////
//
//	Function:  wf_create_cassette_catalog
//
//	Description:
//	Set menu items
//
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//	
//	Revision History
//
//	Developed by 	Date 				Version						Tracking#
//									
// Murali K.			01/31/2008      002 PICS Modifications	 PICS PMS 2.0 Modifications
//																			PMS B.2, PMS B.2.1
//																Introduce flash books media
//																Pass RC and DB as arguments
//																check for rowcount
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//

n_ds lds
integer rtn,i
string ls_current_chno, ls_narrstr, ls_coauthstr, ls_co, ls_narr, ls_return
string ls_return_xml, ls_pmsub_code, ls_pmsub_desc, ls_xml_tag, ls_chnos[], ls_msg, ls_lang_section
string booksvars, booksvars2,ls_lang1,ls_lang2,books_lang
long ll_rows, ll_rows2, ll_count, ll_chno_match, ll_rec
string ls_filename_xml, ls_path_xml
str_pms_data_extract lstr_pms_data_extract[]
long ll_nomatch=0

lds = create n_ds
lds.dataobject = "d_pms_data_extract"

ll_rows = lds.settransobject(sqlservertrans)
//ll_rows = lds.retrieve(ad_start,ad_end,"RC")
	// 01/31/2008 Include DB also as media to string array as retrieval arguments
	String ls_arg[]
	ls_arg[1] ='RC'
	ls_arg[2] ='DB'
	ll_rows = lds.Retrieve(ad_start,ad_end,ls_arg[])
	////////

setpointer(hourglass!)

if ll_rows = -1 then
	messagebox("Database Error","There was an error during the retrieve.  Please try again.")
	FileClose(li_filenum_xml)
	setpointer(arrow!)
	return 0
end if

if ll_rows = 0 then
	messagebox("No records found","Please adjust your date range and try again.")
	FileClose(li_filenum_xml)
	setpointer(arrow!)
	return 0
end if


// perform  db/rc combination setting 01/16/2009
of_dbrc(lds) // lds reference

Filewrite(li_filenum_xml,'<?xml version="1.0" encoding="UTF-8"?>~n')
//Filewrite(li_filenum_xml,'<!DOCTYPE BooksExports SYSTEM "C:\picsorcl9i\talkingbook.dtd">~n')
Filewrite(li_filenum_xml,'<BooksExports xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">~n')
Filewrite(li_filenum_xml,'<BooksForAdults>~n')
Filewrite(li_filenum_xml,'<Division>Books for Adults</Division>~n')
Filewrite(li_filenum_xml,'<Nonfiction>~n')
Filewrite(li_filenum_xml,'<Section>Nonfiction</Section>~n')
		
lds.settransobject(sqlservertrans)
booksvars = "AN";
booksvars2 = "YN";
books_lang = "ENG";
lds.setFilter("(ttlinit_ajyfn = '"+ booksvars +" ' or ttlinit_ajyfn = '"+ booksvars2+ "') and ttlinit_lang = '"+ books_lang +"'");
rtn = lds.Filter()
if rtn = 1 then
	lds.SetSort("mchar_bkseq")
	lds.Sort()
	ll_rows = lds.rowcount()
	// Copy the rows to a structure so we can work with it
	// 01/31/2008 check for rowcount
	IF ll_rows > 0 THEN
		lstr_pms_data_extract[] = lds.object.data
	END IF
else
	messagebox("error","results of ttlinit_ajyfn = an was zero")
end if
		
lstr_pms_data_extract = wf_coauth_narr(lds)
		
FOR ll_count=1 TO ll_rows
	if lstr_pms_data_extract[ll_count].live = true then
		Filewrite(li_filenum_xml,'<book>')
		ls_return_xml = f_export_to_xml(lstr_pms_data_extract[ll_count])
		Filewrite(li_filenum_xml,ls_return_xml)
		ll_rec++
		Filewrite(li_filenum_xml,'</book>')
	end if
NEXT

Filewrite(li_filenum_xml,'</Nonfiction>~n')
Filewrite(li_filenum_xml,'<Fiction>~n')
Filewrite(li_filenum_xml,'<Section>Fiction</Section>~n')
		
// remove the filter
lds.setFilter("");
lds.Filter()
		
lds.settransobject(sqlservertrans)
booksvars = "AF";
booksvars2 = "YF";
books_lang = "ENG";
lds.setFilter("(ttlinit_ajyfn = '"+ booksvars +" ' or ttlinit_ajyfn = '"+ booksvars2+ "') and ttlinit_lang = '"+ books_lang +"'");
rtn = lds.Filter()
if rtn = 1 then
	lds.SetSort("mchar_bkseq")
	lds.Sort()
	ll_rows = lds.rowcount()
	// Copy the rows to a structure so we can work with it
	IF ll_rows > 0 THEN
		lstr_pms_data_extract[] = lds.object.data
	END IF
else
	messagebox("error","results of ttlinit_ajyfn = af was zero")
end if
		
lstr_pms_data_extract = wf_coauth_narr(lds)
		
FOR ll_count=1 TO ll_rows
	if lstr_pms_data_extract[ll_count].live = true then
		Filewrite(li_filenum_xml,'<book>')
		ls_return_xml = f_export_to_xml(lstr_pms_data_extract[ll_count])
		Filewrite(li_filenum_xml,ls_return_xml)
		ll_rec++
		Filewrite(li_filenum_xml,'</book>')
	end if
NEXT

Filewrite(li_filenum_xml,'</Fiction>~n')
Filewrite(li_filenum_xml,'</BooksForAdults>~n')
Filewrite(li_filenum_xml,'<BooksForChildren>~n')
Filewrite(li_filenum_xml,'<Division>Books for Children</Division>~n')
Filewrite(li_filenum_xml,'<Nonfiction>~n')
Filewrite(li_filenum_xml,'<Section>Nonfiction</Section>~n')
		
// remove the filter
lds.setFilter("");
lds.Filter()
		
lds.settransobject(sqlservertrans)
booksvars = "JN";
books_lang = "ENG";
lds.setFilter("ttlinit_ajyfn = '"+ booksvars +" ' and ttlinit_lang = '"+ books_lang +"'");
rtn = lds.Filter()
if rtn = 1 then
	lds.SetSort("mchar_bkseq")
	lds.Sort()
	ll_rows = lds.rowcount()
	// Copy the rows to a structure so we can work with it
	IF ll_rows > 0 THEN
		lstr_pms_data_extract[] = lds.object.data
	END IF
else
	messagebox("error","results of ttlinit_ajyfn = yn was zero")
end if
		
lstr_pms_data_extract = wf_coauth_narr(lds)
		
FOR ll_count=1 TO ll_rows
	if lstr_pms_data_extract[ll_count].live = true then
		Filewrite(li_filenum_xml,'<book>')
		ls_return_xml = f_export_to_xml(lstr_pms_data_extract[ll_count])
		Filewrite(li_filenum_xml,ls_return_xml)
		ll_rec++
		Filewrite(li_filenum_xml,'</book>')
	end if
NEXT
Filewrite(li_filenum_xml,'</Nonfiction>~n')
Filewrite(li_filenum_xml,'<Fiction>~n')
Filewrite(li_filenum_xml,'<Section>Fiction</Section>~n')
		
// remove the filter
lds.setFilter("");
lds.Filter()
		
lds.settransobject(sqlservertrans)
booksvars = "JF";
books_lang = "ENG";
lds.setFilter("ttlinit_ajyfn = '"+ booksvars +" ' and ttlinit_lang = '"+ books_lang +"'");
rtn = lds.Filter()
if rtn = 1 then
	lds.SetSort("mchar_bkseq")
	lds.Sort()
	ll_rows = lds.rowcount()
	// Copy the rows to a structure so we can work with it
	IF LL_ROWS > 0 THEN
		lstr_pms_data_extract[] = lds.object.data
	END IF
else
	messagebox("error","results of ttlinit_ajyfn = yf was zero")
end if
		
lstr_pms_data_extract = wf_coauth_narr(lds)
		
FOR ll_count=1 TO ll_rows
	if lstr_pms_data_extract[ll_count].live = true then
		Filewrite(li_filenum_xml,'<book>')
		ls_return_xml = f_export_to_xml(lstr_pms_data_extract[ll_count])
		Filewrite(li_filenum_xml,ls_return_xml)
		ll_rec++
		Filewrite(li_filenum_xml,'</book>')
	end if
NEXT
Filewrite(li_filenum_xml,'</Fiction>~n')
Filewrite(li_filenum_xml,'</BooksForChildren>~n')
		
// remove the filter
lds.setFilter("");
lds.Filter()
		
lds.settransobject(sqlservertrans)
booksvars = "ENG";
// Get all the foreign langauge books (not english)
lds.setFilter("ttlinit_lang <> '"+ booksvars +" '");
rtn = lds.Filter()
if rtn = 1 then
	ll_rows = lds.rowcount()
	lds.SetSort("ttlinit_lang, mchar_bkseq asc") // 12/8/2009 #2248 sorting 
	lds.Sort()
	IF ll_rows > 0 THEN
		// Copy the rows to a structure so we can work with it
		lstr_pms_data_extract[] = lds.object.data
	END IF
else
	messagebox("error","results of ttlinit_lang <> english was zero")
end if
	
IF ll_rows > 0 THEN
	Filewrite(li_filenum_xml,'<ForeignLanguageBook>~n')
	Filewrite(li_filenum_xml,'<Division>Foreign Language Books</Division>~n')
	ls_lang1 = '<'+trim(lstr_pms_data_extract[1].lang_desc)+'>~n'
	Filewrite(li_filenum_xml,ls_lang1)
	ls_lang_section = '<Section>'+trim(lstr_pms_data_extract[1].lang_desc)+'</Section>~n'
	Filewrite(li_filenum_xml,ls_lang_section)
		
	lstr_pms_data_extract = wf_coauth_narr(lds)
		
		
	FOR ll_count=1 TO ll_rows
		if lstr_pms_data_extract[ll_count].live = true then
			ls_lang2 = '<'+trim(lstr_pms_data_extract[ll_count].lang_desc)+'>~n'
			IF ls_lang2 <> ls_lang1 THEN
				ls_lang1 = '</'+trim(lstr_pms_data_extract[ll_count].lang_desc)+'>~n'
				Filewrite(li_filenum_xml,ls_lang1)
				Filewrite(li_filenum_xml,ls_lang2)
				ls_lang_section = '<Section>'+trim(lstr_pms_data_extract[ll_count].lang_desc)+'</Section>~n'
				Filewrite(li_filenum_xml,ls_lang_section)
			END IF	
			Filewrite(li_filenum_xml,'<book>')
			ls_return_xml = f_export_to_xml(lstr_pms_data_extract[ll_count])
			Filewrite(li_filenum_xml,ls_return_xml)
			ll_rec++
			Filewrite(li_filenum_xml,'</book>')
		
			ls_lang1 = ls_lang2
		end if
	NEXT
	ls_lang1 = '</'+trim(lstr_pms_data_extract[ll_rows].lang_desc)+'>~n'
	Filewrite(li_filenum_xml,ls_lang1)		
	Filewrite(li_filenum_xml,'</ForeignLanguageBook>~n')
END IF

Filewrite(li_filenum_xml,'</BooksExports>~n')
Fileclose(li_filenum_xml)


RETURN ll_rec
end function

public function integer wf_create_braillebookreview_catalog (date ad_start, date ad_end, integer li_filenum_xml);//////////////////////////////////////////////////////////////////////////////////////////////////////////
//
//	Function:  wf_create_cassette_catalog
//
//	Description:
//	Set menu items
//
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//	
//	Revision History
//
//	Developed by 	Date 				Version						Tracking#
//									
// Murali K.			01/31/2008      002 PICS Modifications	 PICS PMS 2.0 Modifications
//																			PMS B.2, PMS B.2.1
//																array argument for media
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//
n_ds lds
integer rtn,i
string ls_current_chno, ls_narrstr, ls_coauthstr, ls_co, ls_narr, ls_return
string ls_return_xml, ls_pmsub_code, ls_pmsub_desc, ls_xml_tag, ls_chnos[], ls_msg, ls_lang_section, ls_pmsub_desc2
string booksvars, booksvars2,ls_lang1,ls_lang2,books_lang
long ll_rows, ll_rows2, ll_count, ll_chno_match, ll_rec
string ls_filename_xml, ls_path_xml,ls_filter
str_pms_data_extract lstr_pms_data_extract[]
long ll_nomatch=0

lds = create n_ds
lds.dataobject = "d_pms_data_extract"

ll_rows = lds.settransobject(sqlservertrans)
//ll_rows = lds.retrieve(ad_start,ad_end,"BR")
// 01/31/2008 array arguments
String ls_arg[]
ls_arg[1] = 'BR'
ll_rows = lds.retrieve(ad_start,ad_end,ls_arg[])


setpointer(hourglass!)

if ll_rows = -1 then
	messagebox("Database Error","There was an error during the retrieve.  Please try again.")
	FileClose(li_filenum_xml)
	setpointer(arrow!)
	return 0
end if

if ll_rows = 0 then
	messagebox("No records found","Please adjust your date range and try again.")
	FileClose(li_filenum_xml)
	setpointer(arrow!)
	return 0
end if

Filewrite(li_filenum_xml,'<?xml version="1.0" encoding="UTF-8"?>~n')
//Filewrite(li_filenum_xml,'<!DOCTYPE BooksExports SYSTEM "C:\picsorcl9i\braillebookreview.dtd">~n')
Filewrite(li_filenum_xml,'<BooksExports xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">~n')
Filewrite(li_filenum_xml,'<BooksForAdults>~n')
Filewrite(li_filenum_xml,'<Division>Books for Adults</Division>~n')
Filewrite(li_filenum_xml,'<AdultNonfiction>~n')
Filewrite(li_filenum_xml,'<Section>Adult Nonfiction</Section>~n')
		
		
booksvars = "AN";
booksvars2 = "YN";
books_lang = "ENG";
lds.setFilter("(ttlinit_ajyfn = '"+ booksvars +" ' or ttlinit_ajyfn = '"+ booksvars2+ "') and ttlinit_lang = '"+ books_lang +"'");
rtn = lds.Filter()
if rtn = 1 then
	lds.SetSort("mchar_bkseq")
	lds.Sort()
	ll_rows = lds.rowcount()
	// Copy the rows to a structure so we can work with it
	IF ll_rows > 0 THEN 
		lstr_pms_data_extract[] = lds.object.data 
	END IF
else
	messagebox("error","The result of the query ttlinit_ajyfn = AN was zero")
	FileClose(li_filenum_xml)
	return 0
end if
		
lstr_pms_data_extract = wf_coauth_narr(lds)
		
FOR ll_count=1 TO ll_rows
	if lstr_pms_data_extract[ll_count].live = true then
		Filewrite(li_filenum_xml,'<book>')
		ls_return_xml = f_export_to_xml(lstr_pms_data_extract[ll_count])
		Filewrite(li_filenum_xml,ls_return_xml)
		ll_rec++
		Filewrite(li_filenum_xml,'</book>')
	end if
NEXT

Filewrite(li_filenum_xml,'</AdultNonfiction>~n')
Filewrite(li_filenum_xml,'<AdultFiction>~n')
Filewrite(li_filenum_xml,'<Section>Adult Fiction</Section>~n')
		
// remove the filter
lds.setFilter("");
lds.Filter()
		
lds.settransobject(sqlservertrans)
booksvars = "AF";
booksvars2 = "YF";
books_lang = "ENG";
lds.setFilter("(ttlinit_ajyfn = '"+ booksvars +" ' or ttlinit_ajyfn = '"+ booksvars2+ "') and ttlinit_lang = '"+ books_lang +"'");

rtn = lds.Filter()
if rtn = 1 then
	lds.SetSort("mchar_bkseq")
	lds.Sort()
	ll_rows = lds.rowcount()
	// Copy the rows to a structure so we can work with it
	IF ll_rows > 0 THEN 
		lstr_pms_data_extract[] = lds.object.data
	END IF
else
	messagebox("error","The result of the query ttlinit_ajyfn = AF was zero")
	FileClose(li_filenum_xml)
	return 0
end if
		
lstr_pms_data_extract = wf_coauth_narr(lds)
		
FOR ll_count=1 TO ll_rows
	if lstr_pms_data_extract[ll_count].live = true then
		Filewrite(li_filenum_xml,'<book>')
		ls_return_xml = f_export_to_xml(lstr_pms_data_extract[ll_count])
		Filewrite(li_filenum_xml,ls_return_xml)
		ll_rec++
		Filewrite(li_filenum_xml,'</book>')
	end if
NEXT

Filewrite(li_filenum_xml,'</AdultFiction>~n')
Filewrite(li_filenum_xml,'</BooksForAdults>~n')
Filewrite(li_filenum_xml,'<BooksForChildren>~n')
Filewrite(li_filenum_xml,'<Division>Books for Children</Division>~n')
Filewrite(li_filenum_xml,'<ChildrenNonfiction>~n')
Filewrite(li_filenum_xml,'<Section>Childrens Nonfiction</Section>~n')
		
// remove the filter
lds.setFilter("");
lds.Filter()
		
lds.settransobject(sqlservertrans)
booksvars = "JN";
books_lang = "ENG";
lds.setFilter("ttlinit_ajyfn = '"+ booksvars +" ' and ttlinit_lang = '"+ books_lang +"'");
rtn = lds.Filter()
if rtn = 1 then
	lds.SetSort("mchar_bkseq")
	lds.Sort()
	ll_rows = lds.rowcount()
	// Copy the rows to a structure so we can work with it
	IF ll_rows > 0 THEN 
		lstr_pms_data_extract[] = lds.object.data
	END IF
else
	messagebox("error","The result of the query ttlinit_ajyfn = YN was zero")
	FileClose(li_filenum_xml)
	return 0
end if
		
lstr_pms_data_extract = wf_coauth_narr(lds)
		
FOR ll_count=1 TO ll_rows
	if lstr_pms_data_extract[ll_count].live = true then
		Filewrite(li_filenum_xml,'<book>')
		ls_return_xml = f_export_to_xml(lstr_pms_data_extract[ll_count])
		Filewrite(li_filenum_xml,ls_return_xml)
		ll_rec++
		Filewrite(li_filenum_xml,'</book>')
	end if
NEXT
Filewrite(li_filenum_xml,'</ChildrenNonfiction>~n')
Filewrite(li_filenum_xml,'<ChildrenFiction>~n')
Filewrite(li_filenum_xml,'<Section>Childrens Fiction</Section>~n')
		
// remove the filter
lds.setFilter("");
lds.Filter()
		
lds.settransobject(sqlservertrans)
booksvars = "JF";
books_lang = "ENG";
lds.setFilter("ttlinit_ajyfn = '"+ booksvars +" ' and ttlinit_lang = '"+ books_lang +"'");
rtn = lds.Filter()
if rtn = 1 then
	lds.SetSort("mchar_bkseq")
	lds.Sort()
	ll_rows = lds.rowcount()
	// Copy the rows to a structure so we can work with it
	IF ll_rows > 0 THEN 
		lstr_pms_data_extract[] = lds.object.data
	END IF
else
	messagebox("error","The result of the query ttlinit_ajyfn = YF was zero")
	FileClose(li_filenum_xml)
	return 0
end if
		
lstr_pms_data_extract = wf_coauth_narr(lds)
		
FOR ll_count=1 TO ll_rows
	if lstr_pms_data_extract[ll_count].live = true then
		Filewrite(li_filenum_xml,'<book>')
		ls_return_xml = f_export_to_xml(lstr_pms_data_extract[ll_count])
		Filewrite(li_filenum_xml,ls_return_xml)
		ll_rec++
		Filewrite(li_filenum_xml,'</book>')
	end if
NEXT
Filewrite(li_filenum_xml,'</ChildrenFiction>~n')
Filewrite(li_filenum_xml,'</BooksForChildren>~n')
		
// remove the filter
lds.setFilter("");
lds.Filter()

ls_pmsub_code = 'Y SPAN'
lds.settransobject(sqlservertrans)
booksvars = "ENG";
booksvars2 = "E/S";
//lds.setFilter("ttlinit_lang <> '"+ booksvars +" ' and ttlinit_lang <> '"+ booksvars2+ " '");
ls_filter = "(ttlinit_pmsub1 = '"+ ls_pmsub_code+ "' or ttlinit_pmsub2 = '"+ ls_pmsub_code+ "' or ttlinit_pmsub3 = '"+ ls_pmsub_code+ "')"+ " and ttlinit_lang <> '"+ booksvars +" ' and ttlinit_lang <> '"+ booksvars2+ " '"
//ls_filter = "ttlinit_pmsub1 = '"+ ls_pmsub_code+ "' or ttlinit_pmsub2 = '"+ ls_pmsub_code+ "' or ttlinit_pmsub3 = '"+ ls_pmsub_code+ " '"
lds.SetFilter(ls_filter);
//Get all the foreign language books (not english)
rtn = lds.Filter()
if rtn = 1 then
	ll_rows = lds.rowcount()
	lds.SetSort("ttlinit_lang")
	lds.Sort()
	IF ll_rows > 0 THEN
		// Copy the rows to a structure so we can work with it
		lstr_pms_data_extract[] = lds.object.data
	END IF
else
	messagebox("foreign language","The result of the filter ttlinit_lang <> english was zero")
	FileClose(li_filenum_xml)
	return 0
end if
	
IF ll_rows > 0 THEN
	Filewrite(li_filenum_xml,'<ForeignLanguageBook>~n')
	Filewrite(li_filenum_xml,'<Division>Foreign Language Book</Division>~n')
	ls_lang1 = '<'+trim(lstr_pms_data_extract[1].lang_desc)+'>~n'
	Filewrite(li_filenum_xml,ls_lang1)
	ls_lang_section = '<Section>'+trim(lstr_pms_data_extract[1].lang_desc)+'</Section>~n'
	Filewrite(li_filenum_xml,ls_lang_section)
		
	lstr_pms_data_extract = wf_coauth_narr(lds)
		
		
	FOR ll_count=1 TO ll_rows
		if lstr_pms_data_extract[ll_count].live = true then
			ls_lang2 = '<'+trim(lstr_pms_data_extract[ll_count].lang_desc)+'>~n'
			IF ls_lang2 <> ls_lang1 THEN
				ls_lang1 = '</'+trim(lstr_pms_data_extract[ll_count].lang_desc)+'>~n'
				Filewrite(li_filenum_xml,ls_lang1)
				Filewrite(li_filenum_xml,ls_lang2)
				ls_lang_section = '<Section>'+trim(lstr_pms_data_extract[ll_count].lang_desc)+'</Section>~n'
				Filewrite(li_filenum_xml,ls_lang_section)
			END IF	
			Filewrite(li_filenum_xml,'<book>')
			ls_return_xml = f_export_to_xml(lstr_pms_data_extract[ll_count])
			Filewrite(li_filenum_xml,ls_return_xml)
			ll_rec++
			Filewrite(li_filenum_xml,'</book>')
		
			ls_lang1 = ls_lang2
		end if
	NEXT
	ls_lang1 = '</'+trim(lstr_pms_data_extract[ll_rows].lang_desc)+'>~n'
	Filewrite(li_filenum_xml,ls_lang1)		
	Filewrite(li_filenum_xml,'</ForeignLanguageBook>~n')
END IF

Filewrite(li_filenum_xml,'</BooksExports>~n')
Fileclose(li_filenum_xml)

RETURN ll_REC
end function

public function integer wf_create_cassette_catalog (date ad_start, date ad_end, integer li_filenum_xml);//////////////////////////////////////////////////////////////////////////////////////////////////////////
//
//	Function:  wf_create_cassette_catalog
//
//	Description:
//	Set menu items
//
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//	
//	Revision History
//
//	Developed by 	Date 				Version						Tracking#
//									
// Murali K.			01/31/2008      002 PICS Modifications	 PICS PMS 2.0 Modifications
//																			PMS B.2, PMS B.2.1
//																Introduce flash books media
//																Pass RC and DB as arguments
//																xml file will be saved as dbbooksplus
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//
n_ds lds,lds2,lds3
Integer rtn,i,J
String ls_current_chno, ls_narrstr, ls_coauthstr, ls_co, ls_narr, ls_return, booksvars,  booksvars2, booksvars3, ls_filter
String ls_return_xml, ls_pmsub_code, ls_pmsub_desc, ls_pmsub_desc2, ls_xml_tag, ls_chnos[], ls2_chnos[], ls_msg, ls_cat
Long ll_rows, ll_rows2, ll_rows3, ll_count, ll_chno_match, ll_rec
String ls_filename_xml, ls_path_xml, ls_lang1, ls_lang2, tmp_chno, newsort, ls_write, LS_TEMP
str_pms_data_extract lstr_pms_data_extract[]
Long ll_nomatch=0

integer li_FileNum

//li_FileNum = FileOpen("C:\clist.txt", LineMode!, Write!, LockWrite!, Replace!)
		  
// Create and load the datastore

lds = CREATE n_ds
lds.dataObject = "d_pms_data_extract"

lds2 = CREATE n_ds
lds2.dataObject = "d_pmsub_desc2"

ll_rows = lds.SetTransObject(SqlServerTrans)
//ll_rows = lds.Retrieve(ad_start,ad_end,"RC") // 01/31/2008
// 01/31/2008 Include DB also as media to string array as retrieval arguments
String ls_arg[]
ls_arg[1] = 'RC'
ls_arg[2] = 'DB'
ll_rows = lds.Retrieve(ad_start,ad_end,ls_arg[])
////////

ll_rows2 = lds2.SetTransObject(SqlServerTrans)
ll_rows2 = lds2.Retrieve(ad_start,ad_end)

SetPointer (hourglass!)

IF ll_rows = -1  OR ll_rows2 = -1 THEN
	Messagebox("Database Error","There was an error during the retrieve.  Please try again.")
	FileClose(li_filenum_xml)
	SetPointer(arrow!)
	RETURN 0
END IF

IF ll_rows = 0  OR ll_rows2 = -1 THEN
	Messagebox("No records found","Please adjust your date range and try again.")
	FileClose(li_filenum_xml)
	SetPointer(arrow!)
	RETURN 0
END IF

// Setup the horizontal progress bar
hpb_1.SetRange (1, 100)
hpb_1.maxPosition = ll_rows2
hpb_1.visible = TRUE


// perform  db/rc combination setting 01/16/2009
of_dbrc(lds) // lds reference

FileWrite(li_filenum_xml,'<?xml version="1.0" encoding="UTF-8"?>~n') // ISO-88591-1
//FileWrite(li_filenum_xml,'<!DOCTYPE BooksExports SYSTEM "C:\picsorcl9i\cassettebooks.dtd">~n')
//FileWrite(li_filenum_xml,'<!DOCTYPE BooksExports SYSTEM "C:\picsorcl9i\dbbooksplus.dtd">~n') // 01/31/2008
FileWrite(li_filenum_xml,'<BooksExports xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">~n')
FileWrite(li_filenum_xml,'<Nonfiction>~n')
FileWrite(li_filenum_xml,'<Section>Nonfiction</Section>~n')

FOR i = 1 TO ll_rows2
		Yield () 
		ls_pmsub_code = lds2.object.pmsub_code[i]

		lds.SetTransObject(SqlServerTrans)
		booksvars = "AN";
		booksvars3 = "YN";
		booksvars2 = "SPA";
		ls_filter = "(ttlinit_ajyfn = '"+ booksvars +"' or  ttlinit_ajyfn = '"+ booksvars3 +"') and (ttlinit_pmsub1 = '"+ ls_pmsub_code+ "' or ttlinit_pmsub2 = '"+ ls_pmsub_code+ "' or ttlinit_pmsub3 = '"+ ls_pmsub_code+ "')"+ " and ttlinit_lang <> '"+ booksvars2+"'"
		lds.SetFilter(ls_filter);
		rtn = lds.Filter()
		IF rtn = 1 THEN
			newsort = "ttlinit_ttl A"
			lds.SetSort(newsort)
			lds.Sort()			
			ll_rows = lds.RowCount()
			IF ll_rows > 0 THEN
				ls_pmsub_desc = f_remove_all_spaces(lds2.object.pmsub_desc[i]) //1/15/2009
				IF Trim(lds2.object.pmsub_desc[i]) = 'Arts, The' THEN
					ls_pmsub_desc = 'ArtsThe'
				END IF
				ls_xml_tag = '<'+ls_pmsub_desc+'>~n'
//ls_xml_tag = '<CategoryGroup group="'+ ls_pmsub_desc + '" >'
//				ls_xml_tag = '<CategoryGroup>'+ls_pmsub_desc+'</CategoryGroup>~n'
				FileWrite(li_filenum_xml,ls_xml_tag)
				ls_pmsub_desc2 = lds2.object.pmsub_desc[i]
				IF ls_pmsub_desc = 'ArtsThe' THEN
					ls_pmsub_desc2 = 'The Arts'
				END IF
				ls_pmsub_desc2 = of_labelcategory(ls_pmsub_code,'N',ls_pmsub_desc2) // 04/07/2009 label certain categories depending on section
				ls_cat = '<Category>'+ls_pmsub_desc2+'</Category>'
				FileWrite(li_filenum_xml,ls_cat)
				// Copy the rows to a structure so we can work with it
				lstr_pms_data_extract[] = lds.object.Data
			ELSE
	//			GOTO NextPMSUBAN
				CONTINUE
			END IF
		ELSE
			Messagebox("error","results of filtering ttlinit_pmsub1 failed")
			FileClose(li_filenum_xml)
			RETURN 0
		END IF
			
		// Modify the coauthors and the narrators in the string array
		lstr_pms_data_extract = wf_coauth_narr(lds)
			
		ls_msg = "Adult Nonfiction PM Subj code of: "+ls_pmsub_desc
		OpenWithParm(w_pics_retrieve_msg_box,ls_msg)
			
		FOR ll_count=1 TO ll_rows
			IF lstr_pms_data_extract[ll_count].live = TRUE THEN
				FileWrite(li_filenum_xml,'<book>')
				ls_return_xml = f_export_to_xml(lstr_pms_data_extract[ll_count])
				FileWrite(li_filenum_xml,ls_return_xml)
				ll_rec++
				FileWrite(li_filenum_xml,'</book>')
			END IF
		NEXT
			
		ls_xml_tag = '</'+ls_pmsub_desc+'>~n'
//		ls_xml_tag = '</CategoryGroup>' + '~n'
	// 01/29/2009
	
		FileWrite(li_filenum_xml,ls_xml_tag)
		// remove the filter for next PMSUB Code
		lds.SetFilter("");
		lds.Filter()
		Close(w_pics_retrieve_msg_box)
		hpb_1.StepIt ()
//		NextPMSUBAN: 
NEXT
FileWrite(li_filenum_xml,'</Nonfiction>~n')

hpb_1.SetRange (1, 100)
hpb_1.maxPosition = ll_rows2
	
FileWrite(li_filenum_xml,'<Fiction>~n')
FileWrite(li_filenum_xml,'<Section>Fiction</Section>~n')
//Adult Fiction Books
FOR i = 1 TO ll_rows2
		Yield () 
		ls_pmsub_code = lds2.object.pmsub_code[i]
		
		lds.SetTransObject(SqlServerTrans)
		booksvars = "AF";
		booksvars3 = "YF";
		booksvars2 = "SPA";
		ls_filter = "(ttlinit_ajyfn = '"+ booksvars +"' or  ttlinit_ajyfn = '"+ booksvars3 +"')  and (ttlinit_pmsub1 = '"+ ls_pmsub_code+ "' or ttlinit_pmsub2 = '"+ ls_pmsub_code+ "' or ttlinit_pmsub3 = '"+ ls_pmsub_code+ "')"+ " and ttlinit_lang <> '"+ booksvars2+"'"
		lds.SetFilter(ls_filter);
		rtn = lds.Filter()
		IF rtn = 1 THEN
			ll_rows = lds.RowCount()
			IF ll_rows > 0 THEN
				
				ls_pmsub_desc = f_remove_all_spaces(lds2.object.pmsub_desc[i])
				IF ls_pmsub_desc = 'Arts,The' THEN
					ls_pmsub_desc = 'TheArts'
				END IF
// 01/29/2009
				ls_xml_tag = '<'+ls_pmsub_desc+'>~n'
//ls_xml_tag = '<CategoryGroup group="'+ ls_pmsub_desc + '" >'
// 01/29/2009

				FileWrite(li_filenum_xml,ls_xml_tag)
				//01/15/2009
				ls_pmsub_desc2 = lds2.object.pmsub_desc[i] 
				//
				ls_pmsub_desc2 = of_labelcategory(ls_pmsub_code,'F',ls_pmsub_desc2) // 04/07/2009 label certain categories depending on section
				ls_cat = '<Category>'+ls_pmsub_desc2+'</Category>'
				
				FileWrite(li_filenum_xml,ls_cat)
				// Copy the rows to a structure so we can work with it
				lstr_pms_data_extract[] = lds.object.Data
			ELSE
//				GOTO NextPMSUBAF
				CONTINUE
			END IF
		ELSE
			Messagebox("error","results of filtering ttlinit_pmsub1 failed")
			FileClose(li_filenum_xml)
			RETURN 0
		END IF
			
		// Modify the coauthors and the narrators in the string array
		lstr_pms_data_extract = wf_coauth_narr(lds)
			
		ls_msg = "Adult Fiction PM Subj code of: "+ls_pmsub_desc
		OpenWithParm(w_pics_retrieve_msg_box,ls_msg)
			
		FOR ll_count=1 TO ll_rows
			IF lstr_pms_data_extract[ll_count].live = TRUE THEN
				FileWrite(li_filenum_xml,'<book>')
				ls_return_xml = f_export_to_xml(lstr_pms_data_extract[ll_count])
				FileWrite(li_filenum_xml,ls_return_xml)
				ll_rec++
				
//				// 01/15/2009
//ls_write = lstr_pms_data_extract[ll_count].bkmed + ','+ string(lstr_pms_data_extract[ll_count].bkseq)  +',' +  lstr_pms_data_extract[ll_count].chno
//				FileWrite(li_FileNum, ls_write)
				// 01/15/2009
				FileWrite(li_filenum_xml,'</book>')
			END IF
		NEXT
			
//01/29/2009
		ls_xml_tag = '</'+ls_pmsub_desc+'>~n'
//		ls_xml_tag = '</CategoryGroup>' + '~n'
//// 01/29/2009

		FileWrite(li_filenum_xml,ls_xml_tag)
		// remove the filter for next PMSUB Code
		lds.SetFilter("");
		lds.Filter()
		Close(w_pics_retrieve_msg_box)
		hpb_1.StepIt ()
//		NextPMSUBAF: 
NEXT


FileWrite(li_filenum_xml,'</Fiction>~n')


// Young Adults
FileWrite(li_filenum_xml,'<YoungAdults>~n')
FileWrite(li_filenum_xml,'<Section>YoungAdults</Section>~n')
FileWrite(li_filenum_xml,'<Nonfiction>~n')
FileWrite(li_filenum_xml,'<Category>Nonfiction</Category>~n')

Yield () 
		
lds.SetTransObject(SqlServerTrans)
booksvars = "YN";
booksvars2 = "SPA";
ls_filter = "ttlinit_ajyfn = '"+ booksvars +"' and (ttlinit_pmsub1 = 'YOUN' or ttlinit_pmsub2 = 'YOUN' or ttlinit_pmsub3 = 'YOUN')"+ " and ttlinit_lang <> '"+ booksvars2+"'"
lds.SetFilter(ls_filter);
rtn = lds.Filter()
ll_rows = lds.RowCount()
hpb_1.SetRange (1, 100)
hpb_1.maxPosition = ll_rows
			
// Modify the coauthors and the narrators in the string array
lstr_pms_data_extract = wf_coauth_narr(lds)
			
ls_msg = "Young Adult Nonfiction"
OpenWithParm(w_pics_retrieve_msg_box,ls_msg)
			
FOR ll_count=1 TO ll_rows
	IF lstr_pms_data_extract[ll_count].live = TRUE THEN
		FileWrite(li_filenum_xml,'<book>')
		ls_return_xml = f_export_to_xml(lstr_pms_data_extract[ll_count])
		FileWrite(li_filenum_xml,ls_return_xml)
		ll_rec++
//						// 01/15/2009
//ls_write = lstr_pms_data_extract[ll_count].bkmed + ','+ string(lstr_pms_data_extract[ll_count].bkseq)  +',' +  lstr_pms_data_extract[ll_count].chno
//				FileWrite(li_FileNum, ls_write)
				// 01/15/2009
		FileWrite(li_filenum_xml,'</book>')
	END IF
	hpb_1.StepIt ()
NEXT
			
// remove the filter for next PMSUB Code
lds.SetFilter("");
lds.Filter()
Close(w_pics_retrieve_msg_box)
FileWrite(li_filenum_xml,'</Nonfiction>~n')

FileWrite(li_filenum_xml,'<Fiction>~n')
FileWrite(li_filenum_xml,'<Category>Fiction</Category>~n')
//Young Adult Fiction Books
Yield () 
		
lds.SetTransObject(SqlServerTrans)
booksvars = "YF";
booksvars2 = "SPA";
ls_filter = "ttlinit_ajyfn = '"+ booksvars +"'"+" and ttlinit_lang <> '"+ booksvars2 +"'"
lds.SetFilter(ls_filter);
rtn = lds.Filter()
ll_rows = lds.RowCount()
hpb_1.SetRange (1, 100)
hpb_1.maxPosition = ll_rows
			
// Modify the coauthors and the narrators in the string array
lstr_pms_data_extract = wf_coauth_narr(lds)
			
ls_msg = "Young Adult Fiction"
OpenWithParm(w_pics_retrieve_msg_box,ls_msg)
			
FOR ll_count=1 TO ll_rows
	IF lstr_pms_data_extract[ll_count].live = TRUE THEN
		FileWrite(li_filenum_xml,'<book>')
		ls_return_xml = f_export_to_xml(lstr_pms_data_extract[ll_count])
		FileWrite(li_filenum_xml,ls_return_xml)
		ll_rec++
//						// 01/15/2009
//ls_write = lstr_pms_data_extract[ll_count].bkmed + ','+ string(lstr_pms_data_extract[ll_count].bkseq)  +',' +  lstr_pms_data_extract[ll_count].chno
//				FileWrite(li_FileNum, ls_write)
				// 01/15/2009
		FileWrite(li_filenum_xml,'</book>')
	END IF
	hpb_1.StepIt ()
NEXT
			
// remove the filter for next PMSUB Code
lds.SetFilter("");
lds.Filter()
Close(w_pics_retrieve_msg_box)


FileWrite(li_filenum_xml,'</Fiction>~n')
FileWrite(li_filenum_xml,'</YoungAdults>~n')


//Spanish
FileWrite(li_filenum_xml,'<Spanish>~n')
FileWrite(li_filenum_xml,'<Section>Spanish</Section>~n')
FileWrite(li_filenum_xml,'<Nonfiction>~n')
FileWrite(li_filenum_xml,'<Category>Nonfiction</Category>~n')
//Spanish Adult Nonfiction Books
Yield () 
		
lds.SetTransObject(SqlServerTrans)
booksvars = "SPA";
booksvars2 = "AN";
ls_filter = "ttlinit_lang = '"+ booksvars +"' and ttlinit_ajyfn = '"+ booksvars2 + "'"
lds.SetFilter(ls_filter);
rtn = lds.Filter()
ll_rows = lds.RowCount()
hpb_1.SetRange (1, 100)
hpb_1.maxPosition = ll_rows
			
// Modify the coauthors and the narrators in the string array
lstr_pms_data_extract = wf_coauth_narr(lds)
			
ls_msg = "Spanish Nonfiction"
OpenWithParm(w_pics_retrieve_msg_box,ls_msg)
			
FOR ll_count=1 TO ll_rows
	IF lstr_pms_data_extract[ll_count].live = TRUE THEN
		FileWrite(li_filenum_xml,'<book>')
		ls_return_xml = f_export_to_xml(lstr_pms_data_extract[ll_count])
		FileWrite(li_filenum_xml,ls_return_xml)
		ll_rec++
//						// 01/15/2009
//ls_write = lstr_pms_data_extract[ll_count].bkmed + ','+ string(lstr_pms_data_extract[ll_count].bkseq)  +',' +  lstr_pms_data_extract[ll_count].chno
//				FileWrite(li_FileNum, ls_write)
				// 01/15/2009
		FileWrite(li_filenum_xml,'</book>')
	END IF
	hpb_1.StepIt ()
NEXT
			
// remove the filter for next PMSUB Code
lds.SetFilter("");
lds.Filter()
Close(w_pics_retrieve_msg_box)
FileWrite(li_filenum_xml,'</Nonfiction>~n')
FileWrite(li_filenum_xml,'<Fiction>~n')
FileWrite(li_filenum_xml,'<Category>Fiction</Category>~n')
//Spanish Fiction Books
Yield () 
		
lds.SetTransObject(SqlServerTrans)
booksvars = "SPA";
booksvars2 = "AF";
ls_filter = "ttlinit_lang = '"+ booksvars +"' and ttlinit_ajyfn = '"+ booksvars2 + "'"
lds.SetFilter(ls_filter);
rtn = lds.Filter()
ll_rows = lds.RowCount()
hpb_1.SetRange (1, 100)
hpb_1.maxPosition = ll_rows
			
// Modify the coauthors and the narrators in the string array
lstr_pms_data_extract = wf_coauth_narr(lds)
			
ls_msg = "Spanish Fiction"
OpenWithParm(w_pics_retrieve_msg_box,ls_msg)
			
FOR ll_count=1 TO ll_rows
	IF lstr_pms_data_extract[ll_count].live = TRUE THEN
		FileWrite(li_filenum_xml,'<book>')
		ls_return_xml = f_export_to_xml(lstr_pms_data_extract[ll_count])
		FileWrite(li_filenum_xml,ls_return_xml)
		ll_rec++
//						// 01/15/2009
//ls_write = lstr_pms_data_extract[ll_count].bkmed + ','+ string(lstr_pms_data_extract[ll_count].bkseq)  +',' +  lstr_pms_data_extract[ll_count].chno
//				FileWrite(li_FileNum, ls_write)
//				// 01/15/2009
				
		FileWrite(li_filenum_xml,'</book>')
	END IF
	hpb_1.StepIt ()
NEXT
			
// remove the filter for next PMSUB Code
lds.SetFilter("");
lds.Filter()
Close(w_pics_retrieve_msg_box)
FileWrite(li_filenum_xml,'</Fiction>~n')

FileWrite(li_filenum_xml,'<Bilingual>~n')
FileWrite(li_filenum_xml,'<Category>Bilingual</Category>~n')
//Spanish Bilingual
Yield () 
		
lds.SetTransObject(SqlServerTrans)
booksvars = "E/S";
ls_filter = "ttlinit_lang = '"+ booksvars +"'"
lds.SetFilter(ls_filter);
rtn = lds.Filter()
ll_rows = lds.RowCount()
IF ll_rows = 0 THEN
//	GOTO NoBilingaul
FileWrite(li_filenum_xml,'</Bilingual>~n')
FileWrite(li_filenum_xml,'</Spanish>~n')

//
FileWrite(li_filenum_xml,'</BooksExports>~n')
FileClose(li_filenum_xml)

//01/15/2009
//fileclose(li_filenum)
			
hpb_1.visible =FALSE
		
Close(w_pics_retrieve_msg_box)
	
SetPointer(arrow!)

// Destroy the datastore 

DESTROY lds

RETURN ll_rec
END IF

hpb_1.SetRange (1, 100)
hpb_1.maxPosition = ll_rows
			
// Modify the coauthors and the narrators in the string array
lstr_pms_data_extract = wf_coauth_narr(lds)
			
ls_msg = "Spanish Bilingual"
OpenWithParm(w_pics_retrieve_msg_box,ls_msg)
			
FOR ll_count=1 TO ll_rows
	IF lstr_pms_data_extract[ll_count].live = TRUE THEN
		FileWrite(li_filenum_xml,'<book>')
		ls_return_xml = f_export_to_xml(lstr_pms_data_extract[ll_count])
		FileWrite(li_filenum_xml,ls_return_xml)
		ll_rec++
//						// 01/15/2009
//ls_write = lstr_pms_data_extract[ll_count].bkmed + ','+ string(lstr_pms_data_extract[ll_count].bkseq)  +',' +  lstr_pms_data_extract[ll_count].chno
//				FileWrite(li_FileNum, ls_write)
				// 01/15/2009
		FileWrite(li_filenum_xml,'</book>')
	END IF
	hpb_1.StepIt ()
NEXT
			
// remove the filter for next PMSUB Code
lds.SetFilter("");
lds.Filter()
Close(w_pics_retrieve_msg_box)

//NoBilingaul:
FileWrite(li_filenum_xml,'</Bilingual>~n')
FileWrite(li_filenum_xml,'</Spanish>~n')

//
FileWrite(li_filenum_xml,'</BooksExports>~n')
FileClose(li_filenum_xml)

//01/15/2009
//fileclose(li_filenum)
			
hpb_1.visible =FALSE
		
Close(w_pics_retrieve_msg_box)
	
SetPointer(arrow!)

// Destroy the datastore 

DESTROY lds

RETURN ll_rec
end function

public function integer wf_create_braille_catalog (date ad_start, date ad_end, integer li_filenum_xml);//////////////////////////////////////////////////////////////////////////////////////////////////////////
//
//	Function:  wf_create_cassette_catalog
//
//	Description:
//	Set menu items
//
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//	
//	Revision History
//
//	Developed by 	Date 				Version						Tracking#
//									
// Murali K.			01/31/2008      002 PICS Modifications	 PICS PMS 2.0 Modifications
//																			PMS B.2, PMS B.2.1
//																			array argument for book media
//						04/07/2009 										label certain categories depending on section
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//
n_ds lds,lds2
Integer rtn,i
String ls_current_chno, ls_narrstr, ls_coauthstr, ls_co, ls_narr, ls_return, booksvars,  booksvars2, ls_filter
String ls_return_xml, ls_pmsub_code, ls_pmsub_desc, ls_xml_tag, ls_chnos[], ls_msg, ls_cat,ls_pmsub_desc2
Long ll_rows, ll_rows2, ll_count, ll_chno_match, ll_rec
String ls_filename_xml, ls_path_xml, ls_lang1, ls_lang2,newsort
str_pms_data_extract lstr_pms_data_extract[]
Long ll_nomatch=0

// Create and load the datastore

lds = CREATE n_ds
lds.dataObject = "d_pms_data_extract"

lds2 = CREATE n_ds
lds2.dataObject = "d_pmsub_desc_extract"

ll_rows = lds.SetTransObject(SqlServerTrans)
//ll_rows = lds.Retrieve(ad_start,ad_end,"BR")
// 01/31/2008 array arguments
String ls_arg[]
ls_arg[1] = 'BR'
ll_rows = lds.retrieve(ad_start,ad_end,ls_arg[])


// Get all the chart numbers
FOR i = 1 TO ll_rows
	ls_chnos [i] = lds.object.chno[i]
NEXT

ll_rows2 = lds2.SetTransObject(SqlServerTrans)
ll_rows2 = lds2.Retrieve("BR",ad_start,ad_end)

SetPointer (hourglass!)

IF ll_rows = -1  OR ll_rows2 = -1 THEN
	Messagebox("Database Error","There was an error during the retrieve.  Please try again.")
	FileClose(li_filenum_xml)
	SetPointer(arrow!)
	RETURN 0
END IF

IF ll_rows = 0  OR ll_rows2 = -1 THEN
	Messagebox("No records found","Please adjust your date range and try again.")
	FileClose(li_filenum_xml)
	SetPointer(arrow!)
	RETURN 0
END IF

// Setup the horizontal progress bar
hpb_1.SetRange (1, 100)
hpb_1.maxPosition = ll_rows2
hpb_1.visible = TRUE

FileWrite(li_filenum_xml,'<?xml version="1.0" encoding="UTF-8"?>~n')
//FileWrite(li_filenum_xml,'<!DOCTYPE BooksExports SYSTEM "C:\picsorcl9i\braillebook.dtd">~n')
FileWrite(li_filenum_xml,'<BooksExports xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">~n')
FileWrite(li_filenum_xml,'<Nonfiction>~n')
FileWrite(li_filenum_xml,'<Heading>Nonfiction</Heading>~n')

FOR i = 1 TO ll_rows2
		Yield () 
		ls_pmsub_code = lds2.object.pmsub_code[i]
		
		lds.SetTransObject(SqlServerTrans)
		booksvars = "AN";
		booksvars2 = "YN";
		ls_filter = "(ttlinit_ajyfn = '"+ booksvars +"' or  ttlinit_ajyfn = '"+ booksvars2 +"') and (ttlinit_pmsub1 = '"+ ls_pmsub_code+ "' or ttlinit_pmsub2 = '"+ ls_pmsub_code+ "' or ttlinit_pmsub3 = '"+ ls_pmsub_code+ "')"
		
		lds.SetFilter(ls_filter);
		rtn = lds.Filter()
		IF rtn = 1 THEN
			newsort = "ttlinit_ttl A"
			lds.SetSort(newsort)
			lds.Sort()			
			ll_rows = lds.RowCount()
			IF ll_rows > 0 THEN
//				MessageBox("rows after filter",ls_filter+" "+string(ll_rows))
//				rtn = MessageBox("return","return?",question!,yesno!,1)
//				if rtn = 1 then
//					return 0
//				end if
				ls_pmsub_desc = f_remove_all_spaces(lds2.object.pmsub_desc[i])
				IF ls_pmsub_desc = 'Arts,The' THEN
					ls_pmsub_desc = 'TheArts'
				END IF
				ls_xml_tag = '<'+ls_pmsub_desc+'>~n'
				FileWrite(li_filenum_xml,ls_xml_tag)
				ls_pmsub_desc2 = lds2.object.pmsub_desc[i]
				ls_pmsub_desc2 = of_labelcategory(ls_pmsub_code,'N',ls_pmsub_desc2) // 04/07/2009 label certain categories depending on section
				ls_cat = '<Category>'+ls_pmsub_desc2+'</Category>'
				FileWrite(li_filenum_xml,ls_cat)
				// Copy the rows to a structure so we can work with it
				lstr_pms_data_extract[] = lds.object.Data
			ELSE
//				GOTO NextPMSUBAN
				CONTINUE
			END IF
		ELSE
			Messagebox("error","results of filtering ttlinit_pmsub1 failed")
			FileClose(li_filenum_xml)
			RETURN 0
		END IF
			
		// Modify the coauthors and the narrators in the string array
		lstr_pms_data_extract = wf_coauth_narr(lds)
			
		ls_msg = "Braille Nonfiction PM Subj code of: "+ls_pmsub_desc
		OpenWithParm(w_pics_retrieve_msg_box,ls_msg)
			
		FOR ll_count=1 TO ll_rows
			IF lstr_pms_data_extract[ll_count].live = TRUE THEN
				FileWrite(li_filenum_xml,'<book>')
				ls_return_xml = f_export_to_xml(lstr_pms_data_extract[ll_count])
				FileWrite(li_filenum_xml,ls_return_xml)
				ll_rec++
				FileWrite(li_filenum_xml,'</book>')
			END IF
		NEXT
			
		ls_xml_tag = '</'+ls_pmsub_desc+'>~n'
		FileWrite(li_filenum_xml,ls_xml_tag)
		// remove the filter for next PMSUB Code
		lds.SetFilter("");
		lds.Filter()
		Close(w_pics_retrieve_msg_box)
		hpb_1.StepIt ()
//		NextPMSUBAN: 
NEXT
FileWrite(li_filenum_xml,'</Nonfiction>~n')

hpb_1.SetRange (1, 100)
hpb_1.maxPosition = ll_rows2
	
FileWrite(li_filenum_xml,'<Fiction>~n')
FileWrite(li_filenum_xml,'<Heading>Fiction</Heading>~n')
//Braille Fiction Books
FOR i = 1 TO ll_rows2
		Yield () 
		ls_pmsub_code = lds2.object.pmsub_code[i]
		
		lds.SetTransObject(SqlServerTrans)
		booksvars = "AF";
		booksvars2 = "YF";
		ls_filter = "(ttlinit_ajyfn = '"+ booksvars +"' or  ttlinit_ajyfn = '"+ booksvars2 +"') and (ttlinit_pmsub1 = '"+ ls_pmsub_code+ "' or ttlinit_pmsub2 = '"+ ls_pmsub_code+ "' or ttlinit_pmsub3 = '"+ ls_pmsub_code+ "')"
		
		lds.SetFilter(ls_filter);
		rtn = lds.Filter()
		IF rtn = 1 THEN
			newsort = "ttlinit_ttl A"
			lds.SetSort(newsort)
			lds.Sort()			
			ll_rows = lds.RowCount()
			IF ll_rows > 0 THEN
				ls_pmsub_desc = f_remove_all_spaces(lds2.object.pmsub_desc[i])
				IF ls_pmsub_desc = 'Arts,The' THEN
					ls_pmsub_desc = 'TheArts'
				END IF
				ls_xml_tag = '<'+ls_pmsub_desc+'>~n'
				FileWrite(li_filenum_xml,ls_xml_tag)
				ls_pmsub_desc2 = lds2.object.pmsub_desc[i]
				ls_pmsub_desc2 = of_labelcategory(ls_pmsub_code,'F',ls_pmsub_desc2) // 04/07/2009 label certain categories depending on section
				ls_cat = '<Category>'+ls_pmsub_desc2+'</Category>'
				FileWrite(li_filenum_xml,ls_cat)
				// Copy the rows to a structure so we can work with it
				lstr_pms_data_extract[] = lds.object.Data
			ELSE
	//			GOTO NextPMSUBAF
				CONTINUE
			END IF
		ELSE
			Messagebox("error","results of filtering ttlinit_pmsub1 failed")
			FileClose(li_filenum_xml)
			RETURN 0
		END IF
			
		// Modify the coauthors and the narrators in the string array
		lstr_pms_data_extract = wf_coauth_narr(lds)
			
		ls_msg = "Braille Fiction PM Subj code of: "+ls_pmsub_desc
		OpenWithParm(w_pics_retrieve_msg_box,ls_msg)
			
		FOR ll_count=1 TO ll_rows
			IF lstr_pms_data_extract[ll_count].live = TRUE THEN
				FileWrite(li_filenum_xml,'<book>')
				ls_return_xml = f_export_to_xml(lstr_pms_data_extract[ll_count])
				FileWrite(li_filenum_xml,ls_return_xml)
				ll_rec++
				FileWrite(li_filenum_xml,'</book>')
			END IF
		NEXT
			
		ls_xml_tag = '</'+ls_pmsub_desc+'>~n'
		FileWrite(li_filenum_xml,ls_xml_tag)
		// remove the filter for next PMSUB Code
		lds.SetFilter("");
		lds.Filter()
		Close(w_pics_retrieve_msg_box)
		hpb_1.StepIt ()
//		NextPMSUBAF: 
NEXT


FileWrite(li_filenum_xml,'</Fiction>~n')


// Young Adults
Filewrite(li_filenum_xml,'<YoungAdults>~n')
FileWrite(li_filenum_xml,'<Heading>Young Adults</Heading>~n') // 04/15/09 space
FileWrite(li_filenum_xml,'<Nonfiction>~n')
FileWrite(li_filenum_xml,'<Category>Nonfiction</Category>~n')

Yield () 
		
lds.SetTransObject(SqlServerTrans)
booksvars = "YN";
ls_filter = "ttlinit_ajyfn = '"+ booksvars +"' and (ttlinit_pmsub1 = 'YOUN' or ttlinit_pmsub2 = 'YOUN' or ttlinit_pmsub3 = 'YOUN')"
lds.SetFilter(ls_filter);
rtn = lds.Filter()
ll_rows = lds.RowCount()
hpb_1.SetRange (1, 100)
hpb_1.maxPosition = ll_rows
			
// Modify the coauthors and the narrators in the string array
lstr_pms_data_extract = wf_coauth_narr(lds)
			
ls_msg = "Young Adult Nonfiction"
OpenWithParm(w_pics_retrieve_msg_box,ls_msg)
			
FOR ll_count=1 TO ll_rows
	IF lstr_pms_data_extract[ll_count].live = TRUE THEN
		FileWrite(li_filenum_xml,'<book>')
		ls_return_xml = f_export_to_xml(lstr_pms_data_extract[ll_count])
		FileWrite(li_filenum_xml,ls_return_xml)
		ll_rec++
		FileWrite(li_filenum_xml,'</book>')
	END IF
	hpb_1.StepIt ()
NEXT
			
// remove the filter for next PMSUB Code
lds.SetFilter("");
lds.Filter()
Close(w_pics_retrieve_msg_box)
FileWrite(li_filenum_xml,'</Nonfiction>~n')

FileWrite(li_filenum_xml,'<Fiction>~n')
FileWrite(li_filenum_xml,'<Category>Fiction</Category>~n')
//Young Adult Fiction Books
Yield () 
		
lds.SetTransObject(SqlServerTrans)
booksvars = "YF";
ls_filter = "ttlinit_ajyfn = '"+ booksvars +"'"
lds.SetFilter(ls_filter);
rtn = lds.Filter()
ll_rows = lds.RowCount()
hpb_1.SetRange (1, 100)
hpb_1.maxPosition = ll_rows
			
// Modify the coauthors and the narrators in the string array
lstr_pms_data_extract = wf_coauth_narr(lds)
			
ls_msg = "Young Adult Fiction"
OpenWithParm(w_pics_retrieve_msg_box,ls_msg)
			
FOR ll_count=1 TO ll_rows
	IF lstr_pms_data_extract[ll_count].live = TRUE THEN
		FileWrite(li_filenum_xml,'<book>')
		ls_return_xml = f_export_to_xml(lstr_pms_data_extract[ll_count])
		FileWrite(li_filenum_xml,ls_return_xml)
		ll_rec++
		FileWrite(li_filenum_xml,'</book>')
	END IF
	hpb_1.StepIt ()
NEXT
			
// remove the filter for next PMSUB Code
lds.SetFilter("");
lds.Filter()
Close(w_pics_retrieve_msg_box)


FileWrite(li_filenum_xml,'</Fiction>~n')
Filewrite(li_filenum_xml,'</YoungAdults>~n')

//Spanish
FileWrite(li_filenum_xml,'<Spanish>~n')
FileWrite(li_filenum_xml,'<Heading>Spanish</Heading>~n')
FileWrite(li_filenum_xml,'<Nonfiction>~n')
FileWrite(li_filenum_xml,'<Category>Nonfiction</Category>~n')
//Spanish Adult Nonfiction Books
Yield () 
		
lds.SetTransObject(SqlServerTrans)
booksvars = "SPA";
booksvars2 = "AN";
ls_filter = "ttlinit_lang = '"+ booksvars +"' and ttlinit_ajyfn = '"+ booksvars2 + "'"
lds.SetFilter(ls_filter);
rtn = lds.Filter()
newsort = "ttlinit_ttl A"
lds.SetSort(newsort)
lds.Sort()			
ll_rows = lds.RowCount()
hpb_1.SetRange (1, 100)
hpb_1.maxPosition = ll_rows
			
// Modify the coauthors and the narrators in the string array
lstr_pms_data_extract = wf_coauth_narr(lds)
			
ls_msg = "Spanish Nonfiction"
OpenWithParm(w_pics_retrieve_msg_box,ls_msg)
			
FOR ll_count=1 TO ll_rows
	IF lstr_pms_data_extract[ll_count].live = TRUE THEN
		FileWrite(li_filenum_xml,'<book>')
		ls_return_xml = f_export_to_xml(lstr_pms_data_extract[ll_count])
		FileWrite(li_filenum_xml,ls_return_xml)
		ll_rec++
		FileWrite(li_filenum_xml,'</book>')
	END IF
	hpb_1.StepIt ()
NEXT
			
// remove the filter for next PMSUB Code
lds.SetFilter("");
lds.Filter()
Close(w_pics_retrieve_msg_box)
FileWrite(li_filenum_xml,'</Nonfiction>~n')
FileWrite(li_filenum_xml,'<Fiction>~n')
FileWrite(li_filenum_xml,'<Category>Fiction</Category>~n')
//Spanish Fiction Books
Yield () 
		
lds.SetTransObject(SqlServerTrans)
booksvars = "SPA";
booksvars2 = "AF";
ls_filter = "ttlinit_lang = '"+ booksvars +"' and ttlinit_ajyfn = '"+ booksvars2 + "'"
lds.SetFilter(ls_filter);
rtn = lds.Filter()
newsort = "ttlinit_ttl A"
lds.SetSort(newsort)
lds.Sort()			
ll_rows = lds.RowCount()
hpb_1.SetRange (1, 100)
hpb_1.maxPosition = ll_rows
			
// Modify the coauthors and the narrators in the string array
lstr_pms_data_extract = wf_coauth_narr(lds)
			
ls_msg = "Spanish Fiction"
OpenWithParm(w_pics_retrieve_msg_box,ls_msg)
			
FOR ll_count=1 TO ll_rows
	IF lstr_pms_data_extract[ll_count].live = TRUE THEN
		FileWrite(li_filenum_xml,'<book>')
		ls_return_xml = f_export_to_xml(lstr_pms_data_extract[ll_count])
		FileWrite(li_filenum_xml,ls_return_xml)
		ll_rec++
		FileWrite(li_filenum_xml,'</book>')
	END IF
	hpb_1.StepIt ()
NEXT
			
// remove the filter for next PMSUB Code
lds.SetFilter("");
lds.Filter()
Close(w_pics_retrieve_msg_box)
FileWrite(li_filenum_xml,'</Fiction>~n')

FileWrite(li_filenum_xml,'<Bilingual>~n')
FileWrite(li_filenum_xml,'<Category>Bilingual</Category>~n')
//Spanish Bilingual
Yield () 
		
lds.SetTransObject(SqlServerTrans)
booksvars = "E/S";
ls_filter = "ttlinit_lang = '"+ booksvars +"'"
lds.SetFilter(ls_filter);
rtn = lds.Filter()
newsort = "ttlinit_ttl A"
lds.SetSort(newsort)
lds.Sort()			
ll_rows = lds.RowCount()
IF ll_rows = 0 THEN
	//	GOTO NoBilingaul
	FileWrite(li_filenum_xml,'</Bilingual>~n')
	FileWrite(li_filenum_xml,'</Spanish>~n')
	
	
	FileWrite(li_filenum_xml,'<UncontractedBraille>~n')
	FileWrite(li_filenum_xml,'<Heading>Uncontracted Braille</Heading>~n')
	
	//01/26/2009
	//FileWrite(li_filenum_xml,'<ContractedBraille>~n')
	//FileWrite(li_filenum_xml,'<Heading>Contracted Braille</Heading>~n')
	
	
	//Grade 1 Braille Books changed to contracted braille 01/26/2009
	Yield () 
			
	lds.SetTransObject(SqlServerTrans)
	booksvars = "Y";
	ls_filter = "g1br = '"+ booksvars +"'"
	lds.SetFilter(ls_filter);
	rtn = lds.Filter()
	newsort = "ttlinit_ttl A"
	lds.SetSort(newsort)
	lds.Sort()			
	ll_rows = lds.RowCount()
	hpb_1.SetRange (1, 100)
	hpb_1.maxPosition = ll_rows
				
	// Modify the coauthors and the narrators in the string array
	lstr_pms_data_extract = wf_coauth_narr(lds)
				
	ls_msg = "Uncontracted Braille  Books"
	//ls_msg = "Contracted Braille Books"
	OpenWithParm(w_pics_retrieve_msg_box,ls_msg)
				
	FOR ll_count=1 TO ll_rows
		IF lstr_pms_data_extract[ll_count].live = TRUE THEN
			FileWrite(li_filenum_xml,'<book>')
			ls_return_xml = f_export_to_xml(lstr_pms_data_extract[ll_count])
			FileWrite(li_filenum_xml,ls_return_xml)
			ll_rec++
			FileWrite(li_filenum_xml,'</book>')
		END IF
		hpb_1.StepIt ()
	NEXT
				
	// remove the filter for next PMSUB Code
	lds.SetFilter("");
	lds.Filter()
	Close(w_pics_retrieve_msg_box)
	FileWrite(li_filenum_xml,'</UncontractedBraille>~n')
	//FileWrite(li_filenum_xml,'</ContractedBraille>~n')
	FileWrite(li_filenum_xml,'</BooksExports>~n')
	FileClose(li_filenum_xml)
				
	hpb_1.visible =FALSE
			
	Close(w_pics_retrieve_msg_box)
		
	SetPointer(arrow!)
	
	// Destroy the datastore 
	
	DESTROY lds
	
	RETURN ll_rec
END IF

hpb_1.SetRange (1, 100)
hpb_1.maxPosition = ll_rows
			
// Modify the coauthors and the narrators in the string array
lstr_pms_data_extract = wf_coauth_narr(lds)
			
ls_msg = "Spanish Bilingual"
OpenWithParm(w_pics_retrieve_msg_box,ls_msg)
			
FOR ll_count=1 TO ll_rows
	IF lstr_pms_data_extract[ll_count].live = TRUE THEN
		FileWrite(li_filenum_xml,'<book>')
		ls_return_xml = f_export_to_xml(lstr_pms_data_extract[ll_count])
		FileWrite(li_filenum_xml,ls_return_xml)
		ll_rec++
		FileWrite(li_filenum_xml,'</book>')
	END IF
	hpb_1.StepIt ()
NEXT
			
// remove the filter for next PMSUB Code
lds.SetFilter("");
lds.Filter()
Close(w_pics_retrieve_msg_box)

//NoBilingaul:
FileWrite(li_filenum_xml,'</Bilingual>~n')
FileWrite(li_filenum_xml,'</Spanish>~n')


FileWrite(li_filenum_xml,'<UncontractedBraille>~n')
FileWrite(li_filenum_xml,'<Heading>Uncontracted Braille</Heading>~n')

//01/26/2009
//FileWrite(li_filenum_xml,'<ContractedBraille>~n')
//FileWrite(li_filenum_xml,'<Heading>Contracted Braille</Heading>~n')


//Grade 1 Braille Books changed to contracted braille 01/26/2009
Yield () 
		
lds.SetTransObject(SqlServerTrans)
booksvars = "Y";
ls_filter = "g1br = '"+ booksvars +"'"
lds.SetFilter(ls_filter);
rtn = lds.Filter()
newsort = "ttlinit_ttl A"
lds.SetSort(newsort)
lds.Sort()			
ll_rows = lds.RowCount()
hpb_1.SetRange (1, 100)
hpb_1.maxPosition = ll_rows
			
// Modify the coauthors and the narrators in the string array
lstr_pms_data_extract = wf_coauth_narr(lds)
			
ls_msg = "Uncontracted Braille  Books"
//ls_msg = "Contracted Braille Books"
OpenWithParm(w_pics_retrieve_msg_box,ls_msg)
			
FOR ll_count=1 TO ll_rows
	IF lstr_pms_data_extract[ll_count].live = TRUE THEN
		FileWrite(li_filenum_xml,'<book>')
		ls_return_xml = f_export_to_xml(lstr_pms_data_extract[ll_count])
		FileWrite(li_filenum_xml,ls_return_xml)
		ll_rec++
		FileWrite(li_filenum_xml,'</book>')
	END IF
	hpb_1.StepIt ()
NEXT
			
// remove the filter for next PMSUB Code
lds.SetFilter("");
lds.Filter()
Close(w_pics_retrieve_msg_box)
FileWrite(li_filenum_xml,'</UncontractedBraille>~n')
//FileWrite(li_filenum_xml,'</ContractedBraille>~n')
FileWrite(li_filenum_xml,'</BooksExports>~n')
FileClose(li_filenum_xml)
			
hpb_1.visible =FALSE
		
Close(w_pics_retrieve_msg_box)
	
SetPointer(arrow!)

// Destroy the datastore 

DESTROY lds

RETURN ll_rec
end function

public function integer wf_create_abridged_document (date ad_start, date ad_end);//////////////////////////////////////////////////////////////////////////////////////////////////////////
//
//	Function:  wf_create_abridged_catalog
//
//	Description:
//	Extract abridged documents
//
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//	
//	Revision History
//
//	Developed by 	Date 				Version						Tracking#
//									
// Murali K.			01/31/2008      002 PICS Modifications	 PICS PMS 2.0 Modifications
//																			PMS B.2, PMS B.2.1
//																Introduce flash books media
//																Pass RC and DB as arguments
// Murali K.			03/10/2009		DB/RC settings for Abridged document. Remove html escaping
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//
n_ds lds
integer rtn,i,ll_len,li_filenum_wp,li_wpfile
string ls_current_chno, ls_narrstr, ls_coauthstr, ls_co, ls_narr, ls_return, ls_filename
string ls_return_wp, ls_path, ls_msg, ls_endfield
string booksvars, booksvars2, booksvars3
long ll_rows, ll_count, ll_rec
str_pms_data_extract lstr_pms_data_extract[]
blob lb_blob, lb_blob2

ls_endfield =  '~h5c~h70~h61~h72~h0d~h0a~h7d~h7b~h5c~h70~h6c~h61~h69~h6e~h20'

// Get the filename
ls_filename = string(ad_end,"mmddyyyy") +'RC_Abridged'
ls_path = "c:\program files\picsorcl9i\extract\" + ls_filename
if GetFileSaveName("Select Extract File",ls_path,ls_filename, "wpd", "WPD Files (*.WPD),*.WPD") <> 1 then
	messagebox("Incorrect file name","Please try again")
	return 0
end if

li_filenum_wp = Fileopen(ls_path,streamMode!,write!,lockwrite!,Replace!)
if li_filenum_wp = -1 then
	messagebox("File Error","the file "+ls_filename+" could not be opened for output.")
	setpointer(arrow!)
	return 0
end if
// insert WP header
li_wpfile = fileopen("c:\program files\picsorcl9i\wptemp.rtf",streamMode!)
if fileread(li_wpfile, lb_blob) < 1 then
	messagebox("Template file error","Cannot open WPTEMP.RTF template file from the c:\program files\picsorcl9i folder")
	setpointer(arrow!)
	return 0
end if
ll_len = len(lb_blob)
lb_blob2 = blobmid(lb_blob,1,721)
fileclose(li_wpfile)

filewrite(li_filenum_wp,lb_blob2)
filewrite(li_filenum_wp," ")

lds = create n_ds
//lds.dataobject = "d_pms_data_extract"
lds.dataobject = "d_pms_data_extract_wp" // 03/10/2009

ll_rows = lds.settransobject(sqlservertrans)
//ll_rows = lds.retrieve(ad_start,ad_end,"RC")
// 01/31/2008
String ls_arg[]
ls_arg[1] = 'RC'
ls_arg[2] = 'DB'
Open(w_pics_retrieve_msg_box)
ll_rows = lds.retrieve(ad_start,ad_end,ls_arg[])

setpointer(hourglass!)

if ll_rows = -1 then
	close(w_pics_retrieve_msg_box)
	messagebox("Database Error","There was an error during the retrieve.  Please try again.")
	setpointer(arrow!)
	return 0
end if

if ll_rows = 0 then
	close(w_pics_retrieve_msg_box)
	messagebox("No records found","Please adjust your date range and try again.")
	setpointer(arrow!)
	return 0
end if

// perform  db/rc combination setting 03/10/2009
of_dbrc(lds) // lds reference
close(w_pics_retrieve_msg_box)

// Setup the horizontal progress bar
hpb_1.SetRange ( 1, 100 )
hpb_1.MaxPosition = ll_rows
hpb_1.visible = TRUE
OpenWithParm(w_pics_retrieve_msg_box,"Creating the document, Please Wait...")

lds.settransobject(sqlservertrans)
booksvars = "AN";
booksvars2 = "YN";
booksvars3 = "ENG";
lds.setFilter("ttlinit_ajyfn = '"+ booksvars +" ' or ttlinit_ajyfn = '"+ booksvars2+ " ' and ttlinit_lang = '"+ booksvars3 +" '");
rtn = lds.Filter()
if rtn = 1 then
	lds.SetSort("mchar_bkseq")
	lds.Sort()
	ll_rows = lds.rowcount()
	// Copy the rows to a structure so we can work with it
	IF ll_rows > 0 THEN
		lstr_pms_data_extract[] = lds.object.data
	END IF
else
	messagebox("error","results of ttlinit_ajyfn = an was zero")
end if
		
lstr_pms_data_extract = wf_coauth_narr(lds)

//Cassette-Adult Nonfiction
ls_msg = 'Cassette-Adult Nonfiction'+ls_endfield+ls_endfield
Filewrite(li_filenum_wp,ls_msg)
		
FOR ll_count=1 TO ll_rows
	if lstr_pms_data_extract[ll_count].live = true then
		ls_return_wp = f_export_abridged(lstr_pms_data_extract[ll_count])
		//lb_blob = f_blob_string(ls_return_wp)
		//Filewrite(li_filenum_wp,lb_blob)
		Filewrite(li_filenum_wp,ls_return_wp)
		ll_rec++
	end if
	hpb_1.StepIt ( )
NEXT

// remove the filter
lds.setFilter("");
lds.Filter()
		
lds.settransobject(sqlservertrans)
booksvars = "AF";
booksvars2 = "YF";
booksvars3 = "ENG";
lds.setFilter("ttlinit_ajyfn = '"+ booksvars +" ' or ttlinit_ajyfn = '"+ booksvars2+ " ' and ttlinit_lang = '"+ booksvars3 +" '");
//lds.setFilter("ttlinit_ajyfn = '"+ booksvars +" ' or ttlinit_ajyfn = '"+ booksvars2+ "'");
	
	
		
rtn = lds.Filter()
if rtn = 1 then
	lds.SetSort("mchar_bkseq")
	lds.Sort()
	ll_rows = lds.rowcount()
	// Copy the rows to a structure so we can work with it
	IF ll_rows > 0 THEN
		lstr_pms_data_extract[] = lds.object.data
	END IF
else
	messagebox("error","results of ttlinit_ajyfn = af was zero")
end if
		
lstr_pms_data_extract = wf_coauth_narr(lds)
		
//Cassette-Adult Fiction
ls_msg = 'Cassette-Adult Fiction'+ls_endfield+ls_endfield
Filewrite(li_filenum_wp,ls_msg)

FOR ll_count=1 TO ll_rows
	if lstr_pms_data_extract[ll_count].live = true then
		ls_return_wp = f_export_abridged(lstr_pms_data_extract[ll_count])
//		lb_blob = f_blob_string(ls_return_wp)
//		Filewrite(li_filenum_wp,lb_blob)
		Filewrite(li_filenum_wp,ls_return_wp)
		ll_rec++
	end if
	hpb_1.StepIt ( )
NEXT

// remove the filter
lds.setFilter("");
lds.Filter()
		
lds.settransobject(sqlservertrans)
booksvars = "JN";
booksvars2 = "ENG";
lds.setFilter("ttlinit_ajyfn = '"+ booksvars +" ' and ttlinit_lang = '"+ booksvars2 +" '");
rtn = lds.Filter()
if rtn = 1 then
	lds.SetSort("mchar_bkseq")
	lds.Sort()
	ll_rows = lds.rowcount()
	// Copy the rows to a structure so we can work with it
	IF ll_rows > 0 THEN
		lstr_pms_data_extract[] = lds.object.data
	END IF
else
	messagebox("error","results of ttlinit_ajyfn = yn was zero")
end if
		
lstr_pms_data_extract = wf_coauth_narr(lds)
		
//Cassette-Children Nonfiction
ls_msg = 'Cassette-Children Nonfiction'+ls_endfield+ls_endfield
Filewrite(li_filenum_wp,ls_msg)

FOR ll_count=1 TO ll_rows
	if lstr_pms_data_extract[ll_count].live = true then
		ls_return_wp = f_export_abridged(lstr_pms_data_extract[ll_count])
//		lb_blob = f_blob_string(ls_return_wp)
//		Filewrite(li_filenum_wp,lb_blob)
		Filewrite(li_filenum_wp,ls_return_wp)
		ll_rec++
	end if
	hpb_1.StepIt ( )
NEXT
// remove the filter
lds.setFilter("");
lds.Filter()
		
lds.settransobject(sqlservertrans)
booksvars = "JF";
booksvars2 = "ENG";
lds.setFilter("ttlinit_ajyfn = '"+ booksvars +" ' and ttlinit_lang = '"+ booksvars2 +" '");
rtn = lds.Filter()
if rtn = 1 then
	lds.SetSort("mchar_bkseq")
	lds.Sort()
	ll_rows = lds.rowcount()
	// Copy the rows to a structure so we can work with it
	IF ll_rows > 0 THEN
		lstr_pms_data_extract[] = lds.object.data
	END IF
else
	messagebox("error","results of ttlinit_ajyfn = yf was zero")
end if
		
lstr_pms_data_extract = wf_coauth_narr(lds)

//Cassette-Children Fiction
ls_msg = 'Cassette-Children Fiction'+ls_endfield+ls_endfield
Filewrite(li_filenum_wp,ls_msg)
		
FOR ll_count=1 TO ll_rows
	if lstr_pms_data_extract[ll_count].live = true then
		ls_return_wp = f_export_abridged(lstr_pms_data_extract[ll_count])
//		lb_blob = f_blob_string(ls_return_wp)
//		Filewrite(li_filenum_wp,lb_blob)
		Filewrite(li_filenum_wp,ls_return_wp)
		ll_rec++
	end if
	hpb_1.StepIt ( )
NEXT
		
// remove the filter
lds.setFilter("");
lds.Filter()
		
lds.settransobject(sqlservertrans)
booksvars = "ENG";
// Get all the foreign langauge books (not english)
lds.setFilter("ttlinit_lang <> '"+ booksvars +" '");
rtn = lds.Filter()
if rtn = 1 then
	ll_rows = lds.rowcount()
	lds.SetSort("ttlinit_lang")
	lds.Sort()
	IF ll_rows > 0 THEN
		// Copy the rows to a structure so we can work with it
		lstr_pms_data_extract[] = lds.object.data
	END IF
else
	messagebox("error","results of ttlinit_lang <> english was zero")
end if
	

IF ll_rows > 0 THEN
	
	//Foreign Language
	ls_msg = 'Foreign Language'+ls_endfield+ls_endfield
	Filewrite(li_filenum_wp,ls_msg)
	
	lstr_pms_data_extract = wf_coauth_narr(lds)
		
		
	FOR ll_count=1 TO ll_rows
		if lstr_pms_data_extract[ll_count].live = true then
			ls_return_wp = f_export_abridged(lstr_pms_data_extract[ll_count])
//			lb_blob = f_blob_string(ls_return_wp)
//			Filewrite(li_filenum_wp,lb_blob)
			Filewrite(li_filenum_wp,ls_return_wp)
			ll_rec++
		end if
		hpb_1.StepIt ( )
	NEXT
	
END IF

Fileclose(li_filenum_wp)
hpb_1.visible = FALSE
close(w_pics_retrieve_msg_box)

RETURN ll_rec
end function

public function any wf_coauth_narr_wp (datastore lds);long ll_rows
int ll_count, ll_chno_match, ll_nomatch
string ls_current_chno, ls_co, ls_narr, ls_coauthor, ls_coauthstr, ls_narrstr, ls_sep_auth
str_pms_data_extract lstr_pms_data_extract[]
Any    la_returnValue

ls_sep_auth = '[COAUTHOR]'

ll_rows = lds.rowcount()

IF ll_rows > 0 THEN
	// Copy the rows to a structure so we can work with it
	
	lstr_pms_data_extract[] = lds.object.data
	
	// Pass one, combine the duplicates
	// Go through the array, and find rows with duplicate chno
	// When we find them, go to the original matching row, and set
	// narrstr to "Various Narrators" or coauthstr to "and others"
	// depending on what matched, and then mark the second as dead.
	
		
	ls_current_chno = "none"
	ll_chno_match = 1
	FOR ll_count=1 TO ll_rows
		
		// trim saves alot of problems...
			
		lstr_pms_data_extract[ll_count].bkmed = trim(lstr_pms_data_extract[ll_count].bkmed)
			
		if (lstr_pms_data_extract[ll_count].chno = ls_current_chno) then
			// it's a match so get to work
				
			// mark it as dead because we don't want it returned in the query
			lstr_pms_data_extract[ll_count].live = false
	
				
			ls_co = trim(lstr_pms_data_extract[ll_count].coauthfn) + ls_sep_auth + trim(lstr_pms_data_extract[ll_count].coauth)
			ls_narr = trim(lstr_pms_data_extract[ll_count].narrfn) + ' ' + trim(lstr_pms_data_extract[ll_count].narr)
				
			// Is ls_co different from ls_coauthorstr?  If so, fix the original coauthstr
			if ls_co <> ls_coauthstr then
				lstr_pms_data_extract[ll_chno_match].coauthstr = "others"
			end if
				
			// Is ls_narr different from ls_narrstr?  If so, fix the original narrstr
			// set it to null if braille, or arious authors if recorded
			if ls_narr <> ls_narrstr then
				if lstr_pms_data_extract[ll_chno_match].bkmed = "RC" then
					lstr_pms_data_extract[ll_chno_match].narrstr = "Various Narrators"
				else
					setnull(lstr_pms_data_extract[ll_chno_match].narrstr)
				end if
			end if
		else 
			// Not a match so keep it live and record the information
			// in case the next one's a match
			lstr_pms_data_extract[ll_count].live = true
			ls_current_chno = lstr_pms_data_extract[ll_count].chno
			ls_coauthstr = trim(lstr_pms_data_extract[ll_count].coauthfn) + ls_sep_auth + trim(lstr_pms_data_extract[ll_count].coauth)
			ls_narrstr = trim(lstr_pms_data_extract[ll_count].narrfn) + ' ' + trim(lstr_pms_data_extract[ll_count].narr)
			lstr_pms_data_extract[ll_count].coauthstr = ls_coauthstr
			lstr_pms_data_extract[ll_count].narrstr = ls_narrstr
			ll_chno_match = ll_count
			ll_nomatch++
		end if
	NEXT

END IF
// Set return value
la_returnValue = lstr_pms_data_extract
Return la_returnValue

end function

public function integer wf_create_romance_catalog (date ad_start, date ad_end, integer li_filenum_xml, string ls_pmsub, string ls_casub, string ls_med);//////////////////////////////////////////////////////////////////////////////////////////////////////////
//
//	Function:  wf_create_cassette_catalog
//
//	Description:
//	Set menu items
//
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//	
//	Revision History
//
//	Developed by 	Date 				Version						Tracking#
//									
// Murali K.			01/31/2008      002 PICS Modifications	 PICS PMS 2.0 Modifications
//																			PMS B.2, PMS B.2.1
//																array as string argument for media
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//
n_ds lds,lds2
integer rtn,i
string ls_current_chno, ls_narrstr, ls_coauthstr, ls_co, ls_narr, ls_return, booksvars,  booksvars2, ls_filter, ls_media
string ls_return_xml, ls_authfullname,ls_authfn, ls_auth, ls_xml_tag, ls_chnos[], ls_msg, ls_cat
long ll_rows, ll_rows2, ll_count, ll_chno_match, ll_rec
string ls_filename_xml, ls_path_xml, ls_lang1, ls_lang2
str_pms_data_extract lstr_pms_data_extract[]
long ll_nomatch=0
boolean RC_EXIST = FALSE,BR_EXIST = FALSE


// Create and load the datastore

lds = create n_ds
IF ls_med = 'ALL' THEN
	lds.dataobject = "d_pms_data_extract_all_media"
	ll_rows = lds.settransobject(sqlservertrans)
	ll_rows = lds.retrieve(ad_start,ad_end)
ELSE
	lds.dataobject = "d_pms_data_extract"
	ll_rows = lds.settransobject(sqlservertrans)
//	ll_rows = lds.retrieve(ad_start,ad_end,ls_med)
	// 01/31/2008 Include DB also as media to string array as retrieval arguments
	String ls_arg[]
	ls_arg[1] = ls_med
	ll_rows = lds.Retrieve(ad_start,ad_end,ls_arg[])
	////////
END IF

IF NOT(IsNull(ls_pmsub)) AND  NOT(IsNull(ls_casub)) THEN
	ls_filter = "ttlinit_casub in ("+ls_casub+") and ( ttlinit_pmsub1 in ("+ls_pmsub+") OR ttlinit_pmsub2 in ("+ls_pmsub+") OR ttlinit_pmsub3 in ("+ls_pmsub+") )"
	lds.SetFilter(ls_filter)
	lds.Filter( )
	ll_rows = lds.RowCount()
ELSEIF NOT(IsNull(ls_pmsub)) AND  IsNull(ls_casub) THEN
	ls_filter = "ttlinit_pmsub1 in ("+ls_pmsub+") OR ttlinit_pmsub2 in ("+ls_pmsub+") OR ttlinit_pmsub3 in ("+ls_pmsub+")"
	lds.SetFilter(ls_filter)
	lds.Filter( )
	ll_rows = lds.RowCount()
ELSEIF IsNull(ls_pmsub) AND  NOT(IsNull(ls_casub)) THEN
	ls_filter = "ttlinit_casub in ("+ls_casub+")"
	lds.SetFilter(ls_filter)
	lds.Filter( )
	ll_rows = lds.RowCount()
END IF

if ll_rows = 0  then
	messagebox("No records found","Please adjust your date range or criteria and try again.")
	Fileclose(li_filenum_xml)
	setpointer(arrow!)
	return 0
end if

// Get all the chart numbers
FOR i = 1 to ll_rows
	ls_chnos [i] = lds.object.chno[i]
NEXT

lds2 = create n_ds
lds2.dataobject = "d_auth_desc"

ll_rows2 = lds2.settransobject(sqlservertrans)
ll_rows2 = lds2.retrieve(ls_chnos)

SetPointer ( HourGlass! )

if ll_rows = -1  OR ll_rows2 = -1 then
	messagebox("Database Error","There was an error during the retrieve.  Please try again.")
	Fileclose(li_filenum_xml)
	setpointer(arrow!)
	return 0
end if

if ll_rows = 0  OR ll_rows2 = -1 then
	messagebox("No records found","Please adjust your date range and try again.")
	Fileclose(li_filenum_xml)
	setpointer(arrow!)
	return 0
end if

// Setup the horizontal progress bar
hpb_1.SetRange ( 1, 100 )
hpb_1.MaxPosition = ll_rows2
hpb_1.visible = TRUE

Filewrite(li_filenum_xml,'<?xml version="1.0" encoding="UTF-8"?>~n')
IF ls_med = 'ALL' THEN
	Filewrite(li_filenum_xml,'<!DOCTYPE BooksExports SYSTEM "C:\picsorcl9i\nss_all_media.dtd">~n')
ELSEIF ls_med = 'RC' THEN // what if DB?? 01/31/2008
	Filewrite(li_filenum_xml,'<!DOCTYPE BooksExports SYSTEM "C:\picsorcl9i\nss_rc.dtd">~n')
ELSEIF ls_med = 'BR' THEN
	Filewrite(li_filenum_xml,'<!DOCTYPE BooksExports SYSTEM "C:\picsorcl9i\nss_br.dtd">~n')
END IF
Filewrite(li_filenum_xml,'<BooksExports xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">~n')
Filewrite(li_filenum_xml,'<Authors>~n')
Filewrite(li_filenum_xml,'<Heading>Authors</Heading>~n')

FOR i = 1 to ll_rows2
	YIELD()
	RC_EXIST = FALSE
	BR_EXIST = FALSE
	ls_auth = lds2.object.auth[i]
	ls_authfn = lds2.object.authfn[i]
	ls_authfullname = lds2.object.authname[i]
			
	lds.settransobject(sqlservertrans)
	
	IF ls_med = 'ALL' THEN
		// Audio
		ls_media = "RC"
		ls_filter ="mchar_bkmed = '"+ls_media+"' and ttlinit_auth = '"+ls_auth+"'  and ttlinit_authfn = '"+ls_authfn+"'"
		lds.setFilter(ls_filter)
		rtn = lds.Filter()
		if rtn = 1 then
			ll_rows = lds.rowcount()
			IF ll_rows > 0 THEN
				// Rows exist
				ls_authfullname = f_remove_all_spaces(lds2.object.authname[i])
				ls_authfullname = f_remove_non_print(lds2.object.authname[i])
				ls_authfullname = f_remove_white_spaces(lds2.object.authname[i])
				// author full name
				ls_xml_tag = '<'+ls_authfullname+'>~n'
				Filewrite(li_filenum_xml,ls_xml_tag)
				ls_cat = '<Category>'+ls_authfullname+'</Category>'
				Filewrite(li_filenum_xml,ls_cat)
				// cassette tag
				Filewrite(li_filenum_xml,'<Audio>~n')
				Filewrite(li_filenum_xml,'<Medium>Audio</Medium>~n')
				// set this flag true, so braille does not print the pmsub tag again
				RC_EXIST = TRUE
				// Copy the rows to a structure so we can work with it
				lstr_pms_data_extract[] = lds.object.data
			ELSE
				RC_EXIST = FALSE
				//3/25/2010 unsupported appeon feature
				//				goto NextMedia 
			// remove the filter for next Author
			lds.setFilter("");
			lds.Filter()
		////////////// 3/25/2010
			END IF
		else
			messagebox("error","results of filtering ttlinit_auth failed")
			Fileclose(li_filenum_xml)
			return 0
		end if
		IF RC_EXIST THEN		// 3/25/2010	
			// Modify the coauthors and the narrators in the string array
			lstr_pms_data_extract = wf_coauth_narr(lds)
				
			ls_msg = "Books for author: "+ls_authfullname
			OpenWithParm(w_pics_retrieve_msg_box,ls_msg)
			
			FOR ll_count=1 TO ll_rows
				if lstr_pms_data_extract[ll_count].live = true then
					Filewrite(li_filenum_xml,'<book>')
					ls_return_xml = f_export_to_xml(lstr_pms_data_extract[ll_count])
					Filewrite(li_filenum_xml,ls_return_xml)
					Filewrite(li_filenum_xml,'</book>')
				end if
			NEXT
			
			ll_rec++
			Filewrite(li_filenum_xml,'</Audio>~n')
		END IF // 3/25/2010				
//		NextMedia: 
				
		// remove the filter for next Author
		// THIS CODE IS NOT NECESSARY BUT LET US CHECK 3/25/2010
		lds.setFilter("");
		lds.Filter()
		/////
		
		lds.settransobject(sqlservertrans)
		//Braille
		ls_media = "BR"
		ls_filter ="mchar_bkmed = '"+ls_media+"' and ttlinit_auth = '"+ls_auth+"'  and ttlinit_authfn = '"+ls_authfn+"'"
		lds.setFilter(ls_filter)
		rtn = lds.Filter()
		if rtn = 1 then
			ll_rows = lds.rowcount()
			IF ll_rows > 0 THEN
				// If rows exist
				ls_authfullname = f_remove_all_spaces(lds2.object.authname[i])
				ls_authfullname = f_remove_non_print(lds2.object.authname[i])
				ls_authfullname = f_remove_white_spaces(lds2.object.authname[i])
				// If Audio existed, no need for auth description tag
				IF NOT(RC_EXIST) THEN
					ls_xml_tag = '<'+ls_authfullname+'>~n'
					Filewrite(li_filenum_xml,ls_xml_tag)
					ls_cat = '<Category>'+ls_authfullname+'</Category>'
					Filewrite(li_filenum_xml,ls_cat)
				END IF
				BR_EXIST = TRUE
				// Braille tag
				Filewrite(li_filenum_xml,'<Braille>~n')
				Filewrite(li_filenum_xml,'<Medium>Braille</Medium>~n')
				// Copy the rows to a structure so we can work with it
				lstr_pms_data_extract[] = lds.object.data
			ELSE
				BR_EXIST = FALSE
		//		goto NextAuth
				// code added unsupported appeon feature GOTO reworked 3/25/2010
					IF BR_EXIST OR RC_EXIST THEN
						ls_xml_tag = '</'+ls_authfullname+'>~n'
						Filewrite(li_filenum_xml,ls_xml_tag)
						ls_cat = '<Category>'+ls_authfullname+'</Category>'
						Filewrite(li_filenum_xml,ls_cat)
					END IF
							
					// remove the filter for next PMSUB Code
					lds.setFilter("");
					lds.Filter()
							
					Close(w_pics_retrieve_msg_box)
							
							
					hpb_1.StepIt ( )
				CONTINUE // NEXT AUTHOR
				//////////////////
			END IF
		else
			messagebox("error","results of filtering ttlinit_auth failed")
			Fileclose(li_filenum_xml)
			return 0
		end if
					
		// Modify the coauthors and the narrators in the string array
		lstr_pms_data_extract = wf_coauth_narr(lds)
					
		FOR ll_count=1 TO ll_rows
			if lstr_pms_data_extract[ll_count].live = true then
				Filewrite(li_filenum_xml,'<book>')
				ls_return_xml = f_export_to_xml(lstr_pms_data_extract[ll_count])
				Filewrite(li_filenum_xml,ls_return_xml)
				Filewrite(li_filenum_xml,'</book>')
			end if
		NEXT
		
		ll_rec++
		Filewrite(li_filenum_xml,'</Braille>~n')
		
	ELSEIF ls_med = 'RC' THEN
	
		// Audio
		ls_media = "RC"
		ls_filter ="mchar_bkmed = '"+ls_media+"' and ttlinit_auth = '"+ls_auth+"'  and ttlinit_authfn = '"+ls_authfn+"'"
		lds.setFilter(ls_filter)
		rtn = lds.Filter()
		if rtn = 1 then
			ll_rows = lds.rowcount()
			IF ll_rows > 0 THEN
				// Rows exist
				ls_authfullname = f_remove_all_spaces(lds2.object.authname[i])
				ls_authfullname = f_remove_non_print(lds2.object.authname[i])
				ls_authfullname = f_remove_white_spaces(lds2.object.authname[i])
				// author full name
				ls_xml_tag = '<'+ls_authfullname+'>~n'
				Filewrite(li_filenum_xml,ls_xml_tag)
				ls_cat = '<Category>'+ls_authfullname+'</Category>'
				Filewrite(li_filenum_xml,ls_cat)
				// cassette tag
				Filewrite(li_filenum_xml,'<Audio>~n')
				Filewrite(li_filenum_xml,'<Medium>Audio</Medium>~n')
				// set this flag true, so braille does not print the pmsub tag again
				RC_EXIST = TRUE
				// Copy the rows to a structure so we can work with it
				lstr_pms_data_extract[] = lds.object.data
			ELSE
				RC_EXIST = FALSE
//				goto NextAuth
				// code added unsupported appeon feature GOTO reworked 3/25/2010
					IF BR_EXIST OR RC_EXIST THEN
						ls_xml_tag = '</'+ls_authfullname+'>~n'
						Filewrite(li_filenum_xml,ls_xml_tag)
						ls_cat = '<Category>'+ls_authfullname+'</Category>'
						Filewrite(li_filenum_xml,ls_cat)
					END IF
							
					// remove the filter for next PMSUB Code
					lds.setFilter("");
					lds.Filter()
					Close(w_pics_retrieve_msg_box)
					hpb_1.StepIt ( )
					CONTINUE // NEXT AUTHOR
				//////////////////

			END IF
		else
			messagebox("error","results of filtering ttlinit_auth failed")
			Fileclose(li_filenum_xml)
			return 0
		end if
					
		// Modify the coauthors and the narrators in the string array
		lstr_pms_data_extract = wf_coauth_narr(lds)
			
		ls_msg = "Books for author: "+ls_authfullname
		OpenWithParm(w_pics_retrieve_msg_box,ls_msg)
		
		FOR ll_count=1 TO ll_rows
			if lstr_pms_data_extract[ll_count].live = true then
				Filewrite(li_filenum_xml,'<book>')
				ls_return_xml = f_export_to_xml(lstr_pms_data_extract[ll_count])
				Filewrite(li_filenum_xml,ls_return_xml)
				Filewrite(li_filenum_xml,'</book>')
			end if
		NEXT
		
		ll_rec++
		Filewrite(li_filenum_xml,'</Audio>~n')
	
	ELSEIF ls_med = 'BR' THEN
		
		//Braille
		ls_media = "BR"
		ls_filter ="mchar_bkmed = '"+ls_media+"' and ttlinit_auth = '"+ls_auth+"'  and ttlinit_authfn = '"+ls_authfn+"'"
		lds.setFilter(ls_filter)
		rtn = lds.Filter()
		if rtn = 1 then
			ll_rows = lds.rowcount()
			IF ll_rows > 0 THEN
				// If rows exist
				ls_authfullname = f_remove_all_spaces(lds2.object.authname[i])
				ls_authfullname = f_remove_non_print(lds2.object.authname[i])
				ls_authfullname = f_remove_white_spaces(lds2.object.authname[i])
				ls_xml_tag = '<'+ls_authfullname+'>~n'
				Filewrite(li_filenum_xml,ls_xml_tag)
				ls_cat = '<Category>'+ls_authfullname+'</Category>'
				Filewrite(li_filenum_xml,ls_cat)
				// Braille tag
				Filewrite(li_filenum_xml,'<Braille>~n')
				Filewrite(li_filenum_xml,'<Medium>Braille</Medium>~n')
				// Copy the rows to a structure so we can work with it
				lstr_pms_data_extract[] = lds.object.data
				BR_EXIST = TRUE
			ELSE
				BR_EXIST = FALSE
//				goto NextAuth
				// code added unsupported appeon feature GOTO reworked 3/25/2010
					IF BR_EXIST OR RC_EXIST THEN
						ls_xml_tag = '</'+ls_authfullname+'>~n'
						Filewrite(li_filenum_xml,ls_xml_tag)
						ls_cat = '<Category>'+ls_authfullname+'</Category>'
						Filewrite(li_filenum_xml,ls_cat)
					END IF
							
					// remove the filter for next PMSUB Code
					lds.setFilter("");
					lds.Filter()
					Close(w_pics_retrieve_msg_box)
					hpb_1.StepIt ( )
					CONTINUE // NEXT AUTHOR
				//////////////////

			END IF
		else
			messagebox("error","results of filtering ttlinit_auth failed")
			Fileclose(li_filenum_xml)
			return 0
		end if
					
		// Modify the coauthors and the narrators in the string array
		lstr_pms_data_extract = wf_coauth_narr(lds)
					
		FOR ll_count=1 TO ll_rows
			if lstr_pms_data_extract[ll_count].live = true then
				Filewrite(li_filenum_xml,'<book>')
				ls_return_xml = f_export_to_xml(lstr_pms_data_extract[ll_count])
				Filewrite(li_filenum_xml,ls_return_xml)
				Filewrite(li_filenum_xml,'</book>')
			end if
		NEXT
		
		ll_rec++
		Filewrite(li_filenum_xml,'</Braille>~n')
	
	END IF
		
//	NextAuth: 

	IF BR_EXIST OR RC_EXIST THEN
		ls_xml_tag = '</'+ls_authfullname+'>~n'
		Filewrite(li_filenum_xml,ls_xml_tag)
		ls_cat = '<Category>'+ls_authfullname+'</Category>'
		Filewrite(li_filenum_xml,ls_cat)
	END IF
			
	// remove the filter for next PMSUB Code
	lds.setFilter("");
	lds.Filter()
			
	Close(w_pics_retrieve_msg_box)
			
			
	hpb_1.StepIt ( )
NEXT


Filewrite(li_filenum_xml,'</Authors>~n')

Filewrite(li_filenum_xml,'</BooksExports>~n')
Fileclose(li_filenum_xml)
			
hpb_1.visible =FALSE
		
close(w_pics_retrieve_msg_box)
	
setpointer(arrow!)

// Destroy the datastore 

destroy lds

RETURN ll_rec
end function

public function integer wf_create_younger_reader_catalog (date ad_rc_start, date ad_rc_end, date ad_br_start, date ad_br_end, integer li_filenum_xml);n_ds lds,lds2,lds3
integer rtn,i,j
string ls_current_chno, ls_narrstr, ls_coauthstr, ls_co, ls_narr, ls_return, booksvars,  booksvars2, ls_filter, ls_media
string ls_return_xml, ls_pmsub_code, ls_pmsub_desc, ls_xml_tag, ls_chnos[], ls_msg, ls_cat, ls_pmsub_desc2
long ll_rows, ll_rows2, ll_count, ll_chno_match, ll_rec, ll_rc_rows, ll_br_rows,ll_tot_rows
string ls_filename_xml, ls_path_xml, ls_lang1, ls_lang2,newsort
str_pms_data_extract lstr_pms_data_extract[]
long ll_nomatch=0
boolean RC_EXIST = FALSE,BR_EXIST = FALSE
date ad_start,ad_end


// To get the range of PM SUB code we need to find the minmun and maximum date range

// Start position
IF ad_rc_start >= ad_br_start THEN
	ad_start = ad_br_start
ELSE
	ad_start = ad_rc_start
END IF
// End position
IF ad_rc_end >= ad_br_end THEN
	ad_end = ad_rc_end
ELSE
	ad_end = ad_br_end
END IF


// Create and load the datastore

lds = create n_ds
lds.dataobject = "d_pms_data_extract_rc"

lds3 = create n_ds
lds3.dataobject = "d_pms_data_extract_br"

// This datastore is the one used for cassettes catalog as well 
lds2 = create n_ds
lds2.dataobject = "d_pmsub_desc2"

ll_rc_rows = lds.settransobject(sqlservertrans)
ll_rc_rows = lds.retrieve(ad_rc_start,ad_rc_end)

ll_br_rows = lds3.settransobject(sqlservertrans)
ll_br_rows = lds3.retrieve(ad_br_start,ad_br_end)

// perform  db/rc combination setting 01/16/2009
of_dbrc(lds) // lds reference

ll_rc_rows = lds.rowcount()

// Get chart numbers for RC books
FOR i = 1 to ll_rc_rows
	ls_chnos [i] = lds.object.chno[i]
NEXT
j=i
// Now get the chart numbers for BR books
FOR i = 1 to ll_br_rows
	ls_chnos [j+i] = lds3.object.chno[i]
NEXT
j=0
i=0
//get the total rows
ll_tot_rows = ll_rc_rows + ll_br_rows

ll_rows2 = lds2.settransobject(sqlservertrans)
ll_rows2 = lds2.retrieve(ad_start,ad_end)

SetPointer ( HourGlass! )

if ll_rc_rows = -1  OR ll_br_rows = -1 OR ll_rows2 = -1 then
	messagebox("Database Error","There was an error during the retrieve.  Please try again.")
	FileClose(li_filenum_xml)
	setpointer(arrow!)
	return 0
end if

if ll_rc_rows = 0  OR ll_rows2 = -1 then
	messagebox("No records found","Please adjust your date range and try again.")
	FileClose(li_filenum_xml)
	setpointer(arrow!)
	return 0
end if

// Setup the horizontal progress bar
hpb_1.SetRange ( 1, 100 )
hpb_1.MaxPosition = ll_rows2
hpb_1.visible = TRUE

Filewrite(li_filenum_xml,'<?xml version="1.0" encoding="UTF-8"?>~n')
//Filewrite(li_filenum_xml,'<!DOCTYPE BooksExports SYSTEM "C:\picsorcl9i\youngerreaders.dtd">~n')
Filewrite(li_filenum_xml,'<BooksExports xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">~n')
// Nonfiction
Filewrite(li_filenum_xml,'<Nonfiction>~n')
Filewrite(li_filenum_xml,'<Heading>Nonfiction</Heading>~n')

FOR i = 1 to ll_rows2
	YIELD()
	RC_EXIST = FALSE
	BR_EXIST = FALSE
	ls_pmsub_code = lds2.object.pmsub_code[i]

	// P10 - XML document 01/27/2009
	CHOOSE CASE Trim(ls_pmsub_code)
		CASE 'GRAD','VERP','VERY'
			CONTINUE
	END CHOOSE
			
	lds.settransobject(sqlservertrans)
	// Young readers non fiction
	booksvars = "JN"
	// Digitalaudio
	ls_media = "RC"
	ls_filter = "ttlinit_ajyfn = '"+ booksvars +"' and (ttlinit_pmsub1 = '"+ ls_pmsub_code+ "' or ttlinit_pmsub2 = '"+ ls_pmsub_code+ "' or ttlinit_pmsub3 = '"+ ls_pmsub_code+ "')"
	lds.setFilter(ls_filter)
	rtn = lds.Filter()
	if rtn = 1 then
		newsort = "ttlinit_ttl A"
		lds.SetSort(newsort)
		lds.Sort()			
		ll_rows = lds.rowcount()
		IF ll_rows > 0 THEN
			// Rows exist
			ls_pmsub_desc = f_remove_all_spaces(lds2.object.pmsub_desc[i])
			IF ls_pmsub_desc = 'Arts,The' THEN
				ls_pmsub_desc = 'TheArts'
			END IF
			// pmsub tag
			ls_xml_tag = '<'+ls_pmsub_desc+'>~n'
			Filewrite(li_filenum_xml,ls_xml_tag)
			ls_pmsub_desc2 = lds2.object.pmsub_desc[i]
			ls_pmsub_desc2 = of_labelcategory(ls_pmsub_code,'N',ls_pmsub_desc2) // 04/07/2009 label certain categories depending on section
			ls_cat = '<Category>'+ls_pmsub_desc2+'</Category>'
			Filewrite(li_filenum_xml,ls_cat)
			// cassette tag
			Filewrite(li_filenum_xml,'<Audio>~n') // 04/07/09 all cassettes to Audio
			Filewrite(li_filenum_xml,'<Medium>Audio</Medium>~n')
			// set this flag true, so braille does not print the pmsub tag again
			RC_EXIST = TRUE
			// Copy the rows to a structure so we can work with it
			lstr_pms_data_extract[] = lds.object.data
		ELSE
			RC_EXIST = FALSE
//			goto NextMedia // unsupported appeon feature GOTO removed 3/25/2010
		END IF
	else
		messagebox("error","results of filtering ttlinit_pmsub1 failed")
		FileClose(li_filenum_xml)
		return 0
	end if
	
	IF RC_EXIST THEN // 3/25/2010
		// Modify the coauthors and the narrators in the string array for RC books
		lstr_pms_data_extract = wf_coauth_narr(lds)
			
		ls_msg = "Younger Reader Nonfiction PM Subj code of: "+ls_pmsub_desc
		OpenWithParm(w_pics_retrieve_msg_box,ls_msg)
		
		FOR ll_count=1 TO ll_rows
			if lstr_pms_data_extract[ll_count].live = true then
				Filewrite(li_filenum_xml,'<book>')
				ls_return_xml = f_export_to_xml(lstr_pms_data_extract[ll_count])
				Filewrite(li_filenum_xml,ls_return_xml)
				ll_rec++
				Filewrite(li_filenum_xml,'</book>')
			end if
		NEXT
		
		Filewrite(li_filenum_xml,'</Audio>~n')
	END IF // 3/25/2010
	
//	NextMedia: 
			
	// remove the filter for next PMSUB Code
	lds.setFilter("");
	lds.Filter()
	
	lds3.settransobject(sqlservertrans)
	//Jouvinal Non Fiction
	booksvars = "JN"
	//Braille
	ls_media = "BR"
	ls_filter = "ttlinit_ajyfn = '"+ booksvars +"' and (ttlinit_pmsub1 = '"+ ls_pmsub_code+ "' or ttlinit_pmsub2 = '"+ ls_pmsub_code+ "' or ttlinit_pmsub3 = '"+ ls_pmsub_code+ "')"
	lds3.setFilter(ls_filter)
	rtn = lds3.Filter()
	if rtn = 1 then
		newsort = "ttlinit_ttl A"
		lds3.SetSort(newsort)
		lds3.Sort()			
		ll_rows = lds3.rowcount()
		IF ll_rows > 0 THEN
			// If rows exist
			ls_pmsub_desc = f_remove_all_spaces(lds2.object.pmsub_desc[i])
			IF ls_pmsub_desc = 'Arts,The' THEN
				ls_pmsub_desc = 'TheArts'
			END IF
			// If cassettes existed, no need for pmsub description tag
			IF NOT(RC_EXIST) THEN
				ls_xml_tag = '<'+ls_pmsub_desc+'>~n'
				Filewrite(li_filenum_xml,ls_xml_tag)
				ls_pmsub_desc2 = lds2.object.pmsub_desc[i]
				ls_pmsub_desc2 = of_labelcategory(ls_pmsub_code,'N',ls_pmsub_desc2) // 04/07/2009 label certain categories depending on section
				ls_cat = '<Category>'+ls_pmsub_desc2+'</Category>'
				Filewrite(li_filenum_xml,ls_cat)
			END IF
			BR_EXIST = TRUE
			// Braille tag
			Filewrite(li_filenum_xml,'<Braille>~n')
			Filewrite(li_filenum_xml,'<Medium>Braille</Medium>~n')
			// Copy the rows to a structure so we can work with it
			lstr_pms_data_extract[] = lds3.object.data
		ELSE
			BR_EXIST = FALSE
// 			goto NextPMSUBJN 3/25/2010
		END IF
	else
		messagebox("error","results of filtering ttlinit_pmsub1 failed")
		FileClose(li_filenum_xml)
		return 0
	end if
	
	IF BR_EXIST THEN // 3/25/2010
					
		// Modify the coauthors and the narrators in the string array for Braille books
		lstr_pms_data_extract = wf_coauth_narr(lds3)
					
		FOR ll_count=1 TO ll_rows
			if lstr_pms_data_extract[ll_count].live = true then
				Filewrite(li_filenum_xml,'<book>')
				ls_return_xml = f_export_to_xml(lstr_pms_data_extract[ll_count])
				Filewrite(li_filenum_xml,ls_return_xml)
				ll_rec++
				Filewrite(li_filenum_xml,'</book>')
			end if
		NEXT
		
		Filewrite(li_filenum_xml,'</Braille>~n')
	END IF // //3/25/2010
	
//	NextPMSUBJN: 

	IF BR_EXIST OR RC_EXIST THEN
		ls_xml_tag = '</'+ls_pmsub_desc+'>~n'
		Filewrite(li_filenum_xml,ls_xml_tag)
		//ls_cat = '<Category>'+ls_pmsub_desc+'</Category>'
		//Filewrite(li_filenum_xml,ls_cat)
	END IF
			
	// remove the filter for next PMSUB Code
	lds3.setFilter("");
	lds3.Filter()
			
	Close(w_pics_retrieve_msg_box)
			
			
	hpb_1.StepIt ( )
NEXT


Filewrite(li_filenum_xml,'</Nonfiction>~n')


hpb_1.SetRange ( 1, 100 )
hpb_1.MaxPosition = ll_rows2
	
// Fiction
Filewrite(li_filenum_xml,'<Fiction>~n')
Filewrite(li_filenum_xml,'<Heading>Fiction</Heading>~n')

FOR i = 1 to ll_rows2
	YIELD()
	RC_EXIST = FALSE
	BR_EXIST = FALSE
	ls_pmsub_code = lds2.object.pmsub_code[i]

	// P10 - XML document 01/27/2009
	CHOOSE CASE Trim(ls_pmsub_code)
		CASE 'GRAD','VERP','VERY'
			CONTINUE
	END CHOOSE


	lds.settransobject(sqlservertrans)
	// Young readers non fiction
	booksvars = "JF"
	// Digitalaudio
	ls_media = "RC"
	ls_filter = "ttlinit_ajyfn = '"+ booksvars +"' and (ttlinit_pmsub1 = '"+ ls_pmsub_code+ "' or ttlinit_pmsub2 = '"+ ls_pmsub_code+ "' or ttlinit_pmsub3 = '"+ ls_pmsub_code+ "')"
	lds.setFilter(ls_filter)
	rtn = lds.Filter()
	if rtn = 1 then
		newsort = "ttlinit_ttl A"
		lds.SetSort(newsort)
		lds.Sort()			
		ll_rows = lds.rowcount()
		IF ll_rows > 0 THEN
			// Rows exist
			ls_pmsub_desc = f_remove_all_spaces(lds2.object.pmsub_desc[i])
			IF ls_pmsub_desc = 'Arts,The' THEN
				ls_pmsub_desc = 'TheArts'
			END IF
			// pmsub tag
			ls_xml_tag = '<'+ls_pmsub_desc+'>~n'
			Filewrite(li_filenum_xml,ls_xml_tag)
			ls_pmsub_desc2 = lds2.object.pmsub_desc[i]
			ls_pmsub_desc2 = of_labelcategory(ls_pmsub_code,'F',ls_pmsub_desc2) // 04/07/2009 label certain categories depending on section
			ls_cat = '<Category>'+ls_pmsub_desc2+'</Category>'
			Filewrite(li_filenum_xml,ls_cat)
			// cassette tag
			Filewrite(li_filenum_xml,'<Audio>~n')
			Filewrite(li_filenum_xml,'<Medium>Audio</Medium>~n')
			// set this flag true, so braille does not print the pmsub tag again
			RC_EXIST = TRUE
			// Copy the rows to a structure so we can work with it
			lstr_pms_data_extract[] = lds.object.data
		ELSE
			RC_EXIST = FALSE
	//		goto NextMediaJF
		END IF
	else
		messagebox("error","results of filtering ttlinit_pmsub1 failed")
		FileClose(li_filenum_xml)
		return 0
	end if
	
	IF RC_EXIST THEN // 3/25/2010
		// Modify the coauthors and the narrators in the string array
		lstr_pms_data_extract = wf_coauth_narr(lds)
			
		ls_msg = "Younger Reader Fiction PM Subj code of: "+ls_pmsub_desc
		OpenWithParm(w_pics_retrieve_msg_box,ls_msg)
		
		FOR ll_count=1 TO ll_rows
			if lstr_pms_data_extract[ll_count].live = true then
				Filewrite(li_filenum_xml,'<book>')
				ls_return_xml = f_export_to_xml(lstr_pms_data_extract[ll_count])
				Filewrite(li_filenum_xml,ls_return_xml)
				ll_rec++
				Filewrite(li_filenum_xml,'</book>')
			end if
		NEXT
		
		Filewrite(li_filenum_xml,'</Audio>~n')
	END IF // 3/25/2010
	
//	NextMediaJF: 
			
	// remove the filter for next PMSUB Code
	lds.setFilter("");
	lds.Filter()
	
	lds3.settransobject(sqlservertrans)
	//Jouvinal Non Fiction
	booksvars = "JF"
	//Braille
	ls_media = "BR"
	ls_filter = "ttlinit_ajyfn = '"+ booksvars +"' and (ttlinit_pmsub1 = '"+ ls_pmsub_code+ "' or ttlinit_pmsub2 = '"+ ls_pmsub_code+ "' or ttlinit_pmsub3 = '"+ ls_pmsub_code+ "')"
	lds3.setFilter(ls_filter)
	rtn = lds3.Filter()
	if rtn = 1 then
		newsort = "ttlinit_ttl A"
		lds3.SetSort(newsort)
		lds3.Sort()			
		ll_rows = lds3.rowcount()
		IF ll_rows > 0 THEN
			// If rows exist
			ls_pmsub_desc = f_remove_all_spaces(lds2.object.pmsub_desc[i])
			IF ls_pmsub_desc = 'Arts,The' THEN
				ls_pmsub_desc = 'TheArts'
			END IF
			// If cassettes existed, no need for pmsub description tag
			IF NOT(RC_EXIST) THEN
				ls_xml_tag = '<'+ls_pmsub_desc+'>~n'
				Filewrite(li_filenum_xml,ls_xml_tag)
				ls_pmsub_desc2 = lds2.object.pmsub_desc[i]
				ls_pmsub_desc2 = of_labelcategory(ls_pmsub_code,'F',ls_pmsub_desc2) // 04/07/2009 label certain categories depending on section
				ls_cat = '<Category>'+ls_pmsub_desc2+'</Category>'
				Filewrite(li_filenum_xml,ls_cat)
			END IF
			BR_EXIST = TRUE
			// Braille tag
			Filewrite(li_filenum_xml,'<Braille>~n')
			Filewrite(li_filenum_xml,'<Medium>Braille</Medium>~n')
			// Copy the rows to a structure so we can work with it
			lstr_pms_data_extract[] = lds3.object.data
		ELSE
			BR_EXIST = FALSE
		//	goto NextPMSUBJF
		END IF
	else
		messagebox("error","results of filtering ttlinit_pmsub1 failed")
		FileClose(li_filenum_xml)
		return 0
	end if
	
	IF BR_EXIST THEN // 3/25/2010
		// Modify the coauthors and the narrators in the string array
		lstr_pms_data_extract = wf_coauth_narr(lds3)
					
		FOR ll_count=1 TO ll_rows
			if lstr_pms_data_extract[ll_count].live = true then
				Filewrite(li_filenum_xml,'<book>')
				ls_return_xml = f_export_to_xml(lstr_pms_data_extract[ll_count])
				Filewrite(li_filenum_xml,ls_return_xml)
				ll_rec++
				Filewrite(li_filenum_xml,'</book>')
			end if
		NEXT
		
		Filewrite(li_filenum_xml,'</Braille>~n')
	END IF // 3/25/2010
	
//	NextPMSUBJF: 

	IF BR_EXIST OR RC_EXIST THEN
		ls_xml_tag = '</'+ls_pmsub_desc+'>~n'
		Filewrite(li_filenum_xml,ls_xml_tag)
		//ls_cat = '<Category>'+ls_pmsub_desc+'</Category>'
		//Filewrite(li_filenum_xml,ls_cat)
	END IF
			
	// remove the filter for next PMSUB Code
	lds3.setFilter("");
	lds3.Filter()
			
	Close(w_pics_retrieve_msg_box)
			
			
	hpb_1.StepIt ( )
NEXT


Filewrite(li_filenum_xml,'</Fiction>~n')

Filewrite(li_filenum_xml,'<PRINTBRAILLE>~n')
Filewrite(li_filenum_xml,'<Heading>PRINT/BRAILLE</Heading>~n')
//Print Braille Books
Yield ( ) 
		
lds.settransobject(sqlservertrans)
ls_pmsub_code = "VERP";
ls_filter = "ttlinit_pmsub1 = '"+ ls_pmsub_code+ "' or ttlinit_pmsub2 = '"+ ls_pmsub_code+ "' or ttlinit_pmsub3 = '"+ ls_pmsub_code+ "'"
lds3.setFilter(ls_filter);
rtn = lds3.Filter()
newsort = "ttlinit_ttl A"
lds3.SetSort(newsort)
lds3.Sort()			
ll_rows = lds3.rowcount()
hpb_1.SetRange ( 1, 100 )
hpb_1.MaxPosition = ll_rows
			
// Modify the coauthors and the narrators in the string array
lstr_pms_data_extract = wf_coauth_narr(lds3)
			
ls_msg = "PRINT//BRAILLE"
OpenWithParm(w_pics_retrieve_msg_box,ls_msg)
			
FOR ll_count=1 TO ll_rows
	if lstr_pms_data_extract[ll_count].live = true then
		Filewrite(li_filenum_xml,'<book>')
		ls_return_xml = f_export_to_xml(lstr_pms_data_extract[ll_count])
		Filewrite(li_filenum_xml,ls_return_xml)
		ll_rec++
		Filewrite(li_filenum_xml,'</book>')
	end if
	hpb_1.StepIt ( )
NEXT
			
// remove the filter for next PMSUB Code
lds3.setFilter("");
lds3.Filter()
Close(w_pics_retrieve_msg_box)
Filewrite(li_filenum_xml,'</PRINTBRAILLE>~n')

Filewrite(li_filenum_xml,'<VeryYoungReaders>~n')
Filewrite(li_filenum_xml,'<Heading>Very Young Readers</Heading>~n')
//VeryYoungReaders Books
Yield ( ) 
		
lds.settransobject(sqlservertrans)
ls_pmsub_code = "VERY";
ls_filter = "ttlinit_pmsub1 = '"+ ls_pmsub_code+ "' or ttlinit_pmsub2 = '"+ ls_pmsub_code+ "' or ttlinit_pmsub3 = '"+ ls_pmsub_code+ "'"
lds.setFilter(ls_filter);
rtn = lds.Filter()
newsort = "ttlinit_ttl A"
lds.SetSort(newsort)
lds.Sort()			
ll_rows = lds.rowcount()
hpb_1.SetRange ( 1, 100 )
hpb_1.MaxPosition = ll_rows
			
// Modify the coauthors and the narrators in the string array
lstr_pms_data_extract = wf_coauth_narr(lds)
			
ls_msg = "Very Young Readers"
OpenWithParm(w_pics_retrieve_msg_box,ls_msg)
			
FOR ll_count=1 TO ll_rows
	if lstr_pms_data_extract[ll_count].live = true then
		Filewrite(li_filenum_xml,'<book>')
		ls_return_xml = f_export_to_xml(lstr_pms_data_extract[ll_count])
		Filewrite(li_filenum_xml,ls_return_xml)
		ll_rec++
		Filewrite(li_filenum_xml,'</book>')
	end if
	hpb_1.StepIt ( )
NEXT
			
// remove the filter for next PMSUB Code
lds.setFilter("");
lds.Filter()
Close(w_pics_retrieve_msg_box)
Filewrite(li_filenum_xml,'</VeryYoungReaders>~n')

/**************************************************************************************************/

Filewrite(li_filenum_xml,'<YoungAdults>~n')
Filewrite(li_filenum_xml,'<Heading>Young Adults</Heading>~n')
//YoungAdults Books

// Nonfiction
Filewrite(li_filenum_xml,'<Nonfiction>~n')
Filewrite(li_filenum_xml,'<Category>Nonfiction</Category>~n')

YIELD()
RC_EXIST = FALSE
BR_EXIST = FALSE
lds.settransobject(sqlservertrans)
ls_pmsub_code = "YOUN";
// Young readers non fiction
booksvars = "YN"
// Digitalaudio
ls_media = "RC"
ls_filter = "ttlinit_ajyfn = '"+ booksvars +"' and (ttlinit_pmsub1 = '"+ ls_pmsub_code+ "' or ttlinit_pmsub2 = '"+ ls_pmsub_code+ "' or ttlinit_pmsub3 = '"+ ls_pmsub_code+ "')"
lds.setFilter(ls_filter)
rtn = lds.Filter()
if rtn = 1 then
	newsort = "ttlinit_ttl A"
	lds.SetSort(newsort)
	lds.Sort()			
	ll_rows = lds.rowcount()
	IF ll_rows > 0 THEN
		// cassette tag
		Filewrite(li_filenum_xml,'<Audio>~n')
		Filewrite(li_filenum_xml,'<Medium>Audio</Medium>~n')
		// set this flag true, so braille does not print the pmsub tag again
		RC_EXIST = TRUE
		// Copy the rows to a structure so we can work with it
		lstr_pms_data_extract[] = lds.object.data
	ELSE
		RC_EXIST = FALSE
	//	goto NextMediaYN
	END IF
else
	messagebox("error","results of filtering ttlinit_pmsub1 failed")
	FileClose(li_filenum_xml)
	return 0
end if
IF RC_EXIST THEN // 3/25/2010				
	// Modify the coauthors and the narrators in the string array for RC books
	lstr_pms_data_extract = wf_coauth_narr(lds)
			
	ls_msg = "Young Adults Non Fiction Digitalaudio "
	OpenWithParm(w_pics_retrieve_msg_box,ls_msg)
		
	FOR ll_count=1 TO ll_rows
		if lstr_pms_data_extract[ll_count].live = true then
			Filewrite(li_filenum_xml,'<book>')
			ls_return_xml = f_export_to_xml(lstr_pms_data_extract[ll_count])
			Filewrite(li_filenum_xml,ls_return_xml)
			ll_rec++
			Filewrite(li_filenum_xml,'</book>')
		end if
	NEXT
		
	Filewrite(li_filenum_xml,'</Audio>~n')
END IF

//NextMediaYN: 
			
// remove the filter for next PMSUB Code
lds.setFilter("");
lds.Filter()
	
lds3.settransobject(sqlservertrans)
//Jouvinal Non Fiction
booksvars = "YN"
//Braille
ls_media = "BR"
ls_filter = "ttlinit_ajyfn = '"+ booksvars +"' and (ttlinit_pmsub1 = '"+ ls_pmsub_code+ "' or ttlinit_pmsub2 = '"+ ls_pmsub_code+ "' or ttlinit_pmsub3 = '"+ ls_pmsub_code+ "')"
lds3.setFilter(ls_filter)
rtn = lds3.Filter()
if rtn = 1 then
	newsort = "ttlinit_ttl A"
	lds3.SetSort(newsort)
	lds3.Sort()			
	ll_rows = lds3.rowcount()
	IF ll_rows > 0 THEN
		BR_EXIST = TRUE
		// Braille tag
		Filewrite(li_filenum_xml,'<Braille>~n')
		Filewrite(li_filenum_xml,'<Medium>Braille</Medium>~n')
		// Copy the rows to a structure so we can work with it
		lstr_pms_data_extract[] = lds3.object.data
	ELSE
		BR_EXIST = FALSE
//		goto NextPMSUBYN
	END IF
else
	messagebox("error","results of filtering ttlinit_pmsub1 failed")
	FileClose(li_filenum_xml)
	return 0
end if
				
// Modify the coauthors and the narrators in the string array for Braille books
IF BR_EXIST THEN // 3/25/2010
	lstr_pms_data_extract = wf_coauth_narr(lds3)
		
	ls_msg = "Young Adults Non Fiction Braille"
	OpenWithParm(w_pics_retrieve_msg_box,ls_msg)
					
	FOR ll_count=1 TO ll_rows
		if lstr_pms_data_extract[ll_count].live = true then
			Filewrite(li_filenum_xml,'<book>')
			ls_return_xml = f_export_to_xml(lstr_pms_data_extract[ll_count])
			Filewrite(li_filenum_xml,ls_return_xml)
			ll_rec++
			Filewrite(li_filenum_xml,'</book>')
		end if
	NEXT
		
	Filewrite(li_filenum_xml,'</Braille>~n')
END IF // 3/25/2010

//NextPMSUBYN: 

Filewrite(li_filenum_xml,'</Nonfiction>~n')

// remove the filter for next PMSUB Code
lds3.setFilter("");
lds3.Filter()
			
Close(w_pics_retrieve_msg_box)
			
hpb_1.StepIt ( )

// Fiction
Filewrite(li_filenum_xml,'<Fiction>~n')
Filewrite(li_filenum_xml,'<Category>Fiction</Category>~n')

YIELD()
RC_EXIST = FALSE
BR_EXIST = FALSE
lds.settransobject(sqlservertrans)
ls_pmsub_code = "YOUN";
// Young Adults Non Fiction
booksvars = "YF"
// Digitalaudio
ls_media = "RC"
ls_filter = "ttlinit_ajyfn = '"+ booksvars +"' and (ttlinit_pmsub1 = '"+ ls_pmsub_code+ "' or ttlinit_pmsub2 = '"+ ls_pmsub_code+ "' or ttlinit_pmsub3 = '"+ ls_pmsub_code+ "')"
lds.setFilter(ls_filter)
rtn = lds.Filter()
if rtn = 1 then
	newsort = "ttlinit_ttl A"
	lds.SetSort(newsort)
	lds.Sort()			
	ll_rows = lds.rowcount()
	IF ll_rows > 0 THEN
		// cassette tag
		Filewrite(li_filenum_xml,'<Audio>~n')
		Filewrite(li_filenum_xml,'<Medium>Audio</Medium>~n')
		// set this flag true, so braille does not print the pmsub tag again
		RC_EXIST = TRUE
		// Copy the rows to a structure so we can work with it
		lstr_pms_data_extract[] = lds.object.data
	ELSE
		RC_EXIST = FALSE
	//	goto NextMediaYF
	END IF
else
	messagebox("error","results of filtering ttlinit_pmsub1 failed")
	FileClose(li_filenum_xml)
	return 0
end if

IF RC_EXIST THEN // 3/25/2010			
	// Modify the coauthors and the narrators in the string array for RC books
	lstr_pms_data_extract = wf_coauth_narr(lds)
		
	ls_msg = "Young Adults Fiction Digitalaudio "
	OpenWithParm(w_pics_retrieve_msg_box,ls_msg)
	
	FOR ll_count=1 TO ll_rows
		if lstr_pms_data_extract[ll_count].live = true then
			Filewrite(li_filenum_xml,'<book>')
			ls_return_xml = f_export_to_xml(lstr_pms_data_extract[ll_count])
			Filewrite(li_filenum_xml,ls_return_xml)
			ll_rec++
			Filewrite(li_filenum_xml,'</book>')
		end if
	NEXT
	
	Filewrite(li_filenum_xml,'</Audio>~n')
END IF // 3/25/2010

//NextMediaYF: 
		
// remove the filter for next PMSUB Code
lds.setFilter("");
lds.Filter()

lds3.settransobject(sqlservertrans)
//Young Adults Fiction
booksvars = "YF"
//Braille
//lds3.saveas('c:\ur1.xls', excel!,true)
ls_media = "BR"
//IF LS_PMSUB_CODE = 'ROMA' then

//end if
ls_filter = "ttlinit_ajyfn = '"+ booksvars +"' and (ttlinit_pmsub1 = '"+ ls_pmsub_code+ "' or ttlinit_pmsub2 = '"+ ls_pmsub_code+ "' or ttlinit_pmsub3 = '"+ ls_pmsub_code+ "')"
lds3.setFilter(ls_filter)
rtn = lds3.Filter()

if rtn = 1 then
	newsort = "ttlinit_ttl A"
	lds3.SetSort(newsort)
	lds3.Sort()			
	ll_rows = lds3.rowcount()
	IF ll_rows > 0 THEN
		BR_EXIST = TRUE
		// Braille tag
		Filewrite(li_filenum_xml,'<Braille>~n')
		Filewrite(li_filenum_xml,'<Medium>Braille</Medium>~n')
		// Copy the rows to a structure so we can work with it
		lstr_pms_data_extract[] = lds3.object.data
	ELSE
		BR_EXIST = FALSE
//		goto NextPMSUBYF
	END IF
else
	messagebox("error","results of filtering ttlinit_pmsub1 failed")
	FileClose(li_filenum_xml)
	return 0
end if

IF BR_EXIST THEN // /3/25/2010
	
	// Modify the coauthors and the narrators in the string array for Braille books
	lstr_pms_data_extract = wf_coauth_narr(lds3)
	
	ls_msg = "Young Adults Fiction Braille "
	OpenWithParm(w_pics_retrieve_msg_box,ls_msg)
				
	FOR ll_count=1 TO ll_rows
		if lstr_pms_data_extract[ll_count].live = true then
			Filewrite(li_filenum_xml,'<book>')
			ls_return_xml = f_export_to_xml(lstr_pms_data_extract[ll_count])
			Filewrite(li_filenum_xml,ls_return_xml)
			ll_rec++
			Filewrite(li_filenum_xml,'</book>')
		end if
	NEXT
	
	Filewrite(li_filenum_xml,'</Braille>~n')
END IF // 3/25/2010

//NextPMSUBYF: 

Filewrite(li_filenum_xml,'</Fiction>~n')

// remove the filter for next PMSUB Code
lds3.setFilter("");
lds3.Filter()
		
Close(w_pics_retrieve_msg_box)
		
		
hpb_1.StepIt ( )

// remove the filter for next PMSUB Code
lds.setFilter("");
lds.Filter()
Close(w_pics_retrieve_msg_box)
Filewrite(li_filenum_xml,'</YoungAdults>~n')

/**************************************************************************************************/

Filewrite(li_filenum_xml,'<Spanish>~n')
Filewrite(li_filenum_xml,'<Heading>Spanish</Heading>~n')
//Spanish Books

// remove the filter for next PMSUB Code
lds3.setFilter("");
lds3.Filter()
lds.setFilter("");
lds.Filter()
			
Close(w_pics_retrieve_msg_box)
			
hpb_1.StepIt ( )

YIELD()
RC_EXIST = FALSE
BR_EXIST = FALSE
lds.settransobject(sqlservertrans)
ls_pmsub_code = "YOUN";
// Young Adults Spanish
booksvars = "SPA"
// Digitalaudio
ls_media = "RC"
ls_filter = "ttlinit_lang = '"+ booksvars +"' and (ttlinit_pmsub1 = '"+ ls_pmsub_code+ "' or ttlinit_pmsub2 = '"+ ls_pmsub_code+ "' or ttlinit_pmsub3 = '"+ ls_pmsub_code+ "')"
lds.setFilter(ls_filter)
rtn = lds.Filter()
if rtn = 1 then
	newsort = "ttlinit_ttl A"
	lds.SetSort(newsort)
	lds.Sort()			
	ll_rows = lds.rowcount()
	IF ll_rows > 0 THEN
		// cassette tag
		Filewrite(li_filenum_xml,'<Audio>~n')
		Filewrite(li_filenum_xml,'<Medium>Audio</Medium>~n')
		// set this flag true, so braille does not print the pmsub tag again
		RC_EXIST = TRUE
		// Copy the rows to a structure so we can work with it
		lstr_pms_data_extract[] = lds.object.data
	ELSE
		RC_EXIST = FALSE
	//	goto NextMediaSPA
	END IF
else
	messagebox("error","results of filtering ttlinit_pmsub1 failed")
	FileClose(li_filenum_xml)
	return 0
end if
IF RC_EXIST THEN // 3/25/2010			
	// Modify the coauthors and the narrators in the string array for RC books
	lstr_pms_data_extract = wf_coauth_narr(lds)
		
	ls_msg = "Young Adults Spanish Digitalaudio "
	OpenWithParm(w_pics_retrieve_msg_box,ls_msg)
	
	FOR ll_count=1 TO ll_rows
		if lstr_pms_data_extract[ll_count].live = true then
			Filewrite(li_filenum_xml,'<book>')
			ls_return_xml = f_export_to_xml(lstr_pms_data_extract[ll_count])
			Filewrite(li_filenum_xml,ls_return_xml)
			ll_rec++
			Filewrite(li_filenum_xml,'</book>')
		end if
	NEXT
	
	Filewrite(li_filenum_xml,'</Audio>~n')
END IF // 3/25/2010

//NextMediaSPA: 
		
// remove the filter for next PMSUB Code
lds.setFilter("");
lds.Filter()

lds3.settransobject(sqlservertrans)
ls_pmsub_code = "YOUN";
//Young Adults SPA
booksvars = "SPA"
//Braille
ls_media = "BR"
ls_filter = "ttlinit_lang = '"+ booksvars +"' and (ttlinit_pmsub1 = '"+ ls_pmsub_code+ "' or ttlinit_pmsub2 = '"+ ls_pmsub_code+ "' or ttlinit_pmsub3 = '"+ ls_pmsub_code+ "')"
lds3.setFilter(ls_filter)
rtn = lds3.Filter()
if rtn = 1 then
	newsort = "ttlinit_ttl A"
	lds3.SetSort(newsort)
	lds3.Sort()			
	ll_rows = lds3.rowcount()
	IF ll_rows > 0 THEN
		BR_EXIST = TRUE
		// Braille tag
		Filewrite(li_filenum_xml,'<Braille>~n')
		Filewrite(li_filenum_xml,'<Medium>Braille</Medium>~n')
		// Copy the rows to a structure so we can work with it
		lstr_pms_data_extract[] = lds3.object.data
	ELSE
		BR_EXIST = FALSE
	//	goto NextPMSUBSPA
	END IF
else
	messagebox("error","results of filtering ttlinit_pmsub1 failed")
	FileClose(li_filenum_xml)
	return 0
end if
IF BR_EXIST THEN // 3/25/2010			
	// Modify the coauthors and the narrators in the string array for Braille books
	lstr_pms_data_extract = wf_coauth_narr(lds3)
	
	ls_msg = "Young Adults Spanish Braille "
	OpenWithParm(w_pics_retrieve_msg_box,ls_msg)
				
	FOR ll_count=1 TO ll_rows
		if lstr_pms_data_extract[ll_count].live = true then
			Filewrite(li_filenum_xml,'<book>')
			ls_return_xml = f_export_to_xml(lstr_pms_data_extract[ll_count])
			Filewrite(li_filenum_xml,ls_return_xml)
			ll_rec++
			Filewrite(li_filenum_xml,'</book>')
		end if
	NEXT
	
	Filewrite(li_filenum_xml,'</Braille>~n')
END IF // 3/25/2010

//NextPMSUBSPA: 

// remove the filter for next PMSUB Code
lds3.setFilter("");
lds3.Filter()
		
Close(w_pics_retrieve_msg_box)
		
		
hpb_1.StepIt ( )

// remove the filter for next PMSUB Code
lds.setFilter("");
lds.Filter()
lds3.setFilter("");
lds3.Filter()
Close(w_pics_retrieve_msg_box)
Filewrite(li_filenum_xml,'</Spanish>~n')


Filewrite(li_filenum_xml,'</BooksExports>~n')
Fileclose(li_filenum_xml)
			
hpb_1.visible =FALSE
		
close(w_pics_retrieve_msg_box)
	
setpointer(arrow!)

// Destroy the datastore 

destroy lds

RETURN ll_rec
end function

public function integer of_dbrc (ref n_ds ads);//////////////////////////////////////////////////////////////////////////////////////////////////////////
//
//	Function:  of_dbrc
// Args:  n_ds lds - extract datastore
//	Description: If RC/DB row exist indicate DB/RC and discard one of the rows
//	
//
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//	
//	Revision History
//
//	Developed by 	Date 				Version						Tracking#
//									
// Murali K.			01/15/2009      XML catalog changes
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//

Long 			ll_rc , ll_loop, ll_bkseq, ll_current_bkseq, lcnt, ll_foundrow
string			ls_bkmed, ls_current_bkmed, ls_temp, ls_find

ll_rc = ads.Rowcount()

FOR ll_loop = 1 TO ll_rc

	   ll_current_bkseq 	= ads.object.mchar_bkseq[ll_loop]
	   ls_current_bkmed 	= ads.object.mchar_bkmed[ll_loop]
	   lcnt 						= ads.object.mcnt[ll_loop]
	
	   IF lcnt > 1 THEN // BOTH RC AND DB PRESENT
		  IF ls_current_bkmed = 'RC' THEN
			 ads.object.dbrc[ll_loop] = 'DB/RC'
			 // 12/4/200i9 #2249 For RTB pair records in WPD
			 // Set media = DB RC
 			IF  cbx_bypass.checked =FALSE THEN
				 ads.object.dbrc[ll_loop] = 'DB RC'
			 END IF
 		  END IF
		ELSEIF lcnt =1 THEN
			 ads.object.dbrc[ll_loop] = ls_current_bkmed
	   END IF
		
NEXT

// discard null dbrcs
ll_loop = 1
for ll_loop = 1 to ads.rowcount()
	if isnull(ads.object.dbrc[ll_loop]) OR &
		Len(trim(ads.object.dbrc[ll_loop])) = 0 then
		ads.deleterow(ll_loop)
		ll_loop --
	end if
next

ads.RowsDiscard(1, ads.deletedCount(), Delete!)

RETURN 1
end function

public function string of_parsecoauthors (string as_text, integer ai_count);//////////////////////////////////////////////////////////////////////////////////////////////////////////
//
//	Function:  OF_PARSECOAUTHORS
// Args: String as_text 
//	Description: Parse coauthors and replace last comma with 'and'
//	
//
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//	
//	Revision History
//
//	Developed by 	Date 				Version						Tracking#
//									
// Murali K.			01/14/2009      XML catalog changes
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//

// replace last comma with 'and'


long      ll_StartPos = 1, i
string    ls_old_str, ls_new_str, ls_txt_str

IF ai_count > 3 THEN
	RETURN as_text
END IF

ls_txt_str = as_text
ls_old_str = ","
ls_new_str = ","

// Find the first occurrence of  ls_old_str ...
ll_StartPos = Pos(ls_txt_str, ls_old_str, ll_StartPos)

// Only enter the loop if you find  ls_old_str ...

DO WHILE ll_StartPos > 0 
	i= ll_startpos
    // Find the next occurrence of  ls_old_str 
    ll_StartPos = Pos(ls_txt_str, ls_old_str, ll_StartPos + Len(ls_old_str))
	 
    // Replace old_str with  ls_new_str ...
	IF ll_startpos > 0 THEN
	    ls_txt_str = Replace(ls_txt_str, i, Len(ls_old_str), ls_new_str)
	ELSE
	    ls_txt_str = Replace(ls_txt_str, i, Len(ls_old_str), ' and ')
	END IF
	 
LOOP

return ls_txt_str
end function

public function string of_setcoauthors (string as_first[], string as_last[], string as_honor[], integer ai_match);
int li_upperbound
string ls_co

li_upperbound = Upperbound(as_last[])

IF li_upperbound < 1 THEN
	RETURN ''
END IF

IF gs_catalog <> 'WP' THEN
	CHOOSE CASE ai_match
			CASE 1
					ls_co = "<Coauthor>" + "~n"
					ls_co += "<ConnectTag>" + ' and ' + "</ConnectTag>"+  "~n"
					IF Isnull( as_first[1]) THEN
						as_first[1] = ' '
					END IF
	
					ls_co +=  "<FirstName>" +as_first[1] +  "</FirstName>" + '~n'  + &
									 "<LastName>" + as_last[1] +  "</LastName>" + '~n' + &
									  "<Honorific>" + as_honor[1] +  "</Honorific>" + '~n'
					ls_co += "</Coauthor>" + "~n"
	
			CASE    ELSE // more then 1
					ls_co = "<Coauthor>" + "~n"
					ls_co += "<ConnectTag>" + ' and others ' + "</ConnectTag>"+  "~n"
					ls_co +=  "<FirstName>" + ' '+  "</FirstName>" + '~n'  + &
									 "<LastName>" + ' ' +  "</LastName>" + '~n' + &
									  "<Honorific>" + ' '  +  "</Honorific>" + '~n'
					ls_co += "</Coauthor>" + "~n"
					
	END CHOOSE

ELSE
		// 03/10/2009 for WP creation
		CHOOSE CASE ai_match
			CASE 1
					ls_co +=  ' and ' 
					IF Isnull( as_first[1]) THEN
						as_first[1] = ' '
					END IF
	
					ls_co +=  as_first[1] +' ' +  as_last[1] +  ' ' + as_honor[1] 

			CASE    ELSE // more then 1
					ls_co +=  '  and others ' 
	END CHOOSE
END IF


RETURN ls_co
end function

public function boolean of_checkcoauthors (string as_first[], string as_last[], string as_f, string as_l);
int li_upperbound, li_loop
boolean lb_found=FALSE

li_upperbound = Upperbound(as_last)
IF li_upperbound < 1 THEN
	RETURN FALSE
END IF

FOR li_loop =  1 TO li_upperbound
	IF Trim(as_f) = Trim(as_first[li_loop]) AND Trim(as_l) = Trim(as_last[li_loop]) THEN
		lb_found = TRUE
		exit
	END IF
NEXT
RETURN lb_found


end function

public function string of_labelcategory (string as_sub_code, string as_section, string as_desc);//////////////////////////////////////////////////////////////////////////////////////////////////////////
//
//	Function:  of_labelcategory
// Args:   as_subcode - PMSUB code, as_section - Fiction or Non Fiction
//			as_desc - Original PMSUB desc
//	Description: Set category labels according to section
//	
//
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//	
//	Revision History
//
//	Developed by 	Date 				Version						Tracking#
//									
// Murali K.			04/07/2009      XML catalog changes 		2159
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//
string ls_desc

IF as_section = 'N' THEN // Non fiction
	CHOOSE CASE Trim(as_sub_code)
		CASE  'HIST'
			ls_desc = 'History'
		CASE  'GOVE'
			ls_desc = 'Government and Politics'
		CASE 'MEDI'
			ls_desc = 'Medicine and Health'
		CASE 'PSYC'
			ls_desc = 'Psychology and Self Help'
		CASE 'SCIE'
			ls_desc = 'Science and Technology'
		CASE ELSE
			ls_desc = as_desc
	END CHOOSE
ELSE 
	// fiction
	CHOOSE CASE Trim(as_sub_code)
		CASE  'HIST'
			ls_desc = 'Historical Fiction'
		CASE  'GOVE'
			ls_desc = 'Political Themes'
		CASE 'MEDI'
			ls_desc = 'Medical Themes'
		CASE 'PSYC'
			ls_desc = 'Psychological Themes'
		CASE 'SCIE'
			ls_desc = 'Science Fiction'
		CASE ELSE
			ls_desc = as_desc
	END CHOOSE
END IF

RETURN ls_desc
end function

public function integer of_setbookmedia (ref n_ds ads);//////////////////////////////////////////////////////////////////////////////////////////////////////////
//
//	Function:  of_dbrc
// Args:  n_ds lds - extract datastore
//	Description: set book media in the DBRC field.
//	
//
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//	
//	Revision History
//
//	Developed by 	Date 				Version						Tracking#
//									
// Murali K.			04/27/2009      XML catalog changes 2159
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//

Long 			ll_rc , ll_loop 
string			 ls_current_bkmed

ll_rc = ads.Rowcount()

FOR ll_loop = 1 TO ll_rc
	   ls_current_bkmed 			= ads.object.mchar_bkmed[ll_loop]
	  ads.object.dbrc[ll_loop] = ls_current_bkmed
NEXT
RETURN 1
end function

on w_pms_data_extract.create
int iCurrent
call super::create
this.rb_crc=create rb_crc
this.rb_cbr=create rb_cbr
this.rb_arc=create rb_arc
this.rb_abr=create rb_abr
this.cbx_bypass=create cbx_bypass
this.hpb_1=create hpb_1
this.rb_abridged=create rb_abridged
this.st_1=create st_1
this.st_2=create st_2
this.st_3=create st_3
this.em_start_date=create em_start_date
this.em_end_date=create em_end_date
this.gb_st_end=create gb_st_end
this.rb_recorded=create rb_recorded
this.rb_braille=create rb_braille
this.cb_extract=create cb_extract
this.cb_clear=create cb_clear
this.cb_exit=create cb_exit
this.gb_wpcat=create gb_wpcat
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.rb_crc
this.Control[iCurrent+2]=this.rb_cbr
this.Control[iCurrent+3]=this.rb_arc
this.Control[iCurrent+4]=this.rb_abr
this.Control[iCurrent+5]=this.cbx_bypass
this.Control[iCurrent+6]=this.hpb_1
this.Control[iCurrent+7]=this.rb_abridged
this.Control[iCurrent+8]=this.st_1
this.Control[iCurrent+9]=this.st_2
this.Control[iCurrent+10]=this.st_3
this.Control[iCurrent+11]=this.em_start_date
this.Control[iCurrent+12]=this.em_end_date
this.Control[iCurrent+13]=this.gb_st_end
this.Control[iCurrent+14]=this.rb_recorded
this.Control[iCurrent+15]=this.rb_braille
this.Control[iCurrent+16]=this.cb_extract
this.Control[iCurrent+17]=this.cb_clear
this.Control[iCurrent+18]=this.cb_exit
this.Control[iCurrent+19]=this.gb_wpcat
end on

on w_pms_data_extract.destroy
call super::destroy
destroy(this.rb_crc)
destroy(this.rb_cbr)
destroy(this.rb_arc)
destroy(this.rb_abr)
destroy(this.cbx_bypass)
destroy(this.hpb_1)
destroy(this.rb_abridged)
destroy(this.st_1)
destroy(this.st_2)
destroy(this.st_3)
destroy(this.em_start_date)
destroy(this.em_end_date)
destroy(this.gb_st_end)
destroy(this.rb_recorded)
destroy(this.rb_braille)
destroy(this.cb_extract)
destroy(this.cb_clear)
destroy(this.cb_exit)
destroy(this.gb_wpcat)
end on

event resize;call super::resize;THIS.x= 407
THIS.y = 609
THIS.width = 2108
THIS.height =1589
end event

event open;call super::open;em_start_date.text = string(relativedate(today(),-7))
em_end_date.text = string(today())
end event

type rb_crc from radiobutton within w_pms_data_extract
integer x = 169
integer y = 1056
integer width = 837
integer height = 80
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 33554432
long backcolor = 80263581
string text = "Childrens Recorded Books"
end type

event clicked;IF this.Checked THEN
	cbx_bypass.Checked = FALSE
END IF
end event

type rb_cbr from radiobutton within w_pms_data_extract
integer x = 169
integer y = 960
integer width = 731
integer height = 80
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 33554432
long backcolor = 80263581
string text = "Childrens Braille Books"
end type

event clicked;IF this.Checked THEN
	cbx_bypass.Checked = FALSE
END IF
end event

type rb_arc from radiobutton within w_pms_data_extract
integer x = 169
integer y = 864
integer width = 731
integer height = 80
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 33554432
long backcolor = 80263581
string text = "Adults Recorded Books"
end type

event clicked;IF this.Checked THEN
	cbx_bypass.Checked = FALSE
END IF
end event

type rb_abr from radiobutton within w_pms_data_extract
integer x = 169
integer y = 768
integer width = 645
integer height = 80
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 33554432
long backcolor = 80263581
string text = "Adults Braille Books"
end type

event clicked;IF this.Checked THEN
	cbx_bypass.Checked = FALSE
END IF
end event

type cbx_bypass from checkbox within w_pms_data_extract
integer x = 37
integer y = 1324
integer width = 1019
integer height = 80
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 33554432
long backcolor = 80263581
string text = "Bypass WordPerfect File Creation"
end type

event clicked;IF this.Checked THEN
	rb_abridged.Checked = FALSE
	rb_braille.Checked = FALSE
	rb_recorded.Checked = FALSE
END IF
end event

type hpb_1 from hprogressbar within w_pms_data_extract
boolean visible = false
integer x = 41
integer y = 1220
integer width = 1947
integer height = 68
unsignedinteger maxposition = 100
integer setstep = 1
end type

type rb_abridged from u_rb within w_pms_data_extract
integer x = 169
integer y = 488
integer width = 489
integer height = 64
integer taborder = 40
integer textsize = -10
string text = "Abridged TBT"
end type

event clicked;call super::clicked;IF this.Checked THEN
	cbx_bypass.Checked = FALSE
END IF
end event

type st_1 from u_st within w_pms_data_extract
integer x = 800
integer y = 24
integer width = 503
integer weight = 700
fontcharset fontcharset = ansi!
boolean underline = true
string text = "Extract for period"
end type

type st_2 from u_st within w_pms_data_extract
integer x = 297
integer y = 208
integer width = 279
integer textsize = -10
string text = "Start Date"
end type

type st_3 from u_st within w_pms_data_extract
integer x = 1129
integer y = 208
integer width = 274
integer textsize = -10
string text = "End Date"
end type

type em_start_date from u_em within w_pms_data_extract
integer x = 622
integer y = 192
integer width = 370
integer height = 100
integer taborder = 10
integer textsize = -10
maskdatatype maskdatatype = datemask!
string mask = "mm/dd/yyyy"
boolean autoskip = true
string displaydata = ""
double increment = 0
string minmax = ""
end type

type em_end_date from u_em within w_pms_data_extract
integer x = 1435
integer y = 192
integer width = 370
integer height = 100
integer taborder = 20
integer textsize = -10
maskdatatype maskdatatype = datemask!
string mask = "mm/dd/yyyy"
boolean autoskip = true
string displaydata = ""
double increment = 0
string minmax = ""
end type

type gb_st_end from u_gb within w_pms_data_extract
integer x = 261
integer y = 88
integer width = 1605
integer height = 280
integer taborder = 0
string text = ""
end type

type rb_recorded from u_rb within w_pms_data_extract
integer x = 169
integer y = 680
integer width = 571
integer height = 68
integer taborder = 30
integer textsize = -10
long backcolor = 79741120
string text = "Recorded Books"
end type

event clicked;call super::clicked;IF this.Checked THEN
	cbx_bypass.Checked = FALSE
END IF
end event

type rb_braille from u_rb within w_pms_data_extract
integer x = 169
integer y = 584
integer width = 462
integer height = 64
integer taborder = 40
integer textsize = -10
string text = "Braille Books"
end type

event clicked;call super::clicked;IF this.Checked THEN
	cbx_bypass.Checked = FALSE
END IF
end event

type cb_extract from u_cb within w_pms_data_extract
string tag = "Extract the file based on start date and end date"
integer x = 1143
integer y = 1320
integer width = 256
integer taborder = 0
integer textsize = -10
string text = "&Extract"
end type

event clicked;call super::clicked;date ld_start, ld_end
string ls_bkmed

// Get start dates and end dates
ld_start = date(em_start_date.Text)
ld_end = date(em_end_date.Text)

// By default start date is a week ago
IF ISNULL(ld_start)  THEN
	ld_start = relativedate(TODAY(),-7)
END IF

// End date is today
IF ISNULL(ld_end)  THEN
	ld_start = TODAY()
END IF

if ld_start < date("1/1/1980") or ld_start > date("1/1/2030") or ld_end < date("1/1/1980") or ld_end > date("1/1/2030") then
	  messagebox("Date Error", "Please check your start and end dates and try again.")
	  return
end if

// Get the book medium type
choose case true
	case rb_braille.checked, rb_abr.checked, rb_cbr.checked
		ls_bkmed = "BR"
	case rb_recorded.checked, rb_arc.checked, rb_crc.checked
		ls_bkmed = "RC"
	case cbx_bypass.checked
		ls_bkmed = "NotUsed"
	case rb_abridged.checked
		ls_bkmed = "RC_Abridged"
	case else
		messagebox("Error","Please select either Recorded or Braille books")
		return
end choose

// Call the export function in the window
Parent.event static trigger ue_export(ld_start, ld_end, ls_bkmed)
end event

type cb_clear from u_cb within w_pms_data_extract
string tag = "Clear the data"
integer x = 1440
integer y = 1320
integer width = 247
integer taborder = 0
integer textsize = -10
string text = "&Clear"
end type

event clicked;call super::clicked;Parent.event dynamic trigger ue_clear()
end event

type cb_exit from u_cb within w_pms_data_extract
string tag = "Exits the record"
integer x = 1733
integer y = 1320
integer width = 261
integer taborder = 0
integer textsize = -10
string text = "E&xit"
end type

event clicked;call super::clicked;parent.Event pfc_close()
end event

type gb_wpcat from groupbox within w_pms_data_extract
integer x = 73
integer y = 384
integer width = 1865
integer height = 800
integer taborder = 50
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 33554432
long backcolor = 80263581
string text = "WordPerfect Extractions"
end type


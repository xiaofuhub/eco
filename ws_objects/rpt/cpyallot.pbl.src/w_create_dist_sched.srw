$PBExportHeader$w_create_dist_sched.srw
forward
global type w_create_dist_sched from w_main
end type
type dw_rs20_batch_tbl_with_libcd from u_pics_dw within w_create_dist_sched
end type
type dw_rs20_batch_tbl from u_pics_dw within w_create_dist_sched
end type
type dw_libcode from u_pics_dw within w_create_dist_sched
end type
type cbx_lib from checkbox within w_create_dist_sched
end type
type cb_refresh from commandbutton within w_create_dist_sched
end type
type uo_progress from u_progressbar within w_create_dist_sched
end type
type dw_sp5_dsweb from u_pics_dw within w_create_dist_sched
end type
type dw_sp1_distsched from u_pics_dw within w_create_dist_sched
end type
type cb_oracle from commandbutton within w_create_dist_sched
end type
type st_dist_file from u_st within w_create_dist_sched
end type
type cb_cancel from commandbutton within w_create_dist_sched
end type
type cb_go from commandbutton within w_create_dist_sched
end type
type cb_dist_file from commandbutton within w_create_dist_sched
end type
type sle_batch_date from singlelineedit within w_create_dist_sched
end type
type gb_3 from groupbox within w_create_dist_sched
end type
type gb_2 from groupbox within w_create_dist_sched
end type
type gb_1 from groupbox within w_create_dist_sched
end type
type sle_survey_date from singlelineedit within w_create_dist_sched
end type
type rb_all from radiobutton within w_create_dist_sched
end type
type rb_bklist from radiobutton within w_create_dist_sched
end type
type st_sel_file from u_st within w_create_dist_sched
end type
type cb_sel_file from commandbutton within w_create_dist_sched
end type
type gb_sel from groupbox within w_create_dist_sched
end type
type dw_1 from datawindow within w_create_dist_sched
end type
type st_1 from statictext within w_create_dist_sched
end type
end forward

global type w_create_dist_sched from w_main
integer width = 1787
integer height = 1220
string title = "Create Distribution Schedule File"
event ue_create_file ( )
event ue_cancel ( )
event ue_dist_file ( )
event ue_all ( )
event ue_single ( )
event ue_enterkey pbm_dwnprocessenter
event ue_sel_file ( )
event ue_bklist ( )
dw_rs20_batch_tbl_with_libcd dw_rs20_batch_tbl_with_libcd
dw_rs20_batch_tbl dw_rs20_batch_tbl
dw_libcode dw_libcode
cbx_lib cbx_lib
cb_refresh cb_refresh
uo_progress uo_progress
dw_sp5_dsweb dw_sp5_dsweb
dw_sp1_distsched dw_sp1_distsched
cb_oracle cb_oracle
st_dist_file st_dist_file
cb_cancel cb_cancel
cb_go cb_go
cb_dist_file cb_dist_file
sle_batch_date sle_batch_date
gb_3 gb_3
gb_2 gb_2
gb_1 gb_1
sle_survey_date sle_survey_date
rb_all rb_all
rb_bklist rb_bklist
st_sel_file st_sel_file
cb_sel_file cb_sel_file
gb_sel gb_sel
dw_1 dw_1
st_1 st_1
end type
global w_create_dist_sched w_create_dist_sched

forward prototypes
public subroutine wf_error (n_ds lds, n_ds lds2)
end prototypes

event ue_create_file();n_ds lds, lds2
string ls_arg_bkmed, ls_arg_bkseq, ls_aray_prdr[]
long ll_rows, ll_count, ll_count2, ll_lastrow
date ld_mydate, ld_mydate2
string ls_header, ls_anno, ls_in, ls_out[], ls_text, ls_anno_array[]
string ls_title, ls_title_array[]
string ls_cfile, ls_sfile, ls_data
integer li_lines, li_filenum, li_title_lines, li_anno_lines, ll_pgno, ll_lict, i, j
string ls_bkmed, ls_bkseq, ls_auth, ls_authfn, ls_publisher, ls_formfeed
string ls_pubyr, ls_srcdoc, ls_reissue, ls_vols, ls_casub,  ls_libcd
string ls_reissue2, ls_bkmed2, ls_current_bkno, ls_prevbkseq, ls_prevbkmed, ls_med
string ls_current_lib, ls_ordqty, ls_defqty
string ls_prdr, ls_cabdt, ls_lastlibcd, ls_libs[200], ls_text1, ls_text2
string ls_bklist[], ls_oldbkseq, ls_oldbkmed, ls_aray_bkseq[]
long ll_defqty, ll_libtotal, ll_libs[200], ll_libcnt, ll_index, ll_index2, ll_bktotal
long ll_pages, ll_pageno, ll_selqty, ll_ordqty, grand_total_books, total_prdrs
long ll_cnt_rc, ll_cnt_pb, ll_cnt_fd, ll_cnt_br, ll_cnt_all
boolean lb_need_header, lb_nobookgiven, lb_lineavail
datetime ldt_mydate, ldt_mydate2
// Validate the date

ld_mydate = date(sle_batch_date.text)
ldt_mydate=datetime(ld_mydate,time('00:00:00'))
if isnull(ld_mydate) or ld_mydate < date("01/01/1980") AND &
	rb_bklist.checked = FALSE then
		messagebox("Invalid Date","Please correct your batch date.")
		wf_error(lds,lds2)
end if

ld_mydate2 = date(sle_survey_date.text)
ldt_mydate2=datetime(ld_mydate2,time('00:00:00'))
if isnull(ld_mydate2) or ld_mydate2 < date("01/01/1980") then
	messagebox("Invalid Date","Please correct your survey date.")
	wf_error(lds,lds2)
end if

// get files

ls_cfile = st_dist_file.text


// Create and load the datastore

lds = create n_ds

if rb_bklist.checked = true then
	lds.dataobject = "d_ds_pgm85_distsched_bklist"
else
	lds.dataobject = "d_ds_pgm85_distsched"
end if

ll_rows = lds.settransobject(sqlservertrans)
lds.Object.DataWindow.Table.Data.Storage = "disk"
open(w_pics_retrieve_msg_box)

lb_nobookgiven = true
if rb_bklist.checked = true then
   for ll_count = 1 to dw_1.rowcount()
		ls_data = trim(dw_1.object.bookno[ll_count])
		if ls_data <> "" then
   		ls_bklist[ll_count] = ls_data
			lb_nobookgiven = false
		end if
	next 
	if lb_nobookgiven = true then
		messagebox("No Books Provided","Please provide a list of books for the distribution schedule.")
		wf_error(lds,lds2)
	end if
	ll_rows = lds.retrieve(ls_bklist[])
else
	ll_rows = lds.retrieve(ldt_mydate)
end if

if (rb_bklist.checked = FALSE AND ll_rows = 0) THEN
	messagebox("No records found","Please adjust your batch date and try again.")
	close(w_pics_retrieve_msg_box)
	wf_error(lds,lds2)
elseif (rb_bklist.checked = TRUE AND ll_rows = 0) THEN
	messagebox("No records found","Please select another set of books and try again.")
	close(w_pics_retrieve_msg_box)
	wf_error(lds,lds2)
end if

if ll_rows = -1 then
	messagebox("Database Error","There was an error during the retrieve.  Please try again.")
	close(w_pics_retrieve_msg_box)
	wf_error(lds,lds2)
end if

w_pics_retrieve_msg_box.sle_retrieve.text = "Sorting Records"
lds.setsort("ancntr_prdr A, mchar_bkseq A, mchar_bkmed A, lib_libcd A")
lds.sort()

w_pics_retrieve_msg_box.sle_retrieve.text = "Updating Batch Table"

IF rb_bklist.checked = FALSE THEN
	UPDATE batch  
	SET dsdt = :ldt_mydate2
	WHERE CABDT = :ldt_mydate
	USING SQLServerTrans;
	IF f_check_dberror(SQLservertrans,"BATCH") THEN
		COMMIT USING SQLServerTrans;
	END IF
END IF

w_pics_retrieve_msg_box.sle_retrieve.text = "Processing Data"

// Distrib.calt - write data file

if not(ls_cfile = "" or ls_cfile = "none") then
	
// create datastore for updates

lds2 = create n_ds
lds2.dataobject = "d_ds_dist_qnty_update"
lds2.settransobject(sqlservertrans)
lds2.reset()

ls_formfeed = "1"

li_filenum = Fileopen(ls_cfile,streammode!,write!,lockwrite!,Replace!)
if li_filenum = -1 then
	messagebox("File Error","the file could not be opened for output.")
	close(w_pics_retrieve_msg_box)
	wf_error(lds,lds2)
end if

ls_current_bkno = ""
grand_total_books =0

for ll_count = 1 to ll_rows
	
	// lets get all the values we'll need
	ls_prdr = trim(string(lds.object.ancntr_prdr[ll_count]))
	IF IsNull(ls_prdr) OR ls_prdr="" THEN
		ls_prdr="Not Assigned Yet"
	END IF
	ls_bkmed = trim(string(lds.object.mchar_bkmed[ll_count]))
	ls_bkseq = trim(string(lds.object.mchar_bkseq[ll_count]))
	ll_defqty = long(string(lds.object.sched_ordqty[ll_count]))
	ls_casub = string(lds.object.ttlinit_casub[ll_count])
	ls_libcd = trim(lds.object.lib_libcd[ll_count])
	ls_med = string(lds.object.mchar_med[ll_count])
	ls_title = string(lds.object.ttlinit_ttl[ll_count])
		
	// did we finish a book?
	grand_total_books += ll_defqty
	ls_aray_prdr[ll_count] = ls_prdr
	ls_aray_bkseq[ll_count ] =ls_bkseq
	if ls_current_bkno <> (ls_bkseq + ls_bkmed) and ls_current_bkno <> "" then
		// dont forget to write the totals
		ll_libcnt++
		ls_libs[ll_libcnt] = ls_lastlibcd
		ll_libs[ll_libcnt] = ll_libtotal
		// yes, so print the book with the info we've collected and the totals
		ll_pages = ceiling(ll_libcnt/104)
		
		// get the total number of books
		ll_bktotal = 0
		for ll_index = 1 to ll_libcnt
			ll_bktotal = ll_bktotal + ll_libs[ll_index]
		next
		
		// write total + book to second datastore
		
		ll_lastrow = lds2.insertrow(0)
		lds2.object.bkseq[ll_lastrow] = long(ls_oldbkseq)
		lds2.object.bkmed[ll_lastrow] = ls_oldbkmed
		lds2.object.cascd[ll_lastrow] = "R"
		lds2.object.qnty[ll_lastrow] = ll_bktotal
		
		for ll_pageno = 1 to ll_pages
		
			// write the header
			filewrite(li_filenum, ls_header)
		
			// build each line
			for ll_index = (ll_pageno - 1)*104+1 to (ll_pageno - 1)*104+52  // first col
				ll_index2 = ll_index + 52   // second col
				lb_lineavail = false
				ls_text1 = "                "
				ls_text2 = "                "
				if ll_index <= ll_libcnt then
					ls_text1 = ls_libs[ll_index] + right("                "+string(ll_libs[ll_index]),12)
					lb_lineavail = true
				end if
				if ll_index2 <= ll_libcnt then
					ls_text2 = ls_libs[ll_index2] + right("                "+string(ll_libs[ll_index2]),12)
					lb_lineavail = true
				end if
				ls_text = "              " + ls_text1 + "             " + ls_text2+"~n"
				if lb_lineavail = true then
					filewrite(li_filenum, ls_text)
				end if
			next // for ll_index
			if ll_pageno = ll_pages then  // last page so print the total
				filewrite(li_filenum,"-~n")
				filewrite(li_filenum, "     TOTAL COPIES REQUIRED: " + right( "      "+string(ll_bktotal),5)+"~n")
			end if
		next //for ll_pageno
		

	end if //end if new bookno and oldbkno <>''
		
	// is this a new book?
	
	if ls_current_bkno <> (ls_bkseq + ls_bkmed)  then
		// yes, so collect the header and clear all the totals
		
		ls_header = "&
1    PRODUCER: " + left(ls_prdr+"          ",10) + &
"                  BOOK NUMBER:      "+ls_bkmed+ls_bkseq+"~n&
     "+left(ls_title+"                                                            ",55) + &
	  + "  "+ trim(ls_casub) + "-" + trim(ls_bkmed) + "~n&
0    BATCH: " + right("0000"+string(month(ld_mydate)),2) + "/" + string(year(ld_mydate)) + &
"                        SCHEDULE DATE:   " + trim(string(ld_mydate2,"mm/dd/yyyy")) + "~n&
0         LIBRARY CODE   QUANTITY       LIBRARY CODE   QUANTITY~n &
         ------------   --------       ------------   --------~n"

		
		
		ll_libcnt = 0
		ls_lastlibcd = ""
		ls_current_bkno = ls_bkseq + ls_bkmed
		ls_oldbkseq = ls_bkseq
		ls_oldbkmed = ls_bkmed
	end if //no condition ls_current_bkno <>''
	
	// add this information to the totals
	
	// did we finish a book?
	
	if ls_libcd <> ls_lastlibcd and ls_lastlibcd<> "" then
		// yes, so add it to list
		ll_libcnt++
		ls_libs[ll_libcnt] = ls_lastlibcd
		ll_libs[ll_libcnt] = ll_libtotal
	end if
	
	// are we starting a new book?
	if ls_libcd <> ls_lastlibcd then
		// yes, so clear totals
		ll_libtotal = 0
		ls_lastlibcd = ls_libcd
	end if
	
	// add this book info to the totals
	
	ll_libtotal = ll_libtotal + ll_defqty
		
	// Refresh the screen.		
	IF mod(ll_count,50)=0 THEN
		w_create_dist_sched.SetRedraw(TRUE) 
		w_pics_retrieve_msg_box.SetRedraw(TRUE)
	END IF
	
next //for ll_count =1 to ll_rows
		
		
// finish the last book		

ll_libcnt++
ls_libs[ll_libcnt] = ls_lastlibcd
ll_libs[ll_libcnt] = ll_libtotal
		
// print out the last book

ll_pages = ceiling(ll_libcnt/104)

// get the total number of books
ll_bktotal = 0
for ll_index = 1 to ll_libcnt
	ll_bktotal = ll_bktotal + ll_libs[ll_index]
next

// write total + book to second datastore

ll_lastrow = lds2.insertrow(0)
lds2.object.bkseq[ll_lastrow] = long(ls_oldbkseq)
lds2.object.bkmed[ll_lastrow] = ls_oldbkmed
lds2.object.cascd[ll_lastrow] = "R"
lds2.object.qnty[ll_lastrow] = ll_bktotal

w_pics_retrieve_msg_box.sle_retrieve.text = "Writing Data Files"

for ll_pageno = 1 to ll_pages

	// write the header
	filewrite(li_filenum, ls_header)
	
	// build each line
	for ll_index = (ll_pageno - 1)*104+1 to (ll_pageno - 1)*104+52  // first col
		lb_lineavail = false
		ll_index2 = ll_index + 52   // second col
		ls_text1 = "                "
		ls_text2 = "                "
		if ll_index <= ll_libcnt then
			ls_text1 = ls_libs[ll_index] + right("                "+string(ll_libs[ll_index]),12)
			lb_lineavail = true
		end if
		if ll_index2 <= ll_libcnt then
			ls_text2 = ls_libs[ll_index2] + right("                "+string(ll_libs[ll_index2]),12)
			lb_lineavail = true
		end if
		ls_text = "              " + ls_text1 + "             " + ls_text2
		if lb_lineavail = true then
			filewrite(li_filenum, ls_text+"~n")
		end if
	next
	if ll_pageno = ll_pages then  // last page so print the total
		filewrite(li_filenum, "     TOTAL COPIES REQUIRED: " + right( "      "+string(ll_bktotal),5)+"~n")
		end if
	// Refresh the screen.		
	IF mod(ll_pageno,50)=0 THEN
		w_create_dist_sched.SetRedraw(TRUE) 
		w_pics_retrieve_msg_box.SetRedraw(TRUE)
	END IF
next

// all done, write the footer

ls_header = "1~n~n"
filewrite(li_filenum,ls_header)

fileclose(li_filenum)

// write out the totals in the second datastore

w_pics_retrieve_msg_box.sle_retrieve.text = "Updating Quantity Totals"

dwitemstatus ldwstat 

for ll_lastrow = 1 to rowcount(lds2)
	lds2.SetItemStatus(ll_lastrow, 0, Primary!, Datamodified!)
	lds2.SetItemStatus(ll_lastrow, 1, Primary!, Datamodified!)
	lds2.SetItemStatus(ll_lastrow, 1, Primary!, Notmodified!)
	lds2.SetItemStatus(ll_lastrow, 2, Primary!, Datamodified!)
	lds2.SetItemStatus(ll_lastrow, 2, Primary!, Notmodified!)
	lds2.SetItemStatus(ll_lastrow, 3, Primary!, DataModified!)
	lds2.SetItemStatus(ll_lastrow, 4, Primary!, Datamodified!)
	ldwstat = lds2.GetItemStatus(ll_lastrow, 1, Primary!)
	if ldwstat <> NotModified! then
		messagebox("Bad Status","row " + string(ll_lastrow) + " col 1")
	end if
	ldwstat = lds2.GetItemStatus(ll_lastrow, 2, Primary!)
	if ldwstat <> NotModified! then
		messagebox("Bad Status","row " + string(ll_lastrow) + " col 2")
	end if
	ldwstat = lds2.GetItemStatus(ll_lastrow, 3, Primary!)
	if ldwstat <> DataModified! then
		messagebox("Bad Status","row " + string(ll_lastrow) + " col 3")
	end if
	ldwstat = lds2.GetItemStatus(ll_lastrow, 4, Primary!)
	if ldwstat <> DataModified! then
		messagebox("Bad Status","row " + string(ll_lastrow) + " col 4")
	end if
next

if lds2.update() = 1 then
	commit using sqlservertrans;
else
	messagebox("Update Error","update error")
	rollback using sqlservertrans;
end if

//messagebox("Update Complete",string(sqlservertrans.sqlnrows) + " Book Total(s) updated")

// close file, all done
end if// IF ls_cfile <>'' and none


// selected quantities

ls_sfile = st_sel_file.text

if not(ls_sfile = "" or ls_sfile = "none") then
	
ls_formfeed = "1"

w_pics_retrieve_msg_box.sle_retrieve.text = "Sorting Records"
lds.setsort("lib_libcd A, mchar_bkseq A, mchar_bkmed A")
lds.sort()

w_pics_retrieve_msg_box.sle_retrieve.text = "Writing Data Files"
li_filenum = Fileopen(ls_sfile,streammode!,write!,lockwrite!,Replace!)
if li_filenum = -1 then
	messagebox("File Error","the file could not be opened for output.")
	wf_error(lds,lds2)
end if

ls_current_lib = ""


for ll_count = 1 to ll_rows
	
	// lets get all the values we'll need
	ls_prdr = trim(string(lds.object.ancntr_prdr[ll_count]))
	ls_bkmed = trim(string(lds.object.mchar_bkmed[ll_count]))
	ls_bkseq = trim(string(lds.object.mchar_bkseq[ll_count]))
	ll_ordqty = long(string(lds.object.sched_ordqty[ll_count]))
	ll_defqty = long(string(lds.object.def_defqty[ll_count]))
	ls_casub = string(lds.object.ttlinit_casub[ll_count])
	ls_libcd = trim(lds.object.lib_libcd[ll_count])
	ls_med = string(lds.object.mchar_med[ll_count])
	ls_title = string(lds.object.ttlinit_ttl[ll_count])
		
	// did we finish a library?
	
	if ls_current_lib <> (ls_libcd) and ls_current_lib <> "" then
		// write the totals and end the page
		ls_text = "~n&
~n&
 Total copies:    " + right("     " + string(ll_cnt_all),5) + "~n&
          RC :    " + right("     " + string(ll_cnt_rc),5) + "~n&
          FD :    " + right("     " + string(ll_cnt_fd),5) + "~n&
          BR :    " + right("     " + string(ll_cnt_br),5) + "~n&
          P/B:    " + right("     " + string(ll_cnt_pb),5) + "~n&
1~n"

		filewrite(li_filenum,ls_text)

	end if
		
	// is this a new library?
	
	if ls_current_lib <> (ls_libcd)  then
		// yes, so print the header and clear all the totals
		
		ll_pageno = 1
		
		ls_header = "&
&
              REPORT OF QUANTITIES SELECTED IN LAST BATCH~n&
&
 BATCH DATE: "+string(ld_mydate,"mm/dd/yyyy")+"                            LIBRARY CODE:  "+trim(ls_libcd)+"~n&
 Date run:  "+string(today(),"mm/dd/yyyy")+"~n~n&
 BOOK NUMBER  SELECTION  DEFAULT   TITLE              DISTRIBUTION CODE~n~n&
"

		filewrite(li_filenum, ls_header)
		ls_current_lib = ls_libcd
		ll_cnt_rc = 0
		ll_cnt_fd = 0
		ll_cnt_br = 0
		ll_cnt_pb = 0
		ll_cnt_all = 0
		li_lines = 0
		ls_current_lib = ls_libcd
		
	end if
	
	if li_lines = 52 then  // we've filled a page
		filewrite(li_filenum,"1~n")
		ll_pageno++
		ls_header = "&
 BATCH DATE: "+string(ld_mydate,"mm/dd/yyyy")+"         -"+string(ll_pageno)+"-                LIBRARY CODE:  "+trim(ls_libcd)+"~n&
~n&
 BOOK NUMBER  SELECTION  DEFAULT   TITLE              DISTRIBUTION CODE~n~n&
"

		filewrite(li_filenum, ls_header)
		li_lines = 0
	end if
		
	// write and add this book info to the totals

	ls_defqty = "      "
	ls_ordqty = "      "
	
   if ll_defqty <> ll_ordqty then
		ls_ordqty = right("      " + string(ll_ordqty),6)
	else
		ls_defqty = right("      " + string(ll_defqty),6)
	end if
	
   ls_text = " "+trim(string(ls_bkmed))+ left(trim(string(ls_bkseq))+"          ",10) + &
	ls_ordqty + "    " + ls_defqty + &
	"      " + left(trim(ls_title) + "                             ",29) + " " + &
	left(trim(ls_casub)+"   ",3) + "-" + trim(ls_bkmed)+"~n"
	
	filewrite(li_filenum,ls_text)
	ll_cnt_all++
	li_lines++
	CHOOSE CASE trim(ls_med)
	CASE "RC"
		ll_cnt_rc+= ll_ordqty
	CASE "BR"
		ll_cnt_br+= ll_ordqty
	CASE "P/B"
		ll_cnt_pb+= ll_ordqty
	CASE "FD"
		ll_cnt_fd+= ll_ordqty
END CHOOSE
		
	// Refresh the screen.		
	IF mod(ll_count,50)=0 THEN
		w_create_dist_sched.SetRedraw(TRUE) 
		w_pics_retrieve_msg_box.SetRedraw(TRUE)
	END IF

next
		
		


// all done, write the totals and end the page

ls_text = "~n&
~n&
 Total copies:    " + right("     " + string(ll_cnt_all),5) + "~n&
          RC :    " + right("     " + string(ll_cnt_rc),5) + "~n&
          FD :    " + right("     " + string(ll_cnt_fd),5) + "~n&
          BR :    " + right("     " + string(ll_cnt_br),5) + "~n&
          P/B:    " + right("     " + string(ll_cnt_pb),5) + "~n&
1~n"

filewrite(li_filenum,ls_text)

fileclose(li_filenum)


// close file, all done

end if

FOR i =1 to (ll_rows - 1)
	IF ls_aray_prdr[i] ='' THEN
		CONTINUE
	END IF
	for j= i+1 to ll_rows 
		IF ls_aray_prdr[j] ='' THEN
			CONTINUE
		END IF
		IF ls_aray_prdr[i] = ls_aray_prdr[j] THEN
			ls_aray_prdr[j] =''
		END IF
	NEXT
NEXT
j=0
FOR i =1 to ll_rows
	IF ls_aray_prdr[i] <>'' THEN
		j++
	END IF
NEXT
total_prdrs =j
Close(w_pics_retrieve_msg_box )
messagebox('Distribution',' Schedule file created'+ &
	'~nTotal books is :  '+string(grand_total_books) +&
	'~nTotal producers is :  '+string(total_prdrs) )

// write all totals





end event

event ue_cancel;call super::ue_cancel;close(this)
end event

event ue_dist_file();string ls_filename, ls_path

ls_filename = "distrib.txt"
ls_path =  ls_filename
if GetFileSaveName("Select Distribution File",ls_path,ls_filename, "txt", "Text Files (*.txt),*.txt") <> 1 then
	messagebox("Incorrect file name","Please try again")
	return
end if

st_dist_file.text = ls_path
end event

event ue_all;call super::ue_all;dw_1.visible = false
gb_sel.visible = true
cb_sel_file.visible = true
st_sel_file.visible = true

end event

event ue_single;call super::ue_single;//sle_bkno.enabled = true
end event

event ue_enterkey;call super::ue_enterkey;Send(Handle(this),256,9,Long(0,0))
return(1)
end event

event ue_sel_file();string ls_filename, ls_path

ls_filename = "selected.txt"
ls_path = ls_filename
if GetFileSaveName("Select Quantities File",ls_path,ls_filename, "txt", "Text Files (*.txt),*.txt") <> 1 then
	messagebox("Incorrect file name","Please try again")
	return
end if

st_sel_file.text = ls_path

rb_bklist.checked = false
rb_all.checked = true
end event

event ue_bklist;call super::ue_bklist;dw_1.visible = true
st_sel_file.text = "none"
cb_sel_file.visible = false
st_sel_file.visible = false
gb_sel.visible = false
end event

public subroutine wf_error (n_ds lds, n_ds lds2);if isvalid(lds) then
	destroy lds
end if

if isvalid(lds2) then
	destroy lds2
end if

if isvalid(w_pics_retrieve_msg_box) then
	close(w_pics_retrieve_msg_box)
end if
return 
end subroutine

on w_create_dist_sched.create
int iCurrent
call super::create
this.dw_rs20_batch_tbl_with_libcd=create dw_rs20_batch_tbl_with_libcd
this.dw_rs20_batch_tbl=create dw_rs20_batch_tbl
this.dw_libcode=create dw_libcode
this.cbx_lib=create cbx_lib
this.cb_refresh=create cb_refresh
this.uo_progress=create uo_progress
this.dw_sp5_dsweb=create dw_sp5_dsweb
this.dw_sp1_distsched=create dw_sp1_distsched
this.cb_oracle=create cb_oracle
this.st_dist_file=create st_dist_file
this.cb_cancel=create cb_cancel
this.cb_go=create cb_go
this.cb_dist_file=create cb_dist_file
this.sle_batch_date=create sle_batch_date
this.gb_3=create gb_3
this.gb_2=create gb_2
this.gb_1=create gb_1
this.sle_survey_date=create sle_survey_date
this.rb_all=create rb_all
this.rb_bklist=create rb_bklist
this.st_sel_file=create st_sel_file
this.cb_sel_file=create cb_sel_file
this.gb_sel=create gb_sel
this.dw_1=create dw_1
this.st_1=create st_1
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_rs20_batch_tbl_with_libcd
this.Control[iCurrent+2]=this.dw_rs20_batch_tbl
this.Control[iCurrent+3]=this.dw_libcode
this.Control[iCurrent+4]=this.cbx_lib
this.Control[iCurrent+5]=this.cb_refresh
this.Control[iCurrent+6]=this.uo_progress
this.Control[iCurrent+7]=this.dw_sp5_dsweb
this.Control[iCurrent+8]=this.dw_sp1_distsched
this.Control[iCurrent+9]=this.cb_oracle
this.Control[iCurrent+10]=this.st_dist_file
this.Control[iCurrent+11]=this.cb_cancel
this.Control[iCurrent+12]=this.cb_go
this.Control[iCurrent+13]=this.cb_dist_file
this.Control[iCurrent+14]=this.sle_batch_date
this.Control[iCurrent+15]=this.gb_3
this.Control[iCurrent+16]=this.gb_2
this.Control[iCurrent+17]=this.gb_1
this.Control[iCurrent+18]=this.sle_survey_date
this.Control[iCurrent+19]=this.rb_all
this.Control[iCurrent+20]=this.rb_bklist
this.Control[iCurrent+21]=this.st_sel_file
this.Control[iCurrent+22]=this.cb_sel_file
this.Control[iCurrent+23]=this.gb_sel
this.Control[iCurrent+24]=this.dw_1
this.Control[iCurrent+25]=this.st_1
end on

on w_create_dist_sched.destroy
call super::destroy
destroy(this.dw_rs20_batch_tbl_with_libcd)
destroy(this.dw_rs20_batch_tbl)
destroy(this.dw_libcode)
destroy(this.cbx_lib)
destroy(this.cb_refresh)
destroy(this.uo_progress)
destroy(this.dw_sp5_dsweb)
destroy(this.dw_sp1_distsched)
destroy(this.cb_oracle)
destroy(this.st_dist_file)
destroy(this.cb_cancel)
destroy(this.cb_go)
destroy(this.cb_dist_file)
destroy(this.sle_batch_date)
destroy(this.gb_3)
destroy(this.gb_2)
destroy(this.gb_1)
destroy(this.sle_survey_date)
destroy(this.rb_all)
destroy(this.rb_bklist)
destroy(this.st_sel_file)
destroy(this.cb_sel_file)
destroy(this.gb_sel)
destroy(this.dw_1)
destroy(this.st_1)
end on

event open;call super::open;dw_1.insertrow(0)
dw_1.visible = false
ib_disableclosequery = true
sle_survey_date.text = string(today())
//sle_survey_date.displayonly = true

end event

type dw_rs20_batch_tbl_with_libcd from u_pics_dw within w_create_dist_sched
boolean visible = false
integer x = 722
integer y = 732
integer width = 78
integer height = 112
integer taborder = 50
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

type dw_rs20_batch_tbl from u_pics_dw within w_create_dist_sched
boolean visible = false
integer x = 841
integer y = 732
integer width = 78
integer height = 112
integer taborder = 110
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

type dw_libcode from u_pics_dw within w_create_dist_sched
boolean visible = false
integer x = 1234
integer y = 860
integer width = 398
integer height = 112
integer taborder = 120
string dataobject = "dddw_libcode"
boolean vscrollbar = false
boolean livescroll = false
end type

event constructor;call super::constructor;THIS.settransobject(sqlservertrans)

end event

type cbx_lib from checkbox within w_create_dist_sched
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

type cb_refresh from commandbutton within w_create_dist_sched
boolean visible = false
integer x = 32
integer y = 868
integer width = 699
integer height = 100
integer taborder = 110
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "Refresh SCHED Table "
end type

event clicked;int rtn
date lcabdt

IF IsNull(sle_batch_date.text) OR sle_batch_date.text="" THEN
	MessageBox("ERROR","Batch date is null, please enter the date.")
	sle_batch_date.SetFocus()
ELSE		
	lcabdt = date(sle_batch_date.text)

	rtn = MessageBox("Data Migration","This process will refresh the data in ~"sched~" table(RS20) using existing data from ~"batch~" table(RS21)." +&
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
			Integer li_count
			long ll_rows
			
			ll_rows = dw_rs20_batch_tbl.retrieve()
			IF ll_rows = 0 THEN
				MessageBox("ERROR","No rows found.")
			ELSE		
				uo_progress.Visible=TRUE
				uo_progress.of_SetMinimum(0)
				uo_progress.of_SetMaximum(ll_rows)
				uo_progress.of_SetDisplayStyle(3)
				uo_progress.of_SetMessageText(ls_msgtext)
				uo_progress.of_SetPosition(0)
				
				uo_progress.Visible=FALSE
			END IF		
		END IF
	END IF
END IF

//
//
//int rtn
//	
//rtn = MessageBox("Refresh default quantity","This process will refresh default quantity from ~"libdef~" table(WEB) into ~"def~" table(PICS)." +&
//							" This process may take some times. Continue?",Question!,YesNo!,1)
//IF rtn = 1	THEN
//	String ls_msgtext[ ] = {" 0 %","10 %", "20 %", "30 %", "40 %", "50 %", "60 %", &
//												"70 %", "80 %", "90 %", "100 %"}
//	
//	long ll_rows,lcnt,nullval,rows_updated=0
//	string llibcd,lmed,lcasub
//	integer ldef_n_def,ldef_p_def,ldef_r_def
//	integer lsel_n_def,lsel_p_def,lsel_r_def
//	date ldefdt,ltoday
//	
//	ltoday = today()
//	
//	SetNull(nullval)
//	
//	// Reset the datawindows
//	dw_sp9_deflib.settransobject(sqlserveroracletrans)
//	dw_sp9_deflib.Reset()
//	dw_ca_def_casub.settransobject(sqlservertrans)
//	//dw_ca_def_casub.Reset()
//				
//	// retrieve the data from libdef table at rs20
//	ll_rows = dw_sp9_deflib.Retrieve()
//	
//	IF ll_rows<>0 THEN
//		
//		IF dw_sp9_deflib.Visible=FALSE THEN
//			dw_sp9_deflib.Visible=TRUE
//			dw_ca_def_casub.Visible=FALSE
//		END IF
//		
//		sle_rows.Text = string(ll_rows)
//		
//		string newsort
//		newsort = "libcd A,med,casub"
//		dw_sp9_deflib.SetSort(newsort)
//		dw_sp9_deflib.Sort()
//
//		uo_progress.Visible=TRUE
//		uo_progress.of_SetMinimum(0)
//		uo_progress.of_SetMaximum(ll_rows)
//		uo_progress.of_SetDisplayStyle(3)
//		uo_progress.of_SetMessageText(ls_msgtext)
//		uo_progress.of_SetPosition(0)
//		
//		openwithparm(w_pics_retrieve_msg_box,"Synchronizing defqty(Normal, Priority and Rush). Please Wait...")
//		
//		
//		FOR lcnt = 1 TO ll_rows
//			
//				llibcd 		= TRIM(dw_sp9_deflib.object.libcd[lcnt])
//				lmed 			= TRIM(dw_sp9_deflib.object.med[lcnt])
//				lcasub 		= TRIM(dw_sp9_deflib.object.casub[lcnt])
//				lsel_n_def  = dw_sp9_deflib.object.sel_n_defqty[lcnt]
//				IF NOT(IsNull(lsel_n_def)) THEN
//					update def
//					set defqty = :lsel_n_def,defdt = :ltoday
//					where libcd = :llibcd
//					and   med   = :lmed
//					and   casub = :lcasub
//					and   priority = 'N'
//					using sqlservertrans;
//					dw_sp9_deflib.object.curr_n_defqty[lcnt] 	=	dw_sp9_deflib.object.sel_n_defqty[lcnt]
//					dw_sp9_deflib.object.sel_n_defqty[lcnt] 	=	nullval
//					rows_updated++
//				END IF
//						
//				lsel_p_def  = dw_sp9_deflib.object.sel_p_defqty[lcnt]
//				IF NOT(IsNull(lsel_p_def)) THEN
//					update def
//					set defqty = :lsel_p_def,defdt = :ltoday
//					where libcd = :llibcd
//					and   med   = :lmed
//					and   casub = :lcasub
//					and   priority = 'P'
//					using sqlservertrans;
//					dw_sp9_deflib.object.curr_p_defqty[lcnt] 	=	dw_sp9_deflib.object.sel_p_defqty[lcnt]
//					dw_sp9_deflib.object.sel_p_defqty[lcnt] 	=	nullval
//					rows_updated++
//				END IF
//				
//				lsel_r_def  = dw_sp9_deflib.object.sel_r_defqty[lcnt]
//				IF NOT(IsNull(lsel_r_def)) THEN
//					update def
//					set defqty = :lsel_r_def,defdt = :ltoday
//					where libcd = :llibcd
//					and   med   = :lmed
//					and   casub = :lcasub
//					and   priority = 'R'
//					using sqlservertrans;
//					dw_sp9_deflib.object.curr_r_defqty[lcnt] 	=	dw_sp9_deflib.object.sel_r_defqty[lcnt]
//					dw_sp9_deflib.object.sel_r_defqty[lcnt] 	=	nullval
//					rows_updated++
//				END IF
//				
//				IF NOT(IsNull(lsel_n_def)) OR &
//					NOT(IsNull(lsel_p_def)) OR &
//					NOT(IsNull(lsel_r_def)) THEN
//					dw_sp9_deflib.object.sent_flag[lcnt] 		=  'Y'
//				END IF					
//				
//				IF lcnt <= uo_progress.of_GetMaximum() THEN
//					uo_progress.of_Increment(1)
//					IF mod(lcnt,100)=0 THEN
//						sle_rows_upd.Text = string(rows_updated)
//						w_ca_load_deflib.SetRedraw(TRUE) 
//					END IF
//				END IF
//		NEXT			
//			
//		close(w_pics_retrieve_msg_box)
//
//		uo_progress.Visible=FALSE
//			
//		dw_sp9_deflib.settransobject(sqlserveroracletrans)
//					
//		rtn = dw_sp9_deflib.of_Update(TRUE,TRUE)
//		IF f_check_dberror(sqlserveroracletrans,"LIBDEF at RS21") THEN
//			IF rtn=1 THEN
//				Commit Using sqlservertrans;
//				Commit Using sqlserveroracletrans;
//				MessageBox("Update",string(rows_updated)+" row(s) updated in DEF Table. ")
//			ELSE
//				MessageBox("Update"," Update libdef table failed.")
//				Rollback Using sqlserveroracletrans;
//				Rollback Using sqlservertrans;
//				uo_progress.Visible=FALSE
//				IF rows_updated > 0 THEN
//					dw_ca_def_casub.retrieve()
//				END IF
//				RETURN
//			END IF
//		END IF
//	
//		uo_progress.Visible=FALSE
//	ELSE
//		MessageBox("ERROR","No rows retrieved from libdef(WEB).")
//	END IF		
//END IF
//
end event

type uo_progress from u_progressbar within w_create_dist_sched
boolean visible = false
integer x = 37
integer y = 996
integer width = 1678
integer height = 92
integer taborder = 110
boolean border = true
borderstyle borderstyle = stylelowered!
end type

on uo_progress.destroy
call u_progressbar::destroy
end on

type dw_sp5_dsweb from u_pics_dw within w_create_dist_sched
boolean visible = false
integer x = 955
integer y = 732
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

event retrievestart;call super::retrievestart;openwithparm(w_pics_retrieve_msg_box,"Populating temporary tables, Please Wait...")
end event

event retrieveend;call super::retrieveend;close(w_pics_retrieve_msg_box)
end event

type dw_sp1_distsched from u_pics_dw within w_create_dist_sched
boolean visible = false
integer x = 1074
integer y = 732
integer width = 82
integer height = 96
integer taborder = 110
string dataobject = "d_sp1_distsched"
boolean vscrollbar = false
boolean livescroll = false
end type

event constructor;call super::constructor;THIS.settransobject(sqlserveroracletrans)

end event

event updatestart;call super::updatestart;openwithparm(w_pics_retrieve_msg_box,"Updating distribution schedule table in RS21n, Please Wait...")
end event

event updateend;call super::updateend;close(w_pics_retrieve_msg_box)
end event

event sqlpreview;call super::sqlpreview;IF This.GetRow() <= uo_progress.of_GetMaximum() THEN
	uo_progress.of_Increment(1)
	IF mod(This.GetRow(),100)=0 THEN
		w_create_dist_sched.SetRedraw(TRUE) 
		w_pics_retrieve_msg_box.SetRedraw(TRUE)
	END IF
END IF

end event

type cb_oracle from commandbutton within w_create_dist_sched
integer x = 32
integer y = 744
integer width = 640
integer height = 92
integer taborder = 100
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "&Load data into Web"
end type

event clicked;int rtn
date lcabdt
datetime ldt_cabdt

IF IsNull(sle_batch_date.text) OR sle_batch_date.text="" THEN
	MessageBox("ERROR","Batch date is null, please enter the date.")
	sle_batch_date.SetFocus()
ELSE		
	lcabdt = date(sle_batch_date.text)
	ldt_cabdt=datetime(lcabdt,time('00:00:00'))
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
			Integer li_count
			long ll_rows
			
			ll_rows = dw_sp5_dsweb.retrieve(ldt_cabdt)
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

type st_dist_file from u_st within w_create_dist_sched
integer x = 46
integer y = 420
integer width = 1010
long backcolor = 1090519039
boolean border = true
borderstyle borderstyle = stylelowered!
end type

type cb_cancel from commandbutton within w_create_dist_sched
integer x = 1467
integer y = 740
integer width = 247
integer height = 96
integer taborder = 90
boolean bringtotop = true
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "&Cancel"
boolean cancel = true
end type

event clicked;IF NOT SQLServerOracleTrans.DBHandle() =0 THEN
	IF SQLServerOracleTrans.of_disconnect() < 0 THEN
		MessageBox("Error","Oracle Database Disconnect Error.",StopSign!)
	END IF
END IF

Parent.triggerevent("ue_cancel")
end event

type cb_go from commandbutton within w_create_dist_sched
integer x = 1193
integer y = 740
integer width = 247
integer height = 96
integer taborder = 80
boolean bringtotop = true
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "&Go"
end type

event clicked;Parent.triggerevent("ue_create_file")
end event

type cb_dist_file from commandbutton within w_create_dist_sched
integer x = 1070
integer y = 420
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

type sle_batch_date from singlelineedit within w_create_dist_sched
integer x = 50
integer y = 120
integer width = 549
integer height = 64
integer taborder = 10
boolean bringtotop = true
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
boolean autohscroll = false
borderstyle borderstyle = stylelowered!
end type

type gb_3 from groupbox within w_create_dist_sched
integer x = 640
integer y = 60
integer width = 590
integer height = 148
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long backcolor = 79741120
string text = "Run Date:"
end type

type gb_2 from groupbox within w_create_dist_sched
integer x = 27
integer y = 60
integer width = 608
integer height = 148
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long backcolor = 79741120
string text = "Batch Date:"
end type

type gb_1 from groupbox within w_create_dist_sched
integer x = 23
integer y = 360
integer width = 1207
integer height = 148
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long backcolor = 79741120
string text = "Distribution Schedule Filename:"
end type

type sle_survey_date from singlelineedit within w_create_dist_sched
integer x = 658
integer y = 120
integer width = 530
integer height = 64
integer taborder = 20
boolean bringtotop = true
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 33554432
boolean autohscroll = false
borderstyle borderstyle = stylelowered!
end type

type rb_all from radiobutton within w_create_dist_sched
integer x = 1285
integer y = 72
integer width = 320
integer height = 76
integer taborder = 50
boolean bringtotop = true
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long backcolor = 79741120
string text = "All Books"
boolean checked = true
end type

event clicked;Parent.triggerevent("ue_all")
end event

type rb_bklist from radiobutton within w_create_dist_sched
integer x = 1280
integer y = 140
integer width = 421
integer height = 76
integer taborder = 60
boolean bringtotop = true
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long backcolor = 79741120
string text = "Listed Books"
end type

event clicked;Parent.triggerevent("ue_bklist")
end event

type st_sel_file from u_st within w_create_dist_sched
integer x = 46
integer y = 608
integer width = 1010
long backcolor = 1090519039
boolean border = true
borderstyle borderstyle = stylelowered!
end type

type cb_sel_file from commandbutton within w_create_dist_sched
integer x = 1070
integer y = 608
integer width = 119
integer height = 68
integer taborder = 40
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

type gb_sel from groupbox within w_create_dist_sched
integer x = 27
integer y = 548
integer width = 1207
integer height = 148
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long backcolor = 79741120
string text = "Quantities Selected Filename:"
end type

type dw_1 from datawindow within w_create_dist_sched
integer x = 1289
integer y = 224
integer width = 425
integer height = 392
integer taborder = 70
boolean bringtotop = true
string dataobject = "d_dist_bklist"
boolean vscrollbar = true
boolean livescroll = true
end type

event itemchanged;if row = rowcount() then
	insertrow(0)
end if
resetupdate()
end event

event losefocus;accepttext()
end event

type st_1 from statictext within w_create_dist_sched
integer x = 1294
integer y = 628
integer width = 402
integer height = 76
boolean bringtotop = true
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long backcolor = 79741120
boolean enabled = false
string text = "Format:  RC44000"
boolean focusrectangle = false
end type


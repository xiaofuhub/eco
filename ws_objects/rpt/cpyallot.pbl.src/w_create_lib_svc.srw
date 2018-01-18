$PBExportHeader$w_create_lib_svc.srw
forward
global type w_create_lib_svc from w_main
end type
type gb_4 from groupbox within w_create_lib_svc
end type
type cb_cancel from commandbutton within w_create_lib_svc
end type
type cb_go from commandbutton within w_create_lib_svc
end type
type sle_date from singlelineedit within w_create_lib_svc
end type
type gb_3 from groupbox within w_create_lib_svc
end type
type gb_2 from groupbox within w_create_lib_svc
end type
type sle_date2 from singlelineedit within w_create_lib_svc
end type
type cb_survey_file from commandbutton within w_create_lib_svc
end type
type cb_card_file from commandbutton within w_create_lib_svc
end type
type st_cfile from u_st within w_create_lib_svc
end type
type gb_1 from groupbox within w_create_lib_svc
end type
type st_sfile from u_st within w_create_lib_svc
end type
end forward

global type w_create_lib_svc from w_main
integer width = 1289
integer height = 904
string title = "Create Library Surveys/Cards"
event ue_create_file ( )
event ue_cancel ( )
event ue_cardfile ( )
event ue_surveyfile ( )
event ue_enterkey pbm_dwnprocessenter
event ue_create_old ( )
gb_4 gb_4
cb_cancel cb_cancel
cb_go cb_go
sle_date sle_date
gb_3 gb_3
gb_2 gb_2
sle_date2 sle_date2
cb_survey_file cb_survey_file
cb_card_file cb_card_file
st_cfile st_cfile
gb_1 gb_1
st_sfile st_sfile
end type
global w_create_lib_svc w_create_lib_svc

forward prototypes
public subroutine wf_error (n_ds lds, long ll_lib_count, long ll_cline_count, long ll_cards_count)
end prototypes

event ue_create_file();n_ds lds
long ll_rows, ll_count, ll_count2
date ld_mydate, ld_mydate2
datetime ld_mydate_dt, ld_mydate2_dt
string ls_header, ls_anno, ls_in, ls_out[], ls_text, ls_anno_array[]
string ls_title, ls_title_array[]
string ls_cfile, ls_sfile
integer li_lines, li_filenum, li_filenum2, li_title_lines, li_anno_lines, ll_pgno, ll_lict,ln_applen
string ls_bkmed, ls_bkseq, ls_auth, ls_authfn, ls_publisher, ls_formfeed
string ls_pubyr, ls_srcdoc, ls_reissue, ls_vols, ls_casub, ls_defqty, ls_libcd
string ls_reissue2, ls_bkmed2, ls_current_lib, ls_prevbkseq, ls_prevbkmed, ls_med
string ls_priority, ls_defonly, ls_applen
boolean lb_need_header
long ll_libcount, ll_libmax, ll_lib_count, ll_cline_count, ll_cards_count

string ls_lib[]

// Initialize the summary statistics variables

ll_lib_count = 0
ll_cline_count = 0
ll_cards_count = 0

// Validate the date

ld_mydate = date(sle_date.text)
ld_mydate_dt=datetime(ld_mydate,time('00:00:00'))
if isnull(ld_mydate) or ld_mydate < date("01/01/1980") then
	messagebox("Invalid Date","Please correct your batch date.")
	return
end if

ld_mydate2 = date(sle_date2.text)
if isnull(ld_mydate2) or ld_mydate2 < date("01/01/1980") then
	messagebox("Invalid Date","Please correct your schedule date.")
	return
end if

// validate the file names

ls_cfile = st_cfile.text
ls_sfile = st_sfile.text

if ls_cfile = "" or ls_cfile = "none" then
	messagebox("Invalid Filename","Please select a location for your card file")
	return
end if

if ls_sfile = "" or ls_sfile = "none" then
	messagebox("Invalid Filename","Please select a location for your survey file")
	return
end if

// get list of libraries

open(w_pics_retrieve_msg_box)
w_pics_retrieve_msg_box.sle_retrieve.text = "Retrieving Library List"

lds = create n_ds
lds.dataobject = "d_ds_pgm83_cardquery_liblist"
lds.settransobject(sqlservertrans)

lds.reset()
ll_rows = lds.retrieve(ld_mydate_dt)
ll_lib_count = ll_rows // count the # of different libs

if ll_rows = -1 then
	wf_error(lds,ll_lib_count,ll_cline_count,ll_cards_count)
	return
end if

// if blank, return

if ll_rows = 0 then
	messagebox("No records found","Please adjust your batch date and try again.")
	wf_error(lds,ll_lib_count,ll_cline_count,ll_cards_count)
	return
end if

for ll_libmax = 1 to ll_rows
  ls_lib[ll_libmax] = trim(lds.object.lib_libcd[ll_libmax])
next

destroy lds

ll_libmax = ll_rows

// open card file

w_pics_retrieve_msg_box.sle_retrieve.text = "Opening Files"
li_filenum = Fileopen(ls_cfile,streammode!,write!,lockwrite!,Replace!)
if li_filenum = -1 then
	messagebox("File Error","the file could not be opened for output.")
	wf_error(lds,ll_lib_count,ll_cline_count,ll_cards_count)
end if

// open survey file

li_filenum2 = Fileopen(ls_sfile,streammode!,write!,lockwrite!,Replace!)
if li_filenum2 = -1 then
	messagebox("File Error","the file could not be opened for output.")
	wf_error(lds,ll_lib_count,ll_cline_count,ll_cards_count)
end if

// create datastore
lds = create n_ds
lds.dataobject = "d_ds_pgm83_cardquery"
lds.settransobject(sqlservertrans)
lds.Object.DataWindow.Table.Data.Storage = "disk"

// Start the card file with a blank line
ls_header = "~n";
ll_cline_count = 1

// for every library

for ll_libcount = 1 to ll_libmax
	// display status
	ls_libcd = trim(ls_lib[ll_libcount])
		
	// retrieve into datastore
	w_pics_retrieve_msg_box.sle_retrieve.text = ls_libcd + ": Reading Library"
	lds.reset()
	ll_rows = lds.retrieve(ld_mydate_dt, ls_libcd)
	if ll_rows = -1 then
		messagebox("Retrieve Error","There was an error during the retrieve")
		wf_error(lds,ll_lib_count,ll_cline_count,ll_cards_count)
		return
	end if
	
	if ll_rows = 0 then
		wf_error(lds,ll_lib_count,ll_cline_count,ll_cards_count)
		return
	end if
	
	w_pics_retrieve_msg_box.sle_retrieve.text = ls_libcd + ": Sorting Records"
	lds.setsort("mchar_bkseq A")
	lds.sort()
	
	// write all cards
	w_pics_retrieve_msg_box.sle_retrieve.text =  ls_libcd + ": Writing Library"
	
	// print the header
		
	ls_header += " ***********************************************~n" + &
	" *                                             *~n" + &
	" *                                             *~n" + &
	" *         LIBRARY:   "+ls_libcd+"                     *~n" + &
	" *                                             *~n" + &
	" *                                             *~n" + &
	" *     LIBRARY OF CONGRESS BOOK CARDS          *~n" + &
	" *             ** NLS/BPH **                   *~n" + &
	" *                                             *~n" + &
	" *                                             *~n" + &
	" *                                             *~n" + &
	" *                                             *~n" + &
	" *                                             *~n" + &
	" *     CARDS ARE ORDERED BY LIBRARY CODE       *~n" + &
	" *            AND BOOK NUMBER                  *~n" + &
	" ***********************************************~n~n"
	ll_cline_count += 17
	ll_cards_count += 1
	filewrite(li_filenum, ls_header)
		
	// print all rows
	
	for ll_count = 1 to ll_rows
		
		// wipe the arrays
		for ll_count2 = 1 to 16
			ls_out[ll_count2] = right("                                                      ",35)
			ls_title_array[ll_count2] = ""
			ls_anno_array[ll_count2] = ""
		next
		
		// put the title into an array
		ls_title = TRIM(string(lds.object.ttlinit_ttl[ll_count]))
		 
		f_word_wrap(ls_title, ls_title_array[], 35, li_title_lines)
		for ll_count2 = 1 to li_title_lines
			ls_text = mid(ls_title_array[ll_count2] + "                                          ",1,35)
			ls_out[ll_count2] = ls_text
		next
		
		// put the annotation into an array
		ls_anno = string(lds.object.annotation_anno[ll_count])
		 
		f_word_wrap(ls_anno, ls_anno_array[], 35, li_anno_lines)
		for ll_count2 = 1 to li_anno_lines
			ls_text = mid(ls_anno_array[ll_count2] + "                                          ",1,35)
			ls_out[ll_count2 + li_title_lines + 1] = ls_text
		next
		
		// lets get all the values we'll need
		ls_bkmed = trim(string(lds.object.mchar_bkmed[ll_count]))
		ls_med = trim(string(lds.object.med[ll_count]))
		ls_bkseq = string(lds.object.mchar_bkseq[ll_count])
		ls_auth = string(lds.object.ttlinit_auth[ll_count])
		ls_authfn = string(lds.object.ttlinit_authfn[ll_count])
		ls_publisher = string(lds.object.ttlinit_publisher[ll_count])
		ls_pubyr = string(lds.object.ttlinit_pubyr[ll_count])
		ls_applen = string(lds.object.mchar_applen[ll_count])
		ls_srcdoc = string(lds.object.ttlinit_srcdoc[ll_count])
		ls_reissue = string(lds.object.mchar_ricd[ll_count])
		ls_vols = string(lds.object.mchar_vols[ll_count])
		ls_casub = string(lds.object.ttlinit_casub[ll_count])
		ls_defqty = string(lds.object.sched_ordqty[ll_count])
		
		ls_prevbkseq = trim(string(lds.object.prevbkseq[ll_count]))
		ls_prevbkmed = trim(string(lds.object.prevbkmed[ll_count]))
		
		// checking for nulls
		
		if isnull(ls_bkmed) then
			ls_bkmed = ""
		end if
		if isnull(ls_med) then
			ls_med = ""
		end if
		if isnull(ls_bkseq) then
			ls_bkseq = ""
		end if
		if isnull(ls_auth) then
			ls_auth = ""
		end if
		if isnull(ls_authfn) then
			ls_authfn = ""
		end if
		if isnull(ls_publisher) then
			ls_publisher = ""
		end if
		if isnull(ls_pubyr) then
			ls_pubyr = ""
		end if
		if isnull(ls_applen) then
			ls_applen = ""
		end if
		if isnull(ls_srcdoc) then
			ls_srcdoc = ""
		end if
		if isnull(ls_casub) then
			ls_casub = ""
		end if
		if isnull(ls_defqty) then
			ls_defqty = ""
		end if
		if isnull(ls_prevbkseq) then
			ls_prevbkseq = ""
		end if
		if isnull(ls_prevbkmed) then
			ls_prevbkmed = ""
		end if
		
		ls_bkmed2 = "RR"
		
		if isnull(ls_vols)=FALSE then
			ln_applen = integer(ls_vols)
			ls_applen = string(ln_applen)
			CHOOSE CASE ls_med
				CASE 'RC'
					ls_bkmed2 = ls_applen + " cass"
				CASE ELSE 
					ls_bkmed2 = ls_applen + " vol"
			END CHOOSE
		else			
			CHOOSE CASE ls_med
				CASE 'RC'
					IF (isnull(ls_applen)=FALSE OR ls_applen<>"") THEN
						ln_applen = integer(ls_applen)
						IF mod(ln_applen,4)>0 THEN
							ln_applen = (ln_applen / 4) + 1 
						ELSE
							ln_applen = ln_applen / 4
						END IF
					ELSE
						ln_applen = 0
					END IF
					ls_applen = string(ln_applen)
					ls_bkmed2 = ls_applen + " cass"
				CASE 'BR'
					IF (isnull(ls_applen)=FALSE OR ls_applen<>"") THEN
						ln_applen = integer(ls_applen)
						IF mod(ln_applen,250)>0 THEN
							ln_applen = (ln_applen / 250) + 1 
						ELSE
							ln_applen = ln_applen / 250
						END IF
					ELSE
						ln_applen = 0
					END IF
					ls_applen = string(ln_applen)
					ls_bkmed2 = ls_applen + " vol"
				CASE 'P/B'
					ls_bkmed2 = "1" + " vol"
				CASE ELSE
					ls_bkmed2 = "0" + " vol"
			END CHOOSE
		end if

		ls_reissue2 = ""
		
		choose case ls_reissue
			case 'RI'
				ls_reissue2 = "REIS: " + ls_prevbkmed + " " + ls_prevbkseq
			case 'RR'
				ls_reissue2 = "RRCD: " + ls_prevbkmed + " " + ls_prevbkseq
		end choose
		
		// format them properly and add them to the lines
		
		ls_out[1] = ls_out[1] + right("            " + ls_bkmed + ls_bkseq, 12)
		ls_out[2] = ls_out[2] + ls_auth
		ls_out[3] = ls_out[3] + ls_authfn
		
		ls_out[5] = ls_out[5] + left(ls_publisher,12)
		ls_out[6] = ls_out[6] + mid(ls_publisher, 13, 12)
		ls_out[7] = ls_out[7] + ls_pubyr
		ls_out[8] = ls_out[8] + mid(ls_srcdoc,1,12)
		ls_out[9] = ls_out[9] + mid(ls_srcdoc,13,12)
		ls_out[10] = ls_out[10] + mid(ls_srcdoc,25,12)
		ls_out[11] = ls_out[11] + ls_reissue2
		ls_out[12] = ls_out[12] + "EST: " + ls_bkmed2
		
		ls_out[14] = ls_out[14] + "CODE: " + ls_casub + "-" + ls_bkmed
		ls_out[15] = ls_out[15] + "DEFAULT: " + ls_defqty
		ls_out[16] = ls_out[16] + right("            " + ls_libcd,12)
		
		// increment the card count
		
		ll_cards_count += 1
	
		// write the output
		for ll_count2 = 1 to 16
			filewrite(li_filenum, " " +ls_out[ll_count2]+"~n")
			// increment the line count
			ll_cline_count += 1
		next
		
		IF (ll_libcount <> ll_libmax) OR (ll_count <> ll_rows) THEN
			Filewrite(li_filenum, " ~n")
			// increment the line count
			ll_cline_count += 1
		END IF
	next
	
	// write all survey
	
	ls_formfeed = "~n1~n"
	
	ls_current_lib = ""
	
	// print the report header if we need to
		
	ll_pgno = 1
	ll_lict = 7

	ls_header = "~n                            SELECTION TALLY SHEET~n~n" + &
	" MUST BE RECEIVED                                    LIBRARY CODE:  "+ls_libcd+ &
	"~n AT NLS BY: "+string(date(sle_date2.text),"mm/dd/yyyy")+"~n~n BATCH DATE:  "+string(date(sle_date.text),"mm/dd/yyyy")+"~n~n"
	
	filewrite(li_filenum2, ls_header)

	lb_need_header = true  // we need the page header
	
	for ll_count = 1 to ll_rows
		
		// lets get all the values we'll need
		ls_bkmed = trim(string(lds.object.mchar_bkmed[ll_count]))
		ls_bkseq = string(lds.object.mchar_bkseq[ll_count])
		ls_defqty = string(lds.object.sched_ordqty[ll_count])
		ls_title = TRIM(string(lds.object.ttlinit_ttl[ll_count]))
		ls_casub = string(lds.object.ttlinit_casub[ll_count])
		ls_med = string(lds.object.med[ll_count])
		ls_priority = string(lds.object.mchar_priority[ll_count])
		
		if lb_need_header = true then
			if ll_pgno = 1 then
				ls_header = " BOOK NUMBER  SELECTION  DEFAULT   TITLE              DISTRIBUTION CODE~n~n"
			else
				ls_header = " BATCH DATE:    "+string(date(sle_date.text),"mm/dd/yyyy")+"    PAGE NUMBER:   "+string(ll_pgno)+"    LIBRARY CODE: "+ls_libcd+"~n~n"
			end if
			filewrite(li_filenum2,ls_header)
			ll_lict = ll_lict + 2
		
			lb_need_header = false
		end if
		
		ls_defonly = "____________"
		if left(ls_med,1) = "F" or left(ls_med,1) = "P" or trim(ls_priority) = "R" then
			ls_defonly = "DEFAULT ONLY"
		end if
		
		ls_text = " " + ls_bkmed + &
			ls_bkseq + " "+ls_defonly+"  " &
			+ right("            "+string(ls_defqty),8) + "    " + &
			left(ls_title+"                                        ",27) + "   " + &
			ls_casub + "-" + trim(ls_med)+"~n"
		filewrite(li_filenum2, ls_text)
		if isnull(ls_text) then
			filewrite(li_filenum2, "NULL!")
		end if
		ll_lict++
		
		if ll_lict = 57 then
			ls_header = "1~n~n"
			filewrite(li_filenum2,ls_header)
			ll_lict = 0
			ll_pgno++
			lb_need_header = true               
		end if

	next
	
	ls_header = "1~n~n"
	filewrite(li_filenum2,ls_header)
	ls_header = ""
	
	w_create_lib_svc.SetRedraw(TRUE)
	w_pics_main.SetRedraw(TRUE)
	
// next library
next

// close files
FileClose(li_filenum)
FileClose(li_filenum2)

			
// close file, all done


end event

event ue_cancel;call super::ue_cancel;close(this)
end event

event ue_cardfile();string ls_filename, ls_path

ls_filename = "cards.txt"
ls_path =  ls_filename
if GetFileSaveName("Select Card File",ls_path,ls_filename, "txt", "Text Files (*.txt),*.txt") <> 1 then
	messagebox("Incorrect file name","Please try again")
	return
end if

st_cfile.text = ls_path
end event

event ue_surveyfile();string ls_filename, ls_path

ls_filename = "survey.txt"
ls_path =  ls_filename
if GetFileSaveName("Select Survey File",ls_path,ls_filename, "txt", "Text Files (*.txt),*.txt") <> 1 then
	messagebox("Incorrect file name","Please try again")
	return
end if

st_sfile.text = ls_path
end event

event ue_enterkey;call super::ue_enterkey;Send(Handle(this),256,9,Long(0,0))

end event

event ue_create_old();n_ds lds
long ll_rows, ll_count, ll_count2
date ld_mydate, ld_mydate2
datetime ld_mydate_dt
string ls_header, ls_anno, ls_in, ls_out[], ls_text, ls_anno_array[]
string ls_title, ls_title_array[]
string ls_cfile, ls_sfile
integer li_lines, li_filenum, li_title_lines, li_anno_lines, ll_pgno, ll_lict
string ls_bkmed, ls_bkseq, ls_auth, ls_authfn, ls_publisher, ls_formfeed
string ls_pubyr, ls_srcdoc, ls_reissue, ls_vols, ls_casub, ls_defqty, ls_libcd
string ls_reissue2, ls_bkmed2, ls_current_lib, ls_prevbkseq, ls_prevbkmed, ls_med
string ls_priority, ls_defonly
boolean lb_need_header


// Validate the date

ld_mydate = date(sle_date.text)
ld_mydate_dt=datetime(ld_mydate,time('00:00:00'))
if isnull(ld_mydate) or ld_mydate < date("01/01/1980") then
	messagebox("Invalid Date","Please correct your batch date.")
	return
end if

ld_mydate2 = date(sle_date2.text)
if isnull(ld_mydate2) or ld_mydate2 < date("01/01/1980") then
	messagebox("Invalid Date","Please correct your schedule date.")
	return
end if

// get files

ls_cfile = st_cfile.text
ls_sfile = st_sfile.text

if ls_cfile = "" or ls_cfile = "none" then
	messagebox("Invalid Filename","Please select a location for your card file")
	return
end if

if ls_sfile = "" or ls_sfile = "none" then
	messagebox("Invalid Filename","Please select a location for your survey file")
	return
end if


// Create and load the datastore

lds = create n_ds
lds.dataobject = "d_ds_pgm83_cardquery"
ll_rows = lds.settransobject(sqlservertrans)
lds.Object.DataWindow.Table.Data.Storage = "disk"
open(w_pics_retrieve_msg_box)

ll_rows = lds.retrieve(ld_mydate_dt)

w_pics_retrieve_msg_box.sle_retrieve.text = "Sorting Records"
lds.setsort("lib_libcd A, mchar_bkseq A")
lds.sort()

if ll_rows = -1 then
	messagebox("Database Error","There was an error during the retrieve.  Please try again.")
	close(w_pics_retrieve_msg_box)
	return
end if

if ll_rows = 0 then
	messagebox("No records found","Please adjust your batch date and try again.")
	close(w_pics_retrieve_msg_box)
	return
end if

w_pics_retrieve_msg_box.sle_retrieve.text = "Writing Data Files"


// Cards.balt - first data file

// open file 

li_filenum = Fileopen(ls_cfile,lineMode!,write!,lockwrite!,Replace!)
if li_filenum = -1 then
	messagebox("File Error","the file could not be opened for output.")
	close(w_pics_retrieve_msg_box)
	return
end if

// keep track of the current library for headers

ls_current_lib = ""

for ll_count = 1 to ll_rows


	// wipe the arrays
	for ll_count2 = 1 to 16
		ls_out[ll_count2] = right("                                                      ",35)
		ls_title_array[ll_count2] = ""
		ls_anno_array[ll_count2] = ""
	next
	
	// put the title into an array
	ls_title = string(lds.object.ttlinit_ttl[ll_count])
	 
	f_word_wrap(ls_title, ls_title_array[], 35, li_title_lines)
	for ll_count2 = 1 to li_title_lines
		ls_text = mid(ls_title_array[ll_count2] + "                                          ",1,35)
		ls_out[ll_count2] = ls_text
	next
	

	// put the annotation into an array
	ls_anno = string(lds.object.annotation_anno[ll_count])
	 
	f_word_wrap(ls_anno, ls_anno_array[], 35, li_anno_lines)
	for ll_count2 = 1 to li_anno_lines
		ls_text = mid(ls_anno_array[ll_count2] + "                                          ",1,35)
		ls_out[ll_count2 + li_title_lines + 1] = ls_text
	next
	
	// lets get all the values we'll need
	ls_bkmed = trim(string(lds.object.mchar_bkmed[ll_count]))
	ls_med = string(lds.object.med[ll_count])
	ls_bkseq = string(lds.object.mchar_bkseq[ll_count])
	ls_auth = string(lds.object.ttlinit_auth[ll_count])
	ls_authfn = string(lds.object.ttlinit_authfn[ll_count])
	ls_publisher = string(lds.object.ttlinit_publisher[ll_count])
	ls_pubyr = string(lds.object.ttlinit_pubyr[ll_count])
	ls_srcdoc = string(lds.object.ttlinit_srcdoc[ll_count])
	ls_reissue = string(lds.object.mchar_ricd[ll_count])
	ls_vols = string(lds.object.mchar_vols[ll_count])
	ls_casub = string(lds.object.ttlinit_casub[ll_count])
	ls_defqty = string(lds.object.sched_ordqty[ll_count])
	ls_libcd = trim(lds.object.lib_libcd[ll_count])
	ls_prevbkseq = trim(string(lds.object.prevbkseq[ll_count]))
	ls_prevbkmed = trim(string(lds.object.prevbkmed[ll_count]))
	
	// checking for nulls
	
	if isnull(ls_pubyr) then
		ls_pubyr = ""
	end if
	
	ls_bkmed2 = "RR"
	
	CHOOSE CASE ls_med
		CASE 'RC'
			ls_bkmed2 = " cass"
		CASE 'BR'
			ls_bkmed2 = " vol"
		CASE 'P/B'
			ls_bkmed2 = " vol"
		CASE 'FD'
			ls_bkmed2 = " disk"
		CASE ELSE
			ls_bkmed2 = " vol"
	END CHOOSE
	
	ls_reissue2 = ""
	
	choose case ls_reissue
		case 'RI'
			ls_reissue2 = "REIS: " + ls_prevbkmed + " " + ls_prevbkseq
		case 'RR'
			ls_reissue2 = "RRCD: " + ls_prevbkmed + " " + ls_prevbkseq
	end choose
	
	// format them properly and add them to the lines
	
	ls_out[1] = ls_out[1] + right("            " + ls_bkmed + ls_bkseq, 12)
	ls_out[2] = ls_out[2] + ls_auth
	ls_out[3] = ls_out[3] + ls_authfn
	
	ls_out[5] = ls_out[5] + left(ls_publisher,12)
	ls_out[6] = ls_out[6] + mid(ls_publisher, 13, 12)
	ls_out[7] = ls_out[7] + ls_pubyr
	ls_out[8] = ls_out[8] + mid(ls_srcdoc,1,12)
	ls_out[9] = ls_out[9] + mid(ls_srcdoc,13,12)
	ls_out[10] = ls_out[10] + mid(ls_srcdoc,25,12)
	ls_out[11] = ls_out[11] + ls_reissue2
	ls_out[12] = ls_out[12] + "EST: " + ls_vols + ls_bkmed2
	
	ls_out[14] = ls_out[14] + "CODE: " + ls_casub + "-" + ls_bkmed
	ls_out[15] = ls_out[15] + "DEFAULT: " + ls_defqty
	ls_out[16] = ls_out[16] + right("            " + ls_libcd,12)

	// print the header if we need to
	
	if ls_current_lib <> ls_libcd then
		ls_header = " ***********************************************~n" + &
		" *                                             *~n" + &
		" *                                             *~n" + &
		" *         LIBRARY:   "+ls_libcd+"                     *~n" + &
		" *                                             *~n" + &
		" *                                             *~n" + &
		" *     LIBRARY OF CONGRESS BOOK CARDS          *~n" + &
		" *             ** NLS/BPH **                   *~n" + &
		" *                                             *~n" + &
		" *                                             *~n" + &
		" *                                             *~n" + &
		" *                                             *~n" + &
		" *                                             *~n" + &
		" *     CARDS ARE ORDERED BY LIBRARY CODE       *~n" + &
		" *            AND BOOK NUMBER                  *~n" + &
		" ***********************************************~n"
		filewrite(li_filenum, ls_header)
		ls_current_lib = ls_libcd
   end if


		// write the output
	for ll_count2 = 1 to 16
		filewrite(li_filenum, " " +ls_out[ll_count2])
	next
	
   Filewrite(li_filenum, "~n")
next

// close file, all done

FileClose(li_filenum)

// second file

ls_formfeed = "~n1~n"

li_filenum = Fileopen(ls_sfile,lineMode!,write!,lockwrite!,Replace!)
if li_filenum = -1 then
	messagebox("File Error","the file could not be opened for output.")
	close(w_pics_retrieve_msg_box)
	return
end if

ls_current_lib = ""

for ll_count = 1 to ll_rows
	
	// lets get all the values we'll need
	ls_bkmed = trim(string(lds.object.mchar_bkmed[ll_count]))
	ls_bkseq = string(lds.object.mchar_bkseq[ll_count])
	ls_defqty = string(lds.object.sched_ordqty[ll_count])
	ls_title = string(lds.object.ttlinit_ttl[ll_count])
	ls_casub = string(lds.object.ttlinit_casub[ll_count])
	ls_libcd = trim(lds.object.lib_libcd[ll_count])
	ls_med = string(lds.object.med[ll_count])
	ls_priority = string(lds.object.mchar_priority[ll_count])
	

	// print the report header if we need to
	
	if ls_current_lib <> ls_libcd then
		
		ll_pgno = 1
		ll_lict = 7
		
		ls_header = "~n                            SELECTION TALLY SHEET~n~n" + &
		" MUST BE RECEIVED                                    LIBRARY CODE:  "+ls_libcd+ &
		"~n AT NLS BY: "+string(date(sle_date2.text))+"~n~n BATCH DATE:  "+string(date(sle_date.text))+"~n"
		
		if ls_current_lib <>"" then //must not be the first one so add formfeed
		  filewrite(li_filenum,ls_formfeed)
		end if

		filewrite(li_filenum, ls_header)
		ls_current_lib = ls_libcd
		lb_need_header = true  // we need the page header
   end if
	
	if lb_need_header = true then
		if ll_pgno = 1 then
			ls_header = " BOOK NUMBER  SELECTION  DEFAULT   TITLE              DISTRIBUTION CODE~n"
		else
			ls_header = " BATCH DATE:    "+string(date(sle_date.text))+"    PAGE NUMBER:   "+string(ll_pgno)+"    LIBRARY CODE: "+ls_libcd+"~n"
		end if
		filewrite(li_filenum,ls_header)
		ll_lict = ll_lict + 2
		lb_need_header = false
	end if
	
	ls_defonly = "____________"
	if left(ls_med,1) = "F" or left(ls_med,1) = "P" or trim(ls_priority) = "R" then
		ls_defonly = "DEFAULT ONLY"
	end if
	
	ls_text = " " + ls_bkmed + &
		ls_bkseq + " "+ls_defonly+"  " &
		+ right("            "+string(ls_defqty),8) + "       " + &
		left(ls_title+"                                        ",27) + "   " + &
		ls_casub + "-" + ls_med+" "
	filewrite(li_filenum, ls_text)
	if isnull(ls_text) then
		filewrite(li_filenum, "NULL!")
	end if
	ll_lict++
	
	if ll_lict = 57 then
		ls_header = "1~n~r"
		filewrite(li_filenum,ls_header)
		ll_lict = 0
		ll_pgno++
		lb_need_header = true
	end if
		
next

ls_header = "1~n~r"
filewrite(li_filenum,ls_header)

fileclose(li_filenum)


// close file, all done

close(w_pics_retrieve_msg_box)

return



end event

public subroutine wf_error (n_ds lds, long ll_lib_count, long ll_cline_count, long ll_cards_count);// changed goto to function unsupported appeon feature 3/24/2010
if isvalid(lds) then
	destroy lds
end if

if isvalid(w_pics_retrieve_msg_box) then
  close(w_pics_retrieve_msg_box)
end if

messageBox('Card/Survey File', 'Cards and survey files successfully extracted.' + '~n' + 'Total number of libraries that'+&
			' will receive cards: '+string(ll_lib_count)+'~n'+&
			'Total number of lines in cards file: '+string(ll_cline_count)+'~n'+&
			'Total number of cards: '+string(ll_cards_count) )
		
return

end subroutine

on w_create_lib_svc.create
int iCurrent
call super::create
this.gb_4=create gb_4
this.cb_cancel=create cb_cancel
this.cb_go=create cb_go
this.sle_date=create sle_date
this.gb_3=create gb_3
this.gb_2=create gb_2
this.sle_date2=create sle_date2
this.cb_survey_file=create cb_survey_file
this.cb_card_file=create cb_card_file
this.st_cfile=create st_cfile
this.gb_1=create gb_1
this.st_sfile=create st_sfile
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.gb_4
this.Control[iCurrent+2]=this.cb_cancel
this.Control[iCurrent+3]=this.cb_go
this.Control[iCurrent+4]=this.sle_date
this.Control[iCurrent+5]=this.gb_3
this.Control[iCurrent+6]=this.gb_2
this.Control[iCurrent+7]=this.sle_date2
this.Control[iCurrent+8]=this.cb_survey_file
this.Control[iCurrent+9]=this.cb_card_file
this.Control[iCurrent+10]=this.st_cfile
this.Control[iCurrent+11]=this.gb_1
this.Control[iCurrent+12]=this.st_sfile
end on

on w_create_lib_svc.destroy
call super::destroy
destroy(this.gb_4)
destroy(this.cb_cancel)
destroy(this.cb_go)
destroy(this.sle_date)
destroy(this.gb_3)
destroy(this.gb_2)
destroy(this.sle_date2)
destroy(this.cb_survey_file)
destroy(this.cb_card_file)
destroy(this.st_cfile)
destroy(this.gb_1)
destroy(this.st_sfile)
end on

type gb_4 from groupbox within w_create_lib_svc
integer x = 23
integer y = 492
integer width = 1207
integer height = 148
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long backcolor = 79741120
string text = "Survey Filename:"
end type

type cb_cancel from commandbutton within w_create_lib_svc
integer x = 983
integer y = 668
integer width = 247
integer height = 108
integer taborder = 60
boolean bringtotop = true
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "&Cancel"
boolean cancel = true
end type

event clicked;Parent.triggerevent("ue_cancel")
end event

type cb_go from commandbutton within w_create_lib_svc
integer x = 690
integer y = 668
integer width = 247
integer height = 108
integer taborder = 50
boolean bringtotop = true
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "&Go"
end type

event clicked;Parent.triggerevent("ue_create_file")
close (w_create_lib_svc)
end event

type sle_date from singlelineedit within w_create_lib_svc
integer x = 50
integer y = 108
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

type gb_3 from groupbox within w_create_lib_svc
integer x = 640
integer y = 48
integer width = 590
integer height = 148
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long backcolor = 79741120
string text = "Survey Due Date:"
end type

type gb_2 from groupbox within w_create_lib_svc
integer x = 23
integer y = 48
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

type sle_date2 from singlelineedit within w_create_lib_svc
integer x = 658
integer y = 108
integer width = 530
integer height = 64
integer taborder = 20
boolean bringtotop = true
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
boolean autohscroll = false
borderstyle borderstyle = stylelowered!
end type

type cb_survey_file from commandbutton within w_create_lib_svc
event clicked pbm_bnclicked
integer x = 1083
integer y = 548
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

event clicked;Parent.triggerevent("ue_surveyfile")
end event

type cb_card_file from commandbutton within w_create_lib_svc
event clicked pbm_bnclicked
integer x = 1079
integer y = 328
integer width = 119
integer height = 68
integer taborder = 30
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "Set"
end type

event clicked;Parent.triggerevent("ue_cardfile")
end event

type st_cfile from u_st within w_create_lib_svc
integer x = 46
integer y = 328
integer width = 1010
long backcolor = 1090519039
boolean border = true
borderstyle borderstyle = stylelowered!
end type

type gb_1 from groupbox within w_create_lib_svc
integer x = 23
integer y = 268
integer width = 1207
integer height = 148
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long backcolor = 79741120
string text = "Card Filename:"
end type

type st_sfile from u_st within w_create_lib_svc
integer x = 46
integer y = 552
integer width = 1010
boolean bringtotop = true
long backcolor = 1090519039
boolean border = true
borderstyle borderstyle = stylelowered!
end type


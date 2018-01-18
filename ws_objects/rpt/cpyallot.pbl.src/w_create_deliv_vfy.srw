$PBExportHeader$w_create_deliv_vfy.srw
forward
global type w_create_deliv_vfy from w_main
end type
type st_cfile from u_st within w_create_deliv_vfy
end type
type cb_cancel from commandbutton within w_create_deliv_vfy
end type
type cb_go from commandbutton within w_create_deliv_vfy
end type
type cb_card_file from commandbutton within w_create_deliv_vfy
end type
type sle_date from singlelineedit within w_create_deliv_vfy
end type
type gb_2 from groupbox within w_create_deliv_vfy
end type
type gb_1 from groupbox within w_create_deliv_vfy
end type
end forward

global type w_create_deliv_vfy from w_main
integer width = 2610
integer height = 332
string title = "Create Delivery Verification File"
event ue_create_file ( )
event ue_cancel ( )
event ue_cardfile ( )
event ue_enterkey pbm_dwnprocessenter
st_cfile st_cfile
cb_cancel cb_cancel
cb_go cb_go
cb_card_file cb_card_file
sle_date sle_date
gb_2 gb_2
gb_1 gb_1
end type
global w_create_deliv_vfy w_create_deliv_vfy

event ue_create_file();n_ds lds
long ll_rows, ll_count, ll_count2, ll_bks_with_shiprpt=0
date ld_mydate, ld_mydate2, ld_maxactenddt

string ls_header, ls_anno, ls_in, ls_out[], ls_text, ls_anno_array[]
string ls_title, ls_title_array[]
string ls_cfile, ls_sfile
integer li_lines, li_filenum, li_title_lines, li_anno_lines, ll_pgno, ll_lict
string ls_bkmed, ls_bkseq, ls_auth, ls_authfn, ls_publisher, ls_formfeed
string ls_pubyr, ls_srcdoc, ls_reissue, ls_vols, ls_casub,  ls_libcd
string ls_reissue2, ls_bkmed2, ls_current_bkno, ls_prevbkseq, ls_prevbkmed, ls_med
string ls_prdr, ls_cabdt, ls_lastlibcd, ls_libs[200], ls_text1, ls_text2, ls_defqty, ls_actenddt
long ll_defqty, ll_libtotal, ll_libs[200], ll_libcnt, ll_index, ll_index2, ll_bktotal
long ll_pages, ll_pageno, ll_max
boolean lb_need_header
string ls_current_lib
date ld_one, ld_two, ld_three, ld_four
datetime ldt_mydate, ldt_three
string ls_date_one, ls_date_two, ls_date_three, ls_date_four

// Validate the date

ld_mydate = date(sle_date.text)
if isnull(ld_mydate) or ld_mydate < date("01/01/1980") then
	messagebox("Invalid Date","Please correct your cutoff date.")
	return
end if

// We need three dates

ld_three = ld_mydate
ld_four = today()

// Now three date strings

ls_date_three = f_monthname(month(ld_three)) + string(ld_three," yyyy")
ls_date_four = string(ld_four, "mm/dd/yyyy")

// get files

ls_cfile = st_cfile.text


if ls_cfile = "" or ls_cfile = "none" then
	messagebox("Invalid Filename","Please select a location for your card file")
	return
end if


// Create and load the datastore

lds = create n_ds
lds.dataobject = "d_ds_pgm86_deliv_vfy"
lds.Object.DataWindow.Table.Data.Storage = "disk"
ll_rows = lds.settransobject(sqlservertrans)
open(w_pics_retrieve_msg_box)
ldt_mydate=datetime(ld_mydate,time('00:00:00'))
ll_rows = lds.retrieve(ldt_mydate)

if ll_rows = -1 then
	messagebox("Database Error","There was an error during the retrieve.  Please try again.")
	close(w_pics_retrieve_msg_box)
	return
end if

if ll_rows = 0 then
	messagebox("No records found","Please adjust your cutoff date and try again.")
	close(w_pics_retrieve_msg_box)
	return
end if

ld_maxactenddt = date(lds.Object.max_actenddt[1])

ld_one = relativedate(ld_maxactenddt,60)
ld_two = relativedate(ld_maxactenddt,90)

ls_date_one = f_monthname(month(ld_one)) + string(ld_one," dd, yyyy")
ls_date_two = f_monthname(month(ld_two)) + string(ld_two," dd, yyyy")

w_pics_retrieve_msg_box.sle_retrieve.text = "Sorting Records"

lds.setsort("lib_libcd A, mchar_bkseq A, mchar_bkmed A")
lds.sort()


w_pics_retrieve_msg_box.sle_retrieve.text = "Writing Data Files"


// Distrib.calt - write data file

ls_formfeed = "1"

li_filenum = Fileopen(ls_cfile,streammode!,write!,lockwrite!,Replace!)
if li_filenum = -1 then
	messagebox("File Error","the file could not be opened for output.")
	close(w_pics_retrieve_msg_box)
	return
end if

ls_current_lib = ""
ll_index = 0  // ll_index is the line of the page we're on


for ll_count = 1 to ll_rows
	
	// lets get all the values we'll need
	ls_prdr = trim(string(lds.object.ancntr_prdr[ll_count]))
	ls_bkmed = trim(string(lds.object.mchar_bkmed[ll_count]))
	ls_bkseq = trim(string(lds.object.mchar_bkseq[ll_count]))
	ls_defqty = string(lds.object.sched_ordqty[ll_count])
	ls_casub = string(lds.object.ttlinit_casub[ll_count])
	ls_libcd = trim(lds.object.lib_libcd[ll_count])
	ls_title = string(lds.object.ttlinit_ttl[ll_count])
	ls_auth = string(lds.object.ttlinit_auth[ll_count])
	ls_actenddt = string(date(lds.object.prod_actenddt[ll_count]),'mm/dd/yyyy')
	
	// new library?  If so write header
	
	if ls_current_lib <> ls_libcd then
		if ll_pageno/2 <> int(ll_pageno/2) then  // odd number of pages
			ls_header = "1~n"
			filewrite(li_filenum,ls_header)
		end if
		ls_header = "&
1~n&
                                   National Library Service for the Blind and Physically Handicapped~n&
                                                     DELIVERY VERIFICATION REPORT~n&
     TO: Multistate Centers                                                                     Date sent to MSC:________________  ~n&
~n&
 *** Please send this report to your MSC not later than "+ls_date_two+",~n&
 *** and not before "+ls_date_one+", even if no shortage is reported.~n&
~n&
 Report date: "+ls_date_four+"                               TITLES SHIPPED IN  "+ls_date_three+"~n&
                                                    or Earlier and Not Yet Reported          _____________________________________~n&
                                                                                                 (Name and telephone # of person~n&
                                                                                               responsible for report preparation~n&
~n&
                                                                                                             For Multistate Centers~n&
     Library code:  "+ls_libcd+"                                                                                               Only~n&
                                                                                             DIFF        TOTAL SENT      TOTAL SENT~n&
                                                                 SUBJ   DATE         QNTY    QNTY        BY MSCE         BY MSCW~n&
     PRDR   BOOKNO     TITLE                       AUTHOR        CAT    SHIPPED      ORDRD   RCVD        TOTAL DATE      TOTAL DATE~n~n&
"
		filewrite(li_filenum,ls_header)
		ll_index = 0
		ll_max = 38
		ll_pageno = 1
		ls_current_lib = ls_libcd
	end if
		
	ll_index++
	
	if ll_index = ll_max then
		ll_index = 1
		ll_max = 48
		ll_pageno++
		ls_header = "~n&
1~n&
                                                                                                            page: "+string(ll_pageno)+"~n&
~n&
                                                                                               For Multistate Centers~n&
     Library code:  "+ls_libcd+"                                                                               Only~n&
                                                                                             DIFF        TOTAL SENT      TOTAL SENT~n&
                                                                 SUBJ   DATE         QNTY    QNTY        BY MSCE         BY MSCW~n&
     PRDR   BOOKNO     TITLE                       AUTHOR        CAT    SHIPPED      ORDRD   RCVD        TOTAL DATE      TOTAL DATE~n~n"


		filewrite(li_filenum,ls_header)
	end if
		
	// build and write the line
	
	ls_text = "     " + left(trim(ls_prdr)+"     ",5) + "  " + ls_bkmed + ls_bkseq + "    " + &
	left(ls_title+"                            ",27) + " " + &
	left(ls_auth +"             ",13) + " " +&
				left(ls_casub + "   ",3) + "  " + string(date(ls_actenddt),"mm/dd/yyyy") + &
				" " + right("       "+ ls_defqty,6) + &
				"      _____       _____ _____  |  _____ _____~n"
	filewrite(li_filenum,ls_text)
		
next
		
ls_header = "1~n~n"
filewrite(li_filenum,ls_header)

fileclose(li_filenum)

// do the update

w_pics_retrieve_msg_box.sle_retrieve.text = "Updating Database"

ldt_three=datetime(ld_three,time('00:00:00'))
execute immediate "lock table mchar in exclusive mode" using sqlservertrans;

update mchar
set shiprptdt = :ldt_three
where mchar.shiprptdt is null
        and mchar.arflag is null
        and mchar.bkseq in 
          (select prod.bkseq from prod where prod.actenddt <= :ldt_three
              and prodstage in ('DU', 'PB', 'PR' ) )
        and mchar.bkmed in 
           (select prod.bkmed from prod where prod.actenddt <= :ldt_three
              and prodstage in ('DU', 'PB', 'PR' ) ) 
USING SQLserverTrans;

IF f_check_dberror(SQLServerTrans,"MCHAR") THEN
	commit using sqlservertrans;
	// Get number or rows updated.
	select count(*) into :ll_bks_with_shiprpt
	from mchar
	where shiprptdt = :ldt_three
	USING SQLserverTrans;
END IF

execute immediate "unlock table mchar" using sqlservertrans;

// close file, all done

close(w_pics_retrieve_msg_box)
messagebox('Delivery Verification File','Successfully Created.'+ &
'~nNumber of books updated with new shiprptdt value = '+string(ll_bks_with_shiprpt))
return
end event

event ue_cancel;call super::ue_cancel;close(this)
end event

event ue_cardfile();string ls_filename, ls_path

ls_filename = "deliver.txt"
ls_path =  ls_filename
if GetFileSaveName("Select Card File",ls_path,ls_filename, "txt", "Text Files (*.txt),*.txt") <> 1 then
	messagebox("Incorrect file name","Please try again")
	return
end if

st_cfile.text = ls_path
end event

event ue_enterkey;call super::ue_enterkey;Send(Handle(this),256,9,Long(0,0))
return(1)
end event

on w_create_deliv_vfy.create
int iCurrent
call super::create
this.st_cfile=create st_cfile
this.cb_cancel=create cb_cancel
this.cb_go=create cb_go
this.cb_card_file=create cb_card_file
this.sle_date=create sle_date
this.gb_2=create gb_2
this.gb_1=create gb_1
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_cfile
this.Control[iCurrent+2]=this.cb_cancel
this.Control[iCurrent+3]=this.cb_go
this.Control[iCurrent+4]=this.cb_card_file
this.Control[iCurrent+5]=this.sle_date
this.Control[iCurrent+6]=this.gb_2
this.Control[iCurrent+7]=this.gb_1
end on

on w_create_deliv_vfy.destroy
call super::destroy
destroy(this.st_cfile)
destroy(this.cb_cancel)
destroy(this.cb_go)
destroy(this.cb_card_file)
destroy(this.sle_date)
destroy(this.gb_2)
destroy(this.gb_1)
end on

type st_cfile from u_st within w_create_deliv_vfy
integer x = 722
integer y = 100
integer width = 1010
long backcolor = 1090519039
boolean border = true
borderstyle borderstyle = stylelowered!
end type

type cb_cancel from commandbutton within w_create_deliv_vfy
integer x = 2295
integer y = 80
integer width = 247
integer height = 108
integer taborder = 40
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

type cb_go from commandbutton within w_create_deliv_vfy
integer x = 1984
integer y = 80
integer width = 247
integer height = 108
integer taborder = 30
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

type cb_card_file from commandbutton within w_create_deliv_vfy
integer x = 1755
integer y = 100
integer width = 119
integer height = 68
integer taborder = 20
boolean bringtotop = true
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "Set"
end type

event clicked;Parent.triggerevent("ue_cardfile")
end event

type sle_date from singlelineedit within w_create_deliv_vfy
integer x = 50
integer y = 100
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

type gb_2 from groupbox within w_create_deliv_vfy
integer x = 23
integer y = 40
integer width = 608
integer height = 148
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long backcolor = 79741120
string text = "Cutoff Date:"
end type

type gb_1 from groupbox within w_create_deliv_vfy
integer x = 699
integer y = 40
integer width = 1207
integer height = 148
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long backcolor = 79741120
string text = "Schedule Filename:"
end type


$PBExportHeader$w_bcs_data_extract.srw
forward
global type w_bcs_data_extract from w_response
end type
type cbx_cutover from u_cbx within w_bcs_data_extract
end type
type cb_extract from u_cb within w_bcs_data_extract
end type
type cb_exit from u_cb within w_bcs_data_extract
end type
type cb_bcs1 from commandbutton within w_bcs_data_extract
end type
type st_1 from u_st within w_bcs_data_extract
end type
type st_file1 from u_st within w_bcs_data_extract
end type
type gb_bcs1 from groupbox within w_bcs_data_extract
end type
type gb_bcs2 from groupbox within w_bcs_data_extract
end type
type cb_bcs2 from commandbutton within w_bcs_data_extract
end type
type st_2 from u_st within w_bcs_data_extract
end type
type st_file2 from u_st within w_bcs_data_extract
end type
type cb_clear from commandbutton within w_bcs_data_extract
end type
type cbx_bcs2 from u_cbx within w_bcs_data_extract
end type
type cbx_bcs1 from u_cbx within w_bcs_data_extract
end type
end forward

global type w_bcs_data_extract from w_response
integer x = 434
integer y = 412
integer width = 1897
integer height = 1088
string title = "BCS Data Extract"
event ue_bcs1_extract ( )
event ue_bcs2_extract ( )
event ue_filename ( )
cbx_cutover cbx_cutover
cb_extract cb_extract
cb_exit cb_exit
cb_bcs1 cb_bcs1
st_1 st_1
st_file1 st_file1
gb_bcs1 gb_bcs1
gb_bcs2 gb_bcs2
cb_bcs2 cb_bcs2
st_2 st_2
st_file2 st_file2
cb_clear cb_clear
cbx_bcs2 cbx_bcs2
cbx_bcs1 cbx_bcs1
end type
global w_bcs_data_extract w_bcs_data_extract

forward prototypes
public subroutine wf_bcs_data_extract_reset ()
end prototypes

event ue_bcs1_extract();n_ds lds
Integer li_filenum, li_exfile
String ls_prev_conno, ls_prev_prodstage, ls_narrstr, ls_coauthstr, ls_co, ls_narr, ls_return
Long ll_rows, ll_count, ll_conno_match, ll_rec
String ls_file, ls_path, ls_data
str_bcs_data_extract lstr_bcs_data_extract[]
Long ll_nomatch=0, ll_match=0, ll_len

// Get the filename

ls_file = st_file1.text

IF ls_file = "" OR ls_file = "none" THEN
	Messagebox("Invalid Filename","Please select a location for your extract file")
	RETURN
END IF

// Create and load the datastore

lds = CREATE n_ds
lds.dataObject = "d_bcs1_data_extract"
ll_rows = lds.SetTransObject(SqlServerTrans)
Open(w_pics_retrieve_msg_box)
ll_rows = lds.Retrieve()

SetPointer(hourglass!)

IF ll_rows = -1 THEN
	Close(w_pics_retrieve_msg_box)
	Messagebox("Database Error","There was an error during the retrieve. Please try again.")
	SetPointer(arrow!)
	wf_bcs_data_extract_reset()
	RETURN
END IF

IF ll_rows = 0 THEN
	Close(w_pics_retrieve_msg_box)
	Messagebox("No Data","No records qualify. Please check and run this extract again.")
	SetPointer(arrow!)
	wf_bcs_data_extract_reset()
	RETURN
END IF


// Copy the rows to a structure so we can work with it

lstr_bcs_data_extract[] = lds.object.Data

// Pass one, combine the duplicates
// Go through the array, and find rows with duplicate conno
// When we find them, get the actenddt from the MAstering row 
// and get the prdr from the DUplication or the PRessing row


ls_prev_conno = "none"
ls_prev_prodstage = "none"
ll_conno_match = 1
FOR ll_count=1 TO ll_rows
	
	IF (lstr_bcs_data_extract[ll_count].conno = ls_prev_conno) THEN
		// it's a match so get to work
		
		IF (lstr_bcs_data_extract[ll_count].bkmed = 'BR') THEN
		//For braille books, check MA and PR rows....
			IF ( (lstr_bcs_data_extract[ll_count].prodstage = 'PR' OR  lstr_bcs_data_extract[ll_count].prodstage = 'EM') AND &
				(ls_prev_prodstage = 'MA' OR ls_prev_prodstage = 'AB')) THEN
			//if a Pressing record is found, check to see if the
			//previous record was a Mastering record
				lstr_bcs_data_extract[ll_count - 1].prdr = lstr_bcs_data_extract[ll_count].prdr
			END IF
		ELSE
		//For others, check MA and DU rows....	
			IF ((lstr_bcs_data_extract[ll_count].prodstage = 'MA' OR lstr_bcs_data_extract[ll_count].prodstage = 'AB') AND ls_prev_prodstage = 'DU') THEN
			//if a Mastering record is found, check to see if the
			//previous record was a Duplication record
				lstr_bcs_data_extract[ll_count - 1].actenddt = lstr_bcs_data_extract[ll_count].actenddt
			END IF
		END IF
	
		// mark it as dead because we don't want it written to the extract file
		lstr_bcs_data_extract[ll_count].live = FALSE
		ll_match++

	ELSE 
		// Not a match so keep it live and record the information
		// in case the next one's a match
		lstr_bcs_data_extract[ll_count].live = TRUE
		ls_prev_conno = lstr_bcs_data_extract[ll_count].conno
		ls_narrstr = Trim(lstr_bcs_data_extract[ll_count].narr) + ', ' + Trim(lstr_bcs_data_extract[ll_count].narrfn)
		lstr_bcs_data_extract[ll_count].narrstr = ls_narrstr
		ll_conno_match = ll_count
		ll_nomatch++
	END IF
	ls_prev_prodstage = lstr_bcs_data_extract[ll_count].prodstage
NEXT

// open the extract file
Open(w_pics_retrieve_msg_box)
w_pics_retrieve_msg_box.sle_retrieve.text = "Writing Extract File..."

ll_rec = 0
li_filenum = FileOpen(ls_file,streamMode!,write!,lockWrite!,replace!)
IF li_filenum = -1 THEN
	Close(w_pics_retrieve_msg_box)
	Messagebox("File Error","the file "+ls_file+" could not be opened for output.")
	SetPointer(arrow!)
	wf_bcs_data_extract_reset()
	RETURN
END IF


// Pass two, for each live record, run the f_export function

FOR ll_count=1 TO ll_rows
	IF (lstr_bcs_data_extract[ll_count].live = TRUE) THEN
		ls_return = f_bcs_data_export(lstr_bcs_data_extract[ll_count], 1)	
		FileWrite(li_filenum, ls_return)
		ll_rec++
	END IF
NEXT

// close file, all done

Close(w_pics_retrieve_msg_box)
FileClose(li_filenum)
SetPointer(arrow!)

Messagebox("Extract Completed", String(ll_rec)+ " records written to file " + ls_file)

// Destroy the datastore 

DESTROY lds


// return the screen to the original state
wf_bcs_data_extract_reset()

RETURN

end event

event ue_bcs2_extract();n_ds lds
integer li_filenum, li_exfile
string ls_prev_conno, ls_prev_prodstage, ls_narrstr, ls_coauthstr, ls_co, ls_narr, ls_return
long ll_rows, ll_count, ll_conno_match, ll_rec
string ls_file, ls_path, ls_data
long ll_nomatch=0, ll_match=0, ll_len



// Get the filename

ls_file = st_file2.text

if ls_file = "" or ls_file = "none" then
	messagebox("Invalid Filename","Please select a location for your extract file")
	wf_bcs_data_extract_reset()
	return
end if

// Create and load the datastore

lds = create n_ds

IF cbx_cutover.Checked=TRUE THEN
	str_bcs3_data_extract lstr_bcs3_data_extract[]
	lds.dataobject = "d_bcs3_data_extract"
ELSE	
	str_bcs_data_extract lstr_bcs_data_extract[]
	lds.dataobject = "d_bcs2_data_extract"
END IF

ll_rows = lds.settransobject(sqlservertrans)
open(w_pics_retrieve_msg_box)
ll_rows = lds.retrieve()

setpointer(hourglass!)

if ll_rows = -1 then
	close(w_pics_retrieve_msg_box)
	messagebox("Database Error","There was an error during the retrieve. Please try again.")
	setpointer(arrow!)
	wf_bcs_data_extract_reset()
	return
end if

if ll_rows = 0 then
	close(w_pics_retrieve_msg_box)
	IF cbx_cutover.Checked=TRUE THEN
		messagebox("No Records","No records qualified for cutover.")
	ELSE
		messagebox("No Records","No records qualify. Please check and run this extract again.")
	END IF
	setpointer(arrow!)
	wf_bcs_data_extract_reset()
	return
end if


// Copy the rows to a structure so we can work with it

IF cbx_cutover.Checked=TRUE THEN
	lstr_bcs3_data_extract[] = lds.object.data
ELSE	
	lstr_bcs_data_extract[] = lds.object.data
END IF

// Pass one, combine the duplicates
// Go through the array, and find rows with duplicate conno
// When we find them, get the actenddt from the MAstering row 
// and get the prdr from the DUplication or the PRessing row


ls_prev_conno = "none"
ls_prev_prodstage = "none"
ll_conno_match = 1

IF cbx_cutover.Checked=TRUE THEN
	FOR ll_count=1 TO ll_rows
		
		if (lstr_bcs3_data_extract[ll_count].conno = ls_prev_conno) then
			// it's a match so get to work			
		
			// mark it as dead because we don't want it written to the extract file
			lstr_bcs3_data_extract[ll_count].live = false
			ll_match++
	
		else 
			// Not a match so keep it live and record the information
			// in case the next one's a match
			lstr_bcs3_data_extract[ll_count].live = true
			ls_prev_conno = lstr_bcs3_data_extract[ll_count].conno
			ll_conno_match = ll_count
			ll_nomatch++
		end if
	NEXT
ELSE	
	FOR ll_count=1 TO ll_rows
		
		if (lstr_bcs_data_extract[ll_count].conno = ls_prev_conno) then
			// it's a match so get to work
			
			if (lstr_bcs_data_extract[ll_count].bkmed = 'BR') then
			//For braille books, check MA and PR rows....
				if (lstr_bcs_data_extract[ll_count].prodstage = 'PR' and (ls_prev_prodstage = 'MA' OR ls_prev_prodstage = 'AB')) then
				//if a Pressing record is found, check to see if the
				//previous record was a Mastering record
					lstr_bcs_data_extract[ll_count - 1].prdr = lstr_bcs_data_extract[ll_count].prdr
				end if
			else
			//For others, check MA and DU rows....	
				if ((lstr_bcs_data_extract[ll_count].prodstage = 'MA' or lstr_bcs_data_extract[ll_count].prodstage = 'AB') and ls_prev_prodstage = 'DU') then
				//if a Mastering record is found, check to see if the
				//previous record was a Duplication record
					lstr_bcs_data_extract[ll_count - 1].actenddt = lstr_bcs_data_extract[ll_count].actenddt
				end if
			end if
		
			// mark it as dead because we don't want it written to the extract file
			lstr_bcs_data_extract[ll_count].live = false
			ll_match++
	
		else 
			// Not a match so keep it live and record the information
			// in case the next one's a match
			lstr_bcs_data_extract[ll_count].live = true
			ls_prev_conno = lstr_bcs_data_extract[ll_count].conno
			ls_narrstr = trim(lstr_bcs_data_extract[ll_count].narr) + ', ' + trim(lstr_bcs_data_extract[ll_count].narrfn)
			lstr_bcs_data_extract[ll_count].narrstr = ls_narrstr
			ll_conno_match = ll_count
			ll_nomatch++
		end if
		ls_prev_prodstage = lstr_bcs_data_extract[ll_count].prodstage
	NEXT
END IF


// open the extract file

w_pics_retrieve_msg_box.sle_retrieve.text = "Writing Extract File..."

ll_rec = 0
li_filenum = Fileopen(ls_file,streamMode!,write!,lockwrite!,Replace!)
if li_filenum = -1 then
	messagebox("File Error","the file "+ls_file+" could not be opened for output.")
	setpointer(arrow!)
	wf_bcs_data_extract_reset()
	return
end if


// Pass two, for each live record, run the f_export function

IF cbx_cutover.Checked=TRUE THEN
	FOR ll_count=1 TO ll_rows
		if (lstr_bcs3_data_extract[ll_count].live = true) then
			ls_return = f_bcs3_data_export(lstr_bcs3_data_extract[ll_count], 2)	
			Filewrite(li_filenum, ls_return)
			ll_rec++
		end if
	NEXT
ELSE
	FOR ll_count=1 TO ll_rows
		if (lstr_bcs_data_extract[ll_count].live = true) then
			ls_return = f_bcs_data_export(lstr_bcs_data_extract[ll_count], 2)	
			Filewrite(li_filenum, ls_return)
			ll_rec++
		end if
	NEXT
END IF	

// close file
Fileclose(li_filenum)

IF cbx_cutover.Checked=FALSE THEN

	//Update the Catalog table if the cutover checkbox is not checked.
	w_pics_retrieve_msg_box.sle_retrieve.text = "Updating Database. Please wait..."
	
	update catalog
			set catalog.s2cd = "S"
	where 
			catalog.s2cd = "Q" USING sqlservertrans;
			
	IF sqlservertrans.SQLCode < 0 THEN
		String ls_message,ls_msgparm[1]
		close(w_pics_retrieve_msg_box)
		ls_message = "A database error has occurred.~n" + &
		"Database error code:  " + String (sqlservertrans.sqldbcode) + "~r~n" + &
		 "Database error message:~r~n" + sqlservertrans.sqlerrtext
		If IsValid(gnv_app.inv_error) Then
			ls_msgparm[1] = ls_message
			gnv_app.inv_error.of_Message ('pfc_dwdberror', ls_msgparm, &
			gnv_app.iapp_object.DisplayName)
		Else
			Messagebox (gnv_app.iapp_object.DisplayName, ls_message, StopSign!, Ok!)
			ROLLBACK USING sqlservertrans;
		End If
		SetMicroHelp(w_pics_main,"Database update failed")
		wf_bcs_data_extract_reset()
		RETURN
	ELSE
		SetMicroHelp(w_pics_main,"Database updated successfully")
		COMMIT USING sqlservertrans;
	END IF

END IF

//All done
close(w_pics_retrieve_msg_box)
setpointer(arrow!)
messagebox("Extract Completed", string(ll_rec)+ " records written to file " + ls_file)

// Destroy the datastore 
destroy lds


// return the screen to the original state
wf_bcs_data_extract_reset()

return


end event

event ue_filename();string ls_filename, ls_path
integer li_ret

boolean lb_exist

ls_filename = "export"

ls_path = "c:\" + ls_filename
if GetFileSaveName("Select Extract File",ls_path,ls_filename, "txt", "Text Files (*.txt),*.txt") <> 1 then
	messagebox("Incorrect file name","Please try again")
	return
end if

lb_exist = FileExists(ls_path)
if lb_exist then li_ret = MessageBox("Save",  &
	ls_path +" already exists. Overwrite? ",  &
	Question!, YesNo!)
	
If	li_ret = 2 then
	return
end if

If cbx_bcs1.Checked = TRUE then
	st_file1.text = ls_path
else
	st_file2.text = ls_path
END IF

cb_extract.Visible = TRUE
cb_extract.Enabled = TRUE
cb_extract.Default = TRUE
cb_extract.Setfocus()
end event

public subroutine wf_bcs_data_extract_reset ();cb_extract.enabled = FALSE
cb_extract.Default = FALSE
gb_bcs1.Visible = FALSE
gb_bcs2.Visible = FALSE
st_1.Visible = FALSE
st_2.Visible = FALSE
st_file1.Visible = FALSE
st_file2.Visible = FALSE
cb_bcs1.Visible = FALSE
cb_bcs2.Visible = FALSE
cbx_bcs1.Checked = FALSE
cbx_bcs1.Visible = TRUE
cbx_cutover.Checked = FALSE
cbx_cutover.Visible = TRUE
cbx_bcs2.Checked = FALSE
cbx_bcs2.Visible = TRUE
cb_extract.SetFocus()
end subroutine

on w_bcs_data_extract.create
int iCurrent
call super::create
this.cbx_cutover=create cbx_cutover
this.cb_extract=create cb_extract
this.cb_exit=create cb_exit
this.cb_bcs1=create cb_bcs1
this.st_1=create st_1
this.st_file1=create st_file1
this.gb_bcs1=create gb_bcs1
this.gb_bcs2=create gb_bcs2
this.cb_bcs2=create cb_bcs2
this.st_2=create st_2
this.st_file2=create st_file2
this.cb_clear=create cb_clear
this.cbx_bcs2=create cbx_bcs2
this.cbx_bcs1=create cbx_bcs1
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.cbx_cutover
this.Control[iCurrent+2]=this.cb_extract
this.Control[iCurrent+3]=this.cb_exit
this.Control[iCurrent+4]=this.cb_bcs1
this.Control[iCurrent+5]=this.st_1
this.Control[iCurrent+6]=this.st_file1
this.Control[iCurrent+7]=this.gb_bcs1
this.Control[iCurrent+8]=this.gb_bcs2
this.Control[iCurrent+9]=this.cb_bcs2
this.Control[iCurrent+10]=this.st_2
this.Control[iCurrent+11]=this.st_file2
this.Control[iCurrent+12]=this.cb_clear
this.Control[iCurrent+13]=this.cbx_bcs2
this.Control[iCurrent+14]=this.cbx_bcs1
end on

on w_bcs_data_extract.destroy
call super::destroy
destroy(this.cbx_cutover)
destroy(this.cb_extract)
destroy(this.cb_exit)
destroy(this.cb_bcs1)
destroy(this.st_1)
destroy(this.st_file1)
destroy(this.gb_bcs1)
destroy(this.gb_bcs2)
destroy(this.cb_bcs2)
destroy(this.st_2)
destroy(this.st_file2)
destroy(this.cb_clear)
destroy(this.cbx_bcs2)
destroy(this.cbx_bcs1)
end on

event pfc_postopen;call super::pfc_postopen;cb_extract.enabled = FALSE
cb_extract.Default = FALSE
gb_bcs1.Visible = FALSE
gb_bcs2.Visible = FALSE
st_1.Visible = FALSE
st_2.Visible = FALSE
st_file1.Visible = FALSE
st_file2.Visible = FALSE
cb_bcs1.Visible = FALSE
cb_bcs2.Visible = FALSE



end event

event key;call super::key;string ls_file
IF key = Keyenter! THEN
	IF gb_bcs1.visible = TRUE THEN
		ls_file = st_file1.text
		if ls_file = "" or ls_file = "none" then
			messagebox("Invalid Filename","Please select a location for your extract file")
			cb_extract.Enabled = FALSE
	      cb_extract.Default = FALSE
	      st_file1.Setfocus()
	      RETURN
		ELSE
	      cb_extract.Enabled = TRUE
         cb_extract.Default = TRUE
		END IF
   END IF
	IF gb_bcs2.visible = TRUE THEN
		ls_file = st_file2.text
		if ls_file = "" or ls_file = "none" then
			messagebox("Invalid Filename","Please select a location for your extract file")
			cb_extract.Enabled = FALSE
	      cb_extract.Default = FALSE
	      st_file2.Setfocus()
	      RETURN
		ELSE
	      cb_extract.Enabled = TRUE
         cb_extract.Default = TRUE
		END IF
   END IF
END IF




end event

type cbx_cutover from u_cbx within w_bcs_data_extract
string tag = "Please check this for BCS Stage 2 Cutover"
integer x = 937
integer y = 424
integer width = 677
integer height = 68
boolean bringtotop = true
integer textsize = -10
long backcolor = 79741120
string text = "BCS Stage 2 Cutover"
end type

event clicked;call super::clicked;IF cbx_cutover.Checked = TRUE THEN
	cbx_bcs2.Checked = FALSE
	gb_bcs1.Visible = FALSE
	st_file1.Visible = FALSE
	cb_bcs1.Visible = FALSE
	st_1.Visible = FALSE
	gb_bcs2.Visible = TRUE
	st_file2.Visible = TRUE
	st_2.Visible = TRUE
	cb_bcs2.Visible = TRUE
	cbx_bcs1.checked = FALSE
	cbx_bcs1.Visible = FALSE
	st_file2.Setfocus()
	SetMicroHelp(w_pics_main, "Ready")
END IF
	
	
end event

type cb_extract from u_cb within w_bcs_data_extract
string tag = "Start the Extract"
integer x = 73
integer y = 860
integer taborder = 0
integer textsize = -10
string text = "&Extract"
end type

event clicked;call super::clicked;If cbx_bcs1.Checked = TRUE then
	Parent.triggerevent("ue_bcs1_extract")
ELSEIF (cbx_bcs2.Checked = TRUE) OR (cbx_cutover.Checked = TRUE) then
	Parent.triggerevent("ue_bcs2_extract")
END IF
cb_extract.enabled = FALSE
cb_extract.Default = FALSE
end event

type cb_exit from u_cb within w_bcs_data_extract
integer x = 1449
integer y = 860
integer taborder = 0
integer textsize = -10
string text = "E&xit"
end type

event clicked;call super::clicked;Parent.Event pfc_close()
end event

type cb_bcs1 from commandbutton within w_bcs_data_extract
event clicked pbm_bnclicked
integer x = 1207
integer y = 220
integer width = 183
integer height = 92
integer taborder = 40
boolean bringtotop = true
integer textsize = -10
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "Set..."
end type

event clicked;Parent.triggerevent("ue_filename")
end event

type st_1 from u_st within w_bcs_data_extract
integer x = 96
integer y = 232
integer width = 343
boolean bringtotop = true
integer textsize = -10
long backcolor = 79741120
string text = "Extract File:"
end type

type st_file1 from u_st within w_bcs_data_extract
integer x = 439
integer y = 232
integer width = 759
integer height = 72
boolean bringtotop = true
integer textsize = -10
long backcolor = 1090519039
boolean border = true
borderstyle borderstyle = stylelowered!
end type

type gb_bcs1 from groupbox within w_bcs_data_extract
integer x = 78
integer y = 100
integer width = 1550
integer height = 272
integer taborder = 11
integer textsize = -10
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long backcolor = 80263581
end type

type gb_bcs2 from groupbox within w_bcs_data_extract
integer x = 73
integer y = 492
integer width = 1550
integer height = 272
integer taborder = 20
integer textsize = -10
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long backcolor = 80263581
end type

type cb_bcs2 from commandbutton within w_bcs_data_extract
event clicked pbm_bnclicked
integer x = 1207
integer y = 608
integer width = 183
integer height = 92
integer taborder = 50
boolean bringtotop = true
integer textsize = -10
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "Set..."
end type

event clicked;Parent.triggerevent("ue_filename")
end event

type st_2 from u_st within w_bcs_data_extract
integer x = 96
integer y = 620
integer width = 343
boolean bringtotop = true
integer textsize = -10
long backcolor = 79741120
string text = "Extract File:"
end type

type st_file2 from u_st within w_bcs_data_extract
integer x = 439
integer y = 620
integer width = 759
integer height = 72
boolean bringtotop = true
integer textsize = -10
long backcolor = 1090519039
boolean border = true
borderstyle borderstyle = stylelowered!
end type

type cb_clear from commandbutton within w_bcs_data_extract
integer x = 795
integer y = 860
integer width = 352
integer height = 92
integer taborder = 30
boolean bringtotop = true
integer textsize = -10
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "&Clear"
end type

event clicked;SetMicroHelp(w_pics_main,"Ready")
wf_bcs_data_extract_reset()
end event

type cbx_bcs2 from u_cbx within w_bcs_data_extract
event clicked pbm_bnclicked
string tag = "Please check this for BCS Stage 2 extract"
integer x = 78
integer y = 428
integer width = 640
integer height = 68
boolean bringtotop = true
integer textsize = -10
long backcolor = 79741120
string text = "BCS Stage 2 Extract"
end type

event clicked;IF cbx_bcs2.Checked = TRUE THEN
	cbx_cutover.Checked = FALSE
	gb_bcs1.Visible = FALSE
	st_file1.Visible = FALSE
	cb_bcs1.Visible = FALSE
	st_1.Visible = FALSE
	gb_bcs2.Visible = TRUE
	st_file2.Visible = TRUE
	st_2.Visible = TRUE
	cb_bcs2.Visible = TRUE
	cbx_bcs1.checked = FALSE
	cbx_bcs1.Visible = FALSE
	st_file2.Setfocus()
	SetMicroHelp(w_pics_main, "Ready")
END IF
	
	
end event

type cbx_bcs1 from u_cbx within w_bcs_data_extract
string tag = "Please check this for BCS Stage 1 extract"
integer x = 87
integer y = 40
integer width = 640
integer height = 68
integer taborder = 10
boolean bringtotop = true
integer textsize = -10
long backcolor = 79741120
string text = "BCS Stage 1 Extract"
end type

event clicked;call super::clicked;IF cbx_bcs1.Checked = TRUE THEN
	gb_bcs2.Visible = FALSE
	st_file2.Visible = FALSE
	cb_bcs2.Visible = FALSE
	st_2.Visible = FALSE
	gb_bcs1.Visible = TRUE
	st_file1.Visible = TRUE
	st_1.Visible = TRUE
	cb_bcs1.Visible = TRUE
	cbx_bcs2.Visible = FALSE
	cbx_bcs2.checked = FALSE
	cbx_cutover.Visible = FALSE
	cbx_cutover.checked = FALSE
	st_file1.Setfocus()
	SetMicroHelp(w_pics_main, "Ready")
END IF
	
end event


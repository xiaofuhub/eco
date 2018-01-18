$PBExportHeader$w_sheet_edit_info.srw
forward
global type w_sheet_edit_info from w_sheet
end type
type dw_bcs_edit_info from u_pics_dw within w_sheet_edit_info
end type
type cb_export from commandbutton within w_sheet_edit_info
end type
type pb_print from commandbutton within w_sheet_edit_info
end type
type pb_delete from commandbutton within w_sheet_edit_info
end type
type pb_update from commandbutton within w_sheet_edit_info
end type
type pb_exit from commandbutton within w_sheet_edit_info
end type
type cb_lock from commandbutton within w_sheet_edit_info
end type
end forward

global type w_sheet_edit_info from w_sheet
integer x = 9
integer y = 8
integer width = 2816
integer height = 1780
string title = "BSC Edit Info Screen"
dw_bcs_edit_info dw_bcs_edit_info
cb_export cb_export
pb_print pb_print
pb_delete pb_delete
pb_update pb_update
pb_exit pb_exit
cb_lock cb_lock
end type
global w_sheet_edit_info w_sheet_edit_info

type variables
string Lconno,Ls1init,Ls2in
boolean edit_info_changed
end variables

forward prototypes
public function string wf_converttostring (datawindow dw, integer row, integer column, string datatype)
public function integer wf_commadelimited (datawindow dw, string filename)
end prototypes

public function string wf_converttostring (datawindow dw, integer row, integer column, string datatype);CHOOSE CASE lower(left(datatype, 5))
	CASE "char("
		return dw.GetItemString(Row,Column)
	CASE "numbe"
		return string(dw.GetItemNumber(Row,Column))
	CASE "decim"
		return string(dw.GetItemDecimal(Row,Column))
	CASE "date"
		return string(dw.GetItemDate(Row,Column))
	CASE "datet"
		return string(dw.GetItemDateTime(Row,Column))
	CASE "time","times"
		return string(dw.GetItemTime(Row,Column))
	CASE ELSE
		return "Error"
END CHOOSE

end function

public function integer wf_commadelimited (datawindow dw, string filename);int ColCount, li, Jj, FileNo
String ColName[]
String DataType[]
String ResultString="",Item
long rowcount, NumRowsWritten=0

// Number of columns and rows
ColCount = integer(dw.Describe("datawindow.column.count"))
rowcount = dw.RowCount()

// This routine gets the datatypes for each column in datawindow
// And places it in the DataType[]. ALso preserve ColName[] for potential
// future use.

for li=ColCount to 1 step - 1
	dw.SetColumn(li)
	ColName[li] = dw.GetColumnName()
	DataType[li] = dw.Describe( ColName[li]+".coltype")
next

// Open File
FileNo = FileOpen(FileName, LineMode!, Write!, LockReadWrite!, Replace!)

If FileNo < -1 THEN
	MessageBox("Bad FileOpen", "WHOOPS")
	return -1
end if

// Loop through all rows
for Jj = 1 to RowCount
	//Loop through all columns
	for li = 1 to ColCount
		
		if ColName[li] <> "initials" and ColName[li] <> "s2in" THEN
			Item = ""
			Item = wf_ConvertToString(dw,Jj,li,DataType[li])
			// Add comma if not the first item
			if li > 1 then
				ResultString = ResultString + ",~r~n"
			end if
			// if item is null, put quotes in position in file
			if IsNull(Item) then
				Item = ""
			end if
		
			ResultString = ResultString + "~'" + Item + "~'"
		end if		
	next
	
	if FileWrite(FileNo, ResultString) > 0 then
		NumRowsWritten++
	end if
	
	ResultString= ""
next

FileClose(FileNo)
// return number of rows written to file
return NumRowsWritten
end function

on w_sheet_edit_info.create
int iCurrent
call super::create
this.dw_bcs_edit_info=create dw_bcs_edit_info
this.cb_export=create cb_export
this.pb_print=create pb_print
this.pb_delete=create pb_delete
this.pb_update=create pb_update
this.pb_exit=create pb_exit
this.cb_lock=create cb_lock
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_bcs_edit_info
this.Control[iCurrent+2]=this.cb_export
this.Control[iCurrent+3]=this.pb_print
this.Control[iCurrent+4]=this.pb_delete
this.Control[iCurrent+5]=this.pb_update
this.Control[iCurrent+6]=this.pb_exit
this.Control[iCurrent+7]=this.cb_lock
end on

on w_sheet_edit_info.destroy
call super::destroy
destroy(this.dw_bcs_edit_info)
destroy(this.cb_export)
destroy(this.pb_print)
destroy(this.pb_delete)
destroy(this.pb_update)
destroy(this.pb_exit)
destroy(this.cb_lock)
end on

event resize;call super::resize;long ll_height

This.X = w_pics_main.X
This.Y = w_pics_main.Y
ll_height = w_pics_main.mdi_1.Height
This.Resize(w_pics_main.WorkSpaceWidth(), ll_height)

end event

event pfc_preopen;call super::pfc_preopen;this.of_SetPreference(TRUE)
this.inv_preference.of_SetToolBars(TRUE)
this.inv_preference.of_SetWindow(TRUE)
this.of_SetResize(TRUE)
this.inv_resize.of_SetOrigSize(this.workspacewidth(),this.workspaceheight())
inv_resize.of_Register(dw_bcs_edit_info, "Scale")
inv_resize.of_Register(pb_delete, "Scale")
inv_resize.of_Register(pb_exit, "Scale")
inv_resize.of_Register(pb_print, "Scale")
inv_resize.of_Register(pb_update, "Scale")
inv_resize.of_Register(cb_export, "Scale")
inv_resize.of_Register(cb_lock, "Scale")

end event

event open;call super::open;this.windowstate = maximized!

end event

event pfc_print;call super::pfc_print;pb_print.TriggerEvent(Clicked!)
return 1
end event

event closequery;Integer	li_pendingrc
Integer	li_validationrc
Integer	li_accepttextrc
Integer	li_msg
Integer	li_rc,rtn
String	ls_msgparms[]

// ib_disableclosequery = TRUE

// Check if the CloseQuery process has been disabled
If ib_disableclosequery Then
	Return 0
End If

// Call event to perform any pre-CloseQuery processing
If This.Event pfc_preclose ( ) <> 1 Then
	// Prevent the window from closing
	Return 1  
End If

// Prevent validation error messages from appearing while the window is closing
// and allow others to check if the  CloseQuery process is in progress
ib_closestatus = True

// Check for any pending updates
li_rc = of_UpdateChecks()
If li_rc = 0 Then
	// Updates are NOT pending, allow the window to be closed.
	Return 0
ElseIf li_rc < 0 Then
	// There are Updates pending, but at least one data entry error was found.
	// Give the user an opportunity to close the window without saving changes
	If IsValid(gnv_app.inv_error) Then
		li_msg = gnv_app.inv_error.of_Message('pfc_closequery_failsvalidation', &
					 ls_msgparms, gnv_app.iapp_object.DisplayName)
	Else
		li_msg = MessageBox (gnv_app.iapp_object.DisplayName, &
					"The information entered does not pass validation and "  + &
					"must be corrected before changes can be saved.~r~n~r~n" + &
					"Close without saving changes?", &
					exclamation!, YesNo!, 2)
	End If
	If li_msg = 1 Then
		Return 0
	End If
ELSEIF edit_info_changed=TRUE THEN
	// Changes to editinfo is pending, prompt the user to determine if they should be saved
	If IsValid(gnv_app.inv_error) Then
		li_msg = gnv_app.inv_error.of_Message('pfc_closequery_savechanges',  &
					ls_msgparms, gnv_app.iapp_object.DisplayName)		
	Else
		li_msg = MessageBox ( gnv_app.iapp_object.DisplayName, &
					"Do you want to save changes?", exclamation!, YesNoCancel!)
	End If
	Choose Case li_msg
		Case 1
			// YES - Update
			// If the update fails, prevent the window from closing
			rtn = pb_update.Event Clicked()
			if rtn = 1 THEN
				RETURN 0
			end if
		Case 2
			// NO - Allow the window to be closed without saving changes
			Return 0
		Case 3
			// CANCEL -  Prevent the window from closing
	End Choose
ELSE
	RETURN 0
End If

// Prevent the window from closing
ib_closestatus = False
Return 1
end event

type dw_bcs_edit_info from u_pics_dw within w_sheet_edit_info
event rbuttondown pbm_dwnrbuttondown
event rbuttonup pbm_dwnrbuttonup
event ue_postconstructor ( )
integer x = 27
integer y = 28
integer width = 2725
integer height = 1500
integer taborder = 20
string dataobject = "d_bcs_edit_info"
boolean vscrollbar = false
boolean livescroll = false
end type

event rbuttondown;//
end event

event rbuttonup;//
end event

event ue_postconstructor;integer ll_rows,RowNum
string Lflag,Linit

SetNull(Lflag)

this.SetTransObject( sqlservertrans )

cb_export.enabled = FALSE
pb_print.enabled = FALSE
pb_delete.enabled = FALSE
pb_update.enabled = FALSE
pb_exit.enabled = FALSE

if IsValid(w_sheet_bcs_stage1) then
	RowNum = w_sheet_bcs_stage1.dw_bcs_stage1.GetRow()
	Lconno = w_sheet_bcs_stage1.dw_bcs_stage1.Object.cconno[RowNum]
	Ls2in = string(w_sheet_bcs_stage1.dw_bcs_stage1.Object.cs2in[RowNum],'MM/DD/YYYY')
elseif IsValid(w_sheet_bcs_stage2) then
	RowNum = w_sheet_bcs_stage2.dw_bcs_stage2.GetRow()
	Lconno = w_sheet_bcs_stage2.dw_bcs_stage2.Object.cconno[RowNum]
	Ls2in = string(w_sheet_bcs_stage2.dw_bcs_stage2.Object.cs2in[RowNum],'MM/DD/YYYY')
end if 

ll_rows = dw_bcs_edit_info.Retrieve(Lconno)
if ll_rows < 1 THEN 
  this.InsertRow(0)
  this.object.conno[1] = Lconno
  this.object.initials[1] = sqlservertrans.userid
  this.object.s2in[1] = date(Ls2in)
  this.SetFocus()
else
	Lflag = this.object.lockflag[1]
	IF (IsNull(Lflag)=FALSE AND Lflag='Y') THEN
		cb_lock.visible = TRUE
	ELSE
		cb_lock.visible = FALSE
	END IF
end if
Linit = this.object.initials[1]
IF (TRIM(Linit) <> TRIM(sqlservertrans.userid)) THEN
	MessageBox("ERROR",sqlservertrans.userid+" is NOT allowed to update or delete this information. ~r Because this record is locked by: "+Linit,Information!)
	dw_bcs_edit_info.enabled = FALSE
	cb_export.enabled = TRUE
	pb_print.enabled 	= TRUE
	pb_delete.enabled = FALSE
	pb_update.enabled = FALSE
	cb_lock.enabled = FALSE
	pb_exit.enabled 	= TRUE
ELSE
	dw_bcs_edit_info.SetColumn(2)
	this.object.s2in[1] = date(Ls2in)

	cb_export.enabled = TRUE
	pb_print.enabled 	= TRUE
	pb_delete.enabled = TRUE
	pb_update.enabled = TRUE
	pb_exit.enabled 	= TRUE
END IF
end event

event itemchanged;call super::itemchanged;IF dwo.name="edit_info" THEN
	edit_info_changed=TRUE
END IF
end event

event pfc_addrow;//
RETURN 1
end event

event sqlpreview;call super::sqlpreview;//MessageBox("SQL",sqlsyntax)
end event

type cb_export from commandbutton within w_sheet_edit_info
event clicked pbm_bnclicked
integer x = 1157
integer y = 1560
integer width = 320
integer height = 88
integer taborder = 30
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "Expor&t..."
end type

event clicked;int rtn

w_sheet_edit_info.dw_bcs_edit_info.Object.conno.tabsequence='10'
w_sheet_edit_info.dw_bcs_edit_info.Object.initials.tabsequence='20'
w_sheet_edit_info.dw_bcs_edit_info.Object.s2in.tabsequence='30'
w_sheet_edit_info.dw_bcs_edit_info.Object.edit_info.tabsequence='40'

wf_CommaDelimited(dw_bcs_edit_info,"export.txt")
rtn = MessageBox("Export","Record has been exported into the file: ~'export.txt~', ~r Would you like to view it?",Question!,YesNo!,1)
if rtn = 1 THEN
	Run("NOTEPAD.EXE export.txt")
END IF
w_sheet_edit_info.dw_bcs_edit_info.Object.conno.tabsequence='0'
w_sheet_edit_info.dw_bcs_edit_info.Object.initials.tabsequence='0'
w_sheet_edit_info.dw_bcs_edit_info.Object.s2in.tabsequence='0'
w_sheet_edit_info.dw_bcs_edit_info.Object.edit_info.tabsequence='10'

end event

type pb_print from commandbutton within w_sheet_edit_info
event clicked pbm_bnclicked
integer x = 1518
integer y = 1560
integer width = 247
integer height = 88
integer taborder = 40
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "&Print"
end type

event clicked;long job
job = PrintOpen( ) 
PrintDataWindow(job, dw_bcs_edit_info) 
PrintClose(job)

end event

type pb_delete from commandbutton within w_sheet_edit_info
event clicked pbm_bnclicked
integer x = 1801
integer y = 1560
integer width = 288
integer height = 88
integer taborder = 50
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "&Delete"
end type

event clicked;integer Ans,rtn
long ll_row
string Ls_conno

ll_row = dw_bcs_edit_info.GetRow()
Ls_conno = dw_bcs_edit_info.object.conno[ll_row]

Ans = MessageBox("Delete Edit Information", "Delete edit information for control number: "+Ls_conno+" ?", Question!, OKCancel!, 1)
if Ans = 1 THEN 
	// Delete the row
	dw_bcs_edit_info.DeleteRow(ll_row)
	// Accept the changes made
	dw_bcs_edit_info.AcceptText()
	// Update the datawindow
  	rtn = dw_bcs_edit_info.Event pfc_update(TRUE,TRUE) 
  	if rtn = 1 THEN
		COMMIT USING sqlservertrans;
		cb_lock.visible = FALSE
   	MessageBox("Delete edit info","Record Deleted.",Information!)
		w_pics_main.setmicrohelp("Record Deleted.")
	else
   	MessageBox("Delete edit info","Delete failed.",StopSign!)
		ROLLBACK USING sqlservertrans;
	end if
  	pb_exit.Event clicked()
else
  return
end if
end event

type pb_update from commandbutton within w_sheet_edit_info
event clicked pbm_bnclicked
integer x = 2117
integer y = 1560
integer width = 311
integer height = 88
integer taborder = 10
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "&Update"
end type

event clicked;integer rtn
string Linit,Lflag,errmsg
boolean val_init

Lflag = 	dw_bcs_edit_info.object.lockflag[1]
Linit =  TRIM(dw_bcs_edit_info.object.initials[1])

// Validate the initials
IF (sqlservertrans.userid <> Linit AND Lflag='Y') THEN
	errmsg ="This Record CAN NOT be updated by userid: "+sqlservertrans.userid+"."+" Because it was updated by userid= "+Linit+"."
	Messagebox ("ERROR",errmsg, StopSign!)
	RETURN 0
END IF

IF (IsNull(Ls1init)) THEN
  	MessageBox("ERROR","You must enter values for ~'Catalog Initials~', before updating database.",StopSign!)
	RETURN 0	  
ELSE
	dw_bcs_edit_info.object.lockflag[1] = 'Y'
	rtn = dw_bcs_edit_info.Event pfc_Update(TRUE,TRUE) 

	if rtn = 1 THEN
		COMMIT USING sqlservertrans;
		cb_lock.visible = TRUE
		w_pics_main.setmicrohelp("Record Updated.")
		RETURN 1
	else
   	MessageBox("Update edit info","update failed.",StopSign!)
		ROLLBACK USING sqlservertrans;
		RETURN 0
	end if
END IF


end event

type pb_exit from commandbutton within w_sheet_edit_info
event clicked pbm_bnclicked
integer x = 2459
integer y = 1560
integer width = 247
integer height = 88
integer taborder = 60
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "E&xit"
end type

event clicked;w_pics_main.setmicrohelp("Ready")
close(parent)

end event

type cb_lock from commandbutton within w_sheet_edit_info
event clicked pbm_bnclicked
boolean visible = false
integer x = 46
integer y = 1560
integer width = 613
integer height = 88
integer taborder = 70
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "Unlock the &Record"
end type

event clicked;integer  rtn
string Linitial,Lflag,Linit,ls_message,ls_msgparm[1]
Boolean val_user

SetNull(Lflag)
	

rtn = MessageBox("Unlock the Record","This process will unlock the record from editinfo table.",Question!,YesNo!,2)
if rtn = 1 THEN
	Linitial = dw_bcs_edit_info.object.initials[1]
	Lconno = dw_bcs_edit_info.object.conno[1]
	
	SELECT lockflag,initials
		INTO   :Lflag,:Linit       
	FROM   editinfo
	WHERE  editinfo.conno = :Lconno
	USING  sqlservertrans;   

	IF sqlservertrans.SQLCode < 0 THEN
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
		RETURN -1
	END IF
	IF (IsNull(Lflag)=TRUE) THEN
		Messagebox ("ERROR","Record is not locked by any user.", StopSign!)
		return -1
	ELSEIF Linit <> Linitial THEN
		Messagebox ("ERROR","Record is locked by another user.", StopSign!)
		return 01
	ELSE
		// update the editinfo and unlock the record.
		dw_bcs_edit_info.object.lockflag[1]='N'
		rtn = dw_bcs_edit_info.Update( ) 
	
		if rtn = 1 THEN
			COMMIT USING SQLCA;
			cb_lock.visible = FALSE
		else
			MessageBox("Update edit info","update failed.",StopSign!)
			ROLLBACK USING SQLCA;
		end if
	END IF
END IF
end event


$PBExportHeader$w_add_sources.srw
$PBExportComments$This screen will allow sources to be added to PICS database
forward
global type w_add_sources from w_response
end type
type cb_exit from commandbutton within w_add_sources
end type
type cb_update from commandbutton within w_add_sources
end type
type dw_add_sources from u_pics_dw within w_add_sources
end type
type cb_clear from commandbutton within w_add_sources
end type
type cb_find from commandbutton within w_add_sources
end type
end forward

global type w_add_sources from w_response
integer x = 197
integer y = 472
integer width = 2542
integer height = 664
string title = "Add/Update Sources"
cb_exit cb_exit
cb_update cb_update
dw_add_sources dw_add_sources
cb_clear cb_clear
cb_find cb_find
end type
global w_add_sources w_add_sources

on w_add_sources.create
int iCurrent
call super::create
this.cb_exit=create cb_exit
this.cb_update=create cb_update
this.dw_add_sources=create dw_add_sources
this.cb_clear=create cb_clear
this.cb_find=create cb_find
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.cb_exit
this.Control[iCurrent+2]=this.cb_update
this.Control[iCurrent+3]=this.dw_add_sources
this.Control[iCurrent+4]=this.cb_clear
this.Control[iCurrent+5]=this.cb_find
end on

on w_add_sources.destroy
call super::destroy
destroy(this.cb_exit)
destroy(this.cb_update)
destroy(this.dw_add_sources)
destroy(this.cb_clear)
destroy(this.cb_find)
end on

event open;call super::open;dw_add_sources.Object.src_desc.TabSequence='0'

dw_add_sources.Object.DataWindow.QueryMode='Yes'
w_add_sources.cb_update.Enabled=FALSE
w_add_sources.cb_clear.Enabled=FALSE
dw_add_sources.SetFocus()
end event

event closequery;Integer	li_pendingrc
Integer	li_validationrc
Integer	li_accepttextrc
Integer	li_msg,rtn
Integer	li_rc
String	ls_msgparms[]

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
Else
	// Changes are pending, prompt the user to determine if they should be saved
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
			rtn = cb_update.Event Clicked()
			If rtn = 1 Then
				// Successful update, allow the window to be closed
				Return 0
			End If
		Case 2
			// NO - Allow the window to be closed without saving changes
			Return 0
		Case 3
			// CANCEL -  Prevent the window from closing
	End Choose
End If

// Prevent the window from closing
ib_closestatus = False
Return 1
end event

type cb_exit from commandbutton within w_add_sources
integer x = 2245
integer y = 384
integer width = 247
integer height = 108
boolean bringtotop = true
integer textsize = -10
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "E&xit"
end type

event clicked;parent.Event pfc_close()

end event

type cb_update from commandbutton within w_add_sources
integer x = 1609
integer y = 384
integer width = 311
integer height = 108
boolean bringtotop = true
integer textsize = -10
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "&Update"
end type

event clicked;int rc

// Accept the text that was put on the screen.
rc = dw_add_sources.AcceptText()

// If there were no changes to the screen, don't try to update the screen.
IF rc=1 THEN
	IF dw_add_sources.ModifiedCount( ) > 0 THEN

		rc = dw_add_sources.Event pfc_update(TRUE,TRUE)
		
		if rc=1 THEN
			Commit Using sqlservertrans;
			DataWindowChild ldwc_src
			w_sheet_initial_title.dw_src1.GetChild ("src_code", ldwc_src)
			ldwc_src.SetTransObject(sqlservertrans)
			ldwc_src.Retrieve()
			w_sheet_initial_title.dw_src2.GetChild ("src_code", ldwc_src)
			ldwc_src.SetTransObject(sqlservertrans)
			ldwc_src.Retrieve()
			w_sheet_initial_title.dw_src3.GetChild ("src_code", ldwc_src)
			ldwc_src.SetTransObject(sqlservertrans)
			ldwc_src.Retrieve()
			dw_add_sources.ResetUpdate()
 			MessageBox("Update","Source table was updated.",Information!)
			return 1
		else 
 			MessageBox("ERROR","Update failed.",StopSign!)
			return 0
		end if
	else
		return 0
 		MessageBox("Update","No Changes to the source.",Information!)
	end if
end if	
end event

type dw_add_sources from u_pics_dw within w_add_sources
integer x = 41
integer y = 36
integer width = 2450
integer height = 320
integer taborder = 10
string dataobject = "d_add_sources"
boolean livescroll = false
end type

event ue_postconstructor();call super::ue_postconstructor;// set the transaction object.
dw_add_sources.SetTransObject(SQLServerTrans)

dw_add_sources.of_SetDropDownSearch(TRUE)
dw_add_sources.inv_dropdownsearch.of_AddColumn("src_code")

end event

type cb_clear from commandbutton within w_add_sources
integer x = 1961
integer y = 384
integer width = 251
integer height = 108
boolean bringtotop = true
integer textsize = -10
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "&Clear"
end type

event clicked;long ll_rows,rtn

dw_add_sources.AcceptText()

IF dw_add_sources.ModifiedCount( ) > 0 THEN
	rtn=MessageBox("Clear","Save the changes before clearing the screen?",Question!,OkCancel!,2)
	IF rtn = 1 THEN
		w_add_sources.cb_update.TriggerEvent(Clicked!)
	END IF
END IF
w_add_sources.cb_find.Enabled=TRUE
w_add_sources.cb_update.Enabled=FALSE
w_add_sources.cb_clear.Enabled=FALSE
dw_add_sources.Object.DataWindow.QueryMode='Yes'
dw_add_sources.SetFocus()

end event

type cb_find from commandbutton within w_add_sources
event clicked pbm_bnclicked
integer x = 1326
integer y = 384
integer width = 247
integer height = 108
integer taborder = 20
boolean bringtotop = true
integer textsize = -10
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "F&ind"
boolean default = true
end type

event clicked;int ll_rows,rtn
String Lsrc

Lsrc = dw_add_sources.GetText()
IF Lsrc <> "" THEN
	dw_add_sources.Object.DataWindow.QueryMode='No'
	ll_rows = dw_add_sources.Retrieve(Lsrc)
  	IF ll_rows < 1 THEN
      rtn = MessageBox("Find Error", "sources Does not exist, Insert new record?" ,Question!, OkCancel!, 1)
		IF rtn=1 THEN
     		dw_add_sources.InsertRow(0)
			dw_add_sources.object.src_code[1]=Lsrc
			dw_add_sources.Object.src_code.TabSequence='10'
			dw_add_sources.Object.src_desc.TabSequence='20'
			w_add_sources.cb_update.Enabled=TRUE
			w_add_sources.cb_clear.Enabled=TRUE
			w_add_sources.cb_find.Enabled=FALSE
		   dw_add_sources.setfocus()
		ELSE
     		dw_add_sources.InsertRow(0)
			dw_add_sources.Object.DataWindow.QueryMode='Yes'
		   dw_add_sources.setfocus()
		END IF
	ELSE
		dw_add_sources.Object.src_code.TabSequence='10'
		dw_add_sources.Object.src_desc.TabSequence='20'
		w_add_sources.cb_clear.Enabled=TRUE
		w_add_sources.cb_update.Enabled=TRUE
		w_add_sources.cb_find.Enabled=FALSE		
		dw_add_sources.setfocus()
  	END IF
ELSE
   MessageBox("Database Error", "Please enter the sources code." ,StopSign!, OK!, 2)
	dw_add_sources.setfocus()
END IF

end event


$PBExportHeader$w_addshelf.srw
forward
global type w_addshelf from w_response
end type
type dw_shelfs_cnts from u_pics_dw within w_addshelf
end type
type dw_shelfs from u_pics_dw within w_addshelf
end type
type cb_new from commandbutton within w_addshelf
end type
type cb_exit from commandbutton within w_addshelf
end type
type cb_update from commandbutton within w_addshelf
end type
end forward

global type w_addshelf from w_response
integer x = 142
integer y = 168
integer width = 2277
integer height = 1600
string title = "Shelf Locations"
dw_shelfs_cnts dw_shelfs_cnts
dw_shelfs dw_shelfs
cb_new cb_new
cb_exit cb_exit
cb_update cb_update
end type
global w_addshelf w_addshelf

type variables
string lcntr,lcntrmed,lcntrtype

end variables

on w_addshelf.create
int iCurrent
call super::create
this.dw_shelfs_cnts=create dw_shelfs_cnts
this.dw_shelfs=create dw_shelfs
this.cb_new=create cb_new
this.cb_exit=create cb_exit
this.cb_update=create cb_update
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_shelfs_cnts
this.Control[iCurrent+2]=this.dw_shelfs
this.Control[iCurrent+3]=this.cb_new
this.Control[iCurrent+4]=this.cb_exit
this.Control[iCurrent+5]=this.cb_update
end on

on w_addshelf.destroy
call super::destroy
destroy(this.dw_shelfs_cnts)
destroy(this.dw_shelfs)
destroy(this.cb_new)
destroy(this.cb_exit)
destroy(this.cb_update)
end on

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

type dw_shelfs_cnts from u_pics_dw within w_addshelf
integer x = 1147
integer y = 28
integer width = 1079
integer height = 1324
integer taborder = 30
string dataobject = "d_shelfs_cnts"
end type

event constructor;call super::constructor;// set the transaction object.
this.SetTransObject(SQLServerTrans)

this.retrieve()
end event

type dw_shelfs from u_pics_dw within w_addshelf
event ue_enterkey pbm_dwnprocessenter
integer x = 27
integer y = 28
integer width = 1115
integer height = 1320
integer taborder = 20
string dataobject = "d_shelfs"
end type

event ue_enterkey;Send(Handle(this),256,9,Long(0,0))
return(1)
end event

event constructor;call super::constructor;// set the transaction object.
this.SetTransObject(SQLServerTrans)

this.retrieve()
end event

event sqlpreview;call super::sqlpreview;//messagebox("sql",sqlsyntax)
end event

event pfc_insertrow;call super::pfc_insertrow;//
return -1
end event

type cb_new from commandbutton within w_addshelf
integer x = 1381
integer y = 1364
integer width = 229
integer height = 112
integer taborder = 30
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "&New"
end type

event clicked;dw_shelfs.Event pfc_addrow()
dw_shelfs.ScrollToRow(dw_shelfs.rowcount())
end event

type cb_exit from commandbutton within w_addshelf
integer x = 1979
integer y = 1364
integer width = 229
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

type cb_update from commandbutton within w_addshelf
integer x = 1659
integer y = 1364
integer width = 265
integer height = 112
boolean bringtotop = true
integer textsize = -10
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "&Update"
end type

event clicked;int rc,i

dw_shelfs.accepttext()


rc = dw_shelfs.Event pfc_update(TRUE,TRUE)
		
if rc=1 THEN
	COMMIT USING SQLServerTrans;
	MessageBox("Update","New location was added.",StopSign!)
	return 1
else 
	ROLLBACK USING SQLServerTrans;
	MessageBox("ERROR","Update failed.",StopSign!)
	return 0
end if
end event


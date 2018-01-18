$PBExportHeader$w_special_foreign_library.srw
forward
global type w_special_foreign_library from w_response
end type
type pb_find from picturebutton within w_special_foreign_library
end type
type dw_special_foreign_library_no_criteria from u_pics_dw within w_special_foreign_library
end type
type sle_go from singlelineedit within w_special_foreign_library
end type
type cb_go from commandbutton within w_special_foreign_library
end type
type cb_new from commandbutton within w_special_foreign_library
end type
type sle_cnts from singlelineedit within w_special_foreign_library
end type
type st_1 from statictext within w_special_foreign_library
end type
type dw_bkno from u_pics_dw within w_special_foreign_library
end type
type cb_delete from commandbutton within w_special_foreign_library
end type
type cb_exit from commandbutton within w_special_foreign_library
end type
type cb_update from commandbutton within w_special_foreign_library
end type
type dw_specail_foreign_library from u_pics_dw within w_special_foreign_library
end type
type cb_clear from commandbutton within w_special_foreign_library
end type
end forward

global type w_special_foreign_library from w_response
integer x = 110
integer y = 136
integer width = 4105
integer height = 2216
string title = "Special Foreign Library Collections"
windowstate windowstate = maximized!
pb_find pb_find
dw_special_foreign_library_no_criteria dw_special_foreign_library_no_criteria
sle_go sle_go
cb_go cb_go
cb_new cb_new
sle_cnts sle_cnts
st_1 st_1
dw_bkno dw_bkno
cb_delete cb_delete
cb_exit cb_exit
cb_update cb_update
dw_specail_foreign_library dw_specail_foreign_library
cb_clear cb_clear
end type
global w_special_foreign_library w_special_foreign_library

on w_special_foreign_library.create
int iCurrent
call super::create
this.pb_find=create pb_find
this.dw_special_foreign_library_no_criteria=create dw_special_foreign_library_no_criteria
this.sle_go=create sle_go
this.cb_go=create cb_go
this.cb_new=create cb_new
this.sle_cnts=create sle_cnts
this.st_1=create st_1
this.dw_bkno=create dw_bkno
this.cb_delete=create cb_delete
this.cb_exit=create cb_exit
this.cb_update=create cb_update
this.dw_specail_foreign_library=create dw_specail_foreign_library
this.cb_clear=create cb_clear
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.pb_find
this.Control[iCurrent+2]=this.dw_special_foreign_library_no_criteria
this.Control[iCurrent+3]=this.sle_go
this.Control[iCurrent+4]=this.cb_go
this.Control[iCurrent+5]=this.cb_new
this.Control[iCurrent+6]=this.sle_cnts
this.Control[iCurrent+7]=this.st_1
this.Control[iCurrent+8]=this.dw_bkno
this.Control[iCurrent+9]=this.cb_delete
this.Control[iCurrent+10]=this.cb_exit
this.Control[iCurrent+11]=this.cb_update
this.Control[iCurrent+12]=this.dw_specail_foreign_library
this.Control[iCurrent+13]=this.cb_clear
end on

on w_special_foreign_library.destroy
call super::destroy
destroy(this.pb_find)
destroy(this.dw_special_foreign_library_no_criteria)
destroy(this.sle_go)
destroy(this.cb_go)
destroy(this.cb_new)
destroy(this.sle_cnts)
destroy(this.st_1)
destroy(this.dw_bkno)
destroy(this.cb_delete)
destroy(this.cb_exit)
destroy(this.cb_update)
destroy(this.dw_specail_foreign_library)
destroy(this.cb_clear)
end on

event open;call super::open;//
dw_specail_foreign_library.SetFocus()


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

event resize;call super::resize;long ll_height

//This script is not commented because when we do minimize this screen then all the DW'S are not resizing 
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
inv_resize.of_Register(dw_specail_foreign_library, "Scale")
inv_resize.of_Register(dw_special_foreign_library_no_criteria, "Scale")
inv_resize.of_Register(dw_bkno, "Scale")
inv_resize.of_Register(cb_clear, "Scale")
inv_resize.of_Register(cb_update, "Scale")
inv_resize.of_Register(cb_delete, "Scale")
inv_resize.of_Register(cb_exit, "Scale")
inv_resize.of_Register(cb_new, "Scale")
inv_resize.of_Register(cb_go, "Scale")
inv_resize.of_Register(pb_find, "Scale")
inv_resize.of_Register(sle_go, "Scale")
inv_resize.of_Register(sle_cnts, "Scale")
inv_resize.of_Register(st_1, "Scale")

end event

type pb_find from picturebutton within w_special_foreign_library
integer x = 1851
integer y = 1964
integer width = 425
integer height = 104
integer taborder = 40
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "&Find..."
end type

event clicked;string rtn,rc,Lconno,Linit
long ll_rows
integer res

dw_specail_foreign_library.visible=FALSE
dw_special_foreign_library_no_criteria.visible=TRUE

//IF find is dispalyed on the push button
IF pb_find.BringToTop = FALSE THEN
	// Turn on query mode so user can specify data
	rtn = dw_special_foreign_library_no_criteria.Modify("DataWindow.QueryMode=YES")
	IF rtn = "" THEN
		// If Modify succeeds, show Execute,
		// Query mode is on and display sort CheckBox
		This.BringToTop = TRUE
		This.Text = "Ex&ecute"
		cb_delete.Enabled=FALSE
		cb_new.Enabled=FALSE
		cb_update.Enabled=FALSE
		dw_special_foreign_library_no_criteria.SetFocus()
	   SetMicroHelp(w_pics_main,"Query Mode...")
	ELSE
		MessageBox("Error", "Can't access query mode to select data.")
	END IF
ELSE
	dw_special_foreign_library_no_criteria.AcceptText()
	// Turn off Query mode and retrieve data 
	// based on user's choices
	rtn = dw_special_foreign_library_no_criteria.Modify("DataWindow.QueryMode=NO")
	IF rtn = "" THEN
		// If Modify succeeds, show Find,
		// Query mode is off, and retrieve data
		This.BringToTop = FALSE
		This.Text = "F&ind"
		ll_rows=dw_special_foreign_library_no_criteria.Retrieve()
		IF ll_rows > 0 THEN 
			// If any rows were retrieved.
      	SetMicroHelp(w_pics_main,"")
			sle_cnts.text = string(ll_rows)
		ELSE
			// If no rows were retrieved, ask if they want to continue with retrieval.
      	SetMicroHelp(w_pics_main,"")
			res = MessageBox("Retrieve Error","No records were retrieved. Continue with query mode?", Question!, OkCancel!, 2 )
			IF res = 1 THEN
				// If yes continue the reterival process
				pb_find.TriggerEvent(Clicked!)
			ELSE
				// Restore the original select statement and modify the datawindow
				// mod_string is a shared variable, and is set in ue_postconstructor event of dw_bcs_stage1.
				res = dw_special_foreign_library_no_criteria.Reset()
				IF res = 1 THEN
					dw_specail_foreign_library.Enabled=TRUE
					dw_special_foreign_library_no_criteria.Enabled=FALSE
				END IF // res = 1
			END IF // res = 1 
		END IF // ll_rows > 0
		cb_delete.Enabled=TRUE
		cb_new.Enabled=TRUE
		cb_update.Enabled=TRUE
	ELSE
		MessageBox("Error","Failure exiting query mode.")
	END IF // rtn = ""
END IF // if pb_find...
end event

type dw_special_foreign_library_no_criteria from u_pics_dw within w_special_foreign_library
event ue_enterkey pbm_dwnprocessenter
integer x = 471
integer y = 24
integer width = 3584
integer height = 1924
integer taborder = 20
string dataobject = "d_special_foreign_library_no_criteria"
boolean resizable = true
boolean hsplitscroll = true
boolean livescroll = false
end type

event ue_enterkey;Send(Handle(this),256,9,Long(0,0))
return(1)

end event

event ue_postconstructor;call super::ue_postconstructor;// set the transaction object.
dw_special_foreign_library_no_criteria.of_SetTransObject(SQLServerTrans)

end event

event retrievestart;call super::retrievestart;openwithparm(w_pics_retrieve_msg_box,"Retrieving data, Please Wait...")
end event

event retrieveend;call super::retrieveend;close(w_pics_retrieve_msg_box)
end event

type sle_go from singlelineedit within w_special_foreign_library
integer x = 1239
integer y = 1968
integer width = 439
integer height = 88
integer taborder = 30
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 33554432
textcase textcase = upper!
borderstyle borderstyle = stylelowered!
end type

event modified;cb_go.TriggerEvent(Clicked!)

end event

type cb_go from commandbutton within w_special_foreign_library
integer x = 928
integer y = 1968
integer width = 288
integer height = 84
integer taborder = 30
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "&Go to"
end type

event clicked;dw_specail_foreign_library.visible=TRUE
dw_special_foreign_library_no_criteria.visible=FALSE

IF sle_go.text="" OR ISNull(sle_go.text) THEN
	MessageBox("ERROR","Please enter a book number.")
	SetFocus(sle_go)
ELSE
	
	string ls_bk_num
	long ll_nbr, ll_foundrow
	ll_nbr = dw_bkno.RowCount()

	// Remove leading and trailing blanks.
	ls_bk_num = Trim(sle_go.Text)
	
	dw_bkno.SetFocus()
	ll_foundrow = dw_bkno.Find("bookno_ = '" + ls_bk_num + "'", 1, ll_nbr)
	//MessageBox("row",String(ll_foundrow))
	IF ll_foundrow=0 THEN
		MessageBox("Find error","No matching book number was found. Please try again.")
		SetFocus(sle_go)
	ELSE
		dw_bkno.ScrollToRow(ll_foundrow)
		dw_specail_foreign_library.SetFocus()
	END IF
END IF
end event

type cb_new from commandbutton within w_special_foreign_library
integer x = 2469
integer y = 1964
integer width = 247
integer height = 108
integer taborder = 30
boolean bringtotop = true
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "&New"
end type

event clicked;long ll_newrow

dw_specail_foreign_library.visible=TRUE
dw_special_foreign_library_no_criteria.visible=FALSE

ll_newrow = dw_specail_foreign_library.InsertRow(0)

dw_specail_foreign_library.ScrollToRow(ll_newrow)

w_special_foreign_library.cb_update.Enabled=TRUE
w_special_foreign_library.cb_clear.Enabled=TRUE
w_special_foreign_library.cb_new.Enabled=FALSE

dw_specail_foreign_library.setfocus()

end event

type sle_cnts from singlelineedit within w_special_foreign_library
integer x = 544
integer y = 1968
integer width = 320
integer height = 80
integer taborder = 30
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 33554432
borderstyle borderstyle = stylelowered!
end type

type st_1 from statictext within w_special_foreign_library
integer x = 41
integer y = 1968
integer width = 517
integer height = 64
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 33554432
long backcolor = 67108864
string text = "Books Counted:"
boolean focusrectangle = false
end type

type dw_bkno from u_pics_dw within w_special_foreign_library
integer x = 32
integer y = 24
integer width = 439
integer height = 1924
integer taborder = 20
string dataobject = "dddw_bkno"
end type

event ue_postconstructor;call super::ue_postconstructor;// set the transaction object.
long ll_rows
this.of_SetTransObject(SQLServerTrans)
ll_rows = this.Retrieve()

sle_cnts.text = string(ll_rows)
end event

event retrievestart;call super::retrievestart;openwithparm(w_pics_retrieve_msg_box,"Retrieving data, Please Wait...")
end event

event retrieveend;call super::retrieveend;close(w_pics_retrieve_msg_box)
dw_specail_foreign_library.visible=TRUE
dw_special_foreign_library_no_criteria.visible=FALSE


end event

event itemfocuschanged;call super::itemfocuschanged;IF dwo.Name = "bookno_" THEN
	string lbkno
	lbkno = this.object.bookno_[row]
	dw_specail_foreign_library.of_SetTransObject(SQLServerTrans)
	dw_specail_foreign_library.Retrieve(lbkno)
END IF

end event

type cb_delete from commandbutton within w_special_foreign_library
integer x = 3109
integer y = 1964
integer width = 288
integer height = 108
integer taborder = 20
boolean bringtotop = true
integer textsize = -10
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "&Delete"
end type

event clicked;Int rtn
String lbkno

lbkno = dw_bkno.object.bookno_[dw_bkno.GetRow()]


rtn = MessageBox("Delete","Do you want to delete book number :"+lbkno+" from database?",Question!,YesNo!,1)
IF rtn = 1 THEN
	dw_specail_foreign_library.DeleteRow(dw_specail_foreign_library.GetRow())
	rtn = dw_specail_foreign_library.Event pfc_update(TRUE, TRUE)
	if rtn = 1 THEN
		COMMIT USING SQLServerTrans;
		MessageBox("Delete","Delete Successful.",Information!)
		dw_specail_foreign_library.Resetupdate()
		cb_clear.TriggerEvent(Clicked!)
		return 1
	else
		MessageBox("ERROR","Delete failed.",StopSign!)
		dw_specail_foreign_library.Resetupdate()
		RETURN 0
	end if
END IF
end event

type cb_exit from commandbutton within w_special_foreign_library
integer x = 3813
integer y = 1964
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

event clicked;dw_specail_foreign_library.ResetUpdate()
parent.Event pfc_close()

end event

type cb_update from commandbutton within w_special_foreign_library
integer x = 2761
integer y = 1964
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

IF dw_specail_foreign_library.visible=TRUE THEN
	// Accept the text that was put on the screen.
	dw_specail_foreign_library.AcceptText()
	
	// If there were no changes to the screen, don't try to update the screen.
	IF dw_specail_foreign_library.ModifiedCount( ) > 0 THEN
		rc = dw_specail_foreign_library.Event pfc_update(TRUE, TRUE)
		if rc = 1 THEN
			COMMIT USING SQLServerTrans;
			MessageBox("Update","Update Successful.",Information!)
			dw_specail_foreign_library.Resetupdate()
			return 1
		else
			MessageBox("ERROR","Update failed.",StopSign!)
			dw_specail_foreign_library.Resetupdate()
			RETURN 0
		end if
	else // if Modifiedcount > 1
		MessageBox("Update","There are no changes to the record.",Information!)
		RETURN 0
	end if		
ELSE
	// Accept the text that was put on the screen.
	dw_special_foreign_library_no_criteria.AcceptText()
	
	// If there were no changes to the screen, don't try to update the screen.
	IF dw_special_foreign_library_no_criteria.ModifiedCount( ) > 0 THEN
		rc = dw_special_foreign_library_no_criteria.Event pfc_update(TRUE, TRUE)
		if rc = 1 THEN
			COMMIT USING SQLServerTrans;
			MessageBox("Update","Update Successful.",Information!)
			dw_special_foreign_library_no_criteria.Resetupdate()
			return 1
		else
			MessageBox("ERROR","Update failed.",StopSign!)
			dw_special_foreign_library_no_criteria.Resetupdate()
			RETURN 0
		end if
	else // if Modifiedcount > 1
		MessageBox("Update","There are no changes to the record.",Information!)
		RETURN 0
	end if	
END IF
end event

type dw_specail_foreign_library from u_pics_dw within w_special_foreign_library
event ue_enterkey pbm_dwnprocessenter
integer x = 480
integer y = 24
integer width = 3579
integer height = 1928
integer taborder = 10
string dataobject = "d_special_foreign_library"
boolean resizable = true
boolean hsplitscroll = true
boolean livescroll = false
end type

event ue_enterkey;//IF (dw_specail_foreign_library.GetColumn() <> 1 ) THEN
	Send(Handle(this),256,9,Long(0,0))
	return(1)
//END IF

end event

event ue_postconstructor;call super::ue_postconstructor;// set the transaction object.
dw_specail_foreign_library.of_SetTransObject(SQLServerTrans)

end event

event itemchanged;call super::itemchanged;//IF dwo.Name = "bookno_" THEN
//	cb_find.TriggerEvent(Clicked!)
//END IF

end event

type cb_clear from commandbutton within w_special_foreign_library
integer x = 3447
integer y = 1964
integer width = 320
integer height = 108
boolean bringtotop = true
integer textsize = -10
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "&Refresh"
end type

event clicked;long ll_rows,rtn

dw_specail_foreign_library.visible=TRUE
dw_special_foreign_library_no_criteria.visible=FALSE

dw_specail_foreign_library.Reset()

dw_bkno.of_SetTransObject(SQLServerTrans)
ll_rows = dw_bkno.Retrieve()

cb_delete.Enabled=TRUE
cb_new.Enabled=TRUE
cb_update.Enabled=TRUE

sle_cnts.text = string(ll_rows)
sle_go.text = ""

dw_specail_foreign_library.of_SetTransObject(SQLServerTrans)
dw_specail_foreign_library.Retrieve(dw_bkno.object.bookno_[1])
dw_specail_foreign_library.SetFocus()

w_special_foreign_library.cb_update.Enabled=TRUE
w_special_foreign_library.cb_clear.Enabled=TRUE
w_special_foreign_library.cb_new.Enabled=TRUE

end event


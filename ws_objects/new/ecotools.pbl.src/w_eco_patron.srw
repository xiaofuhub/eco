$PBExportHeader$w_eco_patron.srw
forward
global type w_eco_patron from w_main
end type
type cb_exit from u_cb within w_eco_patron
end type
type cb_update from u_cb within w_eco_patron
end type
type cb_delete from u_cb within w_eco_patron
end type
type cb_clear from u_cb within w_eco_patron
end type
type cb_find from u_cb within w_eco_patron
end type
type cb_summary from commandbutton within w_eco_patron
end type
type dw_eco_units from u_dw within w_eco_patron
end type
type st_1 from statictext within w_eco_patron
end type
type sle_rows from singlelineedit within w_eco_patron
end type
type dw_eco_patron_sum from u_dw within w_eco_patron
end type
type dw_eco_units_search from u_dw within w_eco_patron
end type
type dw_eco_patron from u_dw within w_eco_patron
end type
end forward

global type w_eco_patron from w_main
integer x = 5
integer y = 4
integer width = 2926
integer height = 1680
string title = "Remote and Amplifier"
cb_exit cb_exit
cb_update cb_update
cb_delete cb_delete
cb_clear cb_clear
cb_find cb_find
cb_summary cb_summary
dw_eco_units dw_eco_units
st_1 st_1
sle_rows sle_rows
dw_eco_patron_sum dw_eco_patron_sum
dw_eco_units_search dw_eco_units_search
dw_eco_patron dw_eco_patron
end type
global w_eco_patron w_eco_patron

type variables
boolean rows_exist=FALSE
end variables

on w_eco_patron.create
int iCurrent
call super::create
this.cb_exit=create cb_exit
this.cb_update=create cb_update
this.cb_delete=create cb_delete
this.cb_clear=create cb_clear
this.cb_find=create cb_find
this.cb_summary=create cb_summary
this.dw_eco_units=create dw_eco_units
this.st_1=create st_1
this.sle_rows=create sle_rows
this.dw_eco_patron_sum=create dw_eco_patron_sum
this.dw_eco_units_search=create dw_eco_units_search
this.dw_eco_patron=create dw_eco_patron
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.cb_exit
this.Control[iCurrent+2]=this.cb_update
this.Control[iCurrent+3]=this.cb_delete
this.Control[iCurrent+4]=this.cb_clear
this.Control[iCurrent+5]=this.cb_find
this.Control[iCurrent+6]=this.cb_summary
this.Control[iCurrent+7]=this.dw_eco_units
this.Control[iCurrent+8]=this.st_1
this.Control[iCurrent+9]=this.sle_rows
this.Control[iCurrent+10]=this.dw_eco_patron_sum
this.Control[iCurrent+11]=this.dw_eco_units_search
this.Control[iCurrent+12]=this.dw_eco_patron
end on

on w_eco_patron.destroy
call super::destroy
destroy(this.cb_exit)
destroy(this.cb_update)
destroy(this.cb_delete)
destroy(this.cb_clear)
destroy(this.cb_find)
destroy(this.cb_summary)
destroy(this.dw_eco_units)
destroy(this.st_1)
destroy(this.sle_rows)
destroy(this.dw_eco_patron_sum)
destroy(this.dw_eco_units_search)
destroy(this.dw_eco_patron)
end on

event open;call super::open;this.windowstate = maximized!

dw_eco_patron_sum.SetFocus()
end event

event pfc_preopen;this.of_SetPreference(TRUE)
this.inv_preference.of_SetToolBars(TRUE)
this.inv_preference.of_SetWindow(TRUE)
this.of_SetResize(TRUE)
this.inv_resize.of_SetOrigSize(this.workspacewidth(),this.workspaceheight())
inv_resize.of_register(cb_clear,"Scale")
inv_resize.of_register(cb_delete,"Scale")
inv_resize.of_register(cb_update,"Scale")
inv_resize.of_register(cb_exit,"Scale")
inv_resize.of_register(cb_find,"Scale")
inv_resize.of_register(cb_summary,"Scale")
inv_resize.of_register(dw_eco_patron,"Scale")
inv_resize.of_register(dw_eco_units,"Scale")
inv_resize.of_register(dw_eco_patron_sum,"Scale")
inv_resize.of_register(sle_rows,"Scale")
inv_resize.of_register(st_1,"Scale")


cb_delete.Enabled = FALSE
cb_clear.Enabled = FALSE
cb_update.Enabled = FALSE
cb_summary.Enabled = FALSE

// Turn on query mode so user can specify data
dw_eco_patron.object.DataWindow.QueryMode='Yes'
dw_eco_units.Reset()


end event

type cb_exit from u_cb within w_eco_patron
integer x = 2597
integer y = 1448
integer width = 265
integer taborder = 0
boolean bringtotop = true
string text = "E&xit"
end type

event clicked;rows_exist=FALSE
close(parent)

end event

type cb_update from u_cb within w_eco_patron
integer x = 2304
integer y = 1448
integer width = 265
integer taborder = 0
boolean bringtotop = true
string text = "&Update"
end type

event clicked;int rc
rows_exist=FALSE

// Check for any pending updates
IF of_UpdateChecks( ) < 0 THEN Return -1

// If there were no changes to the screen, don't try to update the screen.
IF (dw_eco_patron.ModifiedCount() > 0) OR &
	(dw_eco_units.ModifiedCount() > 0) OR &
	(dw_eco_units.DeletedCount() > 0) THEN
	
	IF (dw_eco_units.ModifiedCount() > 0) AND &
		(dw_eco_patron.ModifiedCount() > 0) THEN		
		dw_eco_patron_sum.ShareDataOff()
		rc = dw_eco_patron.Event pfc_Update(True,True)
		IF rc = 1 THEN
			rc = dw_eco_units.Event pfc_Update(True,True)
			IF rc = 1 THEN
				COMMIT USING SQLServerTrans;
				MessageBox("Update","Update Succesful. ",Information!)
			ELSE
				ROLLBACK USING SQLServerTrans;
				MessageBox("Error","Update failed. ",StopSign!)
				RETURN -1
			END IF
		ELSE
			ROLLBACK USING SQLServerTrans;
			MessageBox("Error","Update failed. ",StopSign!)
			RETURN -1
		END IF
		
	ELSEIF (dw_eco_units.DeletedCount() > 0) AND &
			 (dw_eco_patron.ModifiedCount() = 0) THEN
				dw_eco_patron_sum.ShareDataOff()
				rc = dw_eco_units.Event pfc_Update(True,True)
				IF rc = 1 THEN
					COMMIT USING SQLServerTrans;
					MessageBox("Update","Update Succesful. ",Information!)
				ELSE
					ROLLBACK USING SQLServerTrans;
					MessageBox("Error","Update failed. ",StopSign!)
					RETURN -1
				END IF
	ELSEIF (dw_eco_units.DeletedCount() = 0) AND &
			 (dw_eco_patron.ModifiedCount() > 0) THEN
				dw_eco_patron_sum.ShareDataOff()
				rc = dw_eco_patron.Event pfc_Update(True,True)
				IF rc = 1 THEN
					COMMIT USING SQLServerTrans;
					MessageBox("Update","Update Succesful. ",Information!)
				ELSE
					ROLLBACK USING SQLServerTrans;
					MessageBox("Error","Update failed. ",StopSign!)
					RETURN -1
				END IF
	ELSE 
		dw_eco_patron_sum.ShareDataOff()
		rc = dw_eco_patron.Event pfc_Update(True,True)
		IF rc = 1 THEN
			rc = dw_eco_units.Event pfc_Update(True,True)
			IF rc = 1 THEN
				COMMIT USING SQLServerTrans;
				MessageBox("Update","Update Succesful. ",Information!)
			ELSE
				ROLLBACK USING SQLServerTrans;
				MessageBox("Error","Update failed. ",StopSign!)
				RETURN -1
			END IF
		ELSE
			ROLLBACK USING SQLServerTrans;
			MessageBox("Error","Update failed. ",StopSign!)
			RETURN -1
		END IF
	END IF		
	
ELSE // if Modifiedcount > 1
 	MessageBox("Update","There are no changes to the record.",Information!)
	RETURN -1
END IF

IF dw_eco_patron.Sharedata(dw_eco_patron_sum) = -1 THEN
	MessageBox("ERROR", "Error sharing data between datawindows.",StopSign!, OK!, 2)
END IF

end event

type cb_delete from u_cb within w_eco_patron
integer x = 1998
integer y = 1448
integer width = 265
integer taborder = 0
boolean bringtotop = true
string text = "&Delete"
end type

event clicked;int rc
string lname
long ll_currentrow,ll_patron_id


IF (dw_eco_patron_sum.visible = TRUE) THEN
	lname = TRIM(dw_eco_patron.object.lastname[dw_eco_patron_sum.Getrow()])
	ll_patron_id = dw_eco_patron_sum.object.patron_id[dw_eco_patron_sum.GetRow()]
ELSE
	lname = TRIM(dw_eco_patron.object.lastname[dw_eco_patron.Getrow()])
	ll_patron_id = dw_eco_patron.object.patron_id[dw_eco_patron.GetRow()]
END IF

IF (dw_eco_patron_sum.visible = TRUE) THEN
	dw_eco_patron.ScrolltoRow(dw_eco_patron_sum.Getrow())
	dw_eco_patron_sum.visible = FALSE
END IF
cb_summary.text ='Summary...'

rc = MessageBox("Delete","Are you sure you want to delete ~'"+lname+"~' ,Patron_ID="+string(ll_patron_id)+" ,from database?",Question!,YesNocancel!,1)
IF rc = 1 THEN
	rows_exist = FALSE
	//stop sharing the data 
	dw_eco_patron_sum.ShareDataOff()
	
	IF (dw_eco_patron_sum.visible = TRUE) THEN
		rc = dw_eco_patron_sum.DeleteRow(0)
	ELSE
		rc = dw_eco_patron.DeleteRow(0)
	END IF		
	IF rc = 1 THEN
		COMMIT USING SQLServerTrans;
		DELETE FROM eco_units
	   WHERE eco_units.patron_id = :ll_patron_id   
		USING SQLServerTrans;
		IF SQLServerTrans.sqlcode=0 THEN
			parent.Event pfc_save()
			COMMIT USING SQLServerTrans;
			IF dw_eco_patron.Sharedata(dw_eco_patron_sum) = -1 THEN
				MessageBox("ERROR", "Error sharing data between datawindows.",StopSign!, OK!, 2)
			END IF
			dw_eco_units.Reset()
			MessageBox("Delete","Row is deleted. ",Information!)
			cb_find.enabled=TRUE
			cb_clear.enabled=TRUE
		ELSE
			ROLLBACK USING SQLServerTrans;
			MessageBox("Error","Delete failed. ",StopSign!)
			cb_find.enabled=TRUE
			cb_clear.enabled=TRUE
			RETURN -1
		END IF
	ELSE
		ROLLBACK USING SQLServerTrans;
		MessageBox("Error","Delete failed. ",StopSign!)
		cb_find.enabled=TRUE
		cb_clear.enabled=TRUE
		RETURN -1
	END IF
	rows_exist=TRUE
END IF
end event

type cb_clear from u_cb within w_eco_patron
integer x = 1691
integer y = 1448
integer width = 265
integer taborder = 0
boolean bringtotop = true
string text = "&Clear"
end type

event clicked;long ll_rows
rows_exist = FALSE
dw_eco_units_search.visible=FALSE
//stop sharing the data 
dw_eco_patron_sum.ShareDataOff()

dw_eco_patron.Reset()

// Clear the querymode of the datawindow.
//dw_eco_patron.Object.DataWindow.QueryClear = 'Yes'

ll_rows = dw_eco_patron.InsertRow(0)


// Scroll to that row
dw_eco_patron.ScrolltoRow(ll_rows)


// Reset Update flag in all the datawindow for exit button validation
dw_eco_patron.ResetUpdate( )

//
dw_eco_patron.Object.DataWindow.QueryClear = 'Yes'
dw_eco_patron.Object.DataWindow.QueryMode = 'Yes'

IF (dw_eco_patron_sum.visible = TRUE) THEN
	dw_eco_patron_sum.visible = FALSE
ELSE
	dw_eco_patron.visible = TRUE
END IF
cb_summary.text ='Summary...'

//dw_eco_units.DBCancel() // unsupported by appeon
dw_eco_units.Reset()
cb_delete.Enabled = FALSE
cb_clear.Enabled = FALSE
cb_update.Enabled = FALSE
cb_summary.Enabled = FALSE
cb_find.Enabled = TRUE
sle_rows.text=""

// Set the focus on the control number
dw_eco_patron.setfocus()
end event

type cb_find from u_cb within w_eco_patron
integer x = 1390
integer y = 1448
integer width = 270
integer taborder = 0
boolean bringtotop = true
fontcharset fontcharset = ansi!
string text = "F&ind"
end type

event clicked;int rtn
long ll_rows,ll_patron_id,ll_max_patron_id
string lname

rows_exist = FALSE
lname = dw_eco_patron.GetText()

IF (dw_eco_patron_sum.visible = TRUE) THEN
	dw_eco_patron_sum.visible = FALSE
	cb_summary.text ="Su&mmary..."
END IF
// Accept the data before retrieving
dw_eco_patron.AcceptText()
// Turn off Query mode and retrieve data 
// based on user's choices
dw_eco_patron.object.DataWindow.QueryMode='No'
// If Modify succeeds, show Find,
// Query mode is off, and retrieve data
ll_rows = dw_eco_patron.Retrieve()
	
IF ll_rows = -1 THEN
	sqlservertrans.of_Rollback()
	MessageBox("Error","Retrieve error ")
	cb_clear.TriggerEvent(Clicked!)
ELSEIF ll_rows = 0 THEN
	rtn = MessageBox("Retrieve","Patron "+lname+" does not exist. Enter a new patron?",Question!,YesNoCancel!,1)
	IF rtn = 1 THEN
		dw_eco_patron.InsertRow(0)
		dw_eco_patron.object.lastname[1]=lname
		// Get the maximun number as patron id
		SELECT max(eco_patron.patron_id)
    		INTO :ll_max_patron_id  
    	FROM eco_patron  
		USING SQLServerTrans;
		// Add one to it and assign it to the new record.
		IF IsNull(ll_max_patron_id) THEN
			dw_eco_patron.object.patron_id[1] = 1 
		ELSE
			dw_eco_patron.object.patron_id[1]= ll_max_patron_id + 1	
		END IF
		dw_eco_patron.SetColumn(2)

		cb_clear.Enabled = TRUE
		cb_update.Enabled = TRUE
		cb_find.Enabled = FALSE
		dw_eco_patron.SetFocus( )
	ELSE
		cb_clear.TriggerEvent(Clicked!)
	END IF
ELSEIF ll_rows = 1 THEN
	IF dw_eco_patron.Sharedata(dw_eco_patron_sum) = -1 THEN
			MessageBox("ERROR", "Error sharing data between datawindows.",StopSign!, OK!, 2)
	END IF
	ll_patron_id = dw_eco_patron.object.patron_id[1]
	ll_rows = dw_eco_units.Retrieve(ll_patron_id)
	IF ll_rows = 0 THEN
		rtn = MessageBox("Add Units","Patron ID "+"~'"+string(ll_patron_id)+"~'"+" dose not have a unit. Do you want to add one?",Question!,YesNo!,1)
		IF rtn = 1 THEN
			dw_eco_units.Event pfc_AddRow()
			dw_eco_units.Setfocus()
			rows_exist = FALSE
		ELSE
			dw_eco_patron.SetFocus()			
			rows_exist = TRUE
		END IF
	ELSE
		rows_exist = TRUE
	END IF
   SetMicroHelp(w_eco_patron,"")
	cb_delete.Enabled = TRUE
	cb_clear.Enabled = TRUE
	cb_update.Enabled = TRUE
	cb_summary.Enabled = TRUE
	cb_find.Enabled = FALSE
	sle_rows.text = string(dw_eco_patron.RowCount())

	
ELSEIF ll_rows > 1 THEN 
	IF dw_eco_patron.Sharedata(dw_eco_patron_sum) = -1 THEN
		MessageBox("ERROR", "Error sharing data between datawindows.",StopSign!, OK!, 2)
	END IF
	ll_patron_id = dw_eco_patron.object.patron_id[1]
	ll_rows = dw_eco_units.Retrieve(ll_patron_id)
	IF ll_rows = 0 THEN
		rtn = MessageBox("Add Units","Patron ID "+"~'"+string(ll_patron_id)+"~'"+" dose not have a unit. Do you want to add one?",Question!,YesNo!,1)
		IF rtn = 1 THEN
			dw_eco_units.Event pfc_AddRow()
			dw_eco_units.Setfocus()
			rows_exist = FALSE
		ELSE
			dw_eco_patron.SetFocus()			
			rows_exist = TRUE
		END IF
	ELSE
		rows_exist = TRUE
	END IF
   SetMicroHelp(w_eco_patron,"")
	cb_delete.Enabled = TRUE
	cb_clear.Enabled = TRUE
	cb_update.Enabled = TRUE
	cb_summary.Enabled = TRUE
	cb_find.Enabled = FALSE
	sle_rows.text = string(dw_eco_patron.RowCount())

	
	cb_summary.TriggerEvent(Clicked!)
END IF
end event

type cb_summary from commandbutton within w_eco_patron
integer x = 41
integer y = 1448
integer width = 293
integer height = 92
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "Su&mmary..."
end type

event clicked;IF (dw_eco_patron_sum.visible = TRUE) THEN
	dw_eco_patron_sum.visible = FALSE
	cb_summary.text ='Summary...'
	dw_eco_patron.ScrollToRow(dw_eco_patron_sum.GetRow())
	dw_eco_patron.SetFocus()
ELSE
	dw_eco_patron_sum.visible = TRUE
	cb_summary.text ='Detail...'
	dw_eco_patron_sum.SetFocus()
END IF
end event

type dw_eco_units from u_dw within w_eco_patron
event ue_enterkey pbm_dwnprocessenter
integer x = 37
integer y = 968
integer width = 2848
integer height = 420
integer taborder = 30
boolean bringtotop = true
string dataobject = "d_eco_units"
end type

event ue_enterkey;Send(Handle(this),256,9,Long(0,0))
return(1)
end event

event constructor;this.of_SetTransObject(sqlservertrans)
this.of_SetDropDownCalendar(TRUE)
this.iuo_calendar.of_Register("idate",this.iuo_calendar.DDLB)
this.iuo_calendar.of_Register("rdate",this.iuo_calendar.DDLB)



end event

event pfc_addrow;long	ll_rc
long	ll_patron_id


IF (dw_eco_patron_sum.visible = TRUE) THEN
	ll_patron_id = dw_eco_patron_sum.object.patron_id[dw_eco_patron_sum.GetRow()]
ELSE
	ll_patron_id = dw_eco_patron.object.patron_id[dw_eco_patron.GetRow()]
END IF

IF IsNull(ll_patron_id) THEN
	MessageBox("ERROR","You must enter patron ID before entering device information.")
	dw_eco_patron.SetFocus()
	RETURN 1	
END IF
// Allow for pre functionality.
If this.Event pfc_preinsertrow() <= 0 Then
	Return NO_ACTION
End If

// Notify that a new row is about to be added.
IF IsValid ( inv_Linkage ) THEN 
	inv_Linkage.Event pfc_InsertRow (0) 
END IF 

// Insert row.
if IsValid (inv_rowmanager) then
	ll_rc = inv_rowmanager.event pfc_addrow ()
else
	ll_rc = this.InsertRow (0) 
end if

// Notify that a new row has been added.
IF IsValid ( inv_Linkage ) THEN 
	inv_Linkage.Event pfc_InsertRow (ll_rc) 
END IF 

this.object.patron_id[ll_rc] = ll_patron_id
this.object.rdate[ll_rc] = today()
this.object.idate[ll_rc] = today()

rows_exist=FALSE

return ll_rc
end event

event pfc_insertrow;//
return 1
end event

event sqlpreview;call super::sqlpreview;//messagebox("SQL",sqlsyntax)
end event

event pfc_deleterow;call super::pfc_deleterow;integer	li_rc
long		ll_row

// Perform Pre Delete process.
if this.Event pfc_predeleterow() <= PREVENT_ACTION then return NO_ACTION

// Delete row.
if IsValid (inv_RowManager) then
	li_rc = inv_RowManager.event pfc_deleterow () 
else	
	li_rc = this.DeleteRow (0) 
end if

if li_rc > 0 then ll_row = 0 else ll_row = -1

//	Note: The deletion of multiple master rows is not supported by the linkage service.
if IsValid ( inv_Linkage ) then inv_Linkage.Event pfc_deleterow (ll_row) 

rows_exist=FALSE

return li_rc
end event

type st_1 from statictext within w_eco_patron
integer x = 421
integer y = 1452
integer width = 562
integer height = 76
boolean bringtotop = true
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 33554432
long backcolor = 67108864
boolean enabled = false
string text = "Rows Counted:"
boolean focusrectangle = false
end type

type sle_rows from singlelineedit within w_eco_patron
integer x = 754
integer y = 1444
integer width = 274
integer height = 92
boolean bringtotop = true
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 33554432
boolean autohscroll = false
textcase textcase = upper!
borderstyle borderstyle = stylelowered!
end type

type dw_eco_patron_sum from u_dw within w_eco_patron
event ue_enterkey pbm_dwnprocessenter
boolean visible = false
integer x = 37
integer y = 32
integer width = 2853
integer height = 936
integer taborder = 40
string dataobject = "d_eco_patron_sum"
boolean hscrollbar = true
end type

event ue_enterkey;Send(Handle(this),256,9,Long(0,0))
return(1)
end event

event constructor;this.of_SetFilter(TRUE)
this.of_SetFind(TRUE)
this.inv_filter.of_SetStyle(1)
this.inv_filter.of_SetColumnNameSource(2)
this.of_SetSort(TRUE)
this.inv_sort.of_SetStyle(1)
this.inv_sort.of_SetColumnNameSource(2)
this.of_SetTransObject(sqlservertrans)

end event

event doubleclicked;dw_eco_patron_sum.visible = FALSE
dw_eco_patron.visible = TRUE
cb_summary.text ='Summary...'
dw_eco_patron.ScrollToRow(row)
end event

event pfc_deleterow;cb_delete.TriggerEvent(Clicked!)
return 1
end event

event pfc_insertrow;//
return 1
end event

event pfc_addrow;long	ll_rc,ll_max_patron_id

// Allow for pre functionality.
If this.Event pfc_preinsertrow() <= 0 Then
	Return NO_ACTION
End If

// Notify that a new row is about to be added.
IF IsValid ( inv_Linkage ) THEN 
	inv_Linkage.Event pfc_InsertRow (0) 
END IF 

// Insert row.
if IsValid (inv_rowmanager) then
	ll_rc = inv_rowmanager.event pfc_addrow ()
else
	ll_rc = this.InsertRow (0) 
end if

// Notify that a new row has been added.
IF IsValid ( inv_Linkage ) THEN 
	inv_Linkage.Event pfc_InsertRow (ll_rc) 
END IF 



// Get the maximun number as patron id
SELECT max(eco_patron.patron_id)
	INTO :ll_max_patron_id  
  	FROM eco_patron  
USING SQLServerTrans;


// Add one to it and assign it to the new record.
this.object.patron_id[ll_rc]= ll_max_patron_id + 1

this.scrolltorow(ll_rc)
this.setrow(ll_rc)
this.setfocus()
this.SetColumn(2)


dw_eco_units.reset()

return ll_rc

end event

event rowfocuschanged;call super::rowfocuschanged;IF rows_exist THEN
	dw_eco_units.Retrieve(this.object.patron_id[currentrow])
END IF
end event

type dw_eco_units_search from u_dw within w_eco_patron
boolean visible = false
integer x = 32
integer y = 24
integer width = 2848
integer height = 1364
integer taborder = 20
string dataobject = "d_eco_units_search"
end type

event constructor;this.of_SetFilter(TRUE)
this.of_SetFind(TRUE)
this.inv_filter.of_SetStyle(1)
this.inv_filter.of_SetColumnNameSource(2)
this.of_SetSort(TRUE)
this.inv_sort.of_SetStyle(1)
this.inv_sort.of_SetColumnNameSource(2)
this.of_SetTransObject(sqlservertrans)

end event

type dw_eco_patron from u_dw within w_eco_patron
event ue_enterkey pbm_dwnprocessenter
integer x = 37
integer y = 24
integer width = 2857
integer height = 944
integer taborder = 10
string dataobject = "d_eco_patron"
end type

event ue_enterkey;Send(Handle(this),256,9,Long(0,0))
return(1)

end event

event constructor;this.of_SetFilter(TRUE)
this.of_SetFind(TRUE)
this.inv_filter.of_SetStyle(1)
this.inv_filter.of_SetColumnNameSource(2)
this.of_SetSort(TRUE)
this.inv_sort.of_SetStyle(1)
this.inv_sort.of_SetColumnNameSource(2)
this.of_SetTransObject(sqlservertrans)

end event

event retrievestart;call super::retrievestart;OpenWithParm(w_pics_retrieve_msg_box,"Retrieving records, Please wait...")

end event

event retrieveend;call super::retrieveend;Close(w_pics_retrieve_msg_box)
//ib_RowChanged = True


end event

event sqlpreview;call super::sqlpreview;//messagebox("SQL",sqlsyntax)
end event

event pfc_addrow;long	ll_rc,ll_max_patron_id

// Allow for pre functionality.
If this.Event pfc_preinsertrow() <= 0 Then
	Return NO_ACTION
End If

// Notify that a new row is about to be added.
IF IsValid ( inv_Linkage ) THEN 
	inv_Linkage.Event pfc_InsertRow (0) 
END IF 

// Insert row.
if IsValid (inv_rowmanager) then
	ll_rc = inv_rowmanager.event pfc_addrow ()
else
	ll_rc = this.InsertRow (0) 
end if

// Notify that a new row has been added.
IF IsValid ( inv_Linkage ) THEN 
	inv_Linkage.Event pfc_InsertRow (ll_rc) 
END IF 



// Get the maximun number as patron id
SELECT max(eco_patron.patron_id)
	INTO :ll_max_patron_id  
  	FROM eco_patron  
USING SQLServerTrans;


// Add one to it and assign it to the new record.
this.object.patron_id[ll_rc]= ll_max_patron_id + 1

this.scrolltorow(ll_rc)
this.setrow(ll_rc)
this.setfocus()
this.SetColumn(2)


dw_eco_units.reset()

return ll_rc

end event

event pfc_insertrow;//
return 1
end event

event pfc_deleterow;cb_delete.TriggerEvent(Clicked!)
return 1
end event

event rowfocuschanged;call super::rowfocuschanged;IF rows_exist THEN
	dw_eco_units.Retrieve(this.object.patron_id[currentrow])
END IF

end event


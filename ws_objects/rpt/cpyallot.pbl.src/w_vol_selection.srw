$PBExportHeader$w_vol_selection.srw
forward
global type w_vol_selection from w_sheet
end type
type dw_vol_selection from u_dw within w_vol_selection
end type
type cb_update from u_cb within w_vol_selection
end type
type cb_exit from u_cb within w_vol_selection
end type
type sle_count from u_sle within w_vol_selection
end type
type st_counts from u_st within w_vol_selection
end type
type st_totalrowstext from u_st within w_vol_selection
end type
type sle_totalrows from u_sle within w_vol_selection
end type
end forward

global type w_vol_selection from w_sheet
int X=106
int Y=109
int Width=2739
int Height=1733
boolean TitleBar=true
string Title="Selection For Volunteer"
dw_vol_selection dw_vol_selection
cb_update cb_update
cb_exit cb_exit
sle_count sle_count
st_counts st_counts
st_totalrowstext st_totalrowstext
sle_totalrows sle_totalrows
end type
global w_vol_selection w_vol_selection

on w_vol_selection.create
int iCurrent
call w_sheet::create
this.dw_vol_selection=create dw_vol_selection
this.cb_update=create cb_update
this.cb_exit=create cb_exit
this.sle_count=create sle_count
this.st_counts=create st_counts
this.st_totalrowstext=create st_totalrowstext
this.sle_totalrows=create sle_totalrows
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=dw_vol_selection
this.Control[iCurrent+2]=cb_update
this.Control[iCurrent+3]=cb_exit
this.Control[iCurrent+4]=sle_count
this.Control[iCurrent+5]=st_counts
this.Control[iCurrent+6]=st_totalrowstext
this.Control[iCurrent+7]=sle_totalrows
end on

on w_vol_selection.destroy
call w_sheet::destroy
destroy(this.dw_vol_selection)
destroy(this.cb_update)
destroy(this.cb_exit)
destroy(this.sle_count)
destroy(this.st_counts)
destroy(this.st_totalrowstext)
destroy(this.sle_totalrows)
end on

event open;call super::open;THIS.Windowstate = maximized!
end event

event pfc_postopen;call super::pfc_postopen;m_pics_main.m_Edit.m_Addrow.Enabled = FALSE
m_pics_main.m_Edit.m_Deleterow.Enabled = FALSE
m_pics_main.m_Edit.m_Cut.Enabled = FALSE
dw_vol_selection.settransobject(sqlservertrans)
dw_vol_selection.Event pfc_retrieve()

end event

event pfc_preopen;call super::pfc_preopen;this.of_SetPreference(TRUE)
this.inv_preference.of_SetToolBars(TRUE)
this.inv_preference.of_SetWindow(TRUE)
this.of_SetResize(TRUE)
this.inv_resize.of_SetOrigSize(this.workspacewidth(),this.workspaceheight())
inv_resize.of_Register(dw_vol_selection, "scale")
inv_resize.of_Register(cb_update, "scale")
inv_resize.of_Register(cb_exit, "scale")
inv_resize.of_Register(st_counts, "scale")
inv_resize.of_Register(sle_count, "scale")
inv_resize.of_Register(st_totalrowstext, "scale")
inv_resize.of_Register(sle_totalrows, "scale")


end event

event resize;call super::resize;//long ll_height
//
//This.X = w_pics_main.X
//This.Y = w_pics_main.Y
//ll_height = w_pics_main.mdi_1.Height
//This.Resize(w_pics_main.WorkSpaceWidth(), ll_height)
end event

event mousemove;call super::mousemove;w_pics_main.setmicrohelp("Ready")
end event

event closequery;//////////////////////////////////////////////////////////////////////////////
//
//	Event:  closequery
//
//	Description:
//	Search for unsaved datawindows prompting the user if any
//	pending updates are found.
//
//////////////////////////////////////////////////////////////////////////////
//	
//	Revision History
//
//	Version
//	5.0   Initial version
//
//////////////////////////////////////////////////////////////////////////////
//
//	Copyright © 1996-1997 Sybase, Inc. and its subsidiaries.  All rights reserved.
//	Any distribution of the PowerBuilder Foundation Classes (PFC)
//	source code by other than Sybase, Inc. and its subsidiaries is prohibited.
//
//////////////////////////////////////////////////////////////////////////////

Integer	li_pendingrc
Integer	li_validationrc
Integer	li_accepttextrc
Integer	li_msg
Integer	li_rc,rtn
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
					dw_vol_selection.Setfocus()
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
			IF rtn = 1 THEN
				RETURN 0
			END IF
//			If This.Event pfc_save() >= 1 Then
//				// Successful update, allow the window to be closed
//				Return 0
//			End If
		Case 2
			// NO - Allow the window to be closed without saving changes
			Return 0
		Case 3
			dw_vol_selection.Setfocus()
			// CANCEL -  Prevent the window from closing
	End Choose
End If

// Prevent the window from closing
ib_closestatus = False
Return 1
end event

type dw_vol_selection from u_dw within w_vol_selection
event ue_enterkey pbm_dwnprocessenter
event pfc_hinttext pbm_mousemove
event pfc_keydown pbm_dwnkey
int X=10
int Y=17
int Width=2689
int Height=1477
int TabOrder=10
string DataObject="d_vol_selection"
end type

event ue_enterkey;call super::ue_enterkey;Send(Handle(dw_vol_selection),256,9,Long(0,0))
return(1)
end event

event pfc_hinttext;call super::pfc_hinttext;string ls_object, ls_column, ls_column_tag
long ll_pos

//This script set's microhelp at the bottom of the screen dw_vol_selection Datawindow
ls_object = THIS.getobjectatpointer()
ll_pos = pos(ls_object, "~t")
IF NOT pos(ls_object, "_t~t") > 0 THEN
	IF ll_pos > 0 THEN
		ll_pos = ll_pos -1
		ls_column = mid(ls_object,1,ll_pos)
		ls_column_tag = THIS.Describe(ls_column + ".tag")
		w_pics_main.setmicrohelp(ls_column_tag)
	ELSE
		w_pics_main.setmicrohelp("Ready")
	END IF
END IF
end event

event constructor;call super::constructor;ib_rmbmenu = FALSE


end event

event getfocus;call super::getfocus;m_pics_main.m_Edit.m_Addrow.Enabled = FALSE
m_pics_main.m_Edit.m_Deleterow.Enabled = FALSE
m_pics_main.m_Edit.m_Cut.Enabled = FALSE
end event

event retrieveend;call super::retrieveend;sle_totalrows.Text = string(rowcount)

IF NOT Rowcount > 0 THEN
	Close(w_pics_retrieve_msg_box)
	Messagebox("No Rows", "No data found to update the record(s)")
	ib_disableclosequery = TRUE
	Parent.Event pfc_close()	
ELSE
	Close(w_pics_retrieve_msg_box)
	dw_vol_selection.Setfocus()
	ib_disableclosequery = FALSE
	cb_update.Enabled = TRUE
	st_counts.Enabled = TRUE
   sle_count.Enabled = TRUE
END IF

	
	


end event

event pfc_retrieve;call super::pfc_retrieve;RETURN THIS.Retrieve()
end event

event retrievestart;call super::retrievestart;Open(w_pics_retrieve_msg_box)
end event

event itemchanged;call super::itemchanged;long ll_ret,ll_row
integer li_count

li_count = integer(sle_count.Text)

IF dwo.name = "cas_flag" AND data = "I" THEN
	ll_ret = Setitem(row,"mchar_cascd","N")
	sle_count.Text = string(li_count + 1)
ELSE
	dw_vol_selection.Setitem(row,"mchar_cascd","")
	sle_count.Text = string(li_count - 1)
END IF
	

end event

event rowfocuschanged;call super::rowfocuschanged;currentrow = currentrow
end event

event updatestart;call super::updatestart;OPEN(w_pics_update_msg_box)
end event

event updateend;call super::updateend;close(w_pics_update_msg_box)
end event

type cb_update from u_cb within w_vol_selection
event pfc_hinttext pbm_mousemove
int X=1852
int Y=1521
int TabOrder=0
string Tag="Update's the record to the database"
string Text="&Update"
int TextSize=-10
end type

event pfc_hinttext;call super::pfc_hinttext;w_pics_main.setmicrohelp(This.tag)
end event

event clicked;call super::clicked;integer rtn
string ls_message,ls_msgparm[1]

IF dw_vol_selection.ModifiedCount( ) + dw_vol_selection.Deletedcount() = 0 THEN
	RETURN
END IF

dw_vol_selection.Accepttext()

 rtn = parent.Event pfc_save()
 IF rtn = 1 THEN
	COMMIT USING sqlservertrans;
	Messagebox("Update","Update successful")
	RETURN 1
ELSEIF sqlservertrans.SQLCode < 0 THEN
				ls_message = "A database error has occurred in Insert.~n" + &
								 "Database error code:  " + String (sqlservertrans.sqldbcode) + "~r~n" + &
								 "Database error message:~r~n" + sqlservertrans.sqlerrtext
				IF IsValid(gnv_app.inv_error) THEN
					ls_msgparm[1] = ls_message
					gnv_app.inv_error.of_Message ('pfc_dwdberror', ls_msgparm, &
					gnv_app.iapp_object.DisplayName)
				ELSE
					Messagebox (gnv_app.iapp_object.DisplayName, ls_message, StopSign!, Ok!)
					ROLLBACK USING sqlservertrans;
					RETURN -1
				End If
END IF

	
end event

type cb_exit from u_cb within w_vol_selection
event pfc_hinttext pbm_mousemove
int X=2282
int Y=1521
int TabOrder=0
string Tag="Exit the screen"
string Text="E&xit"
int TextSize=-10
end type

event pfc_hinttext;call super::pfc_hinttext;w_pics_main.setmicrohelp(This.tag)
end event

event clicked;call super::clicked;parent.Event pfc_close()
m_pics_main.m_menu.popMenu(300,0)

end event

type sle_count from u_sle within w_vol_selection
event pfc_hinttext pbm_mousemove
int X=791
int Y=1521
int Width=261
int Height=85
int TabOrder=0
string Tag="Number of rows retrieved"
boolean DisplayOnly=true
string Text="0"
long TextColor=255
long BackColor=1090519039
int TextSize=-10
int Weight=700
end type

event pfc_hinttext;call super::pfc_hinttext;w_pics_main.Setmicrohelp(THIS.Tag)
end event

type st_counts from u_st within w_vol_selection
int X=1
int Y=1529
int Width=791
string Text="Total Selected For Next Batch"
long BackColor=12632256
int Weight=700
end type

type st_totalrowstext from u_st within w_vol_selection
int X=1111
int Y=1533
int Width=375
string Text="Total Records"
int Weight=700
end type

type sle_totalrows from u_sle within w_vol_selection
int X=1491
int Y=1525
int Width=266
int Height=85
int TabOrder=0
boolean DisplayOnly=true
TextCase TextCase=Upper!
long TextColor=255
int TextSize=-10
int Weight=700
end type


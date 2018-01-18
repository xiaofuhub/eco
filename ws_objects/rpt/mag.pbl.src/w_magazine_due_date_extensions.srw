$PBExportHeader$w_magazine_due_date_extensions.srw
forward
global type w_magazine_due_date_extensions from w_sheet
end type
type dw_due_date_extensions from u_dw within w_magazine_due_date_extensions
end type
type cb_update from u_cb within w_magazine_due_date_extensions
end type
type cb_clear from u_cb within w_magazine_due_date_extensions
end type
type cb_exit from u_cb within w_magazine_due_date_extensions
end type
type dw_due_date_extensions_magext from u_dw within w_magazine_due_date_extensions
end type
end forward

global type w_magazine_due_date_extensions from w_sheet
string title = "Magazine Due Date Extensions"
dw_due_date_extensions dw_due_date_extensions
cb_update cb_update
cb_clear cb_clear
cb_exit cb_exit
dw_due_date_extensions_magext dw_due_date_extensions_magext
end type
global w_magazine_due_date_extensions w_magazine_due_date_extensions

forward prototypes
public function integer wf_check_data (string ls_magcode, ref date ld_issuedate)
public function integer wf_get_title (string ls_magcode, ref string ls_title)
end prototypes

public function integer wf_check_data (string ls_magcode, ref date ld_issuedate);String ls_temp

 SELECT magiss.magcd
   INTO :ls_temp
   FROM magcntr,   
         magiss,   
         magttl  
   WHERE ( magcntr.fy = magiss.fy ) and  
         ( magiss.magcd = magttl.magcd ) and  
         ( ( magiss.magcd = :ls_magcode ) AND  
         ( magiss.issdt = :ld_issuedate ) AND  
         ( magcntr.fy = magiss.fy ) AND  
         ( magcntr.cntr = magiss.cntr ) AND  
         ( magiss.magcd = magttl.magcd ) )
 USING SqlServerTrans;



Return SqlServerTrans.SqlCode

end function

public function integer wf_get_title (string ls_magcode, ref string ls_title);
//Gets the title and returns it.

  SELECT magttl.title  
  INTO   :ls_title  
  FROM   magttl  
  WHERE  magttl.magcd = :ls_magcode
  USING  SqlServerTrans;
	
Return SqlServerTrans.Sqlcode
end function

on w_magazine_due_date_extensions.create
int iCurrent
call super::create
this.dw_due_date_extensions=create dw_due_date_extensions
this.cb_update=create cb_update
this.cb_clear=create cb_clear
this.cb_exit=create cb_exit
this.dw_due_date_extensions_magext=create dw_due_date_extensions_magext
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_due_date_extensions
this.Control[iCurrent+2]=this.cb_update
this.Control[iCurrent+3]=this.cb_clear
this.Control[iCurrent+4]=this.cb_exit
this.Control[iCurrent+5]=this.dw_due_date_extensions_magext
end on

on w_magazine_due_date_extensions.destroy
call super::destroy
destroy(this.dw_due_date_extensions)
destroy(this.cb_update)
destroy(this.cb_clear)
destroy(this.cb_exit)
destroy(this.dw_due_date_extensions_magext)
end on

event open;call super::open;cb_update.Enabled = FALSE
cb_clear.Enabled = FALSE
dw_due_date_extensions_magext.Enabled = FALSE

//Disable the addrow and deleterow menu functions
m_pics_main.m_edit.m_deleterow.Enabled = FALSE
m_pics_main.m_edit.m_addrow.Enabled = FALSE
end event

event pfc_preopen;call super::pfc_preopen;this.of_SetPreference(TRUE)
this.inv_preference.of_SetToolBars(TRUE)
this.inv_preference.of_SetWindow(TRUE)
this.of_SetResize(TRUE)
this.inv_resize.of_SetOrigSize(this.workspacewidth(),this.workspaceheight())

inv_resize.of_Register(cb_clear, "scale")
inv_resize.of_Register(cb_exit, "scale")
inv_resize.of_Register(cb_update, "scale")
inv_resize.of_Register(dw_due_date_extensions, "scale")
inv_resize.of_Register(dw_due_date_extensions_magext, "scale")

end event

event resize;call super::resize;long ll_height
This.X = w_pics_main.X
This.Y = w_pics_main.Y
ll_height = w_pics_main.mdi_1.Height
This.Resize(w_pics_main.WorkSpaceWidth(), ll_height)
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
Integer	li_rc, li_rtn
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
			li_rtn = cb_update.Event clicked()
			
			IF li_rtn = 1 THEN
				RETURN 0
			END IF
		Case 2
			// NO - Allow the window to be closed without saving changes
			Return 0
		Case 3
			// CANCEL -  Prevent the window from closing
			dw_due_date_extensions_magext.SetFocus()
	End Choose
End If

// Prevent the window from closing
ib_closestatus = False
Return 1
end event

event mousemove;call super::mousemove;w_pics_main.setmicrohelp('Ready')
end event

type dw_due_date_extensions from u_dw within w_magazine_due_date_extensions
event ue_enter_to_tab pbm_dwnprocessenter
event ue_hinttext pbm_mousemove
integer x = 91
integer y = 32
integer width = 2327
integer height = 580
integer taborder = 10
string dataobject = "d_magazine_due_date_extensions"
boolean vscrollbar = false
boolean livescroll = false
end type

event ue_enter_to_tab;call super::ue_enter_to_tab;Send(Handle(this), 256,9,Long(0,0))
Return (1)
end event

event ue_hinttext;call super::ue_hinttext;string ls_object, ls_column, ls_column_tag
long ll_pos

//This script set's microhelp at the bottom of the screen for the Archive Title Datawindow
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

event constructor;call super::constructor;SetTransObject(SqlServerTrans)
ib_rmbmenu = FALSE					//disable right mouse button
dw_due_date_extensions.Event pfc_addrow()
dw_due_date_extensions.SetFocus()
end event

event itemchanged;call super::itemchanged;String ls_magcode,ls_title, null_string, ls_issdt
Integer li_rtn_code
Date  ld_issuedate
datetime ldt_issuedate, null_date

SetNull(null_date)
SetNull(null_string)

//If magazine code entered, enable clear button and check for title.
IF DWO.Name = "magiss_magcd" THEN
	ls_magcode  = TRIM(GetText())
	li_rtn_code = wf_get_title(ls_magcode,ls_title)
	
	cb_clear.Enabled = TRUE
	IF li_rtn_code = 0 THEN
		dw_due_date_extensions.SetItem(row,'magttl_title',ls_title)
	ELSEIF li_rtn_code = 100 THEN
	  dw_due_date_extensions.Object.magiss_magcd.Validationmsg = " Magazine Code Not Found"
	  dw_due_date_extensions.SetItem(row,"magiss_magcd",null_string)
	  RETURN 1
	END IF //IF li_rtn_code = 0 THEN
		
END IF//IF DWO.Name = "magiss_magcd" THEN
	
	
	
//If Issuedate entered then retrieve date. Disabel first datawindow and
// enable the secons data window.
IF DWO.Name = "magiss_issdt" THEN
	ls_magcode = dw_due_date_extensions.GetItemString(row,"magiss_magcd")
	ls_issdt=mid(data,1, 10)
	ld_issuedate = Date(ls_issdt)
	ldt_issuedate=datetime(ld_issuedate,time('00:00:00'))
	ls_magcode = TRIM(ls_magcode)
	
	li_rtn_code = wf_check_data(ls_magcode, ld_issuedate)
	IF li_rtn_code = 0 THEN
		dw_due_date_extensions.Retrieve(ls_magcode,ldt_issuedate)
		dw_due_date_extensions.Enabled = FALSE
		dw_due_date_extensions_magext.Enabled = TRUE
		dw_due_date_extensions_magext.SetItem(1,"magcd",ls_magcode)
		dw_due_date_extensions_magext.SetItem(1,"issdt",ldt_issuedate)
		
		
	ELSEIF li_rtn_code = 100 THEN
	  dw_due_date_extensions.Object.magiss_issdt.Validationmsg = "No Data Found .. ~n Re-Enter New IssueDate or Click On Clear For New Query"
	  dw_due_date_extensions.SetItem(row,"magiss_issdt",null_date)
	  RETURN 1
	END IF
	
	
END IF//IF DWO.Name = "magiss_issdt" THEN 
end event

event itemfocuschanged;call super::itemfocuschanged;
IF DWO.Name = "magiss_issdt" THEN
	w_pics_main.Event pfc_microhelp("Enter Issue Date")
	dw_due_date_extensions.Event pfc_selectall()
END IF
end event

type cb_update from u_cb within w_magazine_due_date_extensions
event ue_hinttext pbm_mousemove
string tag = "Updates the Database"
integer x = 1147
integer y = 1244
integer taborder = 0
integer textsize = -10
string text = "&Update"
end type

event ue_hinttext;call super::ue_hinttext;w_pics_main.setmicrohelp(this.tag)
end event

event clicked;call super::clicked;Integer  li_extcaldays, li_ext , li_rc
String   ls_reason

dw_due_date_extensions.AcceptText()
dw_due_date_extensions_magext.AcceptText()


//Check to see if the extension reason field has an input
  ls_reason = dw_due_date_extensions_magext.Object.extrsn[1] 
  IF Trim(ls_reason) = "" OR IsNull(ls_reason) THEN
	MessageBox('Error',"Extension Reason Needs To Be Input")
	dw_due_date_extensions_magext.SetFocus()
	RETURN 0
  END IF




//Add the extension days to the cumulative extension days in the magiss table.
li_ext = dw_due_date_extensions.GetItemNumber(1,"magiss_extc")
IF IsNull(li_ext) THEN li_ext = 0

li_extcaldays = dw_due_date_extensions_magext.GetItemNumber(1,"ext")
IF IsNull(li_extcaldays) THEN li_extcaldays = 0

dw_due_date_extensions.SetItem(1,"magiss_extc",li_ext+li_extcaldays)



//Update the magiss table.
 li_rc = dw_due_date_extensions.Update(TRUE,FALSE)
 
 IF li_rc = 1 THEN
	COMMIT USING SqlServerTrans;
   li_rc = dw_due_date_extensions_magext.Update(TRUE,FALSE)
     IF li_rc = 1 THEN
		 COMMIT USING SqlServerTrans;
		 cb_clear.Event clicked()
		 Return 1
	  ELSE
       ROLLBACK USING SqlServerTrans;
		 MessageBox('Error','Error Updating magext table .. Contact Your DBA')
	  END IF
ELSE
	ROLLBACK USING SqlServerTrans;
	Messagebox('Error','Error Updating magiss table .. Contact Your DBA')
END IF


end event

type cb_clear from u_cb within w_magazine_due_date_extensions
event ue_hinttext pbm_mousemove
string tag = "Clears the screen for input"
integer x = 1577
integer y = 1244
integer taborder = 0
boolean bringtotop = true
integer textsize = -10
string text = "&Clear"
end type

event ue_hinttext;call super::ue_hinttext;w_pics_main.setmicrohelp(this.tag)
end event

event clicked;call super::clicked;dw_due_date_extensions.Reset()
dw_due_date_extensions.Event pfc_addrow()

dw_due_date_extensions_magext.Reset()
dw_due_date_extensions_magext.Event pfc_addrow()

cb_update.Enabled = FALSE
cb_clear.Enabled = FALSE
ib_disableclosequery=TRUE

dw_due_date_extensions_magext.Enabled = FALSE
dw_due_date_extensions.Enabled = TRUE

dw_due_date_extensions_magext.SetItem(1,"extdt",today())
dw_due_date_extensions.SetFocus()

end event

type cb_exit from u_cb within w_magazine_due_date_extensions
event ue_hinttext pbm_mousemove
string tag = "Exits the screen"
integer x = 2021
integer y = 1244
integer taborder = 0
boolean bringtotop = true
integer textsize = -10
string text = "E&xit"
end type

event ue_hinttext;call super::ue_hinttext;w_pics_main.setmicrohelp(this.tag)
end event

event clicked;call super::clicked;//dw_due_date_extensions.Reset()
//dw_due_date_extensions_magext.Reset()
//close(w_magazine_due_date_extensions)
//// If window is closed then popup the menu
//IF IsValid(w_magazine_due_date_extensions) = FALSE THEN
//   m_pics_main.m_menu.PopMenu ( 300, 0 ) 
//END IF
parent.event pfc_close()
end event

type dw_due_date_extensions_magext from u_dw within w_magazine_due_date_extensions
event ue_enter_to_key pbm_dwnprocessenter
event ue_hinttext pbm_mousemove
integer x = 91
integer y = 668
integer width = 2327
integer height = 472
integer taborder = 20
string dataobject = "d_magazine_due_date_extensions_magext"
boolean vscrollbar = false
boolean livescroll = false
end type

event ue_enter_to_key;call super::ue_enter_to_key;
Send(Handle(this), 256,9,Long(0,0))
RETURN 1
end event

event ue_hinttext;call super::ue_hinttext;string ls_object, ls_column, ls_column_tag
long ll_pos

//This script set's microhelp at the bottom of the screen for the Archive Title Datawindow
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

event constructor;call super::constructor;SetTransObject(sqlServerTrans)
this.Event pfc_addrow()
ib_rmbmenu = FALSE 					//disable right mouse button

//set todays date.
dw_due_date_extensions_magext.SetItem(1,"extdt",today())
end event

event updateend;call super::updateend;Long ll_total

ll_total = rowsinserted + rowsupdated

IF ll_total > 0 THEN
	MessageBox('Update','Updated Row(s) successfully')
END IF

end event

event itemchanged;call super::itemchanged;String ls_reason


//
IF DWO.Name = 'extrsn' THEN
 ls_reason = Data
   IF Trim(ls_reason) = "" OR IsNull(ls_reason) THEN
		RETURN 1
	END IF
END IF
end event

event editchanged;call super::editchanged;String ls_reason


//If reason field is filled then enable the update button
  IF dwo.name = "extrsn" THEN
	ls_reason = data
	IF TRIM(ls_reason) <> "" THEN
	  cb_update.Enabled = TRUE
   END IF
  END IF
end event


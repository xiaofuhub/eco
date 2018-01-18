$PBExportHeader$w_copyright_permission.srw
$PBExportComments$This screen will allow copyright informations to be edited and stored in the PICS database
forward
global type w_copyright_permission from w_sheet
end type
type cb_find from u_cb within w_copyright_permission
end type
type cb_update from u_cb within w_copyright_permission
end type
type cb_clear from u_cb within w_copyright_permission
end type
type cb_exit from u_cb within w_copyright_permission
end type
type dw_crrest from u_dw within w_copyright_permission
end type
type dw_copyright_cr from u_dw within w_copyright_permission
end type
type dw_copyright_permission from u_dw within w_copyright_permission
end type
type dw_acquist_copy_permission from u_dw within w_copyright_permission
end type
end forward

global type w_copyright_permission from w_sheet
integer x = 110
integer y = 172
integer width = 2450
integer height = 1584
string title = "Copyright Permission"
cb_find cb_find
cb_update cb_update
cb_clear cb_clear
cb_exit cb_exit
dw_crrest dw_crrest
dw_copyright_cr dw_copyright_cr
dw_copyright_permission dw_copyright_permission
dw_acquist_copy_permission dw_acquist_copy_permission
end type
global w_copyright_permission w_copyright_permission

type variables
string is_conno
string is_chno
string is_syntax[6]
boolean query_started=FALSE
end variables

forward prototypes
public subroutine wf_update (boolean ab_update)
end prototypes

public subroutine wf_update (boolean ab_update);string ls_update

IF ab_update THEN
	ls_update = "Yes"
ELSE
	ls_update = "No"
END IF
	dw_copyright_cr.Visible = ab_update
//	dw_acquist_copy_permission.Visible = ab_update
	dw_crrest.Visible = ab_update
	






end subroutine

on w_copyright_permission.create
int iCurrent
call super::create
this.cb_find=create cb_find
this.cb_update=create cb_update
this.cb_clear=create cb_clear
this.cb_exit=create cb_exit
this.dw_crrest=create dw_crrest
this.dw_copyright_cr=create dw_copyright_cr
this.dw_copyright_permission=create dw_copyright_permission
this.dw_acquist_copy_permission=create dw_acquist_copy_permission
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.cb_find
this.Control[iCurrent+2]=this.cb_update
this.Control[iCurrent+3]=this.cb_clear
this.Control[iCurrent+4]=this.cb_exit
this.Control[iCurrent+5]=this.dw_crrest
this.Control[iCurrent+6]=this.dw_copyright_cr
this.Control[iCurrent+7]=this.dw_copyright_permission
this.Control[iCurrent+8]=this.dw_acquist_copy_permission
end on

on w_copyright_permission.destroy
call super::destroy
destroy(this.cb_find)
destroy(this.cb_update)
destroy(this.cb_clear)
destroy(this.cb_exit)
destroy(this.dw_crrest)
destroy(this.dw_copyright_cr)
destroy(this.dw_copyright_permission)
destroy(this.dw_acquist_copy_permission)
end on

event mousemove;call super::mousemove;w_pics_main.setmicrohelp("Ready")
end event

event pfc_postopen;dw_copyright_permission.Modify("ttlinit_cryr.Tabsequence = 0")
dw_copyright_permission.Modify("ttlinit_crflag.Tabsequence = 0")
dw_copyright_permission.Modify("ttlinit_crname.Tabsequence = 0")
dw_copyright_permission.Modify("cf_crname.Tabsequence = 0")
m_pics_main.m_Edit.m_Addrow.Enabled = FALSE
m_pics_main.m_Edit.m_Deleterow.Enabled = FALSE
m_pics_main.m_Edit.m_Cut.Enabled = FALSE
//dw_copyright_permission.Event pfc_insertrow()
//dw_copyright_permission.setfocus()
cb_update.Enabled = FALSE
cb_clear.Enabled = FALSE
wf_update(FALSE)
dw_acquist_copy_permission.Visible = FALSE
dw_copyright_permission.setfocus()

end event

event pfc_preopen;call super::pfc_preopen;this.of_SetPreference(TRUE)
this.inv_preference.of_SetToolBars(TRUE)
this.inv_preference.of_SetWindow(TRUE)
this.of_SetResize(TRUE)
this.inv_resize.of_SetOrigSize(this.workspacewidth(),this.workspaceheight())
inv_resize.of_Register(dw_copyright_permission, "scale")
inv_resize.of_Register(dw_copyright_cr,"Scale")
inv_resize.of_Register(dw_acquist_copy_permission,"Scale")
inv_resize.of_Register(dw_crrest,"scale")
inv_resize.of_Register(cb_find, "scale")
inv_resize.of_Register(cb_update, "scale")
inv_resize.of_Register(cb_clear, "scale")
inv_resize.of_Register(cb_exit, "scale")
end event

event resize;call super::resize;//long ll_height
//This.X = w_pics_main.X
//This.Y = w_pics_main.Y
//ll_height = w_pics_main.mdi_1.Height
//This.Resize(w_pics_main.WorkSpaceWidth(), ll_height)
end event

event open;call super::open;THIS.Windowstate = maximized!
end event

event closequery;Integer	li_pendingrc
Integer	li_validationrc
Integer	li_accepttextrc
Integer	li_msg
Integer	li_rc,rtn
String	ls_msgparms[]

IF query_started=FALSE THEN
	ib_disableclosequery = TRUE
END IF

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
					dw_copyright_permission.Setfocus()
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
//			IF THIS.Event pfc_save() >= 1 THEN
//				// Successful update, allow the window to be closed
//				Return 0
//			End If
		Case 2
			// NO - Allow the window to be closed without saving changes
			Return 0
		Case 3
			dw_copyright_permission.Setfocus()
			// CANCEL -  Prevent the window from closing
	End Choose
End If

// Prevent the window from closing
ib_closestatus = False
Return 1
end event

type cb_find from u_cb within w_copyright_permission
event pfc_hinttext pbm_mousemove
string tag = "Press this button to find~'s the record based on valid control number"
integer x = 539
integer y = 1368
integer taborder = 0
integer textsize = -10
string text = "F&ind"
boolean default = true
end type

event pfc_hinttext;call super::pfc_hinttext;w_pics_main.setmicrohelp(This.tag)
end event

event clicked;call super::clicked;string ls_conno, ls_find, ls_dummy,ls_flag,ls_chno
long ll_count, i, ll_find, ll_rows

query_started = TRUE
dw_copyright_permission.AcceptText()
ls_conno = dw_copyright_permission.GetItemString(1, "mchar_conno")

IF len(ls_conno) > 0 THEN
	ls_find = "conno"
ELSE
	MessageBox("Invalid Find Criteria", "Please enter Valid Control NumberTo Retrieve The Record.") 
	dw_copyright_permission.SetFocus()
	RETURN
END IF
IF ls_find = "conno" THEN
	IF len(ls_conno) <> 8 THEN
		MessageBox("Invalid Control Number", "The Control Number must " + &
			"be an eight characters number.  Please reenter the " + &
			"control number")
		dw_copyright_permission.SetItem(1, "mchar_conno", "")
		dw_copyright_permission.SetFocus()
		RETURN
	END IF
	SELECT mchar.conno,mchar.chno 
	    INTO :is_conno,:is_chno 
	    FROM mchar
  	 WHERE mchar.conno = :ls_conno
		USING SQLservertrans ;
	
	IF NOT is_conno = ls_conno THEN
		MessageBox("Invalid Control Number", "The Control Number is " + &
			"invalid.  Please reenter the correct control number")
		dw_copyright_permission.SetItem(1, "mchar_conno", "")
		dw_copyright_permission.SetFocus()
		RETURN
	ELSE
		ll_rows = dw_copyright_permission.Event pfc_retrieve()		
		ll_rows = dw_copyright_cr.Event pfc_retrieve()		
		dw_copyright_permission.Setfocus()
		dw_copyright_permission.setcolumn("ttlinit_cryr")
		IF ll_rows = 0 THEN
			dw_copyright_cr.Event pfc_insertrow()
			dw_copyright_cr.Setitem(1,"chno",is_chno)
		END IF
		ls_flag = dw_copyright_permission.getitemstring(1,"ttlinit_crflag")
		IF ISNULL(ls_flag) OR ls_flag = "" THEN
			dw_copyright_permission.Setitem(1,"ttlinit_crflag","Y")
		END IF
		IF ls_flag = "N" THEN
			wf_update(FALSE)
		ELSE
			wf_update(TRUE)
		END IF
		dw_acquist_copy_permission.Event pfc_retrieve()
		dw_crrest.Event pfc_retrieve()
		dw_copyright_permission.Setfocus()
		cb_find.Enabled = FALSE
		cb_update.Enabled = TRUE
		cb_clear.Enabled = TRUE
	END IF
	w_pics_main.setmicrohelp("Ready")
END IF
dw_copyright_permission.of_setupdateable(TRUE)
cb_find.Default = FALSE
ib_disableclosequery=TRUE		

end event

type cb_update from u_cb within w_copyright_permission
event pfc_hinttext pbm_mousemove
string tag = "Press this button to update the record to the database"
integer x = 1019
integer y = 1368
integer taborder = 0
integer textsize = -10
string text = "&Update"
end type

event pfc_hinttext;call super::pfc_hinttext;w_pics_main.setmicrohelp(This.tag)
end event

event clicked;string ls_flag,ls_medium,Lcode,Lchno,ls_name,lconno
long i,ll_rows,rtn
integer ls_date,li_name,li_count
date ld_date,ld_dates

IF dw_crrest.Accepttext()=1 AND &
	dw_copyright_permission.Accepttext()=1 AND &
	dw_acquist_copy_permission.Accepttext()=1 THEN

	//Checking for the year inbetween 1752 and current year
	
	li_name = dw_copyright_permission.GetitemNumber(1,"cf_crname")
	IF li_name = 1 THEN
		ls_date = dw_copyright_permission.GetitemNumber(1,"ttlinit_cryr")
		IF ls_date < 1752 OR ls_date > Year(today()) THEN
			Messagebox("Invalid year","Please enter the year in between 1752 and current year.")
			dw_copyright_permission.setfocus()
			dw_copyright_permission.Setcolumn("ttlinit_cryr")
			RETURN 2
		END IF
	END IF
	
	
	ls_name = dw_copyright_permission.getitemstring(1,"ttlinit_crname")
	IF ISNULL(ls_name) OR ls_name = "" OR TRIM(ls_name) = "" THEN
		Messagebox("Required data Missing","You must supply the copyright holder name before updating the record.",Information!)
		dw_copyright_permission.Setfocus()
		dw_copyright_permission.setcolumn("ttlinit_crname")
		RETURN 2
	END IF
	
	ls_flag = dw_copyright_permission.Getitemstring(1,"ttlinit_crflag")
	IF ls_flag = "Y" THEN
		ll_rows = dw_crrest.rowcount()
		FOR i = ll_rows TO 1 step -1
			ls_medium = dw_crrest.getitemstring(i,"crresmed")
			IF ls_medium = ""  or ISNULL(ls_medium) THEN
				dw_crrest.Deleterow(i)
			END IF
		NEXT
		rtn = Parent.Event pfc_save()
		IF rtn = 0 THEN
			Messagebox("Update","Nothing to update.")
			RETURN 0
		END IF
		IF rtn = 1 THEN
			dw_copyright_cr.update(TRUE,TRUE)
			dw_crrest.update(TRUE,TRUE)
			dw_acquist_copy_permission.update(TRUE,TRUE)
			COMMIT USING sqlservertrans ;
			Messagebox("Update","Update Successful.")
			RETURN 1
		ELSE
			ROLLBACK USING sqlservertrans ;
		END IF
		
	ELSEIF ls_flag = "N" THEN
		dw_copyright_cr.of_setupdateable(FALSE)
		dw_acquist_copy_permission.of_setupdateable(TRUE)
		dw_crrest.of_setupdateable(FALSE)
		DELETE FROM cr
		WHERE cr.chno = :is_chno
		USING sqlservertrans;
		IF f_check_dberror(sqlservertrans,"CR")=FALSE THEN
			RETURN
		END IF
	//	DELETE FROM acquist
	//	WHERE acquist.chno = :is_chno
	//	USING sqlservertrans;
		DELETE FROM crrest
		WHERE crrest.chno = :is_chno
		USING sqlservertrans;
		IF f_check_dberror(sqlservertrans,"CRREST")=FALSE THEN
			RETURN
		END IF
		Parent.Event pfc_save()
		// Messagebox("Succesful","Removing of copyright request information successful")
		dw_copyright_cr.of_setupdateable(TRUE)
		dw_acquist_copy_permission.of_setupdateable(TRUE)
		dw_crrest.of_setupdateable(TRUE)
		dw_copyright_cr.Reset()
	//	dw_acquist_copy_permission.Reset()
		dw_crrest.Reset()
		COMMIT USING sqlservertrans ;
		Messagebox("Update","Update Successful.")
	END IF
	// Mark the MCHAR table
	lconno = dw_copyright_permission.object.mchar_conno[1]
	f_update_mchar_time(lconno,0,"C","U")
	RETURN 1
END IF


	
	
end event

type cb_clear from u_cb within w_copyright_permission
event pfc_hinttext pbm_mousemove
string tag = "Press this button to clear the record from the screen"
integer x = 1509
integer y = 1368
integer taborder = 0
integer textsize = -10
string text = "&Clear"
end type

event pfc_hinttext;call super::pfc_hinttext;w_pics_main.setmicrohelp(This.tag)
end event

event clicked;call super::clicked;dw_copyright_permission.Modify("mchar_conno.Tabsequence = 10")
dw_copyright_permission.Modify("ttlinit_cryr.Tabsequence = 0")
dw_copyright_permission.Modify("ttlinit_crflag.Tabsequence = 0")
dw_copyright_permission.Modify("cf_crname.Tabsequence = 0")
dw_copyright_permission.Modify("ttlinit_crname.Tabsequence = 0")
dw_copyright_permission.of_setupdateable(FALSE)
dw_copyright_permission.Reset()
dw_copyright_cr.Reset()
dw_acquist_copy_permission.Reset()
dw_crrest.Reset()
dw_copyright_permission.event pfc_addrow()
cb_find.enabled = TRUE
cb_update.Enabled = FALSE
cb_clear.Enabled = FALSE
ib_disableclosequery = FALSE
dw_copyright_permission.Setfocus()
wf_update(FALSE)
dw_acquist_copy_permission.Visible = FALSE
cb_find.Default = TRUE



end event

type cb_exit from u_cb within w_copyright_permission
event pfc_hinttext pbm_mousemove
string tag = "Press this button to exit the current window"
integer x = 1993
integer y = 1368
integer taborder = 0
integer textsize = -10
string text = "E&xit"
end type

event pfc_hinttext;call super::pfc_hinttext;w_pics_main.setmicrohelp(This.tag)
end event

event clicked;call super::clicked;string ls_conno

ls_conno = dw_copyright_permission.object.mchar_conno[1]
IF ISNULL(ls_conno) OR ls_conno = "" THEN
	ib_disableclosequery = TRUE
END IF
parent.Event pfc_close()
m_pics_main.m_menu.popMenu(300,0)
end event

type dw_crrest from u_dw within w_copyright_permission
event pfc_hinttext pbm_mousemove
event ue_enterkey pbm_dwnprocessenter
event ue_downkey pbm_dwntabout
integer y = 1072
integer width = 2405
integer height = 284
integer taborder = 40
string dataobject = "d_crrest"
boolean vscrollbar = false
boolean livescroll = false
boolean ib_isupdateable = false
boolean ib_rmbmenu = false
boolean ib_rmbfocuschange = false
string is_updatesallowed = ""
end type

event pfc_hinttext;call super::pfc_hinttext;string ls_object, ls_column, ls_column_tag
long ll_pos

//This script set's microhelp For crrest Datawindow (Bottom)
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

event ue_enterkey;call super::ue_enterkey;Send(Handle(dw_crrest),256,9,Long(0,0))
return(1)
end event

event ue_downkey;call super::ue_downkey;long ll_row
string ls_med

IF Rowcount() < 2 THEN
	IF Rowcount() = 0 THEN
		dw_crrest.Event pfc_addrow()
		dw_crrest.Setfocus()
		dw_crrest.Setrow(ll_row)
	END IF
	ls_med = Getitemstring(Rowcount(),"crresmed")
	IF NOT ISNULL(ls_med) OR ls_med <> "" THEN
		dw_crrest.Event pfc_addrow()
		dw_crrest.Setfocus()
		dw_crrest.Setrow(ll_row)
		RETURN 1
	END IF
END IF
end event

event constructor;call super::constructor;dw_crrest.of_settransobject(sqlservertrans)
ib_rmbmenu = FALSE

end event

event pfc_retrieve;call super::pfc_retrieve;RETURN THIS.Retrieve(is_chno)
end event

event retrievestart;call super::retrievestart;Open(w_pics_retrieve_msg_box)
end event

event getfocus;call super::getfocus;long ll_row

m_pics_main.m_Edit.m_Addrow.Enabled = TRUE
m_pics_main.m_Edit.m_Cut.Enabled = FALSE
m_pics_main.m_Edit.m_Deleterow.Enabled = TRUE
IF dw_crrest.Rowcount() = 0 THEN
	ll_row = THIS.Event pfc_addrow()
	dw_crrest.Setrow(ll_row)
END IF
end event

event losefocus;call super::losefocus;m_pics_main.m_Edit.m_Addrow.Enabled = FALSE
m_pics_main.m_Edit.m_Deleterow.Enabled = FALSE
end event

event pfc_addrow;call super::pfc_addrow;long ll_rc

ll_rc = THIS.Rowcount()
THIS.Setitem(ll_rc,"chno",is_chno)
THIS.Setrow(ll_rc)
THIS.Scrolltorow(ll_rc)
THIS.Setcolumn("crresqty")
RETURN ll_rc
end event

event sqlpreview;call super::sqlpreview;//
end event

event retrieveend;call super::retrieveend;close(w_pics_retrieve_msg_box)
end event

event dberror;IF sqldbcode = -239 THEN
	Messagebox("INSERT ERROR","Duplicate Copyright Restriction Medium Can't be Allowed.",STOPSIGN!)
	dw_crrest.Setfocus()
	RETURN 1
END IF
	
end event

type dw_copyright_cr from u_dw within w_copyright_permission
event pfc_hinttext pbm_mousemove
event ue_enterkey pbm_dwnprocessenter
integer y = 404
integer width = 2405
integer height = 336
integer taborder = 20
string dataobject = "d_copyright_cr"
boolean vscrollbar = false
boolean livescroll = false
boolean ib_isupdateable = false
boolean ib_rmbmenu = false
boolean ib_rmbfocuschange = false
string is_updatesallowed = ""
end type

event pfc_hinttext;call super::pfc_hinttext;string ls_object, ls_column, ls_column_tag
long ll_pos

//This script set's microhelp For Copyright_cr Datawindow (Middle)
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

event ue_enterkey;call super::ue_enterkey;Send(Handle(dw_copyright_cr),256,9,Long(0,0))
return(1)
end event

event constructor;call super::constructor;dw_copyright_cr.of_settransobject(sqlservertrans)
ib_rmbmenu = FALSE
this.of_SetDropDownCalendar(TRUE)
this.iuo_calendar.of_Register("crodt",this.iuo_calendar.DDLB)
this.iuo_calendar.of_Register("crgdt",this.iuo_calendar.DDLB)

end event

event pfc_retrieve;call super::pfc_retrieve;RETURN THIS.Retrieve(is_chno)
end event

event retrievestart;call super::retrievestart;Open(w_pics_retrieve_msg_box)
end event

event getfocus;call super::getfocus;m_pics_main.m_Edit.m_Deleterow.Enabled = FALSE
m_pics_main.m_Edit.m_Cut.Enabled = FALSE
m_pics_main.m_Edit.m_Addrow.Enabled = FALSE
end event

event sqlpreview;call super::sqlpreview;//MessageBox("sql",sqlsyntax)
end event

event retrieveend;call super::retrieveend;close(w_pics_retrieve_msg_box)
end event

event itemchanged;call super::itemchanged;Datetime ld_crodt
String ls_msg

ld_crodt = this.object.crodt[row]

IF DWO.Name = "crgdt" THEN
	IF	datetime(data) < ld_crodt THEN
		ls_msg = "Received Date can not be earlier than request copyright date"
		this.object.crgdt.Validationmsg =ls_msg
		RETURN 1
	END IF
END IF
end event

type dw_copyright_permission from u_dw within w_copyright_permission
event pfc_hinttext pbm_mousemove
event ue_enterkey pbm_dwnprocessenter
integer width = 2405
integer height = 404
integer taborder = 10
string dataobject = "d_copyright_permission"
boolean vscrollbar = false
boolean livescroll = false
boolean ib_isupdateable = false
boolean ib_rmbmenu = false
boolean ib_rmbfocuschange = false
string is_updatesallowed = ""
end type

event pfc_hinttext;call super::pfc_hinttext;string ls_object, ls_column, ls_column_tag
long ll_pos

//This script set's microhelp For Copyright Permission Datawindow (Upper)
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

event ue_enterkey;call super::ue_enterkey;Send(Handle(dw_copyright_permission),256,9,Long(0,0))
return(1)
end event

event constructor;call super::constructor;dw_copyright_permission.of_settransobject(sqlservertrans)
dw_copyright_permission.of_setupdateable(TRUE)
ib_rmbmenu = FALSE

end event

event pfc_retrieve;call super::pfc_retrieve;RETURN dw_copyright_permission.Retrieve(is_conno)
end event

event retrievestart;call super::retrievestart;Open(w_pics_retrieve_msg_box)
end event

event itemchanged;call super::itemchanged;string ls_date,ls_chno,ls_name
integer ll_rc
long ll_year

SetNull(ll_year)

IF dwo.name = "ttlinit_cryr" THEN
	IF long(data) < 1752 OR long(data) > Year(today()) THEN
		Messagebox("Invalid Year","Please enter the year between 1752 and current year",Information!)
		dw_copyright_permission.Setfocus()
		dw_copyright_permission.Setcolumn("ttlinit_cryr")
		RETURN 1
	END IF
END IF

IF dwo.name = "ttlinit_crflag" THEN
	IF data = "Y" THEN
		wf_update(TRUE)
	ELSE
		wf_update(FALSE)
		Messagebox("Delete","Remove copyright information",Information!)
		END IF
	RETURN
END IF
IF dwo.name = "ttlinit_crname" THEN
	IF NOT data = "Public Domain" THEN
		dw_copyright_permission.object.cf_crname[row]=1
	END IF
	IF len(data) > 0 THEN
		ls_date = string(dw_copyright_cr.object.crodt[1],'mm/dd/yyyy')
		IF ISNULL(ls_date) OR NOT ISDate(ls_date) THEN
			dw_copyright_cr.object.crodt[1]=today()
		END IF
		ls_chno = dw_copyright_cr.object.chno[1]
		IF ISNULL(ls_chno) AND ls_chno = "" THEN
			dw_copyright_cr.object.chno[1]=is_chno
		END IF
	ELSE
		SetNull(ls_date)
		SetNull(ls_chno)
		dw_copyright_cr.object.crodt[1]=ls_date
		END IF
	RETURN
END IF
IF dwo.name = "cf_crname" THEN
	ls_name = dw_copyright_permission.object.ttlinit_crname[row]
	IF long(data) = 1 THEN
		dw_copyright_permission.object.ttlinit_crname[row]=""
	ELSE
		dw_copyright_permission.object.ttlinit_cryr[row]=ll_year
		dw_copyright_permission.object.ttlinit_crname[row]="Public Domain"
	END IF
	RETURN
END IF
		
	
end event

event getfocus;call super::getfocus;m_pics_main.m_Edit.m_Addrow.Enabled = FALSE
m_pics_main.m_Edit.m_Deleterow.Enabled = FALSE
m_pics_main.m_Edit.m_Cut.Enabled = FALSE
end event

event retrieveend;call super::retrieveend;string ls_name

close(w_pics_retrieve_msg_box)

ls_name = getitemstring(rowcount,"ttlinit_crname")
IF Pos(ls_name,"Public Domain") > 0 THEN
	dw_copyright_permission.Setitem(rowcount,"cf_crname",0)
ELSE
	dw_copyright_permission.Setitem(rowcount,"cf_crname",1)
END IF
dw_copyright_permission.Modify("mchar_conno.Tabsequence = 0")
dw_copyright_permission.Modify("ttlinit_cryr.Tabsequence = 20")
dw_copyright_permission.Modify("ttlinit_crflag.Tabsequence = 30")
dw_copyright_permission.Modify("cf_crname.Tabsequence = 40")
dw_copyright_permission.Modify("ttlinit_crname.Tabsequence = 50")

end event

event sqlpreview;call super::sqlpreview;//
end event

event itemerror;call super::itemerror;IF dwo.name = "ttlinit_cryr" THEN
	IF Getitemnumber(row,"cf_crname") = 1 THEN
		RETURN 1
	END IF
END IF
end event

type dw_acquist_copy_permission from u_dw within w_copyright_permission
event ue_enterkey pbm_dwnprocessenter
event pfc_hinttext pbm_mousemove
integer y = 744
integer width = 2405
integer height = 332
integer taborder = 30
string dataobject = "d_acquist_copy_permission"
boolean vscrollbar = false
boolean livescroll = false
boolean ib_isupdateable = false
boolean ib_rmbmenu = false
boolean ib_rmbfocuschange = false
string is_updatesallowed = ""
end type

event ue_enterkey;call super::ue_enterkey;Send(Handle(dw_acquist_copy_permission),256,9,Long(0,0))
return(1)
end event

event pfc_hinttext;call super::pfc_hinttext;string ls_object, ls_column, ls_column_tag
long ll_pos

//This script set's microhelp For Acquist_copy_permission Datawindow (Third)
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

event constructor;call super::constructor;dw_acquist_copy_permission.of_settransobject(sqlservertrans)
ib_rmbmenu = FALSE
this.of_SetDropDownCalendar(TRUE)
this.iuo_calendar.of_Register("pbordt",this.iuo_calendar.DDLB)
this.iuo_calendar.of_Register("pbrecdt",this.iuo_calendar.DDLB)

end event

event getfocus;call super::getfocus;m_pics_main.m_Edit.m_Deleterow.Enabled = FALSE
m_pics_main.m_Edit.m_Addrow.Enabled = FALSE
m_pics_main.m_Edit.m_Cut.Enabled = FALSE
end event

event pfc_retrieve;call super::pfc_retrieve;RETURN THIS.Retrieve(is_chno)
end event

event sqlpreview;call super::sqlpreview;//
end event

event retrieveend;call super::retrieveend;long ll_row

close(w_pics_retrieve_msg_box)
dw_acquist_copy_permission.Visible = TRUE
IF rowcount = 0 THEN
	ll_row = dw_acquist_copy_permission.Event pfc_addrow()
	dw_acquist_copy_permission.SetItem(ll_row, "chno", is_chno)
END IF
end event

event retrievestart;call super::retrievestart;Open(w_pics_retrieve_msg_box)
end event

event itemchanged;call super::itemchanged;Datetime ld_pbordt
String ls_msg

ld_pbordt = this.object.pbordt[row]

IF DWO.Name = "pbrecdt" THEN
	IF	datetime(data) < ld_pbordt THEN
		ls_msg = "Received Date can not be earlier than order date"
		this.object.pbrecdt.Validationmsg =ls_msg
		RETURN 1
	END IF
END IF
end event


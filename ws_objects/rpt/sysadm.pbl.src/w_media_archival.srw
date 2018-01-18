$PBExportHeader$w_media_archival.srw
forward
global type w_media_archival from w_sheet
end type
type dw_archive_title from u_dw within w_media_archival
end type
type dw_medium_archival from u_dw within w_media_archival
end type
type cb_find from u_cb within w_media_archival
end type
type cb_update from u_cb within w_media_archival
end type
type cb_clear from u_cb within w_media_archival
end type
type cb_close from u_cb within w_media_archival
end type
end forward

global type w_media_archival from w_sheet
integer x = 78
integer y = 184
integer width = 3035
integer height = 1460
string title = "Media Archival"
dw_archive_title dw_archive_title
dw_medium_archival dw_medium_archival
cb_find cb_find
cb_update cb_update
cb_clear cb_clear
cb_close cb_close
end type
global w_media_archival w_media_archival

type variables
string is_chno,Lgroupid
string is_conno
string is_syntax[6]

end variables

on w_media_archival.create
int iCurrent
call super::create
this.dw_archive_title=create dw_archive_title
this.dw_medium_archival=create dw_medium_archival
this.cb_find=create cb_find
this.cb_update=create cb_update
this.cb_clear=create cb_clear
this.cb_close=create cb_close
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_archive_title
this.Control[iCurrent+2]=this.dw_medium_archival
this.Control[iCurrent+3]=this.cb_find
this.Control[iCurrent+4]=this.cb_update
this.Control[iCurrent+5]=this.cb_clear
this.Control[iCurrent+6]=this.cb_close
end on

on w_media_archival.destroy
call super::destroy
destroy(this.dw_archive_title)
destroy(this.dw_medium_archival)
destroy(this.cb_find)
destroy(this.cb_update)
destroy(this.cb_clear)
destroy(this.cb_close)
end on

event pfc_postopen;call super::pfc_postopen;dw_archive_title.setfocus()
is_syntax[1] = dw_archive_title.Object.DataWindow.Table.Select
is_syntax[3] = dw_medium_archival.Object.DataWindow.Table.Select
cb_update.Enabled = FALSE
cb_clear.Enabled = FALSE
m_pics_main.m_Edit.m_Addrow.Enabled = FALSE
m_pics_main.m_Edit.m_Deleterow.Enabled = FALSE
m_pics_main.m_Edit.m_Cut.Enabled = FALSE
dw_medium_archival.Visible = FALSE

end event

event pfc_preopen;call super::pfc_preopen;this.of_SetPreference(TRUE)
this.inv_preference.of_SetToolBars(TRUE)
this.inv_preference.of_SetWindow(TRUE)
this.of_SetResize(TRUE)
this.inv_resize.of_SetOrigSize(this.workspacewidth(),this.workspaceheight())
inv_resize.of_Register(dw_archive_title, "scale")
inv_resize.of_Register(dw_medium_archival, "scale")
inv_resize.of_Register(cb_clear, "scale")
inv_resize.of_Register(cb_close, "scale")
inv_resize.of_Register(cb_find, "scale")
inv_resize.of_Register(cb_update, "scale")
end event

event resize;call super::resize;//long ll_height
//This.X = w_pics_main.X
//This.Y = w_pics_main.Y
//ll_height = w_pics_main.mdi_1.Height
//This.Resize(w_pics_main.WorkSpaceWidth(), ll_height)
end event

event mousemove;call super::mousemove;w_pics_main.setmicrohelp("Ready")
end event

event closequery;Integer	li_pendingrc
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
					dw_medium_archival.Setfocus()
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
//				
//				// Successful update, allow the window to be closed
//				Return 0
//			End If
		Case 2
			// NO - Allow the window to be closed without saving changes
			Return 0
		Case 3
			dw_medium_archival.Setfocus()
			// CANCEL -  Prevent the window from closing
	End Choose
End If

// Prevent the window from closing
ib_closestatus = False
Return 1
end event

event open;call super::open;string Luid
Luid = gnv_app.of_GetUserId()
select group_ into :Lgroupid from picsuser where userid=:Luid using sqlservertrans;
Lgroupid = TRIM(Lgroupid)
IF Lgroupid<>'CDS' AND Lgroupid<>'ADMIN' THEN
	MessageBox("ERROR", "You are not authorized to use this screen. Your groupid must be CDS or ADMIN. ")
	close(this)
	RETURN -1
ELSE
	THIS.Windowstate = maximized!
END IF
end event

type dw_archive_title from u_dw within w_media_archival
event pfc_hinttext pbm_mousemove
event ue_enterkey pbm_dwnprocessenter
integer x = 14
integer y = 28
integer width = 2949
integer height = 572
integer taborder = 10
string dataobject = "d_archival_control"
boolean vscrollbar = false
boolean livescroll = false
boolean ib_isupdateable = false
boolean ib_rmbmenu = false
boolean ib_rmbfocuschange = false
string is_updatesallowed = ""
end type

event pfc_hinttext;call super::pfc_hinttext;string ls_object, ls_column, ls_column_tag
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

event ue_enterkey;call super::ue_enterkey;Send(Handle(this),256,9,Long(0,0))
return(1)
end event

event pfc_retrieve;call super::pfc_retrieve;This.Reset()
return this.retrieve(is_chno)
end event

event constructor;call super::constructor;dw_archive_title.of_settransobject(SQLservertrans)
InsertRow(0)
ib_rmbmenu = FALSE
of_SetUpdateable(FALSE)

end event

event retrievestart;call super::retrievestart;w_pics_main.setmicrohelp("Retrieving the Record Please Wait.....")
end event

event rbuttondown;//
end event

event rbuttonup;//
end event

event getfocus;call super::getfocus;m_pics_main.m_Edit.m_Addrow.Enabled = FALSE
m_pics_main.m_Edit.m_Deleterow.Enabled = FALSE
m_pics_main.m_Edit.m_Cut.Enabled = FALSE

end event

event retrieveend;call super::retrieveend;dw_medium_archival.Modify("mchar_conno.Tabsequence = 0")
dw_medium_archival.Modify("mchar_chno.Tabsequence = 0")
end event

type dw_medium_archival from u_dw within w_media_archival
event pfc_hinttext pbm_mousemove
event ue_enterkey pbm_dwnprocessenter
integer x = 14
integer y = 616
integer width = 2953
integer height = 520
integer taborder = 20
string dataobject = "d_arrsn"
boolean hscrollbar = true
boolean vscrollbar = false
boolean livescroll = false
boolean ib_isupdateable = false
boolean ib_rmbmenu = false
boolean ib_rmbfocuschange = false
string is_updatesallowed = ""
end type

event pfc_hinttext;call super::pfc_hinttext;string ls_object, ls_column, ls_column_tag
long ll_pos

//This script displays the microhelp at the bottom of the screen for the Medium Archival Datawindow
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

event ue_enterkey;call super::ue_enterkey;Send(Handle(this),256,9,Long(0,0))
return(1)
end event

event pfc_retrieve;call super::pfc_retrieve;Return This.Retrieve(is_chno)
end event

event constructor;call super::constructor;dw_medium_archival.of_settransobject(SQLservertrans)
ib_rmbmenu = FALSE
of_SetMultiTable(TRUE)


end event

event retrievestart;call super::retrievestart;dw_medium_archival.Visible = TRUE
w_pics_main.setmicrohelp("Retrieving the Record Please Wait.....")
end event

event rbuttondown;//
end event

event getfocus;call super::getfocus;m_pics_main.m_Edit.m_Addrow.Enabled = FALSE
m_pics_main.m_Edit.m_Deleterow.Enabled = FALSE
m_pics_main.m_Edit.m_Cut.Enabled = FALSE

end event

event retrieveend;call super::retrieveend;dw_medium_archival.Modify("mchar_conno.Tabsequence = 0")
dw_medium_archival.Modify("mchar_chno.Tabsequence = 0")
end event

event clicked;call super::clicked;If dwo.name = "mchar_arflag"  THEN
	string ls_flag
	
	ls_flag = dw_medium_archival.object.mchar_arflag[1]
	IF NOT ISNULL(ls_flag) THEN 
	   Messagebox("ERROR","You can not unarchive once it is archived.")
	   RETURN 1
   END IF
END IF

end event

type cb_find from u_cb within w_media_archival
event pfc_hinttext pbm_mousemove
string tag = "Find~'s the Record based on Control Number and Chart Number"
integer x = 1024
integer y = 1240
integer taborder = 0
integer textsize = -10
string text = "F&ind"
boolean default = true
end type

event pfc_hinttext;call super::pfc_hinttext;w_pics_main.setmicrohelp(This.tag)
end event

event clicked;call super::clicked;string ls_conno, ls_chno, ls_find, ls_dummy,ls_reason,ls_archive
long ll_count, i, ll_find

dw_archive_title.AcceptText()
ls_conno = dw_archive_title.GetItemString(1, "mchar_conno")
ls_chno = dw_archive_title.GetItemString(1,"ttlinit_chno")

IF len(ls_conno) > 0 THEN
	ls_find = "conno"
ELSEIF len(ls_chno) > 0 THEN
	ls_find = "chno"
ELSE
	MessageBox("Invalid Find Criteria", "Please enter either " + & 
		"control number or chart number to retrieve the record.")
	dw_archive_title.SetFocus()
	RETURN
END IF

IF ls_find = "conno" THEN
	IF len(ls_conno) <> 8 THEN
		MessageBox("Invalid Control Number", "The Control Number must " + &
			"be an eight characters number.  Please reenter the " + &
			"control number")
		dw_archive_title.SetItem(1, "mchar_conno", "")
		dw_archive_title.SetFocus()
		dw_archive_title.Setcolumn("mchar_conno")
		RETURN
	END IF
	
	  SELECT mchar.chno  
	    INTO :ls_chno  
	    FROM mchar
  	 WHERE mchar.conno = :ls_conno
		USING SQLservertrans ;
	IF f_check_dberror(SQLServerTrans,"MCHAR")=FALSE THEN
		RETURN -1
	END IF
	
	IF ls_chno = "" OR IsNull(ls_chno) THEN
		MessageBox("Invalid Control Number", "The Control Number is " + &
			"invalid.  Please reenter the correct control number")
		dw_archive_title.SetItem(1, "mchar_conno", "")
		dw_archive_title.SetFocus()
		dw_archive_title.Setcolumn("mchar_conno")
		RETURN
	ELSE
		is_chno = ls_chno

		is_syntax[2] = is_syntax[1] + "AND mchar.conno = " + ls_conno 
		dw_archive_title.Object.DataWindow.Table.Select = is_syntax[2]
		
		is_syntax[4] = is_syntax[3] + "AND mchar.conno = " + ls_conno 
		dw_medium_archival.Object.DataWindow.Table.Select = is_syntax[4]
		
		dw_archive_title.Event pfc_retrieve()
		dw_medium_archival.Event pfc_retrieve()
	
		dw_medium_archival.setfocus()
		dw_archive_title.Object.Datawindow.Readonly = TRUE
		This.Enabled = FALSE
		cb_update.Enabled = TRUE
		cb_clear.Enabled = TRUE
		w_pics_main.SetMicroHelp("Ready")		
	END IF
ELSE
	IF len(ls_chno) <> 6 THEN
		MessageBox("Invalid chart Number", "The Chart Number must " + &
			"be a six characters number.  Please reenter the " + &
			"chart number")
		dw_archive_title.SetItem(1, "ttlinit_chno", "")
		dw_archive_title.SetFocus()
		dw_archive_title.setcolumn("ttlinit_chno")
		RETURN
	END IF
	  SELECT mchar.chno  
	    INTO :ls_dummy 
	    FROM mchar  
  	 WHERE mchar.chno = :ls_chno
		USING SQLservertrans ;
	IF f_check_dberror(SQLServerTrans,"MCHAR")=FALSE THEN
		RETURN -1
	END IF

	IF ls_dummy <> ls_chno THEN
		MessageBox("Invalid Chart Number", "The Chart Number is " + &
			"invalid.  Please reenter the correct chart number")
		dw_archive_title.SetItem(1, "ttlinit_chno", "")
		dw_archive_title.SetFocus()
		dw_archive_title.Setcolumn("ttlinit_chno")
		RETURN
	ELSE
		is_chno = ls_chno
		dw_archive_title.Object.DataWindow.Table.Select = is_syntax[1]
		dw_medium_archival.Object.DataWindow.Table.Select = is_syntax[3]
		
		dw_archive_title.Event pfc_retrieve()
		dw_medium_archival.Event pfc_retrieve()
		dw_medium_archival.setfocus()
		dw_archive_title.Object.Datawindow.Readonly = TRUE
		This.Enabled = FALSE
		cb_update.Enabled = TRUE
		cb_clear.Enabled = TRUE
		w_pics_main.SetMicroHelp("Ready")
	END IF
END IF
cb_find.Default = FALSE
      

end event

type cb_update from u_cb within w_media_archival
event pfc_hinttext pbm_mousemove
string tag = "Save Changes to the Database"
integer x = 1499
integer y = 1244
integer taborder = 0
integer textsize = -10
string text = "&Update"
end type

event pfc_hinttext;call super::pfc_hinttext;w_pics_main.setmicrohelp(This.tag)
end event

event clicked;call super::clicked;long ll_count, i,il_count
string ls_reason, ls_conno, ls_archive, ls_dummy,ls_frflag
date ld_date
integer rtn

ls_frflag = dw_archive_title.object.mchar_frflag[1]

dw_medium_archival.Accepttext()
ll_count = dw_medium_archival.Rowcount()
IF ll_count > 0 then
	For i = 1 to ll_count
		ls_archive = dw_medium_archival.object.mchar_arflag[i]
		ls_reason = dw_medium_archival.object.arrsn_reason[i]
		IF ls_archive = "A" THEN
			il_count++
			IF IsNULL(ls_reason) or ls_reason = "" THEN
				Messagebox("Data Error","Please enter correct data in record  " + &
				string(i))
				dw_medium_archival.Setrow(i)
				dw_medium_archival.Setfocus()
				dw_medium_archival.Setcolumn("arrsn_reason")
				RETURN 2
			END IF
		ELSE
			IF len(ls_reason) > 0 THEN
				Messagebox("Data Error","Please click the archive flag check box before updating")
				dw_medium_archival.Setfocus()
				dw_medium_archival.Setcolumn("mchar_arflag")
				RETURN 2
			END IF
		END IF
	NEXT
	IF il_count = 0 THEN
		Messagebox("Update Error","Nothing to update")
		dw_medium_archival.Setfocus()
		RETURN 1
	END IF
	FOR i = 1 to ll_count
		ls_reason = dw_medium_archival.object.arrsn_reason[i]
		ld_date = date(dw_medium_archival.object.arrsn_ardt[i])
		ls_conno = dw_medium_archival.object.conno[i]
		ls_archive = dw_medium_archival.object.mchar_arflag[i]
		IF len(ls_reason) > 0 AND ls_archive = "A" THEN
			IF IsNull(ld_date) OR NOT ISDATE(string(ld_date)) OR string(ld_date) = "1900-01-01" THEN
				dw_medium_archival.SetItem(i, "arrsn_ardt", today())
				ld_date=today()
			END IF
		END IF
		SELECT arrsn.conno  
		  INTO :ls_dummy  
		  FROM arrsn  
		  WHERE arrsn.conno = :ls_conno 
		USING SQLservertrans;
			
		IF ls_archive = "A" AND Lgroupid="ADMIN" THEN
			IF ls_dummy = ls_conno THEN
				UPDATE arrsn
				SET arrsn.conno = :ls_conno,
		 		arrsn.reason = :ls_reason,
		 		arrsn.ardt = :ld_date
				WHERE arrsn.conno = :ls_conno
				USING SQLservertrans;
				IF f_check_dberror(SqlServerTrans,"arrsn") THEN
					COMMIT USING sqlservertrans;
				ELSE
					RETURN -1
				END IF
			ELSE
				INSERT INTO arrsn (conno, reason, ardt) values
				(:ls_conno, :ls_reason, :ld_date)
				USING SQLservertrans;
				IF f_check_dberror(SqlServerTrans,"arrsn") THEN
					COMMIT USING sqlservertrans;
				ELSE
					RETURN -1
				END IF
			END IF
			UPDATE mchar
			SET mchar.arflag = :ls_archive
			WHERE conno = :ls_conno
			USING sqlservertrans;
			IF f_check_dberror(SqlServerTrans,"mchar") THEN
				COMMIT USING sqlservertrans;
			ELSE
				RETURN -1
			END IF
		ELSEIF ls_archive = "A" AND Lgroupid="CDS" THEN
			IF ls_frflag<>'Y' OR IsNull(ls_frflag) THEN
				IF ls_dummy = ls_conno THEN
					UPDATE arrsn
					SET arrsn.conno = :ls_conno,
					arrsn.reason = :ls_reason,
					arrsn.ardt = :ld_date
					WHERE arrsn.conno = :ls_conno
					USING SQLservertrans;
					IF f_check_dberror(SqlServerTrans,"arrsn") THEN
						COMMIT USING sqlservertrans;
					ELSE
						RETURN -1
					END IF
				ELSE
					INSERT INTO arrsn (conno, reason, ardt) values
					(:ls_conno, :ls_reason, :ld_date)
					USING SQLservertrans;
					IF f_check_dberror(SqlServerTrans,"arrsn") THEN
						COMMIT USING sqlservertrans;
					ELSE
						RETURN -1
					END IF
				END IF
				UPDATE mchar
				SET mchar.arflag = :ls_archive
				WHERE conno = :ls_conno
				USING sqlservertrans;
				IF f_check_dberror(SqlServerTrans,"mchar") THEN
					COMMIT USING sqlservertrans;
				ELSE
					RETURN -1
				END IF
			ELSE
				MessageBox("ERROR","This control number completed final review, Therefore you are unable to archive it.",StopSign!)
				cb_clear.TriggerEvent(Clicked!)
				RETURN -1
			END IF
		ELSEIF ls_archive <> "A" AND Lgroupid="ADMIN" THEN
			IF ls_dummy = ls_conno THEN
				DELETE FROM arrsn 
				WHERE arrsn.conno = :ls_conno
				USING sqlservertrans;
				IF f_check_dberror(SqlServerTrans,"arrsn") THEN
					COMMIT USING sqlservertrans;
				ELSE
					RETURN -1
				END IF
			END IF
		END IF
	NEXT
END IF
Messagebox("Update Success","Update successful")
ib_disableclosequery = TRUE
RETURN 1 
			
end event

type cb_clear from u_cb within w_media_archival
event pfc_hinttext pbm_mousemove
string tag = "Clear the Record from the Screen"
integer x = 1970
integer y = 1240
integer taborder = 0
integer textsize = -10
string text = "&Clear"
end type

event pfc_hinttext;call super::pfc_hinttext;w_pics_main.setmicrohelp(This.tag)
end event

event clicked;call super::clicked;dw_archive_title.reset()
dw_medium_archival.reset()
dw_archive_title.event pfc_addrow()
dw_archive_title.Object.Datawindow.Readonly = FALSE
dw_medium_archival.Modify("mchar_conno.Tabsequence = 10")
dw_medium_archival.Modify("mchar_chno.Tabsequence = 20")
cb_find.enabled = TRUE
cb_update.Enabled = FALSE
cb_clear.Enabled = FALSE
ib_disableclosequery = FALSE
dw_archive_title.setfocus()
cb_find.Default = TRUE
dw_medium_archival.Visible = FALSE


end event

type cb_close from u_cb within w_media_archival
event pfc_hinttext pbm_mousemove
string tag = "Closing this Screen"
integer x = 2437
integer y = 1240
integer taborder = 0
integer textsize = -10
string text = "E&xit"
end type

event pfc_hinttext;call super::pfc_hinttext;w_pics_main.setmicrohelp(This.tag)
end event

event clicked;call super::clicked;parent.Event pfc_close()
m_pics_main.m_menu.popMenu(300,0)
end event


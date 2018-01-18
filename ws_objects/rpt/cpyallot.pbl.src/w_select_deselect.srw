$PBExportHeader$w_select_deselect.srw
forward
global type w_select_deselect from w_sheet
end type
type st_ind_counter from statictext within w_select_deselect
end type
type cb_exit from u_cb within w_select_deselect
end type
type cb_update from u_cb within w_select_deselect
end type
type sle_rowcount from u_sle within w_select_deselect
end type
type st_1 from u_st within w_select_deselect
end type
type st_totalrowstext from u_st within w_select_deselect
end type
type sle_totalrows from u_sle within w_select_deselect
end type
type cb_print from u_cb within w_select_deselect
end type
type dw_report_select_deselect from u_dw within w_select_deselect
end type
type dw_select_deselect from u_dw within w_select_deselect
end type
end forward

global type w_select_deselect from w_sheet
integer width = 2729
string title = "Copy Allotment Selection / Deselection"
st_ind_counter st_ind_counter
cb_exit cb_exit
cb_update cb_update
sle_rowcount sle_rowcount
st_1 st_1
st_totalrowstext st_totalrowstext
sle_totalrows sle_totalrows
cb_print cb_print
dw_report_select_deselect dw_report_select_deselect
dw_select_deselect dw_select_deselect
end type
global w_select_deselect w_select_deselect

type variables
long il_db, il_rc, il_br, il_pb // 03/25/2008 counters
end variables

on w_select_deselect.create
int iCurrent
call super::create
this.st_ind_counter=create st_ind_counter
this.cb_exit=create cb_exit
this.cb_update=create cb_update
this.sle_rowcount=create sle_rowcount
this.st_1=create st_1
this.st_totalrowstext=create st_totalrowstext
this.sle_totalrows=create sle_totalrows
this.cb_print=create cb_print
this.dw_report_select_deselect=create dw_report_select_deselect
this.dw_select_deselect=create dw_select_deselect
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_ind_counter
this.Control[iCurrent+2]=this.cb_exit
this.Control[iCurrent+3]=this.cb_update
this.Control[iCurrent+4]=this.sle_rowcount
this.Control[iCurrent+5]=this.st_1
this.Control[iCurrent+6]=this.st_totalrowstext
this.Control[iCurrent+7]=this.sle_totalrows
this.Control[iCurrent+8]=this.cb_print
this.Control[iCurrent+9]=this.dw_report_select_deselect
this.Control[iCurrent+10]=this.dw_select_deselect
end on

on w_select_deselect.destroy
call super::destroy
destroy(this.st_ind_counter)
destroy(this.cb_exit)
destroy(this.cb_update)
destroy(this.sle_rowcount)
destroy(this.st_1)
destroy(this.st_totalrowstext)
destroy(this.sle_totalrows)
destroy(this.cb_print)
destroy(this.dw_report_select_deselect)
destroy(this.dw_select_deselect)
end on

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
					dw_select_deselect.Setfocus()
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
			dw_select_deselect.Setfocus()
			// CANCEL -  Prevent the window from closing
	End Choose
End If

// Prevent the window from closing
ib_closestatus = False
Return 1
end event

event mousemove;call super::mousemove;w_pics_main.setmicrohelp("Ready")
end event

event open;call super::open;
THIS.Windowstate = maximized!


end event

event pfc_postopen;call super::pfc_postopen;long i,li_row_count
string ls_ord_upd

m_pics_main.m_Edit.m_Addrow.Enabled = FALSE
m_pics_main.m_Edit.m_Deleterow.Enabled = FALSE
m_pics_main.m_Edit.m_Cut.Enabled = FALSE

OpenWithParm(w_pics_retrieve_msg_box,"Retrieving CA Selection/Deselection records, Please wait...")

// This sql will set the arflag to NULL if there is blank values in this field.
UPDATE MCHAR
SET arflag=NULL
WHERE arflag=''
AND arflag is not NULL
USING SQLServerTrans;
IF f_check_dberror(sqlservertrans,"MCHAR") THEN
	COMMIT USING SQLServerTrans;
ELSE
	ROLLBACK USING SQLServerTrans;
END IF

dw_select_deselect.settransobject(sqlservertrans)
cb_print.Enabled =FALSE
//dw_select_deselect.Event pfc_retrieve()
li_row_count=dw_select_deselect.Retrieve()
//li_row_count=dw_select_deselect.RowCount()
if li_row_count>0 then
	for i=1 to li_row_count
		ls_ord_upd=dw_select_deselect.object.mchar_ordqty_flg[i]
		if IsNull(ls_ord_upd) or ls_ord_upd="" then
			ls_ord_upd="Y"
			dw_select_deselect.object.mchar_ordqty_flg[i] = ls_ord_upd
		end if
	next
end if
	

//IF dw_select_deselect.Sharedata(dw_report_select_deselect) = -1 THEN
//	MessageBox("ERROR", "Error sharing data between datawindows.",StopSign!, OK!, 2)
//END IF
//
end event

event pfc_preopen;call super::pfc_preopen;this.of_SetPreference(TRUE)
this.inv_preference.of_SetToolBars(TRUE)
this.inv_preference.of_SetWindow(TRUE)
this.of_SetResize(TRUE)
this.inv_resize.of_SetOrigSize(this.workspacewidth(),this.workspaceheight())
inv_resize.of_Register(dw_select_deselect, "scale")
inv_resize.of_Register(cb_update, "scale")
inv_resize.of_Register(cb_exit, "scale")
inv_resize.of_Register(st_1, "scale")
inv_resize.of_Register(sle_rowcount, "scale")
inv_resize.of_Register(st_totalrowstext, "scale")
inv_resize.of_Register(sle_totalrows, "scale")
inv_resize.of_Register(cb_print, "scale")

//03/25/2008
inv_resize.of_Register(st_ind_counter, "scale")
end event

event resize;call super::resize;//long ll_height
//
//This.X = w_pics_main.X
//This.Y = w_pics_main.Y
//ll_height = w_pics_main.mdi_1.Height
//This.Resize(w_pics_main.WorkSpaceWidth(), ll_height)
end event

type st_ind_counter from statictext within w_select_deselect
boolean visible = false
integer x = 23
integer y = 1208
integer width = 654
integer height = 64
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long backcolor = 67108864
string text = "none"
boolean focusrectangle = false
end type

type cb_exit from u_cb within w_select_deselect
event pfc_hinttext pbm_mousemove
string tag = "Exits the screen"
integer x = 2336
integer y = 1248
integer taborder = 0
integer textsize = -10
string text = "E&xit"
end type

event pfc_hinttext;call super::pfc_hinttext;w_pics_main.setmicrohelp(This.tag)
end event

event clicked;call super::clicked;parent.Event pfc_close()
m_pics_main.m_menu.PopMenu(300, 0)

end event

type cb_update from u_cb within w_select_deselect
event pfc_hinttext pbm_mousemove
string tag = "Updating all the records from ~'N~' to ~'Q~' and ~'Q~' to ~'Q~' and ~'X~' to ~'X~'"
integer x = 1573
integer y = 1248
integer taborder = 0
integer textsize = -10
boolean enabled = false
string text = "&Update"
end type

event pfc_hinttext;call super::pfc_hinttext;w_pics_main.setmicrohelp(This.tag)
end event

event clicked;call super::clicked;integer rtn,i,rtns
long ll_rows
string ls_message,ls_msgparm[1],ls_cascd
Datetime ld_today
ld_today = DateTime(Today(), Now())


dw_select_deselect.Accepttext()

ll_rows = dw_select_deselect.Rowcount()
FOR i = ll_rows TO 1 STEP -1
	ls_cascd = dw_select_deselect.Getitemstring(i,"mchar_cascd")
	IF TRIM(UPPER((ls_cascd))) = "N" THEN
		dw_select_deselect.Setitem(i,"mchar_cascd",'Q')
	END IF
	// Mark the MCHAR table
	dw_select_deselect.object.mchar_update_date[i] = ld_today
NEXT

 rtn = dw_select_deselect.Event pfc_update(TRUE,TRUE)
 IF rtn = 1 THEN
	COMMIT USING sqlservertrans;
	Messagebox("Update","Update Successful")
	cb_print.enabled =TRUE
	RETURN 
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

type sle_rowcount from u_sle within w_select_deselect
event pfc_hinttext pbm_mousemove
string tag = "Number of rows retrieved"
integer x = 686
integer y = 1248
integer width = 215
integer height = 92
integer taborder = 0
integer textsize = -10
integer weight = 700
long textcolor = 255
string text = "0"
boolean displayonly = true
end type

event pfc_hinttext;call super::pfc_hinttext;w_pics_main.setmicrohelp(This.tag)
end event

type st_1 from u_st within w_select_deselect
integer x = 5
integer y = 1276
integer width = 663
string text = "Number of Records Excluded"
end type

type st_totalrowstext from u_st within w_select_deselect
integer x = 914
integer y = 1276
integer width = 361
string text = "Total Records "
alignment alignment = center!
end type

type sle_totalrows from u_sle within w_select_deselect
integer x = 1303
integer y = 1248
integer width = 201
integer height = 92
integer taborder = 0
integer textsize = -10
integer weight = 700
long textcolor = 255
boolean displayonly = true
end type

type cb_print from u_cb within w_select_deselect
integer x = 1961
integer y = 1248
integer taborder = 10
string text = "&Print"
end type

event clicked;call super::clicked;dw_report_select_deselect.SetTransObject(sqlservertrans )
dw_report_select_deselect.Retrieve()
//dw_report_select_deselect.sort()
//dw_report_select_deselect.SetSort('mchar_cascd A, prod_bkseq A')
dw_report_select_deselect.Sort()
dw_report_select_deselect.TriggerEvent("pfc_Print")

end event

type dw_report_select_deselect from u_dw within w_select_deselect
boolean visible = false
integer x = 23
integer y = 984
integer width = 151
integer height = 40
integer taborder = 2
string dataobject = "d_report_select_deselect"
boolean vscrollbar = false
boolean border = false
boolean livescroll = false
borderstyle borderstyle = stylebox!
boolean ib_isupdateable = false
boolean ib_rmbmenu = false
boolean ib_rmbfocuschange = false
string is_updatesallowed = ""
end type

type dw_select_deselect from u_dw within w_select_deselect
event pfc_hinttext pbm_mousemove
event ue_enterkey pbm_dwnprocessenter
event pfc_keydown pbm_dwnkey
integer x = 9
integer y = 8
integer width = 2670
integer height = 1188
integer taborder = 20
string dataobject = "d_select_deselect"
boolean hscrollbar = true
boolean hsplitscroll = true
boolean livescroll = false
boolean ib_isupdateable = false
boolean ib_rmbmenu = false
boolean ib_rmbfocuschange = false
string is_updatesallowed = ""
end type

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

event ue_enterkey;call super::ue_enterkey;Send(Handle(dw_select_deselect),256,9,Long(0,0))
return(1)
end event

event constructor;call super::constructor;ib_rmbmenu = FALSE


end event

event getfocus;call super::getfocus;m_pics_main.m_Edit.m_Addrow.Enabled = FALSE
m_pics_main.m_Edit.m_Deleterow.Enabled = FALSE
m_pics_main.m_Edit.m_Cut.Enabled = FALSE
end event

event itemchanged;call super::itemchanged;long ll_ret,ll_row
integer li_count,li_set,j,i
string ls_old_data

li_count = integer( sle_rowcount.Text) 
IF dwo.name ='mchar_cascd' THEN
	ls_old_data =dw_select_deselect.GetItemString( row,'mchar_cascd' )
END IF
IF dwo.name = "mchar_cascd" and TRIM(data) = "X" THEN
	  sle_rowcount.Text = string(li_count + 1 )
ELSE
	IF (TRIM(data) = "N" and ls_old_data ='X' )OR (TRIM(data) = "Q" AND ls_old_data ='X') THEN
	sle_rowcount.Text = string(li_count - 1)
	END IF
END IF
IF dwo.name ='mchar_ordqty_flg' THEN
	if data <>'N' and data<>'Y' then
		return 1
	end if
END IF
//j = 0
//FOR i = 1 to dw_select_deselect.RowCount() 
//	IF dw_select_deselect.object.mchar_cascd[i] = 'X' THEN
//		j = j + 1
//	END IF
//NEXT
//sle_rowcount.Text = string (j)
//
	
     


end event

event pfc_retrieve;call super::pfc_retrieve;RETURN THIS.Retrieve()

end event

event retrieveend;call super::retrieveend;//////////////////////////////////////////////////////////////////////////////////////////////////////////
//
//	Event:  retrieveend for dw_select_deselect
//
//	Description:
//	Set Individual book media counters
//
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//	
//	Revision History
//
//	Developed by 	Date 				Version						Tracking#
//									
// Murali K.			03/25/2008      PICS 2.0 						 Reqs: CA.8
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//
integer i,j
long Lbkseq
string Lbkmed,Lcntr
datetime Lschstdt,Lactstdt,Lactenddt

sle_totalrows.Text = string(rowcount)

IF NOT Rowcount > 0 THEN
	close(w_pics_retrieve_msg_box)
	Messagebox("No Rows", "No data found to update the record(s)")
	ib_disableclosequery = TRUE
	Parent.Event pfc_close()	
ELSE
	j = 0
FOR i = 1 to dw_select_deselect.RowCount() 
	// 03/25/2008 set individual book media wise counters
// 3/29/10 retrieverow code moved here - unsupported appeon feature
	Lbkmed = dw_select_deselect.object.prod_bkmed[i]
	CHOOSE CASE  lbkmed
		CASE 'DB'
			il_db++
		CASE 'RC'
			il_rc++
		CASE 'BR'
			il_br++
		CASE 'P/B'
			il_pb++
	END CHOOSE
	//
	/// 3/29/10
	
	IF dw_select_deselect.object.mchar_cascd[i] = 'X' THEN
		j = j + 1
	END IF
	Lbkseq = dw_select_deselect.object.prod_bkseq[i]
	Lbkmed = dw_select_deselect.object.prod_bkmed[i]
	Lcntr = dw_select_deselect.object.ancntr_cntr[i]
	Select schstdt,actstdt,actenddt into :Lschstdt,:Lactstdt,:Lactenddt from prod 
	where bkseq=:Lbkseq and bkmed=:Lbkmed and cntr=:Lcntr and prodstage in ('MA','PU','AB')
	using sqlservertrans;
	IF f_check_dberror(sqlservertrans,"PROD")=TRUE THEN
		dw_select_deselect.object.prod_schstdt[i] = Lschstdt
		dw_select_deselect.object.prod_actstdt[i] = Lactstdt
		dw_select_deselect.object.prod_actenddt[i] = Lactenddt
	ELSE
		close(w_pics_retrieve_msg_box)
		RETURN
	END IF
	// This part of code was added in regards of PR# 792
	// If the book is print-braille, producers can't change
	// the default quantity of the book.
	IF dw_select_deselect.object.mchar_med[i] = 'P/B' THEN
		dw_select_deselect.object.mchar_ordqty_flg[i] = 'N'
	END IF
NEXT
sle_rowcount.Text = string (j)
	
	close(w_pics_retrieve_msg_box)
	dw_select_deselect.Setfocus()
	ib_disableclosequery = FALSE
	cb_update.Enabled = TRUE
	st_1.Enabled = TRUE
   sle_rowcount.Enabled = TRUE
	
	// 03/25/2008 display individual book media counts
	st_ind_counter.Visible=TRUE
	st_ind_counter.text = 'DB = ' + string(il_db) + ', RC = ' + string(il_rc) + ',  BR = ' + string(il_br) 
	//+ ', P/B = ' + string(il_pb) 
END IF
	
	
	


end event

event rowfocuschanged;call super::rowfocuschanged;currentrow = currentrow 



end event

event updateend;call super::updateend;Close(w_pics_update_msg_box)
end event

event sqlpreview;call super::sqlpreview;//MessageBox("sql",sqlsyntax)
end event

event updatestart;call super::updatestart;Open(w_pics_update_msg_box)

end event


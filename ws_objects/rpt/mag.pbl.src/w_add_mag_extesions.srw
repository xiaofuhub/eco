$PBExportHeader$w_add_mag_extesions.srw
forward
global type w_add_mag_extesions from w_response
end type
type dw_magcode from u_dw within w_add_mag_extesions
end type
type cb_exit from u_cb within w_add_mag_extesions
end type
type cb_update from u_cb within w_add_mag_extesions
end type
end forward

global type w_add_mag_extesions from w_response
integer x = 681
integer y = 268
integer width = 2386
integer height = 1064
string title = "Magazine Extension"
dw_magcode dw_magcode
cb_exit cb_exit
cb_update cb_update
end type
global w_add_mag_extesions w_add_mag_extesions

type variables
str_exist_inv lstr_exist_inv

end variables

on w_add_mag_extesions.create
int iCurrent
call super::create
this.dw_magcode=create dw_magcode
this.cb_exit=create cb_exit
this.cb_update=create cb_update
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_magcode
this.Control[iCurrent+2]=this.cb_exit
this.Control[iCurrent+3]=this.cb_update
end on

on w_add_mag_extesions.destroy
call super::destroy
destroy(this.dw_magcode)
destroy(this.cb_exit)
destroy(this.cb_update)
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
			
			
			
//			// YES - Update
//			// If the update fails, prevent the window from closing
//			If This.Event pfc_save() >= 1 Then
//				// Successful update, allow the window to be closed
//				Return 0
//			End If
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

event open;call super::open;string ls_magcode
date ld_issuedate

lstr_exist_inv = Message.PowerObjectParm

//Get the magazine Code and Issuedate from the dialog box
ls_magcode = lstr_exist_inv.ls_magcd
ld_issuedate = lstr_exist_inv.ld_issdt

dw_magcode.SetTransObject(SqlServerTrans)
dw_magcode.Retrieve(ls_magcode,ld_issuedate)
dw_magcode.Event pfc_addrow()
end event

type dw_magcode from u_dw within w_add_mag_extesions
event ue_enter_to_tab pbm_dwnprocessenter
integer x = 32
integer y = 32
integer width = 2318
integer height = 812
integer taborder = 10
string dataobject = "d_add_update_mag_ext"
end type

event ue_enter_to_tab;call super::ue_enter_to_tab;
SEND(Handle(this), 256, 9, Long(0,0))
RETURN(1)
end event

event constructor;call super::constructor;this.SetTransObject(SqlServerTrans)

end event

event pfc_addrow;call super::pfc_addrow;//////////////////////////////////////////////////////////////////////////////
//	Event:			pfc_addrow
//	Arguments:		None
//	Returns:			long - number of the new row that was inserted
//	 					0 = No row was added.
//						-1 = error
//	Description:	Adds a new row to the end of the DW
//////////////////////////////////////////////////////////////////////////////
//	Rev. History	Version
//						5.0   Initial version
// 					6.0	Enhanced with Pre Insert funcitonality.
// 					7.0	Enhanced with Post Insert funcitonality.
// 					7.0	Linkage service should not fire events when querymode is enabled
//////////////////////////////////////////////////////////////////////////////
//	Copyright © 1996-1999 Sybase, Inc. and its subsidiaries.  All rights reserved.  Any distribution of the 
// PowerBuilder Foundation Classes (PFC) source code by other than Sybase, Inc. and its subsidiaries is prohibited.
//////////////////////////////////////////////////////////////////////////////
long	ll_rc,li_loop
boolean lb_disablelinkage
string ls_magcode

// Allow for pre functionality.
if this.Event pfc_preinsertrow() <= 0 then return NO_ACTION

// Is Querymode enabled?
if IsValid(inv_QueryMode) then lb_disablelinkage = inv_QueryMode.of_GetEnabled()

if not lb_disablelinkage then
	// Notify that a new row is about to be added.
	if IsValid ( inv_Linkage ) then inv_Linkage.Event pfc_InsertRow (0) 
end if

// Insert row.
if IsValid (inv_RowManager) then
	ll_rc = inv_RowManager.event pfc_addrow ()
else
	ll_rc = this.InsertRow (0) 
end if

if not lb_disablelinkage then
	// Notify that a new row has been added.
	if IsValid ( inv_Linkage ) then inv_Linkage.Event pfc_InsertRow (ll_rc) 
end if

this.object.magcd[ll_rc] = lstr_exist_inv.ls_magcd
this.object.issdt[ll_rc] = lstr_exist_inv.ld_issdt
this.object.extdt[ll_rc] = today()

// Allow for post functionality.
this.Post Event pfc_postinsertrow(ll_rc)

//Delete blank lines from the datawindow
FOR li_loop = ll_rc TO 1 step -1
	ls_magcode = this.object.magcd[li_loop]
	IF IsNull(ls_magcode) OR ls_magcode="" THEN
		dw_magcode.DeleteRow(li_loop)
	END IF
NEXT

this.ScrollToRow(ll_rc)
this.setfocus()
return ll_rc
end event

event pfc_insertrow;call super::pfc_insertrow;//
return 1
end event

type cb_exit from u_cb within w_add_mag_extesions
integer x = 2062
integer y = 864
integer width = 297
integer height = 88
integer taborder = 0
integer textsize = -10
string text = "E&xit"
end type

event clicked;call super::clicked;long li_cnt, i, li_ext
string ls_rsn

li_cnt=dw_magcode.RowCount()
for i=1 to li_cnt
	li_ext=dw_magcode.object.ext[i]
	ls_rsn=dw_magcode.object.extrsn[i]
	if isnull(li_ext) and (isnull(ls_rsn) or ls_rsn='') then
		li_ext=dw_magcode.deleteRow(i)
		li_cnt= li_cnt - 1
	end if
next
Parent.Event pfc_close()
end event

type cb_update from u_cb within w_add_mag_extesions
event clicked pbm_bnclicked
integer x = 1714
integer y = 864
integer width = 297
integer height = 88
integer taborder = 0
integer textsize = -10
string text = "&Update"
end type

event clicked;Long li_rtn_code, li_extdays
Integer li_loop, li_max_rows,sum_ext, old_sum_ext,li_dif
String ls_magcode
date ld_issuedate,old_estshipdt, new_estshipdt
datetime ldt_issuedate

dw_magcode.AcceptText()

li_max_rows = dw_magcode.RowCount()

//Delete blank lines from the datawindow
FOR li_loop = li_max_rows TO 1 step -1
	ls_magcode = dw_magcode.object.magcd[li_loop]
	li_extdays=dw_magcode.object.ext[li_loop]
	IF IsNull(ls_magcode) OR ls_magcode="" or (isnull(li_extdays) or li_extdays=0) THEN
		dw_magcode.DeleteRow(li_loop)
	ELSE
		li_extdays=dw_magcode.object.ext[li_loop]
		if not isnull(li_extdays) then
			sum_ext = sum_ext + li_extdays
		end if
	END IF
NEXT
ls_magcode = lstr_exist_inv.ls_magcd
ld_issuedate = lstr_exist_inv.ld_issdt
select sum(ext) into  :old_sum_ext
from magext
where magcd=:ls_magcode and issdt=:ld_issuedate
using sqlservertrans;
IF not f_check_dberror(sqlservertrans,"select sum(ext) from magext") THEN
	return
end if
li_rtn_code = dw_magcode.Event pfc_update(TRUE,true)
IF li_rtn_code = 1 THEN
	
	li_max_rows = dw_magcode.RowCount()
	IF li_max_rows > 0 THEN
		ldt_issuedate = dw_magcode.object.issdt[1]
		ld_issuedate=date(ldt_issuedate)
	ELSE
		ls_magcode = lstr_exist_inv.ls_magcd
		ld_issuedate = lstr_exist_inv.ld_issdt
		sum_ext = 0
	END IF
	li_dif= sum_ext - old_sum_ext
	select estshipdt into :old_estshipdt
	from magiss
	where magcd = :ls_magcode
	and issdt = :ld_issuedate
	using sqlservertrans;
	IF not f_check_dberror(sqlservertrans,"select estshipdt from magiss") THEN
		return
	end if
	new_estshipdt= RelativeDate(old_estshipdt, li_dif)
	update magiss
	set extc = :sum_ext, estshipdt=:new_estshipdt
	where magcd = :ls_magcode
	and issdt = :ld_issuedate
	using sqlservertrans;
	
	IF f_check_dberror(sqlservertrans,"Magiss") THEN
		MessageBox('Update','Updated Row(s) Successfully')
		commit using sqlservertrans;
		w_magazine_issue_maintenance.em_issue_date.TriggerEvent('ue_fresh')
	ELSE
		ROLLBACK USING SqlServerTrans;
		MessageBox('Error','Update Error .. Contact Your DBA')		
	END IF
	
	dw_magcode.ResetUpdate()
	return 1
ELSE
	ROLLBACK USING SqlServerTrans;
	MessageBox('Error','Update Error .. Contact Your DBA')
END IF//IF li_rtn_code = 1 THEN

end event


$PBExportHeader$w_pa_bk_estimation.srw
forward
global type w_pa_bk_estimation from w_sheet
end type
type cb_narr from u_cb within w_pa_bk_estimation
end type
type dw_pa_bk_estimation from u_pics_dw within w_pa_bk_estimation
end type
type cb_exit from u_cb within w_pa_bk_estimation
end type
type cb_clear from u_cb within w_pa_bk_estimation
end type
type cb_update from u_cb within w_pa_bk_estimation
end type
type cb_find from u_cb within w_pa_bk_estimation
end type
type dw_pa_prv_narrator from u_pics_dw within w_pa_bk_estimation
end type
type cb_sitxt from u_cb within w_pa_bk_estimation
end type
type dw_pa_coauth from u_pics_dw within w_pa_bk_estimation
end type
end forward

global type w_pa_bk_estimation from w_sheet
integer x = 27
integer y = 80
integer width = 2885
integer height = 1688
string title = "Book Estimation"
cb_narr cb_narr
dw_pa_bk_estimation dw_pa_bk_estimation
cb_exit cb_exit
cb_clear cb_clear
cb_update cb_update
cb_find cb_find
dw_pa_prv_narrator dw_pa_prv_narrator
cb_sitxt cb_sitxt
dw_pa_coauth dw_pa_coauth
end type
global w_pa_bk_estimation w_pa_bk_estimation

type variables
DataWindowChild ldwc_narr

end variables

forward prototypes
public subroutine wf_set_taborder_org ()
public subroutine wf_set_taborder_zero ()
end prototypes

public subroutine wf_set_taborder_org ();dw_pa_bk_estimation.Object.mchar_conno.TabSequence = 0
dw_pa_bk_estimation.Object.mchar_bkseq.TabSequence = 0
dw_pa_bk_estimation.Object.mchar_bkmed.TabSequence = 0
dw_pa_bk_estimation.Object.ri_prevbkmed.TabSequence = 0
dw_pa_bk_estimation.Object.ri_prevbkseq.TabSequence = 0
dw_pa_bk_estimation.Object.mchar_ricd.TabSequence = 0
//dw_pa_bk_estimation.Object.mchar_andigcd.TabSequence = 10
dw_pa_bk_estimation.Object.mchar_df.TabSequence = 20
dw_pa_bk_estimation.Object.mchar_applen.TabSequence = 30
dw_pa_bk_estimation.Object.mchar_priority.TabSequence = 40
dw_pa_bk_estimation.Object.ttlinit_b_ttl1.TabSequence = 50
dw_pa_bk_estimation.Object.ttlinit_b_ttl2.TabSequence = 60
dw_pa_bk_estimation.Object.mchar_siflag.TabSequence = 70
dw_pa_bk_estimation.Object.ttlinit_serttl.TabSequence = 80
dw_pa_bk_estimation.Object.ttlinit_b_auth.TabSequence = 90
dw_pa_bk_estimation.Object.ttlinit_note.TabSequence = 100
dw_pa_bk_estimation.Object.acquist_pbpage.TabSequence = 110
dw_pa_bk_estimation.Object.mchar_vindx.TabSequence = 120
dw_pa_bk_estimation.Object.cr_crpermcd.TabSequence = 0
dw_pa_bk_estimation.Object.ttlinit_cryr.TabSequence = 130
dw_pa_bk_estimation.Object.cr_crrestxt.TabSequence = 0
dw_pa_bk_estimation.Object.ttlinit_crname.TabSequence = 140
cb_clear.enabled = TRUE
cb_update.enabled = TRUE
cb_find.enabled = FALSE

end subroutine

public subroutine wf_set_taborder_zero ();dw_pa_bk_estimation.Object.mchar_conno.TabSequence = 10
dw_pa_bk_estimation.Object.mchar_bkseq.TabSequence = 20
dw_pa_bk_estimation.Object.mchar_bkmed.TabSequence = 30
dw_pa_bk_estimation.Object.ri_prevbkmed.TabSequence = 0
dw_pa_bk_estimation.Object.ri_prevbkseq.TabSequence = 0
dw_pa_bk_estimation.Object.mchar_ricd.TabSequence = 0
//dw_pa_bk_estimation.Object.mchar_andigcd.TabSequence = 0
dw_pa_bk_estimation.Object.mchar_df.TabSequence = 0
dw_pa_bk_estimation.Object.mchar_applen.TabSequence = 0
dw_pa_bk_estimation.Object.mchar_priority.TabSequence = 0
dw_pa_bk_estimation.Object.mchar_siflag.TabSequence = 0
dw_pa_bk_estimation.Object.mchar_vindx.TabSequence = 0
dw_pa_bk_estimation.Object.ttlinit_b_ttl1.TabSequence = 0
dw_pa_bk_estimation.Object.ttlinit_b_ttl2.TabSequence = 0
dw_pa_bk_estimation.Object.ttlinit_note.TabSequence = 0
dw_pa_bk_estimation.Object.ttlinit_serttl.TabSequence = 0
dw_pa_bk_estimation.Object.acquist_pbpage.TabSequence = 0
dw_pa_bk_estimation.Object.ttlinit_b_auth.TabSequence = 0
dw_pa_bk_estimation.Object.ttlinit_cryr.TabSequence = 0
dw_pa_bk_estimation.Object.ttlinit_crname.TabSequence = 0
dw_pa_bk_estimation.Object.cr_crpermcd.TabSequence = 0
dw_pa_bk_estimation.Object.cr_crrestxt.TabSequence = 0
cb_clear.enabled = FALSE
cb_update.enabled = FALSE
cb_find.enabled = TRUE
cb_find.default = TRUE
cb_sitxt.visible=FALSE


end subroutine

on w_pa_bk_estimation.create
int iCurrent
call super::create
this.cb_narr=create cb_narr
this.dw_pa_bk_estimation=create dw_pa_bk_estimation
this.cb_exit=create cb_exit
this.cb_clear=create cb_clear
this.cb_update=create cb_update
this.cb_find=create cb_find
this.dw_pa_prv_narrator=create dw_pa_prv_narrator
this.cb_sitxt=create cb_sitxt
this.dw_pa_coauth=create dw_pa_coauth
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.cb_narr
this.Control[iCurrent+2]=this.dw_pa_bk_estimation
this.Control[iCurrent+3]=this.cb_exit
this.Control[iCurrent+4]=this.cb_clear
this.Control[iCurrent+5]=this.cb_update
this.Control[iCurrent+6]=this.cb_find
this.Control[iCurrent+7]=this.dw_pa_prv_narrator
this.Control[iCurrent+8]=this.cb_sitxt
this.Control[iCurrent+9]=this.dw_pa_coauth
end on

on w_pa_bk_estimation.destroy
call super::destroy
destroy(this.cb_narr)
destroy(this.dw_pa_bk_estimation)
destroy(this.cb_exit)
destroy(this.cb_clear)
destroy(this.cb_update)
destroy(this.cb_find)
destroy(this.dw_pa_prv_narrator)
destroy(this.cb_sitxt)
destroy(this.dw_pa_coauth)
end on

event open;call super::open;// Open the sheet in Maximized mode
this.windowstate = maximized!

m_pics_main.m_file.m_print.Enabled 			=	TRUE
m_pics_main.m_file.m_pagesetup.Enabled		=	TRUE
m_pics_main.m_file.m_printimmediate.Enabled	=	TRUE

// Initialy set the query mode to yes. 
dw_pa_bk_estimation.Object.DataWindow.QueryMode='Yes'

// set the tab order to zero, and disable some of the command buttons
wf_set_taborder_zero()

// Set the focus to the datawindows.
dw_pa_bk_estimation.SetFocus()


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
			if rtn = 1 THEN
				RETURN 0
			end if
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

event pfc_preopen;call super::pfc_preopen;this.of_SetPreference(TRUE)
this.inv_preference.of_SetToolBars(TRUE)
this.inv_preference.of_SetWindow(TRUE)
this.of_SetResize(TRUE)
this.inv_resize.of_SetOrigSize(this.workspacewidth(),this.workspaceheight())
inv_resize.of_Register(dw_pa_bk_estimation, "Scale")
inv_resize.of_Register(dw_pa_prv_narrator, "Scale")
inv_resize.of_Register(dw_pa_coauth, "Scale")
inv_resize.of_Register(cb_clear, "Scale")
inv_resize.of_Register(cb_narr, "Scale")
inv_resize.of_Register(cb_exit, "Scale")
inv_resize.of_Register(cb_find, "Scale")
inv_resize.of_Register(cb_update, "Scale")
inv_resize.of_Register(cb_sitxt, "Scale")

end event

event resize;call super::resize;//long ll_height
//
//This.X = w_pics_main.X
//This.Y = w_pics_main.Y
//ll_height = w_pics_main.mdi_1.Height
//This.Resize(w_pics_main.WorkSpaceWidth(), ll_height)
//
end event

event pfc_endtran;call super::pfc_endtran;int li_rtn=1
IF ai_update_results = -1 THEN
	IF sqlservertrans.of_rollback() < 0 THEN li_rtn = -1
	MessageBox("Update Status", "Update Falied - Rollback")
ELSEIF ai_update_results = 1 THEN
	IF sqlservertrans.of_commit() < 0 THEN li_rtn= -1
END IF

RETURN li_rtn
end event

type cb_narr from u_cb within w_pa_bk_estimation
integer x = 590
integer y = 1436
integer width = 553
integer height = 96
integer taborder = 0
string text = "Add/Update Narrators.."
end type

event clicked;call super::clicked;open(w_nls_add_narrator)
dw_pa_prv_narrator.GetChild ("prvnarrln", ldwc_narr)
ldwc_narr.SetTransObject(sqlservertrans)
ldwc_narr.retrieve()

end event

type dw_pa_bk_estimation from u_pics_dw within w_pa_bk_estimation
event ue_enterkey pbm_dwnprocessenter
integer x = 32
integer y = 28
integer width = 2779
integer height = 1136
integer taborder = 10
string dataobject = "d_pa_bk_estimation"
boolean vscrollbar = false
boolean livescroll = false
boolean ib_isupdateable = false
boolean ib_rmbmenu = false
boolean ib_rmbfocuschange = false
string is_updatesallowed = ""
end type

event ue_enterkey;int colnum
colnum = dw_pa_bk_estimation.GetColumn()
IF (colnum <> 1) AND (colnum <> 3) THEN
	Send(Handle(this),256,9,Long(0,0))
	return(1)
ELSE
	cb_find.TriggerEvent(Clicked!)
END IF

end event

event ue_postconstructor;call super::ue_postconstructor;dw_pa_bk_estimation.of_SetTransObject( SQLServerTrans )

end event

event itemchanged;call super::itemchanged;int rtn
string Lmsg
IF dwo.name = "mchar_siflag" THEN
	IF data = "N" THEN
		string Lconno,Lcon
		Lconno = dw_pa_bk_estimation.object.mchar_conno[1]
		select conno into :Lcon from specinst where conno = :Lconno
			using SQLServerTrans;
		IF SQLServerTrans.SQLCode = 0 THEN
			rtn = MessageBox("Deleting","Do you want to delete the special instruction for control number: "+Lconno+" ?",Question!,YesNo!,1)
			IF rtn = 1 THEN
				Delete from specinst where conno = :Lconno using SQLServerTrans;
				IF SQLServerTrans.SQLCode <> 0 THEN				
					ROLLBACK USING SQLServerTrans;
					MessageBox("Error","Error deleting special instruction record.")
				ELSE
					// Tracker 2132 12/11/2008 not commiting the delete.
					COMMIT USING SQLSERVERTRANS;
					MessageBox("Update","Special instruction record deleted.")

				END IF
			ELSE
				dw_pa_bk_estimation.object.mchar_siflag[1] = 'N'
			END IF
		END IF
		cb_sitxt.visible=FALSE
	ELSEIF data = "Y" THEN
		cb_sitxt.visible=TRUE
		cb_sitxt.Event clicked()
	END IF
ELSEIF dwo.name = "ri_prevbkmed" THEN
	IF data<>"" THEN
		dw_pa_bk_estimation.Object.ri_prevbkseq.Edit.Required='Yes'
	ELSE
		dw_pa_bk_estimation.Object.ri_prevbkseq.Edit.Required='No'
	END IF
ELSEIF dwo.name = "ri_prevbkseq" THEN
	dw_pa_bk_estimation.Object.ri_prevbkmed.Edit.Required='Yes'
ELSEIF dwo.name = "mchar_ricd" THEN
	string Lprvbkmed
	long Lprvbkseq
	Lprvbkmed = dw_pa_bk_estimation.object.ri_prevbkmed[1]
	Lprvbkseq = dw_pa_bk_estimation.object.ri_prevbkseq[1]
	IF Lprvbkmed="" OR IsNull(Lprvbkmed) OR IsNull(Lprvbkseq) THEN
		dw_pa_bk_estimation.Object.mchar_ricd.ValidationMsg='You must enter a previouse book number.'
		dw_pa_bk_estimation.SetColumn("ri_prevbkmed")
		dw_pa_bk_estimation.Object.ri_prevbkmed.Edit.Required='Yes'
		dw_pa_bk_estimation.Object.ri_prevbkseq.Edit.Required='Yes'
	END IF
ELSEIF dwo.name = "mchar_applen" THEN
	IF data = "" THEN
		RETURN 1
	END IF
ELSEIF dwo.name = "acquist_pbpage" THEN
	IF data = "" THEN
		RETURN 1
	END IF
END IF
end event

event rbuttondown;//
end event

event rbuttonup;//
end event

event pfc_deleterow;//
RETURN -1
end event

event pfc_addrow;//
RETURN -1
end event

event pfc_insertrow;//
RETURN -1
end event

event constructor;call super::constructor;// Make the background color of required field yellow
this.of_setcolorrequired(65535)
end event

event sqlpreview;call super::sqlpreview;//MessageBox("sql",sqlsyntax)

end event

type cb_exit from u_cb within w_pa_bk_estimation
integer x = 2583
integer y = 1448
integer width = 224
integer taborder = 0
string text = "E&xit"
end type

event clicked;call super::clicked;
parent.Event pfc_close()


end event

type cb_clear from u_cb within w_pa_bk_estimation
integer x = 2277
integer y = 1448
integer width = 242
integer taborder = 0
string text = "&Clear"
end type

event clicked;call super::clicked;// Reset the datawindows
dw_pa_bk_estimation.Reset()
dw_pa_prv_narrator.Reset()
dw_pa_coauth.Reset()
//dw_pa_bk_estimation.Object.ri_prevbkseq.Edit.Required='No'
//dw_pa_bk_estimation.Object.ri_prevbkmed.Edit.Required='No'

// Insert a blank row
dw_pa_bk_estimation.InsertRow(0)
dw_pa_prv_narrator.InsertRow(0)
dw_pa_coauth.InsertRow(0)

wf_set_taborder_zero()
dw_pa_bk_estimation.Object.DataWindow.QueryClear = "yes"
dw_pa_bk_estimation.Object.DataWindow.QueryMode='Yes'
dw_pa_bk_estimation.SetFocus()


end event

type cb_update from u_cb within w_pa_bk_estimation
integer x = 1934
integer y = 1448
integer width = 279
integer taborder = 0
boolean bringtotop = true
string text = "&Update"
end type

event clicked;call super::clicked;int rtn,Lcnt,rc, ls_other_conno_df, ls_other_conno_applen
string Lchno,Lconno,Lother_conno,ls_bkmed
string ls_other_conno_priority, ls_other_conno_siflag, ls_other_conno_vindx

dwItemStatus l_status,l_status2

// Check for any pending updates
IF of_UpdateChecks( ) < 0 THEN Return -1

//Check number of pages
IF (IsNull(dw_pa_bk_estimation.object.acquist_pbpage[1]) OR &
			dw_pa_bk_estimation.object.acquist_pbpage[1]=0) THEN 
			MessageBox("ERROR","Invalid number of pages.")
	dw_pa_bk_estimation.SetFocus()
	dw_pa_bk_estimation.SetColumn(31)
	Return -1
END IF

// Accept the text that was put on the screen.
dw_pa_bk_estimation.AcceptText()
dw_pa_coauth.AcceptText()
dw_pa_prv_narrator.AcceptText()

// update ttlinit
rtn = dw_pa_bk_estimation.Event pfc_update(TRUE,FALSE)
IF rtn = 1 THEN
	dw_pa_bk_estimation.Object.ttlinit_chno.key = 'No'
	
	dw_pa_bk_estimation.Object.ttlinit_b_ttl1.Update = 'No'
	dw_pa_bk_estimation.Object.ttlinit_b_ttl2.Update = 'No'
	dw_pa_bk_estimation.Object.ttlinit_serttl.Update = 'No'
	dw_pa_bk_estimation.Object.ttlinit_b_auth.Update = 'No'
	dw_pa_bk_estimation.Object.ttlinit_note.Update = 'No'
	dw_pa_bk_estimation.Object.ttlinit_cryr.Update = 'No'
	dw_pa_bk_estimAtion.Object.ttlinit_crname.UpdaTe = 'No'
	
	dw_pa_bk_estimation.Object.DataWindow.Table.UpdateTable='mchar'
	
	dw_pa_bk_estimation.Object.mchar_conno.Key='Yes'
	
//	dw_pa_bk_estimation.Object.mchar_andigcd.Update = 'Yes'
	dw_pa_bk_estimation.Object.mchar_df.Update = 'Yes'
	dw_pa_bk_estimation.Object.mchar_applen.Update = 'Yes'
	dw_pa_bk_estimation.Object.mchar_bkmed.Update = 'Yes'
	dw_pa_bk_estimation.Object.mchar_priority.Update = 'Yes'
	dw_pa_bk_estimation.Object.mchar_siflag.Update = 'Yes'
	dw_pa_bk_estimation.Object.mchar_ricd.Update = 'Yes'
	dw_pa_bk_estimation.Object.mchar_vindx.Update = 'Yes'
	
	// update mchar
	rc = dw_pa_bk_estimation.Event pfc_Update(True,False)
	if rc = 1 then
	//	dw_pa_bk_estimation.Object.mchar_andigcd.Update = 'No'
		dw_pa_bk_estimation.Object.mchar_df.Update = 'No'
		dw_pa_bk_estimation.Object.mchar_applen.Update = 'No'
		dw_pa_bk_estimation.Object.mchar_bkmed.Update = 'No'
		dw_pa_bk_estimation.Object.mchar_priority.Update = 'No'
		dw_pa_bk_estimation.Object.mchar_siflag.Update = 'No'
		dw_pa_bk_estimation.Object.mchar_ricd.Update = 'No'
		dw_pa_bk_estimation.Object.mchar_vindx.Update = 'No'
		dw_pa_bk_estimation.Object.ttlinit_chno.Update='No'
			
		dw_pa_bk_estimation.Object.DataWindow.Table.UpdateTable='ri'
			
		dw_pa_bk_estimation.Object.ttlinit_chno.key = 'No'
		dw_pa_bk_estimation.Object.mchar_conno.Key='Yes'
			
		dw_pa_bk_estimation.Object.mchar_conno.Update='Yes'
		dw_pa_bk_estimation.Object.ri_prevbkmed.Update='Yes'
		dw_pa_bk_estimation.Object.ri_prevbkseq.Update='Yes'
		
		Lconno = dw_pa_bk_estimation.object.mchar_conno[1]
			
		l_status  = dw_pa_bk_estimation.GetItemStatus(1, "ri_prevbkmed", Primary!)
		l_status2 = dw_pa_bk_estimation.GetItemStatus(1, "ri_prevbkseq", Primary!)
			
		IF l_status = DataModified! OR l_status2 = DataModified! THEN
			Select count(*) into :Lcnt from ri where conno=:Lconno using sqlservertrans;
			IF Lcnt = 0 THEN
		 		dw_pa_bk_estimation.SetItemStatus(1, 0, Primary!, NewModified!)
			ELSE
		 		dw_pa_bk_estimation.SetItemStatus(1, 0, Primary!, DataModified!)
			END IF	
		END IF
		
		//update ri
		rc = dw_pa_bk_estimation.Event pfc_Update(True,False)
		
		if rc = 1 then
			dw_pa_bk_estimation.Object.mchar_conno.Key='No'
			
			dw_pa_bk_estimation.Object.mchar_conno.Update='No'
			dw_pa_bk_estimation.Object.ri_prevbkmed.Update='No'
			dw_pa_bk_estimation.Object.ri_prevbkseq.Update='No'
				
			dw_pa_bk_estimation.Object.DataWindow.Table.UpdateTable='cr'
			
			dw_pa_bk_estimation.Object.ttlinit_chno.Key='Yes'
			
			dw_pa_bk_estimation.Object.ttlinit_chno.Update='Yes'
			dw_pa_bk_estimation.Object.cr_crpermcd.Update='Yes'
			dw_pa_bk_estimation.Object.cr_crrestxt.Update='Yes'
			Lchno = dw_pa_bk_estimation.object.ttlinit_chno[1]
			
			l_status  = dw_pa_bk_estimation.GetItemStatus(1, "cr_crpermcd", Primary!)
			l_status2 = dw_pa_bk_estimation.GetItemStatus(1, "cr_crrestxt", Primary!)
			
			IF l_status = DataModified! OR l_status2 = DataModified! THEN
				Select count(*) into :Lcnt from cr where chno=:Lchno using sqlservertrans;
				IF Lcnt = 0 THEN
			 		dw_pa_bk_estimation.SetItemStatus(1, 0, Primary!, NewModified!)
				ELSE
			 		dw_pa_bk_estimation.SetItemStatus(1, 0, Primary!, DataModified!)
				END IF	
			END IF
			// update cr
			rc = dw_pa_bk_estimation.Event pfc_Update(True,False)
			if rc = 1 then
				dw_pa_bk_estimation.Object.cr_crpermcd.Update='No'
				dw_pa_bk_estimation.Object.cr_crrestxt.Update='No'
								
				dw_pa_bk_estimation.Object.DataWindow.Table.UpdateTable='acquist'
				
				dw_pa_bk_estimation.Object.ttlinit_chno.Key='Yes'
				
				dw_pa_bk_estimation.Object.acquist_chno.Update = 'Yes'
				dw_pa_bk_estimation.Object.acquist_pbpage.Update = 'Yes'
				Lchno = dw_pa_bk_estimation.object.ttlinit_chno[1]
				
				l_status  = dw_pa_bk_estimation.GetItemStatus(1, "acquist_pbpage", Primary!)
				
				IF l_status = DataModified!  THEN
					Select count(*) into :Lcnt from acquist where chno=:Lchno using sqlservertrans;
					IF Lcnt = 0 THEN
						MessageBox("ERROR"," This book number has not gone through Acquisition of Source Material. Update did not take place.",StopSign!)
						RETURN -1
					END IF	
				END IF
				
				// Update acquist
				rc = dw_pa_bk_estimation.Event pfc_Update(True,False)
				if rc = 1 then
					// Set the update back to original ttlinit table.
					dw_pa_bk_estimation.Object.acquist_chno.Update = 'No'
					dw_pa_bk_estimation.Object.acquist_pbpage.Update = 'No'
					dw_pa_bk_estimation.Object.DataWindow.Table.UpdateTable='ttlinit'
				
					dw_pa_bk_estimation.Object.ttlinit_chno.Key='Yes'
					
					dw_pa_bk_estimation.Object.ttlinit_chno.Update = 'Yes'
					dw_pa_bk_estimation.Object.ttlinit_b_ttl1.Update = 'Yes'
					dw_pa_bk_estimation.Object.ttlinit_b_ttl2.Update = 'Yes'
					dw_pa_bk_estimation.Object.ttlinit_serttl.Update = 'Yes'
					dw_pa_bk_estimation.Object.ttlinit_b_auth.Update = 'Yes'
					dw_pa_bk_estimation.Object.ttlinit_note.Update = 'Yes'
					dw_pa_bk_estimation.Object.ttlinit_cryr.Update = 'Yes'
					dw_pa_bk_estimAtion.Object.ttlinit_crname.UpdaTe = 'Yes'
				else // update previouse narrator
					ROLLBACK USING SQLServerTrans;
					MessageBox("Error","Update failed. Error in table acquist",StopSign!)
					RETURN -1
				end if
				
				dw_pa_bk_estimation.ResetUpdate()
				
				rc = dw_pa_prv_narrator.Event pfc_Update(True,True)
				if rc = 1 then
					Lother_conno = dw_pa_bk_estimation.Object.mchar_other_media_conno[1]
					ls_bkmed = dw_pa_bk_estimation.Object.mchar_bkmed[1]
					
					IF NOT(IsNull(Lother_conno)) AND ls_bkmed <> 'BR' THEN
					
						ls_other_conno_df = dw_pa_bk_estimation.Object.mchar_df[1]
						ls_other_conno_applen = dw_pa_bk_estimation.Object.mchar_applen[1]
						ls_other_conno_priority = dw_pa_bk_estimation.Object.mchar_priority[1]
						ls_other_conno_siflag = dw_pa_bk_estimation.Object.mchar_siflag[1]
						ls_other_conno_vindx = dw_pa_bk_estimation.Object.mchar_vindx[1]
						
						UPDATE MCHAR
						SET df = :ls_other_conno_df,
						     applen = :ls_other_conno_applen,
							priority = :ls_other_conno_priority,
							siflag = :ls_other_conno_siflag,
							vindx = :ls_other_conno_vindx
						WHERE conno = :Lother_conno
						USING SQLServerTrans;			
						IF f_check_dberror(sqlservertrans,"MCHAR for other control number") = FALSE THEN
							ROLLBACK USING SqlServerTrans;
							RETURN -1
						END IF
						f_update_mchar_time(Lother_conno,0,"C","U")
					END IF
					
					// Mark the MCHAR Table
					f_update_mchar_time(Lconno,0,"C","U")
					
					COMMIT USING SQLServerTrans;
					MessageBox("Update","Update Successful.")
				else // update previouse narrator
					ROLLBACK USING SQLServerTrans;
					MessageBox("Error","Update failed. Error in table prvnarr",StopSign!)
					RETURN -1
				end if
			else // update cr
				ROLLBACK USING SQLServerTrans;
				MessageBox("Error","Update failed. Error in table cr",StopSign!)
				RETURN -1
			end if
		else // update ri
			ROLLBACK USING SQLServerTrans;
			MessageBox("Error","Update failed. Error in table ri",StopSign!)
			RETURN -1
		end if
	else // update mchar
		ROLLBACK USING SQLServerTrans;
		MessageBox("Error","Update failed. Error in table mchar",StopSign!)
		RETURN -1
	end if
else // update ttlinit
	ROLLBACK USING SQLServerTrans;
	MessageBox("Error","Update failed. Error in table ttlinit",StopSign!)
	RETURN -1
end if
end event

type cb_find from u_cb within w_pa_bk_estimation
integer x = 1618
integer y = 1448
integer width = 251
integer taborder = 0
boolean bringtotop = true
string text = "F&ind"
boolean default = true
end type

event clicked;call super::clicked;long ll_rows
string Lconno,Lsiflag,Lpriority,Lmed,Lchno,Lbkmed
double Ldf

dw_pa_bk_estimation.AcceptText()

IF (IsNull(dw_pa_bk_estimation.GetText())=FALSE AND &
	dw_pa_bk_estimation.GetText()<>"" )THEN
	IF f_is_it_archived(dw_pa_bk_estimation.GetText(),0) THEN
		dw_pa_bk_estimation.SetFocus()
		RETURN
	END IF
	// Set off the query mode.
	dw_pa_bk_estimation.Object.DataWindow.QueryMode='No'
	// Retrieve from database with control number as a argument.
	Open(w_pics_retrieve_msg_box)
	ll_rows = dw_pa_bk_estimation.Retrieve()
	IF ll_rows < 1 THEN
		// If no rows were found display a message to insert
		// a new reocrd or not.
		Close(w_pics_retrieve_msg_box)
		MessageBox("Find Error", "Record does not exist. Probable causes:~r1- Invalid Control Number.~r2- Control number has not yet been acquired, from Acquistion of Source Material.~r3- Copyright has not been processed." ,Information!)
		dw_pa_bk_estimation.InsertRow(0)
		// Set the query mode to yes, to retrieve other records.
		dw_pa_bk_estimation.Object.DataWindow.QueryClear = "Yes"
		dw_pa_bk_estimation.Object.DataWindow.QueryMode='Yes'
		dw_pa_bk_estimation.object.mchar_conno[1]=Lconno
		wf_set_taborder_zero()
	ELSE
		Lsiflag = dw_pa_bk_estimation.object.mchar_siflag[1]
		IF Lsiflag="Y" THEN
			cb_sitxt.Visible = TRUE
		ELSE
			dw_pa_bk_estimation.object.mchar_siflag[1] = "N"
		END IF
		Ldf = dw_pa_bk_estimation.object.mchar_df[1]
		IF IsNull(Ldf) THEN
			dw_pa_bk_estimation.object.mchar_df[1] = 1.00
		END IF
		Lpriority = dw_pa_bk_estimation.object.mchar_priority[1]
		IF IsNull(Lpriority) OR Lpriority="" THEN
			dw_pa_bk_estimation.object.mchar_priority[1] = "N"
		END IF
//		Landigcd = dw_pa_bk_estimation.object.mchar_andigcd[1]
//		IF IsNull(Landigcd) OR Landigcd="" THEN
//			dw_pa_bk_estimation.object.mchar_andigcd[1] = "N"
//		END IF
		Lmed = dw_pa_bk_estimation.object.mchar_med[1]
		Lbkmed = Lmed
		IF IsNull(Lbkmed) OR Lbkmed="" THEN
			MessageBox("Error","Title has not yet passed Confirm Consideration.")
			dw_pa_bk_estimation.InsertRow(0)
			// Set the query mode to yes, to retrieve other records.
			dw_pa_bk_estimation.Object.DataWindow.QueryClear = "yes"
			dw_pa_bk_estimation.Object.DataWindow.QueryMode='Yes'
			dw_pa_bk_estimation.object.mchar_conno[1]=Lconno
			wf_set_taborder_zero()
		ELSE
			wf_set_taborder_org()
			IF Lmed="BR" OR Lmed="P/B" THEN
//				dw_pa_bk_estimation.Object.mchar_andigcd.TabSequence = 0
				dw_pa_bk_estimation.object.mchar_applen_t.text = "Braille Pages"
				dw_pa_prv_narrator.visible = FALSE
			ELSEIF Lmed="RC" OR Lmed="FD" OR Lmed="RTB" THEN
				dw_pa_bk_estimation.object.mchar_applen_t.text = "Estimated Tracks"
				dw_pa_prv_narrator.visible = TRUE
			END IF
			Lchno = dw_pa_bk_estimation.object.ttlinit_chno[1]
			Lconno = dw_pa_bk_estimation.object.mchar_conno[1]
			ll_rows = dw_pa_prv_narrator.Retrieve(Lconno)
			IF ll_rows = 0 THEN
				dw_pa_prv_narrator.InsertRow(0)
			END IF			
			ll_rows = dw_pa_coauth.Retrieve(Lchno)
			IF ll_rows = 0 THEN
				dw_pa_coauth.InsertRow(0)
			END IF
		END IF
		Close(w_pics_retrieve_msg_box)
	END IF
	dw_pa_bk_estimation.setfocus()
ELSE
	MessageBox("ERROR","Please enter control number or a book number")
	dw_pa_bk_estimation.SetFocus()
	RETURN
END IF
end event

type dw_pa_prv_narrator from u_pics_dw within w_pa_bk_estimation
event ue_enterkey pbm_dwnprocessenter
integer x = 1317
integer y = 1168
integer width = 1495
integer height = 240
integer taborder = 20
string dataobject = "d_pa_prv_narrator"
boolean vscrollbar = false
boolean livescroll = false
boolean ib_isupdateable = false
boolean ib_rmbmenu = false
boolean ib_rmbfocuschange = false
string is_updatesallowed = ""
end type

event ue_enterkey;call super::ue_enterkey;Send(Handle(this),256,9,Long(0,0))
return(1)

end event

event ue_postconstructor;call super::ue_postconstructor;this.of_SetTransObject( SQLServerTrans )
this.of_SetDropDownSearch(TRUE)
this.inv_dropdownsearch.of_AddColumn("prvnarrln")
this.inv_dropdownsearch.of_AddColumn("prvnarrfn")
this.GetChild ("prvnarrln", ldwc_narr)
ldwc_narr.SetTransObject(sqlservertrans)

end event

event rbuttondown;//
end event

event rbuttonup;//
end event

event itemchanged;call super::itemchanged;IF dwo.Name = "prvnarrln" THEN
	IF data<>"" THEN
		string Lconno,Lprvnarrfn
		Lconno = dw_pa_bk_estimation.object.mchar_conno[1]
		dw_pa_prv_narrator.object.conno[1] = Lconno
		dw_pa_prv_narrator.object.prvnarrfn[1]=ldwc_narr.GetItemString(ldwc_narr.GetRow(),"narrfn")	
	ELSE
		dw_pa_prv_narrator.object.prvnarrfn[1]=""	
	END IF		
END IF
	

end event

event editchanged;call super::editchanged;IF IsValid(inv_dropdownsearch) THEN
	this.inv_dropdownsearch.Event pfc_editchanged(row,dwo,data)
END IF
end event

event itemfocuschanged;call super::itemfocuschanged;IF IsValid(inv_dropdownsearch) THEN
	this.inv_dropdownsearch.Event pfc_ItemFocusChanged(row,dwo)
END IF
end event

event sqlpreview;call super::sqlpreview;//MessageBox("sql",sqlsyntax)

end event

type cb_sitxt from u_cb within w_pa_bk_estimation
boolean visible = false
integer x = 46
integer y = 1436
integer width = 485
integer height = 96
integer taborder = 0
string text = "Special Instruction..."
end type

event clicked;call super::clicked;string Lconno
Lconno = dw_pa_bk_estimation.object.mchar_conno[1]
openwithparm(w_special_instruction,Lconno)
end event

type dw_pa_coauth from u_pics_dw within w_pa_bk_estimation
integer x = 32
integer y = 1168
integer width = 1285
integer height = 240
integer taborder = 0
string dataobject = "d_pa_coauth"
boolean vscrollbar = false
boolean livescroll = false
boolean ib_isupdateable = false
boolean ib_rmbmenu = false
boolean ib_rmbfocuschange = false
string is_updatesallowed = ""
end type

event ue_postconstructor;call super::ue_postconstructor;dw_pa_coauth.of_SetTransObject( SQLServerTrans )
end event

event rbuttondown;//
end event

event rbuttonup;//
end event

event sqlpreview;call super::sqlpreview;//MessageBox("sql",sqlsyntax)

end event


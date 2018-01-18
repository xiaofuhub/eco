$PBExportHeader$w_sheet_annual_contract_init.srw
$PBExportComments$Window to add/update contract initialization
forward
global type w_sheet_annual_contract_init from w_sheet
end type
type cb_sub from commandbutton within w_sheet_annual_contract_init
end type
type cb_cntr from commandbutton within w_sheet_annual_contract_init
end type
type cb_find from commandbutton within w_sheet_annual_contract_init
end type
type cb_update from commandbutton within w_sheet_annual_contract_init
end type
type cb_clear from commandbutton within w_sheet_annual_contract_init
end type
type cb_exit from commandbutton within w_sheet_annual_contract_init
end type
type dw_annual_anucost from u_pics_dw within w_sheet_annual_contract_init
end type
type dw_contract_init from u_pics_dw within w_sheet_annual_contract_init
end type
type cb_prdr from commandbutton within w_sheet_annual_contract_init
end type
end forward

global type w_sheet_annual_contract_init from w_sheet
integer x = 5
integer y = 4
integer width = 2967
integer height = 1820
string title = "Contract Annual Initialization"
cb_sub cb_sub
cb_cntr cb_cntr
cb_find cb_find
cb_update cb_update
cb_clear cb_clear
cb_exit cb_exit
dw_annual_anucost dw_annual_anucost
dw_contract_init dw_contract_init
cb_prdr cb_prdr
end type
global w_sheet_annual_contract_init w_sheet_annual_contract_init

forward prototypes
public subroutine wf_set_taborder_zero ()
public subroutine wf_set_taborder_original ()
public function boolean wf_get_prodstages (string lcntrtype, string lcntrmed)
end prototypes

public subroutine wf_set_taborder_zero ();dw_contract_init.object.ancntr_cntr.tabsequence='10'
dw_contract_init.object.ancntr_cntrlc.tabsequence='0'
dw_contract_init.object.ancntr_cntrfy.tabsequence='0'   
dw_contract_init.object.ancntr_cntr_no_med.tabsequence='0'   
dw_contract_init.object.ancntr_cntrtype.tabsequence='0'   
dw_contract_init.object.ancntr_prdr.tabsequence='0'   
dw_contract_init.object.ancntr_cntrcvcd.tabsequence='0'   
dw_contract_init.object.controller_userid.tabsequence='0'   
dw_contract_init.object.controller_email.tabsequence='0'   
dw_contract_init.object.ancntr_cntrmed.tabsequence='0'   
dw_contract_init.object.ancntr_prdr.tabsequence='0'   
dw_contract_init.object.pcs_reject.tabsequence='0'   
dw_contract_init.object.qas_reject.tabsequence='0'   
dw_contract_init.object.foreign_lang.tabsequence='0'   
dw_contract_init.object.ancntr_cntrttl.tabsequence='0'   
dw_contract_init.object.ship_completed.tabsequence='0'   
dw_contract_init.object.invoice_completed.tabsequence='0'   
dw_annual_anucost.ResetUpdate()
dw_contract_init.ResetUpdate()
w_sheet_annual_contract_init.cb_update.Enabled=FALSE
w_sheet_annual_contract_init.cb_clear.Enabled=FALSE
w_sheet_annual_contract_init.cb_sub.Enabled=FALSE
cb_cntr.Enabled = FALSE




end subroutine

public subroutine wf_set_taborder_original ();dw_contract_init.object.ancntr_cntr.tabsequence='0'
dw_contract_init.object.ancntr_cntrlc.tabsequence='10'   
dw_contract_init.object.ancntr_cntr_status.tabsequence='20'   
dw_contract_init.object.ancntr_cntrfy.tabsequence='30'   
dw_contract_init.object.ancntr_cntr_no_med.tabsequence='40'   
dw_contract_init.object.ancntr_cntrmed.tabsequence='50'   
dw_contract_init.object.ancntr_cntrtype.tabsequence='60'   
dw_contract_init.object.ancntr_prdr.tabsequence='70'   
dw_contract_init.object.pcs_reject.tabsequence='80'   
dw_contract_init.object.qas_reject.tabsequence='90'   
dw_contract_init.object.foreign_lang.tabsequence='110'   
dw_contract_init.object.ancntr_cntrttl.tabsequence='120'   
dw_contract_init.object.ancntr_avqnty.tabsequence='130'   
dw_contract_init.object.ancntr_avlen.tabsequence='140'   
dw_contract_init.object.ancntr_cntrdol.tabsequence='150'   
dw_contract_init.object.ancntr_booktype.tabsequence='160'   
dw_contract_init.object.ancntr_cntrcvcd.tabsequence='170' 
dw_contract_init.object.controller_userid.tabsequence='180'   
dw_contract_init.object.controller_email.tabsequence='190'   
dw_contract_init.object.cntr_addr.tabsequence='200'   
dw_contract_init.object.cntr_city.tabsequence='210'   
dw_contract_init.object.cntr_state.tabsequence='220'   
dw_contract_init.object.cntr_zip.tabsequence='230' 
dw_contract_init.object.ship_completed.tabsequence='240'   
dw_contract_init.object.invoice_completed.tabsequence='250'   


end subroutine

public function boolean wf_get_prodstages (string lcntrtype, string lcntrmed);// Get the contract type and medium and get the appropriate
// production stages.
int rtn
DataWindowChild ldwc_prodstage
dw_annual_anucost.GetChild ("prodstage", ldwc_prodstage)
ldwc_prodstage.SetTransObject(sqlservertrans)
rtn = ldwc_prodstage.Retrieve(Lcntrtype,Lcntrmed)
IF rtn > 0 THEN
	RETURN TRUE
ELSE
	RETURN FALSE
END IF

end function

event open;call super::open;string ls_flang
// Open the sheet in Maximized mode
this.windowstate = maximized!

m_pics_main.m_file.m_print.Enabled 			=	FALSE
m_pics_main.m_file.m_pagesetup.Enabled		=	TRUE
m_pics_main.m_file.m_printimmediate.Enabled	=	TRUE
// Set the tab order to zero
wf_set_taborder_zero()
// Set the query mode on
dw_contract_init.Object.DataWindow.QueryMode='Yes'
// Disable the bottom datawindow for data entry.
dw_annual_anucost.Enabled = FALSE
// set focus on cntr.
dw_contract_init.SetFocus()

end event

on w_sheet_annual_contract_init.create
int iCurrent
call super::create
this.cb_sub=create cb_sub
this.cb_cntr=create cb_cntr
this.cb_find=create cb_find
this.cb_update=create cb_update
this.cb_clear=create cb_clear
this.cb_exit=create cb_exit
this.dw_annual_anucost=create dw_annual_anucost
this.dw_contract_init=create dw_contract_init
this.cb_prdr=create cb_prdr
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.cb_sub
this.Control[iCurrent+2]=this.cb_cntr
this.Control[iCurrent+3]=this.cb_find
this.Control[iCurrent+4]=this.cb_update
this.Control[iCurrent+5]=this.cb_clear
this.Control[iCurrent+6]=this.cb_exit
this.Control[iCurrent+7]=this.dw_annual_anucost
this.Control[iCurrent+8]=this.dw_contract_init
this.Control[iCurrent+9]=this.cb_prdr
end on

on w_sheet_annual_contract_init.destroy
call super::destroy
destroy(this.cb_sub)
destroy(this.cb_cntr)
destroy(this.cb_find)
destroy(this.cb_update)
destroy(this.cb_clear)
destroy(this.cb_exit)
destroy(this.dw_annual_anucost)
destroy(this.dw_contract_init)
destroy(this.cb_prdr)
end on

event resize;call super::resize;//long ll_height
//
//This.X = w_pics_main.X
//This.Y = w_pics_main.Y
//ll_height = w_pics_main.mdi_1.Height
//This.Resize(w_pics_main.WorkSpaceWidth(), ll_height)
//

end event

event pfc_preopen;call super::pfc_preopen;this.of_SetPreference(TRUE)
this.inv_preference.of_SetToolBars(TRUE)
this.inv_preference.of_SetWindow(TRUE)
this.of_SetResize(TRUE)
this.inv_resize.of_SetOrigSize(this.workspacewidth(),this.workspaceheight())
inv_resize.of_Register(dw_annual_anucost, "Scale")
inv_resize.of_Register(dw_contract_init, "Scale")
inv_resize.of_Register(cb_clear, "Scale")
inv_resize.of_Register(cb_exit, "Scale")
inv_resize.of_Register(cb_find, "Scale")
inv_resize.of_Register(cb_prdr, "Scale")
inv_resize.of_Register(cb_sub, "Scale")
inv_resize.of_Register(cb_cntr, "Scale")
inv_resize.of_Register(cb_update, "Scale")
// go into query mode
dw_contract_init.Object.DataWindow.QueryMode='Yes'


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

type cb_sub from commandbutton within w_sheet_annual_contract_init
integer x = 1275
integer y = 1560
integer width = 686
integer height = 104
integer taborder = 40
integer textsize = -8
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "Add/Update &Subcontractors"
end type

event clicked;string lcntr,lcntrmed,lcntrtype
str_ancntrall lstr_ancntr
lstr_ancntr.cntr = dw_contract_init.object.ancntr_cntr[1]
lstr_ancntr.cntrmed = dw_contract_init.object.ancntr_cntrmed[1]
lstr_ancntr.cntrtype = dw_contract_init.object.ancntr_cntrtype[1]
openwithparm(w_addsub,lstr_ancntr)

end event

type cb_cntr from commandbutton within w_sheet_annual_contract_init
integer x = 649
integer y = 1560
integer width = 613
integer height = 104
integer taborder = 30
integer textsize = -8
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "Contract A&dditional Costs"
end type

event clicked;string lcntr,lcntrmed
str_ancntr lstr_ancntr
lstr_ancntr.cntr = dw_contract_init.object.ancntr_cntr[1]
lstr_ancntr.cntrmed = dw_contract_init.object.ancntr_cntrmed[1]
openwithparm(w_addcost,lstr_ancntr)
dw_contract_init.GroupCalc()
end event

type cb_find from commandbutton within w_sheet_annual_contract_init
event clicked pbm_bnclicked
event mousemove pbm_mousemove
string tag = "Find the record"
integer x = 1975
integer y = 1560
integer width = 219
integer height = 104
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "F&ind"
boolean default = true
end type

event clicked;Long ll_rows
String Lcntr,lcntrtype,lcntrmed, ls_addr, ls_city, ls_state, ls_zip
Int rtn,Lcount
// get the contract number
Lcntr = dw_contract_init.GetText()
IF Lcntr <> "" THEN
	// Enable the bottom datawindow
	dw_annual_anucost.enabled = TRUE
	// Set the query mode to No.
	dw_contract_init.object.DataWindow.QueryMode='No'
	
	// Retrieve the data.
	ll_rows = dw_contract_init.Retrieve(Lcntr)
	// If no data was found
  	IF ll_rows < 1 THEN
		// display a message and ask if a new contract number is getting inserted.
      		rtn = Messagebox("Find Error", "Contract Number: "+Lcntr+" does not exist. Insert new record?" ,question!, okCancel!, 1)
		// If new contract number
		IF rtn=1 THEN
			// Insert a blank row.
     			dw_contract_init.EVENT pfc_InsertRow()
			// Set the contract number and make the contract number unchangeable.
			dw_contract_init.SetItem(1,"ancntr_cntr",Lcntr)
			// Make the contract and active contract
			dw_contract_init.object.ancntr_cntr_status[1]="A"
			dw_contract_init.object.foreign_lang[1]="N"
			dw_contract_init.object.controller_userid[1]="pmag"
			dw_contract_init.object.controller_email[1]="nls@loc.gov"
			// Set the tab order to original for the rest of the fields on the screen
			wf_set_taborder_original()
			
			// Make find and delete push buttons disabled and the rest enabled.
			w_sheet_annual_contract_init.cb_find.enabled=FALSE
		   	w_sheet_annual_contract_init.cb_update.enabled=TRUE
		   	w_sheet_annual_contract_init.cb_clear.enabled=TRUE
			w_sheet_annual_contract_init.cb_prdr.enabled=TRUE
			// Set the focus on contract datawindow
		       dw_contract_init.SetFocus()
			dw_contract_init.SetColumn("ancntr_cntrlc") 
		ELSE // if rtn <>
			// If new contract number is not to be inserted, insert a row anyway
     			dw_contract_init.EVENT pfc_InsertRow()
			// Return to query mode to retrieve the next contract number.
			dw_contract_init.object.DataWindow.QueryMode='Yes'
			// Display the contract number that was just entered
			dw_contract_init.SetItem(1,"ancntr_cntr",Lcntr)
			// Set the focus on contract datawindow
			wf_set_taborder_zero()
		   dw_contract_init.SetFocus()
			dw_contract_init.SetColumn("ancntr_cntrlc") 
		END IF// end of rtn<>1 want to insert non exist cntr #
	ELSE//if ll_row >=1
		cb_cntr.enabled = TRUE
		// Get the contract type and medium and get the appropriate
		// production stages.
		lcntrtype = dw_contract_init.object.ancntr_cntrtype[1]
		lcntrmed  = dw_contract_init.object.ancntr_cntrmed[1]
		ls_addr=dw_contract_init.object.cntr_addr[1]
		IF ls_addr='' OR IsNull(ls_addr) THEN
			ls_addr = dw_contract_init.object.producer_prdr_addr[1]
			ls_city  = dw_contract_init.object.producer_prdr_city[1]
			ls_state = dw_contract_init.object.producer_prdr_state[1]
			ls_zip  = dw_contract_init.object.producer_prdr_zip[1]
			dw_contract_init.object.cntr_addr[1]=ls_addr
			dw_contract_init.object.cntr_city[1]=ls_city
			dw_contract_init.object.cntr_state[1]=ls_state
			dw_contract_init.object.cntr_zip[1]=ls_zip
		END IF
		// If contract number exist and this is an update to existing contract number.
		// Retrieve the annual cost information from anucost table.
		IF wf_get_prodstages(lcntrtype,lcntrmed)=FALSE THEN
			Messagebox("ERROR","There are no production information in prodstage table"+ &
										" associated with contract type = "+lcntrtype+" and "+ &
										" contract media = "+lcntrmed+" .")
		END IF
		SELECT count(*) INTO :Lcount FROM anucost where cntr=:Lcntr USING SqlServerTrans;
		IF f_check_dberror(SqlServerTrans,"ANUCOST")=TRUE THEN
			IF Lcount=0 OR IsNull(Lcount) THEN
				rtn = Messagebox("Find Error", "Contract Number: "+Lcntr+" Does not exist in anucost table ! ~rInsert a new row into anucost table ?" ,question!, okCancel!, 1)
				// If new contract number is getting added into anucost table
				IF rtn=1 THEN
					// Insert a blank row.
     				dw_annual_anucost.EVENT pfc_InsertRow()
				END IF
			ELSE
				dw_annual_anucost.Retrieve(Lcntr)
			END IF
		END IF
		// Set the tab back to original on the rest of the fields.
		wf_set_taborder_original()
		// Enable update,delete,clear and add producer push buttons
		m_pics_main.m_file.m_print.enabled 			=	TRUE
		w_sheet_annual_contract_init.cb_find.enabled=FALSE
		w_sheet_annual_contract_init.cb_update.enabled=TRUE
		w_sheet_annual_contract_init.cb_clear.enabled=TRUE
		w_sheet_annual_contract_init.cb_prdr.enabled=TRUE
		w_sheet_annual_contract_init.cb_sub.enabled=TRUE
		// Set the focus on contract datawindow.
		dw_contract_init.SetFocus()
		dw_contract_init.SetColumn("ancntr_cntrlc") 
  	END IF //end if ll_row>1
ELSE
	// If no contract number was entered, diaplay a error message.
   	Messagebox("Error", "Please enter the Contract Number." ,information!)
	dw_contract_init.SetFocus()
END IF // end IF lcntr=''


end event

event mousemove;SetMicroHelp(w_pics_main,this.tag)
end event

type cb_update from commandbutton within w_sheet_annual_contract_init
event clicked pbm_bnclicked
event mousemove pbm_mousemove
string tag = "Update the record"
integer x = 2213
integer y = 1560
integer width = 242
integer height = 104
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "&Update"
end type

event clicked;DwItemStatus l_status
Int rc,li_rc, lcntrfy, lcnt=0, rtn
String Lcntr, lcntrmed, lcntrtype, Lcntrcvcd, lprdr, lprdrname

// Check for any pending updates
IF ((dw_contract_init.ModifiedCount() > 0  OR &
	 dw_annual_anucost.ModifiedCount() > 0 OR &
	 dw_annual_anucost.DeletedCount() > 0)) THEN

	Lcntr 		= dw_contract_init.object.ancntr_cntr[1]
	lcntrmed 	= dw_contract_init.object.ancntr_cntrmed[1]
	lcntrtype 	= dw_contract_init.object.ancntr_cntrtype[1]
	lcntrfy 		= dw_contract_init.object.ancntr_cntrfy[1]
	Lcntrcvcd	= dw_contract_init.object.ancntr_cntrcvcd[1]
	lprdr 		= dw_contract_init.object.ancntr_prdr[1]
	lprdrname	= dw_contract_init.object.producer_prdr_name[1]
			
	l_status = dw_contract_init.GetItemStatus(1,0,primary!)
	CHOOSE CASE l_status
	CASE dataModified!
		Lcntr = Trim(Lcntr)
		lcntrmed = Trim(lcntrmed)
		
		IF dw_annual_anucost.RowCount() = 0 THEN
			rtn = MessageBox("Warning","You have not created any production cost information for this contract number. Do you want to continue with update?",Question!,YesNo!,1)
			IF rtn = 1 THEN
				// unsupported appeon feature 3/24/10 GOTO replaced with code
						rc = dw_contract_init.EVENT pfc_update(TRUE,FALSE)
		// If successfull, update anucost table
		IF rc = 1 THEN
			rc = dw_annual_anucost.EVENT pfc_update(TRUE,FALSE)
			IF rc = 1 THEN
				COMMIT USING SqlServerTrans;
				// Reset the flags
				dw_annual_anucost.ResetUpdate()
				dw_contract_init.ResetUpdate()
				Messagebox("Update","Update Successful.",information!)
				RETURN 1
			ELSE // update anucost
				ROLLBACK USING SqlServerTrans;
				Messagebox("Error","Update failed.",stopSign!)
				RETURN -1
			END IF
		ELSE // update ancntr
			ROLLBACK USING SqlServerTrans;
			Messagebox("Error","Update failed.",stopSign!)
			RETURN -1
		END IF

				//
			ELSE
				dw_annual_anucost.SetFocus()
				RETURN -1
			END IF
		END IF
		rc = dw_contract_init.EVENT pfc_update(TRUE,FALSE)
		// If successfull, update anucost table
		IF rc = 1 THEN
			rc = dw_annual_anucost.EVENT pfc_update(TRUE,FALSE)
			IF rc = 1 THEN
				COMMIT USING SqlServerTrans;
				// Reset the flags
				dw_annual_anucost.ResetUpdate()
				dw_contract_init.ResetUpdate()
				Messagebox("Update","Update Successful.",information!)
				RETURN 1
			ELSE // update anucost
				ROLLBACK USING SqlServerTrans;
				Messagebox("Error","Update failed.",stopSign!)
				RETURN -1
			END IF
		ELSE // update ancntr
			ROLLBACK USING SqlServerTrans;
			Messagebox("Error","Update failed.",stopSign!)
			RETURN -1
		END IF
	CASE newModified!
		// Insert into ancntr table. (RS21n)
		IF dw_annual_anucost.RowCount() = 0 THEN
			rtn = MessageBox("Warning","You have not created any production cost information for this contract number. Do you want to continue with update?",Question!,YesNo!,1)
			IF rtn = 1 THEN
				rc = dw_contract_init.EVENT pfc_update(TRUE,FALSE)
			// If successfull, update anucost table
			IF rc = 1 THEN
				rc = dw_annual_anucost.EVENT pfc_update(TRUE,FALSE)
				IF rc = 1 THEN
				COMMIT USING SqlServerTrans;
				// Reset the flags
				dw_annual_anucost.ResetUpdate()
				dw_contract_init.ResetUpdate()
				Messagebox("Update","Update Successful.",information!)
				RETURN 1
				ELSE // update anucost
				ROLLBACK USING SqlServerTrans;
				Messagebox("Error","Update failed.",stopSign!)
				RETURN -1
			END IF
		ELSE // update ancntr
			ROLLBACK USING SqlServerTrans;
			Messagebox("Error","Update failed.",stopSign!)
			RETURN -1
		END IF

			ELSE
				dw_annual_anucost.SetFocus()
				RETURN -1
			END IF
		END IF

		rc = dw_contract_init.EVENT pfc_update(TRUE,FALSE)
		// If successfull, update anucost table
		IF rc = 1 THEN
			rc = dw_annual_anucost.EVENT pfc_update(TRUE,FALSE)
			IF rc = 1 THEN
				COMMIT USING SqlServerTrans;
				// Reset the flags
				dw_annual_anucost.ResetUpdate()
				dw_contract_init.ResetUpdate()
				Messagebox("Update","Update Successful.",information!)
				RETURN 1
			ELSE // update anucost
				ROLLBACK USING SqlServerTrans;
				Messagebox("Error","Update failed.",stopSign!)
				RETURN -1
			END IF
		ELSE // update ancntr
			ROLLBACK USING SqlServerTrans;
			Messagebox("Error","Update failed.",stopSign!)
			RETURN -1
		END IF
	END CHOOSE

	IF dw_annual_anucost.ModifiedCount() > 0 THEN
		rc = dw_annual_anucost.EVENT pfc_update(TRUE,TRUE)
		IF rc = 1 THEN
			COMMIT USING SqlServerTrans;
			// Reset the flags
			dw_annual_anucost.ResetUpdate()
			Messagebox("Update","Update Successful.",information!)
			RETURN 1
		ELSE // update anucost
			ROLLBACK USING SqlServerTrans;
			Messagebox("Error","Update table anucost failed.",stopSign!)
			RETURN -1
		END IF
	END IF


END IF	
end event

event mousemove;SetMicroHelp(w_pics_main,this.tag)
end event

type cb_clear from commandbutton within w_sheet_annual_contract_init
event clicked pbm_bnclicked
event mousemove pbm_mousemove
string tag = "Clear the screen"
integer x = 2473
integer y = 1560
integer width = 206
integer height = 104
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "&Clear"
end type

event clicked;long ll_rows,rtn

dw_contract_init.AcceptText()

IF dw_contract_init.ModifiedCount( ) > 0 THEN
	rtn=MessageBox("Clear","Save the changes before clearing the screen?",Question!,YesNo!,2)
	IF rtn = 1 THEN
		w_sheet_annual_contract_init.cb_update.TriggerEvent(Clicked!)
	END IF
END IF
dw_annual_anucost.Reset()
dw_contract_init.Reset()
ll_rows = dw_contract_init.InsertRow(0)
dw_contract_init.ScrolltoRow(ll_rows)
// Set the tab order to zero
wf_set_taborder_zero()
// clear the query
dw_contract_init.Object.DataWindow.QueryClear = "Yes"
// go into query mode
dw_contract_init.Object.DataWindow.QueryMode='Yes'
// set focus on cntr.
dw_contract_init.SetFocus()

w_sheet_annual_contract_init.cb_find.Enabled=TRUE
w_sheet_annual_contract_init.cb_update.Enabled=FALSE
w_sheet_annual_contract_init.cb_clear.Enabled=FALSE
w_sheet_annual_contract_init.cb_find.Default=TRUE

end event

event mousemove;SetMicroHelp(w_pics_main,this.tag)
end event

type cb_exit from commandbutton within w_sheet_annual_contract_init
event clicked pbm_bnclicked
event mousemove pbm_mousemove
string tag = "Exit the screen"
integer x = 2697
integer y = 1560
integer width = 178
integer height = 104
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "E&xit"
end type

event clicked;m_pics_main.m_file.m_print.Enabled 			=	FALSE
m_pics_main.m_file.m_pagesetup.Enabled		=	FALSE
m_pics_main.m_file.m_printimmediate.Enabled	=	FALSE
m_pics_main.m_edit.m_addrow.Enabled =	FALSE
m_pics_main.m_edit.m_deleterow.Enabled =	FALSE
parent.Event pfc_close()
end event

event mousemove;SetMicroHelp(w_pics_main,this.tag)
end event

type dw_annual_anucost from u_pics_dw within w_sheet_annual_contract_init
event ue_enterkey pbm_dwnprocessenter
event pfc_hinttext pbm_mousemove
integer x = 32
integer y = 1128
integer width = 2843
integer height = 404
integer taborder = 20
string dataobject = "d_annual_anucost"
boolean hscrollbar = true
end type

event ue_enterkey;int li_ColNbr
//
//li_ColNbr = dw_annual_anucost.GetColumn() 
////messagebox("test",string(li_colnbr))
//IF (li_ColNbr=8) THEN
 	dw_annual_anucost.Event pfc_addrow ()
	this.SetColumn("prodstage")

//ELSE
//	Send(Handle(this),256,9,Long(0,0))
//	return(1)
//END IF
end event

event pfc_hinttext;string ls_object, ls_column, ls_column_tag
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

event getfocus;call super::getfocus;int rownum
string Lcntr,Lmed

// If the datawindow "anucost" has no rows, or we are the last row of this 
// datawindows, insert a new row and set the value of contract number with
// corresponding contract medium.

IF dw_annual_anucost.RowCount() = 0  THEN	
		dw_contract_init.AcceptText()
		Lcntr = dw_contract_init.object.ancntr_cntr[1]
		Lmed = dw_contract_init.object.ancntr_cntrmed[1]

		dw_annual_anucost.InsertRow(0)
		dw_annual_anucost.object.cntr[1] = Lcntr
		dw_annual_anucost.object.cntrmed[1] = Lmed
		dw_annual_anucost.object.qcdelay[1] = 0 // 02/01/2008 prevent null insert oracle error
END IF
m_pics_main.m_edit.m_addrow.Enabled =	TRUE
m_pics_main.m_edit.m_deleterow.Enabled =	TRUE

end event

event pfc_addrow;long	ll_rc
string Lcntr,Lmed
integer li_loop
string ls_prodstage


// 10/09/2008  Tracker Item - Prevent new row if prod stage is not filled in
ll_rc = This.Rowcount()

IF ll_rc > 0 THEN
	FOR li_loop = 1 TO ll_rc
			ls_prodstage = this.object.prodstage[li_loop]
			IF isnull(ls_prodstage) THEN
				Messagebox('Error', 'Please  fill in the production stage before inserting a new record')
				RETURN -1
			END IF
	NEXT
END IF
///////////

// Insert row
if IsValid (inv_rowmanager) then
	ll_rc = inv_rowmanager.event pfc_addrow ()
else
	ll_rc = this.InsertRow (0) 
end if

// Notify the Linkage Service that a new row has been added.
IF IsValid ( inv_Linkage ) THEN 
	inv_Linkage.Event pfc_InsertRow (ll_rc) 
END IF 

dw_contract_init.AcceptText()
Lcntr = dw_contract_init.object.ancntr_cntr[1]
Lmed = dw_contract_init.object.ancntr_cntrmed[1]

dw_annual_anucost.object.cntr[ll_rc] = Lcntr
dw_annual_anucost.object.cntrmed[ll_rc] = Lmed
dw_annual_anucost.object.qcdelay[ll_rc] = 0

this.ScrollToRow(ll_rc)

return ll_rc
end event

event rbuttonup;//
end event

event rbuttondown;//
end event

event pfc_insertrow;long	ll_currow
long	ll_rc
string Lcntr,Lmed

// Get current row
ll_currow = this.GetRow()
if ll_currow < 0 then
	ll_currow = 0
end if

// Insert row
if IsValid (inv_rowmanager) then
	ll_rc = inv_rowmanager.event pfc_insertrow (ll_currow)
else
	ll_rc = this.InsertRow (ll_currow) 
end if

// Notify the Linkage Service that a new row has been added.
IF IsValid ( inv_Linkage ) THEN 
	inv_Linkage.Event pfc_InsertRow (ll_rc) 
END IF 
dw_contract_init.AcceptText()
Lcntr = dw_contract_init.object.ancntr_cntr[1]
Lmed = dw_contract_init.object.ancntr_cntrmed[1]

dw_annual_anucost.object.cntr[ll_rc] = Lcntr
dw_annual_anucost.object.cntrmed[ll_rc] = Lmed
dw_annual_anucost.object.qcdelay[ll_rc] = 0 // 02/01/2008 prevent null insert oracle error

return ll_rc
end event

event ue_postconstructor;call super::ue_postconstructor;dw_annual_anucost.of_SetTransObject( SQLServerTrans )

end event

event constructor;call super::constructor;this.of_SetPrintPreview(TRUE)
this.of_SetTransObject(SQLServerTrans)

end event

event itemchanged;call super::itemchanged;IF dwo.Name = "prodstage" THEN
	int i,rowcnt
	rowcnt = dw_annual_anucost.RowCount()
	FOR i=1 to rowcnt 
		IF dw_annual_anucost.object.prodstage[i] = data AND i <> row THEN
			dw_annual_anucost.Object.prodstage.ValidationMsg='You may NOT duplicate the production stages. Please select another production stage.'
			return 1
		END IF
	NEXT
END IF
end event

event sqlpreview;call super::sqlpreview;//messagebox("sql",sqlsyntax)
end event

type dw_contract_init from u_pics_dw within w_sheet_annual_contract_init
event ue_enterkey pbm_dwnprocessenter
event pfc_hinttext pbm_mousemove
integer x = 32
integer y = 20
integer width = 2843
integer height = 1104
integer taborder = 10
string dataobject = "d_contract_init"
boolean vscrollbar = false
boolean livescroll = false
end type

event ue_enterkey;call super::ue_enterkey;IF (dw_contract_init.GetColumn() <> 1 ) THEN
	Send(Handle(this),256,9,Long(0,0))
	return(1)
END IF
end event

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

event itemchanged;call super::itemchanged;// If the object is the producer, get the producer information 
// from the database.
IF dwo.name = "ancntr_prdr" THEN
	string Lprdr,Lname,Laddr,Lcity,Lstate,Lzip
	int rtn
	
	  Lprdr = GetText()
	  
	  SELECT producer.prdr_name,   
         	producer.prdr_addr,   
         	producer.prdr_city,   
         	producer.prdr_state,   
         	producer.prdr_zip  
    	INTO 	:Lname,   
         	:Laddr,   
         	:Lcity,   
         	:Lstate,   
         	:Lzip  
    FROM producer  
    WHERE producer.prdr = :Lprdr
	 USING SqlServerTrans;
	 
	 IF SQLServerTrans.SQLCode = 0 THEN
		dw_contract_init.object.producer_prdr_name[1] = Lname   
		dw_contract_init.object.producer_prdr_addr[1] = Laddr
		dw_contract_init.object.producer_prdr_city[1] = Lcity  
		dw_contract_init.object.producer_prdr_state[1] = Lstate
		dw_contract_init.object.producer_prdr_zip[1] = Lzip 
		// POPULATE cntr_addr, cntr_city, cntr_state, cntr_zip of ancntr table
		dw_contract_init.object.cntr_addr[1] = Laddr
		dw_contract_init.object.cntr_city[1] = Lcity  
		dw_contract_init.object.cntr_state[1] = Lstate
		dw_contract_init.object.cntr_zip[1] = Lzip  
	END IF
ELSEIF dwo.Name = "ancntr_cntrfy" THEN
	IF len(data) < 4 THEN
		RETURN 1
	END IF
ELSEIF dwo.Name = "ancntr_cntrtype" THEN
		String Lcntrmed
		Lcntrmed  = dw_contract_init.object.ancntr_cntrmed[1]
		IF (Lcntrmed <> "" AND IsNull(Lcntrmed)=FALSE) THEN
			IF wf_get_prodstages(data,Lcntrmed) = FALSE THEN
				dw_contract_init.Object.ancntr_cntrtype.ValidationMsg='Production information does not exist for this contract medium and contract type.'
			 	RETURN 1
			END IF
		END IF
ELSEIF dwo.Name = "ancntr_cntrmed" THEN
		String Lcntrtype
		Lcntrtype  = dw_contract_init.object.ancntr_cntrtype[1]
		IF (Lcntrtype <> "" AND IsNull(Lcntrtype)=FALSE) THEN
			IF wf_get_prodstages(Lcntrtype,data) = FALSE THEN
				dw_contract_init.Object.ancntr_cntrmed.ValidationMsg='Production information does not exist for this contract medium and contract type.'
			 	RETURN 1
			END IF
		END IF
END IF

end event

event rbuttondown;//
end event

event rbuttonup;//
end event

event constructor;call super::constructor;this.of_SetPrintPreview(TRUE)
// set the transaction objects.
this.of_SetTransObject(SQLServerTrans)

end event

event getfocus;call super::getfocus;m_pics_main.m_edit.m_addrow.Enabled =	FALSE
m_pics_main.m_edit.m_deleterow.Enabled =	FALSE
end event

event retrieveend;call super::retrieveend;//IF IsNull(this.object.foreign_lang[rowcount]) THEN
//	this.object.foreign_lang[rowcount]='N'
//END IF 
end event

event sqlpreview;call super::sqlpreview;//messagebox("sql",sqlsyntax)
end event

event pfc_preupdate;call super::pfc_preupdate;//////////////////////////////////////////////////////////////////////////////////////////////////////////
//
//	Event: pfc_preupdate
//
//	Description:
//	Set  audit column default values before updates
//
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//	
//	Revision History
//
//	Developed by 	Date 				Version						Tracking#
//									
// Murali K.			03/06/2008      005 PICS Modifications	 
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//

long ll_rc
dwItemStatus l_status

ll_rc = This.Rowcount()
IF ll_rc > 0 THEN
   l_status = This.GetItemStatus(ll_rc,0, Primary!)
   IF l_status =new! or  l_status = newmodified! THEN
		This.object.ancntr_created_by[ll_rc] = gnv_app.of_getuserid()
		This.object.ancntr_created_date[ll_rc] =today()
		This.object.ancntr_modified_by[ll_rc] = gnv_app.of_getuserid()
		This.object.ancntr_modified_date[ll_rc] =today()
	ELSEIF l_status = datamodified! THEN
		This.object.ancntr_modified_by[ll_rc] = gnv_app.of_getuserid()
		This.object.ancntr_modified_date[ll_rc] =today()
	END IF
END IF
RETURN 1
end event

type cb_prdr from commandbutton within w_sheet_annual_contract_init
event mousemove pbm_mousemove
string tag = "Add or Update existing porducers."
integer x = 32
integer y = 1560
integer width = 599
integer height = 104
boolean bringtotop = true
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "&Add/Update Producers..."
end type

event mousemove;SetMicroHelp(w_pics_main,this.tag)
end event

event clicked;open(w_add_producer)
end event


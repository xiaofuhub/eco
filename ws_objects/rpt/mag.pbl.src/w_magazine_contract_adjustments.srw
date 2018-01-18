$PBExportHeader$w_magazine_contract_adjustments.srw
forward
global type w_magazine_contract_adjustments from w_sheet
end type
type dw_magazine_contract_adjustments_mag from u_dw within w_magazine_contract_adjustments
end type
type dw_magazine_contract_adjustments_magadj from u_dw within w_magazine_contract_adjustments
end type
type cb_update from u_cb within w_magazine_contract_adjustments
end type
type cb_clear from u_cb within w_magazine_contract_adjustments
end type
type cb_exit from u_cb within w_magazine_contract_adjustments
end type
end forward

global type w_magazine_contract_adjustments from w_sheet
integer width = 2693
integer height = 1684
string title = "Magazine Contract Adjustments"
dw_magazine_contract_adjustments_mag dw_magazine_contract_adjustments_mag
dw_magazine_contract_adjustments_magadj dw_magazine_contract_adjustments_magadj
cb_update cb_update
cb_clear cb_clear
cb_exit cb_exit
end type
global w_magazine_contract_adjustments w_magazine_contract_adjustments

forward prototypes
public function integer wf_check_magcode (string ls_magcode)
public function integer wf_check_if_data_exists (integer li_fy, string ls_contract, string ls_magcode)
end prototypes

public function integer wf_check_magcode (string ls_magcode);String ls_title


SELECT title
INTO   :ls_title
FROM   magttl
WHERE  magttl.magcd = :ls_magcode
USING  SqlServerTrans;

RETURN SqlServerTrans.SqlCode;
end function

public function integer wf_check_if_data_exists (integer li_fy, string ls_contract, string ls_magcode);  Integer li_num_rows
  
 //This function checks to see if data exists for the fiscal year, contract and
 // magazine code.
  
  SELECT count(*)
  INTO	:li_num_rows
    FROM mag,   
         magcntr,   
         magttl  
   WHERE ( mag.fy = magcntr.fy ) and  
         ( mag.magcd = magttl.magcd ) and  
         ( ( mag.fy = :li_fy ) AND  
         ( mag.cntr = :ls_contract ) AND  
         ( mag.magcd = :ls_magcode ) AND  
         ( mag.fy = magcntr.fy ) AND  
         ( mag.cntr = magcntr.cntr ) AND  
         ( mag.magcd = magttl.magcd ) )  
	USING SqlServerTrans;
	
	
	
	RETURN li_num_rows
	

end function

on w_magazine_contract_adjustments.create
int iCurrent
call super::create
this.dw_magazine_contract_adjustments_mag=create dw_magazine_contract_adjustments_mag
this.dw_magazine_contract_adjustments_magadj=create dw_magazine_contract_adjustments_magadj
this.cb_update=create cb_update
this.cb_clear=create cb_clear
this.cb_exit=create cb_exit
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_magazine_contract_adjustments_mag
this.Control[iCurrent+2]=this.dw_magazine_contract_adjustments_magadj
this.Control[iCurrent+3]=this.cb_update
this.Control[iCurrent+4]=this.cb_clear
this.Control[iCurrent+5]=this.cb_exit
end on

on w_magazine_contract_adjustments.destroy
call super::destroy
destroy(this.dw_magazine_contract_adjustments_mag)
destroy(this.dw_magazine_contract_adjustments_magadj)
destroy(this.cb_update)
destroy(this.cb_clear)
destroy(this.cb_exit)
end on

event open;call super::open;cb_clear.Enabled = FALSE
cb_update.Enabled = FALSE
dw_magazine_contract_adjustments_magadj.Enabled = FALSE
w_magazine_contract_adjustments.SetMicroHelp("")

//disable the addrow and deleterow menu options
m_pics_main.m_edit.m_deleterow.Enabled = FALSE
m_pics_main.m_edit.m_addrow.Enabled = FALSE


//Open window in maximized 
this.windowstate = maximized!
end event

event pfc_preopen;call super::pfc_preopen;this.of_SetPreference(TRUE)
this.inv_preference.of_SetToolBars(TRUE)
this.inv_preference.of_SetWindow(TRUE)
this.of_SetResize(TRUE)
this.inv_resize.of_SetOrigSize(this.workspacewidth(),this.workspaceheight())

inv_resize.of_Register(cb_clear, "scale")
inv_resize.of_Register(cb_exit, "scale")
inv_resize.of_Register(cb_update, "scale")
inv_resize.of_Register(dw_magazine_contract_adjustments_mag, "scale")
inv_resize.of_Register(dw_magazine_contract_adjustments_magadj, "scale")

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
Integer	li_rc, li_rtn_code
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
			li_rtn_code = cb_update.TriggerEvent(clicked!)
			IF li_rtn_code = 1 THEN
			   RETURN 0
			END IF
			// YES - Update
			// If the update fails, prevent the window from closing
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

event mousemove;call super::mousemove;w_pics_main.setmicrohelp('Ready')
end event

type dw_magazine_contract_adjustments_mag from u_dw within w_magazine_contract_adjustments
event ue_enter_to_tab pbm_dwnprocessenter
event ue_hint_text pbm_mousemove
integer x = 27
integer y = 56
integer width = 2551
integer height = 756
integer taborder = 10
string dataobject = "d_magazine_contract_adjustments_mag"
boolean vscrollbar = false
end type

event ue_enter_to_tab;call super::ue_enter_to_tab;Send(Handle(this), 256,9,Long(0,0))
RETURN 1

end event

event ue_hint_text;call super::ue_hint_text;string ls_object, ls_column, ls_column_tag
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

event constructor;call super::constructor;
Settransobject(SqlServerTrans)

//disable the right mouse button
ib_rmbmenu = FALSE

dw_magazine_contract_adjustments_mag.Event pfc_addrow()
dw_magazine_contract_adjustments_mag.SetFocus()



end event

event itemchanged;call super::itemchanged;String  ls_magcode, ls_contract
Integer li_rtn_code, li_retrieve, li_fy, li_check


IF DWO.Name = "mag_fy" THEN
 cb_clear.Enabled = TRUE
END IF//IF DWO.Name = "mag_fy" THEN


IF DWO.Name = "mag_magcd" THEN
  ls_magcode = Data
  //Check if Magazine code exists
  li_rtn_code = wf_check_magcode(ls_magcode)
    //IF Magazine Code does not exist output message.
    IF li_rtn_code = 100 THEN
      dw_magazine_contract_adjustments_mag.Object.mag_magcd.validationmsg &
		                = "Magazine Code Not Found"   
      
	 RETURN 1  
    ELSEIF li_rtn_code = 0 THEN
		//Put values into local variables.
		li_fy =         dw_magazine_contract_adjustments_mag.GetItemNumber(row,"mag_fy")
		ls_contract =   dw_magazine_contract_adjustments_mag.GetItemString(row,"mag_cntr")
	   				
		
		//Check if data exists for the fiscal year, contract and the magazine code
		li_check =  wf_check_if_data_exists(li_fy,ls_contract,ls_magcode)
		 
		//If data exists retrieve into data window and set focus to second data window
		IF li_check = 1 THEN
		  
		   dw_magazine_contract_adjustments_mag.Retrieve(li_fy,ls_contract,ls_magcode)
			dw_magazine_contract_adjustments_mag.Resetupdate()
			dw_magazine_contract_adjustments_mag.Enabled = FALSE
									
			dw_magazine_contract_adjustments_magadj.Enabled = TRUE
			li_fy        = dw_magazine_contract_adjustments_mag.GetItemNumber(1,"mag_fy")
			ls_contract  = dw_magazine_contract_adjustments_mag.GetItemString(1,"mag_cntr")
			ls_magcode   = dw_magazine_contract_adjustments_mag.GetItemString(1,"mag_magcd")
		
		
			 dw_magazine_contract_adjustments_magadj.SetItem(1,"fy",li_fy)
			 dw_magazine_contract_adjustments_magadj.SetItem(1,"cntr",ls_contract)
			 dw_magazine_contract_adjustments_magadj.SetItem(1,"magcd",ls_magcode)
		   dw_magazine_contract_adjustments_magadj.SetFocus()
      //If data does not exists then output message and do not change focus.   
	   ELSEIF li_check = 0 THEN
	         dw_magazine_contract_adjustments_mag.Object.mag_magcd.validationmsg &
		                = "No Data Found. Enter New Magazine Code ~n"+ &
							   "Or Click On Clear To Enter New Query"
			 
			 dw_magazine_contract_adjustments_mag.SetItem(row,"mag_magcd",'')
			 dw_magazine_contract_adjustments_mag.SetText('')
			 
			 RETURN 1
      END IF // IF li_check = 1 THEN
		
	END IF // IF li_rtn_code = 100 THEN
END IF // IF DWO.Name = "mag_magcd" THEN

end event

type dw_magazine_contract_adjustments_magadj from u_dw within w_magazine_contract_adjustments
event ue_enter_to_tab pbm_dwnprocessenter
event ue_hint_text pbm_mousemove
integer x = 23
integer y = 892
integer width = 2418
integer height = 408
integer taborder = 20
string dataobject = "d_magazine_contract_adjustments_magadj"
boolean vscrollbar = false
boolean livescroll = false
end type

event ue_enter_to_tab;call super::ue_enter_to_tab;Send(Handle(this), 256,9, Long(0,0))
RETURN 1
end event

event ue_hint_text;call super::ue_hint_text;string ls_object, ls_column, ls_column_tag
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

event constructor;call super::constructor;String ls_date

SetTransObject(SqlServerTrans)

dw_magazine_contract_adjustments_magadj.Event pfc_addrow()


dw_magazine_contract_adjustments_magadj.SetItem(1,"adjdt",Today())

//disable the right mouse button
ib_rmbmenu = FALSE


end event

event updateend;call super::updateend;long ll_total

ll_total = rowsinserted + rowsupdated

IF ll_total > 0 THEN
	w_pics_main.Event pfc_microhelp('Row(s) Updated Successfully')
END IF
end event

event itemchanged;call super::itemchanged;Dec ld_cntcso
String ls_string


//Check if contact cost adjustment is zero .. if it is do not allow to change focus.
IF DWO.Name = "cntcsadj" THEN
  ld_cntcso = Dec(Data)
   
  IF ld_cntcso = 0 THEN
 	dw_magazine_contract_adjustments_magadj.Object.cntcsadj.Validationmsg = "Contract Cost Adjustment Cannot Be Zero"
   Return 1
  END IF//IF lr_cntcso = 0 THEN

END IF

//
IF DWO.Name = "adjrsn" THEN
	ls_string = DATA
	IF Trim(ls_string) = "" THEN
	  dw_magazine_contract_adjustments_magadj.Object.adjrsn.Validationmsg = "Adjustment Reason Needs To Be Input"
	  Return 1
   END IF
END IF
	


end event

event editchanged;call super::editchanged;
//Enable the update button if user clicks on update.
IF DWO.Name = "adjrsn" THEN
	IF TRIM(data) <> "" THEN
	 cb_update.Enabled = TRUE
   END IF
END IF
end event

type cb_update from u_cb within w_magazine_contract_adjustments
event ue_hinttext pbm_mousemove
string tag = "Update the Database"
integer x = 951
integer y = 1444
integer taborder = 0
integer textsize = -10
string text = "&Update"
end type

event ue_hinttext;call super::ue_hinttext;w_pics_main.setmicrohelp(this.tag)
end event

event clicked;call super::clicked;Real lr_cntcsadjc, lr_cntcsadj, lr_cntcsc, lr_cntcso, lr_estmail, lr_temp
Integer li_fy
String  ls_contract, ls_magcode, ls_reason

dw_magazine_contract_adjustments_mag.AcceptText()
dw_magazine_contract_adjustments_magadj.AcceptText()


//Check to see if Reason field is input if not return
   ls_reason = dw_magazine_contract_adjustments_magadj.Object.adjrsn[1]

    IF Trim(ls_reason) = "" OR IsNull(ls_reason) THEN
		MessageBox('Error','Adjustment Reason Needs To Be Input')
		dw_magazine_contract_adjustments_magadj.SetFocus()
		RETURN 0
	END IF




// Get fiscal year, contract and magazine code from dw_magazine_contract_adjustments_mag
// and place them in dw_magazine_contract_adjustments_magadj


	li_fy        = dw_magazine_contract_adjustments_mag.GetItemNumber(1,"mag_fy")
	ls_contract  = dw_magazine_contract_adjustments_mag.GetItemString(1,"mag_cntr")
	ls_magcode   = dw_magazine_contract_adjustments_mag.GetItemString(1,"mag_magcd")


	 dw_magazine_contract_adjustments_magadj.SetItem(1,"fy",li_fy)
	 dw_magazine_contract_adjustments_magadj.SetItem(1,"cntr",ls_contract)
	 dw_magazine_contract_adjustments_magadj.SetItem(1,"magcd",ls_magcode)

// Get contract dollars and do the necessary calculations
   lr_estmail = dw_magazine_contract_adjustments_mag.GetItemNumber(1, "mag_estmail")
	 IF IsNull(lr_estmail) THEN lr_estmail = 0
	lr_cntcsadjc = dw_magazine_contract_adjustments_mag.GetItemNumber(1, "mag_cntcsadjc")
	 IF IsNull(lr_cntcsadjc) THEN lr_cntcsadjc = 0
	lr_cntcsc    = dw_magazine_contract_adjustments_mag.GetItemNumber(1, "mag_cntcsc") 
	 IF IsNull(lr_cntcsc) THEN lr_cntcsc = 0
	lr_cntcso    = dw_magazine_contract_adjustments_mag.GetItemNumber(1,"mag_cntcso")
	 IF IsNull(lr_cntcso) THEN lr_cntcso = 0
	lr_cntcsadj  = dw_magazine_contract_adjustments_magadj.GetItemNumber(1, "cntcsadj")
    IF IsNull(lr_cntcsadj) THEN lr_cntcsadj = 0


// Set the calculated values in the mag data window - dw_magazine_contract_adjustments_mag
  dw_magazine_contract_adjustments_mag.SetItem(1,"mag_cntcsadjc",lr_cntcsadjc+lr_cntcsadj)
  lr_temp = lr_cntcso+lr_cntcsadjc+lr_cntcsadj+lr_estmail 
  dw_magazine_contract_adjustments_mag.SetItem(1,"mag_cntcsc",lr_cntcso+lr_cntcsadjc+lr_cntcsadj+lr_estmail)
  
  


dw_magazine_contract_adjustments_mag.AcceptText()
dw_magazine_contract_adjustments_magadj.AcceptText()

//Update the mag and magadj table.
 IF dw_magazine_contract_adjustments_mag.Update() = 1 THEN
	COMMIT USING SqlServerTrans;
	 IF  dw_magazine_contract_adjustments_magadj.Update() = 1 THEN
		COMMIT USING SqlServerTrans;
		cb_clear.TriggerEvent(clicked!)
		RETURN 1
	 ELSE
		ROLLBACK USING SqlServerTrans;
		MessageBox('Error','Could Not Update magadj table .. Contact Your System Administrator')
	   RETURN 0
    END IF
ELSE
	ROLLBACK USING SqlServerTrans;
     MessageBox('Error','Could Not Update magadj table .. Contact Your System Administrator')	
     RETURN 0
END IF



end event

type cb_clear from u_cb within w_magazine_contract_adjustments
event ue_hinttext pbm_mousemove
string tag = "Clears the screen for Input"
integer x = 1381
integer y = 1444
integer taborder = 0
integer textsize = -10
string text = "&Clear"
end type

event ue_hinttext;call super::ue_hinttext;w_pics_main.setmicrohelp(this.tag)
end event

event clicked;call super::clicked;dw_magazine_contract_adjustments_magadj.Reset()
dw_magazine_contract_adjustments_magadj.Event pfc_addrow()
dw_magazine_contract_adjustments_magadj.SetItem(1,"adjdt",today())
dw_magazine_contract_adjustments_magadj.Enabled = FALSE

cb_clear.Enabled =  FALSE
cb_update.Enabled = FALSE


dw_magazine_contract_adjustments_mag.Reset()
dw_magazine_contract_adjustments_mag.Event pfc_addrow()
dw_magazine_contract_adjustments_mag.Enabled = TRUE
dw_magazine_contract_adjustments_mag.SetFocus()

end event

type cb_exit from u_cb within w_magazine_contract_adjustments
event ue_hinttext pbm_mousemove
string tag = "Exits the screen"
integer x = 1847
integer y = 1444
integer taborder = 0
boolean bringtotop = true
integer textsize = -10
string text = "E&xit"
end type

event ue_hinttext;call super::ue_hinttext;w_pics_main.setmicrohelp(this.tag)
end event

event clicked;call super::clicked;Dec ld_cntcsadj
String ls_reason

dw_magazine_contract_adjustments_magadj.AcceptText()
ld_cntcsadj = dw_magazine_contract_adjustments_magadj.GetItemNumber(1,"cntcsadj")
ls_reason = dw_magazine_contract_adjustments_magadj.Object.adjrsn[1]


IF IsNull(ld_cntcsadj) OR ld_cntcsadj = 0 THEN
	dw_magazine_contract_adjustments_mag.Reset()
	dw_magazine_contract_adjustments_magadj.Reset()
	close(w_magazine_contract_adjustments)
	m_pics_main.m_menu.popmenu(300,0)
ELSE
   Parent.Event pfc_close()
	//Pop up the main menu if the window is closed
	IF IsValid(w_magazine_contract_adjustments) = FALSE THEN
		m_pics_main.m_menu.popmenu(300,0)
	END IF
	
		
END IF//IF IsNull(ld_cntcsadj) OR ld_cntcsadj = 0 THEN
end event


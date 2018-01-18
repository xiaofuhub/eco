$PBExportHeader$w_magazine_issue_maintenance.srw
forward
global type w_magazine_issue_maintenance from w_sheet
end type
type cb_ext from commandbutton within w_magazine_issue_maintenance
end type
type st_1 from u_st within w_magazine_issue_maintenance
end type
type st_2 from u_st within w_magazine_issue_maintenance
end type
type st_3 from u_st within w_magazine_issue_maintenance
end type
type sle_producer from u_sle within w_magazine_issue_maintenance
end type
type sle_format from u_sle within w_magazine_issue_maintenance
end type
type sle_status from u_sle within w_magazine_issue_maintenance
end type
type cb_update from u_cb within w_magazine_issue_maintenance
end type
type cb_clear from u_cb within w_magazine_issue_maintenance
end type
type cb_exit from u_cb within w_magazine_issue_maintenance
end type
type em_issue_date from u_em within w_magazine_issue_maintenance
end type
type st_4 from statictext within w_magazine_issue_maintenance
end type
type dw_due_date_extension from u_dw within w_magazine_issue_maintenance
end type
type dw_magazine_issue_maintenance_magcd from u_dw within w_magazine_issue_maintenance
end type
type dw_magazine_issue_maintenance_rc from u_dw within w_magazine_issue_maintenance
end type
type dw_magazine_issue_maintenance from u_dw within w_magazine_issue_maintenance
end type
end forward

global type w_magazine_issue_maintenance from w_sheet
integer width = 2633
integer height = 1800
string title = "Magazine Issue Maintenance"
cb_ext cb_ext
st_1 st_1
st_2 st_2
st_3 st_3
sle_producer sle_producer
sle_format sle_format
sle_status sle_status
cb_update cb_update
cb_clear cb_clear
cb_exit cb_exit
em_issue_date em_issue_date
st_4 st_4
dw_due_date_extension dw_due_date_extension
dw_magazine_issue_maintenance_magcd dw_magazine_issue_maintenance_magcd
dw_magazine_issue_maintenance_rc dw_magazine_issue_maintenance_rc
dw_magazine_issue_maintenance dw_magazine_issue_maintenance
end type
global w_magazine_issue_maintenance w_magazine_issue_maintenance

type variables
Integer li_fy
Integer li_ext

end variables

forward prototypes
public function integer wf_get_title (string ls_magcode, ref string ls_title)
public subroutine wf_disable_objects ()
public subroutine wf_set_zero ()
public function boolean wf_is_date_empty (string ls_date)
public function integer wf_check_input ()
public function integer wf_check_issuedate (string ls_magcode, date ld_issuedate)
public function integer wf_get_ccd (string ls_magcode, string ls_contract, ref integer li_ccd)
public function integer wf_get_estshipdate (date ld_startdt, string ls_producer, ref date ld_estshipdt)
public function integer wf_get_contract_information (string ls_magcode, ref string ls_producer, ref string ls_format, ref string ls_status, ref string ls_contract, ref integer li_ccd)
end prototypes

public function integer wf_get_title (string ls_magcode, ref string ls_title);

//Gets the magazine title

  SELECT magttl.title  
  INTO   :ls_title  
  FROM   magttl  
  WHERE  magttl.magcd = :ls_magcode
  USING  SqlServerTrans;
	
Return SqlServerTrans.Sqlcode
end function

public subroutine wf_disable_objects ();cb_update.Enabled = TRUE
dw_magazine_issue_maintenance.Object.magiss_magcd.Protect = 0
end subroutine

public subroutine wf_set_zero ();//This function will set zero to all the values that are null.
//It is done for the purpose of calculation in the invoice tracking 
//screen.



IF dw_magazine_issue_maintenance_rc.Visible = TRUE THEN
	IF IsNull(dw_magazine_issue_maintenance_rc.Object.szs[1]) THEN
		dw_magazine_issue_maintenance_rc.Object.szs[1] = 0
	END IF
	IF IsNull(dw_magazine_issue_maintenance_rc.Object.sz[1]) THEN
		dw_magazine_issue_maintenance_rc.Object.sz[1] = 0
	END IF
	IF IsNull(dw_magazine_issue_maintenance_rc.Object.szl[1]) THEN
		dw_magazine_issue_maintenance_rc.Object.szl[1] = 0
	END IF
	
ELSEIF dw_magazine_issue_maintenance.Visible = TRUE THEN
	IF IsNull(dw_magazine_issue_maintenance.Object.magiss_sz[1]) THEN
		dw_magazine_issue_maintenance.Object.magiss_sz[1] = 0
	END IF
	
END IF//IF dw_magazine_issue_maintenance_rc.Visible = TRUE THEN


end subroutine

public function boolean wf_is_date_empty (string ls_date);
ls_date = left(ls_date,4)

IF TRIM(ls_date) = "" THEN
	RETURN TRUE
ELSE
	RETURN FALSE
END IF
end function

public function integer wf_check_input ();//This function checks to see if all fields have been input. If there
//are any errors in the sequence of inputs then it will return 2 to 
//the calling program else it will return 1. It does this for
//both the RC datawindow and the non RC datawindow.

Datetime ld_startdt, ld_shipdt
Integer li_subs, li_mins, li_row_count, li_ext_days
String ls_date, ls_format, ls_reason
Real   lr_sz, lr_szs, lr_szl

//If the non RC datawindow(dw_issue_maintenance) is visible then
//process.
IF dw_magazine_issue_maintenance.Visible = TRUE THEN
	dw_magazine_issue_maintenance.AcceptText()
   dw_due_date_extension.AcceptText()
  
						
				ld_startdt = dw_magazine_issue_maintenance.GetItemDatetime(1,"magiss_startdt")
				ld_shipdt  = dw_magazine_issue_maintenance.GetItemDatetime(1,"magiss_shipdt")
				li_subs    = dw_magazine_issue_maintenance.GetItemNumber(1,"magiss_subs")
				lr_sz		  = dw_magazine_issue_maintenance.GetItemNumber(1,"magiss_sz")
				li_mins	  = dw_magazine_issue_maintenance.GetItemNumber(1,"magiss_mins")
				ls_format  = sle_format.text
				
				
				//If shipdate has been input then start date required too.
				IF Not(IsNull(ld_shipdt)) AND IsNull(ld_startdt) = TRUE THEN
					w_pics_main.SetMicrohelp("")
					MessageBox('Error', 'Start Date Required For Shipment')
					dw_magazine_issue_maintenance.SetFocus()
					dw_magazine_issue_maintenance.SetColumn("magiss_startdt") 
					RETURN 2
				END IF//IF Not(IsNull(ld_shipdt)) AND IsNull(ld_startdt) = TRUE THEN
				
				
				//Checks to see if subs field has entry if shipdate has an entry and
				//if shipdate has an entry if subs field has an input.
				IF (IsNull(li_subs) OR li_subs = 0) AND NOT(IsNull(ld_shipdt)) THEN
					w_pics_main.SetMicrohelp("")
					MessageBox('Error','Subs Required For Shipment')
					dw_magazine_issue_maintenance.SetFocus()
					dw_magazine_issue_maintenance.SetColumn("magiss_subs")
					RETURN 2
				ELSEIF (NOT(IsNull(li_subs)) AND li_subs <> 0 ) AND IsNull(ld_shipdt) THEN
					w_pics_main.SetMicrohelp("")
					MessageBox('Error','Ship Date Required For Subs Entry')
					dw_magazine_issue_maintenance.SetFocus()
					dw_magazine_issue_maintenance.SetColumn("magiss_shipdt")
					RETURN 2
				END IF//IF (IsNull(li_subs) OR li_subs = 0) AND NOT(IsNull(ld_shipdt)) THEN
				
				
				//Checks to see if sz field has entry if shipdate has an entry and
				//if shipdate has an entry if sz field has an input.
				IF (IsNull(lr_sz) OR lr_sz = 0) AND NOT(IsNull(ld_shipdt)) THEN
					w_pics_main.SetMicrohelp("")
					MessageBox('Error','Size Required For Shipment')
					dw_magazine_issue_maintenance.SetFocus()
					dw_magazine_issue_maintenance.SetColumn("magiss_sz")
					RETURN 2
				ELSEIF (NOT(IsNull(lr_sz)) AND lr_sz <> 0 ) AND IsNull(ld_shipdt) THEN
					w_pics_main.SetMicrohelp("")
					MessageBox('Error','Ship Date Required For Size Entry')
					dw_magazine_issue_maintenance.SetFocus()
					dw_magazine_issue_maintenance.SetColumn("magiss_shipdt")
					RETURN 2
				END IF//IF (IsNull(lr_sz) OR lr_sz = 0) AND NOT(IsNull(ld_shipdt)) THEN
				
				//IF format is FD get the minutes. IF shipdate is null and mins field has
				// input or if mins is null and shipdate has input then prompt user.
				IF TRIM(ls_format) = 'FD' THEN
					
					IF ((IsNull(li_mins)) OR li_mins = 0) AND NOT(IsNull(ld_shipdt)) THEN
						w_pics_main.SetMicrohelp("")
						MessageBox('Error','Minutes Required For Shipment')
						dw_magazine_issue_maintenance.SetFocus()
						dw_magazine_issue_maintenance.SetColumn("magiss_mins")
						RETURN 2
					ELSEIF (NOT(IsNull(li_mins)) AND li_mins <> 0 ) AND IsNull(ld_shipdt) THEN
						w_pics_main.SetMicrohelp("")
						MessageBox('Error','Ship Date Required For Mins Entry')
						dw_magazine_issue_maintenance.SetFocus()
						dw_magazine_issue_maintenance.SetColumn("magiss_shipdt")
					   RETURN 2
				   END IF//IF (IsNull(li_mins)) AND NOT(IsNull(ld_shipdt)) THEN
									
				END IF//IF TRIM(ls_format) = 'FD' THEN
				
				//If extension days have been input then reason needs to 
				//be input too.
				IF dw_due_date_extension.RowCount() > 0 THEN
					li_ext_days = dw_due_date_extension.Object.ext[1]
				   IF Not(IsNull(li_ext_days)) AND li_ext_days <> 0 THEN
						ls_reason = dw_due_date_extension.Object.extrsn[1]
				      IF IsNull(ls_reason) OR TRIM(ls_reason) = '' THEN
				         MessageBox('Error','Extension Reason Needs to Be Input')
				         dw_due_date_extension.SetFocus()
							dw_due_date_extension.SetColumn("extrsn") 
							RETURN 2
						END IF// IF IsNull(ls_reason) OR TRIM(ls_reason) = '' THEN
				   END IF//IF Not(IsNull(li_ext_days)) OR li_ext_days <> 0 THEN
				END IF//IF dw_due_date_extension.RowCount() > 0 THEN
				
				
				RETURN 1
				
END IF//IF dw_magazine_issue_maintenance.Visible = TRUE THEN

//If the RC datawindow is up then process the following
IF dw_magazine_issue_maintenance_rc.visible = TRUE THEN
		dw_magazine_issue_maintenance_rc.AcceptText()
	   dw_due_date_extension.AcceptText()
		 
			
		ld_startdt = dw_magazine_issue_maintenance_rc.GetItemDatetime(1,"startdt")
		ld_shipdt  = dw_magazine_issue_maintenance_rc.GetItemDatetime(1,"shipdt")
		li_subs    = dw_magazine_issue_maintenance_rc.GetItemNumber(1,"subs")
		lr_sz		  = dw_magazine_issue_maintenance_rc.GetItemNumber(1,"sz")
		lr_szs	  = dw_magazine_issue_maintenance_rc.GetItemNumber(1,"szs")
		lr_szl     = dw_magazine_issue_maintenance_rc.GetItemNumber(1,"szl")
		li_mins    = dw_magazine_issue_maintenance_rc.GetItemNumber(1,"mins")
		ls_format  = sle_format.text
				
				
	
		//If Shipdate has been input then the following have to be input
		//start date, subs, size, mins
			IF NOT(IsNull(ld_shipdt)) THEN
				IF IsNull(ld_startdt) = TRUE THEN
					MessageBox('Error','Start Date Required For Shipment')
	            dw_magazine_issue_maintenance_rc.SetFocus()
	            dw_magazine_issue_maintenance_rc.SetColumn('startdt')
	            RETURN 2
				ELSEIF (li_subs = 0  OR IsNull(li_subs)) THEN
					MessageBox('Error','Subscribers Required For Shipment')
	            dw_magazine_issue_maintenance_rc.SetFocus()
	            dw_magazine_issue_maintenance_rc.SetColumn('subs')
	            RETURN 2
				ELSEIF (IsNull(lr_szs) OR lr_szs = 0) AND (IsNull(lr_sz) OR &
				     lr_sz = 0) AND (IsNull(lr_szl) OR lr_szl = 0) THEN
				   MessageBox('Error','Size Required For Shipment')
				   dw_magazine_issue_maintenance_rc.SetFocus()
				   dw_magazine_issue_maintenance_rc.SetColumn('szs')
				   RETURN 2
				ELSEIF (IsNull(li_mins) OR li_mins = 0 ) THEN
					MessageBox('Error','Minutes Required For Shipment')
	            dw_magazine_issue_maintenance_rc.SetFocus()
	            dw_magazine_issue_maintenance_rc.SetColumn('mins')
					RETURN 2
				END IF//	IF IsNull(ld_startdt) THEN
			
			ELSE  
			 //If subs, size have input then ship date needs an input.
				IF (NOT(IsNull(li_subs)) AND (li_subs <> 0 )) THEN
					MessageBox('Error','Ship Date Required For Subs Entry')
				   dw_magazine_issue_maintenance_rc.SetFocus()
				   dw_magazine_issue_maintenance_rc.SetColumn("shipdt")
					RETURN 2
				ELSEIF (NOT(IsNull(lr_szs)) AND lr_szs <> 0) OR (NOT(IsNull(lr_sz)) AND &
				     lr_sz <> 0) OR (NOT(IsNull(lr_szl)) AND lr_szl <> 0) THEN
				   MessageBox('Error','Ship Date Required For Size Entry')
				   dw_magazine_issue_maintenance_rc.SetFocus()
				   dw_magazine_issue_maintenance_rc.SetColumn("shipdt")
					RETURN 2
				ELSEIF (Not(IsNull(li_mins)) AND (li_mins <> 0 )) THEN
					MessageBox('Error','Ship Date Required For Minutes Entry')
				   dw_magazine_issue_maintenance_rc.SetFocus()
				   dw_magazine_issue_maintenance_rc.SetColumn("shipdt")
					RETURN 2
				END IF//IF (NOT(IsNull(li_subs)) AND (li_subs <> 0 ) THEN     
			END IF//IF NOT(IsNull(ld_shipdt)) THEN
					
				//If extension days have been input then reason needs to 
				//be input too.
				IF dw_due_date_extension.RowCount() > 0 THEN
					li_ext_days = dw_due_date_extension.Object.ext[1]
				   IF Not(IsNull(li_ext_days)) AND li_ext_days <> 0 THEN
						ls_reason = dw_due_date_extension.Object.extrsn[1]
				      IF IsNull(ls_reason) OR TRIM(ls_reason) = '' THEN
				         MessageBox('Error','Extension Reason Needs to Be Input')
				         dw_due_date_extension.SetFocus()
							dw_due_date_extension.SetColumn("extrsn") 
							RETURN 2
						END IF// IF IsNull(ls_reason) OR TRIM(ls_reason) = '' THEN
				   END IF//IF Not(IsNull(li_ext_days)) OR li_ext_days <> 0 THEN
				END IF//IF dw_due_date_extension.RowCount() > 0 THEN	
					
					
					
					
					
         RETURN 1
END IF//IF dw_magazine_issue_maintenance.Visible = TRUE THEN

end function

public function integer wf_check_issuedate (string ls_magcode, date ld_issuedate);String ls_code

//checks if magazine code exists for that issue date

SELECT magiss.magcd
INTO   :ls_code
FROM	 magiss
WHERE  magiss.fy = :li_fy  AND
		 magiss.magcd = :ls_magcode AND
		 magiss.issdt = :ld_issuedate
USING SqlServerTrans;





RETURN SqlServerTrans.SqlCode
end function

public function integer wf_get_ccd (string ls_magcode, string ls_contract, ref integer li_ccd);//This function gets the CCD (contract calendar work days)
li_ccd = 0

SELECT mag.ccd  
   INTO :li_ccd  
   FROM mag  
WHERE ( mag.fy = :li_fy ) AND  
      ( mag.magcd = :ls_magcode ) AND  
      ( mag.cntr = :ls_contract ) 
USING SqlServerTrans;

IF f_check_dberror(sqlservertrans,"MAG") and isnull(li_ccd)=false THEN
	RETURN li_ccd
ELSE
	RETURN 100
END IF
end function

public function integer wf_get_estshipdate (date ld_startdt, string ls_producer, ref date ld_estshipdt);Integer li_vac_days

//Returns the estimated ship date. 

SELECT sum(holdur)
INTO   :li_vac_days
FROM   prdrhol
WHERE  prdr = :ls_producer AND
		 holdt between :ld_startdt and :ld_estshipdt
USING  SqlServerTrans;


IF NOT(IsNull(li_vac_days)) THEN
   ld_estshipdt = RelativeDate(ld_estshipdt,li_vac_days)
END IF


//If the estimated ship date fall on a sunday then add one, if it
//falls on a saturday add two.
IF DayNumber(ld_estshipdt) = 1 THEN
   ld_estshipdt = RelativeDate(ld_estshipdt,1)
ELSEIF DayNumber(ld_estshipdt) = 7 THEN
	ld_estshipdt = RelativeDate(ld_estshipdt,2)
END IF
  

RETURN SqlServerTrans.SqlCode
end function

public function integer wf_get_contract_information (string ls_magcode, ref string ls_producer, ref string ls_format, ref string ls_status, ref string ls_contract, ref integer li_ccd);
//Gets the contract information for the magazine code for that fiscal year.
//Contract should be active.

SELECT magcntr.prdr,magcntr.format,mag.magst,mag.cntr,mag.ccd
INTO   :ls_producer,
		 :ls_format,
		 :ls_status,
		 :ls_contract,
		 :li_ccd
FROM	 mag,magcntr
WHERE  mag.fy = :li_fy  AND
		 mag.magcd = :ls_magcode AND
		 mag.magst = 'A' AND
		 mag.fy = magcntr.fy AND
		 mag.cntr = magcntr.cntr
USING SqlServerTrans;


Return SqlServerTrans.SqlCode
end function

on w_magazine_issue_maintenance.create
int iCurrent
call super::create
this.cb_ext=create cb_ext
this.st_1=create st_1
this.st_2=create st_2
this.st_3=create st_3
this.sle_producer=create sle_producer
this.sle_format=create sle_format
this.sle_status=create sle_status
this.cb_update=create cb_update
this.cb_clear=create cb_clear
this.cb_exit=create cb_exit
this.em_issue_date=create em_issue_date
this.st_4=create st_4
this.dw_due_date_extension=create dw_due_date_extension
this.dw_magazine_issue_maintenance_magcd=create dw_magazine_issue_maintenance_magcd
this.dw_magazine_issue_maintenance_rc=create dw_magazine_issue_maintenance_rc
this.dw_magazine_issue_maintenance=create dw_magazine_issue_maintenance
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.cb_ext
this.Control[iCurrent+2]=this.st_1
this.Control[iCurrent+3]=this.st_2
this.Control[iCurrent+4]=this.st_3
this.Control[iCurrent+5]=this.sle_producer
this.Control[iCurrent+6]=this.sle_format
this.Control[iCurrent+7]=this.sle_status
this.Control[iCurrent+8]=this.cb_update
this.Control[iCurrent+9]=this.cb_clear
this.Control[iCurrent+10]=this.cb_exit
this.Control[iCurrent+11]=this.em_issue_date
this.Control[iCurrent+12]=this.st_4
this.Control[iCurrent+13]=this.dw_due_date_extension
this.Control[iCurrent+14]=this.dw_magazine_issue_maintenance_magcd
this.Control[iCurrent+15]=this.dw_magazine_issue_maintenance_rc
this.Control[iCurrent+16]=this.dw_magazine_issue_maintenance
end on

on w_magazine_issue_maintenance.destroy
call super::destroy
destroy(this.cb_ext)
destroy(this.st_1)
destroy(this.st_2)
destroy(this.st_3)
destroy(this.sle_producer)
destroy(this.sle_format)
destroy(this.sle_status)
destroy(this.cb_update)
destroy(this.cb_clear)
destroy(this.cb_exit)
destroy(this.em_issue_date)
destroy(this.st_4)
destroy(this.dw_due_date_extension)
destroy(this.dw_magazine_issue_maintenance_magcd)
destroy(this.dw_magazine_issue_maintenance_rc)
destroy(this.dw_magazine_issue_maintenance)
end on

event open;call super::open;//Disable the buttons and disable the addrow, deleterow menu options
cb_update.Enabled = FALSE
cb_ext.Enabled = FALSE
m_pics_main.m_edit.m_deleterow.Enabled = FALSE
m_pics_main.m_edit.m_addrow.Enabled = FALSE

//Disable the datawindow and the edit mask
dw_magazine_issue_maintenance.Enabled = FALSE
dw_magazine_issue_maintenance_rc.Enabled = FALSE
dw_due_date_extension.Enabled = FALSE
em_issue_date.DisplayOnly = TRUE

//Open window in maximized state
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
inv_resize.of_Register(cb_ext, "scale")
inv_resize.of_Register(dw_magazine_issue_maintenance, "scale")
inv_resize.of_Register(dw_magazine_issue_maintenance_rc, "scale")
inv_resize.of_Register(dw_magazine_issue_maintenance_magcd, "scale")
inv_resize.of_Register(dw_due_date_extension, "scale")
inv_resize.of_Register(em_issue_date, "scale")
inv_resize.of_Register(sle_format, "scale")
inv_resize.of_Register(sle_producer, "scale")
inv_resize.of_Register(sle_status, "scale")
inv_resize.of_Register(st_1, "scale")
inv_resize.of_Register(st_2, "scale")
inv_resize.of_Register(st_3, "scale")
inv_resize.of_Register(st_4, "scale")

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
					dw_magazine_issue_maintenance.SetFocus()
	End If
	Choose Case li_msg
		Case 1
			   IF wf_check_input() <> 2 THEN
		   		li_rtn = cb_update.TriggerEvent(clicked!)
					IF li_rtn = 1 THEN
						RETURN 0
					END IF//IF li_rtn = 1 THEN
			  END IF//IF wf_check_input() = 2 THEN
							
		   			
						
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
			dw_magazine_issue_maintenance_magcd.SetFocus()
	End Choose
End If

// Prevent the window from closing
ib_closestatus = False
Return 1
end event

event mousemove;call super::mousemove;w_pics_main.setmicrohelp('Ready')
end event

type cb_ext from commandbutton within w_magazine_issue_maintenance
integer x = 59
integer y = 1536
integer width = 713
integer height = 112
integer taborder = 60
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "Add/Update Extensions"
end type

event clicked;str_exist_inv lstr_exist_inv
string ls_magcode
date ld_issuedate


//Get the magazine Code and Issuedate from the dialog box
ls_magcode = dw_magazine_issue_maintenance_magcd.Object.magcd[1]
ld_issuedate = Date(em_issue_date.text)

lstr_exist_inv.ls_magcd = ls_magcode
lstr_exist_inv.ld_issdt = ld_issuedate
openwithparm(w_add_mag_extesions, lstr_exist_inv)
dw_due_date_extension.ResetUpdate()

end event

type st_1 from u_st within w_magazine_issue_maintenance
integer x = 2089
integer y = 56
integer width = 238
string text = "Producer"
alignment alignment = right!
end type

type st_2 from u_st within w_magazine_issue_maintenance
integer x = 2158
integer y = 192
integer width = 169
boolean bringtotop = true
string text = "Format"
alignment alignment = right!
end type

type st_3 from u_st within w_magazine_issue_maintenance
integer x = 2158
integer y = 304
integer width = 169
boolean bringtotop = true
string text = "Status"
alignment alignment = right!
end type

type sle_producer from u_sle within w_magazine_issue_maintenance
integer x = 2350
integer y = 40
integer width = 219
integer height = 100
integer taborder = 0
integer textsize = -10
long backcolor = 12632256
textcase textcase = upper!
boolean displayonly = true
end type

type sle_format from u_sle within w_magazine_issue_maintenance
integer x = 2350
integer y = 168
integer width = 192
integer height = 92
integer taborder = 0
boolean bringtotop = true
integer textsize = -10
long backcolor = 12632256
boolean displayonly = true
end type

type sle_status from u_sle within w_magazine_issue_maintenance
integer x = 2350
integer y = 288
integer width = 165
integer height = 96
integer taborder = 0
boolean bringtotop = true
integer textsize = -10
long backcolor = 12632256
boolean displayonly = true
end type

type cb_update from u_cb within w_magazine_issue_maintenance
event ue_hinttext pbm_mousemove
string tag = "Updates the Database"
integer x = 1157
integer y = 1536
integer width = 297
integer height = 112
integer taborder = 0
integer textsize = -10
string text = "&Update"
end type

event ue_hinttext;call super::ue_hinttext;w_pics_main.setmicrohelp(this.tag)
end event

event clicked;call super::clicked;Integer li_num_days,rtn
String  ls_magcode
Date	  ld_issue_date
datetime ldt_issue_date
IF dw_magazine_issue_maintenance.Visible = TRUE THEN
	dw_magazine_issue_maintenance_rc.Visible = FALSE
	wf_set_zero()
	dw_magazine_issue_maintenance.AcceptText()
   dw_due_date_extension.AcceptText()
  
	IF dw_magazine_issue_maintenance.ModifiedCount() > 0 THEN
					
			Parent.SetMicrohelp("Updating Records Please Wait ...")
			dw_magazine_issue_maintenance.AcceptText()
			
			//Check if all fields have the necessary inputs
			IF wf_check_input() = 2 THEN
				RETURN 2
			END IF
				

				IF  dw_magazine_issue_maintenance.Update() = 1 THEN
						Parent.SetMicrohelp("")
						COMMIT USING SqlServerTrans;
					IF dw_due_date_extension.RowCount() > 0 THEN	
						//get extension days from datawindow.
						li_num_days = dw_due_date_extension.Object.ext[1]
						IF NOT(IsNull(li_num_days)) AND li_num_days <> 0 THEN
                    //Set the values in the data window and update the 
						//Update the Data Window (magiss table) and the magext table
		
					  //dw_due_date_extension datawindow.
					     ls_magcode = dw_magazine_issue_maintenance_magcd.Object.magcd[1]
						  ld_issue_date = Date(em_issue_date.text)
						  ldt_issue_date=datetime(ld_issue_date,time('00:00:00'))
						  dw_due_date_extension.Object.issdt[1] = ldt_issue_date
						  dw_due_date_extension.Object.magcd[1] = ls_magcode
						  
						  dw_due_date_extension.SetItemStatus(1, 0, Primary!, NewModified!)
						  rtn = dw_due_date_extension.Event pfc_update(TRUE,TRUE)
						  IF rtn = 1 THEN
						    COMMIT USING SqlServerTrans;
						  ELSE
							 MessageBox('Error','Error Accessing Table magext .. No Update Contact Your DBA')
						    ROLLBACK USING SqlServerTrans;
						  END IF
						
                  END IF//IF IsNull(li_num_days) OR li_num_days = 0 THEN
					END IF//IF dw_due_date_extension.RowCount() > 0 THEN		
						
						
						cb_clear.triggerEvent(clicked!)
						RETURN 1		
					ELSE
						MessageBox('ERROR','Error Accessing The Database.. Contact Your Database Administrator')
						ROLLBACK USING SQLServertrans;
						RETURN 1
					END IF//IF  dw_magazine_issue_maintenance.Update() = 1 THEN

					
					
		ELSE
			MessageBox('Update','No Changes Made .. No Update')
			cb_clear.triggerEvent(clicked!)
		END IF//IF dw_magazine_issue_maintenance.ModifiedCount() > 0 THEN
	


//update the data for the RC Cassettes		
ELSEIF dw_magazine_issue_maintenance_rc.visible = TRUE THEN
	   wf_set_zero()
		dw_magazine_issue_maintenance_rc.AcceptText()
	IF dw_magazine_issue_maintenance_rc.ModifiedCount() > 0 THEN
		 	Parent.SetMicrohelp("Updating Records Please Wait ...")
			
	//Check if all fields have the necessary inputs, if not RETURN 2
	   IF wf_check_input() = 2 THEN
			RETURN 2
		END IF
	
		
	//Update the Data Window (magiss table)
	IF  dw_magazine_issue_maintenance_rc.Update() = 1 THEN
		Parent.SetMicrohelp("")
		COMMIT USING SQLServertrans;
		//Get extension days from datawindow.
		li_num_days = dw_due_date_extension.Object.ext[1]
		IF NOT(IsNull(li_num_days)) AND li_num_days <> 0 THEN
		  //Set the values in the data window and update the 
		  //dw_due_date_extension datawindow.
		  ls_magcode = dw_magazine_issue_maintenance_magcd.Object.magcd[1]
		  ld_issue_date = Date(em_issue_date.text)
		  ldt_issue_date=datetime(ld_issue_date,time('00:00:00'))
		  dw_due_date_extension.Object.issdt[1] = ldt_issue_date
		  dw_due_date_extension.Object.magcd[1] = ls_magcode
		  
		  dw_due_date_extension.SetItemStatus(1, 0, Primary!, NewModified!)
		  //update the due date extension datawindow
		  IF dw_due_date_extension.Update() = 1 THEN
			 COMMIT USING SqlServerTrans;
		  ELSE
			 MessageBox('Error','Error Accessing Table magext .. No Update Contact Your DBA')
			 ROLLBACK USING SqlServerTrans;
		  END IF
		
		END IF//IF IsNull(li_num_days) OR li_num_days = 0 THEN
	
		cb_clear.triggerEvent(clicked!)
		RETURN 1		
	ELSE
		MessageBox('ERROR','Error Accessing The Database.. Contact Your Database Administrator')
		ROLLBACK USING SQLServertrans;
		RETURN 1
	END IF//IF dw_magazine_issue_maintenance_rc.Update() = 1 THEN
									
					
   ELSE
		MessageBox('Update','No Changes Made ... No Update')
		cb_clear.TriggerEvent(clicked!)
	END IF//IF dw_magazine_issue_maintenance_rc.ModifiedCount() > 0 THEN
END IF//IF dw_magazine_issue_maintenance.Visible = TRUE THEN

end event

type cb_clear from u_cb within w_magazine_issue_maintenance
event ue_hinttext pbm_mousemove
string tag = "Clears the screen for input"
integer x = 1573
integer y = 1536
integer width = 297
integer height = 112
integer taborder = 0
boolean bringtotop = true
integer textsize = -10
string text = "&Clear"
end type

event ue_hinttext;call super::ue_hinttext;w_pics_main.setmicrohelp(this.tag)
end event

event clicked;call super::clicked;
//clear the controls on the window
sle_format.text = ''
sle_producer.text = ''
sle_status.text = ''
em_issue_date.text = ''

//Disable the button, the edit mask and the datawindow
cb_update.Enabled = FALSE
cb_ext.Enabled = FALSE
dw_magazine_issue_maintenance.Enabled = FALSE
dw_magazine_issue_maintenance_rc.Enabled = FALSE
dw_due_date_extension.Enabled = FALSE
em_issue_date.DisplayOnly = TRUE
ib_disableclosequery=TRUE

//Reset the datawindows, add row and set date for dw_due_date_extension
dw_magazine_issue_maintenance.reset()
dw_magazine_issue_maintenance.Event pfc_addrow()
dw_magazine_issue_maintenance_magcd.reset()
dw_magazine_issue_maintenance_magcd.Event pfc_addrow()
dw_magazine_issue_maintenance_rc.reset()
dw_magazine_issue_maintenance_rc.Event pfc_addrow()
dw_due_date_extension.reset()
dw_due_date_extension.Event pfc_addrow()
dw_due_date_extension.SetItem(1,"extdt",today())


//make the rc datawindow invisible.
dw_magazine_issue_maintenance_rc.Visible = FALSE
dw_magazine_issue_maintenance.Visible = TRUE

//Set focus on datawindow
dw_magazine_issue_maintenance_magcd.SetFocus()
end event

type cb_exit from u_cb within w_magazine_issue_maintenance
event ue_hinttext pbm_mousemove
string tag = "Exits the screen"
integer x = 1989
integer y = 1536
integer width = 297
integer height = 112
integer taborder = 0
boolean bringtotop = true
integer textsize = -10
string text = "E&xit"
end type

event ue_hinttext;call super::ue_hinttext;w_pics_main.setmicrohelp(this.tag)
end event

event clicked;call super::clicked;Integer li_num_days, li_temp
String ls_magcd

dw_due_date_extension.AcceptText()
dw_magazine_issue_maintenance.AcceptText()
dw_magazine_issue_maintenance_rc.AcceptText()



//IF wf_check_input() = 2 THEN
//	RETURN
//END IF



//Check to see if magazine code field has any data if not then
//delete all rows.
ls_magcd = dw_magazine_issue_maintenance_magcd.Object.magcd[1]
IF IsNull(ls_magcd) OR TRIM(ls_magcd) = '' THEN
	dw_due_date_extension.Deleterow(0)
	dw_magazine_issue_maintenance.DeleteRow(0)
	dw_magazine_issue_maintenance_magcd.DeleteRow(0)
	dw_magazine_issue_maintenance_rc.DeleteRow(0)
END IF



//Check to see if any data has been input into the extension days
//datawindow - dw_due_date_extension. If not delete any rows.
IF dw_due_date_extension.RowCount() > 0 THEN
	li_num_days = dw_due_date_extension.Object.ext[1]
	IF IsNull(li_num_days) OR li_num_days = 0 THEN
	 dw_due_date_extension.Deleterow(0)
	END IF
END IF//IF dw_due_date_extension.RowCount() > 0 THEN

parent.Event pfc_close()

IF IsValid(w_magazine_issue_maintenance) = FALSE THEN
	m_pics_main.m_menu.popmenu(300,0)
END IF

end event

type em_issue_date from u_em within w_magazine_issue_maintenance
event ue_key pbm_keydown
event ue_fresh ( )
integer x = 407
integer y = 168
integer width = 343
integer height = 84
integer taborder = 20
integer textsize = -10
alignment alignment = center!
maskdatatype maskdatatype = datemask!
string mask = "mm/dd/yyyy"
string displaydata = "~b"
double increment = 0
string minmax = ""
end type

event ue_key;String ls_fy, ls_magcode, ls_producer, ls_format, ls_status, ls_contract
Integer li_rtn_code, li_ccd
Date ld_issuedate, ld_estshipdt


IF KeyDown(KeyEnter!) THEN
	
	
	//Get values into local variables
 	ls_fy = MID(em_issue_date.text,7)
 	li_fy = Integer(ls_fy)
 	ls_magcode = dw_magazine_issue_maintenance_magcd.Object.magcd[1]
	ld_issuedate = Date(em_issue_date.text)
	
	//Get Contract Information
	li_rtn_code = wf_get_contract_information(ls_magcode,ls_producer,ls_format,ls_status,ls_contract,li_ccd)
	
	//Check if contract exists.
	IF li_rtn_code = 100 THEN
		MessageBox('Error','Magazine Contract Does Not Exist For This Issue Date')
		em_issue_date.SetFocus()
		em_issue_date.Event pfc_selectall()
		RETURN
	ELSEIF li_rtn_code = 0 THEN //contract exists.
		   sle_format.text = ls_format
			sle_producer.text = ls_producer
			sle_status.text = ls_status
			
			
			
		//If the format is RC then process as follows.
		IF TRIM(ls_format) = 'RC' THEN
			
			dw_magazine_issue_maintenance.Visible = FALSE
         dw_magazine_issue_maintenance_rc.Visible = TRUE	
			dw_magazine_issue_maintenance_rc.Enabled = TRUE
			dw_magazine_issue_maintenance_rc.SetFocus()
			dw_magazine_issue_maintenance_rc.Object.fy[1] = li_fy
			dw_magazine_issue_maintenance_rc.Object.cntr[1] = ls_contract
			
			//check if issuedate exists if it does then retrieve data and
			//set the ext to the instance variable li_ext.
			IF wf_check_issuedate(ls_magcode, ld_issuedate) = 0 THEN
			   dw_magazine_issue_maintenance_rc.Retrieve(ls_magcode, ld_issuedate)
			   dw_due_date_extension.Enabled = TRUE
				Parent.Event pfc_microhelp("Proceed with update")
				li_ext = dw_magazine_issue_maintenance_rc.Object.extc[1]
			   dw_magazine_issue_maintenance_rc.SetFocus()
				cb_update.Enabled = TRUE

					  
				IF wf_get_ccd(ls_magcode, ls_contract, li_ccd) = 100 THEN
				  li_ccd = 0
				END IF
				
				
				// 2/12/2001 change the first parameter from ld_startdt to ld_issuedate
				// because the estshipdate will be calculated based on issuedate instead of startdt. PR #791
				ld_estshipdt  = RelativeDate(ld_issuedate, li_ccd)
				
				string local_ext
				local_ext = string(dw_magazine_issue_maintenance_rc.Object.extc[1])
				IF NOT(IsNull(local_ext)) AND TRIM(local_ext) <> "" THEN
					ld_estshipdt  = RelativeDate(ld_estshipdt, integer(local_ext))
				END IF	
				// 2/12/2001 change the first parameter from ld_startdt to ld_issuedate
				// because the estshipdate will be calculated based on issuedate instead of startdt. PR #791
				wf_get_estshipdate(ld_issuedate,ls_producer,ld_estshipdt)
					
				dw_magazine_issue_maintenance_rc.SetItem(1,"estshipdt",ld_estshipdt)

			ELSE
				//Set magcode and issue date to the datawindow
				dw_magazine_issue_maintenance_rc.Object.magcd[1] = ls_magcode
				dw_magazine_issue_maintenance_rc.Object.issdt[1] = ld_issuedate
				cb_update.Enabled = TRUE
				Parent.Event pfc_microhelp("Proceed with Add")
				li_ext = dw_magazine_issue_maintenance_rc.Object.extc[1]
				dw_magazine_issue_maintenance_rc.SetFocus()
			END IF//IF wf_check_issuedate(ls_magcode, ld_issuedate) = 0 THEN
			
			cb_ext.Enabled = TRUE
			dw_due_date_extension.reset()
			dw_due_date_extension.Event pfc_addrow()
			dw_due_date_extension.SetItem(1,"extdt",today())			
			RETURN
		END IF//IF TRIM(ls_format) = 'RC' THEN
		
		
		//IF the format is BR or FD then process as follows
		IF TRIM(ls_format) <> 'RC' THEN
			//make the RC data window non visible and the BR and FD
			//datawindows visible.
			dw_magazine_issue_maintenance.Visible = TRUE
         dw_magazine_issue_maintenance_rc.Visible = FALSE	
			
			
			//protect the the minutes column if the format is BR
			IF TRIM(ls_format) = 'BR' THEN
			  dw_magazine_issue_maintenance.Object.magiss_mins.Protect = 1
		   ELSEIF TRIM(ls_format) = 'FD' THEN
			  dw_magazine_issue_maintenance.Object.magiss_mins.Protect = 0
		   END IF//IF TRIM(ls_format) <> 'FD' THEN
			
			dw_magazine_issue_maintenance.Enabled = TRUE
			//Set the contract and fiscal year to the datawindow
			dw_magazine_issue_maintenance.Object.magiss_fy[1] = li_fy
			dw_magazine_issue_maintenance.Object.magiss_cntr[1] = ls_contract
			
			//check if issuedate exists if it does then retrieve data
			//and enable dw_due_date_extension and set the extension days
			//to the instance variable li_ext.
			IF wf_check_issuedate(ls_magcode, ld_issuedate) = 0 THEN
			   dw_magazine_issue_maintenance.Retrieve(ls_magcode, ld_issuedate)
				dw_due_date_extension.Enabled = TRUE
			   Parent.Event pfc_microhelp("Proceed with update")
			   dw_magazine_issue_maintenance.SetFocus()
				li_ext = dw_magazine_issue_maintenance.Object.magiss_extc[1]
				cb_update.Enabled = TRUE	
						  
				IF wf_get_ccd(ls_magcode, ls_contract, li_ccd) = 100 THEN
				  li_ccd = 0
				END IF
				//	  	  IF wf_is_date_empty(getText()) = FALSE THEN
				
				// 2/12/2001 change the first parameter from ld_startdt to ld_issuedate
				// because the estshipdate will be calculated based on issuedate instead of startdt. PR #791
				ld_estshipdt  = RelativeDate(ld_issuedate, li_ccd)

				local_ext = string(dw_magazine_issue_maintenance.Object.magiss_extc[1])
				IF NOT(IsNull(local_ext)) AND TRIM(local_ext) <> "" THEN
					ld_estshipdt  = RelativeDate(ld_estshipdt, integer(local_ext))
				END IF	
				// 2/12/2001 change the first parameter from ld_startdt to ld_issuedate
				// because the estshipdate will be calculated based on issuedate instead of startdt. PR #791
				wf_get_estshipdate(ld_issuedate,ls_producer,ld_estshipdt)
				
				  
				dw_magazine_issue_maintenance.SetItem(1,"magiss_estshipdt",ld_estshipdt)
			ELSE
				//Set magcode and issue date to the datawindow
				dw_magazine_issue_maintenance.Object.magiss_magcd[1] = ls_magcode
				dw_magazine_issue_maintenance.Object.magiss_issdt[1] = ld_issuedate
				cb_update.Enabled = TRUE
				Parent.Event pfc_microhelp("Proceed with Add")
				li_ext = dw_magazine_issue_maintenance.Object.magiss_extc[1]
				 dw_magazine_issue_maintenance.SetFocus()
			END IF//IF wf_check_issuedate(ls_magcode, ld_issuedate) = 0 THEN
			
			cb_ext.Enabled = TRUE
			dw_due_date_extension.reset()
			dw_due_date_extension.Event pfc_addrow()
			dw_due_date_extension.SetItem(1,"extdt",today())			
			RETURN
		END IF//IF TRIM(ls_format) <> 'RC' THEN

		
		
		
	END IF//IF li_rtn_code = 100 THEN
		
END IF//IF KeyDown(KeyEnter!) THEN
end event

event ue_fresh;String ls_fy, ls_magcode, ls_producer, ls_format, ls_status, ls_contract
Integer li_rtn_code, li_ccd
Date ld_issuedate



	
	
//Get values into local variables
ls_fy = MID(em_issue_date.text,7)
li_fy = Integer(ls_fy)
ls_magcode = dw_magazine_issue_maintenance_magcd.Object.magcd[1]
ld_issuedate = Date(em_issue_date.text)

//Get Contract Information
li_rtn_code = wf_get_contract_information(ls_magcode,ls_producer,ls_format,ls_status,ls_contract,li_ccd)

//Check if contract exists.
IF li_rtn_code = 100 THEN
	MessageBox('Error','Magazine Contract Does Not Exist For This Issue Date')
	em_issue_date.SetFocus()
	em_issue_date.Event pfc_selectall()
	RETURN
ELSEIF li_rtn_code = 0 THEN //contract exists.
		sle_format.text = ls_format
		sle_producer.text = ls_producer
		sle_status.text = ls_status
		
		
		
	//If the format is RC then process as follows.
	IF TRIM(ls_format) = 'RC' THEN
		
		dw_magazine_issue_maintenance.Visible = FALSE
		dw_magazine_issue_maintenance_rc.Visible = TRUE	
		dw_magazine_issue_maintenance_rc.Enabled = TRUE
		dw_magazine_issue_maintenance_rc.SetFocus()
		dw_magazine_issue_maintenance_rc.Object.fy[1] = li_fy
		dw_magazine_issue_maintenance_rc.Object.cntr[1] = ls_contract
		
		//check if issuedate exists if it does then retrieve data and
		//set the ext to the instance variable li_ext.
		IF wf_check_issuedate(ls_magcode, ld_issuedate) = 0 THEN
			dw_magazine_issue_maintenance_rc.Retrieve(ls_magcode, ld_issuedate)
			dw_due_date_extension.Enabled = TRUE
			Parent.Event pfc_microhelp("Proceed with update")
			li_ext = dw_magazine_issue_maintenance_rc.Object.extc[1]
			dw_magazine_issue_maintenance_rc.SetFocus()
			cb_update.Enabled = TRUE
		ELSE
			//Set magcode and issue date to the datawindow
			dw_magazine_issue_maintenance_rc.Object.magcd[1] = ls_magcode
			dw_magazine_issue_maintenance_rc.Object.issdt[1] = ld_issuedate
			cb_update.Enabled = TRUE
			Parent.Event pfc_microhelp("Proceed with Add")
			li_ext = dw_magazine_issue_maintenance_rc.Object.extc[1]
			dw_magazine_issue_maintenance_rc.SetFocus()
		END IF//IF wf_check_issuedate(ls_magcode, ld_issuedate) = 0 THEN
		
		cb_ext.Enabled = TRUE
					
		RETURN
	END IF//IF TRIM(ls_format) = 'RC' THEN
	
	
	//IF the format is BR or FD then process as follows
	IF TRIM(ls_format) <> 'RC' THEN
		//make the RC data window non visible and the BR and FD
		//datawindows visible.
		dw_magazine_issue_maintenance.Visible = TRUE
		dw_magazine_issue_maintenance_rc.Visible = FALSE	
		
		
		//protect the the minutes column if the format is BR
		IF TRIM(ls_format) = 'BR' THEN
		  dw_magazine_issue_maintenance.Object.magiss_mins.Protect = 1
		ELSEIF TRIM(ls_format) = 'FD' THEN
		  dw_magazine_issue_maintenance.Object.magiss_mins.Protect = 0
		END IF//IF TRIM(ls_format) <> 'FD' THEN
		
		dw_magazine_issue_maintenance.Enabled = TRUE
		//Set the contract and fiscal year to the datawindow
		dw_magazine_issue_maintenance.Object.magiss_fy[1] = li_fy
		dw_magazine_issue_maintenance.Object.magiss_cntr[1] = ls_contract
		
		//check if issuedate exists if it does then retrieve data
		//and enable dw_due_date_extension and set the extension days
		//to the instance variable li_ext.
		IF wf_check_issuedate(ls_magcode, ld_issuedate) = 0 THEN
			dw_magazine_issue_maintenance.Retrieve(ls_magcode, ld_issuedate)
			dw_due_date_extension.Enabled = TRUE
			Parent.Event pfc_microhelp("Proceed with update")
			dw_magazine_issue_maintenance.SetFocus()
			li_ext = dw_magazine_issue_maintenance.Object.magiss_extc[1]
			cb_update.Enabled = TRUE
		ELSE
			//Set magcode and issue date to the datawindow
			dw_magazine_issue_maintenance.Object.magiss_magcd[1] = ls_magcode
			dw_magazine_issue_maintenance.Object.magiss_issdt[1] = ld_issuedate
			cb_update.Enabled = TRUE
			Parent.Event pfc_microhelp("Proceed with Add")
			li_ext = dw_magazine_issue_maintenance.Object.magiss_extc[1]
			 dw_magazine_issue_maintenance.SetFocus()
		END IF//IF wf_check_issuedate(ls_magcode, ld_issuedate) = 0 THEN
		
		cb_ext.Enabled = TRUE
					
		RETURN
	END IF//IF TRIM(ls_format) <> 'RC' THEN

	
	
	
END IF//IF li_rtn_code = 100 THEN
	

end event

type st_4 from statictext within w_magazine_issue_maintenance
integer x = 78
integer y = 176
integer width = 283
integer height = 76
boolean bringtotop = true
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long backcolor = 12632256
boolean enabled = false
string text = "Issue Date"
alignment alignment = right!
boolean focusrectangle = false
end type

type dw_due_date_extension from u_dw within w_magazine_issue_maintenance
event ue_enter_to_tab pbm_dwnprocessenter
integer x = 37
integer y = 1024
integer width = 2057
integer height = 428
integer taborder = 50
string dataobject = "d_magazine_due_date_extensions_magext"
boolean vscrollbar = false
boolean livescroll = false
end type

event ue_enter_to_tab;call super::ue_enter_to_tab;
Send(Handle(this), 256,9,Long(0,0))
RETURN 1
end event

event constructor;call super::constructor;SettransObject(SqlServerTrans)


//dw_due_date_extension.Event pfc_addrow()
ib_rmbmenu = FALSE   			//disable the right mouse button
end event

event itemchanged;call super::itemchanged;Integer li_extcaldays, li_extension_days
String ls_ext
date ld_shipdt
datetime ldt_shipdt

//Add the extension days to the magiss_extc field, the li_ext field is
//an instance variable that is set in the ue_key event of em_issue_date.
IF DWO.Name = "ext" THEN
	
	 dw_due_date_extension.Object.extdt[dw_due_date_extension.GetRow()] = today()

	 ls_ext = DATA
	 IF IsNull(ls_ext) OR TRIM(ls_ext) = '' THEN li_extension_days = 0
	 IF NOT(IsNull(ls_ext)) AND TRIM(ls_ext) <> '' THEN li_extension_days = Integer(DATA)
	 IF IsNull(li_ext) THEN li_ext = 0 
	 IF dw_magazine_issue_maintenance.Visible = TRUE THEN
		dw_magazine_issue_maintenance.Object.magiss_extc[1] = li_ext + li_extension_days
		ldt_shipdt=dw_magazine_issue_maintenance.Object.magiss_estshipdt[1]
		ld_shipdt=date(ldt_shipdt)
		dw_magazine_issue_maintenance.Object.magiss_estshipdt[1] =	&
				datetime(RelativeDate(ld_shipdt, li_extension_days),time('00:00:00'))
				
	 ELSEIF dw_magazine_issue_maintenance_rc.Visible = TRUE THEN
		dw_magazine_issue_maintenance_rc.Object.extc[1] = li_ext + li_extension_days
		ldt_shipdt=dw_magazine_issue_maintenance_rc.Object.estshipdt[1]
		ld_shipdt=date(ldt_shipdt)
		dw_magazine_issue_maintenance_rc.Object.estshipdt[1] =	&
				datetime(RelativeDate(ld_shipdt,li_extension_days),time('00:00:00'))
				
    END IF//IF dw_magazine_issue_maintenance.Visible = TRUE THEN
		

END IF//IF DWO.Name = "ext" THEN	

end event

type dw_magazine_issue_maintenance_magcd from u_dw within w_magazine_issue_maintenance
integer x = 32
integer y = 32
integer width = 2002
integer height = 120
integer taborder = 10
boolean bringtotop = true
string dataobject = "d_magazine_issue_maintenance_magcd"
boolean vscrollbar = false
boolean livescroll = false
end type

event constructor;call super::constructor;SetTransObject(SqlServerTrans)
dw_magazine_issue_maintenance_magcd.Event pfc_addrow()

end event

event itemchanged;call super::itemchanged;String ls_magcode
Integer li_rtn_code

//Get value into local variable
ls_magcode = GetText()

li_rtn_code = dw_magazine_issue_maintenance_magcd.Retrieve(ls_magcode)

//Check to see if the entered value is correct if not show
//message box.
IF li_rtn_code = 0 THEN
	dw_magazine_issue_maintenance_magcd.Object.magcd.validationmsg = &
	                   'Magazine Code Not Found'
	dw_magazine_issue_maintenance_magcd.Event pfc_addrow()
   Return 1
ELSEIF li_rtn_code = 1 THEN
	//enable the edit mask
	em_issue_date.DisplayOnly = FALSE
	em_issue_date.SetFocus()
END IF




end event

type dw_magazine_issue_maintenance_rc from u_dw within w_magazine_issue_maintenance
event ue_enter_to_tab pbm_dwnprocessenter
boolean visible = false
integer x = 37
integer y = 284
integer width = 2071
integer height = 688
integer taborder = 30
string dataobject = "d_magazine_issue_maintenance_rc"
boolean vscrollbar = false
boolean border = false
boolean livescroll = false
borderstyle borderstyle = stylebox!
end type

event ue_enter_to_tab;call super::ue_enter_to_tab;
Send(Handle(this), 256,9,Long(0,0))
RETURN 1
end event

event constructor;call super::constructor;this.of_settransobject(sqlServerTrans)
this.Event pfc_addrow()
end event

event itemchanged;call super::itemchanged;String   ls_title, ls_magcode, ls_fy, ls_producer, ls_format, ls_status, ls_contract, &
         null_string, ls_tempstring, ls_subs, ls_issdt
Integer  li_rtn_code, null_number, li_ccd
Date     null_date, ld_issuedate, ld_startdt, ld_estshipdt, ld_shipdt
datetime ldt_estshipdt, ldt_startdt

SetNull(null_string)
SetNull(null_date)
SetNull(null_number)

//messagebox('rc','I am here')
//If the magazine start date is changed then calculate the estimated
//shipdate.
IF  DWO.Name = "startdt"  THEN
	ls_issdt=mid(data,1,10)
	if IsDate(ls_issdt) then
	  ld_startdt  = Date(ls_issdt)
	else
		messagebox('','Error You should enter correct date')
		return 1
	end if
ELSE 
	ldt_startdt =dw_magazine_issue_maintenance_rc.GetItemDatetime(row,"startdt")
END IF
	  
ls_producer = TRIM(sle_producer.text)
ls_magcode  = dw_magazine_issue_maintenance_rc.GetItemString(row,"magcd")
ls_contract = dw_magazine_issue_maintenance_rc.GetItemString(row,"cntr")
ld_issuedate = Date(em_issue_date.text)
	  
IF wf_get_ccd(ls_magcode, ls_contract, li_ccd) = 100 THEN
  li_ccd = 0
END IF


// 2/12/2001 change the first parameter from ld_startdt to ld_issuedate
// because the estshipdate will be calculated based on issuedate instead of startdt. PR #791
ld_estshipdt  = RelativeDate(ld_issuedate, li_ccd)

string local_ext
local_ext = string(dw_magazine_issue_maintenance_rc.Object.extc[1])
IF NOT(IsNull(local_ext)) AND TRIM(local_ext) <> "" THEN
	ld_estshipdt  = RelativeDate(ld_estshipdt, integer(local_ext))
END IF	
// 2/12/2001 change the first parameter from ld_startdt to ld_issuedate
// because the estshipdate will be calculated based on issuedate instead of startdt. PR #791
wf_get_estshipdate(ld_issuedate,ls_producer,ld_estshipdt)
ldt_estshipdt=datetime(ld_estshipdt	,time('00:00:00'))
dw_magazine_issue_maintenance_rc.SetItem(row,"estshipdt",ldt_estshipdt)

//Enable due date extension datawindow		
dw_due_date_extension.Enabled = TRUE
//END IF//IF  DWO.Name = "magiss_startdt" THEN












	

end event

type dw_magazine_issue_maintenance from u_dw within w_magazine_issue_maintenance
event ue_enter_to_tab pbm_dwnprocessenter
event ue_hinttext pbm_mousemove
integer x = 37
integer y = 320
integer width = 2071
integer height = 584
integer taborder = 40
string dataobject = "d_issue_maintenance_magiss"
boolean vscrollbar = false
boolean livescroll = false
end type

event ue_enter_to_tab;call super::ue_enter_to_tab;
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

event itemchanged;call super::itemchanged;String   ls_title, ls_magcode, ls_fy, ls_producer, ls_format, ls_status, ls_contract, &
         null_string, ls_tempstring, ls_subs, ls_issdt
Integer  li_rtn_code, null_number, li_ccd
Date     null_date, ld_issuedate, ld_startdt, ld_estshipdt, ld_shipdt
datetime ldt_startdt, ldt_estshipdt

SetNull(null_string)
SetNull(null_date)
SetNull(null_number)

//If the magazine start date is changed then calculate the estimated
//shipdate.
//messagebox('BR','I am here')
IF  DWO.Name = "magiss_startdt" THEN
	ls_issdt=mid(data,1,10)
	IF IsDate(ls_issdt) = TRUE THEN
	  ld_startdt  = Date(ls_issdt)
	ELSE
		MESSAGEBOX('','Error! You must enter correct date!')
		RETURN 1
	END IF
else
	ldt_startdt  = dw_magazine_issue_maintenance.GetItemdatetime(row,"magiss_startdt")
end if
ls_producer = sle_producer.text
ls_magcode  = dw_magazine_issue_maintenance.GetItemString(row,"magiss_magcd")
ls_contract = dw_magazine_issue_maintenance.GetItemString(row,"magiss_cntr")
ld_issuedate = Date(em_issue_date.text)

	 	  
IF wf_get_ccd(ls_magcode, ls_contract, li_ccd) = 100 THEN
  li_ccd = 0
END IF
//	  	  IF wf_is_date_empty(getText()) = FALSE THEN

// 2/12/2001 change the first parameter from ld_startdt to ld_issuedate
// because the estshipdate will be calculated based on issuedate instead of startdt. PR #791
ld_estshipdt  = RelativeDate(ld_issuedate, li_ccd)
string local_ext
local_ext = string(dw_magazine_issue_maintenance.Object.magiss_extc[1])
IF NOT(IsNull(local_ext)) AND TRIM(local_ext) <> "" THEN
	ld_estshipdt  = RelativeDate(ld_estshipdt, integer(local_ext))
END IF	
// 2/12/2001 change the first parameter from ld_startdt to ld_issuedate
// because the estshipdate will be calculated based on issuedate instead of startdt. PR #791
wf_get_estshipdate(ld_issuedate,ls_producer,ld_estshipdt)

ldt_estshipdt=datetime(ld_estshipdt,time('00:00:00'))
dw_magazine_issue_maintenance.SetItem(row,"magiss_estshipdt",ldt_estshipdt)
	
dw_due_date_extension.Enabled = TRUE













	

end event

event constructor;call super::constructor;SettransObject(SqlServerTrans)


dw_magazine_issue_maintenance.Event pfc_addrow()
ib_rmbmenu = FALSE   			//disable the right mouse button
end event

event updateend;call super::updateend;long ll_total

ll_total = rowsinserted + rowsupdated

IF ll_total = 0 THEN
	MessageBox('Update','No records updated.')
END IF
end event

event itemfocuschanged;call super::itemfocuschanged;//The field gets highlighted when focus changes to it.



IF DWO.Name = "magiss_startdt" THEN
	THIS.Event pfc_selectall()
END IF


IF DWO.Name = "magiss_shipdt" THEN
	THIS.Event pfc_selectall()
END IF

IF DWO.Name = "magiss_subs" THEN
	THIS.Event pfc_selectall()
END IF

IF DWO.Name = "magiss_sz" THEN
	THIS.Event pfc_selectall()
END IF


IF DWO.Name = "magiss_mins" THEN
	THIS.Event pfc_selectall()
END IF


end event


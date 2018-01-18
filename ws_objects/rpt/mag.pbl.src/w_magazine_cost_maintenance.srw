$PBExportHeader$w_magazine_cost_maintenance.srw
forward
global type w_magazine_cost_maintenance from w_sheet
end type
type rb_inv_comp from checkbox within w_magazine_cost_maintenance
end type
type rb_ship_comp from checkbox within w_magazine_cost_maintenance
end type
type st_fy from statictext within w_magazine_cost_maintenance
end type
type st_contract from statictext within w_magazine_cost_maintenance
end type
type st_producer from statictext within w_magazine_cost_maintenance
end type
type st_format from statictext within w_magazine_cost_maintenance
end type
type st_total from statictext within w_magazine_cost_maintenance
end type
type dw_producer from u_pics_dw within w_magazine_cost_maintenance
end type
type sle_format from u_sle within w_magazine_cost_maintenance
end type
type em_cntr_total from editmask within w_magazine_cost_maintenance
end type
type cb_exit from u_cb within w_magazine_cost_maintenance
end type
type cb_clear from u_cb within w_magazine_cost_maintenance
end type
type cb_update from u_cb within w_magazine_cost_maintenance
end type
type cb_toggle from u_cb within w_magazine_cost_maintenance
end type
type em_fy from u_em within w_magazine_cost_maintenance
end type
type sle_contract from u_sle within w_magazine_cost_maintenance
end type
type cb_print from u_cb within w_magazine_cost_maintenance
end type
type st_records from statictext within w_magazine_cost_maintenance
end type
type st_records_no from statictext within w_magazine_cost_maintenance
end type
type dw_magazine_cost_maintenance from u_dw within w_magazine_cost_maintenance
end type
type dw_magazine_cost_maintenance_nested from u_dw within w_magazine_cost_maintenance
end type
type dw_magazine_cost_maintenance_rc_nested from u_dw within w_magazine_cost_maintenance
end type
type dw_magazine_cost_maintenance_rc from u_dw within w_magazine_cost_maintenance
end type
end forward

global type w_magazine_cost_maintenance from w_sheet
integer width = 3223
integer height = 1816
string title = "Magazine Cost Maintenance"
rb_inv_comp rb_inv_comp
rb_ship_comp rb_ship_comp
st_fy st_fy
st_contract st_contract
st_producer st_producer
st_format st_format
st_total st_total
dw_producer dw_producer
sle_format sle_format
em_cntr_total em_cntr_total
cb_exit cb_exit
cb_clear cb_clear
cb_update cb_update
cb_toggle cb_toggle
em_fy em_fy
sle_contract sle_contract
cb_print cb_print
st_records st_records
st_records_no st_records_no
dw_magazine_cost_maintenance dw_magazine_cost_maintenance
dw_magazine_cost_maintenance_nested dw_magazine_cost_maintenance_nested
dw_magazine_cost_maintenance_rc_nested dw_magazine_cost_maintenance_rc_nested
dw_magazine_cost_maintenance_rc dw_magazine_cost_maintenance_rc
end type
global w_magazine_cost_maintenance w_magazine_cost_maintenance

type variables
String ls_temp_contract, ls_prdr
end variables

forward prototypes
public function integer wf_check_producer (integer li_fy, string ls_producer, ref integer li_num_rows)
public function integer wf_get_format (integer li_fy, string ls_producer, ref string ls_contract, ref string ls_format)
public subroutine wf_disable_objects ()
public subroutine wf_calculate_total (ref decimal lr_total_contract_amount)
public subroutine wf_calculate_total_rc (ref decimal lr_total_contract_amount)
public subroutine wf_calculate_contract_cost (integer row)
public subroutine wf_calculate_contract_cost_rc (integer row)
public subroutine wf_set_zero ()
public function integer wf_check_contract (string ls_contract, integer li_fy, ref string ls_producer, ref string ls_format, ref real lr_cntr_total, ref string lc_ship_comp, ref string lc_inv_comp)
end prototypes

public function integer wf_check_producer (integer li_fy, string ls_producer, ref integer li_num_rows);


SELECT count(*) 
INTO  :li_num_rows
FROM  magcntr
WHERE magcntr.fy =   :li_fy AND
		magcntr.prdr = :ls_producer
USING SqlServerTrans;

Return SqlServerTrans.Sqlcode
end function

public function integer wf_get_format (integer li_fy, string ls_producer, ref string ls_contract, ref string ls_format);
SELECT format,cntr
INTO   :ls_format,
       :ls_contract
FROM   magcntr
WHERE  magcntr.fy = :li_fy AND magcntr.prdr = :ls_producer
USING SqlServerTrans;


Return SqlServertrans.sqlcode
end function

public subroutine wf_disable_objects ();em_fy.taborder = 0
sle_contract.taborder = 0
dw_producer.taborder = 0
cb_toggle.Enabled = TRUE
cb_update.Enabled = TRUE
em_fy.DisplayOnly = TRUE
sle_contract.DisplayOnly = TRUE
dw_producer.Enabled = FALSE
end subroutine

public subroutine wf_calculate_total (ref decimal lr_total_contract_amount);Integer li_num_rows, li_loop
Decimal    lr_cntcsc


dw_magazine_cost_maintenance.AcceptText()
li_num_rows = dw_magazine_cost_maintenance.RowCount()
//Calculate the total contract amount and return the value. This value will be
//placed in the em_cntr_total edit mask.
FOR li_loop = 1 to li_num_rows

lr_cntcsc = dw_magazine_cost_maintenance.GetItemNumber(li_loop,"mag_cntcsc")
IF IsNull(lr_cntcsc) THEN lr_cntcsc = 0

 lr_total_contract_amount = lr_total_contract_amount + lr_cntcsc


NEXT


end subroutine

public subroutine wf_calculate_total_rc (ref decimal lr_total_contract_amount);//Calculates the total amount for RC magazines.
Integer li_num_rows, li_loop
Decimal    lr_cntcsc


dw_magazine_cost_maintenance.AcceptText()
li_num_rows = dw_magazine_cost_maintenance_rc.RowCount()
//Calculate the total contract amount and return the value. This value will be
//placed in the em_cntr_total edit mask.
FOR li_loop = 1 to li_num_rows

lr_cntcsc = dw_magazine_cost_maintenance_rc.GetItemNumber(li_loop,"cntcsc")
IF IsNull(lr_cntcsc) THEN lr_cntcsc = 0

 lr_total_contract_amount = lr_total_contract_amount + lr_cntcsc


NEXT

end subroutine

public subroutine wf_calculate_contract_cost (integer row);//This function is called by the itemchanged event in dw_magazine_cost_
//maintenance screen. It calculates the contract cost when the unit cost
//mastering and the unit cost duplications are changed/input. The
//resultant value is placed in the cntcso field (which is the contract
//cost field). 

// Accept the text that was inputed.
dw_magazine_cost_maintenance.AcceptText()

Decimal lr_cntcso, lr_cntcsadjc,  lr_estmail, lr_mastering_cost, lr_duplication_cost
Decimal lr_total, lr_total_contract_amount, lr_ucmast, lr_ucdupl
long li_estmin, li_estsubs, li_estsz, li_estiss

   lr_ucmast =  dw_magazine_cost_maintenance.Object.mag_ucmast[row]
	 IF IsNull(lr_ucmast) THEN lr_ucmast = 0
	lr_ucdupl  =  dw_magazine_cost_maintenance.Object.mag_ucdupl[row]
	 IF IsNull(lr_ucdupl) THEN lr_ucdupl = 0
	li_estmin =  dw_magazine_cost_maintenance.Object.mag_estmin[row]
	 IF IsNull(li_estmin) THEN li_estmin = 0
	li_estsubs = dw_magazine_cost_maintenance.Object.mag_estsubs[row]
	 IF IsNull(li_estsubs) THEN li_estsubs = 0
	li_estsz   = dw_magazine_cost_maintenance.Object.mag_estsz[row]
	 IF IsNull(li_estsz) THEN li_estsz = 0
	li_estiss  = dw_magazine_cost_maintenance.Object.mag_estiss[row]
	 IF IsNull(li_estiss) THEN li_estiss = 0
	 
	//Calculate the mastering cost for FD's and BR's
	IF TRIM(sle_format.text) = 'FD' THEN
		lr_mastering_cost = lr_ucmast * li_estmin
	ELSEIF TRIM(sle_format.text) = 'BR' THEN
		lr_mastering_cost = lr_ucmast * li_estsz
	END IF
	//Calculate the duplication cost
	   lr_duplication_cost = lr_ucdupl * li_estsz * li_estsubs
	
	//Set the calculated total value to contract cost original.
	//Calculate the adjusted total and display in em_cntr_total.
	   lr_cntcso = li_estiss * (lr_mastering_cost + lr_duplication_cost)
		IF NOT(IsNull(lr_cntcso)) OR lr_cntcso <> 0 THEN
	    dw_magazine_cost_maintenance.Object.mag_cntcso[row] = lr_cntcso
		 	IF IsNull(lr_cntcso) THEN lr_cntcso = 0
			lr_cntcsadjc = dw_magazine_cost_maintenance.Object.mag_cntcsadjc[row]
			IF IsNull(lr_cntcsadjc) THEN lr_cntcsadjc = 0
			lr_estmail = dw_magazine_cost_maintenance.Object.mag_estmail[row]
			IF IsNull(lr_estmail) THEN lr_estmail = 0
			lr_total = lr_cntcso  + lr_cntcsadjc + lr_estmail
			dw_magazine_cost_maintenance.Object.mag_cntcsc[row] = lr_total
			wf_calculate_total(lr_total_contract_amount)
			em_cntr_total.text = String(lr_total_contract_amount)
		END IF
		
end subroutine

public subroutine wf_calculate_contract_cost_rc (integer row);//This function is called by the itemchanged event in dw_magazine_cost_
//maintenance_rc screen. It calculates the contract cost when the unit cost
//mastering and the unit cost duplications are changed/input. The
//resultant value is placed in the cntcso field (which is the contract
//cost field). 

// Accept the text that was inputed.
dw_magazine_cost_maintenance_rc.AcceptText()

Decimal lr_cntcsadjc,  lr_estmail
Decimal lr_total_contract_amount, lr_ucmast
Decimal lr_ucdupl, lr_ucdupls, lr_ucdupll, lr_estsz, lr_estszs, lr_estszl
long lr_cntcso, lr_total,li_estsubs, lr_mastering_cost, lr_duplication_cost,li_estmin, li_estsz, li_estszs, li_estszl, li_estiss

//Ge values into local variables
   lr_ucmast =  dw_magazine_cost_maintenance_rc.Object.ucmast[row]
	 IF IsNull(lr_ucmast) THEN lr_ucmast = 0
	lr_ucdupl  =  dw_magazine_cost_maintenance_rc.Object.ucdupl[row]
	 IF IsNull(lr_ucdupl) THEN lr_ucdupl = 0
	lr_ucdupls =  dw_magazine_cost_maintenance_rc.Object.ucdupls[row]
	 IF IsNull(lr_ucdupls) THEN lr_ucdupls = 0
	lr_ucdupll =  dw_magazine_cost_maintenance_rc.Object.ucdupll[row]
	 IF IsNull(lr_ucdupll) THEN lr_ucdupll = 0
	li_estmin =  dw_magazine_cost_maintenance_rc.Object.mag_estmin[row]
	 IF IsNull(li_estmin) THEN li_estmin = 0
	li_estsubs = dw_magazine_cost_maintenance_rc.Object.mag_estsubs[row]
	 IF IsNull(li_estsubs) THEN li_estsubs = 0
	li_estsz   = dw_magazine_cost_maintenance_rc.Object.mag_estsz[row]
	 IF IsNull(li_estsz) THEN li_estsz = 0
	li_estszs   = dw_magazine_cost_maintenance_rc.Object.mag_estszs[row]
	 IF IsNull(li_estszs) THEN li_estszs = 0
	li_estszl   = dw_magazine_cost_maintenance_rc.Object.mag_estszl[row]
	 IF IsNull(li_estszl) THEN li_estszl = 0
	li_estiss  = dw_magazine_cost_maintenance_rc.Object.mag_estiss[row]
	 IF IsNull(li_estiss) THEN li_estiss = 0
	
	//Calculate the mastering cost for FD's and RC's
	   lr_mastering_cost = lr_ucmast * li_estmin
		
	//Calculate the duplication cost for BR,FD,and RC's format
	   lr_duplication_cost = ((lr_ucdupls * li_estszs) + (lr_ucdupl * li_estsz) + (lr_ucdupll * li_estszl)) * li_estsubs
		
	
	//Set the calculated total value to contract cost original.
	//Calculate the adjusted total and display in em_cntr_total.
	   lr_cntcso = li_estiss * (lr_mastering_cost + lr_duplication_cost)
	   IF NOT(IsNull(lr_cntcso)) OR lr_cntcso <> 0 THEN
	    dw_magazine_cost_maintenance_rc.Object.cntcso[row] = lr_cntcso
		 	lr_cntcsadjc = dw_magazine_cost_maintenance_rc.Object.cntcsadjc[row]
			 IF IsNull(lr_cntcsadjc) THEN lr_cntcsadjc = 0
			lr_estmail = dw_magazine_cost_maintenance_rc.Object.estmail[row]
			 IF IsNull(lr_estmail) THEN lr_estmail = 0
			lr_total = lr_cntcso  + lr_cntcsadjc + lr_estmail
			dw_magazine_cost_maintenance_rc.Object.cntcsc[row] = lr_total
			wf_calculate_total_rc(lr_total_contract_amount)
			em_cntr_total.text = String(lr_total_contract_amount)
		END IF
		
end subroutine

public subroutine wf_set_zero ();//This function will set zeros to those values that are null
Integer li_num_rows, li_loop


//IF the data window for RC magazines is visible then set zeros
//to all the values that are null. This is done so that when
//cost are calculated for invoice tracking there will be no null values.
IF dw_magazine_cost_maintenance_rc.Visible = TRUE THEN
  li_num_rows = dw_magazine_cost_maintenance_rc.RowCount()	
   FOR li_loop = 1 TO li_num_rows
    IF IsNull(dw_magazine_cost_maintenance_rc.Object.ucmast[li_loop]) THEN
        dw_magazine_cost_maintenance_rc.Object.ucmast[li_loop] = 0
	 END IF
	 
	 IF IsNull(dw_magazine_cost_maintenance_rc.Object.ucdupls[li_loop]) THEN
        dw_magazine_cost_maintenance_rc.Object.ucdupls[li_loop] = 0
	 END IF
	 
	 IF IsNull(dw_magazine_cost_maintenance_rc.Object.ucdupl[li_loop]) THEN
        dw_magazine_cost_maintenance_rc.Object.ucdupl[li_loop] = 0
	 END IF
	 
	 IF IsNull(dw_magazine_cost_maintenance_rc.Object.ucdupll[li_loop]) THEN
        dw_magazine_cost_maintenance_rc.Object.ucdupll[li_loop] = 0
	 END IF
	 
	IF IsNull(dw_magazine_cost_maintenance_rc.Object.mag_ucothr[li_loop]) THEN
        dw_magazine_cost_maintenance_rc.Object.mag_ucothr[li_loop] = 0
	END IF
	 
	IF IsNull(dw_magazine_cost_maintenance_rc.Object.estmail[li_loop]) THEN
        dw_magazine_cost_maintenance_rc.Object.estmail[li_loop] = 0
	END IF
	 
  NEXT//
ELSEIF dw_magazine_cost_maintenance.Visible = TRUE THEN
	 li_num_rows = dw_magazine_cost_maintenance.RowCount()	
   FOR li_loop = 1 TO li_num_rows
		
		IF IsNull(dw_magazine_cost_maintenance.Object.mag_ucmast[li_loop]) THEN
			dw_magazine_cost_maintenance.Object.mag_ucmast[li_loop] = 0
		END IF
		
		IF IsNull(dw_magazine_cost_maintenance.Object.mag_ucdupl[li_loop]) THEN
			dw_magazine_cost_maintenance.Object.mag_ucdupl[li_loop] = 0
		END IF
	
	   IF IsNull(dw_magazine_cost_maintenance.Object.mag_ucothr[li_loop]) THEN
			dw_magazine_cost_maintenance.Object.mag_ucothr[li_loop] = 0
		END IF
	
	   IF IsNull(dw_magazine_cost_maintenance.Object.mag_estmail[li_loop]) THEN
			dw_magazine_cost_maintenance.Object.mag_estmail[li_loop] = 0
		END IF
	
	   	
	
  NEXT//
END IF//IF dw_magazine_cost_maintenance_rc.Visible = TRUE THEN
end subroutine

public function integer wf_check_contract (string ls_contract, integer li_fy, ref string ls_producer, ref string ls_format, ref real lr_cntr_total, ref string lc_ship_comp, ref string lc_inv_comp);  SELECT magcntr.format,   
         magcntr.prdr,
	  ship_completed,
	  invoice_completed,
         sum(mag.cntcsc)  
    INTO :ls_format,   
         :ls_producer,
	  :lc_ship_comp,
	  :lc_inv_comp,
         :lr_cntr_total
    FROM mag,   
         magcntr  
   WHERE  mag.fy = :li_fy                AND
          mag.fy = magcntr.fy   AND  
          mag.cntr = :ls_contract        AND
			 mag.cntr = magcntr.cntr
	GROUP BY magcntr.fy, magcntr.cntr, magcntr.prdr, magcntr.format,ship_completed,invoice_completed
   USING SqlServerTrans;

Return SqlServerTrans.SqlCode;




end function

on w_magazine_cost_maintenance.create
int iCurrent
call super::create
this.rb_inv_comp=create rb_inv_comp
this.rb_ship_comp=create rb_ship_comp
this.st_fy=create st_fy
this.st_contract=create st_contract
this.st_producer=create st_producer
this.st_format=create st_format
this.st_total=create st_total
this.dw_producer=create dw_producer
this.sle_format=create sle_format
this.em_cntr_total=create em_cntr_total
this.cb_exit=create cb_exit
this.cb_clear=create cb_clear
this.cb_update=create cb_update
this.cb_toggle=create cb_toggle
this.em_fy=create em_fy
this.sle_contract=create sle_contract
this.cb_print=create cb_print
this.st_records=create st_records
this.st_records_no=create st_records_no
this.dw_magazine_cost_maintenance=create dw_magazine_cost_maintenance
this.dw_magazine_cost_maintenance_nested=create dw_magazine_cost_maintenance_nested
this.dw_magazine_cost_maintenance_rc_nested=create dw_magazine_cost_maintenance_rc_nested
this.dw_magazine_cost_maintenance_rc=create dw_magazine_cost_maintenance_rc
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.rb_inv_comp
this.Control[iCurrent+2]=this.rb_ship_comp
this.Control[iCurrent+3]=this.st_fy
this.Control[iCurrent+4]=this.st_contract
this.Control[iCurrent+5]=this.st_producer
this.Control[iCurrent+6]=this.st_format
this.Control[iCurrent+7]=this.st_total
this.Control[iCurrent+8]=this.dw_producer
this.Control[iCurrent+9]=this.sle_format
this.Control[iCurrent+10]=this.em_cntr_total
this.Control[iCurrent+11]=this.cb_exit
this.Control[iCurrent+12]=this.cb_clear
this.Control[iCurrent+13]=this.cb_update
this.Control[iCurrent+14]=this.cb_toggle
this.Control[iCurrent+15]=this.em_fy
this.Control[iCurrent+16]=this.sle_contract
this.Control[iCurrent+17]=this.cb_print
this.Control[iCurrent+18]=this.st_records
this.Control[iCurrent+19]=this.st_records_no
this.Control[iCurrent+20]=this.dw_magazine_cost_maintenance
this.Control[iCurrent+21]=this.dw_magazine_cost_maintenance_nested
this.Control[iCurrent+22]=this.dw_magazine_cost_maintenance_rc_nested
this.Control[iCurrent+23]=this.dw_magazine_cost_maintenance_rc
end on

on w_magazine_cost_maintenance.destroy
call super::destroy
destroy(this.rb_inv_comp)
destroy(this.rb_ship_comp)
destroy(this.st_fy)
destroy(this.st_contract)
destroy(this.st_producer)
destroy(this.st_format)
destroy(this.st_total)
destroy(this.dw_producer)
destroy(this.sle_format)
destroy(this.em_cntr_total)
destroy(this.cb_exit)
destroy(this.cb_clear)
destroy(this.cb_update)
destroy(this.cb_toggle)
destroy(this.em_fy)
destroy(this.sle_contract)
destroy(this.cb_print)
destroy(this.st_records)
destroy(this.st_records_no)
destroy(this.dw_magazine_cost_maintenance)
destroy(this.dw_magazine_cost_maintenance_nested)
destroy(this.dw_magazine_cost_maintenance_rc_nested)
destroy(this.dw_magazine_cost_maintenance_rc)
end on

event key;call super::key;
GraphicObject which_control
SingleLineEdit sle_which
CommandButton cb_which
EditMask      em_which
string text_value

which_control = GetFocus( ) 


CHOOSE CASE TypeOf(which_control)

CASE SingleLineEdit!
	sle_which = which_control
	
  IF sle_which = sle_contract AND Key = KeyEnter! AND TRIM(sle_contract.text) = '' THEN
		

      dw_producer.SetFocus()
        

	END IF //sle_which = sle_producer
  

CASE EditMask!
	em_which = which_control
 IF  (em_which  = em_fy ) THEN

      IF  key = KeyEnter!  THEN
		 IF em_fy.text = '' OR em_fy.text = '0000' THEN	
         MessageBox('Error', 'Data Required')
         em_fy.SetFocus()
         Return
		 ELSE
			sle_contract.SetFocus()
		END IF
	  END IF //key = KeyEnter!  THEN
 END IF //(em_which  = em_fy ) THEN


CASE ELSE
	text_value = ""
END CHOOSE
end event

event open;call super::open;Integer li_fy
String  ls_contract

cb_clear.Enabled = TRUE
cb_toggle.Enabled = TRUE
cb_update.Enabled = TRUE

//If this screen was entered through w_magazine_maintenance then
// get the fiscal year, contract and set to the w_magazine_cost_maintenance screen
// and retrieve the data.
IF IsValid(w_magazine_maintenance) THEN
   li_fy = Integer(w_magazine_maintenance.em_fy.text)
   ls_contract = w_magazine_maintenance.sle_contract.text
	sle_contract.text = ls_contract
   em_fy.text = String(li_fy)
   sle_contract.TriggerEvent(modified!)
   close(w_magazine_maintenance)
ELSE
	cb_toggle.Enabled = FALSE
	cb_update.Enabled = FALSE
   em_fy.SetFocus()
END IF

THIS.windowstate = maximized!
end event

event pfc_preopen;call super::pfc_preopen;
this.of_SetPreference(TRUE)
this.inv_preference.of_SetToolBars(TRUE)

this.inv_preference.of_SetWindow(TRUE)

this.of_SetResize(TRUE)
this.inv_resize.of_setOrigSize(this.WorkSpaceWidth(), this.WorkSpaceHeight())


inv_resize.of_Register(dw_magazine_cost_maintenance,"scale")
inv_resize.of_Register(dw_magazine_cost_maintenance_nested,"scale")
inv_resize.of_Register(dw_magazine_cost_maintenance_rc,"scale")
inv_resize.of_Register(dw_magazine_cost_maintenance_rc_nested,"scale")
inv_resize.of_Register(dw_producer,"scale")
inv_resize.of_Register(cb_exit,"scale")
inv_resize.of_Register(cb_print,"scale")
inv_resize.of_Register(cb_toggle,"scale")
inv_resize.of_Register(cb_update,"scale")
inv_resize.of_Register(cb_clear,"scale")
inv_resize.of_Register(dw_producer,"scale")
inv_resize.of_Register(em_fy,"scale")
inv_resize.of_Register(em_cntr_total,"scale")
inv_resize.of_Register(sle_contract,"scale")
inv_resize.of_Register(sle_format,"scale")
inv_resize.of_Register(st_contract,"scale")
inv_resize.of_Register(st_format,"scale")
inv_resize.of_Register(st_fy,"scale")
inv_resize.of_Register(st_producer,"scale")
inv_resize.of_Register(st_total,"scale")
inv_resize.of_Register(st_records,"scale")
inv_resize.of_Register(st_records_no,"scale")
inv_resize.of_Register(rb_inv_comp,"scale")
inv_resize.of_Register(rb_ship_comp,"scale")




end event

event resize;call super::resize;Long ll_height

this.X = w_pics_main.X
this.Y = w_pics_main.Y

ll_height = w_pics_main.mdi_1.Height
this.Resize(w_pics_main.WorkSpaceWidth(), ll_height)

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

event mousedown;call super::mousedown;w_pics_main.setmicrohelp('Ready')
end event

type rb_inv_comp from checkbox within w_magazine_cost_maintenance
integer x = 2231
integer y = 96
integer width = 654
integer height = 80
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long backcolor = 67108864
string text = "Invoicing Completed"
end type

type rb_ship_comp from checkbox within w_magazine_cost_maintenance
integer x = 2231
integer y = 32
integer width = 654
integer height = 80
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long backcolor = 67108864
string text = "Shipping Completed"
end type

type st_fy from statictext within w_magazine_cost_maintenance
integer x = 37
integer y = 20
integer width = 411
integer height = 76
boolean bringtotop = true
integer textsize = -10
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long backcolor = 67108864
boolean enabled = false
string text = "Fiscal Years:"
boolean focusrectangle = false
end type

type st_contract from statictext within w_magazine_cost_maintenance
integer x = 485
integer y = 20
integer width = 247
integer height = 76
boolean bringtotop = true
integer textsize = -10
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long backcolor = 67108864
boolean enabled = false
string text = "Contract"
alignment alignment = center!
boolean focusrectangle = false
end type

type st_producer from statictext within w_magazine_cost_maintenance
integer x = 905
integer y = 20
integer width = 247
integer height = 76
boolean bringtotop = true
integer textsize = -10
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long backcolor = 67108864
boolean enabled = false
string text = "Producer"
alignment alignment = center!
boolean focusrectangle = false
end type

type st_format from statictext within w_magazine_cost_maintenance
integer x = 1243
integer y = 20
integer width = 247
integer height = 76
boolean bringtotop = true
integer textsize = -10
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long backcolor = 67108864
boolean enabled = false
string text = "Format"
alignment alignment = center!
boolean focusrectangle = false
end type

type st_total from statictext within w_magazine_cost_maintenance
integer x = 1710
integer y = 20
integer width = 219
integer height = 76
boolean bringtotop = true
integer textsize = -10
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long backcolor = 67108864
boolean enabled = false
string text = "Total"
alignment alignment = center!
boolean focusrectangle = false
end type

type dw_producer from u_pics_dw within w_magazine_cost_maintenance
event constructor pbm_constructor
event itemchanged pbm_dwnitemchange
integer x = 841
integer y = 96
integer width = 366
integer height = 96
integer taborder = 60
string dataobject = "ddw_magazine_changes_producer"
boolean vscrollbar = false
boolean border = false
boolean livescroll = false
borderstyle borderstyle = stylebox!
end type

event constructor;call super::constructor;SetTransObject(SqlServerTrans)
this.of_SetTransObject(SqlServerTrans)
This.of_setupdateable(FALSE)
this.of_SetDropDownSearch(TRUE)
this.inv_dropdownsearch.of_AddColumn("prdr")
dw_producer.Event pfc_addrow()









end event

event itemchanged;call super::itemchanged;String ls_producer, ls_format, ls_contract, ls_temp,lc_ship_comp,lc_inv_comp
Integer li_fy, li_num_rows
Real lr_cntr_total

ls_producer = dw_producer.GetText()
li_fy = Integer(em_fy.text)

IF NOT(IsNull(ls_producer)) OR Trim(ls_producer) <> '' THEN
 //If producer not in producer table then do not process.
   SELECT prdr
	INTO :ls_temp
   FROM producer
   where prdr = :ls_producer
   USING SqlServerTrans;
   IF ls_temp <> ls_producer THEN
		RETURN 1
   END IF

	IF sle_contract.text <> '' THEN
		sle_format.SetFocus()
	ELSE
		IF wf_check_producer(li_fy, ls_producer, li_num_rows) <> -1 THEN
			
		 IF li_num_rows = 0 THEN
			dw_producer.object.prdr.Validationmsg = "No Data Found. Enter New Producer"
			RETURN 1
		 ELSEIF li_num_rows = 1 THEN
			//One contract for the producer get format from magcntr
			IF wf_get_format(li_fy,ls_producer,ls_contract,ls_format) <> -1 THEN
				sle_format.text = ls_format
				sle_contract.text = ls_contract
				wf_check_contract(ls_contract,li_fy,ls_producer,ls_format,lr_cntr_total,lc_ship_comp,lc_inv_comp)
				em_cntr_total.text = String(lr_cntr_total)
				ls_prdr = ls_producer
				// Check to see if the flags ship completed or invoice completed are assigned to this contract
				IF lc_ship_comp = 'Y' THEN 
					rb_ship_comp.checked = TRUE
				ELSE
					rb_ship_comp.checked = FALSE
				END IF
					
				IF lc_inv_comp = 'Y' THEN 
					rb_inv_comp.checked = TRUE
				ELSE
					rb_inv_comp.checked = FALSE
				END IF
			//If the format is RC then make dw_magazine_maintenance_rc
			//visible.
			IF Trim(ls_format) = 'RC' THEN
				dw_magazine_cost_maintenance.visible = FALSE
				dw_magazine_cost_maintenance_rc.visible = TRUE
				dw_magazine_cost_maintenance_rc.Retrieve(li_fy, ls_contract) 
            			wf_disable_objects()
				dw_magazine_cost_maintenance_rc.Modify("magcd.Protect='1~tIf(IsRowNew(),0,1)'")
				dw_magazine_cost_maintenance_rc.SetFocus()
			ELSE
				//Enable and disable the objects, retrieve data and
				//set focus on the datawindow dw_magazine_cost_maintenance
				dw_magazine_cost_maintenance.visible = TRUE
				dw_magazine_cost_maintenance_rc.visible = FALSE
				dw_magazine_cost_maintenance.Retrieve(li_fy, ls_contract)
				wf_disable_objects()
				dw_magazine_cost_maintenance.Modify("mag_magcd.Protect='1~tIf(IsRowNew(),0,1)'")
				dw_magazine_cost_maintenance.SetFocus()
			END IF//IF TRIM(ls_format) = 'RC' THEN
		  END IF//IF wf_get_format(li_fy,ls_producer,ls_contract,ls_format) <> -1 THEN
		 ELSEIF li_num_rows > 1 THEN
			//Multiple contracts found for the producer
				 w_magazine_cost_maintenance.SetMicroHelp("Multiple Contracts Found For Producer")
				 Open(w_magazine_cost_maintenance_response)
				 ls_contract = ls_temp_contract
				 li_fy = Integer(em_fy.text)
				 
					SELECT format
					INTO   :ls_format
					FROM   magcntr
					where  cntr = :ls_contract AND prdr = :ls_producer AND fy = :li_fy
					USING SqlServerTrans;
					
					sle_format.text = ls_format
					sle_contract.text = ls_contract
				 
				 wf_check_contract(ls_contract,li_fy,ls_producer,ls_format,lr_cntr_total,lc_ship_comp,lc_inv_comp)
				 em_cntr_total.text = String(lr_cntr_total)
				 ls_prdr = ls_producer
				// Check to see if the flags ship completed or invoice completed are assigned to this contract
				IF lc_ship_comp = 'Y' THEN 
					rb_ship_comp.checked = TRUE
				ELSE
					rb_ship_comp.checked = FALSE
				END IF
					
				IF lc_inv_comp = 'Y' THEN 
					rb_inv_comp.checked = TRUE
				ELSE
					rb_inv_comp.checked = FALSE
				END IF
	
				//If the format is RC then make dw_magazine_maintenance_rc
				//visible.
				IF Trim(ls_format) = 'RC' THEN
					dw_magazine_cost_maintenance.visible = FALSE
					dw_magazine_cost_maintenance_rc.visible = TRUE
					dw_magazine_cost_maintenance_rc.Retrieve(li_fy, ls_contract)
					wf_disable_objects()
					dw_magazine_cost_maintenance_rc.Modify("magcd.Protect='1~tIf(IsRowNew(),0,1)'")
					dw_magazine_cost_maintenance_rc.SetFocus()
				ELSE//If format is BR or FD
				 //Enable objects and set focus on the datawindow
				 dw_magazine_cost_maintenance.visible = TRUE
				 dw_magazine_cost_maintenance_rc.visible = FALSE
				 dw_magazine_cost_maintenance.Retrieve(li_fy, ls_contract)
				 wf_disable_objects()
				 dw_magazine_cost_maintenance.Modify("mag_magcd.Protect='1~tIf(IsRowNew(),0,1)'")
				 dw_magazine_cost_maintenance.SetFocus()		
			  END IF//IF TRIM(ls_format) = 'RC' THEN
		  END IF// IF li_num_rows = 0 THEN
		 
		END IF// IF wf_check_producer(li_fy, ls_producer, li_num_rows) <> -1 THEN
	END IF// IF sle_contract.text <> '' THEN
	
END IF//IF NOT(IsNull(ls_producer)) OR TRIM(ls_producer) <> '' THEN

	
end event

event getfocus;call super::getfocus;w_pics_main.Event pfc_microhelp("Enter Producer Information")
end event

event editchanged;call super::editchanged;IF isValid(inv_dropdownsearch) THEN
	inv_dropdownsearch.Event pfc_EditChanged(row, dwo, data)
END IF
end event

event itemfocuschanged;call super::itemfocuschanged;IF isvalid(inv_dropdownsearch) THEN
	inv_dropdownsearch.Event pfc_ItemFocusChanged(row, dwo)
END IF

end event

type sle_format from u_sle within w_magazine_cost_maintenance
event ue_hint_text pbm_mousemove
string tag = "Format "
integer x = 1230
integer y = 100
integer width = 347
integer height = 88
integer taborder = 0
integer textsize = -10
textcase textcase = upper!
boolean displayonly = true
end type

event ue_hint_text;call super::ue_hint_text;w_pics_main.SetMicroHelp(this.tag)
end event

type em_cntr_total from editmask within w_magazine_cost_maintenance
event ue_hint_text pbm_mousemove
string tag = "Calculated Total Cost (Adds up all the Adjusted Total (with mail) Column)"
integer x = 1614
integer y = 100
integer width = 512
integer height = 88
boolean bringtotop = true
integer textsize = -10
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long backcolor = 12632256
textcase textcase = upper!
boolean displayonly = true
borderstyle borderstyle = stylelowered!
string mask = "$##,###,###.00"
string displaydata = "Ä"
end type

event ue_hint_text;//w_pics_main.SetMicroHelp(this.tag)
end event

type cb_exit from u_cb within w_magazine_cost_maintenance
event ue_hint_text pbm_mousemove
string tag = "Exit the Screen"
integer x = 2377
integer y = 1392
integer taborder = 0
integer textsize = -10
string text = "E&xit"
end type

event ue_hint_text;call super::ue_hint_text;w_pics_main.SetMicroHelp(this.tag)
end event

event clicked;call super::clicked;
//String ls_magcd
//Integer li_ans
//
////If fields have been modified then display message to save any changes
////else quit the window.
//
//IF dw_magazine_cost_maintenance.visible = TRUE THEN
//		IF dw_magazine_cost_maintenance.ModifiedCount() > 0 THEN
//			li_ans = MessageBox('Update','Do you Want to Save Any Changes',Question!,yesno!)
//			
//			IF li_ans = 1 THEN
//				cb_update.TriggerEvent(clicked!)
//				dw_magazine_cost_maintenance.Reset()
//				dw_producer.Reset()
//				
//				close(w_magazine_cost_maintenance)
//				m_pics_main.m_menu.popmenu(300,0)
//			ELSE
//				dw_magazine_cost_maintenance.Reset()
//				dw_producer.Reset()
//				
//				close(w_magazine_cost_maintenance)
//				m_pics_main.m_menu.popmenu(300,0)
//			END IF
//		ELSE
//			dw_magazine_cost_maintenance.Reset()
//			dw_producer.Reset()
//			
//			close(w_magazine_cost_maintenance)
//			m_pics_main.m_menu.popmenu(300,0)
//		END IF
//ELSEIF dw_magazine_cost_maintenance_rc.visible = TRUE THEN
//	   IF dw_magazine_cost_maintenance_rc.ModifiedCount() > 0 THEN
//			li_ans = MessageBox('Update','Do you Want to Save Any Changes',Question!,yesno!)
//			
//			IF li_ans = 1 THEN
//				cb_update.TriggerEvent(clicked!)
//				dw_magazine_cost_maintenance_rc.Reset()
//				dw_producer.Reset()
//				
//				close(w_magazine_cost_maintenance)
//				m_pics_main.m_menu.popmenu(300,0)
//			ELSE
//				dw_magazine_cost_maintenance_rc.Reset()
//				dw_producer.Reset()
//				close(w_magazine_cost_maintenance)
//				m_pics_main.m_menu.popmenu(300,0)
//			END IF
//		ELSE
//			dw_magazine_cost_maintenance_rc.Reset()
//			dw_producer.Reset()
//		
//			close(w_magazine_cost_maintenance)
//			m_pics_main.m_menu.popmenu(300,0)
//		END IF
//			
//END IF
parent.event pfc_close()

end event

type cb_clear from u_cb within w_magazine_cost_maintenance
event ue_hint_text pbm_mousemove
string tag = "Clears the screen for next input"
integer x = 1979
integer y = 1392
integer taborder = 0
integer textsize = -10
string text = "&Clear"
end type

event ue_hint_text;call super::ue_hint_text;w_pics_main.SetMicroHelp(this.tag)
end event

event clicked;call super::clicked;//Clear the edit masks and single line edits
em_cntr_total.text = ''
em_fy.text = ''
sle_contract.text = ''
sle_format.text = ''

//Disable the buttons
cb_clear.Enabled = FALSE
cb_toggle.Enabled = FALSE
cb_update.Enabled = FALSE

//set the edit mask, line edit and dw_producer
em_fy.DisplayOnly = FALSE
sle_contract.DisplayOnly = FALSE
dw_producer.Enabled = TRUE
dw_producer.Reset()
dw_producer.Event pfc_addrow()

//Make the dw_magazine_cost_maintenance not visible and the
// dw_magazine_cost_maintenance_rc visible.
dw_magazine_cost_maintenance_rc.Reset()
dw_magazine_cost_maintenance.Reset()
dw_magazine_cost_maintenance_rc.visible = FALSE
dw_magazine_cost_maintenance.visible = TRUE


//Set Focus to the edit mask
em_fy.SetFocus()


end event

type cb_update from u_cb within w_magazine_cost_maintenance
event ue_hint_text pbm_mousemove
string tag = "Update the Database"
integer x = 1563
integer y = 1392
integer taborder = 0
integer textsize = -10
string text = "&Update"
end type

event ue_hint_text;call super::ue_hint_text;w_pics_main.SetMicroHelp(this.tag)
end event

event clicked;call super::clicked;dw_producer.AcceptText()

//Update dw_magazine_cost_maintenance if it is visible else
//update dw_magazine_cost_maintenance_rc.
IF dw_magazine_cost_maintenance.Visible = TRUE THEN
	wf_set_zero() //set zero to null values
	dw_magazine_cost_maintenance.AcceptText()
	IF dw_magazine_cost_maintenance.modifiedcount() > 0 THEN
		IF dw_magazine_cost_maintenance.Update() = 1 THEN
			COMMIT USING SqlServerTrans;
		ELSE
			MessageBox('Error','Error on Updating .. Contact Your DBA')
			ROLLBACK USING SqlServerTrans;
		END IF
	END IF//IF dw_magazine_cost_maintenance.modifiedcount() > 0 THEN
	dw_magazine_cost_maintenance.SetFocus()
ELSEIF dw_magazine_cost_maintenance_rc.visible = TRUE THEN
 	wf_set_zero() //set zero to null values
	dw_magazine_cost_maintenance_rc.AcceptText()
	
	IF dw_magazine_cost_maintenance_rc.modifiedcount() > 0 THEN
		IF dw_magazine_cost_maintenance_rc.Update() = 1 THEN
			COMMIT USING SqlServerTrans;
		ELSE
			MessageBox('Error','Error on Updating .. Contact Your DBA')
			ROLLBACK USING SqlServerTrans;
		END IF
	END IF//IF dw_magazine_cost_maintenance.modifiedcount() > 0 THEN
	dw_magazine_cost_maintenance_rc.SetFocus()
END IF//IF dw_magazine_maintenance.Visible = TRUE THEN
end event

type cb_toggle from u_cb within w_magazine_cost_maintenance
event ue_hint_text pbm_mousemove
string tag = "Updates this screen and goes to the Magazine Maintenance Screen"
integer x = 27
integer y = 1392
integer width = 590
integer taborder = 0
integer textsize = -10
string text = "&Previous Screen ..."
end type

event ue_hint_text;call super::ue_hint_text;w_pics_main.SetMicroHelp(this.tag)
end event

event clicked;call super::clicked;//Update the data window.
w_magazine_cost_maintenance.Hide()
parent.st_records.visible =false
parent.st_records_no.visible =false
cb_update.TriggerEvent(clicked!)
dw_magazine_cost_maintenance.Reset()
dw_producer.Reset()


OpenSheet(w_magazine_maintenance, w_pics_main, 0, Original!)


////Close the screen
//dw_magazine_cost_maintenance.Reset()
//dw_producer.Reset()
//close(w_magazine_cost_maintenance)

end event

type em_fy from u_em within w_magazine_cost_maintenance
string tag = "Enter Fiscal Year"
integer x = 64
integer y = 100
integer width = 229
integer height = 88
integer taborder = 40
integer textsize = -10
alignment alignment = center!
maskdatatype maskdatatype = datemask!
string mask = "yyyy"
string displaydata = "D"
double increment = 0
string minmax = ""
end type

event getfocus;call super::getfocus;//highlight the edit mask
em_fy.Event pfc_selectall()
end event

event modified;call super::modified;//Enable the clear button
cb_clear.Enabled = TRUE
end event

type sle_contract from u_sle within w_magazine_cost_maintenance
string tag = "Enter Contract "
integer x = 361
integer y = 100
integer height = 88
integer taborder = 50
integer textsize = -10
textcase textcase = upper!
integer limit = 7
end type

event getfocus;call super::getfocus;w_pics_main.Event pfc_microhelp("Enter Contract Information")
end event

event modified;call super::modified;String  ls_contract,ls_producer,ls_format, ls_modstring,lc_ship_comp,lc_inv_comp
Integer li_fy, li_rtn_code, li_row_count
Real    lr_cntr_total
DEC     lr_total_contract_amount

ls_contract = sle_contract.text
li_fy = Integer(em_fy.text)

IF TRIM(sle_contract.text) <>'' THEN
//Call window function to check if the contract exists
li_rtn_code = wf_check_contract(ls_contract,li_fy,ls_producer,ls_format,lr_cntr_total,lc_ship_comp,lc_inv_comp) 
 
IF  li_rtn_code = -1 THEN//Database error .. message and return
	MessageBox('Error', 'Database Error .. Check With System Administrator')
	Return 0
 ELSEIF li_rtn_code = 100 THEN//No data found .. Enter new contract
   MessageBox('Information', 'No Data Found..Enter New Contract No. Or click Clear for New query')
	sle_contract.SetFocus()
 ELSEIF li_rtn_code = 0 THEN//Data exists
	//Set the producer, calculate the total and set it to the total field.
	dw_producer.Settext(ls_producer)
	sle_format.text   = ls_format
	em_cntr_total.text = String(lr_cntr_total)
	ls_prdr = ls_producer
	// Check to see if the flags ship completed or invoice completed are assigned to this contract
	IF lc_ship_comp = 'Y' THEN 
		rb_ship_comp.checked = TRUE
	ELSE
		rb_ship_comp.checked = FALSE
	END IF
					
	IF lc_inv_comp = 'Y' THEN 
		rb_inv_comp.checked = TRUE
	ELSE
		rb_inv_comp.checked = FALSE
	END IF
	
	//If the format is RC then set dw_magazine_cost_maitenance_rc as
	//visible else if format is BR or FD then set dw_magazine_cost_ma
	//intenance as visible.
	IF TRIM (ls_format) = 'RC' THEN
		dw_magazine_cost_maintenance.visible = FALSE
		dw_magazine_cost_maintenance_rc.visible = TRUE
		dw_magazine_cost_maintenance_rc.Retrieve(li_fy, ls_contract)
		li_row_count =dw_magazine_cost_maintenance_rc.Retrieve(li_fy,ls_contract)
		parent.st_records_no.text =string(li_row_count )
		parent.st_records_no.visible =true
		parent.st_records.visible =true
		wf_disable_objects()
		dw_magazine_cost_maintenance_rc.SetFocus()
	ELSE
		//Enable object, retrieve data and set focus on the datawindow
		dw_magazine_cost_maintenance.visible = TRUE
		dw_magazine_cost_maintenance_rc.visible = FALSE
		li_row_count =dw_magazine_cost_maintenance.Retrieve(li_fy,ls_contract)
		parent.st_records_no.text =string(li_row_count )
		parent.st_records_no.visible =true
		parent.st_records.visible =true
		wf_disable_objects()
		dw_magazine_cost_maintenance.SetFocus()
	END IF
	
	
 END IF//li_rtn_code = -1 THEN
ELSE
	dw_producer.SetFocus()
END IF //sle_contract.text <>'' THEN



end event

type cb_print from u_cb within w_magazine_cost_maintenance
integer x = 718
integer y = 1392
integer taborder = 20
integer textsize = -10
string text = "&Print"
end type

event clicked;call super::clicked;Integer li_fy
String  ls_contract, ls_producer, ls_total

//Get values into local variables
li_fy = Integer(em_fy.text)
ls_contract = TRIM(sle_contract.text)

ls_total =em_cntr_total.text
parent.dw_magazine_cost_maintenance_nested.object.total.text =ls_total
dw_magazine_cost_maintenance_nested.AcceptText() 
dw_producer.AcceptText()
ls_producer = dw_producer.GetText()
IF dw_magazine_cost_maintenance.VISIBLE =TRUE	THEN
	//three retrieve arguments for nested report
	dw_magazine_cost_maintenance_nested.SetTransObject(sqlservertrans )
	dw_magazine_cost_maintenance_nested.Retrieve(li_fy, ls_contract, ls_producer )
	//dw_magazine_cost_maintenance_nested.visible =true
	//messagebox('wait','just a minute')
	dw_magazine_cost_maintenance_nested.TriggerEvent('pfc_print' )
END IF
IF dw_magazine_cost_maintenance_rc.VISIBLE =TRUE	THEN
	//three retrieve arguments for nested report
	dw_magazine_cost_maintenance_rc_nested.SetTransObject(sqlservertrans )
	dw_magazine_cost_maintenance_rc_nested.Retrieve(li_fy, ls_contract, ls_producer )
	//dw_magazine_cost_maintenance_nested.visible =true
	//messagebox('wait','just a minute')
	dw_magazine_cost_maintenance_rc_nested.TriggerEvent('pfc_print' )
END IF
end event

type st_records from statictext within w_magazine_cost_maintenance
integer x = 82
integer y = 1304
integer width = 421
integer height = 76
boolean bringtotop = true
integer textsize = -10
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long backcolor = 79741120
boolean enabled = false
string text = "Total Records:"
boolean focusrectangle = false
end type

type st_records_no from statictext within w_magazine_cost_maintenance
integer x = 507
integer y = 1304
integer width = 247
integer height = 76
boolean bringtotop = true
integer textsize = -10
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long backcolor = 79741120
boolean enabled = false
boolean focusrectangle = false
end type

type dw_magazine_cost_maintenance from u_dw within w_magazine_cost_maintenance
event ue_enter_to_tab pbm_dwnprocessenter
event ue_hint_text pbm_mousemove
integer x = 27
integer y = 212
integer width = 2903
integer height = 1048
integer taborder = 80
string dataobject = "d_magazine_cost_maintenance"
boolean vscrollbar = false
boolean livescroll = false
end type

event ue_enter_to_tab;call super::ue_enter_to_tab;//sets the enter to follow the tab key navigation path
Send(Handle(this), 256,9, Long(0,0))
Return(1)
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

event itemchanged;call super::itemchanged;Decimal lr_cntcso, lr_cntcsadjc,  lr_estmail, lr_mastering_cost, lr_duplication_cost
Decimal lr_total, lr_total_contract_amount, lr_ucmast, lr_ucdupl
Integer li_estmin, li_estsubs, li_estsz, li_estiss

//messageBox('check','itemchanged event already happened' )
//If the unit cost mastering is changed then change the
//Contract Cost(without mail)
IF DWO.NAME = 'mag_ucmast' THEN
	wf_calculate_contract_cost(row)
END IF//IF DWO.NAME = 'mag_ucmast' THEN

//If the unit cost duplication is changed then change the
//contract cost (without mail)
IF DWO.NAME = 'mag_ucdupl' THEN
	wf_calculate_contract_cost(row)	
END IF//IF DWO.NAME = 'mag_ucdupl' THEN




//Adds the original contract cost and the cumulative adjusted contract cost and
//puts it into the cumulative contract cost.
IF DWO.NAME = 'mag_cntcso' THEN
		
	lr_cntcso = DEC(Data)
	IF IsNull(lr_cntcso) THEN lr_cntcso = 0
	lr_cntcsadjc = THIS.Object.mag_cntcsadjc[row]
	IF IsNull(lr_cntcsadjc) THEN lr_cntcsadjc = 0
	lr_estmail = THIS.Object.mag_estmail[row]
	IF IsNull(lr_estmail) THEN lr_estmail = 0
	lr_total = lr_cntcso  + lr_cntcsadjc + lr_estmail
	THIS.Object.mag_cntcsc[row] = lr_total
	wf_calculate_total(lr_total_contract_amount)
   em_cntr_total.text = String(lr_total_contract_amount)
END IF//IF DWO.NAME = 'mag_cntcso' THEN


//Do the same if estimated mail has changed.
IF DWO.NAME = 'mag_estmail' THEN
	lr_estmail = REAL(Data)
	IF IsNull(lr_estmail) THEN lr_estmail = 0
	lr_cntcso =  THIS.Object.mag_cntcso[row]
	IF IsNull(lr_cntcso) THEN lr_cntcso = 0
	lr_cntcsadjc = THIS.Object.mag_cntcsadjc[row]
	IF IsNull(lr_cntcsadjc) THEN lr_cntcsadjc = 0
	lr_total = lr_cntcso  + lr_cntcsadjc + lr_estmail
	THIS.Object.mag_cntcsc[row] = lr_total
	wf_calculate_total(lr_total_contract_amount)
	em_cntr_total.text = String(lr_total_contract_amount)
END IF//IF DWO.NAME = 'mag_estmail' THEN


end event

event constructor;call super::constructor;
this.of_settransobject(SqlServerTrans)
end event

event itemfocuschanged;call super::itemfocuschanged;
IF DWO.Name = "mag_magcd" THEN
 	this.Event pfc_selectall()
END IF


IF DWO.Name = "mag_ucmast" THEN
  	this.Event pfc_selectall()
END IF

IF DWO.Name = "mag_ucdupl" THEN
   this.Event pfc_selectall()
END IF

IF DWO.Name = "mag_ucothr" THEN
  	this.Event pfc_selectall()
END IF

IF DWO.Name = "mag_estmail" THEN
  	this.Event pfc_selectall()
END IF


IF DWO.Name = "mag_cntcso" THEN
  	this.Event pfc_selectall()
END IF



IF DWO.Name = "mag_cntcsc" THEN
  	this.Event pfc_selectall()
END IF













end event

event updateend;call super::updateend;//IF rows have been updated then show message.
IF rowsupdated > 0 THEN
	w_pics_main.event pfc_microhelp('Row(s) Updated Successfully')
END IF
end event

type dw_magazine_cost_maintenance_nested from u_dw within w_magazine_cost_maintenance
boolean visible = false
integer x = 27
integer y = 192
integer width = 2903
integer height = 1048
integer taborder = 10
string dataobject = "d_magazine_cost_maintenance_nested"
boolean vscrollbar = false
boolean border = false
boolean livescroll = false
borderstyle borderstyle = stylebox!
end type

type dw_magazine_cost_maintenance_rc_nested from u_dw within w_magazine_cost_maintenance
boolean visible = false
integer x = 27
integer y = 212
integer width = 2903
integer height = 1048
integer taborder = 70
string dataobject = "d_magazine_cost_maintenance_rc_nested"
boolean vscrollbar = false
boolean border = false
boolean livescroll = false
borderstyle borderstyle = stylebox!
end type

type dw_magazine_cost_maintenance_rc from u_dw within w_magazine_cost_maintenance
event ue_enter_to_tab pbm_dwnprocessenter
boolean visible = false
integer x = 27
integer y = 224
integer width = 2903
integer height = 1048
integer taborder = 30
string dataobject = "d_magazine_cost_maintenance_rc"
boolean vscrollbar = false
boolean livescroll = false
end type

event ue_enter_to_tab;call super::ue_enter_to_tab;//sets the enter to follow the tab key navigation path
Send(Handle(this), 256,9, Long(0,0))
Return(1)
end event

event constructor;call super::constructor;this.of_settransobject(SqlServerTrans)
end event

event itemfocuschanged;call super::itemfocuschanged;
IF DWO.Name = "ucmast" THEN
  	this.Event pfc_selectall()
END IF

IF DWO.Name = "ucdupl" THEN
   this.Event pfc_selectall()
END IF


IF DWO.Name = "ucdupls" THEN
   this.Event pfc_selectall()
END IF


IF DWO.Name = "ucdupll" THEN
   this.Event pfc_selectall()
END IF


IF DWO.Name = "mag_ucothr" THEN
  	this.Event pfc_selectall()
END IF

IF DWO.Name = "estmail" THEN
  	this.Event pfc_selectall()
END IF


IF DWO.Name = "cntcso" THEN
  	this.Event pfc_selectall()
END IF



IF DWO.Name = "cntcsc" THEN
  	this.Event pfc_selectall()
END IF

IF DWO.Name = "mag_estszs" THEN
  	this.Event pfc_selectall()
END IF

IF DWO.Name = "mag_estsz" THEN
  	this.Event pfc_selectall()
END IF

IF DWO.Name = "mag_estszl" THEN
  	this.Event pfc_selectall()
END IF




end event

event itemchanged;call super::itemchanged;Decimal lr_cntcso, lr_cntcsadjc,  lr_estmail, lr_mastering_cost, lr_duplication_cost
Decimal lr_total, lr_total_contract_amount, lr_ucmast
Decimal lr_ucdupl, lr_ucdupls, lr_ucdupll, lr_estsz, lr_estszs, lr_estszl
Integer li_estmin, li_estsubs, li_estsz, li_estszs, li_estszl, li_estiss

dw_magazine_cost_maintenance_rc.AcceptText()

//If the unit cost mastering is changed then change the Contract Cost(without mail)
IF DWO.NAME = 'ucmast' THEN
	wf_calculate_contract_cost_rc(row)
END IF//IF DWO.NAME = 'mag_ucmast' THEN


//IF the unit cost duplications were changed calculate the contract cost
IF DWO.NAME = 'ucdupls' OR DWO.NAME = 'ucdupl' OR DWO.NAME = 'ucdupll' THEN
   wf_calculate_contract_cost_rc(row)		
END IF

//Adds the original contract cost and the cumulative adjusted contract cost and
//puts it into the cumulative contract cost.
IF DWO.NAME = 'cntcso' THEN
		
	lr_cntcso = DEC(Data)
	IF IsNull(lr_cntcso) THEN lr_cntcso = 0
	lr_cntcsadjc = THIS.Object.cntcsadjc[row]
	IF IsNull(lr_cntcsadjc) THEN lr_cntcsadjc = 0
	lr_estmail = THIS.Object.estmail[row]
	IF IsNull(lr_estmail) THEN lr_estmail = 0
	lr_total = lr_cntcso  + lr_cntcsadjc + lr_estmail
	THIS.Object.cntcsc[row] = lr_total
	wf_calculate_total_rc(lr_total_contract_amount)
   em_cntr_total.text = String(lr_total_contract_amount)
END IF


//Do the same if estimated mail has changed.
IF DWO.NAME = 'estmail' THEN
	lr_estmail = REAL(Data)
	IF IsNull(lr_estmail) THEN lr_estmail = 0
	lr_cntcso =  THIS.Object.cntcso[row]
	IF IsNull(lr_cntcso) THEN lr_cntcso = 0
	lr_cntcsadjc = THIS.Object.cntcsadjc[row]
	IF IsNull(lr_cntcsadjc) THEN lr_cntcsadjc = 0
	lr_total = lr_cntcso  + lr_cntcsadjc + lr_estmail
	THIS.Object.cntcsc[row] = lr_total
	wf_calculate_total_rc(lr_total_contract_amount)
	em_cntr_total.text = String(lr_total_contract_amount)
END IF//IF DWO.NAME = 'estmail' THEN




end event


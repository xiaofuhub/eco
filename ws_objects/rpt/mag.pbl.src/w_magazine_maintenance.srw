$PBExportHeader$w_magazine_maintenance.srw
forward
global type w_magazine_maintenance from w_sheet
end type
type rb_inv_comp from checkbox within w_magazine_maintenance
end type
type rb_ship_comp from checkbox within w_magazine_maintenance
end type
type st_fy from statictext within w_magazine_maintenance
end type
type em_fy from u_em within w_magazine_maintenance
end type
type st_contract from statictext within w_magazine_maintenance
end type
type sle_contract from u_sle within w_magazine_maintenance
end type
type dw_producer from u_dw within w_magazine_maintenance
end type
type st_producer from statictext within w_magazine_maintenance
end type
type sle_format from u_sle within w_magazine_maintenance
end type
type st_format from statictext within w_magazine_maintenance
end type
type cb_toggle from u_cb within w_magazine_maintenance
end type
type cb_update from u_cb within w_magazine_maintenance
end type
type cb_clear from u_cb within w_magazine_maintenance
end type
type cb_exit from u_cb within w_magazine_maintenance
end type
type cb_deleterow from u_cb within w_magazine_maintenance
end type
type st_lccontract from u_st within w_magazine_maintenance
end type
type em_lccontract from u_em within w_magazine_maintenance
end type
type cb_producers from u_cb within w_magazine_maintenance
end type
type cb_print from u_cb within w_magazine_maintenance
end type
type st_records from statictext within w_magazine_maintenance
end type
type st_records_no from statictext within w_magazine_maintenance
end type
type dw_magazine_maintenance_rc from u_dw within w_magazine_maintenance
end type
type dw_magazine_maintenance_nested from u_dw within w_magazine_maintenance
end type
type dw_magazine_maintenance_rc_nested from u_dw within w_magazine_maintenance
end type
type dw_magazine_maintenance from u_dw within w_magazine_maintenance
end type
end forward

global type w_magazine_maintenance from w_sheet
integer x = 214
integer y = 221
integer width = 3127
integer height = 1900
string title = "Magazine Maintenance"
rb_inv_comp rb_inv_comp
rb_ship_comp rb_ship_comp
st_fy st_fy
em_fy em_fy
st_contract st_contract
sle_contract sle_contract
dw_producer dw_producer
st_producer st_producer
sle_format sle_format
st_format st_format
cb_toggle cb_toggle
cb_update cb_update
cb_clear cb_clear
cb_exit cb_exit
cb_deleterow cb_deleterow
st_lccontract st_lccontract
em_lccontract em_lccontract
cb_producers cb_producers
cb_print cb_print
st_records st_records
st_records_no st_records_no
dw_magazine_maintenance_rc dw_magazine_maintenance_rc
dw_magazine_maintenance_nested dw_magazine_maintenance_nested
dw_magazine_maintenance_rc_nested dw_magazine_maintenance_rc_nested
dw_magazine_maintenance dw_magazine_maintenance
end type
global w_magazine_maintenance w_magazine_maintenance

type variables
String ls_temp_contract, ls_prdr
boolean ib_dw_reset_br= false
boolean ib_dw_reset_rc =false
end variables

forward prototypes
public function boolean wf_check_duplicates (string ls_magcode)
public function integer wf_check_producer (integer li_fy, string ls_producer, ref integer li_num_rows)
public function integer wf_check_status (string ls_magcode, integer li_fy)
public function integer wf_get_title (string ls_magcode, ref string ls_title)
public function boolean wf_check_magazine_format (string ls_magcode)
public subroutine wf_update_rc ()
public function integer wf_get_format (integer li_fy, string ls_producer, ref string ls_contract, ref string ls_format, ref string ls_loc_number)
public subroutine wf_calculate_contract_cost_rc_mainten (integer ai_row)
public function integer wf_check_contract_passby_value (string ls_contract, integer li_fy, ref string ls_producer, ref string ls_format, string ls_loc_number)
public subroutine wf_update ()
public subroutine wf_calculate_contract_cost_maintenance (integer ai_row)
public function integer wf_put_ccd (long ll_row, string ls_magcode, ref long ll_ccd)
public function integer wf_check_contract (string ls_contract, integer li_fy, ref string ls_producer, ref string ls_format, ref string ls_loc_number, ref string lc_ship_comp, ref string lc_inv_comp)
end prototypes

public function boolean wf_check_duplicates (string ls_magcode);Integer li_row_count, li_loop
Boolean FOUND = FALSE



li_row_count = dw_magazine_maintenance.rowcount()


li_loop = 1
DO
	IF TRIM(ls_magcode) = dw_magazine_maintenance.Object.mag_magcd[li_loop] THEN
		FOUND = TRUE
	ELSE
		li_loop++
	END IF
		
LOOP UNTIL(FOUND = TRUE OR li_loop > li_row_count)
   
RETURN FOUND
end function

public function integer wf_check_producer (integer li_fy, string ls_producer, ref integer li_num_rows);//Function checks to see how many magazines assigned to each producer for a 
//specific fiscal year. Returns the number of rows.
//


SELECT count(*) 
INTO  :li_num_rows
FROM  magcntr
WHERE magcntr.fy =   :li_fy AND
		magcntr.prdr = :ls_producer
USING SqlServerTrans;

Return SqlServerTrans.Sqlcode
end function

public function integer wf_check_status (string ls_magcode, integer li_fy);String ls_magstatus


SELECT mag.magst
INTO   :ls_magstatus
FROM   mag
WHERE  mag.fy = :li_fy AND
		 mag.magcd = :ls_magcode AND
		 mag.magst = "A"
USING  SqlServerTrans;

RETURN SqlServerTrans.SqlCode;
	 
end function

public function integer wf_get_title (string ls_magcode, ref string ls_title);

SELECT magttl.title
INTO   :ls_title
FROM   magttl
WHERE  magttl.magcd = :ls_magcode
USING  SqlServerTrans;

RETURN SqlServerTrans.SqlCode
end function

public function boolean wf_check_magazine_format (string ls_magcode);//This function checks the magazine format against the format in the sle_format object
//If the format is does not correspond to the magazine code then a value of false is
//returned.

String ls_format


ls_format = TRIM(sle_format.text)

IF pos(ls_magcode, "4") = 4  THEN
	IF ls_format = 'FD' THEN 
		RETURN TRUE
	ELSE
		RETURN FALSE
	END IF//IF ls_format = 'FD' THEN 
ELSEIF pos(ls_magcode, "1") = 4  THEN
	IF ls_format = 'BR' THEN
	   RETURN TRUE
	ELSE
		RETURN FALSE
	END IF
ELSE
	   RETURN FALSE
END IF//IF pos(ls_magcode, "4") = 4  THEN
end function

public subroutine wf_update_rc ();Integer li_modified_count, li_loop, li_num_rows,li_fy, li_rtn_code, i
String  ls_contract, ls_producer, ls_format, ls_loc_number,lc_ship_comp,lc_inv_comp

//Get values into local variables
li_fy = Integer(em_fy.text)
ls_contract = Trim(sle_contract.text)
ls_format   = Trim(sle_format.text)
ls_loc_number = em_lccontract.text
li_num_rows =  dw_magazine_maintenance_rc.RowCount()

dw_magazine_maintenance_rc.AcceptText() 
dw_producer.AcceptText()
ls_producer = dw_producer.GetText()

 
//Delete the last row of the datawindow
IF  IsNull(dw_magazine_maintenance_rc.GetItemString(li_num_rows,"magcd")) THEN
	 dw_magazine_maintenance_rc.DeleteRow(li_num_rows)
END IF
li_num_rows =  dw_magazine_maintenance_rc.RowCount()
FOR i= 1 TO li_num_rows
	wf_calculate_contract_cost_rc_mainten(i)
NEXT
li_modified_count = dw_magazine_maintenance_rc.ModifiedCount()
//Update Records. Update the mag table. Check to see if the produce exists. If exists
// then update the magcntr table else insert a record.

IF li_modified_count > 0 THEN
 	IF dw_magazine_maintenance_rc.Update() = 1 THEN
		
	  li_rtn_code = wf_check_contract(ls_contract,li_fy,ls_producer,ls_format,ls_loc_number,lc_ship_comp,lc_inv_comp) 
	 // Check to see if the flags ship completed or invoice completed are assigned to this contract
	IF rb_ship_comp.checked = TRUE THEN 
		lc_ship_comp = 'Y' 
	ELSE
		lc_ship_comp = 'N' 
	END IF
						
	IF rb_inv_comp.checked = TRUE THEN 
		 lc_inv_comp = 'Y'
	ELSE
		 lc_inv_comp = 'N'
	END IF
	ls_loc_number = em_lccontract.text
	  
	  IF li_rtn_code = 0 THEN
	  	 UPDATE magcntr  
			SET fy = :li_fy,   
				cntr = :ls_contract,   
				prdr = :ls_producer,   
				format = :ls_format,  
				cntrlc = :ls_loc_number,
				ship_completed = :lc_ship_comp,
				invoice_completed = :lc_inv_comp
			 WHERE (magcntr.fy = :li_fy) AND  
				(magcntr.cntr = :ls_contract)   
			 USING SqlServerTrans;
    	 ELSEIF li_rtn_code = 100 THEN
			INSERT INTO magcntr  
				(fy,   
				  cntr,   
				  prdr,   
				  format, 
				  cntrlc,
				  prodcd,
				  ship_completed,
				  invoice_completed)  
				VALUES (:li_fy,   
				  :ls_contract,   
				  :ls_producer,   
				  :ls_format,   
				  :ls_loc_number,
				  NULL,
				  :lc_ship_comp,
				  :lc_inv_comp)
			USING SqlServerTrans;
	    END IF// IF li_rtn_code = 0 THEN

        IF SqlServerTrans.sqlCode = 0 THEN
		COMMIT USING SqlServerTrans;
	  ELSE
        	ROLLBACK USING SqlServerTrans;
	  END IF//IF SqlServerTrans.SqlCode = 0 THEN
	ELSE
	  ROLLBACK USING SqlServerTrans;
	END IF//IF dw_magazine_maintenance_rc.Update() = 1 THEN
END IF//IF li_modified_count > 0 THEN

end subroutine

public function integer wf_get_format (integer li_fy, string ls_producer, ref string ls_contract, ref string ls_format, ref string ls_loc_number);
SELECT format,cntr,cntrlc
INTO   :ls_format,
       :ls_contract,
		 :ls_loc_number
FROM   magcntr
WHERE  magcntr.fy = :li_fy AND magcntr.prdr = :ls_producer
USING SqlServerTrans;


Return SqlServertrans.sqlcode
end function

public subroutine wf_calculate_contract_cost_rc_mainten (integer ai_row);//This function is called by the itemchanged event in dw_magazine_cost_
//maintenance_rc screen. It calculates the contract cost when the unit cost
//mastering and the unit cost duplications are changed/input. The
//resultant value is placed in the cntcso field (which is the contract
//cost field). 

// Accept the text that was inputed.
dw_magazine_maintenance_rc.AcceptText()

Decimal lr_cntcso, lr_cntcsadjc,  lr_estmail, lr_mastering_cost, lr_duplication_cost
Decimal lr_total, lr_total_contract_amount, lr_ucmast
Decimal lr_ucdupl, lr_ucdupls, lr_ucdupll, lr_estsz, lr_estszs, lr_estszl
long li_estmin, li_estsubs, li_estsz, li_estszs, li_estszl, li_estiss

//Ge values into local variables
   lr_ucmast =  dw_magazine_maintenance_rc.Object.ucmast[ai_row]
	 IF IsNull(lr_ucmast) THEN lr_ucmast = 0
	lr_ucdupl  =  dw_magazine_maintenance_rc.Object.ucdupl[ai_row]
	 IF IsNull(lr_ucdupl) THEN lr_ucdupl = 0
	lr_ucdupls =  dw_magazine_maintenance_rc.Object.ucdupls[ai_row]
	 IF IsNull(lr_ucdupls) THEN lr_ucdupls = 0
	lr_ucdupll =  dw_magazine_maintenance_rc.Object.ucdupll[ai_row]
	 IF IsNull(lr_ucdupll) THEN lr_ucdupll = 0
	li_estmin =  dw_magazine_maintenance_rc.Object.estmin[ai_row]
	 IF IsNull(li_estmin) THEN li_estmin = 0
	li_estsubs = dw_magazine_maintenance_rc.Object.estsubs[ai_row]
	 IF IsNull(li_estsubs) THEN li_estsubs = 0
	li_estsz   = dw_magazine_maintenance_rc.Object.estsz[ai_row]
	 IF IsNull(li_estsz) THEN li_estsz = 0
	li_estszs   = dw_magazine_maintenance_rc.Object.estszs[ai_row]
	 IF IsNull(li_estszs) THEN li_estszs = 0
	li_estszl   = dw_magazine_maintenance_rc.Object.estszl[ai_row]
	 IF IsNull(li_estszl) THEN li_estszl = 0
	li_estiss  = dw_magazine_maintenance_rc.Object.estiss[ai_row]
	 IF IsNull(li_estiss) THEN li_estiss = 0
	
	//Calculate the mastering cost for FD's and RC's
	   lr_mastering_cost = lr_ucmast * li_estmin
		
	//Calculate the duplication cost for BR,FD,and RC's format
	   lr_duplication_cost = ((lr_ucdupls * li_estszs) + (lr_ucdupl * li_estsz) + (lr_ucdupll * li_estszl)) * li_estsubs
		
	
	//Set the calculated total value to contract cost original.
	//Calculate the adjusted total and display in em_cntr_total.
	   lr_cntcso = li_estiss * (lr_mastering_cost + lr_duplication_cost)
	   IF NOT(IsNull(lr_cntcso)) OR lr_cntcso <> 0 THEN
	    dw_magazine_maintenance_rc.Object.cntcso[ai_row] = lr_cntcso
		 	lr_cntcsadjc = dw_magazine_maintenance_rc.Object.cntcsadjc[ai_row]
			 IF IsNull(lr_cntcsadjc) THEN lr_cntcsadjc = 0
			lr_estmail = dw_magazine_maintenance_rc.Object.estmail[ai_row]
			 IF IsNull(lr_estmail) THEN lr_estmail = 0
			lr_total = lr_cntcso  + lr_cntcsadjc + lr_estmail
			dw_magazine_maintenance_rc.Object.cntcsc[ai_row] = lr_total
			dw_magazine_maintenance_rc.Object.cntcsadjc[ai_row]= lr_cntcsadjc
			dw_magazine_maintenance_rc.Object.estmail[ai_row]= lr_estmail
//			wf_calculate_total_rc(lr_total_contract_amount)
//			em_cntr_total.text = String(lr_total_contract_amount)
		END IF
		
end subroutine

public function integer wf_check_contract_passby_value (string ls_contract, integer li_fy, ref string ls_producer, ref string ls_format, string ls_loc_number);//Check to see if the contract exists for the contract and the specified fiscal year.
// called by sle_contract. Returns the producer and the format for the magazines.
SELECT magcntr.prdr, magcntr.format, magcntr.cntrlc
INTO   :ls_producer, :ls_format, :ls_loc_number
FROM   magcntr
WHERE  ((magcntr.fy = :li_fy) AND
       (magcntr.cntr = :ls_contract))
Group BY prdr,format, cntrlc
USING sqlservertrans;
       


Return sqlservertrans.sqlcode 
	
end function

public subroutine wf_update ();Integer li_modified_count, li_loop, li_num_rows,li_fy, li_rtn_code, i
String  ls_contract, ls_producer, ls_format, ls_loc_number,lc_ship_comp,lc_inv_comp

//Get values into local variables
li_fy = Integer(em_fy.text)
ls_contract = Trim(sle_contract.text)
ls_format   = Trim(sle_format.text)
ls_loc_number = em_lccontract.text
li_num_rows =  dw_magazine_maintenance.RowCount()

dw_magazine_maintenance.AcceptText() 
dw_producer.AcceptText()
ls_producer = dw_producer.GetText()
 

IF  IsNull(dw_magazine_maintenance.GetItemString(li_num_rows,"mag_magcd")) THEN
	 dw_magazine_maintenance.DeleteRow(li_num_rows)
END IF
li_num_rows =  dw_magazine_maintenance.RowCount()
FOR i =1 TO li_num_rows
	wf_calculate_contract_cost_maintenance(i)
NEXT
dw_magazine_maintenance.AcceptText()
li_modified_count = dw_magazine_maintenance.ModifiedCount()

//Update Records. Update the mag table. Check to see if the produce exists. If exists
// then update the magcntr table else insert a record.

IF li_modified_count > 0   THEN
 	IF dw_magazine_maintenance.Update() = 1 THEN
		COMMIT USING SqlServerTrans;
 		li_rtn_code = wf_check_contract_passby_value(ls_contract,li_fy,ls_producer,ls_format,ls_loc_number) 
		// Check to see if the flags ship completed or invoice completed are assigned to this contract
		IF rb_ship_comp.checked = TRUE THEN 
			lc_ship_comp = 'Y' 
		ELSE
			lc_ship_comp = 'N' 
		END IF
							
		IF rb_inv_comp.checked = TRUE THEN 
			 lc_inv_comp = 'Y'
		ELSE
			 lc_inv_comp = 'N'
		END IF
		ls_loc_number = em_lccontract.text
		
	 	IF li_rtn_code = 0 THEN
	 		UPDATE magcntr  
			SET fy = :li_fy,   
					cntr = :ls_contract,   
					prdr = :ls_producer,   
					format = :ls_format,
					cntrlc = :ls_loc_number,
					ship_completed = :lc_ship_comp,
					invoice_completed = :lc_inv_comp
			WHERE (magcntr.fy = :li_fy) AND  
					(magcntr.cntr = :ls_contract)   
			USING SqlServerTrans;
			IF SqlServerTrans.sqlCode = 0 THEN
				COMMIT USING SqlServerTrans;
			ELSE
				ROLLBACK USING SqlServerTrans;
			END IF//IF SqlServerTrans.SqlCode = 0 THEN
    	ELSEIF li_rtn_code = 100 THEN
			INSERT INTO magcntr  
				(fy,   
				  cntr,   
				  prdr,   
				  format, 
				  cntrlc,
				  prodcd,
				  ship_completed,
				  invoice_completed)  
				VALUES (:li_fy,   
				  :ls_contract,   
				  :ls_producer,   
				  :ls_format,   
				  :ls_loc_number,
				  NULL,
				  :lc_ship_comp,
				  :lc_inv_comp)
			USING SqlServerTrans;
			IF SqlServerTrans.sqlCode = 0 THEN
				COMMIT USING SqlServerTrans;
			ELSE
				ROLLBACK USING SqlServerTrans;
			END IF//IF SqlServerTrans.SqlCode = 0 THEN
	 	END IF// IF li_rtn_code = 0 THEN
	ELSE
  		ROLLBACK USING SqlServerTrans;
	END IF//IF dw_magazine_maintenance.Update() = 1 THEN
END IF//IF li_modified_count > 0 THEN
end subroutine

public subroutine wf_calculate_contract_cost_maintenance (integer ai_row);//This function is called by the itemchanged event in dw_magazine_cost_
//maintenance screen. It calculates the contract cost when the unit cost
//mastering and the unit cost duplications are changed/input. The
//resultant value is placed in the cntcso field (which is the contract
//cost field). 

// Accept the text that was inputed.
dw_magazine_maintenance.AcceptText()

Decimal lr_cntcso, lr_cntcsadjc,  lr_estmail, lr_mastering_cost, lr_duplication_cost
Decimal lr_total, lr_total_contract_amount, lr_ucmast, lr_ucdupl
long li_estmin, li_estsubs, li_estsz, li_estiss

   lr_ucmast =  dw_magazine_maintenance.Object.mag_ucmast[ai_row]
	 IF IsNull(lr_ucmast) THEN lr_ucmast = 0
	lr_ucdupl  =  dw_magazine_maintenance.Object.mag_ucdupl[ai_row]
	 IF IsNull(lr_ucdupl) THEN lr_ucdupl = 0
	li_estmin =  dw_magazine_maintenance.Object.mag_estmin[ai_row]
	 IF IsNull(li_estmin) THEN li_estmin = 0
	li_estsubs = dw_magazine_maintenance.Object.mag_estsubs[ai_row]
	 IF IsNull(li_estsubs) THEN li_estsubs = 0
	li_estsz   = dw_magazine_maintenance.Object.mag_estsz[ai_row]
	 IF IsNull(li_estsz) THEN li_estsz = 0
	li_estiss  = dw_magazine_maintenance.Object.mag_estiss[ai_row]
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
	    dw_magazine_maintenance.Object.mag_cntcso[ai_row] = lr_cntcso
		 	IF IsNull(lr_cntcso) THEN lr_cntcso = 0
			lr_cntcsadjc = dw_magazine_maintenance.Object.mag_cntcsadjc[ai_row]
			IF IsNull(lr_cntcsadjc) THEN lr_cntcsadjc = 0
			lr_estmail = dw_magazine_maintenance.Object.mag_estmail[ai_row]
			IF IsNull(lr_estmail) THEN lr_estmail = 0
			lr_total = lr_cntcso  + lr_cntcsadjc + lr_estmail
			dw_magazine_maintenance.Object.mag_cntcsc[ai_row] = lr_total
			dw_magazine_maintenance.Object.mag_cntcso[ai_row] = lr_cntcso
			dw_magazine_maintenance.Object.mag_cntcsadjc[ai_row]= lr_cntcsadjc 
			dw_magazine_maintenance.Object.mag_estmail[ai_row]= lr_estmail
//			wf_calculate_total(lr_total_contract_amount)
//			em_cntr_total.text = String(lr_total_contract_amount)
		END IF
		
end subroutine

public function integer wf_put_ccd (long ll_row, string ls_magcode, ref long ll_ccd);//This function would fill in CCD and CWD fields with the values from 
//last years CCD, CWD for that magazine.
Integer li_fy

//Check to see if CCD and CWD exists for that magazine for the last year.
li_fy = Integer(em_fy.text)
li_fy = li_fy - 1

  SELECT mag.ccd
    INTO :ll_ccd
    FROM mag  
   WHERE ( mag.fy = :li_fy ) AND  
         ( mag.magcd = :ls_magcode ) AND  
         ( mag.magst = 'A' ) 
 USING SqlServerTrans;


RETURN SqlServerTrans.SqlCode
  
 



end function

public function integer wf_check_contract (string ls_contract, integer li_fy, ref string ls_producer, ref string ls_format, ref string ls_loc_number, ref string lc_ship_comp, ref string lc_inv_comp);//Check to see if the contract exists for the contract and the specified fiscal year.
// called by sle_contract. Returns the producer and the format for the magazines.
SELECT magcntr.prdr, magcntr.format, magcntr.cntrlc,ship_completed,invoice_completed
INTO   :ls_producer, :ls_format, :ls_loc_number,:lc_ship_comp,:lc_inv_comp
FROM   magcntr
WHERE  ((magcntr.fy = :li_fy) AND
       (magcntr.cntr = :ls_contract))
Group BY prdr,format, cntrlc,ship_completed,invoice_completed
USING sqlservertrans;
       


Return sqlservertrans.sqlcode 
	
end function

on w_magazine_maintenance.create
int iCurrent
call super::create
this.rb_inv_comp=create rb_inv_comp
this.rb_ship_comp=create rb_ship_comp
this.st_fy=create st_fy
this.em_fy=create em_fy
this.st_contract=create st_contract
this.sle_contract=create sle_contract
this.dw_producer=create dw_producer
this.st_producer=create st_producer
this.sle_format=create sle_format
this.st_format=create st_format
this.cb_toggle=create cb_toggle
this.cb_update=create cb_update
this.cb_clear=create cb_clear
this.cb_exit=create cb_exit
this.cb_deleterow=create cb_deleterow
this.st_lccontract=create st_lccontract
this.em_lccontract=create em_lccontract
this.cb_producers=create cb_producers
this.cb_print=create cb_print
this.st_records=create st_records
this.st_records_no=create st_records_no
this.dw_magazine_maintenance_rc=create dw_magazine_maintenance_rc
this.dw_magazine_maintenance_nested=create dw_magazine_maintenance_nested
this.dw_magazine_maintenance_rc_nested=create dw_magazine_maintenance_rc_nested
this.dw_magazine_maintenance=create dw_magazine_maintenance
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.rb_inv_comp
this.Control[iCurrent+2]=this.rb_ship_comp
this.Control[iCurrent+3]=this.st_fy
this.Control[iCurrent+4]=this.em_fy
this.Control[iCurrent+5]=this.st_contract
this.Control[iCurrent+6]=this.sle_contract
this.Control[iCurrent+7]=this.dw_producer
this.Control[iCurrent+8]=this.st_producer
this.Control[iCurrent+9]=this.sle_format
this.Control[iCurrent+10]=this.st_format
this.Control[iCurrent+11]=this.cb_toggle
this.Control[iCurrent+12]=this.cb_update
this.Control[iCurrent+13]=this.cb_clear
this.Control[iCurrent+14]=this.cb_exit
this.Control[iCurrent+15]=this.cb_deleterow
this.Control[iCurrent+16]=this.st_lccontract
this.Control[iCurrent+17]=this.em_lccontract
this.Control[iCurrent+18]=this.cb_producers
this.Control[iCurrent+19]=this.cb_print
this.Control[iCurrent+20]=this.st_records
this.Control[iCurrent+21]=this.st_records_no
this.Control[iCurrent+22]=this.dw_magazine_maintenance_rc
this.Control[iCurrent+23]=this.dw_magazine_maintenance_nested
this.Control[iCurrent+24]=this.dw_magazine_maintenance_rc_nested
this.Control[iCurrent+25]=this.dw_magazine_maintenance
end on

on w_magazine_maintenance.destroy
call super::destroy
destroy(this.rb_inv_comp)
destroy(this.rb_ship_comp)
destroy(this.st_fy)
destroy(this.em_fy)
destroy(this.st_contract)
destroy(this.sle_contract)
destroy(this.dw_producer)
destroy(this.st_producer)
destroy(this.sle_format)
destroy(this.st_format)
destroy(this.cb_toggle)
destroy(this.cb_update)
destroy(this.cb_clear)
destroy(this.cb_exit)
destroy(this.cb_deleterow)
destroy(this.st_lccontract)
destroy(this.em_lccontract)
destroy(this.cb_producers)
destroy(this.cb_print)
destroy(this.st_records)
destroy(this.st_records_no)
destroy(this.dw_magazine_maintenance_rc)
destroy(this.dw_magazine_maintenance_nested)
destroy(this.dw_magazine_maintenance_rc_nested)
destroy(this.dw_magazine_maintenance)
end on

event key;call super::key;
GraphicObject which_control
SingleLineEdit sle_which
CommandButton cb_which
EditMask      em_which
DataWindow    dw_which
string text_value

which_control = GetFocus( ) 


CHOOSE CASE TypeOf(which_control)

CASE CommandButton!
	cb_which = which_control
	text_value = cb_which.Text

CASE SingleLineEdit!
	sle_which = which_control
	
  //IF focus is on sle_contract and the Return Key is depressed	
  IF sle_which = sle_contract AND Key = KeyEnter! THEN
	
	  dw_producer.SetFocus()             

	END IF //sle_which = sle_producer
  
  

CASE DataWindow!
     dw_which = which_control
      	
	

CASE EditMask!
	em_which = which_control
 IF  (em_which  = em_fy ) THEN

      //IF focus is on em.fy and Return Key is depressed
      IF  key = KeyEnter! OR key = KeyTab! THEN
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

event open;call super::open;//Check to see if cost maintenance window is open. If it is open 
//then get contract information and retrieve data from
//sle_contract modified event.
IF IsValid(w_magazine_cost_maintenance) THEN
	  
  sle_contract.text = w_magazine_cost_maintenance.sle_contract.text
  em_fy.text = w_magazine_cost_maintenance.em_fy.text
    
  sle_contract.TriggerEvent(modified!)
  close(w_magazine_cost_maintenance)
ELSE
  //Set Focus to the Fiscal Year object and disable the Update button, 
  // and the Next Screen Button, the delete row button, the datawindow
  em_fy.SetFocus()
  cb_update.Enabled = FALSE
  cb_toggle.Enabled = FALSE
  cb_print.Enabled = FALSE
  cb_deleterow.Enabled = FALSE
  dw_magazine_maintenance.Enabled = FALSE
  this.windowstate = maximized!
END IF




end event

event resize;call super::resize;long ll_height
This.X = w_pics_main.X
This.Y = w_pics_main.Y
ll_height = w_pics_main.mdi_1.Height
This.Resize(w_pics_main.WorkSpaceWidth(), ll_height)
end event

event pfc_preopen;call super::pfc_preopen;this.of_SetPreference(TRUE)
this.inv_preference.of_SetToolBars(TRUE)
this.inv_preference.of_SetWindow(TRUE)
this.of_SetResize(TRUE)
this.inv_resize.of_SetOrigSize(this.workspacewidth(),this.workspaceheight())

inv_resize.of_Register(dw_magazine_maintenance,"scale")
inv_resize.of_Register(dw_magazine_maintenance_nested,"scale")
inv_resize.of_Register(dw_magazine_maintenance_rc,"scale")
inv_resize.of_Register(dw_magazine_maintenance_rc_nested,"scale")
inv_resize.of_Register(dw_producer,"scale")
inv_resize.of_Register(cb_print,"scale")
inv_resize.of_Register(cb_clear,"scale")
inv_resize.of_Register(cb_exit,"scale")
inv_resize.of_Register(cb_toggle,"scale")
inv_resize.of_Register(cb_update,"scale")
inv_resize.of_Register(cb_producers,"scale")
inv_resize.of_Register(cb_deleterow,"scale")
inv_resize.of_Register(em_fy,"scale")
inv_resize.of_Register(sle_contract,"scale")
inv_resize.of_Register(sle_format,"scale")
inv_resize.of_Register(em_lccontract,"scale")
inv_resize.of_Register(st_lccontract,"scale")
inv_resize.of_Register(st_contract,"scale")
inv_resize.of_Register(st_format,"scale")
inv_resize.of_Register(st_fy,"scale")
inv_resize.of_Register(st_producer,"scale")
inv_resize.of_Register(st_records,"scale")
inv_resize.of_Register(st_records_no,"scale")
inv_resize.of_Register(rb_inv_comp,"scale")
inv_resize.of_Register(rb_ship_comp,"scale")

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
			
			wf_update()
						
			
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

type rb_inv_comp from checkbox within w_magazine_maintenance
integer x = 2231
integer y = 96
integer width = 608
integer height = 80
integer taborder = 90
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long backcolor = 67108864
string text = "Invoice Completed"
end type

type rb_ship_comp from checkbox within w_magazine_maintenance
integer x = 2231
integer y = 32
integer width = 535
integer height = 80
integer taborder = 80
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long backcolor = 67108864
string text = "Ship Completed"
end type

type st_fy from statictext within w_magazine_maintenance
integer x = 46
integer y = 16
integer width = 347
integer height = 76
boolean bringtotop = true
integer textsize = -10
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long backcolor = 67108864
boolean enabled = false
string text = "FY"
alignment alignment = center!
boolean focusrectangle = false
end type

type em_fy from u_em within w_magazine_maintenance
event ue_hint_text pbm_mousemove
string tag = "Enter Fiscal Year"
integer x = 87
integer y = 96
integer width = 229
integer height = 84
integer taborder = 30
integer textsize = -10
string text = "0000"
alignment alignment = center!
maskdatatype maskdatatype = datemask!
string mask = "yyyy"
string displaydata = ""
double increment = 0
string minmax = ""
end type

event ue_hint_text;call super::ue_hint_text;w_pics_main.setmicrohelp(This.tag)
end event

event getfocus;call super::getfocus;
w_pics_main.Event pfc_microhelp('Enter Fiscal Year')
em_fy.Event pfc_selectall()
end event

event modified;call super::modified;//Enable the clear button
cb_clear.Enabled = TRUE
end event

type st_contract from statictext within w_magazine_maintenance
integer x = 466
integer y = 16
integer width = 251
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

type sle_contract from u_sle within w_magazine_maintenance
event ue_hint_text pbm_mousemove
string tag = "Enter Contract Information "
integer x = 375
integer y = 96
integer height = 84
integer taborder = 40
integer textsize = -10
textcase textcase = upper!
integer limit = 7
end type

event ue_hint_text;call super::ue_hint_text;w_pics_main.setmicrohelp(This.tag)
end event

event modified;call super::modified;String  ls_contract,ls_producer,ls_format, ls_modstring, ls_loc_number,lc_ship_comp,lc_inv_comp
Integer li_fy,li_rtn_code, li_num_rows, li_row_count

ls_contract = sle_contract.text
li_fy = Integer(em_fy.text)

IF TRIM(sle_contract.text) <>'' THEN
//Call window function to check if the contract exists
   li_rtn_code = wf_check_contract(ls_contract,li_fy,ls_producer,ls_format,ls_loc_number,lc_ship_comp,lc_inv_comp) 
	
	//Messagebox("ship completed flag", lc_ship_comp)
 
		IF  li_rtn_code = -1 THEN //Database error .. return
			MessageBox('Error', 'Database Error .. Check With System Administrator')
			Return 0
		ELSEIF li_rtn_code = 100 THEN  //No data found .. Return 
			MessageBox('Information', 'No Data Found..proceed with Add or click Clear for New query')
			dw_producer.SetFocus()
		ELSEIF li_rtn_code = 0 THEN // Data found proceed..
			ls_prdr = ls_producer
			dw_producer.SetText(ls_producer)
			dw_producer.SetItem(1,"prdr",ls_producer)
			dw_producer.AcceptText()
			IF Not(IsNull(ls_loc_number)) THEN em_lccontract.text = ls_loc_number
			sle_format.text   = ls_format
			// Check to see if the flags ship completed or invoice completed are assigned to this contract
			IF lc_ship_comp = "Y" THEN 
				rb_ship_comp.checked = TRUE
			ELSE
				rb_ship_comp.checked = FALSE
			END IF
					
			IF lc_inv_comp = "Y" THEN 
				rb_inv_comp.checked = TRUE
			ELSE
				rb_inv_comp.checked = FALSE
			END IF
			
			//IF the format is 'RC' THEN make dw_magazine_maintenance_rc
			//visible and retrieve the data. Else retrieve data using 
			//dw_magazine_maintenance.
			IF TRIM(ls_format) = 'RC' THEN
				dw_magazine_maintenance.visible = FALSE
			       dw_magazine_maintenance_rc.visible = TRUE
				dw_magazine_maintenance_rc.Retrieve(li_fy, ls_contract)
				li_row_count =dw_magazine_maintenance_rc.rowcount()
				parent.st_records_no.text = string (li_row_count)
				parent.st_records_no.visible =true
				parent.st_records.visible =true
				dw_magazine_maintenance_rc.Enabled = TRUE
				dw_magazine_maintenance_rc.Event pfc_addrow()
				li_num_rows = dw_magazine_maintenance_rc.rowcount()
				dw_magazine_maintenance_rc.ScrollToRow(li_num_rows)
				dw_magazine_maintenance_rc.SetFocus()
				dw_magazine_maintenance_rc.SetColumn("magcd")
				dw_magazine_maintenance_rc.Modify("magcd.protect='1~tIf(IsRowNew(),0,1)'")
			ELSE//if format is BR or FD
			       dw_magazine_maintenance.visible = TRUE
			       dw_magazine_maintenance_rc.visible = FALSE
				dw_magazine_maintenance.Retrieve(li_fy, ls_contract)
				li_row_count =dw_magazine_maintenance.RowCount()
				parent.st_records_no.text = string (li_row_count)
				parent.st_records_no.visible =true
				parent.st_records.visible =true
				dw_magazine_maintenance.Enabled = TRUE
				dw_magazine_maintenance.Event pfc_addrow()
				li_num_rows = dw_magazine_maintenance.RowCount()
				dw_magazine_maintenance.ScrollToRow(li_num_rows)
				dw_magazine_maintenance.SetFocus()
				dw_magazine_maintenance.SetColumn("mag_magcd")
				dw_magazine_maintenance_rc.Modify("magcd.protect='1~tIf(IsRowNew(),0,1)'")
			END IF//IF TRIM(ls_format) = 'RC' THEN
							
			//Enable and disable objects
			cb_toggle.Enabled = TRUE
			cb_print.Enabled = TRUE
			cb_update.Enabled = TRUE
			
		END IF//li_rtn_code = -1 THEN
ELSE
 //sle_producer.SetFocus()
 dw_producer.SetFocus()
END IF //sle_contract.text <>'' THEN
end event

event getfocus;call super::getfocus;sle_contract.Event pfc_selectall()
w_pics_main.Event pfc_microhelp('Enter Contract or Press Return For Producer')
end event

type dw_producer from u_dw within w_magazine_maintenance
event ue_hint_text pbm_mousemove
string tag = "Enter Producer Information"
integer x = 846
integer y = 96
integer width = 357
integer height = 84
integer taborder = 50
string dataobject = "ddw_magazine_changes_producer"
boolean vscrollbar = false
boolean livescroll = false
end type

event ue_hint_text;call super::ue_hint_text;w_pics_main.setmicrohelp(This.tag)
end event

event constructor;call super::constructor;this.of_SetTransObject(SqlServerTrans)
This.of_setupdateable(FALSE)
this.of_SetDropDownSearch(TRUE)
this.inv_dropdownsearch.of_AddColumn("prdr")
dw_producer.Event pfc_addrow()
end event

event itemchanged;call super::itemchanged;//This event checks to see if contract exists for the producer of the magazines.
//It checks for single or multiple contracts for the producer.
String ls_producer, ls_format, ls_contract, ls_loc_number,ls_temp,lc_ship_comp,lc_inv_comp
Integer li_fy, li_num_rows, li_num_rows_retrieved

ls_prdr = Data
ls_producer = dw_producer.GetText()
li_fy = Integer(em_fy.text)

//Check to see if data has been entered in the producer field.
IF Data <> "" THEN
   SELECT prdr
	INTO :ls_temp
	FROM producer
	where prdr = :ls_producer
	USING SqlServerTrans;
	IF ls_temp <> ls_producer THEN
		RETURN 1
	END IF

	IF Trim(sle_contract.text) <> '' THEN
		em_lccontract.SetFocus()
		em_lccontract.displayOnly = FALSE
//		sle_format.SetFocus()
//		sle_format.DisplayOnly = FALSE
	ELSE
		//Check the number of magazines for the given producer in the specific fiscal year.
		IF wf_check_producer(li_fy, ls_producer, li_num_rows) <> -1 THEN
			
		 IF li_num_rows = 0 THEN //If No Rows found .. error message and return
			dw_producer.object.prdr.Validationmsg = "No Data Found. Enter New Producer"
			dw_producer.EVENT pfc_selectall()
			RETURN 1
		 ELSEIF li_num_rows = 1 THEN //If only one row was found
			//Get format from magcntr
			IF wf_get_format(li_fy,ls_producer,ls_contract,ls_format,ls_loc_number) <> -1 THEN
				sle_format.text = ls_format
				sle_contract.text = ls_contract
				em_lccontract.text = ls_loc_number
				
				//If 'RC' Format then make dw_magazine_maintenance datawindow
				//not visible and make dw_magazine_maintenance_rc visible.
				IF Trim(ls_format) = 'RC' THEN
	            		   	dw_magazine_maintenance.visible = FALSE
				   	dw_magazine_maintenance_rc.visible = TRUE
				   	dw_magazine_maintenance_rc.Retrieve(li_fy, ls_contract)
				   	dw_magazine_maintenance_rc.Modify("magcd.Protect='1~tIf(IsRowNew(),0,1)'")
				   	dw_magazine_maintenance_rc.enabled = TRUE
				   	dw_magazine_maintenance_rc.EVENT pfc_addrow()
				   	li_num_rows_retrieved = dw_magazine_maintenance_rc.RowCount()
				   	dw_magazine_maintenance_rc.ScrollToRow(li_num_rows_retrieved)
				   	dw_magazine_maintenance_rc.SetColumn("magcd")
				   	dw_magazine_maintenance_rc.SetFocus()
				ELSE //if format is FD or BR
					dw_magazine_maintenance.visible = TRUE
					dw_magazine_maintenance_rc.visible = FALSE
					dw_magazine_maintenance.Retrieve(li_fy, ls_contract)
				   	dw_magazine_maintenance.Modify("mag_magcd.Protect='1~tIf(IsRowNew(),0,1)'")	
					dw_magazine_maintenance.enabled = TRUE
					dw_magazine_maintenance.EVENT pfc_addrow()
					li_num_rows_retrieved = dw_magazine_maintenance.RowCount()
					dw_magazine_maintenance.ScrollToRow(li_num_rows_retrieved)
					dw_magazine_maintenance.SetColumn("mag_magcd")
					dw_magazine_maintenance.SetFocus()					
				END IF//IF TRIM(ls_format) = 'RC' THEN
									
				//Enable and disable buttons.
				
				cb_toggle.enabled = TRUE
				cb_update.enabled = TRUE
			END IF//IF wf_get_format(li_fy,ls_producer,ls_contract,ls_format) <> -1 THEN
			
		 ELSEIF li_num_rows > 1 THEN //If more than one row then pop up response window and let user choose the contract.
			//Multiple contracts found for the producer
				 w_magazine_maintenance.SetMicroHelp("Multiple Contracts Found For Producer")
				 Open(w_magazine_maintenance_response)
				 ls_contract = ls_temp_contract
				 li_fy = Integer(em_fy.text)
				 
				 //Get the format for the magazines. Each contract has a single format.
					SELECT format,cntrlc,ship_completed,invoice_completed
					INTO   :ls_format, :ls_loc_number,:lc_ship_comp,:lc_inv_comp
					FROM   magcntr
					where  cntr = :ls_contract AND prdr = :ls_producer AND fy = :li_fy
					USING SqlServerTrans;
					
					sle_format.text = ls_format
					sle_contract.text = ls_contract
					em_lccontract.text = ls_loc_number
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
					
					
					//If 'RC' Format then make dw_magazine_maintenance datawindow
				  //not visible and make dw_magazine_maintenance_rc visible.
					IF Trim(ls_format) = 'RC' THEN
						dw_magazine_maintenance.visible = FALSE
						dw_magazine_maintenance_rc.visible = TRUE
						dw_magazine_maintenance_rc.Retrieve(li_fy, ls_contract)
						dw_magazine_maintenance_rc.Modify("magcd.Protect='1~tIf(IsRowNew(),0,1)'")
						dw_magazine_maintenance_rc.enabled = TRUE
						dw_magazine_maintenance_rc.EVENT pfc_addrow()
						li_num_rows_retrieved = dw_magazine_maintenance_rc.RowCount()
						dw_magazine_maintenance_rc.ScrollToRow(li_num_rows_retrieved)
						dw_magazine_maintenance_rc.SetColumn("magcd")
						dw_magazine_maintenance_rc.SetFocus()
					ELSE//If format is BR or FD
					   	dw_magazine_maintenance.visible = TRUE
						dw_magazine_maintenance_rc.visible = FALSE
					   	dw_magazine_maintenance.Retrieve(li_fy, ls_contract)
				      		dw_magazine_maintenance.Modify("mag_magcd.Protect='1~tIf(IsRowNew(),0,1)'")	
						dw_magazine_maintenance.enabled = TRUE
						dw_magazine_maintenance.EVENT pfc_addrow()
						li_num_rows_retrieved = dw_magazine_maintenance.RowCount()
						dw_magazine_maintenance.ScrollToRow(li_num_rows_retrieved)
						dw_magazine_maintenance.SetColumn("mag_magcd")
						dw_magazine_maintenance.SetFocus()
					END IF//	IF TRIM(ls_format) = 'RC' THEN
						
				 //Enable and disable buttons
					cb_toggle.enabled = TRUE		
					cb_update.enabled = TRUE				
					 
		 END IF// IF li_num_rows = 0 THEN
		 
		END IF// IF wf_check_producer(li_fy, ls_producer, li_num_rows) <> -1 THEN
	
	END IF// IF sle_contract.text <> '' THEN
	
END IF//IF TRIM(sle_contract.text) <> '' THEN
end event

event getfocus;call super::getfocus;
w_pics_main.Event pfc_microhelp('Enter Producer')
end event

event editchanged;call super::editchanged;IF isValid(inv_dropdownsearch) THEN
	inv_dropdownsearch.Event pfc_EditChanged(row, dwo, data)
END IF
end event

event itemfocuschanged;call super::itemfocuschanged;IF isvalid(inv_dropdownsearch) THEN
	inv_dropdownsearch.Event pfc_ItemFocusChanged(row, dwo)
END IF


end event

type st_producer from statictext within w_magazine_maintenance
integer x = 910
integer y = 16
integer width = 265
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

type sle_format from u_sle within w_magazine_maintenance
event ue_hint_text pbm_mousemove
string tag = "Enter Format "
integer x = 1842
integer y = 96
integer width = 297
integer height = 84
integer taborder = 70
integer textsize = -10
textcase textcase = upper!
boolean displayonly = true
end type

event ue_hint_text;call super::ue_hint_text;w_pics_main.setmicrohelp(This.tag)
end event

event modified;call super::modified;
//IF the format is FD or BR then enable the dw_magazine_maintenance
//datawindow and set focus to it. IF the format is RC then make the
//dw_magazine_maintenance_rc datawindow visible and set focus to it.
IF TRIM(sle_format.text) = 'FD' OR TRIM(sle_format.text) = 'BR' THEN
	dw_magazine_maintenance.ENABLED = TRUE
	dw_magazine_maintenance.visible = TRUE
	dw_magazine_maintenance_rc.visible = FALSE
	dw_magazine_maintenance.SetFocus()
ELSEIF TRIM(sle_format.text) = 'RC' THEN
	dw_magazine_maintenance_rc.ENABLED = TRUE
	dw_magazine_maintenance_rc.Event pfc_addrow()
	dw_magazine_maintenance.visible = FALSE
	dw_magazine_maintenance_rc.visible = TRUE
	dw_magazine_maintenance_rc.SetFocus()
ELSE
	MessageBox('Error','Invalid Format .. Valid Formats are BR, FD and RC')
	sle_format.text = ''
	sle_format.SetFocus()
END IF//IF TRIM(sle_format.text) = 'FD' OR TRIM(sle_format.text) = 'BR' THEN
end event

event getfocus;call super::getfocus;String ls_producer

//ls_producer = dw_producer.GetItemString(1,"prdr")
//MessageBox('data',ls_producer)
//


IF IsNull(ls_prdr) OR Trim(ls_prdr) = '' THEN
	MessageBox('Producer','No producer Information')
	dw_producer.SetFocus()
END IF

end event

type st_format from statictext within w_magazine_maintenance
integer x = 1838
integer y = 16
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

type cb_toggle from u_cb within w_magazine_maintenance
event ue_hint_text pbm_mousemove
string tag = "Updates this screen and goes to the Cost Maintenance Screen.."
integer x = 32
integer y = 1248
integer width = 421
integer taborder = 0
integer textsize = -10
string text = "&Next Screen .."
end type

event ue_hint_text;call super::ue_hint_text;w_pics_main.setmicrohelp(This.tag)
end event

event clicked;call super::clicked;//Update the datawindow that is visible and open the cost maintenance
//screen.

parent.st_records.visible =false
parent.st_records_no.visible =false
IF dw_magazine_maintenance.Visible = TRUE THEN
	
	wf_update()
	dw_producer.Reset()
	ib_dw_reset_br =true
	dw_magazine_maintenance.Reset()
	w_magazine_maintenance.Hide()
	OpenSheet(w_magazine_cost_maintenance, w_pics_main, 0, Original!)
	
ELSEIF dw_magazine_maintenance_rc.Visible = TRUE THEN
	
	wf_update_rc()
	dw_producer.Reset()
	ib_dw_reset_rc =true
	dw_magazine_maintenance_rc.Reset()
	w_magazine_maintenance.Hide()
	OpenSheet(w_magazine_cost_maintenance, w_pics_main, 0, Original!)
END IF





end event

type cb_update from u_cb within w_magazine_maintenance
event ue_hint_text pbm_mousemove
string tag = "Updates the Database"
integer x = 1458
integer y = 1248
integer width = 265
integer taborder = 0
boolean bringtotop = true
integer textsize = -10
string text = "&Update"
end type

event ue_hint_text;call super::ue_hint_text;w_pics_main.setmicrohelp(This.tag)
end event

event clicked;call super::clicked;Integer  li_rtn_code, li_num_rows, li_fy
String   ls_contract, ls_producer, ls_format

li_fy       = Integer(em_fy.text)
ls_contract = TRIM(sle_contract.text)
ls_producer = dw_producer.GetItemString(1,"prdr")
ls_format   = TRIM(sle_format.text)


//Call the update function wf_update(), which updates the datawindow.
//Add a new row to the data window and scroll to that row and set focus on magcode column

IF dw_magazine_maintenance.Visible = TRUE THEN
	wf_update()
	dw_magazine_maintenance.Event pfc_addrow()
	li_num_rows = dw_magazine_maintenance.rowcount()
	dw_magazine_maintenance.ScrollToRow(li_num_rows)
	dw_magazine_maintenance.SetFocus()
	dw_magazine_maintenance.SetColumn("mag_magcd")

ELSEIF dw_magazine_maintenance_rc.Visible = TRUE THEN
	wf_update_rc()
	dw_magazine_maintenance_rc.Event pfc_addrow()
	li_num_rows = dw_magazine_maintenance_rc.rowcount()
	dw_magazine_maintenance_rc.ScrollToRow(li_num_rows)
	dw_magazine_maintenance_rc.SetFocus()
	dw_magazine_maintenance_rc.SetColumn("magcd")

END IF//IF dw_magazine_maintenance.Visible = TRUE THEN
















end event

type cb_clear from u_cb within w_magazine_maintenance
event ue_hint_text pbm_mousemove
string tag = "Clears screen for Input"
integer x = 2208
integer y = 1248
integer taborder = 0
boolean bringtotop = true
integer textsize = -10
string text = "&Clear"
end type

event ue_hint_text;call super::ue_hint_text;w_pics_main.setmicrohelp(This.tag)
end event

event clicked;call super::clicked;
//Enable window objects
em_fy.DisplayOnly = FALSE
sle_contract.DisplayOnly = FALSE
em_lccontract.DisplayOnly = FALSE
sle_format.DisplayOnly = TRUE
dw_producer.Enabled = TRUE


//Disable buttons and datawindows
cb_clear.Enabled = False
cb_toggle.Enabled = False
cb_update.Enabled = False
cb_deleterow.Enabled = FALSE
dw_magazine_maintenance.Enabled = FALSE
dw_magazine_maintenance_rc.Enabled = FALSE


em_fy.text = ''
sle_contract.text = ''
sle_format.text = ''
em_lccontract.text = ''

//Reset the datawindows and hide dw_magazine_maintenance_rc and show
//dw_magazine_maintenance.
dw_producer.Reset()
dw_producer.Event pfc_addrow()

dw_magazine_maintenance.Reset()
dw_magazine_maintenance.Event pfc_addrow()
dw_magazine_maintenance.Visible = TRUE

dw_magazine_maintenance_rc.Reset()
dw_magazine_maintenance_rc.Event pfc_addrow()
dw_magazine_maintenance_rc.Visible = FALSE

em_fy.SetFocus()

end event

type cb_exit from u_cb within w_magazine_maintenance
event ue_hint_text pbm_mousemove
string tag = "Exits the screen"
integer x = 2610
integer y = 1248
integer width = 302
integer taborder = 0
boolean bringtotop = true
integer textsize = -10
string text = "E&xit"
end type

event ue_hint_text;call super::ue_hint_text;w_pics_main.setmicrohelp(This.tag)
end event

event clicked;call super::clicked;Integer li_answer
String ls_magcd

//If any fields have been modified then print message for updating
//else quit window.parent.event pfc_close()


IF dw_magazine_maintenance.Visible = Enabled THEN

	IF dw_magazine_maintenance.ModifiedCount() > 0 THEN
			ls_magcd = dw_magazine_maintenance.GetItemString(1,"mag_magcd")
			IF IsNull(ls_magcd) OR TRIM(ls_magcd) = '' OR TRIM(ls_magcd) = '0000' THEN
				dw_magazine_maintenance.Reset()
				dw_producer.Reset()
				parent.event pfc_close()
				 m_pics_main.m_menu.PopMenu ( 300, 0 ) 
			ELSE
			li_answer = MessageBox('Exit','Do You Want to Save Changes',Question!, yesno!)
			
			IF li_answer = 1 THEN
				wf_update()
				dw_magazine_maintenance.Reset()
				dw_producer.Reset()
				parent.event pfc_close()
				m_pics_main.m_menu.PopMenu ( 300, 0 ) 
			ELSE
				dw_magazine_maintenance.Reset()
				dw_producer.Reset()
				parent.event pfc_close()
				m_pics_main.m_menu.PopMenu ( 300, 0 ) 
			END IF
			END IF
			
	ELSE
		//reset the datawindows, close parent and pop up the menu
		dw_magazine_maintenance.Reset()
		dw_producer.Reset()
		parent.event pfc_close()
		m_pics_main.m_menu.PopMenu ( 300, 0 ) 
	END IF//IF dw_magazine_maintenance.ModifiedCount() > 0 THEN
	
ELSE //dw_magazine_maintenance_rc is visible
	IF dw_magazine_maintenance_rc.ModifiedCount() > 0 THEN
			ls_magcd = dw_magazine_maintenance_rc.GetItemString(1,"magcd")
			IF IsNull(ls_magcd) OR TRIM(ls_magcd) = '' OR TRIM(ls_magcd) = '0000' THEN
				dw_magazine_maintenance_rc.Reset()
				dw_producer.Reset()
				parent.event pfc_close()
				 m_pics_main.m_menu.PopMenu ( 300, 0 ) 
			ELSE
			li_answer = MessageBox('Exit','Do You Want to Save Changes',Question!, yesno!)
			
			IF li_answer = 1 THEN
				wf_update_rc()
				dw_magazine_maintenance_rc.Reset()
				dw_producer.Reset()
				parent.event pfc_close()
				m_pics_main.m_menu.PopMenu ( 300, 0 ) 
			ELSE
				dw_magazine_maintenance_rc.Reset()
				dw_producer.Reset()
				parent.event pfc_close()
				m_pics_main.m_menu.PopMenu ( 300, 0 ) 
			END IF
			END IF
			
	ELSE
		//reset the datawindows, close parent and pop up the menu
		dw_magazine_maintenance_rc.Reset()
		dw_producer.Reset()
		parent.event pfc_close()
		m_pics_main.m_menu.PopMenu ( 300, 0 ) 
	END IF//IF dw_magazine_maintenance_rc.ModifiedCount() > 0 THEN
		
	
	
	
END IF//IF dw_magazine_maintenance.Visible = Enabled THEN


















//////////////////////////////////////////////////////////////////
//Integer li_answer
//String ls_magcd
//
//
//
//
////If any fields have been modified then print message for updating
////else quit window.
//
//IF dw_magazine_maintenance.ModifiedCount() > 0 THEN
//
//
//		ls_magcd = dw_magazine_maintenance.GetItemString(1,"mag_magcd")
//		
//		
//		IF IsNull(ls_magcd) OR TRIM(ls_magcd) = '' OR TRIM(ls_magcd) = '0000' THEN
//			dw_magazine_maintenance.Reset()
//			dw_producer.Reset()
//			close(parent)
//			 m_pics_main.m_menu.PopMenu ( 300, 0 ) 
//		ELSE
//		li_answer = MessageBox('Exit','Do You Want to Save Changes',Question!, yesno!)
//		
//		IF li_answer = 1 THEN
//			wf_update()
//			dw_magazine_maintenance.Reset()
//			dw_producer.Reset()
//			close(parent)
//			m_pics_main.m_menu.PopMenu ( 300, 0 ) 
//		ELSE
//			dw_magazine_maintenance.Reset()
//			dw_producer.Reset()
//			close(parent)
//			m_pics_main.m_menu.PopMenu ( 300, 0 ) 
//		END IF
//		END IF
//		
//ELSE
//	//reset the datawindows, close parent and pop up the menu
//	dw_magazine_maintenance.Reset()
//	dw_producer.Reset()
//	close(parent)
//   m_pics_main.m_menu.PopMenu ( 300, 0 ) 
//END IF//IF dw_magazine_maintenance.ModifiedCount() > 0 THEN
end event

type cb_deleterow from u_cb within w_magazine_maintenance
event ue_hint_text pbm_mousemove
string tag = "Deletes the row from the screen"
integer x = 1755
integer y = 1248
integer width = 425
integer taborder = 0
integer textsize = -10
string text = "&Delete Row"
end type

event ue_hint_text;call super::ue_hint_text;w_pics_main.setmicrohelp(This.tag)
end event

event clicked;call super::clicked;Integer li_num_rows, li_ans
String  ls_temp

li_ans = MessageBox('Delete','Do You Want to Delete the row from the screen',Question!,yesno!)
IF li_ans = 1 THEN
		
	IF dw_magazine_maintenance.Visible = TRUE THEN
		
		//Deletes the current row
		dw_magazine_maintenance.Deleterow(0)
		li_num_rows = dw_magazine_maintenance.RowCount()
		//After deleting set the row focus
		IF li_num_rows = 0 THEN
			dw_magazine_maintenance.Event pfc_addrow()
			dw_magazine_maintenance.SetFocus()
			dw_magazine_maintenance.SetColumn("mag_magcd")
		ELSE
			dw_magazine_maintenance.ScrollToRow(li_num_rows)
			dw_magazine_maintenance.SetFocus()
			IF IsNull(dw_magazine_maintenance.GetItemString(li_num_rows,"mag_magcd")) THEN
				dw_magazine_maintenance.SetColumn("mag_magcd")
			ELSE
				dw_magazine_maintenance.SetColumn("mag_freq")
			END IF//IF IsNull(dw_magazine_maintenance.GetItemString(li_num_rows,"mag_magcd")) THEN
		END IF//IF li_num_rows = 0 THEN
	ELSEIF dw_magazine_maintenance_rc.Visible = TRUE THEN
		
		//Deletes the current row
		dw_magazine_maintenance_rc.Deleterow(0)
		li_num_rows = dw_magazine_maintenance_rc.RowCount()
		//After deleting set the row focus
		IF li_num_rows = 0 THEN
			dw_magazine_maintenance_rc.Event pfc_addrow()
			dw_magazine_maintenance_rc.SetFocus()
			dw_magazine_maintenance_rc.SetColumn("magcd")
		ELSE
			dw_magazine_maintenance_rc.ScrollToRow(li_num_rows)
			dw_magazine_maintenance_rc.SetFocus()
			IF IsNull(dw_magazine_maintenance_rc.GetItemString(li_num_rows,"magcd")) THEN
				dw_magazine_maintenance_rc.SetColumn("magcd")
			ELSE
				dw_magazine_maintenance_rc.SetColumn("freq")
			END IF//IF IsNull(dw_magazine_maintenance.GetItemString(li_num_rows,"mag_magcd")) THEN
		END IF//IF li_num_rows = 0 THEN
	END IF//IF dw_magazine_maintenance.Visible = TRUE THEN
ELSE
	IF dw_magazine_maintenance.Visible = TRUE THEN
	 dw_magazine_maintenance.SetFocus()
   ELSEIF dw_magazine_maintenance_rc.Visible = TRUE THEN
	 dw_magazine_maintenance_rc.SetFocus()
  END IF
END IF//IF li_ans = 1 THEN
	


end event

type st_lccontract from u_st within w_magazine_maintenance
integer x = 1234
integer y = 16
integer width = 608
integer height = 76
integer textsize = -10
string text = "LC Contract Number"
end type

type em_lccontract from u_em within w_magazine_maintenance
integer x = 1225
integer y = 96
integer width = 571
integer height = 84
integer taborder = 60
integer textsize = -10
maskdatatype maskdatatype = stringmask!
string mask = "!!!!!!!!!!!!!!!!!!!!!!!!!"
string displaydata = ""
double increment = 0
string minmax = ""
end type

event modified;call super::modified;//Set Focus to the format field
sle_format.SetFocus()
sle_format.DisplayOnly = FALSE
end event

type cb_producers from u_cb within w_magazine_maintenance
integer x = 475
integer y = 1248
integer width = 709
integer taborder = 0
integer textsize = -10
string text = "Add/U&pdate Producers.."
end type

event clicked;call super::clicked;open(w_add_producer)
end event

type cb_print from u_cb within w_magazine_maintenance
integer x = 1211
integer y = 1248
integer width = 219
integer taborder = 0
integer textsize = -10
string text = "&Print"
end type

event clicked;call super::clicked;Integer li_modified_count, li_loop, li_num_rows,li_fy, li_rtn_code, li_row_count
String  ls_contract, ls_producer, ls_format, ls_loc_number

//Get values into local variables
li_fy = Integer(em_fy.text)
ls_contract = TRIM(sle_contract.text)

parent.st_records.visible =false
parent.st_records_no.visible =false
parent.st_records_no.text ='0'
//dw_magazine_maintenance.AcceptText() 
dw_producer.AcceptText()
ls_producer = dw_producer.GetText()
// 3 Retrieve arguments for nested report
IF dw_magazine_maintenance.visible =true THEN
	
	dw_magazine_maintenance_nested.SetTransObject(sqlservertrans )
	dw_magazine_maintenance_nested.Retrieve(li_fy, ls_contract, ls_producer )
	li_row_count =dw_magazine_maintenance_nested.GetItemNumber(1,'count_records_no_rc')
	parent.st_records.visible =true
	parent.st_records_no.visible =true
	parent.st_records_no.text = string(li_row_count)
	dw_magazine_maintenance_nested.TriggerEvent('pfc_print' )
END IF
IF dw_magazine_maintenance_rc.visible =true THEN
	dw_magazine_maintenance_rc_nested.SetTransObject(sqlservertrans )
	dw_magazine_maintenance_rc_nested.Retrieve(li_fy, ls_contract, ls_producer )
	li_row_count =dw_magazine_maintenance_rc_nested.GetItemNumber(1,'count_records_rc')
	parent.st_records.visible =true
	parent.st_records_no.visible =true
	parent.st_records_no.text = string(li_row_count)
	//dw_magazine_maintenance_nested.visible =true
	
	dw_magazine_maintenance_rc_nested.TriggerEvent('pfc_print' )
END IF
end event

type st_records from statictext within w_magazine_maintenance
boolean visible = false
integer x = 50
integer y = 1160
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

type st_records_no from statictext within w_magazine_maintenance
boolean visible = false
integer x = 480
integer y = 1160
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

type dw_magazine_maintenance_rc from u_dw within w_magazine_maintenance
event ue_trap_key pbm_dwnkey
event ue_enter_to_tab pbm_dwnprocessenter
boolean visible = false
integer x = 14
integer y = 220
integer width = 2903
integer height = 936
integer taborder = 100
string dataobject = "d_magazine_maintenance_rc"
boolean hscrollbar = true
end type

event ue_trap_key;call super::ue_trap_key;Integer li_num_rows

li_num_rows = THIS.RowCount()

IF (KeyDown(KeyTab!) OR KeyDown(KeyEnter!)) AND THIS.GetColumn() = 9 AND THIS.GetRow() = li_num_rows THEN
	IF dw_magazine_maintenance_rc.Object.magcd[li_num_rows]  <> '' THEN
   	dw_magazine_maintenance_rc.Event pfc_addrow()
		dw_magazine_maintenance_rc.Modify("magcd.Protect='1~tIf(IsRowNew(),0,1)'")
	ELSE
		dw_magazine_maintenance_rc.SetFocus()
	END IF//IF dw_magazine_maintenance_rc.Object.magcd[li_num_rows]  <> '' THEN
ELSEIF KeyDown(KeyDownArrow!) AND THIS.GetRow() = li_num_rows THEN
	IF dw_magazine_maintenance_rc.Object.mag_magcd[li_num_rows]  <> '' THEN
   	dw_magazine_maintenance_rc.Event pfc_addrow()
		dw_magazine_maintenance_rc.SetFocus()
		dw_magazine_maintenance_rc.SetColumn("magcd")
		dw_magazine_maintenance_rc.Modify("magcd.Protect='1~tIf(IsRowNew(),0,1)'")
	ELSE
		dw_magazine_maintenance_rc.SetFocus()
	END IF//IF dw_magazine_maintenance_rc.Object.magcd[li_num_rows]  <> '' THEN
	
END IF//IF (KeyDown(KeyTab!) OR KeyDown(KeyEnter!)) AND THIS.GetColumn() = 9 AND THIS.GetRow() = li_num_rows THEN

end event

event ue_enter_to_tab;call super::ue_enter_to_tab;//Sets the enter key to the tab path
Send(Handle(this),256,9,Long(0,0))
RETURN (1)
end event

event constructor;call super::constructor;this.of_SetTransObject( SQLServerTrans )

dw_magazine_maintenance.Event pfc_addrow()


end event

event getfocus;call super::getfocus;
//Set microhelp when in magazine code column
IF dw_magazine_maintenance_rc.GetColumn() = 1 THEN
	
	w_pics_main.Event pfc_MicroHelp('Enter Magazine Code ..')
	
END IF



//Disable the fiscal year, contract, producer and format fields 
//from being changes
 em_fy.DisplayOnly = TRUE
 sle_contract.DisplayOnly = TRUE
 sle_format.DisplayOnly = TRUE
 dw_producer.Enabled = FALSE
end event

event itemchanged;call super::itemchanged;String ls_magcode, ls_contract, null_string, ls_title
Integer li_fy,rtn

SetNull(null_string)

//get values into local variables
ls_contract = sle_contract.text
li_fy = Integer(em_fy.text)


IF DWO.Name = 'magcd' THEN
	ls_magcode = GetText()
			
	
	//Check if magazine exist in magttl table.
	IF wf_get_title(ls_magcode,ls_title) = 100 THEN
		//dw_magazine_maintenance.Object.mag_magcd.validationmsg = "Magazine Code Does Not Exist"
		rtn = Messagebox("Warning","Magazine Code Does Not Exist. Would you like to add it?",question!,yesNo!,1)
		IF rtn = 1 THEN
			OpenWithParm(w_add_mag_code,"RC")
		ELSE
			dw_magazine_maintenance_rc.SetText(null_string)
			dw_magazine_maintenance_rc.SetItem(row,1,null_string)
			dw_magazine_maintenance_rc.SetItem(row,2,"")
		   RETURN 1
		END IF
	 ELSE   //Check if magazine format is valid
	    dw_magazine_maintenance_rc.object.magttl_title[row] = ls_title
   END IF

			
	//Check for duplicate codes.	
	IF wf_check_duplicates(ls_magcode) = TRUE THEN
		dw_magazine_maintenance_rc.object.magcd.Validationmsg = "No Duplicates Allowed"
      dw_magazine_maintenance_rc.object.magcd[row] = null_string
		RETURN 1
	END IF//IF wf_check_duplicates(ls_magcode) = TRUE THEN
	
//Check to see if the magazine has been assigned to another contract	  
  IF wf_check_status(ls_magcode,li_fy) = 0 THEN
	dw_magazine_maintenance_rc.object.magcd.Validationmsg = "Magazine Assigned to Another Contract"
	dw_magazine_maintenance_rc.object.magcd[row] = null_string
	RETURN 1
  END IF//wf_check_status(ls_magcode,li_fy) = 0 THEN



//Set the item to the datawindow
dw_magazine_maintenance_rc.object.fy[row] = li_fy
dw_magazine_maintenance_rc.object.cntr[row] = ls_contract





//Enable the update button
  cb_update.enabled = TRUE
  cb_toggle.enabled = TRUE
	
	
END IF//IF DWO.Name = 'magcd' THEN

end event

event clicked;call super::clicked;Integer li_row


//Disable the delete key if title or status is clicked
IF DWO.Name =  "mag_magst" THEN
	cb_deleterow.Enabled = FALSE
ELSE
	cb_deleterow.Enabled = TRUE
END IF
end event

event itemfocuschanged;call super::itemfocuschanged;//set highlight to the following fields

IF dwo.name = "freq" THEN
	dw_magazine_maintenance_rc.Event pfc_selectall()
END IF


IF dwo.name = "estiss" THEN
	dw_magazine_maintenance_rc.Event pfc_selectall()
END IF


IF dwo.name = "estsubs" THEN
	dw_magazine_maintenance_rc.Event pfc_selectall()
END IF


IF dwo.name = "estszs" THEN
	dw_magazine_maintenance_rc.Event pfc_selectall()
END IF


IF dwo.name = "estsz" THEN
	dw_magazine_maintenance_rc.Event pfc_selectall()
END IF



IF dwo.name = "estszl" THEN
	dw_magazine_maintenance_rc.Event pfc_selectall()
END IF



IF dwo.name = "estmin" THEN
	dw_magazine_maintenance_rc.Event pfc_selectall()
END IF


//IF dwo.name = "cwd" THEN
//	dw_magazine_maintenance_rc.Event pfc_selectall()
//END IF



IF dwo.name = "ccd" THEN
	dw_magazine_maintenance_rc.Event pfc_selectall()
END IF




end event

event rowfocuschanged;call super::rowfocuschanged;int li_row_count
	li_row_count =dw_magazine_maintenance_rc.RowCount()
	parent.st_records.visible= true
	parent.st_records_no.visible = true
	if dw_magazine_maintenance_rc.visible and not(ib_dw_reset_rc ) then
		parent.st_records_no.text =string(li_row_count - 1)
	end if
	
end event

type dw_magazine_maintenance_nested from u_dw within w_magazine_maintenance
boolean visible = false
integer x = 14
integer y = 212
integer width = 2903
integer height = 936
integer taborder = 20
string dataobject = "d_magazine_maintenance_nested"
boolean hscrollbar = true
end type

type dw_magazine_maintenance_rc_nested from u_dw within w_magazine_maintenance
boolean visible = false
integer x = 14
integer y = 208
integer width = 2903
integer height = 936
integer taborder = 10
string dataobject = "d_magazine_maintenance_rc_nested"
boolean hscrollbar = true
end type

type dw_magazine_maintenance from u_dw within w_magazine_maintenance
event ue_enter_to_tab pbm_dwnprocessenter
event ue_trap_key pbm_dwnkey
event ue_hint_text pbm_mousemove
boolean visible = false
integer x = 14
integer y = 216
integer width = 2903
integer height = 936
integer taborder = 110
string dataobject = "d_magazine_maintenance"
boolean hscrollbar = true
end type

event ue_enter_to_tab;call super::ue_enter_to_tab;//Sets the enter key to the tab path
Send(Handle(this),256,9,Long(0,0))
RETURN (1)
end event

event ue_trap_key;call super::ue_trap_key;Integer li_num_rows

li_num_rows = THIS.RowCount()

IF (KeyDown(KeyTab!) OR KeyDown(KeyEnter!)) AND THIS.GetColumn() = 8 AND THIS.GetRow() = li_num_rows THEN
	IF dw_magazine_maintenance.Object.mag_magcd[li_num_rows]  <> '' THEN
   	dw_magazine_maintenance.Event pfc_addrow()
		dw_magazine_maintenance.Modify("mag_magcd.Protect='1~tIf(IsRowNew(),0,1)'")
	ELSE
		dw_magazine_maintenance.SetFocus()
	END IF//IF dw_magazine_maintenance.Object.mag_magcd[li_num_rows]  <> '' THEN
ELSEIF KeyDown(KeyDownArrow!) AND THIS.GetRow() = li_num_rows THEN
	IF dw_magazine_maintenance.Object.mag_magcd[li_num_rows]  <> '' THEN
   	dw_magazine_maintenance.Event pfc_addrow()
		dw_magazine_maintenance.SetFocus()
		dw_magazine_maintenance.SetColumn("mag_magcd")
		dw_magazine_maintenance.Modify("mag_magcd.Protect='1~tIf(IsRowNew(),0,1)'")
	ELSE
		dw_magazine_maintenance.SetFocus()
	END IF//IF dw_magazine_maintenance.Object.mag_magcd[li_num_rows]  <> '' THEN
	
END IF//IF (KeyDown(KeyTab!) OR KeyDown(KeyEnter!)) AND THIS.GetColumn() = 9 AND THIS.GetRow() = li_num_rows THEN

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

event constructor;call super::constructor;this.of_SetTransObject( SQLServerTrans )

dw_magazine_maintenance.Event pfc_addrow()


end event

event itemchanged;call super::itemchanged;STRING ls_magcode, ls_title, ls_mag_status,ls_contract, null_string, ls_format, Ls_select
Integer li_fy,rtn
Long    ll_ccd, ll_cwd

SetNull(null_string)


// Check to see if the magazine code exists, or if duplicates have been entered, or 
// has been assigned to another contract.
IF Dwo.Name = "mag_magcd" THEN
 ls_magcode = TRIM(dw_magazine_maintenance.GetText())
  
   li_fy       =  Integer(em_fy.text)
   ls_contract =  TRIM(sle_contract.text)
	ls_format   =  TRIM(sle_format.text)
	
	//Check if magazine exist in magttl table
	IF wf_get_title(ls_magcode,ls_title) = 100 THEN
		//dw_magazine_maintenance.Object.mag_magcd.validationmsg = "Magazine Code Does Not Exist"
		rtn = MessageBox("Warning","Magazine Code Does Not Exist. Would you like to add it?",Question!,YesNo!,1)
		IF rtn = 1 THEN
			openwithparm(w_add_mag_code,"NOTRC")
		ELSE
			dw_magazine_maintenance.SetText(null_string)
			dw_magazine_maintenance.SetItem(row,1,null_string)
			dw_magazine_maintenance.Setitem(row,2,"")
		   RETURN 1
		END IF
	 ELSE   //Check if magazine format is valid
			  IF wf_check_magazine_format(ls_magcode) = TRUE THEN
			    dw_magazine_maintenance.Object.magttl_title[row] = ls_title
			  ELSE
				 dw_magazine_maintenance.Object.mag_magcd.validationmsg = "Magazine is Not In Valid Format"
				 dw_magazine_maintenance.SetText(null_string)
				 dw_magazine_maintenance.SetItem(row,1,null_string)
				 dw_magazine_maintenance.Setitem(row,2,"")
				 RETURN 1
			  END IF	// IF wf_check_magazine_format(ls_magcode) = TRUE 
   END IF // IF wf_get_title(ls_magcode,ls_title) = 100 THEN
	  
	
	
	
   //Check for duplicates
   IF wf_check_duplicates(ls_magcode) = TRUE THEN
		dw_magazine_maintenance.Object.mag_magcd.validationmsg = "No Duplicates Allowed"
		dw_magazine_maintenance.SetText(null_string)
		dw_magazine_maintenance.SetItem(row,1,null_string)
      dw_magazine_maintenance.Setitem(row,2,"")
	   RETURN 1
  END IF	  
	  
  //Check to see if the magazine has been assigned to another contract	  
  IF wf_check_status(ls_magcode,li_fy) = 0 THEN
	dw_magazine_maintenance.Object.mag_magcd.validationmsg = "Magazine Assigned to Another Contract"
	dw_magazine_maintenance.SetText(null_string)
	dw_magazine_maintenance.SetItem(row,1,null_string)
   dw_magazine_maintenance.Setitem(row,2,"")
	RETURN 1
  END IF//wf_check_status(ls_magcode,li_fy) = 0 THEN


//Enable the Update button and the Next Screen Button
cb_update.Enabled = TRUE
cb_toggle.Enabled = TRUE
cb_deleterow.Enabled = TRUE



  //Fill in Default Values for the fields
  dw_magazine_maintenance.Object.mag_fy[row] = li_fy
  dw_magazine_maintenance.Object.mag_cntr[row] = ls_contract
  dw_magazine_maintenance.SetItem(row,"mag_freq",0)
  //Get the CCD and CWD from last year for that magazine and put it into 
  //the datawindow.
	wf_put_ccd(row, ls_magcode,ll_ccd)
    IF IsNull(ll_ccd) THEN ll_ccd = 0
//	 IF IsNull(ll_cwd) THEN ll_cwd = 0
	 
    dw_magazine_maintenance.SetItem(row,"mag_ccd",ll_ccd)
//	 dw_magazine_maintenance.SetItem(row,"mag_cwd",ll_cwd)
  
  
  
  
  
  
//  dw_magazine_maintenance.Object.mag_freq[row] = '0'
//  dw_magazine_maintenance.Object.mag_estiss[row] =0
//  dw_magazine_maintenance.Object.mag_estsubs[row] =0
//  dw_magazine_maintenance.Object.mag_estsz[row] = 0.0
//  dw_magazine_maintenance.Object.mag_estmin[row] =0
//  dw_magazine_maintenance.Object.mag_ccd[row] =0
//  dw_magazine_maintenance.Object.mag_cwd[row] =0
//  dw_magazine_maintenance.Object.mag_magst[row] = 'A'
//  dw_magazine_maintenance.Object.mag_cntcso[row] = 0.00
//  dw_magazine_maintenance.Object.mag_cntcsc[row] = 0.00
//  dw_magazine_maintenance.Object.mag_cntcsadjc[row] = 0.00
//  dw_magazine_maintenance.Object.mag_ucmast[row] = 0.000000
//  dw_magazine_maintenance.Object.mag_ucdupl[row] = 0.00
//  dw_magazine_maintenance.Object.mag_ucothr[row] = 0.00
//  dw_magazine_maintenance.Object.mag_estmail[row] = 0.00
//  dw_magazine_maintenance.Object.mag_invamtc[row] = 0.00
//  
//  
 
END IF // Dwo.Name = "mag_magcd" THEN







end event

event itemfocuschanged;call super::itemfocuschanged;// Set The Microhelp when the field in the Data window get focus

IF DWO.Name = "mag_magcd" THEN
	w_pics_main.Event pfc_MicroHelp("Enter Magazine Code")
END IF


IF DWO.Name = "mag_freq" THEN
	w_pics_main.Event pfc_MicroHelp("Enter Frequency ")
	dw_magazine_maintenance.Event pfc_selectall()
END IF


IF DWO.Name = "mag_estiss" THEN
	w_pics_main.Event pfc_MicroHelp("Enter Estimated Issues")
	dw_magazine_maintenance.Event pfc_selectall()
END IF


IF DWO.Name = "mag_estsubs" THEN
	w_pics_main.Event pfc_MicroHelp("Enter Estimated Subscribers ")
	dw_magazine_maintenance.Event pfc_selectall()
END IF

IF DWO.Name = "mag_estsz" THEN
	w_pics_main.Event pfc_MicroHelp("Enter Estimated Size")
	dw_magazine_maintenance.Event pfc_selectall()
END IF


IF DWO.Name = "mag_estmin" THEN
	w_pics_main.Event pfc_MicroHelp("Enter Estimated Minutes")
	dw_magazine_maintenance.Event pfc_selectall()
END IF


//IF DWO.Name = "mag_cwd" THEN
//	w_pics_main.Event pfc_MicroHelp("Enter Calendar Working Days")
//	dw_magazine_maintenance.Event pfc_selectall()
//END IF


IF DWO.Name = "mag_ccd" THEN
	w_pics_main.Event pfc_MicroHelp("Enter Contract Calendar Days ")
	dw_magazine_maintenance.Event pfc_selectall()
END IF

end event

event getfocus;call super::getfocus;

IF dw_magazine_maintenance.GetColumn() = 1 THEN
	
	w_pics_main.Event pfc_MicroHelp('Enter Magazine Code ..')
	
END IF

//If format is BR then Disable the minutes field in the data window
IF TRIM(sle_format.text) = 'BR' THEN
	dw_magazine_maintenance.Object.mag_estmin.TabSequence = '0'
ELSE
	dw_magazine_maintenance.Object.mag_estmin.TabSequence  = '60'
END IF

//Protect the title and status columns
dw_magazine_maintenance.Object.magttl_title.Protect = 1
dw_magazine_maintenance.Object.mag_magst.Protect = 1

//Disable the fiscal year, contract, producer and format fields 
//from being changes
 em_fy.DisplayOnly = TRUE
 sle_contract.DisplayOnly = TRUE
 sle_format.DisplayOnly = TRUE
 em_lccontract.DisplayOnly = false
 dw_producer.Enabled = FALSE
end event

event rbuttondown;//
end event

event rbuttonup;//
end event

event clicked;call super::clicked;Integer li_row


//Disable the delete key if title or status is clicked
IF DWO.Name = "magttl_title" OR DWO.Name = "mag_magst" THEN
	cb_deleterow.Enabled = FALSE
ELSE
	cb_deleterow.Enabled = TRUE
END IF
end event

event updatestart;call super::updatestart;w_pics_main.Event pfc_microhelp('Updating Please Wait..')
end event

event updateend;call super::updateend;Long ll_updated, ll_inserted, ll_total


ll_updated = rowsupdated
ll_inserted = rowsinserted

ll_total = rowsupdated + rowsinserted

IF ll_total > 0 THEN
  w_pics_main.Event pfc_microhelp('Row(s) Successfully Updated')
  
END IF
end event

event rowfocuschanged;call super::rowfocuschanged;int li_row_count
	li_row_count =dw_magazine_maintenance.RowCount()
	parent.st_records.visible= true
	parent.st_records_no.visible = true
	if dw_magazine_maintenance.visible and not (ib_dw_reset_br ) then
		parent.st_records_no.text =string(li_row_count - 1 )
	end if
	
end event


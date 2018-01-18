$PBExportHeader$w_magazine_invoice_tracking.srw
forward
global type w_magazine_invoice_tracking from w_sheet
end type
type cb_override from commandbutton within w_magazine_invoice_tracking
end type
type cb_update from u_cb within w_magazine_invoice_tracking
end type
type cb_clear from u_cb within w_magazine_invoice_tracking
end type
type cb_exit from u_cb within w_magazine_invoice_tracking
end type
type dw_invoice_tracking_magcode_issuedate from u_dw within w_magazine_invoice_tracking
end type
type dw_display_existing_invno from datawindow within w_magazine_invoice_tracking
end type
type dw_invoice_tracking_est_rc from u_dw within w_magazine_invoice_tracking
end type
type dw_invoice_tracking from u_dw within w_magazine_invoice_tracking
end type
type dw_invoice_tracking_rc from u_dw within w_magazine_invoice_tracking
end type
end forward

global type w_magazine_invoice_tracking from w_sheet
integer x = 5
integer y = 4
integer width = 2962
integer height = 2208
string title = "Magazine Invoice/Tracking"
cb_override cb_override
cb_update cb_update
cb_clear cb_clear
cb_exit cb_exit
dw_invoice_tracking_magcode_issuedate dw_invoice_tracking_magcode_issuedate
dw_display_existing_invno dw_display_existing_invno
dw_invoice_tracking_est_rc dw_invoice_tracking_est_rc
dw_invoice_tracking dw_invoice_tracking
dw_invoice_tracking_rc dw_invoice_tracking_rc
end type
global w_magazine_invoice_tracking w_magazine_invoice_tracking

type variables
date Ladmdt
Boolean NewInvoice

end variables

forward prototypes
public function integer wf_check_invoice (string ls_magcode, date ld_issuedate, string ls_invoice_number, ref real lr_invamt, ref real lr_invmail, ref date ld_admdt, ref date ld_pcsdt, ref character lc_ovrcd)
public function integer wf_get_title (string ls_magcode, ref string ls_title)
public function integer wf_get_invamtc (integer li_fy, string ls_contract, string ls_magcode, ref real lr_invamtc)
public function integer wf_get_invamt (integer li_fy, string ls_magcode, string ls_contract, ref real lr_invamt)
end prototypes

public function integer wf_check_invoice (string ls_magcode, date ld_issuedate, string ls_invoice_number, ref real lr_invamt, ref real lr_invmail, ref date ld_admdt, ref date ld_pcsdt, ref character lc_ovrcd);    SELECT maginv.invamt,   
           maginv.invmail,   
           maginv.admdt,   
           maginv.pcsdt,   
           maginv.ovrcd  
    INTO   :lr_invamt,   
           :lr_invmail,   
           :ld_admdt,   
           :ld_pcsdt,   
           :lc_ovrcd  
    FROM maginv  
   WHERE ( maginv.magcd = :ls_magcode ) AND  
         ( maginv.issdt = :ld_issuedate ) AND  
         ( maginv.invno = :ls_invoice_number ) 
	USING SqlServerTrans;
	
	
	RETURN SqlServerTrans.SqlCode;



end function

public function integer wf_get_title (string ls_magcode, ref string ls_title);
  SELECT magttl.title  
    INTO :ls_title  
    FROM magttl  
   WHERE magttl.magcd = :ls_magcode 
	USING SqlServerTrans;


RETURN SqlServerTrans.SqlCode;
end function

public function integer wf_get_invamtc (integer li_fy, string ls_contract, string ls_magcode, ref real lr_invamtc);//Get the invoice amount cumulative from the mag table.
SELECT invamtc
INTO   :lr_invamtc
FROM   mag	
WHERE  fy    = :li_fy AND
		 cntr  = :ls_contract AND
		 magcd = :ls_magcode
USING SqlServerTrans;

RETURN SqlServerTrans.SqlCode;
end function

public function integer wf_get_invamt (integer li_fy, string ls_magcode, string ls_contract, ref real lr_invamt);
SELECT sum(invamt)
INTO   :lr_invamt	
FROM   maginv
WHERE  magcd = :ls_magcode AND
		 fy = :li_fy AND
		 cntr = :ls_contract
USING SqlServerTrans;

RETURN SqlServerTrans.SqlCode
end function

on w_magazine_invoice_tracking.create
int iCurrent
call super::create
this.cb_override=create cb_override
this.cb_update=create cb_update
this.cb_clear=create cb_clear
this.cb_exit=create cb_exit
this.dw_invoice_tracking_magcode_issuedate=create dw_invoice_tracking_magcode_issuedate
this.dw_display_existing_invno=create dw_display_existing_invno
this.dw_invoice_tracking_est_rc=create dw_invoice_tracking_est_rc
this.dw_invoice_tracking=create dw_invoice_tracking
this.dw_invoice_tracking_rc=create dw_invoice_tracking_rc
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.cb_override
this.Control[iCurrent+2]=this.cb_update
this.Control[iCurrent+3]=this.cb_clear
this.Control[iCurrent+4]=this.cb_exit
this.Control[iCurrent+5]=this.dw_invoice_tracking_magcode_issuedate
this.Control[iCurrent+6]=this.dw_display_existing_invno
this.Control[iCurrent+7]=this.dw_invoice_tracking_est_rc
this.Control[iCurrent+8]=this.dw_invoice_tracking
this.Control[iCurrent+9]=this.dw_invoice_tracking_rc
end on

on w_magazine_invoice_tracking.destroy
call super::destroy
destroy(this.cb_override)
destroy(this.cb_update)
destroy(this.cb_clear)
destroy(this.cb_exit)
destroy(this.dw_invoice_tracking_magcode_issuedate)
destroy(this.dw_display_existing_invno)
destroy(this.dw_invoice_tracking_est_rc)
destroy(this.dw_invoice_tracking)
destroy(this.dw_invoice_tracking_rc)
end on

event resize;call super::resize;long ll_height
this.X = w_pics_main.X
this.Y = w_pics_main.Y
ll_height = w_pics_main.mdi_1.Height


this.Resize(w_pics_main.workspacewidth(), ll_height)
end event

event pfc_preopen;call super::pfc_preopen;//Does the scaling to fit the sheet to the Data Window


this.of_setPreference(TRUE)
this.inv_preference.of_SetToolBars(TRUE)
this.inv_preference.of_SetWindow(TRUE)
this.of_SetResize(TRUE)
this.inv_resize.of_setOrigSize(this.workspacewidth(), this.workspaceheight())
inv_resize.of_Register(dw_invoice_tracking_magcode_issuedate,"scale")
inv_resize.of_Register(dw_invoice_tracking,"scale")
inv_resize.of_Register(dw_invoice_tracking_rc,"scale")
inv_resize.of_Register(dw_invoice_tracking_est_rc,"scale")
inv_resize.of_Register(dw_display_existing_invno,"scale")
inv_resize.of_Register(cb_clear,"scale")
inv_resize.of_Register(cb_exit,"scale")
inv_resize.of_Register(cb_update,"scale")
inv_resize.of_Register(cb_override,"scale")




end event

event close;//
end event

event closequery;//
end event

event pfc_postopen;call super::pfc_postopen;String   ls_magcode, ls_title, ls_contract, ls_producer, ls_format, ls_existing_invoices=" ",ls_invno
Date     ld_issuedate
Integer  li_rtn_code, li_fy, ls_cnt,i
str_exist_inv lstr_exist_inv
DateTime ldt_issdt

//Get the magazine Code and Issuedate from the dialog box
ls_magcode = w_mm_selection_list.ls_magcode
ld_issuedate = w_mm_selection_list.ld_issuedate

//Retrieve data and set focus to data window - dw_invoice_tracking to input invoice number.
//If retrieve is successfull then Disable dw_invoice_tracking_magcode_issuedate.
IF dw_invoice_tracking_magcode_issuedate.Retrieve(ls_magcode,ld_issuedate)  < 0 THEN
   Messagebox('Error', 'Cannot Access Database .. Contact You DBA')
	RETURN
ELSE
	//Hide the selection list screen
	 w_mm_selection_list.Hide()
	//See if invoices already exist
	 SELECT count(*) INTO :ls_cnt
	 FROM maginv
	 where magcd=:ls_magcode
	 AND issdt=:ld_issuedate
	 USING SqlServerTrans;
	 IF ls_cnt>0 THEN
		lstr_exist_inv.ls_magcd = ls_magcode
		lstr_exist_inv.ld_issdt = ld_issuedate
		OpenWithParm(w_magazine_existing_invoices, lstr_exist_inv)
		dw_display_existing_invno.SetTransObject(SqlServerTrans)
		dw_display_existing_invno.visible=TRUE
		dw_display_existing_invno.Retrieve(ls_magcode,ld_issuedate)
	END IF
	IF ls_existing_invoices <> " " THEN
 	  Messagebox('Warning', 'Please DONOT use these invoice numbers:'+ls_existing_invoices)
	END IF

	 
	//Protect the fields in dw_invoice_tracking_magcode datawindow
	  dw_invoice_tracking_magcode_issuedate.object.magttl_magcd.Protect = 1
	  dw_invoice_tracking_magcode_issuedate.object.magiss_issdt.Protect = 1
	//Get the format from the dw_invoice_tracking_magcode datawindow
	ls_format = dw_invoice_tracking_magcode_issuedate.object.magcntr_format[1]
   	IF Trim(ls_format) = 'BR' OR Trim(ls_format) = 'FD' THEN
		dw_invoice_tracking.visible = TRUE
		dw_invoice_tracking.enabled = TRUE
		dw_invoice_tracking_rc.visible = FALSE
	   	dw_invoice_tracking_est_rc.visible = FALSE
		dw_invoice_tracking.SetFocus()
	ELSEIF Trim(ls_format) = 'RC' THEN
		dw_invoice_tracking.visible = FALSE
		dw_invoice_tracking_rc.visible = TRUE
	   	dw_invoice_tracking_est_rc.visible = TRUE
		dw_invoice_tracking_rc.SetFocus()
		li_fy = dw_invoice_tracking_magcode_issuedate.object.magiss_fy[1]
		ls_contract = dw_invoice_tracking_magcode_issuedate.object.magiss_cntr[1]
   		dw_invoice_tracking_est_rc.Retrieve(ls_magcode,ls_contract,li_fy)	
	END IF//IF TRIM(ls_format) = 'BR' OR TRIM(ls_format) = 'FD' THEN
		
   this.windowState = maximized!
END IF//


end event

type cb_override from commandbutton within w_magazine_invoice_tracking
integer x = 1618
integer y = 1936
integer width = 329
integer height = 88
integer taborder = 50
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "Override"
end type

event clicked;open(w_add_mag_override)
end event

type cb_update from u_cb within w_magazine_invoice_tracking
integer x = 2002
integer y = 1936
integer width = 283
integer height = 88
integer taborder = 0
integer textsize = -10
boolean enabled = false
string text = "&Update"
end type

event clicked;call super::clicked;Real lr_diff, lr_total, lr_invamtc, lr_total_invoice, lr_invamt, lr_sz, lr_invmail
Integer li_rc, li_fy, li_subs, li_mins
String ls_magcode, ls_contract, ls_invno, ls_ovrcode 
Datetime ldt_issuedate, ldt_pcsdate, ldt_admindate
Date tday
DwItemStatus l_status
date ld_pcsdate,ld_admindate
time lt_pcsdate,lt_admindate

//ld_OrderDate = Date(sle_orderdate.Text)
//lt_OrderTime = Time(sle_ordertime.Text)


tday = Today()

//Update the RC magazines
IF dw_invoice_tracking_rc.visible = TRUE THEN
	 dw_invoice_tracking.visible = FALSE
	 dw_invoice_tracking_rc.AcceptText()
	 dw_invoice_tracking_magcode_issuedate.AcceptText()
	
	//Get Retrieved values into local variables.
		ldt_issuedate = dw_invoice_tracking_magcode_issuedate.object.magiss_issdt[1]
		li_fy = dw_invoice_tracking_magcode_issuedate.object.magiss_fy[1]
		ls_magcode = dw_invoice_tracking_magcode_issuedate.object.magttl_magcd[1]
		ls_contract = dw_invoice_tracking_magcode_issuedate.object.magiss_cntr[1]

	
		ldt_pcsdate = dw_invoice_tracking_rc.object.maginv_pcsdt[1]
		ld_pcsdate = Date(ldt_pcsdate)
		lt_pcsdate = Time('00:00:00')
		ldt_pcsdate = DateTime(ld_pcsdate, lt_pcsdate)
		
		
		ldt_admindate = dw_invoice_tracking_rc.object.maginv_admdt[1]
		ld_admindate = Date(ldt_admindate)
		lt_admindate = Time('00:00:00')
		ldt_admindate = DateTime(ld_admindate, lt_admindate)

	//Set these values into dw_invoice_tracking_rc datawindow
		dw_invoice_tracking_rc.object.maginv_issdt[1] = ldt_issuedate
	       dw_invoice_tracking_rc.object.maginv_fy[1] = li_fy
	       dw_invoice_tracking_rc.object.maginv_magcd[1] = ls_magcode
	       dw_invoice_tracking_rc.object.maginv_cntr[1] = ls_contract
	       dw_invoice_tracking_rc.object.maginv_pcsdt[1] = ldt_pcsdate
	       dw_invoice_tracking_rc.object.maginv_admdt[1] = ldt_admindate
		dw_invoice_tracking_rc.object.maginv_CREATED_DATE[1] = tday
		dw_invoice_tracking_rc.object.maginv_CREATED_BY[1] = gnv_app.of_GetUserId()
		dw_invoice_tracking_rc.object.maginv_MODIFIED_DATE[1] = tday
		dw_invoice_tracking_rc.object.maginv_MODIFIED_BY[1] = gnv_app.of_GetUserId()
		
		//Set the row to newmodified so that we can use dw_update() for the maginv table
		dw_invoice_tracking_rc.SetItemStatus(1,0,primary!,newModified!)
		
		lr_total = dw_invoice_tracking_rc.object.total[1]	
		lr_invamt = dw_invoice_tracking_rc.object.maginv_invamt[1]
		ls_ovrcode = dw_invoice_tracking_rc.object.maginv_ovrcd[1]
		lr_diff    = lr_invamt - lr_total
		
		//If Invoice amount is null or is zero then do not process
		IF IsNull(lr_invamt) OR lr_invamt = .0 THEN
		 Messagebox('Error','Invoice Amount Needed To Process')
		 dw_invoice_tracking_rc.SetFocus()
		 dw_invoice_tracking_rc.SetColumn("maginv_invamt")
		 RETURN 
		END IF		
	
		
		//Check to see if the difference is greater or less than zero
		IF ((lr_diff > 1.00 OR lr_diff < -1.00) AND  (Trim(ls_ovrcode) <> '' )) OR  &
			((lr_diff <= 1.00 AND lr_diff >=  -1.00) AND  (Trim(ls_ovrcode) = '' OR IsNull(Trim(ls_ovrcode)))) THEN
			
		
	      //Update the datawindow.
			IF dw_invoice_tracking_rc.Update() = -1 THEN
				ROLLBACK USING SqlServerTrans;
				Messagebox('Error','Update Failed Contact Your DBA')
				RETURN 
			ELSE
				COMMIT USING SqlServerTrans;
				//set the calculate the invamtc and set it to the mag table
				wf_get_invamtc(li_fy, ls_contract, ls_magcode, lr_invamtc)
				IF IsNull(lr_invamtc) THEN lr_invamtc = 0
				lr_total_invoice = lr_invamtc + dw_invoice_tracking_rc.GetItemNumber(1,"maginv_invamt")
				//Update the Mag table with the invoice amount (invamtc)
						UPDATE mag  
						set invamtc = :lr_total_invoice  
						WHERE (mag.fy = :li_fy) AND  
								(mag.cntr = :ls_contract) AND  
								(mag.magcd = :ls_magcode)   
						USING SqlServerTrans;
					  IF SqlServerTrans.sqlCode = 0 THEN 
				       COMMIT USING SqlServerTrans;
					  ELSE
						 ROLLBACK USING SqlServerTrans;
						 Messagebox('Error','Could Not Update mag table')
						 RETURN
					  END IF//IF SqlServerTrans.SqlCode = 0 THEN
					  
				     COMMIT USING SqlServerTrans;
				
			END IF//IF dw_invoice_tracking_rc.Update() = -1 THEN
		ELSE		
			Messagebox('Error','Overide Code Needed For Processing')
			dw_invoice_tracking_rc.SetFocus()
			dw_invoice_tracking_rc.SetColumn("maginv_ovrcd")
			RETURN
		END IF//IF ((lr_diff > 1.00 OR lr_diff < -1.00) AND  (TRIM(ls_ovrcode) 
	
	   //Get Ready for next input 
			dw_invoice_tracking_rc.Reset()
			dw_invoice_tracking_est_rc.Reset()
			dw_invoice_tracking_magcode_issuedate.Reset()
			dw_invoice_tracking.Reset()
			dw_display_existing_invno.Reset()
			dw_invoice_tracking.EVENT pfc_addrow()
			dw_invoice_tracking_magcode_issuedate.EVENT pfc_addrow()
		
		//Disable dw_invoice_tracking and set focus to dw_invoice_tracking_magcode_issuedate for
		// next input.
			dw_invoice_tracking.enabled = FALSE
			dw_invoice_tracking_magcode_issuedate.enabled = TRUE
			dw_invoice_tracking_magcode_issuedate.object.magttl_magcd.Protect = 0
			dw_invoice_tracking_magcode_issuedate.object.magiss_issdt.Protect = 0
			dw_invoice_tracking_magcode_issuedate.SetFocus()
		//Hide the RC magazine datawindows and make visible the FD and BR
		   dw_invoice_tracking.visible = TRUE
		   dw_invoice_tracking_rc.visible = FALSE
			dw_invoice_tracking_est_rc.visible = FALSE
		//Disable Update button. It will be Enabled in the Itemchanged event of the data window
		//dw_invoice_tracking_rc.
			cb_update.enabled = FALSE
			RETURN 2	
	
//Update the FD and BR magazines	
ELSEIF dw_invoice_tracking.visible = TRUE THEN
       	dw_invoice_tracking_rc.visible = FALSE
		dw_invoice_tracking.AcceptText()
		dw_invoice_tracking_magcode_issuedate.AcceptText()
		
		//Get Retrieved values into local variables.
		ldt_issuedate = dw_invoice_tracking_magcode_issuedate.object.magiss_issdt[1]
		li_fy = dw_invoice_tracking_magcode_issuedate.object.magiss_fy[1]
		ls_magcode = dw_invoice_tracking_magcode_issuedate.object.magttl_magcd[1]
		ls_contract = dw_invoice_tracking_magcode_issuedate.object.magiss_cntr[1]
		lr_diff = dw_invoice_tracking.object.diff[1]
		
		ls_invno = dw_invoice_tracking.object.maginv_invno[1]
		li_subs = dw_invoice_tracking.object.magiss_subs[1]
		lr_sz   = dw_invoice_tracking.object.magiss_sz[1]
		li_mins = dw_invoice_tracking.object.magiss_mins[1]
		ldt_pcsdate = dw_invoice_tracking.object.maginv_pcsdt[1]
		ldt_admindate = dw_invoice_tracking.object.maginv_admdt[1]
		ls_ovrcode = dw_invoice_tracking.object.maginv_ovrcd[1]
		lr_invamt = dw_invoice_tracking.object.maginv_invamt[1]
		ls_ovrcode = dw_invoice_tracking.object.maginv_ovrcd[1]
		
		ld_pcsdate = Date(ldt_pcsdate)
		lt_pcsdate = Time('00:00:00')
		ldt_pcsdate = DateTime(ld_pcsdate, lt_pcsdate)
		
		ld_admindate = Date(ldt_admindate)
		lt_admindate = Time('00:00:00')
		ldt_admindate = DateTime(ld_admindate, lt_admindate)

		dw_invoice_tracking.object.maginv_magcd[1] = ls_magcode
		dw_invoice_tracking.object.maginv_issdt[1] = ldt_issuedate
		dw_invoice_tracking.object.maginv_ovrcd[1] = ls_ovrcode
		dw_invoice_tracking.object.maginv_fy[1] = li_fy
		dw_invoice_tracking.object.maginv_cntr[1] = ls_contract
	       dw_invoice_tracking.object.maginv_pcsdt[1] = ldt_pcsdate
	       dw_invoice_tracking.object.maginv_admdt[1] = ldt_admindate
		dw_invoice_tracking.object.maginv_CREATED_DATE[1] = tday
		dw_invoice_tracking.object.maginv_CREATED_BY[1] = gnv_app.of_GetUserId()
		dw_invoice_tracking.object.maginv_MODIFIED_DATE[1] = tday
		dw_invoice_tracking.object.maginv_MODIFIED_BY[1] = gnv_app.of_GetUserId()
		
		
		
		
		dw_invoice_tracking.AcceptText()
		//If Invoice amount is null or is zero then do not process
		IF IsNull(lr_invamt) OR lr_invamt = .0 THEN
			 Messagebox('Error','Invoice Amount Needed To Process')
			 dw_invoice_tracking.SetFocus()
			 dw_invoice_tracking.SetColumn("maginv_invamt")
			 RETURN 
		END IF
		
		//If size is zero then do not process
		IF lr_sz = .0 OR IsNull(lr_sz) THEN
		//IF NewInvoice = TRUE THEN
			Messagebox('Error','Size Cannot Be Zero .. Input Size')
			dw_invoice_tracking.SetFocus()
			dw_invoice_tracking.SetColumn("magiss_sz")
			 RETURN 
		END IF
		
		
		//Set the row to newmodified so that we can use dw_update() for the maginv table
		dw_invoice_tracking.SetItemStatus(1,0,primary!,newModified!)
		
			
		
		//IF Difference Is Greater than $1.00 or less than $1.00 and Overide code is not blank then process
		// Else if differrence is less than 1 and greater than 0 and 
		IF ((lr_diff > 1.00 OR lr_diff < -1.00) AND  (Trim(ls_ovrcode) <> '' )) OR  &
			((lr_diff <= 1.00 AND lr_diff >=  -1.00) AND  (Trim(ls_ovrcode) = '' OR IsNull(Trim(ls_ovrcode)))) THEN
		
				
				//Update the maginv table
						
					li_rc = dw_invoice_tracking.Update(TRUE,FALSE)
					IF li_rc = 1 THEN
					 COMMIT USING SqlServerTrans;
					// IF new invoice then get the contract invoice amount from the mag table and add it to the
					// input value. 
						IF NewInvoice = TRUE THEN
							wf_get_invamtc(li_fy, ls_contract, ls_magcode, lr_invamtc)
							IF IsNull(lr_invamtc) THEN lr_invamtc = 0
							lr_total_invoice = lr_invamtc + dw_invoice_tracking.GetItemNumber(1,"maginv_invamt")
						END IF
					
						//Update the Mag table with the invoice amount
						UPDATE mag  
						set invamtc = :lr_total_invoice  
						WHERE (mag.fy = :li_fy) AND  
								(mag.cntr = :ls_contract) AND  
								(mag.magcd = :ls_magcode)   
						USING SqlServerTrans;
						
					IF SqlServerTrans.sqlCode = 0 THEN
					 COMMIT USING SqlServerTrans;	
					//Update the magiss table with the subscriber, size and minutes
					  UPDATE magiss  
					  set subs = :li_subs,   
							sz =   :lr_sz,   
							mins = :li_mins  
					  WHERE (magiss.magcd = :ls_magcode) AND  
							(magiss.issdt = :ldt_issuedate) 
					  USING SqlServerTrans;
				  
					IF SqlServerTrans.sqlCode <> 0 THEN
					  Messagebox('Error','Update Of MAGISS Table Failed')
					  ROLLBACK USING SqlServerTrans;
					ELSE
					 COMMIT USING SqlServerTrans;
					END IF
				
				
				
					ELSE
						Messagebox('Error','Update Of MAG Table Failed')
						ROLLBACK USING SqlServerTrans;
					END IF//IF SqlServerTrans.SqlCode THEN .. update for mag table
					
					
				ELSE
					Messagebox('Error','Update Of MAGINV Table Failed')
					ROLLBACK USING SqlServerTrans;
				END IF//IF li_rc = 1 THEN
		
		ELSE
			Messagebox('Status','Overide Code Needed To Process')
			dw_invoice_tracking.SetFocus()
		   dw_invoice_tracking.SetColumn("maginv_ovrcd")
			RETURN
		END IF//IF ((lr_diff > 1.00 OR ls_diff < -1.00) AND  (ls_ovrcode <> ''  ) OR 
		
		
		
		//Get Ready for next input 
			dw_invoice_tracking.Reset()
			dw_invoice_tracking_magcode_issuedate.Reset()
			dw_invoice_tracking_magcode_issuedate.enabled = TRUE
			dw_display_existing_invno.Reset()
			dw_invoice_tracking.EVENT pfc_addrow()
			dw_invoice_tracking_magcode_issuedate.EVENT pfc_addrow()
		
		//Disable dw_invoice_tracking and set focus to dw_invoice_tracking_magcode_issuedate for
		// next input.
			dw_invoice_tracking.enabled = FALSE
			dw_invoice_tracking_magcode_issuedate.object.magttl_magcd.Protect = 0
			dw_invoice_tracking_magcode_issuedate.object.magiss_issdt.Protect = 0
			dw_invoice_tracking_magcode_issuedate.SetFocus()
		
		//Hide the RC magazine datawindows and make visible the FD and BR
		   dw_invoice_tracking.visible = TRUE
		   dw_invoice_tracking_rc.visible = FALSE
			dw_invoice_tracking_est_rc.visible = FALSE
		
		
		//Disable Update button. It will be Enabled in the Itemchanged event of the data window
		//dw_invoice_tracking.
			cb_update.enabled = FALSE
         RETURN 2

END IF//IF dw_invoice_tracking_rc.Visible = TRUE THEN

end event

type cb_clear from u_cb within w_magazine_invoice_tracking
integer x = 2350
integer y = 1936
integer width = 242
integer height = 88
integer taborder = 0
boolean bringtotop = true
integer textsize = -10
string text = "&Clear"
end type

event clicked;call super::clicked;dw_invoice_tracking.reset()
dw_invoice_tracking_magcode_issuedate.reset()
dw_invoice_tracking_rc.reset()
dw_invoice_tracking_est_rc.reset()
dw_display_existing_invno.reset()

//Unprotect the fields in the datawindow
 dw_invoice_tracking_magcode_issuedate.Object.magttl_magcd.Protect = 0
 dw_invoice_tracking_magcode_issuedate.Object.magiss_issdt.Protect = 0

dw_display_existing_invno.visible=FALSE

w_mm_selection_list.em_magcd.text = ''
w_mm_selection_list.em_issdate.text = ''
w_mm_selection_list.em_magcd.SetFocus()
w_mm_selection_list.Ld_admindate = Ladmdt


close(w_magazine_invoice_tracking)
w_mm_selection_list.Show()
end event

type cb_exit from u_cb within w_magazine_invoice_tracking
integer x = 2661
integer y = 1936
integer width = 219
integer height = 88
integer taborder = 0
boolean bringtotop = true
integer textsize = -10
string text = "E&xit"
end type

event clicked;call super::clicked;Integer li_msg
String  ls_magcode

//If the invoice already exists then exit the screen.
ls_magcode = dw_invoice_tracking_magcode_issuedate.Object.magttl_magcd[1]
	IF NewInvoice = FALSE OR IsNull(ls_magcode) THEN
		dw_invoice_tracking.reset()
		dw_invoice_tracking_rc.reset()
		dw_invoice_tracking_est_rc.reset()
		dw_invoice_tracking_magcode_issuedate.reset()
		close(w_mm_selection_list)
		close(w_magazine_invoice_tracking)
		m_pics_main.m_menu.PopMenu ( 300, 0 ) 
	ELSEIF NewInvoice = TRUE THEN
     li_msg =	MessageBox('Status','Do you Want to Save Any Changes',Question!,YesNo!)
		IF li_msg = 1 THEN
			IF cb_update.Enabled = FALSE THEN
			 MessageBox('Error','Invoice Amount Needed To Process')
			 RETURN
		   ELSE 
			 cb_update.TriggerEvent (clicked!)
		   END IF//IF cb_update.Enabled = FALSE
		ELSE
			dw_invoice_tracking.reset()
		   dw_invoice_tracking_rc.reset()
		   dw_invoice_tracking_est_rc.reset()
		   dw_invoice_tracking_magcode_issuedate.reset()
		   close(w_mm_selection_list)
		   close(w_magazine_invoice_tracking)
		   m_pics_main.m_menu.PopMenu ( 300, 0 ) 
		END IF//
			
			
	END IF//

end event

type dw_invoice_tracking_magcode_issuedate from u_dw within w_magazine_invoice_tracking
event ue_enter_to_tab pbm_dwnprocessenter
integer x = 23
integer width = 2875
integer height = 132
integer taborder = 10
string dataobject = "d_magazine_invoice_tracking_magcode"
boolean livescroll = false
end type

event ue_enter_to_tab;call super::ue_enter_to_tab;
Send(Handle(this), 256, 9, Long(0,0))
Return(1)
end event

event constructor;call super::constructor;this.of_settransobject(SqlServerTrans)
//this.Event pfc_addrow()
end event

event itemchanged;call super::itemchanged;String ls_magcode, ls_title, ls_format, ls_date
Integer li_rtn_code
Date ld_issuedate, ld_shipdate
datetime ldt_shipdate

//Check if Magazine Code exists
IF DWO.Name = "magttl_magcd" THEN
	ls_magcode  = TRIM(GetText())
	li_rtn_code = wf_get_title(ls_magcode, ls_title)
  
	IF  li_rtn_code = 0 THEN
	   dw_invoice_tracking_magcode_issuedate.SetItem(1,"magttl_title", ls_title)
     
   ELSEIF li_rtn_code = 100 THEN
	  dw_invoice_tracking_magcode_issuedate.Object.magttl_magcd.ValidationMsg = 'Magazine Code Not Found' 
	  dw_invoice_tracking_magcode_issuedate.Event pfc_selectall()
     RETURN 1
   END IF//IF  li_rtn_code = 0 THEN

END IF//IF DWO.Name = "magttl_magcd" THEN


//Check if ship date is Null. IF null do not process.
IF DWO.Name = "magiss_issdt" THEN
	ls_magcode   = TRIM(dw_invoice_tracking_magcode_issuedate.Object.magttl_magcd[row])
	ls_title     = dw_invoice_tracking_magcode_issuedate.Object.magttl_title[row]
	ls_date=left(data,10)
	ld_issuedate= date(ls_date)

	 
	dw_invoice_tracking_magcode_issuedate.of_settransobject(SqlServerTrans)
	li_rtn_code = dw_invoice_tracking_magcode_issuedate.Retrieve(ls_magcode, ld_issuedate)
	//If no data then return
	IF li_rtn_code = 0 THEN
      dw_invoice_tracking_magcode_issuedate.Event pfc_addrow()
		MessageBox('Status','No Data For the Issue Date')
		dw_invoice_tracking_magcode_issuedate.Setitem(row, "magttl_magcd", ls_magcode)
		dw_invoice_tracking_magcode_issuedate.Setitem(row, "magttl_title", ls_title)
	   dw_invoice_tracking_magcode_issuedate.SetFocus()
		RETURN
	ELSEIF li_rtn_code = 1 THEN
		 ldt_shipdate = dw_invoice_tracking_magcode_issuedate.Object.magiss_issdt[row]
		 
		 //IF Shipdate is Null Then do not process Else Set focus to Datawindow dw_invoice_tracking
		 IF IsNull(ld_shipdate) THEN
			MessageBox('Status',"Not Allowed To Invoice an Issue ~r~n That Has Not Been Shipped")
		   RETURN
	     ELSE
		   ls_format = dw_invoice_tracking_magcode_issuedate.Object.magcntr_format[1]
         IF TRIM(ls_format) = 'RC' THEN
				dw_invoice_tracking.Visible = FALSE
				dw_invoice_tracking_rc.Event pfc_addrow()
				dw_invoice_tracking_rc.Visible = TRUE
				dw_invoice_tracking_est_rc.Event pfc_addrow()
				dw_invoice_tracking_est_rc.Visible = TRUE
				dw_invoice_tracking_rc.Object.maginv_invno.Protect = 0
				dw_invoice_tracking_rc.Object.maginv_invtype.Protect = 0
				dw_invoice_tracking_rc.SetFocus()
				dw_invoice_tracking_rc.Enabled = TRUE
				
				dw_invoice_tracking_magcode_issuedate.Enabled = FALSE
			ELSE// If format is BR or FD
			   dw_invoice_tracking_magcode_issuedate.Enabled = FALSE
				dw_invoice_tracking.Object.maginv_invno.Protect = 0
				dw_invoice_tracking.Visible = TRUE
				dw_invoice_tracking_rc.Enabled = FALSE
				dw_invoice_tracking_rc.Visible = FALSE
				dw_invoice_tracking_est_rc.Visible = FALSE
				dw_invoice_tracking.SetFocus()
				dw_invoice_tracking.Enabled = TRUE
							
		   END IF//IF TRIM(ls_format) = 'RC' THEN
			dw_display_existing_invno.retrieve(ls_magcode,ld_issuedate)
			
			
		 END IF//IF IsNull(ld_shipdate) THEN
		 
		 
		 
	END IF//IF li_rtn_code = 0 THEN
	
END IF//IF DWO.Name = "magiss_issdt" THEN
end event

event itemfocuschanged;call super::itemfocuschanged;

//If focus then pop up microhelp 
IF DWO.Name = "magttl_magcd" THEN
	dw_invoice_tracking_magcode_issuedate.Event pfc_selectall()
	w_pics_main.Event pfc_microhelp("Enter Magazine Code")
END IF



// If  focus then pop up microhelp
IF DWO.Name = "magiss_issdt" THEN
	dw_invoice_tracking_magcode_issuedate.Event pfc_selectall()
	
END IF
end event

type dw_display_existing_invno from datawindow within w_magazine_invoice_tracking
boolean visible = false
integer x = 27
integer y = 1572
integer width = 2862
integer height = 340
integer taborder = 40
string dataobject = "d_display_existing_invno"
boolean vscrollbar = true
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type dw_invoice_tracking_est_rc from u_dw within w_magazine_invoice_tracking
boolean visible = false
integer x = 27
integer y = 1096
integer width = 2862
integer height = 480
integer taborder = 0
string dataobject = "d_invoice_tracking_est_rc"
end type

event constructor;call super::constructor;this.of_SetTransObject(SqlServerTrans)

end event

type dw_invoice_tracking from u_dw within w_magazine_invoice_tracking
event ue_enter_to_tab pbm_dwnprocessenter
boolean visible = false
integer x = 27
integer y = 140
integer width = 2862
integer height = 964
integer taborder = 20
string dataobject = "d_magazine_invoice_tracking"
boolean vscrollbar = false
boolean livescroll = false
end type

event ue_enter_to_tab;call super::ue_enter_to_tab;//Set return key to follow tab path
Send(Handle(this), 256,9, Long(0,0))
RETURN (1)
end event

event itemchanged;call super::itemchanged;Integer li_fy, li_rtn_code,li_cnt
String  ls_cntr, ls_magcode, ls_invoice_number, ls_temp
DateTime    ldt_issuedate
Double  lr_invamt, lr_est_total

//Get data from data window dw_invoice_tracking_magcode_issuedate into local variables.
li_fy = dw_invoice_tracking_magcode_issuedate.object.magiss_fy[row]
ls_cntr = dw_invoice_tracking_magcode_issuedate.object.magiss_cntr[row]
ls_magcode = dw_invoice_tracking_magcode_issuedate.object.magttl_magcd[row]
ldt_issuedate = dw_invoice_tracking_magcode_issuedate.object.magiss_issdt[row]

//If invoice number has been changed then continue to process
IF DWO.Name = "maginv_invno" THEN
	ls_invoice_number = Data
	ls_invoice_number = Trim(ls_invoice_number)
	
	IF ls_invoice_number <> '' OR NOT(IsNull(ls_invoice_number)) THEN
		
		dw_invoice_tracking.of_settransobject(SqlServerTrans)
	 	li_rtn_code =   dw_invoice_tracking.Retrieve(li_fy, ls_magcode,ls_cntr, Date(ldt_issuedate), ls_invoice_number)
	 
		//Checks if the invoice number is Null. If it is null then no invoice exists 
		//and can continue to process. Set the tab sequence of the invoice tracking field to 0.
		//ls_temp = dw_invoice_tracking.Object.maginv_invno[row]	
		IF li_rtn_code > 0 THEN
			IF IsNull( dw_invoice_tracking.Object.maginv_invno[row]) THEN
				dw_invoice_tracking.SetFocus()
				dw_invoice_tracking.enabled = TRUE
				NewInvoice = TRUE
				dw_invoice_tracking.SetText(ls_invoice_number)
				dw_invoice_tracking.object.maginv_invno[1] = ls_invoice_number
				dw_invoice_tracking.object.maginv_pcsdt[1] = Date(Today())
					
				//Input Admin Date Instance Variable to the Data window else put in today's date.
				ladmdt = w_mm_selection_list.Ld_admindate
						
				IF IsNull(ladmdt) OR String(ladmdt) = '01/01/1900' OR String(ladmdt) = '1/1/00'  THEN
					dw_invoice_tracking.object.maginv_admdt[1] = Date(Today())
				ELSE
					dw_invoice_tracking.object.maginv_admdt[1] = Date(ladmdt)
				END IF
				// Enable the Update button
				  cb_update.enabled = TRUE
				// protect the overide code field			
				dw_invoice_tracking.object.maginv_ovrcd.Protect = '1'			
								
			ELSE// Invoice Number Already Exists
				NewInvoice = FALSE
				Messagebox('Status','Invoice Already Exists .. No Update Possible')         
				dw_invoice_tracking.enabled = FALSE
				cb_update.enabled = FALSE
				RETURN 
			END IF// IF IsNull(ls_temp) THEN
		ELSE // IF li_rtn_code = 0
				dw_invoice_tracking.SetFocus()
				dw_invoice_tracking.enabled = TRUE
				NewInvoice = TRUE
				dw_invoice_tracking.InsertRow(0)
				dw_invoice_tracking.SetText(ls_invoice_number)
				dw_invoice_tracking.object.maginv_invno[1] = ls_invoice_number
				dw_invoice_tracking.object.maginv_pcsdt[1] = Date(Today())
				
				//Input Admin Date Instance Variable to the Data window else put in today's date.
				ladmdt = w_mm_selection_list.Ld_admindate
						
				IF IsNull(ladmdt) OR String(ladmdt) = '01/01/1900' OR String(ladmdt) = '1/1/00'  THEN
					dw_invoice_tracking.object.maginv_admdt[1] = Date(Today())
				ELSE
					dw_invoice_tracking.object.maginv_admdt[1] = Date(ladmdt)
				END IF
				//Enable the Update button
				  cb_update.enabled = TRUE
				//protect the overide code field			
				dw_invoice_tracking.object.maginv_ovrcd.Protect = '1'			
		END IF
 	ELSE
	   RETURN 1
 	END IF//	IF TRIM(ls_invoice_number) <> '' THEN
END IF//IF DWO.Name = "maginv_invno" THEN

//If invoice amount is changed then process
IF DWO.Name = "maginv_invamt" THEN
	lr_invamt = Double(GetText())
	lr_est_total = dw_invoice_tracking.object.est_total[1]
       IF IsNull(lr_est_total) THEN lr_est_total = 0
	//If invoice amount greater than estimated total by a dollar or
	//less than the estimated total by a dollar then unprotect the overide code.
	IF lr_invamt > (lr_est_total + 1.00) OR  lr_invamt < (lr_est_total - 1.00)  THEN
		dw_invoice_tracking.object.maginv_ovrcd.Protect = 0
	ELSE
		dw_invoice_tracking.object.maginv_ovrcd.Protect = 1		
	END IF//IF ( lr_invamt > (lr_est_total + 1.00) OR ( lr_invamt < (lr_est_total - 1.00) ) THEN
 END IF// DWO.Name = "maginv_invamt" THEN

//IF admin date greater than today then cannot process

IF DWO.Name = "maginv_admdt" THEN
 	ls_invoice_number = dw_invoice_tracking.object.maginv_invno[1] 
   	IF Date(GetText()) > Today() THEN
	 	dw_invoice_tracking.object.maginv_admdt.Validationmsg = "Admin. Date Cannot Be Greater than today's Date"	
		 RETURN 1
	ELSE
		ladmdt = Date(GetText())
	END IF	
END IF//IF DWO.Name = "maginv_admdt" THEN





end event

event getfocus;call super::getfocus;w_pics_main.Event pfc_microhelp("Enter Invoice Number")


end event

event itemfocuschanged;call super::itemfocuschanged;//Set Microhelp when in focus
IF DWO.Name = "maginv_ovrcd" THEN
	w_pics_main.Event pfc_microhelp("Enter Override Code")
ELSEIF DWO.Name = "maginv_invamt" THEN
	w_pics_main.Event pfc_microhelp("Enter Enter Invoice Amount ")
	dw_invoice_tracking.Event pfc_selectall()
ELSEIF DWO.Name = "maginv_pcsdt" THEN
	w_pics_main.EVENT pfc_microhelp("Enter PCS Date")
	dw_invoice_tracking.Event pfc_selectall()
ELSEIF DWO.Name = "magiss_subs" THEN
	w_pics_main.EVENT pfc_microhelp("Enter Subscribers")
ELSEIF  DWO.Name = "magiss_sz" THEN
	w_pics_main.Event pfc_microhelp("Enter Size")
ELSEIF DWO.Name = "maginv_invmail" THEN
	w_pics_main.Event pfc_microhelp("Enter Invoice Mail Cost")
       dw_invoice_tracking.Event pfc_selectall()
END IF


end event

event constructor;call super::constructor;this.of_settransobject(SqlServerTrans)
this.Event pfc_addrow()
end event

event rbuttondown;//
end event

event rbuttonup;//
end event

type dw_invoice_tracking_rc from u_dw within w_magazine_invoice_tracking
event ue_enter_to_tab pbm_dwnprocessenter
boolean visible = false
integer x = 27
integer y = 140
integer width = 2862
integer height = 968
integer taborder = 30
string dataobject = "d_magazine_invoice_tracking_rc"
boolean vscrollbar = false
boolean livescroll = false
end type

event ue_enter_to_tab;call super::ue_enter_to_tab;Send(Handle(this), 256,9, Long(0,0))
RETURN (1)
end event

event constructor;call super::constructor;this.of_settransobject(SqlServerTrans)
this.Event pfc_addrow()
end event

event itemchanged;call super::itemchanged;Integer li_fy, li_rtn_code
String  ls_contract, ls_magcode, ls_invoice_number, ls_temp, ls_invoice_type, ls_admdt
Date    ld_issuedate
Double  lr_invamt, lr_est_total
DateTime ldt_issdt


////Get data from data window dw_invoice_tracking_magcode_issuedate into local variables.
//	li_fy = dw_invoice_tracking_magcode_issuedate.Object.magiss_fy[row]
//	ls_contract = dw_invoice_tracking_magcode_issuedate.Object.magiss_cntr[row]
//	ls_magcode = dw_invoice_tracking_magcode_issuedate.Object.magttl_magcd[row]
//	ld_issuedate = dw_invoice_tracking_magcode_issuedate.Object.magiss_issdt[row]
//
ls_admdt=String(ladmdt)
//If Invoice type has been changed then Retrieve
IF DWO.Name = "maginv_invtype" THEN
	   
		
 //Get data from data window dw_invoice_tracking_magcode_issuedate into local variables.
	li_fy = dw_invoice_tracking_magcode_issuedate.object.magiss_fy[row]
	ls_contract = dw_invoice_tracking_magcode_issuedate.object.magiss_cntr[row]
	ls_magcode = dw_invoice_tracking_magcode_issuedate.object.magttl_magcd[row]
	ldt_issdt = dw_invoice_tracking_magcode_issuedate.object.magiss_issdt[row]
	ld_issuedate = Date(ldt_issdt)
   
   //Get data from dw_invoice_tracking_rc
   ls_invoice_number = dw_invoice_tracking_rc.object.maginv_invno[row]
   ls_invoice_type   = Data
   li_rtn_code =   dw_invoice_tracking_rc.Retrieve(li_fy, ls_magcode, ls_contract, ld_issuedate,ls_invoice_number,ls_invoice_type)
  	 
		
	//Retrieve data for view only data window dw_magazine_invoice_tracking_est_rc	
	//and set the invoice type in the datawindow.
//	  dw_invoice_tracking_est_rc.Retrieve(ls_magcode,ls_contract,li_fy)	
//	  dw_invoice_tracking_est_rc.object.maginv_invtype[1] = ls_invoice_type	
		
		
	//If there is no invoice number then put the invoice number and 	 
	//and the invoice type back into the datawindow and ...
	ls_temp = dw_invoice_tracking_rc.object.maginv_invno[row]
	IF IsNull(ls_temp) THEN
		dw_invoice_tracking_rc.object.maginv_invno[row] = ls_invoice_number
		dw_invoice_tracking_rc.object.maginv_invtype[row] = ls_invoice_type
		dw_invoice_tracking_rc.object.maginv_invno.Protect = 1
		dw_invoice_tracking_rc.object.maginv_invtype.Protect = 1
    
		//Input Admin Date Instance Variable to the Data window else put in today's date.
			ladmdt = w_mm_selection_list.Ld_admindate
			
		
			IF IsNull(ladmdt) OR String(ladmdt) = '01/01/1900' OR String(ladmdt) = '1/1/00'  THEN
				dw_invoice_tracking_rc.object.maginv_admdt[1] = Today()
			ELSE
				dw_invoice_tracking_rc.object.maginv_admdt[1] = ladmdt
			END IF
			
		//set the pcsdate to todays date	
		dw_invoice_tracking_rc.object.maginv_pcsdt[row] = Today()
		//Set Instance variable NewInvoice to TRUE
		NewInvoice = TRUE
		//protect the overide code field			
		 dw_invoice_tracking_rc.object.maginv_ovrcd.Protect = '1'	   			 
						 
						 
   ELSE
      dw_invoice_tracking_rc.enabled = FALSE
		Messagebox('Status','Invoice Already Exists .. No Update Possible')
		NewInvoice = FALSE
		RETURN 
	END IF//IF IsNull(ls_temp) THEN
		 
		 
END IF//	IF DWO.Name = "maginv_invtype" THEN

//If invoice amount is changed then process. Enable the update button
IF DWO.Name = "maginv_invamt" THEN
	lr_invamt = Double(GetText())
	lr_est_total = dw_invoice_tracking_rc.object.total[1]

				
	IF lr_invamt > (lr_est_total + 1.00) OR  lr_invamt < (lr_est_total - 1.00)  THEN
		dw_invoice_tracking_rc.object.maginv_ovrcd.Protect = 0
	ELSE
		dw_invoice_tracking_rc.object.maginv_ovrcd.Protect = 1
		
	END IF//IF ( lr_invamt > (lr_est_total + 1.00) OR ( lr_invamt < (lr_est_total - 1.00) ) THEN
	
	cb_update.enabled = TRUE
END IF// DWO.Name = "maginv_invamt" THEN

IF DWO.Name = "maginv_admdt" THEN
   
		IF Date(GetText()) > Today() THEN
		 dw_invoice_tracking_rc.object.maginv_admdt.Validationmsg = "Admin. Date Cannot Be Greater than today's Date"	
		 RETURN 1
		ELSE
			ladmdt = Date(GetText())
		END IF
	
END IF//IF DWO.Name = "maginv_admdt" THEN

end event


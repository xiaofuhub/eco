$PBExportHeader$w_voucher_retrieve.srw
forward
global type w_voucher_retrieve from w_response
end type
type dw_detail_voucher from u_dw within w_voucher_retrieve
end type
type dw_detail_voucher_pcsdt from u_dw within w_voucher_retrieve
end type
type dw_voucher_retrieve from u_dw within w_voucher_retrieve
end type
type cb_pdf from commandbutton within w_voucher_retrieve
end type
type cbx_pcsdate from u_cbx within w_voucher_retrieve
end type
type cbx_contract from u_cbx within w_voucher_retrieve
end type
type st_1 from u_st within w_voucher_retrieve
end type
type st_2 from u_st within w_voucher_retrieve
end type
type em_startpcs_date from u_em within w_voucher_retrieve
end type
type em_pcsend_date from u_em within w_voucher_retrieve
end type
type gb_range from u_gb within w_voucher_retrieve
end type
type cb_find from u_cb within w_voucher_retrieve
end type
type cb_clear from u_cb within w_voucher_retrieve
end type
type cb_exit from u_cb within w_voucher_retrieve
end type
type st_3 from u_st within w_voucher_retrieve
end type
type st_4 from u_st within w_voucher_retrieve
end type
type sle_contract_number from u_sle within w_voucher_retrieve
end type
type em_admin_date from u_em within w_voucher_retrieve
end type
type st_pcs_date from u_st within w_voucher_retrieve
end type
type em_pcs_date from u_em within w_voucher_retrieve
end type
type gb_2 from u_gb within w_voucher_retrieve
end type
end forward

global type w_voucher_retrieve from w_response
integer x = 434
integer y = 412
integer width = 2222
integer height = 1208
string title = "Voucher Report"
dw_detail_voucher dw_detail_voucher
dw_detail_voucher_pcsdt dw_detail_voucher_pcsdt
dw_voucher_retrieve dw_voucher_retrieve
cb_pdf cb_pdf
cbx_pcsdate cbx_pcsdate
cbx_contract cbx_contract
st_1 st_1
st_2 st_2
em_startpcs_date em_startpcs_date
em_pcsend_date em_pcsend_date
gb_range gb_range
cb_find cb_find
cb_clear cb_clear
cb_exit cb_exit
st_3 st_3
st_4 st_4
sle_contract_number sle_contract_number
em_admin_date em_admin_date
st_pcs_date st_pcs_date
em_pcs_date em_pcs_date
gb_2 gb_2
end type
global w_voucher_retrieve w_voucher_retrieve

forward prototypes
public subroutine wf_disable_all ()
public subroutine wf_disable_contract ()
public subroutine wf_disable_start_end_date ()
end prototypes

public subroutine wf_disable_all ();cb_find.Enabled = FALSE
cb_find.Default = FALSE
gb_2.Visible = TRUE
gb_range.Visible = TRUE
em_admin_date.Visible = TRUE
em_pcsend_date.visible = TRUE
em_startpcs_date.Visible = TRUE
sle_contract_number.visible = TRUE
st_1.Visible = TRUE
st_2.Visible = TRUE
st_3.Visible = TRUE
st_4.visible = TRUE
em_admin_date.text = ''
em_pcsend_date.text = ''
em_startpcs_date.text = ''
sle_contract_number.text = ''
cbx_contract.checked = FALSE
em_startpcs_date.Setfocus()
end subroutine

public subroutine wf_disable_contract ();cb_find.Enabled = true
cb_find.Default = true
gb_2.Visible = false
gb_range.Visible = TRUE
em_pcs_date.visible= false
em_admin_date.Visible = false
em_pcsend_date.visible = TRUE
em_startpcs_date.Visible = TRUE
sle_contract_number.visible = false
st_1.Visible = TRUE
st_2.Visible = TRUE
st_3.Visible = false
st_4.visible = false
em_admin_date.text = ''
em_pcsend_date.text = ''
em_startpcs_date.text = ''
sle_contract_number.text = ''
cbx_contract.checked = FALSE
em_startpcs_date.Setfocus()
end subroutine

public subroutine wf_disable_start_end_date ();cb_find.Enabled = true
cb_find.Default = true
gb_2.Visible = TRUE
gb_range.Visible = FALSE
em_pcs_date.visible= TRUE
em_admin_date.Visible = TRUE
em_pcsend_date.visible = FALSE
em_startpcs_date.Visible = FALSE
sle_contract_number.visible = TRUE
st_1.Visible = FALSE
st_2.Visible = FALSE
st_3.Visible = TRUE
st_4.visible = TRUE
st_pcs_date.visible =true
em_admin_date.text = ''
em_pcs_date.TEXT =''
em_pcsend_date.text = ''
em_startpcs_date.text = ''
sle_contract_number.text = ''
cbx_contract.checked = TRUE
em_admin_date.Setfocus()
//em_startpcs_date.Setfocus()
end subroutine

on w_voucher_retrieve.create
int iCurrent
call super::create
this.dw_detail_voucher=create dw_detail_voucher
this.dw_detail_voucher_pcsdt=create dw_detail_voucher_pcsdt
this.dw_voucher_retrieve=create dw_voucher_retrieve
this.cb_pdf=create cb_pdf
this.cbx_pcsdate=create cbx_pcsdate
this.cbx_contract=create cbx_contract
this.st_1=create st_1
this.st_2=create st_2
this.em_startpcs_date=create em_startpcs_date
this.em_pcsend_date=create em_pcsend_date
this.gb_range=create gb_range
this.cb_find=create cb_find
this.cb_clear=create cb_clear
this.cb_exit=create cb_exit
this.st_3=create st_3
this.st_4=create st_4
this.sle_contract_number=create sle_contract_number
this.em_admin_date=create em_admin_date
this.st_pcs_date=create st_pcs_date
this.em_pcs_date=create em_pcs_date
this.gb_2=create gb_2
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_detail_voucher
this.Control[iCurrent+2]=this.dw_detail_voucher_pcsdt
this.Control[iCurrent+3]=this.dw_voucher_retrieve
this.Control[iCurrent+4]=this.cb_pdf
this.Control[iCurrent+5]=this.cbx_pcsdate
this.Control[iCurrent+6]=this.cbx_contract
this.Control[iCurrent+7]=this.st_1
this.Control[iCurrent+8]=this.st_2
this.Control[iCurrent+9]=this.em_startpcs_date
this.Control[iCurrent+10]=this.em_pcsend_date
this.Control[iCurrent+11]=this.gb_range
this.Control[iCurrent+12]=this.cb_find
this.Control[iCurrent+13]=this.cb_clear
this.Control[iCurrent+14]=this.cb_exit
this.Control[iCurrent+15]=this.st_3
this.Control[iCurrent+16]=this.st_4
this.Control[iCurrent+17]=this.sle_contract_number
this.Control[iCurrent+18]=this.em_admin_date
this.Control[iCurrent+19]=this.st_pcs_date
this.Control[iCurrent+20]=this.em_pcs_date
this.Control[iCurrent+21]=this.gb_2
end on

on w_voucher_retrieve.destroy
call super::destroy
destroy(this.dw_detail_voucher)
destroy(this.dw_detail_voucher_pcsdt)
destroy(this.dw_voucher_retrieve)
destroy(this.cb_pdf)
destroy(this.cbx_pcsdate)
destroy(this.cbx_contract)
destroy(this.st_1)
destroy(this.st_2)
destroy(this.em_startpcs_date)
destroy(this.em_pcsend_date)
destroy(this.gb_range)
destroy(this.cb_find)
destroy(this.cb_clear)
destroy(this.cb_exit)
destroy(this.st_3)
destroy(this.st_4)
destroy(this.sle_contract_number)
destroy(this.em_admin_date)
destroy(this.st_pcs_date)
destroy(this.em_pcs_date)
destroy(this.gb_2)
end on

event resize;call super::resize;THIS.X = 476
THIS.Y = 509
THIS.Width = 2108
THIS.Height = 989
end event

event pfc_postopen;call super::pfc_postopen;cb_find.enabled = FALSE
cb_find.Default = FALSE
gb_2.Visible = FALSE
gb_range.Visible = FALSE
em_admin_date.Visible = FALSE
em_pcsend_date.visible = FALSE
em_startpcs_date.Visible = FALSE
sle_contract_number.visible = FALSE
st_1.Visible = FALSE
st_2.Visible = FALSE
st_3.Visible = FALSE
st_4.visible = FALSE
em_admin_date.text = ''
em_pcsend_date.text = ''
em_startpcs_date.text = ''
sle_contract_number.text = ''
cbx_contract.checked = FALSE
st_pcs_date.Visible = FALSE
em_pcs_date.Visible = FALSE
//em_startpcs_date.Setfocus()


end event

event key;call super::key;IF key = Keyenter! THEN
	IF gb_2.visible = TRUE THEN
		IF sle_contract_number.Text = '' THEN
	      Messagebox("Required Data","Please enter valid contract number.")
	      cb_find.Enabled = FALSE
	      cb_find.Default = FALSE
	      sle_contract_number.Setfocus()
	      RETURN
		ELSE
	      cb_find.Enabled = TRUE
         cb_find.Default = TRUE
		END IF
   END IF
END IF




end event

type dw_detail_voucher from u_dw within w_voucher_retrieve
boolean visible = false
integer x = 1097
integer y = 896
integer width = 256
integer height = 128
integer taborder = 80
string dataobject = "d_voucher_contract_update"
boolean vscrollbar = false
boolean border = false
boolean livescroll = false
borderstyle borderstyle = stylebox!
end type

event constructor;call super::constructor;of_settransobject(sqlservertrans)
of_setprintpreview(TRUE)
of_setFilter(TRUE)
of_setFind(TRUE)
of_setSort(TRUE)
end event

type dw_detail_voucher_pcsdt from u_dw within w_voucher_retrieve
boolean visible = false
integer x = 622
integer y = 896
integer width = 219
integer height = 128
integer taborder = 90
string dataobject = "d_voucher_contract_update_pcsdt"
boolean vscrollbar = false
boolean border = false
boolean livescroll = false
borderstyle borderstyle = stylebox!
end type

event constructor;call super::constructor;of_settransobject(sqlservertrans)
of_setprintpreview(TRUE)
of_setFilter(TRUE)
of_setFind(TRUE)
of_setSort(TRUE)
end event

type dw_voucher_retrieve from u_dw within w_voucher_retrieve
boolean visible = false
integer x = 110
integer y = 896
integer width = 219
integer height = 128
integer taborder = 80
string dataobject = "d_voucher_pcs_dates"
boolean vscrollbar = false
boolean border = false
boolean livescroll = false
end type

event constructor;of_settransobject(sqlservertrans)




end event

event retrieveend;call super::retrieveend;Close(w_pics_retrieve_msg_box)

end event

event retrievestart;call super::retrievestart;Open(w_pics_retrieve_msg_box)
end event

type cb_pdf from commandbutton within w_voucher_retrieve
integer x = 1536
integer y = 672
integer width = 475
integer height = 96
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "Print"
end type

event clicked;int rtn,i,ll_rows
string ls_contract,ls_pdf_created_by
datetime ld_admin_date,ld_pcsdt
Boolean pdf_not_created = FALSE


IF cbx_pcsdate.checked = TRUE THEN
	ll_rows = dw_voucher_retrieve.Retrieve(Date(em_startpcs_date.text), Date(em_pcsend_date.text) )
	IF ll_rows > 0 THEN	
		// Go through the loop and print
		FOR i = 1 TO ll_rows
			
			// Contract number, Admin date and PCS date
			ls_contract = dw_voucher_retrieve.object.magcntr_cntr[i]
			ld_admin_date = dw_voucher_retrieve.object.maginv_admdt[i]
			ld_pcsdt = dw_voucher_retrieve.object.maginv_pcsdt[i]
			
			// pdf created by someone
			ls_pdf_created_by = dw_voucher_retrieve.object.pdf_created_by[i]
		
		       // If the pdf was created then print 
			IF NOT(IsNull(ls_pdf_created_by)) or ls_pdf_created_by <> "" THEN
				
				// Retrieve the voucher informations
				rtn = dw_detail_voucher_pcsdt.retrieve(ls_contract,ld_admin_date,ld_pcsdt)
					
				// If records exist for voucher then print
				IF rtn > 0 THEN						
					// Print  the voucher(s). For the first time, display the printer dialog box, but after that just execute print() function
					IF i =1 THEN
						dw_detail_voucher_pcsdt.TriggerEvent("pfc_Print")
					ELSE
						dw_detail_voucher_pcsdt.print()
					END IF
				END IF
			ELSE
				pdf_not_created = TRUE
			END IF
		NEXT
	ELSE
		MessageBox("ERROR","No Data Found",StopSign!)
	END IF

	// Display a message if HTML were not created yet
 	IF pdf_not_created  THEN
		Messagebox("Warning"," Make sure that voucher number(s) are assinged and HTML(s) are created before printing them.",Information!)
	END IF

ELSEIF cbx_contract.checked = TRUE THEN
	dw_voucher_retrieve.dataobject = 'd_voucher_addate_cntrno'
	dw_voucher_retrieve.settransobject(sqlservertrans)
	ll_rows = dw_voucher_retrieve.Retrieve(Date(em_admin_date.text),sle_contract_number.text,Date(em_pcs_date.text))
	IF ll_rows > 0 THEN	
		// Go through the loop and print
		FOR i = 1 TO ll_rows
			
			// Contract number, Admin date and PCS date
			ls_contract = dw_voucher_retrieve.object.magcntr_cntr[i]
			ld_admin_date = dw_voucher_retrieve.object.maginv_admdt[i]
			ld_pcsdt = dw_voucher_retrieve.object.maginv_pcsdt[i]
			
			// pdf created by someone
			ls_pdf_created_by = dw_voucher_retrieve.object.pdf_created_by[i]
		
		       // If the pdf was created then print 
			IF NOT(IsNull(ls_pdf_created_by)) or ls_pdf_created_by <> "" THEN
				
				// Retrieve the voucher informations
				rtn = dw_detail_voucher.retrieve(ls_contract,ld_admin_date,ld_pcsdt)
					
				// If records exist then
				IF rtn > 0 THEN
						
					// Print  the voucher(s). For the first time, display the printer dialog box, but after that just execute print() function
					IF i =1 THEN
						dw_detail_voucher.TriggerEvent("pfc_Print")
					ELSE
						dw_detail_voucher.print()
					END IF
				END IF
			ELSE
				pdf_not_created = TRUE
			END IF
		NEXT
	ELSE
		MessageBox("ERROR","No Data Found",StopSign!)
	END IF
	
	// Display a message if HTML were not created yet
 	IF pdf_not_created  THEN
		Messagebox("Warning"," Make sure that voucher number(s) are assinged and HTML(s) are created before printing them.",Information!)
	END IF

END IF



end event

type cbx_pcsdate from u_cbx within w_voucher_retrieve
string tag = "Please check this to add range of dates"
integer x = 247
integer y = 76
integer width = 439
integer height = 68
integer taborder = 10
integer textsize = -10
long backcolor = 79741120
string text = "All Contracts"
end type

event clicked;call super::clicked;IF cbx_pcsdate.Checked = TRUE THEN
	gb_2.Visible = FALSE
	em_admin_date.Visible = FALSE
	sle_contract_number.Visible = FALSE
	st_3.Visible = FALSE
	st_4.Visible = FALSE
	em_pcs_date.Visible = FALSE
	st_pcs_date.Visible = FALSE   
	gb_range.Visible = TRUE
	em_pcsend_date.Visible = TRUE
	st_1.Visible = TRUE
	st_2.Visible = TRUE
	cbx_contract.checked = FALSE
	em_startpcs_date.Visible = TRUE
	em_startpcs_date.Setfocus()
END IF
	
end event

type cbx_contract from u_cbx within w_voucher_retrieve
integer x = 1211
integer y = 80
integer width = 576
integer height = 68
integer taborder = 40
integer textsize = -10
string text = "Selected Contract"
end type

event clicked;call super::clicked;IF cbx_contract.Checked = TRUE THEN
	gb_range.Visible = FALSE
	em_startpcs_date.Visible = FALSE
	em_pcsend_date.Visible = FALSE
	st_1.Visible = FALSE
	st_2.Visible = FALSE
	cbx_pcsdate.Checked = FALSE	
	gb_2.Visible = TRUE
	sle_contract_number.Visible = TRUE
	st_3.visible = TRUE
	st_4.visible = TRUE
	em_pcs_date.Visible = TRUE
	st_pcs_date.Visible = TRUE
   em_admin_date.Visible = TRUE
	em_admin_date.setfocus()
END IF
	
end event

type st_1 from u_st within w_voucher_retrieve
integer x = 46
integer y = 300
integer width = 498
integer textsize = -10
string text = "Starting PCS Date"
end type

type st_2 from u_st within w_voucher_retrieve
integer x = 46
integer y = 440
integer width = 475
integer textsize = -10
string text = "Ending PCS Date"
end type

type em_startpcs_date from u_em within w_voucher_retrieve
string tag = "Plese enter valid pcs start date"
integer x = 562
integer y = 280
integer width = 370
integer height = 100
integer taborder = 20
integer textsize = -10
integer weight = 700
maskdatatype maskdatatype = datemask!
string mask = "mm/dd/yyyy"
boolean autoskip = true
string displaydata = "D"
double increment = 0
string minmax = ""
end type

type em_pcsend_date from u_em within w_voucher_retrieve
string tag = "Please enter valid pcs end date"
integer x = 562
integer y = 416
integer width = 370
integer height = 96
integer taborder = 30
integer textsize = -10
integer weight = 700
textcase textcase = upper!
maskdatatype maskdatatype = datemask!
string mask = "mm/dd/yyyy"
string displaydata = ""
double increment = 0
string minmax = ""
end type

event modified;call super::modified;IF NOT ISDATE(em_pcsend_date.text) OR em_pcsend_date.text = '00/00/0000' THEN
	Messagebox("Required Data","Please enter valid PCS end date.")
	cb_find.Enabled = FALSE
	cb_find.Default = FALSE
	em_pcsend_date.Setfocus()
	RETURN
ELSE
	cb_find.Enabled = TRUE
   cb_find.Default = TRUE	
END IF




end event

type gb_range from u_gb within w_voucher_retrieve
integer x = 23
integer y = 220
integer width = 946
integer height = 324
integer taborder = 0
long backcolor = 79741120
string text = ""
end type

type cb_find from u_cb within w_voucher_retrieve
string tag = "Find~'s the record based on all contracts or selected contract"
integer x = 96
integer y = 672
integer taborder = 0
integer textsize = -10
string text = "F&ind"
end type

event clicked;call super::clicked;str_voucher_report lstr_voucher_report

IF cbx_pcsdate.checked = TRUE THEN

   IF NOT IsDate(em_startpcs_date.text) OR NOT IsDate(em_pcsend_date.text) THEN
	   Messagebox("REQUIRED DATE","Please enter valid PCS start date and PCS end date to retrieve the records.")
	   wf_disable_all()
      	   cbx_pcsdate.checked = TRUE
	   em_startpcs_date.SetFocus()
	   RETURN	
   ELSE
	   lstr_voucher_report.ld_date = Date(em_startpcs_date.text)
	   lstr_voucher_report.ld_date1 = Date(em_pcsend_date.text)
	   IF lstr_voucher_report.ld_date> lstr_voucher_report.ld_date1 THEN
		 Messagebox("Error!"," end date must after the start date")
		 RETURN
	   END IF
	   OpenSheetWithParm(w_voucher_retrieve_update,lstr_voucher_report,w_pics_main,0,original!)
	   RETURN	
   END IF
ELSEIF cbx_contract.checked = TRUE THEN
    IF NOT IsDate(em_admin_date.text)THEN
	    Messagebox("REQUIRED DATA","Please enter valid admin date and valid contract to retrieve the records.")
	    wf_disable_all()
	    cbx_contract.checked = TRUE
	    em_admin_date.SetFocus()
	    RETURN
    ELSE
	   lstr_voucher_report.ld_dates = Date(em_admin_date.text)
	   lstr_voucher_report.ls_contract = sle_contract_number.text
	   lstr_voucher_report.ld_pcdt = Date(em_pcs_date.text)
	   OpenSheetWithParm(w_voucher_retrieve_update,lstr_voucher_report,w_pics_main,0,original!)
    END IF
END IF
	
//	IF cbx_pcsdate.checked = TRUE THEN
//
//   IF NOT IsDate(em_startpcs_date.text) OR NOT IsDate(em_pcsend_date.text) THEN
//	   Messagebox("REQUIRED DATE","Please enter valid PCS start date and PCS end date to retrieve the records.")
//	   wf_disable_all()
//      	   cbx_pcsdate.checked = TRUE
//	   em_startpcs_date.SetFocus()
//	   RETURN	
//   ELSE
//	   lstr_voucher_report.ld_date = Date(em_startpcs_date.text)
//	   lstr_voucher_report.ld_date1 = Date(em_pcsend_date.text)
//	   IF lstr_voucher_report.ld_date> lstr_voucher_report.ld_date1 THEN
//		 Messagebox("Error!"," end date must after the start date")
//		 RETURN
//	   END IF
//	   OpenSheetWithParm(w_voucher_retrieve_update_new,lstr_voucher_report,w_pics_main,0,original!)
//	   RETURN	
//   END IF
//ELSEIF cbx_contract.checked = TRUE THEN
//    IF NOT IsDate(em_admin_date.text)THEN
//	    Messagebox("REQUIRED DATA","Please enter valid admin date and valid contract to retrieve the records.")
//	    wf_disable_all()
//	    cbx_contract.checked = TRUE
//	    em_admin_date.SetFocus()
//	    RETURN
//    ELSE
//	   lstr_voucher_report.ld_dates = Date(em_admin_date.text)
//	   lstr_voucher_report.ls_contract = sle_contract_number.text
//	   lstr_voucher_report.ld_pcdt = Date(em_pcs_date.text)
//	   OpenSheetWithParm(w_voucher_retrieve_update_new,lstr_voucher_report,w_pics_main,0,original!)
//    END IF
//END IF
end event

type cb_clear from u_cb within w_voucher_retrieve
string tag = "Clear the data"
integer x = 558
integer y = 672
integer taborder = 0
integer textsize = -10
string text = "&Clear"
end type

event clicked;call super::clicked;cb_find.enabled = FALSE
cb_find.Default = FALSE
gb_2.Visible = FALSE
gb_range.Visible = FALSE
em_admin_date.Visible = FALSE
em_pcsend_date.visible = FALSE
em_startpcs_date.Visible = FALSE
sle_contract_number.visible = FALSE
st_1.Visible = FALSE
st_2.Visible = FALSE
st_3.Visible = FALSE
st_4.visible = FALSE
st_pcs_date.visible = FALSE
em_pcs_date.Visible = FALSE
em_pcs_date.text = ''
em_admin_date.text = ''
em_pcsend_date.text = ''
em_startpcs_date.text = ''
sle_contract_number.text = ''
cbx_contract.checked = FALSE
cbx_pcsdate.checked = FALSE


end event

type cb_exit from u_cb within w_voucher_retrieve
integer x = 1019
integer y = 672
integer taborder = 0
integer textsize = -10
string text = "E&xit"
end type

event clicked;call super::clicked;Parent.Event pfc_close()
end event

type st_3 from u_st within w_voucher_retrieve
integer x = 1115
integer y = 248
integer width = 320
integer textsize = -10
string text = "Admin Date"
end type

type st_4 from u_st within w_voucher_retrieve
integer x = 1115
integer y = 504
integer width = 462
integer textsize = -10
string text = "Contract Number"
end type

type sle_contract_number from u_sle within w_voucher_retrieve
integer x = 1600
integer y = 488
integer width = 375
integer height = 100
integer taborder = 70
integer textsize = -10
integer weight = 700
textcase textcase = upper!
integer limit = 7
end type

type em_admin_date from u_em within w_voucher_retrieve
integer x = 1600
integer y = 240
integer width = 370
integer height = 100
integer taborder = 50
integer textsize = -10
integer weight = 700
maskdatatype maskdatatype = datemask!
string mask = "mm/dd/yyyy"
boolean autoskip = true
string displaydata = ""
double increment = 0
string minmax = ""
end type

type st_pcs_date from u_st within w_voucher_retrieve
integer x = 1115
integer y = 376
integer width = 283
integer textsize = -10
string text = "PCS Date"
end type

type em_pcs_date from u_em within w_voucher_retrieve
integer x = 1600
integer y = 364
integer width = 370
integer height = 100
integer taborder = 60
integer textsize = -10
integer weight = 700
maskdatatype maskdatatype = datemask!
string mask = "mm/dd/yyyy"
boolean autoskip = true
string displaydata = ""
double increment = 0
string minmax = ""
end type

type gb_2 from u_gb within w_voucher_retrieve
integer x = 1051
integer y = 160
integer width = 946
integer height = 480
integer taborder = 0
string text = ""
end type


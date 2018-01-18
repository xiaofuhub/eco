$PBExportHeader$w_voucher_retrieve_update.srw
forward
global type w_voucher_retrieve_update from w_sheet
end type
type st_voucher from statictext within w_voucher_retrieve_update
end type
type cb_exit from u_cb within w_voucher_retrieve_update
end type
type cb_update from u_cb within w_voucher_retrieve_update
end type
type cb_select_contract from u_cb within w_voucher_retrieve_update
end type
type cb_print_all from u_cb within w_voucher_retrieve_update
end type
type dw_voucher_retrieve from u_dw within w_voucher_retrieve_update
end type
type dw_detail_voucher_pcsdt from u_dw within w_voucher_retrieve_update
end type
type dw_detail_voucher from u_dw within w_voucher_retrieve_update
end type
end forward

global type w_voucher_retrieve_update from w_sheet
integer width = 2798
integer height = 1376
string title = "Voucher Report"
st_voucher st_voucher
cb_exit cb_exit
cb_update cb_update
cb_select_contract cb_select_contract
cb_print_all cb_print_all
dw_voucher_retrieve dw_voucher_retrieve
dw_detail_voucher_pcsdt dw_detail_voucher_pcsdt
dw_detail_voucher dw_detail_voucher
end type
global w_voucher_retrieve_update w_voucher_retrieve_update

type variables
int ll_pdf_row=0
boolean is_it_pcsdt=FALSE
boolean is_it_contract=FALSE
end variables

on w_voucher_retrieve_update.create
int iCurrent
call super::create
this.st_voucher=create st_voucher
this.cb_exit=create cb_exit
this.cb_update=create cb_update
this.cb_select_contract=create cb_select_contract
this.cb_print_all=create cb_print_all
this.dw_voucher_retrieve=create dw_voucher_retrieve
this.dw_detail_voucher_pcsdt=create dw_detail_voucher_pcsdt
this.dw_detail_voucher=create dw_detail_voucher
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_voucher
this.Control[iCurrent+2]=this.cb_exit
this.Control[iCurrent+3]=this.cb_update
this.Control[iCurrent+4]=this.cb_select_contract
this.Control[iCurrent+5]=this.cb_print_all
this.Control[iCurrent+6]=this.dw_voucher_retrieve
this.Control[iCurrent+7]=this.dw_detail_voucher_pcsdt
this.Control[iCurrent+8]=this.dw_detail_voucher
end on

on w_voucher_retrieve_update.destroy
call super::destroy
destroy(this.st_voucher)
destroy(this.cb_exit)
destroy(this.cb_update)
destroy(this.cb_select_contract)
destroy(this.cb_print_all)
destroy(this.dw_voucher_retrieve)
destroy(this.dw_detail_voucher_pcsdt)
destroy(this.dw_detail_voucher)
end on

event resize;call super::resize;long ll_height

//This script is not commented because when we do minimize this screen then all the DW'S are not resizing 
This.X = w_pics_main.X
This.Y = w_pics_main.Y
ll_height = w_pics_main.mdi_1.Height
This.Resize(w_pics_main.WorkSpaceWidth(), ll_height)

//This.X = -3
//This.Y = 277
//This.Width = 2926
//This.Height = 1465
end event

event pfc_preopen;call super::pfc_preopen;this.of_SetPreference(TRUE)
this.inv_preference.of_SetToolBars(TRUE)
this.inv_preference.of_SetWindow(TRUE)
this.of_SetResize(TRUE)
this.inv_resize.of_SetOrigSize(this.workspacewidth(),this.workspaceheight())
inv_resize.of_Register(dw_voucher_retrieve, "Scale")
inv_resize.of_Register(cb_exit, "Scale")
inv_resize.of_Register(st_voucher, "Scale")
inv_resize.of_Register(cb_update, "Scale")
inv_resize.of_Register(cb_select_contract, "Scale")
inv_resize.of_Register(cb_print_all, "Scale")
inv_resize.of_Register(dw_detail_voucher,"Scale")
inv_resize.of_Register(dw_detail_voucher_pcsdt,"Scale")




end event

event pfc_postopen;call super::pfc_postopen;int rtn
str_voucher_report lstr_voucher_report


lstr_voucher_report = Message.powerobjectparm
IF w_voucher_retrieve.cbx_pcsdate.checked = TRUE THEN
	rtn = dw_voucher_retrieve.Retrieve(lstr_voucher_report.ld_date,lstr_voucher_report.ld_date1 )
	IF rtn > 0 THEN	
		w_voucher_retrieve_update.is_it_pcsdt=TRUE
		w_voucher_retrieve_update.is_it_contract=FALSE
	END IF
	RETURN
END IF
IF w_voucher_retrieve.cbx_contract.checked = TRUE THEN
	dw_voucher_retrieve.dataobject = 'd_voucher_addate_cntrno'
	dw_voucher_retrieve.settransobject(sqlservertrans)
	rtn = dw_voucher_retrieve.Retrieve(lstr_voucher_report.ld_dates,lstr_voucher_report.ls_contract,lstr_voucher_report.ld_pcdt)
	IF rtn > 0 THEN
		w_voucher_retrieve_update.is_it_pcsdt=FALSE
		w_voucher_retrieve_update.is_it_contract=TRUE
	END IF
	Open(w_pics_retrieve_msg_box)
	close(w_pics_retrieve_msg_box)	
	RETURN
END IF


end event

event close;call super::close;m_pics_main.m_file.m_print.Enabled 			=	FALSE
m_pics_main.m_file.m_pagesetup.Enabled		=	FALSE
m_pics_main.m_file.m_printimmediate.Enabled	=	FALSE
m_pics_main.m_edit.m_deleterow.Enabled 	=	FALSE
m_pics_main.m_edit.m_addrow.Enabled 		=	FALSE

end event

type st_voucher from statictext within w_voucher_retrieve_update
integer x = 37
integer y = 1088
integer width = 1655
integer height = 64
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 33554432
long backcolor = 67108864
string text = "Double click on the contract number to see the voucher"
boolean focusrectangle = false
end type

type cb_exit from u_cb within w_voucher_retrieve_update
string tag = "Exits the screen"
integer x = 2377
integer y = 1152
integer width = 329
integer height = 96
integer taborder = 0
integer textsize = -10
string text = "&Exit"
end type

event clicked;call super::clicked;ib_disableclosequery = TRUE
//dw_detail_voucher.AcceptText()
//dw_detail_voucher_pcsdt.AcceptText()
//dw_sum_voucher_contract_update.AcceptText()
//dw_voucher_retrieve.AcceptText()
parent.event pfc_close()
end event

type cb_update from u_cb within w_voucher_retrieve_update
string tag = "Please click this button when all the values are entered"
integer x = 1472
integer y = 1152
integer width = 878
integer height = 96
integer taborder = 0
integer textsize = -10
string text = "Create/&Update Voucher No(s)"
end type

event clicked;call super::clicked;Integer rtn,vno_seq,li_rows,i,lcnt
String ls_curr_year,ls_vno,ls_vno_seq,ls_contract
Datetime ld_pcsdt, ld_admdt
Date tday
BOOLEAN duplicate_found = FALSE

tday = Today()

dw_voucher_retrieve.AcceptText()

// Get the string year in format of YY: 07
ls_curr_year = Right(String(Year(Today())),2)

li_rows = dw_voucher_retrieve.RowCount() 

IF li_rows = 0 THEN
	Messagebox("UPDATE","Nothing to update")
	RETURN
ELSE
	
	FOR i = 1 TO li_rows
		// If voucher number exist, do not assign a new voucher number
		IF IsNull(dw_voucher_retrieve.object.voucher_vno[i]) OR dw_voucher_retrieve.object.voucher_vno[i] = "" THEN
			// Get the next voucher number sequence to work with
			SELECT VOUCHER_NO.NEXTVAL 
			INTO :vno_seq
			FROM dual 
			USING SqlServerTrans;
		
			 // Get the sequence number in format of 0001
			ls_vno_seq = String(vno_seq,"0000;(000);****;empty")
			ls_vno = 'PCS'+ls_curr_year+ls_vno_seq

             		 // Check to see if the duplicate voucher number found	
			SELECT COUNT(*)
				INTO :lcnt
				FROM VOUCHER
				WHERE VNO = :ls_vno
				USING SQLServerTrans;
	
			  // If duplicate voucher number is found then set the boolean flag on
			IF lcnt > 0 THEN
				duplicate_found = TRUE
			ELSE
				duplicate_found = FALSE
			END IF		
		
		       // While duplicate_found flag is true find the next voucher number
			DO WHILE duplicate_found
				// Get the next voucher number sequence to work with
				SELECT VOUCHER_NO.NEXTVAL 
				INTO :vno_seq
				FROM dual 
				USING SqlServerTrans;
			
				 // Get the sequence number in format of 0001
				ls_vno_seq = String(vno_seq,"0000;(000);****;empty")
				ls_vno = 'PCS'+ls_curr_year+ls_vno_seq				
				
				 // Check to see if the duplicate voucher number found	
				SELECT COUNT(*)
					INTO :lcnt
					FROM VOUCHER
					WHERE VNO = :ls_vno
					USING SQLServerTrans;
		
				  // If duplicate voucher number is found then set the boolean flag on
				IF lcnt > 0 THEN
					duplicate_found = TRUE
				ELSE
					duplicate_found = FALSE
				END IF		
			LOOP

			 // Assign the voucher number and voucher date, created_date and created_by
			dw_voucher_retrieve.object.voucher_vno[i] = ls_vno
			dw_voucher_retrieve.object.voucher_vdt[i] = tday		
			dw_voucher_retrieve.object.created_date[i] = tday		
			dw_voucher_retrieve.object.created_by[i] = gnv_app.of_GetUserId()
		END IF
	
	NEXT
	// If there was any modification to this datawindow, updated it.
	IF dw_voucher_retrieve.ModifiedCount() > 0 THEN
		dw_voucher_retrieve.SetTransObject(SQLServerTrans)
		rtn = dw_voucher_retrieve.event pfc_update(TRUE,TRUE)
		IF rtn=1 THEN
			IF f_check_dberror(SQLServerTrans,"Voucher") THEN	
				COMMIT USING SQLServerTrans;
				Messagebox("UPDATE","Update successfull.")
				RETURN 1	
			ELSE
				Messagebox("UPDATE","Update failed contact system administrator.")
				ROLLBACK USING SQLServerTrans;
				RETURN -1
			END IF
		ELSE
			Messagebox("UPDATE","Update failed contact system administrator.")
			ROLLBACK USING SQLServerTrans;
			RETURN -1
		END IF
	END IF
END IF


end event

type cb_select_contract from u_cb within w_voucher_retrieve_update
string tag = "Please click this button to go back and fourth of the window"
integer x = 37
integer y = 1152
integer width = 517
integer height = 96
integer taborder = 0
integer textsize = -10
string text = "&Select Contract"
end type

event clicked;call super::clicked;dw_detail_voucher.Reset()
dw_detail_voucher_pcsdt.Reset()
dw_detail_voucher_pcsdt.visible= FALSE
dw_detail_voucher.visible= FALSE
dw_voucher_retrieve.Show()
dw_detail_voucher.Hide()
cb_update.Enabled = TRUE


	



end event

type cb_print_all from u_cb within w_voucher_retrieve_update
string tag = "Prints all contractor details"
integer x = 571
integer y = 1152
integer width = 887
integer height = 96
integer taborder = 0
integer textsize = -10
string text = "Create &HTML(s) and Print  All"
end type

event clicked;call super::clicked;n_ds lds_pdf,lds_voucher_detail
int rtn,li_rows,i,j,li_pdf_row_count,ll_newrow
string ls_contract,ls_vno,ls_pdf_filename,ls_userid
datetime ld_admin_date, ld_pcsdt
date tday
boolean pdf_created=FALSE

tday = Today()
ls_userid = gnv_app.of_GetUserId()

// Create and load the datastore
lds_pdf = CREATE n_ds
lds_pdf.dataobject = "d_voucher_pcs_dates"
IF ll_pdf_row<>0 THEN
	rtn = dw_voucher_retrieve.RowsCopy(ll_pdf_row, ll_pdf_row, Primary!, lds_pdf, 1, Primary!)
	IF rtn = -1 THEN
		Messagebox("ERROR"," Error in rowscopy")
	END IF 
END IF

// Create a datastore for voucher detail records
lds_voucher_detail = CREATE n_ds
lds_voucher_detail.dataobject = "d_voucher_detail"


li_pdf_row_count = lds_pdf.RowCount()

//// 05/14/09 validate vouchers directory
string ls_dir
ls_dir = 'p:\'
If DirectoryExists (ls_dir ) Then
Else
	Messagebox('Error', ls_dir +   '  drive does not exist, please contact system administrator')
	RETURN -1
End If
ls_dir = 'p:\vouchers'
If DirectoryExists (ls_dir ) Then
Else
	Messagebox('Error', ls_dir +   '  directory does not exist, please contact system administrator')
	RETURN -1
End If


// Get the PDF files
IF dw_detail_voucher_pcsdt.visible= TRUE THEN
	
	ls_vno = dw_detail_voucher_pcsdt.object.vno[1]
	
	IF IsNull(ls_vno) or ls_vno = "" THEN
		Messagebox("ERROR"," You are not allowed to print vouchers until voucher numbers are assigned. ",StopSign!)
		RETURN -1	
	ELSE

		ls_pdf_filename = "P:\vouchers\"+ls_vno+".html"
		
		IF IsNull(lds_pdf.object.pdf_created_by[1]) OR  lds_pdf.object.pdf_created_by[1] = "" THEN
			 pdf_created = FALSE
		ELSE
			 pdf_created = TRUE
		END IF
		
		// Print the PDF
		dw_detail_voucher_pcsdt.TriggerEvent("pfc_Print")
		
		// If PDF is not created yet
		IF pdf_created = FALSE THEN  
			
			 // Create and place the PDFs
			rtn = dw_detail_voucher_pcsdt.SaveAs(ls_pdf_filename,HTMLTable!, TRUE)
			IF rtn <> 1 THEN
				Messagebox("ERROR"," Error in creating the HTML file for voucher number "+ls_pdf_filename)
			ELSE
				// Set the value for date PDF created and by whom
				// Assign the modified date and by whom
				UPDATE VOUCHER 
				SET PDF_CREATED_DATE = :tday, PDF_CREATED_BY = :ls_userid, MODIFIED_DATE = :tday, MODIFIED_BY = :ls_userid
				WHERE vno = :ls_vno
				USING SQLServerTrans;
				IF f_check_dberror(SQLServerTrans,"Voucher") THEN	
					
					// Create a loop for data insertion into voucher detail data store
					FOR j = 1 TO dw_detail_voucher_pcsdt.RowCount()						
						ll_newrow = lds_voucher_detail.InsertRow(0)
						lds_voucher_detail.Object.vno[ll_newrow] = ls_vno
						lds_voucher_detail.Object.vdt[ll_newrow] = tday
						lds_voucher_detail.object.CREATED_DATE[ll_newrow] = tday
						lds_voucher_detail.object.CREATED_BY[ll_newrow] = gnv_app.of_GetUserId()
						lds_voucher_detail.object.MODIFIED_DATE[ll_newrow] = tday
						lds_voucher_detail.object.MODIFIED_BY[ll_newrow] = gnv_app.of_GetUserId()
						lds_voucher_detail.object.invno[ll_newrow] = dw_detail_voucher_pcsdt.object.invno[j]
						lds_voucher_detail.object.invamt[ll_newrow] = dw_detail_voucher_pcsdt.object.invamt[j]						
					NEXT
					
					// Update the voucher detail information
					lds_voucher_detail.SetTransObject(SQLServerTrans)
					lds_voucher_detail.Event pfc_update(TRUE,TRUE)
					
					COMMIT USING SQLServerTrans;
					Messagebox("UPDATE","Update successfull. The HTML(s) are stored in P:\vouchers directory")
					RETURN 1	
				ELSE
					Messagebox("UPDATE","Update failed contact system administrator.")
					ROLLBACK USING SQLServerTrans;
					RETURN -1
				END IF
			END IF
		ELSE // PDF was created before
			
			rtn = Messagebox("Creating HTML","HTML was already created. Do you want to recreate it? ",Question!,YesNo!,1)
			IF rtn = 1 THEN			
				 // Create and place the PDFs
				rtn = dw_detail_voucher_pcsdt.SaveAs(ls_pdf_filename,HTMLTable!, TRUE)
				IF rtn <> 1 THEN
					Messagebox("ERROR"," Error in creating the HTML file for voucher number "+ls_pdf_filename)				
				ELSE
					// Assign the modified date and by whom
					UPDATE VOUCHER 
					SET MODIFIED_DATE = :tday, MODIFIED_BY = :ls_userid
					WHERE vno = :ls_vno
					USING SQLServerTrans;
					IF f_check_dberror(SQLServerTrans,"Voucher") THEN	
						
						// Create a loop for data insertion into voucher detail data store
						FOR j = 1 TO dw_detail_voucher_pcsdt.RowCount()						
							ll_newrow = lds_voucher_detail.InsertRow(0)
							lds_voucher_detail.Object.vno[ll_newrow] = ls_vno
							lds_voucher_detail.Object.vdt[ll_newrow] = tday
							lds_voucher_detail.object.CREATED_DATE[ll_newrow] = tday
							lds_voucher_detail.object.CREATED_BY[ll_newrow] = gnv_app.of_GetUserId()
							lds_voucher_detail.object.MODIFIED_DATE[ll_newrow] = tday
							lds_voucher_detail.object.MODIFIED_BY[ll_newrow] = gnv_app.of_GetUserId()
							lds_voucher_detail.object.invno[ll_newrow] = dw_detail_voucher_pcsdt.object.invno[j]
							lds_voucher_detail.object.invamt[ll_newrow] = dw_detail_voucher_pcsdt.object.invamt[j]						
						NEXT
						
						// Update the voucher detail information
						lds_voucher_detail.SetTransObject(SQLServerTrans)
						lds_voucher_detail.Event pfc_update(TRUE,TRUE)
	
						COMMIT USING SQLServerTrans;
						Messagebox("UPDATE","Update successfull. The HTML(s) are stored in P:\vouchers directory")
						RETURN 1	
					ELSE
						Messagebox("UPDATE","Update failed contact system administrator.")
						ROLLBACK USING SQLServerTrans;
						RETURN -1
					END IF
				END IF
			END IF
		END IF
	END IF
ELSEIF dw_detail_voucher.visible= TRUE THEN

	ls_vno = dw_detail_voucher.object.vno[1]

	IF IsNull(ls_vno) or ls_vno = "" THEN
		Messagebox("ERROR"," You are not allowed to print vouchers until voucher numbers are assigned. ",StopSign!)
		RETURN
	ELSE

		ls_pdf_filename = "P:\vouchers\"+ls_vno+".html"
	
		IF IsNull(lds_pdf.object.pdf_created_by[1]) OR  lds_pdf.object.pdf_created_by[1] = "" THEN
			 pdf_created = FALSE
		ELSE
			 pdf_created = TRUE
		END IF
	
	       // Print the PDF
		dw_detail_voucher.TriggerEvent("pfc_Print")
		
		 // If PDF is not created yet
		IF pdf_created = FALSE THEN 
			
			 // Create and place the PDFs
			rtn = dw_detail_voucher.SaveAs(ls_pdf_filename,HTMLTable!, TRUE)
			IF rtn <> 1 THEN
				Messagebox("ERROR"," Error in creating the HTML file for voucher number "+ls_pdf_filename)
			ELSE
				// Set the value for date PDF created and by whom
				// Assign the modified date and by whom
				UPDATE VOUCHER 
				SET PDF_CREATED_DATE = :tday, PDF_CREATED_BY = :ls_userid, MODIFIED_DATE = :tday, MODIFIED_BY = :ls_userid
				WHERE vno = :ls_vno
				USING SQLServerTrans;
				IF f_check_dberror(SQLServerTrans,"Voucher") THEN	
					
					// Create a loop for data insertion into voucher detail data store
					FOR j = 1 TO dw_detail_voucher.RowCount()						
						ll_newrow = lds_voucher_detail.InsertRow(0)
						lds_voucher_detail.Object.vno[ll_newrow] = ls_vno
						lds_voucher_detail.Object.vdt[ll_newrow] = tday
						lds_voucher_detail.object.CREATED_DATE[ll_newrow] = tday
						lds_voucher_detail.object.CREATED_BY[ll_newrow] = gnv_app.of_GetUserId()
						lds_voucher_detail.object.MODIFIED_DATE[ll_newrow] = tday
						lds_voucher_detail.object.MODIFIED_BY[ll_newrow] = gnv_app.of_GetUserId()
						lds_voucher_detail.object.invno[ll_newrow] = dw_detail_voucher.object.invno[j]
						lds_voucher_detail.object.invamt[ll_newrow] = dw_detail_voucher.object.invamt[j]						
					NEXT
					
					// Update the voucher detail information
					lds_voucher_detail.SetTransObject(SQLServerTrans)
					lds_voucher_detail.Event pfc_update(TRUE,TRUE)
					
					COMMIT USING SQLServerTrans;
					Messagebox("UPDATE","Update successfull. The HTML(s) are stored in P:\vouchers directory")
					RETURN 1	
				ELSE
					Messagebox("UPDATE","Update failed contact system administrator.")
					ROLLBACK USING SQLServerTrans;
					RETURN -1
				END IF
			END IF
		ELSE
			rtn = Messagebox("Creating HTML","HTML was already created. Do you want to recreate it? ",Question!,YesNo!,1)
			IF rtn = 1 THEN			
				 // Create and place the PDFs
				rtn = dw_detail_voucher.SaveAs(ls_pdf_filename,HTMLTable!, TRUE)
				IF rtn <> 1 THEN
					Messagebox("ERROR"," Error in creating the HTML file for voucher number "+ls_pdf_filename)
				ELSE
					// Assign the modified date and by whom
					UPDATE VOUCHER 
					SET MODIFIED_DATE = :tday, MODIFIED_BY = :ls_userid
					WHERE vno = :ls_vno
					USING SQLServerTrans;
					IF f_check_dberror(SQLServerTrans,"Voucher") THEN	
						
						// Create a loop for data insertion into voucher detail data store
						FOR j = 1 TO dw_detail_voucher.RowCount()						
							ll_newrow = lds_voucher_detail.InsertRow(0)
							lds_voucher_detail.Object.vno[ll_newrow] = ls_vno
							lds_voucher_detail.Object.vdt[ll_newrow] = tday
							lds_voucher_detail.object.CREATED_DATE[ll_newrow] = tday
							lds_voucher_detail.object.CREATED_BY[ll_newrow] = gnv_app.of_GetUserId()
							lds_voucher_detail.object.MODIFIED_DATE[ll_newrow] = tday
							lds_voucher_detail.object.MODIFIED_BY[ll_newrow] = gnv_app.of_GetUserId()
							lds_voucher_detail.object.invno[ll_newrow] = dw_detail_voucher.object.invno[j]
							lds_voucher_detail.object.invamt[ll_newrow] = dw_detail_voucher.object.invamt[j]						
						NEXT
						
						// Update the voucher detail information
						lds_voucher_detail.SetTransObject(SQLServerTrans)
						lds_voucher_detail.Event pfc_update(TRUE,TRUE)
						
						COMMIT USING SQLServerTrans;
						Messagebox("UPDATE","Update successfull. The HTML(s) are stored in P:\vouchers directory")
						RETURN 1	
					ELSE
						Messagebox("UPDATE","Update failed contact system administrator.")
						ROLLBACK USING SQLServerTrans;
						RETURN -1
					END IF
				END IF //Save as 
			END IF //PDF created before
		END IF
	END IF
ELSEIF dw_voucher_retrieve.visible = TRUE THEN

	// Go through the loop and create PDF and update database
	FOR i = 1 TO dw_voucher_retrieve.RowCount()
		
		// Contract number, Admin date and PCS date
		ls_contract = dw_voucher_retrieve.object.magcntr_cntr[i]
		ld_admin_date = dw_voucher_retrieve.object.maginv_admdt[i]
		ld_pcsdt = dw_voucher_retrieve.object.maginv_pcsdt[i]
		
		// Voucher number
		ls_vno = dw_voucher_retrieve.object.voucher_vno[i]
		
		IF IsNull(ls_vno) or ls_vno = "" THEN
			Messagebox("ERROR"," You are not allowed to create HTMLs until voucher numbers are assigned. Contract number "+ls_contract,StopSign!)
			CONTINUE
		ELSE
			// PDF filename
			ls_pdf_filename = "P:\vouchers\"+ls_vno+".html"
			
			// Retrieve the voucher informations
			rtn = dw_detail_voucher.retrieve(ls_contract,ld_admin_date,ld_pcsdt)
			
			// If records exist then
			IF rtn > 0 THEN
				// Check to see if the PDF is created and the flag is not NULL.
				// If the fag is NULL then created the PDF and update the record.
				IF IsNull(dw_voucher_retrieve.object.pdf_created_by[i]) OR  dw_voucher_retrieve.object.pdf_created_by[i] = "" THEN
					 pdf_created = FALSE
				ELSE
					 pdf_created = TRUE
				END IF
				
				// If PDF is not created yet
				IF pdf_created = FALSE THEN  
					 // Create and place the PDFs
					rtn = dw_detail_voucher.SaveAs(ls_pdf_filename,HTMLTable!, TRUE)
					IF rtn <> 1 THEN
						Messagebox("ERROR"," Error in creating the HTML file for voucher number "+ls_pdf_filename)
						CONTINUE
					ELSE
						// Set the value for date PDF created and by whom
						dw_voucher_retrieve.object.PDF_CREATED_DATE[i] = tday
						dw_voucher_retrieve.object.PDF_CREATED_BY[i] = gnv_app.of_GetUserId()
						// Assign the modified date and by whom
						dw_voucher_retrieve.object.MODIFIED_DATE[i] = tday
						dw_voucher_retrieve.object.MODIFIED_BY[i] = gnv_app.of_GetUserId()

						// Create a loop for data insertion into voucher detail data store
						FOR j = 1 TO dw_detail_voucher.RowCount()						
							ll_newrow = lds_voucher_detail.InsertRow(0)
							lds_voucher_detail.Object.vno[ll_newrow] = ls_vno
							lds_voucher_detail.Object.vdt[ll_newrow] = tday
							lds_voucher_detail.object.CREATED_DATE[ll_newrow] = tday
							lds_voucher_detail.object.CREATED_BY[ll_newrow] = gnv_app.of_GetUserId()
							lds_voucher_detail.object.invno[ll_newrow] = dw_detail_voucher.object.invno[j]
							lds_voucher_detail.object.invamt[ll_newrow] = dw_detail_voucher.object.invamt[j]						
						NEXT
						
					END IF
				END IF
			ELSE
				Messagebox("ERROR"," no data found")
			END IF
		END IF
//		next_voucher:
	NEXT
	
	rtn = Messagebox("Printing Vouchers","Do you want to print vouchers now? ",Question!,YesNo!,1)
	IF rtn = 1 THEN
		// Go through the loop and print  PDF
		FOR i = 1 TO dw_voucher_retrieve.RowCount()
			// Contract number, Admin date and PCS date
			ls_contract = dw_voucher_retrieve.object.magcntr_cntr[i]
			ld_admin_date = dw_voucher_retrieve.object.maginv_admdt[i]
			ld_pcsdt = dw_voucher_retrieve.object.maginv_pcsdt[i]
			// Voucher number
			ls_vno = dw_voucher_retrieve.object.voucher_vno[i]
			// Retrieve the voucher informations
			
			IF IsNull(ls_vno) or ls_vno = "" THEN
				Messagebox("ERROR"," You are not allowed to print vouchers until voucher numbers are assigned. Contract number "+ls_contract,StopSign!)
				CONTINUE
			ELSE
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
			END IF
//			next_voucher_print:
		NEXT
	END IF
		
END IF	
	
// If there are any changes to the voucher datawindow, update it.
IF dw_voucher_retrieve.ModifiedCount() > 0 THEN
	rtn = dw_voucher_retrieve.Event pfc_update(TRUE,TRUE)
	IF rtn=1 THEN
		// Update the voucher detail information
		lds_voucher_detail.SetTransObject(SQLServerTrans)
		lds_voucher_detail.Event pfc_update(TRUE,TRUE)
		IF f_check_dberror(SQLServerTrans,"Voucher") THEN	
			COMMIT USING SQLServerTrans;
			Messagebox("UPDATE","Update successfull. The HTML(s) are stored in P:\vouchers directory")
			RETURN 1	
		ELSE
			Messagebox("UPDATE","Update failed contact system administrator.")
			ROLLBACK USING SQLServerTrans;
			RETURN -1
		END IF
	ELSE
		Messagebox("UPDATE","Update failed contact system administrator.")
		ROLLBACK USING SQLServerTrans;
		RETURN -1
	END IF
END IF




end event

type dw_voucher_retrieve from u_dw within w_voucher_retrieve_update
integer width = 2743
integer height = 1088
integer taborder = 30
string dataobject = "d_voucher_pcs_dates"
boolean hscrollbar = true
end type

event constructor;of_settransobject(sqlservertrans)
of_setrowmanager(TRUE)
of_SetFilter(TRUE)
of_setFind(TRUE)
inv_filter.of_setstyle(1)
of_SetSort(TRUE)
inv_sort.of_SetStyle(1)
this.SetRowFocusIndicator(Hand!)





end event

event retrievestart;call super::retrievestart;Open(w_pics_retrieve_msg_box)
end event

event retrieveend;call super::retrieveend;Integer i,rtn
String ls_cntr,ls_contracts
datetime ld_admindate,ld_pcsdates

IF rowcount = 0 THEN
	Close(w_pics_retrieve_msg_box)
	Messagebox("No Rows","Please enter another date.")
	Close(w_voucher_retrieve_update)
	IF w_voucher_retrieve.cbx_pcsdate.checked =TRUE THEN
		w_voucher_retrieve.wf_disable_contract()
	ELSEIF	w_voucher_retrieve.cbx_contract.checked =TRUE THEN
		w_voucher_retrieve.wf_disable_start_end_date()
	END IF
	RETURN
END IF

FOR i = rowcount TO 1 STEP -1
	ls_contracts = dw_voucher_retrieve.object.voucher_cntr[i]		
	IF IsNull(ls_contracts) OR ls_contracts = "" THEN			
		ls_cntr = dw_voucher_retrieve.object.magcntr_cntr[i]
		ld_pcsdates = dw_voucher_retrieve.object.maginv_pcsdt[i]
		ld_admindate = dw_voucher_retrieve.object.maginv_admdt[i]
		dw_voucher_retrieve.object.voucher_cntr[i]=ls_cntr
		dw_voucher_retrieve.object.voucher_pcsdt[i]=ld_pcsdates
		dw_voucher_retrieve.object.voucher_admdt[i]=ld_admindate
		dw_voucher_retrieve.SetItemStatus(i,0,Primary!, NewModified!)
      END IF
NEXT

w_voucher_retrieve_update.windowState = maximized!
Close(w_voucher_retrieve)
Close(w_pics_retrieve_msg_box)
dw_voucher_retrieve.visible = TRUE
dw_voucher_retrieve.SetFocus()

end event

event doubleclicked;call super::doubleclicked;string ls_contract
datetime ld_admin_date,ld_pcsdt
integer i,rtn,rownum

IF dwo.name = "voucher_cntr" OR dwo.name = "voucher_vno" THEN
	rownum = this.GetRow()
	ll_pdf_row = rownum
       dw_voucher_retrieve.hide()
	IF is_it_pcsdt THEN
		dw_detail_voucher_pcsdt.visible = TRUE
		dw_detail_voucher_pcsdt.Vscrollbar = TRUE
		dw_detail_voucher_pcsdt.livescroll = TRUE	
		ld_pcsdt = this.object.maginv_pcsdt[rownum]
		ls_contract = this.object.magcntr_cntr[rownum]
		ld_admin_date = this.object.maginv_admdt[rownum]
		dw_detail_voucher_pcsdt.Reset()
		dw_detail_voucher_pcsdt.retrieve(ls_contract,ld_admin_date,ld_pcsdt)
	END IF
	IF is_it_contract THEN
		dw_detail_voucher.visible = TRUE
		dw_detail_voucher.Vscrollbar = TRUE
		dw_detail_voucher.livescroll = TRUE	
		ls_contract = this.object.magcntr_cntr[rownum]
		ld_admin_date = this.object.maginv_admdt[rownum]
		ld_pcsdt = this.object.maginv_pcsdt[rownum]
		dw_detail_voucher.Reset()
		dw_detail_voucher.retrieve(ls_contract,ld_admin_date,ld_pcsdt)
	END IF	
END IF
end event

event sqlpreview;call super::sqlpreview;//MessageBox("sql",sqlsyntax)
end event

event itemchanged;call super::itemchanged;Datetime ld_pdf_created_date
Date NullDate
String NullStr,ls_del_pdf,ls_vno_old,ls_created_by,lmsg
INTEGER rtn,lcnt=0
Date tday

tday = Today()

SetNull(NullDate)
SetNull(NullStr)

IF dwo.name = "voucher_vno" THEN
	ld_pdf_created_date = this.object.pdf_created_date[row]
	ls_created_by = this.object.pdf_created_by[row]
	
	SELECT COUNT(*)
	INTO :lcnt
	FROM VOUCHER
	WHERE VNO = :data
	USING SQLServerTrans;
	
	IF lcnt > 0 THEN
		lmsg = "This voucher number "+data+" already exist in the database. Please select another one."
		Messagebox("Voucher Number",lmsg, StopSign!)
		RETURN 2
	END IF
		
	IF NOT(IsNull(ld_pdf_created_date)) THEN
		ls_vno_old =  UPPER(this.object.vno_old[row])
		rtn = MessageBox("Recreating PDF","PDF was created for the voucher number "+ls_vno_old+" on "+string(ld_pdf_created_date,'MM/DD/YY')+" by "+ls_created_by+".~r~nYou must recreate the PDF and remove the old PDF for this new voucher number "+data+".",Question!,YesNo!,1)
		IF rtn = 1 THEN
			ls_del_pdf = "delete P:\vouchers\"+ls_vno_old+".pdf"
	
			Run(ls_del_pdf)

			this.object.voucher_vdt[row] = tday		
			this.object.pdf_created_date[row] = NullDate
			this.object.pdf_created_by[row] = NullStr
		ELSE
			RETURN 2
		END IF
	END IF
	
	this.object.voucher_vdt[row] = Today()
	
END IF
end event

type dw_detail_voucher_pcsdt from u_dw within w_voucher_retrieve_update
boolean visible = false
integer width = 2743
integer height = 1088
integer taborder = 10
string dataobject = "d_voucher_contract_update_pcsdt"
boolean hscrollbar = true
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

event retrieveend;call super::retrieveend;cb_update.enabled = FALSE

end event

event scrollvertical;call super::scrollvertical;long ll_numrows 

//string ls_firstrow, ls_lastrow

ll_numrows = dw_detail_voucher_pcsdt.RowCount( )

end event

type dw_detail_voucher from u_dw within w_voucher_retrieve_update
boolean visible = false
integer width = 2743
integer height = 1088
integer taborder = 20
string dataobject = "d_voucher_contract_update"
boolean hscrollbar = true
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

event scrollvertical;call super::scrollvertical;long ll_numrows 

//string ls_firstrow, ls_lastrow

ll_numrows = dw_detail_voucher.RowCount( )

end event

event retrieveend;call super::retrieveend;cb_update.enabled = FALSE

end event


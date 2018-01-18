$PBExportHeader$w_pa_assigning_books.srw
forward
global type w_pa_assigning_books from w_sheet
end type
type sle_no_flash from u_sle within w_pa_assigning_books
end type
type st_no_flash from statictext within w_pa_assigning_books
end type
type ole_ezftp from u_ezftp within w_pa_assigning_books
end type
type cb_booknav from commandbutton within w_pa_assigning_books
end type
type dw_dtbbkseq from u_pics_dw within w_pa_assigning_books
end type
type cb_opf from commandbutton within w_pa_assigning_books
end type
type cb_labels from commandbutton within w_pa_assigning_books
end type
type cb_html from commandbutton within w_pa_assigning_books
end type
type dw_par_on_web from u_pics_dw within w_pa_assigning_books
end type
type cb_exit from u_cb within w_pa_assigning_books
end type
type dw_pa_ancntr_data from u_pics_dw within w_pa_assigning_books
end type
type st_no_books from statictext within w_pa_assigning_books
end type
type sle_no_books from u_sle within w_pa_assigning_books
end type
type cb_find from u_cb within w_pa_assigning_books
end type
type cb_clear from u_cb within w_pa_assigning_books
end type
type cb_update from u_cb within w_pa_assigning_books
end type
type dw_pa_par_reprot from u_pics_dw within w_pa_assigning_books
end type
type dw_pa_du_par_report from u_pics_dw within w_pa_assigning_books
end type
type cb_listing from commandbutton within w_pa_assigning_books
end type
type dw_pa_listing_for_prod from u_pics_dw within w_pa_assigning_books
end type
type dw_pa_listing_for_controler from u_pics_dw within w_pa_assigning_books
end type
type dw_pa_dupl_listing_for_controler from u_pics_dw within w_pa_assigning_books
end type
type dw_pa_dupl_book_assignment from u_pics_dw within w_pa_assigning_books
end type
type dw_pa_book_assignment_ad from u_pics_dw within w_pa_assigning_books
end type
type dw_pa_book_assignment from u_pics_dw within w_pa_assigning_books
end type
end forward

global type w_pa_assigning_books from w_sheet
integer x = 9
integer y = 8
integer width = 2981
integer height = 1904
string title = "Assigning Books"
windowstate windowstate = maximized!
sle_no_flash sle_no_flash
st_no_flash st_no_flash
ole_ezftp ole_ezftp
cb_booknav cb_booknav
dw_dtbbkseq dw_dtbbkseq
cb_opf cb_opf
cb_labels cb_labels
cb_html cb_html
dw_par_on_web dw_par_on_web
cb_exit cb_exit
dw_pa_ancntr_data dw_pa_ancntr_data
st_no_books st_no_books
sle_no_books sle_no_books
cb_find cb_find
cb_clear cb_clear
cb_update cb_update
dw_pa_par_reprot dw_pa_par_reprot
dw_pa_du_par_report dw_pa_du_par_report
cb_listing cb_listing
dw_pa_listing_for_prod dw_pa_listing_for_prod
dw_pa_listing_for_controler dw_pa_listing_for_controler
dw_pa_dupl_listing_for_controler dw_pa_dupl_listing_for_controler
dw_pa_dupl_book_assignment dw_pa_dupl_book_assignment
dw_pa_book_assignment_ad dw_pa_book_assignment_ad
dw_pa_book_assignment dw_pa_book_assignment
end type
global w_pa_assigning_books w_pa_assigning_books

type variables
string mod_string,original_select,original_tbl_select, is_cntr, original_ad_select,original_ad_tbl_select
string mod_dupl_string,original_dupl_select,original_dupl_tbl_select
boolean query_mode_on=FALSE,new_batch=FALSE,book_reassigned=FALSE
date local_assinged_date, id_assigndt

end variables

forward prototypes
public function boolean wf_check_bkseq_assigned_to_t (long lbkseq)
public subroutine wf_disable_buttons ()
public subroutine wf_enable_buttons ()
public function boolean wf_get_book_dupl_info (integer row, long lbkseq)
public function boolean wf_get_book_info (integer row, string lbkno)
public function boolean wf_get_cntr_bkseq (long lbkseq, string lcntr)
public subroutine wf_set_required_fields ()
public subroutine wf_set_required_fields_no ()
public subroutine wf_set_taborder_dupl ()
public subroutine wf_set_taborder ()
public subroutine wf_set_taborder_get_conno ()
public function boolean wf_check_bkseq_assigned_to_d (long lbkseq)
public function boolean wf_is_book_selected (string lconno)
public function boolean wf_check_bkseq_assigned_to_m (long lbkseq)
public function boolean wf_check_invoice_exist (string lcntr, long lbkseq, string lbkmed)
public function boolean wf_validate_bk_dupl (long lbkseq)
public function boolean wf_validate_bkseq (long lbkseq, string lstage)
public function boolean wf_get_conno_info (integer row, string lconno_no)
public function boolean wf_validate_conno (string lconno)
public function string wf_get_sub_contractor (string lcntr, string lcntrmed, string lprodstage)
public function boolean wf_check_bkseq ()
public subroutine wf_create_par_on_web (string lconnos[], integer ll_no_connos, datawindow dw_ref)
end prototypes

public function boolean wf_check_bkseq_assigned_to_t (long lbkseq);// This function will check to see if the book number is assigned to
// a record and duplication contracter.
long Lbkseqno
string Lcntr,Lcntrtype,ls_message,ls_msgparm[1]

	SELECT prod.bkseq,prod.cntr,ancntr.cntrtype
	INTO     :Lbkseqno,:Lcntr,:Lcntrtype
    FROM prod,ancntr
   WHERE ( prod.cntr 	= ancntr.cntr 	) and
			( prod.bkmed 	= ancntr.cntrmed ) and
			( ancntr.cntrtype = 'T' ) and
			( prod.prodstage in ('MA','PU','AB') ) and
			( prod.bkseq 	= :Lbkseq)
	USING SQLServerTrans;
	IF sqlservertrans.SQLCode < 0 THEN
		// If more than one row was found.
		IF (sqlservertrans.SQLDBCODE = 0 AND sqlservertrans.sqlcode = -1) THEN
			MessageBox("Warnning","Book number "+string(Lbkseq)+", was assigned to contractors for mastering and duplication.", Information!)
			RETURN TRUE
		ELSE
			ls_message = "A database error has occurred in selecting duplicate book number.~n" + &
							 "Database error code:  " + String (sqlservertrans.sqldbcode) + "~r~n" + &
							 "Database error message:~r~n" + sqlservertrans.sqlerrtext
			IF IsValid(gnv_app.inv_error) THEN
				ls_msgparm[1] = ls_message
				gnv_app.inv_error.of_Message ('pfc_dwdberror', ls_msgparm, &
				gnv_app.iapp_object.DisplayName)
			ELSE
				Messagebox (gnv_app.iapp_object.DisplayName, ls_message, StopSign!, Ok!)
				ROLLBACK USING sqlservertrans;
			End If
			RETURN FALSE
		END IF
	ELSEIF sqlservertrans.SQLCode = 100 THEN
		RETURN TRUE
	ELSEIF sqlservertrans.SQLCode = 0 THEN
		MessageBox("Warnning","Book number "+string(Lbkseq)+", was assigned to contractor "+Lcntr+" for Mastering and Duplication.", Information!)
		RETURN TRUE
	END IF
end function

public subroutine wf_disable_buttons ();cb_clear.Enabled = FALSE
cb_update.Enabled = FALSE
cb_listing.Enabled = FALSE
cb_html.Enabled = FALSE
cb_opf.Enabled = FALSE
cb_labels.Enabled = FALSE
cb_booknav.Enabled = FALSE

end subroutine

public subroutine wf_enable_buttons ();cb_clear.Enabled = TRUE
cb_update.Enabled = TRUE
cb_listing.Enabled = TRUE
cb_html.Enabled = TRUE
cb_opf.Enabled = TRUE
cb_labels.Enabled = TRUE
cb_booknav.Enabled = TRUE

end subroutine

public function boolean wf_get_book_dupl_info (integer row, long lbkseq);String Lconno,lbkmed,lpriority,Lttl,ls_message,ls_msgparm[1]
Integer Lapplen,Lvols,Lqnty

IF query_mode_on=FALSE THEN
	// Check if the book assigned to a duplicator
	IF wf_check_bkseq_assigned_to_d(Lbkseq)=TRUE THEN
		// Check if the book assigned to a Mastering and duplicator
		IF wf_check_bkseq_assigned_to_t(Lbkseq)=TRUE THEN
			// Check if the book assigned to a Mastering
			IF wf_check_bkseq_assigned_to_m(Lbkseq)=TRUE THEN
				// Select the media information of the book for duplication.		
				SELECT mchar.conno,mchar.vols,mchar.applen,
						mchar.bkmed,mchar.qnty,mchar.priority,
						ttlinit.ttl  
				INTO 	:Lconno,:Lvols,:Lapplen,:lbkmed,
						:Lqnty,:lpriority,:Lttl
				FROM mchar,ttlinit  
				where (mchar.chno = ttlinit.chno) AND
						(mchar.bkseq = :Lbkseq) AND
						(mchar.bkmed in ('RC','BR'))
				USING SqlServerTrans;
				IF (SqlServerTrans.sqlCode < 0) THEN
					ls_message = "A database error has occurred in selecting duplicate book number.~n" + &
						 		"Database error code:  " + String (SqlServerTrans.sqlDbCode) + "~r~n" + &
						 		"Database error message:~r~n" + SqlServerTrans.sqlErrText
					IF IsValid(gnv_app.inv_error) THEN
						ls_msgparm[1] = ls_message
						gnv_app.inv_error.of_Message ('pfc_dwdberror', ls_msgparm, &
						gnv_app.iapp_object.displayName)
					ELSE
						Messagebox (gnv_app.iapp_object.displayName, ls_message, stopSign!, ok!)
						ROLLBACK USING SqlServerTrans;
					END IF
					RETURN FALSE
				ELSE
					dw_pa_dupl_book_assignment.object.mchar_conno[row]=Lconno
					dw_pa_dupl_book_assignment.object.mchar_bkseq[row]=Lbkseq
					dw_pa_dupl_book_assignment.object.mchar_bkmed[row]=lbkmed
					dw_pa_dupl_book_assignment.object.mchar_applen[row]=Lapplen
					dw_pa_dupl_book_assignment.object.mchar_priority[row]=lpriority
					dw_pa_dupl_book_assignment.object.mchar_vols[row]=Lvols
					dw_pa_dupl_book_assignment.object.mchar_qnty[row]=Lqnty
					dw_pa_dupl_book_assignment.object.ttlinit_ttl[row]=Lttl
					// If this is not a new batch and the we are not working on an existing assignment date.
					IF (IsNull(local_assinged_date)=FALSE AND (String(local_assinged_date)<>'1/1/1900') AND (String(local_assinged_date)<>'1/1/00') AND new_batch=FALSE) THEN
						// MessageBox("DATES","you are assigning existing assignment date of: "+string(local_assinged_date))
						dw_pa_dupl_book_assignment.object.prod_assigndt[row] = local_assinged_date
					// Else this is a new batch and assignment date is today's date.
					ELSE
						// MessageBox("DATES","you assigning today date.")
						dw_pa_dupl_book_assignment.object.prod_assigndt[row] = Today()
					END IF					
					dw_pa_dupl_book_assignment.EVENT pfc_addrow()
					RETURN TRUE
				END IF
			ELSE
				// checking for existing "MAstering" contractor function return FALSE
				RETURN FALSE
			END IF
		ELSE
			// checking for existing  "Mastering & Duplicator" contractor function return FALSE
			RETURN FALSE
		END IF
	ELSE
		// checking for existing "Duplicator" contractor function return FALSE
		RETURN FALSE
	END IF
ELSE
	RETURN TRUE
END IF
end function

public function boolean wf_get_book_info (integer row, string lbkno);string Lconno,Lbkmed,Lpriority,Lttl,Ls_message,ls_msgparm[1]
string Lchno,Lcntr
long Lbkseq
integer Lapplen,Lestpt,Lvols,Lqnty,rtn
date Lschstdt,Lschenddt,Lassignment_date

Lbkseq = long(Lbkno)
SELECT 	mchar.conno,
        	prod.bkseq,   
     		prod.bkmed,
			mchar.chno,
         mchar.applen,   
         mchar.priority,   
         mchar.vols,
			mchar.qnty,
			prod.cntr,
         prod.schstdt,   
         prod.schenddt,
			prod.assigndt,
         ttlinit.ttl  
 	INTO 	:Lconno,
	 		:Lbkseq,
			:Lbkmed,
			:Lchno,
			:Lapplen,
			:Lpriority,
			:Lvols,
			:Lqnty,
			:Lcntr,
			:Lschstdt,
			:Lschenddt,
			:Lassignment_date,
			:Lttl
	FROM  mchar, prod, ttlinit  
   WHERE ( mchar.chno = ttlinit.chno ) AND
			( mchar.bkseq = prod.bkseq ) AND 
         ( prod.bkseq = :Lbkseq) AND
			( prod.prodstage = 'DU')
	USING SQLServerTrans;
	IF sqlservertrans.SQLCode < 0 THEN
		ls_message = "A database error has occurred in selecting book number.~n" + &
						 "Database error code:  " + String (sqlservertrans.sqldbcode) + "~r~n" + &
						 "Database error message:~r~n" + sqlservertrans.sqlerrtext
		IF IsValid(gnv_app.inv_error) THEN
			ls_msgparm[1] = ls_message
			gnv_app.inv_error.of_Message ('pfc_dwdberror', ls_msgparm, &
			gnv_app.iapp_object.DisplayName)
		ELSE
			Messagebox (gnv_app.iapp_object.DisplayName, ls_message, StopSign!, Ok!)
			ROLLBACK USING sqlservertrans;
		End If
		RETURN FALSE
	ELSEIF sqlservertrans.SQLCode = 100 THEN
		rtn = MessageBox("Duplication","Is this a new book number for duplication?",Question!,YesNo!,1)
		IF rtn=1 THEN
			dw_pa_dupl_book_assignment.object.prod_bkseq[row]=Lbkseq
			dw_pa_dupl_book_assignment.object.mchar_conno[row]=Lconno
			dw_pa_dupl_book_assignment.object.ttlinit_ttl[row]=Lttl
			dw_pa_dupl_book_assignment.object.prod_bkmed[row]=Lbkmed
			dw_pa_dupl_book_assignment.object.mchar_chno[row]=Lchno
			IF IsNull(Lassignment_date)=FALSE THEN
				dw_pa_dupl_book_assignment.object.prod_assigndt[row] = string(Lassignment_date,'MM/DD/YY')
			ELSE
				dw_pa_dupl_book_assignment.object.prod_assigndt[row] = string(Today(),'MM/DD/YY')
			END IF			
			dw_pa_dupl_book_assignment.object.prod_schenddt[row] = string(Lschenddt,'MM/DD/YY')
			dw_pa_dupl_book_assignment.object.mchar_applen[row]=Lapplen
			dw_pa_dupl_book_assignment.object.mchar_priority[row]=Lpriority
			dw_pa_dupl_book_assignment.object.mchar_vols[row]=Lvols
			dw_pa_dupl_book_assignment.object.mchar_qnty[row]=Lqnty
			dw_pa_dupl_book_assignment.object.prod_prodstage[row]="DU"
			dw_pa_dupl_book_assignment.Event pfc_addrow()
			RETURN TRUE
		ELSE					
			dw_pa_dupl_book_assignment.Object.prod_bkseq.ValidationMsg='Book number does not have all the requirments for duplication.'
			RETURN FALSE
		END IF
	ELSE
		dw_pa_dupl_book_assignment.object.prod_bkseq[row]=Lbkseq
		dw_pa_dupl_book_assignment.object.mchar_conno[row]=Lconno
		dw_pa_dupl_book_assignment.object.ttlinit_ttl[row]=Lttl
		dw_pa_dupl_book_assignment.object.prod_bkmed[row]=Lbkmed
		dw_pa_dupl_book_assignment.object.mchar_chno[row]=Lchno
		IF (IsNull(Lassignment_date)=FALSE  AND (string(Lassignment_date)<>'1/1/1900') ) THEN
			dw_pa_dupl_book_assignment.object.prod_assigndt[row] = string(Lassignment_date,'MM/DD/YY')
		ELSE
			dw_pa_dupl_book_assignment.object.prod_assigndt[row] = string(Today(),'MM/DD/YY')
		END IF			
		dw_pa_dupl_book_assignment.object.prod_schenddt[row] = string(Lschenddt,'MM/DD/YY')
		dw_pa_dupl_book_assignment.object.mchar_applen[row]=Lapplen
		dw_pa_dupl_book_assignment.object.mchar_priority[row]=Lpriority
		dw_pa_dupl_book_assignment.object.mchar_vols[row]=Lvols
		dw_pa_dupl_book_assignment.object.mchar_qnty[row]=Lqnty
		dw_pa_dupl_book_assignment.object.prod_prodstage[row]="DU"
		dw_pa_dupl_book_assignment.Event pfc_addrow()
		RETURN TRUE
	END IF
end function

public function boolean wf_get_cntr_bkseq (long lbkseq, string lcntr);int Lcount=0

Select count(*) into :Lcount from prod
where bkseq = :Lbkseq and cntr = :Lcntr
using SQlservertrans;
IF Lcount = 0 THEN
	RETURN FALSE
ELSE
	RETURN TRUE
END IF
end function

public subroutine wf_set_required_fields ();dw_pa_book_assignment.Object.mchar_bkseq.Editmask.Required='Yes'
dw_pa_book_assignment.Object.mchar_applen.Editmask.Required='Yes'
dw_pa_book_assignment.Object.mchar_priority.DDLB.Required='Yes'
dw_pa_book_assignment.Object.mchar_df.DDLB.Required='Yes'
dw_pa_book_assignment.Object.prod_schstdt.Editmask.Required='Yes'
dw_pa_book_assignment.Object.prod_schenddt.Editmask.Required='Yes'
// dw_pa_book_assignment.Object.prod_assigndt.Editmask.Required='Yes'

end subroutine

public subroutine wf_set_required_fields_no ();dw_pa_book_assignment.Object.mchar_bkseq.Editmask.Required='No'
dw_pa_book_assignment.Object.mchar_applen.Editmask.Required='No'
dw_pa_book_assignment.Object.mchar_priority.DDLB.Required='No'
dw_pa_book_assignment.Object.mchar_df.DDLB.Required='No'
dw_pa_book_assignment.Object.prod_schstdt.Editmask.Required='No'
dw_pa_book_assignment.Object.prod_schenddt.Editmask.Required='No'
// dw_pa_book_assignment.Object.prod_assigndt.Editmask.Required='No'

end subroutine

public subroutine wf_set_taborder_dupl ();dw_pa_dupl_book_assignment.Object.mchar_bkseq.TabSequence = 10
dw_pa_dupl_book_assignment.Object.prod_schstdt.TabSequence = 20
dw_pa_dupl_book_assignment.Object.mchar_priority.TabSequence = 30
dw_pa_dupl_book_assignment.Object.mchar_vols.TabSequence = 40
dw_pa_dupl_book_assignment.Object.mchar_qnty.TabSequence = 50
dw_pa_dupl_book_assignment.SetFocus()


end subroutine

public subroutine wf_set_taborder ();dw_pa_book_assignment.Object.mchar_conno.TabSequence = 10
dw_pa_book_assignment.Object.prod_prodstage.TabSequence = 20
dw_pa_book_assignment.Object.mchar_bkseq.TabSequence = 30
dw_pa_book_assignment.Object.prod_schstdt.TabSequence = 40
dw_pa_book_assignment.Object.mchar_applen.TabSequence = 50
dw_pa_book_assignment.Object.mchar_priority.TabSequence = 60
dw_pa_book_assignment.Object.mchar_df.TabSequence = 70
dw_pa_book_assignment.SetFocus()


end subroutine

public subroutine wf_set_taborder_get_conno ();dw_pa_book_assignment.Object.mchar_conno.TabSequence = 10
dw_pa_book_assignment.Object.mchar_bkseq.TabSequence = 0
dw_pa_book_assignment.Object.prod_prodstage.TabSequence = 0
dw_pa_book_assignment.Object.prod_assigndt.TabSequence = 0
dw_pa_book_assignment.Object.prod_schstdt.TabSequence = 0
dw_pa_book_assignment.Object.mchar_applen.TabSequence = 0
dw_pa_book_assignment.Object.mchar_priority.TabSequence = 0
dw_pa_book_assignment.Object.mchar_df.TabSequence = 0
dw_pa_book_assignment.Object.prod_schenddt.TabSequence = 0

end subroutine

public function boolean wf_check_bkseq_assigned_to_d (long lbkseq);// This function will check to see if the book number is assigned to
// a duplicator contracter.
long Lbkseqno
string Lcntr,Lcntrtype,ls_message,ls_msgparm[1]

	SELECT prod.bkseq,prod.cntr,ancntr.cntrtype
	INTO     :Lbkseqno,:Lcntr,:Lcntrtype
    FROM prod,ancntr
   WHERE ( prod.cntr 	= ancntr.cntr 	) and
			( prod.bkmed 	= ancntr.cntrmed ) and
			( ancntr.cntrtype = 'D' ) and
			( prod.prodstage in ('DU','PR','FC') ) and
			( prod.bkseq 	= :Lbkseq)
	USING SQLServerTrans;
	IF sqlservertrans.SQLCode < 0 THEN
		// If more than one row was found.
		IF (sqlservertrans.SQLDBCODE = 0 AND sqlservertrans.sqlcode = -1) THEN
			MessageBox("Warnning","Book number "+string(Lbkseq)+", was assigned to more than one contractor for duplication.", Information!)
			RETURN TRUE
		ELSE
			ls_message = "A database error has occurred in selecting duplicate book number.~n" + &
							 "Database error code:  " + String (sqlservertrans.sqldbcode) + "~r~n" + &
							 "Database error message:~r~n" + sqlservertrans.sqlerrtext
			IF IsValid(gnv_app.inv_error) THEN
				ls_msgparm[1] = ls_message
				gnv_app.inv_error.of_Message ('pfc_dwdberror', ls_msgparm, &
				gnv_app.iapp_object.DisplayName)
			ELSE
				Messagebox (gnv_app.iapp_object.DisplayName, ls_message, StopSign!, Ok!)
				ROLLBACK USING sqlservertrans;
			End If
			RETURN FALSE
		END IF
	ELSEIF sqlservertrans.SQLCode = 100 THEN
		// No contracter was assigned to do duplication this book.
		RETURN TRUE
	ELSEIF sqlservertrans.SQLCode = 0 THEN
		MessageBox("Warnning","Book number "+string(Lbkseq)+", was assigned to contractor "+Lcntr+" for duplication.", Information!)
		RETURN TRUE
	END IF
end function

public function boolean wf_is_book_selected (string lconno);date Lcabdt

SetNull(Lcabdt)

Select cabdt into :Lcabdt
from mchar
where conno = :Lconno
using sqlservertrans;

IF IsNull(Lcabdt) THEN
	RETURN FALSE
ELSE
	RETURN TRUE
END IF
end function

public function boolean wf_check_bkseq_assigned_to_m (long lbkseq);// This function will check to see if the book number is assigned to
// a record only contracter.
long Lbkseqno
string Lcntr,Lcntrtype,ls_message,ls_msgparm[1]

SELECT prod.bkseq,prod.cntr,ancntr.cntrtype
INTO     :Lbkseqno,:Lcntr,:Lcntrtype
    FROM prod,ancntr
   WHERE ( prod.cntr 	= ancntr.cntr 	) and
			( prod.bkmed 	= ancntr.cntrmed ) and
			( ancntr.cntrtype = 'M' ) and
			( prod.prodstage in ('MA','PU','AB') ) and
			( prod.bkseq 	= :Lbkseq)
	USING SQLServerTrans;
	IF sqlservertrans.SQLCode < 0 THEN
		// If more than one row was found.
		IF (sqlservertrans.SQLDBCODE = 0 AND sqlservertrans.sqlcode = -1) THEN
			MessageBox("Warnning","Book number "+string(Lbkseq)+", was assigned to contractors for mastering.", Information!)
			RETURN TRUE
		ELSE
			ls_message = "A database error has occurred in selecting duplicate book number.~n" + &
							 "Database error code:  " + String (sqlservertrans.sqldbcode) + "~r~n" + &
							 "Database error message:~r~n" + sqlservertrans.sqlerrtext
			IF IsValid(gnv_app.inv_error) THEN
				ls_msgparm[1] = ls_message
				gnv_app.inv_error.of_Message ('pfc_dwdberror', ls_msgparm, &
				gnv_app.iapp_object.DisplayName)
			ELSE
				Messagebox (gnv_app.iapp_object.DisplayName, ls_message, StopSign!, Ok!)
				ROLLBACK USING sqlservertrans;
			End If
			RETURN FALSE
		END IF
	ELSEIF sqlservertrans.SQLCode = 100 THEN
		RETURN TRUE
	ELSEIF sqlservertrans.SQLCode = 0 THEN
		MessageBox("Warnning","Book number "+string(Lbkseq)+", was assigned to contractor "+Lcntr+" for Mastering.", Information!)
		RETURN TRUE
	END IF
end function

public function boolean wf_check_invoice_exist (string lcntr, long lbkseq, string lbkmed);// This function will check to see if the book number exist in invoice table.
long Lbkseqno

SELECT distinct bkseq
INTO   :Lbkseqno
FROM 	inv
WHERE ( cntr 	= :lcntr ) and
		( bkseq 	= :lbkseq) and
		( bkmed  = :lbkmed)
USING SQLServerTrans;
IF f_check_dberror(SqlServerTrans,"Invoice")=FALSE THEN
	RETURN FALSE
ELSEIF sqlservertrans.SQLCode = 100 THEN
	RETURN TRUE
ELSEIF sqlservertrans.SQLCode = 0 THEN
	RETURN FALSE
END IF
end function

public function boolean wf_validate_bk_dupl (long lbkseq);//////////////////////////////////////////////////////////////////////////////////////////////////////////
//
//	Function :  of_enabledisable
// Args		:  boolean ab_enable ( TRUE or FALSE)
// RETURNS	: 1 for success, -1 for failure
//	Description:
//  Enables disables row columns based on history
//  
//
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//	
//	Revision History
//
//	Developed by 	Date 				Version						Tracking#
//									
// Murali K.			03/28/2008      PICS Modifications	PICS 2.0 Reissue logic revisited
//															if the book# and book media already exist
//															then prompt for reissue otherwise regular 
//															process
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//

String lmed,ls_message,ls_msgparm[1],Lcntr,Lcntr_data,lcntrtype,lbkmed,lflash_indicator, lsprodstage
Long Lbkno, ll_count
Int rtn

lmed = dw_pa_ancntr_data.object.cntrmed[1]
Lcntr = dw_pa_ancntr_data.object.cntr[1]

// 03/28/2008
lcntrtype = dw_pa_ancntr_data.object.cntrtype[1]

// 01/15/2008 There is no FC only DB
IF lmed = 'FC' THEN
	lmed = 'DB'
END IF

IF query_mode_on=FALSE THEN
	// First, check to see if book number exist in prod table.
//	SELECT Distinct bkseq INTO :Lbkno FROM prod
//	where bkseq=:Lbkseq AND bkmed=:lmed
//	USING SqlServerTrans;

	// 03/28/2008 reissue check
	IF lmed = 'DB' and lcntrtype = 'D' THEN
		lsprodstage = 'FC'
	END IF
	IF lmed = 'RC' and lcntrtype = 'D' THEN
		lsprodstage = 'DU'
	END IF
	IF lmed = 'RTB' and lcntrtype = 'M' THEN
		lsprodstage = 'MA'
	END IF
	IF lmed = 'RTB' and lcntrtype = 'T' THEN
		lsprodstage = 'MA'
	END IF

	
	SELECT count(*) INTO :ll_count  FROM prod
	where bkseq=:Lbkseq AND bkmed=:lmed  and prodstage = :lsprodstage
	USING SqlServerTrans;


	IF SqlServerTrans.sqlCode < 0 THEN
		ls_message = "A database error has occurred in select.~n" + &
						 "Database error code:  " + String (SqlServerTrans.sqlDbCode) + "~r~n" + &
						 "Database error message:~r~n" + SqlServerTrans.sqlErrText
		IF IsValid(gnv_app.inv_error) THEN
			ls_msgparm[1] = ls_message
			gnv_app.inv_error.of_Message ('pfc_dwdberror', ls_msgparm, &
			gnv_app.iapp_object.displayName)
		ELSE
			Messagebox (gnv_app.iapp_object.displayName, ls_message, stopSign!, ok!)
			ROLLBACK USING SqlServerTrans;
			RETURN FALSE
			
		END IF
		RETURN FALSE
		
	ELSEIF ll_count > 0 THEN // SqlServerTrans.sqlCode = 100 THEN // 03/28/2008
		// If it does not exist in prod table find out if this is a reissue book.
		String Lconno,Lricd
		rtn = Messagebox("Reissue Book","Is this a reissue book?",question!,yesNo!,1)
		IF rtn = 1 THEN
			Open(w_pa_get_conno_ri)
			IF (IsNull(message.StringParm)=FALSE AND message.StringParm<>"") THEN
				Lconno = message.StringParm
				IF lmed='P/B' OR lmed='BR' THEN
					lbkmed = 'BR'
				ELSEIF lmed = 'RC' THEN
					lbkmed = 'RC'
				END IF
				rtn = Messagebox("Reissue Book","Book number ~'"+String(Lbkseq)+"~' will be assigned to control number ~'"+Lconno+"~', Continue?",question!,yesNo!,1)
				IF rtn=1 THEN				
					UPDATE mchar set bkseq=:Lbkseq,bkmed=:lbkmed,cascd='N' where conno=:Lconno USING SqlServerTrans;
					IF f_check_dberror(SqlServerTrans,"MCHAR") THEN
						COMMIT USING SqlServerTrans;
						RETURN TRUE
					END IF
				ELSE
					RETURN FALSE
					
				END IF
			ELSE
				RETURN FALSE
				
			END IF			
		ELSE
			dw_pa_dupl_book_assignment.object.mchar_bkseq.ValidationMsg='This book number does not exist.'
		END IF			
		RETURN FALSE
		
	END IF
	// Second, check to see if that book number has already been assigned to this contract number
	SELECT bkseq INTO :Lbkno 
	FROM prod
	where bkseq=:Lbkseq 
	AND bkmed=:lmed 
	AND cntr=:Lcntr 
	AND prodstage in ('DU','FC')
	USING SqlServerTrans;
	IF SqlServerTrans.sqlCode < 0 THEN
		ls_message = "A database error has occurred in select.~n" + &
						 "Database error code:  " + String (SqlServerTrans.sqlDbCode) + "~r~n" + &
						 "Database error message:~r~n" + SqlServerTrans.sqlErrText
		IF IsValid(gnv_app.inv_error) THEN
			ls_msgparm[1] = ls_message
			gnv_app.inv_error.of_Message ('pfc_dwdberror', ls_msgparm, &
			gnv_app.iapp_object.displayName)
		ELSE
			Messagebox (gnv_app.iapp_object.displayName, ls_message, stopSign!, ok!)
			ROLLBACK USING SqlServerTrans;
			RETURN FALSE
			
		END IF
		RETURN FALSE
		
	ELSEIF SqlServerTrans.sqlCode = 0 THEN
		// If exist, return with the error message stating "duplicate book number".
		dw_pa_dupl_book_assignment.object.mchar_bkseq.ValidationMsg='This book number is already assinged to contract number: '+Lcntr
		RETURN FALSE
		
	ELSE
		
		// 01/15/2008 CHECK TO SEE IF RC EXIST IN MCHAR
		IF dw_pa_ancntr_data.object.cntrmed[1] = 'RC' THEN
			SELECT count(*)
			INTO :lL_COUNT
			FROM mchar
			where bkseq=:Lbkseq 
			AND bkmed=:lmed
			USING SqlServerTrans;
			
			IF sqlservertrans.sqlcode < 0 THEN
				// DB ERROR
				Ls_message = "A database error has occurred in select.~n" + &
								 "Database error code:  " + String (SqlServerTrans.sqlDbCode) + "~r~n" + &
								 "Database error message:~r~n" + SqlServerTrans.sqlErrText
				IF IsValid(gnv_app.inv_error) THEN
					ls_msgparm[1] = ls_message
					gnv_app.inv_error.of_Message ('pfc_dwdberror', ls_msgparm, &
					gnv_app.iapp_object.displayName)
				ELSE
					Messagebox (gnv_app.iapp_object.displayName, ls_message, stopSign!, ok!)
					ROLLBACK USING SqlServerTrans;
					RETURN FALSE
					
				END IF
				RETURN FALSE
				
			END IF
			// IF NOT FOUND PROMPT A ERROR MESSAGE DO NOT PROCEED
			IF ll_count = 0 THEN
				dw_pa_dupl_book_assignment.object.mchar_bkseq.ValidationMsg='This book number does not exist for this media: '+lmed
				RETURN FALSE
			ELSE
				RETURN TRUE
			END IF
		END IF
		//////// 01/15/2008

		// 01/15/2008 CHECK TO SEE IF BR EXIST IN MCHAR
		IF dw_pa_ancntr_data.object.cntrmed[1] = 'BR' THEN
			SELECT count(*)
			INTO :lL_COUNT
			FROM mchar
			where bkseq=:Lbkseq 
			AND bkmed=:lmed
			USING SqlServerTrans;
			
			IF sqlservertrans.sqlcode < 0 THEN
				// DB ERROR
				Ls_message = "A database error has occurred in select.~n" + &
								 "Database error code:  " + String (SqlServerTrans.sqlDbCode) + "~r~n" + &
								 "Database error message:~r~n" + SqlServerTrans.sqlErrText
				IF IsValid(gnv_app.inv_error) THEN
					ls_msgparm[1] = ls_message
					gnv_app.inv_error.of_Message ('pfc_dwdberror', ls_msgparm, &
					gnv_app.iapp_object.displayName)
				ELSE
					Messagebox (gnv_app.iapp_object.displayName, ls_message, stopSign!, ok!)
					ROLLBACK USING SqlServerTrans;
					RETURN FALSE
					
				END IF
				RETURN FALSE
				
			END IF
			// IF NOT FOUND PROMPT A ERROR MESSAGE DO NOT PROCEED
			IF ll_count = 0 THEN
				dw_pa_dupl_book_assignment.object.mchar_bkseq.ValidationMsg='This book number does not exist for this media: '+lmed
				RETURN FALSE
			ELSE
				RETURN TRUE
			END IF
		END IF
		//////// 01/15/2008


		//Finally check to see if this is a flash duplication the flash indicator for this book in set in the MCHAR table
		IF dw_pa_ancntr_data.object.cntrmed[1] = 'FC' THEN
			SELECT flash_indicator
			INTO :lflash_indicator
			FROM mchar
			where bkseq=:Lbkseq 
			AND bkmed=:lmed
			USING SqlServerTrans;
			IF SqlServerTrans.sqlCode < 0 THEN
				ls_message = "A database error has occurred in select.~n" + &
								 "Database error code:  " + String (SqlServerTrans.sqlDbCode) + "~r~n" + &
								 "Database error message:~r~n" + SqlServerTrans.sqlErrText
				IF IsValid(gnv_app.inv_error) THEN
					ls_msgparm[1] = ls_message
					gnv_app.inv_error.of_Message ('pfc_dwdberror', ls_msgparm, &
					gnv_app.iapp_object.displayName)
				ELSE
					Messagebox (gnv_app.iapp_object.displayName, ls_message, stopSign!, ok!)
					ROLLBACK USING SqlServerTrans;
					RETURN FALSE
					
				END IF
				RETURN FALSE
				
			ELSE
				IF NOT(IsNull(lflash_indicator)) AND lflash_indicator = 'Y' THEN
					RETURN TRUE
				ELSE
					dw_pa_dupl_book_assignment.object.mchar_bkseq.ValidationMsg='Flash indicator is not set for this book number. Please use final review screen to set this indicator.'
					RETURN FALSE
					
				END IF
			END IF
		END IF
	END IF
ELSE
	RETURN TRUE
END IF
RETURN TRUE
end function

public function boolean wf_validate_bkseq (long lbkseq, string lstage);string Lmed,ls_message,ls_msgparm[1],Lcntr,Lcntr_data,Lcntrtype
long Lbkno
int rtn,Lcnt=0

Lmed = dw_pa_ancntr_data.object.cntrmed[1]
Lcntr = dw_pa_ancntr_data.object.cntr[1]
Lcntrtype = dw_pa_ancntr_data.object.cntrtype[1]

IF query_mode_on=FALSE THEN
	select count(*) into :Lcnt from prod 
	where bkseq=:Lbkseq and bkmed=:Lmed and cntr=:Lcntr
	using sqlservertrans;
	IF lcnt<>0 THEN
		dw_pa_book_assignment.Object.mchar_bkseq.ValidationMsg='Control number has been assigned to this book number.'
		RETURN FALSE
	END IF
	IF (Lstage = "MASTERING") THEN
		// If this book is already assigned to this contracter.
		select bkseq into :Lbkno from prod
			where bkseq=:Lbkseq 
			AND bkmed=:Lmed 
			AND cntr=:Lcntr 
			AND prodstage in (select prodstage from prodstage 
										where cntrtype = :Lcntrtype
										and	cntrmed = :Lmed
										and 	(alternate <> 'Y' OR alternate is NULL)
										and	stageorder = 1)
		using SQLServerTrans;
		IF sqlservertrans.sqlcode = 100 THEN
			// If not found, if this book is assigned to another contractor
			select bkseq,cntr into :Lbkno,:Lcntr_data from prod
				where bkseq=:Lbkseq 
				AND prodstage in (select prodstage from prodstage 
											where cntrtype = :Lcntrtype
											and	cntrmed = :Lmed
											and 	(alternate <> 'Y' OR alternate is NULL)
											and	stageorder = 1)
			using SQLServerTrans;
			IF sqlservertrans.sqlcode = 0 THEN
				// If contractor was found display it to user for confirmation.
				rtn = MessageBox("Book assignment","This book number was assinged to another contractor: "+Lcntr_data+"~r~nDo you want to continue?",Question!,YesNo!,1)
				IF rtn = 1 THEN
					RETURN TRUE
				ELSE
					RETURN FALSE
				END IF
			END IF
		END IF
	ELSEIF Lstage = "DUPLICATION" THEN
		select bkseq,cntr into :Lbkno,:Lcntr_data from prod
			where bkseq=:Lbkseq 
			AND bkmed=:Lmed 
			AND cntr=:Lcntr 
			AND prodstage in (select prodstage from prodstage 
										where cntrtype = :Lcntrtype
										and	cntrmed = :Lmed
										and 	(alternate <> 'Y' OR alternate is NULL)
										and	stageorder = 2)
			using SQLServerTrans;
	END IF
	IF sqlservertrans.SQLCode < 0 THEN
		ls_message = "A database error has occurred in select.~n" + &
						 "Database error code:  " + String (sqlservertrans.sqldbcode) + "~r~n" + &
						 "Database error message:~r~n" + sqlservertrans.sqlerrtext
		IF IsValid(gnv_app.inv_error) THEN
			ls_msgparm[1] = ls_message
			gnv_app.inv_error.of_Message ('pfc_dwdberror', ls_msgparm, &
			gnv_app.iapp_object.DisplayName)
		ELSE
			Messagebox (gnv_app.iapp_object.DisplayName, ls_message, StopSign!, Ok!)
			ROLLBACK USING sqlservertrans;
			RETURN FALSE
		End If
		RETURN FALSE
	ELSEIF SQLserverTrans.SQLCode = 100 THEN
		RETURN TRUE
	ELSEIF SQLServerTrans.SQLCode = 0 THEN
		IF Lstage="DU" THEN
			rtn = MessageBox("Book Assignment","Book number is already assinged to contract number("+Lcntr_data+").~r~n Do you want to continue?",Question!,YesNo!,1)
			IF rtn = 1 THEN
				RETURN TRUE
			ELSE
				RETURN FALSE
			END IF
		ELSE		
			dw_pa_book_assignment.Object.mchar_bkseq.ValidationMsg='Book number exist, Please choose another book number.'
			RETURN FALSE
		END IF		
	ELSE
		RETURN FALSE
	END IF
END IF
end function

public function boolean wf_get_conno_info (integer row, string lconno_no);string Lcontract_med,Lcontract_type,Lcntrcvcd,Lmed
string Lconno,Lbkmed,Lpriority,Lttl,Ls_message,ls_msgparm[1]
string Lchno,Lcntr,Lprod_stage[2],Lstage,Lcntrno,Lflash_ind,Lotherconno
long Lbkseq
integer Lapplen,Lestpt,li_flashind
decimal Ldf
date Lschstdt,Lschenddt,Lassignment_date

Lcontract_type = dw_pa_ancntr_data.object.cntrtype[1]
Lcontract_med = dw_pa_ancntr_data.object.cntrmed[1]
Lcntrno=dw_pa_ancntr_data.object.cntr[1]

// Use Lmaxstageorder to find out how many stages will it takes
// to complete the production of each of these books.
int Lmaxstageorder
select max(stageorder) into :Lmaxstageorder from prodstage
  	where cntrtype = :Lcontract_type
	Using sqlservertrans;
// If it take more than one stage. get the first and second production stages
IF Lmaxstageorder > 1 THEN
	select prodstage into :Lprod_stage[1] from prodstage
		where cntrtype = :Lcontract_type
		and   cntrmed = :Lcontract_med
		and	( alternate <> 'Y' OR alternate is NULL)
		and 	stageorder = 1
	using sqlservertrans;
	IF f_check_dberror(sqlservertrans,"PRODSTAGE")=FALSE THEN
		RETURN FALSE
	END IF
	select prodstage into :Lprod_stage[2] from prodstage
		where cntrtype = :Lcontract_type
		and   cntrmed = :Lcontract_med
		and	( alternate <> 'Y' OR alternate is NULL)
		and 	stageorder = :Lmaxstageorder
	using sqlservertrans;
	IF f_check_dberror(sqlservertrans,"PRODSTAGE")=FALSE THEN
		RETURN FALSE
	END IF
ELSE
	// It takes only one stage to complete this book. Get that production stage.
	select prodstage into :Lprod_stage[1] from prodstage
		where cntrtype = :Lcontract_type
		and   cntrmed = :Lcontract_med
		and	( alternate <> 'Y' OR alternate is NULL)
		and 	stageorder = 1
	using sqlservertrans;
	IF f_check_dberror(sqlservertrans,"PRODSTAGE")=FALSE THEN
		RETURN FALSE
	END IF
END IF			

IF Lcontract_med = 'P/B' THEN
	Lprod_stage[1] = 'PU'
	Lprod_stage[2] = 'PB'
END IF
	
SELECT mchar.conno,mchar.bkseq,mchar.bkmed,mchar.chno,
        	mchar.applen,mchar.priority,mchar.df,prod.prodstage,
			prod.cntr,prod.schstdt,prod.schenddt,prod.assigndt,
        	ttlinit.ttl,flash_indicator,other_media_conno,mchar.med  
	INTO 	:Lconno,:Lbkseq,:Lbkmed,:Lchno,:Lapplen,:Lpriority,:Ldf,:Lstage,:Lcntr,
			:Lschstdt,:Lschenddt,:Lassignment_date,:Lttl,:Lflash_ind,:Lotherconno,:Lmed
 	FROM mchar, prod, ttlinit 
  	WHERE mchar.chno = ttlinit.chno and
         mchar.conno = :Lconno_no  and
			mchar.bkseq = prod.bkseq (+) AND 
			mchar.bkmed = prod.bkmed (+)  
	USING SQLServerTrans;
IF sqlservertrans.SQLCode < 0 THEN
	IF (sqlservertrans.SQLDBCODE = 0 AND sqlservertrans.sqlcode = -1) THEN
		SELECT mchar.conno,mchar.bkseq,mchar.bkmed,mchar.chno,
					mchar.applen,mchar.priority,mchar.df,prod.prodstage,
					prod.cntr,prod.schstdt,prod.schenddt,prod.assigndt,
					ttlinit.ttl  
			INTO 	:Lconno,:Lbkseq,:Lbkmed,:Lchno,:Lapplen,:Lpriority,:Ldf,:Lstage,:Lcntr,:Lschstdt,
					:Lschenddt,:Lassignment_date,:Lttl
			FROM  mchar,prod,ttlinit  
			WHERE ( mchar.chno = ttlinit.chno ) and
					( mchar.bkseq = prod.bkseq ) and
					( mchar.bkmed = prod.bkmed ) and
					( mchar.conno = :Lconno_no) and
					( prod.prodstage = :Lprod_stage[1]) and
					( prod.cntr = :Lcntrno)
			USING SQLServerTrans;
			IF f_check_dberror(sqlservertrans,"MCHAR")=FALSE THEN
				RETURN FALSE
			END IF
	ELSE
		ls_message = "A database error has occurred in select into from prod.~n" + &
					 "Database error code:  " + String (sqlservertrans.sqldbcode) + "~r~n" + &
						 "Database error message:~r~n" + sqlservertrans.sqlerrtext
		IF IsValid(gnv_app.inv_error) THEN
			ls_msgparm[1] = ls_message
			gnv_app.inv_error.of_Message ('pfc_dwdberror', ls_msgparm, &
			gnv_app.iapp_object.DisplayName)
		ELSE
			Messagebox (gnv_app.iapp_object.DisplayName, ls_message, StopSign!, Ok!)
			ROLLBACK USING sqlservertrans;
		End If
		RETURN FALSE
	END IF
END IF
dw_pa_book_assignment.object.mchar_conno[row]=Lconno
// If the other control number is not null and the book media is RC, try to get the flash indicator for the DB row.
IF (NOT(IsNull(Lotherconno)) OR Lotherconno <> "") AND Lbkmed = 'RC' THEN
	SELECT flash_indicator 
	INTO :Lflash_ind
	FROM MCHAR
	WHERE conno = :Lotherconno
	USING SQLServerTrans;
	IF f_check_dberror(sqlservertrans,"MCHAR-Flash-indicator")=FALSE THEN
		RETURN FALSE
	END IF
	IF Lflash_ind = 'Y' THEN
		Li_flashind = Integer(sle_no_flash.text)
		Li_flashind = Li_flashind + 1
		sle_no_flash.text = String(Li_flashind)
	END IF
END IF
IF Lbkmed = 'RC' THEN
	dw_pa_book_assignment.object.other_bkmed[row]='DB'
ELSEIF Lbkmed = 'DB' THEN
	dw_pa_book_assignment.object.other_bkmed[row]='RC'	
END IF
dw_pa_book_assignment.object.ttlinit_ttl[row]=Lttl
dw_pa_book_assignment.object.mchar_bkseq[row]=Lbkseq
IF Lcontract_med = 'P/B' THEN
	Lcontract_med = 'BR'
END IF
dw_pa_book_assignment.object.mchar_bkmed[row]=Lbkmed
dw_pa_book_assignment.object.mchar_chno[row]=Lchno
// MessageBox("DATES","assignment date = "+string(Lassignment_date)+" local assignment date ="+string(local_assinged_date))
// If this is not a new batch and the we are working on an existing batch of books.
IF (IsNull(local_assinged_date)=FALSE AND (string(local_assinged_date)<>'1/1/1900') AND (string(local_assinged_date)<>'1/1/00') AND new_batch=FALSE) THEN
	// MessageBox("DATES","you are assigning existing assignment date of: "+string(local_assinged_date))
	dw_pa_book_assignment.object.prod_assigndt[row] = local_assinged_date
// Else if this is not a new batch and we are working on an existing assignment date.
ELSEIF (IsNull(Lassignment_date)=FALSE AND (string(Lassignment_date)<>'1/1/1900') AND (string(Lassignment_date)<>'1/1/00') AND new_batch=FALSE) THEN
	// MessageBox("DATES","you are assigning chosen assignment date of: "+string(Lassignment_date))
	dw_pa_book_assignment.object.prod_assigndt[row] = Lassignment_date
// Else this is a new batch and assignment date is today's date.
ELSE
	// MessageBox("DATES","you assigning today date.")
	dw_pa_book_assignment.object.prod_assigndt[row] = Today()
END IF			
dw_pa_book_assignment.object.prod_schenddt[row] = Lschenddt
dw_pa_book_assignment.object.flash_indicator[row]=Lflash_ind
dw_pa_book_assignment.object.mchar_other_media_conno[row]=Lotherconno
dw_pa_book_assignment.object.mchar_applen[row]=Lapplen
dw_pa_book_assignment.object.mchar_med[row]=Lmed
dw_pa_book_assignment.object.mchar_priority[row]=Lpriority
dw_pa_book_assignment.object.mchar_df[row]=Ldf
dw_pa_book_assignment.object.prod_prodstage[row]=Lprod_stage[1]
dw_pa_book_assignment.object.mchar_chno[row]=string(f_calculate_estpt(Lcontract_med,Lpriority,Lapplen,Lcntrcvcd,Ldf))
dw_pa_book_assignment.Event pfc_addrow ()
RETURN TRUE
end function

public function boolean wf_validate_conno (string lconno);long Lbkseq
string Lcon,Lmed,Lcntrmed,Lpriority,ls_message,ls_msgparm[1],Lcntrcvcd,Lcntr,Larflag,Lfr,Lotherconno, lbkmed
integer Lcntr_no_med,Lapplen,i
double Ldf

Lcntrmed = dw_pa_ancntr_data.object.cntrmed[1]
Lcntrcvcd = dw_pa_ancntr_data.object.cntrcvcd[1]
Lcntr_no_med = dw_pa_ancntr_data.object.cntr_no_med[1]
Lcntr = dw_pa_ancntr_data.object.cntr[1]
book_reassigned = FALSE

		
select conno,bkseq,med,applen,df,priority,arflag,fr, bkmed
	into :Lcon,:Lbkseq,:Lmed,:Lapplen,:Ldf,:Lpriority,:Larflag,:Lfr, :lbkmed
	from mchar
	where conno=:Lconno
using SQLServerTrans;
IF sqlservertrans.SQLCode < 0 THEN
	ls_message = "A database error has occurred in Insert.~n" + &
					 "Database error code:  " + String (sqlservertrans.sqldbcode) + "~r~n" + &
					 "Database error message:~r~n" + sqlservertrans.sqlerrtext
	IF IsValid(gnv_app.inv_error) THEN
		ls_msgparm[1] = ls_message
		gnv_app.inv_error.of_Message ('pfc_dwdberror', ls_msgparm, &
		gnv_app.iapp_object.DisplayName)
	ELSE
		Messagebox (gnv_app.iapp_object.DisplayName, ls_message, StopSign!, Ok!)
		ROLLBACK USING sqlservertrans;
		RETURN FALSE
	End If
	RETURN FALSE
// If there are no control number 
ELSEIF SQLserverTrans.SQLCode = 100 THEN
	dw_pa_book_assignment.Object.mchar_conno.ValidationMsg='Control number does not exist.'
	RETURN FALSE
// If control number exist
ELSEIF SQLServerTrans.SQLCode = 0 THEN
	//11/03/2009 DTB LABEL DATA VALIDATION
	IF lbkmed= 'DB' THEN
		string ls_data='N', ls_print='N', ls_braille='N'
		integer li_ret
		li_ret = f_get_label_data(lconno, ls_data, ls_print, ls_braille)
		IF ls_print='Y' AND ls_braille='Y' THEN
			ELSE
			Messagebox('Error',' DTB Label Data - Print Label and Braille Label not approved for this control number, Cannot assign book.')
			RETURN FALSE
		END IF
	END IF
	// 11/03/2009

	// If the control number has been archived
	IF Larflag = 'A' THEN
		dw_pa_book_assignment.Object.mchar_conno.ValidationMsg='This control number has been archived.'
		RETURN FALSE
	// If book number has not been assigned.
	ELSEIF (IsNull(Lbkseq) OR Lbkseq=0) THEN
		// If medium has not been assigned
		IF IsNull(Lmed) OR Lmed="" THEN 
			dw_pa_book_assignment.Object.mchar_conno.ValidationMsg='Title has not yet passed Confirm Consideration, Medium has not yet been assigned for this control number.'
			RETURN FALSE
		ELSEIF IsNull(Lfr) OR Lfr = "" THEN
			dw_pa_book_assignment.Object.mchar_conno.ValidationMsg='Title has not yet passed Final Review. You may not use this title for assignment.'
			RETURN FALSE
		ELSE
			// If medium has been assigned, but the contract medium is not the same as control no medium.
			IF ((Lmed <> Lcntrmed) AND (Lcntr_no_med=1)) THEN
				ls_message = 'Medium assigned to this control number ('+Lmed+'), does not match with the Contract Medium ('+Lcntrmed+').'
				dw_pa_book_assignment.Object.mchar_conno.ValidationMsg=ls_message
				RETURN FALSE
			ELSEIF ((Lcntr_no_med=2) AND (Lmed<>"FD") AND (Lmed<>"RC")) THEN
				ls_message = 'Medium assigned to this control number ('+Lmed+'), does not match with the Contract Medium ('+Lcntrmed+').'
				dw_pa_book_assignment.Object.mchar_conno.ValidationMsg=ls_message
				RETURN FALSE
			ELSEIF ((Lmed="BR") OR (Lmed="FD")) THEN			
				IF (IsNull(Lapplen) OR IsNull(Lpriority) OR IsNull(Ldf)) THEN
					MessageBox("Warning"," Missing Est.Trks/Brpage, Priority, or Difficulty Factor.")
					RETURN TRUE
				END IF
			ELSEIF Lmed="RC" THEN
				IF (Lcntrcvcd<>"V" AND (IsNull(Lapplen) OR IsNull(Lpriority) OR IsNull(Ldf))) THEN
					MessageBox("Warning"," Missing Est.Trks/Brpage, Priority, or Difficulty Factor.")
					RETURN TRUE
				END IF
			ELSE
				FOR i=1 TO dw_pa_book_assignment.RowCount()
					// control numbe typed in
					Lcon=dw_pa_book_assignment.object.mchar_conno[i]
					// Other control number from typed in control number, if exist.
					SELECT other_media_conno
					INTO :Lotherconno
					FROM MCHAR
					WHERE conno = :Lcon
					USING SQLServerTrans;
					IF f_check_dberror(sqlservertrans,"MCHAR")=FALSE THEN
						RETURN FALSE
					END IF
					IF NOT(IsNULL(Lotherconno)) OR Lotherconno <> "" THEN					
						IF (Lconno = Lcon OR Lconno = Lotherconno) THEN
							dw_pa_book_assignment.Object.mchar_conno.ValidationMsg='Control number or the associated control number exist in the batch.'
							RETURN FALSE
						END IF
					ELSE
						IF Lconno = Lcon THEN
							dw_pa_book_assignment.Object.mchar_conno.ValidationMsg='Control number exist in the batch.'
							RETURN FALSE
						END IF						
					END IF
				NEXT
				RETURN TRUE
			END IF
			RETURN TRUE
		END IF
		RETURN TRUE
	ELSE
		// If medium has been assigned, but the contract medium is not the same as control no medium.
		IF ((Lmed <> Lcntrmed) AND (Lcntr_no_med=1)) THEN
			ls_message = 'Medium assigned to this control number('+Lmed+'), does not match with the Contract Medium('+Lcntrmed+').'
			dw_pa_book_assignment.Object.mchar_conno.ValidationMsg=ls_message
			RETURN FALSE
		ELSEIF ((Lcntr_no_med=2) AND (Lmed<>"FD") AND (Lmed<>"RC")) THEN
			ls_message = 'Medium assigned to this control number('+Lmed+'), does not match with the Contract Medium('+Lcntrmed+').'
			dw_pa_book_assignment.Object.mchar_conno.ValidationMsg=ls_message
			RETURN FALSE
		ELSEIF ((Lmed="BR") OR (Lmed="FD")) THEN			
			IF (IsNull(Lapplen) OR IsNull(Lpriority) OR IsNull(Ldf)) THEN
				MessageBox("Warning"," Missing Est.Trks/Brpage, Priority, or Difficulty Factor.")
				RETURN TRUE
			END IF
		ELSEIF Lmed="RC" THEN
			IF (Lcntrcvcd<>"V" AND (IsNull(Lapplen) OR IsNull(Lpriority) OR IsNull(Ldf))) THEN
				MessageBox("Warning"," Missing Est.Trks/Brpage, Priority, or Difficulty Factor.")
				RETURN TRUE
			END IF
		END IF
		// If the contract number and book number exist in prod table
		IF wf_get_cntr_bkseq(Lbkseq,Lcntr) THEN
			ls_message= 'Book number: '+string(Lbkseq)+' has already been assigned to this control number using contract no: '+Lcntr+'.'
			dw_pa_book_assignment.Object.mchar_conno.ValidationMsg= ls_message
			RETURN FALSE
		ELSE
			MessageBox("Warning","Book number ~'"+string(Lbkseq)+"~' has already been assigned to another contract number using this control number.")
			book_reassigned = TRUE
			RETURN TRUE
		END IF
		

		
	END IF		
ELSE
	RETURN FALSE
END IF
end function

public function string wf_get_sub_contractor (string lcntr, string lcntrmed, string lprodstage);int lcnt=0
string lsubprdr,nullstr

SetNull(lsubprdr)
SetNull(nullstr)

// See if the subcontractors do exist.
select count(*) into :lcnt
from sub
where cntr=:lcntr
using sqlservertrans;
IF f_check_dberror(sqlservertrans,"SUB") THEN
	IF lcnt > 0 THEN
		// If subcontractors exist, then select it.
		Select subprdr
		into :lsubprdr
		from sub
		where cntr = :lcntr
		and   cntrmed = :lcntrmed
		and   prodstage = :lprodstage
		and   default_sub = 'Y'
		using sqlservertrans;
		IF f_check_dberror(sqlservertrans,"SUB") THEN
			RETURN(lsubprdr)
		ELSE
			RETURN(Nullstr)
		END IF
	ELSE
		// If subcontractor do not exist, get the prime contactor as the sub.
		Select prdr
		into :lsubprdr
		from ancntr
		where cntr = :lcntr
		and cntrmed = :lcntrmed
		using sqlservertrans;
		IF f_check_dberror(sqlservertrans,"ANCNTR") THEN
			RETURN(lsubprdr)
		ELSE
			RETURN(Nullstr)
		END IF
	END IF
ELSE
	RETURN(Nullstr)
END IF

end function

public function boolean wf_check_bkseq ();int i

FOR i = 1 to dw_pa_book_assignment.RowCount() 
	IF IsNull(dw_pa_book_assignment.object.mchar_bkseq[i])=TRUE THEN
		MessageBox("ERROR", "Book Number must be entered.")
		dw_pa_book_assignment.SetRow(i)
		dw_pa_book_assignment.SetColumn("mchar_bkseq")
		RETURN FALSE
	END IF
NEXT
RETURN TRUE
end function

public subroutine wf_create_par_on_web (string lconnos[], integer ll_no_connos, datawindow dw_ref);long lbkseq,ll_rows=0, i
string lbkno,lttl,lauth,lauthfn,lahonorific,lcoauth,lcoauthfn,lchonorific,lbkmed,lchno,laepcd,lconno,lspecinst,ls_priority,ls_template
int lcnt=0,rtn,llcnts,ll_newrow
datetime ldt_DateTime
dec ld_dec=1


SETNULL(lcoauth)

OpenWithParm(w_pics_retrieve_msg_box,"Building PAR record on the WEB, Please Wait...")

// For number or connos in the PAR report, build the PAR on the web.

// MessageBox("INFO","number of recs = "+string(ll_no_connos))

FOR llcnts=1 TO ll_no_connos 
	ldt_DateTime = DateTime(today(),now()) 

	// Update mchar table set the value of parprt to 'P'.
	f_set_parprt(lconnos[llcnts],"P")


	lchno = dw_ref.object.cchno[llcnts]
	
	select count(*)
	into :lcnt
	from coauth
	where chno = :lchno
	using sqlservertrans;
		
	IF lcnt >= 2 THEN
		lcoauth = 'And Others'
	ELSEIF lcnt = 1 THEN
		Select coauth,coauthfn,chonorific
		into :lcoauth,:lcoauthfn,:lchonorific 
		from coauth
		where chno = :lchno
		using sqlservertrans;
		lcoauth = f_add_coauth_coauthfn(lcoauth,lcoauthfn,lchonorific)
	END IF
	
	select distinct(template)
	into :ls_template
	from booknavigation
	where conno = :lconnos[llcnts]
	using sqlservertrans;
	
	dw_par_on_web.of_SetTransObject(sqlserveroracletrans)

	// Insert PARs on the web

	ll_newrow = dw_par_on_web.InsertRow(0)

	dw_par_on_web.ScrollToRow(ll_newrow)
		
	dw_par_on_web.object.bkseq[ll_newrow] 	= dw_ref.object.mchar_bkseq[llcnts]
	dw_par_on_web.object.bkmed[ll_newrow] 	= TRIM(dw_ref.object.prod_bkmed[llcnts])
	dw_par_on_web.object.dt_tim[ll_newrow] 	= ldt_DateTime
	dw_par_on_web.object.conno[ll_newrow] 	= TRIM(lconnos[llcnts])
	dw_par_on_web.object.bkno[ll_newrow] 		= TRIM(dw_ref.object.prod_bkmed[llcnts]) + TRIM(string(dw_ref.object.mchar_bkseq[llcnts]))
	dw_par_on_web.object.auth[ll_newrow] 		= TRIM(dw_ref.object.cc_auth[llcnts])
	dw_par_on_web.object.coauth[ll_newrow] 	= TRIM(lcoauth)
	dw_par_on_web.object.ajyfn[ll_newrow] 	= TRIM(dw_ref.object.ttlinit_ajyfn[llcnts])
	dw_par_on_web.object.bk_color[ll_newrow] = TRIM(dw_ref.object.cc_bk_color[llcnts])
	dw_par_on_web.object.dewey[ll_newrow] 	= TRIM(dw_ref.object.ttlinit_dewey[llcnts])
	dw_par_on_web.object.copyright_info[ll_newrow] = TRIM(dw_ref.object.cc_copyright_info[llcnts])
	dw_par_on_web.object.med[ll_newrow] 		= TRIM(dw_ref.object.mchar_med[llcnts])
	dw_par_on_web.object.narrator[ll_newrow] = TRIM(dw_ref.object.cc_narr[llcnts])
	dw_par_on_web.object.vols[ll_newrow] 		= dw_ref.object.mchar_vols[llcnts]
	dw_par_on_web.object.container[ll_newrow] = dw_ref.object.cc_cc4[llcnts]
	dw_par_on_web.object.anno_foreign[ll_newrow] = TRIM(dw_ref.object.anno_foreign[llcnts])
	dw_par_on_web.object.prv_narrator[ll_newrow] = TRIM(dw_ref.object.cc_prev_narr[llcnts])
	dw_par_on_web.object.andigcd[ll_newrow] 	= TRIM(dw_ref.object.mchar_andigcd[llcnts])
	dw_par_on_web.object.pmsub[ll_newrow] 	= TRIM(dw_ref.object.cc_pmsub[llcnts])
		
	dw_par_on_web.object.anno[ll_newrow] 		= f_change_pipe_html_tag( TRIM(dw_ref.object.annotation_anno[llcnts] ))
	dw_par_on_web.object.oneliner[ll_newrow] = f_change_pipe_html_tag( TRIM(dw_ref.object.ttlinit_oneliner[llcnts]))
	dw_par_on_web.object.serttl[ll_newrow] 	= f_change_pipe_html_tag( TRIM(dw_ref.object.cserttl[llcnts]))
	dw_par_on_web.object.seqnote[ll_newrow] 	= f_change_pipe_html_tag( TRIM(dw_ref.object.cseqnote[llcnts]))
	dw_par_on_web.object.ttl[ll_newrow] 		= f_change_pipe_html_tag( TRIM(dw_ref.object.cc_ttl_with_ttlart[llcnts]))

	dw_par_on_web.object.note[ll_newrow] = TRIM(dw_ref.object.ttlinit_note[llcnts])
	dw_par_on_web.object.annoinit[ll_newrow] = TRIM(dw_ref.object.ttlinit_annoinit[llcnts])
	
	dw_par_on_web.object.cdinit[ll_newrow] 	= TRIM(dw_ref.object.ttlinit_cdinit[llcnts])
	dw_par_on_web.object.pminit[ll_newrow] 	= TRIM(dw_ref.object.ttlinit_pminit[llcnts])
	dw_par_on_web.object.prdr[ll_newrow] 		= TRIM(dw_ref.object.ancntr_prdr[llcnts])
		
	dw_par_on_web.object.cntr[ll_newrow] 		= TRIM(dw_ref.object.ancntr_cntrlc[llcnts])
	dw_par_on_web.object.len[ll_newrow] 		= dw_ref.object.mchar_len[llcnts]
	dw_par_on_web.object.applen[ll_newrow] 	= dw_ref.object.mchar_applen[llcnts]
	dw_par_on_web.object.pbpage[ll_newrow] 	= dw_ref.object.acquist_pbpage[llcnts]
		
	dw_par_on_web.object.schstdt[ll_newrow] 	= datetime(date(dw_ref.object.prod_schstdt[llcnts]),now())
	dw_par_on_web.object.schenddt[ll_newrow] = datetime(date(dw_ref.object.prod_schenddt[llcnts]),now())
	dw_par_on_web.object.assigndt[ll_newrow] = datetime(date(dw_ref.object.prod_assigndt[llcnts]),now())
		
	dw_par_on_web.object.reissue_code[ll_newrow] = TRIM(dw_ref.object.mchar_ricd[llcnts])
	dw_par_on_web.object.reissue[ll_newrow] 	= TRIM(dw_ref.object.cc_reissue[llcnts])
	
	dw_par_on_web.object.specialinstruction[ll_newrow] 	= TRIM(dw_ref.object.specinst_sitxt[llcnts])
	
	// This part added for book navigation
	// Modified 2/22/06 with adding tempalte to one of the parameters (MC)
	dw_par_on_web.object.nav_instr[ll_newrow] 	= f_build_nav_instruction(lconnos[llcnts],ls_template)

	// Priority was added to PAR 2/15/04
	dw_par_on_web.object.priority[ll_newrow] 	= dw_ref.object.mchar_priority[llcnts]


	dw_par_on_web.Event pfc_update(TRUE,TRUE)
	
	SETNULL(lcoauth)
 
	
NEXT // For (llcnts) number of connos in the PAR report

rtn = dw_par_on_web.Event pfc_update(TRUE,TRUE)
IF rtn = 1 THEN
	Commit Using SqlServeroracleTrans;
	MessageBox("Update","PAR updated on the web.")
ELSE
	Rollback Using Sqlserveroracletrans;
	MessageBox("ERROR","Update failed in the web.")
END IF
close(w_pics_retrieve_msg_box)

end subroutine

on w_pa_assigning_books.create
int iCurrent
call super::create
this.sle_no_flash=create sle_no_flash
this.st_no_flash=create st_no_flash
this.ole_ezftp=create ole_ezftp
this.cb_booknav=create cb_booknav
this.dw_dtbbkseq=create dw_dtbbkseq
this.cb_opf=create cb_opf
this.cb_labels=create cb_labels
this.cb_html=create cb_html
this.dw_par_on_web=create dw_par_on_web
this.cb_exit=create cb_exit
this.dw_pa_ancntr_data=create dw_pa_ancntr_data
this.st_no_books=create st_no_books
this.sle_no_books=create sle_no_books
this.cb_find=create cb_find
this.cb_clear=create cb_clear
this.cb_update=create cb_update
this.dw_pa_par_reprot=create dw_pa_par_reprot
this.dw_pa_du_par_report=create dw_pa_du_par_report
this.cb_listing=create cb_listing
this.dw_pa_listing_for_prod=create dw_pa_listing_for_prod
this.dw_pa_listing_for_controler=create dw_pa_listing_for_controler
this.dw_pa_dupl_listing_for_controler=create dw_pa_dupl_listing_for_controler
this.dw_pa_dupl_book_assignment=create dw_pa_dupl_book_assignment
this.dw_pa_book_assignment_ad=create dw_pa_book_assignment_ad
this.dw_pa_book_assignment=create dw_pa_book_assignment
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.sle_no_flash
this.Control[iCurrent+2]=this.st_no_flash
this.Control[iCurrent+3]=this.ole_ezftp
this.Control[iCurrent+4]=this.cb_booknav
this.Control[iCurrent+5]=this.dw_dtbbkseq
this.Control[iCurrent+6]=this.cb_opf
this.Control[iCurrent+7]=this.cb_labels
this.Control[iCurrent+8]=this.cb_html
this.Control[iCurrent+9]=this.dw_par_on_web
this.Control[iCurrent+10]=this.cb_exit
this.Control[iCurrent+11]=this.dw_pa_ancntr_data
this.Control[iCurrent+12]=this.st_no_books
this.Control[iCurrent+13]=this.sle_no_books
this.Control[iCurrent+14]=this.cb_find
this.Control[iCurrent+15]=this.cb_clear
this.Control[iCurrent+16]=this.cb_update
this.Control[iCurrent+17]=this.dw_pa_par_reprot
this.Control[iCurrent+18]=this.dw_pa_du_par_report
this.Control[iCurrent+19]=this.cb_listing
this.Control[iCurrent+20]=this.dw_pa_listing_for_prod
this.Control[iCurrent+21]=this.dw_pa_listing_for_controler
this.Control[iCurrent+22]=this.dw_pa_dupl_listing_for_controler
this.Control[iCurrent+23]=this.dw_pa_dupl_book_assignment
this.Control[iCurrent+24]=this.dw_pa_book_assignment_ad
this.Control[iCurrent+25]=this.dw_pa_book_assignment
end on

on w_pa_assigning_books.destroy
call super::destroy
destroy(this.sle_no_flash)
destroy(this.st_no_flash)
destroy(this.ole_ezftp)
destroy(this.cb_booknav)
destroy(this.dw_dtbbkseq)
destroy(this.cb_opf)
destroy(this.cb_labels)
destroy(this.cb_html)
destroy(this.dw_par_on_web)
destroy(this.cb_exit)
destroy(this.dw_pa_ancntr_data)
destroy(this.st_no_books)
destroy(this.sle_no_books)
destroy(this.cb_find)
destroy(this.cb_clear)
destroy(this.cb_update)
destroy(this.dw_pa_par_reprot)
destroy(this.dw_pa_du_par_report)
destroy(this.cb_listing)
destroy(this.dw_pa_listing_for_prod)
destroy(this.dw_pa_listing_for_controler)
destroy(this.dw_pa_dupl_listing_for_controler)
destroy(this.dw_pa_dupl_book_assignment)
destroy(this.dw_pa_book_assignment_ad)
destroy(this.dw_pa_book_assignment)
end on

event pfc_preopen;call super::pfc_preopen;this.of_SetPreference(TRUE)
this.inv_preference.of_SetToolBars(TRUE)
this.inv_preference.of_SetWindow(TRUE)
this.of_SetResize(TRUE)
this.inv_resize.of_SetOrigSize(this.workspacewidth(),this.workspaceheight())
inv_resize.of_Register(dw_pa_ancntr_data, "Scale")
inv_resize.of_Register(dw_pa_book_assignment, "Scale")
inv_resize.of_Register(dw_pa_book_assignment_ad, "Scale")
inv_resize.of_Register(dw_pa_dupl_book_assignment, "Scale")
inv_resize.of_Register(dw_pa_par_reprot, "Scale")
inv_resize.of_Register(dw_pa_du_par_report, "Scale")
inv_resize.of_Register(dw_pa_listing_for_prod, "Scale")
inv_resize.of_Register(dw_dtbbkseq, "Scale")
inv_resize.of_Register(st_no_books, "Scale")
inv_resize.of_Register(sle_no_books, "Scale")
inv_resize.of_Register(st_no_flash, "Scale")
inv_resize.of_Register(sle_no_flash, "Scale")
inv_resize.of_Register(cb_exit, "Scale")
inv_resize.of_Register(cb_clear, "Scale")
inv_resize.of_Register(cb_update, "Scale")
inv_resize.of_Register(cb_html, "Scale")
inv_resize.of_Register(cb_listing, "Scale")
inv_resize.of_Register(cb_find, "Scale")
inv_resize.of_Register(cb_booknav, "Scale")
inv_resize.of_Register(cb_labels, "Scale")
inv_resize.of_Register(cb_opf, "Scale")
end event

event closequery;Integer	li_pendingrc
Integer	li_validationrc
Integer	li_accepttextrc
Integer	li_msg
Integer	li_rc,rtn
String	ls_msgparms[]

//ib_disableclosequery=false

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

event open;call super::open;// Open the sheet in Maximized mode
this.windowstate = maximized!

m_pics_main.m_file.m_print.Enabled 			=	TRUE
m_pics_main.m_file.m_pagesetup.Enabled		=	TRUE
m_pics_main.m_file.m_printimmediate.Enabled	=	TRUE
m_pics_main.m_edit.m_addrow.Enabled	=	TRUE
m_pics_main.m_edit.m_deleterow.Enabled	=	TRUE

wf_disable_buttons()

// Set the focus to the datawindows.
dw_pa_ancntr_data.SetFocus()


end event

type sle_no_flash from u_sle within w_pa_assigning_books
integer x = 1221
integer y = 1492
integer width = 160
integer height = 92
integer taborder = 30
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
long textcolor = 255
end type

type st_no_flash from statictext within w_pa_assigning_books
integer x = 704
integer y = 1504
integer width = 507
integer height = 76
boolean bringtotop = true
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long backcolor = 67108864
boolean enabled = false
string text = "Total Number of Flash"
alignment alignment = right!
boolean focusrectangle = false
end type

type ole_ezftp from u_ezftp within w_pa_assigning_books
boolean visible = false
integer x = 1403
integer y = 1480
integer taborder = 0
string binarykey = "w_pa_assigning_books.win"
end type

type cb_booknav from commandbutton within w_pa_assigning_books
integer x = 1243
integer y = 1632
integer width = 434
integer height = 112
integer textsize = -8
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "Book Na&vigation"
end type

event clicked;OpenSheet(w_book_navigation, w_pics_main, 0, Original!)
end event

type dw_dtbbkseq from u_pics_dw within w_pa_assigning_books
boolean visible = false
integer x = 2053
integer y = 1496
integer width = 105
integer height = 84
integer taborder = 0
string dataobject = "d_dtbbkseq"
boolean vscrollbar = false
boolean livescroll = false
boolean ib_isupdateable = false
boolean ib_rmbmenu = false
boolean ib_rmbfocuschange = false
end type

event constructor;call super::constructor;this.of_SetTransObject(SQLServerTrans)
this.of_SetFilter(TRUE)
this.of_SetFind(TRUE)
this.inv_filter.of_SetStyle(1)
this.inv_filter.of_SetColumnNameSource(2)
this.of_SetSort(TRUE)
this.inv_sort.of_SetStyle(1)
this.inv_sort.of_SetColumnNameSource(2)

end event

type cb_opf from commandbutton within w_pa_assigning_books
boolean visible = false
integer x = 1554
integer y = 1484
integer width = 471
integer height = 112
boolean bringtotop = true
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
boolean enabled = false
string text = "Create/ftp OPF files"
end type

event clicked;n_ds lds

String Lcntrtype,ls_cfile,ls_dc_url, ls_ttl, ls_anno, ls_pub,	 ls_t_date, ls_creator, ls_subject, ls_remotefile, ls_remoteloc,ls_prdr 
String ls_forma, ls_Identifier, ls_isbn, ls_lang, ls_t_rights, ls_pubyr, ls_sourcepublisher, ls_sourcerights
String ls_aType, ls_dtbnarrator, ls_dtbproducer, ls_dtbproducedate, ls_dtbrevision, ls_dtbrevisiondate, ls_dtbrevisiondescription
string ls_t_metadata, ls_t_dcmetadata, ls_t_dcmetadata2, ls_t_x_metadata, ls_t_xx_metadata, ls_t_close_metadata
String ls_dtbtotaltime,	 ls_dtbaudioformat , lmsg , ls_xmlver, ls_doctype, ls_xmlnls, ls_package, ls_manifest, ls_spine,ls_chno,ls_text    
String ls_ftpsite, ls_ftpuid, ls_ftppwd

int ll_rows,ll_rows2,rtn,i,j,li_ftp_rc
long l_bkseq[],li_filenum,li_bkseq

SetNull(ls_creator)
SetNull(ls_subject)

// Create and load the datastore

lds = create n_ds
lds.dataobject = "d_dtbcreators"

SELECT ftp_site, ftp_uid, ftp_pwd
INTO :ls_ftpsite, :ls_ftpuid, :ls_ftppwd
FROM PCS_FTP_INFO
USING SQLServerTrans;
IF f_check_dberror(SQLServerTrans,"SELECTING FROM PCS_FTP_INFO ") THEN

	li_ftp_rc = w_pa_assigning_books.ole_ezftp.uf_login ( ls_ftpsite, ls_ftpuid, ls_ftppwd)
	
	IF li_ftp_rc = -1 THEN
		messagebox("FTP Error", "Unable to connect to ftp server.")
	ELSE
		ls_prdr = lower(trim(dw_pa_ancntr_data.object.prdr[1]))
		ls_remoteloc = "/pics/prd/opffiles/"+ls_prdr
		w_pa_assigning_books.ole_ezftp.uf_set_currentdirectory(ls_remoteloc)
	END IF
ELSE
	messagebox("FTP Error", "Unable to get ftp information.")
	li_ftp_rc = -1
	
END IF


// Get the contract type
Lcntrtype = dw_pa_ancntr_data.object.cntrtype[1]

IF Lcntrtype="D" THEN
	
	// Popluate the array of bkseq
	FOR i = 1 TO dw_pa_dupl_book_assignment.rowcount()
		l_bkseq[i] = dw_pa_dupl_book_assignment.object.mchar_bkseq[i]
	NEXT
	
	OpenWithParm(w_pics_retrieve_msg_box,"Getting DTD records, Please Wait...")
	dw_dtbbkseq.SetTransObject(SQLServerTrans)
	ll_rows = dw_dtbbkseq.retrieve(l_bkseq[])
	
	Close(w_pics_retrieve_msg_box)
	if ll_rows = 0 THEN
		MessageBox("ERROR", "No record(s) was found.")
		return
	else
		// record found and now create the file from the datawindow dtbbkseqs
		FOR i= 1 TO dw_pa_dupl_book_assignment.rowcount()

			 ls_xmlver = ""
			 ls_doctype = ""
			 ls_xmlnls = ""
			 ls_dc_url = ""
			 ls_ttl = ""
			 ls_anno = ""			   
			 ls_pub = ""
			 ls_t_date = ""			   
			 ls_forma = ""
			 ls_Identifier = ""
			 ls_isbn= ""			   
			 ls_lang= ""
			 ls_t_rights= ""
			 ls_pubyr= ""
			 ls_sourcepublisher= ""
			 ls_sourcerights= ""
			 ls_aType= ""
			 ls_dtbnarrator= ""
			 ls_dtbproducer= ""
			 ls_dtbproducedate= ""
			 ls_dtbrevision= ""
			 ls_dtbrevisiondate= ""
			 ls_dtbrevisiondescription= ""
			 ls_dtbtotaltime= ""
			 ls_dtbaudioformat= ""   
			 ls_manifest = ""
			 ls_spine = ""
			 ls_package = ""
			 ls_creator = ""
			 ls_text = ""
			 ls_subject = ""
			 ls_chno = ""
			
			 ls_cfile= ""
			 li_filenum = -1
			
		 	 li_bkseq = dw_dtbbkseq.object.mchar_bkseq[i]
			 ls_cfile="P:\opffiles\"+string(li_bkseq)+".opf"
			 ls_remotefile = string(li_bkseq)+".opf"
			 li_filenum = Fileopen(ls_cfile,streammode!,write!,shared!,Replace!)
			 if li_filenum = -1 then
				messagebox("File Error","This file could not be opened. Make sure the directory P:\opffiles exist.")
				Close(w_pics_retrieve_msg_box)
				RollBack using SqlServerTrans;
			
				RETURN -1
			 end if

			 lmsg = "Getting DTD file "+ls_cfile+", Please Wait..." 
			 OpenWithParm(w_pics_retrieve_msg_box,lmsg)
			
			 ls_chno = dw_dtbbkseq.object.chno[i]
			 ls_xmlver = dw_dtbbkseq.object.t_xmlver[i]+"~n"
			 ls_doctype = dw_dtbbkseq.object.t_doctype[i]+"~n"
			 ls_xmlnls = dw_dtbbkseq.object.t_xmlnls[i]+"~n"
			 ls_t_metadata = dw_dtbbkseq.object.t_metadata[i]+"~n"
			 ls_dc_url = dw_dtbbkseq.object.dc_url[i]+"~n"
//			 ls_ttl = f_change_pipe_italics_tags(f_replace_nonprn_html_tags(dw_dtbbkseq.object.ttl[i]))+"~n"
			 ls_ttl = f_replace_nonprn_html_tags(dw_dtbbkseq.object.ttl[i])+"~n"
			 
			 ll_rows2 = lds.settransobject(sqlservertrans)
			 ll_rows2 = lds.retrieve(ls_chno)
			 
			 ls_creator = ' '
			 FOR j = 1 to ll_rows2
				ls_text = lds.object.t_creators[j]+"~n"
				ls_creator = ls_creator + ls_text 			
			 NEXT
			 ls_creator = Trim(ls_creator)
			 
			 ls_subject = dw_dtbbkseq.object.t_subject[i]+"~n"
//			 ls_anno = f_change_pipe_italics_tags(f_replace_nonprn_html_tags(dw_dtbbkseq.object.anno[i]))+"~n"   
			 ls_anno = f_replace_nonprn_html_tags(dw_dtbbkseq.object.anno[i])+"~n"   
			 ls_pub = dw_dtbbkseq.object.pub[i]+"~n"
			 ls_t_date = dw_dtbbkseq.object.t_date[i]+"~n"
			 ls_forma = dw_dtbbkseq.object.forma[i]+"~n"
			 ls_Identifier = dw_dtbbkseq.object.Identifier[i]+"~n"
			 ls_isbn= dw_dtbbkseq.object.isbn[i]+"~n"
			 ls_lang= dw_dtbbkseq.object.lang[i]+"~n"
			 ls_t_rights= dw_dtbbkseq.object.t_rights[i]+"~n"
			 ls_t_dcmetadata = dw_dtbbkseq.object.t_dcmetadata[i]+"~n"
			 ls_t_x_metadata = dw_dtbbkseq.object.t_x_metadata[i]+"~n"
			 ls_pubyr= dw_dtbbkseq.object.pubyr[i]+"~n"
			 ls_sourcepublisher= dw_dtbbkseq.object.sourcepublisher[i]+"~n"
			 ls_sourcerights= dw_dtbbkseq.object.sourcerights[i]+"~n"
			 ls_aType= dw_dtbbkseq.object.aType[i]+"~n"
			 ls_dtbnarrator= dw_dtbbkseq.object.dtbnarrator[i]+"~n"
			 ls_dtbproducer= dw_dtbbkseq.object.dtbproducer[i]+"~n"
			 ls_dtbproducedate= dw_dtbbkseq.object.dtbproducedate[i]+"~n"
			 ls_dtbrevision= dw_dtbbkseq.object.dtbrevision[i]+"~n"
			 ls_dtbrevisiondate= dw_dtbbkseq.object.dtbrevisiondate[i]+"~n"
			 ls_dtbrevisiondescription= dw_dtbbkseq.object.dtbrevisiondescription[i]+"~n"
			 ls_dtbtotaltime= dw_dtbbkseq.object.dtbtotaltime[i]+"~n"
			 ls_dtbaudioformat= dw_dtbbkseq.object.dtbaudioformat[i]+"~n"
			 ls_t_xx_metadata = dw_dtbbkseq.object.t_xx_metadata[i]+"~n"
			 ls_t_close_metadata = dw_dtbbkseq.object.t_close_metadata[i]+"~n"
			 ls_manifest = dw_dtbbkseq.object.t_manifest[i]+"~n"
			 ls_spine = dw_dtbbkseq.object.t_spine[i]+"~n"
			 ls_package = dw_dtbbkseq.object.t_package[i]+"~n"
			 
			filewrite(li_filenum, ls_xmlver)
			filewrite(li_filenum, ls_doctype)
			filewrite(li_filenum, ls_xmlnls)
			filewrite(li_filenum, ls_t_metadata)
			filewrite(li_filenum, ls_dc_url)
			filewrite(li_filenum, ls_ttl)
			filewrite(li_filenum, ls_creator)
			filewrite(li_filenum, ls_subject)
			filewrite(li_filenum, ls_anno)
			filewrite(li_filenum, ls_pub)
			filewrite(li_filenum, ls_t_date)
			filewrite(li_filenum, ls_forma)
			filewrite(li_filenum, ls_Identifier)
			filewrite(li_filenum, ls_isbn)
			filewrite(li_filenum, ls_lang)
			filewrite(li_filenum, ls_t_rights)
			filewrite(li_filenum, ls_t_dcmetadata)
			filewrite(li_filenum, ls_t_x_metadata)
			filewrite(li_filenum, ls_pubyr)
			filewrite(li_filenum, ls_sourcepublisher)
			filewrite(li_filenum, ls_sourcerights)
			filewrite(li_filenum, ls_aType)
			filewrite(li_filenum, ls_dtbnarrator)
			filewrite(li_filenum, ls_dtbproducer)
			filewrite(li_filenum, ls_dtbproducedate)
			filewrite(li_filenum, ls_dtbrevision)
			filewrite(li_filenum, ls_dtbrevisiondate)
			filewrite(li_filenum, ls_dtbrevisiondescription)
			filewrite(li_filenum, ls_dtbtotaltime)
			filewrite(li_filenum, ls_dtbaudioformat)
			filewrite(li_filenum, ls_t_xx_metadata)
			filewrite(li_filenum, ls_t_close_metadata)
			filewrite(li_filenum, ls_manifest)
			filewrite(li_filenum, ls_spine)
			filewrite(li_filenum, ls_package)

			fileclose(li_filenum)

			IF li_ftp_rc <> -1 THEN
				w_pa_assigning_books.ole_ezftp.uf_upload ( ls_cfile, ls_remotefile , FALSE )
			END IF

		NEXT
		Close(w_pics_retrieve_msg_box)
		
		MessageBox("opf files","OPF files has been created and placed in P:\opffiles network directory.")
	
	end if
	
ELSE // This is total contract or just narration
	
	// Popluate the array of bkseq
	FOR i = 1 TO dw_pa_book_assignment.rowcount()
		l_bkseq[i] = dw_pa_book_assignment.object.mchar_bkseq[i]
	NEXT
	
	OpenWithParm(w_pics_retrieve_msg_box,"Getting DTD records, Please Wait...")
	dw_dtbbkseq.SetTransObject(SQLServerTrans)
	ll_rows = dw_dtbbkseq.retrieve(l_bkseq[])
	
	Close(w_pics_retrieve_msg_box)
	if ll_rows = 0 THEN
		MessageBox("ERROR", "No record(s) was found.")
		return
	else
		lmsg = "Making DTD files, Please Wait..." 
		OpenWithParm(w_pics_retrieve_msg_box,lmsg)
		
		
		// record found and now create the file from the datawindow dtbbkseqs
		//messagebox('books',string(ll_rows))
		FOR i= 1 TO dw_pa_book_assignment.rowcount()
				 ls_xmlver = ""
				 ls_doctype = ""
				 ls_xmlnls = ""
				 ls_dc_url = ""
				 ls_ttl = ""
				 ls_anno = ""			   
				 ls_pub = ""
				 ls_t_date = ""			   
				 ls_forma = ""
				 ls_Identifier = ""
				 ls_isbn= ""			   
				 ls_lang= ""
				 ls_t_rights= ""
				 ls_pubyr= ""
				 ls_sourcepublisher= ""
				 ls_sourcerights= ""
				 ls_aType= ""
				 ls_dtbnarrator= ""
				 ls_dtbproducer= ""
				 ls_dtbproducedate= ""
				 ls_dtbrevision= ""
				 ls_dtbrevisiondate= ""
				 ls_dtbrevisiondescription= ""
				 ls_dtbtotaltime= ""
				 ls_dtbaudioformat= ""          
				 ls_manifest = ""
				 ls_spine = ""
				 ls_package = ""
				 ls_creator = ""
				 ls_text = ""
				 ls_subject = ""
				 ls_chno = ""
				
				 ls_cfile= ""
				 li_filenum = -1
				 
			 	 li_bkseq = dw_dtbbkseq.object.mchar_bkseq[i]
//				MessageBox("data",string(l_bkseq[i])+" i = "+string(i)+" ll_rows = "+string(ll_rows))
				ls_cfile="P:\opffiles\"+string(li_bkseq)+".opf"
				ls_remotefile = string(li_bkseq)+".opf"
//				MessageBox("data",ls_cfile)
				li_filenum = Fileopen(ls_cfile,streammode!,write!,shared!,Replace!)
				if li_filenum = -1 then
					messagebox("File Error","This file could not be opened. Make sure the directory p:\opffiles exist.")
					Close(w_pics_retrieve_msg_box)
					RollBack using SqlServerTrans;
				
					RETURN -1
				end if
	
	
			 	 ls_chno = dw_dtbbkseq.object.chno[i]
				 ls_xmlver = dw_dtbbkseq.object.t_xmlver[i]+"~n"
				 ls_doctype = dw_dtbbkseq.object.t_doctype[i]+"~n"
				 ls_xmlnls = dw_dtbbkseq.object.t_xmlnls[i]+"~n"
				 ls_t_metadata = dw_dtbbkseq.object.t_metadata[i]+"~n"
				 ls_dc_url = dw_dtbbkseq.object.dc_url[i]+"~n"
//				 ls_ttl = f_change_pipe_italics_tags(f_replace_nonprn_html_tags(dw_dtbbkseq.object.ttl[i]))+"~n"
				 ls_ttl = f_replace_nonprn_html_tags(dw_dtbbkseq.object.ttl[i])+"~n"
			 
				 ll_rows2 = lds.settransobject(sqlservertrans)
				 ll_rows2 = lds.retrieve(ls_chno)
				 
				 ls_creator = ' '
				 FOR j = 1 to ll_rows2
					ls_text = lds.object.t_creators[j]+"~n"
					ls_creator = ls_creator + ls_text 			
				 NEXT
				 ls_creator = Trim(ls_creator)
			 
				 ls_subject = dw_dtbbkseq.object.t_subject[i]+"~n"
				// ls_anno = f_change_pipe_italics_tags(f_replace_nonprn_html_tags(dw_dtbbkseq.object.anno[i]))+"~n"   
				 ls_anno = f_replace_nonprn_html_tags(dw_dtbbkseq.object.anno[i])+"~n"   
				 ls_pub = dw_dtbbkseq.object.pub[i]+"~n"
				 ls_t_date = dw_dtbbkseq.object.t_date[i]+"~n"
				 ls_forma = dw_dtbbkseq.object.forma[i]+"~n"
				 ls_Identifier = dw_dtbbkseq.object.Identifier[i]+"~n"
				 ls_isbn= dw_dtbbkseq.object.isbn[i]+"~n"
				 ls_lang= dw_dtbbkseq.object.lang[i]+"~n"
				 ls_t_rights= dw_dtbbkseq.object.t_rights[i]+"~n"
				 ls_t_dcmetadata = dw_dtbbkseq.object.t_dcmetadata[i]+"~n"
				 ls_t_x_metadata = dw_dtbbkseq.object.t_x_metadata[i]+"~n"
				 ls_pubyr= dw_dtbbkseq.object.pubyr[i]+"~n"
				 ls_sourcepublisher= dw_dtbbkseq.object.sourcepublisher[i]+"~n"
				 ls_sourcerights= dw_dtbbkseq.object.sourcerights[i]+"~n"
				 ls_aType= dw_dtbbkseq.object.aType[i]+"~n"
				 ls_dtbnarrator= dw_dtbbkseq.object.dtbnarrator[i]+"~n"
				 ls_dtbproducer= dw_dtbbkseq.object.dtbproducer[i]+"~n"
				 ls_dtbproducedate= dw_dtbbkseq.object.dtbproducedate[i]+"~n"
				 ls_dtbrevision= dw_dtbbkseq.object.dtbrevision[i]+"~n"
				 ls_dtbrevisiondate= dw_dtbbkseq.object.dtbrevisiondate[i]+"~n"
				 ls_dtbrevisiondescription= dw_dtbbkseq.object.dtbrevisiondescription[i]+"~n"
				 ls_dtbtotaltime= dw_dtbbkseq.object.dtbtotaltime[i]+"~n"
				 ls_dtbaudioformat= dw_dtbbkseq.object.dtbaudioformat[i]+"~n"
				 ls_t_xx_metadata = dw_dtbbkseq.object.t_xx_metadata[i]+"~n"
				 ls_t_close_metadata = dw_dtbbkseq.object.t_close_metadata[i]+"~n"
				 ls_manifest = dw_dtbbkseq.object.t_manifest[i]+"~n"
				 ls_spine = dw_dtbbkseq.object.t_spine[i]+"~n"
				 ls_package = dw_dtbbkseq.object.t_package[i]+"~n"
				 
				filewrite(li_filenum, ls_xmlver)
				filewrite(li_filenum, ls_doctype)
				filewrite(li_filenum, ls_xmlnls)
				filewrite(li_filenum, ls_t_metadata)
				filewrite(li_filenum, ls_dc_url)
				filewrite(li_filenum, ls_ttl)
				filewrite(li_filenum, ls_creator)
				filewrite(li_filenum, ls_subject)
				filewrite(li_filenum, ls_anno)
				filewrite(li_filenum, ls_pub)
				filewrite(li_filenum, ls_t_date)
				filewrite(li_filenum, ls_forma)
				filewrite(li_filenum, ls_Identifier)
				filewrite(li_filenum, ls_isbn)
				filewrite(li_filenum, ls_lang)
				filewrite(li_filenum, ls_t_rights)
				filewrite(li_filenum, ls_t_dcmetadata)
				filewrite(li_filenum, ls_t_x_metadata)
				filewrite(li_filenum, ls_pubyr)
				filewrite(li_filenum, ls_sourcepublisher)
				filewrite(li_filenum, ls_sourcerights)
				filewrite(li_filenum, ls_aType)
				filewrite(li_filenum, ls_dtbnarrator)
				filewrite(li_filenum, ls_dtbproducer)
				filewrite(li_filenum, ls_dtbproducedate)
				filewrite(li_filenum, ls_dtbrevision)
				filewrite(li_filenum, ls_dtbrevisiondate)
				filewrite(li_filenum, ls_dtbrevisiondescription)
				filewrite(li_filenum, ls_dtbtotaltime)
				filewrite(li_filenum, ls_dtbaudioformat)
				filewrite(li_filenum, ls_t_xx_metadata)
				filewrite(li_filenum, ls_t_close_metadata)
				filewrite(li_filenum, ls_manifest)
				filewrite(li_filenum, ls_spine)
				filewrite(li_filenum, ls_package)
	
				fileclose(li_filenum)
				
				IF li_ftp_rc <> -1 THEN
					w_pa_assigning_books.ole_ezftp.uf_upload ( ls_cfile, ls_remotefile , FALSE )
				END IF


	
		NEXT
		Close(w_pics_retrieve_msg_box)
		
		MessageBox("opf files","OPF files has been created and placed in p:\opffiles network directory.")
		IF li_ftp_rc <> -1 THEN
			w_pa_assigning_books.ole_ezftp.uf_logout()
		END IF
	
	end if
	
END IF

end event

type cb_labels from commandbutton within w_pa_assigning_books
integer x = 832
integer y = 1632
integer width = 306
integer height = 112
boolean bringtotop = true
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "Print &Labels"
end type

event clicked;string ls_cntr, ls_date

ls_cntr=is_cntr
if isnull(ls_cntr) or ls_cntr="" then
	messagebox('Error','You must to enter contract number.')
	return
end if
ls_date=string(id_assigndt,'mm/dd/yyyy')
if isnull(ls_date) or ls_date='01/01/1900' or ls_date="" then
	ls_date=string(today(),'mm/dd/yyyy')
end if
ls_cntr=ls_cntr+','+ls_date
OpenSheetWithParm(w_pa_assign_books_label,ls_cntr,w_pics_main, 0, Original!)
end event

type cb_html from commandbutton within w_pa_assigning_books
integer x = 46
integer y = 1632
integer width = 311
integer height = 112
boolean bringtotop = true
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "HT&ML PAR"
end type

event clicked;String Lcntrmed,Lcntrtype,Lcntr,Lconno,lconnos[],list_bkseq,Lprdr
string ls_html,db_inst
int ll_rows,rtn,i,lparid
date Lassigndt, ltday, Lschenddt
long Lbkseq

Inet linet_base

db_inst = w_pics_main.web_db_inst

Lcntr = dw_pa_ancntr_data.object.cntr[1]
// Get the contract type
Lcntrtype = dw_pa_ancntr_data.object.cntrtype[1]
Lcntrmed = dw_pa_ancntr_data.object.cntrmed[1]
Lprdr = dw_Pa_ancntr_data.object.prdr[1]

IF Lcntrtype="D" THEN

	ll_rows = dw_pa_dupl_book_assignment.RowCount()	
	if ll_rows = 0 THEN
		MessageBox("ERROR", "No books are listed")
		RETURN
	else
		list_bkseq = ' '
		// Popluate the array of control numbers and call the function to build the PAR records.
		FOR i = 1 TO ll_rows
			IF i < ll_rows THEN
				list_bkseq = list_bkseq + string(dw_pa_dupl_book_assignment.object.mchar_bkseq[i]) + ','
			ELSE
				list_bkseq = list_bkseq + string(dw_pa_dupl_book_assignment.object.mchar_bkseq[i])
			END IF
			
		NEXT
	end if	
	Randomize(0)
	lparid = Rand(32767)
	ltday = today()	
   //messagebox('parbook','lparid = '+string(lparid)+' list_bkseq = '+list_bkseq+' date = '+string(ltday))	
	INSERT INTO PARBOOKS@pic_link
	VALUES (:lparid, :list_bkseq, :ltday)
	USING SQLServerTrans;
	IF f_check_dberror(SQLServerTrans,"PARBOOKS")=FALSE THEN
		rollback using SQLServerTrans;
		RETURN -1
	ELSE
		commit using SQLServerTrans;
	END IF
	//https://oraserve.loc.gov:4446/pict/par_report.report4?form_choice=console&form_font=11&form_prodcode=APH&form_id=30298
	ls_html = 'https://oraserve.loc.gov:4446/'+lower(db_inst)+'/par_report.report4?form_choice=console&form_font=11&form_prodcode='+Lprdr+'&form_id='+string(lparid)
   //messagebox('ls_html',ls_html)
	this.GetContextService("Internet",linet_base)
	linet_base.HyperlinkToURL(ls_html)
	If Isvalid(linet_base) Then destroy linet_base

ELSEIF Lcntrtype='A'   THEN // This is a conversion contract
	
	ll_rows = dw_pa_book_assignment_ad.RowCount()	
	if ll_rows = 0 THEN
		MessageBox("ERROR", "No books are listed")
		RETURN
	else
		list_bkseq = ' '
		// Popluate the array of control numbers and call the function to build the PAR records.
		FOR i = 1 TO ll_rows
			IF i < ll_rows THEN
				list_bkseq = list_bkseq + string(dw_pa_book_assignment_ad.object.mchar_bkseq[i]) + ','
			ELSE
				list_bkseq = list_bkseq + string(dw_pa_book_assignment_ad.object.mchar_bkseq[i])
			END IF
			
		NEXT
	end if	
	Randomize(0)
	lparid = Rand(32767)
	ltday = today()	
   //messagebox('parbook','lparid = '+string(lparid)+' list_bkseq = '+list_bkseq+' date = '+string(ltday))	
	INSERT INTO PARBOOKS@pic_link
	VALUES (:lparid, :list_bkseq, :ltday)
	USING SQLServerTrans;
	IF f_check_dberror(SQLServerTrans,"PARBOOKS")=FALSE THEN
		rollback using SQLServerTrans;
		RETURN -1
	ELSE
		commit using SQLServerTrans;
	END IF
	ls_html = 'https://oraserve.loc.gov:4446/'+lower(db_inst)+'/par_report.report4?form_choice=console&form_font=11&form_prodcode=ALL&form_id='+string(lparid)
   //messagebox('ls_html',ls_html)
	this.GetContextService("Internet",linet_base)
	linet_base.HyperlinkToURL(ls_html)
	If Isvalid(linet_base) Then destroy linet_base

ELSE // This is total contract or just narration
	
	ll_rows = dw_pa_book_assignment.RowCount()	
	if ll_rows = 0 THEN
		MessageBox("ERROR", "No books are listed")
		RETURN
	else
		list_bkseq = ' '
		// Popluate the array of control numbers and call the function to build the PAR records.
		FOR i = 1 TO ll_rows
			IF i < ll_rows THEN
				list_bkseq = list_bkseq + string(dw_pa_book_assignment.object.mchar_bkseq[i]) + ','
			ELSE
				list_bkseq = list_bkseq + string(dw_pa_book_assignment.object.mchar_bkseq[i])
			END IF
			
		NEXT
	end if	
	Randomize(0)
	lparid = Rand(32767)
	ltday = today()	
   //messagebox('parbook','lparid = '+string(lparid)+' list_bkseq = '+list_bkseq+' date = '+string(ltday))	
	INSERT INTO PARBOOKS@pic_link
	VALUES (:lparid, :list_bkseq, :ltday)
	USING SQLServerTrans;
	IF f_check_dberror(SQLServerTrans,"PARBOOKS")=FALSE THEN
		rollback using SQLServerTrans;
		RETURN -1
	ELSE
		commit using SQLServerTrans;
	END IF
	ls_html = 'https://oraserve.loc.gov:4446/'+lower(db_inst)+'/par_report.report4?form_choice=console&form_font=11&form_prodcode='+Lprdr+'&form_id='+string(lparid)
   //messagebox('ls_html',ls_html)
	this.GetContextService("Internet",linet_base)
	linet_base.HyperlinkToURL(ls_html)
	If Isvalid(linet_base) Then destroy linet_base
	
	
END IF
end event

type dw_par_on_web from u_pics_dw within w_pa_assigning_books
boolean visible = false
integer x = 2743
integer y = 1504
integer width = 91
integer height = 76
integer taborder = 0
string dataobject = "d_create_par_record_web"
boolean vscrollbar = false
boolean livescroll = false
end type

event constructor;call super::constructor;this.of_SetFilter(TRUE)
this.of_SetFind(TRUE)
this.inv_filter.of_SetStyle(1)
this.inv_filter.of_SetColumnNameSource(2)
this.of_SetSort(TRUE)
this.inv_sort.of_SetStyle(1)
this.inv_sort.of_SetColumnNameSource(2)

end event

type cb_exit from u_cb within w_pa_assigning_books
integer x = 2624
integer y = 1632
integer width = 233
integer height = 112
integer taborder = 0
string text = "E&xit"
end type

event clicked;call super::clicked;long li_cnt, li_cnt2,li_re
IF IsValid(w_pics_retrieve_msg_box) THEN
	close(w_pics_retrieve_msg_box)
ELSEIF IsValid(w_pics_update_msg_box) THEN
	close(w_pics_update_msg_box)
END IF
dw_pa_dupl_book_assignment.AcceptText()
dw_pa_book_assignment.AcceptText()
ib_disableclosequery=false
li_cnt=dw_pa_dupl_book_assignment.ModifiedCount ( ) 
li_cnt2=dw_pa_book_assignment.ModifiedCount ( ) 
if li_cnt>0 or li_cnt2>0 then
	li_re=messagebox('','Do you want to save change?',Question!,YesNo!,1)
end if
if li_re=1 then
	cb_update.triggerEvent( clicked!)
end if
ib_disableclosequery=true
close(parent)
//parent.Event pfc_close()


end event

type dw_pa_ancntr_data from u_pics_dw within w_pa_assigning_books
integer y = 8
integer width = 2921
integer height = 156
integer taborder = 10
string dataobject = "d_pa_ancntr_data"
boolean vscrollbar = false
boolean livescroll = false
boolean ib_isupdateable = false
boolean ib_rmbmenu = false
boolean ib_rmbfocuschange = false
string is_updatesallowed = ""
end type

event ue_postconstructor;call super::ue_postconstructor;dw_pa_ancntr_data.of_SetTransObject( SQLServerTrans )

end event

event pfc_insertrow;//
return -1
end event

event pfc_addrow;//
return -1
end event

event pfc_deleterow;//
return -1
end event

event rbuttondown;//
end event

event rbuttonup;//
end event

type st_no_books from statictext within w_pa_assigning_books
integer x = 18
integer y = 1504
integer width = 507
integer height = 76
boolean bringtotop = true
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long backcolor = 67108864
boolean enabled = false
string text = "Total Number of Books"
alignment alignment = right!
boolean focusrectangle = false
end type

type sle_no_books from u_sle within w_pa_assigning_books
integer x = 535
integer y = 1492
integer width = 160
integer height = 92
integer taborder = 0
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
long textcolor = 255
end type

type cb_find from u_cb within w_pa_assigning_books
integer x = 1751
integer y = 1632
integer width = 233
integer height = 112
integer taborder = 0
string text = "F&ind"
boolean default = true
end type

event clicked;call super::clicked;Long ll_rows,Lbkseq,Lgray
Int rtn,rownum,i
String Lcntr,Lprod_stage,where_clause,order_clause,select_clause,rc,lmed,lcntrtype,Lpstage,qrtn,Lassigndt,ls_cntrcvcd,Lotherconno,lflashind
Date Lschenddt,Ldate
String Lps,Lps2,ls_conno
Lgray = Rgb(192,192,192)
Int Lstageorder,li_no_flash

dw_pa_ancntr_data.AcceptText()

Lcntr = dw_pa_ancntr_data.GetText()
is_cntr=Trim(Lcntr)
// Retrieve from database with contract number as a argument.
ll_rows = dw_pa_ancntr_data.Retrieve(Lcntr)
// If rows exist
IF ll_rows > 0 THEN
	// Get the medium
	lmed = dw_pa_ancntr_data.object.cntrmed[1]
	// Get the contract type
	lcntrtype = dw_pa_ancntr_data.object.cntrtype[1]
	// Get thecommercial or volunteer
	ls_cntrcvcd = dw_pa_ancntr_data.object.cntrcvcd[1]
	IF IsNull(ls_cntrcvcd) OR ls_cntrcvcd = "" THEN
		Messagebox("Contract Error", "You have not assigned commercial or volunteer for contract number: " + Lcntr +".",stopSign!)
		cb_clear.TriggerEvent(clicked!)
		RETURN
	END IF

	// Check to see if this is a duplication contract
	IF lcntrtype="D" THEN
		// If yes, show the duplication datawindow.
		dw_pa_book_assignment.Hide()
		// Hide the other datawindow
		dw_pa_dupl_book_assignment.Show()
		IF lmed = 'FC' THEN
			dw_pa_dupl_book_assignment.object.prod_qnty_t.text = "Flash~r~nCartridges"
		END IF
		// Display a message for new batch or not.
		rtn = Messagebox("Book duplication Assignment","Is this a new batch of duplication books for the contract number: "+Lcntr+"?",question!, yesNoCancel!, 1)
		IF rtn=1 THEN
			// when assigning assignment date, since this is a new batch, date will be today's date
			new_batch=TRUE
			// If this is a new batch of books
			dw_pa_ancntr_data.enabled = FALSE
			// Insert a new row
			dw_pa_dupl_book_assignment.DeleteRow(0)
			dw_pa_dupl_book_assignment.InsertRow(0)
			// set the taborder for duplication datawindow.
			wf_set_taborder_dupl()
			cb_find.enabled = FALSE
			cb_clear.enabled = TRUE
			cb_update.enabled = TRUE
			cb_listing.enabled = TRUE
			cb_html.enabled = TRUE
			IF lmed = 'BR' OR lmed = 'P/B' THEN
				cb_opf.enabled = FALSE
			ELSE
				cb_opf.enabled = TRUE
			END IF
			cb_labels.enabled = TRUE
			cb_booknav.enabled = TRUE
		ELSEIF rtn = 2 THEN
			// This is a existing batch of books for duplication
			new_batch=FALSE
			dw_pa_ancntr_data.enabled = FALSE
			// Build your select statement to retrieve all the books for this contract
			// where the production stage is for duplication.
			select_clause=	original_dupl_select
			where_clause = " and prod.cntr=~'" + Lcntr + "~'"
			where_clause = where_clause + " and prod.prodstage in (~'DU~',~'FC~')"
			order_clause = " order by prod.bkseq "
			Open(w_pa_get_assignment_date)

			// If assignment date was specified, add that to your criteria.
			IF IsNull(message.StringParm)=FALSE AND message.StringParm<>"" THEN
				Ldate = Date(message.StringParm)
				id_assigndt=Ldate
		   	where_clause = where_clause + " and prod.assigndt=" + "~'" + String(Ldate,'DD-MMM-YYYY') + "~'"
				Lassigndt = message.StringParm
				local_assinged_date=Date(Lassigndt)
			ELSE
				cb_clear.TriggerEvent(clicked!)
				RETURN
			END IF
		
			mod_dupl_string 	 = "DataWindow.Table.Select=~"" + select_clause + where_clause + order_clause + "~""
			rc = dw_pa_dupl_book_assignment.Modify(mod_dupl_string)
//messagebox('sel', dw_pa_dupl_book_assignment.GetSQLSelect ( ))			
			IF rc = "" THEN
				Open(w_pics_retrieve_msg_box)
				// retrieve the data.
				ll_rows = dw_pa_dupl_book_assignment.Retrieve()
				Close(w_pics_retrieve_msg_box)
				IF ll_rows < 1 THEN 
					// If no books were retrieved, display a error message and close the retrive window.
					IF IsNull(Lassigndt)=FALSE AND Lassigndt<>"" THEN
     					Messagebox("ERROR", "There are no books assigned for duplication on contract number: " + Lcntr +" in "+Lassigndt+" .",stopSign!)
						cb_clear.TriggerEvent(clicked!)
						RETURN
					ELSE						  
     					Messagebox("ERROR", "There are no books assigned for duplication on contract number: " + Lcntr +".",stopSign!)
						cb_clear.TriggerEvent(clicked!)
						RETURN
					END IF						  
					dw_pa_dupl_book_assignment.DeleteRow(0)
					dw_pa_dupl_book_assignment.InsertRow(0)
					dw_pa_dupl_book_assignment.object.prod_assigndt[1] = Today()
				END IF				
				sle_no_books.text = String(ll_rows)
				dw_pa_dupl_book_assignment.ResetUpdate()
				// Get number of flash
				SELECT Count(*)
				INTO :li_no_flash
				FROM prod
				where cntr = :Lcntr
				AND assigndt = to_date(:Lassigndt,'MM/DD/YYYY')
				AND prodstage = 'FC'
				USING SqlServerTrans;
				sle_no_flash.text = String(li_no_flash)
				
			ELSE
     			Messagebox("ERROR", "Can not modify the select statement: "+mod_string,stopSign!)
			END IF
			wf_set_taborder_dupl()
			cb_find.enabled = FALSE
			cb_clear.enabled = TRUE
			cb_update.enabled = TRUE
			cb_listing.enabled = TRUE
			cb_html.enabled = TRUE
			IF lmed = 'BR' OR lmed = 'P/B' THEN
				cb_opf.enabled = FALSE
			ELSE
				cb_opf.enabled = TRUE
			END IF
			cb_labels.enabled = TRUE
			cb_booknav.enabled = TRUE
		ELSEIF rtn=3 THEN
			// If user cancel out.
			new_batch=FALSE
			dw_pa_ancntr_data.Reset()
			dw_pa_ancntr_data.InsertRow(0)
			dw_pa_ancntr_data.SetFocus()
			cb_listing.enabled = FALSE
			cb_html.enabled = FALSE
			cb_update.enabled = FALSE
			cb_clear.enabled = FALSE
			cb_find.enabled = TRUE
			cb_booknav.enabled = TRUE
		END IF
	ELSEIF lcntrtype="A" THEN
		// Conversion books: production stages is AD, DT and only RC and RTB
		dw_pa_dupl_book_assignment.Hide()
		dw_pa_book_assignment.Hide()
		dw_pa_book_assignment_ad.Show()
		dw_pa_book_assignment_ad.object.mchar_applen_t.text = "Est.~rTracks"
		
		original_select = dw_pa_book_assignment_ad.Describe("DataWindow.Table.Select")

		      // This is a existing batch of books
			new_batch=FALSE
			dw_pa_ancntr_data.enabled = FALSE

			select_clause=	original_select

			where_clause = " and prod.cntr = ~'" + Lcntr + "~'"
			where_clause = where_clause +" and prod.prodstage ="+"~'AD~' and mchar.med in ("+"~'RC~'"+",~'RTB~')"
			order_clause = " order by prod.bkseq "
		
			Open(w_pa_get_assignment_date)
			
			// If assigned date was entered, add that to your criteria.
			IF IsNull(message.StringParm)=FALSE AND message.StringParm<>"" THEN
				Ldate = Date(message.StringParm)
		   	       where_clause = where_clause + " and to_char(prod.assigndt,"+"~'MM/DD/YYYY"+"~')"+"= ~'" + String(Ldate,'MM/DD/YYYY') + "~'"
				Lassigndt = message.StringParm	
				id_assigndt=Ldate
				local_assinged_date=Date(Lassigndt)
			ELSE
				cb_clear.TriggerEvent(clicked!)
				RETURN
			END IF
		
			mod_string 	 = "DataWindow.Table.Select=~"" + select_clause + where_clause + order_clause + "~""
			//messagebox("select",mod_string)
			rc = dw_pa_book_assignment_ad.Modify(mod_string)
			IF rc = "" THEN
				Open(w_pics_retrieve_msg_box)
				// retrieve the data
				ll_rows = dw_pa_book_assignment_ad.Retrieve()
				Close(w_pics_retrieve_msg_box)
				IF ll_rows < 1 THEN 
					// If no rows was found.
					IF IsNull(Lassigndt)=FALSE AND Lassigndt<>"" THEN
     					Messagebox("ERROR", "There are no books assigned for contract number: " + Lcntr +" in "+Lassigndt+" .",stopSign!)
						cb_clear.TriggerEvent(clicked!)
						RETURN
					ELSE						  
     					Messagebox("ERROR", "There are no books assigned for contract number: " + Lcntr +".",stopSign!)
						cb_clear.TriggerEvent(clicked!)
						RETURN
					END IF						  
				ELSE
					FOR i=1 TO ll_rows 
						Lbkseq = dw_pa_book_assignment_ad.object.mchar_bkseq[i]
						SELECT schenddt INTO :Lschenddt FROM prod 
						where bkseq = :Lbkseq AND prodstage = 'DT' AND cntr = :Lcntr
						USING SqlServerTrans;
						dw_pa_book_assignment_ad.object.prod_schenddt[i] = Lschenddt
					NEXT
					dw_pa_book_assignment_ad.ResetUpdate()
					sle_no_books.text = String(ll_rows)
					// Get number of flash
					SELECT Count(*)
					INTO :li_no_flash
					FROM prod
					where cntr = :Lcntr
					AND assigndt = to_date(:Lassigndt,'MM/DD/YYYY')
					AND prodstage = 'FC'
					USING SqlServerTrans;
					sle_no_flash.text = String(li_no_flash)
				END IF				 
			ELSE
     			Messagebox("ERROR", "Can not modify the select statement: "+mod_string+" rc = "+rc,stopSign!)
			END IF
			wf_set_taborder()
     			Messagebox("Conversion updates information", "You are only allowed to update due date on this screen for conversion contracts.",information!)
			
			cb_find.enabled = FALSE
			cb_clear.enabled = TRUE
			cb_update.enabled = TRUE
			cb_listing.enabled = TRUE
			cb_html.enabled = TRUE
			cb_opf.enabled = TRUE
			cb_labels.enabled = TRUE
			cb_booknav.enabled = TRUE

	ELSE // This contract is not a duplication contract, it could be any other contract type.
		// Hide the duplication datawindow and show the other one.
		dw_pa_dupl_book_assignment.Hide()
		dw_pa_book_assignment.Show()
		// If media is braille or print-braille, change the text on the datawindow.
		IF lmed="BR" OR lmed="P/B" THEN
			dw_pa_book_assignment.object.mchar_applen_t.text = "Braille~rPages"
		ELSEIF lmed="RC" OR lmed="RTB" THEN
			dw_pa_book_assignment.object.mchar_applen_t.text = "Est.~rTrks"
		END IF
		// Ask, if this is a new set of batch.
		rtn = Messagebox("Book Assignment","Is this a new batch of books for the contract number: "+Lcntr+"?",question!, yesNoCancel!, 1)
		IF rtn=1 THEN
			// If this is a new batch of books
			// when assigning assignment date, since this is a new batch, date will be today's date
			new_batch=TRUE
			dw_pa_ancntr_data.enabled = FALSE
			dw_pa_book_assignment.DeleteRow(0)
			dw_pa_book_assignment.InsertRow(0)
			wf_set_taborder_get_conno()
			dw_pa_book_assignment.SetFocus()
			cb_find.enabled = FALSE
			cb_clear.enabled = TRUE
			cb_update.enabled = TRUE
			cb_listing.enabled = TRUE
			cb_html.enabled = TRUE
			IF lmed = 'BR' OR lmed = 'P/B' THEN
				cb_opf.enabled = FALSE
			ELSE
				cb_opf.enabled = TRUE
			END IF
			cb_labels.enabled = TRUE
			cb_booknav.enabled = TRUE
		ELSEIF rtn = 2 THEN
			// This is a existing batch of books
			new_batch=FALSE
			dw_pa_ancntr_data.enabled = FALSE
			// Build the select statement. Get the production stage from prodstage table
			// based on media and contract type, If there are more than one stages, get
			// the prodstage for the first stage of production.(with alternate flag set to 'N')
			select_clause=	original_select

			SELECT prodstage INTO :Lps2 
				FROM prodstage 
				where cntrtype = :lcntrtype 
				AND cntrmed = :lmed 
				AND stageorder=1 
				AND alternate='Y' 
			USING SqlServerTrans;
			SELECT prodstage INTO :Lps 
				FROM prodstage 
				where cntrtype = :lcntrtype 
				AND cntrmed = :lmed 
				AND stageorder=1 
				AND (alternate<>'Y' OR alternate IS NULL) 
			USING SqlServerTrans;
			IF Lps="" OR IsNull(Lps) THEN
				// If production stage was not found. Display a error message.
				Messagebox("ERROR","Production stages is null for this contract type. Please review this contract type.")
				RETURN -1
			END IF
			
			where_clause = " and prod.cntr = ~'" + Lcntr + "~'"
			where_clause = where_clause +" and prod.prodstage in ("+"~'"+Lps+"~'"+","+"~'"+Lps2+"~'"+")"
			order_clause = " order by prod.bkseq "
		
			Open(w_pa_get_assignment_date)
			
			// If assigned date was entered, add that to your criteria.
			IF IsNull(message.StringParm)=FALSE AND message.StringParm<>"" THEN
				Ldate = Date(message.StringParm)
		   	where_clause = where_clause + " and prod.assigndt=" + "~'" + String(Ldate,'DD-MMM-YYYY') + "~'"
				Lassigndt = message.StringParm	
				id_assigndt=Ldate
				local_assinged_date=Date(Lassigndt)
			ELSE
				cb_clear.TriggerEvent(clicked!)
				RETURN
			END IF
		
			mod_string 	 = "DataWindow.Table.Select=~"" + select_clause + where_clause + order_clause + "~""
//			messagebox("select",mod_string)
			rc = dw_pa_book_assignment.Modify(mod_string)
			IF rc = "" THEN
				Open(w_pics_retrieve_msg_box)
				// retrieve the data
				ll_rows = dw_pa_book_assignment.Retrieve()
				Close(w_pics_retrieve_msg_box)
				IF ll_rows < 1 THEN 
					// If no rows was found.
					IF IsNull(Lassigndt)=FALSE AND Lassigndt<>"" THEN
     					Messagebox("ERROR", "There are no books assigned for contract number: " + Lcntr +" in "+Lassigndt+" .",stopSign!)
						cb_clear.TriggerEvent(clicked!)
						RETURN
					ELSE						  
     					Messagebox("ERROR", "There are no books assigned for contract number: " + Lcntr +".",stopSign!)
						cb_clear.TriggerEvent(clicked!)
						RETURN
					END IF						  
					dw_pa_book_assignment.DeleteRow(0)
					dw_pa_book_assignment.InsertRow(0)
					dw_pa_book_assignment.object.prod_assigndt[1] = Today()
				ELSE
					// Now is the time to get the schedule end date for this books.
					// First, find out how many stages are involved for production of these books.
					// Second, refer to prodstage table to get the last production stage.
					// Third, refer to prod table with that production stage and book number to
					// get the schedule end date of this book.
					
					SELECT Max(stageorder) INTO :Lstageorder 
						FROM prodstage 
						where cntrtype = :lcntrtype 
						AND cntrmed = :lmed 
					USING SqlServerTrans;
					// With RTB this method will work as well, because max(stageorder) will be 3
					// and that will the duplication stage.
					IF Lstageorder > 1 THEN					
						SELECT prodstage INTO :Lps 
							FROM prodstage 
							where cntrtype = :lcntrtype 
							AND cntrmed = :lmed 
							AND stageorder = :Lstageorder
						USING SqlServerTrans;
						//	messagebox("stageorder",string(Lstageorder)+" "+Lps)
						// If the maxprodstage is ZM then this is narration only contract and schenddt should be 
						// from MA production stage

						// ZM Prodstage
						IF Trim(Lps) = 'ZM' THEN
							FOR i=1 TO ll_rows 
								Lbkseq = dw_pa_book_assignment.object.mchar_bkseq[i]
								SELECT schenddt INTO :Lschenddt FROM prod 
								where bkseq = :Lbkseq AND prodstage in ('MA','AB')
								USING SqlServerTrans;
								dw_pa_book_assignment.object.prod_schenddt[i] = Lschenddt
							NEXT
						// Braille prodstage
						ELSEIF (Trim(Lps) = 'EM' OR Trim(Lps) = 'PR') THEN
							FOR i=1 TO ll_rows 
								Lbkseq = dw_pa_book_assignment.object.mchar_bkseq[i]
								SELECT schenddt INTO :Lschenddt FROM prod 
								where bkseq = :Lbkseq AND prodstage in ('PR','EM')
								USING SqlServerTrans;
								dw_pa_book_assignment.object.prod_schenddt[i] = Lschenddt
							NEXT
						ELSEIF Trim(Lps) = 'DU' THEN
							FOR i=1 TO ll_rows 
								Lbkseq = dw_pa_book_assignment.object.mchar_bkseq[i]
								SELECT schenddt INTO :Lschenddt FROM prod 
								where bkseq = :Lbkseq AND prodstage = 'DU'
								USING SqlServerTrans;
								dw_pa_book_assignment.object.prod_schenddt[i] = Lschenddt
							NEXT
						ELSEIF Trim(Lps) = 'FC' THEN
							FOR i=1 TO ll_rows 
								Lbkseq = dw_pa_book_assignment.object.mchar_bkseq[i]
								SELECT schenddt INTO :Lschenddt FROM prod 
								where bkseq = :Lbkseq AND prodstage = 'DU'
								USING SqlServerTrans;
								dw_pa_book_assignment.object.prod_schenddt[i] = Lschenddt
							NEXT
						// Any other prodstage
						END IF
						
						FOR i=1 TO ll_rows 
							SetNull(lflashind)							
							Lotherconno = dw_pa_book_assignment.object.mchar_other_media_conno[i]
							
							IF NOT(IsNull(Lotherconno)) THEN
								SELECT flash_indicator
								INTO :lflashind
								FROM mchar
								where conno = :Lotherconno
								USING SqlServerTrans;
							END IF

							IF NOT(IsNull(lflashind)) THEN
								dw_pa_book_assignment.object.flash_indicator[i] = lflashind
							END IF
						
						NEXT // ll_rows
													
					END IF
					
					dw_pa_book_assignment.ResetUpdate()
					sle_no_books.text = String(ll_rows)
					// Get number of flash
					SELECT Count(*)
					INTO :li_no_flash
					FROM prod
					where cntr = :Lcntr
					AND assigndt = to_date(:Lassigndt,'MM/DD/YYYY')
					AND prodstage = 'FC'
					USING SqlServerTrans;
					sle_no_flash.text = String(li_no_flash)
				END IF				 
			ELSE
     			Messagebox("ERROR", "Can not modify the select statement: "+mod_string+" rc = "+rc,stopSign!)
			END IF
			wf_set_taborder()
			cb_find.enabled = FALSE
			cb_clear.enabled = TRUE
			cb_update.enabled = TRUE
			cb_listing.enabled = TRUE
			cb_html.enabled = TRUE
			IF lmed = 'BR' OR lmed = 'P/B' THEN
				cb_opf.enabled = FALSE
			ELSE
				cb_opf.enabled = TRUE
			END IF
			cb_labels.enabled = TRUE
			cb_booknav.enabled = TRUE
		ELSEIF rtn=3 THEN
			new_batch=FALSE
			dw_pa_ancntr_data.Reset()
			dw_pa_ancntr_data.InsertRow(0)
			dw_pa_ancntr_data.SetFocus()
			cb_listing.enabled = FALSE
			cb_html.enabled = FALSE
			cb_update.enabled = FALSE
			cb_clear.enabled = FALSE
			cb_find.enabled = TRUE
			cb_booknav.enabled = TRUE
		END IF
	END IF // IF the contract type is not a duplication contract.
ELSE
	new_batch=FALSE
	Messagebox("ERROR", "Contract number: " + Lcntr +" does not exist.",stopSign!)
//	NextStep:
	dw_pa_ancntr_data.Reset()
	sle_no_books.text = ""
	sle_no_flash.text = ""

	// Insert a blank row
	dw_pa_ancntr_data.InsertRow(0)
	dw_pa_ancntr_data.SetFocus()
	
	cb_find.enabled = TRUE
	cb_clear.enabled = FALSE
	cb_update.enabled = FALSE
	cb_listing.enabled = FALSE
	cb_opf.enabled = FALSE
	cb_html.enabled = FALSE
	cb_booknav.enabled = TRUE
END IF
end event

type cb_clear from u_cb within w_pa_assigning_books
integer x = 2331
integer y = 1632
integer width = 233
integer height = 112
integer taborder = 0
string text = "&Clear"
end type

event clicked;call super::clicked;string rc,Lcntrtype
Lcntrtype = dw_pa_ancntr_data.object.cntrtype[1]

new_batch=FALSE
ib_disableclosequery=TRUE

IF Lcntrtype = "D" THEN
	// Reset the duplication datawindow to the original select statement.
	rc = dw_pa_dupl_book_assignment.Modify(original_dupl_tbl_select)
	IF rc <> "" THEN
		MessageBox("ERROR", "****** CAN NOT modify the select statement *******"+rc+" "+original_select,StopSign!)
		RETURN -1
	ELSE
		// Insert a blank row
		dw_pa_dupl_book_assignment.Object.DataWindow.QueryClear = 'Yes'
		dw_pa_dupl_book_assignment.Reset()
		dw_pa_dupl_book_assignment.InsertRow(0)
		
		dw_pa_ancntr_data.Enabled = TRUE
		dw_pa_ancntr_data.Reset()
		dw_pa_ancntr_data.InsertRow(0)
		sle_no_books.text = ""
		
		SetNull(local_assinged_date)
		
	
		dw_pa_ancntr_data.SetFocus()
	
		cb_find.enabled = TRUE
		wf_disable_buttons()
	END IF
ELSEIF Lcntrtype = "A" THEN
	// Reset the datawindow to the original select statement.
	rc = dw_pa_book_assignment_ad.Modify(original_ad_tbl_select)
	IF rc <> "" THEN
		MessageBox("ERROR", "****** CAN NOT modify the select statement *******"+rc+" "+original_ad_select,StopSign!)
	ELSE
		// Insert a blank row
	
		dw_pa_book_assignment_ad.Object.DataWindow.QueryClear = 'Yes'
		dw_pa_book_assignment_ad.Reset()
		dw_pa_book_assignment_ad.InsertRow(0)
		
		dw_pa_ancntr_data.Enabled = TRUE
		dw_pa_ancntr_data.Reset()
		dw_pa_ancntr_data.InsertRow(0)
		sle_no_books.text = ""
		
		SetNull(local_assinged_date)
		
		dw_pa_ancntr_data.SetFocus()
		
		cb_find.enabled = TRUE
		wf_disable_buttons()
	END IF
	
ELSE
	// Reset the datawindow to the original select statement.
	rc = dw_pa_book_assignment.Modify(original_tbl_select)
	IF rc <> "" THEN
		MessageBox("ERROR", "****** CAN NOT modify the select statement *******"+rc+" "+original_select,StopSign!)
	ELSE
		// Insert a blank row
	
		dw_pa_book_assignment.Object.DataWindow.QueryClear = 'Yes'
		dw_pa_book_assignment.Reset()
		dw_pa_book_assignment.InsertRow(0)
		
		dw_pa_ancntr_data.Enabled = TRUE
		dw_pa_ancntr_data.Reset()
		dw_pa_ancntr_data.InsertRow(0)
		sle_no_books.text = ""
		
		SetNull(local_assinged_date)
		
		dw_pa_ancntr_data.SetFocus()
		
		cb_find.enabled = TRUE
		wf_disable_buttons()
	END IF
END IF	

end event

type cb_update from u_cb within w_pa_assigning_books
integer x = 2048
integer y = 1632
integer width = 233
integer height = 112
integer taborder = 0
string text = "&Update"
end type

event clicked;call super::clicked;//////////////////////////////////////////////////////////////////////////////////////////////////////////
//
//	Event: clicked for cb_update
//
//	Description:
//	Update assigning books
//
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//	
//	Revision History
//
//	Developed by 	Date 				Version						Tracking#
//									
// Murali K.			03/31/2008     If prodstage is ZM book media should be DB
// Murali K. 			06/10/2008 		Mastering contract should have RC as book media, 
//										    Set book media appropriately depending on Prod stage
// Murali K.			06/12/2008		Populate audit columns while inserting/updating prod
// Murali K.			11/26/2008 		 BR-> MA SHOULD GO AS BR BOOK MEDIA Tracker 2127
// Murali K.			09/15/2009		Tracker 2108 update DU schedule end date while updating FC end date
// mURALI k.		3/25/2010     UNSUPPORTED APPEON FEATURE 'goto' REMOVED
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//
DwItemStatus l_status
Int i,rowcount,Lapplen,rtn,Lvols,Lqnty,Lmaxstageorder,lcnt
String Lconno,lpriority,lbkmed,lcntrmed,Lprodstage,Lcntr,ls_message,Lcntrcvcd,lstpprdr,lprdr,lprdrname
String ls_msgparm[1],lcntrtype,Lprod_stage[4],Lstage_pu_ma,Lfirst_stage,Lbkno
String Lotherconno,Lflash_ind, ls_user
Double Ldf
Long Lbkseq, dummycnt
Date Lschstdt,Lschenddt,Lassignment_date, ld_created
DateTime ld_today
ld_today = DateTime(Today(), Now())

// 06/12/2008 set audit columns for prod
ld_created = today()
ls_user = gnv_app.of_getuserid()

lcntrmed =	dw_pa_ancntr_data.object.cntrmed[1]

IF lcntrmed = 'P/B' OR lcntrmed = 'BR' THEN
	lbkmed = 'BR'
ELSEIF lcntrmed = 'RTB' OR lcntrmed = 'RC' THEN
	lbkmed = 'RC'
ELSEIF lcntrmed = 'FC' THEN
	lbkmed = 'DB'
ELSE
	lbkmed = lcntrmed
END IF

Lcntr = dw_pa_ancntr_data.object.cntr[1]
lcntrtype = dw_pa_ancntr_data.object.cntrtype[1]

IF dw_pa_book_assignment.visible = TRUE THEN
	wf_set_required_fields()
END IF

// Check for any pending updates

IF of_UpdateChecks() < 0 THEN RETURN -1


Open(w_pics_update_msg_box)


// 03/25/2008 This is a dup only books.  Insert into prod appropriately
IF lcntrtype = "D" THEN
   // Update the book duplication datawindow.
	rowcount = dw_pa_dupl_book_assignment.RowCount()
	FOR i=1 TO rowcount
		Lconno  	= 	dw_pa_dupl_book_assignment.object.mchar_conno[i]
		IF IsNull(Lconno) THEN
			dw_pa_dupl_book_assignment.DeleteRow(i)
			sle_no_books.text = String(dw_pa_dupl_book_assignment.RowCount())
		END IF
	NEXT
	rowcount = dw_pa_dupl_book_assignment.RowCount()
	FOR i=1 TO rowcount
     	IF IsNull(dw_pa_dupl_book_assignment.object.mchar_bkseq[i]) THEN
			Close(w_pics_update_msg_box)
			RETURN -1
		END IF
	NEXT
	FOR i=1 TO rowcount
		Lbkseq 	= 	dw_pa_dupl_book_assignment.object.mchar_bkseq[i]
		lbkmed 	= 	Trim(dw_pa_dupl_book_assignment.object.mchar_bkmed[i])
		Lbkno		=  Trim(lbkmed)+String(Lbkseq)
		Lconno  	= 	dw_pa_dupl_book_assignment.object.mchar_conno[i]
		lpriority = dw_pa_dupl_book_assignment.object.mchar_priority[i]
		Lvols		 = dw_pa_dupl_book_assignment.object.mchar_vols[i]
		Lqnty		 = dw_pa_dupl_book_assignment.object.mchar_qnty[i]
		
		// If the contract medium is Flash Cartridges then the production stage for the duplication is FC otherwise the default is RC
		IF lcntrmed = 'FC' THEN
			Lprodstage = 'FC'
			lbkmed = 'DB'
		ELSE
			Lprodstage = 'DU'
		END IF
		
		Lschstdt = 	Date(dw_pa_dupl_book_assignment.object.prod_schstdt[i])
		Lschenddt = Date(dw_pa_dupl_book_assignment.object.prod_schenddt[i])
		Lassignment_date = Date(dw_pa_dupl_book_assignment.object.prod_assigndt[i])
		lstpprdr = wf_get_sub_contractor(Lcntr, lbkmed, Lprodstage)
		
		IF IsNull(Lconno) THEN
			dw_pa_dupl_book_assignment.DeleteRow(i)
			sle_no_books.text = String(dw_pa_dupl_book_assignment.RowCount())
		ELSE			
			l_status = dw_pa_dupl_book_assignment.GetItemStatus(i,0,primary!)
			CHOOSE CASE l_status
			CASE dataModified!
					// This is an update to these record.
					UPDATE mchar  
					set vols = :Lvols,qnty = :Lqnty,priority = :lpriority
					where mchar.conno = :Lconno
					and mchar.bkmed = :lbkmed
					USING SqlServerTrans;
					IF f_check_dberror(SqlServerTrans,"Updating MCHAR:")=FALSE THEN
						Messagebox("UPDATE ERROR","Failed to update mchar with book number: "+ lbkmed + String(Lbkseq) )
//						IF rtn = 1 THEN
//							GOTO Goto3
//						ELSE
							ROLLBACK USING SqlServerTrans;
							Close(w_pics_update_msg_box)
							RETURN -1
//						END IF
					ELSE
//						Goto3:
						// Update the prod table.
					
						// 03/31/2008 if prodstage is Zmastering set the book media as DB
						IF lprodstage = 'ZM' THEN
							lbkmed = 'DB'
						END IF
						
						UPDATE prod
						set assigndt = :Lassignment_date,schstdt = :Lschstdt,schenddt = :Lschenddt,stpprdr = :lstpprdr, modified_by=:ls_user, modified_date=SYSDATE
						where prod.bkseq = :Lbkseq
						AND prod.bkmed = :lbkmed
						AND prod.prodstage = :Lprodstage
						AND prod.cntr = :Lcntr
						USING SqlServerTrans;
						IF f_check_dberror(SqlServerTrans,"Updating PROD duplication:")=FALSE THEN
							Messagebox("UPDATE ERROR","Failed to update prod with book number: "+ lbkmed + String(Lbkseq) )
	//						IF rtn = 1 THEN
	//							GOTO Goto4
	//						ELSE
								ROLLBACK USING SqlServerTrans;
								Close(w_pics_update_msg_box)
								RETURN -1
	//						END IF
						ELSE
	//						Goto4:
						END IF
					END IF						
			CASE newModified!
					UPDATE mchar  
					set vols = :Lvols,qnty = :Lqnty,priority = :lpriority
					where mchar.conno = :Lconno
					and mchar.bkmed = :lbkmed
					USING SqlServerTrans;
					IF f_check_dberror(SqlServerTrans,"Updating MCHAR..")=FALSE THEN
						Messagebox("UPDATE ERROR","Failed to update mchar with book number: "+ lbkmed + String(Lbkseq) )
		//				IF rtn = 1 THEN
		//					GOTO Goto8
		//				ELSE
							ROLLBACK USING SqlServerTrans;
							Close(w_pics_update_msg_box)
							RETURN -1
			//			END IF
					ELSE
				//		Goto8:
						// Insert into prod table. (RS21n)
						// 03/31/2008 if prodstage is Zmastering set the book media as DB
						IF lprodstage = 'ZM' THEN
							lbkmed = 'DB'
						// 07/02/2008 DU should go in as RC book media
						ELSEIF lprodstage = 'DU' THEN
							lbkmed = 'RC'
						END IF

						INSERT INTO prod  
								(bkseq,bkmed,prodstage,cntr,schstdt,schenddt,actstdt,actenddt, stpprdr,  
								  units,subunits,assigndt,sirevdt,ssdflag,created_by,created_date,modified_by,modified_date)  
						values (:Lbkseq,:lbkmed,:Lprodstage,:Lcntr,:Lschstdt,:Lschenddt,NULL,NULL,:lstpprdr,
								  NULL,NULL,:Lassignment_date,NULL,'I',:ls_user,SYSDATE,:ls_user,SYSDATE)
						USING SqlServerTrans;
						IF f_check_dberror(SqlServerTrans,"Inserting into PROD duplication")=FALSE THEN
							Messagebox("INSERT ERROR","Failed to insert prod with book number: "+ lbkmed + String(Lbkseq) )
				//			IF rtn = 1 THEN
				//				GOTO Goto9
				//			ELSE
								ROLLBACK USING SqlServerTrans;
								Close(w_pics_update_msg_box)
								RETURN -1
					//		END IF
						ELSE
						//	Goto9:
							INSERT INTO DUP_RETURN_BOOKS (bkseq,bkmed,cntr)  
							values (:Lbkseq,:lbkmed,:Lcntr)
							USING SqlServerTrans;
							IF f_check_dberror(SqlServerTrans,"Inserting into DUP_RETURN_BOOKS ")=FALSE THEN
								Messagebox("INSERT ERROR","Failed to insert DUP_RETURN_BOOKS with book number: "+ lbkmed + String(Lbkseq) )
//								IF rtn = 1 THEN
//									GOTO Goto91
//								ELSE
									ROLLBACK USING SqlServerTrans;
									Close(w_pics_update_msg_box)
									RETURN -1
	//							END IF
							ELSE
		//						Goto91:							
							END IF
							dw_pa_dupl_book_assignment.SetItemStatus(i,0,primary!,dataModified!)
						END IF
					END IF
				// Mark the MCHAR Table
				f_update_mchar_time(Lconno,0,"C","U")
				IF f_check_dberror(SqlServerTrans,"Duplication- Updating MCHAR")=FALSE THEN
					Messagebox("UPDATE ERROR","Failed to update mchar with book number: "+ lbkmed + String(Lbkseq) )
			//		IF rtn = 1 THEN
			//			GOTO Goto339
			//		ELSE
						ROLLBACK USING SqlServerTrans;
						Close(w_pics_update_msg_box)
						RETURN -1
				//	END IF
				END IF				
//			Goto339:
			CASE ELSE
				CONTINUE	
			END CHOOSE
		END IF
	NEXT
ELSEIF lcntrtype = "A" THEN
	// Conversion Books
	rowcount = dw_pa_book_assignment_ad.RowCount()
	FOR i=1 TO rowcount
		Lconno  	= 	dw_pa_book_assignment_ad.object.mchar_conno[i]
		Lbkseq 	= 	dw_pa_book_assignment_ad.object.mchar_bkseq[i]
		Lschenddt 	= Date(dw_pa_book_assignment_ad.object.prod_schenddt[i])
		UPDATE prod
			set schenddt = :Lschenddt, modified_by=:ls_user, modified_date=SYSDATE
			where prod.bkseq = :Lbkseq
			AND prod.cntr = :Lcntr
			AND prod.prodstage = 'DT'
		USING SqlServerTrans;
		IF f_check_dberror(SqlServerTrans,"Updating PROD")=FALSE THEN
			Messagebox("UPDATE ERROR","Failed to update prod with book number: "+ lbkmed + String(Lbkseq) )
	//		IF rtn = 1 THEN
	//			GOTO Upd_prod_ad
	//		ELSE
				ROLLBACK USING SqlServerTrans;
				Close(w_pics_update_msg_box)
				RETURN -1
	//		END IF
		END IF
	//	Upd_prod_ad:
	NEXT

ELSE
	// The next section will describe the insertion into prod table, rows that
	// will require more than one production stage to complete their production.
	// Calculate the number of rows for our loop.
	rowcount = dw_pa_book_assignment.RowCount()
	FOR i=1 TO rowcount
		Lconno  	= 	dw_pa_book_assignment.object.mchar_conno[i]
		IF IsNull(Lconno) THEN
			dw_pa_book_assignment.DeleteRow(i)
			sle_no_books.text = String(dw_pa_book_assignment.RowCount())
		END IF
	NEXT
	rowcount = dw_pa_book_assignment.RowCount()
	FOR i=1 TO rowcount
     		IF IsNull(dw_pa_book_assignment.object.mchar_bkseq[i]) THEN
			Close(w_pics_update_msg_box)
			RETURN -1
		END IF
	NEXT
	FOR i=1 TO rowcount
		
		// Reset the variables
		Lfirst_stage=""
		Lprod_stage[1]=""
		Lprod_stage[2]=""
		Lprod_stage[3]=""
		Lprod_stage[4]=""
		
		// Get the row information
		Lconno  	= 	dw_pa_book_assignment.object.mchar_conno[i]
		Lotherconno = 	dw_pa_book_assignment.object.mchar_other_media_conno[i]
		Lflash_ind = dw_pa_book_assignment.object.flash_indicator[i]
		Lbkseq 	= 	dw_pa_book_assignment.object.mchar_bkseq[i]
		Lbkno		=  Trim(lbkmed)+String(Lbkseq)
		Lapplen 	= 	dw_pa_book_assignment.object.mchar_applen[i]
		lpriority = dw_pa_book_assignment.object.mchar_priority[i]
		Ldf 		=	dw_pa_book_assignment.object.mchar_df[i]
	
		Lprodstage 	= dw_pa_book_assignment.object.prod_prodstage[i]
		Lschstdt 	=  Date(dw_pa_book_assignment.object.prod_schstdt[i])
		Lschenddt 	= Date(dw_pa_book_assignment.object.prod_schenddt[i])
		Lassignment_date = Date(dw_pa_book_assignment.object.prod_assigndt[i])
		Lcntrcvcd = dw_pa_ancntr_data.object.cntrcvcd[1]
		
		// If the row does not have a book number, delete it
		IF IsNull(Lbkseq) THEN
			dw_pa_book_assignment.DeleteRow(i)
			sle_no_books.text = String(dw_pa_book_assignment.RowCount())
		ELSE
			// Use Lmaxstageorder to find out how many stages will it takes
			// to complete the production of each of these books.
			SELECT Max(stageorder) INTO :Lmaxstageorder FROM prodstage
				where cntrtype = :lcntrtype
				AND	cntrmed = :lcntrmed
				USING SqlServerTrans;
			// If it take more than one stage.
			IF Lmaxstageorder = 2 THEN
				// If the abstruse check box is checked the first stage is AB.
				SELECT prodstage 
				INTO :Lprod_stage[1]
				FROM prodstage
					where cntrtype = :lcntrtype
					AND   cntrmed = :lcntrmed
					AND	(alternate <> 'Y' OR alternate IS NULL)
					AND 	stageorder = 1
				USING SqlServerTrans;
				IF f_check_dberror(SqlServerTrans,"Selecting from PRODSTAGE 1:")=FALSE THEN
					ROLLBACK USING SqlServerTrans;
					Close(w_pics_update_msg_box)
					RETURN -1
				END IF
				SELECT prodstage
				INTO :Lprod_stage[2]
				FROM prodstage
					where cntrtype = :lcntrtype
					AND   cntrmed = :lcntrmed
					AND	(alternate <> 'Y' OR alternate IS NULL)
					AND 	stageorder = :Lmaxstageorder
				USING SqlServerTrans;
				IF f_check_dberror(SqlServerTrans,"Selecting from PRODSTAGE 2:")=FALSE THEN
					ROLLBACK USING SqlServerTrans;
					Close(w_pics_update_msg_box)
					RETURN -1
				END IF
			//RTB books (MA, ZM, and DU)
			ELSEIF Lmaxstageorder = 3 THEN
				SELECT prodstage 
				INTO :Lprod_stage[1] 
				FROM prodstage
					where cntrtype = :lcntrtype
					AND   cntrmed = :lcntrmed
					AND	(alternate <> 'Y' OR alternate IS NULL)
					AND 	stageorder = 1
				USING SqlServerTrans;
				IF f_check_dberror(SqlServerTrans,"RTB- Selecting from PRODSTAGE 1:")=FALSE THEN
					ROLLBACK USING SqlServerTrans;
					Close(w_pics_update_msg_box)
					RETURN -1
				END IF
				SELECT prodstage
				INTO :Lprod_stage[2]
				FROM prodstage
					where cntrtype = :lcntrtype
					AND   cntrmed = :lcntrmed
					AND	(alternate <> 'Y' OR alternate IS NULL)
					AND 	stageorder = 2
				USING SqlServerTrans;
				IF f_check_dberror(SqlServerTrans,"RTB- Selecting from PRODSTAGE 2:")=FALSE THEN
					ROLLBACK USING SqlServerTrans;
					Close(w_pics_update_msg_box)
					RETURN -1
				END IF
				SELECT prodstage 
				INTO :Lprod_stage[3]
				FROM prodstage
					where cntrtype = :lcntrtype
					AND   cntrmed = :lcntrmed
					AND	(alternate <> 'Y' OR alternate IS NULL)
					AND 	stageorder = 3
				USING SqlServerTrans;
				IF f_check_dberror(SqlServerTrans,"RTB- Selecting from PRODSTAGE 3:")=FALSE THEN
					ROLLBACK USING SqlServerTrans;
					Close(w_pics_update_msg_box)
					RETURN -1
				END IF
			//RTB books (MA, ZM, DU, and FC)
			ELSEIF Lmaxstageorder = 4 THEN
				SELECT prodstage 
				INTO :Lprod_stage[1]
				FROM prodstage
					where cntrtype = :lcntrtype
					AND   cntrmed = :lcntrmed
					AND	(alternate <> 'Y' OR alternate IS NULL)
					AND 	stageorder = 1
				USING SqlServerTrans;
				IF f_check_dberror(SqlServerTrans,"RTB- Selecting from PRODSTAGE 1:")=FALSE THEN
					ROLLBACK USING SqlServerTrans;
					Close(w_pics_update_msg_box)
					RETURN -1
				END IF
				SELECT prodstage
				INTO :Lprod_stage[2]
				FROM prodstage
					where cntrtype = :lcntrtype
					AND   cntrmed = :lcntrmed
					AND	(alternate <> 'Y' OR alternate IS NULL)
					AND 	stageorder = 2
				USING SqlServerTrans;
				IF f_check_dberror(SqlServerTrans,"RTB- Selecting from PRODSTAGE 2:")=FALSE THEN
					ROLLBACK USING SqlServerTrans;
					Close(w_pics_update_msg_box)
					RETURN -1
				END IF
				SELECT prodstage 
				INTO :Lprod_stage[3] 
				FROM prodstage
					where cntrtype = :lcntrtype
					AND   cntrmed = :lcntrmed
					AND	(alternate <> 'Y' OR alternate IS NULL)
					AND 	stageorder = 3
				USING SqlServerTrans;
				IF f_check_dberror(SqlServerTrans,"RTB- Selecting from PRODSTAGE 3:")=FALSE THEN
					ROLLBACK USING SqlServerTrans;
					Close(w_pics_update_msg_box)
					RETURN -1
				END IF
				SELECT prodstage
				INTO :Lprod_stage[4] 
				FROM prodstage
					where cntrtype = :lcntrtype
					AND   cntrmed = :lcntrmed
					AND	(alternate <> 'Y' OR alternate IS NULL)
					AND 	stageorder = 4
				USING SqlServerTrans;
				IF f_check_dberror(SqlServerTrans,"RTB- Selecting from PRODSTAGE 4:")=FALSE THEN
					ROLLBACK USING SqlServerTrans;
					Close(w_pics_update_msg_box)
					RETURN -1
				END IF
			ELSE
				// It takes only one stage to complete this book.
				SELECT prodstage 
				INTO :Lprod_stage[1] 
				FROM prodstage
					where cntrtype = :lcntrtype
					AND   cntrmed = :lcntrmed
					AND	(alternate <> 'Y' OR alternate IS NULL)
					AND 	stageorder = 1
				USING SqlServerTrans;
				IF f_check_dberror(SqlServerTrans,"one stage- Selecting from PRODSTAGE:")=FALSE THEN
					ROLLBACK USING SqlServerTrans;
					Close(w_pics_update_msg_box)
					RETURN -1
				END IF
			END IF
			// Is the book is abstruse then first production stage is AB
			IF Lprodstage="AB" THEN 
				Lfirst_stage="AB"
			ELSE
				Lfirst_stage=Lprod_stage[1]
			END IF
			
			// Get the status of the row
			l_status = dw_pa_book_assignment.GetItemStatus(i,0,primary!)
			
			CHOOSE CASE l_status
			CASE dataModified!
			// If more than one stage involved for production of this book. (Two stages Narration and duplication)
			IF Lmaxstageorder = 2 THEN
				// Get the sub contractor
				lstpprdr = wf_get_sub_contractor(Lcntr, lbkmed, Lprod_stage[1])
				// The following 'if' statement was added here to not allow ZM stage to be updated with schenddt
				// for narration contract type of media RTB. Instead the MA stage needs to be updated with schenddt
				IF lcntrtype='M' AND lcntrmed='RTB' THEN
					Lprod_stage[2]='MA'
				END IF
				UPDATE prod
					set schstdt = :Lschstdt,assigndt = :Lassignment_date,prodstage = :Lfirst_stage, stpprdr = :lstpprdr, modified_by=:ls_user, modified_date=SYSDATE
					where prod.bkseq = :Lbkseq
					AND prod.bkmed = :lbkmed
					AND prod.cntr = :Lcntr
					AND prod.prodstage = :Lprod_stage[1]
					USING SqlServerTrans;
				IF f_check_dberror(SqlServerTrans,"Updating PROD")=FALSE THEN
					Messagebox("UPDATE ERROR","Failed to update prod with book number: "+ lbkmed + String(Lbkseq) )
		//			IF rtn = 1 THEN
		//				GOTO Goto13
		//			ELSE
						ROLLBACK USING SqlServerTrans;
						Close(w_pics_update_msg_box)
						RETURN -1
			//		END IF
				ELSE
			//		Goto13:
					// Get the sub contractor
					lstpprdr = wf_get_sub_contractor(Lcntr, lbkmed, Lprod_stage[2])
									
					UPDATE prod
						set schenddt = :Lschenddt, stpprdr = :lstpprdr, modified_by=:ls_user, modified_date=SYSDATE
						where prod.bkseq = :Lbkseq
						AND prod.bkmed = :lbkmed
						AND prod.cntr = :Lcntr
						AND prod.prodstage = :Lprod_stage[2]
						USING SqlServerTrans;
					IF f_check_dberror(SqlServerTrans,"Updating PROD.")=FALSE THEN
						Messagebox("UPDATE ERROR","Failed to update prod with book number: "+ lbkmed + String(Lbkseq) )
		//				IF rtn = 1 THEN
		//					GOTO Goto14
		//				ELSE
							ROLLBACK USING SqlServerTrans;
							Close(w_pics_update_msg_box)
							RETURN -1
			//			END IF
					END IF	
				//	Goto14:
				END IF
			// RTB books (MA, ZM and DU)
			ELSEIF Lmaxstageorder = 3 THEN
				// Get the sub contractor
				lstpprdr = wf_get_sub_contractor(Lcntr, lbkmed, Lprod_stage[1])
					
				UPDATE prod
					set schstdt = :Lschstdt,assigndt = :Lassignment_date,prodstage = :Lfirst_stage, stpprdr = :lstpprdr, modified_by=:ls_user, modified_date=SYSDATE
					where prod.bkseq = :Lbkseq
					AND prod.bkmed = :lbkmed
					AND prod.cntr = :Lcntr
					AND prod.prodstage = :Lprod_stage[1]
					USING SqlServerTrans;
				IF f_check_dberror(SqlServerTrans,"RTB- Updating PROD")=FALSE THEN
					Messagebox("UPDATE ERROR","Failed to update prod with book number: "+ lbkmed + String(Lbkseq) )
				//	IF rtn = 1 THEN
				//		GOTO Goto18
				//	ELSE
						ROLLBACK USING SqlServerTrans;
						Close(w_pics_update_msg_box)
						RETURN -1
				//	END IF
				ELSE
				//	Goto18:
					// Get the sub contractor
					lstpprdr = wf_get_sub_contractor(Lcntr, lbkmed, Lprod_stage[2])
									
					// Production stage 3 is the duplication stage in RTB
					UPDATE prod
						set schenddt = :Lschenddt, stpprdr = :lstpprdr, modified_by=:ls_user, modified_date=SYSDATE
						where prod.bkseq = :Lbkseq
						AND prod.bkmed = :lbkmed
						AND prod.cntr = :Lcntr
						AND prod.prodstage = :Lprod_stage[3]
						USING SqlServerTrans;
					IF f_check_dberror(SqlServerTrans,"RTB- Updating PROD.")=FALSE THEN
						Messagebox("UPDATE ERROR","Failed to update prod with book number: "+ lbkmed + String(Lbkseq) )
				//		IF rtn = 1 THEN
				//			GOTO Goto19
				//		ELSE
							ROLLBACK USING SqlServerTrans;
							Close(w_pics_update_msg_box)
							RETURN -1
				//		END IF
					END IF	
				//	Goto19:
				END IF
			// RTB books (MA, ZM , DU and FC)
			ELSEIF Lmaxstageorder = 4 THEN
				// Get the sub contractor
				lstpprdr = wf_get_sub_contractor(Lcntr, lbkmed, Lprod_stage[1])
					
				UPDATE prod
				set schstdt = :Lschstdt,assigndt = :Lassignment_date,prodstage = :Lfirst_stage, stpprdr = :lstpprdr, modified_by=:ls_user, modified_date=SYSDATE
				where prod.bkseq = :Lbkseq
				AND prod.bkmed = :lbkmed
				AND prod.cntr = :Lcntr
				AND prod.prodstage = :Lprod_stage[1]
				USING SqlServerTrans;
				IF f_check_dberror(SqlServerTrans,"RTB- Updating PROD")=FALSE THEN
					Messagebox("UPDATE ERROR","Failed to update prod with book number: "+ lbkmed + String(Lbkseq) )
				//	IF rtn = 1 THEN
				//		GOTO Goto181
				//	ELSE
						ROLLBACK USING SqlServerTrans;
						Close(w_pics_update_msg_box)
						RETURN -1
				//	END IF
				ELSE
				//	Goto181:
					// Get the sub contractor
					lstpprdr = wf_get_sub_contractor(Lcntr, lbkmed, Lprod_stage[4])
									
					// Production stage 4 is the flash cartridge stage in RTB
					IF Lflash_ind = 'Y' THEN
						UPDATE prod
							set schenddt = :Lschenddt, stpprdr = :lstpprdr, modified_by=:ls_user, modified_date=SYSDATE
							where prod.bkseq = :Lbkseq
							AND prod.bkmed = 'DB'
							AND prod.cntr = :Lcntr
							AND prod.prodstage = :Lprod_stage[4]
							USING SqlServerTrans;
							
							// 09/15/2009 Tracker 2108 update schedule end date for DU stage record also along with the FC stage.
							UPDATE prod
							set schenddt = :Lschenddt, stpprdr = :lstpprdr, modified_by=:ls_user, modified_date=SYSDATE
							where prod.bkseq = :Lbkseq
							AND prod.bkmed = :lbkmed
							AND prod.cntr = :Lcntr
							AND prod.prodstage = :Lprod_stage[3]
							USING SqlServerTrans;					
							
					ELSEIF Lflash_ind = 'N' THEN
						UPDATE prod
							set schenddt = :Lschenddt, stpprdr = :lstpprdr, modified_by=:ls_user, modified_date=SYSDATE
							where prod.bkseq = :Lbkseq
							AND prod.bkmed = :lbkmed
							AND prod.cntr = :Lcntr
							AND prod.prodstage = :Lprod_stage[3]
							USING SqlServerTrans;					
					END IF
					IF f_check_dberror(SqlServerTrans,"RTB- Updating PROD.")=FALSE THEN
						Messagebox("UPDATE ERROR","Failed to update prod with book number: "+ lbkmed + String(Lbkseq) )
				//		IF rtn = 1 THEN
				//			GOTO Goto191
				//		ELSE
							ROLLBACK USING SqlServerTrans;
							Close(w_pics_update_msg_box)
							RETURN -1
					//	END IF
					//Goto191:					
					END IF
				END IF
			ELSE
				// Otherwise this is just and update to a contract with only one production stage.
				lstpprdr = wf_get_sub_contractor(Lcntr, lbkmed, Lprod_stage[1])
						
				UPDATE prod
					set schstdt = :Lschstdt,schenddt = :Lschenddt,assigndt = :Lassignment_date, stpprdr = :lstpprdr, modified_by=:ls_user, modified_date=SYSDATE
					where prod.bkseq = :Lbkseq
					AND prod.bkmed = :lbkmed
					AND prod.cntr = :Lcntr
					AND prod.prodstage = :Lprod_stage[1]
					USING SqlServerTrans;
				IF f_check_dberror(SqlServerTrans,"One stage - Updating PROD")=FALSE THEN
					Messagebox("UPDATE ERROR","Failed to update prod with book number: "+ lbkmed + String(Lbkseq) )
				//	IF rtn = 1 THEN
				//		GOTO Goto22
				//	ELSE
						ROLLBACK USING SqlServerTrans;
						Close(w_pics_update_msg_box)
						RETURN -1
				//	END IF
				END IF
			//	Goto22:
			END IF
			// If this is an update to existing record and the book has already gone through copy allotment and cabdt is assigned.
			IF wf_is_book_selected(Lconno) THEN
				 UPDATE mchar  
					set df = :Ldf,applen = :Lapplen,priority = :lpriority
					where mchar.conno = :Lconno
					OR mchar.conno = :Lotherconno
					USING SqlServerTrans;
			ELSE
				// If the book has not yet gone through copy allotment and cabdt is not assigned.
				// IF the media flag is FCRC, RC, FC or NULL
				UPDATE mchar  
				  set df = :Ldf,applen = :Lapplen,priority = :lpriority,  cascd = 'N'
				  where mchar.conno = :Lconno
				  OR mchar.conno = :Lotherconno
				  USING SqlServerTrans;
			END IF
			// Mark the MCHAR Table
			f_update_mchar_time(Lconno,0,"C","U")
			
			IF f_check_dberror(SqlServerTrans,"Updating MCHAR")=FALSE THEN
				Messagebox("UPDATE ERROR","Failed to update mchar with book number: "+ lbkmed + String(Lbkseq) )
			//	IF rtn = 1 THEN
			//		GOTO Goto23
			//	ELSE
					ROLLBACK USING SqlServerTrans;
					Close(w_pics_update_msg_box)
					RETURN -1
			//	END IF
			END IF
		//	Goto23:
			// If the rows are need to be inserted into prod table.
			CASE newModified!
			  
			IF Lmaxstageorder = 2 THEN
				   
				// Get the sub contractors
				lstpprdr = wf_get_sub_contractor(Lcntr, lbkmed, Lfirst_stage)
				// If narration only contract
				IF lcntrtype='M' AND lcntrmed='RTB' THEN
					// 03/31/2008 if prodstage is Zmastering set the book media as DB
						IF Lprod_stage[2] = 'ZM' THEN
							lbkmed = 'DB'
						// 07/02/2008 DU should go in as RC book media
						ELSEIF Lprod_stage[2] = 'DU' THEN
							lbkmed = 'RC'
						END IF

					INSERT INTO prod  
						(bkseq,bkmed,prodstage,cntr,schstdt,schenddt,actstdt,actenddt,stpprdr,
						  units,subunits,assigndt,sirevdt,ssdflag,created_by,created_date,modified_by,modified_date)  
					values (:Lbkseq,:lbkmed,:Lprod_stage[2],:Lcntr,NULL,NULL,NULL,NULL,:lstpprdr,
						NULL,NULL,:Lassignment_date,NULL,'I',:ls_user,SYSDATE,:ls_user,SYSDATE)
					USING SqlServerTrans;
				ELSE
					// 03/31/2008 if prodstage is Zmastering set the book media as DB
					IF Lprod_stage[2] = 'ZM' THEN
						lbkmed = 'DB'
					// 07/02/2008 DU should go in as RC book media
					ELSEIF Lprod_stage[2] = 'DU' THEN
							lbkmed = 'RC'
					END IF
					
					INSERT INTO prod  
						(bkseq,bkmed,prodstage,cntr,schstdt,schenddt,actstdt,actenddt,stpprdr,
						  units,subunits,assigndt,sirevdt,ssdflag,created_by,created_date,modified_by,modified_date)  
					values (:Lbkseq,:lbkmed,:Lprod_stage[2],:Lcntr,NULL,:Lschenddt,NULL,NULL,:lstpprdr,
						NULL,NULL,:Lassignment_date,NULL,'I',:ls_user,SYSDATE,:ls_user,SYSDATE)
					USING SqlServerTrans;
				END IF								
				IF f_check_dberror(SqlServerTrans,"Inserting into PROD second stage")=FALSE THEN
					Messagebox("INSERT ERROR","Failed to insert prod with book number: "+ lbkmed + String(Lbkseq) )
		//			IF rtn = 1 THEN
		//				GOTO Goto26
		//			ELSE
						ROLLBACK USING SqlServerTrans;
						Close(w_pics_update_msg_box)
						RETURN -1
			//		END IF
				ELSE
			//	Goto26:
				dw_pa_book_assignment.SetItemStatus(i,0,primary!,dataModified!)
				
					 // 06/10/2008 mastering contract should have RC as book media, Set book media appropriately depending on Prod stage
					 CHOOSE CASE Lfirst_stage
						CASE 'MA','AB', 'DU'
							// 11/26/2008 TRACKER BR-> MA SHOULD GO AS BR BOOK MEDIA
							IF lcntrmed='RTB'  THEN
								lbkmed='RC'
							END IF
						CASE 'ZM','FC'
							lbkmed='DB'
					 END CHOOSE
					 
					 ///////////////////////////////////////////////////////// 06/10/2008				

				// If narration only contract update schenddt and schstdt
				IF lcntrtype='M' AND lcntrmed='RTB' THEN
					
					// 06/10/2008
					long ll_count
					select count(*)
					into :ll_count
					from prod
					where bkseq = :lbkseq and prodstage = 'MA' using sqlservertrans ;
					
					IF ll_count > 0 THEN
						Messagebox('Error', 'Mastering already done for this book, Duplicate Mastering stage will not be allowed.')
						RETURN -1
					END IF
					////////////////////
					
					INSERT INTO prod  
						(bkseq,bkmed,prodstage,cntr,schstdt,schenddt,actstdt,actenddt,stpprdr,   
						  units,subunits,assigndt,sirevdt,ssdflag,created_by,created_date,modified_by,modified_date)  
					values (:Lbkseq,:lbkmed,:Lfirst_stage,:Lcntr,:Lschstdt,:Lschenddt,NULL,NULL,:lstpprdr,
						  NULL,NULL,:Lassignment_date,NULL,'I',:ls_user,SYSDATE,:ls_user,SYSDATE)
					USING SqlServerTrans;
				ELSE
					INSERT INTO prod  
						(bkseq,bkmed,prodstage,cntr,schstdt,schenddt,actstdt,actenddt,stpprdr,   
						  units,subunits,assigndt,sirevdt,ssdflag,created_by,created_date,modified_by,modified_date)  
					values (:Lbkseq,:lbkmed,:Lfirst_stage,:Lcntr,:Lschstdt,NULL,NULL,NULL,:lstpprdr,
						  NULL,NULL,:Lassignment_date,NULL,'I',:ls_user,SYSDATE,:ls_user,SYSDATE)
					USING SqlServerTrans;
				END IF										
				IF f_check_dberror(SqlServerTrans,"Inserting into PROD first stage")=FALSE THEN
					Messagebox("INSERT ERROR","Failed to insert prdrbk with book number: "+ lbkmed + String(Lbkseq) )
			//		IF rtn = 1 THEN
			//			GOTO Goto28
			//		ELSE
						ROLLBACK USING SqlServerTrans;
						Close(w_pics_update_msg_box)
						RETURN -1
			//		END IF
				ELSE
			//		Goto28:
					dw_pa_book_assignment.SetItemStatus(i,0,primary!,dataModified!)
				END IF
			END IF				
		//RTB books (MA, ZM and DU) production stages
		ELSEIF Lmaxstageorder = 3 THEN
		   
			// Get the sub contractors
			lstpprdr = wf_get_sub_contractor(Lcntr, lbkmed, Lfirst_stage)
			// 03/31/2008 if prodstage is Zmastering set the book media as DB
			IF Lprod_stage[3] = 'ZM' THEN
				lbkmed = 'DB'
			// 07/02/2008 DU should go in as RC book media
			ELSEIF Lprod_stage[3] = 'DU' THEN
				lbkmed = 'RC'
			END IF		
			
			INSERT INTO prod  
				(bkseq,bkmed,prodstage,cntr,schstdt,schenddt,actstdt,actenddt,stpprdr,
				  units,subunits,assigndt,sirevdt,ssdflag,created_by,created_date,modified_by,modified_date)  
			values (:Lbkseq,:lbkmed,:Lprod_stage[3],:Lcntr,NULL,:Lschenddt,NULL,NULL,:lstpprdr,
				NULL,NULL,:Lassignment_date,NULL,'I',:ls_user,SYSDATE,:ls_user,SYSDATE)
			USING SqlServerTrans;
			IF f_check_dberror(SqlServerTrans,"RTB- Inserting into PROD duplication")=FALSE THEN
				Messagebox("INSERT ERROR","Failed to insert prod with book number: "+ lbkmed + String(Lbkseq) )
			//	IF rtn = 1 THEN
			//		GOTO Goto32
			//	ELSE
					ROLLBACK USING SqlServerTrans;
					Close(w_pics_update_msg_box)
					RETURN -1
			//	END IF
			ELSE
		//		Goto32:
				dw_pa_book_assignment.SetItemStatus(i,0,primary!,dataModified!)
				
				// 03/31/2008 if prodstage is Zmastering set the book media as DB
				IF Lfirst_stage = 'ZM' THEN
					lbkmed = 'DB'
				// 07/02/2008 DU should go in as RC book media
					ELSEIF Lfirst_stage = 'DU' THEN
						lbkmed = 'RC'
				END IF
				
				INSERT INTO prod  
					(bkseq,bkmed,prodstage,cntr,schstdt,schenddt,actstdt,actenddt,stpprdr,   
					  units,subunits,assigndt,sirevdt,ssdflag,created_by,created_date,modified_by,modified_date)  
				values (:Lbkseq,:lbkmed,:Lfirst_stage,:Lcntr,:Lschstdt,NULL,NULL,NULL,:lstpprdr,
					  NULL,NULL,:Lassignment_date,NULL,'I',:ls_user,SYSDATE,:ls_user,SYSDATE)
				USING SqlServerTrans;
				IF f_check_dberror(SqlServerTrans,"RTB- Inserting into PROD mastering:")=FALSE THEN
					Messagebox("INSERT ERROR","Failed to insert prod with book number: "+ lbkmed + String(Lbkseq) )
		//			IF rtn = 1 THEN
		//				GOTO Goto34
		//			ELSE
						ROLLBACK USING SqlServerTrans;
						Close(w_pics_update_msg_box)
						RETURN -1
		//			END IF
				ELSE
		//			Goto34:
					
						IF Lprod_stage[2]= 'ZM' THEN
							lbkmed = 'DB'
						// 07/02/2008 DU should go in as RC book media
						ELSEIF Lprod_stage[2] = 'DU' THEN
							lbkmed = 'RC'
						END IF
				// bkmed as 'DB' hard-coded removed 07/02/2008
					INSERT INTO prod  
						(bkseq,bkmed,prodstage,cntr,schstdt,schenddt,actstdt,actenddt,stpprdr,   
						  units,subunits,assigndt,sirevdt,ssdflag,created_by,created_date,modified_by,modified_date)  
					values (:Lbkseq,'DB',:Lprod_stage[2],:Lcntr,NULL,NULL,NULL,NULL,:lstpprdr,
						  NULL,NULL,:Lassignment_date,NULL,'I',:ls_user,SYSDATE,'goto34',SYSDATE)
					USING SqlServerTrans;
					IF f_check_dberror(SqlServerTrans,"RTB- Inserting into PROD Z mastering:")=FALSE THEN
						Messagebox("INSERT ERROR","Failed to insert prod with book number: "+ lbkmed + String(Lbkseq) )
			//			IF rtn = 1 THEN
			//				GOTO Goto35
			//			ELSE
							ROLLBACK USING SqlServerTrans;
							Close(w_pics_update_msg_box)
							RETURN -1
				//		END IF
					ELSE
				//		Goto35:
						dw_pa_book_assignment.SetItemStatus(i,0,primary!,dataModified!)
					END IF
				END IF							
			END IF				
		//RTB books (MA, ZM, DU and FC) production stages
		ELSEIF Lmaxstageorder = 4 THEN
		   
			// Get the sub contractors
			lstpprdr = wf_get_sub_contractor(Lcntr, lbkmed, Lfirst_stage)
					
			IF Lflash_ind = 'Y' THEN
				// 07/02/2008
				IF Lprod_stage[4] = 'ZM' THEN
					lbkmed = 'DB'
				// 07/02/2008 DU should go in as RC book media
				ELSEIF Lprod_stage[4] = 'DU' THEN
					lbkmed = 'RC'
				END IF
				// hard coded bkmed removed 'DB'
				INSERT INTO prod  
					(bkseq,bkmed,prodstage,cntr,schstdt,schenddt,actstdt,actenddt,stpprdr,
					  units,subunits,assigndt,sirevdt,ssdflag,created_by,created_date,modified_by,modified_date)  
				values (:Lbkseq,'DB',:Lprod_stage[4],:Lcntr,NULL,:Lschenddt,NULL,NULL,:lstpprdr,
					NULL,NULL,:Lassignment_date,NULL,'I',:ls_user,SYSDATE,'flashind',SYSDATE)
				USING SqlServerTrans;
			ELSE
				// 03/31/2008 if prodstage is Zmastering set the book media as DB
				IF Lprod_stage[3] = 'ZM' THEN
					lbkmed = 'DB'
				// 07/02/2008 DU should go in as RC book media
				ELSEIF Lprod_stage[3] = 'DU' THEN
					lbkmed = 'RC'
				END IF
				
				INSERT INTO prod  
					(bkseq,bkmed,prodstage,cntr,schstdt,schenddt,actstdt,actenddt,stpprdr,
					  units,subunits,assigndt,sirevdt,ssdflag,created_by,created_date,modified_by,modified_date)  
				values (:Lbkseq,:lbkmed,:Lprod_stage[3],:Lcntr,NULL,:Lschenddt,NULL,NULL,:lstpprdr,
					NULL,NULL,:Lassignment_date,NULL,'I',:ls_user,SYSDATE,:ls_user,SYSDATE)
				USING SqlServerTrans;
			END IF // Lflash_ind is Y
			IF f_check_dberror(SqlServerTrans,"RTB- Inserting into PROD Flash")=FALSE THEN
				Messagebox("INSERT ERROR","Failed to insert prod with book number: "+ lbkmed + String(Lbkseq) )
			//	IF rtn = 1 THEN
			//		GOTO Goto321
			//	ELSE
					ROLLBACK USING SqlServerTrans;
					Close(w_pics_update_msg_box)
					RETURN -1
			//	END IF
			ELSE
			//	Goto321:
				dw_pa_book_assignment.SetItemStatus(i,0,primary!,dataModified!)
				
				// 03/31/2008 if prodstage is Zmastering set the book media as DB
				IF Lfirst_stage = 'ZM' THEN
					lbkmed = 'DB'
				// 07/02/2008 DU should go in as RC book media
					ELSEIF Lfirst_stage = 'DU' THEN
						lbkmed = 'RC'
				END IF
		
				INSERT INTO prod  
					(bkseq,bkmed,prodstage,cntr,schstdt,schenddt,actstdt,actenddt,stpprdr,   
					  units,subunits,assigndt,sirevdt,ssdflag,created_by,created_date,modified_by,modified_date)  
				values (:Lbkseq,:lbkmed,:Lfirst_stage,:Lcntr,:Lschstdt,NULL,NULL,NULL,:lstpprdr,
					  NULL,NULL,:Lassignment_date,NULL,'I',:ls_user,SYSDATE,:ls_user,SYSDATE)
				USING SqlServerTrans;					
				IF f_check_dberror(SqlServerTrans,"RTB- Inserting into PROD mastering:")=FALSE THEN
					Messagebox("INSERT ERROR","Failed to insert prod with book number: "+ lbkmed + String(Lbkseq) )
			//		IF rtn = 1 THEN
			//			GOTO Goto341
			//		ELSE
						ROLLBACK USING SqlServerTrans;
						Close(w_pics_update_msg_box)
						RETURN -1
			//		END IF
				ELSE
			//		Goto341:
					IF Lprod_stage[2] = 'ZM' THEN
						INSERT INTO prod  
							(bkseq,bkmed,prodstage,cntr,schstdt,schenddt,actstdt,actenddt,stpprdr,   
							  units,subunits,assigndt,sirevdt,ssdflag,created_by,created_date,modified_by,modified_date)  
						values (:Lbkseq,'DB',:Lprod_stage[2],:Lcntr,NULL,NULL,NULL,NULL,:lstpprdr,
							  NULL,NULL,:Lassignment_date,NULL,'I',:ls_user,SYSDATE,:ls_user,SYSDATE)
						USING SqlServerTrans;
					ELSE
							// 07/02/2008 DU stage should go in as 'RC' book media
							IF Lprod_stage[2] = 'DU' THEN
								lbkmed = 'RC'
							END IF
							
						INSERT INTO prod  
							(bkseq,bkmed,prodstage,cntr,schstdt,schenddt,actstdt,actenddt,stpprdr,   
							  units,subunits,assigndt,sirevdt,ssdflag,created_by,created_date,modified_by,modified_date)  
						values (:Lbkseq,:lbkmed,:Lprod_stage[2],:Lcntr,NULL,NULL,NULL,NULL,:lstpprdr,
							  NULL,NULL,:Lassignment_date,NULL,'I',:ls_user,SYSDATE,:ls_user,SYSDATE)
						USING SqlServerTrans;						
					END IF
					IF f_check_dberror(SqlServerTrans,"RTB- Inserting into PROD Z mastering:")=FALSE THEN
						Messagebox("INSERT ERROR","Failed to insert prod with book number: "+ lbkmed + String(Lbkseq) )
			//			IF rtn = 1 THEN
			//				GOTO Goto351
			//			ELSE
							ROLLBACK USING SqlServerTrans;
							Close(w_pics_update_msg_box)
							RETURN -1
			//			END IF
					ELSE
			//			Goto351:
						IF Lflash_ind = 'Y' THEN
							// 07/02/2008 DU stage should go in as 'RC' book media
							IF Lprod_stage[3] = 'DU' THEN
								lbkmed = 'RC'
							ELSE
								lbkmed='DB'
							END IF
							
							// 03/31/2008 for flash DB is the media
							INSERT INTO prod  
								(bkseq,bkmed,prodstage,cntr,schstdt,schenddt,actstdt,actenddt,stpprdr,
								  units,subunits,assigndt,sirevdt,ssdflag,created_by,created_date,modified_by,modified_date)  
								values (:Lbkseq,:lbkmed,:Lprod_stage[3],:Lcntr,NULL,:Lschenddt,NULL,NULL,:lstpprdr,
								NULL,NULL,:Lassignment_date,NULL,'I',:ls_user,SYSDATE,:ls_user,SYSDATE)
							USING SqlServerTrans;						
							IF f_check_dberror(SqlServerTrans,"RTB- Inserting into PROD duplication")=FALSE THEN
								Messagebox("INSERT ERROR","Failed to insert prod with book number: "+ lbkmed + String(Lbkseq) )
			//					IF rtn = 1 THEN
			//						GOTO Goto361
			//					ELSE
									ROLLBACK USING SqlServerTrans;
									Close(w_pics_update_msg_box)
									RETURN -1
			//					END IF
							END IF
						ELSE
			//				Goto361:
							dw_pa_book_assignment.SetItemStatus(i,0,primary!,dataModified!)
						END IF // Flash indicator is set to Y
					END IF // DB error
				END IF // DB error
			END IF // Max stage orders
		ELSE
			// This book has only one production stage.
			// First see if the subcontractors exist.
			lstpprdr = wf_get_sub_contractor(Lcntr, lbkmed, Lfirst_stage)
				// 03/31/2008 if prodstage is Zmastering set the book media as DB
				IF Lfirst_stage = 'ZM' THEN
					lbkmed = 'DB'
				ELSEIF Lfirst_stage = 'DU' THEN
					lbkmed = 'RC'					
				END IF
		

			INSERT INTO prod  
				(bkseq,bkmed,prodstage,cntr,schstdt,schenddt,actstdt,actenddt,stpprdr,
				  units,subunits,assigndt,sirevdt,ssdflag,created_by,created_date,modified_by,modified_date)  
			values (:Lbkseq,:lbkmed,:Lfirst_stage,:Lcntr,:Lschstdt,:Lschenddt,NULL,NULL,:lstpprdr,
				NULL,NULL,:Lassignment_date,NULL,'I',:ls_user,SYSDATE,:ls_user,SYSDATE)
			USING SqlServerTrans;
			IF f_check_dberror(SqlServerTrans,"one stage- Inserting into PROD")=FALSE THEN
				Messagebox("INSERT ERROR","Failed to insert prod with book number: "+ lbkmed + String(Lbkseq) )
			//	IF rtn = 1 THEN
			//		GOTO Goto38
			//	ELSE
					ROLLBACK USING SqlServerTrans;
					Close(w_pics_update_msg_box)
					RETURN -1
			//	END IF
			ELSE
			//	Goto38:
				dw_pa_book_assignment.SetItemStatus(i,0,primary!,dataModified!)
			END IF								
		END IF // Stage orders
		// If this is an update to existing record and the book has already gone through copy allotment and cabdt is assigned.
	  	IF wf_is_book_selected(Lconno) THEN
			UPDATE mchar  
			  set df = :Ldf,applen = :Lapplen,priority = :lpriority
			  where mchar.conno = :Lconno
			  OR mchar.conno = :Lotherconno
			  USING SqlServerTrans;
		ELSE
			// If the book has not yet gone through copy allotment and cabdt is not assigned.
			// IF the media flag is FCRC, RC, FC or NULL
			UPDATE mchar  
			  set bkseq = :Lbkseq, df = :Ldf,applen = :Lapplen,priority = :lpriority,  cascd = 'N'
			  where mchar.conno = :Lconno
			  OR mchar.conno = :Lotherconno
			USING SqlServerTrans;
		END IF
		// Mark the MCHAR Table
		f_update_mchar_time(Lconno,0,"C","U")
	  	IF f_check_dberror(SqlServerTrans,"one stage- Updating MCHAR")=FALSE THEN
			Messagebox("UPDATE ERROR","Failed to update mchar with book number: "+ lbkmed + String(Lbkseq) )
//			IF rtn = 1 THEN
//				GOTO Goto39
//			ELSE
				ROLLBACK USING SqlServerTrans;
				Close(w_pics_update_msg_box)
				RETURN -1
//			END IF
	  	END IF
//		Goto39:
		CASE ELSE
			CONTINUE	
		END CHOOSE
	END IF //  contract type selections
	NEXT // for i = 1 to rowcount()
	IF dw_pa_book_assignment.visible = TRUE THEN
		wf_set_required_fields_no()
	END IF
END IF // // Check for any pending updates
// Commit Oracle database
COMMIT USING SqlServerTrans;
Close(w_pics_update_msg_box)
ib_disableclosequery = TRUE

RETURN 1
end event

type dw_pa_par_reprot from u_pics_dw within w_pa_assigning_books
boolean visible = false
integer x = 2194
integer y = 1504
integer width = 82
integer height = 64
integer taborder = 0
string dataobject = "d_cc_par_report_assigndt"
boolean vscrollbar = false
boolean border = false
boolean livescroll = false
borderstyle borderstyle = stylebox!
boolean ib_isupdateable = false
boolean ib_rmbmenu = false
boolean ib_rmbfocuschange = false
string is_updatesallowed = ""
end type

event constructor;call super::constructor;this.of_SetTransObject(SQLServerTrans)
this.of_SetFilter(TRUE)
this.of_SetFind(TRUE)
this.inv_filter.of_SetStyle(1)
this.inv_filter.of_SetColumnNameSource(2)
this.of_SetSort(TRUE)
this.inv_sort.of_SetStyle(1)
this.inv_sort.of_SetColumnNameSource(2)

end event

event retrieveend;call super::retrieveend;ib_disableclosequery = TRUE
end event

type dw_pa_du_par_report from u_pics_dw within w_pa_assigning_books
boolean visible = false
integer x = 2304
integer y = 1504
integer width = 78
integer height = 68
integer taborder = 0
string dataobject = "d_cc_par_report_bkno_dup"
boolean vscrollbar = false
boolean border = false
boolean livescroll = false
borderstyle borderstyle = stylebox!
boolean ib_isupdateable = false
boolean ib_rmbmenu = false
boolean ib_rmbfocuschange = false
string is_updatesallowed = ""
end type

event constructor;call super::constructor;this.of_SetTransObject(SQLServerTrans)
this.of_SetFilter(TRUE)
this.of_SetFind(TRUE)
this.inv_filter.of_SetStyle(1)
this.inv_filter.of_SetColumnNameSource(2)
this.of_SetSort(TRUE)
this.inv_sort.of_SetStyle(1)
this.inv_sort.of_SetColumnNameSource(2)

end event

event retrieveend;call super::retrieveend;ib_disableclosequery = TRUE
end event

type cb_listing from commandbutton within w_pa_assigning_books
integer x = 443
integer y = 1632
integer width = 315
integer height = 112
boolean bringtotop = true
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "Print &Listing "
end type

event clicked;string rc,Lcntrtype
int rtn,i,k,ll_rows
long lcnt,sumpages
string Lmed,Lcntr,Lprdr,Lcntrlc,Lcntrmed

Lcntrtype = dw_pa_ancntr_data.object.cntrtype[1]
Lcntr = dw_pa_ancntr_data.object.cntr[1]
Lcntrmed = dw_pa_ancntr_data.object.cntrmed[1]

IF not SQLServerTrans.DBHandle() >0 THEN
	SQLServerTrans.of_connect() 
END IF


Select prdr,cntrlc
into :Lprdr,:Lcntrlc
from ancntr
where cntr = :Lcntr
and cntrmed = :Lcntrmed
using sqlservertrans;
IF f_check_dberror(SQLServerOracleTrans,"Selecting from ANCNTR table failed ")=FALSE THEN
	RETURN -1
END IF

IF Lcntrtype = "D" THEN
	OpenWithParm(w_pics_retrieve_msg_box,"Printing Producer Listing Report, Please Wait...")
	dw_pa_dupl_book_assignment.AcceptText()
	IF dw_pa_dupl_book_assignment.Sharedata(dw_pa_dupl_listing_for_controler) = -1 THEN
		Close(w_pics_retrieve_msg_box)
		MessageBox("ERROR", "Error sharing data between datawindows.",StopSign!, OK!, 2)
	ELSE
		FOR k=1 TO dw_pa_dupl_listing_for_controler.RowCount()
			IF ((IsNull(dw_pa_dupl_listing_for_controler.object.mchar_med[k])=TRUE OR &
						  dw_pa_dupl_listing_for_controler.object.mchar_med[k]="") OR &
				 (IsNull(dw_pa_dupl_listing_for_controler.object.ancntr_cntr[k])=TRUE OR &
						  dw_pa_dupl_listing_for_controler.object.ancntr_cntr[k]=""))	  THEN			
				
				Lcntr = dw_pa_ancntr_data.object.cntr[1]
				Lmed = dw_pa_ancntr_data.object.cntrmed[1]
				
				// MessageBox("Data",Lcntr + " " + Lmed,StopSign!, OK!, 2)
				
				dw_pa_dupl_listing_for_controler.object.mchar_med[k]=Lmed
				dw_pa_dupl_listing_for_controler.object.ancntr_cntr[k]=Lcntr
				
			END IF
		NEXT
		
		// Calculate total number of books assigned to this contract
		select count(*) into :lcnt from prod where cntr=:Lcntr and prodstage='DU'
		using sqlservertrans;
		IF f_check_dberror(sqlservertrans,'PROD') THEN
			// Display it
			dw_pa_dupl_listing_for_controler.object.tot_books.text = string(lcnt,'###,###')
		END IF
		
		// Calculate total number of copies for each book in this contract
		select sum(qnty * vols) into :sumpages from mchar 
		where bkseq in (select bkseq from prod where cntr = :Lcntr and prodstage = 'DU')
		using sqlservertrans;
		IF f_check_dberror(sqlservertrans,'MCHAR') THEN
			// Display it
			dw_pa_dupl_listing_for_controler.object.tot_cass.text = string(sumpages,'###,###')
		END IF
		
		Close(w_pics_retrieve_msg_box)
		dw_pa_dupl_listing_for_controler.Event pfc_print()
		dw_pa_dupl_listing_for_controler.ShareDataOff()
	END IF
ELSEIF Lcntrtype <> "A" THEN
	OpenWithParm(w_pics_retrieve_msg_box,"Printing Producer Listing Report, Please Wait...")
	dw_pa_book_assignment.AcceptText()
	IF dw_pa_book_assignment.Sharedata(dw_pa_listing_for_prod) = -1 THEN
		MessageBox("ERROR", "Error sharing data between datawindows.",StopSign!, OK!, 2)
		Close(w_pics_retrieve_msg_box)
	ELSE
		Close(w_pics_retrieve_msg_box)
		FOR k=1 TO dw_pa_listing_for_prod.RowCount()
			IF ((IsNull(dw_pa_listing_for_prod.object.mchar_med[k])=TRUE OR &
						  dw_pa_listing_for_prod.object.mchar_med[k]="") OR &
				 (IsNull(dw_pa_listing_for_prod.object.ancntr_cntr[k])=TRUE OR &
						  dw_pa_listing_for_prod.object.ancntr_cntr[k]=""))	  THEN			
				
				Lcntr = dw_pa_ancntr_data.object.cntr[1]
				Lmed = dw_pa_ancntr_data.object.cntrmed[1]
				
				// MessageBox("Data",Lcntr + " " + Lmed,StopSign!, OK!, 2)
				
				dw_pa_listing_for_prod.object.mchar_med[k]=Lmed
				dw_pa_listing_for_prod.object.ancntr_cntr[k]=Lcntr
				dw_pa_listing_for_prod.object.ancntr_cntrlc[k]=Lcntrlc
				dw_pa_listing_for_prod.object.ancntr_prdr[k]=Lprdr
				
			END IF
		NEXT
		dw_pa_listing_for_prod.Event pfc_print()
		dw_pa_listing_for_prod.ShareDataOff()
	END IF
	
	OpenWithParm(w_pics_retrieve_msg_box,"Printing Controller Listing Report, Please Wait...")
	IF dw_pa_book_assignment.Sharedata(dw_pa_listing_for_controler) = -1 THEN
		MessageBox("ERROR", "Error sharing data between datawindows.",StopSign!, OK!, 2)
		Close(w_pics_retrieve_msg_box)
	ELSE
		Close(w_pics_retrieve_msg_box)
		ll_rows = dw_pa_listing_for_controler.RowCount()
		FOR i=1 TO ll_rows
			Lmed = dw_pa_listing_for_controler.object.mchar_med[i]
			Lcntr = dw_pa_listing_for_controler.object.ancntr_cntr[i]
			IF (IsNull(Lmed) OR Lmed="") OR (IsNull(Lcntr) OR Lcntr="")  THEN			
				
				Lcntr = dw_pa_ancntr_data.object.cntr[1]
				Lmed = dw_pa_ancntr_data.object.cntrmed[1]
				
				// MessageBox("Data",Lcntr + " " + Lmed,StopSign!, OK!, 2)
				
				dw_pa_listing_for_controler.object.mchar_med[i]=Lmed
				dw_pa_listing_for_controler.object.ancntr_cntr[i]=Lcntr
				
			END IF
		NEXT
		dw_pa_listing_for_controler.Event pfc_print()
		dw_pa_listing_for_controler.ShareDataOff()
	END IF	
END IF	
end event

type dw_pa_listing_for_prod from u_pics_dw within w_pa_assigning_books
boolean visible = false
integer x = 2487
integer y = 1504
integer width = 82
integer height = 80
integer taborder = 0
string dataobject = "d_pa_listing_for_prod"
boolean vscrollbar = false
boolean livescroll = false
boolean ib_isupdateable = false
boolean ib_rmbmenu = false
boolean ib_rmbfocuschange = false
string is_updatesallowed = ""
end type

event constructor;call super::constructor;this.of_SetTransObject(SQLServerTrans)
this.of_SetFilter(TRUE)
this.of_SetFind(TRUE)
this.inv_filter.of_SetStyle(1)
this.inv_filter.of_SetColumnNameSource(2)
this.of_SetSort(TRUE)
this.inv_sort.of_SetStyle(1)
this.inv_sort.of_SetColumnNameSource(2)

end event

event retrieveend;call super::retrieveend;ib_disableclosequery = TRUE
end event

type dw_pa_listing_for_controler from u_pics_dw within w_pa_assigning_books
boolean visible = false
integer x = 2414
integer y = 1504
integer width = 78
integer height = 52
integer taborder = 0
string dataobject = "d_pa_listing_for_controler"
boolean vscrollbar = false
boolean border = false
boolean livescroll = false
borderstyle borderstyle = stylebox!
boolean ib_isupdateable = false
boolean ib_rmbmenu = false
boolean ib_rmbfocuschange = false
string is_updatesallowed = ""
end type

event constructor;call super::constructor;this.of_SetTransObject(SQLServerTrans)
this.of_SetFilter(TRUE)
this.of_SetFind(TRUE)
this.inv_filter.of_SetStyle(1)
this.inv_filter.of_SetColumnNameSource(2)
this.of_SetSort(TRUE)
this.inv_sort.of_SetStyle(1)
this.inv_sort.of_SetColumnNameSource(2)

end event

event retrieveend;call super::retrieveend;ib_disableclosequery = TRUE
end event

type dw_pa_dupl_listing_for_controler from u_pics_dw within w_pa_assigning_books
event constructor pbm_constructor
boolean visible = false
integer x = 2597
integer y = 1504
integer width = 96
integer height = 72
integer taborder = 0
string dataobject = "d_pa_dupl_listing_for_controler"
boolean vscrollbar = false
boolean livescroll = false
boolean ib_isupdateable = false
boolean ib_rmbmenu = false
boolean ib_rmbfocuschange = false
string is_updatesallowed = ""
end type

event constructor;call super::constructor;this.of_SetTransObject(SQLServerTrans)
this.of_SetFilter(TRUE)
this.of_SetFind(TRUE)
this.inv_filter.of_SetStyle(1)
this.inv_filter.of_SetColumnNameSource(2)
this.of_SetSort(TRUE)
this.inv_sort.of_SetStyle(1)
this.inv_sort.of_SetColumnNameSource(2)

end event

event retrieveend;call super::retrieveend;ib_disableclosequery = TRUE
end event

type dw_pa_dupl_book_assignment from u_pics_dw within w_pa_assigning_books
event ue_enterkey pbm_dwnprocessenter
boolean visible = false
integer y = 160
integer width = 2917
integer height = 1300
integer taborder = 30
string dataobject = "d_pa_dupl_bk_assignment"
boolean hscrollbar = true
boolean livescroll = false
boolean ib_isupdateable = false
boolean ib_rmbmenu = false
boolean ib_rmbfocuschange = false
string is_updatesallowed = ""
end type

event ue_enterkey;call super::ue_enterkey;Integer li_ColNbr,currow,rowcount
currow = dw_pa_dupl_book_assignment.GetRow()
rowcount = dw_pa_dupl_book_assignment.RowCount()

li_ColNbr = dw_pa_dupl_book_assignment.GetColumn()
// MessageBox("col number",li_ColNbr)
IF li_ColNbr=9 AND ( currow >= rowcount) THEN
	dw_pa_dupl_book_assignment.Event pfc_addrow ()
ELSE
	Send(Handle(this),256,9,Long(0,0))
	return(1)
END IF


end event

event pfc_addrow;long	ll_rc,Lgray,Lwhite

Lgray = RGB(192,192,192)
Lwhite = RGB(255,255,255)

// Set Taborder.
wf_set_taborder_dupl()

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

this.ScrollToRow(ll_rc)
this.SetColumn("mchar_bkseq")
sle_no_books.text = string(ll_rc)

return ll_rc
end event

event ue_postconstructor;call super::ue_postconstructor;// Save the original select statement for later usage.
dw_pa_dupl_book_assignment.of_SetTransObject(SQLServerTrans)
mod_dupl_string = ""
original_dupl_select = ""
original_dupl_tbl_select = ""
original_dupl_select = dw_pa_dupl_book_assignment.Describe("DataWindow.Table.Select")
//original_dupl_tbl_select = "DataWindow.Table.Select='" + original_dupl_select + "'"

original_dupl_tbl_select = "DataWindow.Table.Select='" + &
"  SELECT mchar.conno, " + &
"         mchar.bkseq, " + &
"			 mchar.bkmed, " + &
"			 mchar.chno, " + &
"			 mchar.applen, " + &
"			 mchar.priority, " + &
"			 mchar.df, " + &
"			 mchar.vols, " + &
"			 mchar.qnty, " + &
"			 mchar.med, " + &
"			 prod.prodstage, " + &
"			 prod.cntr, " + &
" 			 prod.schstdt, " + &
" 			 prod.schenddt, "  + &
" 			 prod.assigndt, "  + &
" 			 ttlinit.ttl, " + &
"    		 FROM mchar,prod,ttlinit " + &
"   		 WHERE mchar.bkseq = prod.bkseq  " + &
"			 and   mchar.chno = ttlinit.chno "  + "~'"

// "			 and   mchar.bkmed = prod.bkmed  " + &

end event

event itemchanged;call super::itemchanged;Boolean rtn=FALSE
string Lconno,Lbkmed,Lpriority,Lttl,Ls_message,ls_msgparm[1],ls_cntrmed
string Lchno,Lcntr,Lstage
long Lbkseq
integer Lapplen,Lestpt,Lvols,Lqnty
date Lschstdt,Lschenddt,Lassignment_date

IF dwo.Name = "mchar_bkseq" THEN
	int Lcnt_dupl
	string nullconno
	SetNull(nullconno)
	
	IF f_is_it_archived(nullconno,long(data))=TRUE THEN
		RETURN 1
	ELSE
		rtn = wf_validate_bk_dupl(long(data))
		IF rtn = TRUE THEN
			Open(w_pics_retrieve_msg_box)
			rtn = wf_get_book_dupl_info(row,long(data))
			IF rtn = FALSE THEN
				close(w_pics_retrieve_msg_box)
				RETURN 1
			ELSE
				dw_pa_dupl_book_assignment.SetItemStatus(row, 0, Primary!, NewModified!)
			END IF
			close(w_pics_retrieve_msg_box)
		ELSE
//			dw_pa_dupl_book_assignment.Object.mchar_bkseq.ValidationMsg='Book Number does not exist.'
			RETURN 1
		END IF
	END IF
ELSEIF dwo.Name = "prod_schstdt" THEN
	Lschstdt = date(data)
	Lassignment_date  = date(dw_pa_dupl_book_assignment.object.prod_assigndt[row])
	IF ( Lschstdt < Lassignment_date ) THEN
		dw_pa_dupl_book_assignment.Object.prod_schstdt.ValidationMsg='Schedule Start Date can not be earlier than Assignment Date.'
		RETURN 1
	ELSE
		Lpriority = dw_pa_dupl_book_assignment.object.mchar_priority[row]
		IF Lpriority="N" THEN
			Lestpt=70
		ELSEIF Lpriority="P" THEN
			Lestpt=40
		ELSEIF Lpriority="R" THEN
			Lestpt=20
		END IF
		dw_pa_dupl_book_assignment.object.prod_schenddt[row] = RelativeDate(Lschstdt, Lestpt)
	END IF
	dw_pa_dupl_book_assignment.Object.mchar_chno[row]=string(Lestpt)
ELSEIF dwo.Name = "mchar_priority" THEN
	IF data="N" THEN
		Lestpt=70
	ELSEIF data="P" THEN
		Lestpt=40
	ELSEIF data="R" THEN
		Lestpt=20
	END IF
	Lschstdt  = date(dw_pa_dupl_book_assignment.object.prod_schstdt[row])
	dw_pa_dupl_book_assignment.object.prod_schenddt[row] = RelativeDate(Lschstdt, Lestpt)
	dw_pa_dupl_book_assignment.Object.mchar_chno[row]=string(Lestpt)
ELSEIF dwo.Name = "mchar_vols" THEN
    ls_cntrmed = dw_pa_ancntr_data.object.cntrmed[1]
    IF ls_cntrmed = 'FC' THEN
		IF integer(data) > 2 THEN
			dw_pa_dupl_book_assignment.Object.mchar_vols.ValidationMsg='Volumes can not be more than 2.'
			RETURN 1
		END IF
	END IF
ELSEIF dwo.Name = "prod_schenddt" THEN
	Lschenddt = date(data)
	Lschstdt  = date(dw_pa_dupl_book_assignment.object.prod_schstdt[row])
	IF ( Lschenddt < Lschstdt ) THEN
		dw_pa_dupl_book_assignment.Object.prod_schenddt.ValidationMsg='Schedule End Date can not be earlier than Schedule Start Date.'
		RETURN 1
	END IF
	
////11/03/2009 DTB LABEL DATA VALIDATION
//ELSEIF dwo.name = 'mchar_conno' THEN
//	string ls_data='N', ls_print='N', ls_braille='N'
//	integer li_ret
//	li_ret = f_get_label_data(data, ls_data, ls_print, ls_braille)
//	IF ls_print='Y' AND ls_braille='Y' THEN
//	ELSE
//		Messagebox('Error',' DTB Label Data - Print Label and Braille Label not approved for this control number.')
//		RETURN 1
//	END IF
END IF
end event

event doubleclicked;call super::doubleclicked;//IF dwo.name = "prod_schstdt" THEN
//	IF row > 1 THEN
//		dw_pa_dupl_book_assignment.object.prod_schstdt[row] = &
//		dw_pa_dupl_book_assignment.object.prod_schstdt[row - 1]
//	END IF
//END IF
end event

event constructor;call super::constructor;string ls_excludecols[]
this.of_SetFilter(TRUE)
this.of_SetFind(TRUE)
this.inv_filter.of_SetStyle(1)
this.inv_filter.of_SetColumnNameSource(1)
ls_excludecols[1] = "grayback"
ls_excludecols[2] = "whiteback"
ls_excludecols[3] = "mchar_chno"
ls_excludecols[4] = "prod_prodstage"
this.inv_filter.of_SetExclude(ls_excludecols)
this.of_SetSort(TRUE)
this.inv_sort.of_SetStyle(1)
this.inv_sort.of_SetColumnNameSource(1)
this.inv_sort.of_SetExclude(ls_excludecols)
//this.of_SetDropDownCalendar(TRUE)
//this.iuo_calendar.of_Register("prod_schstdt",this.iuo_calendar.DDLB)

end event

event pfc_deleterow;integer	li_rc


if IsValid (inv_rowmanager) then
	li_rc = inv_rowmanager.event pfc_deleterow () 
else	
	li_rc = this.DeleteRow (0) 
end if

// Notify the Linkage Service 
IF IsValid ( inv_Linkage ) THEN 
	If li_rc > 0 Then inv_Linkage.Event pfc_DeleteRow (0) 
END IF 

sle_no_books.text = string(dw_pa_dupl_book_assignment.rowcount())

return li_rc
end event

type dw_pa_book_assignment_ad from u_pics_dw within w_pa_assigning_books
event ue_enterkey pbm_dwnprocessenter
integer y = 160
integer width = 2917
integer height = 1300
integer taborder = 30
string dataobject = "d_pa_book_assignment_ad"
boolean hscrollbar = true
boolean livescroll = false
boolean ib_isupdateable = false
boolean ib_rmbmenu = false
boolean ib_rmbfocuschange = false
string is_updatesallowed = ""
end type

event ue_enterkey;Integer li_ColNbr,currow,rowcount
currow = this.GetRow()
rowcount = this.RowCount()

li_ColNbr = this.GetColumn() 
// MessageBox("col number",li_ColNbr)
IF li_ColNbr=7 AND ( currow >= rowcount) THEN
	this.Event pfc_addrow ()
ELSE
	Send(Handle(this),256,9,Long(0,0))
	return(1)
END IF


end event

event constructor;call super::constructor;string ls_excludecols[]
this.of_SetFilter(TRUE)
this.of_SetFind(TRUE)
this.inv_filter.of_SetStyle(1)
this.inv_filter.of_SetColumnNameSource(2)
ls_excludecols[1] = "grayback"
ls_excludecols[2] = "whiteback"
ls_excludecols[3] = "mchar_chno"
ls_excludecols[4] = "prod_prodstage"
this.inv_filter.of_SetExclude(ls_excludecols)
this.of_SetSort(TRUE)
this.inv_sort.of_SetStyle(1)
this.inv_sort.of_SetColumnNameSource(2)
this.inv_sort.of_SetExclude(ls_excludecols)
//this.of_SetDropDownCalendar(TRUE)
//this.iuo_calendar.of_Register("prod_schstdt",this.iuo_calendar.DDLB)

end event

event doubleclicked;call super::doubleclicked;//IF dwo.name = "mchar_bkseq" THEN
//	IF row > 1 THEN
//		dw_pa_book_assignment.object.mchar_bkseq[row] = &
//		dw_pa_book_assignment.object.mchar_bkseq[row - 1] + 1
//	END IF
////ELSEIF dwo.name = "prod_schstdt" THEN
////	IF row > 1 THEN
////		dw_pa_book_assignment.object.prod_schstdt[row] = &
////		dw_pa_book_assignment.object.prod_schstdt[row - 1]
////	END IF
//END IF
end event

event itemchanged;call super::itemchanged;string Lcontract_med,Lcontract_type,Lcntrcvcd,Lpriority,Lcntr,Lbkmed
integer Lapplen,Lestpt
long Lbkseq
double Ldf
date Lschstdt,Lassignment_date,Lschenddt
Boolean rtn=FALSE

Lcontract_med = dw_pa_ancntr_data.object.cntrmed[1]

//IF dwo.Name = "mchar_conno" THEN
//	IF query_mode_on=FALSE THEN
//		rtn = wf_validate_conno(data)
//		IF rtn = TRUE THEN
//			rtn = wf_get_conno_info(row,data)
//			IF rtn <> TRUE THEN
//				RETURN 1
//			ELSE
//				dw_pa_book_assignment.SetItemStatus(row, 0, Primary!, NewModified!)
//			END IF
//		ELSE
//			RETURN 1
//		END IF
//	END IF
//ELSEIF dwo.Name = "mchar_bkseq" THEN
//	IF query_mode_on=FALSE THEN
//		rtn = wf_validate_bkseq(long(data),"MASTERING")
//		IF rtn = FALSE THEN
//			RETURN 1
//		ELSE
//			dw_pa_book_assignment.SetItemStatus(row, 0, Primary!, NewModified!)
//		END IF
//	END IF
//ELSEIF dwo.Name = "prod_assigndt" THEN
//	SetNull(Lassignment_date)
//	Lassignment_date = date(data)
//	IF Lassignment_date < date("01/01/1950")  OR  Lassignment_date > date("01/01/2050") THEN
//		RETURN 1
//	END IF
//ELSEIF dwo.name = "prod_prodstage" THEN
//	Lcntr = dw_pa_ancntr_data.object.cntr[1]
//	Lbkseq = dw_pa_book_assignment.object.mchar_bkseq[row]
//	Lbkmed = dw_pa_book_assignment.object.mchar_bkmed[row]
//	IF wf_check_invoice_exist(Lcntr,Lbkseq,Lbkmed)=FALSE THEN
//		MessageBox("Warning","Invoice has already been inputed for book number "+string(Lbkseq)+".~r~nChanging production stage at this time may effect invoicing for this book.",Information!)
//	END IF
//ELSEIF dwo.Name = "prod_schstdt" THEN
//	string lassd
//	SetNull(Lschstdt)
//	Lschstdt = date(data)
//	IF Lschstdt < Date("1900-01-01")  OR  Lschstdt > Date("2050-01-01") THEN
//		RETURN 1
//	END IF
////	lassd = string(dw_pa_book_assignment.object.prod_assigndt[row],'MM/DD/YYYY')
//	Lassignment_date  = date(dw_pa_book_assignment.object.prod_assigndt[row])
//	IF ( Lschstdt < Lassignment_date ) THEN
//		dw_pa_book_assignment.Object.prod_schstdt.ValidationMsg='Schedule Start Date can not be earlier than Assignment Date.'
//		RETURN 1
//	END IF
//	Lcntrcvcd = dw_pa_ancntr_data.object.cntrcvcd[1]
//	Lpriority = dw_pa_book_assignment.object.mchar_priority[row]
//	Lapplen = dw_pa_book_assignment.object.mchar_applen[row]
//	Ldf = dw_pa_book_assignment.object.mchar_df[row]
//	IF IsNull(Lpriority)=FALSE AND &
//		IsNull(Lapplen)=FALSE AND &
//		IsNull(data)=FALSE THEN
//		Lestpt = f_calculate_estpt(Lcontract_med,Lpriority,Lapplen,Lcntrcvcd,Ldf)
//		dw_pa_book_assignment.object.prod_schenddt[row] = string(RelativeDate(Lschstdt, Lestpt),'MM/DD/YY')
//		dw_pa_book_assignment.Object.mchar_chno[row]=string(f_calculate_estpt(Lcontract_med,Lpriority,Lapplen,Lcntrcvcd,Ldf))
//	END IF
//ELSEIF dwo.Name = "mchar_df" THEN
//	Lschstdt = date(dw_pa_book_assignment.object.prod_schstdt[row])
//	Lcntrcvcd = dw_pa_ancntr_data.object.cntrcvcd[1]
//	Lpriority = dw_pa_book_assignment.object.mchar_priority[row]
//	Lapplen = dw_pa_book_assignment.object.mchar_applen[row]
//	Ldf = double(data)
//	IF IsNull(Lpriority)=FALSE AND &
//		IsNull(Lapplen)=FALSE AND &
//		IsNull(data)=FALSE THEN
//		Lestpt = f_calculate_estpt(Lcontract_med,Lpriority,Lapplen,Lcntrcvcd,Ldf)
//		dw_pa_book_assignment.object.prod_schenddt[row] = string( RelativeDate(Lschstdt, Lestpt),'MM/DD/YY')
//		dw_pa_book_assignment.Object.mchar_chno[row]=string(f_calculate_estpt(Lcontract_med,Lpriority,Lapplen,Lcntrcvcd,Ldf))
//	END IF
//ELSEIF dwo.Name = "mchar_applen" THEN
//	Lschstdt = date(dw_pa_book_assignment.object.prod_schstdt[row])
//	Lcntrcvcd = dw_pa_ancntr_data.object.cntrcvcd[1]
//	Lpriority = dw_pa_book_assignment.object.mchar_priority[row]
//	Lapplen = integer(data)
//	Ldf = dw_pa_book_assignment.object.mchar_df[row]
//	IF IsNull(Lpriority)=FALSE AND &
//		IsNull(Lapplen)=FALSE AND &
//		IsNull(Ldf)=FALSE THEN
//		Lestpt = f_calculate_estpt(Lcontract_med,Lpriority,Lapplen,Lcntrcvcd,Ldf)
//		dw_pa_book_assignment.object.prod_schenddt[row] = string (RelativeDate(Lschstdt, Lestpt), 'MM/DD/YY')
//		dw_pa_book_assignment.Object.mchar_chno[row]=string(f_calculate_estpt(Lcontract_med,Lpriority,Lapplen,Lcntrcvcd,Ldf))
//	END IF
//ELSEIF dwo.Name = "mchar_priority" THEN
//	Lschstdt = date(dw_pa_book_assignment.object.prod_schstdt[row])
//	Lcntrcvcd = dw_pa_ancntr_data.object.cntrcvcd[1]
//	Lpriority = data
//	Lapplen = dw_pa_book_assignment.object.mchar_applen[row]
//	Ldf = dw_pa_book_assignment.object.mchar_df[row]
//	IF IsNull(Lpriority)=FALSE AND &
//		IsNull(Lapplen)=FALSE AND &
//		IsNull(Ldf)=FALSE THEN
//		Lestpt = f_calculate_estpt(Lcontract_med,Lpriority,Lapplen,Lcntrcvcd,Ldf)
//		dw_pa_book_assignment.object.prod_schenddt[row] = string (RelativeDate(Lschstdt, Lestpt), 'MM/DD/YY')
//		dw_pa_book_assignment.Object.mchar_chno[row]=string(f_calculate_estpt(Lcontract_med,Lpriority,Lapplen,Lcntrcvcd,Ldf))
//	END IF
IF dwo.Name = "prod_schenddt" THEN
	SetNull(Lschenddt)
	Lschenddt = date(data)
	IF Lschenddt < date("01/01/1950")  OR  Lschenddt > date("01/01/2050") THEN
		RETURN 1
	END IF
	Lschstdt  = date(this.object.prod_schstdt[row])
	IF ( Lschenddt < Lschstdt ) THEN
		this.Object.prod_schenddt.ValidationMsg='Schedule End Date can not be earlier than Schedule Start Date.'
		RETURN 1
	END IF
END IF
end event

event itemfocuschanged;call super::itemfocuschanged;//string Lcontract_med,Lcontract_type,Lcntrcvcd,Lpriority
//integer Lapplen,Lestpt
//double Ldf
//date Lschstdt,Lassignment_date,Lschenddt
//Boolean rtn=FALSE
//Lcontract_med = dw_pa_ancntr_data.object.cntrmed[1]
//IF dwo.Name = "prod_schstdt" THEN
//	Lcntrcvcd 	= dw_pa_ancntr_data.object.cntrcvcd[1]
//	Lschstdt 	= date(dw_pa_book_assignment.object.prod_schstdt[row])
//	Lpriority 	= dw_pa_book_assignment.object.mchar_priority[row]
//	Lapplen 		= dw_pa_book_assignment.object.mchar_applen[row]
//	Ldf 			= dw_pa_book_assignment.object.mchar_df[row]
//	IF IsNull(Lpriority)=FALSE AND &
//		IsNull(Lapplen)=FALSE AND &
//		IsNull(Ldf)=FALSE THEN
//		Lestpt = f_calculate_estpt(Lcontract_med,Lpriority,Lapplen,Lcntrcvcd,Ldf)
//		dw_pa_book_assignment.object.prod_schenddt[row] = string(RelativeDate(Lschstdt, Lestpt),'MM/DD/YY')
//		IF Lcontract_med = "P/B" THEN
//			// Since this is a new print braille book, we will use 90 days
//			// as estimated production time. Remember that we are using the
//			// mchar_chno to show the value of the estpt(Display only).
//			// We will not update chno in our update statement.
//			dw_pa_book_assignment.Object.mchar_chno[row]="90"
//		END IF
//	END IF
//ELSEIF dwo.Name = "mchar_df" THEN
//	Lschstdt	= date(dw_pa_book_assignment.object.prod_schstdt[row])
//	Lcntrcvcd = dw_pa_ancntr_data.object.cntrcvcd[1]
//	Lpriority = dw_pa_book_assignment.object.mchar_priority[row]
//	Lapplen = dw_pa_book_assignment.object.mchar_applen[row]
//	Ldf = dw_pa_book_assignment.object.mchar_df[row]
//	IF IsNull(Lpriority)=FALSE AND &
//		IsNull(Lapplen)=FALSE AND &
//		IsNull(Ldf)=FALSE THEN
//		Lestpt = f_calculate_estpt(Lcontract_med,Lpriority,Lapplen,Lcntrcvcd,Ldf)
//		dw_pa_book_assignment.object.prod_schenddt[row] = string(RelativeDate(Lschstdt, Lestpt),'MM/DD/YY')
//	END IF
//ELSEIF dwo.Name = "mchar_applen" THEN
//	Lschstdt	= date(dw_pa_book_assignment.object.prod_schstdt[row])
//	Lcntrcvcd = dw_pa_ancntr_data.object.cntrcvcd[1]
//	Lpriority = dw_pa_book_assignment.object.mchar_priority[row]
//	Lapplen = dw_pa_book_assignment.object.mchar_applen[row]
//	Ldf = dw_pa_book_assignment.object.mchar_df[row]
//	IF IsNull(Lpriority)=FALSE AND &
//		IsNull(Lapplen)=FALSE AND &
//		IsNull(Ldf)=FALSE THEN
//		Lestpt = f_calculate_estpt(Lcontract_med,Lpriority,Lapplen,Lcntrcvcd,Ldf)
//		dw_pa_book_assignment.object.prod_schenddt[row] = string(RelativeDate(Lschstdt, Lestpt),'MM/DD/YY')
//	END IF
//END IF
end event

event ue_postconstructor;call super::ue_postconstructor;// Save the original select statement for later usage.
dw_Pa_book_assignment_ad.of_SetTransObject(SQLServerTrans)
mod_string = ""
original_ad_select = ""
original_ad_tbl_select = ""
original_ad_select = dw_Pa_book_assignment_ad.Describe("DataWindow.Table.Select")
original_ad_tbl_select = "DataWindow.Table.Select= '" + original_ad_select + "'"
//
original_ad_tbl_select = "DataWindow.Table.Select= '" + &
"  SELECT mchar.conno, " + &
"         mchar.bkseq, " + &
"			 mchar.bkmed, " + &
"			 mchar.chno, " + &
"			 mchar.applen, " + &
"			 mchar.priority, " + &
"			 mchar.df, " + &
"			 mchar.vols, " + &
"			 mchar.qnty, " + &
"			 mchar.med, " + &
"			 prod.prodstage, " + &
"			 prod.cntr, " + &
" 			 prod.schstdt, " + &
" 			 prod.schenddt, "  + &
" 			 prod.assigndt, "  + &
" 			 ttlinit.ttl, " + &
"			 ancntr.cntrlc, " + &
"			 ancntr.prdr " + &
"    		 FROM mchar,prod,ttlinit,ancntr " + &
"   		 WHERE mchar.bkseq = prod.bkseq  " + &
"			 and   mchar.chno = ttlinit.chno "  + &
"			 and   prod.cntr = ancntr.cntr '" 

end event

type dw_pa_book_assignment from u_pics_dw within w_pa_assigning_books
event ue_enterkey pbm_dwnprocessenter
integer y = 164
integer width = 2921
integer height = 1300
integer taborder = 20
string dataobject = "d_pa_book_assignment"
boolean hscrollbar = true
boolean livescroll = false
boolean ib_isupdateable = false
boolean ib_rmbmenu = false
boolean ib_rmbfocuschange = false
string is_updatesallowed = ""
end type

event ue_enterkey;call super::ue_enterkey;Integer li_ColNbr,currow,rowcount
currow = dw_pa_book_assignment.GetRow()
rowcount = dw_pa_book_assignment.RowCount()

li_ColNbr = dw_pa_book_assignment.GetColumn() 
// MessageBox("col number",li_ColNbr)
IF li_ColNbr=7 AND ( currow >= rowcount) THEN
	dw_pa_book_assignment.Event pfc_addrow ()
ELSE
	Send(Handle(this),256,9,Long(0,0))
	return(1)
END IF


end event

event itemchanged;call super::itemchanged;string Lcontract_med,Lcontract_type,Lcntrcvcd,Lpriority,Lcntr,Lbkmed
integer Lapplen,Lestpt
long Lbkseq
double Ldf
date Lschstdt,Lassignment_date,Lschenddt
Boolean rtn=FALSE

Lcontract_med = dw_pa_ancntr_data.object.cntrmed[1]

IF dwo.Name = "mchar_conno" THEN
	IF query_mode_on=FALSE THEN
		rtn = wf_validate_conno(data)
		IF rtn = TRUE THEN
			rtn = wf_get_conno_info(row,data)
			IF rtn <> TRUE THEN
				RETURN 1
			ELSE
				dw_pa_book_assignment.SetItemStatus(row, 0, Primary!, NewModified!)
			END IF
		ELSE
			RETURN 1
		END IF
	END IF
ELSEIF dwo.Name = "mchar_bkseq" THEN
	IF query_mode_on=FALSE THEN
		rtn = wf_validate_bkseq(long(data),"MASTERING")
		IF rtn = FALSE THEN
			RETURN 1
		ELSE
			dw_pa_book_assignment.SetItemStatus(row, 0, Primary!, NewModified!)
		END IF
	END IF
ELSEIF dwo.Name = "prod_assigndt" THEN
	SetNull(Lassignment_date)
	Lassignment_date = date(data)
ELSEIF dwo.name = "prod_prodstage" THEN
	Lcntr = dw_pa_ancntr_data.object.cntr[1]
	Lbkseq = dw_pa_book_assignment.object.mchar_bkseq[row]
	Lbkmed = dw_pa_book_assignment.object.mchar_bkmed[row]
	IF wf_check_invoice_exist(Lcntr,Lbkseq,Lbkmed)=FALSE THEN
		MessageBox("Warning","Invoice has already been inputed for book number "+string(Lbkseq)+".~r~nChanging production stage at this time may effect invoicing for this book.",Information!)
	END IF
ELSEIF dwo.Name = "prod_schstdt" THEN
	string lassd
	SetNull(Lschstdt)
	Lschstdt = date(data)
	Lassignment_date  = date(dw_pa_book_assignment.object.prod_assigndt[row])
	IF ( Lschstdt < Lassignment_date ) THEN
		dw_pa_book_assignment.Object.prod_schstdt.ValidationMsg='Schedule Start Date can not be earlier than Assignment Date.'
		RETURN 1
	END IF
	Lcntrcvcd = dw_pa_ancntr_data.object.cntrcvcd[1]
	Lpriority = dw_pa_book_assignment.object.mchar_priority[row]
	Lapplen = dw_pa_book_assignment.object.mchar_applen[row]
	Ldf = dw_pa_book_assignment.object.mchar_df[row]
	IF IsNull(Lpriority)=FALSE AND &
		IsNull(Lapplen)=FALSE AND &
		IsNull(data)=FALSE THEN
		Lestpt = f_calculate_estpt(Lcontract_med,Lpriority,Lapplen,Lcntrcvcd,Ldf)
		dw_pa_book_assignment.object.prod_schenddt[row] = RelativeDate(Lschstdt, Lestpt)
		dw_pa_book_assignment.Object.mchar_chno[row]=string(f_calculate_estpt(Lcontract_med,Lpriority,Lapplen,Lcntrcvcd,Ldf))
	END IF
ELSEIF dwo.Name = "mchar_df" THEN
	Lschstdt = date(dw_pa_book_assignment.object.prod_schstdt[row])
	Lcntrcvcd = dw_pa_ancntr_data.object.cntrcvcd[1]
	Lpriority = dw_pa_book_assignment.object.mchar_priority[row]
	Lapplen = dw_pa_book_assignment.object.mchar_applen[row]
	Ldf = double(data)
	IF IsNull(Lpriority)=FALSE AND &
		IsNull(Lapplen)=FALSE AND &
		IsNull(data)=FALSE THEN
		Lestpt = f_calculate_estpt(Lcontract_med,Lpriority,Lapplen,Lcntrcvcd,Ldf)
		dw_pa_book_assignment.object.prod_schenddt[row] = RelativeDate(Lschstdt, Lestpt)
		dw_pa_book_assignment.Object.mchar_chno[row]=string(f_calculate_estpt(Lcontract_med,Lpriority,Lapplen,Lcntrcvcd,Ldf))
	END IF
ELSEIF dwo.Name = "mchar_applen" THEN
	Lschstdt = date(dw_pa_book_assignment.object.prod_schstdt[row])
	Lcntrcvcd = dw_pa_ancntr_data.object.cntrcvcd[1]
	Lpriority = dw_pa_book_assignment.object.mchar_priority[row]
	Lapplen = integer(data)
	Ldf = dw_pa_book_assignment.object.mchar_df[row]
	IF IsNull(Lpriority)=FALSE AND &
		IsNull(Lapplen)=FALSE AND &
		IsNull(Ldf)=FALSE THEN
		Lestpt = f_calculate_estpt(Lcontract_med,Lpriority,Lapplen,Lcntrcvcd,Ldf)
		dw_pa_book_assignment.object.prod_schenddt[row] = RelativeDate(Lschstdt, Lestpt)
		dw_pa_book_assignment.Object.mchar_chno[row]=string(f_calculate_estpt(Lcontract_med,Lpriority,Lapplen,Lcntrcvcd,Ldf))
	END IF
ELSEIF dwo.Name = "mchar_priority" THEN
	Lschstdt = date(dw_pa_book_assignment.object.prod_schstdt[row])
	Lcntrcvcd = dw_pa_ancntr_data.object.cntrcvcd[1]
	Lpriority = data
	Lapplen = dw_pa_book_assignment.object.mchar_applen[row]
	Ldf = dw_pa_book_assignment.object.mchar_df[row]
	IF IsNull(Lpriority)=FALSE AND &
		IsNull(Lapplen)=FALSE AND &
		IsNull(Ldf)=FALSE THEN
		Lestpt = f_calculate_estpt(Lcontract_med,Lpriority,Lapplen,Lcntrcvcd,Ldf)
		dw_pa_book_assignment.object.prod_schenddt[row] = RelativeDate(Lschstdt, Lestpt)
		dw_pa_book_assignment.Object.mchar_chno[row]=string(f_calculate_estpt(Lcontract_med,Lpriority,Lapplen,Lcntrcvcd,Ldf))
	END IF
ELSEIF dwo.Name = "prod_schenddt" THEN
	SetNull(Lschenddt)
	Lschenddt = date(data)
	Lschstdt  = date(dw_pa_book_assignment.object.prod_schstdt[row])
	IF ( Lschenddt < Lschstdt ) THEN
		dw_pa_book_assignment.Object.prod_schenddt.ValidationMsg='Schedule End Date can not be earlier than Schedule Start Date.'
		RETURN 1
	END IF
END IF
end event

event pfc_addrow;long	ll_rc
//long Lgray,Lwhite
//
//Lgray = RGB(192,192,192)
//Lwhite = RGB(255,255,255)

// Set Taborder.
wf_set_taborder()

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
this.ScrollToRow(ll_rc)
this.SetColumn("mchar_conno")
sle_no_books.text = string(ll_rc)
return ll_rc
end event

event ue_postconstructor;call super::ue_postconstructor;// Save the original select statement for later usage.
dw_pa_book_assignment.of_SetTransObject(SQLServerTrans)
mod_string = ""
original_select = ""
original_tbl_select = ""
original_select = dw_pa_book_assignment.Describe("DataWindow.Table.Select")
//original_tbl_select = "DataWindow.Table.Select= '" + original_select + "'"

original_tbl_select = "DataWindow.Table.Select= '" + &
"  SELECT mchar.conno, " + &
"         mchar.bkseq, " + &
"			 mchar.bkmed, " + &
"			 mchar.chno, " + &
"			 mchar.applen, " + &
"			 mchar.priority, " + &
"			 mchar.df, " + &
"			 mchar.vols, " + &
"			 mchar.qnty, " + &
"			 mchar.med, " + &
"			 prod.prodstage, " + &
"			 prod.cntr, " + &
" 			 prod.schstdt, " + &
" 			 prod.schenddt, "  + &
" 			 prod.assigndt, "  + &
" 			 ttlinit.ttl, " + &
"			 ancntr.cntrlc, " + &
"			 ancntr.prdr " + &
"    		 FROM mchar,prod,ttlinit,ancntr " + &
"   		 WHERE mchar.bkseq = prod.bkseq  " + &
"			 and   mchar.bkmed = prod.bkmed  " + &
"			 and   mchar.chno = ttlinit.chno "  + &
"			 and   prod.cntr = ancntr.cntr " + &
"			 and	 mchar.med = ancntr.cntrmed '"

end event

event pfc_deleterow;integer	li_rc,rtn,Li_flashind

if IsValid (inv_rowmanager) then
	li_rc = inv_rowmanager.event pfc_deleterow () 
else	
	li_rc = this.DeleteRow (0) 
end if

IF this.object.flash_indicator[li_rc] = 'Y' THEN
	Li_flashind = Integer(sle_no_flash.text)
	Li_flashind = Li_flashind - 1
	sle_no_flash.text = String(Li_flashind)
END IF


// Notify the Linkage Service 
IF IsValid ( inv_Linkage ) THEN 
	If li_rc > 0 Then inv_Linkage.Event pfc_DeleteRow (0) 
END IF 

//END IF	
sle_no_books.text = string(dw_pa_book_assignment.rowcount())

return li_rc
end event

event pfc_insertrow;long	ll_currow
long	ll_rc

//long Lgray,Lwhite
//
//Lgray = RGB(192,192,192)
//Lwhite = RGB(255,255,255)

// Set Taborder.
wf_set_taborder()

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

return ll_rc
end event

event doubleclicked;call super::doubleclicked;IF dwo.name = "mchar_bkseq" THEN
	long ll_bkseq
	string ls_bkseq,ls_cntr
	int lcnt
	str_date lstr_date
	
	ll_bkseq = long(this.object.mchar_bkseq[row])
	
	lstr_date.ls_bkseq = string(this.object.mchar_bkseq[row])
	lstr_date.ls_cntr= dw_pa_ancntr_data.object.cntr[1]
	
	select count(*) into :Lcnt from mchar
	where bkseq=:ll_bkseq
	using sqlservertrans;

	// If the book is not yet in the system
	IF lcnt = 0 THEN				
		IF row > 1 THEN
			dw_pa_book_assignment.object.mchar_bkseq[row] = &
			dw_pa_book_assignment.object.mchar_bkseq[row - 1] + 1
		END IF
	ELSE
		OpenWithParm(w_books_start_end_stages,lstr_date)
	END IF

END IF


end event

event itemfocuschanged;call super::itemfocuschanged;string Lcontract_med,Lcontract_type,Lcntrcvcd,Lpriority
integer Lapplen,Lestpt
double Ldf
date Lschstdt,Lassignment_date,Lschenddt
Boolean rtn=FALSE

Lcontract_med = dw_pa_ancntr_data.object.cntrmed[1]
IF dwo.Name = "prod_schstdt" THEN
	Lcntrcvcd 	= dw_pa_ancntr_data.object.cntrcvcd[1]
	Lschstdt 		= date(dw_pa_book_assignment.object.prod_schstdt[row])
	Lpriority 		= dw_pa_book_assignment.object.mchar_priority[row]
	Lapplen 		= dw_pa_book_assignment.object.mchar_applen[row]
	Ldf 			= dw_pa_book_assignment.object.mchar_df[row]
	IF IsNull(Lpriority)=FALSE AND &
		IsNull(Lapplen)=FALSE AND &
		IsNull(Ldf)=FALSE THEN
		Lestpt = f_calculate_estpt(Lcontract_med,Lpriority,Lapplen,Lcntrcvcd,Ldf)
		dw_pa_book_assignment.object.prod_schenddt[row] = RelativeDate(Lschstdt, Lestpt)
		IF Lcontract_med = "P/B" THEN
			// Since this is a new print braille book, we will use 90 days
			// as estimated production time. Remember that we are using the
			// mchar_chno to show the value of the estpt(Display only).
			// We will not update chno in our update statement.
			dw_pa_book_assignment.Object.mchar_chno[row]="90"
		END IF
	END IF
ELSEIF dwo.Name = "mchar_df" THEN
	Lschstdt	= date(dw_pa_book_assignment.object.prod_schstdt[row])
	Lcntrcvcd = dw_pa_ancntr_data.object.cntrcvcd[1]
	Lpriority = dw_pa_book_assignment.object.mchar_priority[row]
	Lapplen = dw_pa_book_assignment.object.mchar_applen[row]
	Ldf = dw_pa_book_assignment.object.mchar_df[row]
	IF IsNull(Lpriority)=FALSE AND &
		IsNull(Lapplen)=FALSE AND &
		IsNull(Ldf)=FALSE THEN
		Lestpt = f_calculate_estpt(Lcontract_med,Lpriority,Lapplen,Lcntrcvcd,Ldf)
		dw_pa_book_assignment.object.prod_schenddt[row] = RelativeDate(Lschstdt, Lestpt)
	END IF
ELSEIF dwo.Name = "mchar_applen" THEN
	Lschstdt	= date(dw_pa_book_assignment.object.prod_schstdt[row])
	Lcntrcvcd = dw_pa_ancntr_data.object.cntrcvcd[1]
	Lpriority = dw_pa_book_assignment.object.mchar_priority[row]
	Lapplen = dw_pa_book_assignment.object.mchar_applen[row]
	Ldf = dw_pa_book_assignment.object.mchar_df[row]
	IF IsNull(Lpriority)=FALSE AND &
		IsNull(Lapplen)=FALSE AND &
		IsNull(Ldf)=FALSE THEN
		Lestpt = f_calculate_estpt(Lcontract_med,Lpriority,Lapplen,Lcntrcvcd,Ldf)
		dw_pa_book_assignment.object.prod_schenddt[row] = RelativeDate(Lschstdt, Lestpt)
	END IF
END IF
end event

event constructor;call super::constructor;string ls_excludecols[]
this.of_SetFilter(TRUE)
this.of_SetFind(TRUE)
this.inv_filter.of_SetStyle(1)
this.inv_filter.of_SetColumnNameSource(2)
ls_excludecols[1] = "grayback"
ls_excludecols[2] = "whiteback"
ls_excludecols[3] = "mchar_chno"
ls_excludecols[4] = "prod_prodstage"
this.inv_filter.of_SetExclude(ls_excludecols)
this.of_SetSort(TRUE)
this.inv_sort.of_SetStyle(1)
this.inv_sort.of_SetColumnNameSource(2)
this.inv_sort.of_SetExclude(ls_excludecols)


//this.of_SetDropDownCalendar(TRUE)
//this.iuo_calendar.of_Register("prod_schstdt",this.iuo_calendar.DDLB)


end event


Start of PowerBuilder Binary Data Section : Do NOT Edit
0Dw_pa_assigning_books.bin 
2C00000c00e011cfd0e11ab1a1000000000000000000000000000000000003003e0009fffe000000060000000000000000000000010000000100000000000010000000000200000001fffffffe0000000000000000fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffd00000004fffffffefffffffefffffffeffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff006f00520074006f004500200074006e00790072000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000050016ffffffffffffffff0000000100000000000000000000000000000000000000000000000000000000487d7a3001cacc6000000003000000800000000000500003004f0042005800430054005300450052004d0041000000000000000000000000000000000000000000000000000000000000000000000000000000000102001affffffff00000002ffffffff000000000000000000000000000000000000000000000000000000000000000000000000fffffffe0000000000000000004200500043004f00530058004f00540041005200450047000000000000000000000000000000000000000000000000000000000000000000000000000000000001001affffffffffffffff000000036580f76711cf781945446cb80000545300000000487d7a3001cacc60487d7a3001cacc60000000000000000000000000004f00010065006c00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000102000affffffff00000004ffffffff000000000000000000000000000000000000000000000000000000000000000000000000000000000000001400000000fffffffefffffffeffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
2Cffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff0200000100000008000000000000000000000000001f36e80000000000000000000000000000000000000000000000000000000000000000000000000000000000010000000003200000032000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000006f00430074006e006e00650073007400000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000020012ffffffffffffffffffffffff0000000000000000000000000000000000000000000000000000000000000000000000000000000100000018000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000ffffffffffffffffffffffff0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000ffffffffffffffffffffffff0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000ffffffffffffffffffffffff00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
1Dw_pa_assigning_books.bin 
End of PowerBuilder Binary Data Section : No Source Expected After This Point

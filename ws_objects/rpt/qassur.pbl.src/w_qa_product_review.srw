$PBExportHeader$w_qa_product_review.srw
forward
global type w_qa_product_review from w_sheet
end type
type st_3 from statictext within w_qa_product_review
end type
type mle_1 from multilineedit within w_qa_product_review
end type
type cb_spellcheck from u_cb within w_qa_product_review
end type
type dw_inhousecomments from u_pics_dw within w_qa_product_review
end type
type cb_updateweb from u_cb within w_qa_product_review
end type
type cb_add_narr from u_cb within w_qa_product_review
end type
type cb_comments from u_cb within w_qa_product_review
end type
type st_4 from statictext within w_qa_product_review
end type
type cb_find from u_cb within w_qa_product_review
end type
type cb_exit from u_cb within w_qa_product_review
end type
type cb_clear from u_cb within w_qa_product_review
end type
type cb_update from u_cb within w_qa_product_review
end type
type st_1 from statictext within w_qa_product_review
end type
type em_bkno from uo_conno within w_qa_product_review
end type
type dw_qa_qastg from u_pics_dw within w_qa_product_review
end type
type dw_qa_narr from u_pics_dw within w_qa_product_review
end type
type st_2 from statictext within w_qa_product_review
end type
type cb_narr from u_cb within w_qa_product_review
end type
type dw_qa_cntr from u_pics_dw within w_qa_product_review
end type
type dw_qa_prod_review from u_pics_dw within w_qa_product_review
end type
end forward

global type w_qa_product_review from w_sheet
integer x = 9
integer y = 4
integer width = 2935
integer height = 1372
string title = "Quality Assurance (Product Review)"
st_3 st_3
mle_1 mle_1
cb_spellcheck cb_spellcheck
dw_inhousecomments dw_inhousecomments
cb_updateweb cb_updateweb
cb_add_narr cb_add_narr
cb_comments cb_comments
st_4 st_4
cb_find cb_find
cb_exit cb_exit
cb_clear cb_clear
cb_update cb_update
st_1 st_1
em_bkno em_bkno
dw_qa_qastg dw_qa_qastg
dw_qa_narr dw_qa_narr
st_2 st_2
cb_narr cb_narr
dw_qa_cntr dw_qa_cntr
dw_qa_prod_review dw_qa_prod_review
end type
global w_qa_product_review w_qa_product_review

type variables
string local_recagcy
DataWindowChild ldwc_narr,ldwc_cntr
string current_cntr
boolean ib_rowfocuschanged=FALSE, ib_disabled=FALSE, ib_checkedin=FALSE,ib_changedfrominitial=FALSE
string is_cntrtype, is_cntr, is_bkmed,is_prodstage,is_cntrmed  // 03/17/2008
long il_bkseq, il_qrs
datastore  ids_book_status
n_qa_services inv_qa // 10/15/2008
end variables

forward prototypes
public function boolean wf_find_approved_qastg2 (integer rownum)
public function boolean wf_validate_bkno (long lbkno)
public function boolean wf_validate_initial (string linit)
public subroutine wf_validate_med (string media)
public function boolean wf_validate_narr (string lnarrln, string lnarrfn)
public function boolean wf_validate_narr_lastname (string lnarrln)
public function boolean wf_is_qstage1_approved ()
public function boolean wf_validate_recagcy (string lrecagcy)
public subroutine wf_settab_order_noqastg ()
public subroutine wf_set_taborder_org ()
public subroutine wf_reset_update ()
public function boolean wf_find_approved_qastg1 (integer rownum)
public function integer of_setinhousecomments ()
public function integer of_validaterejreason ()
public function integer of_enabledisable (boolean ab_enable)
public function integer of_setreload (string as_stg, long al_row, string as_status)
public function integer of_validateqastg (long al_book, string as_cntr, string as_bkmed, string as_cntrtype, long al_row, string as_qastg)
protected function integer wf_calculate_units (string media, string cntrtype, integer len, integer qnty, integer minlastside, integer volumn, long al_row)
end prototypes

public function boolean wf_find_approved_qastg2 (integer rownum);int i
FOR i=1 TO rownum 
	IF (dw_qa_qastg.object.qastg[i]='2' AND & 
		(dw_qa_qastg.object.qastatcd[i]='A' OR dw_qa_qastg.object.qastatcd[i]='B') ) THEN
		RETURN TRUE
	END IF
NEXT
RETURN FALSE

end function

public function boolean wf_validate_bkno (long lbkno);string Larflag
integer Lcnt=0


Select count(*) into :Lcnt
from mchar
where bkseq = :Lbkno
and arflag='A'
using sqlservertrans;
IF f_check_dberror(sqlservertrans,"mchar")=FALSE THEN
	RETURN FALSE
ELSEIF Lcnt = 1 THEN
	select count(*) into :Lcnt
	from mchar
	where bkseq = :Lbkno
	using sqlservertrans;
	IF Lcnt=1 THEN
		// Book has been archived
		MessageBox("ERROR","This book number has been archived.")
		RETURN FALSE
	ELSE
		RETURN TRUE
	END IF
ELSE
	int rownum=0
	SELECT count(*) INTO :rownum FROM prod WHERE bkseq = :Lbkno USING sqlservertrans;
	IF rownum = 0 THEN
		MessageBox("Error", "Book Number: "+string(Lbkno)+" Does not exist. ~n Enter the correct Book number." ,Information!, OK!, 2)
		RETURN FALSE
	ELSE
		RETURN TRUE
	END IF
END IF
end function

public function boolean wf_validate_initial (string linit);int Lcnt = 0,rtn

select count(*) into :Lcnt 
from qastg 
where	qainit = :Linit
using sqlservertrans;

IF Lcnt = 0 THEN
	rtn = MessageBox("Warning","Initial "+"~'"+Linit+"~'"+" does not belong to the list of initials. ~r~nWould you like to add that to the list of initials?",Question!,YesNo!,1)
	IF rtn = 1 THEN
		RETURN TRUE
	ELSE
		RETURN FALSE
	END IF
ELSE
	RETURN TRUE
END IF

end function

public subroutine wf_validate_med (string media);CHOOSE CASE media
	CASE "P/B"
		IF string(dw_qa_prod_review.Object.mchar_vols[1]) = "" THEN
			dw_qa_prod_review.Object.mchar_vols[1] = 1
		END IF
	CASE "BR"
		IF string(dw_qa_prod_review.Object.mchar_vols[1]) = "" THEN
			dw_qa_prod_review.Object.mchar_vols[1] = 1 
		END IF
	CASE ELSE
END CHOOSE

end subroutine

public function boolean wf_validate_narr (string lnarrln, string lnarrfn);int rowcnt=0
SELECT count(*) into :rowcnt from narrtbl 
where narr=:lnarrln and narrfn=:lnarrfn
using sqlservertrans;
IF rowcnt=0 THEN
	dw_qa_narr.SetColumn("narr")
	RETURN FALSE
ELSE
	RETURN TRUE
END IF
end function

public function boolean wf_validate_narr_lastname (string lnarrln);int rowcnt=0
SELECT count(*) into :rowcnt from narrtbl 
where narr=:lnarrln
using sqlservertrans;
IF rowcnt=0 THEN
	dw_qa_narr.SetColumn("narr")
	RETURN FALSE
ELSE
	RETURN TRUE
END IF
end function

public function boolean wf_is_qstage1_approved ();int i
FOR i=1 TO dw_qa_qastg.rowcount()
	IF dw_qa_qastg.object.qastatcd[i]='A' THEN
		RETURN TRUE
	END IF
NEXT
RETURN FALSE

end function

public function boolean wf_validate_recagcy (string lrecagcy);int rowcnt=0
SELECT count(*) into :rowcnt from narrtbl 
where recagcy=:lrecagcy
using sqlservertrans;
IF rowcnt=0 THEN
	RETURN FALSE
ELSE
	RETURN TRUE
END IF
end function

public subroutine wf_settab_order_noqastg ();dw_qa_qastg.Object.qastg.tabsequence='10'   
dw_qa_qastg.Object.qarecdt.tabsequence='20'   
dw_qa_qastg.Object.qacompdt.tabsequence='30'   
dw_qa_qastg.Object.qastatcd.tabsequence='40'   
dw_qa_qastg.Object.qainit.tabsequence='50'   
dw_qa_qastg.Object.qarejcd.tabsequence='60'   
dw_qa_qastg.Object.qacomments.tabsequence='70'   

end subroutine

public subroutine wf_set_taborder_org ();dw_qa_qastg.Object.qastg.tabsequence='10'   
dw_qa_qastg.Object.qarecdt.tabsequence='20'   
dw_qa_qastg.Object.qacompdt.tabsequence='30'   
dw_qa_qastg.Object.qastatcd.tabsequence='40'   
dw_qa_qastg.Object.qainit.tabsequence='50'   
dw_qa_qastg.Object.qarejcd.tabsequence='60'   
dw_qa_qastg.Object.qacomments.tabsequence='70'   

end subroutine

public subroutine wf_reset_update ();//////////////////////////////////////////////////////////////////////////////////////////////////////////
//
//	Function: wf_reset_update
//  Args: None
//  Returns : None
//	Description:
//	Reset update flags for datawindows
//
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//	
//	Revision History
//
//	Developed by 	Date 				Version										Tracking#
//									
// Murali K.			02/21/2008      005 PICS Modifications Phase 2	 Reqs: QAS a.8.1, A.8.2
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//

dw_qa_narr.ResetUpdate()
dw_qa_prod_review.ResetUpdate()
dw_qa_qastg.ResetUpdate()
//dw_qa_recagcy.ResetUpdate()

// 02/21/2008
dw_inhousecomments.ResetUpdate()
mle_1.text=''
end subroutine

public function boolean wf_find_approved_qastg1 (integer rownum);int i
FOR i=1 TO rownum 
	IF (dw_qa_qastg.object.qastg[i]='1' AND dw_qa_qastg.object.qastatcd[i]='A' ) THEN
		RETURN TRUE
	END IF
NEXT
RETURN FALSE
end function

public function integer of_setinhousecomments ();//////////////////////////////////////////////////////////////////////////////////////////////////////////
//
//	Function: of_setinhousecomments
//
//	Description:
//	Set column default values before updates
//
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//	
//	Revision History
//
//	Developed by 	Date 				Version						Tracking#
//									
// Murali K.			02/21/2008      005 PICS Modifications	 Reqs: QAS A.8.1
// Murali K.			12/03/2008    Populate CNTR and set modified audit only for old records
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//

long ll_rc
string ls_user, ls_cntr
dwItemStatus l_status

ll_rc = dw_inhousecomments.Rowcount()
IF ll_rc > 0 THEN
	dw_inhousecomments.object.inhouse_comments[1] = mle_1.text
	ls_user = gnv_app.of_getuserid()
	ls_cntr = dw_qa_cntr.object.cntr[1]
	dw_inhousecomments.object.cntr[ll_rc] = ls_cntr
   l_status = dw_inhousecomments.GetItemStatus(ll_rc,0, Primary!)
	  IF l_status = new! or l_status = newmodified!  THEN
		dw_inhousecomments.object.created_by[ll_rc] = ls_user
		dw_inhousecomments.object.created_date[ll_rc] =today()
//		dw_inhousecomments.object.modified_by[ll_rc] = ls_user
//		dw_inhousecomments.object.modified_date[ll_rc] =today()
	ELSEIF l_status = datamodified! THEN
		dw_inhousecomments.object.modified_by[ll_rc] = ls_user
		dw_inhousecomments.object.modified_date[ll_rc] =today()
	END IF
END IF		
RETURN 1
end function

public function integer of_validaterejreason ();//////////////////////////////////////////////////////////////////////////////////////////////////////////
//
//	Function:of_validaterejreason
//
//	Description:
//	Validate rejection code settings
//
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//	
//	Revision History
//
//	Developed by 	Date 				Version								 Tracking#
//									
// Murali K.			02/21/2008      005 PICS Modifications Phase 2	 Reqs: QAS A.8.1, A.8.2
//Murali K.			10/30/2008 		Do not allow rejection code null
//Murali K.			11/5/2008 		NO REJECTION CODE VALIDATION FOR QC0
//											all rejected manual entries should be validated
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//

long ll_rc
int li_loop
string ls_rejcd, ls_rejreason, ls_rej, ls_stg, ls_type

ll_rc = dw_qa_qastg.Rowcount()
IF ll_rc > 0 THEN
	FOR li_loop = 1 	TO ll_rc
		  ls_rejcd = dw_qa_qastg.object.qastatcd[li_loop]

		  ls_rej = dw_qa_qastg.object.qarejcd[li_loop]
		 ls_stg =  dw_qa_qastg.object.qastg[li_loop]
		 ls_type = dw_qa_qastg.object.review_type_code[li_loop] 
		 
		  IF ls_rejcd <> 'R' OR Isnull(ls_rejcd) OR ls_stg = '0' THEN continue // 11/5/2008 NO REJECTION CODE VALIDATION FOR QC0
		  
  		  // 10/30/2008 do not allow rejection code null
  		  IF ls_rejcd = 'R' AND (Isnull(ls_rej) OR Len(trim(ls_rej)) =0) THEN 
			Messagebox('Error', 'Please enter rejection code')
			dw_qa_qastg.Setcolumn('qarejcd')
			dw_qa_qastg.Scrolltorow(li_loop)
			dw_qa_qastg.Setrow(li_loop)
			RETURN -1
		END IF


		  ls_rejreason = dw_qa_qastg.object.qacomments[li_loop]
		  IF Isnull(ls_rejreason) OR Len(trim(ls_rejreason)) =0 THEN
			Messagebox('Error', 'Please enter rejection reason in the comments field')
			dw_qa_qastg.Setcolumn('qacomments')
			dw_qa_qastg.Scrolltorow(li_loop)
			dw_qa_qastg.Setrow(li_loop)
			RETURN -1
		END IF
	NEXT
END IF
RETURN 1
end function

public function integer of_enabledisable (boolean ab_enable);//////////////////////////////////////////////////////////////////////////////////////////////////////////
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
// Murali K.			03/17/2008      005 PICS Modifications	 Reqs: QAS A.8.1, A.8.2
// Murali K. 			10/17/2008  	For older book no reload indicator need to be enabled
// Murali K.			10/30/2008 		Disable rejection code if status is A or I
//////////////////////////////////////////////////////////////////////////////////////////////////////////
long ll_row
string ls_status

//
// ENABLE
IF ab_enable THEN 
	dw_qa_qastg.Object.qastg.Background.Color = RGB(255, 255, 255)
	dw_qa_qastg.Object.qarecdt.Background.Color = RGB(255, 255, 255)
	dw_qa_qastg.Object.qacompdt.Background.Color = RGB(255, 255, 255)
	dw_qa_qastg.Object.qastatcd.Background.Color = RGB(255, 255, 255)
	dw_qa_qastg.Object.qainit.Background.Color = RGB(255, 255, 255)
	dw_qa_qastg.Object.review_type_code.Background.Color = RGB(255, 255, 255)
	dw_qa_qastg.Object.reload_indicator_yn.Background.Color = RGB(255, 255, 255)
	dw_qa_qastg.Object.qarejcd.Background.Color = RGB(255, 255, 255)
	dw_qa_qastg.Object.qacomments.Background.Color = RGB(255, 255, 255)

	dw_qa_qastg.Object.qastg.Tabsequence = 10
	dw_qa_qastg.Object.qarecdt.Tabsequence = 20
	dw_qa_qastg.Object.qacompdt.Tabsequence = 30
	dw_qa_qastg.Object.qastatcd.Tabsequence = 40
	dw_qa_qastg.Object.qainit.Tabsequence = 50
	dw_qa_qastg.Object.review_type_code.Tabsequence = 60
	
	// 10/17/2008  for older book no reload indicator need to be set
	IF NOT ib_checkedin THEN
		dw_qa_qastg.Object.reload_indicator_yn.Tabsequence = 0
		dw_qa_qastg.Object.reload_indicator_yn.Background.Color = RGB(192, 192, 192)
	ELSE
		dw_qa_qastg.Object.reload_indicator_yn.Tabsequence = 70
	END IF
	
	dw_qa_qastg.Object.qarejcd.Tabsequence = 80
	dw_qa_qastg.Object.qacomments.Tabsequence = 90
	ib_disabled=FALSE
ELSE
	// disable - do we need to gray it out
	dw_qa_qastg.Object.qastg.Background.Color = RGB(192, 192, 192)
	dw_qa_qastg.Object.qarecdt.Background.Color = RGB(192, 192, 192)
	dw_qa_qastg.Object.qacompdt.Background.Color = RGB(192, 192, 192)
	dw_qa_qastg.Object.qastatcd.Background.Color = RGB(192, 192, 192)
	dw_qa_qastg.Object.qainit.Background.Color = RGB(192, 192, 192)
	dw_qa_qastg.Object.review_type_code.Background.Color = RGB(192, 192, 192)
	dw_qa_qastg.Object.reload_indicator_yn.Background.Color = RGB(192, 192, 192)
	
	
	// 10/30/2008 disable rejection code if status is A or I
	ll_row = dw_qa_qastg.getrow()
	ls_status = dw_qa_qastg.object.qastatcd[ll_row]
//	IF ls_status = 'A' OR ls_status = 'I' THEN
//		dw_qa_qastg.Object.qarejcd.Background.Color = RGB(192, 192, 192)
//	ELSE
//		dw_qa_qastg.Object.qarejcd.Background.Color = RGB(255, 255, 255)
//	END IF
//	dw_qa_qastg.Object.qacomments.Background.Color = RGB(192, 192, 192)
	
	dw_qa_qastg.Object.qastg.Tabsequence = 0
	dw_qa_qastg.Object.qarecdt.Tabsequence = 0
	dw_qa_qastg.Object.qacompdt.Tabsequence = 0
	dw_qa_qastg.Object.qastatcd.Tabsequence = 0
	dw_qa_qastg.Object.qainit.Tabsequence = 0
	dw_qa_qastg.Object.review_type_code.Tabsequence = 0
	dw_qa_qastg.Object.reload_indicator_yn.Tabsequence = 0
//	// 10/30/2008
//	IF ls_status = 'A' OR ls_status = 'I' THEN
//		dw_qa_qastg.Object.qarejcd.Tabsequence = 0
//	ELSE
//		dw_qa_qastg.Object.qarejcd.Tabsequence = 80
//	END IF
//	dw_qa_qastg.Object.qacomments.Tabsequence = 0
	ib_disabled=TRUE
END IF
RETURN 1
end function

public function integer of_setreload (string as_stg, long al_row, string as_status);//////////////////////////////////////////////////////////////////////////////////////////////////////////
//
//	Function: of_setreload
// Args: string as_stg, long al_row
//	Description:
//	Based on qa stages set the reload indicator
//
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//	
//	Revision History
//
//	Developed by 	Date 				Version										Tracking#
//									
// Murali K.			02/21/2008      005 PICS Modifications Phase 2	 Reqs: QAS a.8.1, A.8.2
// Murali K.			10/17/2008 		For older book no reload indicator need to be set
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//

// 10/17/2008  for older book no reload indicator need to be set
IF NOT ib_checkedin THEN
	RETURN 1
END IF

// 02/21/2008 Based on QA stages set reload indicators
CHOOSE CASE as_stg
	CASE '1','5' // reload is mandatory explicit
		IF as_status = 'R' THEN // if rejected automatically set reload indicator
			dw_qa_qastg.object.reload_indicator_yn[al_row] = 'Y'
			dw_qa_qastg.object.reload_indicator_yn.Protect =1
		ELSE
			dw_qa_qastg.object.reload_indicator_yn[al_row] = 'N'
			dw_qa_qastg.object.reload_indicator_yn.Protect =0
		END IF
	CASE '6' // choice to the user if they want to set reload indicator
		dw_qa_qastg.object.reload_indicator_yn.Protect =0
		IF as_status = 'R' AND ( NOT ib_rowfocuschanged) THEN
			Messagebox('Warning', 'Please Indicate if Reload is required based on the nature of the problem.')
		END IF
	CASE ELSE
		dw_qa_qastg.object.reload_indicator_yn[al_row] = 'N'
		dw_qa_qastg.object.reload_indicator_yn.Protect =0
END CHOOSE

// 08/18/2008 If status is Approved there is no reload
IF as_status = 'A' THEN
	dw_qa_qastg.object.reload_indicator_yn[al_row] = 'N'
	dw_qa_qastg.object.reload_indicator_yn.Protect =1
END IF


RETURN 1
end function

public function integer of_validateqastg (long al_book, string as_cntr, string as_bkmed, string as_cntrtype, long al_row, string as_qastg);//////////////////////////////////////////////////////////////////////////////////////////////////////////
//
//	Function :  of_validateqastg
// Args		:  long al_book,
//				  string as_cntr ( contract#)
//				 integer ai_len
//                 string as_cntrtype ( contract type)
// RETURNS	: 1 for success, -1 for failure
//	Description:
//  1. Set book media
//	2. Check and validate various qa stages
//  
//
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//	
//	Revision History
//
//	Developed by 	Date 				Version						Tracking#
//									
// Murali K.			03/17/2008      005 PICS Modifications	 Reqs: QAS A.8.1, A.8.2
// Murali K.			03/26/2008		Validate stages based on contract type M or D
// Murali K.			08/01/2008 		Validate blocks of qa stages after a reject and reload
//											Starting point for validation is considered from the record
//											after a reject/reload
// Murali K. 			10/16/2008		 if not checked do not validate - may be A-D book
// Murali K.    		10/23/2008 		Find flash indicator in either one of the RTB record on the header,
//											to determine if the flash is set to validate for QC6
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//

long ll_insert, ll_rows,ll_prevrow
int li_loop
string ls_qastg, ls_status, ls_current_stg, ls_null, ls_reload, ls_current_status
date ld_dt, ld_cur_dt
boolean lb_approved=FALSE
long ll_foundqc0, ll_foundqc1, ll_foundqc5
string ls_find
					
dw_qa_qastg.Accepttext()

ls_current_stg = as_qastg

ls_current_status = 	dw_qa_qastg.object.qastatcd[al_row]

ll_rows = dw_qa_qastg.rowcount()
setnull(ls_null)

// Note : qa stg  record will not be saved if initials, status code not entered
// Historical qa stg records cannot be edited/changed

// Set the book media for the current row based on qa stage
CHOOSE CASE ls_current_stg
	CASE '0'
		dw_qa_qastg.object.bkmed[al_row]='DB'
	CASE '2', '3'
		IF as_bkmed = 'BR' THEN
			dw_qa_qastg.object.bkmed[al_row]='BR'
		ELSE
			dw_qa_qastg.object.bkmed[al_row]='RC'
		END IF
	CASE '1'
		dw_qa_qastg.object.bkmed[al_row]='RC'
	CASE '4','5','6'
		dw_qa_qastg.object.bkmed[al_row]='DB'
	CASE '7'
		dw_qa_qastg.object.bkmed[al_row]=as_bkmed
END CHOOSE

// 10/16/2008 if not checked do not validate qa stages- may be A-D book
IF NOT inv_qa.of_ischeckedin(al_book,as_bkmed) THEN
	RETURN 1
END IF
	
CHOOSE CASE as_bkmed
	CASE 'BR'
			//Allow only one approved QC2 record
			dw_qa_qastg.object.bkmed[al_row]='BR'
			
			li_loop=1
			FOR li_loop = 1 TO ll_rows
				ls_qastg = dw_qa_qastg.object.qastg[li_loop]
				ls_status = dw_qa_qastg.object.qastatcd[li_loop]
				IF ls_qastg = '2' AND (ls_current_stg =  ls_qastg) AND ls_status = 'A'  AND li_loop <> ll_rows THEN
					dw_qa_qastg.object.qastg[al_row] = ls_null
					Messagebox('Error','Approved QC2 already exists')
					RETURN -1
				END IF
			NEXT

			// Valid qa stages for braile 2,3,7		
			CHOOSE CASE ls_current_stg
				CASE '0','1','3', '4','5','6', '7'
					dw_qa_qastg.object.qastg[al_row] = ls_null
					Messagebox('Error','Please enter a valid QA Stage for Braille.')
					RETURN -1
			END CHOOSE

			//  IF QC2 IS REJECTED DO NOT ALLOW QC7
			IF ls_current_stg = '7' THEN
				li_loop=1
				lb_approved=FALSE
				FOR li_loop = 1 TO ll_rows
					ls_qastg = dw_qa_qastg.object.qastg[li_loop]
					ls_status = dw_qa_qastg.object.qastatcd[li_loop]
					IF ls_qastg = '2' AND ls_status = 'A'  AND li_loop <> ll_rows THEN
						lb_approved=TRUE
					END IF
				NEXT
					IF NOT lb_approved THEN
						dw_qa_qastg.object.qastg[al_row] = ls_null
						Messagebox('Error',"An approved QC2 doesn't exist. Cannot add QC7.")
						RETURN -1
					END IF
			END IF		// qc7 check for BR
			
        // process for RC and DB			
		CASE 'RC', 'DB'
				
				IF as_cntrtype = 'M' THEN
					IF ls_current_stg = '2' OR ls_current_stg = '3' THEN
						Messagebox('Warning','QC2 or QC3 not allowed for a Mastering Contract ')
						dw_qa_qastg.object.qastg[al_row] = ls_null
						RETURN -1
					END IF
				END IF

				IF as_cntrtype = 'D' and is_prodstage = 'FC' THEN
					IF ls_current_stg = '2' OR ls_current_stg = '3' THEN
						Messagebox('Warning','QC2 or QC3 not allowed for a Duplication Contract with a production stage as FC')
						dw_qa_qastg.object.qastg[al_row] = ls_null
						RETURN -1
					END IF
				END IF

				IF as_cntrtype = 'D'  THEN
					IF ls_current_stg = '0' OR ls_current_stg = '1'  OR ls_current_stg = '5' THEN
						Messagebox('Warning','For Dupe only contract QC0,QC1,QC5 is not needed')
						dw_qa_qastg.object.qastg[al_row] = ls_null
						RETURN -1
					END IF
				END IF


				// QC0 - Autotest - inserted automatically by messaging
				// Manual QC0 can only be added if it was rejected earlier
				IF ls_current_stg = '0' THEN
						li_loop=1
						lb_approved=FALSE
						FOR li_loop = 1 TO ll_rows
							ls_qastg = dw_qa_qastg.object.qastg[li_loop]
							ls_status = dw_qa_qastg.object.qastatcd[li_loop]
//							IF ls_qastg = '0'  AND ls_status = 'A'   AND li_loop <> ll_rows THEN
//								lb_approved=TRUE
//							END IF
						NEXT
						
//						// do not allow duplicate qc0
//						IF  lb_approved THEN
//							Messagebox('Error','Approved QC0 already  exists. QC0 can be added only fo rejected Autotest')
//							RETURN -1
//						END IF

						// 07/31/2008 Allow manual entry of QC0 approved/rejected even if it was approved earlier
						// Previous qa stg must have been rejected with a reload
						IF ll_rows > 1 THEN
							ll_prevrow = al_row -1
							ls_qastg = dw_qa_qastg.object.qastg[ll_prevrow]
							ls_reload	 = dw_qa_qastg.object.reload_indicator_yn[ll_prevrow]
							ls_status	 = dw_qa_qastg.object.qastatcd[ll_prevrow]
							IF ls_qastg <> '0' AND ls_reload = 'Y' AND ls_status = 'R' THEN
								//ALLOW
							ELSEIF ls_qastg = '0' AND ls_status = 'R' THEN
								// ALLOW
							ELSE
								lb_approved = TRUE
							END IF
							IF  lb_approved THEN
								Messagebox('Error','QC0 can be manually entered only for previously rejected/reload scenarios')
								dw_qa_qastg.object.qastg[al_row] = ls_null
								RETURN -1
							END IF
						END IF
						/// 07/31/2008
						
						dw_qa_qastg.object.review_type_code[al_row] = 'M'
						dw_qa_qastg.Object.review_type_code.Background.Color = RGB(192, 192, 192)
						dw_qa_qastg.Object.review_type_code.Tabsequence = 0
				END IF

			   // QC1 - Narration
				IF ls_current_stg = '1' THEN
//					li_loop=1
					li_loop = ll_rows
					lb_approved=FALSE
					FOR li_loop = ll_rows  TO 1 Step -1
						ls_qastg = dw_qa_qastg.object.qastg[li_loop]
						ls_status = dw_qa_qastg.object.qastatcd[li_loop]
						
						// 08/01/2008 validate blocks of qa stages after a reject and reload
						ls_reload	 = dw_qa_qastg.object.reload_indicator_yn[li_loop]
						IF  ls_reload = 'Y' AND ls_status = 'R' THEN Exit
								
						IF ls_qastg = '0'  AND ls_status = 'A'   AND li_loop <> ll_rows  THEN
							lb_approved=TRUE
						END IF
					NEXT
					// Did not find approved QC0 , so qc1 not allowed
					IF NOT lb_approved THEN
						Messagebox('Error','An Approved QC0 not found. Cannot add QC1.')
						dw_qa_qastg.object.qastg[al_row] = ls_null
						RETURN -1
					END IF

		//				li_loop=1
						li_loop = ll_rows
						lb_approved=FALSE
						FOR li_loop = ll_rows TO 1 Step -1
							ls_qastg = dw_qa_qastg.object.qastg[li_loop]
							ls_status = dw_qa_qastg.object.qastatcd[li_loop]
							
							// 08/01/2008 validate blocks of qa stages after a reject and reload
							ls_reload	 = dw_qa_qastg.object.reload_indicator_yn[li_loop]
							IF  ls_reload = 'Y' AND ls_status = 'R' THEN Exit
						
							IF ls_qastg = '1'  AND ls_status = 'A'   AND li_loop <> ll_rows  AND ls_current_status = 'A' THEN
								lb_approved=TRUE
							END IF
						NEXT
						// do not allow duplicate qc1
						IF  lb_approved THEN
							Messagebox('Warning','Approved QC1 already exists.')
							dw_qa_qastg.object.qastg[al_row] = ls_null
							RETURN -1
						END IF
				END IF


				/********************************************************************* 
				//	QC2 - Braille/Cassette
				//  should we allow duplicate 2 and 3
				//   on what conditions do we allow 2 and 3 for a RC or DB book?
				//   0,1, must be approved 
				 ********************************************************************/
				
				IF ls_current_stg = '2' OR ls_current_stg = '3' THEN			
					// validate 0 and 1 only for a mastering contract not for dupe only
					IF as_cntrtype = 'M' THEN
						// 08/01/2008 validate blocks of qa stages after a reject and reload
						li_loop = ll_rows
						FOR li_loop = ll_rows TO 1 Step -1
							ls_qastg = dw_qa_qastg.object.qastg[li_loop]
							ls_status = dw_qa_qastg.object.qastatcd[li_loop]
							
							ls_reload	 = dw_qa_qastg.object.reload_indicator_yn[li_loop]
							IF  ls_reload = 'Y' AND ls_status = 'R' THEN Exit
						NEXT							
						
						ls_find = "qastg = '0' and qastatcd = 'A'"
						ll_foundqc0 = dw_qa_qastg.Find(ls_find, li_loop, dw_qa_qastg.RowCount())
	
						ls_find = "qastg = '1' and qastatcd = 'A'"
						ll_foundqc1 = dw_qa_qastg.Find(ls_find, li_loop, dw_qa_qastg.RowCount())
				
						IF ll_foundqc0 > 0 AND ll_foundqc1 > 0  THEN
						ELSE
							Messagebox('Error', 'QC0,QC1 needs to be approved to enter QC2 or QC3.')
							dw_qa_qastg.object.qastg[al_row] = ls_null
							RETURN -1
						END IF
					END IF
				END IF
					
				IF ls_current_stg = '2' THEN
					//	li_loop=1
						li_loop = ll_rows
						lb_approved=FALSE						
						FOR li_loop = ll_rows TO 1 Step -1
							ls_qastg = dw_qa_qastg.object.qastg[li_loop]
							ls_status = dw_qa_qastg.object.qastatcd[li_loop]
							
						// 08/01/2008 validate blocks of qa stages after a reject and reload
						ls_reload	 = dw_qa_qastg.object.reload_indicator_yn[li_loop]
						IF  ls_reload = 'Y' AND ls_status = 'R' THEN Exit
						
							IF ls_qastg = '2'  AND ls_status = 'A'   AND li_loop <> ll_rows  AND ls_current_status = 'A' THEN
								lb_approved=TRUE
							END IF
						NEXT
						IF  lb_approved THEN
							Messagebox('Warning','Approved QC2 already  exists')
							dw_qa_qastg.object.qastg[al_row] = ls_null
							RETURN -1
						END IF
				END IF

				// Qc3 - Intermaster
				IF ls_current_stg = '3' THEN
			//			li_loop=1
						li_loop = ll_rows
						lb_approved=FALSE						
						FOR li_loop = ll_rows  TO 1 Step -1
							ls_qastg = dw_qa_qastg.object.qastg[li_loop]
							ls_status = dw_qa_qastg.object.qastatcd[li_loop]
							
							// 08/01/2008 validate blocks of qa stages after a reject and reload
							ls_reload	 = dw_qa_qastg.object.reload_indicator_yn[li_loop]
							IF  ls_reload = 'Y' AND ls_status = 'R' THEN Exit
			
			
							IF ls_qastg = '3'  AND ls_status = 'A'   AND li_loop <> ll_rows  AND ls_current_status = 'A' THEN
								lb_approved=TRUE
							END IF
						NEXT

						IF  lb_approved THEN
							Messagebox('Warning','Approved QC3 already  exists')
							dw_qa_qastg.object.qastg[al_row] = ls_null
							RETURN -1
						END IF
				END IF

					// QC4 = MP3 is a stand alone - validations are limited to only this stage
					IF ls_current_stg = '4' THEN
//						li_loop=1
						li_loop = ll_rows
						lb_approved=FALSE
						FOR li_loop = ll_rows TO 1 Step -1
							ls_qastg = dw_qa_qastg.object.qastg[li_loop]
							ls_status = dw_qa_qastg.object.qastatcd[li_loop]
							
							// 08/01/2008 validate blocks of qa stages after a reject and reload
							ls_reload	 = dw_qa_qastg.object.reload_indicator_yn[li_loop]
							IF  ls_reload = 'Y' AND ls_status = 'R' THEN Exit
			
			
							IF ls_qastg = '4'  AND ls_status = 'A'   AND li_loop <> ll_rows  AND ls_current_status = 'A' THEN
								lb_approved=TRUE
							END IF
						NEXT

						IF  lb_approved THEN
							Messagebox('Warning','Approved QC4 already  exists')
							dw_qa_qastg.object.qastg[al_row] = ls_null
							RETURN -1
						END IF
				END IF

				// QC5 - AMR/DRM
				IF ls_current_stg = '5' THEN
						// 08/01/2008 validate blocks of qa stages after a reject and reload					
						li_loop = ll_rows
						FOR li_loop = ll_rows TO 1 Step -1
							ls_status = dw_qa_qastg.object.qastatcd[li_loop]
							ls_reload	 = dw_qa_qastg.object.reload_indicator_yn[li_loop]
							IF  ls_reload = 'Y' AND ls_status = 'R' THEN Exit
						NEXT
						
						//
						ls_find = "qastg = '0' and qastatcd = 'A'"
						ll_foundqc0 = dw_qa_qastg.Find(ls_find, li_loop, dw_qa_qastg.RowCount())
	
						ls_find = "qastg = '1' and qastatcd = 'A'"
						ll_foundqc1 = dw_qa_qastg.Find(ls_find, li_loop, dw_qa_qastg.RowCount())
				
						IF ll_foundqc0 > 0 AND ll_foundqc1 > 0  THEN
						ELSE
							Messagebox('Error', 'QC0,QC1 needs to be approved to enter QC5.')
							dw_qa_qastg.object.qastg[al_row] = ls_null							
							RETURN -1
						END IF
					
//					li_loop=1
					li_loop = ll_rows
					lb_approved=FALSE
					FOR li_loop = ll_rows TO 1 Step -1
						ls_qastg = dw_qa_qastg.object.qastg[li_loop]
						ls_status = dw_qa_qastg.object.qastatcd[li_loop]
						
						// 08/01/2008 reload/rejected block validations
						ls_reload	 = dw_qa_qastg.object.reload_indicator_yn[li_loop]
						IF  ls_reload = 'Y' AND ls_status = 'R' THEN Exit

						IF ls_qastg = '5'  AND ls_status = 'A'   AND li_loop <> ll_rows  AND ls_current_status = 'A' THEN
							lb_approved=TRUE
						END IF
					NEXT
					
					// QC5 already approved do not allow duplicates
					IF lb_approved THEN
						Messagebox('Warning','Approved QC5 already exists')
						dw_qa_qastg.object.qastg[al_row] = ls_null
						RETURN -1
					END IF
				END IF
	
				// QC6 processing
				IF ls_current_stg = '6'  AND  dw_qa_prod_review.object.flash_indicator[1] = 'Y'  THEN
					
					// 0,1,5 must be approved
					// 03/26/2008 0,1,5 NEEDED ONLY FOR MASTERING NOT FOR DUPE ONLY
					IF as_cntrtype = 'M' THEN
						// 08/01/2008 validate blocks of qa stages after a reject and reload					
						li_loop = ll_rows
						FOR li_loop = ll_rows TO 1 Step -1
							ls_status = dw_qa_qastg.object.qastatcd[li_loop]
							ls_reload	 = dw_qa_qastg.object.reload_indicator_yn[li_loop]
							IF  ls_reload = 'Y' AND ls_status = 'R' THEN Exit
						NEXT
						
						ls_find = "qastg = '0' and qastatcd = 'A'"
						ll_foundqc0 = dw_qa_qastg.Find(ls_find, li_loop, dw_qa_qastg.RowCount())
	
						ls_find = "qastg = '1' and qastatcd = 'A'"
						ll_foundqc1 = dw_qa_qastg.Find(ls_find, li_loop, dw_qa_qastg.RowCount())
	
						ls_find = "qastg = '5' and qastatcd = 'A'"
						ll_foundqc5 = dw_qa_qastg.Find(ls_find, li_loop, dw_qa_qastg.RowCount())
						
						IF ll_foundqc0 > 0 AND ll_foundqc1 > 0 AND ll_foundqc5 > 0 THEN
						ELSE
							Messagebox('Error', 'QC0,QC1,QC5 needs to be approved before entering QC6.')
							dw_qa_qastg.object.qastg[al_row] = ls_null
							RETURN -1
						END IF
						
						// 08/01/2008 reload/rejected block validations
						li_loop = ll_rows
						lb_approved=FALSE
						FOR li_loop = ll_rows TO 1 Step -1
							ls_qastg = dw_qa_qastg.object.qastg[li_loop]
							ls_status = dw_qa_qastg.object.qastatcd[li_loop]
							
							
							ls_reload	 = dw_qa_qastg.object.reload_indicator_yn[li_loop]
							IF  ls_reload = 'Y' AND ls_status = 'R' THEN Exit
	
							IF ls_qastg = '6'  AND ls_status = 'A'   AND li_loop <> ll_rows  AND ls_current_status = 'A' THEN
								lb_approved=TRUE
							END IF
						NEXT
					
					// QC5 already approved do not allow duplicates
					IF lb_approved THEN
						Messagebox('Warning','Approved QC6 already exists')
						dw_qa_qastg.object.qastg[al_row] = ls_null						
						RETURN -1
					END IF
					
					END IF
					
					IF as_cntrtype = 'D' and is_prodstage = 'DU' THEN
						Messagebox('Error', 'QC6 is not needed, since it is a duplication contract and production stage is duplication')
						dw_qa_qastg.object.qastg[al_row] = ls_null
						RETURN -1
					END IF
				ELSE
					// 10/23/2008 find flash indicator in either one of the RTB record on the header
						long ll_foundflash
						ls_find = "flash_indicator = 'Y'"
						ll_foundflash = dw_qa_prod_review.Find(ls_find, 1, dw_qa_prod_review.RowCount())
		
						IF  ls_current_stg = '6'  AND  ll_foundflash = 0   THEN
							dw_qa_qastg.object.qastg[al_row] = ls_null
							Messagebox('Error', 'Flash indicator not selected to do QC6')
							RETURN -1
						END IF
				END IF

				// QC7 - Field Reject
				IF ls_current_stg = '7' THEN
					
					// 0,1,5 must be approved
					// code repeat fine tune
					// 03/27/2008 validate only if mastering contract
					IF as_cntrtype = 'M' THEN
						// 08/01/2008 validate blocks of qa stages after a reject and reload					
						li_loop = ll_rows
						FOR li_loop = ll_rows TO 1 Step -1
							ls_status = dw_qa_qastg.object.qastatcd[li_loop]
							ls_reload	 = dw_qa_qastg.object.reload_indicator_yn[li_loop]
							IF  ls_reload = 'Y' AND ls_status = 'R' THEN Exit
						NEXT
						
						ls_find = "qastg = '0' and qastatcd = 'A'"
						ll_foundqc0 = dw_qa_qastg.Find(ls_find, li_loop, dw_qa_qastg.RowCount())
	
						ls_find = "qastg = '1' and qastatcd = 'A'"
						ll_foundqc1 = dw_qa_qastg.Find(ls_find, li_loop, dw_qa_qastg.RowCount())
	
						ls_find = "qastg = '5' and qastatcd = 'A'"
						ll_foundqc5 = dw_qa_qastg.Find(ls_find, li_loop, dw_qa_qastg.RowCount())
						
						IF ll_foundqc0 > 0 AND ll_foundqc1 > 0 AND ll_foundqc5 > 0 THEN
						ELSE
							Messagebox('Error', 'QC0,QC1,QC5 needs to be approved before entering QC7.')
							dw_qa_qastg.object.qastg[al_row] = ls_null							
							RETURN -1
						END IF
					END IF
					
//					li_loop=1
					li_loop = ll_rows
					lb_approved=FALSE
					FOR li_loop = ll_rows TO 1 Step -1
						ls_qastg = dw_qa_qastg.object.qastg[li_loop]
						ls_status = dw_qa_qastg.object.qastatcd[li_loop]
						
						// 08/01/2008 reload/rejected block validations
						ls_reload	 = dw_qa_qastg.object.reload_indicator_yn[li_loop]
						IF  ls_reload = 'Y' AND ls_status = 'R' THEN Exit
						
						IF ls_qastg = '7'  AND ls_status = 'A'   AND li_loop <> ll_rows  AND ls_current_status = 'A' THEN
							lb_approved=TRUE
						END IF
					NEXT
					IF lb_approved THEN
						Messagebox('Error','Approved QC7 already exists.')
						dw_qa_qastg.object.qastg[al_row] = ls_null
						RETURN -1
					END IF
					
				END IF


END CHOOSE

RETURN 1
end function

protected function integer wf_calculate_units (string media, string cntrtype, integer len, integer qnty, integer minlastside, integer volumn, long al_row);int li_mchar_len

Long units,subunits, ll_length,lbkseq
Int vols
String lmed,lcntrtype,lbkmed,lflash_indicator, lcntr


lmed = Trim(media)
lbkseq = dw_qa_prod_review.object.prod_bkseq[al_row]
lbkmed = dw_qa_prod_review.object.mchar_bkmed[al_row] // al_row introduced 12/03/2008 getting wrong book media

//IF lbkmed = 'RC' THEN
//	lbkmed = 'DB'
//ELSEIF 	lbkmed = 'DB' THEN
//	lbkmed = 'RC'
//ELSE
//	lbkmed='BR'
//END IF
//

ll_length = dw_qa_prod_review.object.mchar_length_[al_row]
lcntrtype = cntrtype
//11/25/2008 Units/sub units not getting updated Tracker Item 2121
// lncntr not getting to the function
Lcntr = dw_qa_prod_review.object.ancntr_cntr[al_row]

//SELECT cntrtype INTO :lcntrtype FROM ancntr
//	where cntr = :Lcntr
//	AND cntrmed = :lmed
//	USING SqlServerTrans;
//	
//Messagebox("cntrtype",Lcntrtype)

IF lcntrtype = 'T' THEN
	IF lmed = 'RC' THEN // not used
		// First calculate the duplication units for Cassettes
		vols = volumn // Ceiling(len/4) 06/19/2008
		units = vols * qnty
		
		// Update the prod table for duplication of this RC book with
		// the correct number of units.
		
		// Messagebox("output","bkseq = "+string(Lbkseq))
		UPDATE prod set units = :units, subunits = :vols
			where bkseq = :Lbkseq
			AND	bkmed = :lmed
			AND	cntr 	= :Lcntr
			AND	prodstage = 'DU'
		USING SqlServerTrans;
		IF f_check_dberror(SqlServerTrans,"PROD")=FALSE THEN
			RETURN -1
		END IF
		
		// Secondly calculate the mastering units for cassettes
		IF IsNull(minlastside)=FALSE THEN
			units = ((len - 1) * 88) + minlastside
		ELSE
			units = ((len - 1) * 88)
		END IF
		subunits = Ceiling(len/2)
		
		UPDATE prod set units = :units,subunits = :subunits
			where bkseq = :Lbkseq
			AND	bkmed = :lmed
			AND	cntr 	= :Lcntr
			AND	prodstage in ('MA','AB')
		USING SqlServerTrans;
		IF f_check_dberror(SqlServerTrans,"PROD")=FALSE THEN
			RETURN -1
		END IF
ELSEIF lmed = 'RTB' THEN

		// First calculate the duplication units for Cassettes
		vols = volumn // Ceiling(len/4) 06/19/2008
		units = vols * qnty

		// 06/19/2008
		IF lbkmed = 'RC' THEN
			// Update the prod table for duplication of this RC book with
			// the correct number of units.
			UPDATE prod set units = :units, subunits = :vols
				where bkseq = :Lbkseq
				AND	bkmed = 'RC'
				AND	cntr 	= :Lcntr
				AND	prodstage = 'DU'
			USING SqlServerTrans;
			// Messagebox("output","bkseq = "+string(Lbkseq))
			IF f_check_dberror(SqlServerTrans,"RTB - Duplication for PROD")=FALSE THEN
				RETURN -1
			END IF
		END IF // 06/19/2008 ONLY FOR RC
		
		// Update the prod table for duplication of this DB book with
		// the correct number of units.
		
		// 06/19/2008
		IF lbkmed = 'DB' THEN
			
			UPDATE prod set units = :units, subunits = :volumn // 12/4/2008 set subunits to vols
				where bkseq = :Lbkseq
				AND	bkmed = 'DB'
				AND	cntr 	= :Lcntr
				AND	prodstage = 'FC'
			USING SqlServerTrans;
			
			// Messagebox("output","bkseq = "+string(Lbkseq))
			IF f_check_dberror(SqlServerTrans,"RTB - Duplication for PROD")=FALSE THEN
				RETURN -1
			END IF
			
		END IF
		
		// Secondly calculate the mastering units for cassettes
		// 06/19/2008
		IF lbkmed = 'RC' THEN
		
				IF IsNull(minlastside)=FALSE THEN
					units = ll_length // ((len - 1) * 88) + minlastside // 06/19/2008
				ELSE
					units = ll_length // ((len - 1) * 88)
				END IF
		//		subunits = len
				subunits = 	dw_qa_prod_review.object.mchar_len[al_row]	
				
				UPDATE prod set units = :units,subunits = :subunits
					where bkseq = :Lbkseq
					AND	bkmed = :lbkmed
					AND	cntr 	= :Lcntr
					AND	prodstage in ('MA','AB')
				USING SqlServerTrans;
				IF f_check_dberror(SqlServerTrans,"RTB - Mastering for PROD")=FALSE THEN
					RETURN -1
				END IF
			END IF			
		
		// Thirdly calculate the Z mastering units for cassettes
		IF IsNull(minlastside)=FALSE THEN
			units = ll_length // ((len - 1) * 88) + minlastside
		ELSE
			units = ll_length // ((len - 1) * 88)
		END IF
		subunits = 1
		
		UPDATE prod set units = :units,subunits = :subunits
			where bkseq = :Lbkseq
			AND	bkmed = 'DB'
			AND	cntr 	= :Lcntr
			AND	prodstage = 'ZM'
		USING SqlServerTrans;
		IF f_check_dberror(SqlServerTrans,"RTB - Z Mastering for PROD")=FALSE THEN
			RETURN -1
		END IF

	ELSEIF lmed = 'BR' THEN
		// First calculate the duplication units for Braille
		li_mchar_len = dw_qa_prod_review.object.mchar_len[al_row]	
		units = li_mchar_len * qnty // len * qnty // 06/19/2008
		vols = volumn
		//Messagebox("data2","Vols ="+string(vols)+"qnty ="+string(qnty))
		subunits = vols * qnty
		
		// Update the prod table for duplication of this BR book with
		// the correct number of units.
		
		UPDATE prod set units = :units,subunits = :subunits
			where bkseq = :Lbkseq
			AND	bkmed = 'BR'
			AND	cntr 	= :Lcntr
			AND	prodstage in ('PR','EM')
		USING SqlServerTrans;
		IF f_check_dberror(SqlServerTrans,"PROD")=FALSE THEN
			RETURN -1
		END IF
		// Secondly calculate the mastering units for Braille
		units = li_mchar_len // len
		UPDATE prod set units = :units
			where bkseq = :Lbkseq
			AND	bkmed = 'BR'
			AND	cntr 	= :Lcntr
			AND	prodstage in ('MA','AB')
		USING SqlServerTrans;
		IF f_check_dberror(SqlServerTrans,"PROD")=FALSE THEN
			RETURN -1
		END IF
	ELSEIF lmed = 'P/B' THEN
		// First calculate the duplication units for Braille
		units = qnty
		
		// Update the prod table for duplication of this Print Braille book with
		// the correct number of units.
		
		UPDATE prod set units = :units
			where bkseq = :Lbkseq
			AND	bkmed = 'BR'
			AND	cntr 	= :Lcntr
			AND	prodstage in ('PB','EM')
		USING SqlServerTrans;
		IF f_check_dberror(SqlServerTrans,"PROD")=FALSE THEN
			RETURN -1
		END IF
		// Secondly calculate the mastering units for Print Braille
		units = qnty
		UPDATE prod set units = :units
			where bkseq = :Lbkseq
			AND	bkmed = 'BR'
			AND	cntr 	= :Lcntr
			AND	prodstage = 'PU'
		USING SqlServerTrans;
		IF f_check_dberror(SqlServerTrans,"PROD")=FALSE THEN
			RETURN -1
		END IF
	END IF
//------------------------------------------------ Not a Total Contract --	
ELSEIF  lcntrtype <> 'T' THEN
	IF lcntrtype = 'M' THEN
		IF lmed = 'RTB' THEN // was  'RC' before 12/4/2008
//			// Calculate the mastering units for cassettes
//			IF IsNull(minlastside)=FALSE THEN
//				units = ((len - 1) * 88) + minlastside
//			ELSE
//				units = ((len - 1) * 88)
//			END IF
//			subunits = Ceiling(len/2)
//			UPDATE prod set units = :units, subunits = :subunits
//				where bkseq = :Lbkseq
//				AND	bkmed = :lmed
//				AND	cntr 	= :Lcntr
//				AND	prodstage in ('MA','AB')
//			USING SqlServerTrans;
//			IF f_check_dberror(SqlServerTrans,"PROD")=FALSE THEN
//				RETURN -1
//			END IF
		// Secondly calculate the mastering units for cassettes
		// 06/19/2008
		IF lbkmed = 'RC' THEN
		
				IF IsNull(minlastside)=FALSE THEN
					units = ll_length // ((len - 1) * 88) + minlastside // 06/19/2008
				ELSE
					units = ll_length // ((len - 1) * 88)
				END IF

				subunits = 	dw_qa_prod_review.object.mchar_len[al_row]	
				
				UPDATE prod set units = :units,subunits = :subunits
					where bkseq = :Lbkseq
					AND	bkmed = :lbkmed
					AND	cntr 	= :Lcntr
					AND	prodstage in ('MA','AB')
				USING SqlServerTrans;
				IF f_check_dberror(SqlServerTrans,"RTB - Mastering for PROD")=FALSE THEN
					RETURN -1
				END IF
			END IF			
		
		// Thirdly calculate the Z mastering units for cassettes
		IF IsNull(minlastside)=FALSE THEN
			units = ll_length // ((len - 1) * 88) + minlastside
		ELSE
			units = ll_length // ((len - 1) * 88)
		END IF
		subunits = 1
		
		UPDATE prod set units = :units,subunits = :subunits
			where bkseq = :Lbkseq
			AND	bkmed = 'DB'
			AND	cntr 	= :Lcntr
			AND	prodstage = 'ZM'
		USING SqlServerTrans;
		IF f_check_dberror(SqlServerTrans,"RTB - Z Mastering for PROD")=FALSE THEN
			RETURN -1
		END IF

  	ELSEIF lmed = 'BR' THEN
			// Calculate the mastering units for Braille
			li_mchar_len = dw_qa_prod_review.object.mchar_len[al_row]	
			units = li_mchar_len // len
			UPDATE prod set units = :units
				where bkseq = :Lbkseq
				AND	bkmed = 'BR'
				AND	cntr 	= :Lcntr
				AND	prodstage in ('MA','AB')
			USING SqlServerTrans;
			IF f_check_dberror(SqlServerTrans,"PROD")=FALSE THEN
				RETURN -1
			END IF
		ELSEIF lmed = 'P/B'	THEN
			// Calculate the mastering units for Print Braille
			units = qnty
			UPDATE prod set units = :units
				where bkseq = :Lbkseq
				AND	bkmed = 'BR'
				AND	cntr 	= :Lcntr
				AND	prodstage = 'PU'
			USING SqlServerTrans;
			IF f_check_dberror(SqlServerTrans,"PROD")=FALSE THEN
				RETURN -1
			END IF
		END IF
//-------------- Duplication contract
	ELSEIF lcntrtype = 'D' THEN
		IF lmed = 'RTB' THEN // was 'RC' before 12/4/2008

//			// Calculate the duplication units for cassettes
//			IF lbkmed = 'RC' THEN
//				vols = volumn // Ceiling(len/4)
//				units = vols * qnty
//				// Update the prod table for duplication of this RC book with
//				// the correct number of units.
//				UPDATE prod set units = :units, subunits = :vols
//					where bkseq = :Lbkseq
//					AND	bkmed = 'RC'
//					AND	cntr 	= :Lcntr
//					AND	prodstage = 'DU'
//				USING SqlServerTrans;
//				// Messagebox("output","bkseq = "+string(Lbkseq))
//				IF f_check_dberror(SqlServerTrans,"RTB - Duplication for PROD")=FALSE THEN
//					RETURN -1
//				END IF
//				
//			ELSEIF lbkmed = 'DB' THEN
//				units = 1
//				// Update the prod table for duplication of this DB book with
//				// the correct number of units.
//				UPDATE prod set units = :units, subunits = :vols
//					where bkseq = :Lbkseq
//					AND	bkmed = 'DB'
//					AND	cntr 	= :Lcntr
//					AND	prodstage = 'FC'
//				USING SqlServerTrans;
//				// Messagebox("output","bkseq = "+string(Lbkseq))
//				IF f_check_dberror(SqlServerTrans,"RTB - Duplication for PROD")=FALSE THEN
//					RETURN -1
//				END IF
//				
//			END IF		
		// First calculate the duplication units for Cassettes
		vols = volumn // Ceiling(len/4) 06/19/2008
		units = vols * qnty

		// 06/19/2008
		IF lbkmed = 'RC' THEN
			// Update the prod table for duplication of this RC book with
			// the correct number of units.
			UPDATE prod set units = :units, subunits = :vols
				where bkseq = :Lbkseq
				AND	bkmed = 'RC'
				AND	cntr 	= :Lcntr
				AND	prodstage = 'DU'
			USING SqlServerTrans;
			// Messagebox("output","bkseq = "+string(Lbkseq))
			IF f_check_dberror(SqlServerTrans,"RTB - Duplication for PROD")=FALSE THEN
				RETURN -1
			END IF
		END IF // 06/19/2008 ONLY FOR RC
		
		// Update the prod table for duplication of this DB book with
		// the correct number of units.
		
		// 06/19/2008
		IF lbkmed = 'DB' THEN
			
			UPDATE prod set units = :units, subunits = :volumn // 12/4/2008 set subunits to vols
				where bkseq = :Lbkseq
				AND	bkmed = 'DB'
				AND	cntr 	= :Lcntr
				AND	prodstage = 'FC'
			USING SqlServerTrans;
			
			// Messagebox("output","bkseq = "+string(Lbkseq))
			IF f_check_dberror(SqlServerTrans,"RTB - Duplication for PROD")=FALSE THEN
				RETURN -1
			END IF
			
		END IF

	ELSEIF lmed = 'BR' THEN
			// Calculate the duplication units for Braille
			li_mchar_len = dw_qa_prod_review.object.mchar_len[al_row]	
			units = li_mchar_len  * qnty
			vols = volumn
			subunits = vols * qnty
			UPDATE prod set units = :units,subunits = :subunits
				where bkseq = :Lbkseq
				AND	bkmed = 'BR'
				AND	cntr 	= :Lcntr
				AND	prodstage in ('PR','EM')
			USING SqlServerTrans;
			IF f_check_dberror(SqlServerTrans,"PROD")=FALSE THEN
				RETURN -1
			END IF
		ELSEIF lmed = 'P/B'	THEN
			// Calculate the duplication units for Print Braille
			units = qnty
			UPDATE prod set units = :units
				where bkseq = :Lbkseq
				AND	bkmed = 'BR'
				AND	cntr 	= :Lcntr
				AND	prodstage in ('PB','EM')
			USING SqlServerTrans;
			IF f_check_dberror(SqlServerTrans,"PROD")=FALSE THEN
				RETURN -1
			END IF
		ELSEIF lmed = 'FD' THEN
			// Calculate the duplication units for Formated Disk
			vols = volumn // Ceiling(len/2) 06/19/2008
			units = vols * qnty
			UPDATE prod set units = :units
				where bkseq = :Lbkseq
				AND	bkmed = :lmed
				AND	cntr 	= :Lcntr
				AND	prodstage = 'DU'
			USING SqlServerTrans;
			IF f_check_dberror(SqlServerTrans,"PROD")=FALSE THEN
				RETURN -1
			END IF
		END IF
	END IF	
END IF
COMMIT USING SqlServerTrans;
RETURN units

end function

event open;call super::open;// Open the sheet in Maximized mode
this.windowstate = maximized!
em_bkno.SetFocus()

end event

on w_qa_product_review.create
int iCurrent
call super::create
this.st_3=create st_3
this.mle_1=create mle_1
this.cb_spellcheck=create cb_spellcheck
this.dw_inhousecomments=create dw_inhousecomments
this.cb_updateweb=create cb_updateweb
this.cb_add_narr=create cb_add_narr
this.cb_comments=create cb_comments
this.st_4=create st_4
this.cb_find=create cb_find
this.cb_exit=create cb_exit
this.cb_clear=create cb_clear
this.cb_update=create cb_update
this.st_1=create st_1
this.em_bkno=create em_bkno
this.dw_qa_qastg=create dw_qa_qastg
this.dw_qa_narr=create dw_qa_narr
this.st_2=create st_2
this.cb_narr=create cb_narr
this.dw_qa_cntr=create dw_qa_cntr
this.dw_qa_prod_review=create dw_qa_prod_review
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_3
this.Control[iCurrent+2]=this.mle_1
this.Control[iCurrent+3]=this.cb_spellcheck
this.Control[iCurrent+4]=this.dw_inhousecomments
this.Control[iCurrent+5]=this.cb_updateweb
this.Control[iCurrent+6]=this.cb_add_narr
this.Control[iCurrent+7]=this.cb_comments
this.Control[iCurrent+8]=this.st_4
this.Control[iCurrent+9]=this.cb_find
this.Control[iCurrent+10]=this.cb_exit
this.Control[iCurrent+11]=this.cb_clear
this.Control[iCurrent+12]=this.cb_update
this.Control[iCurrent+13]=this.st_1
this.Control[iCurrent+14]=this.em_bkno
this.Control[iCurrent+15]=this.dw_qa_qastg
this.Control[iCurrent+16]=this.dw_qa_narr
this.Control[iCurrent+17]=this.st_2
this.Control[iCurrent+18]=this.cb_narr
this.Control[iCurrent+19]=this.dw_qa_cntr
this.Control[iCurrent+20]=this.dw_qa_prod_review
end on

on w_qa_product_review.destroy
call super::destroy
destroy(this.st_3)
destroy(this.mle_1)
destroy(this.cb_spellcheck)
destroy(this.dw_inhousecomments)
destroy(this.cb_updateweb)
destroy(this.cb_add_narr)
destroy(this.cb_comments)
destroy(this.st_4)
destroy(this.cb_find)
destroy(this.cb_exit)
destroy(this.cb_clear)
destroy(this.cb_update)
destroy(this.st_1)
destroy(this.em_bkno)
destroy(this.dw_qa_qastg)
destroy(this.dw_qa_narr)
destroy(this.st_2)
destroy(this.cb_narr)
destroy(this.dw_qa_cntr)
destroy(this.dw_qa_prod_review)
end on

event pfc_preopen;call super::pfc_preopen;string Luserid
Luserid = SQLserverTrans.userid

this.of_SetPreference(TRUE)
this.inv_preference.of_SetToolBars(TRUE)
this.inv_preference.of_SetWindow(TRUE)
this.of_SetResize(TRUE)
this.inv_resize.of_SetOrigSize(this.workspacewidth(),this.workspaceheight())


inv_resize.of_Register(dw_qa_narr, "Scale")
inv_resize.of_Register(dw_qa_cntr, "Scale")
//inv_resize.of_Register(dw_qa_recagcy, "Scale")
inv_resize.of_Register(dw_qa_prod_review, "Scale")
inv_resize.of_Register(dw_qa_qastg, "Scale")
inv_resize.of_Register(cb_clear, "Scale")
inv_resize.of_Register(cb_exit, "Scale")
inv_resize.of_Register(cb_find, "Scale")
inv_resize.of_Register(cb_update, "Scale")
inv_resize.of_Register(cb_updateweb, "Scale")
inv_resize.of_Register(cb_comments, "Scale")
inv_resize.of_Register(cb_narr, "Scale")
inv_resize.of_Register(cb_add_narr, "Scale")
inv_resize.of_Register(st_1, "Scale")
inv_resize.of_Register(st_2, "Scale")
//inv_resize.of_Register(st_3, "Scale")
inv_resize.of_Register(st_4, "Scale")
inv_resize.of_Register(em_bkno, "Scale")

// 02/20/2008
inv_resize.of_Register(dw_inhousecomments, "Scale")
inv_resize.of_Register(cb_spellcheck, "Scale")
inv_resize.of_Register(mle_1, "Scale")
inv_resize.of_Register(st_3, "Scale")

/*
IF TRIM(Luserid) <> "dsmi" AND TRIM(Luserid) <> "tmcl" THEN
	cb_narr.visible = FALSE
	cb_comments.visible = FALSE
	cb_add_narr.visible = FALSE
ELSE
*/
	cb_narr.visible = TRUE
	cb_comments.visible = TRUE
	cb_add_narr.visible = TRUE
//END IF
	
// 08/27/2008 book server status datastore initialization
ids_book_status = create datastore
ids_book_status.dataobject = 'd_qa_book_server_status_qrs'
ids_book_status.settransobject(sqlservertrans)




end event

event resize;call super::resize;long ll_height

This.X = w_pics_main.X
This.Y = w_pics_main.Y
ll_height = w_pics_main.mdi_1.Height
This.Resize(w_pics_main.WorkSpaceWidth(), ll_height)

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
	Return 0
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

type st_3 from statictext within w_qa_product_review
integer x = 1024
integer y = 884
integer width = 443
integer height = 44
integer textsize = -8
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 33554432
long backcolor = 67108864
string text = "In House Comments"
boolean focusrectangle = false
end type

type mle_1 from multilineedit within w_qa_product_review
integer x = 1029
integer y = 932
integer width = 654
integer height = 208
integer taborder = 60
integer textsize = -8
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long backcolor = 1090519039
string text = "none"
boolean vscrollbar = true
integer limit = 4000
borderstyle borderstyle = stylelowered!
boolean hideselection = false
end type

type cb_spellcheck from u_cb within w_qa_product_review
integer x = 1399
integer y = 1148
integer width = 288
integer taborder = 90
string text = "Spellcheck"
end type

event clicked;call super::clicked;//////////////////////////////////////////////////////////////////////////////////////////////////////////
//
//	Event: clicked for cb_spellcheck
//
//	Description:
//	Spellcheck for In house comments
//
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//	
//	Revision History
//
//	Developed by 	Date 				Version						Tracking#
//									
// Murali K.			02/20/2008      005 PICS Modifications	 Reqs: QAS a.8.1
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//

nca_word lnca_word
String ls_S
	
// Check for any pending updates
IF of_UpdateChecks( ) < 0 THEN Return -1
	
dw_inhousecomments.accepttext()
// in house comments
ls_S = mle_1.text //  dw_inhousecomments.object.inhouse_comments[1]
IF NOT(IsNull(ls_S)) THEN
	lnca_Word.SpellCheck( ls_S )
	mle_1.text = ls_s
	 dw_inhousecomments.object.inhouse_comments[1] = ls_S 
END IF

  

end event

type dw_inhousecomments from u_pics_dw within w_qa_product_review
boolean visible = false
integer x = 1015
integer y = 888
integer width = 681
integer height = 244
integer taborder = 120
string dataobject = "d_qa_in_house_comments"
end type

event ue_postconstructor;call super::ue_postconstructor;//////////////////////////////////////////////////////////////////////////////////////////////////////////
//
//	Event: ue_postconstructor
//
//	Description:
//	Set transaction object, insert a row and set default rows
//
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//	
//	Revision History
//
//	Developed by 	Date 				Version							Tracking#
//									
// Murali K.			02/20/2008      005 PICS Modifications 2.0	 Reqs: QAS A.8.1, A.8.2
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//
This.SetTransObject(sqlservertrans) 
//This.Event pfc_insertrow()
end event

type cb_updateweb from u_cb within w_qa_product_review
string tag = "Update Web records"
boolean visible = false
integer x = 1367
integer y = 1156
integer taborder = 0
boolean bringtotop = true
boolean enabled = false
string text = "Update &Web"
end type

event clicked;call super::clicked;integer rtn,i,qacnt,lcnt,lans,Lapplen,Lqnty,Llen,Lvols
string Lnarr,Lnarrfn,Lmed,Lqastatcd,Lcntr,Lqastg,Lbkno,Lprdr,Lcntrtype,Luserid,Lsubprdr,Lqacomments
long Lbkseq,Lminlastside
date Lqarecdt, Lqacompdt

Lbkseq = long(em_bkno.text)
Lmed = dw_qa_prod_review.object.prod_bkmed[1]
Lbkno = Lmed+string(Lbkseq)
Lcntr = dw_qa_cntr.object.cntr[1]
Lcntrtype = dw_qa_prod_review.object.ancntr_cntrtype[1]
Lprdr = dw_qa_prod_review.object.ancntr_prdr[1]
Lapplen = dw_qa_prod_review.object.mchar_applen[1]
Lqnty = dw_qa_prod_review.object.mchar_qnty[1]
Llen = dw_qa_prod_review.object.mchar_len[1]
Lvols = dw_qa_prod_review.object.mchar_vols[1]
Lminlastside = dw_qa_prod_review.object.mchar_minlastside[1]
Luserid = SQLserverTrans.userid

IF not SQLServerTrans.DBHandle() >0 THEN
	SQLServerTrans.of_connect() 
END IF

IF not SQLServerOracleTrans.DBHandle() >0 THEN
	SQLServerOracleTrans.of_connect() 
END IF
/*
IF TRIM(Luserid) <> "dsmi" AND TRIM(Luserid) <> "tmcl" THEN
	MessageBox("ERROR Updating WEB","Userid "+Luserid+" is not allowed to update WEB Screens." )	
	RETURN 
ELSE
*/	
	rtn = dw_qa_prod_review.AcceptText()
	IF rtn = 1 THEN
/*		
		SetMicroHelp(w_pics_main,"Updating PRDRBK the WEB Please Wait...")

		UPDATE PRDRBK@pic_link
		SET 	APPLEN = :Lapplen,
				LEN = :Llen,
				MINLASTSIDE = :Lminlastside,
				VOLS = :Lvols,
				QNTY = :Lqnty,
				WEB_UPD = NULL
		WHERE BKMED = :Lmed
		AND BKSEQ = :Lbkseq
		USING SQLServerTrans;
		IF f_check_dberror(SQLServerTrans, "Updating PRDRBK")=FALSE THEN
			ROLLBACK USING sqlservertrans;
			RETURN
		ELSE
*/
			COMMIT USING SQLServerTrans;
			SetMicroHelp(w_pics_main,"Database Updated.")
			cb_clear.Event clicked()			
		END IF
//	END IF
			
	dw_qa_qastg.SetFocus()
//END IF

end event

event getfocus;call super::getfocus;IF this.tag <> "" THEN
	SetMicroHelp(w_pics_main,this.tag)
ELSE
	SetMicroHelp(w_pics_main,"")
END IF

end event

type cb_add_narr from u_cb within w_qa_product_review
string tag = "Add or Update books narrator"
boolean visible = false
integer x = 1728
integer y = 1008
integer width = 617
integer taborder = 0
string text = "Add/Update book~'s narrator..."
end type

event clicked;call super::clicked;long lbkseq
string lbkmed,lrecagcy

str_qa_add_narr lstr_qa_add_narr

lstr_qa_add_narr.bkseq = dw_qa_prod_review.object.prod_bkseq[1]
lstr_qa_add_narr.bkmed = dw_qa_prod_review.object.prod_bkmed[1]
//lstr_qa_add_narr.recagcy = dw_qa_recagcy.object.recagcy[1]

openwithparm(w_qa_add_narr_for_book,lstr_qa_add_narr)

// 03/21/2008 why do we need to find again . this wipes out the entire qa stages entry. just retrieve narrator
//cb_find.TriggerEvent(Clicked!) 
//IF Lmed="RC" OR (Lmed="DB" AND Lcntrtype <> 'A') THEN
 dw_qa_narr.Retrieve(lstr_qa_add_narr.bkseq)
//END IF

end event

event getfocus;call super::getfocus;IF this.tag <> "" THEN
	SetMicroHelp(w_pics_main,this.tag)
ELSE
	SetMicroHelp(w_pics_main,"")
END IF

end event

type cb_comments from u_cb within w_qa_product_review
string tag = "Add QA Comments"
integer x = 2368
integer y = 892
integer width = 521
integer taborder = 110
boolean enabled = false
string text = "Add Q&A Comments..."
end type

event clicked;call super::clicked;string lqastg, lqainit, lqastatcd, lprdr, lnarr, Lmed, lbkmed
int ltracks, lpages, ll_rows=0
long lbkseq
date qc_rec_date
str_qa_comments lstr_qa_comments


dw_qa_qastg.AcceptText()
ll_rows = dw_qa_qastg.RowCount()

IF ll_rows > 1 THEN
	
	lbkseq = dw_qa_prod_review.object.prod_bkseq[1]
	
	OpenWithParm(w_qa_select_qastg, Lbkseq)

	
	IF IsNull(Message.PowerObjectParm)=FALSE THEN
		lstr_qa_comments = Message.PowerObjectParm

		lqastg = lstr_qa_comments.qastg
		lqainit = lstr_qa_comments.qainit
		qc_rec_date = lstr_qa_comments.qacompdt
		
		IF IsNull(lqastg) OR lqastg="" THEN
			MessageBox("Error","You must have a row in qastg before entering any comments.")
			RETURN
		END IF
		IF IsNull(lqainit) OR lqainit="" THEN
			MessageBox("Error","You must have a valid initial in qastg before entering any comments.")
			RETURN
		END IF
		IF IsNull(qc_rec_date) THEN
			MessageBox("Error","You must have a QC review date before entering any comments.")
			RETURN
		END IF
		
		Lmed = dw_qa_prod_review.object.prod_bkmed[1]
		IF Lmed="BR" OR Lmed="P/B" THEN
			// If media is Braille or Print Braille
			lpages = dw_qa_prod_review.object.mchar_len[1]
			IF IsNull(lpages) THEN
				MessageBox("Error","You must have a valid number or pages before entering any comments.")
				RETURN
			END IF
		ELSEIF Lmed="RC" OR Lmed="FD" THEN
			// If media is cassettes
			ltracks = dw_qa_prod_review.object.mchar_len[1]
			IF IsNull(ltracks) THEN
				MessageBox("Error","You must have a valid number or tracks before entering any comments.")
				RETURN
			END IF
			// 03/21/2008 no narrator needed
//			ll_rows = dw_qa_narr.rowcount()
//			IF ll_rows > 0 THEN
//				lnarr = dw_qa_narr.object.narr[ll_rows]
//				IF IsNull(lnarr) OR lnarr="" THEN
//					MessageBox("Error","You must have a valid narrator before entering any comments.")
//					RETURN
//				END IF
//			ELSE
//				MessageBox("Error","You must have a valid narrator before entering any comments.")
//				RETURN
//			END IF
		END IF
		lprdr = dw_qa_prod_review.object.ancntr_prdr[1]
		IF IsNull(lprdr) OR lprdr="" THEN
			MessageBox("Error","You must have a valid producer before entering any comments.")
			RETURN
		END IF
		
		lbkmed = dw_qa_prod_review.object.prod_bkmed[1]
		lstr_qa_comments.bkmed = lbkmed
		lstr_qa_comments.bkseq = lbkseq
		
		OpenSheetwithparm(w_qa_cards, lstr_qa_comments, w_pics_main, 0, Original!)
	END IF
ELSEIF ll_rows=1 THEN
	lbkseq = dw_qa_prod_review.object.prod_bkseq[1]
	
	lqastg = dw_qa_qastg.object.qastg[1]
	lqainit = dw_qa_qastg.object.qainit[1]
	qc_rec_date = dw_qa_qastg.object.qacompdt[1]
		
	IF IsNull(lqastg) OR lqastg="" THEN
		MessageBox("Error","You must have a row in qastg before entering any comments.")
		RETURN
	END IF
	IF IsNull(lqainit) OR lqainit="" THEN
		MessageBox("Error","You must have a valid initial in qastg before entering any comments.")
		RETURN
	END IF
	IF IsNull(qc_rec_date) THEN
		MessageBox("Error","You must have a QC review date before entering any comments.")
		RETURN
	END IF
		
	Lmed = dw_qa_prod_review.object.prod_bkmed[1]
	IF Lmed="BR" OR Lmed="P/B" THEN
		// If media is Braille or Print Braille
		lpages = dw_qa_prod_review.object.mchar_len[1]
		IF IsNull(lpages) THEN
			MessageBox("Error","You must have a valid number or pages before entering any comments.")
			RETURN
		END IF
	ELSEIF Lmed="RC" OR Lmed="FD" THEN
		// If media is cassettes
		ltracks = dw_qa_prod_review.object.mchar_len[1]
		IF IsNull(ltracks) THEN
			MessageBox("Error","You must have a valid number or tracks before entering any comments.")
			RETURN
		END IF
		// 03/21/2008
//		ll_rows = dw_qa_narr.rowcount()
//		IF ll_rows > 0 THEN
//			lnarr = dw_qa_narr.object.narr[ll_rows]
//			IF IsNull(lnarr) OR lnarr="" THEN
//				MessageBox("Error","You must have a valid narrator before entering any comments.")
//				RETURN
//			END IF
//		ELSE
//			MessageBox("Error","You must have a valid narrator before entering any comments.")
//			RETURN
//		END IF
	END IF
	lprdr = dw_qa_prod_review.object.ancntr_prdr[1]
	IF IsNull(lprdr) OR lprdr="" THEN
		MessageBox("Error","You must have a valid producer before entering any comments.")
		RETURN
	END IF
		
	lbkmed = dw_qa_prod_review.object.prod_bkmed[1]
	
	lstr_qa_comments.bkmed = lbkmed
	lstr_qa_comments.bkseq = lbkseq
	lstr_qa_comments.qastg = lqastg
	lstr_qa_comments.qainit = lqainit
	lstr_qa_comments.qacompdt = qc_rec_date
		
	OpenSheetwithparm(w_qa_cards, lstr_qa_comments, w_pics_main, 0, Original!)
	
ELSE
	MessageBox("Error","You must enter a row in qastg before entering any comments.")
END IF

end event

event getfocus;call super::getfocus;IF this.tag <> "" THEN
	SetMicroHelp(w_pics_main,this.tag)
ELSE
	SetMicroHelp(w_pics_main,"")
END IF

end event

type st_4 from statictext within w_qa_product_review
integer x = 1550
integer y = 896
integer width = 1019
integer height = 68
integer textsize = -8
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 33554432
long backcolor = 67108864
string text = "A = Approved   R= Rejected   B = Returned"
alignment alignment = center!
boolean focusrectangle = false
end type

type cb_find from u_cb within w_qa_product_review
string tag = "Find a book"
integer x = 2080
integer y = 1132
integer width = 251
integer taborder = 80
string text = "F&ind"
boolean default = true
end type

event clicked;call super::clicked;//////////////////////////////////////////////////////////////////////////////////////////////////////////
//
//	Event: clicked for find
//
//	Description:
//	Retrieve info for the given book number and set in house comments
//
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//	
//	Revision History
//
//	Developed by 	Date 				Version							Tracking#
//									
// Murali K.			02/20/2008      005 PICS Modifications 2.0	 Reqs: QAS A.8.1, A.8.2
// Murali K.  			03/17/2008		PICS 2.0			Validate various qa stages
// Murali K.			03/20/2008		Do not pre-populate qastg since previous stages might not be
//											available
// Murali K.			06/17/2008 		Commented text for RTB Minutes,Vols and Min Last side are read-only
// Murali K.			08/27/2008 		If the book is in quarantine only manual qc0 is allowed
// Murali K.			11/05/2008		For all books enable add/edit narrators 
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//

long ll_rows,Lbkseq
string Lbkno,ls_cntr="",lmed,Lprodstage,Lprodstage2,Lcntrtype,Lcntrmed,Lricd
int rtn,lrtn,lcnt,Llen
boolean bk_just_recieved=FALSE, bk_recieved_for_QC4=FALSE
long ll_insert

SetNull(Llen)
Lbkno = em_bkno.text
Lbkseq = long(Lbkno)
 il_bkseq = Lbkseq // 03/17/2008

IF Lbkno <> "" THEN
	// Validate the book number
	IF wf_validate_bkno(Lbkseq) = TRUE THEN
		// Retrieve the contract numbers assigned to this book number
		rtn = dw_qa_cntr.Retrieve(Lbkseq)
		IF rtn > 0 THEN
			ls_cntr = dw_qa_cntr.object.cntr[1]
			// If contract number exist 
			select cntrtype,cntrmed into :Lcntrtype,:Lcntrmed from ancntr where cntr = :ls_cntr using sqlservertrans;
			IF f_check_dberror(sqlservertrans,"ANCNTR")=FALSE THEN RETURN
			IF (rtn > 1 AND Lcntrtype <> "T") THEN
				// If more than one contract assigned to this book number
				// display the contract selection window
				// If Lcntrtype is T, that means this contract is a Master & Duplication contract,
				// and there are more than one contract number for this contract type.
				OpenWithParm(w_qa_select_contract, Lbkseq)
				IF IsNull(Message.StringParm)=FALSE AND Message.StringParm<>"" THEN
		   		ls_cntr = Message.StringParm 
					select cntrtype,cntrmed into :Lcntrtype,:Lcntrmed from ancntr where cntr = :ls_cntr using sqlservertrans;
					IF f_check_dberror(sqlservertrans,"ANCNTR")=FALSE THEN RETURN
				ELSE
					cb_clear.event clicked()
					RETURN
				END IF
			END IF
			// assign the contract number.
			IF ls_cntr<>"" THEN
				dw_qa_cntr.object.cntr[1] = ls_cntr
			ELSE
				ls_cntr = dw_qa_cntr.object.cntr[1]
			END IF			
			// Get the production stage for this book and contract assigned to it.
			// select prodstage into :Lprodstage from prod where cntr=:ls_cntr and bkseq=:Lbkseq using sqlservertrans;
			IF (Lcntrtype = 'T' OR Lcntrtype = 'M') THEN 
				IF Lcntrmed = 'P/B' THEN
					Lprodstage='PU'
					Lprodstage2='PU'
				ELSE
					Lprodstage='MA'
					Lprodstage2='AB'
				END IF
			ELSEIF Lcntrtype = 'D' THEN
				IF Lcntrmed = 'RC'  THEN
					Lcntrmed = 'RTB'				
					Lprodstage = 'DU'
					Lprodstage2 = 'DU'
				ELSEIF Lcntrmed = 'FC' THEN
					Lcntrmed = 'RTB'				
					Lprodstage = 'FC'
					Lprodstage2 = 'FC'
				END IF
				
			ELSEIF Lcntrtype = 'A' THEN
				Lprodstage = 'AD'
				Lprodstage2 = 'DT'
				Lcntrmed = 'RC'
			ELSEIF Lcntrtype = 'Z' THEN
				Lprodstage = 'ZM'
				Lprodstage2 = 'ZM'
				Lcntrmed = 'RC'
			ELSEIF Lcntrtype = 'P' THEN
				Lprodstage = 'PU'
				Lprodstage2 = 'PU'
			ELSE
				Lprodstage='MA'
				Lprodstage2='AB'
			END IF	
			
			// Set 03/17/2008
			 is_cntrtype = Lcntrtype
			 is_cntr = ls_cntr
			 is_prodstage = Lprodstage
			 is_cntrmed = Lcntrmed
			 
			//	messagebox("prodstage","contract type:"+Lcntrtype+" Contract media:"+Lcntrmed+" Prodstage1"+Lprodstage+" Prodstage2"+Lprodstage2+" Contract No:"+ls_cntr+" Book No:"+string(Lbkseq))				
			// select the media information of the book from mchar table				
			ll_rows = dw_qa_prod_review.Retrieve(Lbkseq,ls_cntr,Lprodstage,Lprodstage2)
  			IF ll_rows > 0 THEN
				// retrieve the recorded agency(prdr) from narrtbl
					//dw_qa_recagcy.Retrieve()
					IF Lcntrmed = 'BR' THEN
						string Var1,Var2
						Var1 = "BR"
						Var2 = "P/B"
						dw_qa_prod_review.SetFilter("mchar_med = '"+ var1 +"' OR mchar_med = '"+ var2 + "'")
						dw_qa_prod_review.SetRedraw(false)
						dw_qa_prod_review.Filter()
						dw_qa_prod_review.SetRedraw(true)
					END IF
					// Get the media of the book
					Lmed = dw_qa_prod_review.object.prod_bkmed[1]
					Llen = dw_qa_prod_review.object.mchar_len[1]
					
					// 03/17/2008 set ins variable book med for future reference
					is_bkmed = lmed
								 
					// validate the media of the book, if BR or P/B no volumns is required.
					wf_validate_med(Lmed)
					// Retrieve the qa information

					ll_rows = dw_qa_qastg.Retrieve(Lbkseq,ls_cntr)
			
					// 10/16/2008 if not checked do not validate qa stages- may be A-D book
					IF  inv_qa.of_ischeckedin(Lbkseq,'') THEN
						ib_checkedin=TRUE
						cb_add_narr.visible = TRUE // for all books enable add/edit narrators 11/5/08
						cb_narr.Enabled=TRUE
						cb_add_narr.Enabled=TRUE
					ELSE
						// 10/20/2008 for older books enable narrator edition
						cb_add_narr.visible = TRUE
						cb_add_narr.Enabled=TRUE
						cb_narr.Enabled=TRUE
					END IF

					IF ll_rows = 0 THEN
						// If no rows were found from qastg table
		//				dw_qa_qastg.InsertRow(1)
						dw_qa_qastg.triggerevent('pfc_addrow')
						IF Lmed="BR" THEN
							// If media is Braille
							dw_qa_prod_review.object.mchar_len_t.text = "Pages"
							dw_qa_prod_review.Object.mchar_len.ValidationMsg='You must enter the number of pages.'
							dw_qa_prod_review.object.mchar_vols_t.text = "Volumes"
							dw_qa_prod_review.Object.mchar_vols.ValidationMsg='You must enter the number of volumes.'
							dw_qa_prod_review.object.mchar_vols.TabSequence = 20
							dw_qa_prod_review.object.mchar_minlastside.TabSequence = 0
							dw_qa_prod_review.object.mchar_minlastside.Background.Color=RGB(192,192,192)
							dw_qa_prod_review.Object.mchar_minlastside.Edit.Required='No'
							dw_qa_qastg.object.qastg[1]='2'
							dw_qa_qastg.object.qarecdt[1]= today()
							dw_qa_qastg.object.bkseq[1]=Lbkseq
							dw_qa_qastg.object.bkmed[1]=Lmed
							dw_qa_qastg.object.cntr[1]=ls_cntr
							dw_qa_narr.Enabled=FALSE
							dw_qa_narr.Hide()
							//dw_qa_recagcy.Hide()
							//st_3.hide()
							dw_qa_qastg.setcolumn("qarecdt")
						ELSEIF Lmed="DB" THEN
							// If media is Digital Book
							////// 06/17/2008 commented text for RTB Minutes,Vols and Min Last side are read-only
							dw_qa_prod_review.object.mchar_len_t.text = "Minutes"
//							dw_qa_prod_review.Object.mchar_len.ValidationMsg='You must enter the length.'
//							dw_qa_qastg.object.qastg[1]='4' // 03/20/2008
							dw_qa_qastg.object.qarecdt[1]= today()
							dw_qa_qastg.object.bkseq[1]=Lbkseq
							dw_qa_qastg.object.bkmed[1]=Lmed
							dw_qa_qastg.object.cntr[1]=ls_cntr
							IF Lcntrtype = 'A' THEN
								dw_qa_narr.Enabled=FALSE
								dw_qa_narr.Hide()
							ELSE
								dw_qa_narr.Enabled=TRUE
								dw_qa_narr.Show()
							END IF
//							dw_qa_qastg.setcolumn("qarecdt")
							dw_qa_qastg.setcolumn("qastg")
							
						ELSE  
							// If media is cassettes
							// 06/17/2008 changes
							dw_qa_prod_review.object.mchar_len_t.text = "Minutes"
//							dw_qa_prod_review.Object.mchar_len.ValidationMsg='You must enter the number of Minutes.'
							IF dw_qa_prod_review.object.flash_indicator[1] = 'Y' THEN
								dw_qa_prod_review.object.mchar_vols_t.text = "Cartridge"
							ELSE
								dw_qa_prod_review.object.mchar_vols_t.text = "Cassettes"
							END IF
							///// 06/17/2008
							
//							dw_qa_prod_review.Object.mchar_vols.ValidationMsg='You must enter the number of Cassettes.'
//							dw_qa_prod_review.object.mchar_vols.TabSequence = 20
//							dw_qa_prod_review.Object.mchar_minlastside.Edit.Required='Yes'
//							dw_qa_prod_review.object.mchar_minlastside.TabSequence = 30
//							dw_qa_prod_review.object.mchar_minlastside.Background.Color=RGB(255,255,255)
							select cntrtype into :Lcntrtype from ancntr where cntr = :ls_cntr using sqlservertrans;
							if Lcntrtype='D' THEN
//								dw_qa_qastg.object.qastg[1]='2' // 03/20/2008
							elseif Lcntrtype='A' THEN
								// Conversion contract only require QC 4
//								dw_qa_qastg.object.qastg[1]='4' // 03/20/2008
							else
//								dw_qa_qastg.object.qastg[1]='1'	 // 03/20/2008							
							end if
							dw_qa_qastg.object.qarecdt[1]= today()
							dw_qa_qastg.object.bkseq[1]=Lbkseq
							dw_qa_qastg.object.bkmed[1]=Lmed
							dw_qa_qastg.object.cntr[1]=ls_cntr
							dw_qa_narr.Enabled=TRUE
							dw_qa_narr.Show()
							//dw_qa_recagcy.Show()
							//st_3.Show()
//							dw_qa_qastg.setcolumn("qarecdt") // 03/20/2008
							dw_qa_qastg.setcolumn("qastg")
							
						END IF
						
	
						// 02/20/2008 in house comments insert and set columns
						ll_insert = dw_inhousecomments.event pfc_insertrow()
						dw_inhousecomments.object.bkseq[ll_insert]=Lbkseq
						dw_inhousecomments.object.bkmed[ll_insert]=Lmed
						dw_inhousecomments.object.cntr[ll_insert]=string( ll_insert)

					ELSEIF ll_rows > 0 THEN // qa stages rows retrieved
						
						// If the book has just recieved for QA and status code is set to I(initial).
						IF (dw_qa_qastg.object.qastatcd[1] = "I") THEN
							dw_qa_qastg.object.qastatcd[1] = ""
							bk_just_recieved = TRUE
						END IF
						// If the book has just recieved for QA 4  set the flag to just recieved.
						IF (dw_qa_qastg.object.qastatcd[ll_rows] = "I" AND  dw_qa_qastg.object.qastg[ll_rows] = "4" ) THEN
							bk_recieved_for_QC4 = TRUE
						END IF
						// If rows, exist in qastg table
						IF Lmed="BR" OR Lmed="P/B" THEN
							// If media is Braille or Print Braille
							dw_qa_prod_review.object.mchar_len_t.text = "Pages"
							dw_qa_prod_review.object.mchar_vols_t.text = "Volumes"
							dw_qa_prod_review.object.mchar_vols.TabSequence = 20
							dw_qa_prod_review.object.mchar_minlastside.TabSequence = 0
							dw_qa_prod_review.object.mchar_minlastside.Background.Color=RGB(192,192,192)
							dw_qa_narr.Enabled=FALSE
							// Enable the comment button
							cb_comments.Enabled=TRUE
							dw_qa_narr.Hide()
							//dw_qa_recagcy.Hide()
							//st_3.hide()
						ELSEIF Lmed="DB" THEN
							// If media is Digital book
							dw_qa_prod_review.object.mchar_len_t.text = "Minutes" 
	//						dw_qa_prod_review.object.mchar_vols.TabSequence = 20
	//						dw_qa_prod_review.object.mchar_minlastside.TabSequence = 30
							dw_qa_narr.Enabled=FALSE
							// Enable the comment button
							cb_comments.Enabled=TRUE
							IF Lcntrtype =  'A' THEN
								dw_qa_narr.Hide()
							ELSE
								dw_qa_narr.Show()
							END IF
							//dw_qa_recagcy.Show()
							//st_3.Show()
						ELSEIF Lmed="RC" OR Lmed="FD" THEN
							// If media is cassettes
							// 06/17/2008 ESTPT RTB changes
							dw_qa_prod_review.object.mchar_len_t.text = "Minutes"
//							dw_qa_prod_review.object.mchar_vols_t.text = "Cassettes"
							IF dw_qa_prod_review.object.flash_indicator[1] = 'Y' THEN
								dw_qa_prod_review.object.mchar_vols_t.text = "Cartridge"
							ELSE
								dw_qa_prod_review.object.mchar_vols_t.text = "Cassettes"
							END IF
							///// 06/17/2008

//							dw_qa_prod_review.object.mchar_vols.TabSequence = 20
//							dw_qa_prod_review.object.mchar_minlastside.TabSequence = 30
//							dw_qa_prod_review.object.mchar_minlastside.Background.Color=RGB(255,255,255)
//// 06/17/2008
							dw_qa_narr.Enabled=TRUE
							// Enable the comment button
							cb_comments.Enabled=TRUE
							dw_qa_narr.Show()
							//dw_qa_recagcy.Show()
							//st_3.Show()
						END IF
						// Set the row to the last row of the qastg datawindow.
						dw_qa_qastg.SetRow(ll_rows)
						// If the qastg has gone through all the three stages of QA,
						// Do not add any rows into qastg datawindow.
						CHOOSE CASE Lmed
							CASE "BR"
								IF (dw_qa_qastg.object.qastg[ll_rows]='2' AND &
									dw_qa_qastg.object.qastatcd[ll_rows]='R') THEN
									dw_qa_qastg.event pfc_addrow()
								ELSEIF (dw_qa_qastg.object.qastg[ll_rows]<>'2' AND &
									dw_qa_qastg.object.qastatcd[ll_rows]<>'A') THEN
									dw_qa_qastg.event pfc_addrow()
								ELSE
									dw_qa_qastg.setcolumn("qarecdt")
								END IF
							CASE "DB"
								IF (dw_qa_qastg.object.qastg[ll_rows]='4' AND &
									dw_qa_qastg.object.qastatcd[ll_rows]='R') THEN
									dw_qa_qastg.event pfc_addrow()
								ELSE
									dw_qa_qastg.setcolumn("qarecdt")
								END IF
							CASE ELSE
								// If the book is not just recieved then add the row.
								// 10/30/2008 add row only if no rows found - rowcount check
								IF (bk_just_recieved=FALSE AND bk_recieved_for_QC4=FALSE) & 
									AND dw_qa_qastg.rowcount() = 0 THEN
									dw_qa_qastg.event pfc_addrow()
								ELSE
									dw_qa_qastg.setcolumn("qarecdt")
								END IF
						END CHOOSE
						
						// check for various qa stages 03/17/2008
						
					END IF
					
					// Narrator information
					
					// If media is RC or FD retrieve the narrator information
					IF Lmed="RC" OR (Lmed="DB" AND Lcntrtype <> 'A') THEN
						rtn = dw_qa_narr.Retrieve(Lbkseq)
					END IF
					Lricd = dw_qa_prod_review.object.mchar_ricd[1]
					IF (Lricd = 'RI' AND NOT(wf_is_qstage1_approved()) AND (Lmed <> "DB")) THEN
						// IF the book is for reissue and has gone through the first qastg,
						// then don't allow update to narr and studio.
						dw_qa_narr.Enabled = TRUE
						//dw_qa_recagcy.Enabled = TRUE
						// QAS wants to be able to update Minutes, volumn and minlastside 
						// even after stage I is approved.
						// dw_qa_prod_review.Enabled = FALSE
					ELSEIF (Lcntrtype='D' OR wf_is_qstage1_approved()) THEN
						// If the book is for duplication only or has gone through the first qastg,
						// then don't allow any update to minlastside, Minutes, narr and studio.
						dw_qa_narr.Enabled = FALSE
						//dw_qa_recagcy.Enabled = FALSE
						// dw_qa_prod_review.Enabled = FALSE
					ELSEIF Lmed = "DB"  AND  Lcntrtype = 'A' THEN
						// IF the book is for conversion
						dw_qa_narr.Enabled = FALSE
					ELSE
						// Book is not for duplication only.
						dw_qa_narr.Enabled = TRUE
						//dw_qa_recagcy.Enabled = TRUE
						dw_qa_prod_review.Enabled = TRUE
					END IF
					
			
					// 02/20/2008 RETRIEVE in house comments
					ll_rows = dw_inhousecomments.Retrieve(lbkseq, lmed)
					IF ll_rows = 0 THEN
						ll_insert = dw_inhousecomments.event pfc_insertrow()
						dw_inhousecomments.object.bkseq[ll_insert]=Lbkseq
						dw_inhousecomments.object.bkmed[ll_insert]=Lmed
						dw_inhousecomments.object.cntr[ll_insert]= string(ll_insert)
					ELSE
						mle_1.text = dw_inhousecomments.object.inhouse_comments[LL_ROWS] 
					END IF
						
					IF IsNull(Llen) THEN
						dw_qa_prod_review.SetFocus()
					ELSE
						dw_qa_qastg.SetFocus()
					END IF
					em_bkno.Enabled = FALSE
					cb_update.Enabled = TRUE
					cb_updateweb.Enabled = TRUE
					cb_clear.Enabled = TRUE

					IF Lmed="RC" OR Lmed="FD" THEN
					END IF
					cb_find.Enabled = FALSE
					
					// 08/27/2008 if the book is in quarantine do not allow edits, allow only manual qc0
					il_qrs = ids_book_status.retrieve(Lbkseq)
					IF il_qrs > 0  THEN
	  					MessageBox("Error", "Book is in the quarantine area, Manual QC0 is only allowed" )
					END IF
					
				ELSEIF dw_qa_cntr.rowcount() > 1 THEN
				   MessageBox("Information","Book Number: "+Lbkno+" has more than one contract assigned to it. ~n Please select appropriate contract number.", Information!, OK!, 1)
					dw_qa_cntr.SetFocus()					
				ELSE					
  					MessageBox("Error", "Book Number: "+Lbkno+" Does not exist. ~n Enter the correct Book number." ,Information!, OK!, 2)
					em_bkno.SetFocus()					
				END IF
		END IF
		//dw_qa_recagcy.ResetUpdate()
	ELSE
		em_bkno.SetFocus()
	END IF		
ELSE
   MessageBox("Database Error", "Please enter the Book Number." ,StopSign!, OK!, 2)
	em_bkno.SetFocus()
END IF
end event

event getfocus;call super::getfocus;IF this.tag <> "" THEN
	SetMicroHelp(w_pics_main,this.tag)
ELSE
	SetMicroHelp(w_pics_main,"")
END IF

end event

type cb_exit from u_cb within w_qa_product_review
string tag = "exist the screen"
integer x = 2642
integer y = 1132
integer width = 238
integer taborder = 130
boolean bringtotop = true
string text = "E&xit"
end type

event clicked;call super::clicked;parent.Event pfc_close()

end event

event getfocus;call super::getfocus;IF this.tag <> "" THEN
	SetMicroHelp(w_pics_main,this.tag)
ELSE
	SetMicroHelp(w_pics_main,"")
END IF

end event

type cb_clear from u_cb within w_qa_product_review
string tag = "clear screen"
integer x = 2363
integer y = 1132
integer width = 247
integer taborder = 100
boolean bringtotop = true
boolean enabled = false
string text = "&Clear"
end type

event clicked;call super::clicked;//////////////////////////////////////////////////////////////////////////////////////////////////////////
//
//	Event: clicked for cb_clear
//
//	Description:
//	clear fields and reset datawindows
//
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//	
//	Revision History
//
//	Developed by 	Date 				Version										Tracking#
//									
// Murali K.			02/21/2008      005 PICS Modifications Phase 2	 Reqs: QAS a.8.1, A.8.2
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//

string Lmed

IF dw_qa_prod_review.rowcount() > 0 THEN
	Lmed = trim(dw_qa_prod_review.Object.mchar_med[1])
END IF

dw_qa_prod_review.setFilter("")
dw_qa_prod_review.Reset()
//dw_qa_prod_review.insertrow(0)

dw_qa_qastg.Reset()
dw_qa_qastg.insertrow(0)

dw_qa_cntr.Reset()
dw_qa_cntr.insertrow(0)

// 02/21/2008
dw_inhousecomments.Reset()




IF (Lmed <> "BR" OR Lmed <> "P/B") THEN
	dw_qa_narr.Reset()
	dw_qa_narr.insertrow(0)
//	dw_qa_recagcy.Reset()
//	dw_qa_recagcy.insertrow(0)
END IF

wf_settab_order_noqastg()

em_bkno.text=""
local_recagcy=""
// Set the current contract to NULL
current_cntr=""

wf_reset_update()

em_bkno.Enabled = TRUE
cb_clear.Enabled=FALSE
cb_find.Enabled=TRUE
cb_update.Enabled=FALSE
cb_updateweb.Enabled=FALSE
cb_find.Default=TRUE
cb_comments.Enabled=FALSE
cb_narr.Enabled=FALSE
cb_add_narr.Enabled=FALSE

em_bkno.SetFocus()


end event

event getfocus;call super::getfocus;IF this.tag <> "" THEN
	SetMicroHelp(w_pics_main,this.tag)
ELSE
	SetMicroHelp(w_pics_main,"")
END IF

end event

type cb_update from u_cb within w_qa_product_review
string tag = "Update record"
integer x = 1792
integer y = 1132
integer width = 251
integer taborder = 70
boolean bringtotop = true
boolean enabled = false
string text = "&Update"
end type

event clicked;call super::clicked;//////////////////////////////////////////////////////////////////////////////////////////////////////////
//
//	Event:  clicked for Update
//
//	Description:
//	Update data - New in house comments update added/ validate rejection reason
//
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//	
//	Revision History
//
//	Developed by 	Date 				Version						Tracking#
//									
// Murali K.			02/20/2008      005 PICS Modifications	 Reqs: QAS A.8.1
// Murali K.			07/17/2008 		update reviewer_assignment if stage 5 is approved
// Murali K. 			07/18/2008		if record generated automated do not discard
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//

Integer rtn,i,qacnt,webrtn, li_cnt, length,qnty,minlastside,volumn,ll_newrow
String media,prodstage,cntrtype,Ls_cntrtype
String Lnarr,Lnarrfn,Lmed,Lqastatcd,Luserid, ls_lname, ls_fname, ls_conno, ls_comments
string ls_review_type
Long Lbkseq
dwitemstatus l_status

Lbkseq = Long(em_bkno.text)


qacnt = dw_qa_qastg.RowCount()
IF qacnt < 1 THEN
	RETURN
END IF

dw_qa_qastg.AcceptText()
Lqastatcd = Trim(dw_qa_qastg.object.qastatcd[qacnt])
ls_comments = Trim(dw_qa_qastg.object.qacomments[qacnt])
ls_cntrtype = dw_qa_prod_review.object.ancntr_cntrtype[1]

Luserid = SqlServerTrans.userId
li_cnt=dw_qa_narr.RowCount()
Lmed = Trim(dw_qa_prod_review.object.prod_bkmed[1])
// bkmed='RC' and without narrator are not allowed to update any thing
// 03/21/2008 narrator entry  not needed
//IF (Lmed='RC' OR Lmed='DB') AND li_cnt=0 AND Lqastatcd <> "B"  AND Ls_cntrtype <> 'A' THEN
//	Messagebox('Error','Narrator is a required field.'+' The status of the book is = '+Lqastatcd)
//	Messagebox('Error','Narrator is a required field.')
//	dw_qa_narr.Visible=TRUE
//	dw_qa_narr.Enabled=TRUE
//	ll_newrow = dw_qa_narr.InsertRow(0)
//	dw_qa_narr.ScrollToRow(ll_newrow)
//	RETURN
//ELSEIF (Lmed='RC' AND Lmed='DB') AND li_cnt>0 AND Ls_cntrtype <> 'A' THEN
//	FOR i=1 TO li_cnt
//		ls_lname=dw_qa_narr.object.narr[i]
//		ls_fname=dw_qa_narr.object.narrfn[i]
//		IF (IsNull(ls_lname) OR Trim(ls_lname)=""  OR IsNull(ls_fname) OR Trim(ls_fname)="") AND Lqastatcd <> "B" THEN
//			Messagebox('Error','Narrator is a required field.')
//			dw_qa_narr.Visible=TRUE
//			dw_qa_narr.Enabled=TRUE
//			ll_newrow = dw_qa_narr.InsertRow(0)
//			dw_qa_narr.ScrollToRow(ll_newrow)
//			RETURN
//		END IF
//	NEXT
//END IF
	
IF Lmed="BR" OR Lmed="P/B" OR Lmed="DB" THEN
	// If media is Braille or DigitalBook 
	dw_qa_prod_review.object.mchar_minlastside.Edit.Required='No'
ELSE
	dw_qa_narr.AcceptText()
	// Else if it is cassettes
//	dw_qa_prod_review.object.mchar_minlastside.Edit.Required='Yes'  // 06/19/2008
	// 03/21/2008
//	IF dw_qa_narr.RowCount() = 0 AND Lqastatcd <> "B" AND Lmed <> "DB" THEN
//		Messagebox("ERROR","Narrator must be selected.",stopSign!)
//		dw_qa_narr.enabled=TRUE
//		//dw_qa_recagcy.Enabled=TRUE
//		dw_qa_narr.TriggerEvent("pfc_addrow") 
//		dw_qa_narr.SetFocus()
//		dw_qa_narr.SetColumn(1)
//		RETURN
//	END IF
END IF

// 02/20/2008 update in house comments and validate rejection reason
IF of_validaterejreason() < 1	THEN
	RETURN
END IF
of_setinhousecomments()
dw_inhousecomments.accepttext()
rtn  = dw_inhousecomments.EVENT pfc_update(TRUE,TRUE)
IF rtn = 1 THEN
	COMMIT USING SqlServerTrans;
ELSE
	ROLLBACK USING SqlServerTrans;
	Messagebox("Error","Update failed for in house comments.",information!)
END IF


// Check to see if this book is on hold. (Retruned)
IF Lqastatcd <> "B" THEN
	rtn = dw_qa_prod_review.AcceptText()
	IF rtn = 1 THEN
		rtn = dw_qa_narr.AcceptText()
		IF rtn = 1 THEN
			rtn = dw_qa_qastg.AcceptText()
			IF rtn = 1 THEN
				
		FOR i = 1 TO dw_qa_prod_review.RowCount()				
				// Calculate the units and subunits for the last time
	//			length 			= dw_qa_prod_review.object.mchar_len[1]	
				length 			= dw_qa_prod_review.object.mchar_length_[i]	// 06/19/2008
				media 			= dw_qa_prod_review.object.mchar_med[i]
				prodstage 		= dw_qa_prod_review.object.prod_prodstage[i]
				cntrtype 		= dw_qa_prod_review.object.ancntr_cntrtype[i]
				qnty 				= dw_qa_prod_review.object.mchar_qnty[i]
				minlastside 	= dw_qa_prod_review.object.mchar_minlastside[i]
				volumn   	     = dw_qa_prod_review.object.mchar_vols[i]
				wf_calculate_units(media,cntrtype,length,qnty,minlastside,volumn,i)					
		NEXT	
				SetMicroHelp(w_pics_main,"Updating Records Please Wait...")
				
				FOR i = 1 TO dw_qa_qastg.RowCount()

					// 07/18/2008 if record generated automated do not discard
					ls_review_type = dw_qa_qastg.object.review_type_code[i]
					l_status = dw_qa_qastg.GetItemStatus(i,0,Primary!)
					IF (Isnull(ls_review_type) OR ls_review_type <> 'A') THEN
						IF (IsNull(dw_qa_qastg.object.qastg[i]) OR & 
							IsNull(dw_qa_qastg.object.qainit[i]) OR &
							  IsNull(dw_qa_qastg.object.qastatcd[i])) THEN
//							dw_qa_qastg.DeleteRow(i)
							Messagebox('Error', 'QA Stg OR Reviewer OR Status not entered.')
							RETURN
						END IF
					END IF
				NEXT
				
							
				///// 7/17/2008 update qa_reviewer_assignment 
				long ll_rc
				string ls_stg, ls_stat
				boolean lb_reviewer_assignment_update=FALSE
				
				ll_rc = dw_qa_qastg.RowCount()
				ls_stg  = dw_qa_qastg.object.qastg[ll_rc]
				ls_stat = dw_qa_qastg.object.qastatcd[ll_rc]
				IF (ls_stg = '5' OR ls_stg = '0') AND ls_stat = 'A' THEN
					lb_reviewer_assignment_update = TRUE
				END IF
				////
				
				// Check for any pending updates
				IF (of_UpdateChecks() < 0 AND Lqastatcd <> "B")THEN RETURN -1
				
				// IF Media is RC or FD THEN Narrator must exist.
				// 03/21/2008
//				IF (Lmed="RC" OR Lmed="DB") AND Lqastatcd <> "B" AND Ls_cntrtype <> 'A' THEN
//					IF (dw_qa_narr.object.narr[1]="" OR IsNull(dw_qa_narr.object.narr[1])) THEN
//						Messagebox("ERROR","Narrator must be selected.",stopSign!)
//						dw_qa_narr.SetFocus()
//						dw_qa_narr.SetColumn(1)
//						RETURN
//					END IF
//				END IF


				rtn = dw_qa_prod_review.EVENT pfc_update(TRUE,TRUE)
				IF rtn = 1 THEN
					IF (Lmed <> "BR" AND Lmed <> "P/B" AND Lqastatcd <> "B") THEN
						rtn = dw_qa_narr.EVENT pfc_update(TRUE,TRUE)
						IF rtn = 1 THEN
							rtn  = dw_qa_qastg.EVENT pfc_update(TRUE,TRUE)
							IF rtn = 1 THEN
								// 07/17/2008 update reviewer_assignment if stage 5 is approved
								IF lb_reviewer_assignment_update THEN
									UPDATE QA_REVIEWER_ASSIGNMENT
									SET ACTIVE_STATUS_CODE = 'T'
									WHERE BKSEQ = :Lbkseq AND BKMED = 'DB' using sqlservertrans ;
								END IF
								
								COMMIT USING SqlServerTrans;
								cb_clear.EVENT Clicked()
							ELSE
								ROLLBACK USING SqlServerTrans;
								Messagebox("Error","Update failed.",information!)
							END IF
						ELSE
							ROLLBACK USING SqlServerTrans;
							Messagebox("Error","Update failed.",information!)
						END IF
					ELSE
						rtn  = dw_qa_qastg.EVENT pfc_update(TRUE,TRUE)
						IF rtn = 1 THEN
							COMMIT USING SqlServerTrans;
							cb_clear.EVENT Clicked()
						ELSE
							ROLLBACK USING SqlServerTrans;
							Messagebox("Error","Update failed.",information!)
						END IF
					END IF
					// If the media is DB and the book was rejected
					IF Lmed = "DB"  AND Lqastatcd = 'R' AND ls_cntrtype = 'A' THEN
					// This book is conversion books. The only way to distinguish converison books
					// from DTB books is with 'DT' row in PROD table
						li_cnt = 0
						SELECT count(*) INTO :li_cnt
						FROM prod 
						where bkseq = :Lbkseq
						AND prodstage = 'DT'
						USING SqlServerTrans;
					
						IF li_cnt = 1 THEN
							// This book is conversion book, the row exist
							SELECT conno INTO :ls_conno
							FROM mchar
							where bkseq = :Lbkseq
							AND bkmed = 'RC'
							USING SqlServerTrans;
							
							IF NOT(IsNull(ls_conno)) THEN
								// Update the conversionbooks table
								UPDATE conversionbooks
								set action_type = 'R', qa_comments = :ls_comments
								where conno = :ls_conno
								USING SqlServerTrans;
								IF f_check_dberror(SqlServerTrans, "sub")=FALSE THEN
									ROLLBACK USING SqlServerTrans;
								ELSE
									COMMIT USING SqlServerTrans;
								END IF
							END IF								
						END IF
					END IF
					// Mark the MCHAR Table
					f_update_mchar_time("",Lbkseq,"B","U")
				ELSE
					ROLLBACK USING SqlServerTrans;
					Messagebox("Error","Update failed.",information!)
				END IF
			ELSE
				dw_qa_qastg.SetFocus()
			END IF
		ELSE
			dw_qa_narr.SetFocus()
		END IF
	ELSE
		dw_qa_prod_review.SetFocus()
	END IF
// This book is on hold (returned)
ELSE
	rtn = dw_qa_prod_review.AcceptText()
	IF rtn = 1 THEN
		rtn = dw_qa_qastg.AcceptText()
		IF rtn = 1 THEN
			dw_qa_narr.AcceptText()
			//dw_qa_recagcy.AcceptText()
			SetMicroHelp(w_pics_main,"Updating Records Please Wait...")
				
			// Calculate the units and subunits for the last time
////			length 			= dw_qa_prod_review.object.mchar_len[1]	
//			length 			= dw_qa_prod_review.object.mchar_length_[1]	
//			media 			= dw_qa_prod_review.object.mchar_med[1]
//			prodstage 		= dw_qa_prod_review.object.prod_prodstage[1]
//			cntrtype 		= dw_qa_prod_review.object.ancntr_cntrtype[1]
//			qnty 				= dw_qa_prod_review.object.mchar_qnty[1]
//			minlastside 	= dw_qa_prod_review.object.mchar_minlastside[1]
//			volumn   	     = dw_qa_prod_review.object.mchar_vols[1]
//			wf_calculate_units(media,cntrtype,length,qnty,minlastside,volumn)					
//			

		FOR i = 1 TO dw_qa_prod_review.RowCount()				
				// Calculate the units and subunits for the last time
	//			length 			= dw_qa_prod_review.object.mchar_len[1]	
				length 			= dw_qa_prod_review.object.mchar_length_[i]	// 06/19/2008
				media 			= dw_qa_prod_review.object.mchar_med[i]
				prodstage 		= dw_qa_prod_review.object.prod_prodstage[i]
				cntrtype 		= dw_qa_prod_review.object.ancntr_cntrtype[i]
				qnty 				= dw_qa_prod_review.object.mchar_qnty[i]
				minlastside 	= dw_qa_prod_review.object.mchar_minlastside[i]
				volumn   	     = dw_qa_prod_review.object.mchar_vols[i]
				wf_calculate_units(media,cntrtype,length,qnty,minlastside,volumn,i)					
		NEXT	
		
			
			FOR i = 1 TO dw_qa_qastg.RowCount()
				// 07/18/2008 if record generated automated do not discard
				ls_review_type = dw_qa_qastg.object.review_type_code[i]
				l_status = dw_qa_qastg.GetItemStatus(i,0,Primary!)
				IF (Isnull(ls_review_type) OR ls_review_type <> 'A') THEN
					IF (IsNull(dw_qa_qastg.object.qastg[i]) OR & 
						IsNull(dw_qa_qastg.object.qainit[i]) OR &
						  IsNull(dw_qa_qastg.object.qastatcd[i])) THEN
//							dw_qa_qastg.DeleteRow(i)
							Messagebox('Error', 'QA Stg OR Reviewer OR Status not entered.')
							RETURN
					END IF
				END IF
			NEXT

			
			IF Lqastatcd <> "B" THEN
				// Check for any pending updates
				IF (of_UpdateChecks() < 0) THEN RETURN -1
			END IF
						
						
			rtn = dw_qa_prod_review.EVENT pfc_update(TRUE,TRUE)
			IF rtn = 1 THEN
				IF (Lmed <> "BR" AND Lmed <> "P/B" AND Lmed <> "DB") AND Lqastatcd <> "B" THEN
					rtn = dw_qa_narr.EVENT pfc_update(TRUE,TRUE)
					IF rtn = 1 THEN
						rtn  = dw_qa_qastg.EVENT pfc_update(TRUE,TRUE)
						IF rtn = 1 THEN
							COMMIT USING SqlServerTrans;
							cb_clear.EVENT Clicked()
						ELSE
							ROLLBACK USING SqlServerTrans;
							Messagebox("Error","Update failed.",information!)
						END IF
					ELSE
						ROLLBACK USING SqlServerTrans;
						Messagebox("Error","Update failed.",information!)
					END IF
				ELSE
					rtn  = dw_qa_qastg.EVENT pfc_update(TRUE,TRUE)
					IF rtn = 1 THEN
						COMMIT USING SqlServerTrans;
						//IF Trim(Luserid) <> "dsmi" AND Trim(Luserid) <> "tmcl" THEN
						//   SetMicroHelp(w_pics_main,"Database Updated.")
						//ELSE
							//webrtn = Messagebox("Updating WEB","Do you want to update WEB database?",Question!,YesNo!)
							//IF webrtn = 1 THEN
						//		cb_updateweb.EVENT Clicked()
							//END IF
						//END IF										
						cb_clear.EVENT Clicked()
					ELSE
						ROLLBACK USING SqlServerTrans;
						Messagebox("Error","Update failed.",information!)
					END IF
				END IF
				// Mark the MCHAR Table
				f_update_mchar_time("",Lbkseq,"B","U")
			ELSE
				ROLLBACK USING SqlServerTrans;
				Messagebox("Error","Update failed.",information!)
			END IF
		ELSE
			dw_qa_qastg.SetFocus()
		END IF
	ELSE
		dw_qa_prod_review.SetFocus()
	END IF
END IF



end event

event getfocus;call super::getfocus;IF this.tag <> "" THEN
	SetMicroHelp(w_pics_main,this.tag)
ELSE
	SetMicroHelp(w_pics_main,"")
END IF

end event

type st_1 from statictext within w_qa_product_review
integer x = 27
integer y = 24
integer width = 334
integer height = 76
boolean bringtotop = true
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long backcolor = 79741120
boolean enabled = false
string text = "Book Number"
boolean focusrectangle = false
end type

type em_bkno from uo_conno within w_qa_product_review
string tag = "Book Number"
integer x = 32
integer y = 100
integer width = 370
integer height = 100
integer taborder = 10
integer textsize = -14
integer weight = 700
long textcolor = 0
maskdatatype maskdatatype = stringmask!
string mask = "!!!!!!!"
string displaydata = ""
end type

event getfocus;call super::getfocus;IF this.tag <> "" THEN
	SetMicroHelp(w_pics_main,this.tag)
ELSE
	SetMicroHelp(w_pics_main,"")
END IF

end event

type dw_qa_qastg from u_pics_dw within w_qa_product_review
event ue_enterkey pbm_dwnprocessenter
event ue_tabkey pbm_dwntabout
string tag = "QA Information"
integer x = 14
integer y = 420
integer width = 2875
integer height = 460
integer taborder = 40
boolean bringtotop = true
string dataobject = "d_qa_qastg"
boolean livescroll = false
boolean ib_isupdateable = false
boolean ib_rmbmenu = false
boolean ib_rmbfocuschange = false
string is_updatesallowed = ""
end type

event ue_enterkey;Integer li_ColNbr,currow,rowcount
string Lmed,Lqastatcd,Lqastg,Linit,Lqarejcd,Lqacomments

currow = dw_qa_qastg.GetRow()
rowcount = dw_qa_qastg.RowCount()

Lmed = dw_qa_prod_review.object.prod_bkmed[1]
Lqastatcd = dw_qa_qastg.object.qastatcd[currow]
Lqastg = dw_qa_qastg.object.qastg[currow]
Linit = dw_qa_qastg.object.qainit[currow]
Lqarejcd = dw_qa_qastg.object.qarejcd[currow]
Lqacomments = dw_qa_qastg.object.qacomments[currow]

li_ColNbr = dw_qa_qastg.GetColumn() 
//messagebox("column number",string(li_colnbr))
IF ((li_ColNbr=10) AND Lqastg='3' AND Lqastatcd='A') THEN
	  	IF wf_find_approved_qastg2(currow) THEN
			Send(Handle(this),256,9,Long(0,0))
			return(1)
		ELSE
			dw_qa_qastg.Event pfc_addrow ()
		END IF			
ELSEIF (li_ColNbr=9) THEN
	IF IsNull(Lqacomments) THEN
		this.SetColumn(10)
		//messagebox("data","no comments")
	ELSE	
		Send(Handle(this),256,9,Long(0,0))
		return(1)
	END IF
ELSEIF (li_ColNbr=10 and (Lqastatcd='R' OR Lqastatcd='B')) THEN
		Send(Handle(this),256,9,Long(0,0))
		return(1)
ELSEIF ( (li_ColNbr=10) AND &
	 		(Lmed = 'BR' OR Lmed = 'P/B') AND &
	 		(currow >= rowcount) AND &
			Lqastatcd <> 'A') THEN
			dw_qa_qastg.SetColumn("qarecdt")
			dw_qa_qastg.Event pfc_addrow ()
ELSEIF ((li_ColNbr=10) AND (Lmed = 'RC') AND (currow >= rowcount) AND & 
		  (dw_qa_narr.RowCount() <> 0 )) THEN
				dw_qa_qastg.SetColumn("qarecdt")
				dw_qa_qastg.Event pfc_addrow ()
ELSEIF ((li_ColNbr=10) AND (Lmed = 'RC') AND (currow >= rowcount) AND & 
		  (dw_qa_narr.RowCount() = 0 )) THEN
				//dw_qa_qastg.SetColumn("qarecdt")
		  		dw_qa_narr.Event pfc_addrow()
		  		dw_qa_narr.SetFocus()
ELSEIF (li_ColNbr=7) THEN
	IF Lqastatcd = "" THEN
		MessageBox("ERROR","Valid codes are A(Approved) or R(Reject)")
	ELSE
		Send(Handle(this),256,9,Long(0,0))
		return(1)
	END IF
ELSE
	Send(Handle(this),256,9,Long(0,0))
	return(1)
END IF
end event

event ue_tabkey;Integer li_ColNbr,currow,rowcount
string Lmed,ls_cntrtype

currow = dw_qa_qastg.GetRow()
rowcount = dw_qa_qastg.RowCount()

Lmed = dw_qa_prod_review.object.prod_bkmed[1]
ls_cntrtype = dw_qa_prod_review.object.ancntr_cntrtype[1]

li_ColNbr = dw_qa_qastg.GetColumn() 

// messagebox("column number",string(li_colnbr))
IF ((li_ColNbr=10) AND (Lmed = 'RC' OR Lmed = 'DB') AND ls_cntrtype <> 'A' AND (currow >= rowcount) AND & 
	  (dw_qa_narr.RowCount() = 0 )) THEN
	  		dw_qa_narr.Event pfc_addrow()
			dw_qa_narr.enabled=true
	  		dw_qa_narr.SetFocus()
END IF
end event

event ue_postconstructor;call super::ue_postconstructor;dw_qa_qastg.SetTransObject(sqlservertrans) 
this.of_SetDropDownSearch(TRUE)
of_SetJaws(TRUE)
//this.inv_dropdownsearch.of_AddColumn("qainit")
//this.of_SetDropDownCalendar(TRUE)
//this.iuo_calendar.of_Register("qarecdt",this.iuo_calendar.DDLB)
//this.iuo_calendar.of_Register("qacompdt",this.iuo_calendar.DDLB)
//


end event

event pfc_addrow;//////////////////////////////////////////////////////////////////////////////////////////////////////////
//
//	Event: pfc_addrow  for dw_qa_qastg
//
//	Description:
//	Cross field validations
//
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//	
//	Revision History
//
//	Developed by 	Date 				Purpose									Tracking#
//									
// Murali K.            03/17/2008		 PICS 2.0 				Fine tune code/ Validate various qa stages
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//

// Add rwo was modifed on 11/6/06 to accomodate the QC5 and QC6 on lines 97-107
Long	ll_rc,Lbkseq
Int cur_row
String Lqa_approve,lqastg,lbkmed,Lcntr,lcntrtype,lmed,ls_userid
Boolean qc2_aprv_not_found=FALSE
Date tday

tday = Today()
ls_userid = gnv_app.of_GetUserId()

cur_row = dw_qa_qastg.RowCount()

IF cur_row > 0 THEN
	Lqa_approve = dw_qa_qastg.object.qastatcd[cur_row]
	lqastg = dw_qa_qastg.object.qastg[cur_row]
END IF

Lbkseq = Long(em_bkno.text)
lbkmed = Trim(dw_qa_prod_review.object.prod_bkmed[1])
// If the contract has not changed then the first contract is the selected contract
// else if the contract is changed used the global variable current_cntr to set the contract number
IF NOT(IsNull(current_cntr)) AND current_cntr <> "" THEN
	Lcntr = current_cntr
ELSE
	Lcntr = dw_qa_cntr.object.cntr[1]
END IF
lcntrtype = dw_qa_prod_review.object.ancntr_cntrtype[1]
lmed = Trim(dw_qa_prod_review.object.mchar_med[1])

// Add a row if the contract type is not a duplicate contract
IF lcntrtype<>'M' THEN
	
	// Insert row
	IF IsValid (inv_rowmanager) THEN
		ll_rc = inv_rowmanager.EVENT pfc_addrow ()
	ELSE
		ll_rc = this.InsertRow (0) 
	END IF
		
	// Notify the Linkage Service that a new row has been added.
	IF IsValid (inv_linkage) THEN 
		inv_linkage.EVENT pfc_InsertRow (ll_rc) 
	END IF
	
	// New row inserted add the house keeing
	dw_qa_qastg.object.created_date[ll_rc] = tday
	dw_qa_qastg.object.modified_date[ll_rc] = tday
	dw_qa_qastg.object.created_by[ll_rc] = ls_userid
	dw_qa_qastg.object.modified_by[ll_rc] = ls_userid

	// 03/17/2008 set the key columns
	dw_qa_qastg.object.bkseq[ll_rc]=Lbkseq
	dw_qa_qastg.object.cntr[ll_rc]=Lcntr
	dw_qa_qastg.object.qarecdt[ll_rc]= Today()


	IF lqastg = '1' AND (Lqa_approve<>'R' AND Lqa_approve<>'B') THEN
		
		// Qastg is 1 and this books is not rejeted and book was not put on hold then set qastg to 2
		// If the medium is RTB, set the qastg to 4 after the QC 1 is approved
		IF lmed = 'RTB' THEN
//			dw_qa_qastg.object.qastg[ll_rc]='4'
			lbkmed='DB'
		ELSE
	//		dw_qa_qastg.object.qastg[ll_rc]='2'
		END IF

	ELSEIF lqastg='1' AND (Lqa_approve='R' OR Lqa_approve='B') THEN
		
		// Qastg is 1 and this books is rejeted or book was put on hold then set qastg to 1 
		dw_qa_qastg.object.qastg[ll_rc]='1'
		dw_qa_qastg.object.bkmed[ll_rc]=lbkmed
	ELSEIF lqastg = '2' THEN
		
		// Qastg is 2
		IF lbkmed <> "BR" THEN
			
			// Book is not braille set qastg = 3
			dw_qa_qastg.object.qastg[ll_rc]='3'
			
		// 03/17/2008 if qastg is rejected then set 2
  	 ELSEIF lbkmed = "BR" AND lqa_approve = 'R' THEN
			// Book is braille set qastg = 2
			dw_qa_qastg.object.qastg[ll_rc]='2'
		END IF
		
	ELSEIF lqastg = '3' AND (Lqa_approve='R' OR Lqa_approve='B') THEN
		IF wf_find_approved_qastg2(ll_rc - 1) THEN
			// qastg is 3 and book is rejected or put on hold then qastg is 3
			dw_qa_qastg.object.qastg[ll_rc]='3'
		ELSE
			dw_qa_qastg.object.qastg[ll_rc]='2'
		END IF		
	ELSEIF lqastg = '3' AND Lqa_approve='A' THEN
		IF wf_find_approved_qastg2(ll_rc - 1)=FALSE THEN
			dw_qa_qastg.object.qastg[ll_rc]='2'
		ELSE
			dw_qa_qastg.object.qastg[ll_rc]='3'
		END IF		
		
//// Stage QC4 is not dependent on any other stages 03/17/2008		
//	ELSEIF lqastg = '4' AND Lqa_approve='R' THEN
//		// RTB - Rejected QC4 will allow business to go on.
//		IF wf_find_approved_qastg2(ll_rc - 1)=FALSE THEN
//			dw_qa_qastg.object.qastg[ll_rc]='2'
//		ELSE
//			dw_qa_qastg.object.qastg[ll_rc]='4'
//		END IF		
//	ELSEIF lqastg = '4' AND Lqa_approve='A' THEN
//		// RTB - Approved QC4 will allow business to go on to QC2 if not yet there.
//		IF wf_find_approved_qastg2(ll_rc - 1)=FALSE THEN
//			dw_qa_qastg.object.qastg[ll_rc]='2'
//		ELSE
//			qc2_aprv_not_found = TRUE
//		END IF
///// 03/17/2008 stage 2 is valid only for braille
//	ELSEIF lqastg = '6' AND Lqa_approve='R' THEN
//		// RTB - Rejected QC5 will allow business to go on.
//		IF wf_find_approved_qastg2(ll_rc - 1)=FALSE THEN
//			dw_qa_qastg.object.qastg[ll_rc]='2'
//		ELSE
//			dw_qa_qastg.object.qastg[ll_rc]='6'
//			lbkmed='DB'
//		END IF		
//	ELSEIF lqastg = '6' AND Lqa_approve='A' THEN
//		// RTB - Approved QC4 will allow business to go on to QC2 if not yet there.
//		IF wf_find_approved_qastg2(ll_rc - 1)=FALSE THEN
//			dw_qa_qastg.object.qastg[ll_rc]='2'
//		ELSE
//			qc2_aprv_not_found = TRUE
//		END IF
//	ELSEIF lqastg = '5' AND Lqa_approve='R' THEN
//		// RTB - Rejected QC5 will allow business to go on.
//		IF wf_find_approved_qastg2(ll_rc - 1)=FALSE THEN
//			dw_qa_qastg.object.qastg[ll_rc]='2'
//		ELSE
//			dw_qa_qastg.object.qastg[ll_rc]='5'
//			lbkmed='DB'
//		END IF		
//	ELSEIF lqastg = '5' AND Lqa_approve='A' THEN
//		// RTB - Approved QC4 will allow business to go on to QC2 if not yet there.
//		IF wf_find_approved_qastg2(ll_rc - 1)=FALSE THEN
//			dw_qa_qastg.object.qastg[ll_rc]='2'
//		ELSE
//			qc2_aprv_not_found = TRUE
//		END IF
//	ELSE
//		qc2_aprv_not_found = TRUE		
//	END IF


	IF	qc2_aprv_not_found = TRUE THEN
		// 03/17/2008 qc4 not dependent
//		IF lqastg = '4' AND  Lqa_approve='A'  THEN
//			dw_qa_qastg.object.qastg[ll_rc]='5'
//			dw_qa_qastg.object.bkseq[ll_rc]=Lbkseq
//			dw_qa_qastg.object.bkmed[ll_rc]='DB'
//			dw_qa_qastg.object.cntr[ll_rc]=Lcntr
//			dw_qa_qastg.object.qarecdt[ll_rc]= Today()
//		ELSEIF lqastg = '4' AND Lqa_approve='B' THEN
//			dw_qa_qastg.object.qastg[ll_rc]='4'
//			dw_qa_qastg.object.bkseq[ll_rc]=Lbkseq
//			dw_qa_qastg.object.bkmed[ll_rc]='DB'
//			dw_qa_qastg.object.cntr[ll_rc]=Lcntr
//			dw_qa_qastg.object.qarecdt[ll_rc]= Today()
		ELSEIF lqastg = '5' AND Lqa_approve='A' THEN
			IF dw_qa_prod_review.object.flash_indicator[1] = 'Y' THEN
				dw_qa_qastg.object.qastg[ll_rc]='6'
				dw_qa_qastg.object.bkseq[ll_rc]=Lbkseq
				dw_qa_qastg.object.bkmed[ll_rc]='DB'
				dw_qa_qastg.object.cntr[ll_rc]=Lcntr
				dw_qa_qastg.object.qarecdt[ll_rc]= Today()
			ELSE
//				Messagebox("QASTG","Flash indicator is not checked for this book.") // 03/21/2008 dont prompt while adding
			END IF
		ELSEIF lqastg = '5' AND Lqa_approve='B' THEN
			dw_qa_qastg.object.qastg[ll_rc]='5'
			dw_qa_qastg.object.bkseq[ll_rc]=Lbkseq
			dw_qa_qastg.object.bkmed[ll_rc]='DB'
			dw_qa_qastg.object.cntr[ll_rc]=Lcntr
			dw_qa_qastg.object.qarecdt[ll_rc]= Today()
		ELSEIF lqastg = '6' AND Lqa_approve='A' THEN
			dw_qa_qastg.object.qastg[ll_rc]='7'
			dw_qa_qastg.object.bkseq[ll_rc]=Lbkseq
			dw_qa_qastg.object.bkmed[ll_rc]=lbkmed
			dw_qa_qastg.object.cntr[ll_rc]=Lcntr
			dw_qa_qastg.object.qarecdt[ll_rc]= Today()
		ELSEIF lqastg = '6' AND Lqa_approve='B' THEN
			dw_qa_qastg.object.qastg[ll_rc]='6'
			dw_qa_qastg.object.bkseq[ll_rc]=Lbkseq
			dw_qa_qastg.object.bkmed[ll_rc]='DB'
			dw_qa_qastg.object.cntr[ll_rc]=Lcntr
			dw_qa_qastg.object.qarecdt[ll_rc]= Today()
		END IF
		//this.deleterow(ll_rc)
	ELSE
		dw_qa_qastg.object.bkseq[ll_rc]=Lbkseq
		dw_qa_qastg.object.bkmed[ll_rc]=lbkmed
		dw_qa_qastg.object.cntr[ll_rc]=Lcntr
		dw_qa_qastg.object.qarecdt[ll_rc]= Today()
	END IF

	// 08/18/2008 review type is manual for stage other than 0
	IF dw_qa_qastg.object.qastg[ll_rc] <> '0' THEN
		dw_qa_qastg.object.review_type_code[ll_rc] = 'M'
		dw_qa_qastg.object.review_type_code.protect = 1
		dw_qa_qastg.Object.review_type_code.Background.Color = RGB(192, 192, 192)
	ELSE
		dw_qa_qastg.object.review_type_code.protect = 0
		dw_qa_qastg.Object.review_type_code.Background.Color = RGB(255, 255, 255)
	END IF
	
	this.ScrollToRow(ll_rc)
	IF (lqastg='2' OR (lqastg = '3' AND Lqa_approve='A'))THEN
		wf_set_taborder_org()
		this.SetColumn("qastg")
	ELSE
		this.SetColumn("qarecdt")
	END IF
// Add a row if the contract is a duplicate contract and it has been rejected.
//ELSEIF Lcntrtype='M' AND (Lqa_approve='R' OR Lqa_approve='B') THEN
// If the contract type is M (volunteer contract) should allow another stage for QC4
ELSEIF lcntrtype='M' THEN
	// IF QC 4 is not done yet or last stage was rejected or was put on hold then
	IF (lqastg <> '4' OR Lqa_approve='R' OR Lqa_approve='B') THEN
		// Insert row
		IF IsValid (inv_rowmanager) THEN
			ll_rc = inv_rowmanager.EVENT pfc_addrow ()
		ELSE
			ll_rc = this.InsertRow (0) 
		END IF
		// Notify the Linkage Service that a new row has been added.
		IF IsValid (inv_linkage) THEN 
			inv_linkage.EVENT pfc_InsertRow (ll_rc) 
		END IF
	END IF
		
		
	IF lqastg = '1' AND (Lqa_approve='R' OR Lqa_approve='B') THEN
		dw_qa_qastg.object.qastg[ll_rc]='1'
		dw_qa_qastg.object.bkmed[ll_rc]=lbkmed
	// 03/17/2008 qc4 stand alone
	ELSEIF lqastg = '1' AND Lqa_approve='A' THEN
		dw_qa_qastg.object.qastg[ll_rc]='4'
		dw_qa_qastg.object.bkmed[ll_rc]='DB'
//	ELSEIF lqastg = '4' AND (Lqa_approve='R' OR Lqa_approve='B') THEN
//		dw_qa_qastg.object.qastg[ll_rc]='4'
//		dw_qa_qastg.object.bkmed[ll_rc]='DB'
	END IF
	
	IF (lqastg <> '4' OR Lqa_approve='R' OR Lqa_approve='B') THEN
		dw_qa_qastg.object.bkseq[ll_rc]=Lbkseq
		dw_qa_qastg.object.bkmed[ll_rc]=lbkmed
		dw_qa_qastg.object.cntr[ll_rc]=Lcntr
		dw_qa_qastg.object.qarecdt[ll_rc]= Today()
		
		this.ScrollToRow(ll_rc)
		this.SetColumn("qarecdt")
	END IF
END IF
this.ScrollToRow(ll_rc)
this.SetColumn("qastg")
m_pics_main.m_edit.m_deleterow.enabled 	=	TRUE
m_pics_main.m_edit.m_addrow.enabled		=	TRUE

RETURN ll_rc

end event

event pfc_insertrow;call super::pfc_insertrow;//
return -1
end event

event itemchanged;call super::itemchanged;//////////////////////////////////////////////////////////////////////////////////////////////////////////
//
//	Event: itemchanged for dw_qa_qastg
//
//	Description:
//	Cross field validations
//
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//	
//	Revision History
//
//	Developed by 	Date 				Purpose									Tracking#
//									
// Murali K.			02/21/2008      005 PICS Modifications Phase 2	 Reqs: QAS a.8.1, A.8.2
// Murali K.            03/17/2008		 PICS 2.0 				Validate various qa stages
// Murali K.		     08/18/2008 		FOR QC0 ENABLE REVIEW TYPE OR ELSE IT IS MANUAL 
// Murali K.			08/27/2008 		If in quarantine allow only manual qc0
// Murali K.			10/03/2008 		for qC7 only Rejection is allowd
// Murali K.			10/07/2008 		IF STATUS IS ENTERED AND THE USER CHANGES 
//											THE STAGE - VALIDATE??
// Murali K. 			10/22/2008 		ALLOW TO TO VIEW ALL REJECTION CODES IF CHANGED
//											FROM INITIAL TO REJECTION (status) FOR OLD HISTORICAL RECORD
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//

String Lmed, lbkmed, ls_statcd, ls_stg, ls_null, ls_userid, lsqacomments
Date null_date, tday
dwitemstatus l_status
string ls_status


SetNull(null_date)

SetNull(ls_null)
ls_userid=SqlServerTrans.userId
tday = Today()

IF DWO.Name = "qastg" THEN
	// 08/27/2008 if in quarantine allow only manual qc0
	IF data <> '0' AND il_qrs > 0 THEN
		dw_qa_qastg.object.qastg.Validationmsg='Book is in quarantine, Only manual QC0 is allowed.'
	     RETURN 1
	END IF		

	
	// 08/18/2008 fOR QC0 ENABLE REVIEW TYPE OR ELSE IT IS MANUAL 
	// #2213 3/16/10 for all manually entered stages review type is M including qC0
//	IF data <>  '0' THEN
		dw_qa_qastg.object.review_type_code.protect = 1
		dw_qa_qastg.Object.review_type_code.Background.Color = RGB(192, 192, 192)
		dw_qa_qastg.object.review_type_code[row] = 'M'
//	ELSE
//		dw_qa_qastg.object.review_type_code.protect = 0
//		dw_qa_qastg.Object.review_type_code.Background.Color = RGB(255, 255, 255)
//	END IF
 ////////////////////
 
	IF data = '6' THEN
		IF dw_qa_prod_review.object.flash_indicator[1] <> 'Y' THEN
			dw_qa_qastg.object.qastg.Validationmsg='Flash indicator is not set for this book and you may not choose QC 6 for this book.'
		     RETURN 1
		ELSE
			dw_qa_qastg.object.bkmed[row]='DB'
		END IF
	ELSEIF data = '5' OR data = '4' THEN
		dw_qa_qastg.object.bkmed[row]='DB'
	END IF
	
	// 10/07/2008 IF STATUS IS ENTERED AND THE USER CHANGES THE STAGE - VALIDATE??
	ls_statcd = dw_qa_qastg.object.qastatcd[row]
	IF Len(Trim(ls_statcd)) > 0 	THEN
		ls_stg = data // dw_qa_qastg.object.qastg[row]
		IF 	of_validateqastg(il_bkseq, is_cntr, is_bkmed,is_cntrtype,row,ls_stg) < 1 	THEN
			RETURN 1
		END IF
	END IF

	// 02/21/2008 Based on QA stages set reload indicators
	ib_rowfocuschanged=FALSE
	ls_statcd = this.object.qastatcd[row]
	of_setreload(data,row,ls_statcd)
	////////// 02/21/2008
	

ELSEIF DWO.Name = "qastatcd" THEN

	// 10/03/2008 for qC7 only Rejection is allowd
	IF this.object.qastg[row] = '7' AND data = 'A' THEN
		dw_qa_qastg.object.qastatcd.Validationmsg='Only Rejection is allowed for QC7.'
		RETURN 1
	END IF
	
	// 03/17/2008 validate qa stages before accepting entry
	ls_stg = dw_qa_qastg.object.qastg[row]
	IF 	of_validateqastg(il_bkseq, is_cntr, is_bkmed,is_cntrtype,row,ls_stg) < 1 	THEN
		RETURN 1
	END IF

	IF Data="A" OR Data="B" THEN
		dw_qa_qastg.object.qarejcd[row]=""
		// 03/21/2008
		dw_qa_qastg.Object.qarejcd.Tabsequence = 0
		// disable - do we need to gray it out
		dw_qa_qastg.Object.qarejcd.Background.Color = RGB(192, 192, 192)
		
		dw_qa_qastg.object.reload_indicator_yn.protect = 1
		dw_qa_qastg.Object.reload_indicator_yn.Background.Color = RGB(192, 192, 192)
		dw_qa_qastg.object.reload_indicator_yn[row] = 'N'
	ELSE
		dw_qa_qastg.Object.qarejcd.Tabsequence = 80
		// disable - do we need to gray it out
		dw_qa_qastg.Object.qarejcd.Background.Color = RGB(255, 255, 255)
		dw_qa_qastg.object.reload_indicator_yn.protect = 0
		dw_qa_qastg.Object.reload_indicator_yn.Background.Color = RGB(255, 255, 255)
	END IF
	
	IF Data="B" THEN
		dw_qa_narr.enabled=FALSE
		dw_qa_narr.Hide()
		//dw_qa_recagcy.Hide()
		//st_3.hide()
	ELSE
		Lmed = Trim(dw_qa_prod_review.object.mchar_med[1])
		lbkmed = Trim(dw_qa_prod_review.object.prod_bkmed[1])
		//Messagebox("med",lmed)
		IF Lmed <> 'P/B' AND Lmed <> 'BR' AND lbkmed <> 'DB' THEN
			dw_qa_narr.enabled=TRUE
			dw_qa_narr.Show()
			//dw_qa_recagcy.Show()
			//st_3.Show()
		END IF
	END IF
	// This part was added for PR #1337
	IF Data = 'A' THEN
		IF dw_qa_prod_review.object.mchar_g1br[1] <> 'Y'  AND dw_qa_prod_review.object.mchar_med[1] = 'BR' THEN
			dw_qa_prod_review.object.mchar_digitalprd[1] = 'W'
		END 	IF
	END IF
	// This part was added to allow books to be receive for QC4 
	IF Data = 'I' THEN
		IF dw_qa_qastg.object.qastg[row] <> '4'   THEN
			dw_qa_qastg.object.qastatcd.Validationmsg='Status I is only used to receive QC 4 books.'
			RETURN 1			
		END 	IF
	END IF
	
	// 10/22/2008 ALLOW TO TO VIEW ALL REJECTION CODES IF CHANGED FROM INITIAL TO REJECTION FOR OLD HISTORICAL RECORD
	IF Data = 'R' THEN
		IF this.object.qastatcd[row] = 'I' THEN // OLD STATUS
			ib_changedfrominitial =TRUE
		ELSE
			ib_changedfrominitial =FALSE
		END IF
	END IF
	//////////////
	
//	// 02/21/2008 if rejected force rejection reason
//	IF data = 'R' THEN
//		lsqacomments = this.object.qacomments[row]
//		IF Isnull(lsqacomments) OR Len(trim(lsqacomments)) = 0 THEN
//			dw_qa_qastg.object.qastatcd.Validationmsg='Please enter reason for rejection in the comments field'
//			RETURN 0
//		END IF
//	END IF
//	// 02/21/2008
	
	
	//House Keeping
	this.object.modified_date[row] = tday
	this.object.modified_by[row] = ls_userid
	
	// 03/17/2008 set reload indicator
	of_setreload(ls_stg,row,data)
	
ELSEIF DWO.Name = "qarejcd" THEN
	IF Data="" THEN
		RETURN 1
	END IF
	
	// 10/30/2008 allow only RR for old rejected rows
	l_status = This.GetItemStatus(row,0,Primary!)
	ls_status = this.object.qastatcd[row]
	IF ls_status = 'R' THEN
//		IF (l_status = notmodified! or l_status = datamodified!) AND ( NOT ib_changedfrominitial)  THEN
//			IF data <> 'RR' THEN
//				dw_qa_qastg.object.qarejcd.Validationmsg='Only Reverse Rejection code allowed'
//				RETURN 1				
//			END IF
//		END IF
	ELSEIF (ls_status = 'A'   OR ls_status = 'I') THEN
		// 11/06/2008
		dw_qa_qastg.object.qarejcd.Validationmsg='Rejection codes not allowed for this status'
		dw_qa_qastg.object.qarejcd[row]=''
		RETURN 1
	END IF
			
	//House Keeping
	this.object.modified_date[row] = tday
	this.object.modified_by[row] = ls_userid
	
ELSEIF DWO.Name = "qacompdt" THEN
	Date Lqarecdt,lqacompdt
	Int ldays,Ldays_tday
	
	Lqarecdt  = Date(dw_qa_qastg.object.qarecdt[row])
	lqacompdt  = Date(Left(Data,10))
	
	ldays = DaysAfter(Lqarecdt,lqacompdt)
	Ldays_tday = DaysAfter(Date(Today()),lqacompdt)
	
	IF (ldays < 0) THEN
		dw_qa_qastg.object.qacompdt.Validationmsg='Review date must be greater than or equal to received date.'
		RETURN 1
	ELSEIF (Date(Data) = null_date) THEN
		RETURN 1
	ELSEIF (Ldays_tday > 0) THEN
		dw_qa_qastg.object.qacompdt.Validationmsg='Review date may not be greater than today~'s date.'
		RETURN 1	
	ELSEIF (ldays > 365) THEN
		MessageBox("Warning","Review date is more than a year later than receive date.")
		RETURN 0
	END IF
	
	//House Keeping
	this.object.modified_date[row] = tday
	this.object.modified_by[row] = ls_userid
	
ELSEIF DWO.Name = "qainit" THEN
	String Linit
	Linit = Trim(Data)
	IF (IsNull(Linit) OR Linit="") THEN
		RETURN 1
	END IF
	
	//House Keeping
	this.object.modified_date[row] = tday
	this.object.modified_by[row] = ls_userid
	
	
ELSEIF DWO.Name = "qarecdt" THEN
	Lqarecdt  = Date(Left(Data,10))
	lqacompdt  = Date(dw_qa_qastg.object.qacompdt[row])
	ldays = DaysAfter(Lqarecdt,lqacompdt)
	IF (Date(Data) = null_date) THEN
		RETURN 1
	ELSEIF (Date(Left(Data,10)) > Date(Today())) THEN
		dw_qa_qastg.object.qarecdt.Validationmsg='Received date may not be greater than today~'s date.'
		RETURN 1		
//	ELSEIF NOT(f_qa_val_recv_dt()) THEN
//		dw_qa_qastg.Object.qarecdt.ValidationMsg='Received date may not be less than last stage~'s completion date.'
//		RETURN 1
	ELSEIF ldays<0 THEN
		dw_qa_qastg.object.qarecdt.Validationmsg='Review date must be greater than or equal to received date.'
		RETURN 1
	END IF
//	if row>1 then
//		Lqacompdt  = date(dw_qa_qastg.object.qacompdt[row -1])
//		lqarecdt=date(left(data,10))
//		if lqarecdt< lqacompdt then
//			dw_qa_qastg.Object.qarecdt.ValidationMsg=&
//			'The receive date of current row can not be less than review date of previous row.'
//			return 1
//		end if
//	end if
	//House Keeping
	this.object.modified_date[row] = tday
	this.object.modified_by[row] = ls_userid
	
ELSEIF DWO.Name = "qacomments" THEN
	Lmed=dw_qa_qastg.object.bkmed[row]
	ls_statcd=dw_qa_qastg.object.qastatcd[row]
	ls_stg=dw_qa_qastg.object.qastg[row]
	IF Lmed<>'BR' AND ls_statcd='A' AND ls_stg='2' THEN
		dw_qa_qastg.EVENT pfc_addrow()
		dw_qa_qastg.SetFocus()
		dw_qa_qastg.SetColumn('qastg')
	ELSEIF ls_statcd='R' AND ls_stg='2'  THEN
		dw_qa_qastg.EVENT pfc_addrow()
		dw_qa_qastg.SetFocus()
		dw_qa_qastg.SetColumn('qastg')
	ELSEIF ls_statcd='R' AND ls_stg='4' AND Lmed='DB' THEN
		dw_qa_qastg.EVENT pfc_addrow()
		dw_qa_qastg.SetFocus()
		dw_qa_qastg.SetColumn('qastg')
	END IF
	
	//House Keeping
	this.object.modified_date[row] = tday
	this.object.modified_by[row] = ls_userid
	
	
END IF
//wf_set_taborder_org()
end event

event editchanged;call super::editchanged;IF IsValid(inv_dropdownsearch) THEN
	this.inv_dropdownsearch.Event pfc_editchanged(row,dwo,data)
END IF
IF dwo.name = "qastatcd" THEN
	IF data="A" OR data="R" OR data="B" THEN
		this.SetColumn(8)
	END IF
END IF
end event

event doubleclicked;call super::doubleclicked;
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//
//	Event: doubleclicked for dw_qa_qastg
//
//	Description:
//	open rejection code and comments box
//
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//	
//	Revision History
//
//	Developed by 	Date 				Purpose									Tracking#
// 	Murali  K.			03/21/2008 - double click not getting to the right col
// 						08/27/2008 if in quarantine do not allow edits
//						 10/16/2008 FOR OLD ll_row ONLY RR - Reverse Rejection REJECTION CODE IS ALLOWED
// 						10/22/2008 if changed from Initial status to Rejected allow all other rejection codes
//						11/05/2008 code cleanup allow to change/edit rejection codes includes 'RR' - Reverse Rejection
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//

//11/05/2008
string ls_col, ls_rejcd, ls_status
long ll_row

ls_col = this.getcolumnname()
ll_row = this.getrow()
dwItemStatus l_status

// 08/27/2008 QUARANTINE CHECK
IF il_qrs < 1  THEN
	IF ls_col = "qarejcd" THEN
		l_status = This.GetItemStatus(ll_row,0,Primary!)
		ls_status = this.object.qastatcd[ll_row]
		// OPEN REJECTION CODE SELECTION ONLY IF IT IS REJECTED '' RR optional
		IF ls_status = 'R' THEN
			// 10/22/2008 if changed from Initial status to Rejected allow all other rejection codes
			ls_rejcd = this.object.qarejcd[ll_row] // pass the already selected rejection codes
			Message.stringparm = ls_rejcd
			Openwithparm(w_qa_popup_rejcode, ls_rejcd)
			dw_qa_qastg.object.qarejcd[ll_row]=Message.StringParm
		END IF
	ELSEIF dwo.name = "qacomments" THEN
			String QAComments
			dw_qa_qastg.AcceptText()
			QAComments = dw_qa_qastg.object.qacomments[ll_row]
			OpenWithParm(w_qa_popup_comments, QAComments)
			dw_qa_qastg.object.qacomments[ll_row]=Message.StringParm
	END IF
END IF




end event

event itemfocuschanged;string ls_stg, ls_med
long li_cnt

IF dwo.name = "qarejcd" THEN
	if dw_qa_qastg.object.qarejcd[row] = "" THEN
		RETURN 1
	end if
ELSEIF dwo.name = "qainit" THEN
	string Linit
	Linit = dw_qa_qastg.object.qainit[row]
	if (Linit = "" OR IsNull(Linit)) THEN
		RETURN 1
	end if
END IF

IF dwo.tag <> "" THEN
	SetMicroHelp(w_pics_main,dwo.tag)
ELSE
	SetMicroHelp(w_pics_main,"")
END IF

end event

event retrievestart;call super::retrievestart;openwithparm(w_pics_retrieve_msg_box,"Retrieving QA Information, Please Wait...")
end event

event retrieveend;call super::retrieveend;close(w_pics_retrieve_msg_box)
end event

event sqlpreview;call super::sqlpreview;//MessageBox("sql",sqlsyntax)

end event

event rowfocuschanged;call super::rowfocuschanged;////////////////////////////////////////////////////////////////////////////////////////////////////////////
////
////	Event: rowfocuschanged for dw_qa_qastg
////
////	Description:
////	Reload indicator settings
////
////////////////////////////////////////////////////////////////////////////////////////////////////////////
////	
////	Revision History
////
////	Developed by 	Date 				Version										Tracking#
////									
//// Murali K.			02/21/2008      005 PICS Modifications Phase 2	 Reqs: QAS a.8.1, A.8.2
//// Murali K.			03/17/2008		of_enabledisable to disable historical row
////////////////////////////////////////////////////////////////////////////////////////////////////////////
////
//string ls_stg,ls_status
//dwItemStatus l_status
//
//IF currentrow > 0 THEN
//	// 03/17/2008 disable old row
//	l_status = This.GetItemStatus(currentrow,0,Primary!)
//	IF l_status = notmodified! or l_status = datamodified! THEN
//		of_enabledisable(FALSE)
//
//	ELSE
//		of_enabledisable(TRUE)
//		
//		ls_stg = This.object.qastg[currentrow]
//		ls_status = This.object.qastatcd[currentrow]
//		of_setreload(ls_stg, currentrow,ls_status)
//	END IFrowocus
//	ib_rowfocuschanged=TRUE
//END IF
end event

event pfc_predeleterow;//////////////////////////////////////////////////////////////////////////////
//	Event:			pfc_predeleterow
//	Arguments:		None
//	Returns:			Integer
//	 					1 = Continue with delete
//  					0 = Prevent the actual delete.
//						-1 = error
//	Description:		Notification of a pending delete operation. if old record
//						prevent deletion
//////////////////////////////////////////////////////////////////////////////
//	Rev. History	Date			Version
//	Murali K. 			08/05/2008	 Phase-2				4.0   
//////////////////////////////////////////////////////////////////////////////
Long ll_row
dwitemstatus l_status

ll_row = This.getrow()
l_status = This.GetItemStatus( ll_row,0, Primary!)

if  l_status = notmodified!  then
	return PREVENT_ACTION
end if

return CONTINUE_ACTION
end event

event pfc_preupdate;call super::pfc_preupdate;//////////////////////////////////////////////////////////////////////////////////////////////////////////
//
//	Event: pfc_preupdate
//
//	Description:
//	Set audit column default values before updates
//
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//	
//	Revision History
//
//	Developed by 	Date 				Version						Tracking#
//									
// Murali K.			08/28/2008		Set audit columns while updating
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//
dwitemstatus l_status
long ll_rc, ll_loop
datetime ld1, ld2

ll_rc = this.rowcount()
FOR ll_loop = 1 TO ll_rc
	l_status = This.GetItemStatus( ll_loop,0, Primary!)
	IF l_status = new!  OR l_status = newmodified! THEN
		ld1 = this.object.qarecdt[ll_loop]
		ld2 = this.object.qacompdt[ll_loop]
		ld1 = datetime(date(ld1), now())
		ld2 = datetime(date(ld2), now())
		this.object.qarecdt[ll_loop] = ld1 
		this.object.qacompdt[ll_loop] = ld2 

//	// New row inserted add the house keeing
//	this.object.created_date[ll_loop] = today()
//	this.object.modified_date[ll_loop] = today()
//	this.object.created_by[ll_loop] = gnv_app.of_getuserid()
//	this.object.modified_by[ll_loop] = gnv_app.of_getuserid()
	
	END IF
NEXT
RETURN 1
end event

event clicked;call super::clicked;//////////////////////////////////////////////////////////////////////////////////////////////////////////
//
//	Event: rowfocuschanged for dw_qa_qastg
//
//	Description:
//	Reload indicator settings
//
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//	
//	Revision History
//
//	Developed by 	Date 				Version										Tracking#
//									
// Murali K.			02/21/2008      005 PICS Modifications Phase 2	 Reqs: QAS a.8.1, A.8.2
// Murali K.			03/17/2008		of_enabledisable to disable historical row
//Murali K.			11/05/2008		moved from rowfocuschanged event
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//
string ls_stg,ls_status
dwItemStatus l_status

IF row > 0 THEN
	// 03/17/2008 disable old row
	l_status = This.GetItemStatus(row,0,Primary!)
	IF l_status = notmodified! or l_status = datamodified! THEN
		of_enabledisable(FALSE)

	ELSE
		of_enabledisable(TRUE)
		
		ls_stg = This.object.qastg[row]
		ls_status = This.object.qastatcd[row]
		of_setreload(ls_stg, row,ls_status)
	END IF
	ib_rowfocuschanged=TRUE
END IF
end event

type dw_qa_narr from u_pics_dw within w_qa_product_review
event ue_enterkey pbm_dwnprocessenter
string tag = "Narrator"
integer x = 14
integer y = 888
integer width = 987
integer height = 248
integer taborder = 50
boolean bringtotop = true
string dataobject = "d_qa_narr"
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

event ue_postconstructor;call super::ue_postconstructor;dw_qa_narr.SetTransObject(sqlservertrans) 
this.of_SetDropDownSearch(TRUE)
this.inv_dropdownsearch.of_AddColumn("narr")
this.inv_dropdownsearch.of_AddColumn("narrfn")
this.GetChild ("narr", ldwc_narr)
end event

event pfc_addrow;long	ll_rc,Lbkseq
int cur_row
string Lbkmed

cur_row = dw_qa_narr.GetRow()
//messagebox('','pfc_addrow')
Lbkseq = long(em_bkno.text)
Lbkmed = dw_qa_prod_review.object.prod_bkmed[1]

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
	
dw_qa_narr.object.narr_bkseq[ll_rc]=Lbkseq

// 03/20/2008 NULL BOOK MEDIA CAN IT BE POSSIBLE
IF NOT Isnull(lbkmed) THEN
	dw_qa_narr.object.bkmed[ll_rc]=Lbkmed
ELSE
	dw_qa_narr.object.bkmed[ll_rc]='DB'
END IF

//dw_qa_narr.object.recagcy[ll_rc]=dw_qa_recagcy.object.recagcy[1]

this.ScrollToRow(ll_rc)
this.SetColumn("narr")


return ll_rc
end event

event pfc_insertrow;// 
return -1
end event

event itemfocuschanged;call super::itemfocuschanged;//messagebox('','itemfocuschenged')
IF IsValid(inv_dropdownsearch) THEN
	this.inv_dropdownsearch.Event pfc_ItemFocusChanged(row,dwo)
END IF

IF dwo.tag <> "" THEN
	SetMicroHelp(w_pics_main,dwo.tag)
ELSE
	SetMicroHelp(w_pics_main,"")
END IF


end event

event editchanged;call super::editchanged;IF IsValid(inv_dropdownsearch) THEN
	this.inv_dropdownsearch.Event pfc_editchanged(row,dwo,data)
END IF
end event

event itemchanged;call super::itemchanged;string lrecagcy, ls_null

SetNull(ls_null)
IF dwo.Name = "narr" THEN
	IF wf_validate_narr_lastname(data)=FALSE THEN
		dw_qa_narr.object.narr[row]=ls_null
		dw_qa_narr.object.narrfn[row]=ls_null
		cb_update.enabled=false
		RETURN 1
	END IF	
	IF data<>"" THEN
		data=TRIM(data)
		dw_qa_narr.object.narrfn[row]=TRIM(ldwc_narr.GetItemString(ldwc_narr.GetRow(),"narrfn"))
		cb_update.enabled=true
	ELSE
		dw_qa_narr.object.narrfn[row]=""	
		cb_update.enabled=false
	END IF
//	lrecagcy = TRIM(ldwc_narr.GetItemString(ldwc_narr.GetRow(),"recagcy"))
//	dw_qa_recagcy.SetItem(1, "recagcy", lrecagcy )
//	messagebox("recagcy",lrecagcy)
ELSEIF dwo.name = "narrfn" THEN
	IF wf_validate_narr(dw_qa_narr.object.narr[row],data)=FALSE THEN
		dw_qa_narr.object.narr[row]=ls_null
		dw_qa_narr.object.narrfn[row]=ls_null
		cb_update.enabled=false
		RETURN 1
	END IF	
END IF
		
end event

event sqlpreview;call super::sqlpreview;//messagebox("sql",sqlsyntax)
end event

event retrievestart;call super::retrievestart;openwithparm(w_pics_retrieve_msg_box,"Retrieving Narrator(s) Information, Please Wait...")
end event

event retrieveend;call super::retrieveend;close(w_pics_retrieve_msg_box)
end event

type st_2 from statictext within w_qa_product_review
integer x = 27
integer y = 204
integer width = 375
integer height = 76
boolean bringtotop = true
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long backcolor = 79741120
boolean enabled = false
string text = "Contract Number"
boolean focusrectangle = false
end type

type cb_narr from u_cb within w_qa_product_review
string tag = "Add Narrator"
integer x = 2386
integer y = 1008
integer width = 494
integer taborder = 0
string text = "&Narrator Manager..."
end type

event clicked;call super::clicked;open(w_qa_add_narr)
end event

event getfocus;call super::getfocus;IF this.tag <> "" THEN
	SetMicroHelp(w_pics_main,this.tag)
ELSE
	SetMicroHelp(w_pics_main,"")
END IF

end event

type dw_qa_cntr from u_pics_dw within w_qa_product_review
string tag = "Contract number"
integer x = 32
integer y = 288
integer width = 384
integer height = 96
integer taborder = 20
string dataobject = "d_qa_cntr"
boolean vscrollbar = false
boolean livescroll = false
boolean ib_isupdateable = false
boolean ib_rmbmenu = false
boolean ib_rmbfocuschange = false
string is_updatesallowed = ""
end type

event itemchanged;call super::itemchanged;int rtn,nav_point
string ls_cntr,Lmed,Lprodstage,Lprodstage2,Lcntrtype,Lcntrmed
long Lbkseq
string DWfilter

ls_cntr	= data
// designate the contract number
current_cntr = data
lbkseq = long(em_bkno.text)
 il_bkseq = Lbkseq // 03/17/2008
 
dw_qa_prod_review.Reset()
dw_qa_prod_review.InsertRow(0)
dw_qa_qastg.Reset()
dw_qa_qastg.InsertRow(0)
dw_qa_narr.Reset()
dw_qa_narr.InsertRow(0)

Open(w_pics_retrieve_msg_box)

select prodstage into :Lprodstage from prod where cntr=:ls_cntr and bkseq=:Lbkseq using sqlservertrans;
Lprodstage2 = Lprodstage

select cntrtype,cntrmed into :Lcntrtype,:Lcntrmed from ancntr where cntr = :ls_cntr using sqlservertrans;
IF Lcntrtype='Z' THEN
	Lcntrmed='RC'
END IF
IF Lcntrmed = 'RC' AND Lcntrtype = 'D' THEN
	Lcntrmed = 'RTB'
END IF

IF Lprodstage = 'ZM' and Lprodstage2='ZM' THEN
	Lprodstage = 'MA'
END IF

// Set 03/17/2008
 is_cntrtype = Lcntrtype
 is_cntr = ls_cntr
 is_prodstage = Lprodstage
 is_cntrmed = Lcntrmed


rtn = dw_qa_prod_review.Retrieve(Lbkseq,ls_cntr,Lprodstage,Lprodstage2)
Close(w_pics_retrieve_msg_box)

IF rtn > 0 THEN
//	dw_qa_recagcy.Retrieve()
	Lmed = dw_qa_prod_review.object.prod_bkmed[1]
	// 03/17/2008 set ins variable
	is_bkmed = lmed
	
	wf_validate_med(Lmed)
	rtn = dw_qa_qastg.Retrieve(Lbkseq,ls_cntr)
	IF rtn = 0 THEN
		dw_qa_qastg.InsertRow(1)
		IF Lmed="BR" THEN
			dw_qa_prod_review.object.mchar_len_t.text = "Pages"
			dw_qa_prod_review.Object.mchar_len.ValidationMsg='You must enter the number of pages.'
			dw_qa_prod_review.object.mchar_vols_t.text = "Volumes"
			dw_qa_prod_review.Object.mchar_vols.ValidationMsg='You must enter the number of volumes.'
			dw_qa_prod_review.object.mchar_vols.TabSequence = 20
			dw_qa_prod_review.object.mchar_minlastside.TabSequence = 0
			dw_qa_prod_review.object.mchar_minlastside.Background.Color=RGB(192,192,192)
			dw_qa_prod_review.Object.mchar_minlastside.Edit.Required='No'
//			dw_qa_qastg.object.qastg[1]='2'
			dw_qa_qastg.object.qarecdt[1]= today()
			dw_qa_qastg.object.bkseq[1]=Lbkseq
			dw_qa_qastg.object.bkmed[1]=Lmed
			dw_qa_qastg.object.cntr[1]=ls_cntr
			dw_qa_narr.Enabled=FALSE
			dw_qa_narr.Hide()
//			dw_qa_recagcy.Hide()
//			st_3.hide()
			dw_qa_qastg.setcolumn("qarecdt")
			DWfilter = "mchar_med  = 'BR' OR mchar_med = 'P/B'"
			dw_qa_prod_review.SetFilter(DWfilter)
			dw_qa_prod_review.Filter( )

			dw_qa_prod_review.object.mchar_len_t.text = "Minutes"
	//		dw_qa_qastg.object.qastg[1]='4'
			dw_qa_qastg.object.qarecdt[1]= today()
			dw_qa_qastg.object.bkseq[1]=Lbkseq
			dw_qa_qastg.object.bkmed[1]=Lmed
			dw_qa_qastg.object.cntr[1]=ls_cntr
			dw_qa_narr.Enabled=FALSE
			dw_qa_narr.Hide()
                     // Set the required column to FALSE so you are able to switch datawindows
			dw_qa_prod_review.Object.mchar_len.Edit.Required = FALSE
			DWfilter = "mchar_med  = 'RC' OR mchar_med = 'RTB'"
			dw_qa_prod_review.SetFilter(DWfilter)
			dw_qa_prod_review.Filter( )
			// Reset the required column back as TRUE
			dw_qa_prod_review.Object.mchar_len.Edit.Required = TRUE


			dw_qa_qastg.setcolumn("qarecdt")
			select units into :nav_point from prod
			where bkseq = :lbkseq
			and cntr = :ls_cntr
			and prodstage in ( 'XD','XE')
			using sqlservertrans;
			dw_qa_prod_review.object.prod_units[1] = nav_point
			
		ELSE
			dw_qa_prod_review.object.mchar_len_t.text = "Minutes"
			dw_qa_prod_review.Object.mchar_len.ValidationMsg='You must enter the number of Minutes.'
			dw_qa_prod_review.object.mchar_vols_t.text = "Cassettes"
			dw_qa_prod_review.Object.mchar_vols.ValidationMsg='You must enter the number of Cassettes.'
			dw_qa_prod_review.object.mchar_vols.TabSequence = 20
			dw_qa_prod_review.Object.mchar_minlastside.Edit.Required='Yes'
			dw_qa_prod_review.object.mchar_minlastside.TabSequence = 30
			dw_qa_prod_review.object.mchar_minlastside.Background.Color=RGB(255,255,255)
			if Lcntrtype='D' THEN
//				dw_qa_qastg.object.qastg[1]='2'
			elseif Lcntrtype='A' THEN
//				dw_qa_qastg.object.qastg[1]='4'
			else
//				dw_qa_qastg.object.qastg[1]='1'
			end if
			dw_qa_qastg.object.qarecdt[1]= today()
			dw_qa_qastg.object.bkseq[1]=Lbkseq
			dw_qa_qastg.object.bkmed[1]=Lmed
			dw_qa_qastg.object.cntr[1]=ls_cntr
			dw_qa_narr.Enabled=TRUE
			dw_qa_narr.Show()
//			dw_qa_recagcy.Show()
//			st_3.Show()
			dw_qa_qastg.setcolumn("qarecdt")
                     // Set the required column to FALSE so you are able to switch datawindows
			dw_qa_prod_review.Object.mchar_len.Edit.Required = FALSE
			DWfilter = "mchar_med  = 'RC' OR mchar_med = 'RTB'"
			dw_qa_prod_review.SetFilter(DWfilter)
			dw_qa_prod_review.Filter( )
			// Reset the required column back as TRUE
			dw_qa_prod_review.Object.mchar_len.Edit.Required = TRUE
		END IF
		dw_qa_prod_review.SetFocus()
	ELSEIF rtn > 0 THEN
		IF Lmed="BR" OR Lmed="P/B" THEN
			DWfilter = "mchar_med  = 'BR' OR mchar_med = 'P/B'"
			dw_qa_prod_review.SetFilter(DWfilter)
			dw_qa_prod_review.Filter( )
			dw_qa_prod_review.object.mchar_len_t.text = "Pages"
			dw_qa_prod_review.object.mchar_vols_t.text = "Volumes"
			dw_qa_prod_review.object.mchar_vols.TabSequence = 20
			dw_qa_prod_review.object.mchar_minlastside.TabSequence = 0
			dw_qa_prod_review.object.mchar_minlastside.Background.Color=RGB(192,192,192)
			dw_qa_narr.Enabled=FALSE
			dw_qa_narr.Hide()
		ELSEIF Lmed="DB" THEN
			DWfilter = "mchar_med  = 'RC' OR mchar_med = 'RTB'"
			dw_qa_prod_review.SetFilter(DWfilter)
			dw_qa_prod_review.Filter( )
			dw_qa_prod_review.object.mchar_len_t.text = "Minutes"
			dw_qa_narr.Enabled=FALSE
			dw_qa_narr.Hide()
		ELSEIF Lmed="RC" OR Lmed="FD" THEN
			DWfilter = "mchar_med  = 'RC' OR mchar_med = 'RTB'"
			dw_qa_prod_review.SetFilter(DWfilter)
			dw_qa_prod_review.Filter( )
			dw_qa_prod_review.object.mchar_len_t.text = "Minutes"
			dw_qa_prod_review.object.mchar_vols_t.text = "Cassettes"
			dw_qa_prod_review.object.mchar_vols.TabSequence = 20
			dw_qa_prod_review.object.mchar_minlastside.TabSequence = 30
			dw_qa_prod_review.object.mchar_minlastside.Background.Color=RGB(255,255,255)
			dw_qa_narr.Enabled=TRUE
			dw_qa_narr.Show()
//			dw_qa_recagcy.Show()
//			st_3.Show()
		END IF
		// If the qastg has gone through all the three stages of QA,
		// Do not add any rows into qastg datawindow.
		CHOOSE CASE Lmed
		CASE "BR"
			IF (dw_qa_qastg.object.qastg[rtn]='2' AND &
				dw_qa_qastg.object.qastatcd[rtn]='R') THEN
				dw_qa_qastg.event pfc_addrow()
			ELSEIF (dw_qa_qastg.object.qastg[rtn]<>'2' AND &
				dw_qa_qastg.object.qastatcd[rtn]<>'A') THEN
				dw_qa_qastg.event pfc_addrow()
			END IF
		CASE "DB"
			IF (dw_qa_qastg.object.qastg[rtn]='4' AND &
				dw_qa_qastg.object.qastatcd[rtn]='R') THEN
				dw_qa_qastg.event pfc_addrow()
			END IF
		CASE ELSE
			IF (dw_qa_qastg.object.qastg[rtn]<>'3' OR &
				dw_qa_qastg.object.qastatcd[rtn]<>'A') THEN
				dw_qa_qastg.event pfc_addrow()
			END IF
		END CHOOSE
	END IF
	
	// in house comments
	
	// narration
	IF Lmed="RC" OR Lmed="FD" THEN
		rtn = dw_qa_narr.Retrieve(Lbkseq)
	END IF
	IF Lcntrtype='D' THEN
		// If the book is for duplication only, then don't allow any update to
		// minlastside, Minutes, narr and studio.
		dw_qa_narr.Enabled = FALSE
		dw_qa_prod_review.Enabled = FALSE
	ELSE
		// Book is not for duplication only.
		dw_qa_narr.Enabled = TRUE
		dw_qa_prod_review.Enabled = TRUE
	END IF
END IF

dw_qa_qastg.SetFocus()
em_bkno.Enabled = FALSE
cb_update.Enabled = TRUE
cb_clear.Enabled = TRUE
cb_narr.Enabled = TRUE
cb_find.Enabled = FALSE
end event

event pfc_addrow;//
RETURN -1
end event

event pfc_deleterow;//
RETURN -1
end event

event pfc_insertrow;//
RETURN -1
end event

event ue_postconstructor;call super::ue_postconstructor;this.of_SetTransObject(sqlservertrans) 
this.of_setupdateable(FALSE)
dw_qa_cntr.GetChild("cntr",ldwc_cntr)
end event

event rbuttondown;//
end event

event rbuttonup;//
end event

event pfc_populatedddw;call super::pfc_populatedddw;long Lbkseq

Lbkseq = long(em_bkno.text)

IF adwc_obj.SetTransObject(SQLServerTrans) = -1 THEN
	Return -1
ELSE
	Return adwc_obj.Retrieve(Lbkseq)
END IF
end event

event retrievestart;call super::retrievestart;this.Event pfc_PopulateDDDW("cntr",ldwc_cntr)

end event

event itemfocuschanged;call super::itemfocuschanged;IF dwo.tag <> "" THEN
	SetMicroHelp(w_pics_main,dwo.tag)
ELSE
	SetMicroHelp(w_pics_main,"")
END IF

end event

type dw_qa_prod_review from u_pics_dw within w_qa_product_review
event ue_enterkey pbm_dwnprocessenter
string tag = "Production information"
integer x = 439
integer y = 32
integer width = 2446
integer height = 388
integer taborder = 30
string dataobject = "d_qa_prod_review"
boolean livescroll = false
boolean ib_isupdateable = false
boolean ib_rmbmenu = false
boolean ib_rmbfocuschange = false
string is_updatesallowed = ""
end type

event ue_enterkey;call super::ue_enterkey;Send(Handle(this),256,9,Long(0,0))
return(1)

end event

event ue_postconstructor;call super::ue_postconstructor;dw_qa_prod_review.SetTransObject(sqlservertrans) 

end event

event itemchanged;call super::itemchanged;int length,volumn,qnty,totunits,minlastside,rownum, ll_tracks
string media,prodstage,cntrtype, ls_bkmed

// Get the number of rows in QASTG table
rownum = dw_qa_qastg.RowCount()

// IF dwo.name = "mchar_len" THEN
IF dwo.name = "mchar_length_" THEN // 06/19/2008
	
	// 11/05/2008
//	IF wf_find_approved_qastg1(rownum) THEN
//		THIS.Object.mchar_len.ValidationMsg='QA Stage 1 has been approved, therefore length can not be changed.'
//		RETURN 1
//	END IF
	
	IF data="" OR ISNull(data) THEN
		RETURN 1
	END IF
	length 			= Integer(data)	
	
	//12/03/2008  set minutes for RC and DB rows if changed for one
	long ll_rc, ll_loop
	ll_rc = this.rowcount()
	FOR ll_loop = 1 TO ll_rc
		IF ll_loop <> row THEN
			this.Object.mchar_LENGTH_[ll_loop] = length
		END IF
			//12/03/2008
	
			media 			= this.Object.mchar_med[ll_loop]
			ls_bkmed 		= this.Object.mchar_bkmed[ll_loop]
			prodstage 		= this.Object.prod_prodstage[ll_loop]
			cntrtype 		= this.Object.ancntr_cntrtype[ll_loop]
			qnty 				= this.Object.mchar_qnty[ll_loop]
			minlastside 	= this.object.mchar_minlastside[ll_loop]
			volumn   	     = this.object.mchar_vols[ll_loop]
			
	CHOOSE CASE media

		CASE "RTB"
				ls_bkmed 		= this.Object.mchar_bkmed[LL_LOOP]					
				IF ls_bkmed = 'RC' THEN
					ll_tracks =  Ceiling(length / 88)
					volumn = Ceiling(ll_tracks / 4)
					this.Object.mchar_len[ll_loop] = ll_tracks
					this.Object.mchar_vols[ll_loop] = volumn
					this.object.mchar_minlastside[ll_loop] = Mod(length,88)
				END IF
				// 12/4/2008
		CASE 'BR'
				this.Object.mchar_len[ll_loop] =length // 12/4/2008
				volumn = round(length / 250,0)
				this.Object.mchar_vols[ll_loop] = volumn
		CASE "P/B"
		CASE ELSE
		END CHOOSE
	
		minlastside 	= this.object.mchar_minlastside[ll_loop]
		totunits = wf_calculate_units(media,cntrtype,length,qnty,minlastside,volumn,ll_loop)
		IF totunits = -1 THEN
			RETURN 1
		ELSE
			this.object.prod_units[ll_loop] = totunits
		END IF
	Next
	//12/03/2008 Minutes/Volumes updates

ELSEIF dwo.name = "mchar_minlastside" THEN
	IF wf_find_approved_qastg1(rownum) THEN
		THIS.Object.mchar_minlastside.ValidationMsg='QA Stage 1 has been approved, therefore minutes last side can not be changed.'
		RETURN 1
	END IF

	IF data="" OR ISNull(data) THEN
		RETURN 1
	END IF
	minlastside 	= Integer(data)
	IF minlastside > 88 THEN
		return 1
	END IF
	length 			= this.object.mchar_len[row]
	media 			= this.Object.mchar_med[row]
	prodstage 		= this.Object.prod_prodstage[row]
	cntrtype 		= this.Object.ancntr_cntrtype[row]
	qnty 				= this.Object.mchar_qnty[row]
	volumn 		     = this.Object.mchar_vols[row]
//	totunits = wf_calculate_units(media,cntrtype,length,qnty,minlastside,volumn)
	IF totunits = -1 THEN
		RETURN 1
	ELSE
		this.object.prod_units[row] = totunits
	END IF
	wf_settab_order_noqastg()
ELSEIF dwo.name = "mchar_vols" THEN
	// 11/05/2008 do not validate for qc1 approved, they might do it after qc5 also
//	IF wf_find_approved_qastg1(rownum) THEN
//		THIS.Object.mchar_vols.ValidationMsg='QA Stage 1 has been approved, therefore vols can not be changed.'
//		RETURN 1
//	END IF
	wf_settab_order_noqastg()
END IF
end event

event pfc_addrow;//
RETURN -1
end event

event pfc_deleterow;//
RETURN -1
end event

event pfc_insertrow;//
RETURN -1
end event

event rbuttondown;//
end event

event rbuttonup;//
end event

event retrievestart;call super::retrievestart;openwithparm(w_pics_retrieve_msg_box,"Retrieving Media Information, Please Wait...")
end event

event retrieveend;call super::retrieveend;// display the correct row if there are more than one row
//if rowcount > 1 then
//	string ls_cntr,ls_filter,ls_media
//	ls_cntr  =  dw_qa_cntr.object.cntr[1]
//	ls_media = upper(right(ls_cntr,2))
//	ls_filter = "mchar_med = '"+ ls_media + "'"
//	dw_qa_prod_review.setFilter(ls_filter)
//	dw_qa_prod_review.Filter()
//end if
close(w_pics_retrieve_msg_box)
IF  wf_find_approved_qastg1(dw_qa_qastg.RowCount()) THEN
	dw_qa_prod_review.Object.mchar_len.Background.Color=RGB(192,192,192)
ELSEIF rowcount > 0 THEN
	IF this.object.mchar_med[1] = 'RTB' and NOT(IsNull(this.object.other_media_conno[1])) THEN
		// 06/20/2008 flash indicator already retrieved no need to set
//		string lflash_ind,lotherconno
//		lotherconno = this.object.other_media_conno[1]
//		SELECT flash_indicator INTO :lflash_ind FROM MCHAR WHERE conno = :lotherconno USING SQLServerTrans;
//		IF NOT(IsNull(lflash_ind)) AND lflash_ind = 'Y' THEN
//			this.object.flash_indicator[1] = 'Y'
//		END IF
	END IF
END IF

end event

event sqlpreview;call super::sqlpreview;//messagebox("sql",sqlsyntax)
end event

event itemfocuschanged;call super::itemfocuschanged;IF dwo.tag <> "" THEN
	SetMicroHelp(w_pics_main,dwo.tag)
ELSE
	SetMicroHelp(w_pics_main,"")
END IF

end event

event rowfocuschanged;call super::rowfocuschanged;// 06/19/2008
string ls_bkmed

if this.rowcount() > 0 THEN
	ls_bkmed = this.object.mchar_bkmed[currentrow]
	CHOOSE CASE ls_bkmed
		CASE 'RC'
			this.object.mchar_vols_t.Text = 'Cassettes'
			// IF CHECKED IN BOOK DO ALLOW TO UPDATE CASSETTES VOLS 12/03/2008 MINUTES /VOLS UPDATE
			IF ib_checkedin THEN
				this.Object.mchar_vols.Background.Color = RGB(192, 192, 192)
				this.Object.mchar_vols.Tabsequence = 0
			ELSE
				this.Object.mchar_vols.Background.Color = RGB(255, 255, 255)
				this.Object.mchar_vols.Tabsequence = 20
			END IF
		CASE 'DB'
			this.object.mchar_vols_t.Text = 'Cartridge'
			this.Object.mchar_vols.Background.Color = RGB(255, 255, 255)
			this.Object.mchar_vols.Tabsequence = 20
	END CHOOSE
END IF


end event

event scrollvertical;call super::scrollvertical;// 06/19/2008
string ls_bkmed

if this.rowcount() > 0 THEN
	ls_bkmed = this.object.mchar_bkmed[this.getrow()]
	CHOOSE CASE ls_bkmed
		CASE 'RC'
			this.object.mchar_vols_t.Text = 'Cassettes'
		CASE 'DB'
			this.object.mchar_vols_t.Text = 'Cartridge'
	END CHOOSE
END IF

end event


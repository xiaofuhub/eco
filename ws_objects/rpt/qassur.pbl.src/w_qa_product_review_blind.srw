$PBExportHeader$w_qa_product_review_blind.srw
forward
global type w_qa_product_review_blind from w_sheet
end type
type cb_add_row from commandbutton within w_qa_product_review_blind
end type
type mle_comments from uo_mle within w_qa_product_review_blind
end type
type cb_spell from commandbutton within w_qa_product_review_blind
end type
type cb_updateweb from u_cb within w_qa_product_review_blind
end type
type cb_add_narr from u_cb within w_qa_product_review_blind
end type
type cb_comments from u_cb within w_qa_product_review_blind
end type
type st_4 from statictext within w_qa_product_review_blind
end type
type cb_find from u_cb within w_qa_product_review_blind
end type
type cb_exit from u_cb within w_qa_product_review_blind
end type
type cb_clear from u_cb within w_qa_product_review_blind
end type
type cb_update from u_cb within w_qa_product_review_blind
end type
type st_1 from statictext within w_qa_product_review_blind
end type
type em_bkno from uo_conno within w_qa_product_review_blind
end type
type dw_qa_prod_review from u_pics_dw within w_qa_product_review_blind
end type
type dw_qa_qastg from u_pics_dw within w_qa_product_review_blind
end type
type dw_qa_narr from u_pics_dw within w_qa_product_review_blind
end type
type cb_narr from u_cb within w_qa_product_review_blind
end type
type st_3 from statictext within w_qa_product_review_blind
end type
type dw_qa_recagcy from u_pics_dw within w_qa_product_review_blind
end type
type dw_qa_cntr from u_pics_dw within w_qa_product_review_blind
end type
end forward

global type w_qa_product_review_blind from w_sheet
integer x = 9
integer y = 4
integer width = 2624
integer height = 1352
string title = "Quality Assurance (Product Review with window eyes)"
windowstate windowstate = maximized!
cb_add_row cb_add_row
mle_comments mle_comments
cb_spell cb_spell
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
dw_qa_prod_review dw_qa_prod_review
dw_qa_qastg dw_qa_qastg
dw_qa_narr dw_qa_narr
cb_narr cb_narr
st_3 st_3
dw_qa_recagcy dw_qa_recagcy
dw_qa_cntr dw_qa_cntr
end type
global w_qa_product_review_blind w_qa_product_review_blind

type variables
string local_recagcy,Lwineyes
DataWindowChild ldwc_narr,ldwc_cntr
boolean comments_exist=FALSE


end variables

forward prototypes
public function boolean wf_find_approved_qastg2 (integer rownum)
public function boolean wf_validate_bkno (long lbkno)
public function boolean wf_validate_initial (string linit)
public subroutine wf_validate_med (string media)
public function boolean wf_is_qstage1_approved ()
public function boolean wf_validate_recagcy (string lrecagcy)
public subroutine wf_reset_update ()
public subroutine wf_set_taborder_org ()
public subroutine wf_settab_order_noqastg ()
public function boolean wf_validate_narr (string lnarrln, string lnarrfn)
public function boolean wf_validate_narr_lastname (string lnarrln)
public function boolean wf_isjawsrunning ()
public function integer wf_calculate_units (string media, string prodstage, string cntrtype, integer len, integer qnty, integer minlastside, integer volumn)
end prototypes

public function boolean wf_find_approved_qastg2 (integer rownum);int i
FOR i=1 TO rownum 
	IF (dw_qa_qastg.object.qastg[i]='2' AND & 
		(dw_qa_qastg.object.qastatcd[i]='A' OR dw_qa_qastg.object.qastatcd[i]='R' OR dw_qa_qastg.object.qastatcd[i]='B') ) THEN
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

public subroutine wf_reset_update ();dw_qa_narr.ResetUpdate()
dw_qa_prod_review.ResetUpdate()
dw_qa_qastg.ResetUpdate()
//dw_qa_recagcy.ResetUpdate()

end subroutine

public subroutine wf_set_taborder_org ();dw_qa_qastg.Object.qastg.tabsequence='10'   
dw_qa_qastg.Object.qarecdt.tabsequence='20'   
dw_qa_qastg.Object.qacompdt.tabsequence='30'   
dw_qa_qastg.Object.qastatcd.tabsequence='40'   
dw_qa_qastg.Object.qainit.tabsequence='50'   
dw_qa_qastg.Object.qarejcd.tabsequence='60'   
dw_qa_qastg.Object.qacomments.tabsequence='70'   

end subroutine

public subroutine wf_settab_order_noqastg ();dw_qa_qastg.Object.qastg.tabsequence='0'   
dw_qa_qastg.Object.qarecdt.tabsequence='10'   
dw_qa_qastg.Object.qacompdt.tabsequence='20'   
dw_qa_qastg.Object.qastatcd.tabsequence='30'   
dw_qa_qastg.Object.qainit.tabsequence='40'   
dw_qa_qastg.Object.qarejcd.tabsequence='50'   
dw_qa_qastg.Object.qacomments.tabsequence='60'   

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

public function boolean wf_isjawsrunning ();boolean lb_return
String  	ls_WindowName

lb_return = false

//Check if Window Eyes is Running
ls_windowName = 'Window-Eyes'
IF FindWindowW (0, ls_windowName) <> 0 THEN 	
	lb_return = TRUE
END IF

Return lb_Return

end function

public function integer wf_calculate_units (string media, string prodstage, string cntrtype, integer len, integer qnty, integer minlastside, integer volumn);int Vols,Lmaxstageorder,SubUnits
Long Lbkseq,Units
String Lcntr,Lmed, Lbkmed

Lmed = TRIM(media)
//IF Lmed='P/B' THEN
//	Lmed='BR'
//END IF

// Use Lmaxstageorder to find out how many stages will it takes
// to complete the production of each of these books.
select max(stageorder) into :Lmaxstageorder from prodstage
	where cntrtype = :cntrtype
	and	cntrmed = :media
	Using sqlservertrans;
// If it take more than one stage, two stages. calculate the units for duplication
// and update the coresponding record and then calculate the units for
// mastering.
IF Lmaxstageorder = 2 THEN
	IF Lmed = 'RC' THEN
		// First calculate the duplication units for Cassettes
		Vols = Ceiling(len/4)
		dw_qa_prod_review.object.mchar_vols[1] = Vols
		Units = Vols * qnty
		
		Lbkseq = dw_qa_prod_review.object.prod_bkseq[1]
		Lcntr = dw_qa_prod_review.object.ancntr_cntr[1]

		// Update the prod table for duplication of this RC book with
		// the correct number of units.
		
		Update prod set units = :Units, subunits = :Vols
			where bkseq = :Lbkseq
			and	bkmed = :Lmed
			and	cntr 	= :Lcntr
			and	prodstage = 'DU'
		using sqlservertrans;
		IF f_check_dberror(sqlservertrans,"PROD")=FALSE THEN
			RETURN -1
		END IF
		
		// Secondly calculate the mastering units for cassettes
		IF IsNull(minlastside)=FALSE THEN
			Units = ((len - 1) * 88) + minlastside
		ELSE
			Units = ((len - 1) * 88)
		END IF
		SubUnits = Ceiling(len/2)
		
		Update prod set units = :Units,subunits = :SubUnits
			where bkseq = :Lbkseq
			and	bkmed = :Lmed
			and	cntr 	= :Lcntr
			and	prodstage in ('MA','AB')
		using sqlservertrans;
		IF f_check_dberror(sqlservertrans,"PROD")=FALSE THEN
			RETURN -1
		END IF
	ELSEIF Lmed = 'BR' OR Lmed = 'P/B' THEN
		// First calculate the duplication units for Braille
		IF Lmed = 'BR' THEN
			Units = len * qnty
		ELSEIF Lmed = 'P/B' THEN
			Units = qnty
		END IF
		
		SubUnits = Volumn * qnty
		
		Lbkseq = dw_qa_prod_review.object.prod_bkseq[1]
		Lcntr = dw_qa_prod_review.object.ancntr_cntr[1]
		
		// Update the prod table for duplication of this BR or P/B book with
		// the correct number of units.
		
		Update prod set units = :Units,subunits = :SubUnits
			where bkseq = :Lbkseq
			and	bkmed = 'BR'
			and	cntr 	= :Lcntr
			and	prodstage in ('PR','PB','EM')
		using sqlservertrans;
		IF f_check_dberror(sqlservertrans,"PROD")=FALSE THEN
			RETURN -1
		END IF
		
		// Secondly calculate the mastering units for Braille or Print Braille
		Units = len
		Update prod set units = :Units
			where bkseq = :Lbkseq
			and	bkmed = :Lmed
			and	cntr 	= :Lcntr
			and	prodstage in ('MA','AB','PU')
		using sqlservertrans;
		IF f_check_dberror(sqlservertrans,"PROD")=FALSE THEN
			RETURN -1
		END IF
	END IF
//RTB Books MA, ZM and DU
ELSEIF Lmaxstageorder = 3 THEN
	IF Lmed = 'RC' OR Lmed = 'RTB' THEN
		// First calculate the duplication units for Cassettes
		Vols = Ceiling(len/4)
		dw_qa_prod_review.object.mchar_vols[1] = Vols
		Units = Vols * qnty
		
		Lbkseq = dw_qa_prod_review.object.prod_bkseq[1]
		Lbkmed = dw_qa_prod_review.object.prod_bkmed[1]
		Lcntr = dw_qa_prod_review.object.ancntr_cntr[1]

		// Update the prod table for duplication of this RC book with
		// the correct number of units.
		
		Update prod set units = :Units, subunits = :Vols
			where bkseq = :Lbkseq
			and	bkmed = :Lbkmed
			and	cntr 	= :Lcntr
			and	prodstage = 'DU'
		using sqlservertrans;
		IF f_check_dberror(sqlservertrans,"PROD")=FALSE THEN
			RETURN -1
		END IF
		
		// Secondly calculate the mastering units for cassettes
		IF IsNull(minlastside)=FALSE THEN
			Units = ((len - 1) * 88) + minlastside
		ELSE
			Units = ((len - 1) * 88)
		END IF
		SubUnits = len
		
		Update prod set units = :Units,subunits = :SubUnits
			where bkseq = :Lbkseq
			and	bkmed = :Lbkmed
			and	cntr 	= :Lcntr
			and	prodstage in ('MA','AB')
		using sqlservertrans;
		IF f_check_dberror(sqlservertrans,"PROD")=FALSE THEN
			RETURN -1
		END IF
		
		// Thirdly calculate the Z Mastering units for cassettes
		IF IsNull(minlastside)=FALSE THEN
			Units = ((len - 1) * 88) + minlastside
		ELSE
			Units = ((len - 1) * 88)
		END IF
		SubUnits = 1
		
		Update prod set units = :Units,subunits = :SubUnits
			where bkseq = :Lbkseq
			and	bkmed = 'DB'
			and	cntr 	= :Lcntr
			and	prodstage = 'ZM'
		using sqlservertrans;
		IF f_check_dberror(sqlservertrans,"PROD")=FALSE THEN
			RETURN -1
		END IF

	END IF
ELSEIF  Lmaxstageorder = 1 THEN
	
	IF cntrtype = 'M' THEN
		IF Lmed = 'RC' THEN
			// Calculate the mastering units for cassettes
			IF IsNull(minlastside)=FALSE THEN
				Units = ((len - 1) * 88) + minlastside
			ELSE
				Units = ((len - 1) * 88)
			END IF
			SubUnits = Ceiling(len/2)
			Update prod set units = :Units, subunits = :SubUnits
				where bkseq = :Lbkseq
				and	bkmed = :Lmed
				and	cntr 	= :Lcntr
				and	prodstage in ('MA','AB')
			using sqlservertrans;
			IF f_check_dberror(sqlservertrans,"PROD")=FALSE THEN
				RETURN -1
			END IF
		ELSEIF Lmed = 'BR' OR Lmed = 'P/B' THEN
			// Calculate the mastering units for Braille
			IF Lmed = 'BR' THEN
				Units = len
			ELSEIF Lmed = 'P/B' THEN
				Units = qnty
			END IF
			
			Update prod set units = :Units
				where bkseq = :Lbkseq
				and	bkmed = :Lmed
				and	cntr 	= :Lcntr
				and	prodstage in ('MA','AB','PU')
			using sqlservertrans;
			IF f_check_dberror(sqlservertrans,"PROD")=FALSE THEN
				RETURN -1
			END IF
		END IF
		
	ELSEIF cntrtype = 'D' THEN
		
		IF Lmed = 'RC' THEN
			// Calculate the duplication units for cassettes
			Vols = Ceiling(len/4)
			Units = Vols * qnty
			Update prod set units = :Units, subunits = :Vols
				where bkseq = :Lbkseq
				and	bkmed = :Lmed
				and	cntr 	= :Lcntr
				and	prodstage = 'DU'
			using sqlservertrans;
			IF f_check_dberror(sqlservertrans,"PROD")=FALSE THEN
				RETURN -1
			END IF
		ELSEIF Lmed = 'BR' THEN
			// Calculate the duplication units for Braille
			Units = len * qnty
			SubUnits = Volumn * qnty
			Update prod set units = :Units,subunits = :SubUnits
				where bkseq = :Lbkseq
				and	bkmed = :Lmed
				and	cntr 	= :Lcntr
				and	prodstage in ('PR','EM')
			using sqlservertrans;
			IF f_check_dberror(sqlservertrans,"PROD")=FALSE THEN
				RETURN -1
			END IF
		ELSEIF media = 'P/B'	THEN
			// Calculate the duplication units for Print Braille
			Units = qnty
			Update prod set units = :Units
				where bkseq = :Lbkseq
				and	bkmed = :Lmed
				and	cntr 	= :Lcntr
				and	prodstage in ( 'PB','EM')
			using sqlservertrans;
			IF f_check_dberror(sqlservertrans,"PROD")=FALSE THEN
				RETURN -1
			END IF
		ELSEIF Lmed = 'FD' THEN
			// Calculate the duplication units for Formated Disk
			Vols = Ceiling(len/2)
			dw_qa_prod_review.object.mchar_vols[1] = Vols
			Units = Vols * Qnty
			Update prod set units = :Units
				where bkseq = :Lbkseq
				and	bkmed = :Lmed
				and	cntr 	= :Lcntr
				and	prodstage = 'DU'
			using sqlservertrans;
			IF f_check_dberror(sqlservertrans,"PROD")=FALSE THEN
				RETURN -1
			END IF
		END IF
	END IF	
END IF
Commit Using Sqlservertrans;
RETURN Units
end function

event open;call super::open;// Open the sheet in Maximized mode
//this.windowstate = maximized!
em_bkno.SetFocus()

end event

on w_qa_product_review_blind.create
int iCurrent
call super::create
this.cb_add_row=create cb_add_row
this.mle_comments=create mle_comments
this.cb_spell=create cb_spell
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
this.dw_qa_prod_review=create dw_qa_prod_review
this.dw_qa_qastg=create dw_qa_qastg
this.dw_qa_narr=create dw_qa_narr
this.cb_narr=create cb_narr
this.st_3=create st_3
this.dw_qa_recagcy=create dw_qa_recagcy
this.dw_qa_cntr=create dw_qa_cntr
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.cb_add_row
this.Control[iCurrent+2]=this.mle_comments
this.Control[iCurrent+3]=this.cb_spell
this.Control[iCurrent+4]=this.cb_updateweb
this.Control[iCurrent+5]=this.cb_add_narr
this.Control[iCurrent+6]=this.cb_comments
this.Control[iCurrent+7]=this.st_4
this.Control[iCurrent+8]=this.cb_find
this.Control[iCurrent+9]=this.cb_exit
this.Control[iCurrent+10]=this.cb_clear
this.Control[iCurrent+11]=this.cb_update
this.Control[iCurrent+12]=this.st_1
this.Control[iCurrent+13]=this.em_bkno
this.Control[iCurrent+14]=this.dw_qa_prod_review
this.Control[iCurrent+15]=this.dw_qa_qastg
this.Control[iCurrent+16]=this.dw_qa_narr
this.Control[iCurrent+17]=this.cb_narr
this.Control[iCurrent+18]=this.st_3
this.Control[iCurrent+19]=this.dw_qa_recagcy
this.Control[iCurrent+20]=this.dw_qa_cntr
end on

on w_qa_product_review_blind.destroy
call super::destroy
destroy(this.cb_add_row)
destroy(this.mle_comments)
destroy(this.cb_spell)
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
destroy(this.dw_qa_prod_review)
destroy(this.dw_qa_qastg)
destroy(this.dw_qa_narr)
destroy(this.cb_narr)
destroy(this.st_3)
destroy(this.dw_qa_recagcy)
destroy(this.dw_qa_cntr)
end on

event pfc_preopen;call super::pfc_preopen;string Luserid
boolean bjaws=FALSE
Luserid = SQLserverTrans.userid

this.of_SetPreference(TRUE)
this.inv_preference.of_SetToolBars(TRUE)
this.inv_preference.of_SetWindow(TRUE)
this.of_SetResize(TRUE)
this.inv_resize.of_SetOrigSize(this.workspacewidth(),this.workspaceheight())
bjaws = wf_isjawsrunning()
if bjaws = TRUE then
	of_SetJaws(TRUE)
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
	inv_resize.of_Register(cb_spell, "Scale")
	inv_resize.of_Register(cb_add_row, "Scale")
	inv_resize.of_Register(st_1, "Scale")
	inv_resize.of_Register(mle_comments, "Scale")
	//inv_resize.of_Register(st_2, "Scale")
	//inv_resize.of_Register(st_3, "Scale")
	inv_resize.of_Register(st_4, "Scale")
	inv_resize.of_Register(em_bkno, "Scale")
	IF TRIM(Luserid) <> "dsmi" AND TRIM(Luserid) <> "tmcl" THEN
		cb_narr.visible = FALSE
		cb_comments.visible = FALSE
		cb_add_narr.visible = FALSE
	ELSE
		cb_narr.visible = TRUE
		cb_comments.visible = TRUE
		cb_add_narr.visible = TRUE
	END IF
	
else
	of_SetJaws(FALSE)
	MessageBox("ERROR in window-eyes","Window-Eyes must be running.")
end if
	




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

type cb_add_row from commandbutton within w_qa_product_review_blind
string tag = "Add Row"
integer x = 1024
integer y = 1116
integer width = 338
integer height = 92
integer taborder = 70
integer textsize = -8
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "Add &Row"
end type

event clicked;int lastrow

dw_qa_qastg.event pfc_addrow()

lastrow = dw_qa_qastg.RowCount()

dw_qa_qastg.SetFocus()
dw_qa_qastg.ScrollToRow(lastrow)
dw_qa_qastg.SetColumn(1)
end event

type mle_comments from uo_mle within w_qa_product_review_blind
string tag = "Comment"
integer x = 18
integer y = 828
integer width = 2482
integer height = 252
integer taborder = 50
end type

event modified;call super::modified;dw_qa_qastg.object.qacomments[dw_qa_qastg.getrow()] = this.text
end event

type cb_spell from commandbutton within w_qa_product_review_blind
string tag = "Spell Checker"
integer x = 640
integer y = 1116
integer width = 334
integer height = 92
integer taborder = 60
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "&Spell Check"
end type

event clicked;nca_word lnca_word
String ls_S
String lcomments
int ll_row

ls_S = mle_comments.text

IF NOT(IsNULL(ls_S)) THEN
	lnca_Word.SpellCheck( ls_S )

	mle_comments.text = ls_S
	dw_qa_qastg.object.qacomments[dw_qa_qastg.getrow()] = mle_comments.text
END IF

mle_comments.SetFocus ()
	

end event

type cb_updateweb from u_cb within w_qa_product_review_blind
string tag = "Update Web records"
boolean visible = false
integer x = 87
integer y = 1108
integer width = 46
integer taborder = 0
boolean bringtotop = true
boolean enabled = false
string text = "Update &Web"
end type

event clicked;call super::clicked;integer rtn,i,qacnt,lcnt,lans,Lapplen,Lqnty,Llen,Lvols
string Lnarr,Lnarrfn,Lmed,Lqastatcd,Lcntr,Lqastg,Lbkno,Lprdr,Lprdrname,Lcntrtype,Luserid,Lsubprdr,Lqacomments
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

IF TRIM(Luserid) <> "dsmi" AND TRIM(Luserid) <> "tmcl" THEN
	MessageBox("ERROR Updating WEB","Userid "+Luserid+" is not allowed to update WEB Screens." )	
	RETURN 
ELSE
	Select prdr_name into :Lprdrname
	from producer
	where prdr = :Lprdr
	using sqlservertrans;
	IF f_check_dberror(SQLServerTrans, "Producer")=FALSE THEN
		ROLLBACK USING SQLServerOracleTrans;
		ROLLBACK USING sqlservertrans;
	END IF
		
	
	select max(subprdr) into :Lsubprdr
	from sub
	where cntr = :Lcntr
	using sqlservertrans;
	IF f_check_dberror(SQLServerTrans, "sub")=FALSE THEN
		ROLLBACK USING SQLServerOracleTrans;
		ROLLBACK USING sqlservertrans;
	END IF
	
	rtn = dw_qa_qastg.AcceptText()
	IF rtn = 1 THEN
		SetMicroHelp(w_pics_main,"Updating Records on the WEB Please Wait...")
		qacnt = dw_qa_qastg.Rowcount()

		FOR i = 1 to qacnt
			IF ( IsNull(dw_qa_qastg.object.qainit[i]) OR &
				IsNull(dw_qa_qastg.object.qastatcd[i]) ) THEN
				dw_qa_qastg.DeleteRow(i)
			END IF
		NEXT
		
		FOR i = 1 to dw_qa_qastg.Rowcount()
			Lqastg = dw_qa_qastg.object.qastg[i]
			Lqastatcd = dw_qa_qastg.object.qastatcd[i]
			Lqarecdt = dw_qa_qastg.object.qarecdt[i]
			Lqacompdt = dw_qa_qastg.object.qacompdt[i]
			Lqacomments = dw_qa_qastg.object.qacomments[i]
			
			lcnt = 0
					
			SELECT COUNT(*)
			INTO :lcnt
			FROM PRDRQASTG
			WHERE BKMED = :Lmed
			AND BKSEQ = :Lbkseq
			AND CNTR = :Lcntr
			AND QASTG = :Lqastg
			AND QASTATCD = :Lqastatcd
			AND QARECDT = :Lqarecdt
			USING SQLServerOracleTrans;
			IF f_check_dberror(SQLServerOracleTrans, "PRDRQASTG") THEN
				IF lcnt = 0 THEN
				// We must insert into PRDRQASTG
							
					INSERT INTO PRDRQASTG (bkmed, bkseq, bkno, cntr, qastg, qarecdt, qastatcd, qacompdt, subprdr, qacomments)
					VALUES (:Lmed, :Lbkseq, :Lbkno, :Lcntr, :Lqastg, :Lqarecdt, :Lqastatcd, :Lqacompdt, :Lsubprdr, :Lqacomments)
					USING SQLServerOracleTrans;
					IF f_check_dberror(SQLServerOracleTrans, " Inserting into PRDRQASTG")=FALSE THEN
						ROLLBACK USING SQLServerOracleTrans;
						ROLLBACK USING sqlservertrans;
						MessageBox("ERROR"," ERROR is in bkseq = "+string(Lbkseq)+" cntr = "+Lcntr+" qastg = "+lqastg+" qarecdt = "+string(lqarecdt)+" qastatcd = "+lqastatcd+ " count = "+string(lcnt))
						RETURN
					END IF
							
				ELSEIF lcnt = 1 THEN
				// We must update existing record
							
					UPDATE PRDRQASTG
					SET qacompdt = :Lqacompdt, subprdr = :Lsubprdr, qacomments = :Lqacomments 
					WHERE BKMED = :Lmed
					AND BKSEQ = :Lbkseq
					AND CNTR = :Lcntr
					AND QASTG = :Lqastg
					AND QASTATCD = :Lqastatcd	
					AND QARECDT = :Lqarecdt
					USING SQLServerOracleTrans;
					IF f_check_dberror(SQLServerOracleTrans, "Updating PRDRQASTG")=FALSE THEN
						ROLLBACK USING SQLServerOracleTrans;
						ROLLBACK USING sqlservertrans;
						MessageBox("ERROR"," ERROR is in bkseq = "+string(Lbkseq)+" cntr = "+Lcntr+" qastg = "+lqastg+" qarecdt = "+string(lqarecdt)+" qastatcd = "+lqastatcd)
						RETURN
					END IF
				ELSE
					// Error, there are to many rows.
					ROLLBACK USING sqlserveroracletrans;
					ROLLBACK USING sqlservertrans;
					MessageBox("Error","Update failed in prdrqastg table. There are to many rows",Information!)
					RETURN
				END IF
			ELSE
				ROLLBACK USING sqlserveroracletrans;
				ROLLBACK USING sqlservertrans;
				MessageBox("Error","Select count failed.",Information!)
				RETURN
			END IF
		NEXT
				
		SELECT COUNT(*)
		INTO :lcnt
		FROM PRDRBK
		WHERE BKMED = :Lmed
		AND BKSEQ = :Lbkseq
		USING SQLServerOracleTrans;
		IF f_check_dberror(SQLServerOracleTrans, "Selecting PRDRQASTG") THEN
			IF lcnt > 0 THEN
				// Record exist in prdrbk, update it.
				UPDATE PRDRBK
				SET 	APPLEN = :Lapplen,
						LEN = :Llen,
						MINLASTSIDE = :Lminlastside,
						VOLS = :Lvols,
						QNTY = :Lqnty
				WHERE BKMED = :Lmed
				AND BKSEQ = :Lbkseq
				USING SQLServerOracleTrans;
				IF f_check_dberror(SQLServerOracleTrans, "Updating PRDRBK")=FALSE THEN
					ROLLBACK USING SQLServerOracleTrans;
					ROLLBACK USING sqlservertrans;
					RETURN
				END IF
			END IF
		END IF	
		COMMIT USING SQLServerOracleTrans;
		COMMIT USING SQLServerTrans;
		SetMicroHelp(w_pics_main,"Database Updated.")
		cb_clear.Event clicked()
	ELSE
		dw_qa_qastg.SetFocus()
	END IF
END IF

end event

event getfocus;call super::getfocus;IF this.tag <> "" THEN
	SetMicroHelp(w_pics_main,this.tag)
ELSE
	SetMicroHelp(w_pics_main,"")
END IF

end event

type cb_add_narr from u_cb within w_qa_product_review_blind
string tag = "Add or Update books narrator"
integer x = 146
integer y = 1108
integer width = 64
integer taborder = 0
boolean enabled = false
string text = "Add/Update book~'s narrator..."
end type

event clicked;call super::clicked;long lbkseq
string lbkmed,lrecagcy

str_qa_add_narr lstr_qa_add_narr

lstr_qa_add_narr.bkseq = dw_qa_prod_review.object.prod_bkseq[1]
lstr_qa_add_narr.bkmed = dw_qa_prod_review.object.prod_bkmed[1]
lstr_qa_add_narr.recagcy = dw_qa_recagcy.object.recagcy[1]

openwithparm(w_qa_add_narr_for_book,lstr_qa_add_narr)

cb_find.TriggerEvent(Clicked!)


end event

event getfocus;call super::getfocus;IF this.tag <> "" THEN
	SetMicroHelp(w_pics_main,this.tag)
ELSE
	SetMicroHelp(w_pics_main,"")
END IF

end event

type cb_comments from u_cb within w_qa_product_review_blind
string tag = "Add QA Comments"
integer x = 293
integer y = 1112
integer width = 50
integer taborder = 0
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
			ll_rows = dw_qa_narr.rowcount()
			IF ll_rows > 0 THEN
				lnarr = dw_qa_narr.object.narr[ll_rows]
				IF IsNull(lnarr) OR lnarr="" THEN
					MessageBox("Error","You must have a valid narrator before entering any comments.")
					RETURN
				END IF
			ELSE
				MessageBox("Error","You must have a valid narrator before entering any comments.")
				RETURN
			END IF
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
		ll_rows = dw_qa_narr.rowcount()
		IF ll_rows > 0 THEN
			lnarr = dw_qa_narr.object.narr[ll_rows]
			IF IsNull(lnarr) OR lnarr="" THEN
				MessageBox("Error","You must have a valid narrator before entering any comments.")
				RETURN
			END IF
		ELSE
			MessageBox("Error","You must have a valid narrator before entering any comments.")
			RETURN
		END IF
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

type st_4 from statictext within w_qa_product_review_blind
boolean visible = false
integer x = 453
integer y = 1124
integer width = 50
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

type cb_find from u_cb within w_qa_product_review_blind
string tag = "Find a book"
integer x = 1408
integer y = 1116
integer width = 251
integer taborder = 80
string text = "F&ind"
boolean default = true
end type

event clicked;call super::clicked;long ll_rows,Lbkseq
string Lbkno,ls_cntr="",lmed,Lprodstage,Lprodstage2,Lcntrtype,Lcntrmed,Lricd,Luserid,Lttl,Lmedium
int rtn,lrtn,lcnt,Llen
boolean bk_just_recieved=FALSE
date lqarecdt

SetNull(Llen)
Lbkno = em_bkno.text
Lbkseq = long(Lbkno)

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
				Lprodstage = 'DU'
				Lprodstage2 = 'DU'
			ELSEIF Lcntrtype = 'A' THEN
				Lprodstage = 'AD'
				Lprodstage2 = 'DT'
			ELSE
				Lprodstage='MA'
				Lprodstage2='AB'
			END IF				
			// messagebox("prodstage","contract type:"+Lcntrtype+" Contract media:"+Lcntrmed+" Prodstage1"+Lprodstage+" Prodstage2"+Lprodstage2+" Contract No:"+ls_cntr+" Book No:"+string(Lbkseq))				
			// select the media information of the book from mchar table				
			ll_rows = dw_qa_prod_review.Retrieve(Lbkseq,ls_cntr,Lprodstage,Lprodstage2)
  			IF ll_rows > 0 THEN
				// retrieve the recorded agency(prdr) from narrtbl
					//dw_qa_recagcy.Retrieve()
					// Get the media of the book
					Lmed = dw_qa_prod_review.object.prod_bkmed[1]
					Lmedium = dw_qa_prod_review.object.mchar_med[1]
					Llen = dw_qa_prod_review.object.mchar_len[1]
					Lttl = dw_qa_prod_review.object.ttlinit_ttl[1]
					// validate the media of the book, if BR or P/B no volumns is required.
					wf_validate_med(Lmed)
					// Retrieve the qa information
					Luserid = SQLserverTrans.userid
					
					Select UPPER(wineyes)
					into :Lwineyes
					from picsuser
					where userid = TRIM(:Luserid)
					Using SqlserverTrans;
					IF f_check_dberror(sqlservertrans,"PICSUSER")=FALSE THEN RETURN
					// 03/09/10 #2269 include qastg 5 also. change array argument list					
					string ls_arg[]
					
					IF Lwineyes = 'Y' THEN					
						IF TRIM(Luserid) = "jshe" THEN
							ls_arg[1] = '1'
							ls_arg[2] = '4'
							ls_arg[3] = '5'
//							ll_rows = dw_qa_qastg.Retrieve(Lbkseq,ls_cntr,'1','4')
							ll_rows = dw_qa_qastg.Retrieve(Lbkseq,ls_cntr,ls_arg[])
						ELSEIF TRIM(Luserid) = "dabro" THEN
//							ll_rows = dw_qa_qastg.Retrieve(Lbkseq,ls_cntr,'2','2')
							ls_arg[1] = '2'
							ll_rows = dw_qa_qastg.Retrieve(Lbkseq,ls_cntr,ls_arg[])

						ELSEIF TRIM(Luserid) = "lbob" THEN
//							ll_rows = dw_qa_qastg.Retrieve(Lbkseq,ls_cntr,'2','2')
							ls_arg[1] = '2'
							ll_rows = dw_qa_qastg.Retrieve(Lbkseq,ls_cntr,ls_arg[])

						ELSEIF TRIM(Luserid) = "mich" THEN
//							ll_rows = dw_qa_qastg.Retrieve(Lbkseq,ls_cntr,'1','4')
							ls_arg[1] = '1'
							ls_arg[2] = '4'
							ls_arg[3] = '5'
							ll_rows = dw_qa_qastg.Retrieve(Lbkseq,ls_cntr,ls_arg[])

						ELSE
//							ll_rows = dw_qa_qastg.Retrieve(Lbkseq,ls_cntr,'1','4')
							ls_arg[1] = '1'
							ls_arg[2] = '4'
							ls_arg[3] = '5'
							ll_rows = dw_qa_qastg.Retrieve(Lbkseq,ls_cntr,ls_arg[])
						END IF
					ELSE
						MessageBox("ERROR","This user is not allowed to use this screen. Please consult with system administrator.")
						RETURN
					END IF
					IF ll_rows = 0 THEN
						comments_exist=FALSE
						// If no rows were found from qastg table
						dw_qa_qastg.InsertRow(1)
						IF Lmed="BR" THEN
							// If media is Braille
							dw_qa_prod_review.object.mchar_len_t.text = "Pages"
							dw_qa_prod_review.Object.mchar_len.ValidationMsg='You must enter the number of pages.'
							dw_qa_prod_review.object.mchar_vols_t.text = "Volumes"
							dw_qa_prod_review.Object.mchar_vols.ValidationMsg='You must enter the number of volumes.'
							dw_qa_qastg.object.qastg[1]='2'
							dw_qa_qastg.object.qarecdt[1]= date(today())
							dw_qa_qastg.object.bkseq[1]=Lbkseq
							dw_qa_qastg.object.bkmed[1]=Lmed
							dw_qa_qastg.object.cntr[1]=ls_cntr
							dw_qa_qastg.object.ttl[1]=Lttl
							dw_qa_narr.Enabled=FALSE
							dw_qa_narr.Hide()
							dw_qa_qastg.setcolumn("qarecdt")
						ELSE
							// If media is cassettes
							dw_qa_prod_review.object.mchar_len_t.text = "Tracks"
							dw_qa_prod_review.Object.mchar_len.ValidationMsg='You must enter the number of Tracks.'
							dw_qa_prod_review.object.mchar_vols_t.text = "Cassettes"
							dw_qa_prod_review.Object.mchar_vols.ValidationMsg='You must enter the number of Cassettes.'
							select cntrtype into :Lcntrtype from ancntr where cntr = :ls_cntr using sqlservertrans;
							if Lcntrtype='D' THEN
								dw_qa_qastg.object.qastg[1]='2'
							else
								dw_qa_qastg.object.qastg[1]='1'
							end if
							dw_qa_qastg.object.qarecdt[1]= date(today())
							dw_qa_qastg.object.bkseq[1]=Lbkseq
							dw_qa_qastg.object.bkmed[1]=Lmed
							dw_qa_qastg.object.cntr[1]=ls_cntr
							dw_qa_qastg.object.ttl[1]=Lttl
							dw_qa_narr.Enabled=TRUE
							dw_qa_narr.Show()
							dw_qa_qastg.setcolumn("qarecdt")
						END IF
					ELSEIF ll_rows > 0 THEN
						// If the book has just recieved for QA and status code is set to I(initial).
						comments_exist=TRUE
						IF (dw_qa_qastg.object.qastatcd[1] = "I") THEN
							dw_qa_qastg.object.qastatcd[1] = ""
							bk_just_recieved = TRUE
						END IF
						lqarecdt = date(dw_qa_qastg.object.qarecdt[1])
						// If rows, exist in qastg table
						IF Lmed="BR" OR Lmed="P/B" THEN
							// If media is Braille or Print Braille
							dw_qa_prod_review.object.mchar_len_t.text = "Pages"
							dw_qa_prod_review.object.mchar_vols_t.text = "Volumes"
							dw_qa_narr.Enabled=FALSE
							// Enable the comment button
							cb_comments.Enabled=TRUE
							dw_qa_narr.Hide()
							//dw_qa_recagcy.Hide()
							//st_3.hide()
						ELSEIF Lmed="RC" OR Lmed="FD" THEN
							// If media is cassettes
							dw_qa_prod_review.object.mchar_len_t.text = "Tracks"
							dw_qa_prod_review.object.mchar_vols_t.text = "Cassettes"
							dw_qa_narr.Enabled=TRUE
							// Enable the comment button
							cb_comments.Enabled=TRUE
							dw_qa_narr.Show()
						END IF
						// Assign qarecdt to the same at the previouse stage
						dw_qa_qastg.object.qarecdt[ll_rows] = date(lqarecdt)
						// Set the row to the last row of the qastg datawindow.
						dw_qa_qastg.ScrollToRow(ll_rows)
						dw_qa_qastg.SetRow(ll_rows)
						mle_comments.text = dw_qa_qastg.object.qacomments[ll_rows]
					END IF
					// If media is RC or FD retrieve the narrator information
					IF Lmed="RC" OR Lmed="FD" THEN
						rtn = dw_qa_narr.Retrieve(Lbkseq)
						IF rtn = 0 THEN
							dw_qa_narr.triggerevent("pfc_addrow")
						end if
					END IF
					Lricd = dw_qa_prod_review.object.mchar_ricd[1]
					IF (Lricd = 'RI' AND NOT(wf_is_qstage1_approved())) THEN
						// IF the book is for reissue and has gone through the first qastg,
						// then don't allow update to narr and studio.
						dw_qa_narr.Enabled = TRUE
						//dw_qa_recagcy.Enabled = TRUE
						// QAS wants to be able to update tracks, volumn and minlastside 
						// even after stage I is approved.
						// dw_qa_prod_review.Enabled = FALSE
					ELSEIF (Lcntrtype='D' OR wf_is_qstage1_approved()) THEN
						// If the book is for duplication only or has gone through the first qastg,
						// then don't allow any update to minlastside, tracks, narr and studio.
						dw_qa_narr.Enabled = FALSE
						//dw_qa_recagcy.Enabled = FALSE
						// dw_qa_prod_review.Enabled = FALSE
					ELSE
						// Book is not for duplication only.
						dw_qa_narr.Enabled = TRUE
						//dw_qa_recagcy.Enabled = TRUE
						dw_qa_prod_review.Enabled = TRUE
					END IF
					IF IsNull(Llen) THEN
						dw_qa_prod_review.SetFocus()
					ELSE
						dw_qa_qastg.SetFocus()
					END IF
					em_bkno.Enabled = FALSE
					cb_update.Enabled = TRUE
					cb_clear.Enabled = TRUE
					cb_find.Enabled = FALSE
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

type cb_exit from u_cb within w_qa_product_review_blind
string tag = "exist the screen"
integer x = 2267
integer y = 1116
integer width = 238
integer taborder = 110
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

type cb_clear from u_cb within w_qa_product_review_blind
string tag = "clear screen"
integer x = 1989
integer y = 1116
integer width = 247
integer taborder = 100
boolean bringtotop = true
boolean enabled = false
string text = "&Clear"
end type

event clicked;call super::clicked;string Lmed

Comments_exist = FALSE

Lmed = dw_qa_prod_review.Object.prod_bkmed[1]

dw_qa_prod_review.setFilter("")
dw_qa_prod_review.Reset()
dw_qa_prod_review.insertrow(0)

dw_qa_qastg.Reset()
dw_qa_qastg.insertrow(0)

dw_qa_cntr.Reset()
dw_qa_cntr.insertrow(0)

IF (Lmed <> "BR" OR Lmed <> "P/B") THEN
	dw_qa_narr.Reset()
	dw_qa_narr.insertrow(0)
	dw_qa_recagcy.Reset()
	dw_qa_recagcy.insertrow(0)
END IF

//wf_settab_order_noqastg()

em_bkno.text=""
local_recagcy=""
mle_comments.text=""

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

type cb_update from u_cb within w_qa_product_review_blind
string tag = "Update record"
integer x = 1691
integer y = 1116
integer width = 251
integer taborder = 90
boolean bringtotop = true
integer weight = 700
fontcharset fontcharset = ansi!
boolean enabled = false
string text = "&Update"
end type

event clicked;call super::clicked;integer rtn,i,qacnt,webrtn
string Lnarr,Lnarrfn,Lmed,Lqastatcd,Luserid,Lcomments
long Lbkseq
Lbkseq = long(em_bkno.text)
comments_exist = FALSE

qacnt = dw_qa_qastg.Rowcount()
Lqastatcd = dw_qa_qastg.object.qastatcd[qacnt]
Luserid = SQLserverTrans.userid

Lmed = TRIM(dw_qa_prod_review.object.prod_bkmed[1])
IF Lmed="BR" OR Lmed="P/B" THEN
	// If media is Braille
ELSE
	IF dw_qa_narr.RowCount() = 0 AND Lqastatcd <> "B" THEN
		MessageBox("ERROR","Narrator must be selected.",StopSign!)
		dw_qa_narr.Enabled=TRUE
		dw_qa_narr.TriggerEvent("pfc_addrow") 
		dw_qa_narr.SetFocus()
		dw_qa_narr.SetColumn(1)
		RETURN
	END IF
END IF

// Check to see if this book is on hold. (Retruned)
IF Lqastatcd <> "B" THEN
	rtn = dw_qa_prod_review.AcceptText()
	IF rtn = 1 THEN
		rtn = dw_qa_narr.AcceptText()
		IF rtn = 1 THEN
			rtn = dw_qa_qastg.AcceptText()
			IF rtn = 1 THEN
				SetMicroHelp(w_pics_main,"Updating Records Please Wait...")
				// Check for any pending updates
				IF (of_UpdateChecks( ) < 0 AND Lqastatcd <> "B" )THEN Return -1
					
					
				// IF Media is RC or FD THEN Narrator must exist.
				IF (Lmed="RC" OR Lmed="FD") AND Lqastatcd <> "B" THEN
					IF (dw_qa_narr.object.narr[1]="" OR IsNull(dw_qa_narr.object.narr[1])) THEN
						MessageBox("ERROR","Narrator must be selected.",StopSign!)
						dw_qa_narr.SetFocus()
						dw_qa_narr.SetColumn(1)
						RETURN
					END IF
				END IF

				rtn = dw_qa_prod_review.Event pfc_update(true,true)
				IF rtn = 1 THEN
					IF (Lmed <> "BR" AND Lmed <> "P/B" AND Lqastatcd <> "B" ) THEN
						rtn = dw_qa_narr.Event pfc_update(true,true)
							IF rtn = 1 THEN
								rtn  = dw_qa_qastg.Event pfc_update(true,true)
								IF rtn = 1 THEN
									COMMIT USING sqlservertrans;
									IF TRIM(Luserid) <> "dsmi" AND TRIM(Luserid) <> "tmcl" THEN
									   SetMicroHelp(w_pics_main,"Database Updated.")
									ELSE
										cb_updateweb.Event clicked()
									END IF										
									cb_clear.Event clicked()
								ELSE
									ROLLBACK USING sqlservertrans;
									MessageBox("Error","Update failed.",Information!)
								END IF
							ELSE
								ROLLBACK USING sqlservertrans;
								MessageBox("Error","Update failed.",Information!)
							END IF
					ELSE
						rtn  = dw_qa_qastg.Event pfc_update(true,true)
						IF rtn = 1 THEN
							COMMIT USING sqlservertrans;
							cb_clear.Event clicked()
						ELSE
							ROLLBACK USING sqlservertrans;
							MessageBox("Error","Update failed.",Information!)
						END IF
					END IF
					// Mark the MCHAR Table
					f_update_mchar_time("",Lbkseq,"B","U")
				ELSE
					ROLLBACK USING sqlservertrans;
					MessageBox("Error","Update failed.",Information!)
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
ELSE
	rtn = dw_qa_prod_review.AcceptText()
	IF rtn = 1 THEN
		rtn = dw_qa_qastg.AcceptText()
		IF rtn = 1 THEN
			dw_qa_narr.AcceptText()
			//dw_qa_recagcy.AcceptText()
			SetMicroHelp(w_pics_main,"Updating Records Please Wait...")
				
			FOR i = 1 to dw_qa_qastg.RowCount()
				IF ( IsNull(dw_qa_qastg.object.qainit[i]) OR &
					  IsNull(dw_qa_qastg.object.qastatcd[i]) ) THEN
						dw_qa_qastg.DeleteRow(i)
				END IF
			NEXT
			
			IF Lqastatcd <> "B" THEN
				// Check for any pending updates
				IF (of_UpdateChecks( ) < 0 ) THEN Return -1
			END IF
						
			// IF Media is RC or FD THEN Narrator must exist.
			IF (Lmed="RC" OR Lmed="FD") AND Lqastatcd <> "B" THEN
				IF (dw_qa_narr.object.narr[1]="" OR IsNull(dw_qa_narr.object.narr[1])) THEN
					MessageBox("ERROR","Narrator must be selected.",StopSign!)
					dw_qa_narr.SetFocus()
					dw_qa_narr.SetColumn(1)
					RETURN
				END IF
			END IF
						
			rtn = dw_qa_prod_review.Event pfc_update(true,true)
			IF rtn = 1 THEN
				IF (Lmed <> "BR" AND Lmed <> "P/B" ) AND Lqastatcd <> "B" THEN
					rtn = dw_qa_narr.Event pfc_update(true,true)
					IF rtn = 1 THEN
						rtn  = dw_qa_qastg.Event pfc_update(true,true)
						IF rtn = 1 THEN
							COMMIT USING sqlservertrans;
							mle_comments.text = ""
							cb_clear.Event clicked()
						ELSE
							ROLLBACK USING sqlservertrans;
							MessageBox("Error","Update failed.",Information!)
						END IF
					ELSE
						ROLLBACK USING sqlservertrans;
						MessageBox("Error","Update failed.",Information!)
					END IF
				ELSE
					rtn  = dw_qa_qastg.Event pfc_update(true,true)
					IF rtn = 1 THEN
						COMMIT USING sqlservertrans;
						cb_clear.Event clicked()
					ELSE
						ROLLBACK USING sqlservertrans;
						MessageBox("Error","Update failed.",Information!)
					END IF
				END IF
				// Mark the MCHAR Table
				f_update_mchar_time("",Lbkseq,"B","U")
			ELSE
				ROLLBACK USING sqlservertrans;
				MessageBox("Error","Update failed.",Information!)
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

type st_1 from statictext within w_qa_product_review_blind
integer x = 1719
integer y = 56
integer width = 306
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

type em_bkno from uo_conno within w_qa_product_review_blind
string tag = "Book Number"
integer x = 2048
integer y = 48
integer width = 302
integer height = 88
integer taborder = 10
long textcolor = 255
maskdatatype maskdatatype = stringmask!
string mask = "!!!!!!!"
string displaydata = ""
end type

type dw_qa_prod_review from u_pics_dw within w_qa_product_review_blind
event ue_enterkey pbm_dwnprocessenter
string tag = "Production information"
integer x = 18
integer y = 16
integer width = 1513
integer height = 460
integer taborder = 20
string dataobject = "d_qa_prod_review_blind"
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

event ue_postconstructor;call super::ue_postconstructor;dw_qa_prod_review.SetTransObject(sqlservertrans) 

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
if rowcount > 1 then
	string ls_cntr,ls_filter,ls_media
	ls_cntr  =  dw_qa_cntr.object.cntr[1]
	ls_media = upper(right(ls_cntr,2))
	ls_filter = "mchar_med = '"+ ls_media + "'"
	dw_qa_prod_review.setFilter(ls_filter)
	dw_qa_prod_review.Filter()
end if
close(w_pics_retrieve_msg_box)
end event

event sqlpreview;call super::sqlpreview;//messagebox("sql",sqlsyntax)
end event

event itemchanged;call super::itemchanged;int length,volumn,qnty,totunits,minlastside
string media,prodstage,cntrtype

IF dwo.name = "mchar_len" THEN
	IF data="" OR ISNull(data) THEN
		RETURN 1
	END IF
	length 			= Integer(data)	
	media 			= this.Object.mchar_med[row]
	prodstage 		= this.Object.prod_prodstage[row]
	cntrtype 		= this.Object.ancntr_cntrtype[row]
	qnty 				= this.Object.mchar_qnty[row]
	minlastside 	= this.object.mchar_minlastside[row]
	volumn 			= this.object.mchar_vols[row]
	totunits = wf_calculate_units(media,prodstage,cntrtype,length,qnty,minlastside,volumn)
	IF totunits = -1 THEN
		RETURN 1
	ELSE
		this.object.prod_units[row] = totunits
	END IF
	CHOOSE CASE media
	CASE "RC"
			volumn = Ceiling(length / 4)
			this.Object.mchar_vols[row] = volumn
	CASE "FD"
			volumn = Ceiling(length / 2)
			this.Object.mchar_vols[row] = volumn
	CASE "P/B"
	CASE ELSE
	END CHOOSE

ELSEIF dwo.name = "mchar_minlastside" THEN
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
	volumn 			= this.Object.mchar_vols[row]
	totunits = wf_calculate_units(media,prodstage,cntrtype,length,qnty,minlastside,volumn)
	IF totunits = -1 THEN
		RETURN 1
	ELSE
		this.object.prod_units[row] = totunits
	END IF
	//wf_settab_order_noqastg()
ELSEIF dwo.name = "mchar_vols" THEN
	//wf_settab_order_noqastg()
END IF
end event

event itemfocuschanged;call super::itemfocuschanged;//IF dwo.name = "mchar_minlastside" THEN
//	inv_jaws.of_read('minutes last side',FALSE)
//ELSEIF dwo.name = "mchar_vols" THEN
//	inv_jaws.of_read('Cassettes',FALSE)
//ELSEIF dwo.name = "mchar_len" THEN
//	inv_jaws.of_read('Length',FALSE)
//ELSEIF dwo.name = "ttlinit_auth" THEN
//	IF isvalid(inv_jaws) THEN
//		inv_jaws.of_read('Author',FALSE)
//	end if
//ELSEIF dwo.name = "ttlinit_authfn" THEN
//	IF isvalid(inv_jaws) THEN
//		inv_jaws.of_read('Author First Name',FALSE)
//	end if
//ELSEIF dwo.name = "ttlinit_ttl" THEN
//	IF isvalid(inv_jaws) THEN
//		inv_jaws.of_read('Title',FALSE)
//	END IF
//END IF
end event

type dw_qa_qastg from u_pics_dw within w_qa_product_review_blind
event ue_enterkey pbm_dwnprocessenter
event ue_tabkey pbm_dwntabout
string tag = "QA Information"
integer x = 18
integer y = 480
integer width = 2487
integer height = 340
integer taborder = 30
boolean bringtotop = true
string dataobject = "d_qa_qastg_blind2"
boolean hscrollbar = true
boolean ib_isupdateable = false
boolean ib_rmbmenu = false
boolean ib_rmbfocuschange = false
string is_updatesallowed = ""
end type

event ue_enterkey;Send(Handle(this),256,9,Long(0,0))
return(1)

end event

event ue_postconstructor;call super::ue_postconstructor;dw_qa_qastg.SetTransObject(sqlservertrans) 

end event

event pfc_addrow;long	ll_rc,Lbkseq
int cur_row
string Lqa_approve,lqastg,Lbkmed,Lcntr,Lcntrtype,Luserid,Lttl,Lqastatcd,Lmed
date lqarecdt

cur_row = dw_qa_qastg.RowCount()

Lqa_approve = dw_qa_qastg.object.qastatcd[cur_row]
Lqastg = dw_qa_qastg.object.qastg[cur_row]
Lqarecdt = date(dw_qa_qastg.object.qarecdt[cur_row])
//Lqarecdt = dw_qa_qastg.object.qarecdt[cur_row]
Lqastatcd = TRIM(dw_qa_qastg.object.qastatcd[cur_row])

Lbkseq = long(em_bkno.text)
Lbkmed = TRIM(dw_qa_prod_review.object.prod_bkmed[1])
Lmed = TRIM(dw_qa_prod_review.object.mchar_med[1])
Lcntr = dw_qa_cntr.object.cntr[1]
Lcntrtype = dw_qa_prod_review.object.ancntr_cntrtype[1]
Lttl = dw_qa_prod_review.object.ttlinit_ttl[1]
Luserid = SQLserverTrans.userid

// If book is rejected or put on hold, then allow them to add a new row.
IF Lqa_approve = 'R' OR Lqastatcd = 'B' THEN
	
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
		
	IF Lqastg = '1' AND Lwineyes = 'Y' THEN
			
	// Qastg is 1 
		dw_qa_qastg.object.bkseq[ll_rc]=Lbkseq
		dw_qa_qastg.object.bkmed[ll_rc]=Lbkmed
		dw_qa_qastg.object.cntr[ll_rc]=Lcntr
		dw_qa_qastg.object.ttl[ll_rc]=Lttl
		dw_qa_qastg.object.qastg[ll_rc]='1'
		dw_qa_qastg.object.qarecdt[ll_rc]=Lqarecdt
	
	ELSEIF Lqastg = '4' AND Lwineyes = 'Y' THEN
			
	// Qastg is 4 
		dw_qa_qastg.object.bkseq[ll_rc]=Lbkseq
		dw_qa_qastg.object.bkmed[ll_rc]=Lbkmed
		dw_qa_qastg.object.cntr[ll_rc]=Lcntr
		dw_qa_qastg.object.ttl[ll_rc]=Lttl
		dw_qa_qastg.object.qastg[ll_rc]='4'
		dw_qa_qastg.object.qarecdt[ll_rc]=Lqarecdt
	
	ELSEIF Lqastg='2' AND Lwineyes = 'Y' THEN
			
	// Qastg is 2
	
		dw_qa_qastg.object.bkseq[ll_rc]=Lbkseq
		dw_qa_qastg.object.bkmed[ll_rc]=Lbkmed
		dw_qa_qastg.object.cntr[ll_rc]=Lcntr
		dw_qa_qastg.object.ttl[ll_rc]=Lttl
		dw_qa_qastg.object.qastg[ll_rc]='2'
		dw_qa_qastg.object.qarecdt[ll_rc]=Lqarecdt

ELSEIF Lqastg='5' AND Lwineyes = 'Y' THEN
			
	// Qastg is 5 #2269
	
		dw_qa_qastg.object.bkseq[ll_rc]=Lbkseq
		dw_qa_qastg.object.bkmed[ll_rc]=Lbkmed
		dw_qa_qastg.object.cntr[ll_rc]=Lcntr
		dw_qa_qastg.object.ttl[ll_rc]=Lttl
		dw_qa_qastg.object.qastg[ll_rc]='5'
		dw_qa_qastg.object.qarecdt[ll_rc]=Lqarecdt
			
	END IF
// If book is approved and medium is RTB and last stage is 1, then allow them to add a new row.
ELSEIF Lqa_approve = 'A' AND Lmed = 'RTB' AND Lqastg = '1' THEN
	
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
		
	IF Lqastg = '1' AND Lwineyes = 'Y' THEN
			
	// Qastg is 1 
		dw_qa_qastg.object.bkseq[ll_rc]=Lbkseq
		dw_qa_qastg.object.bkmed[ll_rc]=Lbkmed
		dw_qa_qastg.object.cntr[ll_rc]=Lcntr
		dw_qa_qastg.object.ttl[ll_rc]=Lttl
		dw_qa_qastg.object.qastg[ll_rc]='4'
		dw_qa_qastg.object.qarecdt[ll_rc]=Lqarecdt
	
	END IF
ELSE
	MessageBox("ERROR","You may only add rows if the book is rejected or put on hold. Recorded Talking Books RTB can have QC1 and QC4 or QC5.")
END IF

return 1
end event

event pfc_insertrow;call super::pfc_insertrow;//
return -1
end event

event retrievestart;call super::retrievestart;openwithparm(w_pics_retrieve_msg_box,"Retrieving QA Information, Please Wait...")
end event

event retrieveend;call super::retrieveend;close(w_pics_retrieve_msg_box)
end event

event sqlpreview;call super::sqlpreview;//MessageBox("sql",sqlsyntax)

end event

event itemfocuschanged;call super::itemfocuschanged;//IF Isvalid(inv_jaws) THEN
//	IF dwo.name = "bkseq" THEN
//		inv_jaws.of_read('Book number',TRUE)
//	ELSEIF dwo.name = "qarecdt" THEN
//		inv_jaws.of_read('Recieve Date',TRUE)
//	ELSEIF dwo.name = "qacompdt" THEN
//		inv_jaws.of_read('Review Date',FALSE)
//	ELSEIF dwo.name = "qarejcd" THEN
//		inv_jaws.of_read('Reject Code',FALSE)
//	ELSEIF dwo.name = "bkmed" THEN
//		inv_jaws.of_read('Book Media',FALSE)
//	ELSEIF dwo.name = "cntr" THEN
//		inv_jaws.of_read('Contract Number',FALSE)
//	ELSEIF dwo.name = "qastg" THEN
//		inv_jaws.of_read('stage number',FALSE)
//	ELSEIF dwo.name = "qacomments" THEN
//		inv_jaws.of_read('Comments',TRUE)
//	ELSEIF dwo.name = "qastatcd" THEN
//		inv_jaws.of_read('Status A,R,B',TRUE)
//	ELSEIF dwo.name = "qainit" THEN
//		inv_jaws.of_read('Reviewer',TRUE)
//	ELSEIF dwo.name = "ttl" THEN
//		inv_jaws.of_read('Title',TRUE)
//	END IF
//END IF // 2269
end event

event itemchanged;call super::itemchanged;date null_date
SetNull(null_date)

IF dwo.name = "qacompdt" THEN
	date Lqarecdt,lqacompdt
	int ldays,Ldays_tday
	
	Lqarecdt  = date(dw_qa_qastg.object.qarecdt[row])
	Lqacompdt  = date(left(data,10))
	
	Ldays = DaysAfter(Lqarecdt,Lqacompdt)
	Ldays_tday = DaysAfter(date(today()),Lqacompdt)
	
	IF ( Ldays < 0 ) THEN
		dw_qa_qastg.Object.qacompdt.ValidationMsg='Review date must be greater than or equal to received date.'
		RETURN 1
	ELSEIF ( date(data) = null_date ) THEN
		RETURN 1
	ELSEIF ( Ldays_tday > 0 ) THEN
		dw_qa_qastg.Object.qacompdt.ValidationMsg='Review date may not be greater than today~'s date.'
		RETURN 1	
	ELSEIF ( Ldays > 365 ) THEN
		dw_qa_qastg.Object.qacompdt.ValidationMsg='Review date may not be one year later than receive date'
		RETURN 1	
	END IF
ELSEIF dwo.name = "qarecdt" THEN
	IF ( date(data) = null_date ) THEN
		RETURN 1
	ELSEIF ( date(left(data,10)) > date(Today()) ) THEN
		dw_qa_qastg.Object.qarecdt.ValidationMsg='Received date may not be greater than today~'s date.'
		RETURN 1		
	END IF
ELSEIF dwo.name = "qastatcd" THEN
	// This part was added for PR #1337
	IF data = 'A' THEN
		IF dw_qa_prod_review.object.mchar_g1br[1] <> 'Y'  AND dw_qa_prod_review.object.mchar_med[1] = 'BR' THEN
			dw_qa_prod_review.object.mchar_digitalprd[1] = 'W'
		END 	IF
	END IF
END IF

end event

event rowfocuschanged;call super::rowfocuschanged;IF comments_exist THEN
	mle_comments.text = dw_qa_qastg.object.qacomments[currentrow]
END IF
end event

type dw_qa_narr from u_pics_dw within w_qa_product_review_blind
event ue_enterkey pbm_dwnprocessenter
string tag = "Narrator"
integer x = 1531
integer y = 220
integer width = 987
integer height = 248
integer taborder = 40
boolean bringtotop = true
string dataobject = "d_qa_narr_blind"
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
dw_qa_narr.object.bkmed[ll_rc]=Lbkmed
//dw_qa_narr.object.recagcy[ll_rc]=dw_qa_recagcy.object.recagcy[1]
this.ScrollToRow(ll_rc)
this.SetColumn("narr")


return ll_rc
end event

event pfc_insertrow;// 
return -1
end event

event itemfocuschanged;call super::itemfocuschanged;IF IsValid(inv_dropdownsearch) THEN
	this.inv_dropdownsearch.Event pfc_ItemFocusChanged(row,dwo)
END IF

IF dwo.tag <> "" THEN
	SetMicroHelp(w_pics_main,dwo.tag)
ELSE
	SetMicroHelp(w_pics_main,"")
END IF


end event

event editchanged;call super::editchanged;//IF IsValid(inv_dropdownsearch) THEN
//	this.inv_dropdownsearch.Event pfc_editchanged(row,dwo,data)
//END IF
end event

event itemchanged;call super::itemchanged;string lrecagcy
IF dwo.Name = "narr" THEN
	IF wf_validate_narr_lastname(data)=FALSE THEN
		RETURN 1
	END IF	
//	IF data<>"" THEN
//		data=TRIM(data)
//		dw_qa_narr.object.narrfn[row]=TRIM(ldwc_narr.GetItemString(ldwc_narr.GetRow(),"narrfn"))
//	ELSE
//		dw_qa_narr.object.narrfn[row]=""	
//	END IF
//	lrecagcy = TRIM(ldwc_narr.GetItemString(ldwc_narr.GetRow(),"recagcy"))
//	dw_qa_recagcy.SetItem(1, "recagcy", lrecagcy )
//	messagebox("recagcy",lrecagcy)
ELSEIF dwo.name = "narrfn" THEN
	IF wf_validate_narr(dw_qa_narr.object.narr[row],data)=FALSE THEN
		RETURN 1
	END IF	
END IF
		
end event

event sqlpreview;call super::sqlpreview;//messagebox("sql",sqlsyntax)
end event

event retrievestart;call super::retrievestart;//openwithparm(w_pics_retrieve_msg_box,"Retrieving Narrator(s) Information, Please Wait...")
end event

event retrieveend;call super::retrieveend;//close(w_pics_retrieve_msg_box)
end event

type cb_narr from u_cb within w_qa_product_review_blind
string tag = "Add Narrator"
integer x = 224
integer y = 1112
integer width = 55
integer taborder = 0
boolean enabled = false
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

type st_3 from statictext within w_qa_product_review_blind
boolean visible = false
integer x = 407
integer y = 1116
integer width = 46
integer height = 76
boolean bringtotop = true
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long backcolor = 79741120
boolean enabled = false
string text = "Recorded Agency"
boolean focusrectangle = false
end type

type dw_qa_recagcy from u_pics_dw within w_qa_product_review_blind
event ue_enterkey pbm_dwnprocessenter
string tag = "Recorded Agency"
boolean visible = false
integer x = 347
integer y = 1108
integer width = 59
integer height = 104
integer taborder = 0
string dataobject = "d_qa_recagcy"
boolean vscrollbar = false
boolean livescroll = false
boolean ib_isupdateable = false
boolean ib_rmbmenu = false
boolean ib_rmbfocuschange = false
string is_updatesallowed = ""
end type

event ue_enterkey;call super::ue_enterkey;dw_qa_prod_review.SetFocus()
dw_qa_prod_review.SetColumn("mchar_len")
end event

event ue_postconstructor;call super::ue_postconstructor;dw_qa_recagcy.of_SetTransObject(sqlservertrans) 
dw_qa_cntr.of_setupdateable(FALSE)
this.of_SetDropDownSearch(TRUE)
this.inv_dropdownsearch.of_AddColumn("recagcy")


end event

event itemchanged;call super::itemchanged;local_recagcy = data

end event

event editchanged;call super::editchanged;IF IsValid(inv_dropdownsearch) THEN
	this.inv_dropdownsearch.Event pfc_editchanged(row,dwo,data)
END IF
end event

event itemfocuschanged;call super::itemfocuschanged;IF IsValid(inv_dropdownsearch) THEN
	this.inv_dropdownsearch.Event pfc_ItemFocusChanged(row,dwo)
END IF

IF dwo.tag <> "" THEN
	SetMicroHelp(w_pics_main,dwo.tag)
ELSE
	SetMicroHelp(w_pics_main,"")
END IF


end event

event sqlpreview;call super::sqlpreview;//MessageBox("sql",sqlsyntax)

end event

type dw_qa_cntr from u_pics_dw within w_qa_product_review_blind
string tag = "Contract number"
boolean visible = false
integer x = 27
integer y = 1108
integer width = 50
integer height = 96
integer taborder = 0
string dataobject = "d_qa_cntr"
boolean vscrollbar = false
boolean livescroll = false
boolean ib_isupdateable = false
boolean ib_rmbmenu = false
boolean ib_rmbfocuschange = false
string is_updatesallowed = ""
end type

event itemchanged;call super::itemchanged;int rtn
string ls_cntr,Lmed,Lprodstage,Lprodstage2,Lcntrtype
long Lbkseq

ls_cntr	= data
lbkseq = long(em_bkno.text)
select prodstage into :Lprodstage from prod where cntr=:ls_cntr and bkseq=:Lbkseq using sqlservertrans;
Lprodstage2 = Lprodstage
dw_qa_prod_review.Reset()
dw_qa_prod_review.InsertRow(0)
dw_qa_qastg.Reset()
dw_qa_qastg.InsertRow(0)
dw_qa_narr.Reset()
dw_qa_narr.InsertRow(0)

Open(w_pics_retrieve_msg_box)
rtn = dw_qa_prod_review.Retrieve(Lbkseq,ls_cntr,Lprodstage,Lprodstage2)
Close(w_pics_retrieve_msg_box)

IF rtn > 0 THEN
	dw_qa_recagcy.Retrieve()
	Lmed = dw_qa_prod_review.object.prod_bkmed[1]
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
			dw_qa_qastg.object.qastg[1]='2'
			dw_qa_qastg.object.qarecdt[1]= today()
			dw_qa_qastg.object.bkseq[1]=Lbkseq
			dw_qa_qastg.object.bkmed[1]=Lmed
			dw_qa_qastg.object.cntr[1]=ls_cntr
			dw_qa_narr.Enabled=FALSE
			dw_qa_narr.Hide()
			dw_qa_recagcy.Hide()
			st_3.hide()
			dw_qa_qastg.setcolumn("qarecdt")
		ELSE
			dw_qa_prod_review.object.mchar_len_t.text = "Tracks"
			dw_qa_prod_review.Object.mchar_len.ValidationMsg='You must enter the number of Tracks.'
			dw_qa_prod_review.object.mchar_vols_t.text = "Cassettes"
			dw_qa_prod_review.Object.mchar_vols.ValidationMsg='You must enter the number of Cassettes.'
			dw_qa_prod_review.object.mchar_vols.TabSequence = 0
			dw_qa_prod_review.Object.mchar_minlastside.Edit.Required='Yes'
			dw_qa_prod_review.object.mchar_minlastside.TabSequence = 20
			dw_qa_prod_review.object.mchar_minlastside.Background.Color=RGB(255,255,255)
			select cntrtype into :Lcntrtype from ancntr where cntr = :ls_cntr using sqlservertrans;
			if Lcntrtype='D' THEN
				dw_qa_qastg.object.qastg[1]='2'
			else
				dw_qa_qastg.object.qastg[1]='1'
			end if
			dw_qa_qastg.object.qarecdt[1]= today()
			dw_qa_qastg.object.bkseq[1]=Lbkseq
			dw_qa_qastg.object.bkmed[1]=Lmed
			dw_qa_qastg.object.cntr[1]=ls_cntr
			dw_qa_narr.Enabled=TRUE
			dw_qa_narr.Show()
			dw_qa_recagcy.Show()
			st_3.Show()
			dw_qa_qastg.setcolumn("qarecdt")
		END IF
		dw_qa_prod_review.SetFocus()
	ELSEIF rtn > 0 THEN
		IF Lmed="BR" OR Lmed="P/B" THEN
			dw_qa_prod_review.object.mchar_len_t.text = "Pages"
			dw_qa_prod_review.object.mchar_vols_t.text = "Volumes"
			dw_qa_prod_review.object.mchar_vols.TabSequence = 20
			dw_qa_prod_review.object.mchar_minlastside.TabSequence = 0
			dw_qa_prod_review.object.mchar_minlastside.Background.Color=RGB(192,192,192)
			dw_qa_narr.Enabled=FALSE
			dw_qa_narr.Hide()
			dw_qa_recagcy.Hide()
			st_3.hide()
		ELSEIF Lmed="RC" OR Lmed="FD" THEN
			dw_qa_prod_review.object.mchar_len_t.text = "Tracks"
			dw_qa_prod_review.object.mchar_vols_t.text = "Cassettes"
			dw_qa_prod_review.object.mchar_vols.TabSequence = 0
			dw_qa_prod_review.object.mchar_minlastside.TabSequence = 20
			dw_qa_prod_review.object.mchar_minlastside.Background.Color=RGB(255,255,255)
			dw_qa_narr.Enabled=TRUE
			dw_qa_narr.Show()
			dw_qa_recagcy.Show()
			st_3.Show()
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
		CASE ELSE
			IF (dw_qa_qastg.object.qastg[rtn]<>'3' OR &
				dw_qa_qastg.object.qastatcd[rtn]<>'A') THEN
				dw_qa_qastg.event pfc_addrow()
			END IF
		END CHOOSE
	END IF
	IF Lmed="RC" OR Lmed="FD" THEN
		rtn = dw_qa_narr.Retrieve(Lbkseq)
		IF rtn > 0 THEN
			dw_qa_recagcy.object.recagcy[1] = dw_qa_narr.object.recagcy[1]
		ELSE
			dw_qa_recagcy.Reset()
			dw_qa_recagcy.insertrow(0)
		END IF
	END IF
	IF Lcntrtype='D' THEN
		// If the book is for duplication only, then don't allow any update to
		// minlastside, tracks, narr and studio.
		dw_qa_narr.Enabled = FALSE
		//dw_qa_recagcy.Enabled = FALSE
		dw_qa_prod_review.Enabled = FALSE
	ELSE
		// Book is not for duplication only.
		dw_qa_narr.Enabled = TRUE
		//dw_qa_recagcy.Enabled = TRUE
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

event itemfocuschanged;call super::itemfocuschanged;//IF isvalid(inv_jaws) THEN
//	inv_jaws.of_read('contract number',FALSE)
//END IF
end event


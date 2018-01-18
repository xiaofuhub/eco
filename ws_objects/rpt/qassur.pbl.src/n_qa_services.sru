$PBExportHeader$n_qa_services.sru
forward
global type n_qa_services from pfc_n_base
end type
end forward

shared variables

end variables

global type n_qa_services from pfc_n_base autoinstantiate
end type

forward prototypes
public function boolean of_ischeckedin (long al_bkseq, string as_bkmed)
public function string of_qaauthority (string as_userid)
end prototypes

public function boolean of_ischeckedin (long al_bkseq, string as_bkmed);
/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//
//	Function : of_ischeckedin
// Args : book number, book media
//	Description: For a given book and book media check to see if the book is checked in ( uploaded)
//					by the producer. If uploaded can't receive the book in the qa received screen
/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//	
//	Revision History
//
//	Developed by 	Date 				Version						Tracking#
//									
// Murali K.			10/15/2008      Phase-2 ESTPT changes	Called from other qa windows
/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//
int li_count

SELECT COUNT(*)
INTO :li_count
FROM PRODUCER_CHECKIN
WHERE BKSEQ = :al_bkseq 
 using sqlserveroracletrans ; //AND BKMED = :as_bkmed

IF f_check_dberror(SqlServerTrans,"n_qa_services->Producer Checked In function check")=FALSE THEN
	RETURN FALSE
END IF
			
IF li_count > 0 THEN
  	RETURN TRUE // CHECKED IN
ELSE
	RETURN FALSE
END IF

RETURN TRUE
end function

public function string of_qaauthority (string as_userid);
/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//
//	Function : of_qaauthority
// Args :user id
//	Description: For a given user id find the qa authority code
/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//	
//	Revision History
//
//	Developed by 	Date 				Version						Tracking#
//									
// Murali K.			10/15/2008      Phase-2 ESTPT changes	Called from other qa windows
/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//
string ls_code

SELECT QA_AUTHORITY_CODE
INTO :ls_code
FROM PICSUSER
WHERE USERID = :as_userid
 using sqlservertrans ; 

IF f_check_dberror(SqlServerTrans,"n_qa_services->QA authority code check failed")=FALSE THEN
	RETURN ''
END IF
			
IF Len(trim(ls_code)) > 0 THEN
  	RETURN ls_code
ELSE
	RETURN ''
END IF

RETURN ls_code
end function

on n_qa_services.create
call super::create
end on

on n_qa_services.destroy
call super::destroy
end on


$PBExportHeader$w_sheet_final_review.srw
forward
global type w_sheet_final_review from w_sheet
end type
type cb_1 from commandbutton within w_sheet_final_review
end type
type cb_bknav from commandbutton within w_sheet_final_review
end type
type cb_spell from commandbutton within w_sheet_final_review
end type
type dw_annotrpt_ec_report from u_dw within w_sheet_final_review
end type
type dw_cdssubject from u_pics_dw within w_sheet_final_review
end type
type cb_zoom from commandbutton within w_sheet_final_review
end type
type cb_anno from commandbutton within w_sheet_final_review
end type
type mle_sga from multilineedit within w_sheet_final_review
end type
type dw_final_review from u_pics_dw within w_sheet_final_review
end type
type dw_coauthors from u_pics_dw within w_sheet_final_review
end type
type dw_copyright from u_dw within w_sheet_final_review
end type
type mle_anno from u_mle within w_sheet_final_review
end type
type st_2 from u_st within w_sheet_final_review
end type
type st_1 from u_st within w_sheet_final_review
end type
type sle_rowcount from u_sle within w_sheet_final_review
end type
type dw_ri from u_dw within w_sheet_final_review
end type
type st_currentrow from u_st within w_sheet_final_review
end type
type sle_currentrow from u_sle within w_sheet_final_review
end type
type cb_find from u_cb within w_sheet_final_review
end type
type cb_update from u_cb within w_sheet_final_review
end type
type cb_clear from u_cb within w_sheet_final_review
end type
type cb_exit from u_cb within w_sheet_final_review
end type
type dw_finalreview_report from u_dw within w_sheet_final_review
end type
type st_3 from statictext within w_sheet_final_review
end type
type sle_totcount from u_sle within w_sheet_final_review
end type
type st_4 from statictext within w_sheet_final_review
end type
type cbx_1 from checkbox within w_sheet_final_review
end type
type st_5 from statictext within w_sheet_final_review
end type
type cbx_2 from checkbox within w_sheet_final_review
end type
type st_6 from statictext within w_sheet_final_review
end type
type cbx_3 from checkbox within w_sheet_final_review
end type
type dw_final_approval from u_dw within w_sheet_final_review
end type
end forward

shared variables

end variables
global type w_sheet_final_review from w_sheet
integer x = 5
integer y = 4
integer width = 2866
integer height = 1932
string title = "Final Review"
boolean resizable = false
cb_1 cb_1
cb_bknav cb_bknav
cb_spell cb_spell
dw_annotrpt_ec_report dw_annotrpt_ec_report
dw_cdssubject dw_cdssubject
cb_zoom cb_zoom
cb_anno cb_anno
mle_sga mle_sga
dw_final_review dw_final_review
dw_coauthors dw_coauthors
dw_copyright dw_copyright
mle_anno mle_anno
st_2 st_2
st_1 st_1
sle_rowcount sle_rowcount
dw_ri dw_ri
st_currentrow st_currentrow
sle_currentrow sle_currentrow
cb_find cb_find
cb_update cb_update
cb_clear cb_clear
cb_exit cb_exit
dw_finalreview_report dw_finalreview_report
st_3 st_3
sle_totcount sle_totcount
st_4 st_4
cbx_1 cbx_1
st_5 st_5
cbx_2 cbx_2
st_6 st_6
cbx_3 cbx_3
dw_final_approval dw_final_approval
end type
global w_sheet_final_review w_sheet_final_review

type variables
string Lsexviol,  Lchno,  Lconno, is_sql, what_text,is_comm_audio
int anno_stage
boolean anno_changed=FALSE,ri_exist=FALSE,anno_exist=FALSE,ib_firsttime=FALSE
boolean ib_found,ib_anno_modified,anno_tags_altered, ib_preupdatefail=FALSE
boolean ib_save
int ii_mle
long il_currow
DataWindowChild ldwc_cdssub,ldwc_cdssubcode
nvo_val_init_con lnvo_val_anno
end variables
forward prototypes
public function string wf_build_sex_sentence (string lc_sex, string lc_viol, string lc_slang)
public function boolean wf_check_anno_flag (string ls_chno)
public function string wf_which_flang (string flang_code)
public function string wf_build_book_sentence (string ls_casub, string ls_prize, string ls_anyr, string ls_bseller_flag, string ls_med, string ls_g1br, string ls_conno, string ls_flang)
public subroutine wf_disable_all ()
public subroutine wf_disable_buttons ()
public subroutine wf_enable_buttons ()
public function string wf_sex_viol_lang ()
public subroutine wf_validate_ricd ()
public function integer wf_retrieve ()
public subroutine wf_get_rest_of_data ()
public subroutine wf_final_approval (date as_date)
public function integer wf_validate_conno (string control_no)
public function integer wf_clear_label_fields (boolean ab_visible)
public function integer wf_get_label_data (string as_conno)
public function integer wf_validate_cdssubject ()
end prototypes

public function string wf_build_sex_sentence (string lc_sex, string lc_viol, string lc_slang);//Building the sex and violance sentance
integer li_sexord,li_violord,li_slangord
string ls_sex,ls_viol,ls_slang,ls_text="",ls_sex_cap,ls_viol_cap,ls_slang_cap

SELECT anno_properties.sex_ord,   
       anno_properties.viol_ord,   
       anno_properties.slang_ord  
INTO :li_sexord,   
     :li_violord,   
     :li_slangord  
FROM anno_properties  USING SQLServerTrans;

IF lc_sex='S' THEN
	ls_sex_cap="Some descriptions of sex"
	ls_sex="some descriptions of sex"
ELSEIF lc_sex='E' THEN
	ls_sex_cap="Explicit descriptions of sex"
	ls_sex="explicit descriptions of sex"
ELSEIF lc_sex='B' THEN
	ls_sex_cap="Some explicit descriptions of sex"
	ls_sex="some explicit descriptions of sex"
ELSEIF lc_sex='Y' THEN
	ls_sex_cap="Descriptions of sex"
	ls_sex="descriptions of sex"
ELSEIF lc_sex='N' THEN
	SetNull(ls_sex)
END IF

IF lc_viol='S' THEN
	ls_viol_cap="Some violence"
	ls_viol="some violence"
ELSEIF lc_viol='Y' THEN
	ls_viol_cap="Violence"
	ls_viol="violence"
ELSEIF lc_viol='N' THEN
	SetNull(ls_viol)
END IF

IF lc_slang='S' THEN
	ls_slang_cap="Some strong language"
	ls_slang="some strong language"
ELSEIF lc_slang='Y' THEN
	ls_slang_cap="Strong language"
	ls_slang="strong language"
ELSEIF lc_slang='N' THEN
	SetNull(ls_slang)
END IF
	
// If order is sex, violence, and strong language
IF li_sexord=1 AND li_violord=2 AND li_slangord=3 THEN
	// If all sex, violence and strong language are not NULL
	IF NOT(IsNull(ls_sex)) AND NOT(IsNull(ls_viol)) AND NOT(IsNull(ls_slang)) THEN
		// If some sex , violence and strong language
		IF mid(lower(ls_sex),1,4)='some' AND mid(lower(ls_viol),1,4)<>'some' AND mid(lower(ls_slang),1,4)<>'some' THEN
			ls_text = ls_viol_cap+", "+ls_slang+", and "+ls_sex+". "
		// If sex, some violence and strong language
		ELSEIF mid(lower(ls_sex),1,4)<>'some' AND mid(lower(ls_viol),1,4)='some' AND mid(lower(ls_slang),1,4)<>'some' THEN
			ls_text = ls_sex_cap+", "+ls_slang+", and "+ls_viol+". "
		// If sex, violence and some strong language
		ELSEIF mid(lower(ls_sex),1,4)<>'some' AND mid(lower(ls_viol),1,4)<>'some' AND mid(lower(ls_slang),1,4)='some' THEN
			ls_text = ls_sex_cap+", "+ls_viol+", and "+ls_slang+". "
		// If some sex, some violence and strong language
		ELSEIF mid(lower(ls_sex),1,4)='some' AND mid(lower(ls_viol),1,4)='some' AND mid(lower(ls_slang),1,4)<>'some' THEN
			ls_text = ls_slang_cap+", "+ls_sex+", and "+ls_viol+". "
		// If some sex, violence and some strong language
		ELSEIF mid(lower(ls_sex),1,4)='some' AND mid(lower(ls_viol),1,4)<>'some' AND mid(lower(ls_slang),1,4)='some' THEN
			ls_text = ls_viol_cap+", "+ls_sex+", and "+ls_slang+". "
		// If sex, some violence and some strong language
		ELSEIF mid(lower(ls_sex),1,4)<>'some' AND mid(lower(ls_viol),1,4)='some' AND mid(lower(ls_slang),1,4)='some' THEN
			ls_text = ls_sex_cap+", "+ls_viol+", and "+ls_slang+". "
		// If some sex, some violence and some strong language
		ELSE
			ls_text = ls_sex_cap+", "+ls_viol+", and "+ls_slang+". "
		END IF
	// Else if sex is NULL
	ELSEIF (NOT(IsNull(ls_viol)) AND NOT(IsNull(ls_slang))) THEN
		// If some violence and strong language
		IF mid(lower(ls_viol),1,4)='some' AND mid(lower(ls_slang),1,4)<>'some' THEN
			ls_text = ls_slang_cap+" and "+ls_viol+". "
		// If violence and some strong language
		ELSE
			ls_text = ls_viol_cap+" and "+ls_slang+". "
		END IF
	// Else if violence is NULL
	ELSEIF (NOT(IsNull(ls_sex)) AND NOT(IsNull(ls_slang))) THEN
		// If some sex and strong language
		IF mid(lower(ls_sex),1,4)='some' AND mid(lower(ls_slang),1,4)<>'some' THEN
			ls_text = ls_slang_cap+" and "+ls_sex+". "
		// If sex and some strong language
		ELSE
			ls_text = ls_sex_cap+" and "+ls_slang+". "
		END IF
	// Else if strong language is NULL
	ELSEIF NOT(IsNull(ls_sex)) AND NOT(IsNull(ls_viol)) THEN
		// If some sex and violence
		IF mid(lower(ls_sex),1,4)='some' AND mid(lower(ls_viol),1,4)<>'some' THEN
			ls_text = ls_viol_cap+" and "+ls_sex+". "
		// If sex and some violence
		ELSE
			ls_text = ls_sex_cap+" and "+ls_viol+". "
		END IF
	// Else if violence is the only text
	ELSEIF NOT(IsNull(ls_viol)) THEN
		ls_text = ls_viol_cap+". "
	// Else if sex is the only text
	ELSEIF NOT(IsNull(ls_sex)) THEN
		ls_text = ls_sex_cap+". "
	// Else if strong language is the only text
	ELSEIF NOT(IsNull(ls_slang)) THEN
		ls_text = ls_slang_cap+". "
	// Else no tags
	ELSEIF IsNull(ls_sex) AND IsNull(ls_viol) AND IsNull(ls_slang) THEN
		ls_text = " "
	END IF		
// Else if order is sex, strong language and violence
ELSEIF li_sexord=1 AND li_violord=3 AND li_slangord=2 THEN
	// If all sex, violence and strong language are not NULL
	IF NOT(IsNull(ls_sex)) AND NOT(IsNull(ls_viol)) AND NOT(IsNull(ls_slang)) THEN
		// If some sex , violence and strong language
		IF mid(lower(ls_sex),1,4)='some' AND mid(lower(ls_viol),1,4)<>'some' AND mid(lower(ls_slang),1,4)<>'some' THEN
			ls_text = ls_slang_cap+", "+ls_viol+", and "+ls_sex+". "
		// If sex , some violence and strong language
		ELSEIF mid(lower(ls_sex),1,4)<>'some' AND mid(lower(ls_viol),1,4)='some' AND mid(lower(ls_slang),1,4)<>'some' THEN
			ls_text = ls_sex_cap+", "+ls_slang+", and "+ls_viol+". "
		// If sex , violence and some strong language
		ELSEIF mid(lower(ls_sex),1,4)<>'some' AND mid(lower(ls_viol),1,4)<>'some' AND mid(lower(ls_slang),1,4)='some' THEN
			ls_text = ls_sex_cap+", "+ls_viol+", and "+ls_slang+". "
		// If some sex , some violence and strong language
		ELSEIF mid(lower(ls_sex),1,4)='some' AND mid(lower(ls_viol),1,4)='some' AND mid(lower(ls_slang),1,4)<>'some' THEN
			ls_text = ls_slang_cap+", "+ls_sex+", and "+ls_viol+". "
		// If some sex , violence and some strong language
		ELSEIF mid(lower(ls_sex),1,4)='some' AND mid(lower(ls_viol),1,4)<>'some' AND mid(lower(ls_slang),1,4)='some' THEN
			ls_text = ls_viol_cap+", "+ls_sex+", and "+ls_slang+". "
		// If sex , some violence and some strong language
		ELSEIF mid(lower(ls_sex),1,4)<>'some' AND mid(lower(ls_viol),1,4)='some' AND mid(lower(ls_slang),1,4)='some' THEN
			ls_text = ls_sex_cap+", "+ls_viol+", and "+ls_slang+". "
		// If some sex , some violence and some strong language
		ELSE
			ls_text = ls_sex_cap+", "+ls_slang+", and "+ls_viol+". "
		END IF
	// Else if sex is NULL
	ELSEIF NOT(IsNull(ls_viol)) AND NOT(IsNull(ls_slang)) THEN
		// If some violence and strong language
		IF mid(lower(ls_viol),1,4)='some' AND mid(lower(ls_slang),1,4)<>'some' THEN
			ls_text = ls_slang_cap+" and "+ls_viol+". "
		// If violence and some strong language
		ELSE
			ls_text = ls_viol_cap+" and "+ls_slang+". "
		END IF
	// Else if violence is NULL
	ELSEIF NOT(IsNull(ls_sex)) AND NOT(IsNull(ls_slang)) THEN
		// If some sex and strong language
		IF mid(lower(ls_sex),1,4)='some' AND mid(lower(ls_slang),1,4)<>'some' THEN
			ls_text = ls_slang_cap+" and "+ls_sex+". "
		// If sex and some strong language
		ELSE
			ls_text = ls_sex_cap+" and "+ls_slang+". "
		END IF
	// Else if strong language is NULL
	ELSEIF NOT(IsNull(ls_sex)) AND NOT(IsNull(ls_viol)) THEN
		// If some violence and sex
		IF mid(lower(ls_viol),1,4)='some' AND mid(lower(ls_sex),1,4)<>'some' THEN
			ls_text = ls_sex_cap+" and "+ls_viol+". "
		// If violence and some sex
		ELSE
			ls_text = ls_viol_cap+" and "+ls_sex+". "
		END IF
	// Else if violence is the only text
	ELSEIF NOT(IsNull(ls_viol)) THEN
		ls_text = ls_viol_cap+". "
	// Else if sex is the only text
	ELSEIF NOT(IsNull(ls_sex)) THEN
		ls_text = ls_sex_cap+". "
	// Else if strong language is the only text
	ELSEIF NOT(IsNull(ls_slang)) THEN
		ls_text = ls_slang_cap+". "
	// Else no tags
	ELSEIF IsNull(ls_sex) AND IsNull(ls_viol) AND IsNull(ls_slang) THEN
		ls_text = " "
	END IF		
// Else if order is violence, sex, and strong language
ELSEIF li_sexord=2 AND li_violord=1 AND li_slangord=3 THEN
	// If all sex, violence and strong language are not NULL
	IF NOT(IsNull(ls_sex)) AND NOT(IsNull(ls_viol)) AND NOT(IsNull(ls_slang)) THEN
		// If some sex , violence and strong language
		IF mid(lower(ls_sex),1,4)='some' AND mid(lower(ls_viol),1,4)<>'some' AND mid(lower(ls_slang),1,4)<>'some' THEN
			ls_text = ls_viol_cap+", "+ls_slang+", and "+ls_sex+". "
		// If sex , some violence and strong language
		ELSEIF mid(lower(ls_sex),1,4)<>'some' AND mid(lower(ls_viol),1,4)='some' AND mid(lower(ls_slang),1,4)<>'some' THEN
			ls_text = ls_sex_cap+", "+ls_slang+", and "+ls_viol+". "
		// If sex , violence and some strong language
		ELSEIF mid(lower(ls_sex),1,4)<>'some' AND mid(lower(ls_viol),1,4)<>'some' AND mid(lower(ls_slang),1,4)='some' THEN
			ls_text = ls_sex_cap+", "+ls_viol+", and "+ls_slang+". "
		// If some sex , some violence and strong language
		ELSEIF mid(lower(ls_sex),1,4)='some' AND mid(lower(ls_viol),1,4)='some' AND mid(lower(ls_slang),1,4)<>'some' THEN
			ls_text = ls_slang_cap+", "+ls_sex+", and "+ls_viol+". "
		// If some sex , violence and some strong language
		ELSEIF mid(lower(ls_sex),1,4)='some' AND mid(lower(ls_viol),1,4)<>'some' AND mid(lower(ls_slang),1,4)='some' THEN
			ls_text = ls_viol_cap+", "+ls_sex+", and "+ls_slang+". "
		// If sex , some violence and some strong language
		ELSEIF mid(lower(ls_sex),1,4)<>'some' AND mid(lower(ls_viol),1,4)='some' AND mid(lower(ls_slang),1,4)='some' THEN
			ls_text = ls_sex_cap+", "+ls_viol+", and "+ls_slang+". "
		// If some sex , some violence and some strong language
		ELSE
			ls_text = ls_viol_cap+", "+ls_sex+", and "+ls_slang+". "
		END IF
	ELSEIF NOT(IsNull(ls_viol)) AND NOT(IsNull(ls_slang)) THEN
		IF mid(lower(ls_viol),1,4)='some' AND mid(lower(ls_slang),1,4)<>'some' THEN
			ls_text = ls_slang_cap+" and "+ls_viol+". "
		ELSE
			ls_text = ls_viol_cap+" and "+ls_slang+". "
		END IF
	ELSEIF NOT(IsNull(ls_sex)) AND NOT(IsNull(ls_slang)) THEN
		IF mid(lower(ls_slang),1,4)='some' AND mid(lower(ls_sex),1,4)<>'some' THEN
			ls_text = ls_sex_cap+" and "+ls_slang+". "
		ELSE
			ls_text = ls_slang_cap+" and "+ls_sex+". "
		END IF
	ELSEIF NOT(IsNull(ls_sex)) AND NOT(IsNull(ls_viol)) THEN
		IF mid(lower(ls_viol),1,4)='some' AND mid(lower(ls_sex),1,4)<>'some' THEN
			ls_text = ls_sex_cap+" and "+ls_viol+". "
		ELSE
			ls_text = ls_viol_cap+" and "+ls_sex+". "
		END IF
	ELSEIF NOT(IsNull(ls_viol)) THEN
		ls_text = ls_viol_cap+". "
	ELSEIF NOT(IsNull(ls_sex)) THEN
		ls_text = ls_sex_cap+". "
	ELSEIF NOT(IsNull(ls_slang)) THEN
		ls_text = ls_slang_cap+". "
	ELSEIF IsNull(ls_sex) AND IsNull(ls_viol) AND IsNull(ls_slang) THEN
		ls_text = " "
	END IF		
// Else if order is strong language, sex  and violence
ELSEIF li_sexord=2 AND li_violord=3 AND li_slangord=1 THEN
	// If all sex, violence and strong language are not NULL
	IF NOT(IsNull(ls_sex)) AND NOT(IsNull(ls_viol)) AND NOT(IsNull(ls_slang)) THEN
		IF mid(lower(ls_sex),1,4)='some' AND mid(lower(ls_viol),1,4)<>'some' AND mid(lower(ls_slang),1,4)<>'some' THEN
			ls_text = ls_slang_cap+", "+ls_viol+", and "+ls_sex+". "
		ELSEIF mid(lower(ls_sex),1,4)<>'some' AND mid(lower(ls_viol),1,4)='some' AND mid(lower(ls_slang),1,4)<>'some' THEN
			ls_text = ls_slang_cap+", "+ls_sex+", and "+ls_viol+". "
		ELSEIF mid(lower(ls_sex),1,4)<>'some' AND mid(lower(ls_viol),1,4)<>'some' AND mid(lower(ls_slang),1,4)='some' THEN
			ls_text = ls_sex_cap+", "+ls_viol+", and "+ls_slang+". "
		ELSEIF mid(lower(ls_sex),1,4)='some' AND mid(lower(ls_viol),1,4)='some' AND mid(lower(ls_slang),1,4)<>'some' THEN
			ls_text = ls_slang_cap+", "+ls_sex+", and "+ls_viol+". "
		ELSEIF mid(lower(ls_sex),1,4)='some' AND mid(lower(ls_viol),1,4)<>'some' AND mid(lower(ls_slang),1,4)='some' THEN
			ls_text = ls_viol_cap+", "+ls_slang+", and "+ls_sex+". "
		ELSEIF mid(lower(ls_sex),1,4)<>'some' AND mid(lower(ls_viol),1,4)='some' AND mid(lower(ls_slang),1,4)='some' THEN
			ls_text = ls_sex_cap+", "+ls_viol+", and "+ls_slang+". "
		ELSE
			ls_text = ls_slang_cap+", "+ls_sex+", and "+ls_viol+". "
		END IF
	ELSEIF NOT(IsNull(ls_viol)) AND NOT(IsNull(ls_slang)) THEN
		IF mid(lower(ls_viol),1,4)='some' AND mid(lower(ls_slang),1,4)<>'some' THEN
			ls_text = ls_slang_cap+" and "+ls_viol+". "
		ELSE
			ls_text = ls_viol_cap+" and "+ls_slang+". "
		END IF
	ELSEIF NOT(IsNull(ls_sex)) AND NOT(IsNull(ls_slang)) THEN
		IF mid(lower(ls_slang),1,4)='some' AND mid(lower(ls_sex),1,4)<>'some' THEN
			ls_text = ls_sex_cap+" and "+ls_slang+". "
		ELSE
			ls_text = ls_sex_cap+" and "+ls_sex+". "
		END IF
	ELSEIF NOT(IsNull(ls_sex)) AND NOT(IsNull(ls_viol)) THEN
		IF mid(lower(ls_viol),1,4)='some' AND mid(lower(ls_sex),1,4)<>'some' THEN
			ls_text = ls_sex_cap+" and "+ls_viol+". "
		ELSE
			ls_text = ls_viol_cap+" and "+ls_sex+". "
		END IF
	ELSEIF NOT(IsNull(ls_viol)) THEN
		ls_text = ls_viol_cap+". "
	ELSEIF NOT(IsNull(ls_sex)) THEN
		ls_text = ls_sex_cap+". "
	ELSEIF NOT(IsNull(ls_slang)) THEN
		ls_text = ls_slang_cap+". "
	ELSEIF IsNull(ls_sex) AND IsNull(ls_viol) AND IsNull(ls_slang) THEN
		ls_text = " "
	END IF		
// Else if order is strong language, violence and sex
ELSEIF li_sexord=3 AND li_violord=2 AND li_slangord=1 THEN
	// If all sex, violence and strong language are not NULL
	IF NOT(IsNull(ls_sex)) AND NOT(IsNull(ls_viol)) AND NOT(IsNull(ls_slang)) THEN
		IF mid(lower(ls_sex),1,4)='some' AND mid(lower(ls_viol),1,4)<>'some' AND mid(lower(ls_slang),1,4)<>'some' THEN
			ls_text = ls_slang_cap+", "+ls_viol+", and "+ls_sex+". "
		ELSEIF mid(lower(ls_sex),1,4)<>'some' AND mid(lower(ls_viol),1,4)='some' AND mid(lower(ls_slang),1,4)<>'some' THEN
			ls_text = ls_slang_cap+", "+ls_sex+", and "+ls_viol+". "
		ELSEIF mid(lower(ls_sex),1,4)<>'some' AND mid(lower(ls_viol),1,4)<>'some' AND mid(lower(ls_slang),1,4)='some' THEN
			ls_text = ls_viol_cap+", "+ls_sex+", and "+ls_slang+". "
		ELSEIF mid(lower(ls_sex),1,4)='some' AND mid(lower(ls_viol),1,4)='some' AND mid(lower(ls_slang),1,4)<>'some' THEN
			ls_text = ls_slang_cap+", "+ls_viol+", and "+ls_sex+". "
		ELSEIF mid(lower(ls_sex),1,4)='some' AND mid(lower(ls_viol),1,4)<>'some' AND mid(lower(ls_slang),1,4)='some' THEN
			ls_text = ls_viol_cap+", "+ls_slang+", and "+ls_sex+". "
		ELSEIF mid(lower(ls_sex),1,4)<>'some' AND mid(lower(ls_viol),1,4)='some' AND mid(lower(ls_slang),1,4)='some' THEN
			ls_text = ls_sex_cap+", "+ls_slang+", and "+ls_viol+". "
		ELSE
			ls_text = ls_slang_cap+", "+ls_viol+", and "+ls_sex+". "
		END IF
	ELSEIF NOT(IsNull(ls_viol)) AND NOT(IsNull(ls_slang)) THEN
		IF mid(lower(ls_viol),1,4)='some' AND mid(lower(ls_slang),1,4)<>'some' THEN
			ls_text = ls_slang_cap+" and "+ls_viol+". "
		ELSE
			ls_text = ls_viol_cap+" and "+ls_slang+". "
		END IF
	ELSEIF NOT(IsNull(ls_sex)) AND NOT(IsNull(ls_slang)) THEN
		IF mid(lower(ls_slang),1,4)='some' AND mid(lower(ls_sex),1,4)<>'some' THEN
			ls_text = ls_sex_cap+" and "+ls_slang+". "
		ELSE
			ls_text = ls_slang_cap+" and "+ls_sex+". "
		END IF
	ELSEIF NOT(IsNull(ls_sex)) AND NOT(IsNull(ls_viol)) THEN
		IF mid(lower(ls_viol),1,4)='some' AND mid(lower(ls_sex),1,4)<>'some' THEN
			ls_text = ls_sex_cap+" and "+ls_viol+". "
		ELSE
			ls_text = ls_viol_cap+" and "+ls_sex+". "
		END IF
	ELSEIF NOT(IsNull(ls_viol)) THEN
		ls_text = ls_viol_cap+". "
	ELSEIF NOT(IsNull(ls_sex)) THEN
		ls_text = ls_sex_cap+". "
	ELSEIF NOT(IsNull(ls_slang)) THEN
		ls_text = ls_slang_cap+". "
	ELSEIF IsNull(ls_sex) AND IsNull(ls_viol) AND IsNull(ls_slang) THEN
		ls_text = " "
	END IF		
// Else if order is violence, strong language and sex
ELSEIF li_sexord=3 AND li_violord=1 AND li_slangord=2 THEN
	// If all sex, violence and strong language are not NULL
	IF NOT(IsNull(ls_sex)) AND NOT(IsNull(ls_viol)) AND NOT(IsNull(ls_slang)) THEN
		IF mid(lower(ls_sex),1,4)='some' AND mid(lower(ls_viol),1,4)<>'some' AND mid(lower(ls_slang),1,4)<>'some' THEN
			ls_text = ls_viol_cap+", "+ls_slang+", and "+ls_sex+". "
		ELSEIF mid(lower(ls_sex),1,4)<>'some' AND mid(lower(ls_viol),1,4)='some' AND mid(lower(ls_slang),1,4)<>'some' THEN
			ls_text = ls_slang_cap+", "+ls_sex+", and "+ls_viol+". "
		ELSEIF mid(lower(ls_sex),1,4)<>'some' AND mid(lower(ls_viol),1,4)<>'some' AND mid(lower(ls_slang),1,4)='some' THEN
			ls_text = ls_viol_cap+", "+ls_sex+", and "+ls_slang+". "
		ELSEIF mid(lower(ls_sex),1,4)='some' AND mid(lower(ls_viol),1,4)='some' AND mid(lower(ls_slang),1,4)<>'some' THEN
			ls_text = ls_slang_cap+", "+ls_viol+", and "+ls_sex+". "
		ELSEIF mid(lower(ls_sex),1,4)='some' AND mid(lower(ls_viol),1,4)<>'some' AND mid(lower(ls_slang),1,4)='some' THEN
			ls_text = ls_viol_cap+", "+ls_slang+", and "+ls_sex+". "
		ELSEIF mid(lower(ls_sex),1,4)<>'some' AND mid(lower(ls_viol),1,4)='some' AND mid(lower(ls_slang),1,4)='some' THEN
			ls_text = ls_sex_cap+", "+ls_slang+", and "+ls_viol+". "
		ELSE
			ls_text = ls_viol_cap+", "+ls_slang+", and "+ls_sex+". "
		END IF
	ELSEIF NOT(IsNull(ls_viol)) AND NOT(IsNull(ls_slang)) THEN
		IF mid(lower(ls_viol),1,4)='some' AND mid(lower(ls_slang),1,4)<>'some' THEN
			ls_text = ls_slang_cap+" and "+ls_viol+". "
		ELSE
			ls_text = ls_viol_cap+" and "+ls_slang+". "
		END IF
	ELSEIF NOT(IsNull(ls_sex)) AND NOT(IsNull(ls_slang)) THEN
		IF mid(lower(ls_slang),1,4)='some' AND mid(lower(ls_sex),1,4)<>'some' THEN
			ls_text = ls_sex_cap+" and "+ls_slang+". "
		ELSE
			ls_text = ls_slang_cap+" and "+ls_sex+". "
		END IF
	ELSEIF NOT(IsNull(ls_sex)) AND NOT(IsNull(ls_viol)) THEN
		IF mid(lower(ls_viol),1,4)='some' AND mid(lower(ls_sex),1,4)<>'some' THEN
			ls_text = ls_sex_cap+" and "+ls_viol+". "
		ELSE
			ls_text = ls_viol_cap+" and "+ls_sex+". "
		END IF
	ELSEIF NOT(IsNull(ls_viol)) THEN
		ls_text = ls_viol_cap+". "
	ELSEIF NOT(IsNull(ls_sex)) THEN
		ls_text = ls_sex_cap+". "
	ELSEIF NOT(IsNull(ls_slang)) THEN
		ls_text = ls_slang_cap+". "
	ELSEIF IsNull(ls_sex) AND IsNull(ls_viol) AND IsNull(ls_slang) THEN
		ls_text = " "
	END IF		
END IF


RETURN ls_text


end function

public function boolean wf_check_anno_flag (string ls_chno);string lannoflg

select prop_added_flg into :lannoflg from annotation where chno = :ls_chno using sqlservertrans;

If lannoflg='Y' THEN
	RETURN TRUE
Else
	RETURN FALSE
END IF

end function

public function string wf_which_flang (string flang_code);string ls_lang_desc

SetNull(ls_lang_desc)

Select lang_desc
into :ls_lang_desc
from language
where lang_code=:flang_code
using SQlServerTrans;

IF NOT(IsNull(ls_lang_desc)) THEN
	RETURN TRIM(ls_lang_desc)+" language"
END IF

end function

public function string wf_build_book_sentence (string ls_casub, string ls_prize, string ls_anyr, string ls_bseller_flag, string ls_med, string ls_g1br, string ls_conno, string ls_flang);int li_casubord,li_prizeord,li_bsellerord,li_anyr,li_pbr,li_gbr,li_flang,li_comm_audio
string ls_bseller,lcasub,lprize,lanyr,lbseller,ls_pbr,ls_gbr,media,lgbr,ls_language
string ls_text1,ls_text2,ls_text3,ls_text4,ls_text5,ls_text6,ls_text7,ls_text8, ls_text,lpanno

n_cst_string	lnv_string
SetNull(lcasub)

SELECT anno_properties.casub_ord,   
       anno_properties.prize_ord,   
       anno_properties.bestseller_modif,   
       anno_properties.bestseller_ord,   
       anno_properties.year_written_ord,
       anno_properties.prnt_br_modif,
       anno_properties.prnt_br_ord,
       anno_properties.grd_1_br_modif,
       anno_properties.grd_1_br_ord,
       anno_properties.flang_ord,
	 anno_properties.comm_audio_ord // 09/30/2008
  INTO :li_casubord,   
       :li_prizeord,   
       :ls_bseller,   
       :li_bsellerord,   
       :li_anyr,
		 :ls_pbr,
		 :li_pbr,
		 :ls_gbr,
		 :li_gbr,
		 :li_flang,
		 :li_comm_audio
FROM anno_properties
USING SQlServerTrans;

select med into :media from mchar where conno = :ls_conno using sqlservertrans;

IF NOT(IsNull(ls_casub)) THEN
	lcasub=ls_casub
END IF

IF NOT(IsNull(ls_prize)) THEN
	lprize=ls_prize
END IF

IF ls_bseller_flag='Y' THEN
	IF IsNull(ls_bseller) THEN
		lbseller="BestSeller"
	ELSE
		lbseller=ls_bseller
	END IF
END IF

IF ls_g1br='Y' THEN
	IF IsNull(ls_gbr) THEN
		lgbr="Grade 1 Braille"
	ELSE
		lgbr= ls_gbr
	END IF
END IF

IF NOT(IsNull(ls_casub)) AND ls_casub<>"" THEN
	Select desc_ into :lcasub 
	from grade
	where gradecd = :ls_casub
	using sqlservertrans;
	
	lcasub = TRIM(lcasub)
ELSE
	IsNull(lcasub)
END IF

IF li_casubord=1 THEN
	IF Not(IsNull(lcasub)) THEN
		ls_text1=lcasub+'. '
	ELSE
		ls_text1=" "		
	END IF
ELSEIF li_casubord=2 THEN
	IF Not(IsNull(lcasub)) THEN
		ls_text2=lcasub+'. '
	ELSE
		ls_text2=" "		
	END IF
ELSEIF li_casubord=3 THEN
	IF Not(IsNull(lcasub)) THEN
		ls_text3=lcasub+'. '
	ELSE
		ls_text3=" "		
	END IF
ELSEIF li_casubord=4 THEN
	IF Not(IsNull(lcasub)) THEN
		ls_text4=lcasub+'. '
	ELSE
		ls_text4=" "		
	END IF
ELSEIF li_casubord=5 THEN
	IF Not(IsNull(lcasub)) THEN
		ls_text5=lcasub+'. '
	ELSE
		ls_text5=" "		
	END IF
ELSEIF li_casubord=6 THEN
	IF Not(IsNull(lcasub)) THEN
		ls_text6=lcasub+'. '
	ELSE
		ls_text6=" "		
	END IF
ELSEIF li_casubord=7 THEN
	IF Not(IsNull(lcasub)) THEN
		ls_text7=lcasub+'. '
	ELSE
		ls_text7=" "		
	END IF
	// 09/30/2008	
ELSEIF li_casubord=8 THEN
	IF Not(IsNull(lcasub)) THEN
		ls_text8=lcasub+'. '
	ELSE
		ls_text8=" "		
	END IF
END IF

// Prize

IF li_prizeord=1 THEN
	IF Not(IsNull(lprize)) THEN
		ls_text1=TRIM(lprize)+'. '
	END IF
ELSEIF li_prizeord=2 THEN
	IF Not(IsNull(lprize)) THEN
		ls_text2=TRIM(lprize)+'. '
	END IF
ELSEIF li_prizeord=3 THEN
	IF Not(IsNull(lprize)) THEN
		ls_text3=TRIM(lprize)+'. '
	END IF
ELSEIF li_prizeord=4 THEN
	IF Not(IsNull(lprize)) THEN
		ls_text4=TRIM(lprize)+'. '
	END IF
ELSEIF li_prizeord=5 THEN
	IF Not(IsNull(lprize)) THEN
		ls_text5=TRIM(lprize)+'. '
	END IF
ELSEIF li_prizeord=6 THEN
	IF Not(IsNull(lprize)) THEN
		ls_text6=TRIM(lprize)+'. '
	END IF
ELSEIF li_prizeord=7 THEN
	IF Not(IsNull(lprize)) THEN
		ls_text7=TRIM(lprize)+'. '
	END IF
	// 09/30/2008	
ELSEIF li_prizeord=8 THEN
	IF Not(IsNull(lprize)) THEN
		ls_text8=TRIM(lprize)+'. '
	END IF
END IF

// Best Seller

IF li_bsellerord=1 THEN
	IF Not(IsNull(lbseller)) THEN
		ls_text1=lbseller+'. '
	END IF
ELSEIF li_bsellerord=2 THEN
	IF Not(IsNull(lbseller)) THEN
		ls_text2=lbseller+'. '
	END IF
ELSEIF li_bsellerord=3 THEN
	IF Not(IsNull(lbseller)) THEN
		ls_text3=lbseller+'. '
	END IF
ELSEIF li_bsellerord=4 THEN
	IF Not(IsNull(lbseller)) THEN
		ls_text4=lbseller+'. '
	END IF
ELSEIF li_bsellerord=5 THEN
	IF Not(IsNull(lbseller)) THEN
		ls_text5=lbseller+'. '
	END IF
ELSEIF li_bsellerord=6 THEN
	IF Not(IsNull(lbseller)) THEN
		ls_text6=lbseller+'. '
	END IF
ELSEIF li_bsellerord=7 THEN
	IF Not(IsNull(lbseller)) THEN
		ls_text7=lbseller+'. '
	END IF
	// 09/30/2008
ELSEIF li_bsellerord=8 THEN
	IF Not(IsNull(lbseller)) THEN
		ls_text8=lbseller+'. '
	END IF
END IF

// 01/28/2008 if commercial audio Add annotation text to the year
string ls_comm_audio_yn, ls_audio=''
IF Len(trim(is_comm_audio)) > 0 THEN
	ls_comm_audio_yn =is_comm_audio 
ELSE
	ls_comm_audio_yn =dw_final_review.object.mchar_comm_audio_yn[1]
END IF

IF ls_comm_audio_yn = 'Y' THEN
	IF Not(IsNull(ls_anyr)) THEN
		ls_audio = 'Commercial audiobook'
	END IF
ELSE
	IF Not(IsNull(ls_anyr)) THEN
		ls_audio = ''
	END IF
END IF

//Commercial Audio
IF li_comm_audio=1 THEN
	IF Not(IsNull(ls_audio)) THEN
		ls_text1=ls_audio+'. '
	END IF
ELSEIF li_comm_audio=2 THEN
	IF Not(IsNull(ls_audio)) THEN
		ls_text2=ls_audio+'. '
	END IF
ELSEIF li_comm_audio=3 THEN
	IF Not(IsNull(ls_audio)) THEN
		ls_text3=ls_audio+'. '
	END IF
ELSEIF li_comm_audio=4 THEN
	IF Not(IsNull(ls_audio)) THEN
		ls_text4=ls_audio+'. '
	END IF
ELSEIF li_comm_audio=5 THEN
	IF Not(IsNull(ls_audio)) THEN
		ls_text5=ls_audio+'. '
	END IF
ELSEIF li_comm_audio=6 THEN
	IF Not(IsNull(ls_audio)) THEN
		ls_text6=ls_audio+'. '
	END IF
ELSEIF li_comm_audio=7 THEN
	IF Not(IsNull(ls_audio)) THEN
		ls_text7=ls_audio+'. '
	END IF
// 09/30/2008
ELSEIF li_comm_audio=8 THEN
	IF Not(IsNull(ls_audio)) THEN
		ls_text8=ls_audio+'. '
	END IF
END IF


// Anntation written year

IF li_anyr=1 THEN
	IF Not(IsNull(ls_anyr)) THEN
		ls_text1=ls_anyr+'. '
	END IF
ELSEIF li_anyr=2 THEN
	IF Not(IsNull(ls_anyr)) THEN
		ls_text2=ls_anyr+'. '
	END IF
ELSEIF li_anyr=3 THEN
	IF Not(IsNull(ls_anyr)) THEN
		ls_text3=ls_anyr+'. '
	END IF
ELSEIF li_anyr=4 THEN
	IF Not(IsNull(ls_anyr)) THEN
		ls_text4=ls_anyr+'. '
	END IF
ELSEIF li_anyr=5 THEN
	IF Not(IsNull(ls_anyr)) THEN
		ls_text5=ls_anyr+'. '
	END IF
ELSEIF li_anyr=6 THEN
	IF Not(IsNull(ls_anyr)) THEN
		ls_text6=ls_anyr+'. '
	END IF
ELSEIF li_anyr=7 THEN
	IF Not(IsNull(ls_anyr)) THEN
		ls_text7=ls_anyr+'. '
	END IF
	// 09/30/2008 
ELSEIF li_anyr=8 THEN
	IF Not(IsNull(ls_anyr)) THEN
		ls_text8=ls_anyr+'. '
	END IF
END IF

// Print Braille

IF media='P/B' THEN
	IF li_pbr=1 THEN
		IF Not(IsNull(ls_pbr)) THEN
			ls_text1=TRIM(ls_pbr)+'. '
		END IF
	ELSEIF li_pbr=2 THEN
		IF Not(IsNull(ls_pbr)) THEN
			ls_text2=TRIM(ls_pbr)+'. '
		END IF
	ELSEIF li_pbr=3 THEN
		IF Not(IsNull(ls_pbr)) THEN
			ls_text3=TRIM(ls_pbr)+'. '
		END IF
	ELSEIF li_pbr=4 THEN
		IF Not(IsNull(ls_pbr)) THEN
			ls_text4=TRIM(ls_pbr)+'. '
		END IF
	ELSEIF li_pbr=5 THEN
		IF Not(IsNull(ls_pbr)) THEN
			ls_text5=TRIM(ls_pbr)+'. '
		END IF
	ELSEIF li_pbr=6 THEN
		IF Not(IsNull(ls_pbr)) THEN
			ls_text6=TRIM(ls_pbr)+'. '
		END IF
	ELSEIF li_pbr=7 THEN
		IF Not(IsNull(ls_pbr)) THEN
			ls_text7=TRIM(ls_pbr)+'. '
		END IF
	// 09/30/2008
	ELSEIF li_pbr=8 THEN
		IF Not(IsNull(ls_pbr)) THEN
			ls_text8=TRIM(ls_pbr)+'. '
		END IF
	END IF
END IF

// Grade I Braille

IF ls_g1br='Y' THEN
	IF li_gbr=1 THEN
		IF Not(IsNull(lgbr)) THEN
			ls_text1=TRIM(lgbr)+'. '
		END IF
	ELSEIF li_gbr=2 THEN
		IF Not(IsNull(lgbr)) THEN
			ls_text2=TRIM(lgbr)+'. '
		END IF
	ELSEIF li_gbr=3 THEN
		IF Not(IsNull(lgbr)) THEN
			ls_text3=TRIM(lgbr)+'. '
		END IF
	ELSEIF li_gbr=4 THEN
		IF Not(IsNull(lgbr)) THEN
			ls_text4=TRIM(lgbr)+'. '
		END IF
	ELSEIF li_gbr=5 THEN
		IF Not(IsNull(lgbr)) THEN
			ls_text5=TRIM(lgbr)+'. '
		END IF
	ELSEIF li_gbr=6 THEN
		IF Not(IsNull(lgbr)) THEN
			ls_text6=TRIM(lgbr)+'. '
		END IF
	ELSEIF li_gbr=7 THEN
		IF Not(IsNull(TRIM(lgbr))) THEN
			ls_text7=TRIM(lgbr)+'. '
		END IF
		  // 09/30/2008		
	ELSEIF li_gbr=8 THEN
		IF Not(IsNull(TRIM(lgbr))) THEN
			ls_text8=TRIM(lgbr)+'. '
		END IF
	END IF
END IF

// Foreign Language


IF ls_flang<>'ENG' THEN
	// This book is in a foreign language, find which one?
	ls_language = wf_which_flang(ls_flang)
	// Find a location for it.
	IF li_flang=1 THEN
		ls_text1=TRIM(ls_language)+'. '
	ELSEIF li_flang=2 THEN
		ls_text2=TRIM(ls_language)+'. '
	ELSEIF li_flang=3 THEN
		ls_text3=TRIM(ls_language)+'. '
	ELSEIF li_flang=4 THEN
		ls_text4=TRIM(ls_language)+'. '
	ELSEIF li_flang=5 THEN
		ls_text5=TRIM(ls_language)+'. '
	ELSEIF li_flang=6 THEN
		ls_text6=TRIM(ls_language)+'. '
	ELSEIF li_flang=7 THEN
		ls_text7=TRIM(ls_language)+'. '
			// 09/30/2008		
	ELSEIF li_flang=8 THEN
		ls_text8 +=TRIM(ls_language)+'. '
	END IF
END IF

IF IsNull(ls_text1) OR ls_text1=". " THEN
	ls_text1=" "
END IF
IF IsNull(ls_text2) OR ls_text2=". " THEN
	ls_text2=" "
END IF
IF IsNull(ls_text3) OR ls_text3=". " THEN
	ls_text3=" "
END IF
IF IsNull(ls_text4) OR ls_text4=". " THEN
	ls_text4=" "
END IF
IF IsNull(ls_text5) OR ls_text5=". " THEN
	ls_text5=" "
END IF
IF IsNull(ls_text6) OR ls_text6=". " THEN
	ls_text6=" "
END IF
IF IsNull(ls_text7) OR ls_text7=". " THEN
	ls_text7=" "
END IF

// 09/30/2008
IF IsNull(ls_text8) OR ls_text8=". " THEN
	ls_text8=" "
END IF

ls_text=ls_text1+ls_text2+ls_text3+ls_text4+ls_text5+ls_text6+ls_text7 + ls_text8 // 09/30/2008 text 8 order

ls_text =  lnv_string.of_GlobalReplace(ls_text, ". . . . . .", " ")
ls_text =  lnv_string.of_GlobalReplace(ls_text, ". . . . .", " ")
ls_text =  lnv_string.of_GlobalReplace(ls_text, ". . . .", " ")
ls_text =  lnv_string.of_GlobalReplace(ls_text, ". . .", " ")
//ls_text =  lnv_string.of_GlobalReplace(ls_text, ". .", " ")

RETURN ls_text


end function

public subroutine wf_disable_all ();dw_final_approval.Enabled = FALSE
dw_final_review.Enabled = FALSE
dw_coauthors.Enabled = FALSE
dw_copyright.Enabled = FALSE
mle_anno.Enabled = FALSE
cb_clear.Enabled = FALSE
cb_update.Enabled = FALSE
cb_find.Enabled = FALSE
cb_anno.Enabled = FALSE

end subroutine

public subroutine wf_disable_buttons ();dw_final_approval.Modify("conno.Tabsequence = 10")
dw_final_approval.Modify("fr.Tabsequence = 20")
dw_final_approval.Modify("priority.Tabsequence = 0")
dw_final_approval.Modify("frflag.Tabsequence = 0")
dw_final_approval.SetItem(1,"conno", "") 
dw_final_approval.Setfocus()
dw_final_approval.Setcolumn("conno")
dw_final_review.Visible = FALSE
dw_ri.Visible = FALSE
dw_coauthors.Visible = FALSE
dw_copyright.Visible = FALSE
dw_cdssubject.Visible = FALSE
mle_anno.Visible = FALSE
st_2.Visible = FALSE
cb_update.Enabled=FALSE
cb_zoom.Enabled=FALSE
cb_clear.Enabled=FALSE
cb_find.Enabled=TRUE
cb_anno.Enabled = FALSE

cb_bknav.Enabled = FALSE
cb_bknav.visible = FALSE

cb_spell.Enabled = FALSE
cb_spell.visible = FALSE



end subroutine

public subroutine wf_enable_buttons ();w_sheet_final_review.cb_update.Enabled=TRUE
w_sheet_final_review.cb_clear.Enabled=TRUE
dw_final_approval.Setfocus()
dw_final_approval.Modify("conno.Tabsequence = 0")
dw_final_approval.Modify("fr.Tabsequence = 10")
dw_final_approval.Modify("pcsstdt.Tabsequence = 20")
dw_final_approval.Modify("priority.Tabsequence = 30")
dw_final_approval.Modify("frflag.Tabsequence = 40")
dw_final_review.Visible = TRUE
dw_ri.Visible = TRUE
dw_copyright.Visible = TRUE
dw_coauthors.Visible = TRUE
dw_cdssubject.Visible = TRUE
mle_anno.Visible = TRUE
st_2.Visible = TRUE
w_sheet_final_review.cb_find.Enabled=FALSE
cb_anno.Enabled = TRUE
cb_zoom.Enabled=TRUE

cb_bknav.Enabled = TRUE
cb_bknav.visible = TRUE

cb_spell.Enabled = TRUE
cb_spell.visible = TRUE



end subroutine

public function string wf_sex_viol_lang ();string ls_sexcd,ls_viol,ls_lang,ls_text,ls_casub,ls_prize,ls_anyr,ls_bseller,ls_book,ls_med,ls_gbr,ls_conno,ls_sextext,ls_flang
integer currow,li_sex_grp_ord,li_med_grp_ord
n_cst_string	lnv_string
SetNull(ls_casub)

SELECT anno_properties.sex_group_ord,   
       anno_properties.med_group_ord 
INTO :li_sex_grp_ord,   
     :li_med_grp_ord
FROM anno_properties  USING SQLServerTrans;

currow = dw_final_review.Getrow()

ls_sexcd = dw_final_review.object.ttlinit_sexcd[currow]
ls_viol 	= dw_final_review.object.ttlinit_violencecd[currow]
ls_lang 	= dw_final_review.object.ttlinit_stronglang[currow]

ls_casub 	= dw_final_review.object.ttlinit_gradecd[currow]
ls_prize 	= dw_final_review.object.ttlinit_prize[currow]
ls_bseller 	= dw_final_review.object.ttlinit_bestseller[currow]
ls_flang 	= dw_final_review.object.ttlinit_lang[currow]
ls_anyr 		= string(dw_final_review.object.ttlinit_anyr[currow])

ls_gbr 		= dw_final_review.object.mchar_g1br[currow]
ls_med 		= dw_final_review.object.mchar_med[currow]


ls_conno = dw_final_approval.Getitemstring(1,"conno")
  

ls_book 		= wf_build_book_sentence(ls_casub,ls_prize,ls_anyr,ls_bseller,ls_med,ls_gbr,ls_conno,ls_flang)
ls_sextext 	= wf_build_sex_sentence(ls_sexcd,ls_viol,ls_lang)

IF li_sex_grp_ord=1 AND li_med_grp_ord=2 THEN
   ls_text = TRIM(ls_sextext)+" "+TRIM(ls_book)
ELSEIF li_sex_grp_ord=2 AND li_med_grp_ord=1 THEN
   ls_text = TRIM(ls_book)+" "+TRIM(ls_sextext)
ELSE
   ls_text = TRIM(ls_book)+" "+TRIM(ls_sextext)
END IF

ls_text =  lnv_string.of_GlobalReplace(ls_text, ". . . . . .",".")
ls_text =  lnv_string.of_GlobalReplace(ls_text, ". . . . .",".")
ls_text =  lnv_string.of_GlobalReplace(ls_text, ". . . .",".")
ls_text =  lnv_string.of_GlobalReplace(ls_text, ". . .",".")
ls_text =  lnv_string.of_GlobalReplace(ls_text, ". .",".")

//MessageBox("ls_text",ls_text)
RETURN ls_text
end function

public subroutine wf_validate_ricd ();//string ricode
//long prevbkseq
//
//ricode = dw_final_review.object.mchar_ricd[1]
//prevbkseq = dw_final_review.object.ri_prevbkseq[1]
//
//IF (ricode <> ""  or IsNull(ricode)=FALSE) and (prevbkseq > 0)  THEN
//	dw_final_review.Object.mchar_ricd.tabsequence='0'
////	dw_final_review.Object.ri_prevbkmed.tabsequence='0'
////	dw_final_review.Object.ri_prevbkseq.tabsequence='0'
//END IF
end subroutine

public function integer wf_retrieve ();int ll_rows

dw_final_approval.of_SetLinkage(TRUE)
dw_final_review.of_SetLinkage(TRUE)
dw_copyright.of_SetLinkage(TRUE)
dw_coauthors.of_SetLinkage(TRUE)
dw_ri.of_SetLinkage(TRUE)
dw_cdssubject.of_SetLinkage(TRUE)

dw_final_review.inv_linkage.of_SetMaster(dw_final_approval)
dw_final_review.inv_linkage.of_Register("conno", "mchar_conno")

dw_copyright.inv_linkage.of_SetMaster(dw_final_approval)
dw_copyright.inv_linkage.of_Register("conno", "mchar_conno")

dw_coauthors.inv_linkage.of_SetMaster(dw_final_approval)
dw_coauthors.inv_linkage.of_Register("conno", "mchar_conno")

dw_cdssubject.inv_linkage.of_SetMaster(dw_final_approval)
dw_cdssubject.inv_linkage.of_Register("conno", "mchar_conno")

dw_ri.inv_linkage.of_SetMaster(dw_final_approval)
dw_ri.inv_linkage.of_Register("conno", "mchar_conno")


dw_final_review.inv_linkage.of_SetStyle(dw_final_review.inv_linkage.RETRIEVE)
dw_copyright.inv_linkage.of_SetStyle(dw_copyright.inv_linkage.RETRIEVE)
dw_coauthors.inv_linkage.of_SetStyle(dw_coauthors.inv_linkage.RETRIEVE)
dw_cdssubject.inv_linkage.of_SetStyle(dw_cdssubject.inv_linkage.RETRIEVE)
dw_ri.inv_linkage.of_SetStyle(dw_ri.inv_linkage.RETRIEVE)

dw_final_approval.of_SetTransObject( sqlservertrans )
ll_rows = dw_final_approval.inv_linkage.of_Retrieve()

return 1


end function

public subroutine wf_get_rest_of_data ();string annotxt, lbkseq, ls_conno, ls_arg, ls_frflag, ls_priority
long ll_row
datetime ld_date,ld_pcsstdt
integer li_rtn
Lchno = dw_final_approval.object.chno[1]
Lconno = dw_final_approval.object.conno[1]

Select anno into :annotxt from annotation where chno = :Lchno using sqlservertrans;
IF sqlservertrans.SQLCode = 0  THEN
	mle_anno.text = annotxt
	anno_exist=TRUE
ELSE
	anno_exist=FALSE
	mle_anno.text = ""
END IF
	
ls_frflag = dw_final_approval.object.frflag[1]
IF ISNULL(ls_frflag) OR TRIM(ls_frflag) = "" THEN
	dw_final_approval.object.frflag[1]="Y"
	// 01/14/2010 #2205 first pass/initial updates
	ib_firsttime=TRUE // if frflag is null, initial pass.
ELSE
	ib_firsttime=FALSE
END IF
	
ld_date = dw_final_approval.object.fr[1]
IF ISNULL(ld_date) THEN
	dw_final_approval.object.fr[1]=Today()
	dw_final_approval.object.frflag[1]="Y"
END IF
	
ld_pcsstdt = dw_final_approval.object.pcsstdt[1]
IF ISNULL(ld_pcsstdt) AND dw_final_approval.object.pcsstdt_flg[1]<>"N" THEN
	dw_final_approval.object.pcsstdt[1]=ld_date
END IF
	
ls_priority = dw_final_approval.object.priority[1]
IF ISNULL(ls_priority) OR TRIM(ls_priority) = "" THEN
	dw_final_approval.object.priority[1]="N"
END IF
	
select prevbkseq into :Lbkseq from ri where conno =:lconno using sqlservertrans;
IF sqlservertrans.sqlcode = 0 then
	ri_exist=TRUE
ELSE
	ri_exist=FALSE
END IF		

end subroutine

public subroutine wf_final_approval (date as_date);string ls_sql, ls_modsql
long ll_pos

//Bringing the Original SQL statement of final approval datawindow
ls_sql = dw_final_approval.object.Datawindow.Table.Select
//Finding the string in final approval datawindow 
ll_pos = Pos(ls_sql, "WHERE ", 1)
//Removing the WHERE clause in the above SQL statement
ls_modsql = Mid(ls_sql, 1, ll_pos - 1)
//Attaching the WHERE clause with argument as date in the place of conno
//ls_modsql = ls_modsql + "WHERE TO_CHAR(mchar.fr, "+"~'"+"MM/DD/YYYY"+"~')"+" = "+ "~'" + string(as_date, "MM/DD/YYYY") + "~'"
//ls_modsql = ls_modsql + "WHERE TO_CHAR(mchar.fr, "+"~'"+"MM/DD/YYYY"+"~') = "+"~'"+as_date+"~'"
ls_modsql = ls_modsql + "WHERE mchar.fr = "+"~'"+string(as_date,'DD-MMM-YYYY')+"~'"
//Bringing all rows with modified WHERE clause date as argument
dw_final_approval.object.Datawindow.Table.Select = ls_modsql
end subroutine

public function integer wf_validate_conno (string control_no);String L99
int length

length = len(control_no)
IF length <> 8 THEN
	return 1
END IF

L99=Mid(control_no,3,2)
IF L99 <> '99' THEN
	return 2
END IF

return 3
end function

public function integer wf_clear_label_fields (boolean ab_visible);// 11/03/2009 DB Labels Project
//cbx_1.visible=ab_visible
//cbx_2.visible=ab_visible
//cbx_3.visible=ab_visible
//cb_1.visible=ab_visible
//st_4.visible=ab_visible
//st_5.visible=ab_visible
//st_6.visible=ab_visible
string ls_bkmed


// validate conno
Lconno = dw_final_approval.object.conno[1]
select bkmed  into :ls_bkmed from mchar where conno = :lconno using sqlservertrans ;

IF ls_bkmed = 'DB' THEN
	cb_1.visible=ab_visible
	cbx_1.visible=ab_visible
	cbx_2.visible=ab_visible
	cbx_3.visible=ab_visible
	st_4.visible=ab_visible
	st_5.visible=ab_visible
	st_6.visible=ab_visible
ELSE
	cb_1.visible=FALSE
	cbx_1.visible=FALSE
	cbx_2.visible=FALSE
	cbx_3.visible=FALSE
	st_4.visible=FALSE
	st_5.visible=FALSE
	st_6.visible=FALSE
END IF

RETURN 1
end function

public function integer wf_get_label_data (string as_conno);//11/03/2009 5.0

string ls_data='N', ls_print='N', ls_braille='N'
integer li_ret

li_ret = f_get_label_data(as_conno, ls_data, ls_print, ls_braille)

wf_clear_label_fields(TRUE)

IF ls_data = 'Y' THEN
	cbx_1.checked=TRUE
ELSE
	cbx_1.checked=FALSE
END IF
IF ls_print = 'Y' THEN
	cbx_2.checked=TRUE
ELSE
	cbx_2.checked=FALSE
END IF

IF ls_braille = 'Y' THEN
	cbx_3.checked=TRUE
ELSE
	cbx_3.checked=FALSE
END IF

	
cb_1.enabled=TRUE

RETURN 1



end function

public function integer wf_validate_cdssubject ();long 		ll_rc, LL_RC2,ll_foundrow
integer 	li_loop, li_return=1
string 	ls_cd

ll_rc = ldwc_cdssubcode.RowCount()

ll_rc2 = dw_cdssubject.rowcount()

FOR  li_loop = 1 TO ll_rc2 
	ls_cd = trim(dw_cdssubject.object.cdssubjectcode[li_loop])
	ll_foundrow = ldwc_cdssubcode.Find( "pmsub_code = '" + ls_cd  + "'", 1, ll_rc)
	IF ll_foundrow = 0 THEN
		li_return = -1
		exit
	END IF
NEXT
return li_return
end function

event open;call super::open;// Open SQL Spy
//gnv_app.inv_debug.inv_SQLSpy.of_OpenSQLSpy(TRUE)

lnvo_val_anno = CREATE nvo_val_init_con
THIS.Windowstate = maximized!




end event

on w_sheet_final_review.create
int iCurrent
call super::create
this.cb_1=create cb_1
this.cb_bknav=create cb_bknav
this.cb_spell=create cb_spell
this.dw_annotrpt_ec_report=create dw_annotrpt_ec_report
this.dw_cdssubject=create dw_cdssubject
this.cb_zoom=create cb_zoom
this.cb_anno=create cb_anno
this.mle_sga=create mle_sga
this.dw_final_review=create dw_final_review
this.dw_coauthors=create dw_coauthors
this.dw_copyright=create dw_copyright
this.mle_anno=create mle_anno
this.st_2=create st_2
this.st_1=create st_1
this.sle_rowcount=create sle_rowcount
this.dw_ri=create dw_ri
this.st_currentrow=create st_currentrow
this.sle_currentrow=create sle_currentrow
this.cb_find=create cb_find
this.cb_update=create cb_update
this.cb_clear=create cb_clear
this.cb_exit=create cb_exit
this.dw_finalreview_report=create dw_finalreview_report
this.st_3=create st_3
this.sle_totcount=create sle_totcount
this.st_4=create st_4
this.cbx_1=create cbx_1
this.st_5=create st_5
this.cbx_2=create cbx_2
this.st_6=create st_6
this.cbx_3=create cbx_3
this.dw_final_approval=create dw_final_approval
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.cb_1
this.Control[iCurrent+2]=this.cb_bknav
this.Control[iCurrent+3]=this.cb_spell
this.Control[iCurrent+4]=this.dw_annotrpt_ec_report
this.Control[iCurrent+5]=this.dw_cdssubject
this.Control[iCurrent+6]=this.cb_zoom
this.Control[iCurrent+7]=this.cb_anno
this.Control[iCurrent+8]=this.mle_sga
this.Control[iCurrent+9]=this.dw_final_review
this.Control[iCurrent+10]=this.dw_coauthors
this.Control[iCurrent+11]=this.dw_copyright
this.Control[iCurrent+12]=this.mle_anno
this.Control[iCurrent+13]=this.st_2
this.Control[iCurrent+14]=this.st_1
this.Control[iCurrent+15]=this.sle_rowcount
this.Control[iCurrent+16]=this.dw_ri
this.Control[iCurrent+17]=this.st_currentrow
this.Control[iCurrent+18]=this.sle_currentrow
this.Control[iCurrent+19]=this.cb_find
this.Control[iCurrent+20]=this.cb_update
this.Control[iCurrent+21]=this.cb_clear
this.Control[iCurrent+22]=this.cb_exit
this.Control[iCurrent+23]=this.dw_finalreview_report
this.Control[iCurrent+24]=this.st_3
this.Control[iCurrent+25]=this.sle_totcount
this.Control[iCurrent+26]=this.st_4
this.Control[iCurrent+27]=this.cbx_1
this.Control[iCurrent+28]=this.st_5
this.Control[iCurrent+29]=this.cbx_2
this.Control[iCurrent+30]=this.st_6
this.Control[iCurrent+31]=this.cbx_3
this.Control[iCurrent+32]=this.dw_final_approval
end on

on w_sheet_final_review.destroy
call super::destroy
destroy(this.cb_1)
destroy(this.cb_bknav)
destroy(this.cb_spell)
destroy(this.dw_annotrpt_ec_report)
destroy(this.dw_cdssubject)
destroy(this.cb_zoom)
destroy(this.cb_anno)
destroy(this.mle_sga)
destroy(this.dw_final_review)
destroy(this.dw_coauthors)
destroy(this.dw_copyright)
destroy(this.mle_anno)
destroy(this.st_2)
destroy(this.st_1)
destroy(this.sle_rowcount)
destroy(this.dw_ri)
destroy(this.st_currentrow)
destroy(this.sle_currentrow)
destroy(this.cb_find)
destroy(this.cb_update)
destroy(this.cb_clear)
destroy(this.cb_exit)
destroy(this.dw_finalreview_report)
destroy(this.st_3)
destroy(this.sle_totcount)
destroy(this.st_4)
destroy(this.cbx_1)
destroy(this.st_5)
destroy(this.cbx_2)
destroy(this.st_6)
destroy(this.cbx_3)
destroy(this.dw_final_approval)
end on

event pfc_preopen;call super::pfc_preopen;this.of_SetPreference(TRUE)
this.inv_preference.of_SetToolBars(TRUE)
this.inv_preference.of_SetWindow(TRUE)
this.of_SetResize(TRUE)
this.inv_resize.of_SetOrigSize(this.workspacewidth(),this.workspaceheight())
inv_resize.of_Register(dw_final_approval, "Scale")
inv_resize.of_Register(dw_final_review, "Scale")
inv_resize.of_Register(dw_coauthors, "Scale")
inv_resize.of_Register(dw_copyright, "Scale")
inv_resize.of_Register(dw_cdssubject, "Scale")
inv_resize.of_Register(dw_ri, "Scale")
inv_resize.of_Register(cb_clear, "Scale")
inv_resize.of_Register(cb_zoom, "Scale")
inv_resize.of_Register(cb_update, "Scale")
inv_resize.of_Register(cb_exit, "Scale")
inv_resize.of_Register(cb_find, "Scale")
inv_resize.of_Register(cb_anno, "Scale")
inv_resize.of_Register(cb_bknav, "Scale")
inv_resize.of_Register(mle_anno,"Scale")
inv_resize.of_Register(st_3, "Scale")
inv_resize.of_Register(st_2, "Scale")
inv_resize.of_Register(st_1, "scale")
inv_resize.of_Register(sle_rowcount, "scale")
inv_resize.of_Register(st_currentrow, "scale")
inv_resize.of_Register(sle_currentrow, "scale")
inv_resize.of_Register(sle_totcount, "scale")
inv_resize.of_Register(cb_spell, "scale")

end event

event resize;call super::resize;long ll_height

//This script is not commented because when we do minimize this screen then all the DW'S are not resizing 
This.X = w_pics_main.X
This.Y = w_pics_main.Y
ll_height = w_pics_main.mdi_1.Height
This.Resize(w_pics_main.WorkSpaceWidth(), ll_height)


end event

event close;call super::close;DESTROY lnvo_val_anno
m_pics_main.m_file.m_print.Enabled 			=	FALSE
m_pics_main.m_file.m_pagesetup.Enabled		=	FALSE
m_pics_main.m_file.m_printimmediate.Enabled	=	FALSE
m_pics_main.m_edit.m_deleterow.Enabled 	=	FALSE
m_pics_main.m_edit.m_addrow.Enabled 		=	FALSE


end event

event closequery;Integer	li_pendingrc
Integer	li_validationrc
Integer	li_accepttextrc
Integer	li_msg
Integer	li_rc,rtn
String	ls_msgparms[]

IF ISNULL(sle_rowcount.Text) OR sle_rowcount.Text = "" THEN
	ib_disableclosequery = TRUE
END IF

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
					dw_final_approval.Setfocus()
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
			rtn = cb_update.Event ue_save()
			if rtn = 1 THEN
				RETURN 0
			end if
		Case 2
			// NO - Allow the window to be closed without saving changes
			Return 0
		Case 3
			dw_final_approval.Setfocus()
			// CANCEL -  Prevent the window from closing
	End Choose
End If

// Prevent the window from closing
ib_closestatus = False
Return 1
end event

event pfc_postopen;call super::pfc_postopen;string fr_conno

wf_disable_buttons()
m_pics_main.m_file.m_print.Enabled 			=	TRUE
m_pics_main.m_file.m_pagesetup.Enabled		=	TRUE
m_pics_main.m_file.m_printimmediate.Enabled	=	TRUE
dw_final_approval.enabled=true
dw_final_approval.visible=true


IF IsValid(w_sheet_spanish_books) THEN
	fr_conno = message.StringParm
	dw_final_approval.object.conno[1] = fr_conno
	cb_find.TriggerEvent(Clicked!)
	IF IsNull(dw_final_review.object.ttlinit_pmedt[dw_final_review.GetRow()]) THEN
		dw_final_review.object.ttlinit_pmedt[dw_final_review.GetRow()] = Today()
	END IF
END IF

//test
cbx_1.bringtotop=true
cbx_2.bringtotop=true
cbx_3.bringtotop=true

st_4.bringtotop=true
st_5.bringtotop=true
st_6.bringtotop=true
cb_1.bringtotop=true
wf_clear_label_fields(FALSE)
end event

event mousemove;call super::mousemove;w_pics_main.setmicrohelp("Ready")
end event

event pfc_endtran;call super::pfc_endtran;string ls_flag,ls_cdinit,ls_frinit
integer li_count,li_counts,rtn, li_cat_count
datetime ld_date,lfr
date lfr_date,ld_date_dt

IF ib_preupdatefail THEN
	RETURN -1
END IF

SetPointer(HourGlass!)
IF ai_update_results = 1 THEN	
	IF sqlservertrans.of_Commit() = 0 THEN
		SELECT COUNT(*) INTO :li_count
		FROM cr
		WHERE cr.chno = :Lchno
		USING sqlservertrans;
      IF NOT li_count > 0 THEN				
			ld_date = dw_copyright.object.cr_crgdt[1]
			INSERT INTO cr(chno,crgdt) VALUES(:Lchno, :ld_date)
			USING sqlservertrans;
			IF f_check_dberror(sqlservertrans,"CR")=FALSE THEN
				RETURN -1
			ELSE
				COMMIT USING SQLServerTrans;
			END IF
		ELSE
			ld_date = dw_copyright.object.cr_crgdt[1]
			ld_date_dt = date(ld_date)
			UPDATE cr SET crgdt =:ld_date_dt
			WHERE cr.chno =:Lchno
			USING sqlservertrans;
			IF f_check_dberror(sqlservertrans,"CR")=FALSE THEN
				RETURN -1
			ELSE
				COMMIT USING SQLServerTrans;
			END IF
		END IF
		SELECT COUNT(*) INTO :li_counts
      FROM catalog
      WHERE catalog.conno = :Lconno
      USING sqlservertrans;
		ls_flag = dw_final_approval.Getitemstring(il_currow,"frflag")
      IF NOT li_counts > 0 AND TRIM(ls_flag) = "Y" THEN
			lfr=dw_final_approval.object.fr[il_currow]
			lfr_date=date(lfr)
	      lfr_date = relativedate(lfr_date,7)
			lfr=datetime(lfr_date,time('00:00:00'))
	      INSERT INTO catalog(conno,s1) VALUES(:Lconno,:lfr)
         USING sqlservertrans;
			IF f_check_dberror(sqlservertrans,"Catalog")=FALSE THEN
				RETURN -1
			ELSE
				COMMIT USING SQLServerTrans;
			END IF
			
			// 04/24/2008 for RTB insert for DB or RC rows ( 2 records in catalog also)
			string ls_other_conno
			ls_other_conno = dw_final_approval.object.other_media_conno[il_currow]
			
			//#2205 12/15/09 avoid duplicate key error, check if exist and then insert
			SELECT COUNT(*) INTO :li_cat_count
				FROM catalog
				WHERE catalog.conno = :ls_other_conno
				USING sqlservertrans;
				
			IF li_cat_count = 0 THEN
				IF NOT Isnull(ls_other_conno) AND Len(Trim(ls_other_conno)) > 0 THEN
						 INSERT INTO catalog(conno,s1) VALUES(:ls_other_conno,:lfr)
							USING sqlservertrans;
					IF f_check_dberror(sqlservertrans,"Catalog")=FALSE THEN
						RETURN -1
					ELSE
						COMMIT USING SQLServerTrans;
					END IF
				END IF
			END IF // 12/16/09
				//////////// 04/24/2008

      ELSE
			IF TRIM(ls_flag) = "Y" THEN
				lfr=dw_final_approval.object.fr[il_currow]
				lfr_date=date(lfr)
				lfr_date = relativedate(lfr_date,7)
				lfr=datetime(lfr_date,time('00:00:00'))
	         UPDATE catalog  
            SET s1 = :lfr   
            WHERE catalog.conno = :Lconno  
	         USING sqlservertrans;
				IF f_check_dberror(sqlservertrans,"Catalog")=FALSE THEN
					RETURN -1
				ELSE
					COMMIT USING SQLServerTrans;
				END IF
			END IF
      END IF
		ls_cdinit = dw_final_review.object.ttlinit_cdinit[1]
		// This part of the code was commented out because of pr # 720
		// CDS initial should not be copied over by final reviewer initial.
		
//		IF ls_cdinit <> UPPER(sqlservertrans.userid) OR ISNULL(ls_cdinit) THEN
//			ls_cdinit = UPPER(gnv_app.of_getuserid())
//			dw_final_review.Setitem(dw_final_review.Getrow(),"ttlinit_cdinit",ls_cdinit)
//			UPDATE ttlinit
//			SET ttlinit.cdinit = :ls_cdinit
//			WHERE ttlinit.chno = :Lchno
//			USING sqlservertrans;
//			IF f_check_dberror(sqlservertrans,"TTLINIT")=FALSE THEN
//				RETURN -1
//			ELSE
//				COMMIT USING SQLServerTrans;
//			END IF
//		END IF

		//Delete the record in ri if there is no mchar_ricd
		string ls_ricd
		
		ls_ricd = dw_final_review.object.mchar_ricd[1]
		IF ISNULL(Trim(ls_ricd)) OR Trim(ls_ricd) = "" THEN
			DELETE FROM ri
			WHERE ri.conno = :Lconno
			USING sqlservertrans;
			IF f_check_dberror(sqlservertrans,"RI")=FALSE THEN
				RETURN -1
			ELSE
				COMMIT USING SQLServerTrans;
			END IF	
		END IF
		
		ls_frinit = dw_final_review.object.mchar_frinit[1]
		IF ls_frinit <> UPPER(sqlservertrans.userid) OR ISNULL(ls_frinit) THEN
			ls_frinit = UPPER(gnv_app.of_getuserid())
			dw_final_review.Setitem(dw_final_review.Getrow(),"mchar_frinit",ls_frinit)
			UPDATE mchar
			SET mchar.frinit = :ls_frinit
			WHERE mchar.conno = :Lconno
			USING sqlservertrans;
			IF f_check_dberror(sqlservertrans,"MCHAR")=FALSE THEN
				RETURN -1
			ELSE
				COMMIT USING SQLServerTrans;
			END IF
			
			// 10/02/2008 update frinit for both RC and DB rows
			ls_other_conno = dw_final_approval.object.other_media_conno[il_currow]
			IF NOT  isnull(ls_other_conno) THEN
				UPDATE mchar
				SET mchar.frinit = :ls_frinit
				WHERE mchar.conno = :ls_other_conno
				USING sqlservertrans;
				IF f_check_dberror(sqlservertrans,"MCHAR")=FALSE THEN
					RETURN -1
				ELSE
					COMMIT USING SQLServerTrans;
				END IF
			END IF
						
		END IF
		
		// 2125 12/3/2008
			String ls_comm_audio_yn, ls_comm_audio_vendor
				ls_comm_audio_yn = dw_final_review.object.mchar_comm_audio_yn[1]
				ls_comm_audio_vendor = dw_final_review.object.mchar_comm_audio_vendor[1]
				// 09/30/2008 Set commercial audio for all conno related to RC and DB for the book
				IF dw_final_review.Rowcount() > 0  THEN
						lconno = dw_final_review.object.mchar_conno[1]
						
						UPDATE mchar
							SET comm_audio_yn = :ls_comm_audio_yn,
							comm_audio_vendor = :ls_comm_audio_vendor					
						WHERE conno = :Lconno
						USING SQLServerTrans;
						IF f_check_dberror(sqlservertrans,"MCHAR - Commercial Audio setting")=FALSE THEN
							RETURN -1
						ELSE
							COMMIT USING SQLServerTrans;
						END IF
				END IF
				
			//12/4/2008 anno property was not updated if no is clicked on the zoom 2128
			Lsexviol = LeftTrim(wf_sex_viol_lang())
			UPDATE annotation
				 SET anno_property = :lsexviol
				WHERE chno = :Lchno
				USING SqlServerTrans;
				IF f_check_dberror(sqlservertrans,"ANNOTATION.ANNO_PROPERTY") THEN
					COMMIT USING SqlServerTrans;
				ELSE 
					ROLLBACK USING SqlServerTrans;
					RETURN -1
				END IF
			/////////
			
	   rtn = Messagebox("Update","Update Successful. Do you want to print the report?",Question!,YESNO!,1)
		IF rtn = 1 THEN
	   	rtn = Messagebox("Printing Reports","Do you want to print the titles passing final review report only(Yes), Edit Annotation only(No)?",Question!,YESNO!,1)
			IF rtn = 1 THEN	
				//OpenSheetwithparm(w_sheet_pics_ole_crystal,"finalreview",w_pics_main, 0, Original!)
				nvo_PowerPrn.of_SetPrinterOrientation(2)
				nvo_PowerPrn.of_SetCopies(1)
				dw_finalreview_report.Event pfc_retrieve()
				dw_finalreview_report.print()
			ELSEIF rtn = 2 THEN
			//ELSE
				//OpenSheetwithparm(w_sheet_pics_ole_crystal,"editannotation",w_pics_main, 0, Original!)
				dw_annotrpt_ec_report.Retrieve(Lconno)
				nvo_PowerPrn.of_SetPrinterOrientation(1)
				dw_annotrpt_ec_report.print()
			//ELSE
			//	nvo_PowerPrn.of_SetPrinterOrientation(2)
			//	dw_finalreview_report.Event pfc_retrieve()
			//	nvo_PowerPrn.of_SetPrinterOrientation(1)
			//	dw_annotrpt_ec_report.Retrieve(Lconno)			
			//	dw_annotrpt_ec_report.print()
			END IF
		END IF
		f_pics_set_def_prn_setting()
		// 12/4/2008 2125 rows changed between updates and retrieve
		dw_final_review.retrieve(Lconno)
		RETURN 1
	ELSE		
		RETURN -1
	END IF
ELSE
	IF sqlservertrans.of_RollBack() = 0 THEN
		Messagebox("Update Error","Update failed! Contact system administrator.")
		RETURN -1
	ELSE		
		RETURN 1
	END IF	
END IF
end event

event pfc_save;integer li_rtn


ib_save = TRUE
li_rtn = Super::Event pfc_save()
RETURN li_rtn





end event

type cb_1 from commandbutton within w_sheet_final_review
integer x = 1938
integer y = 220
integer width = 379
integer height = 92
integer taborder = 90
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "Label Data..."
end type

event clicked;//11/03/2009

string ls_conno
int li_ret 
ls_conno = dw_final_approval.object.conno[1]
IF Isnull(ls_conno) OR Len(Trim(ls_conno)) = 0 THEN
	RETURN
ELSE
	li_ret = f_call_label_app(ls_conno)
END IF

end event

type cb_bknav from commandbutton within w_sheet_final_review
integer x = 1518
integer y = 1720
integer width = 325
integer height = 92
integer taborder = 60
integer textsize = -8
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "Book &Nav."
end type

event clicked;String myconno
myconno = dw_final_review.object.mchar_conno[dw_final_review.GetRow()]

//MessageBox("control number",lconno)
OpenSheetWithParm(w_book_navigation, myconno,w_pics_main, 0, Original!)



end event

type cb_spell from commandbutton within w_sheet_final_review
string tag = "Spell Checker"
integer x = 498
integer y = 1288
integer width = 288
integer height = 96
integer taborder = 80
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "&Spell Check"
end type

event clicked;nca_word lnca_word
String ls_S, loneliner,lttl
	
// Check for any pending updates
IF of_UpdateChecks( ) < 0 THEN Return -1
	
dw_final_review.accepttext()
	
//annotation
ls_S = mle_anno.Text
IF NOT(IsNull(ls_S)) THEN
	lnca_Word.SpellCheck( ls_S )
	mle_anno.Text = ls_S
END IF

//Oneliner
loneliner = dw_final_review.object.ttlinit_oneliner[1]
IF NOT(IsNull(loneliner)) THEN
	lnca_Word.SpellCheck( loneliner )
	dw_final_review.object.ttlinit_oneliner[1] = loneliner
END IF

//Title
lttl = dw_final_review.object.ttlinit_ttl[1]
IF NOT(IsNull(lttl)) THEN
	lnca_Word.SpellCheck( lttl )
	dw_final_review.object.ttlinit_ttl[1] = lttl
END IF
  

end event

type dw_annotrpt_ec_report from u_dw within w_sheet_final_review
event ue_enterkey pbm_dwnprocessenter
event ue_keydown pbm_dwnkey
event pfc_hinttext pbm_mousemove
boolean visible = false
integer x = 9
integer y = 1728
integer width = 50
integer height = 48
integer taborder = 80
string dataobject = "d_annotrpt_ec_report"
end type

event ue_enterkey;//Send(Handle(dw_ri),256,9,Long(0,0))
return(1)
end event

event ue_keydown;//IF key = KeyTab! THEN
//	IF keyflags = 0 THEN
//		IF getcolumn() = 3 THEN
//			RETURN 1
//		END IF
//	ELSEIF keyflags = 1 THEN
//		IF getcolumn() = 2 THEN
//			RETURN 1
//		END IF
//	END IF
//END IF
//
//IF key = KeyDownArrow! THEN
//	dw_final_approval.ScrollToRow(dw_final_approval.GetRow() + 1)
//	dw_final_approval.Setfocus()
//	dw_final_approval.Setcolumn("fr")
//	RETURN 1
//ELSEIF	key = KeyUpArrow! THEN
//	dw_final_approval.ScrollToRow(dw_final_approval.GetRow() - 1)
//	dw_final_approval.Setfocus()
//	dw_final_approval.Setcolumn("fr")
//	RETURN 1
//ELSEIF	key = KeyPagedown! OR key = KeyPageUP! THEN
//	RETURN 1
//END IF
end event

event pfc_hinttext;//string ls_object, ls_column, ls_column_tag
//long ll_pos
//
////This script set's microhelp at the bottom of the screen for the dw_ri 
//ls_object = THIS.getobjectatpointer()
//ll_pos = pos(ls_object, "~t")
//IF NOT pos(ls_object, "_t~t") > 0 THEN
//	IF ll_pos > 0 THEN
//		ll_pos = ll_pos -1
//		ls_column = mid(ls_object,1,ll_pos)
//		ls_column_tag = THIS.Describe(ls_column + ".tag")
//		w_pics_main.setmicrohelp(ls_column_tag)
//	ELSE
//		w_pics_main.setmicrohelp("Ready")
//	END IF
//END IF
end event

event constructor;call super::constructor;this.Settransobject(sqlservertrans)
ib_rmbmenu = FALSE
end event

event getfocus;call super::getfocus;//string ls_cdinit
//
//dw_final_review.Accepttext()
//
//ls_cdinit = dw_final_review.Getitemstring(dw_final_review.Getrow(),"ttlinit_cdinit")
//IF ls_cdinit <> UPPER(sqlservertrans.userid) THEN
//	Messagebox("Required Data Missing","You must supply three letters CDS.")
//	dw_final_review.Scrolltorow(dw_final_review.Getrow())
//	dw_final_review.Setcolumn("ttlinit_cdinit")
//	dw_final_review.Setfocus()	
//	RETURN 1
//END IF
end event

event itemchanged;call super::itemchanged;//string ls_prevbkmed
//  
//
//IF dwo.name = "prevbkmed" THEN
// 	IF data = "" OR ISNULL(data) THEN
//		MessageBox("Warning","If you are removing the RI/RR information, make sure that RICD flag is also cleared.")
////		  IF NOT ISNULL(dw_final_review.object.mchar_ricd[1]) THEN
////		     Messagebox("Required Data Missing","You must enter valid PREVBKMED in this column.")
////		     dw_ri.Setfocus()
////		     dw_ri.Setitem(row,"prevbkmed",data)
////		     RETURN 1
////		END IF
//	END IF
//END IF
//
//		
//	  
//	      
end event

event rbuttondown;//
end event

event retrieveend;call super::retrieveend;//long ll_row
//
//IF NOT rowcount > 0 THEN
//	ll_row = dw_ri.Insertrow(0)
//	dw_ri.Setitem(ll_row,"conno",lconno)
//END IF
//// dw_ri.ResetUpdate()
//dw_ri.Scrolltorow(ll_row)
//
end event

event rowfocuschanged;call super::rowfocuschanged;//string ls_arg,ls_conno
//long ll_row
//
//IF ib_save THEN
//	ls_conno = This.GetItemString(currentrow, "conno")
//	ls_arg = "conno = ~'" + ls_conno + "'"
//	ll_row = dw_final_approval.Find(ls_arg, 0, 1000000)
//	dw_final_approval.ScrollToRow(ll_row)
//	ib_save = FALSE
//END IF
//
//
end event

event sqlpreview;call super::sqlpreview;string ls_syntax

ls_syntax = sqlsyntax
//messagebox("sql",ls_syntax)
end event

event rbuttonup;//
end event

type dw_cdssubject from u_pics_dw within w_sheet_final_review
integer x = 1376
integer y = 1384
integer width = 1463
integer height = 328
integer taborder = 50
string dataobject = "d_cdssubject"
boolean hscrollbar = true
end type

event ue_postconstructor;call super::ue_postconstructor;//long rtncode
////this.of_SetLinkage(TRUE)
//this.of_SetTransObject( SQLServerTrans )
//this.of_SetDropDownSearch(TRUE)
//this.inv_dropdownsearch.of_AddColumn("cdssubject")
//rtncode = this.GetChild('cdssubject', ldwc_cdssub)
//IF rtncode = -1 THEN 
//	MessageBox( "Error", "Not a DataWindowChild")
//END IF
////messagebox('rows',string(rtncode))
//
long rtncode, rtncode2
string Lajyfn
integer rtn

this.of_SetLinkage(TRUE)
this.of_SetTransObject( SQLServerTrans )
this.of_SetDropDownSearch(TRUE)
this.inv_dropdownsearch.of_AddColumn("cdssubject")
this.inv_dropdownsearch.of_AddColumn("cdssubjectcode")
rtncode = this.GetChild('cdssubject', ldwc_cdssub)
rtncode2 = this.GetChild('cdssubjectcode', ldwc_cdssubcode)
IF rtncode = -1 or rtncode2=-1 THEN 
	MessageBox( "Error", "Not a DataWindowChild")
END IF
//messagebox('rows',string(rtncode))


end event

event itemchanged;call super::itemchanged;string lcdssubcode, ls_subject
long li_cur
if dwo.name='cdssubject' then
	ls_subject=data
	lcdssubcode = ldwc_cdssub.GetItemstring(ldwc_cdssub.GetRow(),"pmsub_code")
	//Messagebox("code",lcdssubcode)
	this.object.cdssubjectcode[row] = lcdssubcode
	this.object.chno[row] = lchno
elseif dwo.name='cdssubjectcode' then
	li_cur=ldwc_cdssubcode.GetRow()
	ls_subject=ldwc_cdssubcode.GetItemString(li_cur,'pmsub_desc')
	this.object.cdssubject[row]=ls_subject
	this.object.chno[row] = lchno
end if

end event

event editchanged;call super::editchanged;//messagebox('edit changed event','happed')
string ls_pcode, ls_subject, ls_code
long li_cnt, i,j, li_len, li_cnt2



li_cnt2=ldwc_cdssub.RowCount()
li_cnt=ldwc_cdssubcode.RowCount()
this.SelectRow(0,false)
if dwo.name='cdssubjectcode' then
	ls_pcode=trim(data)
	li_len=len(ls_pcode)
	
	for i=1 to li_cnt
		ls_code=upper(ldwc_cdssubcode.GetItemString(i,'pmsub_code'))
		ls_code=left(ls_code, li_len)
		if ls_code=ls_pcode then
			ls_subject=ldwc_cdssubcode.GetItemString(i,'pmsub_desc')
			this.object.cdssubject[row]= ls_subject
		//	this.SelectRow(0,false)
			//this.SelectRow(row,true )
			exit
		else
			// if the partial code is not found, then blank out desc
			this.object.cdssubject[row]= ''
		end if
	next
end if
		
		
end event

type cb_zoom from commandbutton within w_sheet_final_review
integer x = 1870
integer y = 1720
integer width = 160
integer height = 92
integer taborder = 70
integer textsize = -8
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "&Zoom"
end type

event clicked;// tracker 2125 / 2128 12/2/2008

long ll_yr
string ls_errtext
integer li_ret

li_ret = dw_final_review.AcceptText()

////12/2/2008 li_ret > 0 
IF li_ret > 0  THEN
	Lsexviol = LeftTrim(wf_sex_viol_lang())
	open(w_notepad)
	ib_anno_modified=TRUE
END IF


end event

type cb_anno from commandbutton within w_sheet_final_review
integer x = 1115
integer y = 1720
integer width = 375
integer height = 92
integer textsize = -8
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "Foreign &Anno"
end type

event clicked;String Lchart_no

Lchart_no = dw_final_review.object.ttlinit_chno[dw_final_review.GetRow()]

//IF dw_final_review.object.ttlinit_lang[dw_final_review.GetRow()]='ENG' THEN
//	MessageBox("ERROR","This book is not in foreign language.")
//ELSE
	OpenWithParm(w_anno_foreign, Lchart_no)
//END IF
end event

type mle_sga from multilineedit within w_sheet_final_review
boolean visible = false
integer x = 46
integer y = 988
integer width = 64
integer height = 64
integer taborder = 30
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 33554432
borderstyle borderstyle = stylelowered!
end type

type dw_final_review from u_pics_dw within w_sheet_final_review
event ue_enterkey pbm_dwnprocessenter
event pfc_hinttext pbm_mousemove
event ue_keydown pbm_dwnkey
integer y = 224
integer width = 2848
integer height = 836
integer taborder = 20
string dataobject = "d_final_review"
boolean vscrollbar = false
boolean livescroll = false
end type

event ue_enterkey;call super::ue_enterkey;Send(Handle(THIS),256,9,Long(0,0))
return(1)
end event

event pfc_hinttext;call super::pfc_hinttext;string ls_object, ls_column, ls_column_tag
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

event ue_keydown;call super::ue_keydown;//IF key = KeyTab! THEN
//	IF keyflags = 0  and getcolumn() = 17 THEN
//		dw_ri.ScrolltoRow(dw_ri.Getrow())
//		dw_ri.SetFocus()
//		dw_ri.SetRow(getrow())
//		dw_ri.SetColumn("prevbkmed")
//		RETURN 1
//	END IF
//END IF



end event

event itemchanged;//////////////////////////////////////////////////////////////////////////////////////////////////////////
//
//	Event: itemchanged of dw_final_review
//
//	Description:
//	set field validations
//
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//	
//	Revision History
//
//	Developed by 	Date 				Version						Tracking#
//									
// Murali K.			01/28/2008      005 PICS Modifications	 Reqs: CDS. A.7, CDS A.8a.2
//											Reference to columns share_master_flag replaced with
//											comm_audio_yn, share_master_data replaced with
//											mchar_comm_audio_vendor
//											Reset annotation tag text
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//


Boolean ans
string ls_cdinit,ls_lcno,ls_chno, ls_fn, ls_ln, ls_crname, ls_crnameold,ls_filter
int rtn, li_re, i
long ll_anyr
//DataWindowChild ldwc_cdssubcode,ldwc_casub
DataWindowChild ldwc_casub

SetNull(ll_anyr)
ls_chno = dw_final_review.object.ttlinit_chno[row]

IF dwo.Name = "ttlinit_casub" THEN
	ans =	lnvo_val_anno.of_val_casub_ajyfn(dw_final_review.object.ttlinit_ajyfn[1],data)
	IF ans = FALSE THEN
		MessageBox("AJYFN/CASUB","Invalid ~'AJYFN~' code against ~'CASUB~' code.",StopSign!)
		RETURN 0
	END IF
ELSEIF dwo.Name = "ttlinit_ajyfn" THEN
	string ajyfn
	ajyfn = left(data,2)
	
	 // get handle to child dddws
	dw_cdssubject.GetChild("cdssubject", ldwc_cdssub) 
	dw_cdssubject.GetChild("cdssubjectcode", ldwc_cdssubcode) 
	this.GetChild("ttlinit_casub", ldwc_casub) 

	// Filter the DropDownDatawindow based on the value of the AJYFN
	choose case ajyfn
	case 'AN'
		ls_filter = "AN = "+"~'yes~'"
		ldwc_cdssub.setFilter(ls_filter)
		rtn = ldwc_cdssub.Filter()
		if rtn = 1 then
			ldwc_cdssubcode.setFilter(ls_filter)
			rtn = ldwc_cdssubcode.Filter()
			if rtn = 1 then
				ldwc_casub.setFilter(ls_filter)
				rtn = ldwc_casub.Filter()
				if rtn <> 1 then
					messagebox("error","results of filtering ajyfn failed for casub code")
				end if
			else
				messagebox("error","results of filtering ajyfn failed for CDS subject code")
			end if
		else
			messagebox("error","results of filtering ajyfn failed for CDS subject description")
		end if
	case 'AF'
		ls_filter = "AF = "+"~'yes~'"
		ldwc_cdssub.setFilter(ls_filter)
		rtn = ldwc_cdssub.Filter()
		if rtn = 1 then
			ldwc_cdssubcode.setFilter(ls_filter)
			rtn = ldwc_cdssubcode.Filter()
			if rtn = 1 then
				ldwc_casub.setFilter(ls_filter)
				rtn = ldwc_casub.Filter()
				if rtn <> 1 then
					messagebox("error","results of filtering ajyfn failed for casub code")
				end if
			else
				messagebox("error","results of filtering ajyfn failed for CDS subject code")
			end if
		else
			messagebox("error","results of filtering ajyfn failed for CDS subject description")
		end if
	case 'YF'
		ls_filter = "YF = "+"~'yes~'"
		ldwc_cdssub.setFilter(ls_filter)
		rtn = ldwc_cdssub.Filter()
		if rtn = 1 then
			ldwc_cdssubcode.setFilter(ls_filter)
			rtn = ldwc_cdssubcode.Filter()
			if rtn = 1 then
				ldwc_casub.setFilter(ls_filter)
				rtn = ldwc_casub.Filter()
				if rtn <> 1 then
					messagebox("error","results of filtering ajyfn failed for casub code")
				end if
			else
				messagebox("error","results of filtering ajyfn failed for CDS subject code")
			end if
		else
			messagebox("error","results of filtering ajyfn failed for CDS subject description")
		end if
	case 'YN'
		ls_filter = "YN = "+"~'yes~'"
		ldwc_cdssub.setFilter(ls_filter)
		rtn = ldwc_cdssub.Filter()
		if rtn = 1 then
			ldwc_cdssubcode.setFilter(ls_filter)
			rtn = ldwc_cdssubcode.Filter()
			if rtn = 1 then
				ldwc_casub.setFilter(ls_filter)
				rtn = ldwc_casub.Filter()
				if rtn <> 1 then
					messagebox("error","results of filtering ajyfn failed for casub code")
				end if
			else
				messagebox("error","results of filtering ajyfn failed for CDS subject code")
			end if
		else
			messagebox("error","results of filtering ajyfn failed for CDS subject description")
		end if
	case 'JF'
		ls_filter = "JF = "+"~'yes~'"
		ldwc_cdssub.setFilter(ls_filter)
		rtn = ldwc_cdssub.Filter()
		if rtn = 1 then
			ldwc_cdssubcode.setFilter(ls_filter)
			rtn = ldwc_cdssubcode.Filter()
			if rtn = 1 then
				ldwc_casub.setFilter(ls_filter)
				rtn = ldwc_casub.Filter()
				if rtn <> 1 then
					messagebox("error","results of filtering ajyfn failed for casub code")
				end if
			else
				messagebox("error","results of filtering ajyfn failed for CDS subject code")
			end if
		else
			messagebox("error","results of filtering ajyfn failed for CDS subject description")
		end if
	case 'JN'
		ls_filter = "JN = "+"~'yes~'"
		ldwc_cdssub.setFilter(ls_filter)
		rtn = ldwc_cdssub.Filter()
		if rtn = 1 then
			ldwc_cdssubcode.setFilter(ls_filter)
			rtn = ldwc_cdssubcode.Filter()
			if rtn = 1 then
				ldwc_casub.setFilter(ls_filter)
				rtn = ldwc_casub.Filter()
				if rtn <> 1 then
					messagebox("error","results of filtering ajyfn failed for casub code")
				end if
			else
				messagebox("error","results of filtering ajyfn failed for CDS subject code")
			end if
		else
			messagebox("error","results of filtering ajyfn failed for CDS subject description")
		end if
	case else
		/*None of the above AJYFN */
	end choose
	
	//2/16/2010 #2202 length of pmsub desc increased to 40 for the main datawindow to prevent validation error
	ldwc_cdssubcode.setsort('pmsub_code a')
	ldwc_cdssubcode.sort()
	
	ldwc_cdssub.setsort('pmsub_code a')
	ldwc_cdssub.sort()
	
	IF ajyfn <> 'AF' AND ajyfn <> 'AN' AND ajyfn <> 'JF' AND ajyfn <> 'JN' AND &
		ajyfn <> 'YF' AND ajyfn <> 'YN' THEN 
		RETURN 1
	ELSE
		IF dw_final_review.object.ttlinit_casub[1] <> "" THEN
			ans =	lnvo_val_anno.of_val_casub_ajyfn(data,dw_final_review.object.ttlinit_casub[1])
			IF ans = FALSE THEN
				MessageBox("AJYFN?CASUB","Invalid ~'AJYFN~' code against ~'CASUB~' code.",StopSign!)
				RETURN 0
			END IF
		END IF
	END IF
	
ELSEIF dwo.name = "ttlinit_lcno" THEN //Must enter eight digit number
	    ls_lcno = dw_final_review.Getitemstring(row,"ttlinit_lcno")
	    IF len(data) <> 8 THEN
		    dw_final_review.Setfocus()
		    dw_final_review.Setitem(row,"ttlinit_lcno",ls_lcno)
		    RETURN 1
	    END IF
		 
// 01/28/2008 changes
ELSEIF DWO.Name = "mchar_comm_audio_yn" THEN
	IF Data = 'N' THEN
		IF NOT(IsNull(this.object.mchar_comm_audio_vendor[row])) OR (this.object.mchar_comm_audio_vendor[row] <> "") THEN
		  this.object.mchar_comm_audio_vendor[row] = ""
		END IF
		This.object.mchar_comm_audio_vendor.visible=FALSE
		wf_sex_viol_lang() // Reset annotation remove commercial audio indicator
	ELSE
		This.object.mchar_comm_audio_vendor.visible=TRUE
		wf_sex_viol_lang() // Reset annotation remove commercial audio indicator
	END IF
///////////////


ELSEIF dwo.name = "ttlinit_oneliner" THEN
		string ls_oneliner
		ls_oneliner = dw_final_review.object.ttlinit_oneliner[1]
	    IF ISNULL(data) OR TRIM(data) = "" THEN
			dw_final_review.Setfocus()
			dw_final_review.object.ttlinit_oneliner[row] = ls_oneliner
		    RETURN 1
	    END IF
   //Checking for required column ANYR data entered or not
ELSEIF dwo.name = "ttlinit_anyr" THEN
		IF long(data) > Year(today()) THEN
		   RETURN 1
	   END IF
	//Checking for required column PMSUB1 data entered or not
ELSEIF dwo.name = "ttlinit_pmsub1" THEN
		string ls_pmsub
	    ls_pmsub = dw_final_review.Getitemstring(row,"ttlinit_pmsub1")
	    IF ISNULL(data) OR TRIM(data) = "" THEN
		    dw_final_review.Setfocus()
		    dw_final_review.Setitem(row,"ttlinit_pmsub1",ls_pmsub)
		    RETURN 1
	    END IF
ELSEIF dwo.Name = "ttlinit_aepcd" THEN
	string aepcd, ls_null
	SetNull(ls_null )
	aepcd = left(data,1)
	IF aepcd <> 'A' AND aepcd <> 'C' AND aepcd <> 'E' AND aepcd <> 'P' AND &
		aepcd <> 'S' AND aepcd <> 'N' AND aepcd <> 'T' AND aepcd <> 'R' AND &
		aepcd <> 'I' AND aepcd <> 'L' THEN
		RETURN 1
	END IF
	IF aepcd = 'A' THEN
		dw_final_review.Object.ttlinit_auth.Edit.Required='Yes'
		dw_final_review.Object.ttlinit_authfn.Edit.Required='Yes'
	ELSEIF aepcd = 'L' THEN
		dw_final_review.object.ttlinit_authfn[1] = ls_null
		dw_final_review.object.ttlinit_ahonorific[1] = ""
		dw_final_review.Object.ttlinit_auth.Edit.Required='Yes'
		dw_final_review.Object.ttlinit_authfn.Edit.Required='No'	
	ELSE
		dw_final_review.Object.ttlinit_auth.Edit.Required='No'
		dw_final_review.Object.ttlinit_authfn.Edit.Required='No'
	END IF
ELSEIF dwo.Name = "ttlinit_gradecd" THEN
	IF wf_check_anno_flag(ls_chno) THEN
//		this.object.ttlinit_gradecd.ValidationMsg="Please remove SGA (system generated annotation) from the annotation. ~r~nBefore your changes takes place on grade, click on zoom botton and then uncheck the ~'Y~' checkbox at the bottom of the screen."
//		RETURN 1
		rtn = MessageBox("Annotation Tags","Annotation tags already exist, Do you want to alter them?",Question!,YesNo!,1)
		IF rtn = 1 THEN
          	anno_tags_altered = TRUE
		ELSE
			RETURN 1
		END IF
	END IF
ELSEIF dwo.Name = "ttlinit_lang" THEN
	IF wf_check_anno_flag(ls_chno) AND data <> 'ENG' THEN
//		MessageBox("Warning","Please remove SGA (system generated annotation) from the annotation. ~r~nBefore your changes on language, click on zoom botton and then uncheck the box at the bottom of the screen.")
//		RETURN 1
		rtn = MessageBox("Annotation Tags","Annotation tags already exist, Do you want to alter them?",Question!,YesNo!,1)
		IF rtn = 1 THEN
          	anno_tags_altered = TRUE
		ELSE
			RETURN 1
		END IF
	END IF
ELSEIF dwo.name = 'ttlinit_sexcd' THEN
	IF wf_check_anno_flag(ls_chno) THEN
//		this.object.ttlinit_sexcd.ValidationMsg="Please remove SGA (system generated annotation) from the annotation. ~r~nBefore your changes takes place on sex field, click on zoom botton and then uncheck the ~'Y~' checkbox at the bottom of the screen."
//		RETURN 1
		rtn = MessageBox("Annotation Tags","Annotation tags already exist, Do you want to alter them?",Question!,YesNo!,1)
		IF rtn = 1 THEN
          	anno_tags_altered = TRUE
		ELSE
			RETURN 1
		END IF
	END IF
ELSEIF dwo.name = 'ttlinit_violencecd' THEN
	IF wf_check_anno_flag(ls_chno) THEN
//		this.object.ttlinit_violencecd.ValidationMsg="Please remove SGA (system generated annotation) from the annotation. ~r~nBefore your changes takes place on violance, click on zoom botton and then uncheck the ~'Y~' checkbox at the bottom of the screen."
//		RETURN 1
		rtn = MessageBox("Annotation Tags","Annotation tags already exist, Do you want to alter them?",Question!,YesNo!,1)
		IF rtn = 1 THEN
          	anno_tags_altered = TRUE
		ELSE
			RETURN 1
		END IF
	END IF
ELSEIF dwo.name = 'ttlinit_stronglang' THEN
	IF wf_check_anno_flag(ls_chno) THEN
//		this.object.ttlinit_stronglang.ValidationMsg="Please remove SGA (system generated annotation) from the annotation. ~r~nBefore your changes takes place on strong language, click on zoom botton and then uncheck the ~'Y~' checkbox at the bottom of the screen."
//		RETURN 1
		rtn = MessageBox("Annotation Tags","Annotation tags already exist, Do you want to alter them?",Question!,YesNo!,1)
		IF rtn = 1 THEN
          	anno_tags_altered = TRUE
		ELSE
			RETURN 1
		END IF
	END IF
ELSEIF dwo.name = 'ttlinit_prize' THEN
	IF wf_check_anno_flag(ls_chno) THEN
//		this.object.ttlinit_prize.ValidationMsg="Please remove SGA (system generated annotation) from the annotation. ~r~nBefore your changes takes place on prize, click on zoom botton and then uncheck the ~'Y~' checkbox at the bottom of the screen."
//		RETURN 1
		rtn = MessageBox("Annotation Tags","Annotation tags already exist, Do you want to alter them?",Question!,YesNo!,1)
		IF rtn = 1 THEN
          	anno_tags_altered = TRUE
		ELSE
			RETURN 1
		END IF
	END IF
ELSEIF dwo.name = "ttlinit_bestseller" THEN			   
	IF wf_check_anno_flag(ls_chno) THEN
//		this.object.ttlinit_bestseller.ValidationMsg="Please remove SGA (system generated annotation) from the annotation. ~r~nBefore your changes takes place on best seller, click on zoom botton and then uncheck the ~'Y~' checkbox at the bottom of the screen."
//		RETURN 1
//	ELSEIF data <> 'Y' AND data <> 'N' THEN
//		RETURN 1
		rtn = MessageBox("Annotation Tags","Annotation tags already exist, Do you want to alter them?",Question!,YesNo!,1)
		IF rtn = 1 THEN
          	anno_tags_altered = TRUE
		ELSE
			RETURN 1
		END IF
	END IF
//
ELSEIF dwo.name = "ttlinit_isbn" THEN
	
	String ls_isbn, ls_char 
	Int isbn_length
	
	ls_isbn =Data
	ls_isbn =Trim(ls_isbn)
	
	isbn_length = Len(ls_isbn)
	
	IF isbn_length >13 THEN
		this.object.ttlinit_isbn.Validationmsg ='ISBN # MUST BE AT MOST 13 CHARACTERS'
		RETURN 1
	ELSEIF isbn_length < 10 THEN
		this.object.ttlinit_isbn.Validationmsg ='ISBN # MUST BE AT LEAST 10 CHARACTERS'
		RETURN 1
	END IF
	
	FOR i=1 TO isbn_length - 1
		ls_char =Mid(ls_isbn,i,1)
		IF  (ls_char> '9' OR ls_char<'0') THEN
			this.object.ttlinit_isbn.Validationmsg ='You must use digit except the last character' 
			RETURN 1
		END IF
	NEXT
	
	ls_char =Mid(ls_isbn,isbn_length,1)
	IF  (ls_char >'Z' OR ls_char <'A') AND (ls_char >'9' OR ls_char< '0')THEN
		this.object.ttlinit_isbn.Validationmsg ='The last character should be in set (A...Z) '+ &
		' or in the set (1...9)'
		RETURN 1
	END IF
END IF
//
ls_crnameold=dw_copyright.object.ttlinit_crname[1]
if ls_crnameold<>'Public Domain' then
	if dwo.name='ttlinit_auth' then
		li_re=messagebox('','Author last name changed, Do you want copyright name to be changed'+&
			'~nbecause of this change?',Exclamation!,YesNo!,1)
		if li_re=1 then
			ls_fn=dw_final_review.object.ttlinit_authfn[1]
			ls_crname=f_combine_string1_string2(ls_fn, data)
			dw_copyright.object.ttlinit_crname[1]=ls_crname
		end if
	elseif dwo.name='ttlinit_authfn' then
		li_re=messagebox('','Author first name changed, Do you want copyright name to be changed'+&
			'~nbecause of this change?',Exclamation!,YesNo!,1)
		if li_re=1 then
			ls_ln=dw_final_review.object.ttlinit_auth[1]
			ls_crname=f_combine_string1_string2(data, ls_ln)
			dw_copyright.object.ttlinit_crname[1]=ls_crname
		end if
	end if
end if
	
	
	
	
	

	
	
	
	

end event

event rbuttondown;//
end event

event rbuttonup;//
end event

event constructor;call super::constructor;//////////////////////////////////////////////////////////////////////////////////////////////////////////
//
//	Event: constructor of dw_final_review
//
//	Description:
//	set field validations
//
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//	
//	Revision History
//
//	Developed by 	Date 				Version						Tracking#
//									
// Murali K.			01/28/2008      005 PICS Modifications	 Reqs: CDS. A.7, CDS A.8a.2
//											Reference to columns share_master_flag replaced with
//											comm_audio_yn, share_master_data replaced with
//											mchar_comm_audio_vendor
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//


String ls_ttlinitkey[ ], ls_ttlinitcols[ ], ls_mcharkey[ ], ls_mcharcols[ ]


//Setting the transaction object for finalreview datawindow
dw_final_review.of_SetTransObject( sqlservertrans )
//Setting the multitable service on
This.of_SetMultiTable(TRUE)
//Selecting the key and updateable columns from ttlinit table and mchar table
ls_ttlinitkey[ ] = {"ttlinit_chno"}
ls_ttlinitcols[ ] =  {"ttlinit_chno", "ttlinit_aepcd", "ttlinit_auth", "ttlinit_authfn", "ttlinit_ahonorific", &
		"ttlinit_ttlart", "ttlinit_ttl", "ttlinit_ajyfn","ttlinit_anyr", "ttlinit_lcno", "ttlinit_b_ttl1", "ttlinit_b_ttl2", &
		"ttlinit_b_auth", "ttlinit_casub", "ttlinit_note", "ttlinit_oneliner", "ttlinit_annoinit", "ttlinit_pminit", &
		"ttlinit_cdinit", "ttlinit_pmsub1", "ttlinit_pmsub2", "ttlinit_pmsub3", "ttlinit_specattn", "ttlinit_seqnote", &
		"ttlinit_serttl", "ttlinit_cycle","ttlinit_bestseller","ttlinit_sexcd","ttlinit_violencecd","ttlinit_stronglang", &
		"ttlinit_gradecd","ttlinit_prize"}
ls_mcharkey[ ] = {"mchar_conno"}

//01/28/2008 remove references to shared master flag and shared master data replaced with comm audio cols
ls_mcharcols[ ] =  {"mchar_conno", "mchar_ricd","mchar_frinit","mchar_comm_audio_yn","mchar_comm_audio_vendor"}

This.inv_multitable.of_AddToUpdate("ttlinit", ls_ttlinitkey, ls_ttlinitcols)
This.inv_multitable.of_AddToUpdate("mchar", ls_mcharkey, ls_mcharcols)



end event

event sqlpreview;call super::sqlpreview;//MessageBox("SQL",sqlsyntax)
end event

event getfocus;call super::getfocus;m_pics_main.m_edit.m_deleterow.Enabled 	=	FALSE
m_pics_main.m_edit.m_addrow.Enabled 		=	FALSE
m_pics_main.m_edit.m_cut.Enabled 		=	FALSE




end event

event retrievestart;call super::retrievestart;openwithparm(w_pics_retrieve_msg_box,"Retrieving Final Review Information, Please Wait...")
end event

event retrieveend;call super::retrieveend;string ls_attn,titletxt,ls_bseller
integer ll_row

n_cst_string 	inv_string
ll_row = dw_final_review.GetRow()

IF ll_row > 0 THEN

	ls_attn = dw_final_review.object.ttlinit_specattn[ll_row]
		
	IF ISNULL(ls_attn) OR TRIM(ls_attn) = "" THEN
		dw_final_review.object.ttlinit_specattn[ll_row]="N"
	END IF
		
	ls_bseller = dw_final_review.object.ttlinit_bestseller[ll_row]
		
	IF ISNULL(ls_bseller) OR TRIM(ls_bseller) = "" THEN
		dw_final_review.object.ttlinit_bestseller[ll_row]="N"
	END IF
		
	dw_final_review.Object.ttlinit_bestseller.EditMask.Required='No'
		
	// Get the title and trim out the extra trailing spaces.
	titletxt =  inv_string.of_Trim(dw_final_review.object.ttlinit_ttl[1])
	dw_final_review.object.ttlinit_ttl[1] = titletxt
	//messagebox("title",titletxt+" "+string(len(titletxt)))
	// dw_final_review.ResetUpdate()
END IF

close(w_pics_retrieve_msg_box)

end event

event pfc_preupdate;call super::pfc_preupdate;//////////////////////////////////////////////////////////////////////////////////////////////////////////
//
//	Event: pfc_preupdate
// 
//	Description:
//	Validate commercial audio vendor selection
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//	
//	Revision History
//
//	Developed by 	Date 				Version						Tracking#
//									
// Murali K.			09/30/2008       PICS 2.0 Modifications	 2090
// Murali K			11/19/2008 mandatory column validations Tracker 2122
//////////////////////////////////////////////////////////////////////////////////////////////////////////

string ls_comm_audio, ls_vendor
boolean lb_error=FALSE
string  ls_lang, ls_aepcd, ls_pmsub1, ls_oneliner, ls_errtext
long ll_yr

// 09/30/2008
ls_comm_audio = this.object.mchar_comm_audio_yn[1]
ls_vendor = this.object.mchar_comm_audio_vendor[1] 
IF ls_comm_audio = 'Y' and (Isnull(ls_vendor) OR lEN(TRIM(LS_VENDOR)) = 0 ) THEN
	MessageBOX('Error', 'Please select a commercial audio vendor.')
	ib_preupdatefail = TRUE
	RETURN -1
END IF

//11/19/2008 mandatory column validations Tracker 2122


ll_yr  = this.object.ttlinit_anyr[1] // year written

IF Isnull(ll_yr)  OR ll_yr = 0 THEN
	lb_error=TRUE
	ls_errtext +=' Year Written must be entered.' + '~r~n'
END IF


RETURN 1
///////////////
end event

type dw_coauthors from u_pics_dw within w_sheet_final_review
event ue_enterkey pbm_dwnprocessenter
event ue_keydown pbm_dwnkey
integer y = 1068
integer width = 1367
integer height = 208
integer taborder = 60
string dataobject = "d_final_coauth"
boolean hscrollbar = true
end type

event ue_enterkey;call super::ue_enterkey;IF (dw_coauthors.Rowcount() <> 0 OR dw_coauthors.Rowcount() = 0) THEN
	Send(Handle(this),256,9,Long(0,0))
   return(1)
END IF
end event

event ue_keydown;call super::ue_keydown;IF key = KeyTab! THEN
	IF keyflags = 0 THEN
		IF getcolumn() = 3 THEN
//			dw_final_approval.ScrollToRow(dw_final_approval.GetRow() + 1)
//			RETURN 1
		END IF
	ELSEIF keyflags = 1 THEN
		IF getcolumn() = 1 THEN
			RETURN 1
		END IF
	END IF
END IF

IF key = KeyDownArrow! THEN
	dw_final_approval.ScrollToRow(dw_final_approval.GetRow() + 1)
	dw_final_approval.Setfocus()
	dw_final_approval.Setcolumn("fr")
	RETURN 1
ELSEIF	key = KeyUpArrow! THEN
	dw_final_approval.ScrollToRow(dw_final_approval.GetRow() - 1)
	dw_final_approval.Setfocus()
	dw_final_approval.Setcolumn("fr")
	RETURN 1
ELSEIF	key = KeyPagedown! OR key = KeyPageUP! THEN
	RETURN 1
END IF
end event

event pfc_addrow;long ll_rc

ll_rc = Super::Event pfc_addrow()
dw_coauthors.SetItem(ll_rc, "chno", Lchno)
return ll_rc
end event

event getfocus;call super::getfocus;string ls_crname

m_pics_main.m_edit.m_addrow.Enabled 		=	TRUE
m_pics_main.m_edit.m_deleterow.Enabled 	=	TRUE





end event

event rbuttondown;//
end event

event rbuttonup;//
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

dw_coauthors.SetItemStatus(1, 0, Primary!, DataModified!)

return li_rc
end event

event sqlpreview;call super::sqlpreview;string ls_syntax

ls_syntax = sqlsyntax
end event

event rowfocuschanged;call super::rowfocuschanged;string ls_arg, ls_chno
long ll_row

IF ib_save THEN
	ls_chno = dw_coauthors.GetItemString(dw_coauthors.getrow(), "chno")
	ls_arg = "chno = ~'" + ls_chno + "'"
	ll_row = dw_final_approval.Find(ls_arg, 0, 1000000)
	dw_final_approval.ScrollToRow(ll_row)
	ib_save = FALSE
END IF
end event

event itemchanged;call super::itemchanged;m_pics_main.m_edit.m_addrow.Enabled 		=	TRUE
m_pics_main.m_edit.m_deleterow.Enabled 	=	TRUE


end event

event constructor;call super::constructor;dw_coauthors.of_SetTransObject( SQLServerTrans )
ib_rmbmenu = TRUE

end event

event retrieveend;call super::retrieveend;long ll_row

IF NOT rowcount > 0 THEN
//	ll_row = dw_coauthors.InsertRow(0)
	dw_coauthors.SetItem(ll_row, "chno", Lchno)
END IF
// dw_coauthors.ResetUpdate()
dw_coauthors.ScrollToRow(ll_row)

end event

event ue_postconstructor;call super::ue_postconstructor;dw_coauthors.of_SetLinkage(TRUE)
dw_coauthors.of_SetTransObject( sqlservertrans )

end event

type dw_copyright from u_dw within w_sheet_final_review
event ue_enterkey pbm_dwnprocessenter
event pfc_hinttext pbm_mousemove
event ue_keydown pbm_dwnkey
integer x = 1390
integer y = 1068
integer width = 1467
integer height = 316
integer taborder = 50
string dataobject = "d_copyright"
boolean vscrollbar = false
boolean livescroll = false
end type

event ue_enterkey;call super::ue_enterkey;Send(Handle(this),256,9,Long(0,0))
return(1)
end event

event pfc_hinttext;call super::pfc_hinttext;string ls_object, ls_column, ls_column_tag
long ll_pos

//This script set's microhelp at the bottom of the screen for the dw_copyright 
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

event ue_keydown;call super::ue_keydown;IF key = KeyTab! THEN
	IF keyflags = 0 THEN
		IF getcolumn() = 3 THEN
			RETURN 1
		END IF
	ELSEIF keyflags = 1 THEN
		IF getcolumn() = 1 THEN
			RETURN 1
		END IF
	END IF
END IF

IF key = KeyDownArrow! THEN
	dw_final_approval.ScrollToRow(dw_final_approval.GetRow() + 1)
	dw_final_approval.Setfocus()
	dw_final_approval.Setcolumn("fr")
	RETURN 1
ELSEIF	key = KeyUpArrow! THEN
	dw_final_approval.ScrollToRow(dw_final_approval.GetRow() - 1)
	dw_final_approval.Setfocus()
	dw_final_approval.Setcolumn("fr")	
	RETURN 1
ELSEIF	key = KeyPagedown! OR key = KeyPageUP! THEN
	RETURN 1
END IF
end event

event getfocus;call super::getfocus;m_pics_main.m_edit.m_deleterow.Enabled 	=	FALSE
m_pics_main.m_edit.m_addrow.Enabled 		=	FALSE
m_pics_main.m_edit.m_cut.Enabled 		=	FALSE
end event

event constructor;call super::constructor;dw_copyright.of_SetTransObject( sqlservertrans )
ib_rmbmenu = FALSE

end event

event sqlpreview;call super::sqlpreview;string ls_syntax

ls_syntax = sqlsyntax
end event

event rowfocuschanged;call super::rowfocuschanged;string ls_arg, ls_chno
long ll_row

IF ib_save THEN
	ls_chno = This.GetItemString(currentrow, "ttlinit_chno")
	ls_arg = "chno = ~'" + ls_chno + "'"
	ll_row = dw_final_approval.Find(ls_arg, 0, 1000000)
	dw_final_approval.ScrollToRow(ll_row)
	ib_save = FALSE
END IF
end event

event retrieveend;call super::retrieveend;long ll_row
	
IF NOT rowcount > 0 THEN
	ll_row = dw_copyright.InsertRow(0)
	dw_copyright.SetItem(ll_row, "ttlinit_chno", "lchno")
END IF
// dw_copyright.ResetUpdate()
dw_copyright.ScrollToRow(ll_row)
end event

event losefocus;call super::losefocus;string ls_flags
integer ls_date

dw_copyright.Accepttext()

SetNull(ls_date)


//Checking for year validation between 1752 and current year if crflag is "Y"
IF (dw_copyright.object.ttlinit_crflag[1]) = "Y" THEN
   ls_date = dw_copyright.object.ttlinit_cryr[1]
   IF IsNull(ls_date) OR ls_date < 1752 OR ls_date > Year(today()) THEN
	   Messagebox("Copyright Year","Please enter a year between 1752 and the current year.")
	   dw_copyright.Setfocus()
	   dw_copyright.Setcolumn("ttlinit_cryr")
	   RETURN 1
   END IF
END IF
 




end event

type mle_anno from u_mle within w_sheet_final_review
integer y = 1392
integer width = 805
integer height = 320
integer taborder = 0
boolean vscrollbar = true
boolean displayonly = true
end type

event losefocus;call super::losefocus;ii_mle = 0
end event

event getfocus;call super::getfocus;ii_mle = 1
end event

type st_2 from u_st within w_sheet_final_review
integer x = 137
integer y = 1296
integer width = 343
integer weight = 700
boolean underline = true
long backcolor = 78164112
string text = "Annotation"
alignment alignment = center!
end type

type st_1 from u_st within w_sheet_final_review
integer x = 37
integer y = 1708
integer width = 224
integer height = 104
integer weight = 700
fontcharset fontcharset = ansi!
long backcolor = 79741120
string text = "Rows Counted"
alignment alignment = center!
long bordercolor = 16711935
end type

type sle_rowcount from u_sle within w_sheet_final_review
integer x = 261
integer y = 1720
integer width = 119
integer height = 84
integer taborder = 0
integer textsize = -10
integer weight = 700
long textcolor = 255
boolean displayonly = true
end type

type dw_ri from u_dw within w_sheet_final_review
event ue_enterkey pbm_dwnprocessenter
event ue_keydown pbm_dwnkey
event pfc_hinttext pbm_mousemove
integer x = 805
integer y = 1284
integer width = 562
integer height = 420
integer taborder = 40
string dataobject = "d_ri"
boolean hscrollbar = true
end type

event ue_enterkey;call super::ue_enterkey;Send(Handle(dw_ri),256,9,Long(0,0))
return(1)
end event

event ue_keydown;call super::ue_keydown;IF key = KeyTab! THEN
	IF keyflags = 0 THEN
		IF getcolumn() = 3 THEN
			RETURN 1
		END IF
	ELSEIF keyflags = 1 THEN
		IF getcolumn() = 2 THEN
			RETURN 1
		END IF
	END IF
END IF

IF key = KeyDownArrow! THEN
	dw_final_approval.ScrollToRow(dw_final_approval.GetRow() + 1)
	dw_final_approval.Setfocus()
	dw_final_approval.Setcolumn("fr")
	RETURN 1
ELSEIF	key = KeyUpArrow! THEN
	dw_final_approval.ScrollToRow(dw_final_approval.GetRow() - 1)
	dw_final_approval.Setfocus()
	dw_final_approval.Setcolumn("fr")
	RETURN 1
ELSEIF	key = KeyPagedown! OR key = KeyPageUP! THEN
	RETURN 1
END IF
end event

event pfc_hinttext;call super::pfc_hinttext;string ls_object, ls_column, ls_column_tag
long ll_pos

//This script set's microhelp at the bottom of the screen for the dw_ri 
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

event constructor;call super::constructor;dw_ri.Settransobject(sqlservertrans)
//ib_rmbmenu = FALSE
end event

event retrieveend;call super::retrieveend;long ll_row

IF NOT rowcount > 0 THEN
	ll_row = dw_ri.Insertrow(0)
	dw_ri.Setitem(ll_row,"conno",lconno)
END IF
// dw_ri.ResetUpdate()
dw_ri.Scrolltorow(ll_row)

end event

event rowfocuschanged;call super::rowfocuschanged;string ls_arg,ls_conno
long ll_row

IF ib_save THEN
	ls_conno = This.GetItemString(currentrow, "conno")
	ls_arg = "conno = ~'" + ls_conno + "'"
	ll_row = dw_final_approval.Find(ls_arg, 0, 1000000)
	dw_final_approval.ScrollToRow(ll_row)
	ib_save = FALSE
END IF


end event

event sqlpreview;call super::sqlpreview;string ls_syntax

ls_syntax = sqlsyntax
//messagebox("sql",ls_syntax)
end event

event itemchanged;call super::itemchanged;string ls_prevbkmed
  

IF dwo.name = "prevbkmed" THEN
 	IF data = "" OR ISNULL(data) THEN
		MessageBox("Warning","If you are removing the RI/RR information, make sure that RICD flag is also cleared.")
	END IF
ELSEIF dwo.name = "mchar_ricd" THEN
	dw_final_review.object.mchar_ricd[dw_final_review.GetRow()] = data
END IF

		
	  
	      
end event

event getfocus;call super::getfocus;//string ls_cdinit
//
//dw_final_review.Accepttext()
//
//ls_cdinit = dw_final_review.Getitemstring(dw_final_review.Getrow(),"ttlinit_cdinit")
//IF ls_cdinit <> UPPER(sqlservertrans.userid) THEN
//	Messagebox("Required Data Missing","You must supply three letters CDS.")
//	dw_final_review.Scrolltorow(dw_final_review.Getrow())
//	dw_final_review.Setcolumn("ttlinit_cdinit")
//	dw_final_review.Setfocus()	
//	RETURN 1
//END IF
end event

event pfc_addrow;call super::pfc_addrow;long	ll_rc
boolean lb_disablelinkage
string ls_conno

ls_conno = dw_final_approval.object.conno[1]

// Allow for pre functionality.
if this.Event pfc_preinsertrow() <= 0 then return NO_ACTION

// Is Querymode enabled?
if IsValid(inv_QueryMode) then lb_disablelinkage = inv_QueryMode.of_GetEnabled()

if not lb_disablelinkage then
	// Notify that a new row is about to be added.
	if IsValid ( inv_Linkage ) then inv_Linkage.Event pfc_InsertRow (0) 
end if

// Insert row.
if IsValid (inv_RowManager) then
	ll_rc = inv_RowManager.event pfc_addrow ()
else
	ll_rc = this.InsertRow (0) 
end if

this.object.conno[ll_rc]=ls_conno

if not lb_disablelinkage then
	// Notify that a new row has been added.
	if IsValid ( inv_Linkage ) then inv_Linkage.Event pfc_InsertRow (ll_rc) 
end if

// Allow for post functionality.
this.Post Event pfc_postinsertrow(ll_rc)

return ll_rc
end event

event pfc_deleterow;call super::pfc_deleterow;dw_final_review.object.mchar_ricd[dw_final_review.GetRow()] = ""
RETURN 1
end event

type st_currentrow from u_st within w_sheet_final_review
integer x = 384
integer y = 1712
integer width = 242
integer height = 100
integer weight = 700
long backcolor = 79741120
string text = "Current Record"
alignment alignment = center!
end type

type sle_currentrow from u_sle within w_sheet_final_review
integer x = 640
integer y = 1720
integer width = 119
integer height = 84
integer taborder = 0
integer textsize = -10
integer weight = 700
long textcolor = 255
boolean displayonly = true
end type

type cb_find from u_cb within w_sheet_final_review
event pfc_hinttext pbm_mousemove
string tag = "Find~'s the record"
integer x = 2057
integer y = 1720
integer width = 151
integer taborder = 0
fontcharset fontcharset = ansi!
string text = "&Find"
boolean default = true
end type

event pfc_hinttext;call super::pfc_hinttext;w_pics_main.setmicrohelp(This.tag)
end event

event clicked;call super::clicked;long ll_rows,Lbkseq,rtn,li_bkseq=0
string ls_conno,ls_frinit,titletxt,landigcd, ls_chno,rc,ls_bkmed
datetime ld_date,ld_pcsstdt
date ld_date_d
string select_clause,where_clause,mod_string,lajyfn,ls_filter,lpcsstdt_flg
//DataWindowChild ldwc_cdssubcode,ldwc_casub
DataWindowChild ldwc_casub

int totrecs

SetNull(ls_frinit)
SetNull(ld_pcsstdt)
SetNull(Lbkseq)
		
dw_final_approval.Accepttext()

ls_conno = dw_final_approval.object.conno[1]
ld_date = dw_final_approval.object.fr[1]
li_bkseq = dw_final_approval.object.bkseq[1]
ls_bkmed = dw_final_approval.object.bkmed[1]


// If the control number is null and book number is zero then check the final review date
IF (ls_conno = "" OR ISNULL(ls_conno)) AND li_bkseq = 0 THEN
	ld_date_d=date(ld_date)
	IF IsNull(ld_date) OR NOT ISDATE(string(ld_date_d))  THEN
	   Messagebox("Error","Please enter either a valid control number or a valid final review date to retrieve the records.",STOPSIGN!)
	   dw_final_approval.Setfocus()
	   dw_final_approval.Setcolumn("conno")
	   RETURN
	END IF
END IF


// If the control number is not blank
IF ls_conno <> "" THEN	
	// If the book number exists, disable the media update.
	Select bkseq,frinit,pcsstdt into :Lbkseq,:ls_frinit,:ld_pcsstdt from mchar where conno = :ls_conno using sqlservertrans;
	IF IsNull(Lbkseq) THEN
//		dw_final_approval.Modify("med.Tabsequence = 30")
		// 04/30/2008 keep it readonly as changes to med on final review is not very straight forward
		dw_final_approval.Modify("med.Tabsequence = 0")
	ELSE
		dw_final_approval.Modify("med.Tabsequence = 0")
	END IF	
	
	
	dw_final_approval.Object.Datawindow.Table.Select = is_sql
	
	select_clause=	is_sql
	where_clause = " WHERE conno=~'" + ls_conno + "~'"
	mod_string 	 = "DataWindow.Table.Select=~"" + select_clause + where_clause + "~""
	rc = dw_final_approval.Modify(mod_string)
	IF rc = "" THEN
		// retrieve the data.
		SetNull(ld_date)
		dw_final_approval.Reset()
		ll_rows = dw_final_approval.Retrieve()
		IF ll_rows < 1 THEN
			MessageBox("Control Number", "Control Number: "+ls_conno +" does not exist. ~nEnter the correct Control Number." ,Information!, OK!, 2)
			dw_final_approval.insertrow(0)
			dw_final_approval.SetFocus()
			dw_final_approval.Setcolumn("conno")
			RETURN
		ELSE
			SetNull(lpcsstdt_flg)
			
			ld_date = dw_final_approval.object.fr[1]
			lpcsstdt_flg = dw_final_approval.object.pcsstdt_flg[1]
//			landigcd = dw_final_approval.object.andigcd[1]
			
			// If the format is null assign it to either.
//			IF IsNull(landigcd) OR landigcd="" THEN
//				dw_final_approval.object.andigcd[1] = "M"
//			END IF
			
			//If the pcsstdt is NULL and pcsstdt_flg is not set to "N", assign it to the same value as fr date.
			IF IsNull(ld_pcsstdt) AND lpcsstdt_flg<>"N" THEN
				dw_final_approval.object.pcsstdt[1] = ld_date
			END IF
			//Calculate the total number of record assigned this final review
			ld_date_d=date(ld_date)
			IF string(ld_date_d)<>"" OR NOT(IsNULL(ld_date)) THEN
				select count(*) into :totrecs from mchar where fr=:ld_date using sqlservertrans;
			ELSE
				ld_date = dw_final_approval.object.fr[1]
				select count(*) into :totrecs from mchar where fr=:ld_date using sqlservertrans;
			END IF
			
			wf_get_rest_of_data()
			
		
			sle_totcount.text = string(totrecs)
			wf_enable_buttons()
			// set the focus on final approval datawindow.
			dw_final_approval.SetFocus()
			dw_final_approval.Setcolumn("fr")		
		END IF
	ELSE
		Messagebox("FR Modify","Modify string failed.")		
	END IF
// If the book number is not blank
ELSEIF li_bkseq <> 0  THEN	
	IF (ls_bkmed = "" OR ISNULL(ls_bkmed)) THEN
		MessageBox("Book Media", "Please enter the book media." ,Information!, OK!, 2)
		dw_final_approval.SetFocus()
		dw_final_approval.Setcolumn("bkmed")
		RETURN
	END IF
	// If the book number exists, disable the media update.
	Select conno,frinit,pcsstdt into :ls_conno,:ls_frinit,:ld_pcsstdt from mchar where bkseq = :li_bkseq and bkmed = :ls_bkmed using sqlservertrans;
	
	dw_final_approval.Object.Datawindow.Table.Select = is_sql
	
	select_clause=	is_sql
	where_clause = " WHERE bkseq = "+string(li_bkseq)+" AND bkmed=~'" + ls_bkmed + "~'"
	mod_string 	 = "DataWindow.Table.Select=~"" + select_clause + where_clause + "~""
	rc = dw_final_approval.Modify(mod_string)
	IF rc = "" THEN
		// retrieve the data.
		SetNull(ld_date)
		dw_final_approval.Reset()
		ll_rows = dw_final_approval.Retrieve()
		IF ll_rows < 1 THEN
			MessageBox("Control Number", "Book Number: "+ls_bkmed+string(li_bkseq) +" does not exist. ~nEnter the correct Book Number." ,Information!, OK!, 2)
			dw_final_approval.insertrow(0)
			dw_final_approval.SetFocus()
			dw_final_approval.Setcolumn("bkseq")
			RETURN
		ELSE
			
			SetNull(lpcsstdt_flg)
			
			ld_date = dw_final_approval.object.fr[1]
			lpcsstdt_flg = dw_final_approval.object.pcsstdt_flg[1]
			
			//If the pcsstdt is NULL and pcsstdt_flg is not set to "N", assign it to the same value as fr date.
			IF IsNull(ld_pcsstdt) AND lpcsstdt_flg<>"N" THEN
				dw_final_approval.object.pcsstdt[1] = ld_date
			END IF
			//Calculate the total number of record assigned this final review
			ld_date_d=date(ld_date)
			IF string(ld_date_d)<>"" OR NOT(IsNULL(ld_date)) THEN
				select count(*) into :totrecs from mchar where fr=:ld_date using sqlservertrans;
			ELSE
				ld_date = dw_final_approval.object.fr[1]
				select count(*) into :totrecs from mchar where fr=:ld_date using sqlservertrans;
			END IF
			
			wf_get_rest_of_data()
				
			sle_totcount.text = string(totrecs)
			wf_enable_buttons()
			// set the focus on final approval datawindow.
			dw_final_approval.SetFocus()
			dw_final_approval.Setcolumn("fr")		
		END IF
	ELSE
		Messagebox("FR Modify","Modify string failed.")		
	END IF
ELSE
	ld_date_d=date(ld_date)
	// If the finial review initial exists, disable the media update.
	Select frinit into :ls_frinit from mchar where fr = to_date(:ld_date_d,'DD-MMM-YYYY') using sqlservertrans;
	IF IsNull(ls_frinit) OR ls_frinit=""  THEN
//		dw_final_approval.Modify("med.Tabsequence = 30")
		// 04/24/2008
		dw_final_approval.Modify("med.Tabsequence = 0")
	ELSE
		dw_final_approval.Modify("med.Tabsequence = 0")
	END IF
	
	dw_final_approval.Object.Datawindow.Table.Select = is_sql
	
	select_clause=	is_sql
	where_clause = " WHERE mchar.fr = "+"~'"+string(ld_date_d,'DD-MMM-YYYY')+"~'"
	mod_string 	 = "DataWindow.Table.Select=~"" + select_clause + where_clause + "~""
	rc = dw_final_approval.Modify(mod_string)
	IF rc = "" THEN
		ll_rows = dw_final_approval.Retrieve()
		IF ll_rows < 1 THEN
			Messagebox("FR Date","Please enter correct date.")
			dw_final_approval.insertrow(0)
			dw_final_approval.Setfocus()
			dw_final_approval.Setcolumn("fr")
			RETURN
		END IF
		select count(*) into :totrecs 
		from mchar 
		where fr = :ld_date_d
		using sqlservertrans;
		sle_totcount.text = string(totrecs)
		wf_enable_buttons()
		// set the focus on final approval datawindow.
		dw_final_approval.SetFocus()
		dw_final_approval.Setcolumn("fr")
	ELSE
		Messagebox("FR Modify","Modify string failed.")		
	END IF
END IF
ib_found = TRUE
dw_final_approval.Event rowfocuschanged(1)
ib_disablecloseQuery = TRUE

// This part of code was commented because there
// was no need to reretrieve these DWs. 1/22/03 MC

select chno into :ls_chno
from mchar
where conno=:ls_conno
using sqlserverTrans;
//
dw_cdssubject.SetTransobject(sqlserverTrans)
//dw_coauthors.SetTransobject(sqlserverTrans)
//dw_copyright.SetTransobject(sqlserverTrans)
//dw_final_review.SetTransobject(sqlserverTrans)
//if isnull(ls_chno)=false then
	dw_cdssubject.Retrieve(ls_chno)
//else
//	dw_cdssubject.InsertRow(0)
//end if
//dw_coauthors.Retrieve(ls_conno)
//dw_copyright.Retrieve(ls_conno)
//dw_final_review.Retrieve(ls_conno)
long li_cnt
//string ls_chno
dwitemstatus ldwstatus
li_cnt=dw_coauthors.RowCount()
if li_cnt=1 then
	ls_chno=dw_coauthors.object.chno[1]
	if ls_chno='' or IsNull(ls_chno) then
		ldwstatus=dw_coauthors.getItemstatus(1,0,Primary!)
		if ldwstatus<>New! then
			dw_coauthors.SetItemStatus(1,0, Primary!, New!)
		end if
	end if
end if


// Capture the AJYFN for CDS Subject codes retrival datawindow
Lajyfn  = dw_final_review.object.ttlinit_ajyfn[1]

 // get handle to child dddws
dw_cdssubject.GetChild("cdssubject", ldwc_cdssub) 
dw_cdssubject.GetChild("cdssubjectcode", ldwc_cdssubcode) 
dw_final_review.GetChild("ttlinit_casub", ldwc_casub) 

// Filter the DropDownDatawindow based on the value of the AJYFN
choose case Lajyfn
case 'AN'
	ls_filter = "AN = "+"~'yes~'"
	ldwc_cdssub.setFilter(ls_filter)
	rtn = ldwc_cdssub.Filter()
	if rtn = 1 then
		ldwc_cdssubcode.setFilter(ls_filter)
		rtn = ldwc_cdssubcode.Filter()
		if rtn = 1 then
			ldwc_casub.setFilter(ls_filter)
			rtn = ldwc_casub.Filter()
			if rtn <> 1 then
				messagebox("error","results of filtering ajyfn failed for casub code")
			end if
		else
			messagebox("error","results of filtering ajyfn failed for CDS subject code")
		end if
	else
		messagebox("error","results of filtering ajyfn failed for CDS subject description")
	end if
case 'AF'
	ls_filter = "AF = "+"~'yes~'"
	ldwc_cdssub.setFilter(ls_filter)
	rtn = ldwc_cdssub.Filter()
	if rtn = 1 then
		ldwc_cdssubcode.setFilter(ls_filter)
		rtn = ldwc_cdssubcode.Filter()
		if rtn = 1 then
			ldwc_casub.setFilter(ls_filter)
			rtn = ldwc_casub.Filter()
			if rtn <> 1 then
				messagebox("error","results of filtering ajyfn failed for casub code")
			end if
		else
			messagebox("error","results of filtering ajyfn failed for CDS subject code")
		end if
	else
		messagebox("error","results of filtering ajyfn failed for CDS subject description")
	end if
case 'YF'
	ls_filter = "YF = "+"~'yes~'"
	ldwc_cdssub.setFilter(ls_filter)
	rtn = ldwc_cdssub.Filter()
	if rtn = 1 then
		ldwc_cdssubcode.setFilter(ls_filter)
		rtn = ldwc_cdssubcode.Filter()
		if rtn = 1 then
			ldwc_casub.setFilter(ls_filter)
			rtn = ldwc_casub.Filter()
			if rtn <> 1 then
				messagebox("error","results of filtering ajyfn failed for casub code")
			end if
		else
			messagebox("error","results of filtering ajyfn failed for CDS subject code")
		end if
	else
		messagebox("error","results of filtering ajyfn failed for CDS subject description")
	end if
case 'YN'
	ls_filter = "YN = "+"~'yes~'"
	ldwc_cdssub.setFilter(ls_filter)
	rtn = ldwc_cdssub.Filter()
	if rtn = 1 then
		ldwc_cdssubcode.setFilter(ls_filter)
		rtn = ldwc_cdssubcode.Filter()
		if rtn = 1 then
			ldwc_casub.setFilter(ls_filter)
			rtn = ldwc_casub.Filter()
			if rtn <> 1 then
				messagebox("error","results of filtering ajyfn failed for casub code")
			end if
		else
			messagebox("error","results of filtering ajyfn failed for CDS subject code")
		end if
	else
		messagebox("error","results of filtering ajyfn failed for CDS subject description")
	end if
case 'JF'
	ls_filter = "JF = "+"~'yes~'"
	ldwc_cdssub.setFilter(ls_filter)
	rtn = ldwc_cdssub.Filter()
	if rtn = 1 then
		ldwc_cdssubcode.setFilter(ls_filter)
		rtn = ldwc_cdssubcode.Filter()
		if rtn = 1 then
			ldwc_casub.setFilter(ls_filter)
			rtn = ldwc_casub.Filter()
			if rtn <> 1 then
				messagebox("error","results of filtering ajyfn failed for casub code")
			end if
		else
			messagebox("error","results of filtering ajyfn failed for CDS subject code")
		end if
	else
		messagebox("error","results of filtering ajyfn failed for CDS subject description")
	end if
case 'JN'
	ls_filter = "JN = "+"~'yes~'"
	ldwc_cdssub.setFilter(ls_filter)
	rtn = ldwc_cdssub.Filter()
	if rtn = 1 then
		ldwc_cdssubcode.setFilter(ls_filter)
		rtn = ldwc_cdssubcode.Filter()
		if rtn = 1 then
			ldwc_casub.setFilter(ls_filter)
			rtn = ldwc_casub.Filter()
			if rtn <> 1 then
				messagebox("error","results of filtering ajyfn failed for casub code")
			end if
		else
			messagebox("error","results of filtering ajyfn failed for CDS subject code")
		end if
	else
		messagebox("error","results of filtering ajyfn failed for CDS subject description")
	end if
case else
	/*None of the above AJYFN */
end choose

//11/03/2009
wf_get_label_data(ls_conno)












end event

type cb_update from u_cb within w_sheet_final_review
event type integer ue_save ( )
event pfc_hinttext pbm_mousemove
string tag = "Update the record"
integer x = 2226
integer y = 1720
integer width = 224
integer taborder = 0
fontcharset fontcharset = ansi!
string text = "&Update"
end type

event type integer ue_save();//////////////////////////////////////////////////////////////////////////////////////////////////////////
//
//	Event: ue_save of  cb_update
//
//	Description:
//	Save data
//
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//	
//	Revision History
//
//	Developed by 	Date 				Version						Tracking#
//									
// Murali K.			01/29/2008      005 PICS Modifications	 Reqs: CDS. A.7, CDS A.8a.2
// 											If the book is in production and flash indicator is checked
//											update DB Record only with final review date.
// 											old update for other_conno commented replaced by updating only DB record 
//											if the flash indicator is on
//Murali K. 			04/23/2008 		Update both RTB records with the final review and flag 
//											and flag if flash indicator is not checked
//Murali K.			05/23/2008		Tracker# 2075 - FR date wrong update issue
// Murali K.			12/3/2008		Tracker 2125/2128 commer audio and year written issues
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//

string ls_flag,ls_flags,ls_coauth,ricode,ls_crname,ls_number,ls_crflag, &
			ls_auth, ls_sauth, ls_ttl, ls_sttl, ls_soundex, lcontno,ls_authfn,ls_soundex_auth,ls_parprt,ls_pcsstdt_flg,ls_other_conno,ls_bkmed,ls_frflag
Boolean ans
integer ls_date,li_rtn,ls_yearw,li_count, rtn
long i,ll_rows,ll_prevbkseq,cur_row, lcnt, li_cnt
datetime lpmedt, ld_pcsstdt
date tday
string lannoflg, ls_pub, ls_isbn, ls_coauthfn,  ls_chno, ls_mcharchno, ls_conno
n_cst_string		lnv_string
long ll_bkseq
string ls_flash
date ld_fr


tday = Today()

SetNull(ll_prevbkseq)

// Accept the text that was put on the screen.
dw_final_approval.Accepttext()
dw_final_review.AcceptText()
dw_ri.Accepttext()
dw_copyright.Accepttext()
dw_coauthors.AcceptText()
ls_isbn=dw_final_review.object.ttlinit_isbn[1]
ls_pub=dw_final_review.object.ttlinit_publisher[1]
cur_row = dw_final_review.Getrow()

//Checking for pmedt date
lpmedt = dw_final_review.object.ttlinit_pmedt[cur_row]
IF ISNULL(lpmedt) THEN
	Messagebox("ERROR","PMS Edit Annotation date must be entered by PMS staff.")
	RETURN -1
END IF

//Checking for Lcnumber
ls_number = dw_final_review.object.ttlinit_lcno[cur_row]
IF ISNULL(ls_number) OR TRIM(ls_number) = "" THEN
	Messagebox("LC Card Number","You must enter a LC Card Number.")
	dw_final_review.Setfocus()
	dw_final_review.Setcolumn("ttlinit_lcno")
	RETURN -1
END IF

//Checking for Year written
ls_yearw = dw_final_review.object.ttlinit_anyr[cur_row]
IF ISNULL(ls_yearw) OR ls_yearw = 0 THEN
	Messagebox("Year Written","You must enter the year written (between 1752 and current year).")
	dw_final_review.Setfocus()
	dw_final_review.Setcolumn("ttlinit_anyr")
	RETURN -1
END IF

//Checking for validation for prevbkseq
IF NOT ISNULL(dw_final_review.object.mchar_ricd[1]) THEN
	IF dw_ri.rowcount() > 0 THEN
		IF ISNULL(dw_ri.object.prevbkseq[1]) THEN	
			Messagebox("Previous Book Number","You must enter a previous book number.")
			dw_ri.setfocus()
			dw_ri.setcolumn("prevbkseq")
			RETURN -1
		END IF
	END IF
 END IF
 
//Checking validation for prevbkmed
IF dw_ri.rowcount() > 0 THEN
	ll_prevbkseq = dw_ri.object.prevbkseq[1]
	IF ISNULL(dw_ri.object.prevbkmed[1]) AND (ll_prevbkseq > 0)  THEN
		Messagebox("Previous Book Medium","You must enter a previous book medium.")
		dw_ri.Setfocus()
		dw_ri.Setcolumn("prevbkmed")
		RETURN -1
	END IF
END IF
//Checking whether user entered reissue or rerecord
string ls_ricd		
ls_ricd = dw_final_review.object.mchar_ricd[1]
IF ISNULL(Trim(ls_ricd)) OR Trim(ls_ricd) = "" THEN
	IF dw_ri.rowcount() > 0 THEN
		IF NOT ISNULL(dw_ri.object.prevbkmed[1]) OR NOT ISNULL(ll_prevbkseq) THEN
			Messagebox("RICD","Please select Reissue/Rerecord before pressing Update.")
			dw_final_review.setfocus()
			dw_final_review.Setcolumn("mchar_ricd")
			RETURN -1
		END IF
	END IF
END IF


//Checking for year validation between 1752 and current year if crflag is "Y"
IF TRIM(dw_copyright.object.ttlinit_crflag[1]) = "Y" THEN
   ls_date = dw_copyright.object.ttlinit_cryr[1]
   IF IsNull(ls_date) OR ls_date < 1752 OR ls_date > Year(today()) THEN
	   Messagebox("Copyright Year","Please enter the copyright year (between 1752 and current year).")
	   dw_copyright.Setfocus()
	   dw_copyright.Setcolumn("ttlinit_cryr")
	   RETURN -1
   END IF
END IF
// Checking for crname required column
ls_crname = Trim(dw_copyright.object.ttlinit_crname[1])
IF ISNULL(ls_crname) OR Trim(ls_crname) = "" THEN
	ls_crflag = dw_copyright.object.ttlinit_crflag[1]
	IF TRIM(ls_crflag) = 'Y' THEN
	   Messagebox("CR Label","You must enter the Copyright Label.")	
	   dw_copyright.Setfocus()
	   dw_copyright.Setcolumn("ttlinit_crname")
	   RETURN -1
	END IF
END IF

// Checking for empty row has been added by the user in dw_coauthors, 
// if there is any delete those empty rows and update.
//ll_rows = dw_coauthors.Rowcount()
//FOR i = ll_rows TO 1 STEP -1
//	ls_coauth = trim(dw_coauthors.object.coauth[i])
//	IF ls_coauth = "" OR ISNULL(ls_coauth) THEN
//		dw_coauthors.Deleterow(i)
//	END IF
//NEXT

string ls_comm_audio, ls_vendor

// 09/30/2008
ls_comm_audio = dw_final_review.object.mchar_comm_audio_yn[1]
ls_vendor = dw_final_review.object.mchar_comm_audio_vendor[1] 
IF ls_comm_audio = 'Y' and (Isnull(ls_vendor) OR lEN(TRIM(LS_VENDOR)) = 0 ) THEN
	MessageBOX('Error', 'Please select a commercial audio vendor.')
//	ib_preupdatefail = TRUE
	RETURN -1
END IF

//2203 2/18/2010 validate wrong cds subject codes
dw_cdssubject.accepttext()
IF wf_validate_cdssubject() = -1 THEN
	Messagebox('Error','Please enter a valid CDS Subject code.')
	dw_cdssubject.setfocus()
	RETURN -1
END IF


ls_auth=trim(dw_final_review.object.ttlinit_auth[1])
ls_authfn=trim(dw_final_review.object.ttlinit_authfn[1])
if Isnull( ls_auth) then ls_auth=""
if IsNull( ls_authfn ) then ls_authfn=""
ls_soundex_auth=f_soundex(ls_auth + ls_authfn,'auth' )

ls_ttl=dw_final_review.object.ttlinit_ttl[1]
ls_other_conno = dw_final_approval.object.other_media_conno[1]
ls_pcsstdt_flg = dw_final_approval.object.pcsstdt_flg[1]
ld_pcsstdt = dw_final_approval.object.pcsstdt[1]
ls_bkmed = dw_final_approval.object.bkmed[1]
ls_frflag = dw_final_approval.object.frflag[1]

ls_mcharchno = dw_final_approval.object.chno[1] // 01/14/2010 #2205
ls_conno = dw_final_approval.object.conno[1] // 01/14/2010 #2205

ls_sauth = f_create_sttl(ls_auth)
ls_sttl = f_create_sttl(ls_ttl)
ls_soundex = f_soundex(ls_ttl,'ttl')
dw_final_review.object.ttlinit_soundex_ttl[1] =ls_soundex
dw_final_approval.object.fr[1] = date(dw_final_approval.object.fr[1])
//dw_final_review.object.ttlinit_sauth[1] =ls_sauth
//dw_final_review.object.ttlinit_sttl[1] =ls_sttl
dw_final_review.AcceptText()
li_cnt=dw_coauthors.RowCount()
if li_cnt=1 then
	ls_chno=dw_coauthors.object.chno[1]
	if Isnull(ls_chno) then
		ls_coauth=dw_coauthors.object.coauth[1]
		ls_coauthfn=dw_coauthors.object.coauthfn[1]
		if (isNull(ls_coauth)=false and ls_coauth<>'') or (isNull(ls_coauthfn)=false and ls_coauthfn<>'') then
			dw_coauthors.object.chno[1]=lchno
		end if
	end if
end if
dw_coauthors.AcceptText()
li_rtn = parent.Event pfc_save() 
IF li_rtn = 1 OR li_rtn = 0 THEN	


	/////////// 01/29/2008 following update replaced by updating only DB record if the flash indicator is on
//	// If the book is RC or DB make sure you update the final review flags and PCS date flags
//	IF NOT(IsNull(ls_other_conno)) AND ls_bkmed <> 'BR' THEN
//		UPDATE MCHAR
//		SET pcsstdt = :ld_pcsstdt, pcsstdt_flg = :ls_pcsstdt_flg, frflag = :ls_frflag, fr = :tday
//		WHERE conno = :ls_other_conno
//		USING SqlServerTrans;
//		IF f_check_dberror(sqlservertrans,"MCHAR_other_conno") THEN
//			COMMIT USING SqlServerTrans;
//		ELSE 
//			ROLLBACK USING SqlServerTrans;
//			RETURN -1
//		END IF
//		// Mark the MCHAR Table
//		f_update_mchar_time(ls_other_conno,0,"C","U")
//	END IF

		///////////////////////////////////////////////////////////////////////////////////////// 01/29/2008
		// If the book is in production and flash indicator is checked update DB Record only with
		// Final review date.
		
		ll_bkseq = dw_final_approval.object.bkseq[1]
		ls_flash = dw_final_approval.object.flash_indicator[1]
		
		// Tracker #2075 changes 05/23/2008
		ld_fr = date(dw_final_approval.object.fr[1] )
		IF Isnull(ld_fr) THEN
		//	ld_fr = today()
		ELSE 
			 dw_final_approval.object.frflag[1] = 'Y'
			 ls_frflag='Y'
		END IF
		/////

		// 01/14/2010 #2205 first pass update sibling records
		IF ib_firsttime THEN
			UPDATE MCHAR
				SET pcsstdt = :ld_pcsstdt, pcsstdt_flg = :ls_pcsstdt_flg, frflag = :ls_frflag, fr = :ld_fr
			WHERE chno = :ls_mcharchno and conno <> :ls_conno and frflag is null
				USING SqlServerTrans;
				IF f_check_dberror(sqlservertrans,"MCHAR_chno") THEN
					COMMIT USING SqlServerTrans;
				ELSE 
					ROLLBACK USING SqlServerTrans;
					RETURN -1
				END IF
				// Mark the MCHAR Table
				f_update_mchar_time(ls_mcharchno,0,"C","U")
		END IF
		///// 01/14/2010
		
//		IF ll_bkseq > 0 AND ls_flash='Y' THEN
//			// 01/08/2010
//			IF ls_bkmed = 'DB' THEN
//				dw_final_approval.object.fr[1] = ld_fr
//				
//				UPDATE MCHAR
//				SET fr = :ld_fr
//				WHERE bkseq = :ll_bkseq 
//				and bkmed = 'DB'
//				USING SqlServerTrans;
//				IF f_check_dberror(sqlservertrans,"MCHAR_fr_date update") THEN
//					COMMIT USING SqlServerTrans;
//				ELSE 
//					ROLLBACK USING SqlServerTrans;
//					RETURN -1
//				END IF
//			END IF // 01/08/2010 #2205
//			ELSE
//				// 04/23/2008 update both RTB records with the final review and flag and flag if flash indicator is not checked
//				IF NOT(IsNull(ls_other_conno)) AND ls_bkmed <> 'BR' THEN
//					
//					// 05/23/2008 Tracker #2075 FR wrong update with current date issue
//					select fr
//					into :ld_fr
//					from mchar
//					where conno = :ls_other_conno using sqlservertrans ;
//					
//					IF Isnull(ld_fr) THEN
//						ld_fr = date(dw_final_approval.object.fr[1] )
//						IF Isnull(ld_fr) THEN
//							ld_fr = Today()
//						END IF
//					END IF
//					///////////// #2075 05/23/2008
//					
//					UPDATE MCHAR
//					SET pcsstdt = :ld_pcsstdt, pcsstdt_flg = :ls_pcsstdt_flg, frflag = :ls_frflag, fr = :ld_fr
//					WHERE conno = :ls_other_conno
//					USING SqlServerTrans;
//					IF f_check_dberror(sqlservertrans,"MCHAR_other_conno") THEN
//						COMMIT USING SqlServerTrans;
//					ELSE 
//						ROLLBACK USING SqlServerTrans;
//						RETURN -1
//					END IF
//					// Mark the MCHAR Table
//					f_update_mchar_time(ls_other_conno,0,"C","U")
//				END IF
//		END IF
		////////// 01/29/2008
		
    	UPDATE ttlinit  
	 SET sttl = :ls_sttl ,sauth= :ls_sauth, soundex_ttl= :ls_soundex,&
	 		soundex_auth=:ls_soundex_auth, isbn=:ls_isbn, publisher=:ls_pub
	WHERE chno = :Lchno
	USING SqlServerTrans;
	IF f_check_dberror(sqlservertrans,"ttlinit_STTL") THEN
		COMMIT USING SqlServerTrans;
	ELSE 
		ROLLBACK USING SqlServerTrans;
		RETURN -1
	END IF
	IF dw_final_review.AcceptText() = 1 THEN
					// Get the Chart Number
		Lchno=dw_final_review.object.ttlinit_chno[1]
	END IF
	lcontno = dw_final_review.object.mchar_conno[1]
	f_update_mchar_time(lcontno,0,"C","U")
//	IF Lchno="" OR IsNull(Lchno) THEN
//		Lchno = Local_chno
//	END IF
		// See if the short title exist in database
	Select count(*) into :Lcnt from sttltbl
		where chno=:Lchno
	using sqlservertrans;
				
	// If No rows exist in sttltbl 
	IF Lcnt = 0 THEN
	// Short title does not exist(Insert)
		INSERT INTO sttltbl ( chno,sttl )
		 VALUES ( :Lchno,:ls_sttl)
		USING SQLServerTrans;
		IF f_check_dberror(sqlservertrans,"STTL") THEN
			COMMIT USING SqlServerTrans;
		ELSE 
			ROLLBACK USING SqlServerTrans;
		END IF
	ELSE
	// Short title does exist(Update)
		UPDATE sttltbl  
		 SET sttl = :ls_sttl 
		WHERE chno = :Lchno
		USING SqlServerTrans;
		IF f_check_dberror(sqlservertrans,"STTL") THEN
			COMMIT USING SqlServerTrans;
		ELSE 
			ROLLBACK USING SqlServerTrans;
		END IF
	END IF	
	
	//COMMIT USING SqlServerTrans;
	ib_save = FALSE
	dw_final_approval.Setfocus()
	dw_final_approval.Setcolumn("fr")
	IB_FIRSTTIME=FALSE // 01/14/2010 #2205 reset flag
END IF

	
//Select parprt 
//into :ls_parprt
//from mchar
//where conno = :lcontno
//using sqlservertrans;
//				
//IF f_check_dberror(sqlservertrans,"MCHAR") THEN
//// If parprt is already set to 'P'(printed par) assign it to 'C'(changed par).
//	IF ls_parprt = 'P' THEN
//		f_set_parprt(Lconno,'C')
//	END IF
//END IF
//
//// Check to see if the SGA(system generated annotation) has been appended to the core annotation.
//select prop_added_flg 
//into :lannoflg 
//from annotation 
//where chno = :Lchno 
//using sqlservertrans;
//
//// If not append it with no confirmation
//If lannoflg<>'Y' THEN
//	String Lanno,Lsga
//	
//	SELECT annotation.anno_property  
//    INTO :Lsga  
//    FROM annotation  
//   WHERE annotation.chno = :Lchno   
//   USING SQLServerTrans;
//	IF f_check_dberror(sqlservertrans,"Select Annotation")=FALSE THEN
//		ROLLBACK USING SqlServerTrans;
//	END IF
//	Lanno=mle_anno.text
//	// First remove the newline characters from annotation.
//	Lanno = lnv_string.of_GlobalReplace(Lanno, '~r~n', " ")
//	// use the dummy MLE to populate the SGA
//	mle_sga.text=Lsga
//	
//	IF IsNull(Lsga)=FALSE AND IsNull(Lanno)=FALSE THEN
//		// Append the SGA to annotation
//		Lanno = Lanno+" "+Lsga
//		// Reassign the annotation field
//		mle_anno.text = Lanno
//		// Assign it to a variable
//		Lanno=TRIM(mle_anno.text)
//		// Update annotation table
//		UPDATE annotation
//		 SET anno = :Lanno,prop_added_flg = 'Y'
//		WHERE chno = :Lchno
//		USING SqlServerTrans;
//		IF f_check_dberror(sqlservertrans,"Update Annotation") THEN
//			COMMIT USING SqlServerTrans;
//			anno_tags_altered=FALSE
//		ELSE 
//			ROLLBACK USING SqlServerTrans;
//		END IF
//	END IF
//END IF
//



RETURN li_rtn




	

end event

event clicked;call super::clicked;cb_update.Triggerevent("ue_save")
end event

type cb_clear from u_cb within w_sheet_final_review
event pfc_hinttext pbm_mousemove
string tag = "Clear the screen"
integer x = 2478
integer y = 1720
integer width = 183
integer taborder = 0
fontcharset fontcharset = ansi!
string text = "&Clear"
end type

event pfc_hinttext;call super::pfc_hinttext;w_pics_main.setmicrohelp(This.tag)
end event

event clicked;call super::clicked;integer li_rtn

ib_disableclosequery = TRUE
//IF ib_found THEN	
//	IF of_updatechecks() = 1 OR of_updatechecks() = -3 THEN 
//		li_rtn=MessageBox("Clear","Save the changes before clearing the screen?",Question!,YESNO!,1)
//		IF li_rtn = 1 THEN
//			//li_rtn = Parent.Event pfc_save()
//			li_rtn = cb_update.Event ue_save()
//			IF NOT li_rtn = 1 AND NOT li_rtn = 0 THEN				
//			   RETURN
//			END IF
//		END IF
//	END IF
//END IF

ib_save = FALSE
ib_found = FALSE		

dw_final_approval.Reset()
dw_final_approval.Event pfc_addrow()
dw_final_review.Reset()
dw_ri.Reset()
dw_copyright.Reset()
dw_coauthors.Reset()
dw_cdssubject.Reset()
wf_disable_buttons()
mle_anno.text=""
sle_rowcount.Text = ""
sle_currentrow.Text = ""
sle_totcount.Text = ""
ib_disablecloseQuery = FALSE

wf_clear_label_fields(FALSE)
end event

type cb_exit from u_cb within w_sheet_final_review
event pfc_hinttext pbm_mousemove
string tag = "Exits from the current window"
integer x = 2674
integer y = 1720
integer width = 165
integer taborder = 0
fontcharset fontcharset = ansi!
string text = "E&xit"
end type

event pfc_hinttext;call super::pfc_hinttext;w_pics_main.setmicrohelp(This.tag)
end event

event clicked;call super::clicked;IF ISNULL(sle_rowcount.Text) OR sle_rowcount.Text = "" THEN
	ib_disableclosequery = TRUE
END IF
parent.Event pfc_close()
//m_pics_main.m_menu.PopMenu(300, 0)

end event

type dw_finalreview_report from u_dw within w_sheet_final_review
boolean visible = false
integer x = 910
integer y = 1220
integer width = 0
integer height = 52
integer taborder = 0
string dataobject = "d_cdfrlist_ace"
boolean vscrollbar = false
boolean border = false
boolean livescroll = false
borderstyle borderstyle = stylebox!
end type

event constructor;call super::constructor;This.Settransobject(sqlservertrans)
This.of_setupdateable(FALSE)
end event

event pfc_retrieve;call super::pfc_retrieve;datetime ls_date

ls_date = dw_final_approval.object.fr[1]
RETURN dw_finalreview_report.Retrieve(ls_date)
end event

event retrieveend;call super::retrieveend;dw_finalreview_report.Triggerevent("pfc_print")
end event

type st_3 from statictext within w_sheet_final_review
integer x = 763
integer y = 1708
integer width = 201
integer height = 104
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long backcolor = 79741120
boolean enabled = false
string text = "Total Record"
alignment alignment = center!
boolean focusrectangle = false
end type

type sle_totcount from u_sle within w_sheet_final_review
integer x = 974
integer y = 1720
integer width = 119
integer height = 84
integer taborder = 0
integer textsize = -10
integer weight = 700
long textcolor = 255
boolean displayonly = true
end type

type st_4 from statictext within w_sheet_final_review
integer x = 96
integer y = 240
integer width = 439
integer height = 52
boolean bringtotop = true
integer textsize = -8
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 33554432
long backcolor = 67108864
string text = "Label Data Filled In:"
boolean focusrectangle = false
end type

type cbx_1 from checkbox within w_sheet_final_review
integer x = 549
integer y = 240
integer width = 69
integer height = 72
boolean bringtotop = true
integer textsize = -8
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 33554432
long backcolor = 67108864
boolean enabled = false
boolean automatic = false
boolean thirdstate = true
end type

type st_5 from statictext within w_sheet_final_review
integer x = 663
integer y = 240
integer width = 498
integer height = 64
boolean bringtotop = true
integer textsize = -8
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 33554432
long backcolor = 67108864
string text = "Print Label Approved:"
boolean focusrectangle = false
end type

type cbx_2 from checkbox within w_sheet_final_review
integer x = 1170
integer y = 236
integer width = 69
integer height = 72
boolean bringtotop = true
integer textsize = -8
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 33554432
long backcolor = 67108864
boolean enabled = false
boolean automatic = false
boolean thirdstate = true
end type

type st_6 from statictext within w_sheet_final_review
integer x = 1312
integer y = 240
integer width = 517
integer height = 68
boolean bringtotop = true
integer textsize = -8
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 33554432
long backcolor = 67108864
string text = "Braille Label Approved:"
boolean focusrectangle = false
end type

type cbx_3 from checkbox within w_sheet_final_review
integer x = 1851
integer y = 236
integer width = 69
integer height = 72
boolean bringtotop = true
integer textsize = -8
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 33554432
long backcolor = 67108864
boolean enabled = false
boolean automatic = false
boolean thirdstate = true
end type

type dw_final_approval from u_dw within w_sheet_final_review
event ue_enterkey pbm_dwnprocessenter
event pfc_hinttext pbm_mousemove
event ue_keydown pbm_dwnkey
integer width = 2843
integer height = 228
integer taborder = 10
string dataobject = "d_final_approval"
boolean vscrollbar = false
boolean livescroll = false
end type

event ue_enterkey;call super::ue_enterkey;Send(Handle(this),256,9,Long(0,0))
return(1)
end event

event ue_keydown;call super::ue_keydown;IF key = KeyTab! THEN
	IF keyflags = 0 THEN
		IF getcolumn() = 4 THEN
//			dw_final_review.SetFocus()
//			dw_final_review.SetRow(getrow())
//			dw_final_review.SetColumn(1)
			RETURN 1
		END IF
	END IF
END IF
end event

event constructor;call super::constructor;int li_rtn


li_rtn = dw_final_approval.of_SetLinkage(TRUE)
dw_final_approval.of_SetTransObject( sqlservertrans )
ib_rmbmenu = FALSE

li_rtn = dw_final_approval.of_SetRowManager(TRUE)
li_rtn = dw_final_review.of_SetRowManager(TRUE)
li_rtn = dw_copyright.of_SetRowManager(TRUE)
li_rtn = dw_coauthors.of_SetRowManager(TRUE)
li_rtn = dw_ri.of_SetRowManager(TRUE)
li_rtn = dw_cdssubject.of_SetRowManager(TRUE)


li_rtn = dw_final_review.of_SetLinkage(TRUE)
li_rtn = dw_copyright.of_SetLinkage(TRUE)
li_rtn = dw_coauthors.of_SetLinkage(TRUE)
li_rtn = dw_ri.of_SetLinkage(TRUE)
li_rtn = dw_cdssubject.of_SetLinkage(TRUE)

li_rtn = dw_final_review.inv_linkage.of_SetMaster(dw_final_approval)
li_rtn = dw_final_review.inv_linkage.of_Register("conno", "mchar_conno")

li_rtn = dw_copyright.inv_linkage.of_SetMaster(dw_final_approval)
li_rtn = dw_copyright.inv_linkage.of_Register("conno", "mchar_conno")

li_rtn = dw_coauthors.inv_linkage.of_SetMaster(dw_final_approval)
li_rtn = dw_coauthors.inv_linkage.of_Register("conno", "mchar_conno")

li_rtn = dw_cdssubject.inv_linkage.of_SetMaster(dw_final_approval)
li_rtn = dw_cdssubject.inv_linkage.of_Register("conno", "mchar_conno")

li_rtn = dw_ri.inv_linkage.of_SetMaster(dw_final_approval)
li_rtn = dw_ri.inv_linkage.of_Register("conno", "mchar_conno")


//dw_final_review.inv_linkage.of_SetStyle(dw_final_review.inv_linkage.RETRIEVE)
//dw_copyright.inv_linkage.of_SetStyle(dw_copyright.inv_linkage.RETRIEVE)
//dw_coauthors.inv_linkage.of_SetStyle(dw_coauthors.inv_linkage.RETRIEVE)
//dw_cdssubject.inv_linkage.of_SetStyle(dw_cdssubject.inv_linkage.RETRIEVE)
//dw_ri.inv_linkage.of_SetStyle(dw_ri.inv_linkage.RETRIEVE)

dw_final_review.inv_linkage.of_SetStyle(2)
dw_copyright.inv_linkage.of_SetStyle(2)
dw_coauthors.inv_linkage.of_SetStyle(2)
dw_cdssubject.inv_linkage.of_SetStyle(2)
dw_ri.inv_linkage.of_SetStyle(2)


is_sql = This.Object.Datawindow.Table.Select






end event

event rowfocuschanged;string annotxt, lbkseq, ls_conno, ls_arg, ls_frflag, ls_priority
long ll_row
datetime ld_date,ld_pcsstdt
integer li_rtn

sle_currentrow.Text = string(currentrow)

IF currentrow > 0 and ib_found THEN
	
END IF

IF ib_found THEN	
	ib_save = FALSE	
	Lchno = This.object.chno[currentrow]
	Lconno = This.object.conno[currentrow]
	Call Super::rowfocuschanged

	Select anno into :annotxt from annotation where chno = :Lchno using sqlservertrans;
	IF sqlservertrans.SQLCode = 0  THEN
		mle_anno.text = annotxt
		anno_exist=TRUE
	ELSE
		anno_exist=FALSE
		mle_anno.text = ""
	END IF
	
	ls_frflag = dw_final_approval.object.frflag[currentrow]
	IF ISNULL(ls_frflag) OR TRIM(ls_frflag) = "" THEN
		dw_final_approval.object.frflag[currentrow]="Y"
	END IF
	
	ld_date = dw_final_approval.object.fr[currentrow]
	IF ISNULL(ld_date) THEN
		dw_final_approval.object.fr[currentrow]=Today()
	END IF
	
	ld_pcsstdt = dw_final_approval.object.pcsstdt[currentrow]
	IF ISNULL(ld_pcsstdt) AND dw_final_approval.object.pcsstdt_flg[currentrow]<>"N" THEN
		dw_final_approval.object.pcsstdt[currentrow]=ld_date
	END IF
	
	ls_priority = dw_final_approval.object.priority[currentrow]
	IF ISNULL(ls_priority) OR TRIM(ls_priority) = "" THEN
		dw_final_approval.object.priority[currentrow]="N"
	END IF
	
	// set the flash indicator based on the book media
	IF dw_final_approval.object.bkmed[currentrow] <> 'DB' THEN
		dw_final_approval.object.flash_indicator[currentrow] = ""
	END IF

	
	select prevbkseq into :Lbkseq from ri where conno =:lconno using sqlservertrans;
	IF sqlservertrans.sqlcode = 0 then
		ri_exist=TRUE
	ELSE
		ri_exist=FALSE
	END IF
		
END IF

il_currow = currentrow

end event

event retrieveend;call super::retrieveend;sle_rowcount.Text = string(rowcount)
//dw_final_approval.ResetUpdate()
end event

event itemchanged;call super::itemchanged;//////////////////////////////////////////////////////////////////////////////////////////////////////////
//
//	Event: itemchanged of dw_final_approval
//
//	Description:
//	set field validations
//
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//	
//	Revision History
//
//	Developed by 	Date 				Version						Tracking#
//									
// Murali K.			01/29/2008      005 PICS Modifications	 Reqs: CDS. A.7, CDS A.8a.2
//											If in final review PCS i.e book sequence# is present cannot change
// 											flash indicator to no
// Murali K.			01/06/2010		#2205 remove frflag and process using frdate
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//
datetime ld_date, ld_today, ldt_null
string ls_other_conno,ls_med,ls_conno,ls_msg, ls_org_flash_ind, NullStr, ls_userid
integer rtn,li_count,li_no_conno
long ll_bkseq

       
SetNull(NullStr)
ld_today = datetime(Today(),Now())
ls_userid = sqlservertrans.userid

IF dwo.name = "med" THEN // Required field med

	ls_org_flash_ind = This.object.flash_indicator[row]
	ls_med = this.object.med[row]
	
	IF len(data) < 2  THEN
		this.Object.med.ValidationMsg='You must supply Medium.'
		dw_final_approval.Setfocus()
		dw_final_approval.Setitem(row,"med",ls_med)
		RETURN 1
	// IF media was changed to BR or P/B
	ELSEIF ls_med <> 'RTB' OR ls_med <> 'RC' THEN
		rtn = MessageBox("Media Change","This control number was originally assigned to a RTB.~r~nDo you want to change it?",Question!,YesNo!,1)
		IF rtn = 1 THEN
			ls_other_conno = This.object.other_media_conno[row]
			
			UPDATE MCHAR 
			SET flash_indicator = NULL , other_media_conno = NULL
			WHERE conno = :ls_other_conno
			USING SQLServerTrans;
			IF f_check_dberror(sqlservertrans,"MCHAR") THEN
				COMMIT USING SqlServerTrans;
			ELSE
				ROLLBACK USING SqlServerTrans;
				ls_msg = "Unable to change the flash indicator in MCHAR for control number "+ls_other_conno
				this.Object.mchar_med.ValidationMsg=ls_msg
				RETURN 1
			END IF
					
			This.object.flash_indicator[row] = ""
			This.object.other_media_conno[row] = ""
		ELSE
			This.object.flash_indicator[row] = ls_org_flash_ind
			This.Object.other_media_conno.Visible = TRUE
			RETURN 1
		END IF
	END IF // IF media was changed to BR or P/B
ELSEIF dwo.Name = "flash_indicator" THEN
	
	//01/29/2008 if in final review PCS - book sequence# is present cannot change
	// flash indicator to no
	ll_bkseq = this.object.bkseq[1]
	IF ll_bkseq > 0 AND data = 'N' THEN
		ls_msg = "Flash Indicator cannot be unchecked, since there is a book already present in production"
		this.Object.flash_indicator.ValidationMsg=ls_msg
		RETURN 1
	END IF
				
	this.object.flash_change_date[row] = ld_today
	this.object.flash_change_user[row] = ls_userid		
	// #2205 commented
//If user selects frflag = "NO", then final review and pcsstdt dates should be set null
//ELSEIF dwo.name = "frflag" AND data = "N" THEN
//	ld_date = dw_final_approval.Getitemdatetime(row,"fr")
//	SetNULL(ld_date)
//	dw_final_approval.object.fr[row]=ld_date
//	dw_final_approval.object.pcsstdt[row]=ld_date
//	rtn = Messagebox("Delete","Do you want to delete the record from the catalog table?",QUESTION!,YesNo!)
//	IF rtn = 1 THEN
//		SELECT COUNT(*) INTO :li_count
//		FROM catalog
//		WHERE catalog.conno = :Lconno
//		USING sqlservertrans;
//		IF li_count > 0 THEN
//			DELETE FROM catalog
//		   WHERE catalog.conno = :Lconno
//		   USING sqlservertrans;
//			IF f_check_dberror(sqlservertrans,"CATALOG") THEN
//				COMMIT USING SqlServerTrans;
//				Messagebox("Delete","Record successfully deleted from catalog table.")
//			ELSE
//				ROLLBACK USING SqlServerTrans;
//			END IF
//		ELSE
//			Messagebox("Warning","No Catalog information for control number "+Lconno+" was found.")
//		END IF
//	ELSE
//		SELECT COUNT(*) INTO :li_count
//		FROM catalog
//		WHERE catalog.conno = :Lconno
//		USING sqlservertrans;
//		IF li_count > 0 THEN
//			UPDATE catalog
//			SET s1 = NULL
//		   WHERE catalog.conno = :Lconno
//		   USING sqlservertrans;
//			IF f_check_dberror(sqlservertrans,"CATALOG") THEN
//				COMMIT USING SqlServerTrans;
//				Messagebox("Update","Stage I date in catalog table was set to NULL.")
//			ELSE
//				ROLLBACK USING SqlServerTrans;
//			END IF
//		ELSE
//			Messagebox("Warning","No Catalog information for control number "+Lconno+" was found.")
//		END IF
//	END IF
/// #2205 above commented
//If user selects pcsstdt_flg = "NO", then pcsstdt dates should be set null
ELSEIF dwo.name = "pcsstdt_flg" AND data = "N" THEN
	SetNULL(ld_date)
	dw_final_approval.object.pcsstdt[row]=ld_date
//If user selects pcsstdt_flg = "Yes", then pcsstdt dates should be set fr
ELSEIF dwo.name = "pcsstdt_flg" AND data = "Y" THEN
	dw_final_approval.object.pcsstdt[row]=ld_today
// #2205 1/6/2010	
ELSEIF dwo.name = 'fr' THEN
	date ld_dt
	ld_date = datetime(data)
	dw_final_approval.object.pcsstdt[row]=ld_date
	dw_final_approval.object.pcsstdt_flg[row]='Y'
	ld_dt = date(ld_date)
	IF Isdate(string(ld_dt,'mm/dd/yyyy')) THEN
		dw_final_approval.object.frflag[row]='Y'
	ELSE
		dw_final_approval.object.frflag[row]='N'
		setnull(ldt_null)
		dw_final_approval.object.pcsstdt[row]= ldt_null
		dw_final_approval.object.pcsstdt_flg[row]='N'
	END IF
	// #2205 1/16/2010
END IF
end event

event sqlpreview;call super::sqlpreview;//MessageBox("SQL",sqlsyntax)
end event


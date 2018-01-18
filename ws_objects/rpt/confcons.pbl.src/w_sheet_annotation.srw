$PBExportHeader$w_sheet_annotation.srw
forward
global type w_sheet_annotation from w_sheet
end type
type cb_1 from commandbutton within w_sheet_annotation
end type
type cbx_3 from checkbox within w_sheet_annotation
end type
type st_5 from statictext within w_sheet_annotation
end type
type cbx_2 from checkbox within w_sheet_annotation
end type
type st_4 from statictext within w_sheet_annotation
end type
type cbx_1 from checkbox within w_sheet_annotation
end type
type st_3 from statictext within w_sheet_annotation
end type
type mle_tags from u_mle within w_sheet_annotation
end type
type em_bkseq from u_em within w_sheet_annotation
end type
type st_or from statictext within w_sheet_annotation
end type
type st_bkno from u_st within w_sheet_annotation
end type
type cb_bknav from commandbutton within w_sheet_annotation
end type
type dw_cdssubject_find from u_pics_dw within w_sheet_annotation
end type
type dw_ri from u_pics_dw within w_sheet_annotation
end type
type cb_fanno from commandbutton within w_sheet_annotation
end type
type cb_lang from commandbutton within w_sheet_annotation
end type
type st_1 from statictext within w_sheet_annotation
end type
type em_conno from uo_conno within w_sheet_annotation
end type
type st_2 from statictext within w_sheet_annotation
end type
type cb_find from commandbutton within w_sheet_annotation
end type
type cb_update from commandbutton within w_sheet_annotation
end type
type cb_clear from commandbutton within w_sheet_annotation
end type
type cb_exit from commandbutton within w_sheet_annotation
end type
type mle_anno from u_mle within w_sheet_annotation
end type
type cb_spell from commandbutton within w_sheet_annotation
end type
type dw_annotation from u_pics_dw within w_sheet_annotation
end type
type dw_medium from u_pics_dw within w_sheet_annotation
end type
type dw_coauthors from u_pics_dw within w_sheet_annotation
end type
type cb_print from commandbutton within w_sheet_annotation
end type
type st_wordcnt from statictext within w_sheet_annotation
end type
type cb_zoom from commandbutton within w_sheet_annotation
end type
type dw_cc_pms_par_report from u_pics_dw within w_sheet_annotation
end type
type dw_cc_anno_rpt from u_pics_dw within w_sheet_annotation
end type
type dw_cdssubject from u_pics_dw within w_sheet_annotation
end type
end forward

shared variables

end variables
global type w_sheet_annotation from w_sheet
integer x = 5
integer y = 4
integer width = 3392
integer height = 1928
cb_1 cb_1
cbx_3 cbx_3
st_5 st_5
cbx_2 cbx_2
st_4 st_4
cbx_1 cbx_1
st_3 st_3
mle_tags mle_tags
em_bkseq em_bkseq
st_or st_or
st_bkno st_bkno
cb_bknav cb_bknav
dw_cdssubject_find dw_cdssubject_find
dw_ri dw_ri
cb_fanno cb_fanno
cb_lang cb_lang
st_1 st_1
em_conno em_conno
st_2 st_2
cb_find cb_find
cb_update cb_update
cb_clear cb_clear
cb_exit cb_exit
mle_anno mle_anno
cb_spell cb_spell
dw_annotation dw_annotation
dw_medium dw_medium
dw_coauthors dw_coauthors
cb_print cb_print
st_wordcnt st_wordcnt
cb_zoom cb_zoom
dw_cc_pms_par_report dw_cc_pms_par_report
dw_cc_anno_rpt dw_cc_anno_rpt
dw_cdssubject dw_cdssubject
end type
global w_sheet_annotation w_sheet_annotation

type variables
string Lchno,Lsexviol,Lpmsgrp,org_crname,what_text, is_conno, is_comm_audio, ls_menu
int anno_stage
boolean ib_anno_modified=FALSE,ri_exist=FALSE,anno_exist=FALSE, anno_tags_altered=FALSE, first_time_annotation=FALSE,ib_update=FALSE
DataWindowChild ldwc_cdssub,ldwc_cdssubcode,ldwc_casub
nvo_val_init_con lnvo_val_anno
end variables
forward prototypes
public subroutine wf_disable_buttons ()
public subroutine wf_validate_ricd ()
public function boolean wf_coauthor_dup_check_ok ()
public subroutine wf_count_words ()
public function string wf_build_sex_sentence (string lc_sex, string lc_viol, string lc_slang)
public function string wf_sex_viol_lang ()
public subroutine wf_disable_all ()
public subroutine wf_enable_buttons ()
public subroutine wf_set_spinoff_fields ()
public subroutine wf_view_all ()
public function string wf_which_flang (string flang_code)
public function boolean wf_validate_group_access (string luserid, string lgroupid)
public function boolean wf_validate_anno_stage (string conno, string annoinit, string cdsinit, string pmsinit, integer annostg, string pcsinit)
public function string wf_build_book_sentence (string ls_casub, string ls_prize, string ls_anyr, string ls_bseller_flag, string ls_med, string ls_g1br, string ls_conno, string ls_flang)
public subroutine wf_get_wordcount ()
public function integer wf_set_comm_audio_vendor (string as_comm_audio)
public subroutine wf_check_media_flags ()
public function integer of_mediavalidation (long al_row, string as_old_media, string as_new_media)
public function integer wf_set_bestseller ()
public function integer wf_get_label_data (string as_conno)
public function integer wf_clear_label_fields (boolean ab_visible)
public function integer wf_validate_cdssubject ()
end prototypes

public subroutine wf_disable_buttons ();w_sheet_annotation.cb_update.Enabled=FALSE
w_sheet_annotation.cb_clear.Enabled=FALSE
w_sheet_annotation.cb_spell.Enabled=FALSE
w_sheet_annotation.cb_zoom.Enabled = FALSE
w_sheet_annotation.cb_find.Enabled=TRUE
w_sheet_annotation.cb_print.Enabled=FALSE

end subroutine

public subroutine wf_validate_ricd ();string ricode
long prevbkseq

ricode = dw_annotation.object.mchar_ricd[1]
prevbkseq = dw_annotation.object.ri_prevbkseq[1]

IF (ricode <> ""  or IsNull(ricode)=FALSE) and (prevbkseq > 0)  THEN
	dw_annotation.Object.mchar_ricd.tabsequence='0'
	dw_annotation.Object.ri_prevbkmed.tabsequence='0'
	dw_annotation.Object.ri_prevbkseq.tabsequence='0'
END IF
end subroutine

public function boolean wf_coauthor_dup_check_ok ();int li_row_count, i, j
string ls_auth, ls_authfn, ls_plus_auth, ls_plus_auth_check

li_row_count =this.dw_coauthors.RowCount()
IF li_row_count >=2 THEN
	FOR i =1 to (li_row_count - 1)
		for j= i+1 to li_row_count
			ls_auth= trim(this.dw_coauthors.object.coauth[i])
			ls_authfn =trim(this.dw_coauthors.object.coauthfn[i])
			ls_plus_auth = ls_auth+ ls_authfn 
			ls_auth= trim(this.dw_coauthors.object.coauth[j])
			ls_authfn =trim(this.dw_coauthors.object.coauthfn[j])
			ls_plus_auth_check = ls_auth + ls_authfn
			IF lower(ls_plus_auth) = lower(ls_plus_auth_check) THEN
				//MessageBox('error','the coauthor is duplicate')
				this.dw_coauthors.SetFocus()
				Return false
			END IF
		NEXT
	NEXT
END IF
return true
end function

public subroutine wf_count_words ();Integer			ll_count,ll_count_sex
string 			ls_space,ls_source,ls_sex,ls_chno
n_cst_string 	inv_string

ls_chno 	= dw_annotation.object.ttlinit_chno[1]

Select anno_property
into :ls_sex
from annotation 
where chno=:ls_chno 
using sqlservertrans;

ls_source = mle_anno.text
IF NOT(IsNull(ls_sex))  THEN
	ls_source = ls_source + " " +ls_sex
END IF

ls_space = " "

ls_source =  inv_string.of_GlobalReplace(ls_source, "       ", " ")
ls_source =  inv_string.of_GlobalReplace(ls_source, "      ", " ")
ls_source =  inv_string.of_GlobalReplace(ls_source, "     ", " ")
ls_source =  inv_string.of_GlobalReplace(ls_source, "    ", " ")
ls_source =  inv_string.of_GlobalReplace(ls_source, "   ", " ")
ls_source =  inv_string.of_GlobalReplace(ls_source, "  ", " ")

ll_count = inv_string.of_CountOccurrences(ls_source, ls_space, TRUE)

st_wordcnt.text = String (ll_count+1, '##0')

end subroutine

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

public function string wf_sex_viol_lang ();string ls_sexcd,ls_viol,ls_lang,ls_text,ls_casub,ls_prize,ls_anyr,ls_bseller,ls_book,ls_med,ls_gbr,ls_conno,ls_sextext,ls_flang
integer currow,li_sex_grp_ord,li_med_grp_ord
n_cst_string	lnv_string
SetNull(ls_casub)

SELECT anno_properties.sex_group_ord,   
       anno_properties.med_group_ord 
INTO :li_sex_grp_ord,   
     :li_med_grp_ord
FROM anno_properties  USING SQLServerTrans;

currow = dw_annotation.Getrow()

ls_sexcd = dw_annotation.object.ttlinit_sexcd[currow]
ls_viol 	= dw_annotation.object.ttlinit_violencecd[currow]
ls_lang 	= dw_annotation.object.ttlinit_stronglang[currow]

ls_casub 	= dw_annotation.object.ttlinit_gradecd[currow]
ls_prize 	= dw_annotation.object.ttlinit_prize[currow]
ls_bseller 	=  dw_annotation.object.ttlinit_bestseller[currow]
ls_flang 	= dw_annotation.object.ttlinit_lang[currow]
ls_anyr 		= string(dw_annotation.object.ttlinit_anyr[currow])

IF dw_medium.rowcount() > 0 THEN
	ls_gbr 		= dw_medium.object.mchar_g1br[currow]
	ls_med 		= dw_medium.object.mchar_med[currow]
END IF


ls_conno 	= em_conno.text
  

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

ls_text =  lnv_string.of_GlobalReplace(ls_text, "           "," ")
ls_text =  lnv_string.of_GlobalReplace(ls_text, "          "," ")
ls_text =  lnv_string.of_GlobalReplace(ls_text, "        "," ")
ls_text =  lnv_string.of_GlobalReplace(ls_text, "      "," ")
ls_text =  lnv_string.of_GlobalReplace(ls_text, "    "," ")
ls_text =  lnv_string.of_GlobalReplace(ls_text, "   "," ")
ls_text =  lnv_string.of_GlobalReplace(ls_text, "  "," ")

RETURN ls_text
end function

public subroutine wf_disable_all ();dw_annotation.Enabled = FALSE
dw_coauthors.Enabled = FALSE
dw_medium.Enabled = FALSE
dw_ri.Enabled = FALSE
dw_cdssubject.Enabled = FALSE
//mle_anno.Enabled = FALSE
cb_clear.Enabled = FALSE
cb_update.Enabled = FALSE
cb_spell.Enabled = FALSE
cb_zoom.Enabled = TRUE
cb_print.Enabled = TRUE
cb_find.Enabled = FALSE

end subroutine

public subroutine wf_enable_buttons ();w_sheet_annotation.cb_update.Enabled=TRUE
w_sheet_annotation.cb_clear.Enabled=TRUE
w_sheet_annotation.cb_find.Enabled=FALSE
w_sheet_annotation.cb_spell.Enabled=TRUE
w_sheet_annotation.cb_zoom.Enabled = TRUE
w_sheet_annotation.cb_print.Enabled = TRUE

end subroutine

public subroutine wf_set_spinoff_fields ();			dw_annotation.object.ttlinit_chno.TabSequence = '0'
			dw_annotation.object.ttlinit_aepcd.TabSequence = '0'
			dw_annotation.object.ttlinit_sauth.TabSequence = '0'
			dw_annotation.object.ttlinit_sttl.TabSequence = '0'
			dw_annotation.object.ttlinit_ajyfn.TabSequence = '0'
			dw_annotation.object.ttlinit_lang.TabSequence = '0'
			dw_annotation.object.ttlinit_lcno.TabSequence = '0'
			dw_annotation.object.ttlinit_casub.TabSequence = '0'
			dw_annotation.object.ttlinit_oneliner.TabSequence = '0'
			dw_annotation.object.ttlinit_annoinit.TabSequence = '0'
			dw_annotation.object.ttlinit_pminit.TabSequence = '0'
			dw_annotation.object.ttlinit_cdinit.TabSequence = '0'
			dw_annotation.object.ttlinit_pmsub1.TabSequence = '0'
			dw_annotation.object.ttlinit_pmsub2.TabSequence = '0'
			dw_annotation.object.ttlinit_pmsub3.TabSequence = '0'
			dw_annotation.object.ttlinit_specattn.TabSequence = '0'
			dw_annotation.object.ttlinit_cryr.TabSequence = '0'
			dw_annotation.object.ttlinit_ccdt.TabSequence = '0'
			dw_annotation.object.ttlinit_pmedt.TabSequence = '0'
			dw_annotation.object.ttlinit_anyr.TabSequence = '0'
			dw_annotation.object.ttlinit_cycle.TabSequence = '0'
			dw_annotation.object.ttlinit_auth.TabSequence = '10'
			dw_annotation.object.ttlinit_ahonorific.TabSequence = '20'
			dw_annotation.object.ttlinit_authfn.TabSequence = '30'
			dw_annotation.object.ttlinit_ttlart.TabSequence = '40'
			dw_annotation.object.ttlinit_ttl.TabSequence = '50'
			dw_annotation.object.ttlinit_note.TabSequence = '60'
			dw_annotation.object.ttlinit_serttl.TabSequence = '70'
			dw_annotation.object.ttlinit_seqnote.TabSequence = '80'

end subroutine

public subroutine wf_view_all ();dw_annotation.Enabled = FALSE
dw_ri.Object.ri_prevbkseq.Protect='1'
dw_ri.Object.ri_prevbkmed.Protect='1'
dw_medium.Object.mchar_ricd.Protect='1'
dw_medium.Object.mchar_med.Protect='1'
dw_medium.Object.mchar_conno.Protect='1'
dw_medium.Object.mchar_priority.Protect='1'
dw_ri.Object.ri_conno.Protect='1'
dw_ri.Object.ri_prevbkseq.Protect='1'
dw_ri.Object.ri_prevbkmed.Protect='1'
dw_coauthors.Object.coauth.Protect='1'
dw_coauthors.Object.coauthfn.Protect='1'
dw_coauthors.Object.chonorific.Protect='1'
cb_clear.Enabled = FALSE
cb_update.Enabled = FALSE
cb_spell.Enabled = FALSE
cb_zoom.Enabled = TRUE
cb_print.Enabled = TRUE
cb_find.Enabled = FALSE

end subroutine

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

public function boolean wf_validate_group_access (string luserid, string lgroupid);string Lgrp,Lusr,ls_message,ls_msgparm[1]

SELECT picsuser.group_, 
		 picsuser.userid
INTO 	:Lgrp,
		:Lusr
FROM 	picsuser 
WHERE picsuser.userid = :Luserid AND
		picsuser.group_  = :Lgroupid
USING SQLserverTrans;

IF sqlservertrans.SQLCode < 0 THEN
	ls_message = "A database error has occurred during SELECT.~n" + &
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
ELSEIF sqlservertrans.SQLCode = 100  THEN
	// Row Not found
	MessageBox("ERROR","Userid ~'"+Luserid+"~' does not belong to ~'"+Lgroupid+"~' group. ")
	RETURN FALSE
ELSE
	// Row found
	Lpmsgrp=Lgrp
	RETURN TRUE
END IF
end function

public function boolean wf_validate_anno_stage (string conno, string annoinit, string cdsinit, string pmsinit, integer annostg, string pcsinit);int rtn
string ls_userid, ls_group

//add this parts to catch that if user is admin then do not need to validate annoint, cdinit, pminit, pcsinit
ls_userid=sqlservertrans.userid
select group_   into :ls_group
from picsuser
where userid =:ls_userid
using sqlservertrans;
if f_check_dberror(sqlservertrans, 'find the group that userid associated with')=false then
	return FALSE
end if
if ls_group='ADMIN' THEN RETURN TRUE

CHOOSE CASE annostg
	// Add Annotation
	CASE 1
		IF (IsNull(annoinit)=FALSE OR annoinit<>"") AND &
			(IsNull(cdsinit)=FALSE OR cdsinit<>"" ) AND &
			(IsNull(pmsinit)=FALSE OR pmsinit<>"" ) THEN
   		MessageBox("Error", "Control Number: "+conno+" has been initialized by ~n CDS and PMS staff." ,StopSign!)
			SetNull(em_bkseq.text)
			RETURN FALSE
		ELSEIF (IsNull(annoinit)=FALSE OR annoinit<>"") AND &
				 (IsNull(cdsinit)=FALSE OR cdsinit<>"") THEN
   		MessageBox("Error", "Control Number: "+conno+" has been initialized by ~n CDS staff." ,StopSign!)
			SetNull(em_bkseq.text)
			RETURN FALSE
		ELSEIF (IsNull(annoinit)=FALSE OR annoinit<>"") THEN
   		rtn = MessageBox("Warning", "Annotation already exists.~nDo you really want to continue?" ,Question!, YesNo!, 1)
			IF rtn = 1 THEN
				dw_annotation.Object.ttlinit_annoinit.Protect=0
				dw_annotation.Object.ttlinit_cdinit.Protect=1
				dw_annotation.Object.ttlinit_pminit.Protect=1
				RETURN TRUE
			ELSE
				SetNull(em_bkseq.text)
				RETURN FALSE
			END IF
		ELSE
			dw_annotation.Object.ttlinit_annoinit.Protect=0
			dw_annotation.Object.ttlinit_cdinit.Protect=1
			dw_annotation.Object.ttlinit_pminit.Protect=1
			dw_annotation.Object.ttlinit_annoinit.Edit.Required='Yes'
			first_time_annotation = TRUE
			RETURN TRUE
		END IF
	// CDS Edit Annotation
	CASE 2
		IF (IsNull(annoinit)=TRUE OR annoinit="") THEN
   		MessageBox("Error", "Control Number: "+conno+", has not yet been initialized." ,StopSign!)
			SetNull(em_bkseq.text)
			RETURN FALSE
		ELSEIF (IsNull(annoinit)=FALSE  OR annoinit<>"") AND &
				 (IsNull(cdsinit)=FALSE  OR cdsinit<>"") AND &
				 (IsNull(pmsinit)=FALSE OR pmsinit<>"") THEN
   		MessageBox("Error", "Control Number: "+conno+" has been initialized by ~n CDS and PMS staff." ,StopSign!)
			SetNull(em_bkseq.text)
			RETURN FALSE
		ELSEIF (IsNull(annoinit)=FALSE OR annoinit<>"") AND &
				 (IsNull(cdsinit)=TRUE OR cdsinit="") THEN
			dw_annotation.Object.ttlinit_annoinit.Protect=1
			dw_annotation.Object.ttlinit_cdinit.Protect=0
			dw_annotation.Object.ttlinit_pminit.Protect=1
			dw_annotation.Object.ttlinit_cdinit.Edit.Required='Yes'
			RETURN TRUE
		ELSEIF (IsNull(annoinit)=FALSE OR annoinit<>"") AND &
				 (IsNull(cdsinit)=FALSE OR cdsinit<>"") AND &
				 (IsNull(pmsinit)=TRUE OR pmsinit="") THEN
   		rtn = MessageBox("Warning", " Annotation has already been signed off by CDS Staff.~nDo you really want to continue?" ,Question!, YesNo!, 1)
			IF rtn = 1 THEN
				dw_annotation.Object.ttlinit_annoinit.Protect=1
				dw_annotation.Object.ttlinit_cdinit.Protect=0
				dw_annotation.Object.ttlinit_pminit.Protect=1
				dw_annotation.Object.ttlinit_cdinit.Edit.Required='Yes'
				RETURN TRUE
			ELSE
				SetNull(em_bkseq.text)
				RETURN FALSE
			END IF
		ELSE
			SetNull(em_bkseq.text)
			RETURN FALSE
		END IF
	// PMS Edit Annotation
	CASE 3
		IF (IsNull(annoinit)=TRUE OR annoinit="") THEN
   		MessageBox("Error", " Control Number: "+conno+", has not yet been initialized." ,StopSign!)
			SetNull(em_bkseq.text)
			RETURN FALSE
		ELSEIF (IsNull(cdsinit)=TRUE OR cdsinit="") THEN
   		MessageBox("Error", " Control Number: "+conno+", has not yet been initialized by CDS staff." ,StopSign!)
			SetNull(em_bkseq.text)
			RETURN FALSE
		ELSEIF (IsNull(annoinit)=FALSE OR annoinit<>"") AND &
				 (IsNull(cdsinit)=FALSE OR cdsinit<>"") AND &
				 (IsNull(pmsinit)=FALSE OR pmsinit="") THEN
   		rtn = MessageBox("Warning", " Annotation has already been signed off by PMS Staff.~nDo you really want to continue?" ,Question!, YesNo!, 1)
			IF rtn = 1 THEN
				//dw_coauthors.Enabled = FALSE
				dw_medium.Enabled = FALSE
				dw_cdssubject.Enabled = FALSE
				dw_annotation.object.ttlinit_chno.protect=1   
				dw_annotation.Object.ttlinit_annoinit.Protect=1
				dw_annotation.Object.ttlinit_cdinit.Protect=1
				dw_annotation.Object.ttlinit_pminit.Protect=0
				dw_annotation.Object.ttlinit_pminit.Edit.Required='Yes'
				RETURN TRUE
			ELSE
			   SetNull(em_bkseq.text)
				RETURN FALSE
			END IF
		ELSEIF (IsNull(annoinit)=FALSE OR annoinit<>"") AND &
				 (IsNull(cdsinit)=FALSE OR cdsinit<>"") AND &
				 (IsNull(pmsinit)=TRUE OR pmsinit="") THEN
			//dw_coauthors.Enabled = FALSE
			dw_medium.Enabled = FALSE
			dw_annotation.object.ttlinit_chno.protect=1   
			dw_annotation.Object.ttlinit_annoinit.Protect=1
			dw_annotation.Object.ttlinit_cdinit.Protect=1
			dw_annotation.Object.ttlinit_pminit.Protect=0
			dw_annotation.Object.ttlinit_pminit.Edit.Required='Yes'
			RETURN TRUE
		ELSE
			SetNull(em_bkseq.text)
			RETURN FALSE
		END IF
	// PCS Edit Annotation
	CASE 6
		IF (IsNull(annoinit)=TRUE OR annoinit="") THEN
   		MessageBox("Error", " Control Number: "+conno+", has not yet been initialized." ,StopSign!)
			SetNull(em_bkseq.text)
			RETURN FALSE
		ELSEIF (IsNull(cdsinit)=TRUE OR cdsinit="") THEN
   		MessageBox("Error", " Control Number: "+conno+", has not yet been initialized by CDS staff." ,StopSign!)
			SetNull(em_bkseq.text)
			RETURN FALSE
		ELSEIF (IsNull(pmsinit)=TRUE OR pmsinit="") THEN
   		MessageBox("Error", " Control Number: "+conno+", has not yet been initialized by PMS staff." ,StopSign!)
			SetNull(em_bkseq.text)
			RETURN FALSE
		ELSEIF (IsNull(annoinit)=FALSE OR annoinit<>"") AND &
				 (IsNull(cdsinit)=FALSE OR cdsinit<>"") AND &
				 (IsNull(pmsinit)=FALSE OR pmsinit<>"") AND &
				 (IsNull(pcsinit)=FALSE OR pcsinit="") THEN
   		rtn = MessageBox("Warning", " Annotation has already been signed off by PCS Staff.~nDo you really want to continue?" ,Question!, YesNo!, 1)
			IF rtn = 1 THEN
				//dw_coauthors.Enabled = FALSE
				dw_medium.Enabled = TRUE
				dw_annotation.object.ttlinit_chno.protect=1   
				dw_annotation.Object.ttlinit_annoinit.Protect=1
				dw_annotation.Object.ttlinit_cdinit.Protect=1
				dw_annotation.Object.ttlinit_pminit.Protect=1
				dw_annotation.Object.ttlinit_pcsinit.Protect=0
				dw_annotation.Object.ttlinit_pcsinit.Edit.Required='Yes'
				RETURN TRUE
			ELSE
			   SetNull(em_bkseq.text)
				RETURN FALSE
			END IF
		ELSEIF (IsNull(annoinit)=FALSE OR annoinit<>"") AND &
				 (IsNull(cdsinit)=FALSE OR cdsinit<>"") AND &
				 (IsNull(pmsinit)=FALSE OR pmsinit<>"") AND &
				 (IsNull(pcsinit)=TRUE OR pcsinit="") THEN
			//dw_coauthors.Enabled = FALSE
			dw_medium.Enabled = TRUE
			dw_annotation.object.ttlinit_chno.protect=1   
			dw_annotation.Object.ttlinit_annoinit.Protect=1
			dw_annotation.Object.ttlinit_cdinit.Protect=1
			dw_annotation.Object.ttlinit_pminit.Protect=1
			dw_annotation.Object.ttlinit_pcsinit.Protect=0
			dw_annotation.Object.ttlinit_pcsinit.Edit.Required='Yes'
			RETURN TRUE
		ELSE
			SetNull(em_bkseq.text)
			RETURN FALSE
		END IF
	// CDS Bibliographic Edit Annotation
	CASE 4
		IF (IsNull(annoinit)=TRUE OR annoinit="") THEN
   		MessageBox("Error", " Control Number: "+conno+", has not yet been initialized." ,StopSign!)
			SetNull(em_bkseq.text)
			RETURN FALSE
		ELSEIF (IsNull(cdsinit)=TRUE OR cdsinit="") THEN
   		MessageBox("Error", " Control Number: "+conno+", has not yet been initialized by CDS staff." ,StopSign!)
			SetNull(em_bkseq.text)
			RETURN FALSE
		ELSEIF (IsNull(annoinit)=FALSE OR annoinit<>"") AND &
				 (IsNull(cdsinit)=FALSE OR cdsinit<>"") AND &
				 (IsNull(pmsinit)=FALSE OR pmsinit<>"") THEN
			wf_set_spinoff_fields()
			dw_coauthors.Enabled = FALSE
			mle_anno.Enabled = FALSE
			dw_medium.SetFocus()
			RETURN TRUE
		ELSEIF (IsNull(annoinit)=FALSE OR annoinit<>"") AND &
				 (IsNull(cdsinit)=FALSE OR cdsinit<>"") AND &
				 (IsNull(pmsinit)=TRUE OR pmsinit="") THEN
   		MessageBox("Error", " Control Number: "+conno+", has not yet been initialized by PMS staff." ,StopSign!)
			SetNull(em_bkseq.text)
			RETURN FALSE
		ELSE
			SetNull(em_bkseq.text)
			RETURN FALSE
		END IF
	// View/Print Annotation
	CASE 5
		RETURN TRUE		
	CASE ELSE
		SetNull(em_bkseq.text)
		RETURN FALSE
END CHOOSE
end function

public function string wf_build_book_sentence (string ls_casub, string ls_prize, string ls_anyr, string ls_bseller_flag, string ls_med, string ls_g1br, string ls_conno, string ls_flang);//////////////////////////////////////////////////////////////////////////////////////////////////////////
//
//	Function: wf_build_book_sentence
// Args:  catsub, prize, annotation year, best seller flag, media, grade 1 braille,  
//			conno, foreign language
//	Description:
//	Set the annotation taglines using the ANNO_PROPERTIES table
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//	
//	Revision History
//
//	Developed by 	Date 				Purpose/ Version						Tracking#
//									
// Murali K.			09/30/2008       PICS 2.0 Modifications	 2090 -
//											 Introduce ls_text8 to process 8th order 09/30/2008 , 
//											 introduce commercial audio order
//////////////////////////////////////////////////////////////////////////////////////////////////////////

int li_casubord,li_prizeord,li_bsellerord,li_anyr,li_pbr,li_gbr,li_flang,li_comm_audio
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
IF IsNull(media) THEN
	media = ls_med
END IF

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
	ls_comm_audio_yn =dw_annotation.object.mchar_comm_audio_yn[1]
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
		ls_text1 +=TRIM(ls_language)+'. '
	ELSEIF li_flang=2 THEN
		ls_text2 +=TRIM(ls_language)+'. '
	ELSEIF li_flang=3 THEN
		ls_text3 +=TRIM(ls_language)+'. '
	ELSEIF li_flang=4 THEN
		ls_text4 +=TRIM(ls_language)+'. '
	ELSEIF li_flang=5 THEN
		ls_text5 +=TRIM(ls_language)+'. '
	ELSEIF li_flang=6 THEN
		ls_text6 +=TRIM(ls_language)+'. '
	ELSEIF li_flang=7 THEN
		ls_text7 +=TRIM(ls_language)+'. '
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

public subroutine wf_get_wordcount ();STRING ls_source,ls_space
INTEGER ll_count
n_cst_string  inv_string
		
IF NOT(IsNull(mle_tags.text)) AND  NOT(IsNull(mle_anno.text)) THEN
	ls_source = mle_anno.text + " " +mle_tags.text
ELSEIF (IsNull(mle_tags.text)) AND  NOT(IsNull(mle_anno.text)) THEN
	ls_source = mle_tags.text
ELSEIF NOT (IsNull(mle_tags.text)) AND  (IsNull(mle_anno.text)) THEN
	ls_source = mle_anno.text 
ELSEIF (IsNull(mle_tags.text)) AND  (IsNull(mle_anno.text)) THEN
	ls_source = ""
END IF

ls_space = " "

IF NOT (IsNull(ls_source)) THEN
	ls_source =  inv_string.of_GlobalReplace(ls_source, "       ", " ")
	ls_source =  inv_string.of_GlobalReplace(ls_source, "      ", " ")
	ls_source =  inv_string.of_GlobalReplace(ls_source, "     ", " ")
	ls_source =  inv_string.of_GlobalReplace(ls_source, "    ", " ")
	ls_source =  inv_string.of_GlobalReplace(ls_source, "   ", " ")
	ls_source =  inv_string.of_GlobalReplace(ls_source, "  ", " ")
	
	ll_count = inv_string.of_CountOccurrences(ls_source, ls_space, TRUE)
	st_wordcnt.text = String (ll_count+1, '##0')	
END IF

end subroutine

public function integer wf_set_comm_audio_vendor (string as_comm_audio);//////////////////////////////////////////////////////////////////////////////////////////////////////////
//
//	Function: wf_set_comm_audio_vendor
// Args: string as_comm_audio
//	Description:
//	Comm audio column settings and annotation
//
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//	
//	Revision History
//
//	Developed by 	Date 				Version						Tracking#
//									
// Murali K.			01/28/2008      005 PICS Modifications	 Reqs: CDS. A.7, CDS A.8a.2
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//

String ls_comm_audio_yn, ls_comm_audio_vendor
long ll_year

IF dw_annotation.Rowcount()> 0 THEN
	ls_comm_audio_yn = dw_annotation.object.mchar_comm_audio_yn[1]
	ls_comm_audio_vendor = dw_annotation.object.mchar_comm_audio_vendor[1]
	IF ls_comm_audio_yn = 'Y' THEN
		dw_annotation.object.mchar_comm_audio_vendor.visible=TRUE
	ELSE
		dw_annotation.object.mchar_comm_audio_vendor.visible=FALSE
	END IF
END IF


RETURN 1

end function

public subroutine wf_check_media_flags ();// The purpose of this function is to make sure that the field Other_media_conno is properly populated


int i,ll_rows
string ls_other_conno1,ls_other_conno2,ls_other_conno3,ls_conno1, ls_conno2, ls_conno3,ls_med1,ls_med2, ls_med3

ll_rows = dw_medium.RowCount()

CHOOSE CASE ll_rows
		
	CASE 2
		ls_med1 = dw_medium.object.mchar_med[1]
		ls_med2 = dw_medium.object.mchar_med[2]
		
		IF ls_med1 = 'RTB' AND ls_med2='RTB' THEN
			
			ls_other_conno1 = dw_medium.object.other_media_conno[1]
			ls_other_conno2 = dw_medium.object.other_media_conno[2]
			ls_conno1 = dw_medium.object.mchar_conno[1]
			ls_conno2 = dw_medium.object.mchar_conno[2]
				
			IF ls_other_conno1 <> ls_conno2 THEN
				ls_other_conno1 = ls_conno2
			END IF
				
			IF ls_other_conno2 <> ls_conno1 THEN
				ls_other_conno2 = ls_conno1
			END IF
				
		END IF
	CASE 3
		ls_med1 = dw_medium.object.mchar_med[1]
		ls_med2 = dw_medium.object.mchar_med[2]
		ls_med3 = dw_medium.object.mchar_med[3]
		
		IF ls_med1 = 'RTB' AND ls_med2='RTB' AND ls_med3 <> 'RTB' THEN
			
			ls_other_conno1 = dw_medium.object.other_media_conno[1]
			ls_other_conno2 = dw_medium.object.other_media_conno[2]
				
			ls_conno1 = dw_medium.object.mchar_conno[1]
			ls_conno2 = dw_medium.object.mchar_conno[2]
				
			IF ls_other_conno1 <> ls_conno2 THEN
				ls_other_conno1 = ls_conno2
			END IF
				
			IF ls_other_conno2 <> ls_conno1 THEN
				ls_other_conno2 = ls_conno1
			END IF

		ELSEIF ls_med1 = 'RTB' AND ls_med2<>'RTB' AND ls_med3 = 'RTB' THEN
			
			ls_other_conno1 = dw_medium.object.other_media_conno[1]
			ls_other_conno3 = dw_medium.object.other_media_conno[3]
				
			ls_conno1 = dw_medium.object.mchar_conno[1]
			ls_conno3 = dw_medium.object.mchar_conno[3]
				
			IF ls_other_conno1 <> ls_conno3 THEN
				ls_other_conno1 = ls_conno3
			END IF
				
			IF ls_other_conno3 <> ls_conno1 THEN
				ls_other_conno3 = ls_conno1
			END IF

		ELSEIF ls_med1 <> 'RTB' AND ls_med2 = 'RTB' AND ls_med3 = 'RTB' THEN
			
			ls_other_conno2 = dw_medium.object.other_media_conno[2]
			ls_other_conno3 = dw_medium.object.other_media_conno[3]
				
			ls_conno2 = dw_medium.object.mchar_conno[2]
			ls_conno3 = dw_medium.object.mchar_conno[3]
				
			IF ls_other_conno2 <> ls_conno3 THEN
				ls_other_conno2 = ls_conno3
			END IF
				
			IF ls_other_conno3 <> ls_conno2 THEN
				ls_other_conno3 = ls_conno2
			END IF
		END IF

END CHOOSE




end subroutine

public function integer of_mediavalidation (long al_row, string as_old_media, string as_new_media);//////////////////////////////////////////////////////////////////////////////////////////////////////////
//
//	Function: of_mediavalidation
// Args: Long al_row
//         string as_old_media ( from the same row)
//        string as_new_media  ( from the same row
//	Description:
//	Change Media functionality validations
//
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//	
//	Revision History
//
//	Developed by 	Date 				Version						Tracking#
//									
// Murali K.			04/16/2008       PICS 2.0 Modifications	 Change Request from NLS
//////////////////////////////////////////////////////////////////////////////////////////////////////////
Integer cnt_ri,rtn,ll_row
Long Li_bkseq
long ll_found
String lconno,lmchar_med,original_media, ls_null, ls_msg,Ls_bkmed,lc_frflag,ls_chno,ls_userid
String Lmed1,Lmed2,Lmed3,Lconno1,Lconno2,Lconno3,Lbkmed1,Lbkmed2,Lbkmed3,Lflashind1, Lflashind2, Lflashind3
Date tday
long ll_rc
int li_loop

//	dw_medium.AcceptText()
	
 Li_bkseq = dw_medium.object.bkseq[al_row]
 Ls_bkmed = dw_medium.object.bkmed[al_row]
 lc_frflag = dw_medium.object.frflag[al_row]
 lconno = dw_medium.Object.mchar_conno[al_row]
	 
 setnull(ls_null)
/////////// First Level Validations
IF NOT(IsNull(Li_bkseq)) THEN
	ls_msg = 'Book number '+ Ls_bkmed+string(Li_bkseq) +' has already been assigned to dw_medium control number, you may not change the media.'
	Messagebox('Error', ls_msg)
	RETURN -1
ELSEIF NOT(IsNull(lc_frflag)) THEN
	ls_msg = 'Control number '+ lconno +' has already been approved in final review, you may only change the media in the final review screen.'
	Messagebox('Error', ls_msg)
	RETURN -1
END IF
//////////////////////////

IF  ( not isnull(as_old_media) AND Len(Trim(as_old_media))  > 0 ) THEN // if it is a new record old media will not have any values
	rtn = MessageBox("Warning",  'Are you sure you want to change the media?',   Exclamation!, YesNo!, 2)
ELSE
	rtn = 1
END IF

IF rtn = 1 THEN
	
	CHOOSE CASE dw_medium.rowcount()
			
		// Item# 2.2 Case 1 Validation BR to RTB single row
		CASE 1
			// Media for al_rows 1 
			Lmed1 = dw_medium.object.mchar_med[1]
			
			// Control numbers for al_row 1
			Lconno1 = dw_medium.object.mchar_conno[1]
			Ls_chno = dw_medium.object.mchar_chno[1]
			
			// If the media for the first al_row is equal to RTB
			IF as_new_media  = 'RTB'  THEN
				// Set the bkmed to RC for the first row
				dw_medium.object.bkmed[1] = 'RC'
				// create the second al_row
				ll_row = dw_medium.Insertrow(0)
				// Set the media to RTB
				dw_medium.object.mchar_med[ll_row] = 'RTB'
				// Set the bkmed to DB for the second al_row
				dw_medium.object.bkmed[ll_row] = 'DB'
				// Set the other conno number
				dw_medium.object.other_media_conno[ll_row] = Lconno1
				dw_medium.object.mchar_chno[ll_row]=ls_chno
						
				// 01/30/2008 set created_by and date for new al_row
				dw_medium.object.created_by[ll_row]=gnv_app.of_getuserid()
				dw_medium.object.create_date[ll_row]=today()
				dw_medium.object.modified_by[ll_row]=gnv_app.of_getuserid()
				dw_medium.object.update_date[ll_row]=today()
				
			ELSEIF as_new_media <> 'RTB' THEN
				// Set the bkmed to BR for the first al_row
				dw_medium.object.bkmed[1] = 'BR'				
				//04/22/2008
				dw_medium.object.other_media_conno[1] = ls_null				
				dw_medium.object.flash_indicator[1] = ls_null
			END IF


		CASE 2
			// Media and book media for al_rows 1 an 2
			Lmed1 = dw_medium.object.mchar_med[1]
			Lmed2 = dw_medium.object.mchar_med[2]
			
			Lbkmed1 = dw_medium.object.bkmed[1]
			Lbkmed2 = dw_medium.object.bkmed[2]
			Ls_chno = dw_medium.object.mchar_chno[1]
			
			// Item# 2.3 Case 2
			IF as_old_media = 'RTB'  AND (  as_new_media = 'BR'  OR  as_new_media = 'P/B' ) THEN
				// Set the bkmed to RC for the first al_row
				dw_medium.accepttext()
				dw_medium.object.bkmed[al_row] = 'BR'
				// 04/22/2008 reset other conno and flash indicator if it is BR
				dw_medium.object.other_media_conno[al_row] = ls_null				
				dw_medium.object.flash_indicator[al_row] =ls_null
	
				ll_rc = dw_medium.RowCount()
				FOR li_loop = 1 TO ll_rc
					IF al_row <> li_loop THEN
						Lconno1 = dw_medium.object.mchar_conno[li_loop] 
						dw_medium.deleterow(li_loop) 
						// DELETE CORRESPONDING REISSUE RECORDS
						ll_found = dw_ri.Find("ri_conno = '" + string(lconno1) + "'", 1, dw_ri.RowCount())
						IF ll_found > 0 THEN
							dw_ri.deleterow(ll_found)
						END IF
					END IF
				NEXT
				
			END IF
			
			// Item# 2.4 Case 3  Modify BR only to BR and RTB
			IF as_new_media = 'RTB'  THEN // AND ( Isnull(as_old_media)  OR Len(Trim(as_old_media)) = 0 ) THEN
				// Set the bkmed to RC for the first al_row
				dw_medium.object.bkmed[al_row] = 'RC'
				// create the second al_row
				ll_row = dw_medium.Insertrow(0)
				// Set the media to RTB
				dw_medium.object.mchar_med[ll_row] = 'RTB'
				// Set the bkmed to DB for the second al_row
				dw_medium.object.bkmed[ll_row] = 'DB'
				// Set the other conno number
				dw_medium.object.other_media_conno[ll_row] = Lconno1
				dw_medium.object.mchar_chno[ll_row]=ls_chno
				
				// 01/30/2008 set created_by and date for new al_row
				dw_medium.object.created_by[ll_row]=gnv_app.of_getuserid()
				dw_medium.object.create_date[ll_row]=today()
				dw_medium.object.modified_by[ll_row]=gnv_app.of_getuserid()
				dw_medium.object.update_date[ll_row]=today()
			
			END IF
			

		CASE 3
			
			// Media and book media for al_rows 1 an 2
			Lmed1 = dw_medium.object.mchar_med[1]
			Lmed2 = dw_medium.object.mchar_med[2]
			
			Lbkmed1 = dw_medium.object.bkmed[1]
			Lbkmed2 = dw_medium.object.bkmed[2]
			Ls_chno = dw_medium.object.mchar_chno[1]
			
			// Item# 2.3 Case 2
			IF as_old_media = 'RTB'  AND (  as_new_media = 'BR'  OR  as_new_media = 'P/B' ) THEN
				// Set the bkmed to RC for the first al_row
				dw_medium.accepttext()
				dw_medium.object.bkmed[al_row] = 'BR'
				// 04/22/2008 reset other conno and flash indicator if it is BR
				dw_medium.object.other_media_conno[al_row] = ls_null				
				dw_medium.object.flash_indicator[al_row] = ls_null
				
				ll_rc = dw_medium.RowCount()
				FOR li_loop = 1 TO ll_rc
					IF li_loop > dw_medium.rowcount() THEN
						EXIT
					END IF
					IF al_row <> li_loop THEN
						Lconno1 = dw_medium.object.mchar_conno[li_loop] 
						ls_bkmed = dw_medium.object.bkmed[li_loop] 
						IF ls_bkmed <> 'BR' THEN
							dw_medium.deleterow(li_loop) 
							// DELETE CORRESPONDING REISSUE RECORDS
							ll_found = dw_ri.Find("ri_conno = '" + string(lconno1) + "'", 1, dw_ri.RowCount())
							IF ll_found > 0 THEN
								dw_ri.deleterow(ll_found)
							END IF
						END IF
					END IF
				NEXT
				
			END IF
			
			// Item# 2.4 Case 3  Modify BR only to BR and RTB
			IF as_new_media = 'RTB'  THEN // AND ( Isnull(as_old_media)  OR Len(Trim(as_old_media)) = 0 ) THEN
				// Set the bkmed to RC for the first al_row
				dw_medium.object.bkmed[al_row] = 'RC'
				// create the second al_row
				ll_row = dw_medium.Insertrow(0)
				// Set the media to RTB
				dw_medium.object.mchar_med[ll_row] = 'RTB'
				// Set the bkmed to DB for the second al_row
				dw_medium.object.bkmed[ll_row] = 'DB'
				// Set the other conno number
				dw_medium.object.other_media_conno[ll_row] = Lconno1
				dw_medium.object.mchar_chno[ll_row]=ls_chno
				
				// 01/30/2008 set created_by and date for new al_row
				dw_medium.object.created_by[ll_row]=gnv_app.of_getuserid()
				dw_medium.object.create_date[ll_row]=today()
				dw_medium.object.modified_by[ll_row]=gnv_app.of_getuserid()
				dw_medium.object.update_date[ll_row]=today()
			
			END IF
			

				
	END CHOOSE
ELSE
	RETURN -1
END IF // RTN = 1

RETURN 1
end function

public function integer wf_set_bestseller ();//////////////////////////////////////////////////////////////////////////////////////////////////////////
//
//	Function: wf_set_bestseller
// Args: 
//	Description:
//	Since best seller was a checkbox the toggle between Y or N values
// was  not happening right, so this function is posted to perform the required action.
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//	
//	Revision History
//
//	Developed by 	Date 				Version						Tracking#
//									
// Murali K.			09/30/2008       PICS 2.0 Modifications	 2090
//////////////////////////////////////////////////////////////////////////////////////////////////////////

anno_tags_altered = TRUE
dw_annotation.AcceptText()
mle_tags.text = LeftTrim(wf_sex_viol_lang())
wf_get_wordcount()
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

public function integer wf_clear_label_fields (boolean ab_visible);// 11/03/2009 DB Labels Project
//cbx_1.visible=ab_visible
//cbx_2.visible=ab_visible
//cbx_3.visible=ab_visible
//cb_1.visible=ab_visible
//st_3.visible=ab_visible
//st_4.visible=ab_visible
//st_5.visible=ab_visible
//
// 11/03/2009 DB Labels Project
string ls_bkmed, lconno=''


// validate conno
Lconno = em_conno.text //dw_annotation.object.conno[1]
select bkmed  into :ls_bkmed from mchar where conno = :lconno using sqlservertrans ;

IF ls_bkmed = 'DB' THEN
	cb_1.visible=ab_visible
	cbx_1.visible=ab_visible
	cbx_2.visible=ab_visible
	cbx_3.visible=ab_visible
	st_3.visible=ab_visible
	st_4.visible=ab_visible
	st_5.visible=ab_visible
ELSE
	cb_1.visible=FALSE
	cbx_1.visible=FALSE
	cbx_2.visible=FALSE
	cbx_3.visible=FALSE
	st_3.visible=FALSE
	st_4.visible=FALSE
	st_5.visible=FALSE
END IF


RETURN 1


//	case "Add"
//		anno_stage=1
//		w_sheet_annotation.Title = "Add Annotation"
//	case "CDS"
//		anno_stage=2
//		w_sheet_annotation.Title = "CDS Edit Annotation"
//	case "PMS"
//		anno_stage=3
//		w_sheet_annotation.Title = "PMS Edit Annotation"
//	case "PCS"
//		anno_stage=6
//		w_sheet_annotation.Title = "PCS Edit Annotation"
//	CASE "Spin"
//		anno_stage=4
//		w_sheet_annotation.Title = "CDS Late Spinoff"
//	case "View"
//		anno_stage=5
//		w_sheet_annotation.Title = "View/Print Annotation"
		

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

event open;call super::open;Boolean invalid_userid=FALSE
string ls_userid, ls_group, ls_msg

//first catch the case whether the user is admin, if is admin there is no need check user belong CDS OR PMS OR PCS
// GROUP
ls_userid=sqlservertrans.userid
select group_  into :ls_group
from picsuser
where userid =:ls_userid
using sqlservertrans;
if f_check_dberror(sqlservertrans, 'find group that userid is associated')=false then
	return
end if
this.windowstate = maximized!
lnvo_val_anno = CREATE nvo_val_init_con
ls_msg=Message.StringParm	
ls_menu=ls_msg
// Open the sheet in Maximized mode
choose case ls_msg
	case "Add"
		anno_stage=1
		w_sheet_annotation.Title = "Add Annotation"
	case "CDS"
		anno_stage=2
		w_sheet_annotation.Title = "CDS Edit Annotation"
	case "PMS"
		anno_stage=3
		w_sheet_annotation.Title = "PMS Edit Annotation"
	case "PCS"
		anno_stage=6
		w_sheet_annotation.Title = "PCS Edit Annotation"
	CASE "Spin"
		anno_stage=4
		w_sheet_annotation.Title = "CDS Late Spinoff"
	case "View"
		anno_stage=5
		w_sheet_annotation.Title = "View/Print Annotation"
end choose


if ls_group='ADMIN' THEN // IF user is admin then no need check CDS, PMS, PCS GROUP AT ALL
//	return
else 
	CHOOSE CASE Message.StringParm	
		CASE "Add"
			w_sheet_annotation.Title = "Add Annotation"
			anno_stage=1
		CASE "CDS"
			IF wf_validate_group_access(sqlservertrans.userid,"CDS")=FALSE THEN
				invalid_userid = TRUE
			ELSE
				invalid_userid = FALSE
				w_sheet_annotation.Title = "CDS Edit Annotation"
				anno_stage=2
			END IF
		CASE "PMS"
			IF wf_validate_group_access(sqlservertrans.userid,"PMS")=FALSE THEN
				invalid_userid = TRUE
			ELSE
				invalid_userid = FALSE
				w_sheet_annotation.Title = "PMS Edit Annotation"
				anno_stage=3
			END IF
		CASE "PCS"
			IF wf_validate_group_access(sqlservertrans.userid,"PCS")=FALSE THEN
				invalid_userid = TRUE
			ELSE
				invalid_userid = FALSE
				w_sheet_annotation.Title = "PCS Edit Annotation"
				anno_stage=6
			END IF
		CASE "Spin"
			IF wf_validate_group_access(sqlservertrans.userid,"CDS")=FALSE THEN
				invalid_userid = TRUE
			ELSE
				invalid_userid = FALSE
				w_sheet_annotation.Title = "CDS Late Spinoff"
				anno_stage=4
			END IF
		CASE "View"
			invalid_userid = FALSE
			w_sheet_annotation.Title = "View/Print Annotation"
			cb_find.Default = TRUE
			anno_stage=5
	END CHOOSE
end if
// This varaible is used for spellchecker
what_text = ""

IF Invalid_userid = TRUE THEN
	close(this)
//	m_pics_main.m_menu.m_confirmconsideration.PopMenu(w_pics_main.PointerX(), w_pics_main.PointerY())
ELSE	
	wf_disable_buttons()
	em_conno.setfocus()
	m_pics_main.m_file.m_print.Enabled 			=	TRUE
	m_pics_main.m_file.m_pagesetup.Enabled		=	TRUE
	m_pics_main.m_file.m_printimmediate.Enabled	=	TRUE
END IF
end event

on w_sheet_annotation.create
int iCurrent
call super::create
this.cb_1=create cb_1
this.cbx_3=create cbx_3
this.st_5=create st_5
this.cbx_2=create cbx_2
this.st_4=create st_4
this.cbx_1=create cbx_1
this.st_3=create st_3
this.mle_tags=create mle_tags
this.em_bkseq=create em_bkseq
this.st_or=create st_or
this.st_bkno=create st_bkno
this.cb_bknav=create cb_bknav
this.dw_cdssubject_find=create dw_cdssubject_find
this.dw_ri=create dw_ri
this.cb_fanno=create cb_fanno
this.cb_lang=create cb_lang
this.st_1=create st_1
this.em_conno=create em_conno
this.st_2=create st_2
this.cb_find=create cb_find
this.cb_update=create cb_update
this.cb_clear=create cb_clear
this.cb_exit=create cb_exit
this.mle_anno=create mle_anno
this.cb_spell=create cb_spell
this.dw_annotation=create dw_annotation
this.dw_medium=create dw_medium
this.dw_coauthors=create dw_coauthors
this.cb_print=create cb_print
this.st_wordcnt=create st_wordcnt
this.cb_zoom=create cb_zoom
this.dw_cc_pms_par_report=create dw_cc_pms_par_report
this.dw_cc_anno_rpt=create dw_cc_anno_rpt
this.dw_cdssubject=create dw_cdssubject
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.cb_1
this.Control[iCurrent+2]=this.cbx_3
this.Control[iCurrent+3]=this.st_5
this.Control[iCurrent+4]=this.cbx_2
this.Control[iCurrent+5]=this.st_4
this.Control[iCurrent+6]=this.cbx_1
this.Control[iCurrent+7]=this.st_3
this.Control[iCurrent+8]=this.mle_tags
this.Control[iCurrent+9]=this.em_bkseq
this.Control[iCurrent+10]=this.st_or
this.Control[iCurrent+11]=this.st_bkno
this.Control[iCurrent+12]=this.cb_bknav
this.Control[iCurrent+13]=this.dw_cdssubject_find
this.Control[iCurrent+14]=this.dw_ri
this.Control[iCurrent+15]=this.cb_fanno
this.Control[iCurrent+16]=this.cb_lang
this.Control[iCurrent+17]=this.st_1
this.Control[iCurrent+18]=this.em_conno
this.Control[iCurrent+19]=this.st_2
this.Control[iCurrent+20]=this.cb_find
this.Control[iCurrent+21]=this.cb_update
this.Control[iCurrent+22]=this.cb_clear
this.Control[iCurrent+23]=this.cb_exit
this.Control[iCurrent+24]=this.mle_anno
this.Control[iCurrent+25]=this.cb_spell
this.Control[iCurrent+26]=this.dw_annotation
this.Control[iCurrent+27]=this.dw_medium
this.Control[iCurrent+28]=this.dw_coauthors
this.Control[iCurrent+29]=this.cb_print
this.Control[iCurrent+30]=this.st_wordcnt
this.Control[iCurrent+31]=this.cb_zoom
this.Control[iCurrent+32]=this.dw_cc_pms_par_report
this.Control[iCurrent+33]=this.dw_cc_anno_rpt
this.Control[iCurrent+34]=this.dw_cdssubject
end on

on w_sheet_annotation.destroy
call super::destroy
destroy(this.cb_1)
destroy(this.cbx_3)
destroy(this.st_5)
destroy(this.cbx_2)
destroy(this.st_4)
destroy(this.cbx_1)
destroy(this.st_3)
destroy(this.mle_tags)
destroy(this.em_bkseq)
destroy(this.st_or)
destroy(this.st_bkno)
destroy(this.cb_bknav)
destroy(this.dw_cdssubject_find)
destroy(this.dw_ri)
destroy(this.cb_fanno)
destroy(this.cb_lang)
destroy(this.st_1)
destroy(this.em_conno)
destroy(this.st_2)
destroy(this.cb_find)
destroy(this.cb_update)
destroy(this.cb_clear)
destroy(this.cb_exit)
destroy(this.mle_anno)
destroy(this.cb_spell)
destroy(this.dw_annotation)
destroy(this.dw_medium)
destroy(this.dw_coauthors)
destroy(this.cb_print)
destroy(this.st_wordcnt)
destroy(this.cb_zoom)
destroy(this.dw_cc_pms_par_report)
destroy(this.dw_cc_anno_rpt)
destroy(this.dw_cdssubject)
end on

event pfc_preopen;call super::pfc_preopen;this.of_SetPreference(TRUE)
this.inv_preference.of_SetToolBars(TRUE)
this.inv_preference.of_SetWindow(TRUE)
this.of_SetResize(TRUE)
this.inv_resize.of_SetOrigSize(this.workspacewidth(),this.workspaceheight())
inv_resize.of_Register(dw_annotation, "Scale")
inv_resize.of_Register(dw_coauthors, "Scale")
inv_resize.of_Register(dw_medium, "Scale")
inv_resize.of_Register(dw_cdssubject, "Scale")
inv_resize.of_Register(dw_cdssubject_find, "Scale")
inv_resize.of_Register(dw_ri, "Scale")
inv_resize.of_Register(em_conno, "Scale")
inv_resize.of_Register(em_bkseq, "Scale")
inv_resize.of_Register(cb_clear, "Scale")
inv_resize.of_Register(cb_update, "Scale")
inv_resize.of_Register(cb_exit, "Scale")
inv_resize.of_Register(cb_find, "Scale")
inv_resize.of_Register(mle_anno,"Scale")
inv_resize.of_Register(mle_tags,"Scale")
inv_resize.of_Register(st_1, "Scale")
inv_resize.of_Register(st_2, "Scale")
inv_resize.of_Register(st_or, "Scale")
inv_resize.of_Register(st_bkno, "Scale")
inv_resize.of_Register(cb_spell, "Scale")
inv_resize.of_Register(cb_lang, "Scale")
inv_resize.of_Register(cb_zoom, "Scale")
inv_resize.of_Register(cb_print, "Scale")
inv_resize.of_Register(cb_fanno, "Scale")
inv_resize.of_Register(cb_bknav, "Scale")
inv_resize.of_Register(st_wordcnt, "Scale")


end event

event resize;call super::resize;long ll_height

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

IF	anno_stage=5 THEN
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

event pfc_postopen;call super::pfc_postopen;// 11/03/2009 DB Labels Project
wf_clear_label_fields(FALSE)
end event

type cb_1 from commandbutton within w_sheet_annotation
integer x = 2555
integer y = 996
integer width = 370
integer height = 96
integer taborder = 100
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
ls_conno = em_conno.text
IF Isnull(ls_conno) OR Len(Trim(ls_conno)) = 0 THEN
	RETURN
ELSE
	li_ret = f_call_label_app(ls_conno)
END IF

end event

type cbx_3 from checkbox within w_sheet_annotation
integer x = 2409
integer y = 1016
integer width = 69
integer height = 72
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

type st_5 from statictext within w_sheet_annotation
integer x = 1879
integer y = 1016
integer width = 521
integer height = 100
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

type cbx_2 from checkbox within w_sheet_annotation
integer x = 1769
integer y = 1016
integer width = 69
integer height = 72
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

type st_4 from statictext within w_sheet_annotation
integer x = 1266
integer y = 1016
integer width = 507
integer height = 52
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

type cbx_1 from checkbox within w_sheet_annotation
integer x = 1152
integer y = 1016
integer width = 69
integer height = 72
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

type st_3 from statictext within w_sheet_annotation
integer x = 699
integer y = 1016
integer width = 439
integer height = 52
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

type mle_tags from u_mle within w_sheet_annotation
event pfc_hinttext pbm_mousemove
event ue_char_entered pbm_char
event ue_key_up pbm_keyup
event ue_lmouse_up pbm_lbuttonup
string tag = "Annotation tags"
integer y = 1408
integer width = 1097
integer height = 160
integer taborder = 90
integer textsize = -9
fontfamily fontfamily = roman!
string facename = "Times New Roman"
boolean vscrollbar = true
end type

event pfc_hinttext;call super::pfc_hinttext;w_pics_main.setmicrohelp(mle_anno.tag)

end event

event ue_char_entered;call super::ue_char_entered;if ib_anno_modified = false then
	ib_anno_modified = true
end if

end event

event ue_key_up;call super::ue_key_up;wf_count_words()

end event

event ue_lmouse_up;call super::ue_lmouse_up;wf_count_words()


end event

event getfocus;call super::getfocus;m_pics_main.m_edit.m_deleterow.Enabled 	=	FALSE
m_pics_main.m_edit.m_addrow.Enabled 		=	FALSE

end event

event modified;call super::modified;ib_anno_modified=TRUE
end event

type em_bkseq from u_em within w_sheet_annotation
integer x = 37
integer y = 324
integer width = 297
integer height = 84
integer taborder = 20
integer weight = 700
fontcharset fontcharset = ansi!
long textcolor = 16711680
string mask = "########"
boolean autoskip = true
string displaydata = "Ä"
double increment = 0
string minmax = ""
end type

type st_or from statictext within w_sheet_annotation
integer x = 37
integer y = 188
integer width = 183
integer height = 60
integer textsize = -8
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 33554432
long backcolor = 80263581
string text = "Or"
boolean focusrectangle = false
end type

type st_bkno from u_st within w_sheet_annotation
integer x = 37
integer y = 256
integer width = 366
integer height = 88
fontcharset fontcharset = ansi!
string text = "Book Number"
end type

type cb_bknav from commandbutton within w_sheet_annotation
integer x = 1280
integer y = 1600
integer width = 352
integer height = 96
integer textsize = -8
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "Book Na&v."
end type

event clicked;String Lconno

Lconno = em_conno.text

//MessageBox("control number",lconno)
OpenSheetWithParm(w_book_navigation, Lconno,w_pics_main, 0, Original!)

end event

type dw_cdssubject_find from u_pics_dw within w_sheet_annotation
boolean visible = false
integer x = 183
integer y = 672
integer width = 64
integer height = 64
integer taborder = 0
string dataobject = "d_cdssubject"
boolean vscrollbar = false
boolean livescroll = false
end type

event itemchanged;call super::itemchanged;string lcdssubcode, ls_subject
if dwo.name='cdssubject' then
	ls_subject=data
	lcdssubcode = ldwc_cdssub.GetItemstring(ldwc_cdssub.GetRow(),"cdssubjectcode")
	//Messagebox("code",lcdssubcode)
	this.object.cdssubjectcode[row] = lcdssubcode
end if

end event

event type long pfc_insertrow();call super::pfc_insertrow;long	ll_currow
long	ll_rc
int i

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
	dw_cdssubject.object.chno[ll_rc]=Lchno
end if

// Notify the Linkage Service that a new row has been added.
IF IsValid ( inv_Linkage ) THEN 
	inv_Linkage.Event pfc_InsertRow (ll_rc) 
END IF 

for i = 1 to dw_cdssubject.rowcount()
	if dw_cdssubject.object.chno[i]="" or &
		isnull(dw_cdssubject.object.chno[i]) then
		dw_cdssubject.deleterow(i)
	end if
next

this.ScrollToRow(ll_rc)
this.SetColumn(1)

return ll_rc
end event

event pfc_addrow;call super::pfc_addrow;long	ll_rc
int i

// Insert row
if IsValid (inv_rowmanager) then
	ll_rc = inv_rowmanager.event pfc_addrow ()
else
	ll_rc = this.InsertRow (0) 
	dw_cdssubject.SetItem(ll_rc, "chno", Lchno)
end if


// Notify the Linkage Service that a new row has been added.
IF IsValid ( inv_Linkage ) THEN 
	inv_Linkage.Event pfc_InsertRow (ll_rc) 
END IF 

for i = 1 to dw_cdssubject.rowcount()
	if dw_cdssubject.object.chno[i]="" or &
		isnull(dw_cdssubject.object.chno[i]) then
		dw_cdssubject.deleterow(i)
	end if
next

this.ScrollToRow(ll_rc)
this.SetColumn(1)

return ll_rc
end event

event ue_postconstructor;call super::ue_postconstructor;long rtncode
this.of_SetLinkage(TRUE)
this.of_SetTransObject( SQLServerTrans )
this.of_SetDropDownSearch(TRUE)
this.inv_dropdownsearch.of_AddColumn("cdssubject")
rtncode = this.GetChild('cdssubject', ldwc_cdssub)
IF rtncode = -1 THEN 
	MessageBox( "Error", "Not a DataWindowChild")
END IF
//messagebox('rows',string(rtncode))
	
end event

type dw_ri from u_pics_dw within w_sheet_annotation
event ue_enterkey pbm_dwnprocessenter
event pfc_hinttext pbm_mousemove
integer x = 2569
integer y = 884
integer width = 722
integer height = 304
integer taborder = 60
string dataobject = "d_ri_confm"
boolean hscrollbar = true
boolean ib_rmbfocuschange = false
string is_updatesallowed = "1"
end type

event ue_enterkey;call super::ue_enterkey;Send(Handle(this),256,9,Long(0,0))
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

event getfocus;call super::getfocus;m_pics_main.m_edit.m_deleterow.Enabled 	=	TRUE
m_pics_main.m_edit.m_addrow.Enabled 		=	TRUE

end event

event itemchanged;call super::itemchanged;int cnt_ri,rtn, li_cnt, media_rows
long li_bkseq, ll_foundrow
string ls_chno, ls_ttl, ls_auth, ls_authfn, ls_bkmed, ls_msg
string lconno,lmchar_med,lmchar_med2,original_media

IF dwo.Name = "ri_prevbkmed" THEN
	media_rows = dw_medium.rowcount()
	IF media_rows = 1 THEN
		lmchar_med = TRIM(dw_medium.Object.mchar_med[1])
	ELSEIF media_rows > 1 THEN
		lmchar_med = TRIM(dw_medium.Object.mchar_med[1])
		lmchar_med2 = TRIM(dw_medium.Object.mchar_med[2])
	ELSEIF media_rows = 0 THEN
		this.Object.ri_prevbkmed.ValidationMsg='Media has not been assigned in the original book.'
		RETURN 1
	END IF	
		
	original_media = TRIM(data)
	IF original_media<>"" THEN
		// If you choose a media for the previouse book number
		// 04/17/2008 add DB book media to the condition
		IF original_media<>'RC' AND original_media<>'DB' AND  original_media<>'FD' AND original_media<>'BR' AND original_media<>'BRA' AND &
			original_media<>'P/B' AND original_media<>'CB' AND original_media<>'RD' AND original_media<>'TB' THEN
			RETURN 1
		ELSE
			dw_ri.Object.ri_prevbkseq.tabsequence='50'
			//dw_medium.Object.ri_prevbkseq.EditMask.Required='Yes'
			dw_medium.Object.mchar_ricd.tabsequence='60'
			//dw_medium.Object.mchar_ricd.Edit.Required='Yes'
			
			IF dw_ri.RowCount() = 1 THEN
				CHOOSE CASE lmchar_med
				CASE 'RC'
					IF original_media<>'RC' AND &
						original_media<>'FD' AND &
						original_media<>'RD' AND &
						original_media<>'TB' AND &
						original_media<>'CB' THEN
						IF media_rows > 1 THEN
							IF original_media<>lmchar_med2 THEN
								this.Object.ri_prevbkmed.ValidationMsg='Previouse book media must be of the same type as the original media.'
								RETURN 1
							END IF
						ELSE
							this.Object.ri_prevbkmed.ValidationMsg='Previouse book media must be of the same type as the original media.'
							RETURN 1
						END IF	
					END IF
				// 04/17/2008 added DB bookmedia condition
				CASE 'DB'
					IF original_media<>'DB' AND &
						original_media<>'RC' AND &
						original_media<>'FD' AND &
						original_media<>'RD' AND &
						original_media<>'TB' AND &
						original_media<>'CB' THEN
						IF media_rows > 1 THEN
							IF original_media<>lmchar_med2 THEN
								this.Object.ri_prevbkmed.ValidationMsg='Previouse book media must be of the same type as the original media.'
								RETURN 1
							END IF
						ELSE
							this.Object.ri_prevbkmed.ValidationMsg='Previouse book media must be of the same type as the original media.'
							RETURN 1
						END IF	
					END IF
					
				CASE 'BR'
					IF original_media<>'BR' AND &
						original_media<>'BRA' AND &
						original_media<>'P/B' THEN
						IF media_rows > 1 THEN
							IF original_media<>lmchar_med2 THEN
								this.Object.ri_prevbkmed.ValidationMsg='Previouse book media must be of the same type as the original media.'
								RETURN 1
							END IF
						ELSE
							this.Object.ri_prevbkmed.ValidationMsg='Previouse book media must be of the same type as the original media.'
							RETURN 1
						END IF	
					END IF
				END CHOOSE
			END IF

		END IF
	ELSE
		// If you want to delete the existing previouse book number
		// or there are no previouse book number existed.
		rtn = MessageBox("Warning","Do you want to remove the previous book number?",Question!,YesNo!,1)
		if rtn = 1 THEN		
			dw_ri.Object.ri_prevbkseq.tabsequence='0'
			dw_medium.Object.mchar_ricd.tabsequence='0'
			//dw_medium.Object.ri_prevbkseq.EditMask.Required='No'
			//dw_medium.Object.mchar_ricd.Edit.Required='No'
			dw_ri.Object.ri_prevbkseq[row]=0
			IF dw_medium.rowcount() > 0 THEN
				dw_medium.object.mchar_ricd[row]=""
			END IF
			lconno = dw_medium.Object.mchar_conno[row]
			select count(*) into :cnt_ri from ri where conno = :lconno using sqlservertrans;
			IF f_check_dberror(sqlservertrans,"RI") THEN
				IF cnt_ri > 0 THEN
					DELETE FROM ri WHERE conno=:lconno USING SqlServerTrans;
					IF f_check_dberror(sqlservertrans,"RI") THEN
						commit using sqlservertrans;
						dw_ri.Reset()
					ELSE
						rollback using sqlservertrans;
						return
					END IF
				END IF
			ELSE
				return
			END IF
		else
			RETURN 2
		END IF
	END IF
ELSEIF dwo.Name = "ri_prevbkseq" THEN
	if IsNumber(data) then
		li_bkseq=long(data)
		ls_bkmed=this.object.ri_prevbkmed[row]
		select count(*) into :li_cnt
		from mchar
		where bkseq=:li_bkseq and bkmed=:ls_bkmed
		using sqlservertrans;
		IF not f_check_dberror(sqlservertrans,"select li_cnt from mchar") THEN
			return
		end if
		if li_cnt>0 then
			ls_msg=trim(ls_bkmed)+';'+string(li_bkseq)
			openwithParm(w_show_ttl_auth_response,ls_msg)
		end if// end of li_cnt>0
	end if//end of isnumber(data)

	//11/24/2008 tracker item 2122
ELSEIF DWO.NAME = 'mchar_ricd' THEN
	lconno = this.object.ri_conno[row]
	IF Not Isnull(lconno) THEN
		ll_foundrow = dw_medium.Find( "mchar_conno = '" + lconno+ "'", 1, dw_medium.rowcount())
		IF ll_foundrow > 0 THEN
			dw_medium.object.mchar_ricd[ll_foundrow] = data
		END IF
	END IF
end if//dwo.name=mchar_ricd
end event

event sqlpreview;call super::sqlpreview;//
end event

event pfc_deleterow;integer	li_rc

string lconno, ls_null
long ll_foundrow

//11/24/2008 if ri row is deleted reset ricd in mchar Tracker Item 2122
	lconno = this.object.ri_conno[this.getrow()]
	IF Not Isnull(lconno) THEN
		ll_foundrow = dw_medium.Find( "mchar_conno = '" + lconno+ "'", 1, dw_medium.rowcount())
		IF ll_foundrow > 0 THEN
			setnull(ls_null)
			dw_medium.object.mchar_ricd[ll_foundrow] = ls_null
		END IF
	END IF
/////


if IsValid (inv_rowmanager) then
	li_rc = inv_rowmanager.event pfc_deleterow () 
else	
	li_rc = this.DeleteRow (0) 
end if

// Notify the Linkage Service 
IF IsValid ( inv_Linkage ) THEN 
	If li_rc > 0 Then inv_Linkage.Event pfc_DeleteRow (0) 
END IF 

//dw_medium.SetItemStatus(1, 0, Primary!, DataModified!)




return li_rc

end event

event type long pfc_insertrow();long	ll_currow
long	ll_rc

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
	dw_ri.object.ri_conno[ll_rc]=Lchno
	dw_ri.object.ri_prevbkmed[ll_rc]=""
end if

// Notify the Linkage Service that a new row has been added.
IF IsValid ( inv_Linkage ) THEN 
	inv_Linkage.Event pfc_InsertRow (ll_rc) 
END IF 

this.ScrollToRow(ll_rc)
this.SetColumn(1)

return ll_rc
end event

event type long pfc_addrow();long	ll_rc
int   ll_currow

// Insert row
if IsValid (inv_rowmanager) then
	ll_rc = inv_rowmanager.event pfc_addrow ()
else
	ll_rc = this.InsertRow (0) 
	dw_ri.object.ri_conno[ll_rc]=is_conno
	dw_ri.object.ri_prevbkmed[ll_rc]=''
end if

// Notify the Linkage Service that a new row has been added.
IF IsValid ( inv_Linkage ) THEN 
	inv_Linkage.Event pfc_InsertRow (ll_rc) 
END IF 

this.ScrollToRow(ll_rc)
this.SetColumn(1)

return ll_rc
end event

event ue_postconstructor;call super::ue_postconstructor;this.of_SetTransObject(SQLServerTrans)

end event

event constructor;call super::constructor;this.SetTransObject(SqlServerTrans)
end event

type cb_fanno from commandbutton within w_sheet_annotation
integer x = 914
integer y = 1600
integer width = 334
integer height = 96
integer textsize = -8
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "Foreign Ann&o"
end type

event clicked;String Lchart_no

Lchart_no = dw_annotation.object.ttlinit_chno[1]

//IF dw_annotation.object.ttlinit_lang[1]='ENG' THEN
//	MessageBox("ERROR","This book is not in foreign language.")
//ELSE
	OpenWithParm(w_anno_foreign, Lchart_no)
//END IF
end event

type cb_lang from commandbutton within w_sheet_annotation
integer x = 1659
integer y = 1600
integer width = 293
integer height = 96
integer textsize = -8
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "Lan&guage"
end type

event clicked;open(w_cc_lang)
end event

type st_1 from statictext within w_sheet_annotation
integer x = 37
integer y = 20
integer width = 370
integer height = 76
boolean bringtotop = true
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long backcolor = 79741120
boolean enabled = false
string text = "Control Number"
boolean focusrectangle = false
end type

type em_conno from uo_conno within w_sheet_annotation
integer x = 37
integer y = 96
integer width = 334
integer height = 84
integer taborder = 10
long textcolor = 255
maskdatatype maskdatatype = stringmask!
string displaydata = "Ä"
end type

type st_2 from statictext within w_sheet_annotation
integer x = 5
integer y = 892
integer width = 1088
integer height = 64
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long backcolor = 79741120
boolean enabled = false
string text = "A n n o t a t i o n"
alignment alignment = center!
boolean focusrectangle = false
end type

type cb_find from commandbutton within w_sheet_annotation
event clicked pbm_bnclicked
string tag = "Find the record"
integer x = 2190
integer y = 1600
integer width = 224
integer height = 96
boolean bringtotop = true
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "F&ind"
boolean default = true
end type

event clicked;long ll_rows,Lbkseq,Li_bkseq
integer i,row_cntr,rtn
string Lconno,Lcdsinit,Lannoinit,Lpmsinit,Lpcsinit,Lajyfn,annotxt,ls_priority,ls_lcno,Lspecattn,titletxt, &
		ls_usid, ls_group,ls_filter
		
n_cst_string 	inv_string

IF NOT(IsNull(em_bkseq.text)) AND LONG(em_bkseq.text) <> 0 THEN
	SetNull(Lconno)
	Li_bkseq = LONG(em_bkseq.text)
   select distinct conno 
	into :Lconno
	from mchar
	where bkseq =:Li_bkseq
	using sqlservertrans;
	IF NOT(ISNULL(Lconno)) THEN
		em_conno.text = Lconno
	ELSE
		MessageBox("ERROR","Book number was not found in database.")
		em_bkseq.SetFocus()
		return
	END IF
ELSE
	SetNull(em_bkseq.text)
END IF

ls_usid=sqlservertrans.userid
select group_ into :ls_group
from picsuser
WHere userid=:ls_usid
using sqlservertrans;
if f_check_dberror(sqlservertrans, 'find the group associate with the user')=false then
  return
end if
Lconno = em_conno.text
IF Lconno <> "" THEN
	IF f_is_it_archived(Lconno,0) THEN
		em_conno.SetFocus()
		em_bkseq.text=''
		RETURN
	END IF
	ll_rows = dw_annotation.Retrieve(Lconno)
  	IF ll_rows < 1 THEN
      MessageBox("Error", "Control Number: "+Lconno+" Does not exist. ~n Enter the correct control number." ,Information!, OK!, 2)
		dw_annotation.insertrow(0)
		em_bkseq.text=''
		em_conno.SetFocus()
	ELSE
			// 09/30/2008 set the book number if retrieved by conno
			select distinct bkseq
			into :Li_bkseq
			from mchar
			where conno  =:Lconno
			using sqlservertrans;
			IF NOT(ISNULL(Li_bkseq)) THEN
				em_bkseq.text = string(li_bkseq)
			END IF
		/// 09/30/2008
		
      // Get the title and trim out the extra trailing spaces.
		titletxt =  inv_string.of_Trim(dw_annotation.object.ttlinit_ttl[1])
		dw_annotation.object.ttlinit_ttl[1] = titletxt
		// messagebox("title",titletxt+" "+string(len(titletxt)))
		Lspecattn = dw_annotation.object.ttlinit_specattn[1]
		IF IsNull(Lspecattn) OR TRIM(Lspecattn)="" THEN
			dw_annotation.object.ttlinit_specattn[1]="N"
		END IF
		ls_lcno = dw_annotation.object.ttlinit_lcno[1]
		IF ISNULL(ls_lcno) OR ls_lcno = "" THEN
			dw_annotation.object.ttlinit_lcno[1]="00000001"
		END IF
		Lchno	= dw_annotation.object.ttlinit_chno[1]

		// This section of the code validate the annotation stages
		
		Lcdsinit  = dw_annotation.object.ttlinit_cdinit[1]
		Lannoinit = dw_annotation.object.ttlinit_annoinit[1]
		Lpmsinit  = dw_annotation.object.ttlinit_pminit[1]
		Lpcsinit  = dw_annotation.object.ttlinit_pcsinit[1]
		
		
		IF wf_validate_anno_stage(Lconno,Lannoinit,Lcdsinit,Lpmsinit,anno_stage,Lpcsinit) and &
		   ls_group<>'ADMIN' OR ls_group='ADMIN' THEN
			// Retrieve data for medium section
			row_cntr = dw_medium.Retrieve(Lchno)
			FOR i=1 to row_cntr 
				ls_priority = dw_medium.object.mchar_priority[i]
				IF ISNULL(ls_priority) OR TRIM(ls_priority) = "" THEN
					dw_medium.object.mchar_priority[i]="N"
				END IF
			NEXT
			
			// 01/28/2008 settings for commercial audio vendor
			wf_set_comm_audio_vendor('A')
			
			
			// Retrieve data for ri and coauthors
			is_conno= lconno
			dw_ri.Retrieve(Lconno)
			if dw_ri.RowCount()<=0 then
				dw_ri.Event pfc_addrow()
			end if
//			ldwc_cdssub.Settransobject(Sqlservertrans)
//			ldwc_cdssub.Retrieve()
			dw_cdssubject.Retrieve(Lchno)
			dw_cdssubject_find.Retrieve(lchno)

			// Capture the AJYFN for CDS Subject codes retrival datawindow
			Lajyfn  = dw_annotation.object.ttlinit_ajyfn[1]
			
			 // get handle to child dddws
     			dw_cdssubject.GetChild("cdssubject", ldwc_cdssub) 
     			dw_cdssubject.GetChild("cdssubjectcode", ldwc_cdssubcode) 
     			dw_annotation.GetChild("ttlinit_casub", ldwc_casub) 

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

	
			
			dw_coauthors.Retrieve(Lchno)
			// Fill in the annotation
			Select anno into :annotxt from annotation where chno = :Lchno using SqlServerTrans;
			IF SqlServerTrans.SQLCode = 0  THEN
				// annotxt =  inv_string.of_RemoveNonPrint(annotxt)
				annotxt =  inv_string.of_Trim(annotxt)
				annotxt =  inv_string.of_GlobalReplace(annotxt, "       ", " ")
				annotxt =  inv_string.of_GlobalReplace(annotxt, "      ", " ")
				annotxt =  inv_string.of_GlobalReplace(annotxt, "     ", " ")
				annotxt =  inv_string.of_GlobalReplace(annotxt, "    ", " ")
				annotxt =  inv_string.of_GlobalReplace(annotxt, "   ", " ")
				annotxt =  inv_string.of_GlobalReplace(annotxt, "  ", " ")
				mle_anno.text = annotxt
				mle_tags.text = LeftTrim(wf_sex_viol_lang())
				wf_count_words()
				//mle_anno.text = annotxt
				anno_exist=TRUE
			END IF			
			select prevbkseq into :Lbkseq from ri where conno =:Lconno using sqlservertrans;
			IF sqlservertrans.sqlcode = 0 then
				ri_exist=TRUE
			ELSE
				ri_exist=FALSE
			END IF
			IF anno_stage=1 THEN
				IF IsNull(Lannoinit) OR TRIM(Lannoinit)="" THEN
					dw_annotation.object.ttlinit_annoinit[1]=UPPER(sqlservertrans.userid)
				END IF
			ELSEIF anno_stage=2 THEN
				IF IsNull(Lcdsinit) OR TRIM(Lcdsinit)="" THEN
					dw_annotation.object.ttlinit_cdinit[1]=UPPER(sqlservertrans.userid)
				END IF
			ELSEIF anno_stage=3 THEN
				IF IsNull(Lpmsinit) OR TRIM(Lpmsinit)="" THEN
					dw_annotation.object.ttlinit_pminit[1]="P&M"	
				END IF
				dw_ri.enabled=false
			ELSEIF anno_stage=6 THEN
				IF IsNull(Lpcsinit) OR TRIM(Lpcsinit)="" THEN
					dw_annotation.object.ttlinit_pcsinit[1]=UPPER(sqlservertrans.userid)
				END IF
			END IF
			// IF is view only disable all the bottoms
			IF anno_stage=5 THEN
				wf_view_all()
				cb_clear.Enabled = TRUE
			ELSEIF anno_stage=4 THEN
				cb_clear.Enabled = TRUE
				cb_spell.Enabled = FALSE
				cb_find.Enabled = FALSE
				cb_update.Enabled = TRUE
			ELSE
				// Enable the push buttons.
				wf_enable_buttons()
				// set the focus on annoation datawindow.
				dw_annotation.SetFocus()
			END IF
		ELSE
			dw_annotation.Reset()
			dw_annotation.insertrow(0)
			em_conno.SetFocus()
		END IF
  	END IF
	ib_disableclosequery = TRUE
	// 11/03/2009 GET THE LABEL DATA
	wf_get_label_data(lconno)
ELSE
	MessageBox("Error", "Please enter the Control Number." ,StopSign!, OK!, 2)
	SetNull(em_bkseq.text)
	SetNull(em_conno.text)
	em_conno.SetFocus()
END IF
end event

type cb_update from commandbutton within w_sheet_annotation
event clicked pbm_bnclicked
string tag = "Update the record"
integer x = 2455
integer y = 1600
integer width = 251
integer height = 96
boolean bringtotop = true
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "&Update"
end type

event clicked;//////////////////////////////////////////////////////////////////////////////////////////////////////////
//
//	Event: clicked for cb_update
//
//	Description:
//	Set values and update datawindows
//
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//	
//	Revision History
//
//	Developed by 	Date 				Version						Tracking#
//									
// Murali K.			01/28/2008      005 PICS Modifications	 Reqs: CDS. A.7, CDS A.8a.2
//											Added code to process commercial audio switch and 
//											vendor code based on new columns
//						03/24/2008		IF YOU DELETE ALL THE MEDIUM ROWS ERROR OCCURS
// Murali K.			11/24/2008 		 Tracker 2122 validate entry of RI/RR/RT codes
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//

int rc,i , j,ri_count=0,rtn, li_len, li_row_count, li_count, li, LI_LOOP
string ls_message,ls_msgparm[1],Lchartnum,annotxt,anno_prop_text,Lprvbkmed,Lconno,Lchart_no,&
		 ls_ttl, ls_sttl, ls_auth, ls_authfn, ls_plus_auth, ls_sauth,  local_chno, ls_ttlart ,&
		 ls_plus_auth_check,ls_soundex, ls_soundex_auth, ls_chno,ls_out, ls_out2, ls_parprt,&
		 ls_prevbkmed,  ls_conno, ls_ricd, null_string
long Lprvbkseq, lcnt, li_prevbkseq
Boolean ans

n_cst_string		lnv_string

		  
SetNull(null_string)

IF IsValid(w_notepad) THEN close(w_notepad) 

// If this is PMS edit annotation stage, default the pminit field to "PMS".
IF anno_stage = 3 THEN
	dw_annotation.object.ttlinit_pminit[1] = "P&M"
	dw_annotation.object.ttlinit_pmedt[1] = Today()
ELSEIF anno_stage = 2 OR anno_stage = 4 THEN
	dw_annotation.object.ttlinit_cdedt[1] = Today()
ELSEIF anno_stage = 6 THEN
	dw_annotation.object.ttlinit_pcsedt[1] = Today()
	dw_annotation.object.ttlinit_pcsinit[1]= UPPER(sqlservertrans.userid)
ELSEIF anno_stage = 1 THEN
	dw_annotation.object.ttlinit_ccdt[1] = Today()	
END IF

// Check for any pending updates
IF of_UpdateChecks( ) < 0 THEN Return -1

// Accept the text that was put on the screen.
dw_annotation.AcceptText()
dw_coauthors.AcceptText()
dw_medium.AcceptText()
dw_ri.AcceptText()
dw_cdssubject.AcceptText()

// verify there is not duplication of the coauthor
IF wf_coauthor_dup_check_ok() =FALSE THEN
	MessageBox('Error','You have entered a duplicate coauthor. Please remove the duplicate coauthor.')
	Return
END IF

IF wf_validate_cdssubject() = -1 THEN
	Messagebox('Error','Please enter a valid CDS Subject code.')
	dw_cdssubject.setfocus()
	RETURN
END IF

// If there were no changes to the screen, don't try to update the screen.
IF dw_annotation.ModifiedCount() > 0 OR &
	dw_coauthors.ModifiedCount() > 0 OR &
	dw_cdssubject.ModifiedCount() > 0 OR &
	dw_coauthors.DeletedCount() > 0 OR &
	dw_medium.ModifiedCount() > 0 OR &
	dw_ri.ModifiedCount() > 0 OR &
	ib_anno_modified=TRUE THEN
	
	ans =	lnvo_val_anno.of_val_casub_ajyfn(dw_annotation.object.ttlinit_ajyfn[1], &
														dw_annotation.object.ttlinit_casub[1])
	IF ans = FALSE THEN
		MessageBox("ERROR","Invalid ~'AJYFN~' Code against ~'CASUB~' Code. Update Failed.",StopSign!)
		RETURN -1
	END IF
	IF ISNULL(dw_annotation.object.ttlinit_pmsub1[1]) THEN
		MessageBox("ERROR","PMSUB can not be NULL",StopSign!)
		RETURN -1
	END IF
	li_count=dw_ri.RowCount()
	// 03/24/2008 IF YOU DELETE ALL THE MEDIUM ROWS ERROR OCCURS
	IF dw_medium.rowcount() > 0 THEN
		ls_conno=dw_medium.object.mchar_conno[1]
		ls_ricd=dw_medium.object.mchar_ricd[1]
	END IF
	
	ls_ricd=trim(ls_ricd)
	if li_count>0 then
		ls_prevbkmed=dw_ri.object.ri_prevbkmed[1]
		ls_prevbkmed=trim(ls_prevbkmed)
		li_prevbkseq=dw_ri.object.ri_prevbkseq[1]
		if isnull(li_prevbkseq) and (ls_prevbkmed="" or isNull(ls_prevbkmed) ) then
			dw_ri.Reset()
			dw_ri.object.mchar_ricd[1]=null_string
		end if
	elseif li_count=0 then
		IF dw_medium.rowcount() > 0 THEN
				dw_medium.object.mchar_ricd[1]=null_string
		END IF
	end if
	li_count=dw_ri.RowCount()
	if li_count> 0 then
		for i=1 to li_count
			ls_prevbkmed=dw_ri.GetItemString(i,'ri_prevbkmed')
			li_prevbkseq=dw_ri.GetItemNumber(i,'ri_prevbkseq')
			
			//11/24/2008	 Tracker 2122 validate entry of RI/RR/RT codes
			ls_ricd = dw_ri.GetItemstring(i,'mchar_ricd')
			
			IF (NOT(IsNull(ls_prevbkmed)) AND ls_prevbkmed<>"") AND NOT(IsNull(li_prevbkseq)) then
				if isNull(ls_ricd) or ls_ricd="" then
					//11/24/2008	 Tracker 2122 validate entry of RI/RR/RT codes
					
//					li=messagebox('Warning','RR, RI or RT exist for control #: '+ls_conno+'.'+&
//					'~nRR/RI/RT field is required.'+&
//					'~nDo you want to enter that value (Yes),'+& 
//					'~nor remove the records from RI table (No)?',&
//					Question!,YesNoCancel!,1)

					Messagebox("RICD","Please select Reissue/Rerecord before pressing Update.")
					li =1
				elseif ls_ricd<>'RR' and ls_ricd<>'RI' and ls_ricd<>'RT' THEN
					messagebox('Invalid value','The valid values are RR/RI/RT.')
					dw_ri.ScrollToRow(1)
					dw_ri.SetRow(1)
					dw_ri.SetFocus()
					dw_ri.SetColumn('mchar_ricd')
					return
				END IF
				if li=1 then
					IF i>0 THEN
						dw_ri.ScrollToRow(i)
						dw_ri.SetRow(i)
					END IF
					dw_ri.SetFocus()
					dw_ri.SetColumn('mchar_ricd')
					return
				elseif li=2 then
					ls_conno=dw_ri.object.ri_conno[1]
					dw_ri.Reset()
					delete
					from ri
					where conno=:ls_conno
					using SqlServerTrans;
					IF f_check_dberror(sqlservertrans,"delete from ri")=FALSE THEN
						RETURN 
					ELSE
						COMMIT USING SQLServerTrans;
						exit
					END IF
				elseif li=3 then
					continue
					//return
				end if //end if li=1
			elseif (isnull(ls_prevbkmed) or ls_prevbkmed="" or isnull(li_prevbkseq)) and &
				 (IsNull(ls_ricd)=false and ls_ricd<>"") then
				messagebox('warning','You must enter media and book no.')
				dw_ri.ScrollToRow(i)
				dw_ri.SetRow(i)
				dw_ri.SetFocus()
				dw_ri.SetColumn('ri_prevbkmed')
				return
			end if//end if isnull(ls_prevbkmed)=false
		next
	end if// for i=li_count dw_ri and dw_medium value validation
	
	// Before you update the medium datawindow execute the window function check media flag to make sure the 
	// other media conno is populated properly
	wf_check_media_flags()
	
	// update medium datawindow. arflag is updated to null at this time.
	rc = dw_medium.Event pfc_Update(True,True)
	ls_chno=dw_annotation.object.ttlinit_chno[1]
	ls_auth=dw_annotation.object.ttlinit_auth[1]
	if IsNull(ls_auth) then ls_auth=''
	ls_authfn=dw_annotation.object.ttlinit_authfn[1]
	if IsNull(ls_authfn) then ls_authfn=''
	ls_ttl=dw_annotation.object.ttlinit_ttl[1]
	ls_sauth = f_create_sttl(ls_auth)
	ls_sttl = f_create_sttl(ls_ttl)
	ls_soundex = f_soundex(ls_ttl, 'ttl')
	ls_soundex_auth =f_soundex(ls_auth+ ls_authfn,'auth')
	dw_annotation.object.ttlinit_sauth[1] =ls_sauth
	dw_annotation.object.ttlinit_sttl[1] =ls_sttl
	dw_annotation.object.ttlinit_soundex_ttl[1] =ls_soundex
	dw_annotation.object.ttlinit_soundex_auth[1] =ls_soundex_auth
	IF rc = 1 THEN
		// update coauthors



		rc = dw_coauthors.Event pfc_Update(True,True)
		IF rc = 1 THEN
			// Update the ttlinit table.

			
			rc = dw_annotation.Event pfc_Update(True,True)
			commit  USING SqlServerTrans; 			

			rtn = dw_ri.Event pfc_Update(True,True)
			if rc=1  and rtn=1 then 

				dw_cdssubject.Event pfc_Update(True,True)
				commit  USING SqlServerTrans;// false should be
			else
				ROLLBACK USING SqlServerTrans;
							return -1
			end if
			// If successfull, change the update capability of ttlinit columns to 'no' and
			// then set the update capability if the cr table to 'yes'.
			ls_chno=dw_annotation.object.ttlinit_chno[1]
//			ls_soundex_auth =f_soundex(ls_auth+ ls_authfn,'auth')
//			update ttlinit
//			set soundex_auth=:ls_soundex_auth
//			where chno=:ls_chno using SqlServerTrans;
//			IF f_check_dberror(sqlservertrans,"update ttlinit set soundex_auth") THEN
//				COMMIT USING SqlServerTrans;
//			ELSE 
//				ROLLBACK USING SqlServerTrans;
//			END IF

			IF rc = 1 THEN
				IF dw_annotation.AcceptText() = 1 THEN
					// Get the Chart Number
					lchno=dw_annotation.object.ttlinit_chno[1]
				END IF
				
				IF Lchno="" OR IsNull(Lchno) THEN
					Lchno = Local_chno
				END IF
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
					IF f_check_dberror(sqlservertrans,"STTL - short title") THEN
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
				COMMIT USING SQLServerTrans;
				
				annotxt = mle_anno.text
				annotxt = RightTrim(annotxt)

				// If the annotaion dose not exist insert in to the annotation table
				// otherwise update that table.
				IF Lchno="" OR IsNull(Lchno) THEN
					Lchart_no	= dw_annotation.object.ttlinit_chno[1]					
					Lchno = Lchart_no
				END IF
				
				// First remove the newline characters from annotation.
				annotxt = lnv_string.of_GlobalReplace(annotxt, '~r~n', " ")
				anno_prop_text = LeftTrim(wf_sex_viol_lang())
				
				IF anno_exist=FALSE THEN
					// Insert the annotation into the annotation table.

					INSERT INTO annotation(chno,anno,anno_property) 
					VALUES (:Lchno,:annotxt,:anno_prop_text) 
					USING SQLServerTrans;
				ELSE
					// If annotation exist update the table

					UPDATE annotation 
					SET anno = :annotxt,anno_property=:anno_prop_text
					WHERE chno = :Lchno 
					USING SQLServerTrans;
				END IF
				IF f_check_dberror(sqlservertrans,"ANNOTATION")=FALSE THEN
					RETURN -1
				ELSE
					COMMIT USING SQLServerTrans;
					anno_exist=TRUE
					anno_tags_altered=FALSE
					first_time_annotation=FALSE
				END IF
				
				Lconno = em_conno.text	
				
	// 01/28/2008  added code to process commercial audio switch and vendor code based on new columns
				String ls_comm_audio_yn, ls_comm_audio_vendor
				ls_comm_audio_yn = dw_annotation.object.mchar_comm_audio_yn[1]
				ls_comm_audio_vendor = dw_annotation.object.mchar_comm_audio_vendor[1]
//				

				// 09/30/2008 Set commercial audio for all conno related to RC and DB for the book
				IF dw_medium.Rowcount() > 0  THEN
					FOR li_loop = 1 TO dw_medium.Rowcount()
						lconno = dw_medium.object.mchar_conno[li_loop]
						
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
					NEXT
				ELSE
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
				
	//////////////

				Select parprt 
				into :ls_parprt
				from mchar
				where conno = :Lconno
				using sqlservertrans;
				
				IF f_check_dberror(sqlservertrans,"MCHAR") THEN
					// If parprt is already set to 'P'(printed par) assign it to 'C'(changed par).
					IF ls_parprt = 'P' THEN
						f_set_parprt(Lconno,'C')
					END IF
				END IF
				
//				FOR i=1 to dw_medium.RowCount()
//					Lprvbkmed = dw_ri.object.ri_prevbkmed[i]
//					Lprvbkseq = dw_ri.object.ri_prevbkseq[i]
//					Lconno = dw_medium.object.mchar_conno[i]
//					IF (Lprvbkmed <> "" OR IsNull(Lprvbkmed)=FALSE) AND &
//						(Lprvbkseq <> 0  OR IsNull(Lprvbkseq)=FALSE) THEN
//						   // Check to see if conno exist in ri table
//							SELECT count(*) INTO :ri_count FROM ri 
//							WHERE conno=:Lconno USING SQLServerTrans;
//							// If not insert into ri table
//							IF ri_count=0 THEN
//								INSERT INTO ri(conno,prevbkseq,prevbkmed) 
//								VALUES (:Lconno,:Lprvbkseq,:Lprvbkmed) 
//								USING SQLServerTrans;
//								IF f_check_dberror(sqlservertrans,"RI")=FALSE THEN
//									RETURN -1
//								ELSE
//									COMMIT USING SQLServerTrans;
//								END IF
//							// else update ri table
//							ELSE
//								UPDATE ri SET prevbkmed=:Lprvbkmed,prevbkseq=:Lprvbkseq
//								WHERE conno=:Lconno
//								USING SQLServerTrans;
//								IF f_check_dberror(sqlservertrans,"RI")=FALSE THEN
//									RETURN -1
//								ELSE
//									COMMIT USING SQLServerTrans;
//								END IF
//							END IF
//					END IF
//				NEXT
				// Mark the MCHAR Table
				f_update_mchar_time(Lconno,0,"C","U")
				
 				ib_disableclosequery = TRUE
				 ib_update = TRUE // 04/21/2008
				cb_print.TriggerEvent(Clicked!)
			else // update ttlinit
				ROLLBACK USING SQLServerTrans;
				MessageBox("Error","Updating TTLINIT table failed.",StopSign!)
				RETURN -1
			end if
		else
			ROLLBACK USING SQLServerTrans;
			MessageBox("Error","Updating COAUTHOR table failed.",StopSign!)
			RETURN -1
		end if // update coauthor table
	else
		ROLLBACK USING SQLServerTrans;
		MessageBox("Error","Updating MEDIUM failed.",StopSign!)
		RETURN -1
	end if  // update medium
Else
 	MessageBox("Update","No changes were made.",Information!)
	Return -1
end if 
end event

type cb_clear from commandbutton within w_sheet_annotation
event clicked pbm_bnclicked
string tag = "Clear the screen"
integer x = 2757
integer y = 1600
integer width = 233
integer height = 96
boolean bringtotop = true
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "&Clear"
end type

event clicked;dw_annotation.Reset()
dw_annotation.insertrow(0)
dw_medium.Reset()
dw_medium.insertrow(0)
dw_medium.Object.mchar_ricd.Edit.Required='No'
dw_ri.Reset()
dw_ri.InsertRow(0)
dw_coauthors.Reset()
dw_coauthors.insertrow(0)
dw_coauthors.Enabled = TRUE
dw_cdssubject.Reset()
dw_cdssubject.insertrow(0)
dw_cdssubject.Enabled = TRUE
anno_exist=FALSE
first_time_annotation=FALSE
IF IsValid(w_notepad) THEN close(w_notepad) 
st_wordcnt.text="00"
mle_anno.text=""
mle_tags.text=""
em_conno.text=""
em_bkseq.text=""
what_text=""
SetNull(Lchno)
em_conno.SetFocus()
wf_disable_buttons()
IF anno_stage = 4 THEN
	
END IF

// 11/03/2009 DB Labels Project
wf_clear_label_fields(FALSE)
end event

type cb_exit from commandbutton within w_sheet_annotation
event clicked pbm_bnclicked
string tag = "Exit the screen"
integer x = 3026
integer y = 1600
integer width = 270
integer height = 96
boolean bringtotop = true
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "E&xit"
end type

event clicked;IF IsValid(w_notepad) THEN close(w_notepad) 
parent.Event pfc_close()
m_pics_main.m_menu.PopMenu(300, 0)

end event

type mle_anno from u_mle within w_sheet_annotation
event pfc_hinttext pbm_mousemove
event ue_char_entered pbm_char
event ue_key_up pbm_keyup
event ue_lmouse_up pbm_lbuttonup
string tag = "Please enter the annotation"
integer y = 960
integer width = 1097
integer height = 448
integer taborder = 40
integer textsize = -9
fontfamily fontfamily = roman!
string facename = "Times New Roman"
boolean vscrollbar = true
end type

event pfc_hinttext;call super::pfc_hinttext;w_pics_main.setmicrohelp(mle_anno.tag)

end event

event ue_char_entered;call super::ue_char_entered;if ib_anno_modified = false then
	ib_anno_modified = true
end if

end event

event ue_key_up;wf_get_wordcount()

end event

event ue_lmouse_up;wf_get_wordcount()


end event

event getfocus;call super::getfocus;m_pics_main.m_edit.m_deleterow.Enabled 	=	FALSE
m_pics_main.m_edit.m_addrow.Enabled 		=	FALSE

end event

event modified;call super::modified;ib_anno_modified=TRUE
end event

type cb_spell from commandbutton within w_sheet_annotation
event clicked pbm_bnclicked
string tag = "Spell Checker"
integer x = 197
integer y = 1600
integer width = 334
integer height = 96
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
	
dw_annotation.accepttext()
	
//annotation
ls_S = mle_anno.Text
IF NOT(IsNull(ls_S)) THEN
	lnca_Word.SpellCheck( ls_S )
	mle_anno.Text = ls_S
END IF

//Oneliner
loneliner = dw_annotation.object.ttlinit_oneliner[1]
IF NOT(IsNull(loneliner)) THEN
	lnca_Word.SpellCheck( loneliner )
	dw_annotation.object.ttlinit_oneliner[1] = loneliner
END IF

//Title
lttl = dw_annotation.object.ttlinit_ttl[1]
IF NOT(IsNull(lttl)) THEN
	lnca_Word.SpellCheck( lttl )
	dw_annotation.object.ttlinit_ttl[1] = lttl
END IF
  

end event

type dw_annotation from u_pics_dw within w_sheet_annotation
event ue_enterkey pbm_dwnprocessenter
event pfc_hinttext pbm_mousemove
integer x = 402
integer y = 12
integer width = 2889
integer height = 872
integer taborder = 30
string dataobject = "d_annotation"
boolean hscrollbar = true
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

event itemchanged;Boolean ans 
Int rtn,i, li_len, li_re, li_anyr
String ls_ttlart,ls_chno, ls_fn, ls_ln, ls_crname, ls_crnameold,ls_filter
ls_chno = dw_annotation.object.ttlinit_chno[1]

IF DWO.Name = "ttlinit_casub" THEN
	ans =	lnvo_val_anno.of_val_casub_ajyfn(dw_annotation.object.ttlinit_ajyfn[1],Data)
	IF ans = FALSE THEN
		Messagebox("ERROR","Invalid ~'AJYFN~' Code against ~'CASUB~' Code.",stopSign!)
		RETURN 0
	END IF
ELSEIF DWO.Name = "ttlinit_gradecd" THEN
         	anno_tags_altered = TRUE
		this.AcceptText()
		mle_tags.text = LeftTrim(wf_sex_viol_lang())
		wf_get_wordcount()
ELSEIF DWO.Name = "ttlinit_lang" THEN
          	anno_tags_altered = TRUE
		this.AcceptText()
		mle_tags.text = LeftTrim(wf_sex_viol_lang())
		wf_get_wordcount()
ELSEIF DWO.Name = "ttlinit_lcno" THEN
	String lcno
	lcno = Trim(Data)
	IF Len(lcno) <> 8 THEN 
		RETURN 1
	END IF
ELSEIF DWO.Name = "ttlinit_ajyfn" THEN
	String ajyfn
	ajyfn = Left(Data,2)

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
		IF dw_annotation.object.ttlinit_casub[1] <> "" THEN
			ans =	lnvo_val_anno.of_val_casub_ajyfn(Data,dw_annotation.object.ttlinit_casub[1])
			IF ans = FALSE THEN
				Messagebox("ERROR","Invalid ~'AJYFN~' Code against ~'CASUB~' Code.",stopSign!)
				RETURN 0
			END IF
		END IF
	END IF
	

ELSEIF DWO.Name = "ttlinit_aepcd" THEN
	String aepcd, ls_null
	SetNull(ls_null)
	aepcd = Left(Data,1)
	IF aepcd <> 'A' AND aepcd <> 'C' AND aepcd <> 'E' AND aepcd <> 'P' AND &
		aepcd <> 'S' AND aepcd <> 'N' AND aepcd <> 'T' AND aepcd <> 'R' AND &
		aepcd <> 'I' AND aepcd <> 'L' THEN
		RETURN 1
	END IF
	IF aepcd = 'L' THEN
		dw_annotation.object.ttlinit_authfn[1] =ls_null
		dw_annotation.object.ttlinit_ahonorific[1] = ""
	END IF
	IF aepcd = 'N' THEN
		dw_annotation.object.ttlinit_authfn[1] = ""
		dw_annotation.object.ttlinit_auth[1] = ""
		dw_annotation.object.ttlinit_ahonorific[1] = ""
		IF dw_coauthors.RowCount() > 0 THEN
			rtn = Messagebox("Warnning","Author and Coauthors are not required for this Title Entry. Remove it?",question!,yesNo!,1)
			IF rtn = 1 THEN
				FOR i=1 TO dw_coauthors.RowCount()
					dw_coauthors.DeleteRow(i)
				NEXT
				dw_coauthors.EVENT pfc_update(TRUE,TRUE)
				COMMIT USING SqlServerTrans;
				dw_coauthors.Reset()
				dw_coauthors.enabled = FALSE
			ELSE
				RETURN
			END IF
		ELSE
			dw_coauthors.enabled = FALSE
		END IF
	END IF
ELSEIF DWO.Name = "ttlinit_annoinit" THEN
	IF Data <> Upper(SqlServerTrans.userId) THEN
		RETURN 1
	END IF
ELSEIF DWO.Name = "ttlinit_pminit" THEN
	IF Data <> Upper(SqlServerTrans.userId) THEN
		RETURN 1
	END IF
ELSEIF DWO.Name = "ttlinit_cdinit" THEN
	IF Data <> Upper(SqlServerTrans.userId) THEN
		RETURN 1
	END IF
ELSEIF DWO.Name ='ttlinit_ttlart' THEN
	ls_ttlart =Data
	IF ls_ttlart <>'' AND  (NOT IsNull(ls_ttlart)) THEN
	li_len = Len(ls_ttlart)
	ls_ttlart =Trim(ls_ttlart)
		IF ls_ttlart ='' AND li_len >0 THEN
		   dw_annotation.object.ttlinit_ttlart.Validationmsg ='Title article ' +&
			'can not be filled with spaces. Please delete extra spaces from this field.' 
			RETURN 1
		END IF
	END IF
ELSEIF DWO.Name = 'ttlinit_sexcd' THEN
          	anno_tags_altered = TRUE
		this.AcceptText()
		mle_tags.text = LeftTrim(wf_sex_viol_lang())
		wf_get_wordcount()
ELSEIF DWO.Name = 'ttlinit_violencecd' THEN
          	anno_tags_altered = TRUE
		this.AcceptText()
		mle_tags.text = LeftTrim(wf_sex_viol_lang())
		wf_get_wordcount()
ELSEIF DWO.Name = 'ttlinit_stronglang' THEN
          	anno_tags_altered = TRUE
		this.AcceptText()
		mle_tags.text = LeftTrim(wf_sex_viol_lang())
		wf_get_wordcount()
ELSEIF DWO.Name = 'ttlinit_prize' THEN
          	anno_tags_altered = TRUE
		this.AcceptText()
		mle_tags.text = LeftTrim(wf_sex_viol_lang())
		wf_get_wordcount()
ELSEIF DWO.Name = "ttlinit_bestseller" THEN			   
//         	anno_tags_altered = TRUE
//		this.AcceptText()
//		mle_tags.text = LeftTrim(wf_sex_viol_lang())
//		wf_get_wordcount()
		Post wf_set_bestseller()
ELSEIF DWO.Name = "cf_crname" THEN
	IF Integer(Data) = 1 THEN
		IF org_crname<>"" OR NOT(IsNull(org_crname)) THEN
			this.object.ttlinit_crname[row] = org_crname
		END IF
	ELSE
		this.object.ttlinit_crname[row] = "Public Domain"
	END IF
	RETURN
   //Checking for required column ANYR data entered or not
ELSEIF DWO.Name = "ttlinit_anyr" THEN
	//Problem Report 1868 allows year written to be up to two years in advance
	IF Long(Data) > (Year(Today()) + 2) OR IsNumber(Data)=FALSE OR IsNull(li_anyr) THEN
	   RETURN 1
	END IF
   		anno_tags_altered = TRUE
		this.AcceptText()
		mle_tags.text = LeftTrim(wf_sex_viol_lang())
		wf_get_wordcount()	
	///// reference to columns removed 01/28/2008 shared master flag, shared master data

// 01/28/2008 changes
ELSEIF DWO.Name = "mchar_comm_audio_yn" THEN
	is_comm_audio = data
	IF Data = 'N' THEN
		IF NOT(IsNull(this.object.mchar_comm_audio_vendor[row])) OR (this.object.mchar_comm_audio_vendor[row] <> "") THEN
		  this.object.mchar_comm_audio_vendor[row] = ""
		END IF
		This.object.mchar_comm_audio_vendor.visible=FALSE
		mle_tags.text = wf_sex_viol_lang() // Reset annotation remove commercial audio indicator
	ELSE
		This.object.mchar_comm_audio_vendor.visible=TRUE
		mle_tags.text = wf_sex_viol_lang() // Reset annotation remove commercial audio indicator
	END IF
///////////////


ELSEIF DWO.Name = "ttlinit_isbn" THEN
	
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
ls_crnameold=dw_annotation.object.ttlinit_crname[1]
IF ls_crnameold<>'Public Domain' THEN
	IF DWO.Name='ttlinit_auth' THEN
		li_re=Messagebox('','Author last name changed, Do you want copyright name to be changed'+&
			'~nbecause of this change?',exclamation!,yesNo!,1)
		IF li_re=1 THEN
			ls_fn=dw_annotation.object.ttlinit_authfn[1]
			ls_crname=f_combine_string1_string2(ls_fn, Data)
			dw_annotation.object.ttlinit_crname[1]=ls_crname
		END IF
	ELSEIF DWO.Name='ttlinit_authfn' THEN
		li_re=Messagebox('','Author first name changed, Do you want copyright name to be changed'+&
			'~nbecause of this change?',exclamation!,yesNo!,1)
		IF li_re=1 THEN
			ls_ln=dw_annotation.object.ttlinit_auth[1]
			ls_crname=f_combine_string1_string2(Data, ls_ln)
			dw_annotation.object.ttlinit_crname[1]=ls_crname
		END IF
	END IF
END IF

end event

event itemfocuschanged;call super::itemfocuschanged;IF IsValid(inv_dropdownsearch)  AND &
   (dwo.Name = "ttlinit_pmsub1" OR dwo.Name = "ttlinit_pmsub2" OR dwo.Name = "ttlinit_pmsub3" OR dwo.Name = "ttlinit_casub") THEN
	this.inv_dropdownsearch.event pfc_itemfocuschanged(row, dwo)
end if


end event

event rbuttondown;//
end event

event rbuttonup;//
end event

event editchanged;call super::editchanged;IF IsValid(inv_dropdownsearch)  AND &
   (dwo.Name = "ttlinit_pmsub1" OR dwo.Name = "ttlinit_pmsub2" OR dwo.Name = "ttlinit_pmsub3" OR dwo.Name = "ttlinit_casub") THEN
	this.inv_dropdownsearch.EVENT pfc_EditChanged(row, dwo, data)
END IF
end event

event constructor;call super::constructor;m_pics_main.m_edit.m_deleterow.Enabled 	=	FALSE
m_pics_main.m_edit.m_addrow.Enabled 		=	FALSE

dw_annotation.of_SetDropDownSearch(TRUE)

IF NOT dw_annotation.inv_dropdownsearch.of_IsRegistered ("ttlinit_casub") THEN
    dw_annotation.inv_dropdownsearch.of_Register("ttlinit_casub")
END IF
IF NOT dw_annotation.inv_dropdownsearch.of_IsRegistered ("ttlinit_pmsub1") THEN
    dw_annotation.inv_dropdownsearch.of_Register("ttlinit_pmsub1")
END IF
IF NOT dw_annotation.inv_dropdownsearch.of_IsRegistered ("ttlinit_pmsub2") THEN
    dw_annotation.inv_dropdownsearch.of_Register("ttlinit_pmsub2")
END IF
IF NOT dw_annotation.inv_dropdownsearch.of_IsRegistered ("ttlinit_pmsub3") THEN
    dw_annotation.inv_dropdownsearch.of_Register("ttlinit_pmsub3")
END IF

//01/28/2008 vendor code 
IF NOT dw_annotation.inv_dropdownsearch.of_IsRegistered ("mchar_comm_audio_vendor") THEN
    dw_annotation.inv_dropdownsearch.of_Register("mchar_comm_audio_vendor")
END IF

dw_annotation.of_SetTransObject( SQLServerTrans )
// Make the background color of required field yellow
dw_annotation.of_setcolorrequired(65535)

end event

event retrieveend;call super::retrieveend;string ls_sexcd,ls_violencecd,ls_stronglang,ls_bestseller,ls_specattn

IF rowcount > 0 THEN
	// Get the sexcode
	ls_sexcd = dw_annotation.Getitemstring(Getrow(),"ttlinit_sexcd")
	IF ISNULL(ls_sexcd) OR TRIM(ls_sexcd) = "" THEN
		dw_annotation.Setitem(Getrow(),"ttlinit_sexcd","N")
	END IF
	ls_violencecd = dw_annotation.Getitemstring(Getrow(),"ttlinit_violencecd")
	IF ISNULL(ls_violencecd) OR TRIM(ls_violencecd) = "" THEN
		dw_annotation.Setitem(Getrow(),"ttlinit_violencecd","N")
	END IF
	ls_stronglang = dw_annotation.Getitemstring(Getrow(),"ttlinit_stronglang")
	IF ISNULL(ls_stronglang) OR TRIM(ls_stronglang) = "" THEN
		dw_annotation.Setitem(Getrow(),"ttlinit_stronglang","N")
	END IF
	ls_bestseller = dw_annotation.Getitemstring(Getrow(),"ttlinit_bestseller")
	IF ISNULL(ls_bestseller) OR TRIM(ls_bestseller) = "" THEN
		dw_annotation.Setitem(Getrow(),"ttlinit_bestseller","N")
	END IF
	ls_specattn = dw_annotation.Getitemstring(Getrow(),"ttlinit_specattn")
	IF ISNULL(ls_specattn) OR TRIM(ls_specattn) = "" THEN
		dw_annotation.Setitem(Getrow(),"ttlinit_specattn","N")
	END IF
	org_crname = dw_annotation.object.ttlinit_crname[1]
END IF
end event

event sqlpreview;call super::sqlpreview;//MessageBox("sql",sqlsyntax)
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
	RETURN -1
END IF

//11/19/2008 mandatory column validations Tracker 2122


ll_yr  = this.object.ttlinit_anyr[1] // year written
ls_lang = this.object.ttlinit_lang[1]
ls_aepcd  = this.object.ttlinit_aepcd[1]  // author code
ls_pmsub1 = this.object.ttlinit_pmsub1[1] 
ls_oneliner = this.object.ttlinit_oneliner[1]

IF Isnull(ll_yr)  OR ll_yr = 0 THEN
	lb_error=TRUE
	ls_errtext +=' Year Written must be entered.' + '~r~n'
END IF

IF Isnull(ls_lang) OR Len(Trim(ls_lang)) =  0 THEN
	lb_error=TRUE
	ls_errtext +=' Language must be entered.' + '~r~n'
END IF

IF Isnull(ls_aepcd) OR Len(Trim(ls_aepcd)) =  0 THEN
	lb_error=TRUE
	ls_errtext +=' Author Code  must be entered.' + '~r~n'
END IF

IF Isnull(ls_pmsub1) OR Len(Trim(ls_pmsub1)) =  0 THEN
	lb_error=TRUE
	ls_errtext +=' NLS CAT  must be entered.' + '~r~n'
END IF

IF Isnull(ls_oneliner) OR Len(Trim(ls_oneliner)) =  0 THEN
	lb_error=TRUE
	ls_errtext +=' Oneliner  must be entered.' + '~r~n'
END IF

IF lb_error THEN
	Messagebox('Error', ls_errtext)
	RETURN -1
END IF

RETURN 1
///////////////
end event

type dw_medium from u_pics_dw within w_sheet_annotation
event ue_enterkey pbm_dwnprocessenter
event pfc_hinttext pbm_mousemove
integer x = 1093
integer y = 884
integer width = 1472
integer height = 304
integer taborder = 50
string dataobject = "d_medium_nori"
boolean hscrollbar = true
boolean livescroll = false
boolean ib_isupdateable = false
boolean ib_rmbmenu = false
boolean ib_rmbfocuschange = false
string is_updatesallowed = ""
end type

event ue_enterkey;call super::ue_enterkey;Send(Handle(this),256,9,Long(0,0))
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

event itemchanged;call super::itemchanged;//////////////////////////////////////////////////////////////////////////////////////////////////////////
//
//	Event: itemchanged of dw_annotation
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
// Murali K.			01/30/2008      005 PICS Modifications	 Reqs: CDS. A.7, CDS A.8a.2
//											set audit colmns created_by... for new media row
// Murali K.			04/16/2008		New Media functionality validations
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//

Integer cnt_ri,rtn,ll_row
Long Li_bkseq
String lconno,lmchar_med,original_media, ls_null, ls_msg,Ls_bkmed,lc_frflag,ls_chno,ls_userid, ls_med
String Lmed1,Lmed2,Lmed3,Lconno1,Lconno2,Lconno3,Lbkmed1,Lbkmed2,Lbkmed3,Lflashind1, Lflashind2, Lflashind3
Date tday

SetNull(ls_null)
tday = Today()
ls_userid = sqlservertrans.userid

IF dwo.Name = "flash_indicator" THEN
	
    Li_bkseq = this.object.bkseq[row]
    Ls_bkmed = this.object.bkmed[row]
    lc_frflag = this.object.frflag[row]
	lconno = this.Object.mchar_conno[row]
	 
	IF anno_stage = 3  OR anno_stage = 4  OR anno_stage = 5 OR anno_stage = 6 THEN
		ls_msg = 'Flash indicator can only be set by CDS section during add annotation, CDS edit annotation, or final review.'
		this.Object.flash_indicator.ValidationMsg = ls_msg
		this.ReSet()
		RETURN 1
	END IF	

    IF NOT(IsNull(Li_bkseq)) THEN
		ls_msg = 'Book number '+ Ls_bkmed+string(Li_bkseq) +' has already been assigned to this control number, you may not change the flash indicator.'
		this.Object.flash_indicator.ValidationMsg = ls_msg
		RETURN 1
	ELSEIF NOT(IsNull(lc_frflag)) THEN
		ls_msg = 'Control number '+ lconno +' has already been approved in final review, you may only change the flash indicator in final review screen.'
		this.Object.flash_indicator.ValidationMsg = ls_msg
		RETURN 1
	ELSE
		this.object.flash_change_date[row] = tday
		this.object.flash_change_user[row] = ls_userid		
	END IF
	
	
ELSEIF dwo.Name = "bkmed" THEN

	dw_medium.AcceptText()
	
    Li_bkseq = this.object.bkseq[row]
    Ls_bkmed = this.object.bkmed[row]
    lc_frflag = this.object.frflag[row]
    lconno = this.Object.mchar_conno[row]
    ls_med = this.Object.mchar_med[row]

	IF NOT(IsNull(Li_bkseq)) THEN
		ls_msg = 'Book number '+ Ls_bkmed+string(Li_bkseq) +' has already been assigned to this control number, you may not change the book media.'
		this.Object.bkmed.ValidationMsg = ls_msg
		RETURN 1
	ELSEIF NOT(IsNull(lc_frflag)) THEN
		ls_msg = 'Control number '+ lconno +' has already been approved in final review, you may not change the book media.'
		this.Object.bkmed.ValidationMsg = ls_msg
		RETURN 1
	END IF
	
	CHOOSE CASE this.rowcount()
		CASE 2
			// Media and book media for rows 1 an 2
			Lmed1 = dw_medium.object.mchar_med[1]
			Lmed2 = dw_medium.object.mchar_med[2]
			
			Lbkmed1 = dw_medium.object.bkmed[1]
			Lbkmed2 = dw_medium.object.bkmed[2]

			Lflashind1 = dw_medium.object.flash_indicator[1]
			Lflashind2 = dw_medium.object.flash_indicator[2]
			
			
			IF Lmed1 = 'RTB' AND  Lbkmed1 = 'BR' THEN
				this.Object.bkmed.ValidationMsg='RTB rows can not have BR for book media'
				dw_medium.object.bkmed[1] = 'RC'	
			ELSEIF Lmed2 = 'RTB' AND Lbkmed2 = 'BR' THEN
				this.Object.bkmed.ValidationMsg='RTB rows can not have BR for book media'
				dw_medium.object.bkmed[2] = 'DB'	
			END IF		
			
			// 04/17/2008 for BR and P/B media BR can be the only book media
			IF (Ls_med = 'BR' OR ls_med = 'P/B') AND data <> 'BR' THEN
				this.Object.bkmed.ValidationMsg='Braille and Print Braille can only have BR book media'
				dw_medium.object.bkmed[1] = 'BR'	
				RETURN 1
			END IF				
			
			IF Lmed1 = 'RTB' AND Lbkmed1 = 'RC' AND Lflashind1 = 'Y' THEN
				rtn = Messagebox("Flash Indicator","Flash indicator was established for this control number. Do you want to reset it?",Question!, YesNo!, 1)
				IF rtn=1 THEN
					this.object.flash_change_date[1] = tday
					this.object.flash_change_user[1] = ls_userid
					dw_medium.object.flash_indicator[1] = ""
					IF Lbkmed2 = 'RC' THEN
					   dw_medium.object.bkmed[2] = 'DB'
					END IF
				ELSE
					RETURN 1
				END IF	
			ELSEIF Lmed2 = 'RTB' AND Lbkmed2 = 'RC' AND Lflashind2 = 'Y' THEN
				rtn = Messagebox("Flash Indicator","Flash indicator was established for this control number. Do you want to reset it?",Question!, YesNo!, 1)
				IF rtn=1 THEN
					this.object.flash_change_date[2] = tday
					this.object.flash_change_user[2] = ls_userid
					dw_medium.object.flash_indicator[2] = ""
					IF Lbkmed1 = 'RC' THEN
					   dw_medium.object.bkmed[1] = 'DB'
					END IF
				ELSE
					RETURN 1
				END IF	
			END IF

		CASE 3
			// Media for rows 1 an 2
			Lmed1 = dw_medium.object.mchar_med[1]
			Lmed2 = dw_medium.object.mchar_med[2]
			Lmed3 = dw_medium.object.mchar_med[3]
			
			Lbkmed1 = dw_medium.object.bkmed[1]
			Lbkmed2 = dw_medium.object.bkmed[2]
			Lbkmed3 = dw_medium.object.bkmed[3]
			
			IF Lmed1 = 'RTB' AND Lbkmed1 = 'BR' THEN
				this.Object.bkmed.ValidationMsg='Braille and Print Braille can only have BR book media'
				dw_medium.object.bkmed[1] = 'RC'	
				RETURN 1
			ELSEIF Lmed2 = 'RTB' AND Lbkmed2 = 'BR' THEN
				this.Object.bkmed.ValidationMsg='Braille and Print Braille can only have BR book media'
				dw_medium.object.bkmed[2] = 'DB'	
				RETURN 1
			ELSEIF Lmed3 = 'RTB' AND Lbkmed3 = 'BR' THEN
				this.Object.bkmed.ValidationMsg='Braille and Print Braille can only have BR book media'
				dw_medium.object.bkmed[3] = 'DB'	
				RETURN 1
			END IF				
			
				// 04/17/2008 for BR and P/B media BR can be the only book media
			IF (Ls_med = 'BR' OR ls_med = 'P/B') AND data <> 'BR' THEN
				this.Object.bkmed.ValidationMsg='Braille and Print Braille can only have BR book media'
				dw_medium.object.bkmed[1] = 'BR'	
				RETURN 1
			END IF		
			
		CASE 1
			
			// Media for rows 1 
			Lmed1 = dw_medium.object.mchar_med[1]
			Lbkmed1 = dw_medium.object.bkmed[1]
			
			IF Lmed1 <> 'RTB' AND Lbkmed1 <> 'BR' THEN
				this.Object.bkmed.ValidationMsg='Braille and Print Braille can only have BR book media'
				dw_medium.object.bkmed[1] = 'BR'	
				RETURN 1
			ELSEIF Lmed1 = 'RTB' AND ( Lbkmed1 <> 'RC' OR Lbkmed1 <> 'DB') THEN
				// Set the bkmed to BR for the first row
				this.Object.bkmed.ValidationMsg='RTB rows can not have BR for book media'
				dw_medium.object.bkmed[1] = 'RC'	
			END IF
			
				// 04/17/2008 for BR and P/B media BR can be the only book media
			IF (Ls_med = 'BR' OR ls_med = 'P/B') AND data <> 'BR' THEN
				this.Object.bkmed.ValidationMsg='Braille and Print Braille can only have BR book media'
				dw_medium.object.bkmed[1] = 'BR'	
				RETURN 1
			END IF		
			
	END CHOOSE


ELSEIF dwo.Name = "mchar_conno" THEN

	dw_medium.AcceptText()
	
    Li_bkseq = this.object.bkseq[row]
    Ls_bkmed = this.object.bkmed[row]
    lc_frflag = this.object.frflag[row]
	lconno = this.Object.mchar_conno[row]
	 
    IF NOT(IsNull(Li_bkseq)) THEN
		ls_msg = 'Book number '+ Ls_bkmed+string(Li_bkseq) +' has already been assigned to this control number, you may not change the control number.'
		this.Object.mchar_conno.ValidationMsg = ls_msg
		RETURN 1
	ELSEIF NOT(IsNull(lc_frflag)) THEN
		ls_msg = 'Control number '+ lconno +' has already been approved in final review, you can not change the control number'
		this.Object.mchar_conno.ValidationMsg = ls_msg
		RETURN 1
	END IF
	
	int li_rowcount
	li_rowcount = This.RowCount()

	CHOOSE CASE li_rowcount
		CASE 2
			// Media for rows 1 an 2
			Lmed1 = dw_medium.object.mchar_med[1]
			Lmed2 = dw_medium.object.mchar_med[2]
			
			// Control numbers for rows 1 an 2
			Lconno1 = dw_medium.object.mchar_conno[1]
			Lconno2 = dw_medium.object.mchar_conno[2]
			
			
			IF  Lconno1= Lconno2 THEN
				this.Object.mchar_conno.ValidationMsg='Control numbers must be different between rows'
				RETURN 1
			END IF
			
			// If the media for the both rows are equal to RTB
			IF Lmed1 = 'RTB' AND Lmed2 = 'RTB' THEN
				this.object.other_media_conno[1] = Lconno2
				this.object.other_media_conno[2] = Lconno1
			END IF
			
		CASE 3
			
			// Media for rows 1,2 and 3
			Lmed1 = dw_medium.object.mchar_med[1]
			Lmed2 = dw_medium.object.mchar_med[2]
			Lmed3 = dw_medium.object.mchar_med[3]
			
			// Control number for rows 1, 2 and 3
			Lconno1 = dw_medium.object.mchar_conno[1]
			Lconno2 = dw_medium.object.mchar_conno[2]
			Lconno3 = dw_medium.object.mchar_conno[3]

			IF  Lconno1= Lconno2 OR Lconno1 = Lconno3 OR Lconno2 = Lconno3 THEN
				this.Object.mchar_conno.ValidationMsg='Control numbers must be different between rows'
				RETURN 1
			END IF

			// If the media for the row one and two are RTB and row 3 is not
			IF Lmed1 = 'RTB' AND Lmed2 = 'RTB' AND (Lmed3 = 'P/B' OR Lmed3 = 'BR') THEN
				this.object.other_media_conno[1] = Lconno2
				this.object.other_media_conno[2] = Lconno1
			ELSEIF Lmed1 = 'RTB' AND Lmed3 = 'RTB' AND (Lmed2 = 'P/B' OR Lmed2 = 'BR') THEN
				this.object.other_media_conno[1] = Lconno3
				this.object.other_media_conno[3] = Lconno1
			ELSEIF Lmed2 = 'RTB' AND Lmed3 = 'RTB' AND (Lmed1 = 'P/B' OR Lmed1 = 'BR') THEN
				this.object.other_media_conno[2] = Lconno3
				this.object.other_media_conno[3] = Lconno2
			END IF
	END CHOOSE
ELSEIF dwo.Name = "mchar_med" THEN
	
		// 04/16/2008 new media functionality validation
		IF of_mediavalidation(row,this.object.mchar_med[row],data) < 1 THEN
			RETURN 1
		ELSE
			RETURN 0
		END IF
//
//	dw_medium.AcceptText()
//	
//    Li_bkseq = this.object.bkseq[row]
//    Ls_bkmed = this.object.bkmed[row]
//    lc_frflag = this.object.frflag[row]
//	lconno = this.Object.mchar_conno[row]
//	 
//    IF NOT(IsNull(Li_bkseq)) THEN
//		ls_msg = 'Book number '+ Ls_bkmed+string(Li_bkseq) +' has already been assigned to this control number, you may not change the media.'
//		this.Object.mchar_med.ValidationMsg = ls_msg
//		RETURN 1
//	ELSEIF NOT(IsNull(lc_frflag)) THEN
//		ls_msg = 'Control number '+ lconno +' has already been approved in final review, you may only change the media in the final review screen.'
//		this.Object.mchar_med.ValidationMsg = ls_msg
//		RETURN 1
//	END IF
//	
//	CHOOSE CASE this.rowcount()
//		CASE 2
//			// Media and book media for rows 1 an 2
//			Lmed1 = dw_medium.object.mchar_med[1]
//			Lmed2 = dw_medium.object.mchar_med[2]
//			
//			Lbkmed1 = dw_medium.object.bkmed[1]
//			Lbkmed2 = dw_medium.object.bkmed[2]
//			Ls_chno = dw_medium.object.mchar_chno[1]
//			
//			IF Lmed1 <> 'RTB' AND Lbkmed1 <> 'BR' THEN
//				this.Object.mchar_med.ValidationMsg='Braille and Print Braille can only have BR book media'
//				RETURN 1
//			ELSEIF Lmed2 <>'RTB' AND Lbkmed2 <> 'BR' THEN
//				this.Object.mchar_med.ValidationMsg='Braille and Print Braille can only have BR book media'
//				RETURN 1
//			ELSEIF Lmed1 = 'RTB' AND ( Lmed2 = 'BR' OR Lmed2 ='P/B') THEN
//				// Set the bkmed to RC for the first row
//				dw_medium.object.bkmed[1] = 'RC'
//				// create the second row
//				ll_row = this.InsertRow(0)
//				// Set the media to RTB
//				dw_medium.object.mchar_med[ll_row] = 'RTB'
//				// Set the bkmed to DB for the second row
//				dw_medium.object.bkmed[ll_row] = 'DB'
//				// Set the other conno number
//				dw_medium.object.other_media_conno[ll_row] = Lconno1
//				dw_medium.object.mchar_chno[ll_row]=ls_chno
//				
//				// 01/30/2008 set created_by and date for new row
//				dw_medium.object.created_by[ll_row]=gnv_app.of_getuserid()
//				dw_medium.object.create_date[ll_row]=today()
//				dw_medium.object.modified_by[ll_row]=gnv_app.of_getuserid()
//				dw_medium.object.update_date[ll_row]=today()
//				
//			ELSEIF Lmed2 = 'RTB' AND ( Lmed1 = 'BR' OR Lmed1 ='P/B') THEN
//				// Set the bkmed to RC for the first row
//				dw_medium.object.bkmed[2] = 'RC'
//				// create the second row
//				ll_row = this.InsertRow(0)
//				// Set the media to RTB
//				dw_medium.object.mchar_med[ll_row] = 'RTB'
//				// Set the bkmed to DB for the second row
//				dw_medium.object.bkmed[ll_row] = 'DB'
//				// Set the other conno number
//				dw_medium.object.other_media_conno[ll_row] = Lconno1
//				dw_medium.object.mchar_chno[ll_row]=ls_chno
//						
//				// 01/30/2008 set created_by and date for new row
//				dw_medium.object.created_by[ll_row]=gnv_app.of_getuserid()
//				dw_medium.object.create_date[ll_row]=today()
//				dw_medium.object.modified_by[ll_row]=gnv_app.of_getuserid()
//				dw_medium.object.update_date[ll_row]=today()
//			END IF
//			
//
//		CASE 3
//			// Media for rows 1 an 2
//			Lmed1 = dw_medium.object.mchar_med[1]
//			Lmed2 = dw_medium.object.mchar_med[2]
//			Lmed3 = dw_medium.object.mchar_med[3]
//			
//			Lbkmed1 = dw_medium.object.bkmed[1]
//			Lbkmed2 = dw_medium.object.bkmed[2]
//			Lbkmed3 = dw_medium.object.bkmed[3]
//			
//			// Control numbers for rows 1 an 2
//			Lconno1 = dw_medium.object.mchar_conno[1]
//			Lconno2 = dw_medium.object.mchar_conno[2]
//			Lconno3 = dw_medium.object.mchar_conno[3]
//
//			Ls_chno = dw_medium.object.mchar_chno[1]
//		
//			// If the media for the row 1 is equal to RTB
//			IF Lmed1 = 'RTB' AND Lmed2 = 'RTB' AND ( Lmed3 = 'BR' OR Lmed3 ='P/B') THEN
//				IF  Lconno1= Lconno2 THEN
//					this.Object.mchar_med.ValidationMsg='Control numbers must be different between the two rows of the RTBs.'
//					RETURN 1
//				END IF
//			ELSEIF Lmed1 = 'RTB' AND Lmed3 = 'RTB' AND (Lmed2 = 'P/B' OR Lmed2 = 'BR') THEN
//				IF Lconno1= Lconno3 THEN
//					this.Object.mchar_med.ValidationMsg='Control numbers must be different between the two rows of the RTBs.'
//					RETURN 1
//				END IF
//			ELSEIF Lmed2 = 'RTB' AND Lmed3 = 'RTB' AND (Lmed1 = 'P/B' OR Lmed1 = 'BR') THEN
//				IF  Lconno2= Lconno3 THEN
//					this.Object.mchar_med.ValidationMsg='Control numbers must be different between the two rows of the RTBs.'
//					RETURN 1
//				END IF
//			END IF
//			
//			IF Lmed1 <> 'RTB' AND Lbkmed1 <> 'BR' THEN
//				this.Object.mchar_med.ValidationMsg='Braille and Print Braille can only have BR book media'
//				RETURN 1
//			ELSEIF Lmed2 <> 'RTB' AND Lbkmed2 <> 'BR' THEN
//				this.Object.mchar_med.ValidationMsg='Braille and Print Braille can only have BR book media'
//				RETURN 1
//			ELSEIF Lmed3 <> 'RTB' AND Lbkmed3 <> 'BR' THEN
//				this.Object.mchar_med.ValidationMsg='Braille and Print Braille can only have BR book media'
//				RETURN 1
//			END IF
//			
////			ELSEIF Lmed1 = 'RTB' AND ( Lmed2 <> 'RTB' OR Lmed3 <> 'RTB' ) THEN
////				// Set the bkmed to RC for the first row
////				dw_medium.object.bkmed[1] = 'RC'
////				// create the second row
////				ll_row = this.InsertRow(0)
////				// Set the media to RTB
////				dw_medium.object.mchar_med[ll_row] = 'RTB'
////				// Set the bkmed to DB for the second row
////				dw_medium.object.bkmed[ll_row] = 'DB'
////				// Set the other conno number
////				dw_medium.object.other_media_conno[ll_row] = Lconno1
////				dw_medium.object.mchar_chno[ll_row]=ls_chno
////			ELSEIF  Lmed2 = 'RTB' AND ( Lmed1 <> 'RTB' OR Lmed1 <> 'RTB' ) THEN
////				// Set the bkmed to RC for the second row
////				dw_medium.object.bkmed[2] = 'RC'
////				// create the second row
////				ll_row = this.InsertRow(0)
////				// Set the media to RTB
////				dw_medium.object.mchar_med[ll_row] = 'RTB'
////				// Set the bkmed to DB for the second row
////				dw_medium.object.bkmed[ll_row] = 'DB'
////				// Set the other conno number
////				dw_medium.object.other_media_conno[ll_row] = Lconno1
////				dw_medium.object.mchar_chno[ll_row]=ls_chno
////			ELSEIF  Lmed3 = 'RTB' AND ( Lmed1 <> 'RTB' OR Lmed1 <> 'RTB' ) THEN
////				// Set the bkmed to RC for the third row
////				dw_medium.object.bkmed[3] = 'RC'
////				// create the second row
////				ll_row = this.InsertRow(0)
////				// Set the media to RTB
////				dw_medium.object.mchar_med[ll_row] = 'RTB'
////				// Set the bkmed to DB for the second row
////				dw_medium.object.bkmed[ll_row] = 'DB'
////				// Set the other conno number
////				dw_medium.object.other_media_conno[ll_row] = Lconno1
////				dw_medium.object.mchar_chno[ll_row]=ls_chno
////			END IF
////			
//			
//		CASE 1
//			// Media for rows 1 
//			Lmed1 = dw_medium.object.mchar_med[1]
//			
//			// Control numbers for row 1
//			Lconno1 = dw_medium.object.mchar_conno[1]
//			Ls_chno = dw_medium.object.mchar_chno[1]
//			
//			// If the media for the first row is equal to RTB
//			IF Lmed1 = 'RTB'  THEN
//				// Set the bkmed to RC for the first row
//				dw_medium.object.bkmed[1] = 'RC'
//				// create the second row
//				ll_row = this.InsertRow(0)
//				// Set the media to RTB
//				dw_medium.object.mchar_med[ll_row] = 'RTB'
//				// Set the bkmed to DB for the second row
//				dw_medium.object.bkmed[ll_row] = 'DB'
//				// Set the other conno number
//				dw_medium.object.other_media_conno[ll_row] = Lconno1
//				dw_medium.object.mchar_chno[ll_row]=ls_chno
//						
//				// 01/30/2008 set created_by and date for new row
//				dw_medium.object.created_by[ll_row]=gnv_app.of_getuserid()
//				dw_medium.object.create_date[ll_row]=today()
//				dw_medium.object.modified_by[ll_row]=gnv_app.of_getuserid()
//				dw_medium.object.update_date[ll_row]=today()
//				
//			ELSEIF Lmed1 <> 'RTB' THEN
//				// Set the bkmed to BR for the first row
//				dw_medium.object.bkmed[1] = 'BR'				
//			END IF
//				
//	END CHOOSE
END IF
end event

event pfc_addrow;long	ll_rc
int   ll_currow

// Insert row
if IsValid (inv_rowmanager) then
	ll_rc = inv_rowmanager.event pfc_addrow ()
else
	ll_rc = this.InsertRow (0) 
	dw_medium.object.mchar_chno[ll_rc]=Lchno
	dw_medium.object.mchar_med[ll_rc]=""
end if

// Notify the Linkage Service that a new row has been added.
IF IsValid ( inv_Linkage ) THEN 
	inv_Linkage.Event pfc_InsertRow (ll_rc) 
END IF 

this.ScrollToRow(ll_rc)
this.SetColumn(1)

return ll_rc
end event

event getfocus;call super::getfocus;m_pics_main.m_edit.m_deleterow.Enabled 	=	TRUE
m_pics_main.m_edit.m_addrow.Enabled 		=	TRUE

end event

event pfc_deleterow;//////////////////////////////////////////////////////////////////////////////////////////////////////////
//
//	Event: pfc_deleterow
// 
//	Description:
//	Before Deleting apply validation rules
//
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//	
//	Revision History
//
//	Developed by 	Date 				Version						Tracking#
//									
// Murali K.			04/16/2008       PICS 2.0 Modifications	 Change Request from NLS
//////////////////////////////////////////////////////////////////////////////////////////////////////////

long ll_row
long ll_found
Long Li_bkseq
String lconno,lmchar_med,original_media, ls_null, ls_msg,Ls_bkmed,lc_frflag, ls_med
integer	li_rc, li_rtn

ll_row = this.getrow()
Li_bkseq = dw_medium.object.bkseq[ll_row]
Ls_bkmed = dw_medium.object.bkmed[ll_row]
lc_frflag = dw_medium.object.frflag[ll_row]
lconno = dw_medium.Object.mchar_conno[ll_row]
ls_med = dw_medium.Object.mchar_med[ll_row]
 
/////////// First Level Validations
IF NOT(IsNull(Li_bkseq)) THEN
	ls_msg = 'Book number '+ Ls_bkmed+string(Li_bkseq) +' has already been assigned to  this control number, you cannot delete this row.'
	Messagebox('Error', ls_msg)
	RETURN -1
ELSEIF NOT(IsNull(lc_frflag)) THEN
	ls_msg = 'Control number '+ lconno +' has already been approved in final review, you cannot delete this row.'
	Messagebox('Error', ls_msg)
	RETURN -1
END IF
//////////////////////////

CHOOSE CASE ls_med
	CASE 'RTB'
		li_rtn = MessageBox("Warning",  'Are you sure you want to delete the RTB records?',   Exclamation!, YesNo!, 2)
		IF li_rtn = 1 THEN
  			Lconno = This.object.mchar_conno[ll_row] 
			li_rc = this.DeleteRow (0) 
			// delete reissue records also
			ll_found = dw_ri.Find("ri_conno = '" + string(lconno) + "'", 1, dw_ri.RowCount())
			IF ll_found > 0 THEN
				dw_ri.deleterow(ll_found)
			END IF


			  IF ls_bkmed = 'RC' THEN
					ll_found = THIS.Find("bkmed = 'DB'", 1, this.RowCount())
				ELSE
					ll_found = THIS.Find("bkmed = 'RC'", 1, this.RowCount())
			  END IF
  			Lconno = This.object.mchar_conno[ll_found] 
  			li_rc = this.deleterow(ll_found)
			ll_found = dw_ri.Find("ri_conno = '" + string(lconno) + "'", 1, dw_ri.RowCount())
			IF ll_found > 0 THEN
				dw_ri.deleterow(ll_found)
			END IF

		END IF
	CASE 'BR', 'P/B'
		li_rtn = MessageBox("Warning",  'Are you sure you want to delete the ' + ls_med + ' records?',   Exclamation!, YesNo!, 2)
		IF li_rtn = 1 THEN
			Lconno = This.object.mchar_conno[ll_row] 
			li_rc = this.DeleteRow (0) 
			ll_found = dw_ri.Find("ri_conno = '" + string(lconno) + "'", 1, dw_ri.RowCount())
			IF ll_found > 0 THEN
				dw_ri.deleterow(ll_found)
			END IF
		END IF
END CHOOSE

return li_rc

end event

event ue_postconstructor;call super::ue_postconstructor;this.of_SetTransObject(SQLServerTrans)

end event

event pfc_insertrow;long	ll_currow
long	ll_rc

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
	dw_medium.object.mchar_chno[ll_rc]=Lchno
	dw_medium.object.mchar_med[ll_rc]=""
end if

// Notify the Linkage Service that a new row has been added.
IF IsValid ( inv_Linkage ) THEN 
	inv_Linkage.Event pfc_InsertRow (ll_rc) 
END IF 

this.ScrollToRow(ll_rc)
this.SetColumn(1)

return ll_rc
end event

event sqlpreview;call super::sqlpreview;//
end event

event constructor;call super::constructor;this.SetTransObject(SqlServerTrans)
end event

type dw_coauthors from u_pics_dw within w_sheet_annotation
event ue_enterkey pbm_dwnprocessenter
integer x = 1093
integer y = 1184
integer width = 1033
integer height = 380
integer taborder = 70
string dataobject = "d_cc_coauth"
boolean hscrollbar = true
boolean livescroll = false
boolean ib_isupdateable = false
boolean ib_rmbmenu = false
boolean ib_rmbfocuschange = false
string is_updatesallowed = ""
end type

event ue_enterkey;call super::ue_enterkey;Send(Handle(this),256,9,Long(0,0))
return(1)
end event

event itemchanged;call super::itemchanged;m_pics_main.m_edit.m_deleterow.Enabled 	=	TRUE
m_pics_main.m_edit.m_addrow.Enabled 		=	TRUE

end event

event pfc_addrow;long	ll_rc

// Insert row
if IsValid (inv_rowmanager) then
	ll_rc = inv_rowmanager.event pfc_addrow ()
else
	ll_rc = this.InsertRow (0) 
	dw_coauthors.SetItem(ll_rc, "chno", Lchno)
end if

// Notify the Linkage Service that a new row has been added.
IF IsValid ( inv_Linkage ) THEN 
	inv_Linkage.Event pfc_InsertRow (ll_rc) 
END IF 

this.ScrollToRow(ll_rc)
this.SetColumn(1)

return ll_rc
end event

event getfocus;call super::getfocus;m_pics_main.m_edit.m_deleterow.Enabled 	=	TRUE
m_pics_main.m_edit.m_addrow.Enabled 		=	TRUE

end event

event pfc_deleterow;integer	li_rc

IF dw_coauthors.object.coauth[dw_coauthors.GetRow()]<>"" OR NOT(ISNull(dw_coauthors.object.coauth[dw_coauthors.GetRow()])) THEN
	dw_coauthors.SetItemStatus(1, 0, Primary!, DataModified!)
ELSE
	dw_coauthors.SetItemStatus(1, 0, Primary!, NotModified!)
END IF

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

event ue_postconstructor;call super::ue_postconstructor;dw_coauthors.of_SetLinkage(TRUE)
dw_coauthors.of_SetTransObject( SQLServerTrans )

end event

event pfc_insertrow;long	ll_currow
long	ll_rc

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
	dw_coauthors.object.chno[ll_rc]=Lchno
end if

// Notify the Linkage Service that a new row has been added.
IF IsValid ( inv_Linkage ) THEN 
	inv_Linkage.Event pfc_InsertRow (ll_rc) 
END IF 

this.ScrollToRow(ll_rc)
this.SetColumn(1)

return ll_rc
end event

event sqlpreview;call super::sqlpreview;//
end event

type cb_print from commandbutton within w_sheet_annotation
integer x = 1975
integer y = 1600
integer width = 183
integer height = 96
boolean bringtotop = true
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "&Print"
end type

event clicked;int ll_rows
long job
string Lconno

Lconno = em_conno.text
ll_rows = dw_cc_pms_par_report.retrieve(Lconno)
IF ll_rows > 0 THEN
	dw_cc_pms_par_report.TriggerEvent("pfc_Print")
ELSE
	// 04/21/2008 if the conno in  focus is deleted at least display updated message.
	IF ib_update THEN
		Messagebox('Update','Update Successful')
		ib_update=FALSE
	END IF
END IF

//OpenSheetwithparm(w_sheet_pics_ole_crystal,"editannotation",w_pics_main, 0, Original!)



end event

type st_wordcnt from statictext within w_sheet_annotation
integer x = 14
integer y = 1588
integer width = 165
integer height = 76
integer textsize = -9
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 33554432
boolean enabled = false
string text = "00"
alignment alignment = right!
boolean border = true
borderstyle borderstyle = stylelowered!
boolean focusrectangle = false
end type

type cb_zoom from commandbutton within w_sheet_annotation
integer x = 562
integer y = 1600
integer width = 306
integer height = 96
boolean bringtotop = true
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "&Zoom Anno"
end type

event clicked;dw_annotation.AcceptText()
dw_medium.AcceptText()
Lsexviol = LeftTrim(wf_sex_viol_lang())
open(w_notepad)
ib_anno_modified=TRUE
end event

type dw_cc_pms_par_report from u_pics_dw within w_sheet_annotation
boolean visible = false
integer x = 270
integer y = 688
integer width = 78
integer height = 68
integer taborder = 0
boolean enabled = false
string dataobject = "d_annotrpt_ec_report"
boolean vscrollbar = false
boolean livescroll = false
boolean ib_isupdateable = false
boolean ib_rmbmenu = false
boolean ib_rmbfocuschange = false
string is_updatesallowed = ""
end type

event ue_postconstructor;call super::ue_postconstructor;dw_cc_pms_par_report.of_SetTransObject( SQLServerTrans )

end event

type dw_cc_anno_rpt from u_pics_dw within w_sheet_annotation
boolean visible = false
integer x = 46
integer y = 688
integer width = 78
integer height = 68
integer taborder = 0
boolean enabled = false
string dataobject = "d_cc_anno_rpt"
boolean vscrollbar = false
boolean livescroll = false
boolean ib_isupdateable = false
boolean ib_rmbmenu = false
boolean ib_rmbfocuschange = false
string is_updatesallowed = ""
end type

event itemchanged;call super::itemchanged;dw_cc_anno_rpt.of_SetTransObject( SQLServerTrans )

end event

type dw_cdssubject from u_pics_dw within w_sheet_annotation
integer x = 2144
integer y = 1192
integer width = 1138
integer height = 372
integer taborder = 80
string dataobject = "d_cdssubject"
boolean hscrollbar = true
end type

event pfc_addrow;call super::pfc_addrow;long	ll_rc
int i

// Insert row
if IsValid (inv_rowmanager) then
	ll_rc = inv_rowmanager.event pfc_addrow ()
else
	ll_rc = this.InsertRow (0) 
	dw_cdssubject.SetItem(ll_rc, "chno", Lchno)
end if


// Notify the Linkage Service that a new row has been added.
IF IsValid ( inv_Linkage ) THEN 
	inv_Linkage.Event pfc_InsertRow (ll_rc) 
END IF 

for i = 1 to dw_cdssubject.rowcount()
	if dw_cdssubject.object.chno[i]="" or &
		isnull(dw_cdssubject.object.chno[i]) then
		dw_cdssubject.deleterow(i)
	end if
next

this.ScrollToRow(ll_rc)
this.SetColumn(1)

return ll_rc
end event

event type long pfc_insertrow();call super::pfc_insertrow;long	ll_currow
long	ll_rc
int i

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
	dw_cdssubject.object.chno[ll_rc]=Lchno
end if

// Notify the Linkage Service that a new row has been added.
IF IsValid ( inv_Linkage ) THEN 
	inv_Linkage.Event pfc_InsertRow (ll_rc) 
END IF 

for i = 1 to dw_cdssubject.rowcount()
	if dw_cdssubject.object.chno[i]="" or &
		isnull(dw_cdssubject.object.chno[i]) then
		dw_cdssubject.deleterow(i)
	end if
next

this.ScrollToRow(ll_rc)
this.SetColumn(1)

return ll_rc
end event

event ue_postconstructor;call super::ue_postconstructor;long rtncode, rtncode2
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
elseif dwo.name='cdssubjectcode' then
	li_cur=ldwc_cdssubcode.GetRow()
	ls_subject=ldwc_cdssubcode.GetItemString(li_cur,'pmsub_desc')
	this.object.cdssubject[row]=ls_subject
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
			this.SelectRow(0,false)
			this.SelectRow(row,true )
			exit
		else
			// if the partial code is not found, then blank out desc
			this.object.cdssubject[row]= ''
		end if
	next
end if
		
		
end event


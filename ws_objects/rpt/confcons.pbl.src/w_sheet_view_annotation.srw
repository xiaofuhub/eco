$PBExportHeader$w_sheet_view_annotation.srw
forward
global type w_sheet_view_annotation from w_sheet
end type
type st_bkno from u_st within w_sheet_view_annotation
end type
type st_or from statictext within w_sheet_view_annotation
end type
type em_bkseq from u_em within w_sheet_view_annotation
end type
type dw_cdssubject from u_pics_dw within w_sheet_view_annotation
end type
type dw_ri from u_pics_dw within w_sheet_view_annotation
end type
type st_1 from statictext within w_sheet_view_annotation
end type
type em_conno from uo_conno within w_sheet_view_annotation
end type
type st_2 from statictext within w_sheet_view_annotation
end type
type cb_find from commandbutton within w_sheet_view_annotation
end type
type cb_clear from commandbutton within w_sheet_view_annotation
end type
type cb_exit from commandbutton within w_sheet_view_annotation
end type
type mle_anno from u_mle within w_sheet_view_annotation
end type
type dw_annotation from u_pics_dw within w_sheet_view_annotation
end type
type dw_medium from u_pics_dw within w_sheet_view_annotation
end type
type dw_coauthors from u_pics_dw within w_sheet_view_annotation
end type
type cb_print from commandbutton within w_sheet_view_annotation
end type
type st_wordcnt from statictext within w_sheet_view_annotation
end type
type dw_cc_pms_par_report from u_pics_dw within w_sheet_view_annotation
end type
type dw_cc_anno_rpt from u_pics_dw within w_sheet_view_annotation
end type
end forward

shared variables

end variables
global type w_sheet_view_annotation from w_sheet
integer x = 5
integer y = 4
integer width = 3301
integer height = 1832
string title = "Annotation"
st_bkno st_bkno
st_or st_or
em_bkseq em_bkseq
dw_cdssubject dw_cdssubject
dw_ri dw_ri
st_1 st_1
em_conno em_conno
st_2 st_2
cb_find cb_find
cb_clear cb_clear
cb_exit cb_exit
mle_anno mle_anno
dw_annotation dw_annotation
dw_medium dw_medium
dw_coauthors dw_coauthors
cb_print cb_print
st_wordcnt st_wordcnt
dw_cc_pms_par_report dw_cc_pms_par_report
dw_cc_anno_rpt dw_cc_anno_rpt
end type
global w_sheet_view_annotation w_sheet_view_annotation

type variables
string Lchno,Lsexviol
int anno_stage
boolean ib_anno_modified=FALSE,ri_exist=FALSE,anno_exist=FALSE
nvo_val_init_con lnvo_val_anno
end variables
forward prototypes
public subroutine wf_disable_buttons ()
public subroutine wf_enable_buttons ()
public subroutine wf_validate_ricd ()
public subroutine wf_disable_all ()
public function boolean wf_validate_anno_stage (string conno, string annoinit, string cdsinit, string pmsinit, integer annostg)
public function boolean wf_validate_group_access (string luserid, string lgroupid)
public function string wf_sex_viol_lang ()
public subroutine wf_set_spinoff_fields ()
public subroutine wf_count_words ()
public subroutine wf_view_all ()
end prototypes

public subroutine wf_disable_buttons ();this.cb_clear.Enabled=FALSE
this.cb_find.Enabled=TRUE
this.cb_print.Enabled=FALSE

end subroutine

public subroutine wf_enable_buttons ();this.cb_clear.Enabled=TRUE
this.cb_find.Enabled=FALSE
this.cb_print.Enabled = TRUE

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

public subroutine wf_disable_all ();dw_annotation.Enabled = FALSE
dw_coauthors.Enabled = FALSE
dw_medium.Enabled = FALSE
//mle_anno.Enabled = FALSE
cb_clear.Enabled = FALSE
cb_print.Enabled = TRUE
cb_find.Enabled = FALSE

end subroutine

public function boolean wf_validate_anno_stage (string conno, string annoinit, string cdsinit, string pmsinit, integer annostg);int rtn

CHOOSE CASE annostg
	// Add Annotation
	CASE 1
		IF (IsNull(annoinit)=FALSE OR annoinit<>"") AND &
			(IsNull(cdsinit)=FALSE OR cdsinit<>"" ) AND &
			(IsNull(pmsinit)=FALSE OR pmsinit<>"" ) THEN
   		MessageBox("Error", "Control Number: "+conno+" has been initialized by ~n CDS and PMS staff." ,StopSign!)
			RETURN FALSE
		ELSEIF (IsNull(annoinit)=FALSE OR annoinit<>"") AND &
				 (IsNull(cdsinit)=FALSE OR cdsinit<>"") THEN
   		MessageBox("Error", "Control Number: "+conno+" has been initialized by ~n CDS staff." ,StopSign!)
			RETURN FALSE
		ELSEIF (IsNull(annoinit)=FALSE OR annoinit<>"") THEN
   		rtn = MessageBox("Warning", "Annotation already exists.~nDo you really want to continue?" ,Question!, YesNo!, 1)
			IF rtn = 1 THEN
				dw_annotation.Object.ttlinit_annoinit.Protect=0
				dw_annotation.Object.ttlinit_cdinit.Protect=1
				dw_annotation.Object.ttlinit_pminit.Protect=1
				RETURN TRUE
			ELSE
				RETURN FALSE
			END IF
		ELSE
			dw_annotation.Object.ttlinit_annoinit.Protect=0
			dw_annotation.Object.ttlinit_cdinit.Protect=1
			dw_annotation.Object.ttlinit_pminit.Protect=1
			dw_annotation.Object.ttlinit_annoinit.Edit.Required='Yes'
			RETURN TRUE
		END IF
	// CDS Edit Annotation
	CASE 2
		IF (IsNull(annoinit)=TRUE OR annoinit="") THEN
   		MessageBox("Error", "Control Number: "+conno+", has not yet been initialized." ,StopSign!)
			RETURN FALSE
		ELSEIF (IsNull(annoinit)=FALSE  OR annoinit<>"") AND &
				 (IsNull(cdsinit)=FALSE  OR cdsinit<>"") AND &
				 (IsNull(pmsinit)=FALSE OR pmsinit<>"") THEN
   		MessageBox("Error", "Control Number: "+conno+" has been initialized by ~n CDS and PMS staff." ,StopSign!)
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
				RETURN FALSE
			END IF
		ELSE
			RETURN FALSE
		END IF
	// PMS Edit Annotation
	CASE 3
		IF (IsNull(annoinit)=TRUE OR annoinit="") THEN
   		MessageBox("Error", " Control Number: "+conno+", has not yet been initialized." ,StopSign!)
			RETURN FALSE
		ELSEIF (IsNull(cdsinit)=TRUE OR cdsinit="") THEN
   		MessageBox("Error", " Control Number: "+conno+", has not yet been initialized by CDS staff." ,StopSign!)
			RETURN FALSE
		ELSEIF (IsNull(annoinit)=FALSE OR annoinit<>"") AND &
				 (IsNull(cdsinit)=FALSE OR cdsinit<>"") AND &
				 (IsNull(pmsinit)=FALSE OR pmsinit="") THEN
   		rtn = MessageBox("Warning", " Annotation has already been signed off by PMS Staff.~nDo you really want to continue?" ,Question!, YesNo!, 1)
			IF rtn = 1 THEN
				//dw_coauthors.Enabled = FALSE
				dw_medium.Enabled = FALSE
				dw_annotation.object.ttlinit_chno.protect=1   
				dw_annotation.Object.ttlinit_annoinit.Protect=1
				dw_annotation.Object.ttlinit_cdinit.Protect=1
				dw_annotation.Object.ttlinit_pminit.Protect=0
				dw_annotation.Object.ttlinit_pminit.Edit.Required='Yes'
				RETURN TRUE
			ELSE
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
			RETURN FALSE
		END IF
	// CDS Bibliographic Edit Annotation
	CASE 4
		IF (IsNull(annoinit)=TRUE OR annoinit="") THEN
   		MessageBox("Error", " Control Number: "+conno+", has not yet been initialized." ,StopSign!)
			RETURN FALSE
		ELSEIF (IsNull(cdsinit)=TRUE OR cdsinit="") THEN
   		MessageBox("Error", " Control Number: "+conno+", has not yet been initialized by CDS staff." ,StopSign!)
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
			RETURN FALSE
		ELSE
			RETURN FALSE
		END IF
	// View/Print Annotation
	CASE 5
		RETURN TRUE		
	CASE ELSE
		RETURN FALSE
END CHOOSE
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
ELSEIF sqlservertrans.SQLCode = 100 THEN
	// Row Not found
	MessageBox("ERROR","Userid ~'"+Luserid+"~' does not belong to ~'"+Lgroupid+"~' group. ")
	RETURN FALSE
ELSE
	// Row found
	RETURN TRUE
END IF
end function

public function string wf_sex_viol_lang ();string ls_sexcd,ls_viol,ls_lang,ls_text,ls_bestsell

ls_sexcd = dw_annotation.Getitemstring(dw_annotation.Getrow(),"ttlinit_sexcd")
ls_viol 	= dw_annotation.Getitemstring(dw_annotation.Getrow(),"ttlinit_violencecd")
ls_lang 	= dw_annotation.Getitemstring(dw_annotation.Getrow(),"ttlinit_stronglang")
ls_bestsell 	= dw_annotation.Getitemstring(dw_annotation.Getrow(),"ttlinit_bestseller")

CHOOSE CASE ls_sexcd
	CASE 'E'
		CHOOSE CASE ls_viol
			CASE 'Y'
				CHOOSE CASE ls_lang
					CASE 'Y'
						ls_text = "Explicit descriptions of sex, and violence, and strong language. "
					CASE 'S'
						ls_text = "Explicit descriptions of sex, and violence, and some strong language. "
					CASE 'N'
						ls_text = "Explicit descriptions of sex, and violence. "
				END CHOOSE
			CASE 'S'
				CHOOSE CASE ls_lang
					CASE 'Y'
						ls_text = "Explicit descriptions of sex, and strong language, and some violence. "
					CASE 'S'
						ls_text = "Explicit descriptions of sex, and some strong language, and some violence. "
					CASE 'N'
						ls_text = "Explicit descriptions of sex, and some violence. "
				END CHOOSE
			CASE 'N'
				CHOOSE CASE ls_lang
					CASE 'Y'
						ls_text = "Explicit descriptions of sex, and strong language. "
					CASE 'S'
						ls_text = "Explicit descriptions of sex, and some strong language. "
					CASE 'N'
						ls_text = "Explicit descriptions of sex. "
				END CHOOSE
		END CHOOSE
	CASE 'B'
		CHOOSE CASE ls_viol
			CASE 'Y'
				CHOOSE CASE ls_lang
					CASE 'Y'
						ls_text = "Some explicit descriptions of sex, and violence, and strong language. "
					CASE 'S'
						ls_text = "Some explicit descriptions of sex, and violence, and some strong language. "
					CASE 'N'
						ls_text = "Some explicit descriptions of sex, and violence. "
				END CHOOSE
			CASE 'S'
				CHOOSE CASE ls_lang
					CASE 'Y'
						ls_text = "Some explicit descriptions of sex, and strong language, and some violence. "
					CASE 'S'
						ls_text = "Some explicit descriptions of sex, and some strong language, and some violence. "
					CASE 'N'
						ls_text = "Some explicit descriptions of sex, and some violence. "
				END CHOOSE
			CASE 'N'
				CHOOSE CASE ls_lang
					CASE 'Y'
						ls_text = "Some explicit descriptions of sex, and strong language. "
					CASE 'S'
						ls_text = "Some explicit descriptions of sex, and some strong language. "
					CASE 'N'
						ls_text = "Some explicit descriptions of sex. "
				END CHOOSE
		END CHOOSE
	CASE 'S'
		CHOOSE CASE ls_viol
			CASE 'Y'
				CHOOSE CASE ls_lang
					CASE 'Y'
						ls_text = "Some descriptions of sex, and violence, and strong language. "
					CASE 'S'
						ls_text = "Some descriptions of sex, and violence, and some strong language. "
					CASE 'N'
						ls_text = "Some descriptions of sex, and violence. "
				END CHOOSE
			CASE 'S'
				CHOOSE CASE ls_lang
					CASE 'Y'
						ls_text = "Some descriptions of sex, and strong language, and some violence. "
					CASE 'S'
						ls_text = "Some descriptions of sex, and some strong language, and some violence. "
					CASE 'N'
						ls_text = "Some descriptions of sex, and some violence. "
				END CHOOSE
			CASE 'N'
				CHOOSE CASE ls_lang
					CASE 'Y'
						ls_text = "Some descriptions of sex, and strong language. "
					CASE 'S'
						ls_text = "Some descriptions of sex, and some strong language. "
					CASE 'N'
						ls_text = "Some descriptions of sex. "
				END CHOOSE
		END CHOOSE
	CASE 'N'
		CHOOSE CASE ls_viol
			CASE 'Y'
				CHOOSE CASE ls_lang
					CASE 'Y'
						ls_text = "Violence, and strong language. "
					CASE 'S'
						ls_text = "Violence, and some strong language. "
					CASE 'N'
						ls_text = "Violence. "
				END CHOOSE
			CASE 'S'
				CHOOSE CASE ls_lang
					CASE 'Y'
						ls_text = "Strong language, and some violence. "
					CASE 'S'
						ls_text = "Some strong language, and some violence. "
					CASE 'N'
						ls_text = "Some violence. "
				END CHOOSE
			CASE 'N'
				CHOOSE CASE ls_lang
					CASE 'Y'
						ls_text = "Strong language. "
					CASE 'S'
						ls_text = "Some strong language. "
					CASE 'N'
						ls_text = ""
				END CHOOSE
		END CHOOSE
END CHOOSE

IF ls_bestsell = 'Y' THEN
	ls_text = ls_text + "Bestseller. "
END IF
return ls_text


end function

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
			dw_annotation.object.ttlinit_auth.TabSequence = '10'
			dw_annotation.object.ttlinit_authfn.TabSequence = '20'
			dw_annotation.object.ttlinit_ahonorific.TabSequence = '30'
			dw_annotation.object.ttlinit_ttlart.TabSequence = '40'
			dw_annotation.object.ttlinit_ttl.TabSequence = '50'
			dw_annotation.object.ttlinit_note.TabSequence = '60'
			dw_annotation.object.ttlinit_serttl.TabSequence = '70'
			dw_annotation.object.ttlinit_seqnote.TabSequence = '80'

end subroutine

public subroutine wf_count_words ();Integer			ll_count,ll_count_sex
string 			ls_space,ls_source,ls_sex
n_cst_string 	inv_string

ls_source = mle_anno.text
Ls_sex = wf_sex_viol_lang()
ls_space = " "

ls_source =  inv_string.of_GlobalReplace(ls_source, "       ", " ")
ls_source =  inv_string.of_GlobalReplace(ls_source, "      ", " ")
ls_source =  inv_string.of_GlobalReplace(ls_source, "     ", " ")
ls_source =  inv_string.of_GlobalReplace(ls_source, "    ", " ")
ls_source =  inv_string.of_GlobalReplace(ls_source, "   ", " ")
ls_source =  inv_string.of_GlobalReplace(ls_source, "  ", " ")

ll_count = inv_string.of_CountOccurrences(ls_source, ls_space, TRUE)

IF ls_sex<> "" THEN
	ll_count_sex = inv_string.of_CountOccurrences(ls_sex, ls_space, TRUE)
	ll_count = ll_count + ll_count_sex
END IF

st_wordcnt.text = String (ll_count+1, '##0')

end subroutine

public subroutine wf_view_all ();dw_annotation.Enabled = FALSE
dw_ri.Object.ri_conno.Protect='1'
dw_ri.Object.ri_prevbkseq.Protect='1'
dw_ri.Object.ri_prevbkmed.Protect='1'
dw_medium.Object.mchar_ricd.Protect='1'
dw_medium.Object.mchar_med.Protect='1'
dw_medium.Object.mchar_conno.Protect='1'
dw_medium.Object.mchar_priority.Protect='1'
dw_coauthors.Object.coauth.Protect='1'
dw_coauthors.Object.coauthfn.Protect='1'
dw_coauthors.Object.chonorific.Protect='1'
cb_clear.Enabled = FALSE
cb_print.Enabled = TRUE
cb_find.Enabled = FALSE

end subroutine

event open;call super::open;Boolean invalid_userid=FALSE
// Open the sheet in Maximized mode
this.windowstate = maximized!

lnvo_val_anno = CREATE nvo_val_init_con

CHOOSE CASE Message.StringParm	
	CASE "View"
		invalid_userid = FALSE
		w_sheet_view_annotation.Title = "View/Print Annotation"
		cb_find.Default = TRUE
		anno_stage=5
END CHOOSE

IF Invalid_userid = TRUE THEN
	close(this)
ELSE	
	wf_disable_buttons()
	em_conno.setfocus()
	m_pics_main.m_file.m_print.Enabled 			=	TRUE
	m_pics_main.m_file.m_pagesetup.Enabled		=	TRUE
	m_pics_main.m_file.m_printimmediate.Enabled	=	TRUE
END IF
end event

on w_sheet_view_annotation.create
int iCurrent
call super::create
this.st_bkno=create st_bkno
this.st_or=create st_or
this.em_bkseq=create em_bkseq
this.dw_cdssubject=create dw_cdssubject
this.dw_ri=create dw_ri
this.st_1=create st_1
this.em_conno=create em_conno
this.st_2=create st_2
this.cb_find=create cb_find
this.cb_clear=create cb_clear
this.cb_exit=create cb_exit
this.mle_anno=create mle_anno
this.dw_annotation=create dw_annotation
this.dw_medium=create dw_medium
this.dw_coauthors=create dw_coauthors
this.cb_print=create cb_print
this.st_wordcnt=create st_wordcnt
this.dw_cc_pms_par_report=create dw_cc_pms_par_report
this.dw_cc_anno_rpt=create dw_cc_anno_rpt
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_bkno
this.Control[iCurrent+2]=this.st_or
this.Control[iCurrent+3]=this.em_bkseq
this.Control[iCurrent+4]=this.dw_cdssubject
this.Control[iCurrent+5]=this.dw_ri
this.Control[iCurrent+6]=this.st_1
this.Control[iCurrent+7]=this.em_conno
this.Control[iCurrent+8]=this.st_2
this.Control[iCurrent+9]=this.cb_find
this.Control[iCurrent+10]=this.cb_clear
this.Control[iCurrent+11]=this.cb_exit
this.Control[iCurrent+12]=this.mle_anno
this.Control[iCurrent+13]=this.dw_annotation
this.Control[iCurrent+14]=this.dw_medium
this.Control[iCurrent+15]=this.dw_coauthors
this.Control[iCurrent+16]=this.cb_print
this.Control[iCurrent+17]=this.st_wordcnt
this.Control[iCurrent+18]=this.dw_cc_pms_par_report
this.Control[iCurrent+19]=this.dw_cc_anno_rpt
end on

on w_sheet_view_annotation.destroy
call super::destroy
destroy(this.st_bkno)
destroy(this.st_or)
destroy(this.em_bkseq)
destroy(this.dw_cdssubject)
destroy(this.dw_ri)
destroy(this.st_1)
destroy(this.em_conno)
destroy(this.st_2)
destroy(this.cb_find)
destroy(this.cb_clear)
destroy(this.cb_exit)
destroy(this.mle_anno)
destroy(this.dw_annotation)
destroy(this.dw_medium)
destroy(this.dw_coauthors)
destroy(this.cb_print)
destroy(this.st_wordcnt)
destroy(this.dw_cc_pms_par_report)
destroy(this.dw_cc_anno_rpt)
end on

event pfc_preopen;call super::pfc_preopen;this.of_SetPreference(TRUE)
this.inv_preference.of_SetToolBars(TRUE)
this.inv_preference.of_SetWindow(TRUE)
this.of_SetResize(TRUE)
this.inv_resize.of_SetOrigSize(this.workspacewidth(),this.workspaceheight())
inv_resize.of_Register(dw_annotation, "Scale")
inv_resize.of_Register(dw_coauthors, "Scale")
inv_resize.of_Register(dw_medium, "Scale")
inv_resize.of_Register(dw_ri, "Scale")
inv_resize.of_Register(dw_cdssubject, "Scale")
inv_resize.of_Register(em_conno, "Scale")
inv_resize.of_Register(em_bkseq, "Scale")
inv_resize.of_Register(cb_clear, "Scale")
inv_resize.of_Register(cb_exit, "Scale")
inv_resize.of_Register(cb_find, "Scale")
inv_resize.of_Register(mle_anno,"Scale")
inv_resize.of_Register(st_1, "Scale")
inv_resize.of_Register(st_2, "Scale")
inv_resize.of_Register(st_or, "Scale")
inv_resize.of_Register(st_bkno, "Scale")
inv_resize.of_Register(cb_print, "Scale")
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
//	If IsValid(gnv_app.inv_error) Then
//		li_msg = gnv_app.inv_error.of_Message('pfc_closequery_savechanges',  &
//					ls_msgparms, gnv_app.iapp_object.DisplayName)		
//	Else
//		li_msg = MessageBox ( gnv_app.iapp_object.DisplayName, &
//					"Do you want to save changes?", exclamation!, YesNoCancel!)
//	End If
////	Choose Case li_msg
//		Case 1
//			// YES - Update
//			// If the update fails, prevent the window from closing
//			rtn = cb_update.Event Clicked()
//			if rtn = 1 THEN
//				RETURN 0
//			end if
//		Case 2
//			// NO - Allow the window to be closed without saving changes
//			Return 0
//		Case 3
//			// CANCEL -  Prevent the window from closing
//	End Choose
End If

// Prevent the window from closing
ib_closestatus = False
Return 1
end event

type st_bkno from u_st within w_sheet_view_annotation
integer x = 37
integer y = 256
integer width = 343
integer height = 88
fontcharset fontcharset = ansi!
string text = "Book Number"
end type

type st_or from statictext within w_sheet_view_annotation
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

type em_bkseq from u_em within w_sheet_view_annotation
integer x = 37
integer y = 344
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

type dw_cdssubject from u_pics_dw within w_sheet_view_annotation
integer x = 2194
integer y = 1180
integer width = 1061
integer height = 380
integer taborder = 80
string dataobject = "d_cdssubject"
end type

event constructor;call super::constructor;this.of_SetLinkage(TRUE)
this.of_SetTransObject( SQLServerTrans )

end event

type dw_ri from u_pics_dw within w_sheet_view_annotation
event ue_enterkey pbm_dwnprocessenter
event pfc_hinttext pbm_mousemove
integer x = 2546
integer y = 876
integer width = 709
integer height = 300
integer taborder = 60
string dataobject = "d_ri_confm"
boolean hscrollbar = true
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

event itemchanged;call super::itemchanged;int cnt_ri,rtn
string lconno
IF dwo.Name = "ri_prevbkmed" THEN
	IF data<>"" THEN
		// If you choose a media for the previouse book number
		IF data<>'RC' AND data<>'FD' AND data<>'BR' AND data<>'BRA' AND &
			data<>'P/B' AND data<>'CB' AND data<>'RD' AND data<>'TB' THEN
			RETURN 1
		ELSE
			dw_ri.Object.ri_prevbkseq.tabsequence='50'
			dw_ri.Object.ri_prevbkseq.EditMask.Required='Yes'
			dw_medium.Object.mchar_ricd.tabsequence='60'
			dw_medium.Object.mchar_ricd.Edit.Required='Yes'
		END IF
	ELSE
		// If you want to delete the existing previouse book number
		// or there are no previouse book number existed.
		rtn = MessageBox("Warning","Do you want to remove the previous book number?",Question!,YesNo!,1)
		if rtn = 1 THEN		
			dw_ri.Object.ri_prevbkseq.tabsequence='0'
			dw_medium.Object.mchar_ricd.tabsequence='0'
			dw_ri.Object.ri_prevbkseq.EditMask.Required='No'
			dw_medium.Object.mchar_ricd.Edit.Required='No'
			dw_ri.Object.ri_prevbkseq[row]=0
			dw_medium.object.mchar_ricd[row]=""
			lconno = dw_medium.Object.mchar_conno[row]
			select count(*) into :cnt_ri from ri where conno = :lconno using sqlservertrans;
			IF f_check_dberror(sqlservertrans,"RI") THEN
				IF cnt_ri > 0 THEN
					DELETE FROM ri WHERE conno=:lconno USING SqlServerTrans;
					IF f_check_dberror(sqlservertrans,"RI") THEN
						commit using sqlservertrans;
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
ELSEIF dwo.Name = "mchar_med" THEN
	String Lmed1,Lmed2,Lmed3,Lconno1,Lconno2,Lconno3
	dw_medium.AcceptText()
	CHOOSE CASE row
		CASE 2
			Lmed1 = dw_medium.object.mchar_med[1]
			Lmed2 = dw_medium.object.mchar_med[2]
			Lconno1 = dw_medium.object.mchar_conno[1]
			Lconno2 = dw_medium.object.mchar_conno[2]
			IF Lmed1=Lmed2 or Lconno1=Lconno2 THEN
				MessageBox("Error", "Media/Control numbers must be different between rows!",Information!)
				RETURN -1			
			ELSEIF ((Lmed1="RC" and Lmed2="FD") or (Lmed1="FD" and Lmed2="RC")) THEN
				IF dw_annotation.object.ttlinit_oneliner="" THEN
					MessageBox("Error", "One Line Annotation is required for FD/RC books.",Information!)
					RETURN -1
				END IF
			END IF				
		CASE 3
			Lmed1 = dw_medium.object.mchar_med[1]
			Lmed2 = dw_medium.object.mchar_med[2]
			Lmed3 = dw_medium.object.mchar_med[3]
			Lconno1 = dw_medium.object.mchar_conno[1]
			Lconno2 = dw_medium.object.mchar_conno[2]
			Lconno3 = dw_medium.object.mchar_conno[3]
			IF Lmed1=Lmed2 or Lmed1=Lmed3 or Lmed2=Lmed3 or Lconno1=Lconno2 or Lconno1=Lconno3 or Lconno2=Lconno3 THEN
				MessageBox("Error", "Media/Control numbers must be different between rows!",Information!)
				RETURN -1			
			ELSEIF ((Lmed1="RC" and Lmed2="FD") or (Lmed1="FD" and Lmed2="RC")) THEN
				IF dw_annotation.object.ttlinit_oneliner="" THEN
					MessageBox("Error", "One Line Annotation is required for FD/RC books.",Information!)
					RETURN -1			
				END IF
			END IF				
	END CHOOSE
END IF
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

dw_medium.SetItemStatus(1, 0, Primary!, DataModified!)

return li_rc

end event

event pfc_addrow;long	ll_rc
int   ll_currow

// Insert row
if IsValid (inv_rowmanager) then
	ll_rc = inv_rowmanager.event pfc_addrow ()
else
	ll_rc = this.InsertRow (0) 
	dw_medium.object.mchar_chno[ll_rc]=Lchno
end if

// Notify the Linkage Service that a new row has been added.
IF IsValid ( inv_Linkage ) THEN 
	inv_Linkage.Event pfc_InsertRow (ll_rc) 
END IF 

this.ScrollToRow(ll_rc)
this.SetColumn(1)

return ll_rc
end event

event ue_postconstructor;call super::ue_postconstructor;dw_medium.of_SetTransObject(SQLServerTrans)

end event

event constructor;call super::constructor;this.SetTransObject(SqlServerTrans)
end event

type st_1 from statictext within w_sheet_view_annotation
integer x = 32
integer y = 20
integer width = 361
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

type em_conno from uo_conno within w_sheet_view_annotation
integer x = 32
integer y = 100
integer width = 357
integer height = 84
integer taborder = 10
long textcolor = 255
maskdatatype maskdatatype = stringmask!
string displaydata = "Ä"
end type

type st_2 from statictext within w_sheet_view_annotation
integer x = 5
integer y = 892
integer width = 1120
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

type cb_find from commandbutton within w_sheet_view_annotation
event clicked pbm_bnclicked
string tag = "Find the record"
integer x = 2368
integer y = 1592
integer width = 270
integer height = 100
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
integer i,row_cntr
string Lconno,Lcdsinit,Lannoinit,Lpmsinit,annotxt,ls_priority,ls_lcno,Lspecattn

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
		Lspecattn = dw_annotation.object.ttlinit_specattn[1]
		IF IsNull(Lspecattn) OR TRIM(Lspecattn)="" THEN
			dw_annotation.object.ttlinit_specattn[1]="N"
		END IF
		ls_lcno = dw_annotation.Getitemstring(1,"ttlinit_lcno")
		IF ISNULL(ls_lcno) OR ls_lcno = "" THEN
			dw_annotation.Setitem(1,"ttlinit_lcno","00000001")
		END IF
		Lchno	= dw_annotation.object.ttlinit_chno[1]

		// This section of the code validate the annotation stages
		
		Lcdsinit  = dw_annotation.object.ttlinit_cdinit[1]
		Lannoinit = dw_annotation.object.ttlinit_annoinit[1]
		Lpmsinit  = dw_annotation.object.ttlinit_pminit[1]
		
		IF wf_validate_anno_stage(Lconno,Lannoinit,Lcdsinit,Lpmsinit,anno_stage) THEN
			// Retrieve data for medium section
			row_cntr = dw_medium.Retrieve(Lchno)
			FOR i=1 to row_cntr 
				ls_priority = dw_medium.Getitemstring(i,"mchar_priority")
				IF ISNULL(ls_priority) OR TRIM(ls_priority) = "" THEN
					dw_medium.Setitem(i,"mchar_priority","N")
				END IF
			NEXT
			dw_ri.Retrieve(Lconno)
			// Retrieve data for coauthors
			dw_coauthors.Retrieve(Lchno)
			// Retrieve data for cdssubject
			dw_cdssubject.Retrieve(Lchno)
			// Fill in the annotation
			Select anno into :annotxt from annotation where chno = :Lchno using SqlServerTrans;
			IF SqlServerTrans.SQLCode = 0  THEN
				n_cst_string 	inv_string
				// annotxt =  inv_string.of_RemoveNonPrint(annotxt)
				annotxt =  inv_string.of_Trim(annotxt)
				annotxt =  inv_string.of_GlobalReplace(annotxt, "       ", " ")
				annotxt =  inv_string.of_GlobalReplace(annotxt, "      ", " ")
				annotxt =  inv_string.of_GlobalReplace(annotxt, "     ", " ")
				annotxt =  inv_string.of_GlobalReplace(annotxt, "    ", " ")
				annotxt =  inv_string.of_GlobalReplace(annotxt, "   ", " ")
				annotxt =  inv_string.of_GlobalReplace(annotxt, "  ", " ")
				mle_anno.text = annotxt
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
			END IF
			// IF is view only disable all the bottoms
			IF anno_stage=5 THEN
				wf_view_all()
				cb_clear.Enabled = TRUE
			ELSEIF anno_stage=4 THEN
				cb_clear.Enabled = TRUE
				cb_find.Enabled = FALSE
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
ELSE
	MessageBox("Error", "Please enter the Control Number." ,StopSign!, OK!, 2)
	SetNull(em_bkseq.text)
	SetNull(em_conno.text)
	em_conno.SetFocus()
END IF
end event

type cb_clear from commandbutton within w_sheet_view_annotation
event clicked pbm_bnclicked
string tag = "Clear the screen"
integer x = 2661
integer y = 1592
integer width = 270
integer height = 100
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
dw_ri.Reset()
dw_ri.InsertRow(0)
dw_coauthors.Reset()
dw_coauthors.insertrow(0)
dw_coauthors.Enabled = TRUE
dw_cdssubject.Reset()
anno_exist=FALSE
IF IsValid(w_notepad) THEN close(w_notepad) 
st_wordcnt.text="00"
mle_anno.text=""
em_conno.text=""
SetNull(Lchno)
em_conno.SetFocus()
wf_disable_buttons()
IF anno_stage = 4 THEN
	
END IF

end event

type cb_exit from commandbutton within w_sheet_view_annotation
event clicked pbm_bnclicked
string tag = "Exit the screen"
integer x = 2967
integer y = 1592
integer width = 270
integer height = 104
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

type mle_anno from u_mle within w_sheet_view_annotation
event pfc_hinttext pbm_mousemove
event ue_char_entered pbm_char
event ue_key_up pbm_keyup
event ue_lmouse_up pbm_lbuttonup
string tag = "Please enter the annotation"
integer y = 956
integer width = 1115
integer height = 612
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

event ue_key_up;call super::ue_key_up;wf_count_words()

end event

event ue_lmouse_up;call super::ue_lmouse_up;wf_count_words()


end event

event getfocus;call super::getfocus;m_pics_main.m_edit.m_deleterow.Enabled 	=	FALSE
m_pics_main.m_edit.m_addrow.Enabled 		=	FALSE

end event

event modified;call super::modified;ib_anno_modified=TRUE
end event

type dw_annotation from u_pics_dw within w_sheet_view_annotation
event ue_enterkey pbm_dwnprocessenter
event pfc_hinttext pbm_mousemove
integer x = 398
integer y = 20
integer width = 2862
integer height = 848
integer taborder = 30
string dataobject = "d_annotation"
boolean hscrollbar = true
boolean livescroll = false
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

event itemchanged;call super::itemchanged;Boolean ans
int rtn,i
IF dwo.Name = "ttlinit_casub" THEN
	ans =	lnvo_val_anno.of_val_casub_ajyfn(dw_annotation.object.ttlinit_ajyfn[1],data)
	IF ans = FALSE THEN
		MessageBox("ERROR","Invalid ~'AJYFN~' Code against ~'CASUB~' Code.",StopSign!)
		RETURN 0
	END IF
ELSEIF dwo.Name = "ttlinit_lang" THEN
	string lang
	lang = left(data,3)
	IF lang <> 'ENG' AND lang <> 'OFL' AND lang <> 'DUT' AND lang <> 'GER' AND lang <> 'FRE' AND &
		lang <> 'CHI' AND lang <> 'ARA' AND lang <> 'FIN' AND lang <> 'HAU' AND lang <> 'SPA'THEN 
		RETURN 1
	END IF
ELSEIF dwo.Name = "ttlinit_lcno" THEN
	string lcno
	lcno = TRIM(data)
	IF len(lcno) <> 8 THEN 
		RETURN 1
	END IF
ELSEIF dwo.Name = "ttlinit_ajyfn" THEN
	string ajyfn
	ajyfn = left(data,2)
	IF ajyfn <> 'AF' AND ajyfn <> 'AN' AND ajyfn <> 'JF' AND ajyfn <> 'JN' AND &
		ajyfn <> 'YF' AND ajyfn <> 'YN' THEN 
		RETURN 1
	ELSE
		IF dw_annotation.object.ttlinit_casub[1] <> "" THEN
			ans =	lnvo_val_anno.of_val_casub_ajyfn(data,dw_annotation.object.ttlinit_casub[1])
			IF ans = FALSE THEN
				MessageBox("ERROR","Invalid ~'AJYFN~' Code against ~'CASUB~' Code.",StopSign!)
				RETURN 0
			END IF
		END IF
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
	IF aepcd = 'L' THEN
		dw_annotation.object.ttlinit_authfn[1] =ls_null
		dw_annotation.object.ttlinit_ahonorific[1] = ""
	END IF
	IF aepcd = 'N' THEN
		dw_annotation.object.ttlinit_authfn[1] = ""
		dw_annotation.object.ttlinit_auth[1] = ""
		dw_annotation.object.ttlinit_ahonorific[1] = ""
		IF dw_coauthors.RowCount() > 0 THEN
			rtn = MessageBox("Warnning","Author and Coauthors are not required for this Title Entry. Remove it?",Question!,YesNo!,1)
			IF rtn = 1 THEN
				FOR i=1 TO dw_coauthors.RowCount()
					dw_coauthors.DeleteRow(i)
				NEXT
				dw_coauthors.Event pfc_Update(True,True)
				Commit Using SqlServerTrans;
				dw_coauthors.Reset()
				dw_coauthors.Enabled = FALSE
			ELSE
				RETURN
			END IF
		ELSE
			dw_coauthors.Enabled = FALSE
		END IF
	END IF
ELSEIF dwo.name = "ttlinit_annoinit" THEN
	IF data <> UPPER(sqlservertrans.userid) THEN
		RETURN 1
	END IF
ELSEIF dwo.name = "ttlinit_pminit" THEN
	IF data <> UPPER(sqlservertrans.userid) THEN
		RETURN 1
	END IF
ELSEIF dwo.name = "ttlinit_cdinit" THEN
	IF data <> UPPER(sqlservertrans.userid) THEN
		RETURN 1
	END IF
END IF
end event

event itemfocuschanged;call super::itemfocuschanged;IF dwo.tag <> "" THEN
	SetMicroHelp(w_pics_main,dwo.tag)
ELSE
	SetMicroHelp(w_pics_main,"Ready")
END IF
if IsValid(inv_dropdownsearch) then
	inv_dropdownsearch.event pfc_itemfocuschanged(row, dwo)
end if
end event

event rbuttondown;//
end event

event rbuttonup;//
end event

event editchanged;call super::editchanged;inv_dropdownsearch.event pfc_editchanged(row, dwo, data)
end event

event constructor;call super::constructor;m_pics_main.m_edit.m_deleterow.Enabled 	=	FALSE
m_pics_main.m_edit.m_addrow.Enabled 		=	FALSE
dw_annotation.of_SetDropDownSearch(TRUE)
dw_annotation.of_SetTransObject( SQLServerTrans )

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
END IF
end event

type dw_medium from u_pics_dw within w_sheet_view_annotation
event ue_enterkey pbm_dwnprocessenter
event pfc_hinttext pbm_mousemove
integer x = 1161
integer y = 876
integer width = 1394
integer height = 300
integer taborder = 50
string dataobject = "d_medium_nori"
boolean vscrollbar = false
boolean livescroll = false
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

event itemchanged;call super::itemchanged;int cnt_ri,rtn
string lconno
IF dwo.Name = "ri_prevbkmed" THEN
	IF data<>"" THEN
		// If you choose a media for the previouse book number
		IF data<>'RC' AND data<>'FD' AND data<>'BR' AND data<>'BRA' AND &
			data<>'P/B' AND data<>'CB' AND data<>'RD' AND data<>'TB' THEN
			RETURN 1
		ELSE
//			dw_medium.Object.ri_prevbkseq.tabsequence='50'
//			dw_medium.Object.ri_prevbkseq.EditMask.Required='Yes'
			dw_medium.Object.mchar_ricd.tabsequence='60'
			dw_medium.Object.mchar_ricd.Edit.Required='Yes'
		END IF
	ELSE
		// If you want to delete the existing previouse book number
		// or there are no previouse book number existed.
		rtn = MessageBox("Warning","Do you want to remove the previous book number?",Question!,YesNo!,1)
		if rtn = 1 THEN		
//			dw_medium.Object.ri_prevbkseq.tabsequence='0'
			dw_medium.Object.mchar_ricd.tabsequence='0'
//			dw_medium.Object.ri_prevbkseq.EditMask.Required='No'
			dw_medium.Object.mchar_ricd.Edit.Required='No'
//			dw_medium.Object.ri_prevbkseq[row]=0
			dw_medium.object.mchar_ricd[row]=""
			lconno = dw_medium.Object.mchar_conno[row]
			select count(*) into :cnt_ri from ri where conno = :lconno using sqlservertrans;
			IF f_check_dberror(sqlservertrans,"RI") THEN
				IF cnt_ri > 0 THEN
					DELETE FROM ri WHERE conno=:lconno USING SqlServerTrans;
					IF f_check_dberror(sqlservertrans,"RI") THEN
						commit using sqlservertrans;
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
ELSEIF dwo.Name = "mchar_med" THEN
	String Lmed1,Lmed2,Lmed3,Lconno1,Lconno2,Lconno3
	dw_medium.AcceptText()
	CHOOSE CASE row
		CASE 2
			Lmed1 = dw_medium.object.mchar_med[1]
			Lmed2 = dw_medium.object.mchar_med[2]
			Lconno1 = dw_medium.object.mchar_conno[1]
			Lconno2 = dw_medium.object.mchar_conno[2]
			IF Lmed1=Lmed2 or Lconno1=Lconno2 THEN
				MessageBox("Error", "Media/Control numbers must be different between rows!",Information!)
				RETURN -1			
			ELSEIF ((Lmed1="RC" and Lmed2="FD") or (Lmed1="FD" and Lmed2="RC")) THEN
				IF dw_annotation.object.ttlinit_oneliner="" THEN
					MessageBox("Error", "One Line Annotation is required for FD/RC books.",Information!)
					RETURN -1
				END IF
			END IF				
		CASE 3
			Lmed1 = dw_medium.object.mchar_med[1]
			Lmed2 = dw_medium.object.mchar_med[2]
			Lmed3 = dw_medium.object.mchar_med[3]
			Lconno1 = dw_medium.object.mchar_conno[1]
			Lconno2 = dw_medium.object.mchar_conno[2]
			Lconno3 = dw_medium.object.mchar_conno[3]
			IF Lmed1=Lmed2 or Lmed1=Lmed3 or Lmed2=Lmed3 or Lconno1=Lconno2 or Lconno1=Lconno3 or Lconno2=Lconno3 THEN
				MessageBox("Error", "Media/Control numbers must be different between rows!",Information!)
				RETURN -1			
			ELSEIF ((Lmed1="RC" and Lmed2="FD") or (Lmed1="FD" and Lmed2="RC")) THEN
				IF dw_annotation.object.ttlinit_oneliner="" THEN
					MessageBox("Error", "One Line Annotation is required for FD/RC books.",Information!)
					RETURN -1			
				END IF
			END IF				
	END CHOOSE
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


if IsValid (inv_rowmanager) then
	li_rc = inv_rowmanager.event pfc_deleterow () 
else	
	li_rc = this.DeleteRow (0) 
end if

// Notify the Linkage Service 
IF IsValid ( inv_Linkage ) THEN 
	If li_rc > 0 Then inv_Linkage.Event pfc_DeleteRow (0) 
END IF 

dw_medium.SetItemStatus(1, 0, Primary!, DataModified!)

return li_rc

end event

event ue_postconstructor;call super::ue_postconstructor;dw_medium.of_SetTransObject(SQLServerTrans)

end event

type dw_coauthors from u_pics_dw within w_sheet_view_annotation
event ue_enterkey pbm_dwnprocessenter
integer x = 1134
integer y = 1180
integer width = 1051
integer height = 376
integer taborder = 70
string dataobject = "d_cc_coauth"
boolean hscrollbar = true
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

event ue_postconstructor;call super::ue_postconstructor;dw_coauthors.of_SetLinkage(TRUE)
dw_coauthors.of_SetTransObject( SQLServerTrans )

end event

type cb_print from commandbutton within w_sheet_view_annotation
integer x = 2066
integer y = 1592
integer width = 270
integer height = 100
boolean bringtotop = true
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "&Print"
end type

event clicked;long job
string Lconno

Lconno = em_conno.text
dw_cc_pms_par_report.retrieve(Lconno)
dw_cc_pms_par_report.TriggerEvent("pfc_print")

//OpenSheetwithparm(w_sheet_pics_ole_crystal,"editannotation",w_pics_main, 0, Original!)

end event

type st_wordcnt from statictext within w_sheet_view_annotation
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

type dw_cc_pms_par_report from u_pics_dw within w_sheet_view_annotation
boolean visible = false
integer x = 1934
integer y = 136
integer width = 489
integer taborder = 0
boolean enabled = false
string dataobject = "d_annotrpt_ec_report"
boolean vscrollbar = false
boolean border = false
boolean livescroll = false
borderstyle borderstyle = stylebox!
end type

event ue_postconstructor;call super::ue_postconstructor;dw_cc_pms_par_report.of_SetTransObject( SQLServerTrans )

end event

type dw_cc_anno_rpt from u_pics_dw within w_sheet_view_annotation
boolean visible = false
integer x = 50
integer y = 756
integer width = 78
integer height = 52
integer taborder = 0
boolean enabled = false
string dataobject = "d_cc_anno_rpt"
boolean vscrollbar = false
boolean border = false
boolean livescroll = false
borderstyle borderstyle = stylebox!
end type

event itemchanged;call super::itemchanged;dw_cc_anno_rpt.of_SetTransObject( SQLServerTrans )

end event


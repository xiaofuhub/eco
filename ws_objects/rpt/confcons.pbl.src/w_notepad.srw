$PBExportHeader$w_notepad.srw
forward
global type w_notepad from window
end type
type cb_1 from commandbutton within w_notepad
end type
type st_position4 from statictext within w_notepad
end type
type st_2 from statictext within w_notepad
end type
type st_position2 from statictext within w_notepad
end type
type st_3 from statictext within w_notepad
end type
type mle_notepad from u_mle within w_notepad
end type
type st_1 from statictext within w_notepad
end type
type st_position from statictext within w_notepad
end type
type dw_cc_anno_rpt from u_pics_dw within w_notepad
end type
type mle_sex from u_mle within w_notepad
end type
end forward

shared variables

end variables

global type w_notepad from window
integer x = 187
integer y = 128
integer width = 2560
integer height = 1828
boolean titlebar = true
string title = "Annotation Notepad"
string menuname = "m_notepad"
boolean controlmenu = true
boolean minbox = true
boolean resizable = true
windowtype windowtype = popup!
long backcolor = 79741120
toolbaralignment toolbaralignment = alignatleft!
event ue_savefile pbm_custom18
event ue_set_text_color pbm_custom29
event ue_edit_actions pbm_custom30
event ue_change_fonts pbm_custom31
event ue_closefile pbm_custom32
event ue_modified_text pbm_custom24
event ue_set_background_color pbm_custom25
event ue_spellcheck pbm_custom26
event ue_change_pitch pbm_custom27
event ue_savesetting pbm_custom28
event ue_printanno pbm_custom33
event ue_saveanno pbm_custom73
cb_1 cb_1
st_position4 st_position4
st_2 st_2
st_position2 st_position2
st_3 st_3
mle_notepad mle_notepad
st_1 st_1
st_position st_position
dw_cc_anno_rpt dw_cc_anno_rpt
mle_sex mle_sex
end type
global w_notepad w_notepad

type variables
boolean   ib_text_modified,ib_save_failed,is_sga_active
string   is_filename, is_fullname
//s_notepad_parms   istr_parms

end variables

forward prototypes
protected subroutine wf_set_line_index ()
public function string wf_rm_ext_space (string ls_str)
end prototypes

event ue_savefile;SetPointer (HourGlass!)
string Lchart_no,annotxt,anno_prop_txt
int rtn,ll_excess_count,ll_count

ll_count = Integer(st_position4.text)
IF (ll_count) > 50 THEN
	ll_excess_count = ll_count  - 50
	MessageBox("Annotation word count","You have exceeded  maximum number of fifty words by "+String(ll_excess_count)+" word(s).")
END IF


IF IsValid(w_sheet_annotation) THEN 
	Lchart_no=w_sheet_annotation.Lchno
ELSEIF IsValid(w_sheet_final_review) THEN
	Lchart_no=w_sheet_final_review.Lchno
END IF

annotxt = mle_notepad.text
anno_prop_txt=mle_sex.text

rtn = dw_cc_anno_rpt.retrieve(Lchart_no)
  
IF rtn=0 THEN
	INSERT INTO annotation(chno,anno,anno_property, prop_added_flg) 
	VALUES (:Lchart_no, :annotxt, :anno_prop_txt, NULL) 
		USING SQLServerTrans;
ELSE
	UPDATE annotation 
	SET anno = :annotxt, anno_property = :anno_prop_txt, prop_added_flg = NULL
	WHERE chno = :Lchart_no 
	USING SQLServerTrans;
END IF
IF f_check_dberror(sqlservertrans,"ANNOTATION")=FALSE THEN
	rollback using sqlservertrans;
ELSE
	COMMIT USING SQLServerTrans;
	MessageBox("SAVE","Annotation has been saved into database. You MUST update from original screen.",Information!)
	IF IsValid(w_sheet_annotation) THEN 
		w_sheet_annotation.anno_exist = TRUE
	ELSEIF IsValid(w_sheet_final_review) THEN
		w_sheet_final_review.anno_exist = TRUE
	END IF
END IF


IF IsValid(w_sheet_annotation) THEN 
	w_sheet_annotation.mle_anno.text = mle_notepad.text
	w_sheet_annotation.st_wordcnt.text = st_position4.text
ELSEIF IsValid(w_sheet_final_review) THEN
	w_sheet_final_review.mle_anno.text = mle_notepad.text
END IF

Disable (m_notepad.m_file.m_save)
ib_text_modified = false


end event

event ue_set_text_color;//////////////////////////////////////////////////////////////////////
// Set the text color
//////////////////////////////////////////////////////////////////////

OpenWithParm (w_notepad_colors, mle_notepad)

end event

event ue_edit_actions;//////////////////////////////////////////////////////////////////////
//
// edit text functions (cut, copy, paste, ...)
//
//////////////////////////////////////////////////////////////////////


// Depending on which edit menuitem was selected, take the appropriate
// action on the currently active multilineedit. The current mle is
// stored in a shared variable and set on open or activate of a sheet.

choose case String (message.longparm, "address")

	case "undo"
			// unsupported appeon feature canundo method removed  3/29/10
			Undo (mle_notepad)
			this.TriggerEvent ("ue_modified_text")
	case "cut"
		Cut (mle_notepad)
		this.TriggerEvent ("ue_modified_text")

	case "copy"
	   IF is_sga_active THEN
			Copy (mle_sex)
		ELSE
			Copy (mle_notepad)
		END IF

	case "paste"
		Paste (mle_notepad)
		this.TriggerEvent ("ue_modified_text")

	case "clear"
		Clear (mle_notepad)
		this.TriggerEvent ("ue_modified_text")
		
	case "italic"
		mle_notepad.Italic = TRUE		
		this.TriggerEvent ("ue_modified_text")
		
	case "unitalic"
		mle_notepad.Italic = FALSE
		this.TriggerEvent ("ue_modified_text")

	case "selectall"
	   IF is_sga_active THEN
			mle_sex.SelectText (1, Len (mle_sex.text))
		ELSE
			mle_notepad.SelectText (1, Len (mle_notepad.text))
		END IF
	case else
		MessageBox ("Edit", "There is no such edit action!")
end choose

end event

event ue_change_fonts;//////////////////////////////////////////////////////////////////////
// Change the current text font for the sheet
//////////////////////////////////////////////////////////////////////

mle_notepad.facename = String (Message.LongParm, "address")

end event

on ue_closefile;close (this)
end on

event ue_modified_text;ib_text_modified = true
Enable (m_notepad.m_file.m_save)
end event

event ue_set_background_color;//////////////////////////////////////////////////////////////////////
// Set the text color
//////////////////////////////////////////////////////////////////////

OpenWithParm (w_notepad_back_colors, mle_notepad)

end event

event ue_spellcheck;nca_word lnca_word
String ls_S

ls_S = mle_notepad.text

lnca_Word.SpellCheck( ls_S )

mle_notepad.text = ls_S

end event

event ue_change_pitch;//////////////////////////////////////////////////////////////////////
// Change the current text size for the sheet
//////////////////////////////////////////////////////////////////////

mle_notepad.TextSize = Integer(String (Message.LongParm, "address")) - &
								(2 * Integer(String (Message.LongParm, "address")))
end event

event ue_savesetting;int li_filenum

li_filenum = FileOpen("../anno._cf",LineMode!,Write!,LockWrite!,Replace!)

FileWrite(li_filenum,mle_notepad.facename+":"+string(mle_notepad.TextSize)+":"+ &
			string(mle_notepad.BackColor)+":"+string(mle_notepad.TextColor))
FileClose(li_filenum)

end event

event ue_printanno;long job
string Lchart_no

IF IsValid(w_sheet_annotation) THEN 
	Lchart_no=w_sheet_annotation.Lchno
ELSEIF IsValid(w_sheet_final_review) THEN
	Lchart_no=w_sheet_final_review.Lchno
END IF

dw_cc_anno_rpt.retrieve(Lchart_no)

job = PrintOpen( ) 
// Each DataWindow starts printing on a new page.
PrintDataWindow(job, dw_cc_anno_rpt) 
PrintClose(job)

end event

protected subroutine wf_set_line_index ();Integer			ll_count,ll_count_sex,ll_excess_count
string 			ls_space,ls_source,ls_sex
n_cst_string 	inv_string

ls_source = mle_notepad.text
ls_sex = mle_sex.text
ls_space = " "

IF Not(IsNull(ls_sex)) THEN
	ls_sex =  inv_string.of_GlobalReplace(ls_sex, "     ", " ")
	ls_sex =  inv_string.of_GlobalReplace(ls_sex, "    ", " ")
	ls_sex =  inv_string.of_GlobalReplace(ls_sex, "   ", " ")
	ls_sex =  inv_string.of_GlobalReplace(ls_sex, "  ", " ")
	ll_count_sex = inv_string.of_CountOccurrences(ls_sex, ls_space, TRUE)
	st_position2.text = String (ll_count_sex+1, '##0')
	mle_sex.text = ls_sex
//	ls_source = ls_source + " " + ls_sex
END IF

ls_source =  inv_string.of_GlobalReplace(ls_source, "       ", " ")
ls_source =  inv_string.of_GlobalReplace(ls_source, "      ", " ")
ls_source =  inv_string.of_GlobalReplace(ls_source, "     ", " ")
ls_source =  inv_string.of_GlobalReplace(ls_source, "    ", " ")
ls_source =  inv_string.of_GlobalReplace(ls_source, "   ", " ")
ls_source =  inv_string.of_GlobalReplace(ls_source, "  ", " ")

ll_count = inv_string.of_CountOccurrences(ls_source, ls_space, TRUE)
st_position.text = String (ll_count+1, '##0')
st_position4.text = string(integer(st_position.text) + integer(st_position2.text))

ib_text_modified=TRUE

end subroutine

public function string wf_rm_ext_space (string ls_str);n_cst_string  lnv_string

ls_str =  lnv_string.of_GlobalReplace(ls_str, "       ", " ")
ls_str =  lnv_string.of_GlobalReplace(ls_str, "      ", " ")
ls_str =  lnv_string.of_GlobalReplace(ls_str, "     ", " ")
ls_str =  lnv_string.of_GlobalReplace(ls_str, "    ", " ")
ls_str =  lnv_string.of_GlobalReplace(ls_str, "   ", " ")
ls_str =  lnv_string.of_GlobalReplace(ls_str, "  ", " ")

return ls_str
end function

event closequery;//////////////////////////////////////////////////////////////////////
//
// Ask user if changes need to be saved before closing
//
//////////////////////////////////////////////////////////////////////

int	li_ret

if ib_text_modified then
	li_ret = MessageBox ("Annotation NotePad", &
			"Do you want to save changes in annotation zoom window" + is_filename + "?", &
			question!, yesnocancel!)
	if li_ret = 1 then
		this.Triggerevent ("ue_savefile")
		if ib_save_failed then
			ib_save_failed = false
			Message.ReturnValue = 1
		end if
	else
		if li_ret = 3 then
			message.ReturnValue = 1
		end if
	end if
end if
end event

event open;n_cst_string	lnv_string

integer  li_FileNum,rtn,ll_count, ll_excess_count
string  ls_Setting_Input,ls_setting[ ],ls_sex,ls_sex_org,Lchart_no,lflg
string  ls_anno,ls_anno_without_sex,ls_source
string  ls_space
long    ll_FLength
Boolean anno_tags_altered=FALSE,first_time_annotation=FALSE

SetNull(lflg)

IF IsValid(w_sheet_annotation) THEN 
	Lchart_no=w_sheet_annotation.Lchno
	mle_notepad.text = w_sheet_annotation.mle_anno.text
	ls_sex = w_sheet_annotation.Lsexviol
	anno_tags_altered = w_sheet_annotation.anno_tags_altered
ELSEIF IsValid(w_sheet_final_review) THEN
	Lchart_no=w_sheet_final_review.Lchno
	mle_notepad.text = w_sheet_final_review.mle_anno.text
	ls_sex = w_sheet_final_review.Lsexviol
	anno_tags_altered =  w_sheet_final_review.anno_tags_altered
END IF
	
mle_sex.text = TRIM(ls_sex)

IF first_time_annotation THEN
	mle_notepad.text = wf_rm_ext_space(mle_notepad.text )
ELSE
	Select anno,anno_property,prop_added_flg
	into :ls_anno,:ls_sex_org,:lflg
	from annotation
	where chno = :Lchart_no
	using sqlservertrans;
	
	IF IsNull(ls_anno) OR ls_anno="" THEN
		ls_anno = mle_notepad.text
		ls_sex_org = ls_sex
		SetNULL(lflg )
	END IF
	
	mle_notepad.text = wf_rm_ext_space(mle_notepad.text )
		
END IF

IF (mle_sex.text = "" OR IsNull(mle_sex.text) ) THEN
	mle_sex.Visible = FALSE
ELSE
	mle_sex.Visible = TRUE
	mle_sex.enabled = TRUE
END IF

wf_set_line_index()

li_FileNum = FileOpen("../anno._cf",StreamMode!)
IF li_FileNum <> -1 THEN
	ll_FLength = FileLength("../anno._cf")
	IF ll_FLength < 32767 THEN 
		FileRead(li_FileNum, ls_Setting_Input)
		FileClose(li_FileNum)
	END IF
	
	// Separate the setting into multiple setting separated by ":"
	//MessageBox("setting",ls_Setting_Input)
	
	rtn = lnv_string.of_Parsetoarray(ls_Setting_Input, ":", ls_setting)
	
	mle_notepad.facename=ls_setting[1]
	mle_notepad.TextSize=integer(ls_setting[2])
	mle_notepad.BackColor=long(ls_setting[3])
	mle_notepad.TextColor=long(ls_setting[4])
	
	mle_sex.facename=ls_setting[1]
	mle_sex.TextSize=integer(ls_setting[2])
	mle_sex.BackColor=long(ls_setting[3])
	mle_sex.TextColor=long(ls_setting[4])
	
	//MessageBox("setting",ls_Setting_Input+","+fonttype+","+string(fontsize)+","+string(bckcolor)+","+string(txtcolor))
END IF


end event

event resize;//////////////////////////////////////////////////////////////////////
//
// Size the MLE to fill the sheet, leaving space for the clock and
// line index status indicator.
//
//////////////////////////////////////////////////////////////////////


mle_notepad.Resize (WorkSpaceWidth (this),	WorkSpaceHeight (this) - (st_position.height*1.1) - (st_position2.height*1.1) - (st_position4.height*1.1) -  (mle_sex.height*1.1))
			
mle_sex.Resize (WorkSpaceWidth (this), WorkSpaceHeight (this) - (st_position.height*1.1) - (st_position2.height*1.1) - (st_position4.height*1.1) -  (mle_notepad.height*1.1))
			
// Position the sex and violance text appropriately
mle_sex.Move (mle_notepad.X + mle_notepad.width - (mle_sex.width), 	mle_notepad.Y + mle_notepad.height)

// Position the core annotation word count indicator appropriately
st_position.Move (mle_notepad.X + mle_notepad.width - (st_position.width*1.1),  mle_notepad.Y + (mle_notepad.height) + (mle_sex.height))

// Position the annotation tag word count indicator appropriately
st_position2.Move (mle_notepad.X + mle_notepad.width - (st_position2.width*1.1),  mle_notepad.Y + (mle_notepad.height) + (mle_sex.height) + (st_position.height))

// Position the total annotation tag word count indicator appropriately
st_position4.Move (mle_notepad.X + mle_notepad.width - (st_position4.width*1.1),  mle_notepad.Y + (mle_notepad.height) + (mle_sex.height) + (st_position.height) + (st_position4.height))

// Position the button appropriately
cb_1.Move (cb_1.width ,  mle_notepad.Y + (mle_notepad.height) + (mle_sex.height))

// Position the core annotation word count indicator text appropriately
st_1.Move (mle_notepad.X + mle_notepad.width - ((st_1.width*1.1) + (st_position.width*1.1)),  mle_notepad.Y + (mle_notepad.height) + (mle_sex.height))

// Position the annotation tag word count indicator text appropriately
st_3.Move (mle_notepad.X + mle_notepad.width - ((st_3.width*1.1) + (st_position2.width*1.1)),  mle_notepad.Y + (mle_notepad.height) + (mle_sex.height) + (st_position.height))

// Position the total annotation tag word count indicator text appropriately
st_2.Move (mle_notepad.X + mle_notepad.width - ((st_2.width*1.1) + (st_position4.width*1.1)),  mle_notepad.Y + (mle_notepad.height) + (mle_sex.height) + (st_position.height) +  (st_position4.height))

// Position the st_2 indicator appropriately
//st_2.Move (mle_notepad.x , mle_notepad.height + mle_sex.height*1.1)

// Position the cbx_yes indicator appropriately
//cbx_yesno.Move (mle_notepad.x + st_2.width*1.1 , mle_notepad.height + mle_sex.height*1.1)


end event

event activate;/////////////////////////////////////////////////////////////////////
//
// Set globals and menu items appropriately, in the event that there
// are multiple sheets.
//
/////////////////////////////////////////////////////////////////////

// Enable the 'save' menuitem appropriately depending on whether text
// has been modified for this sheet.
if ib_text_modified then
	Enable (m_notepad.m_file.m_save)
else
	Disable (m_notepad.m_file.m_save)
end if


end event

on w_notepad.create
if this.MenuName = "m_notepad" then this.MenuID = create m_notepad
this.cb_1=create cb_1
this.st_position4=create st_position4
this.st_2=create st_2
this.st_position2=create st_position2
this.st_3=create st_3
this.mle_notepad=create mle_notepad
this.st_1=create st_1
this.st_position=create st_position
this.dw_cc_anno_rpt=create dw_cc_anno_rpt
this.mle_sex=create mle_sex
this.Control[]={this.cb_1,&
this.st_position4,&
this.st_2,&
this.st_position2,&
this.st_3,&
this.mle_notepad,&
this.st_1,&
this.st_position,&
this.dw_cc_anno_rpt,&
this.mle_sex}
end on

on w_notepad.destroy
if IsValid(MenuID) then destroy(MenuID)
destroy(this.cb_1)
destroy(this.st_position4)
destroy(this.st_2)
destroy(this.st_position2)
destroy(this.st_3)
destroy(this.mle_notepad)
destroy(this.st_1)
destroy(this.st_position)
destroy(this.dw_cc_anno_rpt)
destroy(this.mle_sex)
end on

type cb_1 from commandbutton within w_notepad
integer x = 37
integer y = 1344
integer width = 608
integer height = 112
integer taborder = 30
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "View total annotation"
end type

event clicked;open(w_tot_anno)
end event

type st_position4 from statictext within w_notepad
integer x = 2267
integer y = 1476
integer width = 219
integer height = 64
integer textsize = -9
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 41943040
boolean enabled = false
string text = "0001"
alignment alignment = center!
boolean border = true
borderstyle borderstyle = stylelowered!
boolean focusrectangle = false
end type

type st_2 from statictext within w_notepad
integer x = 1554
integer y = 1476
integer width = 695
integer height = 64
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long backcolor = 67108864
boolean enabled = false
string text = "Total Annotation Word Count:"
alignment alignment = right!
boolean focusrectangle = false
end type

type st_position2 from statictext within w_notepad
integer x = 2267
integer y = 1408
integer width = 219
integer height = 64
integer textsize = -9
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 41943040
boolean enabled = false
string text = "0001"
alignment alignment = center!
boolean border = true
borderstyle borderstyle = stylelowered!
boolean focusrectangle = false
end type

type st_3 from statictext within w_notepad
integer x = 1559
integer y = 1408
integer width = 695
integer height = 64
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long backcolor = 67108864
boolean enabled = false
string text = "Annotation Tags Word Count:"
alignment alignment = right!
boolean focusrectangle = false
end type

type mle_notepad from u_mle within w_notepad
event ue_char_entered pbm_char
event ue_key_up pbm_keyup
event ue_lmouse_up pbm_lbuttonup
integer x = 23
integer y = 24
integer width = 2473
integer height = 904
integer taborder = 10
integer textsize = -12
string pointer = "Arrow!"
boolean vscrollbar = true
end type

event ue_char_entered;call super::ue_char_entered;//////////////////////////////////////////////////////////////////////
// Set the modified flag to true and enable the file-save menu item
// when a character is entered into the document.
//////////////////////////////////////////////////////////////////////

if ib_text_modified = false then
	Enable (m_notepad.m_file.m_save)
	ib_text_modified = true
end if

end event

event ue_key_up;call super::ue_key_up;wf_set_line_index ()
end event

event ue_lmouse_up;call super::ue_lmouse_up;wf_set_line_index ()
end event

event constructor;call super::constructor;wf_set_line_index ()
end event

event rbuttondown;call super::rbuttondown;////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Have the edit menu popup when the right mouse button is clicked.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

m_notepad.m_edit.PopMenu (parent.PointerX(), parent.PointerY())
end event

event getfocus;call super::getfocus;is_sga_active=FALSE
end event

type st_1 from statictext within w_notepad
integer x = 1559
integer y = 1344
integer width = 695
integer height = 64
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long backcolor = 67108864
boolean enabled = false
string text = "Core Annotation Word Count:"
alignment alignment = right!
boolean focusrectangle = false
end type

type st_position from statictext within w_notepad
integer x = 2267
integer y = 1340
integer width = 219
integer height = 64
integer textsize = -9
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 41943040
boolean enabled = false
string text = "0001"
alignment alignment = center!
boolean border = true
borderstyle borderstyle = stylelowered!
boolean focusrectangle = false
end type

type dw_cc_anno_rpt from u_pics_dw within w_notepad
boolean visible = false
integer x = 1426
integer y = 1344
integer width = 146
integer height = 96
integer taborder = 20
boolean enabled = false
string dataobject = "d_cc_anno_rpt"
boolean vscrollbar = false
boolean border = false
boolean livescroll = false
borderstyle borderstyle = stylebox!
boolean ib_isupdateable = false
boolean ib_rmbmenu = false
boolean ib_rmbfocuschange = false
string is_updatesallowed = ""
end type

event ue_postconstructor;call super::ue_postconstructor;dw_cc_anno_rpt.of_SetTransObject( SQLServerTrans )

end event

event sqlpreview;call super::sqlpreview;//messagebox("sql",sqlsyntax)
end event

type mle_sex from u_mle within w_notepad
integer x = 18
integer y = 928
integer width = 2482
integer height = 412
integer taborder = 21
boolean vscrollbar = true
boolean displayonly = true
end type

event getfocus;call super::getfocus;is_sga_active=TRUE
end event


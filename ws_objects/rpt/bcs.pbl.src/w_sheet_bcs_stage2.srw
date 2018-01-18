$PBExportHeader$w_sheet_bcs_stage2.srw
forward
global type w_sheet_bcs_stage2 from w_sheet
end type
type cb_summary from commandbutton within w_sheet_bcs_stage2
end type
type st_1 from statictext within w_sheet_bcs_stage2
end type
type st_displayed from statictext within w_sheet_bcs_stage2
end type
type st_2 from statictext within w_sheet_bcs_stage2
end type
type st_modified from statictext within w_sheet_bcs_stage2
end type
type pb_first from picturebutton within w_sheet_bcs_stage2
end type
type pb_prior from picturebutton within w_sheet_bcs_stage2
end type
type pb_next from picturebutton within w_sheet_bcs_stage2
end type
type pb_last from picturebutton within w_sheet_bcs_stage2
end type
type sle_date1 from singlelineedit within w_sheet_bcs_stage2
end type
type sle_date2 from singlelineedit within w_sheet_bcs_stage2
end type
type cb_pub from commandbutton within w_sheet_bcs_stage2
end type
type cb_anno from commandbutton within w_sheet_bcs_stage2
end type
type cb_editinfo from commandbutton within w_sheet_bcs_stage2
end type
type cb_export from commandbutton within w_sheet_bcs_stage2
end type
type cb_sum from commandbutton within w_sheet_bcs_stage2
end type
type pb_find from commandbutton within w_sheet_bcs_stage2
end type
type pb_print from commandbutton within w_sheet_bcs_stage2
end type
type pb_update from commandbutton within w_sheet_bcs_stage2
end type
type pb_exit from commandbutton within w_sheet_bcs_stage2
end type
type cb_range from commandbutton within w_sheet_bcs_stage2
end type
type cb_reset_dateout from commandbutton within w_sheet_bcs_stage2
end type
type st_from from statictext within w_sheet_bcs_stage2
end type
type st_to from statictext within w_sheet_bcs_stage2
end type
type dw_bcs_stage2 from u_pics_dw within w_sheet_bcs_stage2
end type
type dw_summary from u_pics_dw within w_sheet_bcs_stage2
end type
end forward

global type w_sheet_bcs_stage2 from w_sheet
integer x = 5
integer y = 108
integer width = 2894
integer height = 1668
string title = "Cataloging Stage II"
cb_summary cb_summary
st_1 st_1
st_displayed st_displayed
st_2 st_2
st_modified st_modified
pb_first pb_first
pb_prior pb_prior
pb_next pb_next
pb_last pb_last
sle_date1 sle_date1
sle_date2 sle_date2
cb_pub cb_pub
cb_anno cb_anno
cb_editinfo cb_editinfo
cb_export cb_export
cb_sum cb_sum
pb_find pb_find
pb_print pb_print
pb_update pb_update
pb_exit pb_exit
cb_range cb_range
cb_reset_dateout cb_reset_dateout
st_from st_from
st_to st_to
dw_bcs_stage2 dw_bcs_stage2
dw_summary dw_summary
end type
global w_sheet_bcs_stage2 w_sheet_bcs_stage2

type variables
string mod_string,original_select,very_original_select,pass_conno
date ldstg2dt
integer s2print=1
end variables

forward prototypes
public subroutine wf_enable_buttons ()
public subroutine wf_modify_title ()
public subroutine wf_set_counts ()
public subroutine wf_scroll (string as_scroll)
public subroutine wf_get_new_range ()
public subroutine wf_disable_buttons ()
end prototypes

public subroutine wf_enable_buttons ();pb_exit.enabled = TRUE
pb_update.enabled = TRUE
pb_print.enabled = TRUE
pb_find.enabled = TRUE

pb_first.enabled = TRUE
pb_last.enabled = TRUE
pb_next.enabled = TRUE
pb_prior.enabled = TRUE

cb_pub.enabled = TRUE
cb_anno.enabled = TRUE
cb_editinfo.enabled = TRUE
cb_export.enabled = TRUE
cb_sum.enabled = TRUE
cb_summary.enabled = TRUE
cb_range.enabled = TRUE
cb_reset_dateout.enabled = TRUE

end subroutine

public subroutine wf_modify_title ();	String Lttl
	
	Lttl  = dw_bcs_stage2.GetItemString(dw_bcs_stage2.GetRow(),"cttl")
	
	IF Lttl <> "" THEN
		
		String Lchno,subttl,sub1,temp="xyz"
		int lcount,i
		
		Lchno  = dw_bcs_stage2.GetItemString(dw_bcs_stage2.GetRow(),"cchno")
		
		// Get the subtitles and concatenate it into one string.
		DECLARE proc_ret_sttl PROCEDURE FOR
					sret_sttl(:Lchno)
		USING sqlservertrans; 
			
		EXECUTE proc_ret_sttl;

		DO WHILE (temp <> sub1)
			temp = sub1
			FETCH proc_ret_sttl INTO :sub1;
			IF temp <> sub1 THEN
				sub1=RightTrim(sub1)
				subttl=subttl+" "+sub1
			END IF
		LOOP 
		
		CLOSE proc_ret_sttl;
		
		Lttl = RightTrim(Lttl) + " " + RightTrim(subttl)
		
		dw_bcs_stage2.SetItem(dw_bcs_stage2.GetRow(), "cttl", Lttl)
		dw_bcs_stage2.SetItemStatus(dw_bcs_stage2.GetRow(),"cttl",Primary!, DataModified!)

	END IF

end subroutine

public subroutine wf_set_counts ();st_modified.text=String(dw_bcs_stage2.ModifiedCount())
st_displayed.text=String(dw_bcs_stage2.RowCount())
end subroutine

public subroutine wf_scroll (string as_scroll);long ll_to_row, ll_getrow, ll_rowcount

CHOOSE CASE as_scroll
	CASE "FIRST"
		ll_to_row = 0
	CASE "NEXT"
		ll_to_row = dw_bcs_stage2.getrow() + 1
	CASE "PRIOR"
		ll_to_row = dw_bcs_stage2.getrow() - 1
	CASE "LAST"
		ll_to_row = dw_bcs_stage2.rowcount()
END CHOOSE

dw_bcs_stage2.scrolltorow(ll_to_row)
dw_bcs_stage2.setrow(ll_to_row)
dw_bcs_stage2.setfocus()

ll_rowcount = dw_bcs_stage2.rowcount()
ll_getrow = dw_bcs_stage2.getrow()

pb_first.enabled = (ll_getrow > 1)
pb_next.enabled = (ll_getrow < ll_rowcount)
pb_prior.enabled = (ll_getrow > 1)
pb_last.enabled = (ll_getrow < ll_rowcount)

end subroutine

public subroutine wf_get_new_range ();string where_clause,rc,get_select,Ls2infrom,Ls2into
int ll_rows,RowNum

IF IsValid(w_stage2_datein) THEN
	Ls2infrom = w_stage2_datein.S2dateinfrom
	Ls2into   = w_stage2_datein.S2dateinto
	
	IF IsNull(Ls2into)=FALSE AND IsNull(Ls2infrom)=FALSE THEN
		IF Ls2infrom > Ls2into THEN
			MessageBox("Error","Date range is wrong!")
			close(w_sheet_bcs_stage2)
			RETURN
		END IF
	END IF
ELSEIF IsValid(w_s2datein) THEN
	Ls2infrom = w_s2datein.S2datein
END IF

sle_date1.text = Ls2infrom
sle_date2.text = Ls2into

SetMicroHelp(w_pics_main,"Please wait...")

get_select =	original_select

IF IsValid(w_stage2_datein) THEN
	IF (IsNull(Ls2infrom)=FALSE AND IsNull(Ls2into)=FALSE) THEN
		where_clause = " AND TO_CHAR(catalog.s2in,"+"~'"+"MM/DD/YYYY"+"~') BETWEEN "+ "~'" + LS2infrom + "~'" + " AND "+ "~'" + LS2into + "~'"
	ELSEIF (IsNull(Ls2infrom)=TRUE AND IsNull(Ls2into)=FALSE) THEN
		where_clause = " AND TO_CHAR(catalog.s2in,"+"~'"+"MM/DD/YYYY"+"~') <="+ "~'" + LS2into + "~'"
	ELSEIF (IsNull(Ls2infrom)=FALSE AND IsNull(Ls2into)=TRUE) THEN
		where_clause = " AND TO_CHAR(catalog.s2in,"+"~'"+"MM/DD/YYYY"+"~') ="+ "~'" + LS2infrom + "~'"
	END IF
ELSEIF IsValid(w_s2datein) THEN
	where_clause = " AND TO_CHAR(catalog.s2in,"+"~'"+"MM/DD/YYYY"+"~') ="+ "~'" + LS2infrom + "~'"
END IF

mod_string = "DataWindow.Table.Select=~"" + get_select + where_clause + "~""
rc = dw_bcs_stage2.Modify(mod_string)
IF rc = "" THEN
	ll_rows = dw_bcs_stage2.Retrieve()
   IF ll_rows < 1 THEN 
   	MessageBox("ERROR", "Stage II date in: " + string(LS2infrom) +" dose not exist.",StopSign!, OK!, 2)
		SetMicroHelp(w_pics_main,"")
	  
	  	if IsValid(w_s2datein) then
 			close(w_s2datein)
	  	end if
     	close(w_sheet_bcs_stage2)
	ELSE
		IF dw_bcs_stage2.Sharedata(dw_summary) = -1 THEN
   		MessageBox("ERROR", "Error sharing data between datawindows.",StopSign!, OK!, 2)
		END IF
		dw_bcs_stage2.SetSort("cttl")
		dw_bcs_stage2.Sort()
     	wf_set_counts()
		wf_modify_title()
		
     	dw_bcs_stage2.SetFocus()
	 	SetMicroHelp(w_pics_main,"")
		  
	  	if IsValid(w_stage2_datein) then
 		 	close(w_stage2_datein)
	  	elseif IsValid(w_s2datein) then
 		 	close(w_s2datein)
	  	end if
		  
		wf_enable_buttons()
	END IF
ELSE
	MessageBox("Status", "Modify select statement Failed." + rc + mod_string)
END IF

end subroutine

public subroutine wf_disable_buttons ();pb_exit.enabled = FALSE
pb_update.enabled = FALSE
pb_print.enabled = FALSE

pb_first.enabled = FALSE
pb_last.enabled = FALSE
pb_next.enabled = FALSE
pb_prior.enabled = FALSE

cb_pub.enabled = FALSE
cb_anno.enabled = FALSE
cb_editinfo.enabled = FALSE
cb_export.enabled = FALSE
cb_sum.enabled = FALSE
cb_summary.enabled = FALSE
cb_range.enabled = FALSE
end subroutine

on w_sheet_bcs_stage2.create
int iCurrent
call super::create
this.cb_summary=create cb_summary
this.st_1=create st_1
this.st_displayed=create st_displayed
this.st_2=create st_2
this.st_modified=create st_modified
this.pb_first=create pb_first
this.pb_prior=create pb_prior
this.pb_next=create pb_next
this.pb_last=create pb_last
this.sle_date1=create sle_date1
this.sle_date2=create sle_date2
this.cb_pub=create cb_pub
this.cb_anno=create cb_anno
this.cb_editinfo=create cb_editinfo
this.cb_export=create cb_export
this.cb_sum=create cb_sum
this.pb_find=create pb_find
this.pb_print=create pb_print
this.pb_update=create pb_update
this.pb_exit=create pb_exit
this.cb_range=create cb_range
this.cb_reset_dateout=create cb_reset_dateout
this.st_from=create st_from
this.st_to=create st_to
this.dw_bcs_stage2=create dw_bcs_stage2
this.dw_summary=create dw_summary
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.cb_summary
this.Control[iCurrent+2]=this.st_1
this.Control[iCurrent+3]=this.st_displayed
this.Control[iCurrent+4]=this.st_2
this.Control[iCurrent+5]=this.st_modified
this.Control[iCurrent+6]=this.pb_first
this.Control[iCurrent+7]=this.pb_prior
this.Control[iCurrent+8]=this.pb_next
this.Control[iCurrent+9]=this.pb_last
this.Control[iCurrent+10]=this.sle_date1
this.Control[iCurrent+11]=this.sle_date2
this.Control[iCurrent+12]=this.cb_pub
this.Control[iCurrent+13]=this.cb_anno
this.Control[iCurrent+14]=this.cb_editinfo
this.Control[iCurrent+15]=this.cb_export
this.Control[iCurrent+16]=this.cb_sum
this.Control[iCurrent+17]=this.pb_find
this.Control[iCurrent+18]=this.pb_print
this.Control[iCurrent+19]=this.pb_update
this.Control[iCurrent+20]=this.pb_exit
this.Control[iCurrent+21]=this.cb_range
this.Control[iCurrent+22]=this.cb_reset_dateout
this.Control[iCurrent+23]=this.st_from
this.Control[iCurrent+24]=this.st_to
this.Control[iCurrent+25]=this.dw_bcs_stage2
this.Control[iCurrent+26]=this.dw_summary
end on

on w_sheet_bcs_stage2.destroy
call super::destroy
destroy(this.cb_summary)
destroy(this.st_1)
destroy(this.st_displayed)
destroy(this.st_2)
destroy(this.st_modified)
destroy(this.pb_first)
destroy(this.pb_prior)
destroy(this.pb_next)
destroy(this.pb_last)
destroy(this.sle_date1)
destroy(this.sle_date2)
destroy(this.cb_pub)
destroy(this.cb_anno)
destroy(this.cb_editinfo)
destroy(this.cb_export)
destroy(this.cb_sum)
destroy(this.pb_find)
destroy(this.pb_print)
destroy(this.pb_update)
destroy(this.pb_exit)
destroy(this.cb_range)
destroy(this.cb_reset_dateout)
destroy(this.st_from)
destroy(this.st_to)
destroy(this.dw_bcs_stage2)
destroy(this.dw_summary)
end on

event pfc_preopen;this.of_SetPreference(TRUE)
this.inv_preference.of_SetToolBars(TRUE)
this.inv_preference.of_SetWindow(TRUE)
this.of_SetResize(TRUE)
this.inv_resize.of_SetOrigSize(this.workspacewidth(),this.workspaceheight())
inv_resize.of_Register(dw_bcs_stage2, "Scale")
inv_resize.of_Register(dw_summary, "Scale")
inv_resize.of_Register(pb_exit, "Scale")
inv_resize.of_Register(pb_find, "Scale")
inv_resize.of_Register(pb_update, "Scale")
inv_resize.of_Register(pb_print, "Scale")
inv_resize.of_Register(cb_summary, "Scale")
inv_resize.of_Register(cb_range, "Scale")
inv_resize.of_Register(cb_anno, "Scale")
inv_resize.of_Register(cb_editinfo, "Scale")
inv_resize.of_Register(cb_export, "Scale")
inv_resize.of_Register(cb_pub, "Scale")
inv_resize.of_Register(cb_sum, "Scale")
inv_resize.of_Register(cb_reset_dateout, "Scale")
inv_resize.of_Register(st_1, "Scale")
inv_resize.of_Register(st_2, "Scale")
inv_resize.of_Register(st_from, "Scale")
inv_resize.of_Register(st_to, "Scale")
inv_resize.of_Register(sle_date1, "Scale")
inv_resize.of_Register(sle_date2, "Scale")
inv_resize.of_Register(st_displayed, "Scale")
inv_resize.of_Register(st_modified, "Scale")

inv_resize.of_Register(pb_first, "Scale")
inv_resize.of_Register(pb_prior, "Scale")
inv_resize.of_Register(pb_last, "Scale")
inv_resize.of_Register(pb_next, "Scale")

end event

event resize;call super::resize;long ll_height

This.X = w_pics_main.X
This.Y = w_pics_main.Y
ll_height = w_pics_main.mdi_1.Height
This.Resize(w_pics_main.WorkSpaceWidth(), ll_height)

end event

event open;call super::open;m_pics_main.m_file.m_print.Enabled 			=	TRUE
m_pics_main.m_file.m_pagesetup.Enabled		=	TRUE
m_pics_main.m_file.m_printimmediate.Enabled	=	TRUE

end event

event pfc_print;call super::pfc_print;//pb_print.TriggerEvent(Clicked!)
dw_summary.Print(true,false)
return 1
end event

type cb_summary from commandbutton within w_sheet_bcs_stage2
event clicked pbm_bnclicked
integer x = 55
integer y = 1300
integer width = 293
integer height = 92
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
boolean enabled = false
string text = "Su&mmary..."
end type

event clicked;IF (dw_summary.visible = TRUE) THEN
	dw_summary.visible = FALSE
	cb_summary.text ='Summary...'
	s2print=1
ELSE
	dw_summary.visible = TRUE
	cb_summary.text ='Stage II...'
	s2print=2
END IF
end event

type st_1 from statictext within w_sheet_bcs_stage2
integer x = 50
integer y = 1160
integer width = 375
integer height = 72
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long backcolor = 79741120
boolean enabled = false
string text = "Rows Selected:"
alignment alignment = center!
boolean focusrectangle = false
end type

type st_displayed from statictext within w_sheet_bcs_stage2
integer x = 443
integer y = 1160
integer width = 133
integer height = 84
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long backcolor = 16777215
boolean enabled = false
alignment alignment = center!
boolean border = true
borderstyle borderstyle = stylelowered!
boolean focusrectangle = false
end type

type st_2 from statictext within w_sheet_bcs_stage2
integer x = 617
integer y = 1160
integer width = 361
integer height = 72
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long backcolor = 79741120
boolean enabled = false
string text = "Rows Modified:"
boolean focusrectangle = false
end type

type st_modified from statictext within w_sheet_bcs_stage2
integer x = 983
integer y = 1160
integer width = 128
integer height = 84
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long backcolor = 16777215
boolean enabled = false
boolean border = true
borderstyle borderstyle = stylelowered!
boolean focusrectangle = false
end type

type pb_first from picturebutton within w_sheet_bcs_stage2
event clicked pbm_bnclicked
event mousemove pbm_mousemove
string tag = "First Record"
integer x = 1257
integer y = 1164
integer width = 101
integer height = 84
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
boolean originalsize = true
string picturename = "FIRST1.BMP"
alignment htextalign = left!
end type

event clicked;dwItemStatus l_status
string Lconno,Linit

dw_bcs_stage2.AcceptText()

l_status = dw_bcs_stage2.GetItemStatus(dw_bcs_stage2.GetRow(),"cs2out", Primary!)

IF l_status = DataModified! THEN
	int rtn
	rtn = MessageBox("Update","You did not update. Update and Continue?",Question!,YesNoCancel!,1)
	IF rtn = 3 THEN
		RETURN
	ELSEIF rtn = 2 THEN
		wf_scroll("FIRST")
		
		l_status = dw_bcs_stage2.GetItemStatus(dw_bcs_stage2.GetRow(),"cttl", Primary!)
		IF l_status = NotModified! THEN
			wf_modify_title()
		END IF
	ELSEIF rtn = 1 THEN
		pb_update.TriggerEvent(Clicked!)
		dw_bcs_stage2.SetItemStatus(dw_bcs_stage2.GetRow(),0 ,Primary!, NotModified!)
		wf_scroll("FIRST")
		
		l_status = dw_bcs_stage2.GetItemStatus(dw_bcs_stage2.GetRow(),"cttl", Primary!)
		IF l_status = NotModified! THEN
			wf_modify_title()
		END IF
	END IF
ELSE
	wf_scroll("FIRST")
	
	l_status = dw_bcs_stage2.GetItemStatus(dw_bcs_stage2.GetRow(),"cttl", Primary!)
	IF l_status = NotModified! THEN
		wf_modify_title()
	END IF
END IF
end event

event mousemove;SetMicroHelp(w_pics_main,this.tag)

end event

type pb_prior from picturebutton within w_sheet_bcs_stage2
event clicked pbm_bnclicked
event mousemove pbm_mousemove
string tag = "Previous Record"
integer x = 1381
integer y = 1164
integer width = 101
integer height = 84
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
boolean originalsize = true
string picturename = "PRIOR1.BMP"
alignment htextalign = left!
end type

event clicked;dwItemStatus l_status
string Lconno,Linit

dw_bcs_stage2.AcceptText()

l_status = dw_bcs_stage2.GetItemStatus(dw_bcs_stage2.GetRow(),"cs2out", Primary!)

IF l_status = DataModified! THEN
	int rtn
	rtn = MessageBox("Update","You did not update. Update and Continue?",Question!,YesNoCancel!,1)
	IF rtn = 3 THEN
		RETURN
	ELSEIF rtn = 2 THEN
		wf_scroll("PRIOR")
		
		l_status = dw_bcs_stage2.GetItemStatus(dw_bcs_stage2.GetRow(),"cttl", Primary!)
		IF l_status = NotModified! THEN
			wf_modify_title()
		END IF
	ELSEIF rtn = 1 THEN
		pb_update.TriggerEvent(Clicked!)
		dw_bcs_stage2.SetItemStatus(dw_bcs_stage2.GetRow(),0 ,Primary!, NotModified!)
		wf_scroll("PRIOR")
		
		l_status = dw_bcs_stage2.GetItemStatus(dw_bcs_stage2.GetRow(),"cttl", Primary!)
		IF l_status = NotModified! THEN
			wf_modify_title()
		END IF
	END IF
ELSE
	wf_scroll("PRIOR")
	
	l_status = dw_bcs_stage2.GetItemStatus(dw_bcs_stage2.GetRow(),"cttl", Primary!)
	IF l_status = NotModified! THEN
		wf_modify_title()
	END IF
END IF
end event

event mousemove;SetMicroHelp(w_pics_main,this.tag)

end event

type pb_next from picturebutton within w_sheet_bcs_stage2
event clicked pbm_bnclicked
event mousemove pbm_mousemove
string tag = "Next Record"
integer x = 1504
integer y = 1164
integer width = 101
integer height = 84
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
boolean originalsize = true
string picturename = "NEXT1.BMP"
alignment htextalign = left!
end type

event clicked;dwItemStatus l_status
string Lconno,Linit

dw_bcs_stage2.AcceptText()

l_status = dw_bcs_stage2.GetItemStatus(dw_bcs_stage2.GetRow(),"cs2out", Primary!)

IF l_status = DataModified! THEN
	int rtn
	rtn = MessageBox("Update","You did not update. Update and Continue?",Question!,YesNoCancel!,1)
	IF rtn = 3 THEN
		RETURN
	ELSEIF rtn = 2 THEN
		wf_scroll("NEXT")
		
		l_status = dw_bcs_stage2.GetItemStatus(dw_bcs_stage2.GetRow(),"cttl", Primary!)
		IF l_status = NotModified! THEN
			wf_modify_title()
		END IF
	ELSEIF rtn = 1 THEN
		pb_update.TriggerEvent(Clicked!)
		dw_bcs_stage2.SetItemStatus(dw_bcs_stage2.GetRow(),0 ,Primary!, NotModified!)
		wf_scroll("NEXT")
		
		l_status = dw_bcs_stage2.GetItemStatus(dw_bcs_stage2.GetRow(),"cttl", Primary!)
		IF l_status = NotModified! THEN
			wf_modify_title()
		END IF
	END IF
ELSE
	wf_scroll("NEXT")
	
	l_status = dw_bcs_stage2.GetItemStatus(dw_bcs_stage2.GetRow(),"cttl", Primary!)
	IF l_status = NotModified! THEN
		wf_modify_title()
	END IF
END IF
end event

event mousemove;SetMicroHelp(w_pics_main,this.tag)

end event

type pb_last from picturebutton within w_sheet_bcs_stage2
event clicked pbm_bnclicked
event mousemove pbm_mousemove
string tag = "Last Record"
integer x = 1627
integer y = 1164
integer width = 101
integer height = 84
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
boolean originalsize = true
string picturename = "LAST1.BMP"
alignment htextalign = left!
end type

event clicked;dwItemStatus l_status
string Lconno,Linit

dw_bcs_stage2.AcceptText()

l_status = dw_bcs_stage2.GetItemStatus(dw_bcs_stage2.GetRow(),"cs2out", Primary!)

IF l_status = DataModified! THEN
	int rtn
	rtn = MessageBox("Update","You did not update. Update and Continue?",Question!,YesNoCancel!,1)
	IF rtn = 3 THEN
		RETURN
	ELSEIF rtn = 2 THEN
		wf_scroll("LAST")
		
		l_status = dw_bcs_stage2.GetItemStatus(dw_bcs_stage2.GetRow(),"cttl", Primary!)
		IF l_status = NotModified! THEN
			wf_modify_title()
		END IF
	ELSEIF rtn = 1 THEN
		pb_update.TriggerEvent(Clicked!)
		dw_bcs_stage2.SetItemStatus(dw_bcs_stage2.GetRow(),0 ,Primary!, NotModified!)
		wf_scroll("LAST")
		
		l_status = dw_bcs_stage2.GetItemStatus(dw_bcs_stage2.GetRow(),"cttl", Primary!)
		IF l_status = NotModified! THEN
			wf_modify_title()
		END IF
	END IF
ELSE
	wf_scroll("LAST")
	
	l_status = dw_bcs_stage2.GetItemStatus(dw_bcs_stage2.GetRow(),"cttl", Primary!)
	IF l_status = NotModified! THEN
		wf_modify_title()
	END IF
END IF
end event

event mousemove;SetMicroHelp(w_pics_main,this.tag)

end event

type sle_date1 from singlelineedit within w_sheet_bcs_stage2
integer x = 1979
integer y = 1160
integer width = 325
integer height = 84
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
boolean autohscroll = false
borderstyle borderstyle = stylelowered!
end type

type sle_date2 from singlelineedit within w_sheet_bcs_stage2
integer x = 2469
integer y = 1152
integer width = 320
integer height = 88
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
boolean autohscroll = false
borderstyle borderstyle = stylelowered!
end type

type cb_pub from commandbutton within w_sheet_bcs_stage2
event clicked pbm_bnclicked
event mousemove pbm_mousemove
string tag = "Publisher Information"
integer x = 55
integer y = 1432
integer width = 274
integer height = 80
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
boolean enabled = false
string text = "Pu&blisher..."
end type

event clicked;open(w_publisher)
end event

event mousemove;SetMicroHelp(w_pics_main,this.tag)

end event

type cb_anno from commandbutton within w_sheet_bcs_stage2
event clicked pbm_bnclicked
event mousemove pbm_mousemove
string tag = "Annotation Information"
integer x = 361
integer y = 1432
integer width = 325
integer height = 80
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
boolean enabled = false
string text = "&Annotation..."
end type

event clicked;open(w_anno_rpt)

end event

event mousemove;SetMicroHelp(w_pics_main,this.tag)

end event

type cb_editinfo from commandbutton within w_sheet_bcs_stage2
event clicked pbm_bnclicked
event mousemove pbm_mousemove
string tag = "Edit Information Information"
integer x = 727
integer y = 1432
integer width = 480
integer height = 80
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
boolean enabled = false
string text = "Edi&t Information..."
end type

event clicked;int rownum
dw_bcs_stage2.AcceptText()
rownum = dw_bcs_stage2.getrow()

IF dw_bcs_stage2.object.cs1init[rownum]<>"" THEN
	OpenSheet(w_sheet_edit_info, w_pics_main, 0, Original!)
ELSE
	MessageBox("ERROR","Cataloger Initials must be entered!",Information!)
END IF

end event

event mousemove;SetMicroHelp(w_pics_main,this.tag)

end event

type cb_export from commandbutton within w_sheet_bcs_stage2
event clicked pbm_bnclicked
event mousemove pbm_mousemove
string tag = "Export"
integer x = 1243
integer y = 1432
integer width = 265
integer height = 80
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
boolean enabled = false
string text = "Exp&ort..."
end type

event clicked;dw_summary.SaveAs()

end event

event mousemove;SetMicroHelp(w_pics_main,this.tag)

end event

type cb_sum from commandbutton within w_sheet_bcs_stage2
event clicked pbm_bnclicked
event mousemove pbm_mousemove
string tag = "Set date out"
integer x = 1536
integer y = 1432
integer width = 361
integer height = 80
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
boolean enabled = false
string text = "&Set date out"
end type

event clicked;int rownum

FOR rownum=1 TO dw_bcs_stage2.RowCount()
	dw_bcs_stage2.SetItem(rownum, "cs2out", today())
NEXT



end event

event mousemove;SetMicroHelp(w_pics_main,this.tag)

end event

type pb_find from commandbutton within w_sheet_bcs_stage2
event clicked pbm_bnclicked
event mousemove pbm_mousemove
string tag = "Find a record"
integer x = 1975
integer y = 1432
integer width = 192
integer height = 80
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
boolean enabled = false
string text = "F&ind"
end type

event clicked;string rtn,rc
long ll_rows
integer res

IF (dw_summary.visible = TRUE) THEN
	dw_summary.visible = FALSE
	cb_summary.text ='Summary...'
END IF
// IF find is dispalyed on the push button
IF pb_find.BringToTop = FALSE THEN
	// disable the buttons 
   w_sheet_bcs_stage2.wf_disable_buttons()
	// Turn on query mode so user can specify data
	rtn = w_sheet_bcs_stage2.dw_bcs_stage2.Modify("DataWindow.QueryMode=YES")
	IF rtn = "" THEN
		// If Modify succeeds, show Execute,
		// Query mode is on and display sort CheckBox
		This.BringToTop = TRUE
		This.Text = "Ex&ecute"
		dw_bcs_stage2.SetFocus()
	   SetMicroHelp(w_pics_main,"Query Mode...")
	ELSE
		MessageBox("Error", "Can't access query mode to select data.")
	END IF
ELSE
	dw_bcs_stage2.AcceptText()
	// Turn off Query mode and retrieve data 
	// based on user's choices
	rtn = w_sheet_bcs_stage2.dw_bcs_stage2.Modify("DataWindow.QueryMode=NO")
	IF rtn = "" THEN
		// If Modify succeeds, show Find,
		// Query mode is off, and retrieve data
		This.BringToTop = FALSE
		This.Text = "F&ind"
		ll_rows=w_sheet_bcs_stage2.dw_bcs_stage2.Retrieve()
		IF ll_rows > 0 THEN 
			// If any rows were retrieved.
      	SetMicroHelp(w_pics_main,"")
      	w_sheet_bcs_stage2.wf_set_counts()
			wf_modify_title()
	   	w_sheet_bcs_stage2.wf_enable_buttons()
		ELSE
			// If no rows were retrieved, ask if they want to continue with retrieval.
      	SetMicroHelp(w_pics_main,"")
      	w_sheet_bcs_stage2.wf_set_counts()
			res = MessageBox("Retrieve Error","No records were retrieved. Continue with query mode?", Question!, OkCancel!, 2 )
			IF res = 1 THEN
				// If yes continue the reterival process
				pb_find.TriggerEvent(Clicked!)
			ELSE
				// Restore the original select statement and modify the datawindow
				// mod_string is a shared variable, and is set in ue_postconstructor event of dw_bcs_stage2.
				res = dw_bcs_stage2.Reset()
				IF res = 1 THEN
				// Restore the original data from dataobject
				   dw_bcs_stage2.DataObject = dw_bcs_stage2.DataObject
					dw_bcs_stage2.SetTransObject( SQLServerTrans )
					rc = dw_bcs_stage2.Modify(mod_string)
				// if the modify select statement fails display error
				// messages, else retrieve the original data.
					IF rc = "" THEN
						ll_rows = w_sheet_bcs_stage2.dw_bcs_stage2.Retrieve()
						IF ll_rows > 0 THEN
							IF dw_bcs_stage2.Sharedata(dw_summary) = -1 THEN
   							MessageBox("ERROR", "Error sharing data between datawindows.",StopSign!, OK!, 2)
							END IF
      					SetMicroHelp(w_pics_main,"")
      					w_sheet_bcs_stage2.wf_set_counts()
							wf_modify_title()
	   					w_sheet_bcs_stage2.wf_enable_buttons()
						ELSE
							MessageBox("Error","No records were retrieved.")
						END IF // ll_rows > 0
					ELSE
						MessageBox("Error","Error in restoring the original select statement. ReturnCode=" + rc)
					END IF // rc = ""
				END IF // res = 1
			END IF // res = 1 
		END IF // ll_rows > 0
	ELSE
		MessageBox("Error","Failure exiting query mode.")
   	w_sheet_bcs_stage2.wf_enable_buttons()
	END IF // rtn = ""
END IF // if pb_find...
end event

event mousemove;SetMicroHelp(w_pics_main,this.tag)

end event

type pb_print from commandbutton within w_sheet_bcs_stage2
event clicked pbm_bnclicked
event mousemove pbm_mousemove
string tag = "Print the record"
integer x = 2190
integer y = 1432
integer width = 187
integer height = 80
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
boolean enabled = false
string text = "&Print"
end type

event clicked;Ldstg2dt = date(dw_bcs_stage2.object.cs2in[dw_bcs_stage2.getrow()])
OpenSheetwithparm(w_sheet_pics_ole_crystal,"stage2List",w_pics_main, 0, Original!)

end event

event mousemove;SetMicroHelp(w_pics_main,this.tag)

end event

type pb_update from commandbutton within w_sheet_bcs_stage2
event clicked pbm_bnclicked
event mousemove pbm_mousemove
string tag = "Update the record"
integer x = 2400
integer y = 1432
integer width = 201
integer height = 80
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
boolean enabled = false
string text = "&Update"
end type

event clicked;integer  rtn,rownum,ll_row
string Ls1init,Lconno,Ls2out
boolean val_init
dwItemStatus cat_status

dw_bcs_stage2.AcceptText()

IF dw_bcs_stage2.ModifiedCount() > 0 THEN
	
	dw_bcs_stage2.SetTransObject(sqlservertrans)
	
	ll_row = dw_bcs_stage2.GetRow()
	
	Ls1init = dw_bcs_stage2.object.cs1init[ll_row]
	Ls2out = string(dw_bcs_stage2.object.cs2out[ll_row],'MM/DD/YYYY')
	Lconno = dw_bcs_stage2.object.cconno[ll_row]
	
	IF IsNull(Ls1init) THEN
   	MessageBox("ERROR","You must enter values for ~'Catalog Initials~', before updating database.",StopSign!)
   	dw_bcs_stage2.SetFocus()
		RETURN 0
	ELSE 
		SetPointer(HourGlass!)
		val_init = f_valid_initial(Ls1init)
		IF val_init = TRUE THEN
   		SetMicroHelp(w_pics_main,"Updating database, Please wait...")
			
			FOR rownum=1 TO dw_bcs_stage2.RowCount()
				Ls1init = dw_bcs_stage2.object.cs1init[rownum]
				Ls2out = string(dw_bcs_stage2.object.cs2out[rownum],'MM/DD/YYYY')
				Lconno = dw_bcs_stage2.object.cconno[rownum]
				IF IsNuLL(Ls1init) THEN
					CONTINUE
				ELSE
					UPDATE catalog SET s1init = :Ls1init, s2out = to_date(:Ls2out,'MM/DD/YYYY') WHERE conno = :Lconno
					USING sqlservertrans;
				END IF	
				cat_status = dw_bcs_stage2.GetItemStatus(rownum, 0, Primary!)
				IF cat_status = DataModified! THEN
					// Mark MCHAR Table
					Lconno = dw_bcs_stage2.Object.cconno[rownum]
					f_update_mchar_time(Lconno,0,"C","U")
				END IF
			NEXT
			
			IF sqlservertrans.SQLCode < 0 THEN
				String ls_message,ls_msgparm[1]
				ls_message = "A database error has occurred.~n" + &
								 "Database error code:  " + String (sqlservertrans.sqldbcode) + "~r~n" + &
								 "Database error message:~r~n" + sqlservertrans.sqlerrtext
				If IsValid(gnv_app.inv_error) Then
					ls_msgparm[1] = ls_message
					gnv_app.inv_error.of_Message ('pfc_dwdberror', ls_msgparm, &
							gnv_app.iapp_object.DisplayName)
				Else
					Messagebox (gnv_app.iapp_object.DisplayName, ls_message, StopSign!, Ok!)
				End If
   			SetMicroHelp(w_pics_main,"")
			ELSE
				commit using sqlservertrans;
   			SetMicroHelp(w_pics_main,"Database has been updated successfully.")
				SetPointer(Arrow!)
				wf_set_counts()
				ib_disableclosequery = TRUE
				RETURN 1
			END IF
		ELSE
  			MessageBox("ERROR","Invalid catalog initials.",StopSign!)
  			dw_bcs_stage2.SetFocus()
			RETURN 0
		END IF // validate catalog initials
	END IF // Is NULL
ELSE 
   SetMicroHelp(w_pics_main,"No changes to the database.")
	RETURN 0
END IF // Modified count

end event

event mousemove;SetMicroHelp(w_pics_main,this.tag)

end event

type pb_exit from commandbutton within w_sheet_bcs_stage2
event clicked pbm_bnclicked
event mousemove pbm_mousemove
string tag = "Exit application"
integer x = 2624
integer y = 1432
integer width = 197
integer height = 80
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "E&xit"
end type

event clicked;close(parent)
m_pics_main.m_menu.PopMenu(300, 0)

end event

event mousemove;SetMicroHelp(w_pics_main,this.tag)

end event

type cb_range from commandbutton within w_sheet_bcs_stage2
integer x = 407
integer y = 1300
integer width = 329
integer height = 92
boolean bringtotop = true
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
boolean enabled = false
string text = "Set &Range..."
end type

event clicked;dw_bcs_stage2.AcceptText()
dw_bcs_stage2.Object.DataWindow.Table.Select=original_select
open(w_stage2_datein)

end event

type cb_reset_dateout from commandbutton within w_sheet_bcs_stage2
integer x = 800
integer y = 1300
integer width = 384
integer height = 92
boolean bringtotop = true
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
boolean enabled = false
string text = "R&eset date out"
end type

event clicked;open(w_bcs_reset_stage2_out)
end event

type st_from from statictext within w_sheet_bcs_stage2
integer x = 1833
integer y = 1172
integer width = 137
integer height = 64
boolean bringtotop = true
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long backcolor = 79741120
boolean enabled = false
string text = "From:"
boolean focusrectangle = false
end type

type st_to from statictext within w_sheet_bcs_stage2
integer x = 2327
integer y = 1168
integer width = 110
integer height = 68
boolean bringtotop = true
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long backcolor = 79741120
boolean enabled = false
string text = "To:"
boolean focusrectangle = false
end type

type dw_bcs_stage2 from u_pics_dw within w_sheet_bcs_stage2
event clicked pbm_dwnlbuttonclk
event itemfocuschanged pbm_dwnitemchangefocus
event rbuttondown pbm_dwnrbuttondown
event rbuttonup pbm_dwnrbuttonup
event ue_postconstructor ( )
event hitenterkey pbm_dwnprocessenter
integer x = 37
integer y = 32
integer width = 2789
integer height = 1096
integer taborder = 10
string dataobject = "d_bcs_stage2"
boolean vscrollbar = false
boolean livescroll = false
end type

event clicked;call super::clicked;dw_summary.ScrollToRow(row) 
end event

event itemfocuschanged;call super::itemfocuschanged;IF dwo.tag <> "?" THEN
	SetMicroHelp(w_pics_main,dwo.tag)
ELSE
	SetMicroHelp(w_pics_main,"This field is not modifiable!!!")
END IF
end event

event rbuttondown;//
end event

event rbuttonup;//
end event

event ue_postconstructor;string where_clause,rc,Ls2infrom,Ls2into
int ll_rows,RowNum
n_cst_string		lnv_string

IF IsValid(w_stage2_datein) THEN
	Ls2infrom = w_stage2_datein.S2dateinfrom
	Ls2into   = w_stage2_datein.S2dateinto
	
	IF IsNull(Ls2into)=FALSE AND IsNull(Ls2infrom)=FALSE THEN
		IF date(Ls2infrom) > date(Ls2into) THEN
			MessageBox("Error","Date range is wrong!")
			close(parent)
			RETURN
		END IF
	END IF
ELSEIF IsValid(w_s2datein) THEN
	Ls2infrom = w_s2datein.S2datein
END IF

sle_date1.text = Ls2infrom
sle_date2.text = Ls2into

SetMicroHelp(parent,"Please wait...")

this.SetTransObject( SQLServerTrans )

original_select =	this.Describe("DataWindow.Table.Select")

IF IsValid(w_stage2_datein) THEN
	IF (IsNull(Ls2infrom)=FALSE AND IsNull(Ls2into)=FALSE) THEN
		where_clause = " AND catalog.s2in BETWEEN TO_DATE("+ "~'" + LS2infrom + "~',~'mm/dd/yyyy~')" + " AND TO_DATE("+ "~'" + LS2into + "~',~'mm/dd/yyyy~')"
	ELSEIF (IsNull(Ls2infrom)=TRUE AND IsNull(Ls2into)=FALSE) THEN
		where_clause = " AND catalog.s2in <= TO_DATE("+ "~'" + LS2into + "~',~'mm/dd/yyyy~')"
	ELSEIF (IsNull(Ls2infrom)=FALSE AND IsNull(Ls2into)=TRUE) THEN
		where_clause = " AND catalog.s2in = TO_DATE("+ "~'" + LS2infrom + "~',~'mm/dd/yyyy~')"
	END IF
ELSEIF IsValid(w_s2datein) THEN
	where_clause = " AND catalog.s2in = TO_DATE("+ "~'" + LS2infrom + "~',~'mm/dd/yyyy~')"
END IF


original_select = lnv_string.of_GlobalReplace(original_select, "~'MA~'", "~~~'MA~~~'")
original_select = lnv_string.of_GlobalReplace(original_select, "~'PU~'", "~~~'PU~~~'")
original_select = lnv_string.of_GlobalReplace(original_select, "~'AB~'", "~~~'AB~~~'")
original_select = lnv_string.of_GlobalReplace(original_select, "~'T~'", "~~~'T~~~'")
original_select = lnv_string.of_GlobalReplace(original_select, "~'M~'", "~~~'M~~~'")
//messagebox("original_select",original_select)

mod_string = "DataWindow.Table.Select=~"" + original_select + where_clause + "~""
//messagebox("mod_string",mod_string)
rc = this.Modify(mod_string)
IF rc = "" THEN
	ll_rows = this.Retrieve()
   IF ll_rows < 1 THEN 
   	MessageBox("ERROR", "Stage II date in: " + string(LS2infrom) +" does not retrieve any rows.~rProbable causes:~r1- Invalid Stage II date in.~r2- All the control numbers assigned to this date have a stage II date out.",StopSign!, OK!, 2)
		SetMicroHelp(parent,"")
	  
	  	if IsValid(w_s2datein) then
 			close(w_s2datein)
	  	end if
     	close(parent)
	ELSE
		IF dw_bcs_stage2.Sharedata(dw_summary) = -1 THEN
   		MessageBox("ERROR", "Error sharing data between datawindows.",StopSign!, OK!, 2)
		END IF
		dw_bcs_stage2.SetSort("cconno")
		dw_bcs_stage2.Sort()
     	       wf_set_counts()
		wf_modify_title()
		
     	       this.SetFocus()
	 	SetMicroHelp(parent,"")
		  
	  	if IsValid(w_stage2_datein) then
 		 	close(w_stage2_datein)
	  	elseif IsValid(w_s2datein) then
 		 	close(w_s2datein)
	  	end if
		  
		parent.wf_enable_buttons()
		ib_disableclosequery = TRUE
	END IF
ELSE
	MessageBox("Status", "Modify select statement Failed." + rc + mod_string)
END IF
end event

event hitenterkey;call super::hitenterkey;Send(Handle(this),256,9,Long(0,0))
return(1)
end event

event sqlpreview;call super::sqlpreview;//messagebox("select",sqlsyntax)
end event

event constructor;call super::constructor;this.of_SetFilter(TRUE)
this.of_SetFind(TRUE)
this.inv_filter.of_SetStyle(1)
this.inv_filter.of_SetColumnNameSource(2)
this.of_SetSort(TRUE)
this.inv_sort.of_SetStyle(1)
this.inv_sort.of_SetColumnNameSource(2)

end event

event retrievestart;call super::retrievestart;OpenWithParm(w_pics_retrieve_msg_box,"Retrieving BCS Stage II data, Please Wait...")
end event

event retrieveend;call super::retrieveend;close(w_pics_retrieve_msg_box)
end event

event pfc_deleterow;//
RETURN 1
end event

event pfc_addrow;//
RETURN 1
end event

type dw_summary from u_pics_dw within w_sheet_bcs_stage2
event clicked pbm_dwnlbuttonclk
event rbuttondown pbm_dwnrbuttondown
event rowfocuschanged pbm_dwnrowchange
event rbuttonup pbm_dwnrbuttonup
boolean visible = false
integer x = 37
integer y = 32
integer width = 2789
integer height = 1096
integer taborder = 20
string dataobject = "d_bcs_sum"
boolean hscrollbar = true
end type

event clicked;call super::clicked;date null_date
string n_date,tday
SetNull(null_date)

tday = string(today(),'MM/DD/YYYY')

n_date = "00/00/0000"

IF dwo.name = "cs2out" THEN
	IF string(dw_summary.object.cs2out[row],'MM/DD/YYYY') <> n_date THEN
		dw_summary.object.cs2out[row] = null_date
	ELSE
		dw_summary.object.cs2out[row] = date(tday)
	END IF
END IF

ib_disableclosequery = FALSE

dw_bcs_stage2.ScrollToRow(row) 
end event

event rbuttondown;//
end event

event rowfocuschanged;call super::rowfocuschanged;dw_bcs_stage2.ScrollToRow(currentrow)
end event

event rbuttonup;//
end event

event constructor;call super::constructor;string ls_excludecols[]

this.of_SetFind(TRUE)

this.of_SetFilter(TRUE)
this.inv_filter.of_SetStyle(1)
this.inv_filter.of_SetColumnNameSource(2)

ls_excludecols[1] = "mchar_med"
ls_excludecols[2] = "mchar_bkseq"
ls_excludecols[3] = "mchar_vols"
ls_excludecols[4] = "mchar_fr"
ls_excludecols[5] = "ancntr_prdr"
ls_excludecols[6] = "catalog_s2in"
ls_excludecols[7] = "catalog_s2out"
ls_excludecols[8] = "catalog_cat"
ls_excludecols[9] = "catalog_othermed"
ls_excludecols[10] = "narr_narr"
ls_excludecols[11] = "narr_narrfn"
ls_excludecols[12] = "narr_recagcy"
ls_excludecols[13] = "ri_prevbkmed"
ls_excludecols[14] = "ri_prevbkseq"
ls_excludecols[15] = "ttlinit_chno"
ls_excludecols[16] = "ttlinit_auth"
ls_excludecols[17] = "ttlinit_authfn"
ls_excludecols[18] = "ttlinit_sttl"
ls_excludecols[19] = "ttlinit_ajyfn"
ls_excludecols[20] = "ttlinit_publisher"
ls_excludecols[21] = "ttlinit_lcno"
ls_excludecols[22] = "ttlinit_serttl"
ls_excludecols[23] = "ttlinit_seqnote"
ls_excludecols[24] = "ttlinit_dewey"
ls_excludecols[25] = "prod_cntr"
ls_excludecols[26] = "prod_actendt"
this.inv_filter.of_SetExclude(ls_excludecols)

this.of_SetSort(TRUE)
this.inv_sort.of_SetStyle(1)
this.inv_sort.of_SetColumnNameSource(2)



end event

event pfc_addrow;//
RETURN 1
end event

event pfc_deleterow;//
RETURN 1
end event


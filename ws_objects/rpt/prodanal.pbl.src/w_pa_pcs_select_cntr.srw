$PBExportHeader$w_pa_pcs_select_cntr.srw
forward
global type w_pa_pcs_select_cntr from w_child
end type
type em_schstdt from uo_date within w_pa_pcs_select_cntr
end type
type st_fstdate from statictext within w_pa_pcs_select_cntr
end type
type sle_batchsize from singlelineedit within w_pa_pcs_select_cntr
end type
type st_batchsize from statictext within w_pa_pcs_select_cntr
end type
type cb_delete from commandbutton within w_pa_pcs_select_cntr
end type
type cbx_unusable from checkbox within w_pa_pcs_select_cntr
end type
type cb_update from commandbutton within w_pa_pcs_select_cntr
end type
type cbx_deselect from checkbox within w_pa_pcs_select_cntr
end type
type cb_ok from commandbutton within w_pa_pcs_select_cntr
end type
type cb_cancel from commandbutton within w_pa_pcs_select_cntr
end type
type cbx_bk_stats from checkbox within w_pa_pcs_select_cntr
end type
type cbx_bk_selection from checkbox within w_pa_pcs_select_cntr
end type
type dw_pcs_tone_text_stat from u_pics_dw within w_pa_pcs_select_cntr
end type
type dw_unusable_rtbs from u_pics_dw within w_pa_pcs_select_cntr
end type
type dw_disassign_convbooks from u_pics_dw within w_pa_pcs_select_cntr
end type
type dw_selectbooks_convpcs2 from u_pics_dw within w_pa_pcs_select_cntr
end type
type dw_pcs_tone_text_selection from u_pics_dw within w_pa_pcs_select_cntr
end type
type dw_pcs_cntr from u_pics_dw within w_pa_pcs_select_cntr
end type
end forward

global type w_pa_pcs_select_cntr from w_child
string tag = "Conversion Books Assignment Center"
integer width = 3735
integer height = 2076
string title = "Conversion Books Assignment Center"
windowstate windowstate = maximized!
em_schstdt em_schstdt
st_fstdate st_fstdate
sle_batchsize sle_batchsize
st_batchsize st_batchsize
cb_delete cb_delete
cbx_unusable cbx_unusable
cb_update cb_update
cbx_deselect cbx_deselect
cb_ok cb_ok
cb_cancel cb_cancel
cbx_bk_stats cbx_bk_stats
cbx_bk_selection cbx_bk_selection
dw_pcs_tone_text_stat dw_pcs_tone_text_stat
dw_unusable_rtbs dw_unusable_rtbs
dw_disassign_convbooks dw_disassign_convbooks
dw_selectbooks_convpcs2 dw_selectbooks_convpcs2
dw_pcs_tone_text_selection dw_pcs_tone_text_selection
dw_pcs_cntr dw_pcs_cntr
end type
global w_pa_pcs_select_cntr w_pa_pcs_select_cntr

on w_pa_pcs_select_cntr.create
int iCurrent
call super::create
this.em_schstdt=create em_schstdt
this.st_fstdate=create st_fstdate
this.sle_batchsize=create sle_batchsize
this.st_batchsize=create st_batchsize
this.cb_delete=create cb_delete
this.cbx_unusable=create cbx_unusable
this.cb_update=create cb_update
this.cbx_deselect=create cbx_deselect
this.cb_ok=create cb_ok
this.cb_cancel=create cb_cancel
this.cbx_bk_stats=create cbx_bk_stats
this.cbx_bk_selection=create cbx_bk_selection
this.dw_pcs_tone_text_stat=create dw_pcs_tone_text_stat
this.dw_unusable_rtbs=create dw_unusable_rtbs
this.dw_disassign_convbooks=create dw_disassign_convbooks
this.dw_selectbooks_convpcs2=create dw_selectbooks_convpcs2
this.dw_pcs_tone_text_selection=create dw_pcs_tone_text_selection
this.dw_pcs_cntr=create dw_pcs_cntr
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.em_schstdt
this.Control[iCurrent+2]=this.st_fstdate
this.Control[iCurrent+3]=this.sle_batchsize
this.Control[iCurrent+4]=this.st_batchsize
this.Control[iCurrent+5]=this.cb_delete
this.Control[iCurrent+6]=this.cbx_unusable
this.Control[iCurrent+7]=this.cb_update
this.Control[iCurrent+8]=this.cbx_deselect
this.Control[iCurrent+9]=this.cb_ok
this.Control[iCurrent+10]=this.cb_cancel
this.Control[iCurrent+11]=this.cbx_bk_stats
this.Control[iCurrent+12]=this.cbx_bk_selection
this.Control[iCurrent+13]=this.dw_pcs_tone_text_stat
this.Control[iCurrent+14]=this.dw_unusable_rtbs
this.Control[iCurrent+15]=this.dw_disassign_convbooks
this.Control[iCurrent+16]=this.dw_selectbooks_convpcs2
this.Control[iCurrent+17]=this.dw_pcs_tone_text_selection
this.Control[iCurrent+18]=this.dw_pcs_cntr
end on

on w_pa_pcs_select_cntr.destroy
call super::destroy
destroy(this.em_schstdt)
destroy(this.st_fstdate)
destroy(this.sle_batchsize)
destroy(this.st_batchsize)
destroy(this.cb_delete)
destroy(this.cbx_unusable)
destroy(this.cb_update)
destroy(this.cbx_deselect)
destroy(this.cb_ok)
destroy(this.cb_cancel)
destroy(this.cbx_bk_stats)
destroy(this.cbx_bk_selection)
destroy(this.dw_pcs_tone_text_stat)
destroy(this.dw_unusable_rtbs)
destroy(this.dw_disassign_convbooks)
destroy(this.dw_selectbooks_convpcs2)
destroy(this.dw_pcs_tone_text_selection)
destroy(this.dw_pcs_cntr)
end on

event pfc_preopen;call super::pfc_preopen;this.of_SetPreference(TRUE)
this.inv_preference.of_SetToolBars(TRUE)
this.inv_preference.of_SetWindow(TRUE)
this.of_SetResize(TRUE)
this.inv_resize.of_SetOrigSize(this.workspacewidth(),this.workspaceheight())
inv_resize.of_Register(dw_pcs_cntr, "Scale")
inv_resize.of_Register(dw_pcs_tone_text_stat, "Scale")
inv_resize.of_Register(dw_pcs_tone_text_selection, "Scale")
inv_resize.of_Register(dw_selectbooks_convpcs2, "Scale")
inv_resize.of_Register(dw_disassign_convbooks, "Scale")
inv_resize.of_Register(dw_unusable_rtbs, "Scale")
inv_resize.of_Register(cb_cancel, "Scale")
inv_resize.of_Register(cb_ok, "Scale")
inv_resize.of_Register(cb_update, "Scale")
inv_resize.of_Register(cb_delete, "Scale")
inv_resize.of_Register(cbx_bk_selection, "Scale")
inv_resize.of_Register(cbx_bk_stats, "Scale")
inv_resize.of_Register(cbx_deselect, "Scale")
inv_resize.of_Register(cbx_unusable, "Scale")

inv_resize.of_Register(st_batchsize, "Scale")
inv_resize.of_Register(st_fstdate, "Scale")
inv_resize.of_Register(em_schstdt, "Scale")
inv_resize.of_Register(sle_batchsize, "Scale")

cbx_bk_stats.checked=true
dw_pcs_tone_text_selection.visible=true
open(w_pics_retrieve_msg_box)

end event

type em_schstdt from uo_date within w_pa_pcs_select_cntr
boolean visible = false
integer x = 1280
integer y = 1856
integer width = 256
integer height = 64
integer taborder = 20
integer textsize = -8
fontcharset fontcharset = ansi!
borderstyle borderstyle = stylelowered!
string mask = "mm/dd/yy"
string displaydata = "~t/"
end type

type st_fstdate from statictext within w_pa_pcs_select_cntr
boolean visible = false
integer x = 622
integer y = 1856
integer width = 695
integer height = 52
integer textsize = -8
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 33554432
long backcolor = 67108864
string text = "Date of first schedule start date:"
boolean focusrectangle = false
end type

type sle_batchsize from singlelineedit within w_pa_pcs_select_cntr
boolean visible = false
integer x = 439
integer y = 1856
integer width = 110
integer height = 64
integer taborder = 10
integer textsize = -8
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 33554432
borderstyle borderstyle = stylelowered!
end type

type st_batchsize from statictext within w_pa_pcs_select_cntr
boolean visible = false
integer x = 32
integer y = 1856
integer width = 430
integer height = 52
integer textsize = -8
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 33554432
long backcolor = 67108864
string text = "Size of each batch:"
boolean focusrectangle = false
end type

type cb_delete from commandbutton within w_pa_pcs_select_cntr
boolean visible = false
integer x = 1573
integer y = 1856
integer width = 585
integer height = 96
integer textsize = -8
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "&Delete unusable books"
end type

event clicked;String ls_cntr
Int rc, rtn, i , ll_rows
long li_bkseq

ll_rows = dw_unusable_rtbs.RowCount()

IF ll_rows > 0 THEN // If rows exist in unusable books by producers
	IF dw_unusable_rtbs.ModifiedCount() > 0 THEN	// If there are any changes
		dw_unusable_rtbs.AcceptText()		// accept any changes
		rtn = Messagebox("Delete Records","Are you sure that you want to delete these books from PICS database?",question!,yesNo!,2)
		IF rtn = 1 THEN
			FOR i=1 TO ll_rows
				IF dw_unusable_rtbs.object.delete_bk[i] = 'Y' THEN
					li_bkseq = dw_unusable_rtbs.object.booksequence[i]
					DELETE FROM PROD
					WHERE BKSEQ = :li_bkseq
					USING SQLServerTrans;
					IF f_check_dberror(SqlServerTrans,"Updating PROD:")=FALSE THEN
						Messagebox("UPDATE ERROR","Failed to delete book number: "+ String(li_bkseq) )
						ROLLBACK USING SqlServerTrans;
						RETURN -1
					ELSE
						DELETE FROM CONVERSIONBOOKS
						WHERE BOOKSEQUENCE = :li_bkseq
						AND ACTION_TYPE='U'
						USING SQLServerTrans;
						IF f_check_dberror(SqlServerTrans,"Updating CONVERSIONBOOKS:")=FALSE THEN
							Messagebox("UPDATE ERROR","Failed to delete book number: "+ String(li_bkseq) )
							ROLLBACK USING SqlServerTrans;
							RETURN -1
						END IF
					END IF
				END IF
			NEXT
			IF f_check_dberror(SqlServerTrans,"Updating PROD:")=FALSE THEN
				ROLLBACK USING SqlServerTrans;
				RETURN -1
			ELSE
				COMMIT USING SqlServerTrans;
				MessageBox("Books Deletion","Selected unusable books are deleted from PROD table.")
				dw_unusable_rtbs.Retrieve()
			END IF			
		ELSE
			RETURN -1
		END IF			
	ELSE		
		Messagebox("Warning",'There are no changes to the unusable list of books.')		
	END IF
END IF
end event

type cbx_unusable from checkbox within w_pa_pcs_select_cntr
integer x = 2834
integer y = 1024
integer width = 681
integer height = 64
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 33554432
long backcolor = 80263581
string text = "Delete unusable books"
end type

event clicked;int ll_rows

IF this.checked=true THEN
	cbx_bk_stats.checked=false
	cbx_bk_selection.checked=false
	cbx_deselect.checked=false
		
	dw_disassign_convbooks.visible=false
	cb_update.visible=false
	dw_pcs_tone_text_selection.visible=false
	dw_selectbooks_convpcs2.visible=false
	cb_delete.visible=true
	
	open(w_pics_retrieve_msg_box)
	ll_rows = dw_unusable_rtbs.Retrieve()
	IF ll_rows > 0 THEN
		dw_unusable_rtbs.SetFocus()
	ELSE
		MessageBox("ERROR","There are no books set as unusable by producers.")
	END IF
	close(w_pics_retrieve_msg_box)
	
ELSE
	cb_update.visible=false
	cb_delete.visible=false
	cbx_bk_stats.checked=true
	dw_pcs_tone_text_selection.visible=true
END IF
end event

type cb_update from commandbutton within w_pa_pcs_select_cntr
boolean visible = false
integer x = 2194
integer y = 1856
integer width = 475
integer height = 96
integer textsize = -8
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "&Deselect books"
end type

event clicked;string ls_cntr
int rc

IF dw_disassign_convbooks.ModifiedCount() > 0 THEN

	dw_disassign_convbooks.accepttext()

	rc = dw_disassign_convbooks.Event pfc_Update(True,True)
	if rc = 1 then
		COMMIT USING SQLServerTrans;
		MessageBox("Update","Update Successful. Unchecked books were deselected from the list.")
	else 
		ROLLBACK USING SQLServerTrans;
		MessageBox("Error","Update failed. Error in table conversionbooks",StopSign!)
		RETURN -1
	end if	
	
	ls_cntr = dw_disassign_convbooks.object.cntr[1]
	
	dw_disassign_convbooks.Retrieve(ls_cntr)
	dw_pcs_cntr.Retrieve()
ELSE
	
	MessageBox("Warning",'There are no changes to the selection list.')
	
END IF
end event

type cbx_deselect from checkbox within w_pa_pcs_select_cntr
integer x = 2130
integer y = 1024
integer width = 594
integer height = 64
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 33554432
long backcolor = 80263581
string text = "Deselect books "
end type

event clicked;int ll_rows
string ls_cntr

IF this.checked=true THEN
	cbx_bk_stats.checked=false
	cbx_bk_selection.checked=false
	cbx_unusable.checked=false
	cb_delete.visible=false
		
	dw_disassign_convbooks.visible=true
	cb_update.visible=true
	dw_pcs_tone_text_selection.visible=false
	dw_selectbooks_convpcs2.visible=false
	cb_update.visible=true
	
	ll_rows = dw_pcs_cntr.GetRow()
	ls_cntr = dw_pcs_cntr.object.cntr[ll_rows]

	ll_rows = dw_disassign_convbooks.Retrieve(ls_cntr)
	IF ll_rows > 0 THEN
		dw_disassign_convbooks.SetFocus()
	ELSE
		MessageBox("ERROR","No books has been assigned to contract number "+ls_cntr)
	END IF
	
ELSE
	cb_update.visible=false
	cbx_bk_stats.checked=true
	dw_pcs_tone_text_selection.visible=true

END IF
end event

type cb_ok from commandbutton within w_pa_pcs_select_cntr
integer x = 2706
integer y = 1856
integer width = 658
integer height = 96
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "&Display Assignment Screen"
end type

event clicked;int ll_rows,rtn,li_bks_tones_text, li_bks_tones_notext, li_bks_notones_text, li_bks_notones_notext ,i,li_batch_size
string ls_cntr,ls_url,ls_booklist, db_inst
date ld_sched_start_date,ld_today
Inet linet_base

ld_today = DATE(today())
/*
IF IsNull(em_schstdt.text) OR em_schstdt.text = ""  THEN	
	rtn = MessageBox("Schedule Start Date","You have not assinged a date for the first schedule start date. Do you want the system to assign today's date for the first schedule start date?",Question!,YesNo!,2)
	IF rtn = 2 THEN
		SetFocus(em_schstdt)
		RETURN
	ELSE
		em_schstdt.text = String(ld_today,'MM/DD/YY')	
		ld_sched_start_date = ld_today
	END IF
ELSE
	ld_sched_start_date = DATE(em_schstdt.text)
END IF

IF IsNull(sle_batchsize.text) OR sle_batchsize.text = "" THEN
	rtn = MessageBox("Size of the batch","You have not assinged the size of the batch. Do you want the system to assign 1 for the size of the batch?",Question!,YesNo!,2)
	IF rtn = 2 THEN
		SetFocus(sle_batchsize)
		RETURN
	ELSE
		sle_batchsize.text = '1'
	END IF
END IF

li_batch_size = INTEGER(sle_batchsize.text)
*/

db_inst = w_pics_main.web_db_inst

IF cbx_bk_stats.checked=TRUE THEN

   dw_pcs_cntr.SetFocus()
	
	ll_rows = dw_pcs_cntr.GetRow()
	
	ls_cntr = dw_pcs_cntr.object.cntr[ll_rows]
	
	IF ll_rows > 0 THEN
	
		li_bks_tones_text = dw_pcs_tone_text_selection.object.bks_tones_text[1]
		
		li_bks_tones_notext = dw_pcs_tone_text_selection.object.bks_tones_notext[1]
		
		li_bks_notones_text = dw_pcs_tone_text_selection.object.bks_notones_text[1]

		li_bks_notones_notext = dw_pcs_tone_text_selection.object.bks_notones_notext[1]

		ls_url = "https://oraserve.loc.gov:4446/"+lower(db_inst)+"/pcs_convbooks_assign_prdr?form_cntrcode="+ls_cntr+"&form_bks_tones_text="+string(li_bks_tones_text)+"&form_bks_tones_notext="+string(li_bks_tones_notext)+"&form_bks_notones_text="+string(li_bks_notones_text)+"&form_bks_notones_notext="+string(li_bks_notones_notext)+" "

	ELSE
		RETURN
	END IF
	
ELSEIF cbx_bk_selection.checked=TRUE THEN
	
	ll_rows = dw_pcs_cntr.GetRow()
	
	ls_cntr = dw_pcs_cntr.object.cntr[ll_rows]

   IF ll_rows > 0 THEN	
		dw_selectbooks_convpcs2.SetFilter("upd_flag = 'Y'")
		dw_selectbooks_convpcs2.Filter()
	
		ll_rows = dw_selectbooks_convpcs2.rowcount()
		
		ls_booklist = ' '
		FOR i = 1 TO ll_rows
			IF i < ll_rows THEN
				ls_booklist = ls_booklist + string(dw_selectbooks_convpcs2.object.bkseq[i]) + ','
			ELSE
				ls_booklist = ls_booklist + string(dw_selectbooks_convpcs2.object.bkseq[i])
			END IF
		NEXT
		
		ls_booklist = trim(ls_booklist)
		ls_url = "https://oraserve.loc.gov:4446/"+lower(db_inst)+"/pcs_convbks_assign_prdr_bklist?form_cntrcode="+ls_cntr+"&form_booklist="+ls_booklist
	//   messagebox('books',ls_booklist)
	//   messagebox('url',ls_url)
		dw_selectbooks_convpcs2.SetFilter("")
		dw_selectbooks_convpcs2.Filter()
	ELSE
		RETURN
	END IF

ELSE
	MessageBox('ERROR','Please check the apporiate box')
	RETURN
END IF

this.GetContextService("Internet",linet_base)
rtn = linet_base.HyperlinkToURL(ls_url)

If Isvalid(linet_base) Then destroy linet_base



end event

type cb_cancel from commandbutton within w_pa_pcs_select_cntr
integer x = 3401
integer y = 1856
integer width = 256
integer height = 96
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "&Cancel"
end type

event clicked;ib_disableclosequery = TRUE
close(parent)
end event

type cbx_bk_stats from checkbox within w_pa_pcs_select_cntr
integer x = 32
integer y = 1024
integer width = 1051
integer height = 64
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 33554432
long backcolor = 80263581
string text = "Select books using statistics above"
end type

event clicked;IF this.checked=true THEN
	cbx_bk_selection.checked=false
	cbx_deselect.checked=false
	dw_selectbooks_convpcs2.visible=false
	dw_disassign_convbooks.visible=false
	cb_update.visible=false
	dw_pcs_tone_text_selection.visible=true
	cbx_unusable.checked=false
	dw_unusable_rtbs.visible=false
	cb_delete.visible=false
END IF
end event

type cbx_bk_selection from checkbox within w_pa_pcs_select_cntr
integer x = 1129
integer y = 1024
integer width = 933
integer height = 64
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 33554432
long backcolor = 80263581
string text = "Select books using the list"
end type

event clicked;IF this.checked=true THEN
	cbx_bk_stats.checked=false
	cbx_deselect.checked=false
	dw_selectbooks_convpcs2.visible=true
	dw_pcs_tone_text_selection.visible=false
	dw_disassign_convbooks.visible=false
	cb_update.visible=false
	cbx_unusable.checked=false
	cb_delete.visible=false
	dw_selectbooks_convpcs2.SetTransObject( SQLServerTrans)
	dw_selectbooks_convpcs2.Retrieve()
	dw_selectbooks_convpcs2.SetFocus()
END IF
end event

type dw_pcs_tone_text_stat from u_pics_dw within w_pa_pcs_select_cntr
integer x = 32
integer y = 12
integer width = 3616
integer height = 264
integer taborder = 0
string dataobject = "d_pcs_tone_text_stat"
boolean vscrollbar = false
boolean livescroll = false
end type

event constructor;call super::constructor;this.SetTransObject( SQLServerTrans)
this.Retrieve()
end event

type dw_unusable_rtbs from u_pics_dw within w_pa_pcs_select_cntr
integer x = 37
integer y = 1120
integer width = 3611
integer height = 732
integer taborder = 30
string dataobject = "d_unusable_rtbs"
end type

event constructor;call super::constructor;this.SetTransObject( SQLServerTrans)

end event

type dw_disassign_convbooks from u_pics_dw within w_pa_pcs_select_cntr
integer x = 37
integer y = 1120
integer width = 3611
integer height = 732
integer taborder = 50
string dataobject = "d_disassign_convbooks"
end type

event constructor;call super::constructor;this.SetTransObject( SQLServerTrans)

end event

event itemchanged;call super::itemchanged;String nullstr
int job_id, nullid
SetNULL(nullid)

job_id = this.object.job_id[row]


IF dwo.name = "action_type" THEN
	IF data = "P" THEN
		this.object.job_id[row]=nullid
	ELSEIF data = "W" THEN
		this.object.job_id[row]=job_id		
	END IF
END IF
end event

type dw_selectbooks_convpcs2 from u_pics_dw within w_pa_pcs_select_cntr
integer x = 37
integer y = 1120
integer width = 3611
integer height = 732
integer taborder = 40
string dataobject = "d_selectbooks_convpcs2"
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

type dw_pcs_tone_text_selection from u_pics_dw within w_pa_pcs_select_cntr
integer x = 37
integer y = 1120
integer width = 3611
integer height = 724
integer taborder = 60
string dataobject = "d_pcs_tone_text_selection"
boolean vscrollbar = false
boolean livescroll = false
end type

event itemchanged;call super::itemchanged;IF dwo.Name = "bks_tones_text" THEN
	
	this.object.tot_bks[1] = integer(data) + this.object.bks_tones_notext[1] + &
									 this.object.bks_notones_text[1] + this.object.bks_notones_notext[1]

ELSEIF dwo.Name = "bks_tones_notext" THEN
	this.object.tot_bks[1] = this.object.bks_tones_text[1] + integer(data) + this.object.bks_notones_text[1] + this.object.bks_notones_notext[1]
	

ELSEIF dwo.Name = "bks_notones_text" THEN

	this.object.tot_bks[1] = this.object.bks_tones_text[1] + this.object.bks_tones_notext[1] + &
									 integer(data) + this.object.bks_notones_notext[1]
	
ELSEIF dwo.Name = "bks_notones_notext" THEN

	this.object.tot_bks[1] = this.object.bks_tones_text[1] + this.object.bks_tones_notext[1] + &
									 this.object.bks_notones_text[1] + integer(data)
									 
END IF
end event

event ue_postconstructor;call super::ue_postconstructor;this.SetTransObject( SQLServerTrans)
this.Retrieve()

close (w_pics_retrieve_msg_box)


end event

type dw_pcs_cntr from u_pics_dw within w_pa_pcs_select_cntr
integer x = 32
integer y = 280
integer width = 3616
integer height = 736
integer taborder = 0
string dataobject = "d_pcs_cntr"
boolean hscrollbar = true
end type

event ue_postconstructor;call super::ue_postconstructor;this.SetTransObject( SQLServerTrans)
this.Retrieve()
this.SetRowFocusIndicator(Hand!)
end event

event rowfocuschanged;call super::rowfocuschanged;INTEGER	li_ttls_per_week=0
STRING ls_cntr
	
ls_cntr = this.object.cntr[currentrow]
	
SELECT TITLES_PER_WEEK
INTO :li_ttls_per_week
FROM ANCNTR
WHERE CNTR = :ls_cntr
USING SQLServerTrans;
	
sle_batchsize.Text = String(li_ttls_per_week)

end event


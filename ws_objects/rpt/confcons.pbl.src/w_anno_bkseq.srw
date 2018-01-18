$PBExportHeader$w_anno_bkseq.srw
forward
global type w_anno_bkseq from w_sheet
end type
type pb_exit from commandbutton within w_anno_bkseq
end type
type pb_update from commandbutton within w_anno_bkseq
end type
type dw_anno_bkseq from u_pics_dw within w_anno_bkseq
end type
type cb_print from commandbutton within w_anno_bkseq
end type
type st_1 from statictext within w_anno_bkseq
end type
type em_bkseq from u_em within w_anno_bkseq
end type
type cb_find from commandbutton within w_anno_bkseq
end type
type cb_clear from commandbutton within w_anno_bkseq
end type
type cb_spell from commandbutton within w_anno_bkseq
end type
type st_2 from statictext within w_anno_bkseq
end type
type em_bkmed from u_em within w_anno_bkseq
end type
end forward

global type w_anno_bkseq from w_sheet
integer x = 214
integer y = 221
integer width = 3168
integer height = 2172
pb_exit pb_exit
pb_update pb_update
dw_anno_bkseq dw_anno_bkseq
cb_print cb_print
st_1 st_1
em_bkseq em_bkseq
cb_find cb_find
cb_clear cb_clear
cb_spell cb_spell
st_2 st_2
em_bkmed em_bkmed
end type
global w_anno_bkseq w_anno_bkseq

on w_anno_bkseq.create
int iCurrent
call super::create
this.pb_exit=create pb_exit
this.pb_update=create pb_update
this.dw_anno_bkseq=create dw_anno_bkseq
this.cb_print=create cb_print
this.st_1=create st_1
this.em_bkseq=create em_bkseq
this.cb_find=create cb_find
this.cb_clear=create cb_clear
this.cb_spell=create cb_spell
this.st_2=create st_2
this.em_bkmed=create em_bkmed
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.pb_exit
this.Control[iCurrent+2]=this.pb_update
this.Control[iCurrent+3]=this.dw_anno_bkseq
this.Control[iCurrent+4]=this.cb_print
this.Control[iCurrent+5]=this.st_1
this.Control[iCurrent+6]=this.em_bkseq
this.Control[iCurrent+7]=this.cb_find
this.Control[iCurrent+8]=this.cb_clear
this.Control[iCurrent+9]=this.cb_spell
this.Control[iCurrent+10]=this.st_2
this.Control[iCurrent+11]=this.em_bkmed
end on

on w_anno_bkseq.destroy
call super::destroy
destroy(this.pb_exit)
destroy(this.pb_update)
destroy(this.dw_anno_bkseq)
destroy(this.cb_print)
destroy(this.st_1)
destroy(this.em_bkseq)
destroy(this.cb_find)
destroy(this.cb_clear)
destroy(this.cb_spell)
destroy(this.st_2)
destroy(this.em_bkmed)
end on

type pb_exit from commandbutton within w_anno_bkseq
integer x = 2807
integer y = 1920
integer width = 283
integer height = 108
integer taborder = 90
integer textsize = -10
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "&Exit"
end type

event clicked;close(parent)
end event

type pb_update from commandbutton within w_anno_bkseq
integer x = 2482
integer y = 1920
integer width = 283
integer height = 108
integer taborder = 80
integer textsize = -10
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "&Update"
end type

event clicked;int rtn
string ls_oneliner,ls_chno

dw_anno_bkseq.AcceptText()

IF dw_anno_bkseq.ModifiedCount() > 0 THEN
	rtn = dw_anno_bkseq.of_Update(TRUE,TRUE)
	IF f_check_dberror(sqlservertrans,"Annotation") THEN
		IF rtn=1 THEN
			ls_chno = dw_anno_bkseq.object.chno[1]
			ls_oneliner = dw_anno_bkseq.object.oneliner[1]
			UPDATE ttlinit SET oneliner = :ls_oneliner WHERE chno = :ls_chno USING SQLServerTrans;
			IF f_check_dberror(sqlservertrans,"Ttlinit") THEN
				IF rtn=1 THEN
					Commit Using SQLServerTrans;
					MessageBox("Update"," Annotation updated.",Information!)
				ELSE
					Rollback Using SqlServerTrans;
					MessageBox("Update"," Annotation update failed.",StopSign!)
					RETURN
				END IF
			END IF
		ELSE
			Rollback Using SqlServerTrans;
			MessageBox("Update"," Annotation update failed.",StopSign!)
			RETURN
		END IF
	END IF
ELSE
	MessageBox("Update"," Nothing to update.",Information!)
END IF
end event

type dw_anno_bkseq from u_pics_dw within w_anno_bkseq
integer y = 160
integer width = 3072
integer height = 1728
integer taborder = 30
string dataobject = "d_anno_bkseq"
boolean vscrollbar = false
boolean livescroll = false
end type

type cb_print from commandbutton within w_anno_bkseq
integer x = 2158
integer y = 1920
integer width = 283
integer height = 104
integer taborder = 70
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "&Print"
end type

event clicked;dw_anno_bkseq.Triggerevent("pfc_Print")

end event

type st_1 from statictext within w_anno_bkseq
integer x = 32
integer y = 44
integer width = 457
integer height = 72
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 33554432
long backcolor = 67108864
string text = "Book Number"
alignment alignment = right!
boolean focusrectangle = false
end type

type em_bkseq from u_em within w_anno_bkseq
integer x = 526
integer y = 44
integer width = 384
integer height = 92
integer taborder = 10
string mask = "######"
end type

type cb_find from commandbutton within w_anno_bkseq
integer x = 1467
integer y = 1920
integer width = 283
integer height = 104
integer taborder = 50
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "&Find"
boolean default = true
end type

event clicked;long lbkseq
string lbkmed

lbkseq = long(em_bkseq.text)
lbkmed = em_bkmed.text

IF NOT(IsNull(lbkseq)) THEN
	dw_anno_bkseq.settransobject(sqlservertrans)
	dw_anno_bkseq.retrieve(lbkseq,lbkmed)
	dw_anno_bkseq.SetFocus()
END IF



end event

type cb_clear from commandbutton within w_anno_bkseq
integer x = 1815
integer y = 1920
integer width = 283
integer height = 104
integer taborder = 60
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "&Clear"
end type

event clicked;dw_anno_bkseq.reset()
em_bkseq.setFocus()

end event

type cb_spell from commandbutton within w_anno_bkseq
string tag = "Spell Checker"
integer x = 750
integer y = 1916
integer width = 334
integer height = 96
integer taborder = 40
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "&Spell Check"
end type

event clicked;nca_word lnca_word
String ls_S,ls_S2
	
dw_anno_bkseq.accepttext()
	
//annotation
ls_S = dw_anno_bkseq.object.anno[1]
IF NOT(IsNull(ls_S)) THEN
	lnca_Word.SpellCheck( ls_S )
	dw_anno_bkseq.object.anno[1] = ls_S
END IF

//foreign annotation
ls_S2 = dw_anno_bkseq.object.anno_foreign[1]
IF NOT(IsNull(ls_S2)) THEN
	lnca_Word.SpellCheck( ls_S2 )
	dw_anno_bkseq.object.anno_foreign[1] = ls_S2
END IF

//oneliner
ls_S2 = dw_anno_bkseq.object.oneliner[1]
IF NOT(IsNull(ls_S2)) THEN
	lnca_Word.SpellCheck( ls_S2 )
	dw_anno_bkseq.object.oneliner[1] = ls_S2
END IF

end event

type st_2 from statictext within w_anno_bkseq
integer x = 946
integer y = 44
integer width = 411
integer height = 72
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 33554432
long backcolor = 67108864
string text = "Book Medium"
boolean focusrectangle = false
end type

type em_bkmed from u_em within w_anno_bkseq
integer x = 1367
integer y = 44
integer width = 219
integer height = 92
integer taborder = 20
textcase textcase = upper!
maskdatatype maskdatatype = stringmask!
string mask = "XX"
string displaydata = "RC~tRC/BR~tBR/TB~tTB/FD~tFD/CB~tCB/RD~tRD/~t/~t/"
boolean ib_autoselect = true
end type


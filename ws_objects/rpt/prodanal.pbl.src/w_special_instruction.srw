$PBExportHeader$w_special_instruction.srw
forward
global type w_special_instruction from w_response
end type
type mle_sitxt from u_mle within w_special_instruction
end type
type cb_exit from u_cb within w_special_instruction
end type
type cb_update from u_cb within w_special_instruction
end type
type sle_author from singlelineedit within w_special_instruction
end type
type st_1 from statictext within w_special_instruction
end type
type st_2 from statictext within w_special_instruction
end type
type st_3 from statictext within w_special_instruction
end type
type dw_clock from u_pics_dw within w_special_instruction
end type
type cb_print from commandbutton within w_special_instruction
end type
type st_4 from statictext within w_special_instruction
end type
type sle_conno from singlelineedit within w_special_instruction
end type
type sle_title from multilineedit within w_special_instruction
end type
type dw_pa_special_instruction from datawindow within w_special_instruction
end type
type dw_pa_si_print from datawindow within w_special_instruction
end type
end forward

global type w_special_instruction from w_response
integer x = 146
integer y = 200
integer width = 2642
integer height = 1220
string title = "Special Instruction"
mle_sitxt mle_sitxt
cb_exit cb_exit
cb_update cb_update
sle_author sle_author
st_1 st_1
st_2 st_2
st_3 st_3
dw_clock dw_clock
cb_print cb_print
st_4 st_4
sle_conno sle_conno
sle_title sle_title
dw_pa_special_instruction dw_pa_special_instruction
dw_pa_si_print dw_pa_si_print
end type
global w_special_instruction w_special_instruction

type variables
string Lconno
boolean si_exist=FALSE
end variables

on w_special_instruction.create
int iCurrent
call super::create
this.mle_sitxt=create mle_sitxt
this.cb_exit=create cb_exit
this.cb_update=create cb_update
this.sle_author=create sle_author
this.st_1=create st_1
this.st_2=create st_2
this.st_3=create st_3
this.dw_clock=create dw_clock
this.cb_print=create cb_print
this.st_4=create st_4
this.sle_conno=create sle_conno
this.sle_title=create sle_title
this.dw_pa_special_instruction=create dw_pa_special_instruction
this.dw_pa_si_print=create dw_pa_si_print
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.mle_sitxt
this.Control[iCurrent+2]=this.cb_exit
this.Control[iCurrent+3]=this.cb_update
this.Control[iCurrent+4]=this.sle_author
this.Control[iCurrent+5]=this.st_1
this.Control[iCurrent+6]=this.st_2
this.Control[iCurrent+7]=this.st_3
this.Control[iCurrent+8]=this.dw_clock
this.Control[iCurrent+9]=this.cb_print
this.Control[iCurrent+10]=this.st_4
this.Control[iCurrent+11]=this.sle_conno
this.Control[iCurrent+12]=this.sle_title
this.Control[iCurrent+13]=this.dw_pa_special_instruction
this.Control[iCurrent+14]=this.dw_pa_si_print
end on

on w_special_instruction.destroy
call super::destroy
destroy(this.mle_sitxt)
destroy(this.cb_exit)
destroy(this.cb_update)
destroy(this.sle_author)
destroy(this.st_1)
destroy(this.st_2)
destroy(this.st_3)
destroy(this.dw_clock)
destroy(this.cb_print)
destroy(this.st_4)
destroy(this.sle_conno)
destroy(this.sle_title)
destroy(this.dw_pa_special_instruction)
destroy(this.dw_pa_si_print)
end on

event open;call super::open;string Lsitxt,ls_message,ls_msgparm[], ls_conno, ls_ttl, ls_auth, ls_navi
n_cst_string inv_string
long li_len

Lconno = trim(Message.StringParm)
ls_conno=left(lconno,8)
sle_conno.text=ls_conno
li_len=len(lconno)
if li_len=8 then
	sle_title.text=w_pa_bk_estimation.dw_pa_bk_estimation.object.ttlinit_ttl[1]
	sle_author.text=w_pa_bk_estimation.dw_pa_bk_estimation.object.ttlinit_auth[1]
else
	ls_navi=mid(lconno,9)
	if ls_navi='navigation' then
		select b.ttl, b.auth into :ls_ttl, :ls_auth
		from mchar a, ttlinit b
		where a.chno=b.chno and
				a.conno=:ls_conno
		using sqlservertrans;
		if f_check_dberror(sqlservertrans,'select from ttlinit to find ttl, auth')=false then
			return
		end if
		sle_title.text=ls_ttl
		sle_author.text=ls_auth
	end if
end if
select sitxt into :Lsitxt from specinst where conno = :ls_conno
	using sqlservertrans;
IF sqlservertrans.SQLCode < 0 THEN
	ls_message = "A database error has occurred in Insert.~n" + &
					 "Database error code:  " + String (sqlservertrans.sqldbcode) + "~r~n" + &
					 "Database error message:~r~n" + sqlservertrans.sqlerrtext
	IF IsValid(gnv_app.inv_error) THEN
		ls_msgparm[1] = ls_message
		gnv_app.inv_error.of_Message ('pfc_dwdberror', ls_msgparm, &
		gnv_app.iapp_object.DisplayName)
	ELSE
		Messagebox (gnv_app.iapp_object.DisplayName, ls_message, StopSign!, Ok!)
		ROLLBACK USING sqlservertrans;
		RETURN -1
	End If
ELSEIF SQlServerTrans.SQLCode = 100 THEN
	si_exist=FALSE
ELSEIF SQlServerTrans.SQLCode = 0 THEN
	si_exist=TRUE
	
	Lsitxt =  inv_string.of_RemoveNonPrint(Lsitxt)
	Lsitxt =  inv_string.of_Trim(Lsitxt)
	Lsitxt =  inv_string.of_GlobalReplace(Lsitxt, "       ", " ")
	Lsitxt =  inv_string.of_GlobalReplace(Lsitxt, "      ", " ")
	Lsitxt =  inv_string.of_GlobalReplace(Lsitxt, "     ", " ")
	Lsitxt =  inv_string.of_GlobalReplace(Lsitxt, "    ", " ")
	Lsitxt =  inv_string.of_GlobalReplace(Lsitxt, "   ", " ")
	Lsitxt =  inv_string.of_GlobalReplace(Lsitxt, "  ", " ")
	
	mle_sitxt.text = Lsitxt
	dw_pa_special_instruction.retrieve(ls_conno)
	dw_pa_si_print.retrieve(ls_conno)
END IF
mle_sitxt.SetFocus()

end event

type mle_sitxt from u_mle within w_special_instruction
integer x = 41
integer y = 516
integer width = 2565
integer height = 444
integer taborder = 70
integer textsize = -10
boolean vscrollbar = true
end type

type cb_exit from u_cb within w_special_instruction
integer x = 2354
integer y = 1000
integer width = 247
integer taborder = 0
string text = "E&xit"
end type

event clicked;call super::clicked;parent.Event pfc_close()
end event

type cb_update from u_cb within w_special_instruction
integer x = 2048
integer y = 1000
integer width = 233
integer taborder = 0
boolean bringtotop = true
string text = "&Update"
end type

event clicked;call super::clicked;string Lsitxt,ls_message,ls_msgparm[], ls_conno

Lsitxt = mle_sitxt.text
Lsitxt = RightTrim(Lsitxt)

ls_conno=left(lconno,8)
IF (si_exist=FALSE) THEN 
	// Insert the special instruction into the annotation table.
	INSERT INTO specinst(conno,sitxt) VALUES (:ls_conno,:Lsitxt) USING SQLServerTrans;
ELSEIF (si_exist=TRUE) THEN
	// If special Insruction exist update the table
	Update specinst SET sitxt = :Lsitxt WHERE conno = :ls_conno USING SQLServerTrans;
END IF
IF f_check_dberror(sqlservertrans,"SPECINST") THEN
	COMMIT USING SQLServerTrans;
	MessageBox("update","Special Instruction for control number: "+ls_conno+" was updated.",Information!)
	dw_pa_special_instruction.retrieve(ls_conno)
	IF dw_pa_special_instruction.Sharedata(dw_pa_si_print) = -1 THEN
		MessageBox("ERROR", "Error sharing data between datawindows.",StopSign!, OK!, 2)
	END IF
ELSE
	ROLLBACK USING sqlservertrans;
	RETURN -1
END IF
end event

type sle_author from singlelineedit within w_special_instruction
integer x = 142
integer y = 396
integer width = 882
integer height = 92
integer taborder = 40
boolean bringtotop = true
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long backcolor = 12632256
boolean autohscroll = false
borderstyle borderstyle = stylelowered!
end type

type st_1 from statictext within w_special_instruction
integer x = 37
integer y = 160
integer width = 512
integer height = 76
boolean bringtotop = true
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long backcolor = 12632256
boolean enabled = false
string text = "Special Instructions for:"
boolean focusrectangle = false
end type

type st_2 from statictext within w_special_instruction
integer x = 37
integer y = 400
integer width = 82
integer height = 76
boolean bringtotop = true
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long backcolor = 12632256
boolean enabled = false
string text = "By:"
boolean focusrectangle = false
end type

type st_3 from statictext within w_special_instruction
integer x = 1582
integer y = 32
integer width = 142
integer height = 76
boolean bringtotop = true
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long backcolor = 12632256
boolean enabled = false
string text = "Date:"
boolean focusrectangle = false
end type

type dw_clock from u_pics_dw within w_special_instruction
integer x = 1787
integer y = 28
integer width = 731
integer height = 92
integer taborder = 10
string dataobject = "d_clock"
boolean vscrollbar = false
boolean livescroll = false
end type

type cb_print from commandbutton within w_special_instruction
integer x = 1737
integer y = 1000
integer width = 247
integer height = 92
integer taborder = 50
boolean bringtotop = true
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "&Print"
end type

event clicked;long print_job,print_ret

print_job = PrintOpen( ) 
print_ret = PrintDataWindow(print_job, dw_pa_si_print) 
print_ret = PrintClose(print_job)
if print_job < 0 then
   MessageBox("Error in PrintClose", Print_job,StopSign!)
   return
end if

end event

type st_4 from statictext within w_special_instruction
integer x = 55
integer y = 40
integer width = 357
integer height = 76
boolean bringtotop = true
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long backcolor = 12632256
boolean enabled = false
string text = "Control Number:"
boolean focusrectangle = false
end type

type sle_conno from singlelineedit within w_special_instruction
integer x = 430
integer y = 32
integer width = 389
integer height = 92
integer taborder = 30
boolean bringtotop = true
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long backcolor = 12632256
boolean autohscroll = false
borderstyle borderstyle = stylelowered!
end type

type sle_title from multilineedit within w_special_instruction
integer x = 576
integer y = 152
integer width = 2025
integer height = 208
integer taborder = 20
boolean bringtotop = true
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long backcolor = 12632256
boolean autovscroll = true
borderstyle borderstyle = stylelowered!
end type

type dw_pa_special_instruction from datawindow within w_special_instruction
boolean visible = false
integer x = 91
integer y = 680
integer width = 2505
integer height = 164
integer taborder = 60
boolean bringtotop = true
string dataobject = "d_pa_special_instruction"
end type

event constructor;dw_pa_special_instruction.SetTransObject(SQLServerTrans)

end event

type dw_pa_si_print from datawindow within w_special_instruction
boolean visible = false
integer x = 1015
integer y = 600
integer width = 494
integer height = 140
integer taborder = 51
boolean bringtotop = true
boolean enabled = false
string dataobject = "d_pa_si_print"
end type

event constructor;dw_pa_si_print.SetTransObject(SQLServerTrans)

end event


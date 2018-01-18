$PBExportHeader$w_tot_anno.srw
forward
global type w_tot_anno from window
end type
type st_3 from statictext within w_tot_anno
end type
type st_position from statictext within w_tot_anno
end type
type st_2 from statictext within w_tot_anno
end type
type st_position4 from statictext within w_tot_anno
end type
type mle_tot_anno from multilineedit within w_tot_anno
end type
type pb_exit from commandbutton within w_tot_anno
end type
end forward

global type w_tot_anno from window
integer x = 530
integer y = 228
integer width = 2770
integer height = 1492
boolean titlebar = true
string title = "View total annotation"
boolean controlmenu = true
windowtype windowtype = response!
long backcolor = 79741120
st_3 st_3
st_position st_position
st_2 st_2
st_position4 st_position4
mle_tot_anno mle_tot_anno
pb_exit pb_exit
end type
global w_tot_anno w_tot_anno

type variables

end variables

forward prototypes
public function boolean wf_validate_user_access (string luserid)
end prototypes

public function boolean wf_validate_user_access (string luserid);string Lusr,ls_message,ls_msgparm[1]

SELECT picsuser.userid
INTO 	:Lusr
FROM 	picsuser 
WHERE picsuser.userid = :Luserid AND
		picsuser.group_  = 'CDS'
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
	RETURN FALSE
ELSE
	// Row found
	RETURN TRUE
END IF
end function

event key;IF KeyDown(KeyEscape!) THEN 
  pb_exit.TriggerEvent(Clicked!)
END IF

end event

on w_tot_anno.create
this.st_3=create st_3
this.st_position=create st_position
this.st_2=create st_2
this.st_position4=create st_position4
this.mle_tot_anno=create mle_tot_anno
this.pb_exit=create pb_exit
this.Control[]={this.st_3,&
this.st_position,&
this.st_2,&
this.st_position4,&
this.mle_tot_anno,&
this.pb_exit}
end on

on w_tot_anno.destroy
destroy(this.st_3)
destroy(this.st_position)
destroy(this.st_2)
destroy(this.st_position4)
destroy(this.mle_tot_anno)
destroy(this.pb_exit)
end on

event open;IF NOT(IsNull(w_notepad.mle_sex.text)) THEN
	mle_tot_anno.text = w_notepad.mle_notepad.text + " " + w_notepad.mle_sex.text
	st_position.text = w_notepad.st_position4.text	
ELSE
	mle_tot_anno.text = w_notepad.mle_notepad.text
	st_position.text = w_notepad.st_position4.text
END IF

end event

type st_3 from statictext within w_tot_anno
integer x = 37
integer y = 1280
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

type st_position from statictext within w_tot_anno
integer x = 731
integer y = 1280
integer width = 219
integer height = 64
integer textsize = -9
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 41943040
boolean enabled = false
alignment alignment = center!
boolean border = true
borderstyle borderstyle = stylelowered!
boolean focusrectangle = false
end type

type st_2 from statictext within w_tot_anno
integer x = 1573
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

type st_position4 from statictext within w_tot_anno
integer x = 2286
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

type mle_tot_anno from multilineedit within w_tot_anno
integer x = 37
integer y = 32
integer width = 2670
integer height = 1216
integer taborder = 10
integer textsize = -12
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 33554432
long backcolor = 1090519039
borderstyle borderstyle = stylelowered!
end type

type pb_exit from commandbutton within w_tot_anno
integer x = 2414
integer y = 1280
integer width = 283
integer height = 108
integer textsize = -10
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "&Exit"
end type

event clicked;close(parent)
end event


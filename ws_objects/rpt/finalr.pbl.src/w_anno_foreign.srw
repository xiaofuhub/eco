$PBExportHeader$w_anno_foreign.srw
forward
global type w_anno_foreign from window
end type
type cb_print from commandbutton within w_anno_foreign
end type
type dw_anno_foreign from u_pics_dw within w_anno_foreign
end type
type pb_update from commandbutton within w_anno_foreign
end type
type pb_exit from commandbutton within w_anno_foreign
end type
end forward

global type w_anno_foreign from window
integer x = 530
integer y = 228
integer width = 1883
integer height = 1436
boolean titlebar = true
string title = "Foreign Annotation"
boolean controlmenu = true
windowtype windowtype = response!
long backcolor = 79741120
cb_print cb_print
dw_anno_foreign dw_anno_foreign
pb_update pb_update
pb_exit pb_exit
end type
global w_anno_foreign w_anno_foreign

type variables

end variables

forward prototypes
public function boolean wf_validate_user_access (string luserid)
end prototypes

public function boolean wf_validate_user_access (string luserid);string Lusr,ls_message,ls_msgparm[1],Lforeign_books

SELECT UPPER(picsuser.foreign_books)
INTO 	:Lforeign_books
FROM 	picsuser 
WHERE picsuser.userid = :Luserid
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
END IF

// Tracker item 2017
IF Lforeign_books = 'Y' THEN
	// user is allowed to update foreign books
	RETURN TRUE
ELSE
	// user is not allowed to update foreign books
	RETURN FALSE
END IF
end function

event open;string original_select,mod_string,where_clause,rc,Lchno
int RowNum

Lchno = Message.StringParm

dw_anno_foreign.SetTransObject( SQLServerTrans )

original_select =	dw_anno_foreign.Describe("DataWindow.Table.Select")

where_clause = " WHERE annotation.chno="+ "~~'" +Lchno+ "~~'"

mod_string = "DataWindow.Table.Select='" + original_select + where_clause + "'"

rc = dw_anno_foreign.Modify(mod_string)
IF rc = "" THEN
	dw_anno_foreign.Retrieve()
	dw_anno_foreign.SetFocus()
	
	// If the user is allowed to update foreign books, enable the update button
	IF wf_validate_user_access(sqlservertrans.userid) THEN
		pb_update.Enabled = TRUE
	ELSE
		pb_update.Enabled = FALSE
	END IF

ELSE
	MessageBox("Status", "Modify Failed" + rc)
END if



end event

event key;IF KeyDown(KeyEscape!) THEN 
  pb_exit.TriggerEvent(Clicked!)
END IF

end event

on w_anno_foreign.create
this.cb_print=create cb_print
this.dw_anno_foreign=create dw_anno_foreign
this.pb_update=create pb_update
this.pb_exit=create pb_exit
this.Control[]={this.cb_print,&
this.dw_anno_foreign,&
this.pb_update,&
this.pb_exit}
end on

on w_anno_foreign.destroy
destroy(this.cb_print)
destroy(this.dw_anno_foreign)
destroy(this.pb_update)
destroy(this.pb_exit)
end on

type cb_print from commandbutton within w_anno_foreign
integer x = 896
integer y = 1184
integer width = 283
integer height = 104
integer taborder = 30
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "&Print"
end type

event clicked;dw_anno_foreign.Triggerevent("pfc_Print")

end event

type dw_anno_foreign from u_pics_dw within w_anno_foreign
integer x = 32
integer y = 28
integer width = 1806
integer height = 1128
integer taborder = 20
string dataobject = "d_anno_foreign"
boolean vscrollbar = false
boolean livescroll = false
end type

type pb_update from commandbutton within w_anno_foreign
integer x = 1221
integer y = 1184
integer width = 283
integer height = 108
integer taborder = 20
integer textsize = -10
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "&Update"
end type

event clicked;int rtn

dw_anno_foreign.AcceptText()

IF dw_anno_foreign.ModifiedCount() > 0 THEN
	rtn = dw_anno_foreign.of_Update(TRUE,TRUE)
	IF f_check_dberror(sqlservertrans,"Annotation") THEN
		IF rtn=1 THEN
			Commit Using SQLServerTrans;
			MessageBox("Update"," Foreign annotation updated.",Information!)
		ELSE
			Rollback Using SqlServerTrans;
			MessageBox("Update"," Foreign annotation update failed.",StopSign!)
			RETURN
		END IF
	END IF
ELSE
	MessageBox("Update"," Nothing to update.",Information!)
END IF
end event

type pb_exit from commandbutton within w_anno_foreign
integer x = 1545
integer y = 1184
integer width = 283
integer height = 108
integer taborder = 30
integer textsize = -10
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "&Exit"
end type

event clicked;close(parent)
end event


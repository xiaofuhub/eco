$PBExportHeader$w_data_dictionary.srw
forward
global type w_data_dictionary from w_response
end type
type dw_data_dictionary_report from u_pics_dw within w_data_dictionary
end type
type cb_report from commandbutton within w_data_dictionary
end type
type cb_exit from commandbutton within w_data_dictionary
end type
type cb_update from commandbutton within w_data_dictionary
end type
type dw_data_dictionary_sum from u_pics_dw within w_data_dictionary
end type
type dw_data_dictionary from u_pics_dw within w_data_dictionary
end type
end forward

global type w_data_dictionary from w_response
integer x = 165
integer y = 452
integer width = 3433
integer height = 1924
string title = "Data Dictionary"
dw_data_dictionary_report dw_data_dictionary_report
cb_report cb_report
cb_exit cb_exit
cb_update cb_update
dw_data_dictionary_sum dw_data_dictionary_sum
dw_data_dictionary dw_data_dictionary
end type
global w_data_dictionary w_data_dictionary

on w_data_dictionary.create
int iCurrent
call super::create
this.dw_data_dictionary_report=create dw_data_dictionary_report
this.cb_report=create cb_report
this.cb_exit=create cb_exit
this.cb_update=create cb_update
this.dw_data_dictionary_sum=create dw_data_dictionary_sum
this.dw_data_dictionary=create dw_data_dictionary
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_data_dictionary_report
this.Control[iCurrent+2]=this.cb_report
this.Control[iCurrent+3]=this.cb_exit
this.Control[iCurrent+4]=this.cb_update
this.Control[iCurrent+5]=this.dw_data_dictionary_sum
this.Control[iCurrent+6]=this.dw_data_dictionary
end on

on w_data_dictionary.destroy
call super::destroy
destroy(this.dw_data_dictionary_report)
destroy(this.cb_report)
destroy(this.cb_exit)
destroy(this.cb_update)
destroy(this.dw_data_dictionary_sum)
destroy(this.dw_data_dictionary)
end on

type dw_data_dictionary_report from u_pics_dw within w_data_dictionary
boolean visible = false
integer x = 571
integer y = 1708
integer width = 151
integer height = 112
integer taborder = 20
string dataobject = "d_data_dictionary_report"
boolean vscrollbar = false
boolean border = false
boolean livescroll = false
end type

event ue_postconstructor;call super::ue_postconstructor;This.of_SetLinkage(TRUE)
This.of_SetTransObject(SQLServerTrans)
this.of_SetRowManager(TRUE)
this.SetTransObject(SQLServerTrans)

end event

event retrievestart;call super::retrievestart;Open(w_pics_retrieve_msg_box)
end event

event retrieveend;call super::retrieveend;close(w_pics_retrieve_msg_box)
end event

type cb_report from commandbutton within w_data_dictionary
integer x = 37
integer y = 1700
integer width = 457
integer height = 112
integer taborder = 30
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "&Print Report"
end type

event clicked;long rtn

rtn = dw_data_dictionary_report.Retrieve()
IF rtn > 0 THEN
 nvo_PowerPrn.of_SetPrinterOrientation(2)
 dw_data_dictionary_report.TriggerEvent("pfc_Print")
END IF

// Reset the default printer setting
f_pics_set_def_prn_setting()

end event

type cb_exit from commandbutton within w_data_dictionary
integer x = 3136
integer y = 1712
integer width = 247
integer height = 108
boolean bringtotop = true
integer textsize = -10
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "E&xit"
end type

event clicked;parent.Event pfc_close()

end event

type cb_update from commandbutton within w_data_dictionary
integer x = 2775
integer y = 1712
integer width = 311
integer height = 108
boolean bringtotop = true
integer textsize = -10
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "&Update"
end type

event clicked;int rc

// Accept the text that was put on the screen.
rc = dw_data_dictionary.AcceptText()

// If there were no changes to the screen, don't try to update the screen.
IF rc=1 THEN
	IF dw_data_dictionary.ModifiedCount() > 0 THEN
		rc = dw_data_dictionary.Event pfc_update(TRUE,FALSE)
		if rc = 1 THEN
			COMMIT USING SqlServerTrans;
			MessageBox("Update","Update Successful.",Information!)
			return 1
		else
			ROLLBACK USING SqlServerTrans;
			dw_data_dictionary.ResetUpdate()
			MessageBox("ERROR","Update failed.",StopSign!)
			RETURN 0
		end if
	END IF
END IF
end event

type dw_data_dictionary_sum from u_pics_dw within w_data_dictionary
integer x = 32
integer y = 28
integer width = 722
integer height = 1660
integer taborder = 20
string dataobject = "d_data_dictionary_sum"
end type

event retrieveend;call super::retrieveend;string colname
colname = dw_data_dictionary_sum.object.colname[1]
dw_data_dictionary.SetTransObject(SQLServerTrans)
dw_data_dictionary.retrieve(colname)
close(w_pics_retrieve_msg_box)
dw_data_dictionary_sum.SetFocus()

end event

event retrievestart;call super::retrievestart;openwithparm(w_pics_retrieve_msg_box,"Retrieving data , Please Wait...")

end event

event ue_postconstructor;call super::ue_postconstructor;This.of_SetLinkage(TRUE)
This.of_SetTransObject(SQLServerTrans)
this.of_SetRowManager(TRUE)
this.SetTransObject(SQLServerTrans)
this.retrieve()
end event

event itemfocuschanged;call super::itemfocuschanged;string colname
colname = this.object.colname[row]
dw_data_dictionary.SetTransObject(SQLServerTrans)
dw_data_dictionary.retrieve(colname)

end event

type dw_data_dictionary from u_pics_dw within w_data_dictionary
integer x = 763
integer y = 28
integer width = 2624
integer height = 1664
integer taborder = 10
string dataobject = "d_data_dictionary"
boolean livescroll = false
end type


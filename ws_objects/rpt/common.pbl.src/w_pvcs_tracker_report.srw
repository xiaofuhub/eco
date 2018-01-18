$PBExportHeader$w_pvcs_tracker_report.srw
forward
global type w_pvcs_tracker_report from w_response
end type
type cb_print from commandbutton within w_pvcs_tracker_report
end type
type sle_1 from singlelineedit within w_pvcs_tracker_report
end type
type st_1 from statictext within w_pvcs_tracker_report
end type
type cb_exit from commandbutton within w_pvcs_tracker_report
end type
type dw_pvcs_tracker_data from u_pics_dw within w_pvcs_tracker_report
end type
type cb_find from commandbutton within w_pvcs_tracker_report
end type
type cb_clear from commandbutton within w_pvcs_tracker_report
end type
end forward

global type w_pvcs_tracker_report from w_response
integer x = 110
integer y = 136
integer width = 3278
integer height = 1564
string title = "PICS ORACLE Problem Report"
cb_print cb_print
sle_1 sle_1
st_1 st_1
cb_exit cb_exit
dw_pvcs_tracker_data dw_pvcs_tracker_data
cb_find cb_find
cb_clear cb_clear
end type
global w_pvcs_tracker_report w_pvcs_tracker_report

on w_pvcs_tracker_report.create
int iCurrent
call super::create
this.cb_print=create cb_print
this.sle_1=create sle_1
this.st_1=create st_1
this.cb_exit=create cb_exit
this.dw_pvcs_tracker_data=create dw_pvcs_tracker_data
this.cb_find=create cb_find
this.cb_clear=create cb_clear
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.cb_print
this.Control[iCurrent+2]=this.sle_1
this.Control[iCurrent+3]=this.st_1
this.Control[iCurrent+4]=this.cb_exit
this.Control[iCurrent+5]=this.dw_pvcs_tracker_data
this.Control[iCurrent+6]=this.cb_find
this.Control[iCurrent+7]=this.cb_clear
end on

on w_pvcs_tracker_report.destroy
call super::destroy
destroy(this.cb_print)
destroy(this.sle_1)
destroy(this.st_1)
destroy(this.cb_exit)
destroy(this.dw_pvcs_tracker_data)
destroy(this.cb_find)
destroy(this.cb_clear)
end on

event open;call super::open;dw_pvcs_tracker_data.Object.DataWindow.QueryMode='Yes'
dw_pvcs_tracker_data.SetFocus()


end event

type cb_print from commandbutton within w_pvcs_tracker_report
integer x = 2089
integer y = 1336
integer width = 251
integer height = 108
boolean bringtotop = true
integer textsize = -10
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "&Print"
end type

event clicked;dw_pvcs_tracker_data.TriggerEvent("pfc_print")

end event

type sle_1 from singlelineedit within w_pvcs_tracker_report
integer x = 626
integer y = 1316
integer width = 329
integer height = 80
integer taborder = 20
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 33554432
borderstyle borderstyle = stylelowered!
end type

type st_1 from statictext within w_pvcs_tracker_report
integer x = 37
integer y = 1320
integer width = 581
integer height = 64
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 33554432
long backcolor = 67108864
string text = "Number of Problems"
boolean focusrectangle = false
end type

type cb_exit from commandbutton within w_pvcs_tracker_report
integer x = 2985
integer y = 1336
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

event clicked;ib_disableclosequery = TRUE
parent.Event pfc_close()

end event

type dw_pvcs_tracker_data from u_pics_dw within w_pvcs_tracker_report
event ue_enterkey pbm_dwnprocessenter
integer x = 32
integer y = 24
integer width = 3200
integer height = 1252
integer taborder = 10
string dataobject = "d_pvcs_tracker_data"
boolean hscrollbar = true
end type

event ue_enterkey;IF (dw_pvcs_tracker_data.GetColumn() <> 1 ) THEN
	Send(Handle(this),256,9,Long(0,0))
	return(1)
END IF

end event

event ue_postconstructor;call super::ue_postconstructor;// set the transaction object.
dw_pvcs_tracker_data.of_SetTransObject(SQLServerTrackerTrans)

end event

type cb_find from commandbutton within w_pvcs_tracker_report
integer x = 2391
integer y = 1336
integer width = 247
integer height = 108
boolean bringtotop = true
integer textsize = -10
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "F&ind"
boolean default = true
end type

event clicked;int ll_rows,rtn
dw_pvcs_tracker_data.AcceptText()
dw_pvcs_tracker_data.Object.DataWindow.QueryMode='No'
ll_rows = dw_pvcs_tracker_data.Retrieve()
IF ll_rows = 0 THEN
   MessageBox("Find Error", "Problem report does not exist. Try another one." ,Information!, OkCancel!)
	cb_clear.TriggerEvent(Clicked!)
ELSE
	sle_1.text = String(ll_rows)
   dw_pvcs_tracker_data.setfocus()
END IF

end event

type cb_clear from commandbutton within w_pvcs_tracker_report
integer x = 2674
integer y = 1336
integer width = 251
integer height = 108
boolean bringtotop = true
integer textsize = -10
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "&Clear"
end type

event clicked;long ll_rows,rtn

dw_pvcs_tracker_data.Reset()

sle_1.Text=""

dw_pvcs_tracker_data.Object.DataWindow.QueryClear='Yes'
dw_pvcs_tracker_data.Object.DataWindow.QueryMode='Yes'


dw_pvcs_tracker_data.SetFocus()

w_pvcs_tracker_report.cb_find.Enabled=TRUE
w_pvcs_tracker_report.cb_find.Default=TRUE
w_pvcs_tracker_report.cb_clear.Enabled=TRUE

end event


$PBExportHeader$w_s2datein.srw
forward
global type w_s2datein from window
end type
type dw_s2in from u_pics_dw within w_s2datein
end type
type cb_cancel from commandbutton within w_s2datein
end type
type cb_ok from commandbutton within w_s2datein
end type
type st_1 from statictext within w_s2datein
end type
end forward

global type w_s2datein from window
integer x = 599
integer y = 300
integer width = 1618
integer height = 1032
boolean titlebar = true
string title = "Stage II Date in"
windowtype windowtype = response!
long backcolor = 79741120
dw_s2in dw_s2in
cb_cancel cb_cancel
cb_ok cb_ok
st_1 st_1
end type
global w_s2datein w_s2datein

type variables
string S2datein
end variables

on w_s2datein.create
this.dw_s2in=create dw_s2in
this.cb_cancel=create cb_cancel
this.cb_ok=create cb_ok
this.st_1=create st_1
this.Control[]={this.dw_s2in,&
this.cb_cancel,&
this.cb_ok,&
this.st_1}
end on

on w_s2datein.destroy
destroy(this.dw_s2in)
destroy(this.cb_cancel)
destroy(this.cb_ok)
destroy(this.st_1)
end on

type dw_s2in from u_pics_dw within w_s2datein
integer x = 526
integer y = 152
integer width = 494
string dataobject = "d_s2in"
end type

event ue_postconstructor;call super::ue_postconstructor;long ll_rows

cb_ok.enabled = FALSE

dw_s2in.SetTransObject( SQLServerTrans)

ll_rows = dw_s2in.Retrieve()

IF ll_rows < 1 THEN 
 	MessageBox("Database Error", "No rows retrieved.",Exclamation!)
 	cb_cancel.TriggerEvent(Clicked!)  // Process CANCEL.
ELSE
	cb_ok.enabled = TRUE
END IF

SetMicroHelp(w_pics_main,"")

end event

event doubleclicked;call super::doubleclicked;cb_ok.TriggerEvent(Clicked!)
end event

event rbuttondown;//
end event

event rbuttonup;//
end event

event retrievestart;call super::retrievestart;OpenWithParm(w_pics_retrieve_msg_box,"Retrieving Stage II date in, Please Wait...")
end event

event retrieveend;call super::retrieveend;close(w_pics_retrieve_msg_box)
end event

type cb_cancel from commandbutton within w_s2datein
integer x = 841
integer y = 748
integer width = 283
integer height = 108
integer taborder = 20
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "Cancel"
end type

event clicked;//dw_s2in.DBCancel()
close(parent)
end event

type cb_ok from commandbutton within w_s2datein
integer x = 379
integer y = 744
integer width = 306
integer height = 108
integer taborder = 10
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "OK"
boolean default = true
end type

event clicked;int ll_rows
ll_rows = dw_s2in.GetRow()

S2datein = string(dw_s2in.object.s2in[ll_rows],'MM/DD/YYYY')

IF IsValid(w_sheet_bcs_stage2) = FALSE THEN 
	OpenSheet(w_sheet_bcs_stage2, w_pics_main, 0, Original!)
ELSE
	w_sheet_bcs_stage2.dw_bcs_stage2.Event ue_postconstructor()
END IF

end event

type st_1 from statictext within w_s2datein
integer x = 201
integer y = 32
integer width = 1129
integer height = 84
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long backcolor = 79741120
boolean enabled = false
string text = "Please select the Stage II Date in :"
alignment alignment = center!
long bordercolor = 8421504
boolean focusrectangle = false
end type


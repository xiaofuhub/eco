$PBExportHeader$w_s2dateout.srw
forward
global type w_s2dateout from window
end type
type dw_s2out from u_pics_dw within w_s2dateout
end type
type cb_cancel from commandbutton within w_s2dateout
end type
type cb_ok from commandbutton within w_s2dateout
end type
type st_1 from statictext within w_s2dateout
end type
end forward

global type w_s2dateout from window
integer x = 599
integer y = 308
integer width = 1605
integer height = 1104
boolean titlebar = true
string title = "Stage II Date out"
windowtype windowtype = response!
long backcolor = 79741120
dw_s2out dw_s2out
cb_cancel cb_cancel
cb_ok cb_ok
st_1 st_1
end type
global w_s2dateout w_s2dateout

type variables
String S2dateout,Catdate
end variables

event key;IF KeyDown(KeyEnter!) THEN 
  cb_ok.TriggerEvent(Clicked!)
ELSEIF KeyDown(KeyEscape!) THEN 
  cb_cancel.TriggerEvent(Clicked!)
END IF


end event

on w_s2dateout.create
this.dw_s2out=create dw_s2out
this.cb_cancel=create cb_cancel
this.cb_ok=create cb_ok
this.st_1=create st_1
this.Control[]={this.dw_s2out,&
this.cb_cancel,&
this.cb_ok,&
this.st_1}
end on

on w_s2dateout.destroy
destroy(this.dw_s2out)
destroy(this.cb_cancel)
destroy(this.cb_ok)
destroy(this.st_1)
end on

type dw_s2out from u_pics_dw within w_s2dateout
integer x = 539
integer y = 140
integer width = 443
string dataobject = "d_s2out"
end type

event doubleclicked;call super::doubleclicked;cb_ok.TriggerEvent(Clicked!)
end event

event ue_postconstructor;call super::ue_postconstructor;long ll_rows

w_s2dateout.cb_ok.Enabled = FALSE

dw_s2out.SetTransObject( SQLServerTrans )

ll_rows = dw_s2out.Retrieve()

if ll_rows < 1 THEN 
 MessageBox("Database Error", "No rows retrieved.",Exclamation!)
 cb_cancel.TriggerEvent(Clicked!)  // Process CANCEL.
else
 dw_s2out.SetFocus()
end if

w_s2dateout.cb_ok.Enabled = TRUE

end event

event rbuttondown;//
end event

event rbuttonup;//
end event

event retrievestart;call super::retrievestart;OpenWithParm(w_pics_retrieve_msg_box,"Retrieving Stage II date out, Please Wait...")
end event

event retrieveend;call super::retrieveend;close(w_pics_retrieve_msg_box)
end event

type cb_cancel from commandbutton within w_s2dateout
integer x = 846
integer y = 728
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

on clicked;close(parent)
end on

type cb_ok from commandbutton within w_s2dateout
integer x = 384
integer y = 724
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
SetMicroHelp(w_pics_main,"")

ll_rows = dw_s2out.GetRow()

S2dateout = string(dw_s2out.object.s2out[ll_rows])

Catdate = w_stage2_dateout.Catdate

OpenSheet(w_sheet_catalog, w_pics_main, 0, Original!)

end event

type st_1 from statictext within w_s2dateout
integer x = 101
integer y = 32
integer width = 1385
integer height = 84
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long backcolor = 79741120
boolean enabled = false
string text = "Please select the Stage II Date out :"
alignment alignment = center!
long bordercolor = 8421504
boolean focusrectangle = false
end type


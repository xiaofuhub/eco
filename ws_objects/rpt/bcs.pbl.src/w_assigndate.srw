$PBExportHeader$w_assigndate.srw
forward
global type w_assigndate from window
end type
type dw_assigndt from u_pics_dw within w_assigndate
end type
type cb_cancel from commandbutton within w_assigndate
end type
type cb_ok from commandbutton within w_assigndate
end type
type st_1 from statictext within w_assigndate
end type
end forward

global type w_assigndate from window
integer x = 640
integer y = 360
integer width = 1618
integer height = 972
boolean titlebar = true
string title = "Assignment Date"
windowtype windowtype = response!
long backcolor = 79741120
event ue_postevent ( )
dw_assigndt dw_assigndt
cb_cancel cb_cancel
cb_ok cb_ok
st_1 st_1
end type
global w_assigndate w_assigndate

type variables
date Frdate
end variables

event key;IF KeyDown(KeyEnter!) THEN 
  cb_ok.TriggerEvent(Clicked!)
ELSEIF KeyDown(KeyEscape!) THEN 
  cb_cancel.TriggerEvent(Clicked!)
END IF


end event

on w_assigndate.create
this.dw_assigndt=create dw_assigndt
this.cb_cancel=create cb_cancel
this.cb_ok=create cb_ok
this.st_1=create st_1
this.Control[]={this.dw_assigndt,&
this.cb_cancel,&
this.cb_ok,&
this.st_1}
end on

on w_assigndate.destroy
destroy(this.dw_assigndt)
destroy(this.cb_cancel)
destroy(this.cb_ok)
destroy(this.st_1)
end on

type dw_assigndt from u_pics_dw within w_assigndate
integer x = 526
integer y = 140
integer width = 466
string dataobject = "d_pa_assigndt"
end type

event ue_postconstructor;call super::ue_postconstructor;long ll_rows
string Lcntr

cb_ok.enabled = FALSE

dw_assigndt.SetTransObject( SQLServerTrans)

IF IsValid(w_pa_assigning_books) THEN
	Lcntr = w_pa_assigning_books.dw_pa_ancntr_data.object.cntr[1]
	
	ll_rows = dw_assigndt.Retrieve(Lcntr)
	
	IF ll_rows < 1 THEN 
		MessageBox("Database Error", "No rows retrieved.",Exclamation!)
		cb_cancel.TriggerEvent(Clicked!)  // Process CANCEL.
	ELSE
		cb_ok.enabled = TRUE
	END IF
	
	SetMicroHelp(w_pics_main,"")

ELSE
	MessageBox("Error","Error accessing the book assignment screen.")
	RETURN
END IF


end event

event doubleclicked;call super::doubleclicked;cb_ok.TriggerEvent(Clicked!)
end event

event rbuttonup;//
end event

event rbuttondown;//
end event

type cb_cancel from commandbutton within w_assigndate
integer x = 846
integer y = 720
integer width = 297
integer height = 108
integer taborder = 20
integer textsize = -10
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "&Cancel"
end type

event clicked;//dw_assigndt.DBCancel()
close(parent)
end event

type cb_ok from commandbutton within w_assigndate
integer x = 375
integer y = 720
integer width = 306
integer height = 108
integer taborder = 10
integer textsize = -10
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "&OK"
boolean default = true
end type

event clicked;int ll_row
string assigndate
ll_row = dw_assigndt.GetRow()
assigndate = string(dw_assigndt.object.assigndt[ll_row],'MM/DD/YYYY')
CloseWithReturn(w_assigndate,assigndate)

end event

type st_1 from statictext within w_assigndate
integer x = 146
integer y = 32
integer width = 1298
integer height = 84
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long backcolor = 79741120
boolean enabled = false
string text = "Please select the Assignment Date :"
alignment alignment = center!
long bordercolor = 79741120
boolean focusrectangle = false
end type


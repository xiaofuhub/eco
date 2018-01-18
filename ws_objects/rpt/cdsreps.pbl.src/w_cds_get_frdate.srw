$PBExportHeader$w_cds_get_frdate.srw
forward
global type w_cds_get_frdate from window
end type
type dw_fr from u_pics_dw within w_cds_get_frdate
end type
type cb_cancel from commandbutton within w_cds_get_frdate
end type
type cb_ok from commandbutton within w_cds_get_frdate
end type
type st_1 from statictext within w_cds_get_frdate
end type
end forward

global type w_cds_get_frdate from window
integer x = 640
integer y = 360
integer width = 1614
integer height = 972
boolean titlebar = true
string title = "Final Review Date"
windowtype windowtype = response!
long backcolor = 79741120
event ue_postevent ( )
dw_fr dw_fr
cb_cancel cb_cancel
cb_ok cb_ok
st_1 st_1
end type
global w_cds_get_frdate w_cds_get_frdate

type variables
date Frdate
end variables

event key;IF KeyDown(KeyEnter!) THEN 
  cb_ok.TriggerEvent(Clicked!)
ELSEIF KeyDown(KeyEscape!) THEN 
  cb_cancel.TriggerEvent(Clicked!)
END IF


end event

on w_cds_get_frdate.create
this.dw_fr=create dw_fr
this.cb_cancel=create cb_cancel
this.cb_ok=create cb_ok
this.st_1=create st_1
this.Control[]={this.dw_fr,&
this.cb_cancel,&
this.cb_ok,&
this.st_1}
end on

on w_cds_get_frdate.destroy
destroy(this.dw_fr)
destroy(this.cb_cancel)
destroy(this.cb_ok)
destroy(this.st_1)
end on

type dw_fr from u_pics_dw within w_cds_get_frdate
integer x = 526
integer y = 140
integer width = 462
integer taborder = 0
string dataobject = "d_frdate"
boolean vscrollbar = false
boolean livescroll = false
boolean ib_isupdateable = false
boolean ib_rmbmenu = false
boolean ib_rmbfocuschange = false
string is_updatesallowed = ""
end type

event ue_postconstructor;call super::ue_postconstructor;long ll_rows
cb_ok.enabled = FALSE

dw_fr.SetTransObject( SQLServerTrans)

ll_rows = dw_fr.Retrieve()

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

event rbuttonup;//
end event

event rbuttondown;//
end event

event retrievestart;call super::retrievestart;OpenWithParm(w_pics_retrieve_msg_box,"Retrieving Final Review Dates, Please Wait...")
end event

event retrieveend;call super::retrieveend;close(w_pics_retrieve_msg_box)
end event

type cb_cancel from commandbutton within w_cds_get_frdate
integer x = 846
integer y = 720
integer width = 293
integer height = 108
integer taborder = 20
integer textsize = -10
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "&Cancel"
end type

event clicked;//dw_fr.DBCancel()
close(parent)
end event

type cb_ok from commandbutton within w_cds_get_frdate
integer x = 375
integer y = 720
integer width = 302
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

event clicked;datetime frdate_dt

Frdate_dt = dw_fr.GetItemDatetime(dw_fr.GetRow(),"fr")
frdate=date(frdate_dt)
OpenWithParm(w_cds_reports,string(Frdate,'mm/dd/yyyy'))
CloseWithReturn(w_cds_get_frdate,string(Frdate,'mm/dd/yyyy'))

end event

type st_1 from statictext within w_cds_get_frdate
integer x = 101
integer y = 32
integer width = 1381
integer height = 84
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long backcolor = 79741120
boolean enabled = false
string text = "Please select the Final Review Date :"
alignment alignment = center!
long bordercolor = 79741120
boolean focusrectangle = false
end type


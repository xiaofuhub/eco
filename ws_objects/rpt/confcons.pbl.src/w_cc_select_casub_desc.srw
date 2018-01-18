$PBExportHeader$w_cc_select_casub_desc.srw
forward
global type w_cc_select_casub_desc from w_response
end type
type dw_select_casub from u_pics_dw within w_cc_select_casub_desc
end type
type st_1 from statictext within w_cc_select_casub_desc
end type
type cb_cancel from commandbutton within w_cc_select_casub_desc
end type
type cb_ok from commandbutton within w_cc_select_casub_desc
end type
end forward

global type w_cc_select_casub_desc from w_response
integer x = 905
integer y = 620
integer width = 1573
integer height = 1092
string title = "Select Casub Description"
dw_select_casub dw_select_casub
st_1 st_1
cb_cancel cb_cancel
cb_ok cb_ok
end type
global w_cc_select_casub_desc w_cc_select_casub_desc

type variables
long Lbkseq
string Lcasub_code
end variables

on w_cc_select_casub_desc.create
int iCurrent
call super::create
this.dw_select_casub=create dw_select_casub
this.st_1=create st_1
this.cb_cancel=create cb_cancel
this.cb_ok=create cb_ok
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_select_casub
this.Control[iCurrent+2]=this.st_1
this.Control[iCurrent+3]=this.cb_cancel
this.Control[iCurrent+4]=this.cb_ok
end on

on w_cc_select_casub_desc.destroy
call super::destroy
destroy(this.dw_select_casub)
destroy(this.st_1)
destroy(this.cb_cancel)
destroy(this.cb_ok)
end on

event open;call super::open;Lcasub_code = Message.StringParm
dw_select_casub.Retrieve(Lcasub_code)

end event

type dw_select_casub from u_pics_dw within w_cc_select_casub_desc
integer x = 50
integer y = 292
integer width = 1454
integer height = 552
integer taborder = 10
string dataobject = "dddw_casub_code_with_arg"
end type

event constructor;call super::constructor;SetTransObject(SqlServerTrans)


end event

event doubleclicked;call super::doubleclicked;string Lcasub_desc
Lcasub_desc = dw_select_casub.object.casubj_desc[row] 
CloseWithReturn(Parent,Lcasub_desc)

end event

type st_1 from statictext within w_cc_select_casub_desc
integer x = 41
integer y = 32
integer width = 1481
integer height = 232
boolean bringtotop = true
integer textsize = -10
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
boolean enabled = false
string text = "Please select the casub description, which will be added to the annotation properties. Click on OK button, or doubleclick on the selected description."
boolean focusrectangle = false
end type

type cb_cancel from commandbutton within w_cc_select_casub_desc
integer x = 1262
integer y = 872
integer width = 238
integer height = 80
integer taborder = 20
boolean bringtotop = true
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "&Cancel"
end type

event clicked;close(parent)
end event

type cb_ok from commandbutton within w_cc_select_casub_desc
integer x = 997
integer y = 872
integer width = 210
integer height = 80
integer taborder = 3
boolean bringtotop = true
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "&Ok"
boolean default = true
end type

event clicked;string Lcasub_desc
Lcasub_desc = dw_select_casub.object.casubj_desc[dw_select_casub.Getrow()] 
CloseWithReturn(Parent,Lcasub_desc)
end event


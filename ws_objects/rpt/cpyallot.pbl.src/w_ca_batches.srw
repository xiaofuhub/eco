$PBExportHeader$w_ca_batches.srw
forward
global type w_ca_batches from w_main
end type
type dw_batch from u_pics_dw within w_ca_batches
end type
type cb_cancel from commandbutton within w_ca_batches
end type
end forward

global type w_ca_batches from w_main
integer width = 2953
integer height = 1956
string title = "Create Delivery Verification File"
event ue_create_file ( )
event ue_cancel ( )
event ue_cardfile ( )
event ue_enterkey pbm_dwnprocessenter
dw_batch dw_batch
cb_cancel cb_cancel
end type
global w_ca_batches w_ca_batches

event ue_cancel;call super::ue_cancel;close(this)
end event

event ue_enterkey;call super::ue_enterkey;Send(Handle(this),256,9,Long(0,0))
return(1)
end event

on w_ca_batches.create
int iCurrent
call super::create
this.dw_batch=create dw_batch
this.cb_cancel=create cb_cancel
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_batch
this.Control[iCurrent+2]=this.cb_cancel
end on

on w_ca_batches.destroy
call super::destroy
destroy(this.dw_batch)
destroy(this.cb_cancel)
end on

type dw_batch from u_pics_dw within w_ca_batches
integer x = 50
integer y = 28
integer width = 2830
integer height = 1684
integer taborder = 30
string title = "Batches"
string dataobject = "d_batch"
end type

type cb_cancel from commandbutton within w_ca_batches
integer x = 2629
integer y = 1732
integer width = 247
integer height = 108
integer taborder = 40
boolean bringtotop = true
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "&Cancel"
boolean cancel = true
end type

event clicked;Parent.triggerevent("ue_cancel")
end event


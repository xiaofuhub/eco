$PBExportHeader$w_magazine_printer_list.srw
forward
global type w_magazine_printer_list from w_response
end type
type st_1 from u_st within w_magazine_printer_list
end type
type lb_printer from listbox within w_magazine_printer_list
end type
type cb_ok from commandbutton within w_magazine_printer_list
end type
end forward

global type w_magazine_printer_list from w_response
integer x = 1134
integer y = 804
integer width = 782
integer height = 748
string title = "Server Printer List"
st_1 st_1
lb_printer lb_printer
cb_ok cb_ok
end type
global w_magazine_printer_list w_magazine_printer_list

on w_magazine_printer_list.create
int iCurrent
call super::create
this.st_1=create st_1
this.lb_printer=create lb_printer
this.cb_ok=create cb_ok
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_1
this.Control[iCurrent+2]=this.lb_printer
this.Control[iCurrent+3]=this.cb_ok
end on

on w_magazine_printer_list.destroy
call super::destroy
destroy(this.st_1)
destroy(this.lb_printer)
destroy(this.cb_ok)
end on

type st_1 from u_st within w_magazine_printer_list
integer x = 37
integer y = 32
integer width = 709
integer textsize = -10
string text = "Please choose a printer"
alignment alignment = center!
end type

type lb_printer from listbox within w_magazine_printer_list
integer x = 128
integer y = 136
integer width = 494
integer height = 284
integer taborder = 1
boolean bringtotop = true
integer textsize = -10
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long backcolor = 16777215
boolean vscrollbar = true
string item[] = {"pcsnet"}
borderstyle borderstyle = stylelowered!
end type

type cb_ok from commandbutton within w_magazine_printer_list
integer x = 256
integer y = 476
integer width = 247
integer height = 108
integer taborder = 2
boolean bringtotop = true
integer textsize = -10
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "&OK"
end type

event clicked;string lp_name

lp_name = lb_printer.SelectedItem( )
IF lp_name <> "" THEN
	CloseWithReturn(w_magazine_printer_list,lp_name)	
END IF

end event


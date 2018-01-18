$PBExportHeader$w_view_invoices.srw
forward
global type w_view_invoices from w_response
end type
type st_2 from statictext within w_view_invoices
end type
type st_1 from statictext within w_view_invoices
end type
type dw_invoices_mag from u_dw within w_view_invoices
end type
type dw_invoices_book from u_dw within w_view_invoices
end type
type cb_exit from u_cb within w_view_invoices
end type
end forward

global type w_view_invoices from w_response
integer x = 681
integer y = 268
integer width = 3657
integer height = 1572
string title = "Invoice Information"
boolean minbox = true
boolean maxbox = true
boolean resizable = true
windowtype windowtype = main!
windowstate windowstate = maximized!
st_2 st_2
st_1 st_1
dw_invoices_mag dw_invoices_mag
dw_invoices_book dw_invoices_book
cb_exit cb_exit
end type
global w_view_invoices w_view_invoices

on w_view_invoices.create
int iCurrent
call super::create
this.st_2=create st_2
this.st_1=create st_1
this.dw_invoices_mag=create dw_invoices_mag
this.dw_invoices_book=create dw_invoices_book
this.cb_exit=create cb_exit
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_2
this.Control[iCurrent+2]=this.st_1
this.Control[iCurrent+3]=this.dw_invoices_mag
this.Control[iCurrent+4]=this.dw_invoices_book
this.Control[iCurrent+5]=this.cb_exit
end on

on w_view_invoices.destroy
call super::destroy
destroy(this.st_2)
destroy(this.st_1)
destroy(this.dw_invoices_mag)
destroy(this.dw_invoices_book)
destroy(this.cb_exit)
end on

event open;call super::open;String ls_invno
m_pics_main.m_file.m_print.Enabled 			=	TRUE
m_pics_main.m_file.m_pagesetup.Enabled		=	TRUE
m_pics_main.m_file.m_printimmediate.Enabled	=	TRUE
ib_disableclosequery=true

IF IsNull(Message.StringParm)=FALSE AND Message.StringParm<>"" THEN
	ls_invno = Message.StringParm 
	dw_invoices_book.SetTransObject(SqlServerTrans)
	dw_invoices_mag.SetTransObject(SqlServerTrans)
	dw_invoices_book.Retrieve(ls_invno)
	dw_invoices_mag.Retrieve(ls_invno)
	//Parent.Event pfc_close()
	close(w_mm_selection_list)

END IF

end event

event pfc_preopen;call super::pfc_preopen;this.of_SetPreference(TRUE)
this.inv_preference.of_SetToolBars(TRUE)
this.inv_preference.of_SetWindow(TRUE)
this.of_SetResize(TRUE)
this.inv_resize.of_SetOrigSize(this.workspacewidth(),this.workspaceheight())
inv_resize.of_Register(dw_invoices_book, "Scale")
inv_resize.of_Register(dw_invoices_mag, "Scale")
inv_resize.of_Register(cb_exit, "Scale")
inv_resize.of_Register(st_1, "Scale")
inv_resize.of_Register(st_2, "Scale")

end event

type st_2 from statictext within w_view_invoices
integer x = 37
integer y = 608
integer width = 695
integer height = 64
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 33554432
long backcolor = 67108864
string text = "Magazine Invoices"
boolean focusrectangle = false
end type

type st_1 from statictext within w_view_invoices
integer x = 37
integer width = 695
integer height = 64
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 33554432
long backcolor = 67108864
string text = "Books Invoices"
boolean focusrectangle = false
end type

type dw_invoices_mag from u_dw within w_view_invoices
event ue_enter_to_tab pbm_dwnprocessenter
integer y = 704
integer width = 3547
integer height = 512
integer taborder = 20
string dataobject = "d_magazine_invoice"
end type

type dw_invoices_book from u_dw within w_view_invoices
event ue_enter_to_tab pbm_dwnprocessenter
integer y = 96
integer width = 3547
integer height = 416
integer taborder = 10
string dataobject = "d_book_invoice"
end type

type cb_exit from u_cb within w_view_invoices
integer x = 3182
integer y = 1280
integer taborder = 0
integer textsize = -10
string text = "E&xit"
end type

event clicked;call super::clicked;Parent.Event pfc_close()
end event


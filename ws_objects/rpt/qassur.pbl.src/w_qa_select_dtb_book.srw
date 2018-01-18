$PBExportHeader$w_qa_select_dtb_book.srw
forward
global type w_qa_select_dtb_book from w_response
end type
type dw_qa_dtb_book from u_pics_dw within w_qa_select_dtb_book
end type
type cb_ok from u_cb within w_qa_select_dtb_book
end type
type st_1 from statictext within w_qa_select_dtb_book
end type
type cb_cancel from u_cb within w_qa_select_dtb_book
end type
end forward

global type w_qa_select_dtb_book from w_response
integer x = 613
integer y = 348
integer width = 1472
integer height = 524
string title = "Select DTB book"
boolean center = true
boolean ib_disableclosequery = true
dw_qa_dtb_book dw_qa_dtb_book
cb_ok cb_ok
st_1 st_1
cb_cancel cb_cancel
end type
global w_qa_select_dtb_book w_qa_select_dtb_book

type variables
Long ll_bkseq
end variables

on w_qa_select_dtb_book.create
int iCurrent
call super::create
this.dw_qa_dtb_book=create dw_qa_dtb_book
this.cb_ok=create cb_ok
this.st_1=create st_1
this.cb_cancel=create cb_cancel
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_qa_dtb_book
this.Control[iCurrent+2]=this.cb_ok
this.Control[iCurrent+3]=this.st_1
this.Control[iCurrent+4]=this.cb_cancel
end on

on w_qa_select_dtb_book.destroy
call super::destroy
destroy(this.dw_qa_dtb_book)
destroy(this.cb_ok)
destroy(this.st_1)
destroy(this.cb_cancel)
end on

event open;call super::open;dw_qa_dtb_book.SetFocus()
end event

type dw_qa_dtb_book from u_pics_dw within w_qa_select_dtb_book
integer x = 512
integer y = 128
integer width = 329
integer height = 128
integer taborder = 10
string dataobject = "d_qa_dtb_book"
boolean vscrollbar = false
boolean livescroll = false
end type

event constructor;call super::constructor;this.of_SetDropDownSearch(TRUE)
this.SetTransObject(sqlservertrans)

IF NOT this.inv_dropdownsearch.of_IsRegistered ("bkseq") THEN
    this.inv_dropdownsearch.of_Register("bkseq")
END IF
this.retrieve()

end event

event editchanged;call super::editchanged;inv_dropdownsearch.Event pfc_EditChanged(row, dwo, data)
ll_bkseq = long(data)



end event

event itemfocuschanged;call super::itemfocuschanged;inv_dropdownsearch.Event pfc_ItemFocusChanged(row, dwo)



end event

type cb_ok from u_cb within w_qa_select_dtb_book
integer x = 183
integer y = 288
integer taborder = 0
string text = "&OK"
boolean default = true
end type

event clicked;call super::clicked;
//messageBox("bkseq",string(ll_bkseq))

//ib_disableclosequery = TRUE

IF IsNull(ll_bkseq) OR ll_bkseq = 0 THEN
	ll_bkseq = dw_qa_dtb_book.object.bkseq[dw_qa_dtb_book.GetRow()]
//	messageBox("bkseq",string(ll_bkseq))
END IF
	

CloseWithReturn(parent,string(ll_bkseq))

end event

type st_1 from statictext within w_qa_select_dtb_book
integer x = 59
integer y = 32
integer width = 1330
integer height = 68
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long backcolor = 67108864
boolean enabled = false
string text = "Please select a book number"
alignment alignment = center!
boolean focusrectangle = false
end type

type cb_cancel from u_cb within w_qa_select_dtb_book
integer x = 841
integer y = 288
integer taborder = 0
boolean bringtotop = true
string text = "&Cancel"
end type

event clicked;call super::clicked;Close(parent)
end event


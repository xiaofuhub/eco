$PBExportHeader$w_qa_email_results.srw
forward
global type w_qa_email_results from w_response
end type
type cb_clear from u_cb within w_qa_email_results
end type
type dw_qa_dtb_book from u_pics_dw within w_qa_email_results
end type
type cb_ok from u_cb within w_qa_email_results
end type
type cb_cancel from u_cb within w_qa_email_results
end type
end forward

global type w_qa_email_results from w_response
integer x = 613
integer y = 348
integer width = 3438
integer height = 2052
string title = "Email Search"
boolean center = true
boolean ib_disableclosequery = true
cb_clear cb_clear
dw_qa_dtb_book dw_qa_dtb_book
cb_ok cb_ok
cb_cancel cb_cancel
end type
global w_qa_email_results w_qa_email_results

type variables
Long ll_bkseq
end variables

on w_qa_email_results.create
int iCurrent
call super::create
this.cb_clear=create cb_clear
this.dw_qa_dtb_book=create dw_qa_dtb_book
this.cb_ok=create cb_ok
this.cb_cancel=create cb_cancel
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.cb_clear
this.Control[iCurrent+2]=this.dw_qa_dtb_book
this.Control[iCurrent+3]=this.cb_ok
this.Control[iCurrent+4]=this.cb_cancel
end on

on w_qa_email_results.destroy
call super::destroy
destroy(this.cb_clear)
destroy(this.dw_qa_dtb_book)
destroy(this.cb_ok)
destroy(this.cb_cancel)
end on

event open;call super::open;dw_qa_dtb_book.SetFocus()
end event

type cb_clear from u_cb within w_qa_email_results
integer x = 2354
integer y = 1788
integer width = 448
integer taborder = 20
boolean bringtotop = true
string text = "Clear"
end type

type dw_qa_dtb_book from u_pics_dw within w_qa_email_results
integer x = 41
integer y = 36
integer width = 3291
integer height = 1708
integer taborder = 10
string dataobject = "d_qa_email_results"
boolean vscrollbar = false
boolean livescroll = false
borderstyle borderstyle = stylebox!
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

type cb_ok from u_cb within w_qa_email_results
integer x = 1824
integer y = 1788
integer width = 448
integer taborder = 0
string text = "Search for &Emails"
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

type cb_cancel from u_cb within w_qa_email_results
integer x = 2885
integer y = 1788
integer width = 448
integer taborder = 0
boolean bringtotop = true
string text = "Exit"
end type

event clicked;call super::clicked;Close(parent)
end event


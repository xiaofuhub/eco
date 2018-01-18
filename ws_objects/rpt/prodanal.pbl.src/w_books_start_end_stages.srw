$PBExportHeader$w_books_start_end_stages.srw
forward
global type w_books_start_end_stages from w_response
end type
type dw_books from u_pics_dw within w_books_start_end_stages
end type
type cb_exit from u_cb within w_books_start_end_stages
end type
type cb_update from u_cb within w_books_start_end_stages
end type
end forward

global type w_books_start_end_stages from w_response
integer x = 997
integer y = 488
integer width = 1586
integer height = 1056
string title = "Update Schedule Start and End Date"
dw_books dw_books
cb_exit cb_exit
cb_update cb_update
end type
global w_books_start_end_stages w_books_start_end_stages

type variables
long lbkseq
string lbkmed,lrecagcy
Datawindowchild ldwc_narr

end variables

on w_books_start_end_stages.create
int iCurrent
call super::create
this.dw_books=create dw_books
this.cb_exit=create cb_exit
this.cb_update=create cb_update
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_books
this.Control[iCurrent+2]=this.cb_exit
this.Control[iCurrent+3]=this.cb_update
end on

on w_books_start_end_stages.destroy
call super::destroy
destroy(this.dw_books)
destroy(this.cb_exit)
destroy(this.cb_update)
end on

event pfc_postopen;call super::pfc_postopen;int rtn
string ls_bkseq,ls_cntr
long ll_bkseq
str_date lstr_date


IF (IsNull(Message.PowerObjectParm)=FALSE) THEN
	lstr_date = message.powerObjectParm
	ls_bkseq = lstr_date.ls_bkseq
	ls_cntr = lstr_date.ls_cntr
	ll_bkseq = long(ls_bkseq)
	
	dw_books.SetTransObject(SQLServerTrans)
	rtn = dw_books.Retrieve(ll_bkseq,ls_cntr)
	if rtn > 0 then
		w_books_start_end_stages.Title = "Sched. Start and End dates for book no: "+ls_bkseq
		dw_books.SetFocus()
	else
		MessageBox("Warning","There are no production for book number "+"~""+ls_bkseq+"~""+" exist.",information!)
	end if
END IF


end event

type dw_books from u_pics_dw within w_books_start_end_stages
integer x = 23
integer y = 12
integer width = 1536
integer height = 812
integer taborder = 30
string dataobject = "d_books_start_end_stages"
boolean livescroll = false
end type

event ue_postconstructor;call super::ue_postconstructor;this.SetTransObject(sqlservertrans) 


end event

event pfc_addrow;return 0
end event

event pfc_deleterow;call super::pfc_deleterow;return 0
end event

event pfc_insertrow;call super::pfc_insertrow;return 0
end event

type cb_exit from u_cb within w_books_start_end_stages
integer x = 1257
integer y = 848
integer width = 274
integer taborder = 40
string text = "Ex&it"
end type

event clicked;call super::clicked;close(parent)

end event

type cb_update from u_cb within w_books_start_end_stages
integer x = 933
integer y = 848
integer width = 270
integer taborder = 20
boolean bringtotop = true
string text = "&Update"
end type

event clicked;call super::clicked;int rc
rc = dw_books.Event pfc_update(TRUE,TRUE)
if rc = 1 THEN
	MessageBox("UPDATE","Update Successful.",Information!)
	COMMIT USING SQLServerTrans;
else
	MessageBox("ERROR","Update failed.",StopSign!)
	ROLLBACK USING SQLServerTrans;
end if

end event


$PBExportHeader$w_qa_add_narr_for_book.srw
forward
global type w_qa_add_narr_for_book from w_response
end type
type dw_narrtbl from u_pics_dw within w_qa_add_narr_for_book
end type
type cb_exit from u_cb within w_qa_add_narr_for_book
end type
type cb_update from u_cb within w_qa_add_narr_for_book
end type
type cb_add from u_cb within w_qa_add_narr_for_book
end type
type cb_del from u_cb within w_qa_add_narr_for_book
end type
end forward

global type w_qa_add_narr_for_book from w_response
integer x = 997
integer y = 488
integer width = 1522
integer height = 1176
string title = "Add/Update Narrator for a book"
dw_narrtbl dw_narrtbl
cb_exit cb_exit
cb_update cb_update
cb_add cb_add
cb_del cb_del
end type
global w_qa_add_narr_for_book w_qa_add_narr_for_book

type variables
long lbkseq
string lbkmed,lrecagcy
Datawindowchild ldwc_narr

end variables

on w_qa_add_narr_for_book.create
int iCurrent
call super::create
this.dw_narrtbl=create dw_narrtbl
this.cb_exit=create cb_exit
this.cb_update=create cb_update
this.cb_add=create cb_add
this.cb_del=create cb_del
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_narrtbl
this.Control[iCurrent+2]=this.cb_exit
this.Control[iCurrent+3]=this.cb_update
this.Control[iCurrent+4]=this.cb_add
this.Control[iCurrent+5]=this.cb_del
end on

on w_qa_add_narr_for_book.destroy
call super::destroy
destroy(this.dw_narrtbl)
destroy(this.cb_exit)
destroy(this.cb_update)
destroy(this.cb_add)
destroy(this.cb_del)
end on

event pfc_postopen();call super::pfc_postopen;int rtn
str_qa_add_narr lstr_qa_add_narr

lstr_qa_add_narr = Message.PowerObjectParm
lbkseq = lstr_qa_add_narr.bkseq
lbkmed = lstr_qa_add_narr.bkmed
lrecagcy = lstr_qa_add_narr.recagcy

dw_narrtbl.SetTransObject(SQLServerTrans)
rtn = dw_narrtbl.Retrieve(lbkseq,lbkmed)
if rtn > 0 then
	dw_narrtbl.SetFocus()
else
	rtn = MessageBox("Warning","There are no narrator for book number "+"~""+string(lbkseq)+"~""+" exist, would you like to add one?",Question!,YesNoCancel!,1)
	if rtn = 1 then
		cb_add.TriggerEvent(Clicked!)
	end if
end if


end event

type dw_narrtbl from u_pics_dw within w_qa_add_narr_for_book
integer x = 23
integer y = 12
integer width = 1481
integer height = 812
integer taborder = 30
string dataobject = "d_qa_add_narr_for_book"
boolean livescroll = false
end type

event ue_postconstructor();call super::ue_postconstructor;this.SetTransObject(sqlservertrans) 
this.of_SetDropDownSearch(TRUE)
this.inv_dropdownsearch.of_AddColumn("narr")
this.inv_dropdownsearch.of_AddColumn("narrfn")
this.GetChild ("narr", ldwc_narr)
end event

event type long pfc_addrow();long	ll_rc

// Insert row
if IsValid (inv_rowmanager) then
	ll_rc = inv_rowmanager.event pfc_addrow ()
else
	ll_rc = this.InsertRow (0) 
end if

// Notify the Linkage Service that a new row has been added.
IF IsValid ( inv_Linkage ) THEN 
	inv_Linkage.Event pfc_InsertRow (ll_rc) 
END IF 

this.object.bkseq[ll_rc] = lbkseq
this.object.bkmed[ll_rc] = lbkmed
this.object.recagcy[ll_rc] = lrecagcy

this.ScrollToRow(ll_rc)
this.SetColumn(1)

return ll_rc
end event

event itemchanged;call super::itemchanged;string llrecagcy
IF dwo.Name = "narr" THEN
	IF data<>"" THEN
		data=TRIM(data)
		dw_narrtbl.object.narrfn[row]=TRIM(ldwc_narr.GetItemString(ldwc_narr.GetRow(),"narrfn"))
	ELSE
		dw_narrtbl.object.narrfn[row]=""	
	END IF
	llrecagcy = TRIM(ldwc_narr.GetItemString(ldwc_narr.GetRow(),"recagcy"))
	dw_narrtbl.SetItem(row, "recagcy", llrecagcy )

	//11/17/2008 Set audit columns if changed.
//	dw_narrtbl.object.modified_date[ROW] = today()
//	dw_narrtbl.object.modified_by[ROW] = gnv_app.of_getuserid()

END IF

//11/17/2008 Set audit columns if changed.
//IF dwo.name = "narrfn" THEN
//	dw_narrtbl.object.modified_date[ROW] = today()
//	dw_narrtbl.object.modified_by[ROW] = gnv_app.of_getuserid()
//END IF
end event

event pfc_preupdate;call super::pfc_preupdate;
//12/4/2008
INT LI_LOOP
long ll_rc
dwItemStatus l_status
	
ll_rc = this.rowcount()
dw_narrtbl.accepttext()
//11/17/2008 Set audit columns if changed.
FOR li_loop = 1 TO ll_rc
	l_status = this.GetItemStatus(li_loop,0, Primary!)
	IF l_status = datamodified! THEN
		dw_narrtbl.object.modified_date[li_loop] = today()
		dw_narrtbl.object.modified_by[li_loop] = gnv_app.of_getuserid()
	END IF
NEXT
RETURN 1
end event

type cb_exit from u_cb within w_qa_add_narr_for_book
integer x = 1198
integer y = 860
integer width = 274
integer taborder = 40
string text = "Ex&it"
end type

event clicked;call super::clicked;close(parent)

end event

type cb_update from u_cb within w_qa_add_narr_for_book
integer x = 535
integer y = 860
integer width = 270
integer taborder = 20
boolean bringtotop = true
string text = "&Update"
end type

event clicked;call super::clicked;int rc
rc = dw_narrtbl.Event pfc_update(TRUE,TRUE)
if rc = 1 THEN
	MessageBox("UPDATE","Update Successful.",Information!)
	COMMIT USING SQLServerTrans;
else
	MessageBox("ERROR","Update failed.",StopSign!)
	ROLLBACK USING SQLServerTrans;
end if

end event

type cb_add from u_cb within w_qa_add_narr_for_book
integer x = 215
integer y = 860
integer width = 274
integer taborder = 10
boolean bringtotop = true
string text = "&Add Row"
boolean default = true
end type

event clicked;call super::clicked;LONG LL_RC // 11/17/2008
ll_rc = dw_narrtbl.Event pfc_addrow()
// 11/17/2008 set audit columns for narr table
IF ll_rc > 0 THEN
	dw_narrtbl.object.created_date[ll_rc] = today()
	dw_narrtbl.object.created_by[ll_rc] = gnv_app.of_getuserid()
//	dw_narrtbl.object.modified_date[ll_rc] = today()
//	dw_narrtbl.object.modified_by[ll_rc] = gnv_app.of_getuserid()
END IF

dw_narrtbl.SetFocus()
end event

type cb_del from u_cb within w_qa_add_narr_for_book
integer x = 841
integer y = 860
integer width = 329
integer taborder = 2
boolean bringtotop = true
string text = "&Delete Row"
end type

event clicked;call super::clicked;dw_narrtbl.Event pfc_deleterow()
end event


$PBExportHeader$w_qa_add_narr.srw
forward
global type w_qa_add_narr from w_response
end type
type dw_narrtbl from u_pics_dw within w_qa_add_narr
end type
type cb_exit from u_cb within w_qa_add_narr
end type
type cb_update from u_cb within w_qa_add_narr
end type
type cb_add from u_cb within w_qa_add_narr
end type
type cb_del from u_cb within w_qa_add_narr
end type
end forward

global type w_qa_add_narr from w_response
integer x = 997
integer y = 488
integer width = 1870
integer height = 1176
string title = "Add/Update Narrator"
dw_narrtbl dw_narrtbl
cb_exit cb_exit
cb_update cb_update
cb_add cb_add
cb_del cb_del
end type
global w_qa_add_narr w_qa_add_narr

on w_qa_add_narr.create
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

on w_qa_add_narr.destroy
call super::destroy
destroy(this.dw_narrtbl)
destroy(this.cb_exit)
destroy(this.cb_update)
destroy(this.cb_add)
destroy(this.cb_del)
end on

type dw_narrtbl from u_pics_dw within w_qa_add_narr
integer x = 23
integer y = 12
integer width = 1824
integer height = 812
integer taborder = 30
string dataobject = "dddw_narrtbl"
boolean livescroll = false
end type

event ue_postconstructor;call super::ue_postconstructor;dw_narrtbl.SetTransObject(sqlservertrans) 
dw_narrtbl.retrieve()
end event

event pfc_addrow;long	ll_rc

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

this.ScrollToRow(ll_rc)
this.SetColumn(1)

return ll_rc
end event

event pfc_preupdate;call super::pfc_preupdate;//////////////////////////////////////////////////////////////////////////////////////////////////////////
//
//	Event: pfc_preupdate
//
//	Description:
//	Set audit column default values before updates
//
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//	
//	Revision History
//
//	Developed by 	Date 				Version						Tracking#
//									
// Murali K.			08/28/2008		Set audit columns while updating
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//
dwitemstatus l_status
long ll_rc, ll_loop

ll_rc = this.rowcount()
FOR ll_loop = 1 TO ll_rc
	l_status = This.GetItemStatus( ll_loop,0, Primary!)
	IF l_status = datamodified! THEN
		This.object.modified_by[ll_loop] = gnv_app.of_getuserid()
		This.object.modified_date[ll_loop] =Today()
	END IF
NEXT
RETURN 1
end event

type cb_exit from u_cb within w_qa_add_narr
integer x = 1559
integer y = 892
integer width = 274
integer taborder = 40
string text = "Ex&it"
end type

event clicked;call super::clicked;close(parent)
end event

type cb_update from u_cb within w_qa_add_narr
integer x = 896
integer y = 892
integer width = 270
integer taborder = 20
boolean bringtotop = true
string text = "&Update"
end type

event clicked;call super::clicked;int rc
rc = dw_narrtbl.Event pfc_update(TRUE,TRUE)
if rc = 1 THEN
	DataWindowChild ldwc_narr
	w_qa_product_review.dw_qa_narr.GetChild ("narr", ldwc_narr)
	ldwc_narr.SetTransObject(sqlservertrans)
	ldwc_narr.Retrieve()
	RETURN 1
else
	MessageBox("ERROR","Update failed.",StopSign!)
	RETURN 0
end if

end event

type cb_add from u_cb within w_qa_add_narr
integer x = 576
integer y = 892
integer width = 274
integer taborder = 10
boolean bringtotop = true
string text = "&Add Row"
boolean default = true
end type

event clicked;call super::clicked;long ll_rc
ll_rc = dw_narrtbl.Event pfc_addrow()
// 08/28/2008 set audit columns
IF ll_rc > 0 THEN
	dw_narrtbl.object.created_date[ll_rc] = today()
	dw_narrtbl.object.created_by[ll_rc] = gnv_app.of_getuserid()
	dw_narrtbl.object.modified_date[ll_rc] = today()
	dw_narrtbl.object.modified_by[ll_rc] = gnv_app.of_getuserid()
END IF
dw_narrtbl.SetFocus()
end event

type cb_del from u_cb within w_qa_add_narr
integer x = 1202
integer y = 892
integer width = 329
integer taborder = 2
boolean bringtotop = true
string text = "&Delete Row"
end type

event clicked;call super::clicked;dw_narrtbl.Event pfc_deleterow()
end event


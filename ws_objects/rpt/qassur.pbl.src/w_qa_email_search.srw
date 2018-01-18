$PBExportHeader$w_qa_email_search.srw
forward
global type w_qa_email_search from w_response
end type
type cb_clear from u_cb within w_qa_email_search
end type
type dw_search from u_pics_dw within w_qa_email_search
end type
type cb_ok from u_cb within w_qa_email_search
end type
type cb_cancel from u_cb within w_qa_email_search
end type
end forward

global type w_qa_email_search from w_response
integer x = 613
integer y = 348
integer width = 2569
integer height = 832
string title = "Email Search"
boolean center = true
boolean ib_disableclosequery = true
cb_clear cb_clear
dw_search dw_search
cb_ok cb_ok
cb_cancel cb_cancel
end type
global w_qa_email_search w_qa_email_search

type variables
Long ll_bkseq
end variables

on w_qa_email_search.create
int iCurrent
call super::create
this.cb_clear=create cb_clear
this.dw_search=create dw_search
this.cb_ok=create cb_ok
this.cb_cancel=create cb_cancel
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.cb_clear
this.Control[iCurrent+2]=this.dw_search
this.Control[iCurrent+3]=this.cb_ok
this.Control[iCurrent+4]=this.cb_cancel
end on

on w_qa_email_search.destroy
call super::destroy
destroy(this.cb_clear)
destroy(this.dw_search)
destroy(this.cb_ok)
destroy(this.cb_cancel)
end on

event open;call super::open;//////////////////////////////////////////////////////////////////////////////////////////////////////////
//
//	Event: open
//
//	Description:
//	Open settings
//
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//	
//	Revision History
//
//	Developed by 	Date 				Version						Tracking#
//									
// Murali K.			02/06/2008      005 PICS 2.0 Modifications	 QAS A.2.1 , A2.2 
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//

dw_search.SetFocus()
end event

type cb_clear from u_cb within w_qa_email_search
integer x = 1531
integer y = 608
integer width = 448
integer taborder = 20
boolean bringtotop = true
string text = "Clear"
end type

event clicked;call super::clicked;//////////////////////////////////////////////////////////////////////////////////////////////////////////
//
//	Event: clicked for cb_clear
//
//	Description:
//	Reset search datawindow
//
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//	
//	Revision History
//
//	Developed by 	Date 				Version						Tracking#
//									
// Murali K.			02/06/2008      005 PICS 2.0 Modifications	 QAS A.2.1 , A2.2 
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//
dw_search.reset()
dw_search.event pfc_insertrow()
end event

type dw_search from u_pics_dw within w_qa_email_search
integer x = 41
integer y = 36
integer width = 2464
integer height = 520
integer taborder = 10
string dataobject = "d_qa_email_search"
boolean vscrollbar = false
boolean livescroll = false
borderstyle borderstyle = stylebox!
boolean ib_isupdateable = false
boolean ib_rmbmenu = false
boolean ib_rmbfocuschange = false
string is_updatesallowed = ""
end type

event constructor;call super::constructor;//////////////////////////////////////////////////////////////////////////////////////////////////////////
//
//	Event: constructor
//
//	Description:
//	Set transaction and retrieve datawindow
//
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//	
//	Revision History
//
//	Developed by 	Date 				Version						Tracking#
//									
// Murali K.			02/06/2008      005 PICS 2.0 Modifications	 QAS A.2.1 , A2.2 
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//

this.of_SetDropDownSearch(TRUE)
this.SetTransObject(sqlservertrans)
this.event pfc_insertrow()
this.retrieve()
end event

type cb_ok from u_cb within w_qa_email_search
integer x = 1001
integer y = 608
integer width = 448
integer taborder = 0
string text = "Search for &Emails"
boolean default = true
end type

event clicked;call super::clicked;//////////////////////////////////////////////////////////////////////////////////////////////////////////
//
//	Event: clicked for cb_ok
//
//	Description:
//	Set structure variable and open search results window
//
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//	
//	Revision History
//
//	Developed by 	Date 				Version						Tracking#
//									
// Murali K.			02/06/2008      005 PICS 2.0 Modifications	 QAS A.2.1 , A2.2 
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//

str_email_search lstr_email_search

IF dw_search.Rowcount() < 1 THEN
	RETURN  1
END IF

lstr_email_search.prdr 		= dw_search.object.prdr[1]
lstr_email_search.bkseq		= dw_search.object.bkseq[1]
lstr_email_search.ttl 			= dw_search.object.ttl[1]
lstr_email_search.from_dt 	= dw_search.object.from_dt[1]
lstr_email_search.to_dt		= dw_search.object.to_dt[1]

// pass the structure to the search results screen
Openwithparm(w_qa_email_results, lstr_email_search)
end event

type cb_cancel from u_cb within w_qa_email_search
integer x = 2062
integer y = 608
integer width = 448
integer taborder = 0
boolean bringtotop = true
string text = "Exit"
end type

event clicked;call super::clicked;//////////////////////////////////////////////////////////////////////////////////////////////////////////
//
//	Event: clicked for cb_cancel
//
//	Description:
// Close the window
//
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//	
//	Revision History
//
//	Developed by 	Date 				Version						Tracking#
//									
// Murali K.			02/06/2008      005 PICS 2.0 Modifications	 QAS A.2.1 , A2.2 
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//
Close(parent)
end event


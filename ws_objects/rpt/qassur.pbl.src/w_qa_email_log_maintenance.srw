$PBExportHeader$w_qa_email_log_maintenance.srw
forward
global type w_qa_email_log_maintenance from w_sheet
end type
type cb_exit from u_cb within w_qa_email_log_maintenance
end type
type cb_resend from u_cb within w_qa_email_log_maintenance
end type
type cb_clear from u_cb within w_qa_email_log_maintenance
end type
type dw_results from u_pics_dw within w_qa_email_log_maintenance
end type
type cb_search from u_cb within w_qa_email_log_maintenance
end type
type dw_search from u_pics_dw within w_qa_email_log_maintenance
end type
type gb_1 from groupbox within w_qa_email_log_maintenance
end type
end forward

global type w_qa_email_log_maintenance from w_sheet
integer x = 214
integer y = 221
integer width = 4187
integer height = 2348
string title = "Email Log Maintenance"
boolean ib_isupdateable = false
cb_exit cb_exit
cb_resend cb_resend
cb_clear cb_clear
dw_results dw_results
cb_search cb_search
dw_search dw_search
gb_1 gb_1
end type
global w_qa_email_log_maintenance w_qa_email_log_maintenance

type variables

private:
string is_select
long il_id
end variables

forward prototypes
public function integer of_retrieve_results (str_email_search astr_email_search)
end prototypes

public function integer of_retrieve_results (str_email_search astr_email_search);//////////////////////////////////////////////////////////////////////////////////////////////////////////
//
//	Function: of_retrieva_results
// Args : Structure str_email_search
//	Description:
//	Dynamically set search criteria and retrieve results
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


String 		ls_select, ls_where = ' WHERE ', ls_from
Boolean	 	lb_title=FALSE
long ll_rc

ls_select = is_select // dw_results.getsqlselect()

IF NOT Isnull(astr_email_search.ttl) THEN
	lb_title=TRUE
END IF

IF lb_title THEN
	ls_from = ", ttlinit, mchar "
	ls_select += ls_from
END IF

IF NOT Isnull(astr_email_search.prdr)  THEN
   ls_where += " QA_EMAIL_LOG.PRDR = '" + astr_email_search.prdr + "' AND"
END IF

IF NOT Isnull(astr_email_search.bkseq)  AND ( NOT lb_title) THEN
   ls_where += " QA_EMAIL_LOG.BKSEQ =  " + String(astr_email_search.bkseq) + " AND"
END IF

IF lb_title THEN
	astr_email_search.ttl  = lower(astr_email_search.ttl )
	ls_where += " ( Lower(TTLINIT.TTL) LIKE '%" + astr_email_search.ttl  + "%' AND MCHAR.CHNO = TTLINIT.CHNO AND MCHAR.BKSEQ = QA_EMAIL_LOG.BKSEQ  AND "  + &
					"MCHAR.BKMED = QA_EMAIL_LOG.BKMED)   AND"
END IF

IF NOT Isnull(astr_email_search.from_dt)  AND &
	NOT Isnull(astr_email_search.to_dt)	THEN
	// 10/30/2008 CONVERT DATETIME TO DATE FOR DATE RANGE CHECK.
   ls_where += " TO_DATE(QA_EMAIL_LOG.DATE_SENT)  BETWEEN '" + String(astr_email_search.from_dt, 'DD-MMM-YYYY') + "' AND '"  + String(astr_email_search.to_dt, 'DD-MMM-YYYY') + "' AND"
END IF

ls_select += ls_where
// Strip the last AND off 
ls_select = Left(ls_select, len(ls_select) -3)

//messagebox('sql', ls_select)

dw_results.setsqlselect(ls_select)
ll_rc = dw_results.retrieve()
IF ll_rc < 1 THEN
	Messagebox('Error', 'No records found')
END IF 

RETURN 1	
end function

on w_qa_email_log_maintenance.create
int iCurrent
call super::create
this.cb_exit=create cb_exit
this.cb_resend=create cb_resend
this.cb_clear=create cb_clear
this.dw_results=create dw_results
this.cb_search=create cb_search
this.dw_search=create dw_search
this.gb_1=create gb_1
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.cb_exit
this.Control[iCurrent+2]=this.cb_resend
this.Control[iCurrent+3]=this.cb_clear
this.Control[iCurrent+4]=this.dw_results
this.Control[iCurrent+5]=this.cb_search
this.Control[iCurrent+6]=this.dw_search
this.Control[iCurrent+7]=this.gb_1
end on

on w_qa_email_log_maintenance.destroy
call super::destroy
destroy(this.cb_exit)
destroy(this.cb_resend)
destroy(this.cb_clear)
destroy(this.dw_results)
destroy(this.cb_search)
destroy(this.dw_search)
destroy(this.gb_1)
end on

event open;call super::open;//////////////////////////////////////////////////////////////////////////////////////////////////////////
//
//	Event: Open for window
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
// Murali K.			02/07/2008      005 PICS 2.0 Modifications	 QAS A.2.1 , A2.2 
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//

// Open the sheet in Maximized mode
this.windowstate = maximized!
end event

type cb_exit from u_cb within w_qa_email_log_maintenance
integer x = 3648
integer y = 2112
integer width = 448
integer taborder = 60
string text = "Exit"
boolean cancel = true
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

type cb_resend from u_cb within w_qa_email_log_maintenance
integer x = 3168
integer y = 2112
integer width = 448
integer taborder = 50
string text = "&Resend"
end type

event clicked;call super::clicked;//////////////////////////////////////////////////////////////////////////////////////////////////////////
//
//	Event: clicked for resend button
//
//	Description:
//	Open Email Resend window
//
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//	
//	Revision History
//
//	Developed by 	Date 				Version						Tracking#
//									
// Murali K.			02/08/2008      005 PICS Modifications	 Reqs: QAS 2.2.1
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//

Openwithparm(w_qa_email_resend,il_id)
end event

type cb_clear from u_cb within w_qa_email_log_maintenance
integer x = 3648
integer y = 516
integer width = 448
integer taborder = 30
string text = "&Clear"
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
dw_search.setfocus()
end event

type dw_results from u_pics_dw within w_qa_email_log_maintenance
integer x = 50
integer y = 664
integer width = 4023
integer height = 1412
integer taborder = 20
string dataobject = "d_qa_email_results"
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
is_select = this.getsqlselect()
end event

event rowfocuschanged;call super::rowfocuschanged;//////////////////////////////////////////////////////////////////////////////////////////////////////////
//
//	Event: rowfocuschanged for dw
//
//	Description:
//	Highlight row and set the message id
//
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//	
//	Revision History
//
//	Developed by 	Date 				Version						Tracking#
//									
// Murali K.			02/08/2008      005 PICS Modifications	 Reqs: QAS 2.2.1
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//
IF This.rowcount() > 0 THEN
	This.SelectRow(0,FALSE)
	This.SelectRow(currentrow, TRUE)
	il_id = THIS.object.message_id[currentrow]
END IF
end event

event doubleclicked;call super::doubleclicked;//////////////////////////////////////////////////////////////////////////////////////////////////////////
//
//	Event: rowfocuschanged for dw
//
//	Description:
//	Highlight row and set the org code
//
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//	
//	Revision History
//
//	Developed by 	Date 				Version						Tracking#
//									
// Murali K.			01/25/2008      005 PICS Modifications	 Reqs: CDS.A.PTB2,ptb2.1
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//
IF row > 0 THEN
	cb_resend.triggerevent('clicked')
END IF
end event

type cb_search from u_cb within w_qa_email_log_maintenance
integer x = 3136
integer y = 516
integer width = 448
integer taborder = 20
string text = "Search for Ema&ils"
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

dw_search.Accepttext()
// set the structure for search results
lstr_email_search.prdr 		= dw_search.object.prdr[1]
lstr_email_search.bkseq		= dw_search.object.bkseq[1]
lstr_email_search.ttl 			= dw_search.object.ttl[1]
lstr_email_search.from_dt 	= dw_search.object.from_dt[1]
lstr_email_search.to_dt		= dw_search.object.to_dt[1]

IF Isnull(lstr_email_search.ttl)  AND &
    Isnull(lstr_email_search.prdr)  AND &
	Isnull(lstr_email_search.bkseq)  AND &
	Isnull(lstr_email_search.from_dt)  AND &
	Isnull(lstr_email_search.to_dt)	THEN
	Messagebox('Error', 'Please enter some search criteria')
ELSEIF lstr_email_search.to_dt < lstr_email_search.from_dt  THEN
	Messagebox('Error', 'To date cannot be earlier than from date')
ELSE
	//set dynamic where conditions to retrieve search results
	of_retrieve_results(lstr_email_search)
END IF




end event

type dw_search from u_pics_dw within w_qa_email_log_maintenance
event ue_enterkey pbm_dwnprocessenter
integer x = 41
integer y = 32
integer width = 4050
integer height = 472
integer taborder = 10
string dataobject = "d_qa_email_search"
boolean vscrollbar = false
borderstyle borderstyle = stylebox!
boolean ib_isupdateable = false
boolean ib_rmbmenu = false
boolean ib_rmbfocuschange = false
string is_updatesallowed = ""
end type

event ue_enterkey;//////////////////////////////////////////////////////////////////////////////////////////////////////////
//
//	Event:  enter key for dw
//
//	Description:
//	Simulate tab
//
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//	
//	Revision History
//
//	Developed by 	Date 				Version						Tracking#
//									
// Murali K.			02/07/2008      005 PICS Modifications	 Reqs: QAS 2.2.1
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//
Send(Handle(Parent), 273, 0, Handle(cb_search)) // clicked event of search button

end event

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
//this.retrieve()
end event

event editchanged;call super::editchanged;//////////////////////////////////////////////////////////////////////////////////////////////////////////
//
//	Event: editchanged
//
//	Description:
//	only one book number or title can be entered
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
long ll_null
string ls_null

setnull(ll_null)
setnull(ls_null)

CHOOSE CASE dwo.name
	CASE 'bkseq'
		This.object.ttl[1]=ls_null
	CASE 'ttl'
		This.object.bkseq[1]=ll_null
END CHOOSE
end event

type gb_1 from groupbox within w_qa_email_log_maintenance
integer x = 37
integer y = 608
integer width = 4059
integer height = 1480
integer taborder = 40
integer textsize = -8
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 33554432
long backcolor = 67108864
string text = "Email Search Results"
borderstyle borderstyle = stylebox!
end type


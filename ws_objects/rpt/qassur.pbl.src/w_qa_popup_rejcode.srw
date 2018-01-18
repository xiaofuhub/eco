$PBExportHeader$w_qa_popup_rejcode.srw
forward
global type w_qa_popup_rejcode from w_popup
end type
type lb_rejcode from listbox within w_qa_popup_rejcode
end type
type dw_codes from u_dw within w_qa_popup_rejcode
end type
type cb_ok from commandbutton within w_qa_popup_rejcode
end type
type cb_cancel from commandbutton within w_qa_popup_rejcode
end type
end forward

global type w_qa_popup_rejcode from w_popup
integer x = 214
integer y = 221
integer width = 1760
integer height = 1308
string title = "Rejection Code and Description"
boolean minbox = false
boolean maxbox = false
boolean resizable = false
windowtype windowtype = response!
lb_rejcode lb_rejcode
dw_codes dw_codes
cb_ok cb_ok
cb_cancel cb_cancel
end type
global w_qa_popup_rejcode w_qa_popup_rejcode

type variables
string is_rejcd
end variables

forward prototypes
public function integer of_filter (string as_category)
end prototypes

public function integer of_filter (string as_category);//////////////////////////////////////////////////////////////////////////////////////////////////////////
//
//	Function: of_filter
//  Args: String category
//	Description:
//	Populate rejection codes from database table
//
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//	
//	Revision History
//
//	Developed by 	Date 				Purpose									Tracking#
//									
// Murali K.            03/21/2008		 PICS 2.0 				QAS Modifications
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//


long ll_rc 
int li_loop
string  ls_rej_code, ls_rej_code_name , ls_rej_code_desc, ls_text
string ls_filter


IF as_category = 'ALL' THEN
		
		li_loop = dw_codes.of_SetTransObject(SQLserverTrans)
		ll_rc = dw_codes.retrieve()
		ls_filter = "category = 'RECORDED' "
		dw_codes.setfilter(ls_filter)
		dw_codes.filter()
		ll_rc = dw_codes.rowcount()
		FOR li_loop = 1 TO ll_rc
			ls_rej_code 			= dw_codes.object.reject_code[li_loop]
			ls_rej_code_name = dw_codes.object.reject_code_name[li_loop]
			ls_rej_code_desc  = dw_codes.object.reject_code_description[li_loop]
			ls_text = Mid(ls_rej_code,1,2) + ' =' + ls_rej_code_name + '   (' + ls_rej_code_desc + ')'
			lb_rejcode.additem(ls_text)
		NEXT
ELSE
// this part might not be needed
		li_loop = dw_codes.of_SetTransObject(SQLserverTrans)
		ll_rc = dw_codes.retrieve()
		ls_filter = "category = 'RECORDED' and reject_code = 'RR'"
		dw_codes.setfilter(ls_filter)
		dw_codes.filter()
		ll_rc = dw_codes.rowcount()
		FOR li_loop = 1 TO ll_rc
			ls_rej_code 			= dw_codes.object.reject_code[li_loop]
			ls_rej_code_name = dw_codes.object.reject_code_name[li_loop]
			ls_rej_code_desc  = dw_codes.object.reject_code_description[li_loop]
			ls_text = Mid(ls_rej_code,1,2) + ' =' + ls_rej_code_name + '   (' + ls_rej_code_desc + ')'
			lb_rejcode.additem(ls_text)
		NEXT

END IF
RETURN 1
end function

on w_qa_popup_rejcode.create
int iCurrent
call super::create
this.lb_rejcode=create lb_rejcode
this.dw_codes=create dw_codes
this.cb_ok=create cb_ok
this.cb_cancel=create cb_cancel
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.lb_rejcode
this.Control[iCurrent+2]=this.dw_codes
this.Control[iCurrent+3]=this.cb_ok
this.Control[iCurrent+4]=this.cb_cancel
end on

on w_qa_popup_rejcode.destroy
call super::destroy
destroy(this.lb_rejcode)
destroy(this.dw_codes)
destroy(this.cb_ok)
destroy(this.cb_cancel)
end on

event pfc_postopen;call super::pfc_postopen;//////////////////////////////////////////////////////////////////////////////////////////////////////////
//
//	Event: pfc_postopen for window
//
//	Description:
//	Populate rejection codes from database table
//
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//	
//	Revision History
//
//	Developed by 	Date 				Purpose									Tracking#
//									
// Murali K.            03/21/2008		 PICS 2.0 				QAS Modifications
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//

// 10/16/2008 filter for reverse rejection code - no need 11/5/2008
string ls_category

is_rejcd = Message.stringparm
ls_category = 'ALL'
of_filter(ls_category)
end event

type lb_rejcode from listbox within w_qa_popup_rejcode
integer x = 50
integer y = 28
integer width = 1655
integer height = 1024
integer taborder = 10
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
boolean hscrollbar = true
boolean vscrollbar = true
boolean multiselect = true
borderstyle borderstyle = styleraised!
boolean extendedselect = true
end type

type dw_codes from u_dw within w_qa_popup_rejcode
boolean visible = false
integer x = 1609
integer y = 1136
integer width = 96
integer height = 88
integer taborder = 13
string dataobject = "dddw_ref_qa_rejection_codes"
end type

type cb_ok from commandbutton within w_qa_popup_rejcode
integer x = 489
integer y = 1088
integer width = 311
integer height = 100
integer taborder = 20
boolean bringtotop = true
integer textsize = -10
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "&OK"
boolean default = true
end type

event clicked;int li_ItemTotal,li_ItemCount
string reject_str,Lstr

// Get the number of items in the tables ListBox.
li_ItemTotal = lb_rejcode.TotalItems( )

// Loop through all the items.
FOR li_ItemCount = 1 to li_ItemTotal
	// Is the code selected
	IF lb_rejcode.state(li_ItemCount) = 1 THEN  
		Lstr = lb_rejcode.text(li_ItemCount)
		reject_str = reject_str + Mid(Lstr,1,2) // 03/21/2008 1 char to 2 chars
	END IF
NEXT
CloseWithReturn(w_qa_popup_rejcode,reject_str)

end event

type cb_cancel from commandbutton within w_qa_popup_rejcode
integer x = 905
integer y = 1088
integer width = 306
integer height = 100
integer taborder = 3
boolean bringtotop = true
integer textsize = -10
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "&Cancel"
end type

event clicked;close(parent)
end event


$PBExportHeader$w_qa_autotest_rejected_books.srw
forward
global type w_qa_autotest_rejected_books from w_sheet
end type
type em_1 from editmask within w_qa_autotest_rejected_books
end type
type st_3 from statictext within w_qa_autotest_rejected_books
end type
type st_2 from statictext within w_qa_autotest_rejected_books
end type
type dw_3 from u_pics_dw within w_qa_autotest_rejected_books
end type
type st_1 from statictext within w_qa_autotest_rejected_books
end type
type dw_2 from u_pics_dw within w_qa_autotest_rejected_books
end type
type cb_exit from commandbutton within w_qa_autotest_rejected_books
end type
end forward

global type w_qa_autotest_rejected_books from w_sheet
integer x = 233
integer y = 288
integer width = 2830
integer height = 1816
string title = "Autotest Rejected Books"
em_1 em_1
st_3 st_3
st_2 st_2
dw_3 dw_3
st_1 st_1
dw_2 dw_2
cb_exit cb_exit
end type
global w_qa_autotest_rejected_books w_qa_autotest_rejected_books

type variables
n_ds ids
boolean ib_changesmade

string is_len
string is_vols
string is_minlastside
string is_qarecdt

boolean ib_active = false
end variables

forward prototypes
public function integer of_filter ()
end prototypes

public function integer of_filter ();/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//
//	Function : of_filter
// Args: None
//	Description:  filter data based on date range or a book number
//
/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//	
//	Revision History
//
//	Developed by 	Date 				Version						Tracking#
//									
// Murali K.			09/23/2008      Phase-2 
/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//
date ld_data
date ld_from, ld_to
long ll_rc
string ls_filter, ls_sort

dw_2.show()
dw_3.accepttext()
ld_to = date(dw_3.object.qarecdt2[1])
ld_from = date(dw_3.object.qarecdt[1]) 

IF Len(Trim(string(ld_from))) > 0  AND( isnull(ld_to) or  Len(Trim(string(ld_to))) = 0 ) THEN
	ls_filter = "date(qastg_qarecdt) >= date('" + string(ld_from, 'mm/dd/yyyy') + "')" 
	dw_2.setfilter('')
	dw_2.filter()
	dw_2.setfilter(ls_filter)
	 dw_2.filter()
END IF


IF Len(Trim(string(ld_from))) > 0  AND Len(Trim(string(ld_to))) > 0 THEN
	ls_filter = "date(qastg_qarecdt) between date('" + string(ld_from, 'mm/dd/yyyy') + "') and date('" + string(ld_to,'mm/dd/yyyy') +"')"
	dw_2.setfilter('')
	dw_2.filter()
	dw_2.setfilter(ls_filter)
	 dw_2.filter()
END IF

IF Len(trim(em_1.text)) > 0 THEN
	ls_filter = 'bkseq = ' + em_1.text
	dw_2.setfilter('')
	dw_2.filter()
	dw_2.setfilter(ls_filter)
	dw_2.filter()
END IF
ls_sort = 'qacompdt, bkseq asc'
dw_2.setsort(ls_sort)
dw_2.sort()

RETURN 1
end function

on w_qa_autotest_rejected_books.create
int iCurrent
call super::create
this.em_1=create em_1
this.st_3=create st_3
this.st_2=create st_2
this.dw_3=create dw_3
this.st_1=create st_1
this.dw_2=create dw_2
this.cb_exit=create cb_exit
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.em_1
this.Control[iCurrent+2]=this.st_3
this.Control[iCurrent+3]=this.st_2
this.Control[iCurrent+4]=this.dw_3
this.Control[iCurrent+5]=this.st_1
this.Control[iCurrent+6]=this.dw_2
this.Control[iCurrent+7]=this.cb_exit
end on

on w_qa_autotest_rejected_books.destroy
call super::destroy
destroy(this.em_1)
destroy(this.st_3)
destroy(this.st_2)
destroy(this.dw_3)
destroy(this.st_1)
destroy(this.dw_2)
destroy(this.cb_exit)
end on

event closequery;int li_choice

//messagebox("Closequery","closequery")
if ib_changesmade = true then
	li_choice = messagebox("Discard Changes?","This data has not been updated.  Discard changes without saving?",StopSign!,OkCancel!,2)
   if li_choice = 1 then
		return 0
	else
		return 1
	end if
end if

return 0


end event

event resize;call super::resize;long ll_height

This.X = w_pics_main.X
This.Y = w_pics_main.Y
ll_height = w_pics_main.mdi_1.Height
This.Resize(w_pics_main.WorkSpaceWidth(), ll_height)
end event

event pfc_preopen;call super::pfc_preopen;this.of_SetPreference(TRUE)
this.inv_preference.of_SetToolBars(TRUE)
this.inv_preference.of_SetWindow(TRUE)
this.of_SetResize(TRUE)
this.inv_resize.of_SetOrigSize(this.workspacewidth(),this.workspaceheight())
//inv_resize.of_Register(dw_1, "Scale")
inv_resize.of_Register(dw_2, "Scale")
//inv_resize.of_Register(dw_3, "FixedToRight&Bottom")
inv_resize.of_Register(cb_exit, "FixedToRight&Bottom")

end event

event pfc_postopen;call super::pfc_postopen;//////////////////////////////////////////////////////////////////////////////////////////////////////////
//
//	Event:  postopen
//
//	Description:
//	Retrieve based on date range
//
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//	
//	Revision History
//
//	Developed by 	Date 				Version							Tracking#
//									
// Murali K.			09/26/2008      004 PICS 2.0 Modifications	 Reqs: QAS.A.1.1, QAS.A.1.2
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//

of_filter()
end event

type em_1 from editmask within w_qa_autotest_rejected_books
integer x = 1801
integer y = 100
integer width = 343
integer height = 92
integer taborder = 40
integer textsize = -8
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 33554432
borderstyle borderstyle = stylelowered!
string mask = "#########"
end type

event modified;//////////////////////////////////////////////////////////////////////////////////////////////////////////
//
//	Event:  modified for em_1
//
//	Description:
//	Retrieve based on entered book number
//
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//	
//	Revision History
//
//	Developed by 	Date 				Version							Tracking#
//									
// Murali K.			02/05/2008      004 PICS 2.0 Modifications	 Reqs: QAS.A.1.1, QAS.A.1.2
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//
long ll_rc
dw_3.reset()
dw_3.insertrow(0)
//dw_2.dataobject = 'd_qa_prod_recv_by_autotest_rejected_bkseq'
//dw_2.settransobject(sqlservertrans)
//ll_rc = dw_2.Retrieve(Long(this.text))
//
//IF ll_rc < 1 THEN
//    messagebox('Error', 'Book Number not found.')
//END IF
of_filter()


end event

type st_3 from statictext within w_qa_autotest_rejected_books
integer x = 1682
integer y = 24
integer width = 608
integer height = 76
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 33554432
long backcolor = 67108864
string text = "Select Book Number"
boolean focusrectangle = false
end type

type st_2 from statictext within w_qa_autotest_rejected_books
integer x = 1394
integer y = 24
integer width = 123
integer height = 64
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 33554432
long backcolor = 67108864
string text = "OR"
boolean focusrectangle = false
end type

type dw_3 from u_pics_dw within w_qa_autotest_rejected_books
integer x = 46
integer y = 100
integer width = 1170
integer height = 96
integer taborder = 30
string dataobject = "d_qa_qarecdt_range"
boolean vscrollbar = false
boolean border = false
boolean livescroll = false
borderstyle borderstyle = stylebox!
end type

event ue_postconstructor;call super::ue_postconstructor;date lqarecdt

this.SetTransObject(sqlservertrans) 
this.Retrieve()


end event

event itemchanged;call super::itemchanged;//////////////////////////////////////////////////////////////////////////////////////////////////////////
//
//	Event:  itemchanged
//
//	Description:
//	Retrieve based on date range
//
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//	
//	Revision History
//
//	Developed by 	Date 				Version							Tracking#
//									
// Murali K.			02/05/2008      004 PICS 2.0 Modifications	 Reqs: QAS.A.1.1, QAS.A.1.2
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//

date ld_data
date ld_from, ld_to
long ll_rc

CHOOSE CASE dwo.name
	// From date range	
	CASE 'qarecdt'
		em_1.text=''
		ld_to = date(this.object.qarecdt2[1])
		ld_from = date(data)
		IF ld_from > ld_to THEN
			Messagebox('Error','From Date cannot be greater than to date.')
			RETURN 1
		END IF
		of_filter()
	CASE 'qarecdt2'
		em_1.text=''
		ld_from = date(this.object.qarecdt[1])
		IF Isnull(ld_from) THEN
			Messagebox('Error','Please select a from date.')
			RETURN 0
		END IF
		ld_to = date(data)
		IF ld_to < ld_from THEN
			Messagebox('Error','To Date cannot be  earlier than From date.')
			RETURN 1
		END IF
		of_filter()
END CHOOSE
end event

type st_1 from statictext within w_qa_autotest_rejected_books
integer x = 411
integer y = 16
integer width = 562
integer height = 64
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 33554432
long backcolor = 67108864
string text = "Select Date Range"
boolean focusrectangle = false
end type

type dw_2 from u_pics_dw within w_qa_autotest_rejected_books
event ue_enterkey pbm_dwnprocessenter
integer x = 32
integer y = 240
integer width = 2738
integer height = 1296
integer taborder = 20
boolean bringtotop = true
string dataobject = "d_qa_prod_recv_by_autotest_rejected"
boolean livescroll = false
boolean ib_isupdateable = false
boolean ib_rmbmenu = false
boolean ib_rmbfocuschange = false
string is_updatesallowed = ""
end type

event constructor;call super::constructor;dw_2.SetTransObject(sqlservertrans)
dw_2.hide()
dw_2.retrieve()


end event

event sqlpreview;call super::sqlpreview;//messagebox("SQLPREVIEW",sqlsyntax)
end event

type cb_exit from commandbutton within w_qa_autotest_rejected_books
integer x = 2514
integer y = 1564
integer width = 251
integer height = 92
boolean bringtotop = true
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "E&xit"
end type

event clicked;close(parent)
end event


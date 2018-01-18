$PBExportHeader$w_pa_assign_books_label.srw
forward
global type w_pa_assign_books_label from w_sheet
end type
type cb_print from u_cb within w_pa_assign_books_label
end type
type dw_pa_book_assign_label from u_dw within w_pa_assign_books_label
end type
type dw_pa_book_assign_labelcopy from u_dw within w_pa_assign_books_label
end type
type cb_cancel from u_cb within w_pa_assign_books_label
end type
end forward

global type w_pa_assign_books_label from w_sheet
integer x = 214
integer y = 221
integer width = 2757
integer height = 1492
string title = "Book Assignment Label Printing"
long backcolor = 81576884
cb_print cb_print
dw_pa_book_assign_label dw_pa_book_assign_label
dw_pa_book_assign_labelcopy dw_pa_book_assign_labelcopy
cb_cancel cb_cancel
end type
global w_pa_assign_books_label w_pa_assign_books_label

type variables
string is_bklist[]
end variables

on w_pa_assign_books_label.create
int iCurrent
call super::create
this.cb_print=create cb_print
this.dw_pa_book_assign_label=create dw_pa_book_assign_label
this.dw_pa_book_assign_labelcopy=create dw_pa_book_assign_labelcopy
this.cb_cancel=create cb_cancel
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.cb_print
this.Control[iCurrent+2]=this.dw_pa_book_assign_label
this.Control[iCurrent+3]=this.dw_pa_book_assign_labelcopy
this.Control[iCurrent+4]=this.cb_cancel
end on

on w_pa_assign_books_label.destroy
call super::destroy
destroy(this.cb_print)
destroy(this.dw_pa_book_assign_label)
destroy(this.dw_pa_book_assign_labelcopy)
destroy(this.cb_cancel)
end on

event mousemove;call super::mousemove;w_pics_main.setmicrohelp("Ready")
end event

event open;call super::open;
THIS.Windowstate = maximized!


end event

event pfc_postopen;call super::pfc_postopen;long i,li_count, li_cnt, rtn, li_cur, li_pos, li_len, li_add
string ls_auth, ls_bkno, ls_cntr, ls_sttl, ls_cntr2, ls_date, ls_narr, ls_conno
date ld_date

ls_cntr='03APHRC'
ld_date=date('06/25/2003')
if IsNull(Message.StringParm)=FALSE AND Message.StringParm<>"" then
   ls_cntr2=message.stringParm
	li_pos=pos(ls_cntr2,',')
	ls_cntr=left(ls_cntr2, li_pos - 1)
	ls_date=mid(ls_cntr2, li_pos +1 )
	ld_date=date(ls_date)
end if
dw_pa_book_assign_label.settransobject(sqlservertrans)
dw_pa_book_assign_labelcopy.Reset()
li_cnt=dw_pa_book_assign_label.Retrieve(ls_cntr, ld_date)

if li_cnt>0 then
//	messagebox('argument total books','total books is : '+string(li_cnt))
	for i=1 to li_cnt
		ls_conno = dw_pa_book_assign_label.object.conno[i]
		ls_conno = "~*"+ls_conno+"~*"
		ls_bkno=dw_pa_book_assign_label.object.bkno[i]
		ls_auth=trim(dw_pa_book_assign_label.object.auth[i])
		IF NOT(IsNull(ls_auth )) THEN
			ls_auth = ls_auth+' - '
		ELSE
			ls_auth = ' '
		END IF
		li_len=len(ls_auth)
		li_add=33 - li_len
		ls_sttl=trim(dw_pa_book_assign_label.object.sttl[i])
		ls_sttl=left(ls_sttl, li_add)
		ls_sttl=upper(mid(ls_sttl,1,1))+mid(ls_sttl,2)
		ls_auth= ls_auth+ ls_sttl
		li_cur=dw_pa_book_assign_labelcopy.insertRow(0)
		ls_narr='- NARR'
		dw_pa_book_assign_labelcopy.setitem(li_cur,'bkno',ls_bkno)
		dw_pa_book_assign_labelcopy.setitem(li_cur,'narr',ls_narr)
		dw_pa_book_assign_labelcopy.setitem(li_cur,'auth',ls_auth)
		dw_pa_book_assign_labelcopy.setitem(li_cur,'conno',ls_conno)
//		dw_pa_book_assign_labelcopy.setitem(li_cur,'sttl',ls_sttl)
		li_cur=dw_pa_book_assign_labelcopy.insertRow(0)
		ls_narr='- MON'
		dw_pa_book_assign_labelcopy.setitem(li_cur,'bkno',ls_bkno)
		dw_pa_book_assign_labelcopy.setitem(li_cur,'narr',ls_narr)
		dw_pa_book_assign_labelcopy.setitem(li_cur,'auth',ls_auth)
		dw_pa_book_assign_labelcopy.setitem(li_cur,'conno',ls_conno)
//		dw_pa_book_assign_labelcopy.setitem(li_cur,'sttl',ls_sttl)
		li_cur=dw_pa_book_assign_labelcopy.insertRow(0)
		ls_narr='- FOLD'
		dw_pa_book_assign_labelcopy.setitem(li_cur,'bkno',ls_bkno)
		dw_pa_book_assign_labelcopy.setitem(li_cur,'narr',ls_narr)
		dw_pa_book_assign_labelcopy.setitem(li_cur,'auth',ls_auth)
		dw_pa_book_assign_labelcopy.setitem(li_cur,'conno',ls_conno)
//		dw_pa_book_assign_labelcopy.setitem(li_cur,'sttl',ls_sttl)
	next
end if

setpointer(Arrow!)

end event

event pfc_preopen;call super::pfc_preopen;this.of_SetPreference(TRUE)
this.inv_preference.of_SetToolBars(TRUE)
this.inv_preference.of_SetWindow(TRUE)
this.of_SetResize(TRUE)
this.inv_resize.of_SetOrigSize(this.workspacewidth(),this.workspaceheight())
inv_resize.of_Register(cb_cancel, "scale")
inv_resize.of_Register(cb_print, "scale")

inv_resize.of_Register(dw_pa_book_assign_label, "scale")
inv_resize.of_Register(dw_pa_book_assign_labelcopy, "scale")

end event

event resize;call super::resize;//long ll_height
//
//This.X = w_pics_main.X
//This.Y = w_pics_main.Y
//ll_height = w_pics_main.mdi_1.Height
//This.Resize(w_pics_main.WorkSpaceWidth(), ll_height)
end event

type cb_print from u_cb within w_pa_assign_books_label
event pfc_hinttext pbm_mousemove
string tag = "Exits the screen"
integer x = 2144
integer y = 1244
integer width = 238
integer taborder = 30
fontcharset fontcharset = ansi!
string text = "&Print"
end type

event clicked;call super::clicked;dw_pa_book_assign_labelcopy.Event pfc_print()
end event

type dw_pa_book_assign_label from u_dw within w_pa_assign_books_label
event pfc_hinttext pbm_mousemove
event ue_enterkey pbm_dwnprocessenter
event pfc_keydown pbm_dwnkey
boolean visible = false
integer x = 201
integer y = 1284
integer width = 50
integer height = 52
integer taborder = 20
string dataobject = "d_pa_book_assign_label"
boolean vscrollbar = false
boolean border = false
boolean livescroll = false
boolean ib_isupdateable = false
boolean ib_rmbmenu = false
boolean ib_rmbfocuschange = false
string is_updatesallowed = ""
end type

event pfc_hinttext;//string ls_object, ls_column, ls_column_tag
//long ll_pos
//
////This script set's microhelp at the bottom of the screen dw_vol_selection Datawindow
//ls_object = THIS.getobjectatpointer()
//ll_pos = pos(ls_object, "~t")
//IF NOT pos(ls_object, "_t~t") > 0 THEN
//	IF ll_pos > 0 THEN
//		ll_pos = ll_pos -1
//		ls_column = mid(ls_object,1,ll_pos)
//		ls_column_tag = THIS.Describe(ls_column + ".tag")
//		w_pics_main.setmicrohelp(ls_column_tag)
//	ELSE
//		w_pics_main.setmicrohelp("Ready")
//	END IF
//END IF
end event

event ue_enterkey;//Send(Handle(dw_select_deselect),256,9,Long(0,0))
//return(1)
end event

event clicked;call super::clicked;

this.SelectRow(0,false)
this.SelectRow(row,true)
this.ScrollToRow(row)
this.SetRow(row)
this.SetFocus()
end event

event constructor;call super::constructor;ib_rmbmenu = FALSE


end event

event getfocus;call super::getfocus;m_pics_main.m_Edit.m_Addrow.Enabled = FALSE
m_pics_main.m_Edit.m_Deleterow.Enabled = FALSE
m_pics_main.m_Edit.m_Cut.Enabled = FALSE
end event

event itemchanged;//long li_row_count, li_count=0,i
//string  ls_cld
//
////messagebox('','itemchanged')
//
//if dwo.name ='include1' then
//	ls_cld=data
//	dw_distribution_schedule_null_incld_cpy.object.include1[row]= ls_cld
//end if
//li_row_count=dw_distribution_schedule_null_incld_cpy.RowCount()
//for i=1 to li_row_count
//	ls_cld=dw_distribution_schedule_null_incld_cpy.object.include1[row]
//	if ls_cld='Y' then
////		dw_distribution_schedule_null_incld_cpy.object.dsdt[row]= ld_dsdt
//		li_count++
//		
//	end if
//next
//sle_book#.text=string(li_count)
end event

event retrieveend;call super::retrieveend;integer i,j
long Lbkseq
string Lbkmed,Lcntr
date Lschstdt,Lactstdt,Lactenddt
//

	close(w_pics_retrieve_msg_box)

end event

event retrievestart;//OpenWithParm(w_pics_retrieve_msg_box,"Retrieving records, Please wait...")

end event

event rowfocuschanged;call super::rowfocuschanged;this.SelectRow(0,false)
this.SelectRow(currentrow,true)
//this.ScrollToRow(row)
//this.SetRow(row)
//this.SetFocus()
end event

event sqlpreview;call super::sqlpreview;//MessageBox("sql",sqlsyntax)
end event

event updateend;call super::updateend;Close(w_pics_update_msg_box)
end event

event updatestart;call super::updatestart;Open(w_pics_update_msg_box)
end event

event pfc_retrieve;//RETURN THIS.Retrieve()
return 1

end event

type dw_pa_book_assign_labelcopy from u_dw within w_pa_assign_books_label
event pfc_hinttext pbm_mousemove
event ue_enterkey pbm_dwnprocessenter
event pfc_keydown pbm_dwnkey
integer x = 9
integer y = 28
integer width = 2670
integer height = 1184
integer taborder = 10
string dataobject = "d_pa_book_assign_labelcopy"
boolean hscrollbar = true
boolean hsplitscroll = true
boolean livescroll = false
boolean ib_isupdateable = false
boolean ib_rmbmenu = false
boolean ib_rmbfocuschange = false
string is_updatesallowed = ""
end type

event pfc_hinttext;//string ls_object, ls_column, ls_column_tag
//long ll_pos
//
////This script set's microhelp at the bottom of the screen dw_vol_selection Datawindow
//ls_object = THIS.getobjectatpointer()
//ll_pos = pos(ls_object, "~t")
//IF NOT pos(ls_object, "_t~t") > 0 THEN
//	IF ll_pos > 0 THEN
//		ll_pos = ll_pos -1
//		ls_column = mid(ls_object,1,ll_pos)
//		ls_column_tag = THIS.Describe(ls_column + ".tag")
//		w_pics_main.setmicrohelp(ls_column_tag)
//	ELSE
//		w_pics_main.setmicrohelp("Ready")
//	END IF
//END IF
end event

event ue_enterkey;//Send(Handle(dw_select_deselect),256,9,Long(0,0))
//return(1)
end event

event clicked;call super::clicked;

this.SelectRow(0,false)
this.SelectRow(row,true)
this.ScrollToRow(row)
this.SetRow(row)
this.SetFocus()
end event

event constructor;call super::constructor;ib_rmbmenu = FALSE


end event

event getfocus;call super::getfocus;m_pics_main.m_Edit.m_Addrow.Enabled = FALSE
m_pics_main.m_Edit.m_Deleterow.Enabled = FALSE
m_pics_main.m_Edit.m_Cut.Enabled = FALSE
end event

event itemchanged;//long li_row_count, li_count=0,i
//string  ls_cld
//
////messagebox('','itemchanged')
//
//if dwo.name ='include1' then
//	ls_cld=data
//	dw_distribution_schedule_null_incld_cpy.object.include1[row]= ls_cld
//end if
//li_row_count=dw_distribution_schedule_null_incld_cpy.RowCount()
//for i=1 to li_row_count
//	ls_cld=dw_distribution_schedule_null_incld_cpy.object.include1[row]
//	if ls_cld='Y' then
////		dw_distribution_schedule_null_incld_cpy.object.dsdt[row]= ld_dsdt
//		li_count++
//		
//	end if
//next
//sle_book#.text=string(li_count)
end event

event retrieveend;call super::retrieveend;integer i,j
long Lbkseq
string Lbkmed,Lcntr
date Lschstdt,Lactstdt,Lactenddt
//

	close(w_pics_retrieve_msg_box)

end event

event retrievestart;//OpenWithParm(w_pics_retrieve_msg_box,"Retrieving records, Please wait...")

end event

event rowfocuschanged;call super::rowfocuschanged;this.SelectRow(0,false)
this.SelectRow(currentrow,true)
//this.ScrollToRow(row)
//this.SetRow(row)
//this.SetFocus()
end event

event sqlpreview;call super::sqlpreview;//MessageBox("sql",sqlsyntax)
end event

event updateend;call super::updateend;Close(w_pics_update_msg_box)
end event

event updatestart;call super::updatestart;Open(w_pics_update_msg_box)
end event

event pfc_retrieve;//RETURN THIS.Retrieve()
return 1

end event

type cb_cancel from u_cb within w_pa_assign_books_label
event pfc_hinttext pbm_mousemove
string tag = "Exits the screen"
integer x = 2455
integer y = 1244
integer width = 238
integer taborder = 100
fontcharset fontcharset = ansi!
string text = "&Cancel"
end type

event pfc_hinttext;call super::pfc_hinttext;w_pics_main.setmicrohelp(This.tag)
end event

event clicked;call super::clicked;parent.Event pfc_close()
m_pics_main.m_menu.PopMenu(300, 0)

end event


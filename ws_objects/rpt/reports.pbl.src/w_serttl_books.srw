$PBExportHeader$w_serttl_books.srw
forward
global type w_serttl_books from w_sheet
end type
type rb_ln_filter from u_rb within w_serttl_books
end type
type st_3 from statictext within w_serttl_books
end type
type rb_bylastname from u_rb within w_serttl_books
end type
type rb_bytitle from u_rb within w_serttl_books
end type
type st_keyorln from statictext within w_serttl_books
end type
type sle_keyword from u_sle within w_serttl_books
end type
type rb_wildcard from u_rb within w_serttl_books
end type
type rb_stwith from u_rb within w_serttl_books
end type
type st_2 from statictext within w_serttl_books
end type
type sle_book# from u_sle within w_serttl_books
end type
type cb_print from u_cb within w_serttl_books
end type
type cb_cancel from u_cb within w_serttl_books
end type
type cb_all from u_cb within w_serttl_books
end type
type dw_series_title_book from u_dw within w_serttl_books
end type
type gb_1 from groupbox within w_serttl_books
end type
type dw_2 from u_dw within w_serttl_books
end type
type gb_2 from groupbox within w_serttl_books
end type
end forward

global type w_serttl_books from w_sheet
integer width = 2757
string title = "Series Title Report"
long backcolor = 81576884
rb_ln_filter rb_ln_filter
st_3 st_3
rb_bylastname rb_bylastname
rb_bytitle rb_bytitle
st_keyorln st_keyorln
sle_keyword sle_keyword
rb_wildcard rb_wildcard
rb_stwith rb_stwith
st_2 st_2
sle_book# sle_book#
cb_print cb_print
cb_cancel cb_cancel
cb_all cb_all
dw_series_title_book dw_series_title_book
gb_1 gb_1
dw_2 dw_2
gb_2 gb_2
end type
global w_serttl_books w_serttl_books

type variables
//str_distrib_schedule istr_add
//string is_add='N'
end variables

on w_serttl_books.create
int iCurrent
call super::create
this.rb_ln_filter=create rb_ln_filter
this.st_3=create st_3
this.rb_bylastname=create rb_bylastname
this.rb_bytitle=create rb_bytitle
this.st_keyorln=create st_keyorln
this.sle_keyword=create sle_keyword
this.rb_wildcard=create rb_wildcard
this.rb_stwith=create rb_stwith
this.st_2=create st_2
this.sle_book#=create sle_book#
this.cb_print=create cb_print
this.cb_cancel=create cb_cancel
this.cb_all=create cb_all
this.dw_series_title_book=create dw_series_title_book
this.gb_1=create gb_1
this.dw_2=create dw_2
this.gb_2=create gb_2
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.rb_ln_filter
this.Control[iCurrent+2]=this.st_3
this.Control[iCurrent+3]=this.rb_bylastname
this.Control[iCurrent+4]=this.rb_bytitle
this.Control[iCurrent+5]=this.st_keyorln
this.Control[iCurrent+6]=this.sle_keyword
this.Control[iCurrent+7]=this.rb_wildcard
this.Control[iCurrent+8]=this.rb_stwith
this.Control[iCurrent+9]=this.st_2
this.Control[iCurrent+10]=this.sle_book#
this.Control[iCurrent+11]=this.cb_print
this.Control[iCurrent+12]=this.cb_cancel
this.Control[iCurrent+13]=this.cb_all
this.Control[iCurrent+14]=this.dw_series_title_book
this.Control[iCurrent+15]=this.gb_1
this.Control[iCurrent+16]=this.dw_2
this.Control[iCurrent+17]=this.gb_2
end on

on w_serttl_books.destroy
call super::destroy
destroy(this.rb_ln_filter)
destroy(this.st_3)
destroy(this.rb_bylastname)
destroy(this.rb_bytitle)
destroy(this.st_keyorln)
destroy(this.sle_keyword)
destroy(this.rb_wildcard)
destroy(this.rb_stwith)
destroy(this.st_2)
destroy(this.sle_book#)
destroy(this.cb_print)
destroy(this.cb_cancel)
destroy(this.cb_all)
destroy(this.dw_series_title_book)
destroy(this.gb_1)
destroy(this.dw_2)
destroy(this.gb_2)
end on

event closequery;////////////////////////////////////////////////////////////////////////////////
////
////	Event:  closequery
////
////	Description:
////	Search for unsaved datawindows prompting the user if any
////	pending updates are found.
////
////////////////////////////////////////////////////////////////////////////////
////	
////	Revision History
////
////	Version
////	5.0   Initial version
////
////////////////////////////////////////////////////////////////////////////////
////
////	Copyright © 1996-1997 Sybase, Inc. and its subsidiaries.  All rights reserved.
////	Any distribution of the PowerBuilder Foundation Classes (PFC)
////	source code by other than Sybase, Inc. and its subsidiaries is prohibited.
////
////////////////////////////////////////////////////////////////////////////////
//
//Integer	li_pendingrc
//Integer	li_validationrc
//Integer	li_accepttextrc
//Integer	li_msg
//Integer	li_rc,rtn
//String	ls_msgparms[]
//
//// Check if the CloseQuery process has been disabled
//If ib_disableclosequery Then
//	Return 0
//End If
//
//// Call event to perform any pre-CloseQuery processing
//If This.Event pfc_preclose ( ) <> 1 Then
//	// Prevent the window from closing
//	Return 1  
//End If
//
//// Prevent validation error messages from appearing while the window is closing
//// and allow others to check if the  CloseQuery process is in progress
//ib_closestatus = True
//
//// Check for any pending updates
//li_rc = of_UpdateChecks()
//If li_rc = 0 Then
//	// Updates are NOT pending, allow the window to be closed.
//	Return 0
//ElseIf li_rc < 0 Then
//	// There are Updates pending, but at least one data entry error was found.
//	// Give the user an opportunity to close the window without saving changes
//	If IsValid(gnv_app.inv_error) Then
//		li_msg = gnv_app.inv_error.of_Message('pfc_closequery_failsvalidation', &
//					 ls_msgparms, gnv_app.iapp_object.DisplayName)
//	Else
//		li_msg = MessageBox (gnv_app.iapp_object.DisplayName, &
//					"The information entered does not pass validation and "  + &
//					"must be corrected before changes can be saved.~r~n~r~n" + &
//					"Close without saving changes?", &
//					exclamation!, YesNo!, 2)
//					dw_select_deselect.Setfocus()
//	End If
//	If li_msg = 1 Then
//		Return 0
//	End If
//Else
//	// Changes are pending, prompt the user to determine if they should be saved
//	If IsValid(gnv_app.inv_error) Then
//		li_msg = gnv_app.inv_error.of_Message('pfc_closequery_savechanges',  &
//					ls_msgparms, gnv_app.iapp_object.DisplayName)		
//	Else
//		li_msg = MessageBox ( gnv_app.iapp_object.DisplayName, &
//					"Do you want to save changes?", exclamation!, YesNoCancel!)
//	End If
//	Choose Case li_msg
//		Case 1
//			// YES - Update
//			// If the update fails, prevent the window from closing
//			rtn = cb_update.Event Clicked()
//			IF rtn = 1 THEN
//				RETURN 0
//			END IF
////			If This.Event pfc_save() >= 1 Then
////				// Successful update, allow the window to be closed
////				Return 0
////			End If
//		Case 2
//			// NO - Allow the window to be closed without saving changes
//			Return 0
//		Case 3
//			dw_select_deselect.Setfocus()
//			// CANCEL -  Prevent the window from closing
//	End Choose
//End If
//
//// Prevent the window from closing
//ib_closestatus = False
//Return 1
end event

event mousemove;call super::mousemove;w_pics_main.setmicrohelp("Ready")
end event

event open;call super::open;
THIS.Windowstate = maximized!


end event

event pfc_postopen;call super::pfc_postopen;long i,li_row_count, i2
string ls_serttl, ls_sort

m_pics_main.m_Edit.m_Addrow.Enabled = FALSE
m_pics_main.m_Edit.m_Deleterow.Enabled = FALSE
m_pics_main.m_Edit.m_Cut.Enabled = FALSE
rb_bytitle.checked=true
dw_series_title_book.SetTransObject(SqlServerTrans)
dw_series_title_book.Retrieve()
li_row_count= dw_series_title_book.RowCount()
for i=1 to li_row_count
	ls_serttl=dw_series_title_book.GetItemString(i, 'serttl')
	ls_serttl=trim(ls_serttl)
	ls_serttl=Upper(ls_serttl)
	dw_series_title_book.SetItem(i,'ttl', ls_serttl)
next

if rb_bytitle.checked=true then
	ls_sort="ttl A, bkno A, conno A, prdr A"
elseif rb_bylastname.checked=true then
	ls_sort="auth A, bkno A, conno , prdr A"
end if
dw_series_title_book.SetSort(ls_sort)
dw_series_title_book.Sort()
li_row_count=dw_series_title_book.RowCount()
if li_row_count >0 then
	cb_print.enabled=true
else
	cb_print.enabled=false
end if
dw_series_title_book.SelectRow(0,false)
dw_series_title_book.SelectRow(1,true)
dw_series_title_book.ScrollToRow(1)
dw_series_title_book.SetRow(1)
dw_series_title_book.SetFocus()
sle_book#.Text=string(li_row_count)
cb_all.enabled=false
cb_print.enabled=false


end event

event pfc_preopen;call super::pfc_preopen;this.of_SetPreference(TRUE)
this.inv_preference.of_SetToolBars(TRUE)
this.inv_preference.of_SetWindow(TRUE)
this.of_SetResize(TRUE)
this.inv_resize.of_SetOrigSize(this.workspacewidth(),this.workspaceheight())
inv_resize.of_Register(cb_cancel, "scale")
inv_resize.of_Register(gb_1, "scale")
inv_resize.of_Register(gb_2, "scale")
inv_resize.of_Register(cb_all, "scale")
inv_resize.of_Register(rb_ln_filter, "scale")
inv_resize.of_Register(dw_series_title_book, "scale")
inv_resize.of_Register(dw_2, "scale")
inv_resize.of_Register(cb_print, "scale")

inv_resize.of_Register(rb_bylastname, "scale")

inv_resize.of_Register(rb_bytitle, "scale")
inv_resize.of_Register(st_keyorln, "scale")
inv_resize.of_Register(st_3, "scale")
inv_resize.of_Register(st_2, "scale")
inv_resize.of_Register(rb_stwith, "scale")
inv_resize.of_Register(rb_wildcard, "scale")
inv_resize.of_Register(sle_book#, "scale")
inv_resize.of_Register(sle_keyword, "scale")




end event

event resize;call super::resize;//long ll_height
//
//This.X = w_pics_main.X
//This.Y = w_pics_main.Y
//ll_height = w_pics_main.mdi_1.Height
//This.Resize(w_pics_main.WorkSpaceWidth(), ll_height)
end event

type rb_ln_filter from u_rb within w_serttl_books
integer x = 997
integer y = 144
integer width = 535
integer height = 72
string text = "Last Name as Filter"
end type

event clicked;call super::clicked;cb_all.enabled=true
string ls_filter, ls_key, ls_sort
long li_row_count

dw_series_title_book.visible=true
dw_2.visible=false
ls_key= sle_keyword.text
//ls_key= Upper(ls_key)
if IsNull(ls_key) or ls_key='' then
	messagebox('','You should enter the last name first.')
	sle_keyword.SetFocus()
	rb_stwith.checked=false
	rb_wildcard.checked= false
	return
end if
if rb_stwith.checked=true then
	ls_filter="ttl>= '"+ls_key+"'"
elseif rb_wildcard.checked= true then
	ls_filter= "ttl like '"+"%"+ls_key+"%"+"'"
elseif rb_ln_filter.checked=true then
	ls_filter="auth ='"+ls_key+"'"
end if
if rb_bytitle.checked=true then
	ls_sort="ttl A, bkno A, conno A, prdr A"
elseif rb_bylastname.checked=true then
	ls_sort="auth A, bkno A, conno , prdr A"
end if
dw_series_title_book.SetFilter(ls_filter)
dw_series_title_book.Filter()
dw_series_title_book.SetSort(ls_sort)
dw_series_title_book.Sort()
li_row_count=dw_series_title_book.RowCount()
if li_row_count >0 then
	cb_print.enabled=true
else
	cb_print.enabled=false
end if
sle_book#.text=string(li_row_count)
dw_series_title_book.SelectRow(0,false)
dw_series_title_book.SelectRow(1,true)
dw_series_title_book.ScrollToRow(1)
dw_series_title_book.SetRow(1)
dw_series_title_book.SetFocus()
end event

type st_3 from statictext within w_serttl_books
integer x = 78
integer y = 100
integer width = 187
integer height = 52
integer textsize = -8
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 33554432
long backcolor = 80263581
string text = "Sort by:"
boolean focusrectangle = false
end type

type rb_bylastname from u_rb within w_serttl_books
integer x = 279
integer y = 132
integer width = 599
integer height = 92
string text = "Author Last Name"
end type

event clicked;call super::clicked;string ls_sort

if rb_bytitle.checked=true then
	ls_sort="ttl A, bkno A, conno A, prdr A"
elseif rb_bylastname.checked=true then
	ls_sort="auth A, bkno A, conno , prdr A"
end if
dw_series_title_book.SetSort(ls_sort)
dw_series_title_book.Sort()
dw_series_title_book.SelectRow(0,false)
dw_series_title_book.SelectRow(1,true)
dw_series_title_book.ScrollToRow(1)
dw_series_title_book.SetRow(1)
dw_series_title_book.SetFocus()
end event

type rb_bytitle from u_rb within w_serttl_books
integer x = 279
integer y = 48
integer width = 375
integer height = 68
string text = "Series Title"
end type

event clicked;call super::clicked;string ls_sort

if rb_bytitle.checked=true then
	ls_sort="ttl A, bkno A, conno A, prdr A"
elseif rb_bylastname.checked=true then
	ls_sort="auth A, bkno A, conno A, prdr A"
end if
dw_series_title_book.SetSort(ls_sort)
dw_series_title_book.Sort()

dw_series_title_book.SelectRow(0,false)
dw_series_title_book.SelectRow(1,true)
dw_series_title_book.ScrollToRow(1)
dw_series_title_book.SetRow(1)
dw_series_title_book.SetFocus()
end event

type st_keyorln from statictext within w_serttl_books
integer x = 1125
integer y = 60
integer width = 777
integer height = 64
integer textsize = -8
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 33554432
long backcolor = 67108864
string text = "Series Title Key Word or Last Name"
boolean focusrectangle = false
end type

type sle_keyword from u_sle within w_serttl_books
integer x = 1957
integer y = 56
integer width = 503
integer height = 76
integer taborder = 10
end type

type rb_wildcard from u_rb within w_serttl_books
integer x = 1646
integer y = 140
integer width = 480
integer height = 72
string text = "Wild Card Match"
end type

event clicked;call super::clicked;cb_all.enabled=true
string ls_filter, ls_key, ls_sort
long li_row_count

dw_series_title_book.visible=true
dw_2.visible=false
ls_key= sle_keyword.text
ls_key= Upper(ls_key)
if IsNull(ls_key) or ls_key='' then
	messagebox('','You should enter the series title key word first.')
	sle_keyword.SetFocus()
	rb_stwith.checked=false
	rb_wildcard.checked= false
	return
end if
if rb_stwith.checked=true then
	ls_filter="ttl>= '"+ls_key+"'"
elseif rb_wildcard.checked= true then
	ls_filter= "ttl like '"+"%"+ls_key+"%"+"'"
end if
if rb_bytitle.checked=true then
	ls_sort="ttl A, bkno A, conno A, prdr A"
elseif rb_bylastname.checked=true then
	ls_sort="auth A, bkno A, conno , prdr A"
end if
dw_series_title_book.SetFilter(ls_filter)
dw_series_title_book.Filter()
dw_series_title_book.SetSort(ls_sort)
dw_series_title_book.Sort()
li_row_count=dw_series_title_book.RowCount()
if li_row_count >0 then
	cb_print.enabled=true
else
	cb_print.enabled=false
end if
sle_book#.text=string(li_row_count)
dw_series_title_book.SelectRow(0,false)
dw_series_title_book.SelectRow(1,true)
dw_series_title_book.ScrollToRow(1)
dw_series_title_book.SetRow(1)
dw_series_title_book.SetFocus()
end event

type rb_stwith from u_rb within w_serttl_books
integer x = 2249
integer y = 144
integer width = 334
integer height = 68
string text = "Starts With"
end type

event clicked;call super::clicked;cb_all.enabled=true
string ls_filter, ls_key, ls_sort
long li_row_count

dw_series_title_book.visible=true
dw_2.visible=false
ls_key= sle_keyword.text
ls_key= Upper(ls_key)
if IsNull(ls_key) or ls_key='' then
	messagebox('','You should enter the series title key word first.')
	sle_keyword.SetFocus()
	rb_stwith.checked=false
	rb_wildcard.checked= false
	return
end if
if rb_stwith.checked=true then
	ls_filter="ttl>= '"+ls_key+"'"
elseif rb_wildcard.checked= true then
	ls_filter= "ttl like '"+"%"+ls_key+"%"+"'"
end if
if rb_bytitle.checked=true then
	ls_sort="ttl A, bkno A, conno A, prdr A"
elseif rb_bylastname.checked=true then
	ls_sort="auth A, bkno A, conno , prdr A"
end if
dw_series_title_book.SetFilter(ls_filter)
dw_series_title_book.Filter()
dw_series_title_book.SetSort(ls_sort)
dw_series_title_book.Sort()
li_row_count=dw_series_title_book.RowCount()
if li_row_count >0 then
	cb_print.enabled=true
else
	cb_print.enabled=false
end if
sle_book#.text=string(li_row_count)
dw_series_title_book.SelectRow(0,false)
dw_series_title_book.SelectRow(1,true)
dw_series_title_book.ScrollToRow(1)
dw_series_title_book.SetRow(1)
dw_series_title_book.SetFocus()
end event

type st_2 from statictext within w_serttl_books
integer x = 133
integer y = 1236
integer width = 498
integer height = 52
integer textsize = -8
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 33554432
long backcolor = 67108864
string text = "Total Books Included:"
boolean focusrectangle = false
end type

type sle_book# from u_sle within w_serttl_books
integer x = 658
integer y = 1232
integer width = 265
integer height = 76
integer taborder = 0
end type

type cb_print from u_cb within w_serttl_books
event pfc_hinttext pbm_mousemove
string tag = "Save data to oracle data base"
integer x = 1193
integer y = 1232
integer width = 169
integer taborder = 50
fontcharset fontcharset = ansi!
string text = "&Print"
end type

event pfc_hinttext;call super::pfc_hinttext;w_pics_main.setmicrohelp(This.tag)
end event

event clicked;call super::clicked;nvo_PowerPrn.of_SetPrinterOrientation(2)
dw_series_title_book.TriggerEvent('pfc_print')
nvo_PowerPrn.of_SetPrinterOrientation(1)
end event

type cb_cancel from u_cb within w_serttl_books
event pfc_hinttext pbm_mousemove
string tag = "Exits the screen"
integer x = 2245
integer y = 1232
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

type cb_all from u_cb within w_serttl_books
integer x = 1719
integer y = 1232
integer width = 261
integer taborder = 80
fontcharset fontcharset = ansi!
string text = "&Show All"
end type

event clicked;call super::clicked;string ls_sort, ls_filter=''
long li_row_count

sle_keyword.text=''
rb_stwith.checked=false
rb_wildcard.checked =false
rb_bytitle.checked=true
if rb_bytitle.checked=true then
	ls_sort="ttl A, bkno A, conno A, prdr A"
elseif rb_bylastname.checked=true then
	ls_sort="auth A, bkno A, conno , prdr A"
end if
dw_series_title_book.SetFilter(ls_filter)
dw_series_title_book.Filter()
dw_series_title_book.SetSort(ls_sort)
dw_series_title_book.Sort()
li_row_count=dw_series_title_book.RowCount()
if li_row_count >0 then
	cb_print.enabled=true
else
	cb_print.enabled=false
end if
sle_book#.text=string(li_row_count)
dw_series_title_book.SelectRow(0,false)
dw_series_title_book.SelectRow(1,true)
dw_series_title_book.ScrollToRow(1)
dw_series_title_book.SetRow(1)
dw_series_title_book.SetFocus()
end event

type dw_series_title_book from u_dw within w_serttl_books
event pfc_hinttext pbm_mousemove
event ue_enterkey pbm_dwnprocessenter
event pfc_keydown pbm_dwnkey
integer x = 14
integer y = 292
integer width = 2670
integer height = 892
integer taborder = 50
string dataobject = "d_series_title_book_proc"
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

event retrievestart;call super::retrievestart;OpenWithParm(w_pics_retrieve_msg_box,"Retrieving records, Please wait...")

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

event pfc_retrieve;call super::pfc_retrieve;RETURN THIS.Retrieve()

end event

event clicked;call super::clicked;

this.SelectRow(0,false)
this.SelectRow(row,true)
this.ScrollToRow(row)
this.SetRow(row)
this.SetFocus()
end event

type gb_1 from groupbox within w_serttl_books
integer x = 50
integer y = 4
integer width = 837
integer height = 232
integer taborder = 10
integer textsize = -8
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 33554432
long backcolor = 80263581
borderstyle borderstyle = stylelowered!
end type

type dw_2 from u_dw within w_serttl_books
event pfc_hinttext pbm_mousemove
event ue_enterkey pbm_dwnprocessenter
event pfc_keydown pbm_dwnkey
integer x = 23
integer y = 296
integer width = 2670
integer height = 892
integer taborder = 60
string dataobject = "d_series_title_book_proc"
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

event clicked;call super::clicked;//messagebox('','clicked') 
//if dwo.name= 'bkseq' then 
//end if
//	
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

event retrievestart;call super::retrievestart;OpenWithParm(w_pics_retrieve_msg_box,"Retrieving records, Please wait...")

end event

event rowfocuschanged;call super::rowfocuschanged;//long li_row_count, li_count2, i
//string ls_incld
//
////this.SelectRow(0,false)
////this.SelectRow(currentrow,true)
////messagebox('','rowfocuschanged')
//li_row_count=this.RowCount()
//for i=1 to li_row_count
//	ls_incld=this.GetItemString(i,'include1')
//	if ls_incld='Y' then li_count2++
//next
//
//sle_book#.text=string(li_count2)
end event

event sqlpreview;call super::sqlpreview;//MessageBox("sql",sqlsyntax)
end event

event updateend;call super::updateend;Close(w_pics_update_msg_box)
end event

event updatestart;call super::updatestart;Open(w_pics_update_msg_box)
end event

event pfc_retrieve;call super::pfc_retrieve;RETURN THIS.Retrieve()

end event

type gb_2 from groupbox within w_serttl_books
integer x = 942
integer y = 4
integer width = 1728
integer height = 232
integer taborder = 20
integer textsize = -8
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 33554432
long backcolor = 80263581
borderstyle borderstyle = stylelowered!
end type


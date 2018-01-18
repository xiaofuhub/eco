$PBExportHeader$w_books_query.srw
forward
global type w_books_query from w_sheet
end type
type sle_cntrno from singlelineedit within w_books_query
end type
type st_cntrno from statictext within w_books_query
end type
type cb_title from u_cb within w_books_query
end type
type cb_update from u_cb within w_books_query
end type
type cb_detail from u_cb within w_books_query
end type
type cb_clear from u_cb within w_books_query
end type
type sle_bkmed from u_sle within w_books_query
end type
type st_conno from statictext within w_books_query
end type
type st_serttl from statictext within w_books_query
end type
type st_bkno from statictext within w_books_query
end type
type st_ttl from statictext within w_books_query
end type
type st_fn from statictext within w_books_query
end type
type st_ln from statictext within w_books_query
end type
type sle_serttl from u_sle within w_books_query
end type
type sle_conno from u_sle within w_books_query
end type
type sle_bkseq from u_sle within w_books_query
end type
type sle_ttl from u_sle within w_books_query
end type
type sle_fn from u_sle within w_books_query
end type
type gb_2 from groupbox within w_books_query
end type
type rb_auth from u_rb within w_books_query
end type
type rb_conno from u_rb within w_books_query
end type
type rb_keyword from u_rb within w_books_query
end type
type rb_soundex from u_rb within w_books_query
end type
type rb_match from u_rb within w_books_query
end type
type rb_title from u_rb within w_books_query
end type
type rb_bkno from u_rb within w_books_query
end type
type sle_ln from u_sle within w_books_query
end type
type st_total from statictext within w_books_query
end type
type sle_book# from u_sle within w_books_query
end type
type cb_print from u_cb within w_books_query
end type
type cb_cancel from u_cb within w_books_query
end type
type cb_search from u_cb within w_books_query
end type
type gb_1 from groupbox within w_books_query
end type
type dw_book_query from u_dw within w_books_query
end type
type dw_1 from u_dw within w_books_query
end type
end forward

global type w_books_query from w_sheet
integer width = 2757
string title = "Books Query Screen"
long backcolor = 81576884
sle_cntrno sle_cntrno
st_cntrno st_cntrno
cb_title cb_title
cb_update cb_update
cb_detail cb_detail
cb_clear cb_clear
sle_bkmed sle_bkmed
st_conno st_conno
st_serttl st_serttl
st_bkno st_bkno
st_ttl st_ttl
st_fn st_fn
st_ln st_ln
sle_serttl sle_serttl
sle_conno sle_conno
sle_bkseq sle_bkseq
sle_ttl sle_ttl
sle_fn sle_fn
gb_2 gb_2
rb_auth rb_auth
rb_conno rb_conno
rb_keyword rb_keyword
rb_soundex rb_soundex
rb_match rb_match
rb_title rb_title
rb_bkno rb_bkno
sle_ln sle_ln
st_total st_total
sle_book# sle_book#
cb_print cb_print
cb_cancel cb_cancel
cb_search cb_search
gb_1 gb_1
dw_book_query dw_book_query
dw_1 dw_1
end type
global w_books_query w_books_query

type variables
//str_distrib_schedule istr_add
//string is_add='N'
string is_ogn_slt
long i_count=0, i_cntclick
string bkquery_conno
end variables

forward prototypes
public subroutine of_charcheck (ref string as_string)
public subroutine wf_endpart (long li_row_count)
end prototypes

public subroutine of_charcheck (ref string as_string);//////////////////////////////////////////////////////////////////////////////////////////////////////////
//
//	Function :  of_charcheck
// Args		: string as_string
//	Description:
//	Replace apostrophe with escape chars 
//
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//	
//	Revision History
//
//	Developed by 	Date 				Version						Tracking#
//									
// Murali K.			03/05/2008      									 #2061
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//

long ll_start_pos=1
string ls_old_str, ls_new_str, ls_string

ls_string = as_string
ls_old_str = "'"
ls_new_str = "~~'~~'"

// Find the first occurrence of ls_old_str.
ll_start_pos = Pos(ls_string, ls_old_str, ll_start_pos)

// Only enter the loop if you find ls_old_str.
DO WHILE ll_start_pos > 0

    // Replace ls_old_str with ls_new_str.

    ls_string = Replace(ls_string, ll_start_pos, &
      Len(ls_old_str), ls_new_str)

    // Find the next occurrence of ls_old_str.

    ll_start_pos = Pos(ls_string, ls_old_str, &
      ll_start_pos+Len(ls_new_str))

LOOP
as_string = ls_string // passed back as reference


end subroutine

public subroutine wf_endpart (long li_row_count);string ls_sort
if rb_title.checked=true then
	ls_sort="ttl A, bkno A, conno A"
elseif rb_auth.checked=true then
	ls_sort="auth A, bkno A, conno A"
elseif rb_conno.checked=true then
	ls_sort="conno A, bkno A, ttl A"
elseif rb_bkno.checked=true then
	ls_sort="bkno A, conno A, ttl A"
end if
dw_book_query.SetSort(ls_sort)
dw_book_query.Sort()
dw_book_query.SelectRow(0,false)
dw_book_query.SelectRow(1,true)
dw_book_query.ScrollToRow(1)
dw_book_query.SetRow(1)
dw_book_query.SetFocus()
sle_book#.text=string(li_row_count)
if li_row_count >0 then
	cb_print.enabled=true
	cb_clear.enabled=true
	cb_detail.enabled=true
	cb_title.enabled=TRUE
else
	cb_print.enabled=false
	cb_clear.enabled=false
	cb_detail.enabled=false
	cb_title.enabled=FALSE
end if
cb_clear.enabled=TRUE
//////////////

end subroutine

on w_books_query.create
int iCurrent
call super::create
this.sle_cntrno=create sle_cntrno
this.st_cntrno=create st_cntrno
this.cb_title=create cb_title
this.cb_update=create cb_update
this.cb_detail=create cb_detail
this.cb_clear=create cb_clear
this.sle_bkmed=create sle_bkmed
this.st_conno=create st_conno
this.st_serttl=create st_serttl
this.st_bkno=create st_bkno
this.st_ttl=create st_ttl
this.st_fn=create st_fn
this.st_ln=create st_ln
this.sle_serttl=create sle_serttl
this.sle_conno=create sle_conno
this.sle_bkseq=create sle_bkseq
this.sle_ttl=create sle_ttl
this.sle_fn=create sle_fn
this.gb_2=create gb_2
this.rb_auth=create rb_auth
this.rb_conno=create rb_conno
this.rb_keyword=create rb_keyword
this.rb_soundex=create rb_soundex
this.rb_match=create rb_match
this.rb_title=create rb_title
this.rb_bkno=create rb_bkno
this.sle_ln=create sle_ln
this.st_total=create st_total
this.sle_book#=create sle_book#
this.cb_print=create cb_print
this.cb_cancel=create cb_cancel
this.cb_search=create cb_search
this.gb_1=create gb_1
this.dw_book_query=create dw_book_query
this.dw_1=create dw_1
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.sle_cntrno
this.Control[iCurrent+2]=this.st_cntrno
this.Control[iCurrent+3]=this.cb_title
this.Control[iCurrent+4]=this.cb_update
this.Control[iCurrent+5]=this.cb_detail
this.Control[iCurrent+6]=this.cb_clear
this.Control[iCurrent+7]=this.sle_bkmed
this.Control[iCurrent+8]=this.st_conno
this.Control[iCurrent+9]=this.st_serttl
this.Control[iCurrent+10]=this.st_bkno
this.Control[iCurrent+11]=this.st_ttl
this.Control[iCurrent+12]=this.st_fn
this.Control[iCurrent+13]=this.st_ln
this.Control[iCurrent+14]=this.sle_serttl
this.Control[iCurrent+15]=this.sle_conno
this.Control[iCurrent+16]=this.sle_bkseq
this.Control[iCurrent+17]=this.sle_ttl
this.Control[iCurrent+18]=this.sle_fn
this.Control[iCurrent+19]=this.gb_2
this.Control[iCurrent+20]=this.rb_auth
this.Control[iCurrent+21]=this.rb_conno
this.Control[iCurrent+22]=this.rb_keyword
this.Control[iCurrent+23]=this.rb_soundex
this.Control[iCurrent+24]=this.rb_match
this.Control[iCurrent+25]=this.rb_title
this.Control[iCurrent+26]=this.rb_bkno
this.Control[iCurrent+27]=this.sle_ln
this.Control[iCurrent+28]=this.st_total
this.Control[iCurrent+29]=this.sle_book#
this.Control[iCurrent+30]=this.cb_print
this.Control[iCurrent+31]=this.cb_cancel
this.Control[iCurrent+32]=this.cb_search
this.Control[iCurrent+33]=this.gb_1
this.Control[iCurrent+34]=this.dw_book_query
this.Control[iCurrent+35]=this.dw_1
end on

on w_books_query.destroy
call super::destroy
destroy(this.sle_cntrno)
destroy(this.st_cntrno)
destroy(this.cb_title)
destroy(this.cb_update)
destroy(this.cb_detail)
destroy(this.cb_clear)
destroy(this.sle_bkmed)
destroy(this.st_conno)
destroy(this.st_serttl)
destroy(this.st_bkno)
destroy(this.st_ttl)
destroy(this.st_fn)
destroy(this.st_ln)
destroy(this.sle_serttl)
destroy(this.sle_conno)
destroy(this.sle_bkseq)
destroy(this.sle_ttl)
destroy(this.sle_fn)
destroy(this.gb_2)
destroy(this.rb_auth)
destroy(this.rb_conno)
destroy(this.rb_keyword)
destroy(this.rb_soundex)
destroy(this.rb_match)
destroy(this.rb_title)
destroy(this.rb_bkno)
destroy(this.sle_ln)
destroy(this.st_total)
destroy(this.sle_book#)
destroy(this.cb_print)
destroy(this.cb_cancel)
destroy(this.cb_search)
destroy(this.gb_1)
destroy(this.dw_book_query)
destroy(this.dw_1)
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

dw_book_query.SetTransObject(SqlServerTrans)

dw_book_query.SelectRow(0,false)
dw_book_query.SelectRow(1,true)
dw_book_query.ScrollToRow(1)
dw_book_query.SetRow(1)
dw_book_query.SetFocus()
sle_book#.Text=string(li_row_count)
cb_print.enabled=false
cb_search.enabled=false
cb_clear.enabled= false
cb_detail.enabled= false
cb_title.enabled=false
rb_bkno.checked=true


end event

event pfc_preopen;call super::pfc_preopen;this.of_SetPreference(TRUE)
this.inv_preference.of_SetToolBars(TRUE)
this.inv_preference.of_SetWindow(TRUE)
this.of_SetResize(TRUE)
this.inv_resize.of_SetOrigSize(this.workspacewidth(),this.workspaceheight())
inv_resize.of_Register(cb_cancel, "scale")
inv_resize.of_Register(cb_search, "scale")
inv_resize.of_Register(cb_print, "scale")
inv_resize.of_Register(cb_clear, "scale")
inv_resize.of_Register(cb_detail, "scale")
inv_resize.of_Register(cb_title, "scale")
inv_resize.of_Register(cb_update, "scale")
inv_resize.of_Register(dw_book_query, "scale")
inv_resize.of_Register(dw_1, "scale")
inv_resize.of_Register(gb_1, "scale")
inv_resize.of_Register(gb_2, "scale")
inv_resize.of_Register(rb_auth, "scale")

inv_resize.of_Register(rb_bkno, "scale")

inv_resize.of_Register(rb_conno, "scale")
inv_resize.of_Register(rb_keyword, "scale")
inv_resize.of_Register(rb_match, "scale")
inv_resize.of_Register(rb_soundex, "scale")
inv_resize.of_Register(rb_title, "scale")
inv_resize.of_Register(sle_bkmed, "scale")
inv_resize.of_Register(sle_bkseq, "scale")
inv_resize.of_Register(sle_conno, "scale")
inv_resize.of_Register(sle_fn, "scale")
inv_resize.of_Register(sle_ln, "scale")
inv_resize.of_Register(sle_serttl, "scale")
inv_resize.of_Register(sle_ttl, "scale")
inv_resize.of_Register(sle_book#, "scale")
inv_resize.of_Register(sle_cntrno, "scale")
inv_resize.of_Register(st_bkno, "scale")
inv_resize.of_Register(st_conno, "scale")
inv_resize.of_Register(st_fn, "scale")
inv_resize.of_Register(st_ln, "scale")
inv_resize.of_Register(st_serttl, "scale")
inv_resize.of_Register(st_total, "scale")
inv_resize.of_Register(st_ttl, "scale")
inv_resize.of_Register(st_cntrno, "scale")
//inv_resize.of_Register(gb_1, "scale")
//inv_resize.of_Register(gb_2, "scale")
//inv_resize.of_Register(rb_auth, "scale")
//
//inv_resize.of_Register(rb_bkno, "scale")








end event

event resize;call super::resize;//long ll_height
//
//This.X = w_pics_main.X
//This.Y = w_pics_main.Y
//ll_height = w_pics_main.mdi_1.Height
//This.Resize(w_pics_main.WorkSpaceWidth(), ll_height)
end event

type sle_cntrno from singlelineedit within w_books_query
integer x = 1573
integer y = 512
integer width = 475
integer height = 76
integer taborder = 80
integer textsize = -8
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 33554432
long backcolor = 1090519039
boolean autohscroll = false
textcase textcase = upper!
borderstyle borderstyle = stylelowered!
end type

type st_cntrno from statictext within w_books_query
integer x = 1024
integer y = 512
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
string text = "PCS Contract Number:"
boolean focusrectangle = false
end type

type cb_title from u_cb within w_books_query
event pfc_hinttext pbm_mousemove
string tag = "See the detail of the title information"
integer x = 905
integer y = 1232
integer width = 197
integer taborder = 150
fontcharset fontcharset = ansi!
string text = "&Title"
end type

event pfc_hinttext;w_pics_main.setmicrohelp(This.tag)
end event

event clicked;call super::clicked;
long li_slted
string ls_conno


li_slted=dw_book_query.GetSelectedRow(0)
if li_slted=0 then
	messagebox('Warnning','You must select a row first.')
	return
else
	ls_conno=dw_book_query.GetItemString(li_slted,'conno')
	//OpenSheetwithParm(w_books_query_cds_reports,ls_conno,w_pics_main, 0, Original!)
	bkquery_conno = ls_conno
	OpenSheetwithparm(w_sheet_pics_ole_crystal,"titleinformationlistbycontrolnumber",w_pics_main, 0, Original!)
end if
parent.hide()



//li_slted=dw_book_query.GetSelectedRow(0)
//if li_slted=0 then
//	messagebox('Warnning','You must select a row first.')
//	return
//else
//	ls_conno=dw_book_query.GetItemString(li_slted,'conno')
//	OpenSheetwithParm(w_books_query_results,ls_conno,w_pics_main, 0, Original!)
//end if
end event

type cb_update from u_cb within w_books_query
string tag = "Search the specific records"
boolean visible = false
integer x = 1431
integer y = 1232
integer width = 279
integer taborder = 120
fontcharset fontcharset = ansi!
string text = "&Update"
end type

event clicked;call super::clicked;//cb_all.enabled=true
string ls_bkno, ls_bkseq, ls_conno, ls_bkmed
long li_row_count, li_bkseq
string original_select,where_clause,rc ,order_clause, mod_string, ls_ln, ls_fn,&
		ls_ttl, ls_serttl, ls_sndx_auth, ls_sndx_ttl, ls_sort, ls_syntax, ls_syntaxnew
long li_pos, li_pos2, li_pos3, li_pos4
n_cst_string 	inv_string

dw_1.update()
return

dw_book_query.SetTransObject(SqlServerTrans)
dw_book_query.Retrieve(ls_ttl)
return
//if  i_count=0 then
	
//end if
is_ogn_slt=dw_book_query.Describe("DataWindow.Table.Select")
original_select =	is_ogn_slt
//i_count++
//
ls_ln=upper(sle_ln.text)
ls_fn=upper(sle_fn.text)
ls_ttl=upper(sle_ttl.text)
ls_serttl=upper(sle_serttl.text)
li_pos=Pos(ls_ln,"'")
li_pos2=Pos(ls_fn,"'")
li_pos3=Pos(ls_ttl,"'")
li_pos4=Pos(ls_serttl,"'")
if (li_pos>0 or li_pos2>0 or li_pos3>0 or li_pos4>0)  and rb_soundex.checked=false then
	messagebox('Warning','There is apostrophe in some item, You must use soundex search.')
	rb_soundex.checked=true
	if rb_soundex.checked=true then
		sle_bkmed.visible=false
		sle_bkseq.visible=false
		st_bkno.visible=false
		sle_conno.visible=false
		st_conno.visible=false
		sle_ln.visible=true
		st_ln.visible=true
		sle_fn.visible=false
		st_fn.visible=false
		sle_ttl.visible=true
		st_ttl.visible=true
		sle_serttl.visible=false
		st_serttl.visible=false
	end if
	cb_search.TriggerEvent(clicked!)
	return
end if
if rb_match.checked=true then
	ls_bkmed=sle_bkmed.text
	ls_bkseq=sle_bkseq.text
	ls_bkno=trim(ls_bkmed)+trim(ls_bkseq)
	ls_conno=sle_conno.text
	if ls_bkno<>"" and ls_conno<>"" then
		messagebox('Error','for bkno and conno number ,just one field has a value ',StopSign!)
		sle_bkmed.SetFocus()
		return
	end if
	if ls_bkno<>"" then
		ls_bkmed=sle_bkmed.text
		ls_bkseq=sle_bkseq.text
		if Isnumber(ls_bkseq) then
			li_bkseq=Long(ls_bkseq)
		end if
		where_clause = " AND mchar.bkmed="+ "~"" + ls_bkmed + "~"" +" and bkseq= "+&
				string(li_bkseq)+""
		mod_string = "DataWindow.Table.Select='" + original_select + where_clause +"'"
//		messagebox("mod_string",mod_string)
		rc = dw_book_query.Modify(mod_string)
		IF rc = "" THEN
			li_row_count = dw_book_query.Retrieve()
			IF li_row_count < 1 THEN 
				MessageBox("ERROR", "There is not such bkno in database. Try another one.",StopSign!)
				return
			else
				sle_book#.text=string(li_row_count)
				wf_endpart(li_row_count)
			end if
		else
			messagebox('Error','Select statement is wrong: '+rc,StopSign!)
			return
		end if
	end if
	if ls_conno<>"" then
		where_clause = " AND mchar.conno="+ "~"" + ls_conno + "~"" 
		mod_string = "DataWindow.Table.Select='" + original_select + where_clause +"'"
//		messagebox("mod_string",mod_string)
		rc = dw_book_query.Modify(mod_string)
		IF rc = "" THEN
			li_row_count = dw_book_query.Retrieve()
			IF li_row_count < 1 THEN 
				MessageBox("ERROR", "There is not such conno in database. Try another one.",StopSign!)
				return
			else
				sle_book#.text=string(li_row_count)
				wf_endpart(li_row_count)
			end if
		else
			messagebox('Error','Select statement is wrong: '+rc,StopSign!)
			return
		end if
	end if
	if ls_ln <>"" and ls_ttl <>"" then
		where_clause = " AND upper(auth)= "+"~""+ ls_ln + "~""+" and upper(ttl)= "+&
		"~"" + ls_ttl + "~""
		mod_string = "DataWindow.Table.Select='" + original_select + where_clause +"'"
//		messagebox("mod_string",mod_string)
		rc = dw_book_query.Modify(mod_string)
		IF rc = "" THEN
			li_row_count = dw_book_query.Retrieve()
			IF li_row_count < 1 THEN 
				MessageBox("ERROR", "There is not such last name and ttl in database. Try another one.",StopSign!)
			else
				sle_book#.text=string(li_row_count)
			end if
		else
			messagebox('Error','Select statement is wrong: '+rc,StopSign!)
			return
		end if
	elseif ls_ln<>"" then
		where_clause = " AND upper(auth)= "+"~""+ ls_ln + "~""
		mod_string = "DataWindow.Table.Select='" + original_select + where_clause +"'"
//		messagebox("mod_string",mod_string)
		rc = dw_book_query.Modify(mod_string)
		IF rc = "" THEN
			li_row_count = dw_book_query.Retrieve()
			IF li_row_count < 1 THEN 
				MessageBox("ERROR", "There is not such last name in database. Try another one.",StopSign!)
			else
				sle_book#.text=string(li_row_count)
			end if
		else
			messagebox('Error','Select statement is wrong: '+rc,StopSign!)
			return
		end if
	elseif ls_ttl<>"" then
		where_clause = " AND upper(ttl)= "+"~""+ ls_ttl + "~""
		mod_string = "DataWindow.Table.Select='" + original_select + where_clause +"'"
//		messagebox("mod_string",mod_string)
		rc = dw_book_query.Modify(mod_string)
		IF rc = "" THEN
			li_row_count = dw_book_query.Retrieve()
			IF li_row_count < 1 THEN 
				MessageBox("ERROR", "There is not such ttl in database. Try another one.",StopSign!)
			else
				sle_book#.text=string(li_row_count)
			end if
		else
			messagebox('Error','Select statement is wrong: '+rc,StopSign!)
			return
		end if
	elseif ls_ttl="" and ls_ln="" and ls_bkno="" and ls_conno="" then
		messagebox('Error',"You must enter some fields, Can not "+&
			"leave every thing empty.",StopSign!)
		return
	end if//end if ls_ln<>"" and ls_ttl<>""
end if// end if rb_match.checked=true
if rb_keyword.checked=true then
	if ls_ln="" and ls_fn="" and ls_ttl="" then
		where_clause = " AND upper(serttl) like "+ "~"%" + ls_serttl + "%~"" 
		mod_string = "DataWindow.Table.Select='" + original_select + where_clause +"'"
//		messagebox("mod_string",mod_string)
		rc = dw_book_query.Modify(mod_string)
		IF rc = "" THEN
			li_row_count = dw_book_query.Retrieve()
			IF li_row_count < 1 THEN 
				MessageBox("ERROR", "There is not such serttl in database."+&
				" Try another one.",StopSign!)
			end if
		else
			messagebox('Error','Select statement is wrong: '+rc,StopSign!)
			return
		end if
	//	dw_book_query.dataobject='d_book_query_serttl_lk'
	elseif ls_ln="" and ls_fn="" and ls_serttl="" then
		where_clause = " AND upper(ttl) like "+ "~"%" + ls_ttl + "%~"" 
		mod_string = "DataWindow.Table.Select='" + original_select + where_clause +"'"
//		messagebox("mod_string",mod_string)
		rc = dw_book_query.Modify(mod_string)
		IF rc = "" THEN
			li_row_count = dw_book_query.Retrieve()
			IF li_row_count < 1 THEN 
				MessageBox("ERROR", "There is not such ttl in database. "+&
					"Try another one.",StopSign!)
			end if
		else
			messagebox('Error','Select statement is wrong: '+rc,StopSign!)
			return
		end if
	//	dw_book_query.dataobject='d_book_query_ttl_lk'
	elseif ls_ln="" and ls_ttl="" and ls_serttl="" then
		where_clause = " AND upper(authfn) like "+ "~"%" + ls_fn + "%~"" 
		mod_string = "DataWindow.Table.Select='" + original_select + where_clause +"'"
//		messagebox("mod_string",mod_string)
		rc = dw_book_query.Modify(mod_string)
		IF rc = "" THEN
			li_row_count = dw_book_query.Retrieve()
			IF li_row_count < 1 THEN 
				MessageBox("ERROR", "There is not such first name in database. "+&
					"Try another one.",StopSign!)
			end if
		else
			messagebox('Error','Select statement is wrong: '+rc,StopSign!)
			return
		end if
	//	dw_book_query.dataobject='d_book_query_fn_lk'
	elseif ls_fn="" and ls_ttl="" and ls_serttl="" then
		where_clause = " AND upper(auth) like "+ "~"%" + ls_ln + "%~"" 
		mod_string = "DataWindow.Table.Select='" + original_select + where_clause +"'"
//		messagebox("mod_string",mod_string)
		rc = dw_book_query.Modify(mod_string)
		IF rc = "" THEN
			li_row_count = dw_book_query.Retrieve()
			IF li_row_count < 1 THEN 
				MessageBox("ERROR", "There is not such last name in database. "+&
					"Try another one.",StopSign!)
			end if
		else
			messagebox('Error','Select statement is wrong: '+rc,StopSign!)
			return
		end if
	//	dw_book_query.dataobject='d_book_query_ln_lk'
	elseif ls_ln="" and ls_fn="" then
	//	dw_book_query.dataobject='d_book_query_ttl_serttl_lk'
		where_clause = " AND upper(ttl) like "+ "~"%" + ls_ttl + "%~""+" and upper(serttl) "+&
			"like "+"~"%" + ls_serttl + "%~""
		mod_string = "DataWindow.Table.Select='" + original_select + where_clause +"'"
//		messagebox("mod_string",mod_string)
		rc = dw_book_query.Modify(mod_string)
		IF rc = "" THEN
			li_row_count = dw_book_query.Retrieve()
			IF li_row_count < 1 THEN 
				MessageBox("ERROR", "There is not such ttl and serttl in ttlinit. "+&
					"Try another one.",StopSign!)
			end if
		else
			messagebox('Error','Select statement is wrong: '+rc,StopSign!)
			return
		end if
	elseif ls_ln="" and ls_ttl="" then
	//	dw_book_query.dataobject='d_book_query_fn_serttl_lk'
		where_clause = " AND upper(authfn) like "+ "~"%" + ls_fn + "%~""+" and upper(serttl) "+&
			"like "+"~"%" + ls_serttl + "%~""
		mod_string = "DataWindow.Table.Select='" + original_select + where_clause +"'"
//		messagebox("mod_string",mod_string)
		rc = dw_book_query.Modify(mod_string)
		IF rc = "" THEN
			li_row_count = dw_book_query.Retrieve()
			IF li_row_count < 1 THEN 
				MessageBox("ERROR", "There is not such first name and serttl in database. "+&
					"Try another one.",StopSign!)
			end if
		else
			messagebox('Error','Select statement is wrong: '+rc,StopSign!)
			return
		end if
	elseif ls_fn="" and ls_ttl="" then
	//	dw_book_query.dataobject='d_book_query_ln_serttl_lk'
		where_clause = " AND upper(auth) like "+ "~"%" + ls_ln + "%~""+" and upper(serttl) "+&
			"like "+"~"%" + ls_serttl + "%~""
		mod_string = "DataWindow.Table.Select='" + original_select + where_clause +"'"
//		messagebox("mod_string",mod_string)
		rc = dw_book_query.Modify(mod_string)
		IF rc = "" THEN
			li_row_count = dw_book_query.Retrieve()
			IF li_row_count < 1 THEN 
				MessageBox("ERROR", "There is not such last name and serttl in database. "+&
					"Try another one.",StopSign!)
			end if
		else
			messagebox('Error','Select statement is wrong: '+rc,StopSign!)
			return
		end if
	elseif ls_ln="" and ls_serttl="" then
	//	dw_book_query.dataobject='d_book_query_fn_ttl_lk'
		where_clause = " AND upper(authfn) like "+ "~"%" + ls_fn + "%~""+" and upper(ttl) "+&
			"like "+"~"%" + ls_ttl + "%~""
		mod_string = "DataWindow.Table.Select='" + original_select + where_clause +"'"
//		messagebox("mod_string",mod_string)
		rc = dw_book_query.Modify(mod_string)
		IF rc = "" THEN
			li_row_count = dw_book_query.Retrieve()
			IF li_row_count < 1 THEN 
				MessageBox("ERROR", "There is not such first name and ttl in database. "+&
					"Try another one.",StopSign!)
			end if
		else
			messagebox('Error','Select statement is wrong: '+rc,StopSign!)
			return
		end if
	elseif ls_fn="" and ls_serttl="" then
	//	dw_book_query.dataobject='d_book_query_ln_ttl_lk'
		where_clause = " AND upper(auth) like "+ "~"%" + ls_ln + "%~""+" and upper(ttl) "+&
			"like "+"~"%" + ls_ttl + "%~""
		mod_string = "DataWindow.Table.Select='" + original_select + where_clause +"'"
//		messagebox("mod_string",mod_string)
		rc = dw_book_query.Modify(mod_string)
		IF rc = "" THEN
			li_row_count = dw_book_query.Retrieve()
			IF li_row_count < 1 THEN 
				MessageBox("ERROR", "There is not such last name and ttl in database. "+&
					"Try another one.",StopSign!)
			end if
		else
			messagebox('Error','Select statement is wrong: '+rc,StopSign!)
			return
		end if
	elseif ls_ttl="" and ls_serttl="" then
	//	dw_book_query.dataobject='d_book_query_ln_fn_lk'
		where_clause = " AND upper(auth) like "+ "~"%" + ls_ln + "%~""+" and upper(authfn) "+&
			"like "+"~"%" + ls_fn + "%~""
		mod_string = "DataWindow.Table.Select='" + original_select + where_clause +"'"
//		messagebox("mod_string",mod_string)
		rc = dw_book_query.Modify(mod_string)
		IF rc = "" THEN
			li_row_count = dw_book_query.Retrieve()
			IF li_row_count < 1 THEN 
				MessageBox("ERROR", "There is not such last name and first name"+&
											" in database. Try another one.",StopSign!)
			end if
		else
			messagebox('Error','Select statement is wrong: '+rc,StopSign!)
			return
		end if
	elseif ls_ln="" then
	//	dw_book_query.dataobject='d_book_query_fn_ttl_serttl_lk'
		where_clause = " AND upper(authfn) like "+ "~"%" + ls_fn + "%~""+" and upper(ttl) "+&
		"like "+"~"%" + ls_ttl + "%~""+" and upper(serttl) like "+"~"%" + ls_serttl + "%~""
		mod_string = "DataWindow.Table.Select='" + original_select + where_clause +"'"
//		messagebox("mod_string",mod_string)
		rc = dw_book_query.Modify(mod_string)
		IF rc = "" THEN
			li_row_count = dw_book_query.Retrieve()
			IF li_row_count < 1 THEN 
				MessageBox("ERROR", "There is not such  first name , ttl"+&
							" and serttl in database. Try another one.",StopSign!)
			end if
		else
			messagebox('Error','Select statement is wrong: '+rc,StopSign!)
			return
		end if
	elseif ls_fn="" then
	//	dw_book_query.dataobject='d_book_query_ln_ttl_serttl_lk'
		where_clause = " AND upper(auth) like "+ "~"%" + ls_ln + "%~""+" and upper(ttl) "+&
		"like "+"~"%" + ls_ttl + "%~""+" and upper(serttl) like "+"~"%" + ls_serttl + "%~""
		mod_string = "DataWindow.Table.Select='" + original_select + where_clause +"'"
//		messagebox("mod_string",mod_string)
		rc = dw_book_query.Modify(mod_string)
		IF rc = "" THEN
			li_row_count = dw_book_query.Retrieve()
			IF li_row_count < 1 THEN 
				MessageBox("ERROR", "There is not such last name , ttl"+&
							" and serttl in database. Try another one.",StopSign!)
			end if
		else
			messagebox('Error','Select statement is wrong: '+rc,StopSign!)
			return
		end if
	elseif ls_ttl="" then
	//	dw_book_query.dataobject='d_book_query_ln_fn_serttl_lk'
		where_clause = " AND upper(auth) like "+ "~"%" + ls_ln + "%~""+" and upper(authfn) "+&
		"like "+"~"%" + ls_fn + "%~""+" and upper(serttl) like "+"~"%" + ls_serttl + "%~""
		mod_string = "DataWindow.Table.Select='" + original_select + where_clause +"'"
//		messagebox("mod_string",mod_string)
		rc = dw_book_query.Modify(mod_string)
		IF rc = "" THEN
			li_row_count = dw_book_query.Retrieve()
			IF li_row_count < 1 THEN 
				MessageBox("ERROR", "There is not such last name , first name"+&
							" and serttl in database. Try another one.",StopSign!)
			end if
		else
			messagebox('Error','Select statement is wrong: '+rc,StopSign!)
			return
		end if
	elseif ls_serttl="" then
	//	dw_book_query.dataobject='d_book_query_ln_fn_ttl_lk'
		where_clause = " AND upper(auth) like "+ "~"%" + ls_ln + "%~""+" and upper(authfn) "+&
		"like "+"~"%" + ls_fn + "%~""+" and upper(ttl) like "+"~"%" + ls_ttl + "%~""
		mod_string = "DataWindow.Table.Select='" + original_select + where_clause +"'"
//		messagebox("mod_string",mod_string)
		rc = dw_book_query.Modify(mod_string)
		IF rc = "" THEN
			li_row_count = dw_book_query.Retrieve()
			IF li_row_count < 1 THEN 
				MessageBox("ERROR", "There is not such last name , first name"+&
							" and ttl in database. Try another one.",StopSign!)
			end if
		else
			messagebox('Error','Select statement is wrong: '+rc,StopSign!)
			return
		end if
	else
	//	dw_book_query.dataobject='d_book_query_ln_fn_ttl_serttl_lk'
		where_clause = " AND upper(auth) like "+ "~"%" + ls_ln + "%~""+" and upper(authfn) "+&
		"like "+"~"%" + ls_fn + "%~""+" and upper(ttl) like "+"~"%" + ls_ttl + "%~""+&
		" and upper(serttl) like "+"~"%" + ls_serttl + "%~""
		mod_string = "DataWindow.Table.Select='" + original_select + where_clause +"'"
//		messagebox("mod_string",mod_string)
		rc = dw_book_query.Modify(mod_string)
		IF rc = "" THEN
			li_row_count = dw_book_query.Retrieve()
			IF li_row_count < 1 THEN 
				MessageBox("ERROR", "There is not such last name , first name"+&
							" and ttl, serttl in database. Try another one.",StopSign!)
			end if
		else
			messagebox('Error','Select statement is wrong: '+rc,StopSign!)
			return
		end if
	end if
end if//end if rb_keyword.checked=true
if rb_soundex.checked=true then
	ls_sndx_auth=f_soundex(ls_ln + ls_fn,'auth')
	ls_sndx_ttl= f_soundex(ls_ttl, 'ttl')
	ls_sndx_auth=trim(ls_sndx_auth)
	ls_sndx_ttl=trim(ls_sndx_ttl)
	if ls_ln + ls_fn<>"" and ls_ttl<> "" then
		where_clause = " AND soundex_auth like "+ "~"" + ls_sndx_auth + "%~""+" and soundex_ttl "+&
		"like "+"~"" + ls_sndx_ttl + "%~""
		mod_string = "DataWindow.Table.Select='" + original_select + where_clause +"'"
//		messagebox("mod_string",mod_string)
		rc = dw_book_query.Modify(mod_string)
		IF rc = "" THEN
			li_row_count = dw_book_query.Retrieve()
			IF li_row_count < 1 THEN 
				MessageBox("ERROR", "There is not such name"+&
							" and ttl in database. Try another one.",StopSign!)
			end if
		else
			messagebox('Error','Select statement is wrong: '+rc,StopSign!)
			return
		end if
	elseif ls_ln+ ls_fn<>"" then
		where_clause = " AND soundex_auth like "+ "~"" + ls_sndx_auth + "%~""
		mod_string = "DataWindow.Table.Select='" + original_select + where_clause +"'"
//		messagebox("mod_string",mod_string)
		rc = dw_book_query.Modify(mod_string)
		IF rc = "" THEN
			li_row_count = dw_book_query.Retrieve()
			IF li_row_count < 1 THEN 
				MessageBox("ERROR", "There is not such name"+&
							" in database. Try another one.",StopSign!)
			end if
		else
			messagebox('Error','Select statement is wrong: '+rc,StopSign!)
			return
		end if
	elseif ls_ttl<> "" then
		where_clause = " AND soundex_ttl like "+ "~"" + ls_sndx_ttl + "%~""
		mod_string = "DataWindow.Table.Select='" + original_select + where_clause +"'"
//		messagebox("mod_string",mod_string)
		rc = dw_book_query.Modify(mod_string)
		IF rc = "" THEN
			li_row_count = dw_book_query.Retrieve()
			IF li_row_count < 1 THEN 
				MessageBox("ERROR", "There is not such name"+&
							" in database. Try another one.",StopSign!)
			end if
		else
			messagebox('Error','Select statement is wrong: '+rc,StopSign!)
			return
		end if
	elseif ls_ln+ ls_fn="" and ls_ttl=""then
		messagebox('Error','You must enter name and ttl, can not leave both empty.',StopSign!)
		return
	end if
end if// end if rb_soundex.checked=true
wf_endpart(li_row_count)

end event

type cb_detail from u_cb within w_books_query
event pfc_hinttext pbm_mousemove
string tag = "See the detail of this control number "
integer x = 1193
integer y = 1232
integer width = 197
integer taborder = 160
fontcharset fontcharset = ansi!
string text = "&Detail"
end type

event pfc_hinttext;w_pics_main.setmicrohelp(This.tag)
end event

event clicked;call super::clicked;
long li_slted
string ls_conno

li_slted=dw_book_query.GetSelectedRow(0)
if li_slted=0 then
	messagebox('Warnning','You must select a row first.')
	return
else
	ls_conno=dw_book_query.GetItemString(li_slted,'conno')
	OpenSheetwithParm(w_books_query_results,ls_conno,w_pics_main, 0, Original!)
end if
end event

type cb_clear from u_cb within w_books_query
event pfc_hinttext pbm_mousemove
string tag = "Clear datawindow and all controls"
integer x = 2112
integer y = 1232
integer width = 183
integer taborder = 140
fontcharset fontcharset = ansi!
string text = "C&lear"
end type

event pfc_hinttext;w_pics_main.setmicrohelp(This.tag)
end event

event clicked;call super::clicked;long li_row_count

dw_book_query.Reset()
li_row_count=dw_book_query.RowCount()

sle_book#.Text=string(li_row_count)
cb_detail.enabled= false
cb_print.enabled=false
cb_title.enabled=false
cb_clear.enabled=false
//rb_bkno.checked=true
sle_bkmed.text=''
sle_bkseq.text=''
sle_book#.text='0'
sle_conno.text=''
sle_cntrno.text=''
sle_fn.text=''
sle_ln.text=''
sle_serttl.text=''
sle_ttl.text=''
cb_search.default=true






end event

type sle_bkmed from u_sle within w_books_query
integer x = 2098
integer y = 268
integer width = 160
integer height = 76
integer taborder = 50
textcase textcase = upper!
end type

type st_conno from statictext within w_books_query
integer x = 1033
integer y = 268
integer width = 274
integer height = 52
integer textsize = -8
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 33554432
long backcolor = 67108864
string text = "Control No:"
boolean focusrectangle = false
end type

type st_serttl from statictext within w_books_query
integer x = 1033
integer y = 368
integer width = 279
integer height = 60
integer textsize = -8
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 33554432
long backcolor = 67108864
string text = "Series Title:"
boolean focusrectangle = false
end type

type st_bkno from statictext within w_books_query
integer x = 1733
integer y = 268
integer width = 343
integer height = 64
integer textsize = -8
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 33554432
long backcolor = 67108864
string text = "Book Number:"
boolean focusrectangle = false
end type

type st_ttl from statictext within w_books_query
integer x = 1042
integer y = 164
integer width = 174
integer height = 52
integer textsize = -8
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 33554432
long backcolor = 67108864
string text = "Title:"
boolean focusrectangle = false
end type

type st_fn from statictext within w_books_query
integer x = 1947
integer y = 52
integer width = 270
integer height = 52
integer textsize = -8
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 33554432
long backcolor = 67108864
string text = "Author FN:"
boolean focusrectangle = false
end type

type st_ln from statictext within w_books_query
integer x = 1029
integer y = 52
integer width = 279
integer height = 56
integer textsize = -8
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 33554432
long backcolor = 67108864
string text = "Author LN:"
boolean focusrectangle = false
end type

type sle_serttl from u_sle within w_books_query
integer x = 1349
integer y = 368
integer width = 1289
integer height = 76
integer taborder = 70
end type

type sle_conno from u_sle within w_books_query
integer x = 1349
integer y = 268
integer width = 297
integer height = 76
integer taborder = 40
textcase textcase = upper!
end type

type sle_bkseq from u_sle within w_books_query
integer x = 2341
integer y = 268
integer width = 297
integer height = 76
integer taborder = 60
textcase textcase = upper!
end type

type sle_ttl from u_sle within w_books_query
integer x = 1349
integer y = 168
integer width = 1289
integer height = 76
integer taborder = 30
end type

type sle_fn from u_sle within w_books_query
integer x = 2341
integer y = 52
integer width = 297
integer height = 76
integer taborder = 20
end type

type gb_2 from groupbox within w_books_query
integer x = 23
integer y = 20
integer width = 837
integer height = 276
integer taborder = 180
integer textsize = -8
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 33554432
long backcolor = 80263581
string text = "Search Type:"
end type

type rb_auth from u_rb within w_books_query
integer x = 110
integer y = 464
integer width = 302
integer height = 100
string text = "Author LN"
end type

event clicked;call super::clicked;long li_row_count
string ls_sort

li_row_count=dw_book_query.RowCount()
if li_row_count>0 then
	if rb_title.checked=true then
		ls_sort="ttl A, bkno A, conno A"
	elseif rb_auth.checked=true then
		ls_sort="auth A, bkno A, conno A"
	elseif rb_conno.checked=true then
		ls_sort="conno A, bkno A, ttl A"
	elseif rb_bkno.checked=true then
		ls_sort="bkno A, conno A, ttl A"
	end if
	dw_book_query.SetSort(ls_sort)
	dw_book_query.Sort()
end if
end event

type rb_conno from u_rb within w_books_query
integer x = 110
integer y = 380
integer width = 329
integer height = 92
string text = "Control No"
end type

event clicked;call super::clicked;long li_row_count
string ls_sort

li_row_count=dw_book_query.RowCount()
if li_row_count>0 then
	if rb_title.checked=true then
		ls_sort="ttl A, bkno A, conno A"
	elseif rb_auth.checked=true then
		ls_sort="auth A, bkno A, conno A"
	elseif rb_conno.checked=true then
		ls_sort="conno A, bkno A, ttl A"
	elseif rb_bkno.checked=true then
		ls_sort="bkno A, conno A, ttl A"
	end if
	dw_book_query.SetSort(ls_sort)
	dw_book_query.Sort()
end if
end event

type rb_keyword from u_rb within w_books_query
integer x = 110
integer y = 76
integer width = 480
integer height = 72
boolean bringtotop = true
string text = "Key Word"
end type

event clicked;call super::clicked;//cb_all.enabled=true
string ls_bkno, ls_bkseq, ls_conno, ls_bkmed
long li_row_count, li_bkseq
string original_select,where_clause,rc ,order_clause, mod_string, ls_ln, ls_fn,&
		ls_ttl, ls_serttl

if  i_count=0 then
	is_ogn_slt=dw_book_query.Describe("DataWindow.Table.Select")
end if
if rb_keyword.checked=true then
	sle_bkmed.visible=false
	sle_bkseq.visible=false
	st_bkno.visible=false
	sle_conno.visible=false
	st_conno.visible=false
	sle_ln.visible=true
	st_ln.visible=true
	sle_fn.visible=true
	st_fn.visible=true
	sle_ttl.visible=true
	st_ttl.visible=true
	sle_serttl.visible=true
	st_serttl.visible=true
	sle_cntrno.visible=true
	st_cntrno.visible=true
end if
i_count++
cb_search.enabled=true
//ls_bkno=sle_bkno.text
//ls_conno=sle_conno.text
//if ls_bkno<>'' and ls_conno<>'' then
//	messagebox('Error','for bkno and conno number ,just one field has a value ',StopSign!)
//	sle_bkno.SetFocus()
//	return
//end if
//if ls_bkno<>'' then
//	ls_bkmed=left(ls_bkno,2)
//	ls_bkseq=mid(ls_bkno,3,6)
//	ls_bkseq=trim(ls_bkseq)
//	if Isnumber(ls_bkseq) then
//		li_bkseq=Long(ls_bkseq)
//	end if
//	where_clause = " AND mchar.bkmed="+ "~"" + ls_bkmed + "~"" +" and bkseq= "+&
//			string(li_bkseq)+""
//	mod_string = "DataWindow.Table.Select='" + original_select + where_clause +"'"
//	messagebox("mod_string",mod_string)
//	rc = dw_book_query.Modify(mod_string)
//	IF rc = "" THEN
//		li_row_count = dw_book_query.Retrieve()
//		IF li_row_count < 1 THEN 
//			MessageBox("ERROR", "There is not such bkno in database. Try another one.",StopSign!)
//		end if
//	else
//		messagebox('Error','Select statement is wrong: '+rc,StopSign!)
//		return
//	end if
//end if
//
//if ls_conno<>'' then
//	where_clause = " AND mchar.conno="+ "~"" + ls_conno + "~"" 
//	mod_string = "DataWindow.Table.Select='" + original_select + where_clause +"'"
//	messagebox("mod_string",mod_string)
//	rc = dw_book_query.Modify(mod_string)
//	IF rc = "" THEN
//		li_row_count = dw_book_query.Retrieve()
//		IF li_row_count < 1 THEN 
//			MessageBox("ERROR", "There is not such conno in database. Try another one.",StopSign!)
//		end if
//	else
//		messagebox('Error','Select statement is wrong: '+rc,StopSign!)
//		return
//	end if
//
//end if
//ls_ln=upper(sle_ln.text)
//ls_fn=upper(sle_fn.text)
//ls_ttl=upper(sle_ttl.text)
//ls_serttl=upper(sle_serttl.text)
//if ls_ln='' and ls_fn='' and ls_ttl='' then
//	where_clause = " AND upper(serttl) like "+ "~"%" + ls_serttl + "%~"" 
//	mod_string = "DataWindow.Table.Select='" + original_select + where_clause +"'"
//	messagebox("mod_string",mod_string)
//	rc = dw_book_query.Modify(mod_string)
//	IF rc = "" THEN
//		li_row_count = dw_book_query.Retrieve()
//		IF li_row_count < 1 THEN 
//			MessageBox("ERROR", "There is not such serttl in database. Try another one.",StopSign!)
//		end if
//	else
//		messagebox('Error','Select statement is wrong: '+rc,StopSign!)
//		return
//	end if
////	dw_book_query.dataobject='d_book_query_serttl_lk'
//elseif ls_ln='' and ls_fn='' and ls_serttl='' then
//	where_clause = " AND upper(ttl) like "+ "~"%" + ls_ttl + "%~"" 
//	mod_string = "DataWindow.Table.Select='" + original_select + where_clause +"'"
//	messagebox("mod_string",mod_string)
//	rc = dw_book_query.Modify(mod_string)
//	IF rc = "" THEN
//		li_row_count = dw_book_query.Retrieve()
//		IF li_row_count < 1 THEN 
//			MessageBox("ERROR", "There is not such ttl in database. Try another one.",StopSign!)
//		end if
//	else
//		messagebox('Error','Select statement is wrong: '+rc,StopSign!)
//		return
//	end if
////	dw_book_query.dataobject='d_book_query_ttl_lk'
//elseif ls_ln='' and ls_ttl='' and ls_serttl='' then
//	where_clause = " AND upper(authfn) like "+ "~"%" + ls_fn + "%~"" 
//	mod_string = "DataWindow.Table.Select='" + original_select + where_clause +"'"
//	messagebox("mod_string",mod_string)
//	rc = dw_book_query.Modify(mod_string)
//	IF rc = "" THEN
//		li_row_count = dw_book_query.Retrieve()
//		IF li_row_count < 1 THEN 
//			MessageBox("ERROR", "There is not such first name in database. Try another one.",StopSign!)
//		end if
//	else
//		messagebox('Error','Select statement is wrong: '+rc,StopSign!)
//		return
//	end if
////	dw_book_query.dataobject='d_book_query_fn_lk'
//elseif ls_fn='' and ls_ttl='' and ls_serttl='' then
//	where_clause = " AND upper(auth) like "+ "~"%" + ls_ln + "%~"" 
//	mod_string = "DataWindow.Table.Select='" + original_select + where_clause +"'"
//	messagebox("mod_string",mod_string)
//	rc = dw_book_query.Modify(mod_string)
//	IF rc = "" THEN
//		li_row_count = dw_book_query.Retrieve()
//		IF li_row_count < 1 THEN 
//			MessageBox("ERROR", "There is not such last name in database. Try another one.",StopSign!)
//		end if
//	else
//		messagebox('Error','Select statement is wrong: '+rc,StopSign!)
//		return
//	end if
////	dw_book_query.dataobject='d_book_query_ln_lk'
//elseif ls_ln='' and ls_fn='' then
////	dw_book_query.dataobject='d_book_query_ttl_serttl_lk'
//	where_clause = " AND upper(ttl) like "+ "~"%" + ls_ttl + "%~""+" and upper(serttl) "+&
//		"like "+"~"%" + ls_serttl + "%~""
//	mod_string = "DataWindow.Table.Select='" + original_select + where_clause +"'"
//	messagebox("mod_string",mod_string)
//	rc = dw_book_query.Modify(mod_string)
//	IF rc = "" THEN
//		li_row_count = dw_book_query.Retrieve()
//		IF li_row_count < 1 THEN 
//			MessageBox("ERROR", "There is not such ttl and serttl in ttlinit. Try another one.",StopSign!)
//		end if
//	else
//		messagebox('Error','Select statement is wrong: '+rc,StopSign!)
//		return
//	end if
//elseif ls_ln='' and ls_ttl='' then
////	dw_book_query.dataobject='d_book_query_fn_serttl_lk'
//	where_clause = " AND upper(authfn) like "+ "~"%" + ls_fn + "%~""+" and upper(serttl) "+&
//		"like "+"~"%" + ls_serttl + "%~""
//	mod_string = "DataWindow.Table.Select='" + original_select + where_clause +"'"
//	messagebox("mod_string",mod_string)
//	rc = dw_book_query.Modify(mod_string)
//	IF rc = "" THEN
//		li_row_count = dw_book_query.Retrieve()
//		IF li_row_count < 1 THEN 
//			MessageBox("ERROR", "There is not such first name and serttl in database. Try another one.",StopSign!)
//		end if
//	else
//		messagebox('Error','Select statement is wrong: '+rc,StopSign!)
//		return
//	end if
//elseif ls_fn='' and ls_ttl='' then
////	dw_book_query.dataobject='d_book_query_ln_serttl_lk'
//	where_clause = " AND upper(auth) like "+ "~"%" + ls_ln + "%~""+" and upper(serttl) "+&
//		"like "+"~"%" + ls_serttl + "%~""
//	mod_string = "DataWindow.Table.Select='" + original_select + where_clause +"'"
//	messagebox("mod_string",mod_string)
//	rc = dw_book_query.Modify(mod_string)
//	IF rc = "" THEN
//		li_row_count = dw_book_query.Retrieve()
//		IF li_row_count < 1 THEN 
//			MessageBox("ERROR", "There is not such last name and serttl in database. Try another one.",StopSign!)
//		end if
//	else
//		messagebox('Error','Select statement is wrong: '+rc,StopSign!)
//		return
//	end if
//elseif ls_ln='' and ls_serttl='' then
////	dw_book_query.dataobject='d_book_query_fn_ttl_lk'
//	where_clause = " AND upper(authfn) like "+ "~"%" + ls_fn + "%~""+" and upper(ttl) "+&
//		"like "+"~"%" + ls_ttl + "%~""
//	mod_string = "DataWindow.Table.Select='" + original_select + where_clause +"'"
//	messagebox("mod_string",mod_string)
//	rc = dw_book_query.Modify(mod_string)
//	IF rc = "" THEN
//		li_row_count = dw_book_query.Retrieve()
//		IF li_row_count < 1 THEN 
//			MessageBox("ERROR", "There is not such first name and ttl in database. Try another one.",StopSign!)
//		end if
//	else
//		messagebox('Error','Select statement is wrong: '+rc,StopSign!)
//		return
//	end if
//elseif ls_fn='' and ls_serttl='' then
////	dw_book_query.dataobject='d_book_query_ln_ttl_lk'
//	where_clause = " AND upper(auth) like "+ "~"%" + ls_ln + "%~""+" and upper(ttl) "+&
//		"like "+"~"%" + ls_ttl + "%~""
//	mod_string = "DataWindow.Table.Select='" + original_select + where_clause +"'"
//	messagebox("mod_string",mod_string)
//	rc = dw_book_query.Modify(mod_string)
//	IF rc = "" THEN
//		li_row_count = dw_book_query.Retrieve()
//		IF li_row_count < 1 THEN 
//			MessageBox("ERROR", "There is not such last name and ttl in database. Try another one.",StopSign!)
//		end if
//	else
//		messagebox('Error','Select statement is wrong: '+rc,StopSign!)
//		return
//	end if
//elseif ls_ttl='' and ls_serttl='' then
////	dw_book_query.dataobject='d_book_query_ln_fn_lk'
//	where_clause = " AND upper(auth) like "+ "~"%" + ls_ln + "%~""+" and upper(authfn) "+&
//		"like "+"~"%" + ls_fn + "%~""
//	mod_string = "DataWindow.Table.Select='" + original_select + where_clause +"'"
//	messagebox("mod_string",mod_string)
//	rc = dw_book_query.Modify(mod_string)
//	IF rc = "" THEN
//		li_row_count = dw_book_query.Retrieve()
//		IF li_row_count < 1 THEN 
//			MessageBox("ERROR", "There is not such last name and first name"+&
//										" in database. Try another one.",StopSign!)
//		end if
//	else
//		messagebox('Error','Select statement is wrong: '+rc,StopSign!)
//		return
//	end if
//elseif ls_ln='' then
////	dw_book_query.dataobject='d_book_query_fn_ttl_serttl_lk'
//	where_clause = " AND upper(authfn) like "+ "~"%" + ls_fn + "%~""+" and upper(ttl) "+&
//	"like "+"~"%" + ls_ttl + "%~""+" and upper(serttl) like "+"~"%" + ls_serttl + "%~""
//	mod_string = "DataWindow.Table.Select='" + original_select + where_clause +"'"
//	messagebox("mod_string",mod_string)
//	rc = dw_book_query.Modify(mod_string)
//	IF rc = "" THEN
//		li_row_count = dw_book_query.Retrieve()
//		IF li_row_count < 1 THEN 
//			MessageBox("ERROR", "There is not such  first name , ttl"+&
//						" and serttl in database. Try another one.",StopSign!)
//		end if
//	else
//		messagebox('Error','Select statement is wrong: '+rc,StopSign!)
//		return
//	end if
//elseif ls_fn='' then
////	dw_book_query.dataobject='d_book_query_ln_ttl_serttl_lk'
//	where_clause = " AND upper(auth) like "+ "~"%" + ls_ln + "%~""+" and upper(ttl) "+&
//	"like "+"~"%" + ls_ttl + "%~""+" and upper(serttl) like "+"~"%" + ls_serttl + "%~""
//	mod_string = "DataWindow.Table.Select='" + original_select + where_clause +"'"
//	messagebox("mod_string",mod_string)
//	rc = dw_book_query.Modify(mod_string)
//	IF rc = "" THEN
//		li_row_count = dw_book_query.Retrieve()
//		IF li_row_count < 1 THEN 
//			MessageBox("ERROR", "There is not such last name , ttl"+&
//						" and serttl in database. Try another one.",StopSign!)
//		end if
//	else
//		messagebox('Error','Select statement is wrong: '+rc,StopSign!)
//		return
//	end if
//elseif ls_ttl='' then
////	dw_book_query.dataobject='d_book_query_ln_fn_serttl_lk'
//	where_clause = " AND upper(auth) like "+ "~"%" + ls_ln + "%~""+" and upper(authfn) "+&
//	"like "+"~"%" + ls_fn + "%~""+" and upper(serttl) like "+"~"%" + ls_serttl + "%~""
//	mod_string = "DataWindow.Table.Select='" + original_select + where_clause +"'"
//	messagebox("mod_string",mod_string)
//	rc = dw_book_query.Modify(mod_string)
//	IF rc = "" THEN
//		li_row_count = dw_book_query.Retrieve()
//		IF li_row_count < 1 THEN 
//			MessageBox("ERROR", "There is not such last name , first name"+&
//						" and serttl in database. Try another one.",StopSign!)
//		end if
//	else
//		messagebox('Error','Select statement is wrong: '+rc,StopSign!)
//		return
//	end if
//elseif ls_serttl='' then
////	dw_book_query.dataobject='d_book_query_ln_fn_ttl_lk'
//	where_clause = " AND upper(auth) like "+ "~"%" + ls_ln + "%~""+" and upper(authfn) "+&
//	"like "+"~"%" + ls_fn + "%~""+" and upper(ttl) like "+"~"%" + ls_ttl + "%~""
//	mod_string = "DataWindow.Table.Select='" + original_select + where_clause +"'"
//	messagebox("mod_string",mod_string)
//	rc = dw_book_query.Modify(mod_string)
//	IF rc = "" THEN
//		li_row_count = dw_book_query.Retrieve()
//		IF li_row_count < 1 THEN 
//			MessageBox("ERROR", "There is not such last name , first name"+&
//						" and ttl in database. Try another one.",StopSign!)
//		end if
//	else
//		messagebox('Error','Select statement is wrong: '+rc,StopSign!)
//		return
//	end if
//else
////	dw_book_query.dataobject='d_book_query_ln_fn_ttl_serttl_lk'
//	where_clause = " AND upper(auth) like "+ "~"%" + ls_ln + "%~""+" and upper(authfn) "+&
//	"like "+"~"%" + ls_fn + "%~""+" and upper(ttl) like "+"~"%" + ls_ttl + "%~""+&
//	" and upper(serttl) like "+"~"%" + ls_serttl + "%~""
//	mod_string = "DataWindow.Table.Select='" + original_select + where_clause +"'"
//	messagebox("mod_string",mod_string)
//	rc = dw_book_query.Modify(mod_string)
//	IF rc = "" THEN
//		li_row_count = dw_book_query.Retrieve()
//		IF li_row_count < 1 THEN 
//			MessageBox("ERROR", "There is not such last name , first name"+&
//						" and ttl, serttl in database. Try another one.",StopSign!)
//		end if
//	else
//		messagebox('Error','Select statement is wrong: '+rc,StopSign!)
//		return
//	end if
//end if
//
//
////	ls_sort="ttl A, bkno A, conno A, prdr A"
//dw_book_query.SelectRow(0,false)
//dw_book_query.SelectRow(1,true)
//dw_book_query.ScrollToRow(1)
//dw_book_query.SetRow(1)
//dw_book_query.SetFocus()
//sle_book#.text=string(li_row_count)
//
end event

type rb_soundex from u_rb within w_books_query
integer x = 110
integer y = 140
integer width = 334
integer height = 68
boolean bringtotop = true
string text = "Soundex"
end type

event clicked;call super::clicked;if  i_count=0 then
	is_ogn_slt=dw_book_query.Describe("DataWindow.Table.Select")
end if
if rb_soundex.checked=true then
	sle_bkmed.visible=false
	sle_bkseq.visible=false
	st_bkno.visible=false
	sle_conno.visible=false
	st_conno.visible=false
	sle_ln.visible=true
	st_ln.visible=true
	sle_fn.visible=false
	st_fn.visible=false
	sle_ttl.visible=true
	st_ttl.visible=true
	sle_serttl.visible=false
	st_serttl.visible=false
	sle_cntrno.visible=false
	st_cntrno.visible=false
end if
i_count++
cb_search.enabled=true
end event

type rb_match from u_rb within w_books_query
integer x = 110
integer y = 208
integer width = 416
integer height = 68
boolean bringtotop = true
string text = "Exact Match"
end type

event clicked;call super::clicked;if  i_count=0 then
	is_ogn_slt=dw_book_query.Describe("DataWindow.Table.Select")
end if
if rb_match.checked=true then
	sle_bkmed.visible=true
	sle_bkseq.visible=true
	st_bkno.visible=true
	sle_conno.visible=true
	st_conno.visible=true
	sle_ln.visible=true
	st_ln.visible=true
	sle_fn.visible=false
	st_fn.visible=false
	sle_ttl.visible=true
	st_ttl.visible=true
	sle_serttl.visible=false
	st_serttl.visible=false
	sle_cntrno.visible=true
	st_cntrno.visible=true
end if
i_count++
cb_search.enabled=true
cb_search.default=true
end event

type rb_title from u_rb within w_books_query
integer x = 443
integer y = 460
integer width = 402
integer height = 92
string text = "Title"
end type

event clicked;call super::clicked;long li_row_count
string ls_sort

li_row_count=dw_book_query.RowCount()
if li_row_count>0 then
	if rb_title.checked=true then
		ls_sort="ttl A, bkno A, conno A"
	elseif rb_auth.checked=true then
		ls_sort="auth A, bkno A, conno A"
	elseif rb_conno.checked=true then
		ls_sort="conno A, bkno A, ttl A"
	elseif rb_bkno.checked=true then
		ls_sort="bkno A, conno A, ttl A"
	end if
	dw_book_query.SetSort(ls_sort)
	dw_book_query.Sort()
end if
end event

type rb_bkno from u_rb within w_books_query
integer x = 443
integer y = 388
integer width = 402
integer height = 68
string text = "Book Number"
end type

event clicked;call super::clicked;long li_row_count
string ls_sort

li_row_count=dw_book_query.RowCount()
if li_row_count>0 then
	if rb_title.checked=true then
		ls_sort="ttl A, bkno A, conno A"
	elseif rb_auth.checked=true then
		ls_sort="auth A, bkno A, conno A"
	elseif rb_conno.checked=true then
		ls_sort="conno A, bkno A, ttl A"
	elseif rb_bkno.checked=true then
		ls_sort="bkno A, conno A, ttl A"
	end if
	dw_book_query.SetSort(ls_sort)
	dw_book_query.Sort()
end if
end event

type sle_ln from u_sle within w_books_query
integer x = 1349
integer y = 52
integer width = 315
integer height = 76
integer taborder = 10
end type

type st_total from statictext within w_books_query
integer x = 55
integer y = 1232
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
alignment alignment = right!
boolean focusrectangle = false
end type

type sle_book# from u_sle within w_books_query
integer x = 562
integer y = 1232
integer width = 265
integer height = 76
integer taborder = 0
end type

type cb_print from u_cb within w_books_query
event pfc_hinttext pbm_mousemove
string tag = "Print the report"
integer x = 1481
integer y = 1232
integer width = 169
integer taborder = 110
fontcharset fontcharset = ansi!
string text = "&Print"
end type

event pfc_hinttext;call super::pfc_hinttext;w_pics_main.setmicrohelp(This.tag)
end event

event clicked;call super::clicked;nvo_PowerPrn.of_SetPrinterOrientation(2)
dw_book_query.TriggerEvent('pfc_print')
nvo_PowerPrn.of_SetPrinterOrientation(1)
end event

type cb_cancel from u_cb within w_books_query
event pfc_hinttext pbm_mousemove
string tag = "Exits the screen"
integer x = 2386
integer y = 1232
integer width = 238
integer taborder = 170
fontcharset fontcharset = ansi!
string text = "&Exit"
end type

event pfc_hinttext;call super::pfc_hinttext;w_pics_main.setmicrohelp(This.tag)
end event

event clicked;call super::clicked;parent.Event pfc_close()
m_pics_main.m_menu.PopMenu(300, 0)

end event

type cb_search from u_cb within w_books_query
string tag = "Search the specific records"
integer x = 1778
integer y = 1232
integer width = 279
integer taborder = 130
fontcharset fontcharset = ansi!
string text = "&Search"
end type

event clicked;call super::clicked;//////////////////////////////////////////////////////////////////////////////////////////////////////////
//
//	Event:  clicked for cb_search
//
//	Description:
//	Perform Search 
//
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//	
//	Revision History
//
//	Developed by 	Date 				Version												Tracking#
//									
// Murali K.			03/05/2008      	4.0 apostrophe char check							#2061
//////////////////////////////////////////////////////////////////////////////////////////////////////////


//cb_all.enabled=true
String ls_bkno, ls_bkseq, ls_conno, ls_bkmed
Long li_row_count, li_bkseq
String original_select,where_clause,rc ,order_clause, mod_string, ls_ln, ls_fn,&
		ls_ttl, ls_serttl, ls_sndx_auth, ls_sndx_ttl, ls_sort, ls_syntax, ls_syntaxnew,&
		ls_ttl2, ls_error, ls_ln2, ls_fn2,ls_cntr
Long li_pos, li_pos2, li_pos3, li_pos4
n_cst_string 	inv_string

i_cntclick++

IF i_cntclick=1 THEN
	is_ogn_slt=dw_book_query.Describe("DataWindow.Table.Select")
END IF
original_select =	is_ogn_slt
ls_ln=Upper(sle_ln.text)
ls_fn=Upper(sle_fn.text)
ls_ttl=Upper(sle_ttl.text)
ls_serttl=Upper(sle_serttl.text)
// 03/05/2008 apostrophe char handling #2061 of_charcheck()
of_charcheck(ls_serttl)


ls_cntr=Upper(sle_cntrno.text)

li_pos=Pos(ls_ttl,"'")
IF li_pos>0 THEN
	ls_ttl2=Left(ls_ttl, li_pos - 1)+"~''" + Mid(ls_ttl, li_pos+1)
END IF
li_pos=Pos(ls_ln,"'")
IF li_pos>0 THEN
	ls_ln2=Left(ls_ln, li_pos - 1)+"~''" + Mid(ls_ln, li_pos+1)
END IF
li_pos=Pos(ls_fn,"'")
IF li_pos>0 THEN
	ls_fn2=Left(ls_fn, li_pos - 1)+"~''" + Mid(ls_fn, li_pos+1)
END IF
IF rb_match.checked=TRUE THEN
	ls_bkmed=sle_bkmed.text
	ls_bkseq=sle_bkseq.text
	ls_bkno=Trim(ls_bkmed)+Trim(ls_bkseq)
	ls_conno=sle_conno.text
	ls_cntr=sle_cntrno.text
	
	IF ls_bkno<>"" AND ls_conno<>"" THEN
		Messagebox('Error','Only enter book number or control number. ',stopSign!)
		sle_bkmed.SetFocus()
		cb_clear.enabled=TRUE
		RETURN
	END IF
	IF ls_bkno<>"" THEN
		ls_bkmed=sle_bkmed.text
		ls_bkseq=sle_bkseq.text
		IF IsNumber(ls_bkseq) THEN
			li_bkseq=Long(ls_bkseq)
		END IF
		where_clause = " AND mchar.bkmed="+ "~~'" + ls_bkmed + "~~'" +" and mchar.bkseq= "+&
				String(li_bkseq)+""
		mod_string = "DataWindow.Table.Select='" + original_select + where_clause +"'"
//		messagebox("mod_string",mod_string)
		rc = dw_book_query.Modify(mod_string)
		IF rc = "" THEN
			li_row_count = dw_book_query.Retrieve()
			IF li_row_count < 1 THEN 
				Messagebox("ERROR", "Book number "+ls_bkno+" does not exist in the database. Try another one.",stopSign!)
				cb_clear.enabled=TRUE
				RETURN
			ELSE
				sle_book#.text=String(li_row_count)
				wf_endpart(li_row_count)
			END IF
		ELSE
			Messagebox('Error','Select statement is wrong: '+rc,stopSign!)
			cb_clear.enabled=TRUE
			RETURN
		END IF
	END IF
	IF ls_cntr<>"" THEN
		where_clause = " AND prod.cntr="+ "~~'" + ls_cntr + "~~'" +""
		mod_string = "DataWindow.Table.Select='" + original_select + where_clause +"'"
//		messagebox("mod_string",mod_string)
		rc = dw_book_query.Modify(mod_string)
		IF rc = "" THEN
			li_row_count = dw_book_query.Retrieve()
			IF li_row_count < 1 THEN 
				Messagebox("ERROR", "There is no PCS contract number "+ls_cntr+" in database. Try another one.",stopSign!)
				cb_clear.enabled=TRUE
				RETURN
			ELSE
				sle_book#.text=String(li_row_count)
				wf_endpart(li_row_count)
			END IF
		ELSE
			Messagebox('Error','Select statement is wrong: '+rc,stopSign!)
			cb_clear.enabled=TRUE
			RETURN
		END IF
	END IF
	IF ls_conno<>"" THEN
		where_clause = " AND mchar.conno="+ "~~'" + ls_conno + "~~'" 
		mod_string = "DataWindow.Table.Select='" + original_select + where_clause +"'"
//		messagebox("mod_string",mod_string)
		rc = dw_book_query.Modify(mod_string)
		IF rc = "" THEN
			li_row_count = dw_book_query.Retrieve()
			IF li_row_count < 1 THEN 
				Messagebox("ERROR", "Control number "+ls_conno+" does not exist in the database. Try another one.",stopSign!)
				cb_clear.enabled=TRUE
				RETURN
			ELSE
				sle_book#.text=String(li_row_count)
				wf_endpart(li_row_count)
			END IF
		ELSE
			Messagebox('Error','Select statement is wrong: '+rc,stopSign!)
			cb_clear.enabled=TRUE
			RETURN
		END IF
	END IF
	IF ls_ln <>"" AND ls_ttl <>"" THEN
//		where_clause = " AND upper(auth)= "+"~~'"+ ls_ln + "~~'"+" and upper(ttl)= "+&
//		"~~'" + ls_ttl + "~~'"
		where_clause = " AND upper(auth)= "+"~~'"+ ls_ln + "~~'"+" and upper(ttl)= "+&
		"~~'" + ls_ttl + "~~'"
		mod_string = "DataWindow.Table.Select='" + original_select + where_clause +"'"
//		messagebox("mod_string",mod_string)
		rc = dw_book_query.Modify(mod_string)
		IF rc = "" THEN
			li_row_count = dw_book_query.Retrieve()
			IF li_row_count < 1 THEN 
				Messagebox("ERROR", "There is not such last name and ttl in database. Try another one.",stopSign!)
			ELSE
				sle_book#.text=String(li_row_count)
			END IF
		ELSE
			Messagebox('Error','Select statement is wrong: '+rc,stopSign!)
			cb_clear.enabled=TRUE
			RETURN
		END IF
	ELSEIF ls_ln<>"" THEN
//		ls_syntaxnew = ls_syntax + " and ttlinit.ttl =  '"+ls_ttl2+"'"
		IF Pos(ls_ln,"'")=0 THEN
//			where_clause = " AND upper(auth)= "+"~~'"+ ls_ln + "~~'"
//			mod_string = "DataWindow.Table.Select='" + original_select + where_clause +"'"

			where_clause = " AND upper(auth)=   "+"~~'"+ ls_ln+"~~'"
			mod_string = "DataWindow.Table.Select='" + original_select + where_clause +"'"
//			messagebox('original_select',original_select)
//			mod_string = "DataWindow.Table.Select='" + original_select  +"'''"
//			messagebox("mod_string has where clause",mod_string)
			rc = dw_book_query.Modify(mod_string)
		ELSE
//			where_clause = " AND upper(auth)= '"+ls_ln2+"'"
			ls_syntax = original_select
			ls_syntaxnew = ls_syntax + " and upper(auth) =  '"+ls_ln2+"'"
			rc= dw_book_query.Modify('DataWindow.Table.Select="' + ls_syntaxnew + '"')
//			messagebox("ls_syntaxnew",ls_syntaxnew)
		END IF
		IF rc = "" THEN
			li_row_count = dw_book_query.Retrieve()
			IF li_row_count < 1 THEN 
				Messagebox("ERROR", "There is not such last name in database. Try another one.",stopSign!)
			ELSE
				sle_book#.text=String(li_row_count)
			END IF
		ELSE
			Messagebox('Error','Select statement is wrong: '+rc,stopSign!)
			cb_clear.enabled=TRUE
			RETURN
		END IF
	ELSEIF ls_ttl<>"" THEN
		IF Pos(ls_ttl,"'")=0 THEN
			where_clause = " AND upper(ttl)= "+"~~'"+ ls_ttl + "~~'"
			mod_string = "DataWindow.Table.Select='" + original_select + where_clause +"'"
//			messagebox("mod_string",mod_string)
			rc = dw_book_query.Modify(mod_string)
		ELSE
			ls_syntax = original_select
			ls_syntaxnew = ls_syntax + " and upper(ttl) =  '"+ls_ttl2+"'"
			rc= dw_book_query.Modify('DataWindow.Table.Select="' + ls_syntaxnew + '"')
//			messagebox("ls_syntaxnew",ls_syntaxnew)
		END IF
		IF rc = "" THEN
			li_row_count = dw_book_query.Retrieve()
			IF li_row_count < 1 THEN 
				Messagebox("ERROR", "There is not such ttl in database. Try another one.",stopSign!)
			ELSE
				sle_book#.text=String(li_row_count)
			END IF
		ELSE
			Messagebox('Error','Select statement is wrong: '+rc,stopSign!)
			cb_clear.enabled=TRUE
			RETURN
		END IF
	ELSEIF ls_ttl="" AND ls_ln="" AND ls_bkno="" AND ls_conno="" THEN
		Messagebox('Error',"You must enter some fields, Can not "+&
			"leave every thing empty.",stopSign!)
			cb_clear.enabled=TRUE
		RETURN
	END IF//end if ls_ln<>"" and ls_ttl<>""
END IF// end if rb_match.checked=true
IF rb_keyword.checked=TRUE THEN
	IF ls_ln="" AND ls_fn="" AND ls_ttl="" THEN
		where_clause = " AND upper(serttl) like "+ "~~'%" + ls_serttl + "%~~'" 
		mod_string = "DataWindow.Table.Select='" + original_select + where_clause +"'"
//		messagebox("mod_string",mod_string)
		rc = dw_book_query.Modify(mod_string)
		IF rc = "" THEN
			li_row_count = dw_book_query.Retrieve()
			IF li_row_count < 1 THEN 
				Messagebox("ERROR", "There is not such serttl in database."+&
				" Try another one.",stopSign!)
			END IF
		ELSE
			Messagebox('Error','Select statement is wrong: '+rc,stopSign!)
			cb_clear.enabled=TRUE
			RETURN
		END IF
	//	dw_book_query.dataobject='d_book_query_serttl_lk'
	ELSEIF ls_ln="" AND ls_fn="" AND ls_serttl="" THEN
		IF Pos(ls_ttl,"'")=0 THEN
			where_clause = " AND upper(ttl) like "+ "~~'%" + ls_ttl + "%~~'" 
			mod_string = "DataWindow.Table.Select='" + original_select + where_clause +"'"
	//		messagebox("mod_string",mod_string)
			rc = dw_book_query.Modify(mod_string)
		ELSE
			ls_syntax = original_select
			ls_syntaxnew = ls_syntax + " and upper(ttl) like  '%"+ls_ttl2+"%'"
			rc= dw_book_query.Modify('DataWindow.Table.Select="' + ls_syntaxnew + '"')
//			messagebox("ls_syntaxnew",ls_syntaxnew)
		END IF
		IF rc = "" THEN
			li_row_count = dw_book_query.Retrieve()
			IF li_row_count < 1 THEN 
				Messagebox("ERROR", "There is not such ttl in database. "+&
					"Try another one.",stopSign!)
			END IF
		ELSE
			Messagebox('Error','Select statement is wrong: '+rc,stopSign!)
			cb_clear.enabled=TRUE
			RETURN
		END IF
	//	dw_book_query.dataobject='d_book_query_ttl_lk'
	ELSEIF ls_ln="" AND ls_ttl="" AND ls_serttl="" THEN
		IF Pos(ls_fn,"'")=0 THEN
			where_clause = " AND upper(authfn) like "+ "~~'%" + ls_fn + "%~~'" 
			mod_string = "DataWindow.Table.Select='" + original_select + where_clause +"'"
	//		messagebox("mod_string",mod_string)
			rc = dw_book_query.Modify(mod_string)
		ELSE
			ls_syntax = original_select
			ls_syntaxnew = ls_syntax + " and upper(authfn) like  '%"+ls_fn2+"%'"
			rc= dw_book_query.Modify('DataWindow.Table.Select="' + ls_syntaxnew + '"')
		END IF
		IF rc = "" THEN
			li_row_count = dw_book_query.Retrieve()
			IF li_row_count < 1 THEN 
				Messagebox("ERROR", "There is not such first name in database. "+&
					"Try another one.",stopSign!)
			END IF
		ELSE
			Messagebox('Error','Select statement is wrong: '+rc,stopSign!)
			cb_clear.enabled=TRUE
			RETURN
		END IF
	//	dw_book_query.dataobject='d_book_query_fn_lk'
	ELSEIF ls_fn="" AND ls_ttl="" AND ls_serttl="" THEN
		IF Pos(ls_ln,"'")=0 THEN
//			where_clause = " AND upper(auth) like "+ "~~'%" + ls_ln + "%~~'" 
//			mod_string = "DataWindow.Table.Select='" + original_select + where_clause +"'"
			where_clause = " AND upper(auth) like "+ "~~'%" + ls_ln + "%~~'" 
			mod_string = "DataWindow.Table.Select='" + original_select + where_clause +"'"
	//		messagebox("mod_string",mod_string)
			rc = dw_book_query.Modify(mod_string)
		ELSE
			ls_syntax = original_select
			ls_syntaxnew = ls_syntax + " and upper(auth) like  '%"+ls_ln2+"%'"
			rc= dw_book_query.Modify('DataWindow.Table.Select="' + ls_syntaxnew + '"')
//			messagebox("ls_syntaxnew",ls_syntaxnew)
		END IF
		IF rc = "" THEN
			li_row_count = dw_book_query.Retrieve()
			IF li_row_count < 1 THEN 
				Messagebox("ERROR", "There is not such last name in database. "+&
					"Try another one.",stopSign!)
			END IF
		ELSE
			Messagebox('Error','Select statement is wrong: '+rc,stopSign!)
			cb_clear.enabled=TRUE
			RETURN
		END IF
	//	dw_book_query.dataobject='d_book_query_ln_lk'
	ELSEIF ls_ln="" AND ls_fn="" THEN
	//	dw_book_query.dataobject='d_book_query_ttl_serttl_lk'
		where_clause = " AND upper(ttl) like "+ "~~'%" + ls_ttl + "%~~'" +" and upper(serttl) "+&
			"like "+"~~'%" + ls_serttl + "%~~'" 
		mod_string = "DataWindow.Table.Select='" + original_select + where_clause +"'"
//		messagebox("mod_string",mod_string)
		rc = dw_book_query.Modify(mod_string)
		IF rc = "" THEN
			li_row_count = dw_book_query.Retrieve()
			IF li_row_count < 1 THEN 
				Messagebox("ERROR", "There is not such ttl and serttl in ttlinit. "+&
					"Try another one.",stopSign!)
			END IF
		ELSE
			Messagebox('Error','Select statement is wrong: '+rc,stopSign!)
			cb_clear.enabled=TRUE
			RETURN
		END IF
	ELSEIF ls_ln="" AND ls_ttl="" THEN
	//	dw_book_query.dataobject='d_book_query_fn_serttl_lk'
		where_clause = " AND upper(authfn) like "+ "~~'%" + ls_fn + "%~~'" +" and upper(serttl) "+&
			"like "+"~~'%" + ls_serttl + "%~~'" 
		mod_string = "DataWindow.Table.Select='" + original_select + where_clause +"'"
//		messagebox("mod_string",mod_string)
		rc = dw_book_query.Modify(mod_string)
		IF rc = "" THEN
			li_row_count = dw_book_query.Retrieve()
			IF li_row_count < 1 THEN 
				Messagebox("ERROR", "There is not such first name and serttl in database. "+&
					"Try another one.",stopSign!)
			END IF
		ELSE
			Messagebox('Error','Select statement is wrong: '+rc,stopSign!)
			cb_clear.enabled=TRUE
			RETURN
		END IF
	ELSEIF ls_fn="" AND ls_ttl="" THEN
	//	dw_book_query.dataobject='d_book_query_ln_serttl_lk'
		where_clause = " AND upper(auth) like "+ "~~'%" + ls_ln + "%~~'" +" and upper(serttl) "+&
			"like "+"~~'%" + ls_serttl + "%~~'" 
		mod_string = "DataWindow.Table.Select='" + original_select + where_clause +"'"
//		messagebox("mod_string",mod_string)
		rc = dw_book_query.Modify(mod_string)
		IF rc = "" THEN
			li_row_count = dw_book_query.Retrieve()
			IF li_row_count < 1 THEN 
				Messagebox("ERROR", "There is not such last name and serttl in database. "+&
					"Try another one.",stopSign!)
			END IF
		ELSE
			Messagebox('Error','Select statement is wrong: '+rc,stopSign!)
			cb_clear.enabled=TRUE
			RETURN
		END IF
	ELSEIF ls_ln="" AND ls_serttl="" THEN
	//	dw_book_query.dataobject='d_book_query_fn_ttl_lk'
		where_clause = " AND upper(authfn) like "+ "~~'%" + ls_fn + "%~~'" +" and upper(ttl) "+&
			"like "+"~~'%" + ls_ttl + "%~~'" 
		mod_string = "DataWindow.Table.Select='" + original_select + where_clause +"'"
//		messagebox("mod_string",mod_string)
		rc = dw_book_query.Modify(mod_string)
		IF rc = "" THEN
			li_row_count = dw_book_query.Retrieve()
			IF li_row_count < 1 THEN 
				Messagebox("ERROR", "There is not such first name and ttl in database. "+&
					"Try another one.",stopSign!)
			END IF
		ELSE
			Messagebox('Error','Select statement is wrong: '+rc,stopSign!)
			cb_clear.enabled=TRUE
			RETURN
		END IF
	ELSEIF ls_fn="" AND ls_serttl="" THEN
	//	dw_book_query.dataobject='d_book_query_ln_ttl_lk'
		where_clause = " AND upper(auth) like "+ "~~'%" + ls_ln + "%~~'" +" and upper(ttl) "+&
			"like "+"~~'%" + ls_ttl + "%~~'" 
		mod_string = "DataWindow.Table.Select='" + original_select + where_clause +"'"
//		messagebox("mod_string",mod_string)
		rc = dw_book_query.Modify(mod_string)
		IF rc = "" THEN
			li_row_count = dw_book_query.Retrieve()
			IF li_row_count < 1 THEN 
				Messagebox("ERROR", "There is not such last name and ttl in database. "+&
					"Try another one.",stopSign!)
			END IF
		ELSE
			Messagebox('Error','Select statement is wrong: '+rc,stopSign!)
			cb_clear.enabled=TRUE
			RETURN
		END IF
	ELSEIF ls_ttl="" AND ls_serttl="" THEN
	//	dw_book_query.dataobject='d_book_query_ln_fn_lk'
		where_clause = " AND upper(auth) like "+ "~~'%" + ls_ln + "%~~'" +" and upper(authfn) "+&
			"like "+"~~'%" + ls_fn + "%~~'" 
		mod_string = "DataWindow.Table.Select='" + original_select + where_clause +"'"
//		messagebox("mod_string",mod_string)
		rc = dw_book_query.Modify(mod_string)
		IF rc = "" THEN
			li_row_count = dw_book_query.Retrieve()
			IF li_row_count < 1 THEN 
				Messagebox("ERROR", "There is not such last name and first name"+&
											" in database. Try another one.",stopSign!)
			END IF
		ELSE
			Messagebox('Error','Select statement is wrong: '+rc,stopSign!)
			cb_clear.enabled=TRUE
			RETURN
		END IF
	ELSEIF ls_ln="" THEN
	//	dw_book_query.dataobject='d_book_query_fn_ttl_serttl_lk'
		where_clause = " AND upper(authfn) like "+ "~~'%" + ls_fn + "%~~'" +" and upper(ttl) "+&
		"like "+"~~'%" + ls_ttl + "%~~'" +" and upper(serttl) like "+"~~'%" + ls_serttl + "%~~'" 
		mod_string = "DataWindow.Table.Select='" + original_select + where_clause +"'"
//		messagebox("mod_string",mod_string)
		rc = dw_book_query.Modify(mod_string)
		IF rc = "" THEN
			li_row_count = dw_book_query.Retrieve()
			IF li_row_count < 1 THEN 
				Messagebox("ERROR", "There is not such  first name , ttl"+&
							" and serttl in database. Try another one.",stopSign!)
			END IF
		ELSE
			Messagebox('Error','Select statement is wrong: '+rc,stopSign!)
			cb_clear.enabled=TRUE
			RETURN
		END IF
	ELSEIF ls_fn="" THEN
	//	dw_book_query.dataobject='d_book_query_ln_ttl_serttl_lk'
		where_clause = " AND upper(auth) like "+ "~~'%" + ls_ln + "%~~'" +" and upper(ttl) "+&
		"like "+"~~'%" + ls_ttl + "%~~'" +" and upper(serttl) like "+"~~'%" + ls_serttl + "%~~'" 
		mod_string = "DataWindow.Table.Select='" + original_select + where_clause +"'"
//		messagebox("mod_string",mod_string)
		rc = dw_book_query.Modify(mod_string)
		IF rc = "" THEN
			li_row_count = dw_book_query.Retrieve()
			IF li_row_count < 1 THEN 
				Messagebox("ERROR", "There is not such last name , ttl"+&
							" and serttl in database. Try another one.",stopSign!)
			END IF
		ELSE
			Messagebox('Error','Select statement is wrong: '+rc,stopSign!)
			cb_clear.enabled=TRUE
			RETURN
		END IF
	ELSEIF ls_ttl="" THEN
	//	dw_book_query.dataobject='d_book_query_ln_fn_serttl_lk'
		where_clause = " AND upper(auth) like "+ "~~'%" + ls_ln + "%~~'" +" and upper(authfn) "+&
		"like "+"~~'%" + ls_fn + "%~~'" +" and upper(serttl) like "+"~~'%" + ls_serttl + "%~~'" 
		mod_string = "DataWindow.Table.Select='" + original_select + where_clause +"'"
//		messagebox("mod_string",mod_string)
		rc = dw_book_query.Modify(mod_string)
		IF rc = "" THEN
			li_row_count = dw_book_query.Retrieve()
			IF li_row_count < 1 THEN 
				Messagebox("ERROR", "There is not such last name , first name"+&
							" and serttl in database. Try another one.",stopSign!)
			END IF
		ELSE
			Messagebox('Error','Select statement is wrong: '+rc,stopSign!)
			cb_clear.enabled=TRUE
			RETURN
		END IF
	ELSEIF ls_serttl="" THEN
	//	dw_book_query.dataobject='d_book_query_ln_fn_ttl_lk'
		where_clause = " AND upper(auth) like "+ "~~'%" + ls_ln + "%~~'" +" and upper(authfn) "+&
		"like "+"~~'%" + ls_fn + "%~~'" +" and upper(ttl) like "+"~~'%" + ls_ttl + "%~~'" 
		mod_string = "DataWindow.Table.Select='" + original_select + where_clause +"'"
//		messagebox("mod_string",mod_string)
		rc = dw_book_query.Modify(mod_string)
		IF rc = "" THEN
			li_row_count = dw_book_query.Retrieve()
			IF li_row_count < 1 THEN 
				Messagebox("ERROR", "There is not such last name , first name"+&
							" and ttl in database. Try another one.",stopSign!)
			END IF
		ELSE
			Messagebox('Error','Select statement is wrong: '+rc,stopSign!)
			cb_clear.enabled=TRUE
			RETURN
		END IF
	ELSE
	//	dw_book_query.dataobject='d_book_query_ln_fn_ttl_serttl_lk'
		where_clause = " AND upper(auth) like "+ "~~'%" + ls_ln + "%~~'" +" and upper(authfn) "+&
		"like "+"~~'%" + ls_fn + "%~~'" +" and upper(ttl) like "+"~~'%" + ls_ttl + "%~~'" +&
		" and upper(serttl) like "+"~~'%" + ls_serttl + "%~~'" 
		mod_string = "DataWindow.Table.Select='" + original_select + where_clause +"'"
//		messagebox("mod_string",mod_string)
		rc = dw_book_query.Modify(mod_string)
		IF rc = "" THEN
			li_row_count = dw_book_query.Retrieve()
			IF li_row_count < 1 THEN 
				Messagebox("ERROR", "There is not such last name , first name"+&
							" and ttl, serttl in database. Try another one.",stopSign!)
			END IF
		ELSE
			Messagebox('Error','Select statement is wrong: '+rc,stopSign!)
			cb_clear.enabled=TRUE
			RETURN
		END IF
	END IF
END IF//end if rb_keyword.checked=true
IF rb_soundex.checked=TRUE THEN
	ls_sndx_auth=f_soundex(ls_ln + ls_fn,'auth')
	ls_sndx_ttl= f_soundex(ls_ttl, 'ttl')
	ls_sndx_auth=Trim(ls_sndx_auth)
	ls_sndx_ttl=Trim(ls_sndx_ttl)
	IF ls_ln + ls_fn<>"" AND ls_ttl<> "" THEN
		where_clause = " AND soundex_auth like "+ "~~'" + ls_sndx_auth + "%~~'"+" and soundex_ttl "+&
		"like "+"~~'" + ls_sndx_ttl + "%~~'"
		mod_string = "DataWindow.Table.Select='" + original_select + where_clause +"'"
//		messagebox("mod_string",mod_string)
		rc = dw_book_query.Modify(mod_string)
		IF rc = "" THEN
			li_row_count = dw_book_query.Retrieve()
			IF li_row_count < 1 THEN 
				Messagebox("ERROR", "There is not such name"+&
							" and ttl in database. Try another one.",stopSign!)
			END IF
		ELSE
			Messagebox('Error','Select statement is wrong: '+rc,stopSign!)
			cb_clear.enabled=TRUE
			RETURN
		END IF
	ELSEIF ls_ln+ ls_fn<>"" THEN
		where_clause = " AND soundex_auth like "+ "~~'" + ls_sndx_auth + "%~~'"
		mod_string = "DataWindow.Table.Select='" + original_select + where_clause +"'"
//		messagebox("mod_string",mod_string)
		rc = dw_book_query.Modify(mod_string)
		IF rc = "" THEN
			li_row_count = dw_book_query.Retrieve()
			IF li_row_count < 1 THEN 
				Messagebox("ERROR", "There is not such name"+&
							" in database. Try another one.",stopSign!)
			END IF
		ELSE
			Messagebox('Error','Select statement is wrong: '+rc,stopSign!)
			cb_clear.enabled=TRUE
			RETURN
		END IF
	ELSEIF ls_ttl<> "" THEN
		where_clause = " AND soundex_ttl like "+ "~~'" + ls_sndx_ttl + "%~~'"
		mod_string = "DataWindow.Table.Select='" + original_select + where_clause +"'"
//		messagebox("mod_string",mod_string)
		rc = dw_book_query.Modify(mod_string)
		IF rc = "" THEN
			li_row_count = dw_book_query.Retrieve()
			IF li_row_count < 1 THEN 
				Messagebox("ERROR", "There is not such name"+&
							" in database. Try another one.",stopSign!)
			END IF
		ELSE
			Messagebox('Error','Select statement is wrong: '+rc,stopSign!)
			cb_clear.enabled=TRUE
			RETURN
		END IF
	ELSEIF ls_ln+ ls_fn="" AND ls_ttl=""THEN
		Messagebox('Error','You must enter name and ttl, can not leave both empty.',stopSign!)
		cb_clear.enabled=TRUE
		RETURN
	END IF
END IF// end if rb_soundex.checked=true
wf_endpart(li_row_count)

end event

type gb_1 from groupbox within w_books_query
integer x = 23
integer y = 316
integer width = 837
integer height = 272
integer taborder = 190
integer textsize = -8
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 33554432
long backcolor = 80263581
string text = "Sort By"
end type

type dw_book_query from u_dw within w_books_query
event pfc_hinttext pbm_mousemove
event ue_enterkey pbm_dwnprocessenter
event pfc_keydown pbm_dwnkey
integer x = 14
integer y = 628
integer width = 2670
integer height = 556
integer taborder = 90
string dataobject = "d_book_query"
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
datetime Lschstdt,Lactstdt,Lactenddt
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

event doubleclicked;call super::doubleclicked;string ls_conno

ls_conno=dw_book_query.GetItemString(row,'conno')
//OpenSheetWithParm(w_books_query_results,ls_chno,w_pics_main)
//OpenSheetWithParm ( sheetrefvar, parameter {, windowtype }, mdiframe
//	{, position {, arrangeopen } } )
//OpenSheet ( sheetrefvar {, windowtype }, mdiframe {, position 
//	{, arrangeopen } } )
OpenSheetwithParm(w_books_query_results,ls_conno,w_pics_main, 0, Original!)
end event

type dw_1 from u_dw within w_books_query
event pfc_hinttext pbm_mousemove
event ue_enterkey pbm_dwnprocessenter
event pfc_keydown pbm_dwnkey
boolean visible = false
integer x = 9
integer y = 620
integer width = 2670
integer height = 556
integer taborder = 100
string dataobject = "d_test_customer"
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

event doubleclicked;call super::doubleclicked;string ls_conno

ls_conno=dw_book_query.GetItemString(row,'conno')
//OpenSheetWithParm(w_books_query_results,ls_chno,w_pics_main)
//OpenSheetWithParm ( sheetrefvar, parameter {, windowtype }, mdiframe
//	{, position {, arrangeopen } } )
//OpenSheet ( sheetrefvar {, windowtype }, mdiframe {, position 
//	{, arrangeopen } } )
OpenSheetwithParm(w_books_query_results,ls_conno,w_pics_main, 0, Original!)
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
datetime Lschstdt,Lactstdt,Lactenddt
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


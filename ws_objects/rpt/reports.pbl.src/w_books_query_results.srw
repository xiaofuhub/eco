$PBExportHeader$w_books_query_results.srw
forward
global type w_books_query_results from w_sheet
end type
type tab_1 from uo_tab_pics within w_books_query_results
end type
type tab_1 from uo_tab_pics within w_books_query_results
end type
type st_total from statictext within w_books_query_results
end type
type sle_book# from u_sle within w_books_query_results
end type
type cb_print from u_cb within w_books_query_results
end type
type cb_cancel from u_cb within w_books_query_results
end type
type cb_return from u_cb within w_books_query_results
end type
end forward

global type w_books_query_results from w_sheet
integer width = 3342
integer height = 1604
string title = "Books Query Results"
long backcolor = 81576884
tab_1 tab_1
st_total st_total
sle_book# sle_book#
cb_print cb_print
cb_cancel cb_cancel
cb_return cb_return
end type
global w_books_query_results w_books_query_results

type variables
//str_distrib_schedule istr_add
//string is_add='N'
string is_ogn_slt
long i_count=0, i_newidx=0
end variables

on w_books_query_results.create
int iCurrent
call super::create
this.tab_1=create tab_1
this.st_total=create st_total
this.sle_book#=create sle_book#
this.cb_print=create cb_print
this.cb_cancel=create cb_cancel
this.cb_return=create cb_return
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.tab_1
this.Control[iCurrent+2]=this.st_total
this.Control[iCurrent+3]=this.sle_book#
this.Control[iCurrent+4]=this.cb_print
this.Control[iCurrent+5]=this.cb_cancel
this.Control[iCurrent+6]=this.cb_return
end on

on w_books_query_results.destroy
call super::destroy
destroy(this.tab_1)
destroy(this.st_total)
destroy(this.sle_book#)
destroy(this.cb_print)
destroy(this.cb_cancel)
destroy(this.cb_return)
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

event pfc_postopen;call super::pfc_postopen;long i,li_row_count, i2,libkseq
string ls_serttl, ls_sort, ls_conno,lsbkmed, lssdflag, lscntr, LS_ARG[], ls_med // 12/09/2008 Media mix up in reports

THIS.Windowstate = maximized!
ls_conno=message.StringParm
m_pics_main.m_Edit.m_Addrow.Enabled = FALSE
m_pics_main.m_Edit.m_Deleterow.Enabled = FALSE
m_pics_main.m_Edit.m_Cut.Enabled = FALSE

tab_1.pg_bib.dw_bib.SetTransObject(SqlServerTrans)
li_row_count=tab_1.pg_bib.dw_bib.Retrieve(ls_conno)
tab_1.pg_bib.dw_bib.SelectRow(0,false)
//tab_1.pg_bib.dw_bib.SelectRow(1,true)
tab_1.pg_bib.dw_bib.ScrollToRow(1)
tab_1.pg_bib.dw_bib.SetRow(1)
tab_1.pg_bib.dw_bib.SetFocus()
sle_book#.Text=string(li_row_count)

tab_1.pg_prod.dw_prod.SetTransObject(SqlServerTrans)

//12/09/2008 Media mix up in reports eliminated, book media(s) passed to reports
//li_row_count=tab_1.pg_prod.dw_prod.Retrieve(ls_conno)

SELECT MED
INTO :ls_med
FROM MCHAR
WHERE conno = :ls_conno USING SQLSERVERTRANS ;

IF not isnull(ls_med) THEN
	CHOOSE CASE ls_med
			// #2238 same as 2227 09/16/2009
		CASE 'BR' ,'P/B' // #2227 7/29/2009 exception error when p/b  media is not supplied ls_arg is 0
			ls_arg[1] = 'BR'
		CASE 'RTB', 'RC'
			ls_arg[1] = 'RC'
			ls_arg[2] = 'DB'
	END CHOOSE
END IF
li_row_count=tab_1.pg_prod.dw_prod.Retrieve(ls_conno ,ls_arg[])
//// 12/09/2008

if li_row_count>0 then
	libkseq=tab_1.pg_prod.dw_prod.object.bkseq[1]
	lsbkmed=tab_1.pg_prod.dw_prod.object.bkmed[1]	
end if
tab_1.pg_prod.dw_ext.SetTransObject(SqlServerTrans)
li_row_count=tab_1.pg_prod.dw_ext.Retrieve(libkseq,lsbkmed)


tab_1.pg_cy.dw_cy.SetTransObject(SqlServerTrans)
li_row_count=tab_1.pg_cy.dw_cy.Retrieve(ls_conno)

tab_1.pg_qa.dw_qa.SetTransObject(SqlServerTrans)
//li_row_count=tab_1.pg_qa.dw_qa.Retrieve(ls_conno)
//12/09/2008 media mix up issue solved
li_row_count=tab_1.pg_qa.dw_qa.Retrieve(ls_conno, ls_arg[])

tab_1.pg_inv.dw_inv.SetTransObject(SqlServerTrans)
li_row_count=tab_1.pg_inv.dw_inv.Retrieve(ls_conno)
end event

event pfc_preopen;call super::pfc_preopen;this.of_SetPreference(TRUE)
this.inv_preference.of_SetToolBars(TRUE)
this.inv_preference.of_SetWindow(TRUE)
this.of_SetResize(TRUE)
this.inv_resize.of_SetOrigSize(this.workspacewidth(),this.workspaceheight())
inv_resize.of_Register(cb_cancel, "scale")
inv_resize.of_Register(cb_return, "scale")
inv_resize.of_Register(cb_print, "scale")
inv_resize.of_Register(sle_book#, "scale")
inv_resize.of_Register(st_total, "scale")
inv_resize.of_Register(tab_1, "scale")
inv_resize.of_Register(tab_1.pg_bib.dw_bib, "scale")
inv_resize.of_Register(tab_1.pg_prod.dw_prod, "scale")
inv_resize.of_Register(tab_1.pg_prod.dw_ext, "scale")

inv_resize.of_Register(tab_1.pg_qa.dw_qa, "scale")
inv_resize.of_Register(tab_1.pg_cy.dw_cy, "scale")
inv_resize.of_Register(tab_1.pg_inv.dw_inv, "scale")
end event

event resize;call super::resize;//long ll_height
//
//This.X = w_pics_main.X
//This.Y = w_pics_main.Y
//ll_height = w_pics_main.mdi_1.Height
//This.Resize(w_pics_main.WorkSpaceWidth(), ll_height)
end event

type tab_1 from uo_tab_pics within w_books_query_results
integer x = 27
integer y = 4
integer width = 3250
integer taborder = 10
end type

event selectionchanged;call super::selectionchanged;long li_row_count
i_newidx= newindex
if newindex=1 then
	li_row_count=tab_1.pg_bib.dw_bib.RowCount()
	if li_row_count>0 then
		tab_1.pg_bib.dw_bib.SelectRow(0,false)
		tab_1.pg_bib.dw_bib.ScrollToRow(1)
		tab_1.pg_bib.dw_bib.SetRow(1)
		tab_1.pg_bib.dw_bib.SetFocus()
	end if
elseif newindex=2 then
	li_row_count=tab_1.pg_prod.dw_prod.RowCount()
	if li_row_count>0 then
		tab_1.pg_prod.dw_prod.SelectRow(0,false)
		tab_1.pg_prod.dw_prod.ScrollToRow(1)
		tab_1.pg_prod.dw_prod.SetRow(1)
		tab_1.pg_prod.dw_prod.SetFocus()
	end if	
elseif newindex=3 then
	li_row_count=tab_1.pg_cy.dw_cy.RowCount()
	if li_row_count>0 then
		tab_1.pg_cy.dw_cy.SelectRow(0,false)
		tab_1.pg_cy.dw_cy.ScrollToRow(1)
		tab_1.pg_cy.dw_cy.SetRow(1)
		tab_1.pg_cy.dw_cy.SetFocus()
	end if
elseif newindex=4 then
	li_row_count=tab_1.pg_qa.dw_qa.RowCount()
	if li_row_count>0 then
		tab_1.pg_qa.dw_qa.SelectRow(0,false)
		tab_1.pg_qa.dw_qa.ScrollToRow(1)
		tab_1.pg_qa.dw_qa.SetRow(1)
		tab_1.pg_qa.dw_qa.SetFocus()
	end if
elseif newindex=5 then
	li_row_count=tab_1.pg_inv.dw_inv.RowCount()
	if li_row_count>0 then
		tab_1.pg_inv.dw_inv.SelectRow(0,false)
		tab_1.pg_inv.dw_inv.ScrollToRow(1)
		tab_1.pg_inv.dw_inv.SetRow(1)
		tab_1.pg_inv.dw_inv.SetFocus()
	end if
end if
	
sle_book#.Text=string(li_row_count)



end event

type st_total from statictext within w_books_query_results
boolean visible = false
integer x = 133
integer y = 1304
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

type sle_book# from u_sle within w_books_query_results
boolean visible = false
integer x = 658
integer y = 1300
integer width = 265
integer height = 76
integer taborder = 0
end type

type cb_print from u_cb within w_books_query_results
event pfc_hinttext pbm_mousemove
string tag = "Save data to oracle data base"
integer x = 1221
integer y = 1300
integer width = 169
integer taborder = 50
fontcharset fontcharset = ansi!
string text = "&Print"
end type

event pfc_hinttext;call super::pfc_hinttext;w_pics_main.setmicrohelp(This.tag)
end event

event clicked;call super::clicked;long li_row_count
//nvo_PowerPrn.of_SetPrinterOrientation(2)
if i_newidx=1 then
	li_row_count=tab_1.pg_bib.dw_bib.RowCount()
	if li_row_count>0 then
		tab_1.pg_bib.dw_bib.triggerEvent('pfc_print')
	end if 
elseif i_newidx=2 then
	li_row_count=tab_1.pg_prod.dw_prod.RowCount()
	if li_row_count>0 then
		tab_1.pg_prod.dw_prod.triggerEvent('pfc_print')
	end if
elseif i_newidx=3 then
	li_row_count=tab_1.pg_cy.dw_cy.RowCount()
	if li_row_count>0 then
		tab_1.pg_cy.dw_cy.triggerEvent('pfc_print')
	end if
elseif i_newidx=4 then
	li_row_count=tab_1.pg_qa.dw_qa.RowCount()
	if li_row_count>0 then
		tab_1.pg_qa.dw_qa.triggerEvent('pfc_print')
	end if
elseif i_newidx=5 then
	li_row_count=tab_1.pg_inv.dw_inv.RowCount()
	if li_row_count>0 then
		tab_1.pg_inv.dw_inv.triggerEvent('pfc_print')
	end if
end if
//nvo_PowerPrn.of_SetPrinterOrientation(1)
end event

type cb_cancel from u_cb within w_books_query_results
event pfc_hinttext pbm_mousemove
string tag = "Exits the screen"
integer x = 2245
integer y = 1300
integer width = 238
integer taborder = 100
fontcharset fontcharset = ansi!
string text = "&Exit"
end type

event pfc_hinttext;call super::pfc_hinttext;w_pics_main.setmicrohelp(This.tag)
end event

event clicked;call super::clicked;close(w_books_query)
parent.Event pfc_close()
//m_pics_main.m_menu.PopMenu(300, 0)

end event

type cb_return from u_cb within w_books_query_results
integer x = 1586
integer y = 1300
integer width = 462
integer taborder = 80
fontcharset fontcharset = ansi!
string text = "&Return To Search"
end type

event clicked;call super::clicked;parent.Event pfc_close()
end event


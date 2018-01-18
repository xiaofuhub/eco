$PBExportHeader$w_sheet_catalog.srw
forward
global type w_sheet_catalog from w_sheet
end type
type st_3 from statictext within w_sheet_catalog
end type
type st_displayed from statictext within w_sheet_catalog
end type
type st_2 from statictext within w_sheet_catalog
end type
type st_modified from statictext within w_sheet_catalog
end type
type cb_exp from commandbutton within w_sheet_catalog
end type
type pb_print from commandbutton within w_sheet_catalog
end type
type pb_update from commandbutton within w_sheet_catalog
end type
type pb_exit from commandbutton within w_sheet_catalog
end type
type dw_catflag from datawindow within w_sheet_catalog
end type
type dw_catalog from u_pics_dw within w_sheet_catalog
end type
end forward

global type w_sheet_catalog from w_sheet
integer x = 5
integer y = 108
integer width = 2898
integer height = 1576
string title = "Cataloging Final Approval"
st_3 st_3
st_displayed st_displayed
st_2 st_2
st_modified st_modified
cb_exp cb_exp
pb_print pb_print
pb_update pb_update
pb_exit pb_exit
dw_catflag dw_catflag
dw_catalog dw_catalog
end type
global w_sheet_catalog w_sheet_catalog

type variables
string  where_clause, Catdate
end variables

forward prototypes
public subroutine wf_disable_buttons ()
public subroutine wf_enable_buttons ()
public subroutine wf_set_counts (integer n_rows)
end prototypes

public subroutine wf_disable_buttons ();pb_exit.enabled = FALSE
pb_update.enabled = FALSE
pb_print.enabled = FALSE
cb_exp.enabled = FALSE

end subroutine

public subroutine wf_enable_buttons ();pb_exit.enabled = TRUE
pb_update.enabled = TRUE
pb_print.enabled = TRUE
cb_exp.enabled = TRUE

end subroutine

public subroutine wf_set_counts (integer n_rows);st_modified.text=String(n_rows)
st_displayed.text=String(dw_catalog.RowCount())
end subroutine

event closequery;Integer	li_pendingrc
Integer	li_validationrc
Integer	li_accepttextrc
Integer	li_msg
Integer	li_rc,rtn
String	ls_msgparms[]

// Check if the CloseQuery process has been disabled
If ib_disableclosequery Then
	Return 0
End If

// Call event to perform any pre-CloseQuery processing
If This.Event pfc_preclose ( ) <> 1 Then
	// Prevent the window from closing
	Return 1  
End If

// Prevent validation error messages from appearing while the window is closing
// and allow others to check if the  CloseQuery process is in progress
ib_closestatus = True

// Check for any pending updates
li_rc = of_UpdateChecks()
If li_rc = 0 Then
	// Updates are NOT pending, allow the window to be closed.
	Return 0
ElseIf li_rc < 0 Then
	// There are Updates pending, but at least one data entry error was found.
	// Give the user an opportunity to close the window without saving changes
	If IsValid(gnv_app.inv_error) Then
		li_msg = gnv_app.inv_error.of_Message('pfc_closequery_failsvalidation', &
					 ls_msgparms, gnv_app.iapp_object.DisplayName)
	Else
		li_msg = MessageBox (gnv_app.iapp_object.DisplayName, &
					"The information entered does not pass validation and "  + &
					"must be corrected before changes can be saved.~r~n~r~n" + &
					"Close without saving changes?", &
					exclamation!, YesNo!, 2)
	End If
	If li_msg = 1 Then
		Return 0
	End If
Else
	// Changes are pending, prompt the user to determine if they should be saved
	If IsValid(gnv_app.inv_error) Then
		li_msg = gnv_app.inv_error.of_Message('pfc_closequery_savechanges',  &
					ls_msgparms, gnv_app.iapp_object.DisplayName)		
	Else
//		li_msg = MessageBox ( gnv_app.iapp_object.DisplayName, &
//					"Do you want to save changes?", exclamation!, YesNoCancel!)
		Return 0
	End If
	Choose Case li_msg
		Case 1
			// YES - Update
			// If the update fails, prevent the window from closing
			rtn = pb_update.Event Clicked()
			If rtn = 1 Then
				// Successful update, allow the window to be closed
				Return 0
			End If
		Case 2
			// NO - Allow the window to be closed without saving changes
			Return 0
		Case 3
			// CANCEL -  Prevent the window from closing
	End Choose
End If

// Prevent the window from closing
ib_closestatus = False
Return 1
end event

on w_sheet_catalog.create
int iCurrent
call super::create
this.st_3=create st_3
this.st_displayed=create st_displayed
this.st_2=create st_2
this.st_modified=create st_modified
this.cb_exp=create cb_exp
this.pb_print=create pb_print
this.pb_update=create pb_update
this.pb_exit=create pb_exit
this.dw_catflag=create dw_catflag
this.dw_catalog=create dw_catalog
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_3
this.Control[iCurrent+2]=this.st_displayed
this.Control[iCurrent+3]=this.st_2
this.Control[iCurrent+4]=this.st_modified
this.Control[iCurrent+5]=this.cb_exp
this.Control[iCurrent+6]=this.pb_print
this.Control[iCurrent+7]=this.pb_update
this.Control[iCurrent+8]=this.pb_exit
this.Control[iCurrent+9]=this.dw_catflag
this.Control[iCurrent+10]=this.dw_catalog
end on

on w_sheet_catalog.destroy
call super::destroy
destroy(this.st_3)
destroy(this.st_displayed)
destroy(this.st_2)
destroy(this.st_modified)
destroy(this.cb_exp)
destroy(this.pb_print)
destroy(this.pb_update)
destroy(this.pb_exit)
destroy(this.dw_catflag)
destroy(this.dw_catalog)
end on

event pfc_preopen;call super::pfc_preopen;this.of_SetPreference(TRUE)
this.inv_preference.of_SetToolBars(TRUE)
this.inv_preference.of_SetWindow(TRUE)
this.of_SetResize(TRUE)
this.inv_resize.of_SetOrigSize(this.workspacewidth(),this.workspaceheight())
inv_resize.of_Register(dw_catalog, "Scale")
inv_resize.of_Register(pb_exit, "Scale")
inv_resize.of_Register(pb_update, "Scale")
inv_resize.of_Register(pb_print, "Scale")
inv_resize.of_Register(cb_exp, "Scale")
inv_resize.of_Register(st_3, "Scale")
inv_resize.of_Register(st_2, "Scale")
inv_resize.of_Register(st_displayed, "Scale")
inv_resize.of_Register(st_modified, "Scale")

end event

event resize;call super::resize;long ll_height

This.X = w_pics_main.X
This.Y = w_pics_main.Y
ll_height = w_pics_main.mdi_1.Height
This.Resize(w_pics_main.WorkSpaceWidth(), ll_height)


end event

event pfc_print;call super::pfc_print;pb_print.TriggerEvent(Clicked!)
return 1
end event

event open;call super::open;m_pics_main.m_file.m_print.Enabled 			=	TRUE
m_pics_main.m_file.m_pagesetup.Enabled		=	TRUE
m_pics_main.m_file.m_printimmediate.Enabled	=	TRUE

end event

type st_3 from statictext within w_sheet_catalog
integer x = 50
integer y = 1180
integer width = 375
integer height = 72
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long backcolor = 79741120
boolean enabled = false
string text = "Rows Selected:"
alignment alignment = right!
boolean focusrectangle = false
end type

type st_displayed from statictext within w_sheet_catalog
integer x = 466
integer y = 1180
integer width = 123
integer height = 88
integer textsize = -10
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 16777215
boolean enabled = false
boolean border = true
borderstyle borderstyle = stylelowered!
boolean focusrectangle = false
end type

type st_2 from statictext within w_sheet_catalog
integer x = 631
integer y = 1180
integer width = 361
integer height = 72
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long backcolor = 79741120
boolean enabled = false
string text = "Rows Modified:"
alignment alignment = right!
boolean focusrectangle = false
end type

type st_modified from statictext within w_sheet_catalog
integer x = 1038
integer y = 1180
integer width = 128
integer height = 96
integer textsize = -10
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 16777215
boolean enabled = false
boolean border = true
borderstyle borderstyle = stylelowered!
boolean focusrectangle = false
end type

type cb_exp from commandbutton within w_sheet_catalog
event clicked pbm_bnclicked
integer x = 1431
integer y = 1304
integer width = 306
integer height = 108
integer taborder = 40
integer textsize = -10
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
boolean enabled = false
string text = "Expor&t.."
end type

event clicked;string mod_string,selectstmt
integer rtn

rtn = MessageBox("Export","Would you like to export all the records that are CATALOGED?",Question!,YesNo!,1)
IF rtn = 2 THEN
	rtn = MessageBox("Export","Would you like to export all the records that are CATALOGED/NOT CATALOGED?",Question!,YesNo!,1)	
	IF rtn = 1 THEN
		// Build the select statement to select data from hidden datawindow dw_catflag
		selectstmt = "SELECT catalog.catflag, mchar.conno, mchar.bkseq, mchar.bkmed, ttlinit.ttl, catalog.s2out, catalog.cat "
		selectstmt = selectstmt + " FROM catalog, mchar, ttlinit "
		selectstmt = selectstmt + " WHERE ( catalog.conno = mchar.conno ) and ( mchar.chno = ttlinit.chno ) and ( catalog.s2out is not NULL )"
		selectstmt = selectstmt + where_clause
		mod_string = "DataWindow.Table.Select=~'" + selectstmt + "~'"
	ELSE
		RETURN
	END IF
ELSE
	// Build the select statement to select data from hidden datawindow dw_catflag
	// based on catflag is 'Y' and cat is set to a date.

	selectstmt = "SELECT catalog.catflag, mchar.conno, mchar.bkseq, mchar.bkmed, ttlinit.ttl, catalog.s2out, catalog.cat "
	selectstmt = selectstmt + " FROM catalog, mchar, ttlinit "
	selectstmt = selectstmt + " WHERE ( catalog.conno = mchar.conno ) and ( mchar.chno = ttlinit.chno ) and ( catalog.s2out is not NULL )"
	selectstmt = selectstmt + where_clause
	selectstmt = selectstmt + " and (catalog.catflag=~'Y~' )"
	mod_string = "DataWindow.Table.Select=~"" + selectstmt + "~""
END IF

// Copy the dataobject from datawindow catalog into catflag.
dw_catflag.DataObject = dw_catalog.DataObject

// Start the transaction for dw_catflag
dw_catflag.SetTransObject( SQLServerTrans )

//messagebox("mod_string",mod_string)

// Modify the datawindow with the modified select statement.
dw_catflag.Modify(mod_string)

// Set the microhelp
SetMicroHelp(w_pics_main,"Selecting the catalog records, Please wait...")

// disable the buttons
w_sheet_catalog.wf_disable_buttons()

// retrieve the data
rtn = dw_catflag.Retrieve()

IF rtn > 1 THEN
	// If any record were found, bring up the save as dialog box.
	dw_catflag.SaveAs()
ELSE
	// Display the error message that no data was found.
	MessageBox("ERROR Exporting", "There are no data to export!", Information!)
END IF

// Reset the microhelp
SetMicroHelp(w_pics_main,"")

// Enable the buttons
w_sheet_catalog.wf_enable_buttons()

end event

type pb_print from commandbutton within w_sheet_catalog
event clicked pbm_bnclicked
integer x = 1819
integer y = 1304
integer width = 247
integer height = 108
integer taborder = 10
integer textsize = -10
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
boolean enabled = false
string text = "&Print"
end type

event clicked;long job
int Ans

Ans = MessageBox("Print", "Print Uncataloged Records?", Question!, OKCancel!, 2)
if Ans = 1 THEN 
  job = PrintOpen( ) 
  PrintDataWindow(job, dw_catalog) 
  PrintClose(job)
else
  return
end if

end event

type pb_update from commandbutton within w_sheet_catalog
event clicked pbm_bnclicked
integer x = 2162
integer y = 1304
integer width = 311
integer height = 108
integer taborder = 60
integer textsize = -10
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
boolean enabled = false
string text = "&Update"
end type

event clicked;String Lcatflag,Lconno
Date Lcat
int rtn,i,rowcount,ll_rows
dwItemStatus cat_status

dw_catalog.AcceptText()

w_sheet_catalog.wf_disable_buttons()

IF (modifiedcount(dw_catalog) < 1) THEN
  MessageBox("Apply","Nothing to update!",Information!)
  w_sheet_catalog.wf_enable_buttons()
  RETURN 1
ELSE
	SetPointer(HourGlass!)
	SetMicroHelp(w_pics_main,"Updating database, Please wait...")
	
	ll_Rows = dw_catalog.ModifiedCount() 
	
	rtn = dw_catalog.Event pfc_update(TRUE,TRUE)
	IF rtn = 1 THEN
		FOR i= 1 TO dw_catalog.RowCount()
			cat_status = dw_catalog.GetItemStatus(i, 0, Primary!)
			IF cat_status = DataModified! THEN
				// Mark MCHAR Table
				Lconno = dw_catalog.Object.cconno[i]
				f_update_mchar_time(Lconno,0,"C","U")
			END IF
		NEXT
		COMMIT USING SQLServerTrans;
		MessageBox("Update","Update successful.",Information!)
	ELSE
		MessageBox("ERROR","Update failed.",StopSign!)
		ROLLBACK USING SQLServerTrans;
	END IF
	
	SetMicroHelp(w_pics_main,"")
	w_sheet_catalog.wf_enable_buttons()
	w_sheet_catalog.wf_set_counts(ll_rows)
	SetPointer(Arrow!)
	RETURN 1
END IF
end event

type pb_exit from commandbutton within w_sheet_catalog
event clicked pbm_bnclicked
integer x = 2569
integer y = 1304
integer width = 247
integer height = 108
integer taborder = 20
integer textsize = -10
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "E&xit"
end type

event clicked;close(parent)
m_pics_main.m_menu.PopMenu(300, 0)

end event

type dw_catflag from datawindow within w_sheet_catalog
boolean visible = false
integer x = 123
integer y = 836
integer width = 2569
integer height = 192
integer taborder = 30
boolean enabled = false
string dataobject = "d_bcs_cat"
end type

event constructor;this.SetTransObject( SQLServerTrans )

end event

type dw_catalog from u_pics_dw within w_sheet_catalog
event doubleclicked pbm_dwnlbuttondblclk
event itemchanged pbm_dwnitemchange
event rbuttondown pbm_dwnrbuttondown
event rowfocuschanged pbm_dwnrowchange
event rbuttonup pbm_dwnrbuttonup
event ue_postconstructor ( )
integer x = 32
integer y = 24
integer width = 2793
integer height = 1096
integer taborder = 50
string dataobject = "d_bcs_cat"
boolean hscrollbar = true
end type

event doubleclicked;call super::doubleclicked;if this.GetColumnName() = 'cconno' then
	OpenSheet(w_sheet_conno_info, w_pics_main, 0, Original!)
end if
end event

event itemchanged;call super::itemchanged;date null_date
SetNull(null_date)
string setting

setting = this.Describe("DataWindow.QueryMode")
if setting = 'no' then
 CHOOSE CASE this.GetColumnName()
	CASE 'ccatflag'
     if this.GetText() = 'Y' then
       this.SetItem(this.GetRow(), "ccat", date(Catdate))
//       this.SetColumn("ccat")
     else
       this.SetItem(this.GetRow(), "ccat", null_date)
     end if
 END CHOOSE
end if
end event

event rbuttondown;//
end event

event rowfocuschanged;//////////////////////////////////////////////////////////////////////////////
//
//	Event:  rowfocuschanged
//
//	Description:  Send rowfocuschanged notification to services
//
//////////////////////////////////////////////////////////////////////////////
//	
//	Revision History
//
//	Version
//	5.0   Initial version
//
//////////////////////////////////////////////////////////////////////////////
//
//	Copyright © 1996 Powersoft Corporation.  All Rights Reserved.
//	Any distribution of the PowerBuilder Foundation Classes (PFC)
//	source code by other than Powersoft is prohibited.
//
//////////////////////////////////////////////////////////////////////////////

/* Linkage Service */
IF IsValid ( inv_Linkage ) THEN 
	inv_Linkage.Event pfc_RowFocusChanged (currentrow) 
END IF 

dw_catalog.Modify("cat.TabSequence = 0")

end event

event rbuttonup;//
end event

event ue_postconstructor;call super::ue_postconstructor;string mod_string,original_select,rc,Ls2outfrom,Ls2outto
int ll_rows,RowNum

SetMicroHelp(parent,"Please wait...")

this.SetTransObject( SQLServerTrans )
original_select =	this.Describe("DataWindow.Table.Select")

IF IsValid(w_stage2_dateout) AND IsValid(w_s2dateout)=FALSE THEN
	Ls2outfrom = w_stage2_dateout.S2dateoutfrom
	Ls2outto   = w_stage2_dateout.S2dateoutto
	Catdate   = w_stage2_dateout.Catdate
	
	IF IsNull(Ls2outto)=FALSE AND IsNull(Ls2outfrom)=FALSE THEN
		IF Ls2outfrom > Ls2outto THEN
			MessageBox("Error","Date range is wrong!")
			close(parent)
			RETURN
		END IF
	END IF
	IF (IsNull(Ls2outfrom)=FALSE AND IsNull(Ls2outto)=FALSE) THEN
		where_clause = " AND TO_CHAR(catalog.s2out,"+"~'"+"MM/DD/YYYY"+"~') BETWEEN "+ "~'" + LS2outfrom + "~'" + " AND "+ "~'" + LS2outto + "~'"
	ELSEIF (IsNull(Ls2outfrom)=TRUE AND IsNull(Ls2outto)=FALSE) THEN
		where_clause = " AND TO_CHAR(catalog.s2out,"+"~'"+"MM/DD/YYYY"+"~') <="+ "~'" + LS2outto + "~'"
	ELSEIF (IsNull(Ls2outfrom)=FALSE AND IsNull(Ls2outto)=TRUE) THEN
		where_clause = " AND TO_CHAR(catalog.s2out,"+"~'"+"MM/DD/YYYY"+"~') ="+ "~'" + LS2outfrom + "~'"
	END IF
	
ELSEIF IsValid(w_s2dateout) AND IsValid(w_stage2_dateout) THEN
	Ls2outfrom = w_s2dateout.S2dateout
	Catdate   = w_s2dateout.Catdate
	where_clause = " AND TO_CHAR(catalog.s2out,"+"~'"+"MM/DD/YYYY"+"~') ="+ "~'" + LS2outfrom + "~'"
END IF

mod_string = "DataWindow.Table.Select=~"" + original_select + where_clause + "~""
rc = this.Modify(mod_string)
IF rc = "" THEN
	ll_rows = this.Retrieve()
   IF ll_rows < 1 THEN 
     MessageBox("Catalog Error", "All the records for stage II date out of: " + "~'" +string(LS2outfrom)+ "~'" +" have been cataloged.",StopSign!, OK!, 2)
     close(parent)
     return
	ELSE
     wf_set_counts(0)
	  wf_enable_buttons()
     this.SetFocus()
	  
	  if IsValid(w_stage2_dateout) AND IsValid(w_s2dateout)=FALSE then
 		close(w_stage2_dateout)
	  elseif IsValid(w_s2dateout) AND IsValid(w_stage2_dateout) then
 		close(w_stage2_dateout)
 		close(w_s2dateout)
	  end if
	  
   END IF
ELSE
	MessageBox("Status", "Modify Failed" + rc)
END IF

SetMicroHelp(parent,"")

end event

event sqlpreview;call super::sqlpreview;//messagebox("sql",sqlsyntax)
end event

event constructor;call super::constructor;this.of_SetFilter(TRUE)
this.of_SetFind(TRUE)
this.inv_filter.of_SetStyle(1)
this.inv_filter.of_SetColumnNameSource(2)
this.of_SetSort(TRUE)
this.inv_sort.of_SetStyle(1)
this.inv_sort.of_SetColumnNameSource(2)

end event

event retrievestart;call super::retrievestart;OpenWithParm(w_pics_retrieve_msg_box,"Retrieving Catalog data, Please Wait...")
end event

event retrieveend;call super::retrieveend;close(w_pics_retrieve_msg_box)
end event

event pfc_deleterow;//
RETURN 1
end event

event pfc_addrow;//
RETURN 1
end event


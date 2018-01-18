$PBExportHeader$w_sheet_spanish_books.srw
forward
global type w_sheet_spanish_books from w_sheet
end type
type st_1 from statictext within w_sheet_spanish_books
end type
type cb_update from commandbutton within w_sheet_spanish_books
end type
type cb_clear from commandbutton within w_sheet_spanish_books
end type
type cb_exit from commandbutton within w_sheet_spanish_books
end type
type dw_spanish_books from u_pics_dw within w_sheet_spanish_books
end type
end forward

shared variables

end variables

global type w_sheet_spanish_books from w_sheet
integer x = 9
integer y = 8
integer width = 3593
integer height = 1976
string title = "Spanish Language Books"
st_1 st_1
cb_update cb_update
cb_clear cb_clear
cb_exit cb_exit
dw_spanish_books dw_spanish_books
end type
global w_sheet_spanish_books w_sheet_spanish_books

type variables
BOOLEAN src_chng=FALSE,conno_exist=FALSE
String Local_chno
DataWindowChild ldwc_narr

end variables

on w_sheet_spanish_books.create
int iCurrent
call super::create
this.st_1=create st_1
this.cb_update=create cb_update
this.cb_clear=create cb_clear
this.cb_exit=create cb_exit
this.dw_spanish_books=create dw_spanish_books
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_1
this.Control[iCurrent+2]=this.cb_update
this.Control[iCurrent+3]=this.cb_clear
this.Control[iCurrent+4]=this.cb_exit
this.Control[iCurrent+5]=this.dw_spanish_books
end on

on w_sheet_spanish_books.destroy
call super::destroy
destroy(this.st_1)
destroy(this.cb_update)
destroy(this.cb_clear)
destroy(this.cb_exit)
destroy(this.dw_spanish_books)
end on

event pfc_preopen;call super::pfc_preopen;this.of_SetPreference(TRUE)
this.inv_preference.of_SetToolBars(TRUE)
this.inv_preference.of_SetWindow(TRUE)
this.of_SetResize(TRUE)
this.inv_resize.of_SetOrigSize(this.workspacewidth(),this.workspaceheight())
inv_resize.of_Register(dw_spanish_books, "Scale")
inv_resize.of_Register(cb_clear, "Scale")
inv_resize.of_Register(cb_exit, "Scale")
inv_resize.of_Register(cb_update, "Scale")
inv_resize.of_Register(st_1, "Scale")


end event

event resize;call super::resize;long ll_height

This.X = w_pics_main.X
This.Y = w_pics_main.Y
ll_height = w_pics_main.mdi_1.Height
This.Resize(w_pics_main.WorkSpaceWidth(), ll_height)

end event

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
		li_msg = MessageBox ( gnv_app.iapp_object.DisplayName, &
					"Do you want to save changes?", exclamation!, YesNoCancel!)
	End If
	Choose Case li_msg
		Case 1
			// YES - Update
			// If the update fails, prevent the window from closing
			rtn = cb_update.Event Clicked()
			if rtn = 1 THEN
				RETURN 0
			end if
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

event pfc_postopen;call super::pfc_postopen;// Open the sheet in Maximized mode
this.windowstate = maximized!

m_pics_main.m_file.m_print.Enabled 			=	TRUE
m_pics_main.m_file.m_pagesetup.Enabled		=	TRUE
m_pics_main.m_file.m_printimmediate.Enabled	=	TRUE

// Set the focus to the datawindows.
dw_spanish_books.SetFocus()

end event

type st_1 from statictext within w_sheet_spanish_books
integer x = 37
integer y = 1728
integer width = 1975
integer height = 64
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "Double click on Control No for Final Review screen."
boolean focusrectangle = false
end type

type cb_update from commandbutton within w_sheet_spanish_books
event clicked pbm_bnclicked
event getfocus pbm_bnsetfocus
event mousemove pbm_mousemove
string tag = "Update the record"
integer x = 2450
integer y = 1728
integer width = 329
integer height = 96
integer textsize = -12
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "&Update"
end type

event clicked;int rc,  i
long lbkseq
string  lbkmed,lrecagcy,lnarr,lnarrfn
dwItemStatus l_status

// Check for any pending updates
IF of_UpdateChecks( ) < 0 THEN Return -1

// If there were no changes to the screen, don't try to update the screen.
IF ( dw_spanish_books.ModifiedCount() > 0 ) THEN
	
	rc = dw_spanish_books.Event pfc_Update(TRUE,TRUE)
	IF rc  = 1 THEN
		FOR i = 1 to dw_spanish_books.RowCount()
			// Get the data
			lbkseq = dw_spanish_books.object.narr_bkseq[i]
			lbkmed = dw_spanish_books.object.narr_bkmed[i]
			lnarr= dw_spanish_books.object.narr[i]
			lnarrfn = dw_spanish_books.object.narrfn[i]
			lrecagcy = dw_spanish_books.object.recagcy[i]
			
			// IF insert
			IF dw_spanish_books.object.narrflag[i] = 'I' THEN
				INSERT INTO
				NARR(bkseq,bkmed,narr,narrfn,recagcy)
				VALUES (:lbkseq,:lbkmed,:lnarr,:lnarrfn,:lrecagcy)
				USING SQLServerTrans;
			ELSEIF dw_spanish_books.object.narrflag[i] = 'U' THEN
				UPDATE NARR
				SET narr = :lnarr, narrfn = :lnarrfn , recagcy = :lrecagcy
				WHERE bkseq = :lbkseq
				AND bkmed = :lbkmed
				USING SQLServerTrans;
			END IF
		NEXT
		COMMIT USING SQLServerTrans;
		MessageBox("Update","Records updated. ",Information!)
		cb_clear.PostEvent(Clicked!)
	ELSE
		ROLLBACK USING SQLServerTrans;
		MessageBox("Error","Update failed. ",StopSign!)
		RETURN -1
	END IF
END IF

end event

event getfocus;SetMicroHelp(w_pics_main,this.tag)
end event

event mousemove;SetMicroHelp(w_pics_main,this.tag)
end event

type cb_clear from commandbutton within w_sheet_spanish_books
event clicked pbm_bnclicked
event getfocus pbm_bnsetfocus
event mousemove pbm_mousemove
string tag = "Clear the screen"
integer x = 2853
integer y = 1728
integer width = 270
integer height = 96
integer textsize = -12
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "&Clear"
end type

event clicked;dw_spanish_books.Reset()
dw_spanish_books.Retrieve()
dw_spanish_books.SetFocus()
end event

event getfocus;SetMicroHelp(w_pics_main,this.tag)
end event

event mousemove;SetMicroHelp(w_pics_main,this.tag)
end event

type cb_exit from commandbutton within w_sheet_spanish_books
event clicked pbm_bnclicked
event getfocus pbm_bnsetfocus
event mousemove pbm_mousemove
string tag = "Exit the screen"
integer x = 3182
integer y = 1728
integer width = 270
integer height = 96
integer textsize = -12
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "E&xit"
end type

event clicked;parent.Event pfc_close()
m_pics_main.m_menu.PopMenu(300, 0)

end event

event getfocus;SetMicroHelp(w_pics_main,this.tag)
end event

event mousemove;SetMicroHelp(w_pics_main,this.tag)
end event

type dw_spanish_books from u_pics_dw within w_sheet_spanish_books
event ue_enterkey pbm_dwnprocessenter
event pfc_hinttext pbm_mousemove
integer x = 37
integer width = 3511
integer height = 1696
integer taborder = 10
string dataobject = "d_spanish_books"
boolean ib_isupdateable = false
boolean ib_rmbmenu = false
boolean ib_rmbfocuschange = false
string is_updatesallowed = ""
end type

event constructor;call super::constructor;string ls_key_columns[]
string ls_narr_key_columns[]
string ls_ttlinit_updatable_columns[]
string ls_annotation_updatable_columns[]
//string ls_narr_updatable_columns[]

this.of_SetFilter(TRUE)
this.of_SetFind(TRUE)
this.of_SetSort(TRUE)
this.inv_filter.of_SetStyle(1)
this.inv_filter.of_SetColumnNameSource(2)

dw_spanish_books.of_SetLinkage(TRUE)
dw_spanish_books.of_SetTransObject(SQLServerTrans)
dw_spanish_books.of_SetMultiTable(TRUE)
dw_spanish_books.of_SetDropDownSearch(TRUE)

dw_spanish_books.inv_dropdownsearch.of_AddColumn("pmsub1")
dw_spanish_books.inv_dropdownsearch.of_AddColumn("pmsub2")
dw_spanish_books.inv_dropdownsearch.of_AddColumn("pmsub3")
dw_spanish_books.inv_dropdownsearch.of_AddColumn("narr")

this.GetChild ("narr", ldwc_narr)

ls_key_columns[]={"chno"}
//ls_narr_key_columns[]={"narr_bkmed","narr_bkseq"}

ls_ttlinit_updatable_columns[]={"chno","pmsub1","pmsub2","pmsub3","ttlart","ttl","authfn","auth","ahonorific"}
ls_annotation_updatable_columns[]={"chno","anno_foreign"}
//ls_narr_updatable_columns[]={"narr_bkmed","narr_bkseq","narr","narrfn"}

//dw_spanish_books.inv_multitable.of_Register('NARR',ls_narr_key_columns,ls_narr_updatable_columns,TRUE,2)
dw_spanish_books.inv_multitable.of_Register('ANNOTATION',ls_key_columns,ls_annotation_updatable_columns,TRUE,2)
dw_spanish_books.inv_multitable.of_Register('TTLINIT',ls_key_columns,ls_ttlinit_updatable_columns,TRUE,2)


this.of_SetPrintPreview(TRUE)

end event

event ue_postconstructor;call super::ue_postconstructor;This.retrieve()
end event

event retrievestart;call super::retrievestart;OpenWithParm(w_pics_retrieve_msg_box,"Retrieving Spanish Books, Please wait...")

end event

event retrieveend;call super::retrieveend;close(w_pics_retrieve_msg_box)
end event

event sqlpreview;call super::sqlpreview;//messagebox("sql",sqlsyntax)
end event

event itemchanged;call super::itemchanged;long lbkseq
string lbkmed,lnarr
int lcnt=0

IF dwo.Name = "narr" THEN
	
	lbkseq = dw_spanish_books.Object.mchar_bkseq[row]
	lbkmed = dw_spanish_books.Object.mchar_bkmed[row]

	SELECT count(*)
		INTO :lcnt
	FROM NARR
	WHERE bkseq = :lbkseq
	AND bkmed = :lbkmed
	USING SqlServerTrans;
			
	IF lcnt = 0 THEN
		// New narrator
//		dw_spanish_books.SetItemStatus(row,  "narr", Primary!, NewModified!)
		dw_spanish_books.object.narrflag[row] = 'I'
//		MessageBox("narr",'status changed to insert')
	ELSE
		// Old narrator
//		dw_spanish_books.SetItemStatus(row,  "narr", Primary!, DataModified!)
		dw_spanish_books.object.narrflag[row] = 'U'
//		MessageBox("narr",'status changed to update')
	END IF
				
	IF data<>"" THEN
		data=TRIM(data)
		This.object.narrfn[row]=TRIM(ldwc_narr.GetItemString(ldwc_narr.GetRow(),"narrfn"))
		This.object.recagcy[row]=TRIM(ldwc_narr.GetItemString(ldwc_narr.GetRow(),"recagcy"))
		IF IsNull(This.object.narr_bkseq[row]) THEN
			This.object.narr_bkseq[row] = This.object.mchar_bkseq[row]
			This.object.narr_bkmed[row] = This.object.mchar_bkmed[row]
		END IF
	ELSE
		This.object.narrfn[row]=""	
		This.object.narr_bkseq[row]=""	
		This.object.narr_bkmed[row]=""	
		This.object.recagcy[row]=""	
	END IF
ELSEIF dwo.Name = "recagcy" THEN
	IF IsNull(dw_spanish_books.object.narr[row]) THEN
//		dw_spanish_books.Object.recagcy.ValidationMsg='Narrrator must be selected before recorded agency is selected.'
		MessageBox("Warning",'Narrrator must be selected before recording agency is selected.',Information!)
		RETURN 2
	END IF
END IF

end event

event doubleclicked;call super::doubleclicked;Long CurRow
String ls_conno

CurRow = dw_spanish_books.GetRow()

ls_conno = dw_spanish_books.object.conno[CurRow]
OpenWithParm(w_sheet_final_review, ls_conno)
end event


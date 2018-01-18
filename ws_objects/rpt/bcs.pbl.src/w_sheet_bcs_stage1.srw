$PBExportHeader$w_sheet_bcs_stage1.srw
forward
global type w_sheet_bcs_stage1 from w_sheet
end type
type cb_pub from commandbutton within w_sheet_bcs_stage1
end type
type cb_anno from commandbutton within w_sheet_bcs_stage1
end type
type cb_editinfo from commandbutton within w_sheet_bcs_stage1
end type
type cb_export from commandbutton within w_sheet_bcs_stage1
end type
type pb_find from commandbutton within w_sheet_bcs_stage1
end type
type pb_print from commandbutton within w_sheet_bcs_stage1
end type
type pb_exit from commandbutton within w_sheet_bcs_stage1
end type
type pb_update from commandbutton within w_sheet_bcs_stage1
end type
type pb_last from picturebutton within w_sheet_bcs_stage1
end type
type pb_next from picturebutton within w_sheet_bcs_stage1
end type
type pb_prior from picturebutton within w_sheet_bcs_stage1
end type
type pb_first from picturebutton within w_sheet_bcs_stage1
end type
type st_modified from statictext within w_sheet_bcs_stage1
end type
type st_2 from statictext within w_sheet_bcs_stage1
end type
type st_displayed from statictext within w_sheet_bcs_stage1
end type
type st_1 from statictext within w_sheet_bcs_stage1
end type
type cb_lock from commandbutton within w_sheet_bcs_stage1
end type
type cb_finalr from commandbutton within w_sheet_bcs_stage1
end type
type dw_bcs_stage1 from u_pics_dw within w_sheet_bcs_stage1
end type
end forward

global type w_sheet_bcs_stage1 from w_sheet
integer x = 37
integer y = 100
integer width = 2853
integer height = 1564
string title = "Cataloging Stage I"
cb_pub cb_pub
cb_anno cb_anno
cb_editinfo cb_editinfo
cb_export cb_export
pb_find pb_find
pb_print pb_print
pb_exit pb_exit
pb_update pb_update
pb_last pb_last
pb_next pb_next
pb_prior pb_prior
pb_first pb_first
st_modified st_modified
st_2 st_2
st_displayed st_displayed
st_1 st_1
cb_lock cb_lock
cb_finalr cb_finalr
dw_bcs_stage1 dw_bcs_stage1
end type
global w_sheet_bcs_stage1 w_sheet_bcs_stage1

type variables
string mod_string,pass_conno
end variables

forward prototypes
public subroutine wf_disable_buttons ()
public subroutine wf_enable_buttons ()
public subroutine wf_modify_title ()
public subroutine wf_scroll (string as_scroll)
public subroutine wf_set_counts ()
end prototypes

public subroutine wf_disable_buttons ();pb_exit.enabled = FALSE
pb_update.enabled = FALSE
pb_first.enabled = FALSE
pb_last.enabled = FALSE
pb_next.enabled = FALSE
pb_prior.enabled = FALSE
pb_print.enabled = FALSE
cb_pub.enabled = FALSE
cb_anno.enabled = FALSE
cb_editinfo.enabled = FALSE
cb_export.enabled = FALSE
cb_finalr.enabled = FALSE

end subroutine

public subroutine wf_enable_buttons ();pb_exit.enabled = TRUE
pb_update.enabled = TRUE
pb_find.enabled = TRUE

pb_first.enabled = TRUE
pb_last.enabled = TRUE
pb_next.enabled = TRUE
pb_prior.enabled = TRUE

pb_print.enabled = TRUE
cb_pub.enabled = TRUE
cb_anno.enabled = TRUE
cb_editinfo.enabled = TRUE
cb_export.enabled = TRUE
cb_finalr.enabled = TRUE

end subroutine

public subroutine wf_modify_title ();String Lttl
	
Lttl  = dw_bcs_stage1.GetItemString(dw_bcs_stage1.GetRow(),"cttl")
	
IF Lttl <> "" THEN
		
	String Lchno,subttl,sub1,temp="xyz"
	int lcount,i
		
	Lchno  = dw_bcs_stage1.GetItemString(dw_bcs_stage1.GetRow(),"cchno")
	
	// Get the subtitles and concatenate it into one string.
	DECLARE proc_ret_sttl PROCEDURE FOR
				pcsadmin.ret_sttl(:Lchno)
	USING sqlservertrans; 
			
	EXECUTE proc_ret_sttl;

	DO WHILE (temp <> sub1)
		temp = sub1
		FETCH proc_ret_sttl INTO :sub1;
		IF temp <> sub1 THEN
			sub1=RightTrim(sub1)
			subttl=subttl+" "+sub1
		END IF
	LOOP 
		
	CLOSE proc_ret_sttl;
		
	Lttl = RightTrim(Lttl) + " " + RightTrim(subttl)
		
	dw_bcs_stage1.SetItem(dw_bcs_stage1.GetRow(), "cttl", Lttl)
	dw_bcs_stage1.SetItemStatus(dw_bcs_stage1.GetRow(),"cttl",Primary!, DataModified!)
END IF
end subroutine

public subroutine wf_scroll (string as_scroll);long ll_to_row, ll_getrow, ll_rowcount

CHOOSE CASE as_scroll
	CASE "FIRST"
		ll_to_row = 0
	CASE "NEXT"
		ll_to_row = dw_bcs_stage1.getrow() + 1
	CASE "PRIOR"
		ll_to_row = dw_bcs_stage1.getrow() - 1
	CASE "LAST"
		ll_to_row = dw_bcs_stage1.rowcount()
END CHOOSE

dw_bcs_stage1.scrolltorow(ll_to_row)
dw_bcs_stage1.setrow(ll_to_row)
dw_bcs_stage1.setfocus()

ll_rowcount = dw_bcs_stage1.rowcount()
ll_getrow = dw_bcs_stage1.getrow()

pb_first.enabled = (ll_getrow > 1)
pb_next.enabled = (ll_getrow < ll_rowcount)
pb_prior.enabled = (ll_getrow > 1)
pb_last.enabled = (ll_getrow < ll_rowcount)

end subroutine

public subroutine wf_set_counts ();st_modified.text=String(dw_bcs_stage1.ModifiedCount())
st_displayed.text=String(dw_bcs_stage1.RowCount())
end subroutine

event key;call super::key;IF KeyDown(KeyEscape!) THEN 
  pb_exit.TriggerEvent(Clicked!)
END IF

end event

event mousemove;call super::mousemove;SetMicroHelp(w_pics_main,this.tag)
end event

on w_sheet_bcs_stage1.create
int iCurrent
call super::create
this.cb_pub=create cb_pub
this.cb_anno=create cb_anno
this.cb_editinfo=create cb_editinfo
this.cb_export=create cb_export
this.pb_find=create pb_find
this.pb_print=create pb_print
this.pb_exit=create pb_exit
this.pb_update=create pb_update
this.pb_last=create pb_last
this.pb_next=create pb_next
this.pb_prior=create pb_prior
this.pb_first=create pb_first
this.st_modified=create st_modified
this.st_2=create st_2
this.st_displayed=create st_displayed
this.st_1=create st_1
this.cb_lock=create cb_lock
this.cb_finalr=create cb_finalr
this.dw_bcs_stage1=create dw_bcs_stage1
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.cb_pub
this.Control[iCurrent+2]=this.cb_anno
this.Control[iCurrent+3]=this.cb_editinfo
this.Control[iCurrent+4]=this.cb_export
this.Control[iCurrent+5]=this.pb_find
this.Control[iCurrent+6]=this.pb_print
this.Control[iCurrent+7]=this.pb_exit
this.Control[iCurrent+8]=this.pb_update
this.Control[iCurrent+9]=this.pb_last
this.Control[iCurrent+10]=this.pb_next
this.Control[iCurrent+11]=this.pb_prior
this.Control[iCurrent+12]=this.pb_first
this.Control[iCurrent+13]=this.st_modified
this.Control[iCurrent+14]=this.st_2
this.Control[iCurrent+15]=this.st_displayed
this.Control[iCurrent+16]=this.st_1
this.Control[iCurrent+17]=this.cb_lock
this.Control[iCurrent+18]=this.cb_finalr
this.Control[iCurrent+19]=this.dw_bcs_stage1
end on

on w_sheet_bcs_stage1.destroy
call super::destroy
destroy(this.cb_pub)
destroy(this.cb_anno)
destroy(this.cb_editinfo)
destroy(this.cb_export)
destroy(this.pb_find)
destroy(this.pb_print)
destroy(this.pb_exit)
destroy(this.pb_update)
destroy(this.pb_last)
destroy(this.pb_next)
destroy(this.pb_prior)
destroy(this.pb_first)
destroy(this.st_modified)
destroy(this.st_2)
destroy(this.st_displayed)
destroy(this.st_1)
destroy(this.cb_lock)
destroy(this.cb_finalr)
destroy(this.dw_bcs_stage1)
end on

event resize;call super::resize;long ll_height

This.X = w_pics_main.X
This.Y = w_pics_main.Y
ll_height = w_pics_main.mdi_1.Height
This.Resize(w_pics_main.WorkSpaceWidth(), ll_height)


end event

event pfc_preopen;this.of_SetPreference(TRUE)
this.inv_preference.of_SetToolBars(TRUE)
this.inv_preference.of_SetWindow(TRUE)
this.of_SetResize(TRUE)
this.inv_resize.of_SetOrigSize(this.workspacewidth(),this.workspaceheight())
inv_resize.of_Register(dw_bcs_stage1, "Scale")

inv_resize.of_Register(pb_print, "Scale")
inv_resize.of_Register(pb_exit, "Scale")
inv_resize.of_Register(pb_find, "Scale")
inv_resize.of_Register(pb_update, "Scale")
inv_resize.of_Register(cb_anno, "Scale")
inv_resize.of_Register(cb_editinfo, "Scale")
inv_resize.of_Register(cb_export, "Scale")
inv_resize.of_Register(cb_pub, "Scale")
inv_resize.of_Register(cb_finalr, "Scale")

inv_resize.of_Register(cb_lock, "Scale")
inv_resize.of_Register(st_1, "Scale")
inv_resize.of_Register(st_2, "Scale")
inv_resize.of_Register(st_displayed, "Scale")
inv_resize.of_Register(st_modified, "Scale")
// Resize the picture buttons
inv_resize.of_Register(pb_first, "Scale")
inv_resize.of_Register(pb_last, "Scale")
inv_resize.of_Register(pb_next, "Scale")
inv_resize.of_Register(pb_prior, "Scale")

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
//		li_msg = MessageBox ( gnv_app.iapp_object.DisplayName, &
//					"Do you want to save changes?", exclamation!, YesNoCancel!)
		Return 0
	End If
	Choose Case li_msg
		Case 1
			// YES - Update
			// If the update fails, prevent the window from closing
			rtn = pb_update.Event Clicked()
			IF rtn = 1 THEN
				RETURN 0
			END IF
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

event pfc_print;call super::pfc_print;//pb_print.TriggerEvent(Clicked!)
dw_bcs_stage1.Print(true,false)
return 1
end event

event open;call super::open;// Open SQL Spy
//gnv_app.inv_debug.inv_SQLSpy.of_OpenSQLSpy(TRUE)

m_pics_main.m_file.m_print.Enabled 			=	TRUE
m_pics_main.m_file.m_pagesetup.Enabled		=	TRUE
m_pics_main.m_file.m_printimmediate.Enabled	=	TRUE

end event

type cb_pub from commandbutton within w_sheet_bcs_stage1
event clicked pbm_bnclicked
event mousemove pbm_mousemove
string tag = "Publisher Information"
integer x = 37
integer y = 1336
integer width = 293
integer height = 96
integer taborder = 140
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
boolean enabled = false
string text = "Pu&blisher..."
end type

event clicked;open(w_publisher)

end event

type cb_anno from commandbutton within w_sheet_bcs_stage1
event clicked pbm_bnclicked
event mousemove pbm_mousemove
string tag = "Annotation Information"
integer x = 366
integer y = 1336
integer width = 352
integer height = 96
integer taborder = 10
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
boolean enabled = false
string text = "&Annotation..."
end type

event clicked;open(w_anno_rpt)

end event

type cb_editinfo from commandbutton within w_sheet_bcs_stage1
event clicked pbm_bnclicked
event mousemove pbm_mousemove
string tag = "Edit Information"
integer x = 736
integer y = 1336
integer width = 480
integer height = 96
integer taborder = 110
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
boolean enabled = false
string text = "Edi&t Information..."
end type

event clicked;int rownum
dw_bcs_stage1.AcceptText()
rownum = dw_bcs_stage1.getrow()

IF dw_bcs_stage1.object.cs1init[rownum]<>"" THEN
	OpenSheet(w_sheet_edit_info, w_pics_main, 0, Original!)
ELSE
	MessageBox("ERROR","Cataloger Initials must be entered!",Information!)
	dw_bcs_stage1.SetColumn(17)
END IF



end event

type cb_export from commandbutton within w_sheet_bcs_stage1
event clicked pbm_bnclicked
event mousemove pbm_mousemove
string tag = "Export"
boolean visible = false
integer x = 1243
integer y = 1336
integer width = 233
integer height = 96
integer taborder = 100
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
boolean enabled = false
string text = "Exp&ort..."
end type

event clicked;dw_bcs_stage1.SaveAs()

end event

type pb_find from commandbutton within w_sheet_bcs_stage1
event clicked pbm_bnclicked
event mousemove pbm_mousemove
string tag = "Find"
integer x = 1787
integer y = 1336
integer width = 238
integer height = 96
integer taborder = 90
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
boolean enabled = false
string text = "F&ind"
end type

event clicked;string rtn,rc,Lconno,Linit
long ll_rows
integer res


// IF find is dispalyed on the push button
IF pb_find.BringToTop = FALSE THEN
	// disable the buttons 
   w_sheet_bcs_stage1.wf_disable_buttons()
	// Turn on query mode so user can specify data
	rtn = w_sheet_bcs_stage1.dw_bcs_stage1.Modify("DataWindow.QueryMode=YES")
	IF rtn = "" THEN
		// If Modify succeeds, show Execute,
		// Query mode is on and display sort CheckBox
		This.BringToTop = TRUE
		This.Text = "Ex&ecute"
		dw_bcs_stage1.SetFocus()
	   SetMicroHelp(w_pics_main,"Query Mode...")
	ELSE
		MessageBox("Error", "Can't access query mode to select data.")
	END IF
ELSE
	dw_bcs_stage1.AcceptText()
	// Turn off Query mode and retrieve data 
	// based on user's choices
	rtn = w_sheet_bcs_stage1.dw_bcs_stage1.Modify("DataWindow.QueryMode=NO")
	IF rtn = "" THEN
		// If Modify succeeds, show Find,
		// Query mode is off, and retrieve data
		This.BringToTop = FALSE
		This.Text = "F&ind"
		ll_rows=w_sheet_bcs_stage1.dw_bcs_stage1.Retrieve()
		IF ll_rows > 0 THEN 
			// If any rows were retrieved.
      	SetMicroHelp(w_pics_main,"")
      	w_sheet_bcs_stage1.wf_set_counts()
			wf_modify_title()
			Lconno  = dw_bcs_stage1.GetItemString(dw_bcs_stage1.GetRow(),"cconno")
			Linit   = dw_bcs_stage1.GetItemString(dw_bcs_stage1.GetRow(),"cs1init")
			IF f_display_rec_locked(Linit,Lconno)=TRUE THEN
				cb_lock.visible = TRUE
			ELSE
				cb_lock.visible = FALSE
			END IF
	   	w_sheet_bcs_stage1.wf_enable_buttons()
		ELSE
			// If no rows were retrieved, ask if they want to continue with retrieval.
      	SetMicroHelp(w_pics_main,"")
      	w_sheet_bcs_stage1.wf_set_counts()
			res = MessageBox("Retrieve Error","No records were retrieved. Continue with query mode?", Question!, OkCancel!, 2 )
			IF res = 1 THEN
				// If yes continue the reterival process
				pb_find.TriggerEvent(Clicked!)
			ELSE
				// Restore the original select statement and modify the datawindow
				// mod_string is a shared variable, and is set in ue_postconstructor event of dw_bcs_stage1.
				res = dw_bcs_stage1.Reset()
				IF res = 1 THEN
				// Restore the original data from dataobject
				   dw_bcs_stage1.DataObject = dw_bcs_stage1.DataObject
					dw_bcs_stage1.SetTransObject( SQLServerTrans )
					rc = dw_bcs_stage1.Modify(mod_string)
				// if the modify select statement fails display error
				// messages, else retrieve the original data.
					IF rc = "" THEN
						ll_rows = w_sheet_bcs_stage1.dw_bcs_stage1.Retrieve()
						IF ll_rows > 0 THEN
      					SetMicroHelp(w_pics_main,"")
							w_sheet_bcs_stage1.wf_set_counts()
      					wf_modify_title()	
							Lconno  = dw_bcs_stage1.GetItemString(dw_bcs_stage1.GetRow(),"cconno")
							Linit   = dw_bcs_stage1.GetItemString(dw_bcs_stage1.GetRow(),"cs1init")
							IF f_display_rec_locked(Linit,Lconno)=TRUE THEN
								cb_lock.visible = TRUE
							ELSE
								cb_lock.visible = FALSE
							END IF
							w_sheet_bcs_stage1.wf_enable_buttons()
						ELSE
							MessageBox("Error","No records were retrieved.")
						END IF // ll_rows > 0
					ELSE
						MessageBox("Error","Error in restoring the original select statement. ReturnCode=" + rc)
					END IF // rc = ""
				END IF // res = 1
			END IF // res = 1 
		END IF // ll_rows > 0
	ELSE
		MessageBox("Error","Failure exiting query mode.")
   	w_sheet_bcs_stage1.wf_enable_buttons()
	END IF // rtn = ""
END IF // if pb_find...
end event

type pb_print from commandbutton within w_sheet_bcs_stage1
event clicked pbm_bnclicked
event mousemove pbm_mousemove
string tag = "Print"
integer x = 2085
integer y = 1336
integer width = 178
integer height = 96
integer taborder = 120
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
boolean enabled = false
string text = "&Print"
end type

event clicked;//long print_job,print_ret,row_num
//string Ls1, Ls2in,Ls2out,Lactenddt
//string ls_font_facename,Lconno
//int li_font_height, Ans,rtn,RowNum
//
//RowNum = w_sheet_bcs_stage1.dw_bcs_stage1.GetRow()
//Lconno = w_sheet_bcs_stage1.dw_bcs_stage1.Object.cconno[RowNum]
//
//print_job = PrintOpen( ) 
//if print_job < 0 then
//  MessageBox("Error in PrintOpen", Print_job,StopSign!)
//  return
//end if
//
//row_num = dw_bcs_stage1.GetRow()
//
//SetMicroHelp(w_pics_main,"Please wait...")
//  
//ls_font_facename = "Arial"
//li_font_height= 0
//  
//PrintDefineFont(Print_Job, 1, "Courier 10Cpi", &
//		-18, 400, Default!, Decorative!, FALSE, FALSE)
//
//print_ret = PrintSetFont(print_job,1)
//
//print_ret = print(print_job, "BCS Individual Control Report", 4500)
//print_ret = print(print_job, " ")
//
//print_ret = print(print_job, String(Today(), "mmmm d, yyyy"))
//
//print_ret = print(print_job, " ")
//print_ret = print(print_job, " ")
//
//print_ret = print(print_job, 500, "Control Number: " + dw_bcs_stage1.GetItemString(row_num,"cconno"))
//print_ret = print(print_job, " ")
//
//print_ret = PrintSetFont(print_job,0)
//
//Ls1=string(dw_bcs_stage1.object.cs1[row_num],'MM/DD/YYYY')
//Ls2in=string(dw_bcs_stage1.object.cs2in[row_num],'MM/DD/YYYY')
//Ls2out=string(dw_bcs_stage1.object.cs2out[row_num],'MM/DD/YYYY')
//Lactenddt=string(dw_bcs_stage1.object.actenddt[row_num],'MM/DD/YYYY')
//
//
//print_ret = print(print_job, 500, "Stage I Date: " + Ls1)
//print_ret = print(print_job, " ")
//
//print_ret = print(print_job, 500, "LC Card Number: " + dw_bcs_stage1.GetItemString(row_num,"clcno"))
//print_ret = print(print_job, " ")
//
//print_ret = print(print_job, 500, "Medium: " + dw_bcs_stage1.GetItemString(row_num,"cmed"))
//print_ret = print(print_job, " ")
//
//print_ret = print(print_job, 500, "Dewey: " + dw_bcs_stage1.GetItemString(row_num,"cdewey"))
//print_ret = print(print_job, " ")
//
//  print_ret = print(print_job, 500, "Other Medium/BK: " + dw_bcs_stage1.GetItemString(row_num,"cothermed"))
//  print_ret = print(print_job, " ")
//
//  print_ret = print(print_job, 500, "AJYFN: " + dw_bcs_stage1.GetItemString(row_num,"cajyfn"))
//  print_ret = print(print_job, " ")
//
//  print_ret = print(print_job, 500, "Reissue of: " + string(dw_bcs_stage1.object.ri_prevbkseq[row_num]))
//  print_ret = print(print_job, " ")
//
//  print_ret = print(print_job, 500, "author: " + dw_bcs_stage1.GetItemString(row_num,"cauthfn")+" "+dw_bcs_stage1.GetItemString(row_num,"cauth"))
//  print_ret = print(print_job, " ")
//
//  print_ret = print(print_job, 500, "Title: " + dw_bcs_stage1.GetItemString(row_num,"cttl"))
//  print_ret = print(print_job, " ")
//
//  print_ret = print(print_job, 500, "Series: " + dw_bcs_stage1.GetItemString(row_num,"cserttl"))
//  print_ret = print(print_job, " ")
//
//  print_ret = print(print_job, 500, "Sequel: " + dw_bcs_stage1.GetItemString(row_num,"cseqnote"))
//  print_ret = print(print_job, " ")
//
//  print_ret = print(print_job, 500, "Catalog Initial: " + dw_bcs_stage1.GetItemString(row_num,"cs1init"))
//  print_ret = print(print_job, " ")
//
//  print_ret = print(print_job, 500, "Book Number: " + string(dw_bcs_stage1.object.mchar_bkseq[row_num]) + " " + dw_bcs_stage1.GetItemString(row_num,"mchar_bkmed"))
//  print_ret = print(print_job, " ")
//
//  string Lrecagncy="",Lprdr=""
//  Lrecagncy = dw_bcs_stage1.GetItemString(row_num,"crecagcy")
//  Lprdr = dw_bcs_stage1.GetItemString(row_num,"mchar_prdr")
//  
//  print_ret = print(print_job, 500, "Record Agency: " + Lrecagncy  + "    Producer: " + Lprdr )
//  print_ret = print(print_job, " ")
//
//  print_ret = print(print_job, 500, "Narrator: " + dw_bcs_stage1.GetItemString(row_num,"cnarr") + "  Narration Date: " + string(Lactenddt) + "  Number of Volumes: " + string(dw_bcs_stage1.GetItemNumber(row_num,"cvols")))
//  print_ret = print(print_job, " ")
//  print_ret = print(print_job, " ")
//
//  print_ret = print(print_job, 500, "Stage II Completion Date in: " + Ls2in)
//  print_ret = print(print_job, " ")
//  print_ret = print(print_job, " ")
//
//  print_ret = print(print_job, 500, "Stage II Completion Date out: " + Ls2out)
//
//
//  SetMicroHelp(w_pics_main,"")
//  
//  print_ret = PrintClose(print_job)
//  if print_job < 0 then
//    MessageBox("Error in PrintClose", Print_job,StopSign!)
//    return
//  end if
//
//
pass_conno = dw_bcs_stage1.object.cconno[dw_bcs_stage1.getrow()]
OpenSheetwithparm(w_sheet_pics_ole_crystal,"bcsIndividualControlReport",w_pics_main, 0, Original!)


end event

type pb_exit from commandbutton within w_sheet_bcs_stage1
event clicked pbm_bnclicked
event mousemove pbm_mousemove
string tag = "Exit"
integer x = 2597
integer y = 1336
integer width = 178
integer height = 96
integer taborder = 130
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "E&xit"
end type

event clicked;//dw_bcs_stage1.DBCancel()
close(parent)
m_pics_main.m_menu.PopMenu(300, 0)


end event

type pb_update from commandbutton within w_sheet_bcs_stage1
event clicked pbm_bnclicked
event mousemove pbm_mousemove
string tag = "Update"
integer x = 2327
integer y = 1336
integer width = 219
integer height = 96
integer taborder = 80
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
boolean enabled = false
string text = "&Update"
end type

event clicked;integer  rtn,ll_rows
string Ls1init,Ldewey,Lconno,Lchno,Lothermed,Llcno,Ls1,Lajyfn,Llockflg
boolean val_init,val_dewey,val_user

// Accept the input from the screen
dw_bcs_stage1.AcceptText()
// If there are any modification continue, else exit.
IF dw_bcs_stage1.ModifiedCount() > 0 THEN
	
	// Set the transaction object
	dw_bcs_stage1.SetTransObject(sqlservertrans)

	// Get the row number 
	ll_rows = dw_bcs_stage1.GetRow()
	// get the values from the screen and save them in the local
	// variables.
	Ls1 = string(dw_bcs_stage1.object.cs1[ll_rows],'MM/DD/YYYY')
	Ls1init = dw_bcs_stage1.object.cs1init[ll_rows]
	LDewey = dw_bcs_stage1.object.cdewey[ll_rows]
	Lajyfn = dw_bcs_stage1.object.cajyfn[ll_rows]
	Llcno = dw_bcs_stage1.object.clcno[ll_rows]
	Lconno = dw_bcs_stage1.object.cconno[ll_rows]
	Lchno = dw_bcs_stage1.object.cchno[ll_rows]
	Lothermed = dw_bcs_stage1.object.cothermed[ll_rows]

	// If initial or dewey or stage I date are null display a error message.
	IF (IsNull(Ls1init) or IsNull(Ldewey) or IsNull(Ls1)) THEN
   	MessageBox("ERROR","You must enter values for ~'Dewey Number~' and ~'Catalog Initials~' and ~'Stage I Date~', before updating database.",StopSign!)
   	dw_bcs_stage1.SetFocus()
		RETURN 0
	ELSE 
		SetPointer(HourGlass!)
		// disable the push buttons
  		wf_disable_buttons()
		pb_find.enabled = FALSE
		
		//messagebox("userids","logon id= "+sqlservertrans.userid+" initial = "+Ls1init)
		// Validate the initials
		IF sqlservertrans.userid <> Ls1init THEN
			val_init=FALSE
		ELSE
			val_init = f_valid_initial(Ls1init)
		END IF
		// Validate dewey
		val_dewey = f_valid_dewey(LDewey,Lajyfn)
		// Validate the user, If this is the original user, who updated this record.
		val_user = f_valid_user_to_update(Ls1init,Lconno)
		IF val_dewey = FALSE THEN
			rtn = MessageBox("Warning","Invalid dewey number. Please check dewey against ajyfn. ~nContinue Updating...?",Information!, OKCancel!, 1)
			IF rtn = 2 THEN 
				wf_enable_buttons()
				pb_find.enabled = TRUE
				RETURN 1
			ELSE
				dw_bcs_stage1.SetColumn(7)
			END IF
		END IF
		IF (val_init = TRUE) and (val_user = TRUE) THEN
			SetMicroHelp(w_pics_main,"Updating database, Please wait...")
			// Set the lock flag to yes.
			Llockflg = 'Y'
			// update the fields using store procedure.
//			DECLARE proc_s1_update PROCEDURE FOR
//				pcsadmin.s1_update :Lconno,:Ls1init,:Ls1,:Lothermed,:Lchno,:Llcno,:Ldewey,:Llockflg
//			USING sqlservertrans; 
//					
//			EXECUTE proc_s1_update;
			
			UPDATE catalog
			SET s1init = :Ls1init, s1 = TO_DATE(:Ls1,'MM/DD/YYYY'), othermed = :Lothermed, lockflag = :Llockflg
			WHERE conno = :Lconno
			USING SQLServerTrans;
			IF f_check_dberror(SQlServerTrans,"CATALOG Table") THEN
				UPDATE ttlinit
				SET lcno = :Llcno, dewey = :Ldewey
				WHERE chno = :lchno
				USING SQLServerTrans;
				IF f_check_dberror(SQlServerTrans,"TTLINIT Table") THEN
					// Mark MCHAR Table
					f_update_mchar_time(Lconno,0,"C","U")
					//dw_bcs_stage1.SetItemStatus(1, 0, Primary!, NotModified!)			
					SetMicroHelp(w_pics_main,"Record was updated successfully.")
					COMMIT USING SQLServerTrans;
				ELSE
					SetMicroHelp(w_pics_main,"Update Record failed.")
					ROLLBACK USING sqlservertrans;
					RETURN 0
					dw_bcs_stage1.SetFocus()
					wf_enable_buttons()
				END IF
			ELSE
				SetMicroHelp(w_pics_main,"Update Record failed.")
				ROLLBACK USING sqlservertrans;
				RETURN 0
				dw_bcs_stage1.SetFocus()
				wf_enable_buttons()
			END IF

//			IF sqlservertrans.SQLCode < 0 THEN
//				String ls_message,ls_msgparm[1]
//				ls_message = "A database error has occurred.~n" + &
//								 "Database error code:  " + String (sqlservertrans.sqldbcode) + "~r~n" + &
//								 "Database error message:~r~n" + sqlservertrans.sqlerrtext
//				If IsValid(gnv_app.inv_error) Then
//					ls_msgparm[1] = ls_message
//					gnv_app.inv_error.of_Message ('pfc_dwdberror', ls_msgparm, &
//							gnv_app.iapp_object.DisplayName)
//				Else
//					Messagebox (gnv_app.iapp_object.DisplayName, ls_message, StopSign!, Ok!)
//					ROLLBACK USING sqlservertrans;
//				End If
//				SetMicroHelp(w_pics_main,"Update Record failed.")
//				dw_bcs_stage1.SetFocus()
//				wf_enable_buttons()
//			ELSE
//				// Mark MCHAR Table
//				f_update_mchar_time(Lconno,0,"C","U")
//				//dw_bcs_stage1.SetItemStatus(1, 0, Primary!, NotModified!)			
//				SetMicroHelp(w_pics_main,"Record was updated successfully.")
//				COMMIT USING sqlservertrans;
//			END IF
			//Display the unlock the record push buttom
			cb_lock.visible = TRUE
			//CLose the store procedure
//			CLOSE proc_s1_update;
		ELSEIF val_user = FALSE THEN		  
			Messagebox ("ERROR","Record is locked by another user.", StopSign!)
			dw_bcs_stage1.SetFocus()
			wf_enable_buttons()
			RETURN 0
		ELSEIF val_init = FALSE THEN		  
			MessageBox("ERROR","Invalid cataloger initials.",StopSign!)
			dw_bcs_stage1.SetFocus()
			wf_enable_buttons()
			RETURN 0
		END IF
		wf_enable_buttons()
		wf_set_counts()
		pb_find.enabled = TRUE
		RETURN 1
	END IF // Is NULL
	SetPointer(Arrow!)
ELSE 
	MessageBox("Update","No changes to the database.",Information!)
	RETURN 0
END IF // Modified count	
end event

type pb_last from picturebutton within w_sheet_bcs_stage1
event clicked pbm_bnclicked
event mousemove pbm_mousemove
string tag = "Last Record"
integer x = 1728
integer y = 1084
integer width = 101
integer height = 84
integer taborder = 70
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string picturename = "LAST1.BMP"
vtextalign vtextalign = vcenter!
end type

event clicked;wf_scroll("LAST")
dwItemStatus l_status

// If there are any changes to this screen accept them and then update it.
dw_bcs_stage1.AcceptText()

l_status = dw_bcs_stage1.GetItemStatus(dw_bcs_stage1.GetRow(),"cttl", Primary!)
IF l_status = NotModified! THEN
	wf_modify_title()
END IF
string Lconno,Linit
Lconno  = dw_bcs_stage1.GetItemString(dw_bcs_stage1.GetRow(),"cconno")
Linit   = dw_bcs_stage1.GetItemString(dw_bcs_stage1.GetRow(),"cs1init")
IF f_display_rec_locked(Linit,Lconno)=TRUE THEN
	cb_lock.visible = TRUE
ELSE
	cb_lock.visible = FALSE
END IF

end event

event mousemove;SetMicroHelp(w_pics_main,this.tag)
end event

type pb_next from picturebutton within w_sheet_bcs_stage1
event clicked pbm_bnclicked
event mousemove pbm_mousemove
string tag = "Next Record"
integer x = 1605
integer y = 1084
integer width = 101
integer height = 84
integer taborder = 30
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string picturename = "NEXT1.BMP"
vtextalign vtextalign = vcenter!
end type

event clicked;wf_scroll("NEXT")
dwItemStatus l_status

l_status = dw_bcs_stage1.GetItemStatus(dw_bcs_stage1.GetRow(),"cttl", Primary!)
IF l_status = NotModified! THEN
	wf_modify_title()
END IF
string Lconno,Linit
Lconno  = dw_bcs_stage1.GetItemString(dw_bcs_stage1.GetRow(),"cconno")
Linit   = dw_bcs_stage1.GetItemString(dw_bcs_stage1.GetRow(),"cs1init")
IF f_display_rec_locked(Linit,Lconno)=TRUE THEN
	cb_lock.visible = TRUE
ELSE
	cb_lock.visible = FALSE
END IF

end event

event mousemove;SetMicroHelp(w_pics_main,this.tag)
end event

type pb_prior from picturebutton within w_sheet_bcs_stage1
event clicked pbm_bnclicked
event mousemove pbm_mousemove
string tag = "Previous Record"
integer x = 1481
integer y = 1084
integer width = 101
integer height = 84
integer taborder = 40
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string picturename = "PRIOR1.BMP"
vtextalign vtextalign = vcenter!
end type

event clicked;wf_scroll("PRIOR")
dwItemStatus l_status

l_status = dw_bcs_stage1.GetItemStatus(dw_bcs_stage1.GetRow(),"cttl", Primary!)
IF l_status = NotModified! THEN
	wf_modify_title()
END IF
string Lconno,Linit
Lconno  = dw_bcs_stage1.GetItemString(dw_bcs_stage1.GetRow(),"cconno")
Linit   = dw_bcs_stage1.GetItemString(dw_bcs_stage1.GetRow(),"cs1init")
IF f_display_rec_locked(Linit,Lconno)=TRUE THEN
	cb_lock.visible = TRUE
ELSE
	cb_lock.visible = FALSE
END IF

end event

event mousemove;SetMicroHelp(w_pics_main,this.tag)
end event

type pb_first from picturebutton within w_sheet_bcs_stage1
event clicked pbm_bnclicked
event mousemove pbm_mousemove
string tag = "First Record"
integer x = 1358
integer y = 1084
integer width = 101
integer height = 84
integer taborder = 60
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string picturename = "FIRST1.BMP"
vtextalign vtextalign = vcenter!
end type

event clicked;wf_scroll("FIRST")
string Lconno,Linit
Lconno  = dw_bcs_stage1.GetItemString(dw_bcs_stage1.GetRow(),"cconno")
Linit   = dw_bcs_stage1.GetItemString(dw_bcs_stage1.GetRow(),"cs1init")
IF f_display_rec_locked(Linit,Lconno)=TRUE THEN
	cb_lock.visible = TRUE
ELSE
	cb_lock.visible = FALSE
END IF

end event

event mousemove;SetMicroHelp(w_pics_main,this.tag)
end event

type st_modified from statictext within w_sheet_bcs_stage1
integer x = 960
integer y = 1088
integer width = 146
integer height = 84
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long backcolor = 16777215
boolean enabled = false
boolean border = true
borderstyle borderstyle = stylelowered!
boolean focusrectangle = false
end type

type st_2 from statictext within w_sheet_bcs_stage1
integer x = 585
integer y = 1088
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
boolean focusrectangle = false
end type

type st_displayed from statictext within w_sheet_bcs_stage1
integer x = 407
integer y = 1088
integer width = 133
integer height = 88
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long backcolor = 16777215
boolean enabled = false
alignment alignment = center!
boolean border = true
borderstyle borderstyle = stylelowered!
boolean focusrectangle = false
end type

type st_1 from statictext within w_sheet_bcs_stage1
integer x = 37
integer y = 1088
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
alignment alignment = center!
boolean focusrectangle = false
end type

type cb_lock from commandbutton within w_sheet_bcs_stage1
event clicked pbm_bnclicked
boolean visible = false
integer x = 1957
integer y = 1084
integer width = 571
integer height = 84
integer taborder = 50
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "Unlock the Record"
end type

event clicked;integer  rtn,ll_rows
string Ls1init,Ldewey,Lconno,Lchno,Lothermed,Llcno,Ls1,Lajyfn,Llockflg
Boolean val_user

rtn = MessageBox("Unlock the Record","This process will unlock the record from catalog table.",Question!,YesNo!,2)
if rtn = 1 THEN
	// Accept the input from the screen
	dw_bcs_stage1.AcceptText()
	
	// Set the transaction object
	dw_bcs_stage1.SetTransObject(sqlservertrans)

	// Get the row number 
	ll_rows = dw_bcs_stage1.GetRow()
	
	// get the values from the screen and save them in the local
	// variables.

	Ls1 = string(dw_bcs_stage1.object.cs1[ll_rows],'MM/DD/YYYY')
	Ls1init = dw_bcs_stage1.object.cs1init[ll_rows]
	LDewey = dw_bcs_stage1.object.cdewey[ll_rows]
	Lajyfn = dw_bcs_stage1.object.cajyfn[ll_rows]
	Llcno = dw_bcs_stage1.object.clcno[ll_rows]
	Lconno = dw_bcs_stage1.object.cconno[ll_rows]
	Lchno = dw_bcs_stage1.object.cchno[ll_rows]
	Lothermed = dw_bcs_stage1.object.cothermed[ll_rows]

	SetNull(Llockflg)
	
	val_user = f_valid_user_to_update(sqlservertrans.userid,Lconno)
	IF val_user = TRUE THEN
		// update 
		UPDATE catalog
		SET s1init = :Ls1init, s1 = TO_DATE(:Ls1,'MM/DD/YYYY'), othermed = :Lothermed, lockflag = :Llockflg
		WHERE conno = :Lconno
		USING SQLServerTrans;
		IF f_check_dberror(SQlServerTrans,"CATALOG Table") THEN
			UPDATE ttlinit
			SET lcno = :Llcno, dewey = :Ldewey
			WHERE chno = :lchno
			USING SQLServerTrans;
			IF f_check_dberror(SQlServerTrans,"TTLINIT Table") THEN
				SetMicroHelp(w_pics_main,"Record was uplocked successfully.")
				COMMIT USING SQLServerTrans;
			ELSE
				SetMicroHelp(w_pics_main,"Update Record failed.")
				ROLLBACK USING sqlservertrans;
				RETURN 0
			END IF
		ELSE
			SetMicroHelp(w_pics_main,"Update Record failed.")
			ROLLBACK USING sqlservertrans;
			RETURN 0
		END IF
	ELSE
		Messagebox ("ERROR","Record is locked by another user.", StopSign!)
	END IF
END IF
end event

type cb_finalr from commandbutton within w_sheet_bcs_stage1
event mousemove pbm_mousemove
boolean visible = false
integer x = 37
integer y = 1216
integer width = 544
integer height = 96
integer taborder = 80
integer textsize = -8
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
boolean enabled = false
string text = "Select Final Re&view..."
end type

event clicked;close(parent)
m_pics_main.m_menu.m_catalogingactivities.m_stagei.TriggerEvent(Clicked!)

end event

type dw_bcs_stage1 from u_pics_dw within w_sheet_bcs_stage1
event itemfocuschanged pbm_dwnitemchangefocus
event rbuttondown pbm_dwnrbuttondown
event rbuttonup pbm_dwnrbuttonup
event ue_postconstructor ( )
event ue_enterkey pbm_dwnprocessenter
integer x = 27
integer y = 28
integer width = 2752
integer height = 1032
integer taborder = 20
string dataobject = "d_bcs_stage1"
boolean vscrollbar = false
boolean livescroll = false
boolean ib_isupdateable = false
boolean ib_rmbmenu = false
boolean ib_rmbfocuschange = false
string is_updatesallowed = ""
end type

event rbuttondown;//
end event

event rbuttonup;//
end event

event ue_postconstructor;string original_select,where_clause,rc,Lconno,Linit,order_clause,Lfrdate
int ll_rows,RowNum
date Ldate
n_cst_string		lnv_string

IF IsValid(w_final_review) THEN
	Lfrdate = string(w_final_review.Frdate,'MM/DD/YYYY')
ELSEIF IsValid(w_frdate) THEN
	Lfrdate = w_frdate.Frdate
END IF

SetMicroHelp(parent,"Please wait...")

this.SetTransObject( SQLServerTrans )

original_select =	this.Describe("DataWindow.Table.Select")

Ldate = date(Lfrdate)
where_clause = " AND mchar.fr =" + "~'" + string(Ldate,'DD-MMM-YYYY') + "~'" 

order_clause = " ORDER BY ttlinit.sttl ASC"

original_select = lnv_string.of_GlobalReplace(original_select, "~'MA~'", "~~~'MA~~~'")
original_select = lnv_string.of_GlobalReplace(original_select, "~'PU~'", "~~~'PU~~~'")
original_select = lnv_string.of_GlobalReplace(original_select, "~'AB~'", "~~~'AB~~~'")
original_select = lnv_string.of_GlobalReplace(original_select, "~'T~'", "~~~'T~~~'")
original_select = lnv_string.of_GlobalReplace(original_select, "~'M~'", "~~~'M~~~'")
//messagebox("original_select",original_select)

mod_string = "DataWindow.Table.Select=~"" + original_select + where_clause + order_clause + "~""
//messagebox("where_clause",where_clause)

rc = this.Modify(mod_string)
IF rc = "" THEN
	ll_rows = this.Retrieve()
   IF ll_rows < 1 THEN 
     	MessageBox("ERROR", "There are no records ready for Stage 1 with Final Review date of: " + Lfrdate +" .",StopSign!, OK!, 2)
		SetMicroHelp(parent,"")
		
	  	IF IsValid(w_frdate) THEN
 			close(w_frdate)
		END IF
     	close(parent)
	ELSE
		dw_bcs_stage1.SetSort("cttl")
		dw_bcs_stage1.Sort()
     	wf_set_counts()
		wf_modify_title()
		Lconno  = this.GetItemString(this.GetRow(),"cconno")
		Linit   = this.GetItemString(this.GetRow(),"cs1init")
		IF f_display_rec_locked(Linit,Lconno)=TRUE THEN
			cb_lock.visible = TRUE
		ELSE
			cb_lock.visible = FALSE
		END IF

     	this.SetFocus()
		SetMicroHelp(parent,"")
		
	  	IF IsValid(w_frdate) THEN
 		 	close(w_frdate)
		ELSEIF IsValid(w_final_review) then
 		 	close(w_final_review)
		END IF
		parent.wf_enable_buttons()
	  	pb_find.enabled = TRUE	
		
	END IF
ELSE
	MessageBox("Status", "Modify select statement Failed." + rc)
END IF


end event

event ue_enterkey;call super::ue_enterkey;Send(Handle(this),256,9,Long(0,0))
return(1)
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

event retrievestart;call super::retrievestart;OpenWithParm(w_pics_retrieve_msg_box,"Retrieving BCS Stage I data, Please Wait...")
end event

event retrieveend;call super::retrieveend;close(w_pics_retrieve_msg_box)
end event

event pfc_deleterow;//
RETURN 1
end event

event pfc_addrow;//
RETURN 1
end event


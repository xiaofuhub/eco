$PBExportHeader$w_add_distribution_code.srw
forward
global type w_add_distribution_code from w_sheet
end type
type cb_find from u_cb within w_add_distribution_code
end type
type cb_clear from u_cb within w_add_distribution_code
end type
type cb_exit from u_cb within w_add_distribution_code
end type
type cb_update from u_cb within w_add_distribution_code
end type
type cb_delete from u_cb within w_add_distribution_code
end type
type st_1 from u_st within w_add_distribution_code
end type
type st_2 from u_st within w_add_distribution_code
end type
type sle_rowcount from u_sle within w_add_distribution_code
end type
type dw_libcodes from u_dw within w_add_distribution_code
end type
type dw_add_delete_distribution_code from u_dw within w_add_distribution_code
end type
end forward

global type w_add_distribution_code from w_sheet
integer x = 87
integer y = 128
integer width = 2734
integer height = 1704
string title = "Add and Delete Distribution Code"
cb_find cb_find
cb_clear cb_clear
cb_exit cb_exit
cb_update cb_update
cb_delete cb_delete
st_1 st_1
st_2 st_2
sle_rowcount sle_rowcount
dw_libcodes dw_libcodes
dw_add_delete_distribution_code dw_add_delete_distribution_code
end type
global w_add_distribution_code w_add_distribution_code

type variables
string is_libcd

end variables

on w_add_distribution_code.create
int iCurrent
call super::create
this.cb_find=create cb_find
this.cb_clear=create cb_clear
this.cb_exit=create cb_exit
this.cb_update=create cb_update
this.cb_delete=create cb_delete
this.st_1=create st_1
this.st_2=create st_2
this.sle_rowcount=create sle_rowcount
this.dw_libcodes=create dw_libcodes
this.dw_add_delete_distribution_code=create dw_add_delete_distribution_code
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.cb_find
this.Control[iCurrent+2]=this.cb_clear
this.Control[iCurrent+3]=this.cb_exit
this.Control[iCurrent+4]=this.cb_update
this.Control[iCurrent+5]=this.cb_delete
this.Control[iCurrent+6]=this.st_1
this.Control[iCurrent+7]=this.st_2
this.Control[iCurrent+8]=this.sle_rowcount
this.Control[iCurrent+9]=this.dw_libcodes
this.Control[iCurrent+10]=this.dw_add_delete_distribution_code
end on

on w_add_distribution_code.destroy
call super::destroy
destroy(this.cb_find)
destroy(this.cb_clear)
destroy(this.cb_exit)
destroy(this.cb_update)
destroy(this.cb_delete)
destroy(this.st_1)
destroy(this.st_2)
destroy(this.sle_rowcount)
destroy(this.dw_libcodes)
destroy(this.dw_add_delete_distribution_code)
end on

event close;call super::close;m_pics_main.m_file.m_print.Enabled 			=	FALSE

m_pics_main.m_file.m_pagesetup.Enabled		=	FALSE
m_pics_main.m_file.m_printimmediate.Enabled	=	FALSE
m_pics_main.m_edit.m_deleterow.Enabled 	=	FALSE
m_pics_main.m_edit.m_addrow.Enabled 		=	FALSE

end event

event mousemove;call super::mousemove;w_pics_main.setmicrohelp("Ready")
end event

event pfc_postopen;call super::pfc_postopen;cb_clear.Enabled = FALSE
cb_delete.Enabled = FALSE
cb_update.Enabled = FALSE
dw_add_delete_distribution_code.Visible = FALSE
m_pics_main.m_file.m_print.Enabled = TRUE
m_pics_main.m_file.m_pagesetup.Enabled = TRUE
m_pics_main.m_file.m_printimmediate.Enabled = TRUE
dw_libcodes.Setfocus()
datawindowchild ldwc_libcd
long li_cnt

dw_libcodes.GetChild('libcd',ldwc_libcd)
li_cnt=ldwc_libcd.RowCount()
//messagebox('','li_cnt in postopen = : '+string(li_cnt))
end event

event pfc_preopen;call super::pfc_preopen;this.of_SetPreference(TRUE)
this.inv_preference.of_SetToolBars(TRUE)
this.inv_preference.of_SetWindow(TRUE)
this.of_SetResize(TRUE)
this.inv_resize.of_SetOrigSize(this.workspacewidth(),this.workspaceheight())
inv_resize.of_Register(dw_add_delete_distribution_code, "scale")
inv_resize.of_Register(cb_clear, "scale")
inv_resize.of_Register(cb_exit, "scale")
inv_resize.of_Register(cb_find, "scale")
inv_resize.of_Register(cb_update, "scale")
inv_resize.of_Register(cb_delete, "scale")
inv_resize.of_Register(dw_libcodes, "scale")
inv_resize.of_Register(st_1, "scale")
inv_resize.of_Register(st_2, "scale")
inv_resize.of_Register(sle_rowcount, "scale")

end event

event open;call super::open;THIS.Windowstate = maximized!
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
					dw_add_delete_distribution_code.Setfocus()
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
			rtn = cb_update.Triggerevent(CLICKED!)
			if rtn = 1 THEN
				RETURN 0
			end if
		Case 2
			// NO - Allow the window to be closed without saving changes
			Return 0
		Case 3
			dw_add_delete_distribution_code.Setfocus()
			// CANCEL -  Prevent the window from closing
	End Choose
End If

// Prevent the window from closing
ib_closestatus = False
Return 1
end event

type cb_find from u_cb within w_add_distribution_code
event pfc_hinttext pbm_mousemove
string tag = "Find~'s the record based on selected library code from list"
integer x = 553
integer y = 1480
integer taborder = 0
integer textsize = -10
string text = "F&ind"
boolean default = true
end type

event pfc_hinttext;call super::pfc_hinttext;w_pics_main.setmicrohelp(This.tag)
end event

event clicked;call super::clicked;long ll_rows
string ls_libcd

dw_libcodes.Accepttext()
dw_add_delete_distribution_code.Accepttext()

ls_libcd = trim(dw_libcodes.Getitemstring(dw_libcodes.Getrow(),"libcd"))

//This Code is checking if user doesn't enter anything 
//and simply presses enter key then showing the messagebox to the user 
IF ISNULL(ls_libcd) OR TRIM(ls_libcd) = "" THEN
	Messagebox("Required Data","You must enter a valid library code to retrieve the records.",STOPSIGN!)
	dw_libcodes.Setfocus()
	dw_add_delete_distribution_code.Visible = FALSE
	ib_disableclosequery = TRUE
	dw_libcodes.Modify("libcd.DDDW.ShowList= NO")
   dw_libcodes.Modify("libcd.DDDW.VScrollbar = NO")
	RETURN
END IF
//If user enter's a libcd then checking the libcd from "lib" table
IF NOT ISNULL(ls_libcd)  THEN
	SELECT libcd INTO :is_libcd
	FROM lib
	WHERE lib.libcd = :ls_libcd
	USING sqlservertrans;
	IF trim(is_libcd) <> TRIM(ls_libcd) THEN
		Messagebox("Library Code","The library code is invalid. ~nPlease enter a valid library code.")
		dw_libcodes.Reset()
		dw_libcodes.Event pfc_addrow()
		dw_libcodes.Setfocus()
		dw_libcodes.Modify("libcd.Tabsequence = 10")
		cb_find.Enabled = TRUE
      cb_clear.Enabled = FALSE
      ib_disableclosequery = TRUE
      cb_find.Default = TRUE
      cb_delete.Enabled = FALSE
      cb_update.Enabled = FALSE
		dw_add_delete_distribution_code.Visible = FALSE
		dw_libcodes.Modify("libcd.DDDW.ShowList= NO")
      dw_libcodes.Modify("libcd.DDDW.VScrollbar = NO")		
		RETURN
	ELSE
		is_libcd = ls_libcd
	   ll_rows =dw_add_delete_distribution_code.Event pfc_Retrieve()
	   IF ll_rows = 0 THEN
		   dw_add_delete_distribution_code.Reset()
		   dw_libcodes.Reset()
		   dw_libcodes.Modify("libcd.Tabsequence = 10")
		   dw_libcodes.Event pfc_addrow()
		   dw_libcodes.setfocus()
		   dw_libcodes.Setcolumn("libcd")
         cb_find.Enabled = TRUE
         cb_clear.Enabled = FALSE
         ib_disableclosequery = TRUE
         cb_find.Default = TRUE
         cb_delete.Enabled = FALSE
         cb_update.Enabled = FALSE
         dw_add_delete_distribution_code.Visible = FALSE
		   sle_rowcount.Text = "" 
			dw_libcodes.Modify("libcd.DDDW.ShowList= NO")
         dw_libcodes.Modify("libcd.DDDW.VScrollbar = NO")			
		   RETURN
	    ELSE
			dwItemStatus l_status

         l_status = dw_add_delete_distribution_code.GetItemStatus(dw_add_delete_distribution_code.getrow(), 0, Primary!)
         IF l_status = NewModified! THEN
				dw_add_delete_distribution_code.Visible = TRUE
	         dw_add_delete_distribution_code.setfocus()
	         dw_add_delete_distribution_code.setcolumn("def_med")
	         dw_add_delete_distribution_code.Modify("def_med.tabsequence = 10")
	         dw_add_delete_distribution_code.Modify("def_casub.tabsequence = 20")
	         dw_add_delete_distribution_code.Modify("def_defqty.tabsequence = 30")
	         dw_add_delete_distribution_code.Modify("def_defdt.tabsequence = 40")
			   dw_add_delete_distribution_code.setfocus()
	         dw_add_delete_distribution_code.setcolumn("def_med")
				dw_libcodes.Modify("libcd.Tabsequence = 0")
				cb_find.Enabled = FALSE
		      cb_clear.Enabled = TRUE
		      cb_delete.Enabled = TRUE
		      cb_update.Enabled = TRUE
				ib_disableclosequery = FALSE
				RETURN
			END IF
	
		    dw_add_delete_distribution_code.Visible = TRUE
		    cb_find.Enabled = FALSE
		    cb_clear.Enabled = TRUE
		    cb_delete.Enabled = TRUE
		    cb_update.Enabled = TRUE
		    dw_add_delete_distribution_code.Setfocus()
			 dw_add_delete_distribution_code.Modify("def_med.Tabsequence = 0")
			 dw_add_delete_distribution_code.Modify("def_casub.Tabsequence = 0")
			 dw_add_delete_distribution_code.Modify("def_defqty.Tabsequence = 10")
		    dw_add_delete_distribution_code.Modify("def_defdt.Tabsequence = 20")
		    dw_add_delete_distribution_code.Setcolumn("def_defqty")
		    ib_disableclosequery = FALSE
		    dw_libcodes.Modify("libcd.Tabsequence = 0")
		    RETURN
	     END IF
	   END IF
    END IF
	  

end event

type cb_clear from u_cb within w_add_distribution_code
event pfc_hinttext pbm_mousemove
string tag = "Clear the record(s)"
integer x = 1833
integer y = 1480
integer taborder = 0
integer textsize = -10
string text = "&Clear"
end type

event pfc_hinttext;call super::pfc_hinttext;w_pics_main.setmicrohelp(This.tag)
end event

event clicked;call super::clicked;dw_add_delete_distribution_code.Reset()
dw_libcodes.object.libcd[1] = ""

DataWindowChild ldwc_dddw

IF dw_libcodes.GetChild("libcd", ldwc_dddw) = -1 THEN
	Return -1
ELSE
	ldwc_dddw.SetTransObject(sqlservertrans)	
	ldwc_dddw.Retrieve()
END IF
dw_libcodes.Reset()
dw_libcodes.Modify("libcd.DDDW.ShowList= NO")
dw_libcodes.Modify("libcd.DDDW.VScrollbar = NO")
dw_libcodes.Modify("libcd.Tabsequence = 10")
dw_libcodes.Event pfc_addrow()
dw_libcodes.Setfocus()
dw_libcodes.Setcolumn("libcd")
cb_find.Enabled = TRUE
cb_clear.Enabled = FALSE
ib_disableclosequery = TRUE
cb_find.Default = TRUE
cb_delete.Enabled = FALSE
cb_update.Enabled = FALSE
dw_add_delete_distribution_code.Visible = FALSE
sle_rowcount.Text = ""



end event

type cb_exit from u_cb within w_add_distribution_code
event pfc_hinttext pbm_mousemove
string tag = "Exits from current screen"
integer x = 2272
integer y = 1484
integer taborder = 0
integer textsize = -10
string text = "E&xit"
end type

event pfc_hinttext;call super::pfc_hinttext;w_pics_main.setmicrohelp(This.tag)
end event

event clicked;call super::clicked;dw_libcodes.Modify("libcd.DDDW.ShowList= NO")
dw_libcodes.Modify("libcd.DDDW.VScrollbar = NO")
parent.Event pfc_close()
m_pics_main.m_menu.popMenu(300,0)
end event

type cb_update from u_cb within w_add_distribution_code
event pfc_hinttext pbm_mousemove
string tag = "Update the records"
integer x = 983
integer y = 1480
integer taborder = 0
integer textsize = -10
string text = "&Update"
end type

event pfc_hinttext;call super::pfc_hinttext;w_pics_main.setmicrohelp(This.tag)
end event

event clicked;call super::clicked;string ls_med,ls_libcd,ls_casub,ls_message,ls_msgparm[1]
integer rtn
long i,ll_rows


dw_add_delete_distribution_code.Accepttext()
dw_libcodes.Accepttext()



ll_rows = dw_add_delete_distribution_code.Rowcount()
	FOR i = ll_rows TO 1 STEP -1 
	ls_med = dw_add_delete_distribution_code.Getitemstring(i,"def_med")
	IF ISNULL(ls_med) OR TRIM(ls_med) = "" THEN
      dw_add_delete_distribution_code.Deleterow(i)		
	END IF
NEXT
rtn = parent.Event pfc_save()
IF rtn = 0 THEN
	Messagebox("Update","Nothing to update.")
	dw_libcodes.Setfocus()
	RETURN 0
END IF
IF rtn = 1 THEN
	COMMIT USING sqlservertrans;
	Close(w_pics_update_msg_box)
	Messagebox("Update","Update Successful.")
	dw_add_delete_distribution_code.Modify("def_med.tabsequence = 0")
	dw_add_delete_distribution_code.Modify("def_casub.tabsequence = 0")
	dw_add_delete_distribution_code.Modify("def_defqty.tabsequence = 10")
	dw_add_delete_distribution_code.Modify("def_defdt.tabsequence = 20")
	
	RETURN 1
ELSEIF sqlservertrans.SQLCode < 0 THEN
				ls_message = "A database error has occurred during Update.~n" + &
								 "Database error code:  " + String (sqlservertrans.sqldbcode) + "~r~n" + &
								 "Database error message:~r~n" + sqlservertrans.sqlerrtext
				IF IsValid(gnv_app.inv_error) THEN
					ls_msgparm[1] = ls_message
					gnv_app.inv_error.of_Message ('pfc_dwdberror', ls_msgparm, &
					gnv_app.iapp_object.DisplayName)
				ELSE
					Messagebox (gnv_app.iapp_object.DisplayName, ls_message, StopSign!, Ok!)
					ROLLBACK USING sqlservertrans;
					RETURN -1
				End If	
END IF

	
end event

type cb_delete from u_cb within w_add_distribution_code
event pfc_hinttext pbm_mousemove
string tag = "Deletes the highlighted record(s)"
integer x = 1408
integer y = 1480
integer taborder = 0
integer textsize = -10
string text = "&Delete"
end type

event pfc_hinttext;call super::pfc_hinttext;w_pics_main.setmicrohelp(This.tag)
end event

event clicked;call super::clicked;long ll_row, rtn,  ll_count
boolean ib_found
integer i = 1

ll_count = dw_add_delete_distribution_code.Rowcount()
ll_row = dw_add_delete_distribution_code.getselectedrow(ll_row)
IF ll_row > 0 THEN
	rtn = Messagebox("Delete","Deleting highlighted record(s). ~nTo complete deletion click Update")
	dw_add_delete_distribution_code.setfocus()
	is_libcd = trim(dw_add_delete_distribution_code.getitemstring(ll_row,"def_libcd"))
	ib_found = TRUE
	IF rtn = 1 THEN
		DO WHILE ib_found
			ll_row = 0
			ll_row = dw_add_delete_distribution_code.getselectedrow(ll_row)
			IF ll_row > 0 THEN
				is_libcd = trim(dw_add_delete_distribution_code.getitemstring(ll_row,"def_libcd"))
				dw_add_delete_distribution_code.deleterow(ll_row)
				i++
			ELSE
				ib_found = FALSE
			END IF
		LOOP
	ELSE
		RETURN
	END IF
ELSE
	Messagebox("Delete","Invalid record selection. ~nPlease select some record(s) before clicking Delete")
	dw_add_delete_distribution_code.setfocus()
END IF

end event

type st_1 from u_st within w_add_distribution_code
integer x = 23
integer y = 40
integer width = 759
integer textsize = -10
long backcolor = 79741120
string text = "Please Select Library Code"
end type

type st_2 from u_st within w_add_distribution_code
integer x = 1440
integer y = 44
integer width = 919
integer textsize = -10
long backcolor = 79741120
string text = "Total Number of Rows Retrieved"
long bordercolor = 79741120
end type

type sle_rowcount from u_sle within w_add_distribution_code
integer x = 2368
integer y = 28
integer width = 229
integer height = 88
integer taborder = 0
integer textsize = -10
integer weight = 700
long textcolor = 255
boolean displayonly = true
end type

type dw_libcodes from u_dw within w_add_distribution_code
event pfc_keydown pbm_dwnkey
event pfc_hinttext pbm_mousemove
integer x = 791
integer y = 12
integer width = 352
integer height = 104
integer taborder = 10
string dataobject = "d_libcode"
boolean vscrollbar = false
boolean border = false
boolean livescroll = false
borderstyle borderstyle = stylebox!
end type

event pfc_hinttext;call super::pfc_hinttext;string ls_object, ls_column, ls_column_tag
long ll_pos

//This script set's microhelp at the bottom of the screen for the dw_libcodes
ls_object = THIS.getobjectatpointer()
ll_pos = pos(ls_object, "~t")
IF NOT pos(ls_object, "_t~t") > 0 THEN
	IF ll_pos > 0 THEN
		ll_pos = ll_pos -1
		ls_column = mid(ls_object,1,ll_pos)
		ls_column_tag = THIS.Describe(ls_column + ".tag")
		w_pics_main.setmicrohelp(ls_column_tag)
	ELSE
		w_pics_main.setmicrohelp("Ready")
	END IF
END IF
end event

event constructor;call super::constructor;long li_cnt
datawindowchild dwclibcd

THIS.of_settransobject(sqlservertrans)
//of_setrowselect(TRUE)
//of_setrowmanager(TRUE)
//Making Dropdowndatawindow not updateable
of_setupdateable(FALSE)
//Dropdownsearch services on
dw_libcodes.of_SetDropDownSearch(TRUE)
//Retrieve based on entered library code
dw_libcodes.inv_dropdownsearch.of_AddColumn ("libcd")
//GetChild('libcd',dwclibcd)
//li_cnt=dwclibcd.RowCount()
//messagebox('','count : '+string(li_cnt))
//Adding a empty row in dddw
dw_libcodes.Event pfc_addrow()


end event

event pfc_retrievedddw;call super::pfc_retrievedddw;Long	ll_return,li_cnt

ll_return = dw_libcodes.Event pfc_RetrieveDDDW  &
	("libcd")
messagebox('ll_return','row count = : '+string(ll_return))

//This example shows the code you add to the dw_libcodes DataWindow to retrieve rows for the state table:

DataWindowChild ldwc_dddw

IF this.GetChild(as_column, ldwc_dddw) = -1 THEN
	Return -1
ELSE
	ldwc_dddw.SetTransObject(sqlservertrans)	
	ldwc_dddw.retrieve()
	li_cnt=ldwc_dddw.RowCount()
	messagebox('','li_cnt = : '+string(li_cnt))
	Return ldwc_dddw.Retrieve()
END IF
end event

event editchanged;call super::editchanged;IF IsValid(inv_dropdownsearch) THEN
	dw_libcodes.inv_dropdownsearch.Event pfc_EditChanged(row, dwo, data)
	dw_libcodes.Modify("libcd.DDDW.ShowList = Yes")
	dw_libcodes.Modify("libcd.DDDW.VScrollbar = Yes")	
ELSE
	dw_libcodes.Modify("libcd.DDDW.ShowList= NO")
   dw_libcodes.Modify("libcd.DDDW.VScrollbar = NO")
END IF
	
//	dw_libcodes.inv_dropdownsearch.Event pfc_EditChanged(row, dwo, data)
//END IF
//

end event

event itemfocuschanged;call super::itemfocuschanged;IF IsValid(inv_dropdownsearch) THEN
	dw_libcodes.inv_dropdownsearch.Event pfc_ItemFocusChanged(row,dwo)
END IF
end event

type dw_add_delete_distribution_code from u_dw within w_add_distribution_code
event pfc_hinttext pbm_mousemove
event ue_enterkey pbm_dwnprocessenter
event pfc_keydown pbm_dwnkey
integer y = 140
integer width = 2661
integer height = 1312
integer taborder = 20
string dataobject = "d_add_delete_distribution_code"
boolean vscrollbar = false
boolean livescroll = false
end type

event pfc_hinttext;call super::pfc_hinttext;string ls_object, ls_column, ls_column_tag
long ll_pos

//This script set's microhelp at the bottom of the screen for the dw_add_distribution_code
ls_object = THIS.getobjectatpointer()
ll_pos = pos(ls_object, "~t")
IF NOT pos(ls_object, "_t~t") > 0 THEN
	IF ll_pos > 0 THEN
		ll_pos = ll_pos -1
		ls_column = mid(ls_object,1,ll_pos)
		ls_column_tag = THIS.Describe(ls_column + ".tag")
		w_pics_main.setmicrohelp(ls_column_tag)
	ELSE
		w_pics_main.setmicrohelp("Ready")
	END IF
END IF
end event

event ue_enterkey;call super::ue_enterkey;Send(Handle(this),256,9,Long(0,0))
return(1)
end event

event pfc_keydown;call super::pfc_keydown;long ll_row

IF Key = KeyDownArrow! THEN
	ll_row = THIS.getrow()
	THIS.Scrolltorow(ll_row)
END IF
//IF Key = Keys! THEN
//	ll_row = THIS.getrow()
//	IF dw_add_delete_distribution_code.ISSelected(ll_row) THEN
//		dw_add_delete_distribution_code.Selectrow(ll_row,FALSE)
//	ELSE
//		dw_add_delete_distribution_code.Selectrow(ll_row,TRUE)
//	END IF
//END IF
	
end event

event constructor;call super::constructor;THIS.of_SetTransObject( sqlservertrans )
ib_rmbmenu = FALSE
of_setrowselect(TRUE)
of_setrowmanager(TRUE)
of_setupdateable(TRUE)
inv_rowselect.of_setstyle(2)
THIS.setrowfocusindicator(Hand!)




end event

event getfocus;call super::getfocus;m_pics_main.m_edit.m_deleterow.Enabled =	TRUE
m_pics_main.m_edit.m_addrow.Enabled =	TRUE
m_pics_main.m_edit.m_cut.Enabled =	FALSE
end event

event pfc_retrieve;call super::pfc_retrieve;THIS.Reset()
RETURN THIS.Retrieve(is_libcd)
end event

event rowfocuschanged;call super::rowfocuschanged;currentrow = currentrow
end event

event retrieveend;call super::retrieveend;long ll_row
integer rtn

sle_rowcount.Text = string(rowcount)

IF NOT rowcount > 0 THEN
	rtn = Messagebox("Retrieve","No data found. Do you want to add a new record?",QUESTION!,YESNO!)
	IF rtn = 1 THEN
		ll_row = dw_add_delete_distribution_code.Event pfc_addrow()
	   This.Setitem(ll_row,"def_libcd",is_libcd)		
	ELSE
		RETURN 
	END IF
END IF

end event

event pfc_addrow;long ll_rc

ll_rc = Super::Event pfc_addrow()
dw_add_delete_distribution_code.SetItem(ll_rc, "def_libcd",is_libcd)
dw_add_delete_distribution_code.Modify("def_med.tabsequence = 10")
dw_add_delete_distribution_code.Modify("def_casub.tabsequence = 20")
dw_add_delete_distribution_code.Modify("def_defqty.tabsequence = 30")
dw_add_delete_distribution_code.Modify("def_defdt.tabsequence = 40")
dw_add_delete_distribution_code.Setfocus()
dw_add_delete_distribution_code.Setcolumn("def_med")
return ll_rc
end event

event itemchanged;call super::itemchanged;time lt_time=time('00:00:00')
datetime ldt_dt, ldt_today=datetime(today(),lt_time)
date ld_date


IF dwo.name = "def_casub" OR dwo.name = "def_med" OR  dwo.name = "def_defqty" THEN
	ldt_dt = dw_add_delete_distribution_code.Getitemdatetime(row,"def_defdt")
	ld_date=date(ldt_dt)
	IF ISNULL(ld_date) OR ISDATE(string(ld_date)) THEN
      dw_add_delete_distribution_code.Setitem(row,"def_defdt",ldt_today)
		ld_date = Today()
	END IF
END IF






end event

event updatestart;call super::updatestart; Open(w_pics_update_msg_box)
end event

event updateend;call super::updateend; Close(w_pics_update_msg_box)
end event


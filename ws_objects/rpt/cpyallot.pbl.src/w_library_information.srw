$PBExportHeader$w_library_information.srw
forward
global type w_library_information from w_sheet
end type
type dw_rs21_libdef2 from u_pics_dw within w_library_information
end type
type cb_find from u_cb within w_library_information
end type
type cb_update from u_cb within w_library_information
end type
type cb_clear from u_cb within w_library_information
end type
type cb_exit from u_cb within w_library_information
end type
type cb_delete from u_cb within w_library_information
end type
type dw_def_insert from u_dw within w_library_information
end type
type st_1 from u_st within w_library_information
end type
type dw_library_information from u_dw within w_library_information
end type
end forward

global type w_library_information from w_sheet
integer x = 105
integer y = 460
integer width = 2994
integer height = 1524
string title = "Library Information"
dw_rs21_libdef2 dw_rs21_libdef2
cb_find cb_find
cb_update cb_update
cb_clear cb_clear
cb_exit cb_exit
cb_delete cb_delete
dw_def_insert dw_def_insert
st_1 st_1
dw_library_information dw_library_information
end type
global w_library_information w_library_information

type variables
string is_libcd,ls_libcds
datastore ids1,ids2
end variables

on w_library_information.create
int iCurrent
call super::create
this.dw_rs21_libdef2=create dw_rs21_libdef2
this.cb_find=create cb_find
this.cb_update=create cb_update
this.cb_clear=create cb_clear
this.cb_exit=create cb_exit
this.cb_delete=create cb_delete
this.dw_def_insert=create dw_def_insert
this.st_1=create st_1
this.dw_library_information=create dw_library_information
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_rs21_libdef2
this.Control[iCurrent+2]=this.cb_find
this.Control[iCurrent+3]=this.cb_update
this.Control[iCurrent+4]=this.cb_clear
this.Control[iCurrent+5]=this.cb_exit
this.Control[iCurrent+6]=this.cb_delete
this.Control[iCurrent+7]=this.dw_def_insert
this.Control[iCurrent+8]=this.st_1
this.Control[iCurrent+9]=this.dw_library_information
end on

on w_library_information.destroy
call super::destroy
destroy(this.dw_rs21_libdef2)
destroy(this.cb_find)
destroy(this.cb_update)
destroy(this.cb_clear)
destroy(this.cb_exit)
destroy(this.cb_delete)
destroy(this.dw_def_insert)
destroy(this.st_1)
destroy(this.dw_library_information)
end on

event close;call super::close;m_pics_main.m_file.m_print.Enabled 			=	FALSE
m_pics_main.m_file.m_pagesetup.Enabled		=	FALSE
m_pics_main.m_file.m_printimmediate.Enabled	=	FALSE
m_pics_main.m_edit.m_deleterow.Enabled 	=	FALSE
m_pics_main.m_edit.m_addrow.Enabled 		=	FALSE

DESTROY ids1
DESTROY ids2

end event

event closequery;Integer	li_pendingrc
Integer	li_validationrc
Integer	li_accepttextrc
Integer	li_msg
Integer	li_rc,rtn
String	ls_msgparms[]

string ls_libcd

dw_library_information.Accepttext()

ls_libcd = trim(dw_library_information.Getitemstring(dw_library_information.Getrow(),"libcd"))
IF Isnull(Trim(ls_libcd)) OR Trim(ls_libcd) = "" THEN
	ib_disableclosequery = TRUE	
END IF



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
					dw_library_information.Setfocus()
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
			dw_library_information.Setfocus()
			// CANCEL -  Prevent the window from closing
	End Choose
End If

// Prevent the window from closing
ib_closestatus = False
Return 1
end event

event mousemove;call super::mousemove;w_pics_main.setmicrohelp("Ready")
end event

event pfc_postopen;call super::pfc_postopen;dw_library_information.Event pfc_addrow()
cb_clear.Enabled = FALSE
cb_update.Enabled = FALSE
cb_delete.Enabled = FALSE
m_pics_main.m_file.m_print.Enabled = TRUE
m_pics_main.m_file.m_pagesetup.Enabled = TRUE
m_pics_main.m_file.m_printimmediate.Enabled = TRUE
st_1.visible = FALSE
dw_library_information.Setfocus()
dw_def_insert.hide()

end event

event pfc_preopen;call super::pfc_preopen;this.of_SetPreference(TRUE)
this.inv_preference.of_SetToolBars(TRUE)
this.inv_preference.of_SetWindow(TRUE)
this.of_SetResize(TRUE)
this.inv_resize.of_SetOrigSize(this.workspacewidth(),this.workspaceheight())
inv_resize.of_Register(dw_library_information, "scale")
inv_resize.of_Register(dw_def_insert, "scale")
inv_resize.of_Register(cb_clear, "scale")
inv_resize.of_Register(cb_exit, "scale")
inv_resize.of_Register(cb_find, "scale")
inv_resize.of_Register(cb_update, "scale")
inv_resize.of_Register(cb_delete, "scale")
inv_resize.of_Register(st_1, "scale")


end event

event pfc_endtran;call super::pfc_endtran;Integer i,li_count,rtn,lcnt
//datastore lds1,lds2
String ls_libcd,ls_data
Long ll_rows, ll_rows2
String lpriority,llibcd,Lmed,lcasub,lcasub_desc
Integer ldef_n_qty,ldef_p_qty,ldef_r_qty, J=1
//date ldefdt,null_date
DateTime ldt_defdt, null_date


IF ai_update_results = 1 AND dw_def_insert.visible = TRUE THEN
		
		ls_data = Trim(dw_def_insert.object.libcd[1])
		
		IF NOT IsNull(ls_data) THEN
			ids2 = CREATE DataStore
			ids2.dataObject = "d_rs20_libdef"
			ids2.SetTransObject(SqlServerOracleTrans)
			
			ids1 = CREATE DataStore
			ids1.dataObject = "d_rs21_libdef2"
			ids1.SetTransObject(SqlServerOracleTrans)
			//messagebox("SQL",ls_data)
			ids1.Retrieve(ls_data)
			ll_rows = ids1.RowCount()
			li_count = ids1.RowsCopy(1, ll_rows, primary!, ids2,1, primary!)
			IF li_count = 1 THEN
				//messagebox("SQL",ls_data+" "+string(ll_rows))
				ls_libcd = Trim(dw_library_information.object.libcd[1])
				FOR i = ll_rows TO 1 STEP -1
					ids2.object.libcd[i]=ls_libcd
				NEXT	
				rtn = ids2.Update()
				IF rtn=1 THEN
					COMMIT USING SQLserverOracleTrans;
					Messagebox("New Library","New library code for "+ls_libcd+" created with default values of "+ls_data+" into libdef(RS21n) table .")
					RETURN 1
				ELSE
					Messagebox("Update","Updating libdef on WEB has failed.")
					ROLLBACK USING SQLserverOracleTrans;
					 Close(w_pics_update_msg_box)					
					 RETURN -1
				END IF
			ELSE
				Messagebox("ERROR","ERROR in row copy.")
				RETURN -1		
			END IF
		END IF
END IF	
RETURN 1
end event

event pfc_begintran;call super::pfc_begintran; RETURN Open(w_pics_update_msg_box)
end event

type dw_rs21_libdef2 from u_pics_dw within w_library_information
boolean visible = false
integer x = 2505
integer y = 416
integer width = 206
integer height = 136
integer taborder = 20
string dataobject = "d_rs21_libdef2"
end type

event constructor;call super::constructor;this.settransobject(sqlserveroracletrans)
end event

type cb_find from u_cb within w_library_information
event pfc_hinttext pbm_mousemove
string tag = "Find~'s the record based on the valid library code"
integer x = 1074
integer y = 1288
integer width = 389
integer taborder = 0
integer textsize = -10
string text = "F&ind"
boolean default = true
end type

event pfc_hinttext;call super::pfc_hinttext;w_pics_main.setmicrohelp(This.tag)
end event

event clicked;call super::clicked;string ls_libcd
long ll_rows
integer rtn

dw_library_information.Accepttext()

ls_libcd = trim(dw_library_information.Getitemstring(dw_library_information.Getrow(),"libcd"))

IF ISNULL(TRIM(ls_libcd)) OR TRIM(ls_libcd) = "" THEN
	Messagebox("Required Data","You must enter a valid library code to retrieve the records.",STOPSIGN!)
	dw_library_information.setitem(dw_library_information.Getrow(),"libcd","")
	dw_library_information.Modify("libcd.Tabsequence = 10")
	dw_library_information.Modify("libtyp.Tabsequence = 0")
   dw_library_information.Modify("libstcd.Tabsequence = 0")
   dw_library_information.Modify("lendcd.Tabsequence = 0")
   dw_library_information.Modify("msc.Tabsequence = 0")
   dw_library_information.Modify("date_added.Tabsequence = 0")
   dw_library_information.Modify("date_dropped.Tabsequence = 0")
   dw_library_information.Modify("reason.Tabsequence = 0")
	dw_library_information.Setfocus()
	dw_library_information.Setcolumn("libcd")
	ib_disableclosequery = TRUE
	RETURN
END IF

IF NOT ISNULL(ls_libcd)  THEN
	is_libcd = ls_libcd
	ll_rows = dw_library_information.Event pfc_Retrieve()
	IF ll_rows <> 0 THEN		
      cb_find.Enabled = FALSE
      cb_clear.Enabled = TRUE
      ib_disableclosequery = FALSE
      cb_find.Default = FALSE         
      cb_update.Enabled = TRUE
		cb_delete.Enabled = TRUE
      dw_library_information.setitem(dw_library_information.Getrow(),"libcd",ls_libcd)
      dw_library_information.Setfocus()
		dw_library_information.Modify("libcd.Tabsequence = 0")
		dw_library_information.Modify("libtyp.Tabsequence = 20")		
      dw_library_information.Modify("libstcd.Tabsequence = 30")
      dw_library_information.Modify("lendcd.Tabsequence = 40")
      dw_library_information.Modify("msc.Tabsequence = 50")
		dw_library_information.Modify("date_added.Tabsequence = 60")
		dw_library_information.Modify("date_dropped.Tabsequence = 70")
		dw_library_information.Modify("reason.Tabsequence = 80")
		RETURN
	ELSE
		dw_library_information.Event pfc_addrow()
		SETNULL(ls_libcd)
		dw_library_information.setitem(1,"libcd",ls_libcd)
		dw_library_information.Setfocus()
		dw_library_information.Modify("libcd.Tabsequence = 10")
		dw_library_information.Modify("libtyp.Tabsequence = 20")
		dw_library_information.Modify("libstcd.Tabsequence = 30")
      dw_library_information.Modify("lendcd.Tabsequence = 40")
      dw_library_information.Modify("msc.Tabsequence = 50")
		dw_library_information.Modify("date_added.Tabsequence = 60")
		dw_library_information.Modify("date_dropped.Tabsequence = 70")
		dw_library_information.Modify("reason.Tabsequence = 80")
		cb_find.Enabled = TRUE
		cb_clear.Enabled = FALSE		   
		cb_update.Enabled = FALSE
		cb_delete.Enabled = FALSE
		ib_disableclosequery = FALSE			
		RETURN
	END IF
END IF
		
	   
    

end event

type cb_update from u_cb within w_library_information
event pfc_hinttext pbm_mousemove
string tag = "Update the record"
integer x = 1559
integer y = 1288
integer width = 379
integer taborder = 0
integer textsize = -10
string text = "&Update"
end type

event pfc_hinttext;call super::pfc_hinttext;w_pics_main.setmicrohelp(This.tag)
end event

event clicked;call super::clicked;string ls_deflibcd
integer rtn

dw_library_information.Accepttext()
dw_def_insert.accepttext()

//Check Database connectivity
IF NOT sqlserveroracletrans.DbHandle() >0 THEN
	sqlserveroracletrans.of_connect() 
END IF


IF dw_def_insert.visible = TRUE THEN
	ls_deflibcd = trim(dw_def_insert.object.libcd[1])
	IF ISNULL(Trim(ls_deflibcd)) OR Trim(ls_deflibcd) = "" THEN 
	Messagebox("Library Code","Select library code before pressing Update.",STOPSIGN!)
	dw_def_insert.setfocus()
	RETURN
   END IF
END IF


rtn = parent.Event pfc_save()
IF rtn = 0 THEN
	Messagebox("Update","Nothing to update.")
	dw_library_information.Setfocus()
	RETURN 0
END IF
IF rtn = 1 THEN	
  COMMIT USING sqlservertrans;
  Close(w_pics_update_msg_box) 
  Messagebox("Update","Update Successful")
  dw_library_information.Setfocus()
  RETURN 
ELSE
	ROLLBACK USING sqlservertrans;
	RETURN -1
END IF



end event

type cb_clear from u_cb within w_library_information
event pfc_hinttext pbm_mousemove
string tag = "Clear the record"
integer x = 2007
integer y = 1292
integer width = 393
integer taborder = 0
integer textsize = -10
string text = "&Clear"
end type

event pfc_hinttext;call super::pfc_hinttext;w_pics_main.setmicrohelp(This.tag)
end event

event clicked;call super::clicked;dw_def_insert.Reset()
DataWindowChild ldwc_dddws

IF dw_def_insert.GetChild("libcd", ldwc_dddws) = -1 THEN
	Return -1
ELSE
	ldwc_dddws.SetTransObject(sqlservertrans)	
	ldwc_dddws.Retrieve()
END IF
dw_def_insert.Hide()

dw_library_information.Reset()
DataWindowChild ldwc_dddw

IF dw_library_information.GetChild("libcd", ldwc_dddw) = -1 THEN
	Return -1
ELSE
	ldwc_dddw.SetTransObject(sqlservertrans)	
	ldwc_dddw.Retrieve()
END IF
dw_library_information.Reset()
//dw_library_information.triggerevent("pfc_retrievedddw")
dw_library_information.insertrow(0)
//dw_library_information.setitem(dw_library_information.getrow(),"libcd","")
dw_library_information.setfocus()
dw_library_information.Modify("libcd.Tabsequence = 10")
dw_library_information.Modify("libtyp.Tabsequence = 0")
dw_library_information.Modify("libstcd.Tabsequence = 0")
dw_library_information.Modify("lendcd.Tabsequence = 0")
dw_library_information.Modify("msc.Tabsequence = 0")
dw_library_information.Modify("date_added.Tabsequence = 0")
dw_library_information.Modify("date_dropped.Tabsequence = 0")
dw_library_information.Modify("reason.Tabsequence = 0")
cb_find.Enabled = TRUE
cb_clear.Enabled = FALSE
st_1.Visible = FALSE
ib_disableclosequery = TRUE
cb_find.Default = TRUE
cb_update.Enabled = FALSE
cb_delete.Enabled = FALSE


end event

type cb_exit from u_cb within w_library_information
event pfc_hinttext pbm_mousemove
string tag = "Exits the current screen"
integer x = 2496
integer y = 1292
integer width = 402
integer taborder = 0
integer textsize = -10
string text = "E&xit"
end type

event pfc_hinttext;call super::pfc_hinttext;w_pics_main.setmicrohelp(This.tag)
end event

event clicked;call super::clicked;//string ls_libcd
//
//dw_library_information.Accepttext()
//
//ls_libcd = dw_library_information.Getitemstring(dw_library_information.Getrow(),"libcd")
//IF Isnull(Trim(ls_libcd)) OR Trim(ls_libcd) = "" THEN
//	ib_disableclosequery = TRUE	
//END IF
parent.Event pfc_close()
m_pics_main.m_menu.popMenu(300,0)
end event

type cb_delete from u_cb within w_library_information
event pfc_hinttext pbm_mousemove
string tag = "Deletes the library code from lib table and its relatedrecord"
integer x = 46
integer y = 1288
integer width = 393
integer taborder = 0
integer textsize = -10
string text = "&Delete"
end type

event pfc_hinttext;call super::pfc_hinttext;w_pics_main.setmicrohelp(This.tag)
end event

event clicked;call super::clicked;integer rtn,rtns,li_save
string ls_libcd,ls_libtyp,ls_libstcd,ls_lendcd,ls_msc

dw_library_information.Accepttext()

ls_libcd = trim(dw_library_information.getitemstring(dw_library_information.Getrow(),"libcd"))


rtn = Messagebox("Delete","Deleting the record for library code:  "+ls_libcd+"~nAre you sure?",Question!,YESNO!)
IF rtn = 1 THEN

	rtns = dw_library_information.Event pfc_deleterow()
	li_save = dw_library_information.update()

	IF li_save = 1 THEN
		delete from libdef
		where libcd = :ls_libcd
		using sqlserveroracletrans;
		if f_check_dberror(sqlservertrans,"LIBDEF") then
			commit using sqlserveroracletrans;
			commit using sqlservertrans;
		else
			rollback using sqlserveroracletrans;
			rollback using sqlservertrans;
		end if
		Messagebox("Delete","Library record successfully deleted.")
      cb_clear.triggerevent(Clicked!)
		RETURN
	END IF
END IF
	
end event

type dw_def_insert from u_dw within w_library_information
event pfc_keydown pbm_dwnkey
integer x = 2464
integer y = 208
integer width = 320
integer height = 100
integer taborder = 0
string dataobject = "d_def_insertion"
boolean vscrollbar = false
boolean livescroll = false
borderstyle borderstyle = stylebox!
end type

event pfc_keydown;string ls_libcode

dw_def_insert.Accepttext()

IF key = KeyEnter! AND dw_def_insert.Visible = TRUE THEN
	ls_libcode = trim(dw_def_insert.object.libcd[1])
	
    IF ISNULL(ls_libcode) OR ls_libcode = "" THEN
	    Messagebox("Library Code","Please select library code before clicking on update.")
	    dw_def_insert.Setfocus()
	    RETURN
    ELSE
	       Messagebox("Library Code", "Please click on Update to add this "+ls_libcode+" libcd values into Def table.")
	       dw_def_insert.setfocus()
    END IF
END IF

end event

event constructor;call super::constructor;THIS.of_SetTransObject( sqlservertrans )
This.of_setdropdownsearch(TRUE)
This.inv_dropdownsearch.of_addcolumn("libcd")
ib_rmbmenu = FALSE
//of_setrowmanager(TRUE)
of_setupdateable(FALSE)
//This.event pfc_addrow()

end event

event editchanged;call super::editchanged;IF IsValid(inv_dropdownsearch) THEN
	dw_def_insert.inv_dropdownsearch.Event pfc_EditChanged(row, dwo, data)
	dw_def_insert.Modify("libcd.DDDW.ShowList = Yes")
	dw_def_insert.Modify("libcd.DDDW.VScrollbar = Yes")	
ELSE
	dw_def_insert.Modify("libcd.DDDW.ShowList= NO")
  dw_def_insert.Modify("libcd.DDDW.VScrollbar = NO")
END IF

end event

event getfocus;call super::getfocus;m_pics_main.m_edit.m_deleterow.Enabled =	FALSE
m_pics_main.m_edit.m_addrow.Enabled =	FALSE
m_pics_main.m_edit.m_cut.Enabled =	FALSE
this.visible = TRUE

end event

event itemfocuschanged;call super::itemfocuschanged;IF IsValid(inv_dropdownsearch) THEN
	dw_def_insert.inv_dropdownsearch.Event pfc_ItemFocusChanged(row,dwo)
END IF
end event

event pfc_retrievedddw;call super::pfc_retrievedddw;Long	ll_return

ll_return = dw_def_insert.Event pfc_RetrieveDDDW  &
	("libcd")


//This example shows the code you add to the dw_libcodes DataWindow to retrieve rows for the state table:

DataWindowChild ldwc_dddw

IF this.GetChild(as_column, ldwc_dddw) = -1 THEN
	Return -1
ELSE
	ldwc_dddw.SetTransObject(sqlservertrans)	
	Return ldwc_dddw.Retrieve()
END IF
end event

event updatestart;call super::updatestart;Open(w_pics_update_msg_box)
end event

event updateend;call super::updateend; Close(w_pics_update_msg_box)
end event

type st_1 from u_st within w_library_information
integer x = 2350
integer y = 28
integer width = 535
integer height = 156
integer textsize = -10
long backcolor = 79741120
string text = "Please Select Valid Libcd"
alignment alignment = center!
end type

type dw_library_information from u_dw within w_library_information
event pfc_hinttext pbm_mousemove
event ue_enterkey pbm_dwnprocessenter
integer x = 5
integer y = 20
integer width = 2322
integer height = 1216
integer taborder = 10
boolean bringtotop = true
string dataobject = "d_library_information"
boolean vscrollbar = false
boolean livescroll = false
end type

event pfc_hinttext;call super::pfc_hinttext;string ls_object, ls_column, ls_column_tag
long ll_pos

//This script set's microhelp at the bottom of the screen for the dw_library_information
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

event constructor;call super::constructor;THIS.of_SetTransObject( sqlservertrans )
This.of_setdropdownsearch(TRUE)
This.inv_dropdownsearch.of_addcolumn("libcd")
ib_rmbmenu = FALSE
of_setrowmanager(TRUE)
of_setupdateable(TRUE)
this.of_SetDropDownCalendar(TRUE)
this.iuo_calendar.of_Register("date_added",this.iuo_calendar.DDLB)
this.iuo_calendar.of_Register("date_dropped",this.iuo_calendar.DDLB)








end event

event getfocus;call super::getfocus;m_pics_main.m_edit.m_deleterow.Enabled =	FALSE
m_pics_main.m_edit.m_addrow.Enabled =	FALSE
m_pics_main.m_edit.m_cut.Enabled =	FALSE
end event

event pfc_retrieve;call super::pfc_retrieve;THIS.Reset()
RETURN THIS.Retrieve(is_libcd)
end event

event retrieveend;call super::retrieveend;string ls_libcd
long ll_row
integer rtn

IF NOT rowcount > 0 THEN
	rtn = Messagebox("New Library","This is a new library code to be added.~nDo you want to continue?",QUESTION!,YESNO!)
	IF rtn = 1 THEN
		ll_row = dw_library_information.Event pfc_addrow()
		ls_libcd = trim(dw_library_information.Getitemstring(ll_row,"libcd"))
	   This.Setitem(ll_row,"libcd",ls_libcd)
	ELSE
		RETURN 
	END IF
END IF

end event

event pfc_addrow;long ll_rc

ll_rc = Super::Event pfc_addrow()
dw_library_information.SetItem(ll_rc, "libcd",is_libcd)
return ll_rc
end event

event pfc_deleterow;integer	li_rc

if IsValid (inv_rowmanager) then
	li_rc = inv_rowmanager.event pfc_deleterow () 
else	
	li_rc = this.DeleteRow (0) 
end if

// Notify the Linkage Service 
IF IsValid ( inv_Linkage ) THEN 
	If li_rc > 0 Then inv_Linkage.Event pfc_DeleteRow (0) 
END IF 

dw_library_information.SetItemStatus(1, 0, Primary!, DataModified!)

return li_rc
end event

event rowfocuschanged;call super::rowfocuschanged;currentrow = currentrow
end event

event editchanged;call super::editchanged;IF IsValid(inv_dropdownsearch) THEN
	dw_library_information.inv_dropdownsearch.Event pfc_EditChanged(row, dwo, data)
END IF

end event

event itemfocuschanged;call super::itemfocuschanged;IF IsValid(inv_dropdownsearch) THEN
	dw_library_information.inv_dropdownsearch.Event pfc_ItemFocusChanged(row,dwo)
END IF


end event

event itemchanged;call super::itemchanged;IF dwo.name = "msc" and NOT ISNULL(data) THEN
	dwItemStatus l_status
   l_status = dw_library_information.GetItemStatus(row, 0, Primary!)
   IF l_status = NewModified! THEN
		dw_def_insert.Visible = TRUE
		st_1.Visible = TRUE
	   dw_def_insert.Modify("libcd.Tabsequence = 10")
	   dw_def_insert.Event pfc_addrow()
	   dw_def_insert.setfocus()
	END IF
END IF

//IF dwo.name = "libcd" AND ISNULL(data) 
end event

event clicked;call super::clicked;//string ls_libcd
//
//ls_libcd = dw_library_information.getitemstring(dw_library_information.Getrow(),"libcd")
//
//IF ISNULL(Trim(ls_libcd)) OR Trim(ls_libcd) = "" THEN
//	cb_find.Triggerevent(Clicked!)
//END IF
end event


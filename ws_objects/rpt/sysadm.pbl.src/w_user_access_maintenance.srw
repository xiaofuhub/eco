$PBExportHeader$w_user_access_maintenance.srw
forward
global type w_user_access_maintenance from w_sheet
end type
type cb_update from u_cb within w_user_access_maintenance
end type
type cb_exit from u_cb within w_user_access_maintenance
end type
type cb_clear from u_cb within w_user_access_maintenance
end type
type dw_userid from u_dw within w_user_access_maintenance
end type
type dw_user_access_maintenance from u_dw within w_user_access_maintenance
end type
type cb_selectall from u_cb within w_user_access_maintenance
end type
type cb_deselect from u_cb within w_user_access_maintenance
end type
end forward

global type w_user_access_maintenance from w_sheet
integer x = 214
integer y = 221
integer width = 3383
integer height = 1828
string title = "User Access Maintenance"
cb_update cb_update
cb_exit cb_exit
cb_clear cb_clear
dw_userid dw_userid
dw_user_access_maintenance dw_user_access_maintenance
cb_selectall cb_selectall
cb_deselect cb_deselect
end type
global w_user_access_maintenance w_user_access_maintenance

forward prototypes
public function integer wf_check_users (string ls_userid)
public subroutine wf_add_group (str_menu menu_structure, integer array_limit, string ls_userid)
public subroutine wf_disable_keys ()
public subroutine wf_enable_keys ()
end prototypes

public function integer wf_check_users (string ls_userid);String ls_temp



  SELECT picsuser.userid  
    INTO :ls_temp  
    FROM picsuser  
   WHERE picsuser.userid = :ls_userid 
 USING SqlServerTrans;

RETURN SqlServerTrans.SqlCode;
end function

public subroutine wf_add_group (str_menu menu_structure, integer array_limit, string ls_userid);Integer li_loop



//This function puts all the items in the datawindow and sets the userid.
FOR li_loop = 1 to array_limit		
	dw_user_access_maintenance.setitem(li_loop,'menu',menu_structure.menu_array[li_loop])
	dw_user_access_maintenance.setitem(li_loop,'userid',ls_userid)
	dw_user_access_maintenance.Event pfc_addrow()
NEXT

dw_user_access_maintenance.DeleteRow(array_limit+1)





end subroutine

public subroutine wf_disable_keys ();
//Disable the keys
cb_deselect.Enabled = FALSE
cb_selectall.Enabled = FALSE
cb_update.Enabled = FALSE

//Enable the first datawindow
dw_userid.Enabled = TRUE

//Disable the second datawindow
dw_user_access_maintenance.Enabled = FALSE
end subroutine

public subroutine wf_enable_keys ();//Enable the keys and disable the first (dw_userid) datawindow
cb_deselect.Enabled = TRUE
cb_selectall.Enabled = TRUE
cb_update.Enabled = TRUE

//Disable the first datawindow and enable the second datawindow
dw_userid.Enabled = FALSE
dw_user_access_maintenance.Enabled = TRUE
end subroutine

on w_user_access_maintenance.create
int iCurrent
call super::create
this.cb_update=create cb_update
this.cb_exit=create cb_exit
this.cb_clear=create cb_clear
this.dw_userid=create dw_userid
this.dw_user_access_maintenance=create dw_user_access_maintenance
this.cb_selectall=create cb_selectall
this.cb_deselect=create cb_deselect
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.cb_update
this.Control[iCurrent+2]=this.cb_exit
this.Control[iCurrent+3]=this.cb_clear
this.Control[iCurrent+4]=this.dw_userid
this.Control[iCurrent+5]=this.dw_user_access_maintenance
this.Control[iCurrent+6]=this.cb_selectall
this.Control[iCurrent+7]=this.cb_deselect
end on

on w_user_access_maintenance.destroy
call super::destroy
destroy(this.cb_update)
destroy(this.cb_exit)
destroy(this.cb_clear)
destroy(this.dw_userid)
destroy(this.dw_user_access_maintenance)
destroy(this.cb_selectall)
destroy(this.cb_deselect)
end on

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

inv_resize.of_Register(cb_clear, "scale")
inv_resize.of_Register(cb_deselect, "scale")
inv_resize.of_Register(cb_exit, "scale")
inv_resize.of_Register(cb_selectall, "scale")
inv_resize.of_Register(cb_update, "scale")
inv_resize.of_Register(dw_user_access_maintenance, "scale")
inv_resize.of_Register(dw_userid, "scale")


end event

event open;call super::open;// Open the sheet in Maximized mode
this.windowstate = maximized!

//call wf_disable_keys to disable the keys
wf_disable_keys()

//disable addrow and deleterow 
m_pics_main.m_edit.m_addrow.Enabled = FALSE
m_pics_main.m_edit.m_deleterow.Enabled = FALSE


end event

event closequery;Integer	li_pendingrc
Integer	li_validationrc
Integer	li_accepttextrc
Integer	li_msg
Integer	li_rc, li_rtn_code
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
//		li_msg = MessageBox (gnv_app.iapp_object.DisplayName, &
//					"The information entered does not pass validation and "  + &
//					"must be corrected before changes can be saved.~r~n~r~n" + &
//					"Close without saving changes?", &
//					exclamation!, YesNo!, 2)
		li_msg = 1
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
		li_msg = 2
	End If
	Choose Case li_msg
						
		Case 1
			
			li_rtn_code = cb_update.Event clicked()
			IF li_rtn_code = 1 THEN
			  RETURN 0
		   END IF
			
			// YES - Update
			// If the update fails, prevent the window from closing
			//If This.Event pfc_save() >= 1 Then
				// Successful update, allow the window to be closed
			//	Return 0
			//End If
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

type cb_update from u_cb within w_user_access_maintenance
string tag = "Update the database"
integer x = 1504
integer y = 1468
integer taborder = 0
integer textsize = -10
string text = "&Update"
end type

event clicked;call super::clicked;Integer li_rtn_code, li_loop, li_row_count, i
String  ls_userid, ls_menu, ls_access

//Popup the update messagebox and set pointer to hour glass
OpenWithParm(w_pics_update_msg_box,"Updating PICS User Access Table, Please Wait...")
SetPointer(Hourglass!)

dw_user_access_maintenance.AcceptText()

ls_userid =dw_userid.GetItemString(1,"userid")
//Before Updating, delete the menuitems associated with the userid. This is
//done so that if the m_pics_main menu has been changed then orphan rows
//will not remain in the picsaccess table.

DELETE FROM picsaccess  
   WHERE picsaccess.userid = :ls_userid 
USING SqlServerTrans;
IF f_check_dberror(SqlServerTrans,"Picsaccess") THEN
	commit using sqlservertrans;
ELSE
  	ROLLBACK USING SqlServerTrans;
	Close(w_pics_update_msg_box)
	RETURN 1
END IF

//Next get each item from the data window and insert into the database
li_row_count = dw_user_access_maintenance.RowCount()

FOR li_loop = 1 TO li_row_count
	ls_menu = dw_user_access_maintenance.GetItemString(li_loop,"menu")
	ls_access = dw_user_access_maintenance.GetItemString(li_loop,"access_")
	if IsNull(ls_access) then
		ls_access='N'
	end if
//Update the database with the local variables 
  INSERT INTO picsaccess  
			( userid,   
			  menu,   
			  access_ )  
  VALUES ( :ls_userid,   
			  :ls_menu,   
			  :ls_access )
  USING SqlServerTrans;
	IF f_check_dberror(SqlServerTrans,"Picsaccess") THEN
		commit using sqlservertrans;
	ELSE
   	ROLLBACK USING SqlServerTrans;
		Close(w_pics_update_msg_box)
		RETURN 1
	END IF
NEXT

Close(w_pics_update_msg_box)
RETURN 1

end event

type cb_exit from u_cb within w_user_access_maintenance
string tag = "Exit the Screen"
integer x = 2341
integer y = 1468
integer taborder = 0
boolean bringtotop = true
integer textsize = -10
string text = "E&xit"
end type

event clicked;call super::clicked;ib_disableclosequery = TRUE
Parent.Event pfc_close()

end event

type cb_clear from u_cb within w_user_access_maintenance
string tag = "Clears screen for input"
integer x = 1920
integer y = 1468
integer taborder = 0
boolean bringtotop = true
integer textsize = -10
string text = "&Clear"
end type

event clicked;call super::clicked;dw_user_access_maintenance.Reset()
dw_user_access_maintenance.Event pfc_addrow()

dw_userid.Reset()
dw_userid.Event pfc_addrow()



//call disable key function to disable the keys
wf_disable_keys()

//set focus to the datawindow
dw_userid.SetFocus()
end event

type dw_userid from u_dw within w_user_access_maintenance
integer x = 27
integer y = 28
integer width = 2857
integer height = 176
integer taborder = 10
string dataobject = "d_user_access"
boolean vscrollbar = false
boolean livescroll = false
end type

event constructor;call super::constructor;SetTransObject(SqlServerTrans)
dw_userid.Event pfc_addrow()
end event

event itemchanged;call super::itemchanged;String   ls_userid, menu_text = "", ls_menu_1, ls_menu_2, ls_access
Integer  li_rtn_code, li_array_limit, li_maxrows, li_loop, li_inner_loop
str_menu menu_structure
Boolean  Found

IF DWO.Name = "userid" THEN
	ls_userid = DATA
	li_rtn_code = wf_check_users(ls_userid) //check if user id exists
	
	IF li_rtn_code = 100 THEN
		dw_userid.Object.Userid.ValidationMsg = "No User Found .. Enter New User"
		dw_userid.Event pfc_selectall()
		RETURN 1
	ELSEIF li_rtn_code = 0 THEN
		//Pop up the messagebox and set the pointer to hourglass
		open(w_pics_retrieve_msg_box)
		SetPointer(HourGlass!)
		
		dw_userid.Retrieve(ls_userid)
		uof_walk_menu_tree(menu_structure, m_pics_main, menu_text, li_array_limit)
		wf_add_group(menu_structure,li_array_limit,ls_userid)
		
		//Instantiate and populate the datastore. The datastore retrieves the data
		//from the picsaccess table that are specific to the user.
		datastore lds_datastore
		lds_datastore = CREATE datastore
		lds_datastore.DataObject = "d_user_access_datastore"
		lds_datastore.SetTransObject(SqlServerTrans)
		lds_datastore.Retrieve(ls_userid)
		
		li_maxrows = lds_datastore.rowcount()
		//
		FOR li_loop = 1 to li_maxrows
			ls_menu_1 = lds_datastore.GetItemString(li_loop,"menu")
			FOUND = FALSE
			FOR li_inner_loop = 1 to dw_user_access_maintenance.rowcount()
				ls_menu_2 = dw_user_access_maintenance.GetItemString(li_inner_loop,"menu")
				
				IF TRIM(ls_menu_1) = TRIM(ls_menu_2) THEN
					FOUND = TRUE
					ls_access = lds_datastore.GetItemString(li_loop,"access_")
					EXIT
				END IF
			NEXT//FOR li_inner_loop = 1 to dw_user_access_maintenance.rowcount()
			
			IF FOUND = TRUE AND ls_access = 'Y' THEN
			  dw_user_access_maintenance.SetItem(li_inner_loop,"access_",'Y')
			END IF//	IF FOUND = TRUE AND ls_access = 'Y' THEN
		
	    NEXT//FOR li_loop = 1 to li_maxrows
		 
		 DESTROY lds_datastore
		 dw_user_access_maintenance.SetFocus()
		 close(w_pics_retrieve_msg_box)
		 wf_enable_keys()
	END IF//	IF li_rtn_code = 100 THEN
	
END IF//IF DWO.Name = "userid" THEN
end event

type dw_user_access_maintenance from u_dw within w_user_access_maintenance
integer x = 23
integer y = 248
integer width = 2866
integer height = 1164
integer taborder = 20
string dataobject = "d_user_access_maintenance"
boolean hscrollbar = true
end type

event constructor;call super::constructor;Settransobject(SqlServerTrans)
this.Event pfc_addrow()
end event

type cb_selectall from u_cb within w_user_access_maintenance
string tag = "Selects all the menu items"
integer x = 37
integer y = 1468
integer width = 361
integer taborder = 0
integer textsize = -10
string text = "&Select All"
end type

event clicked;call super::clicked;Integer li_loop, li_maxrows

//This will enable all the items in the datawindow
li_maxrows = dw_user_access_maintenance.RowCount()
For li_loop = 1 to li_maxrows
	dw_user_access_maintenance.SetItem(li_loop,"access_",'Y')
NEXT

end event

type cb_deselect from u_cb within w_user_access_maintenance
string tag = "Deselects all the menu items"
integer x = 443
integer y = 1468
integer width = 430
integer taborder = 0
integer textsize = -10
string text = "Unselect Al&l"
end type

event clicked;call super::clicked;Integer li_loop, li_maxrows

//This will enable all the items in the datawindow
li_maxrows = dw_user_access_maintenance.RowCount()
For li_loop = 1 to li_maxrows
	dw_user_access_maintenance.SetItem(li_loop,"access_",'N')
NEXT

end event


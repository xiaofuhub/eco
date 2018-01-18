$PBExportHeader$w_group_access_maintenance.srw
forward
global type w_group_access_maintenance from w_sheet
end type
type dw_group from u_dw within w_group_access_maintenance
end type
type st_1 from u_st within w_group_access_maintenance
end type
type cb_exit from commandbutton within w_group_access_maintenance
end type
type dw_group_access_maintenance from u_dw within w_group_access_maintenance
end type
type cb_clear from commandbutton within w_group_access_maintenance
end type
type cb_update from commandbutton within w_group_access_maintenance
end type
type cb_delete from commandbutton within w_group_access_maintenance
end type
type cb_selectall from u_cb within w_group_access_maintenance
end type
type cb_deselectall from u_cb within w_group_access_maintenance
end type
end forward

global type w_group_access_maintenance from w_sheet
integer width = 3451
integer height = 1660
string title = "Group Access Maintenance"
dw_group dw_group
st_1 st_1
cb_exit cb_exit
dw_group_access_maintenance dw_group_access_maintenance
cb_clear cb_clear
cb_update cb_update
cb_delete cb_delete
cb_selectall cb_selectall
cb_deselectall cb_deselectall
end type
global w_group_access_maintenance w_group_access_maintenance

forward prototypes
public subroutine wf_disable_keys ()
public subroutine wf_enable_keys ()
public function integer wf_check_group_exists (string ls_group)
public subroutine wf_delete_users (string ls_group)
public subroutine wf_update_users (string ls_group)
public subroutine wf_add_group (str_menu menu_structure, integer array_limit, string l_groupid)
end prototypes

public subroutine wf_disable_keys ();//Disable keys
cb_delete.Enabled = FALSE
cb_deselectall.Enabled = FALSE
cb_selectall.Enabled = FALSE
cb_update.Enabled = FALSE

//Disable the datawindow
dw_group_access_maintenance.Enabled = FALSE
end subroutine

public subroutine wf_enable_keys ();//Disable keys
cb_delete.Enabled = TRUE
cb_deselectall.Enabled = TRUE
cb_selectall.Enabled = TRUE
cb_update.Enabled = TRUE

//Disable the datawindow
dw_group_access_maintenance.Enabled = TRUE
end subroutine

public function integer wf_check_group_exists (string ls_group); String ls_temp
 
 
 
 SELECT Unique picsgroup.group_  
    INTO :ls_temp  
    FROM picsgroup  
   WHERE picsgroup.group_ = :ls_group  
USING SqlServerTrans;


RETURN SqlServerTrans.Sqlcode

end function

public subroutine wf_delete_users (string ls_group);Integer li_loop


//Delete from picsaccess table
DELETE FROM picsaccess
WHERE userid in (select userid from picsuser where group_ = :ls_group)
USING sqlServerTrans;

//Delete from picsgroup table
DELETE FROM picsuser
WHERE group = :ls_group
USING SqlServerTrans;


end subroutine

public subroutine wf_update_users (string ls_group);String ls_userid[], ls_temp, ls_menu, ls_access
Integer li_row, li_max_rows, li_loop, li_inner_loop

//Declare cursor that will get all the userids that are in the group (ls_group)
DECLARE  User_cur CURSOR FOR
SELECT   userid
FROM  picsuser
WHERE group_ = :ls_group
USING SqlServerTrans;

//Open the cursor	
OPEN user_cur;

//Fetch the cursor
li_row = 1
FETCH user_cur into :ls_userid[li_row];
DO WHILE SqlServerTrans.Sqlcode = 0 
	li_row = li_row + 1
	FETCH user_cur into :ls_userid[li_row];
		
LOOP

//close cursor
CLOSE user_cur;

li_max_rows = UpperBound(ls_userid)
li_max_rows = li_max_rows - 1


IF li_max_rows > 0 THEN
	//Update all the users in the picsaccess table with the new menu options
	FOR li_loop = 1 TO li_max_rows
		//Delete the user from picsaccess table
		DELETE FROM picsaccess  
		WHERE picsaccess.userid = :ls_userid[li_loop]
		USING SqlServerTrans;
		IF f_check_dberror(SqlServerTrans,"Picsaccess") THEN
			COMMIT USING sqlservertrans;
		ELSE
   		ROLLBACK USING SqlServerTrans;
			Close(w_pics_update_msg_box)
		END IF
		
		//Insert the new menu options
		FOR li_inner_loop = 1 TO dw_group_access_maintenance.RowCount()
			ls_menu = dw_group_access_maintenance.GetItemString(li_inner_loop,"menu")
			ls_access = dw_group_access_maintenance.GetItemString(li_inner_loop,"access_")			  
				INSERT INTO picsaccess  
							( userid,   
							  menu,   
							  access_ )  
				VALUES ( :ls_userid[li_loop],   
					  :ls_menu,
					  :ls_access)
				USING SqlServerTrans;
				IF f_check_dberror(SqlServerTrans,"Picsaccess") THEN
					COMMIT USING sqlservertrans;
				ELSE
   				ROLLBACK USING SqlServerTrans;
					Close(w_pics_update_msg_box)
				END IF	
		NEXT //End of inner loop	
	NEXT //End of outer loop
END IF //IF li_max_rows > 0 THEN
end subroutine

public subroutine wf_add_group (str_menu menu_structure, integer array_limit, string l_groupid);Integer li_loop



//This function puts all the items in the datawindow and sets the group.
FOR li_loop = 1 to array_limit		
	dw_group_access_maintenance.setitem(li_loop,'menu',menu_structure.menu_array[li_loop])
	dw_group_access_maintenance.setitem(li_loop,'group_',l_groupid)
	dw_group_access_maintenance.Event pfc_addrow()
NEXT

dw_group_access_maintenance.DeleteRow(array_limit+1)




end subroutine

on w_group_access_maintenance.create
int iCurrent
call super::create
this.dw_group=create dw_group
this.st_1=create st_1
this.cb_exit=create cb_exit
this.dw_group_access_maintenance=create dw_group_access_maintenance
this.cb_clear=create cb_clear
this.cb_update=create cb_update
this.cb_delete=create cb_delete
this.cb_selectall=create cb_selectall
this.cb_deselectall=create cb_deselectall
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_group
this.Control[iCurrent+2]=this.st_1
this.Control[iCurrent+3]=this.cb_exit
this.Control[iCurrent+4]=this.dw_group_access_maintenance
this.Control[iCurrent+5]=this.cb_clear
this.Control[iCurrent+6]=this.cb_update
this.Control[iCurrent+7]=this.cb_delete
this.Control[iCurrent+8]=this.cb_selectall
this.Control[iCurrent+9]=this.cb_deselectall
end on

on w_group_access_maintenance.destroy
call super::destroy
destroy(this.dw_group)
destroy(this.st_1)
destroy(this.cb_exit)
destroy(this.dw_group_access_maintenance)
destroy(this.cb_clear)
destroy(this.cb_update)
destroy(this.cb_delete)
destroy(this.cb_selectall)
destroy(this.cb_deselectall)
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
inv_resize.of_Register(cb_delete, "scale")
inv_resize.of_Register(cb_exit, "scale")
inv_resize.of_Register(cb_update, "scale")
inv_resize.of_Register(cb_deselectall, "scale")
inv_resize.of_Register(cb_selectall, "scale")
inv_resize.of_Register(dw_group, "scale")
inv_resize.of_Register(dw_group_access_maintenance, "scale")



inv_resize.of_Register(st_1, "scale")

end event

event open;call super::open;// Open the sheet in Maximized mode
this.windowstate = maximized!

//disable the addrow and deleterow menu items
m_pics_main.m_edit.m_addrow.Enabled = FALSE
m_pics_main.m_edit.m_deleterow.Enabled = FALSE

//Disable the keys
wf_disable_keys()
end event

event closequery;//////////////////////////////////////////////////////////////////////////////
//
//	Event:  closequery
//
//	Description:
//	Search for unsaved datawindows prompting the user if any
//	pending updates are found.
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
//	Copyright © 1996-1997 Sybase, Inc. and its subsidiaries.  All rights reserved.
//	Any distribution of the PowerBuilder Foundation Classes (PFC)
//	source code by other than Sybase, Inc. and its subsidiaries is prohibited.
//
//////////////////////////////////////////////////////////////////////////////

Integer	li_pendingrc
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
			li_rtn_code = cb_update.Event clicked()
			IF li_rtn_code = 1 THEN
				RETURN 0
			END IF
			
					
//			// YES - Update
//			// If the update fails, prevent the window from closing
//			If This.Event pfc_save() >= 1 Then
//				// Successful update, allow the window to be closed
//				Return 0
//			End If
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

type dw_group from u_dw within w_group_access_maintenance
integer x = 1170
integer y = 32
integer width = 366
integer height = 128
integer taborder = 10
string dataobject = "d_groupids"
boolean vscrollbar = false
boolean livescroll = false
end type

event constructor;call super::constructor;Datawindowchild	ldwc_groupid


this.SetTransObject(SqlServerTrans)
this.Event pfc_addrow()

this.GetChild("group_",ldwc_groupid)
this.Event pfc_PopulateDDDW("group_", ldwc_groupid)

this.of_SetDropDownSearch(TRUE)
this.inv_dropdownsearch.of_AddColumn("group_")

end event

event itemchanged;call super::itemchanged;str_menu menu_structure
String ls_group, menu_text = "", ls_menu_1,ls_menu_2, ls_access, mod_string
Integer li_rtn_code, li_array_limit, li_max_rows, li_loop, li_inner_loop
Boolean Found
Long    l_pos

IF DWO.Name = 'group_' THEN
	ls_group = TRIM(Data)
	li_rtn_code = wf_check_group_exists(ls_group)
	//If group does not exist then proceed in adding the group to the database.
	IF li_rtn_code = 100 THEN
		MessageBox('Status',"Group Does Not Exist Proceed With Enabling Menu Items")
		//Walk the menu tree and add all the menu item to menu_structure.
		   uof_walk_menu_tree(menu_structure, m_pics_main, menu_text, li_array_limit)
		   wf_add_group(menu_structure, li_array_limit, ls_group)
		   dw_group_access_maintenance.SetFocus()
			
   ELSEIF li_rtn_code = 0 THEN
		
		MessageBox('Status',"Group Exists, Proceed with Changing Access")
		//popup the retrieve messageBox and set the pointer to hour glass
		  Open(w_pics_retrieve_msg_box)
		  SetPointer(Hourglass!)
		uof_walk_menu_tree(menu_structure, m_pics_main, menu_text, li_array_limit)
		wf_add_group(menu_structure, li_array_limit, ls_group)
		
		//Instantiate and populate the data store.The datastore retrieves the menu items
		//that have been enabled and disabled for the specific user in the database
		//table picsgroup.
		   datastore lds_datastore
			lds_datastore = CREATE datastore
			lds_datastore.DataObject = "d_group_access_datastore"
			lds_datastore.SetTransObject(SqlServerTrans)
			lds_datastore.Retrieve(ls_group)
			
			
			li_max_rows = lds_datastore.rowcount()
		   //The following loop checks each item from the datastore with
			//the item in the dw_group_access_maintenance and enables the menu items.
		   FOR li_loop  = 1 to  li_max_rows
			   ls_menu_1 = lds_datastore.GetItemstring(li_loop,"menu")
			  	FOUND = FALSE
			  FOR li_inner_loop = 1 to  dw_group_access_maintenance.rowcount()
				ls_menu_2 =  dw_group_access_maintenance.GetitemString(li_inner_loop,"menu") 
				
				IF TRIM(ls_menu_1) = TRIM(ls_menu_2) THEN
               FOUND = TRUE
					ls_access = lds_datastore.GetItemstring(li_loop,"access_")
					EXIT
			   END IF
			 NEXT//FOR li_inner_loop = 1 to dw_group_access_maintenance.rowcount()
			  IF FOUND = TRUE AND ls_access = 'Y' THEN
				dw_group_access_maintenance.SetItem(li_inner_loop,"access_",'Y')
			  END IF//  IF FOUND = TRUE THEN
			  
			NEXT//FOR li_loop  = 1 to li_max_rows
					
		   DESTROY lds_datastore
			Close(w_pics_retrieve_msg_box)
		   dw_group_access_maintenance.SetFocus()
	END IF//	IF li_rtn_code = 100 THEN
END IF//IF DWO.Name = 'group' THEN


//Change the colors
li_max_rows = dw_group_access_maintenance.Rowcount()

FOR li_loop = 2 TO li_max_rows
	ls_menu_1 = dw_group_access_maintenance.GetItemString(li_loop - 1,"menu")
	ls_menu_2 = dw_group_access_maintenance.GetItemString(li_loop,"menu")
	
	ls_menu_1 = MID(ls_menu_1,2)
	l_pos = POS(ls_menu_1,".")
	IF l_pos <> 0 THEN ls_menu_1 = MID(ls_menu_1,1,l_pos - 1)
	
	ls_menu_2 = MID(ls_menu_2,2)
	l_pos = POS(ls_menu_2,".")
	IF l_pos <> 0 THEN ls_menu_2 = MID(ls_menu_2,1,l_pos - 1)
	
	IF ls_menu_1 = ls_menu_2 THEN
		mod_string = "dw_group_access_maintenance.Object.Menu[1].color = '255'"
	ELSE 
		mod_string = "dw_group_access_maintenance.Object.Menu[1].color = '65280'"
		
   END IF
		
	dw_group_access_maintenance.Modify(mod_string)
NEXT

wf_enable_keys()



end event

type st_1 from u_st within w_group_access_maintenance
integer x = 946
integer y = 64
integer width = 242
integer textsize = -10
string text = "Groups"
alignment alignment = right!
end type

type cb_exit from commandbutton within w_group_access_maintenance
integer x = 2592
integer y = 1404
integer width = 352
integer height = 108
boolean bringtotop = true
integer textsize = -10
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "E&xit"
end type

event clicked;dw_group.Reset()
dw_group_access_maintenance.Reset()
close(w_group_access_maintenance)
end event

type dw_group_access_maintenance from u_dw within w_group_access_maintenance
integer y = 180
integer width = 2789
integer height = 1160
integer taborder = 20
string dataobject = "d_group_access_maintenance"
boolean hscrollbar = true
end type

event constructor;call super::constructor;settransobject(SqlServerTrans)
dw_group_access_maintenance.Event pfc_addrow()
end event

type cb_clear from commandbutton within w_group_access_maintenance
integer x = 2194
integer y = 1404
integer width = 352
integer height = 108
boolean bringtotop = true
integer textsize = -10
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "&Clear"
end type

event clicked;//Reset the datawindows and add rows
dw_group.Reset()
dw_group_access_maintenance.Reset()

dw_group.Event pfc_addrow()
dw_group_access_maintenance.Event pfc_addrow()

//Disable the buttons
wf_disable_keys()

//Set focus to the datawindow
dw_group.SetFocus()
end event

type cb_update from commandbutton within w_group_access_maintenance
integer x = 1390
integer y = 1404
integer width = 352
integer height = 108
boolean bringtotop = true
integer textsize = -10
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "&Update"
end type

event clicked;Integer li_rtn_code,i, k, li_cnt
String  ls_group, ls_access, ls_menu

dw_group_access_maintenance.AcceptText()
//Open the pop up message box
SetPointer(Hourglass!)
OpenWithParm(w_pics_update_msg_box,"Updating PICS Group Access Table, Please Wait...")

ls_group = dw_group.object.group_[1]
//Before Updating delete the menuitems associated with that group. This is
//done so that if the m_pics_main menu has been changed then orphan rows
//will not remain in the picsgroup table.
IF wf_check_group_exists(ls_group) = 0 THEN
	DELETE FROM picsgroup  
   WHERE picsgroup.group_ = :ls_group 
	USING SqlServerTrans;
//	IF f_check_dberror(SqlServerTrans,"Picsgroup") THEN
//		COMMIT USING sqlservertrans;
//	ELSE
//   	ROLLBACK USING SqlServerTrans;
//		Close(w_pics_update_msg_box)
//		RETURN 1
//	END IF
END IF
li_cnt=dw_group_access_maintenance.RowCount()
FOR i = 1 TO li_cnt
	ls_access=dw_group_access_maintenance.object.access_[i]
	ls_menu=dw_group_access_maintenance.object.menu[i]
	if ls_menu = ".File.Close" then
		k=10
	end if	
	IF IsNull(dw_group_access_maintenance.object.access_[i] ) THEN
		dw_group_access_maintenance.object.access_[i]='N'
	ENd IF
NEXT

li_rtn_code = dw_group_access_maintenance.Update()

IF li_rtn_code = 1 THEN
	dw_group_access_maintenance.ResetUpdate()
	COMMIT USING SqlServerTrans;

	//Update all users in this group with the new menu accesses
	wf_update_users(ls_group)
	
	//Refresh the dropdown datawindow (dw_group) with the new group
	DataWindowChild ldwc_pub
	w_group_access_maintenance.dw_group.GetChild ("group_", ldwc_pub)
	ldwc_pub.SetTransObject(sqlservertrans)
	ldwc_pub.Retrieve()
	cb_clear.Event clicked()
	
	//Close messageBox and return 1 as successfull update
	Close(w_pics_update_msg_box)
	RETURN 1
	
ELSE //Display update failed messagebox and return 2
	MessageBox('Status','Update Table picsgroup Failed .. Contact Your DBA')
   ROLLBACK USING SqlServerTrans;
	Close(w_pics_update_msg_box)
	RETURN 2
END IF








end event

type cb_delete from commandbutton within w_group_access_maintenance
integer x = 1787
integer y = 1404
integer width = 370
integer height = 108
boolean bringtotop = true
integer textsize = -10
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "&Delete "
end type

event clicked;String ls_group
Integer li_return_group, li_return_user

ls_group = dw_group.GetItemString(1,"group_")

li_return_group = MessageBox('Delete','This will delete the group As well as all users of the group from the Database .. Do you Want to Continue ?',Question!,YesNo!)
IF li_return_group = 1 THEN
   //Open the message box
	Open(w_pics_update_msg_box)
	SetPointer(HourGlass!)
	
		  DELETE FROM picsgroup  
			WHERE picsgroup.group_ = :ls_group
		  USING SqlServerTrans;
		  
		  IF SqlServerTrans.SqlCode = 0 THEN
			wf_delete_users(ls_group)
			
			Close(w_pics_update_msg_box)
			MessageBox('Status','Group Successfully deleted')
			//Refresh the dropdown datawindow (dw_group) with the new group
			DataWindowChild ldwc_pub
			w_group_access_maintenance.dw_group.GetChild ("group_", ldwc_pub)
			ldwc_pub.SetTransObject(sqlservertrans)
			ldwc_pub.Retrieve()
		  ELSE
			MessageBox('Status','Error On Deletion .. Contact Your DBA')
		  END IF
ELSEIF li_return_group = 2 THEN
	RETURN
END IF
cb_clear.TriggerEvent(Clicked!)
	
end event

type cb_selectall from u_cb within w_group_access_maintenance
string tag = "Selects all the menu items"
integer x = 46
integer y = 1404
integer width = 361
integer taborder = 0
integer textsize = -10
string text = "&Select All"
end type

event clicked;call super::clicked;Integer li_loop, li_maxrows

//This will enable all the items in the datawindow
li_maxrows = dw_group_access_maintenance.RowCount()
For li_loop = 1 to li_maxrows
	dw_group_access_maintenance.SetItem(li_loop,"access_",'Y')
NEXT

end event

type cb_deselectall from u_cb within w_group_access_maintenance
integer x = 466
integer y = 1404
integer width = 430
integer taborder = 0
integer textsize = -10
string text = "Unselect Al&l"
end type

event clicked;call super::clicked;Integer li_loop, li_maxrows

//This will enable all the items in the datawindow
li_maxrows = dw_group_access_maintenance.RowCount()
For li_loop = 1 to li_maxrows
	dw_group_access_maintenance.SetItem(li_loop,"access_",'N')
NEXT

end event


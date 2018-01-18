$PBExportHeader$w_magazine_producer_changes.srw
forward
global type w_magazine_producer_changes from w_sheet
end type
type dw_magazine_producer_changes_main from u_dw within w_magazine_producer_changes
end type
type dw_magazine_producer_changes from u_dw within w_magazine_producer_changes
end type
type st_1 from statictext within w_magazine_producer_changes
end type
type st_2 from statictext within w_magazine_producer_changes
end type
type sle_title from u_sle within w_magazine_producer_changes
end type
type sle_new_contract from u_sle within w_magazine_producer_changes
end type
type st_3 from statictext within w_magazine_producer_changes
end type
type st_4 from statictext within w_magazine_producer_changes
end type
type st_5 from statictext within w_magazine_producer_changes
end type
type st_6 from statictext within w_magazine_producer_changes
end type
type em_remaining_issues from u_em within w_magazine_producer_changes
end type
type cb_exit from u_cb within w_magazine_producer_changes
end type
type cb_update from u_cb within w_magazine_producer_changes
end type
type cb_clear from u_cb within w_magazine_producer_changes
end type
type em_fy from u_em within w_magazine_producer_changes
end type
type sle_new_producer from u_sle within w_magazine_producer_changes
end type
type dw_magazine_producer_changes_main_rc from u_dw within w_magazine_producer_changes
end type
end forward

global type w_magazine_producer_changes from w_sheet
integer x = 110
integer y = 84
integer width = 2839
integer height = 1960
string title = "Magazine Producer Changes"
dw_magazine_producer_changes_main dw_magazine_producer_changes_main
dw_magazine_producer_changes dw_magazine_producer_changes
st_1 st_1
st_2 st_2
sle_title sle_title
sle_new_contract sle_new_contract
st_3 st_3
st_4 st_4
st_5 st_5
st_6 st_6
em_remaining_issues em_remaining_issues
cb_exit cb_exit
cb_update cb_update
cb_clear cb_clear
em_fy em_fy
sle_new_producer sle_new_producer
dw_magazine_producer_changes_main_rc dw_magazine_producer_changes_main_rc
end type
global w_magazine_producer_changes w_magazine_producer_changes

forward prototypes
public function integer wf_check_mag_code (string ls_magcode, ref string ls_title)
public function integer wf_contract_exists (integer li_fy, string ls_contract)
public subroutine wf_disable ()
public subroutine wf_enable ()
public function string wf_get_format (string ls_magcode, integer li_fy)
end prototypes

public function integer wf_check_mag_code (string ls_magcode, ref string ls_title);
 
  
  SELECT magttl.title
    INTO :ls_title  
    FROM magttl   
   WHERE magttl.magcd  = :ls_magcode
  	USING SqlServerTrans;



RETURN SqlServerTrans.Sqlcode;
end function

public function integer wf_contract_exists (integer li_fy, string ls_contract);String ls_temp


SELECT magcntr.cntr
INTO   :ls_temp
FROM   magcntr
WHERE  magcntr.fy = :li_fy AND
		 magcntr.cntr = :ls_contract
USING  SqlServerTrans;


RETURN SqlServerTrans.SqlCode

end function

public subroutine wf_disable ();
em_fy.DisplayOnly = TRUE
sle_title.DisplayOnly = TRUE
dw_magazine_producer_changes.Enabled = FALSE
sle_new_contract.DisplayOnly = TRUE
sle_new_producer.DisplayOnly = TRUE
sle_new_producer.DisplayOnly = TRUE

end subroutine

public subroutine wf_enable ();
em_fy.DisplayOnly = FALSE
sle_title.DisplayOnly = FALSE
dw_magazine_producer_changes.Enabled = TRUE
sle_new_contract.DisplayOnly = FALSE
sle_new_producer.DisplayOnly = FALSE
sle_new_producer.DisplayOnly = FALSE

end subroutine

public function string wf_get_format (string ls_magcode, integer li_fy);//This function will return the format. It is called in the item changed
//event of dw_magazine_producer_changes to check for the format. If the
//format is RC then	dw_magazine_producer_changes_main_rc is made visible.


String ls_format

SELECT magcntr.format
INTO   :ls_format
FROM   mag, magcntr
WHERE  mag.fy = :li_fy  AND
       mag.magcd = :ls_magcode AND
		 mag.fy = magcntr.fy AND
		 mag.cntr = magcntr.cntr
USING  SqlServerTrans;


RETURN ls_format







end function

on w_magazine_producer_changes.create
int iCurrent
call super::create
this.dw_magazine_producer_changes_main=create dw_magazine_producer_changes_main
this.dw_magazine_producer_changes=create dw_magazine_producer_changes
this.st_1=create st_1
this.st_2=create st_2
this.sle_title=create sle_title
this.sle_new_contract=create sle_new_contract
this.st_3=create st_3
this.st_4=create st_4
this.st_5=create st_5
this.st_6=create st_6
this.em_remaining_issues=create em_remaining_issues
this.cb_exit=create cb_exit
this.cb_update=create cb_update
this.cb_clear=create cb_clear
this.em_fy=create em_fy
this.sle_new_producer=create sle_new_producer
this.dw_magazine_producer_changes_main_rc=create dw_magazine_producer_changes_main_rc
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_magazine_producer_changes_main
this.Control[iCurrent+2]=this.dw_magazine_producer_changes
this.Control[iCurrent+3]=this.st_1
this.Control[iCurrent+4]=this.st_2
this.Control[iCurrent+5]=this.sle_title
this.Control[iCurrent+6]=this.sle_new_contract
this.Control[iCurrent+7]=this.st_3
this.Control[iCurrent+8]=this.st_4
this.Control[iCurrent+9]=this.st_5
this.Control[iCurrent+10]=this.st_6
this.Control[iCurrent+11]=this.em_remaining_issues
this.Control[iCurrent+12]=this.cb_exit
this.Control[iCurrent+13]=this.cb_update
this.Control[iCurrent+14]=this.cb_clear
this.Control[iCurrent+15]=this.em_fy
this.Control[iCurrent+16]=this.sle_new_producer
this.Control[iCurrent+17]=this.dw_magazine_producer_changes_main_rc
end on

on w_magazine_producer_changes.destroy
call super::destroy
destroy(this.dw_magazine_producer_changes_main)
destroy(this.dw_magazine_producer_changes)
destroy(this.st_1)
destroy(this.st_2)
destroy(this.sle_title)
destroy(this.sle_new_contract)
destroy(this.st_3)
destroy(this.st_4)
destroy(this.st_5)
destroy(this.st_6)
destroy(this.em_remaining_issues)
destroy(this.cb_exit)
destroy(this.cb_update)
destroy(this.cb_clear)
destroy(this.em_fy)
destroy(this.sle_new_producer)
destroy(this.dw_magazine_producer_changes_main_rc)
end on

event pfc_preopen;call super::pfc_preopen;
this.of_SetPreference(TRUE)
this.inv_preference.of_SetToolBars(TRUE)

this.inv_preference.of_SetWindow(TRUE)

this.of_SetResize(TRUE)
this.inv_resize.of_setOrigSize(this.WorkSpaceWidth(), this.WorkSpaceHeight())


inv_resize.of_Register(cb_clear,"scale")
inv_resize.of_Register(cb_exit,"scale")
inv_resize.of_Register(cb_update,"scale")

inv_resize.of_Register(dw_magazine_producer_changes,"scale")
inv_resize.of_Register(dw_magazine_producer_changes_main,"scale")
inv_resize.of_Register(dw_magazine_producer_changes_main_rc,"scale")
inv_resize.of_Register(em_fy,"scale")
inv_resize.of_Register(em_remaining_issues,"scale")
inv_resize.of_Register(sle_new_contract,"scale")
inv_resize.of_Register(sle_new_producer,"scale")
inv_resize.of_Register(sle_title,"scale")

inv_resize.of_Register(st_1,"scale")
inv_resize.of_Register(st_2,"scale")
inv_resize.of_Register(st_3,"scale")
inv_resize.of_Register(st_4,"scale")
inv_resize.of_Register(st_5,"scale")
inv_resize.of_Register(st_6,"scale")

end event

event resize;call super::resize;Long ll_height

this.X = w_pics_main.X
this.Y = w_pics_main.Y

ll_height = w_pics_main.mdi_1.Height
this.Resize(w_pics_main.WorkSpaceWidth(), ll_height)

end event

event open;call super::open;//Enable and Disable Objects
cb_clear.Enabled = FALSE
cb_update.Enabled = FALSE
dw_magazine_producer_changes_main.Enabled = FALSE
dw_magazine_producer_changes_main_rc.Enabled = FALSE


em_fy.Event pfc_selectall()
em_fy.SetFocus()

//open window in maximized state
this.windowstate = maximized!
end event

event mousemove;call super::mousemove;w_pics_main.setmicrohelp('Ready')
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
			IF li_rtn_code = 0 THEN
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

type dw_magazine_producer_changes_main from u_dw within w_magazine_producer_changes
event ue_enter_to_tab pbm_dwnprocessenter
event ue_hint_text pbm_mousemove
integer x = 18
integer y = 452
integer width = 2734
integer height = 1180
integer taborder = 60
string dataobject = "d_magazine_producer_changes_main"
boolean vscrollbar = false
boolean livescroll = false
end type

event ue_enter_to_tab;call super::ue_enter_to_tab;Send(Handle(this), 256,9,Long(0,0))
end event

event ue_hint_text;call super::ue_hint_text;string ls_object, ls_column, ls_column_tag
long ll_pos

//This script set's microhelp at the bottom of the screen for the Archive Title Datawindow
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

event constructor;call super::constructor;dw_magazine_producer_changes_main.of_SetTransobject(SqlServerTrans)
dw_magazine_producer_changes_main.Event pfc_addrow()
end event

event getfocus;call super::getfocus;
//IF TRIM(em_remaining_issues.text) = '' THEN
//	MessageBox('Error','Need Remaining Issues To Process Information')
//   em_remaining_issues.SetFocus()
//	RETURN
//END IF
//
//wf_disable()


end event

event updateend;call super::updateend;Long ll_rowsinserted, ll_rowsupdated, ll_total

ll_total = rowsinserted + rowsupdated
IF ll_total > 0 THEN
  MessageBox('Update','Row(s) Successfully Updated')
END IF


end event

type dw_magazine_producer_changes from u_dw within w_magazine_producer_changes
event ue_hint_text pbm_mousemove
string tag = "Enter the Magazine Code"
integer x = 425
integer y = 132
integer width = 421
integer height = 144
integer taborder = 20
string dataobject = "d_magazine_producer_changes"
boolean vscrollbar = false
boolean livescroll = false
end type

event ue_hint_text;call super::ue_hint_text;w_pics_main.setmicrohelp(this.tag)
end event

event itemchanged;call super::itemchanged;String  ls_magcode,ls_title, ls_temp, ls_format
Integer li_fy, li_rtn_code, li_ret_rows



IF DWO.Name = "magcd" THEN
	ls_magcode = TRIM(dw_magazine_producer_changes.GetText())
	li_fy = Integer(em_fy.text)
  IF ls_magcode = '' THEN
	   dw_magazine_producer_changes.SetFocus()
		
   ELSE
	    li_rtn_code =  wf_check_mag_code(ls_magcode,ls_title)
	    IF li_rtn_code = 100 THEN
		    RETURN 1
		 END IF
		 
		 sle_title.text = ls_title
		  IF TRIM(wf_get_format(ls_magcode, li_fy)) = 'RC' THEN
				dw_magazine_producer_changes_main_rc.Visible = TRUE
				dw_magazine_producer_changes_main.Visible = FALSE
			   li_ret_rows = dw_magazine_producer_changes_main_rc.Retrieve(li_fy, ls_magcode)
            IF li_ret_rows > 0 THEN
					 sle_new_contract.SetFocus()
				ELSE
						MessageBox('Error', 'No Contract Data Exists..Enter New Magazine Code')
						dw_magazine_producer_changes.Reset()
						dw_magazine_producer_changes.Event pfc_addrow()
						dw_magazine_producer_changes_main_rc.Event pfc_addrow()
						dw_magazine_producer_changes.SetFocus()
				END IF
			
			
			
   		ELSE//If format is not RC.
					 li_ret_rows = dw_magazine_producer_changes_main.Retrieve(li_fy, ls_magcode)
					 
					 IF li_ret_rows > 0 THEN
						 ls_format = dw_magazine_producer_changes_main.GetItemString(1,"magcntr_format")
						 IF TRIM(ls_format) <> 'FD' THEN
							dw_magazine_producer_changes_main.Object.mag_estmin.protect = 1
						 ELSE
							dw_magazine_producer_changes_main.Object.mag_estmin.protect = 0
						 END IF//IF TRIM(ls_format) <> 'FD' THEN
						 sle_new_contract.SetFocus()
						 //Set the ucmast, ucdup, ucothr, estmail, cntcso to zeros.
						  dw_magazine_producer_changes_main.Object.mag_ucmast[row] = 0.0
						  dw_magazine_producer_changes_main.Object.mag_ucdupl[row] = 0.0
						  dw_magazine_producer_changes_main.Object.mag_ucothr[row] = 0.0
						  dw_magazine_producer_changes_main.Object.mag_estmail[row] = 0.0
						  dw_magazine_producer_changes_main.Object.mag_cntcso[row] = 0.0
						 
						 
					ELSE
						MessageBox('Error', 'No Contract Data Exists..Enter New Magazine Code')
						dw_magazine_producer_changes.Reset()
						dw_magazine_producer_changes.Event pfc_addrow()
						dw_magazine_producer_changes_main.Event pfc_addrow()
						dw_magazine_producer_changes.SetFocus()
						
					END IF// IF li_ret_rows > 0 THEN
			 
			 
			 
		END IF//IF wf_get_format(ls_magcode, li_fy) = 'RC' THEN	
	END IF//IF ls_magcode = '' THEN
 END IF//IF DWO.Name = "magcd" THEN
			
	
end event

event constructor;call super::constructor;dw_magazine_producer_changes.of_settransobject(SqlServerTrans)
dw_magazine_producer_changes.Event pfc_addrow()
end event

event getfocus;call super::getfocus;
IF em_fy.text = '0000' THEN
	MessageBox('Error','Need Fiscal Year To Process')
   em_fy.SetFocus()
END IF
end event

event itemfocuschanged;call super::itemfocuschanged;
IF DWO.Name = "magcd" THEN
 w_pics_main.Event pfc_microhelp("Enter Magazine Code")
 dw_magazine_producer_changes.Event pfc_selectall()
END IF
end event

type st_1 from statictext within w_magazine_producer_changes
integer x = 146
integer y = 44
integer width = 256
integer height = 76
boolean bringtotop = true
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long backcolor = 67108864
boolean enabled = false
string text = "Fiscal Year"
alignment alignment = right!
boolean focusrectangle = false
end type

type st_2 from statictext within w_magazine_producer_changes
integer x = 37
integer y = 156
integer width = 361
integer height = 76
boolean bringtotop = true
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long backcolor = 67108864
boolean enabled = false
string text = "Magazine Code"
alignment alignment = right!
boolean focusrectangle = false
end type

type sle_title from u_sle within w_magazine_producer_changes
integer x = 425
integer y = 288
integer width = 896
integer height = 96
integer taborder = 0
end type

type sle_new_contract from u_sle within w_magazine_producer_changes
event ue_key pbm_keydown
event ue_hint_text pbm_mousemove
string tag = "Enter new contract "
integer x = 1943
integer y = 32
integer width = 384
integer height = 96
integer taborder = 30
boolean bringtotop = true
textcase textcase = upper!
end type

event ue_hint_text;call super::ue_hint_text;w_pics_main.setmicrohelp(this.tag)
end event

event modified;call super::modified;String  ls_temp,ls_contract,ls_magcode
Integer li_fy

//Checks to see if the contract exists for the given magazine code and fiscal year.
//If it does then output error message else set focus to sle_new_producer.

	ls_contract = TRIM(sle_new_contract.text)
	li_fy = Integer(em_fy.text)
	ls_magcode = TRIM(dw_magazine_producer_changes.GetItemString(1,'magcd'))
	
	
	SELECT mag.magcd
	INTO   :ls_temp
	FROM   mag
	WHERE  (mag.fy = :li_fy) AND
	       (mag.cntr = :ls_contract) AND
			 (mag.magcd = :ls_magcode)
	USING SqlServerTrans;
	
	
	IF SqlServerTrans.SqlCode = 0 THEN	
	   MessageBox('Error', 'Magazine Contract Record Exists. Enter New Contract')
		sle_new_contract.text = ''
		sle_new_contract.SetFocus()
	ELSEIF SqlServerTrans.SqlCode = 100 THEN
		sle_new_producer.SetFocus()
	END IF//IF SqlServerTrans.SqlCode = 0 THEN	

end event

event getfocus;call super::getfocus;w_pics_main.Event pfc_microhelp("Enter Contract Number")
end event

type st_3 from statictext within w_magazine_producer_changes
integer x = 82
integer y = 296
integer width = 320
integer height = 76
boolean bringtotop = true
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long backcolor = 67108864
boolean enabled = false
string text = "Magazine Title"
alignment alignment = right!
boolean focusrectangle = false
end type

type st_4 from statictext within w_magazine_producer_changes
integer x = 1582
integer y = 52
integer width = 338
integer height = 76
boolean bringtotop = true
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long backcolor = 67108864
boolean enabled = false
string text = "New Contract"
alignment alignment = right!
boolean focusrectangle = false
end type

type st_5 from statictext within w_magazine_producer_changes
integer x = 1577
integer y = 164
integer width = 343
integer height = 76
boolean bringtotop = true
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long backcolor = 67108864
boolean enabled = false
string text = "New Producer"
alignment alignment = right!
boolean focusrectangle = false
end type

type st_6 from statictext within w_magazine_producer_changes
integer x = 1527
integer y = 268
integer width = 393
integer height = 68
boolean bringtotop = true
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long backcolor = 67108864
boolean enabled = false
string text = "Remaining Issues"
alignment alignment = right!
boolean focusrectangle = false
end type

type em_remaining_issues from u_em within w_magazine_producer_changes
event ue_key pbm_keydown
event ue_hint_text pbm_mousemove
string tag = "Enter remaining issues"
integer x = 1943
integer y = 256
integer width = 384
integer height = 96
integer taborder = 50
string mask = "####"
string displaydata = ""
double increment = 0
string minmax = ""
end type

event ue_key;call super::ue_key;IF Keydown(KeyEnter!) THEN
	dw_magazine_producer_changes_main.SetFocus()
END IF
end event

event ue_hint_text;call super::ue_hint_text;w_pics_main.setmicrohelp(this.tag)
end event

event getfocus;call super::getfocus;String ls_producer, ls_temp

ls_producer = TRIM(sle_new_producer.text)
IF TRIM(sle_new_producer.text)= '' THEN
		MessageBox('Error','New Producer Needed To Process')
		sle_new_producer.text = ''
		sle_new_producer.SetFocus()
		RETURN
END IF//IF TRIM(sle_new_producer.text)= '' THEN  	

//Check to see if producer in producer table
SELECT prdr
INTO   :ls_temp
FROM   Producer
WHERE  prdr = :ls_producer
USING  SqlServerTrans;

//IF producer is not in producer table it needs to be added.
IF SqlServerTrans.SqlCode = 100 THEN
	MessageBox('Error','The producer '+ ls_producer +' is not in the Producer Table. The producer needs to be added')
	sle_new_producer.text = ''
	sle_new_producer.SetFocus()
	RETURN
END IF//IF SqlServerTrans.SqlCode = 100 THEN
end event

event modified;call super::modified;//If Remaining Issues is not input then prompt
IF TRIM(em_remaining_issues.text) = '' THEN
	MessageBox('Error','Need Remaining Issues To Process Information')
	em_remaining_issues.SetFocus()
	RETURN
END IF


//Enable the datawindow and the update button if input

IF dw_magazine_producer_changes_main.Visible = TRUE THEN
	dw_magazine_producer_changes_main.Enabled = TRUE
	dw_magazine_producer_changes_main.SetFocus()
	wf_disable()
	cb_update.Enabled = TRUE
ELSEIF dw_magazine_producer_changes_main_rc.Visible = TRUE THEN
	dw_magazine_producer_changes_main_rc.Enabled = TRUE
	dw_magazine_producer_changes_main_rc.SetFocus()
	wf_disable()
	cb_update.Enabled = TRUE
END IF
end event

type cb_exit from u_cb within w_magazine_producer_changes
event ue_hint_text pbm_mousemove
string tag = "Exits the screen"
integer x = 2414
integer y = 1696
integer height = 104
integer taborder = 0
integer textsize = -10
string text = "E&xit"
boolean cancel = true
end type

event ue_hint_text;call super::ue_hint_text;w_pics_main.setmicrohelp(this.tag)
end event

event clicked;call super::clicked;String ls_contract 
Integer li_rtn_code


Parent.Event pfc_close()



//ls_contract = dw_magazine_producer_changes_main.GetItemString(1,"magcntr_cntr")
//
//
//IF dw_magazine_producer_changes_main.Enabled = FALSE THEN
// dw_magazine_producer_changes.Reset()
// dw_magazine_producer_changes_main.Reset()
// close(w_magazine_producer_changes)
//ELSE
//	li_rtn_code = MessageBox('EXIT','Do You Want To Save Changes',Question!,YESNO!)
//	IF li_rtn_code = 1 THEN
//	   cb_update.TriggerEvent(clicked!)
//		dw_magazine_producer_changes.Reset()
//      dw_magazine_producer_changes_main.Reset()
//      close(w_magazine_producer_changes)
//	ELSEIF li_rtn_code = 2 THEN
//		dw_magazine_producer_changes.Reset()
//      dw_magazine_producer_changes_main.Reset()
//      close(w_magazine_producer_changes)
//	END IF//IF li_rtn_code = 1 THEN
//END IF
end event

type cb_update from u_cb within w_magazine_producer_changes
event ue_hint_text pbm_mousemove
string tag = "Updates the database"
integer x = 1573
integer y = 1696
integer height = 104
integer taborder = 0
boolean bringtotop = true
integer textsize = -10
string text = "&Update"
end type

event ue_hint_text;call super::ue_hint_text;w_pics_main.setmicrohelp(this.tag)
end event

event clicked;call super::clicked;Integer li_estiss, li_remaining_issues, li_temp, li_fy, li_rc, li_rtn_code
Real lr_cntcso, lr_estmail
String ls_cntr, ls_magcode, ls_producer, ls_format





IF dw_magazine_producer_changes_main_rc.visible = TRUE THEN
	dw_magazine_producer_changes_main.AcceptText()
	//Place datawindow variables into local variables.
		li_estiss = dw_magazine_producer_changes_main_rc.GetItemNumber(1,"mag_estiss")
		li_remaining_issues = Integer(em_remaining_issues.text)
		lr_cntcso = dw_magazine_producer_changes_main_rc.GetItemNumber(1,"mag_cntcso")
		lr_estmail = dw_magazine_producer_changes_main_rc.Object.mag_estmail[1] 
		ls_producer = TRIM(sle_new_producer.text)
		ls_format = TRIM(dw_magazine_producer_changes_main_rc.GetItemString(1,'magcntr_format'))
		li_temp = li_estiss - li_remaining_issues
		
		//If li_temp is less than zero then set it to zero
		IF li_temp < 0 THEN
		  li_temp = 0
		END IF
		// Update the mag table set the status to 'D'
		li_fy = Integer(em_fy.text)
		ls_cntr = dw_magazine_producer_changes_main_rc.GetItemString(1,"magcntr_cntr")
		ls_magcode = TRIM(dw_magazine_producer_changes.GetItemString(1,"magcd"))
		  UPDATE mag  
			  SET freq = :li_temp,   
					mag.magst = "D"  
			WHERE ( mag.fy = :li_fy ) AND  
					( mag.cntr = :ls_cntr ) AND  
					( mag.magcd = :ls_magcode )
			USING SqlServerTrans;
		
		//IF Update successfull insert into magcntr table.
		IF SqlServerTrans.Sqlcode = 0 THEN
			
			COMMIT USING SqlServerTrans;
			
			ls_cntr = TRIM(sle_new_contract.text)
			li_rtn_code = wf_contract_exists(li_fy, ls_cntr)
			//Check to see if the contract exists. If it does not exist then
			//insert contract information into the magcntr table.
			IF li_rtn_code = 100 THEN
				
				INSERT INTO magcntr
						(fy,
						 cntr,
						 prdr,
						 format,
						 prodcd)
   			VALUES (:li_fy,
				        :ls_cntr,
						  :ls_producer,
				        :ls_format,
						  NULL)
				USING SqlServerTrans;
				
				IF SqlServerTrans.SqlCode = 0 THEN
					COMMIT USING SqlServerTrans;
				ELSE
					ROLLBACK USING SqlServerTrans;
				END IF//IF SqlServerTrans.SqlCode = 0 THEN
			
		END IF//IF li_rtn_code = 100 THEN
				
		//Change the row to new modified and input new data to the
		//datawindow and update.
		dw_magazine_producer_changes_main_rc.Object.mag_cntr[1] = TRIM(sle_new_contract.text)
		dw_magazine_producer_changes_main_rc.Object.mag_freq[1] = Integer(em_remaining_issues.text)
		dw_magazine_producer_changes_main_rc.Object.mag_magst[1] = "A"
		dw_magazine_producer_changes_main_rc.Object.mag_invamtc[1] = 0
		dw_magazine_producer_changes_main_rc.Object.mag_cntcsadjc[1] = 0
		IF IsNull(lr_estmail) THEN lr_estmail = 0
		dw_magazine_producer_changes_main_rc.Object.mag_cntcsc[1] = lr_cntcso + lr_estmail 
		dw_magazine_producer_changes_main_rc.SetItemStatus(1,0,Primary!,NewModified!)
				
		dw_magazine_producer_changes_main_rc.AcceptText()
	   
		//Update the mag datawindow with the new values
		li_rc = dw_magazine_producer_changes_main_rc.Update()
		IF li_rc = 1 THEN
			COMMIT USING SqlServerTrans;
			cb_clear.Event clicked()
		ELSE
			MessageBox("Error","Error Inserting Into Mag Table .. Contact Your DBA")
         ROLLBACK USING SqlServerTrans;
		END IF//IF li_rc = 1 THEN
			
		COMMIT USING SqlServerTrans;
		RETURN 1
	ELSE
		ROLLBACK USING SqlServerTrans;
		RETURN 2
	END IF//IF SqlServerTrans.Sqlcode = 0 THEN
			
ELSEIF dw_magazine_producer_changes_main.visible = TRUE THEN
		//Place datawindow variables into local variables.
		li_estiss = dw_magazine_producer_changes_main.GetItemNumber(1,"mag_estiss")
		li_remaining_issues = Integer(em_remaining_issues.text)
		lr_cntcso = dw_magazine_producer_changes_main.GetItemNumber(1,"mag_cntcso")
		lr_estmail = dw_magazine_producer_changes_main.Object.mag_estmail[1] 
		ls_producer = TRIM(sle_new_producer.text)
		ls_format = TRIM(dw_magazine_producer_changes_main.GetItemString(1,'magcntr_format'))
		li_temp = li_estiss - li_remaining_issues
		
		
		IF li_temp < 0 THEN
		  li_temp = 0
		END IF
		
		// Update the mag table set the status to 'D'
		li_fy = Integer(em_fy.text)
		ls_cntr = dw_magazine_producer_changes_main.GetItemString(1,"magcntr_cntr")
		ls_magcode = TRIM(dw_magazine_producer_changes.GetItemString(1,"magcd"))
		
		  UPDATE mag  
			  SET freq = :li_temp,   
					mag.magst = "D"  
			WHERE ( mag.fy = :li_fy ) AND  
					( mag.cntr = :ls_cntr ) AND  
					( mag.magcd = :ls_magcode )
			USING SqlServerTrans;
		
		
		IF SqlServerTrans.Sqlcode = 0 THEN
				COMMIT USING SqlServerTrans;
				 ls_cntr = TRIM(sle_new_contract.text)
				 li_rtn_code = wf_contract_exists(li_fy,ls_cntr)
				
				IF li_rtn_code = 100 THEN	
				// insert new values into magcntr table
				
				
				  INSERT INTO magcntr  
							( fy,   
							  cntr,   
							  prdr,   
							  format,   
							  prodcd )  
				  VALUES ( :li_fy,   
							  :ls_cntr,   
							  :ls_producer,   
							  :ls_format,   
							  NULL ) 
				 USING SqlServerTrans;
				 
				IF SqlServerTrans.SqlCode = -1 THEN
					MessageBox('Error','Error Updating magcntr Table .. Contact You DBA')
					ROLLBACK USING SqlServerTrans;
				ELSE
					COMMIT USING SqlServerTrans;
				END IF//IF SqlServerTrans.SqlCode = 0 THEN
				
			END IF//IF wf_contract_exists(li_fy,ls_cntr) = 100 THEN	
				
				//Change the row to new modified and input the new data to the datawindow 
				// and update
				
				dw_magazine_producer_changes_main.SetItem(1,'mag_cntr',TRIM(sle_new_contract.text))
				dw_magazine_producer_changes_main.SetItem(1,'mag_freq',Integer(em_remaining_issues.text))
				dw_magazine_producer_changes_main.SetItem(1,'mag_magst',"A")
				dw_magazine_producer_changes_main.SetItem(1,'mag_invamtc',0)
				dw_magazine_producer_changes_main.SetItem(1,'mag_cntcsadjc',0)
				IF IsNull(lr_estmail) THEN lr_estmail = 0
				dw_magazine_producer_changes_main.SetItem(1,'mag_cntcsc',lr_cntcso + lr_estmail )
				dw_magazine_producer_changes_main.SetItemStatus(1,0,Primary!,NewModified!)
				
				dw_magazine_producer_changes_main.AcceptText()
				
				li_rc = dw_magazine_producer_changes_main.update()
				IF li_rc = 1 THEN
					COMMIT USING SqlServerTrans;
					cb_clear.Event clicked()
				ELSE
					MessageBox("Error","Error Inserting Into mag Table .. Contact Your DBA")
					ROLLBACK USING SqlServerTrans;
				END IF//IF li_rc = 1 THEN
				COMMIT USING SqlServerTrans;
				RETURN 1
		ELSE
			ROLLBACK Using SqlServerTrans;
			RETURN 2
		END IF//IF SqlServerTrans.Sqlcode = -1 THEN
		


END IF//IF dw_magazine_producer_changes_main_rc.visible = TRUE THEN






end event

type cb_clear from u_cb within w_magazine_producer_changes
event ue_hint_text pbm_mousemove
string tag = "Clears the screen for input"
integer x = 2011
integer y = 1696
integer height = 104
integer taborder = 0
boolean bringtotop = true
integer textsize = -10
string text = "&Clear"
end type

event ue_hint_text;call super::ue_hint_text;w_pics_main.setmicrohelp(this.tag)
end event

event clicked;call super::clicked;dw_magazine_producer_changes.Reset()
dw_magazine_producer_changes_main.Reset()
dw_magazine_producer_changes_main_rc.Reset()


dw_magazine_producer_changes.Event pfc_addrow()
dw_magazine_producer_changes_main.Event pfc_addrow()
dw_magazine_producer_changes_main.Visible = TRUE
dw_magazine_producer_changes_main_rc.visible = FALSE

//enable objects
wf_enable()

//Disable the buttons and the main datawindow
cb_clear.Enabled = FALSE
cb_update.Enabled = FALSE
dw_magazine_producer_changes_main.Enabled = FALSE


//clear sle and em
em_fy.text = ''
sle_new_contract.text = ''
sle_new_producer.text = ''
sle_title.text = ''
em_remaining_issues.text = ''


em_fy.SetFocus()
end event

type em_fy from u_em within w_magazine_producer_changes
event ue_keydown pbm_keydown
event ue_hint_text pbm_mousemove
string tag = "Enter the fiscal year"
integer x = 425
integer y = 32
integer width = 229
integer height = 88
integer taborder = 10
string text = "0000"
alignment alignment = center!
maskdatatype maskdatatype = datemask!
string mask = "yyyy"
string displaydata = ""
double increment = 0
string minmax = ""
end type

event ue_keydown;call super::ue_keydown;
IF KeyDown(KeyEnter!) THEN
	dw_magazine_producer_changes.SetFocus()
END IF
end event

event ue_hint_text;call super::ue_hint_text;w_pics_main.setmicrohelp(this.tag)
end event

event getfocus;call super::getfocus;w_pics_main.Event pfc_microhelp("Enter Fiscal Year")
em_fy.Event pfc_selectall()
end event

event modified;call super::modified;//Enable the Clear button
cb_clear.Enabled =  TRUE
end event

type sle_new_producer from u_sle within w_magazine_producer_changes
event getfocus pbm_ensetfocus
event ue_key pbm_keydown
string tag = "Enter new producer"
integer x = 1943
integer y = 144
integer width = 384
integer height = 96
integer taborder = 40
boolean bringtotop = true
textcase textcase = upper!
end type

event getfocus;call super::getfocus;
IF TRIM(sle_new_contract.text) = '' THEN
   sle_new_contract.SetFocus()
ELSE
	w_pics_main.Event pfc_microhelp("Enter New Producer")
END IF
end event

event ue_key;call super::ue_key;
IF keyDown(KeyEnter!) THEN
	em_remaining_issues.SetFocus()
END IF
end event

type dw_magazine_producer_changes_main_rc from u_dw within w_magazine_producer_changes
event ue_enter_to_tab pbm_dwnprocessenter
boolean visible = false
integer x = 18
integer y = 452
integer width = 2734
integer height = 1180
integer taborder = 70
string dataobject = "d_magazine_producer_changes_main_rc"
boolean vscrollbar = false
end type

event ue_enter_to_tab;call super::ue_enter_to_tab;Send(Handle(this), 256,9,Long(0,0))
end event

event constructor;call super::constructor;this.of_SetTransObject(SqlServerTrans)
end event


$PBExportHeader$w_magazine_changes.srw
forward
global type w_magazine_changes from w_sheet
end type
type st_1 from u_st within w_magazine_changes
end type
type em_fy from u_em within w_magazine_changes
end type
type dw_magazine_changes_magcode from u_dw within w_magazine_changes
end type
type dw_magazine_changes_producer from u_dw within w_magazine_changes
end type
type dw_magazine_changes from u_dw within w_magazine_changes
end type
type cb_update from u_cb within w_magazine_changes
end type
type cb_clear from u_cb within w_magazine_changes
end type
type cb_exit from u_cb within w_magazine_changes
end type
type st_2 from statictext within w_magazine_changes
end type
type st_3 from statictext within w_magazine_changes
end type
type st_4 from statictext within w_magazine_changes
end type
end forward

global type w_magazine_changes from w_sheet
integer x = 357
integer y = 312
integer width = 3200
integer height = 1788
string title = "Magazine Changes"
st_1 st_1
em_fy em_fy
dw_magazine_changes_magcode dw_magazine_changes_magcode
dw_magazine_changes_producer dw_magazine_changes_producer
dw_magazine_changes dw_magazine_changes
cb_update cb_update
cb_clear cb_clear
cb_exit cb_exit
st_2 st_2
st_3 st_3
st_4 st_4
end type
global w_magazine_changes w_magazine_changes

type variables
String ls_temp_contract

end variables

forward prototypes
public function integer wf_check_for_active_contract (integer li_fy, string ls_magcode)
public function integer wf_check_if_contract_exists (integer li_fy, string ls_contract, string ls_magcode)
public function integer wf_check_if_multiple_contracts (integer li_fy, string ls_producer, ref integer li_num_rows)
public function integer wf_get_contract_by_producer (integer li_fy, string ls_producer, ref string ls_contract, ref string ls_format)
public function integer wf_get_title (string ls_magcode, ref string ls_title)
public subroutine wf_enable_disable_objects ()
end prototypes

public function integer wf_check_for_active_contract (integer li_fy, string ls_magcode);Integer li_total_rows


SELECT count(*)
INTO   :li_total_rows
FROM   mag
WHERE  fy    = :li_fy and
       magcd  = :ls_magcode and
		 magst = "A"
USING SqlServerTrans;

Return li_total_rows
		 
end function

public function integer wf_check_if_contract_exists (integer li_fy, string ls_contract, string ls_magcode);
//Check if contract, magcode and fy exists in magtable
SELECT cntr 
INTO   :ls_contract
FROM   mag
WHERE  mag.cntr = :ls_contract and
       mag.fy   = :li_fy and
		 mag.magcd = :ls_magcode
USING  SqlServerTrans;



return SqlServerTrans.SqlCode
end function

public function integer wf_check_if_multiple_contracts (integer li_fy, string ls_producer, ref integer li_num_rows);SELECT  count(*)
 INTO    :li_num_rows
 FROM    magcntr  
 WHERE   ( magcntr.fy = :li_fy ) AND  
         ( magcntr.prdr = :ls_producer )
 USING   SqlServerTrans;


Return SqlServerTrans.SqlCode
end function

public function integer wf_get_contract_by_producer (integer li_fy, string ls_producer, ref string ls_contract, ref string ls_format);
SELECT cntr,format 
INTO   :ls_contract,
       :ls_format
FROM   magcntr
WHERE  fy = :li_fy AND
       prdr = :ls_producer
USING SqlServerTrans;

Return SqlServerTrans.SqlCode
end function

public function integer wf_get_title (string ls_magcode, ref string ls_title);

  SELECT magttl.title  
  INTO   :ls_title  
  FROM   magttl  
  WHERE  magttl.magcd = :ls_magcode
  USING  SqlServerTrans;
	
Return SqlServerTrans.Sqlcode
end function

public subroutine wf_enable_disable_objects ();dw_magazine_changes.Enabled = TRUE
dw_magazine_changes_magcode.Enabled = FALSE
dw_magazine_changes_producer.Enabled = FALSE
em_fy.taborder = 0
em_fy.DisplayOnly = TRUE
cb_update.Enabled = TRUE
end subroutine

on w_magazine_changes.create
int iCurrent
call super::create
this.st_1=create st_1
this.em_fy=create em_fy
this.dw_magazine_changes_magcode=create dw_magazine_changes_magcode
this.dw_magazine_changes_producer=create dw_magazine_changes_producer
this.dw_magazine_changes=create dw_magazine_changes
this.cb_update=create cb_update
this.cb_clear=create cb_clear
this.cb_exit=create cb_exit
this.st_2=create st_2
this.st_3=create st_3
this.st_4=create st_4
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_1
this.Control[iCurrent+2]=this.em_fy
this.Control[iCurrent+3]=this.dw_magazine_changes_magcode
this.Control[iCurrent+4]=this.dw_magazine_changes_producer
this.Control[iCurrent+5]=this.dw_magazine_changes
this.Control[iCurrent+6]=this.cb_update
this.Control[iCurrent+7]=this.cb_clear
this.Control[iCurrent+8]=this.cb_exit
this.Control[iCurrent+9]=this.st_2
this.Control[iCurrent+10]=this.st_3
this.Control[iCurrent+11]=this.st_4
end on

on w_magazine_changes.destroy
call super::destroy
destroy(this.st_1)
destroy(this.em_fy)
destroy(this.dw_magazine_changes_magcode)
destroy(this.dw_magazine_changes_producer)
destroy(this.dw_magazine_changes)
destroy(this.cb_update)
destroy(this.cb_clear)
destroy(this.cb_exit)
destroy(this.st_2)
destroy(this.st_3)
destroy(this.st_4)
end on

event key;call super::key;
GraphicObject which_control
SingleLineEdit sle_which
CommandButton cb_which
EditMask      em_which
DataWindow    dw_which
string text_value

which_control = GetFocus( ) 


CHOOSE CASE TypeOf(which_control)

CASE CommandButton!
	cb_which = which_control
	text_value = cb_which.Text

CASE SingleLineEdit!
	sle_which = which_control
	//

CASE DataWindow!
     dw_which = which_control
   //   	
	

CASE EditMask!
	em_which = which_control
 IF  (em_which  = em_fy ) THEN

      IF  key = KeyEnter! OR key = keyTab! THEN
		 IF TRIM(em_fy.text) = '' OR em_fy.text = '0000' THEN	
         MessageBox('Error', 'Data Required')
         em_fy.SetFocus()
         RETURN
		 ELSE
			dw_magazine_changes_magcode.SetFocus()
		END IF
	  END IF //key = KeyEnter!  THEN
 END IF //(em_which  = em_fy ) THEN


CASE ELSE
	text_value = ""
END CHOOSE
end event

event pfc_preopen;call super::pfc_preopen;this.of_SetPreference(TRUE)
this.inv_preference.of_SetToolBars(TRUE)
this.inv_preference.of_SetWindow(TRUE)
this.of_SetResize(TRUE)
this.inv_resize.of_SetOrigSize(this.workspacewidth(),this.workspaceheight())

inv_resize.of_Register(dw_magazine_changes, "scale")
inv_resize.of_Register(dw_magazine_changes_magcode, "scale")
inv_resize.of_Register(dw_magazine_changes_producer, "scale")
inv_resize.of_Register(cb_clear, "scale")
inv_resize.of_Register(cb_exit, "scale")
inv_resize.of_Register(cb_update, "scale")



inv_resize.of_Register(st_1, "scale")
inv_resize.of_Register(st_2, "scale")
inv_resize.of_Register(st_3, "scale")
inv_resize.of_Register(st_4, "scale")

end event

event resize;call super::resize;long ll_height
This.X = w_pics_main.X
This.Y = w_pics_main.Y
ll_height = w_pics_main.mdi_1.Height
This.Resize(w_pics_main.WorkSpaceWidth(), ll_height)
end event

event pfc_postopen;call super::pfc_postopen;em_fy.SetFocus()
em_fy.Event pfc_selectall()
w_pics_main.SetMicroHelp(em_fy.tag)

cb_clear.Enabled = FALSE
cb_update.Enabled = FALSE
end event

event open;call super::open;//Disable Buttons
cb_clear.Enabled = FALSE
cb_update.Enabled = FALSE

//Maximize the window
this.windowstate = maximized!
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
Integer	li_rc, li_rtn
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
		li_rtn =	cb_update.TriggerEvent(clicked!)
			IF li_rtn = 1 THEN
				RETURN 0
			END IF
			
			
			
			
			// YES - Update
			// If the update fails, prevent the window from closing
//			If This.Event pfc_save() >= 1 Then
//				// Successful update, allow the window to be closed
//				Return 0
//			End If
		Case 2
			// NO - Allow the window to be closed without saving changes
			Return 0
		Case 3
			dw_magazine_changes.Setfocus()
			// CANCEL -  Prevent the window from closing
	End Choose
End If

// Prevent the window from closing
ib_closestatus = False
Return 1
end event

event mousemove;call super::mousemove;w_pics_main.setmicrohelp('Ready')
end event

type st_1 from u_st within w_magazine_changes
integer x = 37
integer y = 24
integer width = 343
integer textsize = -10
string text = "Fiscal Year"
end type

type em_fy from u_em within w_magazine_changes
event modified pbm_enmodified
string tag = "Enter Fiscal Year"
integer x = 384
integer y = 20
integer width = 229
integer height = 84
integer taborder = 10
boolean bringtotop = true
integer textsize = -10
alignment alignment = center!
maskdatatype maskdatatype = datemask!
string mask = "yyyy"
string displaydata = ""
double increment = 0
string minmax = ""
end type

event modified;call super::modified;dw_magazine_changes_magcode.Enabled = TRUE
cb_clear.Enabled = TRUE

end event

event getfocus;call super::getfocus;
w_pics_main.Event pfc_MicroHelp ( this.tag )
em_fy.Event pfc_selectall()
end event

type dw_magazine_changes_magcode from u_dw within w_magazine_changes
event ue_enter pbm_dwnkey
event ue_hint_text pbm_mousemove
string tag = "Enter Magazine Code"
integer x = 1102
integer y = 20
integer width = 1015
integer height = 256
integer taborder = 20
string dataobject = "d_magazine_changes_magcode"
boolean vscrollbar = false
boolean livescroll = false
borderstyle borderstyle = styleraised!
end type

event ue_enter;call super::ue_enter;String ls_magcode

ls_magcode = dw_magazine_changes_magcode.GetItemString(1,"magcd")


IF  keydown(KeyEnter!)   AND (TRIM(ls_magcode) <> '' OR NOT(IsNull(ls_magcode))) THEN
	dw_magazine_changes_producer.SetFocus()
END IF
end event

event ue_hint_text;call super::ue_hint_text;w_pics_main.setmicrohelp(this.tag)
end event

event getfocus;call super::getfocus;w_pics_main.SetMicroHelp(this.tag)


dw_magazine_changes_magcode.Event pfc_restorerow()
IF em_fy.text = '' OR em_fy.text = '0000' or IsNull(em_fy.text) THEN
	MessageBox('Error','Need Fiscal Year To Process')
   em_fy.SetFocus()
END IF
	
end event

event constructor;call super::constructor;this.of_settransobject(SqlServerTrans)
dw_magazine_changes_magcode.Event pfc_addrow()
end event

event itemchanged;call super::itemchanged;String ls_magcode, ls_title, null_string
Integer li_rtn_code

SetNull(null_string)



IF DWO.Name = "magcd" THEN
	ls_magcode  = TRIM(GetText())
	li_rtn_code = wf_get_title(ls_magcode,ls_title)
	
	IF li_rtn_code = 0 THEN
		dw_magazine_changes_producer.Enabled = TRUE
		dw_magazine_changes_magcode.SetItem(row,'title',ls_title)
		dw_magazine_changes_producer.SetFocus()
	ELSEIF li_rtn_code = 100 THEN
	  dw_magazine_changes_magcode.Object.magcd.Validationmsg = " Magazine Code Not Found"
	  dw_magazine_changes_magcode.SetItem(row,"magcd",null_string)
	  RETURN 1
	END IF //IF li_rtn_code = 0 THEN
		
END IF//IF DWO.Name = "magiss_magcd" THEN
	
	
	
	

end event

type dw_magazine_changes_producer from u_dw within w_magazine_changes
event ue_key pbm_dwnkey
event ue_hint_text pbm_mousemove
string tag = "Enter Producer Or Press Return Key to Retrieve"
integer x = 2437
integer y = 20
integer width = 384
integer height = 100
integer taborder = 30
string dataobject = "ddw_magazine_changes_producer"
boolean vscrollbar = false
boolean livescroll = false
borderstyle borderstyle = styleraised!
end type

event ue_key;call super::ue_key;Integer li_num_rows, li_fy, li_rtn_code
String  ls_magcode,ls_contract, ls_producer, ls_format

li_fy = Integer(em_fy.text)
ls_magcode = dw_magazine_changes_magcode.GetText()
//IF no producer is input then retrieve data based on fiscal year and magazine code.
//IF no data exists then output message and let user enter magazine code.
IF keydown(KeyEnter!) AND TRIM(dw_magazine_changes_producer.GetText()) = ''  THEN
  

  IF dw_magazine_changes.Retrieve(li_fy,ls_magcode) = 1 THEN
	  wf_enable_disable_objects()
	  ls_contract = dw_magazine_changes.Object.cntr[1]
	  ls_producer = dw_magazine_changes.Object.magcntr_prdr[1]
     dw_magazine_changes_producer.SetItem(1,"prdr",ls_producer)
     dw_magazine_changes.SetFocus()   
   ELSE
	  MessageBox('No Data','No Contract Data Found .. Enter New Producer Or Click On Clear For New Query')
     dw_magazine_changes_producer.Reset()
     dw_magazine_changes_producer.Event pfc_addrow()
     dw_magazine_changes_producer.SetFocus()
	  dw_magazine_changes.Event pfc_addrow()
   END IF//IF dw_magazine_changes.Retrieve(li_fy,ls_magcode) = 1 THEN
//If user enters a magazine code then check if data exists. If it does not
//exist then check if it is an active contract. 
ELSEIF KeyDown(KeyEnter!) 	 AND dw_magazine_changes_producer.GetText() <>''  THEN
	 ls_producer = dw_magazine_changes_producer.GetText()
	 wf_check_if_multiple_contracts(li_fy,ls_producer,li_num_rows)
	 	
	//More than one row retrieved
	IF li_num_rows > 1 THEN
	 
	      Open(w_magazine_changes_response)
			ls_contract = ls_temp_contract
			li_rtn_code = wf_check_if_contract_exists(li_fy,ls_contract,ls_magcode)
			
			   IF li_rtn_code = 0 THEN
				     dw_magazine_changes.Retrieve(li_fy,ls_magcode) 
					  wf_enable_disable_objects()
				     dw_magazine_changes.SetFocus()
				 
			   ELSEIF li_rtn_code = 100 THEN
					
				    IF wf_check_for_active_contract(li_fy,ls_magcode) > 0 THEN
				       MessageBox('Error','Magazine Already Assigned To An Active Contract')
				       dw_magazine_changes_producer.Reset()
                   dw_magazine_changes_producer.Event pfc_addrow()
                   dw_magazine_changes_producer.SetFocus()
			       ELSE
						 wf_get_contract_by_producer(li_fy,ls_producer,ls_contract,ls_format)
				       MessageBox('Add Or New Query',' No Contract Data Found ~n  Proceed With Add Or Click On Clear For New Query')
			          wf_enable_disable_objects()
						 dw_magazine_changes.Setitem(1,"cntr",ls_contract)
						 dw_magazine_changes.SetItem(1,"magcntr_format",ls_format)
						 dw_magazine_changes.SetFocus()
					 END IF //	IF wf_check_for_active_contract(li_fy,ls_magcode) = 0 THEN
			 
			 ELSEIF li_rtn_code = -1 THEN
				MessageBox('Error','Database Error..Contact Your Database Administrator')
			 END IF//ELSEIF li_rtn_code = 0 THEN
	         			
//Exactly one row retrieved
   ELSEIF li_num_rows = 1 THEN
		  wf_get_contract_by_producer(li_fy,ls_producer,ls_contract,ls_format)
		  li_rtn_code = wf_check_if_contract_exists(li_fy,ls_contract,ls_magcode)
		  
		  
		  
		 IF li_rtn_code = 100 THEN
			  
			  IF wf_check_for_active_contract(li_fy,ls_magcode) > 0 THEN
				       MessageBox('Error','Magazine Already Assigned To An Active Contract')
				       dw_magazine_changes_producer.Reset()
                   dw_magazine_changes_producer.Event pfc_addrow()
                   dw_magazine_changes_producer.SetFocus()
			  ELSE	
			          MessageBox('Add Or New Query','No Contract Data Found ~n Proceed With Add Or Click On Clear For New Query')
						 wf_enable_disable_objects()
						 dw_magazine_changes.Setitem(1,"cntr",ls_contract)
						 dw_magazine_changes.SetItem(1,"magcntr_format",ls_format)
						 dw_magazine_changes.SetFocus()
						 						 
			  END IF
			
		  ELSEIF li_rtn_code = 0 THEN
			   dw_magazine_changes.Retrieve(li_fy,ls_magcode) 
				wf_enable_disable_objects()
			   dw_magazine_changes.SetFocus()
				
		  ELSEIF li_rtn_code = -1 THEN
			   MessageBox('Error','Database Error .. Contact Your Database Administrator')
        END IF// IF li_rtn_code = 100 THEN

	ELSEIF li_num_rows = 0 THEN
     MessageBox('No Data','No Contract Data For Producer .. Enter New Producer Or Click On Clear For New Query')		
	  dw_magazine_changes_producer.Reset()
	  dw_magazine_changes_producer.Event pfc_addrow()
	  dw_magazine_changes_producer.SetFocus()
	  
 END IF// IF li_num_rows > 1 THEN
			
			
END IF//IF keydown(KeyEnter!) AND dw_magazine_changes_producer.GetText() = ''  THEN

end event

event ue_hint_text;call super::ue_hint_text;w_pics_main.setmicrohelp(this.tag)
end event

event constructor;call super::constructor;this.of_SetTransObject(SqlServerTrans)
dw_magazine_changes_producer.Event pfc_addrow()
end event

event getfocus;call super::getfocus;w_pics_main.SetMicroHelp(this.tag)
end event

type dw_magazine_changes from u_dw within w_magazine_changes
event ue_enter_to_tab pbm_dwnprocessenter
event ue_hint_text pbm_mousemove
string tag = "Enter fiscal year"
integer x = 14
integer y = 288
integer width = 3104
integer height = 1200
integer taborder = 40
string dataobject = "d_magazine_changes"
boolean vscrollbar = false
boolean livescroll = false
end type

event ue_enter_to_tab;call super::ue_enter_to_tab;Send(Handle(this),256,9,Long(0,0))
Return (1)
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

event constructor;call super::constructor;SetTransObject(SqlServerTrans)
dw_magazine_changes.Event pfc_addrow()
end event

event itemchanged;call super::itemchanged;Real lr_total


IF DWO.Name = "cntcso" THEN
	
	lr_total = Real(dw_magazine_changes.GetText()) &
	               + Real(dw_magazine_changes.Object.cntcsadjc[row])
	dw_magazine_changes.SetItem(1,"cntcsc",lr_total)
	
END IF
end event

event updatestart;call super::updatestart;w_pics_main.SetMicroHelp('Updating the Database, Please wait ...')
end event

event getfocus;call super::getfocus;String ls_format

ls_format = dw_magazine_changes.GetItemString(1,"magcntr_format")

IF TRIM(ls_format) = 'BR' THEN
	dw_magazine_changes.Object.estmin.Tabsequence = 0
ELSE
	dw_magazine_changes.Object.estmin.Tabsequence = 60
END IF
end event

event updateend;call super::updateend;long ll_total

ll_total = rowsinserted + rowsupdated

IF ll_total > 0 THEN
	MessageBox('Updated','Row(s) Updated Successfully')
END IF
end event

type cb_update from u_cb within w_magazine_changes
event ue_hint_text pbm_mousemove
string tag = "Updates the Database and Clears the Screen For the Next Input"
integer x = 1902
integer y = 1536
integer taborder = 0
integer textsize = -10
string text = "&Update"
end type

event ue_hint_text;call super::ue_hint_text;w_pics_main.setmicrohelp(this.tag)
end event

event clicked;call super::clicked;Integer li_modified_count, li_fy
String  ls_contract, ls_magcode

li_fy      = Integer(em_fy.text)
ls_magcode = dw_magazine_changes_magcode.GetText()

dw_magazine_changes.Object.mag_fy[1] = li_fy
dw_magazine_changes.Object.mag_magcd[1] = ls_magcode


//IF it is a new row then set magazine status = A and insert row else update
IF dw_magazine_changes.GetItemStatus ( 1, 0, primary!) = NEW! OR &
                    dw_magazine_changes.GetItemStatus ( 1, 0, primary!) = NEWMODIFIED! THEN
	
	dw_magazine_changes.SetItem(1, "magst", "A") 
	dw_magazine_changes.AcceptText()
     IF  dw_magazine_changes.Update() = 1 THEN
	      COMMIT Using SqlServerTrans;
	      cb_clear.TriggerEvent(clicked!)
			RETURN 1
	  ELSE
		   ROLLBACK Using SqlServerTrans;
			MessageBox('Database','Error On Updating the Mag Table ~n Contact You Database Administrator')
	 END IF
ELSE
       dw_magazine_changes.AcceptText()	
  		 IF  dw_magazine_changes.Update() = 1 THEN
	      COMMIT Using SqlServerTrans;
	      cb_clear.TriggerEvent(clicked!)
			RETURN 1
	  ELSE
		   ROLLBACK Using SqlServerTrans;
			MessageBox('Database','Error On Updating the Mag Table ~n Contact You Database Administrator')
	 END IF//IF  dw_magazine_changes.Update() = 1 THEN	 
END IF//dw_magazine_changes.GetItemStatus ( 1, 0, primary!) = NEW! OR &



end event

type cb_clear from u_cb within w_magazine_changes
event ue_hint_text pbm_mousemove
string tag = "Clears The Screen and Ready For Next Input"
integer x = 2304
integer y = 1536
integer taborder = 0
integer textsize = -10
string text = "&Clear"
end type

event ue_hint_text;call super::ue_hint_text;w_pics_main.setmicrohelp(this.tag)
end event

event clicked;call super::clicked;
dw_magazine_changes_magcode.Reset()
dw_magazine_changes_producer.Reset()
dw_magazine_changes.Reset()

dw_magazine_changes_magcode.Event pfc_addrow()
dw_magazine_changes_producer.Event pfc_addrow()
dw_magazine_changes.Event pfc_addrow()

dw_magazine_changes_magcode.Enabled = FALSE
dw_magazine_changes_producer.Enabled = FALSE
dw_magazine_changes.Enabled = FALSE
cb_clear.Enabled = FALSE
cb_update.Enabled = FALSE

em_fy.taborder = 10
em_fy.DisplayOnly = FALSE
em_fy.text = '0000'
em_fy.SetFocus()
end event

type cb_exit from u_cb within w_magazine_changes
event ue_hint_text pbm_mousemove
string tag = "Exits the Screen"
integer x = 2743
integer y = 1536
integer taborder = 0
boolean bringtotop = true
integer textsize = -10
string text = "E&xit"
end type

event ue_hint_text;call super::ue_hint_text;w_pics_main.setmicrohelp(this.tag)
end event

event clicked;call super::clicked;
IF TRIM(em_fy.text) = '' OR em_fy.text = '0000' THEN	
   ib_disableclosequery = TRUE
END IF
Parent.Event pfc_close()
end event

type st_2 from statictext within w_magazine_changes
integer x = 2144
integer y = 28
integer width = 279
integer height = 76
boolean bringtotop = true
integer textsize = -10
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long backcolor = 67108864
boolean enabled = false
string text = "Producer"
alignment alignment = right!
boolean focusrectangle = false
end type

type st_3 from statictext within w_magazine_changes
integer x = 663
integer y = 132
integer width = 430
integer height = 76
boolean bringtotop = true
integer textsize = -10
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

type st_4 from statictext within w_magazine_changes
integer x = 631
integer y = 32
integer width = 462
integer height = 76
boolean bringtotop = true
integer textsize = -10
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


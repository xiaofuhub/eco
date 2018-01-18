$PBExportHeader$w_acquisition_print_book_orders.srw
$PBExportComments$Window for print books order
forward
global type w_acquisition_print_book_orders from w_sheet
end type
type em_chno1 from u_em within w_acquisition_print_book_orders
end type
type em_chno2 from u_em within w_acquisition_print_book_orders
end type
type st_1 from u_st within w_acquisition_print_book_orders
end type
type st_2 from u_st within w_acquisition_print_book_orders
end type
type cb_exit from u_cb within w_acquisition_print_book_orders
end type
type cb_clear from u_cb within w_acquisition_print_book_orders
end type
type cb_update from u_cb within w_acquisition_print_book_orders
end type
type cb_find from u_cb within w_acquisition_print_book_orders
end type
type cb_delete from u_cb within w_acquisition_print_book_orders
end type
type cb_dealer_code from u_cb within w_acquisition_print_book_orders
end type
type dw_acquisition_orders from u_dw within w_acquisition_print_book_orders
end type
end forward

global type w_acquisition_print_book_orders from w_sheet
integer x = 214
integer y = 221
integer width = 3159
integer height = 1716
string title = "Acquisition of Source Material - Print Book Orders"
em_chno1 em_chno1
em_chno2 em_chno2
st_1 st_1
st_2 st_2
cb_exit cb_exit
cb_clear cb_clear
cb_update cb_update
cb_find cb_find
cb_delete cb_delete
cb_dealer_code cb_dealer_code
dw_acquisition_orders dw_acquisition_orders
end type
global w_acquisition_print_book_orders w_acquisition_print_book_orders

forward prototypes
public function boolean wf_check_duplicates (string ls_chartno)
public subroutine wf_disable_fields ()
public subroutine wf_enable_fields ()
public function integer wf_get_row (string ls_chart_number, ref string ls_title, ref string ls_author, ref date ld_order_date, ref date ld_receive_date, ref integer li_copies_ordered, ref integer li_copies_received, ref integer li_page, ref string ls_vendor, ref string ls_acquist_chno)
end prototypes

public function boolean wf_check_duplicates (string ls_chartno);Integer li_row_count, li_loop
String  ls_temp
Boolean Found 

li_row_count = dw_acquisition_orders.RowCount()


Found = False
FOR li_loop = 1 TO li_row_count - 1
	ls_temp = dw_acquisition_orders.GetItemString(li_loop,"ttlinit_chno")
		
	IF Trim(ls_temp) = Trim(ls_chartno) THEN
	  Found = True
	  EXIT
   END IF
		
	
NEXT



Return Found
end function

public subroutine wf_disable_fields ();//Disable the tabs to the fields in the data window
dw_acquisition_orders.Object.ttlinit_chno.tabsequence = '10'
dw_acquisition_orders.Object.acquist_pbordt.tabsequence = '0'
dw_acquisition_orders.Object.acquist_pbrecdt.tabsequence = '0'
dw_acquisition_orders.Object.acquist_pbneed.tabsequence = '0'
dw_acquisition_orders.Object.acquist_pbdlrcd.tabsequence = '0'

//Set the text in the datawindow to be the background color
//so that they will appear invisible. 
dw_acquisition_orders.Object.ttlinit_ttl.color = '12632256'
dw_acquisition_orders.Object.ttlinit_auth.color = '12632256'
dw_acquisition_orders.Object.acquist_pbordt.color = '16777215'
dw_acquisition_orders.Object.acquist_pbrecdt.color = '16777215'
dw_acquisition_orders.Object.acquist_pbneed.color = '16777215'
dw_acquisition_orders.Object.acquist_pbdlrcd.color = '16777215'




////Make invisible the fields in the datawindow
//dw_acquisition_orders.Object.ttlinit_ttl.Visible = '0'
//dw_acquisition_orders.Object.ttlinit_auth.Visible = '0'
//dw_acquisition_orders.Object.acquist_pbordt.Visible = '0'
//dw_acquisition_orders.Object.acquist_pbrecdt.Visible = '0'
//dw_acquisition_orders.Object.acquist_pbneed.Visible = '0'
//dw_acquisition_orders.Object.ttlinit_pboh.Visible = '0'
//dw_acquisition_orders.Object.acquist_pbpage.Visible = '0'
//dw_acquisition_orders.Object.acquist_pbdlrcd.Visible = '0'
//dw_acquisition_orders.Object.acquist_pbstatcd.Visible = '0'
end subroutine

public subroutine wf_enable_fields ();//Enable the tabs to the fields in the data window
dw_acquisition_orders.Object.ttlinit_chno.tabsequence = '0'
dw_acquisition_orders.Object.acquist_pbordt.tabsequence = '20'
dw_acquisition_orders.Object.acquist_pbrecdt.tabsequence = '30'
dw_acquisition_orders.Object.acquist_pbneed.tabsequence = '40'
dw_acquisition_orders.Object.acquist_pbdlrcd.tabsequence = '60'



//Sets the text color of the datawindow to black
dw_acquisition_orders.Object.ttlinit_ttl.color = '0'
dw_acquisition_orders.Object.ttlinit_auth.color = '0'
dw_acquisition_orders.Object.acquist_pbordt.color = '0'
dw_acquisition_orders.Object.acquist_pbrecdt.color = '0'
dw_acquisition_orders.Object.acquist_pbneed.color = '0'
dw_acquisition_orders.Object.acquist_pbdlrcd.color = '0'







end subroutine

public function integer wf_get_row (string ls_chart_number, ref string ls_title, ref string ls_author, ref date ld_order_date, ref date ld_receive_date, ref integer li_copies_ordered, ref integer li_copies_received, ref integer li_page, ref string ls_vendor, ref string ls_acquist_chno);  SELECT ttlinit.ttl,   
         ttlinit.auth,   
         acquist.pbordt,   
         acquist.pbrecdt,   
         acquist.pbneed,   
         acquist.pbdlrcd,   
         acquist.chno  
  INTO   :ls_title,
  			:ls_author,
			:ld_order_date,
			:ld_receive_date,
			:li_copies_ordered,
			:ls_vendor,
			:ls_acquist_chno			
    FROM ttlinit, acquist
   WHERE ttlinit.chno = acquist.chno (+)
	AND 	ttlinit.chno = :ls_chart_number
 USING   SqlServerTrans;

RETURN SqlServerTrans.SqlCode
end function

on w_acquisition_print_book_orders.create
int iCurrent
call super::create
this.em_chno1=create em_chno1
this.em_chno2=create em_chno2
this.st_1=create st_1
this.st_2=create st_2
this.cb_exit=create cb_exit
this.cb_clear=create cb_clear
this.cb_update=create cb_update
this.cb_find=create cb_find
this.cb_delete=create cb_delete
this.cb_dealer_code=create cb_dealer_code
this.dw_acquisition_orders=create dw_acquisition_orders
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.em_chno1
this.Control[iCurrent+2]=this.em_chno2
this.Control[iCurrent+3]=this.st_1
this.Control[iCurrent+4]=this.st_2
this.Control[iCurrent+5]=this.cb_exit
this.Control[iCurrent+6]=this.cb_clear
this.Control[iCurrent+7]=this.cb_update
this.Control[iCurrent+8]=this.cb_find
this.Control[iCurrent+9]=this.cb_delete
this.Control[iCurrent+10]=this.cb_dealer_code
this.Control[iCurrent+11]=this.dw_acquisition_orders
end on

on w_acquisition_print_book_orders.destroy
call super::destroy
destroy(this.em_chno1)
destroy(this.em_chno2)
destroy(this.st_1)
destroy(this.st_2)
destroy(this.cb_exit)
destroy(this.cb_clear)
destroy(this.cb_update)
destroy(this.cb_find)
destroy(this.cb_delete)
destroy(this.cb_dealer_code)
destroy(this.dw_acquisition_orders)
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

inv_resize.of_Register(cb_update, "scale")
inv_resize.of_Register(cb_clear, "scale")
inv_resize.of_Register(cb_exit, "scale")
inv_resize.of_Register(dw_acquisition_orders, "scale")
inv_resize.of_Register(em_chno1, "scale")
inv_resize.of_Register(em_chno2, "scale")
inv_resize.of_Register(cb_find, "scale")
inv_resize.of_Register(cb_delete, "scale")
inv_resize.of_Register(cb_dealer_code, "scale")


inv_resize.of_Register(st_1, "scale")
inv_resize.of_Register(st_2, "scale")

end event

event open;call super::open;//open the sheet in maximized state
this.windowstate = maximized!

//Disable the fields in the datawindow and the buttons
wf_disable_fields()
cb_find.Enabled = FALSE
cb_update.Enabled = FALSE


//Set Focus to the first edit mask
em_chno1.SetFocus()


end event

event pfc_postopen;call super::pfc_postopen;dw_acquisition_orders.Event pfc_addrow()
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
Integer	li_rc,li_rtn
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
	IF li_msg = 1 Then
		Return 0
	End IF
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
			li_rtn = cb_update.TriggerEvent(clicked!)
			IF li_rtn = 1 THEN
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

event mousemove;call super::mousemove;w_pics_main.Event pfc_microhelp("Ready")
end event

type em_chno1 from u_em within w_acquisition_print_book_orders
event ue_hint_text pbm_mousemove
string tag = "Enter first range of chart numbers"
integer x = 608
integer y = 36
integer width = 247
integer height = 96
integer taborder = 10
integer textsize = -10
long textcolor = 16711680
textcase textcase = upper!
maskdatatype maskdatatype = stringmask!
string mask = "######"
boolean autoskip = true
string displaydata = ""
double increment = 0
string minmax = ""
end type

event ue_hint_text;call super::ue_hint_text;w_pics_main.Event pfc_microhelp(this.tag)
end event

type em_chno2 from u_em within w_acquisition_print_book_orders
event ue_hint_text pbm_mousemove
string tag = "Enter Second Range of chart number"
integer x = 1166
integer y = 36
integer width = 247
integer height = 96
integer taborder = 20
boolean bringtotop = true
integer textsize = -10
long textcolor = 16711680
textcase textcase = upper!
maskdatatype maskdatatype = stringmask!
string mask = "######"
boolean autoskip = true
string displaydata = ""
double increment = 0
string minmax = ""
end type

event modified;call super::modified;//The modified event of this edit mask retrieves data from 
//inittl and acquist tables. It takes two chart numbers and
//retrieves the rows for the chart numbers between the two.

String ls_chartno1, ls_chartno2
Integer li_row, li_num_rows

ls_chartno1 = TRIM(em_chno1.text)
ls_chartno2 = TRIM(em_chno2.text)

w_pics_main.Event pfc_microhelp("Retrieving Data Please Wait ..")
li_num_rows =  dw_acquisition_orders.Retrieve(ls_chartno1, ls_chartno2)

//If rows exists then enable the find button.
IF li_num_rows > 0 THEN
	w_pics_main.Event pfc_microhelp(string(li_num_rows)+" Row(s) Retrieved")
	li_row = dw_acquisition_orders.Event pfc_addrow()
	dw_acquisition_orders.SetFocus()
	cb_find.Enabled = TRUE
ELSE
	w_pics_main.Event pfc_microhelp("No data found")
	li_row = dw_acquisition_orders.Event pfc_addrow()
	dw_acquisition_orders.SetItem(li_row,"acquist_pbordt",today())
   dw_acquisition_orders.SetFocus()
END IF
  

end event

type st_1 from u_st within w_acquisition_print_book_orders
integer x = 14
integer y = 64
integer width = 590
integer height = 68
integer textsize = -10
string text = "Chart Numbers From:"
end type

type st_2 from u_st within w_acquisition_print_book_orders
integer x = 1015
integer y = 68
integer width = 137
integer textsize = -10
string text = "To:"
alignment alignment = center!
end type

type cb_exit from u_cb within w_acquisition_print_book_orders
event ue_hint_text pbm_mousemove
string tag = "Exits the screen"
integer x = 2615
integer y = 1444
integer taborder = 0
integer textsize = -10
string text = "E&xit"
end type

event ue_hint_text;call super::ue_hint_text;w_pics_main.Event pfc_microhelp(this.tag)
end event

event clicked;call super::clicked;Parent.Event pfc_close()


end event

type cb_clear from u_cb within w_acquisition_print_book_orders
event ue_hint_text pbm_mousemove
string tag = "Clears the screen for next input "
integer x = 2231
integer y = 1444
integer taborder = 0
boolean bringtotop = true
integer textsize = -10
string text = "&Clear"
end type

event ue_hint_text;call super::ue_hint_text;w_pics_main.Event pfc_microhelp(this.tag)
end event

event clicked;call super::clicked;ib_disableclosequery=TRUE
dw_acquisition_orders.Reset()
dw_acquisition_orders.Event pfc_addrow()
wf_disable_fields()

//Enable the update and Find button
cb_update.Enabled = FALSE
cb_find.Enabled = FALSE
cb_delete.Enabled = FALSE


//Unprotect the edit masks.
em_chno1.DisplayOnly	= False
em_chno2.DisplayOnly = False
em_chno1.TabOrder = 10
em_chno2.TabOrder = 20

em_chno1.text = ''
em_chno2.text = ''


em_chno1.SetFocus()

end event

type cb_update from u_cb within w_acquisition_print_book_orders
event ue_hint_text pbm_mousemove
string tag = "Updates the database "
integer x = 1463
integer y = 1444
integer taborder = 0
boolean bringtotop = true
integer textsize = -10
string text = "&Update"
end type

event ue_hint_text;call super::ue_hint_text;w_pics_main.Event pfc_microhelp(this.tag)
end event

event clicked;call super::clicked;Integer li_max_rows, li_loop, li_rtn_code, li_copies_ordered, li_copies_received,li_pages, li_num_rows_updated
String  ls_acquist_chart_number, ls_ttlinit_chart_number, ls_vendor_code, ld_order_date, ld_receive_date

dw_acquisition_orders.AcceptText()
w_pics_main.Event pfc_microhelp("Updating the Database .. Please Wait..")
li_max_rows = dw_acquisition_orders.RowCount()


//Checks to see if acquist_chno exists. If it exists then it updatest the datawindow.
//If it does not exist then it inserts a new row.
li_num_rows_updated = 0
FOR li_loop = 1 to li_max_rows
	ls_ttlinit_chart_number = dw_acquisition_orders.object.ttlinit_chno[li_loop]
	ls_acquist_chart_number = dw_acquisition_orders.object.acquist_chno[li_loop]
	ld_order_date = string(dw_acquisition_orders.object.acquist_pbordt[li_loop],'MM/DD/YYYY')
	ld_receive_date = string(dw_acquisition_orders.object.acquist_pbrecdt[li_loop],'MM/DD/YYYY')
	li_copies_ordered = dw_acquisition_orders.object.acquist_pbneed[li_loop]
	ls_vendor_code = dw_acquisition_orders.object.acquist_pbdlrcd[li_loop]

		IF Not(IsNull(ls_acquist_chart_number)) THEN
				  
				  UPDATE acquist  
					  SET pbdlrcd = :ls_vendor_code,   
							pbneed = :li_copies_ordered,   
							pbordt = TO_DATE(:ld_order_date,'MM/DD/YYYY'),   
							pbrecdt = TO_DATE(:ld_receive_date,'MM/DD/YYYY')
					WHERE	chno = :ls_acquist_chart_number
					USING SqlServerTrans;
					
							IF SqlServerTrans.SqlCode = -1 THEN
								ROLLBACK USING SqlServerTrans;
								MessageBox('Error','Error Updating the Database! Contact the DBA')
							ELSEIF sqlServerTrans.SqlCode = 0 THEN 
								COMMIT USING SqlServerTrans;
								li_num_rows_updated++
							END IF
				 
				ELSEIF IsNull(ls_acquist_chart_number) THEN
					
						  INSERT INTO acquist  
									( chno,   
									  pbdlrcd,   
									  pbneed,   
									  pbordt,   
									  pbrecdt)   
						  VALUES ( :ls_ttlinit_chart_number,   
									  :ls_vendor_code,   
									  :li_copies_ordered,   
									  TO_DATE(:ld_order_date,'MM/DD/YYYY'),   
									  TO_DATE(:ld_receive_date,'MM/DD/YYYY'))
						  USING  SqlServerTrans;
				  
				  		 IF SqlServerTrans.SqlCode = -1 THEN
							 ROLLBACK USING SqlServerTrans;
							 MessageBox('Error','Error Inserting Into the Database! Contact the DBA')
						 ELSE
							 COMMIT USING SqlServerTrans;
							 li_num_rows_updated++
			END IF//IF SqlServerTrans.SqlCode = -1 THEN
				  
		END IF//IF Not(IsNull(ls_acquist_chart_number)) THEN
				
   
NEXT//FOR li_loop = 1 to li_max_rows

IF li_num_rows_updated > 0 THEN
	MessageBox('Update', 'Update Successful.')
	dw_acquisition_orders.ResetUpdate()
	RETURN 1
END IF
w_pics_main.Event pfc_microhelp("Ready")
end event

type cb_find from u_cb within w_acquisition_print_book_orders
event ue_hint_text pbm_mousemove
string tag = "Get the data"
integer x = 1079
integer y = 1444
integer taborder = 0
boolean bringtotop = true
integer textsize = -10
string text = "F&ind"
boolean default = true
end type

event ue_hint_text;call super::ue_hint_text;w_pics_main.Event pfc_microhelp(this.tag)
end event

event clicked;call super::clicked;Integer li_num_rows, li_loop, li_copies_ordered
String  ls_chart_number,ld_receive_date

li_num_rows = dw_acquisition_orders.RowCount()

//Delete all empty rows.
FOR li_loop = li_num_rows TO 1 STEP - 1
	ls_chart_number = dw_acquisition_orders.GetItemString(li_loop,"ttlinit_chno")
	IF IsNull(ls_chart_number) THEN
		dw_acquisition_orders.DeleteRow(li_loop)
	END IF
NEXT


	 
//Make visible and set tabs to all the fields in the datawindow
wf_enable_fields()
dw_acquisition_orders.SetFocus()
dw_acquisition_orders.SetRow(1)
dw_acquisition_orders.SetColumn("acquist_pbordt")

//Enable the update button and disable the find button
cb_update.Enabled = TRUE
cb_find.Enabled = FALSE

//IF Receive Date (acquist_pbrecdt) is NULL OR Empty then Put today's date in.
//IF Copies ordered (acquist_pbneed) is 0 OR Null then put the value 2 in the field.
li_num_rows = dw_acquisition_orders.RowCount()
FOR li_loop = 1 TO li_num_rows 
 	ld_receive_date   = string(dw_acquisition_orders.object.acquist_pbordt[li_loop],'MM/DD/YYYY')
	li_copies_ordered = dw_acquisition_orders.object.acquist_pbneed[li_loop]
	
	IF IsNull(ld_receive_date) THEN
		dw_acquisition_orders.object.acquist_pbordt[li_loop]=today()
	END IF
	
	IF IsNull(li_copies_ordered) OR li_copies_ordered = 0 THEN
		dw_acquisition_orders.object.acquist_pbneed[li_loop]=2
	END IF	
NEXT//FOR li_loop = 1 TO li_num_rows
ib_disableclosequery=TRUE

end event

type cb_delete from u_cb within w_acquisition_print_book_orders
event ue_hint_text pbm_mousemove
string tag = "Delete row from screen"
integer x = 1847
integer y = 1444
integer taborder = 0
boolean bringtotop = true
integer textsize = -10
boolean enabled = false
string text = "&Delete"
end type

event ue_hint_text;call super::ue_hint_text;w_pics_main.Event pfc_microhelp(this.tag)
end event

event clicked;call super::clicked;Integer li_loop, li_max_rows, li_row, li_rtn_code
String  ls_chart_number

//Sets the highlighted row as the current row
li_max_rows = dw_acquisition_orders.rowcount()

FOR li_loop = 1 TO li_max_rows
	IF dw_acquisition_orders.IsSelected(li_loop) THEN
	   dw_acquisition_orders.ScrollToRow(li_loop)
		dw_acquisition_orders.SetRow(li_loop)
	EXIT
   END IF
NEXT

//Print Message to Screen to confirm delete.
li_rtn_code = MessageBox('Delete','Do you want to delete this Row?', Question!, YesNo!)
 li_row = dw_acquisition_orders.GetRow()

//If answer is yes then unhighlight the row and change it's status to Newmodified!
//so that it will not be deleted from the database and delete the row from the primary
//buffer.
IF li_rtn_code = 1 THEN
 //dw_acquisition_orders.SelectRow(li_row, FALSE)
 dw_acquisition_orders.SetItemStatus(li_row, 0, Primary!, NewModified!)
 dw_acquisition_orders.DeleteRow(li_row)
 dw_acquisition_orders.SetFocus()
 dw_acquisition_orders.SetRow(li_row)
 cb_delete.Enabled = FALSE

ELSE
 dw_acquisition_orders.SelectRow(li_row, FALSE)
 dw_acquisition_orders.SetFocus()
 dw_acquisition_orders.SetRow(li_row)
 cb_delete.Enabled = FALSE

END IF



end event

type cb_dealer_code from u_cb within w_acquisition_print_book_orders
event ue_hint_text pbm_mousemove
string tag = "Maintains (Add and Delete) the dealer code table ..."
integer x = 69
integer y = 1448
integer width = 585
integer textsize = -10
string text = "Add Dealer Code..."
end type

event ue_hint_text;call super::ue_hint_text;w_pics_main.Event pfc_microhelp(this.tag)
end event

event clicked;call super::clicked;Open(w_acquisition_dealer_codes_response)

end event

type dw_acquisition_orders from u_dw within w_acquisition_print_book_orders
event ue_tab_to_enter pbm_dwnprocessenter
event ue_hint_text pbm_mousemove
integer x = 37
integer y = 256
integer width = 2930
integer height = 1076
integer taborder = 30
string dataobject = "d_acquisition_orders"
boolean hscrollbar = true
boolean livescroll = false
boolean ib_isupdateable = false
boolean ib_rmbmenu = false
boolean ib_rmbfocuschange = false
string is_updatesallowed = ""
end type

event ue_tab_to_enter;call super::ue_tab_to_enter;Send(Handle(this), 256,9, Long(0,0))
Return(1)
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

event constructor;Integer li_row
this.of_SetFilter(TRUE)
this.of_SetFind(TRUE)
this.inv_filter.of_SetStyle(1)
this.inv_filter.of_SetColumnNameSource(2)
this.of_SetSort(TRUE)
this.inv_sort.of_SetStyle(1)
this.inv_sort.of_SetColumnNameSource(2)

//Set the transaction object and set todays date as the default
//date in the pbordt field.
SetTransObject(SqlServerTrans)
this.of_SetDropDownCalendar(TRUE)
this.iuo_calendar.of_Register("acquist_pbordt",this.iuo_calendar.DDLB)
this.iuo_calendar.of_Register("acquist_pbrecdt",this.iuo_calendar.DDLB)
ib_rmbmenu = FALSE


end event

event itemchanged;call super::itemchanged;String  ls_chartno, ls_title, ls_author, ls_vendor, null_string,ls_acquist_chno
Date    ld_order_date, ld_receive_date
Integer li_copies_ordered, li_copies_received, li_rtn_code, li_row, li_page

SetNull(null_string)


IF Dwo.name = 'ttlinit_chno' THEN
	ls_chartno = Data
	
	//Prevent duplicate chart numbers from being entered
	IF wf_check_duplicates(ls_chartno) = TRUE THEN
		dw_acquisition_orders.Object.ttlinit_chno.ValidationMsg =  "Duplicate Chart Number Entered"
		dw_acquisition_orders.SetText(null_string)
		dw_acquisition_orders.object.ttlinit_chno[row]=null_string
		
	RETURN 1
	END IF//IF wf_check_duplicates(ls_chartno) = TRUE THEN
	
		
	//Call window function with the chart number
   li_rtn_code =	wf_get_row(ls_chartno,ls_title,ls_author,ld_order_date,ld_receive_date, &
	               li_copies_ordered, li_copies_received, li_page, ls_vendor, ls_acquist_chno)
	
	
	//If data found then enable the buttons and set the local variables to the
	//data window.
	IF li_rtn_code = 0 THEN
    
	  //Enable the Find button
	    cb_find.Enabled = TRUE
	  
	  
	  //Set the local variable to the data window
	  dw_acquisition_orders.SetItem(row,"ttlinit_ttl",ls_title)
	  dw_acquisition_orders.SetItem(row,"ttlinit_auth",ls_author)
	  dw_acquisition_orders.SetItem(row,"acquist_pbordt",ld_order_date)
	  dw_acquisition_orders.SetItem(row,"acquist_chno",ls_acquist_chno)
	  dw_acquisition_orders.SetItem(row,"acquist_pbrecdt",ld_receive_date)
	  dw_acquisition_orders.SetItem(row,"acquist_pbneed",li_copies_ordered)
//	  dw_acquisition_orders.SetItem(row,"acquist_pboh",li_copies_received)
	  dw_acquisition_orders.SetItem(row,"acquist_pbdlrcd",ls_vendor)
//	  dw_acquisition_orders.SetItem(row,"acquist_pbpage",li_page)
	  
	  //Insert New Row
	  li_row = dw_acquisition_orders.Event pfc_addrow()
	  dw_acquisition_orders.SetItem(li_row,"acquist_pbneed",2)
   ELSEIF li_rtn_code = 100 THEN
		dw_acquisition_orders.Object.acquist_chno.ValidationMsg =  "Invalid Chart Number Or Not Ordered yet"
		dw_acquisition_orders.SetText(null_string)
		dw_acquisition_orders.SetItem(row,"acquist_chno",null_string)
	RETURN 1
	  
   END IF//IF li_rtn_code = 0 THEN
	

ELSEIF DWO.Name = "acquist_pbrecdt" THEN

	Datetime ld_pbordt
	String ls_msg
	
	ld_pbordt = this.object.acquist_pbordt[row]
	
	IF	datetime(data) < ld_pbordt THEN
		ls_msg = "Received Date can not be earlier than order date"
		this.object.acquist_pbrecdt.Validationmsg =ls_msg
		RETURN 1
	END IF
	
END IF//IF Dwo.name = 'acquist_chno' THEN
end event

event itemfocuschanged;call super::itemfocuschanged;
IF DWO.Name = "acquist_pbordt" THEN
	dw_acquisition_orders.Event pfc_selectall()
END IF

IF DWO.Name = "acquist_pbrecdt" THEN
	dw_acquisition_orders.Event pfc_selectall()
END IF
end event

event getfocus;call super::getfocus;String ls_original_select



//protect the edit mask from being tabbed or changed.
em_chno1.DisplayOnly	= True
em_chno2.DisplayOnly = True

em_chno1.TabOrder = 0
em_chno2.TabOrder = 0




end event

event doubleclicked;call super::doubleclicked;//Highlights and unhighlights the rows when double clicked.
IF this.Isselected(row) = FALSE THEN
  cb_delete.Enabled = TRUE
 this.selectrow(0,FALSE)	
 this.selectrow(row,TRUE) 
ELSE
  cb_delete.Enabled = FALSE
 this.selectrow(row,FALSE) 
END IF


end event

event retrieveend;call super::retrieveend;Integer li_loop, li_copies_needed
String  ld_date,NullDate
String  ls_acquist_chart_number, ls_ttlinit_chart_number
SetNull(NullDate)

//After Retrieving put today's date for order date if acquist_pbordt is Null and
//if acquist_pbneed is null or zero then put the value 2.
FOR li_loop = 1 TO rowcount
	ld_date = string(dw_acquisition_orders.object.acquist_pbordt[li_loop],'MM/DD/YYYY')
	li_copies_needed = dw_acquisition_orders.object.acquist_pbneed[li_loop]
		
	//If order date is null then put todays date.
	IF ld_date = NullDate OR ld_date = "" THEN
		dw_acquisition_orders.object.acquist_pbordt[li_loop]=today()
	END IF
	
	//If copies needed is null or zero put the numbe 2.
	IF IsNull(li_copies_needed) OR li_copies_needed = 0 THEN
		dw_acquisition_orders.object.acquist_pbneed[li_loop]=2
   END IF		
NEXT

end event


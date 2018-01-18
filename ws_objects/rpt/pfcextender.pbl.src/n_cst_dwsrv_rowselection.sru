$PBExportHeader$n_cst_dwsrv_rowselection.sru
$PBExportComments$(K90) - PFC Extender Row Select Service
forward
global type n_cst_dwsrv_rowselection from nonvisualobject
end type
end forward

global type n_cst_dwsrv_rowselection from nonvisualobject
event type integer pfc_clicked ( integer ai_xpos,  integer ai_ypos,  long al_row,  ref dwobject adwo_obj )
event pfc_lbuttondown ( unsignedlong aul_flags,  integer ai_xpos,  integer ai_ypos )
event pfc_lbuttonup ( unsignedlong aul_flags,  integer ai_xpos,  integer ai_ypos )
event pfc_rbuttonup ( integer ai_xpos,  integer ai_ypos,  long al_row,  dwobject adwo_obj )
event pfc_rbuttondown ( integer ai_xpos,  integer ai_ypos,  long al_row,  dwobject dwo_obj )
end type
global n_cst_dwsrv_rowselection n_cst_dwsrv_rowselection

type variables
Protected:
u_dw	idw_requestor

// Service Behavior
integer	ii_style=0			

// Previous row and keys attributes.
long	il_prevclickedrow=0    	// Previous clickedrow.
boolean	ib_prevcntrl=false		// State of the CNTRL key at the time il_prevclickedrow was captured. 
boolean	ib_prevshift=false		// State of the SHIFT key at the time il_prevclickedrow was captured. 

// Current Anchor row.
long	il_anchorrow=0		

// Is the Left Button currently pressed?
boolean	ib_lbuttonpressed=false	

// Is the Right Button currently pressed?
boolean	ib_rbuttonpressed=false	

//  Arrays used to set "selected" column in dw
int ii_select[], ii_deselect[]

//  State of row highlight.
//  When true, row selection is column based
boolean ib_norowhighlight=false
end variables

forward prototypes
public function long of_selectedcount (ref long al_selectedrows[])
public function integer of_invertselection ()
protected function integer of_rowselectsingle (long al_row)
protected function integer of_rowselectmulti (long al_row)
protected function integer of_rowselectext (long al_row, boolean ab_cntrlpressed, boolean ab_shiftpressed)
protected function integer of_buttonup ()
public function integer of_setstyle (integer ai_style)
public function integer of_GetStyle ()
public function integer of_rowselect (long al_row)
public function boolean of_isselected (readonly long al_row)
public function integer of_setnohighlight (readonly boolean ab_nohighlight)
public function integer of_setrowbitmap (readonly string as_rowbitmap)
public function integer of_selectrow (readonly long al_row, readonly boolean ab_state)
public function integer of_selectrow (readonly long al_startrow, readonly long al_endrow, readonly boolean ab_state)
public function long of_getselectedrow (readonly long al_row)
public function integer of_setrowbitmap (readonly string as_rowbitmap, readonly integer ai_x, readonly integer ai_y)
public subroutine of_setrequestor (u_dw adw_requestor)
end prototypes

event pfc_clicked;//////////////////////////////////////////////////////////////////////////////
//
//	Event:  			pfc_clicked
//
//	Arguments:
//	ai_xpos:  	x position clicked
//	ai_ypos:  	y position clicked
//	al_row:  	row clicked
//	adwo_obj:  	DWobject clicked
//
//	Returns:  		Integer
//						1 if it succeeds and -1 if an error occurs.
//
//	Description:  Clicked behavior.
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

// Make sure request is valid.
if IsNull ( idw_requestor ) or not IsValid ( idw_requestor ) then return -1
if IsNull ( adwo_obj ) then return -1
if IsNull ( al_row ) or al_row <=0 then return -1
if Left ( idw_requestor.GetBandAtPointer ( ), 6) <> "detail" then
	beep(1)
	return -1
end if

// Process clicked behavior depending on selection option
return of_RowSelect ( al_row )
end event

event pfc_lbuttondown;//////////////////////////////////////////////////////////////////////////////
//
//	Event:  pfc_lbuttondown
//
//	(Arguments:
//	aul_flags: passed from lbuttondown event in pfc_u_dw
//	ai_xpos: x-position of the mouse passed from lbuttondown event in pfc_u_dw
// ai_ypos: y-position of the mouse passed from lbuttondown event in pfc_u_dw
//
//	(Returns:  None)
//
//	Description:  Process row-selection when left-mouse button is pressed
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

// Store in service that the Left Button is Pressed.
ib_lbuttonpressed = true

// Clear other button.
ib_rbuttonpressed = false
end event

event pfc_lbuttonup;//////////////////////////////////////////////////////////////////////////////
//
//	Event:  pfc_lbuttonup
//
//	(Arguments:
//	aul_flags: passed from lbuttonup event in pfc_u_dw
//	ai_xpos: x-position of the mouse passed from lbuttondown event in pfc_u_dw
// ai_ypos: y-position of the mouse passed from lbuttondown event in pfc_u_dw
//
//	(Returns:  None)
//
//	Description:  Process row-selection when left-mouse button is released
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

// Store in service that the Left Button is no longer Pressed.
ib_lbuttonpressed = false

// Clear other button.
ib_rbuttonpressed = false

//@@ gnv_app.inv_debug.of_Message(	'Left button up.')

// Perform the Button Up processing.
of_ButtonUp  ( )
end event

event pfc_rbuttonup;//////////////////////////////////////////////////////////////////////////////
//
//	Event:  pfc_rbuttonup
//
//	(Arguments:
//	ai_xpos:  	x position clicked
//	ai_ypos:  	y position clicked
//	al_row:  	row clicked
//	adwo_obj:  	DWobject clicked
//
//	(Returns:  None)
//
//	Description:  Process row-selection when right-mouse button is released
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

// Store in service that the Button is no longer Pressed.
ib_rbuttonpressed = false

// Clear other button.
ib_lbuttonpressed = false

// Perform the Button Up processing.
of_ButtonUp ( )
end event

event pfc_rbuttondown;//////////////////////////////////////////////////////////////////////////////
//
//	Event:  pfc_rbuttondown
//
//	(Arguments:
//	ai_xpos:  	x position clicked
//	ai_ypos:  	y position clicked
//	al_row:  	row clicked
//	adwo_obj:  	DWobject clicked
//
//	(Returns:  None)
//
//	Description:  Process row-selection when right-mouse button is pressed
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

// Store in service that the Button is Pressed.
ib_rbuttonpressed = true

// Clear other button.
ib_lbuttonpressed = false

// Make sure request is valid.
if IsNull ( idw_requestor ) or not IsValid ( idw_requestor ) then return
if IsNull ( al_row ) or al_row <=0 then return

// Process behavior depending on selection option
of_RowSelect ( al_row )
end event

public function long of_selectedcount (ref long al_selectedrows[]);//////////////////////////////////////////////////////////////////////////////
//
//	Function:  		of_SelectedCount
//
//	Access:  		Public
//
//	Arguments:  	none
//
//	Returns:  		Long
//						The number of selected rows in the datawindow.
//						-1 if an error occurs
//
//	Description:  	Count selected rows.
//
//////////////////////////////////////////////////////////////////////////////
//
//	Revision History
//
//	Version
//	5.0   Initial version
// 6.0   Modified to call of_GetSelectedRow instead of PS GetSelectedRow
//
//////////////////////////////////////////////////////////////////////////////
//
//	Copyright © 1996 Powersoft Corporation.  All Rights Reserved.
//	Any distribution of the PowerBuilder Foundation Classes (PFC)
//	source code by other than Powersoft is prohibited.
//
//////////////////////////////////////////////////////////////////////////////
long	ll_selected=0
long	ll_counter=0

// Check for any requirements.
if IsNull ( idw_requestor ) or not IsValid ( idw_requestor ) then return -1

// Loop and count the number of selected rows.
do
	ll_selected = of_GetSelectedRow ( ll_selected )
	if ll_selected > 0 then
		ll_counter++
		al_selectedrows[ll_counter] = ll_selected
	end if
loop while ll_selected > 0

return ll_counter
end function

public function integer of_invertselection ();//////////////////////////////////////////////////////////////////////////////
//
//	Function:  of_InvertSelection
//
//	Access:    Public
//
//	Arguments: None
//
//	Returns:   Integer
//
//	Description:  Reverses the selection (highlight) on all rows
//
//////////////////////////////////////////////////////////////////////////////
//
//	Revision History
//
//	Version
//	5.0   Initial version
// 6.0   Modified to call of_SelectRow instead of PS SelectRow 
// 6.0   Modified to call of_IsSelected instead of PS IsSelected
//
//////////////////////////////////////////////////////////////////////////////
//
//	Copyright © 1996 Powersoft Corporation.  All Rights Reserved.
//	Any distribution of the PowerBuilder Foundation Classes (PFC)
//	source code by other than Powersoft is prohibited.
//
//////////////////////////////////////////////////////////////////////////////
long	ll_max
long	ll_i

// Validate the datawindow reference.
if IsNull ( idw_requestor ) or not IsValid ( idw_requestor ) then return -1

// Get the row count.
ll_max = idw_requestor.RowCount ( ) 

// Prevent flickering and improve performance.
idw_requestor.SetReDraw ( false ) 

// Invert row by row.
for ll_i = 1 to ll_max
	of_SelectRow ( ll_i, not ( of_IsSelected ( ll_i ) ) ) 
next 

// Prevent flickering and improve performance.
idw_requestor.SetReDraw ( true ) 

return 1
end function

protected function integer of_rowselectsingle (long al_row);//////////////////////////////////////////////////////////////////////////////
//
//	Function:  	of_RowSelectSingle
//
//	Access:   	Protected
//

//	Arguments: 	Long
// al_row  		The clicked row, passed from pfc_u_dw's Clicked event argument
//
//	Returns:   	Integer
//					1 if it succeeds and -1 if an error occurs.
//
//	Description:  Selects the clicked row and de-selects any previously
//					  selected rows.
//
//////////////////////////////////////////////////////////////////////////////
//
//	Revision History
//
//	Version
//	5.0   Initial version
// 6.0   Modified to call of_SelectRow instead of PS SelectRow.
//
//////////////////////////////////////////////////////////////////////////////
//
//	Copyright © 1996 Powersoft Corporation.  All Rights Reserved.
//	Any distribution of the PowerBuilder Foundation Classes (PFC)
//	source code by other than Powersoft is prohibited.
//
//////////////////////////////////////////////////////////////////////////////

// Check arguments.
if IsNull ( al_row ) or al_row <0 then	return -1

// Deselect all rows.
of_SelectRow ( 0, false ) 

// Select the one row.
of_SelectRow ( al_row, true ) 

// Set the one row as the current row.
if idw_Requestor.GetRow ( ) <> al_row then
	idw_Requestor.SetRow ( al_row ) 
end if

return 1
end function

protected function integer of_rowselectmulti (long al_row);//////////////////////////////////////////////////////////////////////////////
//
//	Function:  	of_RowSelectMulti
//
//	Access:    	Protected
//
//	Arguments: 	Long
//   al_row   	The clicked row, passed from pfc_u_dw's Clicked event argument
//
//	Returns:   	Integer
//					1 if it succeeds and -1 if an error occurs.
//
//	Description:  Selects/De-Selects the clicked row depending on its present
//					  state.  Does not de-select previously selected rows.
//
//////////////////////////////////////////////////////////////////////////////
//
//	Revision History
//
//	Version
//	5.0   Initial version
// 6.0   Modified to call of_SelectRow instead of PS SelectRow 
//
//////////////////////////////////////////////////////////////////////////////
//
//	Copyright © 1996 Powersoft Corporation.  All Rights Reserved.
//	Any distribution of the PowerBuilder Foundation Classes (PFC)
//	source code by other than Powersoft is prohibited.
//
//////////////////////////////////////////////////////////////////////////////

// Check arguments.
if IsNull ( al_row ) or al_row < 0 then return -1

// Select or Deselect the row.
of_SelectRow ( al_row,  not ( of_IsSelected ( al_row ) ) ) 

// Make the row the current row.
if idw_Requestor.GetRow ( ) <> al_row then
	idw_Requestor.SetRow ( al_row ) 
end if

return 1
end function

protected function integer of_rowselectext (long al_row, boolean ab_cntrlpressed, boolean ab_shiftpressed);//////////////////////////////////////////////////////////////////////////////
//
//	Function:  	of_RowSelectExt
//
//	Access:    	Protected
//
//	Arguments:
//	al_row				The row on which some action is required.
//	ab_cntrlpressed	Flag stating if the CNTRL key is pressed.
//	ab_shiftpressed	Flag stating if the SHIFT key is pressed.
//
//	Returns:  		Integer
//
//	Description:  Performs specific Extended select processing on a row.
//
//////////////////////////////////////////////////////////////////////////////
//
//	Revision History
//
//	Version
//	5.0   Initial version
//	6.0   Modified to call of_SelectRow function instead of PS SelectRow 
// 6.0   Modified to call of_IsSelected instead of PS IsSelected
//
//////////////////////////////////////////////////////////////////////////////
//
//	Copyright © 1996 Powersoft Corporation.  All Rights Reserved.
//	Any distribution of the PowerBuilder Foundation Classes (PFC)
//	source code by other than Powersoft is prohibited.
//
//////////////////////////////////////////////////////////////////////////////
integer	li_i
boolean	lb_waitforbuttonup=false
boolean	lb_takenoaction=false

// Check arguments.
if IsNull ( al_row ) or al_row < 0 then return -1

// @@
// n_cst_conversion lc
// gnv_app.inv_debug.of_Message(	'Row='+string(al_row)+ &
//										'* Ctrl='+lc.of_String(ab_cntrlpressed) + &
//										'* Shift='+lc.of_String(ab_shiftpressed) + &
//										'* lbutton = '+lc.of_String(ib_lbuttonpressed)+ &
//										'* rbutton = '+lc.of_String(ib_rbuttonpressed))

//////////////////////////////////////////////////////////////////////////////
// On the first part of the 'IF' statement:
// 	If the LEFTBUTTON is pressed, the CNTRL key down, and the SHIFT is up - Then
//		according to Win95 the processing will be performed (by this same function)
//		when the Button is released.	
//	On the second part of the 'IF' statement:
// 	If the BUTTON is pressed, the CNTRL key down, and the SHIFT is up - Then
//		according to Win95 the processing will be performed (by this same function)
//		when the Button is released.	
//////////////////////////////////////////////////////////////////////////////
if ((ib_lbuttonpressed or ib_rbuttonpressed) and ab_cntrlpressed and ab_shiftpressed=false) &
or (of_IsSelected(al_row) and ib_lbuttonpressed and ab_cntrlpressed=false and ab_shiftpressed=false) then
	// Wait for the button up to process click.
	lb_waitforbuttonup = true
elseif of_IsSelected(al_row) and ib_rbuttonpressed and ab_cntrlpressed=false and ab_shiftpressed=false then
	// Right Clicking on an already Highlighted row requires a No Action process.
	lb_takenoaction = true
end if

if lb_waitforbuttonup then
	// Handle processing when the Button is released.
	il_prevclickedrow  = al_row
	ib_prevcntrl = ab_cntrlpressed
	ib_prevshift = ab_shiftpressed
	// gnv_app.inv_debug.of_Message(	'Wait for button up process.')	
	return 1
end if

// There is no Previous row information.
il_prevclickedrow  = 0
ib_prevcntrl = false
ib_prevshift = false

if lb_takenoaction then
	// Take the No Action Process.
	// @@
	// gnv_app.inv_debug.of_Message(	'No Action process.')
	return 1
end if

//////////////////////////////////////////////////////////////////////////////
// Perform now.  This is either:
//		1) Processing that does not wait for the Left Button to be released.
//		or
//		2) Processing which waited for the Left Button to be released.
//			The lbuttonup event then called this function with the following
//			variables: (il_prevclickedrow, il_prevcntrl, il_prevshift)
//////////////////////////////////////////////////////////////////////////////
if ab_cntrlpressed and ab_shiftpressed=false then

	// Select or De-Select (as appropriate) the current row.
	of_SelectRow ( al_row, not of_IsSelected ( al_row) ) 

	// Store new Anchor Row.
	il_anchorrow = al_row

elseif ab_cntrlpressed or ab_shiftpressed then

	/* Note: The valid cobinations here are:											*/
	/*					ab_cntrlpressed=true  and ab_shiftpressed=true				*/
	/*					ab_cntrlpressed=false and ab_shiftpressed=true				*/
	/*					ab_cntrlpressed=true  and ab_shiftpressed=false	+++++++	*/	
	/*		+++++++ Because of the "If" prior to this "ElseIf", it is 			*/
	/* 	impossible for ab_cntrlpressed=true and ab_shiftpressed=false.		*/
	
	if ab_cntrlpressed=false then
		// Clear all previously selected rows.	
		of_SelectRow ( 0, false )	
	end if
	
	// If there is no anchor row, then only select the row that was clicked.
	if il_anchorrow = 0 then
		of_SelectRow ( al_row, true )
	else
		// Prevent flickering.  Improve performance.
		idw_requestor.SetReDraw ( false ) 

		// Select all rows in between anchor row and current row */
		of_SelectRow ( il_anchorrow, al_row, true )
	
		// Prevent flickering.  Improve performance.
		idw_requestor.SetReDraw ( true ) 
	end if
	
else
	// Unselect all previous rows (if any) and select the current row.
	of_RowSelectSingle ( al_row )

	// Store new Anchor Row.
	il_anchorrow = al_row
	
end if
	
// Make the row the current row.
if idw_Requestor.GetRow ( ) <> al_row then
	idw_Requestor.SetRow ( al_row ) 
end if	
		
return 1
end function

protected function integer of_buttonup ();//////////////////////////////////////////////////////////////////////////////
//
//	Function:  		of_ButtonUp
//
//	Access:    		Protected
//
//	Arguments: 		None
//
//	Returns:   		Integer 
//						1 if it succeeds and -1 if an error occurs.
//
//	Description: 	Perform the Button Up processing.
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
integer li_rc = 1

// Make sure request is valid.
if IsNull ( idw_requestor ) or not IsValid ( idw_requestor ) then return -1

// Process clicked behavior depending on selection option
choose case ii_style
	// Single Select option
	case 0	
	
	// Multi Select option
	case 1		

	// Extended Select Option
	// Win 95 style of processing Control-clicks on Extended Selections.
	case 2		
		if il_prevclickedrow > 0 then
			li_rc = of_RowSelectExt ( il_prevclickedrow, ib_prevcntrl, ib_prevshift )
		end if	
end choose

// There is no Previous row information.
il_prevclickedrow  = 0
ib_prevcntrl = false
ib_prevshift = false

return li_rc
end function

public function integer of_setstyle (integer ai_style);//////////////////////////////////////////////////////////////////////////////
//
//	Function:  		of_SetStyle
//
//	Access:    		Public
//
//	Arguments:
//	ai_selectoption   The style of row selection desired.
//							Values are: 
//								  0 = Single row selection
//								  1 = Multiple row selection
//								  2 = Extended row selection capabilities
//
//	Returns:  		Integer
//	 					 1 = success
//   					-1 = The selection style requested is not available
//
//	Description:  	Set an indicator in this service for the desired 
//					  	row selection style. 
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

// Check arguments
if IsNull ( ai_style ) or ( ai_style > 2 or ai_style < 0 ) then return -1

ii_style = ai_style
return 1 
end function

public function integer of_GetStyle ();//////////////////////////////////////////////////////////////////////////////
//
//	Function:  	of_GetStyle
//
//	Access:    	Public
//
//	Arguments: 	None
//
//	Returns:   	Integer 
//   				0 = Single row selection is enabled.
//				   1 = Multiple row selection is enabled.
// 				2 = Extended row selection is enabled.
//
//	Description:  To determine the type of row selection option that is set
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
return ii_style
end function

public function integer of_rowselect (long al_row);//////////////////////////////////////////////////////////////////////////////
//
//	Function:  		of_RowSelect
//
//	Access:    		public
//
//	Arguments:
//	al_row			The row on which some action is required.
//
//	Returns:  		Integer
//						1 if it succeeds and -1 if an error occurs.
//
//	Description:   Calls the appropriate specific select processing on a row.
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
integer	li_rc=-1

choose case ii_style

	case 0
		// Single Select option
		li_rc = of_RowSelectSingle ( al_row )

	case 1
		// Multi Select option
		li_rc = of_RowSelectMulti ( al_row ) 

	case 2
		// Extended Select Option
		li_rc = of_RowSelectExt ( al_row, KeyDown ( KeyControl! ), Keydown ( KeyShift! ) ) 

end choose
	
return li_rc
end function

public function boolean of_isselected (readonly long al_row);//////////////////////////////////////////////////////////////////////////////
//
//	Function:  of_IsSelected
//
//	Access:    Public
//
//	Arguments:
//	al_row	  The row to evaluate
//
//	Returns:   boolean - the selected state of the row
//
//	Description:  Determines the selected state for the passed row
//
//////////////////////////////////////////////////////////////////////////////
//
//	Revision History
//
//	Version
//	6.0   Initial version
//
//////////////////////////////////////////////////////////////////////////////
//
//	Copyright © 1997 Powersoft Corporation.  All Rights Reserved.
//	Any distribution of the PowerBuilder Foundation Classes (PFC)
//	source code by other than Powersoft is prohibited.
//
//////////////////////////////////////////////////////////////////////////////

// Validate the datawindow reference.
if IsNull ( idw_requestor ) or not IsValid ( idw_requestor ) then return false
if IsNull ( al_row ) then return false

if ib_norowhighlight then
	/*  Verify that column actually exists and is in the detail band  */
	if idw_requestor.Describe ( "selected.Band") <> "detail" then
		return false
	else
		return ( idw_requestor.object.selected.current [ al_row ] = 1 )
	end if
else
	return idw_requestor.IsSelected ( al_row )
end if

return false
end function

public function integer of_setnohighlight (readonly boolean ab_nohighlight);//////////////////////////////////////////////////////////////////////////////
//
//	Function:  of_SetNoHighlight
//
//	Access:    Public
//
//	Arguments:
//	ab_nohighlight	  A boolean to indicate whether row selection uses highlighting
//						  to indicate selection.  When true, selection is indicated
//						  by setting a 0 or 1 value in a dw column named "pfc_selected".
//
//	Returns:   	  	  1, if it succeeds, otherwise -1
//
//	Description:  Set indicator for row selection without highlighting
//
//////////////////////////////////////////////////////////////////////////////
//
//	Revision History
//
//	Version
//	6.0   Initial version
//
//////////////////////////////////////////////////////////////////////////////
//
//	Copyright © 1997 Powersoft Corporation.  All Rights Reserved.
//	Any distribution of the PowerBuilder Foundation Classes (PFC)
//	source code by other than Powersoft is prohibited.
//
//////////////////////////////////////////////////////////////////////////////
int li_i

//  Verify passed argument
if IsNull ( ab_nohighlight ) then return -1

//  Initialize the arrays for select / deselect ( 5000 rows should be sufficient )
for li_i = 1 to 5000
	ii_select[li_i] = 1
	ii_deselect[li_i] = 0
next

ib_norowhighlight = ab_nohighlight

return 1
end function

public function integer of_setrowbitmap (readonly string as_rowbitmap);//////////////////////////////////////////////////////////////////////////////
//
//	Function:  of_SetRowBitMap
//
//	Access:    Public
//
//	Arguments:
//	as_rowbitmap	  The name of a valid bitmap file
//
//	Returns:   	  	  1, if it succeeds, otherwise -1
//
//	Description:  Create a bitmap column to indicate row selection.
//					  ( of_SetNoHighLight ( true ) must also be set ) 
//
//////////////////////////////////////////////////////////////////////////////
//
//	Revision History
//
//	Version
//	6.0   Initial version
//
//////////////////////////////////////////////////////////////////////////////
//
//	Copyright © 1997 Powersoft Corporation.  All Rights Reserved.
//	Any distribution of the PowerBuilder Foundation Classes (PFC)
//	source code by other than Powersoft is prohibited.
//
//////////////////////////////////////////////////////////////////////////////

//  Position bitmap in leftmost column 
return of_SetRowBitmap ( as_rowbitmap, 33, 4 ) 

end function

public function integer of_selectrow (readonly long al_row, readonly boolean ab_state);//////////////////////////////////////////////////////////////////////////////
//
//	Function:  of_SelectRow
//
//	Access:    Public
//
//	Arguments:
//	al_row	  A long identifying the row you want to select or deselect. 
//				  Specify 0 to select or deselect all rows.
// ab_state	  The selected state for the al_row argument
//
//	Returns:   1, if it succeeds, otherwise -1
//
//	Description:  Selects/Deselects rows
//
//////////////////////////////////////////////////////////////////////////////
//
//	Revision History
//
//	Version
//	6.0   Initial version
//
//////////////////////////////////////////////////////////////////////////////
//
//	Copyright © 1997 Powersoft Corporation.  All Rights Reserved.
//	Any distribution of the PowerBuilder Foundation Classes (PFC)
//	source code by other than Powersoft is prohibited.
//
//////////////////////////////////////////////////////////////////////////////
//  Verify arguments
if IsNull ( al_row ) then return -1
if IsNull ( ab_state ) then return -1

if ib_norowhighlight then

	/*  Verify that column actually exists and is in the detail band  */
	if idw_requestor.Describe ( "selected.Band") <> "detail" then
		return -1
	end if

	if al_row = 0 then 
		if ab_state then 
			/*  Select all rows  */
			idw_requestor.object.selected.current [1, idw_requestor.RowCount()] = ii_select
		else
			/*  De-Select all rows  */
			idw_requestor.object.selected.current [1, idw_requestor.RowCount()] = ii_deselect
		end if
	else
		if ab_state then 
			/*  Select passed row  */
			idw_requestor.object.selected.current [al_row] = 1
		else
			/*  De-Select passed row  */
			idw_requestor.object.selected.current [al_row] = 0
		end if
	end if

	return 1

else
	/*  Normal row selection  */
	return idw_requestor.SelectRow ( al_row, ab_state )
end if

return -1
end function

public function integer of_selectrow (readonly long al_startrow, readonly long al_endrow, readonly boolean ab_state);//////////////////////////////////////////////////////////////////////////////
//
//	Function:  of_SelectRow
//
//	Access:    Public
//
//	Arguments:
//	al_startrow	  A long identifying the first row you want to select or deselect.
//	al_endrow	  A long identifying the last row you want to select or deselect.
// ab_state	  	  The selected state for the row arguments
//
//	Returns:   	  1, if it succeeds, otherwise -1
//
//	Description:  Selects/Deselects rows
//
//////////////////////////////////////////////////////////////////////////////
//
//	Revision History
//
//	Version
//	6.0   Initial version
//
//////////////////////////////////////////////////////////////////////////////
//
//	Copyright © 1997 Powersoft Corporation.  All Rights Reserved.
//	Any distribution of the PowerBuilder Foundation Classes (PFC)
//	source code by other than Powersoft is prohibited.
//
//////////////////////////////////////////////////////////////////////////////
long ll_i

//  Verify arguments
if IsNull ( al_startrow ) or al_startrow = 0 then return -1
if IsNull ( al_endrow ) or al_endrow = 0 then return -1
if IsNull ( ab_state ) then return -1

if ib_norowhighlight then

	/*  Verify that column actually exists and is in the detail band  */
	if idw_requestor.Describe ( "selected.Band") <> "detail" then
		return -1
	end if

	if ab_state then 
		/*  Select row range  */
		idw_requestor.object.selected.current [al_startrow, al_endrow] = ii_select
	else
		/*  De-Select row range  */
		idw_requestor.object.selected.current [al_startrow, al_endrow] = ii_deselect
	end if

	return 1

else
	if al_startrow > al_endrow then
		/*  Normal row selection  */
		for ll_i = al_startrow to al_endrow step -1
			idw_requestor.SelectRow ( ll_i, ab_state ) 
		next
	else
		/*  Normal row selection  */
		for ll_i = al_startrow to al_endrow 
			idw_requestor.SelectRow ( ll_i, ab_state ) 
		next
	end if

	return 1

end if

return -1
end function

public function long of_getselectedrow (readonly long al_row);//////////////////////////////////////////////////////////////////////////////
//
//	Function:  of_GetSelectedRow
//
//	Access:    Public
//
//	Arguments:
//	al_row	  A long identifying the location of the row after which you
//				  want to search for the next selected row.
//
//	Returns:   The number of the first row that is selected after al_row. Returns
//				  0 if no row is selected after the specified row. 
//
//	Description:  Obtains the next selected row.
//
//////////////////////////////////////////////////////////////////////////////
//
//	Revision History
//
//	Version
//	6.0   Initial version
//
//////////////////////////////////////////////////////////////////////////////
//
//	Copyright © 1997 Powersoft Corporation.  All Rights Reserved.
//	Any distribution of the PowerBuilder Foundation Classes (PFC)
//	source code by other than Powersoft is prohibited.
//
//////////////////////////////////////////////////////////////////////////////

// Validate the datawindow reference.
if IsNull ( idw_requestor ) or not IsValid ( idw_requestor ) then return -1

if ib_norowhighlight then
	/*  Verify that column actually exists and is in the detail band  */
	if idw_requestor.Describe ( "selected.Band") <> "detail" then
		return -1
	else
		return idw_requestor.Find ( "selected=1", al_row + 1, idw_requestor.RowCount ( ) ) 
	end if
else
	return idw_requestor.GetSelectedRow ( al_row )
end if

return -1
end function

public function integer of_setrowbitmap (readonly string as_rowbitmap, readonly integer ai_x, readonly integer ai_y);//////////////////////////////////////////////////////////////////////////////
//
//	Function:  of_SetRowBitMap
//
//	Access:    Public
//
//	Arguments:
//	as_rowbitmap	  The name of a valid bitmap file
// ai_x				  The X position of the bitmap column
// ai_y				  The Y position of the bitmap column
//
//	Returns:   	  	  1, if it succeeds, otherwise -1
//
//	Description:  Create a bitmap column to indicate row selection.
//					  ( of_SetNoHighLight ( true ) must also be set ) 
//
//////////////////////////////////////////////////////////////////////////////
//
//	Revision History
//
//	Version
//	6.0   Initial version
//
//////////////////////////////////////////////////////////////////////////////
//
//	Copyright © 1997 Powersoft Corporation.  All Rights Reserved.
//	Any distribution of the PowerBuilder Foundation Classes (PFC)
//	source code by other than Powersoft is prohibited.
//
//////////////////////////////////////////////////////////////////////////////

//  Verify passed argument  
if IsNull ( as_rowbitmap ) or as_rowbitmap = "" then return -1
if IsNull ( ai_x ) or ai_x = 0 then return -1
if IsNull ( ai_y ) or ai_y = 0 then return -1

if not FileExists ( as_rowbitmap ) then return -1

//  Create the bitmap column 
if idw_requestor.Modify ( 'create bitmap(band=detail filename="' + as_rowbitmap + &
			'" x="' + string ( ai_x ) + '" y="' + string ( ai_y ) + '" height="65" ' + &
			'width="74" border="0"  name=pfc_selected_picture ' + &
			'visible="1~tif (  pfc_selected = 1, 1, 0 )" )' ) <> "" then
	return -1
else
	return 1
end if
end function

public subroutine of_setrequestor (u_dw adw_requestor);//////////////////////////////////////////////////////////////////////////////
//
//	Function:  of_SetRequestor
//
//	Access:    Public
//
//	Arguments:
//   adw_Requestor   The datawindow requesting the service
//
//	Returns:  None
//
//	Description:  Associates a datawindow control with a datawindow service NVO
//			        by setting the idw_Requestor instance variable.
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
idw_Requestor = adw_Requestor
end subroutine

on n_cst_dwsrv_rowselection.create
TriggerEvent( this, "constructor" )
end on

on n_cst_dwsrv_rowselection.destroy
TriggerEvent( this, "destructor" )
end on


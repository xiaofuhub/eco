$PBExportHeader$u_pics_dw.sru
forward
global type u_pics_dw from u_dw
end type
end forward

global type u_pics_dw from u_dw
integer width = 814
integer height = 532
event ue_postconstructor ( )
end type
global u_pics_dw u_pics_dw

type variables
// Instance variables
STRING is_backgroundcolor[], &
is_colname[], &
is_dwoname
//n_cst_dwsrv_dropdownfilter	inv_DropDownFilter
//n_cst_dwsrv_dropdownRetrieve	inv_DropDownRetrieve
end variables

forward prototypes
public subroutine of_setcolorrequired (long al_colorrequired)
public subroutine of_setcolorrequired (boolean ab_required)
public function integer of_insertallintodddw (string as_columnname)
public function integer of_setdropdownfilter (boolean ab_switch)
public function integer of_checkrequired (dwbuffer adw_buffer, ref long al_row, ref integer ai_col, ref string as_colname, boolean ab_updateonly)
public function integer of_setdropdownretrieve (boolean ab_switch)
end prototypes

public subroutine of_setcolorrequired (long al_colorrequired);/* Function: of_setcolorrequired(LONG al_colorrequired) RETURNS NONE 

Purpose: This function may be called from any descendant of corp_u_dw.
It is also called from of_setcolrorequired(BOOLEAN ab_required).
It is used to enable the changing of the background color of
required columns, that have no value, to the color specified
in the parameter.

Parameter: al_colorrequired determines the color to set the background
of required columns to when they contain no value. If the
value of al_colorrequired is negative, then the required columns
are all reset to their original background color.

Functionality: This function may be called as needed.  Ordinarily, it
should  be called from the constructor event of the dw object.
However, if your code changes columns or
datawindow objects on the fly,
then this function should be called each time any column or the
datawindow is changed.

Important:  This function changes the attributes of columns.  Therefore if
any  of the columns have a dddw edit style,
then you need to call the GetChild()
function for those columns after this function is called.
*/

// local variables
INTEGER li_NumberOfColumns, &
li_Current, &
li_retval
STRING ls_column, &
ls_colorrequired, &
ls_isrequired, &
ls_modify, &
ls_style, &
ls_result, &
ls_empty[]
BOOLEAN lb_colchanged

/* Get the number of columns in the array. */
li_NumberOfColumns = upperbound(is_colname)

/* If the function has been called before
then check to see if all of the columns
have the same name, in the same order.
If even one has changed, or is out of place,
Or if the number of columns from the
Previous call is not the same as now,
then rebuild the arrays.
*/
IF li_NumberOfColumns > 0 THEN
IF li_NumberOfColumns <> INTEGER(THIS.object.datawindow.column.count) THEN
lb_colchanged = TRUE
ELSE
FOR li_Current = 1 to li_NumberOfColumns
IF is_colname[li_Current] <> &
THIS.Describe('#' + STRING(li_Current) + '.name') THEN
lb_colchanged = TRUE
EXIT
END IF
NEXT
END IF
END IF

/* If this is the first time through,
the array will be empty.

If it is the first time, or
if we are not still working with the same dwo, or
any column has changed,
then build the arrays.
*/
IF li_NumberOfColumns = 0 &
OR THIS.object.datawindow.name <> is_dwoname &
OR lb_colchanged THEN
is_colname = ls_empty
is_backgroundcolor = ls_empty
is_dwoname = THIS.object.datawindow.name
/* Save the column names and background colors. */
li_NumberOfColumns = INTEGER(THIS.object.datawindow.column.count)
FOR li_Current = 1 to li_NumberOfColumns
is_colname[li_Current] = THIS.Describe('#' + STRING(li_Current) + '.name')
is_backgroundcolor[li_Current] = THIS.Describe(is_colname[li_Current] &
+ '.background.color')
NEXT
END IF

ls_colorrequired = STRING(al_colorrequired)

/* Find each required column and place code to change the
color into its background.color attribute.
Note that the programmer either passes a color number
or a negative value in the function parameter.
*/
FOR li_Current = 1 TO li_NumberOfColumns
ls_column = is_colname[li_Current]
ls_style = THIS.Describe(ls_column + '.Edit.Style')
CHOOSE CASE ls_style
CASE 'dddw'
ls_isrequired = THIS.Describe(ls_column + '.dddw.Required')

CASE 'ddlb'
ls_isrequired = THIS.Describe(ls_column + '.ddlb.Required')

CASE 'EditMask'
ls_isrequired = THIS.Describe(ls_column + '.EditMask.Required')

CASE '?'
CONTINUE

CASE ELSE
ls_isrequired = THIS.Describe(ls_column + '.Edit.Required')

END CHOOSE
IF ls_isrequired = 'yes' THEN
/* Set the background color attribute to the original color by default. */
ls_modify = ls_column + ".background.color='" &
+ is_backgroundcolor[li_Current]
/* If the parameter value is positive
then give the column the ability to
change color depending on whether or
not the value is present. */
IF al_colorrequired >= 0 THEN
ls_modify = ls_modify &
+ "~tif(IsNull(" + ls_column + ")," &
+ ls_colorrequired + "," &
+ is_backgroundcolor[li_Current] + ")'"
ELSE
ls_modify = ls_modify + "'"
END IF
ls_result = THIS.Modify(ls_modify)
END IF
NEXT




end subroutine

public subroutine of_setcolorrequired (boolean ab_required);/* Function: of_setcolorrequired(BOOLEAN ab_required) RETURNS NONE

Purpose: This function may be called from any descendant of corp_u_dw.
It is used to enable the changing of the background color of
required columns, that have no value, to the color specified
in gnv_app.il_requiredcolor.

Parameter: ab_required determines whether to set the background color
of required columns when they contain no value.
If the value of ab_required is TRUE, then the required columns
are all set to the color specified in gnv_app.il_requiredcolor.
If the value of ab_required is FALSE, then the required columns
are all reset to their original background color.

Functionality: This function may be called as needed.  Ordinarily, it
should
be called from the constructor event of the dw object.
However, if your code changes columns or
datawindow objects on the fly,
then this function should be called each time any column or the
datawindow is changed.

Important:  This function changes the attributes of columns.  Therefore if
any of the columns have a dddw edit style, then you need to call the
GetChild() function for those columns after this function is called.
*/

IF ab_required THEN
//of_setcolorrequired(gnv_app.il_requiredcolor)
ELSE
of_setcolorrequired(-1)
END IF

end subroutine

public function integer of_insertallintodddw (string as_columnname);//////////////////////////////////////////////////////////////////////////////
//	Public Function:  of_InsertAllIntoDddw
//
//	Arguments:		
//	string		as_columnname   	column whose dropdown datawindow needs to insert
//												All value
//	Returns:			Integer
//	 					C.SUCCESS
//	 					C.FAILURE
//
//	Description:	Insert <All> into dropdown datawindow
//////////////////////////////////////////////////////////////////////////////
//	Author:	Lijun Yang
//
//	Date:		 03/20/2000
//
//=============================================================================
//
//	Revision History
//
//	Date			Initials		Description of Change
//	----   		--------		--------------------
//	03/20/2000 	L.Y.			Initial creation
//=============================================================================

Datawindowchild ldwc_1
long ll_row

// Check arguments
this.GetChild (as_columnName, ldwc_1)

ll_row = ldwc_1.InsertRow (1)

ldwc_1.SetItem (ll_row, 1, c.ALL_DISPLAY)  //colun 1 should be disply column
ldwc_1.SetItem (ll_row, 2, c.ALL_VALUE) //column 2 should be data column

Return c.SUCCESS

end function

public function integer of_setdropdownfilter (boolean ab_switch);//////////////////////////////////////////////////////////////////////////////
//	Public Function:  of_SetDropDownFilter
//	Arguments:		ab_switch boolean
//   					true  - Start (create) the service
//   					false - Stop (destroy ) the service
//	Returns:			Integer - 1 if Successful operation, 0 = No action taken and -1 if an error occured
//	Description:	Starts or stops the DropDownDataWindow filter services
//////////////////////////////////////////////////////////////////////////////
//	Author:	Lijun Yang
//
//	Date:		10/04/99
//
//=============================================================================
//
//	Revision History
//
//	Date			Initials		Description of Change
//	----   		--------		--------------------
//	10/04/99		L.Y.			Initial creation
//=============================================================================

// Check arguments
if IsNull(ab_switch) then return c.FAILURE

if ab_Switch then
//	if IsNull(inv_DropDownFilter) or not IsValid (inv_DropDownFilter) then
//		inv_DropDownFilter = Create n_cst_dwsrv_DropDownFilter
//		inv_DropDownFilter.of_SetRequestor ( this )
		return SUCCESS
//	end if
else 
//	if IsValid (inv_DropDownFilter) then
//		Destroy inv_DropDownFilter
		return SUCCESS
//	end if	
end if 

return NO_ACTION
end function

public function integer of_checkrequired (dwbuffer adw_buffer, ref long al_row, ref integer ai_col, ref string as_colname, boolean ab_updateonly);//////////////////////////////////////////////////////////////////////////////
//	Public Function:  of_CheckRequired 
//	Arguments:		adw_buffer   	The buffer to check for required fields
// 					al_row   		First row to be checked.  Also stores the number of the found row
//						ai_col   		First column to be checked.  Also stores the number of the found column
//						as_colname   	Contains the columnname in error
//	Returns:			Integer
//	 					1 = The required fields test was successful, check arguments for required fields missing
//	 					0 = The required fields test was successful and no errors were found
//  					-1 = Error
//	Description:	Calls the FindRequired function to determine if any of the required columns contain null values.
//////////////////////////////////////////////////////////////////////////////
//	Author:	Lijun Yang
//
//	Date:		11/17/99
//
//=============================================================================
//
//	Revision History
//
//	Date			Initials		Description of Change
//	----   		--------		--------------------
//	11/17/99		L.Y.			Override PFC fucntion in order to display a user-friendly
//									Message
//=============================================================================
w_master	lw_pfcparent
window	lw_parent
boolean	lb_skipmessage=false
string	ls_msgparm[2], ls_colname
integer	li_rc
n_cst_string lnv_string

// Check arguments
if IsNull (adw_buffer) or IsNull (al_row) or IsNull (ai_col) or IsNull (as_colname) then return FAILURE

SetPointer(HourGlass!) 

// Call FindRequired to locate first error, if any
if this.FindRequired (adw_buffer, al_row, ai_col, as_colname, ab_updateonly) < 0 then return FAILURE

// Double Check if failure condition was ecountered.
if al_row < 0 then return FAILURE

// Check if no missing values were found.
if al_row = 0 then return 0

// -- A Missing Value was encountered. --


// Get a reference to the window
this.of_GetParentWindow (lw_parent) 
if IsValid (lw_parent) then
	if lw_parent.TriggerEvent ("pfc_descendant") = 1 then
		lw_pfcparent = lw_parent
	end if
end if	
	
// Make sure the window is not closing.  
if IsValid (lw_pfcparent) then
	if lw_pfcparent.of_GetCloseStatus() then
		// It is closing, so don't show errors now.	
		lb_skipmessage = true
	end if
end if
	
// Skip the message if the window is closing.	
if not lb_skipmessage then
	// Call stub function to either handle condition or provide a more suitable
	// column name.
	li_rc = this.Event pfc_checkrequirederror (al_row, as_colname)
	if li_rc < 0 then return -1
	
	if li_rc >= 1 then
		// Display condition.
		ls_colname = Describe(as_colname +"_t.Text")
		ls_colname = lnv_string.of_GlobalReplace (ls_colname, '&', '')
		if IsValid(gnv_app.inv_error) then			
			ls_msgparm[1] = as_colname
			ls_msgparm[2] = String (al_row)
			gnv_app.inv_error.of_Message("pfc_requiredmissing", ls_msgparm, &
					gnv_app.iapp_object.DisplayName)
		else
			of_MessageBox ("pfc_checkrequired_missingvalue", gnv_app.iapp_object.DisplayName, &
				"Required value missing for '" + ls_colname + "' on row "  + String (al_row) + &
				".  Please enter a value.", information!, Ok!, 1)
		end if
	
		// Make sure row/column gets focus.
		this.SetRow (al_row)
		this.ScrollToRow (al_row)		
		this.SetColumn (ai_col)
		this.SetFocus () 		
	end if
end if
	
// Return that a required column does contain a null value.
return 1
end function

public function integer of_setdropdownretrieve (boolean ab_switch);//////////////////////////////////////////////////////////////////////////////
//	Public Function:  of_SetDropDownRetrieve
//	Arguments:		ab_switch boolean
//   					true  - Start (create) the service
//   					false - Stop (destroy ) the service
//	Returns:			Integer - 1 if Successful operation, 0 = No action taken and -1 if an error occured
//	Description:	Starts or stops the DropDownDataWindow Retrieve services
//////////////////////////////////////////////////////////////////////////////
//	Author:	Lijun Yang
//
//	Date:		10/04/99
//
//=============================================================================
//
//	Revision History
//
//	Date			Initials		Description of Change
//	----   		--------		--------------------
//	10/04/99		L.Y.			Initial creation
//=============================================================================

// Check arguments
if IsNull(ab_switch) then return c.FAILURE

if ab_Switch then
//	if IsNull(inv_DropDownRetrieve) or not IsValid (inv_DropDownRetrieve) then
//		inv_DropDownRetrieve = Create n_cst_dwsrv_DropDownRetrieve
//		inv_DropDownRetrieve.of_SetRequestor ( this )
		return SUCCESS
//	end if
else 
//	if IsValid (inv_DropDownRetrieve) then
//		Destroy inv_DropDownRetrieve
		return SUCCESS
//	end if	
end if 

return NO_ACTION
end function

event constructor;call super::constructor;this.of_SetPrintPreview(TRUE)
this.Event Static Post ue_postconstructor()
end event

on u_pics_dw.create
end on

on u_pics_dw.destroy
end on

event destructor;call super::destructor;of_SetDropDownFilter (false)
of_SEtDropDownRetrieve (false)
end event


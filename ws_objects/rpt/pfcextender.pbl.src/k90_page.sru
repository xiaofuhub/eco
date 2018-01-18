$PBExportHeader$k90_page.sru
$PBExportComments$(K90) - PFC Extender Ancestor to Wizard Pages
forward
global type k90_page from userobject
end type
end forward

shared variables
end variables

global type k90_page from userobject
string tag = "2337,1089"
integer width = 1701
integer height = 1152
long backcolor = 79416533
long tabtextcolor = 255
long tabbackcolor = 65535
long picturemaskcolor = 536870912
event type integer ue_getfocus ( readonly string as_direction )
event type integer ue_losefocus ( readonly string as_direction )
event ue_whatsthis ( readonly string as_text,  readonly integer ai_x,  readonly integer ai_y )
event type integer ue_print ( )
event type integer ue_finished ( )
event lbuttondown pbm_lbuttondown
event resize pbm_size
end type
global k90_page k90_page

type variables
/*  Parent window reference  */
k90_libextender iw_parent
/*  Object which should get focus  */
dragobject idrg_focus
/*  Page has been changed  */
boolean ib_changed
end variables

forward prototypes
public function boolean of_checkrequired ()
public function integer of_cleartext ()
public function integer of_addmessage (readonly string as_error, readonly integer ai_errorcd)
public function integer of_addmessage (readonly long al_row, readonly string as_error, readonly integer ai_errorcd)
end prototypes

event ue_getfocus;//*-------------------------------------------------------------------
//*  Event:				ue_getfocus
//*  Purpose:			Set Next/Back Buttons and Focus
//*-------------------------------------------------------------------
iw_parent.Event ue_next ( of_CheckRequired ( ) ) 
iw_parent.Event ue_back ( TRUE ) 

this.Enabled = TRUE

IF IsValid ( idrg_focus ) THEN idrg_focus.SetFocus ( )

Return 1
end event

event ue_losefocus;//*-------------------------------------------------------------------
//*  Event:				ue_losefocus
//*  Purpose:			Clear status field and Help Text
//*-------------------------------------------------------------------

//*  iw_parent.of_Set...  functions to store data
//*  need in subsequent steps.

iw_parent.Event ue_setinfo ( "" )
//Return of_ClearText ( )
Return 1
end event

event lbuttondown;//*-------------------------------------------------------------------
//*  Event:				lbuttondown
//*  Purpose:			Clear Help Text
//*-------------------------------------------------------------------
//of_ClearText ( )
end event

public function boolean of_checkrequired ();//*-------------------------------------------------------------------
//*  Function:			of_CheckRequired
//*  Purpose:			Check required fields
//*-------------------------------------------------------------------

//*  Check all fields on the page.  If all fields
//*  have valid values, then return TRUE, else FALSE 
Return TRUE
end function

public function integer of_cleartext ();//*-------------------------------------------------------------------
//*  Function:			of_ClearText
//*  Purpose:			Clear the What's This Help Text
//*-------------------------------------------------------------------

//*   Set all statictext for What's This help,
//*	Visible = FALSE  
Return 1
end function

public function integer of_addmessage (readonly string as_error, readonly integer ai_errorcd);//*-------------------------------------------------------------------
//*  Function:			of_AddMessage
//*  Purpose:			Send processing message to uo
//*-------------------------------------------------------------------
Return -1
end function

public function integer of_addmessage (readonly long al_row, readonly string as_error, readonly integer ai_errorcd);//*-------------------------------------------------------------------
//*  Function:			of_AddMessage
//*  Purpose:			Send processing message to uo
//*-------------------------------------------------------------------
Return -1
end function

on k90_page.create
end on

on k90_page.destroy
end on

event constructor;//*-------------------------------------------------------------------
//*  Event:				constructor
//*  Purpose:			Get Reference to parent window
//*-------------------------------------------------------------------
/*  First time, data must change  */
ib_changed = TRUE

/*  Set control that should get focus first  */
//*idrg_focus = sle_library

iw_parent = this.GetParent ( )
end event

event destructor;//*-------------------------------------------------------------------
//*  Event:				destructor
//*  Purpose:			Clean-Up / Validation
//*-------------------------------------------------------------------
end event


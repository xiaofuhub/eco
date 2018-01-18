$PBExportHeader$base_progress.srw
$PBExportComments$(PB70Base) - Progress Bar
forward
global type base_progress from base_center
end type
type hpb_1 from u_progressbar within base_progress
end type
type st_1 from base_statictext within base_progress
end type
type cb_cancel from base_commandbutton within base_progress
end type
end forward

global type base_progress from base_center
integer width = 1298
integer height = 412
string title = "Please wait"
boolean controlmenu = false
boolean minbox = false
boolean maxbox = false
boolean resizable = false
windowtype windowtype = popup!
hpb_1 hpb_1
st_1 st_1
cb_cancel cb_cancel
end type
global base_progress base_progress

type variables
boolean ib_cancel
base_center iw_parent
end variables

forward prototypes
public function integer f_setrange (readonly long al_min, readonly long al_max)
public function boolean f_progress (readonly string as_object)
end prototypes

public function integer f_setrange (readonly long al_min, readonly long al_max);//*-----------------------------------------------------------------*/
//*    f_SetRange:  Establish min / max data points
//*-----------------------------------------------------------------*/
Return hpb_1.SetRange ( al_min, al_max )
end function

public function boolean f_progress (readonly string as_object);//*-----------------------------------------------------------------*/
//*    f_Progress: Update progress								   	*/
//*-----------------------------------------------------------------*/
st_1.Text = "Examining " + as_object

hpb_1.StepIt ( )

Return ib_cancel
end function

on base_progress.create
int iCurrent
call super::create
this.hpb_1=create hpb_1
this.st_1=create st_1
this.cb_cancel=create cb_cancel
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.hpb_1
this.Control[iCurrent+2]=this.st_1
this.Control[iCurrent+3]=this.cb_cancel
end on

on base_progress.destroy
call super::destroy
destroy(this.hpb_1)
destroy(this.st_1)
destroy(this.cb_cancel)
end on

event key;call super::key;//*-----------------------------------------------------------------*/
//*    key: Close dialog on Escape key							   	*/
//*-----------------------------------------------------------------*/
If key = KeyEscape! Then
	cb_cancel.Event Clicked ( )
End If
end event

event open;call super::open;//*-----------------------------------------------------------------*/
//*    open: Initialization										   	*/
//*-----------------------------------------------------------------*/
st_1.Text = Message.StringParm 

iw_parent = this.ParentWindow ( )

hpb_1.OffSetPos ( 0 )
hpb_1.SetStep = 1
end event

type hpb_1 from u_progressbar within base_progress
integer x = 32
integer y = 108
integer width = 1207
integer height = 64
unsignedinteger position = 0
integer setstep = 1
boolean smoothscroll = true
end type

type st_1 from base_statictext within base_progress
integer x = 32
integer y = 28
integer width = 1207
alignment alignment = center!
end type

type cb_cancel from base_commandbutton within base_progress
integer x = 466
integer y = 204
integer taborder = 10
string text = "Cancel"
end type

event clicked;//*-----------------------------------------------------------------*/
//*    clicked: Hide the dialog and cancel the process			   	*/
//*-----------------------------------------------------------------*/
ib_cancel = True

If IsValid ( iw_parent ) Then iw_parent.Dynamic Event ue_cancel ( ib_cancel )
Parent.Hide ( )
end event


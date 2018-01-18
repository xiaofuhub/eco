$PBExportHeader$w_master.srw
$PBExportComments$Extension Master Window class
forward
global type w_master from pfc_w_master
end type
end forward

global type w_master from pfc_w_master
end type
global w_master w_master

type variables
n_cst_winsrv_jaws inv_jaws
end variables

forward prototypes
public function integer of_setjaws (boolean ab_switch)
public function boolean of_isactivedatawindow (datawindow adw_control)
end prototypes

public function integer of_setjaws (boolean ab_switch);//////////////////////////////////////////////////////////////////////////////
//
//	Event:  of_SetJaws
//
//	Arguments:
//	ab_switch   starts/stops the window Jaws service
//
//	Returns:  integer
//	 1 = Successful operation.
//	 0 = No action necessary
//	-1 = An error was encountered
//
//	Description:
//	Starts or stops the window Jaws service
//
//=============================================================================

integer	li_rc

// Check arguments.
if IsNull (ab_switch) then return -1

if ab_Switch then
	if IsNull(inv_Jaws) Or not IsValid (inv_Jaws) then
		inv_Jaws = create n_cst_winsrv_Jaws
		inv_Jaws.of_SetRequestor (this)
		li_rc = 1
	end if
else
	if IsValid (inv_Jaws) then
		destroy inv_Jaws
		li_rc = 1
	end if
end if

return li_rc


end function

public function boolean of_isactivedatawindow (datawindow adw_control);Return (adw_control = idw_active)
end function

on w_master.create
call super::create
end on

on w_master.destroy
call super::destroy
end on

event pfc_postopen;call super::pfc_postopen;if IsValid(inv_jaws) then inv_jaws.of_reread()
end event

event pfc_controlgotfocus;call super::pfc_controlgotfocus;if IsValid(inv_jaws) then inv_jaws.event ue_controlgotfocus (adrg_control)
end event

event close;call super::close;of_SetJaws (False)
end event


$PBExportHeader$base_commandbutton.sru
$PBExportComments$(PB70Base) - CommandButton control, has popup help logic
forward
global type base_commandbutton from commandbutton
end type
end forward

global type base_commandbutton from commandbutton
integer width = 334
integer height = 92
integer textsize = -8
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
string text = "none"
end type
global base_commandbutton base_commandbutton

event help;//*-----------------------------------------------------------------*/
//*    help:  Invoke Context-Sensitive Help
//*-----------------------------------------------------------------*/
//base_wizard lw_parent
//
//lw_parent = this.GetParent ( ).GetParent ( ) 
//
//If IsValid ( lw_parent ) Then lw_parent.f_ShowHelp ( this ) 
end event

on base_commandbutton.create
end on

on base_commandbutton.destroy
end on


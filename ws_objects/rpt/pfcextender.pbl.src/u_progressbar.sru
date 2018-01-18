$PBExportHeader$u_progressbar.sru
$PBExportComments$(PB70Base) - Progress Bar UO
forward
global type u_progressbar from hprogressbar
end type
end forward

global type u_progressbar from hprogressbar
unsignedinteger maxposition = 100
unsignedinteger position = 50
integer setstep = 10
integer width = 475
integer height = 52
event ue_progress ( )
end type
global u_progressbar u_progressbar

event ue_progress;//*-----------------------------------------------------------------*/
//*    ue_progress:  Increment progress bar
//*-----------------------------------------------------------------*/
this.StepIt ( )
end event


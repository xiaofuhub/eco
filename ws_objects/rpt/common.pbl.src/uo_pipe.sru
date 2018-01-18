$PBExportHeader$uo_pipe.sru
$PBExportComments$PICS customized pipe
forward
global type uo_pipe from pipeline
end type
end forward

global type uo_pipe from pipeline
end type
global uo_pipe uo_pipe

event pipestart;//.Text = "Beginning Pipeline Execution ..."
end event

on uo_pipe.create
call pipeline::create
TriggerEvent( this, "constructor" )
end on

on uo_pipe.destroy
call pipeline::destroy
TriggerEvent( this, "destructor" )
end on


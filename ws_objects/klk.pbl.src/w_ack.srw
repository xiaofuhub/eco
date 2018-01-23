$PBExportHeader$w_ack.srw
forward
global type w_ack from window
end type
end forward

global type w_ack from window
integer width = 3959
integer height = 1648
boolean titlebar = true
string title = "Untitled"
boolean controlmenu = true
boolean minbox = true
boolean maxbox = true
boolean resizable = true
long backcolor = 67108864
string icon = "AppIcon!"
boolean center = true
end type
global w_ack w_ack

event open;//08
end event

on w_ack.create
end on

on w_ack.destroy
end on


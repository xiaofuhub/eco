$PBExportHeader$k90_promoter.sru
$PBExportComments$(K90) - PFC Extender Scrollbar object used for moving prefixes
forward
global type k90_promoter from userobject
end type
type p_down from picture within k90_promoter
end type
type p_up from picture within k90_promoter
end type
end forward

global type k90_promoter from userobject
integer width = 82
integer height = 112
long backcolor = 79416533
long tabtextcolor = 33554432
long picturemaskcolor = 536870912
event type integer buttonclicked ( readonly boolean ab_up )
p_down p_down
p_up p_up
end type
global k90_promoter k90_promoter

type variables
end variables

event buttonclicked;IF ab_up THEN
	Return this.GetParent ( ).TriggerEvent ( "ue_up" ) 
ELSE
	Return this.GetParent ( ).TriggerEvent ( "ue_down" ) 
END IF
end event

on k90_promoter.create
this.p_down=create p_down
this.p_up=create p_up
this.Control[]={this.p_down,&
this.p_up}
end on

on k90_promoter.destroy
destroy(this.p_down)
destroy(this.p_up)
end on

type p_down from picture within k90_promoter
integer y = 52
integer width = 73
integer height = 52
boolean originalsize = true
string picturename = "down.bmp"
boolean focusrectangle = false
end type

event clicked;Parent.Event buttonclicked ( FALSE )
end event

type p_up from picture within k90_promoter
integer width = 73
integer height = 52
boolean originalsize = true
string picturename = "up.bmp"
boolean focusrectangle = false
end type

event clicked;Parent.Event buttonclicked ( TRUE )
end event


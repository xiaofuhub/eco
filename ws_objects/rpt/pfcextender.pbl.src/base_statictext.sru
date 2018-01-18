$PBExportHeader$base_statictext.sru
$PBExportComments$(PB70Base) - StaticText control, for proper sizing, fonts, etc.
forward
global type base_statictext from statictext
end type
end forward

global type base_statictext from statictext
integer textsize = -8
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
long backcolor = 67108864
boolean enabled = false
string text = "none"
borderstyle borderstyle = stylelowered!
boolean focusrectangle = false
integer width = 1385
integer height = 76
end type
global base_statictext base_statictext


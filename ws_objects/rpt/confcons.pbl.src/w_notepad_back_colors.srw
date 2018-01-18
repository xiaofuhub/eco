$PBExportHeader$w_notepad_back_colors.srw
forward
global type w_notepad_back_colors from Window
end type
type cb_cancel from commandbutton within w_notepad_back_colors
end type
type cb_ok from commandbutton within w_notepad_back_colors
end type
type uo_colors from u_color_selection within w_notepad_back_colors
end type
end forward

global type w_notepad_back_colors from Window
int X=961
int Y=573
int Width=1002
int Height=773
boolean TitleBar=true
string Title="Change Background Color"
long BackColor=79741120
boolean ControlMenu=true
ToolBarAlignment ToolBarAlignment=AlignAtLeft!
WindowType WindowType=response!
cb_cancel cb_cancel
cb_ok cb_ok
uo_colors uo_colors
end type
global w_notepad_back_colors w_notepad_back_colors

type variables
MultiLineEdit   imle_parm
end variables

event open;long	ll_fillcolor


//////////////////////////////////////////////////////////////////////
// Obtain the MultiLineEdit from the message object
//////////////////////////////////////////////////////////////////////
imle_parm = message.powerobjectparm
ll_fillcolor = imle_parm.BackColor


//////////////////////////////////////////////////////////////////////
// Initialize the user object color changer to the current text color of the
// MultiLineEdit.
//////////////////////////////////////////////////////////////////////
uo_colors.uf_set_rgb (ll_fillcolor)


end event

on w_notepad_back_colors.create
this.cb_cancel=create cb_cancel
this.cb_ok=create cb_ok
this.uo_colors=create uo_colors
this.Control[]={ this.cb_cancel,&
this.cb_ok,&
this.uo_colors}
end on

on w_notepad_back_colors.destroy
destroy(this.cb_cancel)
destroy(this.cb_ok)
destroy(this.uo_colors)
end on

type cb_cancel from commandbutton within w_notepad_back_colors
int X=522
int Y=533
int Width=243
int Height=109
int TabOrder=30
string Text="Cancel"
boolean Cancel=true
int TextSize=-9
int Weight=400
string FaceName="MS Sans Serif"
FontFamily FontFamily=Swiss!
FontPitch FontPitch=Variable!
end type

on clicked;close (parent)
end on

type cb_ok from commandbutton within w_notepad_back_colors
int X=225
int Y=533
int Width=243
int Height=109
int TabOrder=20
string Text="OK"
boolean Default=true
int TextSize=-9
int Weight=400
string FaceName="MS Sans Serif"
FontFamily FontFamily=Swiss!
FontPitch FontPitch=Variable!
end type

event clicked;// Set the text color to the RGB value specified by the user object

imle_parm.backcolor = uo_colors.uf_get_rgb()
close (parent)
end event

type uo_colors from u_color_selection within w_notepad_back_colors
int X=110
int Y=37
int TabOrder=10
boolean Border=false
BorderStyle BorderStyle=StyleBox!
end type

on uo_colors.destroy
call u_color_selection::destroy
end on


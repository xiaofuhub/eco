$PBExportHeader$k90_page4.sru
$PBExportComments$(K90) - PFC Extender Step 4:  Specify customization options
forward
global type k90_page4 from k90_page
end type
type rb_leave from radiobutton within k90_page4
end type
type st_leavehelp from statictext within k90_page4
end type
type rb_move from radiobutton within k90_page4
end type
type st_movehelp from statictext within k90_page4
end type
type gb_options from groupbox within k90_page4
end type
end forward

global type k90_page4 from k90_page
string tag = "Specify How to Treat Customizations"
rb_leave rb_leave
st_leavehelp st_leavehelp
rb_move rb_move
st_movehelp st_movehelp
gb_options gb_options
end type
global k90_page4 k90_page4

type variables
end variables

forward prototypes
end prototypes

event ue_getfocus;call super::ue_getfocus;//*-------------------------------------------------------------------
//*  Event:				ue_getfocus
//*  Purpose:			Populate the statictext info fields
//*-------------------------------------------------------------------
int li_i, li_max
string ls_hierarchy[]

SetPointer ( HourGlass! )

rb_leave.Checked = Not ( iw_parent.ib_PFEIsEmpty )
rb_move.Checked = iw_parent.ib_PFEIsEmpty

/*  Reset text */
rb_leave.Text = "Lea&ve customizations at the @old level"
rb_move.Text = "Pro&mote customizations to @new level"

/*  Change the statictext to reflect the prefix names  */	
rb_leave.Text = iw_parent.inv_common.f_GlobalReplace ( rb_leave.Text, "@old", iw_parent.is_dropprefix, True ) 
rb_move.Text = iw_parent.inv_common.f_GlobalReplace ( rb_move.Text, "@new", iw_parent.is_newprefix, True ) 
st_leavehelp.Text = iw_parent.inv_common.f_GlobalReplace ( st_leavehelp.Text, "@old", iw_parent.is_dropprefix, True ) 
st_leavehelp.Text = iw_parent.inv_common.f_GlobalReplace ( st_leavehelp.Text, "@new", iw_parent.is_newprefix, True ) 
st_movehelp.Text = iw_parent.inv_common.f_GlobalReplace ( st_movehelp.Text, "@old", iw_parent.is_dropprefix, True ) 
st_movehelp.Text = iw_parent.inv_common.f_GlobalReplace ( st_movehelp.Text, "@new", iw_parent.is_newprefix, True ) 

Return 1
end event

event ue_losefocus;call super::ue_losefocus;//*-------------------------------------------------------------------
//*  Event:				ue_losefocus
//*  Purpose:			Save the option chosen
//*-------------------------------------------------------------------
Return iw_parent.of_SetCustomOptions ( rb_move.Checked )
end event

on k90_page4.create
int iCurrent
call super::create
this.rb_leave=create rb_leave
this.st_leavehelp=create st_leavehelp
this.rb_move=create rb_move
this.st_movehelp=create st_movehelp
this.gb_options=create gb_options
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.rb_leave
this.Control[iCurrent+2]=this.st_leavehelp
this.Control[iCurrent+3]=this.rb_move
this.Control[iCurrent+4]=this.st_movehelp
this.Control[iCurrent+5]=this.gb_options
end on

on k90_page4.destroy
call super::destroy
destroy(this.rb_leave)
destroy(this.st_leavehelp)
destroy(this.rb_move)
destroy(this.st_movehelp)
destroy(this.gb_options)
end on

event constructor;call super::constructor;//*-------------------------------------------------------------------
//*  Event:				constructor
//*  Purpose:			Initialization
//*-------------------------------------------------------------------
/*  Set focus object  */
idrg_focus = rb_leave
end event

type rb_leave from radiobutton within k90_page4
integer textsize = -8
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long backcolor = 79416533
string text = "Lea&ve customizations at the @old level"
boolean checked = true
integer x = 174
integer y = 216
integer width = 1431
integer height = 88
integer taborder = 10
boolean bringtotop = true
end type

type st_leavehelp from statictext within k90_page4
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 33554432
long backcolor = 67108864
boolean enabled = false
string text = "(This means that the @new extensions will be created as empty.   Any customizations you~'ve made will remain in the @old level pbls.)"
boolean focusrectangle = false
integer x = 247
integer y = 304
integer width = 1303
integer height = 176
boolean bringtotop = true
end type

type rb_move from radiobutton within k90_page4
integer textsize = -8
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long backcolor = 79416533
string text = "Pro&mote customizations to @new level"
integer x = 174
integer y = 584
integer width = 1422
integer height = 72
integer taborder = 20
boolean bringtotop = true
end type

type st_movehelp from statictext within k90_page4
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 33554432
long backcolor = 67108864
boolean enabled = false
string text = "(This means that your customizations will be moved from the @old level to the @new level.  The @old level will then be recreated as an empty level.)"
boolean focusrectangle = false
integer x = 256
integer y = 668
integer width = 1303
integer height = 172
boolean bringtotop = true
end type

type gb_options from groupbox within k90_page4
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 33554432
long backcolor = 67108864
string text = "Customization Options"
borderstyle borderstyle = stylelowered!
integer x = 41
integer y = 20
integer width = 1605
integer height = 1092
end type


$PBExportHeader$w_library_painter_helper.srw
$PBExportComments$(PTT) - Invisible  window that acts as "helper" window for u_library_painter object
forward
global type w_library_painter_helper from window
end type
type lb_test from listbox within w_library_painter_helper
end type
type st_1 from statictext within w_library_painter_helper
end type
type lb_files from listbox within w_library_painter_helper
end type
type lb_dir from listbox within w_library_painter_helper
end type
type lb_drives from listbox within w_library_painter_helper
end type
end forward

global type w_library_painter_helper from window
boolean visible = false
integer x = 1074
integer y = 484
integer width = 1522
integer height = 588
boolean enabled = false
windowtype windowtype = child!
long backcolor = 79416533
lb_test lb_test
st_1 st_1
lb_files lb_files
lb_dir lb_dir
lb_drives lb_drives
end type
global w_library_painter_helper w_library_painter_helper

on w_library_painter_helper.create
this.lb_test=create lb_test
this.st_1=create st_1
this.lb_files=create lb_files
this.lb_dir=create lb_dir
this.lb_drives=create lb_drives
this.Control[]={this.lb_test,&
this.st_1,&
this.lb_files,&
this.lb_dir,&
this.lb_drives}
end on

on w_library_painter_helper.destroy
destroy(this.lb_test)
destroy(this.st_1)
destroy(this.lb_files)
destroy(this.lb_dir)
destroy(this.lb_drives)
end on

type lb_test from listbox within w_library_painter_helper
integer x = 987
integer y = 376
integer width = 494
integer height = 360
integer taborder = 4
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
boolean hscrollbar = true
boolean vscrollbar = true
borderstyle borderstyle = stylelowered!
end type

type st_1 from statictext within w_library_painter_helper
integer x = 165
integer y = 384
integer width = 1243
integer height = 64
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
long backcolor = 67108864
boolean enabled = false
string text = "none"
boolean focusrectangle = false
end type

type lb_files from listbox within w_library_painter_helper
integer x = 987
integer width = 494
integer height = 360
integer taborder = 10
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
boolean hscrollbar = true
boolean vscrollbar = true
borderstyle borderstyle = stylelowered!
end type

type lb_dir from listbox within w_library_painter_helper
integer x = 494
integer width = 494
integer height = 360
integer taborder = 30
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
boolean hscrollbar = true
boolean vscrollbar = true
borderstyle borderstyle = stylelowered!
end type

type lb_drives from listbox within w_library_painter_helper
integer width = 494
integer height = 360
integer taborder = 20
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
boolean hscrollbar = true
boolean vscrollbar = true
borderstyle borderstyle = stylelowered!
end type


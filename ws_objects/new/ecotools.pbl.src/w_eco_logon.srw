﻿$PBExportHeader$w_eco_logon.srw
$PBExportComments$Extension Logon window
forward
global type w_eco_logon from pfc_w_logon
end type
type st_pics from statictext within w_eco_logon
end type
type cb_sync from u_cb within w_eco_logon
end type
type ole_sync from olecustomcontrol within w_eco_logon
end type
type st_version from statictext within w_eco_logon
end type
end forward

global type w_eco_logon from pfc_w_logon
integer x = 402
integer y = 20
integer width = 2057
integer height = 1784
st_pics st_pics
cb_sync cb_sync
ole_sync ole_sync
st_version st_version
end type
global w_eco_logon w_eco_logon

on w_eco_logon.create
int iCurrent
call super::create
this.st_pics=create st_pics
this.cb_sync=create cb_sync
this.ole_sync=create ole_sync
this.st_version=create st_version
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_pics
this.Control[iCurrent+2]=this.cb_sync
this.Control[iCurrent+3]=this.ole_sync
this.Control[iCurrent+4]=this.st_version
end on

on w_eco_logon.destroy
call super::destroy
destroy(this.st_pics)
destroy(this.cb_sync)
destroy(this.ole_sync)
destroy(this.st_version)
end on

event open;call super::open;sle_userid.SetFocus()
this.of_SetBase(TRUE)
this.inv_base.of_Center()
st_version.text = 'Version ' + gnv_app.of_GetVersion()

end event

type p_logo from pfc_w_logon`p_logo within w_eco_logon
integer y = 344
end type

type st_help from pfc_w_logon`st_help within w_eco_logon
integer x = 27
integer y = 160
integer width = 1989
integer height = 112
integer textsize = -10
string text = "Enter a User ID and Password to log onto "
alignment alignment = center!
end type

type cb_ok from pfc_w_logon`cb_ok within w_eco_logon
integer x = 613
integer y = 1492
integer height = 132
integer taborder = 40
integer textsize = -10
string text = "&OK"
end type

type cb_cancel from pfc_w_logon`cb_cancel within w_eco_logon
integer x = 1019
integer y = 1492
integer height = 132
integer taborder = 60
integer textsize = -10
string text = "&Cancel"
end type

type sle_userid from pfc_w_logon`sle_userid within w_eco_logon
integer x = 942
integer y = 912
integer width = 398
integer height = 92
integer textsize = -10
end type

type sle_password from pfc_w_logon`sle_password within w_eco_logon
integer x = 942
integer y = 1036
integer width = 398
integer height = 92
integer textsize = -10
end type

type st_2 from pfc_w_logon`st_2 within w_eco_logon
integer x = 539
integer y = 920
integer width = 343
integer textsize = -10
alignment alignment = left!
end type

type st_3 from pfc_w_logon`st_3 within w_eco_logon
integer x = 539
integer y = 1040
integer width = 311
integer textsize = -10
alignment alignment = left!
end type

type st_pics from statictext within w_eco_logon
integer x = 32
integer y = 32
integer width = 1984
integer height = 104
integer textsize = -18
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 16711680
long backcolor = 79741120
boolean enabled = false
string text = "Welcome  to NLS"
alignment alignment = center!
boolean focusrectangle = false
end type

type cb_sync from u_cb within w_eco_logon
boolean visible = false
integer x = 759
integer y = 1504
integer width = 594
integer height = 132
integer taborder = 50
integer textsize = -10
string text = "&Synchronize"
boolean cancel = true
end type

event clicked;call super::clicked;ole_sync.object.Execute()

	


end event

type ole_sync from olecustomcontrol within w_eco_logon
boolean visible = false
integer x = 325
integer y = 1640
integer width = 1317
integer height = 768
integer taborder = 10
boolean bringtotop = true
boolean focusrectangle = false
string binarykey = "w_eco_logon.win"
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 33554432
end type

type st_version from statictext within w_eco_logon
integer x = 818
integer y = 1288
integer width = 503
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 33554432
long backcolor = 67108864
boolean focusrectangle = false
end type


Start of PowerBuilder Binary Data Section : Do NOT Edit
02w_eco_logon.bin 
2700000c00e011cfd0e11ab1a1000000000000000000000000000000000003003e0009fffe000000060000000000000000000000010000000100000000000010000000000200000001fffffffe0000000000000000fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffd00000004fffffffefffffffefffffffeffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff006f00520074006f004500200074006e00790072000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000050016ffffffffffffffff00000001000000000000000000000000000000000000000000000000000000001377f91001ca66d400000003000000800000000000500003004f0042005800430054005300450052004d0041000000000000000000000000000000000000000000000000000000000000000000000000000000000102001affffffff00000002ffffffff000000000000000000000000000000000000000000000000000000000000000000000000fffffffe0000000000000000004200500043004f00530058004f00540041005200450047000000000000000000000000000000000000000000000000000000000000000000000000000000000001001affffffffffffffff00000003eae95fc611d0a6ecc000d1a5ff1910e3000000001377f91001ca66d41377f91001ca66d4000000000000000000000000004f00010065006c00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000102000affffffff00000004ffffffff000000000000000000000000000000000000000000000000000000000000000000000000000000000000001400000000fffffffefffffffeffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
22ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff0200000100000008000000000000000000000000001305700000000200000000000000030000000100000001000000010000000200000000000000030000000100010000000005600000052b000000005c3a631373636970705c3233327363696379732e00000001000000050000000100000000000000050000000100000000000000050000000100000000000000050000000100000000000000050000000100000000000000050000000100000000000000050000000100000001000000010000000200000000000000030000000100000001000000060000000100000000000000070000000100000001000000060000000100000000000000040000000100000000000000090000000100000000000000060000000100000000000000050000000100000000000000060000000100000000000000060000000100000000000000050000000100000000000000050000000100000000000000060000000100000000000000060000000100000000000000030000000100000000000000040000000100000000000000050000000100000000000000040000000100000000000000070000000100000000000000060000000100000000000000070000000100000000000000050000000100000000000000070000000100000000000000060000000100000000000000050000000100000000000000050000000100000000006f00430074006e006e00650073007400000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000020012ffffffffffffffffffffffff0000000000000000000000000000000000000000000000000000000000000000000000000000000100000025000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000ffffffffffffffffffffffff0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000ffffffffffffffffffffffff0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000ffffffffffffffffffffffff00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
12w_eco_logon.bin 
End of PowerBuilder Binary Data Section : No Source Expected After This Point

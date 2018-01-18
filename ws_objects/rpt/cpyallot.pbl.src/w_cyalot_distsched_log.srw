$PBExportHeader$w_cyalot_distsched_log.srw
forward
global type w_cyalot_distsched_log from w_main
end type
type cb_alot from commandbutton within w_cyalot_distsched_log
end type
type cb_distsched from commandbutton within w_cyalot_distsched_log
end type
type cb_exit from commandbutton within w_cyalot_distsched_log
end type
type dw_copy_allotment_log from u_pics_dw within w_cyalot_distsched_log
end type
type dw_distsched_log from u_pics_dw within w_cyalot_distsched_log
end type
end forward

global type w_cyalot_distsched_log from w_main
integer width = 3557
integer height = 1664
string title = "Create Distribution Schedule File"
event ue_enterkey pbm_dwnprocessenter
cb_alot cb_alot
cb_distsched cb_distsched
cb_exit cb_exit
dw_copy_allotment_log dw_copy_allotment_log
dw_distsched_log dw_distsched_log
end type
global w_cyalot_distsched_log w_cyalot_distsched_log

type variables
str_distrib_schedule istr
datastore ids_distsched
long i_count=0, i_books, i_libs, i_distfilerows, i_prdr
string is_yes_no='N'
boolean ib_ask_yn= false
end variables

forward prototypes
public function string wf_remove_chars (string sndx_in)
end prototypes

event ue_enterkey;//Send(Handle(this),256,9,Long(0,0))
//return(1)
end event

public function string wf_remove_chars (string sndx_in);//************************************************************
//wf_remove_chars
//************************************************************
 
string  sndx_out
integer  i_len, idx, i, j, alpha_char, li_pos
boolean  b_exit = FALSE
n_cst_string 	inv_string

sndx_in =  inv_string.of_GlobalReplace(sndx_in, "[braille]", "")
sndx_in =  inv_string.of_GlobalReplace(sndx_in, "[sound recording]", "")
//sndx_in =  inv_string.of_GlobalReplace(sndx_in, "(et al.)", "")
//sndx_in =  inv_string.of_GlobalReplace(sndx_in, ",", "")
sndx_in =  inv_string.of_GlobalReplace(sndx_in, "#", "")
sndx_in =  inv_string.of_GlobalReplace(sndx_in, ";", "")
sndx_in =  inv_string.of_GlobalReplace(sndx_in, "/", "")
sndx_in =  inv_string.of_GlobalReplace(sndx_in, "\", "")
sndx_in =  inv_string.of_GlobalReplace(sndx_in, "|", "")
sndx_in =  inv_string.of_GlobalReplace(sndx_in, "~~", "")
//sndx_in =  inv_string.of_GlobalReplace(sndx_in, "(", "")
//sndx_in =  inv_string.of_GlobalReplace(sndx_in, ")", "")
//sndx_in =  inv_string.of_GlobalReplace(sndx_in, "{", "")
//sndx_in =  inv_string.of_GlobalReplace(sndx_in, "}", "")
sndx_in =  inv_string.of_GlobalReplace(sndx_in, "+", "")
//sndx_in =  inv_string.of_GlobalReplace(sndx_in, "-", "")
sndx_in =  inv_string.of_GlobalReplace(sndx_in, "_", "")
sndx_in =  inv_string.of_GlobalReplace(sndx_in, "=", "")
sndx_in =  inv_string.of_GlobalReplace(sndx_in, "/", "")
sndx_in =  inv_string.of_GlobalReplace(sndx_in, "\\", "")
//sndx_in =  inv_string.of_GlobalReplace(sndx_in, "'", "")

sndx_out = sndx_in

RETURN sndx_out
 
 

end function

on w_cyalot_distsched_log.create
int iCurrent
call super::create
this.cb_alot=create cb_alot
this.cb_distsched=create cb_distsched
this.cb_exit=create cb_exit
this.dw_copy_allotment_log=create dw_copy_allotment_log
this.dw_distsched_log=create dw_distsched_log
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.cb_alot
this.Control[iCurrent+2]=this.cb_distsched
this.Control[iCurrent+3]=this.cb_exit
this.Control[iCurrent+4]=this.dw_copy_allotment_log
this.Control[iCurrent+5]=this.dw_distsched_log
end on

on w_cyalot_distsched_log.destroy
call super::destroy
destroy(this.cb_alot)
destroy(this.cb_distsched)
destroy(this.cb_exit)
destroy(this.dw_copy_allotment_log)
destroy(this.dw_distsched_log)
end on

event open;call super::open;
this.of_SetBase(true)
this.inv_base.of_Center()

end event

event pfc_postopen();call super::pfc_postopen;long li_row, li_count

dw_copy_allotment_log.Retrieve()
dw_distsched_log.Retrieve()
dw_distsched_log.visible=false
end event

event pfc_preopen();call super::pfc_preopen;this.windowstate = maximized!

this.of_SetResize(TRUE)

inv_resize.of_Register(dw_copy_allotment_log, "Scale")
inv_resize.of_Register(dw_distsched_log, "Scale")
inv_resize.of_Register(cb_alot, "Scale")
inv_resize.of_Register(cb_distsched, "Scale")
inv_resize.of_Register(cb_exit, "Scale")

end event

type cb_alot from commandbutton within w_cyalot_distsched_log
integer x = 955
integer y = 1324
integer width = 384
integer height = 96
integer taborder = 150
boolean bringtotop = true
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "&Copy Allot Log"
boolean cancel = true
end type

event clicked;

dw_copy_allotment_log.visible=true
dw_distsched_log.visible=false

end event

type cb_distsched from commandbutton within w_cyalot_distsched_log
integer x = 1632
integer y = 1328
integer width = 407
integer height = 96
integer taborder = 250
boolean bringtotop = true
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "&Dist Sched Log"
boolean cancel = true
end type

event clicked;

dw_copy_allotment_log.visible=false
dw_distsched_log.visible=true
end event

type cb_exit from commandbutton within w_cyalot_distsched_log
integer x = 2409
integer y = 1332
integer width = 247
integer height = 96
integer taborder = 140
boolean bringtotop = true
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "&Exit"
boolean cancel = true
end type

event clicked;
close(parent)
end event

type dw_copy_allotment_log from u_pics_dw within w_cyalot_distsched_log
integer x = 55
integer y = 48
integer width = 3401
integer height = 1120
integer taborder = 210
string dataobject = "d_copy_allotment_log"
boolean hscrollbar = true
boolean hsplitscroll = true
end type

event constructor;call super::constructor;THIS.settransobject(sqlservertrans)

end event

type dw_distsched_log from u_pics_dw within w_cyalot_distsched_log
boolean visible = false
integer x = 69
integer y = 56
integer width = 3401
integer height = 1128
integer taborder = 220
string dataobject = "d_distsched_log"
boolean hscrollbar = true
boolean hsplitscroll = true
end type

event constructor;call super::constructor;THIS.settransobject(sqlservertrans)

end event


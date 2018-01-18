$PBExportHeader$w_cmpare_orcl_infx_qastg.srw
forward
global type w_cmpare_orcl_infx_qastg from w_main
end type
type st_total_inx_orc from statictext within w_cmpare_orcl_infx_qastg
end type
type cb_prt from commandbutton within w_cmpare_orcl_infx_qastg
end type
type cb_orc from commandbutton within w_cmpare_orcl_infx_qastg
end type
type dw_qastg_inx_orc from u_pics_dw within w_cmpare_orcl_infx_qastg
end type
type st_total_inx from statictext within w_cmpare_orcl_infx_qastg
end type
type st_total_orc from statictext within w_cmpare_orcl_infx_qastg
end type
type st_process from statictext within w_cmpare_orcl_infx_qastg
end type
type uo_progress from u_progressbar within w_cmpare_orcl_infx_qastg
end type
type cb_inxorc from commandbutton within w_cmpare_orcl_infx_qastg
end type
type cb_inx from commandbutton within w_cmpare_orcl_infx_qastg
end type
type cb_cancel from commandbutton within w_cmpare_orcl_infx_qastg
end type
type dw_qastg_orc from u_pics_dw within w_cmpare_orcl_infx_qastg
end type
type dw_qastg_inx from u_pics_dw within w_cmpare_orcl_infx_qastg
end type
end forward

global type w_cmpare_orcl_infx_qastg from w_main
integer width = 3557
integer height = 1664
string title = "Create Distribution Schedule File"
event ue_enterkey pbm_dwnprocessenter
st_total_inx_orc st_total_inx_orc
cb_prt cb_prt
cb_orc cb_orc
dw_qastg_inx_orc dw_qastg_inx_orc
st_total_inx st_total_inx
st_total_orc st_total_orc
st_process st_process
uo_progress uo_progress
cb_inxorc cb_inxorc
cb_inx cb_inx
cb_cancel cb_cancel
dw_qastg_orc dw_qastg_orc
dw_qastg_inx dw_qastg_inx
end type
global w_cmpare_orcl_infx_qastg w_cmpare_orcl_infx_qastg

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

on w_cmpare_orcl_infx_qastg.create
int iCurrent
call super::create
this.st_total_inx_orc=create st_total_inx_orc
this.cb_prt=create cb_prt
this.cb_orc=create cb_orc
this.dw_qastg_inx_orc=create dw_qastg_inx_orc
this.st_total_inx=create st_total_inx
this.st_total_orc=create st_total_orc
this.st_process=create st_process
this.uo_progress=create uo_progress
this.cb_inxorc=create cb_inxorc
this.cb_inx=create cb_inx
this.cb_cancel=create cb_cancel
this.dw_qastg_orc=create dw_qastg_orc
this.dw_qastg_inx=create dw_qastg_inx
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_total_inx_orc
this.Control[iCurrent+2]=this.cb_prt
this.Control[iCurrent+3]=this.cb_orc
this.Control[iCurrent+4]=this.dw_qastg_inx_orc
this.Control[iCurrent+5]=this.st_total_inx
this.Control[iCurrent+6]=this.st_total_orc
this.Control[iCurrent+7]=this.st_process
this.Control[iCurrent+8]=this.uo_progress
this.Control[iCurrent+9]=this.cb_inxorc
this.Control[iCurrent+10]=this.cb_inx
this.Control[iCurrent+11]=this.cb_cancel
this.Control[iCurrent+12]=this.dw_qastg_orc
this.Control[iCurrent+13]=this.dw_qastg_inx
end on

on w_cmpare_orcl_infx_qastg.destroy
call super::destroy
destroy(this.st_total_inx_orc)
destroy(this.cb_prt)
destroy(this.cb_orc)
destroy(this.dw_qastg_inx_orc)
destroy(this.st_total_inx)
destroy(this.st_total_orc)
destroy(this.st_process)
destroy(this.uo_progress)
destroy(this.cb_inxorc)
destroy(this.cb_inx)
destroy(this.cb_cancel)
destroy(this.dw_qastg_orc)
destroy(this.dw_qastg_inx)
end on

event open;call super::open;
this.of_SetBase(true)
this.inv_base.of_Center()

end event

event pfc_postopen;call super::pfc_postopen;long li_count, i,j, k, li_bkseq, li_cur, li_count2, li_row, li_re, rtn=0
string ls_med,  ls_cntr,ls_cntr_orc, ls_med_orc, ls_stg, ls_stg_orc, ls_stat, ls_stat_orc,&
		ls_null
long li_seq, li_seq_orc	
date ld_recdt, ld_recdt_orc, ld_comdt, ld_comdt_orc, ld_null		
boolean lb_find=false
This.windowstate = maximized!

SetNull(ls_null)
setNull(ld_null)
String ls_msgtext[ ] = {" 0 %","10 %", "20 %", "30 %", "40 %", "50 %", "60 %", &
									"70 %", "80 %", "90 %", "100 %"}
uo_progress.of_SetTextColor(RGB(255, 255, 255))	
st_process.text='Retrieving data from Oracle...'
select count(*) into :li_count
from prdrqastg
using SqlServerOracleTrans;
if f_check_dberror(SqlServerTrans,'select count(*) from prdrqastg')=false then
	return
//	messagebox('li_re','count = '+string(li_re))
end if
if li_count=0 then
	messagebox('Error','There is no records in oracle database, '+&
		'~nContact database administrator.')
	return
end if
this.SetRedraw(true)
uo_progress.Visible=TRUE
uo_progress.of_SetMinimum(0)
uo_progress.of_SetMaximum(li_count * 0.1)
uo_progress.of_SetDisplayStyle(3)
uo_progress.of_SetMessageText(ls_msgtext)
uo_progress.of_SetPosition(0)

SetPointer(HourGlass!)
li_count=dw_qastg_orc.Retrieve()
st_total_orc.text='Total row in oracle: '+string(li_count)

st_process.text='Retieve data from PICS...'

select count(*) into :li_count
from qastg
using SqlServerTrans;
if f_check_dberror(SqlServerTrans,'select count(*) from qastg')=false then
	return
//	messagebox('li_re','count = '+string(li_re))
end if
if li_count=0 then
	messagebox('Error','There is no records in PICS database, '+&
		'~nContact database administrator.')
	return
end if
this.SetRedraw(true)
uo_progress.Visible=TRUE
uo_progress.of_SetMinimum(0)
uo_progress.of_SetMaximum(li_count * 0.1)
uo_progress.of_SetDisplayStyle(3)
uo_progress.of_SetMessageText(ls_msgtext)
uo_progress.of_SetPosition(0)

li_row=dw_qastg_inx.Retrieve()
st_total_inx.text='Total row in PICS: '+string(li_row)
li_count=dw_qastg_orc.RowCount()
li_row=dw_qastg_inx.RowCount()
this.SetRedraw(true)
uo_progress.Visible=TRUE
uo_progress.of_SetMinimum(0)
uo_progress.of_SetMaximum(li_count * 00.1)
uo_progress.of_SetDisplayStyle(3)
uo_progress.of_SetMessageText(ls_msgtext)
uo_progress.of_SetPosition(0)

k=1
st_process.text='Compare data of PICS and Web...'
for i=1 to li_count
	ls_med_orc=trim(dw_qastg_orc.object.bkmed[i])
	li_seq_orc=dw_qastg_orc.object.bkseq[i]
	ls_cntr_orc=trim(dw_qastg_orc.object.cntr[i])
	ls_stg_orc=trim(dw_qastg_orc.object.qastg[i])
	ls_stat_orc=trim(dw_qastg_orc.object.qastatcd[i])
	if ls_stat_orc='I' then continue
	ld_recdt_orc=date(dw_qastg_orc.object.qarecdt[i])
	ld_comdt_orc=date(dw_qastg_orc.object.qacompdt[i])
	if mod(i,100)=0 then
		uo_progress.of_Increment(1)
		this.SetRedraw(TRUE)
	else
		this.SetRedraw(false)
	end if
	lb_find=false
	if (li_seq_orc=35276 or li_seq_orc=53282 )  and ls_med_orc='RC' then
		li_re=100
	end if
	for j=k to li_row
		ls_med=trim(dw_qastg_inx.object.bkmed[j])
		li_seq=dw_qastg_inx.object.bkseq[j]
		ls_cntr=trim(dw_qastg_inx.object.cntr[j])
		ls_stg=trim(dw_qastg_inx.object.qastg[j])
		ls_stat=trim(dw_qastg_inx.object.qastatcd[j])
		ld_recdt=(dw_qastg_inx.object.qarecdt[j])
		ld_comdt=(dw_qastg_inx.object.qacompdt[j])
		if ls_med_orc=ls_med and li_seq_orc=li_seq and ls_cntr_orc= ls_cntr then
//			li_cur=dw_qastg_inx_orc.InsertRow(0)
//	
//			dw_qastg_inx_orc.object.bkmed[li_cur]=ls_med_orc
//			dw_qastg_inx_orc.object.bkseq[li_cur]=li_seq_orc
//			dw_qastg_inx_orc.object.cntr[li_cur]=ls_cntr_orc
//			dw_qastg_inx_orc.object.cntr_orc[li_cur]=ls_cntr_orc
//			dw_qastg_inx_orc.object.qastg[li_cur]=ls_stg
//			dw_qastg_inx_orc.object.qastatcd[li_cur]=ls_stat
//			dw_qastg_inx_orc.object.qarecdt[li_cur]=ld_recdt
//			dw_qastg_inx_orc.object.qacompdt[li_cur]=ld_comdt
//			
//			dw_qastg_inx_orc.object.qastg_orc[li_cur]=ls_stg_orc
//			dw_qastg_inx_orc.object.qastatcd_orc[li_cur]=ls_stat_orc
//			dw_qastg_inx_orc.object.qarecdt_orc[li_cur]=ld_recdt_orc
//			dw_qastg_inx_orc.object.qacompdt_orc[li_cur]=ld_comdt_orc
			lb_find=true
			k=j +1
			exit
		elseif ls_med< ls_med_orc then
			continue
		elseif ls_med= ls_med_orc and li_seq< li_seq_orc then
			continue
		elseif ls_med=ls_med_orc and li_seq=li_seq_orc and ls_cntr< ls_cntr_orc then
			continue
		elseif (ls_med> ls_med_orc or (ls_med=ls_med_orc and li_seq> li_seq_orc) or &
			(ls_med=ls_med_orc and li_seq= li_seq_orc and ls_cntr> ls_cntr_orc)) and &
			 																					lb_find=false then
			 li_cur=dw_qastg_inx_orc.InsertRow(0)
			 dw_qastg_inx_orc.object.bkmed[li_cur]=ls_med_orc
			dw_qastg_inx_orc.object.bkseq[li_cur]=li_seq_orc
			dw_qastg_inx_orc.object.cntr_orc[li_cur]=ls_cntr_orc
			dw_qastg_inx_orc.object.qastg[li_cur]=ls_null
			dw_qastg_inx_orc.object.qastatcd[li_cur]=ls_null
			dw_qastg_inx_orc.object.qarecdt[li_cur]=ld_null
			dw_qastg_inx_orc.object.qacompdt[li_cur]=ld_null
			
			dw_qastg_inx_orc.object.qastg_orc[li_cur]=ls_stg_orc
			dw_qastg_inx_orc.object.qastatcd_orc[li_cur]=ls_stat_orc
			dw_qastg_inx_orc.object.qarecdt_orc[li_cur]=ld_recdt_orc
			dw_qastg_inx_orc.object.qacompdt_orc[li_cur]=ld_comdt_orc
			k=j
			exit
		elseif (ls_med> ls_med_orc or (ls_med=ls_med_orc and li_seq> li_seq_orc) or &
			(ls_med=ls_med_orc and li_seq= li_seq_orc and ls_cntr> ls_cntr_orc)) and &
			 																					lb_find=true then
			k=j
			exit
		end if
	next
next
li_row=dw_qastg_inx_orc.RowCount()
for i=1 to li_row
	ls_cntr_orc=trim(dw_qastg_inx_orc.object.cntr_orc[i])
	ls_stg_orc=trim(dw_qastg_inx_orc.object.qastg_orc[i])
	ls_stat_orc=trim(dw_qastg_inx_orc.object.qastatcd_orc[i])
	ld_recdt_orc=(dw_qastg_inx_orc.object.qarecdt_orc[i])
	ld_comdt_orc=(dw_qastg_inx_orc.object.qacompdt_orc[i])
	
	ls_cntr=trim(dw_qastg_inx_orc.object.cntr[i])
	ls_stg=trim(dw_qastg_inx_orc.object.qastg[i])
	ls_stat=trim(dw_qastg_inx_orc.object.qastatcd[i])
	ld_recdt=(dw_qastg_inx_orc.object.qarecdt[i])
	ld_comdt=(dw_qastg_inx_orc.object.qacompdt[i])
	if isnull(ls_cntr_orc) and isnull(ls_cntr)=false or &
		isnull(ls_cntr_orc)=false and isnull(ls_cntr)=true or &
		ls_cntr_orc<> ls_cntr or &
		isnull(ls_stg_orc) and isnull(ls_stg)=false or &
		isnull(ls_stg_orc)=false and isnull(ls_stg)=true or &
		ls_stg_orc<> ls_stg or &
		isnull(ls_stat_orc) and isnull(ls_stat)=false or &
		isnull(ls_stat_orc)=false and isnull(ls_stat)=true or &
		ls_stat_orc<> ls_stat or &
		isnull(ld_recdt_orc) and isnull(ld_recdt)=false or &
		isnull(ld_recdt_orc)=false and isnull(ld_recdt)=true or &
		ld_recdt_orc<> ld_recdt  then		
		rtn++
	end if
next
this.SetRedraw(true)
uo_progress.Visible=TRUE
uo_progress.of_SetMinimum(0)
uo_progress.of_SetMaximum(li_count * 0.1)
uo_progress.of_SetDisplayStyle(3)
uo_progress.of_SetMessageText(ls_msgtext)
uo_progress.of_SetPosition(0)
st_process.text='Compare data done...'
st_total_inx_orc.text='Number of different row: '+string(rtn)
dw_qastg_inx.visible=false
dw_qastg_orc.visible=false
dw_qastg_inx_orc.visible=true









end event

event pfc_preopen();call super::pfc_preopen;this.of_SetPreference(TRUE)
this.inv_preference.of_SetToolBars(TRUE)
this.inv_preference.of_SetWindow(TRUE)
this.of_SetResize(TRUE)
this.inv_resize.of_SetOrigSize(this.workspacewidth(),this.workspaceheight())
inv_resize.of_Register(cb_inx, "scale")
inv_resize.of_Register(cb_inxorc, "scale")
inv_resize.of_Register(cb_orc, "scale")
inv_resize.of_Register(cb_prt, "scale")
inv_resize.of_Register(cb_cancel, "scale")
inv_resize.of_Register(dw_qastg_inx, "scale")
inv_resize.of_Register(dw_qastg_orc, "scale")
inv_resize.of_Register(uo_progress, "scale")
inv_resize.of_Register(st_process, "scale")
inv_resize.of_Register(st_total_inx, "scale")
inv_resize.of_Register(st_total_orc, "scale")
inv_resize.of_Register(dw_qastg_inx_orc, "scale")

inv_resize.of_Register(st_total_inx_orc, "scale")
end event

event resize;call super::resize;long ll_height

//This script is not commented because when we do minimize this screen then all the DW'S are not resizing 
This.X = w_pics_main.X
This.Y = w_pics_main.Y
ll_height = w_pics_main.mdi_1.Height
This.Resize(w_pics_main.WorkSpaceWidth(), ll_height)


end event

type st_total_inx_orc from statictext within w_cmpare_orcl_infx_qastg
integer x = 974
integer y = 1460
integer width = 951
integer height = 64
integer textsize = -8
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 33554432
long backcolor = 67108864
alignment alignment = center!
boolean focusrectangle = false
end type

type cb_prt from commandbutton within w_cmpare_orcl_infx_qastg
integer x = 87
integer y = 1412
integer width = 169
integer height = 96
integer taborder = 170
boolean bringtotop = true
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "&Print"
boolean cancel = true
end type

event clicked;if dw_qastg_inx.visible=true then
	dw_qastg_inx.triggerEvent('pfc_print')
elseif dw_qastg_orc.visible=true then
	dw_qastg_orc.triggerEvent('pfc_print')
elseif dw_qastg_inx_orc.visible then
	dw_qastg_inx_orc.triggerEvent('pfc_print')
end if

end event

type cb_orc from commandbutton within w_cmpare_orcl_infx_qastg
integer x = 3113
integer y = 1412
integer width = 155
integer height = 96
integer taborder = 260
boolean bringtotop = true
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "&Orc"
boolean cancel = true
end type

event clicked;dw_qastg_inx.visible=false
dw_qastg_orc.visible=true
dw_qastg_inx_orc.visible=false
end event

type dw_qastg_inx_orc from u_pics_dw within w_cmpare_orcl_infx_qastg
integer x = 46
integer y = 44
integer width = 3401
integer height = 1292
integer taborder = 10
boolean bringtotop = true
string dataobject = "d_qastg_inx_orc"
boolean resizable = true
end type

event constructor;call super::constructor;THIS.settransobject(sqlservertrans)

end event

event retrieveend;call super::retrieveend;int li_loop

for li_loop = 1 to rowcount
	i_count++

	if mod(i_count,10)=0 then
		uo_progress.of_Increment(1)
		parent.SetRedraw(TRUE)
	else
		parent.SetRedraw(false)
	end if
	//SetPointer(HourGlass!)
next
end event

type st_total_inx from statictext within w_cmpare_orcl_infx_qastg
integer x = 288
integer y = 1460
integer width = 608
integer height = 64
integer textsize = -8
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 33554432
long backcolor = 67108864
alignment alignment = center!
boolean focusrectangle = false
end type

type st_total_orc from statictext within w_cmpare_orcl_infx_qastg
integer x = 288
integer y = 1372
integer width = 608
integer height = 52
integer textsize = -8
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 33554432
long backcolor = 67108864
alignment alignment = center!
boolean focusrectangle = false
end type

type st_process from statictext within w_cmpare_orcl_infx_qastg
integer x = 974
integer y = 1372
integer width = 951
integer height = 52
integer textsize = -8
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 33554432
long backcolor = 67108864
alignment alignment = center!
boolean focusrectangle = false
end type

type uo_progress from u_progressbar within w_cmpare_orcl_infx_qastg
integer x = 2030
integer y = 1368
integer width = 603
integer taborder = 160
boolean border = true
borderstyle borderstyle = stylelowered!
end type

on uo_progress.destroy
call u_progressbar::destroy
end on

type cb_inxorc from commandbutton within w_cmpare_orcl_infx_qastg
integer x = 2656
integer y = 1412
integer width = 247
integer height = 96
integer taborder = 150
boolean bringtotop = true
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "&inx/orc"
boolean cancel = true
end type

event clicked;dw_qastg_inx.visible=false
dw_qastg_orc.visible=false
dw_qastg_inx_orc.visible=true
end event

type cb_inx from commandbutton within w_cmpare_orcl_infx_qastg
integer x = 2930
integer y = 1412
integer width = 155
integer height = 96
integer taborder = 250
boolean bringtotop = true
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "&Inx"
boolean cancel = true
end type

event clicked;dw_qastg_inx.visible=true
dw_qastg_orc.visible=false
dw_qastg_inx_orc.visible=false
end event

type cb_cancel from commandbutton within w_cmpare_orcl_infx_qastg
integer x = 3296
integer y = 1412
integer width = 187
integer height = 96
integer taborder = 140
boolean bringtotop = true
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "&Close"
boolean cancel = true
end type

event clicked;
//close(parent)
end event

type dw_qastg_orc from u_pics_dw within w_cmpare_orcl_infx_qastg
integer x = 55
integer y = 48
integer width = 3401
integer height = 1296
integer taborder = 210
string dataobject = "d_qastg_orc"
boolean resizable = true
end type

event constructor;call super::constructor;THIS.settransobject(sqlserverOracletrans)

end event

event retrieveend;call super::retrieveend;int li_loop

for li_loop = 1 to rowcount
	i_count++

if mod(i_count,10)=0 then
	uo_progress.of_Increment(1)
	parent.SetRedraw(TRUE)
else
	parent.SetRedraw(false)
end if
//SetPointer(HourGlass!)
next
end event

type dw_qastg_inx from u_pics_dw within w_cmpare_orcl_infx_qastg
integer x = 69
integer y = 56
integer width = 3401
integer height = 1292
integer taborder = 220
string dataobject = "d_qastg_inx"
boolean resizable = true
end type

event constructor;call super::constructor;THIS.settransobject(sqlservertrans)

end event

event retrieveend;call super::retrieveend;int li_loop

for li_loop = 1 to rowcount
	i_count++
	
	if mod(i_count,10)=0 then
		uo_progress.of_Increment(1)
		parent.SetRedraw(TRUE)
	else
		parent.SetRedraw(false)
	end if
	//SetPointer(HourGlass!)
next
end event


$PBExportHeader$w_cy_allt_status.srw
forward
global type w_cy_allt_status from w_sheet
end type
type st_2 from statictext within w_cy_allt_status
end type
type st_1 from statictext within w_cy_allt_status
end type
type tab_1 from u_tab1 within w_cy_allt_status
end type
type tab_1 from u_tab1 within w_cy_allt_status
end type
type cb_exit from u_cb within w_cy_allt_status
end type
type dw_allt_status_fordddw from u_pics_dw within w_cy_allt_status
end type
type dw_dist_for_dddw from u_pics_dw within w_cy_allt_status
end type
end forward

global type w_cy_allt_status from w_sheet
integer x = 165
integer y = 128
integer width = 2779
integer height = 1792
string title = "Copy Allotment Status"
boolean maxbox = false
st_2 st_2
st_1 st_1
tab_1 tab_1
cb_exit cb_exit
dw_allt_status_fordddw dw_allt_status_fordddw
dw_dist_for_dddw dw_dist_for_dddw
end type
global w_cy_allt_status w_cy_allt_status

on w_cy_allt_status.create
int iCurrent
call super::create
this.st_2=create st_2
this.st_1=create st_1
this.tab_1=create tab_1
this.cb_exit=create cb_exit
this.dw_allt_status_fordddw=create dw_allt_status_fordddw
this.dw_dist_for_dddw=create dw_dist_for_dddw
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_2
this.Control[iCurrent+2]=this.st_1
this.Control[iCurrent+3]=this.tab_1
this.Control[iCurrent+4]=this.cb_exit
this.Control[iCurrent+5]=this.dw_allt_status_fordddw
this.Control[iCurrent+6]=this.dw_dist_for_dddw
end on

on w_cy_allt_status.destroy
call super::destroy
destroy(this.st_2)
destroy(this.st_1)
destroy(this.tab_1)
destroy(this.cb_exit)
destroy(this.dw_allt_status_fordddw)
destroy(this.dw_dist_for_dddw)
end on

event mousemove;call super::mousemove;w_pics_main.setmicrohelp("Ready")
end event

event pfc_postopen;call super::pfc_postopen;datawindowchild dwc_cabdt, dwc_dsdt
date ld_cabdt, ld_dsdtmax
datetime ld_dsdtmax_dt
string ls_dsdtmax, ls_filter

//ld_cabdt=today()
This.windowstate = maximized!
dw_allt_status_fordddw.GetChild('cabdt',dwc_cabdt)
dwc_cabdt.SetTransObject(SqlServertrans)
dwc_cabdt.Retrieve()
dw_allt_status_fordddw.SetTransObject(SqlServertrans)
dw_allt_status_fordddw.Retrieve()
dw_dist_for_dddw.GetChild('cabdt',dwc_dsdt)
dwc_dsdt.SetTransObject(SqlServertrans)
dwc_dsdt.Retrieve()
dw_dist_for_dddw.SetTransObject(SqlServertrans)
dw_dist_for_dddw.Retrieve()

tab_1.pg_batch.dw_sel_allt_status.SetTransObject(SqlServertrans)
tab_1.pg_batch.dw_sel_allt_status.Retrieve()
tab_1.pg_dist.dw_dist_sched_status.SetTransObject(SqlServertrans)
tab_1.pg_dist.dw_dist_sched_status.Retrieve()
dw_allt_status_fordddw.visible=true
st_1.visible=true
dw_dist_for_dddw.visible=false
st_2.visible=false

select max(dsdt) into :ld_dsdtmax_dt
from dist
using SqlServertrans;
if not f_check_dberror(SqlServertrans,'select max(dsdt) from dist') then
	return
end if
dw_dist_for_dddw.setItem(1,'dsdt', ld_dsdtmax_dt)
ld_dsdtmax=date(ld_dsdtmax_dt)
ls_dsdtmax=string(ld_dsdtmax)
ls_filter="date(dsdt)=date('"+ ls_dsdtmax+"')"
tab_1.pg_dist.dw_dist_sched_status.SetFilter(ls_filter)
tab_1.pg_dist.dw_dist_sched_status.Filter()
end event

event pfc_preopen();call super::pfc_preopen;this.of_SetPreference(TRUE)
this.inv_preference.of_SetToolBars(TRUE)
this.inv_preference.of_SetWindow(TRUE)
this.of_SetResize(TRUE)
this.inv_resize.of_SetOrigSize(this.workspacewidth(),this.workspaceheight())
inv_resize.of_Register(dw_allt_status_fordddw, "scale")
inv_resize.of_Register(dw_dist_for_dddw, "scale")
inv_resize.of_Register(cb_exit, "scale")
inv_resize.of_Register(st_1 ,"scale")
inv_resize.of_Register(st_2 ,"scale")
inv_resize.of_Register(tab_1, "scale")
inv_resize.of_Register(tab_1.pg_batch.dw_sel_allt_status, "scale")
inv_resize.of_Register(tab_1.pg_dist.dw_dist_sched_status, "scale")
//inv_resize.of_Register(st_1 "scale")
//inv_resize.of_Register(st_1 "scale")

end event

event resize;call super::resize;long ll_height

//This.X = 120
//This.Y = 100
//THIS.width = 3300
//This.height = 1700
//ll_height = w_pics_main.mdi_1.Height
//This.Resize(w_pics_main.WorkSpaceWidth(), ll_height)
end event

type st_2 from statictext within w_cy_allt_status
integer x = 137
integer y = 24
integer width = 302
integer height = 64
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 33554432
long backcolor = 67108864
string text = "DSDT:"
alignment alignment = right!
boolean focusrectangle = false
end type

type st_1 from statictext within w_cy_allt_status
integer x = 178
integer y = 36
integer width = 261
integer height = 52
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 33554432
long backcolor = 67108864
string text = "CABDT:"
alignment alignment = right!
boolean focusrectangle = false
end type

type tab_1 from u_tab1 within w_cy_allt_status
integer y = 124
integer taborder = 11
end type

event selectionchanged;call super::selectionchanged;
if newindex=1 then
	dw_allt_status_fordddw.visible=true
	st_1.visible=true
	dw_dist_for_dddw.visible=false
	st_2.visible=false
elseif newindex=2 then
	dw_dist_for_dddw.visible=true
	st_2.visible=true
	dw_allt_status_fordddw.visible=false
	st_1.visible=false
	
end if
end event

type cb_exit from u_cb within w_cy_allt_status
event pfc_hinttext pbm_mousemove
string tag = "Exits the current Screen"
integer x = 2286
integer y = 1564
integer width = 357
integer taborder = 0
integer textsize = -10
string text = "E&xit"
end type

event pfc_hinttext;call super::pfc_hinttext;w_pics_main.Setmicrohelp(THIS.tag)
end event

event clicked;call super::clicked;ib_disableclosequery = TRUE
parent.Event pfc_close()
m_pics_main.m_menu.popMenu(300,0)
end event

type dw_allt_status_fordddw from u_pics_dw within w_cy_allt_status
integer x = 471
integer y = 20
integer width = 311
integer height = 80
integer taborder = 10
string dataobject = "d_allt_status"
boolean vscrollbar = false
end type

event itemchanged;call super::itemchanged;string ls_colname, ls_cabdt, ls_filter

ls_colname=dwo.name
if ls_colname='cabdt' then
	ls_cabdt= data
	ls_cabdt=left(ls_cabdt,10)
	if IsNull(ls_cabdt)= false then
		ls_filter="date(cabdt) = date('"+ls_cabdt+"')"
		tab_1.pg_batch.dw_sel_allt_status.Setfilter(ls_filter)
		tab_1.pg_batch.dw_sel_allt_status.Filter()
	end if
end if 
end event

type dw_dist_for_dddw from u_pics_dw within w_cy_allt_status
integer x = 475
integer y = 16
integer width = 306
integer height = 80
integer taborder = 20
string dataobject = "d_dist_for_dddw"
boolean vscrollbar = false
end type

event itemchanged;call super::itemchanged;string ls_colname, ls_dsdt, ls_filter

ls_colname=dwo.name
if ls_colname='dsdt' then
	ls_dsdt= data
	ls_dsdt=left(ls_dsdt,10)
	if IsNull(ls_dsdt)=false then
		ls_filter="date(dsdt) = date('"+ls_dsdt+"')"
		tab_1.pg_dist.dw_dist_sched_status.Setfilter(ls_filter)
		tab_1.pg_dist.dw_dist_sched_status.Filter()
	end if
end if
end event


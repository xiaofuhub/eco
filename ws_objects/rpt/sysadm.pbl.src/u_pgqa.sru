$PBExportHeader$u_pgqa.sru
forward
global type u_pgqa from u_tabpg
end type
type dw_prdrbk_cmp from u_pics_dw within u_pgqa
end type
type dw_qastg_oracle_bkno from u_pics_dw within u_pgqa
end type
type dw_qastg_inx from u_pics_dw within u_pgqa
end type
type dw_qastg_orc from u_pics_dw within u_pgqa
end type
type dw_qastg_inx_orc from u_pics_dw within u_pgqa
end type
end forward

global type u_pgqa from u_tabpg
integer width = 3520
integer height = 1156
dw_prdrbk_cmp dw_prdrbk_cmp
dw_qastg_oracle_bkno dw_qastg_oracle_bkno
dw_qastg_inx dw_qastg_inx
dw_qastg_orc dw_qastg_orc
dw_qastg_inx_orc dw_qastg_inx_orc
end type
global u_pgqa u_pgqa

type variables
long i_count
end variables

on u_pgqa.create
int iCurrent
call super::create
this.dw_prdrbk_cmp=create dw_prdrbk_cmp
this.dw_qastg_oracle_bkno=create dw_qastg_oracle_bkno
this.dw_qastg_inx=create dw_qastg_inx
this.dw_qastg_orc=create dw_qastg_orc
this.dw_qastg_inx_orc=create dw_qastg_inx_orc
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_prdrbk_cmp
this.Control[iCurrent+2]=this.dw_qastg_oracle_bkno
this.Control[iCurrent+3]=this.dw_qastg_inx
this.Control[iCurrent+4]=this.dw_qastg_orc
this.Control[iCurrent+5]=this.dw_qastg_inx_orc
end on

on u_pgqa.destroy
call super::destroy
destroy(this.dw_prdrbk_cmp)
destroy(this.dw_qastg_oracle_bkno)
destroy(this.dw_qastg_inx)
destroy(this.dw_qastg_orc)
destroy(this.dw_qastg_inx_orc)
end on

type dw_prdrbk_cmp from u_pics_dw within u_pgqa
boolean visible = false
integer x = 32
integer y = 1088
integer width = 50
integer height = 44
integer taborder = 10
string dataobject = "d_prdrbk_cmp"
boolean vscrollbar = false
boolean livescroll = false
end type

event constructor;call super::constructor;this.SetTransobject(SqlServerTrans)
end event

event sqlpreview;call super::sqlpreview;
i_count++
if mod(i_count,10)=0 then
	w_cmpare_orcl_infx.uo_progress.of_Increment(1)
	w_cmpare_orcl_infx.SetRedraw(TRUE)
else
	w_cmpare_orcl_infx.SetRedraw(false)
end if	
end event

type dw_qastg_oracle_bkno from u_pics_dw within u_pgqa
boolean visible = false
integer x = 87
integer y = 1088
integer width = 59
integer height = 44
integer taborder = 10
string dataobject = "d_qastg_oracle_bkno"
boolean vscrollbar = false
boolean livescroll = false
end type

event constructor;call super::constructor;this.of_SetFilter(TRUE)
this.of_SetFind(TRUE)
this.inv_filter.of_SetStyle(1)
//this.inv_filter.of_SetColumnNameSource(2)
this.of_SetSort(TRUE)
this.inv_sort.of_SetStyle(1)
//this.inv_sort.of_SetColumnNameSource(2)
This.of_SetTransObject(sqlserverOracletrans)


//THIS.settransobject(sqlservertrans)

end event

event retrieverow;call super::retrieverow;//i_count++
//
//if mod(i_count,10)=0 then
//	w_cmpare_orcl_infx.uo_progress.of_Increment(1)
//	w_cmpare_orcl_infx.SetRedraw(TRUE)
//else
//	w_cmpare_orcl_infx.SetRedraw(false)
//end if
//SetPointer(HourGlass!)
end event

type dw_qastg_inx from u_pics_dw within u_pgqa
integer x = 23
integer y = 32
integer width = 3479
integer height = 1104
integer taborder = 10
string dataobject = "d_qastg_inx"
boolean hscrollbar = true
boolean hsplitscroll = true
end type

event constructor;call super::constructor;this.of_SetFilter(TRUE)
this.of_SetFind(TRUE)
this.inv_filter.of_SetStyle(1)
//this.inv_filter.of_SetColumnNameSource(2)
this.of_SetSort(TRUE)
this.inv_sort.of_SetStyle(1)
//this.inv_sort.of_SetColumnNameSource(2)
This.of_SetTransObject(sqlservertrans)


//THIS.settransobject(sqlservertrans)

end event

event retrieveend;call super::retrieveend;int li_loop

for li_loop = 1 to rowcount
	i_count++
	
	if mod(i_count,10)=0 then
		w_cmpare_orcl_infx.uo_progress.of_Increment(1)
		w_cmpare_orcl_infx.SetRedraw(TRUE)
	else
		w_cmpare_orcl_infx.SetRedraw(false)
	end if
//SetPointer(HourGlass!)
next
end event

type dw_qastg_orc from u_pics_dw within u_pgqa
integer x = 14
integer y = 32
integer width = 3479
integer height = 1112
integer taborder = 20
string dataobject = "d_qastg_orc"
boolean hscrollbar = true
boolean hsplitscroll = true
end type

event constructor;call super::constructor;
this.of_SetFilter(TRUE)
this.of_SetFind(TRUE)
this.inv_filter.of_SetStyle(1)
//this.inv_filter.of_SetColumnNameSource(2)
this.of_SetSort(TRUE)
this.inv_sort.of_SetStyle(1)
//this.inv_sort.of_SetColumnNameSource(2)
//This.of_SetTransObject(sqlservertrans)
THIS.settransobject(sqlserveroracletrans)


//THIS.settransobject(sqlservertrans)

end event

event retrieverow;call super::retrieverow;//i_count++
//
//if mod(i_count,10)=0 then
//	w_cmpare_orcl_infx.uo_progress.of_Increment(1)
//	w_cmpare_orcl_infx.SetRedraw(TRUE)
//else
//	w_cmpare_orcl_infx.SetRedraw(false)
//end if
////SetPointer(HourGlass!)
end event

type dw_qastg_inx_orc from u_pics_dw within u_pgqa
boolean visible = false
integer x = 14
integer y = 32
integer width = 3493
integer height = 1100
integer taborder = 20
string dataobject = "d_qastg_inx_orc"
boolean hscrollbar = true
boolean hsplitscroll = true
end type

event constructor;call super::constructor;//THIS.settransobject(sqlservertrans)
this.of_SetFilter(TRUE)
this.of_SetFind(TRUE)
this.inv_filter.of_SetStyle(1)
//this.inv_filter.of_SetColumnNameSource(2)
this.of_SetSort(TRUE)
this.inv_sort.of_SetStyle(1)
//this.inv_sort.of_SetColumnNameSource(2)
This.of_SetTransObject(sqlservertrans)


//THIS.settransobject(sqlservertrans)


end event

event sqlpreview;call super::sqlpreview;//
//uo_progress.of_Increment(1)
//parent.SetRedraw(TRUE)
//SetPointer(HourGlass!)
end event

event retrieveend;call super::retrieveend;int li_loop

for li_loop = 1 to rowcount
	i_count++
	
	if mod(i_count,10)=0 then
		w_cmpare_orcl_infx.uo_progress.of_Increment(1)
		w_cmpare_orcl_infx.SetRedraw(TRUE)
	else
		w_cmpare_orcl_infx.SetRedraw(false)
	end if
//SetPointer(HourGlass!)
next
end event


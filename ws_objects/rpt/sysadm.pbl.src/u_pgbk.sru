$PBExportHeader$u_pgbk.sru
forward
global type u_pgbk from u_tabpg
end type
type dw_book_oracle_bkno from u_pics_dw within u_pgbk
end type
type dw_prdrbk_cmp from u_pics_dw within u_pgbk
end type
type dw_book_orc from u_pics_dw within u_pgbk
end type
type dw_book_inx_orc from u_pics_dw within u_pgbk
end type
type dw_book_inx from u_pics_dw within u_pgbk
end type
end forward

global type u_pgbk from u_tabpg
integer width = 3520
integer height = 1152
dw_book_oracle_bkno dw_book_oracle_bkno
dw_prdrbk_cmp dw_prdrbk_cmp
dw_book_orc dw_book_orc
dw_book_inx_orc dw_book_inx_orc
dw_book_inx dw_book_inx
end type
global u_pgbk u_pgbk

type variables
long i_count
end variables

on u_pgbk.create
int iCurrent
call super::create
this.dw_book_oracle_bkno=create dw_book_oracle_bkno
this.dw_prdrbk_cmp=create dw_prdrbk_cmp
this.dw_book_orc=create dw_book_orc
this.dw_book_inx_orc=create dw_book_inx_orc
this.dw_book_inx=create dw_book_inx
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_book_oracle_bkno
this.Control[iCurrent+2]=this.dw_prdrbk_cmp
this.Control[iCurrent+3]=this.dw_book_orc
this.Control[iCurrent+4]=this.dw_book_inx_orc
this.Control[iCurrent+5]=this.dw_book_inx
end on

on u_pgbk.destroy
call super::destroy
destroy(this.dw_book_oracle_bkno)
destroy(this.dw_prdrbk_cmp)
destroy(this.dw_book_orc)
destroy(this.dw_book_inx_orc)
destroy(this.dw_book_inx)
end on

type dw_book_oracle_bkno from u_pics_dw within u_pgbk
boolean visible = false
integer x = 73
integer y = 1088
integer width = 55
integer height = 44
integer taborder = 10
boolean enabled = false
string dataobject = "d_book_oracle_bkno"
boolean vscrollbar = false
end type

event constructor;call super::constructor;this.SetTransobject(SqlServerOracleTrans)
end event

type dw_prdrbk_cmp from u_pics_dw within u_pgbk
boolean visible = false
integer x = 18
integer y = 1088
integer width = 50
integer height = 56
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

type dw_book_orc from u_pics_dw within u_pgbk
integer x = 14
integer y = 32
integer width = 3479
integer height = 1112
integer taborder = 20
string dataobject = "d_book_orc"
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

type dw_book_inx_orc from u_pics_dw within u_pgbk
boolean visible = false
integer x = 14
integer y = 32
integer width = 3493
integer height = 1100
integer taborder = 20
string dataobject = "d_book_inx_orc"
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

event retrieveend;call super::retrieveend;i_count++
int li_loop

for li_loop = 1 to rowcount
	if mod(i_count,10)=0 then
		w_cmpare_orcl_infx.uo_progress.of_Increment(1)
		w_cmpare_orcl_infx.SetRedraw(TRUE)
	else
		w_cmpare_orcl_infx.SetRedraw(false)
	end if
	//SetPointer(HourGlass!)
next
end event

type dw_book_inx from u_pics_dw within u_pgbk
integer x = 23
integer y = 28
integer width = 3479
integer height = 1104
integer taborder = 10
string dataobject = "d_book_inx"
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

event retrieveend;call super::retrieveend;// appeon unsupported feature 3/29/10 retrieverow moved here
int li_loop


for Li_loop = 1 to rowcount
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


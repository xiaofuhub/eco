$PBExportHeader$u_pgprod.sru
forward
global type u_pgprod from u_tabpg
end type
type dw_prod_oracle_bkno from u_pics_dw within u_pgprod
end type
type dw_prod_oracle from u_pics_dw within u_pgprod
end type
type dw_prod_inx from u_pics_dw within u_pgprod
end type
type dw_prod_inx_orc from u_pics_dw within u_pgprod
end type
type dw_prdrbk_cmp from u_pics_dw within u_pgprod
end type
end forward

global type u_pgprod from u_tabpg
integer width = 3520
integer height = 1156
dw_prod_oracle_bkno dw_prod_oracle_bkno
dw_prod_oracle dw_prod_oracle
dw_prod_inx dw_prod_inx
dw_prod_inx_orc dw_prod_inx_orc
dw_prdrbk_cmp dw_prdrbk_cmp
end type
global u_pgprod u_pgprod

type variables
long i_count
end variables

on u_pgprod.create
int iCurrent
call super::create
this.dw_prod_oracle_bkno=create dw_prod_oracle_bkno
this.dw_prod_oracle=create dw_prod_oracle
this.dw_prod_inx=create dw_prod_inx
this.dw_prod_inx_orc=create dw_prod_inx_orc
this.dw_prdrbk_cmp=create dw_prdrbk_cmp
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_prod_oracle_bkno
this.Control[iCurrent+2]=this.dw_prod_oracle
this.Control[iCurrent+3]=this.dw_prod_inx
this.Control[iCurrent+4]=this.dw_prod_inx_orc
this.Control[iCurrent+5]=this.dw_prdrbk_cmp
end on

on u_pgprod.destroy
call super::destroy
destroy(this.dw_prod_oracle_bkno)
destroy(this.dw_prod_oracle)
destroy(this.dw_prod_inx)
destroy(this.dw_prod_inx_orc)
destroy(this.dw_prdrbk_cmp)
end on

type dw_prod_oracle_bkno from u_pics_dw within u_pgprod
boolean visible = false
integer x = 18
integer y = 1072
integer width = 50
integer height = 64
integer taborder = 40
boolean enabled = false
string dataobject = "d_prod_oracle_bkno"
boolean vscrollbar = false
end type

event constructor;call super::constructor;this.SetTransobject(SqlServerOracleTrans)
end event

type dw_prod_oracle from u_pics_dw within u_pgprod
integer x = 14
integer y = 32
integer width = 3479
integer height = 1112
integer taborder = 20
string dataobject = "d_prod_oracle"
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

type dw_prod_inx from u_pics_dw within u_pgprod
boolean visible = false
integer x = 14
integer y = 32
integer width = 3493
integer height = 1100
integer taborder = 20
string dataobject = "d_prod_inx"
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

type dw_prod_inx_orc from u_pics_dw within u_pgprod
integer x = 23
integer y = 28
integer width = 3479
integer height = 1104
integer taborder = 10
string dataobject = "d_prod_inx_orc"
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

type dw_prdrbk_cmp from u_pics_dw within u_pgprod
boolean visible = false
integer x = 9
integer y = 24
integer width = 3497
integer height = 1104
integer taborder = 10
string dataobject = "d_prdrbk_cmp"
boolean hscrollbar = true
boolean hsplitscroll = true
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


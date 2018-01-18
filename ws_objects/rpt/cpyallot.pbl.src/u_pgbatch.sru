$PBExportHeader$u_pgbatch.sru
forward
global type u_pgbatch from u_tabpg
end type
type dw_sel_allt_status from u_pics_dw within u_pgbatch
end type
end forward

global type u_pgbatch from u_tabpg
integer width = 2679
integer height = 1300
dw_sel_allt_status dw_sel_allt_status
end type
global u_pgbatch u_pgbatch

on u_pgbatch.create
int iCurrent
call super::create
this.dw_sel_allt_status=create dw_sel_allt_status
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_sel_allt_status
end on

on u_pgbatch.destroy
call super::destroy
destroy(this.dw_sel_allt_status)
end on

type dw_sel_allt_status from u_pics_dw within u_pgbatch
integer x = 27
integer y = 32
integer width = 2633
integer height = 1248
integer taborder = 10
string dataobject = "d_sel_allt_status"
end type


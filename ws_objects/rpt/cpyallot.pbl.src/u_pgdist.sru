$PBExportHeader$u_pgdist.sru
forward
global type u_pgdist from u_tabpg
end type
type dw_dist_sched_status from u_pics_dw within u_pgdist
end type
end forward

global type u_pgdist from u_tabpg
integer width = 2683
integer height = 1288
dw_dist_sched_status dw_dist_sched_status
end type
global u_pgdist u_pgdist

on u_pgdist.create
int iCurrent
call super::create
this.dw_dist_sched_status=create dw_dist_sched_status
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_dist_sched_status
end on

on u_pgdist.destroy
call super::destroy
destroy(this.dw_dist_sched_status)
end on

type dw_dist_sched_status from u_pics_dw within u_pgdist
integer x = 18
integer y = 24
integer width = 2638
integer height = 1248
integer taborder = 10
string dataobject = "d_dist_sched_status"
end type


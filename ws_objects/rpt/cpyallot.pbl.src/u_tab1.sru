$PBExportHeader$u_tab1.sru
forward
global type u_tab1 from u_tab
end type
type pg_batch from u_pgbatch within u_tab1
end type
type pg_batch from u_pgbatch within u_tab1
end type
type pg_dist from u_pgdist within u_tab1
end type
type pg_dist from u_pgdist within u_tab1
end type
end forward

global type u_tab1 from u_tab
integer width = 2711
integer height = 1412
pg_batch pg_batch
pg_dist pg_dist
end type
global u_tab1 u_tab1

on u_tab1.create
this.pg_batch=create pg_batch
this.pg_dist=create pg_dist
int iCurrent
call super::create
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.pg_batch
this.Control[iCurrent+2]=this.pg_dist
end on

on u_tab1.destroy
call super::destroy
destroy(this.pg_batch)
destroy(this.pg_dist)
end on

type pg_batch from u_pgbatch within u_tab1
integer x = 18
integer y = 100
integer width = 2674
integer height = 1296
string text = "Batch Formation"
end type

type pg_dist from u_pgdist within u_tab1
integer x = 18
integer y = 100
integer width = 2674
integer height = 1296
string text = "Distribution Schedule"
end type


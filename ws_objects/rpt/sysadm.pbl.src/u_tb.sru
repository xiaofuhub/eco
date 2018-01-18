$PBExportHeader$u_tb.sru
forward
global type u_tb from u_tab
end type
type pgprod from u_pgprod within u_tb
end type
type pgprod from u_pgprod within u_tb
end type
type pgqa from u_pgqa within u_tb
end type
type pgqa from u_pgqa within u_tb
end type
type pgbk from u_pgbk within u_tb
end type
type pgbk from u_pgbk within u_tb
end type
end forward

global type u_tb from u_tab
string tag = "Qastage"
integer width = 3534
integer height = 1260
pgprod pgprod
pgqa pgqa
pgbk pgbk
end type
global u_tb u_tb

on u_tb.create
this.pgprod=create pgprod
this.pgqa=create pgqa
this.pgbk=create pgbk
int iCurrent
call super::create
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.pgprod
this.Control[iCurrent+2]=this.pgqa
this.Control[iCurrent+3]=this.pgbk
end on

on u_tb.destroy
call super::destroy
destroy(this.pgprod)
destroy(this.pgqa)
destroy(this.pgbk)
end on

type pgprod from u_pgprod within u_tb
integer x = 18
integer y = 100
integer width = 3497
integer height = 1144
string text = "Production Information"
end type

type pgqa from u_pgqa within u_tb
integer x = 18
integer y = 100
integer width = 3497
integer height = 1144
string text = "QA Information"
end type

type pgbk from u_pgbk within u_tb
integer x = 18
integer y = 100
integer width = 3497
integer height = 1144
string text = "Book Information"
end type


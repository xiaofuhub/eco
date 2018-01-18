$PBExportHeader$uo_tab_pics.sru
forward
global type uo_tab_pics from u_tab
end type
type pg_bib from userobject within uo_tab_pics
end type
type dw_bib from u_pics_dw within pg_bib
end type
type pg_bib from userobject within uo_tab_pics
dw_bib dw_bib
end type
type pg_prod from userobject within uo_tab_pics
end type
type dw_ext from u_dw within pg_prod
end type
type dw_prod from u_pics_dw within pg_prod
end type
type pg_prod from userobject within uo_tab_pics
dw_ext dw_ext
dw_prod dw_prod
end type
type pg_cy from userobject within uo_tab_pics
end type
type dw_cy from u_pics_dw within pg_cy
end type
type pg_cy from userobject within uo_tab_pics
dw_cy dw_cy
end type
type pg_qa from userobject within uo_tab_pics
end type
type dw_qa from u_pics_dw within pg_qa
end type
type pg_qa from userobject within uo_tab_pics
dw_qa dw_qa
end type
type pg_inv from userobject within uo_tab_pics
end type
type dw_inv from u_pics_dw within pg_inv
end type
type pg_inv from userobject within uo_tab_pics
dw_inv dw_inv
end type
end forward

global type uo_tab_pics from u_tab
integer width = 3849
integer height = 1440
boolean focusonbuttondown = true
boolean showpicture = true
boolean boldselectedtext = true
boolean pictureonright = true
pg_bib pg_bib
pg_prod pg_prod
pg_cy pg_cy
pg_qa pg_qa
pg_inv pg_inv
end type
global uo_tab_pics uo_tab_pics

on uo_tab_pics.create
this.pg_bib=create pg_bib
this.pg_prod=create pg_prod
this.pg_cy=create pg_cy
this.pg_qa=create pg_qa
this.pg_inv=create pg_inv
int iCurrent
call super::create
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.pg_bib
this.Control[iCurrent+2]=this.pg_prod
this.Control[iCurrent+3]=this.pg_cy
this.Control[iCurrent+4]=this.pg_qa
this.Control[iCurrent+5]=this.pg_inv
end on

on uo_tab_pics.destroy
call super::destroy
destroy(this.pg_bib)
destroy(this.pg_prod)
destroy(this.pg_cy)
destroy(this.pg_qa)
destroy(this.pg_inv)
end on

type pg_bib from userobject within uo_tab_pics
event create ( )
event destroy ( )
integer x = 18
integer y = 100
integer width = 3813
integer height = 1324
long backcolor = 79741120
string text = "Bibliographic"
long tabtextcolor = 33554432
long tabbackcolor = 79741120
long picturemaskcolor = 536870912
dw_bib dw_bib
end type

on pg_bib.create
this.dw_bib=create dw_bib
this.Control[]={this.dw_bib}
end on

on pg_bib.destroy
destroy(this.dw_bib)
end on

type dw_bib from u_pics_dw within pg_bib
integer x = -18
integer y = -4
integer width = 3291
integer height = 1152
integer taborder = 11
string dataobject = "d_book_query_result_bib"
boolean hscrollbar = true
boolean hsplitscroll = true
end type

type pg_prod from userobject within uo_tab_pics
event create ( )
event destroy ( )
integer x = 18
integer y = 100
integer width = 3813
integer height = 1324
long backcolor = 79741120
string text = "Production"
long tabtextcolor = 33554432
long tabbackcolor = 79741120
long picturemaskcolor = 536870912
dw_ext dw_ext
dw_prod dw_prod
end type

on pg_prod.create
this.dw_ext=create dw_ext
this.dw_prod=create dw_prod
this.Control[]={this.dw_ext,&
this.dw_prod}
end on

on pg_prod.destroy
destroy(this.dw_ext)
destroy(this.dw_prod)
end on

type dw_ext from u_dw within pg_prod
integer x = -18
integer y = 764
integer width = 3255
integer height = 352
integer taborder = 11
string dataobject = "d_manu_ext_cntr"
end type

type dw_prod from u_pics_dw within pg_prod
integer x = -18
integer y = 28
integer width = 3781
integer height = 736
integer taborder = 11
string dataobject = "d_book_query_result_prod"
end type

type pg_cy from userobject within uo_tab_pics
event create ( )
event destroy ( )
integer x = 18
integer y = 100
integer width = 3813
integer height = 1324
long backcolor = 79741120
string text = "Copy allotment"
long tabtextcolor = 33554432
long tabbackcolor = 79741120
long picturemaskcolor = 536870912
dw_cy dw_cy
end type

on pg_cy.create
this.dw_cy=create dw_cy
this.Control[]={this.dw_cy}
end on

on pg_cy.destroy
destroy(this.dw_cy)
end on

type dw_cy from u_pics_dw within pg_cy
integer x = -18
integer y = -4
integer width = 3255
integer height = 1152
integer taborder = 11
string dataobject = "d_book_query_cy"
end type

type pg_qa from userobject within uo_tab_pics
event create ( )
event destroy ( )
integer x = 18
integer y = 100
integer width = 3813
integer height = 1324
long backcolor = 79741120
string text = "Quality assurance"
long tabtextcolor = 33554432
long tabbackcolor = 79741120
long picturemaskcolor = 536870912
dw_qa dw_qa
end type

on pg_qa.create
this.dw_qa=create dw_qa
this.Control[]={this.dw_qa}
end on

on pg_qa.destroy
destroy(this.dw_qa)
end on

type dw_qa from u_pics_dw within pg_qa
integer x = -18
integer y = -4
integer width = 3291
integer height = 1152
integer taborder = 11
string dataobject = "d_book_query_qa"
boolean hscrollbar = true
boolean hsplitscroll = true
end type

type pg_inv from userobject within uo_tab_pics
event create ( )
event destroy ( )
integer x = 18
integer y = 100
integer width = 3813
integer height = 1324
long backcolor = 79741120
string text = "Invoicing"
long tabtextcolor = 33554432
long tabbackcolor = 79741120
long picturemaskcolor = 536870912
dw_inv dw_inv
end type

on pg_inv.create
this.dw_inv=create dw_inv
this.Control[]={this.dw_inv}
end on

on pg_inv.destroy
destroy(this.dw_inv)
end on

type dw_inv from u_pics_dw within pg_inv
integer x = -18
integer y = -4
integer width = 3291
integer height = 1152
integer taborder = 11
string dataobject = "d_book_query_inv"
end type


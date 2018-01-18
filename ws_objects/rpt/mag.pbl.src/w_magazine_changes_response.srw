$PBExportHeader$w_magazine_changes_response.srw
forward
global type w_magazine_changes_response from w_response
end type
type dw_contracts from u_dw within w_magazine_changes_response
end type
type st_1 from u_st within w_magazine_changes_response
end type
end forward

global type w_magazine_changes_response from w_response
int X=1134
int Y=805
int Width=929
int Height=801
boolean TitleBar=true
string Title="Multiple Contracts Found"
boolean ControlMenu=false
dw_contracts dw_contracts
st_1 st_1
end type
global w_magazine_changes_response w_magazine_changes_response

on w_magazine_changes_response.create
int iCurrent
call w_response::create
this.dw_contracts=create dw_contracts
this.st_1=create st_1
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=dw_contracts
this.Control[iCurrent+2]=st_1
end on

on w_magazine_changes_response.destroy
call w_response::destroy
destroy(this.dw_contracts)
destroy(this.st_1)
end on

type dw_contracts from u_dw within w_magazine_changes_response
int X=183
int Y=193
int Width=513
int Height=257
string DataObject="d_contracts"
end type

event constructor;call super::constructor;String ls_producer
Integer li_fy

//ls_producer = w_magazine_changes.dw


SetTransObject(SqlServerTrans)

ls_producer = w_magazine_changes.dw_magazine_changes_producer.GetText()
li_fy = Integer(w_magazine_changes.em_fy.text)



dw_contracts.Retrieve(ls_producer,li_fy)


end event

event doubleclicked;call super::doubleclicked;String ls_temp

ls_temp = dw_contracts.GetText()


w_magazine_changes.ls_temp_contract = ls_temp

close(w_magazine_changes_response)

end event

type st_1 from u_st within w_magazine_changes_response
int X=74
int Y=33
int Width=769
string Text="Double click on  contract:"
Alignment Alignment=Center!
int TextSize=-10
end type


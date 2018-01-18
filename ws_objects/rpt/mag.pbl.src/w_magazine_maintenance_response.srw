$PBExportHeader$w_magazine_maintenance_response.srw
forward
global type w_magazine_maintenance_response from w_response
end type
type dw_contracts from u_pics_dw within w_magazine_maintenance_response
end type
type st_1 from statictext within w_magazine_maintenance_response
end type
end forward

global type w_magazine_maintenance_response from w_response
int X=906
int Y=661
int Width=865
int Height=589
boolean TitleBar=true
string Title="Contracts"
boolean ControlMenu=false
dw_contracts dw_contracts
st_1 st_1
end type
global w_magazine_maintenance_response w_magazine_maintenance_response

on w_magazine_maintenance_response.create
int iCurrent
call w_response::create
this.dw_contracts=create dw_contracts
this.st_1=create st_1
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=dw_contracts
this.Control[iCurrent+2]=st_1
end on

on w_magazine_maintenance_response.destroy
call w_response::destroy
destroy(this.dw_contracts)
destroy(this.st_1)
end on

type dw_contracts from u_pics_dw within w_magazine_maintenance_response
int X=220
int Y=129
int Width=366
int Height=313
string DataObject="d_contracts"
end type

event constructor;call super::constructor;String  ls_producer
Integer li_fy

SetTransObject(SqlServerTrans)

ls_producer = w_magazine_maintenance.dw_producer.GetText()

li_fy = Integer(w_magazine_maintenance.em_fy.text)

dw_contracts.Retrieve(ls_producer,li_fy)


end event

event doubleclicked;call super::doubleclicked;String ls_temp

ls_temp = dw_contracts.GetText()

w_magazine_maintenance.ls_temp_contract = ls_temp

close(w_magazine_maintenance_response)
end event

type st_1 from statictext within w_magazine_maintenance_response
int X=37
int Y=33
int Width=705
int Height=77
boolean Enabled=false
boolean BringToTop=true
string Text="Double Click On Contract:"
boolean FocusRectangle=false
long BackColor=12632256
int TextSize=-10
int Weight=400
string FaceName="MS Sans Serif"
FontFamily FontFamily=Swiss!
FontPitch FontPitch=Variable!
end type


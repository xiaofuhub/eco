$PBExportHeader$w_qa_select_contract.srw
forward
global type w_qa_select_contract from w_response
end type
type dw_contracts from u_pics_dw within w_qa_select_contract
end type
type st_1 from statictext within w_qa_select_contract
end type
type cb_cancel from commandbutton within w_qa_select_contract
end type
type cb_ok from commandbutton within w_qa_select_contract
end type
end forward

global type w_qa_select_contract from w_response
int X=906
int Y=621
int Width=819
int Height=673
boolean TitleBar=true
string Title="Select Contract"
dw_contracts dw_contracts
st_1 st_1
cb_cancel cb_cancel
cb_ok cb_ok
end type
global w_qa_select_contract w_qa_select_contract

type variables
long Lbkseq
end variables

on w_qa_select_contract.create
int iCurrent
call w_response::create
this.dw_contracts=create dw_contracts
this.st_1=create st_1
this.cb_cancel=create cb_cancel
this.cb_ok=create cb_ok
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=dw_contracts
this.Control[iCurrent+2]=st_1
this.Control[iCurrent+3]=cb_cancel
this.Control[iCurrent+4]=cb_ok
end on

on w_qa_select_contract.destroy
call w_response::destroy
destroy(this.dw_contracts)
destroy(this.st_1)
destroy(this.cb_cancel)
destroy(this.cb_ok)
end on

event open;call super::open;Lbkseq = Message.DoubleParm
dw_contracts.Retrieve(Lbkseq)

end event

type dw_contracts from u_pics_dw within w_qa_select_contract
int X=183
int Y=129
int Width=403
int Height=289
int TabOrder=10
string DataObject="d_qa_contract"
end type

event constructor;call super::constructor;SetTransObject(SqlServerTrans)


end event

event doubleclicked;call super::doubleclicked;string Lcntr
Lcntr = dw_contracts.object.cntr[row] 
CloseWithReturn(Parent,Lcntr)

end event

type st_1 from statictext within w_qa_select_contract
int X=28
int Y=25
int Width=737
int Height=77
boolean Enabled=false
boolean BringToTop=true
string Text="Double Click or Click OK"
boolean FocusRectangle=false
long BackColor=12632256
int TextSize=-10
int Weight=400
string FaceName="Arial"
FontFamily FontFamily=Swiss!
FontPitch FontPitch=Variable!
end type

type cb_cancel from commandbutton within w_qa_select_contract
int X=412
int Y=469
int Width=238
int Height=81
int TabOrder=20
boolean BringToTop=true
string Text="&Cancel"
int TextSize=-8
int Weight=400
string FaceName="MS Sans Serif"
FontFamily FontFamily=Swiss!
FontPitch FontPitch=Variable!
end type

event clicked;close(parent)
end event

type cb_ok from commandbutton within w_qa_select_contract
int X=115
int Y=469
int Width=211
int Height=81
int TabOrder=3
boolean BringToTop=true
string Text="&Ok"
boolean Default=true
int TextSize=-8
int Weight=400
string FaceName="MS Sans Serif"
FontFamily FontFamily=Swiss!
FontPitch FontPitch=Variable!
end type

event clicked;string Lcntr
Lcntr = dw_contracts.object.cntr[dw_contracts.Getrow()] 
CloseWithReturn(Parent,Lcntr)

end event


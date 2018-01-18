$PBExportHeader$w_magazine_existing_invoices.srw
forward
global type w_magazine_existing_invoices from Window
end type
type dw_existing_invno from u_pics_dw within w_magazine_existing_invoices
end type
type cb_ok from commandbutton within w_magazine_existing_invoices
end type
type st_1 from statictext within w_magazine_existing_invoices
end type
end forward

global type w_magazine_existing_invoices from Window
int X=874
int Y=401
int Width=1290
int Height=913
boolean TitleBar=true
string Title="Existing invoice numbers"
long BackColor=79741120
boolean ControlMenu=true
WindowType WindowType=response!
dw_existing_invno dw_existing_invno
cb_ok cb_ok
st_1 st_1
end type
global w_magazine_existing_invoices w_magazine_existing_invoices

on w_magazine_existing_invoices.create
this.dw_existing_invno=create dw_existing_invno
this.cb_ok=create cb_ok
this.st_1=create st_1
this.Control[]={ this.dw_existing_invno,&
this.cb_ok,&
this.st_1}
end on

on w_magazine_existing_invoices.destroy
destroy(this.dw_existing_invno)
destroy(this.cb_ok)
destroy(this.st_1)
end on

event open;str_exist_inv lstr_exist_inv
lstr_exist_inv = Message.PowerObjectParm

st_1.text = "Please DO NOT use any of these invoice numbers for magcd="+lstr_exist_inv.ls_magcd+" issue date= "+ string(lstr_exist_inv.ld_issdt)				
dw_existing_invno.retrieve(lstr_exist_inv.ls_magcd,lstr_exist_inv.ld_issdt)
end event

type dw_existing_invno from u_pics_dw within w_magazine_existing_invoices
int X=385
int Y=341
int Width=426
int Height=325
int TabOrder=0
string DataObject="d_existing_invno"
boolean LiveScroll=false
end type

event constructor;call super::constructor;this.of_settransobject(SqlServerTrans)

end event

type cb_ok from commandbutton within w_magazine_existing_invoices
int X=421
int Y=705
int Width=348
int Height=89
int TabOrder=10
string Text="&OK"
boolean Default=true
int TextSize=-10
int Weight=400
string FaceName="MS Sans Serif"
FontFamily FontFamily=Swiss!
FontPitch FontPitch=Variable!
end type

event clicked;close(w_magazine_existing_invoices)
end event

type st_1 from statictext within w_magazine_existing_invoices
int X=69
int Y=29
int Width=1148
int Height=269
boolean Enabled=false
boolean Border=true
BorderStyle BorderStyle=StyleRaised!
boolean FocusRectangle=false
long BackColor=79741120
int TextSize=-10
int Weight=400
string FaceName="MS Sans Serif"
FontFamily FontFamily=Swiss!
FontPitch FontPitch=Variable!
end type


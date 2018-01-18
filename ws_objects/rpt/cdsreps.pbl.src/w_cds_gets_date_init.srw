$PBExportHeader$w_cds_gets_date_init.srw
forward
global type w_cds_gets_date_init from w_response
end type
type cb_ok from u_cb within w_cds_gets_date_init
end type
type cb_cancel from u_cb within w_cds_gets_date_init
end type
type em_stdt from u_em within w_cds_gets_date_init
end type
type st_1 from u_st within w_cds_gets_date_init
end type
type st_2 from statictext within w_cds_gets_date_init
end type
type em_init from u_em within w_cds_gets_date_init
end type
end forward

global type w_cds_gets_date_init from w_response
integer x = 997
integer y = 500
integer width = 1125
integer height = 576
string title = "Annotations Written"
cb_ok cb_ok
cb_cancel cb_cancel
em_stdt em_stdt
st_1 st_1
st_2 st_2
em_init em_init
end type
global w_cds_gets_date_init w_cds_gets_date_init

type variables
string is_annoinit
end variables

on w_cds_gets_date_init.create
int iCurrent
call super::create
this.cb_ok=create cb_ok
this.cb_cancel=create cb_cancel
this.em_stdt=create em_stdt
this.st_1=create st_1
this.st_2=create st_2
this.em_init=create em_init
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.cb_ok
this.Control[iCurrent+2]=this.cb_cancel
this.Control[iCurrent+3]=this.em_stdt
this.Control[iCurrent+4]=this.st_1
this.Control[iCurrent+5]=this.st_2
this.Control[iCurrent+6]=this.em_init
end on

on w_cds_gets_date_init.destroy
call super::destroy
destroy(this.cb_ok)
destroy(this.cb_cancel)
destroy(this.em_stdt)
destroy(this.st_1)
destroy(this.st_2)
destroy(this.em_init)
end on

event key;call super::key;//messagebox('key event','happened')
IF KeyDown(KeyEnter!) THEN 
  cb_ok.TriggerEvent(Clicked!)
ELSEIF KeyDown(KeyEscape!) THEN 
  cb_cancel.TriggerEvent(Clicked!)
END IF


end event

event open;call super::open;em_stdt.SetFocus()
end event

event pfc_open;call super::pfc_open;em_stdt.SetFocus()
end event

event pfc_postopen;call super::pfc_postopen;//datawindowchild dwc
//dw_init.GetChild('annoinit',dwc )
//dwc.SetTransObject (SqlServerTrans)
//dwc.Retrieve( )
//dw_init.SetTransObject(SqlServerTrans )
//dw_init.Retrieve ()
//
end event

type cb_ok from u_cb within w_cds_gets_date_init
integer x = 96
integer y = 324
integer width = 315
integer height = 84
integer taborder = 30
integer textsize = -10
string text = "&OK"
end type

event clicked;call super::clicked;date ld_stdt
string ls_cntr
str_prod_cntr lstr_prod_cntr
ib_disableclosequery = TRUE	
IF NOT ISDATE(em_stdt.Text)  or IsNull(is_annoinit) or is_annoinit = "" Then
		MessageBox("Error","Expecting start date and annotation initials")
		em_stdt.SetFocus()
		return
ElseIF IsDate(em_stdt.text) and IsNull(is_annoinit) or is_annoinit = "" Then
		MessageBox("Error","Expecting start date and annotation initials")
		em_init.SetFocus()
		Return
Else
	lstr_prod_cntr.ld_stdt = date(em_stdt.text)
	lstr_prod_cntr.ls_cntr = is_annoinit
	
//	OpenWithParm(w_cds_reports,lstr_prod_cntr)
	CloseWithReturn(w_cds_gets_date_init,lstr_prod_cntr)	
End IF




end event

type cb_cancel from u_cb within w_cds_gets_date_init
integer x = 567
integer y = 324
integer width = 315
integer height = 84
integer taborder = 40
integer textsize = -10
string text = "&Cancel"
end type

event clicked;call super::clicked;ib_disableclosequery = TRUE
close(w_cds_gets_date_init)
//close(w_cds_reports)
RETURN

end event

type em_stdt from u_em within w_cds_gets_date_init
integer x = 539
integer y = 56
integer width = 453
integer height = 84
integer taborder = 10
integer textsize = -10
integer weight = 700
alignment alignment = center!
maskdatatype maskdatatype = datemask!
string mask = "mm/dd/yyyy"
boolean autoskip = true
string displaydata = ""
double increment = 0
string minmax = ""
end type

event modified;call super::modified;//messagebox('modified event','happened')
cb_ok.Enabled = True
cb_ok.Default = true
end event

type st_1 from u_st within w_cds_gets_date_init
integer x = 37
integer y = 56
integer width = 306
integer height = 84
integer textsize = -10
string text = "Start Date:"
end type

type st_2 from statictext within w_cds_gets_date_init
integer x = 37
integer y = 208
integer width = 498
integer height = 84
boolean bringtotop = true
integer textsize = -10
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long backcolor = 78164112
boolean enabled = false
string text = "Annotation Initials:"
boolean focusrectangle = false
end type

type em_init from u_em within w_cds_gets_date_init
integer x = 539
integer y = 208
integer width = 453
integer height = 84
integer taborder = 20
integer textsize = -10
integer weight = 700
textcase textcase = upper!
maskdatatype maskdatatype = stringmask!
string mask = "xxxxxxxxxxxxx"
string displaydata = "H"
double increment = 0
string minmax = ""
end type

event modified;call super::modified;is_annoinit = em_init.text
end event


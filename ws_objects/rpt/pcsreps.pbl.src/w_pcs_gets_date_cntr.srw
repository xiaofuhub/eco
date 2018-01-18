$PBExportHeader$w_pcs_gets_date_cntr.srw
forward
global type w_pcs_gets_date_cntr from w_response
end type
type cb_ok from u_cb within w_pcs_gets_date_cntr
end type
type cb_cancel from u_cb within w_pcs_gets_date_cntr
end type
type em_stdt from u_em within w_pcs_gets_date_cntr
end type
type st_1 from u_st within w_pcs_gets_date_cntr
end type
type em_cntr from u_em within w_pcs_gets_date_cntr
end type
type st_2 from statictext within w_pcs_gets_date_cntr
end type
end forward

global type w_pcs_gets_date_cntr from w_response
integer x = 997
integer y = 500
integer width = 969
integer height = 628
string title = "Title Listing Report"
boolean controlmenu = false
cb_ok cb_ok
cb_cancel cb_cancel
em_stdt em_stdt
st_1 st_1
em_cntr em_cntr
st_2 st_2
end type
global w_pcs_gets_date_cntr w_pcs_gets_date_cntr

on w_pcs_gets_date_cntr.create
int iCurrent
call super::create
this.cb_ok=create cb_ok
this.cb_cancel=create cb_cancel
this.em_stdt=create em_stdt
this.st_1=create st_1
this.em_cntr=create em_cntr
this.st_2=create st_2
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.cb_ok
this.Control[iCurrent+2]=this.cb_cancel
this.Control[iCurrent+3]=this.em_stdt
this.Control[iCurrent+4]=this.st_1
this.Control[iCurrent+5]=this.em_cntr
this.Control[iCurrent+6]=this.st_2
end on

on w_pcs_gets_date_cntr.destroy
call super::destroy
destroy(this.cb_ok)
destroy(this.cb_cancel)
destroy(this.em_stdt)
destroy(this.st_1)
destroy(this.em_cntr)
destroy(this.st_2)
end on

event key;call super::key;IF KeyDown(KeyEnter!) THEN 
  cb_ok.TriggerEvent(Clicked!)
ELSEIF KeyDown(KeyEscape!) THEN 
  cb_cancel.TriggerEvent(Clicked!)
END IF


end event

event open;call super::open;em_stdt.SetFocus()
end event

event pfc_open;call super::pfc_open;em_stdt.SetFocus()
end event

type cb_ok from u_cb within w_pcs_gets_date_cntr
integer x = 55
integer y = 392
integer width = 315
integer height = 84
integer taborder = 30
integer textsize = -10
string text = "&OK"
end type

event clicked;call super::clicked;date ld_stdt
string ls_cntr
str_prod_cntr lstr_prod_cntr
	
IF NOT ISDATE(em_stdt.Text)  and IsNull(em_cntr.text) or &
		em_cntr.text = "" Then
		MessageBox("Error","Expecting start date and contract number!")
		em_stdt.SetFocus()
ElseIF ISDATE(em_stdt.Text)  and IsNull(em_cntr.text) or &
		em_cntr.text = "" Then
		MessageBox("Error","Expecting start date and contract number!")
		em_cntr.SetFocus()
		Return
Else
	lstr_prod_cntr.ld_stdt = date(em_stdt.text)
	lstr_prod_cntr.ls_cntr = string(em_cntr.text)
	OpenWithParm(w_pcs_reports,lstr_prod_cntr)
	CloseWithReturn(w_pcs_gets_date_cntr,lstr_prod_cntr)	
End IF




end event

type cb_cancel from u_cb within w_pcs_gets_date_cntr
integer x = 530
integer y = 392
integer width = 315
integer height = 84
integer taborder = 40
integer textsize = -10
string text = "&Cancel"
end type

event clicked;call super::clicked;ib_disableclosequery = TRUE
close(parent)
close(w_pcs_reports)

end event

type em_stdt from u_em within w_pcs_gets_date_cntr
integer x = 530
integer y = 76
integer width = 370
integer height = 88
integer taborder = 10
integer textsize = -10
integer weight = 700
alignment alignment = center!
maskdatatype maskdatatype = datemask!
string mask = "mm/dd/yyyy"
boolean autoskip = true
string displaydata = ""
double increment = 0
string minmax = ""
end type

type st_1 from u_st within w_pcs_gets_date_cntr
integer x = 64
integer y = 76
integer width = 457
integer height = 80
integer textsize = -10
string text = "Assigned date:"
end type

type em_cntr from u_em within w_pcs_gets_date_cntr
integer x = 530
integer y = 220
integer width = 352
integer height = 88
integer taborder = 20
integer textsize = -10
integer weight = 700
alignment alignment = center!
textcase textcase = upper!
maskdatatype maskdatatype = stringmask!
string mask = ""
boolean autoskip = true
string displaydata = "P"
double increment = 0
string minmax = ""
end type

event modified;call super::modified;cb_ok.Enabled = True
cb_ok.Default = true
end event

type st_2 from statictext within w_pcs_gets_date_cntr
integer x = 50
integer y = 228
integer width = 471
integer height = 72
boolean bringtotop = true
integer textsize = -10
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long backcolor = 80263581
boolean enabled = false
string text = "Contract number:"
boolean focusrectangle = false
end type


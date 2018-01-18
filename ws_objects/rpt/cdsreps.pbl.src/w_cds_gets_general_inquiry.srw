$PBExportHeader$w_cds_gets_general_inquiry.srw
forward
global type w_cds_gets_general_inquiry from w_response
end type
type cb_clear from commandbutton within w_cds_gets_general_inquiry
end type
type st_2 from statictext within w_cds_gets_general_inquiry
end type
type st_11 from u_st within w_cds_gets_general_inquiry
end type
type em_assigndt from u_em within w_cds_gets_general_inquiry
end type
type em_recdt from u_em within w_cds_gets_general_inquiry
end type
type em_compdt from u_em within w_cds_gets_general_inquiry
end type
type em_auth from u_em within w_cds_gets_general_inquiry
end type
type st_10 from u_st within w_cds_gets_general_inquiry
end type
type st_9 from u_st within w_cds_gets_general_inquiry
end type
type st_6 from u_st within w_cds_gets_general_inquiry
end type
type cb_ok from u_cb within w_cds_gets_general_inquiry
end type
type cb_cancel from u_cb within w_cds_gets_general_inquiry
end type
type em_chno from u_em within w_cds_gets_general_inquiry
end type
type st_1 from u_st within w_cds_gets_general_inquiry
end type
type st_3 from statictext within w_cds_gets_general_inquiry
end type
type em_conno from u_em within w_cds_gets_general_inquiry
end type
type st_5 from statictext within w_cds_gets_general_inquiry
end type
type em_bkno from u_em within w_cds_gets_general_inquiry
end type
type st_7 from statictext within w_cds_gets_general_inquiry
end type
type em_title from u_em within w_cds_gets_general_inquiry
end type
type st_8 from statictext within w_cds_gets_general_inquiry
end type
end forward

shared variables

end variables

global type w_cds_gets_general_inquiry from w_response
integer x = 201
integer y = 300
integer width = 2391
integer height = 1440
string title = "General Inquiry Criteria Screen"
cb_clear cb_clear
st_2 st_2
st_11 st_11
em_assigndt em_assigndt
em_recdt em_recdt
em_compdt em_compdt
em_auth em_auth
st_10 st_10
st_9 st_9
st_6 st_6
cb_ok cb_ok
cb_cancel cb_cancel
em_chno em_chno
st_1 st_1
st_3 st_3
em_conno em_conno
st_5 st_5
em_bkno em_bkno
st_7 st_7
em_title em_title
st_8 st_8
end type
global w_cds_gets_general_inquiry w_cds_gets_general_inquiry

type variables

end variables

forward prototypes
public subroutine wf_clear_scr ()
end prototypes

public subroutine wf_clear_scr ();
end subroutine

on w_cds_gets_general_inquiry.create
int iCurrent
call super::create
this.cb_clear=create cb_clear
this.st_2=create st_2
this.st_11=create st_11
this.em_assigndt=create em_assigndt
this.em_recdt=create em_recdt
this.em_compdt=create em_compdt
this.em_auth=create em_auth
this.st_10=create st_10
this.st_9=create st_9
this.st_6=create st_6
this.cb_ok=create cb_ok
this.cb_cancel=create cb_cancel
this.em_chno=create em_chno
this.st_1=create st_1
this.st_3=create st_3
this.em_conno=create em_conno
this.st_5=create st_5
this.em_bkno=create em_bkno
this.st_7=create st_7
this.em_title=create em_title
this.st_8=create st_8
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.cb_clear
this.Control[iCurrent+2]=this.st_2
this.Control[iCurrent+3]=this.st_11
this.Control[iCurrent+4]=this.em_assigndt
this.Control[iCurrent+5]=this.em_recdt
this.Control[iCurrent+6]=this.em_compdt
this.Control[iCurrent+7]=this.em_auth
this.Control[iCurrent+8]=this.st_10
this.Control[iCurrent+9]=this.st_9
this.Control[iCurrent+10]=this.st_6
this.Control[iCurrent+11]=this.cb_ok
this.Control[iCurrent+12]=this.cb_cancel
this.Control[iCurrent+13]=this.em_chno
this.Control[iCurrent+14]=this.st_1
this.Control[iCurrent+15]=this.st_3
this.Control[iCurrent+16]=this.em_conno
this.Control[iCurrent+17]=this.st_5
this.Control[iCurrent+18]=this.em_bkno
this.Control[iCurrent+19]=this.st_7
this.Control[iCurrent+20]=this.em_title
this.Control[iCurrent+21]=this.st_8
end on

on w_cds_gets_general_inquiry.destroy
call super::destroy
destroy(this.cb_clear)
destroy(this.st_2)
destroy(this.st_11)
destroy(this.em_assigndt)
destroy(this.em_recdt)
destroy(this.em_compdt)
destroy(this.em_auth)
destroy(this.st_10)
destroy(this.st_9)
destroy(this.st_6)
destroy(this.cb_ok)
destroy(this.cb_cancel)
destroy(this.em_chno)
destroy(this.st_1)
destroy(this.st_3)
destroy(this.em_conno)
destroy(this.st_5)
destroy(this.em_bkno)
destroy(this.st_7)
destroy(this.em_title)
destroy(this.st_8)
end on

event key;call super::key;IF KeyDown(KeyEnter!) THEN 
  cb_ok.TriggerEvent(Clicked!)
ELSEIF KeyDown(KeyEscape!) THEN 
  cb_cancel.TriggerEvent(Clicked!)
END IF


end event

event open;call super::open;SetNull(em_chno.text)
SetNull(em_conno.text)
SetNull(em_bkno.text)
em_chno.SetFocus()
end event

type cb_clear from commandbutton within w_cds_gets_general_inquiry
integer x = 933
integer y = 1160
integer width = 334
integer height = 96
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "C&lear"
end type

event clicked;em_chno.Enabled = TRUE
em_assigndt.Enabled = TRUE
em_auth.Enabled = TRUE
em_compdt.Enabled = TRUE
em_recdt.Enabled = TRUE
em_title.Enabled = TRUE
em_conno.Enabled = TRUE
em_bkno.Enabled = TRUE

em_chno.Text = ""
em_assigndt.Text = ""
em_auth.Text = ""
em_compdt.Text = ""
em_recdt.Text = ""
em_title.Text = ""
em_conno.Text = ""
em_bkno.Text = ""

em_chno.SetFocus()
end event

type st_2 from statictext within w_cds_gets_general_inquiry
integer x = 503
integer y = 52
integer width = 1463
integer height = 88
integer textsize = -12
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
boolean underline = true
long textcolor = 33554432
long backcolor = 80263581
string text = "Please select only one of these criteria"
boolean focusrectangle = false
end type

type st_11 from u_st within w_cds_gets_general_inquiry
integer x = 55
integer y = 648
integer width = 421
integer height = 76
integer textsize = -10
string text = "Assigned Date"
end type

type em_assigndt from u_em within w_cds_gets_general_inquiry
integer x = 512
integer y = 640
integer width = 407
integer height = 88
integer taborder = 70
integer textsize = -10
integer weight = 700
alignment alignment = center!
maskdatatype maskdatatype = datemask!
string mask = "mm/dd/yyyy"
boolean autoskip = true
string displaydata = "~r"
double increment = 0
string minmax = ""
end type

event modified;call super::modified;cb_ok.Enabled = True
cb_ok.Default = true

em_chno.Enabled = FALSE
em_auth.Enabled = FALSE
em_compdt.Enabled = FALSE
em_recdt.Enabled = FALSE
em_title.Enabled = FALSE
em_conno.Enabled = FALSE
em_bkno.Enabled = FALSE

end event

type em_recdt from u_em within w_cds_gets_general_inquiry
integer x = 1554
integer y = 488
integer width = 407
integer height = 88
integer taborder = 60
integer textsize = -10
integer weight = 700
alignment alignment = center!
maskdatatype maskdatatype = datemask!
string mask = "mm/dd/yyyy"
boolean autoskip = true
string displaydata = "~r"
double increment = 0
string minmax = ""
end type

event modified;call super::modified;cb_ok.Enabled = True
cb_ok.Default = true

em_assigndt.Enabled = FALSE
em_auth.Enabled = FALSE
em_compdt.Enabled = FALSE
em_chno.Enabled = FALSE
em_title.Enabled = FALSE
em_conno.Enabled = FALSE
em_bkno.Enabled = FALSE

end event

type em_compdt from u_em within w_cds_gets_general_inquiry
integer x = 1554
integer y = 352
integer width = 407
integer height = 88
integer taborder = 40
integer textsize = -10
integer weight = 700
alignment alignment = center!
maskdatatype maskdatatype = datemask!
string mask = "mm/dd/yyyy"
boolean autoskip = true
string displaydata = "~r"
double increment = 0
string minmax = ""
end type

event modified;call super::modified;cb_ok.Enabled = True
cb_ok.Default = true

em_assigndt.Enabled = FALSE
em_auth.Enabled = FALSE
em_chno.Enabled = FALSE
em_recdt.Enabled = FALSE
em_title.Enabled = FALSE
em_conno.Enabled = FALSE
em_bkno.Enabled = FALSE

end event

type em_auth from u_em within w_cds_gets_general_inquiry
integer x = 1554
integer y = 224
integer width = 768
integer height = 88
integer taborder = 20
integer textsize = -10
integer weight = 700
alignment alignment = center!
maskdatatype maskdatatype = stringmask!
string mask = ""
boolean autoskip = true
string displaydata = ""
double increment = 0
string minmax = ""
end type

event modified;call super::modified;cb_ok.Enabled = True
cb_ok.Default = true

em_assigndt.Enabled = FALSE
em_chno.Enabled = FALSE
em_compdt.Enabled = FALSE
em_recdt.Enabled = FALSE
em_title.Enabled = FALSE
em_conno.Enabled = FALSE
em_bkno.Enabled = FALSE


end event

type st_10 from u_st within w_cds_gets_general_inquiry
integer x = 1111
integer y = 488
integer width = 421
integer height = 68
integer textsize = -10
string text = "QA Recieved"
end type

type st_9 from u_st within w_cds_gets_general_inquiry
integer x = 1111
integer y = 352
integer width = 421
integer height = 68
integer textsize = -10
string text = "QA Completed"
end type

type st_6 from u_st within w_cds_gets_general_inquiry
integer x = 1079
integer y = 224
integer width = 453
integer height = 68
integer textsize = -10
string text = "Author lastname"
end type

type cb_ok from u_cb within w_cds_gets_general_inquiry
integer x = 512
integer y = 1160
integer taborder = 0
integer textsize = -10
string text = "&OK"
end type

event clicked;call super::clicked;string Lchart_no


IF IsNull(em_chno.text)=FALSE AND em_chno.text <> ""  Then
	Lchart_no = '1' + em_chno.text
	OpenWithParm(w_pcs_general_inquiry_screen,Lchart_no)
	CloseWithReturn(w_cds_gets_general_inquiry,Lchart_no)
ELSEIF IsNull(em_conno.text)=FALSE AND em_conno.text <> ""  Then
	Lchart_no = '2' + em_conno.text
	OpenWithParm(w_pcs_general_inquiry_screen,Lchart_no)
	CloseWithReturn(w_cds_gets_general_inquiry,Lchart_no)
ELSEIF IsNull(em_bkno.text)=FALSE AND em_bkno.text <> ""  Then
	Lchart_no = '3' + em_bkno.text
	OpenWithParm(w_pcs_general_inquiry_screen,Lchart_no)
	CloseWithReturn(w_cds_gets_general_inquiry,Lchart_no)	
ELSEIF IsNull(em_title.text)=FALSE AND em_title.text <> ""  Then
	Lchart_no = '4' + em_title.text
	OpenWithParm(w_pcs_general_inquiry_screen,Lchart_no)
	CloseWithReturn(w_cds_gets_general_inquiry,Lchart_no)
ELSEIF IsNull(em_auth.text)=FALSE AND em_auth.text <> ""  Then
	Lchart_no = '5' + em_auth.text
	OpenWithParm(w_pcs_general_inquiry_screen,Lchart_no)
	CloseWithReturn(w_cds_gets_general_inquiry,Lchart_no)
ELSEIF IsNull(em_compdt.text)=FALSE AND em_compdt.text <> ""  Then
	Lchart_no = '6' + em_compdt.text
	OpenWithParm(w_pcs_general_inquiry_screen,Lchart_no)
	CloseWithReturn(w_cds_gets_general_inquiry,Lchart_no)
ELSEIF IsNull(em_recdt.text)=FALSE AND em_recdt.text <> ""  Then
	Lchart_no = '7' + em_recdt.text
	OpenWithParm(w_pcs_general_inquiry_screen,Lchart_no)
	CloseWithReturn(w_cds_gets_general_inquiry,Lchart_no)	
ELSEIF IsNull(em_assigndt.text)=FALSE AND em_assigndt.text <> ""  Then
	Lchart_no = '8' + em_assigndt.text
	OpenWithParm(w_pcs_general_inquiry_screen,Lchart_no)
	CloseWithReturn(w_cds_gets_general_inquiry,Lchart_no)	
END IF
end event

type cb_cancel from u_cb within w_cds_gets_general_inquiry
integer x = 1335
integer y = 1160
integer taborder = 0
integer textsize = -10
string text = "&Cancel"
end type

event clicked;call super::clicked;ib_disableclosequery = TRUE
close(w_cds_gets_general_inquiry)
close(w_pcs_general_inquiry_screen)
Return
end event

type em_chno from u_em within w_cds_gets_general_inquiry
integer x = 512
integer y = 224
integer width = 370
integer height = 88
integer taborder = 10
integer textsize = -10
integer weight = 700
alignment alignment = center!
maskdatatype maskdatatype = stringmask!
string mask = "######"
boolean autoskip = true
string displaydata = ""
double increment = 0
string minmax = ""
end type

event modified;call super::modified;cb_ok.Enabled = True
cb_ok.Default = true

em_assigndt.Enabled = FALSE
em_auth.Enabled = FALSE
em_compdt.Enabled = FALSE
em_recdt.Enabled = FALSE
em_title.Enabled = FALSE
em_conno.Enabled = FALSE
em_bkno.Enabled = FALSE

end event

type st_1 from u_st within w_cds_gets_general_inquiry
integer x = 69
integer y = 224
integer width = 421
integer height = 68
integer textsize = -10
string text = " Chart Number"
end type

type st_3 from statictext within w_cds_gets_general_inquiry
integer x = 32
integer y = 352
integer width = 439
integer height = 76
boolean bringtotop = true
integer textsize = -10
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long backcolor = 80263581
boolean enabled = false
string text = "Control Number"
boolean focusrectangle = false
end type

type em_conno from u_em within w_cds_gets_general_inquiry
event modified pbm_enmodified
integer x = 512
integer y = 352
integer width = 498
integer height = 88
integer taborder = 30
integer textsize = -10
integer weight = 700
alignment alignment = center!
maskdatatype maskdatatype = stringmask!
string mask = "########"
boolean autoskip = true
string displaydata = "~r"
double increment = 0
string minmax = ""
end type

event modified;cb_ok.Enabled = True
cb_ok.Default = true

em_assigndt.Enabled = FALSE
em_auth.Enabled = FALSE
em_compdt.Enabled = FALSE
em_recdt.Enabled = FALSE
em_title.Enabled = FALSE
em_chno.Enabled = FALSE
em_bkno.Enabled = FALSE


end event

type st_5 from statictext within w_cds_gets_general_inquiry
integer x = 96
integer y = 488
integer width = 393
integer height = 76
boolean bringtotop = true
integer textsize = -10
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long backcolor = 80263581
boolean enabled = false
string text = "Book Number"
boolean focusrectangle = false
end type

type em_bkno from u_em within w_cds_gets_general_inquiry
event modified pbm_enmodified
integer x = 512
integer y = 488
integer width = 416
integer height = 88
integer taborder = 50
integer textsize = -10
integer weight = 700
alignment alignment = center!
string mask = "########"
string displaydata = ""
double increment = 0
string minmax = ""
end type

event modified;cb_ok.Enabled = True
cb_ok.Default = true

em_assigndt.Enabled = FALSE
em_auth.Enabled = FALSE
em_compdt.Enabled = FALSE
em_recdt.Enabled = FALSE
em_title.Enabled = FALSE
em_conno.Enabled = FALSE
em_bkno.Enabled = FALSE
end event

type st_7 from statictext within w_cds_gets_general_inquiry
integer x = 338
integer y = 820
integer width = 151
integer height = 76
boolean bringtotop = true
integer textsize = -10
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long backcolor = 80263581
boolean enabled = false
string text = "Title"
boolean focusrectangle = false
end type

type em_title from u_em within w_cds_gets_general_inquiry
integer x = 512
integer y = 804
integer width = 1394
integer height = 104
integer taborder = 80
integer textsize = -10
integer weight = 700
alignment alignment = center!
maskdatatype maskdatatype = stringmask!
string mask = ""
string displaydata = "Ä"
double increment = 0
string minmax = ""
end type

event modified;call super::modified;cb_ok.Enabled = True
cb_ok.Default = true

em_assigndt.Enabled = FALSE
em_auth.Enabled = FALSE
em_compdt.Enabled = FALSE
em_recdt.Enabled = FALSE
em_conno.Enabled = FALSE
em_bkno.Enabled = FALSE
em_chno.Enabled = FALSE
end event

type st_8 from statictext within w_cds_gets_general_inquiry
integer x = 503
integer y = 916
integer width = 1422
integer height = 156
boolean bringtotop = true
integer textsize = -10
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long backcolor = 80263581
boolean enabled = false
string text = "To use wild cards, type part of the title and terminate it with the % sign.  For example: Good Morning%"
boolean focusrectangle = false
end type


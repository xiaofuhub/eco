$PBExportHeader$w_cds_gets_title_info.srw
forward
global type w_cds_gets_title_info from w_response
end type
type cb_ok from u_cb within w_cds_gets_title_info
end type
type cb_cancel from u_cb within w_cds_gets_title_info
end type
type em_chno from u_em within w_cds_gets_title_info
end type
type st_1 from u_st within w_cds_gets_title_info
end type
type st_2 from statictext within w_cds_gets_title_info
end type
type st_3 from statictext within w_cds_gets_title_info
end type
type em_conno from u_em within w_cds_gets_title_info
end type
type st_4 from statictext within w_cds_gets_title_info
end type
type st_5 from statictext within w_cds_gets_title_info
end type
type em_bkno from u_em within w_cds_gets_title_info
end type
type st_6 from statictext within w_cds_gets_title_info
end type
type st_7 from statictext within w_cds_gets_title_info
end type
type em_title from u_em within w_cds_gets_title_info
end type
type st_8 from statictext within w_cds_gets_title_info
end type
end forward

shared variables

end variables

global type w_cds_gets_title_info from w_response
integer x = 946
integer y = 520
integer width = 1541
integer height = 1276
string title = "Title Information List Report"
cb_ok cb_ok
cb_cancel cb_cancel
em_chno em_chno
st_1 st_1
st_2 st_2
st_3 st_3
em_conno em_conno
st_4 st_4
st_5 st_5
em_bkno em_bkno
st_6 st_6
st_7 st_7
em_title em_title
st_8 st_8
end type
global w_cds_gets_title_info w_cds_gets_title_info

type variables

end variables

forward prototypes
public subroutine wf_clear_scr ()
end prototypes

public subroutine wf_clear_scr ();em_bkno.Text = ""
em_chno.Text = ""
em_conno.Text = ""
em_title.Text = ""
end subroutine

on w_cds_gets_title_info.create
int iCurrent
call super::create
this.cb_ok=create cb_ok
this.cb_cancel=create cb_cancel
this.em_chno=create em_chno
this.st_1=create st_1
this.st_2=create st_2
this.st_3=create st_3
this.em_conno=create em_conno
this.st_4=create st_4
this.st_5=create st_5
this.em_bkno=create em_bkno
this.st_6=create st_6
this.st_7=create st_7
this.em_title=create em_title
this.st_8=create st_8
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.cb_ok
this.Control[iCurrent+2]=this.cb_cancel
this.Control[iCurrent+3]=this.em_chno
this.Control[iCurrent+4]=this.st_1
this.Control[iCurrent+5]=this.st_2
this.Control[iCurrent+6]=this.st_3
this.Control[iCurrent+7]=this.em_conno
this.Control[iCurrent+8]=this.st_4
this.Control[iCurrent+9]=this.st_5
this.Control[iCurrent+10]=this.em_bkno
this.Control[iCurrent+11]=this.st_6
this.Control[iCurrent+12]=this.st_7
this.Control[iCurrent+13]=this.em_title
this.Control[iCurrent+14]=this.st_8
end on

on w_cds_gets_title_info.destroy
call super::destroy
destroy(this.cb_ok)
destroy(this.cb_cancel)
destroy(this.em_chno)
destroy(this.st_1)
destroy(this.st_2)
destroy(this.st_3)
destroy(this.em_conno)
destroy(this.st_4)
destroy(this.st_5)
destroy(this.em_bkno)
destroy(this.st_6)
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

type cb_ok from u_cb within w_cds_gets_title_info
integer x = 283
integer y = 1048
integer taborder = 0
integer textsize = -10
string text = "&OK"
end type

event clicked;call super::clicked;string Lchart_no


IF IsNull(em_chno.text) OR em_chno.text = ""  Then
	IF IsNull(em_conno.text) OR em_conno.text = ""  Then
		IF IsNull(em_bkno.text) OR em_bkno.text = ""  Then
			IF IsNull(em_title.text) OR em_title.text = ""  Then
				MessageBox("Error","You must enter a control number or a chart number or a book number or a title.")
				wf_clear_scr()
				em_chno.SetFocus()
				RETURN
			ELSE
				Lchart_no = '4' + em_title.text
				OpenWithParm(w_cds_reports,Lchart_no)
				CloseWithReturn(w_cds_gets_title_info,Lchart_no)
			END IF
		ELSE
			IF IsNull(em_title.text) OR em_title.text = ""  Then
				Lchart_no = '3' + em_bkno.text
				OpenWithParm(w_cds_reports,Lchart_no)
				CloseWithReturn(w_cds_gets_title_info,Lchart_no)
			ELSE
				MessageBox("Error","You must enter a control number or a chart number or a book number or a title.")
				wf_clear_scr()
				em_chno.SetFocus()
				RETURN
			END IF
		END IF
	ELSE
		IF IsNull(em_bkno.text) OR em_bkno.text = ""  Then
			Lchart_no = '2' + em_conno.text
			OpenWithParm(w_cds_reports,Lchart_no)
			CloseWithReturn(w_cds_gets_title_info,Lchart_no)
		ELSE
			MessageBox("Error","You must enter a control number or a chart number or a book number or a title.")
			wf_clear_scr()
			em_chno.SetFocus()
			RETURN
		END IF
	END IF
ELSE
	IF IsNull(em_conno.text) OR em_conno.text = ""  Then	
		IF IsNull(em_bkno.text) OR em_bkno.text = ""  Then
			Lchart_no = '1' + em_chno.text
			OpenWithParm(w_cds_reports,Lchart_no)
			CloseWithReturn(w_cds_gets_title_info,Lchart_no)
		ELSE
			MessageBox("Error","You must enter a control number or a chart number or a book number or a title.")
			wf_clear_scr()
			em_chno.SetFocus()
			RETURN
		END IF
	ELSE
		MessageBox("Error","You must enter a control number or a chart number or a book number or a title.")
		wf_clear_scr()
		em_chno.SetFocus()
		RETURN
	END IF
END IF
end event

type cb_cancel from u_cb within w_cds_gets_title_info
integer x = 759
integer y = 1048
integer taborder = 0
integer textsize = -10
string text = "&Cancel"
end type

event clicked;call super::clicked;ib_disableclosequery = TRUE
close(w_cds_gets_title_info)
close(w_cds_reports)
Return
end event

type em_chno from u_em within w_cds_gets_title_info
integer x = 695
integer y = 68
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
end event

type st_1 from u_st within w_cds_gets_title_info
integer x = 251
integer y = 68
integer width = 421
integer height = 68
integer textsize = -10
string text = " Chart Number"
end type

type st_2 from statictext within w_cds_gets_title_info
integer x = 430
integer y = 156
integer width = 82
integer height = 76
boolean bringtotop = true
integer textsize = -10
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long backcolor = 80263581
boolean enabled = false
string text = "or"
boolean focusrectangle = false
end type

type st_3 from statictext within w_cds_gets_title_info
integer x = 233
integer y = 236
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

type em_conno from u_em within w_cds_gets_title_info
event modified pbm_enmodified
integer x = 690
integer y = 224
integer width = 498
integer height = 88
integer taborder = 20
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

event modified;call super::modified;cb_ok.Enabled = True
cb_ok.Default = true
end event

type st_4 from statictext within w_cds_gets_title_info
integer x = 421
integer y = 352
integer width = 87
integer height = 76
boolean bringtotop = true
integer textsize = -10
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long backcolor = 80263581
boolean enabled = false
string text = "or"
boolean focusrectangle = false
end type

type st_5 from statictext within w_cds_gets_title_info
integer x = 279
integer y = 428
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

type em_bkno from u_em within w_cds_gets_title_info
event modified pbm_enmodified
integer x = 695
integer y = 412
integer width = 416
integer height = 88
integer taborder = 30
integer textsize = -10
integer weight = 700
alignment alignment = center!
string mask = "########"
string displaydata = ""
double increment = 0
string minmax = ""
end type

event modified;call super::modified;cb_ok.Enabled = True
cb_ok.Default = true
end event

type st_6 from statictext within w_cds_gets_title_info
integer x = 421
integer y = 552
integer width = 87
integer height = 76
boolean bringtotop = true
integer textsize = -10
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long backcolor = 80263581
boolean enabled = false
string text = "or"
boolean focusrectangle = false
end type

type st_7 from statictext within w_cds_gets_title_info
integer x = 507
integer y = 552
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

type em_title from u_em within w_cds_gets_title_info
integer x = 73
integer y = 668
integer width = 1394
integer height = 104
integer taborder = 40
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
end event

type st_8 from statictext within w_cds_gets_title_info
integer x = 55
integer y = 796
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


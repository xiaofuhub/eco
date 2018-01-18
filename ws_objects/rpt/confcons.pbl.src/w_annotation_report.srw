$PBExportHeader$w_annotation_report.srw
forward
global type w_annotation_report from Window
end type
type dw_annotrpt_ec_report from u_pics_dw within w_annotation_report
end type
type em_conno from uo_conno within w_annotation_report
end type
type st_2 from statictext within w_annotation_report
end type
type cb_cancel from commandbutton within w_annotation_report
end type
type cb_ok from commandbutton within w_annotation_report
end type
type st_1 from statictext within w_annotation_report
end type
end forward

global type w_annotation_report from Window
int X=846
int Y=461
int Width=1212
int Height=733
boolean TitleBar=true
string Title="Annotation Report"
long BackColor=79741120
boolean ControlMenu=true
WindowType WindowType=response!
event ue_postevent ( )
dw_annotrpt_ec_report dw_annotrpt_ec_report
em_conno em_conno
st_2 st_2
cb_cancel cb_cancel
cb_ok cb_ok
st_1 st_1
end type
global w_annotation_report w_annotation_report

type variables

end variables

event key;IF KeyDown(KeyEnter!) THEN 
  cb_ok.TriggerEvent(Clicked!)
ELSEIF KeyDown(KeyEscape!) THEN 
  cb_cancel.TriggerEvent(Clicked!)
END IF


end event

on w_annotation_report.create
this.dw_annotrpt_ec_report=create dw_annotrpt_ec_report
this.em_conno=create em_conno
this.st_2=create st_2
this.cb_cancel=create cb_cancel
this.cb_ok=create cb_ok
this.st_1=create st_1
this.Control[]={ this.dw_annotrpt_ec_report,&
this.em_conno,&
this.st_2,&
this.cb_cancel,&
this.cb_ok,&
this.st_1}
end on

on w_annotation_report.destroy
destroy(this.dw_annotrpt_ec_report)
destroy(this.em_conno)
destroy(this.st_2)
destroy(this.cb_cancel)
destroy(this.cb_ok)
destroy(this.st_1)
end on

type dw_annotrpt_ec_report from u_pics_dw within w_annotation_report
int X=865
int Y=241
int Width=243
int Height=137
int TabOrder=11
boolean Visible=false
string DataObject="d_annotrpt_ec_report"
boolean VScrollBar=false
boolean LiveScroll=false
end type

event constructor;call super::constructor;This.of_SetTransObject(sqlservertrans)

end event

type em_conno from uo_conno within w_annotation_report
int X=394
int Y=265
int Width=330
int Height=97
int TabOrder=10
MaskDataType MaskDataType=StringMask!
boolean AutoSkip=true
string DisplayData=""
long TextColor=255
int TextSize=-10
end type

type st_2 from statictext within w_annotation_report
int X=42
int Y=133
int Width=1066
int Height=81
boolean Enabled=false
string Text="Please enter the Control Number"
Alignment Alignment=Center!
boolean FocusRectangle=false
long BackColor=79741120
long BorderColor=79741120
int TextSize=-10
int Weight=700
string FaceName="MS Sans Serif"
FontFamily FontFamily=Swiss!
FontPitch FontPitch=Variable!
end type

type cb_cancel from commandbutton within w_annotation_report
int X=663
int Y=465
int Width=298
int Height=109
int TabOrder=30
string Text="&Cancel"
int TextSize=-10
int Weight=400
string FaceName="MS Sans Serif"
FontFamily FontFamily=Swiss!
FontPitch FontPitch=Variable!
end type

event clicked;close(parent)
end event

type cb_ok from commandbutton within w_annotation_report
int X=193
int Y=465
int Width=307
int Height=109
int TabOrder=20
string Text="&OK"
int TextSize=-10
int Weight=400
string FaceName="MS Sans Serif"
FontFamily FontFamily=Swiss!
FontPitch FontPitch=Variable!
end type

event clicked;int rc
string Lconno

Lconno = em_conno.text

IF Lconno <> "" THEN
	rc = dw_annotrpt_ec_report.retrieve(Lconno)
	IF rc > 0 THEN
		dw_annotrpt_ec_report.TriggerEvent("pfc_print")
	ELSE
		MessageBox("ERROR", "Invalid Control number.")
	END IF
ELSE
	MessageBox("ERROR", "Please enter the control number.",StopSign!)
	em_conno.SetFocus()
END IF
end event

type st_1 from statictext within w_annotation_report
int X=229
int Y=37
int Width=682
int Height=81
boolean Enabled=false
string Text="Annotation Report"
Alignment Alignment=Center!
boolean FocusRectangle=false
long BackColor=79741120
long BorderColor=79741120
int TextSize=-10
int Weight=700
string FaceName="MS Sans Serif"
FontFamily FontFamily=Swiss!
FontPitch FontPitch=Variable!
end type


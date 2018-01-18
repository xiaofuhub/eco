$PBExportHeader$w_print_report_chno.srw
forward
global type w_print_report_chno from Window
end type
type cb_print_preview from u_cb within w_print_report_chno
end type
type st_print from statictext within w_print_report_chno
end type
type sle_copies from uo_pics_sle within w_print_report_chno
end type
type dw_cc_par_report from u_pics_dw within w_print_report_chno
end type
type cb_cancel from u_cb within w_print_report_chno
end type
type cb_print from u_cb within w_print_report_chno
end type
type em_chno from u_em within w_print_report_chno
end type
type st_1 from statictext within w_print_report_chno
end type
end forward

global type w_print_report_chno from Window
int X=833
int Y=361
int Width=1372
int Height=953
boolean TitleBar=true
string Title="Print PC.PAR.Report."
long BackColor=12632256
boolean ControlMenu=true
WindowType WindowType=response!
cb_print_preview cb_print_preview
st_print st_print
sle_copies sle_copies
dw_cc_par_report dw_cc_par_report
cb_cancel cb_cancel
cb_print cb_print
em_chno em_chno
st_1 st_1
end type
global w_print_report_chno w_print_report_chno

type variables
structure str_report_param
datawindow idw_dw

end variables

on w_print_report_chno.create
this.cb_print_preview=create cb_print_preview
this.st_print=create st_print
this.sle_copies=create sle_copies
this.dw_cc_par_report=create dw_cc_par_report
this.cb_cancel=create cb_cancel
this.cb_print=create cb_print
this.em_chno=create em_chno
this.st_1=create st_1
this.Control[]={ this.cb_print_preview,&
this.st_print,&
this.sle_copies,&
this.dw_cc_par_report,&
this.cb_cancel,&
this.cb_print,&
this.em_chno,&
this.st_1}
end on

on w_print_report_chno.destroy
destroy(this.cb_print_preview)
destroy(this.st_print)
destroy(this.sle_copies)
destroy(this.dw_cc_par_report)
destroy(this.cb_cancel)
destroy(this.cb_print)
destroy(this.em_chno)
destroy(this.st_1)
end on

event key;IF KeyDown(KeyEnter!) THEN 
  cb_print.TriggerEvent(Clicked!)
ELSEIF KeyDown(KeyEscape!) THEN 
  cb_cancel.TriggerEvent(Clicked!)
END IF


end event

type cb_print_preview from u_cb within w_print_report_chno
int X=750
int Y=673
int Width=535
int Height=125
int TabOrder=31
string Text="P&rint Preview"
int TextSize=-10
int Weight=700
end type

event clicked;call super::clicked;IF Upper(This.Text) = Upper("P&rint Preview")  THEN
	dw_cc_par_report.of_SetPrintPreview(True)
	dw_cc_par_report.inv_PrintPreview.of_SetEnabled(True)
	This.Text = "Preview &Off"
ELSE
//	Upper(This.Text) = Upper("Preview &Off")
END IF

end event

type st_print from statictext within w_print_report_chno
int X=124
int Y=661
int Width=316
int Height=137
boolean Enabled=false
string Text="Number of copies:"
Alignment Alignment=Center!
boolean FocusRectangle=false
long BackColor=12632256
int TextSize=-10
int Weight=700
string FaceName="MS Sans Serif"
FontFamily FontFamily=Swiss!
FontPitch FontPitch=Variable!
end type

type sle_copies from uo_pics_sle within w_print_report_chno
int X=444
int Y=709
int Width=183
int Height=89
int TabOrder=40
string Text="1"
int TextSize=-12
int Weight=700
end type

type dw_cc_par_report from u_pics_dw within w_print_report_chno
int X=942
int Y=297
int Width=353
int Height=157
int TabOrder=20
boolean Visible=false
string DataObject="d_cc_par_report"
end type

event ue_postconstructor;call super::ue_postconstructor;dw_cc_par_report.of_SetTransObject(sqlservertrans)

end event

event constructor;call super::constructor;//dw_1.settransobject(sqlservertrans)
end event

type cb_cancel from u_cb within w_print_report_chno
int X=750
int Y=473
int Width=508
int Height=125
int TabOrder=30
string Text="&Cancel"
int TextSize=-10
int Weight=700
end type

event clicked;call super::clicked;Close(parent)
end event

type cb_print from u_cb within w_print_report_chno
int X=124
int Y=473
int Width=508
int Height=125
int TabOrder=50
string Text="&Print"
int TextSize=-10
int Weight=700
end type

event clicked;call super::clicked;long job
string lchno
int lchn, i

lchno=em_chno.text
select count(*) into :lchn from ttlinit where chno=:lchno using sqlservertrans;
IF lchn > 0 THEN
	
	lchn = dw_cc_par_report.retrieve(lchno)
	
	IF lchn > 0 THEN
		job = PrintOpen( ) 
		
		FOR i = 1 to integer(sle_copies.text)
			PrintDataWindow(job, dw_cc_par_report) 
		NEXT
		
		PrintClose(job)
	ELSE
		MessageBox("ERROR","Report can~'t be created for chart number: "+lchno)
	END IF
ELSE
	MessageBox("ERROR","Invalid Chart Number.")
END IF





end event

type em_chno from u_em within w_print_report_chno
int X=389
int Y=273
int Width=535
int Height=117
int TabOrder=10
Alignment Alignment=Center!
string Mask="######"
string DisplayData="D"
double Increment=0
string MinMax=""
long BackColor=1090519039
int TextSize=-12
int Weight=700
end type

type st_1 from statictext within w_print_report_chno
int X=389
int Y=81
int Width=545
int Height=121
boolean Enabled=false
string Text="Please, Enter the Chart Number:"
Alignment Alignment=Center!
boolean FocusRectangle=false
long BackColor=12632256
int TextSize=-10
int Weight=700
string FaceName="MS Sans Serif"
FontFamily FontFamily=Swiss!
FontPitch FontPitch=Variable!
end type


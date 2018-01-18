$PBExportHeader$w_anno_rpt.srw
forward
global type w_anno_rpt from window
end type
type pb_print from commandbutton within w_anno_rpt
end type
type pb_exit from commandbutton within w_anno_rpt
end type
type dw_anno_rpt from datawindow within w_anno_rpt
end type
end forward

global type w_anno_rpt from window
integer x = 530
integer y = 228
integer width = 1614
integer height = 1232
boolean titlebar = true
string title = "Annotation Report"
boolean controlmenu = true
windowtype windowtype = response!
long backcolor = 79741120
pb_print pb_print
pb_exit pb_exit
dw_anno_rpt dw_anno_rpt
end type
global w_anno_rpt w_anno_rpt

type variables

end variables

event open;long ll_rows
string original_select,mod_string,where_clause,rc,Lchno
int RowNum

if IsValid(w_sheet_bcs_stage1) then
	RowNum = w_sheet_bcs_stage1.dw_bcs_stage1.GetRow()
	Lchno = w_sheet_bcs_stage1.dw_bcs_stage1.Object.cchno[RowNum]
elseif IsValid(w_sheet_bcs_stage2) then
	RowNum = w_sheet_bcs_stage2.dw_bcs_stage2.GetRow()
	Lchno = w_sheet_bcs_stage2.dw_bcs_stage2.Object.cchno[RowNum]
end if	

dw_anno_rpt.SetTransObject( SQLServerTrans )

original_select =	dw_anno_rpt.Describe("DataWindow.Table.Select")

where_clause = " WHERE annotation.chno="+ "~'" +Lchno+ "~'"

mod_string = "DataWindow.Table.Select=~"" + original_select + where_clause + "~""

rc = dw_anno_rpt.Modify(mod_string)
IF rc = "" THEN
	ll_rows = dw_anno_rpt.Retrieve( )
   IF ll_rows < 1 THEN 
     MessageBox("Annotation Error", "There are no annotation for chart Number: " + Lchno,StopSign!, OK!, 2)
     close(w_anno_rpt)
     return
   END IF
ELSE
	MessageBox("Status", "Modify Failed" + rc)
END if



end event

on key;IF KeyDown(KeyEscape!) THEN 
  pb_exit.TriggerEvent(Clicked!)
END IF

end on

on w_anno_rpt.create
this.pb_print=create pb_print
this.pb_exit=create pb_exit
this.dw_anno_rpt=create dw_anno_rpt
this.Control[]={this.pb_print,&
this.pb_exit,&
this.dw_anno_rpt}
end on

on w_anno_rpt.destroy
destroy(this.pb_print)
destroy(this.pb_exit)
destroy(this.dw_anno_rpt)
end on

type pb_print from commandbutton within w_anno_rpt
integer x = 1051
integer y = 988
integer width = 247
integer height = 108
integer taborder = 20
integer textsize = -10
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "&Print"
end type

event clicked;long job
job = PrintOpen( ) 
PrintDataWindow(job, dw_anno_rpt) 
PrintClose(job)

end event

type pb_exit from commandbutton within w_anno_rpt
integer x = 1326
integer y = 988
integer width = 247
integer height = 108
integer taborder = 30
integer textsize = -10
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "&Exit"
end type

event clicked;close(parent)
end event

type dw_anno_rpt from datawindow within w_anno_rpt
integer x = 23
integer y = 24
integer width = 1550
integer height = 944
integer taborder = 10
string dataobject = "d_anno_rpt"
end type


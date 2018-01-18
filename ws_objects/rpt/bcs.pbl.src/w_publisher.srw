$PBExportHeader$w_publisher.srw
forward
global type w_publisher from window
end type
type pb_print from commandbutton within w_publisher
end type
type pb_exit from commandbutton within w_publisher
end type
type dw_publisher from datawindow within w_publisher
end type
end forward

global type w_publisher from window
integer x = 512
integer y = 268
integer width = 1934
integer height = 1236
boolean titlebar = true
string title = "Publisher Information"
boolean controlmenu = true
windowtype windowtype = response!
long backcolor = 79741120
pb_print pb_print
pb_exit pb_exit
dw_publisher dw_publisher
end type
global w_publisher w_publisher

event open;long ll_rows
int Net = 0,RowNum
string original_select,where_clause,mod_string,rc,Lconno

if IsValid(w_sheet_bcs_stage1) then
	RowNum = w_sheet_bcs_stage1.dw_bcs_stage1.GetRow()
	Lconno = w_sheet_bcs_stage1.dw_bcs_stage1.Object.cconno[RowNum]
elseif IsValid(w_sheet_bcs_stage2) then
	RowNum = w_sheet_bcs_stage2.dw_bcs_stage2.GetRow()
	Lconno = w_sheet_bcs_stage2.dw_bcs_stage2.Object.cconno[RowNum]
end if

dw_publisher.SetTransObject( SQLServerTrans )

original_select =	dw_publisher.Describe("DataWindow.Table.Select")

where_clause = " AND mchar.conno="+ "~'" +Lconno+ "~'"

mod_string = "DataWindow.Table.Select=~"" + original_select + where_clause + "~""

rc = dw_publisher.Modify(mod_string)
IF rc = "" THEN
  ll_rows = dw_publisher.Retrieve()

  IF ll_rows < 1 THEN 
    MessageBox("Database Error", "There is no publication information for control Number: " + Lconno,StopSign!, OK!, 2)
    close(w_publisher)
  END IF
ELSE
	MessageBox("Status", "Modify Failed" + rc)
END if
end event

on key;IF KeyDown(KeyEscape!) THEN 
  pb_exit.TriggerEvent(Clicked!)
END IF

end on

on w_publisher.create
this.pb_print=create pb_print
this.pb_exit=create pb_exit
this.dw_publisher=create dw_publisher
this.Control[]={this.pb_print,&
this.pb_exit,&
this.dw_publisher}
end on

on w_publisher.destroy
destroy(this.pb_print)
destroy(this.pb_exit)
destroy(this.dw_publisher)
end on

type pb_print from commandbutton within w_publisher
integer x = 1307
integer y = 996
integer width = 247
integer height = 108
integer taborder = 20
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "Print"
end type

event clicked;long job
int Ans

Ans = MessageBox("Print Publisher Information", "Print Publisher information for control Number: ", Question!, OKCancel!, 2)
if Ans = 1 THEN 
  job = PrintOpen( ) 
  PrintDataWindow(job, dw_publisher) 
  PrintClose(job)
else
  return
end if

end event

type pb_exit from commandbutton within w_publisher
integer x = 1600
integer y = 996
integer width = 247
integer height = 108
integer taborder = 30
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "Exit"
end type

event clicked;close(parent)
end event

type dw_publisher from datawindow within w_publisher
integer x = 78
integer y = 44
integer width = 1769
integer height = 912
integer taborder = 10
string dataobject = "d_publisher"
boolean livescroll = true
end type


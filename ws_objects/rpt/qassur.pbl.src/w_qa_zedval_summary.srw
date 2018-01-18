$PBExportHeader$w_qa_zedval_summary.srw
forward
global type w_qa_zedval_summary from w_sheet
end type
type cb_print from commandbutton within w_qa_zedval_summary
end type
type cb_qa from commandbutton within w_qa_zedval_summary
end type
type st_2 from statictext within w_qa_zedval_summary
end type
type dw_qa_dttime from u_pics_dw within w_qa_zedval_summary
end type
type st_1 from statictext within w_qa_zedval_summary
end type
type cb_exit from commandbutton within w_qa_zedval_summary
end type
type dw_qa_zedval_summary from u_pics_dw within w_qa_zedval_summary
end type
end forward

global type w_qa_zedval_summary from w_sheet
integer x = 214
integer y = 221
integer width = 3534
integer height = 1840
string title = "DTB QA Summary Activity"
cb_print cb_print
cb_qa cb_qa
st_2 st_2
dw_qa_dttime dw_qa_dttime
st_1 st_1
cb_exit cb_exit
dw_qa_zedval_summary dw_qa_zedval_summary
end type
global w_qa_zedval_summary w_qa_zedval_summary

type variables
Datetime ld_review_date

end variables

on w_qa_zedval_summary.create
int iCurrent
call super::create
this.cb_print=create cb_print
this.cb_qa=create cb_qa
this.st_2=create st_2
this.dw_qa_dttime=create dw_qa_dttime
this.st_1=create st_1
this.cb_exit=create cb_exit
this.dw_qa_zedval_summary=create dw_qa_zedval_summary
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.cb_print
this.Control[iCurrent+2]=this.cb_qa
this.Control[iCurrent+3]=this.st_2
this.Control[iCurrent+4]=this.dw_qa_dttime
this.Control[iCurrent+5]=this.st_1
this.Control[iCurrent+6]=this.cb_exit
this.Control[iCurrent+7]=this.dw_qa_zedval_summary
end on

on w_qa_zedval_summary.destroy
call super::destroy
destroy(this.cb_print)
destroy(this.cb_qa)
destroy(this.st_2)
destroy(this.dw_qa_dttime)
destroy(this.st_1)
destroy(this.cb_exit)
destroy(this.dw_qa_zedval_summary)
end on

event open;call super::open;// Open the sheet in Maximized mode
this.windowstate = maximized!



end event

event pfc_preopen;call super::pfc_preopen;string Luserid
Luserid = SQLserverTrans.userid

this.of_SetPreference(TRUE)
this.inv_preference.of_SetToolBars(TRUE)
this.inv_preference.of_SetWindow(TRUE)
this.of_SetResize(TRUE)
this.inv_resize.of_SetOrigSize(this.workspacewidth(),this.workspaceheight())


inv_resize.of_Register(dw_qa_dttime, "Scale")
inv_resize.of_Register(dw_qa_zedval_summary, "Scale")

inv_resize.of_Register(cb_exit, "Scale")
inv_resize.of_Register(cb_qa, "Scale")
inv_resize.of_Register(cb_print, "Scale")
inv_resize.of_Register(st_2, "Scale")

end event

event resize;call super::resize;long ll_height

This.X = w_pics_main.X
This.Y = w_pics_main.Y
ll_height = w_pics_main.mdi_1.Height
This.Resize(w_pics_main.WorkSpaceWidth(), ll_height)

end event

event closequery;call super::closequery;ib_disableclosequery = TRUE
end event

type cb_print from commandbutton within w_qa_zedval_summary
integer x = 2231
integer y = 1600
integer width = 402
integer height = 96
integer taborder = 20
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&Print"
end type

event clicked;ld_review_date = dw_qa_dttime.object.dt_time[dw_qa_dttime.GetRow()]

OpenSheetwithparm(w_sheet_pics_ole_crystal,"dtb_qas",w_pics_main, 0, Original!)

end event

type cb_qa from commandbutton within w_qa_zedval_summary
integer x = 2665
integer y = 1600
integer width = 402
integer height = 96
integer taborder = 20
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&QA Review"
end type

event clicked;OpenSheet(w_qa_product_review, w_pics_main, 0, Original!)
end event

type st_2 from statictext within w_qa_zedval_summary
integer x = 37
integer y = 1568
integer width = 1792
integer height = 64
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "Double click on the book number to get the reports"
boolean focusrectangle = false
end type

type dw_qa_dttime from u_pics_dw within w_qa_zedval_summary
integer x = 805
integer y = 16
integer width = 480
integer height = 104
integer taborder = 10
string dataobject = "d_qa_dttime"
boolean vscrollbar = false
boolean livescroll = false
end type

event ue_postconstructor;call super::ue_postconstructor;date ldt_time

dw_qa_dttime.SetTransObject(sqlservertrans) 
dw_qa_dttime.Retrieve()

select max(dt_time) into :ldt_time
from dtb_data
using sqlservertrans;
if f_check_dberror(sqlservertrans, "Selecting from DTB_DATA") then
	dw_qa_zedval_summary.SetTransObject(sqlservertrans) 
	dw_qa_zedval_summary.Retrieve(ldt_time)
end if

dw_qa_zedval_summary.Visible = TRUE
dw_qa_dttime.SetFocus()

end event

event itemchanged;call super::itemchanged;string ls_data
date ld_date
datetime ldt_date

ls_data=data
ld_date=date(data)
dw_qa_zedval_summary.SetTransObject(sqlservertrans)
dw_qa_zedval_summary.Retrieve(date(data))

end event

type st_1 from statictext within w_qa_zedval_summary
integer x = 41
integer y = 44
integer width = 823
integer height = 64
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 33554432
long backcolor = 67108864
string text = "Please select a review date:"
boolean focusrectangle = false
end type

type cb_exit from commandbutton within w_qa_zedval_summary
integer x = 3109
integer y = 1600
integer width = 315
integer height = 96
integer taborder = 10
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "E&xit"
end type

event clicked;dw_qa_zedval_summary.ResetUpdate() // Clear update flags
dw_qa_dttime.ResetUpdate() 
close(parent)

end event

type dw_qa_zedval_summary from u_pics_dw within w_qa_zedval_summary
integer x = 37
integer y = 128
integer width = 3401
integer height = 1440
integer taborder = 10
string dataobject = "d_qa_zedval_summary"
end type

event ue_postconstructor;call super::ue_postconstructor;string ls_excludecols[]
pointer oldpointer // Declares a pointer variable

oldpointer = SetPointer(HourGlass!)

dw_qa_zedval_summary.of_SetLinkage(TRUE)

dw_qa_zedval_summary.of_SetTransObject(SQLServerTrans)
//
dw_qa_zedval_summary.of_Setfilter(TRUE)
dw_qa_zedval_summary.of_Setfind(TRUE)
dw_qa_zedval_summary.of_SetRowManager(TRUE)
dw_qa_zedval_summary.inv_filter.of_SetStyle(1)
dw_qa_zedval_summary.inv_filter.of_SetColumnNameSource(2)
ls_excludecols[1] = "graycolor"
ls_excludecols[2] = "whitecolor"
dw_qa_zedval_summary.inv_filter.of_SetExclude(ls_excludecols)
dw_qa_zedval_summary.of_SetSort(TRUE)
dw_qa_zedval_summary.inv_sort.of_SetStyle(1)
dw_qa_zedval_summary.inv_sort.of_SetColumnNameSource(2)
dw_qa_zedval_summary.inv_sort.of_SetExclude(ls_excludecols)
dw_qa_zedval_summary.SetRowFocusIndicator(Hand!)




end event

event retrieveend;call super::retrieveend;close(w_pics_retrieve_msg_box)

end event

event retrievestart;call super::retrievestart;openwithparm(w_pics_retrieve_msg_box,"Retrieving Information, Please Wait...")
end event

event doubleclicked;call super::doubleclicked;Long CurRow
String ls_bkseq

CurRow = dw_qa_zedval_summary.GetRow()

ls_bkseq = String(dw_qa_zedval_summary.object.bkseq[CurRow])
OpenWithParm(w_qa_zedval_reports, ls_bkseq)
end event


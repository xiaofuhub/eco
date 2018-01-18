$PBExportHeader$w_pms_reports_criteria.srw
forward
global type w_pms_reports_criteria from w_response
end type
type lb_media from u_lb within w_pms_reports_criteria
end type
type lb_casub from u_lb within w_pms_reports_criteria
end type
type lb_pmsub from u_lb within w_pms_reports_criteria
end type
type dw_casubj from u_pics_dw within w_pms_reports_criteria
end type
type dw_pmsub from u_pics_dw within w_pms_reports_criteria
end type
type st_6 from statictext within w_pms_reports_criteria
end type
type cb_report from commandbutton within w_pms_reports_criteria
end type
type cb_cancel from commandbutton within w_pms_reports_criteria
end type
type st_3 from statictext within w_pms_reports_criteria
end type
type st_2 from statictext within w_pms_reports_criteria
end type
type st_1 from statictext within w_pms_reports_criteria
end type
end forward

global type w_pms_reports_criteria from w_response
integer x = 214
integer y = 221
integer width = 1874
integer height = 1852
string title = "Report Criteria"
lb_media lb_media
lb_casub lb_casub
lb_pmsub lb_pmsub
dw_casubj dw_casubj
dw_pmsub dw_pmsub
st_6 st_6
cb_report cb_report
cb_cancel cb_cancel
st_3 st_3
st_2 st_2
st_1 st_1
end type
global w_pms_reports_criteria w_pms_reports_criteria

on w_pms_reports_criteria.create
int iCurrent
call super::create
this.lb_media=create lb_media
this.lb_casub=create lb_casub
this.lb_pmsub=create lb_pmsub
this.dw_casubj=create dw_casubj
this.dw_pmsub=create dw_pmsub
this.st_6=create st_6
this.cb_report=create cb_report
this.cb_cancel=create cb_cancel
this.st_3=create st_3
this.st_2=create st_2
this.st_1=create st_1
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.lb_media
this.Control[iCurrent+2]=this.lb_casub
this.Control[iCurrent+3]=this.lb_pmsub
this.Control[iCurrent+4]=this.dw_casubj
this.Control[iCurrent+5]=this.dw_pmsub
this.Control[iCurrent+6]=this.st_6
this.Control[iCurrent+7]=this.cb_report
this.Control[iCurrent+8]=this.cb_cancel
this.Control[iCurrent+9]=this.st_3
this.Control[iCurrent+10]=this.st_2
this.Control[iCurrent+11]=this.st_1
end on

on w_pms_reports_criteria.destroy
call super::destroy
destroy(this.lb_media)
destroy(this.lb_casub)
destroy(this.lb_pmsub)
destroy(this.dw_casubj)
destroy(this.dw_pmsub)
destroy(this.st_6)
destroy(this.cb_report)
destroy(this.cb_cancel)
destroy(this.st_3)
destroy(this.st_2)
destroy(this.st_1)
end on

type lb_media from u_lb within w_pms_reports_criteria
integer x = 1353
integer y = 224
integer width = 329
integer height = 192
integer taborder = 60
string item[] = {"All Media","Cassettes","Braille"}
end type

type lb_casub from u_lb within w_pms_reports_criteria
integer x = 695
integer y = 224
integer width = 439
integer height = 1088
integer taborder = 50
boolean multiselect = true
end type

type lb_pmsub from u_lb within w_pms_reports_criteria
integer x = 110
integer y = 224
integer width = 475
integer height = 1088
integer taborder = 40
boolean multiselect = true
end type

type dw_casubj from u_pics_dw within w_pms_reports_criteria
boolean visible = false
integer x = 1390
integer y = 576
integer width = 329
integer height = 96
integer taborder = 40
string dataobject = "d_casubj"
boolean vscrollbar = false
boolean hsplitscroll = true
end type

event constructor;call super::constructor;int ll_rows=0,i

this.of_SetRowManager(TRUE)
this.of_SetRowSelect(TRUE)
this.inv_RowSelect.of_SetStyle(1)
this.inv_RowSelect.of_InvertSelection() 

this.of_SetTransObject( SQLServerTrans )
ll_rows = this.retrieve()
IF ll_rows = 0 THEN
	MessageBox("error","No CASUB CODE Found")
ELSE
	FOR i = 1 TO ll_rows
		lb_casub.additem(string(this.object.casubj_code[i]))
	NEXT
END IF

end event

type dw_pmsub from u_pics_dw within w_pms_reports_criteria
boolean visible = false
integer x = 1390
integer y = 736
integer width = 329
integer height = 96
integer taborder = 10
string dataobject = "d_pmsub"
boolean vscrollbar = false
boolean hsplitscroll = true
end type

event constructor;call super::constructor;int ll_rows=0,i

this.of_SetRowManager(TRUE)
this.of_SetRowSelect(TRUE)
this.inv_RowSelect.of_SetStyle(1)
this.inv_RowSelect.of_InvertSelection() 

this.of_SetTransObject( SQLServerTrans )
ll_rows = this.retrieve()
IF ll_rows = 0 THEN
	MessageBox("error","No CASUB CODE Found")
ELSE
	FOR i = 1 TO ll_rows
		lb_pmsub.additem(this.object.pmsub_code[i])
	NEXT
END IF

end event

type st_6 from statictext within w_pms_reports_criteria
integer x = 55
integer y = 40
integer width = 2085
integer height = 68
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "Please select necessary information and click Get report:"
boolean focusrectangle = false
end type

type cb_report from commandbutton within w_pms_reports_criteria
integer x = 366
integer y = 1440
integer width = 457
integer height = 112
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Get report"
end type

event clicked;string ls_casub,ls_pmsub,ls_media
integer li_ItemTotal, li_ItemCount
str_catalog_info lstr_catalog_info


// Casub List box
li_ItemTotal = lb_casub.TotalItems( )
FOR li_ItemCount = 1 to li_ItemTotal
   IF lb_casub.State(li_ItemCount) = 1 THEN
		ls_casub += "'"+lb_casub.text(li_ItemCount)+"'"+","
   END IF
NEXT
ls_casub = Mid(ls_casub,1,len(ls_casub)-1)

IF lb_casub.TotalSelected() = 0 THEN
      MessageBox("Warning","You have not selected any copy allotment code.",information!)
      SetNull(ls_casub)
END IF



// pmsub List box
li_ItemTotal = lb_pmsub.TotalItems( )
FOR li_ItemCount = 1 to li_ItemTotal
   IF lb_pmsub.State(li_ItemCount) = 1 THEN
		ls_pmsub +=  "'"+lb_pmsub.text(li_ItemCount)+"'"+","
   END IF
NEXT
ls_pmsub = Mid(ls_pmsub,1,len(ls_pmsub)-1)

IF lb_pmsub.TotalSelected() = 0 THEN
      MessageBox("Warning","You have not selected any PMS subject codes.",information!)
      SetNull(ls_pmsub)
END IF

//Media list box
ls_media = lb_media.SelectedItem()
IF ls_media = 'Cassettes' THEN
	ls_media = "RC"
ELSEIF ls_media = 'Braille' THEN
       ls_media = "BR"
ELSEIF ls_media = 'All Media' THEN
	ls_media = "ALL"
ELSE
	ls_media = "RC"
END IF	

lstr_catalog_info.pmsub = ls_pmsub
lstr_catalog_info.casub = ls_casub
lstr_catalog_info.media = ls_media

CloseWithReturn(Parent, lstr_catalog_info)
end event

type cb_cancel from commandbutton within w_pms_reports_criteria
integer x = 914
integer y = 1440
integer width = 402
integer height = 112
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "E&xit"
end type

event clicked;string Nullstr
SetNull(Nullstr)
dw_casubj.reset()
dw_pmsub.reset()
CloseWithReturn(Parent, Nullstr)

end event

type st_3 from statictext within w_pms_reports_criteria
integer x = 1353
integer y = 160
integer width = 219
integer height = 64
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean underline = true
long textcolor = 33554432
long backcolor = 67108864
string text = "Media"
alignment alignment = right!
boolean focusrectangle = false
end type

type st_2 from statictext within w_pms_reports_criteria
integer x = 658
integer y = 160
integer width = 645
integer height = 64
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean underline = true
long textcolor = 33554432
long backcolor = 67108864
string text = "Copy Allotment Code"
boolean focusrectangle = false
end type

type st_1 from statictext within w_pms_reports_criteria
integer x = 110
integer y = 160
integer width = 549
integer height = 64
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean underline = true
long textcolor = 33554432
long backcolor = 67108864
string text = "PM Subject Code"
boolean focusrectangle = false
end type


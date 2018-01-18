$PBExportHeader$w_selectbooks_convpcs.srw
forward
global type w_selectbooks_convpcs from w_sheet
end type
type st_converted from statictext within w_selectbooks_convpcs
end type
type st_auth from statictext within w_selectbooks_convpcs
end type
type st_avail from statictext within w_selectbooks_convpcs
end type
type st_selected from statictext within w_selectbooks_convpcs
end type
type dw_conversionbooks from u_pics_dw within w_selectbooks_convpcs
end type
type dw_for_dddw_sltbkcnvpcs from u_pics_dw within w_selectbooks_convpcs
end type
type cb_cancel from u_cb within w_selectbooks_convpcs
end type
type cb_print from u_cb within w_selectbooks_convpcs
end type
type cb_update from u_cb within w_selectbooks_convpcs
end type
type dw_selectbooks_convpcs from u_pics_dw within w_selectbooks_convpcs
end type
end forward

global type w_selectbooks_convpcs from w_sheet
integer width = 2757
integer height = 1588
st_converted st_converted
st_auth st_auth
st_avail st_avail
st_selected st_selected
dw_conversionbooks dw_conversionbooks
dw_for_dddw_sltbkcnvpcs dw_for_dddw_sltbkcnvpcs
cb_cancel cb_cancel
cb_print cb_print
cb_update cb_update
dw_selectbooks_convpcs dw_selectbooks_convpcs
end type
global w_selectbooks_convpcs w_selectbooks_convpcs

type variables
long i_selected, i_converted, i_avail
end variables

on w_selectbooks_convpcs.create
int iCurrent
call super::create
this.st_converted=create st_converted
this.st_auth=create st_auth
this.st_avail=create st_avail
this.st_selected=create st_selected
this.dw_conversionbooks=create dw_conversionbooks
this.dw_for_dddw_sltbkcnvpcs=create dw_for_dddw_sltbkcnvpcs
this.cb_cancel=create cb_cancel
this.cb_print=create cb_print
this.cb_update=create cb_update
this.dw_selectbooks_convpcs=create dw_selectbooks_convpcs
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_converted
this.Control[iCurrent+2]=this.st_auth
this.Control[iCurrent+3]=this.st_avail
this.Control[iCurrent+4]=this.st_selected
this.Control[iCurrent+5]=this.dw_conversionbooks
this.Control[iCurrent+6]=this.dw_for_dddw_sltbkcnvpcs
this.Control[iCurrent+7]=this.cb_cancel
this.Control[iCurrent+8]=this.cb_print
this.Control[iCurrent+9]=this.cb_update
this.Control[iCurrent+10]=this.dw_selectbooks_convpcs
end on

on w_selectbooks_convpcs.destroy
call super::destroy
destroy(this.st_converted)
destroy(this.st_auth)
destroy(this.st_avail)
destroy(this.st_selected)
destroy(this.dw_conversionbooks)
destroy(this.dw_for_dddw_sltbkcnvpcs)
destroy(this.cb_cancel)
destroy(this.cb_print)
destroy(this.cb_update)
destroy(this.dw_selectbooks_convpcs)
end on

event open;call super::open;THIS.Windowstate = maximized!

end event

event pfc_preopen;call super::pfc_preopen;this.of_SetPreference(TRUE)
this.inv_preference.of_SetToolBars(TRUE)
this.inv_preference.of_SetWindow(TRUE)
this.of_SetResize(TRUE)
this.inv_resize.of_SetOrigSize(this.workspacewidth(),this.workspaceheight())
inv_resize.of_Register(cb_cancel, "scale")
inv_resize.of_Register(cb_print, "scale")
inv_resize.of_Register(cb_update, "scale")
inv_resize.of_Register(dw_selectbooks_convpcs, "scale")
inv_resize.of_Register(dw_for_dddw_sltbkcnvpcs, "scale")
inv_resize.of_Register(dw_conversionbooks, "scale")
inv_resize.of_Register(st_avail, "scale")
inv_resize.of_Register(st_selected, "scale")
inv_resize.of_Register(st_auth, "scale")
inv_resize.of_Register(st_converted, "scale")
end event

event pfc_postopen;call super::pfc_postopen;long i,li_row, i2, li_avail, li_slct, li_cnvt
string  ls_flag, ls_type
datawindowchild dwc_auth

m_pics_main.m_Edit.m_Addrow.Enabled = FALSE
m_pics_main.m_Edit.m_Deleterow.Enabled = FALSE
m_pics_main.m_Edit.m_Cut.Enabled = FALSE
OpenWithParm(w_pics_retrieve_msg_box,'Retrieve data please wait...')
//dw_selectbooks_convpcs.SetTransObject(SqlServerTrans)
dw_selectbooks_convpcs.Retrieve()

dw_for_dddw_sltbkcnvpcs.SetTransobject(sqlservertrans)
dw_for_dddw_sltbkcnvpcs.GetChild('auth',dwc_auth)
dwc_auth.SetTransobject(sqlservertrans)
dwc_auth.Retrieve()
dw_for_dddw_sltbkcnvpcs.Retrieve()
li_row=dw_selectbooks_convpcs.RowCount()
for i=1 to li_row
	ls_type=dw_selectbooks_convpcs.object.action_type[i]
	if ls_type='C' then
		i_avail++
	elseif ls_type='P' then
		i_converted++
	end if
next


i_selected=0
this.title='PCS Select Books For Conversion'
st_selected.text='Selected books for conversion: '+string(0)
st_avail.text='Total number of books available for selection: '+string(i_avail)
st_converted.text='Total number of books selected by PCS: '+string(i_converted)
close(w_pics_retrieve_msg_box)
dw_selectbooks_convpcs.SetFocus()
	

end event

type st_converted from statictext within w_selectbooks_convpcs
integer x = 987
integer y = 1260
integer width = 773
integer height = 60
integer textsize = -8
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 33554432
long backcolor = 67108864
boolean focusrectangle = false
end type

type st_auth from statictext within w_selectbooks_convpcs
integer x = 46
integer y = 1364
integer width = 114
integer height = 48
integer textsize = -8
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 33554432
long backcolor = 67108864
string text = "Author"
boolean focusrectangle = false
end type

type st_avail from statictext within w_selectbooks_convpcs
integer x = 1879
integer y = 1260
integer width = 818
integer height = 60
integer textsize = -8
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 33554432
long backcolor = 67108864
boolean focusrectangle = false
end type

type st_selected from statictext within w_selectbooks_convpcs
integer x = 41
integer y = 1260
integer width = 832
integer height = 60
integer textsize = -8
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 33554432
long backcolor = 67108864
boolean focusrectangle = false
end type

type dw_conversionbooks from u_pics_dw within w_selectbooks_convpcs
boolean visible = false
integer x = 2011
integer y = 1356
integer width = 55
integer height = 48
integer taborder = 20
string dataobject = "d_conversionbooks"
boolean vscrollbar = false
end type

event itemchanged;call super::itemchanged;long li_row, li_cnt,i
string ls_authname, ls_authname2, ls_auth
datawindowchild dwc_auth

dw_for_dddw_sltbkcnvPCS.GetChild('auth',dwc_auth)
li_row=dwc_auth.getrow()
li_cnt=dw_selectbooks_convpcs.RowCount()
ls_auth=data
if dwo.name='auth' then
	ls_authname=trim(dwc_auth.GetItemstring(li_row,'authname'))
end if
for i=1 to li_cnt
	ls_authname2=trim(dw_selectbooks_convpcs.object.authname[i])
	if ls_authname2=ls_authname then
		dw_selectbooks_convpcs.SelectRow(0, false)
		dw_selectbooks_convpcs.SelectRow(i, true)
		dw_selectbooks_convpcs.ScrolltoRow(i)
		dw_selectbooks_convpcs.SetRow(i)
		dw_selectbooks_convpcs.SetFocus()
		exit
	end if
next
	


end event

event constructor;call super::constructor;
this.SetTransObject(sqlservertrans)
end event

type dw_for_dddw_sltbkcnvpcs from u_pics_dw within w_selectbooks_convpcs
integer x = 165
integer y = 1356
integer width = 471
integer height = 64
integer taborder = 10
string dataobject = "d_for_dddw_sltbkcnvpcs"
boolean vscrollbar = false
end type

event itemchanged;call super::itemchanged;long li_row, li_cnt,i
string ls_authname, ls_authname2, ls_auth
datawindowchild dwc_auth

dw_for_dddw_sltbkcnvPCS.GetChild('auth',dwc_auth)
li_row=dwc_auth.getrow()
li_cnt=dw_selectbooks_convpcs.RowCount()
ls_auth=data
if dwo.name='auth' then
	ls_authname=trim(dwc_auth.GetItemstring(li_row,'authname'))
end if
for i=1 to li_cnt
	ls_authname2=trim(dw_selectbooks_convpcs.object.authname[i])
	if ls_authname2=ls_authname then
		dw_selectbooks_convpcs.SelectRow(0, false)
		dw_selectbooks_convpcs.SelectRow(i, true)
		dw_selectbooks_convpcs.ScrolltoRow(i)
		dw_selectbooks_convpcs.SetRow(i)
		dw_selectbooks_convpcs.SetFocus()
		exit
	end if
next
	


end event

type cb_cancel from u_cb within w_selectbooks_convpcs
event pfc_hinttext pbm_mousemove
string tag = "Close this window"
integer x = 2501
integer y = 1356
integer width = 197
integer taborder = 40
fontcharset fontcharset = ansi!
string text = "&Cancel"
end type

event pfc_hinttext;call super::pfc_hinttext;w_pics_main.setmicrohelp(This.tag)
end event

event clicked;call super::clicked;
long li_cnt, i, li_re
string ls_flag



li_cnt=dw_selectbooks_convpcs.Rowcount()
for i=1 to li_cnt
	ls_flag=dw_selectbooks_convpcs.object.upd_flag[i]
	if ls_flag='Y'  then
		li_re=messagebox(' ','update check box is checked, Would you update?',Question!,YesNo!, 1)
		if li_re=1 then
			cb_update.triggerevent('clicked')
			exit
		else
			exit
		end if
	end if
next

ib_disableclosequery=true	
close(w_selectbooks_convpcs)
end event

type cb_print from u_cb within w_selectbooks_convpcs
event pfc_hinttext pbm_mousemove
string tag = "Save data to oracle data base"
integer x = 2089
integer y = 1356
integer width = 169
integer taborder = 50
fontcharset fontcharset = ansi!
string text = "&Print"
end type

event pfc_hinttext;call super::pfc_hinttext;w_pics_main.setmicrohelp(This.tag)
end event

event clicked;call super::clicked;nvo_PowerPrn.of_SetPrinterOrientation(2)
dw_selectbooks_convpcs.TriggerEvent('pfc_print')
nvo_PowerPrn.of_SetPrinterOrientation(1)
end event

type cb_update from u_cb within w_selectbooks_convpcs
event pfc_hinttext pbm_mousemove
string tag = "Update conversionboos table"
integer x = 2281
integer y = 1356
integer width = 201
integer taborder = 30
fontcharset fontcharset = ansi!
string text = "&Update"
end type

event pfc_hinttext;call super::pfc_hinttext;w_pics_main.setmicrohelp(This.tag)
end event

event clicked;call super::clicked;datetime ldt_date
string ls_type='C', ls_flag, ls_conno, ls_conno2, ls_cyr
long li_cur, i, li_re, li_row, li_cnt, li_bkseq, li_priority

li_cnt=dw_selectbooks_convpcs.RowCount()
for i=1 to li_cnt
	ls_flag=dw_selectbooks_convpcs.object.upd_flag[i]
	
	if ls_flag='Y' then
		dw_selectbooks_convpcs.object.action_type[i]='P'
	end if
next
li_re=dw_selectbooks_convpcs.update()
if li_re=1 then
	commit using sqlservertrans;
else
	rollback using sqlservertrans;
end if
i_avail=0
i_converted=0
i_selected=0
parent.triggerevent('pfc_postopen')



end event

type dw_selectbooks_convpcs from u_pics_dw within w_selectbooks_convpcs
integer x = 23
integer y = 24
integer width = 2670
integer height = 1216
integer taborder = 20
string dataobject = "d_selectbooks_convpcs"
end type

event itemchanged;call super::itemchanged;long li_row, li_count=0,i, li_avail, li_cnvt, li_selected, li_bkseq
string  ls_flag, ls_yr, ls_date, ls_conno, ls_type
datetime ldt_cvdate, ldt_cvdate2
date ld_cvdate
//ls_date=string(today(), 'mm/dd/yyyy')
//ls_yr=right(ls_date, 4)
if dwo.name ='upd_flag' then
	ls_flag=data
	ls_type=dw_selectbooks_convpcs.object.action_type[row]
	
	if ls_flag='Y' then
//		dw_selectbooks_convpcs.object.cyr[row]= ls_yr
//		dw_selectbooks_convpcs.object.action_type[row]= 'P'
//		i_converted++
		i_selected++
		i_avail=i_avail - 1
		ls_conno=dw_selectbooks_convpcs.object.conno[row]
		li_bkseq=dw_selectbooks_convpcs.object.bkseq[row]
	elseif  ls_flag='N' then
//		ldt_cvdate=dw_selectbooks_convpcs.object.convassigndate[row]
//		ld_cvdate=date(ldt_cvdate)
//		ls_date=string(ld_cvdate,'mm/dd/yyyy')
//		ls_yr=right(ls_date, 4)
//		dw_selectbooks_convpcs.object.cyr[row]= ls_yr
//		dw_selectbooks_convpcs.object.action_type[row]= 'C'
//		i_converted= i_converted - 1
		i_selected=i_selected - 1
		i_avail=i_avail + 1
	end if
end if

st_selected.text='Selected books for conversion: '+string(i_selected)
st_avail.text='Total number of books available for selection: '+string(i_avail)
st_converted.text='Total number of books selected by PCS: '+string(i_converted)

end event

event constructor;call super::constructor;this.of_SetTransObject(SQLServerTrans)
this.of_SetFilter(TRUE)
this.of_SetFind(TRUE)
this.inv_filter.of_SetStyle(1)
this.inv_filter.of_SetColumnNameSource(2)
this.of_SetSort(TRUE)
this.inv_sort.of_SetStyle(1)
this.inv_sort.of_SetColumnNameSource(2)

end event


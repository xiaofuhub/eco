$PBExportHeader$w_selectbooks_conv.srw
forward
global type w_selectbooks_conv from w_sheet
end type
type cbx_status from checkbox within w_selectbooks_conv
end type
type cbx_not_found from checkbox within w_selectbooks_conv
end type
type st_auth from statictext within w_selectbooks_conv
end type
type st_coverted from statictext within w_selectbooks_conv
end type
type st_avail from statictext within w_selectbooks_conv
end type
type st_selected from statictext within w_selectbooks_conv
end type
type dw_conversionbooks from u_pics_dw within w_selectbooks_conv
end type
type dw_for_dddw_sltbkcnv from u_pics_dw within w_selectbooks_conv
end type
type cb_cancel from u_cb within w_selectbooks_conv
end type
type cb_print from u_cb within w_selectbooks_conv
end type
type cb_update from u_cb within w_selectbooks_conv
end type
type dw_selectbooks_conv from u_pics_dw within w_selectbooks_conv
end type
end forward

global type w_selectbooks_conv from w_sheet
integer width = 2670
integer height = 1576
cbx_status cbx_status
cbx_not_found cbx_not_found
st_auth st_auth
st_coverted st_coverted
st_avail st_avail
st_selected st_selected
dw_conversionbooks dw_conversionbooks
dw_for_dddw_sltbkcnv dw_for_dddw_sltbkcnv
cb_cancel cb_cancel
cb_print cb_print
cb_update cb_update
dw_selectbooks_conv dw_selectbooks_conv
end type
global w_selectbooks_conv w_selectbooks_conv

type variables
long i_selected, i_converted, i_avail
end variables

on w_selectbooks_conv.create
int iCurrent
call super::create
this.cbx_status=create cbx_status
this.cbx_not_found=create cbx_not_found
this.st_auth=create st_auth
this.st_coverted=create st_coverted
this.st_avail=create st_avail
this.st_selected=create st_selected
this.dw_conversionbooks=create dw_conversionbooks
this.dw_for_dddw_sltbkcnv=create dw_for_dddw_sltbkcnv
this.cb_cancel=create cb_cancel
this.cb_print=create cb_print
this.cb_update=create cb_update
this.dw_selectbooks_conv=create dw_selectbooks_conv
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.cbx_status
this.Control[iCurrent+2]=this.cbx_not_found
this.Control[iCurrent+3]=this.st_auth
this.Control[iCurrent+4]=this.st_coverted
this.Control[iCurrent+5]=this.st_avail
this.Control[iCurrent+6]=this.st_selected
this.Control[iCurrent+7]=this.dw_conversionbooks
this.Control[iCurrent+8]=this.dw_for_dddw_sltbkcnv
this.Control[iCurrent+9]=this.cb_cancel
this.Control[iCurrent+10]=this.cb_print
this.Control[iCurrent+11]=this.cb_update
this.Control[iCurrent+12]=this.dw_selectbooks_conv
end on

on w_selectbooks_conv.destroy
call super::destroy
destroy(this.cbx_status)
destroy(this.cbx_not_found)
destroy(this.st_auth)
destroy(this.st_coverted)
destroy(this.st_avail)
destroy(this.st_selected)
destroy(this.dw_conversionbooks)
destroy(this.dw_for_dddw_sltbkcnv)
destroy(this.cb_cancel)
destroy(this.cb_print)
destroy(this.cb_update)
destroy(this.dw_selectbooks_conv)
end on

event open;call super::open;this.windowState = maximized!

end event

event pfc_preopen;call super::pfc_preopen;this.of_SetPreference(TRUE)
this.inv_Preference.of_SetToolBars(TRUE)
this.inv_Preference.of_SetWindow(TRUE)
this.of_SetResize(TRUE)
this.inv_Resize.of_SetOrigSize(this.WorkSpaceWidth(),this.WorkSpaceHeight())
inv_Resize.of_Register(cb_Cancel, "scale")
inv_Resize.of_Register(cb_Print, "scale")
inv_Resize.of_Register(cb_Update, "scale")
inv_Resize.of_Register(dw_Selectbooks_conv, "scale")
inv_Resize.of_Register(dw_For_dddw_sltbkcnv, "scale")
inv_Resize.of_Register(dw_Conversionbooks, "scale")
inv_Resize.of_Register(st_Avail, "scale")
inv_Resize.of_Register(st_Coverted, "scale")
inv_Resize.of_Register(st_Selected, "scale")
inv_Resize.of_Register(st_Auth, "scale")
inv_Resize.of_Register(cbx_not_found, "scale")
inv_Resize.of_Register(cbx_status, "scale")
this.title='CDS Select Books For Conversion'
end event

event pfc_postopen;call super::pfc_postopen;Long i,li_Row, i2,  li_Slct, li_Cnvt
String ls_Auth, ls_Authfn, ls_Name2, ls_Conno, ls_Cyr, ls_Flag
DataWindowChild dwc_Auth

m_Pics_main.m_Edit.m_Addrow.enabled = FALSE
m_Pics_main.m_Edit.m_Deleterow.enabled = FALSE
m_Pics_main.m_Edit.m_Cut.enabled = FALSE
OpenWithParm(w_Pics_retrieve_msg_box,'Retrieve data please wait...')
dw_Selectbooks_conv.SetTransObject(sqlservertrans)
dw_Selectbooks_conv.Retrieve()

dw_For_dddw_sltbkcnv.SetTransObject(sqlservertrans)
dw_For_dddw_sltbkcnv.GetChild('auth',dwc_Auth)
dwc_Auth.SetTransObject(sqlservertrans)
dwc_Auth.Retrieve()
dw_For_dddw_sltbkcnv.Retrieve()
li_Row=dw_Selectbooks_conv.RowCount()

SELECT COUNT(*)
INTO :li_Cnvt
FROM CONVERSIONBOOKS
USING SQLServerTrans;

//i_Avail=li_Row - li_Cnvt
i_Avail=li_Row
i_Converted=li_Cnvt
i_Selected=0

st_Selected.text='Selected books for conversion: '+String(0)
st_Coverted.text='Total number of books converted: '+String(li_Cnvt)
st_Avail.text='Total number of books available: '+String(i_Avail)
Close(w_Pics_retrieve_msg_box)
dw_Selectbooks_conv.SetFocus()
	

end event

type cbx_status from checkbox within w_selectbooks_conv
integer x = 832
integer y = 1376
integer width = 1074
integer height = 72
integer textsize = -8
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "Books that have some status"
end type

event clicked;string laction=""
IF this.Checked=TRUE THEN
	dw_selectbooks_conv.setFilter("Status <> '"+ laction +" '");
	dw_selectbooks_conv.Filter()
ELSE
	// remove the filter
	dw_selectbooks_conv.setFilter("");
	dw_selectbooks_conv.Filter()		
END IF
end event

type cbx_not_found from checkbox within w_selectbooks_conv
integer x = 832
integer y = 1312
integer width = 1074
integer height = 80
integer textsize = -8
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "Books that are selected but not found"
end type

event clicked;string laction="N"
IF this.Checked=TRUE THEN
	dw_selectbooks_conv.setFilter("Shipstatus = '"+ laction +" '");
	dw_selectbooks_conv.Filter()
ELSE
	// remove the filter
	dw_selectbooks_conv.setFilter("");
	dw_selectbooks_conv.Filter()		
END IF
end event

type st_auth from statictext within w_selectbooks_conv
integer x = 41
integer y = 1336
integer width = 151
integer height = 72
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

type st_coverted from statictext within w_selectbooks_conv
integer x = 955
integer y = 1252
integer width = 699
integer height = 56
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

type st_avail from statictext within w_selectbooks_conv
integer x = 1842
integer y = 1252
integer width = 754
integer height = 56
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

type st_selected from statictext within w_selectbooks_conv
integer x = 41
integer y = 1252
integer width = 699
integer height = 56
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

type dw_conversionbooks from u_pics_dw within w_selectbooks_conv
boolean visible = false
integer x = 59
integer y = 1416
integer width = 55
integer height = 48
integer taborder = 0
string dataobject = "d_conversionbooks"
boolean vscrollbar = false
end type

event itemchanged;call super::itemchanged;Long li_Row, li_Cnt,i
String ls_Authname, ls_Authname2, ls_Auth
DataWindowChild dwc_Auth

dw_For_dddw_sltbkcnv.GetChild('auth',dwc_Auth)
li_Row=dwc_Auth.GetRow()
li_Cnt=dw_Selectbooks_conv.RowCount()
ls_Auth=data
IF dwo.name='auth' THEN
	ls_Authname=Trim(dwc_Auth.GetItemString(li_Row,'authname'))
END IF
FOR i=1 TO li_Cnt
	ls_Authname2=Trim(dw_Selectbooks_conv.object.authname[i])
	IF ls_Authname2=ls_Authname THEN
		dw_Selectbooks_conv.SelectRow(0, FALSE)
		dw_Selectbooks_conv.SelectRow(i, TRUE)
		dw_Selectbooks_conv.ScrollToRow(i)
		dw_Selectbooks_conv.SetRow(i)
		dw_Selectbooks_conv.SetFocus()
		EXIT
	END IF
NEXT
	


end event

event constructor;call super::constructor;
this.SetTransObject(sqlservertrans)
end event

type dw_for_dddw_sltbkcnv from u_pics_dw within w_selectbooks_conv
integer x = 206
integer y = 1324
integer width = 594
integer height = 72
integer taborder = 10
string dataobject = "d_for_dddw_sltbkcnv"
boolean vscrollbar = false
end type

event itemchanged;call super::itemchanged;Long li_Row, li_Cnt,i
String ls_Authname, ls_Authname2, ls_Auth
DataWindowChild dwc_Auth

dw_For_dddw_sltbkcnv.GetChild('auth',dwc_Auth)
li_Row=dwc_Auth.GetRow()
li_Cnt=dw_Selectbooks_conv.RowCount()
ls_Auth=data
IF dwo.name='auth' THEN
	ls_Authname=Trim(dwc_Auth.GetItemString(li_Row,'authname'))
END IF
FOR i=1 TO li_Cnt
	ls_Authname2=Trim(dw_Selectbooks_conv.object.authname[i])
	IF ls_Authname2=ls_Authname THEN
		dw_Selectbooks_conv.SelectRow(0, FALSE)
		dw_Selectbooks_conv.SelectRow(i, TRUE)
		dw_Selectbooks_conv.ScrollToRow(i)
		dw_Selectbooks_conv.SetRow(i)
		dw_Selectbooks_conv.SetFocus()
		EXIT
	END IF
NEXT
	


end event

event constructor;call super::constructor;this.of_SetLinkage(TRUE)
this.of_SetDropDownSearch(TRUE)
this.inv_dropdownsearch.of_AddColumn("auth")

end event

type cb_cancel from u_cb within w_selectbooks_conv
event pfc_hinttext pbm_mousemove
string tag = "Close this window"
integer x = 2405
integer y = 1336
integer width = 197
integer taborder = 40
fontcharset fontcharset = ansi!
string text = "&Cancel"
end type

event pfc_hinttext;call super::pfc_hinttext;w_pics_main.setmicrohelp(This.tag)
end event

event clicked;call super::clicked;
Long li_Cnt, i, li_Re
String ls_Flag
Boolean lb_First=FALSE


li_Cnt=dw_Selectbooks_conv.RowCount()
FOR i=1 TO li_Cnt
	ls_Flag=dw_Selectbooks_conv.object.upd_Flag[i]
	IF ls_Flag='Y' AND lb_First=FALSE THEN
		li_Re=Messagebox(' ','update check box is checked, Would you update?',question!,yesNo!, 1)
		IF li_Re=1 THEN
			cb_Update.TriggerEvent('clicked')
			EXIT
		ELSE
			EXIT
		END IF
	END IF
NEXT

ib_Disableclosequery=TRUE	
Close(w_Selectbooks_conv)
end event

type cb_print from u_cb within w_selectbooks_conv
event pfc_hinttext pbm_mousemove
string tag = "Save data to oracle data base"
integer x = 1993
integer y = 1336
integer width = 169
integer taborder = 50
fontcharset fontcharset = ansi!
string text = "&Print"
end type

event pfc_hinttext;call super::pfc_hinttext;w_pics_main.setmicrohelp(This.tag)
end event

event clicked;call super::clicked;nvo_PowerPrn.of_SetPrinterOrientation(2)
dw_selectbooks_conv.TriggerEvent('pfc_print')
nvo_PowerPrn.of_SetPrinterOrientation(1)
end event

type cb_update from u_cb within w_selectbooks_conv
event pfc_hinttext pbm_mousemove
string tag = "Update conversionboos table"
integer x = 2185
integer y = 1336
integer width = 201
integer taborder = 30
fontcharset fontcharset = ansi!
string text = "&Update"
end type

event pfc_hinttext;call super::pfc_hinttext;w_pics_main.setmicrohelp(This.tag)
end event

event clicked;call super::clicked;DateTime ldt_Date
String ls_Type='C', ls_Flag, ls_Conno, ls_Conno2, ls_Cyr
Long li_Cur, i, li_Re, li_Row, li_Cnt, li_Bkseq, li_Priority

ldt_Date=DateTime(Today(),Time('00:00:00'))
li_Cnt=dw_Selectbooks_conv.RowCount()
dw_Conversionbooks.SetTransObject(sqlservertrans)
FOR i=1 TO li_Cnt
	ls_Flag=Trim(dw_Selectbooks_conv.object.upd_Flag[i])
	ls_Conno=Trim(dw_Selectbooks_conv.object.conno[i])
	ls_Conno2=Trim(dw_Selectbooks_conv.object.conno2[i])
	ls_Cyr=Trim(dw_Selectbooks_conv.object.cyr[i])
	ls_Cyr=Right(ls_Cyr,1)
	li_Priority=Long(ls_Cyr)
	IF ls_Flag='Y' AND (IsNull(ls_Conno) OR ls_Conno='') THEN
		li_Cur=dw_Conversionbooks.InsertRow(0)
		dw_Conversionbooks.object.conno[li_Cur]=ls_Conno2
		dw_Conversionbooks.object.priority[li_Cur]= li_Priority
		dw_Conversionbooks.object.convassigndate[li_Cur]=ldt_Date
		dw_Conversionbooks.object.action_Type[li_Cur]=ls_Type
	END IF
	li_Bkseq=dw_Selectbooks_conv.object.bkseq[i]
NEXT
li_Re=dw_Conversionbooks.Update()
IF li_Re=1 THEN
	COMMIT USING sqlservertrans;
ELSE
	ROLLBACK USING sqlservertrans;
END IF

parent.TriggerEvent('pfc_postopen')



end event

type dw_selectbooks_conv from u_pics_dw within w_selectbooks_conv
integer x = 23
integer y = 24
integer width = 2583
integer height = 1216
integer taborder = 20
string dataobject = "d_selectbooks_conv"
end type

event itemchanged;call super::itemchanged;long li_row, li_count=0,i, li_avail, li_cnvt, li_selected
string  ls_flag, ls_yr, ls_date, ls_conno
datetime ldt_cvdate, ldt_cvdate2
date ld_cvdate
ls_date=string(today(), 'mm/dd/yyyy')
ls_yr=right(ls_date, 4)
if dwo.name ='upd_flag' then
	ls_flag=data
	if ls_flag='Y' then
		ldt_cvdate2=dw_selectbooks_conv.object.convassigndate[row]
		if isnull(ldt_cvdate2)=false then
			return 1
		end if
		dw_selectbooks_conv.object.cyr[row]= ls_yr
	else
		ldt_cvdate=dw_selectbooks_conv.object.convassigndate[row]
		ld_cvdate=date(ldt_cvdate)
		ls_date=string(ld_cvdate,'mm/dd/yyyy')
		ls_yr=right(ls_date, 4)
		dw_selectbooks_conv.object.cyr[row]= ls_yr
	end if

   if data='Y' then
		i_selected++
		i_avail= i_avail - 1
	else
		if i_selected>0 then
			i_selected=i_selected - 1
			i_avail=i_avail + 1
		end if
	end if
	
	st_selected.text='Selected books for conversion: '+string(i_selected)
	st_coverted.text='Total number of books converted: '+string(i_converted)
	st_avail.text='Total number of books available: '+string(i_avail)
end if


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

event sqlpreview;call super::sqlpreview;//messagebox("sql",sqlsyntax)
end event


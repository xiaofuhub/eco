$PBExportHeader$w_display_distribution_schedule.srw
forward
global type w_display_distribution_schedule from w_sheet
end type
type cb_clear from u_cb within w_display_distribution_schedule
end type
type cb_exit from u_cb within w_display_distribution_schedule
end type
type st_1 from u_st within w_display_distribution_schedule
end type
type st_2 from u_st within w_display_distribution_schedule
end type
type sle_rowcount from u_sle within w_display_distribution_schedule
end type
type dw_libcodes from u_dw within w_display_distribution_schedule
end type
type dw_distsched_treeview from u_dw within w_display_distribution_schedule
end type
end forward

global type w_display_distribution_schedule from w_sheet
integer x = 87
integer y = 128
integer width = 2734
integer height = 1704
string title = "Add and Delete Distribution Code"
cb_clear cb_clear
cb_exit cb_exit
st_1 st_1
st_2 st_2
sle_rowcount sle_rowcount
dw_libcodes dw_libcodes
dw_distsched_treeview dw_distsched_treeview
end type
global w_display_distribution_schedule w_display_distribution_schedule

type variables
string is_libcd

end variables

on w_display_distribution_schedule.create
int iCurrent
call super::create
this.cb_clear=create cb_clear
this.cb_exit=create cb_exit
this.st_1=create st_1
this.st_2=create st_2
this.sle_rowcount=create sle_rowcount
this.dw_libcodes=create dw_libcodes
this.dw_distsched_treeview=create dw_distsched_treeview
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.cb_clear
this.Control[iCurrent+2]=this.cb_exit
this.Control[iCurrent+3]=this.st_1
this.Control[iCurrent+4]=this.st_2
this.Control[iCurrent+5]=this.sle_rowcount
this.Control[iCurrent+6]=this.dw_libcodes
this.Control[iCurrent+7]=this.dw_distsched_treeview
end on

on w_display_distribution_schedule.destroy
call super::destroy
destroy(this.cb_clear)
destroy(this.cb_exit)
destroy(this.st_1)
destroy(this.st_2)
destroy(this.sle_rowcount)
destroy(this.dw_libcodes)
destroy(this.dw_distsched_treeview)
end on

event pfc_preopen;call super::pfc_preopen;this.of_SetPreference(TRUE)
this.inv_preference.of_SetToolBars(TRUE)
this.inv_preference.of_SetWindow(TRUE)
this.of_SetResize(TRUE)
this.inv_resize.of_SetOrigSize(this.workspacewidth(),this.workspaceheight())
inv_resize.of_Register(dw_distsched_treeview, "scale")
inv_resize.of_Register(cb_clear, "scale")
inv_resize.of_Register(cb_exit, "scale")
inv_resize.of_Register(st_1, "scale")
inv_resize.of_Register(st_2, "scale")
inv_resize.of_Register(sle_rowcount, "scale")

end event

event open;call super::open;THIS.Windowstate = maximized!
end event

event resize;call super::resize;long ll_height

This.X = w_pics_main.X
This.Y = w_pics_main.Y
ll_height = w_pics_main.mdi_1.Height
This.Resize(w_pics_main.WorkSpaceWidth(), ll_height)
end event

type cb_clear from u_cb within w_display_distribution_schedule
event pfc_hinttext pbm_mousemove
string tag = "Clear the record(s)"
integer x = 1833
integer y = 1480
integer taborder = 0
integer textsize = -10
string text = "&Clear"
end type

event pfc_hinttext;call super::pfc_hinttext;w_pics_main.setmicrohelp(This.tag)
end event

type cb_exit from u_cb within w_display_distribution_schedule
event pfc_hinttext pbm_mousemove
string tag = "Exits from current screen"
integer x = 2272
integer y = 1484
integer taborder = 0
integer textsize = -10
string text = "E&xit"
end type

event pfc_hinttext;call super::pfc_hinttext;w_pics_main.setmicrohelp(This.tag)
end event

event clicked;call super::clicked;parent.Event pfc_close()
m_pics_main.m_menu.popMenu(300,0)
end event

type st_1 from u_st within w_display_distribution_schedule
integer x = 23
integer y = 40
integer width = 759
integer textsize = -10
long backcolor = 79741120
string text = "Please Select Library Code"
end type

type st_2 from u_st within w_display_distribution_schedule
integer x = 1440
integer y = 44
integer width = 919
integer textsize = -10
long backcolor = 79741120
string text = "Total Number of Rows Retrieved"
long bordercolor = 79741120
end type

type sle_rowcount from u_sle within w_display_distribution_schedule
integer x = 2368
integer y = 28
integer width = 229
integer height = 88
integer taborder = 0
integer textsize = -10
integer weight = 700
long textcolor = 255
boolean displayonly = true
end type

type dw_libcodes from u_dw within w_display_distribution_schedule
event pfc_keydown pbm_dwnkey
event pfc_hinttext pbm_mousemove
integer x = 791
integer y = 12
integer width = 352
integer height = 104
integer taborder = 10
string dataobject = "d_libcode"
boolean vscrollbar = false
boolean border = false
boolean livescroll = false
borderstyle borderstyle = stylebox!
end type

event pfc_hinttext;call super::pfc_hinttext;string ls_object, ls_column, ls_column_tag
long ll_pos

//This script set's microhelp at the bottom of the screen for the dw_libcodes
ls_object = THIS.getobjectatpointer()
ll_pos = pos(ls_object, "~t")
IF NOT pos(ls_object, "_t~t") > 0 THEN
	IF ll_pos > 0 THEN
		ll_pos = ll_pos -1
		ls_column = mid(ls_object,1,ll_pos)
		ls_column_tag = THIS.Describe(ls_column + ".tag")
		w_pics_main.setmicrohelp(ls_column_tag)
	ELSE
		w_pics_main.setmicrohelp("Ready")
	END IF
END IF
end event

event constructor;call super::constructor;long li_cnt
datawindowchild dwclibcd

THIS.of_settransobject(sqlservertrans)
//of_setrowselect(TRUE)
//of_setrowmanager(TRUE)
//Making Dropdowndatawindow not updateable
of_setupdateable(FALSE)
//Dropdownsearch services on
dw_libcodes.of_SetDropDownSearch(TRUE)
//Retrieve based on entered library code
dw_libcodes.inv_dropdownsearch.of_AddColumn ("libcd")
//GetChild('libcd',dwclibcd)
//li_cnt=dwclibcd.RowCount()
//messagebox('','count : '+string(li_cnt))
//Adding a empty row in dddw
dw_libcodes.Event pfc_addrow()


end event

event pfc_retrievedddw;call super::pfc_retrievedddw;Long	ll_return,li_cnt

ll_return = dw_libcodes.Event pfc_RetrieveDDDW  &
	("libcd")
messagebox('ll_return','row count = : '+string(ll_return))

//This example shows the code you add to the dw_libcodes DataWindow to retrieve rows for the state table:

DataWindowChild ldwc_dddw

IF this.GetChild(as_column, ldwc_dddw) = -1 THEN
	Return -1
ELSE
	ldwc_dddw.SetTransObject(sqlservertrans)	
	ldwc_dddw.retrieve()
	li_cnt=ldwc_dddw.RowCount()
	messagebox('','li_cnt = : '+string(li_cnt))
	Return ldwc_dddw.Retrieve()
END IF
end event

event editchanged;call super::editchanged;IF IsValid(inv_dropdownsearch) THEN
	dw_libcodes.inv_dropdownsearch.Event pfc_EditChanged(row, dwo, data)
	dw_libcodes.Modify("libcd.DDDW.ShowList = Yes")
	dw_libcodes.Modify("libcd.DDDW.VScrollbar = Yes")	
ELSE
	dw_libcodes.Modify("libcd.DDDW.ShowList= NO")
   dw_libcodes.Modify("libcd.DDDW.VScrollbar = NO")
END IF
	
//	dw_libcodes.inv_dropdownsearch.Event pfc_EditChanged(row, dwo, data)
//END IF
//

end event

event itemfocuschanged;call super::itemfocuschanged;IF IsValid(inv_dropdownsearch) THEN
	dw_libcodes.inv_dropdownsearch.Event pfc_ItemFocusChanged(row,dwo)
END IF
end event

type dw_distsched_treeview from u_dw within w_display_distribution_schedule
event pfc_hinttext pbm_mousemove
event ue_enterkey pbm_dwnprocessenter
event pfc_keydown pbm_dwnkey
integer y = 228
integer width = 2661
integer height = 1224
integer taborder = 20
string dataobject = "d_distsched_treeview"
boolean vscrollbar = false
boolean livescroll = false
end type

event ue_enterkey;call super::ue_enterkey;Send(Handle(this),256,9,Long(0,0))
return(1)
end event

event constructor;call super::constructor;THIS.of_SetTransObject( sqlservertrans )





end event

event pfc_retrieve;call super::pfc_retrieve;THIS.Reset()
RETURN THIS.Retrieve(is_libcd)
end event

event pfc_addrow;return -1
end event


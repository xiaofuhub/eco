$PBExportHeader$w_list_comm_vendor.srw
$PBExportComments$Window to Add New Commercial Vendor
forward
global type w_list_comm_vendor from w_sheet
end type
type st_1 from statictext within w_list_comm_vendor
end type
type sle_find from singlelineedit within w_list_comm_vendor
end type
type cb_edit from u_cb within w_list_comm_vendor
end type
type cb_cancel from u_cb within w_list_comm_vendor
end type
type dw_listing from u_dw within w_list_comm_vendor
end type
end forward

global type w_list_comm_vendor from w_sheet
integer x = 69
integer y = 360
integer width = 4585
integer height = 2540
string title = "Commercial Audio Vendor Listing"
st_1 st_1
sle_find sle_find
cb_edit cb_edit
cb_cancel cb_cancel
dw_listing dw_listing
end type
global w_list_comm_vendor w_list_comm_vendor

type variables
string is_orgcd
end variables

on w_list_comm_vendor.create
int iCurrent
call super::create
this.st_1=create st_1
this.sle_find=create sle_find
this.cb_edit=create cb_edit
this.cb_cancel=create cb_cancel
this.dw_listing=create dw_listing
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_1
this.Control[iCurrent+2]=this.sle_find
this.Control[iCurrent+3]=this.cb_edit
this.Control[iCurrent+4]=this.cb_cancel
this.Control[iCurrent+5]=this.dw_listing
end on

on w_list_comm_vendor.destroy
call super::destroy
destroy(this.st_1)
destroy(this.sle_find)
destroy(this.cb_edit)
destroy(this.cb_cancel)
destroy(this.dw_listing)
end on

event close;call super::close;//////////////////////////////////////////////////////////////////////////////////////////////////////////
//
//	Event:  close
//
//	Description:
//	Set menu items
//
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//	
//	Revision History
//
//	Developed by 	Date 				Version						Tracking#
//									
// Murali K.			01/25/2008      005 PICS Modifications	 Reqs: CDS.A.PTB2,ptb2.1
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//
m_pics_main.m_file.m_print.Enabled = False
m_pics_main.m_file.m_pagesetup.Enabled = False
m_pics_main.m_file.m_printimmediate.Enabled = False
m_pics_main.m_edit.m_deleterow.Enabled = False
m_pics_main.m_edit.m_addrow.Enabled = False
end event

event pfc_preopen;call super::pfc_preopen;//////////////////////////////////////////////////////////////////////////////////////////////////////////
//
//	Event: preopen
//
//	Description:
//	Resize
//
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//	
//	Revision History
//
//	Developed by 	Date 				Version						Tracking#
//									
// Murali K.			01/25/2008      005 PICS Modifications	 Reqs: CDS.A.PTB2,ptb2.1
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//

this.of_SetPreference(TRUE)
this.inv_preference.of_SetToolBars(TRUE)
this.inv_preference.of_SetWindow(TRUE)
this.of_SetResize(TRUE)
this.inv_resize.of_SetOrigSize(this.workspacewidth(),this.workspaceheight())
//inv_resize.of_Register(dw_pctil_ec_report, "scale")

end event

event resize;call super::resize;//////////////////////////////////////////////////////////////////////////////////////////////////////////
//
//	Event: resize
//
//	Description:
//	Resize
//
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//	
//	Revision History
//
//	Developed by 	Date 				Version						Tracking#
//									
// Murali K.			01/25/2008      005 PICS Modifications	 Reqs: CDS.A.PTB2,ptb2.1
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//

long ll_height

//This script is not commented because when we do minimize this screen then all the DW'S are not resizing 
This.X = w_pics_main.X
This.Y = w_pics_main.Y
ll_height = w_pics_main.mdi_1.Height
This.Resize(w_pics_main.WorkSpaceWidth(), ll_height)


end event

event pfc_postopen;call super::pfc_postopen;//////////////////////////////////////////////////////////////////////////////////////////////////////////
//
//	Event: preopen
//
//	Description:
//	Resize
//
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//	
//	Revision History
//
//	Developed by 	Date 				Version						Tracking#
//									
// Murali K.			01/25/2008      005 PICS Modifications	 Reqs: CDS.A.PTB2,ptb2.1
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//

dw_listing.event pfc_retrieve()
end event

type st_1 from statictext within w_list_comm_vendor
integer x = 96
integer y = 68
integer width = 590
integer height = 64
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "Find by Vendor Code:"
boolean focusrectangle = false
end type

type sle_find from singlelineedit within w_list_comm_vendor
integer x = 699
integer y = 52
integer width = 242
integer height = 96
integer taborder = 10
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
integer limit = 4
borderstyle borderstyle = stylelowered!
end type

event modified;//////////////////////////////////////////////////////////////////////////////////////////////////////////
//
//	Event: modified for sle_find
//
//	Description:
//	Find the vendor record after typing in the vendor code
//
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//	
//	Revision History
//
//	Developed by 	Date 				Version						Tracking#
//									
// Murali K.			01/25/2008      005 PICS Modifications	 Reqs: CDS.A.PTB2,ptb2.1
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//
String ls_vendor
long ll_nbr, ll_foundrow

ll_nbr = dw_listing.RowCount()
ls_vendor = Trim(sle_find.Text)
ll_foundrow = dw_listing.Find( &
        "vendabbr_vendabbr = '" + ls_vendor + "'", 1, ll_nbr)

 IF ll_foundrow > 0 THEN
	dw_listing.scrolltorow(ll_foundrow)
ELSE
	Messagebox('Warning', 'Vendor not found')
END IF

end event

type cb_edit from u_cb within w_list_comm_vendor
integer x = 3630
integer y = 2292
integer width = 562
integer taborder = 0
string text = "&Edit/Terminate Vendor"
end type

event clicked;call super::clicked;//////////////////////////////////////////////////////////////////////////////////////////////////////////
//
//	Event: clicked for edit/terminate vendor
//
//	Description:
// Open Edit/Terminate Vendor Screen for the selected vendor
//
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//	
//	Revision History
//
//	Developed by 	Date 				Version						Tracking#
//									
// Murali K.			01/25/2008      005 PICS Modifications	 Reqs: CDS.A.PTB2,ptb2.1
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//
Openwithparm(w_edit_terminate_vendor,is_orgcd)
//refresh
dw_listing.setredraw(FALSE)
dw_listing.event pfc_retrieve()
dw_listing.setredraw(TRUE)
end event

type cb_cancel from u_cb within w_list_comm_vendor
integer x = 4251
integer y = 2292
integer width = 274
integer taborder = 0
string text = "E&xit"
end type

event clicked;call super::clicked;//////////////////////////////////////////////////////////////////////////////////////////////////////////
//
//	Event: clicked for cb_cancel
//
//	Description:
// close - exit
//
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//	
//	Revision History
//
//	Developed by 	Date 				Version						Tracking#
//									
// Murali K.			01/25/2008      005 PICS Modifications	 Reqs: CDS.A.PTB2,ptb2.1
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//
ib_disableclosequery = TRUE
parent.Event pfc_close()
m_pics_main.m_menu.PopMenu(300, 0)

end event

type dw_listing from u_dw within w_list_comm_vendor
integer x = 59
integer y = 176
integer width = 4462
integer height = 2092
integer taborder = 0
boolean bringtotop = true
string dataobject = "d_comm_vendor_listing"
boolean hscrollbar = true
boolean livescroll = false
borderstyle borderstyle = stylebox!
boolean ib_isupdateable = false
boolean ib_rmbmenu = false
boolean ib_rmbfocuschange = false
string is_updatesallowed = ""
end type

event constructor;call super::constructor;//////////////////////////////////////////////////////////////////////////////////////////////////////////
//
//	Event: constructor for dw
//
//	Description:
//	Set trans object
//
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//	
//	Revision History
//
//	Developed by 	Date 				Version						Tracking#
//									
// Murali K.			01/25/2008      005 PICS Modifications	 Reqs: CDS.A.PTB2,ptb2.1
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//

This.of_SetTransObject(sqlserveroracletrans)
ib_rmbmenu = False
//This.of_SetRowManager(True)
//This.of_SetPrintPreview(True)
end event

event getfocus;call super::getfocus;//////////////////////////////////////////////////////////////////////////////////////////////////////////
//
//	Event: getfocus  for dw
//
//	Description:
//	Get focus
//
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//	
//	Revision History
//
//	Developed by 	Date 				Version						Tracking#
//									
// Murali K.			01/25/2008      005 PICS Modifications	 Reqs: CDS.A.PTB2,ptb2.1
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//
m_pics_main.m_edit.m_deleterow.Enabled =	FALSE
m_pics_main.m_edit.m_addrow.Enabled =	FALSE
m_pics_main.m_edit.m_cut.Enabled =	FALSE
end event

event pfc_retrieve;call super::pfc_retrieve;//////////////////////////////////////////////////////////////////////////////////////////////////////////
//
//	Event:  pfc_retrievefor dw
//
//	Description:
//	Retrieve vendor listing
//
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//	
//	Revision History
//
//	Developed by 	Date 				Version						Tracking#
//									
// Murali K.			01/25/2008      005 PICS Modifications	 Reqs: CDS.A.PTB2,ptb2.1
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//
THIS.Reset()
RETURN THIS.Retrieve(dw_listing)
end event

event retrievestart;call super::retrievestart;//////////////////////////////////////////////////////////////////////////////////////////////////////////
//
//	Event:  retrievestart for dw
//
//	Description:
//	Display retrieve status
//
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//	
//	Revision History
//
//	Developed by 	Date 				Version						Tracking#
//									
// Murali K.			01/25/2008      005 PICS Modifications	 Reqs: CDS.A.PTB2,ptb2.1
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//

OpenWithParm(w_pics_retrieve_msg_box,"Retrieving Title Information List, Please Wait...")
end event

event retrieveend;call super::retrieveend;//////////////////////////////////////////////////////////////////////////////////////////////////////////
//
//	Event:  retrieveend for dw
//
//	Description:
//	Display retrieve status
//
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//	
//	Revision History
//
//	Developed by 	Date 				Version						Tracking#
//									
// Murali K.			01/25/2008      005 PICS Modifications	 Reqs: CDS.A.PTB2,ptb2.1
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//

IF rowcount = 0 THEN
	Close(w_pics_retrieve_msg_box)
	MessageBox("ERROR","No data found.")	
Else
	Close(w_pics_retrieve_msg_box)
	Close(w_cds_gets_title_info)
End IF










end event

event rowfocuschanged;call super::rowfocuschanged;//////////////////////////////////////////////////////////////////////////////////////////////////////////
//
//	Event: rowfocuschanged for dw
//
//	Description:
//	Highlight row and set the org code
//
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//	
//	Revision History
//
//	Developed by 	Date 				Version						Tracking#
//									
// Murali K.			01/25/2008      005 PICS Modifications	 Reqs: CDS.A.PTB2,ptb2.1
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//
IF This.rowcount() > 0 THEN
	This.SelectRow(0,FALSE)
	This.SelectRow(currentrow, TRUE)
	is_orgcd = THIS.object.org_orgcd[currentrow]
END IF
end event

event doubleclicked;call super::doubleclicked;//////////////////////////////////////////////////////////////////////////////////////////////////////////
//
//	Event: rowfocuschanged for dw
//
//	Description:
//	Highlight row and set the org code
//
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//	
//	Revision History
//
//	Developed by 	Date 				Version						Tracking#
//									
// Murali K.			01/25/2008      005 PICS Modifications	 Reqs: CDS.A.PTB2,ptb2.1
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//
IF row > 0 THEN
	cb_edit.triggerevent('clicked')
END IF
end event


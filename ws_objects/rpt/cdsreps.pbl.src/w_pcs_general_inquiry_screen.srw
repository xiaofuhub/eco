$PBExportHeader$w_pcs_general_inquiry_screen.srw
forward
global type w_pcs_general_inquiry_screen from w_sheet
end type
type cb_print from u_cb within w_pcs_general_inquiry_screen
end type
type cb_print_to_file from u_cb within w_pcs_general_inquiry_screen
end type
type cb_cancel from u_cb within w_pcs_general_inquiry_screen
end type
type rte_1 from u_rte within w_pcs_general_inquiry_screen
end type
type cb_clear from commandbutton within w_pcs_general_inquiry_screen
end type
type cb_dbcancel from commandbutton within w_pcs_general_inquiry_screen
end type
type dw_pcs_general_inquiry_assigndt from u_dw within w_pcs_general_inquiry_screen
end type
type dw_pcs_general_inquiry_bkseq from u_dw within w_pcs_general_inquiry_screen
end type
type dw_pcs_general_inquiry_conno from u_dw within w_pcs_general_inquiry_screen
end type
type dw_pcs_general_inquiry_chno from u_dw within w_pcs_general_inquiry_screen
end type
type dw_pcs_general_inquiry_auth from u_dw within w_pcs_general_inquiry_screen
end type
type dw_pcs_general_inquiry_title from u_dw within w_pcs_general_inquiry_screen
end type
type dw_pcs_general_inquiry_qarecdt from u_dw within w_pcs_general_inquiry_screen
end type
type dw_pcs_general_inquiry_qacompdt from u_dw within w_pcs_general_inquiry_screen
end type
end forward

global type w_pcs_general_inquiry_screen from w_sheet
integer x = 87
integer y = 472
integer width = 2834
integer height = 1388
string title = "General Inquiry Screen"
boolean ib_isupdateable = false
cb_print cb_print
cb_print_to_file cb_print_to_file
cb_cancel cb_cancel
rte_1 rte_1
cb_clear cb_clear
cb_dbcancel cb_dbcancel
dw_pcs_general_inquiry_assigndt dw_pcs_general_inquiry_assigndt
dw_pcs_general_inquiry_bkseq dw_pcs_general_inquiry_bkseq
dw_pcs_general_inquiry_conno dw_pcs_general_inquiry_conno
dw_pcs_general_inquiry_chno dw_pcs_general_inquiry_chno
dw_pcs_general_inquiry_auth dw_pcs_general_inquiry_auth
dw_pcs_general_inquiry_title dw_pcs_general_inquiry_title
dw_pcs_general_inquiry_qarecdt dw_pcs_general_inquiry_qarecdt
dw_pcs_general_inquiry_qacompdt dw_pcs_general_inquiry_qacompdt
end type
global w_pcs_general_inquiry_screen w_pcs_general_inquiry_screen

type variables
date id_stdt, id_enddt
string is_specattn
datawindow idw
boolean ib_cbx_checked =false,ib_cancel=FALSE

end variables

forward prototypes
public subroutine wf_pcs_save_multipartbooks ()
public subroutine wf_pcs_save_nofishe ()
end prototypes

public subroutine wf_pcs_save_multipartbooks ();
end subroutine

public subroutine wf_pcs_save_nofishe ();
end subroutine

on w_pcs_general_inquiry_screen.create
int iCurrent
call super::create
this.cb_print=create cb_print
this.cb_print_to_file=create cb_print_to_file
this.cb_cancel=create cb_cancel
this.rte_1=create rte_1
this.cb_clear=create cb_clear
this.cb_dbcancel=create cb_dbcancel
this.dw_pcs_general_inquiry_assigndt=create dw_pcs_general_inquiry_assigndt
this.dw_pcs_general_inquiry_bkseq=create dw_pcs_general_inquiry_bkseq
this.dw_pcs_general_inquiry_conno=create dw_pcs_general_inquiry_conno
this.dw_pcs_general_inquiry_chno=create dw_pcs_general_inquiry_chno
this.dw_pcs_general_inquiry_auth=create dw_pcs_general_inquiry_auth
this.dw_pcs_general_inquiry_title=create dw_pcs_general_inquiry_title
this.dw_pcs_general_inquiry_qarecdt=create dw_pcs_general_inquiry_qarecdt
this.dw_pcs_general_inquiry_qacompdt=create dw_pcs_general_inquiry_qacompdt
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.cb_print
this.Control[iCurrent+2]=this.cb_print_to_file
this.Control[iCurrent+3]=this.cb_cancel
this.Control[iCurrent+4]=this.rte_1
this.Control[iCurrent+5]=this.cb_clear
this.Control[iCurrent+6]=this.cb_dbcancel
this.Control[iCurrent+7]=this.dw_pcs_general_inquiry_assigndt
this.Control[iCurrent+8]=this.dw_pcs_general_inquiry_bkseq
this.Control[iCurrent+9]=this.dw_pcs_general_inquiry_conno
this.Control[iCurrent+10]=this.dw_pcs_general_inquiry_chno
this.Control[iCurrent+11]=this.dw_pcs_general_inquiry_auth
this.Control[iCurrent+12]=this.dw_pcs_general_inquiry_title
this.Control[iCurrent+13]=this.dw_pcs_general_inquiry_qarecdt
this.Control[iCurrent+14]=this.dw_pcs_general_inquiry_qacompdt
end on

on w_pcs_general_inquiry_screen.destroy
call super::destroy
destroy(this.cb_print)
destroy(this.cb_print_to_file)
destroy(this.cb_cancel)
destroy(this.rte_1)
destroy(this.cb_clear)
destroy(this.cb_dbcancel)
destroy(this.dw_pcs_general_inquiry_assigndt)
destroy(this.dw_pcs_general_inquiry_bkseq)
destroy(this.dw_pcs_general_inquiry_conno)
destroy(this.dw_pcs_general_inquiry_chno)
destroy(this.dw_pcs_general_inquiry_auth)
destroy(this.dw_pcs_general_inquiry_title)
destroy(this.dw_pcs_general_inquiry_qarecdt)
destroy(this.dw_pcs_general_inquiry_qacompdt)
end on

event close;call super::close;//m_pics_main.m_file.m_print.Enabled = False
//m_pics_main.m_file.m_pagesetup.Enabled = False
//m_pics_main.m_file.m_printimmediate.Enabled = False
//m_pics_main.m_edit.m_deleterow.Enabled = False
//m_pics_main.m_edit.m_addrow.Enabled = False
end event

event mousemove;call super::mousemove;w_pics_main.setmicrohelp("Ready")
end event

event pfc_preopen;this.of_SetPreference(TRUE)
this.inv_preference.of_SetToolBars(TRUE)
this.inv_preference.of_SetWindow(TRUE)
this.of_SetResize(TRUE)
this.inv_resize.of_SetOrigSize(this.workspacewidth(),this.workspaceheight())
inv_resize.of_Register(dw_pcs_general_inquiry_chno, "scale")
inv_resize.of_Register(dw_pcs_general_inquiry_conno, "scale")
inv_resize.of_Register(dw_pcs_general_inquiry_bkseq, "scale")
inv_resize.of_Register(dw_pcs_general_inquiry_title, "scale")
inv_resize.of_Register(dw_pcs_general_inquiry_assigndt, "scale")
inv_resize.of_Register(dw_pcs_general_inquiry_auth, "scale")
inv_resize.of_Register(dw_pcs_general_inquiry_qacompdt, "scale")
inv_resize.of_Register(dw_pcs_general_inquiry_qarecdt, "scale")
inv_resize.of_Register(cb_cancel, "scale")
inv_resize.of_Register(cb_print, "scale")
inv_resize.of_Register(cb_print_to_file, "scale")
inv_resize.of_Register(cb_clear, "scale")
inv_resize.of_Register(cb_dbcancel, "scale")



end event

event resize;call super::resize;long ll_height

//This script is not commented because when we do minimize this screen then all the DW'S are not resizing 
This.X = w_pics_main.X
This.Y = w_pics_main.Y
ll_height = w_pics_main.mdi_1.Height
This.Resize(w_pics_main.WorkSpaceWidth(), ll_height)


end event

event pfc_postopen;call super::pfc_postopen;This.windowstate = maximized!
long ll_count
string lchart_no, Title_type


CHOOSE CASE Message.StringParm
	CASE "Title info"            
		Open(w_cds_gets_general_inquiry)
		IF (IsNull(Message.StringParm)=FALSE AND Message.StringParm<>"") THEN
			Title_type = mid(Message.StringParm,1,1)
			lchart_no = mid(Message.StringParm,2)
			CHOOSE CASE Title_type
				CASE '1'
					dw_pcs_general_inquiry_chno.Visible = True
					ll_count = dw_pcs_general_inquiry_chno.Retrieve(lchart_no)
					IF ll_count = 0 THEN 
						Close(w_cds_gets_general_inquiry)
						Close(w_pcs_general_inquiry_screen)
						m_pics_main.m_menu.m_report.m_generalinquiryscreen.TriggerEvent(Clicked!)
						return
					ELSE
						dw_pcs_general_inquiry_chno.SetFocus()
					END IF
				CASE '2'
					dw_pcs_general_inquiry_conno.Visible = True
					ll_count = dw_pcs_general_inquiry_conno.Retrieve(lchart_no)
					IF ll_count = 0 THEN 
						Close(w_cds_gets_general_inquiry)
						Close(w_pcs_general_inquiry_screen)
						m_pics_main.m_menu.m_report.m_generalinquiryscreen.TriggerEvent(Clicked!)
						return
					ELSE
						dw_pcs_general_inquiry_conno.SetFocus()
					END IF
				CASE '3'
					dw_pcs_general_inquiry_bkseq.Visible = True
					ll_count = dw_pcs_general_inquiry_bkseq.Retrieve(long(lchart_no))
					IF ll_count = 0 THEN 
						Close(w_cds_gets_general_inquiry)
						Close(w_pcs_general_inquiry_screen)
						m_pics_main.m_menu.m_report.m_generalinquiryscreen.TriggerEvent(Clicked!)
						return
					ELSE
						dw_pcs_general_inquiry_bkseq.SetFocus()
					END IF
				CASE '4'
					dw_pcs_general_inquiry_title.Visible = True
					ll_count = dw_pcs_general_inquiry_title.Retrieve(lchart_no)
					IF ll_count = 0 THEN 
						Close(w_cds_gets_general_inquiry)
						Close(w_pcs_general_inquiry_screen)
						m_pics_main.m_menu.m_report.m_generalinquiryscreen.TriggerEvent(Clicked!)
						return
					ELSE
						dw_pcs_general_inquiry_title.SetFocus()
					END IF
				CASE '5'
					dw_pcs_general_inquiry_auth.Visible = True
					ll_count = dw_pcs_general_inquiry_auth.Retrieve(lchart_no)
					IF ll_count = 0 THEN 
						Close(w_cds_gets_general_inquiry)
						Close(w_pcs_general_inquiry_screen)
						m_pics_main.m_menu.m_report.m_generalinquiryscreen.TriggerEvent(Clicked!)
						return
					ELSE
						dw_pcs_general_inquiry_auth.SetFocus()
					END IF
				CASE '6'
					dw_pcs_general_inquiry_qacompdt.Visible = True
					ll_count = dw_pcs_general_inquiry_qacompdt.Retrieve(date(lchart_no))
					IF ll_count = 0 THEN 
						Close(w_cds_gets_general_inquiry)
						Close(w_pcs_general_inquiry_screen)
						m_pics_main.m_menu.m_report.m_generalinquiryscreen.TriggerEvent(Clicked!)
						return
					ELSE
						dw_pcs_general_inquiry_qacompdt.SetFocus()
					END IF
				CASE '7'
					dw_pcs_general_inquiry_qarecdt.Visible = True
					ll_count = dw_pcs_general_inquiry_qarecdt.Retrieve(date(lchart_no))
					IF ll_count = 0 THEN 
						Close(w_cds_gets_general_inquiry)
						Close(w_pcs_general_inquiry_screen)
						m_pics_main.m_menu.m_report.m_generalinquiryscreen.TriggerEvent(Clicked!)
						return
					ELSE
						dw_pcs_general_inquiry_qarecdt.SetFocus()
					END IF
				CASE '8'
					dw_pcs_general_inquiry_assigndt.Visible = True
					ll_count = dw_pcs_general_inquiry_assigndt.Retrieve(date(lchart_no))
					IF ll_count = 0 THEN 
						Close(w_cds_gets_general_inquiry)
						Close(w_pcs_general_inquiry_screen)
						m_pics_main.m_menu.m_report.m_generalinquiryscreen.TriggerEvent(Clicked!)
						return
					ELSE
						dw_pcs_general_inquiry_assigndt.SetFocus()
					END IF
			END CHOOSE
		END IF
END CHOOSE
end event

type cb_print from u_cb within w_pcs_general_inquiry_screen
integer x = 1865
integer y = 1180
integer width = 274
integer height = 84
integer taborder = 0
string text = "&Print"
end type

event clicked;call super::clicked;IF			dw_pcs_general_inquiry_bkseq.Visible = True THEN
	dw_pcs_general_inquiry_bkseq.Triggerevent("pfc_Print")
ELSEIF	dw_pcs_general_inquiry_chno.Visible = True THEN
	dw_pcs_general_inquiry_chno.Triggerevent("pfc_Print")
ELSEIF	dw_pcs_general_inquiry_conno.Visible = True THEN
	dw_pcs_general_inquiry_conno.Triggerevent("pfc_Print")
ELSEIF	dw_pcs_general_inquiry_title.Visible = True THEN
	dw_pcs_general_inquiry_title.Triggerevent("pfc_Print")
ELSEIF	dw_pcs_general_inquiry_auth.Visible = True THEN
	dw_pcs_general_inquiry_auth.Triggerevent("pfc_Print")
ELSEIF	dw_pcs_general_inquiry_qacompdt.Visible = True THEN
	dw_pcs_general_inquiry_qacompdt.Triggerevent("pfc_Print")
ELSEIF	dw_pcs_general_inquiry_qarecdt.Visible = True THEN
	dw_pcs_general_inquiry_qarecdt.Triggerevent("pfc_Print")
ELSEIF	dw_pcs_general_inquiry_assigndt.Visible = True THEN
	dw_pcs_general_inquiry_assigndt.Triggerevent("pfc_Print")
END IF

// Reset the default printer setting
f_pics_set_def_prn_setting()

end event

type cb_print_to_file from u_cb within w_pcs_general_inquiry_screen
integer x = 2171
integer y = 1180
integer width = 274
integer height = 84
integer taborder = 0
boolean bringtotop = true
string text = "&Save"
end type

event clicked;call super::clicked;IF			dw_pcs_general_inquiry_bkseq.Visible = True THEN
	dw_pcs_general_inquiry_bkseq.SaveAs()
ELSEIF	dw_pcs_general_inquiry_chno.Visible = True THEN
	dw_pcs_general_inquiry_chno.SaveAs()
ELSEIF	dw_pcs_general_inquiry_conno.Visible = True THEN
	dw_pcs_general_inquiry_conno.SaveAs()
ELSEIF	dw_pcs_general_inquiry_title.Visible = True THEN
	dw_pcs_general_inquiry_title.SaveAs()
ELSEIF	dw_pcs_general_inquiry_auth.Visible = True THEN
	dw_pcs_general_inquiry_auth.SaveAs()
ELSEIF	dw_pcs_general_inquiry_qacompdt.Visible = True THEN
	dw_pcs_general_inquiry_qacompdt.SaveAs()
ELSEIF	dw_pcs_general_inquiry_qarecdt.Visible = True THEN
	dw_pcs_general_inquiry_qarecdt.SaveAs()
ELSEIF	dw_pcs_general_inquiry_assigndt.Visible = True THEN
	dw_pcs_general_inquiry_assigndt.SaveAs()
END IF
end event

type cb_cancel from u_cb within w_pcs_general_inquiry_screen
integer x = 2478
integer y = 1180
integer width = 274
integer height = 84
integer taborder = 0
boolean bringtotop = true
string text = "&Cancel"
end type

event clicked;call super::clicked;f_pics_set_def_prn_setting()
ib_disableclosequery = TRUE
parent.Event pfc_close()
m_pics_main.m_menu.PopMenu(300, 0)
end event

type rte_1 from u_rte within w_pcs_general_inquiry_screen
boolean visible = false
integer y = 1444
integer width = 2743
integer height = 1164
integer taborder = 0
long init_backcolor = 1090519039
end type

on rte_1.create
call super::create
BackColor=1090519039
end on

type cb_clear from commandbutton within w_pcs_general_inquiry_screen
integer x = 1554
integer y = 1180
integer width = 274
integer height = 84
boolean bringtotop = true
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "C&lear"
end type

event clicked;close(w_pcs_general_inquiry_screen)
m_pics_main.m_menu.m_report.m_generalinquiryscreen.TriggerEvent(Clicked!)

end event

type cb_dbcancel from commandbutton within w_pcs_general_inquiry_screen
integer x = 608
integer y = 1180
integer width = 279
integer height = 80
boolean bringtotop = true
integer textsize = -8
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "DB Cancel"
end type

event clicked;// unsupported appeon feature 3/29/10
//IF			dw_pcs_general_inquiry_bkseq.Visible = True THEN
//	dw_pcs_general_inquiry_bkseq.DBCancel()
//ELSEIF	dw_pcs_general_inquiry_chno.Visible = True THEN
//	dw_pcs_general_inquiry_chno.DBCancel()
//ELSEIF	dw_pcs_general_inquiry_conno.Visible = True THEN
//	dw_pcs_general_inquiry_conno.DBCancel()
//ELSEIF	dw_pcs_general_inquiry_title.Visible = True THEN
//	dw_pcs_general_inquiry_title.DBCancel()
//ELSEIF	dw_pcs_general_inquiry_auth.Visible = True THEN
//	dw_pcs_general_inquiry_auth.DBCancel()
//ELSEIF	dw_pcs_general_inquiry_qacompdt.Visible = True THEN
//	dw_pcs_general_inquiry_qacompdt.DBCancel()
//ELSEIF	dw_pcs_general_inquiry_qarecdt.Visible = True THEN
//	dw_pcs_general_inquiry_qarecdt.DBCancel()
//ELSEIF	dw_pcs_general_inquiry_assigndt.Visible = True THEN
//	dw_pcs_general_inquiry_assigndt.DBCancel()
//END IF
end event

type dw_pcs_general_inquiry_assigndt from u_dw within w_pcs_general_inquiry_screen
boolean visible = false
integer x = 41
integer y = 12
integer width = 2743
integer height = 1164
integer taborder = 20
string dataobject = "d_pcs_general_inquiry_assingdt"
boolean hscrollbar = true
end type

event constructor;call super::constructor;this.of_SetFilter(TRUE)
this.of_SetFind(TRUE)
this.inv_filter.of_SetStyle(1)
this.inv_filter.of_SetColumnNameSource(2)
this.of_SetSort(TRUE)
this.inv_sort.of_SetStyle(1)
this.inv_sort.of_SetColumnNameSource(2)
This.of_SetTransObject(sqlservertrans)
ib_rmbmenu = False
end event

event getfocus;call super::getfocus;m_pics_main.m_edit.m_deleterow.Enabled =	FALSE
m_pics_main.m_edit.m_addrow.Enabled =	FALSE
m_pics_main.m_edit.m_cut.Enabled =	FALSE
end event

event retrieveend;call super::retrieveend;IF ib_cancel = TRUE THEN
	RETURN 1
END IF
IF rowcount = 0 THEN
	Close(w_pics_retrieve_msg_box)
	MessageBox("ERROR","No data found.")
ELSE
	Close(w_pics_retrieve_msg_box)	
End IF

end event

event retrievestart;call super::retrievestart;OpenWithParm(w_pics_retrieve_msg_box,"Retrieving data based on assignment date, Please Wait...")
end event

type dw_pcs_general_inquiry_bkseq from u_dw within w_pcs_general_inquiry_screen
boolean visible = false
integer x = 27
integer y = 8
integer width = 2743
integer height = 1164
integer taborder = 10
string dataobject = "d_pcs_general_inquiry_bkseq"
boolean hscrollbar = true
end type

event constructor;call super::constructor;this.of_SetFilter(TRUE)
this.of_SetFind(TRUE)
this.inv_filter.of_SetStyle(1)
this.inv_filter.of_SetColumnNameSource(2)
this.of_SetSort(TRUE)
this.inv_sort.of_SetStyle(1)
this.inv_sort.of_SetColumnNameSource(2)
This.of_SetTransObject(sqlservertrans)
ib_rmbmenu = False
end event

event getfocus;call super::getfocus;m_pics_main.m_edit.m_deleterow.Enabled =	FALSE
m_pics_main.m_edit.m_addrow.Enabled =	FALSE
m_pics_main.m_edit.m_cut.Enabled =	FALSE
end event

event retrieveend;call super::retrieveend;IF ib_cancel = TRUE THEN
	RETURN 1
END IF

IF rowcount = 0 THEN
	Close(w_pics_retrieve_msg_box)
	MessageBox("ERROR","No data found.")
ELSE
	Close(w_pics_retrieve_msg_box)	
End IF

end event

event retrievestart;call super::retrievestart;OpenWithParm(w_pics_retrieve_msg_box,"Retrieving data based on book number, Please Wait...")
end event

type dw_pcs_general_inquiry_conno from u_dw within w_pcs_general_inquiry_screen
boolean visible = false
integer x = 23
integer y = 8
integer width = 2743
integer height = 1164
integer taborder = 10
string dataobject = "d_pcs_general_inquiry_conno"
boolean hscrollbar = true
end type

event constructor;call super::constructor;this.of_SetFilter(TRUE)
this.of_SetFind(TRUE)
this.inv_filter.of_SetStyle(1)
this.inv_filter.of_SetColumnNameSource(2)
this.of_SetSort(TRUE)
this.inv_sort.of_SetStyle(1)
this.inv_sort.of_SetColumnNameSource(2)
This.of_SetTransObject(sqlservertrans)
ib_rmbmenu = False
end event

event getfocus;call super::getfocus;m_pics_main.m_edit.m_deleterow.Enabled =	FALSE
m_pics_main.m_edit.m_addrow.Enabled =	FALSE
m_pics_main.m_edit.m_cut.Enabled =	FALSE
end event

event retrieveend;call super::retrieveend;IF ib_cancel = TRUE THEN
	RETURN 1
END IF

IF rowcount = 0 THEN
	Close(w_pics_retrieve_msg_box)
	MessageBox("ERROR","No data found.")
ELSE
	Close(w_pics_retrieve_msg_box)	
End IF

end event

event retrievestart;call super::retrievestart;OpenWithParm(w_pics_retrieve_msg_box,"Retrieving Data based on control number, Please Wait...")
end event

type dw_pcs_general_inquiry_chno from u_dw within w_pcs_general_inquiry_screen
boolean visible = false
integer x = 14
integer width = 2743
integer height = 1164
integer taborder = 190
string dataobject = "d_pcs_general_inquiry_chno"
boolean hscrollbar = true
end type

event constructor;call super::constructor;this.of_SetFilter(TRUE)
this.of_SetFind(TRUE)
this.inv_filter.of_SetStyle(1)
this.inv_filter.of_SetColumnNameSource(2)
this.of_SetSort(TRUE)
this.inv_sort.of_SetStyle(1)
this.inv_sort.of_SetColumnNameSource(2)
This.of_SetTransObject(sqlservertrans)
ib_rmbmenu = False
end event

event getfocus;call super::getfocus;m_pics_main.m_edit.m_deleterow.Enabled =	FALSE
m_pics_main.m_edit.m_addrow.Enabled =	FALSE
m_pics_main.m_edit.m_cut.Enabled =	FALSE
end event

event retrieveend;call super::retrieveend;IF ib_cancel = TRUE THEN
	RETURN 1
END IF

IF rowcount = 0 THEN
	Close(w_pics_retrieve_msg_box)
	MessageBox("ERROR","No data found.")
ELSE
	Close(w_pics_retrieve_msg_box)	
End IF

end event

event retrievestart;call super::retrievestart;OpenWithParm(w_pics_retrieve_msg_box,"Retrieving data based on chart number, Please Wait...")
end event

type dw_pcs_general_inquiry_auth from u_dw within w_pcs_general_inquiry_screen
boolean visible = false
integer x = 32
integer y = 12
integer width = 2743
integer height = 1164
integer taborder = 200
string dataobject = "d_pcs_general_inquiry_auth"
boolean hscrollbar = true
end type

event constructor;call super::constructor;this.of_SetFilter(TRUE)
this.of_SetFind(TRUE)
this.inv_filter.of_SetStyle(1)
this.inv_filter.of_SetColumnNameSource(2)
this.of_SetSort(TRUE)
this.inv_sort.of_SetStyle(1)
this.inv_sort.of_SetColumnNameSource(2)
This.of_SetTransObject(sqlservertrans)
ib_rmbmenu = False
end event

event getfocus;call super::getfocus;m_pics_main.m_edit.m_deleterow.Enabled =	FALSE
m_pics_main.m_edit.m_addrow.Enabled =	FALSE
m_pics_main.m_edit.m_cut.Enabled =	FALSE
end event

event retrieveend;call super::retrieveend;IF ib_cancel = TRUE THEN
	RETURN 1
END IF

IF rowcount = 0 THEN
	Close(w_pics_retrieve_msg_box)
	MessageBox("ERROR","No data found.")
ELSE
	Close(w_pics_retrieve_msg_box)	
End IF

end event

event retrievestart;call super::retrievestart;OpenWithParm(w_pics_retrieve_msg_box,"Retrieving data based on author lastname, Please Wait...")
end event

type dw_pcs_general_inquiry_title from u_dw within w_pcs_general_inquiry_screen
boolean visible = false
integer x = 27
integer y = 4
integer width = 2743
integer height = 1164
integer taborder = 10
string dataobject = "d_pcs_general_inquiry_title"
boolean hscrollbar = true
end type

event constructor;call super::constructor;this.of_SetFilter(TRUE)
this.of_SetFind(TRUE)
this.inv_filter.of_SetStyle(1)
this.inv_filter.of_SetColumnNameSource(2)
this.of_SetSort(TRUE)
this.inv_sort.of_SetStyle(1)
this.inv_sort.of_SetColumnNameSource(2)
This.of_SetTransObject(sqlservertrans)
ib_rmbmenu = False
end event

event getfocus;call super::getfocus;m_pics_main.m_edit.m_deleterow.Enabled =	FALSE
m_pics_main.m_edit.m_addrow.Enabled =	FALSE
m_pics_main.m_edit.m_cut.Enabled =	FALSE
end event

event retrieveend;call super::retrieveend;IF ib_cancel = TRUE THEN
	RETURN 1
END IF


IF rowcount = 0 THEN
	Close(w_pics_retrieve_msg_box)
	MessageBox("ERROR","No data found.")
ELSE
	Close(w_pics_retrieve_msg_box)	
End IF

end event

event retrievestart;call super::retrievestart;OpenWithParm(w_pics_retrieve_msg_box,"Retrieving Data based on title, Please Wait...")
end event

type dw_pcs_general_inquiry_qarecdt from u_dw within w_pcs_general_inquiry_screen
boolean visible = false
integer x = 32
integer y = 4
integer width = 2743
integer height = 1164
integer taborder = 20
string dataobject = "d_pcs_general_inquiry_qarecdt"
boolean hscrollbar = true
end type

event constructor;call super::constructor;this.of_SetFilter(TRUE)
this.of_SetFind(TRUE)
this.inv_filter.of_SetStyle(1)
this.inv_filter.of_SetColumnNameSource(2)
this.of_SetSort(TRUE)
this.inv_sort.of_SetStyle(1)
this.inv_sort.of_SetColumnNameSource(2)
This.of_SetTransObject(sqlservertrans)
ib_rmbmenu = False
end event

event getfocus;call super::getfocus;m_pics_main.m_edit.m_deleterow.Enabled =	FALSE
m_pics_main.m_edit.m_addrow.Enabled =	FALSE
m_pics_main.m_edit.m_cut.Enabled =	FALSE
end event

event retrieveend;call super::retrieveend;IF ib_cancel = TRUE THEN
	RETURN 1
END IF

IF rowcount = 0 THEN
	Close(w_pics_retrieve_msg_box)
	MessageBox("ERROR","No data found.")
ELSE
	Close(w_pics_retrieve_msg_box)	
End IF

end event

event retrievestart;call super::retrievestart;OpenWithParm(w_pics_retrieve_msg_box,"Retrieving Data based on QA recieve date, Please Wait...")
end event

type dw_pcs_general_inquiry_qacompdt from u_dw within w_pcs_general_inquiry_screen
boolean visible = false
integer x = 41
integer y = 16
integer width = 2743
integer height = 1164
integer taborder = 20
string dataobject = "d_pcs_general_inquiry_qacompdt"
boolean hscrollbar = true
end type

event constructor;call super::constructor;this.of_SetFilter(TRUE)
this.of_SetFind(TRUE)
this.inv_filter.of_SetStyle(1)
this.inv_filter.of_SetColumnNameSource(2)
this.of_SetSort(TRUE)
this.inv_sort.of_SetStyle(1)
this.inv_sort.of_SetColumnNameSource(2)
This.of_SetTransObject(sqlservertrans)
ib_rmbmenu = False
end event

event getfocus;call super::getfocus;m_pics_main.m_edit.m_deleterow.Enabled =	FALSE
m_pics_main.m_edit.m_addrow.Enabled =	FALSE
m_pics_main.m_edit.m_cut.Enabled =	FALSE
end event

event retrieveend;call super::retrieveend;IF ib_cancel = TRUE THEN
	RETURN 1
END IF

IF rowcount = 0 THEN
	Close(w_pics_retrieve_msg_box)
	MessageBox("ERROR","No data found.")
ELSE
	Close(w_pics_retrieve_msg_box)	
End IF

end event

event retrievestart;call super::retrievestart;OpenWithParm(w_pics_retrieve_msg_box,"Retrieving Data based on QA completed date, Please Wait...")
end event


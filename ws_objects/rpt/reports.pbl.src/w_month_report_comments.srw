$PBExportHeader$w_month_report_comments.srw
forward
global type w_month_report_comments from w_response
end type
type dw_rpt_commts_in_amonth_update from u_pics_dw within w_month_report_comments
end type
type st_8 from statictext within w_month_report_comments
end type
type st_7 from statictext within w_month_report_comments
end type
type st_cntrmed from statictext within w_month_report_comments
end type
type st_6 from statictext within w_month_report_comments
end type
type dw_rpt_qa_prd_commts from u_pics_dw within w_month_report_comments
end type
type st_prdr from statictext within w_month_report_comments
end type
type st_5 from statictext within w_month_report_comments
end type
type st_4 from statictext within w_month_report_comments
end type
type st_3 from statictext within w_month_report_comments
end type
type mle_prd from u_mle within w_month_report_comments
end type
type mle_qa from u_mle within w_month_report_comments
end type
type st_day from statictext within w_month_report_comments
end type
type st_2 from statictext within w_month_report_comments
end type
type st_month from statictext within w_month_report_comments
end type
type st_1 from statictext within w_month_report_comments
end type
type cb_update from u_cb within w_month_report_comments
end type
type cb_clear from u_cb within w_month_report_comments
end type
type cb_exit from u_cb within w_month_report_comments
end type
end forward

global type w_month_report_comments from w_response
integer x = 434
integer y = 412
integer width = 3520
integer height = 1644
string title = "Voucher Report"
dw_rpt_commts_in_amonth_update dw_rpt_commts_in_amonth_update
st_8 st_8
st_7 st_7
st_cntrmed st_cntrmed
st_6 st_6
dw_rpt_qa_prd_commts dw_rpt_qa_prd_commts
st_prdr st_prdr
st_5 st_5
st_4 st_4
st_3 st_3
mle_prd mle_prd
mle_qa mle_qa
st_day st_day
st_2 st_2
st_month st_month
st_1 st_1
cb_update cb_update
cb_clear cb_clear
cb_exit cb_exit
end type
global w_month_report_comments w_month_report_comments

type variables
boolean ib_update= false
str_voucher_report istr
end variables

forward prototypes
public subroutine wf_disable_start_end_date ()
public subroutine wf_disable_contract ()
public subroutine wf_disable_all ()
end prototypes

public subroutine wf_disable_start_end_date ();//cb_find.Enabled = true
//cb_find.Default = true
//gb_2.Visible = TRUE
//gb_range.Visible = FALSE
//em_pcs_date.visible= TRUE
//em_admin_date.Visible = TRUE
//em_pcsend_date.visible = FALSE
//em_startpcs_date.Visible = FALSE
//sle_contract_number.visible = TRUE
//st_1.Visible = FALSE
//st_2.Visible = FALSE
//st_3.Visible = TRUE
//st_4.visible = TRUE
//st_pcs_date.visible =true
//em_admin_date.text = ''
//em_pcs_date.TEXT =''
//em_pcsend_date.text = ''
//em_startpcs_date.text = ''
//sle_contract_number.text = ''
//cbx_contract.checked = TRUE
//em_admin_date.Setfocus()
////em_startpcs_date.Setfocus()
end subroutine

public subroutine wf_disable_contract ();//cb_find.Enabled = true
//cb_find.Default = true
//gb_2.Visible = false
//gb_range.Visible = TRUE
//em_pcs_date.visible= false
//em_admin_date.Visible = false
//em_pcsend_date.visible = TRUE
//em_startpcs_date.Visible = TRUE
//sle_contract_number.visible = false
//st_1.Visible = TRUE
//st_2.Visible = TRUE
//st_3.Visible = false
//st_4.visible = false
//em_admin_date.text = ''
//em_pcsend_date.text = ''
//em_startpcs_date.text = ''
//sle_contract_number.text = ''
//cbx_contract.checked = FALSE
//em_startpcs_date.Setfocus()
end subroutine

public subroutine wf_disable_all ();//cb_find.Enabled = FALSE
//cb_find.Default = FALSE
//gb_2.Visible = TRUE
//gb_range.Visible = TRUE
//em_admin_date.Visible = TRUE
//em_pcsend_date.visible = TRUE
//em_startpcs_date.Visible = TRUE
//sle_contract_number.visible = TRUE
//st_1.Visible = TRUE
//st_2.Visible = TRUE
//st_3.Visible = TRUE
//st_4.visible = TRUE
//em_admin_date.text = ''
//em_pcsend_date.text = ''
//em_startpcs_date.text = ''
//sle_contract_number.text = ''
//cbx_contract.checked = FALSE
//em_startpcs_date.Setfocus()
end subroutine

on w_month_report_comments.create
int iCurrent
call super::create
this.dw_rpt_commts_in_amonth_update=create dw_rpt_commts_in_amonth_update
this.st_8=create st_8
this.st_7=create st_7
this.st_cntrmed=create st_cntrmed
this.st_6=create st_6
this.dw_rpt_qa_prd_commts=create dw_rpt_qa_prd_commts
this.st_prdr=create st_prdr
this.st_5=create st_5
this.st_4=create st_4
this.st_3=create st_3
this.mle_prd=create mle_prd
this.mle_qa=create mle_qa
this.st_day=create st_day
this.st_2=create st_2
this.st_month=create st_month
this.st_1=create st_1
this.cb_update=create cb_update
this.cb_clear=create cb_clear
this.cb_exit=create cb_exit
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_rpt_commts_in_amonth_update
this.Control[iCurrent+2]=this.st_8
this.Control[iCurrent+3]=this.st_7
this.Control[iCurrent+4]=this.st_cntrmed
this.Control[iCurrent+5]=this.st_6
this.Control[iCurrent+6]=this.dw_rpt_qa_prd_commts
this.Control[iCurrent+7]=this.st_prdr
this.Control[iCurrent+8]=this.st_5
this.Control[iCurrent+9]=this.st_4
this.Control[iCurrent+10]=this.st_3
this.Control[iCurrent+11]=this.mle_prd
this.Control[iCurrent+12]=this.mle_qa
this.Control[iCurrent+13]=this.st_day
this.Control[iCurrent+14]=this.st_2
this.Control[iCurrent+15]=this.st_month
this.Control[iCurrent+16]=this.st_1
this.Control[iCurrent+17]=this.cb_update
this.Control[iCurrent+18]=this.cb_clear
this.Control[iCurrent+19]=this.cb_exit
end on

on w_month_report_comments.destroy
call super::destroy
destroy(this.dw_rpt_commts_in_amonth_update)
destroy(this.st_8)
destroy(this.st_7)
destroy(this.st_cntrmed)
destroy(this.st_6)
destroy(this.dw_rpt_qa_prd_commts)
destroy(this.st_prdr)
destroy(this.st_5)
destroy(this.st_4)
destroy(this.st_3)
destroy(this.mle_prd)
destroy(this.mle_qa)
destroy(this.st_day)
destroy(this.st_2)
destroy(this.st_month)
destroy(this.st_1)
destroy(this.cb_update)
destroy(this.cb_clear)
destroy(this.cb_exit)
end on

event resize;call super::resize;THIS.X = 100
THIS.Y = 300
THIS.Width = 3520
THIS.Height = 1644
end event

event pfc_postopen;call super::pfc_postopen;str_voucher_report lstr
date ld_day, ld_start_date, ld_end_date
string ls_day,ls_prdr, ls_month, ls_qa_commts, ls_prd_commts, ls_med, ls_start_date,&
		ls_end_date
long li_row_count, li_re
datetime ld_dt, ld_stdt, ld_enddt

lstr =message.PowerObjectParm
istr= lstr
ls_month= lstr.array[1]
ls_prdr =lstr.array[3]
ls_med= lstr.array[13]
ls_start_date= lstr.array[8]
ls_end_date =lstr.array[9]
ld_start_date= date(ls_start_date)
ld_end_date =date( ls_end_date )
ld_day =today()
ls_day =string(today(),'mm/dd/yyyy')
ls_day=mid(ls_day,4,2 )
if left(ls_day,1 ) ='0' then
	ls_day= right(ls_day, 1)
end if
st_month.text =ls_month
st_day.text = ls_day
st_prdr.text =ls_prdr
st_cntrmed.text =ls_med
ld_dt=datetime(ld_day, time('00:00:00'))
dw_rpt_qa_prd_commts.SetTransObject( SqlServerTrans )
li_row_count=dw_rpt_qa_prd_commts.Retrieve(ld_dt, ls_prdr )
if li_row_count=0 then
	dw_rpt_qa_prd_commts.InsertRow(0 )
	dw_rpt_qa_prd_commts.SetItem(1,'rpt_month', ld_dt)
	dw_rpt_qa_prd_commts.SetItem(1,'prdr', ls_prdr )
//	dw_rpt_qa_prd_commts.SetItem(1,'cntrmed', ls_med )
else
	ls_qa_commts=dw_rpt_qa_prd_commts.GetItemString(1,'qa_comments')
	ls_prd_commts=dw_rpt_qa_prd_commts.GetItemString(1,'prd_comments')
	mle_qa.text= ls_qa_commts
	mle_prd.text =ls_prd_commts
end if
ld_stdt=datetime(ld_start_date,time('00:00:00'))
ld_enddt=datetime(ld_end_date, time('00:00:00'))
dw_rpt_commts_in_amonth_update.SetTransObject( SqlServerTrans )
li_row_count=dw_rpt_commts_in_amonth_update.Retrieve(ls_prdr,ld_stdt,ld_enddt )
//if lstr.array[7] ='cancel' then
//	close(this)
//	return 
//end if //if ok
//ls_extract = lstr.array[4]
//lstr =istr
//Openwithparm(w_month_report_comments, lstr)
//SetPointer( Hourglass!)
//this.setRedraw( false)
//istr =message.PowerObjectParm
//lstr =istr
//if lstr.array[7] ='cancel' then
//	close(this)
//	return 
//end if //if ok
//ls_extract = lstr.array[4]
end event

event key;call super::key;//IF key = Keyenter! THEN
//	IF gb_2.visible = TRUE THEN
//		IF sle_contract_number.Text = '' THEN
//	      Messagebox("Required Data","Please enter valid contract number.")
//	      cb_find.Enabled = FALSE
//	      cb_find.Default = FALSE
//	      sle_contract_number.Setfocus()
//	      RETURN
//		ELSE
//	      cb_find.Enabled = TRUE
//         cb_find.Default = TRUE
//		END IF
//   END IF
//END IF
//
//
//
//
end event

type dw_rpt_commts_in_amonth_update from u_pics_dw within w_month_report_comments
integer x = 32
integer y = 484
integer width = 3438
integer height = 932
integer taborder = 30
string dataobject = "d_rpt_commts_in_amonth_update"
boolean hscrollbar = true
boolean hsplitscroll = true
end type

event rbuttonup;//////////////////////////////////////////////////////////////////////////////
//	Event:  			rbuttonup
//	Description:	Popup menu
//////////////////////////////////////////////////////////////////////////////
//	Rev. History	Version
//						5.0   Initial version
//						5.0.04 Modified script to avoid 64K segment problem with 16bit machine code executables
// 					6.0	Added DataWindow Property to the popup menu.
// 					6.0 	Added check for the new RowManager.of_GetRestoreRow() switch.
// 					6.0.01 Added call to pfc_prermbmenuproperty to isolate calls to shared variable.
// 					6.0.01 Corrected so that dwo.protect works properly for protect expressions.
//////////////////////////////////////////////////////////////////////////////
//	Copyright © 1996-1999 Sybase, Inc. and its subsidiaries.  All rights reserved.  Any distribution of the 
// PowerBuilder Foundation Classes (PFC) source code by other than Sybase, Inc. and its subsidiaries is prohibited.
//////////////////////////////////////////////////////////////////////////////
boolean		lb_frame
boolean		lb_desired
boolean		lb_readonly
boolean		lb_editstyleattrib
integer		li_tabsequence
long			ll_getrow
string		ls_editstyle
string		ls_val
string		ls_protect
string		ls_colname
string		ls_currcolname
string		ls_type
string		ls_expression
n_cst_conversion	lnv_conversion
m_dw					lm_dw
window				lw_parent
window				lw_frame
window				lw_sheet
window				lw_childparent

// Determine if RMB popup menu should occur
if not ib_RMBmenu or IsNull (dwo) then	return 1

// No RMB support for OLE objects and graphs
ls_type = dwo.Type
if ls_type = "ole" or ls_type = "tableblob" or ls_type = "graph" then return 1

// No RMB support for print preview mode
if this.Object.DataWindow.Print.Preview = "yes" then return 1

// Determine parent window for PointerX, PointerY offset
this.of_GetParentWindow (lw_parent)
if IsValid (lw_parent) then
	// Get the MDI frame window if available
	lw_frame = lw_parent
	do while IsValid (lw_frame)
		if lw_frame.windowtype = MDI! or lw_frame.windowtype = MDIHelp! then
			lb_frame = true
			exit
		else
			lw_frame = lw_frame.ParentWindow()
		end if
	loop
	
	if lb_frame then
		// If MDI frame window is available, use it as the reference point for the
		// popup menu for sheets (windows opened with OpenSheet function) or child windows
		if lw_parent.windowtype = Child! then
			lw_parent = lw_frame
		else
			lw_sheet = lw_frame.GetFirstSheet()
			if IsValid (lw_sheet) then
				do
					// Use frame reference for popup menu if the parentwindow is a sheet
					if lw_sheet = lw_parent then
						lw_parent = lw_frame
						exit
					end if
					lw_sheet = lw_frame.GetNextSheet (lw_sheet)
				loop until IsNull(lw_sheet) Or not IsValid (lw_sheet)
			end if
		end if
	else
		// SDI application.  All windows except for child windows will use the parent
		// window of the control as the reference point for the popmenu
		if lw_parent.windowtype = Child! then
			lw_childparent = lw_parent.ParentWindow()
			if IsValid (lw_childparent) then
				lw_parent = lw_childparent
			end if
		end if
	end if
else
	return 1
end if

// Create popup menu
lm_dw = create m_dw
lm_dw.of_SetParent (this)

//////////////////////////////////////////////////////////////////////////////
// Main popup menu operations
//////////////////////////////////////////////////////////////////////////////
ll_getrow = this.GetRow()

ls_val = this.Object.DataWindow.ReadOnly
lb_readonly = lnv_conversion.of_Boolean (ls_val)

choose case ls_type
	case "datawindow", "column", "compute", "text", "report", &
		"bitmap", "line", "ellipse", "rectangle", "roundrectangle"

		// Row operations based on readonly status
		lm_dw.m_table.m_insert.Enabled = not lb_readonly
		lm_dw.m_table.m_addrow.Enabled = not lb_readonly
		lm_dw.m_table.m_delete.Enabled = not lb_readonly

		// Menu item enablement for current row
		if not lb_readonly then
			lb_desired = False
			if ll_getrow > 0 then lb_desired = true
			lm_dw.m_table.m_delete.Enabled = lb_desired
			lm_dw.m_table.m_insert.Enabled = lb_desired			
		end if
		
	case else
		lm_dw.m_table.m_insert.Enabled = false
		lm_dw.m_table.m_delete.Enabled = false
		lm_dw.m_table.m_addrow.Enabled = false
end choose

// Get column properties
ls_currcolname = this.GetColumnName()
if ls_type = "column" then
	ls_editstyle = dwo.Edit.Style
	ls_colname = dwo.Name
	ls_protect = dwo.Protect
	if not IsNumber(ls_protect) then
		// Since it is not a number, it must be an expression.
		ls_expression = Right(ls_protect, Len(ls_protect) - Pos(ls_protect, "~t"))
		ls_expression = "Evaluate(~""+ls_expression+","+String(row)+")"
		ls_protect = this.Describe(ls_expression)
	end if
	ls_val = dwo.TabSequence
	if IsNumber (ls_val) then
		li_tabsequence = Integer (ls_val)
	end if
end if

//////////////////////////////////////////////////////////////////////////////
// Transfer operations.  Only enable for editable column edit styles
//////////////////////////////////////////////////////////////////////////////
lm_dw.m_table.m_copy.Enabled = false
lm_dw.m_table.m_cut.Enabled = false
lm_dw.m_table.m_paste.Enabled = false
lm_dw.m_table.m_selectall.Enabled = false

// Get the column/editystyle specific editable flag.
if ls_type = "column" and not lb_readonly then
	choose case ls_editstyle
		case "edit"
			ls_val = dwo.Edit.DisplayOnly
		case "editmask"
			ls_val = dwo.EditMask.Readonly
		case "ddlb"
			ls_val = dwo.DDLB.AllowEdit
		case "dddw"
			ls_val = dwo.DDDW.AllowEdit
		case else
			ls_val = ""
	end choose
	lb_editstyleattrib = lnv_conversion.of_Boolean (ls_val)
	if IsNull(lb_editstyleattrib) then lb_editstyleattrib = false
end if

if ls_type = "column" and not lb_readonly then
	if dwo.BitmapName = "no" and ls_editstyle <> "checkbox" and ls_editstyle <> "radiobuttons" then
		
		if Len (this.SelectedText()) > 0 and ll_getrow = row and ls_currcolname = ls_colname then
			// Copy
			lm_dw.m_table.m_copy.Enabled = true

			// Cut
			if li_tabsequence > 0 and ls_protect = "0" then
				lb_desired = false
				choose case ls_editstyle
					case "edit", "editmask"
						lb_desired = not lb_editstyleattrib
					case "ddlb", "dddw"
						lb_desired = lb_editstyleattrib
				end choose
				lm_dw.m_table.m_cut.Enabled = lb_desired
			end if
		end if
			
		if li_tabsequence > 0 and ls_protect = "0" then
			// Paste
			if Len (ClipBoard()) > 0 then
				lb_desired = false
				choose case ls_editstyle
					case "edit", "editmask"
						lb_desired = not lb_editstyleattrib
					case "ddlb", "dddw"
						lb_desired = lb_editstyleattrib
				end choose
				lm_dw.m_table.m_paste.Enabled = false
			end if

			// Select All
			if Len (this.GetText()) > 0 and ll_getrow = row and ls_currcolname = ls_colname then
				choose case ls_editstyle
					case "ddlb", "dddw"
						lb_desired = lb_editstyleattrib						
					case else
						lb_desired = true
				end choose
				lm_dw.m_table.m_selectall.Enabled = false				
			end if
		end if

	end if
end if

//////////////////////////////////////////////////////////////////////////////
// Services
//////////////////////////////////////////////////////////////////////////////
// Row Manager
if IsValid (inv_RowManager) then
	// Undelete capability
	if inv_RowManager.of_IsRestoreRow() then
		lm_dw.m_table.m_restorerow.Visible = true
		if this.DeletedCount() > 0 and not lb_readonly then
			lm_dw.m_table.m_restorerow.Enabled = true
		else
			lm_dw.m_table.m_restorerow.Enabled = false
		end if
	end if
else
	lm_dw.m_table.m_restorerow.Visible = false
	lm_dw.m_table.m_restorerow.Enabled = false
end if

// QueryMode
// Default to false
lm_dw.m_table.m_operators.Visible = false
lm_dw.m_table.m_operators.Enabled = false
lm_dw.m_table.m_values.Visible = false
lm_dw.m_table.m_values.Enabled = false
lm_dw.m_table.m_dash12.Visible = false

if IsValid (inv_QueryMode) then
	if inv_QueryMode.of_GetEnabled() then
		// Do not allow undelete while in querymode
		lm_dw.m_table.m_restorerow.Visible = false
		lm_dw.m_table.m_restorerow.Enabled = false		

		// Default visible to true if in querymode
		lm_dw.m_table.m_values.Visible = true
		lm_dw.m_table.m_operators.Visible = true
		lm_dw.m_table.m_dash12.Visible = true

		if ls_type = "column" and not lb_readonly then
			if dwo.bitmapname = "no" and ls_editstyle <> "checkbox" and ls_editstyle <> "radiobuttons" then
				if li_tabsequence > 0 and ls_protect = "0" then				
					lb_desired = false
					choose case ls_editstyle
						case "edit", "editmask"
							lb_desired = not lb_editstyleattrib
						case "ddlb", "dddw"
							lb_desired = lb_editstyleattrib
					end choose
					// Enablement based on column				
					lm_dw.m_table.m_values.Enabled = lb_desired
					lm_dw.m_table.m_operators.Enabled = lb_desired
				end if
			end if
		end if
	end if
end if
lm_dw.m_table.m_copy.Enabled = false
lm_dw.m_table.m_cut.Enabled = false
lm_dw.m_table.m_paste.Enabled = false
lm_dw.m_table.m_selectall.Enabled = false
lm_dw.m_table.m_insert.Enabled = false
lm_dw.m_table.m_delete.Enabled = true
lm_dw.m_table.m_addrow.Enabled = false
// DataWindow property entries. (isolate calls to shared variable)

this.event pfc_prermbmenuproperty (lm_dw)

// Allow for any other changes to the popup menu before it opens
this.event pfc_prermbmenu (lm_dw)

// Send rbuttonup notification to row selection service
if IsValid (inv_RowSelect) then inv_RowSelect.event pfc_rbuttonup (xpos, ypos, row, dwo)

// Popup menu
lm_dw.m_table.PopMenu (lw_parent.PointerX() + 5, lw_parent.PointerY() + 10)

destroy lm_dw

return 1
end event

type st_8 from statictext within w_month_report_comments
integer x = 23
integer y = 220
integer width = 343
integer height = 52
integer textsize = -8
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 33554432
long backcolor = 80263581
string text = "Comments:"
boolean focusrectangle = false
end type

type st_7 from statictext within w_month_report_comments
integer x = 23
integer y = 388
integer width = 343
integer height = 68
integer textsize = -8
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 33554432
long backcolor = 80263581
string text = "Comments:"
boolean focusrectangle = false
end type

type st_cntrmed from statictext within w_month_report_comments
integer x = 1326
integer y = 24
integer width = 210
integer height = 64
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 33554432
long backcolor = 67108864
boolean focusrectangle = false
end type

type st_6 from statictext within w_month_report_comments
integer x = 896
integer y = 24
integer width = 219
integer height = 64
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 33554432
long backcolor = 67108864
string text = "Media:"
boolean focusrectangle = false
end type

type dw_rpt_qa_prd_commts from u_pics_dw within w_month_report_comments
boolean visible = false
integer x = 41
integer y = 1476
integer width = 46
integer height = 40
integer taborder = 50
string dataobject = "d_rpt_qa_prd_commts"
end type

type st_prdr from statictext within w_month_report_comments
integer x = 512
integer y = 24
integer width = 174
integer height = 64
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 33554432
long backcolor = 67108864
boolean focusrectangle = false
end type

type st_5 from statictext within w_month_report_comments
integer x = 23
integer y = 24
integer width = 279
integer height = 64
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 33554432
long backcolor = 67108864
string text = "Producer:"
boolean focusrectangle = false
end type

type st_4 from statictext within w_month_report_comments
integer x = 23
integer y = 124
integer width = 343
integer height = 64
integer textsize = -8
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 33554432
long backcolor = 80263581
string text = "Delivery"
boolean focusrectangle = false
end type

type st_3 from statictext within w_month_report_comments
integer x = 23
integer y = 320
integer width = 416
integer height = 52
integer textsize = -8
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 33554432
long backcolor = 80263581
string text = "Quality Assurance"
boolean focusrectangle = false
end type

type mle_prd from u_mle within w_month_report_comments
integer x = 462
integer y = 124
integer width = 3008
integer height = 148
integer taborder = 10
end type

event modified;call super::modified;dw_rpt_qa_prd_commts.SetItem(1,'prd_comments',mle_prd.text )

end event

event rbuttonup;//////////////////////////////////////////////////////////////////////////////
//
//	Event:  rbuttonup
//
//	Description:  Popup menu
//
//////////////////////////////////////////////////////////////////////////////
//	
//	Revision History
//
//	Version
//	5.0   Initial version
//
//////////////////////////////////////////////////////////////////////////////
//
//	Copyright © 1996-1997 Sybase, Inc. and its subsidiaries.  All rights reserved.
//	Any distribution of the PowerBuilder Foundation Classes (PFC)
//	source code by other than Sybase, Inc. and its subsidiaries is prohibited.
//
//////////////////////////////////////////////////////////////////////////////

boolean	lb_frame
m_edit	lm_edit
window	lw_parent
window	lw_frame
window	lw_sheet
window	lw_childparent

// Determine if RMB popup menu should occur
if not ib_rmbmenu then
	return 1
end if

// Determine parent window for PointerX, PointerY offset
this.of_GetParentWindow (lw_parent)
if IsValid (lw_parent) then
	// Get the MDI frame window if available
	lw_frame = lw_parent
	do while IsValid (lw_frame)
		if lw_frame.windowtype = mdi! or lw_frame.windowtype = mdihelp! then
			lb_frame = true
			exit
		else
			lw_frame = lw_frame.ParentWindow()
		end if
	loop
	
	if lb_frame then
		// If MDI frame window is available, use it as the reference point for the
		// popup menu for sheets (windows opened with OpenSheet function) or child windows
		if lw_parent.windowtype = child! then
			lw_parent = lw_frame
		else
			lw_sheet = lw_frame.GetFirstSheet()
			if IsValid (lw_sheet) then
				do
					// Use frame reference for popup menu if the parentwindow is a sheet
					if lw_sheet = lw_parent then
						lw_parent = lw_frame
						exit
					end if
					lw_sheet = lw_frame.GetNextSheet (lw_sheet)
				loop until IsNull(lw_sheet) Or not IsValid (lw_sheet)
			end if
		end if
	else
		// SDI application.  All windows except for child windows will use the parent
		// window of the control as the reference point for the popmenu
		if lw_parent.windowtype = child! then
			lw_childparent = lw_parent.ParentWindow()
			if IsValid (lw_childparent) then
				lw_parent = lw_childparent
			end if
		end if
	end if
else
	return 1
end if

// Create popup menu
lm_edit = create m_edit
lm_edit.of_SetParent (this)

// Enable menu items if appropriate
lm_edit.m_edititem.m_copy.enabled = false
lm_edit.m_edititem.m_cut.enabled = false
if Len (this.SelectedText()) > 0 then
	lm_edit.m_edititem.m_copy.enabled = true
	if not this.displayonly then
		lm_edit.m_edititem.m_cut.enabled = true
	end if
end if

if Len (ClipBoard()) > 0 and not this.displayonly then
	lm_edit.m_edititem.m_paste.enabled = true
else
	lm_edit.m_edititem.m_paste.enabled = false
end if

if Len (this.text) > 0 then
	lm_edit.m_edititem.m_selectall.enabled = true
else
	lm_edit.m_edititem.m_selectall.enabled = false
end if
lm_edit.m_edititem.m_copy.enabled = false
lm_edit.m_edititem.m_cut.enabled = false
lm_edit.m_edititem.m_paste.enabled = false
lm_edit.m_edititem.m_selectall.enabled = false
this.event pfc_prermbmenu (lm_edit)

lm_edit.m_edititem.PopMenu (lw_parent.PointerX() + 5, lw_parent.PointerY() + 10)
destroy lm_edit

return 1


end event

type mle_qa from u_mle within w_month_report_comments
integer x = 462
integer y = 308
integer width = 3008
integer height = 148
integer taborder = 20
end type

event modified;call super::modified;dw_rpt_qa_prd_commts.SetItem(1,'qa_comments',mle_qa.text )

end event

event rbuttonup;//////////////////////////////////////////////////////////////////////////////
//
//	Event:  rbuttonup
//
//	Description:  Popup menu
//
//////////////////////////////////////////////////////////////////////////////
//	
//	Revision History
//
//	Version
//	5.0   Initial version
//
//////////////////////////////////////////////////////////////////////////////
//
//	Copyright © 1996-1997 Sybase, Inc. and its subsidiaries.  All rights reserved.
//	Any distribution of the PowerBuilder Foundation Classes (PFC)
//	source code by other than Sybase, Inc. and its subsidiaries is prohibited.
//
//////////////////////////////////////////////////////////////////////////////

boolean	lb_frame
m_edit	lm_edit
window	lw_parent
window	lw_frame
window	lw_sheet
window	lw_childparent

// Determine if RMB popup menu should occur
if not ib_rmbmenu then
	return 1
end if

// Determine parent window for PointerX, PointerY offset
this.of_GetParentWindow (lw_parent)
if IsValid (lw_parent) then
	// Get the MDI frame window if available
	lw_frame = lw_parent
	do while IsValid (lw_frame)
		if lw_frame.windowtype = mdi! or lw_frame.windowtype = mdihelp! then
			lb_frame = true
			exit
		else
			lw_frame = lw_frame.ParentWindow()
		end if
	loop
	
	if lb_frame then
		// If MDI frame window is available, use it as the reference point for the
		// popup menu for sheets (windows opened with OpenSheet function) or child windows
		if lw_parent.windowtype = child! then
			lw_parent = lw_frame
		else
			lw_sheet = lw_frame.GetFirstSheet()
			if IsValid (lw_sheet) then
				do
					// Use frame reference for popup menu if the parentwindow is a sheet
					if lw_sheet = lw_parent then
						lw_parent = lw_frame
						exit
					end if
					lw_sheet = lw_frame.GetNextSheet (lw_sheet)
				loop until IsNull(lw_sheet) Or not IsValid (lw_sheet)
			end if
		end if
	else
		// SDI application.  All windows except for child windows will use the parent
		// window of the control as the reference point for the popmenu
		if lw_parent.windowtype = child! then
			lw_childparent = lw_parent.ParentWindow()
			if IsValid (lw_childparent) then
				lw_parent = lw_childparent
			end if
		end if
	end if
else
	return 1
end if

// Create popup menu
lm_edit = create m_edit
lm_edit.of_SetParent (this)

// Enable menu items if appropriate
lm_edit.m_edititem.m_copy.enabled = false
lm_edit.m_edititem.m_cut.enabled = false
if Len (this.SelectedText()) > 0 then
	lm_edit.m_edititem.m_copy.enabled = true
	if not this.displayonly then
		lm_edit.m_edititem.m_cut.enabled = true
	end if
end if

if Len (ClipBoard()) > 0 and not this.displayonly then
	lm_edit.m_edititem.m_paste.enabled = true
else
	lm_edit.m_edititem.m_paste.enabled = false
end if

if Len (this.text) > 0 then
	lm_edit.m_edititem.m_selectall.enabled = true
else
	lm_edit.m_edititem.m_selectall.enabled = false
end if
lm_edit.m_edititem.m_copy.enabled = false
lm_edit.m_edititem.m_cut.enabled = false
lm_edit.m_edititem.m_paste.enabled = false
lm_edit.m_edititem.m_selectall.enabled = false

this.event pfc_prermbmenu (lm_edit)

lm_edit.m_edititem.PopMenu (lw_parent.PointerX() + 5, lw_parent.PointerY() + 10)
destroy lm_edit

return 1


end event

type st_day from statictext within w_month_report_comments
integer x = 3090
integer y = 24
integer width = 133
integer height = 64
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 33554432
long backcolor = 67108864
boolean focusrectangle = false
end type

type st_2 from statictext within w_month_report_comments
integer x = 2702
integer y = 24
integer width = 178
integer height = 64
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 33554432
long backcolor = 67108864
string text = "Day:"
boolean focusrectangle = false
end type

type st_month from statictext within w_month_report_comments
integer x = 2158
integer y = 24
integer width = 334
integer height = 64
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 33554432
long backcolor = 67108864
boolean focusrectangle = false
end type

type st_1 from statictext within w_month_report_comments
integer x = 1746
integer y = 24
integer width = 201
integer height = 64
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 33554432
long backcolor = 67108864
string text = "Month:"
boolean focusrectangle = false
end type

type cb_update from u_cb within w_month_report_comments
string tag = "update the comments you have entered"
integer x = 1184
integer y = 1444
integer taborder = 0
integer textsize = -10
string text = "&Update"
end type

event clicked;call super::clicked;long li_re, li_re2
string ls_mle_qa, ls_mle_prd
str_voucher_report lstr
date ld_day, ld_start_date, ld_end_date
string ls_day,ls_prdr, ls_month, ls_qa_commts, ls_prd_commts, ls_med, ls_start_date,&
		ls_end_date
long li_row_count
datetime ld_dt, ld_stdt, ld_enddt

li_re= dw_rpt_qa_prd_commts.Update()
li_re2 =dw_rpt_commts_in_amonth_update.update()
if li_re=1 and li_re2= 1 then
	commit using SqlServerTrans;
	ib_update=true
else
	Rollback using SqlServerTrans;
end if

mle_qa.text=''
mle_prd.text=''
//dw_rpt_qa_prd_commts.SetItem(1,'qa_comments','')
//dw_rpt_qa_prd_commts.SetItem(1,'prd_comments','')
lstr =istr
ls_prdr =lstr.array[3]
ls_start_date= lstr.array[8]
ls_end_date =lstr.array[9]
ld_start_date= date(ls_start_date)
ld_end_date =date( ls_end_date )
ld_day =today()
ld_dt=datetime(ld_day,time('00:00:00'))
dw_rpt_qa_prd_commts.Reset()
li_row_count=dw_rpt_qa_prd_commts.Retrieve(ld_dt, ls_prdr )
if li_row_count=0 then
	dw_rpt_qa_prd_commts.InsertRow(0 )
	dw_rpt_qa_prd_commts.SetItem(1,'rpt_month', ld_dt)
	dw_rpt_qa_prd_commts.SetItem(1,'prdr', ls_prdr )
//	dw_rpt_qa_prd_commts.SetItem(1,'cntrmed', ls_med )
else
	ls_qa_commts=dw_rpt_qa_prd_commts.GetItemString(1,'qa_comments')
	ls_prd_commts=dw_rpt_qa_prd_commts.GetItemString(1,'prd_comments')
	mle_qa.text= ls_qa_commts
	mle_prd.text =ls_prd_commts
end if
dw_rpt_commts_in_amonth_update.Retrieve(ls_prdr,ld_stdt,ld_enddt )
end event

type cb_clear from u_cb within w_month_report_comments
string tag = "Clear the data"
integer x = 1842
integer y = 1444
integer taborder = 0
integer textsize = -10
string text = "&Clear"
end type

event clicked;call super::clicked;mle_qa.text=''
mle_prd.text=''
dw_rpt_qa_prd_commts.SetItem(1,'qa_comments','')
dw_rpt_qa_prd_commts.SetItem(1,'prd_comments','')
//li_row_count=dw_rpt_qa_prd_commts.Retrieve(ld_day, ls_prdr )
//if li_row_count=0 then
//	dw_rpt_qa_prd_commts.InsertRow(0 )
//	dw_rpt_qa_prd_commts.SetItem(1,'rpt_month', ld_day)
//	dw_rpt_qa_prd_commts.SetItem(1,'prdr', ls_prdr )
////	dw_rpt_qa_prd_commts.SetItem(1,'cntrmed', ls_med )
//else
//	ls_qa_commts=dw_rpt_qa_prd_commts.GetItemString(1,'qa_comments')
//	ls_prd_commts=dw_rpt_qa_prd_commts.GetItemString(1,'prd_comments')
//	mle_qa.text= ls_qa_commts
//	mle_prd.text =ls_prd_commts
//end if
end event

type cb_exit from u_cb within w_month_report_comments
integer x = 2501
integer y = 1444
integer taborder = 0
integer textsize = -10
string text = "E&xit"
end type

event clicked;call super::clicked;//Parent.Event pfc_close()
if ib_update=true then
	CloseWithReturn(w_month_report_comments,'update yes')
else
	CloseWithReturn(w_month_report_comments,'update no')
end if
end event


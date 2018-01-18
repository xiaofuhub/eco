$PBExportHeader$w_sheet_mm_bk_new_invoicing.srw
forward
global type w_sheet_mm_bk_new_invoicing from w_sheet
end type
type em_invamt from uo_conno within w_sheet_mm_bk_new_invoicing
end type
type st_tot_inamt from statictext within w_sheet_mm_bk_new_invoicing
end type
type em_cntramt from uo_conno within w_sheet_mm_bk_new_invoicing
end type
type st_tot_cntrdol from statictext within w_sheet_mm_bk_new_invoicing
end type
type cbx_emb from checkbox within w_sheet_mm_bk_new_invoicing
end type
type st_2 from statictext within w_sheet_mm_bk_new_invoicing
end type
type em_totinvamt from uo_conno within w_sheet_mm_bk_new_invoicing
end type
type cb_reverse from commandbutton within w_sheet_mm_bk_new_invoicing
end type
type cb_add from commandbutton within w_sheet_mm_bk_new_invoicing
end type
type st_1 from statictext within w_sheet_mm_bk_new_invoicing
end type
type cb_update from commandbutton within w_sheet_mm_bk_new_invoicing
end type
type cb_clear from commandbutton within w_sheet_mm_bk_new_invoicing
end type
type cb_exit from commandbutton within w_sheet_mm_bk_new_invoicing
end type
type dw_mm_inv from u_pics_dw within w_sheet_mm_bk_new_invoicing
end type
type em_totinv from uo_conno within w_sheet_mm_bk_new_invoicing
end type
type cb_ovrd from commandbutton within w_sheet_mm_bk_new_invoicing
end type
type cbx_abs from checkbox within w_sheet_mm_bk_new_invoicing
end type
type dw_mm_bk_cntr from u_pics_dw within w_sheet_mm_bk_new_invoicing
end type
type dw_qa_cntr from u_pics_dw within w_sheet_mm_bk_new_invoicing
end type
type dw_mm_bkinv from u_pics_dw within w_sheet_mm_bk_new_invoicing
end type
end forward

global type w_sheet_mm_bk_new_invoicing from w_sheet
boolean visible = false
integer x = 5
integer y = 4
integer width = 2926
integer height = 1944
string title = "Invoicing"
windowstate windowstate = maximized!
em_invamt em_invamt
st_tot_inamt st_tot_inamt
em_cntramt em_cntramt
st_tot_cntrdol st_tot_cntrdol
cbx_emb cbx_emb
st_2 st_2
em_totinvamt em_totinvamt
cb_reverse cb_reverse
cb_add cb_add
st_1 st_1
cb_update cb_update
cb_clear cb_clear
cb_exit cb_exit
dw_mm_inv dw_mm_inv
em_totinv em_totinv
cb_ovrd cb_ovrd
cbx_abs cbx_abs
dw_mm_bk_cntr dw_mm_bk_cntr
dw_qa_cntr dw_qa_cntr
dw_mm_bkinv dw_mm_bkinv
end type
global w_sheet_mm_bk_new_invoicing w_sheet_mm_bk_new_invoicing

type variables
datetime Ladmdt=datetime(today(),time('00:00:00'))
boolean inv_valid=FALSE,inv_notmatched=FALSE


end variables

forward prototypes
public function boolean wf_val_admin_dt (date adm_dt)
public function decimal wf_get_inv_cost (string stage)
end prototypes

public function boolean wf_val_admin_dt (date adm_dt);IF today() < adm_dt THEN
	RETURN FALSE
ELSE
	RETURN TRUE
END IF
end function

public function decimal wf_get_inv_cost (string stage);RETURN dw_mm_bkinv.object.invtot[1]
end function

on w_sheet_mm_bk_new_invoicing.create
int iCurrent
call super::create
this.em_invamt=create em_invamt
this.st_tot_inamt=create st_tot_inamt
this.em_cntramt=create em_cntramt
this.st_tot_cntrdol=create st_tot_cntrdol
this.cbx_emb=create cbx_emb
this.st_2=create st_2
this.em_totinvamt=create em_totinvamt
this.cb_reverse=create cb_reverse
this.cb_add=create cb_add
this.st_1=create st_1
this.cb_update=create cb_update
this.cb_clear=create cb_clear
this.cb_exit=create cb_exit
this.dw_mm_inv=create dw_mm_inv
this.em_totinv=create em_totinv
this.cb_ovrd=create cb_ovrd
this.cbx_abs=create cbx_abs
this.dw_mm_bk_cntr=create dw_mm_bk_cntr
this.dw_qa_cntr=create dw_qa_cntr
this.dw_mm_bkinv=create dw_mm_bkinv
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.em_invamt
this.Control[iCurrent+2]=this.st_tot_inamt
this.Control[iCurrent+3]=this.em_cntramt
this.Control[iCurrent+4]=this.st_tot_cntrdol
this.Control[iCurrent+5]=this.cbx_emb
this.Control[iCurrent+6]=this.st_2
this.Control[iCurrent+7]=this.em_totinvamt
this.Control[iCurrent+8]=this.cb_reverse
this.Control[iCurrent+9]=this.cb_add
this.Control[iCurrent+10]=this.st_1
this.Control[iCurrent+11]=this.cb_update
this.Control[iCurrent+12]=this.cb_clear
this.Control[iCurrent+13]=this.cb_exit
this.Control[iCurrent+14]=this.dw_mm_inv
this.Control[iCurrent+15]=this.em_totinv
this.Control[iCurrent+16]=this.cb_ovrd
this.Control[iCurrent+17]=this.cbx_abs
this.Control[iCurrent+18]=this.dw_mm_bk_cntr
this.Control[iCurrent+19]=this.dw_qa_cntr
this.Control[iCurrent+20]=this.dw_mm_bkinv
end on

on w_sheet_mm_bk_new_invoicing.destroy
call super::destroy
destroy(this.em_invamt)
destroy(this.st_tot_inamt)
destroy(this.em_cntramt)
destroy(this.st_tot_cntrdol)
destroy(this.cbx_emb)
destroy(this.st_2)
destroy(this.em_totinvamt)
destroy(this.cb_reverse)
destroy(this.cb_add)
destroy(this.st_1)
destroy(this.cb_update)
destroy(this.cb_clear)
destroy(this.cb_exit)
destroy(this.dw_mm_inv)
destroy(this.em_totinv)
destroy(this.cb_ovrd)
destroy(this.cbx_abs)
destroy(this.dw_mm_bk_cntr)
destroy(this.dw_qa_cntr)
destroy(this.dw_mm_bkinv)
end on

event pfc_preopen;call super::pfc_preopen;this.of_SetPreference(TRUE)
this.inv_preference.of_SetToolBars(TRUE)
this.inv_preference.of_SetWindow(TRUE)
this.of_SetResize(TRUE)
this.inv_resize.of_SetOrigSize(this.workspacewidth(),this.workspaceheight())
inv_resize.of_Register(dw_mm_bk_cntr, "Scale")
inv_resize.of_Register(dw_mm_bkinv, "Scale")
inv_resize.of_Register(dw_mm_inv, "Scale")
inv_resize.of_Register(em_totinv, "Scale")
inv_resize.of_Register(em_totinvamt, "Scale")
inv_resize.of_Register(em_cntramt, "Scale")
inv_resize.of_Register(em_invamt, "Scale")
inv_resize.of_Register(st_1, "Scale")
inv_resize.of_Register(st_2, "Scale")
inv_resize.of_Register(st_tot_inamt, "Scale")
inv_resize.of_Register(st_tot_cntrdol, "Scale")
inv_resize.of_Register(cb_clear, "Scale")
inv_resize.of_Register(cb_exit,"Scale")
inv_resize.of_Register(cb_update, "Scale")
inv_resize.of_Register(cb_add, "Scale")
inv_resize.of_Register(cb_reverse, "Scale")
inv_resize.of_Register(cb_ovrd, "Scale")
inv_resize.of_Register(cbx_abs, "Scale")
inv_resize.of_Register(cbx_emb, "Scale")
//inv_resize.of_Register(cbx_ovd, "Scale")

end event

event resize;call super::resize;//long ll_height
//
//This.X = w_pics_main.X
//This.Y = w_pics_main.Y
//ll_height = w_pics_main.mdi_1.Height
//This.Resize(w_pics_main.WorkSpaceWidth(), ll_height)
end event

event close;//
end event

event pfc_close;//
end event

event closequery;//////////////////////////////////////////////////////////////////////////////
//
//	Event:  closequery
//
//	Description:
//	Search for unsaved datawindows prompting the user if any
//	pending updates are found.
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

Integer	li_pendingrc
Integer	li_validationrc
Integer	li_accepttextrc
Integer	li_msg
Integer	li_rc
String	ls_msgparms[]

//ib_disableclosequery = TRUE
close(w_mm_selection_list)

// Check if the CloseQuery process has been disabled
If ib_disableclosequery Then
	Return 0
End If

// Call event to perform any pre-CloseQuery processing
If This.Event pfc_preclose ( ) <> 1 Then
	// Prevent the window from closing
	Return 1  
End If

// Prevent validation error messages from appearing while the window is closing
// and allow others to check if the  CloseQuery process is in progress
ib_closestatus = True

// Check for any pending updates
li_rc = of_UpdateChecks()
If li_rc = 0 Then
	// Updates are NOT pending, allow the window to be closed.
	Return 0
ElseIf li_rc < 0 Then
	// There are Updates pending, but at least one data entry error was found.
	// Give the user an opportunity to close the window without saving changes
	If IsValid(gnv_app.inv_error) Then
		li_msg = gnv_app.inv_error.of_Message('pfc_closequery_failsvalidation', &
					 ls_msgparms, gnv_app.iapp_object.DisplayName)
	Else
		li_msg = MessageBox (gnv_app.iapp_object.DisplayName, &
					"The information entered does not pass validation and "  + &
					"must be corrected before changes can be saved.~r~n~r~n" + &
					"Close without saving changes?", &
					exclamation!, YesNo!, 2)
	End If
	If li_msg = 1 Then
		Return 0
	End If
Else
	// Changes are pending, prompt the user to determine if they should be saved
	If IsValid(gnv_app.inv_error) Then
		li_msg = gnv_app.inv_error.of_Message('pfc_closequery_savechanges',  &
					ls_msgparms, gnv_app.iapp_object.DisplayName)		
	Else
		li_msg = MessageBox ( gnv_app.iapp_object.DisplayName, &
					"Do you want to save changes?", exclamation!, YesNoCancel!)
	End If
	Choose Case li_msg
		Case 1
			// YES - Update
			// If the update fails, prevent the window from closing
			If This.Event pfc_save() >= 1 Then
				// Successful update, allow the window to be closed
				Return 0
			End If
		Case 2
			// NO - Allow the window to be closed without saving changes
			Return 0
		Case 3
			// CANCEL -  Prevent the window from closing
	End Choose
End If

// Prevent the window from closing
ib_closestatus = False
Return 1
end event

event open;call super::open;m_pics_main.m_file.m_print.Enabled 			=	TRUE
m_pics_main.m_file.m_pagesetup.Enabled		=	TRUE
m_pics_main.m_file.m_printimmediate.Enabled	=	TRUE
m_pics_main.m_edit.m_addrow.Enabled =	TRUE
ib_disableclosequery=true
end event

type em_invamt from uo_conno within w_sheet_mm_bk_new_invoicing
event pfc_hinttext pbm_mousemove
integer x = 1929
integer y = 1636
integer width = 357
integer height = 64
integer taborder = 0
string text = "00"
maskdatatype maskdatatype = decimalmask!
string mask = "###,###.00"
string displaydata = " "
end type

type st_tot_inamt from statictext within w_sheet_mm_bk_new_invoicing
integer x = 1527
integer y = 1644
integer width = 393
integer height = 60
boolean bringtotop = true
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long backcolor = 78164112
boolean enabled = false
string text = "Invoices Paid: $"
alignment alignment = right!
boolean focusrectangle = false
end type

type em_cntramt from uo_conno within w_sheet_mm_bk_new_invoicing
event pfc_hinttext pbm_mousemove
integer x = 1170
integer y = 1632
integer width = 357
integer height = 64
integer taborder = 0
string text = "00"
maskdatatype maskdatatype = decimalmask!
string mask = "###,###.00"
string displaydata = " "
end type

type st_tot_cntrdol from statictext within w_sheet_mm_bk_new_invoicing
integer x = 745
integer y = 1640
integer width = 425
integer height = 60
boolean bringtotop = true
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long backcolor = 78164112
boolean enabled = false
string text = "Contract Amount: $"
alignment alignment = right!
boolean focusrectangle = false
end type

type cbx_emb from checkbox within w_sheet_mm_bk_new_invoicing
integer x = 2341
integer y = 1636
integer width = 503
integer height = 76
boolean bringtotop = true
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long backcolor = 78164112
string text = "Embosser"
boolean lefttext = true
end type

event clicked;int ll_rows,no_rows,i,j
string Lcntr,Lcntrmed
double Lunitcost
no_rows = dw_mm_bkinv.RowCount()	
ll_rows = dw_mm_inv.RowCount()	

Lcntr = dw_qa_cntr.object.cntr[1]
select cntrmed into :Lcntrmed from ancntr where cntr = :Lcntr using sqlservertrans;

IF cbx_emb.Checked = TRUE THEN
	// If rows does exist in invoice datawindow
	IF ll_rows > 0 THEN
		FOR i=1 TO ll_rows
			IF dw_mm_inv.object.prodstage[i]='PR' THEN
				dw_mm_inv.object.prodstage[i]='EM'
			END IF
		NEXT			
		FOR j=1 TO no_rows
			IF dw_mm_bkinv.object.prod_prodstage[j]='PR' THEN
				// assign production stage of AB instead of MA.
				dw_mm_bkinv.object.prod_prodstage[j]='EM'
				// get the unitcost for abstruse mastering and assign it.
				select unitcost into :Lunitcost
				from anucost
				where cntr = :Lcntr
				and cntrmed = :Lcntrmed
				and prodstage = 'EM'
				using sqlservertrans;
				IF IsNull(Lunitcost)=FALSE THEN
					dw_mm_bkinv.object.anucost_unitcost[j]=Lunitcost
					// recalculate the sum.
					//dw_mm_bkinv.Object.invtot.Expression='sum(invcost)'
				END IF							
			END IF
		NEXT
		dw_mm_inv.SetFocus()
	END IF
ELSE
	// If rows does not exist in invoice datawindow
	IF ll_rows > 0 THEN
		FOR i=1 TO ll_rows
			IF dw_mm_inv.object.prodstage[i]='EM' THEN
				dw_mm_inv.object.prodstage[i]='PR'
			END IF
		NEXT			
		FOR j=1 TO no_rows
			IF dw_mm_bkinv.object.prod_prodstage[j]='EM' THEN
				// assign production stage of AB instead of MA.
				dw_mm_bkinv.object.prod_prodstage[j]='PR'
				// get the unitcost for abstruse mastering and assign it.
				select unitcost into :Lunitcost
				from anucost
				where cntr = :Lcntr
				and cntrmed = :Lcntrmed
				and prodstage = 'PR'
				using sqlservertrans;
				IF IsNull(Lunitcost)=FALSE THEN
					dw_mm_bkinv.object.anucost_unitcost[j]=Lunitcost
					// recalculate the sum.
					//dw_mm_bkinv.Object.invtot.Expression='sum(invcost)'
				END IF							
			END IF
		NEXT
		dw_mm_inv.SetFocus()
	END IF
END IF
end event

type st_2 from statictext within w_sheet_mm_bk_new_invoicing
integer x = 827
integer y = 1568
integer width = 411
integer height = 60
boolean bringtotop = true
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long backcolor = 78164112
boolean enabled = false
string text = "Invoice Amount: $"
alignment alignment = right!
boolean focusrectangle = false
end type

type em_totinvamt from uo_conno within w_sheet_mm_bk_new_invoicing
event pfc_hinttext pbm_mousemove
string tag = "Enter the total invoice amount."
integer x = 1234
integer y = 1556
integer width = 270
integer height = 76
integer taborder = 30
string text = "00"
maskdatatype maskdatatype = decimalmask!
string mask = "###,###.00"
string displaydata = " "
end type

event pfc_hinttext;w_pics_main.setmicrohelp(em_totinvamt.tag)

end event

event modified;real inv_amt,inv_press,mastering,unitcost,subunitcost,totinvcost=0
long units,subunits,Lbkno
int i,ll_rows,inv_rows
string Lstage,Lcntr,Lbkmed,Linvno
Date tday

tday = Today()

// save the entered invoice amount in a local variable
inv_amt = real(em_totinvamt.text)

// save the unit and subunit amounts in a local variables
units = dw_mm_bkinv.object.prod_units[2]
subunits = dw_mm_bkinv.object.prod_subunits[2]
unitcost = dw_mm_bkinv.object.anucost_unitcost[2]
subunitcost = dw_mm_bkinv.object.anucost_subunitcost[2]

// Before you delete the dummay row, get the invoice number that was typed in.
dw_mm_inv.AcceptText()
Linvno = dw_mm_inv.object.invno[1]

//MessageBox("invno",string(Linvno))

// Then, delete the dummy row
dw_mm_inv.deleterow(0)

ll_rows = dw_mm_inv.RowCount()
inv_rows = dw_mm_bkinv.RowCount()

Lbkno = long(dw_mm_bk_cntr.Gettext())
Lcntr = dw_mm_bk_cntr.object.prod_cntr[1]
Lbkmed = dw_mm_bk_cntr.object.prod_bkmed[1]

// If invoices dose not exist.
FOR i=1 to inv_rows 
	// populate the invoice's datawindow with the number of production stages
	// that has been retrieved from anucost and prod tables.
	dw_mm_inv.InsertRow(0)
	// set the contract number, book no and book media.
	dw_mm_inv.object.cntr[i] = Lcntr
	dw_mm_inv.object.bkseq[i] = Lbkno
	dw_mm_inv.object.bkmed[i] = dw_mm_bk_cntr.object.prod_bkmed[1]
	dw_mm_inv.object.invno[i] = Linvno
	dw_mm_inv.object.CREATED_DATE[i] = tday
	dw_mm_inv.object.CREATED_BY[i] = gnv_app.of_GetUserId()
	dw_mm_inv.object.MODIFIED_DATE[i] = tday
	dw_mm_inv.object.MODIFIED_BY[i] = gnv_app.of_GetUserId()
	
	// set the production stages
	Lstage = dw_mm_bk_cntr.object.prod_prodstage[i]
	IF Lstage = 'MA' AND cbx_abs.Checked THEN
		Lstage = 'AB'
	ELSEIF Lstage = 'PR' AND i > 1 AND cbx_abs.Checked THEN
		LSTAGE = 'AB'
	ELSEIF Lstage = 'PR' AND i > 1 THEN
		LSTAGE = 'MA'
	END IF
//	MessageBox("stage","row = "+string(i)+" pstage = "+Lstage)
	dw_mm_inv.object.prodstage[i] = Lstage
	// If admin date is NULL, set it to today's date
	IF IsNull(w_mm_selection_list.Ld_admindate) THEN
		//dw_mm_inv.object.admdt[i] = today()
	ELSE
		// Else set it to its value.
		Ladmdt = datetime(w_mm_selection_list.Ld_admindate,time('00:00:00'))
		dw_mm_inv.object.admdt[i] = Ladmdt
	END IF
	// Set the PCS date to today's date as default.
	dw_mm_inv.object.invamt[i] = 0
	dw_mm_inv.object.pcsdt[i] = datetime(today(),time('00:00:00'))
NEXT

// calculate the mastering amount
IF IsNull(subunitcost) THEN
	mastering = (units * unitcost)
ELSE
	mastering = (units * unitcost)+(subunits * subunitcost)
END IF

// Calculate the pressing amount by subtracting total amount by mastering amount.
inv_press = inv_amt - mastering


// Display the invoice amount as it was calculated.
dw_mm_inv.object.invamt[1] = inv_press
dw_mm_inv.object.invamt[2] = mastering
dw_mm_inv.object.admdt[1]= Ladmdt
dw_mm_inv.object.admdt[2]= Ladmdt
FOR i=1 to dw_mm_bkinv.RowCount()
	Lstage = dw_mm_bkinv.object.prod_prodstage[i]
	totinvcost = totinvcost + wf_get_inv_cost(Lstage)
NEXT

real ltotbkinv
ltotbkinv = dw_mm_bkinv.object.invtot[1]
IF abs(inv_amt - ltotbkinv) > 1 THEN
//	MessageBox("amount"," total production invoice = "+string(ltotbkinv)+" total invoice amount = "+string(inv_amt))	
	dw_mm_inv.Object.ovrcd.TabSequence='60'
	cb_update.Enabled = FALSE
ELSE
	cb_update.Enabled = TRUE	
END IF

dw_mm_inv.SetFocus()
dw_mm_inv.SetColumn(7)
end event

type cb_reverse from commandbutton within w_sheet_mm_bk_new_invoicing
string tag = "Reverse the record"
integer x = 242
integer y = 1716
integer width = 736
integer height = 108
boolean bringtotop = true
integer textsize = -10
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "&Reverse Invoicing Row"
end type

event clicked;integer	li_rc, i,rtn,CurRow,lnewrow, li_cnt,li_max
dec Ltot,totinv,lrevinvamt, linvamt
string Linv,Lcntr,Lovrcd,Lbkmed, ls_prdstg, ls_maxstg='DU',ls_rvmax, ls_rvstg, ls_orstg
date myadmdt,ld_pcsdt,tday
long Lbkseq
datetime ldt_admin

tday = Today()

//dw_mm_inv.SetFocus()

CurRow = dw_mm_inv.GetRow()
if currow=0 then
	messagebox('Warnig','You must select a row to reverse that row.'+&
		'~nTo select a row double click the row.')
	return
end if
dw_mm_inv.selectRow(0,false)
dw_mm_inv.SelectRow(curRow,true)
li_cnt=dw_mm_inv.RowCount()
for i=1 to li_cnt
	ls_prdstg=TRIM(dw_mm_inv.object.prodstage[i])
	if ls_prdstg> ls_maxstg then
		ls_maxstg=ls_prdstg
	end if
next
IF (ls_maxstg='MA' or ls_maxstg='DU')  then
	ls_rvstg='R1'
	ls_orstg='R2'
Else
	ls_rvmax=mid(ls_maxstg,2)
	li_max=integer(ls_rvmax)
	ls_rvstg= 'R'+string(li_max + 1)
	ls_orstg= 'R'+string(li_max +2 )
end if
	
IF (dw_mm_inv.IsSelected(CurRow)) THEN
		
	linv = trim(dw_mm_inv.object.invno[CurRow])
	lcntr = dw_mm_inv.object.cntr[CurRow]
	lbkseq = dw_mm_inv.object.bkseq[CurRow]
	lbkmed = dw_mm_inv.object.bkmed[CurRow]
	lovrcd = dw_mm_inv.object.ovrcd[CurRow]
 	ldt_admin = dw_mm_inv.object.admdt[CurRow]
	myadmdt = date(ldt_admin)
	lrevinvamt = -(dw_mm_inv.object.invamt[CurRow])
	linvamt = (dw_mm_inv.object.invamt[CurRow])
//	ls_prdstg=dw_mm_inv.object.prodstage[CurRow]
//	ld_pcsdt=dw_mm_inv.object.pcsdt[CurRow]
	dw_mm_inv.object.prodstage[CurRow]=ls_orstg
	
	dw_mm_inv.SetItemStatus(CurRow, 0, Primary!, DataModified!)
	dw_mm_inv.SetItemStatus(CurRow, 1, Primary!, Notmodified!)
	dw_mm_inv.SetItemStatus(CurRow, 2, Primary!, NOtmodified!)
	dw_mm_inv.SetItemStatus(CurRow, 3, Primary!, Notmodified!)
	dw_mm_inv.SetItemStatus(CurRow, 4, Primary!, DataModified!)
	dw_mm_inv.SetItemStatus(CurRow, 5, Primary!, Notmodified!)
	dw_mm_inv.SetItemStatus(CurRow, 6, Primary!, Notmodified!)
	dw_mm_inv.SetItemStatus(CurRow, 7, Primary!, Notmodified!)
	dw_mm_inv.SetItemStatus(CurRow, 8, Primary!, Notmodified!)
	dw_mm_inv.SetItemStatus(CurRow, 9, Primary!, Notmodified!)
	
	rtn = Messagebox("Reversing Invoice Record","Do you want to reverse invoice number "+"~'"+Linv+"~'"+ &
			" from contract "+"~'"+Lcntr+"~'"+&
			"~nand invoice amount : "+string(linvamt,"###,###.00")+" "+"?",Question!,YESNO!,1)
	If rtn= 1 Then
		
		lnewrow = dw_mm_inv.InsertRow(0)
		// set the contract number, book no and book media.
		dw_mm_inv.object.cntr[lnewrow] = Lcntr
		dw_mm_inv.object.invno[lnewrow] = linv
		dw_mm_inv.object.bkseq[lnewrow] = Lbkseq
		dw_mm_inv.object.bkmed[lnewrow] = Lbkmed
		dw_mm_inv.object.CREATED_DATE[lnewrow] = tday
		dw_mm_inv.object.CREATED_BY[lnewrow] = gnv_app.of_GetUserId()
		dw_mm_inv.object.MODIFIED_DATE[lnewrow] = tday
		dw_mm_inv.object.MODIFIED_BY[lnewrow] = gnv_app.of_GetUserId()
		dw_mm_inv.object.INV_MODIFIED[lnewrow] = 'Y'
		
		dw_mm_inv.object.prodstage[lnewrow] = ls_rvstg
		
		dw_mm_inv.object.admdt[lnewrow] = ladmdt
		// Set the PCS date to today's date as default.
		dw_mm_inv.object.invamt[lnewrow] = lrevinvamt
		dw_mm_inv.object.pcsdt[lnewrow] =  datetime(today(),time('00:00:00'))
		dw_mm_inv.object.ovrcd[lnewrow] = lovrcd
		
		dw_mm_inv.Selectrow(CurRow,FALSE)	
		
		FOR i=1 TO dw_mm_inv.RowCount()
			// Calculate the total invoice amount.
			IF IsNull(dw_mm_inv.object.invamt[i]) THEN
				Ltot = 0 
			ELSE
				Ltot = dw_mm_inv.object.invamt[i]
			END IF
			totinv = totinv + Ltot
		NEXT
		em_totinv.text = string(totinv,"$###,###.00")
		cb_update.Enabled = TRUE
		dw_mm_inv.AcceptText()
		
		MessageBox("Information","You must update before your changes take place in the database. If the voucher has already been created for this invoice number, please regenerate the voucher. ", Information!)
		
		return li_rc
	END IF
	
	return 1
ELSE
	MessageBox("Error","Please select a row before trying to reverse it. Double click on the row for selecting the row.", StopSign!)
	IF dw_mm_inv.IsSelected(CurRow) THEN
		dw_mm_inv.Selectrow(CurRow,FALSE)
	END IF
	return 1
END IF

end event

type cb_add from commandbutton within w_sheet_mm_bk_new_invoicing
string tag = "Add the row"
integer x = 1029
integer y = 1716
integer width = 613
integer height = 108
boolean bringtotop = true
integer textsize = -10
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "&Add Invoicing Row"
end type

event clicked;dw_mm_inv.SetFocus()
dw_mm_inv.Event pfc_addrow()

end event

type st_1 from statictext within w_sheet_mm_bk_new_invoicing
integer x = 1495
integer y = 1564
integer width = 544
integer height = 60
boolean bringtotop = true
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long backcolor = 78164112
boolean enabled = false
string text = "Total Invoice Amount: $"
alignment alignment = right!
boolean focusrectangle = false
end type

type cb_update from commandbutton within w_sheet_mm_bk_new_invoicing
event clicked pbm_bnclicked
string tag = "Update the record"
integer x = 1687
integer y = 1716
integer width = 567
integer height = 108
boolean bringtotop = true
integer textsize = -10
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "&Update Invoicing"
end type

event clicked;Int rtn,i,inv_Rows, li_Cnt, j, li_Cnt2, li_Cl1,li_Cl2, li_Cl3, li_Cl4
Real Linvamt
Date Ladmin
Boolean Override_Exist=FALSE, lb_Find=TRUE
String Lovrcd, ls_Prdstg, ls_Prdstg2,ls_cntr
Decimal ll_inv_paid,ll_money_left_in_cntr,ll_cntrdol
Long Lbkseq
DwItemStatus ldw_Status

// Get the number of rows in the invoice datawindow
inv_Rows = dw_Mm_inv.RowCount()

Lbkseq = dw_Mm_bk_cntr.object.prod_Bkseq[1]

li_Cnt = dw_Mm_inv.RowCount()
FOR i=1 TO li_Cnt
	ldw_Status=dw_Mm_inv.GetItemStatus(i,0, primary!)
	IF ldw_Status=newModified! OR ldw_Status=dataModified! THEN
		lb_Find=FALSE
		ls_Prdstg=dw_Mm_inv.object.prodstage[i]
		li_Cnt2=dw_Mm_bkinv.RowCount()
		FOR j=1 TO li_Cnt2
			ls_Prdstg2=dw_Mm_bkinv.object.prod_Prodstage[j]
			IF ls_Prdstg2=ls_Prdstg THEN
				lb_Find=TRUE
				IF ls_Prdstg='MA' OR ls_Prdstg='AB' OR ls_Prdstg='PU' THEN
					li_Cl1=dw_Mm_bkinv.object.incl1[j]
					li_Cl2=dw_Mm_bkinv.object.incl2[j]
					IF li_Cl1=1 OR li_Cl2=1 THEN
						rtn=Messagebox('Warning','You tried to enter data in the prodstage but the check box for '+&
							'this prodstage in above screen is not checked. Do you want to continue? ',&
							question!,yesNo!,1)
						IF rtn=2 THEN RETURN
					END IF
				ELSEIF ls_Prdstg='DU' OR ls_Prdstg='PB' OR ls_Prdstg='PR' OR ls_Prdstg='EB' THEN
					li_Cl3=dw_Mm_bkinv.object.incl3[j]
					li_Cl4=dw_Mm_bkinv.object.incl4[j]
					IF li_Cl3=1 OR li_Cl4=1 THEN
						rtn=Messagebox('Warning','You tried to enter data in the prodstage but the check box for '+&
							'this prodstage in above screen is not checked. Do you want to continue? ',&
							question!,yesNo!,1)
						IF rtn=2 THEN RETURN
					END IF
				END IF
			END IF
		NEXT
	END IF
NEXT
IF lb_Find=FALSE THEN
	rtn=Messagebox('Warning','There is a inconsistency between the invoicing prodstages and '+&
	  'production prodstages. Do you want to continue?',question!,yesNo!,1)
	IF rtn=2 THEN RETURN
END IF
//FOR i=1 to inv_rows
//	Linvamt 	= 	dw_mm_inv.object.invamt[i]
//	Lovrcd = dw_mm_inv.object.ovrcd[i]
//	IF Linvamt<>0 THEN
//		dw_mm_inv.SetItemStatus(i, 0, Primary!, New!)
//		Ladmdt = dw_mm_inv.object.admdt[i]
//	ELSE
//		dw_mm_inv.SetItemStatus(i, 0, Primary!, DataModified!)
//	END IF
//	IF Lovrcd <> "" THEN
//		Override_exist = TRUE
//	END IF
//NEXT	

//ls_cntr = dw_mm_inv.object.cntr[1]
//
//SELECT cntrdol INTO :ll_cntrdol FROM ANCNTR
//WHERE cntr = :ls_cntr
//USING SQLServerTrans;
//
//IF ll_cntrdol > 0 THEN
//	
//	SELECT SUM(invamt) INTO :ll_inv_paid FROM INV
//	WHERE cntr = :ls_cntr
//	USING SQLServerTrans;
//	
//	IF ll_inv_paid > 0 AND ll_inv_paid >= ll_cntrdol THEN
//		Messagebox("Warning","The total invoice amount paid under this contract "+string(ll_inv_paid)+" has exceeded the total dollar amount for this contract. "+string(ll_cntrdol))
//	ELSEIF ll_inv_paid > 0 AND ( ll_cntrdol - ll_inv_paid < 10000) THEN
//		Messagebox("Warning","The total invoice amount paid under this contract "+string(ll_inv_paid)+" and the total dollar amount for this contract is: "+string(ll_cntrdol))
//	END IF
//END IF

IF (inv_Notmatched=TRUE AND Override_Exist=FALSE) THEN 
	Messagebox("ERROR","You must enter an override code")
	RETURN -1
END IF

rtn = dw_mm_inv.EVENT pfc_Update(TRUE,FALSE)
IF rtn = 1 THEN
	rtn = dw_mm_bkinv.EVENT pfc_Update(TRUE,FALSE)
	IF rtn = 1 THEN
		SetPointer(arrow!)
		SetMicroHelp(w_Pics_main,"Ready")
		w_Mm_selection_list.Ld_Admindate =Date(Ladmdt)
	//	Ladmdt = Ladmin
		cbx_Abs.checked = FALSE
		dw_Mm_inv.object.ovrcd.TabSequence='0'
		dw_Mm_bk_cntr.Reset()
		dw_Mm_bkinv.Reset()
		dw_Mm_inv.Reset()
		dw_Mm_inv.ResetUpdate()
		em_Totinv.text = ".00"
		dw_Mm_bk_cntr.InsertRow(0)
		dw_Mm_bk_cntr.SetFocus()
		
		//Mark the mchar record
		f_update_mchar_time("",Lbkseq,"B","U")		
	
		COMMIT USING sqlservertrans;
		RETURN 1
	ELSE
		ROLLBACK USING sqlservertrans;
		Messagebox("ERROR","Update Failed.")
		RETURN -1
	END IF		
ELSE
	ROLLBACK USING sqlservertrans;
	Messagebox("ERROR","Update Failed.")
	RETURN -1
END IF
end event

type cb_clear from commandbutton within w_sheet_mm_bk_new_invoicing
event clicked pbm_bnclicked
string tag = "Clear the screen"
integer x = 2304
integer y = 1716
integer width = 251
integer height = 108
boolean bringtotop = true
integer textsize = -10
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "&Clear"
end type

event clicked;int i
// If admin date was set to null date, then set it to today's date.
IF String(Ladmdt)='1/1/00' THEN
	w_mm_selection_list.Ld_admindate = Today()
ELSE
	w_mm_selection_list.Ld_admindate = date(Ladmdt)
END IF
close(w_sheet_mm_bk_new_invoicing)
IF IsValid(w_mm_selection_list) THEN
	w_mm_selection_list.show()
	w_mm_selection_list.em_bkno.text=""
	w_mm_selection_list.em_bkno.SetFocus()
ELSE
	open(w_mm_selection_list)	
	w_mm_selection_list.em_bkno.SetFocus()
END IF



end event

type cb_exit from commandbutton within w_sheet_mm_bk_new_invoicing
event clicked pbm_bnclicked
string tag = "Exit the screen"
integer x = 2606
integer y = 1716
integer width = 247
integer height = 108
boolean bringtotop = true
integer textsize = -10
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "E&xit"
end type

event clicked;DWItemStatus ldwstatus
long li_cnt, i,li_re
boolean lb_changed=false

m_pics_main.m_file.m_print.Enabled 			=	FALSE
m_pics_main.m_file.m_pagesetup.Enabled		=	FALSE
m_pics_main.m_file.m_printimmediate.Enabled	=	FALSE
m_pics_main.m_edit.m_addrow.Enabled =	FALSE

//parent.event pfc_close()
//DWItemStatus dwcontrol.GetItemStatus ( long row, integer column, 
//	DWBuffer dwbuffe
li_cnt=dw_mm_inv.Rowcount()
for i=1 to li_cnt
	ldwstatus=dw_mm_inv.getItemStatus(i,0,primary!)
	if  (ldwstatus=newModified! or ldwstatus=DataModified!) then
		lb_changed=true
		exit
	end if
next
if lb_changed then
	li_re=messagebox('','Some thing has changed. Do you want to save change?',&
			Question!,YesNo!)
end if
if li_re=1 then
	cb_update.triggerevent(clicked!)
end if
//Parent.Event pfc_close()
close(w_mm_selection_list)
close(w_sheet_mm_bk_new_invoicing)
m_pics_main.m_menu.PopMenu(300, 0)
end event

type dw_mm_inv from u_pics_dw within w_sheet_mm_bk_new_invoicing
event ue_enterkey pbm_dwnprocessenter
event pfc_hinttext pbm_mousemove
integer x = 23
integer y = 1068
integer width = 2839
integer height = 468
integer taborder = 20
string dataobject = "d_mm_invoicing"
boolean resizable = true
boolean ib_isupdateable = false
boolean ib_rmbmenu = false
boolean ib_rmbfocuschange = false
string is_updatesallowed = ""
end type

event ue_enterkey;//Integer li_ColNbr
//
//li_ColNbr = dw_mm_inv.GetColumn() 
//
//IF (li_ColNbr=8) THEN
//   int i,currow,rowcount
//	real totinv,Ltot,totinvamt,Ldiff
//	string Lstage
//	
//	currow = dw_mm_inv.GetRow()
//	rowcount = dw_mm_inv.RowCount()
//	
//	IF IsNull(dw_mm_inv.object.invno[currow])=FALSE THEN
//		cb_update.Enabled = TRUE
//	END IF
//	// Get the production stage from invoice datawindow
//	Lstage = dw_mm_inv.object.prodstage[currow]
//	// Calculate the total invoice amount based on the production stage.
//	totinvamt = wf_get_inv_cost(Lstage)
//	// Get the invoice amount from invoice datawindow
//	Ltot = real(gettext())
//	// Compare the invoices from anucost and invoice datawindow
//	// If they match, no override is required.
//	Ldiff = abs(totinvamt - Ltot)
//	// MessageBox("differnece",string(Ldiff)+" "+string(totinvamt)+" "+string(Ltot))
//	// IF the difference is greater than 1 OR abstruse checkbox is checked then allow it then to enter override.
//	// AND if the disable override is not checked.
//	IF ((Ldiff > 1) OR (cbx_abs.Checked)) THEN
//		dw_mm_inv.Object.ovrcd.TabSequence='60'
////		w_sheet_mm_bk_invoicing.cb_update.Enabled = FALSE
//	// Otherwise if someone has enter it an override, empty it.
//	ELSEIF dw_mm_inv.object.ovrcd[currow] <> "" THEN
//		dw_mm_inv.object.ovrcd[currow] = ""
//	ELSE
//		dw_mm_inv.Object.ovrcd.TabSequence='0'
//		w_sheet_mm_bk_invoicing.cb_update.Enabled = TRUE
//	END IF
//END IF
Send(Handle(this),256,9,Long(0,0))
//return(1)
end event

event pfc_hinttext;call super::pfc_hinttext;string ls_object, ls_column, ls_column_tag
long ll_pos

//This script set's microhelp at the bottom of the screen for the Archive Title Datawindow
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

event itemchanged;call super::itemchanged;Dec ltotinv,ltotbkinv, currinvamt,ltotprodcost,ldc_Dif
Int rtn, li_Cnt, li_loop
String lmed, ls_Dif, ls_Inv1, ls_Inv2
long ll_rc // 10/10/2008 changes - do not allow duplicate record bkseq, bkmed, cntr,prodstage combination
string ls_bkmed, ls_cntr, ls_prodstage , ls_bkmed_new, ls_cntr_new, ls_prodstage_new
long ll_bkseq, ll_bkseq_new

this.AcceptText()

IF dwo.name = 'invamt' THEN
	lmed = Trim(dw_Mm_bk_cntr.object.mchar_Med[1])
	currinvamt = Dec(data)
	ltotinv = dw_Mm_inv.object.tot_Invamt[1]
	ls_Inv1 = Trim(dw_Mm_inv.object.invno[1])
	IF row=2 THEN 
		ls_Inv2 = Trim(dw_Mm_inv.object.invno[2])
	END IF
	ltotprodcost = dw_Mm_bkinv.object.prodtot[1]
	em_Totinv.text = String(ltotinv,"$###,###.00")
	ltotbkinv = dw_Mm_bkinv.object.invtot[1]
	ldc_Dif=Abs(ltotinv - ltotprodcost)
	li_Cnt=dw_Mm_inv.RowCount()
	IF Abs(currinvamt - ltotbkinv) >= 0.999  and Lmed <> 'RTB' THEN
		Messagebox('Warning','Invoice amount is different by $1 or more.'+&
				'~nPlease choose an Override code, or '+&
				'~nExit/Clear.')
		dw_Mm_inv.object.ovrcd.TabSequence='60'
		dw_Mm_inv.SetColumn(5)
		cb_Update.enabled = FALSE
	ELSEIF ldc_Dif >=0.999 AND row=2 AND lmed <> 'BR' AND lmed <> 'RTB' AND ls_Inv1=ls_Inv2 THEN
		rtn = Messagebox("Warning","The difference of total production cost "+String(ltotprodcost,"$###,###.00")+&
							"~n and the total invoicing amount "+String(ltotinv,"$###,###.00")+ &
							 "~r~nis more than one dollar. Would you like to add override code?",question!,yesNo!,1)
		IF rtn = 1 THEN
			dw_Mm_inv.object.ovrcd.TabSequence='60'
			dw_Mm_inv.SetColumn(5)
			cb_Update.enabled = FALSE
		ELSE
			dw_Mm_inv.object.ovrcd.TabSequence='0'
			dw_Mm_inv.object.ovrcd[row]=""
			cb_Update.enabled = FALSE
		END IF
	ELSEIF ldc_Dif>=0.999 AND row=2 AND lmed = 'BR' AND ls_Inv1=ls_Inv2 THEN
		rtn = Messagebox("Warning","The difference of total production cost "+String(ltotprodcost,"$###,###.00")+&
							"~n and the total invoicing amount "+String(ltotinv,"$###,###.00")+ &
							 "~r~nis not less than one dorllar. Would you like to add override?",question!,yesNo!,1)
		IF rtn = 1 THEN
			dw_Mm_inv.object.ovrcd.TabSequence='60'
			dw_Mm_inv.SetColumn(5)
			cb_Update.enabled = FALSE
		ELSE
			dw_Mm_inv.object.ovrcd.TabSequence='0'
			dw_Mm_inv.object.ovrcd[row]=""
			cb_Update.enabled = FALSE
		END IF
	ELSEIF ldc_Dif>=0.999 AND li_Cnt>=4 AND Mod(li_Cnt,2)=0 THEN
		rtn = Messagebox("Warning","The difference of total production cost "+String(ltotprodcost,"$###,###.00")+&
							"~n and the total invoicing amount "+String(ltotinv,"$###,###.00")+ &
							 "~r~nis not less than one dorllar. Would you like to add override?",question!,yesNo!,1)
		IF rtn = 1 THEN
			dw_Mm_inv.object.ovrcd.TabSequence='60'
			dw_Mm_inv.SetColumn(5)
			cb_Update.enabled = FALSE
		ELSE
			dw_Mm_inv.object.ovrcd.TabSequence='0'
			dw_Mm_inv.object.ovrcd[row]=""
			cb_Update.enabled = FALSE
		END IF
	ELSEIF Abs(currinvamt - ltotbkinv)< 0.999 THEN
		dw_Mm_inv.object.ovrcd.TabSequence='0'
		dw_Mm_inv.object.ovrcd[row]=""
		cb_Update.enabled = TRUE
	ELSE
		rtn = Messagebox("Warning","Would you like to add override code?",question!,yesNo!,1)
		IF rtn = 1 THEN
			dw_Mm_inv.object.ovrcd.TabSequence='60'
			dw_Mm_inv.SetColumn(5)
			cb_Update.enabled = TRUE
		ELSE
			dw_Mm_inv.object.ovrcd.TabSequence='60'
			dw_Mm_inv.object.ovrcd[row]=""
			cb_Update.enabled = FALSE
		END IF
	END IF
ELSEIF dwo.name = 'admdt' THEN
	IF wf_val_admin_dt(Date(data)) THEN
		Ladmdt = dw_Mm_inv.object.admdt[row]
	ELSE
		dw_Mm_inv.object.admdt.ValidationMsg = "Admin date can not be greater than todays date. Please try again."
		RETURN 1
	END IF
ELSEIF dwo.name = 'ovrcd' THEN
	IF (RightTrim(data) <> "" AND IsNull(dw_Mm_inv.object.invno[row])=FALSE) THEN
		cb_Update.enabled = TRUE
	ELSEIF (RightTrim(data) = "" AND IsNull(dw_Mm_inv.object.invno[row])=FALSE) THEN
		cb_Update.enabled = FALSE
		RETURN 1
	ELSE
		RETURN 1
	END IF
ELSEIF dwo.name = 'prodstage' THEN
//	li_cnt=dw_mm_inv.RowCount()
//	if ROW>=3 and (data='MA' OR data='DU') THEN
//		this.SetItemStatus(row,0,Primary!, DataModified!)
//	end if	
//	FOR i= 1 to dw_mm_inv.RowCount()-1
//		IF data = dw_mm_inv.object.prodstage[i] THEN
//			RETURN 1
//		END IF
//	NEXT		
//	IF (data = 'D1' OR data = 'M1' OR data = 'D2' OR data = 'M2') THEN
//		dw_mm_inv.Object.ovrcd.TabSequence='60'
//	ELSE
//		dw_mm_inv.Object.ovrcd.TabSequence='0'
//	END IF

// RTB Modification, If the production stage is ZM set the book media to DB
  	lmed = Trim(dw_Mm_bk_cntr.object.mchar_Med[1])
  	IF data = 'ZM' AND lmed = 'RTB' THEN 
		this.object.bkmed[row] = 'DB'
	ELSEIF lmed = 'RTB' AND data <> 'ZM' THEN
		this.object.bkmed[row] = 'RC'
	ELSE
		this.object.bkmed[row] = dw_Mm_bk_cntr.object.prod_Bkmed[1]
  END IF

	// 10/10/2008
	 ll_rc = this.rowcount()
		 ls_bkmed_new     = this.object.bkmed[row]
		 ls_cntr_new          = this.object.cntr[row]
		 ls_prodstage_new  = data
		 ll_bkseq_new	       = this.object.bkseq[row]

	 FOR li_loop = 1 TO ll_rc
		 ls_bkmed     = this.object.bkmed[li_loop]
		 ls_cntr          = this.object.cntr[li_loop]
		 ls_prodstage = this.object.prodstage[li_loop]
		 ll_bkseq	       = this.object.bkseq[li_loop]
		
		IF li_loop <> row AND ( ls_bkmed = ls_bkmed_new) AND (ll_bkseq = ll_bkseq_new) AND (ls_cntr = ls_cntr_new) AND ( ls_prodstage = ls_prodstage_new) THEN
			// duplicate
			//	Invalid Production stage, or you are duplicating this production stage
			RETURN 1
		END IF
		
	 NEXT
  
END IF


end event

event pfc_addrow;long	ll_rc,Lgray
string Lmed
Date tday

tday = Today()

Lgray = RGB(192,192,192)

// Insert row
if IsValid (inv_rowmanager) then
	ll_rc = inv_rowmanager.event pfc_addrow ()
else
	ll_rc = this.InsertRow (0) 
end if

// Notify the Linkage Service that a new row has been added.
IF IsValid ( inv_Linkage ) THEN 
	inv_Linkage.Event pfc_InsertRow (ll_rc) 
END IF 

Lmed = dw_mm_bk_cntr.object.mchar_med[1]

dw_mm_inv.Object.cntr[ll_rc]  = dw_mm_bk_cntr.object.prod_cntr[1]
IF Lmed = 'RTB' THEN
	dw_mm_inv.Object.bkmed[ll_rc] = 'RC'
ELSE
	dw_mm_inv.Object.bkmed[ll_rc] = dw_mm_bk_cntr.object.prod_bkmed[1]
END IF
dw_mm_inv.Object.bkseq[ll_rc] = dw_mm_bk_cntr.object.prod_bkseq[1]
dw_mm_inv.Object.pcsdt[ll_rc] = dw_mm_inv.object.pcsdt[1]
dw_mm_inv.Object.admdt[ll_rc] = dw_mm_inv.object.admdt[1]
dw_mm_inv.object.CREATED_DATE[ll_rc] = tday
dw_mm_inv.object.CREATED_BY[ll_rc] = gnv_app.of_GetUserId()
dw_mm_inv.object.MODIFIED_DATE[ll_rc] = tday
dw_mm_inv.object.MODIFIED_BY[ll_rc] = gnv_app.of_GetUserId()
//dw_mm_inv.Modify("invamt.Protect='1~tIf(IsRowNew(),0,1)'")
//dw_mm_inv.Modify("invamt.Background.Color='0~tIf(IsRowNew(),16777215,Lgray)'")
//dw_mm_inv.Object.prodstage.TabSequence='10'
//dw_mm_inv.Object.ovrcd.TabSequence='60'
dw_mm_inv.SetRow(ll_rc)
dw_mm_inv.SetColumn(4)

return ll_rc
end event

event type integer pfc_deleterow();integer	li_rc, i,rtn,CurRow
real Ltot,totinv
string Linv,Lcntr

CurRow = dw_mm_inv.GetRow()
IF (dw_mm_inv.IsSelected(CurRow)) THEN
		
	Linv = dw_mm_inv.object.invno[CurRow]
	Lcntr = dw_mm_inv.object.cntr[CurRow]
	
	rtn = Messagebox("Deleting Invoice Record(s)","Do you want to delete invoice number "+"~'"+Linv+"~'"+ &
			"from contract "+"~'"+Lcntr+"~'"+"?",Question!,YESNO!,1)
	If rtn= 1 Then
		if IsValid (inv_rowmanager) then
			li_rc = inv_rowmanager.event pfc_deleterow () 
		else	
			li_rc = this.DeleteRow (0) 
		end if
	
		// Notify the Linkage Service 
		IF IsValid ( inv_Linkage ) THEN 
			If li_rc > 0 Then inv_Linkage.Event pfc_DeleteRow (0) 
		END IF 
	
		FOR i=1 TO dw_mm_inv.RowCount()
			// Calculate the total invoice amount.
			IF IsNull(dw_mm_inv.object.invamt[i]) THEN
				Ltot = 0 
			ELSE
				Ltot = dw_mm_inv.object.invamt[i]
			END IF
			totinv = totinv + Ltot
		NEXT
		em_totinv.text = string(totinv,"$###,###.00")
		cb_update.Enabled = TRUE
		
	   MessageBox("Information","You must update before your changes take place.", Information!)
		
		return li_rc
	END IF
	return 1
ELSE
	MessageBox("Error","Please select a row before trying to delete it. Double click on the row for selecting the row.", StopSign!)
	IF dw_mm_inv.IsSelected(CurRow) THEN
		dw_mm_inv.Selectrow(CurRow,FALSE)
	END IF
	return 1
END IF
end event

event sqlpreview;call super::sqlpreview;//MessageBox("Sql",sqlsyntax)

end event

event doubleclicked;call super::doubleclicked;this.SelectRow(0,FALSE)
this.SelectRow(row,TRUE)
this.SetRow(row)




end event

type em_totinv from uo_conno within w_sheet_mm_bk_new_invoicing
event pfc_hinttext pbm_mousemove
string tag = "Total invoice amount"
integer x = 2039
integer y = 1556
integer width = 270
integer height = 76
integer taborder = 0
boolean enabled = false
string text = "00"
boolean displayonly = true
maskdatatype maskdatatype = decimalmask!
string mask = "###,###.00"
string displaydata = " "
end type

event pfc_hinttext;call super::pfc_hinttext;w_pics_main.setmicrohelp(em_totinv.tag)

end event

type cb_ovrd from commandbutton within w_sheet_mm_bk_new_invoicing
integer x = 32
integer y = 1556
integer width = 722
integer height = 108
boolean bringtotop = true
integer textsize = -10
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "Add/Update Overrides..."
end type

event clicked;open(w_add_override)
end event

type cbx_abs from checkbox within w_sheet_mm_bk_new_invoicing
integer x = 2341
integer y = 1552
integer width = 503
integer height = 76
boolean bringtotop = true
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long backcolor = 78164112
string text = "Abstruse Mastering"
boolean lefttext = true
end type

event clicked;int ll_rows,no_rows,i,j
string Lcntr,Lcntrmed
double Lunitcost
no_rows = dw_mm_bkinv.RowCount()	
ll_rows = dw_mm_inv.RowCount()	

Lcntr = dw_qa_cntr.object.cntr[1]
select cntrmed into :Lcntrmed from ancntr where cntr = :Lcntr using sqlservertrans;

IF cbx_abs.Checked = TRUE THEN
	// If rows does exist in invoice datawindow
	IF ll_rows > 0 THEN
		FOR i=1 TO ll_rows
			IF dw_mm_inv.object.prodstage[i]='MA' THEN
				dw_mm_inv.object.prodstage[i]='AB'
			END IF
		NEXT			
		FOR j=1 TO no_rows
			IF dw_mm_bkinv.object.prod_prodstage[j]='MA' THEN
				// assign production stage of AB instead of MA.
				dw_mm_bkinv.object.prod_prodstage[j]='AB'
				// get the unitcost for abstruse mastering and assign it.
				select unitcost into :Lunitcost
				from anucost
				where cntr = :Lcntr
				and cntrmed = :Lcntrmed
				and prodstage = 'AB'
				using sqlservertrans;
				IF IsNull(Lunitcost)=FALSE THEN
					dw_mm_bkinv.object.anucost_unitcost[j]=Lunitcost
					// recalculate the sum.
					//dw_mm_bkinv.Object.invtot.Expression='sum(invcost)'
				END IF							
			END IF
		NEXT
		dw_mm_inv.SetFocus()
	END IF
ELSE
	// If rows does not exist in invoice datawindow
	IF ll_rows > 0 THEN
		FOR i=1 TO ll_rows
			IF dw_mm_inv.object.prodstage[i]='AB' THEN
				dw_mm_inv.object.prodstage[i]='MA'
			END IF
		NEXT			
		FOR j=1 TO no_rows
			IF dw_mm_bkinv.object.prod_prodstage[j]='AB' THEN
				// assign production stage of AB instead of MA.
				dw_mm_bkinv.object.prod_prodstage[j]='MA'
				// get the unitcost for abstruse mastering and assign it.
				select unitcost into :Lunitcost
				from anucost
				where cntr = :Lcntr
				and cntrmed = :Lcntrmed
				and prodstage = 'MA'
				using sqlservertrans;
				IF IsNull(Lunitcost)=FALSE THEN
					dw_mm_bkinv.object.anucost_unitcost[j]=Lunitcost
					// recalculate the sum.
					//dw_mm_bkinv.Object.invtot.Expression='sum(invcost)'
				END IF							
			END IF
		NEXT
		dw_mm_inv.SetFocus()
	END IF
END IF
end event

type dw_mm_bk_cntr from u_pics_dw within w_sheet_mm_bk_new_invoicing
event ue_postconstructor ( )
integer x = 23
integer y = 16
integer width = 2839
integer height = 132
integer taborder = 0
string dataobject = "d_mm_bk_cntr"
boolean resizable = true
boolean livescroll = false
boolean ib_isupdateable = false
boolean ib_rmbmenu = false
boolean ib_rmbfocuschange = false
string is_updatesallowed = ""
end type

event ue_postconstructor;integer i,j,new_row,rtn,no_rows
long inv_rows,ll_rows,Lbkno,Lbkseq
string Lbkmed,Lcntr,Lstage,ls_cntr,Lcntrtype,Lcntrmed,pstage,Lbookmed
real totinv,Ltot,ll_inv_paid,ll_cntrdol
boolean found_error=FALSE
datetime ldt_admin
Date tday

tday = Today()

// Store the book number in a instance variable if the w_mm_selection is active.
IF IsValid(w_mm_selection_list) THEN
	Lbkno = w_mm_selection_list.Lbkno
END IF

IF IsNull(Lbkno)=FALSE THEN
	Lbkseq = Lbkno
	// Retrieve the contract numbers assigned to this book number
	dw_qa_cntr.of_SetTransObject(sqlservertrans) 
	rtn = dw_qa_cntr.Retrieve(Lbkseq)
	IF rtn > 0 THEN
		ls_cntr = dw_qa_cntr.object.cntr[1]
		
		// If contract number exist 
		select cntrtype,cntrmed 
		into :Lcntrtype,:Lcntrmed 
		from ancntr 
		where cntr = :ls_cntr 
		and TRIM(cntrmed) <> 'FD'
		using sqlservertrans;
		IF f_check_dberror(sqlservertrans,"ANCNTR")=FALSE THEN RETURN
		IF rtn > 1 THEN
			// If more than one contract assigned to this book number
			// display the contract selection window
			// If Lcntrtype is T, that means this contract is a Master & Duplication contract,
			// and there are more than one contract number for this contract type.
			OpenWithParm(w_qa_select_contract, Lbkseq)
			IF IsNull(Message.StringParm)=FALSE AND Message.StringParm<>"" THEN
				ls_cntr = Message.StringParm 
				select cntrtype,cntrmed 
				into :Lcntrtype,:Lcntrmed 
				from ancntr 
				where cntr = :ls_cntr 
				and TRIM(cntrmed) <> 'FD'
				using sqlservertrans;
				IF f_check_dberror(sqlservertrans,"ANCNTR")=FALSE THEN RETURN
			ELSE
				cb_clear.event clicked()
				RETURN
			END IF
		END IF
		// First populate the production stages in middle screen
		DataWindowChild ldwc_prodstage
		dw_mm_bkinv.GetChild ("prod_prodstage", ldwc_prodstage)
		ldwc_prodstage.SetTransObject(sqlservertrans)
		ldwc_prodstage.Retrieve(Lcntrtype,Lcntrmed)		
	END IF
END IF
// Set the transaction object
dw_mm_bk_cntr.of_SetTransObject(SQLServerTrans)

// Retreive the data
inv_rows = dw_mm_bk_cntr.Retrieve(Lbkno,ls_cntr)
// If nothing was retrieved?
IF inv_rows < 1 THEN 
 	MessageBox("ERROR", string(Lbkno) + " is not a valid Book Number.",StopSign!, OK!, 2)
   close(w_sheet_mm_bk_new_invoicing)
	IF IsValid(w_mm_selection_list) THEN
		w_mm_selection_list.show()
		w_mm_selection_list.em_bkno.text=""
		w_mm_selection_list.em_bkno.SetFocus()
	ELSE
		open(w_mm_selection_list)	
		w_mm_selection_list.em_bkno.SetFocus()
	END IF
ELSE
	FOR i = 1 to inv_rows
		pstage = dw_mm_bk_cntr.object.prod_prodstage[i]
		IF (pstage='MA' OR pstage='AB' OR pstage='PU') THEN
			IF (IsNull(dw_mm_bk_cntr.object.anucost_unitcost[i]) AND Lcntrmed<>'P/B') THEN
				MessageBox("Warning","Unit Cost is missing for production stage "+pstage+" of book number: "+string(lbkno)+"~nInvoicing needs unitcost, before completion.",Information!)
				found_error = TRUE				
			ELSEIF (IsNull(dw_mm_bk_cntr.object.prod_units[i]) AND Lcntrmed<>'P/B') THEN
				MessageBox("Warning","Units are missing for production stage "+pstage+" of book number: "+string(lbkno)+"~nInvoicing needs units, before completion.",Information!)
				found_error = TRUE				
			ELSEIF (IsNull(dw_mm_bk_cntr.object.prod_units[i]) AND Lcntrmed='P/B') THEN
				MessageBox("Warning","Units are missing for production stage "+pstage+" of book number: "+string(lbkno)+"~nProduction information is not completed.~nThis Print/Braille book needs to be received by QAS in later times.",Information!)
			END IF
		ELSE
			IF (IsNull(dw_mm_bk_cntr.object.prod_units[i]) OR &
				 IsNull(dw_mm_bk_cntr.object.prod_subunits[i])) THEN
				f_calc_unit_subunit(Lbkseq,Lcntrmed,ls_cntr,Lcntrtype)
			END IF
		END IF
	NEXT
	IF found_error THEN 
		cb_clear.event clicked()
		RETURN
	END IF
	// Disable the updating, untill the invoice amount is changed.
	cb_update.Enabled = FALSE
	// Hide the selection box if is not been hidden already.
	IF IsValid(w_mm_selection_list) THEN
		w_mm_selection_list.hide()
	END IF
	// Set the transaction object
	dw_mm_bkinv.of_SetTransObject(SQLServerTrans)
	// Retreive the data
	no_rows = dw_mm_bkinv.Retrieve(Lbkno,ls_cntr)
	IF no_rows = 0 THEN
		MessageBox("ERROR", "No data found.",StopSign!, OK!, 2)
		close(parent)
	ELSE
		dw_mm_inv.of_SetTransObject(SQLServerTrans)
		Lcntr = dw_mm_bk_cntr.object.prod_cntr[1]
		//Lbkmed = dw_mm_bk_cntr.object.prod_bkmed[1]
		// Retrieve the data from invoice table
		ll_rows = dw_mm_inv.Retrieve(Lbkno,Lcntr)
		
		// Check for abstruse mastering
		IF ll_rows > 0 THEN
			FOR i=1 TO ll_rows
				IF dw_mm_inv.object.prodstage[i]='AB' THEN
					cbx_abs.Checked = TRUE
					FOR j=1 TO no_rows
						IF dw_mm_bkinv.object.prod_prodstage[j]='MA' THEN
							// assign production stage of AB instead of MA.
							dw_mm_bkinv.object.prod_prodstage[j]='AB'
							double Lunitcost
							// get the unitcost for abstruse mastering and assign it.
							select unitcost into :Lunitcost
							from anucost
							where cntr = :Lcntr
							and cntrmed = :Lcntrmed
							and prodstage = 'AB'
							using sqlservertrans;
							IF IsNull(Lunitcost)=FALSE THEN
								dw_mm_bkinv.object.anucost_unitcost[j]=Lunitcost
								// recalculate the sum.
								//dw_mm_bkinv.Object.invtot.Expression='sum(invcost)'
							END IF							
						END IF
					NEXT
				END IF
			NEXT
		END IF
		
		// If there are invoices, but there are not of type
		// cassettes. calculate the total invoice amount and
		// then set focus on the datawindow
		IF ll_rows > 0 THEN
			// Hide the entry for total invoice amount
			em_totinvamt.Visible = FALSE
			st_2.Visible = FALSE
			
			em_totinv.text = string(dw_mm_inv.object.tot_invamt[1],"$###,###.00")
			// Disable the total invoice from modifying.
//			Ladmdt =	dw_mm_inv.object.admdt[1]
			ldt_admin =	dw_mm_inv.object.admdt[1]
			ladmdt=ldt_admin
			em_totinv.Enabled = FALSE
			dw_mm_inv.SetFocus()
		// Else if there are invoice and they are of type cassettes, make sure that the
		// production stages matches between anucost and invoice table. If it does not
		// insert new row into the datwindows with the correct production stages.
		// Finally if there are no records in the invoice table, insert new rows in
		// the datawindow for entering those invoices.
		
		ELSEIF ll_rows = 0 THEN
			// Show the entry for total invoice amount
			em_totinvamt.Visible = TRUE
			st_2.Visible = TRUE
			
			// If invoices dose not exist.
			dw_mm_inv.InsertRow(0)
			// set the contract number, book no and book media.
			dw_mm_inv.object.cntr[1] = Lcntr
			dw_mm_inv.object.bkseq[1] = Lbkno
			IF Lcntrmed = 'RTB' THEN
				dw_mm_inv.object.bkmed[1] = 'RC'
			ELSE
				dw_mm_inv.object.bkmed[1] = dw_mm_bk_cntr.object.prod_bkmed[1]
			END IF
			// set the production stages
			CHOOSE CASE Lcntrmed
					CASE 'RC'
						dw_mm_inv.object.prodstage[1] = 'MA'
						cbx_emb.Visible = FALSE
					CASE 'BR'
						dw_mm_inv.object.prodstage[1] = 'MA'
						cbx_emb.Visible = TRUE
					CASE 'P/B'
						dw_mm_inv.object.prodstage[1] = 'PU'
						cbx_emb.Visible = FALSE
					CASE ELSE
						dw_mm_inv.object.prodstage[1] = 'MA'
						cbx_emb.Visible = FALSE
			END CHOOSE
			// If admin date is NULL, set it to today's date
			IF IsNull(w_mm_selection_list.Ld_admindate) THEN
				dw_mm_inv.object.admdt[1] = datetime(today(),time('00:00:00'))
			ELSE
				// Else set it to its value.
				Ladmdt = datetime(w_mm_selection_list.Ld_admindate,time('00:00:00:'))
				dw_mm_inv.object.admdt[1] = Ladmdt
			END IF
			
			// Set the PCS date to today's date as default.
			dw_mm_inv.object.invamt[1] = 0
			dw_mm_inv.object.pcsdt[1] = datetime(today(),time('00:00:00'))
			// Get the producation stage
			Lstage = dw_mm_bk_cntr.object.prod_prodstage[1]
			// Set the Created Date,Create by, Modified Date and Modified By  (Problem report 2024)
			dw_mm_inv.object.CREATED_DATE[1] = tday
			dw_mm_inv.object.CREATED_BY[1] = gnv_app.of_GetUserId()
			dw_mm_inv.object.MODIFIED_DATE[1] = tday
			dw_mm_inv.object.MODIFIED_BY[1] = gnv_app.of_GetUserId()
			// Set the focus on invoice datawindow.	
			dw_mm_inv.Object.invamt.Background.Color=rgb(255,255,255)
			dw_mm_inv.setfocus()
			dw_mm_inv.Setcolumn(6)
		END IF // No invoices exist
		
		SELECT cntrdol INTO :ll_cntrdol FROM ANCNTR
		WHERE cntr = :Lcntr
		USING SQLServerTrans;
		em_cntramt.text = string(ll_cntrdol,"$###,###.00")


		SELECT SUM(invamt) INTO :ll_inv_paid FROM INV
		WHERE cntr = :Lcntr
		USING SQLServerTrans;
		em_invamt.text = string(ll_inv_paid,"$###,###.00")		

	END IF // Sharing the datawindows
END IF // If no book exist
end event

event rbuttondown;//
end event

event rbuttonup;//
end event

event itemchanged;call super::itemchanged;integer i,j,new_row,rtn,no_rows
long inv_rows,ll_rows,Lbkno,Lbkseq
string Lcntr,Lstage,ls_cntr,Lcntrtype,Lcntrmed,pstage
real totinv,Ltot
boolean found_error
datetime ldt_admin

em_totinv.text = ".00"

dw_mm_bk_cntr.AcceptText()

Lbkno = long(dw_mm_bk_cntr.Gettext())

IF IsNull(Lbkno)=FALSE THEN
	Lbkseq = Lbkno
	// Retrieve the contract numbers assigned to this book number
	dw_qa_cntr.of_SetTransObject(sqlservertrans) 
	rtn = dw_qa_cntr.Retrieve(Lbkseq)
	IF rtn > 0 THEN
		ls_cntr = dw_qa_cntr.object.cntr[1]
		// If contract number exist 
		select cntrtype,cntrmed 
		into :Lcntrtype,:Lcntrmed 
		from ancntr 
		where cntr = :ls_cntr 
		and TRIM(cntrmed) <> 'FD'
		using sqlservertrans;
		IF f_check_dberror(sqlservertrans,"ANCNTR")=FALSE THEN RETURN
		IF (rtn > 1) THEN
			// If more than one contract assigned to this book number
			// display the contract selection window
			// If Lcntrtype is T, that means this contract is a Master & Duplication contract,
			// and there are more than one contract number for this contract type.
			OpenWithParm(w_qa_select_contract, Lbkseq)
			IF IsNull(Message.StringParm)=FALSE AND Message.StringParm<>"" THEN
				ls_cntr = Message.StringParm 
				select cntrtype,cntrmed 
				into :Lcntrtype,:Lcntrmed 
				from ancntr 
				where cntr = :ls_cntr 
				and TRIM(cntrmed) <> 'FD'
				using sqlservertrans;
				IF f_check_dberror(sqlservertrans,"ANCNTR")=FALSE THEN RETURN
			ELSE
				cb_clear.event clicked()
				RETURN
			END IF
		END IF
	END IF
END IF
// Set the transaction object
dw_mm_bk_cntr.of_SetTransObject(SQLServerTrans)

// Retreive the data
inv_rows = dw_mm_bk_cntr.Retrieve(Lbkno,ls_cntr)
// If nothing was retrieved?
IF inv_rows < 1 THEN 
 	MessageBox("ERROR", string(Lbkno) + " is not a valid Book Number.",StopSign!, OK!, 2)
   close(w_sheet_mm_bk_new_invoicing)
	IF IsValid(w_mm_selection_list) THEN
		w_mm_selection_list.show()
		w_mm_selection_list.em_bkno.text=""
		w_mm_selection_list.em_bkno.SetFocus()
	ELSE
		open(w_mm_selection_list)	
		w_mm_selection_list.em_bkno.SetFocus()
	END IF
ELSE
	FOR i = 1 to inv_rows
		pstage = dw_mm_bk_cntr.object.prod_prodstage[i]
		IF (pstage='MA' OR pstage='AB' OR pstage='PU') THEN
			IF (IsNull(dw_mm_bk_cntr.object.anucost_unitcost[i]) AND Lcntrmed<>'P/B') THEN
				MessageBox("Warning","Unit Cost is missing for production stage "+pstage+" of book number: "+string(lbkno)+"~nInvoicing needs unitcost, before completion.",Information!)
				found_error = TRUE				
			ELSEIF (IsNull(dw_mm_bk_cntr.object.prod_units[i]) AND Lcntrmed<>'P/B') THEN
				MessageBox("Warning","Units are missing for production stage "+pstage+" of book number: "+string(lbkno)+"~nInvoicing needs units, before completion.",Information!)
				found_error = TRUE				
			ELSEIF (IsNull(dw_mm_bk_cntr.object.prod_units[i]) AND Lcntrmed='P/B') THEN
				MessageBox("Warning","Units are missing for production stage "+pstage+" of book number: "+string(lbkno)+"~nProduction information is not completed.~nThis Print/Braille book needs to be received by QAS in later times.",Information!)
			END IF
		ELSE
			IF (IsNull(dw_mm_bk_cntr.object.prod_units[i]) OR &
				 IsNull(dw_mm_bk_cntr.object.prod_subunits[i])) THEN
				f_calc_unit_subunit(Lbkseq,Lcntrmed,ls_cntr,Lcntrtype)
			END IF
		END IF
	NEXT
	IF found_error THEN 
		cb_clear.event clicked()
		RETURN
	END IF
	// Disable the updating, untill the invoice amount is changed.
	cb_update.Enabled = FALSE
	// Hide the selection box if is not been hidden already.
	IF IsValid(w_mm_selection_list) THEN
		w_mm_selection_list.hide()
	END IF
	// Set the transaction object
	dw_mm_bkinv.of_SetTransObject(SQLServerTrans)
	// Retreive the data
	no_rows = dw_mm_bkinv.Retrieve(Lbkno,ls_cntr)
	IF no_rows = 0 THEN
		MessageBox("ERROR", "No data found.",StopSign!, OK!, 2)
		close(parent)
	ELSE
		dw_mm_inv.of_SetTransObject(SQLServerTrans)
		Lcntr = dw_mm_bk_cntr.object.prod_cntr[1]
		//Lbkmed = dw_mm_bk_cntr.object.prod_bkmed[1]
		// Retrieve the data from invoice table
		ll_rows = dw_mm_inv.Retrieve(Lbkno,Lcntr)
		
		// Check for abstruse mastering
		IF ll_rows > 0 THEN
			FOR i=1 TO ll_rows
				IF dw_mm_inv.object.prodstage[i]='AB' THEN
					cbx_abs.Checked = TRUE
					FOR j=1 TO no_rows
						IF dw_mm_bkinv.object.prod_prodstage[j]='MA' THEN
							// assign production stage of AB instead of MA.
							dw_mm_bkinv.object.prod_prodstage[j]='AB'
							double Lunitcost
							// get the unitcost for abstruse mastering and assign it.
							select unitcost into :Lunitcost
							from anucost
							where cntr = :Lcntr
							and cntrmed = :Lcntrmed
							and prodstage = 'AB'
							using sqlservertrans;
							IF IsNull(Lunitcost)=FALSE THEN
								dw_mm_bkinv.object.anucost_unitcost[j]=Lunitcost
								// recalculate the sum.
								//dw_mm_bkinv.Object.invtot.Expression='sum(invcost)'
							END IF							
						END IF
					NEXT
				END IF
			NEXT
		END IF
		
		// If there are invoices, but there are not of type
		// cassettes. calculate the total invoice amount and
		// then set focus on the datawindow
		IF ll_rows > 0 THEN
			// Hide the entry for total invoice amount
			em_totinvamt.Visible = FALSE
			st_2.Visible = FALSE
			
			em_totinv.text = string(dw_mm_inv.object.tot_invamt[1],"$###,###.00")
			// Disable the total invoice from modifying.
			ldt_admin =	dw_mm_inv.object.admdt[1]
			Ladmdt =ldt_admin
			em_totinv.Enabled = FALSE
			dw_mm_inv.SetFocus()
		// Else if there are invoice and they are of type cassettes, make sure that the
		// production stages matches between anucost and invoice table. If it does not
		// insert new row into the datwindows with the correct production stages.
		// Finally if there are no records in the invoice table, insert new rows in
		// the datawindow for entering those invoices.
		ELSEIF ll_rows = 0 THEN
			// Show the entry for total invoice amount
			em_totinvamt.Visible = TRUE
			st_2.Visible = TRUE
			
			// If invoices dose not exist.
			dw_mm_inv.InsertRow(0)
			// set the contract number, book no and book media.
			dw_mm_inv.object.cntr[1] = Lcntr
			dw_mm_inv.object.bkseq[1] = Lbkno
			dw_mm_inv.object.bkmed[1] = this.object.prod_bkmed[1]
			dw_mm_inv.object.CREATED_DATE[1] = datetime(today(),time('00:00:00'))
			dw_mm_inv.object.CREATED_BY[1] = gnv_app.of_GetUserId()
			dw_mm_inv.object.MODIFIED_DATE[1] = datetime(today(),time('00:00:00'))
			dw_mm_inv.object.MODIFIED_BY[1] = gnv_app.of_GetUserId()
			// set the production stages
			CHOOSE CASE Lcntrmed
					CASE 'RC'
						dw_mm_inv.object.prodstage[1] = 'MA'
						cbx_emb.Visible = FALSE
					CASE 'BR'
						dw_mm_inv.object.prodstage[1] = 'MA'
						cbx_emb.Visible = TRUE
					CASE 'P/B'
						dw_mm_inv.object.prodstage[1] = 'PU'
						cbx_emb.Visible = FALSE
					CASE ELSE
						dw_mm_inv.object.prodstage[1] = 'MA'
						cbx_emb.Visible = FALSE
			END CHOOSE
			// If admin date is NULL, set it to today's date
			IF IsNull(w_mm_selection_list.Ld_admindate) THEN
				dw_mm_inv.object.admdt[1] = datetime(today(),time('00:00:00'))
			ELSE
				// Else set it to its value.
				Ladmdt = datetime(w_mm_selection_list.Ld_admindate,time('00:00:00'))
				dw_mm_inv.object.admdt[1] = Ladmdt
			END IF
				// Set the PCS date to today's date as default.
			dw_mm_inv.object.invamt[1] = 0
			dw_mm_inv.object.pcsdt[1] = datetime(today(),time('00:00:00'))
			// Get the producation stage
			Lstage = dw_mm_bk_cntr.object.prod_prodstage[1]
			// Set the focus on invoice datawindow.	
			dw_mm_inv.Object.invamt.Background.Color=rgb(255,255,255)
			dw_mm_inv.setfocus()
			dw_mm_inv.Setcolumn(6)
		END IF // No invoices exist
	END IF // Sharing the datawindows
END IF // If no book exist
end event

event pfc_addrow;//
RETURN -1
end event

event pfc_deleterow;//
RETURN -1
end event

event retrieveend;call super::retrieveend;Integer i,rtn

FOR i = 1 TO rowcount
	IF ( 	this.object.prod_prodstage[i]='DU' OR &
			this.object.prod_prodstage[i]='PR' OR &
			this.object.prod_prodstage[i]='PB') THEN
		
		IF IsNull(this.object.prod_actenddt[i]) THEN
			rtn = MessageBox("ERROR","Invoicing duplication is not allowed until the book is shipped.~r~nActenddt field must have a value for duplication stage. Please refer to producer data exchange screens on the web.~r~n~nDo you want to continue?",StopSign!,YesNo!,1)			
			IF rtn = 2 THEN
				exit
				cb_clear.TriggerEvent(Clicked!)
			END IF				
		END IF
	ELSEIF( 	this.object.prod_prodstage[i]='MA' OR &
				this.object.prod_prodstage[i]='AB') THEN
				
			IF IsNull(this.object.prod_actenddt[i]) THEN
				rtn = MessageBox("ERROR","Invoicing narration is not allowed until the book is shipped to QAS.~r~nActenddt field must have a value for narration stage. Please refer to producer data exchange screens on the web.~r~n~n Do you want to continue?",StopSign!,YesNo!,1)
			END IF
			IF rtn = 2 THEN
				exit
				cb_clear.TriggerEvent(Clicked!)
			END IF				
	END IF
NEXT
				
				
end event

type dw_qa_cntr from u_pics_dw within w_sheet_mm_bk_new_invoicing
event itemchanged pbm_dwnitemchange
event rbuttondown pbm_dwnrbuttondown
event rbuttonup pbm_dwnrbuttonup
event type integer pfc_deleterow ( )
event type long pfc_insertrow ( )
event type long pfc_addrow ( )
event ue_postconstructor ( )
boolean visible = false
integer x = 2327
integer y = 44
integer width = 389
integer height = 96
integer taborder = 10
string dataobject = "dddw_cntr"
boolean vscrollbar = false
boolean livescroll = false
boolean ib_isupdateable = false
boolean ib_rmbmenu = false
boolean ib_rmbfocuschange = false
string is_updatesallowed = ""
end type

event rbuttondown;//
end event

event rbuttonup;//
end event

event pfc_deleterow;//
RETURN -1
end event

event pfc_insertrow;//
RETURN -1
end event

event pfc_addrow;//
RETURN -1
end event

event sqlpreview;call super::sqlpreview;//MessageBox("Sql",sqlsyntax)

end event

type dw_mm_bkinv from u_pics_dw within w_sheet_mm_bk_new_invoicing
integer x = 23
integer y = 140
integer width = 2839
integer height = 932
integer taborder = 0
boolean bringtotop = true
string dataobject = "d_mm_book_invoicing"
boolean resizable = true
boolean livescroll = false
boolean ib_isupdateable = false
boolean ib_rmbmenu = false
boolean ib_rmbfocuschange = false
string is_updatesallowed = ""
end type

event rbuttondown;//
end event

event rbuttonup;//
end event

event pfc_addrow;//
RETURN -1
end event

event pfc_deleterow;//
RETURN -1
end event

event sqlpreview;call super::sqlpreview;//MessageBox("Sql",sqlsyntax)

end event

event itemchanged;call super::itemchanged;string ls_stage,ls_bkmed, ls_name
long li_bkseq, li_cl1, li_cl2,li_cl3,li_cl4,li_cl5,i

ls_name=dwo.name

IF dwo.name = 'incl1' OR   &
	dwo.name = 'incl2' OR &
	dwo.name = 'incl3' OR &
	dwo.name = 'incl4' or dwo.name='incl5' THEN
  ls_stage=this.object.prod_prodstage[row]
  ls_bkmed=this.object.prod_bkmed[row]
  li_bkseq=this.object.prod_bkseq[row]

  
  li_cl5=this.object.incl5[row]
  li_cl4=this.object.incl4[row]
  li_cl3=this.object.incl3[row]
  li_cl2=this.object.incl2[row]
  li_cl1=this.object.incl1[row]
  i=100
END IF
end event


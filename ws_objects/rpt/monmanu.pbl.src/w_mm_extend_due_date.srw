$PBExportHeader$w_mm_extend_due_date.srw
forward
global type w_mm_extend_due_date from w_sheet
end type
type cb_ext from commandbutton within w_mm_extend_due_date
end type
type dw_auto_ext from u_pics_dw within w_mm_extend_due_date
end type
type ddlb_ssdflag from u_ddlb within w_mm_extend_due_date
end type
type st_4 from statictext within w_mm_extend_due_date
end type
type dw_mm_select_bkno from u_pics_dw within w_mm_extend_due_date
end type
type cb_exit from commandbutton within w_mm_extend_due_date
end type
type dw_manu_ext from u_pics_dw within w_mm_extend_due_date
end type
type st_1 from statictext within w_mm_extend_due_date
end type
type em_bkno from uo_conno within w_mm_extend_due_date
end type
type cb_update from commandbutton within w_mm_extend_due_date
end type
type cb_clear from commandbutton within w_mm_extend_due_date
end type
type st_2 from statictext within w_mm_extend_due_date
end type
type dw_cntr from u_pics_dw within w_mm_extend_due_date
end type
type dw_manu_prod_info from u_pics_dw within w_mm_extend_due_date
end type
type cb_qa from commandbutton within w_mm_extend_due_date
end type
type cb_find from commandbutton within w_mm_extend_due_date
end type
end forward

global type w_mm_extend_due_date from w_sheet
integer x = 27
integer y = 40
integer width = 2839
integer height = 1828
string title = "Extend Due Date"
cb_ext cb_ext
dw_auto_ext dw_auto_ext
ddlb_ssdflag ddlb_ssdflag
st_4 st_4
dw_mm_select_bkno dw_mm_select_bkno
cb_exit cb_exit
dw_manu_ext dw_manu_ext
st_1 st_1
em_bkno em_bkno
cb_update cb_update
cb_clear cb_clear
st_2 st_2
dw_cntr dw_cntr
dw_manu_prod_info dw_manu_prod_info
cb_qa cb_qa
cb_find cb_find
end type
global w_mm_extend_due_date w_mm_extend_due_date

type variables
string book_status
end variables

forward prototypes
public subroutine wf_insert_auto_extdt (long lbkseq, string ls_cntr, string lbkmed)
public subroutine wf_get_latest_schenddt ()
end prototypes

public subroutine wf_insert_auto_extdt (long lbkseq, string ls_cntr, string lbkmed);long ll_rows, li_cur,li_estmt, li_auto_ext,sum_ext
		string Lbkno,Lssdflag,Ltemp, ls_cntrmed,ls_cvcd, ls_prty, ls_type
		integer li_return_code, li_null_value, rtn, rtncntr,i
		dec  lr_aplen, lr_df
		date ld_orig, ld_enddt, ld_stdt, ld_null
		
		SetNull(ld_null)
		select sum(ext) into :sum_ext
		from ext
		where bkseq=:lbkseq and bkmed=:lbkmed and cntr=:ls_cntr
		using SqlServerTrans;
		if not f_check_dberror(SqlServerTrans,'select from ext to find sum_ext') then
			return 
		end if 
		select max(schenddt),min(schstdt) into :ld_enddt, :ld_stdt
		from prod
		where bkseq=:lbkseq and bkmed=:lbkmed and cntr=:ls_cntr
		using SqlServerTrans;
		if not f_check_dberror(SqlServerTrans,'select from prod to find schenddt') then
			return 
		end if
		select cntrmed, cntrtype,cntrcvcd into :ls_cntrmed, :ls_type, :ls_cvcd
		from ancntr
		where cntr= :ls_cntr
		using SqlServerTrans;
		if not f_check_dberror(SqlServerTrans,'select from ancntr to find cntrmed') then
			return 
		end if
		select priority, applen, df into :ls_prty, :lr_aplen, :lr_df
		from mchar
		where bkseq=:lbkseq and bkmed=:lbkmed and (arflag is null or arflag='')
		using SqlServerTrans;
		if not f_check_dberror(SqlServerTrans,'select from mchar to find df e.t.c') then
			return 
		else
			if ls_type='D' then
				li_estmt=f_calculate_estpt_dupl(ls_prty)
			else
				li_estmt=f_calculate_estpt(ls_cntrmed,ls_prty,lr_aplen,ls_cvcd, lr_df)
			end if
			ld_orig=RelativeDate( ld_stdt, li_estmt)
		end if
		if IsNull(sum_ext) then sum_ext=0
		li_auto_ext=DaysAfter( ld_orig, ld_enddt) - sum_ext
		li_cur=dw_auto_ext.InsertRow(0)
		dw_auto_ext.SetItem(li_cur,'ext',li_auto_ext)
		dw_auto_ext.SetItem(li_cur,'extrsn','Automatically extended')
		dw_auto_ext.ResetUpdate()
		
end subroutine

public subroutine wf_get_latest_schenddt ();string Lcntr,Lbkmed 
long Lbkseq
Lcntr	= dw_cntr.GetText()
Lbkseq = long(em_bkno.text)
Lbkmed = dw_mm_select_bkno.object.prod_bkmed[1]
dw_manu_prod_info.Retrieve(Lbkseq,Lcntr,Lbkmed)

end subroutine

on w_mm_extend_due_date.create
int iCurrent
call super::create
this.cb_ext=create cb_ext
this.dw_auto_ext=create dw_auto_ext
this.ddlb_ssdflag=create ddlb_ssdflag
this.st_4=create st_4
this.dw_mm_select_bkno=create dw_mm_select_bkno
this.cb_exit=create cb_exit
this.dw_manu_ext=create dw_manu_ext
this.st_1=create st_1
this.em_bkno=create em_bkno
this.cb_update=create cb_update
this.cb_clear=create cb_clear
this.st_2=create st_2
this.dw_cntr=create dw_cntr
this.dw_manu_prod_info=create dw_manu_prod_info
this.cb_qa=create cb_qa
this.cb_find=create cb_find
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.cb_ext
this.Control[iCurrent+2]=this.dw_auto_ext
this.Control[iCurrent+3]=this.ddlb_ssdflag
this.Control[iCurrent+4]=this.st_4
this.Control[iCurrent+5]=this.dw_mm_select_bkno
this.Control[iCurrent+6]=this.cb_exit
this.Control[iCurrent+7]=this.dw_manu_ext
this.Control[iCurrent+8]=this.st_1
this.Control[iCurrent+9]=this.em_bkno
this.Control[iCurrent+10]=this.cb_update
this.Control[iCurrent+11]=this.cb_clear
this.Control[iCurrent+12]=this.st_2
this.Control[iCurrent+13]=this.dw_cntr
this.Control[iCurrent+14]=this.dw_manu_prod_info
this.Control[iCurrent+15]=this.cb_qa
this.Control[iCurrent+16]=this.cb_find
end on

on w_mm_extend_due_date.destroy
call super::destroy
destroy(this.cb_ext)
destroy(this.dw_auto_ext)
destroy(this.ddlb_ssdflag)
destroy(this.st_4)
destroy(this.dw_mm_select_bkno)
destroy(this.cb_exit)
destroy(this.dw_manu_ext)
destroy(this.st_1)
destroy(this.em_bkno)
destroy(this.cb_update)
destroy(this.cb_clear)
destroy(this.st_2)
destroy(this.dw_cntr)
destroy(this.dw_manu_prod_info)
destroy(this.cb_qa)
destroy(this.cb_find)
end on

event open;call super::open;// Open the sheet in Maximized mode
this.windowstate = maximized!
// set focus on cntr.
cb_clear.Enabled=FALSE
cb_update.Enabled=FALSE
cb_qa.Enabled=FALSE
m_pics_main.m_edit.m_deleterow.Enabled 	=	TRUE
m_pics_main.m_edit.m_addrow.Enabled 		=	TRUE
em_bkno.SetFocus()

end event

event pfc_preopen();call super::pfc_preopen;this.of_SetPreference(TRUE)
this.inv_preference.of_SetToolBars(TRUE)
this.inv_preference.of_SetWindow(TRUE)
this.of_SetResize(TRUE)
this.inv_resize.of_SetOrigSize(this.workspacewidth(),this.workspaceheight())
inv_resize.of_Register(dw_cntr, "Scale")
inv_resize.of_Register(dw_manu_ext, "Scale")
inv_resize.of_Register(dw_manu_prod_info, "Scale")
inv_resize.of_Register(dw_mm_select_bkno, "Scale")
inv_resize.of_Register(dw_auto_ext, "Scale")
inv_resize.of_Register(cb_clear, "Scale")
inv_resize.of_Register(cb_exit, "Scale")
inv_resize.of_Register(cb_find, "Scale")
inv_resize.of_Register(cb_update, "Scale")
inv_resize.of_Register(cb_qa, "Scale")
inv_resize.of_Register(cb_ext, "Scale")
inv_resize.of_Register(em_bkno, "Scale")
inv_resize.of_Register(st_1, "Scale")
inv_resize.of_Register(st_2, "Scale")
inv_resize.of_Register(st_4, "Scale")
inv_resize.of_Register(ddlb_ssdflag, "Scale")



end event

event resize;call super::resize;//long ll_height
//
//This.X = w_pics_main.X
//This.Y = w_pics_main.Y
//ll_height = w_pics_main.mdi_1.Height
//This.Resize(w_pics_main.WorkSpaceWidth(), ll_height)
//
end event

event closequery;Integer	li_pendingrc
Integer	li_validationrc
Integer	li_accepttextrc
Integer	li_msg
Integer	li_rc,rtn
String	ls_msgparms[]

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
			rtn = cb_update.Event Clicked()
			if rtn = 1 THEN
				RETURN 0
			end if
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

type cb_ext from commandbutton within w_mm_extend_due_date
integer x = 1033
integer y = 1444
integer width = 489
integer height = 92
integer taborder = 71
integer textsize = -8
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "Extension &Reasons"
end type

event clicked;DataWindowChild ldwc_extrsn

open(w_pa_add_ext_rsn)

// Reretrieve the extension reason datawindow
w_mm_extend_due_date.dw_manu_ext.GetChild ("ext_extrsn", ldwc_extrsn)
ldwc_extrsn.SetTransObject(sqlservertrans)
ldwc_extrsn.Retrieve()

end event

type dw_auto_ext from u_pics_dw within w_mm_extend_due_date
event pfc_dddw pbm_dwndropdown
integer x = 1632
integer y = 760
integer width = 1143
integer height = 332
integer taborder = 10
string dataobject = "d_auto_ext"
boolean vscrollbar = false
boolean livescroll = false
boolean ib_isupdateable = false
boolean ib_rmbmenu = false
boolean ib_rmbfocuschange = false
string is_updatesallowed = ""
end type

event pfc_dddw;call super::pfc_dddw;Datawindowchild ldwc

IF THIS.GetColumnName() = 'cntr' THEN
	IF THIS.GetChild('cntr',ldwc) = 1 THEN
		IF ldwc.RowCount() = 1 THEN RETURN 1
	END IF
END IF
end event

event itemchanged;call super::itemchanged;int rtn,i
string ls_cntr,Lbkmed,Lssdflag,Ltemp
long Lbkseq

ls_cntr	= data
lbkseq = long(em_bkno.text)
ddlb_ssdflag.text=""
dw_manu_ext.Reset()
dw_manu_ext.InsertRow(0)
dw_mm_select_bkno.Reset()
dw_mm_select_bkno.InsertRow(0)
dw_manu_prod_info.Reset()
dw_manu_prod_info.InsertRow(0)

select bkmed into :Lbkmed from prod where cntr=:data and bkseq=:lbkseq using sqlservertrans;
rtn = dw_mm_select_bkno.Retrieve(Lbkseq,Ls_cntr,Lbkmed)
if rtn > 0 THEN
	rtn = dw_manu_prod_info.Retrieve(Lbkseq,Ls_cntr,Lbkmed)
	FOR i=1 to rtn
		Ltemp = dw_manu_prod_info.object.ssdflag[i]
		IF IsNull(Ltemp)=FALSE AND Ltemp<>"" THEN
			Lssdflag=Ltemp
		END IF
	NEXT
	CHOOSE CASE Lssdflag
			CASE "I"
				ddlb_ssdflag.text = "Active"
				m_pics_main.m_edit.m_deleterow.Enabled 	=	TRUE
				m_pics_main.m_edit.m_addrow.Enabled 		=	TRUE
				dw_manu_ext.Enabled = TRUE
			CASE "H"
				ddlb_ssdflag.text = "Hold Status"
				m_pics_main.m_edit.m_deleterow.Enabled 	=	FALSE
				m_pics_main.m_edit.m_addrow.Enabled 		=	FALSE
				dw_manu_ext.Enabled = FALSE
				MessageBox("Warning","This book is on hold. Until the ssdflag is not changed, you may not modify the extension table.",Information!)
			CASE "F"
				ddlb_ssdflag.text = "Final"
				m_pics_main.m_edit.m_deleterow.Enabled 	=	TRUE
				m_pics_main.m_edit.m_addrow.Enabled 		=	TRUE
				dw_manu_ext.Enabled = TRUE
			CASE ELSE
				ddlb_ssdflag.text = "Active"
				m_pics_main.m_edit.m_deleterow.Enabled 	=	TRUE
				m_pics_main.m_edit.m_addrow.Enabled 		=	TRUE
				dw_manu_ext.Enabled = TRUE
	END CHOOSE					
				
	rtn = dw_manu_ext.Retrieve(Lbkseq,Ls_cntr,Lbkmed)
	if rtn > 0 THEN
		cb_clear.Enabled=TRUE
		cb_update.Enabled=TRUE
		cb_qa.Enabled=TRUE
		cb_find.Enabled=FALSE
		em_bkno.Enabled=FALSE
		dw_manu_ext.SetFocus()
	else
		cb_clear.Enabled=TRUE
		cb_update.Enabled=TRUE
		cb_qa.Enabled=TRUE
		cb_find.Enabled=FALSE
		em_bkno.Enabled=FALSE
		dw_manu_ext.Event pfc_addrow()
		dw_manu_ext.SetFocus()
	end if
end if
end event

event rbuttondown;//
end event

event retrievestart;call super::retrievestart;this.TriggerEvent("pfc_populatedddw")

end event

event rbuttonup;//
end event

event pfc_deleterow;//
RETURN -1
end event

event pfc_addrow;//
RETURN -1
end event

event pfc_populatedddw;call super::pfc_populatedddw;Datawindowchild ldwc
long lbkseq,ll_rows

lbkseq = long(em_bkno.text)

this.GetChild('cntr',ldwc)

IF ldwc.SetTransObject(SQLServerTrans) = -1 THEN
	Return -1
ELSE
   ll_rows = ldwc.Retrieve(lbkseq)
	IF ll_rows= 0 THEN
		MessageBox("ERROR","There are no books found in prod table, which matches book number: "+string(lbkseq),StopSign!)
		return -1 
	ELSE
		return ll_rows
	END IF
END IF
end event

event ue_postconstructor;call super::ue_postconstructor;dw_cntr.of_SetTransObject(sqlservertrans) 

end event

type ddlb_ssdflag from u_ddlb within w_mm_extend_due_date
integer x = 2149
integer y = 20
integer width = 443
integer height = 388
integer taborder = 50
string item[] = {"Final","Hold Status","Active"}
end type

event selectionchanged;call super::selectionchanged;int i,rtn
long Lbkseq
string Lssdflag

dw_manu_prod_info.AcceptText()

IF ddlb_ssdflag.text = "Hold Status" THEN
	dwItemStatus l_status
	int LastRow
	IF book_status="H" THEN
		MessageBox("ERROR","This book is already on hold status.")
	ELSE
		l_status = dw_manu_ext.GetItemStatus(LastRow,0, Primary!)
		IF (l_status = NewModified!) THEN
			LastRow = dw_manu_ext.RowCount()
			dw_manu_ext.object.ext_ext[LastRow]=0
		ELSE
			dw_manu_ext.Event pfc_addrow()
			LastRow = dw_manu_ext.RowCount()
			dw_manu_ext.object.ext_ext[LastRow]=0
			dw_manu_ext.Setfocus()
			dw_manu_ext.SetColumn("ext_extrsn")
		END IF
		FOR i=1 to dw_manu_prod_info.RowCount()
			dw_manu_prod_info.object.ssdflag[i] = "H"
		NEXT
		book_status = "H"
		m_pics_main.m_edit.m_deleterow.Enabled 	=	FALSE
		m_pics_main.m_edit.m_addrow.Enabled 		=	FALSE
	END IF
elseif ddlb_ssdflag.text = "Active" THEN
	FOR i=1 to dw_manu_prod_info.RowCount()
		dw_manu_prod_info.object.ssdflag[i] = "I"
	NEXT
	dw_manu_ext.Enabled = TRUE
	m_pics_main.m_edit.m_deleterow.Enabled 	=	TRUE
	m_pics_main.m_edit.m_addrow.Enabled 		=	TRUE
	IF book_status="H" THEN
		Lbkseq = dw_manu_prod_info.object.bkseq[1]
		select ssdflag into :Lssdflag
		from prod
		where bkseq = :Lbkseq
		and prodstage in ("DU","PR")
		using sqlservertrans;
		IF Lssdflag = "H" THEN
			rtn = MessageBox("Warning","You must update ssdflag in prod table before adding an extension to this book. Update Now?",Question!,YesNo!,1)
			IF rtn = 1 THEN
				cb_update.TriggerEvent(Clicked!)
			END IF
		END IF
	END IF
	book_status = ""
elseif ddlb_ssdflag.text = "Final" THEN
	FOR i=1 to dw_manu_prod_info.RowCount()
		dw_manu_prod_info.object.ssdflag[i] = "F"
	NEXT
	dw_manu_ext.Enabled = TRUE
	m_pics_main.m_edit.m_deleterow.Enabled 	=	TRUE
	m_pics_main.m_edit.m_addrow.Enabled 		=	TRUE
	IF book_status="H" THEN
		Lbkseq = dw_manu_prod_info.object.bkseq[1]
		select ssdflag into :Lssdflag
		from prod
		where bkseq = :Lbkseq
		and prodstage in ("DU","PR")
		using sqlservertrans;
		IF Lssdflag = "H" THEN
			rtn = MessageBox("Warning","You must update ssdflag in prod table before adding an extension to this book. Update Now?",Question!,YesNo!,1)
			IF rtn = 1 THEN
				cb_update.TriggerEvent(Clicked!)
			END IF
		END IF
	END IF
	book_status = ""
end if
end event

type st_4 from statictext within w_mm_extend_due_date
integer x = 1925
integer y = 20
integer width = 210
integer height = 76
boolean bringtotop = true
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long backcolor = 67108864
boolean enabled = false
string text = "SSDFlag"
boolean focusrectangle = false
end type

type dw_mm_select_bkno from u_pics_dw within w_mm_extend_due_date
integer x = 14
integer y = 140
integer width = 2761
integer height = 144
integer taborder = 50
string dataobject = "d_mm_select_bkno"
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

event ue_postconstructor;call super::ue_postconstructor;dw_mm_select_bkno.of_SetTransObject(sqlservertrans) 

end event

event pfc_deleterow;//
RETURN -1
end event

event pfc_addrow;//
RETURN -1
end event

event sqlpreview;call super::sqlpreview;//MessageBox("SQL",sqlsyntax)
end event

event retrievestart;call super::retrievestart;openwithparm(w_pics_retrieve_msg_box,"Retrieving Title Information, Please wait...")

end event

event retrieveend;call super::retrieveend;close(w_pics_retrieve_msg_box)

end event

type cb_exit from commandbutton within w_mm_extend_due_date
integer x = 2528
integer y = 1444
integer width = 247
integer height = 92
boolean bringtotop = true
integer textsize = -10
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "E&xit"
end type

event clicked;parent.Event pfc_close()

end event

type dw_manu_ext from u_pics_dw within w_mm_extend_due_date
event ue_enterkey pbm_dwnprocessenter
integer x = 14
integer y = 760
integer width = 1609
integer height = 644
integer taborder = 60
string dataobject = "d_manu_ext"
boolean vscrollbar = false
boolean livescroll = false
boolean ib_isupdateable = false
boolean ib_rmbmenu = false
boolean ib_rmbfocuschange = false
string is_updatesallowed = ""
end type

event ue_enterkey;call super::ue_enterkey;Send(Handle(this),256,9,Long(0,0))
return(1)
end event

event pfc_addrow;long	ll_rc,Lbkseq
int Lprev_row, li_null
date Lschenddt,Ldate
string ls_cntr,Lbkmed

SetNull(li_null)
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
// set the extention date to today's date
dw_manu_ext.Object.ext_extdt[ll_rc] = today()
// Assign the book number, contract number
dw_manu_ext.object.ext_bkseq[ll_rc] = long(em_bkno.text)
dw_manu_ext.object.ext_bkmed[ll_rc] = dw_mm_select_bkno.object.prod_bkmed[1]
dw_manu_ext.object.ext_cntr[ll_rc]  = dw_cntr.GetText()
dw_manu_ext.object.ext_ext[ll_rc]  = li_null
// Get the schedule end date from prod datawindow
Lschenddt = date(dw_manu_prod_info.object.max_schenddt[1])
// Get the previouse row number, if not the first row.
Lprev_row = ll_rc - 1
IF Lprev_row < 1 THEN
	Lprev_row = 1
END IF
// get the date of extpcdt from previouse row
Ldate = date(dw_manu_ext.object.ext_extpcdt[Lprev_row])
// If schenddt greater than exppcdt or the Ldate is NULL then extpcdt equal to schenddt
IF (Lschenddt > Ldate OR IsNull(Ldate)=TRUE) THEN
	dw_manu_ext.object.ext_extpcdt[ll_rc] = Lschenddt
ELSE
	dw_manu_ext.object.ext_extpcdt[ll_rc] = Ldate
END IF

// Make extpcdt, extrsn and extdt required fields
dw_manu_ext.Object.ext_extpcdt.Editmask.Required='Yes'
//dw_manu_ext.Object.ext_extrsn.Edit.Required='Yes'
dw_manu_ext.Object.ext_extdt.Editmask.Required='Yes'

dw_manu_ext.SetRow(ll_rc)
dw_manu_ext.SetColumn(1)
return ll_rc
end event

event itemchanged;call super::itemchanged;int i
Long Lbkseq,li_ext,li_sum_ext=0, li_count,li_row_count, li_auto_ext,li_old_ext,li_net_ext
string ls_cntr,Lbkmed, ls_date
date ld_schenddt, ld_origenddt, ld_schenddtmax=date('01/01/1900'), ld_date, ld_max_schenddt

IF dwo.name="ext_ext" THEN
	IF data = "" THEN
		RETURN 2
	ELSE
		li_old_ext=dw_manu_ext.object.ext_ext[row]
		if not IsNull(li_old_ext) then
			li_net_ext= long(data) - li_old_ext
		else
			li_net_ext= long(data)
		end if
		// First assign the maximum date from production into the extpcdt.

		// Second, add the extension days to that date to obtain the new extpcdt.
		dw_manu_ext.object.ext_extpcdt[row] = &
		RelativeDate(date(dw_manu_ext.object.ext_extpcdt[row]),li_net_ext)
		// Third, increment the schendt date for all the production stages
		// based on the ext. (Only if the date is not NULL).
		dw_manu_ext.object.ext_bkmed[row] = dw_mm_select_bkno.object.prod_bkmed[1]
		FOR i=1 to dw_manu_prod_info.RowCount()
			IF IsNull(dw_manu_prod_info.object.schenddt[i])=FALSE THEN
				dw_manu_prod_info.object.schenddt[i] =	&
				RelativeDate(date(dw_manu_prod_info.object.schenddt[i]),li_net_ext)
			END IF				
		NEXT
	END IF
ELSEIF dwo.name="ext_extpcdt" THEN
	IF data = "" THEN
		RETURN 2
	ELSE
		long ldays,ldaysold, ldaysnew
		ls_date=left(data,10)
		ld_date=date(ls_date)
		ld_max_schenddt = date(dw_manu_prod_info.object.max_schenddt[1])
		ldays = DaysAfter(ld_max_schenddt,ld_date)
		ldaysold=dw_manu_ext.object.ext_ext[row]
		if IsNull(ldaysold) then ldaysold=0
		ldaysnew= ldaysold + ldays
		dw_manu_ext.object.ext_ext[row] = ldaysnew
//		wf_get_latest_schenddt()
		FOR i=1 to dw_manu_prod_info.RowCount()
			IF IsNull(dw_manu_prod_info.object.schenddt[i])=FALSE THEN
				ld_date=RelativeDate(date(dw_manu_prod_info.object.schenddt[i]), ldays)
				dw_manu_prod_info.object.schenddt[i] = datetime(ld_date,time('00:00:00'))
			END IF				
		NEXT
	END IF
ELSEIF dwo.name="ext_extrsn" THEN
	IF data = "" THEN
		RETURN 2
	END IF
END IF
IF (dwo.name="ext_ext"  or dwo.name= "ext_extpcdt" ) THEN
	li_count=dw_manu_prod_info.RowCount()
	li_row_count=dw_manu_ext.RowCount()
	if dwo.name='ext_ext' then
		dw_manu_ext.object.ext_ext[row]=integer(data)
	end if
	for i=1 to li_row_count
		li_ext=dw_manu_ext.object.ext_ext[i]
		if Isnull(li_ext) then li_ext=0
		li_sum_ext +=li_ext
	next
	for i=1 to li_count
		IF IsNull(dw_manu_prod_info.object.schenddt[i])=FALSE THEN
			ld_schenddt= date(dw_manu_prod_info.object.schenddt[i])
			if ld_schenddt > ld_schenddtmax then
				ld_schenddtmax=ld_schenddt
			end if
		END IF
	next
	for i=1 to li_count
		IF IsNull(dw_manu_prod_info.object.original_ssd[i])=FALSE THEN
			ld_origenddt=date( dw_manu_prod_info.object.original_ssd[i])
		END IF
	next
	li_auto_ext=DaysAfter(ld_origenddt,ld_schenddtmax) - li_sum_ext
	dw_auto_ext.SetItem(1,'ext',li_auto_ext)
	
end if
end event

event ue_postconstructor;call super::ue_postconstructor;dw_manu_ext.of_SetTransObject(sqlservertrans) 

end event

event pfc_deleterow;date Lextpcdt,Lschenddt
datetime lextpcdt_dt, lschenddt_dt
integer	li_rc,rtn,LastRow,i,Lext
string ls_cntr,Lbkmed
long Lbkseq

dwItemStatus l_status

LastRow = dw_manu_ext.RowCount()
l_status = dw_manu_ext.GetItemStatus(LastRow,0, Primary!)
IF (l_status <> NewModified! AND l_status <> New!) THEN
	rtn = MessageBox("Warning","You are about to remove the last Extension record. ~nAre you sure?",Question!, YesNoCancel!, 1)
	IF rtn = 1 THEN 
		Lextpcdt_dt = dw_manu_ext.object.ext_extpcdt[LastRow]
		lextpcdt=date(lextpcdt_dt)
		IF Lextpcdt <> date(dw_manu_ext.object.max_extpcdt[1]) or &
			IsNull(Lextpcdt) THEN
			MessageBox("ERROR","This extension can not be deleted. ~nPlease make sure that you select the latest extension date",Information!)
			RETURN -1
		ELSE
			Lext = integer(dw_manu_ext.object.ext_ext[LastRow])
			Lext = - Lext
			FOR i=1 to dw_manu_prod_info.RowCount()
				IF IsNull(dw_manu_prod_info.object.schenddt[i])=FALSE THEN
					Lschenddt = RelativeDate(date(dw_manu_prod_info.object.schenddt[i]),Lext)
					lschenddt_dt=datetime(lschenddt,time('00:00:00'))
					dw_manu_prod_info.object.schenddt[i] = Lschenddt_dt
				END IF
			NEXT
			if IsValid (inv_rowmanager) then
				li_rc = inv_rowmanager.event pfc_deleterow () 
			else	
				li_rc = this.DeleteRow (LastRow) 
			end if
			// Notify the Linkage Service 
			IF IsValid ( inv_Linkage ) THEN 
				If li_rc > 0 Then inv_Linkage.Event pfc_DeleteRow (LastRow) 
			END IF 
			cb_update.TriggerEvent(Clicked!)
		END IF
	END IF
ELSE
	dw_manu_ext.accepttext()
	Lext = integer(dw_manu_ext.object.ext_ext[LastRow])
	if IsNull(Lext) then Lext=0
	Lext = - Lext
	FOR i=1 to dw_manu_prod_info.RowCount()
		IF IsNull(dw_manu_prod_info.object.schenddt[i])=FALSE THEN
			Lschenddt = RelativeDate(date(dw_manu_prod_info.object.schenddt[i]),Lext)
			lschenddt_dt=datetime(lschenddt,time('00:00:00'))
			dw_manu_prod_info.object.schenddt[i] = Lschenddt_dt
		END IF
	NEXT
	if IsValid (inv_rowmanager) then
		li_rc = inv_rowmanager.event pfc_deleterow () 
	else	
		li_rc = this.DeleteRow (LastRow) 
	end if
	// Notify the Linkage Service 
	IF IsValid ( inv_Linkage ) THEN 
		If li_rc > 0 Then inv_Linkage.Event pfc_DeleteRow (LastRow) 
	END IF 
//	wf_get_latest_schenddt()
END IF
return li_rc
end event

event pfc_insertrow;//
return -1
end event

event sqlpreview;call super::sqlpreview;//MessageBox("SQL",sqlsyntax)
end event

event retrievestart;call super::retrievestart;openwithparm(w_pics_retrieve_msg_box,"Retrieving Extension Information, Please wait...")

end event

event retrieveend;call super::retrieveend;close(w_pics_retrieve_msg_box)

end event

type st_1 from statictext within w_mm_extend_due_date
integer x = 123
integer y = 20
integer width = 219
integer height = 76
boolean bringtotop = true
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long backcolor = 79741120
boolean enabled = false
string text = "Book No"
boolean focusrectangle = false
end type

type em_bkno from uo_conno within w_mm_extend_due_date
event ue_enterkey pbm_dwnprocessenter
string tag = "Enter Book Number"
integer x = 352
integer y = 20
integer width = 293
integer height = 104
integer taborder = 30
long textcolor = 255
end type

event modified;call super::modified;//cb_find.TriggerEvent(Clicked!)
end event

type cb_update from commandbutton within w_mm_extend_due_date
event clicked pbm_bnclicked
integer x = 1897
integer y = 1444
integer width = 311
integer height = 92
boolean bringtotop = true
integer textsize = -10
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "&Update"
end type

event clicked;Int rtn, li_count, i, li_ext
Long Lbkseq
Date Lschenddt
String lcntrtype, Lprodstage, lbkmed, Lcntr, lstage

Lbkseq = Long(em_bkno.text)

SetMicroHelp(w_pics_main,"Updating Records Please Wait...")
li_count= dw_manu_ext.RowCount()
FOR i=1 TO li_count
	li_ext=dw_manu_ext.object.ext_ext[i]
	IF IsNull(li_ext) OR li_ext=0 THEN
		dw_manu_ext.DeleteRow(i)
		li_count =li_count -1
	END IF
NEXT
// Check for any pending updates
IF of_UpdateChecks() < 0 THEN RETURN -1


// These few statements are added to update oracle database with
// the value of schenddt.
FOR i=1 TO dw_manu_prod_info.RowCount()
	
	SetMicroHelp(w_pics_main,"Updating Web database record Please Wait...")
	
	lcntrtype = Trim(dw_manu_prod_info.object.ancntr_cntrtype[i])
	Lprodstage = Trim(dw_manu_prod_info.object.prodstage[i])
	lbkmed = Trim(dw_manu_prod_info.object.bkmed[i])
	Lcntr = Trim(dw_manu_prod_info.object.cntr[i])
	
	IF  lcntrtype = 'T' OR lcntrtype = 'D' THEN
		IF Lprodstage = 'DU' OR Lprodstage = 'PR' OR Lprodstage = 'EM' OR Lprodstage = 'PB' THEN
			Lschenddt = Date(dw_manu_prod_info.object.schenddt[i])
			lstage = dw_manu_prod_info.object.prodstage[i]
		END IF
	ELSEIF lcntrtype = 'M' THEN
		IF Lprodstage = 'MA' THEN
			Lschenddt = Date(dw_manu_prod_info.object.schenddt[i])
			lstage = dw_manu_prod_info.object.prodstage[i]
		END IF
	ELSE
		Messagebox("error","schedule end date is null")
		RETURN -1
	END IF
NEXT

//MessageBox("schenddt","value of schedule = "+string(lschenddt)+" prodstage = "+lstage+" bkseq = "+string(lbkseq)+ " bkmed = "+lbkmed+" cntr = "+lcntr)

UPDATE prdrprod@pic_link
set schenddt = :Lschenddt, web_upd = NULL
where bkseq = :Lbkseq
AND Trim(bkmed) = :lbkmed
AND Trim(cntr) = :Lcntr
AND Trim(prodstage) = :lstage
USING SqlServerTrans;
IF f_check_dberror(SqlServerTrans,"Updating PRDRPROD with new extensions")=FALSE THEN
	ROLLBACK USING SqlServerTrans;
	SetMicroHelp(w_pics_main,"Ready")
	RETURN -1
ELSE
	SetMicroHelp(w_pics_main,"Updating Records Please Wait...")
	// Update the Extension datawindow
	rtn = dw_manu_ext.EVENT pfc_update(TRUE,TRUE)
	IF rtn = 1 THEN
		// If update successful, update the production information.
		// Remember: Only schedule end date is updated in prod table.
		rtn = dw_manu_prod_info.EVENT pfc_update(TRUE,TRUE)
		IF rtn = 1 THEN
			//Mark the mchar record
			f_update_mchar_time("",Lbkseq,"B","U")
			COMMIT USING SqlServerTrans;
			wf_get_latest_schenddt()
			SetMicroHelp(w_pics_main,"Ready")
			RETURN 1
		ELSE
			ROLLBACK USING SqlServerTrans;
			SetMicroHelp(w_pics_main,"Update prod table failed.")
			Messagebox("Update","Update prod table failed.",stopSign!)
			RETURN -1
		END IF			
	ELSE
		ROLLBACK USING SqlServerTrans;
		SetMicroHelp(w_pics_main,"Update ext table failed.")
		Messagebox("Update","Update ext table failed.",stopSign!)
		RETURN -1
	END IF
END IF



end event

type cb_clear from commandbutton within w_mm_extend_due_date
event clicked pbm_bnclicked
integer x = 2235
integer y = 1444
integer width = 251
integer height = 92
boolean bringtotop = true
integer textsize = -10
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "&Clear"
end type

event clicked;long ll_rows,rtn

dw_manu_ext.AcceptText()

IF dw_manu_ext.ModifiedCount( ) > 0 THEN
	rtn=MessageBox("Clear","Save the changes before clearing the screen?",Question!,OkCancel!,2)
	IF rtn = 1 THEN
		w_mm_extend_due_date.cb_update.TriggerEvent(Clicked!)
	END IF
END IF
ib_disableclosequery=TRUE
em_bkno.text=""
ddlb_ssdflag.text=""
book_status=""
dw_cntr.Reset()
dw_cntr.InsertRow(0)
dw_manu_ext.Reset()
dw_manu_ext.InsertRow(0)
dw_mm_select_bkno.Reset()
dw_mm_select_bkno.InsertRow(0)
dw_manu_prod_info.Reset()
dw_manu_prod_info.InsertRow(0)
dw_auto_ext.Reset()
m_pics_main.m_edit.m_deleterow.Enabled 	=	TRUE
m_pics_main.m_edit.m_addrow.Enabled 		=	TRUE

cb_find.Enabled=TRUE
cb_find.Default=TRUE
cb_update.Enabled=FALSE
cb_clear.Enabled=FALSE
em_bkno.Enabled=TRUE

em_bkno.SetFocus()


end event

type st_2 from statictext within w_mm_extend_due_date
integer x = 1006
integer y = 20
integer width = 293
integer height = 76
boolean bringtotop = true
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long backcolor = 79741120
boolean enabled = false
string text = "Contract No"
boolean focusrectangle = false
end type

type dw_cntr from u_pics_dw within w_mm_extend_due_date
event pfc_dddw pbm_dwndropdown
integer x = 1298
integer y = 20
integer width = 379
integer height = 104
integer taborder = 40
string dataobject = "d_cntr"
boolean vscrollbar = false
boolean livescroll = false
boolean ib_isupdateable = false
boolean ib_rmbmenu = false
boolean ib_rmbfocuschange = false
string is_updatesallowed = ""
end type

event pfc_dddw;call super::pfc_dddw;Datawindowchild ldwc

IF THIS.GetColumnName() = 'cntr' THEN
	IF THIS.GetChild('cntr',ldwc) = 1 THEN
		IF ldwc.RowCount() = 1 THEN RETURN 1
	END IF
END IF
end event

event rbuttondown;//
end event

event rbuttonup;//
end event

event ue_postconstructor;call super::ue_postconstructor;dw_cntr.of_SetTransObject(sqlservertrans) 

end event

event pfc_deleterow;//
RETURN -1
end event

event pfc_addrow;//
RETURN -1
end event

event itemchanged;call super::itemchanged;int rtn,i
string ls_cntr,Lbkmed,Lssdflag,Ltemp
long Lbkseq

ls_cntr	= data
lbkseq = long(em_bkno.text)
ddlb_ssdflag.text=""
dw_manu_ext.Reset()
dw_manu_ext.InsertRow(0)
dw_mm_select_bkno.Reset()
dw_mm_select_bkno.InsertRow(0)
dw_manu_prod_info.Reset()
dw_manu_prod_info.InsertRow(0)

select bkmed into :Lbkmed from prod where cntr=:data and bkseq=:lbkseq using sqlservertrans;
rtn = dw_mm_select_bkno.Retrieve(Lbkseq,Ls_cntr,Lbkmed)
if rtn > 0 THEN
	rtn = dw_manu_prod_info.Retrieve(Lbkseq,Ls_cntr,Lbkmed)
	FOR i=1 to rtn
		Ltemp = dw_manu_prod_info.object.ssdflag[i]
		IF IsNull(Ltemp)=FALSE AND Ltemp<>"" THEN
			Lssdflag=Ltemp
		END IF
	NEXT
	CHOOSE CASE Lssdflag
			CASE "I"
				ddlb_ssdflag.text = "Active"
				m_pics_main.m_edit.m_deleterow.Enabled 	=	TRUE
				m_pics_main.m_edit.m_addrow.Enabled 		=	TRUE
				dw_manu_ext.Enabled = TRUE
			CASE "H"
				ddlb_ssdflag.text = "Hold Status"
				m_pics_main.m_edit.m_deleterow.Enabled 	=	FALSE
				m_pics_main.m_edit.m_addrow.Enabled 		=	FALSE
				dw_manu_ext.Enabled = FALSE
				MessageBox("Warning","This book is on hold. Until the ssdflag is not changed, you may not modify the extension table.",Information!)
			CASE "F"
				ddlb_ssdflag.text = "Final"
				m_pics_main.m_edit.m_deleterow.Enabled 	=	TRUE
				m_pics_main.m_edit.m_addrow.Enabled 		=	TRUE
				dw_manu_ext.Enabled = TRUE
			CASE ELSE
				ddlb_ssdflag.text = "Active"
				m_pics_main.m_edit.m_deleterow.Enabled 	=	TRUE
				m_pics_main.m_edit.m_addrow.Enabled 		=	TRUE
				dw_manu_ext.Enabled = TRUE
	END CHOOSE					
				
	rtn = dw_manu_ext.Retrieve(Lbkseq,Ls_cntr,Lbkmed)
	if rtn > 0 THEN
		cb_clear.Enabled=TRUE
		cb_update.Enabled=TRUE
		cb_qa.Enabled=TRUE
		cb_find.Enabled=FALSE
		em_bkno.Enabled=FALSE
		dw_manu_ext.SetFocus()
	else
		cb_clear.Enabled=TRUE
		cb_update.Enabled=TRUE
		cb_qa.Enabled=TRUE
		cb_find.Enabled=FALSE
		em_bkno.Enabled=FALSE
		dw_manu_ext.Event pfc_addrow()
		dw_manu_ext.SetFocus()
	end if
end if
end event

event pfc_populatedddw;call super::pfc_populatedddw;Datawindowchild ldwc
long lbkseq,ll_rows

lbkseq = long(em_bkno.text)

this.GetChild('cntr',ldwc)

IF ldwc.SetTransObject(SQLServerTrans) = -1 THEN
	Return -1
ELSE
   ll_rows = ldwc.Retrieve(lbkseq)
	IF ll_rows= 0 THEN
		MessageBox("ERROR","There are no books found in prod table, which matches book number: "+string(lbkseq),StopSign!)
		return -1 
	ELSE
		return ll_rows
	END IF
END IF
end event

event retrievestart;call super::retrievestart;this.TriggerEvent("pfc_populatedddw")

end event

type dw_manu_prod_info from u_pics_dw within w_mm_extend_due_date
integer x = 14
integer y = 280
integer width = 2761
integer height = 480
integer taborder = 20
string dataobject = "d_manu_prod_info"
boolean vscrollbar = false
boolean livescroll = false
boolean ib_isupdateable = false
boolean ib_rmbmenu = false
boolean ib_rmbfocuschange = false
string is_updatesallowed = ""
end type

event ue_postconstructor;call super::ue_postconstructor;dw_manu_prod_info.of_SetTransObject(sqlservertrans) 

end event

event pfc_addrow;//
RETURN -1
end event

event pfc_deleterow;//
RETURN -1
end event

event rbuttondown;//

end event

event rbuttonup;//

end event

event sqlpreview;call super::sqlpreview;//MessageBox("SQL",sqlsyntax)
end event

type cb_qa from commandbutton within w_mm_extend_due_date
integer x = 27
integer y = 1444
integer width = 901
integer height = 92
boolean bringtotop = true
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "&Quality Assurance Stages(View only)..."
end type

event clicked;open(w_mm_qastg_info)
end event

type cb_find from commandbutton within w_mm_extend_due_date
event clicked pbm_bnclicked
integer x = 1618
integer y = 1444
integer width = 247
integer height = 92
integer taborder = 61
boolean bringtotop = true
integer textsize = -10
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "F&ind"
boolean default = true
end type

event clicked;DatawindowChild ldwc_cntr
long ll_rows,Lbkseq, li_cur,li_ext, li_auto_ext,sum_ext
string Lbkno,ls_cntr,Lbkmed,Lssdflag,Ltemp, ls_cntrmed,ls_cvcd, ls_prty
integer li_return_code, li_null_value, rtn, rtncntr,i
dec  lr_aplen, lr_df
date ld_orig, ld_enddt, ld_stdt
Lbkno = em_bkno.text
IF Lbkno <> "" THEN
	// If the book number is not NULL
	SetNull(li_null_value)
	Lbkseq = long(Lbkno)
	// Open the retrival window
	SetMicroHelp(w_pics_main,"Retrieving Data Please Wait...")
	// retrieve the contract numbers assigned to this book number
	rtncntr = dw_cntr.Retrieve(Lbkseq)
	IF rtncntr > 0 THEN
		// If number of books assigned greater than zero
		// assign the book media from contract selection.
		Lbkmed=dw_cntr.object.bkmed[dw_cntr.GetRow()]
		IF Lbkmed='DB' THEN
			Lbkmed='RC'
		END IF
		// Populate the drop down datawindow child with the contract numbers.
		li_return_code = dw_cntr.GetChild("cntr",ldwc_cntr)
		IF li_return_code = -1 THEN
			MessageBox("Error", " Not a DataWindowChild")
			SetMicroHelp(w_pics_main,"Ready")
			RETURN
		END IF
		ldwc_cntr.SetTransObject(SQLServerTrans)
		rtn = ldwc_cntr.Retrieve(Lbkseq)
		if rtn > 0 THEN
			ls_cntr	= dw_cntr.object.cntr[1]
			// Based on book number, contract number and book media 
			// retrieve the title information
			rtn = dw_mm_select_bkno.Retrieve(Lbkseq,Ls_cntr,Lbkmed)
			if rtn > 0 THEN
				// retrieve the production information of the book
				rtn = dw_manu_prod_info.Retrieve(Lbkseq,Ls_cntr,Lbkmed)
				FOR i=1 to rtn
					Ltemp = dw_manu_prod_info.object.ssdflag[i]
					IF IsNull(Ltemp)=FALSE AND Ltemp<>"" THEN
						Lssdflag=Ltemp
					END IF
				NEXT
				CHOOSE CASE Lssdflag
					CASE "I"
						ddlb_ssdflag.text = "Active"
						m_pics_main.m_edit.m_deleterow.Enabled 	=	TRUE
						m_pics_main.m_edit.m_addrow.Enabled 		=	TRUE
						dw_manu_ext.Enabled = TRUE
					CASE "H"
						ddlb_ssdflag.text = "Hold Status"
						m_pics_main.m_edit.m_deleterow.Enabled 	=	FALSE
						m_pics_main.m_edit.m_addrow.Enabled 		=	FALSE
						dw_manu_ext.Enabled = FALSE
						book_status="H"
						MessageBox("Warning","This book is on hold. Until the ssdflag is not changed, you may not modify the extension table.",Information!)
					CASE "F"
						ddlb_ssdflag.text = "Final"
						m_pics_main.m_edit.m_deleterow.Enabled 	=	TRUE
						m_pics_main.m_edit.m_addrow.Enabled 		=	TRUE
						dw_manu_ext.Enabled = TRUE
					CASE ELSE
						ddlb_ssdflag.text = "Active"
						m_pics_main.m_edit.m_deleterow.Enabled 	=	TRUE
						m_pics_main.m_edit.m_addrow.Enabled 		=	TRUE
						dw_manu_ext.Enabled = TRUE
				END CHOOSE					
				
				// retrieve the Extension information
				rtn = dw_manu_ext.Retrieve(Lbkseq,Ls_cntr,Lbkmed)
				if rtn > 0 THEN
					cb_clear.Enabled=TRUE
					cb_update.Enabled=TRUE
					cb_qa.Enabled=TRUE
					cb_find.Enabled=FALSE
					em_bkno.Enabled=FALSE
					
				else
					cb_clear.Enabled=TRUE
					cb_update.Enabled=TRUE
					cb_qa.Enabled=TRUE
					cb_find.Enabled=FALSE
					em_bkno.Enabled=FALSE
					if ddlb_ssdflag.text <> "Hold Status" THEN
						dw_manu_ext.Event pfc_addrow()
					end if				
				end if
				wf_insert_auto_extdt(lbkseq, ls_cntr, lbkmed)
				dw_manu_ext.SetFocus()
				ib_disableclosequery=TRUE
			else
				MessageBox("ERROR","Title Information NOT found. (Title, medium and producer)")
			end if
		else
			MessageBox("Error", "This Book Number: "+Lbkno+" has not been assigned yet to any contract. ~n ~n Enter another book number." ,Information!)
			SetMicroHelp(w_pics_main,"Ready")
			em_bkno.SetFocus()
			RETURN
		end if
	else
		MessageBox("Error", "This Book Number: "+Lbkno+" has not been assigned yet to any contract. ~n ~n Enter another book number." ,Information!)
		SetMicroHelp(w_pics_main,"Ready")
		em_bkno.SetFocus()
		RETURN
	end if
SetMicroHelp(w_pics_main,"Ready")
else
	MessageBox("Error", "Please enter the Book Number." ,StopSign!, OK!, 2)
	em_bkno.SetFocus()
end if
end event


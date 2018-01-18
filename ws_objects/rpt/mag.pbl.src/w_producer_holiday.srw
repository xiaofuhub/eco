$PBExportHeader$w_producer_holiday.srw
forward
global type w_producer_holiday from w_sheet
end type
type dw_prdr from datawindow within w_producer_holiday
end type
type st_1 from statictext within w_producer_holiday
end type
type sle_prdr from u_sle within w_producer_holiday
end type
type cb_find from commandbutton within w_producer_holiday
end type
type cb_update from commandbutton within w_producer_holiday
end type
type cb_clear from commandbutton within w_producer_holiday
end type
type cb_exit from commandbutton within w_producer_holiday
end type
type dw_producer_holiday from u_pics_dw within w_producer_holiday
end type
type st_producer_holiday from statictext within w_producer_holiday
end type
type st_prdr from statictext within w_producer_holiday
end type
end forward

shared variables

end variables

global type w_producer_holiday from w_sheet
integer x = 9
integer y = 8
integer width = 2926
integer height = 2128
string title = "Producer Holidays"
dw_prdr dw_prdr
st_1 st_1
sle_prdr sle_prdr
cb_find cb_find
cb_update cb_update
cb_clear cb_clear
cb_exit cb_exit
dw_producer_holiday dw_producer_holiday
st_producer_holiday st_producer_holiday
st_prdr st_prdr
end type
global w_producer_holiday w_producer_holiday

type variables
BOOLEAN src_chng=FALSE
String Local_chno, is_data

end variables

forward prototypes
public function integer wf_valid_conno (string control_no)
public function boolean wf_val_ajyfn_casub (string ajyfn, string casub)
end prototypes

public function integer wf_valid_conno (string control_no);String L99
int length

length = len(control_no)
IF length <> 8 THEN
	return 1
END IF

L99=Mid(control_no,3,2)
IF L99 <> '99' THEN
	return 2
END IF

return 3
end function

public function boolean wf_val_ajyfn_casub (string ajyfn, string casub);ajyfn=RightTrim(ajyfn)
casub=RightTrim(casub)

IF			((Mid(casub,1,2) = "JF") and &
			(ajyfn="JF")) THEN
	RETURN TRUE
ELSEIF	((Mid(casub,1,2) = "JN") and &
			(ajyfn="JN")) THEN
	RETURN TRUE
ELSEIF	((ajyfn="AN" or ajyfn="YN") and &
			(Mid(casub,1,1) >= "0") and &
			(Mid(casub,1,1) <= "9")) THEN
	RETURN TRUE
ELSEIF	(casub="SPA" or casub="OFL") THEN
	RETURN TRUE
ELSEIF	((ajyfn="AF") and &
			(casub<>"SPA" AND casub<>"OFL")) THEN
	RETURN TRUE
ELSEIF	((ajyfn="AF" or ajyfn="YF") and &
			(Mid(casub,1,1) >= "A") and &
			(Mid(casub,1,1) < "J" or Mid(casub,1,1) > "J") and &
			(Mid(casub,1,1) <= "Z")) THEN
	RETURN TRUE
ELSE
	RETURN FAlSE
END IF
end function

on w_producer_holiday.create
int iCurrent
call super::create
this.dw_prdr=create dw_prdr
this.st_1=create st_1
this.sle_prdr=create sle_prdr
this.cb_find=create cb_find
this.cb_update=create cb_update
this.cb_clear=create cb_clear
this.cb_exit=create cb_exit
this.dw_producer_holiday=create dw_producer_holiday
this.st_producer_holiday=create st_producer_holiday
this.st_prdr=create st_prdr
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_prdr
this.Control[iCurrent+2]=this.st_1
this.Control[iCurrent+3]=this.sle_prdr
this.Control[iCurrent+4]=this.cb_find
this.Control[iCurrent+5]=this.cb_update
this.Control[iCurrent+6]=this.cb_clear
this.Control[iCurrent+7]=this.cb_exit
this.Control[iCurrent+8]=this.dw_producer_holiday
this.Control[iCurrent+9]=this.st_producer_holiday
this.Control[iCurrent+10]=this.st_prdr
end on

on w_producer_holiday.destroy
call super::destroy
destroy(this.dw_prdr)
destroy(this.st_1)
destroy(this.sle_prdr)
destroy(this.cb_find)
destroy(this.cb_update)
destroy(this.cb_clear)
destroy(this.cb_exit)
destroy(this.dw_producer_holiday)
destroy(this.st_producer_holiday)
destroy(this.st_prdr)
end on

event pfc_preopen;call super::pfc_preopen;this.of_SetPreference(TRUE)
this.inv_preference.of_SetToolBars(TRUE)
this.inv_preference.of_SetWindow(TRUE)
this.of_SetResize(TRUE)
this.inv_resize.of_SetOrigSize(this.workspacewidth(),this.workspaceheight())

inv_resize.of_Register(cb_clear, "Scale")
inv_resize.of_Register(cb_exit, "Scale")
inv_resize.of_Register(dw_prdr, "Scale")
inv_resize.of_Register(sle_prdr, "Scale")
inv_resize.of_Register(cb_find, "Scale")
inv_resize.of_Register(cb_update, "Scale")
inv_resize.of_Register(dw_producer_holiday, "Scale")
inv_resize.of_Register(st_prdr, "Scale")
inv_resize.of_Register(st_1, "Scale")
inv_resize.of_Register(st_producer_holiday, "Scale")




end event

event resize;call super::resize;long ll_height

This.X = w_pics_main.X
This.Y = w_pics_main.Y
ll_height = w_pics_main.mdi_1.Height
This.Resize(w_pics_main.WorkSpaceWidth(), ll_height)

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

event pfc_postopen;call super::pfc_postopen;// Open the sheet in Maximized mode
this.windowstate = maximized!

m_pics_main.m_file.m_print.Enabled 			=	TRUE
m_pics_main.m_file.m_pagesetup.Enabled		=	TRUE
m_pics_main.m_file.m_printimmediate.Enabled	=	TRUE
m_pics_main.m_edit.m_addrow.Enabled 			=	TRUE

// set the tab order to zero, and disable some of the command buttons

dw_producer_holiday.visible =True
cb_clear.enabled =false
cb_exit.enabled =false
cb_update.enabled =false
cb_find.enabled =true
sle_prdr.SetFocus()
// Set the focus to the datawindows.


end event

type dw_prdr from datawindow within w_producer_holiday
integer x = 2249
integer y = 160
integer width = 379
integer height = 100
integer taborder = 20
string dataobject = "d_producer_col_only"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event constructor;this.SetTransObject(sqlservertrans)
this.retrieve()

end event

event itemchanged;is_data=data
sle_prdr.text=is_data
cb_find.TriggerEvent(clicked! )
end event

type st_1 from statictext within w_producer_holiday
integer x = 2190
integer y = 64
integer width = 521
integer height = 72
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long backcolor = 79741120
string text = "Existing Producers"
boolean focusrectangle = false
end type

type sle_prdr from u_sle within w_producer_holiday
integer x = 224
integer y = 176
integer width = 293
integer height = 92
integer taborder = 10
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
textcase textcase = upper!
integer limit = 4
end type

event modified;call super::modified;is_data= sle_prdr.text
dw_producer_holiday.visible =true
cb_clear.enabled =false
cb_exit.enabled =true
cb_update.enabled =false
cb_find.enabled =true
cb_find.default =true
cb_find.TriggerEvent(clicked! )
end event

type cb_find from commandbutton within w_producer_holiday
event clicked pbm_bnclicked
event getfocus pbm_bnsetfocus
event mousemove pbm_mousemove
string tag = "Find the record"
integer x = 1513
integer y = 1780
integer width = 233
integer height = 108
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "F&ind"
end type

event clicked;int rtn
dw_producer_holiday.visible =true

IF IsNull(is_data) OR is_data="" THEN
	MessageBox("ERROR","Producer is null.")
	RETURN
END IF
rtn = dw_producer_holiday.Retrieve(is_data)
IF rtn=0 THEN
	rtn = MessageBox("ERROR","No Holiday exist for producer "+is_data+", would you like to add a few?",Question!,YesNo!,1)
   IF rtn = 1 THEN
		dw_producer_holiday.Event pfc_addrow()
	END IF
END IF

cb_clear.enabled =true
cb_exit.enabled =true
cb_update.enabled =true
cb_find.enabled =true

end event

event getfocus;SetMicroHelp(w_pics_main,this.tag)
end event

event mousemove;SetMicroHelp(w_pics_main,this.tag)
end event

type cb_update from commandbutton within w_producer_holiday
event clicked pbm_bnclicked
event getfocus pbm_bnsetfocus
event mousemove pbm_mousemove
string tag = "Update the record"
integer x = 1806
integer y = 1780
integer width = 256
integer height = 108
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "&Update"
end type

event clicked;ib_disableclosequery =true
cb_clear.enabled =true
cb_exit.enabled =true
cb_update.enabled =true
cb_find.enabled =true
	
dw_producer_holiday.update()
IF dw_producer_holiday.AcceptText() < 0 THEN 
	RETURN
END IF
IF of_UpdateChecks( ) < 0 THEN 
	Return -1
ELSE 
	dw_producer_holiday.Event pfc_Update(True,True)	
END IF

IF f_check_dberror(SqlServerTrans,'w_producer_holiday') THEN
	COMMIT USING SQLServerTrans;
	MessageBox('update prdrhol','Update Successful.' )

END IF


end event

event getfocus;SetMicroHelp(w_pics_main,this.tag)

end event

event mousemove;SetMicroHelp(w_pics_main,this.tag)
end event

type cb_clear from commandbutton within w_producer_holiday
event clicked pbm_bnclicked
event getfocus pbm_bnsetfocus
event mousemove pbm_mousemove
string tag = "Clear the screen"
integer x = 2126
integer y = 1780
integer width = 233
integer height = 108
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "&Clear"
end type

event clicked;dw_producer_holiday.Reset()
cb_clear.enabled =false
cb_exit.enabled =true
cb_find.enabled =true
cb_update.enabled =false
is_data=""
sle_prdr.Text=""
sle_prdr.SetFocus()
end event

event getfocus;SetMicroHelp(w_pics_main,this.tag)
end event

event mousemove;SetMicroHelp(w_pics_main,this.tag)
end event

type cb_exit from commandbutton within w_producer_holiday
event clicked pbm_bnclicked
event getfocus pbm_bnsetfocus
event mousemove pbm_mousemove
string tag = "Exit the screen"
integer x = 2423
integer y = 1780
integer width = 233
integer height = 108
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "E&xit"
end type

event clicked;m_pics_main.m_file.m_print.Enabled 			=	FALSE
m_pics_main.m_file.m_pagesetup.Enabled		=	FALSE
m_pics_main.m_file.m_printimmediate.Enabled	=	FALSE
parent.Event pfc_close()
m_pics_main.m_menu.PopMenu(300, 0)

end event

event getfocus;SetMicroHelp(w_pics_main,this.tag)
end event

event mousemove;SetMicroHelp(w_pics_main,this.tag)
end event

type dw_producer_holiday from u_pics_dw within w_producer_holiday
integer x = 50
integer y = 328
integer width = 2574
integer height = 1416
integer taborder = 20
string dataobject = "d_producer_holiday"
end type

event clicked;call super::clicked;//string ls_col_name, ls_prdr
//ls_col_name= dwo.name
//ls_prdr =dw_producer_holiday.GetItemString(row,'prdr')
//IF ls_col_name ='prdr' and ( ls_prdr <>'' and Not IsNull(ls_prdr ) )THEN
//	dw_producer_holiday.Object.datawindow.QueryMode ='Yes'
//	cb_clear.enabled =FALSE
//	cb_exit.enabled =FALSE
//	cb_update.enabled =FALSE
//	cb_find.enabled =true
//	cb_find.default =true
//END IF
//
end event

event itemchanged;call super::itemchanged;//string ls_col_name
//ls_col_name= dwo.name
//IF ls_col_name ='prdr' THEN
//	dw_producer_holiday.Object.datawindow.QueryMode ='Yes'
//	cb_clear.enabled =FALSE
//	cb_exit.enabled =FALSE
//	cb_update.enabled =FALSE
//	cb_find.enabled =true
//	cb_find.default =true
//END IF
end event

event pfc_insertrow;//
return 1
end event

event pfc_addrow;long	ll_rc
string Lprdr

Lprdr = sle_prdr.Text

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
dw_producer_holiday.object.prdr[ll_rc] = Lprdr
//dw_producer_holiday.SetFocus()
dw_producer_holiday.ScrollToRow(ll_rc)
dw_producer_holiday.SetFocus()
dw_producer_holiday.SetColumn("holdt")
return ll_rc
end event

event constructor;call super::constructor;this.of_SetFilter(TRUE)
this.of_SetFind(TRUE)
this.inv_filter.of_SetStyle(1)
this.inv_filter.of_SetColumnNameSource(2)
this.of_SetSort(TRUE)
this.inv_sort.of_SetStyle(1)
this.inv_sort.of_SetColumnNameSource(2)
This.of_SetTransObject(sqlservertrans)
This.of_SetDropDownCalendar(TRUE)
This.iuo_calendar.of_Register("holdt",this.iuo_calendar.DDLB)


end event

type st_producer_holiday from statictext within w_producer_holiday
integer x = 827
integer y = 104
integer width = 1047
integer height = 128
boolean bringtotop = true
integer textsize = -18
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
boolean underline = true
long backcolor = 79741120
boolean enabled = false
string text = "Producer Holidays"
boolean focusrectangle = false
end type

type st_prdr from statictext within w_producer_holiday
integer x = 146
integer y = 56
integer width = 475
integer height = 88
boolean bringtotop = true
integer textsize = -10
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long backcolor = 79741120
boolean enabled = false
string text = "Producer Name"
boolean focusrectangle = false
end type


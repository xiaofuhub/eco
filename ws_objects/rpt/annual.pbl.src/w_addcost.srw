$PBExportHeader$w_addcost.srw
$PBExportComments$Window for Adding/Updating Additional Contract Costs
forward
global type w_addcost from w_response
end type
type sle_books from singlelineedit within w_addcost
end type
type st_2 from statictext within w_addcost
end type
type dw_addcost from u_pics_dw within w_addcost
end type
type cb_new from commandbutton within w_addcost
end type
type cbx_books from checkbox within w_addcost
end type
type st_1 from statictext within w_addcost
end type
type dw_mm_get_all_titles from u_pics_dw within w_addcost
end type
type cb_exit from commandbutton within w_addcost
end type
type cb_update from commandbutton within w_addcost
end type
end forward

global type w_addcost from w_response
integer x = 142
integer y = 168
integer width = 3406
integer height = 1888
string title = "Contract Additional Cost"
sle_books sle_books
st_2 st_2
dw_addcost dw_addcost
cb_new cb_new
cbx_books cbx_books
st_1 st_1
dw_mm_get_all_titles dw_mm_get_all_titles
cb_exit cb_exit
cb_update cb_update
end type
global w_addcost w_addcost

type variables
string lcntr,lcntrmed

end variables

on w_addcost.create
int iCurrent
call super::create
this.sle_books=create sle_books
this.st_2=create st_2
this.dw_addcost=create dw_addcost
this.cb_new=create cb_new
this.cbx_books=create cbx_books
this.st_1=create st_1
this.dw_mm_get_all_titles=create dw_mm_get_all_titles
this.cb_exit=create cb_exit
this.cb_update=create cb_update
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.sle_books
this.Control[iCurrent+2]=this.st_2
this.Control[iCurrent+3]=this.dw_addcost
this.Control[iCurrent+4]=this.cb_new
this.Control[iCurrent+5]=this.cbx_books
this.Control[iCurrent+6]=this.st_1
this.Control[iCurrent+7]=this.dw_mm_get_all_titles
this.Control[iCurrent+8]=this.cb_exit
this.Control[iCurrent+9]=this.cb_update
end on

on w_addcost.destroy
call super::destroy
destroy(this.sle_books)
destroy(this.st_2)
destroy(this.dw_addcost)
destroy(this.cb_new)
destroy(this.cbx_books)
destroy(this.st_1)
destroy(this.dw_mm_get_all_titles)
destroy(this.cb_exit)
destroy(this.cb_update)
end on

event closequery;Integer	li_pendingrc
Integer	li_validationrc
Integer	li_accepttextrc
Integer	li_msg,rtn
Integer	li_rc
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
			If rtn = 1 Then
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

event pfc_postopen;call super::pfc_postopen;int rtn
str_ancntr lstr_ancntr

lstr_ancntr = Message.PowerObjectParm
lcntr = lstr_ancntr.cntr
lcntrmed = lstr_ancntr.cntrmed
dw_addcost.SetTransObject(SQLServerTrans)
rtn = dw_addcost.Retrieve(lcntr,lcntrmed)
if rtn > 0 then
	dw_addcost.SetFocus()
else
	rtn = MessageBox("Warning","There are no additional cost for contract number "+"~""+lcntr+"~""+" exist, would you like to add one?",Question!,YesNoCancel!,1)
	if rtn = 1 then
		cb_new.TriggerEvent(Clicked!)
	end if
end if
	

end event

type sle_books from singlelineedit within w_addcost
integer x = 1815
integer y = 1652
integer width = 274
integer height = 88
integer taborder = 40
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 33554432
borderstyle borderstyle = stylelowered!
end type

type st_2 from statictext within w_addcost
integer x = 1019
integer y = 1652
integer width = 773
integer height = 72
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 33554432
long backcolor = 67108864
string text = "Number of books in contract"
boolean focusrectangle = false
end type

type dw_addcost from u_pics_dw within w_addcost
event ue_enterkey pbm_dwnprocessenter
integer x = 46
integer y = 36
integer width = 3319
integer height = 544
integer taborder = 20
string dataobject = "d_addcost"
end type

event ue_enterkey;Send(Handle(this),256,9,Long(0,0))
return(1)
end event

event itemchanged;call super::itemchanged;//IF dwo.name="chcost" THEN
//	IF (NOT(IsNull(data)) OR long(data)<>0 ) THEN
//		this.object.ancntr_cntrdol[row] = lcntrdol + long(data)
//	ELSE
//		this.object.ancntr_cntrdol[row] = lcntrdol
//	END IF
//END IF
end event

event constructor;call super::constructor;// set the transaction object.
this.SetTransObject(SQLServerTrans)

end event

event sqlpreview;call super::sqlpreview;//messagebox("sql",sqlsyntax)
end event

event pfc_insertrow;call super::pfc_insertrow;//
return -1
end event

event pfc_addrow;call super::pfc_addrow;long	ll_rc
boolean lb_disablelinkage
DATETIME Tdate
Tdate = datetime(Today(), now())

// Allow for pre functionality.
if this.Event pfc_preinsertrow() <= 0 then return NO_ACTION

// Is Querymode enabled?
if IsValid(inv_QueryMode) then lb_disablelinkage = inv_QueryMode.of_GetEnabled()

if not lb_disablelinkage then
	// Notify that a new row is about to be added.
	if IsValid ( inv_Linkage ) then inv_Linkage.Event pfc_InsertRow (0) 
end if

// Insert row.
if IsValid (inv_RowManager) then
	ll_rc = inv_RowManager.event pfc_addrow ()
else
	ll_rc = this.InsertRow (0) 
	this.object.cntr[ll_rc] = lcntr
	this.object.cntrmed[ll_rc] = lcntrmed
	this.object.chgdt[ll_rc] = Tdate
end if

if not lb_disablelinkage then
	// Notify that a new row has been added.
	if IsValid ( inv_Linkage ) then inv_Linkage.Event pfc_InsertRow (ll_rc) 
end if

// Allow for post functionality.
this.Post Event pfc_postinsertrow(ll_rc)

int i
for i = 1 to dw_addcost.rowcount()
	if IsNull(dw_addcost.object.cntr[i]) then
		dw_addcost.deleterow(i)
	end if
next

return ll_rc
end event

type cb_new from commandbutton within w_addcost
integer x = 2523
integer y = 1652
integer width = 229
integer height = 112
integer taborder = 30
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "&New"
end type

event clicked;dw_addcost.Event pfc_addrow()
dw_addcost.ScrollToRow(dw_addcost.rowcount())
end event

type cbx_books from checkbox within w_addcost
integer x = 46
integer y = 1652
integer width = 96
integer height = 80
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 33554432
long backcolor = 67108864
borderstyle borderstyle = stylelowered!
end type

event clicked;int rtn

IF this.Checked = TRUE THEN
	dw_mm_get_all_titles.SetTransObject(SQLServerTrans)
	rtn = dw_mm_get_all_titles.Retrieve(lcntr,lcntrmed)
	if rtn = 0 then
		MessageBox("ERROR","No data exist")
	else
		sle_books.text = string(dw_mm_get_all_titles.rowcount())
	end if
ELSE
	dw_mm_get_all_titles.Reset()
END IF
end event

type st_1 from statictext within w_addcost
integer x = 142
integer y = 1652
integer width = 549
integer height = 64
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 33554432
long backcolor = 67108864
string text = "Show me the books"
boolean focusrectangle = false
end type

type dw_mm_get_all_titles from u_pics_dw within w_addcost
integer x = 41
integer y = 580
integer width = 3319
integer height = 1048
integer taborder = 20
string dataobject = "d_mm_get_all_titles"
end type

event retrievestart;call super::retrievestart;openwithparm(w_pics_retrieve_msg_box,"Retrieving data, Please wait...")

end event

event retrieveend;call super::retrieveend;close(w_pics_retrieve_msg_box)

end event

type cb_exit from commandbutton within w_addcost
integer x = 3122
integer y = 1652
integer width = 229
integer height = 112
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

type cb_update from commandbutton within w_addcost
integer x = 2802
integer y = 1652
integer width = 265
integer height = 112
boolean bringtotop = true
integer textsize = -10
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "&Update"
end type

event clicked;int rc
decimal new_cntrdol

rc = dw_addcost.Event pfc_update(TRUE,TRUE)
		
if rc=1 THEN
	MessageBox("Update","AddCost table was updated.",Information!)
	dw_addcost.ResetUpdate()
	return 1
else 
	MessageBox("ERROR","Update failed.",StopSign!)
	return 0
end if
end event


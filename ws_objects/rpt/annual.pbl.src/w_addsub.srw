$PBExportHeader$w_addsub.srw
$PBExportComments$Window for adding/updating SubContractors
forward
global type w_addsub from w_response
end type
type cb_new from commandbutton within w_addsub
end type
type cb_exit from commandbutton within w_addsub
end type
type cb_update from commandbutton within w_addsub
end type
type dw_add_sub_cntr from u_pics_dw within w_addsub
end type
end forward

global type w_addsub from w_response
integer x = 142
integer y = 168
integer width = 2002
integer height = 1100
string title = "SubContractors"
cb_new cb_new
cb_exit cb_exit
cb_update cb_update
dw_add_sub_cntr dw_add_sub_cntr
end type
global w_addsub w_addsub

type variables
string lcntr,lcntrmed,lcntrtype

end variables

forward prototypes
public function boolean wf_get_prodstages ()
end prototypes

public function boolean wf_get_prodstages ();// Get the contract type and medium and get the appropriate
// production stages.
int rtn
DataWindowChild ldwc_prodstage
dw_add_sub_cntr.GetChild ("prodstage", ldwc_prodstage)
ldwc_prodstage.SetTransObject(sqlservertrans)
rtn = ldwc_prodstage.Retrieve(lcntrtype,lcntrmed)
IF rtn > 0 THEN
	RETURN TRUE
ELSE
	RETURN FALSE
END IF

end function

on w_addsub.create
int iCurrent
call super::create
this.cb_new=create cb_new
this.cb_exit=create cb_exit
this.cb_update=create cb_update
this.dw_add_sub_cntr=create dw_add_sub_cntr
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.cb_new
this.Control[iCurrent+2]=this.cb_exit
this.Control[iCurrent+3]=this.cb_update
this.Control[iCurrent+4]=this.dw_add_sub_cntr
end on

on w_addsub.destroy
call super::destroy
destroy(this.cb_new)
destroy(this.cb_exit)
destroy(this.cb_update)
destroy(this.dw_add_sub_cntr)
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
str_ancntrall lstr_ancntr
DataWindowChild ldwc_prodstage

lstr_ancntr = Message.PowerObjectParm
lcntr = lstr_ancntr.cntr
lcntrmed = lstr_ancntr.cntrmed
lcntrtype = lstr_ancntr.cntrtype

//MessageBox("data","cntr = "+lcntr+" cntrmed = "+lcntrmed+" cntrtype = "+lcntrtype)

dw_add_sub_cntr.GetChild ("prodstage", ldwc_prodstage)
ldwc_prodstage.SetTransObject(sqlservertrans)
ldwc_prodstage.Retrieve(lcntrtype,lcntrmed)

dw_add_sub_cntr.SetTransObject(SQLServerTrans)

rtn = dw_add_sub_cntr.Retrieve(lcntr,lcntrmed)
w_addsub.Title = "Subcontractors for "+lcntr
if rtn > 0 then
	dw_add_sub_cntr.SetFocus()
else
	rtn = MessageBox("Warning","There are no subcontractors for contract number "+"~""+lcntr+"~""+" exist, would you like to add one?",Question!,YesNoCancel!,1)
	if rtn = 1 then
		cb_new.TriggerEvent(Clicked!)
	end if
end if

	

end event

type cb_new from commandbutton within w_addsub
integer x = 1111
integer y = 864
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

event clicked;dw_add_sub_cntr.Event pfc_addrow()
dw_add_sub_cntr.ScrollToRow(dw_add_sub_cntr.rowcount())
end event

type cb_exit from commandbutton within w_addsub
integer x = 1710
integer y = 864
integer width = 229
integer height = 108
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

type cb_update from commandbutton within w_addsub
integer x = 1390
integer y = 864
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

dw_add_sub_cntr.accepttext()

rc = dw_add_sub_cntr.Event pfc_update(TRUE,TRUE)
		
if rc=1 THEN
	MessageBox("Update","SUB table was updated.",Information!)
	COMMIT USING SQLServerTrans;
	return 1
else 
	MessageBox("ERROR","Update failed.",StopSign!)
	return 0
end if
end event

type dw_add_sub_cntr from u_pics_dw within w_addsub
event ue_enterkey pbm_dwnprocessenter
integer x = 46
integer y = 36
integer width = 1906
integer height = 796
integer taborder = 20
string dataobject = "d_add_sub_cntr"
end type

event ue_enterkey;Send(Handle(this),256,9,Long(0,0))
return(1)
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
	IF wf_get_prodstages()=FALSE THEN
			MessageBox("ERROR","There are no production information in prodstage table"+ &
									" associated with contract type = "+lcntrtype+" and "+ &
									" contract media = "+lcntrmed+" .")
	END IF
end if

if not lb_disablelinkage then
	// Notify that a new row has been added.
	if IsValid ( inv_Linkage ) then inv_Linkage.Event pfc_InsertRow (ll_rc) 
end if

// Allow for post functionality.
this.Post Event pfc_postinsertrow(ll_rc)

int i
for i = 1 to dw_add_sub_cntr.rowcount()
	if IsNull(dw_add_sub_cntr.object.cntr[i]) then
		dw_add_sub_cntr.deleterow(i)
	end if
next

return ll_rc
end event


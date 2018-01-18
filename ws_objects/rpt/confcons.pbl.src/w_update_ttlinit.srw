$PBExportHeader$w_update_ttlinit.srw
forward
global type w_update_ttlinit from w_response
end type
type cb_print from commandbutton within w_update_ttlinit
end type
type sle_chang from singlelineedit within w_update_ttlinit
end type
type sle_cnt from singlelineedit within w_update_ttlinit
end type
type st_2 from statictext within w_update_ttlinit
end type
type st_1 from statictext within w_update_ttlinit
end type
type cb_soundex from commandbutton within w_update_ttlinit
end type
type dw_update_ttlinit from u_pics_dw within w_update_ttlinit
end type
type cb_exit from u_cb within w_update_ttlinit
end type
type cb_update from u_cb within w_update_ttlinit
end type
end forward

global type w_update_ttlinit from w_response
integer x = 443
integer y = 452
integer width = 2775
integer height = 1580
string title = "Grade Code and Descriptions"
cb_print cb_print
sle_chang sle_chang
sle_cnt sle_cnt
st_2 st_2
st_1 st_1
cb_soundex cb_soundex
dw_update_ttlinit dw_update_ttlinit
cb_exit cb_exit
cb_update cb_update
end type
global w_update_ttlinit w_update_ttlinit

type variables

end variables

forward prototypes
public function boolean wf_check_duplicates (string ls_dealer_code)
end prototypes

public function boolean wf_check_duplicates (string ls_dealer_code);return true
end function

on w_update_ttlinit.create
int iCurrent
call super::create
this.cb_print=create cb_print
this.sle_chang=create sle_chang
this.sle_cnt=create sle_cnt
this.st_2=create st_2
this.st_1=create st_1
this.cb_soundex=create cb_soundex
this.dw_update_ttlinit=create dw_update_ttlinit
this.cb_exit=create cb_exit
this.cb_update=create cb_update
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.cb_print
this.Control[iCurrent+2]=this.sle_chang
this.Control[iCurrent+3]=this.sle_cnt
this.Control[iCurrent+4]=this.st_2
this.Control[iCurrent+5]=this.st_1
this.Control[iCurrent+6]=this.cb_soundex
this.Control[iCurrent+7]=this.dw_update_ttlinit
this.Control[iCurrent+8]=this.cb_exit
this.Control[iCurrent+9]=this.cb_update
end on

on w_update_ttlinit.destroy
call super::destroy
destroy(this.cb_print)
destroy(this.sle_chang)
destroy(this.sle_cnt)
destroy(this.st_2)
destroy(this.st_1)
destroy(this.cb_soundex)
destroy(this.dw_update_ttlinit)
destroy(this.cb_exit)
destroy(this.cb_update)
end on

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
Integer	li_rc, li_rtn
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
			li_rtn = cb_update.Event clicked()
			IF li_rtn = 1 THEN
				RETURN 0
			END IF
			
			
			
//			// YES - Update
//			// If the update fails, prevent the window from closing
//			If This.Event pfc_save() >= 1 Then
//				// Successful update, allow the window to be closed
//				Return 0
//			End If
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

event resize;call super::resize;string ls_flang
//// Open the sheet in Maximized mode
//this.windowstate = maximized!
this.of_SetPreference(TRUE)
this.inv_preference.of_SetToolBars(TRUE)
this.inv_preference.of_SetWindow(TRUE)
this.of_SetResize(TRUE)
this.inv_resize.of_SetOrigSize(this.workspacewidth(),this.workspaceheight())
inv_resize.of_Register(cb_exit, "Scale")
inv_resize.of_Register(cb_print, "Scale")
inv_resize.of_Register(cb_soundex, "Scale")
inv_resize.of_Register(cb_update, "Scale")
inv_resize.of_Register(dw_update_ttlinit, "Scale")
inv_resize.of_Register(sle_chang, "Scale")
inv_resize.of_Register(sle_cnt, "Scale")
inv_resize.of_Register(st_1, "Scale")
inv_resize.of_Register(st_2, "Scale")

end event

event open;call super::open;// Open the sheet in Maximized mode
this.windowstate = maximized!
end event

event pfc_postopen;call super::pfc_postopen;date ld_date
string ls_date, ls_message
datetime ldt_datetime
time lt_time

lt_time = time('17:00:00')

open(w_gets_date_to_update)
ls_message=Message.StringParm
if ls_message='cancel' then
	return
else
	ls_date=Message.StringParm
	ld_date=date(ls_date)
	ldt_datetime=DateTime(ld_date,lt_time)
	dw_update_ttlinit.SetTransObject(SqlServerTrans)
	dw_update_ttlinit.Retrieve(ldt_datetime)
end if

end event

type cb_print from commandbutton within w_update_ttlinit
integer x = 1541
integer y = 1396
integer width = 256
integer height = 88
integer taborder = 40
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "print"
end type

event clicked;long job

job = PrintOpen( )

PrintDataWindow(job, dw_update_ttlinit)

PrintClose(job)
end event

type sle_chang from singlelineedit within w_update_ttlinit
integer x = 1216
integer y = 1388
integer width = 251
integer height = 92
integer taborder = 30
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 33554432
borderstyle borderstyle = stylelowered!
end type

type sle_cnt from singlelineedit within w_update_ttlinit
integer x = 475
integer y = 1388
integer width = 251
integer height = 92
integer taborder = 20
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 33554432
borderstyle borderstyle = stylelowered!
end type

type st_2 from statictext within w_update_ttlinit
integer x = 736
integer y = 1396
integer width = 457
integer height = 64
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 33554432
long backcolor = 67108864
string text = "Rows Changed"
boolean focusrectangle = false
end type

type st_1 from statictext within w_update_ttlinit
integer y = 1392
integer width = 457
integer height = 64
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 33554432
long backcolor = 67108864
string text = "Rows Counted:"
boolean focusrectangle = false
end type

type cb_soundex from commandbutton within w_update_ttlinit
integer x = 1833
integer y = 1396
integer width = 293
integer height = 88
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "soundex"
end type

event clicked;
Long li_loop, li_max_rows,llcnt=0, li_re, i
string ls_auth, ls_authfn, ls_ttl
boolean lb_not_cnct
li_max_rows = dw_update_ttlinit.RowCount()




//Delete blank lines from the datawindow
FOR li_loop =1 TO li_max_rows
	ls_auth=dw_update_ttlinit.object.auth[li_loop]
	ls_authfn=dw_update_ttlinit.object.authfn[li_loop]
	ls_ttl=dw_update_ttlinit.object.ttl[li_loop]
	if isnull(ls_auth) then ls_auth=''
	if IsNull(ls_authfn) then ls_authfn=''
	if IsNull(ls_ttl ) then ls_ttl=''
	dw_update_ttlinit.object.soundex_ttl[li_loop] = f_soundex(ls_ttl,'ttl')
	dw_update_ttlinit.object.soundex_auth[li_loop] = f_soundex(ls_auth+ls_authfn,'auth')
	llcnt ++
	
	if mod(llcnt,100)=0 then
		IF SQLserverTrans.sqlcode <> 0 THEN
			SQLserverTrans.of_connect()
			if SQLserverTrans.sqlcode <> 0 then
				i=0
				lb_not_cnct=true
				Do While lb_not_cnct
					i=i + 1
					if i=500 then
						SQLserverTrans.of_connect()
						IF SQLserverTrans.sqlcode = 0 THEN
							lb_not_cnct= false
						else
							lb_not_cnct= true
							i=0
						end if
					end if
				Loop
			end if //if sqlcode<>=0
		end if //if sqlcode<>0
		li_re=dw_update_ttlinit.event pfc_update(TRUE,TRUE)	
		OpenWithParm(w_pics_update_msg_box,"Updating data, Please Wait...")
		sle_chang.text = string(llcnt)
		if li_re=1 then
			commit using SQLServertrans;
			dw_update_ttlinit.resetupdate()
			close( w_pics_update_msg_box)
		else
			Rollback using SQLServertrans;
			close( w_pics_update_msg_box)
			li_loop= li_loop - 100 
			llcnt=llcnt - 100
		end if
	end if// if mod(,100)=0
NEXT
sle_chang.text = string(llcnt)
li_re=dw_update_ttlinit.update()	
OpenWithParm(w_pics_update_msg_box,"Updating data, Please Wait...")
if li_re=1 then
	commit using SQLServertrans;
	close( w_pics_update_msg_box)
else
	Rollback using SQLServertrans;
	close( w_pics_update_msg_box)
end if
//close(w_pics_update_msg_box)

end event

type dw_update_ttlinit from u_pics_dw within w_update_ttlinit
event ue_enterkey pbm_dwnprocessenter
integer x = 32
integer y = 32
integer width = 2706
integer height = 1344
integer taborder = 10
string title = "update soundex auth and soundex ttl"
string dataobject = "d_update_ttlinit_with_soundex2"
boolean hscrollbar = true
boolean resizable = true
boolean hsplitscroll = true
end type

event ue_enterkey;SEND(Handle(this), 256, 9, Long(0,0))
RETURN(1)
end event

event constructor;call super::constructor;SetTransObject(SqlServerTrans)

end event

event sqlpreview;call super::sqlpreview;//
end event

event rbuttondown;call super::rbuttondown;//////////////////////////////////////////////////////////////////////////////
//	Event:			rbuttondown
//	Description:	Allow for focus change on rbuttondown
//////////////////////////////////////////////////////////////////////////////
//	Rev. History	Version
//						5.0   Initial version
// 					6.0 	Added Linkage service notification.
//////////////////////////////////////////////////////////////////////////////
//	Copyright © 1996-1999 Sybase, Inc. and its subsidiaries.  All rights reserved.  Any distribution of the 
// PowerBuilder Foundation Classes (PFC) source code by other than Sybase, Inc. and its subsidiaries is prohibited.
//////////////////////////////////////////////////////////////////////////////
integer	li_rc
long		ll_currow
string	ls_colname
string	ls_curcolname

// Validate arguments.
if not ib_RMBfocuschange or IsNull (dwo) or row <= 0 then return

if IsValid (inv_Linkage) then
	If inv_Linkage.event pfc_rbuttondown (xpos, ypos, row, dwo) <> &
		inv_Linkage.CONTINUE_ACTION then
		// The user or an error prevents changing to a new row.
		return
	end if
end if

if IsValid (inv_RowSelect) then inv_RowSelect.event pfc_rbuttondown (xpos, ypos, row, dwo)

if dwo.type <> "column" then return

// Perform no action if already over current row/column.
ls_colname = dwo.name
ls_curcolname = this.GetColumnName()
ll_currow = this.GetRow()
if (ls_colname = ls_curcolname) and (row = ll_currow) then return

// Set row & column.
if this.SetRow (row) = 1 then	this.SetColumn (ls_colname)
end event

event ue_postconstructor;call super::ue_postconstructor;//This.Retrieve()

end event

event retrievestart;call super::retrievestart;OpenWithParm(w_pics_update_msg_box,"Retreiving data, Please Wait...")

end event

event retrieveend;call super::retrieveend;close(w_pics_update_msg_box)
sle_cnt.text = string(rowcount)

end event

event updatestart;call super::updatestart;OpenWithParm(w_pics_update_msg_box,"Updating data, Please Wait...")

end event

event updateend;call super::updateend;close(w_pics_update_msg_box)

end event

type cb_exit from u_cb within w_update_ttlinit
integer x = 2469
integer y = 1396
integer width = 238
integer height = 88
integer taborder = 0
integer textsize = -10
string text = "E&xit"
end type

event clicked;close(parent)
end event

type cb_update from u_cb within w_update_ttlinit
event clicked pbm_bnclicked
integer x = 2162
integer y = 1396
integer width = 270
integer height = 88
integer taborder = 0
integer textsize = -10
string text = "&Update"
end type

event clicked;Long li_rtn_code

li_rtn_code = parent.Event pfc_save() 
	
IF li_rtn_code = 1 THEN
	COMMIT USING SQLServerTrans;										
	MessageBox('Update','Database updated.')
ELSE
	ROLLBACK USING SqlServerTrans;
	MessageBox('Error','Update Error .. Contact Your DBA')
END IF//IF li_rtn_code = 1 THEN


end event


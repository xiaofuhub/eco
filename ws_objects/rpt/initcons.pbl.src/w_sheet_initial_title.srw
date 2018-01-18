$PBExportHeader$w_sheet_initial_title.srw
$PBExportComments$This screen will initiate the collection development section by creating the control number and other essential bibliographic information about the book
forward
global type w_sheet_initial_title from w_sheet
end type
type cb_2 from commandbutton within w_sheet_initial_title
end type
type cbx_3 from checkbox within w_sheet_initial_title
end type
type st_4 from statictext within w_sheet_initial_title
end type
type cbx_2 from checkbox within w_sheet_initial_title
end type
type st_3 from statictext within w_sheet_initial_title
end type
type cbx_1 from checkbox within w_sheet_initial_title
end type
type st_2 from statictext within w_sheet_initial_title
end type
type cb_find from commandbutton within w_sheet_initial_title
end type
type cb_update from commandbutton within w_sheet_initial_title
end type
type cb_clear from commandbutton within w_sheet_initial_title
end type
type cb_exit from commandbutton within w_sheet_initial_title
end type
type dw_title from u_pics_dw within w_sheet_initial_title
end type
type dw_coauthor from u_pics_dw within w_sheet_initial_title
end type
type st_1 from statictext within w_sheet_initial_title
end type
type cb_pub from commandbutton within w_sheet_initial_title
end type
type dw_src1 from u_pics_dw within w_sheet_initial_title
end type
type dw_src2 from u_pics_dw within w_sheet_initial_title
end type
type dw_src3 from u_pics_dw within w_sheet_initial_title
end type
type cb_srcs from commandbutton within w_sheet_initial_title
end type
end forward

shared variables

end variables

global type w_sheet_initial_title from w_sheet
integer x = 9
integer y = 8
integer width = 2926
integer height = 1988
string title = "Initial Consideration"
cb_2 cb_2
cbx_3 cbx_3
st_4 st_4
cbx_2 cbx_2
st_3 st_3
cbx_1 cbx_1
st_2 st_2
cb_find cb_find
cb_update cb_update
cb_clear cb_clear
cb_exit cb_exit
dw_title dw_title
dw_coauthor dw_coauthor
st_1 st_1
cb_pub cb_pub
dw_src1 dw_src1
dw_src2 dw_src2
dw_src3 dw_src3
cb_srcs cb_srcs
end type
global w_sheet_initial_title w_sheet_initial_title

type variables
BOOLEAN src_chng=FALSE,conno_exist=FALSE
String Local_chno
end variables

forward prototypes
public subroutine wf_set_taborder_original ()
public subroutine wf_set_taborder_zero ()
public function integer wf_valid_conno (string control_no)
public function boolean wf_val_ajyfn_casub (string ajyfn, string casub)
public subroutine wf_reset_dws ()
public function string wf_comb_authfn_auth (string authfn, string auth)
public function integer wf_get_label_data (string as_conno)
public function integer wf_clear_label_fields (boolean ab_visible)
end prototypes

public subroutine wf_set_taborder_original ();dw_title.Object.ttlinit_aepcd.tabsequence='10'   
dw_title.Object.ttlinit_cycle.tabsequence='20'   
dw_title.Object.ttlinit_auth.tabsequence='30'   
dw_title.Object.ttlinit_authfn.tabsequence='40'   
dw_title.Object.ttlinit_ahonorific.tabsequence='50'   
dw_title.Object.ttlinit_ttlart.tabsequence='60'   
dw_title.Object.ttlinit_ttl.tabsequence='70' 
dw_title.Object.ttlinit_publisher.tabsequence='80'   
dw_title.Object.ttlinit_pubyr.tabsequence='90'   
dw_title.Object.ttlinit_ajyfn.tabsequence='100'
dw_title.Object.ttlinit_casub.tabsequence='110'   
dw_title.Object.ttlinit_isbn.tabsequence='120'   
dw_title.Object.ttlinit_lang.tabsequence='130'   

end subroutine

public subroutine wf_set_taborder_zero ();dw_title.Object.ttlinit_aepcd.tabsequence='0'   
dw_title.Object.ttlinit_auth.tabsequence='0'   
dw_title.Object.ttlinit_authfn.tabsequence='0'   
dw_title.Object.ttlinit_ahonorific.tabsequence='0'   
dw_title.Object.ttlinit_ttldt.tabsequence='0'   
dw_title.Object.ttlinit_ttlart.tabsequence='0'   
dw_title.Object.ttlinit_ttl.tabsequence='0'   
dw_title.Object.ttlinit_ajyfn.tabsequence='0'   
dw_title.Object.ttlinit_cycle.tabsequence='0'   
dw_title.Object.ttlinit_isbn.tabsequence='0'   
dw_title.Object.ttlinit_lang.tabsequence='0'   
dw_title.Object.ttlinit_pubyr.tabsequence='0'   
dw_title.Object.ttlinit_publisher.tabsequence='0'   
dw_title.Object.ttlinit_casub.tabsequence='0'   

w_sheet_initial_title.cb_clear.Enabled=FALSE
w_sheet_initial_title.cb_update.Enabled=FALSE
w_sheet_initial_title.cb_find.Enabled=TRUE
cb_find.Default=TRUE
cb_pub.Enabled = FALSE
cb_srcs.Enabled = FALSE


end subroutine

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

public subroutine wf_reset_dws ();dw_coauthor.ResetUpdate( )	
dw_src1.ResetUpdate( )	
dw_src2.ResetUpdate( )	
dw_src3.ResetUpdate( )	
dw_title.ResetUpdate( )	
		
end subroutine

public function string wf_comb_authfn_auth (string authfn, string auth);IF	(IsNull(auth) or auth="") and (IsNull(authfn) or authfn="") THEN
	RETURN ("")
ELSE
	IF	(Not IsNull(auth) and Not IsNull(authfn)) THEN
		Return (authfn + " " + auth )
		
	ELSEIF	(Not IsNull(auth) and (IsNull(authfn) or TRIM(authfn) = "" )) THEN
		Return (auth)
	
	ELSEIF	((IsNull(auth) or auth = "" ) and Not IsNull(authfn) ) THEN
		return(authfn)
		
	ELSEIF   (NOT IsNull(auth) and NOT IsNull(authfn) ) THEN
		return(authfn + " " + auth)
		
	ELSE
		return(authfn + " " + auth)
	END IF
END IF




end function

public function integer wf_get_label_data (string as_conno);//11/03/2009 5.0

string ls_data='N', ls_print='N', ls_braille='N'
integer li_ret

li_ret = f_get_label_data(as_conno, ls_data, ls_print, ls_braille)

// 11/03/2009 DB Labels Project
wf_clear_label_fields(TRUE)


IF ls_data = 'Y' THEN
	cbx_1.checked=TRUE
ELSE
	cbx_1.checked=FALSE
END IF
IF ls_print = 'Y' THEN
	cbx_2.checked=TRUE
ELSE
	cbx_2.checked=FALSE
END IF

IF ls_braille = 'Y' THEN
	cbx_3.checked=TRUE
ELSE
	cbx_3.checked=FALSE
END IF
cb_2.enabled=TRUE
RETURN 1



end function

public function integer wf_clear_label_fields (boolean ab_visible);// 11/03/2009 DB Labels Project
string ls_bkmed, lconno=''


// validate conno
Lconno = dw_title.object.mchar_conno[1]
//select bkmed  into :ls_bkmed from mchar where conno = :lconno using sqlservertrans ;
//
//IF ls_bkmed = 'DB' THEN
//	cb_2.visible=ab_visible
//	cbx_1.visible=ab_visible
//	cbx_2.visible=ab_visible
//	cbx_3.visible=ab_visible
//	st_2.visible=ab_visible
//	st_3.visible=ab_visible
//	st_4.visible=ab_visible
//ELSE
	cb_2.visible=FALSE
	cbx_1.visible=FALSE
	cbx_2.visible=FALSE
	cbx_3.visible=FALSE
	st_2.visible=FALSE
	st_3.visible=FALSE
	st_4.visible=FALSE
//END IF

RETURN 1
end function

on w_sheet_initial_title.create
int iCurrent
call super::create
this.cb_2=create cb_2
this.cbx_3=create cbx_3
this.st_4=create st_4
this.cbx_2=create cbx_2
this.st_3=create st_3
this.cbx_1=create cbx_1
this.st_2=create st_2
this.cb_find=create cb_find
this.cb_update=create cb_update
this.cb_clear=create cb_clear
this.cb_exit=create cb_exit
this.dw_title=create dw_title
this.dw_coauthor=create dw_coauthor
this.st_1=create st_1
this.cb_pub=create cb_pub
this.dw_src1=create dw_src1
this.dw_src2=create dw_src2
this.dw_src3=create dw_src3
this.cb_srcs=create cb_srcs
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.cb_2
this.Control[iCurrent+2]=this.cbx_3
this.Control[iCurrent+3]=this.st_4
this.Control[iCurrent+4]=this.cbx_2
this.Control[iCurrent+5]=this.st_3
this.Control[iCurrent+6]=this.cbx_1
this.Control[iCurrent+7]=this.st_2
this.Control[iCurrent+8]=this.cb_find
this.Control[iCurrent+9]=this.cb_update
this.Control[iCurrent+10]=this.cb_clear
this.Control[iCurrent+11]=this.cb_exit
this.Control[iCurrent+12]=this.dw_title
this.Control[iCurrent+13]=this.dw_coauthor
this.Control[iCurrent+14]=this.st_1
this.Control[iCurrent+15]=this.cb_pub
this.Control[iCurrent+16]=this.dw_src1
this.Control[iCurrent+17]=this.dw_src2
this.Control[iCurrent+18]=this.dw_src3
this.Control[iCurrent+19]=this.cb_srcs
end on

on w_sheet_initial_title.destroy
call super::destroy
destroy(this.cb_2)
destroy(this.cbx_3)
destroy(this.st_4)
destroy(this.cbx_2)
destroy(this.st_3)
destroy(this.cbx_1)
destroy(this.st_2)
destroy(this.cb_find)
destroy(this.cb_update)
destroy(this.cb_clear)
destroy(this.cb_exit)
destroy(this.dw_title)
destroy(this.dw_coauthor)
destroy(this.st_1)
destroy(this.cb_pub)
destroy(this.dw_src1)
destroy(this.dw_src2)
destroy(this.dw_src3)
destroy(this.cb_srcs)
end on

event pfc_preopen;call super::pfc_preopen;this.of_SetPreference(TRUE)
this.inv_preference.of_SetToolBars(TRUE)
this.inv_preference.of_SetWindow(TRUE)
this.of_SetResize(TRUE)
this.inv_resize.of_SetOrigSize(this.workspacewidth(),this.workspaceheight())
inv_resize.of_Register(dw_title, "Scale")
inv_resize.of_Register(dw_src1, "Scale")
inv_resize.of_Register(dw_src2, "Scale")
inv_resize.of_Register(dw_src3, "Scale")
inv_resize.of_Register(dw_coauthor, "Scale")
inv_resize.of_Register(cb_clear, "Scale")
inv_resize.of_Register(cb_exit, "Scale")
inv_resize.of_Register(cb_find, "Scale")
inv_resize.of_Register(cb_update, "Scale")
inv_resize.of_Register(cb_pub, "Scale")
inv_resize.of_Register(cb_srcs, "Scale")
inv_resize.of_Register(st_1, "Scale")




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
dw_coauthor.Enabled = FALSE

// set the tab order to zero, and disable some of the command buttons
wf_set_taborder_zero()

OpenWithParm(w_pics_retrieve_msg_box,"Initializing Title Information Screen. Please Wait...")
// Initialy set the query mode to yes. 
dw_title.Object.DataWindow.QueryMode='Yes'

close(w_pics_retrieve_msg_box)

// Set the focus to the datawindows.
dw_title.SetFocus()

// 11/03/2009 DB Labels Project
wf_clear_label_fields(FALSE)

end event

type cb_2 from commandbutton within w_sheet_initial_title
string tag = "Add/Update Source information"
boolean visible = false
integer x = 2025
integer y = 836
integer width = 379
integer height = 108
integer taborder = 60
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "Label Data..."
end type

event clicked;string ls_conno
int li_ret 
ls_conno = dw_title.Object.mchar_conno[1]
li_ret = f_call_label_app(ls_conno)

end event

type cbx_3 from checkbox within w_sheet_initial_title
boolean visible = false
integer x = 1915
integer y = 852
integer width = 69
integer height = 52
integer textsize = -8
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 33554432
long backcolor = 67108864
boolean enabled = false
boolean automatic = false
boolean thirdstate = true
end type

type st_4 from statictext within w_sheet_initial_title
boolean visible = false
integer x = 1371
integer y = 852
integer width = 503
integer height = 52
integer textsize = -8
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 33554432
long backcolor = 67108864
string text = "Braille Label Approved:"
boolean focusrectangle = false
end type

type cbx_2 from checkbox within w_sheet_initial_title
boolean visible = false
integer x = 1230
integer y = 852
integer width = 69
integer height = 52
integer textsize = -8
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 33554432
long backcolor = 67108864
boolean enabled = false
boolean automatic = false
boolean thirdstate = true
end type

type st_3 from statictext within w_sheet_initial_title
boolean visible = false
integer x = 722
integer y = 852
integer width = 485
integer height = 52
integer textsize = -8
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 33554432
long backcolor = 67108864
string text = "Print Label Approved:"
boolean focusrectangle = false
end type

type cbx_1 from checkbox within w_sheet_initial_title
boolean visible = false
integer x = 608
integer y = 852
integer width = 69
integer height = 52
integer textsize = -8
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 33554432
long backcolor = 67108864
boolean enabled = false
boolean automatic = false
boolean thirdstate = true
end type

type st_2 from statictext within w_sheet_initial_title
boolean visible = false
integer x = 155
integer y = 852
integer width = 439
integer height = 52
integer textsize = -8
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 33554432
long backcolor = 67108864
string text = "Label Data Filled In:"
boolean focusrectangle = false
end type

type cb_find from commandbutton within w_sheet_initial_title
event clicked pbm_bnclicked
event getfocus pbm_bnsetfocus
event mousemove pbm_mousemove
string tag = "Find the record"
integer x = 1641
integer y = 1720
integer width = 233
integer height = 108
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "F&ind"
boolean default = true
end type

event clicked;long ll_rows
string Lconno,Lchno,Lsrc,titletxt
int rtn,pos1=0,pos2=0,pos3=0,lcnt
n_cst_string 	inv_string

DataWindowChild src_child,src2_child,src3_child
dw_src1.GetChild('src_code', src_child)
dw_src2.GetChild('src_code', src2_child)
dw_src3.GetChild('src_code', src3_child)
src_child.SettransObject(SqlserverTrans)
src_child.SettransObject(SqlserverTrans)
src_child.SettransObject(SqlserverTrans)
src_child.retrieve()
src2_child.retrieve()
src3_child.retrieve()
dw_src1.retrieve()
dw_src2.retrieve()
dw_src3.retrieve()
dw_src1.object.src_desc[1] = ""
dw_src2.object.src_desc[1] = ""
dw_src3.object.src_desc[1] = ""
dw_title.AcceptText()

// Validate the control number prior to any other activities
Lconno = dw_title.GetText()


IF Lconno <> "" THEN
	CHOOSE CASE wf_valid_conno(Lconno)
	CASE 1
  		MessageBox("Validation Error","Control Number must be 8 digit.",StopSign!)
		dw_title.Object.mchar_conno.TabSequence='10'
		dw_title.Object.DataWindow.QueryMode='Yes'
		wf_set_taborder_zero()
		dw_title.ResetUpdate()
		dw_title.SetFocus()
		w_sheet_initial_title.cb_clear.Enabled=FALSE
		w_sheet_initial_title.cb_update.Enabled=FALSE
		RETURN
	CASE 2
  		MessageBox("Validation Error","Format of the Control number is ##99####, (example: 77991234)",StopSign!)
		dw_title.Object.mchar_conno.TabSequence='10'
		dw_title.Object.DataWindow.QueryMode='Yes'
		wf_set_taborder_zero()
		dw_title.ResetUpdate()
		dw_title.SetFocus()
		w_sheet_initial_title.cb_clear.Enabled=FALSE
		w_sheet_initial_title.cb_update.Enabled=FALSE
		RETURN
	END CHOOSE
	IF f_is_it_archived(Lconno,0) THEN
		dw_title.Object.mchar_conno.TabSequence='10'
		dw_title.Object.DataWindow.QueryMode='Yes'
		wf_set_taborder_zero()
		dw_title.ResetUpdate()
		dw_title.SetFocus()
		w_sheet_initial_title.cb_clear.Enabled=FALSE
		w_sheet_initial_title.cb_update.Enabled=FALSE
		RETURN
	END IF
	// Set off the query mode.
	dw_title.Object.DataWindow.QueryMode='No'
	// Retrieve from database with control number as a argument.
	ll_rows = dw_title.Retrieve( )
  	IF ll_rows < 1 THEN
		conno_exist = FALSE
		// Insert a new row 
		dw_title.InsertRow(0)
		// If no rows were found display a message to insert
		// a new reocrd or not.
      rtn = MessageBox("Find Error", "Control Number: "+Lconno+" Does not exist, Insert new record?" ,Question!, OkCancel!, 1)
		IF rtn=1 THEN
			// Set the control number.
			dw_title.object.mchar_conno[1]=Lconno
			// Set the title creation date to today's date
			dw_title.object.ttlinit_ttldt[1]=today()
			// Set the crflag to 'N' 
			dw_title.object.ccrflag[1]='N'
			// Set the crname to NULL
			dw_title.object.ccrname[1]=""
			dw_title.object.ttlinit_lang[1]="ENG"
			// Set the tab order of the control number and chart number to zero
			// and calculate the value of the chart number
			dw_title.Object.mchar_conno.TabSequence='0'
			dw_title.Object.ttlinit_ttldt.TabSequence='0'
			
			Lchno=Mid(Lconno,1,2)+Mid(Lconno,5,4)
			Local_chno = Lchno
			dw_title.object.ttlinit_chno[1]=Lchno

			// set the chart number in coauthor screen.
			//dw_coauthor.object.chno[1]=Lchno
			
			// Enable the pushbuttons
			w_sheet_initial_title.cb_find.Enabled	=FALSE
		   w_sheet_initial_title.cb_update.Enabled=TRUE
		   w_sheet_initial_title.cb_clear.Enabled	=TRUE
		   w_sheet_initial_title.cb_exit.Enabled	=TRUE
			w_sheet_initial_title.cb_pub.Enabled 	=TRUE
			w_sheet_initial_title.cb_srcs.Enabled 	=TRUE
			m_pics_main.m_edit.m_addrow.Enabled 	=TRUE
			m_pics_main.m_edit.m_deleterow.Enabled =TRUE
			
		ELSE
			cb_clear.TriggerEvent(Clicked!)
		END IF
	ELSE
		conno_exist = TRUE
      // Get the title and trim out the extra trailing spaces.
		titletxt =  inv_string.of_Trim(dw_title.object.ttlinit_ttl[1])
		dw_title.object.ttlinit_ttl[1] = titletxt
		// messagebox("title",titletxt+" "+string(len(titletxt)))
		// Retrieve the Source

		// If this is an update to existing record
		Lchno = dw_title.object.ttlinit_chno[1]
		// get the coauthors
		ll_rows = dw_coauthor.Retrieve(Lchno)
		if ll_rows < 1 THEN
			//dw_coauthor.Event pfc_InsertRow()
			//dw_coauthor.object.chno[1]=Lchno
		end if
		Lsrc = dw_title.object.ttlinit_srcdoc[1]
		// Get the position of the first ";"
		pos1=pos(Lsrc,";")
		IF pos1<>0 THEN
			pos2=pos(Lsrc,";",pos1+1)
			IF pos2<>0 THEN
				pos3=pos(Lsrc,";",pos2+1)
			END IF
		END IF
		//MessageBox("position",string(pos1)+" "+string(pos2)+" "+string(pos3)+" "+string(Len(Lsrc)))
		
		// Get the first source doc based on the first position of ";"
		IF pos1=0 THEN
			IF left(Lsrc,len(Lsrc))<>"" THEN
				//ddlb_src1.text = left(Lsrc,Len(Lsrc))
				dw_src1.object.src_desc[1] = left(Lsrc,Len(Lsrc))
			END IF
		ELSEIF left(Lsrc,pos1) <> "" THEN
			dw_src1.object.src_desc[1] = left(Lsrc,pos1)
			//ddlb_src1.text = left(Lsrc,pos1)
		ELSE
			dw_src1.object.src_desc[1] = ""
		END IF
		
		IF pos2<>0 AND pos2>pos1 THEN
			dw_src2.object.src_desc[1] = Mid(Lsrc,pos1+1,pos2 - pos1)
			//ddlb_src2.text = Mid(Lsrc,pos1+1,pos2 - pos1)
		ELSEIF pos2=0 AND pos1<>0 AND Len(Lsrc)>pos1 THEN
			dw_src2.object.src_desc[1] = Mid(Lsrc,pos1+1,Len(Lsrc))
			//ddlb_src2.text = Mid(Lsrc,pos1+1,Len(Lsrc))
		ELSE
			dw_src2.object.src_desc[1] = ""
		END IF
			
		IF pos3<>0 AND pos3>pos2 THEN
			// Get the third source doc based on the second position of ";"
			dw_src3.object.src_desc[1] = mid(Lsrc,pos2+1,pos3 - pos2)
			//ddlb_src3.text = mid(Lsrc,pos2+1,pos3 - pos2)
		ELSEIF pos3=0 AND pos2<>0 AND Len(Lsrc)>pos2 THEN
			dw_src3.object.src_desc[1] = Mid(Lsrc,pos2+1,Len(Lsrc))
			//ddlb_src3.text = Mid(Lsrc,pos2+1,Len(Lsrc))
		ELSE
			dw_src3.object.src_desc[1] = ""
		END IF
		
		// If you are updating the existing row.
		// Unable editing control number and the chart number.
		dw_title.Object.mchar_conno.TabSequence='0'
		// Enable the push buttons
		w_sheet_initial_title.cb_clear.Enabled	=TRUE
		w_sheet_initial_title.cb_update.Enabled=TRUE
		w_sheet_initial_title.cb_find.Enabled	=FALSE
		w_sheet_initial_title.cb_pub.Enabled 	=TRUE
		w_sheet_initial_title.cb_srcs.Enabled 	=TRUE
		m_pics_main.m_edit.m_addrow.Enabled =	TRUE
		m_pics_main.m_edit.m_deleterow.Enabled =	TRUE
  	END IF
	// Set the tab order.
	wf_set_taborder_original()
	dw_title.Object.ttlinit_ttldt.TabSequence='0'
	// Set the focus on the datawindow
	dw_title.SetFocus()
	dw_coauthor.Enabled = TRUE
	ib_disableclosequery = TRUE
	
	// 11/03/2009 GET THE LABEL DATA
	wf_get_label_data(lconno)
ELSE
   MessageBox("Database Error", "Please enter the Control Number." ,StopSign!, OK!, 2)
	dw_title.Reset()
	dw_title.setfocus()
END IF
end event

event getfocus;SetMicroHelp(w_pics_main,this.tag)
end event

event mousemove;SetMicroHelp(w_pics_main,this.tag)
end event

type cb_update from commandbutton within w_sheet_initial_title
event clicked pbm_bnclicked
event getfocus pbm_bnsetfocus
event mousemove pbm_mousemove
string tag = "Update the record"
integer x = 1938
integer y = 1720
integer width = 256
integer height = 108
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "&Update"
end type

event clicked;int rc,ll_rows,i,lcnt,li_re, li_row_count
String Lconno,Lchno,Lttl,Lsrc,Lcoauth,Lsttl, ls_ttl, ls_sttl, ls_sauth, &
		ls_auth,Laepcd,ls_authfn,ls_soundex, ls_soundex_auth,Ls_parprt, ls_src, ls_ttldt,ls_data
Boolean ans

// Accept the text that was put on the screen.
//IF dw_title.AcceptText() < 0 THEN RETURN
//dw_coauthor.AcceptText()
dw_src1.accepttext()
dw_src2.accepttext()
dw_src2.accepttext()
// Check for any pending updates
IF of_UpdateChecks( ) < 0 THEN Return -1

// If there were no changes to the screen, don't try to update the screen.
IF (dw_title.ModifiedCount() > 0 OR &
	 dw_coauthor.ModifiedCount() > 0 OR &
	 dw_coauthor.DeletedCount() > 0 OR &
	 src_chng = TRUE) THEN
	
	// validate ajyfn against casub
	ans =	wf_val_ajyfn_casub(dw_title.object.ttlinit_ajyfn[1],dw_title.object.ttlinit_casub[1])
	IF ans = FALSE THEN
		MessageBox("ERROR","Invalid ~'AJYFN~' Code against ~'CASUB~' Code.",StopSign!)
		RETURN -1
	END IF
	
	Lconno=dw_title.object.mchar_conno[1]
	Ls_parprt=dw_title.object.mchar_parprt[1]
	Lchno=dw_title.object.ttlinit_chno[1]
	Laepcd=dw_title.object.ttlinit_aepcd[1]
	Lttl=trim(dw_title.object.ttlinit_ttl[1])
	ls_auth=trim(dw_title.object.ttlinit_auth[1])
	ls_authfn=trim(dw_title.object.ttlinit_authfn[1])
	ls_ttl=trim(dw_title.object.ttlinit_ttl[1])
	ls_sauth = trim(f_create_sttl(ls_auth))
	ls_sttl = trim(f_create_sttl(ls_ttl))
	// Get the soundex title
	ls_soundex = trim(f_soundex(Lttl,'ttl'))
	ls_soundex_auth = trim(f_soundex(ls_auth+ls_authfn,'auth'))
	dw_title.object.ttlinit_sauth[1] =ls_sauth
	dw_title.object.ttlinit_sttl[1] =ls_sttl
	dw_title.object.ttlinit_soundex_ttl[1] = ls_soundex
	dw_title.object.ttlinit_soundex_auth[1] = ls_soundex_auth
	ls_ttldt = trim( String(dw_title.object.ttlinit_ttldt[1],'MM/DD/YYYY'))
	dw_title.object.ttlinit_ttldt[1]=datetime(date(ls_ttldt))
//	dw_title.object.ttlinit_ttldt[1]=date(ls_ttldt)
	
	// If parprt is already set to 'P'(printed par) assign it to 'C'(changed par).
	IF Ls_parprt = 'P' THEN
		f_set_parprt(Lconno,'C')
	END IF
	
	
	IF ((IsNull(ls_auth) OR ls_auth="") AND Laepcd="L") THEN
  		MessageBox("Validation Error","Author's lastname must be entered.",StopSign!)
		dw_title.SetFocus()
		dw_title.SetColumn(4) 
		RETURN -1
	END IF
	
	IF ((IsNull(ls_authfn) OR ls_authfn="") AND Laepcd="A") THEN
  		MessageBox("Validation Error","Author's firstname must be entered.",StopSign!)
		dw_title.SetFocus()
		dw_title.SetColumn(6) 
		RETURN -1
	END IF
	select count(*) into :lcnt
	from mchar
	where conno=:lconno
	using SqlServerTrans;
	if not f_check_dberror(SqlserverTrans,'select count from mchar') then
		return
	end if
	if lcnt=0 then
   // Get the Copyright name
		dw_title.object.ccrname[1] = wf_comb_authfn_auth(ls_authfn,ls_auth)
	end if
	
	IF (IsNull(Lconno) or IsNull(Lchno) or IsNull(Lttl)) THEN
  		MessageBox("Validation Error","Control Number and title information must be entered.",StopSign!)
		RETURN -1
	ELSE
		//Assign the srcdoc
		Lsrc=dw_src1.object.src_desc[1]+dw_src2.object.src_desc[1]+dw_src3.object.src_desc[1]
		dw_title.object.ttlinit_srcdoc[1]=Lsrc
		ls_src=dw_src1.object.src_code[1]
// ls_data =' Lconno = '+Lconno+'  Lchno = '+Lchno+'  Lttl= '+Lttl+' Lsrc =  '+Lsrc+' Lsttl = '+Lsttl+'  ls_ttl ='+ls_ttl+'  ls_sttl = '+ls_sttl+'  ls_sauth ='+ls_sauth+' ls_auth= '+ls_auth+'  Laepcd='+Laepcd+' ls_soundex= '+ls_soundex+' ls_soundex_auth= '+ls_soundex_auth+' ls_src= '+ls_src+' ls_ttldt= '+ls_ttldt
// Messagebox('ERROR MESSAGE',ls_data)
		rc = dw_title.Event pfc_Update(True,True)
		IF rc = 1 THEN
			// Check to see if this is not Title only books (without Author or CoAuthor)
			IF Laepcd <> "N" THEN
				// Update coauth table
				FOR i=1 to dw_coauthor.RowCount()
					
					Lchno=dw_coauthor.object.chno[i]
					Lcoauth=dw_coauthor.object.coauth[i]
					
					IF NOT(IsNull(Lcoauth)) or Lcoauth <> "" THEN
						Select count(*) into :Lcnt from coauth 
						where chno=:Lchno and coauth=:Lcoauth
						using sqlservertrans;
						
						IF Lcnt = 0 THEN
							dw_coauthor.SetItemStatus(i, 0, Primary!, NewModified!)
						ELSE
							dw_coauthor.SetItemStatus(i, 0, Primary!, DataModified!)
						END IF
//					ELSE
//						dw_coauthor.DeleteRow(i)
					END IF
				NEXT
				IF dw_coauthor.Event pfc_update(true,true)=1 THEN
					wf_reset_dws()
					COMMIT USING SQLServerTrans;
				ELSE
					ROLLBACK USING SQLServerTrans;
					MessageBox("Error","Update coauthor failed. ",StopSign!)
					RETURN -1
				END IF
			END IF
			// Get the short title
			Lsttl = f_create_sttl(Lttl)
			
			IF dw_title.AcceptText() = 1 THEN
				// Get the Chart Number
				Lchno=dw_title.object.ttlinit_chno[1]
			END IF
			
			IF Lchno="" OR IsNull(Lchno) THEN
				Lchno = Local_chno
			END IF
			
			// See if the short title exist in database
			Select count(*) into :Lcnt from sttltbl
				where chno=:Lchno
			using sqlservertrans;
						
			// If No rows exist in sttltbl 
			IF Lcnt = 0 THEN
			// Short title does not exist(Insert)
				INSERT INTO sttltbl ( chno,sttl )
  				 VALUES ( :Lchno,:Lsttl )
				USING SQLServerTrans;
				IF f_check_dberror(sqlservertrans,"STTL") THEN
					COMMIT USING SqlServerTrans;
				ELSE 
					ROLLBACK USING SqlServerTrans;
				END IF
			ELSE
			// Short title does exist(Update)
				UPDATE sttltbl  
     			 SET sttl = :Lsttl  
				WHERE chno = :Lchno
				USING SqlServerTrans;
				IF f_check_dberror(sqlservertrans,"STTL") THEN
					COMMIT USING SqlServerTrans;
				ELSE 
					ROLLBACK USING SqlServerTrans;
				END IF
			END IF
			IF conno_exist=FALSE THEN
				f_update_mchar_time(Lconno,0,"C","A")
			ELSE
				f_update_mchar_time(Lconno,0,"C","U")
			END IF				
			COMMIT USING SQLServerTrans;
			MessageBox("Update","Record is updated.",Information!)
			RETURN 1
		ELSE // update ttlinit
			ROLLBACK USING SQLServerTrans;
			MessageBox("Error","Update failed. Error in table ttlinit",StopSign!)
			RETURN -1
		END IF
	END IF // Validation test		
ELSE // if Modifiedcount > 1
 	MessageBox("Update","There are no changes to the record.",Information!)
	RETURN -1
END IF
end event

event getfocus;SetMicroHelp(w_pics_main,this.tag)
end event

event mousemove;SetMicroHelp(w_pics_main,this.tag)
end event

type cb_clear from commandbutton within w_sheet_initial_title
event clicked pbm_bnclicked
event getfocus pbm_bnsetfocus
event mousemove pbm_mousemove
string tag = "Clear the screen"
integer x = 2258
integer y = 1720
integer width = 233
integer height = 108
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "&Clear"
end type

event clicked;long ll_rows,rtn

dw_coauthor.Reset()
dw_title.Reset()

dw_src1.object.src_desc[1]=""
dw_src2.object.src_desc[1]=""
dw_src3.object.src_desc[1]=""

// Clear the querymode of the datawindow.
//dw_title.Object.DataWindow.QueryClear = 'Yes'

dw_coauthor.Reset()

// Insert a blank row
ll_rows = dw_coauthor.InsertRow(0)
ll_rows = dw_title.InsertRow(0)


// Scroll to that row
dw_title.ScrolltoRow(ll_rows)

// Set the tab order of control number
dw_title.Object.mchar_conno.TabSequence='10'

// Set the tab order of the remaining fields to zero
wf_set_taborder_zero()

// Reset Update flag in all the datawindow for exit button validation
dw_coauthor.ResetUpdate( )
dw_src1.ResetUpdate( )
dw_src2.ResetUpdate( )
dw_src3.ResetUpdate( )
dw_title.ResetUpdate( )

// Set on the query mode
dw_title.Object.DataWindow.QueryMode='Yes'
dw_title.object.mchar_conno[1]=""

// Set the focus on the control number
dw_title.setfocus()

// 11/03/2009 DB Labels Project
wf_clear_label_fields(FALSE)

end event

event getfocus;SetMicroHelp(w_pics_main,this.tag)
end event

event mousemove;SetMicroHelp(w_pics_main,this.tag)
end event

type cb_exit from commandbutton within w_sheet_initial_title
event clicked pbm_bnclicked
event getfocus pbm_bnsetfocus
event mousemove pbm_mousemove
string tag = "Exit the screen"
integer x = 2555
integer y = 1720
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
m_pics_main.m_edit.m_addrow.Enabled =	FALSE
parent.Event pfc_close()
m_pics_main.m_menu.PopMenu(300, 0)

end event

event getfocus;SetMicroHelp(w_pics_main,this.tag)
end event

event mousemove;SetMicroHelp(w_pics_main,this.tag)
end event

type dw_title from u_pics_dw within w_sheet_initial_title
event ue_enterkey pbm_dwnprocessenter
event pfc_hinttext pbm_mousemove
event ue_key pbm_dwnkey
integer x = 37
integer y = 12
integer width = 2743
integer height = 1048
integer taborder = 10
string dataobject = "d_initial_title"
boolean vscrollbar = false
boolean livescroll = false
boolean ib_isupdateable = false
boolean ib_rmbmenu = false
boolean ib_rmbfocuschange = false
string is_updatesallowed = ""
end type

event ue_enterkey;call super::ue_enterkey;IF (dw_title.GetColumn() <> 1 ) THEN
	Send(Handle(this),256,9,Long(0,0))
	return(1)
end if
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

event ue_key;//////////////////////////////////////////////////////////////////////////////////////////////////////////
//
//	Event:  key event for dw_title
//
//	Description:
//	Problem with the tab scroling to other rows, which are not used on the screen
// resulting in an PB error
//
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//	
//	Revision History
//
//	Developed by 	Date 				Version						Tracking#
//									
// Murali K.			01/29/2008      005 PICS Modifications	 Reqs: CDS.A.PTB2,ptb2.1
//											If tab key is pressed, simulate find functionality for the 
//											entered conno
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//

string ls_conno
IF  KeyDown(KeyTab!) THEN
   // determine whether it is a tab or back-tab
	IF this.GetColumnName ( ) = 'mchar_conno' THEN
		ls_conno = this.gettext() //object.mchar_conno[1]
		IF len(trim(ls_conno)) > 0 THEN
		      cb_find.triggerevent ('clicked')
			ELSE
				RETURN 1
		END IF
	END IF
END IF
RETURN 0
end event

event itemchanged;Int rtn,i, li_re, li_cnt,li_len
String ls_null, ls_ln, ls_fn, ls_conno, ls_crname, ls_chno, ls_crnameold
SetNull(ls_null)
IF DWO.Name = "ttlinit_casub" THEN
	IF	(wf_val_ajyfn_casub(dw_title.object.ttlinit_ajyfn[1],GetText()) = FALSE) THEN
		Messagebox("ERROR","Invalid ~'AJYFN~' Code against ~'CASUB~' Code.",stopSign!)
		RETURN 0
	END IF
ELSEIF DWO.Name = "ttlinit_aepcd" THEN
	String aepcd
	aepcd = Left(Data,1)
	IF aepcd <> 'A' AND aepcd <> 'C' AND aepcd <> 'E' AND aepcd <> 'P' AND &
		aepcd <> 'S' AND aepcd <> 'N' AND aepcd <> 'T' AND aepcd <> 'R' AND &
		aepcd <> 'I' AND aepcd <> 'L' THEN
		RETURN 1
	END IF
	IF aepcd = 'A' THEN
		dw_title.object.ttlinit_auth.Edit.Required='Yes'
		dw_title.object.ttlinit_authfn.Edit.Required='Yes'
	ELSEIF aepcd = 'L' THEN
		dw_title.object.ttlinit_authfn[1] = ls_null
		dw_title.object.ttlinit_ahonorific[1] = ""
		dw_title.object.ttlinit_auth.Edit.Required='Yes'
		dw_title.object.ttlinit_authfn.Edit.Required='No'	
	ELSE
		dw_title.object.ttlinit_auth.Edit.Required='No'
		dw_title.object.ttlinit_authfn.Edit.Required='No'
	END IF
	IF aepcd = 'N' THEN
		dw_title.object.ttlinit_authfn[1] = ""
		dw_title.object.ttlinit_auth[1] = ""
		dw_title.object.ttlinit_ahonorific[1] = ""
		IF (dw_coauthor.RowCount() > 0 AND NOT(IsNull(dw_coauthor.object.coauth[1]))) THEN
			rtn = Messagebox("Warnning","Author and Coauthors are not required for this Title Entry. Remove it?",question!,yesNo!,1)
			IF rtn = 1 THEN
				FOR i=1 TO dw_coauthor.RowCount()
					dw_coauthor.DeleteRow(i)
				NEXT
				dw_coauthor.EVENT pfc_update(TRUE,TRUE)
				COMMIT USING SqlServerTrans;
				dw_coauthor.Reset()
				dw_coauthor.enabled = FALSE
			ELSE
				RETURN
			END IF
		ELSE
			dw_coauthor.enabled = FALSE
		END IF
	ELSE
		dw_coauthor.enabled = TRUE
	END IF
ELSEIF DWO.Name = "ttlinit_cycle" THEN
	String cycle
	cycle = Left(Data,2)
	IF cycle <> 'CU' AND cycle <> 'PB' AND cycle <> 'PU' AND cycle <> 'RV' THEN
		RETURN 1
	END IF
ELSEIF DWO.Name = "ttlinit_ajyfn" THEN
	String ajyfn
	ajyfn = Left(Data,2)
	IF ajyfn <> 'AF' AND ajyfn <> 'AN' AND ajyfn <> 'JF' AND ajyfn <> 'JN' AND &
		ajyfn <> 'YF' AND ajyfn <> 'YN' THEN 
		RETURN 1
	ELSE
		IF dw_title.object.ttlinit_casub[1] <> "" THEN
			IF (wf_val_ajyfn_casub(GetText(),dw_title.object.ttlinit_casub[1]) = FALSE) THEN
				Messagebox("ERROR","Invalid ~'AJYFN~' Code against ~'CASUB~' Code.",stopSign!)
				RETURN 0
			END IF
		END IF
	END IF
//ELSEIF dwo.Name = "ttlinit_lang" THEN
//	string lang
//	lang = left(data,3)
//	IF lang <> 'ENG' AND lang <> 'SPA' AND lang <> 'ARA' AND lang <> 'CHI' AND &
//		lang <> 'DAN' AND lang <> 'FRE' AND lang <> 'GER' AND lang <> 'HAU' AND &
//		lang <> 'OFL' AND lang <> 'FIN' AND lang <> 'DUT' 							THEN
//		RETURN 1
//	END IF
ELSEIF DWO.Name = "ttlinit_pubyr" THEN
	IF Len(Data) < 4 THEN
		RETURN 1
	END IF
ELSEIF DWO.Name = "ttlinit_isbn" THEN
	
	String ls_isbn, ls_char 
	Int isbn_length
	
	ls_isbn =Data
	ls_isbn =Trim(ls_isbn)
	
	isbn_length = Len(ls_isbn)
	
	IF isbn_length >13 THEN
		dw_title.object.ttlinit_isbn.Validationmsg ='ISBN # MUST BE AT MOST 13 CHARACTERS'
		RETURN 1
	ELSEIF isbn_length < 10 THEN
		dw_title.object.ttlinit_isbn.Validationmsg ='ISBN # MUST BE AT LEAST 10 CHARACTERS'
		RETURN 1
	END IF
	
	FOR i=1 TO isbn_length - 1
		ls_char =Mid(ls_isbn,i,1)
		IF  (ls_char> '9' OR ls_char<'0') THEN
			dw_title.object.ttlinit_isbn.Validationmsg ='You must use digit except the last character' 
			RETURN 1
		END IF
	NEXT
	
	ls_char =Mid(ls_isbn,isbn_length,1)
	IF  (ls_char >'Z' OR ls_char <'A') AND (ls_char >'9' OR ls_char< '0')THEN
		dw_title.object.ttlinit_isbn.Validationmsg ='The last character should be in set (A...Z) '+ &
		' or in the set (1...9)'
		RETURN 1
	END IF
	
END IF
IF DWO.Name='ttlinit_auth' OR DWO.Name='ttlinit_authfn' THEN
	ls_conno=dw_title.object.mchar_conno[1]
	IF (ls_conno='' OR IsNull(ls_conno)) THEN RETURN
	SELECT count(*) INTO :li_cnt
	FROM mchar
	where conno=:ls_conno
	USING SqlServerTrans;
	IF NOT f_check_dberror(SqlServerTrans,'select count(*) from mchar using ls_conno') THEN
		RETURN
	END IF
	IF li_cnt=0 THEN RETURN
	ls_crnameold=dw_title.object.ccrname[1]
	IF ls_crnameold<> 'Public Damain' THEN
		IF DWO.Name='ttlinit_auth' THEN
			li_re=Messagebox('','Author last name changed, Do you want copyright name '+&
				'~nto be changed because of this change?',exclamation!,yesNo!,1)
			IF li_re=1 THEN
				ls_fn=dw_title.object.ttlinit_authfn[1]
				IF IsNull(ls_fn) THEN ls_fn=''
				ls_crname=f_combine_string1_string2(ls_fn, Data)
				IF Len(ls_crname) > 40 THEN
					li_len = Len(ls_crname) - 40
					Messagebox("Copyright length", "Please reduce the size of the copyright length by "+String(li_len)+" characters, "+ls_crname)
				END IF
				dw_title.object.ccrname[1]= ls_crname
			END IF
		ELSEIF DWO.Name='ttlinit_authfn' THEN
			li_re=Messagebox('','Author first name changed, Do you want copyright name'+&
				'~nto be changed because of this change?',exclamation!,yesNo!,1)
			IF li_re=1 THEN
				ls_ln=dw_title.object.ttlinit_auth[1]
				IF IsNull(ls_ln) THEN ls_ln=''
				ls_crname=f_combine_string1_string2(Data, ls_ln)
				IF Len(ls_crname) > 40 THEN
					li_len = Len(ls_crname) - 40
					Messagebox("Copyright length", "Please reduce the size of the copyright length by "+String(li_len)+" characters, "+ls_crname)
				END IF
				dw_title.object.ccrname[1]= ls_crname
			END IF
		END IF
	END IF// end if 'Public Domain'
END IF// ttlinit_auth or ttlinit_authfn





end event

event itemfocuschanged;call super::itemfocuschanged;string Lauth
string Lauthfn

IF IsValid(inv_dropdownsearch) THEN
	this.inv_dropdownsearch.Event pfc_ItemFocusChanged(row,dwo)
END IF
IF dwo.tag <> "" THEN
	SetMicroHelp(w_pics_main,dwo.tag)
ELSE
	SetMicroHelp(w_pics_main,"")
END IF

//IF dwo.name = "ttlinit_isbn" THEN
//	String Lisbn
//	Lisbn = RightTrim(GetText())
//	IF Len(Lisbn) < 10 THEN
//		RETURN 1
//	END IF
//END IF
//
end event

event rbuttondown;//
end event

event rbuttonup;//
end event

event doubleclicked;call super::doubleclicked;IF dwo.name = "ttlinit_publisher" THEN
	cb_pub.TriggerEvent(Clicked!)
END IF
end event

event ue_postconstructor;call super::ue_postconstructor;dw_title.of_SetTransObject( SQLServerTrans )
this.of_SetDropDownSearch(TRUE)
this.inv_dropdownsearch.of_AddColumn("ttlinit_publisher")

end event

event pfc_deleterow;//
RETURN 1
end event

event pfc_addrow;//
RETURN 1
end event

event pfc_insertrow;//
RETURN 1
end event

event constructor;call super::constructor;string ls_table_name
string ls_key_columns[]
string ls_updatable_columns[]

dw_title.of_SetLinkage(TRUE)
dw_title.of_SetTransObject(SQLServerTrans)
dw_title.of_SetMultiTable(TRUE)
dw_title.of_SetDropDownSearch(TRUE)
dw_title.inv_dropdownsearch.of_AddColumn("ttlinit_lang")
dw_title.inv_dropdownsearch.of_AddColumn("ttlinit_publisher")

ls_table_name="mchar"
ls_key_columns[]={"mchar_conno","mchar_conno"}
ls_updatable_columns[]={"mchar_conno","mchar_parprt","ttlinit_chno"}

dw_title.inv_multitable.of_addtoupdate(ls_table_name,ls_key_columns,ls_updatable_columns,TRUE,2)

ls_table_name="ttlinit"
ls_key_columns[]={"ttlinit_chno","ttlinit_chno"}
ls_updatable_columns[]={"ttlinit_chno","ttlinit_aepcd","ttlinit_auth","ttlinit_sauth",&
            "ttlinit_authfn", &
				"ttlinit_ahonorific","ttlinit_ttlart","ttlinit_ttl", "ttlinit_sttl", &
				"ttlinit_ttldt","ttlinit_publisher","ttlinit_pubyr","ttlinit_lang", &
				"ttlinit_isbn","ttlinit_casub","ttlinit_ajyfn","ttlinit_cycle", &
				"ttlinit_srcdoc","ccrflag","ccrname","ttlinit_soundex_ttl","ttlinit_soundex_auth"}
dw_title.inv_multitable.of_addtoupdate(ls_table_name,ls_key_columns,ls_updatable_columns,TRUE,2)

this.of_SetPrintPreview(TRUE)
end event

event editchanged;call super::editchanged;
IF IsValid(inv_dropdownsearch) THEN
	this.inv_dropdownsearch.Event pfc_editchanged(row,dwo,data)
END IF


end event

event sqlpreview;call super::sqlpreview;//messagebox("sql",sqlsyntax)
end event

type dw_coauthor from u_pics_dw within w_sheet_initial_title
event ue_enterkey pbm_dwnprocessenter
event pfc_hinttext pbm_mousemove
integer x = 41
integer y = 1300
integer width = 2743
integer height = 376
integer taborder = 50
string dataobject = "d_coauthor"
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

event pfc_addrow;long	ll_rc
string Lchno

// Insert row
if IsValid (inv_rowmanager) then
	ll_rc = inv_rowmanager.event pfc_addrow ()
else
	ll_rc = this.InsertRow (0)
	Lchno = dw_title.object.ttlinit_chno[1]
	dw_coauthor.object.chno[ll_rc] = Lchno
end if

// Notify the Linkage Service that a new row has been added.
IF IsValid ( inv_Linkage ) THEN 
	inv_Linkage.Event pfc_InsertRow (ll_rc) 
END IF 

this.ScrollToRow(ll_rc)
this.SetColumn("coauth")

return ll_rc
end event

event pfc_insertrow;long	ll_currow
long	ll_rc
string Lchno

// Get current row
ll_currow = this.GetRow()
if ll_currow < 0 then
	ll_currow = 0
end if

// Insert row
if IsValid (inv_rowmanager) then
	ll_rc = inv_rowmanager.event pfc_insertrow (ll_currow)
else
	ll_rc = this.InsertRow (ll_currow) 
	Lchno = dw_title.object.ttlinit_chno[1]
	dw_coauthor.object.chno[ll_rc] = Lchno
end if

// Notify the Linkage Service that a new row has been added.
IF IsValid ( inv_Linkage ) THEN 
	inv_Linkage.Event pfc_InsertRow (ll_rc) 
END IF 

return ll_rc
end event

event ue_postconstructor;call super::ue_postconstructor;dw_coauthor.of_SetTransObject( SQLServerTrans )

end event

event itemchanged;call super::itemchanged;String Lchno
Lchno = dw_title.object.ttlinit_chno[1]
dw_coauthor.object.chno[row]= Lchno

end event

event sqlpreview;call super::sqlpreview;//MessageBox("sql",sqlsyntax)
end event

event constructor;call super::constructor;this.of_SetPrintPreview(TRUE)
end event

type st_1 from statictext within w_sheet_initial_title
integer x = 82
integer y = 1144
integer width = 247
integer height = 76
boolean bringtotop = true
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 33554432
long backcolor = 78164112
boolean enabled = false
string text = "Sources"
alignment alignment = right!
boolean focusrectangle = false
end type

type cb_pub from commandbutton within w_sheet_initial_title
event ue_hinttext pbm_mousemove
string tag = "Add/Update Publisher information"
integer x = 73
integer y = 1720
integer width = 411
integer height = 108
boolean bringtotop = true
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "&Add Publisher..."
end type

event ue_hinttext;SetMicroHelp(w_pics_main,this.tag)
end event

event clicked;open(w_add_publisher)

end event

type dw_src1 from u_pics_dw within w_sheet_initial_title
event ue_enterkey pbm_dwnprocessenter
event ue_hinttext pbm_mousemove
integer x = 366
integer y = 1120
integer width = 805
integer height = 96
integer taborder = 20
string dataobject = "d_src"
boolean vscrollbar = false
boolean livescroll = false
boolean ib_isupdateable = false
boolean ib_rmbmenu = false
boolean ib_rmbfocuschange = false
string is_updatesallowed = ""
end type

event ue_hinttext;call super::ue_hinttext;string ls_object, ls_column, ls_column_tag
long ll_pos

//This script set's microhelp For Copyright Permission Datawindow (Upper)
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

event itemchanged;call super::itemchanged;string ls_data, ls_src

ls_data=righttrim(data)
ls_src=this.object.src_code[row]
src_chng=TRUE
dw_src1.Object.src_code.TabSequence='60'
end event

event ue_postconstructor;call super::ue_postconstructor;dw_src1.of_SetTransObject( sqlservertrans)

end event

type dw_src2 from u_pics_dw within w_sheet_initial_title
event ue_enterkey pbm_dwnprocessenter
event ue_hinttext pbm_mousemove
string tag = "Please Choose a Source. To seperate the sources, please terminate it with semicolon. Ex: BW 9/9/97;"
integer x = 1170
integer y = 1120
integer width = 805
integer height = 96
integer taborder = 30
boolean bringtotop = true
string dataobject = "d_src"
boolean vscrollbar = false
boolean livescroll = false
boolean ib_isupdateable = false
boolean ib_rmbmenu = false
boolean ib_rmbfocuschange = false
string is_updatesallowed = ""
end type

event ue_enterkey;call super::ue_enterkey;//dw_src3.SetFocus()
end event

event ue_hinttext;call super::ue_hinttext;string ls_object, ls_column, ls_column_tag
long ll_pos

//This script set's microhelp For Copyright Permission Datawindow (Upper)
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

event itemchanged;call super::itemchanged;righttrim(data)
src_chng=TRUE
end event

event rbuttondown;//
end event

event rbuttonup;//
end event

event ue_postconstructor;call super::ue_postconstructor;dw_src2.of_SetTransObject( sqlservertrans)

end event

type dw_src3 from u_pics_dw within w_sheet_initial_title
event ue_enterkey pbm_dwnprocessenter
event ue_hinttext pbm_mousemove
string tag = "Please Choose a Source. To seperate the sources, please terminate it with semicolon. Ex: BW 9/9/97;"
integer x = 1975
integer y = 1120
integer width = 805
integer height = 96
integer taborder = 40
boolean bringtotop = true
string dataobject = "d_src"
boolean vscrollbar = false
boolean livescroll = false
boolean ib_isupdateable = false
boolean ib_rmbmenu = false
boolean ib_rmbfocuschange = false
string is_updatesallowed = ""
end type

event ue_enterkey;call super::ue_enterkey;//dw_coauthor.SetFocus()
end event

event ue_hinttext;call super::ue_hinttext;string ls_object, ls_column, ls_column_tag
long ll_pos

//This script set's microhelp For Copyright Permission Datawindow (Upper)
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

event itemchanged;call super::itemchanged;righttrim(data)
src_chng=TRUE
end event

event rbuttondown;//
end event

event rbuttonup;//
end event

event ue_postconstructor;call super::ue_postconstructor;dw_src3.of_SetTransObject( sqlservertrans)

end event

type cb_srcs from commandbutton within w_sheet_initial_title
event ue_hinttext pbm_mousemove
string tag = "Add/Update Source information"
integer x = 521
integer y = 1720
integer width = 411
integer height = 108
boolean bringtotop = true
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "Add &Sources..."
end type

event ue_hinttext;SetMicroHelp(w_pics_main,this.tag)
end event

event clicked;open(w_add_sources)
end event


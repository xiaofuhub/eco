$PBExportHeader$w_sheet_bcs_search.srw
forward
global type w_sheet_bcs_search from w_sheet
end type
type pb_clear from commandbutton within w_sheet_bcs_search
end type
type pb_find from commandbutton within w_sheet_bcs_search
end type
type pb_exit from commandbutton within w_sheet_bcs_search
end type
type st_displayed from statictext within w_sheet_bcs_search
end type
type st_1 from statictext within w_sheet_bcs_search
end type
type cb_cancel from commandbutton within w_sheet_bcs_search
end type
type cb_sum from commandbutton within w_sheet_bcs_search
end type
type dw_bcs_search_sum from u_pics_dw within w_sheet_bcs_search
end type
type dw_bcs_search from u_pics_dw within w_sheet_bcs_search
end type
end forward

global type w_sheet_bcs_search from w_sheet
integer x = 5
integer y = 88
integer width = 2880
integer height = 1280
string title = "PICS BCS Search (View Only)"
pb_clear pb_clear
pb_find pb_find
pb_exit pb_exit
st_displayed st_displayed
st_1 st_1
cb_cancel cb_cancel
cb_sum cb_sum
dw_bcs_search_sum dw_bcs_search_sum
dw_bcs_search dw_bcs_search
end type
global w_sheet_bcs_search w_sheet_bcs_search

type variables
string mod_string
boolean ib_cancel=FALSE
end variables

forward prototypes
public subroutine wf_set_counts ()
public subroutine wf_modify_title ()
public subroutine wf_cancel ()
public function integer wf_retrieve ()
end prototypes

public subroutine wf_set_counts ();st_displayed.text=String(dw_bcs_search.RowCount())
end subroutine

public subroutine wf_modify_title ();String Lttl
	
Lttl  = dw_bcs_search.GetItemString(dw_bcs_search.GetRow(),"cttl")
	
IF Lttl <> "" THEN
		
	String Lchno,subttl,sub1,temp="xyz"
	int lcount,i
		
	Lchno  = dw_bcs_search.GetItemString(dw_bcs_search.GetRow(),"cchno")
	
	// Get the subtitles and concatenate it into one string.
	DECLARE proc_ret_sttl PROCEDURE FOR
				ret_sttl(:Lchno)
	USING sqlservertrans; 
			
	EXECUTE proc_ret_sttl;

	DO WHILE (temp <> sub1)
		temp = sub1
		FETCH proc_ret_sttl INTO :sub1;
		IF temp <> sub1 THEN
			sub1=RightTrim(sub1)
			subttl=subttl+" "+sub1
		END IF
	LOOP 
		
	CLOSE proc_ret_sttl;
		
	Lttl = RightTrim(Lttl) + " " + RightTrim(subttl)
		
	dw_bcs_search.SetItem(dw_bcs_search.GetRow(), "cttl", Lttl)
	dw_bcs_search.SetItemStatus(dw_bcs_search.GetRow(),"cttl",Primary!, DataModified!)
END IF
end subroutine

public subroutine wf_cancel ();ib_cancel = TRUE
//dw_bcs_search.DBCancel( )
cb_cancel.Visible=FALSE
end subroutine

public function integer wf_retrieve ();int rc
ib_cancel = FALSE
sqlservertrans.DBParm = 'Async = 1'
cb_cancel.Visible=TRUE
rc = dw_bcs_search.Retrieve( )
return rc
end function

event key;call super::key;IF KeyDown(KeyEscape!) THEN 
  pb_exit.TriggerEvent(Clicked!)
END IF

end event

event mousemove;call super::mousemove;SetMicroHelp(w_pics_main,this.tag)
end event

on w_sheet_bcs_search.create
int iCurrent
call super::create
this.pb_clear=create pb_clear
this.pb_find=create pb_find
this.pb_exit=create pb_exit
this.st_displayed=create st_displayed
this.st_1=create st_1
this.cb_cancel=create cb_cancel
this.cb_sum=create cb_sum
this.dw_bcs_search_sum=create dw_bcs_search_sum
this.dw_bcs_search=create dw_bcs_search
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.pb_clear
this.Control[iCurrent+2]=this.pb_find
this.Control[iCurrent+3]=this.pb_exit
this.Control[iCurrent+4]=this.st_displayed
this.Control[iCurrent+5]=this.st_1
this.Control[iCurrent+6]=this.cb_cancel
this.Control[iCurrent+7]=this.cb_sum
this.Control[iCurrent+8]=this.dw_bcs_search_sum
this.Control[iCurrent+9]=this.dw_bcs_search
end on

on w_sheet_bcs_search.destroy
call super::destroy
destroy(this.pb_clear)
destroy(this.pb_find)
destroy(this.pb_exit)
destroy(this.st_displayed)
destroy(this.st_1)
destroy(this.cb_cancel)
destroy(this.cb_sum)
destroy(this.dw_bcs_search_sum)
destroy(this.dw_bcs_search)
end on

event resize;call super::resize;long ll_height

This.X = w_pics_main.X
This.Y = w_pics_main.Y
ll_height = w_pics_main.mdi_1.Height
This.Resize(w_pics_main.WorkSpaceWidth(), ll_height)


end event

event pfc_preopen;call super::pfc_preopen;this.of_SetPreference(TRUE)
this.inv_preference.of_SetToolBars(TRUE)
this.inv_preference.of_SetWindow(TRUE)
this.of_SetResize(TRUE)
this.inv_resize.of_SetOrigSize(this.workspacewidth(),this.workspaceheight())
inv_resize.of_Register(dw_bcs_search, "Scale")
inv_resize.of_Register(dw_bcs_search_sum, "Scale")
inv_resize.of_Register(cb_cancel, "Scale")
inv_resize.of_Register(cb_sum, "Scale")
inv_resize.of_Register(pb_exit, "Scale")
inv_resize.of_Register(pb_find, "Scale")
inv_resize.of_Register(pb_clear, "Scale")
inv_resize.of_Register(st_1, "Scale")
inv_resize.of_Register(st_displayed, "Scale")

end event

event open;call super::open;m_pics_main.m_file.m_print.Enabled 			=	TRUE
m_pics_main.m_file.m_pagesetup.Enabled		=	TRUE
m_pics_main.m_file.m_printimmediate.Enabled	=	TRUE
this.windowstate = maximized!
w_sheet_bcs_search.pb_find.Event Clicked()
end event

type pb_clear from commandbutton within w_sheet_bcs_search
event mousemove pbm_mousemove
string tag = "Clear"
integer x = 2176
integer y = 968
integer width = 187
integer height = 108
integer taborder = 50
integer textsize = -8
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "C&lear"
end type

event clicked;dw_bcs_search.Reset()

end event

type pb_find from commandbutton within w_sheet_bcs_search
event clicked pbm_bnclicked
event mousemove pbm_mousemove
string tag = "Find"
integer x = 2400
integer y = 968
integer width = 187
integer height = 108
integer taborder = 20
integer textsize = -8
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "F&ind"
end type

event clicked;string rtn,rc,Lconno,Linit
long ll_rows
integer res


// IF find is dispalyed on the push button
IF pb_find.BringToTop = FALSE THEN
	// Turn on query mode so user can specify data
	rtn = dw_bcs_search.Modify("DataWindow.QueryMode=YES")
	IF rtn = "" THEN
		// If Modify succeeds, show Execute,
		// Query mode is on and display sort CheckBox
		This.BringToTop = TRUE
		This.Text = "Exec&ute"
		dw_bcs_search.SetFocus()
	   SetMicroHelp(w_pics_main,"Query Mode...")
	ELSE
		MessageBox("Error", "Can't access query mode to select data.")
	END IF
ELSE
	dw_bcs_search.AcceptText()
	// Turn off Query mode and retrieve data 
	// based on user's choices
	rtn = dw_bcs_search.Modify("DataWindow.QueryMode=NO")
	IF rtn = "" THEN
		// If Modify succeeds, show Find,
		// Query mode is off, and retrieve data
		This.BringToTop = FALSE
		This.Text = "F&ind"
		Open(w_pics_retrieve_msg_box)
		SetPointer(HourGlass!)
		ll_rows=wf_Retrieve()
		IF ll_rows > 0 THEN 
			// If any rows were retrieved.
      	SetMicroHelp(w_pics_main,"")
			wf_modify_title()
      	wf_set_counts()
			dw_bcs_search.Resetupdate()
			cb_cancel.Visible=FALSE
			IF dw_bcs_search.Sharedata(dw_bcs_search_sum) = -1 THEN
   			MessageBox("ERROR", "Error sharing data between datawindows.",StopSign!, OK!, 2)
			END IF
		ELSE
			MessageBox("ERROR","No rows found")
			// If no rows were retrieved, ask if they want to continue with retrieval.
      	SetMicroHelp(w_pics_main,"")
		END IF // ll_rows > 0
		SetPointer(Arrow!)
		Close(w_pics_retrieve_msg_box)
	ELSE
		MessageBox("Error","Failure exiting query mode.")
	END IF // rtn = ""
END IF // if pb_find...
end event

type pb_exit from commandbutton within w_sheet_bcs_search
event clicked pbm_bnclicked
event mousemove pbm_mousemove
string tag = "Exit"
integer x = 2629
integer y = 968
integer width = 187
integer height = 108
integer taborder = 50
integer textsize = -8
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "E&xit"
end type

event clicked;ib_disableclosequery=TRUE
close(parent)


end event

type st_displayed from statictext within w_sheet_bcs_search
integer x = 773
integer y = 968
integer width = 183
integer height = 72
integer textsize = -8
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long backcolor = 16777215
boolean enabled = false
boolean border = true
borderstyle borderstyle = stylelowered!
boolean focusrectangle = false
end type

type st_1 from statictext within w_sheet_bcs_search
integer x = 398
integer y = 968
integer width = 361
integer height = 72
integer textsize = -8
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long backcolor = 79741120
boolean enabled = false
string text = "Rows Selected:"
boolean focusrectangle = false
end type

type cb_cancel from commandbutton within w_sheet_bcs_search
boolean visible = false
integer x = 1568
integer y = 968
integer width = 517
integer height = 108
integer taborder = 40
boolean bringtotop = true
integer textsize = -8
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "Cancel Retrieve"
end type

event clicked;wf_cancel( )
end event

type cb_sum from commandbutton within w_sheet_bcs_search
integer x = 50
integer y = 968
integer width = 288
integer height = 108
integer taborder = 30
boolean bringtotop = true
integer textsize = -8
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "Summary..."
end type

event clicked;IF (dw_bcs_search_sum.visible = TRUE) THEN
	dw_bcs_search_sum.visible = FALSE
	cb_sum.text ='Summary...'
ELSE
	dw_bcs_search_sum.visible = TRUE
	cb_sum.text ='Detail...'
END IF
end event

type dw_bcs_search_sum from u_pics_dw within w_sheet_bcs_search
boolean visible = false
integer y = 28
integer width = 2811
integer height = 884
integer taborder = 2
string dataobject = "d_bcs_search_sum"
end type

event constructor;call super::constructor;this.of_SetFilter(TRUE)
this.of_SetFind(TRUE)
this.inv_filter.of_SetStyle(1)
this.inv_filter.of_SetColumnNameSource(2)
this.of_SetSort(TRUE)
this.inv_sort.of_SetStyle(1)
this.inv_sort.of_SetColumnNameSource(2)

end event

type dw_bcs_search from u_pics_dw within w_sheet_bcs_search
event itemfocuschanged pbm_dwnitemchangefocus
event rbuttondown pbm_dwnrbuttondown
event rbuttonup pbm_dwnrbuttonup
event ue_postconstructor ( )
event ue_enterkey pbm_dwnprocessenter
integer x = 5
integer y = 32
integer width = 2811
integer height = 884
integer taborder = 10
string dataobject = "d_bcs_search"
boolean livescroll = false
end type

event rbuttondown;//
end event

event rbuttonup;//
end event

event ue_postconstructor;call super::ue_postconstructor;dw_bcs_search.SetTransObject( SQLServerTrans )

end event

event ue_enterkey;call super::ue_enterkey;Send(Handle(this),256,9,Long(0,0))
return(1)
end event

event sqlpreview;call super::sqlpreview;//messagebox("sql",sqlsyntax)
end event

event retrievestart;call super::retrievestart;IF ib_cancel = TRUE THEN
	RETURN 1
END IF
end event

event constructor;call super::constructor;this.of_SetFilter(TRUE)
this.of_SetFind(TRUE)
this.inv_filter.of_SetStyle(1)
this.inv_filter.of_SetColumnNameSource(2)
this.of_SetSort(TRUE)
this.inv_sort.of_SetStyle(1)
this.inv_sort.of_SetColumnNameSource(2)

end event

event retrieveend;call super::retrieveend;IF ib_cancel = TRUE THEN
	RETURN 1
END IF
end event


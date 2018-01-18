$PBExportHeader$k90_page1.sru
$PBExportComments$(K90) - PFC Extender Step 1:  Specify application object
forward
global type k90_page1 from k90_page
end type
type cb_browse from base_browsebutton within k90_page1
end type
type st_instruct from statictext within k90_page1
end type
type st_dragtext from statictext within k90_page1
end type
type cbx_skip from checkbox within k90_page1
end type
type sle_targetfile from singlelineedit within k90_page1
end type
end forward

global type k90_page1 from k90_page
string tag = "Specify Application Object"
cb_browse cb_browse
st_instruct st_instruct
st_dragtext st_dragtext
cbx_skip cbx_skip
sle_targetfile sle_targetfile
end type
global k90_page1 k90_page1

type variables
/*  Name of application object  */
string is_applib

long il_selecteditem
end variables

forward prototypes
public function boolean of_checkrequired ()
end prototypes

public function boolean of_checkrequired ();//*-------------------------------------------------------------------
//*  Function:			of_CheckRequired
//*  Purpose:			Is App Object selected, or Skip chosen ?
//*-------------------------------------------------------------------
If cbx_skip.Checked Then Return True

IF sle_targetfile.Text = "" THEN Return False

If Not FileExists ( sle_targetfile.Text ) Then Return False 

Return True
end function

event constructor;call super::constructor;//*-------------------------------------------------------------------
//*  Event:				constructor
//*  Purpose:			Set Focus Control
//*-------------------------------------------------------------------
/*  Set control that should get focus first  */
idrg_focus = sle_targetfile
end event

event ue_getfocus;call super::ue_getfocus;//*-------------------------------------------------------------------
//*  Event:				ue_getfocus
//*  Purpose:			Populate saved list from the Registry
//*-------------------------------------------------------------------
SetPointer ( HourGlass! )

/*  Step 1, no back available  */
iw_parent.Event ue_back ( FALSE ) 

/*  If everything is complete, then enable Next  */
iw_parent.Event ue_next ( of_CheckRequired ( ) ) 

Return 1
end event

on k90_page1.create
int iCurrent
call super::create
this.cb_browse=create cb_browse
this.st_instruct=create st_instruct
this.st_dragtext=create st_dragtext
this.cbx_skip=create cbx_skip
this.sle_targetfile=create sle_targetfile
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.cb_browse
this.Control[iCurrent+2]=this.st_instruct
this.Control[iCurrent+3]=this.st_dragtext
this.Control[iCurrent+4]=this.cbx_skip
this.Control[iCurrent+5]=this.sle_targetfile
end on

on k90_page1.destroy
call super::destroy
destroy(this.cb_browse)
destroy(this.st_instruct)
destroy(this.st_dragtext)
destroy(this.cbx_skip)
destroy(this.sle_targetfile)
end on

event ue_losefocus;call super::ue_losefocus;//*-------------------------------------------------------------------
//*  Event:				ue_losefocus 
//*  Purpose:			Store library list
//*-------------------------------------------------------------------
IF ib_changed THEN
	SetPointer ( HourGlass! )
	ib_changed = FALSE
	Return iw_parent.of_SetTarget ( sle_targetfile.Text, cbx_skip.Checked ) 
ELSE
	Return 1
END IF 
end event

event destructor;call super::destructor;//*-------------------------------------------------------------------
//*  Event:				destructor
//*  Purpose:			Clean-Up
//*-------------------------------------------------------------------
end event

type cb_browse from base_browsebutton within k90_page1
integer x = 1577
integer y = 232
integer taborder = 20
end type

event clicked;call super::clicked;//*-----------------------------------------------------------------*/
//*    clicked:  Browse for target file name
//*-----------------------------------------------------------------*/
string ls_filename, ls_file
int li_rc

ls_filename = sle_targetfile.Text
li_rc = GetFileOpenName ( "Select Target File", &
			ls_filename, ls_file, "pbt", &
			"PowerBuilder Target (*.pbt),*.pbt" )

If li_rc = 1 Then
	sle_targetfile.Text = ls_filename
	sle_targetfile.TriggerEvent ( Modified! ) 
End If
end event

type st_instruct from statictext within k90_page1
integer x = 41
integer y = 28
integer width = 1591
integer height = 132
boolean bringtotop = true
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 33554432
long backcolor = 67108864
boolean enabled = false
string text = "Please pick a target file containing the application to use in building the new P&FC extensions."
boolean focusrectangle = false
end type

type st_dragtext from statictext within k90_page1
boolean visible = false
integer x = 1367
integer y = 1036
integer width = 247
integer height = 76
boolean bringtotop = true
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 33554432
long backcolor = 67108864
boolean enabled = false
boolean focusrectangle = false
end type

type cbx_skip from checkbox within k90_page1
integer x = 46
integer y = 396
integer width = 1189
integer height = 88
integer taborder = 30
integer textsize = -8
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long backcolor = 79416533
string text = "&Skip, let me build extensions without an application"
end type

event clicked;//*-------------------------------------------------------------------
//*  Event:				clicked
//*  Purpose:			Determine if Next button can be enabled
//*-------------------------------------------------------------------
/*  Clear application information  */
ib_changed = TRUE
//is_applib = ""

IF Not this.Checked THEN
	//tv_filelist.Event Clicked ( il_selecteditem ) 
END IF

/*  Can 'NEXT' button be enabled ?  */
iw_parent.Event ue_next ( of_CheckRequired ( ) )
end event

type sle_targetfile from singlelineedit within k90_page1
integer x = 46
integer y = 232
integer width = 1531
integer height = 88
integer taborder = 10
integer textsize = -8
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 33554432
integer accelerator = 99
boolean displayonly = true
borderstyle borderstyle = stylelowered!
end type

event modified;//*-------------------------------------------------------------------
//*  Event:				Modified
//*  Purpose:			Determine if Next button can be enabled
//*-------------------------------------------------------------------
ib_changed = TRUE
//is_applib = this.Text

//IF this.Text > "" THEN cbx_skip.Checked = FALSE

/*  Can 'NEXT' button be enabled ?  */
iw_parent.Event ue_next ( of_CheckRequired ( ) )
end event


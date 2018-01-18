$PBExportHeader$k90_page6.sru
$PBExportComments$(K90) - PFC Extender Step 6:  Specify the location of new files
forward
global type k90_page6 from k90_page
end type
type dw_location from datawindow within k90_page6
end type
type st_instruct from statictext within k90_page6
end type
end forward

global type k90_page6 from k90_page
string tag = "Specify the Location of Files"
dw_location dw_location
st_instruct st_instruct
end type
global k90_page6 k90_page6

type variables
boolean ib_warnedonce
end variables

forward prototypes
public function boolean of_checkrequired ()
end prototypes

public function boolean of_checkrequired ();int li_files, li_f, li_rc, li_warnedonce
string ls_filename

li_files = dw_location.RowCount ( ) 

FOR li_f = 1 to li_files
	li_warnedonce = dw_location.GetItemNumber ( li_f, "warned" ) 
	IF li_warnedonce = 0 THEN
		ls_filename = dw_location.GetItemString ( li_f, "LibraryName" ) 
		IF FileExists ( ls_filename ) THEN
			li_rc = MessageBox ( "Library Exists", ls_filename + " already exists!  Okay to overwrite?", &
					Question!, YesNoCancel!, 1 )
					
			CHOOSE CASE li_rc 
	
				CASE 1
					dw_location.SetItem ( li_f, "warned", 1 ) 
					Continue
	
				CASE 2
					dw_location.SetRow ( li_f ) 
					dw_location.SetFocus ( )
					Return FALSE
	
				CASE 3
					Return FALSE
	
			END CHOOSE
		END IF 

	END IF

NEXT

Return TRUE
end function

on k90_page6.create
int iCurrent
call super::create
this.dw_location=create dw_location
this.st_instruct=create st_instruct
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_location
this.Control[iCurrent+2]=this.st_instruct
end on

on k90_page6.destroy
call super::destroy
destroy(this.dw_location)
destroy(this.st_instruct)
end on

event ue_getfocus;call super::ue_getfocus;//*-------------------------------------------------------------------
//*  Event:				ue_getfocus
//*  Purpose:			Populate the list of library names
//*-------------------------------------------------------------------
int li_totlibs 

SetPointer ( HourGlass! ) 

IF as_direction = "next" THEN
	IF iw_parent.of_GetPage6Status ( FALSE ) THEN
		dw_location.DataObject = "k90_librarynames"
		li_totlibs = iw_parent.of_GetLibList (  )
		iw_parent.ids_LibNames.ShareData ( dw_Location )
		dw_location.Filter ( )
	END IF
END IF

dw_location.Object.DataWindow.HorizontalScrollPosition = 1
dw_location.Visible = True

Return 1
end event

event ue_losefocus;call super::ue_losefocus;//*-------------------------------------------------------------------
//*  Event:				ue_losefocus
//*  Purpose:			Save the library name changes
//*-------------------------------------------------------------------
dw_location.AcceptText ( ) 

SetPointer ( HourGlass! ) 

If as_direction = "next" Then 
	IF Not of_CheckRequired ( ) THEN Return -1
End If

IF ib_changed THEN
	dw_location.SetFilter ( "" )
	dw_location.Filter ( )
	Return 1

ELSE
	Return 1
END IF
end event

event constructor;call super::constructor;//*-------------------------------------------------------------------
//*  Event:				constuctor
//*  Purpose:			Initialization
//*-------------------------------------------------------------------
/*  Set the focus object  */
idrg_focus = dw_location
end event

event resize;call super::resize;dw_location.Resize ( newwidth - 150, newheight - 140 )

end event

type dw_location from datawindow within k90_page6
boolean visible = false
integer x = 18
integer y = 144
integer width = 1664
integer height = 976
integer taborder = 1
boolean bringtotop = true
string dataobject = "k90_librarynames"
boolean hscrollbar = true
boolean vscrollbar = true
boolean border = false
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event itemchanged;//*-------------------------------------------------------------------
//*  Event:				itemchanged
//*  Purpose:			User changed the library path
//*-------------------------------------------------------------------
ib_changed = TRUE
this.SetItem ( row, "warned", 0 ) 
end event

event buttonclicked;string ls_filename, ls_file
int li_rc

IF IsNull ( dwo ) THEN Return 
IF Not IsValid ( dwo ) THEN Return

IF row > 0 THEN
	ls_filename = this.GetItemString ( row, "LibraryName" ) 
	li_rc = GetFileSaveName ( "Select Library", ls_filename, ls_file, &
		"PBL", "PowerBuilder Libraries (*.PBL),*.PBL," )
	IF li_rc = 1 THEN
		this.SetItem ( row, "LibraryName", Lower ( ls_filename ) )
		this.SetItem ( row, "warned", 0 ) 
	END IF
END IF
end event

type st_instruct from statictext within k90_page6
integer x = 37
integer y = 16
integer width = 1627
integer height = 128
boolean bringtotop = true
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 33554432
long backcolor = 67108864
boolean enabled = false
string text = "Specify the location where the extensions will be created.   If you accept the defaults, click Next."
boolean focusrectangle = false
end type


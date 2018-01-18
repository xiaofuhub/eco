$PBExportHeader$w_quick_select.srw
forward
global type w_quick_select from window
end type
type cb_save from commandbutton within w_quick_select
end type
type cb_open from commandbutton within w_quick_select
end type
type cb_edit from commandbutton within w_quick_select
end type
type cb_exe from commandbutton within w_quick_select
end type
type st_5 from statictext within w_quick_select
end type
type mle_sql from u_mle within w_quick_select
end type
type rb_grid from radiobutton within w_quick_select
end type
type rb_form from radiobutton within w_quick_select
end type
type rb_tabular from radiobutton within w_quick_select
end type
type cb_addall from commandbutton within w_quick_select
end type
type st_16 from statictext within w_quick_select
end type
type st_15 from statictext within w_quick_select
end type
type st_14 from statictext within w_quick_select
end type
type st_13 from statictext within w_quick_select
end type
type st_12 from statictext within w_quick_select
end type
type st_11 from statictext within w_quick_select
end type
type dw_criteria from datawindow within w_quick_select
end type
type dw_columns from datawindow within w_quick_select
end type
type dw_tables from datawindow within w_quick_select
end type
type st_9 from statictext within w_quick_select
end type
type st_8 from statictext within w_quick_select
end type
type cb_cancel from commandbutton within w_quick_select
end type
type cb_ok from commandbutton within w_quick_select
end type
type st_4 from statictext within w_quick_select
end type
type st_3 from statictext within w_quick_select
end type
type st_2 from statictext within w_quick_select
end type
type st_1 from statictext within w_quick_select
end type
type gb_styl from groupbox within w_quick_select
end type
end forward

global type w_quick_select from window
integer x = 187
integer y = 4
integer width = 2610
integer height = 2008
boolean titlebar = true
string title = "Report Query Painter"
windowtype windowtype = popup!
long backcolor = 79741120
toolbaralignment toolbaralignment = alignatleft!
cb_save cb_save
cb_open cb_open
cb_edit cb_edit
cb_exe cb_exe
st_5 st_5
mle_sql mle_sql
rb_grid rb_grid
rb_form rb_form
rb_tabular rb_tabular
cb_addall cb_addall
st_16 st_16
st_15 st_15
st_14 st_14
st_13 st_13
st_12 st_12
st_11 st_11
dw_criteria dw_criteria
dw_columns dw_columns
dw_tables dw_tables
st_9 st_9
st_8 st_8
cb_cancel cb_cancel
cb_ok cb_ok
st_4 st_4
st_3 st_3
st_2 st_2
st_1 st_1
gb_styl gb_styl
end type
global w_quick_select w_quick_select

type variables
integer il_HighlightedTable, il_HighlightedColumn
String is_visiblecolumns[], is_TableName
String sqlstmt
Boolean dynsql=FALSE


end variables

forward prototypes
public function string wf_replace_underscores_with_spaces (string a_instring)
end prototypes

public function string wf_replace_underscores_with_spaces (string a_instring);// string Function wf_replace_underscores_with_spaces (string a_instring)

// Returns value of a_instring with each underscore replaced by a space character

int		p
string	s_in, s_out

s_in = a_instring
p = PosA ( s_in, '_' )

do while p > 0
	s_out =	s_out + LeftA ( s_in, p -1 ) + ' '
	s_in   = MidA ( s_in, p +1)	
	p = PosA (s_in, '_' )
loop

s_out = s_out + s_in

return  s_out

end function

event open;// Open script for w_quick_select

dw_tables.SetTransObject (sqlservertrans)
dw_columns.SetTransObject (sqlservertrans)

//populate the tables datawindow

dw_tables.Retrieve ()
mle_sql.Enabled = TRUE
mle_sql.SetFocus()
end event

on w_quick_select.create
this.cb_save=create cb_save
this.cb_open=create cb_open
this.cb_edit=create cb_edit
this.cb_exe=create cb_exe
this.st_5=create st_5
this.mle_sql=create mle_sql
this.rb_grid=create rb_grid
this.rb_form=create rb_form
this.rb_tabular=create rb_tabular
this.cb_addall=create cb_addall
this.st_16=create st_16
this.st_15=create st_15
this.st_14=create st_14
this.st_13=create st_13
this.st_12=create st_12
this.st_11=create st_11
this.dw_criteria=create dw_criteria
this.dw_columns=create dw_columns
this.dw_tables=create dw_tables
this.st_9=create st_9
this.st_8=create st_8
this.cb_cancel=create cb_cancel
this.cb_ok=create cb_ok
this.st_4=create st_4
this.st_3=create st_3
this.st_2=create st_2
this.st_1=create st_1
this.gb_styl=create gb_styl
this.Control[]={this.cb_save,&
this.cb_open,&
this.cb_edit,&
this.cb_exe,&
this.st_5,&
this.mle_sql,&
this.rb_grid,&
this.rb_form,&
this.rb_tabular,&
this.cb_addall,&
this.st_16,&
this.st_15,&
this.st_14,&
this.st_13,&
this.st_12,&
this.st_11,&
this.dw_criteria,&
this.dw_columns,&
this.dw_tables,&
this.st_9,&
this.st_8,&
this.cb_cancel,&
this.cb_ok,&
this.st_4,&
this.st_3,&
this.st_2,&
this.st_1,&
this.gb_styl}
end on

on w_quick_select.destroy
destroy(this.cb_save)
destroy(this.cb_open)
destroy(this.cb_edit)
destroy(this.cb_exe)
destroy(this.st_5)
destroy(this.mle_sql)
destroy(this.rb_grid)
destroy(this.rb_form)
destroy(this.rb_tabular)
destroy(this.cb_addall)
destroy(this.st_16)
destroy(this.st_15)
destroy(this.st_14)
destroy(this.st_13)
destroy(this.st_12)
destroy(this.st_11)
destroy(this.dw_criteria)
destroy(this.dw_columns)
destroy(this.dw_tables)
destroy(this.st_9)
destroy(this.st_8)
destroy(this.cb_cancel)
destroy(this.cb_ok)
destroy(this.st_4)
destroy(this.st_3)
destroy(this.st_2)
destroy(this.st_1)
destroy(this.gb_styl)
end on

type cb_save from commandbutton within w_quick_select
integer x = 1797
integer y = 1216
integer width = 187
integer height = 76
integer taborder = 62
integer textsize = -9
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "&Save"
end type

event clicked;//////////////////////////////////////////////////////////////////////
//
// Save the contents of the current sheet to a file
//
//////////////////////////////////////////////////////////////////////

int		li_fileid, &
			li_ret
string 	ls_old_fullname, &
			ls_save_filename, &
			ls_save_pathname,is_fullname,is_filename


SetPointer (HourGlass!)

//////////////////////////////////////////////////////////////////////
// remember original file name. If user tries to change it, see if it
// already exists; if so prompt 'OK to replace?'
//////////////////////////////////////////////////////////////////////
ls_old_fullname = ""
ls_save_filename = ""
ls_save_pathname = ""

if GetFileSaveName ("Save", ls_save_pathname, &
							ls_save_filename,"sql", &
							"Text Files (*.txt),*.txt," + &
							"SQL Files (*.sql),*.sql") < 1 then
	return
end if


//////////////////////////////////////////////////////////////////////
// If user changed the name, check if file already exists
//////////////////////////////////////////////////////////////////////
if ls_save_pathname <> ls_old_fullname then
	if FileExists (ls_save_pathname) then
		li_ret = Messagebox ("ReportBuilder", &
							"Do you want to replace " + ls_save_pathname + "?", &
							question!, YesNoCancel!, 2)
		if li_ret = 3 then return		// User clicked "Cancel"

		do while li_ret = 2				// User clicked "No"
			li_ret = GetFileSaveName ("Save", ls_save_pathname, &
									ls_save_filename,"txt", &
				"Text Files (*.txt),*.txt,INI Files (*.ini),*.ini,Batch Files (*.bat),*.bat")
			if li_ret < 1 then return
			if FileExists (ls_save_pathname) then
				li_ret = Messagebox ("PowerBuilt Notepad", &
							"Do you want to replace " + ls_save_pathname + "?", &
							question!, YesNoCancel!, 2)
				if li_ret = 3 then return	// User clicked "Cancel"
			end if	
		loop			// Repeat as long as user hits No

	end if		// End:  User tried to replace existing file
end if		// End:  User changed file name


//////////////////////////////////////////////////////////////////////
// Write out the contents of the MultiLine Edit to the file from
// which it was read.
//////////////////////////////////////////////////////////////////////
is_fullname = ls_save_pathname
is_filename = ls_save_filename
SetPointer (HourGlass!)

li_fileid = FileOpen (is_fullname, StreamMode!, write!, lockwrite!, Replace!)
FileWrite (li_fileid, mle_sql.text)
FileClose (li_fileid)
end event

type cb_open from commandbutton within w_quick_select
integer x = 1541
integer y = 1216
integer width = 187
integer height = 76
integer taborder = 70
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "&Open"
end type

event clicked;string docname, named

integer value

mle_sql.Enabled = TRUE
mle_sql.text = ""

value = GetFileOpenName("Select File",  &
	+ docname, named, "SQL",  &
	+ "Text Files (*.TXT),*.TXT,"  &
	+ "SQL Files (*.SQL),*.SQL")
	

IF value = 1 THEN 
	int li_fileid
	li_fileid = FileOpen(docname,StreamMode!)
	FileRead (li_fileid, mle_sql.text)
	mle_sql.SetFocus()
	FileClose (li_fileid)
END IF
end event

type cb_edit from commandbutton within w_quick_select
integer x = 2048
integer y = 1216
integer width = 187
integer height = 76
integer taborder = 60
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "&Edit"
end type

event clicked;IF dw_criteria.DataObject <> '' THEN
	mle_sql.text = dw_criteria.getsqlselect()
END IF
mle_sql.Enabled = TRUE
mle_sql.SetFocus()
end event

type cb_exe from commandbutton within w_quick_select
integer x = 2290
integer y = 1212
integer width = 238
integer height = 76
integer taborder = 80
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "E&xecute"
end type

event clicked;/*************************************************************************
		When this button is clicked open up the window to test the
		DataWindow.  Open a new sheet in the frame to create
		the datawindow in.
*************************************************************************/

IF mle_sql.Enabled = TRUE THEN
	SetPointer(HourGlass!)

	window newwindow

	dynsql = TRUE
	sqlstmt = mle_sql.text
	OpenSheet(newwindow,"w_quick_select_sheet", ParentWindow(), 2, Original!)
END IF


end event

type st_5 from statictext within w_quick_select
integer x = 55
integer y = 624
integer width = 2290
integer height = 76
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long backcolor = 12632256
boolean enabled = false
string text = "SQL Statement: (If you are familiar with SQL commands, you can type your statement here and execute it.)"
boolean focusrectangle = false
end type

type mle_sql from u_mle within w_quick_select
integer x = 87
integer y = 700
integer width = 2437
integer height = 492
integer taborder = 90
boolean enabled = false
boolean vscrollbar = true
end type

type rb_grid from radiobutton within w_quick_select
integer x = 2139
integer y = 1460
integer width = 242
integer height = 68
integer textsize = -9
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 41943040
long backcolor = 74481808
string text = "&Grid"
boolean checked = true
end type

type rb_form from radiobutton within w_quick_select
integer x = 2139
integer y = 1528
integer width = 256
integer height = 68
integer textsize = -9
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 41943040
long backcolor = 74481808
string text = "&Form"
end type

type rb_tabular from radiobutton within w_quick_select
integer x = 2139
integer y = 1604
integer width = 302
integer height = 68
integer textsize = -9
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 41943040
long backcolor = 74481808
string text = "&Tabular"
end type

type cb_addall from commandbutton within w_quick_select
integer x = 2112
integer y = 444
integer width = 265
integer height = 100
integer taborder = 50
integer textsize = -9
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
boolean enabled = false
string text = "A&dd All"
end type

event clicked;/************************************************************************
	If the current column was previously selected, Skip it.
	If the Current column was not previoustly selected, selected it and 
		make it visible in the criteria grid.
*************************************************************************/
int			li_current_visibility, li_new_visibility
int			li_DWColumnsRows
string		ls_name, ls_colname, ls_moderr
long		ll_row

SetPointer(HourGlass!)

li_DWColumnsRows = dw_columns.RowCount()

For ll_row = 1 To li_DWColumnsRows

	il_HighLightedColumn = ll_row
	ls_colname = RightTrim(dw_columns.Object.column_name[ll_row])		/*****  Get name of column that was clicked	*****/
	ls_name = ls_colname + '.visible'

	/************************************************************************
		Find out if this column had already been chosen or not, by seeing 
		if it is presently visible in the Criteria grid.
	*************************************************************************/
	li_current_visibility = Integer ( dw_criteria.Describe ( ls_name ) )

	/************************************************************************
		Set New Visibility to the opposite of Current Visibility
	*************************************************************************/
	li_new_visibility = 1 - li_current_visibility

	/************************************************************************
		Select or deselect this column's name in the column list.
	*************************************************************************/
	If li_new_visibility = 1 Then
		dw_columns.SelectRow ( ll_row, TRUE )
		il_HighLightedColumn = ll_row
		if cb_ok.enabled = FALSE then cb_ok.enabled = TRUE
		is_VisibleColumns[UpperBound(is_VisibleColumns) + 1 ] = ls_colname
	End If

	/************************************************************************
		Make this column visible if it is not already in the Criteria grid.
	*************************************************************************/
	if li_new_visibility > 0 then
		ls_name = ls_colname + '.visible="' + String ( li_new_visibility ) + '"'
	end if
	ls_moderr = dw_criteria.Modify ( ls_name )
	IF ls_moderr <> "" THEN
		MessageBox("Error","Modify Failed " + ls_moderr)
		RETURN
	END IF
	dw_criteria.Modify("DataWindow.QuerySort=yes")	
next
mle_sql.text = dw_criteria.getsqlselect()

end event

type st_16 from statictext within w_quick_select
integer x = 37
integer y = 1708
integer width = 261
integer height = 68
integer textsize = -9
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 41943040
long backcolor = 74481808
boolean enabled = false
string text = "Or:"
alignment alignment = right!
boolean focusrectangle = false
end type

type st_15 from statictext within w_quick_select
integer x = 37
integer y = 1640
integer width = 261
integer height = 68
integer textsize = -9
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 41943040
long backcolor = 74481808
boolean enabled = false
string text = "Or:"
alignment alignment = right!
boolean focusrectangle = false
end type

type st_14 from statictext within w_quick_select
integer x = 37
integer y = 1564
integer width = 261
integer height = 76
integer textsize = -9
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 41943040
long backcolor = 74481808
boolean enabled = false
string text = "Or:"
alignment alignment = right!
boolean focusrectangle = false
end type

type st_13 from statictext within w_quick_select
integer x = 37
integer y = 1500
integer width = 261
integer height = 68
integer textsize = -9
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 41943040
long backcolor = 74481808
boolean enabled = false
string text = "Criteria:"
alignment alignment = right!
boolean focusrectangle = false
end type

type st_12 from statictext within w_quick_select
integer x = 37
integer y = 1436
integer width = 261
integer height = 68
integer textsize = -9
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 41943040
long backcolor = 74481808
boolean enabled = false
string text = "Sort:"
alignment alignment = right!
boolean focusrectangle = false
end type

type st_11 from statictext within w_quick_select
integer x = 64
integer y = 1236
integer width = 430
integer height = 68
integer textsize = -9
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 41943040
long backcolor = 74481808
boolean enabled = false
string text = "&Selected Columns:"
boolean focusrectangle = false
end type

type dw_criteria from datawindow within w_quick_select
integer x = 315
integer y = 1344
integer width = 1728
integer height = 540
integer taborder = 100
boolean hscrollbar = true
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type dw_columns from datawindow within w_quick_select
event rbuttonup pbm_rbuttonup
integer x = 1074
integer y = 208
integer width = 965
integer height = 408
integer taborder = 20
string dataobject = "d_column_list"
boolean vscrollbar = true
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event clicked;/************************************************************************
	If the clicked column was previously selected, deselect it and make it 
		invisible in the criteria grid.
	If the clicked column was previoustly not selected, selected it and 
		make it visible in the criteria grid.
*************************************************************************/

Long		ll_ReturnCode
Integer	li_current_visibility, li_new_visibility, li_Index, li_ArrayBound
String	ls_name, ls_colname, ls_moderr

il_HighLightedColumn = Row
ls_colname = RightTrim(This.Object.column_name[row])		/*****  Get name of column that was clicked	*****/
ls_name = ls_colname + '.visible'

/************************************************************************
	Find out If this column had already been chosen or not, by seeing 
	If it is presently visible in the Criteria grid.
*************************************************************************/
li_current_visibility = Integer ( dw_criteria.Describe ( ls_name ) )

/************************************************************************
	Set New Visibility to the opposite of Current Visibility
*************************************************************************/
li_new_visibility = 1 - li_current_visibility

/************************************************************************
	Select or deselect this column's name in the column list.
*************************************************************************/
If li_new_visibility = 1 Then
	selectRow ( Row , True )
	il_HighLightedColumn = Row
	If cb_ok.enabled = False Then cb_ok.enabled = True
	is_VisibleColumns[UpperBound(is_VisibleColumns) + 1 ] = ls_colname
Else
	selectRow ( row, False )
	il_HighLightedColumn = 0
	ll_returncode = GetSelectedRow(0)
	If ll_returncode < 1 Then cb_ok.enabled = False
	li_ArrayBound = UpperBound(is_VisibleColumns)
	For li_Index = 1 to li_ArrayBound
		If is_VisibleColumns[li_Index] = ls_colname Then
			is_VisibleColumns[li_Index] = ""
		End If 
	Next
End If

/************************************************************************
	Make this column visible or invisible in the Criteria grid.
*************************************************************************/
ls_name = ls_colname + '.visible="' + String ( li_new_visibility ) + '"'
ls_moderr = dw_criteria.Modify ( ls_name )
IF ls_moderr <> "" THEN
	MessageBox("Error","Modify Failed " + ls_moderr)
	RETURN
END IF
ls_moderr = dw_criteria.Modify("DataWindow.QuerySort=yes")	
IF ls_moderr <> "" THEN
	MessageBox("Error","Modify Failed " + ls_moderr)
	RETURN
END IF
end event

type dw_tables from datawindow within w_quick_select
event rbuttonup pbm_rbuttonup
integer x = 82
integer y = 208
integer width = 965
integer height = 408
integer taborder = 10
string dataobject = "d_table_list"
boolean vscrollbar = true
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event clicked;Integer		li_index, li_num_of_gobs
String		ls_name, ls_tname , ls_sql_syntax , ls_dw_syntax , ls_errors
String		dw_style , ls_hdgs[] , ls_cols[] , cols_no_underscores[]
String 		ls_emptyarray []

/******************************************************************************************************
	Clear out DataObject name (if any), in case user has already selected a table, and
	now is selecting a different one
 ******************************************************************************************************/

dw_criteria.DataObject = ''
is_visiblecolumns[] = ls_emptyarray[]
/******************************************************************************************************
	Note that the Repository definitions for fonts prevail over the parameters specified
	in dw_style, below.  Thus, the several Modify commands after the Create, below.
******************************************************************************************************/
dw_style =	'style(Type=grid) ' + &
				'Text(font.face="MS Sans Serif"  font.Height=12  font.weight=400 font.family=2' + &
						'font.pitch=2 font.charset=0) ' + &
				'Column(font.face="MS Sans Serif"  font.Height=12  font.weight=400 font.family=2' + &
						'font.pitch=2 font.charset=0) ' 

SetPointer ( HourGlass! )
	
// Highlight this row
selectRow (0, False )			
selectRow (row, True )
il_HighlightedTable = Row

// Get table name
ls_tname = this.object.table_name[row]
is_TableName = ls_tname

dw_columns.SetRedraw ( False )

//  Load up the column names	
dw_columns.Retrieve(ls_tname)	

//	Build generic SELECT statement and generate DataWindow syntax
ls_sql_syntax = 'SELECT * FROM ' + ls_tname
ls_dw_syntax = sqlservertrans.SyntaxFromSQL( ls_sql_syntax, dw_style, ls_errors)

// If no ls_errors (there shouldn't be any), create the Criteria grid-style DataWindow
If LenA(ls_errors) = 0 Then
	Parent.SetRedraw ( False )			/*****  Prevent "flash"	*****/
	cb_addAll.enabled = True

	If dw_criteria.Create (ls_dw_syntax) > 0 Then
		dw_criteria.Modify ('datawindow.Detail.Height=70 ')

		//  Insert 6 rows for Sort & Criteria
		For li_index = 1 to 6						
			dw_criteria.InsertRow ( 0 )
		Next

		//	Get names of heading-text items into hdgs[ ] array, 
		// and names of column items into cols[ ] array.
		li_num_of_gobs= f_parse_obj_string(dw_criteria, ls_hdgs, "text", "header")
		li_num_of_gobs= f_parse_obj_string(dw_criteria, ls_cols, "column", "detail")

		//	For each column (and its heading), set font and other attributes.  Also, make
		//	each of them invisible for now.
 
		For li_index = 1 to li_num_of_gobs
			//	Substitute spaces for underscores in column names.
			cols_no_underscores[li_index] = wf_replace_underscores_with_spaces( ls_cols[li_index] )

			//	Build the Modify command string
			ls_name = 'datawindow.header.Height=70 ' + &
			ls_hdgs[li_index] + '.Text="***~tWordcap(~'' + cols_no_underscores[li_index] + '~' )" ' + &
			ls_hdgs[li_index] + '.Font.Face="MS Sans Serif" ' + &
			ls_hdgs[li_index] + '.Font.Height="60" ' + &
			ls_hdgs[li_index] + '.Height="60" ' + &
			ls_hdgs[li_index] + '.Font.Weight="400" ' + &
			ls_hdgs[li_index] + '.Font.Family="0" ' + &
			ls_hdgs[li_index] + '.Font.Pitch="2" ' + &
			ls_hdgs[li_index] + '.Font.Charset="0" ' + &
			ls_cols[li_index]  + '.y="4" ' + &
			ls_cols[li_index] + '.Font.Face="MS Sans Serif" ' + &
			ls_cols[li_index] + '.Font.Height="52" ' + &
			ls_cols[li_index] + '.Height="60" ' + &
			ls_cols[li_index] + '.Font.Weight="400" ' + &
			ls_cols[li_index] + '.Font.Family="0" ' + &
			ls_cols[li_index] + '.Font.Pitch="2" ' + &
			ls_cols[li_index] + '.Font.Charset="0" ' + &
			ls_cols[li_index] + '.Tabsequence='+'~''+string(li_index)+'~''+ &
			ls_cols[li_index] + '.Visible="0" ' + &
			' '
			dw_criteria.Modify (ls_name)
		Next
	End If
End if
Parent.SetRedraw ( True )		//  Now, get the window ready to show criteria	

dw_criteria.setTransObject(sqlservertrans)
dw_columns.SetRedraw ( True )

end event

type st_9 from statictext within w_quick_select
integer x = 1070
integer y = 132
integer width = 274
integer height = 68
integer textsize = -9
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 41943040
long backcolor = 74481808
boolean enabled = false
string text = "&Columns:"
boolean focusrectangle = false
end type

type st_8 from statictext within w_quick_select
integer x = 18
integer y = 132
integer width = 238
integer height = 68
integer textsize = -9
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 41943040
long backcolor = 74481808
boolean enabled = false
string text = "&Tables:"
boolean focusrectangle = false
end type

type cb_cancel from commandbutton within w_quick_select
integer x = 2112
integer y = 308
integer width = 270
integer height = 100
integer taborder = 40
integer textsize = -9
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "&Cancel"
boolean cancel = true
end type

on clicked;// Clicked script for cb_cancel

Close (Parent)

end on

type cb_ok from commandbutton within w_quick_select
integer x = 2112
integer y = 196
integer width = 265
integer height = 100
integer taborder = 30
integer textsize = -9
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
boolean enabled = false
string text = "&OK"
boolean default = true
end type

event clicked;/*************************************************************************
		When this button is clicked open up the window to test the
		DataWindow.  Open a new sheet in the frame to create
		the datawindow in.
*************************************************************************/

SetPointer(HourGlass!)

window newwindow

int	li_rc
li_rc = dw_criteria.accepttext( )

If li_rc = 1 Then
	dynsql = FALSE
	OpenSheet(newwindow,"w_quick_select_sheet", ParentWindow(), 2, Original!)
End If

end event

type st_4 from statictext within w_quick_select
integer x = 1445
integer y = 84
integer width = 896
integer height = 68
integer textsize = -9
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 41943040
long backcolor = 74481808
boolean enabled = false
string text = "in the Selected Columns box."
boolean focusrectangle = false
end type

type st_3 from statictext within w_quick_select
integer x = 1376
integer y = 12
integer width = 960
integer height = 68
integer textsize = -9
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 41943040
long backcolor = 74481808
boolean enabled = false
string text = "3.  (Optional) Enter selection criteria"
boolean focusrectangle = false
end type

type st_2 from statictext within w_quick_select
integer x = 617
integer y = 16
integer width = 832
integer height = 68
integer textsize = -9
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 41943040
long backcolor = 74481808
boolean enabled = false
string text = "2.  Select one or more columns."
boolean focusrectangle = false
end type

type st_1 from statictext within w_quick_select
integer x = 32
integer y = 20
integer width = 562
integer height = 68
integer textsize = -9
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 41943040
long backcolor = 74481808
boolean enabled = false
string text = "1.  Select a table."
boolean focusrectangle = false
end type

type gb_styl from groupbox within w_quick_select
integer x = 2121
integer y = 1396
integer width = 338
integer height = 300
integer taborder = 110
integer textsize = -9
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 41943040
long backcolor = 74481808
string text = "Style"
end type


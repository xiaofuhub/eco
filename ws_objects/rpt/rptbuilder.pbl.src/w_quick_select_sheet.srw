$PBExportHeader$w_quick_select_sheet.srw
forward
global type w_quick_select_sheet from window
end type
type dw_1 from datawindow within w_quick_select_sheet
end type
end forward

global type w_quick_select_sheet from window
integer x = 430
integer y = 276
integer width = 2889
integer height = 1628
boolean titlebar = true
string title = "Runtime ReportWriter Painter"
string menuname = "m_quick_select_sheet"
boolean controlmenu = true
boolean minbox = true
boolean maxbox = true
boolean resizable = true
long backcolor = 79741120
string icon = "tutorial.ico"
toolbaralignment toolbaralignment = alignatleft!
event ue_postopen pbm_custom02
event ue_closesheet pbm_custom03
event ue_printdw pbm_custom04
event ue_postopensql pbm_custom05
dw_1 dw_1
end type
global w_quick_select_sheet w_quick_select_sheet

type variables
string is_object_name []
string is_hold_color []
string is_hold_backcolor []
string is_hold_backmode []
boolean is_text
boolean ib_updating

end variables

event ue_postopen;/*************************************************************************
		Make sure that the Quick Select window is open,  if it is then
		use the syntax in the grid datawindow (dw_criteria) as the source 
		for the DataWindow control in this window.  If the Quick Select 
		window is not open then give a message and default with a blank 
		DW Control and disable the capability of saving the DataWindow.

		The syntax for the datawindow will have to be modified to discover
		the visible fields, and then to build the Where statement by taking
		values out of the Grid DataWindow.  The Existing Grid DataWindow 
		Selects all columns from the table and then works depending on the 
		visible attributes.
*************************************************************************/
int 			li_createreturncode, li_arraybound, li_index, li_colcount
String 		ls_dwSyntax,  ls_dwerrorbuffer, ls_visiblecolumns[], ls_dwselectstmt
String		ls_tablename, ls_style, ls_whereclause, ls_criteriaselectstmt, ls_rc
String		ls_orderclause, ls_column, ls_report_type
DataWindow 	ldw_newdw
long		ll_start, ll_end, ll_rc
m_quick_select_sheet	 lm_menu 

//get id of the menu instance for this window
lm_menu = this.menuid

If Handle(w_quick_select) < 1 then
	MessageBox("There is No Quick Selection", "Unable to create Datawindow")
	Return
else	
	//*********************************************************************
	//		Build the Select statement Based on the Visible columns
	//		Copy the Grid DW from the Quick Select window to the local
	//		DW here and the instance array of visible column names.
	//*********************************************************************	
	ldw_newdw = W_quick_select.dw_Criteria
	ls_criteriaselectstmt = ldw_newdw.getsqlselect()
 	ls_visiblecolumns = w_quick_select.is_VisibleColumns
	ls_tablename	= w_quick_select.is_TableName
	
	//get the type datawindow from the quick select menu
	If w_quick_select.rb_grid.checked Then
		ls_report_type = "grid"
	ElseIf w_quick_select.rb_form.checked Then
		ls_report_type = "form"
	ElseIf  w_quick_select.rb_tabular.checked Then
		ls_report_type = "tabular"
	End if

	ls_style = 'style(type=' + ls_report_type + ')' + &
				'Text(background.mode=0 background.color=1073741824 color=0 ' +&
				'font.face = "MS Sans Serif"  font.height = -10  font.weight = 400 font.family = 2' + &
						'font.pitch=2  ) ' + &
				'Column(background.mode=0 background.color=1073741824 color=0 ' +&
				'font.face = "MS Sans Serif"  font.height = -8  font.weight = 400 font.family = 2' + &
						'font.pitch = 2 ) ' 
	this.title = "Report for table - " + ls_tablename

	//*********************************************************************
	//		Build the Select Statement
	//*********************************************************************
	ls_dwselectstmt = "Select " 
	li_arraybound = UpperBound(ls_visiblecolumns)
	For li_index = 1 to li_arraybound
			If ls_visiblecolumns[li_index] <> "" then
				ls_dwselectstmt = ls_dwselectstmt + ls_visiblecolumns[li_index]
				If li_index <> li_arraybound then
					ls_dwselectstmt = ls_dwselectstmt + ", "
				End If
			End If
	end for	

	// add the FROM clause 
	ls_dwselectstmt = ls_dwselectstmt + " From " + ls_tablename

	////////////////////////////////////////////////////////////////////////////////////
	//build the where clause from the querymode datawindow
	///////////////////////////////////////////////////////////////////////////////////
	ll_start = pos(lower(ls_criteriaselectstmt)," where ")
	
	If ll_start > 0 Then
	//order of stmts after where are groub by, having, order by 
	//however, in this example, only ORDER BY can occur
	//check if this occurs, if so, this is the end of the where clause
		ll_end = Pos(lower(ls_criteriaselectstmt)," order by ")

		If ll_end = 0 Then 	// no order by
			ll_end = len (ls_criteriaselectstmt)
		End If

		ls_whereclause = Mid ( ls_criteriaselectstmt, ll_start, ll_end - ll_start +1)
		ls_dwselectstmt = ls_dwselectstmt + ls_whereclause	

	End If 	//Get Where Clause


	////////////////////////////////////////////////////////////////////////////////////
	//build the order by clause from the querymode datawindow
	//This routine will parse through the criteria order by statement
	//converting the column #'s to database column names.
	//This is because the column #'s do not match the new
	//datawindow's column numbers.
	///////////////////////////////////////////////////////////////////////////////////
	ll_start = pos(lower(ls_criteriaselectstmt)," order by ")
	
	If ll_start > 0 Then
		// there is an order by, now parse it out
		ll_end = len (ls_criteriaselectstmt)
	
		ls_orderclause = " Order By "
	
		ll_start = ll_start + 11 // skip past the order by
		ll_end = pos(ls_criteriaselectstmt, " ", ll_start)
		do while ll_end > 0
			If Lower(Mid(ls_criteriaselectstmt, ll_start, 1)) = "a" or  &
			   Lower(Mid(ls_criteriaselectstmt, ll_start, 1)) = "d"  Then
				//This is the asc or desc stmt, just copy it
				ll_rc = pos (ls_criteriaselectstmt, ",", ll_end) + 1   // adjust for space after order sequence 
				If ll_rc > 1 Then ll_end = ll_rc    //no need to adjust if this was the last asc/desc 
				ls_orderclause = ls_orderclause + mid(ls_criteriaselectstmt, ll_start, ll_end - ll_start + 1)
		      Else  			// must be a column # --- convert it to a column name
				ls_column = mid (ls_criteriaselectstmt, ll_start, ll_end - ll_start +1)
				ls_orderclause = ls_orderclause + &
					ldw_newdw.describe ("#" + ls_column + ".dbname") + " "
		      End If				

			//move start to next token
			ll_start = ll_end + 1
			ll_end = pos(ls_criteriaselectstmt, " ", ll_start)
		loop
		
		//build it into main select statement
		ls_dwselectstmt = ls_dwselectstmt + ls_orderclause	

	End If 	//Get Order Clause

	ls_dwSyntax= sqlservertrans.SyntaxFromSQL( ls_dwselectstmt, ls_style, ls_dwerrorbuffer)
	
	li_createreturncode = dw_1.Create(ls_dwSyntax)

End If

ls_rc = dw_1.modify ("datawindow.selected.mouse=no")
if ls_rc <>"" then messagebox("hi",ls_rc)

If li_createreturncode > 0 then
	li_colcount = Integer (dw_1.Describe ("datawindow.column.count"))
	
	For li_index = 1 to li_colcount
		dw_1.SetTabOrder(li_index,0)
	Next


	dw_1.settransobject( sqlservertrans)	
	dw_1.retrieve( )
	
	//Enable local sheet print setup and print report
	Enable (lm_menu.m_file.m_printsetup)
	Enable (lm_menu.m_file.m_savedatawindow)

	w_quick_select.hide()
Else
	messagebox("Creation of the Report Failed", &
					"The return code is "+ string(li_createreturncode) + "~r~n" + &
					"Error Description: "+ls_dwerrorbuffer)
//	w_quick_select_sheet.TriggerEvent ("ue_closesheet")
	w_quick_select.show()
End If

end event

event ue_closesheet;//close this sheet

w_quick_select.sqlstmt=''
close(this)
end event

event ue_printdw;long job
Integer rtn

job = PrintOpen( ) 
rtn = PrintDataWindow(job, dw_1) 
IF rtn = 1  THEN
	MessageBox("Print","Report has successfully printed.",Information!)
ELSE
	MessageBox("Error","Print failed",StopSign!)
END IF
PrintClose(job)
end event

event ue_postopensql;/*************************************************************************
		Make sure that the Quick Select window is open,  if it is then
		use the syntax in the grid datawindow (dw_criteria) as the source 
		for the DataWindow control in this window.  If the Quick Select 
		window is not open then give a message and default with a blank 
		DW Control and disable the capability of saving the DataWindow.

		The syntax for the datawindow will have to be modified to discover
		the visible fields, and then to build the Where statement by taking
		values out of the Grid DataWindow.  The Existing Grid DataWindow 
		Selects all columns from the table and then works depending on the 
		visible attributes.
*************************************************************************/
int 			li_createreturncode, li_arraybound, li_index, li_colcount
String 		ls_dwSyntax,  ls_dwerrorbuffer, ls_dwselectstmt
String		ls_style, ls_criteriaselectstmt, ls_rc
String		ls_column, ls_report_type
DataWindow 	ldw_newdw
integer		ll_start_update, ll_start_delete
long     ll_rc
m_quick_select_sheet	 lm_menu 

//get id of the menu instance for this window
lm_menu = this.menuid

If Handle(w_quick_select) < 1 then
	MessageBox("There is No Quick Selection", "Unable to create Datawindow")
	Return
else	
	//*********************************************************************
	//		Build the Select statement Based on the Visible columns
	//		Copy the Grid DW from the Quick Select window to the local
	//		DW here and the instance array of visible column names.
	//*********************************************************************	
	ldw_newdw = W_quick_select.dw_Criteria
	ls_criteriaselectstmt = w_quick_select.sqlstmt
	
	ll_start_update = pos(lower(ls_criteriaselectstmt),"update ")
	ll_start_delete = pos(lower(ls_criteriaselectstmt),"delete ")
	
	IF ll_start_update>0 OR ll_start_delete>0 THEN
		MessageBox("ERROR","Delete or Update are not allowed in building a report! ",StopSign!)
		return
	END IF
	
	//get the type datawindow from the quick select menu
	If w_quick_select.rb_grid.checked Then
		ls_report_type = "grid"
	ElseIf w_quick_select.rb_form.checked Then
		ls_report_type = "form"
	ElseIf  w_quick_select.rb_tabular.checked Then
		ls_report_type = "tabular"
	End if

	ls_style = 'style(type=' + ls_report_type + ')' + &
				'Text(background.mode=0 background.color=1073741824 color=0 ' +&
				'font.face = "MS Sans Serif"  font.height = -10  font.weight = 400 font.family = 2' + &
						'font.pitch=2  ) ' + &
				'Column(background.mode=0 background.color=1073741824 color=0 ' +&
				'font.face = "MS Sans Serif"  font.height = -8  font.weight = 400 font.family = 2' + &
						'font.pitch = 2 ) ' 
	this.title = "Report for SQL Statement "

	ls_dwSyntax= sqlservertrans.SyntaxFromSQL( ls_criteriaselectstmt, ls_style, ls_dwerrorbuffer)
	
	li_createreturncode = dw_1.Create(ls_dwSyntax)

End If

ls_rc = dw_1.modify ("datawindow.selected.mouse=no")
if ls_rc <>"" then messagebox("hi",ls_rc)

If li_createreturncode > 0 then
	li_colcount = Integer (dw_1.Describe ("datawindow.column.count"))
	
	For li_index = 1 to li_colcount
		dw_1.SetTabOrder(li_index,0)
	Next

	dw_1.settransobject( sqlservertrans)	
	dw_1.retrieve( )
	
	//Enable local sheet print setup and print report
	Enable (lm_menu.m_file.m_printsetup)
	Enable (lm_menu.m_file.m_savedatawindow)

	w_quick_select.hide()
Else
	messagebox("Creation of the Report Failed", &
					"The return code is "+ string(li_createreturncode) + "~r~n" + &
					"Error Description: "+ls_dwerrorbuffer)
//	w_quick_select_sheet.TriggerEvent ("ue_closesheet")
	w_quick_select.show()
End If

end event

event open;// Open the sheet in Maximized mode
this.windowstate = maximized!
IF w_quick_select.dynsql=TRUE THEN
	this.postevent("ue_postopensql")
ELSE
	this.postevent("ue_postopen")
END IF

end event

on w_quick_select_sheet.create
if this.MenuName = "m_quick_select_sheet" then this.MenuID = create m_quick_select_sheet
this.dw_1=create dw_1
this.Control[]={this.dw_1}
end on

on w_quick_select_sheet.destroy
if IsValid(MenuID) then destroy(MenuID)
destroy(this.dw_1)
end on

type dw_1 from datawindow within w_quick_select_sheet
integer x = 14
integer y = 36
integer width = 2802
integer height = 1376
integer taborder = 10
boolean hscrollbar = true
boolean vscrollbar = true
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event retrievestart;Open(w_pics_retrieve_msg_box)
end event

event retrieveend;close(w_pics_retrieve_msg_box)
end event


$PBExportHeader$w_isbn_label.srw
forward
global type w_isbn_label from w_sheet
end type
type cbx_isbn from checkbox within w_isbn_label
end type
type dw_isbn_bkno from u_pics_dw within w_isbn_label
end type
type cb_isbn from commandbutton within w_isbn_label
end type
type cb_update from commandbutton within w_isbn_label
end type
type cb_shelf from commandbutton within w_isbn_label
end type
type cb_print from commandbutton within w_isbn_label
end type
type cb_exit from u_cb within w_isbn_label
end type
type cb_clear from u_cb within w_isbn_label
end type
type cb_find from u_cb within w_isbn_label
end type
type dw_isbn_conno from u_dw within w_isbn_label
end type
type dw_isbn_scan from u_pics_dw within w_isbn_label
end type
end forward

global type w_isbn_label from w_sheet
integer width = 4073
integer height = 1608
string title = "Book labels Query only"
cbx_isbn cbx_isbn
dw_isbn_bkno dw_isbn_bkno
cb_isbn cb_isbn
cb_update cb_update
cb_shelf cb_shelf
cb_print cb_print
cb_exit cb_exit
cb_clear cb_clear
cb_find cb_find
dw_isbn_conno dw_isbn_conno
dw_isbn_scan dw_isbn_scan
end type
global w_isbn_label w_isbn_label

type variables
DataWindowChild lddwc_shelfs
end variables

on w_isbn_label.create
int iCurrent
call super::create
this.cbx_isbn=create cbx_isbn
this.dw_isbn_bkno=create dw_isbn_bkno
this.cb_isbn=create cb_isbn
this.cb_update=create cb_update
this.cb_shelf=create cb_shelf
this.cb_print=create cb_print
this.cb_exit=create cb_exit
this.cb_clear=create cb_clear
this.cb_find=create cb_find
this.dw_isbn_conno=create dw_isbn_conno
this.dw_isbn_scan=create dw_isbn_scan
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.cbx_isbn
this.Control[iCurrent+2]=this.dw_isbn_bkno
this.Control[iCurrent+3]=this.cb_isbn
this.Control[iCurrent+4]=this.cb_update
this.Control[iCurrent+5]=this.cb_shelf
this.Control[iCurrent+6]=this.cb_print
this.Control[iCurrent+7]=this.cb_exit
this.Control[iCurrent+8]=this.cb_clear
this.Control[iCurrent+9]=this.cb_find
this.Control[iCurrent+10]=this.dw_isbn_conno
this.Control[iCurrent+11]=this.dw_isbn_scan
end on

on w_isbn_label.destroy
call super::destroy
destroy(this.cbx_isbn)
destroy(this.dw_isbn_bkno)
destroy(this.cb_isbn)
destroy(this.cb_update)
destroy(this.cb_shelf)
destroy(this.cb_print)
destroy(this.cb_exit)
destroy(this.cb_clear)
destroy(this.cb_find)
destroy(this.dw_isbn_conno)
destroy(this.dw_isbn_scan)
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

inv_resize.of_Register(cbx_isbn, "scale")
inv_resize.of_Register(cb_clear, "scale")
inv_resize.of_Register(cb_exit, "scale")
inv_resize.of_Register(cb_find, "scale")
inv_resize.of_Register(cb_print, "scale")
inv_resize.of_Register(cb_shelf, "scale")
inv_resize.of_Register(cb_isbn, "scale")
inv_resize.of_Register(cb_update, "scale")
inv_resize.of_Register(dw_isbn_conno, "scale")
inv_resize.of_Register(dw_isbn_scan, "scale")

end event

event open;call super::open;//open the sheet in maximized state
this.windowstate = maximized!


end event

event mousemove;call super::mousemove;w_pics_main.Event pfc_microhelp("Ready")
end event

type cbx_isbn from checkbox within w_isbn_label
integer x = 2501
integer y = 1288
integer width = 457
integer height = 80
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 33554432
long backcolor = 67108864
string text = "Scan ISBN"
end type

event clicked;IF this.Checked=TRUE THEN
	cb_find.visible = FALSE
	cb_clear.TriggerEvent(Clicked!)
	dw_isbn_conno.visible=FALSE
	dw_isbn_scan.visible=TRUE
	dw_isbn_scan.insertrow(0)
	dw_isbn_scan.setfocus()
ELSE
	cb_find.visible = TRUE
	dw_isbn_conno.visible=TRUE
	dw_isbn_scan.visible=FALSE
	dw_isbn_conno.setfocus()
END IF
end event

type dw_isbn_bkno from u_pics_dw within w_isbn_label
boolean visible = false
integer x = 2542
integer y = 1396
integer width = 169
integer height = 88
integer taborder = 20
string dataobject = "d_isbn_bkno"
boolean vscrollbar = false
boolean border = false
boolean livescroll = false
end type

event constructor;call super::constructor;this.SetTransObject(SqlServerTrans)

end event

type cb_isbn from commandbutton within w_isbn_label
integer x = 1047
integer y = 1288
integer width = 631
integer height = 96
integer taborder = 20
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "View Books Shelved"
end type

event clicked;OpenSheet(w_isbn_only, w_pics_main, 0, Original!)
end event

type cb_update from commandbutton within w_isbn_label
integer x = 1705
integer y = 1288
integer width = 704
integer height = 96
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "Update Books Shelved"
end type

event clicked;Int rc,i

// not scanned isbn(s)
IF cbx_isbn.checked=FALSE THEN
	
	// Restore all the deleted rows back to primary buffer
	dw_isbn_conno.RowsMove(1, dw_isbn_conno.DeletedCount(), delete!, dw_isbn_conno, 1, primary!)
	
	dw_isbn_conno.AcceptText()
	
	rc = dw_isbn_conno.EVENT pfc_update(TRUE,TRUE)
			
	IF rc=1 THEN
		COMMIT USING SqlServerTrans;
		Messagebox("Update","Books shelved.",Information!)
		RETURN 1
	ELSE 
		ROLLBACK USING SqlServerTrans;
		Messagebox("ERROR","Update failed.",stopSign!)
		RETURN 0
	END IF	
	
ELSE
// scanned isbn(s)

	dw_isbn_scan.AcceptText()
	
	FOR i = 1 TO  dw_isbn_scan.RowCount()
		IF dw_isbn_scan.object.isbn[i] = "" OR IsNull(dw_isbn_scan.object.isbn[i]) THEN
			dw_isbn_scan.DeleteRow(i)
		END IF
	NEXT
	
	rc = dw_isbn_scan.EVENT pfc_update(TRUE,TRUE)
			
	IF rc=1 THEN
		COMMIT USING SqlServerTrans;
		Messagebox("Update","Books shelved.",Information!)
		RETURN 1
	ELSE 
		ROLLBACK USING SqlServerTrans;
		Messagebox("ERROR","Update failed.",stopSign!)
		RETURN 0
	END IF	

END IF
end event

type cb_shelf from commandbutton within w_isbn_label
integer x = 443
integer y = 1288
integer width = 581
integer height = 96
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "Add New Shelf Loc"
end type

event clicked;open(w_addshelf)


IF w_isbn_label.cbx_isbn.Checked=FALSE THEN

	dw_isbn_conno.GetChild("shelf_loc",lddwc_shelfs)
	dw_isbn_conno.SetTransObject(SqlServerTrans)
	lddwc_shelfs.SetTransObject(SqlServerTrans)
	lddwc_shelfs.retrieve()
	
ELSE
	dw_isbn_scan.GetChild("shelf_loc",lddwc_shelfs)
	dw_isbn_scan.SetTransObject(SqlServerTrans)
	lddwc_shelfs.SetTransObject(SqlServerTrans)
	lddwc_shelfs.retrieve()
	
END IF	


end event

type cb_print from commandbutton within w_isbn_label
integer x = 37
integer y = 1288
integer width = 379
integer height = 96
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "Print Labels"
end type

event clicked;int i, ll_rows
string DWfilter


IF cbx_isbn.Checked=FALSE THEN


	dw_isbn_conno.accepttext()
	
	DWfilter = "prnt = 'Y'"
	dw_isbn_conno.SetFilter(DWfilter)
	dw_isbn_conno.Filter( )
	
	ll_rows = dw_isbn_conno.rowcount()
	
	dw_isbn_bkno.reset()
	
	FOR i = 1 to ll_rows
		dw_isbn_bkno.InsertRow(0)
		dw_isbn_bkno.object.bkno[i] = dw_isbn_conno.object.bkno[i]
		dw_isbn_bkno.object.conno[i] = dw_isbn_conno.object.conno[i]
		dw_isbn_bkno.object.sttl[i] = dw_isbn_conno.object.sttl[i]
	NEXT
	
	dw_isbn_bkno.accepttext()
		
	dw_isbn_bkno.SaveAs("label.csv",CSV!, TRUE)
	
	MessageBox("Labels","label.csv was created in the default directory with these records. You will need to run the PT-Editor to create the labels.")
	
	Run("Ptedit3.exe")
	
ELSE
	
	dw_isbn_scan.accepttext()
	
	DWfilter = "prnt = 'Y'"
	dw_isbn_scan.SetFilter(DWfilter)
	dw_isbn_scan.Filter( )
	
	ll_rows = dw_isbn_scan.rowcount()
	
	dw_isbn_bkno.reset()
	
	FOR i = 1 to ll_rows
		dw_isbn_bkno.InsertRow(0)
		dw_isbn_bkno.object.bkno[i] = dw_isbn_scan.object.bkno[i]
		dw_isbn_bkno.object.conno[i] = dw_isbn_scan.object.conno[i]
		dw_isbn_bkno.object.sttl[i] = dw_isbn_scan.object.sttl[i]
	NEXT
	
	dw_isbn_bkno.accepttext()
		
	dw_isbn_bkno.SaveAs("label.csv",CSV!, TRUE)
	
	MessageBox("Labels","label.csv was created in the default directory with these records. You will need to run the PT-Editor to create the labels.")
	
	Run("Ptedit3.exe")

	
	
END IF

end event

type cb_exit from u_cb within w_isbn_label
event ue_hint_text pbm_mousemove
string tag = "Exits the screen"
integer x = 3730
integer y = 1280
integer width = 183
integer height = 96
integer taborder = 0
integer textsize = -10
string text = "E&xit"
end type

event ue_hint_text;call super::ue_hint_text;w_pics_main.Event pfc_microhelp(this.tag)
end event

event clicked;call super::clicked;ib_disableclosequery = TRUE
Parent.Event pfc_close()

end event

type cb_clear from u_cb within w_isbn_label
event ue_hint_text pbm_mousemove
string tag = "Clears the screen for input"
integer x = 3511
integer y = 1280
integer width = 219
integer height = 96
integer taborder = 0
boolean bringtotop = true
integer textsize = -10
string text = "&Clear"
end type

event ue_hint_text;call super::ue_hint_text;w_pics_main.Event pfc_microhelp(this.tag)
end event

event clicked;call super::clicked;IF cbx_isbn.Checked=FALSE THEN

	ib_disableclosequery=TRUE
	//reset the datawindow
	dw_isbn_conno.Reset()
	
	//remove the where clause and set query mode to yes
	dw_isbn_conno.Object.DataWindow.QueryClear = "yes"
	dw_isbn_conno.Object.DataWindow.QueryMode = "yes"
	
	
	dw_isbn_conno.SetFocus()
	dw_isbn_conno.setcolumn("isbn")

ELSE
	
	dw_isbn_scan.Reset()
	dw_isbn_scan.event pfc_addrow()
	dw_isbn_scan.setfocus()
	dw_isbn_scan.setcolumn(2)
	
END IF
	

end event

type cb_find from u_cb within w_isbn_label
event ue_hint_text pbm_mousemove
string tag = "Get data"
integer x = 3255
integer y = 1280
integer width = 219
integer height = 96
integer taborder = 0
integer textsize = -10
string text = "F&ind"
end type

event ue_hint_text;call super::ue_hint_text;w_pics_main.Event pfc_microhelp(this.tag)
end event

event clicked;call super::clicked;Integer li_Rtn_code,i
String laction_Type,Lconno,lmsg,Lcntr,Lbkno,lprdr


//Disable the query mode and retrieve data
dw_Isbn_conno.AcceptText()
dw_Isbn_conno.object.DataWindow.QueryMode = "no"

li_Rtn_code = dw_Isbn_conno.Retrieve()

IF li_Rtn_code = 0 THEN
	Messagebox('Error','No Data Found')
	cb_Clear.TriggerEvent(clicked!)
	RETURN
ELSE
	FOR i = 1 TO li_Rtn_code
		IF IsNull(dw_Isbn_conno.object.conno[i]) OR dw_Isbn_conno.object.conno[i]="" THEN
			dw_Isbn_conno.DeleteRow(i)
		END IF
		laction_Type = dw_Isbn_conno.object.action_Type[i]
		IF NOT(IsNull(laction_Type)) THEN
			IF laction_Type <> 'C' THEN
				Lbkno = dw_Isbn_conno.object.bkmed[i]+String(dw_Isbn_conno.object.bkseq[i])
				Lconno = dw_Isbn_conno.object.conno[i]
				Lcntr = dw_Isbn_conno.object.cntr[i]
				Lprdr = dw_Isbn_conno.object.prdr[i]
				IF IsNull(lcntr) THEN
					lmsg = 'Control No = ' + Lconno + ' Book No = ' + Lbkno +' has already been selected for a Conversion '
				ELSE
					lmsg = 'Control No = ' + Lconno + ' Book No = ' + Lbkno +' has already been selected for a Conversion, Contract No = '+Lcntr+ ', Shipped to prodcuer = '+lprdr
				END IF
				MessageBox('Conversion Information',lmsg)
			END IF
		END IF
	NEXT
	dw_Isbn_conno.SetFocus()
	
	//Set focus to the chno field and enable the update button
	dw_Isbn_conno.SetColumn("shelf_loc")
END IF


end event

type dw_isbn_conno from u_dw within w_isbn_label
event ue_enter_to_tab pbm_dwnprocessenter
event ue_hint_text pbm_mousemove
integer y = 32
integer width = 4023
integer height = 1216
integer taborder = 10
string dataobject = "d_isbn_conno"
boolean hscrollbar = true
boolean vscrollbar = false
end type

event ue_enter_to_tab;Send(Handle(this), 256, 9, Long(0,0))
Return (1)
end event

event constructor;SetTransObject(SqlServerTrans)
dw_isbn_conno.Object.DataWindow.QueryMode = "yes"
dw_isbn_conno.SetRowFocusIndicator(Hand!)
end event

event retrieveend;call super::retrieveend;int i

FOR i = 1 to rowcount
	dw_isbn_conno.object.prnt[i]='Y'
NEXT
end event

event itemchanged;call super::itemchanged;String   ls_shelf,ls_bk
Integer  rtn, ll_rows, i

IF DWO.Name = "shelf_loc" THEN
	ls_shelf = Data
	rtn = Messagebox("Shelf Location", " Do you want to assign this self location to the rest of the rows?", question!,yesNo!,1)
	IF rtn = 1 THEN
		
		ll_rows = dw_isbn_conno.RowCount()
		FOR i = 1 TO ll_rows
			dw_isbn_conno.object.shelf_loc[i] = ls_shelf
			dw_isbn_conno.object.book[i] = 'Monitor'		
		NEXT
	ELSE
		dw_isbn_conno.object.book[row] = 'Monitor'		
	END IF

ELSEIF DWO.Name = "cd_shelf_loc" THEN
	ls_shelf = Data
	rtn = Messagebox("CD Shelf Location", " Do you want to assign this CD self location to the rest of the rows?", question!,yesNo!,1)
	IF rtn = 1 THEN
		ll_rows = dw_isbn_conno.RowCount()
		FOR i = 1 TO ll_rows
			dw_isbn_conno.object.cd_shelf_loc[i] = ls_shelf
		NEXT
	END IF	
END IF

end event

event sqlpreview;call super::sqlpreview;//messagebox("sql",sqlsyntax)
end event

type dw_isbn_scan from u_pics_dw within w_isbn_label
boolean visible = false
integer y = 32
integer width = 4023
integer height = 1216
integer taborder = 20
string dataobject = "d_isbn_scan"
end type

event constructor;call super::constructor;SetTransObject(SqlServerTrans)
this.SetRowFocusIndicator(Hand!)
end event

event itemchanged;call super::itemchanged;String   ls_shelf, new_isbn
Integer  rtn, ll_rows, i

IF DWO.Name = "isbn" THEN
	
	string lconno, lisbn,lsttl,lbkmed, lbkno, lshelf_loc,lprnt, lauth
	long lbkseq

	new_isbn = mid(data, 4, 9)
	this. object. isbn[row] = new_isbn
	
	  SELECT conno,
			isbn,
			sttl,
         auth,
			bkseq, 
			bkmed,  
			bkmed||' '||bkseq bkno,
			shelf_loc,
			aepcd prnt
	 into :lconno,
	      :lisbn,
			:lsttl,
			:lauth,
			:lbkseq,
			:lbkmed,
			:lbkno,
			:lshelf_loc,
			:lprnt
    FROM mchar, ttlinit 
   WHERE ttlinit.chno = mchar.chno 
	AND  substr(isbn, 1, 9) = :new_isbn
	AND 	mchar.med = 'RC' 
	USING sqlservertrans;
	
	IF f_check_dberror(sqlservertrans, "TTLINIT") = FALSE THEN
		MessageBox("ERROR","Error in getting ISBN")
		RETURN -1
	ELSE
		IF IsNull(lisbn) OR lisbn = "" THEN
			MessageBox("No Data","Data not found for this barcode")
			RETURN 1
		ELSE
			
			this.object.isbn[row] = lisbn
			this.object.conno[row] = lconno
			this.object.sttl[row] = lsttl
			this.object.auth[row] = lauth
			this.object.bkseq[row] = lbkseq
			this.object.bkmed[row] = lbkmed
			this.object.bkno[row] = lbkno
			this.object.shelf_loc[row] = lshelf_loc
			this.object.prnt[row] = 'Y'
			
			this.SetItemStatus(row, 0,	Primary!, DataModified!)
			ll_rows = this.event pfc_addrow()
			this.setrow(this.rowcount())
		END IF
	
	END IF

ELSEIF DWO.Name = "shelf_loc" THEN
	ls_shelf = DATA
	rtn = MessageBox("Shelf Location", " Do you want to assign this self location to the rest of the rows?", Question!,YesNo!,1)
	IF rtn = 1 THEN
		
		ll_rows = dw_isbn_scan.RowCount()
		FOR i = 1 TO ll_rows
			dw_isbn_scan.object.shelf_loc[i] = ls_shelf
			dw_isbn_scan.object.book[i] = 'Monitor'
		NEXT
	ELSE
		dw_isbn_scan.object.book[row] = 'Monitor'
		
	END IF


END IF
end event

event sqlpreview;call super::sqlpreview;//messagebox("sql",sqlsyntax)
end event


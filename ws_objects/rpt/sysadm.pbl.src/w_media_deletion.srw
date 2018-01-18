$PBExportHeader$w_media_deletion.srw
forward
global type w_media_deletion from w_sheet
end type
type dw_archive_title_delete from u_dw within w_media_deletion
end type
type cb_find from u_cb within w_media_deletion
end type
type cb_confirm from u_cb within w_media_deletion
end type
type cb_clear from u_cb within w_media_deletion
end type
type cb_close from u_cb within w_media_deletion
end type
type cb_delete from u_cb within w_media_deletion
end type
type dw_medium_deletion from u_dw within w_media_deletion
end type
end forward

global type w_media_deletion from w_sheet
integer x = 146
integer y = 172
integer width = 2766
integer height = 1396
string title = "Media Deletion"
dw_archive_title_delete dw_archive_title_delete
cb_find cb_find
cb_confirm cb_confirm
cb_clear cb_clear
cb_close cb_close
cb_delete cb_delete
dw_medium_deletion dw_medium_deletion
end type
global w_media_deletion w_media_deletion

type variables
string is_chno
string is_conno
string is_syntax[6]
string is_connum[]
long il_bkseq[]
string is_chno_del
long il_currow = 1
end variables

on w_media_deletion.create
int iCurrent
call super::create
this.dw_archive_title_delete=create dw_archive_title_delete
this.cb_find=create cb_find
this.cb_confirm=create cb_confirm
this.cb_clear=create cb_clear
this.cb_close=create cb_close
this.cb_delete=create cb_delete
this.dw_medium_deletion=create dw_medium_deletion
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_archive_title_delete
this.Control[iCurrent+2]=this.cb_find
this.Control[iCurrent+3]=this.cb_confirm
this.Control[iCurrent+4]=this.cb_clear
this.Control[iCurrent+5]=this.cb_close
this.Control[iCurrent+6]=this.cb_delete
this.Control[iCurrent+7]=this.dw_medium_deletion
end on

on w_media_deletion.destroy
call super::destroy
destroy(this.dw_archive_title_delete)
destroy(this.cb_find)
destroy(this.cb_confirm)
destroy(this.cb_clear)
destroy(this.cb_close)
destroy(this.cb_delete)
destroy(this.dw_medium_deletion)
end on

event mousemove;call super::mousemove;w_pics_main.setmicrohelp("Ready")
end event

event pfc_preopen;call super::pfc_preopen;this.of_SetPreference(TRUE)
this.inv_preference.of_SetToolBars(TRUE)
this.inv_preference.of_SetWindow(TRUE)
this.of_SetResize(TRUE)
this.inv_resize.of_SetOrigSize(this.workspacewidth(),this.workspaceheight())
inv_resize.of_Register(dw_archive_title_delete, "scale")
inv_resize.of_Register(dw_medium_deletion, "scale")
inv_resize.of_Register(cb_clear, "scale")
inv_resize.of_Register(cb_close, "scale")
inv_resize.of_Register(cb_find, "scale")
inv_resize.of_Register(cb_confirm, "scale")
inv_resize.of_Register(cb_delete, "scale")
end event

event pfc_postopen;call super::pfc_postopen;dw_archive_title_delete.setfocus()
is_syntax[1] = dw_archive_title_delete.Object.DataWindow.Table.Select
is_syntax[3] = dw_medium_deletion.Object.DataWindow.Table.Select
cb_confirm.Enabled = FALSE
cb_clear.Enabled = FALSE
cb_delete.Enabled = FALSE
m_pics_main.m_Edit.m_Addrow.Enabled = FALSE
m_pics_main.m_Edit.m_Deleterow.Enabled = FALSE
m_pics_main.m_Edit.m_Cut.Enabled = FALSE
dw_medium_deletion.Visible = FALSE


end event

event resize;call super::resize;//long ll_height
//This.X = w_pics_main.X
//This.Y = w_pics_main.Y
//ll_height = w_pics_main.mdi_1.Height
//This.Resize(w_pics_main.WorkSpaceWidth(), ll_height)
end event

event pfc_save;call super::pfc_save;integer li_count, i
long ll_count

li_count = UPPERBOUND(is_connum[])
dw_medium_deletion.Accepttext()
IF dw_medium_deletion.update() = 1 THEN
	Open(w_pics_update_msg_box)	
	FOR i = 1 TO li_count
		IF NOT is_connum[i] = "" THEN
			DELETE FROM ri
			WHERE ri.conno = :is_connum[i]
			USING sqlservertrans ;
			DELETE FROM specinst
			WHERE specinst.conno = :is_connum[i]
			USING sqlservertrans ;
			DELETE FROM catalog
			WHERE catalog.conno = :is_connum[i]
			USING sqlservertrans ;
			DELETE FROM arrsn
			WHERE arrsn.conno = :is_connum[i]
			USING sqlservertrans ;
			DELETE FROM prvnarr
			WHERE prvnarr.bkseq = :il_bkseq[i]
			USING sqlservertrans ;
			DELETE FROM inv
			WHERE inv.bkseq = :il_bkseq[i]
			USING sqlservertrans ;
			DELETE FROM qastg
			WHERE qastg.bkseq = :il_bkseq[i]
			USING sqlservertrans ;
			DELETE FROM ext
			WHERE ext.bkseq = :il_bkseq[i]
			USING sqlservertrans ;
			DELETE FROM narr
			WHERE narr.bkseq = :il_bkseq[i]
			USING sqlservertrans ;
			DELETE FROM prod
			WHERE prod.bkseq = :il_bkseq[i]
			USING sqlservertrans ;
			DELETE FROM sched
			WHERE sched.bkseq = :il_bkseq[i]
			USING sqlservertrans ;
	
		END IF
	NEXT
	IF NOT is_chno_del = "" OR NOT ISNULL(is_chno_del) THEN
		SELECT count(*)
		INTO :ll_count
		FROM mchar
		WHERE mchar.chno = :is_chno_del
		USING sqlservertrans ;
		IF NOT ll_count > 0 THEN
			DELETE FROM crrest
			WHERE crrest.chno = :is_chno_del
			USING sqlservertrans ;
			DELETE FROM coauth
			WHERE coauth.chno = :is_chno_del
			USING sqlservertrans ;
			DELETE FROM acquist
			WHERE acquist.chno = :is_chno_del
			USING sqlservertrans ;
			DELETE FROM ttlinit
			WHERE ttlinit.chno = :is_chno_del
			USING sqlservertrans ;
			DELETE FROM cr
			WHERE cr.chno = :is_chno_del
			USING sqlservertrans ;
			DELETE FROM annotation
			WHERE annotation.chno = :is_chno_del
			USING sqlservertrans ;
			dw_medium_deletion.setfocus()
		END IF
	END IF
END IF
RETURN 1

			
			
end event

event open;call super::open;THIS.Windowstate = maximized!
end event

type dw_archive_title_delete from u_dw within w_media_deletion
event pfc_hinttext pbm_mousemove
event ue_enterkey pbm_dwnprocessenter
integer x = 14
integer y = 32
integer width = 2697
integer height = 548
integer taborder = 10
string dataobject = "d_archival_control"
boolean vscrollbar = false
end type

event pfc_hinttext;call super::pfc_hinttext;string ls_object, ls_column, ls_column_tag
long ll_pos

//This script set's microhelp at the bottom of the screen for the Archive Title Delete Datawindow
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

event ue_enterkey;call super::ue_enterkey;//Send(Handle(this),256,9,Long(0,0))
//return(1)
end event

event constructor;call super::constructor;THIS.of_settransobject(SQLservertrans)
InsertRow(0)
ib_rmbmenu = FALSE
of_SetUpdateable(FALSE)
end event

event pfc_retrieve;call super::pfc_retrieve;This.Reset()
return this.retrieve(is_chno)
end event

event retrievestart;call super::retrievestart;w_pics_main.setmicrohelp("Retrieving the Record Please Wait.....")
end event

event retrieveend;call super::retrieveend;dw_medium_deletion.setfocus()
end event

event getfocus;call super::getfocus;m_pics_main.m_Edit.m_Addrow.Enabled = FALSE
m_pics_main.m_Edit.m_Deleterow.Enabled = FALSE
m_pics_main.m_Edit.m_Cut.Enabled = FALSE

end event

type cb_find from u_cb within w_media_deletion
event pfc_hinttext pbm_mousemove
string tag = "Find~'s the Record Based On Valid Control Number Or Valid Chart Number"
integer x = 553
integer y = 1184
integer taborder = 0
integer textsize = -10
string text = "F&ind"
boolean default = true
end type

event pfc_hinttext;call super::pfc_hinttext;w_pics_main.setmicrohelp(THIS.tag)
end event

event clicked;call super::clicked;string ls_conno, ls_chno, ls_find, ls_dummy
long ll_count, i, ll_find

dw_archive_title_delete.AcceptText()
ls_conno = dw_archive_title_delete.GetItemString(1, "mchar_conno")
ls_chno = dw_archive_title_delete.GetItemString(1,"ttlinit_chno")

IF len(ls_conno) > 0 THEN
	ls_find = "conno"
ELSEIF len(ls_chno) > 0 THEN
	ls_find = "chno"
ELSE
	MessageBox("Invalid Find Criteria", "Please enter either " + & 
		"control number or chart number to retrieve the record.")
	dw_archive_title_delete.SetFocus()
	dw_archive_title_delete.setcolumn("mchar_conno")
	RETURN
END IF

IF ls_find = "conno" THEN
	IF len(ls_conno) <> 8 THEN
		MessageBox("Invalid Control Number", "The Control Number must " + &
			"be an eight characters number.  Please reenter the " + &
			"control number")
		dw_archive_title_delete.SetItem(1, "mchar_conno", "")
		dw_archive_title_delete.SetFocus()
		dw_archive_title_delete.Setcolumn("mchar_conno")
		RETURN
	END IF
	
	  SELECT mchar.chno  
	    INTO :ls_chno  
	    FROM mchar
  	 WHERE mchar.conno = :ls_conno
		USING SQLservertrans ;
	
	IF ls_chno = "" OR IsNull(ls_chno) THEN
		MessageBox("Invalid Control Number", "The Control Number is " + &
			"invalid.  Please reenter the correct control number")
		dw_archive_title_delete.SetItem(1, "mchar_conno", "")
		dw_archive_title_delete.SetFocus()
		dw_archive_title_delete.Setcolumn("mchar_conno")
		RETURN
	ELSE
		is_chno = ls_chno

		is_syntax[2] = is_syntax[1] + "AND  mchar.conno = " + ls_conno 
		dw_archive_title_delete.Object.DataWindow.Table.Select = is_syntax[2]
		is_syntax[4] = is_syntax[3] + "AND  mchar.conno = " + ls_conno 
		dw_medium_deletion.Object.DataWindow.Table.Select = is_syntax[4]
		dw_archive_title_delete.Event pfc_retrieve()
		dw_medium_deletion.Event pfc_retrieve()
		
		dw_archive_title_delete.Object.Datawindow.Readonly = TRUE
		This.Enabled = FALSE
		cb_confirm.Enabled = TRUE
		cb_clear.Enabled = TRUE
		cb_delete.Enabled = TRUE
		w_pics_main.SetMicroHelp("Ready")		
	END IF
ELSE
	IF len(ls_chno) <> 6 THEN
		MessageBox("Invalid chart Number", "The Chart Number must " + &
			"be a six characters number.  Please reenter the " + &
			"chart number")
		dw_archive_title_delete.SetItem(1, "ttlinit_chno", "")
		dw_archive_title_delete.SetFocus()
		dw_archive_title_delete.Setcolumn("ttlinit_chno")
		RETURN
	END IF
	  SELECT mchar.chno  
	    INTO :ls_dummy 
	    FROM mchar  
  	 WHERE mchar.chno = :ls_chno
		USING SQLservertrans ;

	IF ls_dummy <> ls_chno THEN
		MessageBox("Invalid Chart Number", "The Chart Number is " + &
			"invalid.  Please reenter the correct chart number")
		dw_archive_title_delete.SetItem(1, "ttlinit_chno", "")
		dw_archive_title_delete.SetFocus()
		dw_archive_title_delete.Setcolumn("ttlinit_chno")
		RETURN
	ELSE
		is_chno = ls_chno
		dw_archive_title_delete.Object.DataWindow.Table.Select = is_syntax[1]
		dw_medium_deletion.Object.DataWindow.Table.Select = is_syntax[3]
		
		dw_archive_title_delete.Event pfc_retrieve()
		dw_medium_deletion.Event pfc_retrieve()
		
		dw_archive_title_delete.Object.Datawindow.Readonly = TRUE
		This.Enabled = FALSE
		cb_confirm.Enabled = TRUE
		cb_clear.Enabled = TRUE
		cb_delete.Enabled = TRUE
		w_pics_main.SetMicroHelp("Ready")
	END IF
END IF
ll_count = UPPERBOUND(il_bkseq[])
IF ll_count > 0 THEN
	FOR i= 1 TO ll_count
		il_bkseq[i] = 0
	NEXT
END IF
ll_count = UPPERBOUND(is_connum[])
IF ll_count > 0 THEN
	FOR i = 1 TO ll_count
		is_connum[i] = ""
	NEXT
END IF
is_chno_del = ""
cb_find.Default = FALSE

end event

type cb_confirm from u_cb within w_media_deletion
event pfc_hinttext pbm_mousemove
string tag = "press this button to save changes to the database"
integer x = 1463
integer y = 1184
integer taborder = 0
integer textsize = -10
string text = "Co&nfirm"
end type

event pfc_hinttext;call super::pfc_hinttext;w_pics_main.setmicrohelp(THIS.tag)
end event

event clicked;call super::clicked;integer rtn

IF dw_medium_deletion.ModifiedCount( ) + dw_medium_deletion.Deletedcount() = 0 THEN
	RETURN
END IF
 rtn = parent.Event pfc_save()
 IF rtn = 1 THEN
	COMMIT USING sqlservertrans;
	Close(w_pics_update_msg_box)
	Messagebox("Update","Record(s) updated succesfully")
ELSE
	ROLLBACK USING sqlservertrans;
	Close(w_pics_update_msg_box)
	Messagebox("Error","Updating of the record(s) failed")
	ib_disableclosequery = TRUE
	Parent.Event pfc_close()	
END IF

	
end event

type cb_clear from u_cb within w_media_deletion
event pfc_hinttext pbm_mousemove
string tag = "Clears the Screen"
integer x = 1915
integer y = 1184
integer taborder = 0
integer textsize = -10
string text = "&Clear"
end type

event pfc_hinttext;call super::pfc_hinttext;w_pics_main.setmicrohelp(THIS.tag)
end event

event clicked;call super::clicked;dw_archive_title_delete.reset()
dw_medium_deletion.reset()
dw_archive_title_delete.Event pfc_addrow()
dw_archive_title_delete.Object.Datawindow.Readonly = FALSE
cb_find.enabled = TRUE
cb_confirm.Enabled = FALSE
cb_clear.Enabled = FALSE
cb_delete.Enabled = FALSE
ib_disableclosequery = FALSE
dw_archive_title_delete.setfocus()
cb_find.Default = TRUE
dw_medium_deletion.Visible = FALSE


end event

type cb_close from u_cb within w_media_deletion
event pfc_hinttext pbm_mousemove
string tag = "Exits the Window"
integer x = 2345
integer y = 1184
integer taborder = 0
integer textsize = -10
string text = "E&xit"
end type

event pfc_hinttext;call super::pfc_hinttext;w_pics_main.setmicrohelp(THIS.tag)
end event

event clicked;call super::clicked;parent.Event pfc_close()
m_pics_main.m_menu.popMenu(300,0)
end event

type cb_delete from u_cb within w_media_deletion
event pfc_hinttext pbm_mousemove
string tag = "first highlight the row then press this button to delete the record(s)from the database "
integer x = 1015
integer y = 1184
integer taborder = 0
integer textsize = -10
string text = "&Delete"
end type

event pfc_hinttext;call super::pfc_hinttext;w_pics_main.setmicrohelp(THIS.tag)
end event

event clicked;call super::clicked;long ll_row, rtn,  ll_count
boolean ib_found
integer i = 1

ll_count = dw_medium_deletion.Rowcount()
ll_row = dw_medium_deletion.getselectedrow(ll_row)
IF ll_row > 0 THEN
	rtn = Messagebox("Deleting Highlighted Record(s)","To complete the deletion,press the confirm button",Question!,YESNO!,1)
	dw_medium_deletion.setfocus()
	is_chno_del = dw_medium_deletion.getitemstring(ll_row,"chno")
	ib_found = TRUE
	IF rtn = 1 THEN
		DO WHILE ib_found
			ll_row = 0
			ll_row = dw_medium_deletion.getselectedrow(ll_row)
			IF ll_row > 0 THEN
				is_connum[i] = dw_medium_deletion.getitemstring(ll_row,"mchar_conno")
				il_bkseq[i] = dw_medium_deletion.getitemnumber(ll_row,"bkseq")
				dw_medium_deletion.deleterow(ll_row)
				i++
			ELSE
				ib_found = FALSE
			END IF
		LOOP
	ELSE
		RETURN
	END IF
ELSE
	Messagebox("Invalid Record Selection","Please Select a Record before Clicking on the Delete Button")
	dw_medium_deletion.setfocus()
END IF
end event

type dw_medium_deletion from u_dw within w_media_deletion
event pfc_hinttext pbm_mousemove
event ue_enterkey pbm_dwnprocessenter
event pfc_keydown pbm_dwnkey
integer x = 14
integer y = 600
integer width = 2697
integer height = 496
integer taborder = 20
string dataobject = "d_media_deletion"
end type

event pfc_hinttext;call super::pfc_hinttext;string ls_object, ls_column, ls_column_tag
long ll_pos

//This script set's microhelp at the bottom of the screen for the Medium Deletion Datawindow
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

event ue_enterkey;call super::ue_enterkey;//Send(Handle(this),256,9,Long(0,0))
//return(1)
end event

event pfc_keydown;call super::pfc_keydown;long ll_row

IF Key = KeyDownArrow! THEN
	ll_row = THIS.getrow()
	THIS.Scrolltorow(ll_row)
END IF
IF Key = Keys! THEN
	ll_row = THIS.getrow()
	IF dw_medium_deletion.ISSelected(ll_row) THEN
		dw_medium_deletion.Selectrow(ll_row,FALSE)
	ELSE
		dw_medium_deletion.Selectrow(ll_row,TRUE)
	END IF
END IF
	
end event

event constructor;call super::constructor;dw_medium_deletion.of_settransobject(SQLservertrans)
Insertrow(0)
ib_rmbmenu = FALSE
of_setrowselect(TRUE)
inv_rowselect.of_setstyle(1)
THIS.object.datawindow.readonly = 'YES'
dw_medium_deletion.setrowfocusindicator(Hand!)


end event

event pfc_retrieve;call super::pfc_retrieve;This.Reset()
return this.retrieve(is_chno)
end event

event sqlpreview;call super::sqlpreview;String ls_syntax

ls_syntax = sqlsyntax
end event

event retrievestart;call super::retrievestart;dw_medium_deletion.Visible = TRUE
w_pics_main.setmicrohelp("Retrieving the Record Please Wait......")
end event

event rowfocuschanged;call super::rowfocuschanged;currentrow = currentrow
end event

event retrieveend;call super::retrieveend;dw_medium_deletion.setfocus()
end event

event getfocus;call super::getfocus;m_pics_main.m_Edit.m_Addrow.Enabled = FALSE
m_pics_main.m_Edit.m_Deleterow.Enabled = FALSE
m_pics_main.m_Edit.m_Cut.Enabled = FALSE

end event


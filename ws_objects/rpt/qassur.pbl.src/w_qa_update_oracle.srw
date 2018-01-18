$PBExportHeader$w_qa_update_oracle.srw
forward
global type w_qa_update_oracle from w_sheet
end type
type cb_spellcheck from commandbutton within w_qa_update_oracle
end type
type st_stages from statictext within w_qa_update_oracle
end type
type cb_upd_infx from commandbutton within w_qa_update_oracle
end type
type cb_unall from commandbutton within w_qa_update_oracle
end type
type cb_all from commandbutton within w_qa_update_oracle
end type
type dw_qa_qacompdt from u_pics_dw within w_qa_update_oracle
end type
type st_1 from statictext within w_qa_update_oracle
end type
type cb_update from commandbutton within w_qa_update_oracle
end type
type cb_edit from commandbutton within w_qa_update_oracle
end type
type cb_exit from commandbutton within w_qa_update_oracle
end type
type gb_qa_stages from groupbox within w_qa_update_oracle
end type
type dw_qa_books_not_in_oracle_sum from u_pics_dw within w_qa_update_oracle
end type
type dw_qa_books_not_in_oracle_detail from u_pics_dw within w_qa_update_oracle
end type
end forward

global type w_qa_update_oracle from w_sheet
integer width = 3017
integer height = 1848
cb_spellcheck cb_spellcheck
st_stages st_stages
cb_upd_infx cb_upd_infx
cb_unall cb_unall
cb_all cb_all
dw_qa_qacompdt dw_qa_qacompdt
st_1 st_1
cb_update cb_update
cb_edit cb_edit
cb_exit cb_exit
gb_qa_stages gb_qa_stages
dw_qa_books_not_in_oracle_sum dw_qa_books_not_in_oracle_sum
dw_qa_books_not_in_oracle_detail dw_qa_books_not_in_oracle_detail
end type
global w_qa_update_oracle w_qa_update_oracle

type variables
boolean ib_edit=TRUE
end variables

on w_qa_update_oracle.create
int iCurrent
call super::create
this.cb_spellcheck=create cb_spellcheck
this.st_stages=create st_stages
this.cb_upd_infx=create cb_upd_infx
this.cb_unall=create cb_unall
this.cb_all=create cb_all
this.dw_qa_qacompdt=create dw_qa_qacompdt
this.st_1=create st_1
this.cb_update=create cb_update
this.cb_edit=create cb_edit
this.cb_exit=create cb_exit
this.gb_qa_stages=create gb_qa_stages
this.dw_qa_books_not_in_oracle_sum=create dw_qa_books_not_in_oracle_sum
this.dw_qa_books_not_in_oracle_detail=create dw_qa_books_not_in_oracle_detail
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.cb_spellcheck
this.Control[iCurrent+2]=this.st_stages
this.Control[iCurrent+3]=this.cb_upd_infx
this.Control[iCurrent+4]=this.cb_unall
this.Control[iCurrent+5]=this.cb_all
this.Control[iCurrent+6]=this.dw_qa_qacompdt
this.Control[iCurrent+7]=this.st_1
this.Control[iCurrent+8]=this.cb_update
this.Control[iCurrent+9]=this.cb_edit
this.Control[iCurrent+10]=this.cb_exit
this.Control[iCurrent+11]=this.gb_qa_stages
this.Control[iCurrent+12]=this.dw_qa_books_not_in_oracle_sum
this.Control[iCurrent+13]=this.dw_qa_books_not_in_oracle_detail
end on

on w_qa_update_oracle.destroy
call super::destroy
destroy(this.cb_spellcheck)
destroy(this.st_stages)
destroy(this.cb_upd_infx)
destroy(this.cb_unall)
destroy(this.cb_all)
destroy(this.dw_qa_qacompdt)
destroy(this.st_1)
destroy(this.cb_update)
destroy(this.cb_edit)
destroy(this.cb_exit)
destroy(this.gb_qa_stages)
destroy(this.dw_qa_books_not_in_oracle_sum)
destroy(this.dw_qa_books_not_in_oracle_detail)
end on

event open;call super::open;// Open the sheet in Maximized mode
this.windowstate = maximized!



end event

event pfc_preopen;call super::pfc_preopen;string Luserid
Luserid = SQLserverTrans.userid

this.of_SetPreference(TRUE)
this.inv_preference.of_SetToolBars(TRUE)
this.inv_preference.of_SetWindow(TRUE)
this.of_SetResize(TRUE)
this.inv_resize.of_SetOrigSize(this.workspacewidth(),this.workspaceheight())


inv_resize.of_Register(dw_qa_qacompdt, "Scale")
inv_resize.of_Register(dw_qa_books_not_in_oracle_sum, "Scale")
inv_resize.of_Register(dw_qa_books_not_in_oracle_detail, "Scale")

inv_resize.of_Register(cb_exit, "Scale")
inv_resize.of_Register(cb_edit, "Scale")
inv_resize.of_Register(cb_update, "Scale")
inv_resize.of_Register(cb_upd_infx, "Scale")
inv_resize.of_Register(cb_all, "Scale")
inv_resize.of_Register(cb_unall, "Scale")

inv_resize.of_Register(st_1, "Scale")
inv_resize.of_Register(st_stages, "Scale")
inv_resize.of_Register(gb_qa_stages, "Scale")

//02/21/2008
inv_resize.of_Register(cb_spellcheck, "Scale")

//IF TRIM(Luserid) <> "dsmi" OR TRIM(Luserid) <> "tmcl" OR  TRIM(Luserid) <> "mich" THEN
//	cb_update.visible = FALSE
//ELSE
//	cb_update.visible = TRUE
//END IF
//
end event

event resize;call super::resize;long ll_height

This.X = w_pics_main.X
This.Y = w_pics_main.Y
ll_height = w_pics_main.mdi_1.Height
This.Resize(w_pics_main.WorkSpaceWidth(), ll_height)

end event

event closequery;call super::closequery;ib_disableclosequery = TRUE
end event

type cb_spellcheck from commandbutton within w_qa_update_oracle
integer x = 480
integer y = 1612
integer width = 370
integer height = 112
integer taborder = 30
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "Spellcheck"
end type

event clicked;//////////////////////////////////////////////////////////////////////////////////////////////////////////
//
//	Event: clicked for cb_spellcheck
//
//	Description:
//	Spellcheck for comments
//
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//	
//	Revision History
//
//	Developed by 	Date 				Version						Tracking#
//									
// Murali K.			02/20/2008      005 PICS Modifications	 Reqs: QAS a.8.1
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//

nca_word lnca_word
String ls_S, ls_dw
	
// Check for any pending updates
IF of_UpdateChecks( ) < 0 THEN Return -1

IF ib_edit = FALSE THEN
	dw_qa_books_not_in_oracle_sum.accepttext()
	ls_S =  dw_qa_books_not_in_oracle_sum.object.qacomments[dw_qa_books_not_in_oracle_sum.getrow()] 
	IF NOT(IsNull(ls_S)) THEN
		lnca_Word.SpellCheck( ls_S )
	    dw_qa_books_not_in_oracle_sum.object.qacomments[dw_qa_books_not_in_oracle_sum.getrow()] = ls_S 
	END IF
ELSE
	dw_qa_books_not_in_oracle_detail.accepttext()
	ls_S =  dw_qa_books_not_in_oracle_detail.object.qacomments[dw_qa_books_not_in_oracle_detail.getrow()] 
	IF NOT(IsNull(ls_S)) THEN
		lnca_Word.SpellCheck( ls_S )
	    dw_qa_books_not_in_oracle_detail.object.qacomments[dw_qa_books_not_in_oracle_detail.getrow()] = ls_S 
	END IF
END IF

  

end event

type st_stages from statictext within w_qa_update_oracle
boolean visible = false
integer x = 1344
integer y = 56
integer width = 2167
integer height = 60
integer textsize = -8
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "0-Autotest 1-Narration 2-Cassette/Braille 3-Intermaster 4-MP3 5-AMR/DRB 6-Flash"
boolean focusrectangle = false
end type

type cb_upd_infx from commandbutton within w_qa_update_oracle
integer x = 1632
integer y = 1612
integer width = 494
integer height = 112
integer taborder = 20
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "Update PICS"
end type

event clicked;int rtn
datetime ldt_date

rtn = dw_qa_books_not_in_oracle_detail.AcceptText()
//ldt_date=dw_qa_books_not_in_oracle_sum.object.qarecdt[1]
IF rtn = 1 THEN
	rtn = dw_qa_books_not_in_oracle_detail.event pfc_update(TRUE, TRUE)
	IF rtn = 1 THEN
	 COMMIT USING SQLServerTrans;
	 MessageBox("Update"," Records updated in PICS.")
	ELSE
	 ROLLBACK USING SQlServerTrans;
	 MessageBox("ERROR","Error in update in PICS.")
	END IF
END IF
	
	



end event

type cb_unall from commandbutton within w_qa_update_oracle
integer x = 891
integer y = 1612
integer width = 416
integer height = 112
integer taborder = 30
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "Un&select All"
end type

event clicked;int llrow

FOR llrow = 1 TO dw_qa_books_not_in_oracle_sum.rowcount()

	dw_qa_books_not_in_oracle_sum.object.ancntr_cntrcvcd[llrow] = 'N'
	
NEXT
end event

type cb_all from commandbutton within w_qa_update_oracle
integer x = 37
integer y = 1612
integer width = 361
integer height = 112
integer taborder = 20
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "S&elect All"
end type

event clicked;int llrow

FOR llrow = 1 TO dw_qa_books_not_in_oracle_sum.rowcount()

	dw_qa_books_not_in_oracle_sum.object.ancntr_cntrcvcd[llrow] = 'Y'
	
NEXT
end event

type dw_qa_qacompdt from u_pics_dw within w_qa_update_oracle
integer x = 805
integer y = 16
integer width = 480
integer height = 104
integer taborder = 10
string dataobject = "d_qa_qacompdt"
boolean vscrollbar = false
boolean livescroll = false
end type

event ue_postconstructor;call super::ue_postconstructor;date lqacompdt

dw_qa_qacompdt.SetTransObject(sqlservertrans) 
dw_qa_qacompdt.Retrieve()

select max(qacompdt) into :lqacompdt
from qastg
using sqlservertrans;

// 09/03/2008
this.object.qacompdt[1] = lqacompdt

if f_check_dberror(sqlservertrans, "Selecting from QASTG") then
	dw_qa_books_not_in_oracle_sum.SetTransObject(sqlservertrans) 
	dw_qa_books_not_in_oracle_sum.Retrieve(lqacompdt)
end if

dw_qa_books_not_in_oracle_detail.Visible = FALSE
dw_qa_books_not_in_oracle_sum.Visible = TRUE
dw_qa_qacompdt.SetFocus()

end event

event itemchanged;call super::itemchanged;string ls_data
date ld_date
datetime ldt_date

ls_data=data
ld_date=date(data)
dw_qa_books_not_in_oracle_sum.SetTransObject(sqlservertrans)
dw_qa_books_not_in_oracle_sum.Retrieve(date(data))

end event

type st_1 from statictext within w_qa_update_oracle
integer x = 41
integer y = 44
integer width = 823
integer height = 64
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 33554432
long backcolor = 67108864
string text = "Please select a review date:"
boolean focusrectangle = false
end type

type cb_update from commandbutton within w_qa_update_oracle
integer x = 2158
integer y = 1612
integer width = 416
integer height = 112
integer taborder = 10
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "&Update Web"
end type

event clicked;//////////////////////////////////////////////////////////////////////////////////////////////////////////
//
//	Event: clicked  of  cb_update
//
//	Description:
//	Update prod to oracle
//
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//	
//	Revision History
//
//	Developed by 	Date 				Version						Tracking#
//									
// Murali K.			02/01/2008      005 PICS Modifications	 
//											insert into prdrqastg failed because new audit
//											columns were added but not included in insert values
// Murali K. 			11/03/2008 		include specific columns in the insert to prevent  DB 
//											error code 947 - not enough values error
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//

int llrow, lcnt, rowsupdated = 0, rowsinserted = 0, rtn
long lbkseq, ll_testno
string lbkmed, lcntr, lqastg, lqastatcd, lbkno, lprdr, lqacomments, lsubprdr, lflag, lqainit, ls_user
date lqarecdt, lqacompdt, ldt_today
boolean upd_rec=FALSE

// 02/01/2008
ls_user = gnv_app.of_getuserid()
ldt_today = today()

rtn = MessageBox("Updating Web","This process will update all the records that are flaged for update (To Web is checked), and place them in Web. Continue?",Question!,YesNo!,1)
IF rtn = 1 THEN

	FOR llrow = 1 TO dw_qa_books_not_in_oracle_sum.RowCount()
		
		openwithparm(w_pics_retrieve_msg_box,"Inserting/Updating Records into Web, Please Wait...")	
		
		lbkseq = dw_qa_books_not_in_oracle_sum.object.bkseq[llrow]
		lbkmed = TRIM(dw_qa_books_not_in_oracle_sum.object.bkmed[llrow])
		lbkno = lbkmed + string(lbkseq)
		lcntr = TRIM(dw_qa_books_not_in_oracle_sum.object.cntr[llrow])
		lqastg = TRIM(dw_qa_books_not_in_oracle_sum.object.qastg[llrow])
		lqarecdt = date(dw_qa_books_not_in_oracle_sum.object.qastg_qarecdt[llrow])
		lqacompdt = date(dw_qa_books_not_in_oracle_sum.object.qastg_qacompdt[llrow])
		lqastatcd = TRIM(dw_qa_books_not_in_oracle_sum.object.qastg_qastatcd[llrow])
		lprdr = TRIM(dw_qa_books_not_in_oracle_sum.object.prdr[llrow])
		lqacomments = TRIM(dw_qa_books_not_in_oracle_sum.object.qacomments[llrow])
		lqainit = TRIM(dw_qa_books_not_in_oracle_sum.object.qainit[llrow])
		// 11/03/2008 include test_no also
		ll_testno = dw_qa_books_not_in_oracle_sum.object.qastg_test_no[llrow]
		
		// Check to see if the user wants to update this row
		lflag = TRIM(dw_qa_books_not_in_oracle_sum.object.ancntr_cntrcvcd[llrow])
		
		IF lflag = 'Y' THEN
			upd_rec = TRUE
		ELSE
			upd_rec = FALSE
		END IF
		
		select max(subprdr) into :Lsubprdr
		from sub
		where cntr = :Lcntr
		using sqlservertrans;
		IF f_check_dberror(SQLServerTrans, "sub")=FALSE THEN
			ROLLBACK USING SQLServerOracleTrans;
			ROLLBACK USING sqlservertrans;
		END IF
		
		lcnt = 0
						
		SELECT COUNT(*)
		INTO :lcnt
		FROM PRDRQASTG
		WHERE BKMED = :lbkmed
		AND BKSEQ = :lbkseq
		AND CNTR = :lcntr
		AND QASTG = :lqastg
		AND QASTATCD = :lqastatcd
		AND QARECDT = :lqarecdt
		USING SQLServerOracleTrans;
		IF f_check_dberror(SQLServerOracleTrans, "PRDRQASTG") THEN
			IF lcnt = 0 AND upd_rec THEN
				// Record does not exist and the user wants to update the row, therfore insert it into PRDRQASTG
				
				// 02/01/2008 add audit columns to the insert - oracle error
//				INSERT INTO PRDRQASTG
//				VALUES (:lbkmed, :lbkseq, :lbkno, :lcntr, :lqastg, :lqarecdt, :lqastatcd, :lqacompdt, :lprdr, :lsubprdr, :lqacomments,:lqainit, 'N', :ls_user, :ldt_today, :ls_user, :ldt_today)
				// 11/03/2008 include specific columns in the insert to prevent  DB error code 947 - not enough values error
				INSERT INTO PRDRQASTG 
				(BKMED,BKSEQ,BKNO,CNTR,QASTG,QARECDT,QASTATCD, QACOMPDT,PRDR,SUBPRDR,QACOMMENTS, QAINIT, SPECIALQAINFO, 
				CREATED_BY, CREATED_DATE,MODIFIED_BY, MODIFIED_DATE,TEST_NO)
				VALUES (:lbkmed, :lbkseq, :lbkno, :lcntr, :lqastg, :lqarecdt, :lqastatcd, :lqacompdt, :lprdr, :lsubprdr, :lqacomments,:lqainit, 'N',
				:ls_user, sysdate, :ls_user, sysdate,:ll_testno)
				USING SQLServerOracleTrans;
				IF f_check_dberror(SQLServerOracleTrans, "PRDRQASTG")=FALSE THEN
					close(w_pics_retrieve_msg_box)
					ROLLBACK USING SQLServerOracleTrans;
					MessageBox("ERROR","Error inserting into PRDRQASTG table in Oracle")
					RETURN
				ELSE
					rowsinserted++
				END IF
			ELSEIF lcnt > 0 AND upd_rec THEN
				// Record does exist and the user wants to update the row, therefore update PRDRQASTG 
				
				UPDATE PRDRQASTG
				SET QACOMPDT = :lqacompdt, PRDR = :lprdr, QACOMMENTS = :lqacomments, SUBPRDR = :lsubprdr, qainit=:lqainit, specialqainfo = 'N'
				WHERE BKMED = :lbkmed
				AND BKSEQ = :lbkseq
				AND CNTR = :lcntr
				AND QASTG = :lqastg
				AND QASTATCD = :lqastatcd
				AND QARECDT = :lqarecdt
				USING SQLServerOracleTrans;
				IF f_check_dberror(SQLServerOracleTrans, "PRDRQASTG")=FALSE THEN
					close(w_pics_retrieve_msg_box)
					ROLLBACK USING SQLServerOracleTrans;
					MessageBox("ERROR","Error updating PRDRQASTG table in Oracle")
					RETURN
				ELSE
					rowsupdated++
				END IF
				
			END IF
		END IF
		
	NEXT
	
	IF f_check_dberror(SQLServerOracleTrans, "PRDRQASTG") THEN
		COMMIT USING SQLServerOracleTrans;
		MessageBox("UPDATE","Total number of records inserted = "+string(rowsinserted)+" , and total number of records updated = "+string(rowsupdated))
	END IF	
	
	close(w_pics_retrieve_msg_box)
END IF

end event

type cb_edit from commandbutton within w_qa_update_oracle
integer x = 1335
integer y = 1612
integer width = 265
integer height = 112
integer taborder = 10
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "&Edit..."
end type

event clicked;int llrow

IF dw_qa_books_not_in_oracle_detail.visible = TRUE THEN
	dw_qa_books_not_in_oracle_detail.visible = FALSE
	dw_qa_books_not_in_oracle_sum.visible = TRUE
	cb_edit.text ='Edit...'
	ib_edit=FALSE
ELSE
	llrow = dw_qa_books_not_in_oracle_sum.GetRow()
	//MessageBox("row",string(llrow))
	dw_qa_books_not_in_oracle_sum.visible = FALSE
	dw_qa_books_not_in_oracle_detail.visible = TRUE
	dw_qa_books_not_in_oracle_detail.ScrollToRow(llrow)	
	cb_edit.text ='Summary...'
	ib_edit=TRUE
END IF
end event

type cb_exit from commandbutton within w_qa_update_oracle
integer x = 2633
integer y = 1612
integer width = 315
integer height = 112
integer taborder = 10
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "E&xit"
end type

event clicked;dw_qa_books_not_in_oracle_sum.ResetUpdate() // Clear update flags
dw_qa_qacompdt.ResetUpdate() 
close(parent)

end event

type gb_qa_stages from groupbox within w_qa_update_oracle
boolean visible = false
integer x = 1330
integer y = 8
integer width = 1591
integer height = 128
integer taborder = 20
integer textsize = -9
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "QA Stage number information"
end type

type dw_qa_books_not_in_oracle_sum from u_pics_dw within w_qa_update_oracle
integer x = 37
integer y = 152
integer width = 2917
integer height = 1440
integer taborder = 10
string dataobject = "d_qa_books_not_in_oracle_sum"
end type

event ue_postconstructor;call super::ue_postconstructor;string ls_excludecols[]
pointer oldpointer // Declares a pointer variable

oldpointer = SetPointer(HourGlass!)

dw_qa_books_not_in_oracle_sum.of_SetLinkage(TRUE)

dw_qa_books_not_in_oracle_sum.of_SetTransObject(SQLServerTrans)
//
dw_qa_books_not_in_oracle_sum.of_Setfilter(TRUE)
dw_qa_books_not_in_oracle_sum.of_Setfind(TRUE)
dw_qa_books_not_in_oracle_sum.of_SetRowManager(TRUE)
dw_qa_books_not_in_oracle_sum.inv_filter.of_SetStyle(1)
dw_qa_books_not_in_oracle_sum.inv_filter.of_SetColumnNameSource(2)
ls_excludecols[1] = "graycolor"
ls_excludecols[2] = "whitecolor"
dw_qa_books_not_in_oracle_sum.inv_filter.of_SetExclude(ls_excludecols)
dw_qa_books_not_in_oracle_sum.of_SetSort(TRUE)
dw_qa_books_not_in_oracle_sum.inv_sort.of_SetStyle(1)
dw_qa_books_not_in_oracle_sum.inv_sort.of_SetColumnNameSource(2)
dw_qa_books_not_in_oracle_sum.inv_sort.of_SetExclude(ls_excludecols)
dw_qa_books_not_in_oracle_sum.SetRowFocusIndicator(Hand!)




end event

event retrieveend;call super::retrieveend;int llrow, lcnt
long lbkseq
string lbkmed, lcntr, lqastg, lqastatcd
date lqarecdt
string ls_stg, ls_rej

FOR llrow = 1 TO rowcount
		// unsupported appeon feature retrieverow code moved 3/29/10
		ls_stg = this.object.qastg[llrow]
		ls_rej = this.object.qastg_qastatcd[llrow]
		
		IF  ls_stg = '6' and ls_rej ='R' THEN
			this.object.qastg_reload_indicator_yn[llrow] ='Y'
		END IF
		//// above moved from retrieverow
		
	lbkseq = dw_qa_books_not_in_oracle_sum.object.bkseq[llrow]
	lbkmed = TRIM(dw_qa_books_not_in_oracle_sum.object.bkmed[llrow])
	lcntr = TRIM(dw_qa_books_not_in_oracle_sum.object.cntr[llrow])
	lqastg = TRIM(dw_qa_books_not_in_oracle_sum.object.qastg[llrow])
	lqarecdt = date(dw_qa_books_not_in_oracle_sum.object.qastg_qarecdt[llrow])
	lqastatcd = TRIM(dw_qa_books_not_in_oracle_sum.object.qastg_qastatcd[llrow])
	
	lcnt = 0
					
	SELECT COUNT(*)
	INTO :lcnt
	FROM PRDRQASTG
	WHERE BKMED = :lbkmed
	AND BKSEQ = :lbkseq
	AND CNTR = :lcntr
	AND QASTG = :lqastg
	AND QASTATCD = :lqastatcd
	AND QARECDT = :lqarecdt
	USING SQLServerOracleTrans;
	IF f_check_dberror(SQLServerOracleTrans, "PRDRQASTG") THEN
		IF lcnt = 0 THEN
			dw_qa_books_not_in_oracle_sum.object.ancntr_cntrcvcd[llrow] = 'Y'
			dw_qa_books_not_in_oracle_sum.object.ancntr_cntrtype[llrow] = 'N'
		ELSE
			dw_qa_books_not_in_oracle_sum.object.ancntr_cntrtype[llrow] = 'Y'
			dw_qa_books_not_in_oracle_sum.object.ancntr_cntrcvcd[llrow] = 'N'
		END IF
	END IF
	
NEXT

dw_qa_books_not_in_oracle_sum.ShareData(dw_qa_books_not_in_oracle_detail)

close(w_pics_retrieve_msg_box)

end event

event retrievestart;call super::retrievestart;openwithparm(w_pics_retrieve_msg_box,"Retrieving Information, Please Wait...")
end event

event doubleclicked;call super::doubleclicked;long CurRow

boolean result

CurRow = dw_qa_books_not_in_oracle_sum.GetRow()

result = dw_qa_books_not_in_oracle_sum.IsSelected(CurRow)

IF result THEN
		dw_qa_books_not_in_oracle_sum.SelectRow(CurRow, FALSE)

ELSE
		dw_qa_books_not_in_oracle_sum.SelectRow(CurRow, TRUE)

END IF

IF dw_qa_books_not_in_oracle_detail.visible = TRUE THEN
	dw_qa_books_not_in_oracle_detail.visible = FALSE
	dw_qa_books_not_in_oracle_sum.visible = TRUE
	cb_edit.text ='Edit...'
ELSE
	//MessageBox("row",string(CurRow))
	dw_qa_books_not_in_oracle_sum.visible = FALSE
	dw_qa_books_not_in_oracle_detail.visible = TRUE
	dw_qa_books_not_in_oracle_detail.ScrollToRow(CurRow)	
	cb_edit.text ='Summary...'
END IF
end event

type dw_qa_books_not_in_oracle_detail from u_pics_dw within w_qa_update_oracle
integer x = 50
integer y = 160
integer width = 2898
integer height = 1424
integer taborder = 20
string dataobject = "d_qa_books_not_in_oracle_detail"
end type


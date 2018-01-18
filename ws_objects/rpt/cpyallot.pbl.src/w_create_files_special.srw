$PBExportHeader$w_create_files_special.srw
forward
global type w_create_files_special from w_main
end type
type st_1 from statictext within w_create_files_special
end type
type dw_ds_pgm85_distsched_bklist_rowcnt from u_pics_dw within w_create_files_special
end type
type dw_dist from u_pics_dw within w_create_files_special
end type
type dw_distsched_inx from u_pics_dw within w_create_files_special
end type
type st_transupdate from statictext within w_create_files_special
end type
type dw_sp1_distsched_orl from u_pics_dw within w_create_files_special
end type
type sle_batch_date from singlelineedit within w_create_files_special
end type
type uo_progress from u_progressbar within w_create_files_special
end type
type cb_cancel from commandbutton within w_create_files_special
end type
type cb_go from commandbutton within w_create_files_special
end type
end forward

global type w_create_files_special from w_main
integer width = 1394
integer height = 888
string title = "Create Special Distribution Schedule"
boolean minbox = false
boolean maxbox = false
boolean resizable = false
windowtype windowtype = response!
event ue_cancel ( )
event ue_dist_file ( )
event ue_all ( )
event ue_single ( )
event ue_enterkey pbm_dwnprocessenter
event ue_sel_file ( )
event ue_bklist ( )
st_1 st_1
dw_ds_pgm85_distsched_bklist_rowcnt dw_ds_pgm85_distsched_bklist_rowcnt
dw_dist dw_dist
dw_distsched_inx dw_distsched_inx
st_transupdate st_transupdate
dw_sp1_distsched_orl dw_sp1_distsched_orl
sle_batch_date sle_batch_date
uo_progress uo_progress
cb_cancel cb_cancel
cb_go cb_go
end type
global w_create_files_special w_create_files_special

type variables
str_distrib_schedule istr
datastore ids_distsched
long i_count=0, i_books, i_libs, i_distfilerows, i_prdr
string is_yes_no='N'
boolean ib_ask_yn= false

end variables

forward prototypes
public subroutine wf_recreate_dist (boolean ab_both)
public function integer of_updatemcharqnty (datetime ad_sched)
end prototypes

event ue_cancel;call super::ue_cancel;close(this)
end event

event ue_dist_file();String ls_filename, ls_path, ls_scheddate, ls_m, ls_d, ls_y
Date ld_scheddate
str_distrib_schedule lstr

lstr= istr
ld_scheddate= lstr.ld_date1

ls_scheddate=String(ld_scheddate,'mm/dd/yy')
ls_m=Mid(ls_scheddate,1,2)
ls_d=Mid(ls_scheddate,4,2)
ls_y=Mid(ls_scheddate,7,2)
ls_filename="distrib"+ls_m+ls_d+ls_y+".txt"
//ls_filename = "distrib.txt"
ls_path =  ls_filename
IF GetFileSaveName("Select Distribution File",ls_path,ls_filename, "txt", "Text Files (*.txt),*.txt") <> 1 THEN
	Messagebox("File Error","Incorrect file name. Please try again.")
	RETURN
END IF

//st_dist_file.text = ls_path
end event

event ue_sel_file();String ls_filename, ls_path, ls_m, ls_d, ls_y, ls_scheddate
Date ld_scheddate
str_distrib_schedule lstr

lstr= istr
ld_scheddate= lstr.ld_date1

ls_scheddate=String(ld_scheddate,'mm/dd/yy')
ls_m=Mid(ls_scheddate,1,2)
ls_d=Mid(ls_scheddate,4,2)
ls_y=Mid(ls_scheddate,7,2)
ls_filename="selected"+ls_m+ls_d+ls_y+".txt"
//ls_filename = "selected.txt"
ls_path =  ls_filename
IF GetFileSaveName("Select Quantities File",ls_path,ls_filename, "txt", "Text Files (*.txt),*.txt") <> 1 THEN
	Messagebox("File Error","Incorrect file name, Please try again")
	RETURN
END IF

//st_sel_file.text = ls_path

//rb_bklist.checked = false
//rb_all.checked = true
end event

public subroutine wf_recreate_dist (boolean ab_both);str_distrib_schedule lstr
string ls_cfile, ls_sfile, ls_dsflag, ls_bklist[], ls_prdr, ls_bkmed, ls_bkseq, ls_bkno,&
		ls_usid, ls_libcd, ls_null,ls_oracle_ext_env, ls_bkmed2, ls_del
long rtn, li_row_count2, ll_count2, ll_rows, li_row_count, li_count, i, li_cur, li_ordqty,&
	li_re, li_bkseq ,li_bound, li_bklist[], k, j, li_bkseq2, li_delcnt
date ld_cabdt, ld_scheddate
datetime ldt_cur, ldt_cabdt, ldt_scheddate
boolean lb_done


lstr=istr
ld_scheddate= lstr.ld_date1
ldt_scheddate=datetime(ld_scheddate,time('00:00:00'))
String ls_msgtext[ ] = {" 0 %","10 %", "20 %", "30 %", "40 %", "50 %", "60 %", &
									"70 %", "80 %", "90 %", "100 %"}
	uo_progress.of_SetTextColor(RGB(255, 255, 255))
	SetNull(ls_dsflag)
//	li_bound= upperbound(w_special_distribution_schedule.ii_bkseq[])
	ls_bklist[]= lstr.arraymed[]
if ab_both=false then

	
	li_row_count = dw_ds_pgm85_distsched_bklist_rowcnt.retrieve(ls_bklist[])
	ll_count2= 0.1 * li_row_count

	this.SetRedraw(true)
	uo_progress.Visible=TRUE
	uo_progress.of_SetMinimum(0)
	uo_progress.of_SetMaximum(ll_count2)
	uo_progress.of_SetDisplayStyle(3)
	uo_progress.of_SetMessageText(ls_msgtext)
	uo_progress.of_SetPosition(0)
	st_transupdate.text= "                         Retrieving data ... "
	//select rows from ancntr,mchar,ttlinit, sched, prod,def where bkno in ls_bklist
	//and sched.ordqty>0 for transfer the rows to distsched of oracle
	dw_distsched_inx.dataobject = "d_ds_pgm85_distsched_bklist_my"
	dw_distsched_inx.settransobject(sqlservertrans)
	dw_distsched_inx.Retrieve(ls_bklist[])
end if//end if ab_both=false
ll_rows = dw_distsched_inx.RowCount()

ll_rows = dw_distsched_inx.RowCount()

	IF not SQLServerOracleTrans.DBHandle() >0 THEN
		SQLServerOracleTrans.of_connect() 
	END IF
	IF SqlServerOracleTrans.sqlcode <> 0 THEN
		IF SqlServerOracleTrans.SqlDbcode = -951 THEN            //check for invalid userid
		  MessageBox("Login Error","Invalid User ID/Password. Please re-enter.",StopSign!)
		  Return 	
		ELSEIF SqlServerOracleTrans.SqlDBcode = -952 THEN       //check for invalid password
		  MessageBox("Login Error","Invalid User ID/Password. Please re-enter.",StopSign!)
		  Return 
		Else                                             //check for other error messages
		MessageBox("Database Connection Error","Unable to Connect. Please try again." +& 
		string(SqlServerOracleTrans.sqldbcode) + " " +&
		SqlServerOracleTrans.SQLErrText, &
		StopSign!)
		Return 
	  END IF
	ELSe
		
		dw_sp1_distsched_orl.settransobject(sqlserveroracletrans)
	
		ll_rows=dw_distsched_inx.RowCount()
		li_count= 0.1 * ll_rows
		//this for each bkseq to find how many libaries are associated with
		dw_distsched_inx.setsort("prdr A, bkseq A, bkmed A, libcd A")
		dw_distsched_inx.sort()
			
		IF ll_rows = 0 THEN
			MessageBox("ERROR","System error while retrieving."+&
				"~nPlease contact system administrator.")
		ELSE
			this.SetRedraw(true)
			uo_progress.Visible=TRUE
			uo_progress.of_SetMinimum(0)
			uo_progress.of_SetMaximum(li_count)
			uo_progress.of_SetDisplayStyle(3)
			uo_progress.of_SetMessageText(ls_msgtext)
			uo_progress.of_SetPosition(0)
			st_transupdate.text="                     Loading data to the web. "
			//transfer each row from  to oracle
			for i = 1 to ll_rows
				li_cur=dw_sp1_distsched_orl.InsertRow(0)
				dw_sp1_distsched_orl.TriggerEvent('ue_addrow')
				// lets get all the values we'll need
				ls_prdr = trim(string(dw_distsched_inx.object.prdr[i]))
				IF IsNull(ls_prdr) OR ls_prdr="" THEN
					ls_prdr="Not Assigned Yet"
				END IF
				ls_bkmed = trim(string(dw_distsched_inx.object.bkmed[i]))
				ls_bkseq = trim(string(dw_distsched_inx.object.bkseq[i]))
				li_bkseq=long(ls_bkseq)
				ls_bkno=ls_bkmed+ls_bkseq
				li_ordqty = long(string(dw_distsched_inx.object.ordqty[i]))
				ls_libcd = string(dw_distsched_inx.object.libcd[i])
				ldt_cabdt = dw_distsched_inx.object.cabdt[i]
	
				dw_sp1_distsched_orl.SetItem(li_cur,'producer', ls_prdr)
				dw_sp1_distsched_orl.SetItem(li_cur,'cabdt', ldt_cabdt)
				dw_sp1_distsched_orl.SetItem(li_cur,'libcd', ls_libcd)
				dw_sp1_distsched_orl.SetItem(li_cur,'bkno', ls_bkno)
				dw_sp1_distsched_orl.SetItem(li_cur,'qty', li_ordqty)
				dw_sp1_distsched_orl.SetItem(li_cur,'bkmed', ls_bkmed)
				dw_sp1_distsched_orl.SetItem(li_cur,'bkseq', li_bkseq)
				dw_sp1_distsched_orl.SetItem(li_cur,'scheddate', ldt_scheddate)
				dw_sp1_distsched_orl.SetItem(li_cur,'dsflag', ls_null)
			next
			dw_sp1_distsched_orl.Sort()
			w_create_files_special.SetRedraw(TRUE)
			uo_progress.Visible=true
			uo_progress.of_SetMinimum(0)
			uo_progress.of_SetMaximum(li_count)
			uo_progress.of_SetDisplayStyle(3)
			uo_progress.of_SetMessageText(ls_msgtext)
			uo_progress.of_SetPosition(0)
		
			i_count=0
			st_transupdate.text="Updating the web database. This process will take several minutes."
			rtn = dw_sp1_distsched_orl.of_Update(TRUE,TRUE)
			st_transupdate.text=''
			w_create_files_special.SetRedraw(TRUE)
			uo_progress.visible=false
			//now is time to update dw_dsdtdsflag in w_special_distribution_schedule for mchar table
			li_re=w_special_distribution_schedule.dw_dsdtdsflag.RowCount()
			ldt_cur=datetime(today(),now())
			IF f_check_dberror(sqlserveroracletrans,"distsched at SP9") THEN
				IF rtn=1 THEN
					rtn=w_special_distribution_schedule.dw_dsdtdsflag.update()
					if rtn=1 then
						w_special_distribution_schedule.st_dist.text=&
												string(ldt_cur,'mm/dd/yyyy hh:mm:ss')
						w_special_distribution_schedule.cbx_distsched.checked=true
						Commit Using sqlserveroracletrans;
						Commit Using sqlservertrans;
						MessageBox("Distribution Schedule",string(ll_rows)+&
																" rows inserted into the web Database. ")
						lb_done=true
						
					else
						RollBack Using sqlserveroracletrans;
						RollBack Using sqlservertrans;
					end if
				else
					RollBack Using sqlserveroracletrans;
					RollBack Using sqlservertrans;
				end if
			ELSE
				MessageBox("Update Error","Error while trying to update the Oracle Database."+&
					"~nPlease contact system administration.")
				Rollback Using sqlserveroracletrans;
				RETURN
			END IF// f_check_dberror
			uo_progress.Visible=FALSE
		END IF// end if ll_rows=0	
	END IF //end if  SqlServerOracleTrans.sqlcode <> 0
	if lb_done=true then
		w_special_distribution_schedule.ib_web_done=true
	//	closewithreturn(w_create_files_special,'Y')
	else
		w_special_distribution_schedule.ib_web_done=false
	//	closewithReturn(w_create_files_special,'N')
	end if
	close(this)
end subroutine

public function integer of_updatemcharqnty (datetime ad_sched);// 07/30/2009 #2137
LONG ll_rc, ll_qty, ll_loop, ll_bk
string ls_bkmed


//ll_rc = dw_distsched_inx.Rowcount()

//FOR ll_loop = 1 TO ll_rc
//		ll_bk		 = dw_distsched_inx.object.bkseq[ll_loop]
//		ls_bkmed = dw_distsched_inx.object.bkmed[ll_loop]
//		
//		SELECT SUM(QTY) INTO :LL_QTY
//		FROM DISTSCHED@PIC_LINK
//		WHERE BKSEQ = :ll_bk AND
//				BKMED = :ls_bkmed USING SQLSERVERTRANS ;
//		
//		IF f_check_dberror(SQLserverTrans,"Summing qty from distsched for " + ls_bkmed + string(ll_bk)  ) THEN
//			UPDATE MCHAR
//			SET QNTY = :ll_qty
//			WHERE BKSEQ = :ll_bk AND
//				BKMED = :ls_bkmed USING SQLSERVERTRANS ;
//				
//			IF f_check_dberror(SQLserverTrans,"Updating mchar qty from distsched for " + ls_bkmed + string(ll_bk)  ) = FALSE THEN
//				RETURN -1
//			END IF		
//		END IF
//		
//NEXT
STRING LS_MSG
date ad

ad = date(ad_sched)

ls_msg = "Updating QNTY for the books..."
OpenWithParm(w_pics_retrieve_msg_box,ls_msg)	

UPDATE MCHAR M
SET M.QNTY = 	(	SELECT SUM(QTY) 		FROM DISTSCHED@PIC_LINK D
		WHERE D.BKSEQ = M.BKSEQ  AND
				D.BKMED = M.BKMED )
	WHERE M.BKMED || M.BKSEQ IN (SELECT DISTINCT BKMED || BKSEQ FROM DISTSCHED WHERE SCHEDDATE = :ad) USING SQLSERVERTRANS ;
	
IF f_check_dberror(SQLserverTrans,"Summing qty from distsched for SCHED DATE " + STRING(AD)) THEN	
	COMMIT USING SqlServerTrans;
ELSE
	ROLLBACK USING SQLSERVERTRANS ;
END IF

Close(w_pics_retrieve_msg_box)	

RETURN 1
end function

on w_create_files_special.create
int iCurrent
call super::create
this.st_1=create st_1
this.dw_ds_pgm85_distsched_bklist_rowcnt=create dw_ds_pgm85_distsched_bklist_rowcnt
this.dw_dist=create dw_dist
this.dw_distsched_inx=create dw_distsched_inx
this.st_transupdate=create st_transupdate
this.dw_sp1_distsched_orl=create dw_sp1_distsched_orl
this.sle_batch_date=create sle_batch_date
this.uo_progress=create uo_progress
this.cb_cancel=create cb_cancel
this.cb_go=create cb_go
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_1
this.Control[iCurrent+2]=this.dw_ds_pgm85_distsched_bklist_rowcnt
this.Control[iCurrent+3]=this.dw_dist
this.Control[iCurrent+4]=this.dw_distsched_inx
this.Control[iCurrent+5]=this.st_transupdate
this.Control[iCurrent+6]=this.dw_sp1_distsched_orl
this.Control[iCurrent+7]=this.sle_batch_date
this.Control[iCurrent+8]=this.uo_progress
this.Control[iCurrent+9]=this.cb_cancel
this.Control[iCurrent+10]=this.cb_go
end on

on w_create_files_special.destroy
call super::destroy
destroy(this.st_1)
destroy(this.dw_ds_pgm85_distsched_bklist_rowcnt)
destroy(this.dw_dist)
destroy(this.dw_distsched_inx)
destroy(this.st_transupdate)
destroy(this.dw_sp1_distsched_orl)
destroy(this.sle_batch_date)
destroy(this.uo_progress)
destroy(this.cb_cancel)
destroy(this.cb_go)
end on

event open;call super::open;
this.of_SetBase(true)
this.inv_base.of_Center()
IF  SQLServerOracleTrans.DBHandle() >0 THEN
	IF SQLServerOracleTrans.of_disconnect() < 0 THEN
		MessageBox("Error","Disconnected from the Oracle Database. Please try again",StopSign!)
	END IF
END IF

end event

event pfc_postopen;call super::pfc_postopen;str_distrib_schedule lstr
long li_bound, i
string ls_bkno_list[], ls_nocntr

lstr=Message.PowerObjectParm
istr= lstr
ls_nocntr= lstr.ls_contract
if ls_nocntr='Recreate' then
//	cb_dist_file.visible=false
//	cb_sel_file.visible=false
//	gb_1.visible=false
//	gb_sel.visible=false
//	st_dist_file.visible=false
//	st_sel_file.visible=false
//	r_dist.visible=true
//	rb_file.visible=true
//	rb_dist.visible=true
//	rb_both.visible=true
	this.title='Recreate Distribution Schedule'
//	cb_go.enabled=false
else
//	cb_dist_file.visible=true
//	cb_sel_file.visible=true
//	gb_1.visible=true
//	gb_sel.visible=true
//	st_dist_file.visible=true
//	st_sel_file.visible=true
//	r_dist.visible=false
//	rb_file.visible=false
//	rb_dist.visible=false
//	rb_both.visible=false
	this.title='Create Distribution Schedule'
end if


end event

type st_1 from statictext within w_create_files_special
integer x = 256
integer y = 128
integer width = 1024
integer height = 64
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long backcolor = 79741120
string text = "Please Click the Process button"
boolean focusrectangle = false
end type

type dw_ds_pgm85_distsched_bklist_rowcnt from u_pics_dw within w_create_files_special
boolean visible = false
integer x = 622
integer y = 32
integer width = 55
integer height = 64
integer taborder = 20
string dataobject = "d_ds_pgm85_distsched_bklist_rowcnt"
boolean vscrollbar = false
boolean livescroll = false
end type

event constructor;call super::constructor;THIS.settransobject(sqlservertrans)

end event

event sqlpreview;call super::sqlpreview;//i_count++
//if mod(i_count,1000)=0 then
//		parent.SetRedraw(TRUE)
//	else
//		parent.SetRedraw(false)
//	end if
end event

event retrievestart;call super::retrievestart;OpenWithParm(w_pics_retrieve_msg_box,"Getting book counts, Please Wait...")

end event

event retrieveend;call super::retrieveend;close(w_pics_retrieve_msg_box)
end event

type dw_dist from u_pics_dw within w_create_files_special
boolean visible = false
integer x = 512
integer y = 32
integer width = 55
integer height = 64
integer taborder = 210
string dataobject = "d_dist"
boolean vscrollbar = false
boolean livescroll = false
end type

event constructor;call super::constructor;THIS.settransobject(sqlservertrans)

end event

type dw_distsched_inx from u_pics_dw within w_create_files_special
boolean visible = false
integer x = 178
integer y = 56
integer width = 50
integer height = 64
integer taborder = 200
string dataobject = "d_ds_pgm85_distsched_bklist_my"
boolean vscrollbar = false
boolean livescroll = false
end type

event constructor;call super::constructor;THIS.settransobject(sqlservertrans)

end event

event sqlpreview;call super::sqlpreview;//i_count++
//if mod(i_count,100)=0 then
//		parent.SetRedraw(TRUE)
//	else
//		parent.SetRedraw(false)
//	end if
end event

event retrievestart;call super::retrievestart;openwithparm(w_pics_retrieve_msg_box,"Analyzing data from ancntr, mchar, prod, sched and lib tables , Please Wait...")
end event

event retrieveend;call super::retrieveend;w_create_files_special.SetRedraw(TRUE)
w_special_distribution_schedule.SetRedraw(TRUE)
w_pics_retrieve_msg_box.SetRedraw(TRUE)

close(w_pics_retrieve_msg_box)


end event

type st_transupdate from statictext within w_create_files_special
integer x = 41
integer y = 892
integer width = 1285
integer height = 124
integer textsize = -8
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 33554432
long backcolor = 67108864
boolean focusrectangle = false
end type

type dw_sp1_distsched_orl from u_pics_dw within w_create_files_special
event ue_addrow ( )
boolean visible = false
integer x = 311
integer y = 48
integer width = 50
integer height = 68
integer taborder = 10
string dataobject = "d_sp1_distsched_orl"
boolean hscrollbar = true
boolean hsplitscroll = true
boolean livescroll = false
end type

event constructor;call super::constructor;this.settransobject(sqlserveroracletrans)

end event

type sle_batch_date from singlelineedit within w_create_files_special
boolean visible = false
integer x = 101
integer y = 56
integer width = 50
integer height = 64
integer taborder = 50
boolean bringtotop = true
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
boolean autohscroll = false
borderstyle borderstyle = stylelowered!
end type

type uo_progress from u_progressbar within w_create_files_special
integer x = 73
integer y = 512
integer width = 1207
integer height = 92
integer taborder = 180
boolean border = true
borderstyle borderstyle = stylelowered!
end type

on uo_progress.destroy
call u_progressbar::destroy
end on

type cb_cancel from commandbutton within w_create_files_special
integer x = 768
integer y = 640
integer width = 247
integer height = 96
integer taborder = 140
boolean bringtotop = true
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "&Cancel"
boolean cancel = true
end type

event clicked;IF NOT SQLServerOracleTrans.DBHandle() =0 THEN
	IF SQLServerOracleTrans.of_disconnect() < 0 THEN
		MessageBox("Error","Disconnected from the Oracle Database. Pleas try again.",StopSign!)
	END IF
END IF
//
//Parent.triggerevent("ue_cancel")
w_special_distribution_schedule.ib_web_done=false
close(parent)
end event

type cb_go from commandbutton within w_create_files_special
integer x = 219
integer y = 640
integer width = 247
integer height = 96
integer taborder = 130
boolean bringtotop = true
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "&Process"
end type

event clicked;//////////////////////////////////////////////////////////////////////////////////////////////////////////
//
//	Event: clicked for cb_go
//
//	Description:
//	Update batch_data after a successful distribution schedule
//
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//	
//	Revision History
//
//	Developed by 	Date 				Version						Tracking#
//									
// Murali K.			04/02/2008      PICS 2.5 Modifications	 Reqs: CA.9
// Murali K.			04/08/2008 		Set dist sched type and audit columns
// Murali K. 			06/23/2008		Update Actual Qty after Special Distribution  Schedule
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//


n_ds lds
Long rtn, li_count,ll_count2, li_re, i, m, j, li_bkseq,li_ordqty,li_cur, ll_rows, li_bound,&
	li_row_count, li_bkseq2,li_delcnt=0,ll_max_rows,ll_ds_rows
	
Integer li_tot_ordqty=0
Date ld_cabdt, ld_scheddate
DateTime ld_cabdt_dt, ld_scheddate_dt
String ls_bklist[], ls_date, ls_prdr, ls_bkmed, ls_bkseq, ls_libcd, ls_bkno,ls_bkno2, &
		ls_dsflag, ls_cfile, ls_sfile, ls_null, ls_usid, ls_bkno_old, ls_libcd_old,&
		ls_prdr_old, ls_chno, ls_oracle_ext_env, ls_bkmed2, ls_del, ls_nocntr,ls_filter,ls_msg ,ls_user
str_distrib_schedule lstr
Long li_bklist[]
Boolean lb_done=FALSE, lb_find
DateTime ldt_cur
Date ld_cabdt_old=Date('01/01/1900'), ld_today
Pointer oldpointer // Declares a pointer variable

// 10/22/2008
oldpointer = SetPointer(hourglass!)
cb_cancel.enabled = FALSE

// 04/02/2008
ls_user = gnv_app.of_getuserid()
ld_today = today()

st_transupdate.text= ""

// Create and load the datastore
lds = CREATE n_ds
lds.dataObject = "d_ds_pgm85_distsched_bklist_my"

SetNull(ls_null)

String ls_msgtext[ ] = {" 0 %","10 %", "20 %", "30 %", "40 %", "50 %", "60 %", &
									"70 %", "80 %", "90 %", "100 %"}
uo_progress.of_SetTextColor(Rgb(255, 255, 255))
lstr=istr
//ld_cabdt=lstr.ld_date
ld_scheddate= lstr.ld_date1
ld_scheddate_dt=DateTime(ld_scheddate,Time('00:00:00'))
SetNull(ls_dsflag)
li_bound= UpperBound(w_special_distribution_schedule.ii_bkseq[])
ls_bklist[]= lstr.arraymed[]
li_bound=UpperBound(ls_bklist[])

ldt_cur=DateTime(Today(),Now())
ls_usid=SqlServerTrans.userId
//if the step of create file is finished last time you run application,but not finish
//insert into distsched table in oracle. now you start from insert distsched directlly,
//you must re retrieve data from  again.
w_special_distribution_schedule.dw_dist.SetTransObject(SqlServerTrans)
IF w_special_distribution_schedule.cbx_distsched.checked=FALSE THEN

	parent.SetRedraw(TRUE)
	w_special_distribution_schedule.SetRedraw(TRUE)
	uo_progress.visible=TRUE
	uo_progress.of_SetMinimum(0)
	uo_progress.of_SetDisplayStyle(3)
	uo_progress.of_SetMessageText(ls_msgtext)
	uo_progress.of_SetPosition(0)
	// Max number of books multiply with 150 libraries, that give you the max number of rows
	ll_max_rows = 0.1 * (li_bound * 150)
	uo_progress.of_SetMaximum(ll_max_rows)
	st_transupdate.text= "                         Retrieving data ... "
	//select rows from ancntr,mchar,ttlinit, sched, prod,def where bkno in ls_bklist
	//and sched.ordqty>0 for transfer the rows to distsched of oracle
	dw_distsched_inx.dataObject = "d_ds_pgm85_distsched_bklist_my"
	dw_distsched_inx.SetTransObject(SqlServerTrans)
	oldpointer = SetPointer(hourglass!)
	dw_distsched_inx.Retrieve(ls_bklist[])
	SetPointer(oldpointer)
	ll_rows = dw_distsched_inx.RowCount()
	
	IF ll_rows > 0 THEN
		lds.Reset()
		dw_distsched_inx.RowsCopy(1,ll_rows, primary!,lds, 1, primary!)
	END IF

		
END IF// cbx_file.checked and cbx_distsched not checked

parent.SetRedraw(TRUE)
w_special_distribution_schedule.SetRedraw(TRUE)
//this is regular case create file and insert distsched of oracle at same run
IF w_special_distribution_schedule.cbx_file.checked =FALSE THEN
	 w_special_distribution_schedule.cbx_file.enabled=TRUE
	 
	 w_special_distribution_schedule.dw_dist.object.distfile[1]=ldt_cur
	 w_special_distribution_schedule.dw_dist.object.distfilerows[1]=i_distfilerows
	 w_special_distribution_schedule.dw_dist.object.distfilelib[1]=i_libs
	 w_special_distribution_schedule.dw_dist.object.distfilebooks[1]=i_books
	 w_special_distribution_schedule.dw_dist.object.distprdr[1]=i_prdr
	 w_special_distribution_schedule.dw_dist.object.distfileuser[1]=ls_usid
	 rtn=w_special_distribution_schedule.dw_dist.Update()
	 IF rtn=1 THEN	
		COMMIT USING SqlServerTrans;
		w_special_distribution_schedule.cbx_file.checked =TRUE
	 ELSE		
		ROLLBACK USING SqlServerTrans;
		// 10/22/2008
		cb_cancel.enabled = TRUE
		RETURN
	 END IF
END IF

IF w_special_distribution_schedule.cbx_distsched.checked =TRUE THEN
	Messagebox('Distribution Schedule','Distribution schedule has been created on the web.')
	// 10/22/2008
	cb_cancel.enabled = TRUE
	RETURN
END IF
w_special_distribution_schedule.cbx_distsched.enabled=TRUE

IF NOT SQLserverOracleTrans.DbHandle() >0 THEN
	SQLserverOracleTrans.of_connect() 
END IF

dw_sp1_distsched_orl.SetTransObject(SQLserverOracleTrans)

parent.SetRedraw(TRUE)
w_special_distribution_schedule.SetRedraw(TRUE)

ll_rows=dw_distsched_inx.RowCount()
li_count= 0.1 * ll_rows
		
IF ll_rows = 0 THEN
	Messagebox("ERROR","System error while retrieving."+&
		"~nPlease contact system administrator.")
	// 10/22/2008
	cb_cancel.enabled = TRUE
ELSE
	parent.SetRedraw(TRUE)
	uo_progress.visible=TRUE
	uo_progress.of_SetMinimum(0)
	uo_progress.of_SetMaximum(li_count)
	uo_progress.of_SetDisplayStyle(3)
	uo_progress.of_SetMessageText(ls_msgtext)
	uo_progress.of_SetPosition(0)
	st_transupdate.text="                     Loading data to the web. "
	//transfer each row from  to oracle
	ls_bkno_old=""
	ls_libcd_old=""
	ls_prdr_old=""
	
	//this for each bkseq to find how many libaries are associated with
//	dw_distsched_inx.SetSort("bkseq A,bkmed A,prdr A,libcd A")
//	dw_distsched_inx.Sort()
       
	FOR m = 1 TO ll_rows
		j = m
		IF Mod(j,10)=0 THEN
			uo_progress.of_Increment(1)
		END IF
		IF Mod(j,100)=0 THEN
			w_create_files_special.SetRedraw(TRUE)
		ELSE
			w_create_files_special.SetRedraw(FALSE)
		END IF
		
		// lets get all the values we'll need
			
		ls_prdr = Trim(String(dw_distsched_inx.object.prdr[m]))
		IF IsNull(ls_prdr) OR ls_prdr="" THEN
			ls_prdr="Not Assigned Yet"
		END IF
		ls_bkmed = Trim(String(dw_distsched_inx.object.bkmed[m]))
		ls_bkseq = Trim(String(dw_distsched_inx.object.bkseq[m]))
		li_bkseq= Long(ls_bkseq)
		ls_bkno=ls_bkmed+ls_bkseq

		li_ordqty = Long(String(dw_distsched_inx.object.ordqty[m]))
		ls_libcd = Trim(dw_distsched_inx.object.libcd[m])
		ld_cabdt_dt = dw_distsched_inx.object.cabdt[m]
		ld_cabdt=Date(ld_cabdt_dt)
		IF ls_bkno=ls_bkno_old AND ls_libcd=ls_libcd_old AND ls_prdr=ls_prdr_old &
				AND ld_cabdt=ld_cabdt_old THEN
				CONTINUE
		END IF
		
		ls_filter = "bkno = '"+ ls_bkno + "'"
		
		// If we are working with a different book move on. For example: DB40000 <> RC40000 OR RC40001 <> RC40002
//		IF ls_bkno<>ls_bkno_old  THEN
//			lds.SetFilter(ls_filter)
//			rtn = lds.Filter()
//			IF rtn = 1 THEN
//				ll_ds_rows = lds.RowCount()
//				
//				FOR i = 1 TO ll_ds_rows 
//					li_tot_ordqty += lds.object.ordqty[i]
//				NEXT
//				ls_msg = "Book Number = "+ ls_bkno + " Number of libraries = " + String(ll_ds_rows) + " QNTY in MCHAR = " + String(li_tot_ordqty)
//				OpenWithParm(w_pics_retrieve_msg_box,ls_msg)	
//				// Update MCHAR with total qunatity for each book
//				IF li_tot_ordqty > 0 THEN
//					UPDATE mchar 
//					set qnty = :li_tot_ordqty
//					where bkseq = :li_bkseq
//					AND bkmed = :ls_bkmed
//					USING SqlServerTrans;
//				
//					IF f_check_dberror(SqlServerTrans,"MCHAR")=FALSE THEN
//						ROLLBACK USING SqlServerTrans;
//						// 10/22/2008
//						cb_cancel.enabled = TRUE
//						RETURN
//					ELSE
//						COMMIT USING SqlServerTrans;					
//					END IF
//				END IF
//				
//				lds.SetFilter("");
//				lds.Filter()
//				
//				li_re += 1
//				li_tot_ordqty=0
//				Close(w_pics_retrieve_msg_box)	
//			END IF
//		END IF
//		dw_sp1_distsched_orl.TriggerEvent('ue_addrow')
		li_cur=dw_sp1_distsched_orl.InsertRow(0)
		dw_sp1_distsched_orl.SetItem(li_cur,'producer', ls_prdr)
		dw_sp1_distsched_orl.SetItem(li_cur,'cabdt', ld_cabdt_dt)
		dw_sp1_distsched_orl.SetItem(li_cur,'libcd', ls_libcd)
		dw_sp1_distsched_orl.SetItem(li_cur,'bkno', ls_bkno)
		dw_sp1_distsched_orl.SetItem(li_cur,'qty', li_ordqty)
		dw_sp1_distsched_orl.SetItem(li_cur,'scheddate', ld_scheddate_dt)
		dw_sp1_distsched_orl.SetItem(li_cur,'dsflag', ls_null)
		dw_sp1_distsched_orl.SetItem(li_cur,'bkmed', ls_bkmed)
		dw_sp1_distsched_orl.SetItem(li_cur,'bkseq', li_bkseq)
		
		// 04/08/2008 set dist sched type and audit columns
		dw_sp1_distsched_orl.SetItem(li_cur,'distsched_type_code', 'S') // special schedule for rush books
		dw_sp1_distsched_orl.SetItem(li_cur,'created_by', ls_user) 
		dw_sp1_distsched_orl.SetItem(li_cur,'modified_by', ls_user) 
		dw_sp1_distsched_orl.SetItem(li_cur,'created_date', ld_today) 
		dw_sp1_distsched_orl.SetItem(li_cur,'modified_date', ld_today) 

		// Update Actual Qty after Special Distribution 06/23/2008
		update batch@pic_link
		set actual_qty = nvl(selqty,defqty)
		where cabdt =:ld_cabdt_dt  and bkseq = :li_bkseq and bkmed = :ls_bkmed using sqlservertrans;


		IF ls_prdr<> ls_prdr_old THEN
			ls_prdr_old=ls_prdr
		END IF
		IF ls_bkno<>ls_bkno_old THEN
			ls_bkno_old=ls_bkno
		END IF
		IF ls_libcd<> ls_libcd_old THEN
			ls_libcd_old=ls_libcd
		END IF
		IF ld_cabdt<> ld_cabdt_old THEN
			ld_cabdt_old=ld_cabdt
		END IF
	NEXT
	dw_sp1_distsched_orl.Sort()
	w_create_files_special.SetRedraw(TRUE)
	uo_progress.visible=TRUE
	uo_progress.of_SetMinimum(0)
	uo_progress.of_SetMaximum(li_count)
	uo_progress.of_SetDisplayStyle(3)
	uo_progress.of_SetMessageText(ls_msgtext)
	uo_progress.of_SetPosition(0)
	
	i_count=0
	st_transupdate.text="Updating the web database. This process will take several minutes."
	ls_msg = "Updating the web database. This process will take several minutes."
	OpenWithParm(w_pics_retrieve_msg_box,ls_msg)	
	rtn = dw_sp1_distsched_orl.of_Update(TRUE,TRUE)
	st_transupdate.text=''
//	w_create_files_special.SetRedraw(TRUE)
	uo_progress.visible=FALSE
	//now is time to update dw_dsdtdsflag in w_special_distribution_schedule for mchar table
	//li_re=w_special_distribution_schedule.dw_dsdtdsflag.RowCount()
	ldt_cur=DateTime(Today(),Now())
	IF f_check_dberror(SQLserverOracleTrans,"distsched") THEN
		IF rtn=1 THEN
			rtn=w_special_distribution_schedule.dw_dsdtdsflag.Update()
			IF rtn=1 THEN					
				w_special_distribution_schedule.dw_dist.object.distsched[1]=ldt_cur
				w_special_distribution_schedule.dw_dist.object.distschedrows[1]=li_cur
				w_special_distribution_schedule.dw_dist.object.mcharupdrows[1]=li_re
				w_special_distribution_schedule.dw_dist.object.distscheduser[1]=ls_usid
				rtn=w_special_distribution_schedule.dw_dist.Update()
				IF rtn=1 THEN
					// Now lets do some cleaning up and remove some of the old records from distsched table.
					Close(w_pics_retrieve_msg_box)	
					w_special_distribution_schedule.st_dist.text = String(ldt_cur,'mm/dd/yyyy hh:mm:ss')
					w_special_distribution_schedule.cbx_distsched.checked=TRUE
					
//					// 04/02/2008 Update batch_data table after creating distribution schedule - no update
//					UPDATE BATCH_DATA@pic_link
//						SET   BATCH_STATUS_CODE = 'D',
//	  						    MODIFIED_DATE =:ld_today ,
//						         MODIFIED_BY =:ls_user
//					WHERE  CABDT =:ld_cabdt 	using sqlservertrans ;
					///////////
				
					COMMIT USING SQLserverOracleTrans;
					COMMIT USING SqlServerTrans;
					
					// 07/30/2009 #2137 Update MCHAR.QNTY for each book distributed for the schedule date
					of_updatemcharqnty(ld_scheddate_dt)
					
					Messagebox("Distribution Schedule",String(ll_rows)+" rows inserted into the web Database. ")
					lb_done=TRUE
				ELSE
					Close(w_pics_retrieve_msg_box)	
					ROLLBACK USING SQLserverOracleTrans;
					ROLLBACK USING SqlServerTrans;
					IF IsValid(w_create_files_special) THEN
						closewithreturn(w_create_files_special,'N')
					ELSE
						// 10/22/2008
						cb_cancel.enabled = TRUE
						RETURN
					END IF
				END IF
			ELSE
				Close(w_pics_retrieve_msg_box)	
				ROLLBACK USING SQLserverOracleTrans;
				ROLLBACK USING SqlServerTrans;
				IF IsValid(w_create_files_special) THEN
					closewithreturn(w_create_files_special,'N')
				ELSE
					// 10/22/2008
					cb_cancel.enabled = TRUE
					RETURN
				END IF
			END IF
		ELSE
			Close(w_pics_retrieve_msg_box)	
			ROLLBACK USING SQLserverOracleTrans;
			ROLLBACK USING SqlServerTrans;
			IF IsValid(w_create_files_special) THEN
				closewithreturn(w_create_files_special,'N')
			ELSE
				// 10/22/2008
				cb_cancel.enabled = TRUE
				RETURN
			END IF
		END IF
	ELSE
		Close(w_pics_retrieve_msg_box)	
		Messagebox("Update Error","Error while trying to update the Oracle Database."+"~nPlease contact system administration.")
		ROLLBACK USING SQLserverOracleTrans;
		IF IsValid(w_create_files_special) THEN
			closewithreturn(w_create_files_special,'N')
		ELSE
			// 10/22/2008
			cb_cancel.enabled = TRUE
			RETURN
		END IF
	END IF
IF IsValid(w_pics_retrieve_msg_box) THEN
     Close(w_pics_retrieve_msg_box)	
END IF	
uo_progress.visible=FALSE
END IF //end if  SqlServerOracleTrans.sqlcode <> 0
IF lb_done=TRUE THEN
	w_special_distribution_schedule.ib_web_done=TRUE
	IF IsValid(w_create_files_special) THEN
		closewithreturn(w_create_files_special,'Y')
	END IF
ELSE
	w_special_distribution_schedule.ib_web_done=FALSE
	IF IsValid(w_create_files_special) THEN
		closewithreturn(w_create_files_special,'N')
	END IF
END IF
IF IsValid(w_create_files_special) THEN
	Close(w_create_files_special)
END IF

end event


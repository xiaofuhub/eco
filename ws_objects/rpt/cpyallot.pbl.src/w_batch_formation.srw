$PBExportHeader$w_batch_formation.srw
forward
global type w_batch_formation from w_sheet
end type
type dw_batch_data from u_pics_dw within w_batch_formation
end type
type cbx_apnd from u_cbx within w_batch_formation
end type
type dw_ri_1_or_2_conno from u_dw within w_batch_formation
end type
type dw_ri_2_prevbk from u_dw within w_batch_formation
end type
type st_email from statictext within w_batch_formation
end type
type st_marc from statictext within w_batch_formation
end type
type st_web from statictext within w_batch_formation
end type
type st_sched from statictext within w_batch_formation
end type
type st_def from statictext within w_batch_formation
end type
type dw_sp9_deflib from u_dw within w_batch_formation
end type
type cb_mail from u_cb within w_batch_formation
end type
type cbx_email from u_cbx within w_batch_formation
end type
type cbx_batbks from u_cbx within w_batch_formation
end type
type cbx_sched from u_cbx within w_batch_formation
end type
type cbx_defupd from u_cbx within w_batch_formation
end type
type st_cnt from statictext within w_batch_formation
end type
type st_updated from statictext within w_batch_formation
end type
type sle_rows_cnt from singlelineedit within w_batch_formation
end type
type sle_rows_upd from singlelineedit within w_batch_formation
end type
type dw_batch_process from u_dw within w_batch_formation
end type
type st_transupdate from statictext within w_batch_formation
end type
type uo_progress from u_progressbar within w_batch_formation
end type
type dw_get_bklist from u_pics_dw within w_batch_formation
end type
type dw_update_for_books_orl from u_pics_dw within w_batch_formation
end type
type dw_update_for_batch_orl from u_pics_dw within w_batch_formation
end type
type cb_exit from u_cb within w_batch_formation
end type
type cb_update from u_cb within w_batch_formation
end type
type dw_batch_formation from u_dw within w_batch_formation
end type
type cb_find from u_cb within w_batch_formation
end type
type cb_clear from u_cb within w_batch_formation
end type
type cb_print from u_cb within w_batch_formation
end type
type dw_get_data_for_books from u_dw within w_batch_formation
end type
type dw_get_data_for_batch from u_dw within w_batch_formation
end type
type dw_distribution_schedule from u_dw within w_batch_formation
end type
type gb_1 from groupbox within w_batch_formation
end type
type cbx_file from u_cbx within w_batch_formation
end type
end forward

global type w_batch_formation from w_sheet
integer x = 352
integer y = 572
integer width = 3538
integer height = 1072
string title = "Batch Formation"
dw_batch_data dw_batch_data
cbx_apnd cbx_apnd
dw_ri_1_or_2_conno dw_ri_1_or_2_conno
dw_ri_2_prevbk dw_ri_2_prevbk
st_email st_email
st_marc st_marc
st_web st_web
st_sched st_sched
st_def st_def
dw_sp9_deflib dw_sp9_deflib
cb_mail cb_mail
cbx_email cbx_email
cbx_batbks cbx_batbks
cbx_sched cbx_sched
cbx_defupd cbx_defupd
st_cnt st_cnt
st_updated st_updated
sle_rows_cnt sle_rows_cnt
sle_rows_upd sle_rows_upd
dw_batch_process dw_batch_process
st_transupdate st_transupdate
uo_progress uo_progress
dw_get_bklist dw_get_bklist
dw_update_for_books_orl dw_update_for_books_orl
dw_update_for_batch_orl dw_update_for_batch_orl
cb_exit cb_exit
cb_update cb_update
dw_batch_formation dw_batch_formation
cb_find cb_find
cb_clear cb_clear
cb_print cb_print
dw_get_data_for_books dw_get_data_for_books
dw_get_data_for_batch dw_get_data_for_batch
dw_distribution_schedule dw_distribution_schedule
gb_1 gb_1
cbx_file cbx_file
end type
global w_batch_formation w_batch_formation

type variables
date ld_date
boolean ib_update =true
long i_count=0, i_rows
datetime  ld_calculated_rtndt

end variables

forward prototypes
public function integer wf_send_mail ()
public function integer wf_get_data_libdef ()
public function integer wf_update_def ()
public function integer wf_librarylock (date ad_cabdt)
public subroutine wf_error ()
end prototypes

public function integer wf_send_mail ();return 1


end function

public function integer wf_get_data_libdef ();//If we have lost our connection to oracle, reconnect.
IF NOT SqlServerOracleTrans.DbHandle() >0 THEN
	SqlServerOracleTrans.of_connect() 
	IF SqlServerOracleTrans.sqlCode <> 0 THEN
		IF SqlServerOracleTrans.sqlDbCode = -951 THEN            //check for invalid userid
			Messagebox("Login Error","Invalid User ID/Password using net8web.",stopSign!)
			RETURN -1	
		ELSEIF SqlServerOracleTrans.sqlDbCode = -952 THEN       //check for invalid password
			Messagebox("Login Error","Invalid User ID/Password using net8web.",stopSign!)
			RETURN -1
		ELSE                                             //check for other error messages
			Messagebox("Database Connection Error","Unable to Connect using net8web." +& 
					String(SqlServerOracleTrans.sqlDbCode) + " " +&
					SqlServerOracleTrans.sqlErrText, &
					stopSign!)
					RETURN -1
		END IF
	END IF
END IF
	
Int rtn	
rtn = Messagebox("Default Quantities","This process will import the default quantities " +&
			"~nfrom the web database. These new default quantities will be used to create "+&
			"~nthe current Copy Allotment batch."+&
							" Continue?",question!,yesNo!,1)
IF rtn = 1	THEN
	Long ll_Rows
	dw_Sp9_deflib.SetTransObject(SqlServerOracleTrans)
	// retrieve the data from libdef table at rs21n
	ll_Rows = dw_Sp9_deflib.Retrieve()
	IF ll_Rows = 0 THEN		
		Messagebox("","No rows retrieved from the web database.")
		RETURN - 1
	END IF		
ELSE
	RETURN - 1
END IF
RETURN 1
end function

public function integer wf_update_def ();//Check Database connectivity
IF NOT SqlServerOracleTrans.DbHandle() >0 THEN
	SqlServerOracleTrans.of_connect() 
	IF SqlServerOracleTrans.sqlCode <> 0 THEN
		IF SqlServerOracleTrans.sqlDbCode = -951 THEN            //check for invalid userid
			Messagebox("Login Error","Invalid User ID/Password using net8web.",stopSign!)
			RETURN -1	
		ELSEIF SqlServerOracleTrans.sqlDbCode = -952 THEN       //check for invalid password
			Messagebox("Login Error","Invalid User ID/Password using net8web.",stopSign!)
			RETURN -1
		ELSE                                             //check for other error messages
			Messagebox("Database Connection Error","Unable to Connect using net8web." +& 
					String(SqlServerOracleTrans.sqlDbCode) + " " +&
					SqlServerOracleTrans.sqlErrText, &
					stopSign!)
					RETURN -1
		END IF
	END IF
END IF
	
String ls_Msgtext[ ] = {" 0 %","10 %", "20 %", "30 %", "40 %", "50 %", "60 %", &
											"70 %", "80 %", "90 %", "100 %"}

uo_Progress.of_SetTextColor(Rgb(255, 255, 255))
Long ll_Rows,lcnt,nullval,rows_Updated=0
String llibcd,lmed,lcasub
Integer ldef_N_def,ldef_P_def,ldef_R_def
Integer lsel_N_def,lsel_P_def,lsel_R_def
DateTime ldefdt,ltoday
Time lt_Time=Time('00:00:00')

ltoday = DateTime(Today(), lt_Time)

SetNull(nullval)
ll_Rows = dw_Sp9_deflib.RowCount()

IF  ll_Rows <> 0 THEN
	sle_Rows_cnt.text = String(ll_Rows)
	sle_Rows_cnt.visible=FALSE
	st_Cnt.visible=FALSE
	String newsort
	newsort = "libcd A,med,casub"
	dw_Sp9_deflib.SetSort(newsort)
	dw_Sp9_deflib.Sort()

	uo_Progress.visible=TRUE
	uo_Progress.of_SetMinimum(0)
	uo_Progress.of_SetMaximum(ll_Rows)
	uo_Progress.of_SetDisplayStyle(3)
	uo_Progress.of_SetMessageText(ls_Msgtext)
	uo_Progress.of_SetPosition(0)
//		openwithparm(w_pics_retrieve_msg_box,"Synchronizing defqty(Normal, Priority and Rush). Please Wait...")
	
	FOR lcnt = 1 TO ll_Rows
			llibcd 		= Trim(dw_Sp9_deflib.object.libcd[lcnt])
			lmed 			= Trim(dw_Sp9_deflib.object.med[lcnt])
			lcasub 		= Trim(dw_Sp9_deflib.object.casub[lcnt])
			lsel_N_def  = dw_Sp9_deflib.object.sel_N_defqty[lcnt]
			IF NOT(IsNull(lsel_N_def)) THEN
				UPDATE def
				SET defqty = :lsel_N_def,defdt = :ltoday
				WHERE libcd = :llibcd
				AND   med   = :lmed
				AND   casub = :lcasub
				AND   priority = 'N'
				USING sqlservertrans;
				dw_Sp9_deflib.object.curr_N_defqty[lcnt] 	=	dw_Sp9_deflib.object.sel_N_defqty[lcnt]
				dw_Sp9_deflib.object.sel_N_defqty[lcnt] 	=	nullval
				rows_Updated++
			END IF
					
			lsel_P_def  = dw_Sp9_deflib.object.sel_P_defqty[lcnt]
			IF NOT(IsNull(lsel_P_def)) THEN
				UPDATE def
				SET defqty = :lsel_P_def,defdt = :ltoday
				WHERE libcd = :llibcd
				AND   med   = :lmed
				AND   casub = :lcasub
				AND   priority = 'P'
				USING sqlservertrans;
				dw_Sp9_deflib.object.curr_P_defqty[lcnt] 	=	dw_Sp9_deflib.object.sel_P_defqty[lcnt]
				dw_Sp9_deflib.object.sel_P_defqty[lcnt] 	=	nullval
				rows_Updated++
			END IF
			
			lsel_R_def  = dw_Sp9_deflib.object.sel_R_defqty[lcnt]
			IF NOT(IsNull(lsel_R_def)) THEN
				UPDATE def
				SET defqty = :lsel_R_def,defdt = :ltoday
				WHERE libcd = :llibcd
				AND   med   = :lmed
				AND   casub = :lcasub
				AND   priority = 'R'
				USING sqlservertrans;
				dw_Sp9_deflib.object.curr_R_defqty[lcnt] 	=	dw_Sp9_deflib.object.sel_R_defqty[lcnt]
				dw_Sp9_deflib.object.sel_R_defqty[lcnt] 	=	nullval
				rows_Updated++
			END IF
			
			IF NOT(IsNull(lsel_N_def)) OR &
				NOT(IsNull(lsel_P_def)) OR &
				NOT(IsNull(lsel_R_def)) THEN
				dw_Sp9_deflib.object.sent_Flag[lcnt] 		=  'Y'
			END IF					
			
			IF lcnt <= uo_Progress.of_GetMaximum() THEN
				uo_Progress.of_Increment(1)
				IF Mod(lcnt,100)=0 THEN
					sle_Rows_upd.text = String(rows_Updated)
					sle_Rows_upd.visible=TRUE
					st_Updated.visible=TRUE
					st_Updated.text='Total rows updated: '
				END IF
			END IF
	NEXT			
	i_Rows=rows_Updated	
//		close(w_pics_retrieve_msg_box)
	uo_Progress.visible=FALSE
	Long rtn
	dw_Sp9_deflib.SetTransObject(SqlServerOracleTrans)			
	rtn = dw_Sp9_deflib.of_Update(TRUE,TRUE)
	IF f_check_dberror(SqlServerOracleTrans,"LIBDEF at RS21n") THEN
		IF rtn=1 THEN
//				Commit Using sqlservertrans;
//				Commit Using sqlserveroracletrans;
			Messagebox("Default Quantities", "Default Quantities successfully imported")
		ELSE
			Messagebox("Error","Error while updating LIBDEF table. "+&
				"~nPlease contact your system administrator.")	
				
			ROLLBACK USING SqlServerOracleTrans;
			ROLLBACK USING sqlservertrans;
			uo_Progress.visible=FALSE
			IF rows_Updated > 0 THEN
			END IF
			RETURN -1
		END IF
	END IF

	uo_Progress.visible=FALSE
ELSE
	Messagebox("Default Quantities","No rows retrieved from the web database.")
END IF		

sle_Rows_cnt.visible=FALSE
st_Cnt.visible=FALSE
sle_Rows_upd.visible=FALSE
st_Updated.visible=FALSE
RETURN 1
end function

public function integer wf_librarylock (date ad_cabdt);//////////////////////////////////////////////////////////////////////////////////////////////////////////
//
//	Functiont: wf_librarylock
// Args: Date Batch date
//	Description:
//	1. Lock libraries if already locked .
//
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//	
//	Revision History
//
//	Developed by 	Date 				Version						Tracking#
//									
// Murali K. 			07/10/2008 		Need to set the BATCH.Flag to 'C' or null based on the library lock
//											Update rtndt if user changes it to all records in batch ( pic)
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//

// 
Long ll_rc, ll_count
Integer li_loop
string ls_libcd, ls_flag

ll_rc = dw_get_data_for_batch.Rowcount()

FOR li_loop  = 1 TO ll_rc
	
		ls_libcd = 	dw_get_data_for_batch.object.libcd[li_loop]
	
		select count(*) 
		into :ll_count
		from batch@pic_link
		where cabdt = :ad_cabdt and libcd = :ls_libcd   and (flag is not null or pcs_lock_yn is not null) using sqlservertrans ;
		
		IF ll_count > 0 THEN
			UPDATE BATCH@pic_link
			SET FLAG = 'C'
			WHERE cabdt = :ad_cabdt and libcd = :ls_libcd  using sqlservertrans ;
			COMMIT USING sqlservertrans;
		END IF
NEXT
UPDATE BATCH@pic_link
SET  rtndt = :ld_calculated_rtndt
WHERE cabdt = :ad_cabdt  using sqlservertrans ;
COMMIT USING sqlservertrans;


RETURN 1
end function

public subroutine wf_error ();// created this function from Error label to prevent unsupported appeon feature 3/24/2010
IF  sqlserveroracletrans.DbHandle() >0 THEN
	IF sqlserveroracletrans.of_disconnect() < 0 THEN
		Messagebox("Error","Disconnected from the Oracle Database. Please try again",stopSign!)
		IF IsValid(w_pics_retrieve_msg_box) THEN
			Close(w_pics_retrieve_msg_box)
		END IF
		RETURN
	END IF
END IF

end subroutine

event mousemove;call super::mousemove;w_pics_main.setmicrohelp("Ready")
end event

event pfc_postopen;call super::pfc_postopen;Long ll_Count

m_Pics_main.m_Edit.m_Addrow.enabled = FALSE
m_Pics_main.m_Edit.m_Deleterow.enabled = FALSE
m_Pics_main.m_Edit.m_Cut.enabled = FALSE
dw_Batch_formation.InsertRow(0)
cb_Update.enabled = FALSE
cb_Clear.enabled = FALSE
cb_Print.enabled =FALSE
dw_Batch_formation.SetTransObject(sqlservertrans)

//OpenWithParm(w_pics_retrieve_msg_box,"Accessing qualified rows from mchar table, Please Wait...")
//SELECT COUNT(*) INTO :ll_count FROM mchar
//WHERE cascd = 'Q'
//USING sqlservertrans;
//close(w_pics_retrieve_msg_box)
//IF ll_count > 0 THEN
//ELSE
//	Messagebox("DATA","No data found in MCHAR table to update.",STOPSIGN!)
//	ib_disableclosequery = TRUE	
//	CLOSE(w_batch_formation)
//	return
//END IF
//cbx_Defupd.enabled=FALSE
//cbx_Defupd.checked=FALSE
cbx_Sched.enabled=FALSE
cbx_Sched.checked=FALSE
cbx_Batbks.enabled=FALSE
cbx_Batbks.checked=FALSE
cbx_File.enabled=FALSE
cbx_File.checked=FALSE
cbx_Email.enabled=FALSE
cbx_Email.checked=FALSE

sle_Rows_cnt.visible=FALSE
st_Cnt.visible=FALSE
sle_Rows_upd.visible=TRUE
st_Updated.visible=TRUE



	

end event

event pfc_preopen();call super::pfc_preopen;this.of_SetBase(true)
this.inv_base.of_Center()
this.of_SetPreference(TRUE)
this.inv_preference.of_SetToolBars(TRUE)
this.inv_preference.of_SetWindow(TRUE)
this.of_SetResize(TRUE)
this.inv_resize.of_SetOrigSize(this.workspacewidth(),this.workspaceheight())
inv_resize.of_Register(dw_batch_formation, "scale")
inv_resize.of_Register(cb_update, "scale")
inv_resize.of_Register(cb_exit, "scale")
inv_resize.of_Register(cb_print, "scale")
inv_resize.of_Register(cb_clear, "scale")
inv_resize.of_Register(cb_find, "scale")
inv_resize.of_Register(dw_get_data_for_batch, "scale")
inv_resize.of_Register(dw_get_data_for_books, "scale")
inv_resize.of_Register(dw_update_for_batch_orl, "scale")
inv_resize.of_Register(dw_update_for_books_orl, "scale")
inv_resize.of_Register(dw_distribution_schedule, "scale")
inv_resize.of_Register(dw_get_bklist, "scale")

end event

event resize;//long ll_height

//This.X = 300
this.Y = 400
//This.width = 2300
//This.height = 913
////ll_height = w_pics_main.mdi_1.Height
////This.Resize(w_pics_main.WorkSpaceWidth(), ll_height)
end event

on w_batch_formation.create
int iCurrent
call super::create
this.dw_batch_data=create dw_batch_data
this.cbx_apnd=create cbx_apnd
this.dw_ri_1_or_2_conno=create dw_ri_1_or_2_conno
this.dw_ri_2_prevbk=create dw_ri_2_prevbk
this.st_email=create st_email
this.st_marc=create st_marc
this.st_web=create st_web
this.st_sched=create st_sched
this.st_def=create st_def
this.dw_sp9_deflib=create dw_sp9_deflib
this.cb_mail=create cb_mail
this.cbx_email=create cbx_email
this.cbx_batbks=create cbx_batbks
this.cbx_sched=create cbx_sched
this.cbx_defupd=create cbx_defupd
this.st_cnt=create st_cnt
this.st_updated=create st_updated
this.sle_rows_cnt=create sle_rows_cnt
this.sle_rows_upd=create sle_rows_upd
this.dw_batch_process=create dw_batch_process
this.st_transupdate=create st_transupdate
this.uo_progress=create uo_progress
this.dw_get_bklist=create dw_get_bklist
this.dw_update_for_books_orl=create dw_update_for_books_orl
this.dw_update_for_batch_orl=create dw_update_for_batch_orl
this.cb_exit=create cb_exit
this.cb_update=create cb_update
this.dw_batch_formation=create dw_batch_formation
this.cb_find=create cb_find
this.cb_clear=create cb_clear
this.cb_print=create cb_print
this.dw_get_data_for_books=create dw_get_data_for_books
this.dw_get_data_for_batch=create dw_get_data_for_batch
this.dw_distribution_schedule=create dw_distribution_schedule
this.gb_1=create gb_1
this.cbx_file=create cbx_file
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_batch_data
this.Control[iCurrent+2]=this.cbx_apnd
this.Control[iCurrent+3]=this.dw_ri_1_or_2_conno
this.Control[iCurrent+4]=this.dw_ri_2_prevbk
this.Control[iCurrent+5]=this.st_email
this.Control[iCurrent+6]=this.st_marc
this.Control[iCurrent+7]=this.st_web
this.Control[iCurrent+8]=this.st_sched
this.Control[iCurrent+9]=this.st_def
this.Control[iCurrent+10]=this.dw_sp9_deflib
this.Control[iCurrent+11]=this.cb_mail
this.Control[iCurrent+12]=this.cbx_email
this.Control[iCurrent+13]=this.cbx_batbks
this.Control[iCurrent+14]=this.cbx_sched
this.Control[iCurrent+15]=this.cbx_defupd
this.Control[iCurrent+16]=this.st_cnt
this.Control[iCurrent+17]=this.st_updated
this.Control[iCurrent+18]=this.sle_rows_cnt
this.Control[iCurrent+19]=this.sle_rows_upd
this.Control[iCurrent+20]=this.dw_batch_process
this.Control[iCurrent+21]=this.st_transupdate
this.Control[iCurrent+22]=this.uo_progress
this.Control[iCurrent+23]=this.dw_get_bklist
this.Control[iCurrent+24]=this.dw_update_for_books_orl
this.Control[iCurrent+25]=this.dw_update_for_batch_orl
this.Control[iCurrent+26]=this.cb_exit
this.Control[iCurrent+27]=this.cb_update
this.Control[iCurrent+28]=this.dw_batch_formation
this.Control[iCurrent+29]=this.cb_find
this.Control[iCurrent+30]=this.cb_clear
this.Control[iCurrent+31]=this.cb_print
this.Control[iCurrent+32]=this.dw_get_data_for_books
this.Control[iCurrent+33]=this.dw_get_data_for_batch
this.Control[iCurrent+34]=this.dw_distribution_schedule
this.Control[iCurrent+35]=this.gb_1
this.Control[iCurrent+36]=this.cbx_file
end on

on w_batch_formation.destroy
call super::destroy
destroy(this.dw_batch_data)
destroy(this.cbx_apnd)
destroy(this.dw_ri_1_or_2_conno)
destroy(this.dw_ri_2_prevbk)
destroy(this.st_email)
destroy(this.st_marc)
destroy(this.st_web)
destroy(this.st_sched)
destroy(this.st_def)
destroy(this.dw_sp9_deflib)
destroy(this.cb_mail)
destroy(this.cbx_email)
destroy(this.cbx_batbks)
destroy(this.cbx_sched)
destroy(this.cbx_defupd)
destroy(this.st_cnt)
destroy(this.st_updated)
destroy(this.sle_rows_cnt)
destroy(this.sle_rows_upd)
destroy(this.dw_batch_process)
destroy(this.st_transupdate)
destroy(this.uo_progress)
destroy(this.dw_get_bklist)
destroy(this.dw_update_for_books_orl)
destroy(this.dw_update_for_batch_orl)
destroy(this.cb_exit)
destroy(this.cb_update)
destroy(this.dw_batch_formation)
destroy(this.cb_find)
destroy(this.cb_clear)
destroy(this.cb_print)
destroy(this.dw_get_data_for_books)
destroy(this.dw_get_data_for_batch)
destroy(this.dw_distribution_schedule)
destroy(this.gb_1)
destroy(this.cbx_file)
end on

event open;call super::open;this.of_SetBase(true)
this.inv_base.of_Center()
end event

type dw_batch_data from u_pics_dw within w_batch_formation
boolean visible = false
integer x = 242
integer y = 692
integer width = 64
integer height = 104
integer taborder = 60
string dataobject = "d_batch_data"
end type

event constructor;call super::constructor;this.SettransObject(SqlServertrans)
end event

type cbx_apnd from u_cbx within w_batch_formation
boolean visible = false
integer x = 2039
integer y = 388
integer width = 882
integer height = 68
string text = "Append Batch(PICS)"
end type

event clicked;call super::clicked;//////////////////////////////////////////////////////////////////////////////////////////////////////////
//
//	Event:clicked for cbk_apnd
//
//	Description:
//	Validate and initiate Append allotment batch
//
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//	
//	Revision History
//
//	Developed by 	Date 				Version						Tracking#
//									
// Murali K.			03/26/2008      PICS 2.5 Modifications	 Reqs: CA.9
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//
date ld_dt
ld_dt = date(dw_batch_formation.object.rtndt[1])

IF This.Checked = TRUE THEN
	IF today() > ld_dt THEN
		Messagebox('Error', 'Can append batch only within the cut-off date of current batch')
		This.Checked=FALSE
		RETURN -1
	END IF
	cbx_sched.checked=FALSE
	cbx_batbks.checked=FALSE
	cbx_sched.enabled=FALSE
	cbx_batbks.enabled=FALSE
	
	cbx_File.checked=FALSE
	cbx_Email.checked = FALSE
	cbx_File.enabled=FALSE
	cbx_Email.enabled = FALSE

ELSE
	cbx_sched.enabled=TRUE
	cbx_batbks.enabled=TRUE
END IF
end event

type dw_ri_1_or_2_conno from u_dw within w_batch_formation
event pfc_hinttext pbm_mousemove
boolean visible = false
integer x = 256
integer y = 848
integer width = 50
integer height = 68
integer taborder = 60
string dataobject = "d_ri_1_or_2_conno"
boolean vscrollbar = false
boolean livescroll = false
end type

event pfc_hinttext;//string ls_object, ls_column, ls_column_tag
//long ll_pos
//
////This script set's microhelp at the bottom of the screen for the dw_batch_formation
//ls_object = THIS.getobjectatpointer()
//ll_pos = pos(ls_object, "~t")
//IF NOT pos(ls_object, "_t~t") > 0 THEN
//	IF ll_pos > 0 THEN
//		ll_pos = ll_pos -1
//		ls_column = mid(ls_object,1,ll_pos)
//		ls_column_tag = THIS.Describe(ls_column + ".tag")
//		w_pics_main.setmicrohelp(ls_column_tag)
//	ELSE
//		w_pics_main.setmicrohelp("Ready")
//	END IF
//END IF
end event

event constructor;call super::constructor;THIS.settransobject(sqlservertrans)
ib_rmbmenu = FALSE


end event

event getfocus;call super::getfocus;m_pics_main.m_Edit.m_Addrow.Enabled = FALSE
m_pics_main.m_Edit.m_Deleterow.Enabled = FALSE
m_pics_main.m_Edit.m_Cut.Enabled = FALSE
end event

event itemchanged;call super::itemchanged;//date ld_cabdt, ld_rtndt
//string ls_date
//if dwo.name='batch_cabdt' then
//	ls_date=data
//	if isnull(ls_date) then
//		messagebox('Error','You must enter a valid Copy Allotment batch date')
//		return 1
//	end if
//	if IsDate(ls_date) then
//		ld_cabdt=Date(ls_date)
//		ld_rtndt=RelativeDate(ld_cabdt, 25)
//		dw_batch_formation.SetItem(1,'rtndt', ld_rtndt)
//	else
//		messagebox('Error','You must enter a valid Copy Allotment batch date')
//		return 1
//	end if
//end if
end event

event retrieveend;call super::retrieveend;//close(w_pics_retrieve_msg_box)
//sle_rows_cnt.Text = string(rowcount)
end event

event retrievestart;call super::retrievestart;//Open(w_pics_retrieve_msg_box)
end event

event pfc_addrow;long ll_rc
//
//ll_rc = Super::Event pfc_addrow()
//dw_batch_formation.SetItem(ll_rc, "batch_cabdt",ld_date)
return ll_rc
end event

event pfc_retrieve;long ll_row_count , i
////ll_row_count =THIS.Retrieve(ld_date)
return ll_row_count
end event

type dw_ri_2_prevbk from u_dw within w_batch_formation
event pfc_hinttext pbm_mousemove
boolean visible = false
integer x = 169
integer y = 848
integer width = 50
integer height = 68
integer taborder = 50
string dataobject = "d_ri_2_prevbk"
boolean vscrollbar = false
boolean livescroll = false
end type

event pfc_hinttext;//string ls_object, ls_column, ls_column_tag
//long ll_pos
//
////This script set's microhelp at the bottom of the screen for the dw_batch_formation
//ls_object = THIS.getobjectatpointer()
//ll_pos = pos(ls_object, "~t")
//IF NOT pos(ls_object, "_t~t") > 0 THEN
//	IF ll_pos > 0 THEN
//		ll_pos = ll_pos -1
//		ls_column = mid(ls_object,1,ll_pos)
//		ls_column_tag = THIS.Describe(ls_column + ".tag")
//		w_pics_main.setmicrohelp(ls_column_tag)
//	ELSE
//		w_pics_main.setmicrohelp("Ready")
//	END IF
//END IF
end event

event constructor;call super::constructor;THIS.settransobject(sqlservertrans)
ib_rmbmenu = FALSE


end event

event getfocus;call super::getfocus;m_pics_main.m_Edit.m_Addrow.Enabled = FALSE
m_pics_main.m_Edit.m_Deleterow.Enabled = FALSE
m_pics_main.m_Edit.m_Cut.Enabled = FALSE
end event

event itemchanged;call super::itemchanged;//date ld_cabdt, ld_rtndt
//string ls_date
//if dwo.name='batch_cabdt' then
//	ls_date=data
//	if isnull(ls_date) then
//		messagebox('Error','You must enter a valid Copy Allotment batch date')
//		return 1
//	end if
//	if IsDate(ls_date) then
//		ld_cabdt=Date(ls_date)
//		ld_rtndt=RelativeDate(ld_cabdt, 25)
//		dw_batch_formation.SetItem(1,'rtndt', ld_rtndt)
//	else
//		messagebox('Error','You must enter a valid Copy Allotment batch date')
//		return 1
//	end if
//end if
end event

event retrieveend;call super::retrieveend;//close(w_pics_retrieve_msg_box)
//sle_rows_cnt.Text = string(rowcount)
end event

event retrievestart;call super::retrievestart;//Open(w_pics_retrieve_msg_box)
end event

event pfc_addrow;long ll_rc
//
//ll_rc = Super::Event pfc_addrow()
//dw_batch_formation.SetItem(ll_rc, "batch_cabdt",ld_date)
return ll_rc
end event

event pfc_retrieve;long ll_row_count , i
////ll_row_count =THIS.Retrieve(ld_date)
return ll_row_count
end event

type st_email from statictext within w_batch_formation
integer x = 2898
integer y = 708
integer width = 530
integer height = 64
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

type st_marc from statictext within w_batch_formation
integer x = 2898
integer y = 628
integer width = 530
integer height = 64
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

type st_web from statictext within w_batch_formation
integer x = 2898
integer y = 548
integer width = 530
integer height = 64
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

type st_sched from statictext within w_batch_formation
integer x = 2898
integer y = 464
integer width = 530
integer height = 64
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

type st_def from statictext within w_batch_formation
boolean visible = false
integer x = 2898
integer y = 388
integer width = 530
integer height = 64
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

type dw_sp9_deflib from u_dw within w_batch_formation
event pfc_hinttext pbm_mousemove
boolean visible = false
integer x = 96
integer y = 848
integer width = 50
integer height = 68
integer taborder = 40
string dataobject = "d_sp9_deflib_my"
boolean vscrollbar = false
boolean livescroll = false
end type

event pfc_hinttext;call super::pfc_hinttext;string ls_object, ls_column, ls_column_tag
long ll_pos

//This script set's microhelp at the bottom of the screen for the dw_batch_formation
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

event constructor;call super::constructor;THIS.settransobject(sqlservertrans)
ib_rmbmenu = FALSE


end event

event getfocus;call super::getfocus;m_pics_main.m_Edit.m_Addrow.Enabled = FALSE
m_pics_main.m_Edit.m_Deleterow.Enabled = FALSE
m_pics_main.m_Edit.m_Cut.Enabled = FALSE
end event

event itemchanged;call super::itemchanged;date ld_cabdt, ld_rtndt
string ls_date
if dwo.name='batch_cabdt' then
	ls_date=data
	if isnull(ls_date) then
		messagebox('Error','You must enter a valid Copy Allotment batch date')
		return 1
	end if
	if IsDate(ls_date) then
		ld_cabdt=Date(ls_date)
		ld_rtndt=RelativeDate(ld_cabdt, 25)
		dw_batch_formation.SetItem(1,'rtndt', ld_rtndt)
	else
		messagebox('Error','You must enter a valid Copy Allotment batch date')
		return 1
	end if
end if
end event

event retrieveend;call super::retrieveend;//close(w_pics_retrieve_msg_box)
sle_rows_cnt.Text = string(rowcount)
end event

event retrievestart;call super::retrievestart;//Open(w_pics_retrieve_msg_box)
end event

event pfc_addrow;long ll_rc

ll_rc = Super::Event pfc_addrow()
dw_batch_formation.SetItem(ll_rc, "batch_cabdt",ld_date)
return ll_rc
end event

event type long pfc_retrieve();long ll_row_count , i
//ll_row_count =THIS.Retrieve(ld_date)
return ll_row_count
end event

type cb_mail from u_cb within w_batch_formation
event pfc_hinttext pbm_mousemove
string tag = "Exits the current screen"
boolean visible = false
integer x = 3109
integer y = 832
integer taborder = 50
integer textsize = -10
string text = "M&ail"
end type

event clicked;call super::clicked;mailSession				mSes
mailReturnCode			mRet
mailMessage			mMsg
mailFileDescription		mAttach
string					ls_ret, ls_syntax, ls_name, ls_open_pathname, ls_filename
string					ls_attach_name='marc0801.dat.txt'
int						li_index, li_nret, li_nrecipients, li_nfile

//mailsession.mailLogon ( { userid, password } {, logonoption } )
mRet=mailReturnFailure!

mSes = CREATE mailSession


mRet = mSes.mailLogon ( mailNewSession! )
ls_ret = f_mail_error_to_string ( mRet, 'Logon:', FALSE )

If mRet <> mailReturnSuccess! Then
	MessageBox ("Mail Logon failed", 'Return Code <> mailReturnSuccess! '+ls_ret )
	mSes.mailLogoff()
	DESTROY mSes
	return
End If
SetPointer(HourGlass!)
mMsg.Subject = 'create MARC file'
mMsg.NoteText = 'this is text file'

mAttach.FileType = mailAttach!
mAttach.PathName = ls_attach_name
mAttach.FileName = 'marc0801.dat.txt'
	
mMsg.AttachmentFile[1] = mAttach
mMsg.Recipient[1].Name = 'raxt@loc.gov'
mMsg.Recipient[2].Name = 'pmag@loc.gov'
mMsg.Recipient[3].Name = 'Cheng, Sheng'
//mMsg.Recipient[4].Name = 'sheng.cheng@getronicsgov.com'
mMsg.Recipient[4].Name = 'sj_cheng@yahoo.com'
mRet = mSes.mailsend ( mMsg )
ls_ret = f_mail_error_to_string ( mRet, 'Send mail:', FALSE )
IF mRet <> mailreturnsuccess! THEN
		MessageBox ("Mail Send",'Return Code <> mailReturnSuccess! '+ls_ret )
	RETURN
END IF	
//	wf_logoff_mail(mSes, ls_attach_name)
mRet=mSes.mailLogoff()
DESTROY mSes


end event

type cbx_email from u_cbx within w_batch_formation
integer x = 2039
integer y = 708
integer width = 800
integer height = 68
boolean bringtotop = true
string text = "E-Mail MARC File"
end type

type cbx_batbks from u_cbx within w_batch_formation
integer x = 2039
integer y = 548
integer width = 800
integer height = 68
string text = "Create Batch/Books (Web)"
end type

type cbx_sched from u_cbx within w_batch_formation
integer x = 2039
integer y = 464
integer width = 800
integer height = 72
string text = "Create Batch (PICS)"
end type

type cbx_defupd from u_cbx within w_batch_formation
boolean visible = false
integer x = 2039
integer y = 388
integer width = 882
integer height = 68
string text = "Import Default Quantities "
end type

type st_cnt from statictext within w_batch_formation
integer x = 18
integer y = 540
integer width = 562
integer height = 128
integer textsize = -8
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 33554432
long backcolor = 67108864
string text = "Rows Count:"
boolean focusrectangle = false
end type

type st_updated from statictext within w_batch_formation
integer x = 18
integer y = 384
integer width = 571
integer height = 128
integer textsize = -8
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 33554432
long backcolor = 67108864
string text = "Rows updated:"
boolean focusrectangle = false
end type

type sle_rows_cnt from singlelineedit within w_batch_formation
integer x = 603
integer y = 536
integer width = 210
integer height = 80
integer taborder = 50
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 33554432
borderstyle borderstyle = stylelowered!
end type

type sle_rows_upd from singlelineedit within w_batch_formation
integer x = 603
integer y = 376
integer width = 210
integer height = 80
integer taborder = 40
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 33554432
borderstyle borderstyle = stylelowered!
end type

type dw_batch_process from u_dw within w_batch_formation
event pfc_hinttext pbm_mousemove
boolean visible = false
integer x = 50
integer y = 840
integer width = 50
integer height = 68
integer taborder = 30
string dataobject = "d_batch_process"
boolean vscrollbar = false
boolean livescroll = false
end type

event pfc_hinttext;call super::pfc_hinttext;string ls_object, ls_column, ls_column_tag
long ll_pos

//This script set's microhelp at the bottom of the screen for the dw_batch_formation
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

event constructor;call super::constructor;THIS.settransobject(sqlservertrans)
ib_rmbmenu = FALSE


end event

event getfocus;call super::getfocus;m_pics_main.m_Edit.m_Addrow.Enabled = FALSE
m_pics_main.m_Edit.m_Deleterow.Enabled = FALSE
m_pics_main.m_Edit.m_Cut.Enabled = FALSE
end event

event itemchanged;call super::itemchanged;date ld_cabdt, ld_rtndt
string ls_date
if dwo.name='batch_cabdt' then
	ls_date=data
	if isnull(ls_date) then
		messagebox('Error','You must enter a valid Copy Allotment batch date')
		return 1
	end if
	if IsDate(ls_date) then
		ld_cabdt=Date(ls_date)
		ld_rtndt=RelativeDate(ld_cabdt, 25)
		dw_batch_formation.SetItem(1,'rtndt', ld_rtndt)
	else
		messagebox('Error','You must enter a valid Copy Allotment batch date')
		return 1
	end if
end if
end event

event retrieveend;call super::retrieveend;//close(w_pics_retrieve_msg_box)
sle_rows_cnt.Text = string(rowcount)
end event

event retrievestart;call super::retrievestart;//Open(w_pics_retrieve_msg_box)
end event

event pfc_addrow;long ll_rc

ll_rc = Super::Event pfc_addrow()
dw_batch_formation.SetItem(ll_rc, "batch_cabdt",datetime(ld_date,time('00:00:00')))
return ll_rc
end event

event type long pfc_retrieve();long ll_row_count , i
//ll_row_count =THIS.Retrieve(ld_date)
return ll_row_count
end event

type st_transupdate from statictext within w_batch_formation
integer x = 841
integer y = 504
integer width = 1079
integer height = 148
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

type uo_progress from u_progressbar within w_batch_formation
integer x = 841
integer y = 368
integer width = 1079
integer height = 92
integer taborder = 20
boolean border = true
borderstyle borderstyle = stylelowered!
end type

on uo_progress.destroy
call u_progressbar::destroy
end on

type dw_get_bklist from u_pics_dw within w_batch_formation
boolean visible = false
integer x = 37
integer y = 768
integer width = 64
integer height = 44
integer taborder = 50
string dataobject = "d_get_bklist"
end type

event constructor;call super::constructor;this.SettransObject(SqlServertrans)
end event

type dw_update_for_books_orl from u_pics_dw within w_batch_formation
boolean visible = false
integer x = 37
integer y = 640
integer width = 64
integer height = 104
integer taborder = 40
string dataobject = "d_update_for_books_orl"
end type

event constructor;call super::constructor;this.SettransObject(SqlServerOracletrans)
end event

event sqlpreview;call super::sqlpreview;i_count++
	if uo_progress.visible=true and mod(i_count,10)=0 then
		uo_progress.of_Increment(1)
	end if
	if mod(i_count,100)=0 then
		parent.SetRedraw(TRUE)
	else
		parent.SetRedraw(false)
	end if
end event

type dw_update_for_batch_orl from u_pics_dw within w_batch_formation
boolean visible = false
integer x = 110
integer y = 704
integer width = 64
integer height = 104
integer taborder = 30
string dataobject = "d_update_for_batch_orl"
end type

event constructor;call super::constructor;this.SettransObject(SqlServerOracletrans)
end event

event sqlpreview;call super::sqlpreview;i_count++
	if uo_progress.visible=true and mod(i_count,10)=0 then
		uo_progress.of_Increment(1)
	end if
	if mod(i_count,100)=0 then
		parent.SetRedraw(TRUE)
	else
		parent.SetRedraw(false)
	end if
end event

type cb_exit from u_cb within w_batch_formation
event pfc_hinttext pbm_mousemove
string tag = "Exits the current screen"
integer x = 2702
integer y = 828
integer taborder = 0
integer textsize = -10
string text = "E&xit"
end type

event pfc_hinttext;call super::pfc_hinttext;w_pics_main.Setmicrohelp(THIS.Tag)
end event

event clicked;call super::clicked;datetime ld_dates

dw_batch_formation.accepttext()

ld_dates = dw_batch_formation.object.batch_cabdt[1]
IF ISNULL(ld_dates) THEN
	ib_disableclosequery = TRUE
END IF
ib_disableclosequery = TRUE
Parent.Event pfc_close()
m_pics_main.m_menu.PopMenu(300, 0)

end event

type cb_update from u_cb within w_batch_formation
event pfc_hinttext pbm_mousemove
string tag = "Click here to update the records"
integer x = 1531
integer y = 828
integer taborder = 0
integer textsize = -10
string text = "&Update"
end type

event pfc_hinttext;call super::pfc_hinttext;w_pics_main.Setmicrohelp(THIS.Tag)
end event

event clicked;//////////////////////////////////////////////////////////////////////////////////////////////////////////
//
//	Event: clicked for cb_update
//
//	Description:
//	1. Introduce parent table BATCH_DATA insert
//
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//	
//	Revision History
//
//	Developed by 	Date 				Version						Tracking#
//									
// Murali K.			03/26/2008      PICS 2.5 Modifications	 Reqs: CA.9
// Murali K.			03/27/2008		Update batch_data if appending
//Murali K. 			03/28/2008 		1. Added web_moved_yn in sched table to 
//											differentiate appended or new records , set it as N
//											2.Set amended col values while appending batch
//											3.Once the batch is copied to web update sched with 
//												book moved indicator to Y
//											4. Set actual qty, created by and created date audit cols
// Murali K.			03/31/2008		1.If appending change text for the checkboxes accordingly
//											2. if appened automatically copy to web no prompt
//						04/01/2008 		Unlock the batch entires after appending
// 						04/29/2008		RC is not the media RTB is the media, handle estimated sizes
//											based on book media 3.7 changes
// Murali K. 			06/19/2008 		Do not update actual_qty
// Murali K. 			07/10/2008 		Need to set the BATCH.Flag to 'C' or null based on the library lock
//											 IF CLOSED OR DISTRIBUTED DO NOT ALLOW PROCESS.
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//

Long rtn,rtns, li_qualify_count, li_qualify=0, li_count=0, li_count2, li_cpust, li_cpu,&
		li_cpuold, li_re, li_cnt, li_vols, li_bkseq, rtn2, rtn3,li_cnt2, li_prevbkseq, k
Long ll_count, i,j, li_ordqty, li_applen, li_pubyr, li_cur , li_row_count,li_row_count2
String ls_message,ls_msgparm[1], ls_dates, ls_bklist[], ls_bookList, ls_bkno, ls_libcd,ls_ordqty_flg,&
		ls_rtndt,ls_auth, ls_ttl, ls_publisher, ls_anno, ls_med, ls_casub, ls_review,&
		ls_ricd, ls_priority, ls_usid, ls_aepcd, ls_authfn, ls_estsz, ls_bkmed,&
		ls_oracle_ext_env, ls_prebk, ls_conno, ls_conno2, ls_ricd2, ls_prevbkmed,&
		ls_m, ls_yr, ls_d, ls_month, ls_user, ls_status
DateTime ld_dates, ld_cabdt, ld_rtndt
DateTime ldt_cabdt, ldt_cur= DateTime(Today(),Now()), ldt_rtndt
Date ld_date2
Int lcnt
date ld_today


n_cst_string inv_string//mailReturnTooManyFiles!

str_distrib_schedule lstr
//*******

ls_usid=sqlservertrans.userId
dw_batch_formation.AcceptText()
ld_dates = dw_batch_formation.object.batch_cabdt[1]
SetPointer(hourglass!)
ld_rtndt = dw_batch_formation.object.rtndt[1]
ld_date2=Date(ld_dates)
ls_rtndt=String(Date(ld_rtndt),'mm/dd/yyyy')
IF IsNull(ld_rtndt) THEN
	Messagebox('Error','You must enter a Return by Date.',exclamation!)
	dw_batch_formation.ScrollToRow(1)
	dw_batch_formation.SetRow(1)
	dw_batch_formation.SetFocus()
	dw_batch_formation.SetColumn('rtndt')
	RETURN
END IF


ld_today=today()
ls_user = gnv_app.of_getuserid()

// 07/10/2008 IF CLOSED OR DISTRIBUTED DO NOT ALLOW PROCESS.
SELECT BATCH_STATUS_CODE
INTO :ls_status
FROM BATCH_DATA@pic_link
WHERE CABDT =:ld_date2 using sqlservertrans ;

IF ls_status = 'C' OR ls_status = 'D' THEN
	messagebox('Error', 'Batch is either Closed or Distributed, Cannot Process any new books')
	RETURN
END IF

//03/27/2008  UPDATE BATCH_DATA for appending batch
IF cbx_apnd.checked = TRUE THEN
	UPDATE BATCH_DATA@pic_link
	SET MODIFIED_DATE =:ld_today ,
		MODIFIED_BY =:ls_user
		WHERE CABDT =:ld_date2
		using sqlservertrans ;

	IF sqlservertrans.sqlcode = 0 THEN
		Commit using sqlservertrans;
	ELSE
		ROLLBACK USING SQLSERVERTRANS ;
		Messagebox('DB Error', String(sqlservertrans.sqlcode) + ' '  + sqlservertrans.sqlerrtext)
		RETURN
	END IF
END IF

// If the Create Batch is not checked
IF cbx_sched.checked= FALSE THEN
	// 03/31/2008 change text
	IF cbx_apnd.checked = TRUE THEN
		cbx_sched.text = 'Appending Batch (PICS)'
	END IF
	ldt_cur= DateTime(Today(),Now())
	dw_batch_process.object.cabdt[1]=ld_dates// finish first step to update def of 
	rtn=dw_batch_process.Update(TRUE, TRUE)
	IF rtn= -1 THEN 
		ROLLBACK USING sqlserveroracletrans;
		ROLLBACK USING sqlservertrans;
		cbx_sched.enabled=FALSE
		wf_error()
	ELSE
		COMMIT USING sqlserveroracletrans;
		COMMIT USING sqlservertrans;
		cbx_sched.enabled=TRUE
		END IF
	END IF

	ls_dates = String (Date(ld_dates),'mm/dd/yyyy')

	//Check Database connectivity
	IF NOT sqlserveroracletrans.DbHandle() >0 THEN
		sqlserveroracletrans.of_connect() 
	END IF
	
       // Get the count of rows in batch table that have the new batch date
	SELECT COUNT(*)
		INTO :ll_count
	FROM batch
	WHERE batch.cabdt = :ld_dates
	USING sqlservertrans;
	IF f_check_dberror(sqlservertrans, 'select count(*) from batch where cabdt=:ls_dates') THEN

		// If the count is zero then insert into batch new batch date
		IF ll_count <= 0 THEN
			
			// 03/26/2008 insert into batch_data parent table
			ls_user = gnv_app.of_getuserid()
			INSERT INTO BATCH_DATA@pic_link (CABDT, BATCH_ANNOUNCED_YN,BATCH_STATUS_CODE,
															CREATED_BY, CREATED_DATE,MODIFIED_BY, MODIFIED_DATE) 
										VALUES			(:ld_dates, 'Y','O',:ls_user, :ldt_cur, :ls_user, :ldt_cur) using sqlservertrans ;
			COMMIT USING sqlservertrans; // update batch table must commit immediately
			
			INSERT INTO batch	(cabdt) VALUES (:ld_dates)
			USING sqlservertrans;
													
		END IF
		IF f_check_dberror(sqlservertrans,'insert into batch (cabdt) values(:ld_dates)') THEN
			COMMIT USING sqlservertrans; // update batch table must commit immediately
			li_row_count=dw_get_bklist.Retrieve()// get bkno list using where cascd='Q'
			IF li_row_count>0 THEN
				FOR i=1 TO li_row_count
					ls_bkno=dw_get_bklist.object.bkno[i]
					ls_bklist[i]=ls_bkno
				NEXT
			END IF
			IF cbx_sched.checked= FALSE THEN
				IF li_row_count<=0 THEN
					Messagebox('Error','No qualifying books. Can not insert into sched table,'+&
						'~nplease contact database administrator.')
					RETURN
				END IF
					
				st_transupdate.text='Updating data .'+&
				'This process will take several minutes.'
				SELECT COUNT(*) INTO :li_qualify_count
				FROM mchar
				WHERE mchar.cascd ='Q'
				USING sqlservertrans;
				
				li_qualify = li_qualify_count
				// count how many rows will be inserted into sched table
					SELECT COUNT (*) INTO :li_row_count2
					  FROM libdef@pic_link libdef, mchar, ttlinit, batch, org@pic_link
					 WHERE (mchar.med = libdef.med)
					     AND (mchar.bkmed = libdef.bkmed)
						AND (mchar.chno = ttlinit.chno)
						AND (ttlinit.casub = libdef.casub)
						AND (mchar.cascd = 'Q')
						AND (batch.cabdt = :ld_dates)
						AND (org.orgcd = libdef.libcd)
						AND (org.libstcd <> 'D')					  
						USING sqlservertrans;
						
				ls_m=Mid(ls_dates,1,2)
				ls_d=Mid(ls_dates,4,2)
				ls_yr=Mid(ls_dates,7,4)
				CHOOSE CASE ls_m
					CASE '01'
					 ls_month='Jan'	
					CASE '02'
					 ls_month='Feb'
					CASE '03'
					 ls_month='Mar'
					CASE '04'
					 ls_month='Apr'	
					CASE '05'
					 ls_month='May'
					CASE '06'
					 ls_month='Jun'
					CASE '07'
					 ls_month='Jul'	
					CASE '08'
					 ls_month='Aug'
					CASE '09'
					 ls_month='Sep'
					CASE '10'
					 ls_month='Oct'	
					CASE '11'
					 ls_month='Nov'
					CASE '12'
					 ls_month='Dec'
				END CHOOSE
				
				ls_dates=ls_d+'-'+ls_month+'-'+ls_yr
				sle_rows_cnt.text=String(li_row_count2)
				st_cnt.text='Total rows inserted into SCHED: '
				sle_rows_upd.text= String(li_qualify)
				st_updated.text='Total books in current batch: '
				sle_rows_cnt.visible=TRUE
				st_cnt.visible=TRUE
				sle_rows_upd.visible=TRUE
				st_updated.visible=TRUE	
				
				// Insert into SCHED table
				// 03/28/2008 added web_moved_yn in sched table to differentiate appended or new records , set it as N last col before it is moved to oracle/web)
				// 11/14/2008 remove generic insert 
				INSERT INTO sched ( BKSEQ,   
										       BKMED,   
										       LIBCD,   
										       CABDT,   
										       SELRTNDT,   
										       SELCD,   
										       SELQTY,   
				 						      LIBRECDT,   
										       LIBRECQTY,   
										       ORDQTY,   
					       WEB_MOVED_YN,
							 CREATED_BY,
							 CREATED_DATE)  // add audit columns
		 
				 SELECT mchar.bkseq, mchar.bkmed, libdef.libcd, batch.cabdt, '', 'D', 0, '', 0,
						  DECODE (MCHAR.PRIORITY,'N', CURR_N_DEFQTY,'P', CURR_P_DEFQTY,'R', CURR_R_DEFQTY), 'N',:ls_user,sysdate
					FROM libdef@pic_link libdef, mchar, ttlinit, batch, org@pic_link
					 WHERE (mchar.med = libdef.med)
					 AND (mchar.bkmed = libdef.bkmed)
					 AND (mchar.chno = ttlinit.chno)
					 AND (org.orgcd = libdef.libcd)
					 AND (org.libstcd <> 'D')					  
					 AND (    (ttlinit.casub = libdef.casub)
							AND (mchar.cascd = 'Q')
							AND (batch.cabdt = :ld_dates)
						  )				
					USING sqlservertrans;
					
				IF f_check_dberror(sqlservertrans,'insert sched') THEN
					COMMIT USING sqlservertrans;
				ELSE
					ROLLBACK USING sqlservertrans;
						wf_error()
				END IF
	
// Update MCHAR table	
				UPDATE mchar
					SET mchar.cascd = 'S',
						 mchar.cabdt = :ld_dates
				 WHERE mchar.cascd = 'Q'
				USING sqlservertrans;
				IF f_check_dberror(sqlservertrans,'update mchar') THEN
					COMMIT USING sqlservertrans;
				ELSE
					ROLLBACK USING sqlservertrans;
						wf_error()
				END IF
				li_cpuold=Cpu()
				
				// Check for any errors
				IF f_check_dberror(sqlservertrans,'delete,insert,update opration') THEN
	//				rtns=dw_batch_formation.Retrieve(ld_dates)
					ldt_cur= DateTime(Today(),Now())
					//step 2 finished
					dw_batch_process.object.schedins[1]=ldt_cur
					dw_batch_process.object.schedinsrows[1]=li_row_count2
					dw_batch_process.object.schedinsuser[1]=ls_usid
					rtn=dw_batch_process.Update(TRUE, TRUE)
					IF rtn= -1 THEN 
						ROLLBACK USING sqlserveroracletrans;
						ROLLBACK USING sqlservertrans;
						wf_error()
					ELSEIF rtn=1 THEN
						cbx_sched.checked=TRUE
						COMMIT USING sqlserveroracletrans;
						COMMIT USING sqlservertrans; //update sched table(insert rows)
						st_sched.text=String(ldt_cur,'mm/dd/yyyy hh:mm:ss')
					END IF
					ib_disableclosequery = TRUE
				ELSE
					ROLLBACK USING sqlservertrans; 
					wf_error()
				END IF
			END IF// end if cbx_sched.checked=false
			
		ELSE //else if f_check_dberror
	
			dw_batch_formation.Reset()
			dw_batch_formation.InsertRow(0)	
			dw_batch_formation.SetFocus()
			cb_find.enabled = TRUE
			cb_clear.enabled = FALSE
			ib_disableclosequery = TRUE
			cb_find.Default = TRUE
			cb_update.enabled = FALSE
			wf_error()
		END IF // end if f_check_dberror insert batch(cabdt) values(:cabdt)
	SetPointer(hourglass!)
	IF cbx_batbks.checked=FALSE THEN
		// 03/31/2008 if appened automatically copy to web no prompt
		IF cbx_apnd.checked = FALSE THEN
			li_re = Messagebox("","Do you wish to transfer the new copy "+&
				"~allotment batch to the web?",question!,yesNo!,1)	
			ELSE
				li_re = 1
			END IF
	ELSE
		li_re=3
	END IF
	
	
	// copy to web
	IF li_re = 1 THEN
		uo_progress.of_SetTextColor(Rgb(255, 255, 255))
		String ls_msgtext[ ] = {" 0 %","10 %", "20 %", "30 %", "40 %", "50 %", "60 %", &
			"70 %", "80 %", "90 %","100 %"}
		li_count2=(li_row_count2 *0.1 + li_qualify *0.1)
		IF cbx_batbks.checked =FALSE THEN
			
			cbx_batbks.enabled=TRUE
			// 03/31/2008 change text
			IF cbx_apnd.checked = TRUE THEN
				cbx_batbks.text = 'Appending Batch/Books (Web)'
			END IF


			parent.SetRedraw(TRUE)
			uo_progress.visible=TRUE
			uo_progress.of_SetMinimum(0)
			uo_progress.of_SetMaximum(li_count2)
			uo_progress.of_SetDisplayStyle(3)
			uo_progress.of_SetMessageText(ls_msgtext)
			uo_progress.of_SetPosition(0)
	
			sle_rows_cnt.visible=FALSE
			st_cnt.visible=FALSE
			sle_rows_upd.visible=FALSE
			st_updated.visible=FALSE
			st_transupdate.text='Retrieving data .'+&
				'This process will take several minutes.'
			SetPointer(hourglass!)
			//select rows from mchar, sched for inserting batch table in oracle

			ll_count=dw_get_data_for_batch.Retrieve(ld_dates)

			IF IsValid(w_pics_retrieve_msg_box) THEN
				Close(w_pics_retrieve_msg_box)
			END IF
			li_row_count2=ll_count
			SetPointer(hourglass!)
			IF ll_count=0 THEN wf_error()

			FOR i=1 TO ll_count
				SetPointer(hourglass!)
				ls_libcd=Trim(dw_get_data_for_batch.object.libcd[i])
				ls_bkno=dw_get_data_for_batch.object.bkno[i]
				ld_cabdt=dw_get_data_for_batch.object.cabdt[i]
				ldt_cabdt=DateTime(Date(ld_cabdt),Time('00:00:00'))
				ldt_rtndt=DateTime(Date(ld_rtndt),Time('00:00:00'))
				li_ordqty=dw_get_data_for_batch.object.ordqty[i]
				ls_ordqty_flg=dw_get_data_for_batch.object.ordqty_flg[i]
				ls_bkmed=dw_get_data_for_batch.object.sched_bkmed[i]
				li_bkseq=dw_get_data_for_batch.object.sched_bkseq[i]
				li_cur=dw_update_for_batch_orl.InsertRow(0)
				dw_update_for_batch_orl.object.libcd[li_cur]=ls_libcd
				dw_update_for_batch_orl.object.bkno[li_cur]=ls_bkno
				dw_update_for_batch_orl.object.defqty[li_cur]=li_ordqty	
				dw_update_for_batch_orl.object.cabdt[li_cur]=ldt_cabdt
				dw_update_for_batch_orl.object.rtndt[li_cur]=ldt_rtndt
				dw_update_for_batch_orl.object.ordqty_flg[li_cur]=ls_ordqty_flg
				dw_update_for_batch_orl.object.bkmed[li_cur]=ls_bkmed
				dw_update_for_batch_orl.object.bkseq[li_cur]=li_bkseq
				// 03/28/2008 Set amended col values while appending batch
				IF cbx_apnd.checked=TRUE THEN
					dw_update_for_batch_orl.object.book_amended_yn[li_cur]='Y'
					dw_update_for_batch_orl.object.date_amended[li_cur]=today()
				END IF
				// 06/19/2008 do not update actual_qty
//				dw_update_for_batch_orl.object.actual_qty[li_cur]=li_ordqty	 // actual qty set to def qty
				dw_update_for_batch_orl.object.created_by[li_cur]=ls_user
				dw_update_for_batch_orl.object.created_date[li_cur]=ld_today
				dw_update_for_batch_orl.object.modified_by[li_cur]=ls_user
				dw_update_for_batch_orl.object.modified_date[li_cur]=ld_today
								
				// 03/31/2008 if negative qty set the batch neg cond yn indicator in batch_data table to Y
				IF li_ordqty < 0 THEN
						UPDATE BATCH_DATA@pic_link
						SET BATCH_NEG_COND_YN = 'Y',
						MODIFIED_DATE =:ld_today ,
						MODIFIED_BY =:ls_user
						WHERE CABDT =:ld_date2
						using sqlservertrans ;
				END IF
				///////////
			NEXT
			SetPointer(hourglass!)

			dw_get_data_for_books.Retrieve(ld_dates)

			IF IsValid(w_pics_retrieve_msg_box) THEN
				Close(w_pics_retrieve_msg_box)
			END IF
			ll_count=dw_get_data_for_books.RowCount()
			SetPointer(hourglass!)
			IF ll_count=0 THEN wf_error()

			FOR i=1 TO ll_count
				SetPointer(hourglass!)
				//select data from mchar, ttlinit, annotation to get data for insert books
				//in database orace
				ls_bkno=dw_get_data_for_books.object.bkno[i]
				ls_auth=dw_get_data_for_books.object.auth[i]
				ls_authfn=dw_get_data_for_books.object.authfn[i]
				ls_aepcd=dw_get_data_for_books.object.aepcd[i]
				li_vols=dw_get_data_for_books.object.vols[i]
				ls_ttl=dw_get_data_for_books.object.ttl[i]
//				ls_Ttl = inv_String.of_RemoveNonPrint(ls_Ttl)
				ls_ttl = f_change_pipe_html_tag(ls_ttl)
				ls_ttl=Left(ls_ttl,255)
				ls_publisher=dw_get_data_for_books.object.publisher[i]
				ls_anno=dw_get_data_for_books.object.anno[i]
//				ls_Anno = inv_String.of_RemoveNonPrint(ls_Anno)
				ls_anno = f_change_pipe_html_tag(ls_anno)
				ls_anno=Left(ls_anno,500)
				ld_cabdt=dw_get_data_for_books.object.cabdt[i]
				ldt_cabdt=DateTime(Date(ld_cabdt),Time('00:00:00'))
				ls_med=dw_get_data_for_books.object.med[i]
				ls_casub=dw_get_data_for_books.object.casub[i]
				li_pubyr=dw_get_data_for_books.object.pubyr[i]
				ls_review=dw_get_data_for_books.object.srcdoc[i]
//				ls_Review = inv_String.of_RemoveNonPrint(ls_Review)
				ls_review = f_change_pipe_html_tag(ls_review)
				ls_review=Left(ls_review, 34)
				li_applen=dw_get_data_for_books.object.applen[i]
				ls_conno=Trim(dw_get_data_for_books.object.conno[i])
				
				ls_ricd=Trim(dw_get_data_for_books.object.ricd[i])
				IF IsNull(ls_ricd)=FALSE AND ls_ricd<>"" THEN
					li_cnt=dw_ri_2_prevbk.Retrieve(ls_conno)
					IF li_cnt=1 THEN
						IF ls_ricd='RI' THEN
							ls_prebk=dw_ri_2_prevbk.object.prebk[1]
							ls_prebk='Reissue '+ls_prebk
						ELSEIF ls_ricd='RR' THEN
							ls_prebk=dw_ri_2_prevbk.object.prebk[1]
							ls_prebk='Rerecord '+ls_prebk
						END IF
					ELSEIF li_cnt=2 THEN// if there are two prevbkseq
						FOR j=1 TO li_cnt
							ls_prevbkmed=Trim(dw_ri_2_prevbk.object.prevbkmed[j])
							li_prevbkseq=dw_ri_2_prevbk.object.prevbkseq[j]
							li_cnt2=dw_ri_1_or_2_conno.Retrieve(ls_prevbkmed,li_prevbkseq)
							IF li_cnt2=2 THEN
								FOR k=1 TO li_cnt2
									ls_conno2=dw_ri_1_or_2_conno.object.conno[k]
									IF ls_conno2<> ls_conno THEN
										SELECT ricd INTO :ls_ricd2
										FROM mchar
										WHERE conno=:ls_conno2
										USING sqlservertrans;
										IF f_check_dberror(sqlservertrans,'select ls_ricd2 from mchar')=FALSE THEN
											RETURN
										END IF
										IF ls_ricd2='RI' THEN
											ls_prebk='Reissue '+ls_prevbkmed+String(li_prevbkseq)
										ELSEIF ls_ricd2='RR' THEN
											ls_prebk='Rerecord '+ls_prevbkmed+String(li_prevbkseq)
										END IF
									END IF// end if ls_conno2<> ls_conno
								NEXT// for k=1 to 
							END IF// end if li_cnt2=2
						NEXT	// for j=1 to 
					END IF //end if li_cnt=2
				END IF// end if isnull(ls_ricd)=fals
				ls_priority=dw_get_data_for_books.object.priority[i]
				ls_bkmed=dw_get_data_for_books.object.mchar_bkmed[i]
				li_bkseq=dw_get_data_for_books.object.mchar_bkseq[i]
				li_cur=dw_update_for_books_orl.InsertRow(0)
				CHOOSE CASE ls_aepcd
					CASE 'I'
						IF IsNull(ls_authfn)=FALSE THEN
							ls_auth=Trim(ls_auth)+' '+Trim(ls_authfn)
						ELSE
							ls_auth=Trim(ls_auth)
						END IF
					CASE 'N'
						ls_auth=' '
					CASE 'L'
						ls_auth=Trim(ls_auth)
					CASE ELSE
						IF IsNull(ls_authfn)=FALSE THEN
							ls_auth=Trim(ls_auth)+', '+Trim(ls_authfn)
						ELSE
							ls_auth=Trim(ls_auth)
						END IF
				END CHOOSE
				ls_med=Trim(ls_med)
				// If the medium is RTB, deal with it just like RC
				// 04/29/2008 RTB media should go in as RTB FOR RC AND DB book medias in the books table
				IF ls_bkmed = 'RC' OR ls_bkmed = 'DB' THEN
					ls_med = 'RTB'
				END IF
//				IF ls_med = 'RTB' THEN
//					ls_med = 'RC'
//				END IF
//				CHOOSE CASE ls_med
///  Check book media instead of media - 04/29/2008
			CHOOSE CASE ls_med
					// 04/29/2008 address DB estimated size also
				CASE 'RTB'
					IF ls_bkmed =  'DB' THEN
						IF IsNull(li_vols)=FALSE THEN
							ls_estsz=String(li_vols)+' '+'cart'
						ELSEIF IsNull(li_applen)=FALSE THEN
							IF Mod(li_applen,4)=0 THEN 
								ls_estsz=String(Int(li_applen/4))+' cart'
							ELSE
								ls_estsz=String(Int(li_applen/4 + 1))+' cart'
							END IF
						ELSE
							ls_estsz='0 cart'
						END IF
					ELSE
					
						IF IsNull(li_vols)=FALSE THEN
							ls_estsz=String(li_vols)+' '+'cass'
						ELSEIF IsNull(li_applen)=FALSE THEN
							IF Mod(li_applen,4)=0 THEN 
								ls_estsz=String(Int(li_applen/4))+' cass'
							ELSE
								ls_estsz=String(Int(li_applen/4 + 1))+' cass'
							END IF
						ELSE
							ls_estsz='0 cass'
						END IF
					END IF
				CASE 'BR'
						IF IsNull(li_vols)=FALSE THEN
							ls_estsz=String(li_vols)+' '+'vols'
						ELSEIF IsNull(li_applen)=FALSE THEN
							IF Mod(li_applen,250)=0 THEN
								ls_estsz=String(Int(li_applen/250))+' vols'
							ELSE
								ls_estsz=String(Int(li_applen/250 + 1))+' vols'
							END IF
						ELSE
							ls_estsz='0 vols'
						END IF
				CASE 'P/B'
						ls_estsz='1 vols'
				CASE ELSE
						ls_estsz='unknown'
				END CHOOSE
				
				lcnt = 0
				
				SELECT COUNT(*) INTO :lcnt
				FROM books
				WHERE bkseq = :li_bkseq and bkmed = :ls_bkmed
				USING SQLServerOracleTrans;
				
				IF lcnt > 0 THEN
				  	DELETE FROM books WHERE bkseq = :li_bkseq and bkmed = :ls_bkmed USING SQLServerOracleTrans;
				 	// Check for any errors
					IF f_check_dberror(SQLServerOracleTrans,'delete from books') THEN
						COMMIT USING SQLServerOracleTrans; // commit immediately
					END IF				  
				END IF
				
				dw_update_for_books_orl.object.bkno[li_cur]=ls_bkno
				dw_update_for_books_orl.object.auth[li_cur]=ls_auth
				
				dw_update_for_books_orl.object.ttl[li_cur]=ls_ttl
				dw_update_for_books_orl.object.publisher[li_cur]=ls_publisher
				dw_update_for_books_orl.object.anno[li_cur]=ls_anno
	
				dw_update_for_books_orl.object.batchdt[li_cur]=ldt_cabdt
				dw_update_for_books_orl.object.med[li_cur]=ls_med
				dw_update_for_books_orl.object.casub[li_cur]=ls_casub
				dw_update_for_books_orl.object.pubyr[li_cur]=li_pubyr
				
				dw_update_for_books_orl.object.review[li_cur]=ls_review
				dw_update_for_books_orl.object.estsz[li_cur]=ls_estsz
				IF IsNull(ls_ricd)=TRUE OR Trim(ls_ricd)="" THEN
					dw_update_for_books_orl.object.ricd[li_cur]=ls_ricd
				ELSE
					dw_update_for_books_orl.object.ricd[li_cur]=ls_prebk
				END IF
				dw_update_for_books_orl.object.priority[li_cur]=ls_priority
				dw_update_for_books_orl.object.bkmed[li_cur]=ls_bkmed
				dw_update_for_books_orl.object.bkseq[li_cur]=li_bkseq
			NEXT

		ELSE
			st_transupdate.text=''
		END IF	// end if cbx_batbks.checked
	ELSEIF li_re=2 THEN //else if li_re=1 transfer to web or not
		RETURN
	END IF
ELSE// end if f_check_dberr('','selectSqlServerOracleTrans
	RETURN
END IF
IF IsValid(w_pics_retrieve_msg_box) THEN
	Close(w_pics_retrieve_msg_box)
END IF
IF cbx_batbks.checked= FALSE AND	li_re = 1 THEN
	SetPointer(hourglass!)
	li_count=(dw_update_for_books_orl.RowCount()+dw_update_for_batch_orl.RowCount())* 0.1
	st_transupdate.text="Updating the web database. This process will take several minutes."
	parent.SetRedraw(TRUE)
	uo_progress.visible=TRUE
	uo_progress.of_SetMinimum(0)
	uo_progress.of_SetMaximum(li_count)
	uo_progress.of_SetDisplayStyle(3)
	uo_progress.of_SetMessageText(ls_msgtext)
	uo_progress.of_SetPosition(0)
	
	dw_update_for_books_orl.SetTransObject(sqlserveroracletrans)

	SetPointer(hourglass!)
	dw_update_for_batch_orl.SetTransObject(sqlserveroracletrans)
	rtn=dw_update_for_batch_orl.Update()
	SetPointer(hourglass!)

	IF rtn=1 THEN
		
		// 03/28/2008 Once the batch is copied to web, update sched with web moved indicator to Y
		update sched
		set web_moved_yn = 'Y'
		where web_moved_yn = 'N' and
				cabdt =:ld_date2  using sqlservertrans ;
				
		SetPointer(hourglass!)
		ll_count = dw_update_for_books_orl.RowCount()
		FOR i=1 TO ll_count
			ls_bkno=dw_update_for_books_orl.object.bkno[i]
			IF pos ( ls_booklist, ls_bkno ) > 0 THEN
				MessageBox("Duplicate Book Number", ls_bkno)
			END IF
			IF i < ll_count THEN
				ls_booklist= ls_booklist +ls_bkno + ','
			ELSE
				ls_booklist= ls_booklist +ls_bkno
			END IF	
		NEXT
//		MessageBox('Books List', ls_booklist)
		rtn=dw_update_for_books_orl.Update()
		
		SetPointer(hourglass!)
		parent.SetRedraw(TRUE)
		uo_progress.visible=TRUE
		st_transupdate.text=''
		uo_progress.of_SetMinimum(0)
		uo_progress.of_SetMaximum(li_count)
		uo_progress.of_SetDisplayStyle(3)
		uo_progress.of_SetMessageText(ls_msgtext)
		uo_progress.of_SetPosition(0)
		
		IF rtn=1 THEN
			SetPointer(hourglass!)
			ldt_cur= DateTime(Today(),Now())
//			dw_batch_process.Retrieve(ld_dates)
// you finish the 3th step
			dw_batch_process.object.webupd[1]=ldt_cur
			dw_batch_process.object.batchinsrows[1]=li_row_count2
			dw_batch_process.object.booksinsrows[1]=ll_count
			dw_batch_process.object.webupduser[1]=ls_usid
			
			rtn=dw_batch_process.Update(TRUE, TRUE)
			SetPointer(hourglass!)
			IF rtn= -1 THEN 
				ROLLBACK USING sqlserveroracletrans;
				ROLLBACK USING sqlservertrans;
				wf_error()
			ELSEIF rtn=1 THEN
				
				
				COMMIT USING sqlserveroracletrans;
				COMMIT USING sqlservertrans;
				
				// 04/01/2008 unlock the batch entires after appending
				IF cbx_apnd.checked = TRUE THEN
					// 07/10/2008 need to set the flag to 'C' or null based on the library lock
					wf_librarylock(ld_date2)
				END IF


				st_web.text=String(ldt_cur,'mm/dd/yyyy hh:mm:ss')
				cbx_batbks.checked=TRUE
				Messagebox('Batch Formation','Copy allotment batch successfully'+&
					'~ntransferred to the web.')
			END IF// end if update dw_batch_process
		ELSE
			ROLLBACK USING sqlserveroracletrans;
			ROLLBACK USING sqlservertrans;
			wf_error()
		END IF// end if update books
	ELSE
		ROLLBACK USING sqlserveroracletrans;
		ROLLBACK USING sqlservertrans;
		wf_error()
	END IF //end if update batch table
END IF// end if cbx_batbks.checked=false
IF  sqlserveroracletrans.DbHandle() >0 THEN
	IF sqlserveroracletrans.of_disconnect() < 0 THEN
		Messagebox("Error","Disconnected from the Oracle Database. Please try again",stopSign!)
		RETURN
	END IF
END IF
IF IsValid(w_pics_retrieve_msg_box) THEN
	Close(w_pics_retrieve_msg_box)
END IF
SetPointer(arrow!)

lstr.ld_date= Date(ld_dates)
IF cbx_file.checked=FALSE THEN
	cbx_file.enabled=TRUE
	OpenWithParm(w_create_marc_file_on_demand, lstr)
END IF
IF IsValid(w_pics_retrieve_msg_box) THEN
	Close(w_pics_retrieve_msg_box)
END IF


// 03/31/2008 update closed status in batch data if all the checkboxes are ticked
// how many times they can append, if closed can they append?

RETURN


end event

type dw_batch_formation from u_dw within w_batch_formation
event pfc_hinttext pbm_mousemove
integer x = 169
integer y = 20
integer width = 3136
integer height = 292
integer taborder = 10
string dataobject = "d_batch_formation"
boolean vscrollbar = false
boolean livescroll = false
end type

event pfc_hinttext;call super::pfc_hinttext;string ls_object, ls_column, ls_column_tag
long ll_pos

//This script set's microhelp at the bottom of the screen for the dw_batch_formation
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

event getfocus;call super::getfocus;m_pics_main.m_Edit.m_Addrow.Enabled = FALSE
m_pics_main.m_Edit.m_Deleterow.Enabled = FALSE
m_pics_main.m_Edit.m_Cut.Enabled = FALSE
end event

event constructor;call super::constructor;THIS.settransobject(sqlservertrans)
ib_rmbmenu = FALSE


end event

event pfc_retrieve;long ll_row_count , i
ll_row_count =THIS.Retrieve(ld_date)
return ll_row_count
end event

event retrievestart;call super::retrievestart;Open(w_pics_retrieve_msg_box)
end event

event retrieveend;call super::retrieveend;date ld_dates
long ll_row,ll_count
integer rtn

dw_batch_formation.Accepttext()

  ll_row = dw_batch_formation.Event pfc_addrow()
  SELECT COUNT(*) INTO :ll_count FROM mchar
  WHERE cascd = 'Q'
  USING sqlservertrans;
  IF f_check_dberror( sqlservertrans, "select count(*) from mchar' whrere cascd = 'Q'") Then
	  dw_batch_formation.Setitem(dw_batch_formation.Getrow(),"mchar_cascd",ll_count)

	  This.Setitem(ll_row,"batch_cabdt",ld_date)
	  dw_batch_formation.Setfocus()
	  IF ll_count >0 then
		  cb_find.Enabled = FALSE
		  cb_clear.Enabled = TRUE
		  ib_disableclosequery = FALSE
		  cb_find.Default = FALSE         
		  cb_update.Enabled = TRUE
		  cb_print.Enabled =FALSE
		  RETURN
		ELSE
//			MessageBox('WARNING!','No data retrieved, Nothing to update!')
			cb_find.Enabled = TRUE
		  cb_clear.Enabled = FALSE
		  ib_disableclosequery = TRUE
		  cb_find.Default = TRUE    
		  cb_update.Enabled = FALSE
		  ib_update =FALSE
		  cb_print.Enabled =FALSE
		  RETURN
		END IF
	END IF


end event

event pfc_addrow;long ll_rc

ll_rc = Super::Event pfc_addrow()
dw_batch_formation.SetItem(ll_rc, "batch_cabdt",ld_date)
return ll_rc
end event

event itemchanged;call super::itemchanged;//////////////////////////////////////////////////////////////////////////////////////////////////////////
//
//	Event: itemchanged fo dw_batch_formation
//
//	Description:
//	Batch date validations
//
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//	
//	Revision History
//
//	Developed by 	Date 				Version						Tracking#
//									
// Murali K.			03/26/2008      PICS 2.5 Modifications	 Reqs: CA.9
// Murali K.			07/14/2008 check 'D' for distributed also 
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//

DateTime ld_cabdt
Date ld_cabdtdate, ld_rtndtdate
Long li_pos, ll_count
string ls_status
String ls_date


IF DWO.Name='batch_cabdt' THEN
	ls_date=Data
	IF IsNull(ls_date) THEN
		Messagebox('Error','You must enter a valid Copy Allotment batch date')
		RETURN 1
	END IF
	ls_date=Left(ls_date,10)
	IF IsDate(ls_date) THEN
		ld_cabdtdate=Date(ls_date)
		ld_rtndtdate=RelativeDate(ld_cabdtdate, 25)
		ld_calculated_rtndt=DateTime(ld_rtndtdate,Time('00:00:00'))
		dw_batch_formation.SetItem(1,'rtndt', ld_calculated_rtndt)
	ELSE
		Messagebox('Error','You must enter a valid Copy Allotment batch date')
		RETURN 1
	END IF
	
	////////////////////////////////////////////////////////////////////////////////////////////////////////////
	// 03/26/2008 Batch date should be beginning of every month
	ld_cabdtdate=Date(ls_date)
	IF Day(ld_cabdtdate) <> 1 THEN
		Messagebox('Error','Batch Date should be beginning of the month')
		RETURN 1
	END IF

	ls_date = String(ld_cabdtdate,'dd-mmm-yyyy')
	
	// Cannot skip batches
	select count(*)
	into :ll_count
	from batch_data@pic_link
	where cabdt = TO_DATE(ADD_MONTHS(:LS_DATE,-1)) using sqlservertrans ;

	IF ll_count = 0 THEN
		Messagebox('Error','Cannot skip Batch month')
		RETURN 1
	END IF

	// Previous batch must be closed
	select batch_status_code
	into :ls_status
	from batch_data@pic_link
	where cabdt = TO_DATE(ADD_MONTHS(:LS_DATE,-1)) using sqlservertrans ;
	// 07/14/2008 check 'D' for distributed also 
	IF ( ls_status <> 'C' AND ls_status <> 'D' )THEN
		Messagebox('Error','Previous batch need to be closed')
		RETURN 1
	END IF
	////////////////////////////////////////////////////////////////////////////////////////////////////////////
	
	
ELSEIF DWO.Name='rtndt' THEN
	ls_date=Data
	ls_date=Left(ls_date,10)
	IF IsDate(ls_date) THEN
		ld_rtndtdate=Date(ls_date)
		ld_calculated_rtndt=DateTime(ld_rtndtdate,Time('00:00:00'))
	ELSE
		Messagebox('Error','You must enter a valid return date')
		RETURN 1
	END IF	
	
	ld_cabdtdate = Date(this.object.batch_cabdt[1])
	IF Month(ld_rtndtdate) <> Month(ld_cabdtdate)  THEN
		Messagebox('Error','Return Date must be within the same batch month')
		RETURN 1
	END IF
	
END IF
end event

event itemerror;call super::itemerror;RETURN 1
end event

type cb_find from u_cb within w_batch_formation
event pfc_hinttext pbm_mousemove
string tag = "Find~'s the records based on copyallotment batch date"
integer x = 946
integer y = 828
integer taborder = 0
integer textsize = -10
string text = "F&ind"
boolean default = true
end type

event pfc_hinttext;call super::pfc_hinttext;w_pics_main.setmicrohelp(This.tag)
end event

event clicked;call super::clicked;//////////////////////////////////////////////////////////////////////////////////////////////////////////
//
//	Event: clicked for cb_find
//
//	Description:
//	Appending validations
//
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//	
//	Revision History
//
//	Developed by 	Date 				Version						Tracking#
//									
// Murali K.			03/27/2008      PICS 2.5 Modifications	 Reqs: CA.9
//											Within cutoff date allow appending 
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//

String ls_Libcd, ls_Cabdt, ls_M, ls_Mbefore, ls_Yr
Long ll_Rows,ll_Count, li_Yr, li_Yrbefore, li_Find, i
Integer rtn
DateTime ldt_Null
DateTime ld_Dates, ld_Rtndt, ld_Cabdt,ld_Cabdt2, ld_Cabdtbefore
SetNull(ldt_Null)
DateTime ldt_Defimp, ldt_Schedins, ldt_Webupd,ldt_Marcexp, ldt_Marcemail, &
			ldt_Batchimp, ldt_Distfile, ldt_Distsched
Date ld_Date2, ld_Rtndate
Time lt_Time=Time('00:00:00')
String ls_status
date ld_dt
	
ldt_Defimp=ldt_Null
ldt_Schedins=ldt_Null
ldt_Webupd=ldt_Null
ldt_Marcexp=ldt_Null
ldt_Marcemail=ldt_Null
ldt_Batchimp=ldt_Null
ldt_Distfile=ldt_Null
ldt_Distsched=ldt_Null
dw_Batch_formation.AcceptText()

ld_Dates = dw_Batch_formation.object.batch_Cabdt[1]
ld_Cabdt= ld_Dates
ld_Date2=Date(ld_Dates)


IF IsNull(ld_Dates) THEN
	Messagebox("Error","You must enter a valid Copy Allotment batch date.",stopSign!)
	SetNull(ld_Dates)
	dw_Batch_formation.SetItem(1,"batch_cabdt",ld_Dates)	
	dw_Batch_formation.SetFocus()
	dw_Batch_formation.SetColumn("batch_cabdt")
	ib_Disableclosequery = TRUE
	RETURN
END IF

IF NOT IsNull(ld_Dates)  THEN
	//**this datawindow do not use for update purpose, just check how many row have cascd='Q'
   ll_Rows = dw_Batch_formation.Retrieve(ld_Dates)
	IF ll_Rows <> 0 THEN	
		Close(w_Pics_retrieve_msg_box)
      		cb_Find.enabled = FALSE
     
      		ib_Disableclosequery = FALSE
      		cb_Find.Default = FALSE
		cb_Clear.enabled = TRUE
		cb_Update.enabled = TRUE
		
		cb_Print.enabled =TRUE
      		dw_Batch_formation.SetItem(dw_Batch_formation.GetRow(),"batch_cabdt",ld_Dates)
//		ld_calculated_rtndt = dw_Batch_formation.object.rtndt[ll_rows]
		
		// 07/11/2008 retrieve latest return date and set it.
		select distinct rtndt
		into :ld_calculated_rtndt
		from batch@pic_link
		where cabdt = :ld_dates using sqlservertrans ;
		
		ld_Rtndate=RelativeDate(ld_Date2,25)
		ld_Rtndt=DateTime(ld_Rtndate,lt_Time)
		IF NOT(IsNull(ld_calculated_rtndt)) THEN
			dw_Batch_formation.object.rtndt[1]=ld_calculated_rtndt
		ELSE
			dw_Batch_formation.object.rtndt[1]=ld_Rtndt
			ld_calculated_rtndt = ld_rtndt // 07/11/2008
		END IF
      		dw_Batch_formation.SetFocus()		
	ELSE
		Close(w_Pics_retrieve_msg_box)
		dw_Batch_formation.EVENT pfc_addrow()
		dw_Batch_formation.SetItem(dw_Batch_formation.GetRow(),"batch_cabdt",ld_Dates)
		
		// 07/11/2008 retrieve latest return date and set it.
		select distinct rtndt
		into :ld_calculated_rtndt
		from batch@pic_link
		where cabdt = :ld_dates using sqlservertrans ;
		
		ld_Rtndate=RelativeDate(ld_Date2,25)
		ld_Rtndt=DateTime(ld_Rtndate,lt_Time)
		IF NOT(IsNull(ld_calculated_rtndt)) THEN
			dw_Batch_formation.object.rtndt[1]=ld_calculated_rtndt
		ELSE
			dw_Batch_formation.object.rtndt[1]=ld_Rtndt
			ld_calculated_rtndt = ld_rtndt // 07/11/2008
		END IF
		dw_Batch_formation.SetFocus()		
		cb_Find.enabled = FALSE
		cb_Clear.enabled = FALSE		   
		cb_Update.enabled = FALSE	
		cb_Print.enabled =FALSE
		ib_Disableclosequery = TRUE			
	END IF
END IF

dw_Batch_process.SetTransObject(sqlservertrans)
ll_Count=dw_Batch_process.Retrieve(ld_Cabdt)
//**the following check how many steps have been finished, step by step to finish in term
IF ll_Count=0 THEN
	cbx_Sched.enabled=FALSE
	cbx_Batbks.enabled=FALSE
	cbx_File.enabled=FALSE
	cbx_Email.enabled=FALSE
	dw_Batch_process.InsertRow(0)
ELSE
	ldt_Schedins=dw_Batch_process.object.schedins[1]   
	ldt_Webupd=dw_Batch_process.object.webupd[1]
	ldt_Marcexp=dw_Batch_process.object.marcexp[1]
	ldt_Marcemail=dw_Batch_process.object.marcemail[1]
	ldt_Batchimp=dw_Batch_process.object.batchimp[1]   
	ldt_Distfile=dw_Batch_process.object.distfile[1]
	ldt_Distsched=dw_Batch_process.object.distsched[1] 
END IF

IF IsNull(ldt_Schedins)=FALSE THEN
	cbx_Sched.enabled=TRUE
	cbx_Sched.checked=TRUE
	st_Sched.text=String(ldt_Schedins,'mm/dd/yyyy hh:mm:ss')
END IF
IF IsNull(ldt_Webupd)=FALSE THEN
	cbx_Batbks.enabled=TRUE
	cbx_Batbks.checked=TRUE
	st_Web.text=String(ldt_Webupd,'mm/dd/yyyy hh:mm:ss')
END IF
IF IsNull(ldt_Marcexp)=FALSE THEN
	cbx_File.enabled=TRUE
	cbx_File.checked=TRUE
	st_Marc.text=String(ldt_Marcexp,'mm/dd/yyyy hh:mm:ss')
END IF
IF IsNull(ldt_Marcemail)=FALSE THEN
	cbx_Email.enabled=TRUE
	cbx_Email.checked=TRUE
	st_Email.text=String(ldt_Marcemail,'mm/dd/yyyy hh:mm:ss')
END IF


// 03/27/2008 check with batch_data table 
// New validation 03/27/2008 for PICS 2.5
ll_rows = dw_batch_data.Retrieve(date(ld_dates)) // ?? 
cbx_apnd.visible=FALSE

IF cbx_Sched.checked=TRUE AND cbx_Batbks.checked=TRUE &
	AND cbx_File.checked=TRUE AND cbx_Email.checked=TRUE THEN
	IF IsNull(ldt_Batchimp)=FALSE AND IsNull(ldt_Distfile)=FALSE AND &
		IsNull(ldt_Distsched)=FALSE THEN
		Messagebox('Batch Formation','All processing for this batch has already been completed.'+&
			'~nPlease use a different copy allotment batch date/or append to the batch.')
	END IF
	cb_Update.enabled=FALSE
	cbx_apnd.visible=FALSE
END IF

IF ll_rows > 0 THEN
	ls_status = dw_batch_data.object.batch_status_code[ll_rows]
	IF(ls_status <> 'C' AND ls_status <> 'D') THEN
		// within cutoff date allow appending 
		ld_dt = date(dw_batch_formation.object.rtndt[1])
		IF today() <=  ld_dt THEN
			cbx_apnd.visible=TRUE
			cbx_apnd.enabled=TRUE // if Open allow to append
			cb_Update.enabled=TRUE
		END IF
	END IF
END IF


	
end event

type cb_clear from u_cb within w_batch_formation
event pfc_hinttext pbm_mousemove
string tag = "Clear the record"
integer x = 2117
integer y = 828
integer taborder = 0
integer textsize = -10
string text = "&Clear"
end type

event pfc_hinttext;call super::pfc_hinttext;w_pics_main.Setmicrohelp(THIS.Tag)
end event

event clicked;call super::clicked;dw_batch_formation.Reset()
dw_batch_formation.insertrow(0)
dw_batch_formation.setfocus()
cb_find.Enabled = TRUE
cb_clear.Enabled = FALSE
ib_disableclosequery = TRUE
cb_find.Default = TRUE
cb_update.Enabled = FALSE


// 03/27/2008 when cleared reset checkboxes
cbx_Sched.enabled=FALSE
cbx_Sched.checked=FALSE
cbx_Batbks.enabled=FALSE
cbx_Batbks.checked=FALSE
cbx_File.enabled=FALSE
cbx_File.checked=FALSE
cbx_Email.enabled=FALSE
cbx_Email.checked=FALSE
cbx_apnd.visible=FALSE

end event

type cb_print from u_cb within w_batch_formation
string tag = "Print~'s the report for distribution schedule based on cabdt"
integer x = 361
integer y = 828
integer taborder = 0
integer textsize = -10
string text = "&Print"
end type

event clicked;call super::clicked;	dw_distribution_schedule. Event pfc_retrieve()
	cb_find.Enabled =FALSE
	cb_print.Enabled= FALSE
	
	cb_update.Enabled =TRUE
	




end event

type dw_get_data_for_books from u_dw within w_batch_formation
event pfc_hinttext pbm_mousemove
boolean visible = false
integer y = 128
integer width = 110
integer height = 160
integer taborder = 10
string dataobject = "d_get_data_for_books"
boolean vscrollbar = false
boolean livescroll = false
end type

event pfc_hinttext;call super::pfc_hinttext;string ls_object, ls_column, ls_column_tag
long ll_pos

//This script set's microhelp at the bottom of the screen for the dw_batch_formation
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

event constructor;call super::constructor;THIS.settransobject(sqlservertrans)
ib_rmbmenu = FALSE


end event

event getfocus;call super::getfocus;m_pics_main.m_Edit.m_Addrow.Enabled = FALSE
m_pics_main.m_Edit.m_Deleterow.Enabled = FALSE
m_pics_main.m_Edit.m_Cut.Enabled = FALSE
end event

event retrieveend;call super::retrieveend;int li_loop 

For li_loop = 1 to rowcount
	i_count++
	if uo_progress.visible=true and mod(i_count,10)=0 then
		uo_progress.of_Increment(1)
	end if
	if mod(i_count,100)=0 then
		parent.SetRedraw(TRUE)
	else
		parent.SetRedraw(false)
	end if
next

end event

event retrievestart;call super::retrievestart;//Open(w_pics_retrieve_msg_box)
end event

event pfc_addrow;long ll_rc

ll_rc = Super::Event pfc_addrow()
dw_batch_formation.SetItem(ll_rc, "batch_cabdt",ld_date)
return ll_rc
end event

event pfc_retrieve;long ll_row_count , i
ll_row_count =THIS.Retrieve(ld_date)
return ll_row_count
end event

type dw_get_data_for_batch from u_dw within w_batch_formation
event pfc_hinttext pbm_mousemove
boolean visible = false
integer x = 23
integer width = 146
integer height = 96
integer taborder = 10
string dataobject = "d_get_data_for_batch"
boolean vscrollbar = false
boolean livescroll = false
end type

event pfc_hinttext;call super::pfc_hinttext;string ls_object, ls_column, ls_column_tag
long ll_pos

//This script set's microhelp at the bottom of the screen for the dw_batch_formation
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

event constructor;call super::constructor;THIS.settransobject(sqlservertrans)
ib_rmbmenu = FALSE


end event

event getfocus;call super::getfocus;m_pics_main.m_Edit.m_Addrow.Enabled = FALSE
m_pics_main.m_Edit.m_Deleterow.Enabled = FALSE
m_pics_main.m_Edit.m_Cut.Enabled = FALSE
end event

event retrieveend;call super::retrieveend;int li_loop 

For li_loop = 1 to rowcount
	i_count++
	if uo_progress.visible=true and mod(i_count,10)=0 then
		uo_progress.of_Increment(1)
	end if
	if mod(i_count,100)=0 then
		parent.SetRedraw(TRUE)
	else
		parent.SetRedraw(false)
	end if
next

end event

event retrievestart;call super::retrievestart;//Open(w_pics_retrieve_msg_box)
end event

event pfc_addrow;long ll_rc

ll_rc = Super::Event pfc_addrow()
dw_batch_formation.SetItem(ll_rc, "batch_cabdt",ld_date)
return ll_rc
end event

event pfc_retrieve;long ll_row_count , i
ll_row_count =THIS.Retrieve(ld_date)
return ll_row_count
end event

type dw_distribution_schedule from u_dw within w_batch_formation
boolean visible = false
integer x = 1248
integer y = 636
integer width = 0
integer height = 40
integer taborder = 20
boolean bringtotop = true
string dataobject = "d_report_select_deselect_for_s"
boolean vscrollbar = false
boolean border = false
boolean livescroll = false
borderstyle borderstyle = stylebox!
end type

event constructor;call super::constructor;This.Settransobject(sqlservertrans)
This.of_setupdateable(FALSE)
end event

event pfc_retrieve;call super::pfc_retrieve;datetime ld_select_date

ld_select_date = dw_batch_formation.object.batch_cabdt[1]
RETURN dw_distribution_schedule.Retrieve(ld_select_date)
//RETURN dw_distribution_schedule.Retrieve()
end event

event retrieveend;call super::retrieveend;dw_distribution_schedule.Triggerevent("pfc_print")
end event

type gb_1 from groupbox within w_batch_formation
integer x = 2007
integer y = 340
integer width = 1467
integer height = 464
integer taborder = 30
integer textsize = -8
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 33554432
long backcolor = 80263581
end type

type cbx_file from u_cbx within w_batch_formation
integer x = 2039
integer y = 628
integer width = 800
integer height = 68
boolean bringtotop = true
string text = "Create MARC File"
end type


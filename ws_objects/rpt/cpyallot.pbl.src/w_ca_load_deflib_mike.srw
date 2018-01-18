$PBExportHeader$w_ca_load_deflib_mike.srw
forward
global type w_ca_load_deflib_mike from w_main
end type
type st_7 from statictext within w_ca_load_deflib_mike
end type
type cb_update from commandbutton within w_ca_load_deflib_mike
end type
type st_6 from statictext within w_ca_load_deflib_mike
end type
type st_5 from statictext within w_ca_load_deflib_mike
end type
type st_3 from statictext within w_ca_load_deflib_mike
end type
type st_4 from statictext within w_ca_load_deflib_mike
end type
type cb_retrieve from commandbutton within w_ca_load_deflib_mike
end type
type cbx_libdef from checkbox within w_ca_load_deflib_mike
end type
type sle_rows_upd from singlelineedit within w_ca_load_deflib_mike
end type
type st_2 from statictext within w_ca_load_deflib_mike
end type
type sle_rows from singlelineedit within w_ca_load_deflib_mike
end type
type st_1 from statictext within w_ca_load_deflib_mike
end type
type cb_extract from commandbutton within w_ca_load_deflib_mike
end type
type uo_progress from u_progressbar within w_ca_load_deflib_mike
end type
type cb_oracle from commandbutton within w_ca_load_deflib_mike
end type
type cb_cancel from commandbutton within w_ca_load_deflib_mike
end type
type dw_sp9_deflib from u_pics_dw within w_ca_load_deflib_mike
end type
type dw_ca_def_casub from u_pics_dw within w_ca_load_deflib_mike
end type
end forward

global type w_ca_load_deflib_mike from w_main
integer width = 3323
integer height = 1852
string title = "Load and refresh selected quantity from web"
event ue_create_file ( )
event ue_cancel ( )
event ue_dist_file ( )
event ue_all ( )
event ue_single ( )
event ue_enterkey pbm_dwnprocessenter
event ue_sel_file ( )
event ue_bklist ( )
st_7 st_7
cb_update cb_update
st_6 st_6
st_5 st_5
st_3 st_3
st_4 st_4
cb_retrieve cb_retrieve
cbx_libdef cbx_libdef
sle_rows_upd sle_rows_upd
st_2 st_2
sle_rows sle_rows
st_1 st_1
cb_extract cb_extract
uo_progress uo_progress
cb_oracle cb_oracle
cb_cancel cb_cancel
dw_sp9_deflib dw_sp9_deflib
dw_ca_def_casub dw_ca_def_casub
end type
global w_ca_load_deflib_mike w_ca_load_deflib_mike

type variables
long rows_leftover,updated_rows

end variables

event ue_cancel;close(this)
end event

event ue_enterkey;call super::ue_enterkey;Send(Handle(this),256,9,Long(0,0))
return(1)
end event

on w_ca_load_deflib_mike.create
int iCurrent
call super::create
this.st_7=create st_7
this.cb_update=create cb_update
this.st_6=create st_6
this.st_5=create st_5
this.st_3=create st_3
this.st_4=create st_4
this.cb_retrieve=create cb_retrieve
this.cbx_libdef=create cbx_libdef
this.sle_rows_upd=create sle_rows_upd
this.st_2=create st_2
this.sle_rows=create sle_rows
this.st_1=create st_1
this.cb_extract=create cb_extract
this.uo_progress=create uo_progress
this.cb_oracle=create cb_oracle
this.cb_cancel=create cb_cancel
this.dw_sp9_deflib=create dw_sp9_deflib
this.dw_ca_def_casub=create dw_ca_def_casub
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_7
this.Control[iCurrent+2]=this.cb_update
this.Control[iCurrent+3]=this.st_6
this.Control[iCurrent+4]=this.st_5
this.Control[iCurrent+5]=this.st_3
this.Control[iCurrent+6]=this.st_4
this.Control[iCurrent+7]=this.cb_retrieve
this.Control[iCurrent+8]=this.cbx_libdef
this.Control[iCurrent+9]=this.sle_rows_upd
this.Control[iCurrent+10]=this.st_2
this.Control[iCurrent+11]=this.sle_rows
this.Control[iCurrent+12]=this.st_1
this.Control[iCurrent+13]=this.cb_extract
this.Control[iCurrent+14]=this.uo_progress
this.Control[iCurrent+15]=this.cb_oracle
this.Control[iCurrent+16]=this.cb_cancel
this.Control[iCurrent+17]=this.dw_sp9_deflib
this.Control[iCurrent+18]=this.dw_ca_def_casub
end on

on w_ca_load_deflib_mike.destroy
call super::destroy
destroy(this.st_7)
destroy(this.cb_update)
destroy(this.st_6)
destroy(this.st_5)
destroy(this.st_3)
destroy(this.st_4)
destroy(this.cb_retrieve)
destroy(this.cbx_libdef)
destroy(this.sle_rows_upd)
destroy(this.st_2)
destroy(this.sle_rows)
destroy(this.st_1)
destroy(this.cb_extract)
destroy(this.uo_progress)
destroy(this.cb_oracle)
destroy(this.cb_cancel)
destroy(this.dw_sp9_deflib)
destroy(this.dw_ca_def_casub)
end on

event open;call super::open;//open the sheet in maximized state
//this.windowstate = maximized!

ib_disableclosequery=TRUE

end event

type st_7 from statictext within w_ca_load_deflib_mike
integer x = 41
integer y = 1440
integer width = 873
integer height = 64
integer textsize = -8
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 33554432
long backcolor = 67108864
string text = "Press ~"Update~", To update DEF table."
boolean focusrectangle = false
end type

type cb_update from commandbutton within w_ca_load_deflib_mike
string tag = "Update date in DEF(RS20) table."
integer x = 2606
integer y = 1612
integer width = 379
integer height = 92
integer taborder = 40
integer textsize = -8
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "&Update (DEF)"
end type

event clicked;// If we have lost out connection to oracle, reconnect.
IF SQLServerOracleTrans.DBHandle() = 0 THEN
	
	openwithparm(w_pics_retrieve_msg_box,"Making the connection to Oracle, Please Wait...")
	
	SqlServerOracleTrans.DBMS = "O73 ORACLE 7.3"
	SqlServerOracleTrans.LogPass = "picadmin"
	SqlServerOracleTrans.ServerName = "@TNS:ORAPICS"
	SqlServerOracleTrans.LogId = "picadmin"
	SqlServerOracleTrans.AutoCommit = False
	SqlServerOracleTrans.DBParm = "PBCatalogOwner='PICADMIN'"
	SqlServerOracleTrans.of_connect()
	IF SqlServerOracleTrans.sqlcode <> 0 THEN
		close(w_pics_retrieve_msg_box)
		IF SqlServerOracleTrans.SqlDbcode = -951 THEN            //check for invalid userid
			MessageBox("Login Error","Invalid User ID/Password. Please re-enter.",StopSign!)
			Return -1	
		ELSEIF SqlServerOracleTrans.SqlDBcode = -952 THEN       //check for invalid password
			MessageBox("Login Error","Invalid User ID/Password. Please re-enter.",StopSign!)
			Return -1
		Else                                             //check for other error messages
			MessageBox("Database Connection Error","Unable to Connect. " +& 
			string(SqlServerOracleTrans.sqldbcode) + " " +&
			SqlServerOracleTrans.SQLErrText, &
			StopSign!)
			Return -1
		END IF
	ELSE
		close(w_pics_retrieve_msg_box)
	END IF
	
END IF
	
int rtn
	
rtn = MessageBox("Update Def table","This process will update default quantities ~"def~" table()." +&
							" Continue?",Question!,YesNo!,1)
IF rtn = 1	THEN
	String ls_msgtext[ ] = {" 0 %","10 %", "20 %", "30 %", "40 %", "50 %", "60 %", &
												"70 %", "80 %", "90 %", "100 %"}
	
	long ll_rows,lcnt,nullval,rows_updated=0
	string llibcd,lmed,lcasub
	integer ldef_n_def,ldef_p_def,ldef_r_def
	integer lsel_n_def,lsel_p_def,lsel_r_def
	date ldefdt,ltoday
	
	ltoday = today()
	
	SetNull(nullval)

	IF dw_ca_def_casub.Visible = TRUE THEN
		dw_ca_def_casub.Visible = FALSE
		dw_sp9_deflib.Visible = TRUE
	END IF

	dw_ca_def_casub.settransobject(sqlservertrans)
	//dw_ca_def_casub.Reset()

	ll_rows = dw_sp9_deflib.RowCount()
	
	IF  ll_rows <> 0 THEN
		
		IF dw_sp9_deflib.Visible=FALSE THEN
			dw_sp9_deflib.Visible=TRUE
			dw_ca_def_casub.Visible=FALSE
		END IF
		
		sle_rows.Text = string(ll_rows)
		
		string newsort
		newsort = "libcd A,med,casub"
		dw_sp9_deflib.SetSort(newsort)
		dw_sp9_deflib.Sort()

		uo_progress.Visible=TRUE
		uo_progress.of_SetMinimum(0)
		uo_progress.of_SetMaximum(ll_rows)
		uo_progress.of_SetDisplayStyle(3)
		uo_progress.of_SetMessageText(ls_msgtext)
		uo_progress.of_SetPosition(0)
		
		openwithparm(w_pics_retrieve_msg_box,"Synchronizing defqty(Normal, Priority and Rush). Please Wait...")
		
		
		FOR lcnt = 1 TO ll_rows
			
				llibcd 		= TRIM(dw_sp9_deflib.object.libcd[lcnt])
				lmed 			= TRIM(dw_sp9_deflib.object.med[lcnt])
				lcasub 		= TRIM(dw_sp9_deflib.object.casub[lcnt])
				lsel_n_def  = dw_sp9_deflib.object.sel_n_defqty[lcnt]
				IF NOT(IsNull(lsel_n_def)) THEN
					update def
					set defqty = :lsel_n_def,defdt = :ltoday
					where libcd = :llibcd
					and   med   = :lmed
					and   casub = :lcasub
					and   priority = 'N'
					using sqlservertrans;
					dw_sp9_deflib.object.curr_n_defqty[lcnt] 	=	dw_sp9_deflib.object.sel_n_defqty[lcnt]
					dw_sp9_deflib.object.sel_n_defqty[lcnt] 	=	nullval
					rows_updated++
				END IF
						
				lsel_p_def  = dw_sp9_deflib.object.sel_p_defqty[lcnt]
				IF NOT(IsNull(lsel_p_def)) THEN
					update def
					set defqty = :lsel_p_def,defdt = :ltoday
					where libcd = :llibcd
					and   med   = :lmed
					and   casub = :lcasub
					and   priority = 'P'
					using sqlservertrans;
					dw_sp9_deflib.object.curr_p_defqty[lcnt] 	=	dw_sp9_deflib.object.sel_p_defqty[lcnt]
					dw_sp9_deflib.object.sel_p_defqty[lcnt] 	=	nullval
					rows_updated++
				END IF
				
				lsel_r_def  = dw_sp9_deflib.object.sel_r_defqty[lcnt]
				IF NOT(IsNull(lsel_r_def)) THEN
					update def
					set defqty = :lsel_r_def,defdt = :ltoday
					where libcd = :llibcd
					and   med   = :lmed
					and   casub = :lcasub
					and   priority = 'R'
					using sqlservertrans;
					dw_sp9_deflib.object.curr_r_defqty[lcnt] 	=	dw_sp9_deflib.object.sel_r_defqty[lcnt]
					dw_sp9_deflib.object.sel_r_defqty[lcnt] 	=	nullval
					rows_updated++
				END IF
				
				IF NOT(IsNull(lsel_n_def)) OR &
					NOT(IsNull(lsel_p_def)) OR &
					NOT(IsNull(lsel_r_def)) THEN
					dw_sp9_deflib.object.sent_flag[lcnt] 		=  'Y'
				END IF					
				
				IF lcnt <= uo_progress.of_GetMaximum() THEN
					uo_progress.of_Increment(1)
					IF mod(lcnt,100)=0 THEN
						sle_rows_upd.Text = string(rows_updated)
						w_ca_load_deflib.SetRedraw(TRUE) 
					END IF
				END IF
		NEXT			
			
		close(w_pics_retrieve_msg_box)

		uo_progress.Visible=FALSE
			
		dw_sp9_deflib.settransobject(sqlserveroracletrans)
					
		rtn = dw_sp9_deflib.of_Update(TRUE,TRUE)
		IF f_check_dberror(sqlserveroracletrans,"LIBDEF at RS21") THEN
			IF rtn=1 THEN
				Commit Using sqlservertrans;
				Commit Using sqlserveroracletrans;
				MessageBox("Update",string(rows_updated)+" row(s) updated in DEF Table. ")
			ELSE
				MessageBox("Update"," Update libdef table failed.")
				Rollback Using sqlserveroracletrans;
				Rollback Using sqlservertrans;
				uo_progress.Visible=FALSE
				IF rows_updated > 0 THEN
					//dw_ca_def_casub.retrieve()
				END IF
				RETURN
			END IF
		END IF
	
		uo_progress.Visible=FALSE
	ELSE
		MessageBox("ERROR","No rows retrieved from libdef(WEB).")
	END IF		
END IF
end event

type st_6 from statictext within w_ca_load_deflib_mike
integer x = 41
integer y = 1384
integer width = 1513
integer height = 64
integer textsize = -8
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 33554432
long backcolor = 67108864
string text = "Press ~"Get data from Web~", To retrieve data from LIBDEF table."
boolean focusrectangle = false
end type

type st_5 from statictext within w_ca_load_deflib_mike
integer x = 41
integer y = 1328
integer width = 2327
integer height = 64
integer textsize = -8
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 33554432
long backcolor = 67108864
string text = "Press ~"Initial load into Web~", if this is the first time you are updating LIBDEF table with data from DEF table."
boolean focusrectangle = false
end type

type st_3 from statictext within w_ca_load_deflib_mike
integer x = 41
integer y = 1272
integer width = 1847
integer height = 64
integer textsize = -8
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 33554432
long backcolor = 67108864
string text = "Press ~"Retrieve~" button to retrieve data from DEF table and display it on the screen."
boolean focusrectangle = false
end type

type st_4 from statictext within w_ca_load_deflib_mike
integer x = 1042
integer y = 1540
integer width = 1600
integer height = 64
integer textsize = -8
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 33554432
long backcolor = 67108864
string text = "(Check this box, if you want to switch between DEF and LIBDEF screen.)"
boolean focusrectangle = false
end type

type cb_retrieve from commandbutton within w_ca_load_deflib_mike
string tag = "Retrieve data from DEF(RS20) table."
integer x = 727
integer y = 1612
integer width = 379
integer height = 92
integer taborder = 40
integer textsize = -8
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "Retrie&ve (DEF)"
end type

event clicked;IF dw_ca_def_casub.Visible = FALSE THEN
	dw_ca_def_casub.Visible = TRUE
	dw_sp9_deflib.Visible = FALSE
END IF

dw_ca_def_casub.retrieve()
end event

type cbx_libdef from checkbox within w_ca_load_deflib_mike
integer x = 960
integer y = 1532
integer width = 96
integer height = 80
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 33554432
long backcolor = 67108864
end type

event clicked;IF This.Checked THEN
	IF dw_sp9_deflib.Visible=FALSE THEN
		dw_sp9_deflib.Visible=TRUE
		dw_ca_def_casub.Visible=FALSE
	END IF
ELSE
	IF dw_ca_def_casub.Visible=FALSE THEN
		dw_sp9_deflib.Visible=FALSE
		dw_ca_def_casub.Visible=TRUE
	END IF
END IF	

end event

type sle_rows_upd from singlelineedit within w_ca_load_deflib_mike
integer x = 375
integer y = 1616
integer width = 302
integer height = 80
integer taborder = 30
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 33554432
borderstyle borderstyle = stylelowered!
end type

type st_2 from statictext within w_ca_load_deflib_mike
integer x = 41
integer y = 1616
integer width = 338
integer height = 64
integer textsize = -8
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 33554432
long backcolor = 67108864
string text = "Rows Updated"
boolean focusrectangle = false
end type

type sle_rows from singlelineedit within w_ca_load_deflib_mike
integer x = 375
integer y = 1536
integer width = 302
integer height = 80
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 33554432
borderstyle borderstyle = stylelowered!
end type

type st_1 from statictext within w_ca_load_deflib_mike
integer x = 41
integer y = 1536
integer width = 347
integer height = 64
integer textsize = -8
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 33554432
long backcolor = 67108864
string text = "Rows Counted"
boolean focusrectangle = false
end type

type cb_extract from commandbutton within w_ca_load_deflib_mike
string tag = "Press this key if you want to get data from LIBDEF(RS21) table."
integer x = 1874
integer y = 1612
integer width = 695
integer height = 92
integer textsize = -8
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "&Get data from Web (LIBDEF)"
end type

event clicked;// If we have lost out connection to oracle, reconnect.
IF SQLServerOracleTrans.DBHandle() = 0 THEN
	
	openwithparm(w_pics_retrieve_msg_box,"Making the connection to Oracle, Please Wait...")
	
	SqlServerOracleTrans.DBMS = "O73 ORACLE 7.3"
	SqlServerOracleTrans.LogPass = "picadmin"
	SqlServerOracleTrans.ServerName = "@TNS:ORAPICS"
	SqlServerOracleTrans.LogId = "picadmin"
	SqlServerOracleTrans.AutoCommit = False
	SqlServerOracleTrans.DBParm = "PBCatalogOwner='PICADMIN'"
	SqlServerOracleTrans.of_connect()
	IF SqlServerOracleTrans.sqlcode <> 0 THEN
		close(w_pics_retrieve_msg_box)
		IF SqlServerOracleTrans.SqlDbcode = -951 THEN            //check for invalid userid
			MessageBox("Login Error","Invalid User ID/Password. Please re-enter.",StopSign!)
			Return -1	
		ELSEIF SqlServerOracleTrans.SqlDBcode = -952 THEN       //check for invalid password
			MessageBox("Login Error","Invalid User ID/Password. Please re-enter.",StopSign!)
			Return -1
		Else                                             //check for other error messages
			MessageBox("Database Connection Error","Unable to Connect. " +& 
			string(SqlServerOracleTrans.sqldbcode) + " " +&
			SqlServerOracleTrans.SQLErrText, &
			StopSign!)
			Return -1
		END IF
	ELSE
		close(w_pics_retrieve_msg_box)
	END IF
	
END IF
	
int rtn
	
rtn = MessageBox("Get default quantity","This process will get records that are changed from default quantities ~"libdef~" table(WEB)." +&
							" Continue?",Question!,YesNo!,1)
IF rtn = 1	THEN
	
	long ll_rows
	
	
	// Reset the datawindows
	dw_sp9_deflib.settransobject(sqlserveroracletrans)
	dw_sp9_deflib.Reset()
	
	IF dw_ca_def_casub.Visible = TRUE THEN
		dw_ca_def_casub.Visible = FALSE
		dw_sp9_deflib.Visible = TRUE
	END IF
				
	// retrieve the data from libdef table at rs20
	ll_rows = dw_sp9_deflib.Retrieve()
	
	IF ll_rows = 0 THEN		
		MessageBox("ERROR","No rows retrieved from libdef(WEB).")
	END IF		
END IF
end event

type uo_progress from u_progressbar within w_ca_load_deflib_mike
boolean visible = false
integer x = 32
integer y = 1172
integer width = 3223
integer height = 92
boolean border = true
borderstyle borderstyle = stylelowered!
end type

on uo_progress.destroy
call u_progressbar::destroy
end on

type cb_oracle from commandbutton within w_ca_load_deflib_mike
string tag = "Press this key if you want to load data for the first into libdef(RS21) table."
integer x = 1134
integer y = 1612
integer width = 709
integer height = 92
integer textsize = -8
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "&Initial load into Web (LIBDEF)"
end type

event clicked;// If we have lost out connection to oracle, reconnect.
string lpriority,llibcd,lmed,lcasub,lcasub_desc
integer ldef_n_qty,ldef_p_qty,ldef_r_qty, j=1
date ldefdt,null_date
datetime ldt_defdt

SetNull(null_date)


IF SQLServerOracleTrans.DBHandle() = 0 THEN
	openwithparm(w_pics_retrieve_msg_box,"Making the connection to Oracle, Please Wait...")
	
	SqlServerOracleTrans.DBMS = "O73 ORACLE 7.3"
	SqlServerOracleTrans.LogPass = "picadmin"
	SqlServerOracleTrans.ServerName = "@TNS:ORAPICS"
	SqlServerOracleTrans.LogId = "picadmin"
	SqlServerOracleTrans.AutoCommit = False
	SqlServerOracleTrans.DBParm = "PBCatalogOwner='PICADMIN'"
	SqlServerOracleTrans.of_connect()
	IF SqlServerOracleTrans.sqlcode <> 0 THEN
		close(w_pics_retrieve_msg_box)
		IF SqlServerOracleTrans.SqlDbcode = -951 THEN            //check for invalid userid
			MessageBox("Login Error","Invalid User ID/Password. Please re-enter.",StopSign!)
			Return -1	
		ELSEIF SqlServerOracleTrans.SqlDBcode = -952 THEN       //check for invalid password
			MessageBox("Login Error","Invalid User ID/Password. Please re-enter.",StopSign!)
			Return -1
		Else                                             //check for other error messages
			MessageBox("Database Connection Error","Unable to Connect. " +& 
			string(SqlServerOracleTrans.sqldbcode) + " " +&
			SqlServerOracleTrans.SQLErrText, &
			StopSign!)
			Return -1
		END IF
	ELSE
		close(w_pics_retrieve_msg_box)
	END IF
END IF

int rtn
long ll_rows=0,lcnt,ll_rows_reminder=0

dw_sp9_deflib.settransobject(sqlserveroracletrans)
ll_rows = dw_sp9_deflib.Retrieve()

IF ll_rows > 0 THEN
	MessageBox("ERROR","Data exist in libdef table (WEB).~r~nIf this is the initial load into WEB, please delete the records from LIBDEF table before performing this procedure.")
	RETURN 1
ELSE
	rtn = MessageBox("Data Migration","This process will migrate selected records from ~"def~" table(PICS) into ~"libdef~" table(WEB)." +&
								" This process may take some time. Continue?",Question!,YesNo!,1)
	IF rtn = 1	THEN
		String ls_msgtext[ ] = {" 0 %","10 %", "20 %", "30 %", "40 %", "50 %", "60 %", &
												"70 %", "80 %", "90 %", "100 %"}
		Integer li_count
				
		ll_rows = dw_ca_def_casub.RowCount()
		IF ll_rows=0 THEN
			MessageBox("ERROR","Please retrieve data for DEF(RS20) table prior to loading it into LIBDEF(RS21).")
			RETURN 1
		ELSE
			sle_rows.Text = string(ll_rows)
		END IF
			
		uo_progress.Visible=TRUE
		uo_progress.of_SetMinimum(0)
		IF ll_rows > 32765 THEN
			ll_rows_reminder = ll_rows - 32765
			uo_progress.of_SetMaximum(32765)
		ELSE
			uo_progress.of_SetMaximum(ll_rows)
		END IF			
		uo_progress.of_SetDisplayStyle(3)
		uo_progress.of_SetMessageText(ls_msgtext)
		uo_progress.of_SetPosition(0)
		
		IF ll_rows_reminder > 0 THEN	
			openwithparm(w_pics_retrieve_msg_box,"Loading the first 32,765 of data into libdef, Please Wait...")

			FOR lcnt = 1 TO 32763
				
				IF MOD(lcnt,3) <> 0 THEN
					llibcd 		= dw_ca_def_casub.object.libcd[lcnt]
					lmed 			= dw_ca_def_casub.object.med[lcnt]
					lcasub 		= dw_ca_def_casub.object.casub[lcnt]
					lcasub_desc = dw_ca_def_casub.object.casub_desc[lcnt]
					ldefdt 		= dw_ca_def_casub.object.defdt[lcnt]
					IF ldefdt = null_date THEN
						ldt_defdt = DateTime(today(),now())
					ELSE
						ldt_defdt = DateTime(ldefdt,now())
					END IF					 
					lpriority 	= dw_ca_def_casub.object.priority[lcnt]

					CHOOSE CASE lpriority
					CASE 'N'
						ldef_n_qty = dw_ca_def_casub.object.defqty[lcnt]
					CASE 'P'
						ldef_p_qty = dw_ca_def_casub.object.defqty[lcnt]
					END CHOOSE
				ELSE
					// Rush qty is always the third one: N-P-R
					ldef_r_qty = dw_ca_def_casub.object.defqty[lcnt]
					
					dw_sp9_deflib.InsertRow(0)
					dw_sp9_deflib.object.libcd[j] 		= llibcd
					dw_sp9_deflib.object.med[j] 			= lmed
					dw_sp9_deflib.object.casub[j] 		= lcasub
					dw_sp9_deflib.object.casub_desc[j] 		= lcasub_desc
					dw_sp9_deflib.object.curr_p_defqty[j] = ldef_p_qty
					dw_sp9_deflib.object.curr_r_defqty[j] = ldef_r_qty
					dw_sp9_deflib.object.curr_n_defqty[j] = ldef_n_qty
					dw_sp9_deflib.object.defdt[j] 		= ldt_defdt
					
					j += 1
				END IF
		
				IF lcnt <= 32763 THEN
					uo_progress.of_Increment(1)
					IF mod(lcnt,100)=0 THEN
						w_ca_load_deflib.SetRedraw(TRUE) 
					END IF
				END IF
		
			NEXT
			uo_progress.of_SetMinimum(0)
			uo_progress.of_SetMaximum(ll_rows_reminder)
			uo_progress.of_SetDisplayStyle(3)
			uo_progress.of_SetMessageText(ls_msgtext)
			uo_progress.of_SetPosition(0)
			// Insert the rest of the rows
			close(w_pics_retrieve_msg_box)
			openwithparm(w_pics_retrieve_msg_box,"Loading the remainder of data into libdef, Please Wait...")
			
			FOR lcnt = 32764 TO ll_rows			
				
				IF MOD(lcnt,3) <> 0 THEN
					llibcd 		= dw_ca_def_casub.object.libcd[lcnt]
					lmed 			= dw_ca_def_casub.object.med[lcnt]
					lcasub 		= dw_ca_def_casub.object.casub[lcnt]
					lcasub_desc = dw_ca_def_casub.object.casub_desc[lcnt]
					ldefdt 		= dw_ca_def_casub.object.defdt[lcnt]
					IF ldefdt = null_date THEN
						ldt_defdt = DateTime(today(),now())
					ELSE
						ldt_defdt = DateTime(ldefdt,now())
					END IF					 
					lpriority 	= dw_ca_def_casub.object.priority[lcnt]
						
					CHOOSE CASE lpriority
					CASE 'P'
						ldef_p_qty = dw_ca_def_casub.object.defqty[lcnt]
					CASE 'N'
						ldef_n_qty = dw_ca_def_casub.object.defqty[lcnt]
					END CHOOSE
				ELSE
					ldef_r_qty = dw_ca_def_casub.object.defqty[lcnt]
					dw_sp9_deflib.InsertRow(0)
					dw_sp9_deflib.object.libcd[j] 		= llibcd
					dw_sp9_deflib.object.med[j] 			= lmed
					dw_sp9_deflib.object.casub[j] 		= lcasub
					dw_sp9_deflib.object.casub_desc[j] 		= lcasub_desc
					dw_sp9_deflib.object.curr_p_defqty[j] = ldef_p_qty
					dw_sp9_deflib.object.curr_r_defqty[j] = ldef_r_qty
					dw_sp9_deflib.object.curr_n_defqty[j] = ldef_n_qty
					dw_sp9_deflib.object.defdt[j] 		= ldt_defdt
					
					j += 1
					
				END IF
				
				IF lcnt <= ll_rows THEN
					uo_progress.of_Increment(1)
					IF mod(lcnt,100)=0 THEN
						w_ca_load_deflib.SetRedraw(TRUE) 
					END IF
				END IF
			NEXT
			
			close(w_pics_retrieve_msg_box)
			
		ELSE
			openwithparm(w_pics_retrieve_msg_box,"Loading data into libdef, Please Wait...")
			FOR lcnt = 1 TO ll_rows	
				
				IF MOD(lcnt,3) <> 0 THEN
					llibcd 		= dw_ca_def_casub.object.libcd[lcnt]
					lmed 			= dw_ca_def_casub.object.med[lcnt]
					lcasub 		= dw_ca_def_casub.object.casub[lcnt]
					lcasub_desc 		= dw_ca_def_casub.object.casub_desc[lcnt]
					ldefdt 		= dw_ca_def_casub.object.defdt[lcnt]
					IF ldefdt = null_date THEN
						ldt_defdt = DateTime(today(),now())
					ELSE
						ldt_defdt = DateTime(ldefdt,now())
					END IF					 
					lpriority 	= dw_ca_def_casub.object.priority[lcnt]
						
					CHOOSE CASE lpriority
					CASE 'P'
						ldef_p_qty = dw_ca_def_casub.object.defqty[lcnt]
					CASE 'N'
						ldef_n_qty = dw_ca_def_casub.object.defqty[lcnt]
					END CHOOSE
				ELSE
					ldef_r_qty = dw_ca_def_casub.object.defqty[lcnt]
					dw_sp9_deflib.InsertRow(0)
					dw_sp9_deflib.object.libcd[j] 		= llibcd
					dw_sp9_deflib.object.med[j] 			= lmed
					dw_sp9_deflib.object.casub[j] 		= lcasub
					dw_sp9_deflib.object.casub_desc[j] 		= lcasub_desc
					dw_sp9_deflib.object.curr_p_defqty[j] = ldef_p_qty
					dw_sp9_deflib.object.curr_r_defqty[j] = ldef_r_qty
					dw_sp9_deflib.object.curr_n_defqty[j] = ldef_n_qty
					dw_sp9_deflib.object.defdt[j] 		= ldt_defdt
					
					j += 1
					
				END IF
				
				IF lcnt <= ll_rows THEN
					uo_progress.of_Increment(1)
					IF mod(lcnt,100)=0 THEN
						w_ca_load_deflib.SetRedraw(TRUE) 
					END IF
				END IF			
		
			NEXT		
			close(w_pics_retrieve_msg_box)
		END IF			
			
//MessageBox("ROWs",string(dw_sp9_deflib.RowCount()))

		ll_rows = dw_sp9_deflib.RowCount()
		sle_rows.Text = string(ll_rows)
		uo_progress.of_SetMinimum(0)
		uo_progress.of_SetMaximum(ll_rows)
		uo_progress.of_SetDisplayStyle(3)
		uo_progress.of_SetMessageText(ls_msgtext)
		uo_progress.of_SetPosition(0)
		
		IF dw_sp9_deflib.Visible=FALSE THEN
			dw_sp9_deflib.Visible=TRUE
			dw_ca_def_casub.Visible=FALSE
		END IF
					
		rtn = dw_sp9_deflib.of_Update(TRUE,TRUE)
		IF f_check_dberror(sqlserveroracletrans,"LIBDEF (WEB)") THEN
			IF rtn=1 THEN
				Commit Using sqlserveroracletrans;
				MessageBox("Update",string(ll_rows)+ " rows inserted into libdef table. ")
			ELSE
				MessageBox("Update","Updating libdef on WEB has failed.")
				Rollback Using sqlserveroracletrans;
				RETURN
			END IF
		END IF
		uo_progress.Visible=FALSE
	END IF
END IF
end event

type cb_cancel from commandbutton within w_ca_load_deflib_mike
integer x = 3026
integer y = 1612
integer width = 233
integer height = 92
boolean bringtotop = true
integer textsize = -8
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "&Cancel"
boolean cancel = true
end type

event clicked;IF NOT SQLServerOracleTrans.DBHandle() =0 THEN
	IF SQLServerOracleTrans.of_disconnect() < 0 THEN
		MessageBox("Error","Oracle Database Disconnect Error.",StopSign!)
	END IF
END IF
ib_disableclosequery=TRUE
Parent.triggerevent("ue_cancel")
end event

type dw_sp9_deflib from u_pics_dw within w_ca_load_deflib_mike
integer x = 37
integer y = 28
integer width = 3214
integer height = 1140
integer taborder = 0
string dataobject = "d_sp9_deflib"
end type

event constructor;call super::constructor;this.of_SetFilter(TRUE)
this.of_SetFind(TRUE)
this.inv_filter.of_SetStyle(1)
this.inv_filter.of_SetColumnNameSource(2)
this.of_SetSort(TRUE)
this.inv_sort.of_SetStyle(1)
this.inv_sort.of_SetColumnNameSource(2)
This.of_SetTransObject(sqlserveroracletrans)

end event

event updatestart;call super::updatestart;openwithparm(w_pics_retrieve_msg_box,"Updating deflib table in RS21, Please wait...")

end event

event updateend;call super::updateend;close(w_pics_retrieve_msg_box)

end event

event sqlpreview;call super::sqlpreview;updated_rows = updated_rows + 1
IF updated_rows < 32675 THEN
	uo_progress.of_Increment(1)
	IF mod(updated_rows,100)=0 THEN
		sle_rows_upd.text = string(updated_rows)
		w_ca_load_deflib.SetRedraw(TRUE) 
	END IF
ELSEIF updated_rows = 32675 THEN
	uo_progress.of_SetMinimum(0)
	uo_progress.of_SetMaximum(rows_leftover)
	uo_progress.of_SetDisplayStyle(3)
	uo_progress.of_SetPosition(0)	
ELSEIF updated_rows > 32675 THEN
	uo_progress.of_Increment(1)
	IF mod(updated_rows,100)=0 THEN
		w_ca_load_deflib.SetRedraw(TRUE) 
		sle_rows_upd.text = string(updated_rows)
	END IF	
END IF

end event

event retrievestart;call super::retrievestart;openwithparm(w_pics_retrieve_msg_box,"Retrieving data from WEB, Please Wait...")

end event

event retrieveend;call super::retrieveend;close(w_pics_retrieve_msg_box)
sle_rows.Text = string(rowcount)
end event

event dberror;call super::dberror;//MessageBox("error",sqlsyntax+": "+sqlerrtext)
end event

type dw_ca_def_casub from u_pics_dw within w_ca_load_deflib_mike
integer x = 37
integer y = 24
integer width = 3209
integer height = 1148
integer taborder = 10
string dataobject = "d_ca_def_casub"
end type

event constructor;call super::constructor;this.of_SetFilter(TRUE)
this.of_SetFind(TRUE)
this.inv_filter.of_SetStyle(1)
this.inv_filter.of_SetColumnNameSource(2)
this.of_SetSort(TRUE)
this.inv_sort.of_SetStyle(1)
this.inv_sort.of_SetColumnNameSource(2)
This.of_SetTransObject(sqlservertrans)

end event

event retrievestart;call super::retrievestart;openwithparm(w_pics_retrieve_msg_box,"Retrieving data from PICS, Please Wait...")

end event

event retrieveend;call super::retrieveend;close(w_pics_retrieve_msg_box)
sle_rows.Text = string(rowcount)

end event

event ue_postconstructor;call super::ue_postconstructor;// Initialy set the query mode to yes.
dw_sp9_deflib.Visible=FALSE


end event

event sqlpreview;call super::sqlpreview;//	MessageBox("sql",sqlsyntax)
	updated_rows = updated_rows + 1
	IF updated_rows < 32675 THEN
		uo_progress.of_Increment(1)
		IF mod(updated_rows,100)=0 THEN
			sle_rows_upd.text = string(updated_rows)
			w_ca_load_deflib.SetRedraw(TRUE) 
		END IF
	ELSEIF updated_rows = 32675 THEN
		uo_progress.of_SetMinimum(0)
		uo_progress.of_SetMaximum(rows_leftover)
		uo_progress.of_SetDisplayStyle(3)
		uo_progress.of_SetPosition(0)	
	ELSEIF updated_rows > 32675 THEN
		uo_progress.of_Increment(1)
		IF mod(updated_rows,100)=0 THEN
			w_ca_load_deflib.SetRedraw(TRUE) 
			sle_rows_upd.text = string(updated_rows)
		END IF	
	END IF


end event

event updatestart;call super::updatestart;openwithparm(w_pics_retrieve_msg_box,"Updating def table in PICS, Please Wait...")

end event

event updateend;call super::updateend;close(w_pics_retrieve_msg_box)

end event


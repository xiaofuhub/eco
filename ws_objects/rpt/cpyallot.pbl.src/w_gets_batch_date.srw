$PBExportHeader$w_gets_batch_date.srw
forward
global type w_gets_batch_date from w_response
end type
type dw_batch from u_pics_dw within w_gets_batch_date
end type
type dw_for_dddw_batch_date from u_pics_dw within w_gets_batch_date
end type
type cb_ok from u_cb within w_gets_batch_date
end type
type cb_cancel from u_cb within w_gets_batch_date
end type
type st_1 from u_st within w_gets_batch_date
end type
end forward

global type w_gets_batch_date from w_response
integer x = 878
integer y = 640
integer width = 974
integer height = 620
string title = "Import Copy Allotment Batch"
dw_batch dw_batch
dw_for_dddw_batch_date dw_for_dddw_batch_date
cb_ok cb_ok
cb_cancel cb_cancel
st_1 st_1
end type
global w_gets_batch_date w_gets_batch_date

on w_gets_batch_date.create
int iCurrent
call super::create
this.dw_batch=create dw_batch
this.dw_for_dddw_batch_date=create dw_for_dddw_batch_date
this.cb_ok=create cb_ok
this.cb_cancel=create cb_cancel
this.st_1=create st_1
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_batch
this.Control[iCurrent+2]=this.dw_for_dddw_batch_date
this.Control[iCurrent+3]=this.cb_ok
this.Control[iCurrent+4]=this.cb_cancel
this.Control[iCurrent+5]=this.st_1
end on

on w_gets_batch_date.destroy
call super::destroy
destroy(this.dw_batch)
destroy(this.dw_for_dddw_batch_date)
destroy(this.cb_ok)
destroy(this.cb_cancel)
destroy(this.st_1)
end on

event key;call super::key;IF KeyDown(KeyEnter!) THEN 
  cb_ok.TriggerEvent(Clicked!)
ELSEIF KeyDown(KeyEscape!) THEN 
  cb_cancel.TriggerEvent(Clicked!)
END IF


end event

event open;call super::open;date ld_today
datetime ld_today_dt, ldt
datawindowchild dwc_date
long li_row_count, li_handle, li_re
string ls_oracle_ext_env

this.of_SetBase(true)
this.inv_base.of_Center()

//If we have lost our connection to oracle, reconnect.
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

ld_today=today()
ld_today_dt=datetime(ld_today,time('00:00:00'))
dw_for_dddw_batch_date.SetTransObject(SqlServerOracleTrans)
dw_for_dddw_batch_date.GetChild('cabdt',dwc_date)
dwc_date.SetTransObject(SqlServerOracleTrans)
dwc_date.Retrieve(ld_today_dt)
ldt=dwc_date.getitemDatetime(1,'cabdt')
dw_for_dddw_batch_date.Retrieve()
li_row_count=dw_for_dddw_batch_date.RowCount()
IF li_row_count = 0 THEN
		MessageBox("ERROR","No copy allotment batch date is found.")
		if isvalid(w_gets_batch_date) then
			str_distrib_schedule lstr
			lstr.arraymed[1]='CANCEL'
			closewithreturn(w_gets_batch_date, lstr)
		end if
else
		dw_for_dddw_batch_date.object.cabdt[1]=ldt
end if
		
	

end event

type dw_batch from u_pics_dw within w_gets_batch_date
boolean visible = false
integer x = 869
integer y = 380
integer width = 73
integer height = 92
integer taborder = 20
string dataobject = "d_batch_data"
boolean vscrollbar = false
boolean border = false
end type

event constructor;call super::constructor;
this.SettransObject(SqlServertrans)
end event

type dw_for_dddw_batch_date from u_pics_dw within w_gets_batch_date
integer x = 462
integer y = 124
integer width = 398
integer height = 92
integer taborder = 10
string dataobject = "d_for_dddw_batch_date"
boolean vscrollbar = false
boolean border = false
end type

type cb_ok from u_cb within w_gets_batch_date
integer x = 96
integer y = 372
integer width = 306
integer height = 72
integer taborder = 20
integer textsize = -10
string text = "&OK"
end type

event clicked;call super::clicked;//////////////////////////////////////////////////////////////////////////////////////////////////////////
//
//	Event: clicked for cb_OK
//
//	Description:
//	Validate closed batch situation
//
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//	
//	Revision History
//
//	Developed by 	Date 				Version						Tracking#
//									
// Murali K.			04/02/2008      PICS 2.5 Modifications	 Reqs: CA.9
//////////////////////////////////////////////////////////////////////////////////////////////////////////


date ld_date
datetime ldt_dt
long li_row, ll_rc
str_distrib_schedule lstr

ib_disableclosequery=true
li_row=dw_for_dddw_batch_date.Rowcount()
if li_row<=0 then
	messagebox('Error','Please select a valid copy allotment batch date .',StopSign!)
	return
end if
ldt_dt=dw_for_dddw_batch_date.GetItemDateTime(1,'cabdt')
ld_date=date(ldt_dt)
if IsNull(ld_date) then
	messagebox('Error','Please select a valid copy allotment batch date .',StopSign!)
	return
end if


lstr.ld_date=ld_date
// 04/02/2008 validate if the batch is closed , do not allow distribution schedule
//ll_rc = dw_batch.Retrieve(ld_date)
//IF ll_rc > 0 THEN
//	IF dw_batch.object.batch_status_code[ll_rc] <> 'C' THEN
//		Messagebox('Error', 'Batch not closed, cannot proceed with distribution schedule')
//		lstr.arraymed[1]='CANCEL'		
//	ELSE
//		lstr.arraymed[1]='OK'		
//	END IF
//ELSE
//	lstr.arraymed[1]='OK'		
//END IF
////////////////////

lstr.arraymed[1]='OK'		
closewithreturn(w_gets_batch_date, lstr)



end event

type cb_cancel from u_cb within w_gets_batch_date
integer x = 544
integer y = 372
integer width = 306
integer height = 72
integer taborder = 30
integer textsize = -10
string text = "&Cancel"
end type

event clicked;call super::clicked;ib_disableclosequery = TRUE

if SqlServerOracleTrans.dbhandle() >0 then
	SqlServerOracleTrans.of_disconnect()
end if
//SqlServerOracleTrans.of_disconnect()
IF SqlServerOracleTrans.sqlcode <> 0 THEN                          
	//check for other error messages
	MessageBox("Database Connection Error","Unable to Disconnect. " +& 
	string(SqlServerOracleTrans.sqldbcode) + " " +&
	SqlServerOracleTrans.SQLErrText, &
	StopSign!)
	return
end if
str_distrib_schedule lstr
lstr.arraymed[1]='CANCEL'
closewithreturn(w_gets_batch_date, lstr)
//close(w_pcs_reports)
//
end event

type st_1 from u_st within w_gets_batch_date
integer x = 82
integer y = 132
integer width = 338
integer height = 68
integer textsize = -10
string text = "Batch Date:"
end type


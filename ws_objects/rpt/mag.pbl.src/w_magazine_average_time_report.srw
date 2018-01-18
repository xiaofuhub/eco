$PBExportHeader$w_magazine_average_time_report.srw
forward
global type w_magazine_average_time_report from w_sheet
end type
type em_fy from u_em within w_magazine_average_time_report
end type
type st_1 from u_st within w_magazine_average_time_report
end type
type sle_contract from u_sle within w_magazine_average_time_report
end type
type st_2 from u_st within w_magazine_average_time_report
end type
type sle_producer from u_sle within w_magazine_average_time_report
end type
type st_3 from u_st within w_magazine_average_time_report
end type
type cb_print from u_cb within w_magazine_average_time_report
end type
type cb_ok from u_cb within w_magazine_average_time_report
end type
type cb_exit from u_cb within w_magazine_average_time_report
end type
type st_4 from u_st within w_magazine_average_time_report
end type
type st_message from u_st within w_magazine_average_time_report
end type
end forward

global type w_magazine_average_time_report from w_sheet
integer width = 1728
integer height = 1052
string title = "Average Time Start To Ship"
em_fy em_fy
st_1 st_1
sle_contract sle_contract
st_2 st_2
sle_producer sle_producer
st_3 st_3
cb_print cb_print
cb_ok cb_ok
cb_exit cb_exit
st_4 st_4
st_message st_message
end type
global w_magazine_average_time_report w_magazine_average_time_report

on w_magazine_average_time_report.create
int iCurrent
call super::create
this.em_fy=create em_fy
this.st_1=create st_1
this.sle_contract=create sle_contract
this.st_2=create st_2
this.sle_producer=create sle_producer
this.st_3=create st_3
this.cb_print=create cb_print
this.cb_ok=create cb_ok
this.cb_exit=create cb_exit
this.st_4=create st_4
this.st_message=create st_message
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.em_fy
this.Control[iCurrent+2]=this.st_1
this.Control[iCurrent+3]=this.sle_contract
this.Control[iCurrent+4]=this.st_2
this.Control[iCurrent+5]=this.sle_producer
this.Control[iCurrent+6]=this.st_3
this.Control[iCurrent+7]=this.cb_print
this.Control[iCurrent+8]=this.cb_ok
this.Control[iCurrent+9]=this.cb_exit
this.Control[iCurrent+10]=this.st_4
this.Control[iCurrent+11]=this.st_message
end on

on w_magazine_average_time_report.destroy
call super::destroy
destroy(this.em_fy)
destroy(this.st_1)
destroy(this.sle_contract)
destroy(this.st_2)
destroy(this.sle_producer)
destroy(this.st_3)
destroy(this.cb_print)
destroy(this.cb_ok)
destroy(this.cb_exit)
destroy(this.st_4)
destroy(this.st_message)
end on

event open;call super::open;
//Disable the OK and Print buttons
cb_ok.Enabled = FALSE
cb_print.Enabled = FALSE
end event

type em_fy from u_em within w_magazine_average_time_report
integer x = 462
integer y = 240
integer width = 274
integer height = 84
integer taborder = 10
integer textsize = -10
alignment alignment = center!
maskdatatype maskdatatype = datemask!
string mask = "yyyy"
boolean autoskip = true
string displaydata = "Ä"
double increment = 0
string minmax = ""
end type

event modified;call super::modified;//Enable the OK button
cb_ok.Enabled = TRUE
end event

type st_1 from u_st within w_magazine_average_time_report
integer x = 91
integer y = 248
integer width = 343
integer textsize = -10
string text = "Fiscal Year"
alignment alignment = right!
end type

type sle_contract from u_sle within w_magazine_average_time_report
integer x = 462
integer y = 352
integer height = 84
integer taborder = 20
integer textsize = -10
textcase textcase = upper!
end type

type st_2 from u_st within w_magazine_average_time_report
integer x = 91
integer y = 360
integer width = 343
integer height = 68
integer textsize = -10
string text = "Contract"
alignment alignment = right!
end type

type sle_producer from u_sle within w_magazine_average_time_report
integer x = 462
integer y = 456
integer width = 311
integer height = 84
integer taborder = 30
textcase textcase = upper!
end type

type st_3 from u_st within w_magazine_average_time_report
integer x = 91
integer y = 460
integer width = 343
integer textsize = -10
string text = "Producer"
alignment alignment = right!
end type

type cb_print from u_cb within w_magazine_average_time_report
integer x = 617
integer y = 620
integer taborder = 0
integer textsize = -10
string text = "&Print"
end type

event clicked;call super::clicked;String ls_filename, ls_userid, lp_name, prnt_cmd

open(w_magazine_printer_list)
lp_name = Message.StringParm

IF lp_name <> "" THEN
	ls_userid = SqlServerTrans.userid
	
	ls_filename = 'average.'+ls_userid
	
	//Declare the procedure
	DECLARE printit PROCEDURE FOR
	 printproc :lp_name, :ls_userid
	USING SqlServerTrans;	
	
	st_message.text = 'Printing Report Please Wait ...'
	
	//EXECUTE THE PROCEDURE
	EXECUTE printit;
  	IF f_check_dberror(sqlservertrans,"printing magazine average report") THEN 
		CLOSE printit;
		MessageBox("Average Report","Report was printed successfully.")
	ELSE
		// There was an error executing the procedure.
		CLOSE printit;
	END IF	
	st_message.text = ''
ELSE
	MessageBox("ERROR","You must select a printer.")
	open(w_magazine_printer_list)
END IF


end event

type cb_ok from u_cb within w_magazine_average_time_report
integer x = 201
integer y = 620
integer taborder = 0
integer textsize = -10
string text = "&OK"
end type

event clicked;call super::clicked;String ls_contract,ls_producer, ls_fy, ls_combo, ls_temp, ls_userid

//Get data into local variables
ls_fy = em_fy.text
ls_contract = TRIM(sle_contract.text)
ls_producer = TRIM(sle_producer.text)



//If Contract and Producer are both input then it is the wrong combination
IF ( ls_contract <> ''  AND  ls_producer <> '') THEN
	MessageBox('Error','Wrong Combination .. Input either the Contract or the Producer')
	RETURN
END IF	

//Get the combo information to be sent to the stored procedure
//magavgtime_proc. If the fiscal year is the only data then combo = f,
//if fiscal year and contract then combo = c, if fiscal year and 
//producer then combo = p.

IF ls_contract <> '' THEN
	ls_combo = 'c'
	ls_temp = ls_contract
ELSEIF ls_producer <> '' THEN
	ls_combo = 'p'
	ls_temp = ls_producer
ELSE
	ls_combo = 'f' 
	ls_temp = ''
END IF

ls_userid = SqlServerTrans.userid

string output_text
output_text = "Average."+ls_userid
output_text = output_text+" has been stored in /pics2/f_outputs."


st_message.text = 'Generating Report Please Wait ...'

//Declare the stored procedure
	DECLARE magavgtime_proc PROCEDURE FOR
	  PCSADMIN.magavgtime_proc :ls_combo, :ls_fy, :ls_temp, :ls_userid
	USING SqlServerTrans;

//Execute the stored procedure
  EXECUTE magavgtime_proc;
  	IF f_check_dberror(sqlservertrans,"magazine average report") THEN 
		CLOSE magavgtime_proc;
		MessageBox("Finance Report",output_text)
		cb_print.Enabled = TRUE
	ELSE
		CLOSE magavgtime_proc;
		cb_print.Enabled = FALSE
	END IF
    
//clear the message and enable the print button  
st_message.text = ''  

end event

type cb_exit from u_cb within w_magazine_average_time_report
integer x = 1061
integer y = 620
integer taborder = 0
integer textsize = -10
string text = "E&xit"
end type

event clicked;call super::clicked;close(parent)
end event

type st_4 from u_st within w_magazine_average_time_report
integer x = 151
integer y = 28
integer width = 1198
integer height = 116
integer textsize = -10
integer weight = 700
string text = "Input the Fiscal Year and Contract Or the Fiscal Year and  Producer"
alignment alignment = center!
end type

type st_message from u_st within w_magazine_average_time_report
integer x = 229
integer y = 768
integer width = 1170
integer height = 112
string text = ""
end type


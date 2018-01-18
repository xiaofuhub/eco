$PBExportHeader$w_magazine_finance_report.srw
forward
global type w_magazine_finance_report from w_sheet
end type
type em_fy from u_em within w_magazine_finance_report
end type
type st_1 from u_st within w_magazine_finance_report
end type
type sle_format from u_sle within w_magazine_finance_report
end type
type st_2 from u_st within w_magazine_finance_report
end type
type cb_ok from u_cb within w_magazine_finance_report
end type
type cb_exit from u_cb within w_magazine_finance_report
end type
type cb_print from u_cb within w_magazine_finance_report
end type
type st_message from u_st within w_magazine_finance_report
end type
end forward

global type w_magazine_finance_report from w_sheet
integer x = 768
integer y = 640
integer width = 1399
integer height = 792
string title = "Comparative Financial Analyses"
em_fy em_fy
st_1 st_1
sle_format sle_format
st_2 st_2
cb_ok cb_ok
cb_exit cb_exit
cb_print cb_print
st_message st_message
end type
global w_magazine_finance_report w_magazine_finance_report

on w_magazine_finance_report.create
int iCurrent
call super::create
this.em_fy=create em_fy
this.st_1=create st_1
this.sle_format=create sle_format
this.st_2=create st_2
this.cb_ok=create cb_ok
this.cb_exit=create cb_exit
this.cb_print=create cb_print
this.st_message=create st_message
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.em_fy
this.Control[iCurrent+2]=this.st_1
this.Control[iCurrent+3]=this.sle_format
this.Control[iCurrent+4]=this.st_2
this.Control[iCurrent+5]=this.cb_ok
this.Control[iCurrent+6]=this.cb_exit
this.Control[iCurrent+7]=this.cb_print
this.Control[iCurrent+8]=this.st_message
end on

on w_magazine_finance_report.destroy
call super::destroy
destroy(this.em_fy)
destroy(this.st_1)
destroy(this.sle_format)
destroy(this.st_2)
destroy(this.cb_ok)
destroy(this.cb_exit)
destroy(this.cb_print)
destroy(this.st_message)
end on

event open;call super::open;// print button
cb_print.Enabled = FALSE
end event

type em_fy from u_em within w_magazine_finance_report
integer x = 553
integer y = 60
integer width = 279
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

type st_1 from u_st within w_magazine_finance_report
integer x = 192
integer y = 64
integer width = 343
integer height = 68
integer textsize = -10
string text = "Fiscal Year"
alignment alignment = right!
end type

type sle_format from u_sle within w_magazine_finance_report
integer x = 553
integer y = 176
integer width = 279
integer height = 80
integer taborder = 20
integer textsize = -10
textcase textcase = upper!
end type

event modified;call super::modified;//String null_string
//
//
//SetNull(null_string)
//
//
//IF TRIM(sle_format.Text) <> 'FD' AND TRIM(sle_format.text) <> 'BR' THEN
//	MessageBox('Error','Format is "BR" or "FD" ')
//	sle_format.text = null_string
//	sle_format.SetFocus()
//	RETURN
//END IF
//

end event

type st_2 from u_st within w_magazine_finance_report
integer x = 306
integer y = 180
integer width = 229
integer textsize = -10
string text = "Format"
alignment alignment = right!
end type

type cb_ok from u_cb within w_magazine_finance_report
integer x = 293
integer y = 360
integer width = 265
integer taborder = 0
integer textsize = -10
string text = "&OK"
boolean default = true
end type

event clicked;call super::clicked;String ls_format, li_fy, ls_userid

li_fy = em_fy.text
ls_format = sle_format.text

//Check to see if the fiscal year and the format have been input.
IF IsNull(li_fy) OR TRIM(li_fy) = '' OR li_fy = '0000' THEN 
	MessageBox('Error','Fiscal Year Needed to Process')
	em_fy.SetFocus()
ELSEIF IsNull(ls_format) OR TRIM(ls_format) = '' THEN
	MessageBox('Error','Format Needed to Process')
	sle_format.SetFocus()	
ELSEIF NOT(IsNull(ls_format)) and NOT(IsNull(li_fy)) THEN 

	//Get the login userid
	ls_userid = SqlServerTrans.userid
	string output_text
	output_text = "Finance."+ls_userid
	output_text = output_text+" has been stored in /pics2/f_outputs directory of SP5 server."
	
	//Declare the stored procedure
	DECLARE magproc PROCEDURE FOR                 
		magfinance_proc :li_fy, :ls_format, :ls_userid
	USING sqlservertrans; 
		
	st_message.text = 'Generating Report Please Wait ...'
		
	//Execute the stored procedure
	EXECUTE magproc;
  	IF f_check_dberror(sqlservertrans,"magazine financial report") THEN 
		CLOSE magproc;
		MessageBox("Finance Report",output_text)
		cb_print.Enabled = TRUE
	ELSE
		CLOSE magproc;
		cb_print.Enabled = FALSE
	END IF
	
	st_message.text = ''
	
END IF









end event

type cb_exit from u_cb within w_magazine_finance_report
integer x = 951
integer y = 360
integer width = 265
integer taborder = 0
integer textsize = -10
string text = "E&xit"
end type

event clicked;call super::clicked;close(w_magazine_finance_report)
end event

type cb_print from u_cb within w_magazine_finance_report
integer x = 622
integer y = 360
integer width = 265
integer taborder = 0
integer textsize = -10
string text = "P&rint"
end type

event clicked;call super::clicked;String ls_filename, ls_userid, lp_name, prnt_cmd

open(w_magazine_printer_list)
lp_name = Message.StringParm

IF lp_name <> "" THEN
	ls_userid = SqlServerTrans.userid
	
	ls_filename = 'Finance.'+ls_userid
	
	//Declare the procedure
	DECLARE printit PROCEDURE FOR
	 printproc :lp_name, :ls_userid
	USING SqlServerTrans;	
	
	st_message.text = 'Printing Report Please Wait ...'
	
	//EXECUTE THE PROCEDURE
	EXECUTE printit;
  	IF f_check_dberror(sqlservertrans,"printing magazine financial report") THEN 
		CLOSE printit;
		MessageBox("Financial Report","Report was printed successfully.")
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

type st_message from u_st within w_magazine_finance_report
integer x = 133
integer y = 516
integer width = 1079
integer height = 88
string text = ""
end type


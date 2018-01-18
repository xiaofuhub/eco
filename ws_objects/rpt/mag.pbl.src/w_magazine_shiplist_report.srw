$PBExportHeader$w_magazine_shiplist_report.srw
forward
global type w_magazine_shiplist_report from w_sheet
end type
type em_shipdate from u_em within w_magazine_shiplist_report
end type
type st_1 from u_st within w_magazine_shiplist_report
end type
type cb_ok from u_cb within w_magazine_shiplist_report
end type
type cb_print from u_cb within w_magazine_shiplist_report
end type
type cb_exit from u_cb within w_magazine_shiplist_report
end type
type st_report from u_st within w_magazine_shiplist_report
end type
type st_2 from statictext within w_magazine_shiplist_report
end type
type st_3 from statictext within w_magazine_shiplist_report
end type
type today_date from u_em within w_magazine_shiplist_report
end type
end forward

global type w_magazine_shiplist_report from w_sheet
integer width = 1545
integer height = 800
string title = "Shiplist"
em_shipdate em_shipdate
st_1 st_1
cb_ok cb_ok
cb_print cb_print
cb_exit cb_exit
st_report st_report
st_2 st_2
st_3 st_3
today_date today_date
end type
global w_magazine_shiplist_report w_magazine_shiplist_report

forward prototypes
public function integer wf_check_shipdate (string ls_shipdate)
end prototypes

public function integer wf_check_shipdate (string ls_shipdate);long ll_num_rows
date ld_date
string ls_date, ls_day, ls_mon, ls_yr, ls_month

ld_date =today()
ls_date=string(ld_date,'dd-Mon-yyyy')
ls_day=mid(ls_shipdate,4,2)
ls_mon=mid(ls_shipdate,1,2)
ls_yr=mid(ls_shipdate,7,4)
choose case ls_mon
	case '01'
		ls_month='Jan'
	case '02'
		ls_month='Feb'
	case '03'
		ls_month='Mar'
	case '04'
		ls_month='Apr'
	case '05'
		ls_month='May'
	case '06'
		ls_month='Jun'
	case '07'
		ls_month='Jul'
	case '08'
		ls_month='Aug'
	case '09'
		ls_month='Sep'
	case '10'
		ls_month='Oct'
	case '11'
		ls_month='Nov'
	case '12'
		ls_month='Dec'
end choose
ls_shipdate=ls_day+'-'+ls_month+'-'+ls_yr
SELECT count(*)
INTO   :ll_num_rows
FROM	 magiss
WHERE  shipdt > :ls_shipdate AND shipdt < :ld_date
USING SqlServerTrans;

RETURN  ll_num_rows



end function

on w_magazine_shiplist_report.create
int iCurrent
call super::create
this.em_shipdate=create em_shipdate
this.st_1=create st_1
this.cb_ok=create cb_ok
this.cb_print=create cb_print
this.cb_exit=create cb_exit
this.st_report=create st_report
this.st_2=create st_2
this.st_3=create st_3
this.today_date=create today_date
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.em_shipdate
this.Control[iCurrent+2]=this.st_1
this.Control[iCurrent+3]=this.cb_ok
this.Control[iCurrent+4]=this.cb_print
this.Control[iCurrent+5]=this.cb_exit
this.Control[iCurrent+6]=this.st_report
this.Control[iCurrent+7]=this.st_2
this.Control[iCurrent+8]=this.st_3
this.Control[iCurrent+9]=this.today_date
end on

on w_magazine_shiplist_report.destroy
call super::destroy
destroy(this.em_shipdate)
destroy(this.st_1)
destroy(this.cb_ok)
destroy(this.cb_print)
destroy(this.cb_exit)
destroy(this.st_report)
destroy(this.st_2)
destroy(this.st_3)
destroy(this.today_date)
end on

event open;call super::open;//Disable the OK and Print Buttons
cb_print.Enabled = FALSE

//Set focus to the shipdate
em_shipdate.SetFocus()
end event

type em_shipdate from u_em within w_magazine_shiplist_report
integer x = 494
integer y = 136
integer width = 366
integer height = 84
integer taborder = 10
integer textsize = -10
alignment alignment = center!
maskdatatype maskdatatype = datemask!
string mask = "mm/dd/yyyy"
string displaydata = "è"
double increment = 0
string minmax = ""
end type

event modified;call super::modified;cb_OK.TriggerEvent(Clicked!)
end event

type st_1 from u_st within w_magazine_shiplist_report
integer x = 265
integer y = 148
integer width = 192
integer textsize = -10
string text = "From:"
alignment alignment = right!
end type

type cb_ok from u_cb within w_magazine_shiplist_report
integer x = 160
integer y = 424
integer taborder = 0
integer textsize = -10
string text = "&OK"
boolean default = true
end type

event clicked;call super::clicked;String  ls_date, ls_userid
Long li_num_rows
date ld_date

//Get data into the local variable
ls_date = em_shipdate.text

st_report.text = 'Validating the dates, Please Wait ...'
//Check to see if there is data for that shipdate
li_num_rows = wf_check_shipdate(ls_date)

//If no data display message and return
IF li_num_rows = 0 THEN
	st_report.text = ''
	MessageBox('Error','No Data Found .. Report Not Generated')
	em_shipdate.SetFocus()
	em_shipdate.Event pfc_selectall()
	RETURN
END IF
st_report.text = ''

st_report.text = 'Generating Report, Please Wait ...'

ls_userid = SqlServerTrans.userid
ld_date = date(ls_date)
ls_date = string(ld_date,'mm/dd/yyyy')

// MessageBox("data","date = "+ls_date+" userid = "+ls_userid)

//Declare the procedure
DECLARE proc_magship PROCEDURE FOR
 magship_proc :ls_date, :ls_userid
USING SqlServerTrans;

//Execute the procedure
Execute proc_magship;

IF f_check_dberror(sqlservertrans,"magazine shiplist report") THEN 
	CLOSE proc_magship;
	MessageBox("Shiplist Report","Report has been stored in /pics2/f_outputs directory of SP5 server.")
	cb_print.Enabled = TRUE
ELSE
	CLOSE proc_magship;
	cb_print.Enabled = FALSE
END IF

//close the procedure
CLOSE proc_magship;

st_report.text = ''
end event

type cb_print from u_cb within w_magazine_shiplist_report
integer x = 571
integer y = 424
integer taborder = 0
integer textsize = -10
string text = "&Print"
end type

event clicked;call super::clicked;String ls_filename, ls_userid, lp_name, prnt_cmd

open(w_magazine_printer_list)
lp_name = Message.StringParm

IF lp_name <> "" THEN
	ls_userid = SqlServerTrans.userid
	
	ls_filename = 'Shipdate.'+ls_userid
	
	//Declare the procedure
	DECLARE printit PROCEDURE FOR
	 printproc :lp_name, :ls_userid
	USING SqlServerTrans;	
	
//	st_message.text = 'Printing Report Please Wait ...'
	
	//EXECUTE THE PROCEDURE
	EXECUTE printit;
  	IF f_check_dberror(sqlservertrans,"printing magazine shipdate report") THEN 
		CLOSE printit;
		MessageBox("Shipdate Report","Report was printed successfully.")
	ELSE
		// There was an error executing the procedure.
		CLOSE printit;
	END IF	
//	st_message.text = ''
ELSE
	MessageBox("ERROR","You must select a printer.")
	open(w_magazine_printer_list)
END IF
end event

type cb_exit from u_cb within w_magazine_shiplist_report
integer x = 983
integer y = 424
integer taborder = 0
integer textsize = -10
string text = "E&xit"
end type

event clicked;call super::clicked;close(parent)
end event

type st_report from u_st within w_magazine_shiplist_report
integer x = 91
integer y = 560
integer width = 1248
integer height = 100
long backcolor = 79741120
string text = ""
end type

type st_2 from statictext within w_magazine_shiplist_report
integer x = 41
integer y = 12
integer width = 1390
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long backcolor = 80263581
boolean enabled = false
string text = "Please enter a date for magazines that are shiped:"
alignment alignment = right!
boolean focusrectangle = false
end type

type st_3 from statictext within w_magazine_shiplist_report
integer x = 302
integer y = 252
integer width = 155
integer height = 76
boolean bringtotop = true
integer textsize = -10
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long backcolor = 80263581
boolean enabled = false
string text = "To:"
alignment alignment = right!
boolean focusrectangle = false
end type

type today_date from u_em within w_magazine_shiplist_report
event modified pbm_enmodified
integer x = 498
integer y = 248
integer width = 357
integer height = 84
integer taborder = 0
integer textsize = -10
alignment alignment = center!
maskdatatype maskdatatype = datemask!
string mask = "mm/dd/yyyy"
boolean autoskip = true
string displaydata = ""
double increment = 0
string minmax = ""
end type

event constructor;call super::constructor;today_date.text = string(Today())
end event


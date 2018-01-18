$PBExportHeader$w_duplicate_title_check.srw
forward
global type w_duplicate_title_check from w_sheet
end type
type cb_exit from u_cb within w_duplicate_title_check
end type
type cb_find from u_cb within w_duplicate_title_check
end type
type sle_date from u_sle within w_duplicate_title_check
end type
type em_1 from u_em within w_duplicate_title_check
end type
end forward

global type w_duplicate_title_check from w_sheet
integer x = 183
integer y = 288
integer width = 1911
integer height = 964
string title = "Duplicated Title Report"
cb_exit cb_exit
cb_find cb_find
sle_date sle_date
em_1 em_1
end type
global w_duplicate_title_check w_duplicate_title_check

type variables
string is_chno
string is_conno
string is_syntax[6]
end variables

on w_duplicate_title_check.create
int iCurrent
call super::create
this.cb_exit=create cb_exit
this.cb_find=create cb_find
this.sle_date=create sle_date
this.em_1=create em_1
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.cb_exit
this.Control[iCurrent+2]=this.cb_find
this.Control[iCurrent+3]=this.sle_date
this.Control[iCurrent+4]=this.em_1
end on

on w_duplicate_title_check.destroy
call super::destroy
destroy(this.cb_exit)
destroy(this.cb_find)
destroy(this.sle_date)
destroy(this.em_1)
end on

event pfc_postopen;call super::pfc_postopen;w_pics_main.SetMicroHelp("Please Enter The Valid Date To BE Checked")

end event

event resize;call super::resize;//This script maintains the resize of the Window Duplicate Title Check

This.X = w_pics_main.X
This.Y = w_pics_main.Y
end event

type cb_exit from u_cb within w_duplicate_title_check
event pfc_hinttext pbm_mousemove
integer x = 1152
integer y = 644
integer taborder = 30
integer textsize = -10
string text = "E&xit"
end type

event pfc_hinttext;call super::pfc_hinttext;w_pics_main.setmicrohelp("Exit's the current window")
end event

event clicked;call super::clicked;Close(parent)
end event

type cb_find from u_cb within w_duplicate_title_check
event pfc_hinttext pbm_mousemove
integer x = 338
integer y = 644
integer taborder = 20
integer textsize = -10
string text = "&Find"
boolean default = true
end type

event pfc_hinttext;call super::pfc_hinttext;w_pics_main.setmicrohelp("Find's the Duplicate Title Based On Valid Date")
end event

event clicked;call super::clicked;date ld_date, ld_dummy
n_ds lds_datastore


IF NOT ISDate(em_1.Text) THEN
	Messagebox("Invalid Date", "Please enter a valid date and press Find.")
	em_1.Setfocus()
	RETURN
ELSE
	ld_date = date(em_1.Text)
	DECLARE duplicate_date CURSOR FOR
	SELECT ttlinit.ttldt
	FROM ttlinit
	WHERE ttlinit.ttldt = :ld_date
	USING sqlservertrans ;
	
	OPEN duplicate_date;
	FETCH duplicate_date INTO:ld_dummy;
	IF NOT sqlservertrans.sqlcode = 0 THEN
		Messagebox("Invalid Date","Please enter a valid date."+   em_1.text)
		CLOSE duplicate_date;
		RETURN
	END IF
	CLOSE duplicate_date;
	lds_datastore = CREATE n_ds
	lds_datastore.dataobject = 'd_duplicate_title_report'
	lds_datastore.Settransobject(sqlservertrans)
	lds_datastore.Retrieve(ld_date)
	IF lds_datastore.Rowcount() > 0 THEN
		lds_datastore.Print()
		Messagebox("Report", "Duplicate Titles report has been printed.")
	ELSE
		Messagebox("Report","No duplicate titles were found.")
	END IF
END IF
	
end event

type sle_date from u_sle within w_duplicate_title_check
integer x = 133
integer y = 328
integer width = 937
integer height = 76
integer taborder = 0
integer textsize = -10
long backcolor = 67108864
string text = "ENTER DATE TO BE CHECKED:"
boolean border = false
borderstyle borderstyle = stylebox!
end type

type em_1 from u_em within w_duplicate_title_check
event pfc_hinttext pbm_mousemove
integer x = 1061
integer y = 312
integer width = 347
integer height = 96
integer taborder = 10
integer textsize = -10
maskdatatype maskdatatype = datemask!
string mask = "mm/dd/yy"
string displaydata = "Ä"
double increment = 0
string minmax = ""
end type

event pfc_hinttext;call super::pfc_hinttext;w_pics_main.setmicrohelp("Enter Valid Date To Be Checked")
end event


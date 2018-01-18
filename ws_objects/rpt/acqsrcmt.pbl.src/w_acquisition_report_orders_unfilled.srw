$PBExportHeader$w_acquisition_report_orders_unfilled.srw
$PBExportComments$Window reporting unfilled order acquisitions
forward
global type w_acquisition_report_orders_unfilled from w_sheet
end type
type dw_acquisition_unfilled from u_dw within w_acquisition_report_orders_unfilled
end type
type st_1 from u_st within w_acquisition_report_orders_unfilled
end type
type st_2 from u_st within w_acquisition_report_orders_unfilled
end type
type cb_clear from u_cb within w_acquisition_report_orders_unfilled
end type
type cb_exit from u_cb within w_acquisition_report_orders_unfilled
end type
type cb_print from u_cb within w_acquisition_report_orders_unfilled
end type
type em_date1 from u_em within w_acquisition_report_orders_unfilled
end type
type em_date2 from u_em within w_acquisition_report_orders_unfilled
end type
end forward

global type w_acquisition_report_orders_unfilled from w_sheet
integer width = 2917
integer height = 1692
string title = "Unfilled Orders"
dw_acquisition_unfilled dw_acquisition_unfilled
st_1 st_1
st_2 st_2
cb_clear cb_clear
cb_exit cb_exit
cb_print cb_print
em_date1 em_date1
em_date2 em_date2
end type
global w_acquisition_report_orders_unfilled w_acquisition_report_orders_unfilled

forward prototypes
public subroutine wf_print_title (long print_job, string ls_print_string, integer tab2)
public subroutine wf_print_author (long print_job, string ls_print_string, integer tab1)
public subroutine wf_print_copies_ordered (long print_job, string ls_print_string, integer tab)
end prototypes

public subroutine wf_print_title (long print_job, string ls_print_string, integer tab2);
IF len(ls_print_string) < 25 THEN ls_print_string = ls_print_string+fill(' ',25 - len(ls_print_string))
Print(print_job,ls_print_string+' ',tab2)
end subroutine

public subroutine wf_print_author (long print_job, string ls_print_string, integer tab1);
IF len(ls_print_string) < 15 THEN ls_print_string = ls_print_string+Fill(' ',15 - len(ls_print_string))
Print(print_job,ls_print_string+' ',tab1)
end subroutine

public subroutine wf_print_copies_ordered (long print_job, string ls_print_string, integer tab);IF len(ls_print_string) < 6 THEN ls_print_string = ls_print_string + Fill(' ', 6 - len(ls_print_string))

Print(print_job,ls_print_string+' ',tab)
end subroutine

event pfc_preopen;call super::pfc_preopen;this.of_SetPreference(TRUE)
this.inv_preference.of_SetToolBars(TRUE)
this.inv_preference.of_SetWindow(TRUE)
this.of_SetResize(TRUE)
this.inv_resize.of_SetOrigSize(this.workspacewidth(),this.workspaceheight())

inv_resize.of_Register(dw_acquisition_unfilled, "scale")
inv_resize.of_Register(cb_clear, "scale")
inv_resize.of_Register(cb_exit, "scale")
inv_resize.of_Register(cb_print, "scale")
inv_resize.of_Register(em_date1, "scale")
inv_resize.of_Register(em_date2, "scale")


inv_resize.of_Register(st_1, "scale")
inv_resize.of_Register(st_2, "scale")


end event

event resize;call super::resize;long ll_height
This.X = w_pics_main.X
This.Y = w_pics_main.Y
ll_height = w_pics_main.mdi_1.Height
This.Resize(w_pics_main.WorkSpaceWidth(), ll_height)
end event

on w_acquisition_report_orders_unfilled.create
int iCurrent
call super::create
this.dw_acquisition_unfilled=create dw_acquisition_unfilled
this.st_1=create st_1
this.st_2=create st_2
this.cb_clear=create cb_clear
this.cb_exit=create cb_exit
this.cb_print=create cb_print
this.em_date1=create em_date1
this.em_date2=create em_date2
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_acquisition_unfilled
this.Control[iCurrent+2]=this.st_1
this.Control[iCurrent+3]=this.st_2
this.Control[iCurrent+4]=this.cb_clear
this.Control[iCurrent+5]=this.cb_exit
this.Control[iCurrent+6]=this.cb_print
this.Control[iCurrent+7]=this.em_date1
this.Control[iCurrent+8]=this.em_date2
end on

on w_acquisition_report_orders_unfilled.destroy
call super::destroy
destroy(this.dw_acquisition_unfilled)
destroy(this.st_1)
destroy(this.st_2)
destroy(this.cb_clear)
destroy(this.cb_exit)
destroy(this.cb_print)
destroy(this.em_date1)
destroy(this.em_date2)
end on

event open;call super::open;//open the sheet in maximized state
this.windowstate = maximized!


end event

event mousemove;call super::mousemove;w_pics_main.Event pfc_microhelp("Ready")
end event

type dw_acquisition_unfilled from u_dw within w_acquisition_report_orders_unfilled
event ue_hint_text pbm_mousemove
integer x = 32
integer y = 224
integer width = 2775
integer height = 992
integer taborder = 0
string dataobject = "d_report_unfilled_orders"
boolean hscrollbar = true
boolean ib_isupdateable = false
boolean ib_rmbmenu = false
boolean ib_rmbfocuschange = false
string is_updatesallowed = ""
end type

event ue_hint_text;call super::ue_hint_text;string ls_object, ls_column, ls_column_tag
long ll_pos

//This script set's microhelp at the bottom of the screen for the Archive Title Datawindow
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

event constructor;call super::constructor;SetTransObject(SqlServerTrans)

end event

type st_1 from u_st within w_acquisition_report_orders_unfilled
integer x = 1248
integer y = 60
integer width = 119
integer textsize = -10
string text = "To"
alignment alignment = right!
end type

type st_2 from u_st within w_acquisition_report_orders_unfilled
integer x = 626
integer y = 56
integer width = 155
boolean bringtotop = true
integer textsize = -10
string text = "From"
alignment alignment = right!
end type

type cb_clear from u_cb within w_acquisition_report_orders_unfilled
event ue_hint_text pbm_mousemove
string tag = "Clears the screen"
integer x = 1650
integer y = 1316
integer taborder = 0
integer textsize = -10
string text = "&Clear"
end type

event ue_hint_text;call super::ue_hint_text;
w_pics_main.Event pfc_microhelp(this.tag)
end event

event clicked;call super::clicked;dw_acquisition_unfilled.Reset()
em_date1.text = ''
em_date2.text = ''
cb_print.Enabled = FALSE
em_date1.setfocus()
end event

type cb_exit from u_cb within w_acquisition_report_orders_unfilled
event ue_hint_text pbm_mousemove
string tag = "Exit the screen"
integer x = 2103
integer y = 1316
integer taborder = 0
boolean bringtotop = true
integer textsize = -10
string text = "E&xit"
end type

event ue_hint_text;call super::ue_hint_text;
w_pics_main.Event pfc_microhelp(this.tag)
end event

event clicked;call super::clicked;dw_acquisition_unfilled.Reset()
close(w_acquisition_report_orders_unfilled)
end event

type cb_print from u_cb within w_acquisition_report_orders_unfilled
event ue_hint_text pbm_mousemove
string tag = "Print Report"
integer x = 1207
integer y = 1316
integer taborder = 0
integer textsize = -10
boolean enabled = false
string text = "&Print"
end type

event ue_hint_text;call super::ue_hint_text;w_pics_main.Event pfc_microhelp(this.tag)
end event

event clicked;call super::clicked;Integer li_rtn_code, li_max_rows
Long    print_job, print_ret, li_loop, ll_length
String  ls_font_name, ls_date1, ls_date2, ls_chno, ls_title, ls_author, ls_order_date, ls_copies_ordered, ls_vendor_code
String  ls_total,ld_order_date
    

//Get the range of dates from the edit masks
ls_date1 = em_date1.text
ls_date2 = em_date2.text

li_rtn_code = MessageBox('Print Report',"Do you want to print the report?",Question!, yesno!)

IF li_rtn_code = 2 THEN
	RETURN 
END IF

//Create the print job
print_job = PrintOpen()
//Define the Font and other characteristics
PrintDefineFont(print_job,1,"MS Sans Serif", &
                0, 400, Default!, AnyFont!, False, False)
					 
PrintDefineFont(print_job,2,"Ms Sans Serif", &
                -13,650, Default!,AnyFont!, False,FALSE)
					 
PrintDefineFont(print_job,3,"Ms Sans Serif", &
                -11, 650, Default!,AnyFont!, FALSE, TRUE)


//Print Report Heading
PrintSetFont(print_job,2)
Print(print_job,0,"Report For Unfilled Orders For Dates Between: "+ls_date1+' AND '+ls_date2)
Print(Print_job,0,' ')

//Print Title Heading
PrintSetFont(print_job,3)
Print(print_job,0,"Chart "+fill(' ',2)+"Title"+fill(' ',32)+"Author"+fill(' ',8)+"Ord. Date "+"Cop. Ord."+fill(' ',3)+"Vendor")


//Print the contents of the datawindow
w_pics_main.Event pfc_microhelp("Printing Report ...")
li_max_rows = dw_acquisition_unfilled.rowcount()

FOR li_loop = 1 to li_max_rows
ls_chno = dw_acquisition_unfilled.object.ttlinit_chno[li_loop]
ls_title = dw_acquisition_unfilled.object.ttlinit_ttl[li_loop]
//Build the ls_title string
IF IsNull(ls_title) THEN
 ls_title = fill(' ',30)
ELSE 
	IF len(ls_title) > 30 THEN ls_title = mid(ls_title,1,30)
	IF len(ls_title) < 30 THEN ls_title = ls_title + fill(' ',30 - len(ls_title))
END IF
//Build the ls_author string
ls_author = dw_acquisition_unfilled.GetItemString(li_loop,"ttlinit_auth")
IF IsNull(ls_author) THEN 
	ls_author = fill(' ',15)
ELSE
	IF len(ls_author) > 15 THEN ls_author = mid(ls_author,1,15)
	IF len(ls_author) < 15 THEN ls_author = ls_author + fill(' ',15 - len(ls_author))
END IF
ld_order_date = string(dw_acquisition_unfilled.object.acquist_pbordt[li_loop],'MM/DD/YYYY')
ls_copies_ordered = String(dw_acquisition_unfilled.object.acquist_pbneed[li_loop])
//Build ls_copies_ordered string
IF IsNull(ls_copies_ordered) THEN
	ls_copies_ordered = fill(' ',6)
ELSE
	IF len(ls_copies_ordered) < 6 THEN ls_copies_ordered = ls_copies_ordered + fill(' ',4 - len(ls_copies_ordered))
END IF
//Build vendor code
ls_vendor_code    =dw_acquisition_unfilled.object.acquist_pbdlrcd[li_loop]
IF IsNull(ls_vendor_code) THEN ls_vendor_code = ' '

PrintSetFont(print_job,1)

//Print the chart number
PRINT(Print_job,0,ls_chno+' '+ls_title+' '+ls_author+' '+ld_order_date+fill(' ',3)+String(ls_copies_ordered)+fill(' ',6)+ls_vendor_code)

NEXT





//Close the print job
PrintClose(print_job)



					 
					 
					 
					 
					 
					 

end event

type em_date1 from u_em within w_acquisition_report_orders_unfilled
string tag = "Enter the first range of chart numbers"
integer x = 841
integer y = 44
integer width = 334
integer height = 96
integer taborder = 10
integer textsize = -10
alignment alignment = center!
maskdatatype maskdatatype = datemask!
string mask = "mm/dd/yyyy"
boolean autoskip = true
string displaydata = " "
double increment = 0
string minmax = ""
end type

type em_date2 from u_em within w_acquisition_report_orders_unfilled
integer x = 1431
integer y = 44
integer width = 325
integer height = 96
integer taborder = 20
boolean bringtotop = true
integer textsize = -10
alignment alignment = center!
maskdatatype maskdatatype = datemask!
string mask = "mm/dd/yyyy"
string displaydata = "ô"
double increment = 0
string minmax = ""
end type

event modified;call super::modified;Date ld_date1, ld_date2
Long ll_rows

ld_date1 = Date(em_date1.Text)
ld_date2 = Date(em_date2.Text)


w_pics_main.Event pfc_microhelp('Retrieving Data .. Please Wait ...')
ll_rows = dw_acquisition_unfilled.Retrieve(ld_date1, ld_date2)


IF ll_rows = 0 THEN
	w_pics_main.Event pfc_microhelp("No Rows Retrieved")
ELSE
	cb_print.Enabled = TRUE
	w_pics_main.Event pfc_microhelp(String(ll_rows)+ ' Rows Retrieved')
END IF
	
end event

event getfocus;call super::getfocus;String ls_date1, ls_date2

ls_date1 = em_date1.text
em_date2.text = ls_date1
end event


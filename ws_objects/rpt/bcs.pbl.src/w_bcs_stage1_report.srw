$PBExportHeader$w_bcs_stage1_report.srw
forward
global type w_bcs_stage1_report from w_sheet
end type
type dw_media from u_pics_dw within w_bcs_stage1_report
end type
type em_span from u_em within w_bcs_stage1_report
end type
type st_4 from statictext within w_bcs_stage1_report
end type
type ddlb_media from dropdownlistbox within w_bcs_stage1_report
end type
type st_3 from statictext within w_bcs_stage1_report
end type
type em_s1date from u_em within w_bcs_stage1_report
end type
type st_1 from u_st within w_bcs_stage1_report
end type
type sle_initial from u_sle within w_bcs_stage1_report
end type
type st_2 from u_st within w_bcs_stage1_report
end type
type cb_ok from u_cb within w_bcs_stage1_report
end type
type cb_exit from u_cb within w_bcs_stage1_report
end type
type cb_print from u_cb within w_bcs_stage1_report
end type
type st_message from u_st within w_bcs_stage1_report
end type
end forward

global type w_bcs_stage1_report from w_sheet
integer x = 768
integer y = 640
integer width = 1198
integer height = 1112
string title = "BCS Stage I Report"
boolean minbox = false
boolean maxbox = false
boolean resizable = false
windowtype windowtype = response!
dw_media dw_media
em_span em_span
st_4 st_4
ddlb_media ddlb_media
st_3 st_3
em_s1date em_s1date
st_1 st_1
sle_initial sle_initial
st_2 st_2
cb_ok cb_ok
cb_exit cb_exit
cb_print cb_print
st_message st_message
end type
global w_bcs_stage1_report w_bcs_stage1_report

on w_bcs_stage1_report.create
int iCurrent
call super::create
this.dw_media=create dw_media
this.em_span=create em_span
this.st_4=create st_4
this.ddlb_media=create ddlb_media
this.st_3=create st_3
this.em_s1date=create em_s1date
this.st_1=create st_1
this.sle_initial=create sle_initial
this.st_2=create st_2
this.cb_ok=create cb_ok
this.cb_exit=create cb_exit
this.cb_print=create cb_print
this.st_message=create st_message
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_media
this.Control[iCurrent+2]=this.em_span
this.Control[iCurrent+3]=this.st_4
this.Control[iCurrent+4]=this.ddlb_media
this.Control[iCurrent+5]=this.st_3
this.Control[iCurrent+6]=this.em_s1date
this.Control[iCurrent+7]=this.st_1
this.Control[iCurrent+8]=this.sle_initial
this.Control[iCurrent+9]=this.st_2
this.Control[iCurrent+10]=this.cb_ok
this.Control[iCurrent+11]=this.cb_exit
this.Control[iCurrent+12]=this.cb_print
this.Control[iCurrent+13]=this.st_message
end on

on w_bcs_stage1_report.destroy
call super::destroy
destroy(this.dw_media)
destroy(this.em_span)
destroy(this.st_4)
destroy(this.ddlb_media)
destroy(this.st_3)
destroy(this.em_s1date)
destroy(this.st_1)
destroy(this.sle_initial)
destroy(this.st_2)
destroy(this.cb_ok)
destroy(this.cb_exit)
destroy(this.cb_print)
destroy(this.st_message)
end on

event open;call super::open;// print button
cb_print.Enabled = FALSE
this.center =TRUE
end event

type dw_media from u_pics_dw within w_bcs_stage1_report
integer x = 251
integer y = 184
integer height = 124
integer taborder = 20
string dataobject = "d_media_types"
boolean vscrollbar = false
boolean border = false
boolean livescroll = false
borderstyle borderstyle = stylebox!
boolean ib_isupdateable = false
boolean ib_rmbmenu = false
boolean ib_rmbfocuschange = false
end type

event constructor;call super::constructor;//////////////////////////////////////////////////////////////////////////////////////////////////////////
//
//	Event: constructor for dw
//
//	Description:
//	Set trans object and insert a blank row populate media dropdown
//
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//	
//	Revision History
//
//	Developed by 	Date 				Version						Tracking#
//									
// Murali K.			01/30/2008      002 PICS Modifications	 BCS PICS 2.0 modifications
//											Media dropdown from reference table
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//

This.of_SetTransObject(sqlservertrans)
This.event pfc_insertrow()
end event

event ue_postconstructor;call super::ue_postconstructor;//////////////////////////////////////////////////////////////////////////////////////////////////////////
//
//	Event: ue_posconstructor for dw
//
//	Description:
//	Filter the dddw
//
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//	
//	Revision History
//
//	Developed by 	Date 				Version						Tracking#
//									
// Murali K.			02/01/2008      002 PICS Modifications	 BCS PICS 2.0 modifications
//											media dropdown from pics_media table
//											filter other than P/B - print braille
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//

datawindowchild ldwc
string ls_filter
int li_rtn

li_rtn = this.getchild('media',ldwc)
ls_filter = "med <> "+"'P/B'"
ldwc.setFilter(ls_filter)
li_rtn = ldwc.Filter()

end event

type em_span from u_em within w_bcs_stage1_report
integer x = 512
integer y = 340
integer width = 137
integer height = 100
integer taborder = 30
integer textsize = -10
string text = "7"
alignment alignment = center!
string mask = "#"
boolean autoskip = true
string displaydata = "Ä"
string minmax = "1~~9"
end type

type st_4 from statictext within w_bcs_stage1_report
integer x = 55
integer y = 348
integer width = 425
integer height = 64
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 33554432
long backcolor = 80263581
string text = "Days Spanned"
alignment alignment = right!
boolean focusrectangle = false
end type

type ddlb_media from dropdownlistbox within w_bcs_stage1_report
boolean visible = false
integer x = 1006
integer y = 548
integer width = 279
integer height = 300
integer taborder = 20
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 33554432
long backcolor = 1090519039
string text = "none"
string item[] = {"BR","DB","RC"}
borderstyle borderstyle = stylelowered!
end type

event constructor;ddlb_media.text="RC"
end event

type st_3 from statictext within w_bcs_stage1_report
boolean visible = false
integer x = 974
integer y = 456
integer width = 215
integer height = 64
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 33554432
long backcolor = 80263581
string text = "Media"
alignment alignment = right!
boolean focusrectangle = false
end type

type em_s1date from u_em within w_bcs_stage1_report
integer x = 512
integer y = 48
integer width = 453
integer height = 100
integer taborder = 10
integer textsize = -10
alignment alignment = center!
maskdatatype maskdatatype = datemask!
string mask = "mm/dd/yyyy"
boolean autoskip = true
string displaydata = "Ä"
double increment = 0
string minmax = ""
end type

type st_1 from u_st within w_bcs_stage1_report
integer x = 137
integer y = 64
integer width = 343
integer height = 68
integer textsize = -10
string text = "Stage I date"
alignment alignment = right!
end type

type sle_initial from u_sle within w_bcs_stage1_report
integer x = 512
integer y = 472
integer width = 279
integer height = 92
integer taborder = 40
integer textsize = -10
string text = "all"
textcase textcase = lower!
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

type st_2 from u_st within w_bcs_stage1_report
integer x = 251
integer y = 484
integer width = 229
integer textsize = -10
string text = "Initials"
alignment alignment = right!
end type

type cb_ok from u_cb within w_bcs_stage1_report
integer x = 256
integer y = 700
integer width = 265
integer taborder = 0
integer textsize = -10
string text = "&OK"
boolean default = true
end type

event clicked;call super::clicked;//////////////////////////////////////////////////////////////////////////////////////////////////////////
//
//	Event: clicked for cb_ok
//
//	Description:
//	Call stored procedures to generate BCS Stage 1 report
//
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//	
//	Revision History
//
//	Developed by 	Date 				Version						Tracking#
//									
// Murali K.			01/30/2008      002 PICS Modifications	 BCS PICS 2.0 modifications
//											Call new proc for DB media
//											Set media var from dw
//						01/31/2008		01/31/2008 for rc and db stored proc is bcs1rtb
//						02/01/2008 		RC and DB replaced by RTB
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//


String ls_initial, li_s1date, ls_userid,ls_today,ls_filename,ls_filetype,ls_media
int li_span,li_mon,li_year,li_day

li_s1date = em_s1date.text
ls_initial = sle_initial.text
//ls_media = ddlb_media.text
// 01/30/2008
ls_media = dw_media.object.media[1]
li_span = integer(em_span.text)
li_mon = month(today())
li_year= year(today())
li_day= day(today())

ls_today = string(li_mon)+ string(li_day) + Mid(string(li_year),3,2)

//Check to see if the stage I date have been inputed.
IF IsNull(li_s1date) THEN 
	MessageBox('Error','Stage I date needed to Process')
	em_s1date.SetFocus()
ELSEIF IsNull(ls_initial) OR TRIM(ls_initial) = '' THEN 
	MessageBox('Error','Initial needed to Process')
	sle_initial.SetFocus()
ELSEIF f_valid_initial(ls_initial)=FALSE AND ls_initial<>'all' THEN
	MessageBox('Error','Initial is not valid')
	sle_initial.SetFocus()	
ELSEIF IsNull(ls_media) OR TRIM(ls_media) = '' THEN
	MessageBox('Error','Media needed to Process')
//	ddlb_media.SetFocus()	
	// 01/30/2008
	dw_media.setfocus()
ELSEIF NOT(IsNull(ls_initial)) and NOT(IsNull(li_s1date)) and NOT(IsNull(ls_media)) THEN 

	
	string output_text,dbname
	
	
	select lower(name) into :dbname from v$database using sqlservertrans;
	if IsNull(dbname) then
		dbname='pcsd'
	end if
	
	ls_filetype = "."+ls_initial
	ls_filename = "S1"+ls_media+ls_today
	ls_filename = ls_filename+ls_filetype
	output_text = ls_filename+" is stored in /pics2/f_outputs/"+dbname+" directory of RS21n server."
	
	//Declare the stored procedure
	// 01/31/2008 for rc and db stored proc is bcs1rtb
	// 02/01/2008 RC and DB replaced by RTB
	IF ls_media='RC'  OR &
	    ls_media= 'DB'  OR  &
		 ls_media= 'RTB'     THEN	
		DECLARE s1repproc PROCEDURE FOR 
			bcs1rtb (:li_s1date, :li_span, :ls_initial, :ls_filename)
		USING sqlservertrans; 
		
		st_message.text = 'Generating Report Please Wait ...'
			
		//Execute the stored procedure
		
		EXECUTE s1repproc;
		IF f_check_dberror(sqlservertrans,"BCS Stage1 report") THEN 
		CLOSE s1repproc;
			MessageBox("BCS Stage1 Report",output_text)
			cb_print.Enabled = TRUE
		ELSE
			CLOSE s1repproc;
			cb_print.Enabled = FALSE
		END IF
//	//01/30/2008 call new proc for media DB
//	ELSEIF ls_media='DB' THEN	
//		DECLARE s3repproc PROCEDURE FOR 
//			bcs1db (:li_s1date, :li_span, :ls_initial, :ls_filename)
//		USING sqlservertrans; 
//		
//		st_message.text = 'Generating Report Please Wait ...'
//			
//		//Execute the stored procedure
//		EXECUTE s3repproc;
//		IF f_check_dberror(sqlservertrans,"BCS Stage1 report for DB media") THEN 
//		CLOSE s3repproc;
//			MessageBox("BCS Stage1 Report for DB media",output_text)
//			cb_print.Enabled = TRUE
//		ELSE
//			CLOSE s3repproc;
//			cb_print.Enabled = FALSE
//		END IF
	ELSE	// BR
		DECLARE s2repproc PROCEDURE FOR 
			bcs1br (:li_s1date, :li_span, :ls_initial, :ls_filename)
		USING sqlservertrans; 
		
		st_message.text = 'Generating Report Please Wait ...'
			
		//Execute the stored procedure
		
		EXECUTE s2repproc;
		IF f_check_dberror(sqlservertrans,"BCS Stage1 report") THEN 
		CLOSE s2repproc;
			MessageBox("BCS Stage1 Report",output_text)
			cb_print.Enabled = TRUE
		ELSE
			CLOSE s2repproc;
			cb_print.Enabled = FALSE
		END IF
	END IF
	st_message.text = ''
	
END IF
end event

type cb_exit from u_cb within w_bcs_stage1_report
integer x = 599
integer y = 696
integer width = 265
integer taborder = 0
integer textsize = -10
string text = "E&xit"
end type

event clicked;call super::clicked;close(w_bcs_stage1_report)
end event

type cb_print from u_cb within w_bcs_stage1_report
boolean visible = false
integer x = 453
integer y = 584
integer width = 265
integer taborder = 0
integer textsize = -10
string text = "P&rint"
end type

event clicked;call super::clicked;//String ls_filename, ls_userid, lp_name, prnt_cmd
//
//open(w_magazine_printer_list)
//lp_name = Message.StringParm
//
//IF lp_name <> "" THEN
//	ls_userid = SqlServerTrans.userid
//	
//	ls_filename = 'Finance.'+ls_userid
//	
//	//Declare the procedure
//	DECLARE printit PROCEDURE FOR
//	 printproc :lp_name, :ls_userid
//	USING SqlServerTrans;	
//	
//	st_message.text = 'Printing Report Please Wait ...'
//	
//	//EXECUTE THE PROCEDURE
//	EXECUTE printit;
//  	IF f_check_dberror(sqlservertrans,"printing magazine financial report") THEN 
//		CLOSE printit;
//		MessageBox("Financial Report","Report was printed successfully.")
//	ELSE
//		// There was an error executing the procedure.
//		CLOSE printit;
//	END IF	
//	st_message.text = ''
//ELSE
//	MessageBox("ERROR","You must select a printer.")
//	open(w_magazine_printer_list)
//END IF
end event

type st_message from u_st within w_bcs_stage1_report
integer x = 101
integer y = 852
integer width = 992
integer height = 88
string text = ""
end type


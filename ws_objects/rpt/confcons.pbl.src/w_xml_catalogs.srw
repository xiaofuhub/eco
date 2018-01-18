$PBExportHeader$w_xml_catalogs.srw
forward
global type w_xml_catalogs from w_response
end type
type em_br_end_date from u_em within w_xml_catalogs
end type
type st_5 from u_st within w_xml_catalogs
end type
type em_br_start_date from u_em within w_xml_catalogs
end type
type st_4 from u_st within w_xml_catalogs
end type
type em_rc_end_date from u_em within w_xml_catalogs
end type
type st_3 from u_st within w_xml_catalogs
end type
type em_rc_start_date from u_em within w_xml_catalogs
end type
type st_2 from u_st within w_xml_catalogs
end type
type rb_nss from radiobutton within w_xml_catalogs
end type
type st_1 from statictext within w_xml_catalogs
end type
type rb_youngerreaders from u_rb within w_xml_catalogs
end type
type rb_braillebooks from u_rb within w_xml_catalogs
end type
type rb_cassettebooks from u_rb within w_xml_catalogs
end type
type rb_braillebookreview from u_rb within w_xml_catalogs
end type
type rb_talkingbooktopic from u_rb within w_xml_catalogs
end type
type cb_ok from commandbutton within w_xml_catalogs
end type
type cb_cancel from commandbutton within w_xml_catalogs
end type
type gb_catalogs from groupbox within w_xml_catalogs
end type
type gb_nl_catalogs from groupbox within w_xml_catalogs
end type
type gb_young_reader from groupbox within w_xml_catalogs
end type
end forward

global type w_xml_catalogs from w_response
integer width = 2062
integer height = 1804
string title = "Catalog Selection"
em_br_end_date em_br_end_date
st_5 st_5
em_br_start_date em_br_start_date
st_4 st_4
em_rc_end_date em_rc_end_date
st_3 st_3
em_rc_start_date em_rc_start_date
st_2 st_2
rb_nss rb_nss
st_1 st_1
rb_youngerreaders rb_youngerreaders
rb_braillebooks rb_braillebooks
rb_cassettebooks rb_cassettebooks
rb_braillebookreview rb_braillebookreview
rb_talkingbooktopic rb_talkingbooktopic
cb_ok cb_ok
cb_cancel cb_cancel
gb_catalogs gb_catalogs
gb_nl_catalogs gb_nl_catalogs
gb_young_reader gb_young_reader
end type
global w_xml_catalogs w_xml_catalogs

on w_xml_catalogs.create
int iCurrent
call super::create
this.em_br_end_date=create em_br_end_date
this.st_5=create st_5
this.em_br_start_date=create em_br_start_date
this.st_4=create st_4
this.em_rc_end_date=create em_rc_end_date
this.st_3=create st_3
this.em_rc_start_date=create em_rc_start_date
this.st_2=create st_2
this.rb_nss=create rb_nss
this.st_1=create st_1
this.rb_youngerreaders=create rb_youngerreaders
this.rb_braillebooks=create rb_braillebooks
this.rb_cassettebooks=create rb_cassettebooks
this.rb_braillebookreview=create rb_braillebookreview
this.rb_talkingbooktopic=create rb_talkingbooktopic
this.cb_ok=create cb_ok
this.cb_cancel=create cb_cancel
this.gb_catalogs=create gb_catalogs
this.gb_nl_catalogs=create gb_nl_catalogs
this.gb_young_reader=create gb_young_reader
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.em_br_end_date
this.Control[iCurrent+2]=this.st_5
this.Control[iCurrent+3]=this.em_br_start_date
this.Control[iCurrent+4]=this.st_4
this.Control[iCurrent+5]=this.em_rc_end_date
this.Control[iCurrent+6]=this.st_3
this.Control[iCurrent+7]=this.em_rc_start_date
this.Control[iCurrent+8]=this.st_2
this.Control[iCurrent+9]=this.rb_nss
this.Control[iCurrent+10]=this.st_1
this.Control[iCurrent+11]=this.rb_youngerreaders
this.Control[iCurrent+12]=this.rb_braillebooks
this.Control[iCurrent+13]=this.rb_cassettebooks
this.Control[iCurrent+14]=this.rb_braillebookreview
this.Control[iCurrent+15]=this.rb_talkingbooktopic
this.Control[iCurrent+16]=this.cb_ok
this.Control[iCurrent+17]=this.cb_cancel
this.Control[iCurrent+18]=this.gb_catalogs
this.Control[iCurrent+19]=this.gb_nl_catalogs
this.Control[iCurrent+20]=this.gb_young_reader
end on

on w_xml_catalogs.destroy
call super::destroy
destroy(this.em_br_end_date)
destroy(this.st_5)
destroy(this.em_br_start_date)
destroy(this.st_4)
destroy(this.em_rc_end_date)
destroy(this.st_3)
destroy(this.em_rc_start_date)
destroy(this.st_2)
destroy(this.rb_nss)
destroy(this.st_1)
destroy(this.rb_youngerreaders)
destroy(this.rb_braillebooks)
destroy(this.rb_cassettebooks)
destroy(this.rb_braillebookreview)
destroy(this.rb_talkingbooktopic)
destroy(this.cb_ok)
destroy(this.cb_cancel)
destroy(this.gb_catalogs)
destroy(this.gb_nl_catalogs)
destroy(this.gb_young_reader)
end on

event open;call super::open;//////////////////////////////////////////////////////////////////////////////////////////////////////////
//
//	Event:  OPEN
//
//	Description:
//	open
//
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//	
//	Revision History
//
//	Developed by 	Date 				Version						Tracking#
//									
// Murali K.			01/31/2008      002 PICS Modifications	 PICS PMS 2.0 Modifications
//																			PMS B.2, PMS B.2.1
//																Introduce flash books media
//																label changed for radio buttons												
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//
rb_talkingbooktopic.checked = TRUE

end event

type em_br_end_date from u_em within w_xml_catalogs
integer x = 1472
integer y = 1264
integer width = 370
integer height = 100
integer taborder = 70
integer textsize = -10
maskdatatype maskdatatype = datemask!
string mask = "mm/dd/yyyy"
boolean autoskip = true
string displaydata = ""
double increment = 0
string minmax = ""
end type

type st_5 from u_st within w_xml_catalogs
integer x = 983
integer y = 1280
integer width = 370
integer textsize = -10
string text = "BR End Date"
end type

type em_br_start_date from u_em within w_xml_catalogs
integer x = 571
integer y = 1248
integer width = 370
integer height = 100
integer taborder = 60
integer textsize = -10
maskdatatype maskdatatype = datemask!
string mask = "mm/dd/yyyy"
boolean autoskip = true
string displaydata = ""
double increment = 0
string minmax = ""
end type

type st_4 from u_st within w_xml_catalogs
integer x = 82
integer y = 1280
integer width = 384
integer textsize = -10
string text = "BR Start Date"
end type

type em_rc_end_date from u_em within w_xml_catalogs
integer x = 1472
integer y = 1140
integer width = 370
integer height = 100
integer taborder = 50
integer textsize = -10
maskdatatype maskdatatype = datemask!
string mask = "mm/dd/yyyy"
boolean autoskip = true
string displaydata = ""
double increment = 0
string minmax = ""
end type

type st_3 from u_st within w_xml_catalogs
integer x = 983
integer y = 1156
integer width = 475
integer textsize = -10
string text = "RC/DB End Date"
end type

type em_rc_start_date from u_em within w_xml_catalogs
integer x = 571
integer y = 1140
integer width = 370
integer height = 100
integer taborder = 40
integer textsize = -10
maskdatatype maskdatatype = datemask!
string mask = "mm/dd/yyyy"
boolean autoskip = true
string displaydata = ""
double increment = 0
string minmax = ""
end type

type st_2 from u_st within w_xml_catalogs
integer x = 82
integer y = 1156
integer width = 489
integer textsize = -10
string text = "RC/DB Start Date"
end type

type rb_nss from radiobutton within w_xml_catalogs
integer x = 503
integer y = 832
integer width = 1115
integer height = 100
integer textsize = -12
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 33554432
long backcolor = 80263581
string text = "Network Services Catalogs"
end type

event clicked;IF (rb_talkingbooktopic.Checked = TRUE OR &
	 rb_cassettebooks.Checked = TRUE OR &
	 rb_braillebookreview.Checked = TRUE OR &
	 rb_braillebooks.Checked = TRUE ) THEN
	
	rb_talkingbooktopic.Checked = FALSE 
	rb_cassettebooks.Checked = FALSE 
	rb_braillebookreview.Checked = FALSE 
	rb_braillebooks.Checked = FALSE
	
END IF

end event

type st_1 from statictext within w_xml_catalogs
integer x = 430
integer y = 36
integer width = 910
integer height = 80
integer textsize = -12
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 33554432
long backcolor = 80263581
string text = "Please Select a catalog"
boolean focusrectangle = false
end type

type rb_youngerreaders from u_rb within w_xml_catalogs
integer x = 503
integer y = 612
integer width = 1106
integer height = 80
integer textsize = -12
integer weight = 700
fontcharset fontcharset = ansi!
string text = "For Younger Readers"
end type

event clicked;call super::clicked;IF (rb_talkingbooktopic.Checked = TRUE OR &
	 rb_cassettebooks.Checked = TRUE OR &
	 rb_braillebookreview.Checked = TRUE OR &
	 rb_nss.Checked = TRUE OR &
	 rb_braillebooks.Checked = TRUE) THEN
	
	rb_talkingbooktopic.Checked = FALSE 
	rb_cassettebooks.Checked = FALSE 
	rb_braillebookreview.Checked = FALSE 
	rb_braillebooks.Checked = FALSE
	rb_nss.Checked = FALSE
END IF

end event

type rb_braillebooks from u_rb within w_xml_catalogs
integer x = 503
integer y = 516
integer width = 1106
integer height = 68
integer textsize = -12
integer weight = 700
fontcharset fontcharset = ansi!
string text = "Braille Books"
end type

event clicked;call super::clicked;IF (rb_talkingbooktopic.Checked = TRUE OR &
	 rb_cassettebooks.Checked = TRUE OR &
	 rb_braillebookreview.Checked = TRUE OR &
	 rb_nss.Checked = TRUE OR &	 
	 rb_youngerreaders.Checked = TRUE) THEN
	
	rb_talkingbooktopic.Checked = FALSE 
	rb_cassettebooks.Checked = FALSE 
	rb_braillebookreview.Checked = FALSE 
	rb_youngerreaders.Checked = FALSE
	rb_nss.Checked = FALSE	 
	
END IF

end event

type rb_cassettebooks from u_rb within w_xml_catalogs
integer x = 503
integer y = 416
integer width = 1326
integer height = 68
integer textsize = -12
integer weight = 700
fontcharset fontcharset = ansi!
string text = "Digital Talking Books Plus Books"
end type

event clicked;call super::clicked;IF (rb_talkingbooktopic.Checked = TRUE OR &
	 rb_braillebooks.Checked = TRUE OR &
	 rb_braillebookreview.Checked = TRUE OR &
	 rb_nss.Checked = TRUE OR &
	 rb_youngerreaders.Checked = TRUE) THEN
	
	rb_talkingbooktopic.Checked = FALSE 
	rb_braillebooks.Checked = FALSE 
	rb_braillebookreview.Checked = FALSE 
	rb_youngerreaders.Checked = FALSE
       rb_nss.Checked = FALSE
	
END IF

end event

type rb_braillebookreview from u_rb within w_xml_catalogs
integer x = 503
integer y = 320
integer width = 1106
integer height = 68
integer textsize = -12
integer weight = 700
fontcharset fontcharset = ansi!
string text = "Braille Book Review"
end type

event clicked;call super::clicked;IF (rb_talkingbooktopic.Checked = TRUE OR &
	 rb_braillebooks.Checked = TRUE OR &
	 rb_cassettebooks.Checked = TRUE OR &
	 rb_nss.Checked = TRUE OR &
	 rb_youngerreaders.Checked = TRUE) THEN
	
	rb_talkingbooktopic.Checked = FALSE 
	rb_braillebooks.Checked = FALSE 
	rb_cassettebooks.Checked = FALSE 
	rb_youngerreaders.Checked = FALSE
       rb_nss.Checked = FALSE
END IF

end event

type rb_talkingbooktopic from u_rb within w_xml_catalogs
integer x = 503
integer y = 212
integer width = 1106
integer height = 84
integer textsize = -12
integer weight = 700
fontcharset fontcharset = ansi!
string text = "Talking Book Topics"
end type

event clicked;call super::clicked;IF (rb_braillebookreview.Checked = TRUE OR &
	 rb_braillebooks.Checked = TRUE OR &
	 rb_cassettebooks.Checked = TRUE OR &
	 rb_nss.Checked = TRUE OR &
	 rb_youngerreaders.Checked = TRUE) THEN
	
	rb_braillebookreview.Checked = FALSE 
	rb_braillebooks.Checked = FALSE 
	rb_cassettebooks.Checked = FALSE 
	rb_youngerreaders.Checked = FALSE
       rb_nss.Checked = FALSE
	
END IF
end event

type cb_ok from commandbutton within w_xml_catalogs
integer x = 475
integer y = 1568
integer width = 402
integer height = 112
integer taborder = 80
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&Ok"
end type

event clicked;string ls_catalog
str_xml_export lstr_xml_export
date ld_rc_start, ld_rc_end, ld_br_start, ld_br_end

// Get start dates and end dates for younger readers
ld_rc_start = date(em_rc_start_date.Text)
ld_rc_end = date(em_rc_end_date.Text)
ld_br_start = date(em_br_start_date.Text)
ld_br_end = date(em_br_end_date.Text)

IF (rb_braillebookreview.Checked = TRUE) THEN
	ls_catalog = 'braillebookreview'
ELSEIF (rb_braillebooks.Checked = TRUE) THEN
	ls_catalog = 'braillebooks'
ELSEIF (rb_cassettebooks.Checked = TRUE) THEN
//	ls_catalog = 'cassettebooks'
	ls_catalog = 'dtbplus'
ELSEIF (rb_youngerreaders.Checked = TRUE) THEN
	ls_catalog = 'youngerreaders'
	IF ld_rc_start = date("1/1/1900") OR ld_rc_end = date("1/1/1900") OR ld_br_start = date("1/1/1900") OR  ld_br_end = date("1/1/1900") THEN
		MessageBox ("Error","The younger reader date criteria can not be null.")
		IF ld_rc_start = date("1/1/1900") THEN
			em_rc_start_date.SetFocus()
			RETURN
		ELSEIF ld_rc_end = date("1/1/1900") THEN
			em_rc_end_date.SetFocus()
			RETURN
		ELSEIF ld_br_start = date("1/1/1900") THEN
			em_br_start_date.SetFocus()
			RETURN
		ELSEIF ld_br_end = date("1/1/1900") THEN
			em_br_end_date.SetFocus()
			RETURN
		END IF
	END IF
ELSEIF (rb_talkingbooktopic.Checked = TRUE) THEN
	ls_catalog = 'talkingbooktopic'
ELSEIF (rb_nss.Checked = TRUE) THEN
	ls_catalog = 'nss'
ELSE
	ls_catalog = 'nocatalog'
END IF

lstr_xml_export.ls_catalog = ls_catalog
lstr_xml_export.ld_rc_stdate = ld_rc_start
lstr_xml_export.ld_rc_enddt = ld_rc_end
lstr_xml_export.ld_br_stdate = ld_br_start
lstr_xml_export.ld_br_enddt = ld_br_end

//MessageBox("dates",string(ld_rc_start) + "   " + string(ld_rc_end) + "  " + string(ld_br_start) + "  " + string(ld_br_end) + "  " + ls_catalog)

CloseWithReturn(Parent, lstr_xml_export)

//CloseWithReturn(Parent, ls_catalog)


end event

type cb_cancel from commandbutton within w_xml_catalogs
integer x = 951
integer y = 1568
integer width = 402
integer height = 112
integer taborder = 90
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&Cancel"
end type

event clicked;string ls_catalog
str_xml_export lstr_xml_export

ls_catalog = 'nocatalog'

lstr_xml_export.ls_catalog = ls_catalog

CloseWithReturn(Parent, lstr_xml_export)


end event

type gb_catalogs from groupbox within w_xml_catalogs
integer x = 37
integer y = 124
integer width = 1902
integer height = 612
integer taborder = 10
integer textsize = -12
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 33554432
long backcolor = 80263581
string text = "PMS Catalogs"
end type

type gb_nl_catalogs from groupbox within w_xml_catalogs
integer x = 37
integer y = 736
integer width = 1902
integer height = 288
integer taborder = 20
integer textsize = -12
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 33554432
long backcolor = 80263581
string text = "Network Services Section"
end type

type gb_young_reader from groupbox within w_xml_catalogs
integer x = 37
integer y = 1024
integer width = 1902
integer height = 384
integer taborder = 30
integer textsize = -12
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 33554432
long backcolor = 80263581
string text = "Younger Readers Criteria"
end type


$PBExportHeader$w_mm_selection_list.srw
forward
global type w_mm_selection_list from w_sheet
end type
type em_invno from uo_conno within w_mm_selection_list
end type
type st_3 from statictext within w_mm_selection_list
end type
type st_1 from statictext within w_mm_selection_list
end type
type st_2 from statictext within w_mm_selection_list
end type
type em_bkno from uo_conno within w_mm_selection_list
end type
type st_4 from statictext within w_mm_selection_list
end type
type em_magcd from uo_conno within w_mm_selection_list
end type
type st_5 from statictext within w_mm_selection_list
end type
type em_issdate from uo_date within w_mm_selection_list
end type
type cb_1 from commandbutton within w_mm_selection_list
end type
type cb_2 from commandbutton within w_mm_selection_list
end type
end forward

global type w_mm_selection_list from w_sheet
integer x = 553
integer y = 380
integer width = 1961
integer height = 988
string title = "Invoicing Selection List"
boolean minbox = false
boolean maxbox = false
boolean resizable = false
em_invno em_invno
st_3 st_3
st_1 st_1
st_2 st_2
em_bkno em_bkno
st_4 st_4
em_magcd em_magcd
st_5 st_5
em_issdate em_issdate
cb_1 cb_1
cb_2 cb_2
end type
global w_mm_selection_list w_mm_selection_list

type variables
long Lbkno
string Lmed,  Ls_magcode
date Ld_issuedate,Ld_admindate

end variables

forward prototypes
public function integer wf_check_magcode ()
end prototypes

public function integer wf_check_magcode ();String ls_title

SELECT magttl.title
INTO   :ls_title
FROM   magttl
WHERE  magttl.magcd = :Ls_magcode
USING  SqlServerTrans;

RETURN SqlServerTrans.SqlCode;
end function

event open;call super::open;SetNull(Ld_admindate)
end event

on w_mm_selection_list.create
int iCurrent
call super::create
this.em_invno=create em_invno
this.st_3=create st_3
this.st_1=create st_1
this.st_2=create st_2
this.em_bkno=create em_bkno
this.st_4=create st_4
this.em_magcd=create em_magcd
this.st_5=create st_5
this.em_issdate=create em_issdate
this.cb_1=create cb_1
this.cb_2=create cb_2
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.em_invno
this.Control[iCurrent+2]=this.st_3
this.Control[iCurrent+3]=this.st_1
this.Control[iCurrent+4]=this.st_2
this.Control[iCurrent+5]=this.em_bkno
this.Control[iCurrent+6]=this.st_4
this.Control[iCurrent+7]=this.em_magcd
this.Control[iCurrent+8]=this.st_5
this.Control[iCurrent+9]=this.em_issdate
this.Control[iCurrent+10]=this.cb_1
this.Control[iCurrent+11]=this.cb_2
end on

on w_mm_selection_list.destroy
call super::destroy
destroy(this.em_invno)
destroy(this.st_3)
destroy(this.st_1)
destroy(this.st_2)
destroy(this.em_bkno)
destroy(this.st_4)
destroy(this.em_magcd)
destroy(this.st_5)
destroy(this.em_issdate)
destroy(this.cb_1)
destroy(this.cb_2)
end on

type em_invno from uo_conno within w_mm_selection_list
integer x = 1097
integer y = 576
integer width = 439
integer height = 96
integer taborder = 40
integer textsize = -10
textcase textcase = upper!
maskdatatype maskdatatype = stringmask!
string mask = ""
string displaydata = "~t/"
end type

type st_3 from statictext within w_mm_selection_list
integer x = 183
integer y = 576
integer width = 914
integer height = 96
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long backcolor = 79741120
string text = "Book/Magazine Invoice Number:"
boolean focusrectangle = false
end type

type st_1 from statictext within w_mm_selection_list
integer x = 146
integer y = 32
integer width = 1646
integer height = 160
integer textsize = -10
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long backcolor = 79741120
boolean enabled = false
string text = "Please select a book number or Magazine code/Issue date or Book/Magazine Invoice number."
alignment alignment = center!
boolean focusrectangle = false
end type

type st_2 from statictext within w_mm_selection_list
integer x = 485
integer y = 288
integer width = 421
integer height = 76
integer textsize = -10
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long backcolor = 79741120
boolean enabled = false
string text = "Book Number:"
boolean focusrectangle = false
end type

type em_bkno from uo_conno within w_mm_selection_list
event modified pbm_enmodified
integer x = 914
integer y = 276
integer width = 361
integer height = 100
integer taborder = 10
integer textsize = -10
string mask = "#######"
end type

type st_4 from statictext within w_mm_selection_list
integer x = 55
integer y = 432
integer width = 466
integer height = 76
integer textsize = -10
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long backcolor = 79741120
boolean enabled = false
string text = "Magazine Code:"
boolean focusrectangle = false
end type

type em_magcd from uo_conno within w_mm_selection_list
integer x = 526
integer y = 412
integer width = 315
integer height = 104
integer taborder = 20
integer textsize = -10
textcase textcase = upper!
maskdatatype maskdatatype = stringmask!
string mask = "aaaa"
boolean autoskip = true
string displaydata = "Ä"
end type

type st_5 from statictext within w_mm_selection_list
integer x = 933
integer y = 420
integer width = 384
integer height = 76
integer textsize = -10
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long backcolor = 79741120
boolean enabled = false
string text = "Issue Date:"
boolean focusrectangle = false
end type

type em_issdate from uo_date within w_mm_selection_list
integer x = 1335
integer y = 408
integer width = 347
integer height = 108
integer taborder = 30
borderstyle borderstyle = stylelowered!
string displaydata = "Ä"
end type

type cb_1 from commandbutton within w_mm_selection_list
event clicked pbm_bnclicked
integer x = 576
integer y = 736
integer width = 311
integer height = 108
integer taborder = 50
integer textsize = -10
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "&OK"
boolean default = true
end type

event clicked;DateTime    ldT_shipdate, ldt_issdt
String ls_message,ls_msgparm[1]

IF (em_magcd.enabled AND em_bkno.text = "" AND em_invno.text="") THEN
	ls_magcode = Trim(em_magcd.text)
	ld_issuedate = Date(em_issdate.text)
	ldt_issdt=DateTime(ld_issuedate,Time('00:00:00'))
	IF ls_magcode <> '' THEN
		IF wf_check_magcode() = 100 THEN
	 		Messagebox('Status','Magazine Code Does Not Exist')
			em_magcd.SetFocus() 
			em_magcd.SelectText(1,Len(em_issdate.text))
			RETURN
		END IF//IF wf_check_magcode(Ls_magcode) = 100 THEN
	ELSE	
		Messagebox('Status','Need Magazine Code Input')
		em_issdate.text = '' 
		em_magcd.SetFocus()
		RETURN
	END IF//IF TRIM(Ls_magcode) <> '' THEN

  	SELECT magiss.shipdt
    INTO :ldT_shipdate 
    FROM magcntr,   
         magiss  
   where magcntr.fy   = magiss.fy AND  
			magcntr.cntr = magiss.cntr AND
         magiss.magcd = :ls_magcode  AND  
			magiss.issdt = :ldt_issdt 
	USING SqlServerTrans;
	
	IF SqlServerTrans.sqlCode = 0 THEN
  		IF IsNull(ldT_shipdate) THEN
	 		Messagebox('Status','Not Allowed To Invoice an Issue ~r~n That Has Not Been Shipped')
	 		em_issdate.SetFocus()
	 		RETURN
  		ELSE	 
	 		OpenSheet(w_magazine_invoice_tracking, w_pics_main, 0, original!)
  		END IF//IF IsNull(ld_shipdate) THEN
	ELSEIF SqlServerTrans.sqlCode = 100 THEN
		Messagebox('Status','No Data Found. Enter New Issue Or Enter New Query')
		em_issdate.SetFocus()
		em_issdate.SelectText(1,Len(em_issdate.text))
		RETURN
	ELSEIF SqlServerTrans.sqlCode = -1 THEN
		ls_message = "A database error has occurred in selecting from magiss table.~n" + &
						 "Database error code:  " + String (SqlServerTrans.sqlDbCode) + "~r~n" + &
						 "Database error message:~r~n" + SqlServerTrans.sqlErrText
		IF IsValid(gnv_app.inv_error) THEN
			ls_msgparm[1] = ls_message
			gnv_app.inv_error.of_Message ("pfc_dwdberror", ls_msgparm[1], stopSign!, ok!, 1, 100, TRUE, TRUE)		
		ELSE
			Messagebox (gnv_app.iapp_object.displayName, ls_message, stopSign!, ok!)
			ROLLBACK USING SqlServerTrans;
			RETURN -1
		END IF
   	RETURN	
	END IF//IF wf_does_mag_exist(Ls_magcode, Ld_issuedate, ld_shipdate) = 100 THEN
ELSEIF (em_bkno.text <> "" AND em_magcd.text = "" AND em_invno.text = "") THEN
	Lbkno = Long(em_bkno.text)
	OpenSheet(w_sheet_mm_bk_new_invoicing, w_pics_main, 0, original!)
	em_magcd.enabled=TRUE
	em_issdate.enabled=TRUE
ELSEIF (em_bkno.text = "" AND em_magcd.text = "" AND em_invno.text <> "") THEN
	
//	MessageBox("selection","You have selected an invoice number.")
//	OpenWithParm(w_view_invoices, em_invno.text)
//	OpenSheetWithParm(w_view_invoices, em_invno.text, w_pics_main, 0, original!)
	integer li_return
	try
		 li_return = OpenSheetWithParm(w_view_invoices, em_invno.text, w_pics_main, 0, original!)
		if IsNull(li_return) then
				 MessageBox ("Failure", "Null argument provided")
		end if
	catch (runtimeerror rt)	
		Messagebox("Failure", "Sheet open failed. "+ rt.getmessage()) //Handle the error
	end try
	
ELSEIF (em_bkno.text <> "" AND em_magcd.text <> "" AND em_invno.text <> "") OR &
		(em_bkno.text = "" AND em_magcd.text = "" AND em_invno.text = "") THEN
		
	Messagebox("Error", "Select a book number or a magazine code or a invoice number")
	em_bkno.SetFocus()

ELSEIF (em_bkno.text <> "" AND em_magcd.text = "" AND em_invno.text <> "") THEN
		
	Messagebox("Error", "Select a book number or a magazine code or a invoice number")
	em_bkno.SetFocus()
END IF//IF em_magcd.enabled THEN

















end event

type cb_2 from commandbutton within w_mm_selection_list
event clicked pbm_bnclicked
integer x = 1056
integer y = 736
integer width = 329
integer height = 108
integer taborder = 60
integer textsize = -10
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "&Cancel"
end type

event clicked;close(parent)
end event


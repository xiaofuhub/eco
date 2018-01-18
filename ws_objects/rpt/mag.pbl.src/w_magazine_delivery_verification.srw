$PBExportHeader$w_magazine_delivery_verification.srw
forward
global type w_magazine_delivery_verification from w_sheet
end type
type dw_delivery_verification from u_dw within w_magazine_delivery_verification
end type
type em_fy from u_em within w_magazine_delivery_verification
end type
type em_cutoff from u_em within w_magazine_delivery_verification
end type
type em_msc from u_em within w_magazine_delivery_verification
end type
type cb_update from u_cb within w_magazine_delivery_verification
end type
type cb_clear from u_cb within w_magazine_delivery_verification
end type
type cb_exit from u_cb within w_magazine_delivery_verification
end type
type st_1 from u_st within w_magazine_delivery_verification
end type
type st_2 from u_st within w_magazine_delivery_verification
end type
type st_3 from u_st within w_magazine_delivery_verification
end type
type sle_rowsretrieved from u_sle within w_magazine_delivery_verification
end type
type st_4 from u_st within w_magazine_delivery_verification
end type
end forward

shared variables

end variables

global type w_magazine_delivery_verification from w_sheet
integer width = 2967
integer height = 1588
string title = "Magazine Shipment Verification"
dw_delivery_verification dw_delivery_verification
em_fy em_fy
em_cutoff em_cutoff
em_msc em_msc
cb_update cb_update
cb_clear cb_clear
cb_exit cb_exit
st_1 st_1
st_2 st_2
st_3 st_3
sle_rowsretrieved sle_rowsretrieved
st_4 st_4
end type
global w_magazine_delivery_verification w_magazine_delivery_verification

type variables
Long ll_get_row
end variables

forward prototypes
public subroutine wf_build_sql_for_cutoffdt (string original_select, string ls_cutoff)
public subroutine wf_build_sql_for_msc (string original_select)
public subroutine wf_build_sql_for_msc_cutoff (string original_select)
public subroutine wf_disable_objects ()
public subroutine wf_reset_sql_query (string original_select)
end prototypes

public subroutine wf_build_sql_for_cutoffdt (string original_select, string ls_cutoff);String rc, mod_string, where_clause, ls_newcutoff,ls_part, ls_mon, ls_day, ls_yr

ls_part=mid(ls_cutoff,1,2)
choose case ls_part
	case '01'
		ls_mon='Jan'
	case '02'
		ls_mon='Feb'
	case '03'
		ls_mon='Mar'
	case '04'
		ls_mon='Apr'	
	case '05'
		ls_mon='May'
	case '06'
		ls_mon='Jun'
	case '07'
		ls_mon='Jul'
	case '08'
		ls_mon='Aug'
	case '09'
		ls_mon='Sep'
	case '10'
		ls_mon='Oct'
	case '11'
		ls_mon='Nov'
	case '12'
		ls_mon='Dec'	
end choose
ls_day=mid(ls_cutoff,4,2)
ls_yr=mid(ls_cutoff,7,4)
ls_newcutoff=ls_day+'-'+ls_mon+'-'+ls_yr
//where_clause = " AND magiss.shipdt <= " + " ~""+ls_cutoff + " ~""
//
//mod_string   = "DataWindow.Table.Select='"+ original_select + where_clause+"'"
where_clause = " AND magiss.shipdt <= " + " ~~'"+ls_newcutoff + " ~~'"

mod_string   = "DataWindow.Table.Select='"+ original_select + where_clause+"'"
messagebox('modify string for cut of date',mod_string)


rc = dw_delivery_verification.Modify(mod_string)

IF rc <> "" THEN
 MessageBox("Status","Modify Failed" +rc)
END IF

end subroutine

public subroutine wf_build_sql_for_msc (string original_select);String rc, mod_string, where_clause, ls_msc, order_by

ls_msc = em_msc.text
order_by = "ORDER BY magcntr.format, magiss.magcd, magiss.issdt"

//where_clause = " AND magttl.msc = "+ "~"" + ls_msc + "~""
//mod_string = "DataWindow.Table.Select='"+original_select+where_clause+' '+Order_by + "'"

where_clause = " AND magttl.msc = "+ "~~'" + ls_msc + "~~'"
mod_string = "DataWindow.Table.Select='"+original_select+where_clause+' '+Order_by + "'"
//messagebox('modify string',mod_string)
rc = dw_delivery_verification.Modify(mod_string)

IF rc <> "" THEN
	MessageBox('Status',"Modify Failed "+rc)	
END IF


 
end subroutine

public subroutine wf_build_sql_for_msc_cutoff (string original_select);String rc, mod_string, where_clause, ls_msc, order_by, ls_cutoff, &
		ls_newcutoff,ls_part, ls_mon, ls_day, ls_yr

ls_msc = em_msc.text
ls_cutoff = em_cutoff.text
ls_part=mid(ls_cutoff,1,2)
choose case ls_part
	case '01'
		ls_mon='Jan'
	case '02'
		ls_mon='Feb'
	case '03'
		ls_mon='Mar'
	case '04'
		ls_mon='Apr'	
	case '05'
		ls_mon='May'
	case '06'
		ls_mon='Jun'
	case '07'
		ls_mon='Jul'
	case '08'
		ls_mon='Aug'
	case '09'
		ls_mon='Sep'
	case '10'
		ls_mon='Oct'
	case '11'
		ls_mon='Nov'
	case '12'
		ls_mon='Dec'	
end choose
ls_day=mid(ls_cutoff,4,2)
ls_yr=mid(ls_cutoff,7,4)
ls_newcutoff=ls_day+'-'+ls_mon+'-'+ls_yr
order_by = "ORDER BY magcntr.format, magiss.magcd, magiss.issdt"

where_clause = " AND magttl.msc = "+ "~~'" + ls_msc + "~~'" +" AND magiss.shipdt <= " + " ~~'"+ls_newcutoff + " ~~'"
mod_string = "DataWindow.Table.Select='"+original_select+where_clause+' '+Order_by + "'"
//messagebox('modify string for cut of date',mod_string)
rc = dw_delivery_verification.Modify(mod_string)

IF rc <> "" THEN
	MessageBox('Status',"Modify Failed "+rc)	
END IF




end subroutine

public subroutine wf_disable_objects ();cb_clear.Enabled =  TRUE
cb_exit.Enabled  =  TRUE
cb_update.Enabled = TRUE


dw_delivery_verification.Enabled = TRUE

em_cutoff.DisplayOnly = TRUE
em_fy.DisplayOnly = TRUE
em_msc.DisplayOnly = TRUE


end subroutine

public subroutine wf_reset_sql_query (string original_select);String rc, mod_string

mod_string = "DataWindow.Table.Select='" + original_select+"'"

rc = dw_delivery_verification.Modify(mod_string)


IF rc <> "" THEN
	MessageBox('Status',"Modify Failed" + rc)
END IF
	
end subroutine

event open;call super::open;
//Disable clear, update and datawindow and set focus on the fiscal year.
cb_clear.Enabled =  FALSE
cb_update.Enabled = FALSE
dw_delivery_verification.Enabled = FALSE

//disable the addrow and deleterow menu functions
m_pics_main.m_edit.m_deleterow.Enabled 	=	FALSE
m_pics_main.m_edit.m_addrow.Enabled			=  FALSE

//Set focus to the edit mask and set the microhelp
em_fy.SetFocus()
w_pics_main.Event pfc_microhelp("Enter Fiscal Year")
end event

on w_magazine_delivery_verification.create
int iCurrent
call super::create
this.dw_delivery_verification=create dw_delivery_verification
this.em_fy=create em_fy
this.em_cutoff=create em_cutoff
this.em_msc=create em_msc
this.cb_update=create cb_update
this.cb_clear=create cb_clear
this.cb_exit=create cb_exit
this.st_1=create st_1
this.st_2=create st_2
this.st_3=create st_3
this.sle_rowsretrieved=create sle_rowsretrieved
this.st_4=create st_4
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_delivery_verification
this.Control[iCurrent+2]=this.em_fy
this.Control[iCurrent+3]=this.em_cutoff
this.Control[iCurrent+4]=this.em_msc
this.Control[iCurrent+5]=this.cb_update
this.Control[iCurrent+6]=this.cb_clear
this.Control[iCurrent+7]=this.cb_exit
this.Control[iCurrent+8]=this.st_1
this.Control[iCurrent+9]=this.st_2
this.Control[iCurrent+10]=this.st_3
this.Control[iCurrent+11]=this.sle_rowsretrieved
this.Control[iCurrent+12]=this.st_4
end on

on w_magazine_delivery_verification.destroy
call super::destroy
destroy(this.dw_delivery_verification)
destroy(this.em_fy)
destroy(this.em_cutoff)
destroy(this.em_msc)
destroy(this.cb_update)
destroy(this.cb_clear)
destroy(this.cb_exit)
destroy(this.st_1)
destroy(this.st_2)
destroy(this.st_3)
destroy(this.sle_rowsretrieved)
destroy(this.st_4)
end on

event pfc_preopen;call super::pfc_preopen;this.of_SetPreference(TRUE)
this.inv_preference.of_SetToolBars(TRUE)
this.inv_preference.of_SetWindow(TRUE)
this.of_SetResize(TRUE)
this.inv_resize.of_SetOrigSize(this.workspacewidth(),this.workspaceheight())

inv_resize.of_Register(cb_clear, "scale")
inv_resize.of_Register(cb_exit, "scale")
inv_resize.of_Register(cb_update, "scale")
inv_resize.of_Register(dw_delivery_verification, "scale")
inv_resize.of_Register(em_cutoff, "scale")
inv_resize.of_Register(em_msc, "scale")
inv_resize.of_Register(st_4, "scale")
inv_resize.of_Register(sle_rowsretrieved, "scale")



inv_resize.of_Register(st_1, "scale")
inv_resize.of_Register(st_2, "scale")
inv_resize.of_Register(st_3, "scale")


end event

event resize;call super::resize;long ll_height
This.X = w_pics_main.X
This.Y = w_pics_main.Y
ll_height = w_pics_main.mdi_1.Height
This.Resize(w_pics_main.WorkSpaceWidth(), ll_height)
end event

event mousemove;call super::mousemove;w_pics_main.setmicrohelp('Ready')
end event

type dw_delivery_verification from u_dw within w_magazine_delivery_verification
integer x = 151
integer y = 220
integer width = 2510
integer height = 1000
integer taborder = 40
string dataobject = "d_magazine_delivery_verification"
end type

event constructor;call super::constructor;SetTransObject(SqlServerTrans)
dw_delivery_verification.Event pfc_addrow()
ib_rmbmenu = FALSE                   //disable right mouse button

end event

event itemfocuschanged;call super::itemfocuschanged;//If focus then highlight and set microhelp

IF DWO.Name = "magiss_dlvrdt" THEN
	dw_delivery_verification.Event pfc_selectall()
	w_pics_main.Event pfc_microhelp("Enter Magazine Delivery Date")
END IF
end event

event retrievestart;call super::retrievestart;w_pics_main.Event pfc_microhelp("Retrieving Data Please Wait ...")
end event

event retrieveend;call super::retrieveend;sle_rowsretrieved.text = String(rowcount)
end event

event updateend;call super::updateend;Long ll_row_pointer

//Output message if update successfull and set focus to the row that the cursor was before
//the update button was clicked. The variable ll_get_row is an instance variable
//that was initialized in the update button clicked event.
w_pics_main.Event pfc_microhelp('Row(s) Updated Successfully')


IF NOT(IsNull(dw_delivery_verification.GetItemDatetime(ll_get_row,"magiss_dlvrdt"))) THEN
   ll_row_pointer = ll_get_row + 1
ELSE
   ll_row_pointer = ll_get_row
END IF

IF ll_row_pointer >= Integer(sle_rowsretrieved.text) THEN
   ll_row_pointer = Long(sle_rowsretrieved.text)
END IF



	dw_delivery_verification.SetFocus()
	dw_delivery_verification.SetRow( ll_row_pointer)
	dw_delivery_verification.SetColumn("magiss_dlvrdt")

end event

type em_fy from u_em within w_magazine_delivery_verification
event ue_hinttext pbm_mousemove
string tag = "Enter Fiscal Year"
integer x = 457
integer y = 72
integer width = 229
integer height = 104
integer taborder = 10
integer textsize = -10
string text = "0000"
alignment alignment = center!
textcase textcase = upper!
maskdatatype maskdatatype = datemask!
string mask = "yyyy"
boolean autoskip = true
string displaydata = "H"
double increment = 0
string minmax = ""
end type

event ue_hinttext;call super::ue_hinttext;w_pics_main.setmicrohelp(this.tag)
end event

event modified;call super::modified;cb_clear.Enabled = True

end event

event getfocus;call super::getfocus;em_fy.text = '0000'
w_pics_main.Event pfc_microhelp("Enter Fiscal Year")
em_fy.Event pfc_selectall()
end event

type em_cutoff from u_em within w_magazine_delivery_verification
event ue_key pbm_keydown
event ue_hinttext pbm_mousemove
string tag = "Enter Cut Off Date"
integer x = 1321
integer y = 72
integer width = 366
integer height = 108
integer taborder = 20
integer textsize = -10
alignment alignment = center!
maskdatatype maskdatatype = datemask!
string mask = "mm/dd/yyyy"
string displaydata = ","
double increment = 0
string minmax = ""
end type

event ue_key;call super::ue_key;

IF KeyDown(KeyEnter!) THEN

	  em_msc.SetFocus()
	
END IF
end event

event ue_hinttext;call super::ue_hinttext;w_pics_main.setmicrohelp(this.tag)
end event

event getfocus;call super::getfocus;
w_pics_main.Event pfc_microhelp("Enter Cut Off Date")
end event

type em_msc from u_em within w_magazine_delivery_verification
event ue_key pbm_keydown
event ue_hinttext pbm_mousemove
string tag = "Enter Mult State Center (East - E OR West - W)"
integer x = 2331
integer y = 72
integer width = 229
integer height = 100
integer taborder = 30
integer textsize = -10
alignment alignment = center!
textcase textcase = upper!
maskdatatype maskdatatype = stringmask!
string mask = "!"
string displaydata = ""
double increment = 0
string minmax = ""
end type

event ue_key;call super::ue_key;Integer li_fy
Date    ld_cut_off_date
String  ls_cutoff, original_select, where_clause, mod_string,rc,order_by


ls_cutoff = em_cutoff.text
li_fy = Integer(em_fy.text)
original_select = dw_delivery_verification.Describe("DataWindow.Table.Select")

IF KeyDown(KeyEnter!) THEN
	
 IF em_msc.text = "" OR IsNull(em_msc.text) THEN
	
		 IF IsNull(ls_cutoff) OR  ls_cutoff = "" OR ls_cutoff = '00/00/0000' THEN
						
			  IF dw_delivery_verification.Retrieve(li_fy) <= 0 THEN
				 MessageBox('Error','No Data Found .. Enter New Query')
				 cb_clear.TriggerEvent(clicked!)
			  ELSE
				   wf_disable_objects()
				  dw_delivery_verification.SetFocus()
			  END IF//IF dw_delivery_verification.Retrieve(li_fy) <= 0 THEN
				
		 ELSE		
			  wf_build_sql_for_cutoffdt(original_select,ls_cutoff)
			  	
				  IF dw_delivery_verification.Retrieve(li_fy) <= 0 THEN
					MessageBox('Error','No Data Found .. Enter New Query')
					cb_clear.TriggerEvent(clicked!)
				  ELSE
					wf_disable_objects()
					dw_delivery_verification.SetFocus()
				  END IF//IF dw_delivery_verification.Retrieve(li_fy) <= 0 THEN
			  wf_reset_sql_query(original_select)		 
		 END IF//IF IsNull(ls_cutoff) OR  ls_cutoff = "" OR ls_cutoff = '00/00/0000' THEN
		 
  ELSE // IF em_msc.text <> ""	 
  		 IF IsNull(ls_cutoff) OR  ls_cutoff = "" OR ls_cutoff = '00/00/0000' THEN
				wf_build_sql_for_msc(original_select)
			 IF dw_delivery_verification.Retrieve(li_fy)  <= 0 THEN
				MessageBox('Error','No Data Found .. Enter New Query')
				cb_clear.TriggerEvent(clicked!)
			 ELSE
				wf_disable_objects()
				dw_delivery_verification.SetFocus()
			 END IF	
			    wf_reset_sql_query(original_select)	
		 ELSE	// IF cutoff date not null 
		 	wf_build_sql_for_msc_cutoff(original_select)	
			IF	dw_delivery_verification.Retrieve(li_fy)  <= 0 THEN
				MessageBox('Error','No Data Found .. Enter New Query')
				cb_clear.TriggerEvent(clicked!)
			 ELSE
				wf_disable_objects()
				dw_delivery_verification.SetFocus()
			 END IF	
			    wf_reset_sql_query(original_select)	
				 
       END IF // IF IsNull(ls_cutoff) OR  ls_cutoff = "" OR ls_cutoff = '00/00/0000' THEN
  END IF//IF em_msc.text = "" OR IsNull(em_msc.text) THEN
END IF //IF KeyDown(KeyEnter!) THEN

end event

event ue_hinttext;call super::ue_hinttext;w_pics_main.setmicrohelp(this.tag)
end event

event getfocus;call super::getfocus;w_pics_main.Event pfc_microhelp("Enter Multi State Center")
end event

type cb_update from u_cb within w_magazine_delivery_verification
event ue_hinttext pbm_mousemove
string tag = "Updates the Database"
integer x = 1294
integer y = 1264
integer taborder = 0
integer textsize = -10
string text = "&Update"
end type

event ue_hinttext;call super::ue_hinttext;w_pics_main.setmicrohelp(this.tag)
end event

event clicked;call super::clicked;Integer li_rtn


dw_delivery_verification.AcceptText()

//Set the instance variable to the current row. This will be used in the 
//update end event of the datawindow.
ll_get_row = dw_delivery_verification.GetRow()


//If Row has been modified then update the datawindow
IF dw_delivery_verification.ModifiedCount() > 0 THEN
			
   li_rtn = dw_delivery_verification.Update( ) 

	IF li_rtn = 1 THEN
		COMMIT USING SqlServerTrans;
	ELSE
		ROLLBACK USING SqlServerTrans;
		MessageBox('data','Update Failed Contact Your DBA ')
	END IF
ELSE//output message .. no data has been modified. Set row focus.
	MessageBox('Status','No Dates Have Been Input .. No Update Performed')
   dw_delivery_verification.SetFocus()
	dw_delivery_verification.SetRow(ll_get_row)

END IF //IF dw_delivery_verification.ModifiedCount() > 0 THEN
end event

type cb_clear from u_cb within w_magazine_delivery_verification
event ue_hinttext pbm_mousemove
string tag = "Clears the screen for input"
integer x = 1751
integer y = 1264
integer taborder = 0
integer textsize = -10
string text = "&Clear"
end type

event ue_hinttext;call super::ue_hinttext;w_pics_main.setmicrohelp(this.tag)
end event

event clicked;call super::clicked;

em_fy.text = ""
em_cutoff.text = ""
em_msc.text = ""
sle_rowsretrieved.text = ""

cb_clear.Enabled = FALSE
cb_update.Enabled = FALSE


em_cutoff.DisplayOnly = FALSE
em_fy.DisplayOnly = FALSE
em_msc.DisplayOnly = FALSE


dw_delivery_verification.Reset()
dw_delivery_verification.Event pfc_addrow()
dw_delivery_verification.Enabled = FALSE

em_fy.SetFocus()
end event

type cb_exit from u_cb within w_magazine_delivery_verification
event ue_hinttext pbm_mousemove
string tag = "Exits the screen"
integer x = 2272
integer y = 1264
integer taborder = 0
integer textsize = -10
string text = "E&xit"
end type

event ue_hinttext;call super::ue_hinttext;w_pics_main.setmicrohelp(this.tag)
end event

event clicked;call super::clicked;Integer li_ans

IF dw_delivery_verification.ModifiedCount() > 0 THEN
  li_ans = MessageBox('Update','Do you want to Save changes',Question!,YesNo!)
 
	IF li_ans = 1 THEN
		  cb_update.Event clicked()
		  dw_delivery_verification.Reset()
		  Close(w_magazine_delivery_verification)
		  m_pics_main.m_menu.PopMenu ( 300, 0 ) 
	ELSE
		  dw_delivery_verification.Reset()
		  Close(w_magazine_delivery_verification)
		  m_pics_main.m_menu.PopMenu ( 300, 0 ) 
	END IF//IF li_ans = 1 THEN
ELSE
	dw_delivery_verification.Reset()
	Close(w_magazine_delivery_verification)
	m_pics_main.m_menu.PopMenu ( 300, 0 ) 
END IF//IF dw_delivery_verification.ModifiedCount() > 0 THEN

end event

type st_1 from u_st within w_magazine_delivery_verification
integer x = 46
integer y = 72
integer width = 343
integer textsize = -10
string text = "Fiscal Year"
alignment alignment = center!
end type

type st_2 from u_st within w_magazine_delivery_verification
integer x = 827
integer y = 80
integer width = 389
integer textsize = -10
string text = "Cut Off Date"
alignment alignment = center!
end type

type st_3 from u_st within w_magazine_delivery_verification
integer x = 1979
integer y = 84
integer width = 251
integer textsize = -10
string text = "MSC"
alignment alignment = center!
end type

type sle_rowsretrieved from u_sle within w_magazine_delivery_verification
integer x = 823
integer y = 1308
integer width = 197
integer height = 84
integer taborder = 2
integer textsize = -10
long textcolor = 255
boolean displayonly = true
end type

type st_4 from u_st within w_magazine_delivery_verification
integer x = 37
integer y = 1308
integer width = 773
integer textsize = -10
string text = "Number Of Rows Retrieved:"
end type


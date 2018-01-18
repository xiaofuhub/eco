$PBExportHeader$w_magazine_issue_delete.srw
forward
global type w_magazine_issue_delete from w_sheet
end type
type dw_magazine_issue_delete from u_dw within w_magazine_issue_delete
end type
type cb_delete from commandbutton within w_magazine_issue_delete
end type
type cb_clear from commandbutton within w_magazine_issue_delete
end type
type cb_exit from commandbutton within w_magazine_issue_delete
end type
type sle_producer from u_sle within w_magazine_issue_delete
end type
type sle_format from u_sle within w_magazine_issue_delete
end type
type sle_status from u_sle within w_magazine_issue_delete
end type
type st_1 from u_st within w_magazine_issue_delete
end type
type st_2 from u_st within w_magazine_issue_delete
end type
type st_3 from u_st within w_magazine_issue_delete
end type
type dw_magazine_issue_delete_magcd from u_dw within w_magazine_issue_delete
end type
type dw_magazine_issue_delete_rc from u_dw within w_magazine_issue_delete
end type
type em_issue_date from u_em within w_magazine_issue_delete
end type
type st_4 from u_st within w_magazine_issue_delete
end type
end forward

global type w_magazine_issue_delete from w_sheet
integer width = 3067
integer height = 1584
string title = "Magazine Issue Deletion"
dw_magazine_issue_delete dw_magazine_issue_delete
cb_delete cb_delete
cb_clear cb_clear
cb_exit cb_exit
sle_producer sle_producer
sle_format sle_format
sle_status sle_status
st_1 st_1
st_2 st_2
st_3 st_3
dw_magazine_issue_delete_magcd dw_magazine_issue_delete_magcd
dw_magazine_issue_delete_rc dw_magazine_issue_delete_rc
em_issue_date em_issue_date
st_4 st_4
end type
global w_magazine_issue_delete w_magazine_issue_delete

forward prototypes
public function integer wf_check_if_ext (string ls_magcode, date ld_issuedate)
public function integer wf_check_if_invoiced (string ls_magcode, date ld_issuedate)
public function integer wf_check_issuedate (string ls_magcode, date ld_issuedate)
public function integer wf_get_contract_information (string ls_magcode, integer li_fy, ref string ls_producer, ref string ls_format, ref string ls_status, ref string ls_contract)
public function boolean wf_is_date_empty (string ls_date)
end prototypes

public function integer wf_check_if_ext (string ls_magcode, date ld_issuedate);String ls_magcd


SELECT magcd
INTO   :ls_magcd
FROM   magext
WHERE  magcd  = :ls_magcode  AND
		 issdt  = :ld_issuedate
USING  SqlServerTrans;


Return SqlServerTrans.SqlCode
       


end function

public function integer wf_check_if_invoiced (string ls_magcode, date ld_issuedate);String ls_temp

SELECT magcd
INTO   :ls_temp
FROM   maginv
WHERE  maginv.magcd = :ls_magcode AND
		 maginv.issdt  = :ld_issuedate
USING  SqlServerTrans;



RETURN SqlServerTrans.SqlCode;



	


end function

public function integer wf_check_issuedate (string ls_magcode, date ld_issuedate);String ls_code


SELECT magiss.magcd
INTO   :ls_code
FROM	 magiss
WHERE  magiss.magcd = :ls_magcode AND
		 magiss.issdt = :ld_issuedate
USING SqlServerTrans;





RETURN SqlServerTrans.SqlCode
end function

public function integer wf_get_contract_information (string ls_magcode, integer li_fy, ref string ls_producer, ref string ls_format, ref string ls_status, ref string ls_contract);//Get contract information. The producer, format, etc will be
//sent back to the calling screen.


SELECT magcntr.prdr,magcntr.format,mag.magst,mag.cntr
INTO   :ls_producer,
		 :ls_format,
		 :ls_status,
		 :ls_contract
FROM	 mag,magcntr
WHERE  mag.fy = :li_fy  AND
		 mag.magcd = :ls_magcode AND
		 mag.magst = 'A' AND
		 mag.fy = magcntr.fy AND
		 mag.cntr = magcntr.cntr
USING SqlServerTrans;


Return SqlServerTrans.SqlCode
end function

public function boolean wf_is_date_empty (string ls_date);
ls_date = left(ls_date,4)

IF ls_date = "" THEN
	RETURN TRUE
ELSE
	RETURN FALSE
END IF
end function

event open;call super::open;cb_delete.Enabled = FALSE

//Disable the addrow menu option
m_pics_main.m_edit.m_deleterow.Enabled 	=	FALSE
m_pics_main.m_edit.m_addrow.Enabled 		=	FALSE

//Setfocus to the first datawindow
dw_magazine_issue_delete_magcd.SetFocus()
end event

on w_magazine_issue_delete.create
int iCurrent
call super::create
this.dw_magazine_issue_delete=create dw_magazine_issue_delete
this.cb_delete=create cb_delete
this.cb_clear=create cb_clear
this.cb_exit=create cb_exit
this.sle_producer=create sle_producer
this.sle_format=create sle_format
this.sle_status=create sle_status
this.st_1=create st_1
this.st_2=create st_2
this.st_3=create st_3
this.dw_magazine_issue_delete_magcd=create dw_magazine_issue_delete_magcd
this.dw_magazine_issue_delete_rc=create dw_magazine_issue_delete_rc
this.em_issue_date=create em_issue_date
this.st_4=create st_4
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_magazine_issue_delete
this.Control[iCurrent+2]=this.cb_delete
this.Control[iCurrent+3]=this.cb_clear
this.Control[iCurrent+4]=this.cb_exit
this.Control[iCurrent+5]=this.sle_producer
this.Control[iCurrent+6]=this.sle_format
this.Control[iCurrent+7]=this.sle_status
this.Control[iCurrent+8]=this.st_1
this.Control[iCurrent+9]=this.st_2
this.Control[iCurrent+10]=this.st_3
this.Control[iCurrent+11]=this.dw_magazine_issue_delete_magcd
this.Control[iCurrent+12]=this.dw_magazine_issue_delete_rc
this.Control[iCurrent+13]=this.em_issue_date
this.Control[iCurrent+14]=this.st_4
end on

on w_magazine_issue_delete.destroy
call super::destroy
destroy(this.dw_magazine_issue_delete)
destroy(this.cb_delete)
destroy(this.cb_clear)
destroy(this.cb_exit)
destroy(this.sle_producer)
destroy(this.sle_format)
destroy(this.sle_status)
destroy(this.st_1)
destroy(this.st_2)
destroy(this.st_3)
destroy(this.dw_magazine_issue_delete_magcd)
destroy(this.dw_magazine_issue_delete_rc)
destroy(this.em_issue_date)
destroy(this.st_4)
end on

event pfc_preopen;call super::pfc_preopen;this.of_SetPreference(TRUE)
this.inv_preference.of_SetToolBars(TRUE)
this.inv_preference.of_SetWindow(TRUE)
this.of_SetResize(TRUE)
this.inv_resize.of_SetOrigSize(this.workspacewidth(),this.workspaceheight())

inv_resize.of_Register(cb_clear, "scale")
inv_resize.of_Register(cb_delete, "scale")
inv_resize.of_Register(cb_exit, "scale")
inv_resize.of_Register(dw_magazine_issue_delete, "scale")
inv_resize.of_Register(dw_magazine_issue_delete_magcd, "scale")
inv_resize.of_Register(dw_magazine_issue_delete_rc, "scale")
inv_resize.of_Register(sle_format, "scale")
inv_resize.of_Register(sle_producer, "scale")
inv_resize.of_Register(sle_status, "scale")


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

event closequery;//
end event

type dw_magazine_issue_delete from u_dw within w_magazine_issue_delete
event ue_enter_to_tab pbm_dwnprocessenter
integer x = 64
integer y = 420
integer width = 2107
integer height = 580
integer taborder = 0
string dataobject = "d_issue_maintenance_magiss"
boolean vscrollbar = false
boolean livescroll = false
end type

event ue_enter_to_tab;call super::ue_enter_to_tab;Send(Handle(this), 256,9, Long(0,0))
Return (1)
end event

event constructor;call super::constructor;SettransObject(SqlServerTrans)



//set the rightmouse button to false
ib_rmbmenu = FALSE
//
////addrow a row to the datawindow
//dw_magazine_issue_delete.Event pfc_addrow()
//
////Protect the following fields
//dw_magazine_issue_delete.Object.magiss_startdt.Protect = 1
//dw_magazine_issue_delete.Object.magiss_shipdt.Protect = 1
//dw_magazine_issue_delete.Object.magiss_subs.Protect = 1
//dw_magazine_issue_delete.Object.magiss_sz.Protect = 1
//dw_magazine_issue_delete.Object.magiss_mins.Protect = 1
//
////set focus to the datawindow
//dw_magazine_issue_delete.SetFocus()
end event

event pfc_deleterow;//////////////////////////////////////////////////////////////////////////////
//
//	Event:  pfc_deleterow
//
//	Arguments:  none
//
//	Returns:  integer
//	 1 = success
//	-1 = error
//
//	Description:
//	Deletes the current or selected row(s)
//
//////////////////////////////////////////////////////////////////////////////
//	
//	Revision History
//
//	Version
//	5.0   Initial version
//
//////////////////////////////////////////////////////////////////////////////
//
//	Copyright © 1996-1997 Sybase, Inc. and its subsidiaries.  All rights reserved.
//	Any distribution of the PowerBuilder Foundation Classes (PFC)
//	source code by other than Sybase, Inc. and its subsidiaries is prohibited.
//
//////////////////////////////////////////////////////////////////////////////

integer	li_rc


if IsValid (inv_rowmanager) then
	li_rc = inv_rowmanager.event pfc_deleterow () 
else	
	li_rc = cb_delete.Event clicked()
end if

// Notify the Linkage Service 
IF IsValid ( inv_Linkage ) THEN 
	If li_rc > 0 Then inv_Linkage.Event pfc_DeleteRow (0) 
END IF 

return li_rc
end event

type cb_delete from commandbutton within w_magazine_issue_delete
integer x = 1271
integer y = 1288
integer width = 329
integer height = 108
boolean bringtotop = true
integer textsize = -10
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "&Delete"
end type

event clicked;Date    ld_issuedate
String  ls_magcode
Integer li_rtn_code, li_yes_no, li_temp
datetime ldt_issdt

Parent.SetMicrohelp("Deleting Records Please Wait ...")


IF dw_magazine_issue_delete.Visible = TRUE THEN		
	   dw_magazine_issue_delete_rc.Visible = FALSE
		dw_magazine_issue_delete.AcceptText()
		//Get issuedate and magazine code 		
		ldt_issdt = dw_magazine_issue_delete.GetItemDatetime(1,"magiss_issdt")
		ld_issuedate = date(ldt_issdt)
		ls_magcode   = dw_magazine_issue_delete.GetItemString(1,"magiss_magcd")
		
		//Check to see if the magazine has been invoiced. If invoiced
		//then prevent it from being deleted.
		IF wf_check_if_invoiced(ls_magcode,ld_issuedate) = 0 THEN
			MessageBox('Error',' Magazine Issue has been Invoiced, Deletion Not Allowed')
			cb_clear.TriggerEvent(clicked!)
			RETURN
		END IF
		
		//Check to see if magazine has extension. Do not delete if it
		//has an extension.
		IF wf_check_if_ext(ls_magcode, ld_issuedate) = 0 THEN
			MessageBox('Error','Magazine Issue Has Extensions, Deletion Not Allowed')
			cb_clear.TriggerEvent(clicked!)
			RETURN
		END IF
		
		li_yes_no = MessageBox('Deletion','Do You Want To Delete This Issue ?',Question!,YesNo!)
		
		IF li_yes_no = 1 THEN
			
			
			IF  dw_magazine_issue_delete.DeleteRow(0) = 1 THEN
			  IF dw_magazine_issue_delete.Update() = 1 THEN
					COMMIT USING SqlServerTrans;
					Parent.SetMicrohelp("")
					cb_clear.TriggerEvent(clicked!)
					RETURN 1
			  ELSE
				  MessageBox('ERROR','Error Accessing The Database.. Contact Your Database Administrator')
				  ROLLBACK USING SQLServertrans;
			  END IF //IF dw_magazine_issue_delete.Update()
				  
			 END IF //IF  dw_magazine_issue_delete.DeleteRow(0) = 1 THEN
		 
     
				
		ELSEIF li_yes_no = 2 THEN
			cb_clear.TriggerEvent(clicked!)
		
		END IF//IF li_yes_no = 1 THEN
		
		 cb_clear.TriggerEvent(clicked!)
		
ELSEIF dw_magazine_issue_delete_rc.Visible = TRUE THEN //Process for RC
	    dw_magazine_issue_delete.Visible = FALSE
		 dw_magazine_issue_delete_rc.AcceptText()
		//Get issuedate and magazine code 	
		ldt_issdt = dw_magazine_issue_delete_rc.Object.issdt[1]
		ld_issuedate = date(ldt_issdt)
		ls_magcode   = dw_magazine_issue_delete_rc.Object.magcd[1]
		
		//Check to see if the magazine has been invoiced. If invoiced
		//then prevent it from being deleted.
		IF wf_check_if_invoiced(ls_magcode,ld_issuedate) = 0 THEN
			MessageBox('Error',' Magazine Issue has been Invoiced, Deletion Not Allowed')
			cb_clear.TriggerEvent(clicked!)
			RETURN
		END IF
		
		//Check to see if magazine has extension. Do not delete if it
		//has an extension.
		IF wf_check_if_ext(ls_magcode, ld_issuedate) = 0 THEN
			MessageBox('Error','Magazine Issue Has Extensions, Deletion Not Allowed')
			cb_clear.TriggerEvent(clicked!)
			RETURN
		END IF
		
		li_yes_no = MessageBox('Deletion','Do You Want To Delete This Issue ?',Question!,YesNo!)
				
		IF li_yes_no = 1 THEN
			
			
			
			IF  dw_magazine_issue_delete_rc.DeleteRow(0) = 1 THEN
			  IF dw_magazine_issue_delete_rc.Update() = 1 THEN
					COMMIT USING SqlServerTrans;
					Parent.SetMicrohelp("")
					cb_clear.TriggerEvent(clicked!)
					RETURN 1
			  ELSE
				  MessageBox('ERROR','Error Accessing The Database.. Contact Your Database Administrator')
				  ROLLBACK USING SQLServertrans;
			  END IF //IF dw_magazine_issue_delete.Update()
				  
			 END IF //IF  dw_magazine_issue_delete.DeleteRow(0) = 1 THEN
		 
				
		  cb_clear.TriggerEvent(clicked!)		
		ELSEIF li_yes_no = 2 THEN
			cb_clear.TriggerEvent(clicked!)
		
		END IF//IF li_yes_no = 1 THEN
		
		cb_clear.TriggerEvent(clicked!)
	
	
	
	
END IF//IF dw_magazine_issue_delete.Visible = TRUE THEN	



end event

type cb_clear from commandbutton within w_magazine_issue_delete
integer x = 1696
integer y = 1288
integer width = 329
integer height = 108
boolean bringtotop = true
integer textsize = -10
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "&Clear"
end type

event clicked;parent.SetMicroHelp('')
sle_format.text = ''
sle_producer.text = ''
sle_status.text = ''
em_issue_date.text = ''


cb_delete.Enabled = FALSE

//Reset the datawindows
dw_magazine_issue_delete.Reset()
dw_magazine_issue_delete.Visible = TRUE

dw_magazine_issue_delete_rc.Reset()
dw_magazine_issue_delete_rc.Visible = FALSE

dw_magazine_issue_delete_magcd.Enabled = TRUE
dw_magazine_issue_delete_magcd.Reset()
dw_magazine_issue_delete_magcd.Event pfc_addrow()
dw_magazine_issue_delete_magcd.SetFocus()

end event

type cb_exit from commandbutton within w_magazine_issue_delete
integer x = 2098
integer y = 1288
integer width = 329
integer height = 108
boolean bringtotop = true
integer textsize = -10
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "E&xit"
end type

event clicked;long li_cnt, li_cnt2,li_cnt3, li_cnt4, li_re

//Reset the data windows and close the screen.
//dw_magazine_issue_delete.Reset()
ib_disableclosequery=true
dw_magazine_issue_delete.AcceptText()
dw_magazine_issue_delete_rc.AcceptText()
dw_magazine_issue_delete_magcd.Reset()
//dw_magazine_issue_delete_rc.Reset()
li_cnt=dw_magazine_issue_delete.DeletedCount()
li_cnt2=dw_magazine_issue_delete.ModifiedCount()
li_cnt3=dw_magazine_issue_delete_rc.DeletedCount()
li_cnt4=dw_magazine_issue_delete_rc.ModifiedCount()
if li_cnt=0 and li_cnt2=0 and li_cnt3=0 and li_cnt4=0 then
	
	close(parent)
	return
else
	li_re=messagebox('','Do you want ot save change?',Question!,YesNo!,1)
	if li_re=1 then
		li_re=dw_magazine_issue_delete.update()
		if li_re=1 then 
			li_re=dw_magazine_issue_delete_rc.update()
			if li_re=1 then
				commit using SqlServerTrans;
			else
				RollBack using SqlServerTrans;
			end if
		else
			RollBack using SqlServerTrans;
		end if
	end if
end if

close(parent)
m_pics_main.m_menu.PopMenu ( 300, 0 ) 

end event

type sle_producer from u_sle within w_magazine_issue_delete
integer x = 2455
integer y = 32
integer width = 229
integer height = 92
integer taborder = 0
integer textsize = -10
long backcolor = 12632256
boolean displayonly = true
end type

type sle_format from u_sle within w_magazine_issue_delete
integer x = 2455
integer y = 144
integer width = 206
integer height = 92
integer taborder = 0
boolean bringtotop = true
integer textsize = -10
long backcolor = 12632256
boolean displayonly = true
end type

type sle_status from u_sle within w_magazine_issue_delete
integer x = 2455
integer y = 264
integer width = 187
integer height = 92
integer taborder = 0
boolean bringtotop = true
integer textsize = -10
long backcolor = 12632256
boolean displayonly = true
end type

type st_1 from u_st within w_magazine_issue_delete
integer x = 2139
integer y = 44
integer width = 288
string text = "Producer"
alignment alignment = right!
end type

type st_2 from u_st within w_magazine_issue_delete
integer x = 2199
integer y = 156
integer width = 229
boolean bringtotop = true
string text = "Format"
alignment alignment = right!
end type

type st_3 from u_st within w_magazine_issue_delete
integer x = 2208
integer y = 276
integer width = 219
boolean bringtotop = true
string text = "Status"
alignment alignment = right!
end type

type dw_magazine_issue_delete_magcd from u_dw within w_magazine_issue_delete
integer x = 55
integer y = 20
integer width = 2085
integer height = 108
integer taborder = 10
string dataobject = "d_magazine_issue_maintenance_magcd"
boolean vscrollbar = false
end type

event constructor;call super::constructor;SettransObject(SqlServerTrans)
THIS.Event pfc_addrow()

end event

event itemchanged;call super::itemchanged;String ls_magcode
Integer li_rtn_code

//Get value into local variable
ls_magcode = GetText()

li_rtn_code = dw_magazine_issue_delete_magcd.Retrieve(ls_magcode)

//Check to see if the entered value is correct if not show
//message box.
IF li_rtn_code = 0 THEN
	dw_magazine_issue_delete_magcd.Object.magcd.validationmsg = &
	                   'Magazine Code Not Found'
	dw_magazine_issue_delete_magcd.Event pfc_addrow()
   Return 1
ELSEIF li_rtn_code = 1 THEN
	//enable the edit mask
	em_issue_date.DisplayOnly = FALSE
	em_issue_date.SetFocus()
END IF


end event

type dw_magazine_issue_delete_rc from u_dw within w_magazine_issue_delete
boolean visible = false
integer x = 64
integer y = 284
integer width = 2117
integer height = 824
integer taborder = 0
string dataobject = "d_magazine_issue_maintenance_rc"
boolean vscrollbar = false
end type

event constructor;call super::constructor;this.of_setTransObject(SqlServerTrans)
end event

type em_issue_date from u_em within w_magazine_issue_delete
event ue_key pbm_keydown
integer x = 347
integer y = 168
integer width = 302
integer height = 84
integer taborder = 20
alignment alignment = center!
maskdatatype maskdatatype = datemask!
string mask = "mm/dd/yyyy"
string displaydata = ""
double increment = 0
string minmax = ""
end type

event modified;call super::modified;//





String ls_magcode, ls_fy, ls_producer, ls_format, ls_status, ls_contract
Date   ld_issuedate
Integer li_rtn_code, li_fy

IF KeyDown(KeyEnter!) THEN
	//Get values into local variables. Set the fiscal year into
	//ls_fy variable.
   ls_fy = MID(em_issue_date.text,7)
 	li_fy = Integer(ls_fy)
 	ls_magcode = dw_magazine_issue_delete_magcd.Object.magcd[1]	
   ld_issuedate = Date(em_issue_date.text)	
	
	//check if the issuedate exists
	li_rtn_code = wf_check_issuedate(ls_magcode, ld_issuedate)
	
  	IF li_rtn_code = 100 THEN
		MessageBox('Error','No Data For this Issue Date')
		RETURN
	ELSEIF li_rtn_code = 0 THEN
		 li_rtn_code = wf_get_contract_information(ls_magcode, li_fy, ls_producer, ls_format, ls_status, ls_contract)
		IF li_rtn_code = 100 THEN
			MessageBox('Error','Magazine Contract Does Not Exist For This Issue Date')
			em_issue_date.SetFocus()
			em_issue_date.Event pfc_selectall()
			RETURN
	   ELSEIF li_rtn_code = 0 THEN 
			//Retrieve data, check if it is invoiced or has an extension.
		   sle_format.text = ls_format
			sle_producer.text = ls_producer
			sle_status.text = ls_status
			
			IF TRIM(ls_format) = 'RC' THEN
				dw_magazine_issue_delete.Visible = FALSE
				dw_magazine_issue_delete_rc.Visible = TRUE
				dw_magazine_issue_delete_rc.Retrieve(ls_magcode, ld_issuedate)
				dw_magazine_issue_delete_magcd.Enabled = FALSE
				cb_delete.Enabled = TRUE
			ELSEIF TRIM(ls_format) <> 'RC' THEN
				dw_magazine_issue_delete.Visible = TRUE
				dw_magazine_issue_delete_rc.Visible = FALSE
				dw_magazine_issue_delete.Retrieve(ls_magcode, ld_issuedate)
				dw_magazine_issue_delete_magcd.Enabled = FALSE
			   cb_delete.Enabled = TRUE
			END IF//IF TRIM(ls_format) = 'RC' THEN
				
			
		END IF//IF li_rtn_code = 100 THEN
		
	END IF//IF li_rtn_code = 100 THEN
	  
	  
	  
	  
	  
	  
	  
	  
	  
	  
	  
	  
	  
	  
	
	
	
	
	
	
	
	
	
	
	
END IF//IF KeyDown(KeyEnter!) THEN
end event

event getfocus;call super::getfocus;
THIS.Event pfc_selectall()
end event

type st_4 from u_st within w_magazine_issue_delete
integer x = 55
integer y = 176
integer width = 256
string text = "Issue Date"
alignment alignment = right!
end type


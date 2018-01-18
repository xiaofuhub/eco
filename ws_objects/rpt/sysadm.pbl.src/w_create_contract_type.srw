$PBExportHeader$w_create_contract_type.srw
forward
global type w_create_contract_type from w_sheet
end type
type cb_exit from u_cb within w_create_contract_type
end type
type cb_clear from u_cb within w_create_contract_type
end type
type cb_update from u_cb within w_create_contract_type
end type
type st_1 from statictext within w_create_contract_type
end type
type st_2 from statictext within w_create_contract_type
end type
type em_cntrtype from uo_conno within w_create_contract_type
end type
type ddlb_cntrmed from dropdownlistbox within w_create_contract_type
end type
type dw_create_contract_type from u_pics_dw within w_create_contract_type
end type
type st_3 from statictext within w_create_contract_type
end type
type sle_cntrdesc from u_sle within w_create_contract_type
end type
end forward

global type w_create_contract_type from w_sheet
integer width = 2770
integer height = 1560
string title = "Add/View Contract Type"
cb_exit cb_exit
cb_clear cb_clear
cb_update cb_update
st_1 st_1
st_2 st_2
em_cntrtype em_cntrtype
ddlb_cntrmed ddlb_cntrmed
dw_create_contract_type dw_create_contract_type
st_3 st_3
sle_cntrdesc sle_cntrdesc
end type
global w_create_contract_type w_create_contract_type

type variables
string Lmed
end variables

on w_create_contract_type.create
int iCurrent
call super::create
this.cb_exit=create cb_exit
this.cb_clear=create cb_clear
this.cb_update=create cb_update
this.st_1=create st_1
this.st_2=create st_2
this.em_cntrtype=create em_cntrtype
this.ddlb_cntrmed=create ddlb_cntrmed
this.dw_create_contract_type=create dw_create_contract_type
this.st_3=create st_3
this.sle_cntrdesc=create sle_cntrdesc
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.cb_exit
this.Control[iCurrent+2]=this.cb_clear
this.Control[iCurrent+3]=this.cb_update
this.Control[iCurrent+4]=this.st_1
this.Control[iCurrent+5]=this.st_2
this.Control[iCurrent+6]=this.em_cntrtype
this.Control[iCurrent+7]=this.ddlb_cntrmed
this.Control[iCurrent+8]=this.dw_create_contract_type
this.Control[iCurrent+9]=this.st_3
this.Control[iCurrent+10]=this.sle_cntrdesc
end on

on w_create_contract_type.destroy
call super::destroy
destroy(this.cb_exit)
destroy(this.cb_clear)
destroy(this.cb_update)
destroy(this.st_1)
destroy(this.st_2)
destroy(this.em_cntrtype)
destroy(this.ddlb_cntrmed)
destroy(this.dw_create_contract_type)
destroy(this.st_3)
destroy(this.sle_cntrdesc)
end on

event open;call super::open;ddlb_cntrmed.enabled=FALSE
sle_cntrdesc.text=""
sle_cntrdesc.enabled=FALSE
dw_create_contract_type.Enabled=FALSE
cb_clear.Enabled = TRUE
cb_update.Enabled = FALSE
end event

type cb_exit from u_cb within w_create_contract_type
event clicked pbm_bnclicked
integer x = 2295
integer y = 1284
integer taborder = 0
integer textsize = -10
string text = "E&xit"
end type

event clicked;call super::clicked;dw_create_contract_type.ResetUpdate()
close(parent)

end event

type cb_clear from u_cb within w_create_contract_type
event clicked pbm_bnclicked
integer x = 1911
integer y = 1288
integer taborder = 0
integer textsize = -10
string text = "&Clear"
end type

event clicked;call super::clicked;dw_create_contract_type.Reset()
em_cntrtype.text = ""
IF ddlb_cntrmed.enabled THEN
	ddlb_cntrmed.text = ""
	ddlb_cntrmed.clear()
	ddlb_cntrmed.enabled=FALSE
END IF
IF sle_cntrdesc.enabled THEN
	sle_cntrdesc.enabled=FALSE
	sle_cntrdesc.text=""
END IF
dw_create_contract_type.Enabled=FALSE
cb_update.Enabled = FALSE
em_cntrtype.SetFocus()
end event

type cb_update from u_cb within w_create_contract_type
event clicked pbm_bnclicked
string tag = "Update the database"
integer x = 1518
integer y = 1292
integer taborder = 0
integer textsize = -10
string text = "&Update"
end type

event clicked;call super::clicked;String LsUserID,lcntrtype,lcntrdesc
int rtn,index
date tday

LsUserID = gnv_app.of_GetUserId()
tday = Today()


rtn = dw_create_contract_type.Event pfc_update(TRUE,TRUE)
IF rtn=1 then
		
	Lcntrtype = em_cntrtype.text
	Lcntrdesc = sle_cntrdesc.text
	
	update prodstage 
	set cntrtypedesc = :Lcntrdesc, Modified_Date = :tday, Modified_By = :LsUserID
	where cntrtype = :Lcntrtype
	and trim(cntrmed) = :Lmed
	using sqlservertrans;

	IF f_check_dberror(SqlServerTrans,"Updating PRODSTAGE")=FALSE THEN
		Messagebox("Update Error","Failed to PRODSTAGE",StopSign!)
		ROLLBACK USING SqlServerTrans;
	ELSE
		Messagebox("Update Success","Database updated",Information!)
		COMMIT USING SqlServerTrans;
	END IF
END IF
end event

type st_1 from statictext within w_create_contract_type
integer x = 146
integer y = 44
integer width = 398
integer height = 76
boolean bringtotop = true
integer textsize = -10
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long backcolor = 67108864
boolean enabled = false
string text = "Contract Type"
boolean focusrectangle = false
end type

type st_2 from statictext within w_create_contract_type
integer x = 827
integer y = 44
integer width = 471
integer height = 76
boolean bringtotop = true
integer textsize = -10
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long backcolor = 67108864
boolean enabled = false
string text = "Contract Medium"
boolean focusrectangle = false
end type

type em_cntrtype from uo_conno within w_create_contract_type
integer x = 549
integer y = 44
integer width = 142
integer height = 88
integer taborder = 10
integer textsize = -10
alignment alignment = center!
textcase textcase = upper!
maskdatatype maskdatatype = stringmask!
string mask = "x"
string displaydata = "ð"
end type

event modified;call super::modified;string Ltemp
Ltemp = em_cntrtype.text
IF (Ltemp = '0' OR Ltemp = '1' OR Ltemp = '2' OR Ltemp = '3' OR Ltemp = '4' OR Ltemp = '5' OR &
    Ltemp = '6' OR Ltemp = '7' OR Ltemp = '8' OR Ltemp = '9') THEN
 	return -1
ELSE
	ddlb_cntrmed.enabled=TRUE
	ddlb_cntrmed.SetFocus()
END IF
	
end event

type ddlb_cntrmed from dropdownlistbox within w_create_contract_type
integer x = 1307
integer y = 44
integer width = 901
integer height = 496
integer taborder = 20
boolean bringtotop = true
integer textsize = -10
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long backcolor = 16777215
boolean vscrollbar = true
string item[] = {"Braille","Print Braille","Recorded Talking Books","Cassettes","Flash Cartridges"}
end type

event selectionchanged;string Lcntrtype,Lcntrmed,LsUserID,ls_contract_type
int lcnt,rtn
date lday

lday = Today()
LsUserID = gnv_app.of_GetUserId()

Lcntrtype = em_cntrtype.text
CHOOSE CASE ddlb_cntrmed.text(index)
	CASE "Cassettes"
		Lcntrmed = "RC"
	CASE "Flash Cartridges"
		ls_contract_type = em_cntrtype.text
		IF ls_contract_type <> 'D' THEN
			MessageBox("Warning","Flash Cartridges can not be used with contract type: "+"~'"+ls_contract_type+"~'"+"~r~nYou must use another contract medium.",StopSign!)
             RETURN			
		ELSE 		
			Lcntrmed = "FC"
		END IF
	CASE "Braille"
		Lcntrmed = "BR"
	CASE "Print Braille"
		Lcntrmed = "P/B"
	CASE "Recorded Talking Books"
		ls_contract_type = em_cntrtype.text
		IF ls_contract_type = 'D' THEN
			MessageBox("Warning","Please choose between Cassettes or Flash Cartridges for Recorded Media types.",StopSign!)
             RETURN			
		ELSE 		
			Lcntrmed = "RTB"
		END IF
END CHOOSE

Lmed = Lcntrmed

select count(*) into :lcnt 
	from prodstage 
	where cntrtype = :Lcntrtype 
	and cntrmed = :Lcntrmed 
	using sqlservertrans;
IF lcnt > 0 THEN
	rtn = MessageBox("Warning","Contract type: "+"~'"+Lcntrtype+"~'"+" with media type: "+"~'"+ddlb_cntrmed.text(index)+"~'"+" exist. Do you want to modify this contract type?.",Question!,YesNo!,1)
	IF rtn = 1 THEN
		rtn = MessageBox("Warning","BE AWARE THAT CHANGING THE CONTRACT TYPE INFORMATION, COULD CORRUPT SOME EXISTING DATA! ~nContinue?.",Question!,YesNo!,1)
		IF rtn = 1 THEN		
			dw_create_contract_type.Retrieve(Lcntrtype,Lcntrmed)
			sle_cntrdesc.text = dw_create_contract_type.object.cntrtypedesc[1]
			sle_cntrdesc.enabled = TRUE
			sle_cntrdesc.Setfocus()
			dw_create_contract_type.Enabled = TRUE
			cb_clear.Enabled = TRUE
			cb_update.Enabled = TRUE
		ELSE
			dw_create_contract_type.Retrieve(Lcntrtype,Lcntrmed)
			sle_cntrdesc.text = dw_create_contract_type.object.cntrtypedesc[1]
			cb_clear.Enabled = TRUE
		END IF	
	ELSE
		return
	END IF
ELSE
	rtn = MessageBox("Warning","Contract type: "+"~'"+Lcntrtype+"~'"+" with media type: "+"~'"+ddlb_cntrmed.text(index)+"~'"+" does not exist. Would you like to add this contract type?",Question!,YesNo!,1)
	IF rtn = 1 THEN	
		dw_create_contract_type.Enabled = TRUE
		dw_create_contract_type.InsertRow(0)
		dw_create_contract_type.object.cntrtype[1] = Lcntrtype
		dw_create_contract_type.object.cntrmed[1] = Lcntrmed
		dw_create_contract_type.object.Created_Date[1] = lday
		dw_create_contract_type.object.Created_By[1] = LsUserID
		sle_cntrdesc.enabled = TRUE
		sle_cntrdesc.SetFocus()
		cb_update.Enabled = TRUE
		cb_clear.Enabled = TRUE
	ELSE
		RETURN
	END IF
END IF 
end event

type dw_create_contract_type from u_pics_dw within w_create_contract_type
event ue_enterkey pbm_dwnprocessenter
integer x = 119
integer y = 368
integer width = 2528
integer height = 880
integer taborder = 40
string dataobject = "d_create_contract_type"
end type

event ue_enterkey;call super::ue_enterkey;Send(Handle(this),256,9,Long(0,0))
return(1)
end event

event constructor;call super::constructor;dw_create_contract_type.of_settransobject(SQLservertrans)

end event

event pfc_addrow;long	ll_rc
string Lcntrtype,Lcntrmed,LsUserID
Date lday

lday = Today()

LsUserID = gnv_app.of_GetUserId()

// Insert row
if IsValid (inv_rowmanager) then
	ll_rc = inv_rowmanager.event pfc_addrow ()
else
	ll_rc = this.InsertRow (0) 
	Lcntrtype = em_cntrtype.text
	Lcntrmed = Lmed
	dw_create_contract_type.object.cntrtype[ll_rc] = Lcntrtype
	dw_create_contract_type.object.cntrmed[ll_rc] = Lcntrmed
	dw_create_contract_type.object.Created_Date[ll_rc] = lday
	dw_create_contract_type.object.Created_By[ll_rc] = LsUserID
end if

// Notify the Linkage Service that a new row has been added.
IF IsValid ( inv_Linkage ) THEN 
	inv_Linkage.Event pfc_InsertRow (ll_rc) 
END IF 

this.ScrollToRow(ll_rc)
this.SetColumn("prodstage")

return ll_rc
end event

event pfc_insertrow;long	ll_currow
long	ll_rc
string Lcntrtype,Lcntrmed

// Get current row
ll_currow = this.GetRow()
if ll_currow < 0 then
	ll_currow = 0
end if

// Insert row
if IsValid (inv_rowmanager) then
	ll_rc = inv_rowmanager.event pfc_insertrow (ll_currow)
else
	ll_rc = this.InsertRow (ll_currow) 
	Lcntrtype = em_cntrtype.text
	Lcntrmed = Lmed
	dw_create_contract_type.object.cntrtype[ll_rc] = Lcntrtype
	dw_create_contract_type.object.cntrmed[ll_rc] = Lcntrmed
end if

// Notify the Linkage Service that a new row has been added.
IF IsValid ( inv_Linkage ) THEN 
	inv_Linkage.Event pfc_InsertRow (ll_rc) 
END IF 

return ll_rc

end event

type st_3 from statictext within w_create_contract_type
integer x = 142
integer y = 192
integer width = 581
integer height = 76
boolean bringtotop = true
integer textsize = -10
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long backcolor = 67108864
boolean enabled = false
string text = "Contract description "
boolean focusrectangle = false
end type

type sle_cntrdesc from u_sle within w_create_contract_type
event ue_enterkey pbm_dwnprocessenter
integer x = 731
integer y = 180
integer width = 1275
integer height = 108
integer taborder = 30
integer textsize = -10
boolean autohscroll = true
end type

event ue_enterkey;call super::ue_enterkey;Send(Handle(this),256,9,Long(0,0))
return(1)
end event

event modified;call super::modified;dw_create_contract_type.setfocus()
end event


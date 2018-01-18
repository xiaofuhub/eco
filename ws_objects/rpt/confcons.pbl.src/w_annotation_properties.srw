$PBExportHeader$w_annotation_properties.srw
forward
global type w_annotation_properties from w_main
end type
type cb_grade from commandbutton within w_annotation_properties
end type
type cb_lang from commandbutton within w_annotation_properties
end type
type cb_casub from commandbutton within w_annotation_properties
end type
type dw_annotation_properties from u_dw within w_annotation_properties
end type
type cb_update from commandbutton within w_annotation_properties
end type
type cb_exit from commandbutton within w_annotation_properties
end type
end forward

global type w_annotation_properties from w_main
string tag = "Annotation Properties"
integer width = 2629
integer height = 1868
string title = "Annotation Properties"
boolean minbox = false
boolean maxbox = false
boolean resizable = false
windowtype windowtype = response!
event ue_enterkey pbm_dwnprocessenter
cb_grade cb_grade
cb_lang cb_lang
cb_casub cb_casub
dw_annotation_properties dw_annotation_properties
cb_update cb_update
cb_exit cb_exit
end type
global w_annotation_properties w_annotation_properties

type variables
boolean ib_insert=FALSE
end variables

forward prototypes
public function integer wf_check_group_ordering_3 (integer ordernum)
public function integer wf_check_group_ordering_2 (integer ordernum)
public function integer wf_check_group_ordering (integer ordernum)
public function integer wf_check_ordering_1 (integer ordernum)
public function integer wf_check_ordering_2 (integer ordernum)
public function integer wf_check_ordering_3 (integer ordernum)
public function integer wf_check_ordering_4 (integer ordernum)
public function integer wf_check_ordering_5 (integer ordernum)
public function integer wf_check_ordering_6 (integer ordernum)
public function integer wf_check_ordering_7 (integer ordernum)
public function integer wf_check_ordering_8 (integer ordernum)
public function integer wf_check_ordering_9 (integer ordernum)
public function integer wf_check_ordering_10 (integer ordernum)
end prototypes

public function integer wf_check_group_ordering_3 (integer ordernum);IF dw_annotation_properties.object.med_group_ord[1]=ordernum OR &
	dw_annotation_properties.object.sex_group_ord[1]=ordernum THEN
	RETURN 1
ELSE
	RETURN 2
END IF

end function

public function integer wf_check_group_ordering_2 (integer ordernum);IF dw_annotation_properties.object.sex_group_ord[1]=ordernum THEN
	RETURN 1
ELSE
	RETURN 2
END IF

end function

public function integer wf_check_group_ordering (integer ordernum);IF dw_annotation_properties.object.med_group_ord[1]=ordernum THEN
	RETURN 1
ELSE
	RETURN 2
END IF

end function

public function integer wf_check_ordering_1 (integer ordernum);IF dw_annotation_properties.object.grd_1_br_ord[1]=ordernum OR &
	dw_annotation_properties.object.flang_ord[1]=ordernum OR &
 	dw_annotation_properties.object.casub_ord[1]=ordernum OR &
	dw_annotation_properties.object.prize_ord[1]=ordernum OR &
	dw_annotation_properties.object.bestseller_ord[1]=ordernum OR &
	dw_annotation_properties.object.year_written_ord[1]=ordernum THEN
	RETURN 1
ELSE
	RETURN 2
END IF

end function

public function integer wf_check_ordering_2 (integer ordernum);IF dw_annotation_properties.object.prnt_br_ord[1]=ordernum OR &
	dw_annotation_properties.object.flang_ord[1]=ordernum OR &
 	dw_annotation_properties.object.casub_ord[1]=ordernum OR &
	dw_annotation_properties.object.prize_ord[1]=ordernum OR &
	dw_annotation_properties.object.bestseller_ord[1]=ordernum OR &
	dw_annotation_properties.object.year_written_ord[1]=ordernum THEN
	RETURN 1
ELSE
	RETURN 2
END IF

end function

public function integer wf_check_ordering_3 (integer ordernum);IF dw_annotation_properties.object.grd_1_br_ord[1]=ordernum OR &
	dw_annotation_properties.object.prnt_br_ord[1]=ordernum OR &
 	dw_annotation_properties.object.casub_ord[1]=ordernum OR &
	dw_annotation_properties.object.prize_ord[1]=ordernum OR &
	dw_annotation_properties.object.bestseller_ord[1]=ordernum OR &
	dw_annotation_properties.object.year_written_ord[1]=ordernum THEN
	RETURN 1
ELSE
	RETURN 2
END IF

end function

public function integer wf_check_ordering_4 (integer ordernum);IF dw_annotation_properties.object.grd_1_br_ord[1]=ordernum OR &
	dw_annotation_properties.object.flang_ord[1]=ordernum OR &
 	dw_annotation_properties.object.prnt_br_ord[1]=ordernum OR &
	dw_annotation_properties.object.prize_ord[1]=ordernum OR &
	dw_annotation_properties.object.bestseller_ord[1]=ordernum OR &
	dw_annotation_properties.object.year_written_ord[1]=ordernum THEN
	RETURN 1
ELSE
	RETURN 2
END IF

end function

public function integer wf_check_ordering_5 (integer ordernum);IF dw_annotation_properties.object.grd_1_br_ord[1]=ordernum OR &
	dw_annotation_properties.object.flang_ord[1]=ordernum OR &
 	dw_annotation_properties.object.casub_ord[1]=ordernum OR &
	dw_annotation_properties.object.prnt_br_ord[1]=ordernum OR &
	dw_annotation_properties.object.bestseller_ord[1]=ordernum OR &
	dw_annotation_properties.object.year_written_ord[1]=ordernum THEN
	RETURN 1
ELSE
	RETURN 2
END IF

end function

public function integer wf_check_ordering_6 (integer ordernum);IF dw_annotation_properties.object.grd_1_br_ord[1]=ordernum OR &
	dw_annotation_properties.object.flang_ord[1]=ordernum OR &
 	dw_annotation_properties.object.casub_ord[1]=ordernum OR &
	dw_annotation_properties.object.prize_ord[1]=ordernum OR &
	dw_annotation_properties.object.prnt_br_ord[1]=ordernum OR &
	dw_annotation_properties.object.year_written_ord[1]=ordernum THEN
	RETURN 1
ELSE
	RETURN 2
END IF

end function

public function integer wf_check_ordering_7 (integer ordernum);IF dw_annotation_properties.object.grd_1_br_ord[1]=ordernum OR &
	dw_annotation_properties.object.flang_ord[1]=ordernum OR &
 	dw_annotation_properties.object.casub_ord[1]=ordernum OR &
	dw_annotation_properties.object.prize_ord[1]=ordernum OR &
	dw_annotation_properties.object.bestseller_ord[1]=ordernum OR &
	dw_annotation_properties.object.prnt_br_ord[1]=ordernum THEN
	RETURN 1
ELSE
	RETURN 2
END IF

end function

public function integer wf_check_ordering_8 (integer ordernum);IF dw_annotation_properties.object.viol_ord[1]=ordernum OR &
	dw_annotation_properties.object.slang_ord[1]=ordernum THEN
	RETURN 1
ELSE
	RETURN 2
END IF

end function

public function integer wf_check_ordering_9 (integer ordernum);IF	dw_annotation_properties.object.sex_ord[1]=ordernum OR &
	dw_annotation_properties.object.slang_ord[1]=ordernum THEN
	RETURN 1
ELSE
	RETURN 2
END IF

end function

public function integer wf_check_ordering_10 (integer ordernum);IF dw_annotation_properties.object.sex_ord[1]=ordernum OR &
	dw_annotation_properties.object.viol_ord[1]=ordernum THEN
	RETURN 1
ELSE
	RETURN 2
END IF

end function

on w_annotation_properties.create
int iCurrent
call super::create
this.cb_grade=create cb_grade
this.cb_lang=create cb_lang
this.cb_casub=create cb_casub
this.dw_annotation_properties=create dw_annotation_properties
this.cb_update=create cb_update
this.cb_exit=create cb_exit
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.cb_grade
this.Control[iCurrent+2]=this.cb_lang
this.Control[iCurrent+3]=this.cb_casub
this.Control[iCurrent+4]=this.dw_annotation_properties
this.Control[iCurrent+5]=this.cb_update
this.Control[iCurrent+6]=this.cb_exit
end on

on w_annotation_properties.destroy
call super::destroy
destroy(this.cb_grade)
destroy(this.cb_lang)
destroy(this.cb_casub)
destroy(this.dw_annotation_properties)
destroy(this.cb_update)
destroy(this.cb_exit)
end on

event pfc_postopen;call super::pfc_postopen;//////////////////////////////////////////////////////////////////////////////////////////////////////////
//
//	Event: pfc_postopen of window
//
//	Description:
//	set insert indicator if new record
//
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//	
//	Revision History
//
//	Developed by 	Date 				Version						Tracking#
//									
// Murali K.			01/28/2008      005 PICS Modifications	 Reqs: CDS. A.8a, CDS A.8a.2
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//


long ll_rtn, ll_row
ll_rtn = dw_annotation_properties.Retrieve()
IF ll_rtn = 0 THEN
	ll_row = dw_annotation_properties.insertrow(0)
	ib_insert=TRUE
	dw_annotation_properties.object.comm_audio_ord[ll_row] =7 // default for commercial audio
END IF
end event

event pfc_preopen;call super::pfc_preopen;this.of_SetPreference(TRUE)
this.inv_preference.of_SetToolBars(TRUE)
this.inv_preference.of_SetWindow(TRUE)
this.of_SetResize(TRUE)
this.of_SetBase(TRUE)
this.inv_base.of_Center()
this.inv_resize.of_SetOrigSize(this.workspacewidth(),this.workspaceheight())
inv_resize.of_Register(dw_annotation_properties, "Scale")
inv_resize.of_Register(cb_exit, "Scale")
inv_resize.of_Register(cb_casub, "Scale")
inv_resize.of_Register(cb_lang, "Scale")
inv_resize.of_Register(cb_update, "Scale")

end event

type cb_grade from commandbutton within w_annotation_properties
integer x = 2185
integer y = 592
integer width = 393
integer height = 112
integer taborder = 20
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "&GRADE"
end type

event clicked;open(w_cc_add_grade)
end event

type cb_lang from commandbutton within w_annotation_properties
integer x = 2185
integer y = 964
integer width = 393
integer height = 112
integer taborder = 20
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "&Language"
end type

event clicked;open(w_cc_lang)
end event

type cb_casub from commandbutton within w_annotation_properties
integer x = 2185
integer y = 772
integer width = 393
integer height = 112
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "&CASUB"
end type

event clicked;open(w_cc_add_casub)
end event

type dw_annotation_properties from u_dw within w_annotation_properties
event ue_enterkey pbm_dwnprocessenter
integer x = 46
integer y = 36
integer width = 2085
integer height = 1732
integer taborder = 10
string dataobject = "d_annotation_properties"
boolean vscrollbar = false
boolean livescroll = false
end type

event ue_enterkey;Send(Handle(this),256,9,Long(0,0))
return(1)
end event

event constructor;call super::constructor;this.of_SetTransObject( SQLServerTrans )

end event

event itemchanged;call super::itemchanged;int ordernum,rtnval
This.AcceptText()
ordernum = integer(data)

IF ordernum<>0 AND NOT(IsNull(ordernum)) THEN	
	IF dwo.name = "sex_ord" THEN
		rtnval =  wf_check_ordering_8(ordernum)
	ELSEIF dwo.name = "viol_ord" THEN
		rtnval =  wf_check_ordering_9(ordernum)
	ELSEIF dwo.name = "slang_ord" THEN
		rtnval =  wf_check_ordering_10(ordernum)
	ELSEIF dwo.name = "prnt_br_ord" THEN
		rtnval =  wf_check_ordering_1(ordernum)
	ELSEIF dwo.name = "grd_1_br_ord" THEN
		rtnval =  wf_check_ordering_2(ordernum)
	ELSEIF dwo.name = "flang_ord" THEN
		rtnval =  wf_check_ordering_3(ordernum)
	ELSEIF dwo.name = "casub_ord" THEN
		rtnval =  wf_check_ordering_4(ordernum)
	ELSEIF dwo.name = "prize_ord" THEN
		rtnval =  wf_check_ordering_5(ordernum)
	ELSEIF dwo.name = "bestseller_ord" THEN
		rtnval =  wf_check_ordering_6(ordernum)
	ELSEIF dwo.name = "year_written_ord" THEN
		rtnval =  wf_check_ordering_7(ordernum)
	ELSEIF dwo.name = "sex_group_ord" THEN
		rtnval =  wf_check_group_ordering(ordernum)		
	ELSEIF dwo.name = "med_group_ord" THEN
		rtnval =  wf_check_group_ordering_2(ordernum)		
	END IF
END IF

IF rtnval=1 THEN
	cb_update.Enabled=FALSE
ELSE
	cb_update.Enabled=TRUE
END IF

RETURN rtnval
end event

event pfc_preupdate;call super::pfc_preupdate;//////////////////////////////////////////////////////////////////////////////////////////////////////////
//
//	Event: pfc_preupdate of dw_annotation_properties
//
//	Description:
//	set primary key and audit columns appropriately
//
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//	
//	Revision History
//
//	Developed by 	Date 				Version						Tracking#
//									
// Murali K.			01/28/2008      005 PICS Modifications	 Reqs: CDS. A.8a, CDS A.8a.2
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//
long ll_seq=1

IF ib_insert THEN
//	//select the next primary key sequence number
//	SELECT ANNO_PROPERTIES_SEQ.NEXTVAL 
//	INTO :ll_seq 
//	FROM DUAL using SQLserverTrans;

	select count(*) + 1
	into :ll_seq
	from anno_properties using sqlservertrans ;
	
	IF sqlservertrans.sqlcode < 0 THEN
		RETURN -1
	END IF
	
	this.object.created_by[1] = gnv_app.of_getuserid()
	this.object.created_date[1] = today()
	this.object.anno_property_no[1] = ll_seq
ELSE
	this.object.modified_by[1] = gnv_app.of_getuserid()
	this.object.modified_date[1] = today()
END IF
RETURN 1
end event

type cb_update from commandbutton within w_annotation_properties
integer x = 2185
integer y = 1388
integer width = 393
integer height = 112
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "&Update"
end type

event clicked;int rtn

rtn = dw_annotation_properties.AcceptText()
IF rtn =1 THEN
	rtn = dw_annotation_properties.Event pfc_update(TRUE,TRUE)
	IF rtn=1 THEN
		Commit Using SqlServerTrans;
		MessageBox("Update","Rules updated.",Information!)
	ELSE
		RollBack Using SqlServerTrans;
		MessageBox("ERROR","Error in updating annotation property table.",StopSign!)
	END IF
END IF
end event

type cb_exit from commandbutton within w_annotation_properties
integer x = 2185
integer y = 1548
integer width = 393
integer height = 112
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "&Exit"
end type

event clicked;close(parent)
end event


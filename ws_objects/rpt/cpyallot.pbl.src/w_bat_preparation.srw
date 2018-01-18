$PBExportHeader$w_bat_preparation.srw
forward
global type w_bat_preparation from w_sheet
end type
type dw_batch_prepare from u_dw within w_bat_preparation
end type
type cb_update from u_cb within w_bat_preparation
end type
type cb_exit from u_cb within w_bat_preparation
end type
end forward

global type w_bat_preparation from w_sheet
integer x = 165
integer y = 128
integer width = 2486
integer height = 964
string title = "Batch Preparation"
boolean maxbox = false
dw_batch_prepare dw_batch_prepare
cb_update cb_update
cb_exit cb_exit
end type
global w_bat_preparation w_bat_preparation

on w_bat_preparation.create
int iCurrent
call super::create
this.dw_batch_prepare=create dw_batch_prepare
this.cb_update=create cb_update
this.cb_exit=create cb_exit
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_batch_prepare
this.Control[iCurrent+2]=this.cb_update
this.Control[iCurrent+3]=this.cb_exit
end on

on w_bat_preparation.destroy
call super::destroy
destroy(this.dw_batch_prepare)
destroy(this.cb_update)
destroy(this.cb_exit)
end on

event mousemove;call super::mousemove;
w_pics_main.setmicrohelp("Ready")
end event

event pfc_postopen;call super::pfc_postopen;string ls_cntr
long ll_cascd

Open(w_pics_retrieve_msg_box)
m_pics_main.m_Edit.m_Addrow.Enabled = FALSE
m_pics_main.m_Edit.m_Deleterow.Enabled = FALSE
m_pics_main.m_Edit.m_Cut.Enabled = FALSE
dw_batch_prepare.settransobject(sqlservertrans)
dw_batch_prepare.Event pfc_addrow()
SELECT COUNT(*)
 INTO :ll_cascd
 FROM mchar
 WHERE mchar.cascd = 'N'
 USING sqlservertrans; 
 dw_batch_prepare.Setitem(1,'N',ll_cascd)
 IF NOT ll_cascd > 0 THEN
	SELECT COUNT(*)
   INTO :ll_cascd
   FROM mchar
   WHERE mchar.cascd = 'Q'
   USING sqlservertrans;
	dw_batch_prepare.Setitem(1,'Q',ll_cascd)
	SELECT COUNT(*)
   INTO :ls_cntr
   FROM prod,mchar,ancntr
   WHERE (prod.cntr = ancntr.cntr) and
   (mchar.bkseq = prod.bkseq)and
   ((prod.actenddt is not NULL)and
   (mchar.cascd is NULL)and
   (ancntr.prdr is not NULL and
    ancntr.cntrcvcd <> 'C'))
   USING sqlservertrans;
//	Close(w_pics_retrieve_msg_box)
   dw_batch_prepare.Setitem(1,"prod_cntr",ls_cntr)
	Close(w_pics_retrieve_msg_box)
	Messagebox("SORRY","No rows found to update the records from 'N' to 'Q'.")
	ib_disableclosequery = TRUE
	cb_update.Visible = FALSE
	CLOSE(w_bat_preparation)
ELSE
	Open(w_pics_retrieve_msg_box)
	SELECT COUNT(*)
   INTO :ll_cascd
   FROM mchar
   WHERE mchar.cascd = 'Q'
   USING sqlservertrans;
   dw_batch_prepare.Setitem(1,'Q',ll_cascd)
   SELECT count(*)
   INTO :ls_cntr
   FROM prod,mchar,ancntr
   WHERE (prod.cntr = ancntr.cntr) and
   (mchar.bkseq = prod.bkseq)and
   ((prod.actenddt is not NULL)and
   (mchar.cascd is NULL)and
   (ancntr.prdr is not NULL and
    ancntr.cntrcvcd <> 'C'))
   USING sqlservertrans;
	Close(w_pics_retrieve_msg_box)
   dw_batch_prepare.Setitem(1,"prod_cntr",ls_cntr)
   dw_batch_prepare.setfocus()
   cb_update.Enabled = TRUE
   cb_update.Visible = TRUE
END IF


end event

event pfc_preopen;call super::pfc_preopen;this.of_SetPreference(TRUE)
this.inv_preference.of_SetToolBars(TRUE)
this.inv_preference.of_SetWindow(TRUE)
this.of_SetResize(TRUE)
this.inv_resize.of_SetOrigSize(this.workspacewidth(),this.workspaceheight())
inv_resize.of_Register(dw_batch_prepare, "scale")
inv_resize.of_Register(cb_update, "scale")
inv_resize.of_Register(cb_exit, "scale")


end event

event resize;call super::resize;long ll_height

This.X = 220
This.Y = 233
THIS.width = 2483
This.height = 965
//ll_height = w_pics_main.mdi_1.Height
//This.Resize(w_pics_main.WorkSpaceWidth(), ll_height)
end event

type dw_batch_prepare from u_dw within w_bat_preparation
integer x = 5
integer y = 12
integer width = 2427
integer height = 656
integer taborder = 10
string dataobject = "d_batch_preparation"
boolean vscrollbar = false
boolean livescroll = false
end type

event constructor;call super::constructor;ib_rmbmenu = FALSE
end event

event getfocus;call super::getfocus;m_pics_main.m_Edit.m_Addrow.Enabled = FALSE
m_pics_main.m_Edit.m_Deleterow.Enabled = FALSE
m_pics_main.m_Edit.m_Cut.Enabled = FALSE
end event

type cb_update from u_cb within w_bat_preparation
event pfc_hinttext pbm_mousemove
string tag = "Click here to update the records from ~" N ~" to ~" Q ~""
integer x = 1541
integer y = 732
integer taborder = 0
integer textsize = -10
boolean enabled = false
string text = "&Update"
end type

event pfc_hinttext;call super::pfc_hinttext;w_pics_main.Setmicrohelp(THIS.tag)
end event

event clicked;call super::clicked;integer rtn

rtn = Messagebox("UPDATE","Do you want to update the records",Question!,YESNO!,1)
IF rtn = 1 THEN
	Open(w_pics_update_msg_box)
  UPDATE mchar  
     SET cascd = 'Q'  
   WHERE mchar.cascd = 'N'
   USING sqlservertrans;	
	cb_update.Visible = FALSE
	ib_disableclosequery = TRUE
	Close(w_pics_update_msg_box)
	Messagebox("Success","Update successful")
	ib_disableclosequery = TRUE
	Parent.Event pfc_close()
ELSE
	ib_disableclosequery = TRUE
	Parent.Event pfc_close()	
END IF

end event

type cb_exit from u_cb within w_bat_preparation
event pfc_hinttext pbm_mousemove
string tag = "Exits the current Screen"
integer x = 2007
integer y = 732
integer taborder = 0
integer textsize = -10
string text = "E&xit"
end type

event pfc_hinttext;call super::pfc_hinttext;w_pics_main.Setmicrohelp(THIS.tag)
end event

event clicked;call super::clicked;ib_disableclosequery = TRUE
parent.Event pfc_close()
m_pics_main.m_menu.popMenu(300,0)
end event


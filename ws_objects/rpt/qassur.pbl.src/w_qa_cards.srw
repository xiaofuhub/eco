$PBExportHeader$w_qa_cards.srw
forward
global type w_qa_cards from w_response
end type
type dw_qas_prdrqastg from u_pics_dw within w_qa_cards
end type
type cb_update_web from u_cb within w_qa_cards
end type
type cb_clear from u_cb within w_qa_cards
end type
type st_1 from statictext within w_qa_cards
end type
type cb_spell from commandbutton within w_qa_cards
end type
type mle_comments from u_mle within w_qa_cards
end type
type dw_qas_cards_comments from u_pics_dw within w_qa_cards
end type
type dw_qas_cards from u_pics_dw within w_qa_cards
end type
type cb_exit from u_cb within w_qa_cards
end type
type cb_update from u_cb within w_qa_cards
end type
type cb_del from u_cb within w_qa_cards
end type
end forward

global type w_qa_cards from w_response
integer x = 997
integer y = 488
integer width = 3246
integer height = 2040
string title = "QAS Cards"
windowstate windowstate = maximized!
dw_qas_prdrqastg dw_qas_prdrqastg
cb_update_web cb_update_web
cb_clear cb_clear
st_1 st_1
cb_spell cb_spell
mle_comments mle_comments
dw_qas_cards_comments dw_qas_cards_comments
dw_qas_cards dw_qas_cards
cb_exit cb_exit
cb_update cb_update
cb_del cb_del
end type
global w_qa_cards w_qa_cards

type variables


end variables

on w_qa_cards.create
int iCurrent
call super::create
this.dw_qas_prdrqastg=create dw_qas_prdrqastg
this.cb_update_web=create cb_update_web
this.cb_clear=create cb_clear
this.st_1=create st_1
this.cb_spell=create cb_spell
this.mle_comments=create mle_comments
this.dw_qas_cards_comments=create dw_qas_cards_comments
this.dw_qas_cards=create dw_qas_cards
this.cb_exit=create cb_exit
this.cb_update=create cb_update
this.cb_del=create cb_del
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_qas_prdrqastg
this.Control[iCurrent+2]=this.cb_update_web
this.Control[iCurrent+3]=this.cb_clear
this.Control[iCurrent+4]=this.st_1
this.Control[iCurrent+5]=this.cb_spell
this.Control[iCurrent+6]=this.mle_comments
this.Control[iCurrent+7]=this.dw_qas_cards_comments
this.Control[iCurrent+8]=this.dw_qas_cards
this.Control[iCurrent+9]=this.cb_exit
this.Control[iCurrent+10]=this.cb_update
this.Control[iCurrent+11]=this.cb_del
end on

on w_qa_cards.destroy
call super::destroy
destroy(this.dw_qas_prdrqastg)
destroy(this.cb_update_web)
destroy(this.cb_clear)
destroy(this.st_1)
destroy(this.cb_spell)
destroy(this.mle_comments)
destroy(this.dw_qas_cards_comments)
destroy(this.dw_qas_cards)
destroy(this.cb_exit)
destroy(this.cb_update)
destroy(this.cb_del)
end on

event resize;call super::resize;long ll_height

This.X = w_pics_main.X
This.Y = w_pics_main.Y
ll_height = w_pics_main.mdi_1.Height
This.Resize(w_pics_main.WorkSpaceWidth(), ll_height)

end event

event pfc_preopen;call super::pfc_preopen;this.of_SetPreference(TRUE)
this.inv_preference.of_SetToolBars(TRUE)
this.inv_preference.of_SetWindow(TRUE)
this.of_SetResize(TRUE)
this.inv_resize.of_SetOrigSize(this.workspacewidth(),this.workspaceheight())
inv_resize.of_Register(dw_qas_cards, "Scale")
inv_resize.of_Register(dw_qas_cards_comments, "Scale")
inv_resize.of_Register(dw_qas_prdrqastg, "Scale")
inv_resize.of_Register(mle_comments, "Scale")
inv_resize.of_Register(cb_del, "Scale")
inv_resize.of_Register(cb_exit, "Scale")
inv_resize.of_Register(cb_update, "Scale")
inv_resize.of_Register(cb_update_web, "Scale")
inv_resize.of_Register(cb_clear, "Scale")
inv_resize.of_Register(cb_spell, "Scale")
inv_resize.of_Register(st_1, "Scale")

end event

type dw_qas_prdrqastg from u_pics_dw within w_qa_cards
boolean visible = false
integer x = 2030
integer y = 1208
integer width = 119
integer height = 68
integer taborder = 0
string dataobject = "d_qas_prdrqastg"
boolean vscrollbar = false
boolean livescroll = false
end type

event constructor;call super::constructor;this.SetTransObject(sqlserveroracletrans) 

end event

type cb_update_web from u_cb within w_qa_cards
string tag = "Update Comments on the WEB"
integer x = 654
integer y = 1820
integer width = 750
integer taborder = 20
boolean bringtotop = true
string text = "&Update Comments on the WEB"
end type

event clicked;call super::clicked;int rc,rtn,ll_rows
string lqastg, lbkmed, lbkno, lqastatcd, lcntr, lqacomments
long lbkseq
date lqarecdt, lqacompdt

lqastg = dw_qas_cards.object.qastg[1]
lbkseq = dw_qas_cards.object.bkseq[1]
lbkmed = dw_qas_cards.object.bkmed[1]
lbkno = TRIM(lbkmed)+TRIM(string(lbkseq))
lcntr = dw_qas_cards.object.cntr[1]
lqastatcd = dw_qas_cards.object.qastatcd[1]
lqarecdt = dw_qas_cards.object.qarecdt[1]
lqacompdt = dw_qas_cards.object.qacompdt[1]


dw_qas_cards.object.qacomments[1] = mle_comments.text

lqacomments = mle_comments.text

dw_qas_cards.AcceptText()

rc = dw_qas_cards.Event pfc_update(TRUE,FALSE)
if rc = 1 THEN
	// If we have lost our connection to oracle, reconnect.
	IF SQLServerOracleTrans.DBHandle() = 0 THEN
		
		openwithparm(w_pics_retrieve_msg_box,"Making the connection to Oracle, Please Wait...")
		
		SqlServerOracleTrans.DBMS = "O73 ORACLE 7.3"
		SqlServerOracleTrans.LogPass = "picadmin"
		SqlServerOracleTrans.ServerName = "@TNS:ORAPICS"
		SqlServerOracleTrans.LogId = "picadmin"
		SqlServerOracleTrans.AutoCommit = False
		SqlServerOracleTrans.DBParm = "PBCatalogOwner='PICADMIN'"
		SqlServerOracleTrans.of_connect()
		IF SqlServerOracleTrans.sqlcode <> 0 THEN
			close(w_pics_retrieve_msg_box)
			IF SqlServerOracleTrans.SqlDbcode = -951 THEN            //check for invalid userid
				MessageBox("Login Error","Invalid User ID/Password. Please re-enter.",StopSign!)
				Return 
			ELSEIF SqlServerOracleTrans.SqlDBcode = -952 THEN       //check for invalid password
				MessageBox("Login Error","Invalid User ID/Password. Please re-enter.",StopSign!)
				Return 		
			Else                                             //check for other error messages
				MessageBox("Database Connection Error","Unable to Connect. " +& 
				string(SqlServerOracleTrans.sqldbcode) + " " +&
				SqlServerOracleTrans.SQLErrText, &
				StopSign!)
				Return 
			END IF
		ELSE
			close(w_pics_retrieve_msg_box)
		END IF
		
	END IF

	dw_qas_prdrqastg.of_SetTransObject(sqlserveroracletrans)
	
	ll_rows = dw_qas_prdrqastg.retrieve(lbkmed, lcntr, lqastg, lbkseq)
	
	//Messagebox("Info","book number="+lbkno+" cntr = "+lcntr+" qastg = "+lqastg+" qastatcd = "+lqastatcd+" qarecdt= "+string(lqarecdt)+" qacompdt= "+string(lqacompdt)+" qacomments = "+lqacomments)
	
	IF ll_rows = 0 THEN
		//Messagebox("info"," Qastg doesn't exist. add it.")
		// qastg doesn't exist on the web. create it.
		dw_qas_prdrqastg.InsertRow(0)
		dw_qas_prdrqastg.object.bkno[1] 	= TRIM(lbkno)
		dw_qas_prdrqastg.object.bkseq[1] = lbkseq
		dw_qas_prdrqastg.object.bkmed[1]	= TRIM(lbkmed)
		dw_qas_prdrqastg.object.cntr[1] 	= TRIM(lcntr)
		dw_qas_prdrqastg.object.qastg[1] = TRIM(lqastg)
		dw_qas_prdrqastg.object.qarecdt[1] 	= datetime(lqarecdt)
		dw_qas_prdrqastg.object.qastatcd[1] = TRIM(lqastatcd)
		dw_qas_prdrqastg.object.qacompdt[1] 	= datetime(lqacompdt)
		dw_qas_prdrqastg.object.qacomments[1] 	= TRIM(lqacomments)
		dw_qas_prdrqastg.object.prdr[1] 	= ""
		dw_qas_prdrqastg.object.subprdr[1] 	= ""

	ELSE
		// Record exist, just update it.
		//Messagebox("info"," Qastg exist. update it.")
		dw_qas_prdrqastg.object.bkno[1] 	= TRIM(lbkno)
		dw_qas_prdrqastg.object.bkseq[1] = lbkseq
		dw_qas_prdrqastg.object.bkmed[1]	= TRIM(lbkmed)
		dw_qas_prdrqastg.object.cntr[1] 	= TRIM(lcntr)
		dw_qas_prdrqastg.object.qastg[1] = TRIM(lqastg)
		dw_qas_prdrqastg.object.qarecdt[1] 	= datetime(lqarecdt)
		dw_qas_prdrqastg.object.qastatcd[1] = TRIM(lqastatcd)
		dw_qas_prdrqastg.object.qacompdt[1] 	= datetime(lqacompdt)
		dw_qas_prdrqastg.object.qacomments[1] 	= TRIM(lqacomments)
	
	END IF
	rtn = dw_qas_prdrqastg.update()
	IF rtn=1 THEN
		Commit Using SQlServerOracleTrans;
		COMMIT USING SQLServerTrans;
		
		dw_qas_cards.ResetUpdate()
		MessageBox("Update Information","Comments added to the qastg "+lqastg+" of the book number: "+lbkno+".",Information!)
		
		dw_qas_cards_comments.SetTransObject(sqlservertrans) 
		dw_qas_cards.SetTransObject(sqlservertrans) 
			
		dw_qas_cards.retrieve(lbkseq, lbkmed, lqastg)
		dw_qas_cards_comments.Retrieve(lbkseq, lbkmed)
		
		RETURN 1
	ELSE
		Rollback using SqlServerOracleTrans;
		MessageBox("ERROR","Update failed.",StopSign!)
		RETURN 0
	END IF
	
else
	ROLLBACK USING SQLServerTrans;
	MessageBox("ERROR","Update failed.",StopSign!)
	RETURN 0
end if
end event

type cb_clear from u_cb within w_qa_cards
string tag = "Clear Comments"
integer x = 1449
integer y = 1820
integer width = 471
integer taborder = 30
boolean bringtotop = true
string text = "&Clear Comments"
end type

event clicked;call super::clicked;int rtn

IF mle_comments.text<>"" THEN
	rtn = MessageBox("Clear","Are you sure you want to clear the comments?",question!,yesno!,1)
	if rtn = 1 then
		mle_comments.text=""
		mle_comments.SetFocus()
	end if
END IF
	


end event

type st_1 from statictext within w_qa_cards
integer x = 27
integer y = 1212
integer width = 1554
integer height = 68
integer textsize = -11
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = roman!
string facename = "Times New Roman"
boolean underline = true
long textcolor = 33554432
long backcolor = 67108864
boolean focusrectangle = false
end type

type cb_spell from commandbutton within w_qa_cards
string tag = "Spell Checker"
integer x = 32
integer y = 1816
integer width = 334
integer height = 96
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "&Spell Check"
end type

event clicked;nca_word lnca_word
String ls_S
String lcomments
int ll_row

ls_S = mle_comments.text

lnca_Word.SpellCheck( ls_S )

mle_comments.text = ls_S

mle_comments.SetFocus ()
	

end event

type mle_comments from u_mle within w_qa_cards
event pfc_hinttext pbm_mousemove
event ue_char_entered pbm_char
event ue_key_up pbm_keyup
event ue_lmouse_up pbm_lbuttonup
string tag = "Please enter the comments"
integer x = 14
integer y = 1288
integer width = 3191
integer height = 516
integer taborder = 10
integer textsize = -11
fontfamily fontfamily = roman!
string facename = "Times New Roman"
boolean vscrollbar = true
integer limit = 512
end type

event pfc_hinttext;w_pics_main.setmicrohelp(mle_comments.tag)

end event

event getfocus;call super::getfocus;m_pics_main.m_edit.m_deleterow.Enabled 	=	FALSE
m_pics_main.m_edit.m_addrow.Enabled 		=	FALSE

end event

type dw_qas_cards_comments from u_pics_dw within w_qa_cards
integer x = 14
integer y = 676
integer width = 3195
integer height = 516
integer taborder = 0
string dataobject = "d_qas_cards_comments"
end type

event ue_postconstructor();call super::ue_postconstructor;this.SetTransObject(sqlservertrans) 

end event

event rbuttondown;call super::rbuttondown;//
end event

event rbuttonup;call super::rbuttonup;//
end event

type dw_qas_cards from u_pics_dw within w_qa_cards
integer x = 14
integer y = 20
integer width = 3195
integer height = 656
integer taborder = 0
string dataobject = "d_qas_cards"
boolean vscrollbar = false
boolean livescroll = false
end type

event ue_postconstructor();call super::ue_postconstructor;string lbkmed, lqastg
long lbkseq
int ll_rows

this.SetTransObject(sqlservertrans) 
str_qa_comments lstr_qa_comments

lstr_qa_comments = Message.PowerObjectParm

lbkseq = lstr_qa_comments.bkseq
lbkmed = lstr_qa_comments.bkmed
lqastg = lstr_qa_comments.qastg

//MessageBox("rows","bkseq = "+string(lbkseq)+" bkmed = "+lbkmed+" qastg = "+lqastg)

ll_rows = this.retrieve(lbkseq, lbkmed, lqastg)
IF ll_rows > 0 THEN
	dw_qas_cards_comments.SetTransObject(sqlservertrans) 
	dw_qas_cards_comments.Retrieve(lbkseq, lbkmed)
	st_1.text = "Enter your comments for stage "+lqastg+" at the designated space below."
	mle_comments.text = this.object.qacomments[ll_rows]
	mle_comments.SetFocus()
END IF

end event

type cb_exit from u_cb within w_qa_cards
string tag = "exit "
integer x = 2939
integer y = 1820
integer width = 274
integer taborder = 60
string text = "Ex&it"
end type

event clicked;call super::clicked;close(parent)
end event

type cb_update from u_cb within w_qa_cards
string tag = "Update Comments"
integer x = 1957
integer y = 1820
integer width = 471
integer taborder = 40
boolean bringtotop = true
string text = "&Update Comments"
end type

event clicked;call super::clicked;int rc,rtn
string lqastg, lbkmed
long lbkseq

lqastg = dw_qas_cards.object.qastg[1]
lbkseq = dw_qas_cards.object.bkseq[1]
lbkmed = dw_qas_cards.object.bkmed[1]

dw_qas_cards.object.qacomments[1] = mle_comments.text

dw_qas_cards.AcceptText()

rc = dw_qas_cards.Event pfc_update(TRUE,FALSE)
if rc = 1 THEN
	dw_qas_cards.ResetUpdate()
	COMMIT USING SQLServerTrans;
	MessageBox("Update","Comments was added to qastg "+lqastg+".",Information!)
	
	dw_qas_cards_comments.SetTransObject(sqlservertrans) 
	dw_qas_cards.SetTransObject(sqlservertrans) 
		
	dw_qas_cards.retrieve(lbkseq, lbkmed, lqastg)
	dw_qas_cards_comments.Retrieve(lbkseq, lbkmed)
	
	RETURN 1
else
	ROLLBACK USING SQLServerTrans;
	MessageBox("ERROR","Update failed.",StopSign!)
	RETURN 0
end if

end event

type cb_del from u_cb within w_qa_cards
string tag = "Delete Comments"
integer x = 2455
integer y = 1820
integer width = 471
integer taborder = 50
boolean bringtotop = true
string text = "&Delete Comments"
boolean default = true
end type

event clicked;call super::clicked;int rtn
string lqastg, lbkmed, lqastatcd
long lbkseq
date lqarecdt

lqastg = dw_qas_cards.object.qastg[1]
lbkseq = dw_qas_cards.object.bkseq[1]
lbkmed = dw_qas_cards.object.bkmed[1]
lqastatcd = dw_qas_cards.object.qastatcd[1]
lqarecdt = dw_qas_cards.object.qarecdt[1]

rtn = MessageBox("Delete Comments","Are you sure you want to remove the comments for qastg "+lqastg+"?")
IF rtn = 1 THEN
	Update qastg
	set qacomments=NULL
	where bkseq = :lbkseq
	and bkmed = :lbkmed
	and qastg = :lqastg
	and qastatcd = :lqastatcd
	and qarecdt = :lqarecdt
	using sqlservertrans;
	IF f_check_dberror(sqlservertrans,"QASTG") THEN
		MessageBox("Update","Comment was removed from qastg "+lqastg+".")
		COMMIT USING SQLServerTrans;
		
		dw_qas_cards_comments.SetTransObject(sqlservertrans) 
		dw_qas_cards.SetTransObject(sqlservertrans) 
		
		dw_qas_cards.retrieve(lbkseq, lbkmed, lqastg)
		dw_qas_cards_comments.Retrieve(lbkseq, lbkmed)
		
		mle_comments.text=""
		mle_comments.SetFocus()
	ELSE
		MessageBox("ERROR","Error removing comments from qastg table.")
		ROLLBACK USING SQLServerTrans;
	END IF
END IF

end event


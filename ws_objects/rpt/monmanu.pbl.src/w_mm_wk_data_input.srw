$PBExportHeader$w_mm_wk_data_input.srw
forward
global type w_mm_wk_data_input from w_sheet
end type
type dw_manu_bkno from u_pics_dw within w_mm_wk_data_input
end type
type cb_find from commandbutton within w_mm_wk_data_input
end type
type cb_update from commandbutton within w_mm_wk_data_input
end type
type cb_clear from commandbutton within w_mm_wk_data_input
end type
type cb_exit from commandbutton within w_mm_wk_data_input
end type
type sle_prdr from u_sle within w_mm_wk_data_input
end type
type st_2 from statictext within w_mm_wk_data_input
end type
type sle_rows_updated from singlelineedit within w_mm_wk_data_input
end type
end forward

global type w_mm_wk_data_input from w_sheet
boolean visible = false
integer x = 5
integer y = 4
integer width = 3122
integer height = 1940
string title = "Weekly Data Input"
dw_manu_bkno dw_manu_bkno
cb_find cb_find
cb_update cb_update
cb_clear cb_clear
cb_exit cb_exit
sle_prdr sle_prdr
st_2 st_2
sle_rows_updated sle_rows_updated
end type
global w_mm_wk_data_input w_mm_wk_data_input

type variables

end variables

forward prototypes
public function boolean wf_valid_book (integer row, long lbkno, string lprdr)
public function boolean wf_find_book_no (integer row, long lbkno, string lprdr)
end prototypes

public function boolean wf_valid_book (integer row, long lbkno, string lprdr);int lcount=0,i
string Lcntrtype,lproducer,lmsg

// Select record from prod table to check the validity of the book number.
select count(*) into :lcount from prod
where bkseq = :Lbkno
using sqlservertrans;
IF f_check_dberror(sqlservertrans,"PROD")=TRUE THEN
	// If there were no errors
	IF lcount > 0 THEN
		// If at least one row was found. 
		// Select the contract type assigned for this book.
		select DISTINCT cntrtype into :lcntrtype from ancntr 
			where prdr = :Lprdr and	cntr in (select cntr from prod where bkseq=:Lbkno)
		using sqlservertrans;
		IF f_check_dberror(sqlservertrans,"ANCNTR Selecting contract type.")=FALSE THEN
			RETURN FALSE
		END IF
		// Select the producer assigned to work on this book.
		select DISTINCT prdr into :lproducer from ancntr
			where cntr in (select cntr from prod where bkseq=:Lbkno) and cntrtype=:lcntrtype and prdr=:lprdr
		using sqlservertrans;
		IF f_check_dberror(sqlservertrans,"ANCNTR Selecting producer.")=TRUE THEN
			// If there were no errors
			//MessageBox("data","lprdr="+lprdr+" "+"lproducer="+lproducer+"Lcntrtype="+Lcntrtype)
			// Make sure that the producer is the same that we are looking for.
			IF TRIM(lprdr) = TRIM(lproducer) THEN
				// Go through all the books that we selected, and check that books are not redundant.
				FOR i=1 to dw_manu_bkno.RowCount()-1
					IF dw_manu_bkno.object.prod_bkseq[i] = lbkno THEN
						dw_manu_bkno.Object.prod_bkseq.ValidationMsg= " Book Number is already selected! "
						RETURN FALSE
					END IF
				NEXT
				// The book is validated, RETURN TRUE
				RETURN TRUE
			// If the prodcuer that we retrieved from database is NULL
			ELSEIF (lproducer="" OR IsNull(lproducer)) THEN
				// First check to which producer was suppose to do this book. In spite of the contract type.
				select DISTINCT prdr into :lproducer from ancntr
					where cntr in (select cntr from prod where bkseq=:Lbkno)
				using sqlservertrans;
				IF f_check_dberror(sqlservertrans,"ANCNTR Selecting producer.")=TRUE THEN
					// If producers don't match, display a message that this book is done with a different producer.
					lmsg = "Book number: "+string(lbkno)+" was done by producer "+lproducer
					dw_manu_bkno.Object.prod_bkseq.ValidationMsg= lmsg
					RETURN FALSE
				ELSE
					RETURN FALSE
				END IF
			// If the producer that was assigned and the one we are looking for are different.
			ELSE
				// If producers don't match, display a message that this book is done with a different producer.
				lmsg = "Book number: "+string(lbkno)+" was done by producer "+lproducer
				dw_manu_bkno.Object.prod_bkseq.ValidationMsg= lmsg
				RETURN FALSE
			END IF
		ELSE
			RETURN FALSE
		END IF
	ELSEIF lcount=0 THEN
		dw_manu_bkno.Object.prod_bkseq.ValidationMsg='Invalid book number.'
		RETURN FALSE
	END IF
ELSE
	dw_manu_bkno.Object.prod_bkseq.ValidationMsg='Invalid book number.'
	RETURN FALSE
END IF
		
		
		
end function

public function boolean wf_find_book_no (integer row, long lbkno, string lprdr);int llen,Lmaxstageorder
string Lcntrtype,Lcntrmed,xconno,xttl,xcntrtype,xbkmed,xcntr,Lprod_stage[2],Lcontract_no,Lfirststage
long xbkno
date stdt,comp,ship,mstdt

select DISTINCT cntrtype,cntrmed,cntr into :Lcntrtype,:Lcntrmed,:Lcontract_no from ancntr 
where prdr = :Lprdr and
	cntr in (select cntr from prod where bkseq=:Lbkno)
using sqlservertrans;

IF f_check_dberror(sqlservertrans,"ANCNTR")=TRUE THEN
	select prodstage into :Lfirststage 
	from prod
	where bkseq=:Lbkno
	and cntr=:Lcontract_no
	and prodstage in ('MA','AB')
	Using Sqlservertrans;
	IF f_check_dberror(sqlservertrans,"PROD")=FALSE THEN
		RETURN FALSE
	END IF
	//MessageBox("cntr","contract number = "+Lcontract_no+"book number = "+string(Lbkno)+" Prodstage = "+Lfirststage)
	// Use Lmaxstageorder to find out how many stages will it takes
	// to complete the production of each of these books.
	select max(stageorder) into :Lmaxstageorder from prodstage
		where cntrtype = :Lcntrtype
		and	cntrmed = :Lcntrmed
	Using sqlservertrans;
	// If it take more than one stage.
	IF Lmaxstageorder > 1 THEN
		// First production stage will be the mastering stage.
		IF Lfirststage="AB" THEN
			Lprod_stage[1]="AB"
		ELSE
			select prodstage into :Lprod_stage[1] from prodstage
				where cntrtype = :Lcntrtype
				and   cntrmed = :Lcntrmed
				and	(alternate <> 'Y' OR alternate is NULL)
				and 	stageorder = 1
			using sqlservertrans;
			IF f_check_dberror(sqlservertrans,"PRODSTAGE")=FALSE THEN
				RETURN FALSE
			END IF
		END IF
		// Last production stage will the duplication stage.
		select prodstage into :Lprod_stage[2] from prodstage
			where cntrtype = :Lcntrtype
			and   cntrmed = :Lcntrmed
			and	(alternate <> 'Y' OR alternate is NULL)
			and 	stageorder = :Lmaxstageorder
		using sqlservertrans;
		IF f_check_dberror(sqlservertrans,"PRODSTAGE")=FALSE THEN
			RETURN FALSE
		END IF
	ELSE
			// It takes only one stage to complete this book.(mastering stage)
			select prodstage into :Lprod_stage[1] from prodstage
				where cntrtype = :Lcntrtype
				and   cntrmed = :Lcntrmed
				and	(alternate <> 'Y' OR alternate is NULL)
				and 	stageorder = 1
			using sqlservertrans;
			IF f_check_dberror(sqlservertrans,"PRODSTAGE")=FALSE THEN
				RETURN FALSE
			END IF
		END IF			

	IF Lcntrtype <> 'D' THEN
		
		if Lmaxstageorder > 1 then
			select prod1.bkseq,prod1.bkmed,prod1.cntr,prod1.actstdt,
					prod1.actenddt,prod2.actenddt,mchar.pcrecmstdt,mchar.conno,
					ttlinit.ttl,'T',mchar.len
				into :xbkno,:xbkmed,:xcntr,:stdt,:comp,:ship,:mstdt,:xconno,:xttl,:xcntrtype,:llen
				from prod prod1,prod prod2,ttlinit,mchar
				where prod1.bkseq = :Lbkno
				and prod1.bkseq = prod2.bkseq
				and prod2.prodstage = :Lprod_stage[2]
				and prod1.prodstage = :Lprod_stage[1]
				and prod1.prodstage <> prod2.prodstage
				and prod1.bkseq = mchar.bkseq
				and mchar.chno = ttlinit.chno
				using sqlservertrans;
		elseif Lmaxstageorder = 1 then
					select prod.bkseq,prod.bkmed,prod.cntr,prod.actstdt,prod.actenddt,
					mchar.pcrecmstdt,mchar.conno,ttlinit.ttl,'M',mchar.len
					 into :xbkno,:xbkmed,:xcntr,:stdt,:comp,:mstdt,:xconno,:xttl,:xcntrtype,:llen
					from prod,mchar,ttlinit
					where prod.bkseq = :Lbkno
					and prod.bkseq = mchar.bkseq
					and prod.bkmed = mchar.bkmed
					and mchar.chno = ttlinit.chno
					and prod.prodstage = :Lprod_stage[1]
					using sqlservertrans;
		end if		
		IF Lcntrtype="T" THEN
			string Lps
			IF Lcntrmed = 'BR' THEN
				Lps = 'PR'
			ELSEIF Lcntrmed = 'P/B' THEN
				Lps = 'PB'
			ELSEIF Lcntrmed = 'RC' THEN
				Lps = 'DU'
			ELSE
				Lps = 'DU'
			END IF
			SELECT actenddt INTO :ship FROM PROD 
			WHERE bkseq = :xbkno AND prodstage = :Lps AND cntr = :xcntr
			USING SQLServerTrans;
		END IF

	elseif lcntrtype = 'D'  then
			
		select prod.bkseq,prod.bkmed,prod.cntr,prod.actstdt,prod.actenddt,
				mchar.pcrecmstdt,mchar.conno,ttlinit.ttl,'D',mchar.len
		into :xbkno,:xbkmed,:xcntr,:stdt,:ship,:mstdt,:xconno,:xttl,:xcntrtype,:llen
		from prod,mchar,ttlinit
		where prod.bkseq = :Lbkno
			  	and prod.bkseq = mchar.bkseq
			  	and prod.bkmed = mchar.bkmed
			  	and mchar.chno = ttlinit.chno
			  	and prod.prodstage = 'DU'
		using sqlservertrans;
			
	else
			return FALSE			
	end if
			
	dw_manu_bkno.object.prod_bkseq[row] 	= xbkno
	dw_manu_bkno.object.prod_bkmed[row] 	= xbkmed
	dw_manu_bkno.object.prod_cntr[row] 	= xcntr
	dw_manu_bkno.object.ttlinit_ttl[row] 		= xttl
	dw_manu_bkno.object.prod_actstdt[row] = stdt
	dw_manu_bkno.object.ccompdate[row] = comp
	dw_manu_bkno.object.cshipdate[row] = ship
	dw_manu_bkno.object.mchar_pcrecmstdt[row] = mstdt
	dw_manu_bkno.object.mchar_len[row] = llen
	dw_manu_bkno.object.mchar_conno[row] = xconno
	dw_manu_bkno.object.ancntr_cntrtype[row] = lcntrtype
	RETURN TRUE
ELSE
	dw_manu_bkno.Object.prod_bkseq.ValidationMsg='Invalid producer code for this book.'
	RETURN FALSE
END IF	
end function

on w_mm_wk_data_input.create
int iCurrent
call super::create
this.dw_manu_bkno=create dw_manu_bkno
this.cb_find=create cb_find
this.cb_update=create cb_update
this.cb_clear=create cb_clear
this.cb_exit=create cb_exit
this.sle_prdr=create sle_prdr
this.st_2=create st_2
this.sle_rows_updated=create sle_rows_updated
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_manu_bkno
this.Control[iCurrent+2]=this.cb_find
this.Control[iCurrent+3]=this.cb_update
this.Control[iCurrent+4]=this.cb_clear
this.Control[iCurrent+5]=this.cb_exit
this.Control[iCurrent+6]=this.sle_prdr
this.Control[iCurrent+7]=this.st_2
this.Control[iCurrent+8]=this.sle_rows_updated
end on

on w_mm_wk_data_input.destroy
call super::destroy
destroy(this.dw_manu_bkno)
destroy(this.cb_find)
destroy(this.cb_update)
destroy(this.cb_clear)
destroy(this.cb_exit)
destroy(this.sle_prdr)
destroy(this.st_2)
destroy(this.sle_rows_updated)
end on

event open;call super::open;// Open the sheet in Maximized mode
this.windowstate = maximized!

dw_manu_bkno.Object.prod_actstdt.tabsequence='0'   
dw_manu_bkno.Object.ccompdate.tabsequence='0'   
dw_manu_bkno.Object.cshipdate.tabsequence='0'   
dw_manu_bkno.Object.mchar_pcrecmstdt.tabsequence='0' 

//sle_prdr.setFocus()
//dw_manu_bkno.Enabled=FALSE
cb_find.Enabled=FALSE
dw_manu_bkno.event pfc_addrow()

dw_manu_bkno.Object.prod_bkseq.tabsequence='10'   
dw_manu_bkno.Object.prod_bkseq.Background.Color='16777215'
dw_manu_bkno.Object.prod_actstdt.tabsequence='0'   
dw_manu_bkno.Object.ccompdate.tabsequence='0'   
dw_manu_bkno.Object.cshipdate.tabsequence='0'   
dw_manu_bkno.Object.mchar_pcrecmstdt.tabsequence='0'   


dw_manu_bkno.SetFocus()

cb_clear.Enabled = TRUE
//cb_find.Enabled = TRUE

end event

event pfc_preopen;call super::pfc_preopen;this.of_SetPreference(TRUE)
this.inv_preference.of_SetToolBars(TRUE)
this.inv_preference.of_SetWindow(TRUE)
this.of_SetResize(TRUE)
this.inv_resize.of_SetOrigSize(this.workspacewidth(),this.workspaceheight())
inv_resize.of_Register(dw_manu_bkno, "Scale")
//inv_resize.of_Register(sle_prdr, "Scale")
inv_resize.of_Register(cb_clear, "Scale")
inv_resize.of_Register(cb_exit, "Scale")
inv_resize.of_Register(cb_find, "Scale")
inv_resize.of_Register(cb_update, "Scale")
inv_resize.of_Register(st_2, "Scale")
inv_resize.of_Register(sle_rows_updated, "Scale")


end event

event resize;call super::resize;//long ll_height
//
//This.X = w_pics_main.X
//This.Y = w_pics_main.Y
//ll_height = w_pics_main.mdi_1.Height
//This.Resize(w_pics_main.WorkSpaceWidth(), ll_height)
//
//
end event

event closequery;Integer	li_pendingrc
Integer	li_validationrc
Integer	li_accepttextrc
Integer	li_msg
Integer	li_rc,rtn
String	ls_msgparms[]

// Check if the CloseQuery process has been disabled
If ib_disableclosequery Then
	Return 0
End If

// Call event to perform any pre-CloseQuery processing
If This.Event pfc_preclose ( ) <> 1 Then
	// Prevent the window from closing
	Return 1  
End If

// Prevent validation error messages from appearing while the window is closing
// and allow others to check if the  CloseQuery process is in progress
ib_closestatus = True

// Check for any pending updates
li_rc = of_UpdateChecks()
If li_rc = 0 Then
	// Updates are NOT pending, allow the window to be closed.
	Return 0
ElseIf li_rc < 0 Then
	// There are Updates pending, but at least one data entry error was found.
	// Give the user an opportunity to close the window without saving changes
	If IsValid(gnv_app.inv_error) Then
		li_msg = gnv_app.inv_error.of_Message('pfc_closequery_failsvalidation', &
					 ls_msgparms, gnv_app.iapp_object.DisplayName)
	Else
		li_msg = MessageBox (gnv_app.iapp_object.DisplayName, &
					"The information entered does not pass validation and "  + &
					"must be corrected before changes can be saved.~r~n~r~n" + &
					"Close without saving changes?", &
					exclamation!, YesNo!, 2)
	End If
	If li_msg = 1 Then
		Return 0
	End If
Else
	// Changes are pending, prompt the user to determine if they should be saved
	If IsValid(gnv_app.inv_error) Then
		li_msg = gnv_app.inv_error.of_Message('pfc_closequery_savechanges',  &
					ls_msgparms, gnv_app.iapp_object.DisplayName)		
	Else
		li_msg = MessageBox ( gnv_app.iapp_object.DisplayName, &
					"Do you want to save changes?", exclamation!, YesNoCancel!)
	End If
	Choose Case li_msg
		Case 1
			// YES - Update
			// If the update fails, prevent the window from closing
			rtn = cb_update.Event Clicked()
			if rtn = 1 THEN
				RETURN 0
			end if
		Case 2
			// NO - Allow the window to be closed without saving changes
			Return 0
		Case 3
			// CANCEL -  Prevent the window from closing
	End Choose
End If

// Prevent the window from closing
ib_closestatus = False
Return 1
end event

type dw_manu_bkno from u_pics_dw within w_mm_wk_data_input
event ue_enterkey pbm_dwnprocessenter
event pfc_hinttext pbm_mousemove
integer x = 9
integer y = 12
integer width = 3049
integer height = 1572
integer taborder = 20
string dataobject = "d_manu_bkno"
boolean ib_isupdateable = false
boolean ib_rmbmenu = false
boolean ib_rmbfocuschange = false
string is_updatesallowed = ""
end type

event ue_enterkey;Integer li_ColNbr,currow,rowcount

currow = dw_manu_bkno.GetRow()
rowcount = dw_manu_bkno.RowCount()

li_ColNbr = dw_manu_bkno.GetColumn() 

IF NOT((li_ColNbr=1) AND ( currow >= rowcount)) THEN
//	messagebox('','ue_enterkey event pbm_dwnprocessenter')
	Send(Handle(this),256,9,Long(0,0))
	return(1)
END IF	

end event

event pfc_hinttext;call super::pfc_hinttext;string ls_object, ls_column, ls_column_tag
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

event ue_postconstructor;call super::ue_postconstructor;cb_clear.Enabled = FALSE
cb_find.Enabled = FALSE
cb_update.Enabled = FALSE
// Set the transaction objects
//this.of_SetQuerymode(TRUE)
this.of_SetTransObject( SQLServerTrans )

end event

event rbuttondown;//
end event

event rbuttonup;//
end event

event sqlpreview;call super::sqlpreview;MessageBox("SQL",sqlsyntax)
end event

event itemchanged;call super::itemchanged;//messagebox('','itemchanged event')
IF dwo.name = "prod_bkseq" THEN
	string Lprdr,nullstring
	long Lbkseq
	SetNULL(nullstring)

	Lbkseq = long(data)
	select max(ancntr.prdr) into  :Lprdr
	from ancntr, prod
	where ancntr.cntr=prod.cntr 
		 and  bkseq=:Lbkseq 
	using SqlServerTrans;
	if not f_check_dberror(SqlServerTrans,'select prdr from prod using bkseq') then
		return
	end if
	if IsNull(Lprdr) then
		messagebox(' ','Error, invalid bkno',StopSign!)
		return 1
	end if
	IF f_is_it_archived(nullstring,Lbkseq) THEN
		return 2
	ELSEIF wf_valid_book(row,Lbkseq,Lprdr)=FALSE THEN
		return 1
	ELSE
		dw_manu_bkno.event pfc_addrow()
		cb_find.Enabled = TRUE
	END IF
END IF
end event

event constructor;call super::constructor;string ls_excludecols[]
this.of_SetFilter(TRUE)
this.of_SetFind(TRUE)
this.inv_filter.of_SetStyle(1)
this.inv_filter.of_SetColumnNameSource(2)
ls_excludecols[1] = "graycolor"
ls_excludecols[2] = "whitecolor"
this.inv_filter.of_SetExclude(ls_excludecols)
this.of_SetSort(TRUE)
this.inv_sort.of_SetStyle(1)
this.inv_sort.of_SetColumnNameSource(2)
this.inv_sort.of_SetExclude(ls_excludecols)
this.of_SetDropDownCalendar(TRUE)
this.iuo_calendar.of_Register("prod_actstdt",this.iuo_calendar.DDLB)
this.iuo_calendar.of_Register("ccompdate",this.iuo_calendar.DDLB)
this.iuo_calendar.of_Register("cshipdate",this.iuo_calendar.DDLB)
this.iuo_calendar.of_Register("mchar_pcrecmstdt",this.iuo_calendar.DDLB)

end event

type cb_find from commandbutton within w_mm_wk_data_input
event clicked pbm_bnclicked
event getfocus pbm_bnsetfocus
event mousemove pbm_mousemove
string tag = "Find the record"
integer x = 1897
integer y = 1600
integer width = 238
integer height = 108
integer textsize = -10
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
boolean enabled = false
string text = "F&ind "
end type

event clicked;int i,rowcount,ll_rows
String Lprdr,Lcntr,Lcntrtype
long Lbkseq
date Lshipdate

//Lprdr = sle_prdr.text
SetNull(Lcntr)

rowcount = dw_manu_bkno.RowCount()
	
open(w_pics_retrieve_msg_box)
dw_manu_bkno.AcceptText()
FOR i=1 TO rowcount
	IF NOT(IsNull(dw_manu_bkno.object.prod_bkseq[i])) THEN
		Lbkseq = dw_manu_bkno.object.prod_bkseq[i]
		select max(ancntr.prdr) into  :Lprdr
		from ancntr, prod
		where ancntr.cntr=prod.cntr 
			 and  bkseq=:Lbkseq 
		using SqlServerTrans;
		if not f_check_dberror(SqlServerTrans,'select prdr from prod using bkseq') then
			return
		end if
		IF wf_find_book_no(i,Lbkseq,Lprdr)=FALSE THEN
			dw_manu_bkno.deleterow(i)
		END IF
		dw_manu_bkno.object.dupflg[i]	= 'N'
		Select cntr into :Lcntr
		from prod
		where bkseq=:Lbkseq
		and prodstage in ('DU','PB')
		using sqlservertrans;
		IF f_check_dberror(sqlservertrans,"PROD Selecting contract number.")=FALSE THEN
			RETURN
		ELSE
			IF Lcntr<>"" AND NOT(IsNull(Lcntr)) THEN
				Select cntrtype into :Lcntrtype
				from ancntr
				where cntr = :lcntr
				using sqlservertrans;
				
				IF Lcntrtype = 'D' THEN
					Select actenddt into :Lshipdate
					from prod
					where bkseq=:Lbkseq
					and cntr = :Lcntr
					and prodstage in ('DU','PB')
					using sqlservertrans;
					IF f_check_dberror(sqlservertrans,"PROD Selecting contract number.")=FALSE THEN
						RETURN
					ELSE
						dw_manu_bkno.object.cshipdate[i] = Lshipdate
						dw_manu_bkno.object.dupflg[i] = 'Y'
					END IF
				END IF
			END IF
		END IF
	ELSE
		dw_manu_bkno.deleterow(i)
	END IF

NEXT
close(w_pics_retrieve_msg_box)


dw_manu_bkno.Object.prod_bkseq.tabsequence='0'
dw_manu_bkno.Object.prod_bkseq.Background.Color='12632256'
dw_manu_bkno.Object.ttlinit_ttl.Background.Color='12632256'
	 
dw_manu_bkno.Object.prod_actstdt.tabsequence='20'   
dw_manu_bkno.Object.ccompdate.tabsequence='30'   
dw_manu_bkno.Object.cshipdate.tabsequence='40'   
dw_manu_bkno.Object.mchar_pcrecmstdt.tabsequence='50'   

dw_manu_bkno.ResetUpdate( )
dw_manu_bkno.SetFocus()
		
cb_clear.Enabled = TRUE
cb_find.Enabled = TRUE
cb_update.Enabled = TRUE
cb_find.Enabled = FALSE

end event

event mousemove;SetMicroHelp(w_pics_main,this.tag)
end event

type cb_update from commandbutton within w_mm_wk_data_input
event mousemove pbm_mousemove
string tag = "Update the Record"
integer x = 2185
integer y = 1600
integer width = 247
integer height = 108
boolean bringtotop = true
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
boolean enabled = false
string text = "&Update"
end type

event mousemove;SetMicroHelp(w_pics_main,this.tag)
end event

event clicked;int rtn,i,rowcount
long Lbkseq
string Lbkmed,Lcntr,Lcntrtype,Lconno,Ldupflg
date Lactstdt,Lcompdate,Lshipdate,Lpcrecmstdt
Datetime ltoday
ltoday = DateTime(Today(), Now())

dw_manu_bkno.Accepttext()

SetMicroHelp(w_pics_main,"Updating Records Please Wait...")

rowcount = dw_manu_bkno.RowCount()
FOR i=1 TO rowcount
	Lbkseq 	 	= 	dw_manu_bkno.object.prod_bkseq[i]
	Lbkmed 	 	= 	dw_manu_bkno.object.prod_bkmed[i]
	Lcntr  	 	= 	dw_manu_bkno.object.prod_cntr[i]
	Lactstdt 	= 	dw_manu_bkno.object.prod_actstdt[i]
	Lcompdate 	= 	dw_manu_bkno.object.ccompdate[i]
	Lpcrecmstdt = 	dw_manu_bkno.object.mchar_pcrecmstdt[i]
	Lconno 		= 	dw_manu_bkno.object.mchar_conno[i]
	Ldupflg 	   = 	dw_manu_bkno.object.dupflg[i]
			
	Select cntrtype into :Lcntrtype from ancntr
		where cntr = :Lcntr 
	using sqlservertrans;
	
	// Update MCHAR Table, and set update_date to todays date
	UPDATE mchar
	SET pcrecmstdt = :Lpcrecmstdt, update_date = :ltoday
	WHERE mchar.conno = :Lconno
	USING SQLServerTrans;
	IF f_check_dberror(sqlservertrans,"MCHAR")=FALSE THEN
		close(w_pics_update_msg_box)
		RETURN -1
	END IF
	
	//MessageBox("contact type","contract type = "+Lcntrtype)
	// Update PROD based on contract type
	CHOOSE CASE Lcntrtype
	CASE 'T'
		// Update narration stage
			UPDATE prod  
			SET actenddt = :Lcompdate,actstdt = :Lactstdt
			WHERE bkseq = :Lbkseq
			AND cntr = :Lcntr
			AND prodstage in ('MA','PU','AB')
			USING SQLServerTrans;
			IF f_check_dberror(sqlservertrans,"PROD")=FALSE THEN
					close(w_pics_update_msg_box)
					RETURN -1
			END IF
		// Update duplication stage
			Lshipdate =    dw_manu_bkno.object.cshipdate[i]
			UPDATE prod  
			SET actenddt = :Lshipdate
			WHERE bkseq = :Lbkseq
			AND cntr = :Lcntr
			AND prodstage in ('DU','PR','PB','EM')
			USING SQLServerTrans;
			IF f_check_dberror(sqlservertrans,"PROD")=FALSE THEN
				close(w_pics_update_msg_box)
				RETURN -1
			END IF
	CASE 'M'
			UPDATE prod  
			SET actenddt = :Lcompdate,actstdt = :Lactstdt
			WHERE bkseq = :Lbkseq
			AND cntr = :Lcntr
			AND prodstage in ('MA','PU','AB')
			USING SQLServerTrans;
			IF f_check_dberror(sqlservertrans,"PROD")=FALSE THEN
				close(w_pics_update_msg_box)
				RETURN -1
			END IF
			IF Ldupflg='Y' THEN
				// First get the dup only contract number.
				Select cntr into :Lcntr
				from prod
				where bkseq= :lbkseq
				and prodstage in ('DU','PR')
				USING SQLServerTrans;
				IF f_check_dberror(sqlservertrans,"PROD")=FALSE THEN
					close(w_pics_update_msg_box)
					RETURN -1
				END IF
				// Assign the shipdate to that contract number.
				Lshipdate =    dw_manu_bkno.object.cshipdate[i]
				UPDATE prod  
				SET actenddt = :Lshipdate
				WHERE bkseq = :Lbkseq
				AND cntr = :Lcntr
				AND prodstage in ('DU','PR')
				USING SQLServerTrans;
				IF f_check_dberror(sqlservertrans,"PROD")=FALSE THEN
					close(w_pics_update_msg_box)
					RETURN -1
				END IF
			END IF				
	CASE 'D'
		//MessageBox("dup only","updating dup contract type")
		Lshipdate =    dw_manu_bkno.object.cshipdate[i]
		UPDATE prod  
		SET actenddt = :Lshipdate,actstdt = :Lactstdt
		WHERE bkseq = :Lbkseq
		AND cntr = :Lcntr
		AND prodstage in ('DU','PR')
		USING SQLServerTrans;
		IF f_check_dberror(sqlservertrans,"PROD")=FALSE THEN
			close(w_pics_update_msg_box)
			RETURN -1
		END IF
	CASE ELSE
	END CHOOSE
	
NEXT

COMMIT USING sqlservertrans;
dw_manu_bkno.ResetUpdate( )	
sle_rows_updated.text = string(rowcount)		
SetMicroHelp(w_pics_main,"Update was successful.")
RETURN 1
end event

type cb_clear from commandbutton within w_mm_wk_data_input
event mousemove pbm_mousemove
string tag = "Clear the screen"
integer x = 2496
integer y = 1600
integer width = 247
integer height = 108
boolean bringtotop = true
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
boolean enabled = false
string text = "&Clear"
end type

event mousemove;SetMicroHelp(w_pics_main,this.tag)
end event

event clicked;// Reset the datawindow
dw_manu_bkno.Reset()
// Set the taborder in a way to enter book numbers.
dw_manu_bkno.Object.prod_bkseq.tabsequence='10'   
dw_manu_bkno.Object.prod_bkseq.Background.Color='16777215'
dw_manu_bkno.Object.prod_actstdt.tabsequence='0'   
dw_manu_bkno.Object.ccompdate.tabsequence='0'   
dw_manu_bkno.Object.cshipdate.tabsequence='0'   
dw_manu_bkno.Object.mchar_pcrecmstdt.tabsequence='0' 

SetMicroHelp(w_pics_main,"Ready")
sle_rows_updated.text = ""

// Set taborder
//sle_prdr.enabled = TRUE
//sle_prdr.text=""
//sle_prdr.SetFocus()

cb_clear.Enabled = FALSE
cb_find.Enabled = FALSE
cb_update.Enabled = FALSE
cb_find.Enabled = FALSE
dw_manu_bkno.InsertRow(0)
dw_manu_bkno.SetFocus()
dw_manu_bkno.SetColumn('prod_bkseq')
end event

type cb_exit from commandbutton within w_mm_wk_data_input
event mousemove pbm_mousemove
string tag = "Exit the screen"
integer x = 2807
integer y = 1600
integer width = 247
integer height = 108
boolean bringtotop = true
integer textsize = -10
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "E&xit"
end type

event mousemove;SetMicroHelp(w_pics_main,this.tag)
end event

event clicked;IF IsValid(w_pics_retrieve_msg_box) = FALSE THEN close(w_pics_retrieve_msg_box)
SetMicroHelp(w_pics_main,"Ready")
//close(parent)
parent.event pfc_close()

end event

type sle_prdr from u_sle within w_mm_wk_data_input
event ue_enterkey pbm_dwnprocessenter
boolean visible = false
integer x = 809
integer y = 1604
integer width = 50
integer height = 100
integer taborder = 10
integer textsize = -10
integer weight = 700
long textcolor = 255
textcase textcase = upper!
integer limit = 4
end type

event ue_enterkey;call super::ue_enterkey;Send(Handle(this),256,9,Long(0,0))
return(1)
end event

event modified;call super::modified;int rtn,i
string Lprdr
Lprdr = this.text
IF f_validate_prdr(Lprdr)=TRUE THEN
	dw_manu_bkno.event pfc_addrow()
	
	dw_manu_bkno.Object.prod_bkseq.tabsequence='10'   
	dw_manu_bkno.Object.prod_bkseq.Background.Color='16777215'
	dw_manu_bkno.Object.prod_actstdt.tabsequence='0'   
	dw_manu_bkno.Object.ccompdate.tabsequence='0'   
	dw_manu_bkno.Object.cshipdate.tabsequence='0'   
	dw_manu_bkno.Object.mchar_pcrecmstdt.tabsequence='0'   
	

	dw_manu_bkno.SetFocus()
	sle_prdr.enabled = FALSE
	
	cb_clear.Enabled = TRUE
	cb_find.Enabled = TRUE
ELSE
	MessageBox("ERROR","Invalid producer, Please enter the correct producer code.",StopSign!)
END IF
end event

type st_2 from statictext within w_mm_wk_data_input
integer x = 32
integer y = 1612
integer width = 539
integer height = 76
boolean bringtotop = true
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long backcolor = 78164112
boolean enabled = false
string text = "Number of rows updated"
boolean focusrectangle = false
end type

type sle_rows_updated from singlelineedit within w_mm_wk_data_input
integer x = 581
integer y = 1600
integer width = 165
integer height = 88
boolean bringtotop = true
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 255
long backcolor = 16777215
boolean autohscroll = false
borderstyle borderstyle = stylelowered!
end type


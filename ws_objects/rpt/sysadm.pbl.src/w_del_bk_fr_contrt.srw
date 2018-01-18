$PBExportHeader$w_del_bk_fr_contrt.srw
forward
global type w_del_bk_fr_contrt from window
end type
type dw_for_contract from u_pics_dw within w_del_bk_fr_contrt
end type
type dw_for_bkmed from u_pics_dw within w_del_bk_fr_contrt
end type
type st_prdr from statictext within w_del_bk_fr_contrt
end type
type st_stage2 from statictext within w_del_bk_fr_contrt
end type
type st_stage1 from statictext within w_del_bk_fr_contrt
end type
type st_hstage from statictext within w_del_bk_fr_contrt
end type
type st_hprdr from statictext within w_del_bk_fr_contrt
end type
type st_title from statictext within w_del_bk_fr_contrt
end type
type st_httl from statictext within w_del_bk_fr_contrt
end type
type cb_cancel from commandbutton within w_del_bk_fr_contrt
end type
type cb_find from commandbutton within w_del_bk_fr_contrt
end type
type sle_bkseq from singlelineedit within w_del_bk_fr_contrt
end type
type st_userid from statictext within w_del_bk_fr_contrt
end type
type cb_close from commandbutton within w_del_bk_fr_contrt
end type
type cb_delete from commandbutton within w_del_bk_fr_contrt
end type
type st_1 from statictext within w_del_bk_fr_contrt
end type
type st_oldgroup from statictext within w_del_bk_fr_contrt
end type
end forward

global type w_del_bk_fr_contrt from window
integer x = 1047
integer y = 776
integer width = 1586
integer height = 1056
boolean titlebar = true
string title = "Deleting a book from PROD and PRDRPROD tables"
boolean controlmenu = true
windowtype windowtype = response!
long backcolor = 79741120
dw_for_contract dw_for_contract
dw_for_bkmed dw_for_bkmed
st_prdr st_prdr
st_stage2 st_stage2
st_stage1 st_stage1
st_hstage st_hstage
st_hprdr st_hprdr
st_title st_title
st_httl st_httl
cb_cancel cb_cancel
cb_find cb_find
sle_bkseq sle_bkseq
st_userid st_userid
cb_close cb_close
cb_delete cb_delete
st_1 st_1
st_oldgroup st_oldgroup
end type
global w_del_bk_fr_contrt w_del_bk_fr_contrt

type variables
long ii_re=1
string is_tbls
end variables

forward prototypes
public function integer wf_check_and_delete (string as_table, long ai_bkseq, string as_bkmed, string as_cntr)
end prototypes

public function integer wf_check_and_delete (string as_table, long ai_bkseq, string as_bkmed, string as_cntr);
long li_count, li_count1,  li_null,li_pos
string ls_choose='one', ls_stage, ls_null, ls_tbls_right,ls_tbls_left

SetNull(li_null)
SetNull(ls_null)
choose case as_table
case 'prod'
	
	select count(distinct cntr) into :li_count
	from prod
	where bkseq=:ai_bkseq and bkmed=:as_bkmed
	using SqlServerTrans;
	if not f_check_dberror(SqlServerTrans,'select count from using bkseq,bkmed') then
		return -1
	end if
	if li_count=2 then
		select prodstage into :ls_stage
		from prod
		where bkseq=:ai_bkseq and bkmed=:as_bkmed and cntr=:as_cntr
		using SqlServerTrans;
		if not f_check_dberror(SqlServerTrans,'select count from using bkseq,bkmed,cntr') then
			return -1
		end if
		if ls_stage ='MA' or ls_stage='AB' THEN
			ii_re=messagebox('','This is total contract.You are removing mastering.'+&
			'~nDo You want to remove mastering?'+&
			'~nIf You want to remove mastering only, choose Yes;'+&
			'~nIf You want to remove mastering and duplication/press both, choose No.',Exclamation!,&
			YesNo!,2)
		elseif ls_stage='DU' or ls_stage='PR' THEN
			ii_re=messagebox('','This is total contract.You are removing duplication/press.'+&
			'~nDo You want to remove duplication/press ?'+&
			'~nIf You want to remove duplication/press only, choose Yes;'+&
			'~nIf You want to remove duplication/press and mastering both, choose No.',Exclamation!,&
			YesNo!,2)
		elseif ls_stage='PU' THEN
			ii_re=messagebox('','This is total contract.You are removing purchase.'+&
			'~nDo You want to remove purchase ?'+&
			'~nIf You want to remove purchase only, choose Yes;'+&
			'~nIf You want to remove purchase and print braille both, choose No.',Exclamation!,&
			YesNo!,2)
		elseif ls_stage='PB' THEN
			ii_re=messagebox('','This is total contract.You are removing purchase.'+&
			'~nDo You want to remove print braille ?'+&
			'~nIf You want to remove print braille only, choose Yes;'+&
			'~nIf You want to remove print braille and purchase both, choose No.',Exclamation!,&
			YesNo!,2)
		end if		
	end if
	if ii_re=1 then
		delete
		from prod
		where bkseq=:ai_bkseq and bkmed=:as_bkmed and cntr=:as_cntr
		using SqlServerTrans;
		if not f_check_dberror(SqlServerTrans,'delete from prod using bkseq,bkmed and cntr') then
			return -1
		end if
	elseif ii_re=2 then
		delete
		from prod
		where bkseq=:ai_bkseq and bkmed=:as_bkmed
		using SqlServerTrans;
		if not f_check_dberror(SqlServerTrans,'delete from prod using bkseq,bkmed only') then
			return -1
		end if
	end if
//	is_tbls='prod'+', '		
//case 'mchar'
//	select count(*) into :li_count
//	from mchar
//	where bkseq=:ai_bkseq and bkmed=:as_bkmed
//	using SqlServerTrans;
//	if not f_check_dberror(SqlServerTrans,'select count from mchar using bkseq,bkmed') then
//		return -1
//	elseif li_count>0 then
//		update mchar
//		set bkseq=:li_null, bkmed= :ls_null
//		where bkseq=:ai_bkseq and bkmed=:as_bkmed
//		using SqlServerTrans;
//		if not f_check_dberror(SqlServerTrans,'update mchar set bkseq and bkmed null') then
//			return -1
//		end if
//		is_tbls=is_tbls+'mchar'+', '
//	end if
//case 'catalog'
//	select count(*) into :li_count
//	from catalog
//	where bkseq=:ai_bkseq and bkmed=:as_bkmed
//	using SqlServerTrans;
//	if not f_check_dberror(SqlServerTrans,'select count from catalog using bkseq,bkmed') then
//		return -1
//	elseif li_count>0 then
//		update catalog
//		set bkseq=:li_null, bkmed= :ls_null
//		where bkseq=:ai_bkseq and bkmed=:as_bkmed
//		using SqlServerTrans;
//		if not f_check_dberror(SqlServerTrans,'update catalog set bkseq and bkmed null') then
//			return -1
//		end if
//		is_tbls=is_tbls+'catalog'+', '
//	end if	
//case 'narr'
//	select count(*) into :li_count
//	from narr
//	where bkseq=:ai_bkseq and bkmed=:as_bkmed
//	using SqlServerTrans;
//	if not f_check_dberror(SqlServerTrans,'select count from narr using bkseq,bkmed') then
//		return -1
//	elseif li_count>0 then
//		delete
//		from narr
//		where bkseq=:ai_bkseq and bkmed=:as_bkmed
//		using SqlServerTrans;
//		if not f_check_dberror(SqlServerTrans,'delete from narr using bkseq and bkmed ') then
//			return -1
//		end if
//		is_tbls=is_tbls+'narr'+', '
//	end if
//case 'sched'
//	select count(*) into :li_count
//	from sched
//	where bkseq=:ai_bkseq and bkmed=:as_bkmed
//	using SqlServerTrans;
//	if not f_check_dberror(SqlServerTrans,'select count from sched using bkseq,bkmed') then
//		return -1
//	elseif li_count>0 then
//		delete
//		from sched
//		where bkseq=:ai_bkseq and bkmed=:as_bkmed
//		using SqlServerTrans;
//		if not f_check_dberror(SqlServerTrans,'delete from sched using bkseq and bkmed ') then
//			return -1
//		end if
//		is_tbls=is_tbls+'sched'+', '
//	end if
case 'inv'
	if ii_re=1 then
		select count(*) into :li_count
		from inv
		where bkseq=:ai_bkseq and bkmed=:as_bkmed and cntr=:as_cntr
		using SqlServerTrans;
		if not f_check_dberror(SqlServerTrans,'select count from inv using bkseq,bkmed,cntr') then
			return -1
		elseif li_count>0 then
			delete
			from inv
			where bkseq=:ai_bkseq and bkmed=:as_bkmed and cntr=:as_cntr
			using SqlServerTrans;
			if not f_check_dberror(SqlServerTrans,'delete from inv using bkseq,bkmed,cntr') then
				return -1
			end if
			is_tbls=is_tbls+'inv'+', '
		end if
	elseif ii_re=2 then
		select count(*) into :li_count
		from inv
		where bkseq=:ai_bkseq and bkmed=:as_bkmed 
		using SqlServerTrans;
		if not f_check_dberror(SqlServerTrans,'select count from inv using bkseq,bkmed') then
			return -1
		elseif li_count>0 then
			delete
			from inv
			where bkseq=:ai_bkseq and bkmed=:as_bkmed
			using SqlServerTrans;
			if not f_check_dberror(SqlServerTrans,'delete from inv using bkno only') then
				return -1
			end if
			is_tbls=is_tbls+'inv'+', '
		end if
	end if
case 'qastg'
	if ii_re=1 then
		select count(*) into :li_count
		from qastg
		where bkseq=:ai_bkseq and bkmed=:as_bkmed and cntr=:as_cntr
		using SqlServerTrans;
		if not f_check_dberror(SqlServerTrans,'select count from qastg using bkseq,bkmed,cntr') then
			return -1
		elseif li_count>0 then
			delete
			from qastg
			where bkseq=:ai_bkseq and bkmed=:as_bkmed and cntr=:as_cntr
			using SqlServerTrans;
			if not f_check_dberror(SqlServerTrans,'delete from qastg using bkno and cntr') then
				return -1
			end if
			is_tbls=is_tbls+'qastg'+', '
		end if
	elseif ii_re=2 then
		select count(*) into :li_count
		from qastg
		where bkseq=:ai_bkseq and bkmed=:as_bkmed 
		using SqlServerTrans;
		if not f_check_dberror(SqlServerTrans,'select count from qastg using bkseq,bkmed only') then
			return -1
		elseif li_count>0 then
			delete
			from qastg
			where bkseq=:ai_bkseq and bkmed=:as_bkmed
			using SqlServerTrans;
			if not f_check_dberror(SqlServerTrans,'delete from qastg using bkseq,bkmed only') then
				return -1
			end if
			is_tbls=is_tbls+'qastg'+', '
		end if
	end if	
case 'ext'
	if ii_re=1 then
		select count(*) into :li_count
		from ext
		where bkseq=:ai_bkseq and bkmed=:as_bkmed and cntr=:as_cntr
		using SqlServerTrans;
		if not f_check_dberror(SqlServerTrans,'select count from ext using bkseq,bkmed,cntr') then
			return -1
		elseif li_count>0 then
			delete
			from ext
			where bkseq=:ai_bkseq and bkmed=:as_bkmed and cntr=:as_cntr
			using SqlServerTrans;
			if not f_check_dberror(SqlServerTrans,'delete from ext using bkseq,bkmed,cntr') then
				return -1
			end if
			is_tbls=is_tbls+'ext'+', '
		end if
	elseif ii_re=2 then
		select count(*) into :li_count
		from ext
		where bkseq=:ai_bkseq and bkmed=:as_bkmed 
		using SqlServerTrans;
		if not f_check_dberror(SqlServerTrans,'select count from ext using bkseq,bkmed only') then
			return -1
		elseif li_count>0 then
			delete
			from ext
			where bkseq=:ai_bkseq and bkmed=:as_bkmed
			using SqlServerTrans;
			if not f_check_dberror(SqlServerTrans,'delete from ext using bkseq,bkmed only') then
				return -1
			end if
			is_tbls=is_tbls+'ext'+', '
		end if
	end if		
end choose

return 1
end function

on w_del_bk_fr_contrt.create
this.dw_for_contract=create dw_for_contract
this.dw_for_bkmed=create dw_for_bkmed
this.st_prdr=create st_prdr
this.st_stage2=create st_stage2
this.st_stage1=create st_stage1
this.st_hstage=create st_hstage
this.st_hprdr=create st_hprdr
this.st_title=create st_title
this.st_httl=create st_httl
this.cb_cancel=create cb_cancel
this.cb_find=create cb_find
this.sle_bkseq=create sle_bkseq
this.st_userid=create st_userid
this.cb_close=create cb_close
this.cb_delete=create cb_delete
this.st_1=create st_1
this.st_oldgroup=create st_oldgroup
this.Control[]={this.dw_for_contract,&
this.dw_for_bkmed,&
this.st_prdr,&
this.st_stage2,&
this.st_stage1,&
this.st_hstage,&
this.st_hprdr,&
this.st_title,&
this.st_httl,&
this.cb_cancel,&
this.cb_find,&
this.sle_bkseq,&
this.st_userid,&
this.cb_close,&
this.cb_delete,&
this.st_1,&
this.st_oldgroup}
end on

on w_del_bk_fr_contrt.destroy
destroy(this.dw_for_contract)
destroy(this.dw_for_bkmed)
destroy(this.st_prdr)
destroy(this.st_stage2)
destroy(this.st_stage1)
destroy(this.st_hstage)
destroy(this.st_hprdr)
destroy(this.st_title)
destroy(this.st_httl)
destroy(this.cb_cancel)
destroy(this.cb_find)
destroy(this.sle_bkseq)
destroy(this.st_userid)
destroy(this.cb_close)
destroy(this.cb_delete)
destroy(this.st_1)
destroy(this.st_oldgroup)
end on

event open;This.X = 1000
This.Y = 200
sle_bkseq.SetFocus()
cb_cancel.visible=false
cb_delete.visible =false
st_httl.visible=false
st_hprdr.visible=false
st_hstage.visible=false
st_title.visible= false
st_prdr.visible=false
st_stage1.visible= false
st_stage2.visible=false
cb_delete.visible=false
cb_cancel.visible=false

end event

type dw_for_contract from u_pics_dw within w_del_bk_fr_contrt
integer x = 343
integer y = 168
integer width = 352
integer height = 100
integer taborder = 70
string dataobject = "dddw_for_contract"
boolean vscrollbar = false
end type

event itemchanged;call super::itemchanged;datawindowchild dwc_cntr, dwc_bkmed
string ls_bkseq,ls_bkmed
long li_bkseq, li_count

ls_bkseq= sle_bkseq.text
if not IsNumber(ls_bkseq) then
	messagebox('','This is not number. Reenter it again.')
	sle_bkseq.SetFocus()
	return
end if
ls_bkmed= dw_for_bkmed.GetItemString(1,'bkmed')
if ls_bkmed='' or IsNull(ls_bkmed) then
	messagebox('','You must choose book bkmed first.')
	return
end if
li_bkseq= long( ls_bkseq )
select count(distinct cntr) into :li_count
from prod
where bkseq=:li_bkseq and bkmed= :ls_bkmed
using SqlServerTrans;
//dw_for_contract.SetTransObject(SqlServerTrans)
//dw_for_contract.GetChild('cntr',dwc_cntr)
//dwc_cntr.SetTransObject(SqlServerTrans)
//dwc_cntr.Retrieve(li_bkseq, ls_bkmed)
//li_count=dwc_cntr.RowCount()
if li_count=0 then
	messagebox('','These are not valid book bkmed. Reenter it again.')
	dw_for_bkmed.SetFocus()
	return
end if
cb_cancel.visible=true
end event

type dw_for_bkmed from u_pics_dw within w_del_bk_fr_contrt
integer x = 1129
integer y = 48
integer width = 352
integer height = 100
integer taborder = 60
string dataobject = "dddw_for_bkmed"
boolean vscrollbar = false
end type

event itemchanged;call super::itemchanged;datawindowchild dwc_cntr, dwc_bkmed
string ls_bkseq,ls_bkmed
long li_bkseq, li_count

ls_bkseq= sle_bkseq.text
if not IsNumber(ls_bkseq) then
	messagebox('','This is not number. Reenter it again.')
	sle_bkseq.SetFocus()
	return
end if
ls_bkmed= data
li_bkseq= long( ls_bkseq )
dw_for_contract.SetTransObject(SqlServerTrans)
dw_for_contract.GetChild('cntr',dwc_cntr)
dwc_cntr.SetTransObject(SqlServerTrans)
dwc_cntr.Retrieve(li_bkseq, ls_bkmed)
dw_for_contract.Retrieve(li_bkseq, ls_bkmed)
dw_for_contract.InsertRow(0)
li_count=dwc_cntr.RowCount()
if li_count=0 then
	messagebox('','These are not valid book bkmed. Reenter it again.')
	dw_for_bkmed.SetFocus()
	return
end if
cb_cancel.visible=true
end event

type st_prdr from statictext within w_del_bk_fr_contrt
integer x = 617
integer y = 468
integer width = 293
integer height = 72
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long backcolor = 12632256
boolean enabled = false
boolean border = true
borderstyle borderstyle = stylelowered!
boolean focusrectangle = false
end type

type st_stage2 from statictext within w_del_bk_fr_contrt
integer x = 1010
integer y = 608
integer width = 293
integer height = 68
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long backcolor = 12632256
boolean enabled = false
boolean border = true
borderstyle borderstyle = stylelowered!
boolean focusrectangle = false
end type

type st_stage1 from statictext within w_del_bk_fr_contrt
integer x = 617
integer y = 608
integer width = 293
integer height = 68
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long backcolor = 12632256
boolean enabled = false
boolean border = true
borderstyle borderstyle = stylelowered!
boolean focusrectangle = false
end type

type st_hstage from statictext within w_del_bk_fr_contrt
integer x = 183
integer y = 604
integer width = 411
integer height = 52
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long backcolor = 78164112
boolean enabled = false
string text = "Production Stage:"
boolean focusrectangle = false
end type

type st_hprdr from statictext within w_del_bk_fr_contrt
integer x = 315
integer y = 468
integer width = 279
integer height = 52
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long backcolor = 78164112
boolean enabled = false
string text = "Producer:"
alignment alignment = right!
boolean focusrectangle = false
end type

type st_title from statictext within w_del_bk_fr_contrt
integer x = 215
integer y = 328
integer width = 1230
integer height = 64
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long backcolor = 12632256
boolean enabled = false
boolean border = true
borderstyle borderstyle = stylelowered!
boolean focusrectangle = false
end type

type st_httl from statictext within w_del_bk_fr_contrt
integer x = 64
integer y = 324
integer width = 123
integer height = 52
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long backcolor = 78164112
boolean enabled = false
string text = "Title:"
boolean focusrectangle = false
end type

type cb_cancel from commandbutton within w_del_bk_fr_contrt
integer x = 818
integer y = 776
integer width = 247
integer height = 108
integer taborder = 30
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "&Cancel"
end type

event clicked;	dw_for_bkmed.Reset()
	sle_bkseq.text=''
	sle_bkseq.text=''
	dw_for_contract.Reset()
	st_httl.visible=false
	st_hprdr.visible=false
	st_hstage.visible=false
	st_title.visible=false
	st_prdr.visible= false
	st_stage1.visible=false
	st_stage2.visible=false
	cb_delete.visible=false
end event

type cb_find from commandbutton within w_del_bk_fr_contrt
integer x = 498
integer y = 776
integer width = 247
integer height = 108
integer taborder = 20
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "&Find"
boolean default = true
end type

event clicked;string ls_cntr, ls_bkmed, ls_prdr, ls_maxstg, ls_minstg, ls_type,ls_chno,ls_ttl,&
			ls_bkseq, ls_text
long li_bkseq, li_count1, li_count2

li_count1=dw_for_contract.RowCount()
li_count2=dw_for_bkmed.RowCount()
if li_count1=0  or li_count2=0 then
	messagebox('','Invalid book number, reenter it again.')
	sle_bkseq.SetFocus()
	return
end if

ls_cntr=dw_for_contract.GetItemString(1,'cntr')
ls_bkseq=sle_bkseq.text
li_bkseq= long( ls_bkseq )
ls_bkmed=dw_for_bkmed.GetItemString(1,'bkmed')
if ls_cntr='' or IsNull(ls_cntr) then
	messagebox('','You must choose cntr number first.')
	return
end if
if ls_bkmed='' or IsNull(ls_bkmed) then
	messagebox('','You must choose book med first.')
	return
end if
select count(*),max(prodstage),min(prodstage) into :li_count1,:ls_maxstg, :ls_minstg
from prod
where cntr=:ls_cntr and bkseq=:li_bkseq and bkmed= :ls_bkmed
using SqlServerTrans;
if not f_check_dberror(SqlServerTrans,'find the count from prod table') then
end if
if li_count1=0 then
	messagebox('','No this record in the prod table')
end if
select  chno into  :ls_chno
from mchar
where bkseq=:li_bkseq and bkmed= :ls_bkmed
using SqlServerTrans;
if not f_check_dberror(SqlServerTrans,'find the count from mchar table') then
end if
if IsNull(ls_chno) or ls_chno='' then
	messagebox('','No this record in the mchar table')

end if
select  ttl into  :ls_ttl
from ttlinit
where chno=:ls_chno
using SqlServerTrans;
if not f_check_dberror(SqlServerTrans,'find the title from ttlinit table') then
end if
if IsNull(ls_ttl) or ls_ttl='' then
	messagebox('','No title record in the ttlinit table')
end if
select prdr,cntrtype into :ls_prdr, :ls_type
from ancntr
where cntr=:ls_cntr 
using SqlServerTrans;
if not f_check_dberror(SqlServerTrans,'find the producer from ancntr table') then
end if
if IsNull(ls_prdr) or ls_prdr='' then
	messagebox('','No producer is associated with this cntr :'+ls_cntr+' in ancntr table')
end if
if li_count1<>0 or (not (IsNull(ls_prdr) or ls_prdr=''))   then
										
	cb_cancel.visible=true
	cb_delete.visible =true	
else
	messagebox('','Invalid cntr and bkseq and bkmed')
end if
if (not isnull(ls_prdr)) and (not(ls_prdr='')) then
	st_prdr.text=ls_prdr
	st_prdr.visible=true
	st_hprdr.visible=true
end if
if (not isnull(ls_ttl)) and (not(ls_ttl='')) then
	st_title.text=ls_ttl
	st_title.visible= true
	st_httl.visible=true
end if
if li_count1=1 then
	st_hstage.visible=true
	choose case ls_maxstg
		CASE 'DU'
			ls_text='Duplication'
		CASE 'MA'
			ls_text='Mastering'
		CASE 'AB'
			ls_text='Abstruse'
		CASE 'PR'
			ls_text='Press'
	END CHOOSE
	st_stage1.text=ls_text
	st_stage1.visible= true
elseif li_count1=2 then
	st_hstage.visible=true
	st_stage1.visible= true
	st_stage2.visible=true
	if ls_type="T" then
		if ls_maxstg='DU' or ls_minstg='DU' then
			st_stage2.text='Duplication'
			if ls_maxstg='MA' then
				st_stage1.text='Mastering'
			else
				st_stage1.text='Abstruse'
			end if
		end if
		if ls_maxstg='PR' or ls_minstg='PR' then
			st_stage2.text='Press'
			if ls_minstg='AB' then
				st_stage1.text='Abstruse'
			else
				st_stage1.text='Mastering'
			end if
		end if
		if ls_maxstg='PB' or ls_minstg='PB' then
			st_stage2.text='Print Braille'
			st_stage1.text='Purchase'
		end if
	end if //if ls_type="M"
end if //if li_count1=2
if li_count1>0 then
	cb_delete.visible=true
end if
		
		
		
		
		
		
		
		
	
	
	
	
	
	
	

end event

type sle_bkseq from singlelineedit within w_del_bk_fr_contrt
integer x = 347
integer y = 40
integer width = 343
integer height = 92
integer taborder = 50
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long backcolor = 16777215
boolean autohscroll = false
borderstyle borderstyle = stylelowered!
end type

event modified;datawindowchild dwc_cntr, dwc_bkmed
string ls_bkseq,ls_bkmed
long li_bkseq, li_count, li_count2

ls_bkseq= sle_bkseq.text
if not IsNumber(ls_bkseq) then
//	messagebox('','This is not number. Reenter it again.')
	sle_bkseq.SetFocus()
	return
end if
li_bkseq= long( ls_bkseq )
select count(*) into :li_count
from prod
where bkseq= :li_bkseq
using SqlServerTrans;
if not f_check_dberror(SqlServerTrans,'select from prod using bkseq') then
	return
end if
if li_count=0 then
//	messagebox('','You enter invalid bkseq number, try it again.')
	sle_bkseq.SetFocus()
	return
end if
dw_for_bkmed.SetTransObject(SqlServerTrans)
dw_for_bkmed.GetChild('bkmed',dwc_bkmed)
dwc_bkmed.SetTransObject(SqlServerTrans)
li_count2=dwc_bkmed.Retrieve(li_bkseq)
dw_for_bkmed.Retrieve(li_bkseq)
//messagebox('','li_count2 is :   '+string(li_count2) )
if li_count2>0 then
	dw_for_bkmed.InsertRow(0)
end if
li_count=dwc_bkmed.RowCount()

if li_count=0 then
//	messagebox('','This is not valid book number. Reenter it again.')
	sle_bkseq.SetFocus()
	return
end if
if li_count2>=1 then
	ls_bkmed=dw_for_bkmed.GetItemString(1,'bkmed')
	dw_for_contract.SetTransObject(SqlServerTrans)
	dw_for_contract.GetChild('cntr',dwc_cntr)
	dwc_cntr.SetTransObject(SqlServerTrans)
	dwc_cntr.Retrieve(li_bkseq, ls_bkmed)
	li_count=dw_for_contract.Retrieve(li_bkseq, ls_bkmed)
	if li_count>0 then
		dw_for_contract.InsertRow(0)
	end if
	li_count=dwc_cntr.RowCount()

	if li_count=0 then
//		messagebox('','This is not valid book number. Reenter it again.')
		sle_bkseq.SetFocus()
		return
	end if
end if
cb_cancel.visible=true
end event

type st_userid from statictext within w_del_bk_fr_contrt
integer x = 64
integer y = 48
integer width = 247
integer height = 52
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long backcolor = 78164112
boolean enabled = false
string text = "Book No:"
boolean focusrectangle = false
end type

type cb_close from commandbutton within w_del_bk_fr_contrt
integer x = 1138
integer y = 776
integer width = 247
integer height = 108
integer taborder = 40
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "&Close"
end type

event clicked;close(parent)
end event

type cb_delete from commandbutton within w_del_bk_fr_contrt
integer x = 178
integer y = 776
integer width = 247
integer height = 108
integer taborder = 10
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "&Delete"
end type

event clicked;string ls_cntr, ls_bkmed, ls_prdr, ls_maxstg, ls_minstg, ls_type,ls_chno,ls_ttl,&
			ls_bkseq, ls_table,ls_tbls_left, ls_tbls_right
long li_bkseq, li_count1=0, li_count2=0, li_re, i, li_pos,li_len,li_count

li_count1=dw_for_contract.RowCount()
li_count2=dw_for_bkmed.RowCount()
if li_count1=0 or li_count2=0 then
	messagebox('','You must choose bkmed and cntract number first.')
	return
end if
ls_cntr=dw_for_contract.getItemString(1,'cntr')
ls_cntr=trim(ls_cntr)
ls_bkseq=sle_bkseq.text
if IsNumber(ls_bkseq) then
	li_bkseq= long( ls_bkseq )
else
	messagebox('','You must enter number.')
	sle_bkseq.SetFocus()
	return
end if
ls_bkmed=dw_for_bkmed.getItemString(1,'bkmed')
ls_bkmed= trim(ls_bkmed)
if li_bkseq>0 and (not ls_cntr='') and (not isNull(ls_cntr)) and &
							ls_bkmed<>''  and	(not IsNull(ls_bkmed))			then
	li_re=messagebox('Delete Book number','Are You sure you want to delete book number '+string(ls_bkseq)+' from PROD and PRDRPROD tables?',Question!,YesNo!,2)
	if li_re=2 then
		return
	else	
		select count(*) into :li_count
			from inv
		where bkseq=:ls_bkseq and bkmed=:ls_bkmed and cntr=:ls_cntr
		using SqlServerTrans;
		if not f_check_dberror(SqlServerTrans,'select count from inv using bkseq,bkmed,cntr') then
			return -1
		elseif li_count > 0 then
			MessageBox("ERROR","Invoicing already exist for book number: "+string(ls_bkseq)+". Unable to delete it from database.",StopSign!)
			RETURN 
		elseif li_count = 0 then
			// No invoicing exist, delete the book from drop table.
			delete
			from prod
			where bkseq=:ls_bkseq and bkmed=:ls_bkmed and cntr=:ls_cntr
			using SqlServerTrans;
			if not f_check_dberror(SqlServerTrans,'delete from prod using bkseq,bkmed and cntr') then
				return -1
			else
			  // Now delete the book from PRDRPROD table
				delete
				from prdrprod@pic_link
				where bkseq=:ls_bkseq and bkmed=:ls_bkmed and cntr=:ls_cntr
				using SqlServerTrans;
				if not f_check_dberror(SqlServerTrans,'delete from prdrprod using bkseq,bkmed and cntr') then
					return -1
				end if
			end if		
		end if
	end if
	if li_re=1 then
		Commit using SqlServerTrans;
		messagebox('Deleting','Book No: '+ls_bkseq+' '+ls_bkmed+' has been removed from PROD and PRDRPROD tables successfully.')
		dw_for_bkmed.Reset()
		sle_bkseq.text=''
		dw_for_contract.Reset()
		st_httl.visible=false
		st_hprdr.visible=false
		st_hstage.visible=false
		st_title.visible=false
		st_prdr.visible= false
		st_stage1.visible=false
		st_stage2.visible=false
		cb_delete.visible=false
	end if
else
	messagebox('Delete a book','You must choose a book number,media and a contract number first.')
	return
end if	
is_tbls=''
end event

type st_1 from statictext within w_del_bk_fr_contrt
integer x = 64
integer y = 176
integer width = 247
integer height = 52
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long backcolor = 78164112
boolean enabled = false
string text = "Contract:"
boolean focusrectangle = false
end type

type st_oldgroup from statictext within w_del_bk_fr_contrt
integer x = 841
integer y = 48
integer width = 247
integer height = 52
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long backcolor = 78164112
boolean enabled = false
string text = "Media:"
alignment alignment = right!
boolean focusrectangle = false
end type

